#include "mixed/scenario.hpp"
#include <map>
#include "mixed/stream_oracle.hpp"
#include "mixed/stream_recovery.hpp"
#include "mixed/store_address_replay.hpp"
#include "mixed/stream_cancellation.hpp"

namespace memblock::scenarios::mixed {
namespace {
// Architectural age belongs to this deque, independent of LSQ dequeue order.
struct StreamWork {
    unsigned kind = 0, generation = 0;
    memblock::LoadTransaction load;
    memblock::StoreTransaction store;
    memblock::PrefetchTransaction prefetch;
    std::vector<memblock::VectorMemoryTransaction> vectors;
    RandomVectorShape shape{};
    std::vector<bool> issued, offered;
    std::vector<std::uint64_t> vector_ready;
    std::vector<std::optional<memblock::VectorMemoryTransaction>> replay;
    bool address_offered = false, data_offered = false, paired_initial = false;
    std::array<std::optional<std::uint64_t>, 2> pair_edges;

    bool address_issued = false;
    bool data_issued = false;
    bool requires_head = false;
    bool data_first = false;
    bool address_consumed = false, data_consumed = false;
    unsigned address_retries = 0;
    StoreAddressReplay address_replay;
    std::uint64_t expected_scalar = 0;
    unsigned recovery_id = 0, recovery_index = 0;
    bool refetch = false, checked = false, permission_probe = false;
    std::optional<std::uint64_t> exception_anchor, anchor_wb_ready_cycle;
    bool fault() const { return kind == 0 && load.expected_exception_mask != 0; }
    std::optional<std::uint64_t> retire_ready_cycle, restart_ready_cycle, restart_block_cycle;
    bool restart_blocked = false, restart_flushed = false;
    bool needs_restart_flush() const {
        return !vectors.empty() && vectors.front().vstart != 0;
    }
    // A producer's checked-completion lifetime can outlive its deque entry.
    struct Producer { bool complete = false, retired = false; unsigned depth = 0; };
    std::shared_ptr<Producer> result = std::make_shared<Producer>();
    std::shared_ptr<Producer> address_source, data_source;
    std::shared_ptr<StreamByteOracle::Checkpoint> oracle_before;
    std::uint8_t rob() const {
        if (kind == 0) return load.rob;
        if (kind == 2) return store.rob;
        if (kind == 4) return prefetch.rob;
        return vectors.front().rob;
    }
    bool flag() const {
        if (kind == 0) return load.rob_flag;
        if (kind == 2) return store.rob_flag;
        if (kind == 4) return prefetch.rob_flag;
        return vectors.front().rob_flag;
    }
    bool is_store() const { return kind == 2 || kind == 3; }
    bool accepted() const {
        return address_issued || data_issued ||
            std::find(issued.begin(), issued.end(), true) != issued.end();
    }
};
constexpr std::array<unsigned, 5> stream_operations{{
    RandomConstraints::scalar_load, RandomConstraints::vector_load,
    RandomConstraints::scalar_store, RandomConstraints::vector_store,
    RandomConstraints::prefetch}};
}

bool MixedScenario::run_concurrent_stream(unsigned instruction_limit)
{
    if (instruction_limit == 0) return true;
    phase = "mixed-stream-enter";
    auto translation = choose_translation_context();
    translation.access = choose_stream_access();
    if (translation.regime != RandomConstraints::translation_bare && constraints.stream_mxr_per_mille != 0)
        translation.mxr = random() % 1000U < constraints.stream_mxr_per_mille;
    const auto leaf = choose_translation_leaf(translation);
    if (!enter_translation_context(translation) ||
        !issue_constrained_fence(RandomConstraints::scalar_load) ||
        !environment.run_until_all_complete(constrained_completion_timeout) ||
        !environment.run_until_queues_retired(constrained_completion_timeout) ||
        !environment.begin_store_release_epoch(constrained_completion_timeout)) return false;
    const auto ptw_before = environment.ptw_requests();
    const auto requests_before = environment.tilelink_requests();
    const auto arena = constrained_cacheable_address(64, translation, leaf, false);
    StreamByteOracle oracle(environment.memory());
    std::deque<StreamWork> live;
    using IssueKind = memblock::OrdinaryIssueKind;
    using IssueOutcome = memblock::OrdinaryIssueOutcome;
    struct Attempt { unsigned generation, kind, uop; bool replay; std::uint64_t offered_at; };
    std::unordered_map<std::uint64_t, Attempt> attempts;
    std::map<std::uint64_t, unsigned> accepted_ports;
    std::uint64_t next_attempt = 1;
    if (!environment.begin_issue_session()) return false;

    std::array<std::set<unsigned>, 3> destinations;
    const unsigned depth = 2U + random() % 7U;
    const unsigned length = std::min(instruction_limit, constraints.stream_min_instructions +
        unsigned(random() % (constraints.stream_max_instructions - constraints.stream_min_instructions + 1U)));
    unsigned target_admissions = length;
    std::set<unsigned> admitted_rob_identities;
    unsigned admitted = 0, retired = 0, recovery_serial = 0, canceled = 0;
    std::optional<StreamRecovery> recovery;
    auto expected_violations = environment.memory_violation_stats().count;
    auto &recovery_counts = coverage.stream_recovery;
    auto &inflight = coverage.stream_recovery_inflight;
    const auto drained_before = environment.store_retirement_stats().drained;
    auto &permission = coverage.stream_permission;
    unsigned permission_probes = 0;
    const unsigned permission_budget = constraints.stream_permission_load_per_mille != 0 &&
        translation.regime != RandomConstraints::translation_bare ? 1U + random() % 4U : 0U;
    std::vector<std::pair<std::uint64_t, std::uint64_t>> denied_pages;
    const auto uncache_before = environment.uncache_requests();
    bool returning_exception = false;
    permission[18] += translation.mxr;
    const auto check_permission_suppression = [&]() {
        if (denied_pages.empty()) return true;
        ++permission[17];
        for (const auto &[page, first_request] : denied_pages)
            for (unsigned offset = 0; offset < 4096; offset += 64)
                if (environment.dcache_requests_from_source_covering_since(2, page + offset, first_request) != 0) {
                    phase += ":denied-page-issued-CPU-demand"; return false;
                }
        if (environment.uncache_requests() != uncache_before) {
            phase += ":cacheable-stream-issued-uncache"; return false;
        }
        return true;
    };
    auto last_progress = environment.cycle();
    std::uint64_t deadline = environment.cycle() + std::uint64_t{target_admissions} * constrained_completion_timeout * 4U;
    const auto start_action = actions;
    ++coverage.stream[0];
    ++coverage.stream_access_epochs.at(translation.access * 3U + translation.regime);
    coverage.stream[10] += actions > coverage.concurrent_actions + coverage.stream[2];

    const auto pending = [&](const StreamWork &work) {
        if (work.kind == 0) return environment.scalar_load_pending(work.load);
        if (work.kind == 2) return environment.scalar_store_pending(work.store);
        if (work.kind == 4) return environment.prefetch_pending(work.prefetch);
        return std::any_of(work.vectors.begin(), work.vectors.end(),
            [&](const auto &uop) { return environment.vector_memory_pending(uop); });
    };
    const auto feedback_waits_at = [&](std::uint64_t edge, const StreamWork *actor = nullptr) {
        const auto &samples = environment.iq_slow_feedback_stats().sta_samples;
        return std::count_if(live.begin(), live.end(), [&](const auto &work) {
            return &work != actor && work.kind == 2 && !work.recovery_id &&
                work.address_replay.waiting_at(edge, samples, work.store);
        });
    };
    const auto note_wait_overlap = [&](unsigned bin, const StreamWork *actor = nullptr) {
        if (feedback_waits_at(environment.cycle() - 1U, actor)) ++coverage.stream_sta[bin];
    };
    const auto all_issued = [](const StreamWork &work) {
        if (work.kind == 2) return work.address_issued && work.data_issued &&
            (work.recovery_id || work.address_replay.translated());
        return std::find(work.issued.begin(), work.issued.end(), false) == work.issued.end();
    };
    const auto consume_issue_events = [&]() {
        for (const auto &event : environment.take_issue_events()) {
            const auto found = attempts.find(event.tag);
            if (found == attempts.end()) { phase += ":unknown-producer-event"; return false; }
            const auto attempt = found->second;
            const auto owner = std::find_if(live.begin(), live.end(), [&](const auto &work) {
                return work.generation == attempt.generation;
            });
            if (owner == live.end()) { phase += ":producer-event-after-owner-removal"; return false; }
            auto &work = *owner;
            const auto expected_kind = attempt.kind == 1 ? IssueKind::load : attempt.kind == 2 ?
                IssueKind::store_address : attempt.kind == 3 ? IssueKind::store_data : IssueKind::vector;
            const auto lane = attempt.kind == 1 ? (work.kind == 0 ? work.load.lane : work.prefetch.lane) :
                attempt.kind == 2 ? work.store.address_lane : attempt.kind == 3 ? work.store.data_lane :
                work.replay[attempt.uop] ? work.replay[attempt.uop]->lane : work.vectors[attempt.uop].lane;
            if (event.kind != expected_kind || event.lane != lane || event.edge < attempt.offered_at) return false;
            attempts.erase(found);
            if (attempt.kind == 2) work.address_offered = false;
            else if (attempt.kind == 3) work.data_offered = false;
            else work.offered[attempt.uop] = false;
            if (event.outcome != IssueOutcome::accepted) {
                if (event.outcome == IssueOutcome::retry) {
                    if (attempt.kind != 4 || event.killed) return false;
                    work.vector_ready[attempt.uop] = event.edge + 9U;
                    ++coverage.stream_transport[2];
                } else {
                    if (!event.killed || (!returning_exception &&
                        environment.memory_violation_stats().count == expected_violations)) return false;
                    ++coverage.stream_transport[3];
                }
                continue;
            }
            if (recovery && recovery->old_response_active()) ++inflight[9];
            if (event.privilege != translation.expected_privilege()) {
                phase += ":accepted-producer-privilege-context"; return false;
            }
            const auto &roots = event.translation_roots;
            const auto &modes = event.translation_modes;
            const auto host_root = (translation.user_pages() ? user_stage1_roots : random_stage1_roots).at(translation.stage1_mode) >> 12;
            const auto vs_root = (translation.user_pages() ? user_vs_roots : random_vs_roots).at(translation.vs_mode) >> 12;
            if ((translation.regime == RandomConstraints::translation_bare && modes[0] != 0) ||
                (translation.regime == RandomConstraints::translation_stage1 &&
                 (roots[0] != host_root || modes[0] != 8U + translation.stage1_mode)) ||
                (translation.regime == RandomConstraints::translation_nested &&
                 (modes[0] != 0 || roots[1] != vs_root || roots[2] != (random_g_roots.at(translation.g_mode) >> 12) ||
                  modes[1] != 8U + translation.vs_mode || modes[2] != 8U + translation.g_mode))) {
                phase += ":accepted-producer-translation-root"; return false;
            }
            const unsigned access_issue = attempt.kind == 1 ? (work.kind == 0 ? 0U : 5U) :
                attempt.kind == 2 ? 1U : attempt.kind == 3 ? 2U : work.kind == 1 ? 3U : 4U;
            ++coverage.stream_access_issue.at(translation.access * 6U + access_issue);
            ++coverage.stream_transport[1];
            coverage.stream_transport[4] += event.killed;
            accepted_ports[event.edge] |= 1U << unsigned(event.kind);
            const auto consume = [&](const auto &source, bool data) {
                if (!source || (data ? work.data_consumed : work.address_consumed)) return true;
                if (!source->complete) { phase += ":dependency-before-producer-completion"; return false; }
                ++coverage.stream_dependencies[data ? 3U : 2U];
                ++coverage.stream_dependencies[source->retired ? 7U : 6U];
                (data ? work.data_consumed : work.address_consumed) = true;
                return true;
            };
            if (attempt.kind != 3 && !consume(work.address_source, false)) return false;
            if (attempt.kind == 3 && !consume(work.data_source, true)) return false;
            unsigned initial_events = 1;
            if (attempt.kind == 1) {
                work.issued[0] = true;
                if (work.permission_probe) ++permission[work.fault() ? 3U : 4U];
                initial_events = !work.refetch;
                coverage.stream_issue[work.kind == 0 ? 0U : 5U] += initial_events;
                if (recovery && work.recovery_id == recovery->id) {
                    ++recovery->issue;
                    if (recovery->issue < recovery->issue_order().size())
                        recovery->ready_cycle = environment.cycle() + recovery->issue_delays()[recovery->issue];
                }
            } else if (attempt.kind == 2) {
                const bool first = !work.address_issued;
                initial_events = first;
                coverage.stream_scalar_retries += !first;
                work.address_retries += !first;
                if (!work.address_replay.accept(event.feedback_cursor, event.edge)) return false;
                work.address_issued = true;
                ++coverage.stream_sta[first ? 0U : 1U];
                coverage.stream_issue[1] += first;
                coverage.stream_sta[10] = std::max<std::uint64_t>(coverage.stream_sta[10],
                    1U + feedback_waits_at(event.edge, &work));
                if (work.paired_initial && first) work.pair_edges[0] = event.edge;
            } else if (attempt.kind == 3) {
                work.data_first = !work.address_issued;
                work.data_issued = true; ++coverage.stream_issue[2];
                if (work.paired_initial) work.pair_edges[1] = event.edge;
                if (recovery && work.recovery_id == recovery->id && recovery->phase == StreamRecovery::Phase::refetch)
                    recovery->promoted[work.recovery_index] = true;
            } else {
                if (attempt.replay) { ++coverage.stream[9]; initial_events = 0; }
                else {
                    if (attempt.uop && vector_load_depends_on_previous(work.vectors[attempt.uop-1], work.vectors[attempt.uop]))
                        ++coverage.vector_operand_chains[2];
                    work.issued[attempt.uop] = true;
                    ++coverage.stream_issue[work.kind == 1 ? 3U : 4U];
                }
                work.replay[attempt.uop].reset();
            }
            if (work.paired_initial && work.pair_edges[0] && work.pair_edges[1]) {
                const auto a = *work.pair_edges[0], d = *work.pair_edges[1];
                auto &pairs = coverage.stream_store_pair;
                ++pairs[a == d ? 0U : a < d ? 1U : 2U];
                const auto skew = std::max(a, d) - std::min(a, d);
                pairs[3] += skew; pairs[4] = std::max(pairs[4], skew);
                work.paired_initial = false;
            }
            coverage.stream[8] += initial_events;
            coverage.stream_store_retirement[7] += (work.kind != 2 || !work.recovery_id) && event.committed_pending;
            const unsigned bin = attempt.kind == 1 ? (work.kind == 0 ? 5U : 9U) :
                attempt.kind == 2 ? 6U : attempt.kind == 3 ? 7U : 8U;
            if (feedback_waits_at(event.edge, &work)) ++coverage.stream_sta[bin];
            last_progress = environment.cycle();
        }
        return environment.ok();
    };
    const auto held_offer = [&]() {
        return std::any_of(attempts.begin(), attempts.end(), [&](const auto &entry) {
            return entry.second.offered_at < environment.cycle();
        });
    };
    const auto allocate_destination = [&](unsigned domain) {
        const unsigned count = domain == 0 ? memblock::kIntegerPhysicalRegisters :
            domain == 1 ? memblock::kFpPhysicalRegisters : memblock::kVectorPhysicalRegisters;
        std::vector<unsigned> available;
        for (unsigned value = 1; value < count; ++value)
            if (!destinations[domain].count(value)) available.push_back(value);
        if (available.empty()) throw std::logic_error("stream destination pool exhausted");
        const auto value = available[random() % available.size()];
        destinations[domain].insert(value);
        return static_cast<std::uint8_t>(value);
    };
    const auto barrier_at_front = [&]() {
        return recovery && !live.empty() && live.front().recovery_id == recovery->id;
    };
    const auto install_head = [&]() {
        if (barrier_at_front()) return environment.set_rob_head_state(
            memblock::rob_pointer_value(recovery->anchor), memblock::rob_pointer_flag(recovery->anchor), false);
        if (!live.empty() && live.front().exception_anchor) return environment.set_rob_head_state(
            memblock::rob_pointer_value(*live.front().exception_anchor),
            memblock::rob_pointer_flag(*live.front().exception_anchor), false);
        return live.empty() || environment.set_rob_head_state(
            live.front().rob(), live.front().flag(), live.front().is_store() &&
                (!live.front().restart_blocked || live.front().restart_flushed));
    };
    const auto graph_work = [&](unsigned index, bool store) -> StreamWork & {
        const auto found = std::find_if(live.begin(), live.end(), [&](const auto &work) {
            return recovery && work.recovery_id == recovery->id && work.recovery_index == index &&
                work.kind == (store ? 2U : 0U);
        });
        if (found == live.end()) throw std::logic_error("missing live stream recovery role");
        return *found;
    };
    const auto begin_recovery = [&](std::uint64_t base, unsigned slots,
        unsigned lq_capacity, unsigned sq_capacity) {
        recovery.emplace();
        auto &episode = *recovery;
        episode.id = ++recovery_serial;
        episode.anchor = rob_offset++;
        episode.base = base & ~std::uint64_t{63};
        episode.plan = make_unresolved_store_graph(random,
            constraints.scalar_unresolved_conflict_per_mille, constraints.scalar_load_policy(),
            std::min({4U, slots - 1U, sq_capacity}));
        const unsigned load_capacity = std::min({4U, slots - unsigned(episode.plan.stores.size()), lq_capacity});
        if (episode.plan.conflict && load_capacity >= 2 &&
            random() % 1000U < constraints.stream_unresolved_inflight_per_mille) {
            ++inflight[0];
            const auto pages = reference_pages_for(episode.base);
            const unsigned first = random() % 64U;
            for (unsigned step = 0; step < 64; ++step) {
                const auto address = (episode.base & ~std::uint64_t{4095}) + ((first + step) % 64U) * 64U;
                const auto physical = pages ? pages->physical_address(address) : address;
                if (address == episode.base || oracle.has_store_in_line(physical)) continue;
                bool prior_effect = false;
                for (unsigned byte = 0; byte < 64 && !prior_effect; ++byte)
                    prior_effect = environment.memory().has_explicit_byte(physical + byte) ||
                        environment.dcache_requests_covering_since(physical + byte, 0) != 0;
                if (prior_effect) continue;
                episode.cold_address = address;
                episode.cold_physical = physical;
                environment.hold_dcache_response_at(physical);
                break; // Arm the hold before another DUT tick or prefetch.
            }
            inflight[1] += !episode.cold_address;
        }
        episode.group = make_unresolved_load_group(random, episode.plan, constraints.scalar_load_policy(),
            load_capacity, episode.cold_address.has_value());
        if (episode.cold_address) {
            const auto &shape = episode.group.loads.back();
            *episode.cold_address += (random() % (64U / shape.width())) * shape.width();
            memblock::LoadTransaction cold;
            cold.address = *episode.cold_address;
            cold.reference_pages = reference_pages_for(cold.address);
            cold.op = static_cast<memblock::LoadOp>(shape.op);
            cold.fp_wen = shape.fp;
            cold.rf_wen = !shape.fp;
            episode.cold_expected = oracle.scalar_load(cold);
            ++inflight[2];
            inflight[7] += shape.fp;
        }
        // This includes unretired ordinary prefix inputs, never DUT/cache data.
        const auto snapshot = oracle.line_snapshot(episode.base, reference_pages_for(episode.base));
        distinguish_unresolved_epochs(episode.plan, episode.group, snapshot);
        episode.bytes = unresolved_graph_bytes(episode.plan);
        episode.image.emplace(snapshot, episode.bytes);
        episode.promoted.resize(episode.plan.stores.size());
        episode.trained.resize(episode.group.loads.size());
        episode.first_ftq = random() % 24U;
        episode.pc = 0x1000U + 32U * (random() % 16U);
        episode.sq_dequeue_limit = environment.sq_allocated() - environment.sq_canceled();
        episode.sq_cancel_before = environment.sq_redirect_canceled_observed();
        episode.scalar_cancel_before = environment.scalar_load_redirect_canceled();
        ++recovery_counts[0];
        ++recovery_counts[episode.plan.conflict ? 1U : 2U];
        recovery_counts[11] += std::any_of(live.begin(), live.end(),
            [&](const auto &work) { return work.accepted() && pending(work); });
        recovery_counts[16] = std::max<std::uint64_t>(recovery_counts[16], episode.plan.stores.size());
        recovery_counts[17] = std::max<std::uint64_t>(recovery_counts[17], episode.group.loads.size());
        ++coverage.stream_recovery_shapes[episode.plan.stores.size() - 1U];
        ++coverage.stream_recovery_shapes[4U + episode.group.loads.size() - 1U];
    };

    const auto prepare_refetch = [&]() {
        auto &episode = *recovery;
        const unsigned victim = episode.refetch_victim;
        reset_unresolved_ftq_suffix(episode.loads, victim);
        for (unsigned index = victim; index < episode.loads.size(); ++index) {
            auto &load_work = graph_work(index, false);
            const bool initially_canceled = episode.rounds == 1 && episode.group.unfinished && index == *episode.group.unfinished;
            if (pending(load_work) || (!initially_canceled && !load_work.checked)) return false;
            if (!initially_canceled) {
                // Each invalidated completed execution was strictly checked.
                // The final execution is sampled at architectural retirement.
                coverage.sample(load_work.load);
                coverage.sample_fp_load(load_work.load, load_work.expected_scalar, 1);
                recovery_counts[15] += load_work.load.fp_wen;
                ++recovery_counts[7];
            }
            auto &load = episode.loads[index];
            load.lq = memblock::lq_pointer_value(lq_offset);
            load.lq_flag = memblock::lq_pointer_flag(lq_offset++);
            load.store_set_hit = episode.trained[index];
            load_work.load = load;
            load_work.expected_scalar = episode.expected(index);
            load_work.issued[0] = load_work.checked = false;
            load_work.retire_ready_cycle.reset();
            load_work.refetch = true;
            environment.expect_load_data(load_work.load, load_work.expected_scalar);
            if (!environment.enqueue_load(load_work.load)) return false;
        }
        episode.phase = StreamRecovery::Phase::refetch;
        episode.issue = 0;
        episode.ready_cycle = environment.cycle() + episode.issue_delays().front();
        return true;
    };

    const auto retire_head = [&]() {
        auto &work = live.front();
        if (work.fault()) { phase += ":fault-cannot-retire"; return false; }
        if (work.permission_probe) ++permission[6];
        coverage.stream_transport[7] += held_offer();
        coverage.stream[5] += live.size() > 1;
        coverage.stream_access_overlap[translation.access * 2U + 1U] += live.size() > 1;
        if (work.recovery_id) ++recovery_counts[work.kind == 2 ? 9U : 10U];
        if (recovery) {
            ++recovery_counts[12];
            inflight[10] += recovery->old_response_active();
        }
        if (work.kind == 2 && !environment.retire_store(work.store)) return false;
        if (work.kind == 3 && !environment.retire_vector_stores(work.vectors)) return false;
        coverage.stream_store_retirement[0] += work.kind == 2;
        coverage.stream_store_retirement[1] += work.kind == 3;
        coverage.stream_store_retirement[2] += work.kind == 3 ? work.vectors.size() : 0;
        if (work.kind == 0) destinations[work.load.fp_wen ? 1U : 0U].erase(work.load.pdest);
        if (work.kind == 1) for (const auto &uop : work.vectors) destinations[2].erase(uop.pdest);
        const auto old_rob = work.rob(); const auto old_flag = work.flag();
        ++coverage.stream_ops[work.kind];
        ++coverage.stream[2]; ++retired; ++actions;
        ++constraint_coverage.actions;
        ++constraint_coverage.operations[stream_operations[work.kind]];
        if (work.kind == 0) {
            coverage.sample(work.load);
            coverage.sample_fp_load(work.load, work.expected_scalar, 1);
            coverage.sample_scalar_format_choice(1, work.load.fp_wen);
            constraint_coverage.sample_dcache_load(std::nullopt);
        } else if (work.kind == 2) coverage.sample(work.store, work.data_first);
        else if (work.kind == 4) coverage.sample(work.prefetch);
        else {
            for (const auto &uop : work.vectors) coverage.sample(uop);
            sample_constrained_vector_instruction(work.kind == 3, work.shape, work.vectors);
        }
        ++coverage.cacheable;
        work.result->retired = true;
        live.pop_front();
        if (live.empty()) {
            if (!environment.set_rob_head_state(old_rob, old_flag, false)) return false;
        } else if (!install_head()) return false;
        coverage.stream_store_retirement[8] +=
            environment.store_retirement_stats().pending_before_last_edge != 0;
        last_progress = environment.cycle();
        return true;
    };

    const auto cancel_suffix = [&](std::uint8_t rob, bool flag, bool flush_itself, bool trap) {
        const auto kills = [&](const StreamWork &item) {
            const bool equal = item.rob() == rob && item.flag() == flag;
            const bool younger = item.flag() == flag ? item.rob() > rob : item.rob() < rob;
            return younger || (flush_itself && equal);
        };
        const auto killed = static_cast<unsigned>(std::count_if(live.begin(), live.end(), kills));
        const auto first_killed = std::find_if(live.begin(), live.end(), kills);
        std::ostringstream detail;
        detail << ":redirect=" << unsigned(rob) << '/' << flag << ":trap=" << trap;
        if (killed == 0 || first_killed == live.end() || !first_killed->oracle_before || recovery ||
            (trap && (&*first_killed != &live.front() || !first_killed->fault() || !first_killed->checked))) return false;
        const auto lq_before = environment.lq_redirect_canceled_observed();
        const auto sq_before = environment.sq_redirect_canceled_observed();
        // Exact expected bytes and identities remain active on E/E+1/E+2.
        // Killed work may complete during propagation or cancel at return;
        // retaining the expectation never waives a wrong/duplicate result.
        returning_exception = trap;
        if (trap) environment.select_store_exception_address(false);
        std::uint64_t captured_fault_va = 0;
        const bool returned = trap
            ? live.front().exception_anchor && environment.return_load_exception_after_nonmemory_wb(
                live.front().load, memblock::rob_pointer_value(*live.front().exception_anchor),
                memblock::rob_pointer_flag(*live.front().exception_anchor), captured_fault_va) &&
                environment.cancel_pending_for_redirect(rob, flag, flush_itself)
            : return_ordinary_redirect(environment, rob, flag, flush_itself);
        if (!returned) {
            phase += detail.str() + ":redirect-drive-or-terminal-cancellation-failed";
            return false;
        }
        if (!consume_issue_events()) return false;
        returning_exception = false;
        if (trap) {
            if (captured_fault_va != live.front().load.address) {
                phase += ":oldest-fault-VA-at-trap-consumption"; return false;
            }
            ++permission[7]; ++permission[16];
            permission[13] += environment.store_retirement_stats().pending != 0;
            permission[14] += killed > 1U;
            permission[15] += killed - 1U;
        }
        if (!check_permission_suppression()) return false;
        ++coverage.stream_cancel[0];
        for (auto &item : live) {
            if (!kills(item)) continue;
            if (item.permission_probe) {
                if (item.fault()) {
                    const bool owner = trap && item.rob() == rob && item.flag() == flag;
                    permission[8] += !owner;
                    permission[10] += !owner && item.checked;
                } else ++permission[9];
            }
            StreamCanceledGeneration record;
            record.kind = item.kind;
            record.scalar_issued = (item.kind == 0 || item.kind == 4) && item.issued[0];
            record.vector_uops = item.vectors.size();
            record.accepted_vector_uops = std::count(item.issued.begin(), item.issued.end(), true);
            if (item.vectors.empty()) record.accepted_vector_uops = 0;
            record.unflushed_vstart = item.needs_restart_flush() && !item.restart_flushed;
            for (unsigned uop = 1; uop < item.vectors.size(); ++uop) {
                const auto &previous = item.vectors[uop - 1U];
                const auto &current = item.vectors[uop];
                if (!memblock::vector_is_special_indexed(current) ||
                    memblock::vector_indexed_vd_index(previous) != memblock::vector_indexed_vd_index(current)) continue;
                ++record.vector_dependencies;
                record.unconsumed_vector_dependencies += !current.store && !item.issued[uop];
            }
            record.unconsumed_address_dependency = item.address_source && !item.address_consumed;
            record.unconsumed_data_dependency = item.data_source && !item.data_consumed;
            if (item.kind == 2) {
                const auto observation = item.address_replay.observe(
                    environment.iq_slow_feedback_stats().sta_samples, item.store);
                if (observation == StoreAddressReplay::Observation::invalid) {
                    phase += ":canceled-store-address-feedback"; return false;
                }
                coverage.stream_sta[2] += observation == StoreAddressReplay::Observation::hit;
                coverage.stream_sta[3] += observation == StoreAddressReplay::Observation::miss;
                record.address_issued = item.address_issued;
                record.data_issued = item.data_issued;
                record.address_retries = item.address_retries;
                record.address_state = !item.address_issued ? StreamCanceledGeneration::AddressState::unissued :
                    item.address_replay.translated() ? StreamCanceledGeneration::AddressState::translated :
                    item.address_replay.waiting() ? StreamCanceledGeneration::AddressState::feedback_pending :
                    StreamCanceledGeneration::AddressState::retry_ready;
            }
            record.sample(coverage.stream_cancel);
        }
        canceled += killed;
        const auto lq_canceled = environment.lq_redirect_canceled_observed() - lq_before;
        const auto sq_canceled = environment.sq_redirect_canceled_observed() - sq_before;
        if (lq_canceled > lq_offset || sq_canceled > sq_offset ||
            !environment.run_cycles(6)) {
            phase += detail.str() + ":queue-recovery-failed";
            return false;
        }
        lq_offset -= lq_canceled;
        sq_offset -= sq_canceled;
        oracle.restore(*first_killed->oracle_before);
        for (const auto &item : live) {
            if (!kills(item)) continue;
            if (item.kind == 0) destinations[item.load.fp_wen ? 1U : 0U].erase(item.load.pdest);
            if (item.kind == 1) for (const auto &uop : item.vectors) destinations[2].erase(uop.pdest);
        }
        for (const auto &[tag, attempt] : attempts)
            for (const auto &item : live)
                if (item.generation == attempt.generation && kills(item)) return false;
        live.erase(std::remove_if(live.begin(), live.end(), kills), live.end());
        target_admissions += killed;
        deadline += std::uint64_t{killed} * constrained_completion_timeout * 4U;
        if (!live.empty() && !install_head()) return false;
        last_progress = environment.cycle();
        phase += ":killed=" + std::to_string(killed) +
            ":lq_cancel=" + std::to_string(lq_canceled) +
            ":sq_cancel=" + std::to_string(sq_canceled);
        return true;
    };

    while (retired < length) {
        phase = "mixed-stream:admitted=" + std::to_string(admitted) +
            ":retired=" + std::to_string(retired) + ":live=" + std::to_string(live.size());
        report_progress();
        if (!consume_issue_events()) return false;
        coverage.stream[7] = std::max<std::uint64_t>(coverage.stream[7], std::count_if(live.begin(), live.end(),
            [&](const auto &work) { return work.accepted() && pending(work); }));
        if (environment.cycle() - last_progress > 2U * constrained_completion_timeout) {
            phase += ":no-accepted-or-retired-progress"; return false;
        }
        if (environment.cycle() > deadline) { phase += ":epoch-deadline"; return false; }

        if (environment.memory_violation_stats().count != expected_violations) {
            const auto &violation = environment.memory_violation_stats();
            if (violation.count != expected_violations + 1U) {
                phase += ":multiple-unhandled-redirects";
                return false;
            }
            std::ostringstream detail;
            detail << ":redirect-recovery:cycle=" << violation.last_cycle
                << ":rob=" << unsigned(violation.last.rob_value) << '/' << violation.last.rob_flag
                << ":ftq=" << unsigned(violation.last.ftq_value) << '/' << unsigned(violation.last.ftq_offset)
                << ":level=" << violation.last.level;
            const auto kills = [&](const StreamWork &item) {
                const bool equal = item.rob() == violation.last.rob_value &&
                    item.flag() == violation.last.rob_flag;
                const bool younger = item.flag() == violation.last.rob_flag
                    ? item.rob() > violation.last.rob_value
                    : item.rob() < violation.last.rob_value;
                return younger || (violation.last.level && equal);
            };
            for (const auto &item : live) {
                detail << ":live={rob=" << unsigned(item.rob()) << '/' << item.flag()
                    << ",kind=" << item.kind << ",issued=" << all_issued(item)
                    << ",pending=" << pending(item) << ",recovery=" << item.recovery_id;
                if (item.kind == 0) detail << ",address=" << item.load.address;
                if (item.kind == 2) detail << ",address=" << item.store.address;
                for (const auto &uop : item.vectors)
                    detail << ",vector=" << uop.address << '/' << unsigned(uop.vuop_idx)
                        << '/' << unsigned(uop.vstart);
                detail << '}';
            }
            const auto victim = std::find_if(live.begin(), live.end(), [&](const StreamWork &item) {
                return item.rob() == violation.last.rob_value && item.flag() == violation.last.rob_flag;
            });
            const auto victim_identity_ok = [&]() {
                if (victim == live.end() || victim->kind == 2U || victim->kind == 3U || victim->kind == 4U)
                    return false;
                if (violation.last.ftq_flag)
                    return false;
                if (victim->kind == 0U)
                    return violation.last.ftq_value == victim->load.ftq_ptr &&
                        violation.last.ftq_offset == victim->load.ftq_offset &&
                        violation.last.is_rvc == victim->load.predecode_rvc;
                return !victim->vectors.empty() &&
                    violation.last.ftq_value == victim->vectors.front().ftq_ptr &&
                    violation.last.ftq_offset == victim->vectors.front().ftq_offset &&
                    !violation.last.is_rvc;
            };
            const auto killed = static_cast<unsigned>(std::count_if(live.begin(), live.end(), kills));
            const auto first_killed = std::find_if(live.begin(), live.end(), kills);
            if (!victim_identity_ok() || killed == 0 || first_killed == live.end() ||
                !first_killed->oracle_before || recovery) {
                phase += detail.str() + ":unmatched-or-nested";
                return false;
            }
            if (!cancel_suffix(violation.last.rob_value, violation.last.rob_flag, violation.last.level, false)) return false;
            expected_violations = violation.count;
            continue;
        }
        for (unsigned i = 0; i < live.size(); ++i) {
            auto &work = live[i];
            if (!work.permission_probe || work.checked || !work.issued[0] || pending(work)) continue;
            work.checked = true;
            ++permission[work.fault() ? 5U : 21U];
            if (work.fault()) {
                // A was allocated before F and remains an incomplete independent
                // nonmemory instruction. The event selects A's actual WB; it
                // cannot defer an already eligible F-head exception.
                work.anchor_wb_ready_cycle = environment.cycle() + 4U;
                permission[11] += i != 0;
                permission[12] += std::any_of(live.begin(), live.begin() + i, [&](const auto &older) {
                    return older.is_store() && (!all_issued(older) || pending(older));
                });
                if (!check_permission_suppression()) return false;
            }
        }
        unsigned waiting_addresses = 0;
        for (auto &work : live) if (work.kind == 2 && !work.recovery_id) {
            const auto observation = work.address_replay.observe(
                environment.iq_slow_feedback_stats().sta_samples, work.store);
            if (observation == StoreAddressReplay::Observation::invalid) {
                phase += ":unexpected-store-address-feedback"; return false;
            }
            coverage.stream_sta[2] += observation == StoreAddressReplay::Observation::hit;
            coverage.stream_sta[3] += observation == StoreAddressReplay::Observation::miss;
            if (work.address_replay.timed_out(environment.cycle(), constrained_completion_timeout)) {
                phase += ":store-address-feedback-timeout"; return false;
            }
            waiting_addresses += work.address_replay.waiting();
        }
        coverage.stream_sta[10] = std::max<std::uint64_t>(coverage.stream_sta[10], waiting_addresses);
        if (recovery) {
            auto &episode = *recovery;
            if (environment.sq_dequeued() > episode.sq_dequeue_limit ||
                environment.sq_redirect_canceled_observed() != episode.sq_cancel_before ||
                environment.scalar_load_redirect_canceled() != episode.scalar_cancel_before + unsigned(episode.cold_canceled)) {
                phase += ":surviving-prefix-queue-contract"; return false;
            }
            if (episode.phase == StreamRecovery::Phase::initial || episode.phase == StreamRecovery::Phase::refetch) {
                bool complete = episode.loads.size() == episode.group.loads.size();
                for (unsigned index = 0; index < episode.loads.size(); ++index) {
                    auto &work = graph_work(index, false);
                    if (episode.phase == StreamRecovery::Phase::initial && episode.group.unfinished && index == *episode.group.unfinished) {
                        complete &= work.issued[0] && environment.dcache_response_held_at(*episode.cold_physical) && pending(work);
                        continue;
                    }
                    if (work.issued[0] && !work.checked && !pending(work)) {
                        if (episode.phase == StreamRecovery::Phase::initial) {
                            if (!episode.obligation->provisional(index)) return false;
                            ++recovery_counts[5];
                        } else {
                            if (!episode.obligation->refetched(index, work.load)) return false;
                            ++recovery_counts[8];
                            inflight[6] += episode.group.unfinished && index == *episode.group.unfinished;
                        }
                        work.checked = true;
                    }
                    complete &= work.checked;
                }
                if (complete) {
                    inflight[3] += episode.phase == StreamRecovery::Phase::initial && episode.group.unfinished.has_value();
                    if (episode.phase == StreamRecovery::Phase::refetch && !episode.obligation->finish_round()) return false;
                    episode.phase = StreamRecovery::Phase::operands;
                    episode.ready_cycle = environment.cycle() + (episode.operand < episode.plan.after_provisional.size()
                        ? episode.plan.after_provisional[episode.operand].delay : 0U);
                }
            }
            if (episode.phase == StreamRecovery::Phase::operands) {
                while (episode.operand < episode.plan.after_provisional.size()) {
                    const auto &event = episode.plan.after_provisional[episode.operand];
                    if (event.kind != UnresolvedOperandKind::data || !graph_work(event.store, true).data_issued) break;
                    if (!episode.promoted[event.store]) return false;
                    ++episode.operand;
                    if (episode.operand < episode.plan.after_provisional.size())
                        episode.ready_cycle = environment.cycle() + episode.plan.after_provisional[episode.operand].delay;
                }
                if (episode.operand == episode.plan.after_provisional.size()) {
                    bool complete = true;
                    for (unsigned index = 0; index < episode.stores.size(); ++index) {
                        const auto &work = graph_work(index, true);
                        complete &= all_issued(work) && !pending(work);
                    }
                    if (complete) {
                        episode.phase = StreamRecovery::Phase::quiet;
                        episode.ready_cycle = environment.cycle() + 32U;
                    }
                }
            }
            if (episode.phase == StreamRecovery::Phase::drain_old) {
                const auto requests = environment.dcache_requests_covering_since(*episode.cold_physical, 0);
                // Prefix helpers may advance many ticks. A complete successor
                // Acquire/Grant between idle samples must restart quiet time.
                if (!environment.dcache_line_grants_idle(*episode.cold_physical) ||
                    (episode.old_quiet_since && requests != episode.old_quiet_requests)) episode.old_quiet_since.reset();
                if (!episode.old_quiet_since && environment.dcache_line_grants_idle(*episode.cold_physical)) {
                    episode.old_quiet_since = environment.cycle();
                    episode.old_quiet_requests = requests;
                }
                if (episode.old_quiet_since && environment.cycle() - *episode.old_quiet_since >= 32U) {
                    if (environment.dcache_response_held_at(*episode.cold_physical) ||
                        pending(graph_work(*episode.group.unfinished, false))) return false;
                    ++inflight[5];
                    if (!prepare_refetch()) return false;
                }
            }
            if (episode.phase == StreamRecovery::Phase::quiet && environment.cycle() >= episode.ready_cycle) {
                if (episode.rounds != episode.group.rounds.size() || episode.rounds != episode.obligation->rounds() ||
                    !episode.obligation->resolved() || !episode.obligation->done()) return false;
                for (unsigned index = 0; index < episode.loads.size(); ++index)
                    if (graph_work(index, false).expected_scalar != episode.expected(index)) return false;
                ++recovery_counts[19];
                recovery_counts[18] = std::max<std::uint64_t>(recovery_counts[18], episode.rounds);
                ++coverage.stream_recovery_shapes[8U + episode.rounds];
                recovery.reset();
                if (!install_head()) return false;
            }
        }

        for (auto &work : live) for (unsigned u = 0; u < work.vectors.size(); ++u) {
            if (!work.issued[u] || work.replay[u] || work.offered[u]) continue;
            memblock::VectorMemoryTransaction replay;
            bool found = false;
            if (!environment.poll_vector_replay(work.vectors[u], replay, found)) return false;
            if (found) work.replay[u] = replay;
        }

        const auto lq_live = environment.lq_allocated() - environment.lq_canceled() - environment.lq_dequeued();
        const auto sq_live = environment.sq_allocated() - environment.sq_canceled() - environment.sq_dequeued();
        const auto lq_free = memblock::kVirtualLoadQueueEntries - memblock::kLqEnqueueHeadroom - lq_live;
        const auto sq_free = memblock::kStoreQueueEntries - memblock::kSqEnqueueHeadroom - sq_live;
        const auto fits_vector = [&](std::uint64_t capacity) {
            return std::any_of(vector_shapes.begin(), vector_shapes.end(),
                [&](const auto &shape) { return vector_shape_total_flows(shape) <= capacity; });
        };
        std::array<unsigned, 5> weights{};
        for (unsigned kind = 0; kind < 5; ++kind) {
            const auto capacity = kind == 2 || kind == 3 ? sq_free : lq_free;
            const bool fits = kind == 1 || kind == 3 ? fits_vector(capacity) : capacity != 0;
            if (fits) weights[kind] = constraints.operation_weights[stream_operations[kind]];
        }
        // 0 = admission, 1 = scalar load/prefetch, 2 = STA, 3 = STD,
        // 4 = vector uop, 5 = architectural retirement, 8 = VSTART flush-after.
        // Candidates are regenerated from current obligations.
        struct Event { unsigned kind, instruction, uop; };
        std::vector<Event> ready;
        // Completion and backend retirement are separate producer states.
        // A scalar scommit edge precedes changed pendingPtr by one edge; vector
        // stores emit no scommit but use the same explicit retirement edge.
        // Prep/nonmemory vector uops are abstracted as already completed.
        for (auto &work : live) {
            if (work.fault() || !all_issued(work) || pending(work)) continue;
            if (!work.retire_ready_cycle) {
                const auto latency = work.kind == 1 || work.kind == 3
                    ? memblock::kStreamVectorWritebackToHead : memblock::kStreamScalarWritebackToHead;
                work.retire_ready_cycle = environment.cycle() + latency - (work.is_store() ? 2U : 1U);
                if (work.needs_restart_flush()) work.restart_block_cycle = environment.cycle() + 2U;
            }
        }
        const bool completed_head = !barrier_at_front() && !live.empty() &&
            all_issued(live.front()) && !pending(live.front());
        const bool restart_pending = completed_head && live.front().needs_restart_flush() &&
            !live.front().restart_flushed;
        if (restart_pending) {
            auto &head = live.front();
            // Keep pendingst high while memory execution needs the oldest
            // store (notably cross-page misalignment). ROB flush blocking can
            // first be sampled low at E+3, after its final successful WB.
            if (!head.restart_blocked && head.restart_block_cycle &&
                environment.cycle() >= *head.restart_block_cycle) {
                head.restart_blocked = true;
                if (!install_head()) return false;
                head.restart_ready_cycle = environment.cycle() + 8U + random() % 9U;
            }
            if (head.restart_ready_cycle && environment.cycle() >= *head.restart_ready_cycle)
                ready.push_back({8, 0, 0});
        }
        if (completed_head && live.front().fault()) {
            auto &head = live.front();
            if (!head.exception_anchor) { phase += ":missing-declared-exception-anchor"; return false; }
            if (head.anchor_wb_ready_cycle && environment.cycle() >= *head.anchor_wb_ready_cycle)
                ready.push_back({11, 0, 0});
        }
        const bool retire_ready = completed_head && !live.front().fault() && !restart_pending && live.front().retire_ready_cycle &&
            environment.cycle() >= *live.front().retire_ready_cycle;
        if (retire_ready) ready.push_back({5, 0, 0});
        // A nonzero-VSTART prep uop records a full-instruction flush-after.
        // Serialize its suffix as a chosen constraint; retain older work.
        const bool restart_barrier = std::any_of(live.begin(), live.end(),
            [](const auto &work) { return work.needs_restart_flush(); });
        if (!restart_barrier && admitted < target_admissions && live.size() < depth &&
            (!recovery || recovery->phase == StreamRecovery::Phase::admission) &&
            std::accumulate(weights.begin(), weights.end(), std::uint64_t{0}) != 0)
            ready.push_back({0, 0, 0});
        if (recovery && recovery->phase == StreamRecovery::Phase::release_old && environment.cycle() >= recovery->ready_cycle)
            ready.push_back({7, 0, 0});
        // Never feed observed DUT data back into generation. The scoreboard
        // validates a producer before this external backend releases consumers.
        for (auto &work : live)
            if (work.kind == 0 && !work.fault() && !work.recovery_id && work.issued[0] && !pending(work))
                work.result->complete = true;
        bool older_addresses_known = true, prefix_addresses_known = true;
        if (recovery && recovery->phase == StreamRecovery::Phase::operands &&
            recovery->operand < recovery->plan.after_provisional.size() && environment.cycle() >= recovery->ready_cycle) {
            const auto &operand = recovery->plan.after_provisional[recovery->operand];
            const bool conflict = operand.kind == UnresolvedOperandKind::address &&
                std::any_of(recovery->group.loads.begin(), recovery->group.loads.end(), [&](const auto &load) {
                    return unresolved_overlap(recovery->bytes[operand.store], load);
                });
            // The actual ROB drops pendingst during redirect/walk. This bounded
            // direction admits a conflict only with a non-store head or A.
            if (environment.issue_session_empty() && (!conflict || barrier_at_front() || !live.front().is_store()))
                ready.push_back({6, operand.store, 0});
        }
        for (unsigned i = 0; i < live.size(); ++i) {
            auto &work = live[i];
            const bool graph_role = recovery && work.recovery_id == recovery->id;
            const bool head_eligible = !work.requires_head || (i == 0 && !barrier_at_front());
            const bool address_ready = !work.address_source || work.address_source->complete;
            const bool data_ready = !work.data_source || work.data_source->complete;
            if (work.kind == 0 && graph_role) {
                const auto &episode = *recovery;
                const bool executing = episode.phase == StreamRecovery::Phase::initial ||
                    episode.phase == StreamRecovery::Phase::refetch;
                bool data_ready = true;
                for (unsigned index = 0; index < episode.stores.size(); ++index) {
                    const auto &store = graph_work(index, true);
                    if (episode.phase == StreamRecovery::Phase::initial ? episode.plan.stores[index].data_before_load : store.address_issued)
                        data_ready &= store.data_issued;
                }
                if (executing && !work.issued[0] && !work.offered[0] &&
                    environment.issue_session_available(IssueKind::load, work.load.lane) && prefix_addresses_known && data_ready &&
                    episode.issue < episode.issue_order().size() &&
                    episode.issue_order()[episode.issue] == work.recovery_index && environment.cycle() >= episode.ready_cycle)
                    ready.push_back({1, i, 0});
            } else if ((work.kind == 0 || work.kind == 4) && !work.issued[0] && !work.offered[0] &&
                environment.issue_session_available(IssueKind::load, work.kind == 0 ? work.load.lane : work.prefetch.lane) && head_eligible && address_ready &&
                // The reserved fault page is disjoint from every older store.
                // CPU load issue can speculate before older store addresses.
                (work.fault() || work.kind == 4 || older_addresses_known)) ready.push_back({1, i, 0});
            if (work.kind == 2 && graph_role) {
                const auto &episode = *recovery;
                const bool early = (episode.phase == StreamRecovery::Phase::admission ||
                    episode.phase == StreamRecovery::Phase::initial) && episode.plan.stores[work.recovery_index].data_before_load;
                const bool promote = episode.phase == StreamRecovery::Phase::refetch && work.address_issued;
                if (!work.data_issued && !work.data_offered &&
                    environment.issue_session_available(IssueKind::store_data, work.store.data_lane) && (early || promote)) ready.push_back({3, i, 0});
            } else if (work.kind == 2) {
                // Beat-crossing STA must not block older retirement.
                if (!work.recovery_id && !work.address_offered &&
                    environment.issue_session_available(IssueKind::store_address, work.store.address_lane) &&
                    head_eligible && address_ready && work.address_replay.ready(environment.cycle()))
                    ready.push_back({2, i, 0});
                if (!work.data_issued && !work.data_offered &&
                    environment.issue_session_available(IssueKind::store_data, work.store.data_lane) && data_ready) ready.push_back({3, i, 0});
            }
            if (work.kind == 1 || work.kind == 3) {
                const bool ordered_load = work.kind == 1 && work.vectors.front().addressing ==
                    memblock::VectorAddressingMode::indexed_ordered;
                if (head_eligible && address_ready && (!ordered_load || i == 0) && (work.kind == 3 || older_addresses_known)) {
                    for (unsigned u = 0; u < work.vectors.size(); ++u) {
                        if (work.offered[u] || environment.cycle() < work.vector_ready[u]) continue;
                        const auto &offer = work.replay[u] ? *work.replay[u] : work.vectors[u];
                        if (!environment.issue_session_available(IssueKind::vector, offer.lane)) continue;
                        if (work.issued[u]) {
                            if (work.replay[u]) ready.push_back({10, i, u});
                            continue;
                        }
                        if (work.vectors[u].addressing != memblock::VectorAddressingMode::indexed_unordered &&
                            u != unsigned(std::count(work.issued.begin(), work.issued.end(), true))) continue;
                        if (u && vector_load_depends_on_previous(work.vectors[u-1], work.vectors[u]) &&
                            (!work.issued[u-1] || environment.vector_memory_pending(work.vectors[u-1]))) continue;
                        ready.push_back({4, i, u});
                    }
                }
            }
            if (work.kind == 2) {
                const bool known = work.address_issued && !environment.scalar_store_address_pending(work.store);
                older_addresses_known &= known;
                if (!graph_role) prefix_addresses_known &= known;
            }
            if (work.kind == 3) {
                older_addresses_known &= all_issued(work) && !pending(work);
                prefix_addresses_known &= all_issued(work) && !pending(work);
            }
        }
        if (!environment.issue_session_empty()) ready.push_back({9, 0, 0});
        if (ready.empty()) {
            if (!environment.run_cycles(1)) return false;
            if (environment.cycle() - last_progress > 2U * constrained_completion_timeout) {
                phase += ":no-accepted-or-retired-progress";
                return false;
            }
            continue;
        }
        const auto event = ready[random() % ready.size()];
        if (completed_head && !live.front().exception_anchor &&
            event.kind != 5 && event.kind != 7 && event.kind != 8 && event.kind != 9 && event.kind != 11)
            ++coverage.stream_waiting_head[event.kind == 0 ? 0U : 1U];
        if (event.kind == 9) {
            if (!environment.run_cycles(1)) return false;
            continue;
        }
        if (event.kind == 11) {
            if (!cancel_suffix(live.front().rob(), live.front().flag(), true, true)) return false;
        } else if (event.kind == 5) {
            if (!retire_head()) return false;
        } else if (event.kind == 8) {
            auto &head = live.front();
            if (live.size() != 1 || recovery || !restart_pending) return false;
            const auto lq_cancel = environment.lq_redirect_canceled_observed();
            const auto sq_cancel = environment.sq_redirect_canceled_observed();
            // pendingPtr stays at this instruction, with pendingst low during
            // the ROB flush block and conservative local recovery guard.
            if (!environment.redirect_after(head.rob(), head.flag(), false) ||
                environment.lq_redirect_canceled_observed() != lq_cancel ||
                environment.sq_redirect_canceled_observed() != sq_cancel ||
                !environment.run_cycles(6)) return false;
            head.restart_flushed = true;
            ++coverage.stream_vstart_flush[2];
            coverage.stream_vstart_flush[3] += environment.store_retirement_stats().pending != 0;
            if (!install_head()) return false;
        } else if (event.kind == 7) {
            if (!environment.release_held_dcache_response_at(*recovery->cold_physical)) return false;
            recovery->phase = StreamRecovery::Phase::drain_old;
            recovery->old_quiet_since.reset();
        } else if (event.kind == 6) {
            if (!environment.end_issue_session()) return false;
            ++coverage.stream_transport[8];
            if (recovery->old_response_active()) ++inflight[9];
            auto &episode = *recovery;
            const auto operand = episode.plan.after_provisional.at(episode.operand++);
            auto &work = graph_work(operand.store, true);
            if (operand.kind == UnresolvedOperandKind::data) {
                if (work.data_issued || !environment.issue_store_data(work.store, constrained_completion_timeout)) return false;
                work.data_first = !work.address_issued;
                work.data_issued = true;
                ++coverage.stream_issue[2];
            } else {
                if (work.address_issued || !environment.issue_store_address_until_tlb_hit(work.store, constrained_completion_timeout) ||
                    !episode.image->resolve(operand.store)) return false;
                work.address_issued = true;
                ++coverage.stream_issue[1];
                unsigned victim = episode.loads.size();
                for (unsigned index = 0; index < episode.group.loads.size(); ++index)
                    if (unresolved_overlap(episode.bytes[operand.store], episode.group.loads[index])) { victim = index; break; }
                if (victim != episode.loads.size()) {
                    const auto &round = episode.group.rounds.at(episode.rounds);
                    if (round.store != operand.store || round.victim != victim ||
                        (!barrier_at_front() && live.front().is_store())) return false;
                    // No blocking prefix helper may consume the two-register
                    // backend redirect deadline after the successful STA.
                    for (unsigned cycle = 0; cycle < constrained_completion_timeout &&
                         environment.memory_violation_stats().count == expected_violations; ++cycle)
                        if (!environment.run_cycles(1)) return false;
                    const auto violation = environment.memory_violation_stats();
                    if (violation.count != expected_violations + 1U ||
                        !episode.obligation->redirect(violation.last, victim)) return false;
                    const auto cancel_before = environment.lq_redirect_canceled_observed();
                    const auto return_cycle = violation.last_cycle + 2U;
                    if (environment.cycle() > return_cycle ||
                        !environment.run_cycles(static_cast<unsigned>(return_cycle - environment.cycle())) ||
                        !environment.redirect_after(episode.loads[victim].rob, episode.loads[victim].rob_flag, true)) return false;
                    if (episode.rounds == 0 && episode.group.unfinished) {
                        const auto &cold = episode.loads[*episode.group.unfinished];
                        if (environment.lq_redirect_canceled_observed() == cancel_before ||
                            !environment.dcache_response_held_at(*episode.cold_physical) ||
                            !pending(graph_work(*episode.group.unfinished, false)) ||
                            !environment.cancel_scalar_load_after_redirect(cold) ||
                            !episode.obligation->initial_canceled(*episode.group.unfinished)) return false;
                        episode.cold_canceled = true;
                        ++inflight[4];
                    }
                    if (!episode.obligation->returned() || !environment.run_cycles(8)) return false;
                    const auto canceled = environment.lq_redirect_canceled_observed() - cancel_before;
                    if (canceled > episode.loads.size() - victim || canceled > lq_offset ||
                        environment.sq_redirect_canceled_observed() != episode.sq_cancel_before ||
                        environment.sq_dequeued() > episode.sq_dequeue_limit) return false;
                    lq_offset -= canceled;
                    recovery_counts[14] += canceled;
                    ++expected_violations;
                    ++episode.rounds;
                    ++recovery_counts[6];
                    ++coverage.stream_recovery_victims[victim];
                    for (unsigned index = 0; index < victim; ++index)
                        if (graph_work(index, false).expected_scalar != episode.expected(index)) return false;
                    episode.trained[victim] = true;
                    episode.refetch_victim = victim;
                    if (episode.rounds == 1 && episode.group.unfinished) {
                        episode.phase = StreamRecovery::Phase::release_old;
                        const auto gap = random() % 8U;
                        inflight[8] += gap;
                        episode.ready_cycle = environment.cycle() + gap;
                    } else if (!prepare_refetch()) return false;
                }
            }
            ++coverage.stream[8];
            if (episode.phase == StreamRecovery::Phase::operands && episode.operand < episode.plan.after_provisional.size())
                episode.ready_cycle = environment.cycle() + episode.plan.after_provisional[episode.operand].delay;
            if (!environment.begin_issue_session()) return false;
        } else if (event.kind == 0) {
            coverage.stream_transport[6] += held_offer();
            const bool had_accepted_pending = std::any_of(live.begin(), live.end(),
                [&](const auto &work) { return work.accepted() && pending(work); });
            coverage.stream[3] += had_accepted_pending;
            coverage.stream_access_overlap[translation.access * 2U] += had_accepted_pending;
            coverage.stream_history[2] += admitted >= 32U && had_accepted_pending;
            coverage.stream[4] += retired != 0 && !live.empty();
            StreamWork work;
            work.generation = admitted;
            if (!recovery) {
                work.oracle_before = std::make_shared<StreamByteOracle::Checkpoint>(oracle.checkpoint());
                auto selection = random() % std::accumulate(weights.begin(), weights.end(), std::uint64_t{0});
                for (work.kind = 0; work.kind < 4 && selection >= weights[work.kind]; ++work.kind)
                    selection -= weights[work.kind];
            } else work.kind = recovery->stores.size() < recovery->plan.stores.size() ? 2U : 0U;
            auto line = recovery ? recovery->base : random() % 4U != 0 ? arena + (random() % 8U) * 64U :
                constrained_cacheable_address(64, translation, leaf, false);
            const unsigned slots = std::min<unsigned>(depth - live.size(), target_admissions - admitted);
            if (!recovery && work.kind == 2 && slots >= 2 && lq_free != 0 && sq_free != 0 &&
                weights[0] != 0 && std::none_of(live.begin(), live.end(), [](const auto &older) { return older.fault(); }) &&
                random() % 1000U < constraints.stream_unresolved_per_mille)
                begin_recovery(line, slots, lq_free, sq_free);
            const bool graph_admission = recovery.has_value();
            if (graph_admission) {
                work.recovery_id = recovery->id;
                work.recovery_index = work.kind == 2 ? recovery->stores.size() : recovery->loads.size();
                line = recovery->base;
            } else recovery_counts[13] += std::any_of(live.begin(), live.end(),
                [](const auto &older) { return older.recovery_id != 0; });
            work.permission_probe = !graph_admission && work.kind == 0 &&
                permission_probes < permission_budget &&
                std::none_of(live.begin(), live.end(), [](const auto &older) { return older.fault(); }) &&
                random() % 1000U < constraints.stream_permission_load_per_mille;
            std::optional<std::uint64_t> address_value, data_value;
            if (!graph_admission && !work.permission_probe && constraints.stream_dependency_per_mille != 0) {
                std::vector<const StreamWork *> sources;
                for (const auto &older : live)
                    if (older.kind == 0 && !older.fault() && !older.load.fp_wen && !older.recovery_id)
                        sources.push_back(&older);
                const auto choose_source = [&](bool data) {
                    if (sources.empty() || random() % 1000U >= constraints.stream_dependency_per_mille) return;
                    const auto &source = *sources[random() % sources.size()];
                    (data ? work.data_source : work.address_source) = source.result;
                    (data ? data_value : address_value) = source.expected_scalar;
                    work.result->depth = std::max(work.result->depth, source.result->depth + 1U);
                    auto &counts = coverage.stream_dependencies;
                    ++counts[data ? 1U : 0U];
                    counts[data ? 5U : 4U] += !source.result->complete;
                    counts[8] = std::max<std::uint64_t>(counts[8], work.result->depth);
                };
                choose_source(false);
                if (work.kind == 2) choose_source(true);
            }
            // Model a bounded integer ALU address calculation in this page.
            // Preserve the independently chosen byte/alignment class. The seed
            // supplies the addend, the producer supplies the line displacement.
            const auto dependent_address = [&](std::uint64_t address) {
                if (!address_value) return address;
                const auto offset = ((address & 0xfffU) + ((*address_value & 63U) << 6U)) & 0xfffU;
                return (address & ~std::uint64_t{0xfff}) | offset;
            };
            // Reserve a separate one-uop nonmemory ROB entry before F exists.
            // Its operands may remain unavailable independently of F. It owns
            // no LSQ entry and cannot compress with the noncompressible load.
            if (work.permission_probe && !translation.mxr) work.exception_anchor = rob_offset++;
            if (work.kind == 0) {
                const auto format = graph_admission ? ScalarLoadShape{recovery->group.loads[work.recovery_index].op,
                    recovery->group.loads[work.recovery_index].fp} : constraints.scalar_load_policy().choose(random);
                const bool cold_role = graph_admission && recovery->group.unfinished && work.recovery_index == *recovery->group.unfinished;
                work.load = make_load(cold_role ? *recovery->cold_address : graph_admission ? line + recovery->group.loads[work.recovery_index].offset :
                    dependent_address(constrained_window_address(line, 1U << (format.op & 3U))),
                    static_cast<memblock::LoadOp>(format.op), random() % memblock::kScalarLoadLanes, format.fp);
                work.load.pdest = allocate_destination(format.fp ? 1U : 0U);
                if (work.permission_probe) {
                    ++permission_probes; ++permission[0]; ++permission[translation.mxr ? 2U : 1U];
                    const unsigned width = 1U << (format.op & 3U);
                    work.load.address = permission_virtual_base + (random() % permission_page_count) * 0x1000ULL +
                        (random() % (4096U / width)) * width;
                    work.load.reference_pages.reset();
                    work.load.oracle_address = stream_permission_physical(translation, work.load.address);
                    work.load.expected_exception_mask = translation.mxr ? 0U : memblock::kExceptionLoadPageFault;
                    if (!validate_stream_permission_load(work.load, translation)) return false;
                    if (work.fault()) denied_pages.emplace_back(*work.load.oracle_address & ~std::uint64_t{4095},
                        environment.tilelink_requests());
                }
                if (graph_admission) {
                    auto &episode = *recovery;
                    work.load.predecode_rvc = (format.op == 3U || (!format.fp && format.op == 2U)) && (random() & 1U);
                    work.load.pc = episode.pc;
                    work.load.ftq_ptr = episode.first_ftq;
                    work.load.ftq_offset = episode.halfword;
                    episode.pc += work.load.predecode_rvc ? 2U : 4U;
                    episode.halfword += work.load.predecode_rvc ? 1U : 2U;
                    work.load.store_set_hit = work.load.load_wait_bit = work.load.load_wait_strict = false;
                    work.load.wait_for_rob_flag = false;
                    work.load.wait_for_rob_value = 0;
                    work.expected_scalar = episode.expected(work.recovery_index);
                    episode.loads.push_back(work.load);
                    ++recovery_counts[4];
                } else work.expected_scalar = oracle.scalar_load(work.load);
                work.requires_head = (work.load.address & 15U) + (1U << (format.op & 3U)) > 16U;
                work.issued.assign(1, false);
                work.offered.assign(1, false);
                for (unsigned byte = 0; !work.permission_probe && byte < (1U << (static_cast<unsigned>(work.load.op) & 3U)); ++byte) {
                    if (!validate_translation_byte(work.load.address + byte,
                            memblock::reference_byte_address(work.load, work.load.address + byte),
                            false, translation)) return false;
                }
                environment.expect_load_data(work.load, work.expected_scalar);
                if (!environment.enqueue_load(work.load)) return false;
            } else if (work.kind == 2) {
                if (graph_admission && !recovery->stores.empty()) ++rob_offset;
                const auto op = static_cast<memblock::StoreOp>(graph_admission ?
                    std::countr_zero(recovery->bytes[work.recovery_index].width) : random() % 4U);
                const unsigned width = 1U << static_cast<unsigned>(op);
                auto address = graph_admission ? line + recovery->bytes[work.recovery_index].offset :
                    dependent_address(constrained_window_address(line, width));
                if ((address & 0xfffU) + width > 0x1000U)
                    address = (address & ~std::uint64_t{0xfff}) | (0x1000U - width);
                work.store = make_store(address, graph_admission ? recovery->bytes[work.recovery_index].data : data_value ? *data_value : random(),
                    op, random() % memblock::kScalarStoreLanes);
                if (graph_admission) { recovery->stores.push_back(work.store); ++recovery_counts[3]; }
                if ((address & 15U) + width > 16U) {
                    work.store.expected_address_writeback_lane = 0;
                    work.requires_head = true;
                }
                for (unsigned byte = 0; byte < width; ++byte) {
                    if (!validate_translation_byte(work.store.address + byte,
                            memblock::reference_byte_address(work.store, work.store.address + byte),
                            true, translation)) return false;
                }
                oracle.store(work.store);
                environment.expect_store(work.store);
                if (!environment.enqueue_store(work.store) ||
                    !environment.track_store_release_inputs(work.store)) return false;
            } else if (work.kind == 4) {
                work.prefetch = make_prefetch(dependent_address(line), static_cast<memblock::PrefetchOp>(8U + random() % 3U),
                    random() % memblock::kScalarLoadLanes);
                work.issued.assign(1, false);
                work.offered.assign(1, false);
                if (!validate_translation_byte(work.prefetch.address,
                        memblock::reference_byte_address(work.prefetch, work.prefetch.address), false, translation)) return false;
                environment.expect_prefetch(work.prefetch);
                if (!environment.enqueue_prefetch(work.prefetch)) return false;
            } else {
                const bool store = work.kind == 3;
                work.shape = choose_vector_shape(store ? 1U : 0U, store ? sq_free : lq_free, false);
                const auto eew = work.shape.addressing >= 2 ? work.shape.vsew : work.shape.eew;
                work.vectors = make_random_vector_uops(store, dependent_address(constrained_window_address(line, 1U << eew)),
                    work.shape, random() % 1000U < constraints.vector_corner_per_mille);
                work.issued.assign(work.vectors.size(), false);
                work.offered.assign(work.vectors.size(), false);
                work.vector_ready.resize(work.vectors.size());
                work.replay.resize(work.vectors.size());
                if (work.needs_restart_flush()) ++coverage.stream_vstart_flush[store ? 1U : 0U];
                if (!store) {
                    for (auto &uop : work.vectors) uop.pdest = allocate_destination(2);
                    oracle.prepare_vector_load(work.vectors);
                }
                for (const auto &uop : work.vectors) {
                    if (!validate_vector_translation(uop)) return false;
                    const unsigned width = 1U << memblock::vector_data_eew(uop);
                    const auto active = memblock::active_vector_elements(uop);
                    for (unsigned element = 0; element < 16U / width; ++element) {
                        if (!((active >> element) & 1U)) continue;
                        const auto address = memblock::vector_element_virtual_address(uop, element);
                        work.requires_head |= store ? (address & (width - 1U)) != 0 :
                            (address & 15U) + width > 16U;
                    }
                    if (store) { oracle.store(uop); environment.expect_vector(uop); }
                    else environment.expect_vector_data(uop, oracle.vector_load(uop));
                    if (!environment.enqueue_vector(uop) ||
                        (store && !environment.track_vector_release_inputs(uop))) return false;
                }
                coverage.stream_vector_uops[store ? 1U : 0U] += work.vectors.size();
            }
            // Reuse is legal only after the previous architectural lifetime.
            if (std::any_of(live.begin(), live.end(), [&](const auto &older) {
                return work.rob() == older.rob() && work.flag() == older.flag();
            })) { phase += ":live-rob-identity-reuse"; return false; }
            coverage.stream_history[3] += !admitted_rob_identities.insert(
                unsigned(work.rob()) + (work.flag() ? memblock::kRobEntries : 0U)).second;
            const bool was_empty = live.empty();
            live.push_back(std::move(work));
            note_wait_overlap(4);
            coverage.stream_store_retirement[6] +=
                environment.store_retirement_stats().pending_before_last_edge != 0; ++admitted; ++coverage.stream[1];
            if (was_empty && !install_head()) return false;
            if (graph_admission && recovery->loads.size() == recovery->group.loads.size()) {
                recovery->obligation.emplace(recovery->loads, recovery->stores.size(), recovery->group.unfinished);
                recovery->phase = StreamRecovery::Phase::initial;
                recovery->ready_cycle = environment.cycle() + recovery->group.issue_delays.front();
            }
        } else {
            auto &work = live[event.instruction];
            ++coverage.stream_transport[9];
            const bool pair_operands = work.kind == 2 && !work.recovery_id && !work.address_issued &&
                !work.data_issued && !work.address_offered && !work.data_offered && (event.kind == 2 || event.kind == 3) &&
                std::any_of(ready.begin(), ready.end(), [&](const auto &other) {
                    return other.instruction == event.instruction && other.kind == (event.kind == 2 ? 3U : 2U);
                }) && constraints.stream_store_pair_per_mille != 0 &&
                random() % 1000U < constraints.stream_store_pair_per_mille;
            const auto offer = [&](unsigned kind, unsigned uop, bool replay) {
                const auto tag = next_attempt++;
                const bool accepted = kind == 1 ? (work.kind == 0 ? environment.offer_load(tag, work.load) :
                    environment.offer_prefetch(tag, work.prefetch)) : kind == 2 ? environment.offer_store_address(tag, work.store) :
                    kind == 3 ? environment.offer_store_data(tag, work.store) :
                    environment.offer_vector(tag, replay ? *work.replay[uop] : work.vectors[uop]);
                if (!accepted) return false;
                if (kind == 2) work.address_offered = true;
                else if (kind == 3) work.data_offered = true;
                else work.offered[uop] = true;
                attempts.emplace(tag, Attempt{work.generation, kind, uop, replay, environment.cycle()});
                ++coverage.stream_transport[0];
                coverage.stream_transport[5] = std::max<std::uint64_t>(coverage.stream_transport[5], attempts.size());
                return true;
            };
            if (pair_operands) {
                work.paired_initial = true;
                if (!offer(2, 0, false) || !offer(3, 0, false)) return false;
            } else if (!offer(event.kind == 10 ? 4U : event.kind, event.uop, event.kind == 10)) return false;
            // Selecting another producer at this same boundary permits genuine
            // heterogeneous handshakes. A later clock/admission/head edge
            // advances every lane; no offer waits inside a blocking helper.
            continue;
        }
        coverage.stream[6] = std::max<std::uint64_t>(coverage.stream[6], live.size());
        const auto accepted_pending = std::count_if(live.begin(), live.end(),
            [&](const auto &work) { return work.accepted() && pending(work); });
        coverage.stream[7] = std::max<std::uint64_t>(coverage.stream[7], accepted_pending);
        last_progress = environment.cycle();
        if (!environment.run_cycles(random() % 4U)) return false;
    }
    phase = "mixed-stream-drain";
    if (!consume_issue_events() || !attempts.empty() || !environment.end_issue_session()) return false;
    for (const auto &[edge, ports] : accepted_ports) ++coverage.stream_issue_ports[ports];
    if (recovery || environment.memory_violation_stats().count != expected_violations ||
        !environment.run_until_queues_retired(constrained_completion_timeout)) return false;
    const auto &store_retirement = environment.store_retirement_stats();
    if (store_retirement.pending != 0) return false;
    coverage.stream_store_retirement[3] = store_retirement.pending_at_retirement;
    coverage.stream_store_retirement[4] = store_retirement.already_dequeued;
    coverage.stream_store_retirement[5] = store_retirement.max_pending;
    coverage.stream_store_retirement[9] += store_retirement.drained - drained_before;
    coverage.stream_cancel[22] = std::max<std::uint64_t>(coverage.stream_cancel[22], canceled);
    coverage.stream_cancel[23] += canceled != 0;
    coverage.stream_cancel[24] = std::max<std::uint64_t>(coverage.stream_cancel[24], admitted);
    coverage.stream_cancel[25] += admitted > 32U;
    coverage.stream_history[0] += retired > 32U;
    coverage.stream_history[1] = std::max<std::uint64_t>(coverage.stream_history[1], retired);
    coverage.stream[11] = actions;
    constraint_coverage.sample_dcache(requests_before, environment.tilelink_requests());
    constraint_coverage.sample_translation(translation, leaf, ptw_before,
        environment.ptw_requests(), actions - start_action, actions - start_action);
    permission[19] = std::max<std::uint64_t>(permission[19], permission_probes);
    permission[20] += permission_probes != 0;
    return environment.end_store_release_epoch(constrained_completion_timeout) &&
        (denied_pages.empty() || (environment.run_cycles(32) &&
            environment.run_until_dcache_idle(constrained_completion_timeout) && check_permission_suppression())) &&
        environment.run_cycles(8) && leave_stream_access_context() && check_permission_suppression();
}
} // namespace memblock::scenarios::mixed
