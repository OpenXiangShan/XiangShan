#include "mixed/scenario.hpp"
#include "mixed/stream_oracle.hpp"
#include "mixed/stream_recovery.hpp"

namespace memblock::scenarios::mixed {
namespace {
// Architectural age belongs to this deque, independent of LSQ dequeue order.
struct StreamWork {
    unsigned kind = 0;
    memblock::LoadTransaction load;
    memblock::StoreTransaction store;
    memblock::PrefetchTransaction prefetch;
    std::vector<memblock::VectorMemoryTransaction> vectors;
    RandomVectorShape shape{};
    std::vector<bool> issued;
    bool address_issued = false;
    bool data_issued = false;
    bool requires_head = false;
    bool data_first = false;
    std::uint64_t last_address_issue = 0;
    std::uint64_t expected_scalar = 0;
    unsigned recovery_id = 0, recovery_index = 0;
    bool refetch = false, checked = false;
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
    const auto translation = choose_translation_context();
    const auto leaf = choose_translation_leaf(translation);
    if (!enter_translation_context(translation) ||
        !issue_constrained_fence(RandomConstraints::scalar_load) ||
        !environment.run_until_all_complete(constrained_completion_timeout) ||
        !environment.run_until_queues_retired(constrained_completion_timeout)) return false;
    const auto ptw_before = environment.ptw_requests();
    const auto requests_before = environment.tilelink_requests();
    const auto arena = constrained_cacheable_address(64, translation, leaf, false);
    StreamByteOracle oracle(environment.memory());
    std::deque<StreamWork> live;
    std::array<std::set<unsigned>, 3> destinations;
    const unsigned depth = 2U + random() % 7U;
    const unsigned length = std::min(instruction_limit, 8U + unsigned(random() % 25U));
    unsigned admitted = 0, retired = 0, recovery_serial = 0;
    std::optional<StreamRecovery> recovery;
    auto expected_violations = environment.memory_violation_stats().count;
    auto &recovery_counts = coverage.stream_recovery;
    auto &inflight = coverage.stream_recovery_inflight;
    auto last_progress = environment.cycle();
    const auto deadline = environment.cycle() + std::uint64_t{length} * constrained_completion_timeout * 4U;
    const auto start_action = actions;
    ++coverage.stream[0];
    coverage.stream[10] += actions > coverage.concurrent_actions + coverage.stream[2];

    const auto pending = [&](const StreamWork &work) {
        if (work.kind == 0) return environment.scalar_load_pending(work.load);
        if (work.kind == 2) return environment.scalar_store_pending(work.store);
        if (work.kind == 4) return environment.prefetch_pending(work.prefetch);
        return std::any_of(work.vectors.begin(), work.vectors.end(),
            [&](const auto &uop) { return environment.vector_memory_pending(uop); });
    };
    const auto all_issued = [](const StreamWork &work) {
        if (work.kind == 2) return work.address_issued && work.data_issued;
        return std::find(work.issued.begin(), work.issued.end(), false) == work.issued.end();
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
        return live.empty() || environment.set_rob_head_state(
            live.front().rob(), live.front().flag(), live.front().is_store());
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
        for (const auto &uop : work.vectors) {
            bool replayed = false;
            if (!environment.service_vector_replay(uop, replayed, 256)) return false;
        }
        coverage.stream[5] += live.size() > 1;
        if (work.recovery_id) ++recovery_counts[work.kind == 2 ? 9U : 10U];
        if (recovery) {
            ++recovery_counts[12];
            inflight[10] += recovery->old_response_active();
        }
        if (work.kind == 2 && !environment.commit_store(work.store, constrained_completion_timeout)) return false;
        if (work.kind == 3) for (const auto &uop : work.vectors)
            if (!environment.commit_vector_store(uop, constrained_completion_timeout)) return false;
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
        live.pop_front();
        if (live.empty()) {
            if (!environment.set_rob_head_state(old_rob, old_flag, false)) return false;
        } else if (!install_head()) return false;
        last_progress = environment.cycle();
        return true;
    };

    while (retired < length) {
        phase = "mixed-stream:admitted=" + std::to_string(admitted) +
            ":retired=" + std::to_string(retired) + ":live=" + std::to_string(live.size());
        report_progress();
        if (environment.cycle() > deadline) { phase += ":epoch-deadline"; return false; }

        if (environment.memory_violation_stats().count != expected_violations) {
            phase += ":unexpected-redirect"; return false;
        }
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

        bool replayed = false;
        for (auto &work : live) {
            for (unsigned i = 0; i < work.vectors.size(); ++i) {
                if (!work.issued[i]) continue;
                bool serviced = false;
                if (!environment.service_vector_replay(work.vectors[i], serviced, 256)) return false;
                if (serviced) { ++coverage.stream[9]; replayed = true; break; }
            }
            if (replayed) break;
        }
        if (replayed) { last_progress = environment.cycle(); continue; }

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
        // 4 = vector uop, 5 = completed architectural head retirement.
        // Candidates are regenerated from current obligations.
        struct Event { unsigned kind, instruction, uop; };
        std::vector<Event> ready;
        const bool completed_head = !barrier_at_front() && !live.empty() && all_issued(live.front()) && !pending(live.front());
        if (completed_head) ready.push_back({5, 0, 0});
        if (admitted < length && live.size() < depth &&
            (!recovery || recovery->phase == StreamRecovery::Phase::admission) &&
            std::accumulate(weights.begin(), weights.end(), std::uint64_t{0}) != 0)
            ready.push_back({0, 0, 0});
        if (recovery && recovery->phase == StreamRecovery::Phase::release_old && environment.cycle() >= recovery->ready_cycle)
            ready.push_back({7, 0, 0});
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
            if (!conflict || barrier_at_front() || !live.front().is_store())
                ready.push_back({6, operand.store, 0});
        }
        for (unsigned i = 0; i < live.size(); ++i) {
            auto &work = live[i];
            const bool graph_role = recovery && work.recovery_id == recovery->id;
            const bool head_eligible = !work.requires_head || (i == 0 && !barrier_at_front());
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
                if (executing && !work.issued[0] && prefix_addresses_known && data_ready &&
                    episode.issue < episode.issue_order().size() &&
                    episode.issue_order()[episode.issue] == work.recovery_index && environment.cycle() >= episode.ready_cycle)
                    ready.push_back({1, i, 0});
            } else if ((work.kind == 0 || work.kind == 4) && !work.issued[0] && head_eligible &&
                (work.kind == 4 || older_addresses_known)) ready.push_back({1, i, 0});
            if (work.kind == 2 && graph_role) {
                const auto &episode = *recovery;
                const bool early = (episode.phase == StreamRecovery::Phase::admission ||
                    episode.phase == StreamRecovery::Phase::initial) && episode.plan.stores[work.recovery_index].data_before_load;
                const bool promote = episode.phase == StreamRecovery::Phase::refetch && work.address_issued;
                if (!work.data_issued && (early || promote)) ready.push_back({3, i, 0});
            } else if (work.kind == 2) {
                // Beat-crossing STA must not block older retirement.
                if (head_eligible && (!work.address_issued ||
                    (environment.scalar_store_address_pending(work.store) &&
                     environment.cycle() - work.last_address_issue >= 32))) ready.push_back({2, i, 0});
                if (!work.data_issued) ready.push_back({3, i, 0});
            }
            if (work.kind == 1 || work.kind == 3) {
                const bool ordered_load = work.kind == 1 && work.vectors.front().addressing ==
                    memblock::VectorAddressingMode::indexed_ordered;
                if (head_eligible && (!ordered_load || i == 0) && (work.kind == 3 || older_addresses_known)) {
                    for (unsigned u = 0; u < work.vectors.size(); ++u) {
                        if (work.issued[u]) continue;
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
        if (ready.empty()) {
            if (!environment.run_cycles(1)) return false;
            if (environment.cycle() - last_progress > 2U * constrained_completion_timeout) {
                phase += ":no-accepted-or-retired-progress";
                return false;
            }
            continue;
        }
        const auto event = ready[random() % ready.size()];
        if (recovery && recovery->old_response_active() && event.kind >= 1 && event.kind <= 4)
            ++inflight[9];
        if (completed_head && event.kind != 5 && event.kind != 7)
            ++coverage.stream_waiting_head[event.kind == 0 ? 0U : 1U];
        if (event.kind == 5) {
            if (!retire_head()) return false;
        } else if (event.kind == 7) {
            if (!environment.release_held_dcache_response_at(*recovery->cold_physical)) return false;
            recovery->phase = StreamRecovery::Phase::drain_old;
            recovery->old_quiet_since.reset();
        } else if (event.kind == 6) {
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
                work.last_address_issue = environment.cycle();
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
        } else if (event.kind == 0) {
            const bool had_accepted_pending = std::any_of(live.begin(), live.end(),
                [&](const auto &work) { return work.accepted() && pending(work); });
            coverage.stream[3] += had_accepted_pending;
            coverage.stream[4] += retired != 0 && !live.empty();
            StreamWork work;
            if (!recovery) {
                auto selection = random() % std::accumulate(weights.begin(), weights.end(), std::uint64_t{0});
                for (work.kind = 0; work.kind < 4 && selection >= weights[work.kind]; ++work.kind)
                    selection -= weights[work.kind];
            } else work.kind = recovery->stores.size() < recovery->plan.stores.size() ? 2U : 0U;
            auto line = recovery ? recovery->base : random() % 4U != 0 ? arena + (random() % 8U) * 64U :
                constrained_cacheable_address(64, translation, leaf, false);
            const unsigned slots = std::min<unsigned>(depth - live.size(), length - admitted);
            if (!recovery && work.kind == 2 && slots >= 2 && lq_free != 0 && sq_free != 0 &&
                weights[0] != 0 && random() % 1000U < constraints.stream_unresolved_per_mille)
                begin_recovery(line, slots, lq_free, sq_free);
            const bool graph_admission = recovery.has_value();
            if (graph_admission) {
                work.recovery_id = recovery->id;
                work.recovery_index = work.kind == 2 ? recovery->stores.size() : recovery->loads.size();
                line = recovery->base;
            } else recovery_counts[13] += std::any_of(live.begin(), live.end(),
                [](const auto &older) { return older.recovery_id != 0; });
            if (work.kind == 0) {
                const auto format = graph_admission ? ScalarLoadShape{recovery->group.loads[work.recovery_index].op,
                    recovery->group.loads[work.recovery_index].fp} : constraints.scalar_load_policy().choose(random);
                const bool cold_role = graph_admission && recovery->group.unfinished && work.recovery_index == *recovery->group.unfinished;
                work.load = make_load(cold_role ? *recovery->cold_address : graph_admission ? line + recovery->group.loads[work.recovery_index].offset :
                    constrained_window_address(line, 1U << (format.op & 3U)),
                    static_cast<memblock::LoadOp>(format.op), random() % memblock::kScalarLoadLanes, format.fp);
                work.load.pdest = allocate_destination(format.fp ? 1U : 0U);
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
                environment.expect_load_data(work.load, work.expected_scalar);
                if (!environment.enqueue_load(work.load)) return false;
            } else if (work.kind == 2) {
                if (graph_admission && !recovery->stores.empty()) ++rob_offset;
                const auto op = static_cast<memblock::StoreOp>(graph_admission ?
                    std::countr_zero(recovery->bytes[work.recovery_index].width) : random() % 4U);
                const unsigned width = 1U << static_cast<unsigned>(op);
                auto address = graph_admission ? line + recovery->bytes[work.recovery_index].offset :
                    constrained_window_address(line, width);
                if ((address & 0xfffU) + width > 0x1000U)
                    address = (address & ~std::uint64_t{0xfff}) | (0x1000U - width);
                work.store = make_store(address, graph_admission ? recovery->bytes[work.recovery_index].data : random(),
                    op, random() % memblock::kScalarStoreLanes);
                if (graph_admission) { recovery->stores.push_back(work.store); ++recovery_counts[3]; }
                if ((address & 15U) + width > 16U) {
                    work.store.expected_address_writeback_lane = 0;
                    work.requires_head = true;
                }
                oracle.store(work.store);
                environment.expect_store(work.store);
                if (!environment.enqueue_store(work.store)) return false;
            } else if (work.kind == 4) {
                work.prefetch = make_prefetch(line, static_cast<memblock::PrefetchOp>(8U + random() % 3U),
                    random() % memblock::kScalarLoadLanes);
                work.issued.assign(1, false);
                environment.expect_prefetch(work.prefetch);
                if (!environment.enqueue_prefetch(work.prefetch)) return false;
            } else {
                const bool store = work.kind == 3;
                work.shape = choose_vector_shape(store ? 1U : 0U, store ? sq_free : lq_free, false);
                const auto eew = work.shape.addressing >= 2 ? work.shape.vsew : work.shape.eew;
                work.vectors = make_random_vector_uops(store, constrained_window_address(line, 1U << eew),
                    work.shape, random() % 1000U < constraints.vector_corner_per_mille);
                work.issued.assign(work.vectors.size(), false);
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
                    if (!environment.enqueue_vector(uop)) return false;
                }
                coverage.stream_vector_uops[store ? 1U : 0U] += work.vectors.size();
            }
            const bool was_empty = live.empty();
            live.push_back(std::move(work)); ++admitted; ++coverage.stream[1];
            if (was_empty && !install_head()) return false;
            if (graph_admission && recovery->loads.size() == recovery->group.loads.size()) {
                recovery->obligation.emplace(recovery->loads, recovery->stores.size(), recovery->group.unfinished);
                recovery->phase = StreamRecovery::Phase::initial;
                recovery->ready_cycle = environment.cycle() + recovery->group.issue_delays.front();
            }
        } else {
            auto &work = live[event.instruction];
            bool initial_event = true;
            if (event.kind == 1) {
                if (work.kind == 0 ? !environment.issue_load(work.load, constrained_completion_timeout) :
                    !environment.issue_prefetch(work.prefetch, constrained_completion_timeout)) return false;
                work.issued[0] = true;
                initial_event = !work.refetch;
                coverage.stream_issue[work.kind == 0 ? 0U : 5U] += initial_event;
                if (recovery && work.recovery_id == recovery->id) {
                    ++recovery->issue;
                    if (recovery->issue < recovery->issue_order().size())
                        recovery->ready_cycle = environment.cycle() + recovery->issue_delays()[recovery->issue];
                }
            } else if (event.kind == 2) {
                const bool first = !work.address_issued;
                initial_event = first;
                coverage.stream_scalar_retries += !first;
                if (!environment.issue_store_address_until_tlb_hit(work.store, constrained_completion_timeout)) return false;
                work.address_issued = true; work.last_address_issue = environment.cycle();
                coverage.stream_issue[1] += first;
            } else if (event.kind == 3) {
                work.data_first = !work.address_issued;
                if (!environment.issue_store_data(work.store, constrained_completion_timeout)) return false;
                work.data_issued = true; ++coverage.stream_issue[2];
                if (recovery && work.recovery_id == recovery->id && recovery->phase == StreamRecovery::Phase::refetch)
                    recovery->promoted[work.recovery_index] = true;
            } else {
                const auto &uop = work.vectors[event.uop];
                if (!environment.issue_vector(uop, constrained_completion_timeout)) return false;
                if (event.uop && vector_load_depends_on_previous(work.vectors[event.uop-1], uop))
                    ++coverage.vector_operand_chains[2];
                work.issued[event.uop] = true;
                ++coverage.stream_issue[work.kind == 1 ? 3U : 4U];
            }
            coverage.stream[8] += initial_event;
        }
        coverage.stream[6] = std::max<std::uint64_t>(coverage.stream[6], live.size());
        const auto accepted_pending = std::count_if(live.begin(), live.end(),
            [&](const auto &work) { return work.accepted() && pending(work); });
        coverage.stream[7] = std::max<std::uint64_t>(coverage.stream[7], accepted_pending);
        last_progress = environment.cycle();
        if (!environment.run_cycles(random() % 4U)) return false;
    }
    phase = "mixed-stream-drain";
    if (recovery || environment.memory_violation_stats().count != expected_violations ||
        !environment.run_until_queues_retired(constrained_completion_timeout)) return false;
    coverage.stream[11] = actions;
    constraint_coverage.sample_dcache(requests_before, environment.tilelink_requests());
    constraint_coverage.sample_translation(translation, leaf, ptw_before,
        environment.ptw_requests(), actions - start_action, actions - start_action);
    return environment.run_cycles(8);
}
} // namespace memblock::scenarios::mixed
