// Synthetic timing and ports; transaction builders, vector producers, coverage,
// constraints, and both scheduling decision methods are production code.
#include "scenarios/mixed/state.hpp"
#include "scenarios/mixed/vector_indices.hpp"
#include "scenarios/mixed/vector_operands.hpp"
#include "scenarios/mixed/stream_oracle.hpp"
#include "scenarios/mixed/stream_recovery.hpp"
#include "scenarios/mixed/store_address_replay.hpp"
#include "scenarios/mixed/stream_cancellation.hpp"
#include <functional>
#include <map>
#include <tuple>
#include <variant>

using namespace memblock;
using namespace memblock::scenarios;
using namespace memblock::scenarios::mixed;

static void require(bool condition, const std::string &message) {
    if (!condition) throw std::runtime_error(message);
}

struct SyntheticEnvironment {
    struct Uop {
        bool issued = false, committed = false;
        mutable bool completed = false;
        std::uint64_t due = 0, sq_target = 0;
        unsigned destination = 0;
        VectorMemoryTransaction vector;
    };
    struct Instruction {
        unsigned kind = 0, id = 0, flows = 0, group = 0, epoch = 0;
        bool refetch_pending = false, initial_cold = false, cold_canceled = false;
        LoadTransaction load;
        StoreTransaction store;
        std::uint8_t rob = 0;
        bool flag = false, address = false, data = false, committed = false;
        bool last_uop_seen = false, needs_head = false, needs_pending_store = false, restart_blocked = false;
        std::optional<std::uint64_t> restart_boundary;
        bool needs_restart_flush() const { return !uops.empty() && uops.front().vector.vstart != 0; }
        unsigned address_attempts = 0, address_retries = 0;
        std::uint64_t address_due = 0, data_due = 0, sq_target = 0;
        std::vector<Uop> uops;
        bool is_store() const { return kind == 2 || kind == 3; }
    };
    struct LqEntry { unsigned id, epoch, flows; bool done = false; };
    mutable std::deque<LqEntry> lq_entries;
    mutable std::uint64_t canceled_lq = 0;
    bool recovery_enabled = false, cold_enabled = false, narrow_history = false, successor_history = false, at_anchor = false;
    std::optional<std::uint64_t> held_line, released_at, redirect_boundary;
    std::optional<LoadTransaction> cold_snapshot;
    std::set<std::uint64_t> requested_lines;
    mutable std::map<std::uint64_t, std::uint64_t> request_counts;
    mutable bool successor_recorded = false;
    struct Dependency { unsigned consumer, producer; bool data; };
    std::vector<Dependency> dependencies;
    unsigned dependency_operand_checks = 0, late_overlap_admissions = 0, rob_reuses = 0;
    std::set<unsigned> rob_history;
    void bind_dependency(unsigned consumer, const LoadTransaction &source, bool data, std::uint64_t value) {
        const auto &producer = find(source.rob, source.rob_flag);
        require(producer.kind == 0 && !producer.load.fp_wen && producer.id < consumer,
            "dependency source was not an older integer load");
        require(value == load_expectations.at(identity(source.rob, source.rob_flag)),
            "dependency value was not frozen from the older load expectation");
        dependencies.push_back({consumer, producer.id, data});
        trace.push_back("D" + std::to_string(consumer) + ":" + std::to_string(producer.id) + (data ? ":data" : ":address"));
    }
    void check_dependency(unsigned consumer, bool data) {
        for (const auto &edge : dependencies) if (edge.consumer == consumer && edge.data == data) {
            require(complete(instructions.at(edge.producer)), "consumer issued before independent producer completion");
            if (data) require(instructions.at(consumer).store.data ==
                load_expectations.at(identity(instructions.at(edge.producer).rob, instructions.at(edge.producer).flag)),
                "dependent store data disagrees with producer expectation");
            ++dependency_operand_checks;
        }
    }
    std::uint64_t check_dependent_address(std::uint64_t before, std::uint64_t after, std::uint64_t expected) {
        const auto edge = std::find_if(dependencies.begin(), dependencies.end(), [&](const auto &item) {
            return item.consumer == instructions.size() && !item.data;
        });
        require(edge != dependencies.end(), "dependent address lacked recorded producer");
        const auto &producer = instructions.at(edge->producer);
        const auto wanted_value = load_expectations.at(identity(producer.rob, producer.flag));
        require(expected == wanted_value, "dependent address value disagrees with bound producer expectation");
        const auto displacement = (wanted_value % 64U) * 64U;
        const auto wanted = (before & ~std::uint64_t{4095}) + ((before % 4096U + displacement) % 4096U);
        require(after == wanted, "dependent ALU address disagrees with input model");
        return after;
    }
    unsigned cold_cancellations = 0, cold_releases = 0, cold_refetches = 0;
    unsigned prefix_old_producers = 0, prefix_old_retirements = 0;
    std::uint64_t grant_idle_at = 0;
    bool old_response_active() const { return cold_snapshot && !cold_refetches_for_epoch; }
    bool cold_refetches_for_epoch = false;
    std::uint64_t dcache_requests_covering_since(std::uint64_t address, std::uint64_t) const {
        const auto line = address & ~std::uint64_t{63};
        // A completed successor may be visible only through counted accepted-A
        // history at the next scheduler poll, while current readiness is idle.
        if (successor_history && released_at && cold_snapshot && !successor_recorded &&
            tick >= grant_idle_at + 8U) {
            ++request_counts[cold_snapshot->address & ~std::uint64_t{63}];
            successor_recorded = true;
        }
        const auto found = request_counts.find(line);
        return unsigned(narrow_history && (address & 63U) == 1U) +
            (found == request_counts.end() ? 0U : found->second);
    }
    void hold_dcache_response_at(std::uint64_t address) {
        const auto line = address & ~std::uint64_t{63};
        require(!narrow_history, "cold hold ignored a narrow prior A");
        require(!held_line && !requested_lines.count(line), "cold hold lacked a fresh target line");
        held_line = line; released_at.reset(); cold_snapshot.reset(); cold_refetches_for_epoch = false;
        trace.push_back("H");
    }
    bool dcache_response_held_at(std::uint64_t address) const {
        return held_line == (address & ~std::uint64_t{63}) && requested_lines.count(*held_line);
    }
    bool release_held_dcache_response_at(std::uint64_t address) {
        require(dcache_response_held_at(address) && cold_snapshot &&
            tick >= *redirect_boundary + 8U, "cold response released before exact cancellation/filter recovery");
        require(!expected.count(key(*cold_snapshot, 0)), "old response released with uncanceled scalar obligation");
        held_line.reset(); released_at = tick; successor_recorded = false;
        grant_idle_at = tick + 11U + (cold_snapshot->pdest % 9U);
        ++cold_releases; trace.push_back("D"); return true;
    }
    bool dcache_line_grants_idle(std::uint64_t address) const {
        if (held_line == (address & ~std::uint64_t{63})) return false;
        return !released_at || tick >= grant_idle_at;
    }
    bool cancel_scalar_load_after_redirect(const LoadTransaction &t) {
        require(redirect_boundary == tick && cold_snapshot.has_value(), "missed exact scalar cancellation boundary");
        auto &work = find(t.rob, t.rob_flag);
        require(work.initial_cold && !work.cold_canceled && work.refetch_pending &&
            t.pdest == cold_snapshot->pdest && t.fp_wen == cold_snapshot->fp_wen &&
            t.lq == cold_snapshot->lq && t.lq_flag == cold_snapshot->lq_flag &&
            dcache_response_held_at(t.address) && canceled_lq,
            "wrong or duplicate pending cold cancellation");
        require(expected.erase(key(t, 0)) == 1 && enqueued.erase(key(t, 0)) == 1,
            "cold obligation disappeared before explicit cancellation");
        work.cold_canceled = true; ++cold_cancellations; trace.push_back("C"); return true;
    }
    unsigned next_rob = 157, group_serial = 0, active_group = 0, first_graph = 0;
    std::optional<unsigned> anchor_identity;
    unsigned refetch_issues = 0, invalidated_executions = 0, surviving_prefix_redirects = 0;
    unsigned checked_scalar_values = 0, expected_next_ftq = 0;
    std::map<unsigned, std::uint64_t> load_expectations;
    SparseMemory bytes;
    std::vector<Instruction> instructions;
    std::array<std::set<unsigned>, 3> destinations;
    std::set<std::tuple<unsigned, unsigned, unsigned>> expected, enqueued;
    std::vector<std::string> trace;
    std::vector<unsigned> producer_counts;
    unsigned *actions = nullptr;
    unsigned retired = 0, latency = 0, peak_live = 0, peak_accepted = 0;
    unsigned overlap_admissions = 0, continued_admissions = 0;
    unsigned loads_before_older_std = 0, head_checks = 0, dependency_checks = 0;
    unsigned issues = 0, commits = 0;
    bool delayed_store_feedback = false;
    mutable Environment::IqSlowFeedbackStats store_feedback;
    mutable std::vector<Environment::StoreSlowFeedbackSample> future_store_feedback;
    std::map<unsigned, std::uint64_t> address_retry_after;
    unsigned address_misses = 0;
    std::vector<std::array<std::uint64_t, 2>> accepted_operand_pairs;
    const Environment::IqSlowFeedbackStats &iq_slow_feedback_stats() const {
        for (auto it = future_store_feedback.begin(); it != future_store_feedback.end();) {
            if (it->cycle > tick) { ++it; continue; }
            store_feedback.sta_samples.push_back(*it);
            it = future_store_feedback.erase(it);
        }
        return store_feedback;
    }

    unsigned admissions_with_completed_head = 0, issues_with_completed_head = 0;
    std::optional<unsigned> installed_head;
    bool pending_store_level = false;
    std::uint64_t tick = 0, lq_used = 0, sq_used = 0;
    mutable std::uint64_t sq_done = 0;
    struct RetiredStore { std::uint64_t target, due; unsigned id; };
    mutable std::deque<RetiredStore> retired_stores;
    mutable Environment::StoreRetirementStats retirement_stats;
    unsigned retired_pending_admissions = 0, retired_pending_issues = 0, retired_pending_heads = 0;
    unsigned head_retirement_marker = 0;
    void refresh_sq() const {
        while (!retired_stores.empty() && retired_stores.front().due <= tick) {
            sq_done = retired_stores.front().target;
            retired_stores.pop_front();
            ++retirement_stats.drained;
        }
        retirement_stats.pending = retired_stores.size();
    }
    struct Offer {
        std::uint64_t tag, created, ready_at;
        OrdinaryIssueKind kind;
        unsigned lane;
        std::variant<LoadTransaction, PrefetchTransaction, StoreTransaction, VectorMemoryTransaction> payload;
    };
    bool session = false, processing_offers = false;
    std::vector<Offer> offers;
    std::vector<OrdinaryIssueEvent> transport_events;
    std::set<unsigned> selected_pairs;
    std::array<std::uint64_t, 10> observed_transport{};
    std::map<std::uint64_t, unsigned> observed_ports;
    std::optional<bool> retirement_had_stall;
    bool held_live() const {
        return std::any_of(offers.begin(), offers.end(), [&](const auto &offer) { return offer.created < tick; });
    }
    std::map<unsigned, std::array<std::optional<std::uint64_t>, 2>> pair_acceptance_edges;
    void select_pair(const StoreTransaction &store) { selected_pairs.insert(find(store.rob, store.rob_flag).id); }
    void note_selection(unsigned kind) {
        if (kind == 0 || kind == 5 || kind == 7 || kind == 8 || kind == 9) return;
        issues_with_completed_head += !at_anchor && retired < instructions.size() && complete(instructions[retired]);
        if (kind != 6) ++observed_transport[9];
    }
    bool begin_issue_session() { require(!session, "nested issue session"); session = true; return true; }
    bool end_issue_session() {
        require(session && offers.empty() && transport_events.empty(), "session ended with live offers/events");
        session = false; return true;
    }
    bool ok() const { return true; }
    bool issue_session_empty() const { return offers.empty(); }
    bool issue_session_available(OrdinaryIssueKind kind, unsigned lane) const {
        return session && std::none_of(offers.begin(), offers.end(), [&](const auto &offer) {
            return offer.kind == kind && offer.lane == lane;
        });
    }
    template<class Transaction>
    bool offer(std::uint64_t tag, OrdinaryIssueKind kind, unsigned lane, const Transaction &transaction) {
        require(issue_session_available(kind, lane), "offer overwrote an occupied producer lane");
        const auto &work = find(transaction.rob, transaction.rob_flag);
        // Independent bounded per-port backpressure; never consult ready list.
        const auto delay = 1U + (work.id * 7U + unsigned(kind) * (13U + (work.id % 3U) * 2U)) % 5U;
        offers.push_back({tag, tick, tick + delay, kind, lane, transaction});
        ++observed_transport[0];
        observed_transport[5] = std::max<std::uint64_t>(observed_transport[5], offers.size());
        return true;
    }
    bool offer_load(std::uint64_t tag, const LoadTransaction &t) { return offer(tag, OrdinaryIssueKind::load, t.lane, t); }
    bool offer_prefetch(std::uint64_t tag, const PrefetchTransaction &t) { return offer(tag, OrdinaryIssueKind::load, t.lane, t); }
    bool offer_store_address(std::uint64_t tag, const StoreTransaction &t) { return offer(tag, OrdinaryIssueKind::store_address, t.address_lane, t); }
    bool offer_store_data(std::uint64_t tag, const StoreTransaction &t) { return offer(tag, OrdinaryIssueKind::store_data, t.data_lane, t); }
    bool offer_vector(std::uint64_t tag, const VectorMemoryTransaction &t) { return offer(tag, OrdinaryIssueKind::vector, t.lane, t); }
    auto take_issue_events() { auto events = std::move(transport_events); transport_events.clear(); return events; }
    bool poll_vector_replay(const VectorMemoryTransaction &, VectorMemoryTransaction &, bool &found) { found = false; return true; }
    void process_offers() {
        if (!session || processing_offers) return;
        processing_offers = true;
        const bool saved = defer_producer_edge;
        defer_producer_edge = true;
        for (auto it = offers.begin(); it != offers.end();) {
            if (tick < it->ready_at) { ++it; continue; }
            const auto cursor = iq_slow_feedback_stats().sta_samples.size();
            const auto edge = tick;
            const bool pending = store_retirement_stats().pending != 0;
            std::visit([&](const auto &transaction) {
                using T = std::decay_t<decltype(transaction)>;
                if constexpr (std::is_same_v<T, LoadTransaction>) require(issue_load(transaction, 1), "load acceptance failed");
                else if constexpr (std::is_same_v<T, PrefetchTransaction>) require(issue_prefetch(transaction, 1), "prefetch acceptance failed");
                else if constexpr (std::is_same_v<T, VectorMemoryTransaction>) require(issue_vector(transaction, 1), "vector acceptance failed");
                else {
                    require(it->kind == OrdinaryIssueKind::store_address ? issue_store_address(transaction, 1) :
                        issue_store_data(transaction, 1), "store acceptance failed");
                    const auto id = find(transaction.rob, transaction.rob_flag).id;
                    if (selected_pairs.count(id)) {
                        auto &edges = pair_acceptance_edges[id];
                        edges[it->kind == OrdinaryIssueKind::store_address ? 0 : 1] = edge;
                        if (edges[0] && edges[1]) {
                            accepted_operand_pairs.push_back({*edges[0], *edges[1]}); selected_pairs.erase(id);
                        }
                    }
                }
            }, it->payload);
            require(tick == edge, "synthetic acceptance advanced a hidden edge");
            ++observed_transport[1];
            observed_ports[edge] |= 1U << unsigned(it->kind);
            transport_events.push_back({it->tag, edge, it->kind, it->lane,
                OrdinaryIssueOutcome::accepted, false, cursor, pending});
            it = offers.erase(it);
        }
        defer_producer_edge = saved;
        processing_offers = false;
    }
    bool defer_producer_edge = false, suppress_producer_note = false;
    void capture_store_edge() {
        if (!processing_offers) process_offers();
        refresh_sq();
        retirement_stats.pending_before_last_edge = retired_stores.size();
        if (!defer_producer_edge) ++tick;
        refresh_sq();
    }
    const Environment::StoreRetirementStats &store_retirement_stats() const {
        refresh_sq(); return retirement_stats;
    }
    void register_retired_store(std::uint64_t target, unsigned id, bool vector) {
        require(target > sq_done && target <= sq_used, "retired store lost its physical SQ target");
        ++(vector ? retirement_stats.vector_uops : retirement_stats.scalar);
        ++retirement_stats.pending_at_retirement;
        retired_stores.push_back({target, tick + 19U + 3U * (id % 7U), id});
        retirement_stats.pending = retired_stores.size();
        retirement_stats.max_pending = std::max(retirement_stats.max_pending, retirement_stats.pending);
    }
    std::uint64_t completion_edge(const Instruction &work) const {
        if (work.kind == 2) return std::max(work.address_due, work.data_due);
        std::uint64_t edge = 0;
        for (const auto &uop : work.uops) edge = std::max(edge, uop.due);
        return edge;
    }
    mutable std::uint64_t lq_done = 0;
    static constexpr unsigned initial_lq = 69, initial_sq = 53;

    explicit SyntheticEnvironment(unsigned timing) : latency(timing) {
        bytes.define_repeating_incrementing(0x80000000, 0x100000, 256, 0x39);
    }
    SparseMemory &memory() { return bytes; }
    std::uint64_t cycle() const { return tick; }
    std::uint64_t ptw_requests() const { return 0; }
    std::uint64_t tilelink_requests() const { return issues; }
    std::uint64_t lq_allocated() const { return lq_used; }
    std::uint64_t sq_allocated() const { return sq_used; }
    std::uint64_t lq_canceled() const { return canceled_lq; }
    std::uint64_t sq_canceled() const { return 0; }
    Environment::MemoryViolationStats violation;
    const Environment::MemoryViolationStats &memory_violation_stats() const { return violation; }
    std::uint64_t sq_redirect_canceled_observed() const { return 0; }
    std::uint64_t lq_redirect_canceled_observed() const { return canceled_lq; }
    std::uint64_t scalar_load_redirect_canceled() const { return cold_cancellations; }
    bool cancel_pending_for_redirect(std::uint8_t, bool, bool) { return true; }
    bool redirect_after(std::uint8_t rob, bool flag, bool itself) {
        if (!itself) {
            auto &head = find(rob, flag);
            check_installed_head();
            require(head.id == retired && head.id + 1U == instructions.size() &&
                head.needs_restart_flush() && complete(head) && !head.restart_boundary,
                "VSTART flush-after lacked the sole completed architectural head");
            require(!pending_store_level && tick >= completion_edge(head) + 7U,
                "VSTART flush-after violated producer timing or pendingst");
            tick += 3U;
            head.restart_boundary = tick;
            trace.push_back("VSF" + std::to_string(head.id));
            return true;
        }
        require(recovery_enabled && itself && violation.last.rob_value == rob && violation.last.rob_flag == flag,
            "returned wrong redirect victim");
        require(tick == violation.last_cycle + 2, "missed backend redirect deadline");
        check_installed_head();
        require(at_anchor || !instructions[retired].is_store(), "redirect held a store pendingst level");
        refresh_lq();
        const auto &victim = find(rob, flag);
        surviving_prefix_redirects += retired < first_graph;
        expected_next_ftq = victim.load.ftq_ptr + 1;
        unsigned offset = 0;
        for (unsigned index = victim.id; index < instructions.size(); ++index) {
            auto &work = instructions[index];
            require(work.kind == 0 && work.group == active_group && work.uops[0].issued,
                "redirect canceled an unrelated suffix");
            const bool cold = work.initial_cold && work.epoch == 0;
            if (cold) {
                require(!complete(work) && dcache_response_held_at(work.load.address) &&
                    expected.count(key(work.load, 0)), "cold initial expectation was not strict through redirect");
                cold_snapshot = work.load;
            } else {
                require(complete(work), "redirect canceled an unfinished warm suffix");
                expected.erase(key(work.load, 0)); enqueued.erase(key(work.load, 0));
                ++invalidated_executions;
            }
            work.refetch_pending = true;
            work.load.ftq_ptr = expected_next_ftq; work.load.ftq_offset = offset;
            offset += work.load.predecode_rvc ? 1 : 2;
        }
        for (auto it = lq_entries.begin(); it != lq_entries.end();) {
            if (it->id >= victim.id) { canceled_lq += it->flows; it = lq_entries.erase(it); }
            else ++it;
        }
        trace.push_back("X" + std::to_string(victim.id));
        tick += 3;
        redirect_boundary = tick;
        return true;
    }
    void refresh_lq() const {
        if (!recovery_enabled) return;
        for (auto &entry : lq_entries)
            if (entry.epoch == instructions[entry.id].epoch && complete(instructions[entry.id])) entry.done = true;
        while (!lq_entries.empty() && lq_entries.front().done) {
            lq_done += lq_entries.front().flows;
            lq_entries.pop_front();
        }
    }
    std::uint64_t lq_dequeued() const { refresh_lq(); return lq_done; }
    std::uint64_t sq_dequeued() const { refresh_sq(); return sq_done; }
    bool complete(const Instruction &work) const {
        if (work.initial_cold && work.epoch == 0) return false;
        if (work.kind == 2)
            return work.address && work.data && tick >= work.address_due && tick >= work.data_due;
        if (recovery_enabled && work.kind == 0 && !work.uops.empty() && !work.uops[0].completed) {
            const unsigned width = 1U << (unsigned(work.load.op) & 3U);
            for (unsigned index = 0; index < work.id; ++index) {
                const auto &older = instructions[index];
                if (older.kind != 2 || !older.address) continue;
                const unsigned store_width = 1U << unsigned(older.store.op);
                if (older.store.address < work.load.address + width && work.load.address < older.store.address + store_width &&
                    (!older.data || tick < older.data_due)) return false;
            }
        }
        if (work.needs_pending_store &&
            std::any_of(work.uops.begin(), work.uops.end(), [](const auto &uop) { return !uop.completed; }) &&
            (installed_head != work.id || !pending_store_level)) return false;
        return work.last_uop_seen && !work.uops.empty() &&
            std::all_of(work.uops.begin(), work.uops.end(), [&](const auto &uop) {
                if (uop.issued && tick >= uop.due) uop.completed = true;
                return uop.completed;
            });
    }
    bool accepted(const Instruction &work) const {
        return work.address_attempts || work.address || work.data || std::any_of(work.uops.begin(), work.uops.end(),
            [](const auto &uop) { return uop.issued; });
    }
    void synchronize_retirement() {
        require(actions != nullptr, "missing architectural action counter");
        require(*actions >= retired && *actions <= instructions.size(), "retirement outside admitted prefix");
        while (retired < *actions) {
            observed_transport[7] += retirement_had_stall.value_or(held_live());
            retirement_had_stall.reset();
            const auto &work = instructions.at(retired);
            require(complete(work), "retired incomplete architectural head");
            if (work.needs_restart_flush())
                require(work.restart_boundary && tick >= *work.restart_boundary + 6U,
                    "VSTART retirement lacked recovered whole-instruction flush-after");
            if (work.kind == 2) require(work.committed, "scalar store retired without commit");
            if (work.kind == 3) for (const auto &uop : work.uops)
                require(uop.committed, "vector store retired without all commits");
            if (work.kind == 0 || work.kind == 1) {
                const unsigned domain = work.kind == 1 ? 2 : scalar_domains.at(work.id);
                for (const auto &uop : work.uops)
                    require(destinations[domain].erase(uop.destination) == 1, "destination released twice");
            }
            const unsigned obligations = work.kind == 2 ? 1U : work.uops.size();
            for (unsigned uop = 0; uop < obligations; ++uop) {
                const auto terminal = std::make_tuple(identity(work.rob, work.flag), work.kind, uop);
                require(expected.erase(terminal) == 1 && enqueued.erase(terminal) == 1,
                    "terminal identity lacked exactly one expectation and dispatch");
            }
            if (!work.is_store() && !recovery_enabled) lq_done += work.flows;
            const auto age = work.kind == 1 || work.kind == 3 ? 4U : 3U;
            require(tick >= completion_edge(work) + age,
                "architectural retirement preceded backend writeback readiness");
            prefix_old_retirements += old_response_active() && work.group != active_group;
            trace.push_back("R" + std::to_string(work.id));
            ++retired;
        }
        refresh_lq();
        refresh_sq();
    }
    std::map<unsigned, unsigned> scalar_domains;
    Instruction &find(std::uint8_t rob, bool flag) {
        synchronize_retirement();
        for (unsigned index = retired; index < instructions.size(); ++index)
            if (instructions[index].rob == rob && instructions[index].flag == flag)
                return instructions[index];
        throw std::runtime_error("event did not match a live architectural identity");
    }
    const Instruction &find(std::uint8_t rob, bool flag) const {
        for (unsigned index = retired; index < instructions.size(); ++index)
            if (instructions[index].rob == rob && instructions[index].flag == flag)
                return instructions[index];
        throw std::runtime_error("pending query did not match a live identity");
    }
    static unsigned identity(std::uint8_t rob, bool flag) { return rob + (flag ? kRobEntries : 0U); }
    template<class T> std::tuple<unsigned, unsigned, unsigned> key(const T &t, unsigned kind, unsigned uop = 0) const {
        return std::make_tuple(identity(t.rob, t.rob_flag), kind, uop);
    }
    template<class T> void expect(const T &t, unsigned kind, unsigned uop = 0) {
        require(expected.insert(key(t, kind, uop)).second, "duplicate expectation");
    }
    void expect_load_data(const LoadTransaction &t, std::uint64_t value) {
        expect(t, 0); load_expectations[identity(t.rob, t.rob_flag)] = value;
    }
    void expect_store(const StoreTransaction &t) { expect(t, 2); }
    void expect_prefetch(const PrefetchTransaction &t) { expect(t, 4); }
    void expect_vector(const VectorMemoryTransaction &t) { expect(t, t.store ? 3 : 1, t.vuop_idx); }
    void expect_vector_data(const VectorMemoryTransaction &t, std::array<unsigned char, 16>) { expect_vector(t); }

    template<class T> Instruction &admit(const T &t, unsigned kind, unsigned uop = 0) {
        synchronize_retirement();
        require(expected.count(key(t, kind, uop)) != 0, "dispatch lacked prior expectation");
        require(enqueued.insert(key(t, kind, uop)).second, "duplicate dispatch");
        if (uop == 0) {
            observed_transport[6] += held_live();
            admissions_with_completed_head += !at_anchor && retired < instructions.size() && complete(instructions[retired]);
            for (unsigned index = retired; index < instructions.size(); ++index)
                require(!instructions[index].needs_restart_flush(), "younger admission escaped VSTART barrier");
            const unsigned next = instructions.size();
            // Builders start immediately before ROB/queue wrap boundaries.
            const auto received = identity(t.rob, t.rob_flag);
            const bool graph_load = [&] { if constexpr (requires { t.pc; }) return t.pc.has_value(); else return false; }();
            if (recovery_enabled && active_group && std::any_of(instructions.begin(), instructions.end(),
                [&](const auto &work) { return work.group == active_group && work.kind == 0; }) &&
                (!graph_load || received == (next_rob + 1U) % (2U * kRobEntries))) {
                for (const auto &work : instructions) if (work.group == active_group)
                    require(complete(work) && !work.refetch_pending, "younger admission escaped unresolved recovery barrier");
                active_group = 0; anchor_identity.reset(); at_anchor = false;
            }
            if (recovery_enabled && kind == 2 && received == (next_rob + 1U) % (2U * kRobEntries)) {
                if (!active_group) {
                    active_group = ++group_serial;
                    first_graph = next;
                    anchor_identity = next_rob % (2U * kRobEntries);
                }
                ++next_rob; // Independent nonmemory producer marker.
            }
            require(received == next_rob % (2U * kRobEntries),
                "admission reordered or duplicated the architectural ROB prefix");
            ++next_rob;
            bool had_pending = false;
            for (unsigned index = retired; index < instructions.size(); ++index)
                had_pending |= accepted(instructions[index]) && !complete(instructions[index]);
            overlap_admissions += had_pending;
            late_overlap_admissions += next >= 32U && had_pending;
            rob_reuses += !rob_history.insert(received).second;
            continued_admissions += retired != 0 && retired != instructions.size();
            instructions.push_back(Instruction{.kind = kind, .id = next, .group = active_group, .rob = t.rob, .flag = t.rob_flag});
            trace.push_back("A" + std::to_string(next) + ":" + std::to_string(kind));
            producer_counts.push_back(0);
        }
        auto &work = instructions.back();
        require(work.rob == t.rob && work.flag == t.rob_flag && work.kind == kind,
            "vector siblings escaped their architectural admission");
        const auto lq = initial_lq + lq_used - canceled_lq, sq = initial_sq + sq_used;
        require(t.lq == lq_pointer_value(lq) && t.lq_flag == lq_pointer_flag(lq) &&
            t.sq == sq_pointer_value(sq) && t.sq_flag == sq_pointer_flag(sq),
            "dispatch cursor snapshot disagreed with allocated prefix");
        peak_live = std::max(peak_live, unsigned(instructions.size() - retired));
        require(peak_live <= 8, "live-depth bound exceeded");
        return work;
    }
    void allocate(Instruction &work, unsigned count) {
        work.flows += count;
        (work.kind == 2 || work.kind == 3 ? sq_used : lq_used) += count;
        if (work.is_store()) {
            work.sq_target = sq_used;
            if (!work.uops.empty()) work.uops.back().sq_target = sq_used;
        }
        if (recovery_enabled && !work.is_store()) lq_entries.push_back({work.id, work.epoch, count});
        refresh_lq();
        refresh_sq();
        capture_store_edge();
        if (work.kind != 1 && work.kind != 3 || work.last_uop_seen)
            retired_pending_admissions += retirement_stats.pending_before_last_edge != 0;
        require(lq_used - canceled_lq - lq_done <= kVirtualLoadQueueEntries - kLqEnqueueHeadroom,
            "LQ admission exceeded reserved capacity");
        require(sq_used - sq_done <= kStoreQueueEntries - kSqEnqueueHeadroom,
            "SQ admission exceeded reserved capacity");
    }
    void destination(Instruction &work, unsigned domain, unsigned value) {
        const unsigned registers = domain == 0 ? kIntegerPhysicalRegisters :
            domain == 1 ? kFpPhysicalRegisters : kVectorPhysicalRegisters;
        require(value > 0 && value < registers, "destination outside register-file domain");
        require(destinations[domain].insert(value).second, "physical destination reused while live");
        work.uops.back().destination = value;
    }
    bool enqueue_load(const LoadTransaction &t) {
        if (recovery_enabled) {
            for (auto &work : instructions) if (work.rob == t.rob && work.flag == t.rob_flag && work.refetch_pending) {
                require(work.load.pdest == t.pdest && work.load.fp_wen == t.fp_wen && work.load.pc == t.pc &&
                    work.load.sq == t.sq && work.load.sq_flag == t.sq_flag && work.load.predecode_rvc == t.predecode_rvc &&
                    work.load.ftq_ptr == t.ftq_ptr && work.load.ftq_offset == t.ftq_offset,
                    "refetch changed architectural or frontend identity");
                require(t.lq == lq_pointer_value(initial_lq + lq_used - canceled_lq) &&
                    t.lq_flag == lq_pointer_flag(initial_lq + lq_used - canceled_lq), "refetch used a stale LQ cursor");
                require(expected.count(key(t, 0)) && enqueued.insert(key(t, 0)).second,
                    "refetch lacked one fresh expectation");
                if (work.initial_cold && work.epoch == 0) {
                    require(work.cold_canceled && released_at && tick >= grant_idle_at + (successor_history ? 8U : 0U) + 32U,
                        "cold refetch preceded old Grant completion and bounded quiet witness");
                    cold_refetches_for_epoch = true;
                    ++cold_refetches;
                }
                work.load = t; work.refetch_pending = false; ++work.epoch;
                work.uops[0].issued = work.uops[0].completed = false; work.uops[0].due = 0;
                const auto prior_retired_pending = retired_pending_admissions;
                allocate(work, 1); retired_pending_admissions = prior_retired_pending;
                trace.push_back("F" + std::to_string(work.id)); return true;
            }
        }
        auto &work = admit(t, 0); work.load = t; work.last_uop_seen = true; work.uops.emplace_back();
        work.initial_cold = work.group && held_line == (t.address & ~std::uint64_t{63});
        const unsigned width = 1U << (static_cast<unsigned>(t.op) & 3U);
        work.needs_head = (t.address & 15U) + width > 16;
        scalar_domains[work.id] = t.fp_wen ? 1 : 0;
        destination(work, t.fp_wen ? 1 : 0, t.pdest); allocate(work, 1); return true;
    }
    bool enqueue_store(const StoreTransaction &t) {
        auto &work = admit(t, 2); work.store = t;
        const unsigned width = 1U << static_cast<unsigned>(t.op);
        work.needs_head = (t.address & 15U) + width > 16U;
        allocate(work, 1); return true;
    }
    bool begin_store_release_epoch(unsigned) {
        require(retired == instructions.size(), "release epoch began with unfinished architectural work");
        return true;
    }
    bool end_store_release_epoch(unsigned) {
        synchronize_retirement();
        require(retired == instructions.size() && retired_stores.empty(),
            "release epoch ended with unfinished physical or architectural work");
        return true;
    }
    bool track_store_release_inputs(const StoreTransaction &t) {
        const auto &work = find(t.rob, t.rob_flag);
        require(work.kind == 2 && !work.address && !work.data && work.sq_target != 0,
            "release tracking was not installed after scalar dispatch and before acceptance");
        return true;
    }
    bool track_vector_release_inputs(const VectorMemoryTransaction &t) {
        const auto &work = find(t.rob, t.rob_flag);
        require(t.store && t.vuop_idx < work.uops.size() && !work.uops[t.vuop_idx].issued,
            "release tracking was not installed after vector dispatch and before acceptance");
        return true;
    }
    bool enqueue_prefetch(const PrefetchTransaction &t) {
        auto &work = admit(t, 4); work.last_uop_seen = true; work.uops.emplace_back(); allocate(work, 1); return true;
    }
    bool enqueue_vector(const VectorMemoryTransaction &t) {
        auto &work = admit(t, t.store ? 3 : 1, t.vuop_idx);
        require(t.vuop_idx == work.uops.size(), "non-prefix vector dispatch");
        work.uops.push_back(Uop{.vector = t}); work.last_uop_seen = t.last_uop;
        if (!t.store) destination(work, 2, t.pdest);
        const unsigned width = 1U << vector_data_eew(t);
        const auto active = active_vector_elements(t);
        for (unsigned element = 0; element < 16U / width; ++element) {
            if (((active >> element) & 1U) == 0) continue;
            const auto address = vector_element_virtual_address(t, element);
            work.needs_head |= t.store ? (address & (width - 1U)) != 0 : (address & 15U) + width > 16U;
            work.needs_pending_store |= t.store && (address & 4095U) + width > 4096U;
        }
        allocate(work, t.flow_num); return true;
    }
    void check_load_age(const Instruction &work) {
        for (unsigned index = retired; index < work.id; ++index) {
            const auto &older = instructions[index];
            if (older.kind == 2) {
                if (work.group && work.group == older.group && work.load.pc) continue;
                require(older.address && tick >= older.address_due,
                    "younger load issued before an older scalar-store address was known");
                loads_before_older_std += !older.data;
            }
            if (older.kind == 3) require(complete(older),
                "younger load issued before an older vector-store address set was complete");
        }
    }
    void check_installed_head() const {
        require(installed_head && *installed_head == retired, "accepted producer without current architectural ROB head");
        const auto &head = instructions.at(retired);
        const bool flush_blocked = head.restart_blocked &&
            (!head.restart_boundary || tick < *head.restart_boundary + 6U);
        require(pending_store_level == (!at_anchor && head.is_store() && !flush_blocked), "accepted producer with wrong pending-store level");
    }
    void check_head(const Instruction &work, bool ordered = false) {
        if (work.needs_head || ordered) {
            ++head_checks;
            require(work.id == retired, "head-constrained instruction issued behind an older instruction");
        }
    }
    void sample_accepted() {
        unsigned count = 0;
        for (unsigned index = retired; index < instructions.size(); ++index)
            count += accepted(instructions[index]) && !complete(instructions[index]);
        peak_accepted = std::max(peak_accepted, count);
    }
    std::uint64_t due(const Instruction &work, unsigned uop = 0) const {
        // Completion timing does not inspect the scheduler's candidate list.
        return tick + (cold_enabled && !work.group && work.kind == 1 ? 63U + (work.id * 11U + uop * 17U) % 81U :
            cold_enabled && !work.group && work.kind == 3 ? 143U : latency ? 37U + ((work.id * 7U + uop * 11U) % 31U) : 3U + (work.id + uop) % 5U);
    }
    void note_producer() {
        if (suppress_producer_note) return;
        prefix_old_producers += old_response_active();
    }
    void issue(Instruction &work, unsigned index, const char *tag) {
        check_installed_head();
        note_producer();
        check_dependency(work.id, false);
        auto &uop = work.uops.at(index);
        require(!uop.issued, "duplicate uop acceptance");
        uop.issued = true; uop.due = due(work, index); ++producer_counts[work.id]; ++issues;
        if (work.kind == 0 && work.epoch) ++refetch_issues;
        trace.push_back(std::string(tag) + std::to_string(work.id) + ":" + std::to_string(index));
        sample_accepted(); capture_store_edge();
        retired_pending_issues += retirement_stats.pending_before_last_edge != 0;
    }
    void check_scalar_value(const Instruction &load, const LoadTransaction &t) {
        const unsigned width = 1U << (unsigned(t.op) & 3U);
        std::uint64_t payload = 0;
        for (unsigned byte = 0; byte < width; ++byte) {
            const auto address = t.address + byte;
            unsigned char value = bytes.read_byte(address);
            for (unsigned index = 0; index < load.id; ++index) {
                const auto &older = instructions[index];
                if (older.kind == 2) {
                    if (load.group == older.group && load.load.pc && !older.address) continue;
                    const auto first = older.store.address;
                    if (address >= first && address - first < (1U << unsigned(older.store.op)))
                        value = older.store.data >> (8U * (address - first));
                } else if (older.kind == 3) {
                    for (const auto &uop : older.uops) {
                        const auto &v = uop.vector;
                        const unsigned element_width = 1U << vector_data_eew(v);
                        const auto active = active_vector_elements(v);
                        for (unsigned element = 0; element < 16U / element_width; ++element) {
                            if (!((active >> element) & 1U)) continue;
                            const auto first = vector_element_virtual_address(v, element);
                            if (address >= first && address - first < element_width)
                                value = v.data[element * element_width + address - first];
                        }
                    }
                }
            }
            payload |= std::uint64_t{value} << (8U * byte);
        }
        if (width < 8 && (t.fp_wen || (unsigned(t.op) < 4 && (payload >> (width * 8U - 1U)))))
            payload |= ~((std::uint64_t{1} << (width * 8U)) - 1U);
        require(load_expectations.at(identity(t.rob, t.rob_flag)) == payload,
            "stream scalar expected value disagreed with independent input byte history");
        ++checked_scalar_values;
    }
    bool issue_load(const LoadTransaction &t, unsigned) {
        auto &work = find(t.rob, t.rob_flag); check_load_age(work); check_head(work);
        if (recovery_enabled) check_scalar_value(work, t);
        requested_lines.insert(t.address & ~std::uint64_t{63});
        ++request_counts[t.address & ~std::uint64_t{63}];
        issue(work, 0, "L"); return true;
    }
    bool issue_prefetch(const PrefetchTransaction &t, unsigned) {
        auto &work = find(t.rob, t.rob_flag); issue(work, 0, "P"); return true;
    }
    bool issue_store_address(const StoreTransaction &t, unsigned timeout) {
        auto &work = find(t.rob, t.rob_flag);
        iq_slow_feedback_stats();
        require(!work.address, "STA retried after successful address acceptance");
        require(!address_retry_after.count(work.id) || tick >= address_retry_after.at(work.id),
            "STA retried before its own miss feedback/cooldown");
        const bool miss = delayed_store_feedback && work.address_attempts == 0;
        const auto accepted_at = tick;
        if (miss) {
            check_installed_head(); check_head(work); check_dependency(work.id, false); note_producer();
            ++issues; ++producer_counts[work.id]; ++address_misses;
            trace.push_back("SM" + std::to_string(work.id)); capture_store_edge();
            retired_pending_issues += retirement_stats.pending_before_last_edge != 0;
        } else if (!issue_store_address_until_tlb_hit(t, timeout)) return false;
        work.address_retries += work.address_attempts != 0;
        ++work.address_attempts;
        const auto feedback_at = delayed_store_feedback ? accepted_at + 19U + (work.id % 7U) : tick;
        future_store_feedback.push_back({.lane=t.address_lane, .hit=!miss,
            .sq_flag=t.sq_flag, .sq_value=t.sq, .cycle=feedback_at});
        address_retry_after[work.id] = miss ? feedback_at + 8U : std::numeric_limits<std::uint64_t>::max();
        return true;
    }
    bool issue_store_address_until_tlb_hit(const StoreTransaction &t, unsigned) {
        observed_transport[8] += !session;
        auto &work = find(t.rob, t.rob_flag);
        check_installed_head(); check_head(work); note_producer();
        if (recovery_enabled && work.group == active_group && active_group) {
            for (unsigned index = work.id + 1; index < instructions.size(); ++index) {
                const auto &load = instructions[index];
                if (load.group != active_group || load.kind != 0) continue;
                require(complete(load) || (load.initial_cold && load.epoch == 0 &&
                    load.uops[0].issued && dcache_response_held_at(load.load.address)),
                    "graph STA preceded a strict initial/refetch completion");
            }
        }
        if (!work.address) check_dependency(work.id, false);
        if (!work.address) { work.address = true; work.address_due = latency ? due(work) : tick + 1U; ++producer_counts[work.id]; ++issues; }
        else require(tick < work.address_due, "reissued an already completed store address");
        trace.push_back("SA" + std::to_string(work.id)); sample_accepted(); capture_store_edge();
        retired_pending_issues += !work.group && retirement_stats.pending_before_last_edge != 0;
        if (recovery_enabled && work.group == active_group && active_group) {
            for (unsigned index = work.id + 1; index < instructions.size(); ++index) {
                const auto &load = instructions[index];
                if (load.group != active_group || load.kind != 0) continue;
                const auto a = t.address, b = load.load.address;
                if (a < b + (1U << (unsigned(load.load.op) & 3U)) && b < a + (1U << unsigned(t.op))) {
                    ++violation.count; violation.last_cycle = tick;
                    violation.last = {.valid = true, .is_rvc = load.load.predecode_rvc,
                        .rob_flag = load.flag, .rob_value = load.rob,
                        .ftq_value = static_cast<std::uint8_t>(load.load.ftq_ptr),
                        .ftq_offset = load.load.ftq_offset, .level = true};
                    break;
                }
            }
        }
        return true;
    }
    bool issue_store_data(const StoreTransaction &t, unsigned) {
        observed_transport[8] += !session;
        auto &work = find(t.rob, t.rob_flag); check_installed_head(); require(!work.data, "duplicate scalar store data");
        note_producer();
        check_dependency(work.id, true);
        work.data = true; work.data_due = due(work, 1); ++producer_counts[work.id]; ++issues;
        trace.push_back("SD" + std::to_string(work.id)); sample_accepted(); capture_store_edge();
        retired_pending_issues += !work.group && retirement_stats.pending_before_last_edge != 0; return true;
    }
    bool issue_store_operands(const StoreTransaction &t, Environment::StoreOperandAcceptances &acceptances, unsigned timeout) {
        // Independent test shell varies accepted edge order using instruction
        // identity; production retained-pin timing has separate host contracts.
        acceptances = {};
        const auto mode = find(t.rob, t.rob_flag).id % 3U;
        note_producer();
        suppress_producer_note = true;
        defer_producer_edge = mode == 0U;
        if (mode == 2U) {
            acceptances.edges[1] = tick;
            const auto before = retired_pending_issues;
            if (!issue_store_data(t, timeout)) return false;
            acceptances.committed_pending += retired_pending_issues - before;
        }
        acceptances.edges[0] = tick;
        auto before = retired_pending_issues;
        if (!issue_store_address(t, timeout)) return false;
        acceptances.committed_pending += retired_pending_issues - before;
        if (mode != 2U) {
            acceptances.edges[1] = tick;
            before = retired_pending_issues;
            if (!issue_store_data(t, timeout)) return false;
            acceptances.committed_pending += retired_pending_issues - before;
        }
        defer_producer_edge = false;
        suppress_producer_note = false;
        if (mode == 0U) capture_store_edge();
        accepted_operand_pairs.push_back(acceptances.edges);
        return true;
    }
    bool issue_vector(const VectorMemoryTransaction &t, unsigned) {
        auto &work = find(t.rob, t.rob_flag);
        const unsigned index = t.vuop_idx;
        if (!t.store) check_load_age(work);
        check_head(work, !t.store && t.addressing == VectorAddressingMode::indexed_ordered);
        if (t.addressing != VectorAddressingMode::indexed_unordered)
            for (unsigned prior = 0; prior < index; ++prior)
                require(work.uops[prior].issued, "ordered vector producer issued out of order");
        if (!t.store && index && vector_is_special_indexed(t) &&
            vector_indexed_vd_index(work.uops[index - 1].vector) == vector_indexed_vd_index(t)) {
            ++dependency_checks;
            const auto &prior = work.uops[index - 1];
            require(prior.issued && tick >= prior.due, "repeated-Vd consumer preceded producer completion");
        }
        issue(work, index, t.store ? "VS" : "VL"); return true;
    }
    bool scalar_load_pending(const LoadTransaction &t) const {
        const auto &work = find(t.rob, t.rob_flag);
        return work.initial_cold && work.cold_canceled && work.epoch == 0 ? false : !complete(work);
    }
    bool scalar_store_pending(const StoreTransaction &t) const { return !complete(find(t.rob, t.rob_flag)); }
    bool prefetch_pending(const PrefetchTransaction &t) const { return !complete(find(t.rob, t.rob_flag)); }
    bool scalar_store_address_pending(const StoreTransaction &t) const {
        const auto &work = find(t.rob, t.rob_flag); return !work.address || tick < work.address_due;
    }
    bool vector_memory_pending(const VectorMemoryTransaction &t) const {
        const auto &uop = find(t.rob, t.rob_flag).uops.at(t.vuop_idx); return !uop.issued || tick < uop.due;
    }
    bool service_vector_replay(const VectorMemoryTransaction &, bool &serviced, unsigned) { serviced = false; return true; }
    bool retire_store(const StoreTransaction &t) {
        auto &work = find(t.rob, t.rob_flag);
        require(work.id == retired && complete(work), "scalar retirement outside completed head");
        require(tick >= completion_edge(work) + 2U, "scalar scommit preceded backend readiness");
        require(!work.committed, "duplicate scalar retirement");
        retirement_had_stall = held_live();
        capture_store_edge();
        work.committed = true; ++commits;
        register_retired_store(work.sq_target, work.id, false);
        return true;
    }
    bool retire_vector_stores(const std::vector<VectorMemoryTransaction> &transactions) {
        require(!transactions.empty(), "empty vector retirement");
        auto &work = find(transactions.front().rob, transactions.front().rob_flag);
        require(work.id == retired && complete(work) && transactions.size() == work.uops.size(),
            "vector retirement outside complete instruction");
        require(tick >= completion_edge(work) + 3U, "vector retirement preceded backend readiness");
        retirement_had_stall = held_live();
        capture_store_edge();
        for (const auto &t : transactions) {
            auto &uop = work.uops.at(t.vuop_idx);
            require(!uop.committed, "duplicate vector retirement");
            uop.committed = true; ++commits;
            register_retired_store(uop.sq_target, work.id, true);
        }
        return true;
    }
    bool set_rob_head_state(std::uint8_t rob, bool flag, bool pending_store) {
        synchronize_retirement();
        const bool advanced = retired != head_retirement_marker;
        if (advanced) {
            const auto &old = instructions.at(retired - 1U);
            const unsigned delay = old.kind == 1 || old.kind == 3 ? 5U : 4U;
            require(tick + 1U >= completion_edge(old) + delay,
                "public ROB head advanced before backend writeback pipeline");
        }
        head_retirement_marker = retired;
        if (retired == instructions.size()) {
            require(!pending_store, "pending-store level escaped empty stream");
            installed_head.reset(); pending_store_level = false;
            capture_store_edge(); retired_pending_heads += advanced && retirement_stats.pending_before_last_edge != 0;
            return true;
        }
        auto &head = instructions.at(retired);
        if (recovery_enabled && active_group && retired == first_graph && anchor_identity == identity(rob, flag)) {
            require(!pending_store, "nonmemory recovery anchor asserted pendingst");
            at_anchor = true;
        } else {
            if (head.is_store() && head.needs_restart_flush() && !pending_store && !head.restart_boundary) {
                require(complete(head) && tick + 1U >= completion_edge(head) + 3U,
                    "VSTART low pendingst preceded completed writeback and ROB flush blocking");
            }
            if (head.needs_restart_flush() && !pending_store) head.restart_blocked = true;
            const bool flush_blocked = head.restart_blocked &&
                (!head.restart_boundary || tick < *head.restart_boundary + 6U);
            require(head.rob == rob && head.flag == flag && pending_store == (head.is_store() && !flush_blocked),
                "installed ROB head or pending-store level disagreed with oldest live instruction");
            if (recovery_enabled && active_group && retired == first_graph) {
                for (const auto &work : instructions) if (work.group == active_group)
                    require(complete(work) && !work.refetch_pending, "released recovery anchor before all obligations completed");
                active_group = 0; anchor_identity.reset();
            }
            at_anchor = false;
        }
        installed_head = retired; pending_store_level = pending_store;
        capture_store_edge(); retired_pending_heads += advanced && retirement_stats.pending_before_last_edge != 0;
        return true;
    }
    bool run_cycles(unsigned count) { synchronize_retirement(); for (unsigned i=0; i<count; ++i) capture_store_edge(); return true; }
    bool run_until_all_complete(unsigned) {
        synchronize_retirement();
        return std::all_of(instructions.begin() + retired, instructions.end(), [&](const auto &work) { return complete(work); });
    }
    bool run_until_queues_retired(unsigned) {
        synchronize_retirement();
        for (const auto &store : retired_stores) tick = std::max(tick, store.due) + 1U;
        refresh_sq();
        return lq_used == lq_done + canceled_lq && sq_used == sq_done && retired_stores.empty();
    }
    void finish(unsigned count, const MixedCoverage &coverage, const ConstraintCoverage &constraints) {
        synchronize_retirement();
        require(retired == count && instructions.size() == count, "lost or excess architectural work");
        require(expected.empty() && enqueued.empty(), "expectation/dispatch conservation failed");
        require(lq_used == lq_done + canceled_lq && sq_used == sq_done, "queue conservation failed");
        for (const auto &domain : destinations) require(domain.empty(), "physical destination leaked");
        require(coverage.stream[1] == count && coverage.stream[2] == count && constraints.actions == count,
            "production action/admission/retirement accounting disagreed");
        require(coverage.stream[3] == overlap_admissions && coverage.stream[4] == continued_admissions,
            "production overlap coverage disagreed with independent admission observations");
        require(coverage.stream_waiting_head == std::array<std::uint64_t, 2>{
            admissions_with_completed_head, issues_with_completed_head},
            "production completed-head choices disagreed with independent observations");
        require(coverage.stream[8] + refetch_issues + coverage.stream_scalar_retries == issues, "production issue count disagreed with accepted producers");
        std::array<std::uint64_t, 5> expected_pairs{};
        for (const auto &edges : accepted_operand_pairs) {
            if (edges[0] == edges[1]) ++expected_pairs[0];
            else if (edges[0] < edges[1]) ++expected_pairs[1];
            else ++expected_pairs[2];
            const auto distance = edges[0] > edges[1] ? edges[0] - edges[1] : edges[1] - edges[0];
            expected_pairs[3] += distance;
            if (expected_pairs[4] < distance) expected_pairs[4] = distance;
        }
        require(coverage.stream_store_pair == expected_pairs,
            "production operand-pair direction/skew disagreed with independent accepted edges");
        require(coverage.stream_transport == observed_transport,
            "production transport coverage disagreed with independent offered/accepted boundaries");
        std::array<std::uint64_t, 16> port_sets{};
        for (const auto &[edge, mask] : observed_ports) ++port_sets[mask];
        require(coverage.stream_issue_ports == port_sets,
            "production accepted port sets disagreed with actual handshake edges");
        require(coverage.stream_recovery_inflight[2] == cold_cancellations && cold_cancellations == cold_releases &&
            cold_releases == cold_refetches, "unfinished stream lifecycle accounting disagreed");
        require(coverage.stream_recovery_inflight[9] == prefix_old_producers &&
            coverage.stream_recovery_inflight[10] == prefix_old_retirements,
            "old-response prefix scheduling accounting disagreed");
        std::array<std::uint64_t, 5> operations{};
        for (const auto &work : instructions) {
            ++operations[work.kind];
            const auto count = work.kind == 2 ? 2U + work.address_retries : work.kind == 0 ? 1U + work.epoch : work.uops.size();
            require(producer_counts[work.id] == count, "producer conservation failed");
        }
        require(coverage.stream_ops == operations, "retired operation-kind accounting disagreed");
        std::array<std::uint64_t, 2> restart_counts{};
        for (const auto &work : instructions) if (work.needs_restart_flush()) {
            ++restart_counts[work.is_store() ? 1U : 0U];
            require(work.restart_boundary.has_value(), "VSTART instruction missed flush-after");
        }
        require(coverage.stream_vstart_flush[0] == restart_counts[0] &&
            coverage.stream_vstart_flush[1] == restart_counts[1] &&
            coverage.stream_vstart_flush[2] == restart_counts[0] + restart_counts[1],
            "VSTART flush accounting disagreed with independent instruction history");
        const std::array<std::uint64_t, 10> expected_retirement{
            operations[2], operations[3], retirement_stats.vector_uops,
            retirement_stats.pending_at_retirement, retirement_stats.already_dequeued,
            retirement_stats.max_pending, retired_pending_admissions, retired_pending_issues,
            retired_pending_heads, retirement_stats.drained};
        for (unsigned i = 0; i < expected_retirement.size(); ++i)
            require(coverage.stream_store_retirement[i] == expected_retirement[i],
                "store retirement/drain overlap bin " + std::to_string(i) + " actual=" +
                std::to_string(coverage.stream_store_retirement[i]) + " expected=" +
                std::to_string(expected_retirement[i]));
        require(coverage.stream_history == std::array<std::uint64_t, 4>{
            unsigned(count > 32U), count, late_overlap_admissions, rob_reuses},
            "sustained history observations disagree with independent admissions");
        const std::array<unsigned, 5> mapping{{RandomConstraints::scalar_load, RandomConstraints::vector_load,
            RandomConstraints::scalar_store, RandomConstraints::vector_store, RandomConstraints::prefetch}};
        for (unsigned kind = 0; kind < mapping.size(); ++kind)
            require(constraints.operations[mapping[kind]] == operations[kind], "constraint operation count disagreed");
    }
};

namespace memblock::scenarios::mixed {
class MixedScenario : public IdentityState, public ProgressState, public VectorShapeState {
public:
    RandomConstraints constraints;
    SyntheticEnvironment environment;
    std::mt19937_64 random;
    MixedCoverage coverage;
    ConstraintCoverage constraint_coverage;
    static constexpr unsigned constrained_completion_timeout = 512;
    explicit MixedScenario(std::uint64_t seed, unsigned timing, unsigned family, bool validated_profile = false)
        : constraints(RandomConstraints::preset(validated_profile ? "cmo-stress" : "coverage")), environment(timing), random(seed) {
        rob_offset = 157; lq_offset = SyntheticEnvironment::initial_lq; sq_offset = SyntheticEnvironment::initial_sq;
        environment.actions = &actions;
        constraints.operation_weights.fill(0);
        const std::array<unsigned, 5> operations{{RandomConstraints::scalar_load, RandomConstraints::vector_load,
            RandomConstraints::scalar_store, RandomConstraints::vector_store, RandomConstraints::prefetch}};
        for (unsigned index = 0; index < operations.size(); ++index)
            constraints.operation_weights[operations[index]] = validated_profile ? ((family >> index) & 1U) :
                family == 0 ? (index == 0 ? 7 : 3) : (family == index + 1 ? 1 : 0);
        constraints.stream_max_instructions = 32;
        constraints.stream_unresolved_per_mille = 0;
        constraints.stream_unresolved_inflight_per_mille = 0;
        constraints.misaligned_per_mille = 450;
        constraints.scalar_fp_per_mille = 350;
        if (validated_profile) {
            constraints.concurrent_actions_per_mille = constraints.concurrent_stream_per_mille = 1000;
            constraints.validate();
        }
        // Decoder-legal representatives include ordinary, unordered, ordered,
        // multi-uop and repeated-Vd shapes. Shape generation is production code.
        vector_shapes = {{0, 0, 0, 0, 0, 1}, {0, 2, 2, 1, 1, 1},
            {1, 1, 1, 0, 0, 1}, {2, 2, 0, 0, 2, 1}, {2, 1, 1, 1, 1, 1}, {3, 2, 2, 1, 1, 1}};
    }
    bool run_concurrent_stream(unsigned);
    void report_progress(bool = false) { environment.synchronize_retirement(); }
    TranslationContext choose_translation_context() { return {}; }
    TranslationLeafTopology choose_translation_leaf(const TranslationContext &) { return {}; }
    bool enter_translation_context(const TranslationContext &) { return true; }
    bool issue_constrained_fence(unsigned) { return true; }
    std::uint64_t constrained_cacheable_address(unsigned, const TranslationContext &, const TranslationLeafTopology &,
        std::optional<bool> = {}, bool = true) { return 0x80010000 + (random() % 128U) * 64U; }
    unsigned long constrained_window_address(std::uint64_t, unsigned);
    std::shared_ptr<const ReferencePageMap> reference_pages_for(std::uint64_t) { return {}; }
    bool validate_vector_translation(const VectorMemoryTransaction &) { return true; }
    bool choose_binary_class(unsigned probability, const std::array<std::uint64_t, 2> &) { return random() % 1000U < probability; }
    LoadTransaction make_load(std::uint64_t, LoadOp, unsigned, bool = false);
    StoreTransaction make_store(std::uint64_t, std::uint64_t, StoreOp, unsigned);
    PrefetchTransaction make_prefetch(std::uint64_t, PrefetchOp, unsigned);
    unsigned vector_shape_uop_count(const RandomVectorShape &);
    unsigned vector_shape_flow_num(const RandomVectorShape &);
    unsigned vector_shape_total_flows(const RandomVectorShape &);
    unsigned vector_shape_deficit(const RandomVectorShape &, unsigned);
    const RandomVectorShape &choose_vector_shape(unsigned, std::optional<unsigned>, bool);
    std::vector<VectorMemoryTransaction> make_random_vector_uops(bool, std::uint64_t, const RandomVectorShape &, bool);
    void sample_constrained_vector_instruction(bool, const RandomVectorShape &, const std::vector<VectorMemoryTransaction> &);
};
// PRODUCTION_BUILDERS_AND_VECTOR_PRODUCERS
} // namespace memblock::scenarios::mixed

// PRODUCTION_STREAM

namespace memblock::scenarios::mixed {
class BudgetFixture {
public:
    RandomConstraints constraints;
    std::mt19937_64 random;
    MixedCoverage coverage;
    std::string phase;
    unsigned actions = 0, target_before_redirect = 0, concurrent_action_limit = 0, budget = 0;
    std::vector<std::string> trace;
    BudgetFixture(std::uint64_t seed, unsigned total, unsigned concurrent, unsigned rate)
        : constraints(RandomConstraints::preset("coverage")), random(seed), target_before_redirect(total),
          concurrent_action_limit(concurrent), budget(concurrent) { constraints.concurrent_stream_per_mille = rate; }
    bool initialize_run() { return true; }
    bool prepare_random_translation_contexts() { return true; }
    bool prepare_tail() { return true; }
    bool run_serial_action() { trace.push_back("S"); ++actions; return true; }
    bool run_concurrent_windows() {
        // Model the legacy helper's public granularity, including its optional
        // special load and unused residual; do not consume a whole allowance.
        while (actions + 7 <= concurrent_action_limit) {
            const unsigned count = actions + 8 <= concurrent_action_limit && random() % 2U ? 8U : 7U;
            actions += count; coverage.concurrent_actions += count;
            trace.push_back("W" + std::to_string(count));
            if (constraints.concurrent_stream_per_mille != 0) return true;
        }
        return true;
    }
    bool run_concurrent_stream(unsigned maximum) {
        require(maximum > 0, "empty stream budget");
        const unsigned count = std::min(maximum, 3U + unsigned(random() % 13U));
        actions += count; coverage.stream[2] += count; trace.push_back("C" + std::to_string(count)); return true;
    }
    bool finish() {
        require(actions == target_before_redirect, "execute did not conserve total action budget");
        const auto used = coverage.stream[2] + coverage.concurrent_actions;
        require(used <= budget, "execute exceeded concurrent budget");
        require(constraints.concurrent_stream_per_mille == 0 ? budget - used < 7U : used == budget,
            "execute lost usable concurrent budget");
        return true;
    }
    bool execute();
};
// PRODUCTION_EXECUTE
} // namespace memblock::scenarios::mixed

static std::string join(const std::vector<std::string> &values) {
    std::string text;
    for (const auto &value : values) { if (!text.empty()) text += ','; text += value; }
    return text;
}
int main(int argc, char **argv) {
    try {
        require(argc >= 2, "fixture mode required");
        const std::string mode = argv[1];
        if (mode == "constraints") {
            unsigned presets = 0, boundaries = 0, rejected = 0;
            require(RandomConstraints{}.concurrent_stream_per_mille == 750, "default stream rate changed");
            for (const auto *name : {"coverage", "spec", "corner", "cmo-stress"}) {
                auto policy = RandomConstraints::preset(name);
                policy.validate();
                require(policy.concurrent_stream_per_mille == 750, "preset lost default stream rate");
                require(policy.stream_unresolved_inflight_per_mille == 250, "preset lost cold stream rate");
                ++presets;
                for (const auto *field : {"concurrent-stream", "stream-unresolved-inflight"}) {
                    for (unsigned value : {0U, 1000U}) {
                        policy.apply(std::string(field) + "=" + std::to_string(value));
                        policy.validate();
                        const auto actual = std::string(field) == "concurrent-stream" ?
                            policy.concurrent_stream_per_mille : policy.stream_unresolved_inflight_per_mille;
                        require(actual == value, "stream-rate override was not applied");
                        const auto target = std::string(field) == "concurrent-stream" ?
                            "target_concurrent_stream=" : "target_stream_unresolved_inflight=";
                        require(policy.summary().find(target + std::to_string(value) + " ") != std::string::npos,
                            "stream-rate override absent from serialized policy");
                        ++boundaries;
                    }
                    policy.apply(std::string(field) + "=1001");
                    try { policy.validate(); }
                    catch (const std::invalid_argument &) { ++rejected; }
                    policy.apply(std::string(field) + "=0");
                }
            }
            require(rejected == 2 * presets, "stream rate above 1000 was accepted");
            std::cout << "{\"presets\":" << presets << ",\"boundary_overrides\":" << boundaries
                << ",\"out_of_range_rejections\":" << rejected << "}\n";
        } else if (mode == "oracle-rollback") {
            SparseMemory base;
            base.define_repeating_incrementing(0x80010000, 0x1000, 256, 0x31);
            StreamByteOracle oracle(base);
            LoadTransaction load;
            load.address = 0x80010020;
            load.op = LoadOp::ld;
            const auto before = oracle.scalar_load(load);
            const auto checkpoint = oracle.checkpoint();
            StoreTransaction store;
            store.address = 0x80010020;
            store.data = 0x8877665544332211ULL;
            store.op = StoreOp::sd;
            oracle.store(store);
            const auto overlaid = oracle.scalar_load(load);
            require(overlaid != before, "store overlay did not change independent oracle bytes");
            oracle.restore(checkpoint);
            require(oracle.scalar_load(load) == before,
                "oracle checkpoint failed to remove canceled-store bytes");
            std::cout << "{\"before\":" << before << ",\"overlaid\":" << overlaid
                << ",\"restored\":" << oracle.scalar_load(load) << "}\n";
        } else if (mode == "stream" || mode == "pair-rate" || mode == "recovery" || mode == "profile" || mode == "dependency" || mode == "independent" || mode == "sustained" || mode == "feedback") {
            require(argc == 6 || (mode == "pair-rate" && argc == 7) || (mode == "recovery" && (argc == 7 || argc == 8 || argc == 9)), "stream seed/timing/family/limit required");
            MixedScenario fixture(std::stoull(argv[2]), std::stoul(argv[3]), std::stoul(argv[4]), mode == "profile");
            const unsigned limit = std::stoul(argv[5]);
            if (mode == "pair-rate") fixture.constraints.apply(std::string("stream-store-pair=") + argv[6]);
            if (mode == "sustained") {
                fixture.constraints.stream_min_instructions = fixture.constraints.stream_max_instructions = std::max(1U, limit);
                fixture.constraints.stream_dependency_per_mille = 1000;
                fixture.constraints.scalar_fp_per_mille = 0;
            }
            if (mode == "dependency") {
                fixture.constraints.stream_dependency_per_mille = 1000;
                if (std::stoul(argv[4]) == 1) fixture.constraints.scalar_fp_per_mille = 0;
            }
            if (mode == "feedback") fixture.environment.delayed_store_feedback = true;
            if (mode == "independent") fixture.constraints.stream_dependency_per_mille = 0;
            if (mode == "recovery") {
                fixture.environment.recovery_enabled = true;
                fixture.constraints.stream_unresolved_per_mille = 1000;
                fixture.constraints.scalar_unresolved_conflict_per_mille = argc >= 7 ? std::stoul(argv[6]) : 500;
                fixture.constraints.stream_unresolved_inflight_per_mille = argc >= 8 ? std::stoul(argv[7]) : 0;
                fixture.environment.cold_enabled = fixture.constraints.stream_unresolved_inflight_per_mille != 0;
                if (argc == 9) {
                    if (std::string(argv[8]) == "unavailable") {
                        for (std::uint64_t address = 0x80010000; address < 0x80012000; ++address)
                            fixture.environment.bytes.write_reference_byte(address, (address * 17U) >> 3U);
                    } else {
                        const std::string history = argv[8];
                        require(history == "narrow-history" || history == "successor-history", "unknown freshness mode");
                        fixture.environment.narrow_history = history == "narrow-history";
                        fixture.environment.successor_history = history == "successor-history";
                    }
                }
            }
            const bool completed = fixture.run_concurrent_stream(limit);
            require(completed, "production stream failed: " + fixture.phase);
            auto &env = fixture.environment;
            env.finish(fixture.actions, fixture.coverage, fixture.constraint_coverage);
            require(fixture.actions <= limit && (limit == 0 || fixture.actions != 0), "stream violated caller instruction limit");
            std::cout << "{\"schedule\":\"" << join(env.trace) << "\",\"actions\":" << fixture.actions
                << ",\"peak_live\":" << env.peak_live << ",\"peak_accepted\":" << env.peak_accepted
                << ",\"overlap_admissions\":" << env.overlap_admissions << ",\"continued_admissions\":" << env.continued_admissions
                << ",\"loads_before_older_std\":" << env.loads_before_older_std << ",\"head_checks\":" << env.head_checks
                << ",\"dependency_checks\":" << env.dependency_checks << ",\"issues\":" << env.issues
                << ",\"admissions_with_completed_head\":" << env.admissions_with_completed_head
                << ",\"issues_with_completed_head\":" << env.issues_with_completed_head
                << ",\"recovery_windows\":" << fixture.coverage.stream_recovery[0]
                << ",\"recovery_rounds\":" << fixture.coverage.stream_recovery[6]
                << ",\"recovery_prefix\":" << fixture.coverage.stream_recovery[11]
                << ",\"refetch_issues\":" << env.refetch_issues
                << ",\"surviving_prefix_redirects\":" << env.surviving_prefix_redirects
                << ",\"checked_scalar_values\":" << env.checked_scalar_values
                << ",\"lq_canceled\":" << env.canceled_lq
                << ",\"stream_history\":[" << fixture.coverage.stream_history[0] << "," << fixture.coverage.stream_history[1]
                << "," << fixture.coverage.stream_history[2] << "," << fixture.coverage.stream_history[3] << "]"
                << ",\"stream_sta\":[";
            for (unsigned i = 0; i < fixture.coverage.stream_sta.size(); ++i)
                std::cout << (i ? "," : "") << fixture.coverage.stream_sta[i];
            std::cout << "],\"stream_transport\":[";
            for (unsigned i=0; i<fixture.coverage.stream_transport.size(); ++i)
                std::cout << (i ? "," : "") << fixture.coverage.stream_transport[i];
            std::cout << "],\"stream_issue_ports\":[";
            for (unsigned i=0; i<fixture.coverage.stream_issue_ports.size(); ++i)
                std::cout << (i ? "," : "") << fixture.coverage.stream_issue_ports[i];
            std::cout << "],\"stream_store_pair\":[";
            for (unsigned i = 0; i < fixture.coverage.stream_store_pair.size(); ++i)
                std::cout << (i ? "," : "") << fixture.coverage.stream_store_pair[i];
            std::cout << "]" << ",\"vstart_cross_page\":" << std::count_if(env.instructions.begin(), env.instructions.end(),
                [](const auto &work) { return work.needs_restart_flush() && work.needs_pending_store; });
            std::cout << ",\"stream_vstart_flush\":[";
            for (unsigned i = 0; i < fixture.coverage.stream_vstart_flush.size(); ++i)
                std::cout << (i ? "," : "") << fixture.coverage.stream_vstart_flush[i];
            std::cout << "]" << ",\"stream_store_retirement\":[";
            for (unsigned i = 0; i < fixture.coverage.stream_store_retirement.size(); ++i)
                std::cout << (i ? "," : "") << fixture.coverage.stream_store_retirement[i];
            std::cout << "]"
                << ",\"address_misses\":" << env.address_misses
                << ",\"dependency_edges\":" << env.dependencies.size()
                << ",\"dependency_operand_checks\":" << env.dependency_operand_checks
                << ",\"dependency_counts\":[";
            for (unsigned i = 0; i < fixture.coverage.stream_dependencies.size(); ++i)
                std::cout << (i ? "," : "") << fixture.coverage.stream_dependencies[i];
            std::cout << "]"
                << ",\"cold_attempts\":" << fixture.coverage.stream_recovery_inflight[0]
                << ",\"cold_unavailable\":" << fixture.coverage.stream_recovery_inflight[1]
                << ",\"cold_selected\":" << fixture.coverage.stream_recovery_inflight[2]
                << ",\"cold_canceled\":" << env.cold_cancellations
                << ",\"old_released\":" << env.cold_releases
                << ",\"cold_refetched\":" << env.cold_refetches
                << ",\"old_prefix_producers\":" << env.prefix_old_producers
                << ",\"old_prefix_retirements\":" << env.prefix_old_retirements
                << ",\"commits\":" << env.commits << ",\"lq_allocated\":" << env.lq_used
                << ",\"sq_allocated\":" << env.sq_used << ",\"producer_counts\":[";
            for (unsigned i = 0; i < env.producer_counts.size(); ++i) std::cout << (i ? "," : "") << env.producer_counts[i];
            std::cout << "],\"kinds\":[";
            for (unsigned i = 0; i < env.instructions.size(); ++i) std::cout << (i ? "," : "") << env.instructions[i].kind;
            std::cout << "]}\n";
        } else if (mode == "reservation") {
            require(argc == 4, "reservation profile/available required");
            const auto policy = RandomConstraints::preset(argv[2]);
            const auto available = static_cast<unsigned>(std::stoul(argv[3]));
            std::cout << "{\"floor\":" << policy.minimum_serial_actions()
                << ",\"budget\":" << policy.concurrent_action_budget(available)
                << ",\"rate\":" << policy.concurrent_actions_per_mille << "}\n";
        } else if (mode == "budget") {
            require(argc == 6, "budget seed/total/concurrent/rate required");
            BudgetFixture fixture(std::stoull(argv[2]), std::stoul(argv[3]), std::stoul(argv[4]), std::stoul(argv[5]));
            require(fixture.execute(), "production execute failed");
            std::cout << "{\"schedule\":\"" << join(fixture.trace) << "\",\"actions\":" << fixture.actions
                << ",\"concurrent\":" << fixture.coverage.stream[2] + fixture.coverage.concurrent_actions << "}\n";
        } else throw std::runtime_error("unknown fixture mode");
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n'; return 1;
    }
}
