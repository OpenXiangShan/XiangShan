#pragma once

#include "memblock_env.hpp"

namespace memblock::scenarios {

// Isolated ordinary store/load producer path. Architectural retirement and SQ
// cleanup are distinct even when this directed case waits for both.
template <class Env>
bool retire_alias_store(Env &environment, const StoreTransaction &store)
{
    environment.expect_store(store);
    if (!environment.set_rob_head_state(store.rob, store.rob_flag, true) ||
        !environment.enqueue_store(store)) return false;
    const auto target = environment.sq_allocated() - environment.sq_canceled();
    // This isolated instruction sequence has a non-store circular successor.
    // ROB's old-head scommit tuple lasts one edge, even if SQ/cache drain waits.
    const auto next_rob = std::uint64_t{store.rob} +
        (store.rob_flag ? kRobEntries : 0U) + 1U;
    return environment.issue_store_address_until_tlb_hit(store, 4096) &&
        environment.issue_store_data(store, 2048) &&
        environment.run_until_store_complete(16384) &&
        environment.run_cycles(kStreamScalarWritebackToHead - 2U) &&
        environment.retire_store(store) &&
        environment.set_rob_head_state(rob_pointer_value(next_rob),
            rob_pointer_flag(next_rob), false) &&
        environment.run_until_sq_dequeued(target, 16384);
}

struct AliasRefetchResult {
    std::uint64_t discarded = 0;
    bool refetched = false;
    const char *phase = "initial-load";
};

// A provisional data pulse is excused only by LoadScoreboard's simultaneous
// matching flush-itself rule. This helper supplies the corresponding backend
// return and a fresh strict data obligation; it never learns expected bytes.
template <class Env>
bool complete_alias_load(Env &environment, LoadTransaction load,
    std::uint64_t expected, AliasRefetchResult &result)
{
    result = {};
    const auto violations_before = environment.memory_violation_stats().count;
    const auto discarded_before = environment.scalar_load_canceled_observed();
    const auto completed_before = environment.writebacks();
    const auto allocated_before = environment.lq_allocated();
    const auto sq_canceled_before = environment.sq_canceled();
    load.tolerate_canceled_result = true;
    environment.expect_load_data(load, expected);
    if (!environment.set_rob_head_state(load.rob, load.rob_flag, false) ||
        !environment.enqueue_load(load) ||
        !environment.issue_load(load, 4096) ||
        !environment.run_until_complete(16384)) return false;

    result.discarded = environment.scalar_load_canceled_observed() - discarded_before;
    if (result.discarded > 1) return false;
    for (unsigned cycle = 0; cycle < 16 &&
         environment.memory_violation_stats().count == violations_before; ++cycle)
        if (!environment.run_cycles(1)) return false;
    const auto violation = environment.memory_violation_stats();
    result.refetched = violation.count == violations_before + 1;
    result.phase = "redirect-evidence";
    if (violation.count > violations_before + 1 ||
        (result.discarded && !result.refetched) ||
        (result.refetched && (!violation.last.valid || !violation.last.level ||
            violation.last.rob_value != load.rob ||
            violation.last.rob_flag != load.rob_flag || violation.last.ftq_flag ||
            violation.last.ftq_value != load.ftq_ptr ||
            violation.last.ftq_offset != load.ftq_offset ||
            violation.last.is_rvc != load.predecode_rvc))) return false;

    if (result.refetched) {
        result.phase = "redirect-recovery";
        // CtrlBlock and RedirectGenerator each register the isolated request.
        const auto return_cycle = violation.last_cycle + 2U;
        if (environment.cycle() > return_cycle ||
            !environment.run_cycles(static_cast<unsigned>(return_cycle - environment.cycle())) ||
            !environment.redirect_after(load.rob, load.rob_flag, true) ||
            !environment.run_until_queues_retired(4096) ||
            !environment.run_until_sbuffer_empty(4096) ||
            !environment.run_cycles(8)) return false;
        // Dequeue returns capacity; only observed cancellation rewinds dispatch.
        const auto next_lq = environment.lq_allocated() -
            environment.lq_redirect_canceled_observed();
        load.lq = lq_pointer_value(next_lq);
        load.lq_flag = lq_pointer_flag(next_lq);
        // No intervening rename allocation: preserve ROB, pdest and frontend
        // identity, with a new strict expectation and the recovered LQ cursor.
        load.tolerate_canceled_result = false;
        result.phase = "refetch";
        environment.expect_load_data(load, expected);
        if (!environment.set_rob_head_state(load.rob, load.rob_flag, false) ||
            !environment.enqueue_load(load) ||
            !environment.issue_load(load, 4096) ||
            !environment.run_until_complete(16384)) return false;
    }
    result.phase = "terminal-accounting";
    if (!environment.run_until_queues_retired(4096) ||
        !environment.run_cycles(kStreamScalarWritebackToHead - 1U) ||
        environment.lq_allocated() != allocated_before + 1U + result.refetched ||
        environment.lq_dequeued() + environment.lq_canceled() != environment.lq_allocated() ||
        environment.lq_canceled_unobserved() != 0 ||
        environment.sq_canceled() != sq_canceled_before ||
        environment.scalar_load_canceled_observed() != discarded_before + result.discarded ||
        environment.writebacks() != completed_before + 1U + result.refetched - result.discarded ||
        environment.memory_violation_stats().count != violation.count) return false;
    result.phase = "complete";
    return true;
}

} // namespace memblock::scenarios
