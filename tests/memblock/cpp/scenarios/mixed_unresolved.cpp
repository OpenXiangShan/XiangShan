#include "mixed/scenario.hpp"
#include "mixed/unresolved_graph.hpp"
#include <set>

namespace memblock::scenarios::mixed {

bool MixedScenario::execute_scalar_unresolved(ActionWindow &window)
{
    phase = "random-scalar-unresolved";
    const unsigned timeout = constrained_completion_timeout;
    if (!environment.run_until_all_complete(timeout) ||
        !environment.run_until_queues_retired(timeout)) return false;
    const auto base = action_cacheable_address(window, 64, false) & ~std::uint64_t{63};
    auto plan = make_unresolved_store_graph(random,
        constraints.scalar_unresolved_conflict_per_mille, constraints.scalar_load_policy());
    const auto pages = reference_pages_for(base);
    std::array<unsigned char, 64> snapshot{};
    for (unsigned byte = 0; byte < snapshot.size(); ++byte)
        snapshot[byte] = environment.memory().read_byte(pages
            ? pages->physical_address(base + byte) : base + byte);
    distinguish_unresolved_graph(plan, snapshot);
    const auto byte_stores = unresolved_graph_bytes(plan);
    const auto initial = scalar_dependency_expected(snapshot, {}, plan.load);
    const auto final = scalar_dependency_expected(snapshot, byte_stores, plan.load);
    if ((initial != final) != plan.conflict) {
        phase += ":indistinguishable-plan";
        return false;
    }
    std::set<unsigned> words;
    for (const auto &store : byte_stores) words.insert(store.offset & ~7U);
    const auto pdests = unresolved_graph_destinations(random, 2U + words.size(), plan.load.fp
        ? memblock::kFpPhysicalRegisters : memblock::kIntegerPhysicalRegisters);
    auto warm = make_load(base, memblock::LoadOp::ld, random() % memblock::kScalarLoadLanes);
    warm.pdest = pdests[0];
    if (!environment.set_rob_head_state(warm.rob, warm.rob_flag, false) ||
        !issue_load(warm, std::nullopt, false) ||
        !environment.run_until_queues_retired(timeout)) return false;
    coverage.sample(warm);
    const auto anchor = rob_offset++;
    if (!environment.set_rob_head_state(memblock::rob_pointer_value(anchor),
            memblock::rob_pointer_flag(anchor), false)) return false;
    std::vector<memblock::StoreTransaction> stores;
    for (const auto &item : byte_stores) {
        // S0's producers share A with its unfinished independent FP member.
        // Later producer groups are separated by stores, so compression cannot
        // merge their ROB identities. Their operand readiness is abstracted.
        if (!stores.empty()) ++rob_offset;
        stores.push_back(make_store(base + item.offset, item.data,
            static_cast<memblock::StoreOp>(std::countr_zero(item.width)),
            random() % memblock::kScalarStoreLanes));
    }
    auto load = make_load(base + plan.load.offset,
        static_cast<memblock::LoadOp>(plan.load.op), random() % memblock::kScalarLoadLanes,
        plan.load.fp);
    load.pdest = pdests[1];
    load.store_set_hit = load.load_wait_bit = load.load_wait_strict = false;
    load.wait_for_rob_flag = false;
    load.wait_for_rob_value = 0;
    UnresolvedLoadObligation obligation(load, plan.conflict);
    const auto violations_before = environment.memory_violation_stats().count;
    const auto sq_before = environment.sq_dequeued();
    std::vector<bool> data_issued(stores.size(), false), address_issued(stores.size(), false);
    std::vector<bool> data_first(stores.size(), false);
    unsigned early_data = 0;
    for (const auto &store : stores) {
        environment.expect_store(store);
        if (!environment.enqueue_store(store)) return false;
    }
    environment.expect_load_data(load, initial);
    if (!environment.enqueue_load(load)) return false;
    for (unsigned index = 0; index < stores.size(); ++index) {
        if (!plan.stores[index].data_before_load) continue;
        if (!environment.issue_store_data(stores[index], timeout)) return false;
        data_issued[index] = data_first[index] = true;
        ++early_data;
    }
    if (!environment.issue_load(load, timeout) ||
        !environment.run_until_load_complete(load, timeout) || !obligation.provisional() ||
        environment.sq_dequeued() != sq_before ||
        environment.memory_violation_stats().count != violations_before) {
        phase += ":provisional";
        return false;
    }
    const auto lq_cancel_before = environment.lq_redirect_canceled_observed();
    const auto sq_cancel_before = environment.sq_redirect_canceled_observed();
    bool recovered = false;
    unsigned address_rank = 0, first_conflict_rank = 0, address_inversions = 0;
    for (const auto &event : plan.after_provisional) {
        phase = "random-scalar-unresolved:stores=" + std::to_string(stores.size()) +
            ":operand=" + std::to_string(static_cast<unsigned>(event.kind)) +
            ":store=" + std::to_string(event.store) + ":recovered=" + std::to_string(recovered);
        if (!environment.run_cycles(event.delay) ||
            environment.memory_violation_stats().count != violations_before + unsigned(recovered)) return false;
        const auto &store = stores.at(event.store);
        if (event.kind == UnresolvedOperandKind::data) {
            if (data_issued[event.store] || !environment.issue_store_data(store, timeout)) return false;
            data_first[event.store] = !address_issued[event.store];
            data_issued[event.store] = true;
        } else {
            if (address_issued[event.store]) return false;
            for (unsigned older = 0; older < event.store; ++older)
                address_inversions += !address_issued[older];
            if (!environment.issue_store_address_until_tlb_hit(store, timeout)) return false;
            address_issued[event.store] = true;
            ++address_rank;
            const auto &bytes = byte_stores[event.store];
            const bool overlap = bytes.offset < plan.load.offset + plan.load.width() &&
                plan.load.offset < bytes.offset + bytes.width;
            if (overlap && !recovered) {
                // Do not resolve another address while this RAW request is
                // pending: each overlapping STA can independently select L.
                for (unsigned cycle = 0; cycle < timeout &&
                     environment.memory_violation_stats().count == violations_before; ++cycle)
                    if (!environment.run_cycles(1)) return false;
                const auto violation = environment.memory_violation_stats();
                if (violation.count != violations_before + 1 || !obligation.redirect(violation.last)) {
                    phase += ":missing-or-mismatched-redirect";
                    return false;
                }
                const auto return_cycle = violation.last_cycle + 2U;
                if (environment.cycle() > return_cycle ||
                    !environment.run_cycles(static_cast<unsigned>(return_cycle - environment.cycle())) ||
                    !environment.redirect_after(load.rob, load.rob_flag, true) ||
                    !obligation.returned() || !environment.run_cycles(8)) {
                    phase += ":redirect-return";
                    return false;
                }
                const auto canceled = environment.lq_redirect_canceled_observed() - lq_cancel_before;
                if (canceled > 1 || canceled > lq_offset ||
                    environment.sq_redirect_canceled_observed() != sq_cancel_before ||
                    environment.sq_dequeued() != sq_before) {
                    phase += ":redirect-accounting";
                    return false;
                }
                lq_offset -= canceled;
                coverage.scalar_unresolved_lq_canceled += canceled;
                recovered = true;
                first_conflict_rank = address_rank;
            }
        }
        if (environment.sq_dequeued() != sq_before ||
            environment.memory_violation_stats().count != violations_before + unsigned(recovered)) {
            phase += ":unexpected-side-effect";
            return false;
        }
    }
    if (!std::all_of(data_issued.begin(), data_issued.end(), [](bool issued) { return issued; }) ||
        !std::all_of(address_issued.begin(), address_issued.end(), [](bool issued) { return issued; }) ||
        recovered != plan.conflict || !environment.run_until_store_complete(timeout) ||
        environment.memory_violation_stats().count != violations_before + unsigned(recovered)) return false;
    unsigned forwarded = 0;
    for (unsigned byte = plan.load.offset; byte < plan.load.offset + plan.load.width(); ++byte)
        forwarded += std::any_of(byte_stores.begin(), byte_stores.end(), [&](const auto &store) {
            return byte >= store.offset && byte < store.offset + store.width;
        });
    if (recovered) {
        // Every older address/data obligation is now complete. The surviving
        // prefix supplies exact bytes before any store is allowed to retire.
        auto replay = load;
        replay.lq = memblock::lq_pointer_value(lq_offset);
        replay.lq_flag = memblock::lq_pointer_flag(lq_offset++);
        environment.expect_load_data(replay, final);
        if (!environment.enqueue_load(replay) || !environment.issue_load(replay, timeout) ||
            !environment.run_until_load_complete(replay, timeout) || !obligation.refetched()) return false;
        coverage.sample(replay);
        coverage.sample_fp_load(replay, final, 3, forwarded, plan.load.width() - forwarded);
    } else if (!environment.run_cycles(32) ||
        environment.memory_violation_stats().count != violations_before || !obligation.no_conflict()) return false;
    if (environment.sq_dequeued() != sq_before) return false;
    for (const auto &store : stores) {
        if (!environment.set_rob_head_state(store.rob, store.rob_flag, true) ||
            !environment.commit_store(store, timeout)) return false;
    }
    if (!environment.set_rob_head_state(load.rob, load.rob_flag, false) ||
        !environment.run_until_queues_retired(timeout) || !environment.pulse_sbuffer_flush() ||
        !environment.run_until_sbuffer_empty(timeout)) return false;
    unsigned role = 2;
    for (auto word : words) {
        auto readback = make_load(base + word, memblock::LoadOp::ld,
            random() % memblock::kScalarLoadLanes);
        readback.pdest = pdests[role++];
        const DependencyLoad expected_word{word, static_cast<unsigned>(memblock::LoadOp::ld), false};
        if (!environment.set_rob_head_state(readback.rob, readback.rob_flag, false) ||
            !issue_load(readback, scalar_dependency_expected(snapshot, byte_stores, expected_word), false) ||
            !environment.run_until_queues_retired(timeout)) return false;
        coverage.sample(readback);
    }
    if (!obligation.readback() || !obligation.done() ||
        environment.memory_violation_stats().count != violations_before + unsigned(plan.conflict)) return false;
    for (unsigned index = 0; index < stores.size(); ++index) coverage.sample(stores[index], data_first[index]);
    coverage.sample(load);
    coverage.sample_fp_load(load, initial, 3, 0, plan.load.width());
    coverage.sample_scalar_format_choice(3, plan.load.fp);
    ++coverage.scalar_unresolved_windows;
    coverage.scalar_unresolved_conflicts += plan.conflict;
    coverage.scalar_unresolved_controls += !plan.conflict;
    ++coverage.scalar_unresolved_provisional;
    coverage.scalar_unresolved_redirects += recovered;
    coverage.scalar_unresolved_returned += recovered;
    coverage.scalar_unresolved_refetched += recovered;
    ++coverage.scalar_unresolved_readbacks;
    coverage.scalar_unresolved_data_before += early_data != 0;
    coverage.scalar_unresolved_overlap_bytes += forwarded;
    auto &graph = coverage.scalar_unresolved_graph;
    graph[0] += stores.size(); graph[1] += stores.size() > 1;
    graph[2] += early_data; graph[3] += plan.after_provisional.size();
    graph[4] += address_inversions; graph[5] = std::max<std::uint64_t>(graph[5], stores.size());
    graph[6] += recovered ? stores.size() : 0; graph[7] += words.size();
    for (unsigned first = 0; first < byte_stores.size(); ++first)
        for (unsigned second = first + 1; second < byte_stores.size(); ++second)
            graph[8] += byte_stores[first].offset < byte_stores[second].offset + byte_stores[second].width &&
                byte_stores[second].offset < byte_stores[first].offset + byte_stores[first].width;
    if (recovered) ++coverage.scalar_unresolved_first_conflict[first_conflict_rank - 1U];
    coverage.cacheable += 1U + stores.size() + 1U + unsigned(recovered) + words.size();
    window.probe_candidate = pages ? pages->physical_address(base) : base;
    ++actions;
    return true;
}

} // namespace memblock::scenarios::mixed
