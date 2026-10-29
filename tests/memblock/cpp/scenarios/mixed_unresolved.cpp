#include "mixed/scenario.hpp"
#include "mixed/unresolved_load_group.hpp"
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
    const auto group = make_unresolved_load_group(random, plan, constraints.scalar_load_policy());
    distinguish_unresolved_load_group(plan, group, snapshot);
    const auto byte_stores = unresolved_graph_bytes(plan);
    std::vector<std::uint64_t> initial, final;
    for (const auto &shape : group.loads) {
        initial.push_back(scalar_dependency_expected(snapshot, {}, shape));
        final.push_back(scalar_dependency_expected(snapshot, byte_stores, shape));
        const bool overlaps = std::any_of(byte_stores.begin(), byte_stores.end(),
            [&](const auto &store) { return unresolved_overlap(store, shape); });
        if ((initial.back() != final.back()) != overlaps) {
            phase += ":indistinguishable-plan";
            return false;
        }
    }
    std::set<unsigned> words;
    for (const auto &store : byte_stores) words.insert(store.offset & ~7U);
    const bool any_fp = std::any_of(group.loads.begin(), group.loads.end(),
        [](const auto &load) { return load.fp; });
    const auto pdests = unresolved_graph_destinations(random, 1U + group.loads.size() + words.size(), any_fp
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
    std::vector<memblock::LoadTransaction> loads;
    for (unsigned index = 0; index < group.loads.size(); ++index) {
        const auto &shape = group.loads[index];
        auto load = make_load(base + shape.offset,
            static_cast<memblock::LoadOp>(shape.op), random() % memblock::kScalarLoadLanes, shape.fp);
        load.pdest = pdests[index + 1U];
        load.store_set_hit = load.load_wait_bit = load.load_wait_strict = false;
        load.wait_for_rob_flag = false;
        load.wait_for_rob_value = 0;
        loads.push_back(load);
    }
    const auto &primary = loads[group.primary];
    UnresolvedLoadGroupObligation obligation(loads, group.victim);
    const auto violations_before = environment.memory_violation_stats().count;
    const auto sq_before = environment.sq_dequeued();
    std::vector<bool> data_issued(stores.size(), false), address_issued(stores.size(), false);
    std::vector<bool> data_first(stores.size(), false);
    unsigned early_data = 0;
    for (const auto &store : stores) {
        environment.expect_store(store);
        if (!environment.enqueue_store(store)) return false;
    }
    for (unsigned index = 0; index < loads.size(); ++index) {
        environment.expect_load_data(loads[index], initial[index]);
        if (!environment.enqueue_load(loads[index])) return false;
    }
    for (unsigned index = 0; index < stores.size(); ++index) {
        if (!plan.stores[index].data_before_load) continue;
        if (!environment.issue_store_data(stores[index], timeout)) return false;
        data_issued[index] = data_first[index] = true;
        ++early_data;
    }
    unsigned load_issue_inversions = 0;
    std::vector<bool> load_issued(loads.size());
    for (unsigned event = 0; event < group.issue_order.size(); ++event) {
        const unsigned index = group.issue_order[event];
        for (unsigned older = 0; older < index; ++older) load_issue_inversions += !load_issued[older];
        if (!environment.run_cycles(group.issue_delays[event]) ||
            !environment.issue_load(loads[index], timeout)) return false;
        load_issued[index] = true;
    }
    for (unsigned index = 0; index < loads.size(); ++index) {
        if (!environment.run_until_load_complete(loads[index], timeout) || !obligation.provisional(index) ||
            environment.sq_dequeued() != sq_before ||
            environment.memory_violation_stats().count != violations_before) {
            phase += ":provisional=" + std::to_string(index);
            return false;
        }
    }
    const auto lq_cancel_before = environment.lq_redirect_canceled_observed();
    const auto sq_cancel_before = environment.sq_redirect_canceled_observed();
    bool recovered = false;
    unsigned address_rank = 0, first_conflict_rank = 0, address_inversions = 0, competitors = 0;
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
            unsigned oldest = loads.size();
            for (unsigned index = 0; index < group.loads.size(); ++index)
                if (unresolved_overlap(bytes, group.loads[index])) { oldest = index; break; }
            const bool overlap = oldest != loads.size();
            if (overlap && oldest != group.victim) {
                phase += ":invalid-oldest-victim-geometry";
                return false;
            }
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
                for (unsigned index = group.victim + 1U; index < loads.size(); ++index)
                    competitors += unresolved_overlap(bytes, group.loads[index]);
                const auto return_cycle = violation.last_cycle + 2U;
                if (environment.cycle() > return_cycle ||
                    !environment.run_cycles(static_cast<unsigned>(return_cycle - environment.cycle())) ||
                    !environment.redirect_after(primary.rob, primary.rob_flag, true) ||
                    !obligation.returned() || !environment.run_cycles(8)) {
                    phase += ":redirect-return";
                    return false;
                }
                const auto canceled = environment.lq_redirect_canceled_observed() - lq_cancel_before;
                if (canceled > loads.size() - group.victim || canceled > lq_offset ||
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
    unsigned invalidated = 0, disjoint_invalidated = 0;
    if (recovered) {
        // All older operands are now complete. Restore every invalidated
        // instruction in program order, preserving ROB, destination and SQ
        // snapshots; only its observed-allocation LQ epoch is fresh.
        auto replays = loads;
        for (unsigned index = group.victim; index < loads.size(); ++index) {
            auto &replay = replays[index];
            replay.lq = memblock::lq_pointer_value(lq_offset);
            replay.lq_flag = memblock::lq_pointer_flag(lq_offset++);
            environment.expect_load_data(replay, final[index]);
            if (!environment.enqueue_load(replay)) return false;
            ++invalidated;
            disjoint_invalidated += initial[index] == final[index];
        }
        for (unsigned index : group.refetch_order)
            if (!environment.issue_load(replays[index], timeout)) return false;
        for (unsigned index = group.victim; index < loads.size(); ++index) {
            const auto &replay = replays[index];
            if (!environment.run_until_load_complete(replay, timeout) || !obligation.refetched(index)) return false;
            unsigned covered = 0;
            const auto &shape = group.loads[index];
            for (unsigned byte = shape.offset; byte < shape.offset + shape.width(); ++byte)
                covered += std::any_of(byte_stores.begin(), byte_stores.end(), [&](const auto &store) {
                    return byte >= store.offset && byte < store.offset + store.width;
                });
            coverage.sample(replay);
            coverage.sample_fp_load(replay, final[index], 3, covered, shape.width() - covered);
        }
    } else if (!environment.run_cycles(32) ||
        environment.memory_violation_stats().count != violations_before) return false;
    if (!obligation.resolved()) return false;
    if (environment.sq_dequeued() != sq_before) return false;
    for (const auto &store : stores) {
        if (!environment.set_rob_head_state(store.rob, store.rob_flag, true) ||
            !environment.commit_store(store, timeout)) return false;
    }
    if (!environment.set_rob_head_state(loads.back().rob, loads.back().rob_flag, false) ||
        !environment.run_until_queues_retired(timeout) || !environment.pulse_sbuffer_flush() ||
        !environment.run_until_sbuffer_empty(timeout)) return false;
    unsigned role = 1U + loads.size();
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
    for (unsigned index = 0; index < loads.size(); ++index) {
        coverage.sample(loads[index]);
        coverage.sample_fp_load(loads[index], initial[index], 3, 0, group.loads[index].width());
        coverage.sample_scalar_format_choice(3, group.loads[index].fp);
    }
    auto &load_group = coverage.scalar_unresolved_load_group;
    load_group[0] += loads.size(); load_group[1] += loads.size() > 1;
    load_group[2] += loads.size(); load_group[3] += loads.size() - invalidated;
    load_group[4] += invalidated; load_group[5] += invalidated;
    load_group[6] += disjoint_invalidated; load_group[7] += invalidated - disjoint_invalidated;
    load_group[8] += competitors; load_group[9] += load_issue_inversions;
    load_group[10] = std::max<std::uint64_t>(load_group[10], loads.size());
    if (recovered) ++coverage.scalar_unresolved_victim[group.victim];
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
    coverage.cacheable += 1U + stores.size() + loads.size() + invalidated + words.size();
    window.probe_candidate = pages ? pages->physical_address(base) : base;
    ++actions;
    return true;
}

} // namespace memblock::scenarios::mixed
