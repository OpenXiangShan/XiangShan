#include "mixed/scenario.hpp"
#include "mixed/unresolved_epochs.hpp"
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
    std::optional<std::uint64_t> cold_address, cold_physical;
    auto &inflight = coverage.scalar_unresolved_inflight;
    if (plan.conflict && random() % 1000U < constraints.scalar_unresolved_inflight_per_mille) {
        ++inflight[0];
        // A never-requested line could still have older committed bytes in
        // SBuffer. Drain those producers before classifying a held cold load.
        if (!environment.pulse_sbuffer_flush() ||
            !environment.run_until_sbuffer_empty(timeout)) return false;
        const unsigned first = random() % 64U;
        for (unsigned step = 0; step < 64; ++step) {
            const auto candidate = (base & ~std::uint64_t{4095}) + ((first + step) % 64U) * 64U;
            const auto physical = pages ? pages->physical_address(candidate) : candidate;
            if (candidate == base || environment.dcache_requests_covering_since(physical, 0) != 0) continue;
            cold_address = candidate;
            cold_physical = physical;
            // Arm before another DUT tick can prefetch this fresh physical line.
            environment.hold_dcache_response_at(physical);
            break;
        }
        inflight[1] += !cold_address;
    }
    const auto group = make_unresolved_load_group(random, plan, constraints.scalar_load_policy(),
        4, cold_address.has_value());
    if (cold_address) {
        const auto width = group.loads.back().width();
        *cold_address += (random() % (64U / width)) * width;
        ++inflight[2];
        inflight[7] += group.loads.back().fp;
    }
    distinguish_unresolved_epochs(plan, group, snapshot);
    const auto byte_stores = unresolved_graph_bytes(plan);
    UnresolvedEpochImage image(snapshot, byte_stores);
    if (plan.conflict != !group.rounds.empty()) return false;
    std::uint64_t cold_expected = 0;
    if (cold_address) {
        const auto &shape = group.loads.back();
        std::uint64_t payload = 0;
        const auto physical = pages ? pages->physical_address(*cold_address) : *cold_address;
        for (unsigned byte = 0; byte < shape.width(); ++byte)
            payload |= std::uint64_t{environment.memory().read_byte(physical + byte)} << (byte * 8U);
        cold_expected = memblock::scalar_load_value(static_cast<memblock::LoadOp>(shape.op), shape.fp, payload);
    }
    const auto expected_value = [&](unsigned index) {
        return group.unfinished && index == *group.unfinished
            ? cold_expected : image.expected(group.loads[index]);
    };
    std::vector<std::uint64_t> current;
    for (unsigned index = 0; index < group.loads.size(); ++index) current.push_back(expected_value(index));
    std::set<unsigned> words;
    for (const auto &store : byte_stores) words.insert(store.offset & ~7U);
    const bool any_fp = std::any_of(group.loads.begin(), group.loads.end(),
        [](const auto &load) { return load.fp; });
    const auto pdests = unresolved_graph_destinations(random, 1U + group.loads.size() + words.size(), any_fp
        ? memblock::kFpPhysicalRegisters : memblock::kIntegerPhysicalRegisters);
    auto warm = make_load(base, memblock::LoadOp::ld, random() % memblock::kScalarLoadLanes);
    warm.pdest = pdests[0];
    if (!environment.set_rob_head_state(warm.rob, warm.rob_flag, false) ||
        !issue_load(warm, std::nullopt, false) || !environment.run_until_queues_retired(timeout)) return false;
    coverage.sample(warm);
    const auto anchor = rob_offset++;
    if (!environment.set_rob_head_state(memblock::rob_pointer_value(anchor),
            memblock::rob_pointer_flag(anchor), false)) return false;
    std::vector<memblock::StoreTransaction> stores;
    for (const auto &item : byte_stores) {
        // A contains S0's producers and an unfinished independent FP member;
        // later producer groups are separated by their consuming stores.
        if (!stores.empty()) ++rob_offset;
        stores.push_back(make_store(base + item.offset, item.data,
            static_cast<memblock::StoreOp>(std::countr_zero(item.width)),
            random() % memblock::kScalarStoreLanes));
    }
    std::vector<memblock::LoadTransaction> loads;
    const unsigned first_ftq = random() % 24U;
    std::uint64_t pc = 0x1000U + 32U * (random() % 16U);
    unsigned halfword = 0;
    for (unsigned index = 0; index < group.loads.size(); ++index) {
        const auto &shape = group.loads[index];
        auto load = make_load(group.unfinished && index == *group.unfinished ? *cold_address : base + shape.offset,
            static_cast<memblock::LoadOp>(shape.op), random() % memblock::kScalarLoadLanes, shape.fp);
        load.pdest = pdests[index + 1U];
        load.predecode_rvc = (shape.op == 3U || (!shape.fp && shape.op == 2U)) && (random() & 1U);
        load.pc = pc;
        load.ftq_ptr = first_ftq;
        load.ftq_offset = halfword;
        pc += load.predecode_rvc ? 2U : 4U;
        halfword += load.predecode_rvc ? 1U : 2U;
        load.store_set_hit = load.load_wait_bit = load.load_wait_strict = false;
        load.wait_for_rob_flag = false;
        load.wait_for_rob_value = 0;
        loads.push_back(load);
    }
    UnresolvedLoadGroupObligation obligation(loads, stores.size(), group.unfinished);
    const auto violations_before = environment.memory_violation_stats().count;
    const auto sq_before = environment.sq_dequeued();
    const auto sq_cancel_before = environment.sq_redirect_canceled_observed();
    const auto scalar_cancel_before = environment.scalar_load_redirect_canceled();
    std::vector<bool> data_issued(stores.size()), address_issued(stores.size()), data_first(stores.size());
    std::vector<bool> promoted(stores.size()), trained(loads.size());
    unsigned early_data = 0, promoted_data = 0;
    for (const auto &store : stores) {
        environment.expect_store(store);
        if (!environment.enqueue_store(store)) return false;
    }
    for (unsigned index = 0; index < loads.size(); ++index) {
        environment.expect_load_data(loads[index], current[index]);
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
        coverage.sample_scalar_format_choice(3, group.loads[index].fp);
        if (group.unfinished && index == *group.unfinished) continue;
        if (!environment.run_until_load_complete(loads[index], timeout) || !obligation.provisional(index) ||
            environment.sq_dequeued() != sq_before ||
            environment.memory_violation_stats().count != violations_before) return false;
        coverage.sample(loads[index]);
        coverage.sample_fp_load(loads[index], current[index], 3, 0, group.loads[index].width());
    }
    if (group.unfinished) {
        phase = "random-scalar-unresolved:hold-unfinished";
        for (unsigned cycle = 0; cycle < timeout && !environment.dcache_response_held_at(*cold_physical); ++cycle)
            if (!environment.run_cycles(1)) return false;
        if (!environment.dcache_response_held_at(*cold_physical) ||
            !environment.scalar_load_pending(loads[*group.unfinished])) return false;
        ++inflight[3];
    }
    unsigned rounds = 0, address_rank = 0, first_conflict_rank = 0, address_inversions = 0;
    unsigned first_competitors = 0, epoch_invalidated = 0, speculative_refetches = 0;
    unsigned changed_refetches = 0, unchanged_refetches = 0, known_store_sum = 0;
    unsigned refetch_inversions = 0, trained_refetches = 0, previous_victim = loads.size();
    for (const auto &event : plan.after_provisional) {
        phase = "random-scalar-unresolved:stores=" + std::to_string(stores.size()) +
            ":loads=" + std::to_string(loads.size()) + ":operand=" +
            std::to_string(static_cast<unsigned>(event.kind)) + ":store=" +
            std::to_string(event.store) + ":round=" + std::to_string(rounds);
        if (!environment.run_cycles(event.delay) ||
            environment.memory_violation_stats().count != violations_before + rounds) return false;
        const auto &store = stores.at(event.store);
        if (event.kind == UnresolvedOperandKind::data) {
            if (data_issued[event.store]) {
                if (!promoted[event.store]) return false;
                continue; // Already consumed at the matching known-data barrier.
            }
            if (!environment.issue_store_data(store, timeout)) return false;
            data_first[event.store] = !address_issued[event.store];
            data_issued[event.store] = true;
        } else {
            if (address_issued[event.store]) return false;
            for (unsigned older = 0; older < event.store; ++older)
                address_inversions += !address_issued[older];
            if (!environment.issue_store_address_until_tlb_hit(store, timeout) || !image.resolve(event.store)) return false;
            address_issued[event.store] = true;
            ++address_rank;
            unsigned victim = loads.size();
            for (unsigned index = 0; index < group.loads.size(); ++index)
                if (unresolved_overlap(byte_stores[event.store], group.loads[index])) { victim = index; break; }
            if (victim != loads.size()) {
                const auto &round = group.rounds.at(rounds);
                if (round.store != event.store || round.victim != victim) return false;
                for (unsigned cycle = 0; cycle < timeout &&
                     environment.memory_violation_stats().count == violations_before + rounds; ++cycle)
                    if (!environment.run_cycles(1)) return false;
                const auto violation = environment.memory_violation_stats();
                if (violation.count != violations_before + rounds + 1U ||
                    !obligation.redirect(violation.last, victim)) return false;
                const auto lq_cancel_before = environment.lq_redirect_canceled_observed();
                const auto return_cycle = violation.last_cycle + 2U;
                if (environment.cycle() > return_cycle ||
                    !environment.run_cycles(static_cast<unsigned>(return_cycle - environment.cycle())) ||
                    !environment.redirect_after(loads[victim].rob, loads[victim].rob_flag, true)) return false;
                if (rounds == 0 && group.unfinished) {
                    if (environment.lq_redirect_canceled_observed() == lq_cancel_before ||
                        !environment.dcache_response_held_at(*cold_physical) ||
                        !environment.cancel_scalar_load_after_redirect(loads[*group.unfinished]) ||
                        !obligation.initial_canceled(*group.unfinished)) return false;
                    ++inflight[4];
                }
                if (!obligation.returned() || !environment.run_cycles(8)) return false;
                const auto canceled = environment.lq_redirect_canceled_observed() - lq_cancel_before;
                if (canceled > loads.size() - victim || canceled > lq_offset ||
                    environment.sq_redirect_canceled_observed() != sq_cancel_before ||
                    environment.sq_dequeued() != sq_before) return false;
                lq_offset -= canceled;
                coverage.scalar_unresolved_lq_canceled += canceled;
                if (rounds == 0 && group.unfinished) {
                    const unsigned release_gap = random() % 8U;
                    inflight[8] += release_gap;
                    // Keep the old epoch unregistered while its complete D/E
                    // lifecycle drains. Any old RF completion is unexpected.
                    if (!environment.run_cycles(release_gap) ||
                        !environment.release_held_dcache_response_at(*cold_physical) ||
                        !environment.run_until_dcache_idle(timeout) ||
                        !environment.run_cycles(32) ||
                        environment.dcache_response_held_at(*cold_physical) ||
                        environment.scalar_load_pending(loads[*group.unfinished])) return false;
                    ++inflight[5];
                }
                if (rounds == 0) {
                    first_conflict_rank = address_rank;
                    for (unsigned index = victim + 1U; index < loads.size(); ++index)
                        first_competitors += unresolved_overlap(byte_stores[event.store], group.loads[index]);
                } else ++coverage.scalar_unresolved_victim_movement[victim < previous_victim ? 0U : victim == previous_victim ? 1U : 2U];
                previous_victim = victim;
                trained[victim] = true;
                // Each known address must have ready data before strict replay.
                // Pull only those still-pending STD events ahead, in random order.
                const auto data_order = unresolved_random_order(random, 0, stores.size());
                for (unsigned index : data_order) {
                    if (!address_issued[index] || data_issued[index]) continue;
                    if (!environment.issue_store_data(stores[index], timeout)) return false;
                    data_issued[index] = promoted[index] = true;
                    data_first[index] = false;
                    ++promoted_data;
                }
                for (unsigned index = 0; index < victim; ++index)
                    if (current[index] != expected_value(index)) return false;
                reset_unresolved_ftq_suffix(loads, victim);
                std::vector<std::uint64_t> expected = current;
                for (unsigned index = victim; index < loads.size(); ++index) {
                    auto &replay = loads[index];
                    replay.lq = memblock::lq_pointer_value(lq_offset);
                    replay.lq_flag = memblock::lq_pointer_flag(lq_offset++);
                    // Distinct initially-unassigned PCs: training marks a victim
                    // hit, while original untrained stores never entered LFST.
                    replay.store_set_hit = trained[index];
                    expected[index] = expected_value(index);
                    environment.expect_load_data(replay, expected[index]);
                    if (!environment.enqueue_load(replay)) return false;
                    ++epoch_invalidated;
                    speculative_refetches += image.known_count() < stores.size();
                    changed_refetches += current[index] != expected[index];
                    unchanged_refetches += current[index] == expected[index];
                    trained_refetches += replay.store_set_hit;
                }
                std::vector<bool> issued(loads.size());
                for (unsigned ordinal = 0; ordinal < round.issue_order.size(); ++ordinal) {
                    const unsigned index = round.issue_order[ordinal];
                    for (unsigned older = victim; older < index; ++older) refetch_inversions += !issued[older];
                    if (!environment.run_cycles(round.issue_delays[ordinal]) ||
                        !environment.issue_load(loads[index], timeout)) return false;
                    issued[index] = true;
                }
                for (unsigned index = victim; index < loads.size(); ++index) {
                    if (!environment.run_until_load_complete(loads[index], timeout) ||
                        !obligation.refetched(index, loads[index])) return false;
                    current[index] = expected[index];
                    inflight[6] += group.unfinished && index == *group.unfinished;
                    unsigned covered = 0;
                    const auto &shape = group.loads[index];
                    for (unsigned byte = shape.offset; byte < shape.offset + shape.width(); ++byte)
                        for (unsigned known = 0; known < byte_stores.size(); ++known) {
                            const auto &bytes = byte_stores[known];
                            if (address_issued[known] && byte >= bytes.offset && byte < bytes.offset + bytes.width) {
                                ++covered;
                                break;
                            }
                        }
                    coverage.sample(loads[index]);
                    coverage.sample_fp_load(loads[index], current[index], 3, covered, shape.width() - covered);
                }
                if (!obligation.finish_round()) return false;
                known_store_sum += image.known_count();
                ++rounds;
                ++coverage.scalar_unresolved_round_victim[victim];
            }
        }
        if (environment.sq_dequeued() != sq_before ||
            environment.memory_violation_stats().count != violations_before + rounds) return false;
    }
    if (!std::all_of(data_issued.begin(), data_issued.end(), [](bool issued) { return issued; }) ||
        !std::all_of(address_issued.begin(), address_issued.end(), [](bool issued) { return issued; }) ||
        rounds != group.rounds.size() || rounds != obligation.rounds() ||
        !environment.run_until_store_complete(timeout) || !environment.run_cycles(32) ||
        environment.memory_violation_stats().count != violations_before + rounds) return false;
    for (unsigned index = 0; index < loads.size(); ++index)
        if (current[index] != expected_value(index)) return false;
    if (!obligation.resolved() || environment.sq_dequeued() != sq_before ||
        environment.scalar_load_redirect_canceled() - scalar_cancel_before != unsigned(group.unfinished.has_value())) return false;
    for (const auto &store : stores)
        if (!environment.set_rob_head_state(store.rob, store.rob_flag, true) || !environment.commit_store(store, timeout)) return false;
    if (!environment.set_rob_head_state(loads.back().rob, loads.back().rob_flag, false) ||
        !environment.run_until_queues_retired(timeout) || !environment.pulse_sbuffer_flush() ||
        !environment.run_until_sbuffer_empty(timeout)) return false;
    unsigned role = 1U + loads.size();
    for (auto word : words) {
        auto readback = make_load(base + word, memblock::LoadOp::ld, random() % memblock::kScalarLoadLanes);
        readback.pdest = pdests[role++];
        if (!environment.set_rob_head_state(readback.rob, readback.rob_flag, false) ||
            !issue_load(readback, image.expected({word, static_cast<unsigned>(memblock::LoadOp::ld), false}), false) ||
            !environment.run_until_queues_retired(timeout)) return false;
        coverage.sample(readback);
    }
    if (!obligation.readback() || !obligation.done() ||
        environment.memory_violation_stats().count != violations_before + rounds) return false;
    unsigned invalidated = 0, disjoint_invalidated = 0, forwarded = 0;
    for (unsigned index = 0; index < loads.size(); ++index) {
        if (obligation.executions(index) > 1 || (group.unfinished && index == *group.unfinished)) {
            ++invalidated;
            disjoint_invalidated += std::none_of(byte_stores.begin(), byte_stores.end(),
                [&](const auto &store) { return unresolved_overlap(store, group.loads[index]); });
        }
    }
    for (unsigned byte = plan.load.offset; byte < plan.load.offset + plan.load.width(); ++byte)
        forwarded += std::any_of(byte_stores.begin(), byte_stores.end(), [&](const auto &store) {
            return byte >= store.offset && byte < store.offset + store.width;
        });
    for (unsigned index = 0; index < stores.size(); ++index) coverage.sample(stores[index], data_first[index]);
    auto &load_group = coverage.scalar_unresolved_load_group;
    load_group[0] += loads.size(); load_group[1] += loads.size() > 1;
    load_group[2] += loads.size() - unsigned(group.unfinished.has_value()); load_group[3] += loads.size() - invalidated;
    load_group[4] += invalidated; load_group[5] += invalidated;
    load_group[6] += disjoint_invalidated; load_group[7] += invalidated - disjoint_invalidated;
    load_group[8] += first_competitors; load_group[9] += load_issue_inversions;
    load_group[10] = std::max<std::uint64_t>(load_group[10], loads.size());
    if (rounds) ++coverage.scalar_unresolved_victim[group.victim];
    auto &epochs = coverage.scalar_unresolved_epochs;
    epochs[0] += rounds; epochs[1] += rounds > 1;
    epochs[2] += epoch_invalidated; epochs[3] += epoch_invalidated;
    epochs[4] += epoch_invalidated - invalidated; epochs[5] += speculative_refetches;
    epochs[6] += changed_refetches; epochs[7] += unchanged_refetches;
    epochs[8] += known_store_sum; epochs[9] += promoted_data; epochs[10] += refetch_inversions;
    epochs[11] = std::max<std::uint64_t>(epochs[11], rounds); epochs[12] += trained_refetches;
    ++coverage.scalar_unresolved_windows;
    coverage.scalar_unresolved_conflicts += plan.conflict;
    coverage.scalar_unresolved_controls += !plan.conflict;
    ++coverage.scalar_unresolved_provisional;
    coverage.scalar_unresolved_redirects += rounds != 0;
    coverage.scalar_unresolved_returned += rounds != 0;
    coverage.scalar_unresolved_refetched += rounds != 0;
    ++coverage.scalar_unresolved_readbacks;
    coverage.scalar_unresolved_data_before += early_data != 0;
    coverage.scalar_unresolved_overlap_bytes += forwarded;
    auto &graph = coverage.scalar_unresolved_graph;
    graph[0] += stores.size(); graph[1] += stores.size() > 1;
    graph[2] += early_data; graph[3] += plan.after_provisional.size();
    graph[4] += address_inversions; graph[5] = std::max<std::uint64_t>(graph[5], stores.size());
    graph[6] += rounds ? stores.size() : 0; graph[7] += words.size();
    for (unsigned first = 0; first < byte_stores.size(); ++first)
        for (unsigned second = first + 1; second < byte_stores.size(); ++second)
            graph[8] += byte_stores[first].offset < byte_stores[second].offset + byte_stores[second].width &&
                byte_stores[second].offset < byte_stores[first].offset + byte_stores[first].width;
    if (rounds) ++coverage.scalar_unresolved_first_conflict[first_conflict_rank - 1U];
    coverage.cacheable += 1U + stores.size() + loads.size() + epoch_invalidated + words.size();
    window.probe_candidate = pages ? pages->physical_address(base) : base;
    ++actions;
    return true;
}

} // namespace memblock::scenarios::mixed
