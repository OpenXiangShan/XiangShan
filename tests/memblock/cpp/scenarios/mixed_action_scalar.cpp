#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

bool MixedScenario::execute_scalar_load(ActionWindow &window)
{
    auto &dcache_load_error_denied = window.dcache_load_error_denied;
    auto &bank_conflict_action = window.bank_conflict_action;
    auto &bank_conflict_depth_index = window.bank_conflict_depth_index;
    auto &bank_conflict_bank = window.bank_conflict_bank;
    auto &translation = window.translation;
    auto &ptw_requests_before = window.ptw_requests_before;
    auto &sample_dcache = window.sample_dcache;
    auto &ordinary_leaf_addressed = window.ordinary_leaf_addressed;

    if (bank_conflict_action) {
        phase = "random-bank-conflict";
        const unsigned depth = bank_conflict_depth_index + 2U;
        if (bank_conflict_line + depth > bank_conflict_lines) {
            phase = "random-bank-conflict-address-space-exhausted";
            return false;
        }
        if (!environment.run_until_all_complete(
                constrained_completion_timeout) ||
            !environment.run_until_queues_retired(
                constrained_completion_timeout) ||
            !environment.run_until_dcache_idle(
                constrained_completion_timeout)) {
            return false;
        }
        std::vector<std::uint64_t> lines;
        lines.reserve(depth);
        for (unsigned index = 0; index < depth; ++index) {
            const std::uint64_t line_index =
                (bank_conflict_line_offset +
                    (bank_conflict_line + index) *
                        bank_conflict_line_stride) %
                bank_conflict_lines;
            const std::uint64_t line = bank_conflict_base +
                line_index * bank_conflict_line_bytes;
            const std::uint64_t address =
                line + bank_conflict_bank * sizeof(std::uint64_t);
            lines.push_back(line);
        }
        for (unsigned index = 0; index < depth; ++index) {
            const std::uint64_t line = lines[index];
            const std::uint64_t address =
                line + bank_conflict_bank * sizeof(std::uint64_t);
            const auto preload = make_load(
                address, memblock::LoadOp::ld,
                index % memblock::kScalarLoadLanes);
            if (!issue_load(preload, std::nullopt, false)) {
                return false;
            }
        }
        bank_conflict_line += depth;
        if (!environment.run_until_queues_retired(
                constrained_completion_timeout) ||
            !environment.run_until_dcache_idle(
                constrained_completion_timeout)) {
            return false;
        }
        // Setup fills are not constrained-random actions, but they are
        // real translated walk windows. Record that observation, then
        // classify the resident target wave from a fresh baseline so
        // translation reuse remains visible without inferring cache or
        // replay behavior.
        if (translation.regime !=
                RandomConstraints::translation_bare &&
            environment.ptw_requests() > ptw_requests_before) {
            ++constraint_coverage.translation_walk_windows;
        }
        ptw_requests_before = environment.ptw_requests();

        std::vector<memblock::LoadTransaction> transactions;
        transactions.reserve(depth);
        for (unsigned index = 0; index < depth; ++index) {
            transactions.push_back(make_load(
                lines[index] +
                    bank_conflict_bank * sizeof(std::uint64_t),
                memblock::LoadOp::ld, index));
            environment.expect_load(transactions.back());
        }
        const std::uint64_t writebacks_before_conflict =
            environment.writebacks();
        const std::uint64_t lq_before_conflict =
            environment.lq_dequeued();
        std::vector<unsigned> dispatch_lanes(depth);
        std::iota(
            dispatch_lanes.begin(), dispatch_lanes.end(), 0U);
        if (!environment.set_rob_head(
                transactions.front().rob,
                transactions.front().rob_flag) ||
            !environment.enqueue_load_batch(
                transactions, dispatch_lanes) ||
            !environment.issue_load_batch(
                transactions, constrained_completion_timeout, true) ||
            !environment.run_until_all_complete(
                constrained_completion_timeout) ||
            !environment.run_until_queues_retired(
                constrained_completion_timeout)) {
            return false;
        }
        const std::array<std::uint64_t, 4> terminal_delta{{
            1,
            depth,
            environment.writebacks() - writebacks_before_conflict,
            environment.lq_dequeued() - lq_before_conflict,
        }};
        if (terminal_delta[2] != depth ||
            terminal_delta[3] != depth) {
            std::ostringstream detail;
            detail << "random-bank-conflict-terminal"
                   << ":depth=" << depth
                   << ":bank=" << bank_conflict_bank
                   << ":terminal=";
            for (unsigned index = 0; index < terminal_delta.size();
                 ++index) {
                detail << (index == 0 ? "" : ",")
                       << terminal_delta[index];
            }
            phase = detail.str();
            return false;
        }
        for (const auto &transaction : transactions) {
            coverage.sample(transaction);
        }
        constraint_coverage.sample_bank_conflict(
            bank_conflict_depth_index, bank_conflict_bank,
            translation.regime, terminal_delta);
        constraint_coverage.sample_dcache_load(std::nullopt);
        sample_dcache = false;
        ++constraint_coverage.locality[0];
        ordinary_leaf_addressed = true;
        ++actions;
        coverage.cacheable += depth;
    } else {
        const auto shape = constraints.scalar_load_policy().choose(random);
        const auto op = static_cast<memblock::LoadOp>(shape.op);
        const unsigned size =
            1U << (static_cast<unsigned>(op) & 3U);
        const bool error_response =
            dcache_load_error_denied.has_value();
        const bool stride_shaped = !error_response &&
            constraints.stride_stream_per_mille != 0 &&
            random() % 1000 < constraints.stride_stream_per_mille;
        std::uint64_t address = 0;
        if (error_response) {
            constexpr std::uint64_t error_lines =
                dcache_error_span / 64;
            if (dcache_error_line >= error_lines) {
                phase = "random-dcache-load-error-address-space-exhausted";
                return false;
            }
            const std::uint64_t sequence = dcache_error_line++;
            const std::uint64_t line = sequence ^ (sequence >> 1);
            address = dcache_error_base + line * 64 +
                ((sequence & 1U) != 0 ? 0x38 : 0x18);
            ordinary_leaf_addressed = true;
        } else if (stride_shaped) {
            address = random_stride_base +
                (stride_stream_loads % random_stride_slots) *
                    random_stride_bytes;
            ordinary_leaf_addressed = true;
        } else {
            address = action_cacheable_address(window, size);
        }
        auto transaction = make_load(
            address, op, random() % 3, shape.fp);
        if (stride_shaped) {
            const std::uint64_t epoch =
                stride_stream_loads / random_stride_slots;
            transaction.pc = 0x6000 + epoch * 4;
            ++stride_stream_loads;
            ++constraint_coverage.locality[2];
        }
        if (error_response) {
            phase = *dcache_load_error_denied
                ? "random-dcache-load-denied"
                : "random-dcache-load-corrupt";
            if (!environment.run_until_dcache_idle(
                    constrained_completion_timeout) ||
                !environment.run_until_queues_retired(
                    constrained_completion_timeout)) {
                return false;
            }
            transaction.expected_exception_mask =
                *dcache_load_error_denied
                ? memblock::kExceptionLoadAccessFault
                : memblock::kExceptionHardwareError;
            const std::string error_operation_phase = phase;
            const std::uint64_t requests_before_error =
                environment.tilelink_requests();
            const std::uint64_t refills_before =
                environment.dcache_error_response_refills();
            const std::uint64_t error_responses_before =
                environment.dcache_error_response_requests();
            const std::uint64_t denied_beats_before =
                environment.dcache_denied_d_beats();
            const std::uint64_t corrupt_beats_before =
                environment.dcache_corrupt_d_beats();
            const std::uint64_t grant_acks_before =
                environment.dcache_error_response_grant_acks();
            const std::uint64_t bus_data_before =
                environment.bus_expected_load(transaction.address, op);
            const std::uint64_t lq_dequeued_before =
                environment.lq_dequeued();
            const std::uint64_t sq_dequeued_before =
                environment.sq_dequeued();
            const std::uint64_t lq_cancels_before =
                environment.lq_redirect_canceled_observed();
            const std::uint64_t sq_cancels_before =
                environment.sq_redirect_canceled_observed();
            environment.inject_dcache_response_error_at(
                transaction.address & ~std::uint64_t{63},
                *dcache_load_error_denied,
                !*dcache_load_error_denied);
            if (!environment.set_rob_head(
                    transaction.rob, transaction.rob_flag) ||
                !issue_load(transaction) ||
                !recover_random_precise_error(
                    transaction.rob, transaction.rob_flag, false,
                    lq_dequeued_before, sq_dequeued_before,
                    lq_cancels_before, sq_cancels_before) ||
                !environment.run_until_dcache_idle(
                    constrained_completion_timeout)) {
                return false;
            }
            const std::array<std::uint64_t, 5> manager_delta{{
                environment.dcache_error_response_requests() -
                    error_responses_before,
                environment.dcache_denied_d_beats() -
                    denied_beats_before,
                environment.dcache_corrupt_d_beats() -
                    corrupt_beats_before,
                environment.dcache_error_response_grant_acks() -
                    grant_acks_before,
                environment.dcache_error_response_refills() -
                    refills_before,
            }};
            if (environment.tilelink_requests() <
                    requests_before_error + 1 ||
                manager_delta != std::array<std::uint64_t, 5>{
                                     1, *dcache_load_error_denied ? 2U : 0U,
                                     2, 1, 1} ||
                (environment.dcache_last_error_response_address() &
                    ~std::uint64_t{63}) !=
                    (transaction.address & ~std::uint64_t{63}) ||
                environment.dcache_last_error_response_keyword() !=
                    ((transaction.address & 32U) != 0) ||
                environment.bus_expected_load(transaction.address, op) !=
                    bus_data_before) {
                std::ostringstream detail;
                detail << phase << ":manager-accounting"
                       << ":requests="
                       << environment.tilelink_requests() -
                        requests_before_error
                       << ":manager=" << manager_delta[0] << ','
                       << manager_delta[1] << ',' << manager_delta[2]
                       << ',' << manager_delta[3] << ','
                       << manager_delta[4]
                       << ":last_address=0x" << std::hex
                       << environment.dcache_last_error_response_address()
                       << ":expected_address=0x"
                       << transaction.address << std::dec
                       << ":last_keyword="
                       << environment.dcache_last_error_response_keyword()
                       << ":expected_keyword="
                       << ((transaction.address & 32U) != 0)
                       << ":operation_phase={" << error_operation_phase
                       << "}:rob=" << unsigned(transaction.rob) << ':'
                       << transaction.rob_flag
                       << ":lq=" << unsigned(transaction.lq) << ':'
                       << transaction.lq_flag
                       << ":op=0x" << std::hex
                       << static_cast<unsigned>(transaction.op)
                       << std::dec << ":accepted_a={"
                       << environment.dcache_request_summary_since(
                              requests_before_error)
                       << '}';
                phase = detail.str();
                return false;
            }
            constraint_coverage.sample_dcache_load(
                dcache_load_error_denied, manager_delta);
        } else {
            if (!issue_load(transaction)) {
                return false;
            }
            constraint_coverage.sample_dcache_load(std::nullopt);
        }
        ++coverage.cacheable;
        coverage.sample_scalar_format_choice(0, shape.fp);
    }

    return true;
}

bool MixedScenario::execute_load_merge(ActionWindow &window)
{
    auto &translation = window.translation;
    auto &ordinary_leaf_addressed = window.ordinary_leaf_addressed;

    phase = "random-load-merge";
    unsigned depth_index =
        constraints.choose_load_merge_depth(random());
    unsigned pattern =
        constraints.choose_load_merge_pattern(random());
    unsigned critical_beat = random() & 1U;
    bool shape_forced = false;
    for (unsigned candidate_depth = 0;
         candidate_depth < 2 && !shape_forced;
         ++candidate_depth) {
        if (constraints.load_merge_depth_weights[
                candidate_depth] == 0) {
            continue;
        }
        for (unsigned candidate_pattern = 0;
             candidate_pattern <
                 RandomConstraints::load_merge_pattern_count &&
             !shape_forced;
             ++candidate_pattern) {
            if (constraints.load_merge_pattern_weights[
                    candidate_pattern] == 0) {
                continue;
            }
            for (unsigned candidate_beat = 0;
                 candidate_beat < 2; ++candidate_beat) {
                if (constraint_coverage.load_merge_shapes[
                        candidate_depth][candidate_pattern]
                                                         [candidate_beat] == 0) {
                    depth_index = candidate_depth;
                    pattern = candidate_pattern;
                    critical_beat = candidate_beat;
                    shape_forced = true;
                    break;
                }
            }
        }
    }

    const unsigned depth = depth_index + 2;
    std::array<unsigned, 3> offsets{};
    const unsigned leader_offset = critical_beat ? 40U : 8U;
    offsets[0] = leader_offset;
    if (pattern == RandomConstraints::load_merge_same_address) {
        offsets[1] = leader_offset;
        offsets[2] = leader_offset;
    } else if (pattern ==
        RandomConstraints::load_merge_same_beat) {
        offsets[1] = critical_beat ? 48U : 16U;
        offsets[2] = critical_beat ? 56U : 24U;
    } else {
        offsets[1] = critical_beat ? 8U : 40U;
        offsets[2] = critical_beat ? 16U : 48U;
    }

    if (!environment.run_until_all_complete(
            constrained_completion_timeout) ||
        !environment.run_until_queues_retired(
            constrained_completion_timeout) ||
        !environment.run_until_dcache_idle(
            constrained_completion_timeout)) {
        return false;
    }
    constexpr std::uint64_t merge_lines = load_merge_span / 64;
    std::optional<std::uint64_t> fresh_line;
    while (load_merge_line < merge_lines) {
        const std::uint64_t sequence = load_merge_line++;
        const std::uint64_t line_index =
            sequence ^ (sequence >> 1);
        const std::uint64_t candidate =
            load_merge_base + line_index * 64;
        if (environment.dcache_requests_covering_since(
                candidate, 0) == 0) {
            fresh_line = candidate;
            break;
        }
    }
    if (!fresh_line) {
        phase = "random-load-merge-address-space-exhausted";
        return false;
    }
    const std::uint64_t line = *fresh_line;
    std::vector<memblock::LoadTransaction> transactions;
    transactions.reserve(depth);
    for (unsigned index = 0; index < depth; ++index) {
        const auto op = static_cast<memblock::LoadOp>(random() % 7);
        const std::uint64_t address = line + offsets[index];
        // Preserve the frozen constraint-stream sequence after
        // replacing the former late random byte write.
        static_cast<void>(random());
        transactions.push_back(make_load(address, op, index));
    }
    for (const auto &transaction : transactions) {
        environment.expect_load(transaction);
    }

    const std::uint64_t merge_requests_before =
        environment.tilelink_requests();
    const std::uint64_t refills_before =
        environment.dcache_refills();
    const std::uint64_t grant_acks_before =
        environment.dcache_grant_acks();
    const std::uint64_t acquire_perms_before =
        environment.dcache_acquire_perms();
    const std::uint64_t writebacks_before =
        environment.writebacks();
    std::vector<unsigned> dispatch_lanes(depth);
    std::iota(
        dispatch_lanes.begin(), dispatch_lanes.end(), 0U);
    if (!environment.set_rob_head(
            transactions.front().rob,
            transactions.front().rob_flag) ||
        !environment.enqueue_load_batch(
            transactions, dispatch_lanes) ||
        !environment.issue_load_batch(
            transactions, 4096, true) ||
        !environment.run_until_all_complete(
            constrained_completion_timeout) ||
        !environment.run_until_queues_retired(
            constrained_completion_timeout) ||
        !environment.run_until_dcache_idle(
            constrained_completion_timeout)) {
        return false;
    }
    const std::array<std::uint64_t, 4> manager_delta{{
        environment.dcache_requests_covering_since(
            line, merge_requests_before),
        environment.dcache_refills() - refills_before,
        environment.dcache_grant_acks() - grant_acks_before,
        environment.writebacks() - writebacks_before,
    }};
    if (manager_delta[0] < 1 ||
        manager_delta[1] < manager_delta[0] ||
        manager_delta[2] != manager_delta[1] +
                (environment.dcache_acquire_perms() - acquire_perms_before) ||
        manager_delta[3] != depth) {
        std::ostringstream detail;
        detail << "random-load-merge-manager"
               << ":depth=" << depth
               << ":pattern=" << pattern
               << ":critical_beat=" << critical_beat
               << ":manager=" << manager_delta[0] << ','
               << manager_delta[1] << ',' << manager_delta[2]
               << ',' << manager_delta[3]
               << ":perms="
               << (environment.dcache_acquire_perms() -
                   acquire_perms_before);
        phase = detail.str();
        return false;
    }
    for (const auto &transaction : transactions) {
        coverage.sample(transaction);
    }
    constraint_coverage.sample_load_merge(
        depth_index, pattern, critical_beat,
        translation.regime, depth, manager_delta);
    ordinary_leaf_addressed = true;
    ++actions;
    coverage.cacheable += depth;

    return true;
}

bool MixedScenario::execute_scalar_store(ActionWindow &window)
{
    auto &probe_candidate = window.probe_candidate;
    if (constraints.scalar_unresolved_per_mille != 0 &&
        random() % 1000U < constraints.scalar_unresolved_per_mille) {
        return execute_scalar_unresolved(window);
    }
    if (constraints.scalar_dependency_per_mille != 0 &&
        random() % 1000U < constraints.scalar_dependency_per_mille) {
        return execute_scalar_dependency(window);
    }

    const auto op = static_cast<memblock::StoreOp>(random() % 4);
    const unsigned size = 1U << static_cast<unsigned>(op);
    const auto transaction = make_store(
        action_cacheable_address(window, size), random(), op,
        random() % 2);
    if (!issue_store(transaction, (random() & 1U) != 0) ||
        !environment.commit_store(transaction, 8192) ||
        !environment.run_until_queues_retired(8192)) {
        return false;
    }
    probe_candidate = memblock::reference_byte_address(
        transaction, transaction.address) & ~std::uint64_t{63};
    ++coverage.cacheable;

    return true;
}

bool MixedScenario::execute_scalar_dependency(ActionWindow &window)
{
    phase = "random-scalar-dependency";
    const unsigned timeout = constrained_completion_timeout;
    if (!environment.run_until_all_complete(timeout) ||
        !environment.run_until_queues_retired(timeout)) {
        return false;
    }
    const std::uint64_t base = action_cacheable_address(window, 64, false) &
        ~std::uint64_t{63};
    std::array<unsigned char, 64> snapshot{};
    for (unsigned byte = 0; byte < snapshot.size(); ++byte) {
        const auto pages = reference_pages_for(base);
        snapshot[byte] = environment.memory().read_byte(pages
            ? pages->physical_address(base + byte) : base + byte);
    }
    auto plan = make_scalar_dependency_plan(random, constraints.scalar_load_policy());
    if (plan.younger) {
        // Make the age-exclusion control distinguishable in at least one byte.
        const auto expected = scalar_dependency_expected(
            snapshot, plan.older, plan.loads.front());
        if (static_cast<unsigned char>(plan.younger->data) ==
            static_cast<unsigned char>(expected)) {
            plan.younger->data ^= 1U;
        }
    }
    // An older non-memory instruction can hold retirement while ordinary
    // cacheable stores execute. No store is the pending ROB head yet.
    const std::uint64_t anchor = rob_offset++;
    if (!environment.set_rob_head(
            memblock::rob_pointer_value(anchor),
            memblock::rob_pointer_flag(anchor))) {
        return false;
    }
    const auto sq_before = environment.sq_dequeued();
    std::vector<memblock::StoreTransaction> stores;
    std::vector<memblock::LoadTransaction> loads;
    for (const auto &item : plan.older) {
        stores.push_back(make_store(
            base + item.offset, item.data,
            static_cast<memblock::StoreOp>(std::countr_zero(item.width)),
            random() % memblock::kScalarStoreLanes));
        environment.expect_store(stores.back());
        if (!environment.enqueue_store(stores.back())) {
            return false;
        }
    }
    // Keep simultaneous destinations distinct. Mixed-class windows use the
    // smaller FP domain so every selected class has a valid physical index.
    const unsigned register_count = std::any_of(plan.loads.begin(), plan.loads.end(),
        [](const auto &load) { return load.fp; })
        ? memblock::kFpPhysicalRegisters : memblock::kIntegerPhysicalRegisters;
    const unsigned first_pdest = 1U + random() % (register_count - plan.loads.size());
    for (const auto &item : plan.loads) {
        loads.push_back(make_load(base + item.offset,
            static_cast<memblock::LoadOp>(item.op),
            random() % memblock::kScalarLoadLanes, item.fp));
        loads.back().pdest = static_cast<std::uint8_t>(
            first_pdest + loads.size() - 1U);
        environment.expect_load_data(loads.back(),
            scalar_dependency_expected(snapshot, plan.older, item));
        if (!environment.enqueue_load(loads.back())) {
            return false;
        }
    }
    if (plan.younger) {
        stores.push_back(make_store(
            base + plan.younger->offset, plan.younger->data,
            static_cast<memblock::StoreOp>(std::countr_zero(plan.younger->width)),
            random() % memblock::kScalarStoreLanes));
        environment.expect_store(stores.back());
        if (!environment.enqueue_store(stores.back())) {
            return false;
        }
    }
    std::vector<bool> seen_address(stores.size(), false);
    std::vector<bool> seen_data(stores.size(), false);
    std::vector<bool> data_first(stores.size(), false);
    bool interleaved = false;
    for (const auto &event : plan.issue_events) {
        bool issued = false;
        switch (event.kind) {
        case DependencyIssueKind::address:
            issued = environment.issue_store_address_until_tlb_hit(
                stores[event.index], timeout);
            seen_address[event.index] = issued;
            break;
        case DependencyIssueKind::data:
            data_first[event.index] = !seen_address[event.index];
            issued = environment.issue_store_data(stores[event.index], timeout);
            seen_data[event.index] = issued;
            break;
        case DependencyIssueKind::load: {
            // Addresses are known, but data can still be unavailable. The
            // expected bytes include every older store regardless of arrival.
            if (!std::all_of(seen_address.begin(), seen_address.begin() + plan.older.size(),
                    [](bool seen) { return seen; })) {
                phase += ":load-before-older-address";
                return false;
            }
            issued = environment.issue_load(loads[event.index], timeout);
            if (issued) {
                const bool early = std::find(seen_address.begin(), seen_address.end(), false) != seen_address.end() ||
                    std::find(seen_data.begin(), seen_data.end(), false) != seen_data.end();
                unsigned pending_sources = 0;
                const auto &load = plan.loads[event.index];
                for (unsigned store = 0; store < plan.older.size(); ++store) {
                    const auto &source = plan.older[store];
                    pending_sources += !seen_data[store] &&
                        source.offset < load.offset + load.width() &&
                        load.offset < source.offset + source.width;
                }
                coverage.scalar_dependency_interleaving[0] += early;
                coverage.scalar_dependency_interleaving[1] += pending_sources != 0;
                coverage.scalar_dependency_interleaving[2] += pending_sources > 1;
                interleaved |= early;
            }
            break;
        }
        }
        if (!issued || !environment.run_cycles(event.delay) ||
            environment.sq_dequeued() != sq_before) {
            phase += ":operand-issue-or-premature-retirement";
            return false;
        }
    }
    coverage.scalar_dependency_interleaving[3] += interleaved;
    if (!environment.run_until_all_complete(timeout) ||
        environment.sq_dequeued() != sq_before) {
        phase += ":load-completion";
        return false;
    }
    for (unsigned index = 0; index < stores.size(); ++index) {
        if (!environment.commit_store(stores[index], timeout)) {
            return false;
        }
        coverage.sample(stores[index], data_first[index]);
    }
    if (!environment.set_rob_head(
            memblock::rob_pointer_value(rob_offset - 1U),
            memblock::rob_pointer_flag(rob_offset - 1U)) ||
        !environment.run_until_queues_retired(timeout)) {
        return false;
    }
    for (unsigned index = 0; index < loads.size(); ++index) {
        const auto &load = loads[index];
        const auto &item = plan.loads[index];
        coverage.sample(load);
        unsigned forwarded = 0;
        for (unsigned byte = 0; byte < item.width(); ++byte) {
            forwarded += std::any_of(plan.older.begin(), plan.older.end(), [&](const auto &store) {
                return item.offset + byte >= store.offset &&
                    item.offset + byte - store.offset < store.width;
            });
        }
        coverage.sample_fp_load(load, scalar_dependency_expected(snapshot, plan.older, item),
            2, forwarded, item.width() - forwarded);
        coverage.sample_scalar_format_choice(2, item.fp);
    }
    ++coverage.scalar_dependency_windows;
    coverage.scalar_dependency_stores += stores.size();
    coverage.scalar_dependency_loads += loads.size();
    coverage.scalar_dependency_younger += plan.younger.has_value();
    for (const auto &load : plan.loads) {
        std::array<bool, 4> sources{};
        for (unsigned byte = 0; byte < load.width(); ++byte) {
            std::optional<unsigned> source;
            for (unsigned store = 0; store < plan.older.size(); ++store) {
                const auto &item = plan.older[store];
                if (load.offset + byte >= item.offset &&
                    load.offset + byte - item.offset < item.width) {
                    source = store;
                }
            }
            if (source) {
                ++coverage.scalar_dependency_forwarded_bytes;
                sources[*source] = true;
            } else {
                ++coverage.scalar_dependency_memory_bytes;
            }
        }
        coverage.scalar_dependency_multiple_sources +=
            std::count(sources.begin(), sources.end(), true) > 1;
    }
    coverage.cacheable += stores.size() + loads.size();
    const auto pages = reference_pages_for(base);
    window.probe_candidate = pages ? pages->physical_address(base) : base;
    ++actions; // One compound action; all accepted uops remain accounted.
    return true;
}

bool MixedScenario::execute_scalar_unresolved(ActionWindow &window)
{
    phase = "random-scalar-unresolved";
    const unsigned timeout = constrained_completion_timeout;
    if (!environment.run_until_all_complete(timeout) ||
        !environment.run_until_queues_retired(timeout)) return false;
    const auto base = action_cacheable_address(window, 64, false) & ~std::uint64_t{63};
    auto plan = make_unresolved_dependency_plan(
        random, constraints.scalar_unresolved_conflict_per_mille, constraints.scalar_load_policy());
    std::array<unsigned char, 64> snapshot{};
    for (unsigned byte = 0; byte < snapshot.size(); ++byte) {
        const auto pages = reference_pages_for(base);
        snapshot[byte] = environment.memory().read_byte(pages
            ? pages->physical_address(base + byte) : base + byte);
    }
    if (plan.conflict) {
        const unsigned common = std::max(plan.store.offset, plan.load.offset);
        const unsigned shift = (common - plan.store.offset) * 8U;
        if (static_cast<unsigned char>(plan.store.data >> shift) == snapshot[common]) {
            plan.store.data ^= std::uint64_t{1} << shift;
        }
    }
    const auto initial = scalar_dependency_expected(snapshot, {}, plan.load);
    const auto final = scalar_dependency_expected(snapshot, {plan.store}, plan.load);
    if ((initial != final) != plan.conflict) {
        phase += ":indistinguishable-plan";
        return false;
    }
    // Warm the line and translation before allocating the unresolved store.
    // This is a reachability aid; no correctness decision requires a cache hit.
    const auto pdests = unresolved_load_destinations(random, plan.load.fp
        ? memblock::kFpPhysicalRegisters : memblock::kIntegerPhysicalRegisters);
    auto warm = make_load(base, memblock::LoadOp::ld, random() % memblock::kScalarLoadLanes);
    warm.pdest = pdests[0];
    if (!environment.set_rob_head(warm.rob, warm.rob_flag) ||
        !issue_load(warm, std::nullopt, false) ||
        !environment.run_until_queues_retired(timeout)) return false;
    coverage.sample(warm);
    const auto anchor = rob_offset++;
    if (!environment.set_rob_head(memblock::rob_pointer_value(anchor),
            memblock::rob_pointer_flag(anchor))) return false;
    auto store = make_store(base + plan.store.offset, plan.store.data,
        static_cast<memblock::StoreOp>(std::countr_zero(plan.store.width)),
        random() % memblock::kScalarStoreLanes);
    auto load = make_load(base + plan.load.offset,
        static_cast<memblock::LoadOp>(plan.load.op), random() % memblock::kScalarLoadLanes,
        plan.load.fp);
    load.pdest = pdests[1];
    // A legal untrained predictor tuple permits independent younger speculation.
    load.store_set_hit = load.load_wait_bit = load.load_wait_strict = false;
    load.wait_for_rob_flag = false;
    load.wait_for_rob_value = 0;
    UnresolvedLoadObligation obligation(load, plan.conflict);
    const auto violations_before = environment.memory_violation_stats().count;
    const auto sq_before = environment.sq_dequeued();
    environment.expect_store(store);
    environment.expect_load_data(load, initial);
    if (!environment.enqueue_store(store) ||
        !environment.enqueue_load(load) ||
        (plan.data_before_load && !environment.issue_store_data(store, timeout)) ||
        !environment.issue_load(load, timeout) ||
        !environment.run_until_load_complete(load, timeout) ||
        !obligation.provisional() ||
        environment.sq_dequeued() != sq_before ||
        environment.memory_violation_stats().count != violations_before) {
        phase += ":provisional";
        return false;
    }
    if ((!plan.data_before_load && !environment.issue_store_data(store, timeout)) ||
        !environment.run_cycles(plan.resolve_delay)) return false;
    const auto lq_cancel_before = environment.lq_redirect_canceled_observed();
    const auto sq_cancel_before = environment.sq_redirect_canceled_observed();
    // Poll promptly after STA: a long completion wait could miss the backend's
    // registered redirect-return deadline. Store TLB feedback precedes RAW
    // selection; replay only its miss, since the store TLB can still be cold.
    if (!environment.issue_store_address_until_tlb_hit(store, timeout)) return false;
    if (plan.conflict) {
        for (unsigned cycle = 0; cycle < timeout &&
             environment.memory_violation_stats().count == violations_before; ++cycle) {
            if (!environment.run_cycles(1)) return false;
        }
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
        if (canceled > 1 || environment.sq_redirect_canceled_observed() != sq_cancel_before ||
            environment.sq_dequeued() != sq_before || canceled > lq_offset) {
            phase += ":redirect-accounting";
            return false;
        }
        lq_offset -= canceled;
        coverage.scalar_unresolved_lq_canceled += canceled;
        // Flush-itself restores the same architectural ROB identity. Allocate a
        // fresh LQ/scoreboard epoch without advancing the ROB tail. Rename
        // restores the free-list head, so this first refetched load reuses pdest.
        auto replay = load;
        replay.lq = memblock::lq_pointer_value(lq_offset);
        replay.lq_flag = memblock::lq_pointer_flag(lq_offset++);
        replay.pdest = load.pdest;
        environment.expect_load_data(replay, final);
        if (!environment.enqueue_load(replay) || !environment.issue_load(replay, timeout) ||
            !environment.run_until_load_complete(replay, timeout) || !obligation.refetched()) {
            phase += ":refetch";
            return false;
        }
        coverage.sample(replay);
        const unsigned forwarded = std::min(plan.store.offset + plan.store.width,
            plan.load.offset + plan.load.width()) - std::max(plan.store.offset, plan.load.offset);
        coverage.sample_fp_load(replay, final, 3, forwarded, plan.load.width() - forwarded);
    } else {
        if (!environment.run_until_store_complete(timeout) || !environment.run_cycles(32) ||
            environment.memory_violation_stats().count != violations_before ||
            !obligation.no_conflict()) {
            phase += ":nonoverlap-control";
            return false;
        }
    }
    if (!environment.run_until_store_complete(timeout) || environment.sq_dequeued() != sq_before ||
        !environment.commit_store(store, timeout) ||
        !environment.set_rob_head(load.rob, load.rob_flag) ||
        !environment.run_until_queues_retired(timeout) ||
        !environment.pulse_sbuffer_flush() || !environment.run_until_sbuffer_empty(timeout)) {
        phase += ":store-retirement";
        return false;
    }
    auto readback = make_load(base + (plan.store.offset & ~7U), memblock::LoadOp::ld,
        random() % memblock::kScalarLoadLanes);
    readback.pdest = pdests[2];
    if (!environment.set_rob_head(readback.rob, readback.rob_flag) ||
        !issue_load(readback, std::nullopt, false) ||
        !environment.run_until_queues_retired(timeout) || !obligation.readback() ||
        !obligation.done() || environment.memory_violation_stats().count !=
            violations_before + static_cast<unsigned>(plan.conflict)) {
        phase += ":terminal-readback";
        return false;
    }
    coverage.sample(store, true);
    coverage.sample(load);
    coverage.sample_fp_load(load, initial, 3, 0, plan.load.width());
    coverage.sample_scalar_format_choice(3, plan.load.fp);
    coverage.sample(readback);
    ++coverage.scalar_unresolved_windows;
    coverage.scalar_unresolved_conflicts += plan.conflict;
    coverage.scalar_unresolved_controls += !plan.conflict;
    ++coverage.scalar_unresolved_provisional;
    coverage.scalar_unresolved_redirects += plan.conflict;
    coverage.scalar_unresolved_returned += plan.conflict;
    coverage.scalar_unresolved_refetched += plan.conflict;
    ++coverage.scalar_unresolved_readbacks;
    coverage.scalar_unresolved_data_before += plan.data_before_load;
    if (plan.conflict) {
        coverage.scalar_unresolved_overlap_bytes +=
            std::min(plan.store.offset + plan.store.width, plan.load.offset + plan.load.width()) -
            std::max(plan.store.offset, plan.load.offset);
    }
    coverage.cacheable += 4U + static_cast<unsigned>(plan.conflict);
    const auto pages = reference_pages_for(base);
    window.probe_candidate = pages ? pages->physical_address(base) : base;
    ++actions;
    return true;
}

bool MixedScenario::execute_prefetch(ActionWindow &window)
{
    const auto transaction = make_prefetch(
        action_cacheable_address(window, 8),
        static_cast<memblock::PrefetchOp>(8 + random() % 3),
        random() % 3);
    environment.expect_prefetch(transaction);
    if (!environment.enqueue_prefetch(transaction) ||
        !environment.issue_prefetch(transaction, 4096) ||
        !environment.run_until_all_complete(8192)) {
        return false;
    }
    coverage.sample(transaction);
    ++actions;
    ++coverage.cacheable;

    return true;
}

} // namespace memblock::scenarios::mixed
