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
        const auto op = static_cast<memblock::LoadOp>(random() % 7);
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
            address, op, random() % 3);
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
        manager_delta[2] != manager_delta[1] ||
        manager_delta[3] != depth) {
        std::ostringstream detail;
        detail << "random-load-merge-manager"
               << ":depth=" << depth
               << ":pattern=" << pattern
               << ":critical_beat=" << critical_beat
               << ":manager=" << manager_delta[0] << ','
               << manager_delta[1] << ',' << manager_delta[2]
               << ',' << manager_delta[3];
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

    const auto op = static_cast<memblock::StoreOp>(random() % 4);
    const unsigned size = 1U << static_cast<unsigned>(op);
    const auto transaction = make_store(
        action_cacheable_address(window, size), random(), op,
        random() % 2, random() % 2);
    if (!issue_store(transaction, (random() & 1U) != 0) ||
        !environment.commit_store(transaction, 8192) ||
        !environment.run_until_queues_retired(8192)) {
        return false;
    }
    probe_candidate = transaction.address & ~std::uint64_t{63};
    ++coverage.cacheable;

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
