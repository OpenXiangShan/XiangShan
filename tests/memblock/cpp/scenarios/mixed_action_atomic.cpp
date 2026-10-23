#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

bool MixedScenario::execute_atomic(ActionWindow &window)
{
    auto &atomic_family = window.atomic_family;
    auto &atomic_width = window.atomic_width;
    auto &atomic_probe_depth = window.atomic_probe_depth;
    auto &atomic_error_denied = window.atomic_error_denied;
    auto &ordinary_leaf_addressed = window.ordinary_leaf_addressed;

    ordinary_leaf_addressed = true;
    const std::size_t slot = random() % atomic_values.size();
    const bool word = atomic_width == 0;
    const std::uint64_t operand = random();
    const bool error_response = atomic_error_denied.has_value();
    const bool probe_composed =
        !error_response && atomic_probe_depth != 0;
    std::uint64_t address = atomic_base + slot * 64;
    if (error_response) {
        constexpr std::uint64_t error_lines =
            (atomic_error_span / 2) / 64;
        if (atomic_error_line >= error_lines) {
            phase = "random-atomic-error-address-space-exhausted";
            return false;
        }
        const std::uint64_t sequence = atomic_error_line++;
        const std::uint64_t line = sequence ^ (sequence >> 1);
        address = atomic_error_base + line * 64 + (word ? 4 : 0);
    } else if (probe_composed) {
        constexpr std::uint64_t target_lines =
            atomic_probe_target_span / 64;
        std::optional<std::uint64_t> fresh_target_line;
        for (std::uint64_t attempt = 0;
             attempt < target_lines && !fresh_target_line;
             ++attempt) {
            const std::uint64_t sequence =
                atomic_probe_target_line++;
            const std::uint64_t line =
                (sequence ^ (sequence >> 1)) % target_lines;
            const std::uint64_t candidate =
                atomic_probe_target_base + line * 64;
            if (environment.dcache_requests_covering_since(
                    candidate, 0) == 0) {
                fresh_target_line = candidate;
            }
        }
        if (!fresh_target_line) {
            phase = "random-atomic-probe-address-space-exhausted";
            return false;
        }
        address = *fresh_target_line;
    }
    const memblock::LoadOp read_op = word
        ? memblock::LoadOp::lwu : memblock::LoadOp::ld;
    std::uint64_t atomic_value = error_response
        ? environment.memory().expected_load(address, read_op)
        : probe_composed
            ? environment.memory().expected_load(
                  address, memblock::LoadOp::ld)
            : atomic_values[slot];
    const std::uint64_t original_value = word
        ? static_cast<std::uint32_t>(atomic_value) : atomic_value;
    const auto expect_atomic_writeback = [&] (
                                             const auto &transaction, std::uint64_t value,
                                             std::uint32_t exception = 0) {
        const memblock::LoadTransaction writeback{
            .address = transaction.address,
            .op = read_op,
            .rob = transaction.rob,
            .rob_flag = transaction.rob_flag,
            .pdest = transaction.pdest,
            .lane = 0,
            .expected_exception_mask = exception,
            .rf_wen = exception == 0,
        };
        environment.expect_load_data(writeback, value);
    };
    memblock::AtomicTransaction transaction;
    if (atomic_family == RandomConstraints::atomic_amo) {
        const auto &operations =
            word ? atomic_operations_w : atomic_operations_d;
        const auto op = operations[random() % operations.size()];
        transaction = make_atomic(address, op, operand);
    } else if (atomic_family == RandomConstraints::atomic_cas) {
        transaction = make_atomic(
            address,
            word ? memblock::AtomicOp::amocas_w
                 : memblock::AtomicOp::amocas_d,
            operand);
        const bool compare_match = (random() & 1U) != 0;
        transaction.compare = compare_match
            ? original_value : original_value ^ 1U;
    } else {
        transaction = make_atomic(
            address,
            word ? memblock::AtomicOp::lr_w
                 : memblock::AtomicOp::lr_d,
            0);
    }

    std::vector<std::uint64_t> atomic_probe_lines;
    if (probe_composed) {
        phase = "random-atomic-probe-drain";
        if (!environment.run_until_all_complete(
                constrained_completion_timeout) ||
            !environment.run_until_queues_retired(
                constrained_completion_timeout) ||
            !environment.run_until_sbuffer_empty(
                constrained_completion_timeout) ||
            !environment.run_until_dcache_idle(
                constrained_completion_timeout)) {
            return false;
        }
        constexpr std::uint64_t group_bytes = 0x800;
        constexpr std::uint64_t groups =
            atomic_auxiliary_probe_span / group_bytes;
        const std::uint64_t group =
            atomic_auxiliary_probe_group++ % groups;
        const std::uint64_t group_base =
            atomic_auxiliary_probe_base + group * group_bytes;
        const unsigned target_set =
            static_cast<unsigned>((address >> 6) & 0x7fU);
        for (unsigned candidate = 0;
             atomic_probe_lines.size() < atomic_probe_depth;
             ++candidate) {
            const std::uint64_t auxiliary_line =
                group_base + candidate * 64U;
            if (((auxiliary_line >> 6) & 0x7fU) == target_set) {
                continue;
            }
            atomic_probe_lines.push_back(auxiliary_line);
        }
        for (const std::uint64_t auxiliary_line :
            atomic_probe_lines) {
            const auto auxiliary_load = make_load(
                auxiliary_line + 8U * (random() % 8U),
                memblock::LoadOp::ld,
                random() % memblock::kScalarLoadLanes);
            environment.expect_load(auxiliary_load);
            phase = "random-atomic-probe-warm";
            if (!environment.set_rob_head(
                    auxiliary_load.rob, auxiliary_load.rob_flag) ||
                !environment.enqueue_load(auxiliary_load) ||
                !environment.issue_load(auxiliary_load, 4096) ||
                !environment.run_until_complete(
                    constrained_completion_timeout) ||
                !environment.run_until_lq_retired(8192)) {
                return false;
            }
        }
    }

    const auto issue_atomic_success = [&] (
                                          const memblock::AtomicTransaction &current) {
        if (!probe_composed) {
            return environment.issue_atomic(current, 4096) &&
                environment.run_until_complete(
                    constrained_completion_timeout);
        }
        const std::uint64_t target_line =
            address & ~std::uint64_t{63};
        const std::uint64_t target_requests_before =
            environment.tilelink_requests();
        const std::uint64_t probes_before =
            environment.dcache_probes();
        const std::uint64_t probe_responses_before =
            environment.dcache_probe_responses();
        const std::uint64_t probe_data_before =
            environment.dcache_probe_data();
        const std::uint64_t writebacks_before =
            environment.writebacks();
        environment.hold_dcache_response_at(target_line);
        phase = "random-atomic-probe-issue";
        if (!environment.issue_atomic(current, 4096)) {
            phase += ":issue";
            return false;
        }
        for (unsigned cycle = 0;
             cycle < constrained_completion_timeout &&
             !environment.dcache_response_held_at(target_line);
             ++cycle) {
            if (!environment.run_cycles(1)) {
                phase += ":wait-held";
                return false;
            }
        }
        if (!environment.dcache_response_held_at(target_line)) {
            phase += ":held-timeout";
            return false;
        }
        if (environment.dcache_requests_covering_since(
                target_line, target_requests_before) != 1) {
            phase += ":target-request-count";
            return false;
        }
        if (environment.writebacks() != writebacks_before) {
            phase += ":early-writeback";
            return false;
        }
        if (!environment.hold_dcache_c_until_probes(
                probes_before + atomic_probe_depth)) {
            phase += ":hold-c";
            return false;
        }
        for (const std::uint64_t auxiliary_line :
            atomic_probe_lines) {
            if (!environment.request_dcache_probe(
                    auxiliary_line, 2, false, 2)) {
                phase += ":probe-request";
                return false;
            }
        }
        if (environment.dcache_probe_data() != probe_data_before) {
            phase += ":early-probe-data";
            return false;
        }
        if (environment.writebacks() != writebacks_before) {
            phase += ":writeback-before-d";
            return false;
        }
        if (!environment.dcache_response_held_at(target_line)) {
            phase += ":lost-held-d";
            return false;
        }
        if (!environment.release_held_dcache_response_at(
                target_line)) {
            phase += ":release-d";
            return false;
        }
        if (!environment.run_until_probe_responses(
                probe_responses_before + atomic_probe_depth,
                constrained_completion_timeout)) {
            phase += ":probe-responses";
            return false;
        }
        if (environment.dcache_probes() !=
            probes_before + atomic_probe_depth) {
            phase += ":probe-count";
            return false;
        }
        if (environment.dcache_probe_data() != probe_data_before) {
            phase += ":probe-data";
            return false;
        }
        if (environment.dcache_max_probe_outstanding() <
            atomic_probe_depth) {
            phase += ":probe-depth";
            return false;
        }
        if (!environment.run_until_complete(
                constrained_completion_timeout)) {
            phase += ":atomic-completion";
            return false;
        }
        if (environment.dcache_response_held_at(target_line)) {
            phase += ":held-d-not-drained";
            return false;
        }
        return true;
    };

    std::array<std::uint64_t, 5> manager_delta{};
    if (error_response) {
        phase = *atomic_error_denied
            ? "random-atomic-denied"
            : "random-atomic-corrupt";
        if (!environment.run_until_all_complete(
                constrained_completion_timeout) ||
            !environment.run_until_queues_retired(
                constrained_completion_timeout) ||
            !environment.run_until_dcache_idle(
                constrained_completion_timeout)) {
            return false;
        }
        const std::uint32_t expected_exception =
            !*atomic_error_denied
            ? memblock::kExceptionHardwareError
            : atomic_family == RandomConstraints::atomic_lrsc
                ? memblock::kExceptionLoadAccessFault
                : memblock::kExceptionStoreAccessFault;
        expect_atomic_writeback(
            transaction, 0, expected_exception);
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
            environment.bus_expected_load(address, read_op);
        environment.inject_dcache_response_error_at(
            address & ~std::uint64_t{63},
            *atomic_error_denied, !*atomic_error_denied);
        if (!environment.set_rob_head(
                transaction.rob, transaction.rob_flag) ||
            !environment.issue_atomic(transaction, 4096) ||
            !environment.run_until_complete(
                constrained_completion_timeout) ||
            !environment.run_until_dcache_idle(
                constrained_completion_timeout)) {
            return false;
        }
        manager_delta = {{
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
                                 1, *atomic_error_denied ? 2U : 0U,
                                 2, 1, 1} ||
            (environment.dcache_last_error_response_address() &
                ~std::uint64_t{63}) !=
                (address & ~std::uint64_t{63}) ||
            environment.dcache_last_error_response_keyword() ||
            environment.bus_expected_load(address, read_op) !=
                bus_data_before) {
            phase += ":oracle";
            return false;
        }
        if (!environment.run_cycles(8) ||
            !environment.redirect_after(
                transaction.rob, transaction.rob_flag, true) ||
            !environment.run_cycles(16)) {
            phase += ":trap-recovery";
            return false;
        }
        --rob_offset;
        // The design intentionally installs an errored refill as a
        // poisoned line. AMO/CAS still update that private cache
        // image, while the manager backing store stays unchanged
        // until a possible dirty eviction. Track the expected
        // ReleaseData without treating exceptional data as an ISA
        // result.
        bool cache_image_updated = false;
        std::uint64_t cache_image_value = original_value;
        if (atomic_family == RandomConstraints::atomic_amo) {
            cache_image_value = atomic_result(
                transaction.op, original_value, operand);
            cache_image_updated = true;
        } else if (
            atomic_family == RandomConstraints::atomic_cas &&
            transaction.compare == original_value) {
            cache_image_value = operand;
            cache_image_updated = true;
        }
        if (cache_image_updated) {
            const unsigned bytes = word ? 4U : 8U;
            for (unsigned byte = 0; byte < bytes; ++byte) {
                environment.memory().write_reference_byte(
                    address + byte,
                    static_cast<std::uint8_t>(
                        cache_image_value >> (8 * byte)));
            }
        }
        ++coverage.exceptions;
    } else if (atomic_family == RandomConstraints::atomic_amo) {
        expect_atomic_writeback(
            transaction,
            atomic_writeback(atomic_value, word));
        if (!issue_atomic_success(transaction)) {
            return false;
        }
        atomic_value = atomic_result(
            transaction.op, atomic_value, operand);
    } else if (atomic_family == RandomConstraints::atomic_cas) {
        const std::uint64_t old_value = atomic_value;
        const std::uint64_t compare_value = word
            ? static_cast<std::uint32_t>(old_value) : old_value;
        expect_atomic_writeback(
            transaction, atomic_writeback(old_value, word));
        if (!issue_atomic_success(transaction)) {
            return false;
        }
        if (transaction.compare == compare_value) {
            atomic_value = word
                ? ((old_value & ~std::uint64_t{0xffffffff}) |
                      static_cast<std::uint32_t>(operand))
                : operand;
        }
    } else {
        expect_atomic_writeback(
            transaction,
            atomic_writeback(atomic_value, word));
        if (!issue_atomic_success(transaction)) {
            return false;
        }
        const auto sc = make_atomic(
            address,
            word ? memblock::AtomicOp::sc_w
                 : memblock::AtomicOp::sc_d,
            operand);
        expect_atomic_writeback(sc, 0);
        if (!environment.issue_atomic(sc, 4096) ||
            !environment.run_until_complete(
                constrained_completion_timeout)) {
            return false;
        }
        atomic_value = word
            ? ((atomic_value & ~std::uint64_t{0xffffffff}) |
                  static_cast<std::uint32_t>(operand))
            : operand;
    }
    if (!error_response) {
        if (!probe_composed) {
            atomic_values[slot] = atomic_value;
        }
        environment.record_atomic_result(address, atomic_value);
        constraint_coverage.sample_atomic_probe(
            atomic_family, atomic_width, atomic_probe_depth);
    }
    constraint_coverage.sample_atomic(
        atomic_family, atomic_width, atomic_error_denied,
        manager_delta);
    ++actions;
    ++coverage.cacheable;

    return true;
}

bool MixedScenario::execute_cmo(ActionWindow &window)
{
    auto &cmo_operation = window.cmo_operation;
    auto &cmo_dirty = window.cmo_dirty;
    auto &cmo_younger_overlap = window.cmo_younger_overlap;
    auto &cmo_probe_depth_class = window.cmo_probe_depth_class;
    auto &cmo_error_denied = window.cmo_error_denied;
    auto &ordinary_leaf_addressed = window.ordinary_leaf_addressed;

    ordinary_leaf_addressed = true;
    if (!issue_random_cmo(
            cmo_operation, cmo_dirty, cmo_younger_overlap,
            cmo_error_denied, cmo_probe_depth_class)) {
        return false;
    }
    ++constraint_coverage.cmo_operations[cmo_operation];
    ++constraint_coverage.cmo_line_states[cmo_dirty ? 1 : 0];
    ++constraint_coverage.cmo_younger_overlaps[
        cmo_younger_overlap ? 1 : 0];
    ++constraint_coverage.cmo_errors[
        cmo_error_denied ? 1 : 0];
    if (cmo_error_denied) {
        const unsigned error_kind = *cmo_error_denied ? 1U : 0U;
        ++constraint_coverage.cmo_error_kinds[error_kind];
        ++constraint_coverage.cmo_operation_errors[
            cmo_operation][error_kind];
    } else {
        constraint_coverage.sample_cmo_probe(
            cmo_operation, cmo_dirty, cmo_probe_depth_class);
    }
    ++actions;
    ++coverage.cacheable;

    return true;
}

} // namespace memblock::scenarios::mixed
