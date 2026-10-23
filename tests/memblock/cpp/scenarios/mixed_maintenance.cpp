#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

bool MixedScenario::probe_coverage_incomplete()
{
    return constraint_coverage
        .first_missing_probe_cross(constraints).has_value();
}

bool MixedScenario::issue_random_cmo(
    unsigned operation, bool dirty, bool younger_overlap,
    std::optional<bool> error_denied, unsigned probe_depth_class)
{
    const unsigned cmo_completion_timeout =
        std::max(constrained_completion_timeout, 16384U);
    phase = "random-cmo-drain";
    if (!environment.run_until_all_complete(8192) ||
        !environment.run_until_queues_retired(8192) ||
        !environment.run_until_sbuffer_empty(
            cmo_completion_timeout)) {
        return false;
    }

    unsigned locality = constraints.choose_locality(random());
    for (unsigned candidate = 0;
         candidate < constraint_coverage.locality.size(); ++candidate) {
        if (constraints.locality_weights[candidate] != 0 &&
            constraint_coverage.locality[candidate] == 0) {
            locality = candidate;
            break;
        }
    }
    ++constraint_coverage.locality[locality];

    constexpr std::uint64_t lines_per_half_block = 0x100000 / 64;
    const std::uint64_t sequence = cmo_locality_lines[locality]++;
    const std::uint64_t block = locality == 0
        ? 0 : locality == 1 ? 1 : 2 + (sequence / lines_per_half_block) % 6;
    const std::uint64_t line_in_block = locality == 0
        ? sequence % 32
        : locality == 1
            ? sequence % 512
            : sequence % lines_per_half_block;
    const std::uint64_t line = cmo_base + block * 0x200000 +
        line_in_block * 64;
    const std::uint64_t address = line + 8 * (1 + random() % 6);

    const auto warm = make_load(
        address, memblock::LoadOp::ld,
        random() % memblock::kScalarLoadLanes);
    environment.expect_load(warm);
    phase = "random-cmo-warm";
    if (!environment.set_rob_head(warm.rob, warm.rob_flag) ||
        !environment.enqueue_load(warm) ||
        !environment.issue_load(warm, 4096) ||
        !environment.run_until_complete(
            cmo_completion_timeout) ||
        !environment.run_until_lq_retired(8192)) {
        return false;
    }

    if (dirty) {
        phase = "random-cmo-dirty";
        const auto dirty_store = make_store(
            address, random(), memblock::StoreOp::sd,
            random() % memblock::kScalarStoreLanes,
            random() % memblock::kScalarStoreLanes);
        environment.expect_store(dirty_store);
        if (!environment.set_rob_head(
                dirty_store.rob, dirty_store.rob_flag) ||
            !environment.enqueue_store(
                dirty_store, memblock::lq_pointer_value(lq_offset)) ||
            !environment.issue_store_data(dirty_store, 4096) ||
            !environment.issue_store_address_until_tlb_hit(
                dirty_store, cmo_completion_timeout) ||
            !environment.run_until_store_complete(
                cmo_completion_timeout) ||
            !environment.commit_store(dirty_store, 8192) ||
            environment.sbuffer_empty()) {
            return false;
        }
    }

    const unsigned probe_depth = probe_depth_class + 1U;
    std::vector<std::uint64_t> auxiliary_probe_lines;
    if (!error_denied) {
        constexpr std::uint64_t group_bytes = 0x400;
        constexpr std::uint64_t groups =
            cmo_auxiliary_probe_span / group_bytes;
        const std::uint64_t group =
            cmo_auxiliary_probe_group++ % groups;
        const std::uint64_t group_base =
            cmo_auxiliary_probe_base + group * group_bytes;
        const unsigned target_set =
            static_cast<unsigned>((line >> 6) & 0x7fU);
        for (unsigned candidate = 0;
             auxiliary_probe_lines.size() + 1U < probe_depth;
             ++candidate) {
            const std::uint64_t auxiliary_line =
                group_base + candidate * 64U;
            if (((auxiliary_line >> 6) & 0x7fU) == target_set) {
                continue;
            }
            auxiliary_probe_lines.push_back(auxiliary_line);
        }
        for (const std::uint64_t auxiliary_line :
            auxiliary_probe_lines) {
            const auto auxiliary_load = make_load(
                auxiliary_line + 8U * (random() % 8U),
                memblock::LoadOp::ld,
                random() % memblock::kScalarLoadLanes);
            environment.expect_load(auxiliary_load);
            phase = "random-cmo-auxiliary-probe-warm";
            if (!environment.set_rob_head(
                    auxiliary_load.rob, auxiliary_load.rob_flag) ||
                !environment.enqueue_load(auxiliary_load) ||
                !environment.issue_load(auxiliary_load, 4096) ||
                !environment.run_until_complete(
                    cmo_completion_timeout) ||
                !environment.run_until_lq_retired(8192)) {
                return false;
            }
        }
    }

    constexpr std::array<memblock::StoreOp,
        RandomConstraints::cmo_operation_count>
        cmo_ops{{
            memblock::StoreOp::cbo_clean,
            memblock::StoreOp::cbo_flush,
            memblock::StoreOp::cbo_inval,
        }};
    auto cmo_transaction = make_store(
        line + random() % 64, random(), cmo_ops[operation],
        random() % memblock::kScalarStoreLanes,
        random() % memblock::kScalarStoreLanes);
    cmo_transaction.expected_address_writeback_lane = 0;
    cmo_transaction.oracle_address = line;
    cmo_transaction.expected_output_flush_pipe = true;
    cmo_transaction.expected_debug_is_mmio = false;
    cmo_transaction.expected_debug_is_ncio = false;
    if (error_denied) {
        cmo_transaction.expected_exception_mask = *error_denied
            ? memblock::kExceptionStoreAccessFault
            : memblock::kExceptionHardwareError;
    }
    const std::vector<unsigned char> expected_line =
        environment.memory().read_beat(line, 64);
    const bool retain = operation == RandomConstraints::cmo_clean;
    const std::uint8_t probe_cap = retain ? 1U : 2U;
    const std::uint8_t expected_report = dirty
        ? (retain ? 0U : 1U)
        : (retain ? 4U : 2U);
    const std::uint64_t cmo_before =
        environment.dcache_cmo_requests(cmo_transaction.op);
    const std::uint64_t probe_responses_before =
        environment.dcache_probe_responses();
    const std::uint64_t probe_data_before =
        environment.dcache_probe_data();
    const std::uint64_t store_writebacks_before =
        environment.store_writebacks();
    const std::uint64_t error_responses_before =
        environment.dcache_error_response_requests();
    const std::uint64_t denied_beats_before =
        environment.dcache_denied_d_beats();
    const std::uint64_t corrupt_beats_before =
        environment.dcache_corrupt_d_beats();
    const std::uint64_t bus_line_before =
        environment.bus_expected_load(line + 24, memblock::LoadOp::ld);
    const std::uint64_t sq_cancels_before =
        environment.sq_redirect_canceled_observed();
    environment.expect_store(cmo_transaction);
    if (error_denied) {
        environment.inject_dcache_cmo_response_error_at(
            line, *error_denied, !*error_denied);
    }
    phase = "random-cmo-issue";
    if (!environment.enqueue_store(
            cmo_transaction, memblock::lq_pointer_value(lq_offset)) ||
        !environment.issue_store_address_until_tlb_hit(
            cmo_transaction, cmo_completion_timeout) ||
        !environment.issue_store_data(cmo_transaction, 4096)) {
        return false;
    }
    const unsigned cmo_response_delay = younger_overlap
        ? 512U + static_cast<unsigned>(random() % 513U)
        : 128U + static_cast<unsigned>(random() % 897U);
    environment.force_next_cmo_response_delay(cmo_response_delay);
    phase = "random-cmo-probe";
    if (!environment.wait_for_cmo_store_request(
            cmo_transaction, cmo_before + 1, 8192) ||
        environment.dcache_last_cmo_address() != line ||
        environment.dcache_last_cmo_source() !=
            memblock::kDcacheCmoSource) {
        return false;
    }
    if (!error_denied) {
        const std::uint64_t probes_before =
            environment.dcache_probes();
        if (!environment.hold_dcache_c_until_probes(
                probes_before + probe_depth)) {
            return false;
        }
        for (const std::uint64_t auxiliary_line :
            auxiliary_probe_lines) {
            if (!environment.request_dcache_probe(
                    auxiliary_line, 2, false, 2)) {
                return false;
            }
        }
        if (!environment.request_dcache_probe(
                line, probe_cap, false, expected_report,
                dirty ? expected_line : std::vector<unsigned char>{}) ||
            !environment.run_until_probe_responses(
                probe_responses_before + probe_depth,
                cmo_completion_timeout) ||
            environment.dcache_probes() !=
                probes_before + probe_depth ||
            environment.dcache_probe_data() !=
                probe_data_before + (dirty ? 1U : 0U) ||
            environment.dcache_max_probe_outstanding() < probe_depth) {
            return false;
        }
    }
    if (environment.store_writebacks() != store_writebacks_before ||
        environment.pending_scalar_stores() != 1) {
        return false;
    }

    std::optional<memblock::LoadTransaction> younger;
    std::uint64_t younger_writebacks_before = environment.writebacks();
    std::uint64_t younger_cancels_before = 0;
    unsigned younger_response_delay = 0;
    if (younger_overlap) {
        phase = "random-cmo-younger-overlap";
        const std::uint64_t younger_line =
            cmo_base + block * 0x200000 + 0x100000 +
            (cmo_overlap_line++ % lines_per_half_block) * 64;
        younger = make_load(
            younger_line + 8 * (random() % 8),
            memblock::LoadOp::ld,
            random() % memblock::kScalarLoadLanes);
        const std::uint64_t requests_before =
            environment.tilelink_requests();
        younger_writebacks_before = environment.writebacks();
        younger_cancels_before =
            environment.lq_redirect_canceled_observed();
        younger_response_delay = cmo_response_delay + 512U +
            static_cast<unsigned>(random() % 1025U);
        environment.force_next_dcache_response_delay(
            younger_response_delay);
        if (!environment.enqueue_load(*younger) ||
            !environment.issue_load(*younger, 4096) ||
            !environment.run_until_dcache_requests(
                requests_before + 1, 8192) ||
            environment.store_writebacks() != store_writebacks_before ||
            environment.writebacks() != younger_writebacks_before ||
            environment.pending_scalar_stores() != 1) {
            return false;
        }
    }

    phase = "random-cmo-complete";
    const auto fail_cmo_completion = [&](const char *check) {
        std::ostringstream detail;
        detail << "random-cmo-complete-" << check
               << ":op=" << operation
               << ":dirty=" << dirty
               << ":overlap=" << younger_overlap
               << ":error="
               << (error_denied.has_value()
                          ? (*error_denied ? "denied" : "corrupt")
                          : "clean");
        phase = detail.str();
        return false;
    };
    if (!environment.run_until_store_complete(
            cmo_completion_timeout)) {
        return fail_cmo_completion("store-writeback");
    }
    if (younger_overlap || error_denied.has_value()) {
        if (!environment.redirect_after(
                cmo_transaction.rob, cmo_transaction.rob_flag,
                error_denied.has_value())) {
            return fail_cmo_completion("redirect");
        }
        if (environment.writebacks() != younger_writebacks_before) {
            return fail_cmo_completion("younger-writeback-at-redirect");
        }
    }
    if (!error_denied &&
        !environment.commit_store(cmo_transaction, 8192)) {
        return fail_cmo_completion("commit");
    }
    if (!environment.run_cycles(
            younger_overlap ? younger_response_delay + 256U
                : error_denied ? 96U : 16U)) {
        return fail_cmo_completion("settle");
    }
    if (!environment.run_until_dcache_idle(cmo_completion_timeout)) {
        return fail_cmo_completion("manager-drain");
    }
    if (younger_overlap &&
        environment.writebacks() != younger_writebacks_before) {
        return fail_cmo_completion("younger-writeback-after-redirect");
    }
    if (younger_overlap &&
        environment.lq_redirect_canceled_observed() !=
            younger_cancels_before + 1) {
        return fail_cmo_completion("younger-cancel");
    }
    if (!environment.run_until_queues_retired(8192)) {
        return fail_cmo_completion("queue-retire");
    }
    if (environment.dcache_cmo_requests(cmo_transaction.op) !=
        cmo_before + 1) {
        return fail_cmo_completion("request-count");
    }
    if (!environment.sbuffer_empty()) {
        return fail_cmo_completion("sbuffer-empty");
    }
    if (error_denied &&
        (environment.dcache_error_response_requests() !=
                error_responses_before + 1 ||
            environment.dcache_denied_d_beats() !=
                denied_beats_before + (*error_denied ? 1U : 0U) ||
            environment.dcache_corrupt_d_beats() !=
                corrupt_beats_before + (*error_denied ? 0U : 1U) ||
            environment.dcache_probe_responses() !=
                probe_responses_before ||
            environment.bus_expected_load(line + 24, memblock::LoadOp::ld) !=
                bus_line_before)) {
        return false;
    }
    if (error_denied) {
        --rob_offset;
        const std::uint64_t canceled_sq =
            environment.sq_redirect_canceled_observed() -
            sq_cancels_before;
        if (canceled_sq > 1 || canceled_sq > sq_offset) {
            return false;
        }
        sq_offset -= canceled_sq;
    }
    if (younger_overlap) {
        --rob_offset;
        --lq_offset;
    }
    phase = "seeded-mixed-tail";
    return true;
}

} // namespace memblock::scenarios::mixed
