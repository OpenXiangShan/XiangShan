#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

bool MixedScenario::execute_hypervisor(ActionWindow &window)
{
    auto &hypervisor_family = window.hypervisor_family;
    auto &hypervisor_spvp_user = window.hypervisor_spvp_user;
    auto &hypervisor_misaligned = window.hypervisor_misaligned;
    auto &hypervisor_pma_device = window.hypervisor_pma_device;
    auto &hypervisor_pmp_relation = window.hypervisor_pmp_relation;
    auto &hypervisor_pbmt_pair = window.hypervisor_pbmt_pair;
    auto &translation = window.translation;
    auto &leaf = window.leaf;
    auto &leaf_addressed = window.leaf_addressed;

    const unsigned spvp_index = hypervisor_spvp_user ? 1U : 0U;
    const auto spvp = hypervisor_spvp_user
        ? memblock::ReferencePrivilegeMode::user
        : memblock::ReferencePrivilegeMode::supervisor;
    const auto pbmt_mapping =
        hypervisor_pbmt_mappings[hypervisor_pbmt_pair];
    const auto pbmt_value = [](bool nc, bool io) {
        return nc ? memblock::ReferencePbmt::nc
            : io ? memblock::ReferencePbmt::io
                  : memblock::ReferencePbmt::pma;
    };
    const auto final_pbmt = memblock::reference_two_stage_pbmt(
        pbmt_value(pbmt_mapping.vs_nc, pbmt_mapping.vs_io),
        pbmt_value(pbmt_mapping.g_nc, pbmt_mapping.g_io));
    const bool final_nc = final_pbmt == memblock::ReferencePbmt::nc;
    const bool final_io = final_pbmt == memblock::ReferencePbmt::io ||
        (final_pbmt == memblock::ReferencePbmt::pma &&
            hypervisor_pma_device);
    const bool physical_pmp_active = hypervisor_pmp_relation !=
        RandomConstraints::hypervisor_pmp_none;
    const bool physical_pmp_denied =
        hypervisor_pmp_relation >=
        RandomConstraints::hypervisor_pmp_below;
    if (((final_nc || final_io) && hypervisor_misaligned) ||
        (hypervisor_pma_device &&
            hypervisor_pbmt_pair != RandomConstraints::
                hypervisor_pbmt_pma_pma) ||
        (physical_pmp_active &&
            (hypervisor_pma_device ||
                hypervisor_pbmt_pair != RandomConstraints::
                    hypervisor_pbmt_pma_pma))) {
        phase += ":hypervisor-address-class";
        return false;
    }
    // A committed store can leave the LSQ/Sbuffer before its DCache miss
    // sends A.  Drain both before taking the exact manager-count baseline;
    // otherwise a legal prior store request would be misattributed to this
    // operation (and backpressured runs make that window observable).
    if (!environment.run_until_all_complete(8192) ||
        !environment.run_until_queues_retired(8192) ||
        !environment.run_until_sbuffer_empty(16384) ||
        !environment.run_until_dcache_idle(16384) ||
        !environment.run_cycles(32) ||
        !environment.run_until_dcache_idle(16384) ||
        !environment.set_hypervisor_access_permissions(
            spvp, false, false, false,
            physical_pmp_active
                ? memblock::ReferencePrivilegeMode::machine
                : memblock::ReferencePrivilegeMode::supervisor)) {
        return false;
    }
    const unsigned topology = leaf.nested_index();
    const std::uint64_t pmp_outer_base =
        hypervisor_pmp_physical_base(spvp_index, topology);
    constexpr std::uint8_t pmp_napot_deny = 0x18;
    constexpr std::uint8_t pmp_napot_rwx = 0x1f;
    if (physical_pmp_active) {
        const auto napot_address = [](
                                       std::uint64_t base,
                                       std::uint64_t size) {
            return (base | (size / 2 - 1)) >> 2;
        };
        if (!environment.configure_pmp(
                {napot_address(
                     pmp_outer_base +
                         hypervisor_pmp_region_offset,
                     hypervisor_pmp_region_size),
                    napot_address(pmp_outer_base, 0x4000),
                    ~std::uint64_t{0}},
                {pmp_napot_rwx, pmp_napot_deny,
                    pmp_napot_rwx})) {
            phase += ":hypervisor-pmp-configuration";
            return false;
        }
    }
    const auto hypervisor_address = [&](unsigned size) {
        leaf_addressed = true;
        return constrained_hypervisor_address(
            size, leaf, spvp_index, hypervisor_pbmt_pair,
            hypervisor_pma_device, hypervisor_misaligned,
            hypervisor_pmp_relation);
    };
    const auto hypervisor_oracle_address = [&](std::uint64_t address) {
        const std::uint64_t virtual_base = physical_pmp_active
            ? hypervisor_pmp_virtual_base(spvp_index, topology)
            : hypervisor_pma_device
                ? hypervisor_pma_device_virtual_base(
                      spvp_index, topology)
                : hypervisor_virtual_base(
                      spvp_index, hypervisor_pbmt_pair, topology);
        const std::uint64_t physical_base = physical_pmp_active
            ? pmp_outer_base
            : hypervisor_pma_device
                ? hypervisor_pma_device_physical_base(
                      spvp_index, topology)
                : hypervisor_physical_base(
                      hypervisor_pbmt_pair, topology);
        return physical_base + (address - virtual_base);
    };
    if (*hypervisor_family == RandomConstraints::hypervisor_hsv) {
        const auto op = hypervisor_misaligned
            ? hypervisor_store_operations[
                  1 + random() %
                      (hypervisor_store_operations.size() - 1)]
            : hypervisor_store_operations[
                  random() % hypervisor_store_operations.size()];
        const unsigned size = memblock::scalar_store_bytes(op);
        const std::uint64_t address = hypervisor_address(size);
        if (((address & (size - 1U)) != 0) !=
            hypervisor_misaligned) {
            phase += ":hypervisor-store-alignment";
            return false;
        }
        const std::uint64_t store_mask = size == 8
            ? ~std::uint64_t{0}
            : (std::uint64_t{1} << (size * 8U)) - 1U;
        const auto hypervisor_store_readback_value =
            [&](const memblock::StoreTransaction &store) {
                return memblock::sign_extend(
                    store.data & store_mask, size * 8U);
            };
        const auto reference = memblock::reference_two_stage_access(
            environment.memory(),
            random_vs_roots[translation.vs_mode],
            random_g_roots[translation.g_mode], address,
            memblock::ReferenceMemoryAccess::store,
            page_mode(translation.vs_mode),
            page_mode(translation.g_mode), spvp);
        if (!reference.translated ||
            reference.physical_address !=
                hypervisor_oracle_address(address)) {
            phase += ":hypervisor-store-reference";
            return false;
        }
        auto transaction = make_store(
            address,
            random(), op, random() % 2);
        transaction.oracle_address = reference.physical_address;
        transaction.cacheable = !(final_nc || final_io);
        transaction.expected_debug_is_mmio = hypervisor_pma_device;
        transaction.expected_debug_is_ncio = false;
        transaction.expected_exception_mask = physical_pmp_denied
            ? memblock::kExceptionStoreAccessFault : 0U;
        if (!physical_pmp_denied && final_io) {
            transaction.expected_address_writeback_lane = 0;
        }
        if (hypervisor_pmp_relation ==
            RandomConstraints::hypervisor_pmp_cross_upper) {
            // The first half is permitted; the second half faults
            // after this cross-page store enters the shared split
            // path, whose exceptional completion uses output 0.
            transaction.expected_address_writeback_lane = 0;
        }
        const bool data_first = (random() & 1U) != 0;
        const std::uint64_t dcache_before =
            environment.tilelink_requests();
        const std::uint64_t uncache_before =
            environment.uncache_requests();
        const std::string operation_phase = phase;
        bool completed_store = false;
        if (physical_pmp_denied) {
            const std::uint64_t lq_dequeued_before =
                environment.lq_dequeued();
            const std::uint64_t sq_dequeued_before =
                environment.sq_dequeued();
            const std::uint64_t lq_cancels_before =
                environment.lq_redirect_canceled_observed();
            const std::uint64_t sq_cancels_before =
                environment.sq_redirect_canceled_observed();
            completed_store = environment.set_rob_head(
                                  transaction.rob,
                                  transaction.rob_flag) &&
                issue_store(
                    transaction, data_first,
                    hypervisor_misaligned) &&
                recover_random_precise_error(
                    transaction.rob, transaction.rob_flag, true,
                    lq_dequeued_before, sq_dequeued_before,
                    lq_cancels_before, sq_cancels_before);
        } else if (final_io) {
            environment.expect_store(transaction);
            completed_store = environment.set_rob_head(
                                  transaction.rob,
                                  transaction.rob_flag) &&
                environment.enqueue_store(
                    transaction,
                    memblock::lq_pointer_value(lq_offset)) &&
                (data_first
                        ? environment.issue_store_data(
                              transaction, 4096) &&
                            environment.
                            issue_store_address_until_tlb_hit(
                                transaction,
                                constrained_completion_timeout)
                        : environment.issue_store_address_until_tlb_hit(
                              transaction,
                              constrained_completion_timeout) &&
                            environment.issue_store_data(
                                transaction, 4096)) &&
                environment.run_cycles(64) &&
                (environment.uncache_requests() != uncache_before ||
                    environment.wait_for_mmio_store_request(
                        transaction.rob, transaction.rob_flag, 8192)) &&
                environment.run_until_store_complete(
                    constrained_completion_timeout) &&
                environment.commit_stores_through(transaction, 1) &&
                environment.run_cycles(16);
            if (completed_store) {
                environment.record_committed_store(transaction);
                coverage.sample(transaction, data_first);
                ++actions;
            }
        } else {
            completed_store =
                issue_store(transaction, data_first) &&
                environment.commit_store(transaction, 8192);
        }
        const std::uint64_t expected_uncache =
            uncache_before +
            (!physical_pmp_denied && (final_nc || final_io)
                    ? 1U : 0U);
        // Attribute DCache traffic by request source: a suppressed store or
        // device access must not create a CPUStoreData acquire.  The whole
        // channel total can legitimately change due to prior committed-store
        // Sbuffer drains or background hardware prefetch.
        const std::uint64_t store_source_requests =
            environment.dcache_requests_from_source_since(
                3, dcache_before);
        const std::uint64_t store_target_requests =
            environment.dcache_requests_from_source_covering_since(
                3,
                transaction.oracle_address.value_or(
                    reference.physical_address) &
                    ~std::uint64_t{63},
                dcache_before);
        if (!completed_store ||
            !environment.run_until_queues_retired(8192) ||
            !environment.run_until_sbuffer_empty(16384) ||
            !environment.run_until_dcache_idle(16384) ||
            environment.uncache_requests() != expected_uncache ||
            ((physical_pmp_denied || final_nc || final_io) &&
                store_target_requests != 0)) {
            std::ostringstream detail;
            detail << phase << ":hypervisor-store-manager"
                   << ":operation_phase={" << operation_phase << '}'
                   << ":family=" << *hypervisor_family
                   << ":op=0x" << std::hex
                   << static_cast<unsigned>(transaction.op)
                   << ":address=0x" << transaction.address
                   << ":pa=0x" << reference.physical_address
                   << std::dec
                   << ":rob=" << unsigned(transaction.rob) << ':'
                   << transaction.rob_flag
                   << ":sq=" << unsigned(transaction.sq) << ':'
                   << transaction.sq_flag
                   << ":pbmt=" << hypervisor_pbmt_pair
                   << ":pmp_relation=" << hypervisor_pmp_relation
                   << ":dcache=" << dcache_before << '/'
                   << environment.tilelink_requests()
                   << ":store_source_requests=" << store_source_requests
                   << ":store_target_requests=" << store_target_requests
                   << ":last_dcache=0x" << std::hex
                   << environment.dcache_last_request_address()
                   << std::dec
                   << ":uncache=" << uncache_before << '/'
                   << environment.uncache_requests()
                   << ":accepted_a={"
                   << environment.dcache_request_summary_since(
                          dcache_before)
                   << '}';
            phase = detail.str();
            return false;
        }
        if (physical_pmp_active) {
            const auto readback_op = static_cast<memblock::LoadOp>(
                static_cast<unsigned>(op));
            const std::uint64_t readback_rob = rob_offset++;
            const std::uint64_t readback_lq = lq_offset++;
            auto readback = memblock::LoadTransaction{
                .address = address,
                .op = readback_op,
                .rob = memblock::rob_pointer_value(readback_rob),
                .rob_flag = memblock::rob_pointer_flag(readback_rob),
                .lq = memblock::lq_pointer_value(readback_lq),
                .lq_flag = memblock::lq_pointer_flag(readback_lq),
                .sq = memblock::sq_pointer_value(sq_offset),
                .sq_flag = memblock::sq_pointer_flag(sq_offset),
                .pdest = static_cast<std::uint8_t>(
                    1U + readback_rob % 255U),
                .lane = static_cast<std::uint8_t>(
                    readback_rob % memblock::kScalarLoadLanes),
            };
            readback.oracle_address = reference.physical_address;
            readback.expected_debug_is_mmio = false;
            readback.expected_debug_is_ncio = false;
            readback.expected_debug_is_perf_cnt = false;
            const std::optional<std::uint64_t> expected_readback =
                !physical_pmp_denied && !hypervisor_misaligned
                ? std::optional<std::uint64_t>{
                      hypervisor_store_readback_value(transaction)}
                : std::nullopt;
            if (!environment.configure_pmp(
                    {~std::uint64_t{0}}, {pmp_napot_rwx}) ||
                !issue_load(readback, expected_readback, false) ||
                !environment.run_until_queues_retired(8192)) {
                phase += ":hypervisor-pmp-store-readback";
                return false;
            }
        }
        if (!physical_pmp_denied && (final_nc || final_io)) {
            const std::array<memblock::LoadOp, 4> read_ops{{
                memblock::LoadOp::lbu, memblock::LoadOp::lhu,
                memblock::LoadOp::lwu, memblock::LoadOp::ld,
            }};
            const std::uint64_t mask = size == 8
                ? ~std::uint64_t{0}
                : (std::uint64_t{1} << (size * 8)) - 1;
            if (environment.bus_expected_load(
                    reference.physical_address,
                    read_ops[static_cast<unsigned>(op) & 3U]) !=
                (transaction.data & mask)) {
                phase += ":hypervisor-store-physical-image";
                return false;
            }
        }
        if (!physical_pmp_active && !physical_pmp_denied &&
            !final_nc && !final_io && !hypervisor_misaligned) {
            phase += ":hypervisor-cacheable-store-readback";
            const std::uint64_t readback_rob = rob_offset++;
            const std::uint64_t readback_lq = lq_offset++;
            memblock::LoadTransaction readback{
                .address = address,
                .oracle_address = reference.physical_address,
                .op = static_cast<memblock::LoadOp>(
                    static_cast<unsigned>(op)),
                .rob = memblock::rob_pointer_value(readback_rob),
                .rob_flag = memblock::rob_pointer_flag(readback_rob),
                .lq = memblock::lq_pointer_value(readback_lq),
                .lq_flag = memblock::lq_pointer_flag(readback_lq),
                .sq = memblock::sq_pointer_value(sq_offset),
                .sq_flag = memblock::sq_pointer_flag(sq_offset),
                .pdest = static_cast<std::uint8_t>(
                    1U + static_cast<unsigned>(readback_rob % 255U)),
                .lane = static_cast<unsigned>(
                    readback_rob % memblock::kScalarLoadLanes),
                .expected_debug_is_mmio = false,
                .expected_debug_is_ncio = false,
                .expected_debug_is_perf_cnt = false,
            };
            if (!issue_load(
                    readback,
                    hypervisor_store_readback_value(transaction),
                    false) ||
                !environment.run_until_queues_retired(8192)) {
                phase += ":hypervisor-cacheable-store-readback-failed";
                return false;
            }
        }
    } else {
        memblock::LoadOp op{};
        if (*hypervisor_family ==
            RandomConstraints::hypervisor_hlvx) {
            op = hypervisor_execute_operations[
                random() % hypervisor_execute_operations.size()];
        } else if (hypervisor_misaligned) {
            op = hypervisor_misaligned_load_operations[
                random() %
                hypervisor_misaligned_load_operations.size()];
        } else {
            op = hypervisor_load_operations[
                random() % hypervisor_load_operations.size()];
        }
        const unsigned size =
            1U << (static_cast<unsigned>(op) & 3U);
        const std::uint64_t address = hypervisor_address(size);
        if (((address & (size - 1U)) != 0) !=
            hypervisor_misaligned) {
            phase += ":hypervisor-load-alignment";
            return false;
        }
        const auto reference = memblock::reference_two_stage_access(
            environment.memory(),
            random_vs_roots[translation.vs_mode],
            random_g_roots[translation.g_mode], address,
            memblock::ReferenceMemoryAccess::load,
            page_mode(translation.vs_mode),
            page_mode(translation.g_mode),
            spvp, false, false,
            *hypervisor_family ==
                RandomConstraints::hypervisor_hlvx);
        if (!reference.translated ||
            reference.physical_address !=
                hypervisor_oracle_address(address)) {
            std::ostringstream detail;
            detail << phase << ":hypervisor-reference"
                   << ":family=" << *hypervisor_family
                   << ":op=0x" << std::hex
                   << static_cast<unsigned>(op)
                   << ":address=0x" << address
                   << ":pa=0x" << reference.physical_address
                   << std::dec
                   << ":stage1_pf="
                   << reference.stage1_page_fault
                   << ":gpf=" << reference.guest_page_fault
                   << ":af=" << reference.access_fault;
            phase = detail.str();
            return false;
        }
        {
            std::ostringstream detail;
            detail << phase << ":family=" << *hypervisor_family
                   << ":op=0x" << std::hex
                   << static_cast<unsigned>(op)
                   << ":address=0x" << address << std::dec;
            phase = detail.str();
        }
        auto transaction = make_load(
            address, op, random() % 3);
        transaction.oracle_address = reference.physical_address;
        const bool physical_pma_execute_denied =
            hypervisor_pma_device &&
            *hypervisor_family ==
                RandomConstraints::hypervisor_hlvx;
        transaction.expected_exception_mask =
            physical_pma_execute_denied || physical_pmp_denied
            ? memblock::kExceptionLoadAccessFault : 0U;
        transaction.expected_debug_is_mmio =
            physical_pma_execute_denied || physical_pmp_denied
            ? std::nullopt : std::optional<bool>{final_io};
        transaction.expected_debug_is_ncio =
            final_io && !physical_pma_execute_denied
            ? std::optional<bool>{false} : std::nullopt;
        transaction.expected_debug_is_perf_cnt = false;
        const std::uint64_t dcache_before =
            environment.tilelink_requests();
        const std::uint64_t uncache_before =
            environment.uncache_requests();
        const std::string operation_phase = phase;
        bool completed_load = false;
        if (physical_pmp_denied || physical_pma_execute_denied) {
            const std::uint64_t lq_dequeued_before =
                environment.lq_dequeued();
            const std::uint64_t sq_dequeued_before =
                environment.sq_dequeued();
            const std::uint64_t lq_cancels_before =
                environment.lq_redirect_canceled_observed();
            const std::uint64_t sq_cancels_before =
                environment.sq_redirect_canceled_observed();
            completed_load = environment.set_rob_head(
                                 transaction.rob,
                                 transaction.rob_flag) &&
                issue_load(transaction) &&
                recover_random_precise_error(
                    transaction.rob, transaction.rob_flag, false,
                    lq_dequeued_before, sq_dequeued_before,
                    lq_cancels_before, sq_cancels_before);
        } else if (final_io) {
            environment.expect_load(transaction);
            completed_load = environment.set_rob_head(
                                 transaction.rob,
                                 transaction.rob_flag) &&
                environment.enqueue_load(transaction) &&
                environment.issue_load(transaction, 4096) &&
                environment.wait_for_mmio_request(
                    transaction.rob, transaction.rob_flag, 8192) &&
                environment.run_until_complete(
                    constrained_completion_timeout) &&
                environment.run_until_lq_retired(8192);
            if (completed_load) {
                coverage.sample(transaction);
                ++actions;
            }
        } else {
            completed_load = issue_load(transaction) &&
                environment.run_until_queues_retired(8192);
        }
        const std::uint64_t expected_uncache =
            uncache_before +
            (!physical_pma_execute_denied && !physical_pmp_denied &&
                        (final_nc || final_io)
                    ? 1U : 0U);
        const std::uint64_t target_line =
            reference.physical_address & ~std::uint64_t{63};
        // Other operations may still be retiring through the
        // manager after architectural completion.  Attribute only
        // demand-load requests, and require any permitted PMP
        // prefix request to cover this load's translated line.
        // Unrelated store/prefetch traffic is not part of this
        // operation's no-side-effect contract.
        const std::uint64_t demand_requests =
            environment.dcache_requests_from_source_since(
                2, dcache_before);
        const std::uint64_t demand_target_requests =
            environment.dcache_requests_from_source_covering_since(
                2, target_line, dcache_before);
        const bool pmp_prefix_request_allowed =
            physical_pmp_denied &&
            hypervisor_pmp_relation == RandomConstraints::
                hypervisor_pmp_cross_upper;
        const char *suppression_error =
            memblock::dcache_suppressed_demand_error(
                physical_pma_execute_denied || physical_pmp_denied ||
                    final_nc || final_io,
                pmp_prefix_request_allowed, demand_requests,
                demand_target_requests);
        if (!completed_load ||
            environment.uncache_requests() != expected_uncache ||
            suppression_error != nullptr) {
            std::ostringstream detail;
            detail << phase << ":hypervisor-load-manager"
                   << ":operation_phase={" << operation_phase << '}'
                   << ":family=" << *hypervisor_family
                   << ":op=0x" << std::hex
                   << static_cast<unsigned>(transaction.op)
                   << ":address=0x" << transaction.address
                   << ":pa=0x" << reference.physical_address
                   << std::dec
                   << ":rob=" << unsigned(transaction.rob) << ':'
                   << transaction.rob_flag
                   << ":lq=" << unsigned(transaction.lq) << ':'
                   << transaction.lq_flag
                   << ":pbmt=" << hypervisor_pbmt_pair
                   << ":pmp_relation="
                   << hypervisor_pmp_relation
                   << ":dcache=" << dcache_before << '/'
                   << environment.tilelink_requests()
                   << ":demand=" << demand_requests
                   << ":target_demand=" << demand_target_requests
                   << ":target_line=0x" << std::hex << target_line
                   << std::dec
                   << ":suppression="
                   << (suppression_error == nullptr
                              ? "valid" : suppression_error)
                   << ":last_dcache=0x" << std::hex
                   << environment.dcache_last_request_address()
                   << std::dec
                   << ":uncache=" << uncache_before << '/'
                   << environment.uncache_requests()
                   << ":accepted_a={"
                   << environment.dcache_request_summary_since(
                          dcache_before)
                   << '}';
            phase = detail.str();
            return false;
        }
        // Early wakeup/cancel pulse counts are diagnostic. Exact
        // fault completion, RF suppression, manager restrictions,
        // and identity-specific queue recovery above decide PASS.
    }
    if (!activate_translation_context(translation)) {
        return false;
    }
    ++constraint_coverage.hypervisor_families[*hypervisor_family];
    ++constraint_coverage.hypervisor_spvp[spvp_index];
    ++constraint_coverage.hypervisor_crosses[
        *hypervisor_family][spvp_index];
    const unsigned alignment_index =
        hypervisor_misaligned ? 1U : 0U;
    ++constraint_coverage.hypervisor_alignments[alignment_index];
    ++constraint_coverage.hypervisor_alignment_crosses[
        *hypervisor_family][spvp_index][alignment_index];
    ++constraint_coverage.hypervisor_pbmt_pairs[
        hypervisor_pbmt_pair];
    ++constraint_coverage.hypervisor_pbmt_crosses[
        *hypervisor_family][spvp_index][hypervisor_pbmt_pair];
    const unsigned pma_device_index =
        hypervisor_pma_device ? 1U : 0U;
    ++constraint_coverage.hypervisor_pma_devices[pma_device_index];
    ++constraint_coverage.hypervisor_pma_device_crosses[
        *hypervisor_family][spvp_index][pma_device_index];
    ++constraint_coverage.hypervisor_pmp_relations[
        hypervisor_pmp_relation];
    ++constraint_coverage.hypervisor_pmp_relation_crosses[
        *hypervisor_family][spvp_index][hypervisor_pmp_relation];
    if (final_nc || final_io) {
        ++coverage.noncacheable;
    } else {
        ++coverage.cacheable;
    }

    return true;
}

} // namespace memblock::scenarios::mixed
