#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

bool MixedScenario::recover_random_precise_error(
    std::uint8_t rob, bool rob_flag, bool store,
    std::uint64_t lq_dequeued_before,
    std::uint64_t sq_dequeued_before,
    std::uint64_t lq_cancels_before,
    std::uint64_t sq_cancels_before)
{
    const char *const redirect_phase = store
        ? "random-precise-store-redirect"
        : "random-precise-load-redirect";
    phase = redirect_phase;
    if (!environment.run_cycles(8)) {
        phase += ":pre-redirect-delay";
        return false;
    }
    if (!environment.redirect_after(rob, rob_flag, true)) {
        phase += ":redirect";
        return false;
    }
    if (!environment.run_cycles(96)) {
        phase += ":post-redirect-delay";
        return false;
    }
    if (!environment.run_until_queues_retired(8192)) {
        phase += ":queue-retire";
        return false;
    }
    const std::uint64_t lq_dequeued =
        environment.lq_dequeued() - lq_dequeued_before;
    const std::uint64_t sq_dequeued =
        environment.sq_dequeued() - sq_dequeued_before;
    const std::uint64_t lq_canceled =
        environment.lq_redirect_canceled_observed() -
        lq_cancels_before;
    const std::uint64_t sq_canceled =
        environment.sq_redirect_canceled_observed() -
        sq_cancels_before;
    if (lq_dequeued + lq_canceled != (store ? 0U : 1U) ||
        sq_dequeued + sq_canceled != (store ? 1U : 0U)) {
        std::ostringstream detail;
        detail << redirect_phase << ":queue-accounting:lq="
               << lq_dequeued << '+' << lq_canceled << ":sq="
               << sq_dequeued << '+' << sq_canceled;
        phase = detail.str();
        return false;
    }
    --rob_offset;
    if (store) {
        sq_offset -= sq_canceled;
    } else {
        lq_offset -= lq_canceled;
    }
    ++coverage.exceptions;
    return true;
}

bool MixedScenario::issue_canonical_boundary(unsigned cross)
{
    const unsigned eew = cross % 3U + 1U;
    const unsigned width = 1U << eew;
    const bool store = (cross / 3U) % 2U != 0;
    const unsigned mode = (cross / 6U) % 2U;
    const bool invalid_first = cross / 12U != 0;
    const unsigned bits = mode == 0 ? 39U : 48U;
    const std::uint64_t low_limit = std::uint64_t{1} << (bits - 1);
    const std::uint64_t high_base = 0U - low_limit;
    const unsigned distance = 1U + random() % (width - 1U);
    const std::uint64_t address =
        (invalid_first ? high_base : low_limit) - distance;
    const std::uint64_t expected_fault = invalid_first ? address : low_limit;
    {
        std::ostringstream detail;
        detail << "random-canonical-boundary:cross=" << cross
               << ":mode=" << bits << ":store=" << store
               << ":invalid_first=" << invalid_first
               << ":width=" << width << ":distance=" << distance
               << ":address=0x" << std::hex << address;
        phase = detail.str();
    }
    const std::string canonical_phase = phase;
    TranslationContext context;
    context.regime = RandomConstraints::translation_stage1;
    context.stage1_mode = mode;
    if (!environment.run_until_all_complete(constrained_completion_timeout) ||
        !environment.run_until_queues_retired(constrained_completion_timeout) ||
        !enter_translation_context(context) ||
        !environment.set_pointer_masking(memblock::PointerMaskingConfig{}) ||
        !issue_constrained_fence(store ? RandomConstraints::scalar_store
                                       : RandomConstraints::scalar_load)) {
        return false;
    }
    std::optional<std::uint64_t> first_invalid;
    unsigned valid_bytes = 0;
    for (unsigned byte = 0; byte < width; ++byte) {
        const auto walk = memblock::reference_page_walk(
            environment.memory(), random_stage1_roots[mode],
            address + byte, page_mode(mode));
        if (walk.translated) {
            ++valid_bytes;
        } else if (!first_invalid) {
            first_invalid = address + byte;
        }
    }
    if (first_invalid != expected_fault || valid_bytes == 0 ||
        valid_bytes == width) {
        phase += ":reference-range";
        return false;
    }
    std::array<std::uint64_t, 2> control_virtual{{low_limit - 8U, high_base}};
    std::array<std::uint64_t, 2> control_physical{{
        canonical_boundary_physical + mode * 0x2000U + 0xff8U,
        canonical_boundary_physical + mode * 0x2000U + 0x1000U}};
    std::array<std::uint64_t, 2> control_data{};
    for (unsigned side = 0; side < 2; ++side) {
        control_data[side] = environment.bus_expected_load(
            control_physical[side], memblock::LoadOp::ld);
    }
    const auto check_controls = [&]() {
        for (unsigned side = 0; side < 2; ++side) {
            auto control = make_load(
                control_virtual[side], memblock::LoadOp::ld,
                random() % memblock::kScalarLoadLanes);
            control.oracle_address = control_physical[side];
            const auto walk = memblock::reference_page_walk(
                environment.memory(), random_stage1_roots[mode],
                control.address, page_mode(mode));
            if (!walk.translated ||
                walk.physical_address != control_physical[side] ||
                environment.bus_expected_load(
                    control_physical[side], memblock::LoadOp::ld) !=
                    control_data[side] ||
                !environment.set_rob_head(control.rob, control.rob_flag) ||
                !issue_load(control, control_data[side], false) ||
                !environment.run_until_lq_retired(constrained_completion_timeout)) {
                return false;
            }
            ++canonical_boundary_controls;
        }
        return true;
    };
    // Positive controls prove both canonical aliases before checking
    // the terminal fault; their results also define the store pre-state.
    if (!check_controls()) {
        phase += ":positive-control";
        return false;
    }
    environment.select_store_exception_address(store);
    const auto ptw_before = environment.ptw_requests();
    const auto uncache_before = environment.uncache_requests();
    const auto lq_before = environment.lq_dequeued();
    const auto sq_before = environment.sq_dequeued();
    const auto lq_cancels = environment.lq_redirect_canceled_observed();
    const auto sq_cancels = environment.sq_redirect_canceled_observed();
    std::uint8_t fault_rob = 0;
    bool fault_rob_flag = false;
    bool completed_fault = false;
    if (store) {
        auto fault = make_store(
            address, random(), static_cast<memblock::StoreOp>(eew),
            random() % memblock::kScalarStoreLanes,
            random() % memblock::kScalarStoreLanes);
        fault.expected_exception_mask = memblock::kExceptionStorePageFault;
        fault_rob = fault.rob;
        fault_rob_flag = fault.rob_flag;
        completed_fault = environment.set_rob_head(fault.rob, fault.rob_flag) &&
            issue_store(fault, (random() & 1U) != 0, true);
    } else {
        auto fault = make_load(
            address, static_cast<memblock::LoadOp>(eew),
            random() % memblock::kScalarLoadLanes);
        fault.expected_exception_mask = memblock::kExceptionLoadPageFault;
        fault_rob = fault.rob;
        fault_rob_flag = fault.rob_flag;
        completed_fault = environment.set_rob_head(fault.rob, fault.rob_flag) &&
            issue_load(fault);
    }
    if (!completed_fault || !environment.run_cycles(8) ||
        environment.exception_vaddr() != expected_fault ||
        environment.exception_is_for_vs_nonleaf_pte() ||
        environment.uncache_requests() != uncache_before) {
        std::ostringstream detail;
        detail << canonical_phase << ":terminal-fault:expected_vaddr=0x"
               << std::hex << expected_fault << ":actual_vaddr=0x"
               << environment.exception_vaddr();
        phase = detail.str();
        return false;
    }
    // Legal speculative cacheable prefix traffic is allowed. Precise
    // fault identity/address and observed redirect queue conservation
    // decide correctness, without predicting pipeline/replay timing.
    if (!recover_random_precise_error(
            fault_rob, fault_rob_flag, store,
            lq_before, sq_before, lq_cancels, sq_cancels)) {
        return false;
    }
    phase = canonical_phase;
    if (store) {
        // Read the actual cache image after cancellation, as well as
        // the bus pre-state: a forbidden speculative store must not
        // be hidden by comparing two reference-model copies alone.
        if (!check_controls()) {
            phase += ":store-side-effect";
            return false;
        }
        ++canonical_boundary_store_checks;
    }
    environment.select_store_exception_address(false);
    ++canonical_boundary_crosses[cross];
    ++canonical_boundary_recoveries;
    constraint_coverage.sample_operation(
        store ? RandomConstraints::scalar_store : RandomConstraints::scalar_load);
    constraint_coverage.sample_translation(
        context, TranslationLeafTopology{}, ptw_before,
        environment.ptw_requests(), 1, 0);
    return true;
}

bool MixedScenario::issue_vector_canonical_boundary(unsigned cross)
{
    const unsigned eew = cross % 3U + 1U;
    const unsigned width = 1U << eew;
    const unsigned addressing = (cross / 3U) % 3U + 1U;
    const unsigned mode = (cross / 9U) % 2U;
    const bool invalid_first = cross / 18U != 0;
    const unsigned bits = mode == 0 ? 39U : 48U;
    const std::uint64_t low_limit = std::uint64_t{1} << (bits - 1);
    const std::uint64_t high_base = 0U - low_limit;
    const unsigned distance = 1U + random() % (width - 1U);
    const std::uint64_t element_address =
        (invalid_first ? high_base : low_limit) - distance;
    const std::uint64_t expected_fault =
        invalid_first ? element_address : low_limit;
    {
        std::ostringstream detail;
        detail << "random-vector-canonical-boundary:cross=" << cross
               << ":mode=" << bits << ":addressing=" << addressing
               << ":invalid_first=" << invalid_first
               << ":width=" << width << ":distance=" << distance
               << ":address=0x" << std::hex << element_address;
        phase = detail.str();
    }
    const std::string canonical_phase = phase;
    TranslationContext context;
    context.regime = RandomConstraints::translation_stage1;
    context.stage1_mode = mode;
    if (!constraints.vector_canonical_boundary_cross_enabled(cross) ||
        !environment.run_until_all_complete(constrained_completion_timeout) ||
        !environment.run_until_queues_retired(constrained_completion_timeout) ||
        !enter_translation_context(context) ||
        !environment.set_pointer_masking(memblock::PointerMaskingConfig{}) ||
        !issue_constrained_fence(RandomConstraints::vector_store)) {
        return false;
    }
    std::array<std::uint64_t, 2> control_virtual{{low_limit - 8U, high_base}};
    std::array<std::uint64_t, 2> control_physical{{
        canonical_boundary_physical + mode * 0x2000U + 0xff8U,
        canonical_boundary_physical + mode * 0x2000U + 0x1000U}};
    std::array<std::uint64_t, 2> control_data{};
    for (unsigned side = 0; side < 2; ++side) {
        control_data[side] = environment.bus_expected_load(
            control_physical[side], memblock::LoadOp::ld);
    }
    const auto check_controls = [&]() {
        for (unsigned side = 0; side < 2; ++side) {
            auto control = make_load(
                control_virtual[side], memblock::LoadOp::ld,
                random() % memblock::kScalarLoadLanes);
            control.oracle_address = control_physical[side];
            const auto walk = memblock::reference_page_walk(
                environment.memory(), random_stage1_roots[mode],
                control.address, page_mode(mode));
            if (!walk.translated ||
                walk.physical_address != control_physical[side] ||
                environment.bus_expected_load(
                    control_physical[side], memblock::LoadOp::ld) !=
                    control_data[side] ||
                !environment.set_rob_head(control.rob, control.rob_flag) ||
                !issue_load(control, control_data[side], false) ||
                !environment.run_until_lq_retired(constrained_completion_timeout)) {
                return false;
            }
            ++vector_canonical_boundary_controls;
        }
        return true;
    };
    if (!check_controls()) {
        phase += ":positive-control";
        return false;
    }
    const auto ptw_before = environment.ptw_requests();
    const auto uncache_before = environment.uncache_requests();
    const auto lq_before = environment.lq_dequeued();
    const auto sq_before = environment.sq_dequeued();
    const auto lq_cancels = environment.lq_redirect_canceled_observed();
    const auto sq_cancels = environment.sq_redirect_canceled_observed();
    auto fault = make_vector(
        true, element_address, eew,
        random() % memblock::kVectorMemoryLanes,
        static_cast<memblock::VectorAddressingMode>(addressing));
    fault.vsew = static_cast<std::uint8_t>(eew);
    fault.vl = 1;
    fault.vstart = 0;
    fault.vm = true;
    fault.vma = random() % 1000 < constraints.vector_vma_per_mille;
    fault.vta = random() % 1000 < constraints.vector_vta_per_mille;
    fault.expected_exception_mask = memblock::kExceptionStorePageFault;
    fault.expected_writeback_vstart = 0;
    if (addressing == 1) {
        const std::int64_t stride = width * (1U + random() % 16U);
        fault.stride = (random() & 1U) != 0 ? stride : -stride;
    } else {
        const std::uint64_t index_offset = (random() % 256U) * width;
        fault.address = element_address - index_offset;
        for (unsigned byte = 0; byte < width; ++byte) {
            fault.index[byte] = static_cast<unsigned char>(
                index_offset >> (byte * 8U));
        }
    }
    // Walk actual effective bytes independently of the faulting
    // transaction's expected exception and without a PA override.
    if (memblock::active_vector_elements(fault) != 1U ||
        memblock::vector_element_address(fault, 0) != element_address) {
        phase += ":active-element";
        return false;
    }
    std::optional<std::uint64_t> first_invalid;
    unsigned valid_bytes = 0;
    for (unsigned byte = 0; byte < width; ++byte) {
        const auto walk = memblock::reference_page_walk(
            environment.memory(), random_stage1_roots[mode],
            memblock::vector_element_address(fault, 0) + byte,
            page_mode(mode));
        if (walk.translated) {
            ++valid_bytes;
        } else if (!first_invalid) {
            first_invalid = element_address + byte;
        }
    }
    if (first_invalid != expected_fault || valid_bytes == 0 ||
        valid_bytes == width) {
        phase += ":reference-range";
        return false;
    }
    environment.select_store_exception_address(true);
    environment.expect_vector(fault);
    if (!environment.set_rob_head(fault.rob, fault.rob_flag) ||
        !environment.enqueue_vector(fault) ||
        !environment.issue_vector(fault, 2048) ||
        !environment.run_until_vector_complete_with_replays(
            fault, constrained_completion_timeout) ||
        !environment.run_cycles(8) ||
        environment.exception_vaddr() != expected_fault ||
        environment.exception_is_for_vs_nonleaf_pte() ||
        environment.uncache_requests() != uncache_before) {
        std::ostringstream detail;
        detail << canonical_phase << ":terminal-fault:expected_vaddr=0x"
               << std::hex << expected_fault << ":actual_vaddr=0x"
               << environment.exception_vaddr();
        phase = detail.str();
        return false;
    }
    // VLS exceptions require the backend's vector redirect qualifier.
    // All reserved flows must retire or be canceled on observed IO.
    if (!environment.redirect_after(fault.rob, fault.rob_flag, true, true) ||
        !environment.run_cycles(96) ||
        !environment.run_until_queues_retired(constrained_completion_timeout)) {
        phase += ":redirect-retire";
        return false;
    }
    const auto lq_dequeued = environment.lq_dequeued() - lq_before;
    const auto sq_dequeued = environment.sq_dequeued() - sq_before;
    const auto lq_canceled =
        environment.lq_redirect_canceled_observed() - lq_cancels;
    const auto sq_canceled =
        environment.sq_redirect_canceled_observed() - sq_cancels;
    if (lq_dequeued + lq_canceled != 0 ||
        sq_dequeued + sq_canceled != fault.flow_num) {
        std::ostringstream detail;
        detail << canonical_phase << ":queue-accounting:lq="
               << lq_dequeued << '+' << lq_canceled << ":sq="
               << sq_dequeued << '+' << sq_canceled
               << ":flows=" << unsigned(fault.flow_num);
        phase = detail.str();
        return false;
    }
    --rob_offset;
    sq_offset -= sq_canceled;
    ++coverage.exceptions;
    phase = canonical_phase;
    if (!check_controls()) {
        phase += ":store-side-effect";
        return false;
    }
    ++vector_canonical_boundary_store_checks;
    environment.select_store_exception_address(false);
    // A real scalar commit/readback pair proves the SQ can accept and
    // retire useful work after the fault, using the observed cursor.
    auto survivor = make_store(
        high_base + 0x100U, random(), memblock::StoreOp::sd,
        random() % memblock::kScalarStoreLanes,
        random() % memblock::kScalarStoreLanes);
    survivor.oracle_address =
        canonical_boundary_physical + mode * 0x2000U + 0x1100U;
    const auto survivor_walk = memblock::reference_page_walk(
        environment.memory(), random_stage1_roots[mode],
        survivor.address, page_mode(mode));
    environment.expect_store(survivor);
    if (!survivor_walk.translated ||
        survivor_walk.physical_address != *survivor.oracle_address ||
        !environment.set_rob_head(survivor.rob, survivor.rob_flag) ||
        !environment.enqueue_store(
            survivor, memblock::lq_pointer_value(lq_offset)) ||
        !environment.issue_store_data(survivor, 2048) ||
        !environment.issue_store_address(survivor, 2048) ||
        !environment.run_until_store_complete_with_replay(
            survivor, constrained_completion_timeout) ||
        !environment.commit_store(survivor, constrained_completion_timeout)) {
        phase += ":survivor-store";
        return false;
    }
    auto survivor_load = make_load(
        survivor.address, memblock::LoadOp::ld,
        random() % memblock::kScalarLoadLanes);
    survivor_load.oracle_address = survivor.oracle_address;
    if (!environment.set_rob_head(survivor_load.rob, survivor_load.rob_flag) ||
        !issue_load(survivor_load, survivor.data, false) ||
        !environment.run_until_queues_retired(constrained_completion_timeout)) {
        phase += ":survivor-load";
        return false;
    }
    ++vector_canonical_boundary_crosses[cross];
    ++vector_canonical_boundary_recoveries;
    vector_canonical_boundary_flows += fault.flow_num;
    constraint_coverage.sample_operation(RandomConstraints::vector_store);
    constraint_coverage.sample_translation(
        context, TranslationLeafTopology{}, ptw_before,
        environment.ptw_requests(), 1, 0);
    ++actions;
    return true;
}

bool MixedScenario::issue_gstage_boundary(unsigned cross, bool vector)
{
    const unsigned eew = cross % 3U + 1U;
    const unsigned width = 1U << eew;
    const unsigned mode = vector ? cross / 9U : cross / 6U;
    const bool store = vector || (cross / 3U) % 2U != 0;
    const unsigned addressing = vector ? (cross / 3U) % 3U + 1U : 0U;
    const unsigned bits = mode == 0 ? 41U : 50U;
    const std::uint64_t limit = std::uint64_t{1} << bits;
    const unsigned distance = 1U + random() % (width - 1U);
    const std::uint64_t element_address = limit - distance;
    {
        std::ostringstream detail;
        detail << "random-gstage-boundary:cross=" << cross
               << ":vector=" << vector << ":mode=" << bits
               << ":store=" << store << ":addressing=" << addressing
               << ":width=" << width << ":distance=" << distance
               << ":address=0x" << std::hex << element_address;
        phase = detail.str();
    }
    const std::string boundary_phase = phase;
    if (!(vector ? constraints.vector_gstage_boundary_cross_enabled(cross)
                 : constraints.gstage_boundary_cross_enabled(cross)) ||
        !environment.run_until_all_complete(constrained_completion_timeout) ||
        !environment.run_until_queues_retired(constrained_completion_timeout) ||
        !environment.run_until_dcache_idle(constrained_completion_timeout) ||
        !flush_pending_translation_fences()) {
        return false;
    }
    translation_context_valid = false;
    pending_translation_fences |= pending_hfence_gvma;
    if (!environment.activate_two_stage_modes(
            memblock::ReferencePageMode::bare, page_mode(mode),
            0, gstage_boundary_roots[mode], 0, 37) ||
        !environment.set_pointer_masking(memblock::PointerMaskingConfig{}) ||
        !environment.issue_sfence(0, 0, true, true, false, true)) {
        return false;
    }
    const auto walk_access = [&](std::uint64_t address, bool write) {
        return memblock::reference_two_stage_access(
            environment.memory(), 0, gstage_boundary_roots[mode], address,
            write ? memblock::ReferenceMemoryAccess::store
                  : memblock::ReferenceMemoryAccess::load,
            memblock::ReferencePageMode::bare, page_mode(mode));
    };
    const std::array<std::uint64_t, 2> control_guest{{limit - 8U, 0}};
    const std::array<std::uint64_t, 2> control_physical{{
        gstage_boundary_physical + mode * 0x2000U + 0xff8U,
        gstage_boundary_physical + mode * 0x2000U + 0x1000U}};
    std::array<std::uint64_t, 2> control_data{};
    for (unsigned side = 0; side < 2; ++side) {
        control_data[side] = environment.bus_expected_load(
            control_physical[side], memblock::LoadOp::ld);
    }
    const auto check_controls = [&]() {
        for (unsigned side = 0; side < 2; ++side) {
            auto control = make_load(
                control_guest[side], memblock::LoadOp::ld,
                random() % memblock::kScalarLoadLanes);
            control.oracle_address = control_physical[side];
            const auto walk = walk_access(control.address, false);
            if (!walk.translated ||
                walk.physical_address != control_physical[side] ||
                environment.bus_expected_load(
                    control_physical[side], memblock::LoadOp::ld) !=
                    control_data[side] ||
                !environment.set_rob_head(control.rob, control.rob_flag) ||
                !issue_load(control, control_data[side], false) ||
                !environment.run_until_lq_retired(constrained_completion_timeout)) {
                return false;
            }
            if (vector) {
                ++vector_gstage_boundary_controls;
            } else {
                ++gstage_boundary_controls;
            }
        }
        return true;
    };
    if (!check_controls()) {
        phase += ":positive-control";
        return false;
    }
    environment.select_store_exception_address(store);
    const auto uncache_before = environment.uncache_requests();
    const auto lq_before = environment.lq_dequeued();
    const auto sq_before = environment.sq_dequeued();
    const auto lq_cancels = environment.lq_redirect_canceled_observed();
    const auto sq_cancels = environment.sq_redirect_canceled_observed();
    std::uint8_t fault_rob = 0;
    bool fault_rob_flag = false;
    unsigned fault_flows = 1;
    const auto check_range = [&](std::uint64_t address) {
        std::optional<std::uint64_t> first_invalid;
        unsigned valid_bytes = 0;
        for (unsigned byte = 0; byte < width; ++byte) {
            const auto walk = walk_access(address + byte, store);
            if (walk.translated) {
                if (walk.physical_address !=
                    control_physical[0] + 8U - distance + byte) {
                    return false;
                }
                ++valid_bytes;
            } else {
                if (!walk.guest_page_fault || walk.stage1_page_fault ||
                    walk.access_fault || walk.is_for_vs_nonleaf_pte ||
                    walk.faulting_guest_physical_address != address + byte) {
                    return false;
                }
                if (!first_invalid) {
                    first_invalid = address + byte;
                }
            }
        }
        return first_invalid == limit && valid_bytes == distance;
    };
    bool completed_fault = false;
    if (vector) {
        auto fault = make_vector(
            true, element_address, eew,
            random() % memblock::kVectorMemoryLanes,
            static_cast<memblock::VectorAddressingMode>(addressing));
        fault.vsew = static_cast<std::uint8_t>(eew);
        fault.vl = 1;
        fault.vstart = 0;
        fault.vm = true;
        fault.vma = random() % 1000 < constraints.vector_vma_per_mille;
        fault.vta = random() % 1000 < constraints.vector_vta_per_mille;
        fault.expected_exception_mask = memblock::kExceptionStoreGuestPageFault;
        fault.expected_writeback_vstart = 0;
        if (addressing == 1) {
            const std::int64_t stride = width * (1U + random() % 16U);
            fault.stride = (random() & 1U) != 0 ? stride : -stride;
        } else {
            const std::uint64_t offset = (random() % 256U) * width;
            fault.address = element_address - offset;
            for (unsigned byte = 0; byte < width; ++byte) {
                fault.index[byte] = static_cast<unsigned char>(
                    offset >> (byte * 8U));
            }
        }
        const auto effective = memblock::vector_element_address(fault, 0);
        if (memblock::active_vector_elements(fault) != 1U ||
            effective != element_address || !check_range(effective)) {
            phase += ":vector-reference-range";
            return false;
        }
        fault_rob = fault.rob;
        fault_rob_flag = fault.rob_flag;
        fault_flows = fault.flow_num;
        environment.expect_vector(fault);
        completed_fault = environment.set_rob_head(fault.rob, fault.rob_flag) &&
            environment.enqueue_vector(fault) &&
            environment.issue_vector(fault, 2048) &&
            environment.run_until_vector_complete_with_replays(
                fault, constrained_completion_timeout);
    } else if (store) {
        auto fault = make_store(
            element_address, random(), static_cast<memblock::StoreOp>(eew),
            random() % memblock::kScalarStoreLanes,
            random() % memblock::kScalarStoreLanes);
        fault.expected_exception_mask = memblock::kExceptionStoreGuestPageFault;
        if (!check_range(fault.address)) {
            phase += ":scalar-store-reference-range";
            return false;
        }
        fault_rob = fault.rob;
        fault_rob_flag = fault.rob_flag;
        environment.expect_store(fault);
        const bool data_first = (random() & 1U) != 0;
        completed_fault = environment.set_rob_head(fault.rob, fault.rob_flag) &&
            environment.enqueue_store(fault, memblock::lq_pointer_value(lq_offset)) &&
            (data_first
                    ? environment.issue_store_data(fault) &&
                        environment.issue_store_address(fault)
                    : environment.issue_store_address(fault) &&
                        environment.issue_store_data(fault)) &&
            environment.run_until_store_complete_with_replay(
                fault, constrained_completion_timeout, true);
        coverage.sample(fault, data_first);
    } else {
        auto fault = make_load(
            element_address, static_cast<memblock::LoadOp>(eew),
            random() % memblock::kScalarLoadLanes);
        fault.expected_exception_mask = memblock::kExceptionLoadGuestPageFault;
        if (!check_range(fault.address)) {
            phase += ":scalar-load-reference-range";
            return false;
        }
        fault_rob = fault.rob;
        fault_rob_flag = fault.rob_flag;
        completed_fault = environment.set_rob_head(fault.rob, fault.rob_flag) &&
            issue_load(fault, std::nullopt, false);
        coverage.sample(fault);
    }
    // Legal speculative prefix traffic is allowed. The transaction's
    // exact completion and full first-invalid VA/GPA decide the result.
    if (!completed_fault || !environment.run_cycles(8) ||
        environment.exception_vaddr() != limit ||
        environment.exception_gpaddr() != limit ||
        environment.exception_is_for_vs_nonleaf_pte() ||
        environment.uncache_requests() != uncache_before) {
        std::ostringstream detail;
        detail << boundary_phase << ":terminal-fault:expected=0x"
               << std::hex << limit << ":actual_va=0x"
               << environment.exception_vaddr() << ":actual_gpa=0x"
               << environment.exception_gpaddr();
        phase = detail.str();
        return false;
    }
    if (!environment.redirect_after(fault_rob, fault_rob_flag, true, vector) ||
        !environment.run_cycles(96) ||
        !environment.run_until_queues_retired(constrained_completion_timeout)) {
        phase += ":redirect-retire";
        return false;
    }
    const auto lq_dequeued = environment.lq_dequeued() - lq_before;
    const auto sq_dequeued = environment.sq_dequeued() - sq_before;
    const auto lq_canceled = environment.lq_redirect_canceled_observed() - lq_cancels;
    const auto sq_canceled = environment.sq_redirect_canceled_observed() - sq_cancels;
    if (lq_dequeued + lq_canceled != (store ? 0U : 1U) ||
        sq_dequeued + sq_canceled != (store ? fault_flows : 0U)) {
        std::ostringstream detail;
        detail << boundary_phase << ":queue-accounting:lq="
               << lq_dequeued << '+' << lq_canceled << ":sq="
               << sq_dequeued << '+' << sq_canceled
               << ":flows=" << fault_flows;
        phase = detail.str();
        return false;
    }
    --rob_offset;
    lq_offset -= lq_canceled;
    sq_offset -= sq_canceled;
    ++coverage.exceptions;
    phase = boundary_phase;
    if (!check_controls()) {
        phase += ":edge-readback";
        return false;
    }
    environment.select_store_exception_address(false);
    auto survivor = make_store(
        0x100U, random(), memblock::StoreOp::sd,
        random() % memblock::kScalarStoreLanes,
        random() % memblock::kScalarStoreLanes);
    survivor.oracle_address = control_physical[1] + 0x100U;
    const auto survivor_walk = walk_access(survivor.address, true);
    environment.expect_store(survivor);
    if (!survivor_walk.translated ||
        survivor_walk.physical_address != *survivor.oracle_address ||
        !environment.set_rob_head(survivor.rob, survivor.rob_flag) ||
        !environment.enqueue_store(survivor, memblock::lq_pointer_value(lq_offset)) ||
        !environment.issue_store_data(survivor, 2048) ||
        !environment.issue_store_address(survivor, 2048) ||
        !environment.run_until_store_complete_with_replay(
            survivor, constrained_completion_timeout) ||
        !environment.commit_store(survivor, constrained_completion_timeout)) {
        phase += ":survivor-store";
        return false;
    }
    auto survivor_load = make_load(
        survivor.address, memblock::LoadOp::ld,
        random() % memblock::kScalarLoadLanes);
    survivor_load.oracle_address = survivor.oracle_address;
    if (!environment.set_rob_head(survivor_load.rob, survivor_load.rob_flag) ||
        !issue_load(survivor_load, survivor.data, false) ||
        !environment.run_until_queues_retired(constrained_completion_timeout)) {
        phase += ":survivor-load";
        return false;
    }
    if (vector) {
        ++vector_gstage_boundary_crosses[cross];
        ++vector_gstage_boundary_recoveries;
        ++vector_gstage_boundary_store_checks;
        vector_gstage_boundary_flows += fault_flows;
    } else {
        ++gstage_boundary_crosses[cross];
        ++gstage_boundary_recoveries;
        gstage_boundary_store_checks += store;
    }
    constraint_coverage.sample_operation(vector ? RandomConstraints::vector_store
            : store ? RandomConstraints::scalar_store : RandomConstraints::scalar_load);
    ++actions;
    return true;
}

} // namespace memblock::scenarios::mixed
