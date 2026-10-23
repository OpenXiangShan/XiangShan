#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

bool MixedScenario::execute_vector_memory(ActionWindow &window)
{
    auto &kind = window.kind;
    auto &vector_fof = window.vector_fof;
    auto &vector_fof_first_fault = window.vector_fof_first_fault;
    auto &vector_fof_eew = window.vector_fof_eew;
    auto &translation = window.translation;
    auto &leaf_addressed = window.leaf_addressed;

    const bool store = kind == RandomConstraints::vector_store;
    if (vector_fof) {
        phase = vector_fof_first_fault
            ? "random-vector-fof-first-fault"
            : "random-vector-fof-later-fault";
        if (!environment.run_until_all_complete(8192)) {
            phase += ":initial-completion-drain";
            return false;
        }
        if (!environment.run_until_queues_retired(8192)) {
            phase += ":initial-queue-drain";
            return false;
        }
        const unsigned eew = vector_fof_eew;
        const unsigned element_bytes = 1U << eew;
        const std::uint64_t address = vector_fof_virtual_base +
            0x1000U - (vector_fof_first_fault ? 0U : element_bytes);
        const auto first_reference =
            memblock::reference_page_walk_permissions(
                environment.memory(),
                random_stage1_roots[translation.stage1_mode],
                address, page_mode(translation.stage1_mode));
        const auto next_reference =
            memblock::reference_page_walk_permissions(
                environment.memory(),
                random_stage1_roots[translation.stage1_mode],
                address + element_bytes,
                page_mode(translation.stage1_mode));
        const bool first_is_valid = first_reference.walk.translated &&
            first_reference.walk.physical_address ==
                vector_fof_physical_base + 0x1000U - element_bytes;
        if ((vector_fof_first_fault &&
                first_reference.walk.translated) ||
            (!vector_fof_first_fault &&
                (!first_is_valid || next_reference.walk.translated))) {
            phase += ":reference-page-layout";
            return false;
        }

        const std::uint64_t rob = rob_offset++;
        const std::uint64_t lq = lq_offset;
        lq_offset += 2U;
        memblock::VectorMemoryTransaction data_uop{
            .address = address,
            .oracle_address = vector_fof_first_fault
                ? std::nullopt
                : std::optional<std::uint64_t>{
                      first_reference.walk.physical_address},
            .eew = static_cast<std::uint8_t>(eew),
            .vl = 2,
            .rob = memblock::rob_pointer_value(rob),
            .rob_flag = memblock::rob_pointer_flag(rob),
            .lq = memblock::lq_pointer_value(lq),
            .lq_flag = memblock::lq_pointer_flag(lq),
            .sq = memblock::sq_pointer_value(sq_offset),
            .sq_flag = memblock::sq_pointer_flag(sq_offset),
            .pdest = static_cast<std::uint8_t>(1U + random() % 254U),
            .lane = 0,
            .flow_num = 2,
            .expected_exception_mask = vector_fof_first_fault
                ? memblock::kExceptionLoadPageFault : 0,
            .expected_trigger = vector_fof_first_fault
                ? std::nullopt
                : std::optional<std::uint8_t>{
                      memblock::kVectorWritebackTriggerNone},
            .vuop_idx = 0,
            .last_uop = false,
            .fault_only_first = true,
            .is_vleff = true,
            .expected_debug_is_mmio = false,
            .expected_debug_is_ncio = false,
            .expected_debug_is_perf_cnt = false,
            .fof_unspecified_elements = static_cast<std::uint16_t>(
                vector_fof_first_fault ? 0U : 1U << 1),
        };
        for (auto &byte : data_uop.data) {
            byte = static_cast<unsigned char>(random());
        }
        auto expected_data = data_uop.data;
        if (!vector_fof_first_fault) {
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                expected_data[byte] = environment.memory().read_byte(
                    first_reference.walk.physical_address + byte);
            }
        }
        const std::uint64_t lq_allocated_before =
            environment.lq_allocated();
        environment.expect_vector_data(data_uop, expected_data);
        if (!environment.set_rob_head(
                data_uop.rob, data_uop.rob_flag)) {
            phase += ":set-rob-head";
            return false;
        }
        if (!environment.enqueue_vector(data_uop)) {
            phase += ":enqueue";
            return false;
        }
        if (!environment.issue_vector(
                data_uop, constrained_completion_timeout)) {
            phase += ":issue";
            return false;
        }
        if (!environment.run_until_vector_complete_with_replays(
                data_uop, constrained_completion_timeout)) {
            phase += ":vector-completion";
            return false;
        }
        if (!environment.run_until_lq_retired(8192)) {
            phase += ":lq-retirement";
            return false;
        }
        if (environment.lq_allocated() !=
            lq_allocated_before + 2U) {
            std::ostringstream detail;
            detail << phase << ":lq-allocation"
                   << ":before=" << lq_allocated_before
                   << ":after=" << environment.lq_allocated();
            phase = detail.str();
            return false;
        }
        auto fix_vl_uop = data_uop;
        fix_vl_uop.pdest =
            static_cast<std::uint8_t>(data_uop.pdest + 1U);
        fix_vl_uop.expected_exception_mask = 0;
        fix_vl_uop.expected_vl = vector_fof_first_fault ? 2U : 1U;
        fix_vl_uop.vuop_idx = 1;
        fix_vl_uop.last_uop = true;
        fix_vl_uop.vl_wen = true;
        fix_vl_uop.fof_unspecified_elements = 0;
        fix_vl_uop.expected_debug_is_mmio.reset();
        fix_vl_uop.expected_debug_is_ncio.reset();
        fix_vl_uop.expected_debug_is_perf_cnt.reset();
        const std::uint64_t fix_writebacks_before =
            environment.vector_fof_fix_writebacks();
        environment.expect_vector_data(fix_vl_uop, {});
        if (!environment.issue_vector(
                fix_vl_uop, constrained_completion_timeout)) {
            phase += ":fix-vl-issue";
            return false;
        }
        if (!environment.run_until_vector_complete(
                constrained_completion_timeout)) {
            phase += ":fix-vl-completion";
            return false;
        }
        if (environment.lq_allocated() !=
            lq_allocated_before + 2U) {
            std::ostringstream detail;
            detail << phase << ":fix-vl-lq-allocation"
                   << ":expected=" << lq_allocated_before + 2U
                   << ":actual=" << environment.lq_allocated();
            phase = detail.str();
            return false;
        }
        if (environment.vector_fof_fix_writebacks() !=
            fix_writebacks_before + 1U) {
            std::ostringstream detail;
            detail << phase << ":fix-vl-writeback"
                   << ":expected=" << fix_writebacks_before + 1U
                   << ":actual="
                   << environment.vector_fof_fix_writebacks();
            phase = detail.str();
            return false;
        }
        if (vector_fof_first_fault &&
            !environment.redirect_after(
                data_uop.rob, data_uop.rob_flag, true, true)) {
            phase += ":trap-redirect";
            return false;
        }

        ++constraint_coverage.vector_fof_presence[1];
        ++constraint_coverage.vector_fof_fault_positions[
            vector_fof_first_fault ? 1U : 0U];
        ++constraint_coverage.vector_fof_eews[eew];
        ++constraint_coverage.vector_fof_stage1_modes[
            translation.stage1_mode];
        ++constraint_coverage.vector_fof_crosses[
            ((vector_fof_first_fault ? 1U : 0U) * 2U +
                translation.stage1_mode) * 4U + eew];
        ++constraint_coverage.vector_fof_fix_vl;
        coverage.sample(data_uop);
        ++actions;
        leaf_addressed = true;
    } else {
        const auto &shape = choose_vector_shape(
            store ? 1U : 0U, std::nullopt, false);
        const unsigned data_eew = shape.addressing >= 2
            ? shape.vsew : shape.eew;
        const bool vector_corner =
            random() % 1000 < constraints.vector_corner_per_mille;
        const auto uops = make_random_vector_uops(
            store, action_cacheable_address(window, 1U << data_eew),
            shape, vector_corner);
        if (!issue_random_vector_instruction(uops)) {
            return false;
        }
        sample_constrained_vector_instruction(store, shape, uops);
    }
    ++coverage.cacheable;

    return true;
}

bool MixedScenario::execute_vector_segment(ActionWindow &window)
{
    auto &vector_segment_store = window.vector_segment_store;
    auto &vector_segment_fof = window.vector_segment_fof;
    auto &vector_segment_fof_first_fault = window.vector_segment_fof_first_fault;
    auto &vector_segment_fof_stage1_mode = window.vector_segment_fof_stage1_mode;
    auto &vector_segment_fof_shape = window.vector_segment_fof_shape;
    auto &vector_segment_high_va_cross = window.vector_segment_high_va_cross;
    auto &translation = window.translation;
    auto &leaf_addressed = window.leaf_addressed;

    if (vector_segment_fof) {
        if (!issue_random_vector_segment_fof(
                vector_segment_fof_first_fault,
                vector_segment_fof_stage1_mode,
                *vector_segment_fof_shape)) {
            return false;
        }
        leaf_addressed = true;
    } else {
        if (!environment.run_until_all_complete(8192) ||
            !environment.run_until_queues_retired(8192)) {
            return false;
        }
        const bool high_va_misaligned = vector_segment_high_va_cross &&
            *vector_segment_high_va_cross % 2U != 0;
        const auto &shape = choose_vector_segment_shape(high_va_misaligned);
        const auto addressing =
            static_cast<memblock::VectorAddressingMode>(
                shape.addressing);
        const bool indexed = shape.addressing >= 2;
        const unsigned fields = shape.nf_index + 2U;
        const unsigned lmul_uops = 1U << static_cast<unsigned>(
                                       std::max(shape.lmul_log2, 0));
        const unsigned emul_uops = 1U << static_cast<unsigned>(
                                       std::max(shape.emul_log2, 0));
        const unsigned data_group_uops = indexed
            ? lmul_uops : emul_uops;
        const unsigned data_uops = data_group_uops * fields;
        const unsigned uop_count = indexed
            ? std::max(data_uops, emul_uops) : data_uops;
        const unsigned data_eew = indexed ? shape.vsew : shape.eew;
        const unsigned data_bytes = 1U << data_eew;
        const unsigned vector_bytes = shape.lmul_log2 < 0
            ? 16U >> static_cast<unsigned>(-shape.lmul_log2)
            : 16U << static_cast<unsigned>(shape.lmul_log2);
        const unsigned vlmax = vector_bytes >> shape.vsew;
        const bool vector_corner =
            random() % 1000 < constraints.vector_corner_per_mille;
        std::int64_t stride = 0;
        if (addressing == memblock::VectorAddressingMode::strided) {
            const std::int64_t magnitude =
                static_cast<std::int64_t>(
                    (fields + 1U) * data_bytes *
                    (1U + random() % 4U));
            stride = (random() & 1U) == 0 ? magnitude : -magnitude;
        }
        std::uint64_t address = action_cacheable_address(window,
            data_bytes, vector_segment_high_va_cross
                ? std::optional<bool>{high_va_misaligned} : std::nullopt);
        if (stride < 0) {
            address += static_cast<std::uint64_t>(-stride) *
                (vlmax - 1U);
        }
        const std::uint64_t physical_address = address;
        if (vector_segment_high_va_cross) {
            const unsigned virtual_mode =
                translation.regime == RandomConstraints::translation_nested
                ? translation.vs_mode : translation.stage1_mode;
            const std::uint64_t high_base = 0U - (std::uint64_t{1} <<
                                                     (virtual_mode == 0 ? 38U : 47U));
            address += high_base;
            std::ostringstream detail;
            detail << phase << ":segment-high-va:cross="
                   << *vector_segment_high_va_cross
                   << ":regime="
                   << (translation.regime ==
                                  RandomConstraints::translation_nested
                              ? "nested" : "stage1")
                   << ":vs=" << translation.vs_mode
                   << ":g=" << translation.g_mode
                   << ":va=0x" << std::hex << address
                   << ":pa=0x" << physical_address << std::dec;
            phase = detail.str();
        }
        const std::uint64_t rob = rob_offset++;
        const std::uint8_t vl = static_cast<std::uint8_t>(
            vector_corner ? 1U + random() % vlmax : vlmax);
        const bool vm = !vector_corner || (random() & 1U) != 0;
        std::uint16_t mask_bits = static_cast<std::uint16_t>(random());
        const std::uint8_t vstart = vector_corner
            ? static_cast<std::uint8_t>(random() %
                  (vector_segment_high_va_cross
                          ? (vm ? vl : std::min<unsigned>(vl, 16U)) : vl + 1U))
            : 0;
        if (vector_segment_high_va_cross && !vm) {
            // A high-VA bin requires actual translated memory work;
            // zero-active-element instructions cannot satisfy it.
            mask_bits |= static_cast<std::uint16_t>(1U << vstart);
        }
        const std::uint8_t first_pdest = static_cast<std::uint8_t>(
            1 + random() % (256 - uop_count));
        const std::uint8_t vlmul = static_cast<std::uint8_t>(
            shape.lmul_log2 < 0
                ? shape.lmul_log2 + 8 : shape.lmul_log2);
        const unsigned bytes_per_data_uop = data_group_uops == 1 &&
                (indexed ? shape.lmul_log2 : shape.emul_log2) < 0
            ? 16U >> static_cast<unsigned>(
                         -(indexed ? shape.lmul_log2 : shape.emul_log2))
            : 16U;
        const unsigned flow_num = bytes_per_data_uop >> data_eew;
        std::array<unsigned char, 128> index_group{};
        if (indexed) {
            const unsigned index_bytes = 1U << shape.eew;
            for (unsigned element = 0; element < vlmax; ++element) {
                const std::uint64_t offset =
                    static_cast<std::uint64_t>(element) * data_bytes *
                    (fields + 1U);
                for (unsigned byte = 0; byte < index_bytes; ++byte) {
                    index_group[element * index_bytes + byte] =
                        static_cast<unsigned char>(offset >> (8 * byte));
                }
            }
        }
        const auto segment_loads_before = environment.vector_load_writebacks();
        const auto segment_stores_before = environment.vector_store_writebacks();
        std::vector<memblock::VectorMemoryTransaction> uops;
        uops.reserve(uop_count);
        for (unsigned uop = 0; uop < uop_count; ++uop) {
            uops.push_back(memblock::VectorMemoryTransaction{
                .store = vector_segment_store,
                .segment = true,
                .address = address,
                .stride = stride,
                .addressing = addressing,
                .eew = static_cast<std::uint8_t>(shape.eew),
                .vsew = static_cast<std::uint8_t>(shape.vsew),
                .vl = vl,
                .rob = memblock::rob_pointer_value(rob),
                .rob_flag = memblock::rob_pointer_flag(rob),
                .lq = memblock::lq_pointer_value(lq_offset),
                .lq_flag = memblock::lq_pointer_flag(lq_offset),
                .sq = memblock::sq_pointer_value(sq_offset),
                .sq_flag = memblock::sq_pointer_flag(sq_offset),
                .pdest = vector_segment_store
                    ? std::uint8_t{0}
                    : static_cast<std::uint8_t>(first_pdest + uop),
                .lane = 0,
                .flow_num = static_cast<std::uint8_t>(flow_num),
                .vlmul = vlmul,
                .vuop_idx = static_cast<std::uint8_t>(uop),
                .last_uop = uop + 1 == uop_count,
                .nf = static_cast<std::uint8_t>(fields - 1U),
                .vec_wen = !vector_segment_store && uop < data_uops,
            });
            auto &segment_uop = uops.back();
            if (vector_segment_high_va_cross) {
                segment_uop.oracle_address = physical_address;
            }
            segment_uop.oracle_index_group = indexed
                ? std::optional{index_group} : std::nullopt;
            segment_uop.vm = vm;
            segment_uop.mask_bits = mask_bits;
            segment_uop.vstart = vstart;
            segment_uop.ftq_ptr = random() & 0x3fU;
            segment_uop.ftq_offset =
                static_cast<std::uint8_t>(random() & 7U);
            for (auto &byte : segment_uop.data) {
                byte = static_cast<unsigned char>(random());
            }
            if (indexed && uop < emul_uops) {
                const unsigned index_group_bytes = shape.emul_log2 < 0
                    ? 16U >> static_cast<unsigned>(-shape.emul_log2)
                    : 16U;
                const unsigned source_offset = shape.emul_log2 < 0
                    ? 0 : uop * 16U;
                std::copy_n(
                    index_group.begin() + source_offset,
                    index_group_bytes, segment_uop.index.begin());
            }
            if (vector_segment_high_va_cross && uop < data_uops) {
                auto virtual_uop = segment_uop;
                virtual_uop.oracle_address.reset();
                const auto active = memblock::active_vector_elements(segment_uop);
                for (unsigned element = 0; element < 16U / data_bytes; ++element) {
                    if (((active >> element) & 1U) == 0) {
                        continue;
                    }
                    const auto virtual_element =
                        memblock::vector_element_address(virtual_uop, element);
                    const auto physical_element =
                        memblock::vector_element_address(segment_uop, element);
                    for (unsigned byte = 0; byte < data_bytes; ++byte) {
                        const auto walk =
                            translation.regime ==
                                RandomConstraints::translation_nested
                            ? memblock::reference_two_stage_walk(
                                  environment.memory(),
                                  random_vs_roots[translation.vs_mode],
                                  random_g_roots[translation.g_mode],
                                  virtual_element + byte,
                                  page_mode(translation.vs_mode),
                                  page_mode(translation.g_mode))
                            : memblock::reference_two_stage_walk(
                                  environment.memory(),
                                  random_stage1_roots[
                                      translation.stage1_mode],
                                  random_g_roots[0],
                                  virtual_element + byte,
                                  page_mode(translation.stage1_mode),
                                  memblock::ReferencePageMode::bare);
                        if (!walk.translated || walk.physical_address !=
                                physical_element + byte) {
                            phase += ":segment-high-va-reference";
                            return false;
                        }
                    }
                }
            }
            if (!vector_segment_store && uop >= data_uops) {
                environment.expect_vector_data(
                    segment_uop, segment_uop.data);
            } else {
                environment.expect_vector(segment_uop);
            }
            if (!environment.issue_vector(segment_uop, 4096)) {
                return false;
            }
        }
        if (!environment.run_until_vector_complete(
                constrained_completion_timeout)) {
            return false;
        }
        if (vector_segment_high_va_cross &&
            (environment.vector_load_writebacks() - segment_loads_before !=
                    (vector_segment_store ? 0U : uop_count) ||
                environment.vector_store_writebacks() - segment_stores_before !=
                    (vector_segment_store ? uop_count : 0U))) {
            phase += ":segment-high-va-completions";
            return false;
        }
        for (unsigned uop = 0; uop < data_uops; ++uop) {
            coverage.sample(uops[uop]);
        }
        if (vector_segment_store) {
            if (!environment.pulse_sbuffer_flush() ||
                !environment.run_until_sbuffer_empty(
                    constrained_completion_timeout)) {
                return false;
            }
            for (unsigned uop = 0; uop < data_uops; ++uop) {
                environment.record_committed_vector_store(uops[uop]);
            }
        }
        if (vector_segment_high_va_cross) {
            std::uint64_t active_elements = 0;
            for (unsigned uop = 0; uop < data_uops; ++uop) {
                const auto &completed = uops[uop];
                auto virtual_uop = completed;
                virtual_uop.oracle_address.reset();
                const auto active = memblock::active_vector_elements(completed);
                for (unsigned element = 0; element < 16U / data_bytes; ++element) {
                    if (((active >> element) & 1U) == 0) {
                        continue;
                    }
                    ++active_elements;
                    if (vector_segment_store) {
                        auto readback = make_load(
                            memblock::vector_element_address(virtual_uop, element),
                            static_cast<memblock::LoadOp>(data_eew == 3U
                                    ? 3U : data_eew + 4U),
                            random() % memblock::kScalarLoadLanes);
                        readback.oracle_address =
                            memblock::vector_element_address(completed, element);
                        if (!environment.set_rob_head(
                                readback.rob, readback.rob_flag) ||
                            !issue_load(readback, std::nullopt, false)) {
                            phase += ":segment-high-va-store-readback";
                            return false;
                        }
                    }
                }
            }
            if (active_elements == 0 ||
                !environment.run_until_queues_retired(
                    constrained_completion_timeout)) {
                phase += ":segment-high-va-retire";
                return false;
            }
            ++constraint_coverage.vector_segment_high_va_crosses[
                *vector_segment_high_va_cross];
            constraint_coverage.vector_segment_high_va_data_uops += data_uops;
            constraint_coverage.vector_segment_high_va_elements += active_elements;
            // The high-VA alias is a separately declared 4-KiB
            // mapping. It cannot satisfy or contradict a profile's
            // ordinary NAPOT-only topology requirement.
            leaf_addressed =
                translation.regime ==
                    RandomConstraints::translation_nested
                ? constraints.nested_vs_napot_per_mille != 1000 &&
                    constraints.nested_g_napot_per_mille != 1000
                : constraints.stage1_napot_per_mille != 1000;
        }
        ++actions;
        ++constraint_coverage.vector_segment_directions[
            vector_segment_store ? 1 : 0];
        if (!vector_segment_store) {
            ++constraint_coverage.vector_segment_fof_presence[0];
        }
        ++constraint_coverage.vector_segment_addressing[
            shape.addressing];
        ++constraint_coverage.vector_segment_eews[shape.eew];
        ++constraint_coverage.vector_segment_sews[shape.vsew];
        ++constraint_coverage.vector_segment_lmuls[
            shape.lmul_log2 + 3];
        ++constraint_coverage.vector_segment_emuls[
            shape.emul_log2 + 3];
        ++constraint_coverage.vector_segment_nfs[shape.nf_index];
    }
    ++coverage.cacheable;

    return true;
}

} // namespace memblock::scenarios::mixed
