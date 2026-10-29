#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

unsigned int MixedScenario::vector_shape_uop_count(
    const RandomVectorShape &shape)
{
    return 1U << static_cast<unsigned>(std::max(
               shape.addressing >= 2
                   ? std::max(shape.lmul_log2, shape.emul_log2)
                   : shape.emul_log2,
               0));
}

unsigned int MixedScenario::vector_shape_flow_num(
    const RandomVectorShape &shape)
{
    if (shape.addressing == 0) {
        return memblock::kVectorUnitStrideMaxFlows;
    }
    if (shape.addressing == 1) {
        const unsigned bytes_per_uop = shape.emul_log2 < 0
            ? 16U >> static_cast<unsigned>(-shape.emul_log2)
            : 16U;
        return bytes_per_uop >> shape.eew;
    }
    const int flow_mul_log2 = std::max(
        shape.emul_log2, shape.lmul_log2);
    const unsigned bytes_per_uop = flow_mul_log2 < 0
        ? 16U >> static_cast<unsigned>(-flow_mul_log2)
        : 16U;
    return bytes_per_uop >>
        (shape.emul_log2 > shape.lmul_log2
                ? shape.eew : shape.vsew);
}

unsigned int MixedScenario::vector_shape_total_flows(
    const RandomVectorShape &shape)
{
    return vector_shape_uop_count(shape) *
        vector_shape_flow_num(shape);
}

unsigned int MixedScenario::vector_shape_deficit(const RandomVectorShape &shape,
    unsigned direction)
{
    (void)shape;
    (void)direction;
    return 0;
}

const RandomVectorShape & MixedScenario::choose_vector_shape(
    unsigned direction, std::optional<unsigned> maximum_flows,
    bool concurrent_slot)
{
    const auto eligible = [&](const RandomVectorShape &shape) {
        return !maximum_flows ||
            vector_shape_total_flows(shape) <= *maximum_flows;
    };
    const auto selection_deficit = [&](const RandomVectorShape &shape) {
        return vector_shape_deficit(shape, direction);
    };
    unsigned maximum_deficit = 0;
    for (const auto &shape : vector_shapes) {
        if (!eligible(shape)) {
            continue;
        }
        maximum_deficit = std::max(
            maximum_deficit, selection_deficit(shape));
    }
    long double total_weight = 0;
    for (const auto &shape : vector_shapes) {
        if (eligible(shape) &&
            selection_deficit(shape) == maximum_deficit) {
            total_weight += shape.weight;
        }
    }
    const long double unit = static_cast<long double>(random() >> 11) /
        static_cast<long double>(std::uint64_t{1} << 53);
    long double selection = unit * total_weight;
    const RandomVectorShape *fallback = nullptr;
    for (const auto &shape : vector_shapes) {
        if (!eligible(shape) ||
            selection_deficit(shape) != maximum_deficit) {
            continue;
        }
        fallback = &shape;
        if (selection < shape.weight) {
            return shape;
        }
        selection -= shape.weight;
    }
    if (fallback == nullptr) {
        throw std::logic_error(
            "no enabled vector shape fits the concurrent LSQ budget");
    }
    return *fallback;
}

unsigned int MixedScenario::vector_segment_shape_deficit(
    const RandomVectorSegmentShape &shape)
{
    (void)shape;
    return 0;
}

const RandomVectorSegmentShape & MixedScenario::choose_vector_segment_shape(bool require_misaligned)
{
    const auto eligible = [&](const RandomVectorSegmentShape &shape) {
        return !require_misaligned ||
            (shape.addressing >= 2 ? shape.vsew : shape.eew) != 0;
    };
    unsigned maximum_deficit = 0;
    for (const auto &shape : vector_segment_shapes) {
        if (!eligible(shape)) {
            continue;
        }
        maximum_deficit = std::max(
            maximum_deficit, vector_segment_shape_deficit(shape));
    }
    long double total_weight = 0;
    for (const auto &shape : vector_segment_shapes) {
        if (eligible(shape) &&
            vector_segment_shape_deficit(shape) == maximum_deficit) {
            total_weight += shape.weight;
        }
    }
    const long double unit = static_cast<long double>(random() >> 11) /
        static_cast<long double>(std::uint64_t{1} << 53);
    long double selection = unit * total_weight;
    const RandomVectorSegmentShape *fallback = nullptr;
    for (const auto &shape : vector_segment_shapes) {
        if (!eligible(shape) ||
            vector_segment_shape_deficit(shape) != maximum_deficit) {
            continue;
        }
        fallback = &shape;
        if (selection < shape.weight) {
            return shape;
        }
        selection -= shape.weight;
    }
    return *fallback;
}

const RandomVectorSegmentShape & MixedScenario::choose_vector_segment_fof_shape()
{
    long double total_weight = 0;
    for (const auto &shape : vector_segment_shapes) {
        if (shape.addressing == 0) {
            total_weight += shape.weight;
        }
    }
    const long double unit = static_cast<long double>(random() >> 11) /
        static_cast<long double>(std::uint64_t{1} << 53);
    long double selection = unit * total_weight;
    const RandomVectorSegmentShape *fallback = nullptr;
    for (const auto &shape : vector_segment_shapes) {
        if (shape.addressing != 0) {
            continue;
        }
        fallback = &shape;
        if (selection < shape.weight) {
            return shape;
        }
        selection -= shape.weight;
    }
    if (fallback == nullptr) {
        throw std::logic_error(
            "no enabled unit-stride vector segment FOF shape");
    }
    return *fallback;
}

bool MixedScenario::choose_binary_class(
    unsigned true_per_mille,
    const std::array<std::uint64_t, 2> &counts)
{
    (void)counts;
    if (true_per_mille == 0) {
        return false;
    }
    if (true_per_mille == 1000) {
        return true;
    }
    return random() % 1000 < true_per_mille;
}

std::vector<memblock::VectorMemoryTransaction> MixedScenario::make_random_vector_uops(
    bool store, std::uint64_t address, const RandomVectorShape &shape,
    bool vector_corner)
{
    const bool indexed = shape.addressing >= 2;
    const unsigned data_eew = indexed ? shape.vsew : shape.eew;
    const unsigned data_bytes = 1U << data_eew;
    const unsigned vector_bytes = shape.lmul_log2 < 0
        ? 16U >> static_cast<unsigned>(-shape.lmul_log2)
        : 16U << static_cast<unsigned>(shape.lmul_log2);
    const unsigned vlmax = vector_bytes >> shape.vsew;
    const unsigned uop_count = vector_shape_uop_count(shape);
    const unsigned flow_num = vector_shape_flow_num(shape);
    std::int64_t stride = 0;
    if (shape.addressing == 1) {
        const std::int64_t magnitude =
            static_cast<std::int64_t>(data_bytes *
                (1U + random() % 4U));
        if (vector_corner && !store && random() % 6U == 0) {
            stride = 0;
        } else {
            stride = (random() & 1U) == 0 ? magnitude : -magnitude;
        }
        if (stride < 0) {
            address += static_cast<std::uint64_t>(-stride) *
                (vlmax - 1U);
        }
    }
    bool masked = choose_binary_class(
        constraints.vector_masked_per_mille,
        constraint_coverage.vector_masked);
    bool vma = choose_binary_class(
        constraints.vector_vma_per_mille,
        constraint_coverage.vector_vma);
    bool vta = choose_binary_class(
        constraints.vector_vta_per_mille,
        constraint_coverage.vector_vta);
    bool partial_vl = choose_binary_class(
        constraints.vector_partial_vl_per_mille,
        constraint_coverage.vector_partial_vl);
    bool nonzero_vstart = choose_binary_class(
        constraints.vector_nonzero_vstart_per_mille,
        constraint_coverage.vector_nonzero_vstart);
    if (partial_vl && masked && nonzero_vstart && vlmax < 3U) {
        if (constraints.vector_partial_vl_per_mille != 1000) {
            partial_vl = false;
        } else if (constraints.vector_masked_per_mille != 1000) {
            masked = false;
        } else {
            nonzero_vstart = false;
        }
    }
    unsigned vl = vlmax;
    if (partial_vl) {
        const unsigned minimum_vl = masked && nonzero_vstart
            ? 2U : (masked || nonzero_vstart ? 1U : 0U);
        vl = minimum_vl + random() % (vlmax - minimum_vl);
    }
    if (masked && nonzero_vstart && vl < 2U) {
        nonzero_vstart = false;
    }
    unsigned vstart = 0;
    if (nonzero_vstart) {
        const unsigned maximum = masked
            ? std::min(vl - 1U, 15U) : vl;
        if (maximum == 0) {
            nonzero_vstart = false;
        } else {
            vstart = 1U + random() % maximum;
        }
    }
    std::uint16_t mask_bits = static_cast<std::uint16_t>(random());
    std::array<std::uint64_t, 2> mask_high{{random(), random()}};
    if (masked) {
        mask_bits &= static_cast<std::uint16_t>(~(1U << vstart));
    }
    const std::uint64_t rob = rob_offset++;
    std::uint64_t &queue_cursor = store ? sq_offset : lq_offset;
    const std::uint8_t vlmul = static_cast<std::uint8_t>(
        shape.lmul_log2 < 0
            ? shape.lmul_log2 + 8 : shape.lmul_log2);
    const std::uint8_t first_pdest = random_destination_block(
        random, memblock::kVectorPhysicalRegisters, uop_count);
    const unsigned index_bytes = 1U << shape.eew;
    const unsigned index_group_bytes_per_uop = shape.emul_log2 < 0
        ? 16U >> static_cast<unsigned>(-shape.emul_log2)
        : 16U;
    const unsigned index_elements_per_vreg =
        index_group_bytes_per_uop >> shape.eew;
    const auto indices = indexed
        ? make_random_vector_indices(random, vlmax, index_bytes, data_bytes,
              1U, store, constraints.vector_index_repeat_per_mille)
        : RandomVectorIndices{};
    if (indexed) {
        coverage.sample_vector_indices(indices.offsets, store ? 1U : 0U);
    }

    std::vector<memblock::VectorMemoryTransaction> uops;
    uops.reserve(uop_count);
    for (unsigned uop = 0; uop < uop_count; ++uop) {
        const std::uint64_t lq_pointer =
            store ? lq_offset : queue_cursor;
        const std::uint64_t sq_pointer =
            store ? queue_cursor : sq_offset;
        uops.push_back(memblock::VectorMemoryTransaction{
            .store = store,
            .address = address,
            .reference_pages = reference_pages_for(address),
            .stride = stride,
            .addressing = static_cast<memblock::VectorAddressingMode>(
                shape.addressing),
            .eew = static_cast<std::uint8_t>(shape.eew),
            .vsew = static_cast<std::uint8_t>(shape.vsew),
            .vl = static_cast<std::uint8_t>(vl),
            .vstart = static_cast<std::uint8_t>(vstart),
            .vm = !masked,
            .mask_bits = mask_bits,
            .mask_high = mask_high,
            .vma = vma,
            .vta = vta,
            .rob = memblock::rob_pointer_value(rob),
            .rob_flag = memblock::rob_pointer_flag(rob),
            .lq = memblock::lq_pointer_value(lq_pointer),
            .lq_flag = memblock::lq_pointer_flag(lq_pointer),
            .sq = memblock::sq_pointer_value(sq_pointer),
            .sq_flag = memblock::sq_pointer_flag(sq_pointer),
            .pdest = store ? std::uint8_t{0}
                : static_cast<std::uint8_t>(first_pdest + uop),
            .lane = uop % memblock::kVectorMemoryLanes,
            .flow_num = static_cast<std::uint8_t>(flow_num),
            .ftq_ptr = random() & 0x3fU,
            .ftq_offset = static_cast<std::uint8_t>(random() & 7U),
            .vlmul = vlmul,
            .vuop_idx = static_cast<std::uint8_t>(uop),
            .last_uop = uop + 1U == uop_count,
        });
        auto &transaction = uops.back();
        // Decode copies one frontend instruction into every memory uop.
        // Preserve the draws above so unrelated seeded choices stay unchanged.
        transaction.ftq_ptr = uops.front().ftq_ptr;
        transaction.ftq_offset = uops.front().ftq_offset;
        for (auto &byte : transaction.data) {
            byte = static_cast<unsigned char>(random());
        }
        if (indexed) {
            const unsigned global_first = uop * flow_num;
            const unsigned index_register_base =
                global_first / index_elements_per_vreg *
                index_elements_per_vreg;
            std::copy_n(indices.bytes.begin() + index_register_base * index_bytes,
                index_group_bytes_per_uop, transaction.index.begin());
        }
        queue_cursor += flow_num;
    }
    coverage.vector_operand_chains[store ? 1U : 0U] +=
        chain_indexed_vector_sources(uops, environment.memory());
    return uops;
}

void MixedScenario::sample_constrained_vector_instruction(
    bool store, const RandomVectorShape &shape,
    const std::vector<memblock::VectorMemoryTransaction> &uops)
{
    ++constraint_coverage.vector_directions[store ? 1U : 0U];
    ++constraint_coverage.vector_addressing[shape.addressing];
    ++constraint_coverage.vector_eews[shape.eew];
    ++constraint_coverage.vector_sews[shape.vsew];
    ++constraint_coverage.vector_lmuls[shape.lmul_log2 + 3];
    ++constraint_coverage.vector_emuls[shape.emul_log2 + 3];
    constraint_coverage.sample_vector_shape(
        store ? 1U : 0U, shape.addressing, shape.eew, shape.vsew,
        shape.lmul_log2);
    const auto &policy = uops.front();
    const unsigned vector_bytes = shape.lmul_log2 < 0
        ? 16U >> static_cast<unsigned>(-shape.lmul_log2)
        : 16U << static_cast<unsigned>(shape.lmul_log2);
    const unsigned vlmax = vector_bytes >> shape.vsew;
    ++constraint_coverage.vector_masked[policy.vm ? 0U : 1U];
    ++constraint_coverage.vector_vma[policy.vma ? 1U : 0U];
    ++constraint_coverage.vector_vta[policy.vta ? 1U : 0U];
    ++constraint_coverage.vector_partial_vl[
        policy.vl < vlmax ? 1U : 0U];
    ++constraint_coverage.vector_nonzero_vstart[
        policy.vstart != 0 ? 1U : 0U];
    constraint_coverage.vector_mask_agnostic += !store && policy.vma &&
        std::any_of(uops.begin(), uops.end(), [](const auto &uop) {
            return memblock::vector_mask_inactive_elements(uop) != 0;
        });
    constraint_coverage.vector_tail_agnostic += !store && policy.vta &&
        std::any_of(uops.begin(), uops.end(), [](const auto &uop) {
            return memblock::vector_tail_elements(uop) != 0;
        });
    ++constraint_coverage.vector_shape_operations;
    constraint_coverage.vector_uops += uops.size();
    constraint_coverage.vector_multi_uop += uops.size() > 1U;
    if (!store) {
        ++constraint_coverage.vector_fof_presence[
            policy.fault_only_first ? 1U : 0U];
    }
}

bool MixedScenario::issue_random_vector_instruction(
    const std::vector<memblock::VectorMemoryTransaction> &uops)
{
    const bool store = uops.front().store;
    std::size_t begin = 0;
    while (begin < uops.size()) {
        std::size_t end = begin;
        unsigned window_flows = 0;
        const unsigned capacity = store
            ? memblock::kStoreQueueEntries - memblock::kSqEnqueueHeadroom
            : memblock::kVirtualLoadQueueEntries -
                memblock::kLqEnqueueHeadroom;
        while (end < uops.size() &&
            window_flows + uops[end].flow_num <= capacity) {
            window_flows += uops[end].flow_num;
            ++end;
        }
        const std::vector<memblock::VectorMemoryTransaction> window(
            uops.begin() + begin, uops.begin() + end);
        bool requires_misaligned_head = false;
        bool requires_store_pending = false;
        for (const auto &uop : window) {
            if (!validate_vector_translation(uop)) return false;
            const unsigned element_bytes =
                1U << memblock::vector_data_eew(uop);
            const std::uint16_t active =
                memblock::active_vector_elements(uop);
            for (unsigned element = 0;
                 element < 16U / element_bytes; ++element) {
                if (((active >> element) & 1U) == 0) {
                    continue;
                }
                const std::uint64_t element_address =
                    memblock::vector_element_virtual_address(uop, element);
                requires_misaligned_head |= !store &&
                    ((element_address & 0xfU) + element_bytes) > 16U;
                requires_store_pending |= store &&
                    (element_address & (element_bytes - 1U)) != 0;
            }
        }
        if (((requires_misaligned_head || requires_store_pending) &&
                !environment.set_rob_head(
                    window.front().rob, window.front().rob_flag))) {
            return false;
        }
        for (const auto &uop : window) {
            environment.expect_vector(uop);
            if (!environment.enqueue_vector(uop)) {
                return false;
            }
        }
        for (const unsigned issue_index : vector_uop_issue_order(window, random)) {
            const std::size_t global_index = begin + issue_index;
            if (global_index != 0 && vector_load_depends_on_previous(
                    uops[global_index - 1U], uops[global_index])) {
                if (!environment.run_until_vector_complete_with_replays(
                        uops[global_index - 1U], constrained_completion_timeout)) {
                    return false;
                }
                ++coverage.vector_operand_chains[2];
            }
            if (!environment.issue_vector(
                    window[issue_index], constrained_completion_timeout)) {
                return false;
            }
        }
        if (requires_store_pending &&
            (!environment.run_cycles(32) ||
                !environment.pulse_pending_store(
                    window.front().rob, window.front().rob_flag))) {
            return false;
        }
        if (!environment.run_until_vector_complete_with_replays(
                window, constrained_completion_timeout,
                requires_store_pending)) {
            return false;
        }
        if (store) {
            for (const auto &uop : window) {
                if (!environment.commit_vector_store(uop, 8192)) {
                    return false;
                }
            }
            if (!environment.run_until_queues_retired(8192)) {
                return false;
            }
        } else if (!environment.run_until_lq_retired(8192)) {
            return false;
        }
        for (const auto &uop : window) {
            coverage.sample(uop);
        }
        begin = end;
    }
    ++actions;
    return true;
}

bool MixedScenario::issue_random_vector_segment_fof(
    bool first_fault, unsigned stage1_mode,
    const RandomVectorSegmentShape &shape)
{
    const unsigned eew = shape.eew;
    const unsigned sew = shape.vsew;
    const int lmul_log2 = shape.lmul_log2;
    const int emul_log2 = shape.emul_log2;
    const unsigned nf_index = shape.nf_index;
    const unsigned fields = nf_index + 2U;
    const unsigned element_bytes = 1U << eew;
    const unsigned uops_per_field =
        1U << static_cast<unsigned>(std::max(emul_log2, 0));
    const unsigned data_uops_count = fields * uops_per_field;
    const unsigned bytes_per_data_uop = emul_log2 < 0
        ? 16U >> static_cast<unsigned>(-emul_log2)
        : 16U;
    const unsigned flow_num = bytes_per_data_uop >> eew;
    const unsigned elements_per_vreg = 16U >> eew;
    const unsigned fault_element = first_fault ? 0U : 1U;
    const std::uint8_t vlmul = static_cast<std::uint8_t>(
        lmul_log2 < 0 ? lmul_log2 + 8 : lmul_log2);
    const std::uint64_t address = vector_fof_virtual_base + 0x1000U -
        (first_fault ? 0U : fields * element_bytes);
    const std::uint64_t physical_address =
        vector_fof_physical_base + 0x1000U - fields * element_bytes;
    {
        std::ostringstream detail;
        detail << (first_fault
                          ? "random-vector-segment-fof-first-fault"
                          : "random-vector-segment-fof-later-fault")
               << ":mode=" << stage1_mode << ":eew=" << eew
               << ":sew=" << sew << ":lmul=" << lmul_log2
               << ":emul=" << emul_log2 << ":nf=" << fields;
        phase = detail.str();
    }
    if (!environment.run_until_all_complete(8192) ||
        !environment.run_until_queues_retired(8192)) {
        phase += ":initial-drain";
        return false;
    }

    for (unsigned field = 0; field < fields; ++field) {
        const std::uint64_t first_address =
            address + field * element_bytes;
        const auto first_reference =
            memblock::reference_page_walk_permissions(
                environment.memory(), random_stage1_roots[stage1_mode],
                first_address, page_mode(stage1_mode));
        if (first_fault) {
            if (first_reference.walk.translated) {
                phase += ":reference-first-segment";
                return false;
            }
            continue;
        }
        const std::uint64_t expected_physical =
            physical_address + field * element_bytes;
        const bool permitted = !first_reference.leaf_permissions ||
            memblock::reference_memory_access_permitted(
                memblock::ReferenceMemoryAccess::load,
                *first_reference.leaf_permissions,
                memblock::ReferencePrivilegeMode::supervisor,
                false, false);
        const auto next_reference =
            memblock::reference_page_walk_permissions(
                environment.memory(), random_stage1_roots[stage1_mode],
                first_address + fields * element_bytes,
                page_mode(stage1_mode));
        if (!first_reference.walk.translated || !permitted ||
            first_reference.walk.physical_address != expected_physical ||
            next_reference.walk.translated) {
            phase += ":reference-segment-boundary";
            return false;
        }
    }

    const std::uint64_t data_writebacks_before =
        environment.vector_load_writebacks();
    const std::uint64_t fix_writebacks_before =
        environment.vector_fof_fix_writebacks();
    const std::uint64_t page_fault_writebacks_before =
        environment.vector_page_fault_writebacks();
    const std::uint64_t rob = rob_offset++;
    const std::uint8_t first_pdest = random_destination_block(
        random, memblock::kVectorPhysicalRegisters, data_uops_count);
    std::vector<memblock::VectorMemoryTransaction> data_uops;
    data_uops.reserve(data_uops_count);
    for (unsigned uop = 0; uop < data_uops_count; ++uop) {
        const unsigned field = uop / uops_per_field;
        const unsigned vd_index = uop % uops_per_field;
        std::uint16_t unspecified_elements = 0;
        for (unsigned element = 0; element < elements_per_vreg;
             ++element) {
            const unsigned global_element =
                vd_index * elements_per_vreg + element;
            if (global_element >= fault_element &&
                global_element < 2U) {
                unspecified_elements |= static_cast<std::uint16_t>(
                    1U << element);
            }
        }
        data_uops.push_back(memblock::VectorMemoryTransaction{
            .segment = true,
            .address = address,
            .oracle_address = first_fault
                ? std::nullopt
                : std::optional<std::uint64_t>{physical_address},
            .eew = static_cast<std::uint8_t>(eew),
            .vsew = static_cast<std::uint8_t>(sew),
            .vl = 2,
            .rob = memblock::rob_pointer_value(rob),
            .rob_flag = memblock::rob_pointer_flag(rob),
            .lq = memblock::lq_pointer_value(lq_offset),
            .lq_flag = memblock::lq_pointer_flag(lq_offset),
            .sq = memblock::sq_pointer_value(sq_offset),
            .sq_flag = memblock::sq_pointer_flag(sq_offset),
            .pdest = static_cast<std::uint8_t>(first_pdest + uop),
            .lane = 0,
            .flow_num = static_cast<std::uint8_t>(flow_num),
            .expected_exception_mask = 0,
            .allowed_additional_exception_mask = first_fault
                ? memblock::kExceptionLoadPageFault : 0,
            .expected_trigger = first_fault
                ? std::nullopt
                : std::optional<std::uint8_t>{
                      memblock::kVectorWritebackTriggerNone},
            .expected_writeback_vstart = first_fault
                ? std::optional<std::uint8_t>{0} : std::nullopt,
            .vlmul = vlmul,
            .vuop_idx = static_cast<std::uint8_t>(uop),
            .last_uop = false,
            .nf = static_cast<std::uint8_t>(fields - 1U),
            .fault_only_first = true,
            .is_vleff = false,
            .vec_wen = true,
            .fof_unspecified_elements = unspecified_elements,
        });
        auto &data_uop = data_uops.back();
        for (auto &byte : data_uop.data) {
            byte = static_cast<unsigned char>(random());
        }
        auto expected_data = data_uop.data;
        for (unsigned element = 0; element < elements_per_vreg;
             ++element) {
            const unsigned global_element =
                vd_index * elements_per_vreg + element;
            if (global_element >= fault_element ||
                global_element >= 2U) {
                continue;
            }
            const std::uint64_t expected_address =
                memblock::vector_element_address(data_uop, element);
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                expected_data[element * element_bytes + byte] =
                    environment.memory().read_byte(
                        expected_address + byte);
            }
        }
        environment.expect_vector_data(data_uop, expected_data);
        if (!environment.issue_vector(
                data_uop, constrained_completion_timeout)) {
            phase += ":data-issue";
            return false;
        }
    }

    auto fix_vl_uop = data_uops.front();
    fix_vl_uop.pdest = random_destination_block(
        random, memblock::kVlPhysicalRegisters, 1U);
    fix_vl_uop.expected_exception_mask = 0;
    fix_vl_uop.allowed_additional_exception_mask = 0;
    fix_vl_uop.expected_trigger =
        memblock::kVectorWritebackTriggerNone;
    fix_vl_uop.expected_vl = first_fault ? 2U : 1U;
    fix_vl_uop.vuop_idx =
        static_cast<std::uint8_t>(data_uops_count);
    fix_vl_uop.last_uop = true;
    fix_vl_uop.vl_wen = true;
    fix_vl_uop.vec_wen = false;
    fix_vl_uop.fof_unspecified_elements = 0;
    environment.expect_vector_data(fix_vl_uop, {});
    if (!environment.issue_vector(
            fix_vl_uop, constrained_completion_timeout) ||
        !environment.run_until_vector_complete(
            constrained_completion_timeout)) {
        phase += ":completion";
        return false;
    }
    const std::uint64_t data_uops_observed =
        environment.vector_load_writebacks() - data_writebacks_before;
    if (data_uops_observed != data_uops_count ||
        environment.vector_fof_fix_writebacks() !=
            fix_writebacks_before + 1U ||
        (first_fault && environment.vector_page_fault_writebacks() ==
                page_fault_writebacks_before)) {
        std::ostringstream detail;
        detail << phase << ":terminal-accounting"
               << ":data=" << data_uops_observed << '/'
               << data_uops_count
               << ":fix="
               << environment.vector_fof_fix_writebacks() -
                fix_writebacks_before;
        phase = detail.str();
        return false;
    }
    if (first_fault && !environment.redirect_after(
                           fix_vl_uop.rob, fix_vl_uop.rob_flag,
                           true, true)) {
        phase += ":trap-redirect";
        return false;
    }
    for (const auto &data_uop : data_uops) {
        coverage.sample(data_uop);
    }
    constraint_coverage.sample_vector_segment_fof(
        first_fault, stage1_mode, eew, sew, lmul_log2, nf_index,
        static_cast<unsigned>(data_uops_observed));
    ++actions;
    return true;
}

} // namespace memblock::scenarios::mixed
