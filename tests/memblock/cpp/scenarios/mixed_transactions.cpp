#include "mixed/scenario.hpp"

namespace memblock::scenarios::mixed {

memblock::LoadTransaction MixedScenario::make_load(std::uint64_t address, memblock::LoadOp op,
    unsigned lane)
{
    const std::uint64_t rob = rob_offset++;
    const std::uint64_t lq = lq_offset++;
    auto transaction = memblock::LoadTransaction{
        .address = address,
        .op = op,
        .rob = memblock::rob_pointer_value(rob),
        .rob_flag = memblock::rob_pointer_flag(rob),
        .lq = memblock::lq_pointer_value(lq),
        .lq_flag = memblock::lq_pointer_flag(lq),
        .sq = memblock::sq_pointer_value(sq_offset),
        .sq_flag = memblock::sq_pointer_flag(sq_offset),
        .pdest = static_cast<std::uint8_t>(1 + random() % 255),
        .lane = lane,
    };
    transaction.predecode_rvc = (rob % 4U) == 0 || (random() & 3U) == 0;
    transaction.ftq_ptr = random() & 0x3fU;
    transaction.ftq_offset = static_cast<std::uint8_t>(random() & 7U);
    transaction.store_set_hit = (rob % 5U) == 0 || (random() % 5U) == 0;
    transaction.load_wait_bit = rob != 0 &&
        ((rob % 7U) == 0 || (random() % 7U) == 0);
    // Strict wait is coupled to the backend's older-store scheduling;
    // exercise that bit in the isolated metadata contract instead of
    // creating an unbounded wait cycle in a mixed burst.
    transaction.load_wait_strict = false;
    transaction.wait_for_rob_flag = transaction.load_wait_bit &&
        memblock::rob_pointer_flag(rob - 1);
    transaction.wait_for_rob_value = transaction.load_wait_bit
        ? memblock::rob_pointer_value(rob - 1)
        : 0;
    return transaction;
}

memblock::StoreTransaction MixedScenario::make_store(std::uint64_t address, std::uint64_t data,
    memblock::StoreOp op, unsigned address_lane,
    unsigned data_lane)
{
    const std::uint64_t rob = rob_offset++;
    const std::uint64_t sq = sq_offset++;
    auto transaction = memblock::StoreTransaction{
        .address = address,
        .data = data,
        .op = op,
        .rob = memblock::rob_pointer_value(rob),
        .rob_flag = memblock::rob_pointer_flag(rob),
        .sq = memblock::sq_pointer_value(sq),
        .sq_flag = memblock::sq_pointer_flag(sq),
        .address_lane = address_lane,
        .data_lane = data_lane,
    };
    return transaction;
}

memblock::PrefetchTransaction MixedScenario::make_prefetch(std::uint64_t address, memblock::PrefetchOp op,
    unsigned lane)
{
    const std::uint64_t rob = rob_offset++;
    const std::uint64_t lq = lq_offset++;
    return memblock::PrefetchTransaction{
        .address = address,
        .op = op,
        .rob = memblock::rob_pointer_value(rob),
        .rob_flag = memblock::rob_pointer_flag(rob),
        .lq = memblock::lq_pointer_value(lq),
        .lq_flag = memblock::lq_pointer_flag(lq),
        .sq = memblock::sq_pointer_value(sq_offset),
        .sq_flag = memblock::sq_pointer_flag(sq_offset),
        .lane = lane,
    };
}

memblock::VectorMemoryTransaction MixedScenario::make_vector(bool store, std::uint64_t address, unsigned eew,
    unsigned lane,
    memblock::VectorAddressingMode addressing)
{
    const std::uint64_t rob = rob_offset++;
    const std::uint64_t lq = lq_offset;
    const std::uint64_t sq = sq_offset;
    const std::uint8_t flow_num = addressing ==
            memblock::VectorAddressingMode::unit_stride
        ? 2
        : static_cast<std::uint8_t>(16U >> eew);
    if (store) {
        sq_offset += flow_num;
    } else {
        lq_offset += flow_num;
    }
    memblock::VectorMemoryTransaction transaction{
        .store = store,
        .address = address,
        .addressing = addressing,
        .eew = static_cast<std::uint8_t>(eew),
        .vl = static_cast<std::uint8_t>(16U >> eew),
        .rob = memblock::rob_pointer_value(rob),
        .rob_flag = memblock::rob_pointer_flag(rob),
        .lq = memblock::lq_pointer_value(lq),
        .lq_flag = memblock::lq_pointer_flag(lq),
        .sq = memblock::sq_pointer_value(sq),
        .sq_flag = memblock::sq_pointer_flag(sq),
        .pdest = static_cast<std::uint8_t>(1 + random() % 255),
        .lane = lane,
        .flow_num = flow_num,
    };
    for (auto &byte : transaction.data) {
        byte = static_cast<unsigned char>(random());
    }
    transaction.ftq_ptr = random() & 0x3fU;
    transaction.ftq_offset = static_cast<std::uint8_t>(random() & 7U);
    // VLSU flow_num and vtype/LMUL are coupled by the backend.  The
    // standalone MemBlock boundary has no vset* state, so keep the legal
    // baseline LMUL here; dedicated vector tests may override vlmul with
    // a matching flow shape.
    transaction.vlmul = 0;
    return transaction;
}

memblock::AtomicTransaction MixedScenario::make_atomic(std::uint64_t address, memblock::AtomicOp op,
    std::uint64_t data)
{
    const std::uint64_t rob = rob_offset++;
    return memblock::AtomicTransaction{
        .address = address,
        .op = op,
        .data = data,
        .rob = memblock::rob_pointer_value(rob),
        .rob_flag = memblock::rob_pointer_flag(rob),
        .sq = memblock::sq_pointer_value(sq_offset),
        .sq_flag = memblock::sq_pointer_flag(sq_offset),
        .pdest = static_cast<std::uint8_t>(1 + random() % 255),
        .address_lane = static_cast<unsigned>(random() % 2),
        .data_lane = static_cast<unsigned>(random() % 2),
    };
}

unsigned int MixedScenario::random_delay(unsigned minimum, unsigned maximum)
{
    return minimum + static_cast<unsigned>(random() % (maximum - minimum + 1));
}

std::uint64_t MixedScenario::random_bare_address(std::uint64_t region, unsigned alignment,
    unsigned span)
{
    const std::uint64_t offset = random() % span;
    return region + ((offset / alignment) * alignment);
}

std::uint64_t MixedScenario::constrained_locality_line()
{
    const unsigned locality = constraints.choose_locality(random());
    ++constraint_coverage.locality[locality];
    std::uint64_t line = 0;
    if (locality == 0) {
        line = random() % 32;
    } else if (locality == 1) {
        line = random() % 512;
    } else {
        // Traverse a working set eight times larger than L1D without
        // making the line sequence trivially ascending.
        line = (constrained_cold_line++ * 4051U) % 8192;
    }
    return line;
}

std::uint64_t MixedScenario::constrained_cacheable_address(
    unsigned alignment, const TranslationContext &translation,
    const TranslationLeafTopology &leaf,
    std::optional<bool> force_misaligned)
{
    std::uint64_t line = constrained_locality_line();
    std::uint64_t region_base = cache0_base + 0x10000;
    if (translation.regime == RandomConstraints::translation_stage1 &&
        leaf.stage1_napot) {
        region_base = stage1_napot_base;
    } else if (translation.regime == RandomConstraints::translation_nested) {
        constexpr std::array<std::uint64_t, 4> nested_regions{{
            0,
            nested_g_napot_base,
            nested_vs_napot_base,
            nested_both_napot_base,
        }};
        const std::uint64_t nested_region =
            nested_regions[leaf.nested_index()];
        if (nested_region != 0) {
            region_base = nested_region;
        }
    }
    if (region_base != cache0_base + 0x10000) {
        // Leave 4 KiB on either side for negative strides, indexed
        // elements, and multi-uop segment address expansion.
        line = 64 + line % 896;
    }
    std::uint64_t address = region_base + line * 64;
    const bool selected_misaligned = force_misaligned
        ? *force_misaligned
        : random() % 1000 < constraints.misaligned_per_mille;
    const bool misaligned = alignment > 1 && selected_misaligned;
    if (misaligned && alignment > 1) {
        address += 1 + random() % (alignment - 1);
    } else {
        address += ((random() % 64) / alignment) * alignment;
    }
    return address;
}

std::uint64_t MixedScenario::constrained_hypervisor_address(
    unsigned alignment, const TranslationLeafTopology &leaf,
    unsigned spvp, unsigned pbmt, bool pma_device, bool misaligned,
    unsigned pmp_relation)
{
    const unsigned topology = leaf.nested_index();
    if (pmp_relation != RandomConstraints::hypervisor_pmp_none) {
        const std::uint64_t base =
            hypervisor_pmp_virtual_base(spvp, topology);
        const std::uint64_t first = base + hypervisor_pmp_region_offset;
        const std::uint64_t above = first + hypervisor_pmp_region_size;
        switch (pmp_relation) {
        case RandomConstraints::hypervisor_pmp_first:
            return first;
        case RandomConstraints::hypervisor_pmp_last:
            return above - alignment;
        case RandomConstraints::hypervisor_pmp_below:
            return first - alignment;
        case RandomConstraints::hypervisor_pmp_above:
            return above;
        case RandomConstraints::hypervisor_pmp_cross_lower:
            return first - alignment / 2;
        case RandomConstraints::hypervisor_pmp_cross_upper:
            return above - alignment / 2;
        default:
            return std::uint64_t{0};
        }
    }
    const std::uint64_t base = pma_device
        ? hypervisor_pma_device_virtual_base(spvp, topology)
        : hypervisor_virtual_base(spvp, pbmt, topology);
    std::uint64_t address = base +
        constrained_locality_line() * 64;
    if (misaligned) {
        address += 1 + random() % (alignment - 1);
    } else {
        address += ((random() % 64) / alignment) * alignment;
    }
    return address;
}

unsigned long MixedScenario::constrained_window_address(std::uint64_t base,
    unsigned alignment)
{
    if (alignment <= 1) {
        return base + random() % 64;
    }
    const bool misaligned =
        random() % 1000 < constraints.misaligned_per_mille;
    if (!misaligned) {
        return base + ((random() % 64) / alignment) * alignment;
    }
    unsigned offset = 0;
    do {
        offset = random() % 64;
    } while ((offset & (alignment - 1)) == 0);
    return base + offset;
}

bool MixedScenario::issue_load(const memblock::LoadTransaction &transaction,
    std::optional<std::uint64_t> expected,
    bool count_action)
{
    if (expected) {
        environment.expect_load_data(transaction, *expected);
    } else {
        environment.expect_load(transaction);
    }
    if (!environment.enqueue_load(transaction) ||
        !environment.issue_load(transaction) ||
        !environment.run_until_all_complete(constrained_completion_timeout)) {
        return false;
    }
    if (count_action) {
        coverage.sample(transaction);
        ++actions;
    }
    return true;
}

bool MixedScenario::issue_vector(
    const memblock::VectorMemoryTransaction &transaction,
    std::optional<std::array<unsigned char, 16>> expected)
{
    if (expected) {
        environment.expect_vector_data(transaction, *expected);
    } else {
        environment.expect_vector(transaction);
    }
    bool requires_misaligned_head = false;
    bool requires_store_pending = false;
    const unsigned element_bytes = 1U << transaction.eew;
    const std::uint16_t active =
        memblock::active_vector_elements(transaction);
    for (unsigned element = 0;
         element < 16U / element_bytes; ++element) {
        if (((active >> element) & 1U) == 0) {
            continue;
        }
        const std::uint64_t address =
            memblock::vector_element_address(transaction, element);
        requires_misaligned_head |= !transaction.store &&
            ((address & 0xfU) + element_bytes) > 16U;
        requires_store_pending |= transaction.store &&
            (address & (element_bytes - 1)) != 0;
    }
    if (((requires_misaligned_head || requires_store_pending) &&
            !environment.set_rob_head(
                transaction.rob, transaction.rob_flag)) ||
        !environment.enqueue_vector(transaction) ||
        !environment.issue_vector(transaction) ||
        (requires_store_pending &&
            (!environment.run_cycles(32) ||
                !environment.pulse_pending_store(
                    transaction.rob, transaction.rob_flag))) ||
        !environment.run_until_vector_complete_with_replays(
            transaction, constrained_completion_timeout,
            requires_store_pending)) {
        return false;
    }
    coverage.sample(transaction);
    ++actions;
    return true;
}

bool MixedScenario::issue_store(const memblock::StoreTransaction &transaction,
    bool data_first,
    bool hold_pending_store)
{
    environment.expect_store(transaction);
    const bool issued = environment.enqueue_store(
                            transaction,
                            memblock::lq_pointer_value(lq_offset)) &&
        (data_first
                ? environment.issue_store_data(transaction) &&
                    environment.issue_store_address(transaction)
                : environment.issue_store_address(transaction) &&
                    environment.issue_store_data(transaction));
    if (!issued || !environment.run_until_store_complete_with_replay(
                       transaction, constrained_completion_timeout,
                       hold_pending_store)) {
        return false;
    }
    coverage.sample(transaction, data_first);
    ++actions;
    return true;
}

std::uint64_t MixedScenario::scalar_forward_value(std::uint64_t data, unsigned size,
    bool is_unsigned)
{
    const unsigned bits = size * 8;
    const std::uint64_t mask = bits == 64
        ? ~std::uint64_t{0}
        : (std::uint64_t{1} << bits) - 1;
    const std::uint64_t raw = data & mask;
    return is_unsigned ? raw : memblock::sign_extend(raw, bits);
}

std::uint64_t MixedScenario::atomic_writeback(std::uint64_t old_value, bool word)
{
    return word
        ? memblock::sign_extend(static_cast<std::uint32_t>(old_value), 32)
        : old_value;
}

unsigned long MixedScenario::atomic_result(memblock::AtomicOp op, std::uint64_t old_value,
    std::uint64_t operand)
{
    const auto replace_word = [&](std::uint32_t value) {
        return (old_value & ~std::uint64_t{0xffffffff}) | value;
    };
    const std::uint32_t old_word = static_cast<std::uint32_t>(old_value);
    const std::uint32_t operand_word = static_cast<std::uint32_t>(operand);
    switch (op) {
    case memblock::AtomicOp::amoadd_w:
        return replace_word(old_word + operand_word);
    case memblock::AtomicOp::amoxor_w:
        return replace_word(old_word ^ operand_word);
    case memblock::AtomicOp::amoand_w:
        return replace_word(old_word & operand_word);
    case memblock::AtomicOp::amoor_w:
        return replace_word(old_word | operand_word);
    case memblock::AtomicOp::amoswap_w:
        return replace_word(operand_word);
    case memblock::AtomicOp::amomin_w:
        return replace_word(
            static_cast<std::int32_t>(old_word) <
                    static_cast<std::int32_t>(operand_word)
                ? old_word : operand_word);
    case memblock::AtomicOp::amomax_w:
        return replace_word(
            static_cast<std::int32_t>(old_word) >
                    static_cast<std::int32_t>(operand_word)
                ? old_word : operand_word);
    case memblock::AtomicOp::amominu_w:
        return replace_word(std::min(old_word, operand_word));
    case memblock::AtomicOp::amomaxu_w:
        return replace_word(std::max(old_word, operand_word));
    case memblock::AtomicOp::amoadd_d:
        return old_value + operand;
    case memblock::AtomicOp::amoxor_d:
        return old_value ^ operand;
    case memblock::AtomicOp::amoand_d:
        return old_value & operand;
    case memblock::AtomicOp::amoor_d:
        return old_value | operand;
    case memblock::AtomicOp::amoswap_d:
        return operand;
    case memblock::AtomicOp::amomin_d:
        return static_cast<std::int64_t>(old_value) <
                static_cast<std::int64_t>(operand)
            ? old_value : operand;
    case memblock::AtomicOp::amomax_d:
        return static_cast<std::int64_t>(old_value) >
                static_cast<std::int64_t>(operand)
            ? old_value : operand;
    case memblock::AtomicOp::amominu_d:
        return std::min(old_value, operand);
    case memblock::AtomicOp::amomaxu_d:
        return std::max(old_value, operand);
    default:
        return old_value;
    }
}

std::array<unsigned char, 16> MixedScenario::overlay_vector_store(const memblock::VectorMemoryTransaction &store,
    const memblock::VectorMemoryTransaction &load)
{
    auto expected = environment.memory().expected_vector_load(load);
    std::unordered_map<std::uint64_t, unsigned char> forwarded;
    const unsigned store_bytes = 1U << store.eew;
    const unsigned store_elements = 16U / store_bytes;
    const std::uint16_t store_active = memblock::active_vector_elements(store);
    for (unsigned element = 0; element < store_elements; ++element) {
        if (((store_active >> element) & 1U) == 0) {
            continue;
        }
        const std::uint64_t address =
            memblock::vector_element_address(store, element);
        for (unsigned byte = 0; byte < store_bytes; ++byte) {
            forwarded[address + byte] =
                store.data[element * store_bytes + byte];
        }
    }
    const unsigned load_bytes = 1U << load.eew;
    const unsigned load_elements = 16U / load_bytes;
    const std::uint16_t load_active = memblock::active_vector_elements(load);
    for (unsigned element = 0; element < load_elements; ++element) {
        if (((load_active >> element) & 1U) == 0) {
            continue;
        }
        const std::uint64_t address =
            memblock::vector_element_address(load, element);
        for (unsigned byte = 0; byte < load_bytes; ++byte) {
            const auto value = forwarded.find(address + byte);
            if (value != forwarded.end()) {
                expected[element * load_bytes + byte] = value->second;
            }
        }
    }
    return expected;
}

void MixedScenario::randomize_vector_addressing(memblock::VectorMemoryTransaction &transaction)
{
    const unsigned element_bytes = 1U << transaction.eew;
    const unsigned elements = 16U >> transaction.eew;
    if (transaction.addressing == memblock::VectorAddressingMode::strided) {
        if (transaction.store) {
            const std::array<std::int64_t, 5> stride_classes{{
                -static_cast<std::int64_t>(2U * element_bytes),
                -static_cast<std::int64_t>(element_bytes),
                static_cast<std::int64_t>(element_bytes),
                static_cast<std::int64_t>(2U * element_bytes),
                static_cast<std::int64_t>(4U * element_bytes),
            }};
            transaction.stride =
                stride_classes[random() % stride_classes.size()];
        } else {
            const std::array<std::int64_t, 6> stride_classes{{
                -static_cast<std::int64_t>(2U * element_bytes),
                -static_cast<std::int64_t>(element_bytes),
                0,
                static_cast<std::int64_t>(element_bytes),
                static_cast<std::int64_t>(2U * element_bytes),
                static_cast<std::int64_t>(4U * element_bytes),
            }};
            transaction.stride =
                stride_classes[random() % stride_classes.size()];
        }
        return;
    }
    if (transaction.addressing != memblock::VectorAddressingMode::indexed_unordered &&
        transaction.addressing != memblock::VectorAddressingMode::indexed_ordered) {
        return;
    }
    std::vector<unsigned> offset_slots(256U / element_bytes);
    std::iota(offset_slots.begin(), offset_slots.end(), 0U);
    if (transaction.store) {
        std::shuffle(offset_slots.begin(), offset_slots.end(), random);
    }
    for (unsigned element = 0; element < elements; ++element) {
        const std::uint64_t slot = transaction.store
            ? offset_slots[element]
            : random() % offset_slots.size();
        const std::uint64_t offset = slot * element_bytes;
        for (unsigned byte = 0; byte < element_bytes; ++byte) {
            transaction.index[element * element_bytes + byte] =
                static_cast<unsigned char>(offset >> (8 * byte));
        }
    }
}

} // namespace memblock::scenarios::mixed
