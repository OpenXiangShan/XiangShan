#pragma once

#include "generated_port_defaults.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <deque>
#include <iomanip>
#include <iostream>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace memblock {

constexpr unsigned kScalarLoadLanes = 3;
constexpr unsigned kScalarStoreLanes = 2;
constexpr unsigned kVectorMemoryLanes = 2;
constexpr unsigned kVirtualLoadQueueEntries = 72;
constexpr unsigned kStoreQueueEntries = 56;
// LsqEnqCtrl deliberately reserves the maximum dispatch width before it
// asserts canAccept. Keep the software driver below the same watermark even
// though the generated MemBlock top does not expose canAccept as an output.
constexpr unsigned kLqEnqueueHeadroom = 6;
constexpr unsigned kSqEnqueueHeadroom = 4;
constexpr unsigned kEnqueueSettleCycles = 16;
constexpr unsigned kRobEntries = 160;
constexpr std::uint64_t kFuTypeLoad = std::uint64_t{1} << 15;
constexpr std::uint64_t kFuTypeStore = std::uint64_t{1} << 16;
constexpr std::uint64_t kFuTypeAtomic = std::uint64_t{1} << 17;
constexpr std::uint64_t kFuTypeVectorLoad = std::uint64_t{1} << 31;
constexpr std::uint64_t kFuTypeVectorStore = std::uint64_t{1} << 32;
constexpr std::uint16_t kVectorLoadUnitStride = 0x080;
constexpr std::uint16_t kVectorLoadIndexedUnordered = 0x0a0;
constexpr std::uint16_t kVectorLoadStrided = 0x0c0;
constexpr std::uint16_t kVectorLoadIndexedOrdered = 0x0e0;
constexpr std::uint16_t kVectorStoreUnitStride = 0x100;
constexpr std::uint16_t kVectorStoreIndexedUnordered = 0x120;
constexpr std::uint16_t kVectorStoreStrided = 0x140;
constexpr std::uint16_t kVectorStoreIndexedOrdered = 0x160;
constexpr std::uint64_t kDefaultMemoryBase = 0x80000000ULL;
// TriggerAction.None is the architectural no-trigger encoding.  Zero is a
// breakpoint action, so leaving this field at zero would inject a breakpoint
// into every software-generated uop.
constexpr std::uint8_t kTriggerNone = 15;
// The standalone MemBlock issue adapters do not carry trigger in the vector
// or scalar-store issue payload.  Their writeback paths therefore expose the
// zero-initialized action for an untriggered operation; keep that boundary
// distinct from scalar-load/prefetch, whose uop carries TriggerAction.None.
constexpr std::uint8_t kStoreWritebackTriggerNone = 0;
constexpr std::uint8_t kVectorWritebackTriggerNone = 0;

constexpr std::uint32_t kExceptionLoadAddressMisaligned = 1U << 4;
constexpr std::uint32_t kExceptionBreakpoint = 1U << 3;
constexpr std::uint32_t kExceptionLoadAccessFault = 1U << 5;
constexpr std::uint32_t kExceptionStoreAddressMisaligned = 1U << 6;
constexpr std::uint32_t kExceptionStoreAccessFault = 1U << 7;
constexpr std::uint32_t kExceptionLoadPageFault = 1U << 13;
constexpr std::uint32_t kExceptionStorePageFault = 1U << 15;
constexpr std::uint32_t kExceptionHardwareError = 1U << 19;
constexpr std::uint32_t kExceptionLoadGuestPageFault = 1U << 21;
constexpr std::uint32_t kExceptionStoreGuestPageFault = 1U << 23;

constexpr std::uint8_t circular_pointer_value(
    std::uint64_t offset, unsigned entries)
{
    return static_cast<std::uint8_t>(offset % entries);
}

constexpr bool circular_pointer_flag(std::uint64_t offset, unsigned entries)
{
    return ((offset / entries) & 1U) != 0;
}

constexpr std::uint8_t lq_pointer_value(std::uint64_t offset)
{
    return circular_pointer_value(offset, kVirtualLoadQueueEntries);
}

constexpr bool lq_pointer_flag(std::uint64_t offset)
{
    return circular_pointer_flag(offset, kVirtualLoadQueueEntries);
}

constexpr std::uint8_t sq_pointer_value(std::uint64_t offset)
{
    return circular_pointer_value(offset, kStoreQueueEntries);
}

constexpr bool sq_pointer_flag(std::uint64_t offset)
{
    return circular_pointer_flag(offset, kStoreQueueEntries);
}

constexpr std::uint8_t rob_pointer_value(std::uint64_t offset)
{
    return circular_pointer_value(offset, kRobEntries);
}

constexpr bool rob_pointer_flag(std::uint64_t offset)
{
    return circular_pointer_flag(offset, kRobEntries);
}

enum class LoadOp : std::uint16_t {
    lb = 0,
    lh = 1,
    lw = 2,
    ld = 3,
    lbu = 4,
    lhu = 5,
    lwu = 6,
};

enum class StoreOp : std::uint16_t {
    sb = 0,
    sh = 1,
    sw = 2,
    sd = 3,
    cbo_zero = 7,
};

enum class PrefetchOp : std::uint16_t {
    instruction = 0x8,
    read = 0x9,
    write = 0xa,
};

enum class AtomicOp : std::uint16_t {
    lr_w = 0x02,
    sc_w = 0x06,
    amoswap_w = 0x0a,
    amoadd_w = 0x0e,
    amoxor_w = 0x12,
    amoand_w = 0x16,
    amoor_w = 0x1a,
    amomin_w = 0x1e,
    amomax_w = 0x22,
    amominu_w = 0x26,
    amomaxu_w = 0x2a,
    amocas_w = 0x2e,
    lr_d = 0x03,
    sc_d = 0x07,
    amoswap_d = 0x0b,
    amoadd_d = 0x0f,
    amoxor_d = 0x13,
    amoand_d = 0x17,
    amoor_d = 0x1b,
    amomin_d = 0x1f,
    amomax_d = 0x23,
    amominu_d = 0x27,
    amomaxu_d = 0x2b,
    amocas_d = 0x2f,
};

struct LoadTransaction {
    std::uint64_t address = kDefaultMemoryBase;
    std::optional<std::uint64_t> oracle_address;
    LoadOp op = LoadOp::ld;
    std::uint8_t rob = 0;
    bool rob_flag = false;
    std::uint8_t lq = 0;
    bool lq_flag = false;
    std::uint8_t sq = 0;
    bool sq_flag = false;
    std::uint8_t pdest = 0;
    unsigned lane = 0;
    std::uint32_t expected_exception_mask = 0;
    bool check_data_on_exception = false;
    bool rf_wen = true;
    bool fp_wen = false;
    std::uint32_t input_exception_mask = 0;
    std::uint8_t input_trigger = kTriggerNone;
    bool input_flush_pipe = false;
    std::uint8_t expected_trigger = kTriggerNone;
    bool predecode_rvc = false;
    std::uint64_t ftq_ptr = 0;
    std::uint8_t ftq_offset = 0;
    bool store_set_hit = false;
    bool wait_for_rob_flag = false;
    std::uint8_t wait_for_rob_value = 0;
    bool load_wait_bit = false;
    bool load_wait_strict = false;
    // Optional debug-class expectations for writeback sidebands.
    std::optional<bool> expected_debug_is_mmio;
    std::optional<bool> expected_debug_is_ncio;
    std::optional<bool> expected_debug_is_perf_cnt;
};

struct StoreTransaction {
    std::uint64_t address = kDefaultMemoryBase;
    std::optional<std::uint64_t> oracle_address;
    std::uint64_t data = 0;
    StoreOp op = StoreOp::sd;
    std::uint8_t rob = 0;
    bool rob_flag = false;
    std::uint8_t sq = 0;
    bool sq_flag = false;
    unsigned address_lane = 0;
    unsigned data_lane = 0;
    std::uint32_t expected_exception_mask = 0;
    std::uint32_t input_exception_mask = 0;
    std::uint8_t input_trigger = kTriggerNone;
    bool input_flush_pipe = false;
    std::optional<std::uint8_t> expected_trigger;
    // Store-address writeback exposes the memory-class debug bits.  Keep
    // these optional because existing callers may not model the translation
    // class of an address yet.
    std::optional<bool> expected_debug_is_mmio;
    std::optional<bool> expected_debug_is_ncio;
};

struct AtomicTransaction {
    std::uint64_t address = kDefaultMemoryBase;
    AtomicOp op = AtomicOp::amoadd_d;
    std::uint64_t data = 0;
    std::uint64_t compare = 0;
    std::uint8_t rob = 0;
    bool rob_flag = false;
    std::uint8_t sq = 0;
    bool sq_flag = false;
    std::uint8_t pdest = 0;
    unsigned address_lane = 0;
    unsigned data_lane = 0;
};

struct PrefetchTransaction {
    std::uint64_t address = kDefaultMemoryBase;
    std::optional<std::uint64_t> oracle_address;
    PrefetchOp op = PrefetchOp::read;
    std::uint8_t rob = 0;
    bool rob_flag = false;
    std::uint8_t lq = 0;
    bool lq_flag = false;
    std::uint8_t sq = 0;
    bool sq_flag = false;
    unsigned lane = 0;
    std::uint32_t input_exception_mask = 0;
    std::uint8_t input_trigger = kTriggerNone;
    bool input_flush_pipe = false;
    std::uint8_t expected_trigger = kTriggerNone;
    std::optional<bool> expected_debug_is_mmio;
    std::optional<bool> expected_debug_is_ncio;
    std::optional<bool> expected_debug_is_perf_cnt;
};

enum class VectorAddressingMode : std::uint8_t {
    unit_stride,
    strided,
    indexed_unordered,
    indexed_ordered,
};

struct VectorMemoryTransaction {
    bool store = false;
    std::uint64_t address = kDefaultMemoryBase;
    std::optional<std::uint64_t> oracle_address;
    std::array<unsigned char, 16> data{};
    std::array<unsigned char, 16> index{};
    std::int64_t stride = 0;
    VectorAddressingMode addressing = VectorAddressingMode::unit_stride;
    std::uint8_t eew = 0;
    std::uint8_t vl = 16;
    std::uint8_t vstart = 0;
    bool vm = true;
    std::uint16_t mask_bits = 0xffff;
    bool vma = false;
    bool vta = false;
    std::uint8_t rob = 0;
    bool rob_flag = false;
    std::uint8_t lq = 0;
    bool lq_flag = false;
    std::uint8_t sq = 0;
    bool sq_flag = false;
    std::uint8_t pdest = 0;
    unsigned lane = 0;
    std::uint8_t flow_num = 2;
    bool is_part_replay = false;
    std::uint16_t replay_mask = 0;
    std::uint8_t replay_mb_index = 0;
    std::uint32_t expected_exception_mask = 0;
    std::uint32_t input_exception_mask = 0;
    std::uint8_t input_trigger = kTriggerNone;
    bool input_flush_pipe = false;
    std::optional<std::uint8_t> expected_trigger;
    std::uint64_t ftq_ptr = 0;
    std::uint8_t ftq_offset = 0;
    std::uint8_t vlmul = 0;
    // Lane 0 exposes all three debug classes.  Lane 1 is intentionally
    // checked only when a caller has an explicit expectation because those
    // generated sideband ports are pruned in this top-level build.
    std::optional<bool> expected_debug_is_mmio;
    std::optional<bool> expected_debug_is_ncio;
    std::optional<bool> expected_debug_is_perf_cnt;
};

inline std::uint16_t vector_fu_op_type(const VectorMemoryTransaction &transaction)
{
    switch (transaction.addressing) {
    case VectorAddressingMode::unit_stride:
        return transaction.store ? kVectorStoreUnitStride : kVectorLoadUnitStride;
    case VectorAddressingMode::strided:
        return transaction.store ? kVectorStoreStrided : kVectorLoadStrided;
    case VectorAddressingMode::indexed_unordered:
        return transaction.store
            ? kVectorStoreIndexedUnordered
            : kVectorLoadIndexedUnordered;
    case VectorAddressingMode::indexed_ordered:
        return transaction.store
            ? kVectorStoreIndexedOrdered
            : kVectorLoadIndexedOrdered;
    }
    throw std::logic_error("unknown vector addressing mode");
}

inline std::uint64_t vector_element_address(
    const VectorMemoryTransaction &transaction, unsigned element)
{
    const std::uint64_t base = transaction.oracle_address.value_or(
        transaction.address);
    const unsigned element_bytes = 1U << transaction.eew;
    switch (transaction.addressing) {
    case VectorAddressingMode::unit_stride:
        return base + element * element_bytes;
    case VectorAddressingMode::strided:
        {
            const std::int64_t delta =
                transaction.stride * static_cast<std::int64_t>(element);
            return delta >= 0
                ? base + static_cast<std::uint64_t>(delta)
                : base - static_cast<std::uint64_t>(-(delta + 1)) - 1U;
        }
    case VectorAddressingMode::indexed_unordered:
    case VectorAddressingMode::indexed_ordered: {
        const unsigned index_bytes = 1U << transaction.eew;
        std::uint64_t offset = 0;
        for (unsigned byte = 0; byte < index_bytes; ++byte) {
            offset |= std::uint64_t{transaction.index[element * index_bytes + byte]}
                      << (8 * byte);
        }
        return base + offset;
    }
    }
    throw std::logic_error("unknown vector addressing mode");
}

inline std::uint16_t active_vector_elements(
    const VectorMemoryTransaction &transaction)
{
    const unsigned element_count = 16U >> transaction.eew;
    std::uint16_t result = 0;
    for (unsigned element = 0; element < element_count; ++element) {
        const bool in_range = element >= transaction.vstart &&
                              element < transaction.vl;
        const bool enabled = transaction.vm ||
                             ((transaction.mask_bits >> element) & 1U) != 0;
        if (in_range && enabled) {
            result |= static_cast<std::uint16_t>(1U << element);
        }
    }
    return result;
}

inline std::uint64_t sign_extend(std::uint64_t value, unsigned bits)
{
    if (bits == 64) {
        return value;
    }
    const std::uint64_t sign = std::uint64_t{1} << (bits - 1);
    const std::uint64_t mask = (std::uint64_t{1} << bits) - 1;
    return ((value & mask) ^ sign) - sign;
}

struct RobIdentity {
    std::uint8_t value;
    bool flag;
    bool operator==(const RobIdentity &other) const
    {
        return value == other.value && flag == other.flag;
    }
};

struct RobIdentityHash {
    std::size_t operator()(const RobIdentity &identity) const
    {
        return static_cast<std::size_t>(identity.value) |
               (static_cast<std::size_t>(identity.flag) << 8);
    }
};

inline RobIdentity rob_identity(std::uint8_t value, bool flag)
{
    return RobIdentity{value, flag};
}

class SparseMemory {
public:
    explicit SparseMemory(SparseMemory *write_mirror = nullptr)
        : write_mirror_(write_mirror)
    {}

    void write_byte(std::uint64_t address, std::uint8_t value)
    {
        bytes_[address] = value;
        if (write_mirror_ != nullptr) {
            write_mirror_->write_reference_byte(address, value);
        }
    }

    void write_reference_byte(std::uint64_t address, std::uint8_t value)
    {
        bytes_[address] = value;
    }

    std::uint8_t read_byte(std::uint64_t address) const
    {
        const auto it = bytes_.find(address);
        return it == bytes_.end() ? 0 : it->second;
    }

    void write_u64(std::uint64_t address, std::uint64_t value)
    {
        for (unsigned byte = 0; byte < 8; ++byte) {
            write_byte(
                address + byte,
                static_cast<std::uint8_t>(value >> (8 * byte)));
        }
    }

    std::uint64_t read_u64(std::uint64_t address) const
    {
        std::uint64_t value = 0;
        for (unsigned byte = 0; byte < 8; ++byte) {
            value |= std::uint64_t{read_byte(address + byte)} << (8 * byte);
        }
        return value;
    }

    void fill_incrementing(std::uint64_t address, std::size_t size, std::uint8_t first)
    {
        for (std::size_t offset = 0; offset < size; ++offset) {
            write_byte(address + offset, static_cast<std::uint8_t>(first + offset));
        }
    }

    std::vector<unsigned char> read_beat(std::uint64_t address, std::size_t size) const
    {
        std::vector<unsigned char> result(size);
        for (std::size_t offset = 0; offset < size; ++offset) {
            result[offset] = read_byte(address + offset);
        }
        return result;
    }

    std::uint64_t expected_load(std::uint64_t address, LoadOp op) const
    {
        const unsigned encoding = static_cast<unsigned>(op);
        const unsigned size = std::uint64_t{1} << (encoding & 3U);
        std::uint64_t value = 0;
        for (unsigned offset = 0; offset < size; ++offset) {
            value |= std::uint64_t{read_byte(address + offset)} << (8 * offset);
        }
        const bool is_unsigned = (encoding & 4U) != 0;
        return is_unsigned ? value : sign_extend(value, size * 8);
    }

    std::array<unsigned char, 16> expected_vector_load(
        const VectorMemoryTransaction &transaction) const
    {
        std::array<unsigned char, 16> result = transaction.data;
        const unsigned element_bytes = 1U << transaction.eew;
        const std::uint16_t active = active_vector_elements(transaction);
        for (unsigned element = 0; element < 16U / element_bytes; ++element) {
            if (((active >> element) & 1U) == 0) {
                continue;
            }
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                const unsigned offset = element * element_bytes + byte;
                result[offset] = read_byte(
                    vector_element_address(transaction, element) + byte);
            }
        }
        return result;
    }

private:
    SparseMemory *write_mirror_;
    std::unordered_map<std::uint64_t, std::uint8_t> bytes_;
};

struct ReferenceStageWalkResult {
    bool translated = false;
    std::uint64_t physical_address = 0;
    std::uint64_t faulting_pte_address = 0;
    unsigned fault_level = 0;
};

// RISC-V satp/hgatp mode encodings used by the MemBlock CSR interface.
// Keeping the encoding here avoids accidentally testing a mode with a
// different number of page-table levels than the DUT.
enum class ReferencePageMode : std::uint8_t {
    bare = 0,
    sv39 = 8,
    sv48 = 9,
};

inline unsigned reference_page_levels(ReferencePageMode mode)
{
    if (mode == ReferencePageMode::bare) {
        return 0;
    }
    return mode == ReferencePageMode::sv48 ? 4U : 3U;
}

inline bool reference_canonical_virtual_address(
    std::uint64_t address, ReferencePageMode mode)
{
    if (mode == ReferencePageMode::bare) {
        return true;
    }
    if (mode == ReferencePageMode::sv48) {
        const bool sign = ((address >> 47) & 1U) != 0;
        const std::uint64_t upper = address >> 48;
        return upper == (sign ? 0xffffU : 0U);
    }
    const bool sign = ((address >> 38) & 1U) != 0;
    const std::uint64_t upper = address >> 39;
    return upper == (sign ? 0x1ffffffU : 0U);
}

inline bool reference_gpa_in_range(
    std::uint64_t address, ReferencePageMode mode)
{
    if (mode == ReferencePageMode::bare) {
        return true;
    }
    // Sv39x4 exposes a 41-bit GPA and Sv48x4 exposes a 50-bit GPA.
    const unsigned bits = mode == ReferencePageMode::sv48 ? 50U : 41U;
    return (address >> bits) == 0;
}

inline std::uint64_t reference_pte_ppn(std::uint64_t pte)
{
    return (pte >> 10) & ((std::uint64_t{1} << 44) - 1);
}

inline bool reference_pte_is_invalid(std::uint64_t pte)
{
    const bool valid = (pte & 1U) != 0;
    const bool readable = (pte & 2U) != 0;
    const bool writable = (pte & 4U) != 0;
    return !valid || (writable && !readable);
}

inline bool reference_pte_is_leaf(std::uint64_t pte)
{
    return (pte & (2U | 8U)) != 0;
}

inline std::uint64_t reference_leaf_address(
    std::uint64_t pte, std::uint64_t input_address, unsigned level)
{
    const unsigned low_bits = 12 + 9 * level;
    const std::uint64_t low_mask = (std::uint64_t{1} << low_bits) - 1;
    return ((reference_pte_ppn(pte) << 12) & ~low_mask) |
           (input_address & low_mask);
}

inline ReferenceStageWalkResult reference_page_walk(
    const SparseMemory &memory,
    std::uint64_t root_page_table,
    std::uint64_t input_address,
    ReferencePageMode mode,
    bool x4 = false)
{
    if (mode == ReferencePageMode::bare) {
        return {true, input_address, 0, 0};
    }
    if ((root_page_table & (x4 ? 0x3fffULL : 0xfffULL)) != 0) {
        return {false, 0, root_page_table, 0};
    }
    if (x4 ? !reference_gpa_in_range(input_address, mode)
           : !reference_canonical_virtual_address(input_address, mode)) {
        return {false, 0, 0, 0};
    }
    const unsigned top_level = reference_page_levels(mode) - 1U;
    std::uint64_t table = root_page_table;
    for (int level = static_cast<int>(top_level); level >= 0; --level) {
        const unsigned shift = 12 + 9 * static_cast<unsigned>(level);
        const std::uint64_t index_mask =
            x4 && static_cast<unsigned>(level) == top_level ? 0x7ff : 0x1ff;
        const std::uint64_t index = (input_address >> shift) & index_mask;
        const std::uint64_t pte_address = table + index * 8;
        const std::uint64_t pte = memory.read_u64(pte_address);
        if (reference_pte_is_invalid(pte)) {
            return {false, 0, pte_address, static_cast<unsigned>(level)};
        }
        if (reference_pte_is_leaf(pte)) {
            const unsigned lower_ppn_bits = 9 * static_cast<unsigned>(level);
            if (lower_ppn_bits != 0 &&
                (reference_pte_ppn(pte) &
                 ((std::uint64_t{1} << lower_ppn_bits) - 1)) != 0) {
                return {false, 0, pte_address, static_cast<unsigned>(level)};
            }
            return {
                true,
                reference_leaf_address(pte, input_address, level),
                0,
                0,
            };
        }
        if (level == 0) {
            return {false, 0, pte_address, 0};
        }
        table = reference_pte_ppn(pte) << 12;
    }
    return {};
}

inline ReferenceStageWalkResult reference_sv39_walk(
    const SparseMemory &memory,
    std::uint64_t root_page_table,
    std::uint64_t input_address,
    bool sv39x4 = false)
{
    return reference_page_walk(
        memory, root_page_table, input_address,
        ReferencePageMode::sv39, sv39x4);
}

inline ReferenceStageWalkResult reference_sv48_walk(
    const SparseMemory &memory,
    std::uint64_t root_page_table,
    std::uint64_t input_address,
    bool sv48x4 = false)
{
    return reference_page_walk(
        memory, root_page_table, input_address,
        ReferencePageMode::sv48, sv48x4);
}

struct ReferenceTwoStageWalkResult {
    bool translated = false;
    std::uint64_t physical_address = 0;
    bool guest_page_fault = false;
    bool stage1_page_fault = false;
    std::uint64_t faulting_guest_physical_address = 0;
    bool is_for_vs_nonleaf_pte = false;
};

inline ReferenceTwoStageWalkResult reference_two_stage_walk(
    const SparseMemory &memory,
    std::uint64_t vs_root_page_table,
    std::uint64_t g_root_page_table,
    std::uint64_t guest_virtual_address,
    ReferencePageMode vs_mode = ReferencePageMode::sv39,
    ReferencePageMode g_mode = ReferencePageMode::sv39)
{
    if (vs_mode != ReferencePageMode::bare &&
        !reference_canonical_virtual_address(guest_virtual_address, vs_mode)) {
        return {};
    }
    std::uint64_t guest_physical_address = guest_virtual_address;
    if (vs_mode != ReferencePageMode::bare) {
        const unsigned top_level = reference_page_levels(vs_mode) - 1U;
        std::uint64_t vs_table_gpa = vs_root_page_table;
        for (int level = static_cast<int>(top_level); level >= 0; --level) {
            const unsigned shift = 12 + 9 * static_cast<unsigned>(level);
            const std::uint64_t index = (guest_virtual_address >> shift) & 0x1ff;
            const std::uint64_t pte_gpa = vs_table_gpa + index * 8;
            const auto pte_translation = reference_page_walk(
                memory, g_root_page_table, pte_gpa, g_mode, true);
            if (!pte_translation.translated) {
                return {false, 0, true, false, pte_gpa, true};
            }

            const std::uint64_t pte = memory.read_u64(
                pte_translation.physical_address);
            if (reference_pte_is_invalid(pte)) {
                return {false, 0, false, true, pte_gpa, false};
            }
            if (reference_pte_is_leaf(pte)) {
                const unsigned lower_ppn_bits = 9 * static_cast<unsigned>(level);
                if (lower_ppn_bits != 0 &&
                    (reference_pte_ppn(pte) &
                     ((std::uint64_t{1} << lower_ppn_bits) - 1)) != 0) {
                    return {false, 0, false, true, pte_gpa, false};
                }
                guest_physical_address = reference_leaf_address(
                    pte, guest_virtual_address, static_cast<unsigned>(level));
                break;
            }
            if (level == 0) {
                return {false, 0, false, true, pte_gpa, false};
            }
            vs_table_gpa = reference_pte_ppn(pte) << 12;
        }
    }
    if (g_mode == ReferencePageMode::bare) {
        return {true, guest_physical_address, false, false, 0, false};
    }
    const auto final_translation = reference_page_walk(
        memory, g_root_page_table, guest_physical_address, g_mode, true);
    if (!final_translation.translated) {
        return {false, 0, true, false, guest_physical_address, false};
    }
    return {true, final_translation.physical_address, false, false, 0, false};
}

enum class ResponseLatencyProfile {
    compact,
    spec,
};

struct ResponseLatencyStats {
    std::array<std::uint64_t, 4> buckets{};
    std::uint64_t samples = 0;
    std::uint64_t total_cycles = 0;
    unsigned max_cycles = 0;

    void sample(unsigned cycles)
    {
        const std::size_t bucket = cycles < 20 ? 0 : cycles < 40 ? 1 :
            cycles < 100 ? 2 : 3;
        ++buckets[bucket];
        ++samples;
        total_cycles += cycles;
        max_cycles = std::max(max_cycles, cycles);
    }
};

inline unsigned sample_response_latency(
    ResponseLatencyProfile profile, std::uint64_t random,
    std::uint64_t sample_index = 4)
{
    if (profile == ResponseLatencyProfile::compact) {
        return 1 + static_cast<unsigned>(random % 4);
    }

    // Give short smoke runs a deterministic floor in every latency class;
    // subsequent samples follow the calibrated distribution.
    if (sample_index < 4) {
        const std::array<unsigned, 4> floor{{12, 25, 60, 100}};
        return sample_index == 3
            ? floor[3] + static_cast<unsigned>(random % 301)
            : floor[sample_index];
    }

    // Final-measurement MSHR A-to-D latency from 4,206 SPEC checkpoints:
    // about 74.1% <20, 14.4% 20-39, 5.1% 40-99, and 6.4% >=100.
    const unsigned percentile = static_cast<unsigned>(random % 10000);
    if (percentile < 7410) {
        return 8 + static_cast<unsigned>((random >> 16) % 12);
    }
    if (percentile < 8853) {
        return 20 + static_cast<unsigned>((random >> 16) % 20);
    }
    if (percentile < 9359) {
        return 40 + static_cast<unsigned>((random >> 16) % 60);
    }
    return 100 + static_cast<unsigned>((random >> 16) % 301);
}

class TileLinkMemoryAgent {
public:
    TileLinkMemoryAgent(SparseMemory &memory, const SparseMemory &reference_memory)
        : memory_(memory), reference_memory_(reference_memory)
    {}

    void configure_backpressure(
        std::uint64_t seed, bool enabled,
        ResponseLatencyProfile latency_profile = ResponseLatencyProfile::compact)
    {
        random_state_ = seed == 0 ? 1 : seed;
        random_backpressure_ = enabled;
        latency_profile_ = latency_profile;
        response_latency_stats_ = {};
        force_a_stall_ = enabled;
    }

    void inject_next_response_error(bool denied, bool corrupt)
    {
        inject_denied_ = denied;
        inject_corrupt_ = corrupt;
    }

    void drive(UTMemBlock &dut)
    {
        const bool accept_a = !random_backpressure_ ||
                              (!force_a_stall_ && (next_random() & 3U) != 0);
        const bool accept_c = !random_backpressure_ || (next_random() & 3U) != 0;
        dut.auto_inner_dcache_client_out_a_ready.ImmSet(accept_a);
        dut.auto_inner_dcache_client_out_c_ready.ImmSet(accept_c);
        if (d_beats_.empty()) {
            dut.auto_inner_dcache_client_out_d_valid.ImmSet(std::uint64_t{0});
            d_presenting_ = false;
            return;
        }

        if (!d_presenting_) {
            if (d_gap_ != 0) {
                --d_gap_;
                ++response_delay_cycles_;
                dut.auto_inner_dcache_client_out_d_valid.ImmSet(std::uint64_t{0});
                return;
            }
            d_presenting_ = true;
        }

        const DBeat &beat = d_beats_.front();
        dut.auto_inner_dcache_client_out_d_bits_opcode.ImmSet(beat.opcode);
        dut.auto_inner_dcache_client_out_d_bits_param.ImmSet(beat.param);
        dut.auto_inner_dcache_client_out_d_bits_size.ImmSet(beat.size);
        dut.auto_inner_dcache_client_out_d_bits_source.ImmSet(beat.source);
        dut.auto_inner_dcache_client_out_d_bits_sink.ImmSet(beat.sink);
        dut.auto_inner_dcache_client_out_d_bits_denied.ImmSet(beat.denied);
        dut.auto_inner_dcache_client_out_d_bits_echo_isKeyword.ImmSet(beat.keyword);
        auto bytes = beat.data;
        dut.auto_inner_dcache_client_out_d_bits_data.ImmSetBytes(bytes);
        dut.auto_inner_dcache_client_out_d_bits_corrupt.ImmSet(beat.corrupt);
        dut.auto_inner_dcache_client_out_d_valid.ImmSet(std::uint64_t{1});
    }

    void capture_before_tick(UTMemBlock &dut)
    {
        const bool a_valid = dut.auto_inner_dcache_client_out_a_valid.B();
        const bool a_ready = dut.auto_inner_dcache_client_out_a_ready.B();
        if (a_valid && !a_ready) {
            ++request_stall_cycles_;
            force_a_stall_ = false;
        }
        a_fire_ = a_valid && a_ready;
        if (a_fire_) {
            captured_a_ = ARequest{
                static_cast<std::uint8_t>(dut.auto_inner_dcache_client_out_a_bits_opcode.U()),
                static_cast<std::uint8_t>(dut.auto_inner_dcache_client_out_a_bits_param.U()),
                static_cast<std::uint8_t>(dut.auto_inner_dcache_client_out_a_bits_size.U()),
                static_cast<std::uint8_t>(dut.auto_inner_dcache_client_out_a_bits_source.U()),
                dut.auto_inner_dcache_client_out_a_bits_address.U(),
                dut.auto_inner_dcache_client_out_a_bits_echo_isKeyword.B(),
            };
            if (captured_a_->opcode != 4 && captured_a_->opcode != 6 &&
                captured_a_->opcode != 7) {
                error_ = "unsupported DCache TileLink A opcode";
            }
            if (captured_a_->size > 6) {
                error_ = "oversized DCache TileLink A request";
            }
        }
        c_fire_ = dut.auto_inner_dcache_client_out_c_valid.B() &&
                  dut.auto_inner_dcache_client_out_c_ready.B();
        if (c_fire_) {
            captured_c_ = CRequest{
                static_cast<std::uint8_t>(dut.auto_inner_dcache_client_out_c_bits_opcode.U()),
                static_cast<std::uint8_t>(dut.auto_inner_dcache_client_out_c_bits_param.U()),
                static_cast<std::uint8_t>(dut.auto_inner_dcache_client_out_c_bits_size.U()),
                static_cast<std::uint8_t>(dut.auto_inner_dcache_client_out_c_bits_source.U()),
                dut.auto_inner_dcache_client_out_c_bits_address.U(),
                dut.auto_inner_dcache_client_out_c_bits_echo_isKeyword.B(),
                dut.auto_inner_dcache_client_out_c_bits_data.GetBytes(),
            };
            if (captured_c_->opcode != 6 && captured_c_->opcode != 7) {
                error_ = "unsupported DCache TileLink C opcode";
            }
            if (captured_c_->size > 6) {
                error_ = "oversized DCache TileLink C request";
            }
        }
        d_fire_ = !d_beats_.empty() &&
                  dut.auto_inner_dcache_client_out_d_valid.B() &&
                  dut.auto_inner_dcache_client_out_d_ready.B();
        if (d_fire_) {
            const DBeat &expected = d_beats_.front();
            const auto actual_data =
                dut.auto_inner_dcache_client_out_d_bits_data.GetBytes();
            if (dut.auto_inner_dcache_client_out_d_bits_opcode.U() != expected.opcode ||
                dut.auto_inner_dcache_client_out_d_bits_param.U() != expected.param ||
                dut.auto_inner_dcache_client_out_d_bits_size.U() != expected.size ||
                dut.auto_inner_dcache_client_out_d_bits_source.U() != expected.source ||
                dut.auto_inner_dcache_client_out_d_bits_sink.U() != expected.sink ||
                dut.auto_inner_dcache_client_out_d_bits_echo_isKeyword.B() != expected.keyword ||
                dut.auto_inner_dcache_client_out_d_bits_denied.B() != expected.denied ||
                dut.auto_inner_dcache_client_out_d_bits_corrupt.B() != expected.corrupt ||
                actual_data != expected.data) {
                error_ = "DCache TileLink D response identity or payload mismatch";
            }
        }
    }

    void update_after_tick()
    {
        if (d_fire_) {
            d_beats_.pop_front();
            d_presenting_ = false;
            d_gap_ = d_beats_.empty() ? 0 : d_beats_.front().delay_before;
        }
        if (a_fire_ && captured_a_) {
            respond(*captured_a_);
            ++request_count_;
        }
        if (c_fire_ && captured_c_) {
            accept_release(*captured_c_);
        }
        a_fire_ = false;
        c_fire_ = false;
        d_fire_ = false;
        captured_a_.reset();
        captured_c_.reset();
    }

    bool ok() const { return error_.empty(); }
    const std::string &error() const { return error_; }
    std::uint64_t request_count() const { return request_count_; }
    std::uint64_t request_stall_cycles() const { return request_stall_cycles_; }
    std::uint64_t response_delay_cycles() const { return response_delay_cycles_; }
    const ResponseLatencyStats &response_latency_stats() const
    {
        return response_latency_stats_;
    }
    std::uint64_t release_count() const { return release_count_; }
    std::uint64_t release_data_count() const { return release_data_count_; }
    std::uint64_t release_data_verified_count() const
    {
        return release_data_verified_count_;
    }
    void expect_release_line(
        std::uint64_t base, const std::vector<unsigned char> &bytes)
    {
        if (bytes.empty() || bytes.size() % kBeatBytes != 0) {
            error_ = "expected ReleaseData line is not beat sized";
            return;
        }
        expected_release_lines_[base] = bytes;
    }

    bool verify_memory_bytes(
        std::uint64_t address, const std::vector<unsigned char> &expected)
    {
        for (std::size_t index = 0; index < expected.size(); ++index) {
            if (memory_.read_byte(address + index) != expected[index]) {
                std::ostringstream message;
                message << "post-commit store readback mismatch address=0x"
                        << std::hex << (address + index)
                        << " expected=0x" << static_cast<unsigned>(expected[index])
                        << " actual=0x"
                        << static_cast<unsigned>(memory_.read_byte(address + index));
                error_ = message.str();
                return false;
            }
        }
        return true;
    }

    bool check_memory_bytes(
        std::uint64_t address, const std::vector<unsigned char> &expected)
    {
        return verify_memory_bytes(address, expected);
    }

private:
    static constexpr std::size_t kBeatBytes = 32;

    struct ARequest {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t address;
        bool keyword;
    };

    struct DBeat {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::uint16_t sink;
        bool keyword;
        std::vector<unsigned char> data;
        bool denied = false;
        bool corrupt = false;
        unsigned delay_before = 0;
    };

    struct CRequest {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t address;
        bool keyword;
        std::vector<unsigned char> data;
    };

    struct ReleaseDataState {
        std::uint64_t base;
        std::uint8_t size;
        std::uint8_t source;
        bool keyword;
        std::size_t beats;
        std::size_t received = 0;
    };

    void respond(const ARequest &request)
    {
        const bool denied = inject_denied_;
        const bool corrupt = inject_corrupt_;
        inject_denied_ = false;
        inject_corrupt_ = false;
        const std::uint64_t transfer_bytes = std::uint64_t{1} << request.size;
        const std::uint64_t base = request.address & ~(transfer_bytes - 1);
        switch (request.opcode) {
        case 4: { // Get -> AccessAckData
            const std::uint64_t beat_base = request.address & ~(kBeatBytes - 1);
            push_response(DBeat{
                1, 0, request.size, request.source, 0, request.keyword,
                memory_.read_beat(beat_base, kBeatBytes), denied, corrupt,
            }, true);
            break;
        }
        case 6: { // AcquireBlock -> GrantData
            const std::uint8_t cap = request.param == 0 ? 1 : 0;
            const std::size_t beats = static_cast<std::size_t>(
                transfer_bytes > kBeatBytes ? transfer_bytes / kBeatBytes : 1);
            for (std::size_t beat = 0; beat < beats; ++beat) {
                const std::size_t memory_beat = request.keyword ? beat ^ 1U : beat;
                push_response(DBeat{
                    5, cap, request.size, request.source, 1, request.keyword,
                    memory_.read_beat(base + memory_beat * kBeatBytes, kBeatBytes),
                    denied, corrupt,
                }, beat == 0);
            }
            break;
        }
        case 7: // AcquirePerm -> Grant
            push_response(DBeat{
                4, 0, request.size, request.source, 1, request.keyword,
                std::vector<unsigned char>(kBeatBytes, 0),
            }, true);
            break;
        default: {
            std::ostringstream message;
            message << "unsupported DCache TileLink A opcode "
                    << static_cast<unsigned>(request.opcode);
            error_ = message.str();
            break;
        }
        }
    }

    void accept_release(const CRequest &request)
    {
        if (request.opcode == 6) { // Release -> ReleaseAck
            push_response(DBeat{
                6, 0, request.size, request.source, 0, request.keyword,
                std::vector<unsigned char>(kBeatBytes, 0),
            }, true);
            ++release_count_;
            return;
        }
        if (request.opcode != 7) { // ReleaseData -> ReleaseAck
            std::ostringstream message;
            message << "unsupported DCache TileLink C opcode "
                    << static_cast<unsigned>(request.opcode);
            error_ = message.str();
            return;
        }

        const std::uint64_t transfer_bytes = std::uint64_t{1} << request.size;
        const std::uint64_t base = request.address & ~(transfer_bytes - 1);
        const std::size_t beats = static_cast<std::size_t>(
            transfer_bytes > kBeatBytes ? transfer_bytes / kBeatBytes : 1);
        if (!release_data_) {
            release_data_ = ReleaseDataState{
                base, request.size, request.source, request.keyword, beats, 0};
        }
        if (release_data_->base != base || release_data_->size != request.size ||
            release_data_->source != request.source ||
            release_data_->beats != beats) {
            error_ = "interleaved or inconsistent DCache ReleaseData transaction";
            return;
        }
        const auto expected = expected_release_lines_.find(base);
        const bool has_expected_line = expected != expected_release_lines_.end();
        const std::size_t offset = release_data_->received * kBeatBytes;
        if (has_expected_line && offset + kBeatBytes > expected->second.size()) {
            error_ = "ReleaseData exceeded expected immutable line image";
            return;
        }
        for (std::size_t byte = 0; byte < kBeatBytes; ++byte) {
            const unsigned char expected_byte = has_expected_line
                ? expected->second[offset + byte]
                : reference_memory_.read_byte(base + offset + byte);
            if (request.data.at(byte) != expected_byte) {
                std::ostringstream message;
                message << "ReleaseData byte mismatch base=0x" << std::hex
                        << base << " beat=" << std::dec
                        << release_data_->received << " byte=" << byte
                        << " expected=0x" << std::hex
                        << static_cast<unsigned>(expected_byte)
                        << " actual=0x"
                        << static_cast<unsigned>(request.data.at(byte));
                error_ = message.str();
                return;
            }
        }
        for (std::size_t byte = 0; byte < kBeatBytes; ++byte) {
            memory_.write_byte(
                base + release_data_->received * kBeatBytes + byte,
                request.data.at(byte));
        }
        ++release_data_->received;
        if (release_data_->received == release_data_->beats) {
            push_response(DBeat{
                6, 0, request.size, request.source, 0, request.keyword,
                std::vector<unsigned char>(kBeatBytes, 0),
            }, true);
            release_data_.reset();
            ++release_data_verified_count_;
            if (has_expected_line) {
                expected_release_lines_.erase(base);
            }
            ++release_count_;
            ++release_data_count_;
        }
    }

    std::uint64_t next_random()
    {
        random_state_ ^= random_state_ << 13;
        random_state_ ^= random_state_ >> 7;
        random_state_ ^= random_state_ << 17;
        return random_state_;
    }

    unsigned response_delay(bool first_beat)
    {
        if (!random_backpressure_) {
            return 0;
        }
        if (!first_beat) {
            return static_cast<unsigned>(next_random() % 4);
        }
        const unsigned delay = sample_response_latency(
            latency_profile_, next_random(), response_latency_stats_.samples);
        response_latency_stats_.sample(delay);
        return delay;
    }

    void push_response(DBeat response, bool first_beat)
    {
        const bool was_empty = d_beats_.empty();
        response.delay_before = response_delay(first_beat);
        d_beats_.push_back(std::move(response));
        if (was_empty) {
            d_presenting_ = false;
            d_gap_ = d_beats_.front().delay_before;
        }
    }

    SparseMemory &memory_;
    const SparseMemory &reference_memory_;
    std::deque<DBeat> d_beats_;
    std::optional<ARequest> captured_a_;
    std::optional<CRequest> captured_c_;
    std::optional<ReleaseDataState> release_data_;
    std::unordered_map<std::uint64_t, std::vector<unsigned char>>
        expected_release_lines_;
    bool a_fire_ = false;
    bool c_fire_ = false;
    bool d_fire_ = false;
    std::uint64_t request_count_ = 0;
    std::uint64_t release_count_ = 0;
    std::uint64_t release_data_count_ = 0;
    std::uint64_t release_data_verified_count_ = 0;
    std::uint64_t random_state_ = 1;
    unsigned d_gap_ = 0;
    bool random_backpressure_ = false;
    ResponseLatencyProfile latency_profile_ = ResponseLatencyProfile::compact;
    ResponseLatencyStats response_latency_stats_;
    bool force_a_stall_ = false;
    bool d_presenting_ = false;
    bool inject_denied_ = false;
    bool inject_corrupt_ = false;
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
    std::string error_;
};

class PtwMemoryAgent {
public:
    explicit PtwMemoryAgent(SparseMemory &memory) : memory_(memory) {}

    void configure_backpressure(
        std::uint64_t seed, bool enabled,
        ResponseLatencyProfile latency_profile = ResponseLatencyProfile::compact)
    {
        random_state_ = seed == 0 ? 1 : seed;
        random_backpressure_ = enabled;
        latency_profile_ = latency_profile;
        response_latency_stats_ = {};
        force_a_stall_ = enabled;
    }

    void drive(UTMemBlock &dut)
    {
        const bool accept_a = !random_backpressure_ ||
                              (!force_a_stall_ && (next_random() & 3U) != 0);
        dut.auto_inner_ptw_to_l2_buffer_out_a_ready.ImmSet(accept_a);
        if (responses_.empty()) {
            dut.auto_inner_ptw_to_l2_buffer_out_d_valid.ImmSet(std::uint64_t{0});
            d_presenting_ = false;
            return;
        }
        if (!d_presenting_) {
            if (d_gap_ != 0) {
                --d_gap_;
                ++response_delay_cycles_;
                dut.auto_inner_ptw_to_l2_buffer_out_d_valid.ImmSet(
                    std::uint64_t{0});
                return;
            }
            d_presenting_ = true;
        }
        const Response &response = responses_.front();
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_opcode.ImmSet(response.opcode);
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_param.ImmSet(response.param);
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_size.ImmSet(response.size);
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_source.ImmSet(response.source);
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_sink.ImmSet(std::uint64_t{0});
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_denied.ImmSet(std::uint64_t{0});
        auto data = response.data;
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_data.ImmSetBytes(data);
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_corrupt.ImmSet(std::uint64_t{0});
        dut.auto_inner_ptw_to_l2_buffer_out_d_valid.ImmSet(std::uint64_t{1});
    }

    void capture_before_tick(UTMemBlock &dut)
    {
        const bool a_valid = dut.auto_inner_ptw_to_l2_buffer_out_a_valid.B();
        const bool a_ready = dut.auto_inner_ptw_to_l2_buffer_out_a_ready.B();
        if (a_valid && !a_ready) {
            ++request_stall_cycles_;
            force_a_stall_ = false;
        }
        a_fire_ = a_valid && a_ready;
        if (a_fire_) {
            request_ = Request{
                static_cast<std::uint8_t>(
                    dut.auto_inner_ptw_to_l2_buffer_out_a_bits_opcode.U()),
                static_cast<std::uint8_t>(
                    dut.auto_inner_ptw_to_l2_buffer_out_a_bits_param.U()),
                static_cast<std::uint8_t>(
                    dut.auto_inner_ptw_to_l2_buffer_out_a_bits_size.U()),
                static_cast<std::uint8_t>(
                    dut.auto_inner_ptw_to_l2_buffer_out_a_bits_source.U()),
                dut.auto_inner_ptw_to_l2_buffer_out_a_bits_address.U(),
            };
            if (request_->opcode != 4 && request_->opcode != 6) {
                error_ = "unsupported PTW TileLink A opcode";
            }
            if (request_->size > 6) {
                error_ = "oversized PTW TileLink A request";
            }
        }
        d_fire_ = !responses_.empty() &&
                  dut.auto_inner_ptw_to_l2_buffer_out_d_valid.B() &&
                  dut.auto_inner_ptw_to_l2_buffer_out_d_ready.B();
        if (d_fire_) {
            const Response &expected = responses_.front();
            const auto actual_data =
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_data.GetBytes();
            if (dut.auto_inner_ptw_to_l2_buffer_out_d_bits_opcode.U() != expected.opcode ||
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_param.U() != expected.param ||
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_size.U() != expected.size ||
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_source.U() != expected.source ||
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_sink.U() != 0 ||
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_denied.B() ||
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_corrupt.B() ||
                actual_data != expected.data) {
                error_ = "PTW TileLink D response identity or payload mismatch";
            }
        }
    }

    void update_after_tick()
    {
        if (d_fire_) {
            responses_.pop_front();
            d_presenting_ = false;
            d_gap_ = responses_.empty() ? 0 : responses_.front().delay_before;
        }
        if (a_fire_ && request_) {
            respond(*request_);
            ++request_count_;
        }
        a_fire_ = false;
        d_fire_ = false;
        request_.reset();
    }

    bool ok() const { return error_.empty(); }
    const std::string &error() const { return error_; }
    std::uint64_t request_count() const { return request_count_; }
    std::uint64_t request_stall_cycles() const { return request_stall_cycles_; }
    std::uint64_t response_delay_cycles() const { return response_delay_cycles_; }
    const ResponseLatencyStats &response_latency_stats() const
    {
        return response_latency_stats_;
    }

private:
    static constexpr std::size_t kBeatBytes = 32;

    struct Request {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t address;
    };

    struct Response {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::vector<unsigned char> data;
        unsigned delay_before = 0;
    };

    void respond(const Request &request)
    {
        if (request.opcode != 4 && request.opcode != 6) {
            std::ostringstream message;
            message << "unsupported PTW TileLink A opcode "
                    << static_cast<unsigned>(request.opcode);
            error_ = message.str();
            return;
        }
        const std::uint64_t transfer_bytes = std::uint64_t{1} << request.size;
        const std::uint64_t base = request.address & ~(transfer_bytes - 1);
        const std::size_t beats = static_cast<std::size_t>(
            transfer_bytes > kBeatBytes ? transfer_bytes / kBeatBytes : 1);
        for (std::size_t beat = 0; beat < beats; ++beat) {
            push_response(Response{
                static_cast<std::uint8_t>(request.opcode == 4 ? 1 : 5),
                static_cast<std::uint8_t>(request.opcode == 4 ? 0 : 1),
                request.size,
                request.source,
                memory_.read_beat(base + beat * kBeatBytes, kBeatBytes),
            }, beat == 0);
        }
    }

    std::uint64_t next_random()
    {
        random_state_ ^= random_state_ << 13;
        random_state_ ^= random_state_ >> 7;
        random_state_ ^= random_state_ << 17;
        return random_state_;
    }

    unsigned response_delay(bool first_beat)
    {
        if (!random_backpressure_) {
            return 0;
        }
        if (!first_beat) {
            return static_cast<unsigned>(next_random() % 4);
        }
        const unsigned delay = sample_response_latency(
            latency_profile_, next_random(), response_latency_stats_.samples);
        response_latency_stats_.sample(delay);
        return delay;
    }

    void push_response(Response response, bool first_beat)
    {
        const bool was_empty = responses_.empty();
        response.delay_before = response_delay(first_beat);
        responses_.push_back(std::move(response));
        if (was_empty) {
            d_presenting_ = false;
            d_gap_ = responses_.front().delay_before;
        }
    }

    SparseMemory &memory_;
    std::deque<Response> responses_;
    std::optional<Request> request_;
    bool a_fire_ = false;
    bool d_fire_ = false;
    std::uint64_t request_count_ = 0;
    std::uint64_t random_state_ = 1;
    unsigned d_gap_ = 0;
    bool random_backpressure_ = false;
    ResponseLatencyProfile latency_profile_ = ResponseLatencyProfile::compact;
    ResponseLatencyStats response_latency_stats_;
    bool force_a_stall_ = false;
    bool d_presenting_ = false;
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
    std::string error_;
};

class UncacheMemoryAgent {
public:
    explicit UncacheMemoryAgent(SparseMemory &memory) : memory_(memory) {}

    void inject_next_response_error(bool denied, bool corrupt)
    {
        inject_denied_ = denied;
        inject_corrupt_ = corrupt;
    }

    void configure_backpressure(
        std::uint64_t seed, bool enabled,
        ResponseLatencyProfile latency_profile = ResponseLatencyProfile::compact)
    {
        random_state_ = seed == 0 ? 1 : seed;
        random_backpressure_ = enabled;
        latency_profile_ = latency_profile;
        response_latency_stats_ = {};
        force_a_stall_ = enabled;
    }

    void drive(UTMemBlock &dut)
    {
        const bool accept_a = !random_backpressure_ ||
                              (!force_a_stall_ && (next_random() & 3U) != 0);
        dut.auto_inner_buffers_out_a_ready.ImmSet(accept_a);
        if (responses_.empty()) {
            dut.auto_inner_buffers_out_d_valid.ImmSet(std::uint64_t{0});
            d_presenting_ = false;
            return;
        }
        if (!d_presenting_) {
            if (d_gap_ != 0) {
                --d_gap_;
                ++response_delay_cycles_;
                dut.auto_inner_buffers_out_d_valid.ImmSet(std::uint64_t{0});
                return;
            }
            d_presenting_ = true;
        }
        const Response &response = responses_.front();
        dut.auto_inner_buffers_out_d_bits_opcode.ImmSet(response.opcode);
        dut.auto_inner_buffers_out_d_bits_param.ImmSet(std::uint64_t{0});
        dut.auto_inner_buffers_out_d_bits_size.ImmSet(response.size);
        dut.auto_inner_buffers_out_d_bits_source.ImmSet(response.source);
        dut.auto_inner_buffers_out_d_bits_sink.ImmSet(std::uint64_t{0});
        dut.auto_inner_buffers_out_d_bits_denied.ImmSet(response.denied);
        dut.auto_inner_buffers_out_d_bits_data.ImmSet(response.data);
        dut.auto_inner_buffers_out_d_bits_corrupt.ImmSet(response.corrupt);
        dut.auto_inner_buffers_out_d_valid.ImmSet(std::uint64_t{1});
    }

    void capture_before_tick(UTMemBlock &dut)
    {
        const bool a_valid = dut.auto_inner_buffers_out_a_valid.B();
        const bool a_ready = dut.auto_inner_buffers_out_a_ready.B();
        if (a_valid && !a_ready) {
            ++request_stall_cycles_;
            force_a_stall_ = false;
        }
        a_fire_ = a_valid && a_ready;
        if (a_fire_) {
            request_ = Request{
                static_cast<std::uint8_t>(dut.auto_inner_buffers_out_a_bits_opcode.U()),
                static_cast<std::uint8_t>(dut.auto_inner_buffers_out_a_bits_size.U()),
                static_cast<std::uint8_t>(dut.auto_inner_buffers_out_a_bits_source.U()),
                dut.auto_inner_buffers_out_a_bits_address.U(),
                static_cast<std::uint8_t>(dut.auto_inner_buffers_out_a_bits_mask.U()),
                dut.auto_inner_buffers_out_a_bits_data.U(),
            };
            if (request_->opcode != 0 && request_->opcode != 1 &&
                request_->opcode != 4) {
                error_ = "unsupported uncache TileLink A opcode";
            }
            if (request_->size > 6) {
                error_ = "oversized uncache TileLink A request";
            }
            if (request_->size > 3) {
                error_ = "uncache TileLink A request exceeds the 8-byte beat";
            } else {
                const unsigned transfer_bytes = 1U << request_->size;
                const unsigned beat_offset =
                    static_cast<unsigned>(request_->address & 7U);
                const std::uint8_t expected_mask = static_cast<std::uint8_t>(
                    ((1U << transfer_bytes) - 1U) << beat_offset);
                if ((request_->address & (transfer_bytes - 1U)) != 0 ||
                    beat_offset + transfer_bytes > 8 ||
                    request_->mask != expected_mask) {
                    error_ = "uncache TileLink A size/address/mask mismatch";
                }
            }
        }
        d_fire_ = !responses_.empty() &&
                  dut.auto_inner_buffers_out_d_valid.B() &&
                  dut.auto_inner_buffers_out_d_ready.B();
        if (d_fire_) {
            const Response &expected = responses_.front();
            if (dut.auto_inner_buffers_out_d_bits_opcode.U() != expected.opcode ||
                dut.auto_inner_buffers_out_d_bits_size.U() != expected.size ||
                dut.auto_inner_buffers_out_d_bits_source.U() != expected.source ||
                dut.auto_inner_buffers_out_d_bits_sink.U() != 0 ||
                dut.auto_inner_buffers_out_d_bits_denied.B() != expected.denied ||
                dut.auto_inner_buffers_out_d_bits_corrupt.B() != expected.corrupt ||
                dut.auto_inner_buffers_out_d_bits_data.U() != expected.data) {
                error_ = "uncache TileLink D response identity or payload mismatch";
            }
        }
    }

    void update_after_tick()
    {
        if (d_fire_) {
            responses_.pop_front();
            d_presenting_ = false;
            d_gap_ = responses_.empty() ? 0 : responses_.front().delay_before;
        }
        if (a_fire_ && request_) {
            respond(*request_);
            ++request_count_;
        }
        a_fire_ = false;
        d_fire_ = false;
        request_.reset();
    }

    bool ok() const { return error_.empty(); }
    const std::string &error() const { return error_; }
    std::uint64_t request_count() const { return request_count_; }
    std::uint64_t request_stall_cycles() const { return request_stall_cycles_; }
    std::uint64_t response_delay_cycles() const { return response_delay_cycles_; }
    const ResponseLatencyStats &response_latency_stats() const
    {
        return response_latency_stats_;
    }

private:
    struct Request {
        std::uint8_t opcode;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t address;
        std::uint8_t mask;
        std::uint64_t data;
    };

    struct Response {
        std::uint8_t opcode;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t data;
        bool denied = false;
        bool corrupt = false;
        unsigned delay_before = 0;
    };

    void respond(const Request &request)
    {
        const bool denied = inject_denied_;
        const bool corrupt = inject_corrupt_;
        inject_denied_ = false;
        inject_corrupt_ = false;
        if (request.opcode == 4) {
            const std::uint64_t beat_base = request.address & ~std::uint64_t{7};
            push_response(Response{
                1, request.size, request.source,
                // TileLink returns the complete 8-byte beat. LoadUnit selects
                // the requested byte lane later using the physical address.
                memory_.read_u64(beat_base),
                denied, corrupt,
            }, true);
            return;
        }
        if (request.opcode != 0 && request.opcode != 1) {
            std::ostringstream message;
            message << "unsupported uncache TileLink A opcode "
                    << static_cast<unsigned>(request.opcode);
            error_ = message.str();
            return;
        }
        const std::uint64_t beat_base = request.address & ~std::uint64_t{7};
        for (unsigned byte = 0; byte < 8; ++byte) {
            if (((request.mask >> byte) & 1U) != 0) {
                memory_.write_byte(
                    beat_base + byte,
                    static_cast<std::uint8_t>(request.data >> (8 * byte)));
            }
        }
        push_response(
            Response{0, request.size, request.source, 0, denied, corrupt}, true);
    }

    std::uint64_t next_random()
    {
        random_state_ ^= random_state_ << 13;
        random_state_ ^= random_state_ >> 7;
        random_state_ ^= random_state_ << 17;
        return random_state_;
    }

    unsigned response_delay()
    {
        if (!random_backpressure_) {
            return 0;
        }
        const unsigned delay = sample_response_latency(
            latency_profile_, next_random(), response_latency_stats_.samples);
        response_latency_stats_.sample(delay);
        return delay;
    }

    void push_response(Response response, bool first_beat)
    {
        const bool was_empty = responses_.empty();
        response.delay_before = first_beat ? response_delay() : 0;
        responses_.push_back(std::move(response));
        if (was_empty) {
            d_presenting_ = false;
            d_gap_ = responses_.front().delay_before;
        }
    }

    SparseMemory &memory_;
    std::deque<Response> responses_;
    std::optional<Request> request_;
    bool a_fire_ = false;
    bool d_fire_ = false;
    std::uint64_t request_count_ = 0;
    std::uint64_t random_state_ = 1;
    unsigned d_gap_ = 0;
    bool random_backpressure_ = false;
    ResponseLatencyProfile latency_profile_ = ResponseLatencyProfile::compact;
    ResponseLatencyStats response_latency_stats_;
    bool force_a_stall_ = false;
    bool d_presenting_ = false;
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
    bool inject_denied_ = false;
    bool inject_corrupt_ = false;
    std::string error_;
};

class LoadScoreboard {
public:
    struct Expected {
        std::uint64_t data;
        std::uint8_t pdest;
        bool rob_flag;
        bool prefetch;
        std::uint32_t exception_mask;
        bool check_data_on_exception;
        bool rf_wen;
        bool fp_wen;
        std::uint8_t trigger;
        bool flush_pipe;
        bool debug_is_mmio;
        bool debug_is_ncio;
        bool debug_is_perf_cnt;
        std::uint64_t address;
        std::uint16_t op;
    };

    void expect(const LoadTransaction &transaction, std::uint64_t data)
    {
        const auto [_, inserted] = expected_.emplace(
            rob_identity(transaction.rob, transaction.rob_flag),
            Expected{
                data,
                transaction.pdest,
                transaction.rob_flag,
                false,
                transaction.expected_exception_mask,
                transaction.check_data_on_exception,
                transaction.expected_exception_mask == 0 && transaction.rf_wen,
                transaction.expected_exception_mask == 0 && transaction.fp_wen,
                transaction.expected_trigger,
                transaction.input_flush_pipe,
                transaction.expected_debug_is_mmio.value_or(false),
                transaction.expected_debug_is_ncio.value_or(false),
                transaction.expected_debug_is_perf_cnt.value_or(false),
                transaction.address,
                static_cast<std::uint16_t>(transaction.op),
            });
        if (!inserted && error_.empty()) {
            std::ostringstream message;
            message << "duplicate outstanding scalar load ROB value rob="
                    << static_cast<unsigned>(transaction.rob)
                    << " flag=" << transaction.rob_flag;
            error_ = message.str();
        }
    }

    void expect_prefetch(const PrefetchTransaction &transaction)
    {
        const auto [_, inserted] = expected_.emplace(
            rob_identity(transaction.rob, transaction.rob_flag),
            Expected{
                0, 0, transaction.rob_flag, true, 0, false, false, false,
                transaction.expected_trigger, transaction.input_flush_pipe,
                transaction.expected_debug_is_mmio.value_or(false),
                transaction.expected_debug_is_ncio.value_or(false),
                transaction.expected_debug_is_perf_cnt.value_or(false),
                transaction.address, static_cast<std::uint16_t>(transaction.op)});
        if (!inserted && error_.empty()) {
            error_ = "duplicate outstanding scalar load ROB value";
        } else if (inserted) {
            ++pending_prefetch_;
        }
    }

    void observe(unsigned lane, const generated::ScalarLoadWriteback &writeback)
    {
        if (!writeback.valid) {
            return;
        }
        ++observed_;
        const auto it = expected_.find(
            rob_identity(writeback.rob_value, writeback.rob_flag));
        if (it == expected_.end()) {
            fail("unexpected load writeback", lane, writeback);
            return;
        }
        if (it->second.prefetch) {
            if (writeback.exception_mask != 0 || writeback.replay ||
                writeback.rf_wen || writeback.fp_wen ||
                writeback.trigger != it->second.trigger ||
                writeback.flush_pipe != it->second.flush_pipe ||
                writeback.rob_flag != it->second.rob_flag ||
                writeback.debug_is_mmio != it->second.debug_is_mmio ||
                writeback.debug_is_ncio != it->second.debug_is_ncio ||
                writeback.debug_is_perf_cnt != it->second.debug_is_perf_cnt) {
                fail("mismatched software-prefetch completion", lane, writeback,
                     &it->second);
                return;
            }
            ++prefetch_observed_;
            --pending_prefetch_;
            expected_.erase(it);
            return;
        }
        if (writeback.exception_mask != it->second.exception_mask ||
            writeback.replay || writeback.rf_wen != it->second.rf_wen ||
            writeback.fp_wen != it->second.fp_wen ||
            writeback.trigger != it->second.trigger ||
            writeback.flush_pipe != it->second.flush_pipe ||
            writeback.pdest != it->second.pdest ||
            writeback.rob_flag != it->second.rob_flag ||
            writeback.debug_is_mmio != it->second.debug_is_mmio ||
            writeback.debug_is_ncio != it->second.debug_is_ncio ||
            writeback.debug_is_perf_cnt != it->second.debug_is_perf_cnt ||
            ((it->second.exception_mask == 0 ||
              it->second.check_data_on_exception) &&
             writeback.data != it->second.data)) {
            fail("mismatched load writeback", lane, writeback, &it->second);
            return;
        }
        expected_.erase(it);
    }

    bool done() const { return expected_.empty(); }
    std::size_t pending() const { return expected_.size(); }
    std::size_t pending_prefetch() const { return pending_prefetch_; }
    std::size_t pending_load() const { return expected_.size() - pending_prefetch_; }
    bool ok() const { return error_.empty(); }
    std::uint64_t observed() const { return observed_; }
    std::uint64_t prefetch_observed() const { return prefetch_observed_; }
    const std::string &error() const { return error_; }
    std::string pending_summary() const
    {
        std::ostringstream message;
        for (const auto &[identity, expected] : expected_) {
            message << " rob=" << static_cast<unsigned>(identity.value)
                    << ':' << identity.flag
                    << " address=0x" << std::hex << expected.address
                    << " op=0x" << expected.op << std::dec
                    << " prefetch=" << expected.prefetch;
        }
        return message.str();
    }

private:
    void fail(
        const char *reason,
        unsigned lane,
        const generated::ScalarLoadWriteback &actual,
        const Expected *expected = nullptr)
    {
        if (!error_.empty()) {
            return;
        }
        std::ostringstream message;
        message << reason << " lane=" << lane
                << " rob=" << static_cast<unsigned>(actual.rob_value)
                << " pdest=" << static_cast<unsigned>(actual.pdest)
                << " data=0x" << std::hex << actual.data
                << " exception=0x" << actual.exception_mask
                << " replay=" << std::dec << actual.replay
                << " rf_wen=" << actual.rf_wen
                << " fp_wen=" << actual.fp_wen
                << " flush_pipe=" << actual.flush_pipe
                << " trigger=" << static_cast<unsigned>(actual.trigger)
                << " mmio=" << actual.debug_is_mmio
                << " ncio=" << actual.debug_is_ncio
                << " perf_cnt=" << actual.debug_is_perf_cnt;
        if (expected != nullptr) {
            message << " expected_pdest=" << static_cast<unsigned>(expected->pdest)
                    << " expected_rob_flag=" << expected->rob_flag
                    << " expected_exception=0x" << std::hex
                    << expected->exception_mask
                    << " expected_data=0x" << std::hex << expected->data
                    << " check_data_on_exception=" << std::dec
                    << expected->check_data_on_exception
                    << " expected_rf_wen=" << std::dec << expected->rf_wen
                    << " expected_fp_wen=" << expected->fp_wen
                    << " expected_flush_pipe=" << expected->flush_pipe
                    << " expected_trigger=" << static_cast<unsigned>(expected->trigger)
                    << " expected_mmio=" << expected->debug_is_mmio
                    << " expected_ncio=" << expected->debug_is_ncio
                    << " expected_perf_cnt=" << expected->debug_is_perf_cnt;
        }
        error_ = message.str();
    }

    std::unordered_map<RobIdentity, Expected, RobIdentityHash> expected_;
    std::uint64_t observed_ = 0;
    std::uint64_t prefetch_observed_ = 0;
    std::size_t pending_prefetch_ = 0;
    std::string error_;
};

class StoreScoreboard {
public:
    void expect(const StoreTransaction &transaction)
    {
        const auto [_, inserted] = expected_.emplace(
            rob_identity(transaction.rob, transaction.rob_flag),
            Expected{
                transaction.rob_flag,
                transaction.expected_exception_mask,
                false,
                false,
                false,
                false,
                0,
                0,
                transaction.expected_trigger,
                transaction.input_flush_pipe,
                transaction.expected_debug_is_mmio,
                transaction.expected_debug_is_ncio,
            });
        if (!inserted && error_.empty()) {
            error_ = "duplicate outstanding scalar store ROB value";
        }
    }

    void observe_address(
        unsigned lane, const generated::ScalarStoreWriteback &writeback,
        std::uint64_t sample_cycle)
    {
        if (!writeback.valid) {
            return;
        }
        const auto it = expected_.find(
            rob_identity(writeback.rob_value, writeback.rob_flag));
        if (it == expected_.end()) {
            fail("unexpected store-address writeback", lane, writeback);
            return;
        }
        // The store-address output has no ready signal.  A valid pulse can
        // therefore still be visible after an issue attempt that was stalled;
        // do not attribute that pulse until the input handshake was observed.
        if (!it->second.address_issued) {
            return;
        }
        if (sample_cycle < it->second.address_issue_cycle) {
            return;
        }
        const bool trigger_mismatch =
            (it->second.trigger.has_value() &&
             writeback.trigger != *it->second.trigger);
        const bool debug_mismatch =
            (it->second.debug_is_mmio.has_value() &&
             writeback.debug_is_mmio != *it->second.debug_is_mmio) ||
            (it->second.debug_is_ncio.has_value() &&
             writeback.debug_is_ncio != *it->second.debug_is_ncio);
        if (writeback.exception_mask != it->second.exception_mask ||
            writeback.rob_flag != it->second.rob_flag ||
            trigger_mismatch ||
            writeback.flush_pipe != it->second.flush_pipe ||
            debug_mismatch) {
            if (error_.empty()) {
                std::ostringstream message;
                message << "mismatched store-address writeback lane=" << lane
                        << " rob=" << static_cast<unsigned>(writeback.rob_value)
                        << " rob_flag=" << writeback.rob_flag
                        << " exception=0x" << std::hex << writeback.exception_mask
                        << " trigger=" << std::dec
                        << static_cast<unsigned>(writeback.trigger)
                        << " flush_pipe=" << writeback.flush_pipe
                        << " expected_rob_flag=" << it->second.rob_flag
                        << " expected_exception=0x" << std::hex
                        << it->second.exception_mask
                        << " expected_trigger=" << std::dec
                        << static_cast<unsigned>(it->second.trigger.value_or(kTriggerNone))
                        << " expected_flush_pipe=" << it->second.flush_pipe;
                error_ = message.str();
            }
            return;
        }
        if (it->second.address_seen) {
            fail("duplicate store-address writeback", lane, writeback);
            return;
        }
        it->second.address_seen = true;
        retire_if_complete(it);
    }

    void observe_data(
        unsigned lane, const generated::ScalarStoreWriteback &writeback,
        std::uint64_t sample_cycle)
    {
        if (!writeback.valid) {
            return;
        }
        Iterator match = expected_.end();
        for (auto it = expected_.begin(); it != expected_.end(); ++it) {
            if (it->first.value != writeback.rob_value ||
                !it->second.data_issued) {
                continue;
            }
            if (sample_cycle < it->second.data_issue_cycle) {
                continue;
            }
            if (match != expected_.end()) {
                fail("ambiguous store-data writeback", lane, writeback);
                return;
            }
            match = it;
        }
        if (match == expected_.end()) {
            // The RTL output is a bare valid pulse (no ready and no ROB flag)
            // and may retain the previous ROB value while the next data issue
            // is stalled.  With no accepted data outstanding there is no
            // architectural event to attribute, so ignore that stale pulse.
            return;
        }
        if (match->second.data_seen) {
            fail("duplicate store-data writeback", lane, writeback);
            return;
        }
        match->second.data_seen = true;
        retire_if_complete(match);
    }

    bool done() const { return expected_.empty(); }
    std::size_t pending() const { return expected_.size(); }
    bool ok() const { return error_.empty(); }
    std::uint64_t observed() const { return observed_; }
    const std::string &error() const { return error_; }

    bool mark_address_issued(
        const StoreTransaction &transaction, std::uint64_t issue_cycle)
    {
        const auto it = expected_.find(
            rob_identity(transaction.rob, transaction.rob_flag));
        if (it == expected_.end()) {
            if (error_.empty()) {
                error_ = "store-address issue was not expected";
            }
            return false;
        }
        // A TLB miss may require the same address uop to be replayed after
        // translation.  Recording the handshake is therefore intentionally
        // idempotent; duplicate *writebacks* remain an error below.
        it->second.address_issued = true;
        it->second.address_issue_cycle = issue_cycle;
        return true;
    }

    bool mark_data_issued(
        const StoreTransaction &transaction, unsigned lane,
        std::uint64_t issue_cycle)
    {
        if (lane >= kScalarStoreLanes) {
            if (error_.empty()) {
                error_ = "invalid scalar store-data issue lane";
            }
            return false;
        }
        const auto match = expected_.find(
            rob_identity(transaction.rob, transaction.rob_flag));
        if (match == expected_.end()) {
            if (error_.empty()) {
                error_ = "scalar store-data issue was not expected";
            }
            return false;
        }
        // Store-data can likewise be retried when the SQ entry is replayed.
        // Keep the issued bit as a monotonic fact rather than rejecting a
        // legal retry; duplicate output pulses are still checked separately.
        match->second.data_issued = true;
        match->second.data_issue_cycle = issue_cycle;
        return true;
    }

private:
    struct Expected {
        bool rob_flag;
        std::uint32_t exception_mask;
        bool address_seen;
        bool data_seen;
        bool address_issued;
        bool data_issued;
        std::uint64_t address_issue_cycle;
        std::uint64_t data_issue_cycle;
        std::optional<std::uint8_t> trigger;
        bool flush_pipe;
        std::optional<bool> debug_is_mmio;
        std::optional<bool> debug_is_ncio;
    };

    using Iterator = std::unordered_map<RobIdentity, Expected, RobIdentityHash>::iterator;

    void retire_if_complete(Iterator it)
    {
        // An exceptional scalar store reports its architectural disposition
        // on the address writeback and intentionally has no data writeback.
        // Normal stores still require both halves, so a missing data pulse
        // cannot be hidden by this exception rule.
        if (it->second.address_seen &&
            (it->second.data_seen || it->second.exception_mask != 0)) {
            ++observed_;
            expected_.erase(it);
        }
    }

    void fail(
        const char *reason,
        unsigned lane,
        const generated::ScalarStoreWriteback &writeback)
    {
        if (!error_.empty()) {
            return;
        }
        std::ostringstream message;
        message << reason << " lane=" << lane
                << " rob=" << static_cast<unsigned>(writeback.rob_value)
                << " rob_flag=" << writeback.rob_flag
                << " exception=0x" << std::hex << writeback.exception_mask
                << " trigger=" << static_cast<unsigned>(writeback.trigger)
                << " flush_pipe=" << std::dec << writeback.flush_pipe;
        if (reason == std::string("unexpected store-data writeback") ||
            reason == std::string("ambiguous store-data writeback")) {
            message << " pending_robs=";
            bool first = true;
            for (const auto &entry : expected_) {
                if (!first) {
                    message << ',';
                }
                first = false;
                message << static_cast<unsigned>(entry.first.value)
                        << ':' << entry.first.flag;
            }
        }
        error_ = message.str();
    }

    std::unordered_map<RobIdentity, Expected, RobIdentityHash> expected_;
    std::uint64_t observed_ = 0;
    std::string error_;
};

class VectorMemoryScoreboard {
public:
    struct Expected {
        bool store;
        std::array<unsigned char, 16> data;
        std::uint16_t active_elements;
        std::uint16_t fu_op_type;
        std::uint8_t eew;
        std::uint8_t vl;
        std::uint8_t vstart;
        std::uint8_t pdest;
        bool rob_flag;
        std::uint32_t exception_mask;
        std::optional<std::uint8_t> trigger;
        bool vec_wen;
        bool v0_wen;
        bool vl_wen;
        bool flush_pipe;
        std::optional<bool> debug_is_mmio;
        std::optional<bool> debug_is_ncio;
        std::optional<bool> debug_is_perf_cnt;
        bool vma;
        bool vta;
        std::uint64_t address;
        VectorAddressingMode addressing;
        std::int64_t stride;
        std::uint16_t mask_bits;
        std::array<unsigned char, 16> index;
    };

    void expect(
        const VectorMemoryTransaction &transaction,
        const std::array<unsigned char, 16> &data)
    {
        const auto [_, inserted] = expected_.emplace(
            rob_identity(transaction.rob, transaction.rob_flag),
            Expected{
                transaction.store,
                data,
                active_vector_elements(transaction),
                vector_fu_op_type(transaction),
                transaction.eew,
                transaction.vl,
                transaction.vstart,
                transaction.pdest,
                transaction.rob_flag,
                transaction.expected_exception_mask,
                transaction.expected_trigger,
                !transaction.store,
                false,
                false,
                transaction.input_flush_pipe,
                transaction.expected_debug_is_mmio,
                transaction.expected_debug_is_ncio,
                transaction.expected_debug_is_perf_cnt,
                transaction.vma,
                transaction.vta,
                transaction.address,
                transaction.addressing,
                transaction.stride,
                transaction.mask_bits,
                transaction.index,
            });
        if (!inserted && error_.empty()) {
            error_ = "duplicate outstanding vector memory ROB value";
        }
    }

    void observe(unsigned lane, const generated::VectorMemoryWriteback &writeback)
    {
        if (!writeback.valid) {
            return;
        }
        const auto it = expected_.find(
            rob_identity(writeback.rob_value, writeback.rob_flag));
        if (it == expected_.end()) {
            fail("unexpected vector memory writeback", lane, writeback);
            return;
        }
        const Expected &expected = it->second;
        const bool normal_progress = expected.exception_mask == 0;
        const bool exception_progress = expected.exception_mask != 0;
        const bool debug_mismatch =
            (expected.debug_is_mmio.has_value() &&
             writeback.debug_is_mmio != *expected.debug_is_mmio) ||
            (expected.debug_is_ncio.has_value() &&
             writeback.debug_is_ncio != *expected.debug_is_ncio) ||
            (expected.debug_is_perf_cnt.has_value() &&
             writeback.debug_is_perf_cnt != *expected.debug_is_perf_cnt);
        const bool trigger_mismatch =
            (expected.trigger.has_value() &&
             writeback.trigger != *expected.trigger);
        if (writeback.exception_mask != expected.exception_mask || writeback.replay ||
            writeback.flush_pipe != expected.flush_pipe ||
            trigger_mismatch ||
            writeback.vec_wen != expected.vec_wen ||
            writeback.v0_wen != expected.v0_wen ||
            writeback.vl_wen != expected.vl_wen ||
            debug_mismatch ||
            writeback.fu_op_type != expected.fu_op_type ||
            writeback.rob_flag != expected.rob_flag ||
            ((!exception_progress) &&
             (writeback.vsew != expected.eew || writeback.veew != expected.eew ||
              writeback.vl != expected.vl || writeback.vstart != 0 ||
              writeback.vuop_idx != 0))) {
            fail("mismatched vector memory metadata", lane, writeback, &expected);
            return;
        }
        if (!expected.store && expected.exception_mask == 0) {
            if (!writeback.vec_wen || writeback.pdest != expected.pdest ||
                !matches_load_data(writeback.data, expected) ||
                !matches_active_mask(writeback.vmask, expected.active_elements)) {
                fail("mismatched vector load data", lane, writeback, &expected);
                return;
            }
            ++load_observed_;
        } else if (expected.store) {
            ++store_observed_;
        } else {
            ++load_observed_;
        }
        expected_.erase(it);
    }

    bool done() const { return expected_.empty(); }
    std::size_t pending() const { return expected_.size(); }
    std::size_t pending_loads() const
    {
        return static_cast<std::size_t>(std::count_if(
            expected_.begin(), expected_.end(),
            [](const auto &entry) { return !entry.second.store; }));
    }
    std::size_t pending_stores() const
    {
        return static_cast<std::size_t>(std::count_if(
            expected_.begin(), expected_.end(),
            [](const auto &entry) { return entry.second.store; }));
    }
    bool ok() const { return error_.empty(); }
    std::uint64_t load_observed() const { return load_observed_; }
    std::uint64_t store_observed() const { return store_observed_; }
    const std::string &error() const { return error_; }

private:
    static bool matches_load_data(
        const std::vector<unsigned char> &actual, const Expected &expected)
    {
        if (actual.size() != expected.data.size()) {
            return false;
        }
        const unsigned element_bytes = 1U << expected.eew;
        const unsigned elements = expected.data.size() / element_bytes;
        for (unsigned element = 0; element < elements; ++element) {
            bool preserved = true;
            bool all_ones = true;
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                const unsigned offset = element * element_bytes + byte;
                preserved &= actual[offset] == expected.data[offset];
                all_ones &= actual[offset] == 0xff;
            }
            const bool active =
                ((expected.active_elements >> element) & 1U) != 0;
            const bool tail_agnostic = element >= expected.vl && expected.vta;
            const bool mask_agnostic =
                element >= expected.vstart && element < expected.vl &&
                !active && expected.vma;
            if (active || (!tail_agnostic && !mask_agnostic)) {
                if (!preserved) {
                    return false;
                }
            } else if (!preserved && !all_ones) {
                return false;
            }
        }
        return true;
    }

    static bool matches_active_mask(
        const std::vector<unsigned char> &actual, std::uint16_t expected)
    {
        if (actual.size() < 2) {
            return false;
        }
        const std::uint16_t value = static_cast<std::uint16_t>(actual[0]) |
                                    (static_cast<std::uint16_t>(actual[1]) << 8);
        if (value != expected) {
            return false;
        }
        for (std::size_t index = 2; index < actual.size(); ++index) {
            if (actual[index] != 0) {
                return false;
            }
        }
        return true;
    }

    void fail(
        const char *reason,
        unsigned lane,
        const generated::VectorMemoryWriteback &actual,
        const Expected *expected = nullptr)
    {
        if (!error_.empty()) {
            return;
        }
        std::ostringstream message;
        message << reason << " lane=" << lane
                << " rob=" << static_cast<unsigned>(actual.rob_value)
                << " op=0x" << std::hex << actual.fu_op_type
                << " exception=0x" << actual.exception_mask
                << " replay=" << std::dec << actual.replay
                << " flush_pipe=" << actual.flush_pipe
                << " trigger=" << static_cast<unsigned>(actual.trigger)
                << " vec_wen=" << actual.vec_wen
                << " pdest=" << static_cast<unsigned>(actual.pdest)
                << " vl=" << static_cast<unsigned>(actual.vl)
                << " vstart=" << static_cast<unsigned>(actual.vstart)
                << " eew=" << static_cast<unsigned>(actual.veew);
        if (expected != nullptr) {
            message << " expected_op=0x" << std::hex << expected->fu_op_type
                    << " expected_exception=0x" << expected->exception_mask
                    << " expected_trigger="
                    << static_cast<unsigned>(expected->trigger.value_or(kVectorWritebackTriggerNone))
                    << " expected_active=0x" << expected->active_elements
                    << " address=0x" << expected->address << std::dec
                    << " addressing="
                    << static_cast<unsigned>(expected->addressing)
                    << " stride=" << expected->stride
                    << " mask=0x" << std::hex << expected->mask_bits
                    << " index=";
            for (const auto byte : expected->index) {
                message << std::setw(2) << std::setfill('0')
                        << static_cast<unsigned>(byte);
            }
            message << " actual_data=";
            for (const auto byte : actual.data) {
                message << std::setw(2) << std::setfill('0')
                        << static_cast<unsigned>(byte);
            }
            message << " expected_data=";
            for (const auto byte : expected->data) {
                message << std::setw(2) << std::setfill('0')
                        << static_cast<unsigned>(byte);
            }
        }
        error_ = message.str();
    }

    std::unordered_map<RobIdentity, Expected, RobIdentityHash> expected_;
    std::uint64_t load_observed_ = 0;
    std::uint64_t store_observed_ = 0;
    std::string error_;
};

class Environment {
    struct VectorReplayRequest {
        unsigned lane;
        bool lq_flag;
        std::uint8_t lq_value;
        bool sq_flag;
        std::uint8_t sq_value;
        bool is_part_replay;
        std::uint16_t replay_mask;
        std::uint8_t replay_mb_index;
    };

public:
    struct L2TlbResponse {
        std::uint64_t paddr = 0;
        std::uint8_t pbmt = 0;
        bool miss = false;
        bool guest_page_fault = false;
        bool page_fault = false;
        bool access_fault = false;
        bool pmp_load_denied = false;
        bool pmp_mmio = false;
    };

    Environment(int argc, char **argv)
        : dut_(argc, argv), memory_(&bus_memory_),
          memory_agent_(bus_memory_, memory_),
          ptw_agent_(bus_memory_), uncache_agent_(bus_memory_)
    {
        dut_.InitClock(dut_.clock);
        generated::drive_idle_inputs(dut_);
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(std::uint64_t{3});
        dut_.io_ooo_to_mem_tlbCsr_priv_imode.ImmSet(std::uint64_t{3});
    }

    ~Environment() { dut_.Finish(); }

    SparseMemory &memory() { return memory_; }
    std::uint64_t bus_expected_load(std::uint64_t address, LoadOp op) const
    {
        return bus_memory_.expected_load(address, op);
    }
    void expect_release_line(
        std::uint64_t base, const std::vector<unsigned char> &bytes)
    {
        memory_agent_.expect_release_line(base, bytes);
    }
    void configure_backpressure(
        std::uint64_t seed, bool enabled,
        ResponseLatencyProfile latency_profile = ResponseLatencyProfile::compact)
    {
        memory_agent_.configure_backpressure(seed, enabled, latency_profile);
        ptw_agent_.configure_backpressure(
            seed ^ 0x9e3779b97f4a7c15ULL, enabled, latency_profile);
        uncache_agent_.configure_backpressure(
            seed ^ 0x3c6ef372fe94f82aULL, enabled, latency_profile);
    }

    void inject_next_dcache_response_error(bool denied, bool corrupt)
    {
        memory_agent_.inject_next_response_error(denied, corrupt);
    }

    void inject_next_uncache_response_error(bool denied, bool corrupt)
    {
        uncache_agent_.inject_next_response_error(denied, corrupt);
    }

    void configure_cache_error_enable(bool enable)
    {
        dut_.io_ooo_to_mem_csrCtrl_cache_error_enable.ImmSet(enable);
    }

    // The L2-to-L1 DTLB request has no ready pin at the MemBlock boundary:
    // MemBlock ties the response consumer ready high internally.  Hold the
    // request valid for one cycle, then wait for the returned response while
    // the ordinary PTW agent services any page walk generated by the request.
    bool issue_l2_tlb_request(
        std::uint64_t vaddr, std::uint8_t cmd, bool kill, bool is_prefetch,
        bool no_translate, L2TlbResponse &response, unsigned timeout = 4096)
    {
        dut_.io_l2_tlb_req_req_bits_vaddr.ImmSet(vaddr);
        dut_.io_l2_tlb_req_req_bits_cmd.ImmSet(cmd);
        dut_.io_l2_tlb_req_req_bits_kill.ImmSet(kill);
        dut_.io_l2_tlb_req_req_bits_isPrefetch.ImmSet(is_prefetch);
        dut_.io_l2_tlb_req_req_bits_no_translate.ImmSet(no_translate);
        dut_.io_l2_tlb_req_req_valid.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_l2_tlb_req_req_valid.ImmSet(std::uint64_t{0});
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            if (dut_.io_l2_tlb_req_resp_valid.B()) {
                response.paddr = dut_.io_l2_tlb_req_resp_bits_paddr_0.U();
                response.pbmt = static_cast<std::uint8_t>(
                    dut_.io_l2_tlb_req_resp_bits_pbmt_0.U());
                response.miss = dut_.io_l2_tlb_req_resp_bits_miss.B();
                response.guest_page_fault =
                    dut_.io_l2_tlb_req_resp_bits_excp_0_gpf_ld.B();
                response.page_fault =
                    dut_.io_l2_tlb_req_resp_bits_excp_0_pf_ld.B();
                response.access_fault =
                    dut_.io_l2_tlb_req_resp_bits_excp_0_af_ld.B();
                response.pmp_load_denied = dut_.io_l2_pmp_resp_ld.B();
                response.pmp_mmio = dut_.io_l2_pmp_resp_mmio.B();
                return check_components();
            }
            tick(false);
        }
        error_ = "timed out waiting for L2-to-L1 DTLB response";
        return false;
    }

    bool issue_killed_l2_tlb_request(
        std::uint64_t vaddr, std::uint8_t cmd, bool is_prefetch,
        bool no_translate, unsigned timeout = 128)
    {
        dut_.io_l2_tlb_req_req_bits_vaddr.ImmSet(vaddr);
        dut_.io_l2_tlb_req_req_bits_cmd.ImmSet(cmd);
        dut_.io_l2_tlb_req_req_bits_kill.ImmSet(std::uint64_t{1});
        dut_.io_l2_tlb_req_req_bits_isPrefetch.ImmSet(is_prefetch);
        dut_.io_l2_tlb_req_req_bits_no_translate.ImmSet(no_translate);
        dut_.io_l2_tlb_req_req_valid.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_l2_tlb_req_req_valid.ImmSet(std::uint64_t{0});
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            if (dut_.io_l2_tlb_req_resp_valid.B()) {
                error_ = "killed L2-to-L1 DTLB request produced a response";
                return false;
            }
            tick(false);
        }
        return check_components();
    }

    bool pulse_l2_hint(std::uint8_t source_id, bool is_keyword)
    {
        if (source_id >= 16) {
            error_ = "L2 hint source ID exceeds the top-level field width";
            return false;
        }
        dut_.io_l2_hint_bits_sourceId.ImmSet(source_id);
        dut_.io_l2_hint_bits_isKeyword.ImmSet(is_keyword);
        dut_.io_l2_hint_valid.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_l2_hint_valid.ImmSet(std::uint64_t{0});
        // MemBlock registers the hint before distributing it to DCache/LSQ.
        // Let that one-cycle pulse drain and reject any unexpected terminal
        // traffic caused by a hint with no matching outstanding MSHR.
        return run_cycles(4) && check_components();
    }

    bool configure_memory_trigger(
        unsigned index, std::uint64_t address, std::uint8_t action,
        bool load, bool store, bool enable = true)
    {
        if (index >= 4) {
            error_ = "memory trigger index exceeds TriggerNum";
            return false;
        }
        // The CSR block presents a two-cycle delayed tdata update. Keep all
        // trigger fields explicit so this helper is independent of the idle
        // policy and can be reused by constrained-random trigger tests.
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr.ImmSet(index);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action.ImmSet(action);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store.ImmSet(store);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load.ImmSet(load);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2.ImmSet(address);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0.ImmSet(
            index == 0 && enable);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1.ImmSet(
            index == 1 && enable);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2.ImmSet(
            index == 2 && enable);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3.ImmSet(
            index == 3 && enable);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_debugMode.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid.ImmSet(
            std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid.ImmSet(
            std::uint64_t{0});
        return run_cycles(3) && check_components();
    }
    std::uint64_t cycle() const { return dut_.xclock.clk; }
    std::uint64_t tilelink_requests() const { return memory_agent_.request_count(); }
    std::uint64_t tilelink_releases() const { return memory_agent_.release_count(); }
    std::uint64_t tilelink_release_data() const
    {
        return memory_agent_.release_data_count();
    }
    std::uint64_t tilelink_release_data_verified() const
    {
        return memory_agent_.release_data_verified_count();
    }
    std::uint64_t ptw_requests() const { return ptw_agent_.request_count(); }
    std::uint64_t dcache_request_stalls() const
    {
        return memory_agent_.request_stall_cycles();
    }
    std::uint64_t dcache_response_delays() const
    {
        return memory_agent_.response_delay_cycles();
    }
    const ResponseLatencyStats &dcache_response_latency_stats() const
    {
        return memory_agent_.response_latency_stats();
    }
    std::uint64_t ptw_request_stalls() const
    {
        return ptw_agent_.request_stall_cycles();
    }
    std::uint64_t ptw_response_delays() const
    {
        return ptw_agent_.response_delay_cycles();
    }
    const ResponseLatencyStats &ptw_response_latency_stats() const
    {
        return ptw_agent_.response_latency_stats();
    }
    std::uint64_t exception_vaddr()
    {
        return dut_.io_mem_to_ooo_lsqio_vaddr.U();
    }
    std::uint64_t exception_gpaddr()
    {
        return dut_.io_mem_to_ooo_lsqio_gpaddr.U();
    }
    bool exception_is_for_vs_nonleaf_pte()
    {
        return dut_.io_mem_to_ooo_lsqio_isForVSnonLeafPTE.B();
    }
    std::uint64_t uncache_requests() const
    {
        return uncache_agent_.request_count();
    }
    std::uint64_t uncache_request_stalls() const
    {
        return uncache_agent_.request_stall_cycles();
    }
    std::uint64_t uncache_response_delays() const
    {
        return uncache_agent_.response_delay_cycles();
    }
    const ResponseLatencyStats &uncache_response_latency_stats() const
    {
        return uncache_agent_.response_latency_stats();
    }
    bool store_mmio_valid()
    {
        return dut_.io_mem_to_ooo_lsqio_storeMmio.B();
    }
    std::uint8_t store_mmio_rob()
    {
        return static_cast<std::uint8_t>(
            dut_.io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value.U());
    }
    std::uint64_t store_tlb_feedbacks() const { return store_tlb_feedbacks_; }
    std::uint64_t store_tlb_misses() const { return store_tlb_misses_; }
    std::uint64_t lq_allocated() const { return lq_allocated_; }
    std::uint64_t lq_dequeued() const { return lq_dequeued_; }
    std::uint64_t lq_canceled() const { return lq_canceled_; }
    std::uint64_t sq_allocated() const { return sq_allocated_; }
    std::uint64_t sq_dequeued() const { return sq_dequeued_; }
    std::uint64_t sq_canceled() const { return sq_canceled_; }
    std::uint64_t writebacks() const { return scoreboard_.observed(); }
    std::uint64_t prefetch_writebacks() const
    {
        return scoreboard_.prefetch_observed();
    }
    std::uint64_t store_writebacks() const { return store_scoreboard_.observed(); }
    std::uint64_t vector_load_writebacks() const
    {
        return vector_scoreboard_.load_observed();
    }
    std::uint64_t vector_store_writebacks() const
    {
        return vector_scoreboard_.store_observed();
    }
    std::size_t pending_scalar_loads() const { return scoreboard_.pending_load(); }
    std::size_t pending_prefetches() const { return scoreboard_.pending_prefetch(); }
    std::size_t pending_scalar_stores() const { return store_scoreboard_.pending(); }
    std::size_t pending_vector_loads() const
    {
        return vector_scoreboard_.pending_loads();
    }
    std::size_t pending_vector_stores() const
    {
        return vector_scoreboard_.pending_stores();
    }
    std::uint64_t vector_replay_feedbacks() const
    {
        return vector_replay_feedbacks_;
    }
    std::uint64_t pin_space_digest() const { return pin_space_digest_; }

    bool check_pin_space()
    {
        pin_space_digest_ = 1469598103934665603ULL;
        for (unsigned pattern = 0;
             pattern < generated::kPinSpacePatternCount; ++pattern) {
            generated::drive_pin_space_pattern(dut_, pattern);
            dut_.reset.ImmSet(std::uint64_t{1});
            dut_.RefreshComb();
            if (!generated::verify_pin_space_pattern(dut_, pattern)) {
                error_ = "input-space pattern readback mismatch";
                return false;
            }
            pin_space_digest_ ^= generated::sample_all_outputs(dut_);
            pin_space_digest_ *= 1099511628211ULL;
        }
        generated::drive_idle_inputs(dut_);
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(std::uint64_t{3});
        dut_.io_ooo_to_mem_tlbCsr_priv_imode.ImmSet(std::uint64_t{3});
        dut_.RefreshComb();
        return true;
    }

    bool reset()
    {
        // Re-assert external reset for every invocation.  Relying on the
        // constructor's initial value made repeated-reset scenarios silently
        // run without resetting the DUT.
        dut_.reset.ImmSet(std::uint64_t{1});
        for (unsigned cycle = 0; cycle < 8; ++cycle) {
            tick(false);
        }
        dut_.reset.ImmSet(std::uint64_t{0});
        for (unsigned cycle = 0; cycle < 16; ++cycle) {
            tick(false);
            if (!dut_.io_reset_backend.B()) {
                return true;
            }
        }
        error_ = "internal reset did not deassert within 16 cycles";
        return false;
    }

    bool configure_sv39(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x90000000ULL,
        bool writable = true,
        bool noncacheable = false)
    {
        constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
        constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
        constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
        constexpr std::uint64_t pte_accessed = std::uint64_t{1} << 6;
        constexpr std::uint64_t pte_dirty = std::uint64_t{1} << 7;
        constexpr std::uint64_t pte_pbmt_nc = std::uint64_t{1} << 61;
        constexpr std::uint64_t gigabyte_mask = (std::uint64_t{1} << 30) - 1;
        if ((physical_address & gigabyte_mask) !=
            (virtual_address & gigabyte_mask)) {
            error_ = "Sv39 NC helper requires equal 1-GiB page offsets";
            return false;
        }
        const std::uint64_t physical_base = physical_address & ~gigabyte_mask;
        const std::uint64_t vpn2 = (virtual_address >> 30) & 0x1ff;
        const std::uint64_t permissions = pte_read | (writable ? pte_write : 0);
        const std::uint64_t pte = ((physical_base >> 12) << 10) |
                                  (noncacheable ? pte_pbmt_nc : 0) |
                                  pte_dirty | pte_accessed |
                                  permissions | pte_valid;
        memory_.write_u64(root_page_table + vpn2 * 8, pte);

        // Permit S-mode page-table walks and data accesses over the full PA space.
        if (!write_distributed_csr(0x3b0, ~std::uint64_t{0}) ||
            !write_distributed_csr(0x3a0, 0x1f)) {
            return false;
        }
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_satp_mode.ImmSet(std::uint64_t{8});
        dut_.io_ooo_to_mem_tlbCsr_satp_asid.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_satp_ppn.ImmSet(root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_mPBMTE.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_satp_changed.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_satp_changed.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{0});
        // MemBlock pipelines the TLB CSR and fence indication before the DTLBs.
        // Do not issue the cold miss while that delayed flush is still active.
        return run_cycles(16) && check_components();
    }

    bool configure_sv39_nc(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x90000000ULL,
        bool writable = true)
    {
        return configure_sv39(
            virtual_address, physical_address, root_page_table, writable, true);
    }

    bool map_sv39_4k(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false,
        bool io = false)
    {
        constexpr std::uint64_t page_mask = 0xfff;
        constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
        constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
        constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
        constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
        constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
        constexpr std::uint64_t pte_accessed = std::uint64_t{1} << 6;
        constexpr std::uint64_t pte_dirty = std::uint64_t{1} << 7;
        constexpr std::uint64_t pte_pbmt_nc = std::uint64_t{1} << 61;
        constexpr std::uint64_t pte_pbmt_io = std::uint64_t{1} << 62;
        if ((virtual_address & page_mask) != (physical_address & page_mask) ||
            (root_page_table & page_mask) != 0) {
            error_ = "Sv39 4-KiB mapping requires aligned root and equal page offsets";
            return false;
        }
        if (writable && !readable) {
            error_ = "Sv39 does not permit W=1,R=0 leaf mappings";
            return false;
        }
        if (noncacheable && io) {
            error_ = "Sv39 PBMT mapping cannot select NC and IO simultaneously";
            return false;
        }

        auto allocate_table = [&]() {
            auto [it, inserted] = next_page_table_.emplace(
                root_page_table, root_page_table + 0x1000);
            const std::uint64_t result = it->second;
            it->second += 0x1000;
            return result;
        };
        const std::uint64_t vpn2 = (virtual_address >> 30) & 0x1ff;
        const std::uint64_t vpn1 = (virtual_address >> 21) & 0x1ff;
        const std::uint64_t vpn0 = (virtual_address >> 12) & 0x1ff;
        const std::uint64_t l1_key = root_page_table ^ (vpn2 << 12);
        auto [l1_it, l1_inserted] = sv39_l1_tables_.emplace(l1_key, 0);
        if (l1_inserted) {
            l1_it->second = allocate_table();
            memory_.write_u64(
                root_page_table + vpn2 * 8,
                ((l1_it->second >> 12) << 10) | pte_valid);
        }
        const std::uint64_t l0_key = l1_it->second ^ (vpn1 << 12);
        auto [l0_it, l0_inserted] = sv39_l0_tables_.emplace(l0_key, 0);
        if (l0_inserted) {
            l0_it->second = allocate_table();
            memory_.write_u64(
                l1_it->second + vpn1 * 8,
                ((l0_it->second >> 12) << 10) | pte_valid);
        }
        const std::uint64_t flags = pte_valid |
            (readable ? pte_read : 0) |
            (writable ? pte_write : 0) |
            (executable ? pte_execute : 0) |
            (user ? pte_user : 0) | pte_accessed |
            (writable ? pte_dirty : 0) |
            (noncacheable ? pte_pbmt_nc : 0) |
            (io ? pte_pbmt_io : 0);
        memory_.write_u64(
            l0_it->second + vpn0 * 8,
            (((physical_address & ~page_mask) >> 12) << 10) | flags);
        return true;
    }

    bool map_sv48_4k(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false,
        bool io = false)
    {
        constexpr std::uint64_t page_mask = 0xfff;
        constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
        constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
        constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
        constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
        constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
        constexpr std::uint64_t pte_accessed = std::uint64_t{1} << 6;
        constexpr std::uint64_t pte_dirty = std::uint64_t{1} << 7;
        constexpr std::uint64_t pte_pbmt_nc = std::uint64_t{1} << 61;
        constexpr std::uint64_t pte_pbmt_io = std::uint64_t{1} << 62;
        if ((virtual_address & page_mask) != (physical_address & page_mask) ||
            (root_page_table & page_mask) != 0) {
            error_ = "Sv48 4-KiB mapping requires aligned root and equal page offsets";
            return false;
        }
        if (writable && !readable) {
            error_ = "Sv48 does not permit W=1,R=0 leaf mappings";
            return false;
        }
        if (noncacheable && io) {
            error_ = "Sv48 PBMT mapping cannot select NC and IO simultaneously";
            return false;
        }
        if (!reference_canonical_virtual_address(
                virtual_address, ReferencePageMode::sv48)) {
            error_ = "Sv48 mapping requires a canonical virtual address";
            return false;
        }

        auto allocate_table = [&]() {
            auto [it, inserted] = next_page_table_.emplace(
                root_page_table, root_page_table + 0x1000);
            const std::uint64_t result = it->second;
            it->second += 0x1000;
            return result;
        };
        const std::uint64_t vpn3 = (virtual_address >> 39) & 0x1ff;
        const std::uint64_t vpn2 = (virtual_address >> 30) & 0x1ff;
        const std::uint64_t vpn1 = (virtual_address >> 21) & 0x1ff;
        const std::uint64_t vpn0 = (virtual_address >> 12) & 0x1ff;
        const std::uint64_t l2_key = root_page_table ^ (vpn3 << 12);
        auto [l2_it, l2_inserted] = sv48_l2_tables_.emplace(l2_key, 0);
        if (l2_inserted) {
            l2_it->second = allocate_table();
            memory_.write_u64(
                root_page_table + vpn3 * 8,
                ((l2_it->second >> 12) << 10) | pte_valid);
        }
        const std::uint64_t l1_key = l2_it->second ^ (vpn2 << 12);
        auto [l1_it, l1_inserted] = sv48_l1_tables_.emplace(l1_key, 0);
        if (l1_inserted) {
            l1_it->second = allocate_table();
            memory_.write_u64(
                l2_it->second + vpn2 * 8,
                ((l1_it->second >> 12) << 10) | pte_valid);
        }
        const std::uint64_t l0_key = l1_it->second ^ (vpn1 << 12);
        auto [l0_it, l0_inserted] = sv48_l0_tables_.emplace(l0_key, 0);
        if (l0_inserted) {
            l0_it->second = allocate_table();
            memory_.write_u64(
                l1_it->second + vpn1 * 8,
                ((l0_it->second >> 12) << 10) | pte_valid);
        }
        const std::uint64_t flags = pte_valid |
            (readable ? pte_read : 0) |
            (writable ? pte_write : 0) |
            (executable ? pte_execute : 0) |
            (user ? pte_user : 0) | pte_accessed |
            (writable ? pte_dirty : 0) |
            (noncacheable ? pte_pbmt_nc : 0) |
            (io ? pte_pbmt_io : 0);
        memory_.write_u64(
            l0_it->second + vpn0 * 8,
            (((physical_address & ~page_mask) >> 12) << 10) | flags);
        return true;
    }

    bool activate_sv39(
        std::uint64_t root_page_table = 0x91000000ULL,
        std::uint16_t asid = 0)
    {
        return activate_stage_one(
            ReferencePageMode::sv39, root_page_table, asid);
    }

    bool activate_sv48(
        std::uint64_t root_page_table = 0x91000000ULL,
        std::uint16_t asid = 0)
    {
        return activate_stage_one(
            ReferencePageMode::sv48, root_page_table, asid);
    }

    bool activate_bare(std::uint16_t asid = 0)
    {
        return activate_stage_one(ReferencePageMode::bare, 0, asid);
    }

    bool map_sv39_leaf(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        unsigned leaf_level,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false)
    {
        return map_reference_leaf(
            virtual_address, physical_address, root_page_table,
            ReferencePageMode::sv39, false, leaf_level, readable, writable,
            executable, user, noncacheable);
    }

    bool map_sv39_2m(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false)
    {
        return map_sv39_leaf(
            virtual_address, physical_address, 1, root_page_table, readable,
            writable, executable, user, noncacheable);
    }

    bool map_sv39_1g(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false)
    {
        return map_sv39_leaf(
            virtual_address, physical_address, 2, root_page_table, readable,
            writable, executable, user, noncacheable);
    }

    bool map_sv48_leaf(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        unsigned leaf_level,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false)
    {
        return map_reference_leaf(
            virtual_address, physical_address, root_page_table,
            ReferencePageMode::sv48, false, leaf_level, readable, writable,
            executable, user, noncacheable);
    }

    bool map_sv48_2m(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false)
    {
        return map_sv48_leaf(
            virtual_address, physical_address, 1, root_page_table, readable,
            writable, executable, user, noncacheable);
    }

    bool map_sv48_1g(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false)
    {
        return map_sv48_leaf(
            virtual_address, physical_address, 2, root_page_table, readable,
            writable, executable, user, noncacheable);
    }

    bool map_sv48_512g(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false)
    {
        return map_sv48_leaf(
            virtual_address, physical_address, 3, root_page_table, readable,
            writable, executable, user, noncacheable);
    }

    bool map_sv48x4_4k(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        constexpr std::uint64_t page_mask = 0xfff;
        constexpr std::uint64_t root_mask = 0x3fff;
        constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
        constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
        constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
        constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
        constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
        constexpr std::uint64_t pte_accessed = std::uint64_t{1} << 6;
        constexpr std::uint64_t pte_dirty = std::uint64_t{1} << 7;
        if ((guest_physical_address & page_mask) !=
                (host_physical_address & page_mask) ||
            (root_page_table & root_mask) != 0) {
            error_ = "Sv48x4 mapping requires a 16-KiB root and equal page offsets";
            return false;
        }
        if (writable && !readable) {
            error_ = "Sv48x4 does not permit W=1,R=0 leaf mappings";
            return false;
        }
        if (!reference_gpa_in_range(
                guest_physical_address, ReferencePageMode::sv48)) {
            error_ = "Sv48x4 mapping exceeds the 50-bit guest physical address space";
            return false;
        }

        auto allocate_table = [&]() {
            auto [it, inserted] = next_gstage_page_table_.emplace(
                root_page_table, root_page_table + 0x4000);
            const std::uint64_t result = it->second;
            it->second += 0x1000;
            return result;
        };
        const std::uint64_t vpn3 = (guest_physical_address >> 39) & 0x7ff;
        const std::uint64_t vpn2 = (guest_physical_address >> 30) & 0x1ff;
        const std::uint64_t vpn1 = (guest_physical_address >> 21) & 0x1ff;
        const std::uint64_t vpn0 = (guest_physical_address >> 12) & 0x1ff;
        const std::uint64_t l2_key = root_page_table ^ (vpn3 << 14);
        auto [l2_it, l2_inserted] = gstage_sv48_l2_tables_.emplace(l2_key, 0);
        if (l2_inserted) {
            l2_it->second = allocate_table();
            memory_.write_u64(
                root_page_table + vpn3 * 8,
                ((l2_it->second >> 12) << 10) | pte_valid);
        }
        const std::uint64_t l1_key = l2_it->second ^ (vpn2 << 12);
        auto [l1_it, l1_inserted] = gstage_sv48_l1_tables_.emplace(l1_key, 0);
        if (l1_inserted) {
            l1_it->second = allocate_table();
            memory_.write_u64(
                l2_it->second + vpn2 * 8,
                ((l1_it->second >> 12) << 10) | pte_valid);
        }
        const std::uint64_t l0_key = l1_it->second ^ (vpn1 << 12);
        auto [l0_it, l0_inserted] = gstage_sv48_l0_tables_.emplace(l0_key, 0);
        if (l0_inserted) {
            l0_it->second = allocate_table();
            memory_.write_u64(
                l1_it->second + vpn1 * 8,
                ((l0_it->second >> 12) << 10) | pte_valid);
        }
        const std::uint64_t flags = pte_valid |
            (readable ? pte_read : 0) |
            (writable ? pte_write : 0) |
            (executable ? pte_execute : 0) | pte_user |
            pte_accessed | (writable ? pte_dirty : 0);
        memory_.write_u64(
            l0_it->second + vpn0 * 8,
            (((host_physical_address & ~page_mask) >> 12) << 10) | flags);
        return true;
    }

    bool map_sv48x4_leaf(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        unsigned leaf_level,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        return map_reference_leaf(
            guest_physical_address, host_physical_address, root_page_table,
            ReferencePageMode::sv48, true, leaf_level, readable, writable,
            executable, true, false);
    }

    bool map_sv48x4_2m(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        return map_sv48x4_leaf(
            guest_physical_address, host_physical_address, 1,
            root_page_table, readable, writable, executable);
    }

    bool map_sv48x4_1g(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        return map_sv48x4_leaf(
            guest_physical_address, host_physical_address, 2,
            root_page_table, readable, writable, executable);
    }

    bool map_sv48x4_512g(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        return map_sv48x4_leaf(
            guest_physical_address, host_physical_address, 3,
            root_page_table, readable, writable, executable);
    }

private:
    bool map_reference_leaf(
        std::uint64_t input_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table,
        ReferencePageMode mode,
        bool x4,
        unsigned leaf_level,
        bool readable,
        bool writable,
        bool executable,
        bool user,
        bool noncacheable)
    {
        constexpr std::uint64_t page_mask = 0xfff;
        constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
        constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
        constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
        constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
        constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
        constexpr std::uint64_t pte_accessed = std::uint64_t{1} << 6;
        constexpr std::uint64_t pte_dirty = std::uint64_t{1} << 7;
        constexpr std::uint64_t pte_pbmt_nc = std::uint64_t{1} << 61;
        if (leaf_level >= reference_page_levels(mode)) {
            error_ = "page mapping leaf level exceeds selected mode";
            return false;
        }
        const unsigned offset_bits = 12 + 9 * leaf_level;
        const std::uint64_t offset_mask =
            (std::uint64_t{1} << offset_bits) - 1;
        const std::uint64_t root_mask = x4 ? 0x3fffULL : page_mask;
        if ((root_page_table & root_mask) != 0 ||
            (input_address & offset_mask) != (physical_address & offset_mask)) {
            error_ = x4
                ? "x4 mapping requires a 16-KiB root and equal leaf offsets"
                : "mapping requires an aligned root and equal leaf offsets";
            return false;
        }
        if (x4 ? !reference_gpa_in_range(input_address, mode)
               : !reference_canonical_virtual_address(input_address, mode)) {
            error_ = x4 ? "x4 mapping exceeds the guest address space"
                        : "mapping requires a canonical virtual address";
            return false;
        }
        if (writable && !readable) {
            error_ = "page mapping does not permit W=1,R=0 leaf mappings";
            return false;
        }

        auto &next_tables = x4 ? next_gstage_page_table_ : next_page_table_;
        const std::uint64_t initial = root_page_table + (x4 ? 0x4000 : 0x1000);
        auto allocate_table = [&]() {
            auto [it, inserted] = next_tables.emplace(root_page_table, initial);
            const std::uint64_t result = it->second;
            it->second += 0x1000;
            return result;
        };

        const unsigned top_level = reference_page_levels(mode) - 1;
        std::uint64_t table = root_page_table;
        for (int level = static_cast<int>(top_level);
             level > static_cast<int>(leaf_level); --level) {
            const unsigned shift = 12 + 9 * static_cast<unsigned>(level);
            const std::uint64_t index_mask =
                x4 && static_cast<unsigned>(level) == top_level ? 0x7ff : 0x1ff;
            const std::uint64_t index = (input_address >> shift) & index_mask;
            const std::uint64_t pte_address = table + index * 8;
            const std::uint64_t pte = memory_.read_u64(pte_address);
            std::uint64_t child = 0;
            if ((pte & 1U) != 0 && !reference_pte_is_leaf(pte)) {
                child = reference_pte_ppn(pte) << 12;
            } else {
                child = allocate_table();
                memory_.write_u64(
                    pte_address, ((child >> 12) << 10) | pte_valid);
            }
            table = child;
        }

        const unsigned leaf_shift = 12 + 9 * leaf_level;
        const std::uint64_t leaf_mask =
            x4 && leaf_level == top_level ? 0x7ff : 0x1ff;
        const std::uint64_t leaf_index =
            (input_address >> leaf_shift) & leaf_mask;
        const std::uint64_t flags = pte_valid |
            (readable ? pte_read : 0) |
            (writable ? pte_write : 0) |
            (executable ? pte_execute : 0) |
            ((user || x4) ? pte_user : 0) |
            pte_accessed | (writable ? pte_dirty : 0) |
            (noncacheable ? pte_pbmt_nc : 0);
        memory_.write_u64(
            table + leaf_index * 8,
            (((physical_address & ~page_mask) >> 12) << 10) | flags);
        return true;
    }

    bool activate_stage_one(
        ReferencePageMode mode,
        std::uint64_t root_page_table,
        std::uint16_t asid)
    {
        if (!write_distributed_csr(0x3b0, ~std::uint64_t{0}) ||
            !write_distributed_csr(0x3a0, 0x1f)) {
            return false;
        }
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_satp_mode.ImmSet(
            static_cast<std::uint64_t>(mode));
        dut_.io_ooo_to_mem_tlbCsr_satp_asid.ImmSet(asid);
        dut_.io_ooo_to_mem_tlbCsr_satp_ppn.ImmSet(root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_mPBMTE.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_satp_changed.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_satp_changed.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

public:

    bool map_sv39x4_4k(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        constexpr std::uint64_t page_mask = 0xfff;
        constexpr std::uint64_t root_mask = 0x3fff;
        constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
        constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
        constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
        constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
        constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
        constexpr std::uint64_t pte_accessed = std::uint64_t{1} << 6;
        constexpr std::uint64_t pte_dirty = std::uint64_t{1} << 7;
        if ((guest_physical_address & page_mask) !=
                (host_physical_address & page_mask) ||
            (root_page_table & root_mask) != 0) {
            error_ = "Sv39x4 mapping requires a 16-KiB root and equal page offsets";
            return false;
        }
        if (writable && !readable) {
            error_ = "Sv39x4 does not permit W=1,R=0 leaf mappings";
            return false;
        }

        auto allocate_table = [&]() {
            auto [it, inserted] = next_gstage_page_table_.emplace(
                root_page_table, root_page_table + 0x4000);
            const std::uint64_t result = it->second;
            it->second += 0x1000;
            return result;
        };
        const std::uint64_t vpn2 = (guest_physical_address >> 30) & 0x7ff;
        const std::uint64_t vpn1 = (guest_physical_address >> 21) & 0x1ff;
        const std::uint64_t vpn0 = (guest_physical_address >> 12) & 0x1ff;
        const std::uint64_t l1_key = root_page_table ^ (vpn2 << 14);
        auto [l1_it, l1_inserted] = gstage_l1_tables_.emplace(l1_key, 0);
        if (l1_inserted) {
            l1_it->second = allocate_table();
            memory_.write_u64(
                root_page_table + vpn2 * 8,
                ((l1_it->second >> 12) << 10) | pte_valid);
        }
        const std::uint64_t l0_key = l1_it->second ^ (vpn1 << 12);
        auto [l0_it, l0_inserted] = gstage_l0_tables_.emplace(l0_key, 0);
        if (l0_inserted) {
            l0_it->second = allocate_table();
            memory_.write_u64(
                l1_it->second + vpn1 * 8,
                ((l0_it->second >> 12) << 10) | pte_valid);
        }
        const std::uint64_t flags = pte_valid |
            (readable ? pte_read : 0) |
            (writable ? pte_write : 0) |
            (executable ? pte_execute : 0) | pte_user | pte_accessed |
            (writable ? pte_dirty : 0);
        memory_.write_u64(
            l0_it->second + vpn0 * 8,
            (((host_physical_address & ~page_mask) >> 12) << 10) | flags);
        return true;
    }

    bool map_sv39x4_leaf(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        unsigned leaf_level,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        return map_reference_leaf(
            guest_physical_address, host_physical_address, root_page_table,
            ReferencePageMode::sv39, true, leaf_level, readable, writable,
            executable, true, false);
    }

    bool map_sv39x4_2m(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        return map_sv39x4_leaf(
            guest_physical_address, host_physical_address, 1,
            root_page_table, readable, writable, executable);
    }

    bool map_sv39x4_1g(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false)
    {
        return map_sv39x4_leaf(
            guest_physical_address, host_physical_address, 2,
            root_page_table, readable, writable, executable);
    }

    bool activate_two_stage(
        std::uint64_t vs_root_page_table,
        std::uint64_t g_root_page_table,
        std::uint16_t asid = 0,
        std::uint16_t vmid = 0)
    {
        return activate_two_stage_modes(
            ReferencePageMode::sv39,
            ReferencePageMode::sv39,
            vs_root_page_table,
            g_root_page_table,
            asid,
            vmid);
    }

    bool activate_two_stage_modes(
        ReferencePageMode vs_mode,
        ReferencePageMode g_mode,
        std::uint64_t vs_root_page_table,
        std::uint64_t g_root_page_table,
        std::uint16_t asid = 0,
        std::uint16_t vmid = 0)
    {
        if (!write_distributed_csr(0x3b0, ~std::uint64_t{0}) ||
            !write_distributed_csr(0x3a0, 0x1f)) {
            return false;
        }
        dut_.io_ooo_to_mem_tlbCsr_satp_mode.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_vsatp_mode.ImmSet(
            static_cast<std::uint64_t>(vs_mode));
        dut_.io_ooo_to_mem_tlbCsr_vsatp_asid.ImmSet(asid);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_ppn.ImmSet(vs_root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_mode.ImmSet(
            static_cast<std::uint64_t>(g_mode));
        dut_.io_ooo_to_mem_tlbCsr_hgatp_vmid.ImmSet(vmid);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_ppn.ImmSet(g_root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_vsatp_changed.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_hgatp_changed.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_changed.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_hgatp_changed.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool issue_sfence(
        std::uint64_t address = 0,
        std::uint16_t id = 0,
        bool all_virtual_addresses = true,
        bool all_contexts = true,
        bool hypervisor_virtual = false,
        bool hypervisor_guest = false,
        bool flush_pipe = true)
    {
        dut_.io_ooo_to_mem_sfence_bits_rs1.ImmSet(all_virtual_addresses);
        dut_.io_ooo_to_mem_sfence_bits_rs2.ImmSet(all_contexts);
        dut_.io_ooo_to_mem_sfence_bits_addr.ImmSet(address);
        dut_.io_ooo_to_mem_sfence_bits_id.ImmSet(id);
        dut_.io_ooo_to_mem_sfence_bits_flushPipe.ImmSet(flush_pipe);
        dut_.io_ooo_to_mem_sfence_bits_hv.ImmSet(hypervisor_virtual);
        dut_.io_ooo_to_mem_sfence_bits_hg.ImmSet(hypervisor_guest);
        dut_.io_ooo_to_mem_sfence_valid.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_sfence_valid.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool enable_misaligned_accesses(bool load = true, bool store = true)
    {
        dut_.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable.ImmSet(load);
        dut_.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable.ImmSet(store);
        return run_cycles(8) && check_components();
    }

    bool set_rob_head(std::uint8_t value, bool flag = false)
    {
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(value);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(flag);
        tick();
        return check_components();
    }

    bool pulse_pending_load(std::uint8_t value, bool flag = false)
    {
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(value);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(flag);
        dut_.io_ooo_to_mem_lsqio_pendingMMIOld.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_ooo_to_mem_lsqio_pendingMMIOld.ImmSet(std::uint64_t{0});
        return check_components();
    }

    bool wait_for_mmio_request(
        std::uint8_t value, bool flag = false, unsigned timeout = 2048)
    {
        const std::uint64_t request_before = uncache_agent_.request_count();
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(value);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(flag);
        dut_.io_ooo_to_mem_lsqio_pendingMMIOld.ImmSet(std::uint64_t{1});
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            tick();
            if (!check_components()) {
                dut_.io_ooo_to_mem_lsqio_pendingMMIOld.ImmSet(std::uint64_t{0});
                return false;
            }
            if (uncache_agent_.request_count() > request_before) {
                dut_.io_ooo_to_mem_lsqio_pendingMMIOld.ImmSet(std::uint64_t{0});
                return true;
            }
        }
        dut_.io_ooo_to_mem_lsqio_pendingMMIOld.ImmSet(std::uint64_t{0});
        error_ = "timed out waiting for MMIO Uncache request";
        return false;
    }

    bool pulse_pending_store(std::uint8_t value, bool flag = false)
    {
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(value);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(flag);
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
        return check_components();
    }

    bool wait_for_mmio_store_request(
        std::uint8_t value, bool flag = false, unsigned timeout = 2048)
    {
        const std::uint64_t request_before = uncache_agent_.request_count();
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(value);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(flag);
        // ROB exposes pendingst as a one-cycle commit pulse.  Keep the pulse
        // shape here and only wait after it has been sampled by StoreQueue;
        // holding it high changes the real ROB/LSQ timing contract.
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
            if (uncache_agent_.request_count() > request_before) {
                return true;
            }
        }
        error_ = "timed out waiting for MMIO store Uncache request";
        return false;
    }

    bool set_wfi(bool enabled)
    {
        dut_.io_wfi_wfiReq.ImmSet(enabled);
        tick();
        return check_components();
    }

    bool enqueue_load(const LoadTransaction &transaction)
    {
        if (!wait_for_enqueue_capacity(1, 0)) {
            return false;
        }
        generated::LsqEnqueue enqueue;
        enqueue.need_alloc = 1;
        enqueue.exception_mask = transaction.input_exception_mask;
        enqueue.trigger = transaction.input_trigger;
        enqueue.flush_pipe = transaction.input_flush_pipe;
        enqueue.fu_type = kFuTypeLoad;
        enqueue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        enqueue.rob_flag = transaction.rob_flag;
        enqueue.rob_value = transaction.rob;
        enqueue.lq_flag = transaction.lq_flag;
        enqueue.lq_value = transaction.lq;
        enqueue.sq_flag = transaction.sq_flag;
        enqueue.sq_value = transaction.sq;
        generated::drive_lsq_enqueue(dut_, 0, enqueue);
        tick();
        generated::clear_lsq_enqueue_valids(dut_);
        ++lq_allocated_;
        return check_components();
    }

    bool enqueue_load_batch(
        const std::vector<LoadTransaction> &transactions,
        const std::vector<unsigned> &dispatch_lanes)
    {
        if (transactions.empty() ||
            transactions.size() != dispatch_lanes.size() ||
            transactions.size() > generated::kLsqEnqueueLanes ||
            !std::is_sorted(dispatch_lanes.begin(), dispatch_lanes.end())) {
            error_ = "LSQ load batch requires one sorted unique dispatch lane per load";
            return false;
        }
        std::array<bool, generated::kLsqEnqueueLanes> lane_used{};
        if (!wait_for_enqueue_capacity(transactions.size(), 0)) {
            return false;
        }
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            const unsigned lane = dispatch_lanes[index];
            if (lane >= generated::kLsqEnqueueLanes || lane_used[lane]) {
                error_ = "LSQ load batch has an invalid or duplicate dispatch lane";
                generated::clear_lsq_enqueue_valids(dut_);
                return false;
            }
            lane_used[lane] = true;
            const auto &transaction = transactions[index];
            generated::LsqEnqueue enqueue;
            enqueue.need_alloc = 1;
            enqueue.exception_mask = transaction.input_exception_mask;
            enqueue.trigger = transaction.input_trigger;
            enqueue.flush_pipe = transaction.input_flush_pipe;
            enqueue.fu_type = kFuTypeLoad;
            enqueue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
            enqueue.rob_flag = transaction.rob_flag;
            enqueue.rob_value = transaction.rob;
            enqueue.lq_flag = transaction.lq_flag;
            enqueue.lq_value = transaction.lq;
            enqueue.sq_flag = transaction.sq_flag;
            enqueue.sq_value = transaction.sq;
            generated::drive_lsq_enqueue(dut_, lane, enqueue);
        }
        tick();
        generated::clear_lsq_enqueue_valids(dut_);
        lq_allocated_ += transactions.size();
        return check_components();
    }

    bool issue_load(const LoadTransaction &transaction, unsigned timeout = 32)
    {
        generated::ScalarLoadIssue issue;
        issue.pc = 0x1000 + transaction.rob * 4;
        issue.predecode_rvc = transaction.predecode_rvc;
        issue.ftq_ptr = transaction.ftq_ptr;
        issue.ftq_offset = transaction.ftq_offset;
        issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        issue.rf_wen = transaction.rf_wen;
        issue.fp_wen = transaction.fp_wen;
        issue.pdest = transaction.pdest;
        issue.rob_flag = transaction.rob_flag;
        issue.rob_value = transaction.rob;
        issue.lq_flag = transaction.lq_flag;
        issue.lq_value = transaction.lq;
        issue.sq_flag = transaction.sq_flag;
        issue.sq_value = transaction.sq;
        issue.store_set_hit = transaction.store_set_hit;
        issue.wait_for_rob_flag = transaction.wait_for_rob_flag;
        issue.wait_for_rob_value = transaction.wait_for_rob_value;
        issue.load_wait_bit = transaction.load_wait_bit;
        issue.load_wait_strict = transaction.load_wait_strict;
        issue.src = transaction.address;

        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            generated::drive_scalar_load_issue(dut_, transaction.lane, issue);
            dut_.RefreshComb();
            const bool ready = generated::scalar_load_issue_ready(dut_, transaction.lane);
            tick();
            if (ready) {
                generated::clear_scalar_load_issue_valid(dut_, transaction.lane);
                return check_components();
            }
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_scalar_load_issue_valid(dut_, transaction.lane);
        error_ = "scalar load issue timed out waiting for ready";
        return false;
    }

    bool issue_load_batch(
        const std::vector<LoadTransaction> &transactions, unsigned timeout = 64)
    {
        if (transactions.empty() || transactions.size() > kScalarLoadLanes) {
            error_ = "scalar load batch must contain one to three transactions";
            return false;
        }
        std::array<bool, kScalarLoadLanes> lane_used{};
        std::vector<generated::ScalarLoadIssue> issues(transactions.size());
        std::vector<bool> pending(transactions.size(), true);
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            const auto &transaction = transactions[index];
            if (transaction.lane >= kScalarLoadLanes || lane_used[transaction.lane]) {
                error_ = "scalar load batch lanes must be unique";
                return false;
            }
            lane_used[transaction.lane] = true;
            auto &issue = issues[index];
            issue.pc = 0x1000 + transaction.rob * 4;
            issue.predecode_rvc = transaction.predecode_rvc;
            issue.ftq_ptr = transaction.ftq_ptr;
            issue.ftq_offset = transaction.ftq_offset;
            issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
            issue.rf_wen = transaction.rf_wen;
            issue.fp_wen = transaction.fp_wen;
            issue.pdest = transaction.pdest;
            issue.rob_flag = transaction.rob_flag;
            issue.rob_value = transaction.rob;
            issue.lq_flag = transaction.lq_flag;
            issue.lq_value = transaction.lq;
            issue.sq_flag = transaction.sq_flag;
            issue.sq_value = transaction.sq;
            issue.store_set_hit = transaction.store_set_hit;
            issue.wait_for_rob_flag = transaction.wait_for_rob_flag;
            issue.wait_for_rob_value = transaction.wait_for_rob_value;
            issue.load_wait_bit = transaction.load_wait_bit;
            issue.load_wait_strict = transaction.load_wait_strict;
            issue.src = transaction.address;
        }

        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            for (std::size_t index = 0; index < transactions.size(); ++index) {
                if (pending[index]) {
                    generated::drive_scalar_load_issue(
                        dut_, transactions[index].lane, issues[index]);
                }
            }
            dut_.RefreshComb();
            std::vector<bool> accepted(transactions.size(), false);
            for (std::size_t index = 0; index < transactions.size(); ++index) {
                accepted[index] = pending[index] && generated::scalar_load_issue_ready(
                    dut_, transactions[index].lane);
            }
            tick();
            for (std::size_t index = 0; index < transactions.size(); ++index) {
                if (accepted[index]) {
                    pending[index] = false;
                    generated::clear_scalar_load_issue_valid(
                        dut_, transactions[index].lane);
                }
            }
            if (!check_components()) {
                return false;
            }
            bool any_pending = false;
            for (const bool value : pending) {
                any_pending = any_pending || value;
            }
            if (!any_pending) {
                return true;
            }
        }
        generated::clear_scalar_load_issue_valids(dut_);
        error_ = "scalar load issue batch timed out waiting for ready";
        return false;
    }

    void expect_load(const LoadTransaction &transaction)
    {
        scoreboard_.expect(
            transaction,
            memory_.expected_load(
                transaction.oracle_address.value_or(transaction.address),
                transaction.op));
    }

    void expect_load_data(const LoadTransaction &transaction, std::uint64_t data)
    {
        scoreboard_.expect(transaction, data);
    }

    void expect_prefetch(const PrefetchTransaction &transaction)
    {
        scoreboard_.expect_prefetch(transaction);
    }

    bool enqueue_prefetch(const PrefetchTransaction &transaction)
    {
        if (!wait_for_enqueue_capacity(1, 0)) {
            return false;
        }
        generated::LsqEnqueue enqueue;
        enqueue.need_alloc = 1;
        enqueue.exception_mask = transaction.input_exception_mask;
        enqueue.trigger = transaction.input_trigger;
        enqueue.flush_pipe = transaction.input_flush_pipe;
        enqueue.fu_type = kFuTypeLoad;
        enqueue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        enqueue.rob_flag = transaction.rob_flag;
        enqueue.rob_value = transaction.rob;
        enqueue.lq_flag = transaction.lq_flag;
        enqueue.lq_value = transaction.lq;
        enqueue.sq_flag = transaction.sq_flag;
        enqueue.sq_value = transaction.sq;
        generated::drive_lsq_enqueue(dut_, 0, enqueue);
        tick();
        generated::clear_lsq_enqueue_valids(dut_);
        ++lq_allocated_;
        return check_components();
    }

    bool issue_prefetch(
        const PrefetchTransaction &transaction, unsigned timeout = 32)
    {
        generated::ScalarLoadIssue issue;
        issue.pc = 0x1800 + transaction.rob * 4;
        issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        issue.rf_wen = false;
        issue.fp_wen = false;
        issue.pdest = 0;
        issue.rob_flag = transaction.rob_flag;
        issue.rob_value = transaction.rob;
        issue.lq_flag = transaction.lq_flag;
        issue.lq_value = transaction.lq;
        issue.sq_flag = transaction.sq_flag;
        issue.sq_value = transaction.sq;
        issue.src = transaction.address;
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            generated::drive_scalar_load_issue(dut_, transaction.lane, issue);
            dut_.RefreshComb();
            const bool ready = generated::scalar_load_issue_ready(
                dut_, transaction.lane);
            tick();
            if (ready) {
                generated::clear_scalar_load_issue_valid(
                    dut_, transaction.lane);
                return check_components();
            }
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_scalar_load_issue_valid(
            dut_, transaction.lane);
        error_ = "software-prefetch issue timed out waiting for ready";
        return false;
    }

    void expect_store(const StoreTransaction &transaction)
    {
        store_scoreboard_.expect(transaction);
    }

    bool enqueue_store(const StoreTransaction &transaction, std::uint8_t lq_value)
    {
        if (!wait_for_enqueue_capacity(0, 1)) {
            return false;
        }
        generated::LsqEnqueue enqueue;
        enqueue.need_alloc = 2;
        enqueue.exception_mask = transaction.input_exception_mask;
        enqueue.trigger = transaction.input_trigger;
        enqueue.flush_pipe = transaction.input_flush_pipe;
        enqueue.fu_type = kFuTypeStore;
        enqueue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        enqueue.rob_flag = transaction.rob_flag;
        enqueue.rob_value = transaction.rob;
        enqueue.lq_value = lq_value;
        enqueue.sq_flag = transaction.sq_flag;
        enqueue.sq_value = transaction.sq;
        generated::drive_lsq_enqueue(dut_, 0, enqueue);
        tick();
        generated::clear_lsq_enqueue_valids(dut_);
        ++sq_allocated_;
        return check_components();
    }

    void expect_vector(const VectorMemoryTransaction &transaction)
    {
        const auto expected = transaction.store
            ? transaction.data
            : memory_.expected_vector_load(transaction);
        vector_scoreboard_.expect(transaction, expected);
    }

    void expect_vector_data(
        const VectorMemoryTransaction &transaction,
        const std::array<unsigned char, 16> &data)
    {
        vector_scoreboard_.expect(transaction, data);
    }

    bool enqueue_vector(const VectorMemoryTransaction &transaction)
    {
        const unsigned elements = transaction.flow_num;
        if (!wait_for_enqueue_capacity(
                transaction.store ? 0 : elements,
                transaction.store ? elements : 0)) {
            return false;
        }
        generated::LsqEnqueue enqueue;
        enqueue.need_alloc = transaction.store ? 2 : 1;
        enqueue.exception_mask = transaction.input_exception_mask;
        enqueue.trigger = transaction.input_trigger;
        enqueue.flush_pipe = transaction.input_flush_pipe;
        enqueue.fu_type = transaction.store ? kFuTypeVectorStore : kFuTypeVectorLoad;
        enqueue.fu_op_type = vector_fu_op_type(transaction);
        enqueue.rob_flag = transaction.rob_flag;
        enqueue.rob_value = transaction.rob;
        enqueue.lq_flag = transaction.lq_flag;
        enqueue.lq_value = transaction.lq;
        enqueue.sq_flag = transaction.sq_flag;
        enqueue.sq_value = transaction.sq;
        enqueue.num_ls_elem = transaction.flow_num;
        generated::drive_lsq_enqueue(dut_, 0, enqueue);
        tick();
        generated::clear_lsq_enqueue_valids(dut_);
        if (!transaction.store) {
            lq_allocated_ += enqueue.num_ls_elem;
        } else {
            sq_allocated_ += enqueue.num_ls_elem;
        }
        return check_components();
    }

    bool issue_vector(
        const VectorMemoryTransaction &transaction, unsigned timeout = 64)
    {
        generated::VectorMemoryIssue issue;
        issue.ftq_ptr = transaction.ftq_ptr;
        issue.ftq_offset = transaction.ftq_offset;
        issue.fu_type = transaction.store ? kFuTypeVectorStore : kFuTypeVectorLoad;
        issue.fu_op_type = vector_fu_op_type(transaction);
        issue.vec_wen = !transaction.store;
        issue.vma = transaction.vma;
        issue.vta = transaction.vta;
        issue.vsew = transaction.eew;
        issue.vlmul = transaction.vlmul;
        issue.vm = transaction.vm;
        issue.vstart = transaction.vstart;
        issue.veew = transaction.eew;
        issue.pdest = transaction.pdest;
        issue.rob_flag = transaction.rob_flag;
        issue.rob_value = transaction.rob;
        issue.lq_flag = transaction.lq_flag;
        issue.lq_value = transaction.lq;
        issue.sq_flag = transaction.sq_flag;
        issue.sq_value = transaction.sq;
        issue.flow_num = transaction.flow_num;
        issue.is_part_replay = transaction.is_part_replay;
        issue.replay_mask = transaction.replay_mask;
        issue.replay_mb_index = transaction.replay_mb_index;
        for (unsigned byte = 0; byte < 8; ++byte) {
            issue.src[0][byte] = static_cast<unsigned char>(
                transaction.address >> (8 * byte));
        }
        issue.src[2] = transaction.data;
        if (transaction.addressing == VectorAddressingMode::strided) {
            const auto stride = static_cast<std::uint64_t>(transaction.stride);
            for (unsigned byte = 0; byte < 8; ++byte) {
                issue.src[1][byte] = static_cast<unsigned char>(
                    stride >> (8 * byte));
            }
        } else if (
            transaction.addressing == VectorAddressingMode::indexed_unordered ||
            transaction.addressing == VectorAddressingMode::indexed_ordered) {
            issue.src[1] = transaction.index;
        }
        issue.src[3][0] = static_cast<unsigned char>(transaction.mask_bits);
        issue.src[3][1] = static_cast<unsigned char>(transaction.mask_bits >> 8);
        issue.src[4][0] = transaction.vl;

        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            generated::drive_vector_memory_issue(dut_, transaction.lane, issue);
            dut_.RefreshComb();
            const bool ready = generated::vector_memory_issue_ready(
                dut_, transaction.lane);
            tick();
            if (ready) {
                generated::clear_vector_memory_issue_valids(dut_);
                return check_components();
            }
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_vector_memory_issue_valids(dut_);
        std::ostringstream message;
        message << "vector memory issue timed out waiting for ready"
                << " store=" << transaction.store
                << " addressing=" << static_cast<unsigned>(transaction.addressing)
                << " eew=" << static_cast<unsigned>(transaction.eew)
                << " flow_num=" << static_cast<unsigned>(transaction.flow_num)
                << " lane=" << transaction.lane
                << " rob=" << static_cast<unsigned>(transaction.rob)
                << " lq=" << static_cast<unsigned>(transaction.lq)
                << " sq=" << static_cast<unsigned>(transaction.sq);
        error_ = message.str();
        return false;
    }

    bool issue_load_vector_pair(
        const LoadTransaction &load,
        const VectorMemoryTransaction &vector,
        unsigned timeout = 64)
    {
        generated::ScalarLoadIssue scalar_issue;
        scalar_issue.pc = 0x1000 + load.rob * 4;
        scalar_issue.fu_op_type = static_cast<std::uint16_t>(load.op);
        scalar_issue.rf_wen = load.rf_wen;
        scalar_issue.fp_wen = load.fp_wen;
        scalar_issue.pdest = load.pdest;
        scalar_issue.rob_flag = load.rob_flag;
        scalar_issue.rob_value = load.rob;
        scalar_issue.lq_flag = load.lq_flag;
        scalar_issue.lq_value = load.lq;
        scalar_issue.sq_flag = load.sq_flag;
        scalar_issue.sq_value = load.sq;
        scalar_issue.src = load.address;

        generated::VectorMemoryIssue vector_issue;
        vector_issue.fu_type = vector.store
            ? kFuTypeVectorStore
            : kFuTypeVectorLoad;
        vector_issue.fu_op_type = vector_fu_op_type(vector);
        vector_issue.vec_wen = !vector.store;
        vector_issue.vma = vector.vma;
        vector_issue.vta = vector.vta;
        vector_issue.vsew = vector.eew;
        vector_issue.vm = vector.vm;
        vector_issue.vstart = vector.vstart;
        vector_issue.veew = vector.eew;
        vector_issue.pdest = vector.pdest;
        vector_issue.rob_flag = vector.rob_flag;
        vector_issue.rob_value = vector.rob;
        vector_issue.lq_flag = vector.lq_flag;
        vector_issue.lq_value = vector.lq;
        vector_issue.sq_flag = vector.sq_flag;
        vector_issue.sq_value = vector.sq;
        vector_issue.flow_num = vector.flow_num;
        vector_issue.is_part_replay = vector.is_part_replay;
        vector_issue.replay_mask = vector.replay_mask;
        vector_issue.replay_mb_index = vector.replay_mb_index;
        for (unsigned byte = 0; byte < 8; ++byte) {
            vector_issue.src[0][byte] = static_cast<unsigned char>(
                vector.address >> (8 * byte));
        }
        vector_issue.src[2] = vector.data;
        if (vector.addressing == VectorAddressingMode::strided) {
            const auto stride = static_cast<std::uint64_t>(vector.stride);
            for (unsigned byte = 0; byte < 8; ++byte) {
                vector_issue.src[1][byte] = static_cast<unsigned char>(
                    stride >> (8 * byte));
            }
        } else if (
            vector.addressing == VectorAddressingMode::indexed_unordered ||
            vector.addressing == VectorAddressingMode::indexed_ordered) {
            vector_issue.src[1] = vector.index;
        }
        vector_issue.src[3][0] = static_cast<unsigned char>(vector.mask_bits);
        vector_issue.src[3][1] = static_cast<unsigned char>(
            vector.mask_bits >> 8);
        vector_issue.src[4][0] = vector.vl;

        bool scalar_pending = true;
        bool vector_pending = true;
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            if (scalar_pending) {
                generated::drive_scalar_load_issue(dut_, load.lane, scalar_issue);
            }
            if (vector_pending) {
                generated::drive_vector_memory_issue(
                    dut_, vector.lane, vector_issue);
            }
            dut_.RefreshComb();
            const bool scalar_accepted = scalar_pending &&
                generated::scalar_load_issue_ready(dut_, load.lane);
            const bool vector_accepted = vector_pending &&
                generated::vector_memory_issue_ready(dut_, vector.lane);
            tick();
            if (scalar_accepted) {
                scalar_pending = false;
                generated::clear_scalar_load_issue_valid(dut_, load.lane);
            }
            if (vector_accepted) {
                vector_pending = false;
                generated::clear_vector_memory_issue_valids(dut_);
            }
            if (!check_components()) {
                return false;
            }
            if (!scalar_pending && !vector_pending) {
                return true;
            }
        }
        generated::clear_scalar_load_issue_valids(dut_);
        generated::clear_vector_memory_issue_valids(dut_);
        error_ = "mixed scalar/vector load issue timed out waiting for ready";
        return false;
    }

    bool issue_store_address(const StoreTransaction &transaction, unsigned timeout = 32)
    {
        generated::ScalarStoreIssue issue;
        issue.fu_type = kFuTypeStore;
        issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        issue.rob_flag = transaction.rob_flag;
        issue.rob_value = transaction.rob;
        issue.sq_flag = transaction.sq_flag;
        issue.sq_value = transaction.sq;
        issue.src = transaction.address;
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            generated::drive_scalar_store_address(
                dut_, transaction.address_lane, issue);
            dut_.RefreshComb();
            const bool ready = generated::scalar_store_address_ready(
                dut_, transaction.address_lane);
            if (ready && !store_scoreboard_.mark_address_issued(
                    transaction, this->cycle())) {
                generated::clear_scalar_store_issue_valids(dut_);
                return false;
            }
            tick();
            if (ready) {
                generated::clear_scalar_store_issue_valids(dut_);
                return check_components();
            }
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_scalar_store_issue_valids(dut_);
        error_ = "scalar store-address issue timed out waiting for ready";
        return false;
    }

    // Atomics enter the store-address port but are handled by AtomicsUnit,
    // rather than StoreQueue.  They therefore do not have an LSQ allocation
    // or StoreScoreboard entry; the scalar-load scoreboard observes their
    // old-value writeback on the atomic WB port.
    bool issue_atomic(const AtomicTransaction &transaction, unsigned timeout = 256)
    {
        generated::ScalarStoreIssue address;
        address.fu_type = kFuTypeAtomic;
        address.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        address.rf_wen = true;
        address.pdest = transaction.pdest;
        address.rob_flag = transaction.rob_flag;
        address.rob_value = transaction.rob;
        address.sq_flag = transaction.sq_flag;
        address.sq_value = transaction.sq;
        address.src = transaction.address;
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            generated::drive_scalar_store_address(
                dut_, transaction.address_lane, address);
            dut_.RefreshComb();
            const bool ready = generated::scalar_store_address_ready(
                dut_, transaction.address_lane);
            tick();
            if (ready) {
                generated::clear_scalar_store_issue_valids(dut_);
                break;
            }
            if (!check_components()) {
                generated::clear_scalar_store_issue_valids(dut_);
                return false;
            }
            if (cycle + 1 == timeout) {
                generated::clear_scalar_store_issue_valids(dut_);
                error_ = "atomic address issue timed out waiting for ready";
                return false;
            }
        }

        const bool is_compare_swap = transaction.op == AtomicOp::amocas_w ||
            transaction.op == AtomicOp::amocas_d;
        const unsigned data_count = is_compare_swap ? 2 : 1;
        for (unsigned data_index = 0; data_index < data_count; ++data_index) {
            generated::ScalarStoreIssue data = address;
            data.src = is_compare_swap && data_index == 0
                ? transaction.compare
                : transaction.data;
            data.rf_wen = false;
            // AtomicsUnit uses the upper fuOpType bits to identify the
            // compare/swap std uop (index 0 and 1) while the low six bits
            // retain the AMOCAS.W/D operation encoding.
            data.fu_op_type = static_cast<std::uint16_t>(transaction.op) |
                static_cast<std::uint16_t>(data_index << 6);
            bool accepted = false;
            for (unsigned cycle = 0; cycle < timeout; ++cycle) {
                generated::drive_scalar_store_data(
                    dut_, transaction.data_lane, data);
                dut_.RefreshComb();
                const bool ready = generated::scalar_store_data_ready(
                    dut_, transaction.data_lane);
                tick();
                if (ready) {
                    accepted = true;
                    generated::clear_scalar_store_issue_valids(dut_);
                    break;
                }
                if (!check_components()) {
                    generated::clear_scalar_store_issue_valids(dut_);
                    return false;
                }
            }
            if (!accepted) {
                generated::clear_scalar_store_issue_valids(dut_);
                error_ = "atomic data issue timed out waiting for ready";
                return false;
            }
        }
        return check_components();
    }

    bool warm_store_translation(
        const StoreTransaction &transaction, unsigned timeout = 512)
    {
        const std::uint64_t initial_requests = ptw_agent_.request_count();
        if (!issue_store_address(transaction)) {
            return false;
        }
        for (unsigned cycle = 0;
             cycle < timeout && ptw_agent_.request_count() == initial_requests;
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (ptw_agent_.request_count() == initial_requests) {
            std::ostringstream message;
            message << "timed out waiting for store DTLB page-table walk"
                    << " feedbacks=" << store_tlb_feedbacks_
                    << " misses=" << store_tlb_misses_;
            error_ = message.str();
            return false;
        }
        if (!run_cycles(64)) {
            return false;
        }
        return issue_store_address(transaction);
    }

    bool run_until_store_complete_with_replay(
        const StoreTransaction &transaction, unsigned timeout = 4096)
    {
        constexpr unsigned replay_interval = 32;
        for (unsigned elapsed = 0;
             elapsed < timeout && !store_scoreboard_.done();) {
            const unsigned cycles = std::min(replay_interval, timeout - elapsed);
            if (!run_cycles(cycles)) {
                return false;
            }
            elapsed += cycles;
            if (store_scoreboard_.done()) {
                return check_components();
            }
            if (!issue_store_address(transaction, replay_interval)) {
                return false;
            }
            ++elapsed;
        }
        if (!store_scoreboard_.done()) {
            error_ = "timed out waiting for replayed scalar store writeback";
            return false;
        }
        return check_components();
    }

    bool run_until_store_tlb_misses(
        std::uint64_t target, unsigned timeout = 512)
    {
        for (unsigned cycle = 0;
             cycle < timeout && store_tlb_misses_ < target; ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (store_tlb_misses_ < target) {
            error_ = "timed out waiting for store TLB-miss feedback";
            return false;
        }
        return check_components();
    }

    bool run_until_ptw_requests(std::uint64_t target, unsigned timeout = 512)
    {
        for (unsigned cycle = 0;
             cycle < timeout && ptw_agent_.request_count() < target; ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (ptw_agent_.request_count() < target) {
            error_ = "timed out waiting for page-table walk request";
            return false;
        }
        return check_components();
    }

    bool run_until_uncache_requests(
        std::uint64_t target, unsigned timeout = 4096)
    {
        for (unsigned cycle = 0;
             cycle < timeout && uncache_agent_.request_count() < target;
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (uncache_agent_.request_count() < target) {
            error_ = "timed out waiting for target Uncache request count";
            return false;
        }
        return check_components();
    }

    bool issue_store_data(const StoreTransaction &transaction, unsigned timeout = 32)
    {
        generated::ScalarStoreIssue issue;
        issue.fu_type = kFuTypeStore;
        issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        issue.rob_value = transaction.rob;
        issue.sq_flag = transaction.sq_flag;
        issue.sq_value = transaction.sq;
        issue.src = transaction.data;
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            generated::drive_scalar_store_data(dut_, transaction.data_lane, issue);
            dut_.RefreshComb();
            const bool ready = generated::scalar_store_data_ready(
                dut_, transaction.data_lane);
            if (ready && !store_scoreboard_.mark_data_issued(
                    transaction, transaction.data_lane, this->cycle())) {
                generated::clear_scalar_store_issue_valids(dut_);
                return false;
            }
            tick();
            if (ready) {
                generated::clear_scalar_store_issue_valids(dut_);
                return check_components();
            }
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_scalar_store_issue_valids(dut_);
        error_ = "scalar store-data issue timed out waiting for ready";
        return false;
    }

    bool run_until_store_complete(unsigned timeout)
    {
        for (unsigned cycle = 0; cycle < timeout && !store_scoreboard_.done(); ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!store_scoreboard_.done()) {
            error_ = "timed out waiting for scalar store writebacks";
            return false;
        }
        return check_components();
    }

    bool run_until_complete(unsigned timeout)
    {
        for (unsigned cycle = 0; cycle < timeout && !scoreboard_.done(); ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!scoreboard_.done()) {
            error_ = "timed out waiting for scalar load writeback";
            return false;
        }
        return check_components();
    }

    bool run_until_vector_complete(unsigned timeout)
    {
        for (unsigned cycle = 0;
             cycle < timeout && !vector_scoreboard_.done(); ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!vector_scoreboard_.done()) {
            error_ = "timed out waiting for vector memory writeback";
            return false;
        }
        return check_components();
    }

    bool run_until_vector_complete_with_replays(
        const VectorMemoryTransaction &transaction,
        unsigned timeout,
        bool pulse_store_commit_after_replay = false)
    {
        const std::uint64_t deadline = cycle() + timeout;
        while (!vector_scoreboard_.done() && cycle() < deadline) {
            const auto replay_it = std::find_if(
                vector_replay_requests_.begin(), vector_replay_requests_.end(),
                [&](const VectorReplayRequest &request) {
                    const unsigned entries = transaction.store
                        ? kStoreQueueEntries : kVirtualLoadQueueEntries;
                    const unsigned value = transaction.store
                        ? request.sq_value : request.lq_value;
                    const bool flag = transaction.store
                        ? request.sq_flag : request.lq_flag;
                    const unsigned base = transaction.store
                        ? transaction.sq : transaction.lq;
                    const bool base_flag = transaction.store
                        ? transaction.sq_flag : transaction.lq_flag;
                    const unsigned packed = value + (flag ? entries : 0);
                    const unsigned packed_base = base + (base_flag ? entries : 0);
                    const unsigned distance =
                        (packed + 2 * entries - packed_base) % (2 * entries);
                    return distance < transaction.flow_num;
                });
            if (replay_it == vector_replay_requests_.end()) {
                tick();
                if (!check_components()) {
                    return false;
                }
                continue;
            }
            const VectorReplayRequest request = *replay_it;
            vector_replay_requests_.erase(replay_it);
            auto replay = transaction;
            replay.lane = request.lane;
            replay.is_part_replay = request.is_part_replay;
            replay.replay_mask = request.replay_mask;
            replay.replay_mb_index = request.replay_mb_index;
            if (!issue_vector(replay, 256)) {
                return false;
            }
            if (pulse_store_commit_after_replay && replay.store &&
                (!run_cycles(32) ||
                 !pulse_pending_store(replay.rob, replay.rob_flag))) {
                return false;
            }
        }
        if (!vector_scoreboard_.done()) {
            std::ostringstream message;
            message << "timed out waiting for vector memory writeback after replay"
                    << " store=" << transaction.store
                    << " address=0x" << std::hex << transaction.address << std::dec
                    << " addressing="
                    << static_cast<unsigned>(transaction.addressing)
                    << " eew=" << static_cast<unsigned>(transaction.eew)
                    << " vl=" << static_cast<unsigned>(transaction.vl)
                    << " vstart=" << static_cast<unsigned>(transaction.vstart)
                    << " vm=" << transaction.vm
                    << " mask=0x" << std::hex << transaction.mask_bits << std::dec
                    << " active=0x" << std::hex
                    << active_vector_elements(transaction) << std::dec
                    << " index=0x" << std::hex;
            for (auto it = transaction.index.rbegin();
                 it != transaction.index.rend(); ++it) {
                message << std::setw(2) << std::setfill('0')
                        << static_cast<unsigned>(*it);
            }
            message << std::dec
                    << " stride=" << transaction.stride
                    << " flow_num="
                    << static_cast<unsigned>(transaction.flow_num)
                    << " lane=" << transaction.lane
                    << " rob=" << static_cast<unsigned>(transaction.rob)
                    << " lq=" << static_cast<unsigned>(transaction.lq)
                    << '/' << transaction.lq_flag
                    << " sq=" << static_cast<unsigned>(transaction.sq)
                    << '/' << transaction.sq_flag
                    << " ptw_requests=" << ptw_agent_.request_count()
                    << " dcache_requests=" << memory_agent_.request_count()
                    << " replay_feedbacks=" << vector_replay_feedbacks_
                    << " pending_replays=" << vector_replay_requests_.size();
            error_ = message.str();
            return false;
        }
        return check_components();
    }

    bool run_until_all_complete(unsigned timeout)
    {
        for (unsigned cycle = 0;
             cycle < timeout &&
             (!scoreboard_.done() || !store_scoreboard_.done() ||
              !vector_scoreboard_.done());
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!scoreboard_.done() || !store_scoreboard_.done() ||
            !vector_scoreboard_.done()) {
            std::ostringstream message;
            message << "timed out waiting for mixed memory writebacks"
                    << " scalar_load_pending=" << scoreboard_.pending()
                    << " scalar_store_pending=" << store_scoreboard_.pending()
                    << " vector_pending=" << vector_scoreboard_.pending()
                    << scoreboard_.pending_summary();
            error_ = message.str();
            return false;
        }
        return check_components();
    }

    bool run_until_lq_retired(unsigned timeout = 32)
    {
        for (unsigned cycle = 0;
             cycle < timeout && lq_dequeued_ + lq_canceled_ < lq_allocated_;
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (lq_dequeued_ + lq_canceled_ < lq_allocated_) {
            std::ostringstream message;
            message << "timed out waiting for LQ retirement allocated="
                    << lq_allocated_ << " dequeued=" << lq_dequeued_
                    << " canceled=" << lq_canceled_;
            error_ = message.str();
            return false;
        }
        return true;
    }

    bool account_lq_cancellation(unsigned count)
    {
        if (lq_dequeued_ + lq_canceled_ + count > lq_allocated_) {
            error_ = "LQ cancellation accounting exceeds allocated entries";
            return false;
        }
        lq_canceled_ += count;
        return true;
    }

    bool account_sq_cancellation(unsigned count)
    {
        if (sq_dequeued_ + sq_canceled_ + count > sq_allocated_) {
            error_ = "SQ cancellation accounting exceeds allocated entries";
            return false;
        }
        sq_canceled_ += count;
        return true;
    }

    bool run_until_queues_retired(unsigned timeout = 512)
    {
        for (unsigned cycle = 0;
             cycle < timeout &&
             (lq_dequeued_ + lq_canceled_ < lq_allocated_ ||
              sq_dequeued_ + sq_canceled_ < sq_allocated_);
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (lq_dequeued_ + lq_canceled_ < lq_allocated_ ||
            sq_dequeued_ + sq_canceled_ < sq_allocated_) {
            std::ostringstream message;
            message << "timed out waiting for mixed LSQ retirement"
                    << " lq=" << lq_dequeued_ << '+' << lq_canceled_
                    << '/' << lq_allocated_
                    << " sq=" << sq_dequeued_ << '+' << sq_canceled_
                    << '/' << sq_allocated_;
            error_ = message.str();
            return false;
        }
        return check_components();
    }

    bool commit_store(const StoreTransaction &transaction, unsigned timeout = 512)
    {
        const std::uint64_t target = sq_dequeued_ + 1;
        if (!commit_stores_through(transaction, 1)) {
            return false;
        }
        if (!run_until_sq_dequeued(target, timeout)) {
            std::ostringstream message;
            message << error_ << " rob=" << static_cast<unsigned>(transaction.rob)
                    << " rob_flag=" << transaction.rob_flag
                    << " address=0x" << std::hex << transaction.address
                    << " op=" << std::dec
                    << static_cast<unsigned>(transaction.op)
                    << " sq=" << static_cast<unsigned>(transaction.sq)
                    << " sq_target=" << target
                    << " sq_counts=" << sq_dequeued_ << '/' << sq_allocated_;
            error_ = message.str();
            return false;
        }
        const std::uint64_t raw_address = transaction.oracle_address.value_or(
            transaction.address);
        const std::uint64_t address = transaction.op == StoreOp::cbo_zero
            ? raw_address & ~std::uint64_t{63}
            : raw_address;
        // CBO.ZERO is encoded as 0x7 but architecturally covers one cache
        // line, not a 128-byte scalar transfer.
        const unsigned bytes = transaction.op == StoreOp::cbo_zero
            ? 64U
            : 1U << static_cast<unsigned>(transaction.op);
        for (unsigned byte = 0; byte < bytes; ++byte) {
            memory_.write_reference_byte(
                address + byte,
                transaction.op == StoreOp::cbo_zero
                    ? 0
                    : static_cast<std::uint8_t>(transaction.data >> (8 * byte)));
        }
        return true;
    }

    bool commit_vector_store(
        const VectorMemoryTransaction &transaction, unsigned timeout = 512)
    {
        if (!transaction.store) {
            error_ = "cannot commit a vector load as a store";
            return false;
        }
        const std::uint64_t target = sq_dequeued_ + transaction.flow_num;
        StoreTransaction commit_point{
            .rob = transaction.rob,
            .rob_flag = transaction.rob_flag,
        };
        if (!commit_stores_through(commit_point, 1)) {
            return false;
        }
        if (!run_until_sq_dequeued(target, timeout)) {
            std::ostringstream message;
            message << error_ << " vector_rob="
                    << static_cast<unsigned>(transaction.rob)
                    << " vector_rob_flag=" << transaction.rob_flag
                    << " vector_sq=" << static_cast<unsigned>(transaction.sq)
                    << " flow_num=" << static_cast<unsigned>(transaction.flow_num)
                    << " sq_target=" << target
                    << " sq_counts=" << sq_dequeued_ << '/' << sq_allocated_;
            error_ = message.str();
            return false;
        }
        const std::uint64_t address = transaction.oracle_address.value_or(
            transaction.address);
        const unsigned element_bytes = 1U << transaction.eew;
        const unsigned elements = 16U >> transaction.eew;
        const std::uint16_t active = active_vector_elements(transaction);
        for (unsigned element = 0; element < elements; ++element) {
            if (((active >> element) & 1U) == 0) {
                continue;
            }
            const std::uint64_t element_address =
                vector_element_address(transaction, element);
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                memory_.write_reference_byte(
                    element_address + byte,
                    transaction.data[element * element_bytes + byte]);
            }
        }
        return true;
    }

    // Stress bursts advance pendingPtr to retire a whole ROB window at once.
    // Keep the architectural reference image synchronized with those DUT
    // commits without issuing a second commit pulse for every store.
    void record_committed_store(const StoreTransaction &transaction)
    {
        const std::uint64_t address = transaction.oracle_address.value_or(
            transaction.address);
        const unsigned bytes = 1U << static_cast<unsigned>(transaction.op);
        for (unsigned byte = 0; byte < bytes; ++byte) {
            memory_.write_reference_byte(
                address + byte,
                static_cast<std::uint8_t>(transaction.data >> (8 * byte)));
        }
    }

    void record_atomic_result(std::uint64_t address, std::uint64_t value)
    {
        for (unsigned byte = 0; byte < 8; ++byte) {
            memory_.write_reference_byte(
                address + byte,
                static_cast<std::uint8_t>(value >> (8 * byte)));
        }
    }

    void record_committed_vector_store(
        const VectorMemoryTransaction &transaction)
    {
        const unsigned element_bytes = 1U << transaction.eew;
        const unsigned elements = 16U >> transaction.eew;
        const std::uint16_t active = active_vector_elements(transaction);
        for (unsigned element = 0; element < elements; ++element) {
            if (((active >> element) & 1U) == 0) {
                continue;
            }
            const std::uint64_t address = vector_element_address(transaction, element);
            for (unsigned byte = 0; byte < element_bytes; ++byte) {
                memory_.write_reference_byte(
                    address + byte,
                    transaction.data[element * element_bytes + byte]);
            }
        }
    }

    bool commit_stores_through(
        const StoreTransaction &transaction, unsigned count)
    {
        if (count == 0 || count > 8) {
            error_ = "store commit count must be between one and eight";
            return false;
        }
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(transaction.rob_flag);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(transaction.rob);
        dut_.io_ooo_to_mem_lsqio_scommit.ImmSet(count);
        tick();
        dut_.io_ooo_to_mem_lsqio_scommit.ImmSet(std::uint64_t{0});
        return check_components();
    }

    bool run_until_sq_dequeued(std::uint64_t target, unsigned timeout = 512)
    {
        for (unsigned cycle = 0; cycle < timeout && sq_dequeued_ < target; ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (sq_dequeued_ < target) {
            error_ = "timed out waiting for committed store to leave SQ";
            return false;
        }
        return check_components();
    }

    bool run_until_release_data(unsigned timeout = 4096)
    {
        for (unsigned cycle = 0;
             cycle < timeout && memory_agent_.release_data_count() == 0; ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (memory_agent_.release_data_count() == 0) {
            error_ = "timed out waiting for DCache ReleaseData";
            return false;
        }
        return check_components();
    }

    bool run_until_release_data_count(
        std::uint64_t target, unsigned timeout = 4096)
    {
        for (unsigned cycle = 0;
             cycle < timeout && memory_agent_.release_data_count() < target;
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (memory_agent_.release_data_count() < target) {
            error_ = "timed out waiting for target DCache ReleaseData count";
            return false;
        }
        return check_components();
    }

    bool check_idle(unsigned cycles)
    {
        for (unsigned cycle = 0; cycle < cycles; ++cycle) {
            tick(false);
            if (!generated::expect_quiescent_outputs(dut_)) {
                error_ = "non-quiescent output during idle smoke interval";
                return false;
            }
        }
        return check_components();
    }

    bool run_cycles(unsigned cycles)
    {
        for (unsigned cycle = 0; cycle < cycles; ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        return true;
    }

    bool redirect_after(std::uint8_t rob_value, bool rob_flag, bool flush_itself)
    {
        dut_.io_redirect_bits_robIdx_flag.ImmSet(rob_flag);
        dut_.io_redirect_bits_robIdx_value.ImmSet(rob_value);
        dut_.io_redirect_bits_level.ImmSet(flush_itself);
        dut_.io_redirect_bits_isVlsException.ImmSet(std::uint64_t{0});
        dut_.io_redirect_valid.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_redirect_valid.ImmSet(std::uint64_t{0});
        return check_components();
    }

    bool ok() const { return error_.empty(); }
    const std::string &error() const { return error_; }

private:
    bool wait_for_enqueue_capacity(
        unsigned lq_needed, unsigned sq_needed, unsigned timeout = 256)
    {
        if (lq_needed > kVirtualLoadQueueEntries - kLqEnqueueHeadroom ||
            sq_needed > kStoreQueueEntries - kSqEnqueueHeadroom) {
            error_ = "LSQ enqueue request exceeds reserved queue capacity";
            return false;
        }
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            const std::uint64_t lq_retired = lq_dequeued_ + lq_canceled_;
            const std::uint64_t sq_retired = sq_dequeued_ + sq_canceled_;
            if (lq_retired > lq_allocated_ || sq_retired > sq_allocated_) {
                error_ = "LSQ software accounting moved past allocation";
                return false;
            }
            const std::uint64_t lq_outstanding = lq_allocated_ - lq_retired;
            const std::uint64_t sq_outstanding = sq_allocated_ - sq_retired;
            const bool capacity_ok =
                lq_outstanding + lq_needed <=
                    kVirtualLoadQueueEntries - kLqEnqueueHeadroom &&
                sq_outstanding + sq_needed <=
                    kStoreQueueEntries - kSqEnqueueHeadroom;
            if (capacity_ok) {
                // canAccept and the queue counters are registered in
                // LsqEnqCtrl. A short quiet interval avoids sampling the
                // pre-retirement value after a commit/redirect transition.
                for (unsigned settle = 0;
                     settle < kEnqueueSettleCycles; ++settle) {
                    tick();
                    if (!check_components()) {
                        return false;
                    }
                }
                return true;
            }
            tick();
            if (!check_components()) {
                return false;
            }
        }
        std::ostringstream message;
        message << "timed out waiting for LSQ enqueue capacity"
                << " lq_needed=" << lq_needed
                << " sq_needed=" << sq_needed
                << " lq=" << lq_allocated_ - (lq_dequeued_ + lq_canceled_)
                << '/' << kVirtualLoadQueueEntries
                << " sq=" << sq_allocated_ - (sq_dequeued_ + sq_canceled_)
                << '/' << kStoreQueueEntries;
        error_ = message.str();
        return false;
    }

    bool write_distributed_csr(std::uint16_t address, std::uint64_t data)
    {
        dut_.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr.ImmSet(address);
        dut_.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data.ImmSet(data);
        dut_.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid.ImmSet(std::uint64_t{0});
        return check_components();
    }

    void tick(bool monitor = true)
    {
        memory_agent_.drive(dut_);
        ptw_agent_.drive(dut_);
        uncache_agent_.drive(dut_);
        dut_.RefreshComb();
        memory_agent_.capture_before_tick(dut_);
        ptw_agent_.capture_before_tick(dut_);
        uncache_agent_.capture_before_tick(dut_);

        // Writeback valid is a combinational projection of the execution-unit
        // output fire.  Observe the pins before the clock edge; after Step()
        // they may already describe the following transaction.  LSQ dequeue
        // pulses are registered separately and are counted below instead.
        if (monitor) {
            for (unsigned lane = 0; lane < kScalarLoadLanes; ++lane) {
                scoreboard_.observe(
                    lane, generated::sample_scalar_load_writeback(dut_, lane));
            }
            for (unsigned lane = 0; lane < kScalarStoreLanes; ++lane) {
                const auto address_writeback =
                    generated::sample_scalar_store_address_writeback(dut_, lane);
                const auto data_writeback =
                    generated::sample_scalar_store_data_writeback(dut_, lane);
                store_scoreboard_.observe_address(
                    lane, address_writeback, cycle());
                store_scoreboard_.observe_data(lane, data_writeback, cycle());
            }
            for (unsigned lane = 0; lane < kVectorMemoryLanes; ++lane) {
                vector_scoreboard_.observe(
                    lane, generated::sample_vector_memory_writeback(dut_, lane));
            }
        }

        dut_.Step();
        memory_agent_.update_after_tick();
        ptw_agent_.update_after_tick();
        uncache_agent_.update_after_tick();
        lq_dequeued_ += dut_.io_mem_to_ooo_lqDeq.U();
        sq_dequeued_ += dut_.io_mem_to_ooo_sqDeq.U();
        if (dut_.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid.B()) {
            ++store_tlb_feedbacks_;
            store_tlb_misses_ +=
                dut_.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit.B() ? 0 : 1;
        }
        if (dut_.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid.B()) {
            ++store_tlb_feedbacks_;
            store_tlb_misses_ +=
                dut_.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit.B() ? 0 : 1;
        }
        if (dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid.B() &&
            !dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit.B()) {
            vector_replay_requests_.push_back(VectorReplayRequest{
                .lane = 0,
                .lq_flag = dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag.B(),
                .lq_value = static_cast<std::uint8_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value.U()),
                .sq_flag = dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag.B(),
                .sq_value = static_cast<std::uint8_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value.U()),
                .is_part_replay = dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_isVecPartReplay.B(),
                .replay_mask = static_cast<std::uint16_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMask.U()),
                .replay_mb_index = static_cast<std::uint8_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMbIdx.U()),
            });
            ++vector_replay_feedbacks_;
        }
        if (dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid.B() &&
            !dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit.B()) {
            vector_replay_requests_.push_back(VectorReplayRequest{
                .lane = 1,
                .lq_flag = dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag.B(),
                .lq_value = static_cast<std::uint8_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value.U()),
                .sq_flag = dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag.B(),
                .sq_value = static_cast<std::uint8_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value.U()),
                .is_part_replay = dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_isVecPartReplay.B(),
                .replay_mask = static_cast<std::uint16_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMask.U()),
                .replay_mb_index = static_cast<std::uint8_t>(
                    dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMbIdx.U()),
            });
            ++vector_replay_feedbacks_;
        }
    }

    bool check_components()
    {
        if (!memory_agent_.ok()) {
            error_ = memory_agent_.error();
        } else if (!ptw_agent_.ok()) {
            error_ = ptw_agent_.error();
        } else if (!uncache_agent_.ok()) {
            error_ = uncache_agent_.error();
        } else if (!scoreboard_.ok()) {
            error_ = scoreboard_.error();
        } else if (!store_scoreboard_.ok()) {
            error_ = store_scoreboard_.error();
        } else if (!vector_scoreboard_.ok()) {
            error_ = vector_scoreboard_.error();
        }
        return error_.empty();
    }

    UTMemBlock dut_;
    SparseMemory bus_memory_;
    SparseMemory memory_;
    TileLinkMemoryAgent memory_agent_;
    PtwMemoryAgent ptw_agent_;
    UncacheMemoryAgent uncache_agent_;
    LoadScoreboard scoreboard_;
    StoreScoreboard store_scoreboard_;
    VectorMemoryScoreboard vector_scoreboard_;
    std::uint64_t pin_space_digest_ = 0;
    std::uint64_t lq_allocated_ = 0;
    std::uint64_t lq_dequeued_ = 0;
    std::uint64_t lq_canceled_ = 0;
    std::uint64_t sq_allocated_ = 0;
    std::uint64_t sq_dequeued_ = 0;
    std::uint64_t sq_canceled_ = 0;
    std::uint64_t store_tlb_feedbacks_ = 0;
    std::uint64_t store_tlb_misses_ = 0;
    std::deque<VectorReplayRequest> vector_replay_requests_;
    std::uint64_t vector_replay_feedbacks_ = 0;
    std::unordered_map<std::uint64_t, std::uint64_t> next_page_table_;
    std::unordered_map<std::uint64_t, std::uint64_t> sv39_l1_tables_;
    std::unordered_map<std::uint64_t, std::uint64_t> sv39_l0_tables_;
    std::unordered_map<std::uint64_t, std::uint64_t> sv48_l2_tables_;
    std::unordered_map<std::uint64_t, std::uint64_t> sv48_l1_tables_;
    std::unordered_map<std::uint64_t, std::uint64_t> sv48_l0_tables_;
    std::unordered_map<std::uint64_t, std::uint64_t> next_gstage_page_table_;
    std::unordered_map<std::uint64_t, std::uint64_t> gstage_l1_tables_;
    std::unordered_map<std::uint64_t, std::uint64_t> gstage_l0_tables_;
    std::unordered_map<std::uint64_t, std::uint64_t> gstage_sv48_l2_tables_;
    std::unordered_map<std::uint64_t, std::uint64_t> gstage_sv48_l1_tables_;
    std::unordered_map<std::uint64_t, std::uint64_t> gstage_sv48_l0_tables_;
    std::string error_;
};

} // namespace memblock
