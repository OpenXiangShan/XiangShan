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
static_assert(generated::kScalarLoadFeedbackLanes == kScalarLoadLanes);
static_assert(generated::kIfetchPrefetchLanes == kScalarLoadLanes);
constexpr unsigned kScalarStoreLanes = 2;
constexpr unsigned kVectorMemoryLanes = 2;
constexpr unsigned kVirtualLoadQueueEntries = 72;
constexpr unsigned kStoreQueueEntries = 56;
constexpr unsigned kVectorUnitStrideMaxFlows = 2;
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
constexpr std::uint64_t kFuTypeVectorSegmentLoad = std::uint64_t{1} << 33;
constexpr std::uint64_t kFuTypeVectorSegmentStore = std::uint64_t{1} << 34;
constexpr std::uint16_t kVectorLoadUnitStride = 0x080;
constexpr std::uint16_t kVectorLoadWholeRegister = 0x088;
constexpr std::uint16_t kVectorLoadFaultOnlyFirst = 0x090;
constexpr std::uint16_t kVectorLoadIndexedUnordered = 0x0a0;
constexpr std::uint16_t kVectorLoadStrided = 0x0c0;
constexpr std::uint16_t kVectorLoadIndexedOrdered = 0x0e0;
constexpr std::uint16_t kVectorStoreUnitStride = 0x100;
constexpr std::uint16_t kVectorStoreWholeRegister = 0x108;
constexpr std::uint16_t kVectorStoreIndexedUnordered = 0x120;
constexpr std::uint16_t kVectorStoreStrided = 0x140;
constexpr std::uint16_t kVectorStoreIndexedOrdered = 0x160;
constexpr std::uint64_t kDefaultMemoryBase = 0x80000000ULL;
// TriggerAction.None is the architectural no-trigger encoding.  Zero is a
// breakpoint action, so leaving this field at zero would inject a breakpoint
// into every software-generated uop.
constexpr std::uint8_t kTriggerNone = 15;
constexpr std::uint8_t kTriggerBreakpoint = 0;
constexpr std::uint8_t kTriggerDebugMode = 1;
constexpr std::uint8_t kTriggerMatchEqual = 0;
constexpr std::uint8_t kTriggerMatchGreaterOrEqual = 2;
constexpr std::uint8_t kTriggerMatchLessThan = 3;
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

constexpr std::uint64_t reference_bitmap_word_address(
    std::uint64_t bitmap_base, std::uint64_t physical_address)
{
    const std::uint64_t physical_page_number = physical_address >> 12;
    return bitmap_base + ((physical_page_number >> 6) << 3);
}

constexpr std::uint64_t reference_bitmap_deny_mask(
    std::uint64_t physical_address)
{
    return std::uint64_t{1} << ((physical_address >> 12) & 63U);
}

enum class PointerMaskingMode : std::uint8_t {
    disabled = 0,
    pmlen7 = 2,
    pmlen16 = 3,
};

struct PointerMaskingConfig {
    PointerMaskingMode machine = PointerMaskingMode::disabled;
    PointerMaskingMode supervisor = PointerMaskingMode::disabled;
    PointerMaskingMode virtual_supervisor = PointerMaskingMode::disabled;
    PointerMaskingMode hypervisor_user = PointerMaskingMode::disabled;
    PointerMaskingMode user = PointerMaskingMode::disabled;
};

constexpr std::uint64_t reference_pointer_mask(
    std::uint64_t address,
    PointerMaskingMode mode,
    bool virtual_address)
{
    const unsigned retained_bits = mode == PointerMaskingMode::pmlen7
        ? 57U
        : mode == PointerMaskingMode::pmlen16 ? 48U : 64U;
    if (retained_bits == 64U) {
        return address;
    }
    const std::uint64_t retained_mask =
        (std::uint64_t{1} << retained_bits) - 1U;
    const std::uint64_t retained = address & retained_mask;
    if (!virtual_address) {
        return retained;
    }
    const std::uint64_t sign_bit = std::uint64_t{1} << (retained_bits - 1U);
    return (retained ^ sign_bit) - sign_bit;
}

static_assert(reference_pointer_mask(
                  0xabcd000012345678ULL,
                  PointerMaskingMode::pmlen16,
                  false) == 0x0000000012345678ULL);
static_assert(reference_pointer_mask(
                  0x1234800012345678ULL,
                  PointerMaskingMode::pmlen16,
                  true) == 0xffff800012345678ULL);

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
    hlvb = 0x10,
    hlvh = 0x11,
    hlvw = 0x12,
    hlvd = 0x13,
    hlvbu = 0x14,
    hlvhu = 0x15,
    hlvwu = 0x16,
    hlvxhu = 0x1d,
    hlvxwu = 0x1e,
};

enum class StoreOp : std::uint16_t {
    sb = 0,
    sh = 1,
    sw = 2,
    sd = 3,
    cbo_zero = 7,
    hsvb = 0x10,
    hsvh = 0x11,
    hsvw = 0x12,
    hsvd = 0x13,
};

constexpr unsigned scalar_store_bytes(StoreOp op)
{
    return op == StoreOp::cbo_zero
        ? 64U
        : 1U << (static_cast<unsigned>(op) & 3U);
}

static_assert(scalar_store_bytes(StoreOp::sb) == 1U);
static_assert(scalar_store_bytes(StoreOp::sd) == 8U);
static_assert(scalar_store_bytes(StoreOp::hsvb) == 1U);
static_assert(scalar_store_bytes(StoreOp::hsvd) == 8U);
static_assert(scalar_store_bytes(StoreOp::cbo_zero) == 64U);

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
    std::int32_t immediate = 0;
    std::optional<std::uint64_t> pc;
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
    std::uint32_t allowed_additional_exception_mask = 0;
    // Only a DebugMode trigger makes the returned data architecturally
    // irrelevant; LoadScoreboard rejects disabling this oracle otherwise.
    bool check_data = true;
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
    std::int32_t immediate = 0;
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

struct MemoryTriggerConfig {
    unsigned index = 0;
    std::uint64_t address = 0;
    std::uint8_t action = kTriggerBreakpoint;
    std::uint8_t match_type = kTriggerMatchEqual;
    std::uint8_t enable_mask = 1;
    bool select = false;
    bool chain = false;
    bool load = true;
    bool store = false;
    bool trigger_can_raise_breakpoint = true;
    bool debug_mode = false;
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
    std::int32_t immediate = 0;
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

inline bool scalar_immediate_is_valid(std::int32_t immediate)
{
    return immediate >= -2048 && immediate <= 2047;
}

inline std::uint64_t scalar_issue_base_address(
    std::uint64_t effective_address, std::int32_t immediate)
{
    const std::int64_t signed_immediate = immediate;
    return signed_immediate < 0
        ? effective_address + static_cast<std::uint64_t>(-signed_immediate)
        : effective_address - static_cast<std::uint64_t>(signed_immediate);
}

enum class VectorAddressingMode : std::uint8_t {
    unit_stride,
    strided,
    indexed_unordered,
    indexed_ordered,
};

struct VectorMemoryTransaction {
    bool store = false;
    bool segment = false;
    bool whole_register = false;
    std::uint64_t address = kDefaultMemoryBase;
    std::optional<std::uint64_t> oracle_address;
    std::array<unsigned char, 16> data{};
    std::array<unsigned char, 16> index{};
    std::optional<std::array<unsigned char, 128>> oracle_index_group;
    std::int64_t stride = 0;
    VectorAddressingMode addressing = VectorAddressingMode::unit_stride;
    std::uint8_t eew = 0;
    std::optional<std::uint8_t> vsew;
    std::uint8_t vl = 16;
    std::optional<std::uint8_t> expected_vl;
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
    std::uint8_t flow_num = kVectorUnitStrideMaxFlows;
    bool is_part_replay = false;
    std::uint16_t replay_mask = 0;
    std::uint8_t replay_mb_index = 0;
    std::uint32_t expected_exception_mask = 0;
    std::uint32_t input_exception_mask = 0;
    std::uint8_t input_trigger = kTriggerNone;
    bool input_flush_pipe = false;
    std::optional<std::uint8_t> expected_trigger;
    std::optional<std::uint8_t> expected_writeback_vstart;
    // As for scalar loads, only DebugMode action may make returned vector
    // data non-architectural.  The scoreboard rejects every other opt-out.
    bool check_data = true;
    std::uint64_t ftq_ptr = 0;
    std::uint8_t ftq_offset = 0;
    std::uint8_t vlmul = 0;
    std::uint8_t vuop_idx = 0;
    bool last_uop = true;
    std::uint8_t nf = 0;
    bool fault_only_first = false;
    bool is_vleff = false;
    bool vl_wen = false;
    std::optional<bool> vec_wen;
    // Lane 0 exposes all three debug classes.  Lane 1 is intentionally
    // checked only when a caller has an explicit expectation because those
    // generated sideband ports are pruned in this top-level build.
    std::optional<bool> expected_debug_is_mmio;
    std::optional<bool> expected_debug_is_ncio;
    std::optional<bool> expected_debug_is_perf_cnt;
};

inline std::uint8_t vector_vsew(
    const VectorMemoryTransaction &transaction)
{
    return transaction.vsew.value_or(transaction.eew);
}

inline bool vector_is_indexed(const VectorMemoryTransaction &transaction)
{
    return transaction.addressing == VectorAddressingMode::indexed_unordered ||
           transaction.addressing == VectorAddressingMode::indexed_ordered;
}

inline std::uint8_t vector_data_eew(
    const VectorMemoryTransaction &transaction)
{
    return vector_is_indexed(transaction)
        ? vector_vsew(transaction)
        : transaction.eew;
}

inline int vector_lmul_log2(const VectorMemoryTransaction &transaction)
{
    const unsigned encoded = transaction.vlmul & 7U;
    if (encoded == 4U) {
        throw std::logic_error("reserved vector LMUL encoding");
    }
    return encoded >= 5U
        ? static_cast<int>(encoded) - 8
        : static_cast<int>(encoded);
}

inline int vector_emul_log2(const VectorMemoryTransaction &transaction)
{
    return static_cast<int>(transaction.eew) -
           static_cast<int>(vector_vsew(transaction)) +
           vector_lmul_log2(transaction);
}

inline bool vector_is_special_indexed(
    const VectorMemoryTransaction &transaction)
{
    return !transaction.segment && vector_is_indexed(transaction) &&
           vector_emul_log2(transaction) > vector_lmul_log2(transaction);
}

inline unsigned vector_indexed_vd_index(
    const VectorMemoryTransaction &transaction)
{
    if (!vector_is_special_indexed(transaction)) {
        return transaction.vuop_idx;
    }
    return transaction.vuop_idx >> static_cast<unsigned>(
        vector_emul_log2(transaction) - vector_lmul_log2(transaction));
}

inline unsigned vector_indexed_split_offset(
    const VectorMemoryTransaction &transaction)
{
    if (!vector_is_special_indexed(transaction)) {
        return 0;
    }
    const unsigned flows_before_uop =
        static_cast<unsigned>(transaction.vuop_idx) * transaction.flow_num;
    const unsigned data_elements_per_vd = 16U >> vector_vsew(transaction);
    const unsigned flows_before_vd =
        vector_indexed_vd_index(transaction) * data_elements_per_vd;
    return flows_before_uop - flows_before_vd;
}

inline int vector_segment_data_mul_log2(
    const VectorMemoryTransaction &transaction)
{
    return vector_is_indexed(transaction)
        ? vector_lmul_log2(transaction)
        : vector_emul_log2(transaction);
}

inline unsigned vector_segment_uops_per_field(
    const VectorMemoryTransaction &transaction)
{
    const int data_mul_log2 = vector_segment_data_mul_log2(transaction);
    return 1U << static_cast<unsigned>(std::max(data_mul_log2, 0));
}

inline unsigned vector_segment_field_index(
    const VectorMemoryTransaction &transaction)
{
    return transaction.vuop_idx / vector_segment_uops_per_field(transaction);
}

inline unsigned vector_segment_vd_index(
    const VectorMemoryTransaction &transaction)
{
    return transaction.vuop_idx % vector_segment_uops_per_field(transaction);
}

inline std::uint16_t vector_fu_op_type(const VectorMemoryTransaction &transaction)
{
    if (transaction.eew > 3 || vector_vsew(transaction) > 3) {
        throw std::logic_error("vector EEW/SEW encoding exceeds 64 bits");
    }
    if (transaction.whole_register) {
        const bool legal_nf = transaction.nf == 0 || transaction.nf == 1 ||
            transaction.nf == 3 || transaction.nf == 7;
        if (transaction.segment || transaction.fault_only_first ||
            transaction.is_vleff ||
            transaction.addressing != VectorAddressingMode::unit_stride ||
            !legal_nf) {
            throw std::logic_error(
                "whole-register vector memory operation has invalid fields");
        }
        return transaction.store
            ? kVectorStoreWholeRegister
            : kVectorLoadWholeRegister;
    }
    if (transaction.fault_only_first) {
        if (transaction.store ||
            transaction.addressing != VectorAddressingMode::unit_stride) {
            throw std::logic_error("vleff must be a unit-stride vector load");
        }
        return kVectorLoadFaultOnlyFirst;
    }
    if (transaction.is_vleff) {
        throw std::logic_error("is_vleff requires a fault-only-first load");
    }
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

inline std::uint8_t vector_effective_vl(
    const VectorMemoryTransaction &transaction)
{
    if (!transaction.whole_register) {
        return transaction.vl;
    }
    const unsigned registers = static_cast<unsigned>(transaction.nf) + 1U;
    return static_cast<std::uint8_t>(
        (registers * 16U) >> transaction.eew);
}

inline std::uint64_t vector_fu_type(const VectorMemoryTransaction &transaction)
{
    if (transaction.segment) {
        return transaction.store
            ? kFuTypeVectorSegmentStore
            : kFuTypeVectorSegmentLoad;
    }
    return transaction.store ? kFuTypeVectorStore : kFuTypeVectorLoad;
}

inline std::uint64_t vector_element_address(
    const VectorMemoryTransaction &transaction, unsigned element)
{
    std::uint64_t base = transaction.oracle_address.value_or(
        transaction.address);
    const unsigned element_bytes = 1U << vector_data_eew(transaction);
    if (!transaction.segment) {
        if (transaction.addressing == VectorAddressingMode::unit_stride) {
            base += static_cast<std::uint64_t>(transaction.vuop_idx) * 16U;
        } else if (transaction.addressing == VectorAddressingMode::strided) {
            const std::int64_t elements_per_uop = 16 / element_bytes;
            const std::int64_t delta = transaction.stride *
                static_cast<std::int64_t>(transaction.vuop_idx) *
                elements_per_uop;
            base = delta >= 0
                ? base + static_cast<std::uint64_t>(delta)
                : base - static_cast<std::uint64_t>(-(delta + 1)) - 1U;
        }
    }
    const unsigned segment_element = transaction.segment
        ? vector_segment_vd_index(transaction) * (16U / element_bytes) +
            element
        : element;
    const unsigned segment_field = transaction.segment
        ? vector_segment_field_index(transaction)
        : 0;
    const std::uint64_t field_offset =
        static_cast<std::uint64_t>(segment_field) * element_bytes;
    switch (transaction.addressing) {
    case VectorAddressingMode::unit_stride:
        if (transaction.segment) {
            return base +
                (segment_element *
                    (static_cast<unsigned>(transaction.nf) + 1U) +
                 segment_field) * element_bytes;
        }
        return base + element * element_bytes;
    case VectorAddressingMode::strided:
        {
            const std::int64_t delta =
                transaction.stride * static_cast<std::int64_t>(
                    segment_element);
            return delta >= 0
                ? base + static_cast<std::uint64_t>(delta) + field_offset
                : base - static_cast<std::uint64_t>(-(delta + 1)) - 1U +
                    field_offset;
        }
    case VectorAddressingMode::indexed_unordered:
    case VectorAddressingMode::indexed_ordered: {
        const unsigned index_bytes = 1U << transaction.eew;
        unsigned index_element = segment_element;
        if (!transaction.segment) {
            const unsigned split_offset =
                vector_indexed_split_offset(transaction);
            if (element < split_offset) {
                throw std::logic_error(
                    "indexed output element precedes this uop's split range");
            }
            const unsigned split_index = element - split_offset;
            const unsigned global_element =
                static_cast<unsigned>(transaction.vuop_idx) *
                    transaction.flow_num + split_index;
            const int emul_log2 = vector_emul_log2(transaction);
            const unsigned index_group_bytes = emul_log2 < 0
                ? 16U >> static_cast<unsigned>(-emul_log2)
                : 16U;
            const unsigned index_elements_per_vreg =
                index_group_bytes >> transaction.eew;
            index_element = global_element & (index_elements_per_vreg - 1U);
        }
        std::uint64_t offset = 0;
        for (unsigned byte = 0; byte < index_bytes; ++byte) {
            const unsigned index_offset = index_element * index_bytes + byte;
            const unsigned char value =
                transaction.segment && transaction.oracle_index_group
                ? transaction.oracle_index_group->at(index_offset)
                : transaction.index.at(index_offset);
            offset |= std::uint64_t{value}
                      << (8 * byte);
        }
        return base + offset + field_offset;
    }
    }
    throw std::logic_error("unknown vector addressing mode");
}

inline std::uint16_t active_vector_elements(
    const VectorMemoryTransaction &transaction)
{
    if (!transaction.segment && vector_is_indexed(transaction)) {
        const unsigned split_offset =
            vector_indexed_split_offset(transaction);
        std::uint16_t result = 0;
        for (unsigned split_index = 0;
             split_index < transaction.flow_num; ++split_index) {
            const unsigned global_element =
                static_cast<unsigned>(transaction.vuop_idx) *
                    transaction.flow_num + split_index;
            const bool in_range = global_element >= transaction.vstart &&
                                  global_element < transaction.vl;
            const bool enabled = transaction.vm ||
                (global_element < 16 &&
                 ((transaction.mask_bits >> global_element) & 1U) != 0);
            if (in_range && enabled) {
                result |= static_cast<std::uint16_t>(
                    1U << (split_offset + split_index));
            }
        }
        return result;
    }
    const unsigned element_count = 16U >> vector_data_eew(transaction);
    const unsigned effective_vl = vector_effective_vl(transaction);
    const unsigned element_base = transaction.segment
        ? vector_segment_vd_index(transaction) * element_count
        : transaction.vuop_idx * element_count;
    std::uint16_t result = 0;
    for (unsigned element = 0; element < element_count; ++element) {
        const unsigned global_element = element_base + element;
        const bool in_range = global_element >= transaction.vstart &&
                              global_element < effective_vl;
        const bool enabled = transaction.vm ||
            (global_element < 16 &&
             ((transaction.mask_bits >> global_element) & 1U) != 0);
        if (in_range && enabled) {
            result |= static_cast<std::uint16_t>(1U << element);
        }
    }
    return result;
}

inline std::optional<unsigned> vector_writeback_global_element(
    const VectorMemoryTransaction &transaction, unsigned element)
{
    const unsigned element_count = 16U >> vector_data_eew(transaction);
    if (transaction.segment) {
        return vector_segment_vd_index(transaction) * element_count + element;
    }
    if (vector_is_indexed(transaction)) {
        return vector_indexed_vd_index(transaction) * element_count + element;
    }
    return transaction.vuop_idx * element_count + element;
}

inline std::uint16_t vector_tail_elements(
    const VectorMemoryTransaction &transaction)
{
    const unsigned element_count = 16U >> vector_data_eew(transaction);
    const unsigned effective_vl = vector_effective_vl(transaction);
    std::uint16_t result = 0;
    for (unsigned element = 0; element < element_count; ++element) {
        const auto global = vector_writeback_global_element(transaction, element);
        if (global && *global >= effective_vl) {
            result |= static_cast<std::uint16_t>(1U << element);
        }
    }
    return result;
}

inline std::uint16_t vector_mask_inactive_elements(
    const VectorMemoryTransaction &transaction)
{
    if (transaction.vm) {
        return 0;
    }
    const unsigned element_count = 16U >> vector_data_eew(transaction);
    const unsigned effective_vl = vector_effective_vl(transaction);
    std::uint16_t result = 0;
    for (unsigned element = 0; element < element_count; ++element) {
        const auto global = vector_writeback_global_element(transaction, element);
        if (global && *global >= transaction.vstart && *global < effective_vl &&
            (*global >= 16 ||
             ((transaction.mask_bits >> *global) & 1U) == 0)) {
            result |= static_cast<std::uint16_t>(1U << element);
        }
    }
    return result;
}

inline std::uint16_t vector_writeback_elements(
    const VectorMemoryTransaction &transaction)
{
    if (!vector_is_special_indexed(transaction)) {
        return active_vector_elements(transaction);
    }
    const unsigned data_elements_per_vd = 16U >> vector_vsew(transaction);
    const unsigned global_base =
        vector_indexed_vd_index(transaction) * data_elements_per_vd;
    std::uint16_t result = 0;
    // VSplit forwards (srcMask >> flowsPrevThisVd)[15:0] to the merge
    // buffer. Bits above the architectural elements in this Vd therefore
    // remain observable on the top-level writeback mask.
    for (unsigned element = 0; element < 16; ++element) {
        const unsigned global_element = global_base + element;
        const bool in_range = global_element >= transaction.vstart &&
                              global_element < transaction.vl;
        const bool enabled = transaction.vm ||
            (global_element < 16 &&
             ((transaction.mask_bits >> global_element) & 1U) != 0);
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
        const unsigned element_bytes = 1U << vector_data_eew(transaction);
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
    bool access_fault = false;
};

// Must remain identical to PAddrBits for the MemBlock configuration.  A
// Sv39/Sv48 PTE carries a 44-bit PPN even when the implementation exposes a
// narrower physical address, so valid PTE encodings can still raise AF.
inline constexpr unsigned kReferencePhysicalAddressBits = 48;

// RISC-V satp/hgatp mode encodings used by the MemBlock CSR interface.
// Keeping the encoding here avoids accidentally testing a mode with a
// different number of page-table levels than the DUT.
enum class ReferencePageMode : std::uint8_t {
    bare = 0,
    sv39 = 8,
    sv48 = 9,
};

// Must remain identical to HasPtwConst in MMUConst.scala.
enum class PtwTranslationMode : std::uint8_t {
    no_stage_two = 0,
    only_stage_one = 1,
    only_stage_two = 2,
    all_stages = 3,
};

enum class ReferencePbmt : std::uint8_t {
    pma = 0,
    nc = 1,
    io = 2,
};

inline ReferencePbmt reference_two_stage_pbmt(
    ReferencePbmt vs_pbmt, ReferencePbmt g_pbmt)
{
    return vs_pbmt == ReferencePbmt::pma ? g_pbmt : vs_pbmt;
}

enum class ReferencePrivilegeMode : std::uint8_t {
    user = 0,
    supervisor = 1,
    machine = 3,
};

struct ReferencePtePermissions {
    bool readable = true;
    bool writable = true;
    bool executable = false;
    bool user = false;
    bool accessed = true;
    bool dirty = true;
};

inline bool reference_load_permitted(
    const ReferencePtePermissions &permissions,
    ReferencePrivilegeMode privilege,
    bool sum,
    bool mxr,
    bool guest_stage = false)
{
    const bool mode_permitted = guest_stage
        ? permissions.user
        : privilege == ReferencePrivilegeMode::user
            ? permissions.user
            : !permissions.user || sum;
    return mode_permitted && permissions.accessed &&
           (permissions.readable || (mxr && permissions.executable));
}

inline bool reference_store_permitted(
    const ReferencePtePermissions &permissions,
    ReferencePrivilegeMode privilege,
    bool sum,
    bool guest_stage = false)
{
    const bool mode_permitted = guest_stage
        ? permissions.user
        : privilege == ReferencePrivilegeMode::user
            ? permissions.user
            : !permissions.user || sum;
    return mode_permitted && permissions.accessed && permissions.dirty &&
           permissions.writable;
}

inline bool reference_hlvx_permitted(
    const ReferencePtePermissions &permissions,
    ReferencePrivilegeMode privilege,
    bool sum,
    bool guest_stage = false)
{
    const bool mode_permitted = guest_stage
        ? permissions.user
        : privilege == ReferencePrivilegeMode::user
            ? permissions.user
            : !permissions.user || sum;
    return mode_permitted && permissions.accessed && permissions.executable;
}

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

inline bool reference_pte_physical_address_fault(std::uint64_t pte)
{
    constexpr unsigned page_offset_bits = 12;
    return (pte & 1U) != 0 &&
           (reference_pte_ppn(pte) >>
            (kReferencePhysicalAddressBits - page_offset_bits)) != 0;
}

inline bool reference_pte_guest_address_fault(
    std::uint64_t pte, ReferencePageMode gstage_mode)
{
    if (gstage_mode == ReferencePageMode::bare || (pte & 1U) == 0) {
        return false;
    }
    constexpr unsigned page_offset_bits = 12;
    const unsigned guest_address_bits =
        gstage_mode == ReferencePageMode::sv48 ? 50U : 41U;
    return (reference_pte_ppn(pte) >>
            (guest_address_bits - page_offset_bits)) != 0;
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

inline bool reference_pte_is_napot(std::uint64_t pte, unsigned level)
{
    constexpr std::uint64_t pte_napot = std::uint64_t{1} << 63;
    return reference_pte_is_leaf(pte) && (pte & pte_napot) != 0 &&
           level == 0 && (reference_pte_ppn(pte) & 0xfU) == 8U;
}

inline bool reference_pte_encoding_fault(
    std::uint64_t pte,
    unsigned level,
    bool pbmte = true)
{
    constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
    constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
    constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
    constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
    constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
    constexpr std::uint64_t pte_accessed = std::uint64_t{1} << 6;
    constexpr std::uint64_t pte_dirty = std::uint64_t{1} << 7;
    constexpr std::uint64_t pte_reserved = std::uint64_t{0x7f} << 54;
    constexpr std::uint64_t pte_pbmt_mask = std::uint64_t{3} << 61;
    constexpr std::uint64_t pte_napot = std::uint64_t{1} << 63;

    const unsigned pbmt = static_cast<unsigned>((pte >> 61) & 3U);
    if ((pte & pte_reserved) != 0 || pbmt == 3U || (!pbmte && pbmt != 0U)) {
        return true;
    }
    const bool valid = (pte & pte_valid) != 0;
    const bool readable = (pte & pte_read) != 0;
    const bool writable = (pte & pte_write) != 0;
    const bool executable = (pte & pte_execute) != 0;
    const bool next = valid && !readable && !writable && !executable;
    if (next) {
        return (pte & (pte_user | pte_accessed | pte_dirty |
                       pte_pbmt_mask | pte_napot)) != 0;
    }
    if (!valid || (!readable && writable)) {
        return true;
    }
    if ((pte & pte_napot) != 0 &&
        (level != 0 || (reference_pte_ppn(pte) & 0xfU) != 8U)) {
        return true;
    }
    const unsigned lower_ppn_bits = 9 * level;
    return reference_pte_is_leaf(pte) && lower_ppn_bits != 0 &&
           (reference_pte_ppn(pte) &
            ((std::uint64_t{1} << lower_ppn_bits) - 1)) != 0;
}

inline std::uint64_t reference_leaf_address(
    std::uint64_t pte, std::uint64_t input_address, unsigned level)
{
    if (reference_pte_is_napot(pte, level)) {
        constexpr std::uint64_t napot_offset_mask = 0xffff;
        constexpr std::uint64_t napot_ppn_mask = ~std::uint64_t{0xf};
        return ((reference_pte_ppn(pte) & napot_ppn_mask) << 12) |
               (input_address & napot_offset_mask);
    }
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
    bool x4 = false,
    bool pbmte = true)
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
        if (reference_pte_encoding_fault(
                pte, static_cast<unsigned>(level), pbmte)) {
            return {false, 0, pte_address, static_cast<unsigned>(level)};
        }
        if (reference_pte_physical_address_fault(pte)) {
            return {
                false,
                0,
                pte_address,
                static_cast<unsigned>(level),
                true,
            };
        }
        if (reference_pte_is_leaf(pte)) {
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

inline std::optional<std::uint64_t> reference_pte_address_at_level(
    const SparseMemory &memory,
    std::uint64_t root_page_table,
    std::uint64_t input_address,
    ReferencePageMode mode,
    unsigned target_level,
    bool x4 = false)
{
    const unsigned levels = reference_page_levels(mode);
    if (levels == 0 || target_level >= levels ||
        (root_page_table & (x4 ? 0x3fffULL : 0xfffULL)) != 0 ||
        (x4 ? !reference_gpa_in_range(input_address, mode)
            : !reference_canonical_virtual_address(input_address, mode))) {
        return std::nullopt;
    }
    std::uint64_t table = root_page_table;
    for (int level = static_cast<int>(levels) - 1;
         level >= static_cast<int>(target_level); --level) {
        const unsigned shift = 12 + 9 * static_cast<unsigned>(level);
        const std::uint64_t index_mask =
            x4 && static_cast<unsigned>(level) == levels - 1 ? 0x7ff : 0x1ff;
        const std::uint64_t index = (input_address >> shift) & index_mask;
        const std::uint64_t pte_address = table + index * 8;
        if (static_cast<unsigned>(level) == target_level) {
            return pte_address;
        }
        const std::uint64_t pte = memory.read_u64(pte_address);
        if (reference_pte_encoding_fault(pte, static_cast<unsigned>(level)) ||
            reference_pte_is_leaf(pte)) {
            return std::nullopt;
        }
        table = reference_pte_ppn(pte) << 12;
    }
    return std::nullopt;
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
    bool access_fault = false;
};

inline ReferenceTwoStageWalkResult reference_two_stage_walk(
    const SparseMemory &memory,
    std::uint64_t vs_root_page_table,
    std::uint64_t g_root_page_table,
    std::uint64_t guest_virtual_address,
    ReferencePageMode vs_mode = ReferencePageMode::sv39,
    ReferencePageMode g_mode = ReferencePageMode::sv39,
    bool vs_pbmte = true,
    bool g_pbmte = true)
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
                memory, g_root_page_table, pte_gpa, g_mode, true, g_pbmte);
            if (!pte_translation.translated) {
                return {
                    false,
                    0,
                    !pte_translation.access_fault,
                    false,
                    pte_gpa,
                    level != 0,
                    pte_translation.access_fault,
                };
            }

            const std::uint64_t pte = memory.read_u64(
                pte_translation.physical_address);
            if (reference_pte_encoding_fault(
                    pte, static_cast<unsigned>(level), vs_pbmte)) {
                return {false, 0, false, true, pte_gpa, false};
            }
            const std::uint64_t generated_address = reference_leaf_address(
                pte, guest_virtual_address, static_cast<unsigned>(level));
            if (g_mode == ReferencePageMode::bare &&
                reference_pte_physical_address_fault(pte)) {
                return {
                    false,
                    0,
                    false,
                    false,
                    generated_address,
                    false,
                    true,
                };
            }
            if (reference_pte_guest_address_fault(pte, g_mode)) {
                return {
                    false,
                    0,
                    true,
                    false,
                    generated_address,
                    level != 0,
                    false,
                };
            }
            if (reference_pte_is_leaf(pte)) {
                guest_physical_address = generated_address;
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
        memory, g_root_page_table, guest_physical_address, g_mode, true,
        g_pbmte);
    if (!final_translation.translated) {
        return {
            false,
            0,
            !final_translation.access_fault,
            false,
            guest_physical_address,
            false,
            final_translation.access_fault,
        };
    }
    return {true, final_translation.physical_address, false, false, 0, false};
}

enum class ResponseLatencyProfile {
    compact,
    spec,
};

enum class PtwCorruptBeat {
    all,
    first,
    last,
};

struct ResponseLatencyProfiles {
    ResponseLatencyProfile dcache = ResponseLatencyProfile::compact;
    ResponseLatencyProfile ptw = ResponseLatencyProfile::compact;
    ResponseLatencyProfile uncache = ResponseLatencyProfile::compact;
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
        e_random_state_ = (seed ^ 0xd1b54a32d192ed03ULL) == 0
            ? 1
            : seed ^ 0xd1b54a32d192ed03ULL;
        random_backpressure_ = enabled;
        latency_profile_ = latency_profile;
        response_latency_stats_ = {};
        forced_next_response_delay_.reset();
        forced_next_interbeat_delay_.reset();
        force_a_stall_ = enabled;
        force_e_stall_ = enabled;
    }

    void inject_next_response_error(bool denied, bool corrupt)
    {
        inject_denied_ = denied;
        inject_corrupt_ = corrupt;
    }

    void force_next_response_delay(unsigned cycles)
    {
        forced_next_response_delay_ = cycles;
    }

    void force_next_interbeat_delay(unsigned cycles)
    {
        forced_next_interbeat_delay_ = cycles;
    }

    void reset_link_state()
    {
        b_beats_.clear();
        d_beats_.clear();
        probe_responses_.clear();
        expected_grant_acks_.clear();
        captured_a_.reset();
        captured_c_.reset();
        release_data_.reset();
        expected_release_lines_.clear();
        a_fire_ = false;
        b_fire_ = false;
        c_fire_ = false;
        d_fire_ = false;
        e_fire_ = false;
        d_gap_ = 0;
        d_presenting_ = false;
        forced_next_response_delay_.reset();
        forced_next_interbeat_delay_.reset();
        inject_denied_ = false;
        inject_corrupt_ = false;
        force_a_stall_ = random_backpressure_;
        force_e_stall_ = random_backpressure_;
        probe_canceled_count_ += probe_request_count_ -
            probe_response_count_ - probe_canceled_count_;
        next_probe_source_ = 0;
        probe_sources_seen_.fill(false);
    }

    void drive(UTMemBlock &dut)
    {
        const bool accept_a = !random_backpressure_ ||
                              (!force_a_stall_ && (next_random() & 3U) != 0);
        const bool accept_c = !random_backpressure_ || (next_random() & 3U) != 0;
        const bool accept_e = !random_backpressure_ ||
                              (!force_e_stall_ && (next_e_random() & 3U) != 0);
        dut.auto_inner_dcache_client_out_a_ready.ImmSet(accept_a);
        dut.auto_inner_dcache_client_out_c_ready.ImmSet(accept_c);
        dut.auto_inner_dcache_client_out_e_ready.ImmSet(accept_e);
        if (b_beats_.empty()) {
            dut.auto_inner_dcache_client_out_b_valid.ImmSet(std::uint64_t{0});
        } else {
            const BBeat &beat = b_beats_.front();
            dut.auto_inner_dcache_client_out_b_bits_opcode.ImmSet(beat.opcode);
            dut.auto_inner_dcache_client_out_b_bits_param.ImmSet(beat.param);
            dut.auto_inner_dcache_client_out_b_bits_size.ImmSet(beat.size);
            dut.auto_inner_dcache_client_out_b_bits_source.ImmSet(beat.source);
            dut.auto_inner_dcache_client_out_b_bits_address.ImmSet(beat.address);
            dut.auto_inner_dcache_client_out_b_bits_mask.ImmSet(beat.mask);
            auto bytes = beat.data;
            dut.auto_inner_dcache_client_out_b_bits_data.ImmSetBytes(bytes);
            dut.auto_inner_dcache_client_out_b_bits_corrupt.ImmSet(beat.corrupt);
            dut.auto_inner_dcache_client_out_b_valid.ImmSet(std::uint64_t{1});
        }
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
        const bool b_valid = dut.auto_inner_dcache_client_out_b_valid.B();
        const bool b_ready = dut.auto_inner_dcache_client_out_b_ready.B();
        if (b_valid && !b_ready) {
            ++probe_stall_cycles_;
        }
        b_fire_ = b_valid && b_ready;
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
                dut.auto_inner_dcache_client_out_c_bits_corrupt.B(),
                dut.auto_inner_dcache_client_out_c_bits_data.GetBytes(),
            };
            if (captured_c_->opcode < 4 || captured_c_->opcode > 7) {
                error_ = "unsupported DCache TileLink C opcode";
            }
            if (captured_c_->size > 6) {
                error_ = "oversized DCache TileLink C request";
            }
        }
        const bool e_valid = dut.auto_inner_dcache_client_out_e_valid.B();
        const bool e_ready = dut.auto_inner_dcache_client_out_e_ready.B();
        if (e_valid && !e_ready) {
            ++grant_ack_stall_cycles_;
            force_e_stall_ = false;
        }
        e_fire_ = e_valid && e_ready;
        if (e_fire_) {
            if (expected_grant_acks_.empty()) {
                error_ = "unexpected DCache TileLink E GrantAck";
            } else if (dut.auto_inner_dcache_client_out_e_bits_sink.U() !=
                       expected_grant_acks_.front()) {
                error_ = "DCache TileLink E GrantAck sink mismatch";
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
        if (b_fire_) {
            const std::uint8_t source = b_beats_.front().source;
            if (!probe_sources_seen_[source]) {
                probe_sources_seen_[source] = true;
                ++probe_source_count_;
            }
            b_beats_.pop_front();
            ++probe_request_count_;
            max_probe_outstanding_ = std::max(
                max_probe_outstanding_,
                probe_request_count_ - probe_response_count_ -
                    probe_canceled_count_);
        }
        if (d_fire_) {
            if (d_beats_.front().opcode == 5) {
                ++grant_data_beat_count_;
            }
            d_beats_.pop_front();
            d_presenting_ = false;
            d_gap_ = d_beats_.empty() ? 0 : d_beats_.front().delay_before;
        }
        if (a_fire_ && captured_a_) {
            last_request_address_ = captured_a_->address;
            respond(*captured_a_);
            ++request_count_;
        }
        if (c_fire_ && captured_c_) {
            if (captured_c_->opcode == 4 || captured_c_->opcode == 5) {
                accept_probe_response(*captured_c_);
            } else {
                accept_release(*captured_c_);
            }
        }
        if (e_fire_ && !expected_grant_acks_.empty()) {
            expected_grant_acks_.pop_front();
            ++grant_ack_count_;
        }
        a_fire_ = false;
        b_fire_ = false;
        c_fire_ = false;
        d_fire_ = false;
        e_fire_ = false;
        captured_a_.reset();
        captured_c_.reset();
    }

    bool ok() const { return error_.empty(); }
    const std::string &error() const { return error_; }
    std::uint64_t request_count() const { return request_count_; }
    std::uint64_t last_request_address() const { return last_request_address_; }
    std::uint64_t get_count() const { return get_count_; }
    std::uint64_t refill_count() const { return refill_count_; }
    std::uint64_t keyword_refill_count() const
    {
        return keyword_refill_count_;
    }
    std::uint64_t nonkeyword_refill_count() const
    {
        return nonkeyword_refill_count_;
    }
    std::uint64_t acquire_perm_count() const { return acquire_perm_count_; }
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
    std::uint64_t probe_request_count() const { return probe_request_count_; }
    std::uint64_t probe_response_count() const { return probe_response_count_; }
    std::uint64_t probe_data_count() const { return probe_data_count_; }
    std::uint64_t probe_source_count() const { return probe_source_count_; }
    std::uint64_t max_probe_outstanding() const
    {
        return max_probe_outstanding_;
    }
    std::uint64_t probe_stall_cycles() const { return probe_stall_cycles_; }
    std::uint64_t grant_ack_count() const { return grant_ack_count_; }
    std::uint64_t grant_data_beat_count() const
    {
        return grant_data_beat_count_;
    }
    std::uint64_t grant_ack_stall_cycles() const
    {
        return grant_ack_stall_cycles_;
    }

    bool request_probe(
        std::uint64_t address, std::uint8_t cap, bool need_data,
        std::uint8_t expected_report,
        const std::vector<unsigned char> &expected_data = {})
    {
        if ((address & (kLineBytes - 1)) != 0 || cap > 2 ||
            expected_report > 5 ||
            (!expected_data.empty() && expected_data.size() != kLineBytes)) {
            error_ = "invalid DCache Probe request or expectation";
            return false;
        }
        std::vector<unsigned char> data(kBeatBytes, 0);
        // XiangShan carries the virtual-index alias in B.data[2:1] and the
        // manager's data request in B.data[0]. Bare mappings use PA == VA.
        data[0] = static_cast<unsigned char>(
            (need_data ? 1U : 0U) | ((address >> 11) & 0x6U));
        const std::uint8_t source = next_probe_source_;
        if (std::any_of(
                probe_responses_.begin(), probe_responses_.end(),
                [source](const auto &response) {
                    return response.b_source == source;
                })) {
            error_ = "DCache Probe source reused while still outstanding";
            return false;
        }
        next_probe_source_ = static_cast<std::uint8_t>(
            (next_probe_source_ + 1U) & 0x3fU);
        b_beats_.push_back(BBeat{
            6, cap, 6, source, address, 0xffffffffU,
            std::move(data), false});
        probe_responses_.push_back(ProbeResponseState{
            source, address, expected_report, expected_data, 0});
        return true;
    }

    bool probes_idle() const
    {
        return b_beats_.empty() && probe_responses_.empty();
    }
    bool grant_acks_idle() const { return expected_grant_acks_.empty(); }
    void expect_release_line(
        std::uint64_t base, const std::vector<unsigned char> &bytes)
    {
        if (bytes.empty() || bytes.size() % kBeatBytes != 0) {
            error_ = "expected ReleaseData line is not beat sized";
            return;
        }
        expected_release_lines_[base] = bytes;
    }

    void clear_release_line_expectations()
    {
        expected_release_lines_.clear();
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
    static constexpr std::size_t kLineBytes = 64;

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

    struct BBeat {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t address;
        std::uint32_t mask;
        std::vector<unsigned char> data;
        bool corrupt;
    };

    struct CRequest {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t address;
        bool keyword;
        bool corrupt;
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

    struct ProbeResponseState {
        std::uint8_t b_source;
        std::uint64_t base;
        std::uint8_t report;
        std::vector<unsigned char> expected_data;
        std::size_t received;
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
            ++get_count_;
            const std::uint64_t beat_base = request.address & ~(kBeatBytes - 1);
            push_response(DBeat{
                1, 0, request.size, request.source, 0, request.keyword,
                memory_.read_beat(beat_base, kBeatBytes), denied,
                corrupt || denied,
            }, true);
            break;
        }
        case 6: { // AcquireBlock -> GrantData
            ++refill_count_;
            if (request.keyword) {
                ++keyword_refill_count_;
            } else {
                ++nonkeyword_refill_count_;
            }
            const std::uint8_t cap = request.param == 0 ? 1 : 0;
            const std::uint16_t sink =
                static_cast<std::uint16_t>(1U + request.source);
            const std::size_t beats = static_cast<std::size_t>(
                transfer_bytes > kBeatBytes ? transfer_bytes / kBeatBytes : 1);
            for (std::size_t beat = 0; beat < beats; ++beat) {
                const std::size_t memory_beat = request.keyword ? beat ^ 1U : beat;
                push_response(DBeat{
                    5, cap, request.size, request.source, sink, request.keyword,
                    memory_.read_beat(base + memory_beat * kBeatBytes, kBeatBytes),
                    denied, corrupt || denied,
                }, beat == 0);
            }
            expected_grant_acks_.push_back(sink);
            break;
        }
        case 7: { // AcquirePerm -> Grant
            ++acquire_perm_count_;
            if (corrupt) {
                error_ = "cannot inject corrupt on a data-less DCache Grant";
                return;
            }
            const std::uint16_t sink =
                static_cast<std::uint16_t>(1U + request.source);
            push_response(DBeat{
                4, 0, request.size, request.source, sink, request.keyword,
                std::vector<unsigned char>(kBeatBytes, 0),
                denied, false,
            }, true);
            expected_grant_acks_.push_back(sink);
            break;
        }
        default: {
            std::ostringstream message;
            message << "unsupported DCache TileLink A opcode "
                    << static_cast<unsigned>(request.opcode);
            error_ = message.str();
            break;
        }
        }
    }

    void accept_probe_response(const CRequest &response)
    {
        const auto expected_it = std::find_if(
            probe_responses_.begin(), probe_responses_.end(),
            [&](const auto &expected) {
                return expected.base == response.address;
            });
        if (expected_it == probe_responses_.end()) {
            error_ = "unexpected DCache ProbeAck address";
            return;
        }
        ProbeResponseState &expected = *expected_it;
        const bool with_data = !expected.expected_data.empty();
        const std::uint8_t expected_opcode = with_data ? 5 : 4;
        if (response.opcode != expected_opcode || response.param != expected.report ||
            response.size != 6 || response.address != expected.base ||
            response.corrupt) {
            std::ostringstream message;
            message << "DCache ProbeAck identity or permission mismatch"
                    << " expected_opcode="
                    << static_cast<unsigned>(expected_opcode)
                    << " actual_opcode="
                    << static_cast<unsigned>(response.opcode)
                    << " expected_param="
                    << static_cast<unsigned>(expected.report)
                    << " actual_param="
                    << static_cast<unsigned>(response.param)
                    << " expected_size=6 actual_size="
                    << static_cast<unsigned>(response.size)
                    << " expected_address=0x" << std::hex << expected.base
                    << " actual_address=0x" << response.address << std::dec
                    << " corrupt=" << response.corrupt
                    << " beat=" << expected.received;
            error_ = message.str();
            return;
        }
        if (with_data) {
            const std::size_t offset = expected.received * kBeatBytes;
            if (offset + kBeatBytes > expected.expected_data.size()) {
                error_ = "DCache ProbeAckData exceeded one cache line";
                return;
            }
            for (std::size_t byte = 0; byte < kBeatBytes; ++byte) {
                if (response.data.at(byte) != expected.expected_data[offset + byte]) {
                    std::ostringstream message;
                    message << "DCache ProbeAckData byte mismatch base=0x"
                            << std::hex << expected.base << " beat=" << std::dec
                            << expected.received << " byte=" << byte;
                    error_ = message.str();
                    return;
                }
                memory_.write_byte(
                    expected.base + offset + byte, response.data.at(byte));
            }
            ++expected.received;
            if (expected.received != kLineBytes / kBeatBytes) {
                return;
            }
            ++probe_data_count_;
        }
        probe_responses_.erase(expected_it);
        ++probe_response_count_;
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

    std::uint64_t next_e_random()
    {
        e_random_state_ ^= e_random_state_ << 13;
        e_random_state_ ^= e_random_state_ >> 7;
        e_random_state_ ^= e_random_state_ << 17;
        return e_random_state_;
    }

    unsigned response_delay(bool first_beat)
    {
        if (first_beat && forced_next_response_delay_) {
            const unsigned delay = *forced_next_response_delay_;
            forced_next_response_delay_.reset();
            response_latency_stats_.sample(delay);
            return delay;
        }
        if (!first_beat && forced_next_interbeat_delay_) {
            const unsigned delay = *forced_next_interbeat_delay_;
            forced_next_interbeat_delay_.reset();
            return delay;
        }
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
    std::deque<BBeat> b_beats_;
    std::deque<DBeat> d_beats_;
    std::deque<ProbeResponseState> probe_responses_;
    std::deque<std::uint16_t> expected_grant_acks_;
    std::optional<ARequest> captured_a_;
    std::optional<CRequest> captured_c_;
    std::optional<ReleaseDataState> release_data_;
    std::unordered_map<std::uint64_t, std::vector<unsigned char>>
        expected_release_lines_;
    bool a_fire_ = false;
    bool b_fire_ = false;
    bool c_fire_ = false;
    bool d_fire_ = false;
    bool e_fire_ = false;
    std::uint64_t request_count_ = 0;
    std::uint64_t last_request_address_ = 0;
    std::uint64_t get_count_ = 0;
    std::uint64_t refill_count_ = 0;
    std::uint64_t keyword_refill_count_ = 0;
    std::uint64_t nonkeyword_refill_count_ = 0;
    std::uint64_t acquire_perm_count_ = 0;
    std::uint64_t release_count_ = 0;
    std::uint64_t release_data_count_ = 0;
    std::uint64_t release_data_verified_count_ = 0;
    std::uint64_t probe_request_count_ = 0;
    std::uint64_t probe_response_count_ = 0;
    std::uint64_t probe_canceled_count_ = 0;
    std::uint64_t probe_data_count_ = 0;
    std::uint64_t probe_source_count_ = 0;
    std::uint64_t max_probe_outstanding_ = 0;
    std::uint64_t probe_stall_cycles_ = 0;
    std::uint64_t grant_ack_count_ = 0;
    std::uint64_t grant_data_beat_count_ = 0;
    std::uint64_t grant_ack_stall_cycles_ = 0;
    std::uint8_t next_probe_source_ = 0;
    std::array<bool, 64> probe_sources_seen_{};
    std::uint64_t random_state_ = 1;
    std::uint64_t e_random_state_ = 1;
    unsigned d_gap_ = 0;
    bool random_backpressure_ = false;
    ResponseLatencyProfile latency_profile_ = ResponseLatencyProfile::compact;
    ResponseLatencyStats response_latency_stats_;
    bool force_a_stall_ = false;
    bool force_e_stall_ = false;
    bool d_presenting_ = false;
    bool inject_denied_ = false;
    bool inject_corrupt_ = false;
    std::optional<unsigned> forced_next_response_delay_;
    std::optional<unsigned> forced_next_interbeat_delay_;
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
    std::string error_;
};

class PtwMemoryAgent {
public:
    explicit PtwMemoryAgent(SparseMemory &memory) : memory_(memory) {}

    void inject_response_error_after(
        unsigned clean_requests, bool denied, bool corrupt,
        PtwCorruptBeat corrupt_beat = PtwCorruptBeat::all)
    {
        if (!denied && !corrupt) {
            throw std::invalid_argument(
                "PTW response error injection requires denied or corrupt");
        }
        if (!corrupt && corrupt_beat != PtwCorruptBeat::all) {
            throw std::invalid_argument(
                "PTW corrupt beat selection requires a corrupt injection");
        }
        pending_response_error_ = PendingResponseError{
            clean_requests, denied, corrupt, corrupt_beat};
    }

    void configure_backpressure(
        std::uint64_t seed, bool enabled,
        ResponseLatencyProfile latency_profile = ResponseLatencyProfile::compact)
    {
        random_state_ = seed == 0 ? 1 : seed;
        random_backpressure_ = enabled;
        latency_profile_ = latency_profile;
        response_latency_stats_ = {};
        outstanding_requests_ = 0;
        max_outstanding_requests_ = 0;
        forced_next_response_delay_.reset();
        force_a_stall_ = enabled;
    }

    void force_next_response_delay(unsigned cycles)
    {
        forced_next_response_delay_ = cycles;
    }

    void reset_link_state()
    {
        responses_.clear();
        request_.reset();
        a_fire_ = false;
        d_fire_ = false;
        d_gap_ = 0;
        d_presenting_ = false;
        outstanding_requests_ = 0;
        forced_next_response_delay_.reset();
        force_a_stall_ = random_backpressure_;
        pending_response_error_.reset();
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
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_denied.ImmSet(response.denied);
        auto data = response.data;
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_data.ImmSetBytes(data);
        dut.auto_inner_ptw_to_l2_buffer_out_d_bits_corrupt.ImmSet(response.corrupt);
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
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_denied.B() != expected.denied ||
                dut.auto_inner_ptw_to_l2_buffer_out_d_bits_corrupt.B() != expected.corrupt ||
                actual_data != expected.data) {
                error_ = "PTW TileLink D response identity or payload mismatch";
            }
        }
    }

    void update_after_tick()
    {
        if (d_fire_) {
            const bool completed_request = responses_.front().last_beat;
            responses_.pop_front();
            if (completed_request) {
                --outstanding_requests_;
            }
            d_presenting_ = false;
            d_gap_ = responses_.empty() ? 0 : responses_.front().delay_before;
        }
        if (a_fire_ && request_) {
            request_->response_delay = respond(*request_);
            request_history_.push_back(*request_);
            ++request_count_;
            ++outstanding_requests_;
            max_outstanding_requests_ = std::max(
                max_outstanding_requests_, outstanding_requests_);
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
    std::uint64_t error_response_requests() const
    {
        return error_response_requests_;
    }
    std::uint64_t last_error_response_address() const
    {
        return last_error_response_address_;
    }
    std::uint8_t last_error_response_source() const
    {
        return last_error_response_source_;
    }
    std::uint64_t max_outstanding_requests() const
    {
        return max_outstanding_requests_;
    }
    const ResponseLatencyStats &response_latency_stats() const
    {
        return response_latency_stats_;
    }
    bool request_covers_address_since(
        std::uint64_t address, std::uint64_t first_request) const
    {
        if (first_request >= request_history_.size()) {
            return false;
        }
        for (std::size_t index = static_cast<std::size_t>(first_request);
             index < request_history_.size(); ++index) {
            const Request &request = request_history_[index];
            const std::uint64_t bytes = std::uint64_t{1} << request.size;
            const std::uint64_t base = request.address & ~(bytes - 1);
            if (address >= base && address - base < bytes) {
                return true;
            }
        }
        return false;
    }

    std::uint64_t request_covering_count_since(
        std::uint64_t address, std::uint64_t first_request) const
    {
        std::uint64_t count = 0;
        for (std::size_t index = static_cast<std::size_t>(first_request);
             index < request_history_.size(); ++index) {
            const Request &request = request_history_[index];
            const std::uint64_t bytes = std::uint64_t{1} << request.size;
            const std::uint64_t base = request.address & ~(bytes - 1);
            count += address >= base && address - base < bytes;
        }
        return count;
    }

    bool request_covering_address_has_min_delay_since(
        std::uint64_t address,
        std::uint64_t first_request,
        unsigned min_response_delay) const
    {
        if (first_request >= request_history_.size()) {
            return false;
        }
        for (std::size_t index = static_cast<std::size_t>(first_request);
             index < request_history_.size(); ++index) {
            const Request &request = request_history_[index];
            const std::uint64_t bytes = std::uint64_t{1} << request.size;
            const std::uint64_t base = request.address & ~(bytes - 1);
            if (address >= base && address - base < bytes) {
                return request.response_delay >= min_response_delay;
            }
        }
        return false;
    }

private:
    static constexpr std::size_t kBeatBytes = 32;

    struct Request {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t address;
        unsigned response_delay = 0;
    };

    struct Response {
        std::uint8_t opcode;
        std::uint8_t param;
        std::uint8_t size;
        std::uint8_t source;
        std::vector<unsigned char> data;
        bool denied = false;
        bool corrupt = false;
        bool last_beat = true;
        unsigned delay_before = 0;
    };

    struct PendingResponseError {
        unsigned clean_requests;
        bool denied;
        bool corrupt;
        PtwCorruptBeat corrupt_beat;
    };

    unsigned respond(const Request &request)
    {
        if (request.opcode != 4 && request.opcode != 6) {
            std::ostringstream message;
            message << "unsupported PTW TileLink A opcode "
                    << static_cast<unsigned>(request.opcode);
            error_ = message.str();
            return 0;
        }
        const std::uint64_t transfer_bytes = std::uint64_t{1} << request.size;
        const std::uint64_t base = request.address & ~(transfer_bytes - 1);
        const std::size_t beats = static_cast<std::size_t>(
            transfer_bytes > kBeatBytes ? transfer_bytes / kBeatBytes : 1);
        bool denied = false;
        bool corrupt = false;
        PtwCorruptBeat corrupt_beat = PtwCorruptBeat::all;
        if (pending_response_error_) {
            if (pending_response_error_->clean_requests == 0) {
                denied = pending_response_error_->denied;
                corrupt = pending_response_error_->corrupt;
                corrupt_beat = pending_response_error_->corrupt_beat;
                pending_response_error_.reset();
                ++error_response_requests_;
                last_error_response_address_ = request.address;
                last_error_response_source_ = request.source;
            } else {
                --pending_response_error_->clean_requests;
            }
        }
        unsigned first_response_delay = 0;
        for (std::size_t beat = 0; beat < beats; ++beat) {
            const bool selected_corrupt_beat =
                corrupt_beat == PtwCorruptBeat::all ||
                (corrupt_beat == PtwCorruptBeat::first && beat == 0) ||
                (corrupt_beat == PtwCorruptBeat::last && beat + 1 == beats);
            const unsigned delay = push_response(Response{
                static_cast<std::uint8_t>(request.opcode == 4 ? 1 : 5),
                static_cast<std::uint8_t>(request.opcode == 4 ? 0 : 1),
                request.size,
                request.source,
                memory_.read_beat(base + beat * kBeatBytes, kBeatBytes),
                denied,
                // denied is fixed across a multibeat D response and implies
                // corrupt on every beat carrying data.  Independent corrupt
                // errors may legally identify only the affected data beat.
                denied || (corrupt && selected_corrupt_beat),
                beat + 1 == beats,
            }, beat == 0);
            if (beat == 0) {
                first_response_delay = delay;
            }
        }
        return first_response_delay;
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
        if (first_beat && forced_next_response_delay_) {
            const unsigned delay = *forced_next_response_delay_;
            forced_next_response_delay_.reset();
            response_latency_stats_.sample(delay);
            return delay;
        }
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

    unsigned push_response(Response response, bool first_beat)
    {
        const bool was_empty = responses_.empty();
        response.delay_before = response_delay(first_beat);
        const unsigned delay = response.delay_before;
        responses_.push_back(std::move(response));
        if (was_empty) {
            d_presenting_ = false;
            d_gap_ = responses_.front().delay_before;
        }
        return delay;
    }

    SparseMemory &memory_;
    std::deque<Response> responses_;
    std::vector<Request> request_history_;
    std::optional<Request> request_;
    bool a_fire_ = false;
    bool d_fire_ = false;
    std::uint64_t request_count_ = 0;
    std::uint64_t random_state_ = 1;
    unsigned d_gap_ = 0;
    bool random_backpressure_ = false;
    ResponseLatencyProfile latency_profile_ = ResponseLatencyProfile::compact;
    ResponseLatencyStats response_latency_stats_;
    std::optional<unsigned> forced_next_response_delay_;
    std::optional<PendingResponseError> pending_response_error_;
    bool force_a_stall_ = false;
    bool d_presenting_ = false;
    std::uint64_t outstanding_requests_ = 0;
    std::uint64_t max_outstanding_requests_ = 0;
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
    std::uint64_t error_response_requests_ = 0;
    std::uint64_t last_error_response_address_ = 0;
    std::uint8_t last_error_response_source_ = 0;
    std::string error_;
};

class UncacheMemoryAgent {
public:
    struct DeviceAccess {
        std::uint64_t sequence;
        bool write;
        std::uint8_t size;
        std::uint8_t source;
        std::uint64_t address;
        std::uint8_t mask;
        std::uint64_t data;
        std::uint64_t read_data;
        bool denied;
        bool corrupt;
    };

    explicit UncacheMemoryAgent(SparseMemory &memory) : memory_(memory) {}

    void configure_device_window(
        std::uint64_t base, std::uint64_t size, bool read_clear)
    {
        if (size == 0) {
            throw std::invalid_argument("uncache device window cannot be empty");
        }
        device_window_ = DeviceWindow{base, size, read_clear};
        device_accesses_.clear();
    }

    const std::vector<DeviceAccess> &device_accesses() const
    {
        return device_accesses_;
    }

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
        outstanding_requests_ = 0;
        max_outstanding_requests_ = 0;
        forced_next_response_delay_.reset();
        force_a_stall_ = enabled;
    }

    void force_next_response_delay(unsigned cycles)
    {
        forced_next_response_delay_ = cycles;
    }

    void reset_link_state()
    {
        responses_.clear();
        request_.reset();
        a_fire_ = false;
        d_fire_ = false;
        d_gap_ = 0;
        d_presenting_ = false;
        outstanding_requests_ = 0;
        forced_next_response_delay_.reset();
        force_a_stall_ = random_backpressure_;
        inject_denied_ = false;
        inject_corrupt_ = false;
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
            --outstanding_requests_;
            d_presenting_ = false;
            d_gap_ = responses_.empty() ? 0 : responses_.front().delay_before;
        }
        if (a_fire_ && request_) {
            respond(*request_);
            ++request_count_;
            ++outstanding_requests_;
            max_outstanding_requests_ = std::max(
                max_outstanding_requests_, outstanding_requests_);
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
    std::uint64_t max_outstanding_requests() const
    {
        return max_outstanding_requests_;
    }
    std::uint64_t outstanding_requests() const
    {
        return outstanding_requests_;
    }
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

    struct DeviceWindow {
        std::uint64_t base;
        std::uint64_t size;
        bool read_clear;
    };

    bool is_device_request(const Request &request) const
    {
        if (!device_window_ || request.size > 3 ||
            request.address < device_window_->base) {
            return false;
        }
        const std::uint64_t transfer_bytes = std::uint64_t{1} << request.size;
        const std::uint64_t offset = request.address - device_window_->base;
        return transfer_bytes <= device_window_->size &&
               offset <= device_window_->size - transfer_bytes;
    }

    void record_device_access(
        const Request &request, std::uint64_t read_data,
        bool denied, bool corrupt)
    {
        device_accesses_.push_back(DeviceAccess{
            device_accesses_.size(), request.opcode != 4, request.size,
            request.source, request.address, request.mask, request.data,
            read_data, denied, corrupt,
        });
    }

    void respond(const Request &request)
    {
        const bool denied = inject_denied_;
        const bool corrupt = inject_corrupt_;
        inject_denied_ = false;
        inject_corrupt_ = false;
        if (request.opcode == 4) {
            const bool response_corrupt = corrupt || denied;
            const std::uint64_t beat_base = request.address & ~std::uint64_t{7};
            const std::uint64_t read_data = memory_.read_u64(beat_base);
            if (is_device_request(request)) {
                record_device_access(
                    request, read_data, denied, response_corrupt);
                if (device_window_->read_clear &&
                    !denied && !response_corrupt) {
                    for (unsigned byte = 0; byte < 8; ++byte) {
                        if (((request.mask >> byte) & 1U) != 0) {
                            memory_.write_byte(beat_base + byte, 0);
                        }
                    }
                }
            }
            push_response(Response{
                1, request.size, request.source,
                // TileLink returns the complete 8-byte beat. LoadUnit selects
                // the requested byte lane later using the physical address.
                read_data,
                denied, response_corrupt,
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
        if (corrupt) {
            error_ = "cannot inject corrupt on a data-less Uncache AccessAck";
            return;
        }
        const std::uint64_t beat_base = request.address & ~std::uint64_t{7};
        const bool device_request = is_device_request(request);
        if (device_request) {
            record_device_access(request, 0, denied, false);
        }
        if (!denied) {
            for (unsigned byte = 0; byte < 8; ++byte) {
                if (((request.mask >> byte) & 1U) != 0) {
                    memory_.write_byte(
                        beat_base + byte,
                        static_cast<std::uint8_t>(request.data >> (8 * byte)));
                }
            }
        }
        push_response(
            Response{0, request.size, request.source, 0, denied, false}, true);
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
        if (forced_next_response_delay_) {
            const unsigned delay = *forced_next_response_delay_;
            forced_next_response_delay_.reset();
            response_latency_stats_.sample(delay);
            return delay;
        }
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
    std::optional<unsigned> forced_next_response_delay_;
    bool force_a_stall_ = false;
    bool d_presenting_ = false;
    std::uint64_t outstanding_requests_ = 0;
    std::uint64_t max_outstanding_requests_ = 0;
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
    bool inject_denied_ = false;
    bool inject_corrupt_ = false;
    std::optional<DeviceWindow> device_window_;
    std::vector<DeviceAccess> device_accesses_;
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
        std::uint32_t allowed_additional_exception_mask;
        bool check_data;
        bool check_data_on_exception;
        bool rf_wen;
        bool fp_wen;
        std::uint8_t trigger;
        bool flush_pipe;
        std::optional<bool> debug_is_mmio;
        std::optional<bool> debug_is_ncio;
        std::optional<bool> debug_is_perf_cnt;
        std::uint64_t address;
        std::uint16_t op;
    };

    void expect(const LoadTransaction &transaction, std::uint64_t data)
    {
        if (!transaction.check_data &&
            transaction.expected_trigger != kTriggerDebugMode) {
            if (error_.empty()) {
                error_ = "load data checking may only be disabled for DebugMode trigger";
            }
            return;
        }
        const auto [_, inserted] = expected_.emplace(
            rob_identity(transaction.rob, transaction.rob_flag),
            Expected{
                data,
                transaction.pdest,
                transaction.rob_flag,
                false,
                transaction.expected_exception_mask,
                transaction.allowed_additional_exception_mask,
                transaction.check_data,
                transaction.check_data_on_exception,
                transaction.expected_exception_mask == 0 && transaction.rf_wen,
                transaction.expected_exception_mask == 0 && transaction.fp_wen,
                transaction.expected_trigger,
                transaction.input_flush_pipe,
                transaction.expected_debug_is_mmio,
                transaction.expected_debug_is_ncio,
                transaction.expected_debug_is_perf_cnt,
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
                0, 0, transaction.rob_flag, true, 0, 0, false, false, false,
                false,
                transaction.expected_trigger, transaction.input_flush_pipe,
                transaction.expected_debug_is_mmio,
                transaction.expected_debug_is_ncio,
                transaction.expected_debug_is_perf_cnt,
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
                optional_mismatch(
                    writeback.debug_is_mmio, it->second.debug_is_mmio) ||
                optional_mismatch(
                    writeback.debug_is_ncio, it->second.debug_is_ncio) ||
                optional_mismatch(
                    writeback.debug_is_perf_cnt,
                    it->second.debug_is_perf_cnt)) {
                fail("mismatched software-prefetch completion", lane, writeback,
                     &it->second);
                return;
            }
            ++prefetch_observed_;
            --pending_prefetch_;
            expected_.erase(it);
            return;
        }
        const std::uint32_t missing_exception =
            it->second.exception_mask & ~writeback.exception_mask;
        const std::uint32_t unexpected_exception = writeback.exception_mask &
            ~(it->second.exception_mask |
              it->second.allowed_additional_exception_mask);
        if (missing_exception != 0 || unexpected_exception != 0 ||
            writeback.replay || writeback.rf_wen != it->second.rf_wen ||
            writeback.fp_wen != it->second.fp_wen ||
            writeback.trigger != it->second.trigger ||
            writeback.flush_pipe != it->second.flush_pipe ||
            writeback.pdest != it->second.pdest ||
            writeback.rob_flag != it->second.rob_flag ||
            optional_mismatch(
                writeback.debug_is_mmio, it->second.debug_is_mmio) ||
            optional_mismatch(
                writeback.debug_is_ncio, it->second.debug_is_ncio) ||
            optional_mismatch(
                writeback.debug_is_perf_cnt,
                it->second.debug_is_perf_cnt) ||
            (it->second.check_data &&
             (it->second.exception_mask == 0 ||
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
    static bool optional_mismatch(
        bool actual,
        const std::optional<bool> &expected)
    {
        return expected.has_value() && actual != *expected;
    }

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
                    << " allowed_additional_exception=0x"
                    << expected->allowed_additional_exception_mask
                    << " expected_data=0x" << std::hex << expected->data
                    << " check_data=" << std::dec << expected->check_data
                    << " check_data_on_exception=" << std::dec
                    << expected->check_data_on_exception
                    << " expected_rf_wen=" << std::dec << expected->rf_wen
                    << " expected_fp_wen=" << expected->fp_wen
                    << " expected_flush_pipe=" << expected->flush_pipe
                    << " expected_trigger=" << static_cast<unsigned>(expected->trigger)
                    << " expected_mmio=";
            if (expected->debug_is_mmio.has_value()) {
                message << *expected->debug_is_mmio;
            } else {
                message << "unchecked";
            }
            message << " expected_ncio=";
            if (expected->debug_is_ncio.has_value()) {
                message << *expected->debug_is_ncio;
            } else {
                message << "unchecked";
            }
            message << " expected_perf_cnt=";
            if (expected->debug_is_perf_cnt.has_value()) {
                message << *expected->debug_is_perf_cnt;
            } else {
                message << "unchecked";
            }
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
                        << " expected_flush_pipe=" << it->second.flush_pipe
                        << " mmio=" << writeback.debug_is_mmio
                        << " ncio=" << writeback.debug_is_ncio
                        << " expected_mmio=";
                if (it->second.debug_is_mmio.has_value()) {
                    message << *it->second.debug_is_mmio;
                } else {
                    message << "unchecked";
                }
                message << " expected_ncio=";
                if (it->second.debug_is_ncio.has_value()) {
                    message << *it->second.debug_is_ncio;
                } else {
                    message << "unchecked";
                }
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
        bool segment;
        std::array<unsigned char, 16> data;
        std::uint16_t active_elements;
        std::uint16_t fu_op_type;
        std::uint8_t eew;
        std::uint8_t vsew;
        std::uint8_t vlmul;
        std::uint8_t vl;
        std::uint8_t vstart;
        std::uint8_t vuop_idx;
        std::uint8_t nf;
        std::uint8_t pdest;
        bool rob_flag;
        std::uint32_t exception_mask;
        std::optional<std::uint8_t> trigger;
        std::optional<std::uint8_t> expected_writeback_vstart;
        bool check_data;
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
        std::uint16_t tail_elements;
        std::uint16_t mask_inactive_elements;
    };

    void expect(
        const VectorMemoryTransaction &transaction,
        const std::array<unsigned char, 16> &data)
    {
        if (!transaction.check_data &&
            transaction.expected_trigger != kTriggerDebugMode) {
            if (error_.empty()) {
                error_ = "vector data checking may only be disabled for DebugMode trigger";
            }
            return;
        }
        auto output_transaction = transaction;
        output_transaction.vl = transaction.expected_vl.value_or(
            vector_effective_vl(transaction));
        const RobIdentity identity = rob_identity(
            transaction.rob, transaction.rob_flag);
        const auto range = expected_.equal_range(identity);
        const bool duplicate = std::any_of(
            range.first, range.second, [&](const auto &entry) {
                return entry.second.vuop_idx == transaction.vuop_idx &&
                       entry.second.pdest == transaction.pdest;
            });
        if (duplicate) {
            if (error_.empty()) {
                error_ = "duplicate outstanding vector memory uop";
            }
            return;
        }
        expected_.emplace(
            identity,
            Expected{
                transaction.store,
                transaction.segment,
                data,
                vector_writeback_elements(output_transaction),
                vector_fu_op_type(transaction),
                transaction.eew,
                vector_vsew(transaction),
                transaction.vlmul,
                output_transaction.vl,
                transaction.vstart,
                transaction.vuop_idx,
                transaction.nf,
                transaction.pdest,
                transaction.rob_flag,
                transaction.expected_exception_mask,
                transaction.expected_trigger,
                transaction.expected_writeback_vstart,
                transaction.check_data,
                transaction.vec_wen.value_or(
                    !transaction.store && !transaction.vl_wen),
                false,
                transaction.vl_wen,
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
                vector_tail_elements(output_transaction),
                vector_mask_inactive_elements(output_transaction),
            });
    }

    void observe(unsigned lane, const generated::VectorMemoryWriteback &writeback)
    {
        if (!writeback.valid) {
            return;
        }
        const auto range = expected_.equal_range(
            rob_identity(writeback.rob_value, writeback.rob_flag));
        const auto it = std::find_if(
            range.first, range.second, [&](const auto &entry) {
                return entry.second.vuop_idx == writeback.vuop_idx &&
                       entry.second.pdest == writeback.pdest;
            });
        if (it == range.second) {
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
        const bool vstart_mismatch = expected.expected_writeback_vstart
            ? writeback.vstart != *expected.expected_writeback_vstart
            : (!exception_progress && writeback.vstart != 0);
        if ((expected.segment && lane != 0) ||
            writeback.exception_mask != expected.exception_mask || writeback.replay ||
            writeback.flush_pipe != expected.flush_pipe ||
            trigger_mismatch ||
            writeback.vec_wen != expected.vec_wen ||
            writeback.v0_wen != expected.v0_wen ||
            writeback.vl_wen != expected.vl_wen ||
            debug_mismatch ||
            writeback.fu_op_type != expected.fu_op_type ||
            writeback.rob_flag != expected.rob_flag ||
            vstart_mismatch ||
            ((!exception_progress) &&
             (writeback.vsew != expected.vsew ||
              writeback.veew != expected.eew ||
              writeback.vlmul != expected.vlmul ||
              writeback.vl != expected.vl ||
              writeback.vuop_idx != expected.vuop_idx ||
              writeback.nf != expected.nf))) {
            fail("mismatched vector memory metadata", lane, writeback, &expected);
            return;
        }
        if (expected.vl_wen && expected.exception_mask == 0) {
            const bool vl_data_matches =
                writeback.data.size() == 16 &&
                writeback.data[0] == expected.vl &&
                std::all_of(
                    writeback.data.begin() + 1, writeback.data.end(),
                    [](unsigned char byte) { return byte == 0; });
            const bool mask_matches =
                writeback.vmask.size() == 16 &&
                std::all_of(
                    writeback.vmask.begin(), writeback.vmask.end(),
                    [](unsigned char byte) { return byte == 0xff; });
            if ((expected.segment ? lane != 0 : lane != 1) ||
                writeback.vec_wen ||
                writeback.pdest != expected.pdest ||
                !vl_data_matches || !mask_matches) {
                fail("mismatched vector FOF fix-VL writeback", lane, writeback,
                     &expected);
                return;
            }
            ++fof_fix_observed_;
        } else if (!expected.store && expected.exception_mask == 0 &&
                   expected.vec_wen) {
            if (!writeback.vec_wen || writeback.pdest != expected.pdest ||
                (expected.check_data &&
                 !matches_load_data(writeback.data, expected)) ||
                !matches_active_mask(writeback.vmask, expected.active_elements)) {
                fail("mismatched vector load data", lane, writeback, &expected);
                return;
            }
            ++load_observed_;
        } else if (!expected.store && expected.exception_mask == 0) {
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
    std::uint64_t fof_fix_observed() const { return fof_fix_observed_; }
    const std::string &error() const { return error_; }

private:
    static bool matches_load_data(
        const std::vector<unsigned char> &actual, const Expected &expected)
    {
        if (actual.size() != expected.data.size()) {
            return false;
        }
        const unsigned element_bytes = 1U <<
            (expected.addressing == VectorAddressingMode::indexed_unordered ||
             expected.addressing == VectorAddressingMode::indexed_ordered
                ? expected.vsew
                : expected.eew);
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
            const bool tail_agnostic =
                ((expected.tail_elements >> element) & 1U) != 0 && expected.vta;
            const bool mask_agnostic =
                ((expected.mask_inactive_elements >> element) & 1U) != 0 &&
                expected.vma;
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
                << " sew/eew/lmul=" << static_cast<unsigned>(actual.vsew)
                << '/' << static_cast<unsigned>(actual.veew) << '/'
                << static_cast<unsigned>(actual.vlmul);
        if (expected != nullptr) {
            message << " expected_op=0x" << std::hex << expected->fu_op_type
                    << " expected_exception=0x" << expected->exception_mask
                    << " expected_trigger="
                    << static_cast<unsigned>(expected->trigger.value_or(kVectorWritebackTriggerNone))
                    << " check_data=" << std::dec << expected->check_data
                    << " expected_writeback_vstart=";
            if (expected->expected_writeback_vstart) {
                message << static_cast<unsigned>(
                    *expected->expected_writeback_vstart);
            } else {
                message << "default";
            }
            message
                    << " expected_active=0x" << std::hex
                    << expected->active_elements
                    << " address=0x" << expected->address << std::dec
                    << " expected_sew/eew/lmul="
                    << static_cast<unsigned>(expected->vsew) << '/'
                    << static_cast<unsigned>(expected->eew) << '/'
                    << static_cast<unsigned>(expected->vlmul)
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

    std::unordered_multimap<RobIdentity, Expected, RobIdentityHash> expected_;
    std::uint64_t load_observed_ = 0;
    std::uint64_t store_observed_ = 0;
    std::uint64_t fof_fix_observed_ = 0;
    std::string error_;
};

struct L2PrefetchControl {
    bool master_enable = false;
    bool receive_enable = false;
    bool pbop_enable = false;
    bool vbop_enable = false;
    bool tp_enable = false;
    std::uint16_t delay_latency = 0;

    bool operator==(const L2PrefetchControl &) const = default;
};

struct ExternalInterruptState {
    bool msip = false;
    bool mtip = false;
    bool meip = false;
    bool seip = false;
    bool debug = false;
    bool nmi_31 = false;
    bool nmi_43 = false;

    bool operator==(const ExternalInterruptState &) const = default;
};

constexpr unsigned kTraceGroups = 3;

struct TraceGroupState {
    bool valid = false;
    std::uint64_t iaddr = 0;
    std::uint8_t ftq_offset = 0;
    std::uint8_t itype = 0;
    std::uint8_t iretire = 0;
    bool ilastsize = false;
};

struct TraceBridgeStimulus {
    bool encoder_enable = false;
    bool encoder_stall = false;
    std::uint8_t privilege = 0;
    std::uint64_t mstatus = 0;
    std::uint64_t trap_cause = 0;
    std::uint64_t trap_tval = 0;
    std::array<TraceGroupState, kTraceGroups> groups{};
};

struct TopBridgeStimulus {
    bool msi_ack = false;
    bool frontend_reset = false;
    bool beu_valid = false;
    std::uint64_t beu_address = 0;
    bool msi_info_valid = false;
    std::uint16_t msi_info = 0;
    bool clint_time_valid = false;
    std::uint64_t clint_time = 0;
    bool interrupt_msip = false;
    bool interrupt_mtip = false;
    bool interrupt_meip = false;
    bool interrupt_seip = false;
    bool interrupt_debug = false;
    bool interrupt_nmi_31 = false;
    bool interrupt_nmi_43 = false;
    bool interrupt_beu_local = false;
    std::array<
        std::uint8_t, generated::kHcPerfEventHighestInputLane + 1>
        hc_perf_events{};
    L2PrefetchControl l2_prefetch{};
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

    static generated::VectorMemoryIssue make_vector_memory_issue(
        const VectorMemoryTransaction &transaction)
    {
        generated::VectorMemoryIssue issue;
        issue.ftq_ptr = transaction.ftq_ptr;
        issue.ftq_offset = transaction.ftq_offset;
        issue.fu_type = vector_fu_type(transaction);
        issue.fu_op_type = vector_fu_op_type(transaction);
        issue.vec_wen = transaction.vec_wen.value_or(
            !transaction.store && !transaction.vl_wen);
        issue.vl_wen = transaction.vl_wen;
        issue.vma = transaction.vma;
        issue.vta = transaction.vta;
        issue.vsew = vector_vsew(transaction);
        issue.vlmul = transaction.vlmul;
        issue.vm = transaction.vm;
        issue.vstart = transaction.vstart;
        issue.vuop_idx = transaction.vuop_idx;
        issue.last_uop = transaction.last_uop;
        issue.nf = transaction.nf;
        issue.veew = transaction.eew;
        issue.is_vleff = transaction.is_vleff;
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
        return issue;
    }

public:
    struct IFetchPtwResponse {
        std::uint8_t s2xlate = 0;
        std::uint64_t s1_tag = 0;
        std::uint16_t s1_asid = 0;
        std::uint16_t s1_vmid = 0;
        bool s1_n = false;
        std::uint8_t s1_pbmt = 0;
        bool s1_d = false;
        bool s1_a = false;
        bool s1_g = false;
        bool s1_u = false;
        bool s1_x = false;
        bool s1_w = false;
        bool s1_r = false;
        std::uint8_t s1_level = 0;
        bool s1_v = false;
        std::uint64_t s1_ppn = 0;
        std::uint8_t s1_addr_low = 0;
        std::array<std::uint8_t, 8> s1_ppn_low{};
        std::uint8_t s1_valididx = 0;
        std::uint8_t s1_pteidx = 0;
        bool s1_pf = false;
        bool s1_af = false;
        std::uint64_t s2_tag = 0;
        std::uint16_t s2_vmid = 0;
        bool s2_n = false;
        std::uint8_t s2_pbmt = 0;
        std::uint64_t s2_ppn = 0;
        bool s2_d = false;
        bool s2_a = false;
        bool s2_g = false;
        bool s2_u = false;
        bool s2_x = false;
        bool s2_w = false;
        bool s2_r = false;
        std::uint8_t s2_level = 0;
        bool s2_gpf = false;
        bool s2_gaf = false;

        bool operator==(const IFetchPtwResponse &other) const
        {
            return s2xlate == other.s2xlate && s1_tag == other.s1_tag &&
                s1_asid == other.s1_asid && s1_vmid == other.s1_vmid &&
                s1_n == other.s1_n && s1_pbmt == other.s1_pbmt &&
                s1_d == other.s1_d && s1_a == other.s1_a &&
                s1_g == other.s1_g && s1_u == other.s1_u &&
                s1_x == other.s1_x && s1_w == other.s1_w &&
                s1_r == other.s1_r && s1_level == other.s1_level &&
                s1_v == other.s1_v && s1_ppn == other.s1_ppn &&
                s1_addr_low == other.s1_addr_low &&
                s1_ppn_low == other.s1_ppn_low &&
                s1_valididx == other.s1_valididx &&
                s1_pteidx == other.s1_pteidx && s1_pf == other.s1_pf &&
                s1_af == other.s1_af && s2_tag == other.s2_tag &&
                s2_vmid == other.s2_vmid && s2_n == other.s2_n &&
                s2_pbmt == other.s2_pbmt && s2_ppn == other.s2_ppn &&
                s2_d == other.s2_d && s2_a == other.s2_a &&
                s2_g == other.s2_g && s2_u == other.s2_u &&
                s2_x == other.s2_x && s2_w == other.s2_w &&
                s2_r == other.s2_r && s2_level == other.s2_level &&
                s2_gpf == other.s2_gpf && s2_gaf == other.s2_gaf;
        }
    };

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

    struct FrontendBridgeStats {
        std::uint64_t requests = 0;
        std::uint64_t responses = 0;
        std::uint64_t request_stalls = 0;
        std::uint64_t response_stalls = 0;
        std::uint64_t source_credit_stalls = 0;
        std::uint64_t field_checks = 0;
    };

    struct ScalarLoadWakeupSample {
        bool valid = false;
        bool rf_wen = false;
        bool fp_wen = false;
        std::uint8_t pdest = 0;
        std::uint64_t cycle = 0;
    };

    struct ScalarLoadFeedbackStats {
        std::array<std::uint64_t, kScalarLoadLanes> wakeups{};
        std::array<std::uint64_t, kScalarLoadLanes> ld2_cancels{};
        std::array<ScalarLoadWakeupSample, kScalarLoadLanes> last_wakeup{};
        std::array<std::uint64_t, kScalarLoadLanes> last_cancel_cycle{};
    };

    struct StoreSlowFeedbackSample {
        unsigned lane = 0;
        bool hit = false;
        bool sq_flag = false;
        std::uint8_t sq_value = 0;
        std::uint64_t cycle = 0;
    };

    struct VectorStoreSlowFeedbackSample {
        unsigned lane = 0;
        bool hit = false;
        bool lq_flag = false;
        std::uint8_t lq_value = 0;
        bool sq_flag = false;
        std::uint8_t sq_value = 0;
        bool is_part_replay = false;
        std::uint16_t replay_mask = 0;
        std::uint8_t replay_mb_index = 0;
        std::uint64_t cycle = 0;
    };

    struct IqSlowFeedbackStats {
        std::array<std::uint64_t, kScalarStoreLanes> sta_valid{};
        std::array<std::uint64_t, kScalarStoreLanes> sta_hits{};
        std::array<std::uint64_t, kScalarStoreLanes> sta_misses{};
        std::array<std::uint64_t, kVectorMemoryLanes> vstu_valid{};
        std::array<std::uint64_t, kVectorMemoryLanes> vstu_hits{};
        std::array<std::uint64_t, kVectorMemoryLanes> vstu_misses{};
        std::vector<StoreSlowFeedbackSample> sta_samples;
        std::vector<VectorStoreSlowFeedbackSample> vstu_samples;
    };

    struct MemoryViolationStats {
        std::uint64_t count = 0;
        generated::MemoryViolation last{};
        std::uint64_t last_cycle = 0;
    };

    struct IfetchPrefetchStats {
        std::array<std::uint64_t, kScalarLoadLanes> requests{};
        std::array<std::uint64_t, kScalarLoadLanes> last_vaddr{};
        std::array<std::uint64_t, kScalarLoadLanes> last_cycle{};
    };

    struct HardwarePrefetchStats {
        std::uint64_t l2_requests = 0;
        std::uint64_t l3_requests = 0;
        std::array<std::uint64_t, 32> l2_source_counts{};
        std::array<std::vector<std::uint64_t>, 32> l2_addresses_by_source{};
        std::array<std::uint64_t, 32> last_l2_addr_by_source{};
        std::array<std::uint64_t, 32> last_l2_cycle_by_source{};
        std::uint64_t last_l2_addr = 0;
        std::uint8_t last_l2_source = 0;
        std::uint64_t last_l2_cycle = 0;
        std::uint64_t last_l3_addr = 0;
        std::uint64_t last_l3_cycle = 0;
    };

    struct BusErrorStats {
        std::uint64_t dcache_reports = 0;
        std::uint64_t uncache_reports = 0;
        std::uint64_t last_dcache_address = 0;
        std::uint64_t last_uncache_address = 0;
    };

    struct TopDownStats {
        std::uint64_t replay_allocate_cycles = 0;
        std::uint64_t sq_full_cycles = 0;
        std::uint64_t sb_full_cycles = 0;
        std::uint64_t l1_miss_cycles = 0;
        std::uint64_t l2_miss_cycles = 0;
        std::uint64_t l3_miss_cycles = 0;
        std::uint64_t delay_checks = 0;
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
    void clear_release_line_expectations()
    {
        memory_agent_.clear_release_line_expectations();
    }
    bool request_dcache_probe(
        std::uint64_t address, std::uint8_t cap, bool need_data,
        std::uint8_t expected_report,
        const std::vector<unsigned char> &expected_data = {})
    {
        return memory_agent_.request_probe(
            address, cap, need_data, expected_report, expected_data);
    }
    void configure_backpressure(
        std::uint64_t seed, bool enabled,
        ResponseLatencyProfile latency_profile = ResponseLatencyProfile::compact)
    {
        configure_backpressure(
            seed, enabled,
            ResponseLatencyProfiles{
                latency_profile, latency_profile, latency_profile});
    }

    void configure_backpressure(
        std::uint64_t seed, bool enabled,
        const ResponseLatencyProfiles &latency_profiles)
    {
        memory_agent_.configure_backpressure(
            seed, enabled, latency_profiles.dcache);
        ptw_agent_.configure_backpressure(
            seed ^ 0x9e3779b97f4a7c15ULL, enabled, latency_profiles.ptw);
        uncache_agent_.configure_backpressure(
            seed ^ 0x3c6ef372fe94f82aULL, enabled, latency_profiles.uncache);
    }

    void inject_next_dcache_response_error(bool denied, bool corrupt)
    {
        memory_agent_.inject_next_response_error(denied, corrupt);
    }

    void inject_ptw_response_error_after(
        unsigned clean_requests, bool denied, bool corrupt,
        PtwCorruptBeat corrupt_beat = PtwCorruptBeat::all)
    {
        ptw_agent_.inject_response_error_after(
            clean_requests, denied, corrupt, corrupt_beat);
    }

    void force_next_dcache_response_delay(unsigned cycles)
    {
        memory_agent_.force_next_response_delay(cycles);
    }

    void force_next_dcache_interbeat_delay(unsigned cycles)
    {
        memory_agent_.force_next_interbeat_delay(cycles);
    }

    void inject_next_uncache_response_error(bool denied, bool corrupt)
    {
        uncache_agent_.inject_next_response_error(denied, corrupt);
    }

    void force_next_ptw_response_delay(unsigned cycles)
    {
        ptw_agent_.force_next_response_delay(cycles);
    }

    void force_next_uncache_response_delay(unsigned cycles)
    {
        uncache_agent_.force_next_response_delay(cycles);
    }

    void configure_uncache_device(
        std::uint64_t base, std::uint64_t size, bool read_clear)
    {
        uncache_agent_.configure_device_window(base, size, read_clear);
    }

    const std::vector<UncacheMemoryAgent::DeviceAccess> &
    uncache_device_accesses() const
    {
        return uncache_agent_.device_accesses();
    }

    void configure_cache_error_enable(bool enable)
    {
        dut_.io_ooo_to_mem_csrCtrl_cache_error_enable.ImmSet(enable);
    }

    void configure_ldld_violation_check(bool enable)
    {
        dut_.io_ooo_to_mem_csrCtrl_ldld_vio_check_enable.ImmSet(enable);
    }

    bool configure_stride_prefetch(bool enable)
    {
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable.ImmSet(enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable.ImmSet(enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold.ImmSet(
            std::uint64_t{12});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride.ImmSet(
            std::uint64_t{30});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride.ImmSet(enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable.ImmSet(
            enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency.ImmSet(
            std::uint64_t{0});
        return run_cycles(4);
    }

    bool configure_sms_pht_prefetch(bool output_enable)
    {
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_train_on_hit.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_agt.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_pht.ImmSet(
            output_enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_threshold.ImmSet(
            std::uint64_t{12});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_active_stride.ImmSet(
            std::uint64_t{30});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l1D_pf_enable_stride.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_store_only.ImmSet(
            std::uint64_t{0});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable.ImmSet(
            std::uint64_t{1});
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency.ImmSet(
            std::uint64_t{0});
        return run_cycles(4);
    }

    bool expect_l2_prefetch_control(bool master_enabled, bool receive_enabled)
    {
        dut_.RefreshComb();
        const bool matches =
            dut_.io_outer_l2PfCtrl_l2_pf_master_en.B() == master_enabled &&
            dut_.io_outer_l2PfCtrl_l2_pf_recv_en.B() == receive_enabled &&
            dut_.io_outer_l2PfCtrl_l2_pbop_en.B() &&
            dut_.io_outer_l2PfCtrl_l2_vbop_en.B() &&
            dut_.io_outer_l2PfCtrl_l2_tp_en.B() &&
            dut_.io_outer_l2PfCtrl_l2_pf_delay_latency.U() == 0;
        if (!matches) {
            std::ostringstream message;
            message << "L2 prefetch control output mismatch master="
                    << dut_.io_outer_l2PfCtrl_l2_pf_master_en.B()
                    << " recv="
                    << dut_.io_outer_l2PfCtrl_l2_pf_recv_en.B()
                    << " pbop=" << dut_.io_outer_l2PfCtrl_l2_pbop_en.B()
                    << " vbop=" << dut_.io_outer_l2PfCtrl_l2_vbop_en.B()
                    << " tp=" << dut_.io_outer_l2PfCtrl_l2_tp_en.B()
                    << " delay="
                    << dut_.io_outer_l2PfCtrl_l2_pf_delay_latency.U();
            error_ = message.str();
            return false;
        }
        return check_components();
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
                // The retained L2 PMP response comes from a leaveHitMux
                // checker, whose match and config are registered one cycle
                // after the TLB response payload becomes valid.
                tick(false);
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

    bool start_ifetch_ptw_request(
        std::uint64_t vpn, PtwTranslationMode mode, unsigned timeout = 16384)
    {
        if ((vpn >> 38) != 0) {
            error_ = "invalid IFetch PTW request";
            return false;
        }
        const auto s2xlate = static_cast<std::uint8_t>(mode);

        dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{0});
        dut_.io_fetch_to_mem_itlb_req_0_bits_vpn.ImmSet(vpn);
        dut_.io_fetch_to_mem_itlb_req_0_bits_s2xlate.ImmSet(s2xlate);
        dut_.io_fetch_to_mem_itlb_req_0_valid.ImmSet(std::uint64_t{1});
        bool accepted = false;
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            dut_.RefreshComb();
            const bool fire = dut_.io_fetch_to_mem_itlb_req_0_ready.B();
            tick(false);
            if (fire) {
                accepted = true;
                break;
            }
            if (!check_components()) {
                break;
            }
        }
        dut_.io_fetch_to_mem_itlb_req_0_valid.ImmSet(std::uint64_t{0});
        if (!accepted) {
            if (error_.empty()) {
                error_ = "timed out accepting IFetch PTW request";
            }
            dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{1});
            return false;
        }
        ++ifetch_ptw_pending_;
        return check_components();
    }

    bool complete_ifetch_ptw_request(
        IFetchPtwResponse &response, unsigned response_stall_cycles = 4,
        unsigned timeout = 16384)
    {
        if (ifetch_ptw_pending_ == 0) {
            error_ = "no IFetch PTW request pending";
            return false;
        }
        const auto capture = [&]() {
            IFetchPtwResponse result;
            result.s2xlate = static_cast<std::uint8_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s2xlate.U());
            result.s1_tag = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_tag.U();
            result.s1_asid = static_cast<std::uint16_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_asid.U());
            result.s1_vmid = static_cast<std::uint16_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid.U());
            result.s1_n = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_n.B();
            result.s1_pbmt = static_cast<std::uint8_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt.U());
            result.s1_d = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d.B();
            result.s1_a = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a.B();
            result.s1_g = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g.B();
            result.s1_u = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u.B();
            result.s1_x = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x.B();
            result.s1_w = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w.B();
            result.s1_r = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r.B();
            result.s1_level = static_cast<std::uint8_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_level.U());
            result.s1_v = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_v.B();
            result.s1_ppn = dut_.io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn.U();
            result.s1_addr_low = static_cast<std::uint8_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_addr_low.U());
#define CAPTURE_IFETCH_SECTOR(index)                                           \
            result.s1_ppn_low[index] = static_cast<std::uint8_t>(             \
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_##index.U());  \
            result.s1_valididx |= static_cast<std::uint8_t>(                  \
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_##index.B())  \
                << index;                                                     \
            result.s1_pteidx |= static_cast<std::uint8_t>(                    \
                dut_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_##index.B())    \
                << index
            CAPTURE_IFETCH_SECTOR(0);
            CAPTURE_IFETCH_SECTOR(1);
            CAPTURE_IFETCH_SECTOR(2);
            CAPTURE_IFETCH_SECTOR(3);
            CAPTURE_IFETCH_SECTOR(4);
            CAPTURE_IFETCH_SECTOR(5);
            CAPTURE_IFETCH_SECTOR(6);
            CAPTURE_IFETCH_SECTOR(7);
#undef CAPTURE_IFETCH_SECTOR
            result.s1_pf = dut_.io_fetch_to_mem_itlb_resp_bits_s1_pf.B();
            result.s1_af = dut_.io_fetch_to_mem_itlb_resp_bits_s1_af.B();
            result.s2_tag = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag.U();
            result.s2_vmid = static_cast<std::uint16_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid.U());
            result.s2_n = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_n.B();
            result.s2_pbmt = static_cast<std::uint8_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt.U());
            result.s2_ppn = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn.U();
            result.s2_d = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d.B();
            result.s2_a = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a.B();
            result.s2_g = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g.B();
            result.s2_u = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u.B();
            result.s2_x = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x.B();
            result.s2_w = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w.B();
            result.s2_r = dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r.B();
            result.s2_level = static_cast<std::uint8_t>(
                dut_.io_fetch_to_mem_itlb_resp_bits_s2_entry_level.U());
            result.s2_gpf = dut_.io_fetch_to_mem_itlb_resp_bits_s2_gpf.B();
            result.s2_gaf = dut_.io_fetch_to_mem_itlb_resp_bits_s2_gaf.B();
            return result;
        };

        bool observed = false;
        IFetchPtwResponse held;
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            if (dut_.io_fetch_to_mem_itlb_resp_valid.B()) {
                held = capture();
                observed = true;
                break;
            }
            tick(false);
        }
        if (!observed) {
            error_ = "timed out waiting for IFetch PTW response";
            dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{1});
            ifetch_ptw_pending_ = 0;
            return false;
        }
        for (unsigned cycle = 0; cycle < response_stall_cycles; ++cycle) {
            if (!dut_.io_fetch_to_mem_itlb_resp_valid.B() ||
                !(capture() == held)) {
                error_ = "IFetch PTW response changed while stalled";
                dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{1});
                ifetch_ptw_pending_ = 0;
                return false;
            }
            tick(false);
        }
        response = held;
        dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{1});
        tick(false);
        --ifetch_ptw_pending_;
        if (ifetch_ptw_pending_ != 0) {
            dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{0});
            dut_.RefreshComb();
        } else if (dut_.io_fetch_to_mem_itlb_resp_valid.B()) {
            error_ = "IFetch PTW response did not retire after handshake";
            return false;
        }
        return check_components();
    }

    bool issue_ifetch_ptw_request(
        std::uint64_t vpn, PtwTranslationMode mode,
        IFetchPtwResponse &response, unsigned response_stall_cycles = 4,
        unsigned timeout = 16384)
    {
        if (ifetch_ptw_pending_ != 0) {
            error_ = "synchronous IFetch PTW request requires an empty queue";
            return false;
        }
        return start_ifetch_ptw_request(vpn, mode, timeout) &&
            complete_ifetch_ptw_request(
                response, response_stall_cycles, timeout);
    }

    bool confirm_ifetch_ptw_flushed(unsigned quiet_cycles = 1024)
    {
        if (ifetch_ptw_pending_ == 0) {
            error_ = "no pending IFetch PTW request to flush";
            return false;
        }
        dut_.io_fetch_to_mem_itlb_req_0_valid.ImmSet(std::uint64_t{0});
        dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{1});
        ifetch_ptw_pending_ = 0;
        for (unsigned cycle = 0; cycle < quiet_cycles; ++cycle) {
            dut_.RefreshComb();
            if (dut_.io_fetch_to_mem_itlb_resp_valid.B()) {
                error_ = "flushed IFetch PTW request produced a stale response";
                return false;
            }
            tick(false);
            if (!check_components()) {
                return false;
            }
        }
        dut_.RefreshComb();
        if (dut_.io_fetch_to_mem_itlb_resp_valid.B()) {
            error_ = "flushed IFetch PTW request produced a stale response";
            return false;
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

    bool configure_memory_trigger(const MemoryTriggerConfig &config)
    {
        if (config.index >= 4 || config.action >= 16 ||
            (config.match_type != kTriggerMatchEqual &&
             config.match_type != kTriggerMatchGreaterOrEqual &&
             config.match_type != kTriggerMatchLessThan) ||
            config.enable_mask >= 16) {
            error_ = "invalid memory trigger configuration";
            return false;
        }
        // The CSR block presents a two-cycle delayed tdata update. Keep all
        // trigger fields explicit so this helper is independent of the idle
        // policy and can be reused by constrained-random trigger tests.
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_addr.ImmSet(
            config.index);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_matchType.ImmSet(
            config.match_type);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_select.ImmSet(
            config.select);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_action.ImmSet(
            config.action);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_chain.ImmSet(
            config.chain);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_store.ImmSet(
            config.store);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_load.ImmSet(
            config.load);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_bits_tdata_tdata2.ImmSet(
            config.address);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_0.ImmSet(
            (config.enable_mask & 0x1U) != 0);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_1.ImmSet(
            (config.enable_mask & 0x2U) != 0);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_2.ImmSet(
            (config.enable_mask & 0x4U) != 0);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tEnableVec_3.ImmSet(
            (config.enable_mask & 0x8U) != 0);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_triggerCanRaiseBpExp.ImmSet(
            config.trigger_can_raise_breakpoint);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_debugMode.ImmSet(
            config.debug_mode);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid.ImmSet(
            std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid.ImmSet(
            std::uint64_t{0});
        return run_cycles(3) && check_components();
    }
    std::uint64_t cycle() const { return dut_.xclock.clk; }
    std::uint64_t tilelink_requests() const { return memory_agent_.request_count(); }
    std::uint64_t dcache_last_request_address() const
    {
        return memory_agent_.last_request_address();
    }
    std::uint64_t dcache_gets() const { return memory_agent_.get_count(); }
    std::uint64_t dcache_refills() const { return memory_agent_.refill_count(); }
    std::uint64_t dcache_keyword_refills() const
    {
        return memory_agent_.keyword_refill_count();
    }
    std::uint64_t dcache_nonkeyword_refills() const
    {
        return memory_agent_.nonkeyword_refill_count();
    }
    std::uint64_t dcache_acquire_perms() const
    {
        return memory_agent_.acquire_perm_count();
    }
    std::uint64_t tilelink_releases() const { return memory_agent_.release_count(); }
    std::uint64_t tilelink_release_data() const
    {
        return memory_agent_.release_data_count();
    }
    std::uint64_t tilelink_release_data_verified() const
    {
        return memory_agent_.release_data_verified_count();
    }
    std::uint64_t dcache_probes() const
    {
        return memory_agent_.probe_request_count();
    }
    std::uint64_t dcache_probe_responses() const
    {
        return memory_agent_.probe_response_count();
    }
    std::uint64_t dcache_probe_data() const
    {
        return memory_agent_.probe_data_count();
    }
    std::uint64_t dcache_probe_sources() const
    {
        return memory_agent_.probe_source_count();
    }
    std::uint64_t dcache_max_probe_outstanding() const
    {
        return memory_agent_.max_probe_outstanding();
    }
    std::uint64_t dcache_probe_stalls() const
    {
        return memory_agent_.probe_stall_cycles();
    }
    std::uint64_t dcache_grant_acks() const
    {
        return memory_agent_.grant_ack_count();
    }
    std::uint64_t dcache_grant_data_beats() const
    {
        return memory_agent_.grant_data_beat_count();
    }
    std::uint64_t dcache_grant_ack_stalls() const
    {
        return memory_agent_.grant_ack_stall_cycles();
    }
    bool dcache_grants_drained() const
    {
        return memory_agent_.grant_acks_idle();
    }
    std::uint64_t ptw_requests() const { return ptw_agent_.request_count(); }
    std::uint64_t ptw_error_response_requests() const
    {
        return ptw_agent_.error_response_requests();
    }
    std::uint64_t ptw_last_error_response_address() const
    {
        return ptw_agent_.last_error_response_address();
    }
    std::uint8_t ptw_last_error_response_source() const
    {
        return ptw_agent_.last_error_response_source();
    }
    std::uint64_t ptw_requests_covering_since(
        std::uint64_t address, std::uint64_t first_request) const
    {
        return ptw_agent_.request_covering_count_since(address, first_request);
    }
    std::uint64_t pending_ifetch_ptw_requests() const
    {
        return ifetch_ptw_pending_;
    }
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
    std::uint64_t ptw_max_outstanding_requests() const
    {
        return ptw_agent_.max_outstanding_requests();
    }
    const ResponseLatencyStats &ptw_response_latency_stats() const
    {
        return ptw_agent_.response_latency_stats();
    }
    std::uint64_t exception_vaddr()
    {
        return dut_.io_mem_to_ooo_lsqio_vaddr.U();
    }
    void select_store_exception_address(bool store)
    {
        dut_.io_ooo_to_mem_isStoreException.ImmSet(store);
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
    std::uint64_t uncache_max_outstanding_requests() const
    {
        return uncache_agent_.max_outstanding_requests();
    }
    std::uint64_t uncache_outstanding_requests() const
    {
        return uncache_agent_.outstanding_requests();
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
    std::uint64_t lq_enqueued_observed() const
    {
        return lq_enqueued_observed_;
    }
    std::uint64_t lq_dequeued() const { return lq_dequeued_; }
    std::uint64_t lq_canceled() const { return lq_canceled_; }
    std::uint64_t lq_redirect_canceled_observed() const
    {
        return lq_redirect_canceled_observed_;
    }
    std::uint64_t lq_canceled_unobserved() const
    {
        return lq_canceled_unobserved_;
    }
    std::uint64_t sq_allocated() const { return sq_allocated_; }
    std::uint64_t sq_enqueued_observed() const
    {
        return sq_enqueued_observed_;
    }
    std::uint64_t sq_dequeued() const { return sq_dequeued_; }
    std::uint64_t sq_canceled() const { return sq_canceled_; }
    std::uint64_t sq_redirect_canceled_observed() const
    {
        return sq_redirect_canceled_observed_;
    }
    std::uint64_t sq_canceled_unobserved() const
    {
        return sq_canceled_unobserved_;
    }
    std::uint64_t redirect_cancellation_events_observed() const
    {
        return redirect_cancellation_events_observed_;
    }
    const std::array<std::uint64_t, generated::kLsqEnqueueLanes> &
    lsq_enqueue_widths_observed() const
    {
        return lsq_enqueue_widths_observed_;
    }
    const std::array<std::uint64_t, generated::kLsqEnqueueLanes> &
    lsq_enqueue_lanes_observed() const
    {
        return lsq_enqueue_lanes_observed_;
    }
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
    std::uint64_t vector_fof_fix_writebacks() const
    {
        return vector_scoreboard_.fof_fix_observed();
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
    const FrontendBridgeStats &frontend_bridge_stats() const
    {
        return frontend_bridge_stats_;
    }
    const ScalarLoadFeedbackStats &scalar_load_feedback_stats() const
    {
        return scalar_load_feedback_stats_;
    }
    const BusErrorStats &bus_error_stats() const { return bus_error_stats_; }
    const TopDownStats &top_down_stats() const { return top_down_stats_; }
    const IqSlowFeedbackStats &iq_slow_feedback_stats() const
    {
        return iq_slow_feedback_stats_;
    }
    const MemoryViolationStats &memory_violation_stats() const
    {
        return memory_violation_stats_;
    }
    const IfetchPrefetchStats &ifetch_prefetch_stats() const
    {
        return ifetch_prefetch_stats_;
    }
    const HardwarePrefetchStats &hardware_prefetch_stats() const
    {
        return hardware_prefetch_stats_;
    }
    bool sbuffer_empty()
    {
        dut_.RefreshComb();
        return generated::sample_sbuffer_empty(dut_);
    }

    void drive_top_down_misses(bool l2_miss, bool l3_miss)
    {
        dut_.io_topDownInfo_fromL2Top_l2Miss.ImmSet(l2_miss);
        dut_.io_topDownInfo_fromL2Top_l3Miss.ImmSet(l3_miss);
    }

    bool run_until_sbuffer_empty(unsigned timeout = 4096)
    {
        for (unsigned cycle = 0; cycle < timeout && !sbuffer_empty(); ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!sbuffer_empty()) {
            error_ = "timed out waiting for SBuffer to empty";
            return false;
        }
        return check_components();
    }

    bool exercise_frontend_bridges(
        unsigned transaction_count, std::uint64_t seed)
    {
        if (transaction_count < 32) {
            error_ = "frontend bridge coverage requires at least 32 transactions";
            return false;
        }
        frontend_bridge_stats_ = {};

        auto mix64 = [](std::uint64_t value) -> std::uint64_t {
            value += 0x9e3779b97f4a7c15ULL;
            value = (value ^ (value >> 30)) * 0xbf58476d1ce4e5b9ULL;
            value = (value ^ (value >> 27)) * 0x94d049bb133111ebULL;
            return value ^ (value >> 31);
        };
        auto pattern_u64 = [&](std::uint64_t domain,
                               std::uint64_t index) -> std::uint64_t {
            return mix64(seed ^ domain ^
                         (index * 0xd6e8feb86659fd93ULL));
        };
        auto pattern_bytes = [&](std::uint64_t domain, std::uint64_t index,
                                 std::size_t count) {
            std::vector<unsigned char> result(count);
            for (std::size_t byte = 0; byte < count; ++byte) {
                const std::uint64_t word = pattern_u64(
                    domain + byte / sizeof(std::uint64_t), index);
                result[byte] = static_cast<unsigned char>(
                    word >> (8 * (byte % sizeof(std::uint64_t))));
            }
            return result;
        };
        auto fail_value = [&](const char *path, std::uint64_t index,
                              const char *field, std::uint64_t expected,
                              std::uint64_t actual) {
            std::ostringstream message;
            message << "frontend bridge mismatch path=" << path
                    << " index=" << index << " field=" << field
                    << " expected=0x" << std::hex << expected
                    << " actual=0x" << actual;
            error_ = message.str();
            return false;
        };
        auto check_value = [&](const char *path, std::uint64_t index,
                               const char *field, std::uint64_t expected,
                               std::uint64_t actual) {
            if (expected != actual) {
                return fail_value(path, index, field, expected, actual);
            }
            ++frontend_bridge_stats_.field_checks;
            return true;
        };
        auto check_bytes = [&](const char *path, std::uint64_t index,
                               const char *field,
                               const std::vector<unsigned char> &expected,
                               const std::vector<unsigned char> &actual) {
            if (expected != actual) {
                std::ostringstream message;
                message << "frontend bridge mismatch path=" << path
                        << " index=" << index << " field=" << field;
                error_ = message.str();
                return false;
            }
            ++frontend_bridge_stats_.field_checks;
            return true;
        };
        auto check_index = [&](const char *path, std::uint64_t index,
                               std::uint64_t limit) {
            if (index < limit) {
                return true;
            }
            std::ostringstream message;
            message << "unexpected extra frontend bridge transfer path=" << path
                    << " index=" << index << " limit=" << limit;
            error_ = message.str();
            return false;
        };
        auto icache_address = [](std::uint64_t index) -> std::uint64_t {
            return 0x84000000ULL + index * 64;
        };
        auto instr_address = [](std::uint64_t index) -> std::uint64_t {
            return 0x88000000ULL + index * 8;
        };
        auto ctrl_opcode = [](std::uint64_t index) {
            constexpr std::array<std::uint8_t, 3> opcodes{0, 1, 4};
            return opcodes[index % opcodes.size()];
        };
        auto ctrl_size = [](std::uint64_t index) {
            return static_cast<unsigned>(index % 4);
        };
        auto ctrl_address = [&](std::uint64_t index) -> std::uint64_t {
            const unsigned bytes = 1U << ctrl_size(index);
            const unsigned lane = static_cast<unsigned>(
                ((index * 3U) & 7U) / bytes * bytes);
            return 0x100000ULL + index * 16 + lane;
        };
        auto ctrl_mask = [&](std::uint64_t index) {
            const unsigned bytes = 1U << ctrl_size(index);
            const unsigned lane = static_cast<unsigned>(ctrl_address(index) & 7U);
            std::uint8_t mask = static_cast<std::uint8_t>(
                ((1U << bytes) - 1U) << lane);
            if (ctrl_opcode(index) == 1 && bytes > 1) {
                const std::uint8_t partial = static_cast<std::uint8_t>(
                    pattern_u64(0x6374726c6d61736bULL, index));
                mask &= partial;
                if (mask == 0) {
                    mask = static_cast<std::uint8_t>(1U << lane);
                }
            }
            return mask;
        };

        std::uint64_t readiness = seed ^ 0x6a09e667f3bcc909ULL;
        if (readiness == 0) {
            readiness = 1;
        }
        auto next_ready = [&]() {
            readiness ^= readiness << 13;
            readiness ^= readiness >> 7;
            readiness ^= readiness << 17;
            return (readiness & 3U) != 0;
        };

        std::uint64_t icache_input = 0;
        std::uint64_t icache_requests = 0;
        std::uint64_t icache_response_input = 0;
        std::uint64_t icache_responses = 0;
        std::uint64_t instr_input = 0;
        std::uint64_t instr_requests = 0;
        std::uint64_t instr_response_input = 0;
        std::uint64_t instr_responses = 0;
        std::uint64_t ctrl_input = 0;
        std::uint64_t ctrl_requests = 0;
        std::uint64_t ctrl_response_input = 0;
        std::uint64_t ctrl_responses = 0;
        const std::uint64_t icache_response_count =
            std::uint64_t{transaction_count} * 2;
        const std::uint64_t timeout =
            std::uint64_t{transaction_count} * 64 + 4096;

        for (std::uint64_t local_cycle = 0; local_cycle < timeout; ++local_cycle) {
            const bool initial_request_stall = local_cycle < 16;
            const bool icache_a_ready =
                !initial_request_stall && next_ready();
            const bool instr_a_ready =
                !initial_request_stall && next_ready();
            const bool ctrl_a_ready =
                !initial_request_stall && next_ready();
            const bool ctrl_d_ready = local_cycle >= 48 && next_ready();

            const std::uint64_t icache_completed_requests =
                icache_responses / 2;
            frontend_bridge_stats_.source_credit_stalls +=
                (icache_input < transaction_count &&
                 icache_input - icache_completed_requests >= 16) +
                (instr_input < transaction_count &&
                 instr_input != instr_responses) +
                (ctrl_input < transaction_count &&
                 ctrl_input - ctrl_responses >= 32);
            const bool icache_input_valid = icache_input < transaction_count &&
                icache_input - icache_completed_requests < 16;
            dut_.auto_inner_frontendBridge_icache_in_a_valid.ImmSet(
                icache_input_valid);
            if (icache_input_valid) {
                dut_.auto_inner_frontendBridge_icache_in_a_bits_source.ImmSet(
                    icache_input & 0xfU);
                dut_.auto_inner_frontendBridge_icache_in_a_bits_address.ImmSet(
                    icache_address(icache_input));
            }
            dut_.auto_inner_frontendBridge_icache_out_a_ready.ImmSet(
                icache_a_ready);

            const bool instr_input_valid = instr_input < transaction_count &&
                instr_input == instr_responses;
            dut_.auto_inner_frontendBridge_instr_uncache_in_a_valid.ImmSet(
                instr_input_valid);
            if (instr_input_valid) {
                dut_.auto_inner_frontendBridge_instr_uncache_in_a_bits_address.ImmSet(
                    instr_address(instr_input));
            }
            dut_.auto_inner_frontendBridge_instr_uncache_out_a_ready.ImmSet(
                instr_a_ready);

            const bool ctrl_input_valid = ctrl_input < transaction_count &&
                ctrl_input - ctrl_responses < 32;
            dut_.auto_inner_frontendBridge_icachectrl_in_a_valid.ImmSet(
                ctrl_input_valid);
            if (ctrl_input_valid) {
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_opcode.ImmSet(
                    ctrl_opcode(ctrl_input));
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_param.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_size.ImmSet(
                    ctrl_size(ctrl_input));
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_source.ImmSet(
                    ctrl_input & 0x1fU);
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_address.ImmSet(
                    ctrl_address(ctrl_input));
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_mask.ImmSet(
                    ctrl_mask(ctrl_input));
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_data.ImmSet(
                    pattern_u64(0x6374726c64617461ULL, ctrl_input));
                dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_corrupt.ImmSet(
                    std::uint64_t{0});
            }
            dut_.auto_inner_frontendBridge_icachectrl_out_a_ready.ImmSet(
                ctrl_a_ready);
            dut_.auto_inner_frontendBridge_icachectrl_in_d_ready.ImmSet(
                ctrl_d_ready);

            const bool icache_d_valid =
                icache_response_input < icache_requests * 2;
            dut_.auto_inner_frontendBridge_icache_out_d_valid.ImmSet(
                icache_d_valid);
            if (icache_d_valid) {
                const std::uint64_t request = icache_response_input / 2;
                dut_.auto_inner_frontendBridge_icache_out_d_bits_opcode.ImmSet(
                    std::uint64_t{1});
                dut_.auto_inner_frontendBridge_icache_out_d_bits_param.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icache_out_d_bits_size.ImmSet(
                    std::uint64_t{6});
                dut_.auto_inner_frontendBridge_icache_out_d_bits_source.ImmSet(
                    request & 0xfU);
                dut_.auto_inner_frontendBridge_icache_out_d_bits_sink.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icache_out_d_bits_denied.ImmSet(
                    std::uint64_t{0});
                auto data = pattern_bytes(
                    0x6963616368656461ULL, icache_response_input, 32);
                dut_.auto_inner_frontendBridge_icache_out_d_bits_data.ImmSetBytes(
                    data);
                dut_.auto_inner_frontendBridge_icache_out_d_bits_corrupt.ImmSet(
                    (icache_response_input % 11U) == 0);
            }

            const bool instr_d_valid = instr_response_input < instr_requests;
            dut_.auto_inner_frontendBridge_instr_uncache_out_d_valid.ImmSet(
                instr_d_valid);
            if (instr_d_valid) {
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_opcode.ImmSet(
                    std::uint64_t{1});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_param.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_size.ImmSet(
                    std::uint64_t{3});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_source.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_sink.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_denied.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_data.ImmSet(
                    pattern_u64(0x696e737472646174ULL, instr_response_input));
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_bits_corrupt.ImmSet(
                    (instr_response_input % 13U) == 0);
            }

            const bool ctrl_d_valid = ctrl_response_input < ctrl_requests;
            dut_.auto_inner_frontendBridge_icachectrl_out_d_valid.ImmSet(
                ctrl_d_valid);
            if (ctrl_d_valid) {
                dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_opcode.ImmSet(
                    ctrl_opcode(ctrl_response_input) == 4 ? 1 : 0);
                dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_size.ImmSet(
                    ctrl_size(ctrl_response_input));
                dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_source.ImmSet(
                    ctrl_response_input & 0x1fU);
                dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_data.ImmSet(
                    pattern_u64(0x6374726c72657370ULL, ctrl_response_input));
            }

            dut_.RefreshComb();

            if (dut_.auto_inner_frontendBridge_icache_out_a_valid.B()) {
                if (!check_index("icache-a", icache_requests, transaction_count) ||
                    !check_value("icache-a", icache_requests, "opcode", 4,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_opcode.U()) ||
                    !check_value("icache-a", icache_requests, "param", 0,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_param.U()) ||
                    !check_value("icache-a", icache_requests, "size", 6,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_size.U()) ||
                    !check_value("icache-a", icache_requests, "source",
                                 icache_requests & 0xfU,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_source.U()) ||
                    !check_value("icache-a", icache_requests, "address",
                                 icache_address(icache_requests),
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_address.U()) ||
                    !check_value("icache-a", icache_requests, "alias", 0,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_user_alias.U()) ||
                    !check_value("icache-a", icache_requests, "reqSource", 1,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_user_reqSource.U()) ||
                    !check_value("icache-a", icache_requests, "needHint", 0,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_user_needHint.U()) ||
                    !check_value("icache-a", icache_requests, "mask", 0xffffffffU,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_mask.U()) ||
                    !check_bytes("icache-a", icache_requests, "data",
                                 std::vector<unsigned char>(32, 0),
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_data.GetBytes()) ||
                    !check_value("icache-a", icache_requests, "corrupt", 0,
                                 dut_.auto_inner_frontendBridge_icache_out_a_bits_corrupt.U())) {
                    return false;
                }
            }
            if (dut_.auto_inner_frontendBridge_instr_uncache_out_a_valid.B()) {
                if (!check_index("instr-a", instr_requests, transaction_count) ||
                    !check_value("instr-a", instr_requests, "param", 0,
                                 dut_.auto_inner_frontendBridge_instr_uncache_out_a_bits_param.U()) ||
                    !check_value("instr-a", instr_requests, "address",
                                 instr_address(instr_requests),
                                 dut_.auto_inner_frontendBridge_instr_uncache_out_a_bits_address.U()) ||
                    !check_value("instr-a", instr_requests, "corrupt", 0,
                                 dut_.auto_inner_frontendBridge_instr_uncache_out_a_bits_corrupt.U())) {
                    return false;
                }
            }
            if (dut_.auto_inner_frontendBridge_icachectrl_out_a_valid.B()) {
                if (!check_index("ctrl-a", ctrl_requests, transaction_count) ||
                    !check_value("ctrl-a", ctrl_requests, "opcode",
                                 ctrl_opcode(ctrl_requests),
                                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_opcode.U()) ||
                    !check_value("ctrl-a", ctrl_requests, "size",
                                 ctrl_size(ctrl_requests),
                                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_size.U()) ||
                    !check_value("ctrl-a", ctrl_requests, "source",
                                 ctrl_requests & 0x1fU,
                                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_source.U()) ||
                    !check_value("ctrl-a", ctrl_requests, "address",
                                 ctrl_address(ctrl_requests),
                                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_address.U()) ||
                    !check_value("ctrl-a", ctrl_requests, "mask",
                                 ctrl_mask(ctrl_requests),
                                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_mask.U()) ||
                    !check_value("ctrl-a", ctrl_requests, "data",
                                 pattern_u64(0x6374726c64617461ULL, ctrl_requests),
                                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_data.U())) {
                    return false;
                }
            }

            if (dut_.auto_inner_frontendBridge_icache_in_d_valid.B()) {
                if (!check_index(
                        "icache-d", icache_responses, icache_response_count) ||
                    !check_value("icache-d", icache_responses, "opcode", 1,
                                 dut_.auto_inner_frontendBridge_icache_in_d_bits_opcode.U()) ||
                    !check_value("icache-d", icache_responses, "source",
                                 (icache_responses / 2) & 0xfU,
                                 dut_.auto_inner_frontendBridge_icache_in_d_bits_source.U()) ||
                    !check_bytes("icache-d", icache_responses, "data",
                                 pattern_bytes(
                                     0x6963616368656461ULL, icache_responses, 32),
                                 dut_.auto_inner_frontendBridge_icache_in_d_bits_data.GetBytes()) ||
                    !check_value("icache-d", icache_responses, "corrupt",
                                 (icache_responses % 11U) == 0,
                                 dut_.auto_inner_frontendBridge_icache_in_d_bits_corrupt.U())) {
                    return false;
                }
            }
            if (dut_.auto_inner_frontendBridge_instr_uncache_in_d_valid.B()) {
                if (!check_index("instr-d", instr_responses, transaction_count) ||
                    !check_value("instr-d", instr_responses, "source", 0,
                                 dut_.auto_inner_frontendBridge_instr_uncache_in_d_bits_source.U()) ||
                    !check_value("instr-d", instr_responses, "data",
                                 pattern_u64(0x696e737472646174ULL, instr_responses),
                                 dut_.auto_inner_frontendBridge_instr_uncache_in_d_bits_data.U()) ||
                    !check_value("instr-d", instr_responses, "corrupt",
                                 (instr_responses % 13U) == 0,
                                 dut_.auto_inner_frontendBridge_instr_uncache_in_d_bits_corrupt.U())) {
                    return false;
                }
            }
            if (dut_.auto_inner_frontendBridge_icachectrl_in_d_valid.B()) {
                if (!check_index("ctrl-d", ctrl_responses, transaction_count) ||
                    !check_value("ctrl-d", ctrl_responses, "opcode",
                                 ctrl_opcode(ctrl_responses) == 4 ? 1 : 0,
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_opcode.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "param", 0,
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_param.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "size",
                                 ctrl_size(ctrl_responses),
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_size.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "source",
                                 ctrl_responses & 0x1fU,
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_source.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "sink", 0,
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_sink.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "denied", 0,
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_denied.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "data",
                                 pattern_u64(0x6374726c72657370ULL, ctrl_responses),
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_data.U()) ||
                    !check_value("ctrl-d", ctrl_responses, "corrupt", 0,
                                 dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_corrupt.U())) {
                    return false;
                }
            }

            const bool icache_input_fire = icache_input_valid &&
                dut_.auto_inner_frontendBridge_icache_in_a_ready.B();
            const bool instr_input_fire = instr_input_valid &&
                dut_.auto_inner_frontendBridge_instr_uncache_in_a_ready.B();
            const bool ctrl_input_fire = ctrl_input_valid &&
                dut_.auto_inner_frontendBridge_icachectrl_in_a_ready.B();
            const bool icache_request_fire =
                dut_.auto_inner_frontendBridge_icache_out_a_valid.B() &&
                icache_a_ready;
            const bool instr_request_fire =
                dut_.auto_inner_frontendBridge_instr_uncache_out_a_valid.B() &&
                instr_a_ready;
            const bool ctrl_request_fire =
                dut_.auto_inner_frontendBridge_icachectrl_out_a_valid.B() &&
                ctrl_a_ready;
            const bool icache_response_input_fire = icache_d_valid &&
                dut_.auto_inner_frontendBridge_icache_out_d_ready.B();
            const bool instr_response_input_fire = instr_d_valid &&
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_ready.B();
            const bool ctrl_response_input_fire = ctrl_d_valid &&
                dut_.auto_inner_frontendBridge_icachectrl_out_d_ready.B();
            const bool icache_response_fire =
                dut_.auto_inner_frontendBridge_icache_in_d_valid.B();
            const bool instr_response_fire =
                dut_.auto_inner_frontendBridge_instr_uncache_in_d_valid.B();
            const bool ctrl_response_fire =
                dut_.auto_inner_frontendBridge_icachectrl_in_d_valid.B() &&
                ctrl_d_ready;

            frontend_bridge_stats_.request_stalls +=
                (icache_input_valid &&
                 !dut_.auto_inner_frontendBridge_icache_in_a_ready.B()) +
                (instr_input_valid &&
                 !dut_.auto_inner_frontendBridge_instr_uncache_in_a_ready.B()) +
                (ctrl_input_valid &&
                 !dut_.auto_inner_frontendBridge_icachectrl_in_a_ready.B()) +
                (dut_.auto_inner_frontendBridge_icache_out_a_valid.B() &&
                 !icache_a_ready) +
                (dut_.auto_inner_frontendBridge_instr_uncache_out_a_valid.B() &&
                 !instr_a_ready) +
                (dut_.auto_inner_frontendBridge_icachectrl_out_a_valid.B() &&
                 !ctrl_a_ready);
            frontend_bridge_stats_.response_stalls +=
                (icache_d_valid &&
                 !dut_.auto_inner_frontendBridge_icache_out_d_ready.B()) +
                (instr_d_valid &&
                 !dut_.auto_inner_frontendBridge_instr_uncache_out_d_ready.B()) +
                (ctrl_d_valid &&
                 !dut_.auto_inner_frontendBridge_icachectrl_out_d_ready.B()) +
                (dut_.auto_inner_frontendBridge_icachectrl_in_d_valid.B() &&
                 !ctrl_d_ready);

            tick(false);
            icache_input += icache_input_fire;
            instr_input += instr_input_fire;
            ctrl_input += ctrl_input_fire;
            icache_requests += icache_request_fire;
            instr_requests += instr_request_fire;
            ctrl_requests += ctrl_request_fire;
            icache_response_input += icache_response_input_fire;
            instr_response_input += instr_response_input_fire;
            ctrl_response_input += ctrl_response_input_fire;
            icache_responses += icache_response_fire;
            instr_responses += instr_response_fire;
            ctrl_responses += ctrl_response_fire;

            if (icache_input == transaction_count &&
                icache_requests == transaction_count &&
                icache_response_input == icache_response_count &&
                icache_responses == icache_response_count &&
                instr_input == transaction_count &&
                instr_requests == transaction_count &&
                instr_response_input == transaction_count &&
                instr_responses == transaction_count &&
                ctrl_input == transaction_count &&
                ctrl_requests == transaction_count &&
                ctrl_response_input == transaction_count &&
                ctrl_responses == transaction_count) {
                frontend_bridge_stats_.requests =
                    icache_requests + instr_requests + ctrl_requests;
                frontend_bridge_stats_.responses =
                    icache_responses + instr_responses + ctrl_responses;
                if (frontend_bridge_stats_.request_stalls == 0 ||
                    frontend_bridge_stats_.response_stalls == 0 ||
                    frontend_bridge_stats_.source_credit_stalls == 0) {
                    error_ = "frontend bridge run missed a required stall class";
                    return false;
                }
                dut_.auto_inner_frontendBridge_icache_in_a_valid.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_instr_uncache_in_a_valid.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icachectrl_in_a_valid.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icache_out_d_valid.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_instr_uncache_out_d_valid.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icachectrl_out_d_valid.ImmSet(
                    std::uint64_t{0});
                dut_.auto_inner_frontendBridge_icache_out_a_ready.ImmSet(
                    std::uint64_t{1});
                dut_.auto_inner_frontendBridge_instr_uncache_out_a_ready.ImmSet(
                    std::uint64_t{1});
                dut_.auto_inner_frontendBridge_icachectrl_out_a_ready.ImmSet(
                    std::uint64_t{1});
                dut_.auto_inner_frontendBridge_icachectrl_in_d_ready.ImmSet(
                    std::uint64_t{1});
                return check_idle(8);
            }
        }

        std::ostringstream message;
        message << "timed out draining frontend bridges"
                << " icache=" << icache_input << '/' << icache_requests
                << '/' << icache_response_input << '/' << icache_responses
                << " instr=" << instr_input << '/' << instr_requests
                << '/' << instr_response_input << '/' << instr_responses
                << " ctrl=" << ctrl_input << '/' << ctrl_requests
                << '/' << ctrl_response_input << '/' << ctrl_responses;
        error_ = message.str();
        return false;
    }

    bool check_frontend_bridge_reset_recovery()
    {
        constexpr std::uint64_t old_icache_address = 0x84001000ULL;
        constexpr std::uint64_t old_instr_address = 0x88001000ULL;
        constexpr std::uint64_t old_ctrl_address = 0x00123464ULL;
        constexpr std::uint64_t old_ctrl_data = 0x1122334455667788ULL;
        constexpr std::uint64_t new_icache_address = 0x84002000ULL;
        constexpr std::uint64_t new_instr_address = 0x88002000ULL;
        constexpr std::uint64_t new_ctrl_address = 0x00124560ULL;
        constexpr std::uint64_t new_ctrl_data = 0x8877665544332211ULL;
        constexpr unsigned stalled_cycles = 4;

        if (!reset()) {
            return false;
        }
        frontend_reset_canceled_requests_ = 0;
        frontend_reset_canceled_responses_ = 0;
        frontend_reset_survivor_requests_ = 0;
        frontend_reset_stall_checks_ = 0;

        const auto drive_request_valid = [&](const std::array<bool, 3> &valid) {
            dut_.auto_inner_frontendBridge_icache_in_a_valid.ImmSet(valid[0]);
            dut_.auto_inner_frontendBridge_instr_uncache_in_a_valid.ImmSet(
                valid[1]);
            dut_.auto_inner_frontendBridge_icachectrl_in_a_valid.ImmSet(
                valid[2]);
        };
        const auto drive_requests = [&](std::uint64_t icache_address,
                                        std::uint64_t instr_address,
                                        std::uint64_t ctrl_address,
                                        std::uint64_t ctrl_data) {
            dut_.auto_inner_frontendBridge_icache_in_a_bits_source.ImmSet(
                std::uint64_t{3});
            dut_.auto_inner_frontendBridge_icache_in_a_bits_address.ImmSet(
                icache_address);
            dut_.auto_inner_frontendBridge_instr_uncache_in_a_bits_address.ImmSet(
                instr_address);
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_opcode.ImmSet(
                std::uint64_t{1});
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_param.ImmSet(
                std::uint64_t{0});
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_size.ImmSet(
                std::uint64_t{2});
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_source.ImmSet(
                std::uint64_t{27});
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_address.ImmSet(
                ctrl_address);
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_mask.ImmSet(
                std::uint64_t{0xf0});
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_data.ImmSet(
                ctrl_data);
            dut_.auto_inner_frontendBridge_icachectrl_in_a_bits_corrupt.ImmSet(
                std::uint64_t{0});
        };
        const auto request_input_fires = [&]() {
            return std::array<bool, 3>{
                dut_.auto_inner_frontendBridge_icache_in_a_ready.B(),
                dut_.auto_inner_frontendBridge_instr_uncache_in_a_ready.B(),
                dut_.auto_inner_frontendBridge_icachectrl_in_a_ready.B(),
            };
        };
        const auto request_output_valids = [&]() {
            return std::array<bool, 3>{
                dut_.auto_inner_frontendBridge_icache_out_a_valid.B(),
                dut_.auto_inner_frontendBridge_instr_uncache_out_a_valid.B(),
                dut_.auto_inner_frontendBridge_icachectrl_out_a_valid.B(),
            };
        };
        const auto check_request_payload = [&](std::uint64_t icache_address,
                                               std::uint64_t instr_address,
                                               std::uint64_t ctrl_address,
                                               std::uint64_t ctrl_data,
                                               const char *phase,
                                               bool require_all_valid) {
            const auto valid = request_output_valids();
            const bool icache_mismatch = valid[0] &&
                (dut_.auto_inner_frontendBridge_icache_out_a_bits_opcode.U() != 4 ||
                 dut_.auto_inner_frontendBridge_icache_out_a_bits_size.U() != 6 ||
                 dut_.auto_inner_frontendBridge_icache_out_a_bits_source.U() != 3 ||
                 dut_.auto_inner_frontendBridge_icache_out_a_bits_address.U() !=
                     icache_address);
            const bool instr_mismatch = valid[1] &&
                dut_.auto_inner_frontendBridge_instr_uncache_out_a_bits_address.U() !=
                    instr_address;
            const bool ctrl_mismatch = valid[2] &&
                (dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_opcode.U() != 1 ||
                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_size.U() != 2 ||
                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_source.U() != 27 ||
                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_address.U() !=
                     ctrl_address ||
                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_mask.U() !=
                     0xf0 ||
                 dut_.auto_inner_frontendBridge_icachectrl_out_a_bits_data.U() !=
                     ctrl_data);
            if ((require_all_valid &&
                 (!valid[0] || !valid[1] || !valid[2])) ||
                icache_mismatch || instr_mismatch || ctrl_mismatch) {
                std::ostringstream message;
                message << "frontend request buffer mismatch phase=" << phase
                        << " valid=" << valid[0] << valid[1] << valid[2];
                error_ = message.str();
                return false;
            }
            return true;
        };
        const auto accept_all_requests = [&](std::array<bool, 3> &accepted,
                                             const char *phase) {
            for (unsigned elapsed = 0; elapsed < 16; ++elapsed) {
                const std::array<bool, 3> valid{
                    !accepted[0], !accepted[1], !accepted[2]};
                drive_request_valid(valid);
                dut_.RefreshComb();
                const auto ready = request_input_fires();
                const std::array<bool, 3> fire{
                    valid[0] && ready[0], valid[1] && ready[1],
                    valid[2] && ready[2]};
                tick(false);
                for (unsigned path = 0; path < accepted.size(); ++path) {
                    accepted[path] = accepted[path] || fire[path];
                }
                if (accepted[0] && accepted[1] && accepted[2]) {
                    drive_request_valid({false, false, false});
                    return true;
                }
            }
            std::ostringstream message;
            message << "timed out accepting frontend requests phase=" << phase
                    << " accepted=" << accepted[0] << accepted[1]
                    << accepted[2];
            error_ = message.str();
            return false;
        };
        const auto require_no_buffered_traffic = [&](const char *phase,
                                                     unsigned cycles) {
            for (unsigned elapsed = 0; elapsed < cycles; ++elapsed) {
                dut_.RefreshComb();
                const auto request_valid = request_output_valids();
                if (request_valid[0] || request_valid[1] || request_valid[2] ||
                    dut_.auto_inner_frontendBridge_icache_in_d_valid.B() ||
                    dut_.auto_inner_frontendBridge_instr_uncache_in_d_valid.B() ||
                    dut_.auto_inner_frontendBridge_icachectrl_in_d_valid.B()) {
                    std::ostringstream message;
                    message << "stale frontend bridge traffic phase=" << phase
                            << " elapsed=" << elapsed;
                    error_ = message.str();
                    return false;
                }
                tick(false);
            }
            return true;
        };

        dut_.auto_inner_frontendBridge_icache_out_a_ready.ImmSet(
            std::uint64_t{0});
        dut_.auto_inner_frontendBridge_instr_uncache_out_a_ready.ImmSet(
            std::uint64_t{0});
        dut_.auto_inner_frontendBridge_icachectrl_out_a_ready.ImmSet(
            std::uint64_t{0});
        drive_requests(old_icache_address, old_instr_address,
                       old_ctrl_address, old_ctrl_data);
        std::array<bool, 3> accepted{};
        if (!accept_all_requests(accepted, "pre-reset-request")) {
            return false;
        }
        bool all_requests_buffered = false;
        for (unsigned elapsed = 0; elapsed < 8; ++elapsed) {
            dut_.RefreshComb();
            const auto valid = request_output_valids();
            if (valid[0] && valid[1] && valid[2]) {
                all_requests_buffered = true;
                break;
            }
            tick(false);
        }
        if (!all_requests_buffered) {
            error_ = "timed out advancing accepted frontend requests to stalled outputs";
            return false;
        }
        for (unsigned elapsed = 0; elapsed < stalled_cycles; ++elapsed) {
            dut_.RefreshComb();
            if (!check_request_payload(
                    old_icache_address, old_instr_address, old_ctrl_address,
                    old_ctrl_data, "pre-reset-stall", true)) {
                return false;
            }
            ++frontend_reset_stall_checks_;
            tick(false);
        }
        if (!reset() ||
            !require_no_buffered_traffic("request-reset", stalled_cycles)) {
            return false;
        }
        frontend_reset_canceled_requests_ = 3;

        dut_.auto_inner_frontendBridge_icachectrl_in_d_ready.ImmSet(
            std::uint64_t{0});
        dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_opcode.ImmSet(
            std::uint64_t{1});
        dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_size.ImmSet(
            std::uint64_t{2});
        dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_source.ImmSet(
            std::uint64_t{27});
        dut_.auto_inner_frontendBridge_icachectrl_out_d_bits_data.ImmSet(
            std::uint64_t{0xa5a55a5adeadbeefULL});
        dut_.auto_inner_frontendBridge_icachectrl_out_d_valid.ImmSet(
            std::uint64_t{1});
        bool response_accepted = false;
        for (unsigned elapsed = 0; elapsed < 16 && !response_accepted;
             ++elapsed) {
            dut_.RefreshComb();
            response_accepted =
                dut_.auto_inner_frontendBridge_icachectrl_out_d_ready.B();
            tick(false);
        }
        dut_.auto_inner_frontendBridge_icachectrl_out_d_valid.ImmSet(
            std::uint64_t{0});
        if (!response_accepted) {
            error_ = "timed out buffering ICache-control response";
            return false;
        }
        bool response_buffered = false;
        for (unsigned elapsed = 0; elapsed < 8; ++elapsed) {
            dut_.RefreshComb();
            if (dut_.auto_inner_frontendBridge_icachectrl_in_d_valid.B()) {
                response_buffered = true;
                break;
            }
            tick(false);
        }
        if (!response_buffered) {
            error_ = "timed out advancing ICache-control response to stalled output";
            return false;
        }
        for (unsigned elapsed = 0; elapsed < stalled_cycles; ++elapsed) {
            dut_.RefreshComb();
            if (!dut_.auto_inner_frontendBridge_icachectrl_in_d_valid.B() ||
                dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_opcode.U() != 1 ||
                dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_size.U() != 2 ||
                dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_source.U() != 27 ||
                dut_.auto_inner_frontendBridge_icachectrl_in_d_bits_data.U() !=
                    0xa5a55a5adeadbeefULL) {
                error_ = "ICache-control response changed while stalled before reset";
                return false;
            }
            ++frontend_reset_stall_checks_;
            tick(false);
        }
        if (!reset() ||
            !require_no_buffered_traffic("response-reset", stalled_cycles)) {
            return false;
        }
        frontend_reset_canceled_responses_ = 1;

        dut_.auto_inner_frontendBridge_icache_out_a_ready.ImmSet(
            std::uint64_t{1});
        dut_.auto_inner_frontendBridge_instr_uncache_out_a_ready.ImmSet(
            std::uint64_t{1});
        dut_.auto_inner_frontendBridge_icachectrl_out_a_ready.ImmSet(
            std::uint64_t{1});
        dut_.auto_inner_frontendBridge_icachectrl_in_d_ready.ImmSet(
            std::uint64_t{1});
        drive_requests(new_icache_address, new_instr_address,
                       new_ctrl_address, new_ctrl_data);
        std::array<bool, 3> survivor_accepted{};
        std::array<bool, 3> survivor_emerged{};
        for (unsigned elapsed = 0; elapsed < 32; ++elapsed) {
            const std::array<bool, 3> input_valid{
                !survivor_accepted[0], !survivor_accepted[1],
                !survivor_accepted[2]};
            drive_request_valid(input_valid);
            dut_.RefreshComb();
            const auto input_ready = request_input_fires();
            const auto output_valid = request_output_valids();
            if ((output_valid[0] || output_valid[1] || output_valid[2]) &&
                !check_request_payload(
                    new_icache_address, new_instr_address, new_ctrl_address,
                    new_ctrl_data, "post-reset-survivor", false)) {
                return false;
            }
            const std::array<bool, 3> input_fire{
                input_valid[0] && input_ready[0],
                input_valid[1] && input_ready[1],
                input_valid[2] && input_ready[2]};
            tick(false);
            for (unsigned path = 0; path < survivor_accepted.size(); ++path) {
                survivor_accepted[path] =
                    survivor_accepted[path] || input_fire[path];
                if (output_valid[path]) {
                    if (survivor_emerged[path]) {
                        error_ = "duplicate post-reset frontend request";
                        return false;
                    }
                    survivor_emerged[path] = true;
                    ++frontend_reset_survivor_requests_;
                }
            }
            if (survivor_emerged[0] && survivor_emerged[1] &&
                survivor_emerged[2]) {
                drive_request_valid({false, false, false});
                break;
            }
        }
        if (frontend_reset_survivor_requests_ != 3) {
            error_ = "timed out draining post-reset frontend requests";
            return false;
        }
        return reset() && check_idle(2);
    }

    std::uint64_t frontend_reset_canceled_requests() const
    {
        return frontend_reset_canceled_requests_;
    }

    std::uint64_t frontend_reset_canceled_responses() const
    {
        return frontend_reset_canceled_responses_;
    }

    std::uint64_t frontend_reset_survivor_requests() const
    {
        return frontend_reset_survivor_requests_;
    }

    std::uint64_t frontend_reset_stall_checks() const
    {
        return frontend_reset_stall_checks_;
    }

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
        // XSTileWrap applies the same child reset to the core and tile-local
        // managers. Model that reset domain by discarding every in-flight
        // protocol transaction before the DUT can reuse source identifiers.
        memory_agent_.reset_link_state();
        ptw_agent_.reset_link_state();
        uncache_agent_.reset_link_state();
        scalar_load_feedback_stats_ = {};
        iq_slow_feedback_stats_ = {};
        memory_violation_stats_ = {};
        ifetch_prefetch_stats_ = {};
        hardware_prefetch_stats_ = {};
        bus_error_stats_ = {};
        top_down_stats_ = {};
        expected_top_down_l2_miss_ = false;
        expected_top_down_l3_miss_ = false;
        ifetch_ptw_pending_ = 0;
        dut_.io_fetch_to_mem_itlb_req_0_valid.ImmSet(std::uint64_t{0});
        dut_.io_fetch_to_mem_itlb_resp_ready.ImmSet(std::uint64_t{1});
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

    bool check_reset_backend_contract()
    {
        constexpr unsigned sync_stages = 3;
        constexpr unsigned functional_release_cycles = 2 * sync_stages;
        constexpr std::array<unsigned, 3> functional_pulse_widths{1, 2, 5};

        if (!reset()) {
            return false;
        }
        reset_functional_pulses_ = 0;
        reset_dft_pulses_ = 0;
        reset_scan_transitions_ = 0;
        reset_async_assertions_ = 0;

        const auto require_reset = [&](bool expected, const char *phase,
                                       unsigned elapsed) {
            dut_.RefreshComb();
            const bool observed = dut_.io_reset_backend.B();
            if (observed == expected) {
                return true;
            }
            std::ostringstream message;
            message << "backend reset mismatch phase=" << phase
                    << " elapsed=" << elapsed
                    << " expected=" << expected
                    << " observed=" << observed;
            error_ = message.str();
            return false;
        };
        const auto require_synchronous_release = [&](unsigned latency,
                                                     const char *phase) {
            if (!require_reset(true, phase, 0)) {
                return false;
            }
            for (unsigned elapsed = 1; elapsed <= latency; ++elapsed) {
                tick(false);
                if (!require_reset(elapsed < latency, phase, elapsed)) {
                    return false;
                }
            }
            return true;
        };

        // Functional reset crosses the top-level and right MemBlock reset
        // generators. Both asynchronously assert and synchronously release.
        for (const unsigned pulse_width : functional_pulse_widths) {
            dut_.reset.ImmSet(std::uint64_t{1});
            if (!require_reset(true, "functional-assert", 0)) {
                return false;
            }
            ++reset_async_assertions_;
            for (unsigned elapsed = 0; elapsed < pulse_width; ++elapsed) {
                tick(false);
                if (!require_reset(true, "functional-hold", elapsed + 1)) {
                    return false;
                }
            }
            dut_.reset.ImmSet(std::uint64_t{0});
            if (!require_synchronous_release(
                    functional_release_cycles, "functional-release")) {
                return false;
            }
            ++reset_functional_pulses_;
        }

        // DFT functional mode selects lgc_rst_n independently in each
        // ResetGen. External functional reset must therefore be isolated, and
        // release reaches this output after one three-stage synchronizer.
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{1});
        dut_.io_dft_reset_mode.ImmSet(std::uint64_t{1});
        dut_.io_dft_reset_scan_mode.ImmSet(std::uint64_t{0});
        dut_.reset.ImmSet(std::uint64_t{1});
        if (!require_reset(false, "dft-functional-isolation", 0)) {
            return false;
        }
        tick(false);
        if (!require_reset(false, "dft-functional-isolation", 1)) {
            return false;
        }
        dut_.reset.ImmSet(std::uint64_t{0});
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{0});
        if (!require_reset(true, "dft-functional-assert", 0)) {
            return false;
        }
        ++reset_async_assertions_;
        for (unsigned elapsed = 0; elapsed < 2; ++elapsed) {
            tick(false);
            if (!require_reset(true, "dft-functional-hold", elapsed + 1)) {
                return false;
            }
        }
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{1});
        if (!require_synchronous_release(
                sync_stages, "dft-functional-release")) {
            return false;
        }
        ++reset_dft_pulses_;

        // Scan mode bypasses the synchronizer output and directly projects
        // the active-low logic reset pin.
        dut_.io_dft_reset_mode.ImmSet(std::uint64_t{0});
        dut_.io_dft_reset_scan_mode.ImmSet(std::uint64_t{1});
        if (!require_reset(false, "scan-idle", 0)) {
            return false;
        }
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{0});
        if (!require_reset(true, "scan-assert", 0)) {
            return false;
        }
        ++reset_async_assertions_;
        ++reset_scan_transitions_;
        tick(false);
        if (!require_reset(true, "scan-hold", 1)) {
            return false;
        }
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{1});
        if (!require_reset(false, "scan-release", 0)) {
            return false;
        }
        ++reset_scan_transitions_;

        dut_.io_dft_reset_scan_mode.ImmSet(std::uint64_t{0});
        dut_.io_dft_reset_mode.ImmSet(std::uint64_t{0});
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{1});
        return reset() && check_idle(2);
    }

    std::uint64_t reset_functional_pulses() const
    {
        return reset_functional_pulses_;
    }

    std::uint64_t reset_dft_pulses() const { return reset_dft_pulses_; }

    std::uint64_t reset_scan_transitions() const
    {
        return reset_scan_transitions_;
    }

    std::uint64_t reset_async_assertions() const
    {
        return reset_async_assertions_;
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
        bool io = false,
        bool global = false)
    {
        constexpr std::uint64_t page_mask = 0xfff;
        constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
        constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
        constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
        constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
        constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
        constexpr std::uint64_t pte_global = std::uint64_t{1} << 5;
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
            (user ? pte_user : 0) | (global ? pte_global : 0) |
            pte_accessed |
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
        bool io = false,
        bool global = false)
    {
        constexpr std::uint64_t page_mask = 0xfff;
        constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
        constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
        constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
        constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
        constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
        constexpr std::uint64_t pte_global = std::uint64_t{1} << 5;
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
            (user ? pte_user : 0) | (global ? pte_global : 0) |
            pte_accessed |
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
        bool noncacheable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool io = false,
        bool global = false)
    {
        return map_reference_leaf(
            virtual_address, physical_address, root_page_table,
            ReferencePageMode::sv39, false, leaf_level, readable, writable,
            executable, user, noncacheable, accessed, dirty, io, global);
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
        bool noncacheable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool io = false,
        bool global = false)
    {
        return map_reference_leaf(
            virtual_address, physical_address, root_page_table,
            ReferencePageMode::sv48, false, leaf_level, readable, writable,
            executable, user, noncacheable, accessed, dirty, io, global);
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
        bool executable = false,
        bool global = false)
    {
        constexpr std::uint64_t page_mask = 0xfff;
        constexpr std::uint64_t root_mask = 0x3fff;
        constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
        constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
        constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
        constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
        constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
        constexpr std::uint64_t pte_global = std::uint64_t{1} << 5;
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
            (global ? pte_global : 0) |
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
        bool executable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool user = true,
        bool noncacheable = false,
        bool io = false,
        bool global = false)
    {
        return map_reference_leaf(
            guest_physical_address, host_physical_address, root_page_table,
            ReferencePageMode::sv48, true, leaf_level, readable, writable,
            executable, user, noncacheable, accessed, dirty, io, global);
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

    bool map_sv39_napot64k(
        std::uint64_t virtual_base,
        std::uint64_t physical_base,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool io = false)
    {
        return map_reference_napot64k(
            virtual_base, physical_base, root_page_table,
            ReferencePageMode::sv39, false, readable, writable, executable,
            user, noncacheable, accessed, dirty, io);
    }

    bool map_sv48_napot64k(
        std::uint64_t virtual_base,
        std::uint64_t physical_base,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool io = false)
    {
        return map_reference_napot64k(
            virtual_base, physical_base, root_page_table,
            ReferencePageMode::sv48, false, readable, writable, executable,
            user, noncacheable, accessed, dirty, io);
    }

    bool map_sv39x4_napot64k(
        std::uint64_t guest_physical_base,
        std::uint64_t host_physical_base,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool user = true,
        bool noncacheable = false,
        bool io = false)
    {
        return map_reference_napot64k(
            guest_physical_base, host_physical_base, root_page_table,
            ReferencePageMode::sv39, true, readable, writable, executable,
            user, noncacheable, accessed, dirty, io);
    }

    bool map_sv48x4_napot64k(
        std::uint64_t guest_physical_base,
        std::uint64_t host_physical_base,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool user = true,
        bool noncacheable = false,
        bool io = false)
    {
        return map_reference_napot64k(
            guest_physical_base, host_physical_base, root_page_table,
            ReferencePageMode::sv48, true, readable, writable, executable,
            user, noncacheable, accessed, dirty, io);
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
        bool noncacheable,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool io = false,
        bool global = false)
    {
        constexpr std::uint64_t page_mask = 0xfff;
        constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
        constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
        constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
        constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
        constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
        constexpr std::uint64_t pte_global = std::uint64_t{1} << 5;
        constexpr std::uint64_t pte_accessed = std::uint64_t{1} << 6;
        constexpr std::uint64_t pte_dirty = std::uint64_t{1} << 7;
        constexpr std::uint64_t pte_pbmt_nc = std::uint64_t{1} << 61;
        constexpr std::uint64_t pte_pbmt_io = std::uint64_t{1} << 62;
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
        if (noncacheable && io) {
            error_ = "page mapping cannot select NC and IO simultaneously";
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
            (user ? pte_user : 0) |
            (global ? pte_global : 0) |
            (accessed ? pte_accessed : 0) |
            (dirty.value_or(writable) ? pte_dirty : 0) |
            (noncacheable ? pte_pbmt_nc : 0) |
            (io ? pte_pbmt_io : 0);
        memory_.write_u64(
            table + leaf_index * 8,
            (((physical_address & ~page_mask) >> 12) << 10) | flags);
        return true;
    }

    bool map_reference_napot64k(
        std::uint64_t input_base,
        std::uint64_t physical_base,
        std::uint64_t root_page_table,
        ReferencePageMode mode,
        bool x4,
        bool readable,
        bool writable,
        bool executable,
        bool user,
        bool noncacheable,
        bool accessed,
        std::optional<bool> dirty,
        bool io)
    {
        constexpr std::uint64_t napot_size = 0x10000;
        constexpr std::uint64_t napot_mask = napot_size - 1;
        constexpr std::uint64_t page_size = 0x1000;
        constexpr std::uint64_t pte_ppn_mask =
            ((std::uint64_t{1} << 44) - 1) << 10;
        constexpr std::uint64_t pte_napot = std::uint64_t{1} << 63;
        if ((input_base & napot_mask) != 0 ||
            (physical_base & napot_mask) != 0) {
            error_ = "Svnapot 64-KiB mapping requires aligned virtual and physical bases";
            return false;
        }

        for (unsigned page = 0; page < 16; ++page) {
            const std::uint64_t offset = page * page_size;
            if (!map_reference_leaf(
                    input_base + offset, physical_base + offset,
                    root_page_table, mode, x4, 0, readable, writable,
                    executable, user, noncacheable, accessed, dirty, io)) {
                return false;
            }
        }

        const std::uint64_t napot_ppn = (physical_base >> 12) | 8U;
        for (unsigned page = 0; page < 16; ++page) {
            const std::uint64_t address = input_base + page * page_size;
            const auto pte_address = reference_pte_address_at_level(
                memory_, root_page_table, address, mode, 0, x4);
            if (!pte_address) {
                error_ = "failed to locate Svnapot leaf PTE";
                return false;
            }
            const std::uint64_t ordinary_pte = memory_.read_u64(*pte_address);
            memory_.write_u64(
                *pte_address,
                (ordinary_pte & ~pte_ppn_mask) | (napot_ppn << 10) |
                    pte_napot);
        }
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
        bool executable = false,
        bool global = false)
    {
        constexpr std::uint64_t page_mask = 0xfff;
        constexpr std::uint64_t root_mask = 0x3fff;
        constexpr std::uint64_t pte_valid = std::uint64_t{1} << 0;
        constexpr std::uint64_t pte_read = std::uint64_t{1} << 1;
        constexpr std::uint64_t pte_write = std::uint64_t{1} << 2;
        constexpr std::uint64_t pte_execute = std::uint64_t{1} << 3;
        constexpr std::uint64_t pte_user = std::uint64_t{1} << 4;
        constexpr std::uint64_t pte_global = std::uint64_t{1} << 5;
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
            (executable ? pte_execute : 0) | pte_user |
            (global ? pte_global : 0) | pte_accessed |
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
        bool executable = false,
        bool accessed = true,
        std::optional<bool> dirty = std::nullopt,
        bool user = true,
        bool noncacheable = false,
        bool io = false,
        bool global = false)
    {
        return map_reference_leaf(
            guest_physical_address, host_physical_address, root_page_table,
            ReferencePageMode::sv39, true, leaf_level, readable, writable,
            executable, user, noncacheable, accessed, dirty, io, global);
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

    bool update_stage_one_context(
        ReferencePageMode mode,
        std::uint64_t root_page_table,
        std::uint16_t asid)
    {
        dut_.io_ooo_to_mem_tlbCsr_satp_mode.ImmSet(
            static_cast<std::uint64_t>(mode));
        dut_.io_ooo_to_mem_tlbCsr_satp_asid.ImmSet(asid);
        dut_.io_ooo_to_mem_tlbCsr_satp_ppn.ImmSet(root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_satp_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_satp_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool update_vs_context(
        ReferencePageMode mode,
        std::uint64_t root_page_table,
        std::uint16_t asid)
    {
        dut_.io_ooo_to_mem_tlbCsr_vsatp_mode.ImmSet(
            static_cast<std::uint64_t>(mode));
        dut_.io_ooo_to_mem_tlbCsr_vsatp_asid.ImmSet(asid);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_ppn.ImmSet(root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool update_two_stage_context(
        ReferencePageMode vs_mode,
        ReferencePageMode g_mode,
        std::uint64_t vs_root_page_table,
        std::uint64_t g_root_page_table,
        std::uint16_t asid,
        std::uint16_t vmid)
    {
        dut_.io_ooo_to_mem_tlbCsr_vsatp_mode.ImmSet(
            static_cast<std::uint64_t>(vs_mode));
        dut_.io_ooo_to_mem_tlbCsr_vsatp_asid.ImmSet(asid);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_ppn.ImmSet(vs_root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_mode.ImmSet(
            static_cast<std::uint64_t>(g_mode));
        dut_.io_ooo_to_mem_tlbCsr_hgatp_vmid.ImmSet(vmid);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_ppn.ImmSet(g_root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_changed.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_hgatp_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_changed.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_hgatp_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool update_g_context(
        ReferencePageMode mode,
        std::uint64_t root_page_table,
        std::uint16_t vmid)
    {
        dut_.io_ooo_to_mem_tlbCsr_hgatp_mode.ImmSet(
            static_cast<std::uint64_t>(mode));
        dut_.io_ooo_to_mem_tlbCsr_hgatp_vmid.ImmSet(vmid);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_ppn.ImmSet(root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool set_translation_permissions(
        ReferencePrivilegeMode data_privilege,
        bool mxr = false,
        bool sum = false,
        bool vmxr = false,
        bool vsum = false)
    {
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(
            static_cast<std::uint64_t>(data_privilege));
        dut_.io_ooo_to_mem_tlbCsr_priv_mxr.ImmSet(mxr);
        dut_.io_ooo_to_mem_tlbCsr_priv_sum.ImmSet(sum);
        dut_.io_ooo_to_mem_tlbCsr_priv_vmxr.ImmSet(vmxr);
        dut_.io_ooo_to_mem_tlbCsr_priv_vsum.ImmSet(vsum);
        // TlbCsrBundle is registered and duplicated before it reaches every
        // DTLB port. Let all copies settle before issuing the first request.
        return run_cycles(16) && check_components();
    }

    bool set_instruction_privilege(ReferencePrivilegeMode privilege)
    {
        dut_.io_ooo_to_mem_tlbCsr_priv_imode.ImmSet(
            static_cast<std::uint64_t>(privilege));
        return run_cycles(16) && check_components();
    }

    bool set_debug_mode(bool enabled)
    {
        dut_.io_ooo_to_mem_tlbCsr_priv_debug.ImmSet(enabled);
        // TlbCsrBundle is registered and duplicated before its PMA/PMP use.
        return run_cycles(16) && check_components();
    }

    bool set_hypervisor_access_permissions(
        ReferencePrivilegeMode spvp,
        bool mxr = false,
        bool vmxr = false,
        bool vsum = false,
        ReferencePrivilegeMode current_privilege =
            ReferencePrivilegeMode::supervisor)
    {
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(
            static_cast<std::uint64_t>(current_privilege));
        dut_.io_ooo_to_mem_tlbCsr_priv_virt.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_priv_spvp.ImmSet(
            static_cast<std::uint64_t>(spvp));
        dut_.io_ooo_to_mem_tlbCsr_priv_mxr.ImmSet(mxr);
        dut_.io_ooo_to_mem_tlbCsr_priv_vmxr.ImmSet(vmxr);
        dut_.io_ooo_to_mem_tlbCsr_priv_vsum.ImmSet(vsum);
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{1});
        tick(false);
        dut_.io_ooo_to_mem_tlbCsr_priv_virt_changed.ImmSet(std::uint64_t{0});
        return run_cycles(16) && check_components();
    }

    bool configure_pmp(
        const std::vector<std::uint64_t> &encoded_addresses,
        const std::vector<std::uint8_t> &config_bytes)
    {
        constexpr unsigned pmp_entries = 32;
        constexpr unsigned entries_per_config_csr = 8;
        constexpr std::uint16_t pmpcfg0 = 0x3a0;
        constexpr std::uint16_t pmpaddr0 = 0x3b0;
        if (encoded_addresses.size() != config_bytes.size() ||
            encoded_addresses.size() > pmp_entries) {
            error_ = "PMP address/config vectors must have equal size at most 32";
            return false;
        }
        // Address registers must be established before enabling TOR/NAPOT.
        // This ordering also lets tests observe that a locked entry rejects a
        // later address rewrite.
        for (unsigned index = 0; index < encoded_addresses.size(); ++index) {
            if (!write_distributed_csr(
                    static_cast<std::uint16_t>(pmpaddr0 + index),
                    encoded_addresses[index])) {
                return false;
            }
        }
        for (unsigned bank = 0; bank < pmp_entries / entries_per_config_csr;
             ++bank) {
            std::uint64_t packed = 0;
            for (unsigned byte = 0; byte < entries_per_config_csr; ++byte) {
                const unsigned index = bank * entries_per_config_csr + byte;
                if (index < config_bytes.size()) {
                    packed |= std::uint64_t{config_bytes[index]} << (byte * 8);
                }
            }
            if (!write_distributed_csr(
                    static_cast<std::uint16_t>(pmpcfg0 + bank * 2), packed)) {
                return false;
            }
        }
        return run_cycles(16) && check_components();
    }

    bool set_page_based_memory_types(
        bool machine_enabled,
        bool hypervisor_enabled)
    {
        dut_.io_ooo_to_mem_tlbCsr_mPBMTE.ImmSet(machine_enabled);
        dut_.io_ooo_to_mem_tlbCsr_hPBMTE.ImmSet(hypervisor_enabled);
        return run_cycles(16) && check_components();
    }

    bool set_uncache_write_outstanding(bool enabled)
    {
        dut_.io_ooo_to_mem_csrCtrl_uncache_write_outstanding_enable.ImmSet(
            enabled);
        return run_cycles(16) && check_components();
    }

    bool pulse_sbuffer_flush()
    {
        dut_.io_ooo_to_mem_flushSb.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_ooo_to_mem_flushSb.ImmSet(std::uint64_t{0});
        tick();
        return check_components();
    }

    void drive_l2_flush(bool enable, bool done)
    {
        dut_.io_ooo_to_mem_csrCtrl_flush_l2_enable.ImmSet(enable);
        dut_.io_l2_flush_done.ImmSet(done);
    }

    bool outer_l2_flush_enabled()
    {
        return dut_.io_outer_l2_flush_en.B();
    }

    bool backend_l2_flush_done()
    {
        return dut_.io_mem_to_ooo_topToBackendBypass_l2FlushDone.B();
    }

    std::uint64_t l2_flush_checks() const
    {
        return l2_flush_checks_;
    }

    void drive_top_controls(
        std::uint8_t hart_id, std::uint64_t reset_vector,
        bool power_down, bool cpu_halted, bool cpu_critical_error)
    {
        dut_.io_hartId.ImmSet(hart_id & 0x3fU);
        dut_.io_outer_reset_vector.ImmSet(
            reset_vector & ((std::uint64_t{1} << 48) - 1U));
        dut_.io_ooo_to_mem_csrCtrl_power_down_enable.ImmSet(power_down);
        dut_.io_ooo_to_mem_backendToTopBypass_cpuHalted.ImmSet(cpu_halted);
        dut_.io_ooo_to_mem_backendToTopBypass_cpuCriticalError.ImmSet(
            cpu_critical_error);
    }

    std::uint8_t backend_hart_id()
    {
        return static_cast<std::uint8_t>(
            dut_.io_mem_to_ooo_topToBackendBypass_hartId.U());
    }

    std::uint64_t inner_reset_vector()
    {
        return dut_.io_inner_reset_vector.U();
    }

    bool outer_power_down_enabled()
    {
        return dut_.io_outer_power_down_en.B();
    }

    bool outer_cpu_halted()
    {
        return dut_.io_outer_cpu_halt.B();
    }

    bool outer_cpu_critical_error()
    {
        return dut_.io_outer_cpu_critical_error.B();
    }

    std::uint64_t top_control_checks() const
    {
        return top_control_checks_;
    }

    void drive_top_bridge_stimulus(const TopBridgeStimulus &stimulus)
    {
        dut_.io_ooo_to_mem_backendToTopBypass_msiAck.ImmSet(stimulus.msi_ack);
        dut_.io_resetInFrontendBypass_fromFrontend.ImmSet(
            stimulus.frontend_reset);
        dut_.io_inner_beu_errors_icache_ecc_error_valid.ImmSet(
            stimulus.beu_valid);
        dut_.io_inner_beu_errors_icache_ecc_error_bits.ImmSet(
            stimulus.beu_address & ((std::uint64_t{1} << 48) - 1U));
        dut_.io_fromTopToBackend_msiInfo_valid.ImmSet(
            stimulus.msi_info_valid);
        dut_.io_fromTopToBackend_msiInfo_bits.ImmSet(
            stimulus.msi_info & 0xfffU);
        dut_.io_fromTopToBackend_clintTime_valid.ImmSet(
            stimulus.clint_time_valid);
        dut_.io_fromTopToBackend_clintTime_bits.ImmSet(stimulus.clint_time);
        dut_.auto_inner_clint_int_sink_in_0.ImmSet(stimulus.interrupt_msip);
        dut_.auto_inner_clint_int_sink_in_1.ImmSet(stimulus.interrupt_mtip);
        dut_.auto_inner_plic_int_sink_in_0_0.ImmSet(stimulus.interrupt_meip);
        dut_.auto_inner_plic_int_sink_in_1_0.ImmSet(stimulus.interrupt_seip);
        dut_.auto_inner_debug_int_sink_in_0.ImmSet(stimulus.interrupt_debug);
        dut_.auto_inner_nmi_int_sink_in_0.ImmSet(stimulus.interrupt_nmi_31);
        dut_.auto_inner_nmi_int_sink_in_1.ImmSet(stimulus.interrupt_nmi_43);
        dut_.auto_inner_beu_local_int_sink_in_0.ImmSet(
            stimulus.interrupt_beu_local);
        for (unsigned lane = generated::kHcPerfEventFirstInputLane;
             lane <= generated::kHcPerfEventHighestInputLane; ++lane) {
            generated::drive_hc_perf_event(
                dut_, lane, stimulus.hc_perf_events[lane]);
        }
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable.ImmSet(
            stimulus.l2_prefetch.master_enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable.ImmSet(
            stimulus.l2_prefetch.receive_enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable.ImmSet(
            stimulus.l2_prefetch.pbop_enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable.ImmSet(
            stimulus.l2_prefetch.vbop_enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable.ImmSet(
            stimulus.l2_prefetch.tp_enable);
        dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency.ImmSet(
            stimulus.l2_prefetch.delay_latency & 0x3ffU);
    }

    std::uint64_t top_bridge_checks() const
    {
        return top_bridge_checks_;
    }

    void drive_trace_bridge_stimulus(const TraceBridgeStimulus &stimulus)
    {
        dut_.io_traceCoreInterfaceBypass_toL2Top_fromEncoder_enable.ImmSet(
            stimulus.encoder_enable);
        dut_.io_traceCoreInterfaceBypass_toL2Top_fromEncoder_stall.ImmSet(
            stimulus.encoder_stall);
        dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_priv.ImmSet(
            stimulus.privilege & 0x7U);
        dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_mstatus.ImmSet(
            stimulus.mstatus);
        dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_trap_cause.ImmSet(
            stimulus.trap_cause);
        dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_trap_tval.ImmSet(
            stimulus.trap_tval & ((std::uint64_t{1} << 50) - 1U));

        const auto drive_group = [](auto &valid, auto &iaddr, auto &ftq_offset,
                                    auto &itype, auto &iretire, auto &ilastsize,
                                    const TraceGroupState &group) {
            valid.ImmSet(group.valid);
            iaddr.ImmSet(group.iaddr & ((std::uint64_t{1} << 50) - 1U));
            ftq_offset.ImmSet(group.ftq_offset & 0xfU);
            itype.ImmSet(group.itype & 0xfU);
            iretire.ImmSet(group.iretire & 0x7fU);
            ilastsize.ImmSet(group.ilastsize);
        };
        drive_group(
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_valid,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_iaddr,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_ftqOffset,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_itype,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_iretire,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_ilastsize,
            stimulus.groups[0]);
        drive_group(
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_valid,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_iaddr,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_ftqOffset,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_itype,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_iretire,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_ilastsize,
            stimulus.groups[1]);
        drive_group(
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_valid,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_iaddr,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_ftqOffset,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_itype,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_iretire,
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_ilastsize,
            stimulus.groups[2]);
    }

    std::uint64_t trace_bridge_checks() const
    {
        return trace_bridge_checks_;
    }

    bool check_dft_bridge_space()
    {
        dft_bridge_patterns_ = 0;
        dft_bridge_digest_ = 1469598103934665603ULL;
        dut_.reset.ImmSet(std::uint64_t{1});
        for (unsigned pattern = 0; pattern < 1024; ++pattern) {
            const bool ram_hold = (pattern & (1U << 0)) != 0;
            const bool ram_bypass = (pattern & (1U << 1)) != 0;
            const bool ram_bp_clken = (pattern & (1U << 2)) != 0;
            const bool ram_aux_clk = (pattern & (1U << 3)) != 0;
            const bool ram_aux_ckbp = (pattern & (1U << 4)) != 0;
            const bool ram_mcp_hold = (pattern & (1U << 5)) != 0;
            const bool cgen = (pattern & (1U << 6)) != 0;
            const bool lgc_rst_n = (pattern & (1U << 7)) != 0;
            const bool reset_mode = (pattern & (1U << 8)) != 0;
            const bool scan_mode = (pattern & (1U << 9)) != 0;
            dut_.io_dft_ram_hold.ImmSet(ram_hold);
            dut_.io_dft_ram_bypass.ImmSet(ram_bypass);
            dut_.io_dft_ram_bp_clken.ImmSet(ram_bp_clken);
            dut_.io_dft_ram_aux_clk.ImmSet(ram_aux_clk);
            dut_.io_dft_ram_aux_ckbp.ImmSet(ram_aux_ckbp);
            dut_.io_dft_ram_mcp_hold.ImmSet(ram_mcp_hold);
            dut_.io_dft_cgen.ImmSet(cgen);
            dut_.io_dft_reset_lgc_rst_n.ImmSet(lgc_rst_n);
            dut_.io_dft_reset_mode.ImmSet(reset_mode);
            dut_.io_dft_reset_scan_mode.ImmSet(scan_mode);
            dut_.RefreshComb();
            const bool matches =
                dut_.io_dft_frnt_ram_hold.B() == ram_hold &&
                dut_.io_dft_frnt_ram_bypass.B() == ram_bypass &&
                dut_.io_dft_frnt_ram_bp_clken.B() == ram_bp_clken &&
                dut_.io_dft_frnt_ram_aux_clk.B() == ram_aux_clk &&
                dut_.io_dft_frnt_ram_aux_ckbp.B() == ram_aux_ckbp &&
                dut_.io_dft_frnt_ram_mcp_hold.B() == ram_mcp_hold &&
                dut_.io_dft_frnt_cgen.B() == cgen &&
                dut_.io_dft_bcknd_cgen.B() == cgen &&
                dut_.io_dft_reset_frnt_lgc_rst_n.B() == lgc_rst_n &&
                dut_.io_dft_reset_frnt_mode.B() == reset_mode &&
                dut_.io_dft_reset_frnt_scan_mode.B() == scan_mode &&
                dut_.io_dft_reset_bcknd_lgc_rst_n.B() == lgc_rst_n &&
                dut_.io_dft_reset_bcknd_mode.B() == reset_mode &&
                dut_.io_dft_reset_bcknd_scan_mode.B() == scan_mode;
            if (!matches) {
                std::ostringstream message;
                message << "DFT bridge mismatch pattern=0x" << std::hex
                        << pattern;
                error_ = message.str();
                return false;
            }
            ++dft_bridge_patterns_;
            dft_bridge_digest_ ^= pattern;
            dft_bridge_digest_ *= 1099511628211ULL;
        }
        dut_.io_dft_ram_hold.ImmSet(std::uint64_t{0});
        dut_.io_dft_ram_bypass.ImmSet(std::uint64_t{0});
        dut_.io_dft_ram_bp_clken.ImmSet(std::uint64_t{0});
        dut_.io_dft_ram_aux_clk.ImmSet(std::uint64_t{0});
        dut_.io_dft_ram_aux_ckbp.ImmSet(std::uint64_t{0});
        dut_.io_dft_ram_mcp_hold.ImmSet(std::uint64_t{0});
        dut_.io_dft_cgen.ImmSet(std::uint64_t{0});
        dut_.io_dft_reset_lgc_rst_n.ImmSet(std::uint64_t{1});
        dut_.io_dft_reset_mode.ImmSet(std::uint64_t{0});
        dut_.io_dft_reset_scan_mode.ImmSet(std::uint64_t{0});
        dut_.RefreshComb();
        return true;
    }

    std::uint64_t dft_bridge_patterns() const
    {
        return dft_bridge_patterns_;
    }

    std::uint64_t dft_bridge_digest() const
    {
        return dft_bridge_digest_;
    }

    bool set_sbuffer_timeout(std::uint32_t cycles)
    {
        constexpr std::uint32_t timeout_width = 22;
        if (cycles >= (std::uint32_t{1} << timeout_width)) {
            error_ = "SBuffer timeout exceeds the 22-bit CSR field";
            return false;
        }
        dut_.io_ooo_to_mem_csrCtrl_sbuffer_timeout.ImmSet(cycles);
        return run_cycles(4) && check_components();
    }

    bool set_mbmc(
        bool bitmap_enabled, bool confidential_mode,
        std::uint64_t bitmap_base)
    {
        if ((bitmap_base & 63U) != 0) {
            error_ = "MBMC bitmap base must be 64-byte aligned";
            return false;
        }
        dut_.io_ooo_to_mem_tlbCsr_mbmc_BME.ImmSet(bitmap_enabled);
        dut_.io_ooo_to_mem_tlbCsr_mbmc_CMODE.ImmSet(confidential_mode);
        dut_.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR.ImmSet(std::uint64_t{0});
        dut_.io_ooo_to_mem_tlbCsr_mbmc_BMA.ImmSet(bitmap_base >> 6);
        return run_cycles(16) && check_components();
    }

    bool pulse_mbmc_bitmap_clear()
    {
        dut_.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR.ImmSet(std::uint64_t{1});
        if (!run_cycles(8)) {
            return false;
        }
        dut_.io_ooo_to_mem_tlbCsr_mbmc_BCLEAR.ImmSet(std::uint64_t{0});
        return run_cycles(8) && check_components();
    }

    bool set_pointer_masking(const PointerMaskingConfig &config)
    {
        dut_.io_ooo_to_mem_tlbCsr_pmm_mseccfg.ImmSet(
            static_cast<std::uint64_t>(config.machine));
        dut_.io_ooo_to_mem_tlbCsr_pmm_menvcfg.ImmSet(
            static_cast<std::uint64_t>(config.supervisor));
        dut_.io_ooo_to_mem_tlbCsr_pmm_henvcfg.ImmSet(
            static_cast<std::uint64_t>(config.virtual_supervisor));
        dut_.io_ooo_to_mem_tlbCsr_pmm_hstatus.ImmSet(
            static_cast<std::uint64_t>(config.hypervisor_user));
        dut_.io_ooo_to_mem_tlbCsr_pmm_senvcfg.ImmSet(
            static_cast<std::uint64_t>(config.user));
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

    bool issue_sfence_with_redirect(
        std::uint8_t redirect_rob_value,
        bool redirect_rob_flag,
        bool redirect_flush_itself,
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
        tick();
        dut_.io_ooo_to_mem_sfence_valid.ImmSet(std::uint64_t{0});

        // MemBlock registers sfence twice and redirect once. Pulse redirect
        // one top-level cycle later so both reach their consumers together.
        dut_.io_redirect_bits_robIdx_flag.ImmSet(redirect_rob_flag);
        dut_.io_redirect_bits_robIdx_value.ImmSet(redirect_rob_value);
        dut_.io_redirect_bits_level.ImmSet(redirect_flush_itself);
        dut_.io_redirect_bits_isVlsException.ImmSet(std::uint64_t{0});
        dut_.io_redirect_valid.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_redirect_valid.ImmSet(std::uint64_t{0});
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
        // ROB keeps pendingst high while this store remains at its head. Keep
        // the real level contract until StoreQueue emits the Uncache request.
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{1});
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            tick();
            if (!check_components()) {
                dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
                return false;
            }
            if (uncache_agent_.request_count() > request_before) {
                dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
                return true;
            }
        }
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
        error_ = "timed out waiting for MMIO store Uncache request";
        return false;
    }

    bool set_wfi(bool enabled)
    {
        dut_.io_wfi_wfiReq.ImmSet(enabled);
        tick();
        return check_components();
    }

    bool wfi_safe()
    {
        dut_.RefreshComb();
        return dut_.io_wfi_wfiSafe.B();
    }

    bool require_wfi_unsafe(unsigned cycles)
    {
        for (unsigned elapsed = 0; elapsed < cycles; ++elapsed) {
            dut_.RefreshComb();
            if (dut_.io_wfi_wfiSafe.B()) {
                std::ostringstream message;
                message << "wfiSafe asserted while a manager response was pending"
                        << " elapsed=" << elapsed;
                error_ = message.str();
                return false;
            }
            tick();
            if (!check_components()) {
                return false;
            }
        }
        return true;
    }

    bool run_until_wfi_safe(unsigned timeout = 4096)
    {
        for (unsigned elapsed = 0; elapsed < timeout; ++elapsed) {
            dut_.RefreshComb();
            if (dut_.io_wfi_wfiSafe.B()) {
                return check_components();
            }
            tick();
            if (!check_components()) {
                return false;
            }
        }
        error_ = "timed out waiting for wfiSafe after manager drain";
        return false;
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
        if (!scalar_immediate_is_valid(transaction.immediate)) {
            error_ = "scalar load immediate exceeds signed 12-bit range";
            return false;
        }
        generated::ScalarLoadIssue issue;
        issue.pc = transaction.pc.value_or(0x1000 + transaction.rob * 4);
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
        issue.imm = static_cast<std::uint32_t>(transaction.immediate);
        issue.src = scalar_issue_base_address(
            transaction.address, transaction.immediate);

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
        const std::vector<LoadTransaction> &transactions, unsigned timeout = 64,
        bool require_same_cycle = false)
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
            if (!scalar_immediate_is_valid(transaction.immediate)) {
                error_ = "scalar load immediate exceeds signed 12-bit range";
                return false;
            }
            lane_used[transaction.lane] = true;
            auto &issue = issues[index];
            issue.pc = transaction.pc.value_or(
                0x1000 + transaction.rob * 4);
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
            issue.imm = static_cast<std::uint32_t>(transaction.immediate);
            issue.src = scalar_issue_base_address(
                transaction.address, transaction.immediate);
        }

        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            if (require_same_cycle) {
                for (std::size_t index = 0; index < transactions.size(); ++index) {
                    generated::drive_scalar_load_issue(
                        dut_, transactions[index].lane, issues[index]);
                }
                dut_.RefreshComb();
                const bool all_ready = std::all_of(
                    transactions.begin(), transactions.end(),
                    [&](const auto &transaction) {
                        return generated::scalar_load_issue_ready(
                            dut_, transaction.lane);
                    });
                if (all_ready) {
                    tick();
                    generated::clear_scalar_load_issue_valids(dut_);
                    return check_components();
                }
                generated::clear_scalar_load_issue_valids(dut_);
                tick();
                if (!check_components()) {
                    return false;
                }
                continue;
            }
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
        error_ = require_same_cycle
            ? "scalar load issue batch timed out waiting for same-cycle ready"
            : "scalar load issue batch timed out waiting for ready";
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

    bool enqueue_prefetch_batch(
        const std::vector<PrefetchTransaction> &transactions,
        const std::vector<unsigned> &dispatch_lanes)
    {
        if (transactions.empty() ||
            transactions.size() != dispatch_lanes.size() ||
            transactions.size() > generated::kLsqEnqueueLanes ||
            !std::is_sorted(dispatch_lanes.begin(), dispatch_lanes.end())) {
            error_ = "LSQ prefetch batch requires one sorted unique dispatch lane per request";
            return false;
        }
        std::array<bool, generated::kLsqEnqueueLanes> lane_used{};
        if (!wait_for_enqueue_capacity(transactions.size(), 0)) {
            return false;
        }
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            const unsigned lane = dispatch_lanes[index];
            if (lane >= generated::kLsqEnqueueLanes || lane_used[lane]) {
                error_ = "LSQ prefetch batch has an invalid or duplicate dispatch lane";
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

    bool issue_prefetch(
        const PrefetchTransaction &transaction, unsigned timeout = 32)
    {
        if (!scalar_immediate_is_valid(transaction.immediate)) {
            error_ = "software-prefetch immediate exceeds signed 12-bit range";
            return false;
        }
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
        issue.imm = static_cast<std::uint32_t>(transaction.immediate);
        issue.src = scalar_issue_base_address(
            transaction.address, transaction.immediate);
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

    bool issue_prefetch_batch_same_cycle(
        const std::vector<PrefetchTransaction> &transactions,
        unsigned timeout = 64)
    {
        if (transactions.empty() || transactions.size() > kScalarLoadLanes) {
            error_ = "software-prefetch batch must contain one to three requests";
            return false;
        }
        std::array<bool, kScalarLoadLanes> lane_used{};
        std::vector<generated::ScalarLoadIssue> issues(transactions.size());
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            const auto &transaction = transactions[index];
            if (transaction.lane >= kScalarLoadLanes ||
                lane_used[transaction.lane]) {
                error_ = "software-prefetch batch lanes must be unique";
                return false;
            }
            if (!scalar_immediate_is_valid(transaction.immediate)) {
                error_ = "software-prefetch immediate exceeds signed 12-bit range";
                return false;
            }
            lane_used[transaction.lane] = true;
            auto &issue = issues[index];
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
            issue.imm = static_cast<std::uint32_t>(transaction.immediate);
            issue.src = scalar_issue_base_address(
                transaction.address, transaction.immediate);
        }
        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            for (std::size_t index = 0; index < transactions.size(); ++index) {
                generated::drive_scalar_load_issue(
                    dut_, transactions[index].lane, issues[index]);
            }
            dut_.RefreshComb();
            const bool all_ready = std::all_of(
                transactions.begin(), transactions.end(),
                [&](const auto &transaction) {
                    return generated::scalar_load_issue_ready(
                        dut_, transaction.lane);
                });
            if (all_ready) {
                tick();
                generated::clear_scalar_load_issue_valids(dut_);
                return check_components();
            }
            generated::clear_scalar_load_issue_valids(dut_);
            tick();
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_scalar_load_issue_valids(dut_);
        error_ = "software-prefetch batch timed out waiting for same-cycle ready";
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
        return enqueue_store_pressure(transaction, lq_value);
    }

    bool enqueue_store_pressure(
        const StoreTransaction &transaction, std::uint8_t lq_value)
    {
        const std::uint64_t retired = sq_dequeued_ + sq_canceled_;
        if (retired > sq_allocated_ ||
            sq_allocated_ - retired >= kStoreQueueEntries) {
            error_ = "store pressure enqueue exceeds StoreQueue capacity";
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
        scalar_store_sq_targets_[scalar_store_key(transaction)] =
            sq_allocated_ - sq_canceled_;
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
        if (transaction.segment) {
            return check_components();
        }
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
        enqueue.fu_type = vector_fu_type(transaction);
        enqueue.fu_op_type = vector_fu_op_type(transaction);
        enqueue.uop_idx = transaction.vuop_idx;
        enqueue.last_uop = transaction.last_uop;
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
            vector_store_sq_targets_[vector_store_key(transaction)] =
                sq_allocated_ - sq_canceled_;
        }
        return check_components();
    }

    bool issue_vector(
        const VectorMemoryTransaction &transaction, unsigned timeout = 64)
    {
        const generated::VectorMemoryIssue issue =
            make_vector_memory_issue(transaction);

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

    bool issue_vector_batch_same_cycle(
        const std::vector<VectorMemoryTransaction> &transactions,
        unsigned timeout = 64)
    {
        if (transactions.empty() || transactions.size() > kVectorMemoryLanes) {
            error_ = "vector issue batch must contain one or two transactions";
            return false;
        }
        std::array<bool, kVectorMemoryLanes> lane_used{};
        std::vector<generated::VectorMemoryIssue> issues;
        issues.reserve(transactions.size());
        for (const auto &transaction : transactions) {
            if (transaction.lane >= kVectorMemoryLanes ||
                lane_used[transaction.lane]) {
                error_ = "vector issue batch lanes must be unique";
                return false;
            }
            lane_used[transaction.lane] = true;
            issues.push_back(make_vector_memory_issue(transaction));
        }

        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            for (std::size_t index = 0; index < transactions.size(); ++index) {
                generated::drive_vector_memory_issue(
                    dut_, transactions[index].lane, issues[index]);
            }
            dut_.RefreshComb();
            const bool all_ready = std::all_of(
                transactions.begin(), transactions.end(), [&](const auto &transaction) {
                    return generated::vector_memory_issue_ready(
                        dut_, transaction.lane);
                });
            if (all_ready) {
                tick();
                generated::clear_vector_memory_issue_valids(dut_);
                return check_components();
            }
            generated::clear_vector_memory_issue_valids(dut_);
            tick();
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_vector_memory_issue_valids(dut_);
        error_ = "vector issue batch timed out waiting for same-cycle ready";
        return false;
    }

    bool issue_load_vector_pair(
        const LoadTransaction &load,
        const VectorMemoryTransaction &vector,
        unsigned timeout = 64)
    {
        if (!scalar_immediate_is_valid(load.immediate)) {
            error_ = "scalar load immediate exceeds signed 12-bit range";
            return false;
        }
        generated::ScalarLoadIssue scalar_issue;
        scalar_issue.pc = load.pc.value_or(0x1000 + load.rob * 4);
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
        scalar_issue.imm = static_cast<std::uint32_t>(load.immediate);
        scalar_issue.src = scalar_issue_base_address(
            load.address, load.immediate);

        const generated::VectorMemoryIssue vector_issue =
            make_vector_memory_issue(vector);

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
        if (!scalar_immediate_is_valid(transaction.immediate)) {
            error_ = "scalar store immediate exceeds signed 12-bit range";
            return false;
        }
        generated::ScalarStoreIssue issue;
        issue.fu_type = kFuTypeStore;
        issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
        issue.rob_flag = transaction.rob_flag;
        issue.rob_value = transaction.rob;
        issue.sq_flag = transaction.sq_flag;
        issue.sq_value = transaction.sq;
        issue.imm = static_cast<std::uint32_t>(transaction.immediate);
        issue.src = scalar_issue_base_address(
            transaction.address, transaction.immediate);
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

    bool issue_store_address_batch(
        const std::vector<StoreTransaction> &transactions,
        unsigned timeout = 64)
    {
        if (transactions.empty() || transactions.size() > kScalarStoreLanes) {
            error_ = "scalar store-address batch must contain one or two transactions";
            return false;
        }
        std::array<bool, kScalarStoreLanes> lane_used{};
        std::vector<generated::ScalarStoreIssue> issues(transactions.size());
        for (std::size_t index = 0; index < transactions.size(); ++index) {
            const auto &transaction = transactions[index];
            if (transaction.address_lane >= kScalarStoreLanes ||
                lane_used[transaction.address_lane]) {
                error_ = "scalar store-address batch lanes must be unique";
                return false;
            }
            if (!scalar_immediate_is_valid(transaction.immediate)) {
                error_ = "scalar store immediate exceeds signed 12-bit range";
                return false;
            }
            lane_used[transaction.address_lane] = true;
            auto &issue = issues[index];
            issue.fu_type = kFuTypeStore;
            issue.fu_op_type = static_cast<std::uint16_t>(transaction.op);
            issue.rob_flag = transaction.rob_flag;
            issue.rob_value = transaction.rob;
            issue.sq_flag = transaction.sq_flag;
            issue.sq_value = transaction.sq;
            issue.imm = static_cast<std::uint32_t>(transaction.immediate);
            issue.src = scalar_issue_base_address(
                transaction.address, transaction.immediate);
        }

        for (unsigned cycle = 0; cycle < timeout; ++cycle) {
            for (std::size_t index = 0; index < transactions.size(); ++index) {
                generated::drive_scalar_store_address(
                    dut_, transactions[index].address_lane, issues[index]);
            }
            dut_.RefreshComb();
            const bool all_ready = std::all_of(
                transactions.begin(), transactions.end(), [&](const auto &transaction) {
                    return generated::scalar_store_address_ready(
                        dut_, transaction.address_lane);
                });
            if (all_ready) {
                for (const auto &transaction : transactions) {
                    if (!store_scoreboard_.mark_address_issued(
                            transaction, this->cycle())) {
                        generated::clear_scalar_store_issue_valids(dut_);
                        return false;
                    }
                }
                tick();
                generated::clear_scalar_store_issue_valids(dut_);
                return check_components();
            }
            generated::clear_scalar_store_issue_valids(dut_);
            tick();
            if (!check_components()) {
                return false;
            }
        }
        generated::clear_scalar_store_issue_valids(dut_);
        error_ = "scalar store-address batch timed out waiting for same-cycle ready";
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

    bool issue_store_address_until_tlb_hit(
        const StoreTransaction &transaction, unsigned timeout = 4096)
    {
        const std::uint64_t deadline = cycle() + timeout;
        while (cycle() < deadline) {
            const std::uint64_t feedbacks_before = store_tlb_feedbacks_;
            const std::uint64_t misses_before = store_tlb_misses_;
            const unsigned issue_timeout = static_cast<unsigned>(std::min(
                std::uint64_t{256}, deadline - cycle()));
            if (issue_timeout == 0 ||
                !issue_store_address(transaction, issue_timeout)) {
                return false;
            }
            while (cycle() < deadline &&
                   store_tlb_feedbacks_ == feedbacks_before) {
                tick();
                if (!check_components()) {
                    return false;
                }
            }
            if (store_tlb_feedbacks_ == feedbacks_before) {
                break;
            }
            if (store_tlb_misses_ == misses_before) {
                return check_components();
            }
            if (!run_cycles(static_cast<unsigned>(std::min(
                    std::uint64_t{8}, deadline - cycle())))) {
                return false;
            }
        }
        error_ = "timed out replaying store address until DTLB hit";
        return false;
    }

    bool run_until_store_complete_with_replay(
        const StoreTransaction &transaction, unsigned timeout = 4096,
        bool hold_pending_store = false)
    {
        // ROB keeps pendingst asserted while the same store remains at its
        // head. A cold replay may not enter StoreMisalignBuffer until well
        // after the first address issue, so retain that level across replays.
        if (hold_pending_store) {
            dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(transaction.rob);
            dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(transaction.rob_flag);
            dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{1});
        }
        const auto clear_pending_store = [&]() {
            if (hold_pending_store) {
                dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
            }
        };
        constexpr unsigned replay_interval = 32;
        for (unsigned elapsed = 0;
             elapsed < timeout && !store_scoreboard_.done();) {
            const unsigned cycles = std::min(replay_interval, timeout - elapsed);
            if (!run_cycles(cycles)) {
                clear_pending_store();
                return false;
            }
            elapsed += cycles;
            if (store_scoreboard_.done()) {
                clear_pending_store();
                return check_components();
            }
            if (!issue_store_address(transaction, replay_interval)) {
                clear_pending_store();
                return false;
            }
            ++elapsed;
        }
        clear_pending_store();
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

    bool run_until_dcache_requests(
        std::uint64_t target, unsigned timeout = 4096)
    {
        for (unsigned elapsed = 0;
             elapsed < timeout && memory_agent_.request_count() < target;
             ++elapsed) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (memory_agent_.request_count() < target) {
            error_ = "timed out waiting for target DCache request count";
            return false;
        }
        return check_components();
    }

    bool run_until_ptw_request_covering(
        std::uint64_t address,
        std::uint64_t first_request,
        unsigned timeout = 4096,
        unsigned min_response_delay = 0)
    {
        for (unsigned cycle = 0;
             cycle < timeout &&
             !ptw_agent_.request_covers_address_since(address, first_request);
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (!ptw_agent_.request_covers_address_since(address, first_request)) {
            std::ostringstream message;
            message << "timed out waiting for PTW request covering address 0x"
                    << std::hex << address;
            error_ = message.str();
            return false;
        }
        if (!ptw_agent_.request_covering_address_has_min_delay_since(
                address, first_request, min_response_delay)) {
            std::ostringstream message;
            message << "PTW request covering address 0x" << std::hex
                    << address << std::dec
                    << " did not receive the required minimum response delay "
                    << min_response_delay;
            error_ = message.str();
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

    bool run_until_uncache_drained(
        std::uint64_t request_target, unsigned timeout = 32768)
    {
        for (unsigned cycle = 0;
             cycle < timeout &&
             (uncache_agent_.request_count() < request_target ||
              uncache_agent_.outstanding_requests() != 0);
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (uncache_agent_.request_count() < request_target ||
            uncache_agent_.outstanding_requests() != 0) {
            std::ostringstream message;
            message << "timed out waiting for Uncache traffic to drain"
                    << " requests=" << uncache_agent_.request_count()
                    << '/' << request_target
                    << " outstanding="
                    << uncache_agent_.outstanding_requests();
            error_ = message.str();
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

    bool run_until_load_writebacks(std::uint64_t target, unsigned timeout)
    {
        for (unsigned cycle = 0;
             cycle < timeout && scoreboard_.observed() < target; ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (scoreboard_.observed() < target) {
            std::ostringstream message;
            message << "timed out waiting for scalar load writebacks observed="
                    << scoreboard_.observed() << '/' << target;
            error_ = message.str();
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

    bool run_until_vector_complete_with_replays(
        const std::vector<VectorMemoryTransaction> &transactions,
        unsigned timeout,
        bool pulse_store_commit_after_replay = false)
    {
        const auto matches = [](const VectorMemoryTransaction &transaction,
                                const VectorReplayRequest &request) {
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
        };

        const std::uint64_t deadline = cycle() + timeout;
        while (!vector_scoreboard_.done() && cycle() < deadline) {
            if (vector_replay_requests_.empty()) {
                tick();
                if (!check_components()) {
                    return false;
                }
                continue;
            }
            const VectorReplayRequest request = vector_replay_requests_.front();
            vector_replay_requests_.pop_front();
            const auto transaction = std::find_if(
                transactions.begin(), transactions.end(),
                [&](const VectorMemoryTransaction &candidate) {
                    return matches(candidate, request);
                });
            if (transaction == transactions.end()) {
                error_ = "vector replay did not match a mixed-window transaction";
                return false;
            }
            auto replay = *transaction;
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
            message << "timed out waiting for mixed vector replays"
                    << " transactions=" << transactions.size()
                    << " pending_replays=" << vector_replay_requests_.size()
                    << " ptw_requests=" << ptw_agent_.request_count();
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
        lq_canceled_unobserved_ += count;
        return true;
    }

    bool account_sq_cancellation(unsigned count)
    {
        if (sq_dequeued_ + sq_canceled_ + count > sq_allocated_) {
            error_ = "SQ cancellation accounting exceeds allocated entries";
            return false;
        }
        sq_canceled_ += count;
        sq_canceled_unobserved_ += count;
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
        const auto target_it = scalar_store_sq_targets_.find(
            scalar_store_key(transaction));
        if (target_it == scalar_store_sq_targets_.end()) {
            error_ = "scalar store has no recorded SQ allocation target";
            return false;
        }
        const std::uint64_t target = target_it->second;
        // A misaligned store at the ROB head can leave the SQ while another
        // outstanding class is still draining.  Do not turn that completed
        // store into a wait for the following SQ entry.
        if (sq_dequeued_ < target &&
            !commit_stores_through(transaction, 1)) {
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
        scalar_store_sq_targets_.erase(target_it);
        const std::uint64_t raw_address = transaction.oracle_address.value_or(
            transaction.address);
        const std::uint64_t address = transaction.op == StoreOp::cbo_zero
            ? raw_address & ~std::uint64_t{63}
            : raw_address;
        // CBO.ZERO is encoded as 0x7 but architecturally covers one cache
        // line, not a 128-byte scalar transfer.
        const unsigned bytes = scalar_store_bytes(transaction.op);
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
        const auto target_it = vector_store_sq_targets_.find(
            vector_store_key(transaction));
        if (target_it == vector_store_sq_targets_.end()) {
            error_ = "vector store has no recorded SQ allocation target";
            return false;
        }
        const std::uint64_t target = target_it->second;
        StoreTransaction commit_point{
            .rob = transaction.rob,
            .rob_flag = transaction.rob_flag,
        };
        if (sq_dequeued_ < target &&
            !commit_stores_through(commit_point, 1)) {
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
        vector_store_sq_targets_.erase(target_it);
        const std::uint64_t address = transaction.oracle_address.value_or(
            transaction.address);
        const unsigned element_bytes = 1U << vector_data_eew(transaction);
        const unsigned elements = 16U >> vector_data_eew(transaction);
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
        const unsigned bytes = scalar_store_bytes(transaction.op);
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
        const unsigned element_bytes = 1U << vector_data_eew(transaction);
        const unsigned elements = 16U >> vector_data_eew(transaction);
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

    bool run_until_probe_responses(
        std::uint64_t target, unsigned timeout = 4096)
    {
        for (unsigned cycle = 0;
             cycle < timeout && memory_agent_.probe_response_count() < target;
             ++cycle) {
            tick();
            if (!check_components()) {
                return false;
            }
        }
        if (memory_agent_.probe_response_count() < target ||
            !memory_agent_.probes_idle()) {
            error_ = "timed out waiting for DCache ProbeAck response";
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
        // MemBlock registers redirect once; the queues publish their retained
        // redirectCancelCount values after two more clock edges. Sample once
        // per known redirect instead of treating the retained value as a pulse.
        tick();
        tick();
        dut_.RefreshComb();
        const std::uint64_t lq_canceled =
            dut_.io_mem_to_ooo_lqCancelCnt.U();
        const std::uint64_t sq_canceled =
            dut_.io_mem_to_ooo_sqCancelCnt.U();
        if (lq_dequeued_ + lq_canceled_ + lq_canceled > lq_allocated_ ||
            sq_dequeued_ + sq_canceled_ + sq_canceled > sq_allocated_) {
            std::ostringstream message;
            message << "observed redirect cancellation exceeds queue allocation"
                    << " lq=" << lq_dequeued_ << '+' << lq_canceled_ << '+'
                    << lq_canceled << '/' << lq_allocated_
                    << " sq=" << sq_dequeued_ << '+' << sq_canceled_ << '+'
                    << sq_canceled << '/' << sq_allocated_;
            error_ = message.str();
            return false;
        }
        ++redirect_cancellation_events_observed_;
        lq_redirect_canceled_observed_ += lq_canceled;
        sq_redirect_canceled_observed_ += sq_canceled;
        lq_canceled_ += lq_canceled;
        sq_canceled_ += sq_canceled;
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

        const bool top_down_l2_input =
            dut_.io_topDownInfo_fromL2Top_l2Miss.B();
        const bool top_down_l3_input =
            dut_.io_topDownInfo_fromL2Top_l3Miss.B();
        const bool l2_flush_enable_input =
            dut_.io_ooo_to_mem_csrCtrl_flush_l2_enable.B();
        const bool l2_flush_done_input = dut_.io_l2_flush_done.B();
        const std::uint8_t hart_id_input =
            static_cast<std::uint8_t>(dut_.io_hartId.U());
        const std::uint64_t reset_vector_input =
            dut_.io_outer_reset_vector.U();
        const bool power_down_input =
            dut_.io_ooo_to_mem_csrCtrl_power_down_enable.B();
        const bool cpu_halted_input =
            dut_.io_ooo_to_mem_backendToTopBypass_cpuHalted.B();
        const bool cpu_critical_error_input =
            dut_.io_ooo_to_mem_backendToTopBypass_cpuCriticalError.B();
        const bool msi_ack_input =
            dut_.io_ooo_to_mem_backendToTopBypass_msiAck.B();
        const bool frontend_reset_input =
            dut_.io_resetInFrontendBypass_fromFrontend.B();
        const bool beu_valid_input =
            dut_.io_inner_beu_errors_icache_ecc_error_valid.B();
        const std::uint64_t beu_address_input =
            dut_.io_inner_beu_errors_icache_ecc_error_bits.U();
        const bool msi_info_valid_input =
            dut_.io_fromTopToBackend_msiInfo_valid.B();
        const std::uint16_t msi_info_input = static_cast<std::uint16_t>(
            dut_.io_fromTopToBackend_msiInfo_bits.U());
        const bool clint_time_valid_input =
            dut_.io_fromTopToBackend_clintTime_valid.B();
        const std::uint64_t clint_time_input =
            dut_.io_fromTopToBackend_clintTime_bits.U();
        const ExternalInterruptState external_interrupt_input{
            .msip = dut_.auto_inner_clint_int_sink_in_0.B(),
            .mtip = dut_.auto_inner_clint_int_sink_in_1.B(),
            .meip = dut_.auto_inner_plic_int_sink_in_0_0.B(),
            .seip = dut_.auto_inner_plic_int_sink_in_1_0.B(),
            .debug = dut_.auto_inner_debug_int_sink_in_0.B(),
            .nmi_31 = dut_.auto_inner_nmi_int_sink_in_0.B() ||
                      dut_.auto_inner_beu_local_int_sink_in_0.B(),
            .nmi_43 = dut_.auto_inner_nmi_int_sink_in_1.B(),
        };
        std::array<
            std::uint8_t, generated::kHcPerfEventHighestInputLane + 1>
            hc_perf_event_inputs{};
        for (unsigned lane = generated::kHcPerfEventFirstInputLane;
             lane <= generated::kHcPerfEventHighestInputLane; ++lane) {
            hc_perf_event_inputs[lane] =
                generated::sample_hc_perf_event_input(dut_, lane);
        }
        const L2PrefetchControl l2_prefetch_input{
            .master_enable =
                dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_enable.B(),
            .receive_enable =
                dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_recv_enable.B(),
            .pbop_enable =
                dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_pbop_enable.B(),
            .vbop_enable =
                dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_vbop_enable.B(),
            .tp_enable =
                dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_tp_enable.B(),
            .delay_latency = static_cast<std::uint16_t>(
                dut_.io_ooo_to_mem_csrCtrl_pf_ctrl_l2_pf_delay_latency.U()),
        };
        const bool trace_encoder_enable_input =
            dut_.io_traceCoreInterfaceBypass_toL2Top_fromEncoder_enable.B();
        const bool trace_encoder_stall_input =
            dut_.io_traceCoreInterfaceBypass_toL2Top_fromEncoder_stall.B();
        const std::uint8_t trace_privilege_input = static_cast<std::uint8_t>(
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_priv.U());
        const std::uint64_t trace_mstatus_input =
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_mstatus.U();
        const std::uint64_t trace_trap_cause_input =
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_trap_cause.U();
        const std::uint64_t trace_trap_tval_input =
            dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_trap_tval.U();
        const auto sample_trace_group = [](auto &valid, auto &iaddr,
                                           auto &ftq_offset, auto &itype,
                                           auto &iretire, auto &ilastsize) {
            return TraceGroupState{
                .valid = valid.B(),
                .iaddr = iaddr.U(),
                .ftq_offset = static_cast<std::uint8_t>(ftq_offset.U()),
                .itype = static_cast<std::uint8_t>(itype.U()),
                .iretire = static_cast<std::uint8_t>(iretire.U()),
                .ilastsize = ilastsize.B(),
            };
        };
        const std::array<TraceGroupState, kTraceGroups> trace_group_inputs{{
            sample_trace_group(
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_valid,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_iaddr,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_ftqOffset,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_itype,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_iretire,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_0_bits_ilastsize),
            sample_trace_group(
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_valid,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_iaddr,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_ftqOffset,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_itype,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_iretire,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_1_bits_ilastsize),
            sample_trace_group(
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_valid,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_iaddr,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_ftqOffset,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_itype,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_iretire,
                dut_.io_traceCoreInterfaceBypass_fromBackend_toEncoder_groups_2_bits_ilastsize),
        }};

        // Writeback valid is a combinational projection of the execution-unit
        // output fire.  Observe the pins before the clock edge; after Step()
        // they may already describe the following transaction.  LSQ dequeue
        // pulses are registered separately and are counted below instead.
        if (monitor) {
            unsigned lsq_enqueue_width = 0;
            for (unsigned lane = 0; lane < generated::kLsqEnqueueLanes;
                 ++lane) {
                const auto enqueue =
                    generated::sample_lsq_enqueue(dut_, lane);
                if (!enqueue.valid) {
                    continue;
                }
                ++lsq_enqueue_width;
                ++lsq_enqueue_lanes_observed_[lane];
                if ((enqueue.need_alloc != 1 && enqueue.need_alloc != 2) ||
                    enqueue.num_ls_elem == 0) {
                    std::ostringstream message;
                    message << "invalid accepted LSQ enqueue on lane " << lane
                            << " need_alloc="
                            << static_cast<unsigned>(enqueue.need_alloc)
                            << " num_ls_elem="
                            << static_cast<unsigned>(enqueue.num_ls_elem);
                    error_ = message.str();
                    continue;
                }
                if (enqueue.need_alloc == 1) {
                    lq_enqueued_observed_ += enqueue.num_ls_elem;
                } else {
                    sq_enqueued_observed_ += enqueue.num_ls_elem;
                }
            }
            if (lsq_enqueue_width != 0) {
                ++lsq_enqueue_widths_observed_.at(lsq_enqueue_width - 1);
            }
            ++l2_flush_checks_;
            if (dut_.io_outer_l2_flush_en.B() != l2_flush_enable_input) {
                error_ = "L2 flush enable did not pass through combinationally";
            }
            if (dut_.io_mem_to_ooo_topToBackendBypass_l2FlushDone.B() !=
                expected_l2_flush_done_) {
                error_ = "L2 flush completion violated one-cycle delay";
            }
            ++top_control_checks_;
            if (dut_.io_mem_to_ooo_topToBackendBypass_hartId.U() !=
                hart_id_input) {
                error_ = "backend hart ID did not pass through combinationally";
            }
            if (dut_.io_outer_power_down_en.B() != power_down_input) {
                error_ = "power-down enable did not pass through combinationally";
            }
            if (dut_.io_inner_reset_vector.U() != expected_reset_vector_) {
                error_ = "reset vector violated one-cycle delay";
            }
            if (dut_.io_outer_cpu_halt.B() != expected_cpu_halted_) {
                error_ = "CPU halt violated one-cycle delay";
            }
            if (dut_.io_outer_cpu_critical_error.B() !=
                expected_cpu_critical_error_) {
                error_ = "CPU critical error violated one-cycle delay";
            }
            ++top_bridge_checks_;
            if (dut_.io_outer_msi_ack.B() != msi_ack_input) {
                error_ = "MSI acknowledgement did not pass through combinationally";
            }
            if (dut_.io_resetInFrontendBypass_toL2Top.B() !=
                frontend_reset_input) {
                error_ = "frontend reset bypass did not pass through combinationally";
            }
            if (dut_.io_outer_beu_errors_icache_ecc_error_valid.B() !=
                    expected_beu_valid_ ||
                dut_.io_outer_beu_errors_icache_ecc_error_bits.U() !=
                    expected_beu_address_) {
                error_ = "I-cache BEU metadata violated one-cycle delay";
            }
            if (dut_.io_mem_to_ooo_topToBackendBypass_msiInfo_valid.B() !=
                    expected_msi_info_valid_ ||
                (expected_msi_info_valid_ &&
                 dut_.io_mem_to_ooo_topToBackendBypass_msiInfo_bits.U() !=
                     expected_msi_info_)) {
                error_ = "MSI information violated valid-gated one-cycle delay";
            }
            if (dut_.io_mem_to_ooo_topToBackendBypass_clintTime_valid.B() !=
                    expected_clint_time_valid_ ||
                (expected_clint_time_valid_ &&
                 dut_.io_mem_to_ooo_topToBackendBypass_clintTime_bits.U() !=
                     expected_clint_time_)) {
                error_ = "CLINT time violated valid-gated one-cycle delay";
            }
            const ExternalInterruptState external_interrupt_output{
                .msip = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip.B(),
                .mtip = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip.B(),
                .meip = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip.B(),
                .seip = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip.B(),
                .debug = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug.B(),
                .nmi_31 = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31.B(),
                .nmi_43 = dut_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43.B(),
            };
            if (external_interrupt_output != expected_external_interrupt_) {
                error_ = "external interrupts violated one-cycle mapping";
            }
            if (generated::sample_hc_perf_event_output(
                    dut_, generated::kHcPerfEventFirstOutputLane) != 0) {
                error_ = "constant hardware-counter event lane was nonzero";
            }
            for (unsigned lane = generated::kHcPerfEventFirstSharedLane;
                 lane <= generated::kHcPerfEventLastSharedLane; ++lane) {
                if (generated::sample_hc_perf_event_output(dut_, lane) !=
                    expected_hc_perf_events_[lane]) {
                    error_ = "hardware-counter perf event violated one-cycle delay";
                }
            }
            const L2PrefetchControl l2_prefetch_output{
                .master_enable =
                    dut_.io_outer_l2PfCtrl_l2_pf_master_en.B(),
                .receive_enable =
                    dut_.io_outer_l2PfCtrl_l2_pf_recv_en.B(),
                .pbop_enable = dut_.io_outer_l2PfCtrl_l2_pbop_en.B(),
                .vbop_enable = dut_.io_outer_l2PfCtrl_l2_vbop_en.B(),
                .tp_enable = dut_.io_outer_l2PfCtrl_l2_tp_en.B(),
                .delay_latency = static_cast<std::uint16_t>(
                    dut_.io_outer_l2PfCtrl_l2_pf_delay_latency.U()),
            };
            if (l2_prefetch_output != expected_l2_prefetch_output_) {
                error_ = "L2 prefetch control violated two-cycle delay";
            }
            ++trace_bridge_checks_;
            if (dut_.io_traceCoreInterfaceBypass_fromBackend_fromEncoder_enable.B() !=
                    expected_trace_encoder_enable_ ||
                dut_.io_traceCoreInterfaceBypass_fromBackend_fromEncoder_stall.B() !=
                    expected_trace_encoder_stall_) {
                error_ = "trace encoder feedback violated one-cycle delay";
            }
            if (dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_mstatus.U() !=
                expected_trace_mstatus_) {
                error_ = "trace mstatus violated one-cycle delay";
            }
            if (trace_privilege_initialized_ &&
                dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_priv.U() !=
                    expected_trace_privilege_) {
                error_ = "trace privilege violated group-0 valid hold contract";
            }
            if (trace_trap_initialized_ &&
                (dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_trap_cause.U() !=
                     expected_trace_trap_cause_ ||
                 dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_trap_tval.U() !=
                     expected_trace_trap_tval_)) {
                error_ = "trace trap metadata violated trap-only hold contract";
            }
            const auto sample_trace_output = [](auto &valid, auto &iaddr,
                                                auto &itype, auto &iretire,
                                                auto &ilastsize) {
                return TraceGroupState{
                    .valid = valid.B(),
                    .iaddr = iaddr.U(),
                    .itype = static_cast<std::uint8_t>(itype.U()),
                    .iretire = static_cast<std::uint8_t>(iretire.U()),
                    .ilastsize = ilastsize.B(),
                };
            };
            const std::array<TraceGroupState, kTraceGroups> trace_group_outputs{{
                sample_trace_output(
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_valid,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_iaddr,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_itype,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_iretire,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_0_bits_ilastsize),
                sample_trace_output(
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_valid,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_iaddr,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_itype,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_iretire,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_1_bits_ilastsize),
                sample_trace_output(
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_valid,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_iaddr,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_itype,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_iretire,
                    dut_.io_traceCoreInterfaceBypass_toL2Top_toEncoder_groups_2_bits_ilastsize),
            }};
            for (unsigned group = 0; group < kTraceGroups; ++group) {
                const auto &actual = trace_group_outputs[group];
                const auto &expected = expected_trace_groups_[group];
                if (actual.valid != expected.valid ||
                    actual.itype != expected.itype ||
                    actual.iretire != expected.iretire) {
                    error_ = "trace group unconditional fields violated one-cycle delay";
                }
                if (trace_group_payload_initialized_[group] &&
                    (actual.iaddr != expected.iaddr ||
                     actual.ilastsize != expected.ilastsize)) {
                    error_ = "trace group payload violated valid hold contract";
                }
            }
            const bool top_down_l2_output =
                dut_.io_topDownInfo_toBackend_l2TopMiss_l2Miss.B();
            const bool top_down_l3_output =
                dut_.io_topDownInfo_toBackend_l2TopMiss_l3Miss.B();
            ++top_down_stats_.delay_checks;
            if (top_down_l2_output != expected_top_down_l2_miss_ ||
                top_down_l3_output != expected_top_down_l3_miss_) {
                std::ostringstream message;
                message << "top-down L2/L3 miss output violated one-cycle delay"
                        << " expected=" << expected_top_down_l2_miss_ << ','
                        << expected_top_down_l3_miss_ << " observed="
                        << top_down_l2_output << ',' << top_down_l3_output;
                error_ = message.str();
            }
            top_down_stats_.replay_allocate_cycles +=
                dut_.io_topDownInfo_toBackend_replayAllocate.B();
            top_down_stats_.sq_full_cycles +=
                dut_.io_topDownInfo_toBackend_sqFull.B();
            top_down_stats_.sb_full_cycles +=
                dut_.io_topDownInfo_toBackend_sbFull.B();
            top_down_stats_.l1_miss_cycles +=
                dut_.io_topDownInfo_toBackend_l1Miss.B();
            top_down_stats_.l2_miss_cycles += top_down_l2_output;
            top_down_stats_.l3_miss_cycles += top_down_l3_output;
            if (dut_.io_dcacheError_ecc_error_valid.B()) {
                ++bus_error_stats_.dcache_reports;
                bus_error_stats_.last_dcache_address =
                    dut_.io_dcacheError_ecc_error_bits.U();
            }
            if (dut_.io_uncacheError_ecc_error_valid.B()) {
                ++bus_error_stats_.uncache_reports;
                bus_error_stats_.last_uncache_address =
                    dut_.io_uncacheError_ecc_error_bits.U();
            }
            const auto memory_violation =
                generated::sample_memory_violation(dut_);
            if (memory_violation.valid) {
                ++memory_violation_stats_.count;
                memory_violation_stats_.last = memory_violation;
                memory_violation_stats_.last_cycle = cycle();
            }
            for (unsigned lane = 0; lane < kScalarLoadLanes; ++lane) {
                const auto ifetch_prefetch =
                    generated::sample_ifetch_prefetch(dut_, lane);
                if (ifetch_prefetch.valid) {
                    ++ifetch_prefetch_stats_.requests[lane];
                    ifetch_prefetch_stats_.last_vaddr[lane] =
                        ifetch_prefetch.vaddr;
                    ifetch_prefetch_stats_.last_cycle[lane] = cycle();
                }
                const auto wakeup =
                    generated::sample_scalar_load_wakeup(dut_, lane);
                if (wakeup.valid) {
                    ++scalar_load_feedback_stats_.wakeups[lane];
                    scalar_load_feedback_stats_.last_wakeup[lane] = {
                        .valid = true,
                        .rf_wen = wakeup.rf_wen,
                        .fp_wen = wakeup.fp_wen,
                        .pdest = wakeup.pdest,
                        .cycle = cycle(),
                    };
                }
                if (generated::sample_scalar_load_cancel(dut_, lane)) {
                    ++scalar_load_feedback_stats_.ld2_cancels[lane];
                    scalar_load_feedback_stats_.last_cancel_cycle[lane] = cycle();
                }
                scoreboard_.observe(
                    lane, generated::sample_scalar_load_writeback(dut_, lane));
            }
            const auto hardware_prefetch =
                generated::sample_hardware_prefetch_outputs(dut_);
            if (hardware_prefetch.l2_valid) {
                ++hardware_prefetch_stats_.l2_requests;
                ++hardware_prefetch_stats_.l2_source_counts[
                    hardware_prefetch.l2_source];
                hardware_prefetch_stats_.l2_addresses_by_source[
                    hardware_prefetch.l2_source].push_back(
                        hardware_prefetch.l2_addr);
                hardware_prefetch_stats_.last_l2_addr_by_source[
                    hardware_prefetch.l2_source] = hardware_prefetch.l2_addr;
                hardware_prefetch_stats_.last_l2_cycle_by_source[
                    hardware_prefetch.l2_source] = cycle();
                hardware_prefetch_stats_.last_l2_addr =
                    hardware_prefetch.l2_addr;
                hardware_prefetch_stats_.last_l2_source =
                    hardware_prefetch.l2_source;
                hardware_prefetch_stats_.last_l2_cycle = cycle();
            }
            if (hardware_prefetch.l3_valid) {
                ++hardware_prefetch_stats_.l3_requests;
                hardware_prefetch_stats_.last_l3_addr =
                    hardware_prefetch.l3_addr;
                hardware_prefetch_stats_.last_l3_cycle = cycle();
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
        expected_top_down_l2_miss_ = top_down_l2_input;
        expected_top_down_l3_miss_ = top_down_l3_input;
        expected_l2_flush_done_ = l2_flush_done_input;
        expected_reset_vector_ = reset_vector_input;
        expected_cpu_halted_ = cpu_halted_input;
        expected_cpu_critical_error_ = cpu_critical_error_input;
        expected_beu_valid_ = beu_valid_input;
        expected_beu_address_ = beu_address_input;
        expected_msi_info_valid_ = msi_info_valid_input;
        if (msi_info_valid_input) {
            expected_msi_info_ = msi_info_input;
        }
        expected_clint_time_valid_ = clint_time_valid_input;
        if (clint_time_valid_input) {
            expected_clint_time_ = clint_time_input;
        }
        expected_external_interrupt_ = external_interrupt_input;
        expected_hc_perf_events_ = hc_perf_event_inputs;
        expected_l2_prefetch_output_ = l2_prefetch_delay_stage_;
        l2_prefetch_delay_stage_ = l2_prefetch_input;
        expected_trace_encoder_enable_ = trace_encoder_enable_input;
        expected_trace_encoder_stall_ = trace_encoder_stall_input;
        expected_trace_mstatus_ = trace_mstatus_input;
        for (unsigned group = 0; group < kTraceGroups; ++group) {
            const auto &input = trace_group_inputs[group];
            auto &expected = expected_trace_groups_[group];
            expected.valid = input.valid;
            expected.itype = input.itype;
            expected.iretire = input.iretire;
            if (input.valid) {
                expected.iaddr =
                    (input.iaddr + (std::uint64_t{input.ftq_offset} << 1)) &
                    ((std::uint64_t{1} << 50) - 1U);
                expected.ilastsize = input.ilastsize;
                trace_group_payload_initialized_[group] = true;
            }
        }
        if (trace_group_inputs[0].valid) {
            expected_trace_privilege_ = trace_privilege_input;
            trace_privilege_initialized_ = true;
            if (trace_group_inputs[0].itype == 1 ||
                trace_group_inputs[0].itype == 2) {
                expected_trace_trap_cause_ = trace_trap_cause_input;
                expected_trace_trap_tval_ = trace_trap_tval_input;
                trace_trap_initialized_ = true;
            }
        }
        memory_agent_.update_after_tick();
        ptw_agent_.update_after_tick();
        uncache_agent_.update_after_tick();
        lq_dequeued_ += dut_.io_mem_to_ooo_lqDeq.U();
        sq_dequeued_ += dut_.io_mem_to_ooo_sqDeq.U();
        if (dut_.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid.B()) {
            const bool hit =
                dut_.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit.B();
            ++store_tlb_feedbacks_;
            store_tlb_misses_ += hit ? 0 : 1;
            ++iq_slow_feedback_stats_.sta_valid[0];
            ++(hit ? iq_slow_feedback_stats_.sta_hits[0]
                   : iq_slow_feedback_stats_.sta_misses[0]);
            iq_slow_feedback_stats_.sta_samples.push_back(
                StoreSlowFeedbackSample{
                    .lane = 0,
                    .hit = hit,
                    .sq_flag = dut_.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag.B(),
                    .sq_value = static_cast<std::uint8_t>(
                        dut_.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value.U()),
                    .cycle = cycle(),
                });
        }
        if (dut_.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid.B()) {
            const bool hit =
                dut_.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit.B();
            ++store_tlb_feedbacks_;
            store_tlb_misses_ += hit ? 0 : 1;
            ++iq_slow_feedback_stats_.sta_valid[1];
            ++(hit ? iq_slow_feedback_stats_.sta_hits[1]
                   : iq_slow_feedback_stats_.sta_misses[1]);
            iq_slow_feedback_stats_.sta_samples.push_back(
                StoreSlowFeedbackSample{
                    .lane = 1,
                    .hit = hit,
                    .sq_flag = dut_.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag.B(),
                    .sq_value = static_cast<std::uint8_t>(
                        dut_.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value.U()),
                    .cycle = cycle(),
                });
        }
        if (dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid.B()) {
            const VectorStoreSlowFeedbackSample sample{
                .lane = 0,
                .hit = dut_.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit.B(),
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
                .cycle = cycle(),
            };
            ++iq_slow_feedback_stats_.vstu_valid[0];
            ++(sample.hit ? iq_slow_feedback_stats_.vstu_hits[0]
                          : iq_slow_feedback_stats_.vstu_misses[0]);
            iq_slow_feedback_stats_.vstu_samples.push_back(sample);
            if (!sample.hit) {
                vector_replay_requests_.push_back(VectorReplayRequest{
                    .lane = sample.lane,
                    .lq_flag = sample.lq_flag,
                    .lq_value = sample.lq_value,
                    .sq_flag = sample.sq_flag,
                    .sq_value = sample.sq_value,
                    .is_part_replay = sample.is_part_replay,
                    .replay_mask = sample.replay_mask,
                    .replay_mb_index = sample.replay_mb_index,
                });
                ++vector_replay_feedbacks_;
            }
        }
        if (dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid.B()) {
            const VectorStoreSlowFeedbackSample sample{
                .lane = 1,
                .hit = dut_.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit.B(),
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
                .cycle = cycle(),
            };
            ++iq_slow_feedback_stats_.vstu_valid[1];
            ++(sample.hit ? iq_slow_feedback_stats_.vstu_hits[1]
                          : iq_slow_feedback_stats_.vstu_misses[1]);
            iq_slow_feedback_stats_.vstu_samples.push_back(sample);
            if (!sample.hit) {
                vector_replay_requests_.push_back(VectorReplayRequest{
                    .lane = sample.lane,
                    .lq_flag = sample.lq_flag,
                    .lq_value = sample.lq_value,
                    .sq_flag = sample.sq_flag,
                    .sq_value = sample.sq_value,
                    .is_part_replay = sample.is_part_replay,
                    .replay_mask = sample.replay_mask,
                    .replay_mb_index = sample.replay_mb_index,
                });
                ++vector_replay_feedbacks_;
            }
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
        } else if (lq_enqueued_observed_ != lq_allocated_ ||
                   sq_enqueued_observed_ != sq_allocated_) {
            std::ostringstream message;
            message << "LSQ enqueue monitor disagrees with driver accounting"
                    << " lq_observed=" << lq_enqueued_observed_
                    << " lq_expected=" << lq_allocated_
                    << " sq_observed=" << sq_enqueued_observed_
                    << " sq_expected=" << sq_allocated_;
            error_ = message.str();
        }
        return error_.empty();
    }

    static std::uint64_t scalar_store_key(const StoreTransaction &transaction)
    {
        return transaction.rob |
            (std::uint64_t{transaction.rob_flag} << 8) |
            (std::uint64_t{transaction.sq} << 9) |
            (std::uint64_t{transaction.sq_flag} << 17);
    }

    static std::uint64_t vector_store_key(
        const VectorMemoryTransaction &transaction)
    {
        return transaction.rob |
            (std::uint64_t{transaction.rob_flag} << 8) |
            (std::uint64_t{transaction.sq} << 9) |
            (std::uint64_t{transaction.sq_flag} << 17);
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
    FrontendBridgeStats frontend_bridge_stats_;
    std::uint64_t frontend_reset_canceled_requests_ = 0;
    std::uint64_t frontend_reset_canceled_responses_ = 0;
    std::uint64_t frontend_reset_survivor_requests_ = 0;
    std::uint64_t frontend_reset_stall_checks_ = 0;
    ScalarLoadFeedbackStats scalar_load_feedback_stats_;
    IqSlowFeedbackStats iq_slow_feedback_stats_;
    MemoryViolationStats memory_violation_stats_;
    IfetchPrefetchStats ifetch_prefetch_stats_;
    HardwarePrefetchStats hardware_prefetch_stats_;
    BusErrorStats bus_error_stats_;
    TopDownStats top_down_stats_;
    bool expected_top_down_l2_miss_ = false;
    bool expected_top_down_l3_miss_ = false;
    bool expected_l2_flush_done_ = false;
    std::uint64_t l2_flush_checks_ = 0;
    std::uint64_t expected_reset_vector_ = 0;
    bool expected_cpu_halted_ = false;
    bool expected_cpu_critical_error_ = false;
    std::uint64_t top_control_checks_ = 0;
    bool expected_beu_valid_ = false;
    std::uint64_t expected_beu_address_ = 0;
    bool expected_msi_info_valid_ = false;
    std::uint16_t expected_msi_info_ = 0;
    bool expected_clint_time_valid_ = false;
    std::uint64_t expected_clint_time_ = 0;
    ExternalInterruptState expected_external_interrupt_{};
    std::array<
        std::uint8_t, generated::kHcPerfEventHighestInputLane + 1>
        expected_hc_perf_events_{};
    L2PrefetchControl l2_prefetch_delay_stage_{};
    L2PrefetchControl expected_l2_prefetch_output_{};
    std::uint64_t top_bridge_checks_ = 0;
    bool expected_trace_encoder_enable_ = false;
    bool expected_trace_encoder_stall_ = false;
    std::uint8_t expected_trace_privilege_ = 0;
    std::uint64_t expected_trace_mstatus_ = 0;
    std::uint64_t expected_trace_trap_cause_ = 0;
    std::uint64_t expected_trace_trap_tval_ = 0;
    std::array<TraceGroupState, kTraceGroups> expected_trace_groups_{};
    std::array<bool, kTraceGroups> trace_group_payload_initialized_{};
    bool trace_privilege_initialized_ = false;
    bool trace_trap_initialized_ = false;
    std::uint64_t trace_bridge_checks_ = 0;
    std::uint64_t dft_bridge_patterns_ = 0;
    std::uint64_t dft_bridge_digest_ = 0;
    std::uint64_t reset_functional_pulses_ = 0;
    std::uint64_t reset_dft_pulses_ = 0;
    std::uint64_t reset_scan_transitions_ = 0;
    std::uint64_t reset_async_assertions_ = 0;
    std::uint64_t ifetch_ptw_pending_ = 0;
    std::uint64_t lq_allocated_ = 0;
    std::uint64_t lq_enqueued_observed_ = 0;
    std::uint64_t lq_dequeued_ = 0;
    std::uint64_t lq_canceled_ = 0;
    std::uint64_t lq_redirect_canceled_observed_ = 0;
    std::uint64_t lq_canceled_unobserved_ = 0;
    std::uint64_t sq_allocated_ = 0;
    std::uint64_t sq_enqueued_observed_ = 0;
    std::uint64_t sq_dequeued_ = 0;
    std::uint64_t sq_canceled_ = 0;
    std::uint64_t sq_redirect_canceled_observed_ = 0;
    std::uint64_t sq_canceled_unobserved_ = 0;
    std::uint64_t redirect_cancellation_events_observed_ = 0;
    std::array<std::uint64_t, generated::kLsqEnqueueLanes>
        lsq_enqueue_widths_observed_{};
    std::array<std::uint64_t, generated::kLsqEnqueueLanes>
        lsq_enqueue_lanes_observed_{};
    std::unordered_map<std::uint64_t, std::uint64_t>
        scalar_store_sq_targets_;
    std::unordered_map<std::uint64_t, std::uint64_t>
        vector_store_sq_targets_;
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
