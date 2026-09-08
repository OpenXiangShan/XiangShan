// This file is included inside memblock_env.hpp's memblock namespace.

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
constexpr unsigned kDcacheMissEntries = 16;
constexpr unsigned kDcacheProbeEntries = 8;
constexpr unsigned kDcacheProbeSourceBits = 6;
constexpr unsigned kDcacheProbeSourceCount = 1U << kDcacheProbeSourceBits;
static_assert(
    (kDcacheProbeSourceCount & (kDcacheProbeSourceCount - 1U)) == 0 &&
    kDcacheProbeSourceCount <= 256U);
constexpr unsigned kDcacheCmoSource = kDcacheMissEntries + 1;
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
    cbo_clean = 0x0c,
    cbo_flush = 0x0d,
    cbo_inval = 0x0e,
    hsvb = 0x10,
    hsvh = 0x11,
    hsvw = 0x12,
    hsvd = 0x13,
};

constexpr bool is_cbo_zero(StoreOp op)
{
    return op == StoreOp::cbo_zero;
}

constexpr bool is_cmo(StoreOp op)
{
    return op == StoreOp::cbo_clean || op == StoreOp::cbo_flush ||
        op == StoreOp::cbo_inval;
}

constexpr unsigned cmo_operation_index(StoreOp op)
{
    return is_cmo(op)
        ? static_cast<unsigned>(op) - static_cast<unsigned>(StoreOp::cbo_clean)
        : 3U;
}

constexpr unsigned scalar_store_bytes(StoreOp op)
{
    return is_cbo_zero(op)
        ? 64U
        : 1U << (static_cast<unsigned>(op) & 3U);
}

static_assert(scalar_store_bytes(StoreOp::sb) == 1U);
static_assert(scalar_store_bytes(StoreOp::sd) == 8U);
static_assert(scalar_store_bytes(StoreOp::hsvb) == 1U);
static_assert(scalar_store_bytes(StoreOp::hsvd) == 8U);
static_assert(scalar_store_bytes(StoreOp::cbo_zero) == 64U);
static_assert(is_cmo(StoreOp::cbo_clean));
static_assert(is_cmo(StoreOp::cbo_flush));
static_assert(is_cmo(StoreOp::cbo_inval));
static_assert(cmo_operation_index(StoreOp::cbo_clean) == 0U);
static_assert(cmo_operation_index(StoreOp::cbo_flush) == 1U);
static_assert(cmo_operation_index(StoreOp::cbo_inval) == 2U);

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
    std::optional<bool> expected_output_flush_pipe;
    std::optional<std::uint8_t> expected_trigger;
    // Store-address writeback exposes the memory-class debug bits.  Keep
    // these optional because existing callers may not model the translation
    // class of an address yet.
    std::optional<bool> expected_debug_is_mmio;
    std::optional<bool> expected_debug_is_ncio;
    bool allow_preliminary_success_writeback = false;
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

inline ReferencePtePermissions reference_pte_permissions(std::uint64_t pte)
{
    return {
        .readable = (pte & (std::uint64_t{1} << 1)) != 0,
        .writable = (pte & (std::uint64_t{1} << 2)) != 0,
        .executable = (pte & (std::uint64_t{1} << 3)) != 0,
        .user = (pte & (std::uint64_t{1} << 4)) != 0,
        .accessed = (pte & (std::uint64_t{1} << 6)) != 0,
        .dirty = (pte & (std::uint64_t{1} << 7)) != 0,
    };
}

struct ReferencePermissionWalkResult {
    ReferenceStageWalkResult walk{};
    std::optional<ReferencePtePermissions> leaf_permissions;
};

inline ReferencePermissionWalkResult reference_page_walk_permissions(
    const SparseMemory &memory,
    std::uint64_t root_page_table,
    std::uint64_t input_address,
    ReferencePageMode mode,
    bool x4 = false,
    bool pbmte = true)
{
    ReferencePermissionWalkResult result{
        .walk = reference_page_walk(
            memory, root_page_table, input_address, mode, x4, pbmte),
    };
    if (!result.walk.translated || mode == ReferencePageMode::bare) {
        return result;
    }

    const unsigned top_level = reference_page_levels(mode) - 1U;
    std::uint64_t table = root_page_table;
    for (int level = static_cast<int>(top_level); level >= 0; --level) {
        const unsigned shift = 12 + 9 * static_cast<unsigned>(level);
        const std::uint64_t index_mask =
            x4 && static_cast<unsigned>(level) == top_level ? 0x7ff : 0x1ff;
        const std::uint64_t index = (input_address >> shift) & index_mask;
        const std::uint64_t pte = memory.read_u64(table + index * 8);
        if (reference_pte_is_leaf(pte)) {
            result.leaf_permissions = reference_pte_permissions(pte);
            return result;
        }
        table = reference_pte_ppn(pte) << 12;
    }
    result.walk = {};
    return result;
}

enum class ReferenceMemoryAccess : std::uint8_t {
    load,
    store,
};

inline bool reference_memory_access_permitted(
    ReferenceMemoryAccess access,
    const ReferencePtePermissions &permissions,
    ReferencePrivilegeMode privilege,
    bool sum,
    bool mxr,
    bool guest_stage = false)
{
    return access == ReferenceMemoryAccess::load
        ? reference_load_permitted(
              permissions, privilege, sum, mxr, guest_stage)
        : reference_store_permitted(
              permissions, privilege, sum, guest_stage);
}

inline bool reference_implicit_gstage_read_permitted(
    const ReferencePtePermissions &permissions)
{
    // A VS page-table memory access is always an implicit read through
    // G-stage. MXR and the original instruction's store direction cannot
    // replace R or introduce W/D requirements.
    return permissions.user && permissions.accessed && permissions.readable;
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

inline ReferenceTwoStageWalkResult reference_two_stage_access(
    const SparseMemory &memory,
    std::uint64_t vs_root_page_table,
    std::uint64_t g_root_page_table,
    std::uint64_t guest_virtual_address,
    ReferenceMemoryAccess access,
    ReferencePageMode vs_mode = ReferencePageMode::sv39,
    ReferencePageMode g_mode = ReferencePageMode::sv39,
    ReferencePrivilegeMode privilege = ReferencePrivilegeMode::supervisor,
    bool vsum = false,
    bool mxr = false,
    bool vmxr = false,
    bool vs_pbmte = true,
    bool g_pbmte = true)
{
    if (vs_mode != ReferencePageMode::bare &&
        !reference_canonical_virtual_address(guest_virtual_address, vs_mode)) {
        return {false, 0, false, true, 0, false, false};
    }

    std::uint64_t guest_physical_address = guest_virtual_address;
    if (vs_mode != ReferencePageMode::bare) {
        const unsigned top_level = reference_page_levels(vs_mode) - 1U;
        std::uint64_t vs_table_gpa = vs_root_page_table;
        for (int level = static_cast<int>(top_level); level >= 0; --level) {
            const unsigned shift = 12 + 9 * static_cast<unsigned>(level);
            const std::uint64_t index =
                (guest_virtual_address >> shift) & 0x1ff;
            const std::uint64_t pte_gpa = vs_table_gpa + index * 8;
            const auto pte_translation = reference_page_walk_permissions(
                memory, g_root_page_table, pte_gpa, g_mode, true, g_pbmte);
            if (!pte_translation.walk.translated) {
                return {
                    false,
                    0,
                    !pte_translation.walk.access_fault,
                    false,
                    pte_gpa,
                    true,
                    pte_translation.walk.access_fault,
                };
            }
            if (pte_translation.leaf_permissions &&
                !reference_implicit_gstage_read_permitted(
                    *pte_translation.leaf_permissions)) {
                return {
                    false, 0, true, false, pte_gpa, true, false,
                };
            }

            const std::uint64_t pte = memory.read_u64(
                pte_translation.walk.physical_address);
            if (reference_pte_encoding_fault(
                    pte, static_cast<unsigned>(level), vs_pbmte)) {
                return {false, 0, false, true, pte_gpa, false, false};
            }
            const std::uint64_t generated_address = reference_leaf_address(
                pte, guest_virtual_address, static_cast<unsigned>(level));
            if (g_mode == ReferencePageMode::bare &&
                reference_pte_physical_address_fault(pte)) {
                return {
                    false, 0, false, false, generated_address, false, true,
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
                const auto permissions = reference_pte_permissions(pte);
                if (!reference_memory_access_permitted(
                        access, permissions, privilege, vsum,
                        mxr || vmxr)) {
                    return {false, 0, false, true, 0, false, false};
                }
                guest_physical_address = generated_address;
                break;
            }
            if (level == 0) {
                return {false, 0, false, true, pte_gpa, false, false};
            }
            vs_table_gpa = reference_pte_ppn(pte) << 12;
        }
    }

    if (g_mode == ReferencePageMode::bare) {
        return {true, guest_physical_address, false, false, 0, false, false};
    }
    const auto final_translation = reference_page_walk_permissions(
        memory, g_root_page_table, guest_physical_address, g_mode, true,
        g_pbmte);
    if (!final_translation.walk.translated) {
        return {
            false,
            0,
            !final_translation.walk.access_fault,
            false,
            guest_physical_address,
            false,
            final_translation.walk.access_fault,
        };
    }
    if (final_translation.leaf_permissions &&
        !reference_memory_access_permitted(
            access, *final_translation.leaf_permissions,
            ReferencePrivilegeMode::supervisor, false, mxr, true)) {
        return {
            false, 0, true, false, guest_physical_address, false, false,
        };
    }
    return {
        true,
        final_translation.walk.physical_address,
        false,
        false,
        0,
        false,
        false,
    };
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

enum class DcacheCorruptBeat {
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
