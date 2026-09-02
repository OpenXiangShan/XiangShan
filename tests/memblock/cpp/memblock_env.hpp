#pragma once

#include "generated_port_defaults.hpp"

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

constexpr std::uint32_t kExceptionLoadAddressMisaligned = 1U << 4;
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
};

enum class PrefetchOp : std::uint16_t {
    instruction = 0x8,
    read = 0x9,
    write = 0xa,
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
    bool rf_wen = true;
    bool fp_wen = false;
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
    void write_byte(std::uint64_t address, std::uint8_t value)
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
    std::unordered_map<std::uint64_t, std::uint8_t> bytes_;
};

struct ReferenceStageWalkResult {
    bool translated = false;
    std::uint64_t physical_address = 0;
    std::uint64_t faulting_pte_address = 0;
    unsigned fault_level = 0;
};

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

inline ReferenceStageWalkResult reference_sv39_walk(
    const SparseMemory &memory,
    std::uint64_t root_page_table,
    std::uint64_t input_address,
    bool sv39x4 = false)
{
    std::uint64_t table = root_page_table;
    for (int level = 2; level >= 0; --level) {
        const unsigned shift = 12 + 9 * static_cast<unsigned>(level);
        const std::uint64_t index_mask = sv39x4 && level == 2 ? 0x7ff : 0x1ff;
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
    std::uint64_t guest_virtual_address)
{
    std::uint64_t vs_table_gpa = vs_root_page_table;
    for (int level = 2; level >= 0; --level) {
        const unsigned shift = 12 + 9 * static_cast<unsigned>(level);
        const std::uint64_t index = (guest_virtual_address >> shift) & 0x1ff;
        const std::uint64_t pte_gpa = vs_table_gpa + index * 8;
        const auto pte_translation = reference_sv39_walk(
            memory, g_root_page_table, pte_gpa, true);
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
            const std::uint64_t guest_physical_address =
                reference_leaf_address(pte, guest_virtual_address, level);
            const auto final_translation = reference_sv39_walk(
                memory, g_root_page_table, guest_physical_address, true);
            if (!final_translation.translated) {
                return {
                    false, 0, true, false, guest_physical_address, false,
                };
            }
            return {true, final_translation.physical_address, false, false, 0, false};
        }
        if (level == 0) {
            return {false, 0, false, true, pte_gpa, false};
        }
        vs_table_gpa = reference_pte_ppn(pte) << 12;
    }
    return {};
}

class TileLinkMemoryAgent {
public:
    explicit TileLinkMemoryAgent(SparseMemory &memory) : memory_(memory) {}

    void configure_backpressure(std::uint64_t seed, bool enabled)
    {
        random_state_ = seed == 0 ? 1 : seed;
        random_backpressure_ = enabled;
        force_a_stall_ = enabled;
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
        dut.auto_inner_dcache_client_out_d_bits_denied.ImmSet(std::uint64_t{0});
        dut.auto_inner_dcache_client_out_d_bits_echo_isKeyword.ImmSet(beat.keyword);
        auto bytes = beat.data;
        dut.auto_inner_dcache_client_out_d_bits_data.ImmSetBytes(bytes);
        dut.auto_inner_dcache_client_out_d_bits_corrupt.ImmSet(std::uint64_t{0});
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
        }
        d_fire_ = !d_beats_.empty() &&
                  dut.auto_inner_dcache_client_out_d_valid.B() &&
                  dut.auto_inner_dcache_client_out_d_ready.B();
    }

    void update_after_tick()
    {
        if (d_fire_) {
            d_beats_.pop_front();
            d_presenting_ = false;
            d_gap_ = random_backpressure_ ? static_cast<unsigned>(next_random() % 4) : 0;
        }
        if (a_fire_ && captured_a_) {
            const bool was_empty = d_beats_.empty();
            respond(*captured_a_);
            if (was_empty && !d_beats_.empty() && random_backpressure_) {
                d_gap_ = 1 + static_cast<unsigned>(next_random() % 4);
            }
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
    std::uint64_t release_count() const { return release_count_; }
    std::uint64_t release_data_count() const { return release_data_count_; }

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
        const std::uint64_t transfer_bytes = std::uint64_t{1} << request.size;
        const std::uint64_t base = request.address & ~(transfer_bytes - 1);
        switch (request.opcode) {
        case 4: { // Get -> AccessAckData
            const std::uint64_t beat_base = request.address & ~(kBeatBytes - 1);
            d_beats_.push_back(DBeat{
                1, 0, request.size, request.source, 0, request.keyword,
                memory_.read_beat(beat_base, kBeatBytes),
            });
            break;
        }
        case 6: { // AcquireBlock -> GrantData
            const std::uint8_t cap = request.param == 0 ? 1 : 0;
            const std::size_t beats = static_cast<std::size_t>(
                transfer_bytes > kBeatBytes ? transfer_bytes / kBeatBytes : 1);
            for (std::size_t beat = 0; beat < beats; ++beat) {
                const std::size_t memory_beat = request.keyword ? beat ^ 1U : beat;
                d_beats_.push_back(DBeat{
                    5, cap, request.size, request.source, 1, request.keyword,
                    memory_.read_beat(base + memory_beat * kBeatBytes, kBeatBytes),
                });
            }
            break;
        }
        case 7: // AcquirePerm -> Grant
            d_beats_.push_back(DBeat{
                4, 0, request.size, request.source, 1, request.keyword,
                std::vector<unsigned char>(kBeatBytes, 0),
            });
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
            d_beats_.push_back(DBeat{
                6, 0, request.size, request.source, 0, request.keyword,
                std::vector<unsigned char>(kBeatBytes, 0),
            });
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
        for (std::size_t byte = 0; byte < kBeatBytes; ++byte) {
            memory_.write_byte(
                base + release_data_->received * kBeatBytes + byte,
                request.data.at(byte));
        }
        ++release_data_->received;
        if (release_data_->received == release_data_->beats) {
            d_beats_.push_back(DBeat{
                6, 0, request.size, request.source, 0, request.keyword,
                std::vector<unsigned char>(kBeatBytes, 0),
            });
            release_data_.reset();
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

    SparseMemory &memory_;
    std::deque<DBeat> d_beats_;
    std::optional<ARequest> captured_a_;
    std::optional<CRequest> captured_c_;
    std::optional<ReleaseDataState> release_data_;
    bool a_fire_ = false;
    bool c_fire_ = false;
    bool d_fire_ = false;
    std::uint64_t request_count_ = 0;
    std::uint64_t release_count_ = 0;
    std::uint64_t release_data_count_ = 0;
    std::uint64_t random_state_ = 1;
    unsigned d_gap_ = 0;
    bool random_backpressure_ = false;
    bool force_a_stall_ = false;
    bool d_presenting_ = false;
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
    std::string error_;
};

class PtwMemoryAgent {
public:
    explicit PtwMemoryAgent(SparseMemory &memory) : memory_(memory) {}

    void configure_backpressure(std::uint64_t seed, bool enabled)
    {
        random_state_ = seed == 0 ? 1 : seed;
        random_backpressure_ = enabled;
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
        }
        d_fire_ = !responses_.empty() &&
                  dut.auto_inner_ptw_to_l2_buffer_out_d_valid.B() &&
                  dut.auto_inner_ptw_to_l2_buffer_out_d_ready.B();
    }

    void update_after_tick()
    {
        if (d_fire_) {
            responses_.pop_front();
            d_presenting_ = false;
            d_gap_ = random_backpressure_
                ? static_cast<unsigned>(next_random() & 3U)
                : 0;
        }
        if (a_fire_ && request_) {
            const bool was_empty = responses_.empty();
            respond(*request_);
            if (was_empty && !responses_.empty() && random_backpressure_) {
                d_gap_ = 1 + static_cast<unsigned>(next_random() % 4);
            }
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
            responses_.push_back(Response{
                static_cast<std::uint8_t>(request.opcode == 4 ? 1 : 5),
                static_cast<std::uint8_t>(request.opcode == 4 ? 0 : 1),
                request.size,
                request.source,
                memory_.read_beat(base + beat * kBeatBytes, kBeatBytes),
            });
        }
    }

    std::uint64_t next_random()
    {
        random_state_ ^= random_state_ << 13;
        random_state_ ^= random_state_ >> 7;
        random_state_ ^= random_state_ << 17;
        return random_state_;
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
    bool force_a_stall_ = false;
    bool d_presenting_ = false;
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
    std::string error_;
};

class UncacheMemoryAgent {
public:
    explicit UncacheMemoryAgent(SparseMemory &memory) : memory_(memory) {}

    void configure_backpressure(std::uint64_t seed, bool enabled)
    {
        random_state_ = seed == 0 ? 1 : seed;
        random_backpressure_ = enabled;
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
        dut.auto_inner_buffers_out_d_bits_denied.ImmSet(std::uint64_t{0});
        dut.auto_inner_buffers_out_d_bits_data.ImmSet(response.data);
        dut.auto_inner_buffers_out_d_bits_corrupt.ImmSet(std::uint64_t{0});
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
        }
        d_fire_ = !responses_.empty() &&
                  dut.auto_inner_buffers_out_d_valid.B() &&
                  dut.auto_inner_buffers_out_d_ready.B();
    }

    void update_after_tick()
    {
        if (d_fire_) {
            responses_.pop_front();
            d_presenting_ = false;
        }
        if (a_fire_ && request_) {
            const bool was_empty = responses_.empty();
            respond(*request_);
            if (was_empty && !responses_.empty() && random_backpressure_) {
                d_gap_ = 1 + static_cast<unsigned>(next_random() % 4);
            }
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
    };

    void respond(const Request &request)
    {
        if (request.opcode == 4) {
            responses_.push_back(Response{
                1, request.size, request.source,
                memory_.expected_load(request.address, LoadOp::ld),
            });
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
        responses_.push_back(Response{0, request.size, request.source, 0});
    }

    std::uint64_t next_random()
    {
        random_state_ ^= random_state_ << 13;
        random_state_ ^= random_state_ >> 7;
        random_state_ ^= random_state_ << 17;
        return random_state_;
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
    bool force_a_stall_ = false;
    bool d_presenting_ = false;
    std::uint64_t request_stall_cycles_ = 0;
    std::uint64_t response_delay_cycles_ = 0;
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
        bool rf_wen;
        bool fp_wen;
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
                transaction.expected_exception_mask == 0 && transaction.rf_wen,
                transaction.expected_exception_mask == 0 && transaction.fp_wen,
            });
        if (!inserted && error_.empty()) {
            error_ = "duplicate outstanding scalar load ROB value";
        }
    }

    void expect_prefetch(const PrefetchTransaction &transaction)
    {
        const auto [_, inserted] = expected_.emplace(
            rob_identity(transaction.rob, transaction.rob_flag),
            Expected{0, 0, transaction.rob_flag, true, 0, false, false});
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
                writeback.rob_flag != it->second.rob_flag) {
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
            writeback.pdest != it->second.pdest ||
            writeback.rob_flag != it->second.rob_flag ||
            (it->second.exception_mask == 0 &&
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
                << " replay=" << std::dec << actual.replay;
        if (expected != nullptr) {
            message << " expected_pdest=" << static_cast<unsigned>(expected->pdest)
                    << " expected_rob_flag=" << expected->rob_flag
                    << " expected_exception=0x" << std::hex
                    << expected->exception_mask
                    << " expected_data=0x" << std::hex << expected->data;
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
        if (writeback.exception_mask != it->second.exception_mask ||
            writeback.rob_flag != it->second.rob_flag) {
            fail("mismatched store-address writeback", lane, writeback);
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
    };

    using Iterator = std::unordered_map<RobIdentity, Expected, RobIdentityHash>::iterator;

    void retire_if_complete(Iterator it)
    {
        if (it->second.address_seen && it->second.data_seen) {
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
                << " exception=0x" << std::hex << writeback.exception_mask;
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
        if (writeback.exception_mask != expected.exception_mask || writeback.replay ||
            writeback.flush_pipe || writeback.fu_op_type != expected.fu_op_type ||
            writeback.rob_flag != expected.rob_flag ||
            ((!exception_progress) &&
             (writeback.vsew != expected.eew || writeback.veew != expected.eew ||
              writeback.vl != expected.vl || writeback.vstart != 0 ||
              writeback.vuop_idx != 0))) {
            fail("mismatched vector memory metadata", lane, writeback, &expected);
            return;
        }
        if (!expected.store && expected.exception_mask == 0) {
            const std::vector<unsigned char> expected_data(
                expected.data.begin(), expected.data.end());
            if (!writeback.vec_wen || writeback.pdest != expected.pdest ||
                writeback.data != expected_data ||
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
                << " vec_wen=" << actual.vec_wen
                << " pdest=" << static_cast<unsigned>(actual.pdest)
                << " vl=" << static_cast<unsigned>(actual.vl)
                << " vstart=" << static_cast<unsigned>(actual.vstart)
                << " eew=" << static_cast<unsigned>(actual.veew);
        if (expected != nullptr) {
            message << " expected_op=0x" << std::hex << expected->fu_op_type
                    << " expected_exception=0x" << expected->exception_mask
                    << " expected_active=0x" << expected->active_elements
                    << " actual_data=";
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
        bool is_part_replay;
        std::uint16_t replay_mask;
        std::uint8_t replay_mb_index;
    };

public:
    Environment(int argc, char **argv)
        : dut_(argc, argv), memory_agent_(memory_), ptw_agent_(memory_),
          uncache_agent_(memory_)
    {
        dut_.InitClock(dut_.clock);
        generated::drive_idle_inputs(dut_);
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(std::uint64_t{3});
        dut_.io_ooo_to_mem_tlbCsr_priv_imode.ImmSet(std::uint64_t{3});
    }

    ~Environment() { dut_.Finish(); }

    SparseMemory &memory() { return memory_; }
    void configure_backpressure(std::uint64_t seed, bool enabled)
    {
        memory_agent_.configure_backpressure(seed, enabled);
        ptw_agent_.configure_backpressure(
            seed ^ 0x9e3779b97f4a7c15ULL, enabled);
        uncache_agent_.configure_backpressure(
            seed ^ 0x3c6ef372fe94f82aULL, enabled);
    }
    std::uint64_t cycle() const { return dut_.xclock.clk; }
    std::uint64_t tilelink_requests() const { return memory_agent_.request_count(); }
    std::uint64_t tilelink_releases() const { return memory_agent_.release_count(); }
    std::uint64_t tilelink_release_data() const
    {
        return memory_agent_.release_data_count();
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
    std::uint64_t ptw_request_stalls() const
    {
        return ptw_agent_.request_stall_cycles();
    }
    std::uint64_t ptw_response_delays() const
    {
        return ptw_agent_.response_delay_cycles();
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
        bool noncacheable = false)
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
        if ((virtual_address & page_mask) != (physical_address & page_mask) ||
            (root_page_table & page_mask) != 0) {
            error_ = "Sv39 4-KiB mapping requires aligned root and equal page offsets";
            return false;
        }
        if (writable && !readable) {
            error_ = "Sv39 does not permit W=1,R=0 leaf mappings";
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
            (noncacheable ? pte_pbmt_nc : 0);
        memory_.write_u64(
            l0_it->second + vpn0 * 8,
            (((physical_address & ~page_mask) >> 12) << 10) | flags);
        return true;
    }

    bool activate_sv39(
        std::uint64_t root_page_table = 0x91000000ULL,
        std::uint16_t asid = 0)
    {
        if (!write_distributed_csr(0x3b0, ~std::uint64_t{0}) ||
            !write_distributed_csr(0x3a0, 0x1f)) {
            return false;
        }
        dut_.io_ooo_to_mem_tlbCsr_priv_dmode.ImmSet(std::uint64_t{1});
        dut_.io_ooo_to_mem_tlbCsr_satp_mode.ImmSet(std::uint64_t{8});
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

    bool activate_two_stage(
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
        dut_.io_ooo_to_mem_tlbCsr_vsatp_mode.ImmSet(std::uint64_t{8});
        dut_.io_ooo_to_mem_tlbCsr_vsatp_asid.ImmSet(asid);
        dut_.io_ooo_to_mem_tlbCsr_vsatp_ppn.ImmSet(vs_root_page_table >> 12);
        dut_.io_ooo_to_mem_tlbCsr_hgatp_mode.ImmSet(std::uint64_t{8});
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

    bool pulse_pending_store(std::uint8_t value, bool flag = false)
    {
        dut_.io_ooo_to_mem_lsqio_pendingPtr_value.ImmSet(value);
        dut_.io_ooo_to_mem_lsqio_pendingPtr_flag.ImmSet(flag);
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{1});
        tick();
        dut_.io_ooo_to_mem_lsqio_pendingst.ImmSet(std::uint64_t{0});
        return check_components();
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

    bool issue_load(const LoadTransaction &transaction, unsigned timeout = 32)
    {
        generated::ScalarLoadIssue issue;
        issue.pc = 0x1000 + transaction.rob * 4;
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
        issue.fu_type = transaction.store ? kFuTypeVectorStore : kFuTypeVectorLoad;
        issue.fu_op_type = vector_fu_op_type(transaction);
        issue.vec_wen = !transaction.store;
        issue.vma = transaction.vma;
        issue.vta = transaction.vta;
        issue.vsew = transaction.eew;
        issue.vlmul = 0;
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
            if (vector_replay_requests_.empty()) {
                tick();
                if (!check_components()) {
                    return false;
                }
                continue;
            }
            const VectorReplayRequest request = vector_replay_requests_.front();
            vector_replay_requests_.pop_front();
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
                    << " vector_pending=" << vector_scoreboard_.pending();
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
        const std::uint64_t address = transaction.oracle_address.value_or(
            transaction.address);
        const unsigned bytes = 1U << static_cast<unsigned>(transaction.op);
        for (unsigned byte = 0; byte < bytes; ++byte) {
            memory_.write_byte(
                address + byte,
                static_cast<std::uint8_t>(transaction.data >> (8 * byte)));
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
                memory_.write_byte(
                    element_address + byte,
                    transaction.data[element * element_bytes + byte]);
            }
        }
        return true;
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
    std::unordered_map<std::uint64_t, std::uint64_t> next_gstage_page_table_;
    std::unordered_map<std::uint64_t, std::uint64_t> gstage_l1_tables_;
    std::unordered_map<std::uint64_t, std::uint64_t> gstage_l0_tables_;
    std::string error_;
};

} // namespace memblock
