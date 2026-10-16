#include "dfi/ddr4_model.h"

#include <algorithm>

namespace dfi {
namespace {

constexpr uint64_t kDefaultWl = 10U;
constexpr uint64_t kMinLatency = 4U;
constexpr uint64_t kMaxLatency = 64U;
constexpr uint64_t kDefaultVirtualRamSizeBytes = 16ULL << 30U;

}  // namespace

ddr4_model::ddr4_model()
    : dfi_frequency_(0U),
      channel_(0U),
      cas_write_latency_{9U, true},
      banks_(),
      address_mapping_(load_address_mapping_config(kDefaultVirtualRamSizeBytes)),
      ram_(address_mapping_.ram_size_bytes) {}

void ddr4_model::reset() {
    dfi_frequency_ = 0U;
    cas_write_latency_ = {9U, true};
    for (auto& rank : banks_) {
        for (auto& bank : rank) {
            bank = bank_state();
        }
    }
    // The simulator owns and preloads memory; an MC reset only clears protocol state.
}

void ddr4_model::set_channel(uint8_t channel) {
    channel_ = channel & 0x1U;
}

void ddr4_model::set_frequency_point(uint8_t dfi_frequency) {
    dfi_frequency_ = dfi_frequency;
}

bool ddr4_model::bind_memory_backend(uint64_t address_base,
                                     uint64_t size_bytes,
                                     void* context,
                                     memory_read_callback read,
                                     memory_write_callback write) {
    return ram_.bind_external(address_base, size_bytes, context, read, write);
}

bool ddr4_model::memory_ready() const {
    return ram_.ready();
}

void ddr4_model::on_mode_register_write(uint8_t mr_index, uint16_t mr_value) {
    if (mr_index == 2U) {
        cas_write_latency_ = decode_mr2_cwl(extract_mr2_cwl_code(mr_value));
    }
}

void ddr4_model::on_activate(uint8_t rank, uint8_t bank, uint32_t row) {
    if (rank < kMaxRanks && bank < kMaxBanks) {
        banks_[rank][bank].row_open = true;
        banks_[rank][bank].row = row & 0x3FFFFU;
    }
}

void ddr4_model::on_precharge(uint8_t rank, uint8_t bank, bool precharge_all) {
    if (rank >= kMaxRanks) {
        return;
    }
    if (precharge_all) {
        for (auto& state : banks_[rank]) {
            state = bank_state();
        }
    } else if (bank < kMaxBanks) {
        banks_[rank][bank] = bank_state();
    }
}

void ddr4_model::on_refresh(uint8_t rank) {
    on_precharge(rank, 0U, true);
}

bool ddr4_model::commit_write_at_row(uint8_t rank,
                                     uint8_t bank,
                                     uint32_t row,
                                     uint32_t col,
                                     const data_beat& data,
                                     uint32_t byte_mask) {
    uint64_t byte_address = 0U;
    if (rank >= kMaxRanks || bank >= kMaxBanks ||
        !compose_byte_address(rank, bank, row, col, byte_address)) {
        return false;
    }

    return ram_.write_beat(byte_address, data.data(), byte_mask);
}

bool ddr4_model::query_read_at_row(uint8_t rank,
                                   uint8_t bank,
                                   uint32_t row,
                                   uint32_t col,
                                   data_beat& data) const {
    uint64_t byte_address = 0U;
    return rank < kMaxRanks && bank < kMaxBanks &&
           compose_byte_address(rank, bank, row, col, byte_address) &&
           ram_.read_beat(byte_address, data.data());
}

uint32_t ddr4_model::open_row(uint8_t rank, uint8_t bank) const {
    return resolve_row(rank, bank);
}

uint64_t ddr4_model::write_latency_cycles() const {
    const uint64_t base = write_latency_from_frequency_point(dfi_frequency_);
    return clamp_latency(cas_write_latency_.valid ? cas_write_latency_.cycles : base);
}

ddr4_model::data_beat ddr4_model::fallback_read_data(uint64_t seed) const {
    data_beat data{};
    const uint32_t tag = static_cast<uint32_t>((seed ^ dfi_frequency_) & 0xFFFFU);
    for (size_t i = 0U; i < data.size(); ++i) {
        data[i] = 0xD4040000U | ((tag + static_cast<uint32_t>(i)) & 0xFFFFU);
    }
    return data;
}

uint64_t ddr4_model::write_latency_from_frequency_point(uint8_t dfi_frequency) {
    switch (dfi_frequency & 0x1FU) {
        case 0U: return 9U;
        case 1U:
        case 2U: return 10U;
        case 3U:
        case 4U: return 12U;
        case 5U: return 16U;
        default: return kDefaultWl;
    }
}

uint64_t ddr4_model::clamp_latency(uint64_t value) {
    return std::max(kMinLatency, std::min(value, kMaxLatency));
}

uint8_t ddr4_model::extract_mr2_cwl_code(uint16_t mr2) {
    return static_cast<uint8_t>((mr2 >> 3U) & 0x7U);
}

ddr4_model::latency_decode ddr4_model::decode_mr2_cwl(uint8_t code) {
    switch (code) {
        case 0U: return {9U, true};
        case 1U: return {10U, true};
        case 2U: return {11U, true};
        case 3U: return {12U, true};
        case 4U: return {14U, true};
        case 5U: return {16U, true};
        case 6U: return {18U, true};
        default: return {0U, false};
    }
}

uint32_t ddr4_model::resolve_row(uint8_t rank, uint8_t bank) const {
    if (rank >= kMaxRanks || bank >= kMaxBanks || !banks_[rank][bank].row_open) {
        return 0U;
    }
    return banks_[rank][bank].row;
}

bool ddr4_model::compose_byte_address(uint8_t rank,
                                      uint8_t bank,
                                      uint32_t row,
                                      uint32_t col,
                                      uint64_t& byte_address) const {
    if (!ram_.ready()) {
        return false;
    }
    const dfi_address address{channel_, rank, bank, row, col};
    byte_address = compose_mapped_byte_address(address_mapping_, address);
    return true;
}

}  // namespace dfi
