#ifndef DFI_DDR4_MODEL_H_
#define DFI_DDR4_MODEL_H_

#include "dfi/address_mapping.h"
#include "dfi/virtual_ram.h"

#include <array>
#include <cstddef>
#include <cstdint>

namespace dfi {

class ddr4_model {
public:
    using data_beat = std::array<uint32_t, 8>;
    using memory_read_callback = virtual_ram::read_callback;
    using memory_write_callback = virtual_ram::write_callback;

    ddr4_model();

    void reset();
    void set_channel(uint8_t channel);
    void set_frequency_point(uint8_t dfi_frequency);
    bool bind_memory_backend(uint64_t address_base,
                             uint64_t size_bytes,
                             void* context,
                             memory_read_callback read,
                             memory_write_callback write);
    bool memory_ready() const;

    void on_mode_register_write(uint8_t mr_index, uint16_t mr_value);
    void on_activate(uint8_t rank, uint8_t bank, uint32_t row);
    void on_precharge(uint8_t rank, uint8_t bank, bool precharge_all);
    void on_refresh(uint8_t rank);

    bool commit_write_at_row(uint8_t rank,
                             uint8_t bank,
                             uint32_t row,
                             uint32_t col,
                             const data_beat& data,
                             uint32_t byte_mask);
    bool query_read_at_row(uint8_t rank,
                           uint8_t bank,
                           uint32_t row,
                           uint32_t col,
                           data_beat& data) const;
    uint32_t open_row(uint8_t rank, uint8_t bank) const;

    uint64_t write_latency_cycles() const;
    data_beat fallback_read_data(uint64_t seed) const;

private:
    struct bank_state {
        bool row_open;
        uint32_t row;
        bank_state() : row_open(false), row(0U) {}
    };

    struct latency_decode {
        uint64_t cycles;
        bool valid;
    };

    static constexpr uint8_t kMaxRanks = 2U;
    static constexpr uint8_t kMaxBanks = 16U;

    static uint64_t write_latency_from_frequency_point(uint8_t dfi_frequency);
    static uint64_t clamp_latency(uint64_t value);
    static uint8_t extract_mr2_cwl_code(uint16_t mr2);
    static latency_decode decode_mr2_cwl(uint8_t cwl_code);
    uint32_t resolve_row(uint8_t rank, uint8_t bank) const;
    bool compose_byte_address(uint8_t rank,
                              uint8_t bank,
                              uint32_t row,
                              uint32_t col,
                              uint64_t& byte_address) const;

    uint8_t dfi_frequency_;
    uint8_t channel_;
    latency_decode cas_write_latency_;
    std::array<std::array<bank_state, kMaxBanks>, kMaxRanks> banks_;
    address_mapping_config address_mapping_;
    virtual_ram ram_;
};

}  // namespace dfi

#endif  // DFI_DDR4_MODEL_H_
