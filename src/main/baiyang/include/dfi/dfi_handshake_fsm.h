#ifndef DFI_DFI_HANDSHAKE_FSM_H_
#define DFI_DFI_HANDSHAKE_FSM_H_

#include <array>
#include <cstddef>
#include <cstdint>

#include "dfi/dfi_bus.h"
#include "dfi/ddr4_model.h"
#include "dfi/fixed_fifo.h"
#include "param.h"

namespace dfi {

class dfi_handshake_fsm {
public:
    using memory_read_callback = ddr4_model::memory_read_callback;
    using memory_write_callback = ddr4_model::memory_write_callback;

    dfi_handshake_fsm();

    void reset(dfi_output_drive& out);
    void set_channel(uint8_t channel);
    bool bind_memory_backend(uint64_t address_base,
                             uint64_t size_bytes,
                             void* context,
                             memory_read_callback read,
                             memory_write_callback write);
    bool memory_ready() const;
    void begin_core_cycle(uint32_t frequency,
                          uint32_t ctrlupd_req,
                          uint32_t lp_req,
                          dfi_output_drive& out);
    bool needs_write_data() const {
        return (wrdata_en_d1_ & SIM_DFI_DATA_EN_FULL_MASK) != 0U;
    }
    void end_core_cycle(const dfi_data_sample& in, dfi_output_drive& out);

    void on_active(uint32_t phase, uint32_t rank, uint32_t bank, uint32_t addr);
    void on_read(uint32_t phase, uint32_t rank, uint32_t bank, uint32_t col);
    void on_write(uint32_t phase, uint32_t rank, uint32_t bank, uint32_t col);
    void on_precharge(uint32_t phase, uint32_t rank, uint32_t bank, uint32_t all_banks);
    void on_refresh(uint32_t phase, uint32_t rank);
    void on_mrs(uint32_t phase, uint32_t rank, uint32_t mr_index, uint32_t value);
    void on_zq(uint32_t phase, uint32_t rank);

private:
    struct pulse_response_fsm {
        enum class state : uint8_t { kIdle = 0U, kDelay, kPulse };

        state current_state;
        bool last_req;
        uint64_t delay_count;
        uint64_t pulse_count;

        pulse_response_fsm()
            : current_state(state::kIdle), last_req(false), delay_count(0U), pulse_count(0U) {}
    };

    struct write_half_beat {
        std::array<uint32_t, 4> data;
        uint16_t byte_enable;
    };

    struct pending_read {
        uint8_t rank;
        uint8_t bank;
        uint32_t row;
        uint32_t col;
    };

    struct pending_read_return {
        uint64_t due_cycle;
        uint8_t rank;
        uint8_t bank;
        uint32_t row;
        uint32_t col;
    };

    struct pending_write {
        uint64_t due_cycle;
        uint8_t rank;
        uint8_t bank;
        uint32_t row;
        uint32_t col;
    };

    static uint64_t sanitize_pulse_cycles(uint64_t pulse_cycles);
    void step_pulse_response_fsm(bool req,
                                 uint64_t delay_cycles,
                                 uint64_t pulse_cycles,
                                 pulse_response_fsm& fsm,
                                 uint32_t& resp);

    void capture_write_data(const dfi_data_sample& in);
    void service_read_return(const dfi_data_sample& in, dfi_output_drive& out);
    void append_write_data_half(const std::array<uint32_t, 8>& data,
                                uint32_t dfi_byte_mask,
                                size_t half_word_offset);
    void commit_completed_write_beats();
    uint64_t core_cycle_count_;
    uint64_t fallback_seed_;
    pulse_response_fsm ctrlupd_ack_fsm_;
    pulse_response_fsm lp_ack_fsm_;
    ddr4_model ddr4_;
    static constexpr size_t kQueueCapacity = SIM_DFI_MAX_PENDING_DEPTH;
    fixed_fifo<write_half_beat, kQueueCapacity> write_half_fifo_;
    fixed_fifo<pending_read, kQueueCapacity> pending_reads_;
    fixed_fifo<pending_read_return, kQueueCapacity> pending_read_returns_;
    fixed_fifo<pending_write, kQueueCapacity> pending_writes_;
    uint32_t wrdata_en_d1_;
    uint32_t active_write_burst_beat_idx_;
    bool last_rddata_en_active_;
    uint32_t active_read_burst_beat_idx_;
};

}  // namespace dfi

#endif  // DFI_DFI_HANDSHAKE_FSM_H_
