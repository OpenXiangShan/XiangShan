#include "dfi/dfi_handshake_fsm.h"

#include <algorithm>
#include <cstdlib>
#include <iostream>

namespace dfi {
namespace {

constexpr uint32_t kDfiDataBeatsPerCommand = 2U;

void require_queue_capacity(size_t size, const char* name, uint64_t cycle) {
    if (size < SIM_DFI_MAX_PENDING_DEPTH) {
        return;
    }
    std::cerr << "[DFI] " << name << " queue overflow at core cycle " << cycle
              << " (depth=" << size << ")" << std::endl;
    std::abort();
}

}  // namespace

dfi_handshake_fsm::dfi_handshake_fsm()
    : core_cycle_count_(0U),
      fallback_seed_(0U),
      ctrlupd_ack_fsm_(),
      lp_ack_fsm_(),
      ddr4_(),
      write_half_fifo_(),
      pending_reads_(),
      pending_read_returns_(),
      pending_writes_(),
      wrdata_en_d1_(0U),
      active_write_burst_beat_idx_(0U),
      last_rddata_en_active_(false),
      active_read_burst_beat_idx_(0U) {}

void dfi_handshake_fsm::reset(dfi_output_drive& out) {
    core_cycle_count_ = 0U;
    fallback_seed_ = 0U;
    ctrlupd_ack_fsm_ = pulse_response_fsm();
    lp_ack_fsm_ = pulse_response_fsm();
    ddr4_.reset();
    write_half_fifo_.clear();
    pending_reads_.clear();
    pending_read_returns_.clear();
    pending_writes_.clear();
    wrdata_en_d1_ = 0U;
    active_write_burst_beat_idx_ = 0U;
    last_rddata_en_active_ = false;
    active_read_burst_beat_idx_ = 0U;
    out.clear();
}

void dfi_handshake_fsm::set_channel(uint8_t channel) {
    ddr4_.set_channel(channel);
}

bool dfi_handshake_fsm::bind_memory_backend(uint64_t address_base,
                                            uint64_t size_bytes,
                                            void* context,
                                            memory_read_callback read,
                                            memory_write_callback write) {
    return ddr4_.bind_memory_backend(address_base, size_bytes, context, read, write);
}

bool dfi_handshake_fsm::memory_ready() const {
    return ddr4_.memory_ready();
}

void dfi_handshake_fsm::begin_core_cycle(uint32_t frequency,
                                         uint32_t ctrlupd_req,
                                         uint32_t lp_req,
                                         dfi_output_drive& out) {
    ++core_cycle_count_;
    ddr4_.set_frequency_point(static_cast<uint8_t>(frequency & 0x1FU));
    step_pulse_response_fsm(ctrlupd_req != 0U,
                            SIM_DFI_CTRLUPD_ACK_DELAY_CORE_CYCLES,
                            SIM_DFI_CTRLUPD_ACK_PULSE_CORE_CYCLES,
                            ctrlupd_ack_fsm_,
                            out.ctrlupd_ack);
    out.ctrlupd_ack2 = out.ctrlupd_ack;
    step_pulse_response_fsm(lp_req != 0U,
                            SIM_DFI_LP_ACK_DELAY_CORE_CYCLES,
                            SIM_DFI_LP_ACK_PULSE_CORE_CYCLES,
                            lp_ack_fsm_,
                            out.lp_ack);
}

void dfi_handshake_fsm::end_core_cycle(const dfi_data_sample& in, dfi_output_drive& out) {
    capture_write_data(in);
    commit_completed_write_beats();
    service_read_return(in, out);
}

void dfi_handshake_fsm::on_active(uint32_t phase,
                                  uint32_t rank,
                                  uint32_t bank,
                                  uint32_t addr) {
    (void)phase;
    ddr4_.on_activate(static_cast<uint8_t>(rank), static_cast<uint8_t>(bank), addr);
}

void dfi_handshake_fsm::on_read(uint32_t phase,
                                uint32_t rank,
                                uint32_t bank,
                                uint32_t col) {
    (void)phase;
    const uint8_t rank_id = static_cast<uint8_t>(rank);
    const uint8_t bank_id = static_cast<uint8_t>(bank);
    // Snapshot the row at command time; a later PRE/ACT must not retarget this read.
    require_queue_capacity(pending_reads_.size(), "pending read", core_cycle_count_);
    pending_reads_.push_back(
        {rank_id, bank_id, ddr4_.open_row(rank_id, bank_id), col & 0xFFFU});
}

void dfi_handshake_fsm::on_write(uint32_t phase,
                                 uint32_t rank,
                                 uint32_t bank,
                                 uint32_t col) {
    (void)phase;
    const uint8_t rank_id = static_cast<uint8_t>(rank);
    const uint8_t bank_id = static_cast<uint8_t>(bank);
    const uint64_t latency =
        std::max<uint64_t>(ddr4_.write_latency_cycles(), SIM_DFI_TPHY_WRLAT_CORE_CYCLES);
    require_queue_capacity(pending_writes_.size(), "pending write", core_cycle_count_);
    pending_writes_.push_back({core_cycle_count_ + latency,
                               rank_id,
                               bank_id,
                               ddr4_.open_row(rank_id, bank_id),
                               col & 0xFFFU});
}

void dfi_handshake_fsm::on_precharge(uint32_t phase,
                                     uint32_t rank,
                                     uint32_t bank,
                                     uint32_t all_banks) {
    (void)phase;
    ddr4_.on_precharge(static_cast<uint8_t>(rank),
                       static_cast<uint8_t>(bank),
                       all_banks != 0U);
}

void dfi_handshake_fsm::on_refresh(uint32_t phase, uint32_t rank) {
    (void)phase;
    ddr4_.on_refresh(static_cast<uint8_t>(rank));
}

void dfi_handshake_fsm::on_mrs(uint32_t phase,
                               uint32_t rank,
                               uint32_t mr_index,
                               uint32_t value) {
    (void)phase;
    (void)rank;
    ddr4_.on_mode_register_write(static_cast<uint8_t>(mr_index & 0x7U),
                                 static_cast<uint16_t>(value & 0x3FFFU));
}

void dfi_handshake_fsm::on_zq(uint32_t phase, uint32_t rank) {
    (void)phase;
    (void)rank;
}

uint64_t dfi_handshake_fsm::sanitize_pulse_cycles(uint64_t pulse_cycles) {
    return pulse_cycles == 0U ? 1U : pulse_cycles;
}

void dfi_handshake_fsm::step_pulse_response_fsm(bool req,
                                                uint64_t delay_cycles,
                                                uint64_t pulse_cycles,
                                                pulse_response_fsm& fsm,
                                                uint32_t& resp) {
    resp = 0U;
    switch (fsm.current_state) {
        case pulse_response_fsm::state::kIdle:
            if (!fsm.last_req && req) {
                if (delay_cycles == 0U) {
                    fsm.current_state = pulse_response_fsm::state::kPulse;
                    resp = 1U;
                } else {
                    fsm.current_state = pulse_response_fsm::state::kDelay;
                }
                fsm.delay_count = 0U;
                fsm.pulse_count = 0U;
            }
            break;
        case pulse_response_fsm::state::kDelay:
            if (++fsm.delay_count >= delay_cycles) {
                fsm.current_state = pulse_response_fsm::state::kPulse;
                fsm.pulse_count = 0U;
                resp = 1U;
            }
            break;
        case pulse_response_fsm::state::kPulse:
            resp = 1U;
            if (++fsm.pulse_count >= sanitize_pulse_cycles(pulse_cycles)) {
                resp = 0U;
                fsm.current_state = pulse_response_fsm::state::kIdle;
                fsm.delay_count = 0U;
                fsm.pulse_count = 0U;
            }
            break;
    }
    fsm.last_req = req;
}

void dfi_handshake_fsm::capture_write_data(const dfi_data_sample& in) {
    const uint32_t wrdata_en = in.wrdata_en & SIM_DFI_DATA_EN_FULL_MASK;
    const uint32_t sample_enable = wrdata_en_d1_ & SIM_DFI_DATA_EN_FULL_MASK;

    // Baiyang presents wrdata one core cycle after wrdata_en. Sample the current
    // payload with the delayed enable and split the 256-bit DFI bus into halves.
    if ((sample_enable & 0x00FFU) != 0U) {
        append_write_data_half(in.wrdata, in.wrdata_mask, 0U);
    }
    if ((sample_enable & 0xFF00U) != 0U) {
        append_write_data_half(in.wrdata, in.wrdata_mask, 4U);
    }
    wrdata_en_d1_ = wrdata_en;
}

void dfi_handshake_fsm::append_write_data_half(const std::array<uint32_t, 8>& data,
                                               uint32_t dfi_byte_mask,
                                               size_t word_offset) {
    write_half_beat half{};
    for (size_t i = 0U; i < half.data.size(); ++i) {
        half.data[i] = data[word_offset + i];
    }
    const size_t byte_offset = word_offset * sizeof(uint32_t);
    for (size_t i = 0U; i < 16U; ++i) {
        half.byte_enable |= static_cast<uint16_t>(
            ((dfi_byte_mask >> (byte_offset + i)) & 0x1U) << i);
    }
    require_queue_capacity(write_half_fifo_.size(), "write data", core_cycle_count_);
    write_half_fifo_.push_back(half);
}

void dfi_handshake_fsm::commit_completed_write_beats() {
    while (!pending_writes_.empty() && write_half_fifo_.size() >= 2U &&
           core_cycle_count_ >= pending_writes_.front().due_cycle) {
        const pending_write write = pending_writes_.front();
        const write_half_beat first = write_half_fifo_.front();
        write_half_fifo_.pop_front();
        const write_half_beat second = write_half_fifo_.front();
        write_half_fifo_.pop_front();

        ddr4_model::data_beat data{};
        for (size_t i = 0U; i < first.data.size(); ++i) {
            data[i] = first.data[i];
            data[i + first.data.size()] = second.data[i];
        }
        const uint32_t mask = static_cast<uint32_t>(first.byte_enable) |
                              (static_cast<uint32_t>(second.byte_enable) << 16U);
        const uint32_t beat = active_write_burst_beat_idx_;
        const uint32_t col = (write.col + beat) & 0xFFFU;
        if (!ddr4_.commit_write_at_row(
                write.rank, write.bank, write.row, col, data, mask)) {
            std::cerr << "[DFI] external RAM write failed"
                      << " rank=" << static_cast<unsigned>(write.rank)
                      << " bank=" << static_cast<unsigned>(write.bank)
                      << " row=0x" << std::hex << write.row
                      << " col=0x" << col << std::dec << std::endl;
            std::abort();
        }

        if (++active_write_burst_beat_idx_ == kDfiDataBeatsPerCommand) {
            pending_writes_.pop_front();
            active_write_burst_beat_idx_ = 0U;
        }
    }
}

void dfi_handshake_fsm::service_read_return(const dfi_data_sample& in,
                                            dfi_output_drive& out) {
    out.rddata_valid = 0U;
    out.rddata_dbi = 0U;

    const bool rddata_en = (in.rddata_en & SIM_DFI_DATA_EN_FULL_MASK) != 0U;
    if (rddata_en && !pending_reads_.empty()) {
        const pending_read& read = pending_reads_.front();
        require_queue_capacity(
            pending_read_returns_.size(), "pending read return", core_cycle_count_);
        pending_read_returns_.push_back({core_cycle_count_ + SIM_DFI_PHY_RDLAT_CORE_CYCLES,
                                         read.rank,
                                         read.bank,
                                         read.row,
                                         (read.col + active_read_burst_beat_idx_) & 0xFFFU});
        if (++active_read_burst_beat_idx_ == kDfiDataBeatsPerCommand) {
            pending_reads_.pop_front();
            active_read_burst_beat_idx_ = 0U;
        }
    }

    // Drop an incomplete command when rddata_en deasserts before both beats.
    if (!rddata_en) {
        if (last_rddata_en_active_ && active_read_burst_beat_idx_ != 0U &&
            !pending_reads_.empty()) {
            pending_reads_.pop_front();
        }
        active_read_burst_beat_idx_ = 0U;
    }
    last_rddata_en_active_ = rddata_en;

    if (pending_read_returns_.empty() ||
        pending_read_returns_.front().due_cycle > core_cycle_count_) {
        return;
    }

    const pending_read_return read = pending_read_returns_.front();
    pending_read_returns_.pop_front();
    if (!ddr4_.query_read_at_row(read.rank, read.bank, read.row, read.col, out.rddata)) {
        // Keep failures visible to difftest instead of silently returning zeros.
        out.rddata = ddr4_.fallback_read_data(fallback_seed_++);
    }
    out.rddata_valid = SIM_DFI_RDDATA_VALID_MASK;
}

}  // namespace dfi
