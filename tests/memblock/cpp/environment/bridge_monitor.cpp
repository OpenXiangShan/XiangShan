#include "bridge_monitor.hpp"

namespace memblock {

void BridgeMonitor::capture_inputs()
{
    const bool top_down_l2_input =
        dut_.top_down.input.l2_miss.boolean();
    const bool top_down_l3_input =
        dut_.top_down.input.l3_miss.boolean();
    const bool l2_flush_enable_input =
        dut_.control.csr.flush_l2_enable.boolean();
    const bool l2_flush_done_input = dut_.control.l2_flush_done.boolean();
    const std::uint8_t hart_id_input =
        static_cast<std::uint8_t>(dut_.top.hart_id.value());
    const std::uint64_t reset_vector_input =
        dut_.top.output.reset_vector.value();
    const bool power_down_input =
        dut_.control.csr.power_down_enable.boolean();
    const bool cpu_halted_input =
        dut_.top.backend_input.cpu_halted.boolean();
    const bool cpu_critical_error_input =
        dut_.top.backend_input.cpu_critical_error.boolean();
    const bool msi_ack_input =
        dut_.top.backend_input.msi_ack.boolean();
    const bool frontend_reset_input =
        dut_.reset.frontend.from_frontend.boolean();
    const bool beu_valid_input =
        dut_.errors.frontend_input.icache_ecc_error_valid.boolean();
    const std::uint64_t beu_address_input =
        dut_.errors.frontend_input.icache_ecc_error_bits.value();
    const bool msi_info_valid_input =
        dut_.top.input.msi_info_valid.boolean();
    const std::uint16_t msi_info_input = static_cast<std::uint16_t>(
        dut_.top.input.msi_info_bits.value());
    const bool clint_time_valid_input =
        dut_.top.input.clint_time_valid.boolean();
    const std::uint64_t clint_time_input =
        dut_.top.input.clint_time_bits.value();
    const ExternalInterruptState external_interrupt_input{
        .msip = dut_.interrupts.clint.lane_0.boolean(),
        .mtip = dut_.interrupts.clint.lane_1.boolean(),
        .meip = dut_.interrupts.plic.lane_0_0.boolean(),
        .seip = dut_.interrupts.plic.lane_1_0.boolean(),
        .debug = dut_.interrupts.debug.lane_0.boolean(),
        .nmi_31 = dut_.interrupts.nmi.lane_0.boolean() ||
                  dut_.interrupts.bus_error.lane_0.boolean(),
        .nmi_43 = dut_.interrupts.nmi.lane_1.boolean(),
    };
    std::array<
        std::uint8_t, generated::kHcPerfEventHighestInputLane + 1>
        hc_perf_event_inputs{};
    for (unsigned lane = generated::kHcPerfEventFirstInputLane;
         lane <= generated::kHcPerfEventHighestInputLane; ++lane) {
        hc_perf_event_inputs[lane] =
            dut_.sample_hc_perf_event_input(lane);
    }
    const L2PrefetchControl l2_prefetch_input{
        .master_enable =
            dut_.control.prefetch.l2_pf_enable.boolean(),
        .receive_enable =
            dut_.control.prefetch.l2_pf_recv_enable.boolean(),
        .pbop_enable =
            dut_.control.prefetch.l2_pf_pbop_enable.boolean(),
        .vbop_enable =
            dut_.control.prefetch.l2_pf_vbop_enable.boolean(),
        .tp_enable =
            dut_.control.prefetch.l2_pf_tp_enable.boolean(),
        .delay_latency = static_cast<std::uint16_t>(
            dut_.control.prefetch.l2_pf_delay_latency.value()),
    };
    const bool trace_encoder_enable_input =
        dut_.trace.top.from_encoder_enable.boolean();
    const bool trace_encoder_stall_input =
        dut_.trace.top.from_encoder_stall.boolean();
    const std::uint8_t trace_privilege_input = static_cast<std::uint8_t>(
        dut_.trace.backend.to_encoder_priv.value());
    const std::uint64_t trace_mstatus_input =
        dut_.trace.backend.to_encoder_mstatus.value();
    const std::uint64_t trace_trap_cause_input =
        dut_.trace.backend.to_encoder_trap_cause.value();
    const std::uint64_t trace_trap_tval_input =
        dut_.trace.backend.to_encoder_trap_tval.value();
    const auto sample_trace_group = [](auto &valid, auto &iaddr,
                                       auto &ftq_offset, auto &itype,
                                       auto &iretire, auto &ilastsize) {
        return TraceGroupState{
            .valid = valid.boolean(),
            .iaddr = iaddr.value(),
            .ftq_offset = static_cast<std::uint8_t>(ftq_offset.value()),
            .itype = static_cast<std::uint8_t>(itype.value()),
            .iretire = static_cast<std::uint8_t>(iretire.value()),
            .ilastsize = ilastsize.boolean(),
        };
    };
    const std::array<TraceGroupState, kTraceGroups> trace_group_inputs{{
        sample_trace_group(
            dut_.trace.backend.to_encoder_groups_0_valid,
            dut_.trace.backend.to_encoder_groups_0_iaddr,
            dut_.trace.backend.to_encoder_groups_0_ftq_offset,
            dut_.trace.backend.to_encoder_groups_0_itype,
            dut_.trace.backend.to_encoder_groups_0_iretire,
            dut_.trace.backend.to_encoder_groups_0_ilastsize),
        sample_trace_group(
            dut_.trace.backend.to_encoder_groups_1_valid,
            dut_.trace.backend.to_encoder_groups_1_iaddr,
            dut_.trace.backend.to_encoder_groups_1_ftq_offset,
            dut_.trace.backend.to_encoder_groups_1_itype,
            dut_.trace.backend.to_encoder_groups_1_iretire,
            dut_.trace.backend.to_encoder_groups_1_ilastsize),
        sample_trace_group(
            dut_.trace.backend.to_encoder_groups_2_valid,
            dut_.trace.backend.to_encoder_groups_2_iaddr,
            dut_.trace.backend.to_encoder_groups_2_ftq_offset,
            dut_.trace.backend.to_encoder_groups_2_itype,
            dut_.trace.backend.to_encoder_groups_2_iretire,
            dut_.trace.backend.to_encoder_groups_2_ilastsize),
    }};

    inputs_ = {
        top_down_l2_input,
        top_down_l3_input,
        l2_flush_enable_input,
        l2_flush_done_input,
        hart_id_input,
        reset_vector_input,
        power_down_input,
        cpu_halted_input,
        cpu_critical_error_input,
        msi_ack_input,
        frontend_reset_input,
        beu_valid_input,
        beu_address_input,
        msi_info_valid_input,
        msi_info_input,
        clint_time_valid_input,
        clint_time_input,
        external_interrupt_input,
        hc_perf_event_inputs,
        l2_prefetch_input,
        trace_encoder_enable_input,
        trace_encoder_stall_input,
        trace_privilege_input,
        trace_mstatus_input,
        trace_trap_cause_input,
        trace_trap_tval_input,
        trace_group_inputs,
    };
}

void BridgeMonitor::check_before_edge()
{
    ++l2_flush_checks_;
    if (dut_.top.output.l2_flush_en.boolean() != inputs_.l2_flush_enable_input) {
        error_.record("L2 flush enable did not pass through combinationally");
    }
    if (dut_.top.backend.l2_flush_done.boolean() !=
        expected_l2_flush_done_) {
        error_.record("L2 flush completion violated one-cycle delay");
    }
    ++top_control_checks_;
    if (dut_.top.backend.hart_id.value() !=
        inputs_.hart_id_input) {
        error_.record("backend hart ID did not pass through combinationally");
    }
    if (dut_.top.output.power_down_en.boolean() != inputs_.power_down_input) {
        error_.record("power-down enable did not pass through combinationally");
    }
    if (dut_.top.inner_reset_vector.value() != expected_reset_vector_) {
        error_.record("reset vector violated one-cycle delay");
    }
    if (dut_.top.output.cpu_halt.boolean() != expected_cpu_halted_) {
        error_.record("CPU halt violated one-cycle delay");
    }
    if (dut_.top.output.cpu_critical_error.boolean() !=
        expected_cpu_critical_error_) {
        error_.record("CPU critical error violated one-cycle delay");
    }
    ++top_bridge_checks_;
    if (dut_.top.output.msi_ack.boolean() != inputs_.msi_ack_input) {
        error_.record("MSI acknowledgement did not pass through combinationally");
    }
    if (dut_.reset.frontend.to_l2_top.boolean() !=
        inputs_.frontend_reset_input) {
        error_.record("frontend reset bypass did not pass through combinationally");
    }
    if (dut_.errors.frontend_output.icache_ecc_error_valid.boolean() !=
            expected_beu_valid_ ||
        dut_.errors.frontend_output.icache_ecc_error_bits.value() !=
            expected_beu_address_) {
        error_.record("I-cache BEU metadata violated one-cycle delay");
    }
    if (dut_.top.backend.msi_info_valid.boolean() !=
            expected_msi_info_valid_ ||
        (expected_msi_info_valid_ &&
         dut_.top.backend.msi_info_bits.value() !=
             expected_msi_info_)) {
        error_.record("MSI information violated valid-gated one-cycle delay");
    }
    if (dut_.top.backend.clint_time_valid.boolean() !=
            expected_clint_time_valid_ ||
        (expected_clint_time_valid_ &&
         dut_.top.backend.clint_time_bits.value() !=
             expected_clint_time_)) {
        error_.record("CLINT time violated valid-gated one-cycle delay");
    }
    const ExternalInterruptState external_interrupt_output{
        .msip = dut_.top.backend.external_interrupt_msip.boolean(),
        .mtip = dut_.top.backend.external_interrupt_mtip.boolean(),
        .meip = dut_.top.backend.external_interrupt_meip.boolean(),
        .seip = dut_.top.backend.external_interrupt_seip.boolean(),
        .debug = dut_.top.backend.external_interrupt_debug.boolean(),
        .nmi_31 = dut_.top.backend.external_interrupt_nmi_nmi_31.boolean(),
        .nmi_43 = dut_.top.backend.external_interrupt_nmi_nmi_43.boolean(),
    };
    if (external_interrupt_output != expected_external_interrupt_) {
        error_.record("external interrupts violated one-cycle mapping");
    }
    if (dut_.sample_hc_perf_event_output(generated::kHcPerfEventFirstOutputLane) != 0) {
        error_.record("constant hardware-counter event lane was nonzero");
    }
    for (unsigned lane = generated::kHcPerfEventFirstSharedLane;
         lane <= generated::kHcPerfEventLastSharedLane; ++lane) {
        if (dut_.sample_hc_perf_event_output(lane) !=
            expected_hc_perf_events_[lane]) {
            error_.record("hardware-counter perf event violated one-cycle delay");
        }
    }
    const L2PrefetchControl l2_prefetch_output{
        .master_enable =
            dut_.control.l2_prefetch_output.l2_pf_master_en.boolean(),
        .receive_enable =
            dut_.control.l2_prefetch_output.l2_pf_recv_en.boolean(),
        .pbop_enable = dut_.control.l2_prefetch_output.l2_pbop_en.boolean(),
        .vbop_enable = dut_.control.l2_prefetch_output.l2_vbop_en.boolean(),
        .tp_enable = dut_.control.l2_prefetch_output.l2_tp_en.boolean(),
        .delay_latency = static_cast<std::uint16_t>(
            dut_.control.l2_prefetch_output.l2_pf_delay_latency.value()),
    };
    if (l2_prefetch_output != expected_l2_prefetch_output_) {
        error_.record("L2 prefetch control violated two-cycle delay");
    }
    ++trace_bridge_checks_;
    if (dut_.trace.backend.from_encoder_enable.boolean() !=
            expected_trace_encoder_enable_ ||
        dut_.trace.backend.from_encoder_stall.boolean() !=
            expected_trace_encoder_stall_) {
        error_.record("trace encoder feedback violated one-cycle delay");
    }
    if (dut_.trace.top.to_encoder_mstatus.value() !=
        expected_trace_mstatus_) {
        error_.record("trace mstatus violated one-cycle delay");
    }
    if (trace_privilege_initialized_ &&
        dut_.trace.top.to_encoder_priv.value() !=
            expected_trace_privilege_) {
        error_.record("trace privilege violated group-0 valid hold contract");
    }
    if (trace_trap_initialized_ &&
        (dut_.trace.top.to_encoder_trap_cause.value() !=
             expected_trace_trap_cause_ ||
         dut_.trace.top.to_encoder_trap_tval.value() !=
             expected_trace_trap_tval_)) {
        error_.record("trace trap metadata violated trap-only hold contract");
    }
    const auto sample_trace_output = [](auto &valid, auto &iaddr,
                                        auto &itype, auto &iretire,
                                        auto &ilastsize) {
        return TraceGroupState{
            .valid = valid.boolean(),
            .iaddr = iaddr.value(),
            .itype = static_cast<std::uint8_t>(itype.value()),
            .iretire = static_cast<std::uint8_t>(iretire.value()),
            .ilastsize = ilastsize.boolean(),
        };
    };
    const std::array<TraceGroupState, kTraceGroups> trace_group_outputs{{
        sample_trace_output(
            dut_.trace.top.to_encoder_groups_0_valid,
            dut_.trace.top.to_encoder_groups_0_iaddr,
            dut_.trace.top.to_encoder_groups_0_itype,
            dut_.trace.top.to_encoder_groups_0_iretire,
            dut_.trace.top.to_encoder_groups_0_ilastsize),
        sample_trace_output(
            dut_.trace.top.to_encoder_groups_1_valid,
            dut_.trace.top.to_encoder_groups_1_iaddr,
            dut_.trace.top.to_encoder_groups_1_itype,
            dut_.trace.top.to_encoder_groups_1_iretire,
            dut_.trace.top.to_encoder_groups_1_ilastsize),
        sample_trace_output(
            dut_.trace.top.to_encoder_groups_2_valid,
            dut_.trace.top.to_encoder_groups_2_iaddr,
            dut_.trace.top.to_encoder_groups_2_itype,
            dut_.trace.top.to_encoder_groups_2_iretire,
            dut_.trace.top.to_encoder_groups_2_ilastsize),
    }};
    for (unsigned group = 0; group < kTraceGroups; ++group) {
        const auto &actual = trace_group_outputs[group];
        const auto &expected = expected_trace_groups_[group];
        if (actual.valid != expected.valid ||
            actual.itype != expected.itype ||
            actual.iretire != expected.iretire) {
            error_.record("trace group unconditional fields violated one-cycle delay");
        }
        if (trace_group_payload_initialized_[group] &&
            (actual.iaddr != expected.iaddr ||
             actual.ilastsize != expected.ilastsize)) {
            error_.record("trace group payload violated valid hold contract");
        }
    }
    const bool top_down_l2_output =
        dut_.top_down.output.l2_top_miss_l2_miss.boolean();
    const bool top_down_l3_output =
        dut_.top_down.output.l2_top_miss_l3_miss.boolean();
    ++top_down_stats_.delay_checks;
    if (top_down_l2_output != expected_top_down_l2_miss_ ||
        top_down_l3_output != expected_top_down_l3_miss_) {
        std::ostringstream message;
        message << "top-down L2/L3 miss output violated one-cycle delay"
                << " expected=" << expected_top_down_l2_miss_ << ','
                << expected_top_down_l3_miss_ << " observed="
                << top_down_l2_output << ',' << top_down_l3_output;
        error_.record(message.str());
    }
    top_down_stats_.replay_allocate_cycles +=
        dut_.top_down.output.replay_allocate.boolean();
    top_down_stats_.sq_full_cycles +=
        dut_.top_down.output.sq_full.boolean();
    top_down_stats_.sb_full_cycles +=
        dut_.top_down.output.sb_full.boolean();
    top_down_stats_.l1_miss_cycles +=
        dut_.top_down.output.l1_miss.boolean();
    top_down_stats_.l2_miss_cycles += top_down_l2_output;
    top_down_stats_.l3_miss_cycles += top_down_l3_output;
}

void BridgeMonitor::advance_after_edge()
{
    expected_top_down_l2_miss_ = inputs_.top_down_l2_input;
    expected_top_down_l3_miss_ = inputs_.top_down_l3_input;
    expected_l2_flush_done_ = inputs_.l2_flush_done_input;
    expected_reset_vector_ = inputs_.reset_vector_input;
    expected_cpu_halted_ = inputs_.cpu_halted_input;
    expected_cpu_critical_error_ = inputs_.cpu_critical_error_input;
    expected_beu_valid_ = inputs_.beu_valid_input;
    expected_beu_address_ = inputs_.beu_address_input;
    expected_msi_info_valid_ = inputs_.msi_info_valid_input;
    if (inputs_.msi_info_valid_input) {
        expected_msi_info_ = inputs_.msi_info_input;
    }
    expected_clint_time_valid_ = inputs_.clint_time_valid_input;
    if (inputs_.clint_time_valid_input) {
        expected_clint_time_ = inputs_.clint_time_input;
    }
    expected_external_interrupt_ = inputs_.external_interrupt_input;
    expected_hc_perf_events_ = inputs_.hc_perf_event_inputs;
    expected_l2_prefetch_output_ = l2_prefetch_delay_stage_;
    l2_prefetch_delay_stage_ = inputs_.l2_prefetch_input;
    expected_trace_encoder_enable_ = inputs_.trace_encoder_enable_input;
    expected_trace_encoder_stall_ = inputs_.trace_encoder_stall_input;
    expected_trace_mstatus_ = inputs_.trace_mstatus_input;
    for (unsigned group = 0; group < kTraceGroups; ++group) {
        const auto &input = inputs_.trace_group_inputs[group];
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
    if (inputs_.trace_group_inputs[0].valid) {
        expected_trace_privilege_ = inputs_.trace_privilege_input;
        trace_privilege_initialized_ = true;
        if (inputs_.trace_group_inputs[0].itype == 1 ||
            inputs_.trace_group_inputs[0].itype == 2) {
            expected_trace_trap_cause_ = inputs_.trace_trap_cause_input;
            expected_trace_trap_tval_ = inputs_.trace_trap_tval_input;
            trace_trap_initialized_ = true;
        }
    }
}

void BridgeMonitor::reset_observations()
{
    top_down_stats_ = {};
    expected_top_down_l2_miss_ = false;
    expected_top_down_l3_miss_ = false;
}
} // namespace memblock
