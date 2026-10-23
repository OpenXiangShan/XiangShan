#pragma once
#include "memblock_env.hpp"
#include "dut_adapter.hpp"

namespace memblock {

// Owns expected bridge delay/hold state. Captures only top-level inputs;
// output observations never determine the next expected value.
class BridgeMonitor {
public:
    BridgeMonitor(DutAdapter &dut, FailureState &error) : dut_(dut), error_(error) {}
    void capture_inputs();
    void check_before_edge();
    void advance_after_edge();
    void reset_observations();
    const Environment::TopDownStats &top_down_stats() const { return top_down_stats_; }
    std::uint64_t l2_flush_checks() const { return l2_flush_checks_; }
    std::uint64_t top_control_checks() const { return top_control_checks_; }
    std::uint64_t top_bridge_checks() const { return top_bridge_checks_; }
    std::uint64_t trace_bridge_checks() const { return trace_bridge_checks_; }
private:
    struct Inputs {
        bool top_down_l2_input{};
        bool top_down_l3_input{};
        bool l2_flush_enable_input{};
        bool l2_flush_done_input{};
        std::uint8_t hart_id_input{};
        std::uint64_t reset_vector_input{};
        bool power_down_input{};
        bool cpu_halted_input{};
        bool cpu_critical_error_input{};
        bool msi_ack_input{};
        bool frontend_reset_input{};
        bool beu_valid_input{};
        std::uint64_t beu_address_input{};
        bool msi_info_valid_input{};
        std::uint16_t msi_info_input{};
        bool clint_time_valid_input{};
        std::uint64_t clint_time_input{};
        ExternalInterruptState external_interrupt_input{};
        std::array<
        std::uint8_t, generated::kHcPerfEventHighestInputLane + 1> hc_perf_event_inputs{};
        L2PrefetchControl l2_prefetch_input{};
        bool trace_encoder_enable_input{};
        bool trace_encoder_stall_input{};
        std::uint8_t trace_privilege_input{};
        std::uint64_t trace_mstatus_input{};
        std::uint64_t trace_trap_cause_input{};
        std::uint64_t trace_trap_tval_input{};
        std::array<TraceGroupState, kTraceGroups> trace_group_inputs{};
    } inputs_;
    DutAdapter &dut_;
    FailureState &error_;
    Environment::TopDownStats top_down_stats_;
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
};
} // namespace memblock
