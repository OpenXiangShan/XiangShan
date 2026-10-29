#pragma once

#include "generated_interface_types.hpp"
#include "failure.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <optional>
#include <memory>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace memblock {

#include "environment/model.inc"
#include "environment/dcache_a_contract.inc"


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

    struct LoadMmioStats {
        std::array<std::uint64_t, kScalarLoadLanes> pulses{};
        std::array<std::uint8_t, kScalarLoadLanes> last_rob{};
        std::array<std::uint64_t, kScalarLoadLanes> last_cycle{};
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

    Environment(int argc, char **argv);

    ~Environment();

    SparseMemory &memory();

    // Constrained-random dirty-write tracking.  While enabled, commit/record
    // helpers for cacheable identity-mapped stores accumulate the 8-byte words
    // whose final architectural bytes must be observed by a DUT readback before
    // the run can pass.  Alias/non-cacheable effects are counted separately and
    // are not claimed by this oracle.
    void set_final_observation_tracking(bool enabled);
    std::vector<std::uint64_t> final_observation_words() const;
    std::vector<std::uint64_t> final_observation_nc_words() const;
    std::uint64_t final_observation_bytes() const;
    std::uint64_t final_observation_nc_bytes() const;
    bool verify_final_nc_effects();
    std::uint64_t final_observation_alias_skips() const;
    std::uint64_t final_observation_observed_words() const;
    void set_final_observation_observed_words(std::uint64_t words);
    void add_final_observation_observed_words(std::uint64_t words);
    void discard_final_observation_range(
        std::uint64_t address, unsigned bytes);

    std::uint64_t bus_expected_load(std::uint64_t address, LoadOp op) const;

    void expect_release_line(
        std::uint64_t base, const std::vector<unsigned char> &bytes);

    void clear_release_line_expectations();

    bool request_dcache_probe(
        std::uint64_t address, std::uint8_t cap, bool need_data,
        std::uint8_t expected_report,
        const std::vector<unsigned char> &expected_data = {});

    bool hold_dcache_c_until_probes(std::uint64_t target_requests);

    void configure_backpressure(
        std::uint64_t seed, bool enabled,
        ResponseLatencyProfile latency_profile = ResponseLatencyProfile::compact);

    void configure_backpressure(
        std::uint64_t seed, bool enabled,
        const ResponseLatencyProfiles &latency_profiles);

    void inject_next_dcache_response_error(
        bool denied, bool corrupt,
        DcacheCorruptBeat corrupt_beat = DcacheCorruptBeat::all);

    void inject_dcache_response_error_at(
        std::uint64_t address, bool denied, bool corrupt,
        DcacheCorruptBeat corrupt_beat = DcacheCorruptBeat::all);

    void inject_dcache_cmo_response_error_at(
        std::uint64_t address, bool denied, bool corrupt);

    void inject_ptw_response_error_after(
        unsigned clean_requests, bool denied, bool corrupt,
        PtwCorruptBeat corrupt_beat = PtwCorruptBeat::all);

    void inject_ptw_response_error_at(
        std::uint64_t address, bool denied, bool corrupt,
        PtwCorruptBeat corrupt_beat = PtwCorruptBeat::all);

    void inject_persistent_ptw_response_error_at(
        std::uint64_t address, bool denied, bool corrupt,
        PtwCorruptBeat corrupt_beat = PtwCorruptBeat::all);

    void clear_ptw_response_error();

    void force_next_dcache_response_delay(unsigned cycles);

    void force_next_cmo_response_delay(unsigned cycles);

    void force_next_dcache_interbeat_delay(unsigned cycles);

    void hold_dcache_response_at(std::uint64_t address);

    bool dcache_response_held() const;

    bool dcache_response_held_at(std::uint64_t address) const;

    std::size_t dcache_held_response_count() const;

    bool release_held_dcache_response();

    bool release_held_dcache_response_at(std::uint64_t address);

    bool schedule_held_dcache_response_release_at(
        std::uint64_t address, unsigned after_ticks = 1);

    bool begin_dcache_release_ready_window(
        const std::vector<std::uint64_t> &target_lines,
        unsigned stall_valid_cycles);

    bool end_dcache_release_ready_window();

    void inject_next_uncache_response_error(bool denied, bool corrupt);

    void force_next_ptw_response_delay(unsigned cycles);

    void force_next_uncache_response_delay(unsigned cycles);

    void configure_uncache_device(
        std::uint64_t base, std::uint64_t size, bool read_clear);

    const std::vector<UncacheDeviceAccess> &
    uncache_device_accesses() const;

    void configure_cache_error_enable(bool enable);

    void configure_ldld_violation_check(bool enable);

    bool configure_stride_prefetch(bool enable);

    bool configure_sms_pht_prefetch(bool output_enable);

    bool expect_l2_prefetch_control(bool master_enabled, bool receive_enabled);

    bool issue_l2_tlb_request(
        std::uint64_t vaddr, std::uint8_t cmd, bool kill, bool is_prefetch,
        bool no_translate, L2TlbResponse &response, unsigned timeout = 4096);

    bool issue_killed_l2_tlb_request(
        std::uint64_t vaddr, std::uint8_t cmd, bool is_prefetch,
        bool no_translate, unsigned timeout = 128);

    bool start_ifetch_ptw_request(
        std::uint64_t vpn, PtwTranslationMode mode, unsigned timeout = 16384);

    bool complete_ifetch_ptw_request(
        IFetchPtwResponse &response, unsigned response_stall_cycles = 4,
        unsigned timeout = 16384);

    bool issue_ifetch_ptw_request(
        std::uint64_t vpn, PtwTranslationMode mode,
        IFetchPtwResponse &response, unsigned response_stall_cycles = 4,
        unsigned timeout = 16384);

    bool confirm_ifetch_ptw_flushed(unsigned quiet_cycles = 1024);

    bool pulse_l2_hint(std::uint8_t source_id, bool is_keyword);

    bool configure_memory_trigger(const MemoryTriggerConfig &config);

    std::uint64_t cycle() const;

    std::uint64_t tilelink_requests() const;

    std::uint64_t dcache_last_request_address() const;

    const std::array<std::uint64_t, 7> &dcache_request_classes() const;

    std::string dcache_request_summary_since(
        std::uint64_t first_request, unsigned maximum_records = 8) const;

    const char *dcache_faulting_load_requests_error_since(
        std::uint64_t target_line, bool target_read_allowed,
        std::uint64_t first_request) const;

    std::uint64_t dcache_requests_covering_since(
        std::uint64_t address, std::uint64_t first_request) const;

    std::uint64_t dcache_store_ntot_line_count_since(
        std::uint64_t line, std::uint64_t first_request) const;

    std::uint64_t dcache_requests_from_source_since(
        std::uint8_t request_source, std::uint64_t first_request) const;

    std::uint64_t dcache_requests_from_source_covering_since(
        std::uint8_t request_source, std::uint64_t address,
        std::uint64_t first_request) const;

    void begin_dcache_release_observation();

    void end_dcache_release_observation();

    std::uint64_t observed_dcache_releases_covering(
        std::uint64_t address) const;

    std::uint64_t observed_dcache_release_data_covering(
        std::uint64_t address) const;

    std::uint64_t observed_dcache_stalled_releases_covering(
        std::uint64_t address, bool data_only = false) const;

    std::uint64_t observed_dcache_release_stall_cycles_covering(
        std::uint64_t address, bool data_only = false) const;

    std::uint64_t dcache_gets() const;

    std::uint64_t dcache_refills() const;

    std::uint64_t dcache_keyword_refills() const;

    std::uint64_t dcache_nonkeyword_refills() const;

    std::uint64_t dcache_acquire_perms() const;

    std::uint64_t dcache_cmo_requests(StoreOp operation) const;

    std::uint64_t dcache_last_cmo_address() const;

    std::uint8_t dcache_last_cmo_source() const;

    std::uint64_t tilelink_releases() const;

    std::uint64_t tilelink_release_data() const;

    std::uint64_t tilelink_release_data_verified() const;

    std::uint64_t tilelink_expected_release_data_verified() const;

    std::uint64_t dcache_alias_recalls() const;
    std::uint64_t dcache_alias_recall_responses() const;
    std::uint64_t dcache_probes() const;

    std::uint64_t dcache_probe_responses() const;

    std::uint64_t dcache_probe_data() const;

    std::uint64_t dcache_probe_sources() const;

    std::uint64_t dcache_probe_source_reuses() const;

    std::uint64_t dcache_probe_source_wraps() const;

    std::uint64_t dcache_max_probe_outstanding() const;

    std::uint64_t dcache_probe_stalls() const;

    std::uint64_t dcache_grant_acks() const;

    std::uint64_t dcache_grant_data_beats() const;

    std::uint64_t dcache_denied_d_beats() const;

    std::uint64_t dcache_corrupt_d_beats() const;

    std::uint64_t dcache_error_response_requests() const;

    std::uint64_t dcache_error_response_refills() const;

    std::uint64_t dcache_error_response_grant_acks() const;

    std::uint64_t dcache_last_error_response_address() const;

    bool dcache_last_error_response_keyword() const;

    std::uint64_t dcache_max_outstanding_requests() const;

    std::uint64_t dcache_outstanding_requests() const;

    std::uint64_t dcache_grant_ack_stalls() const;

    std::uint64_t dcache_c_payload_stability_checks() const;

    std::uint64_t dcache_release_target_payload_stability_checks() const;

    bool dcache_grants_drained() const;

    // Read-only physical 64-byte line's cacheable Acquire/Grant lifecycle,
    // including armed/held responses. Other lines do not block the query;
    // this does not establish general Get/Release or pipeline idle.
    bool dcache_line_grants_idle(std::uint64_t address) const;

    bool dcache_responses_idle() const;

    bool dcache_release_channel_idle() const;

    std::uint64_t ptw_requests() const;

    std::uint64_t ptw_error_response_requests() const;

    std::uint64_t ptw_denied_d_beats() const;

    std::uint64_t ptw_corrupt_d_beats() const;

    std::uint64_t ptw_last_error_response_address() const;

    std::uint8_t ptw_last_error_response_source() const;

    std::uint64_t ptw_requests_covering_since(
        std::uint64_t address, std::uint64_t first_request) const;

    std::uint64_t pending_ifetch_ptw_requests() const;

    std::uint64_t dcache_request_stalls() const;

    std::uint64_t dcache_response_delays() const;

    const ResponseLatencyStats &dcache_response_latency_stats() const;

    std::uint64_t ptw_request_stalls() const;

    std::uint64_t ptw_response_delays() const;

    std::uint64_t ptw_max_outstanding_requests() const;

    std::uint64_t ptw_outstanding_requests() const;

    const ResponseLatencyStats &ptw_response_latency_stats() const;

    std::uint64_t exception_vaddr();

    void select_store_exception_address(bool store);

    std::uint64_t exception_gpaddr();

    bool exception_is_for_vs_nonleaf_pte();

    std::uint64_t uncache_requests() const;

    std::uint64_t uncache_request_stalls() const;

    std::uint64_t uncache_response_delays() const;

    std::uint64_t uncache_max_outstanding_requests() const;

    std::uint64_t uncache_outstanding_requests() const;

    std::uint64_t uncache_error_response_requests() const;

    std::uint64_t uncache_denied_d_beats() const;

    std::uint64_t uncache_corrupt_d_beats() const;

    const ResponseLatencyStats &uncache_response_latency_stats() const;

    bool store_mmio_valid();

    std::uint8_t store_mmio_rob();

    std::uint64_t store_tlb_feedbacks() const;

    std::uint64_t store_tlb_misses() const;

    std::uint64_t lq_allocated() const;

    std::uint64_t lq_enqueued_observed() const;

    std::uint64_t lq_dequeued() const;

    std::uint64_t lq_canceled() const;

    std::uint64_t lq_redirect_canceled_observed() const;

    std::uint64_t lq_canceled_unobserved() const;

    std::uint64_t sq_allocated() const;

    std::uint64_t sq_enqueued_observed() const;

    std::uint64_t sq_dequeued() const;

    std::uint64_t sq_canceled() const;

    std::uint64_t sq_redirect_canceled_observed() const;

    std::uint64_t sq_canceled_unobserved() const;

    std::uint64_t redirect_cancellation_events_observed() const;

    const std::array<std::uint64_t, generated::kLsqEnqueueLanes> &
    lsq_enqueue_widths_observed() const;

    const std::array<std::uint64_t, generated::kLsqEnqueueLanes> &
    lsq_enqueue_lanes_observed() const;

    const std::array<std::uint64_t, 6> &lsq_enqueue_cadence() const;
    std::uint64_t writebacks() const;

    std::uint64_t prefetch_writebacks() const;

    std::uint64_t store_writebacks() const;

    std::uint64_t vector_load_writebacks() const;

    std::uint64_t vector_store_writebacks() const;

    std::uint64_t vector_fof_fix_writebacks() const;

    std::uint64_t vector_page_fault_writebacks() const;

    std::size_t pending_scalar_loads() const;

    bool scalar_load_pending(const LoadTransaction &transaction) const;
    bool scalar_store_pending(const StoreTransaction &transaction) const;
    bool scalar_store_address_pending(const StoreTransaction &transaction) const;
    bool prefetch_pending(const PrefetchTransaction &transaction) const;
    bool vector_memory_pending(const VectorMemoryTransaction &transaction) const;
    bool service_vector_replay(const VectorMemoryTransaction &transaction,
        bool &replayed, unsigned timeout = 256);
    bool set_rob_head_state(std::uint8_t value, bool flag, bool pending_store);


    bool cancel_scalar_load_for_reset(const LoadTransaction &transaction);
    bool cancel_scalar_load_after_redirect(const LoadTransaction &transaction);
    std::uint64_t scalar_load_redirect_canceled() const;

    std::uint64_t scalar_load_reset_canceled() const;
    std::uint64_t scalar_load_canceled_observed() const;
    bool cancel_scalar_store_for_reset(const StoreTransaction &transaction);
    std::uint64_t scalar_store_reset_canceled() const;

    std::size_t pending_prefetches() const;

    std::size_t pending_scalar_stores() const;

    std::size_t pending_vector_loads() const;

    std::size_t pending_vector_stores() const;

    std::uint64_t vector_replay_feedbacks() const;

    std::uint64_t pin_space_digest() const;

    const FrontendBridgeStats &frontend_bridge_stats() const;

    const ScalarLoadFeedbackStats &scalar_load_feedback_stats() const;

    const LoadMmioStats &load_mmio_stats() const;

    const BusErrorStats &bus_error_stats() const;

    const TopDownStats &top_down_stats() const;

    const IqSlowFeedbackStats &iq_slow_feedback_stats() const;

    const MemoryViolationStats &memory_violation_stats() const;

    const IfetchPrefetchStats &ifetch_prefetch_stats() const;

    const HardwarePrefetchStats &hardware_prefetch_stats() const;

    bool sbuffer_empty();

    void drive_top_down_misses(bool l2_miss, bool l3_miss);

    bool run_until_sbuffer_empty(unsigned timeout = 4096);

    bool exercise_frontend_bridges(
        unsigned transaction_count, std::uint64_t seed);

    bool check_frontend_bridge_reset_recovery();

    std::uint64_t frontend_reset_canceled_requests() const;

    std::uint64_t frontend_reset_canceled_responses() const;

    std::uint64_t frontend_reset_survivor_requests() const;

    std::uint64_t frontend_reset_stall_checks() const;

    bool check_pin_space();

    bool reset();

    bool check_reset_backend_contract();

    std::uint64_t reset_functional_pulses() const;

    std::uint64_t reset_dft_pulses() const;

    std::uint64_t reset_scan_transitions() const;

    std::uint64_t reset_async_assertions() const;

    bool configure_sv39(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x90000000ULL,
        bool writable = true,
        bool noncacheable = false);

    bool configure_sv39_nc(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x90000000ULL,
        bool writable = true);

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
        bool global = false);

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
        bool global = false);

    bool activate_sv39(
        std::uint64_t root_page_table = 0x91000000ULL,
        std::uint16_t asid = 0);

    bool activate_sv48(
        std::uint64_t root_page_table = 0x91000000ULL,
        std::uint16_t asid = 0);

    bool activate_bare(std::uint16_t asid = 0);

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
        bool global = false);

    bool map_sv39_2m(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false);

    bool map_sv39_1g(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false);

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
        bool global = false);

    bool map_sv48_2m(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false);

    bool map_sv48_1g(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false);

    bool map_sv48_512g(
        std::uint64_t virtual_address,
        std::uint64_t physical_address,
        std::uint64_t root_page_table = 0x91000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool user = false,
        bool noncacheable = false);

    bool map_sv48x4_4k(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool global = false);

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
        bool global = false);

    bool map_sv48x4_2m(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false);

    bool map_sv48x4_1g(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false);

    bool map_sv48x4_512g(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false);

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
        bool io = false);

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
        bool io = false);

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
        bool io = false);

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
        bool io = false);

    bool map_sv39x4_4k(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false,
        bool global = false);

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
        bool global = false);

    bool map_sv39x4_2m(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false);

    bool map_sv39x4_1g(
        std::uint64_t guest_physical_address,
        std::uint64_t host_physical_address,
        std::uint64_t root_page_table = 0x95000000ULL,
        bool readable = true,
        bool writable = true,
        bool executable = false);

    bool activate_two_stage(
        std::uint64_t vs_root_page_table,
        std::uint64_t g_root_page_table,
        std::uint16_t asid = 0,
        std::uint16_t vmid = 0);

    bool activate_two_stage_modes(
        ReferencePageMode vs_mode,
        ReferencePageMode g_mode,
        std::uint64_t vs_root_page_table,
        std::uint64_t g_root_page_table,
        std::uint16_t asid = 0,
        std::uint16_t vmid = 0);

    bool update_stage_one_context(
        ReferencePageMode mode,
        std::uint64_t root_page_table,
        std::uint16_t asid);

    bool update_vs_context(
        ReferencePageMode mode,
        std::uint64_t root_page_table,
        std::uint16_t asid);

    bool update_two_stage_context(
        ReferencePageMode vs_mode,
        ReferencePageMode g_mode,
        std::uint64_t vs_root_page_table,
        std::uint64_t g_root_page_table,
        std::uint16_t asid,
        std::uint16_t vmid);

    bool update_g_context(
        ReferencePageMode mode,
        std::uint64_t root_page_table,
        std::uint16_t vmid);

    bool set_translation_permissions(
        ReferencePrivilegeMode data_privilege,
        bool mxr = false,
        bool sum = false,
        bool vmxr = false,
        bool vsum = false);

    bool set_instruction_privilege(ReferencePrivilegeMode privilege);

    bool set_debug_mode(bool enabled);

    bool set_hypervisor_access_permissions(
        ReferencePrivilegeMode spvp,
        bool mxr = false,
        bool vmxr = false,
        bool vsum = false,
        ReferencePrivilegeMode current_privilege =
            ReferencePrivilegeMode::supervisor);

    bool configure_pmp(
        const std::vector<std::uint64_t> &encoded_addresses,
        const std::vector<std::uint8_t> &config_bytes);

    bool set_page_based_memory_types(
        bool machine_enabled,
        bool hypervisor_enabled);

    bool set_uncache_write_outstanding(bool enabled);

    bool pulse_sbuffer_flush();

    void drive_l2_flush(bool enable, bool done);

    bool outer_l2_flush_enabled();

    bool backend_l2_flush_done();

    std::uint64_t l2_flush_checks() const;

    void drive_top_controls(
        std::uint8_t hart_id, std::uint64_t reset_vector,
        bool power_down, bool cpu_halted, bool cpu_critical_error);

    std::uint8_t backend_hart_id();

    std::uint64_t inner_reset_vector();

    bool outer_power_down_enabled();

    bool outer_cpu_halted();

    bool outer_cpu_critical_error();

    std::uint64_t top_control_checks() const;

    void drive_top_bridge_stimulus(const TopBridgeStimulus &stimulus);

    std::uint64_t top_bridge_checks() const;

    void drive_trace_bridge_stimulus(const TraceBridgeStimulus &stimulus);

    std::uint64_t trace_bridge_checks() const;

    bool check_dft_bridge_space();

    std::uint64_t dft_bridge_patterns() const;

    std::uint64_t dft_bridge_digest() const;

    bool set_sbuffer_timeout(std::uint32_t cycles);

    bool set_mbmc(
        bool bitmap_enabled, bool confidential_mode,
        std::uint64_t bitmap_base);

    bool pulse_mbmc_bitmap_clear();

    bool set_pointer_masking(const PointerMaskingConfig &config);

    bool issue_sfence(
        std::uint64_t address = 0,
        std::uint16_t id = 0,
        bool all_virtual_addresses = true,
        bool all_contexts = true,
        bool hypervisor_virtual = false,
        bool hypervisor_guest = false,
        bool flush_pipe = true);

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
        bool flush_pipe = true);

    bool enable_misaligned_accesses(bool load = true, bool store = true);

    bool set_rob_head(std::uint8_t value, bool flag = false);

    bool pulse_pending_load(std::uint8_t value, bool flag = false);

    bool wait_for_mmio_request(
        std::uint8_t value, bool flag = false, unsigned timeout = 2048);

    bool pulse_pending_store(std::uint8_t value, bool flag = false);

    bool pulse_store_commit_keep_pending(
        std::uint8_t value, bool flag = false);

    bool wait_for_mmio_store_request(
        std::uint8_t value, bool flag = false, unsigned timeout = 2048);

    bool set_wfi(bool enabled);

    bool wfi_safe();

    bool require_wfi_unsafe(unsigned cycles);

    bool run_until_wfi_safe(unsigned timeout = 4096);

    bool enqueue_load(const LoadTransaction &transaction);

    bool enqueue_load_batch(
        const std::vector<LoadTransaction> &transactions,
        const std::vector<unsigned> &dispatch_lanes);

    bool issue_load(const LoadTransaction &transaction, unsigned timeout = 32);

    bool issue_load_batch(
        const std::vector<LoadTransaction> &transactions, unsigned timeout = 64,
        bool require_same_cycle = false);

    void expect_load(const LoadTransaction &transaction);

    void expect_load_data(const LoadTransaction &transaction, std::uint64_t data);

    void expect_prefetch(const PrefetchTransaction &transaction);

    bool enqueue_prefetch(const PrefetchTransaction &transaction);

    bool enqueue_prefetch_batch(
        const std::vector<PrefetchTransaction> &transactions,
        const std::vector<unsigned> &dispatch_lanes);

    bool issue_prefetch(
        const PrefetchTransaction &transaction, unsigned timeout = 32);

    bool issue_prefetch_batch_same_cycle(
        const std::vector<PrefetchTransaction> &transactions,
        unsigned timeout = 64);

    void expect_store(const StoreTransaction &transaction);

    // Canonical typed dispatch checks both queue cursors, then drives the
    // transaction's immutable opposite-queue snapshot. Explicit-pointer
    // overloads remain available for scenario-owned legacy contracts.
    bool enqueue_store(const StoreTransaction &transaction);
    bool enqueue_store(const StoreTransaction &transaction, std::uint8_t lq_value,
        bool lq_flag = false);

    bool enqueue_store_pressure(
        const StoreTransaction &transaction, std::uint8_t lq_value,
        bool lq_flag = false);

    void expect_vector(const VectorMemoryTransaction &transaction);

    void expect_vector_data(
        const VectorMemoryTransaction &transaction,
        const std::array<unsigned char, 16> &data);

    bool enqueue_vector(const VectorMemoryTransaction &transaction);

    bool issue_vector(
        const VectorMemoryTransaction &transaction, unsigned timeout = 64);

    bool issue_vector_batch_same_cycle(
        const std::vector<VectorMemoryTransaction> &transactions,
        unsigned timeout = 64);

    bool issue_load_vector_pair(
        const LoadTransaction &load,
        const VectorMemoryTransaction &vector,
        unsigned timeout = 64);

    bool issue_store_address(const StoreTransaction &transaction, unsigned timeout = 32);

    bool issue_store_address_batch(
        const std::vector<StoreTransaction> &transactions,
        unsigned timeout = 64);

    bool issue_atomic(const AtomicTransaction &transaction, unsigned timeout = 256);

    bool warm_store_translation(
        const StoreTransaction &transaction, unsigned timeout = 512);

    bool issue_store_address_until_tlb_hit(
        const StoreTransaction &transaction, unsigned timeout = 4096);

    bool run_until_store_complete_with_replay(
        const StoreTransaction &transaction, unsigned timeout = 4096,
        bool hold_pending_store = false);

    bool run_until_store_tlb_misses(
        std::uint64_t target, unsigned timeout = 512);

    bool run_until_ptw_requests(std::uint64_t target, unsigned timeout = 512);

    bool run_until_dcache_requests(
        std::uint64_t target, unsigned timeout = 4096);

    bool run_until_dcache_idle(unsigned timeout = 4096);
    bool checkpoint_dcache_alias_reference(std::uint64_t base, std::uint64_t bytes);

    bool run_until_dcache_release_idle(unsigned timeout = 4096);

    bool run_until_ptw_idle(unsigned timeout = 4096);

    bool wait_for_cmo_store_request(
        const StoreTransaction &transaction, std::uint64_t target,
        unsigned timeout = 4096);

    bool run_until_ptw_request_covering(
        std::uint64_t address,
        std::uint64_t first_request,
        unsigned timeout = 4096,
        unsigned min_response_delay = 0);

    bool run_until_uncache_requests(
        std::uint64_t target, unsigned timeout = 4096);

    bool run_until_uncache_drained(
        std::uint64_t request_target, unsigned timeout = 32768);

    bool issue_store_data(const StoreTransaction &transaction, unsigned timeout = 32);

    bool run_until_store_complete(unsigned timeout);

    bool run_until_complete(unsigned timeout);

    bool run_until_load_complete(
        const LoadTransaction &transaction, unsigned timeout);

    bool run_until_load_writebacks(std::uint64_t target, unsigned timeout);

    bool run_until_vector_complete(unsigned timeout);

    bool run_until_vector_complete_with_replays(
        const VectorMemoryTransaction &transaction,
        unsigned timeout,
        bool pulse_pending_store_after_replay = false,
        bool hold_pending_store = false);

    bool run_until_vector_complete_with_replays(
        const std::vector<VectorMemoryTransaction> &transactions,
        unsigned timeout,
        bool pulse_pending_store_after_replay = false);

    bool run_until_all_complete(unsigned timeout);

    bool run_until_lq_retired(unsigned timeout = 32);

    bool account_lq_cancellation(unsigned count);

    bool account_sq_cancellation(unsigned count);

    bool run_until_queues_retired(unsigned timeout = 512);

    bool commit_store(const StoreTransaction &transaction, unsigned timeout = 512);

    bool commit_vector_store(
        const VectorMemoryTransaction &transaction, unsigned timeout = 512);

    void record_committed_store(const StoreTransaction &transaction);

    void record_atomic_result(
        std::uint64_t address, std::uint64_t value, bool cacheable = true,
        bool identity_mapped = true);

    void record_committed_vector_store(
        const VectorMemoryTransaction &transaction);

    bool commit_stores_through(
        const StoreTransaction &transaction, unsigned count);

    bool run_until_sq_dequeued(std::uint64_t target, unsigned timeout = 512);

    bool run_until_release_data(unsigned timeout = 4096);

    bool run_until_release_data_count(
        std::uint64_t target, unsigned timeout = 4096);

    bool run_until_expected_release_data_count(
        std::uint64_t target, unsigned timeout = 4096);

    bool run_until_probe_responses(
        std::uint64_t target, unsigned timeout = 4096);

    bool check_idle(unsigned cycles);

    bool run_cycles(unsigned cycles);

    bool redirect_after(std::uint8_t rob_value, bool rob_flag, bool flush_itself,
                        bool is_vls_exception = false);

    const std::vector<Failure> &failures() const;
    std::string failure_report() const;

    bool ok() const;

    const std::string &error() const;

    Environment(const Environment &) = delete;
    Environment &operator=(const Environment &) = delete;

private:
    class Impl;
    std::unique_ptr<Impl> impl_;
};

} // namespace memblock
