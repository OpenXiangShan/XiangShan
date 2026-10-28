#pragma once

// Private implementation boundary. Scenarios must include memblock_env.hpp.
#include "memblock_env.hpp"
#include "dut_adapter.hpp"
#include "bridge_monitor.hpp"
#include "page_table_builder.hpp"
#include "feedback_monitor.hpp"
#include "scoreboards.hpp"
#include "queue_accounting.hpp"
#include "debug_trace.hpp"

namespace memblock {
#include "dcache_agent.inc"
#include "ptw_agent.inc"
#include "uncache_agent.inc"

class Environment::Impl {
    static generated::VectorMemoryIssue make_vector_memory_issue(
        const VectorMemoryTransaction &transaction);

public:

    Impl(int argc, char **argv)
        : dut_(argc, argv), memory_(&bus_memory_),
          memory_agent_(bus_memory_, memory_),
          ptw_agent_(bus_memory_), uncache_agent_(bus_memory_){
        dut_.initialize_clock();
        dut_.drive_idle_inputs();
        dut_.translation.csr.priv_dmode.set(std::uint64_t{3});
        dut_.translation.csr.priv_imode.set(std::uint64_t{3});
#ifdef MEMBLOCK_MEM_DIRECT_DEBUG
        initialize_internal_debug();
#endif
    }

    ~Impl() {
        if (!error_.empty()) std::cerr << "MEMBLOCK_FAILURES " << error_.json() << '\n';
        dut_.finish();
    }

    SparseMemory &memory();
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

    // The L2-to-L1 DTLB request has no ready pin at the MemBlock boundary:
    // MemBlock ties the response consumer ready high internally.  Hold the
    // request valid for one cycle, then wait for the returned response while
    // the ordinary PTW agent services any page walk generated by the request.
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
    std::uint64_t writebacks() const;
    std::uint64_t prefetch_writebacks() const;
    std::uint64_t store_writebacks() const;
    std::uint64_t vector_load_writebacks() const;
    std::uint64_t vector_store_writebacks() const;
    std::uint64_t vector_fof_fix_writebacks() const;
    std::uint64_t vector_page_fault_writebacks() const;
    std::size_t pending_scalar_loads() const;
    bool scalar_load_pending(const LoadTransaction &transaction) const;
    bool cancel_scalar_load_for_reset(const LoadTransaction &transaction);
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
        bool global = false);

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
        bool io);

    bool activate_stage_one(
        ReferencePageMode mode,
        std::uint64_t root_page_table,
        std::uint16_t asid);

public:

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

    bool enqueue_store(const StoreTransaction &transaction, std::uint8_t lq_value);

    bool enqueue_store_pressure(
        const StoreTransaction &transaction, std::uint8_t lq_value);

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

    // Atomics enter the store-address port but are handled by AtomicsUnit,
    // rather than StoreQueue.  They therefore do not have an LSQ allocation
    // or StoreScoreboard entry; the scalar-load scoreboard observes their
    // old-value writeback on the atomic WB port.
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
        bool pulse_store_commit_after_replay = false,
        bool hold_pending_store = false);

    bool run_until_vector_complete_with_replays(
        const std::vector<VectorMemoryTransaction> &transactions,
        unsigned timeout,
        bool pulse_store_commit_after_replay = false);

    bool run_until_all_complete(unsigned timeout);

    bool run_until_lq_retired(unsigned timeout = 32);

    bool account_lq_cancellation(unsigned count);

    bool account_sq_cancellation(unsigned count);

    bool run_until_queues_retired(unsigned timeout = 512);

    bool commit_store(const StoreTransaction &transaction, unsigned timeout = 512);

    bool commit_vector_store(
        const VectorMemoryTransaction &transaction, unsigned timeout = 512);

    // Stress bursts advance pendingPtr to retire a whole ROB window at once.
    // Keep the architectural reference image synchronized with those DUT
    // commits without issuing a second commit pulse for every store.
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

    const std::vector<Failure> &failures() const { return error_.records(); }
    std::string failure_report() const { return error_.json(); }

    bool ok() const;
    const std::string &error() const;

private:
    void set_failure_cycle()
    {
        error_.set_cycle(cycle());
        memory_agent_.set_failure_cycle(cycle());
        ptw_agent_.set_failure_cycle(cycle());
        uncache_agent_.set_failure_cycle(cycle());
        scoreboard_.set_failure_cycle(cycle());
        store_scoreboard_.set_failure_cycle(cycle());
        vector_scoreboard_.set_failure_cycle(cycle());
    }

#ifdef MEMBLOCK_MEM_DIRECT_DEBUG
    static std::uint64_t debug_line_from_env()
    {
        const char *value = std::getenv("MEMBLOCK_MEM_DIRECT_DEBUG_LINE");
        return value == nullptr ? 0 : std::strtoull(value, nullptr, 0);
    }

    static bool debug_flag_from_env(const char *name)
    {
        const char *value = std::getenv(name);
        return value != nullptr && value[0] != '\0' && value[0] != '0';
    }

    static std::string debug_bytes(ReadPort *signal)
    {
        if (signal == nullptr) {
            return "<missing>";
        }
        const auto bytes = signal->bytes();
        std::ostringstream value;
        value << "0x" << std::hex;
        for (auto it = bytes.rbegin(); it != bytes.rend(); ++it) {
            value << std::setw(2) << std::setfill('0')
                  << static_cast<unsigned>(*it);
        }
        return value.str();
    }

    static std::string debug_byte_array(
        const std::array<ReadPort *, 16> &signals)
    {
        if (std::any_of(signals.begin(), signals.end(),
                        [](const ReadPort *signal) { return signal == nullptr; })) {
            return "<missing>";
        }
        std::ostringstream value;
        value << "0x" << std::hex;
        for (auto it = signals.rbegin(); it != signals.rend(); ++it) {
            value << std::setw(2) << std::setfill('0')
                  << static_cast<unsigned>((*it)->value() & 0xffU);
        }
        return value.str();
    }

    static std::uint64_t debug_u(ReadPort *signal)
    {
        return signal == nullptr ? 0 : signal->value();
    }

    static ReadPort *debug_signal(DutAdapter &dut, const std::string &name)
    {
        return dut.internal_signal(name);
    }

    void initialize_internal_debug()
    {
        debug_line_ = debug_line_from_env();
        debug_trace_all_a_ = debug_flag_from_env(
            "MEMBLOCK_MEM_DIRECT_TRACE_ALL_A");
        const char *path = std::getenv("MEMBLOCK_MEM_DIRECT_TRACE_FILE");
        if (path != nullptr && path[0] != '\0') {
            const auto mode = debug_flag_from_env(
                "MEMBLOCK_MEM_DIRECT_TRACE_APPEND")
                ? std::ios::app : std::ios::trunc;
            debug_trace_.open(path, std::ios::out | mode);
        }
        if (!debug_trace_.is_open()) {
            return;
        }
        for (unsigned lane = 0; lane < kScalarLoadLanes; ++lane) {
            const auto load_prefix = std::string(
                "MemBlock_top.MemBlock.inner_LoadUnit_") +
                std::to_string(lane) + '.';
            debug_s2_valid_[lane] = debug_signal(dut_, load_prefix + "s2_valid");
            debug_s2_paddr_[lane] = debug_signal(
                dut_, load_prefix + "s2_in_r_paddr");
            debug_s2_vaddr_[lane] = debug_signal(
                dut_, load_prefix + "s2_in_r_vaddr");
            debug_s2_rob_value_[lane] = debug_signal(
                dut_, load_prefix + "s2_in_r_uop_robIdx_value");
            debug_s2_rob_flag_[lane] = debug_signal(
                dut_, load_prefix + "s2_in_r_uop_robIdx_flag");
            debug_s2_from_d_[lane] = debug_signal(
                dut_, load_prefix + "s2_fwd_frm_d_chan");
            debug_s2_miss_[lane] = debug_signal(
                dut_, load_prefix + "s2_dcache_miss");
            debug_s2_full_fwd_[lane] = debug_signal(
                dut_, load_prefix + "s2_full_fwd");
            for (unsigned byte = 0; byte < 16; ++byte) {
                debug_s2_d_data_[lane][byte] = debug_signal(
                    dut_, load_prefix + "s2_fwd_data_frm_d_chan_" +
                        std::to_string(byte));
            }
            debug_s3_valid_[lane] = debug_signal(
                dut_, load_prefix + "s3_out_valid");
            debug_s3_paddr_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_paddr");
            debug_s3_vaddr_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_vaddr");
            debug_s3_rob_value_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_uop_robIdx_value");
            debug_s3_rob_flag_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_uop_robIdx_flag");
            debug_s3_handled_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_handledByMSHR");
            debug_s3_mshrid_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_rep_info_mshr_id");
            debug_s3_data_select_[lane] = debug_signal(
                dut_, load_prefix + "s3_data_select");
            debug_s3_input_data_[lane] = debug_signal(
                dut_, load_prefix + "s3_in_data");
            debug_s3_merged_data_[lane] = debug_signal(
                dut_, load_prefix + "s3_merged_data_frm_pipe");
            debug_s3_picked_data_[lane] = debug_signal(
                dut_, load_prefix + "s3_picked_data_frm_pipe_0");

            const auto mshr_prefix = std::string(
                "MemBlock_top.MemBlock.inner_dcache.dcache.missQueue.");
            const auto lane_suffix = lane == 0
                ? std::string{}
                : std::string("_") + std::to_string(lane);
            debug_mshr_forward_valid_[lane] = debug_signal(
                dut_, mshr_prefix + "io_forward_" + std::to_string(lane) +
                    "_forward_result_valid_REG");
            debug_mshr_forward_[lane] = debug_signal(
                dut_, mshr_prefix + "forward_mshr" + lane_suffix);
            for (unsigned byte = 0; byte < 16; ++byte) {
                const auto data_suffix = lane == 0
                    ? std::to_string(byte)
                    : std::to_string(lane) + '_' + std::to_string(byte);
                debug_mshr_forward_data_[lane][byte] = debug_signal(
                    dut_, mshr_prefix + "forwardData_" + data_suffix);
            }
            const std::string sq_prefix =
                "MemBlock_top.MemBlock.inner_lsq.storeQueue.";
            const auto sq_suffix = lane == 0
                ? std::string{}
                : std::string("_") + std::to_string(lane);
            debug_sq_forward_mask1_[lane] =
                debug_signal(dut_, sq_prefix + "forwardMask1" + sq_suffix);
            debug_sq_forward_mask2_[lane] =
                debug_signal(dut_, sq_prefix + "forwardMask2" + sq_suffix);
            debug_sq_need_forward_[lane] =
                debug_signal(dut_, sq_prefix + "needForward" + sq_suffix);
        }
        const std::string sq_prefix =
            "MemBlock_top.MemBlock.inner_lsq.storeQueue.";
        debug_sq_deq0_flag_ =
            debug_signal(dut_, sq_prefix + "deqPtrExt_0_flag");
        debug_sq_deq0_value_ =
            debug_signal(dut_, sq_prefix + "deqPtrExt_0_value");
        debug_sq_deq1_flag_ =
            debug_signal(dut_, sq_prefix + "deqPtrExt_1_flag");
        debug_sq_deq1_value_ =
            debug_signal(dut_, sq_prefix + "deqPtrExt_1_value");
        debug_sq_enq_flag_ =
            debug_signal(dut_, sq_prefix + "enqPtrExt_0_flag");
        debug_sq_enq_value_ =
            debug_signal(dut_, sq_prefix + "enqPtrExt_0_value");
        debug_sq_cmt_flag_ =
            debug_signal(dut_, sq_prefix + "cmtPtrExt_0_flag");
        debug_sq_cmt_value_ =
            debug_signal(dut_, sq_prefix + "cmtPtrExt_0_value");
        debug_sq_allocated_24_ =
            debug_signal(dut_, sq_prefix + "allocated_24");
        debug_sq_committed_24_ =
            debug_signal(dut_, sq_prefix + "committed_24");
        debug_sq_completed_24_ =
            debug_signal(dut_, sq_prefix + "completed_24");
        const std::string lq_exception_prefix =
            "MemBlock_top.MemBlock.inner_lsq.loadQueue.exceptionBuffer.";
        debug_lq_exception_valid_ = debug_signal(
            dut_, lq_exception_prefix + "req_valid");
        debug_lq_exception_fullva_ = debug_signal(
            dut_, lq_exception_prefix + "req_fullva");
        debug_lq_exception_rob_value_ = debug_signal(
            dut_, lq_exception_prefix + "req_uop_robIdx_value");
        debug_lq_exception_rob_flag_ = debug_signal(
            dut_, lq_exception_prefix + "req_uop_robIdx_flag");
        for (unsigned port = 0; port < debug_lq_exception_s2_enqueue_.size();
             ++port) {
            const auto rtl_port = std::to_string(kScalarLoadLanes + port);
            debug_lq_exception_s2_enqueue_[port] = debug_signal(
                dut_, lq_exception_prefix + "s2_enqueue_" + rtl_port);
            debug_lq_exception_s2_fullva_[port] = debug_signal(
                dut_, lq_exception_prefix + "s2_req_" + rtl_port +
                          "_fullva");
            debug_lq_exception_s2_rob_value_[port] = debug_signal(
                dut_, lq_exception_prefix + "s2_req_" + rtl_port +
                          "_uop_robIdx_value");
            debug_lq_exception_s2_rob_flag_[port] = debug_signal(
                dut_, lq_exception_prefix + "s2_req_" + rtl_port +
                          "_uop_robIdx_flag");
        }
        debug_sms_req_valid_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.req_valid");
        debug_sms_buffer_state_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.bufferState");
        debug_sms_is_cross_page_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.isCrossPage");
        debug_sms_req_isvec_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.req_isvec");
        debug_sms_req_rob_value_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.req_uop_robIdx_value");
        debug_sms_req_sq_value_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.req_uop_sqIdx_value");
        debug_sms_req_sq_flag_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.req_uop_sqIdx_flag");
        debug_sms_cross_page_with_hit_ = debug_signal(
            dut_, "MemBlock_top.MemBlock._inner_storeMisalignBuffer_io_sqControl_toStoreQueue_crossPageWithHit");
        debug_sms_vec_wb_[0] = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.io_vecWriteBack_0_valid_0");
        debug_sms_vec_wb_[1] = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_storeMisalignBuffer.io_vecWriteBack_1_valid_0");
        debug_sq_do_deq_ = debug_signal(
            dut_, "MemBlock_top.MemBlock.inner_lsq.storeQueue._io_maControl_toStoreMisalignBuffer_doDeq_T");
        debug_trace_ << "picker_mem_direct_debug line=0x" << std::hex
                     << debug_line_ << std::dec
                     << " trace_all_a=" << debug_trace_all_a_
                     << " sms_signals="
                     << (debug_sms_buffer_state_ != nullptr)
                     << (debug_sms_cross_page_with_hit_ != nullptr)
                     << (debug_sq_do_deq_ != nullptr)
                     << " lq_exception_signals="
                     << (debug_lq_exception_valid_ != nullptr)
                     << (debug_lq_exception_fullva_ != nullptr)
                     << (debug_lq_exception_s2_enqueue_[0] != nullptr)
                     << (debug_lq_exception_s2_enqueue_[1] != nullptr) << '\n';
        debug_trace_.flush();
    }

    void sample_internal_debug()
    {
        if (!debug_trace_.is_open()) {
            return;
        }
        const auto emit = [&](const std::string &line) {
            debug_trace_ << "cycle=" << debug_cycle_ << ' ' << line << '\n';
            debug_trace_.flush();
        };
        const bool lq_exception_valid =
            debug_u(debug_lq_exception_valid_) != 0;
        const std::uint64_t lq_exception_fullva =
            debug_u(debug_lq_exception_fullva_);
        const unsigned lq_exception_rob_value = static_cast<unsigned>(
            debug_u(debug_lq_exception_rob_value_));
        const bool lq_exception_rob_flag =
            debug_u(debug_lq_exception_rob_flag_) != 0;
        if (lq_exception_valid != debug_lq_exception_previous_valid_ ||
            (lq_exception_valid &&
             (lq_exception_fullva != debug_lq_exception_previous_fullva_ ||
              lq_exception_rob_value !=
                  debug_lq_exception_previous_rob_value_ ||
              lq_exception_rob_flag != debug_lq_exception_previous_rob_flag_))) {
            std::ostringstream text;
            text << "lq_exception retained=" << lq_exception_valid
                 << " rob=" << lq_exception_rob_value << ':'
                 << lq_exception_rob_flag << " fullva=0x" << std::hex
                 << lq_exception_fullva << " top_vaddr=0x"
                 << dut_.queues.observation.vaddr.value() << std::dec
                 << " redirect=" << dut_.queues.redirect.valid.boolean();
            emit(text.str());
        }
        debug_lq_exception_previous_valid_ = lq_exception_valid;
        debug_lq_exception_previous_fullva_ = lq_exception_fullva;
        debug_lq_exception_previous_rob_value_ = lq_exception_rob_value;
        debug_lq_exception_previous_rob_flag_ = lq_exception_rob_flag;
        for (unsigned port = 0; port < debug_lq_exception_s2_enqueue_.size();
             ++port) {
            if (debug_u(debug_lq_exception_s2_enqueue_[port]) == 0) {
                continue;
            }
            std::ostringstream text;
            text << "lq_exception candidate_port=" << port << " rob="
                 << debug_u(debug_lq_exception_s2_rob_value_[port]) << ':'
                 << debug_u(debug_lq_exception_s2_rob_flag_[port])
                 << " fullva=0x" << std::hex
                 << debug_u(debug_lq_exception_s2_fullva_[port]) << std::dec;
            emit(text.str());
        }
        const bool a_valid = dut_.dcache.a.valid.boolean();
        const bool a_fire = a_valid &&
            dut_.dcache.a.ready.boolean();
        if (a_fire) {
            const auto source = static_cast<unsigned>(
                dut_.dcache.a.source.value());
            const auto address =
                dut_.dcache.a.address.value();
            const auto line = address & ~std::uint64_t{63};
            const bool target = line == debug_line_;
            if (source < debug_target_sources_.size()) {
                debug_target_sources_[source] = target;
                const auto transfer_bytes = std::uint64_t{1} <<
                    dut_.dcache.a.size.value();
                debug_target_beats_remaining_[source] = target
                    ? static_cast<unsigned>(std::max<std::uint64_t>(
                          1, transfer_bytes / 32))
                    : 0;
            }
            if (debug_trace_all_a_ || target) {
                std::ostringstream text;
                text << "A fire=1 opcode=0x" << std::hex
                     << dut_.dcache.a.opcode.value()
                     << " source=" << source << " address=0x" << address
                     << " line=0x" << line << std::dec;
                emit(text.str());
            }
        }
        const bool d_valid = dut_.dcache.d.valid.boolean();
        const bool d_fire = d_valid &&
            dut_.dcache.d.ready.boolean();
        if (d_fire) {
            const auto source = static_cast<unsigned>(
                dut_.dcache.d.source.value());
            const bool target = source < debug_target_sources_.size() &&
                debug_target_sources_[source];
            if (target) {
                std::ostringstream text;
                text << "D fire=1 opcode=0x" << std::hex
                     << dut_.dcache.d.opcode.value()
                     << " source=" << source << " data="
                     << debug_bytes(&dut_.dcache.d.data)
                     << " denied=" << std::dec
                     << dut_.dcache.d.denied.boolean()
                     << " corrupt="
                     << dut_.dcache.d.corrupt.boolean();
                emit(text.str());
                debug_internal_window_ = 4;
            }
            if (source < debug_target_sources_.size() &&
                debug_target_beats_remaining_[source] != 0 &&
                --debug_target_beats_remaining_[source] == 0) {
                debug_target_sources_[source] = false;
            }
        }
        if (debug_internal_window_ != 0) {
            --debug_internal_window_;
            for (unsigned lane = 0; lane < kScalarLoadLanes; ++lane) {
                if (debug_u(debug_mshr_forward_valid_[lane]) != 0) {
                    std::ostringstream text;
                    text << "mshr_forward lane=" << lane << " selected="
                         << debug_u(debug_mshr_forward_[lane]) << " data="
                         << debug_byte_array(debug_mshr_forward_data_[lane]);
                    emit(text.str());
                }
            }
        }
        for (unsigned lane = 0; lane < kScalarLoadLanes; ++lane) {
            const auto s2_paddr = debug_u(debug_s2_paddr_[lane]);
            if (debug_u(debug_s2_valid_[lane]) != 0 &&
                (s2_paddr & ~std::uint64_t{63}) == debug_line_) {
                std::ostringstream text;
                text << "load_s2 lane=" << lane << " rob="
                     << debug_u(debug_s2_rob_value_[lane]) << ':'
                     << debug_u(debug_s2_rob_flag_[lane]) << " vaddr=0x"
                     << std::hex << debug_u(debug_s2_vaddr_[lane])
                     << " paddr=0x" << s2_paddr << std::dec
                     << " from_d=" << debug_u(debug_s2_from_d_[lane])
                     << " miss=" << debug_u(debug_s2_miss_[lane])
                     << " full_fwd=" << debug_u(debug_s2_full_fwd_[lane])
                     << " sq_deq0=" << debug_u(debug_sq_deq0_flag_) << ':'
                     << debug_u(debug_sq_deq0_value_)
                     << " sq_deq1=" << debug_u(debug_sq_deq1_flag_) << ':'
                     << debug_u(debug_sq_deq1_value_)
                     << " sq_enq=" << debug_u(debug_sq_enq_flag_) << ':'
                     << debug_u(debug_sq_enq_value_)
                     << " sq_cmt=" << debug_u(debug_sq_cmt_flag_) << ':'
                     << debug_u(debug_sq_cmt_value_)
                     << " sq24=" << debug_u(debug_sq_allocated_24_) << ','
                     << debug_u(debug_sq_committed_24_) << ','
                     << debug_u(debug_sq_completed_24_)
                     << " sq_fwd1=0x" << std::hex
                     << debug_u(debug_sq_forward_mask1_[lane])
                     << " sq_fwd2=0x" << debug_u(debug_sq_forward_mask2_[lane])
                     << std::dec
                     << " sq_need=" << debug_u(debug_sq_need_forward_[lane])
                     << " d_data="
                     << debug_byte_array(debug_s2_d_data_[lane]);
                emit(text.str());
            }
            const auto s3_paddr = debug_u(debug_s3_paddr_[lane]);
            if (debug_u(debug_s3_valid_[lane]) != 0 &&
                (s3_paddr & ~std::uint64_t{63}) == debug_line_) {
                std::ostringstream text;
                text << "load_s3 lane=" << lane << " rob="
                     << debug_u(debug_s3_rob_value_[lane]) << ':'
                     << debug_u(debug_s3_rob_flag_[lane]) << " vaddr=0x"
                     << std::hex << debug_u(debug_s3_vaddr_[lane])
                     << " paddr=0x" << s3_paddr << " input="
                     << debug_bytes(debug_s3_input_data_[lane])
                     << " merged="
                     << debug_bytes(debug_s3_merged_data_[lane])
                     << " picked=0x" << debug_u(debug_s3_picked_data_[lane])
                     << std::dec << " select="
                     << debug_u(debug_s3_data_select_[lane])
                     << " handled=" << debug_u(debug_s3_handled_[lane])
                     << " mshr=" << debug_u(debug_s3_mshrid_[lane]);
                emit(text.str());
            }
        }
        const auto sms_state = debug_u(debug_sms_buffer_state_);
        const bool sms_active = debug_u(debug_sms_req_valid_) != 0;
        const bool sms_observable = sms_active ||
            debug_u(debug_sms_cross_page_with_hit_) != 0 ||
            debug_u(debug_sq_do_deq_) != 0 ||
            debug_u(debug_sms_vec_wb_[0]) != 0 ||
            debug_u(debug_sms_vec_wb_[1]) != 0 ||
            dut_.queues.backend.scommit.value() != 0;
        if (sms_observable) {
            std::ostringstream text;
            text << "sms state=" << sms_state
                 << " req=" << debug_u(debug_sms_req_valid_)
                 << " vec=" << debug_u(debug_sms_req_isvec_)
                 << " cross=" << debug_u(debug_sms_is_cross_page_)
                 << " rob=" << debug_u(debug_sms_req_rob_value_)
                 << " sq=" << debug_u(debug_sms_req_sq_value_) << ':'
                 << debug_u(debug_sms_req_sq_flag_)
                 << " with_hit=" << debug_u(debug_sms_cross_page_with_hit_)
                 << " do_deq=" << debug_u(debug_sq_do_deq_)
                 << " vec_wb=" << debug_u(debug_sms_vec_wb_[0]) << ','
                 << debug_u(debug_sms_vec_wb_[1])
                 << " top_vec_wb="
                 << dut_.feedback.vector_writeback.lane_0_valid.value() << ','
                 << dut_.feedback.vector_writeback.lane_1_valid.value()
                 << " slow_fb="
                 << dut_.feedback.vector_store.lane_0_feedback_slow_valid.value()
                 << ','
                 << dut_.feedback.vector_store.lane_1_feedback_slow_valid.value()
                 << " pendingst=" << dut_.queues.backend.pendingst.value()
                 << " scommit=" << dut_.queues.backend.scommit.value();
            emit(text.str());
        }
        ++debug_cycle_;
    }

    void trace_scalar_writeback_debug(
        unsigned lane, const generated::ScalarLoadWriteback &writeback)
    {
        if (!debug_trace_.is_open() || !writeback.valid) {
            return;
        }
        const auto *expected = scoreboard_.expected_for(writeback);
        if (expected == nullptr ||
            (expected->address & ~std::uint64_t{63}) != debug_line_) {
            return;
        }
        debug_trace_ << "cycle=" << debug_cycle_ << " writeback lane=" << lane
                     << " rob=" << static_cast<unsigned>(writeback.rob_value)
                     << ':' << writeback.rob_flag << " address=0x" << std::hex
                     << expected->address << " actual=0x" << writeback.data
                     << " expected=0x" << expected->data << std::dec << '\n';
        debug_trace_.flush();
    }

#endif
    bool wait_for_enqueue_capacity(
        unsigned lq_needed, unsigned sq_needed, unsigned timeout = 256);

    bool write_distributed_csr(std::uint16_t address, std::uint64_t data);

    void tick(bool monitor = true);

    bool check_components();

    void track_final_observation_bytes(
        std::uint64_t address, unsigned bytes, bool cacheable,
        bool identity_mapped);

    static std::uint64_t scalar_store_key(const StoreTransaction &transaction);

    static std::uint64_t vector_store_key(
        const VectorMemoryTransaction &transaction);

    DutAdapter dut_;
    FailureState error_{"environment", "environment-contract"};
    std::optional<std::uint64_t> last_reset_completed_cycle_;
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
    FeedbackMonitor feedback_;
    BridgeMonitor bridge_{dut_, error_};
    std::uint64_t dft_bridge_patterns_ = 0;
    std::uint64_t dft_bridge_digest_ = 0;
    std::uint64_t reset_functional_pulses_ = 0;
    std::uint64_t reset_dft_pulses_ = 0;
    std::uint64_t reset_scan_transitions_ = 0;
    std::uint64_t reset_async_assertions_ = 0;
    std::uint64_t ifetch_ptw_pending_ = 0;
    bool final_observation_tracking_ = false;
    std::set<std::uint64_t> final_observation_words_;
    std::set<std::uint64_t> final_observation_nc_words_;
    std::uint64_t final_observation_bytes_ = 0;
    std::uint64_t final_observation_nc_bytes_ = 0;
    std::uint64_t final_observation_alias_skips_ = 0;
    std::uint64_t final_observation_observed_words_ = 0;
    QueueAccounting queues_{error_};
    PageTableBuilder page_tables_{memory_, error_};
#ifdef MEMBLOCK_MEM_DIRECT_DEBUG
    std::ofstream debug_trace_;
    std::uint64_t debug_line_ = 0;
    std::uint64_t debug_cycle_ = 0;
    std::uint32_t debug_internal_window_ = 0;
    bool debug_trace_all_a_ = false;
    std::array<bool, 64> debug_target_sources_{};
    std::array<unsigned, 64> debug_target_beats_remaining_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_mshr_forward_valid_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_mshr_forward_{};
    std::array<std::array<ReadPort *, 16>, kScalarLoadLanes>
        debug_mshr_forward_data_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s2_valid_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s2_paddr_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s2_vaddr_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s2_rob_value_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s2_rob_flag_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s2_from_d_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s2_miss_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s2_full_fwd_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_sq_forward_mask1_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_sq_forward_mask2_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_sq_need_forward_{};
    ReadPort *debug_sq_deq0_flag_ = nullptr;
    ReadPort *debug_sq_deq0_value_ = nullptr;
    ReadPort *debug_sq_deq1_flag_ = nullptr;
    ReadPort *debug_sq_deq1_value_ = nullptr;
    ReadPort *debug_sq_enq_flag_ = nullptr;
    ReadPort *debug_sq_enq_value_ = nullptr;
    ReadPort *debug_sq_cmt_flag_ = nullptr;
    ReadPort *debug_sq_cmt_value_ = nullptr;
    ReadPort *debug_sq_allocated_24_ = nullptr;
    ReadPort *debug_sq_committed_24_ = nullptr;
    ReadPort *debug_sq_completed_24_ = nullptr;
    std::array<std::array<ReadPort *, 16>, kScalarLoadLanes> debug_s2_d_data_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s3_valid_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s3_paddr_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s3_vaddr_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s3_rob_value_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s3_rob_flag_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s3_handled_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s3_mshrid_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s3_data_select_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s3_input_data_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s3_merged_data_{};
    std::array<ReadPort *, kScalarLoadLanes> debug_s3_picked_data_{};
    ReadPort *debug_lq_exception_valid_ = nullptr;
    ReadPort *debug_lq_exception_fullva_ = nullptr;
    ReadPort *debug_lq_exception_rob_value_ = nullptr;
    ReadPort *debug_lq_exception_rob_flag_ = nullptr;
    std::array<ReadPort *, kVectorMemoryLanes>
        debug_lq_exception_s2_enqueue_{};
    std::array<ReadPort *, kVectorMemoryLanes>
        debug_lq_exception_s2_fullva_{};
    std::array<ReadPort *, kVectorMemoryLanes>
        debug_lq_exception_s2_rob_value_{};
    std::array<ReadPort *, kVectorMemoryLanes>
        debug_lq_exception_s2_rob_flag_{};
    bool debug_lq_exception_previous_valid_ = false;
    std::uint64_t debug_lq_exception_previous_fullva_ = 0;
    unsigned debug_lq_exception_previous_rob_value_ = 0;
    bool debug_lq_exception_previous_rob_flag_ = false;
    ReadPort *debug_sms_req_valid_ = nullptr;
    ReadPort *debug_sms_buffer_state_ = nullptr;
    ReadPort *debug_sms_is_cross_page_ = nullptr;
    ReadPort *debug_sms_req_isvec_ = nullptr;
    ReadPort *debug_sms_req_rob_value_ = nullptr;
    ReadPort *debug_sms_req_sq_value_ = nullptr;
    ReadPort *debug_sms_req_sq_flag_ = nullptr;
    ReadPort *debug_sms_cross_page_with_hit_ = nullptr;
    std::array<ReadPort *, 2> debug_sms_vec_wb_{};
    ReadPort *debug_sq_do_deq_ = nullptr;
#endif
};
} // namespace memblock
