#include "implementation.hpp"

namespace memblock {

Environment::Environment(int argc, char **argv)
    : impl_(std::make_unique<Impl>(argc, argv))
{}

Environment::~Environment() = default;

auto Environment::memory()  -> SparseMemory &
{
    return impl_->memory();
}

auto Environment::set_final_observation_tracking(bool enabled)  -> void
{
    impl_->set_final_observation_tracking(enabled);
}

auto Environment::final_observation_words() const
    -> std::vector<std::uint64_t>
{
    return impl_->final_observation_words();
}

auto Environment::final_observation_nc_words() const
    -> std::vector<std::uint64_t>
{
    return impl_->final_observation_nc_words();
}

auto Environment::final_observation_bytes() const -> std::uint64_t
{
    return impl_->final_observation_bytes();
}

auto Environment::final_observation_nc_bytes() const -> std::uint64_t
{
    return impl_->final_observation_nc_bytes();
}

auto Environment::verify_final_nc_effects()  -> bool
{
    return impl_->verify_final_nc_effects();
}

auto Environment::final_observation_alias_skips() const -> std::uint64_t
{
    return impl_->final_observation_alias_skips();
}

auto Environment::final_observation_observed_words() const -> std::uint64_t
{
    return impl_->final_observation_observed_words();
}

auto Environment::set_final_observation_observed_words(std::uint64_t words)
    -> void
{
    impl_->set_final_observation_observed_words(words);
}

auto Environment::add_final_observation_observed_words(std::uint64_t words)
    -> void
{
    impl_->add_final_observation_observed_words(words);
}

auto Environment::discard_final_observation_range(
    std::uint64_t address, unsigned bytes)  -> void
{
    impl_->discard_final_observation_range(address, bytes);
}

auto Environment::bus_expected_load(std::uint64_t address,
    LoadOp op) const -> std::uint64_t
{
    return impl_->bus_expected_load(address, op);
}

auto Environment::expect_release_line(std::uint64_t base,
    const std::vector<unsigned char> &bytes)  -> void
{
    return impl_->expect_release_line(base, bytes);
}

auto Environment::clear_release_line_expectations()  -> void
{
    return impl_->clear_release_line_expectations();
}

auto Environment::request_dcache_probe(std::uint64_t address,
    std::uint8_t cap,
    bool need_data,
    std::uint8_t expected_report,
    const std::vector<unsigned char> &expected_data)  -> bool
{
    return impl_->request_dcache_probe(address, cap, need_data, expected_report, expected_data);
}

auto Environment::hold_dcache_c_until_probes(std::uint64_t target_requests)  -> bool
{
    return impl_->hold_dcache_c_until_probes(target_requests);
}

auto Environment::configure_backpressure(std::uint64_t seed,
    bool enabled,
    ResponseLatencyProfile latency_profile)  -> void
{
    return impl_->configure_backpressure(seed, enabled, latency_profile);
}

auto Environment::configure_backpressure(std::uint64_t seed,
    bool enabled,
    const ResponseLatencyProfiles &latency_profiles)  -> void
{
    return impl_->configure_backpressure(seed, enabled, latency_profiles);
}

auto Environment::inject_next_dcache_response_error(bool denied,
    bool corrupt,
    DcacheCorruptBeat corrupt_beat)  -> void
{
    return impl_->inject_next_dcache_response_error(denied, corrupt, corrupt_beat);
}

auto Environment::inject_dcache_response_error_at(std::uint64_t address,
    bool denied,
    bool corrupt,
    DcacheCorruptBeat corrupt_beat)  -> void
{
    return impl_->inject_dcache_response_error_at(address, denied, corrupt, corrupt_beat);
}

auto Environment::inject_dcache_cmo_response_error_at(std::uint64_t address,
    bool denied,
    bool corrupt)  -> void
{
    return impl_->inject_dcache_cmo_response_error_at(address, denied, corrupt);
}

auto Environment::inject_ptw_response_error_after(unsigned clean_requests,
    bool denied,
    bool corrupt,
    PtwCorruptBeat corrupt_beat)  -> void
{
    return impl_->inject_ptw_response_error_after(clean_requests, denied, corrupt, corrupt_beat);
}

auto Environment::inject_ptw_response_error_at(std::uint64_t address,
    bool denied,
    bool corrupt,
    PtwCorruptBeat corrupt_beat)  -> void
{
    return impl_->inject_ptw_response_error_at(address, denied, corrupt, corrupt_beat);
}

auto Environment::inject_persistent_ptw_response_error_at(std::uint64_t address,
    bool denied,
    bool corrupt,
    PtwCorruptBeat corrupt_beat)  -> void
{
    return impl_->inject_persistent_ptw_response_error_at(address, denied, corrupt, corrupt_beat);
}

auto Environment::clear_ptw_response_error()  -> void
{
    return impl_->clear_ptw_response_error();
}

auto Environment::force_next_dcache_response_delay(unsigned cycles)  -> void
{
    return impl_->force_next_dcache_response_delay(cycles);
}

auto Environment::force_next_cmo_response_delay(unsigned cycles)  -> void
{
    return impl_->force_next_cmo_response_delay(cycles);
}

auto Environment::force_next_dcache_interbeat_delay(unsigned cycles)  -> void
{
    return impl_->force_next_dcache_interbeat_delay(cycles);
}

auto Environment::hold_dcache_response_at(std::uint64_t address)  -> void
{
    return impl_->hold_dcache_response_at(address);
}

auto Environment::dcache_response_held() const -> bool
{
    return impl_->dcache_response_held();
}

auto Environment::dcache_response_held_at(std::uint64_t address) const -> bool
{
    return impl_->dcache_response_held_at(address);
}

auto Environment::dcache_held_response_count() const -> std::size_t
{
    return impl_->dcache_held_response_count();
}

auto Environment::release_held_dcache_response()  -> bool
{
    return impl_->release_held_dcache_response();
}

auto Environment::release_held_dcache_response_at(std::uint64_t address)  -> bool
{
    return impl_->release_held_dcache_response_at(address);
}

auto Environment::schedule_held_dcache_response_release_at(std::uint64_t address,
    unsigned after_ticks)  -> bool
{
    return impl_->schedule_held_dcache_response_release_at(address, after_ticks);
}

auto Environment::begin_dcache_release_ready_window(const std::vector<std::uint64_t> &target_lines,
    unsigned stall_valid_cycles)  -> bool
{
    return impl_->begin_dcache_release_ready_window(target_lines, stall_valid_cycles);
}

auto Environment::end_dcache_release_ready_window()  -> bool
{
    return impl_->end_dcache_release_ready_window();
}

auto Environment::inject_next_uncache_response_error(bool denied,
    bool corrupt)  -> void
{
    return impl_->inject_next_uncache_response_error(denied, corrupt);
}

auto Environment::force_next_ptw_response_delay(unsigned cycles)  -> void
{
    return impl_->force_next_ptw_response_delay(cycles);
}

auto Environment::force_next_uncache_response_delay(unsigned cycles)  -> void
{
    return impl_->force_next_uncache_response_delay(cycles);
}

auto Environment::configure_uncache_device(std::uint64_t base,
    std::uint64_t size,
    bool read_clear)  -> void
{
    return impl_->configure_uncache_device(base, size, read_clear);
}

auto Environment::uncache_device_accesses() const -> const std::vector<UncacheDeviceAccess> &
{
    return impl_->uncache_device_accesses();
}

auto Environment::configure_cache_error_enable(bool enable)  -> void
{
    return impl_->configure_cache_error_enable(enable);
}

auto Environment::configure_ldld_violation_check(bool enable)  -> void
{
    return impl_->configure_ldld_violation_check(enable);
}

auto Environment::configure_stride_prefetch(bool enable)  -> bool
{
    return impl_->configure_stride_prefetch(enable);
}

auto Environment::configure_sms_pht_prefetch(bool output_enable)  -> bool
{
    return impl_->configure_sms_pht_prefetch(output_enable);
}

auto Environment::expect_l2_prefetch_control(bool master_enabled,
    bool receive_enabled)  -> bool
{
    return impl_->expect_l2_prefetch_control(master_enabled, receive_enabled);
}

auto Environment::issue_l2_tlb_request(std::uint64_t vaddr,
    std::uint8_t cmd,
    bool kill,
    bool is_prefetch,
    bool no_translate,
    L2TlbResponse &response,
    unsigned timeout)  -> bool
{
    return impl_->issue_l2_tlb_request(vaddr, cmd, kill, is_prefetch, no_translate, response, timeout);
}

auto Environment::issue_killed_l2_tlb_request(std::uint64_t vaddr,
    std::uint8_t cmd,
    bool is_prefetch,
    bool no_translate,
    unsigned timeout)  -> bool
{
    return impl_->issue_killed_l2_tlb_request(vaddr, cmd, is_prefetch, no_translate, timeout);
}

auto Environment::start_ifetch_ptw_request(std::uint64_t vpn,
    PtwTranslationMode mode,
    unsigned timeout)  -> bool
{
    return impl_->start_ifetch_ptw_request(vpn, mode, timeout);
}

auto Environment::complete_ifetch_ptw_request(IFetchPtwResponse &response,
    unsigned response_stall_cycles,
    unsigned timeout)  -> bool
{
    return impl_->complete_ifetch_ptw_request(response, response_stall_cycles, timeout);
}

auto Environment::issue_ifetch_ptw_request(std::uint64_t vpn,
    PtwTranslationMode mode,
    IFetchPtwResponse &response,
    unsigned response_stall_cycles,
    unsigned timeout)  -> bool
{
    return impl_->issue_ifetch_ptw_request(vpn, mode, response, response_stall_cycles, timeout);
}

auto Environment::confirm_ifetch_ptw_flushed(unsigned quiet_cycles)  -> bool
{
    return impl_->confirm_ifetch_ptw_flushed(quiet_cycles);
}

auto Environment::pulse_l2_hint(std::uint8_t source_id,
    bool is_keyword)  -> bool
{
    return impl_->pulse_l2_hint(source_id, is_keyword);
}

auto Environment::configure_memory_trigger(const MemoryTriggerConfig &config)  -> bool
{
    return impl_->configure_memory_trigger(config);
}

auto Environment::cycle() const -> std::uint64_t
{
    return impl_->cycle();
}

auto Environment::tilelink_requests() const -> std::uint64_t
{
    return impl_->tilelink_requests();
}

auto Environment::dcache_last_request_address() const -> std::uint64_t
{
    return impl_->dcache_last_request_address();
}

auto Environment::dcache_request_classes() const -> const std::array<std::uint64_t, 7> &
{
    return impl_->dcache_request_classes();
}

auto Environment::dcache_request_summary_since(std::uint64_t first_request,
    unsigned maximum_records) const -> std::string
{
    return impl_->dcache_request_summary_since(first_request, maximum_records);
}

auto Environment::dcache_faulting_load_requests_error_since(std::uint64_t target_line,
    bool target_read_allowed,
    std::uint64_t first_request) const -> const char *
{
    return impl_->dcache_faulting_load_requests_error_since(target_line, target_read_allowed, first_request);
}

auto Environment::dcache_requests_covering_since(std::uint64_t address,
    std::uint64_t first_request) const -> std::uint64_t
{
    return impl_->dcache_requests_covering_since(address, first_request);
}

auto Environment::dcache_store_ntot_line_count_since(std::uint64_t line,
    std::uint64_t first_request) const -> std::uint64_t
{
    return impl_->dcache_store_ntot_line_count_since(line, first_request);
}

auto Environment::dcache_requests_from_source_since(std::uint8_t request_source,
    std::uint64_t first_request) const -> std::uint64_t
{
    return impl_->dcache_requests_from_source_since(request_source, first_request);
}

auto Environment::dcache_requests_from_source_covering_since(std::uint8_t request_source,
    std::uint64_t address,
    std::uint64_t first_request) const -> std::uint64_t
{
    return impl_->dcache_requests_from_source_covering_since(request_source, address, first_request);
}

auto Environment::begin_dcache_release_observation()  -> void
{
    return impl_->begin_dcache_release_observation();
}

auto Environment::end_dcache_release_observation()  -> void
{
    return impl_->end_dcache_release_observation();
}

auto Environment::observed_dcache_releases_covering(std::uint64_t address) const -> std::uint64_t
{
    return impl_->observed_dcache_releases_covering(address);
}

auto Environment::observed_dcache_release_data_covering(std::uint64_t address) const -> std::uint64_t
{
    return impl_->observed_dcache_release_data_covering(address);
}

auto Environment::observed_dcache_stalled_releases_covering(std::uint64_t address,
    bool data_only) const -> std::uint64_t
{
    return impl_->observed_dcache_stalled_releases_covering(address, data_only);
}

auto Environment::observed_dcache_release_stall_cycles_covering(std::uint64_t address,
    bool data_only) const -> std::uint64_t
{
    return impl_->observed_dcache_release_stall_cycles_covering(address, data_only);
}

auto Environment::dcache_gets() const -> std::uint64_t
{
    return impl_->dcache_gets();
}

auto Environment::dcache_refills() const -> std::uint64_t
{
    return impl_->dcache_refills();
}

auto Environment::dcache_keyword_refills() const -> std::uint64_t
{
    return impl_->dcache_keyword_refills();
}

auto Environment::dcache_nonkeyword_refills() const -> std::uint64_t
{
    return impl_->dcache_nonkeyword_refills();
}

auto Environment::dcache_acquire_perms() const -> std::uint64_t
{
    return impl_->dcache_acquire_perms();
}

auto Environment::dcache_cmo_requests(StoreOp operation) const -> std::uint64_t
{
    return impl_->dcache_cmo_requests(operation);
}

auto Environment::dcache_last_cmo_address() const -> std::uint64_t
{
    return impl_->dcache_last_cmo_address();
}

auto Environment::dcache_last_cmo_source() const -> std::uint8_t
{
    return impl_->dcache_last_cmo_source();
}

auto Environment::tilelink_releases() const -> std::uint64_t
{
    return impl_->tilelink_releases();
}

auto Environment::tilelink_release_data() const -> std::uint64_t
{
    return impl_->tilelink_release_data();
}

auto Environment::tilelink_release_data_verified() const -> std::uint64_t
{
    return impl_->tilelink_release_data_verified();
}

auto Environment::tilelink_expected_release_data_verified() const -> std::uint64_t
{
    return impl_->tilelink_expected_release_data_verified();
}

auto Environment::dcache_probes() const -> std::uint64_t
{
    return impl_->dcache_probes();
}

auto Environment::dcache_probe_responses() const -> std::uint64_t
{
    return impl_->dcache_probe_responses();
}

auto Environment::dcache_probe_data() const -> std::uint64_t
{
    return impl_->dcache_probe_data();
}

auto Environment::dcache_probe_sources() const -> std::uint64_t
{
    return impl_->dcache_probe_sources();
}

auto Environment::dcache_probe_source_reuses() const -> std::uint64_t
{
    return impl_->dcache_probe_source_reuses();
}

auto Environment::dcache_probe_source_wraps() const -> std::uint64_t
{
    return impl_->dcache_probe_source_wraps();
}

auto Environment::dcache_max_probe_outstanding() const -> std::uint64_t
{
    return impl_->dcache_max_probe_outstanding();
}

auto Environment::dcache_probe_stalls() const -> std::uint64_t
{
    return impl_->dcache_probe_stalls();
}

auto Environment::dcache_grant_acks() const -> std::uint64_t
{
    return impl_->dcache_grant_acks();
}

auto Environment::dcache_grant_data_beats() const -> std::uint64_t
{
    return impl_->dcache_grant_data_beats();
}

auto Environment::dcache_denied_d_beats() const -> std::uint64_t
{
    return impl_->dcache_denied_d_beats();
}

auto Environment::dcache_corrupt_d_beats() const -> std::uint64_t
{
    return impl_->dcache_corrupt_d_beats();
}

auto Environment::dcache_error_response_requests() const -> std::uint64_t
{
    return impl_->dcache_error_response_requests();
}

auto Environment::dcache_error_response_refills() const -> std::uint64_t
{
    return impl_->dcache_error_response_refills();
}

auto Environment::dcache_error_response_grant_acks() const -> std::uint64_t
{
    return impl_->dcache_error_response_grant_acks();
}

auto Environment::dcache_last_error_response_address() const -> std::uint64_t
{
    return impl_->dcache_last_error_response_address();
}

auto Environment::dcache_last_error_response_keyword() const -> bool
{
    return impl_->dcache_last_error_response_keyword();
}

auto Environment::dcache_max_outstanding_requests() const -> std::uint64_t
{
    return impl_->dcache_max_outstanding_requests();
}

auto Environment::dcache_outstanding_requests() const -> std::uint64_t
{
    return impl_->dcache_outstanding_requests();
}

auto Environment::dcache_grant_ack_stalls() const -> std::uint64_t
{
    return impl_->dcache_grant_ack_stalls();
}

auto Environment::dcache_c_payload_stability_checks() const -> std::uint64_t
{
    return impl_->dcache_c_payload_stability_checks();
}

auto Environment::dcache_release_target_payload_stability_checks() const -> std::uint64_t
{
    return impl_->dcache_release_target_payload_stability_checks();
}

auto Environment::dcache_grants_drained() const -> bool
{
    return impl_->dcache_grants_drained();
}

auto Environment::dcache_responses_idle() const -> bool
{
    return impl_->dcache_responses_idle();
}

auto Environment::dcache_release_channel_idle() const -> bool
{
    return impl_->dcache_release_channel_idle();
}

auto Environment::ptw_requests() const -> std::uint64_t
{
    return impl_->ptw_requests();
}

auto Environment::ptw_error_response_requests() const -> std::uint64_t
{
    return impl_->ptw_error_response_requests();
}

auto Environment::ptw_denied_d_beats() const -> std::uint64_t
{
    return impl_->ptw_denied_d_beats();
}

auto Environment::ptw_corrupt_d_beats() const -> std::uint64_t
{
    return impl_->ptw_corrupt_d_beats();
}

auto Environment::ptw_last_error_response_address() const -> std::uint64_t
{
    return impl_->ptw_last_error_response_address();
}

auto Environment::ptw_last_error_response_source() const -> std::uint8_t
{
    return impl_->ptw_last_error_response_source();
}

auto Environment::ptw_requests_covering_since(std::uint64_t address,
    std::uint64_t first_request) const -> std::uint64_t
{
    return impl_->ptw_requests_covering_since(address, first_request);
}

auto Environment::pending_ifetch_ptw_requests() const -> std::uint64_t
{
    return impl_->pending_ifetch_ptw_requests();
}

auto Environment::dcache_request_stalls() const -> std::uint64_t
{
    return impl_->dcache_request_stalls();
}

auto Environment::dcache_response_delays() const -> std::uint64_t
{
    return impl_->dcache_response_delays();
}

auto Environment::dcache_response_latency_stats() const -> const ResponseLatencyStats &
{
    return impl_->dcache_response_latency_stats();
}

auto Environment::ptw_request_stalls() const -> std::uint64_t
{
    return impl_->ptw_request_stalls();
}

auto Environment::ptw_response_delays() const -> std::uint64_t
{
    return impl_->ptw_response_delays();
}

auto Environment::ptw_max_outstanding_requests() const -> std::uint64_t
{
    return impl_->ptw_max_outstanding_requests();
}

auto Environment::ptw_outstanding_requests() const -> std::uint64_t
{
    return impl_->ptw_outstanding_requests();
}

auto Environment::ptw_response_latency_stats() const -> const ResponseLatencyStats &
{
    return impl_->ptw_response_latency_stats();
}

auto Environment::exception_vaddr()  -> std::uint64_t
{
    return impl_->exception_vaddr();
}

auto Environment::select_store_exception_address(bool store)  -> void
{
    return impl_->select_store_exception_address(store);
}

auto Environment::exception_gpaddr()  -> std::uint64_t
{
    return impl_->exception_gpaddr();
}

auto Environment::exception_is_for_vs_nonleaf_pte()  -> bool
{
    return impl_->exception_is_for_vs_nonleaf_pte();
}

auto Environment::uncache_requests() const -> std::uint64_t
{
    return impl_->uncache_requests();
}

auto Environment::uncache_request_stalls() const -> std::uint64_t
{
    return impl_->uncache_request_stalls();
}

auto Environment::uncache_response_delays() const -> std::uint64_t
{
    return impl_->uncache_response_delays();
}

auto Environment::uncache_max_outstanding_requests() const -> std::uint64_t
{
    return impl_->uncache_max_outstanding_requests();
}

auto Environment::uncache_outstanding_requests() const -> std::uint64_t
{
    return impl_->uncache_outstanding_requests();
}

auto Environment::uncache_error_response_requests() const -> std::uint64_t
{
    return impl_->uncache_error_response_requests();
}

auto Environment::uncache_denied_d_beats() const -> std::uint64_t
{
    return impl_->uncache_denied_d_beats();
}

auto Environment::uncache_corrupt_d_beats() const -> std::uint64_t
{
    return impl_->uncache_corrupt_d_beats();
}

auto Environment::uncache_response_latency_stats() const -> const ResponseLatencyStats &
{
    return impl_->uncache_response_latency_stats();
}

auto Environment::store_mmio_valid()  -> bool
{
    return impl_->store_mmio_valid();
}

auto Environment::store_mmio_rob()  -> std::uint8_t
{
    return impl_->store_mmio_rob();
}

auto Environment::store_tlb_feedbacks() const -> std::uint64_t
{
    return impl_->store_tlb_feedbacks();
}

auto Environment::store_tlb_misses() const -> std::uint64_t
{
    return impl_->store_tlb_misses();
}

auto Environment::lq_allocated() const -> std::uint64_t
{
    return impl_->lq_allocated();
}

auto Environment::lq_enqueued_observed() const -> std::uint64_t
{
    return impl_->lq_enqueued_observed();
}

auto Environment::lq_dequeued() const -> std::uint64_t
{
    return impl_->lq_dequeued();
}

auto Environment::lq_canceled() const -> std::uint64_t
{
    return impl_->lq_canceled();
}

auto Environment::lq_redirect_canceled_observed() const -> std::uint64_t
{
    return impl_->lq_redirect_canceled_observed();
}

auto Environment::lq_canceled_unobserved() const -> std::uint64_t
{
    return impl_->lq_canceled_unobserved();
}

auto Environment::sq_allocated() const -> std::uint64_t
{
    return impl_->sq_allocated();
}

auto Environment::sq_enqueued_observed() const -> std::uint64_t
{
    return impl_->sq_enqueued_observed();
}

auto Environment::sq_dequeued() const -> std::uint64_t
{
    return impl_->sq_dequeued();
}

auto Environment::sq_canceled() const -> std::uint64_t
{
    return impl_->sq_canceled();
}

auto Environment::sq_redirect_canceled_observed() const -> std::uint64_t
{
    return impl_->sq_redirect_canceled_observed();
}

auto Environment::sq_canceled_unobserved() const -> std::uint64_t
{
    return impl_->sq_canceled_unobserved();
}

auto Environment::redirect_cancellation_events_observed() const -> std::uint64_t
{
    return impl_->redirect_cancellation_events_observed();
}

auto Environment::lsq_enqueue_widths_observed() const -> const std::array<std::uint64_t, generated::kLsqEnqueueLanes> &
{
    return impl_->lsq_enqueue_widths_observed();
}

auto Environment::lsq_enqueue_lanes_observed() const -> const std::array<std::uint64_t, generated::kLsqEnqueueLanes> &
{
    return impl_->lsq_enqueue_lanes_observed();
}

auto Environment::writebacks() const -> std::uint64_t
{
    return impl_->writebacks();
}

auto Environment::prefetch_writebacks() const -> std::uint64_t
{
    return impl_->prefetch_writebacks();
}

auto Environment::store_writebacks() const -> std::uint64_t
{
    return impl_->store_writebacks();
}

auto Environment::vector_load_writebacks() const -> std::uint64_t
{
    return impl_->vector_load_writebacks();
}

auto Environment::vector_store_writebacks() const -> std::uint64_t
{
    return impl_->vector_store_writebacks();
}

auto Environment::vector_fof_fix_writebacks() const -> std::uint64_t
{
    return impl_->vector_fof_fix_writebacks();
}

auto Environment::vector_page_fault_writebacks() const -> std::uint64_t
{
    return impl_->vector_page_fault_writebacks();
}

auto Environment::pending_scalar_loads() const -> std::size_t
{
    return impl_->pending_scalar_loads();
}

auto Environment::scalar_load_pending(const LoadTransaction &transaction) const -> bool
{
    return impl_->scalar_load_pending(transaction);
}

auto Environment::scalar_store_pending(const StoreTransaction &transaction) const -> bool
{
    return impl_->scalar_store_pending(transaction);
}

auto Environment::scalar_store_address_pending(const StoreTransaction &transaction) const -> bool
{ return impl_->scalar_store_address_pending(transaction); }

auto Environment::prefetch_pending(const PrefetchTransaction &transaction) const -> bool
{ return impl_->prefetch_pending(transaction); }

auto Environment::vector_memory_pending(const VectorMemoryTransaction &transaction) const -> bool
{ return impl_->vector_memory_pending(transaction); }

auto Environment::service_vector_replay(const VectorMemoryTransaction &transaction,
    bool &replayed, unsigned timeout) -> bool
{ return impl_->service_vector_replay(transaction, replayed, timeout); }

auto Environment::set_rob_head_state(std::uint8_t value, bool flag, bool pending_store) -> bool
{ return impl_->set_rob_head_state(value, flag, pending_store); }

auto Environment::cancel_scalar_load_for_reset(const LoadTransaction &transaction)  -> bool
{
    return impl_->cancel_scalar_load_for_reset(transaction);
}

auto Environment::cancel_scalar_load_after_redirect(const LoadTransaction &transaction) -> bool
{
    return impl_->cancel_scalar_load_after_redirect(transaction);
}

auto Environment::scalar_load_redirect_canceled() const -> std::uint64_t
{ return impl_->scalar_load_redirect_canceled(); }

auto Environment::scalar_load_reset_canceled() const -> std::uint64_t
{
    return impl_->scalar_load_reset_canceled();
}

auto Environment::scalar_load_canceled_observed() const -> std::uint64_t
{
    return impl_->scalar_load_canceled_observed();
}

auto Environment::cancel_scalar_store_for_reset(const StoreTransaction &transaction)  -> bool
{
    return impl_->cancel_scalar_store_for_reset(transaction);
}

auto Environment::scalar_store_reset_canceled() const -> std::uint64_t
{
    return impl_->scalar_store_reset_canceled();
}

auto Environment::pending_prefetches() const -> std::size_t
{
    return impl_->pending_prefetches();
}

auto Environment::pending_scalar_stores() const -> std::size_t
{
    return impl_->pending_scalar_stores();
}

auto Environment::pending_vector_loads() const -> std::size_t
{
    return impl_->pending_vector_loads();
}

auto Environment::pending_vector_stores() const -> std::size_t
{
    return impl_->pending_vector_stores();
}

auto Environment::vector_replay_feedbacks() const -> std::uint64_t
{
    return impl_->vector_replay_feedbacks();
}

auto Environment::pin_space_digest() const -> std::uint64_t
{
    return impl_->pin_space_digest();
}

auto Environment::frontend_bridge_stats() const -> const FrontendBridgeStats &
{
    return impl_->frontend_bridge_stats();
}

auto Environment::scalar_load_feedback_stats() const -> const ScalarLoadFeedbackStats &
{
    return impl_->scalar_load_feedback_stats();
}

auto Environment::load_mmio_stats() const -> const LoadMmioStats &
{
    return impl_->load_mmio_stats();
}

auto Environment::bus_error_stats() const -> const BusErrorStats &
{
    return impl_->bus_error_stats();
}

auto Environment::top_down_stats() const -> const TopDownStats &
{
    return impl_->top_down_stats();
}

auto Environment::iq_slow_feedback_stats() const -> const IqSlowFeedbackStats &
{
    return impl_->iq_slow_feedback_stats();
}

auto Environment::memory_violation_stats() const -> const MemoryViolationStats &
{
    return impl_->memory_violation_stats();
}

auto Environment::ifetch_prefetch_stats() const -> const IfetchPrefetchStats &
{
    return impl_->ifetch_prefetch_stats();
}

auto Environment::hardware_prefetch_stats() const -> const HardwarePrefetchStats &
{
    return impl_->hardware_prefetch_stats();
}

auto Environment::sbuffer_empty()  -> bool
{
    return impl_->sbuffer_empty();
}

auto Environment::drive_top_down_misses(bool l2_miss,
    bool l3_miss)  -> void
{
    return impl_->drive_top_down_misses(l2_miss, l3_miss);
}

auto Environment::run_until_sbuffer_empty(unsigned timeout)  -> bool
{
    return impl_->run_until_sbuffer_empty(timeout);
}

auto Environment::exercise_frontend_bridges(unsigned transaction_count,
    std::uint64_t seed)  -> bool
{
    return impl_->exercise_frontend_bridges(transaction_count, seed);
}

auto Environment::check_frontend_bridge_reset_recovery()  -> bool
{
    return impl_->check_frontend_bridge_reset_recovery();
}

auto Environment::frontend_reset_canceled_requests() const -> std::uint64_t
{
    return impl_->frontend_reset_canceled_requests();
}

auto Environment::frontend_reset_canceled_responses() const -> std::uint64_t
{
    return impl_->frontend_reset_canceled_responses();
}

auto Environment::frontend_reset_survivor_requests() const -> std::uint64_t
{
    return impl_->frontend_reset_survivor_requests();
}

auto Environment::frontend_reset_stall_checks() const -> std::uint64_t
{
    return impl_->frontend_reset_stall_checks();
}

auto Environment::check_pin_space()  -> bool
{
    return impl_->check_pin_space();
}

auto Environment::reset()  -> bool
{
    return impl_->reset();
}

auto Environment::check_reset_backend_contract()  -> bool
{
    return impl_->check_reset_backend_contract();
}

auto Environment::reset_functional_pulses() const -> std::uint64_t
{
    return impl_->reset_functional_pulses();
}

auto Environment::reset_dft_pulses() const -> std::uint64_t
{
    return impl_->reset_dft_pulses();
}

auto Environment::reset_scan_transitions() const -> std::uint64_t
{
    return impl_->reset_scan_transitions();
}

auto Environment::reset_async_assertions() const -> std::uint64_t
{
    return impl_->reset_async_assertions();
}

auto Environment::configure_sv39(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool writable,
    bool noncacheable)  -> bool
{
    return impl_->configure_sv39(virtual_address, physical_address, root_page_table, writable, noncacheable);
}

auto Environment::configure_sv39_nc(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool writable)  -> bool
{
    return impl_->configure_sv39_nc(virtual_address, physical_address, root_page_table, writable);
}

auto Environment::map_sv39_4k(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable,
    bool io,
    bool global)  -> bool
{
    return impl_->map_sv39_4k(virtual_address, physical_address, root_page_table, readable, writable, executable, user, noncacheable, io, global);
}

auto Environment::map_sv48_4k(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable,
    bool io,
    bool global)  -> bool
{
    return impl_->map_sv48_4k(virtual_address, physical_address, root_page_table, readable, writable, executable, user, noncacheable, io, global);
}

auto Environment::activate_sv39(std::uint64_t root_page_table,
    std::uint16_t asid)  -> bool
{
    return impl_->activate_sv39(root_page_table, asid);
}

auto Environment::activate_sv48(std::uint64_t root_page_table,
    std::uint16_t asid)  -> bool
{
    return impl_->activate_sv48(root_page_table, asid);
}

auto Environment::activate_bare(std::uint16_t asid)  -> bool
{
    return impl_->activate_bare(asid);
}

auto Environment::map_sv39_leaf(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    unsigned leaf_level,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable,
    bool accessed,
    std::optional<bool> dirty,
    bool io,
    bool global)  -> bool
{
    return impl_->map_sv39_leaf(virtual_address, physical_address, leaf_level, root_page_table, readable, writable, executable, user, noncacheable, accessed, dirty, io, global);
}

auto Environment::map_sv39_2m(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable)  -> bool
{
    return impl_->map_sv39_2m(virtual_address, physical_address, root_page_table, readable, writable, executable, user, noncacheable);
}

auto Environment::map_sv39_1g(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable)  -> bool
{
    return impl_->map_sv39_1g(virtual_address, physical_address, root_page_table, readable, writable, executable, user, noncacheable);
}

auto Environment::map_sv48_leaf(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    unsigned leaf_level,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable,
    bool accessed,
    std::optional<bool> dirty,
    bool io,
    bool global)  -> bool
{
    return impl_->map_sv48_leaf(virtual_address, physical_address, leaf_level, root_page_table, readable, writable, executable, user, noncacheable, accessed, dirty, io, global);
}

auto Environment::map_sv48_2m(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable)  -> bool
{
    return impl_->map_sv48_2m(virtual_address, physical_address, root_page_table, readable, writable, executable, user, noncacheable);
}

auto Environment::map_sv48_1g(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable)  -> bool
{
    return impl_->map_sv48_1g(virtual_address, physical_address, root_page_table, readable, writable, executable, user, noncacheable);
}

auto Environment::map_sv48_512g(std::uint64_t virtual_address,
    std::uint64_t physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable)  -> bool
{
    return impl_->map_sv48_512g(virtual_address, physical_address, root_page_table, readable, writable, executable, user, noncacheable);
}

auto Environment::map_sv48x4_4k(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool global)  -> bool
{
    return impl_->map_sv48x4_4k(guest_physical_address, host_physical_address, root_page_table, readable, writable, executable, global);
}

auto Environment::map_sv48x4_leaf(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    unsigned leaf_level,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool accessed,
    std::optional<bool> dirty,
    bool user,
    bool noncacheable,
    bool io,
    bool global)  -> bool
{
    return impl_->map_sv48x4_leaf(guest_physical_address, host_physical_address, leaf_level, root_page_table, readable, writable, executable, accessed, dirty, user, noncacheable, io, global);
}

auto Environment::map_sv48x4_2m(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable)  -> bool
{
    return impl_->map_sv48x4_2m(guest_physical_address, host_physical_address, root_page_table, readable, writable, executable);
}

auto Environment::map_sv48x4_1g(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable)  -> bool
{
    return impl_->map_sv48x4_1g(guest_physical_address, host_physical_address, root_page_table, readable, writable, executable);
}

auto Environment::map_sv48x4_512g(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable)  -> bool
{
    return impl_->map_sv48x4_512g(guest_physical_address, host_physical_address, root_page_table, readable, writable, executable);
}

auto Environment::map_sv39_napot64k(std::uint64_t virtual_base,
    std::uint64_t physical_base,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable,
    bool accessed,
    std::optional<bool> dirty,
    bool io)  -> bool
{
    return impl_->map_sv39_napot64k(virtual_base, physical_base, root_page_table, readable, writable, executable, user, noncacheable, accessed, dirty, io);
}

auto Environment::map_sv48_napot64k(std::uint64_t virtual_base,
    std::uint64_t physical_base,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool user,
    bool noncacheable,
    bool accessed,
    std::optional<bool> dirty,
    bool io)  -> bool
{
    return impl_->map_sv48_napot64k(virtual_base, physical_base, root_page_table, readable, writable, executable, user, noncacheable, accessed, dirty, io);
}

auto Environment::map_sv39x4_napot64k(std::uint64_t guest_physical_base,
    std::uint64_t host_physical_base,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool accessed,
    std::optional<bool> dirty,
    bool user,
    bool noncacheable,
    bool io)  -> bool
{
    return impl_->map_sv39x4_napot64k(guest_physical_base, host_physical_base, root_page_table, readable, writable, executable, accessed, dirty, user, noncacheable, io);
}

auto Environment::map_sv48x4_napot64k(std::uint64_t guest_physical_base,
    std::uint64_t host_physical_base,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool accessed,
    std::optional<bool> dirty,
    bool user,
    bool noncacheable,
    bool io)  -> bool
{
    return impl_->map_sv48x4_napot64k(guest_physical_base, host_physical_base, root_page_table, readable, writable, executable, accessed, dirty, user, noncacheable, io);
}

auto Environment::map_sv39x4_4k(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool global)  -> bool
{
    return impl_->map_sv39x4_4k(guest_physical_address, host_physical_address, root_page_table, readable, writable, executable, global);
}

auto Environment::map_sv39x4_leaf(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    unsigned leaf_level,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable,
    bool accessed,
    std::optional<bool> dirty,
    bool user,
    bool noncacheable,
    bool io,
    bool global)  -> bool
{
    return impl_->map_sv39x4_leaf(guest_physical_address, host_physical_address, leaf_level, root_page_table, readable, writable, executable, accessed, dirty, user, noncacheable, io, global);
}

auto Environment::map_sv39x4_2m(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable)  -> bool
{
    return impl_->map_sv39x4_2m(guest_physical_address, host_physical_address, root_page_table, readable, writable, executable);
}

auto Environment::map_sv39x4_1g(std::uint64_t guest_physical_address,
    std::uint64_t host_physical_address,
    std::uint64_t root_page_table,
    bool readable,
    bool writable,
    bool executable)  -> bool
{
    return impl_->map_sv39x4_1g(guest_physical_address, host_physical_address, root_page_table, readable, writable, executable);
}

auto Environment::activate_two_stage(std::uint64_t vs_root_page_table,
    std::uint64_t g_root_page_table,
    std::uint16_t asid,
    std::uint16_t vmid)  -> bool
{
    return impl_->activate_two_stage(vs_root_page_table, g_root_page_table, asid, vmid);
}

auto Environment::activate_two_stage_modes(ReferencePageMode vs_mode,
    ReferencePageMode g_mode,
    std::uint64_t vs_root_page_table,
    std::uint64_t g_root_page_table,
    std::uint16_t asid,
    std::uint16_t vmid)  -> bool
{
    return impl_->activate_two_stage_modes(vs_mode, g_mode, vs_root_page_table, g_root_page_table, asid, vmid);
}

auto Environment::update_stage_one_context(ReferencePageMode mode,
    std::uint64_t root_page_table,
    std::uint16_t asid)  -> bool
{
    return impl_->update_stage_one_context(mode, root_page_table, asid);
}

auto Environment::update_vs_context(ReferencePageMode mode,
    std::uint64_t root_page_table,
    std::uint16_t asid)  -> bool
{
    return impl_->update_vs_context(mode, root_page_table, asid);
}

auto Environment::update_two_stage_context(ReferencePageMode vs_mode,
    ReferencePageMode g_mode,
    std::uint64_t vs_root_page_table,
    std::uint64_t g_root_page_table,
    std::uint16_t asid,
    std::uint16_t vmid)  -> bool
{
    return impl_->update_two_stage_context(vs_mode, g_mode, vs_root_page_table, g_root_page_table, asid, vmid);
}

auto Environment::update_g_context(ReferencePageMode mode,
    std::uint64_t root_page_table,
    std::uint16_t vmid)  -> bool
{
    return impl_->update_g_context(mode, root_page_table, vmid);
}

auto Environment::set_translation_permissions(ReferencePrivilegeMode data_privilege,
    bool mxr,
    bool sum,
    bool vmxr,
    bool vsum)  -> bool
{
    return impl_->set_translation_permissions(data_privilege, mxr, sum, vmxr, vsum);
}

auto Environment::set_instruction_privilege(ReferencePrivilegeMode privilege)  -> bool
{
    return impl_->set_instruction_privilege(privilege);
}

auto Environment::set_debug_mode(bool enabled)  -> bool
{
    return impl_->set_debug_mode(enabled);
}

auto Environment::set_hypervisor_access_permissions(ReferencePrivilegeMode spvp,
    bool mxr,
    bool vmxr,
    bool vsum,
    ReferencePrivilegeMode current_privilege)  -> bool
{
    return impl_->set_hypervisor_access_permissions(spvp, mxr, vmxr, vsum, current_privilege);
}

auto Environment::configure_pmp(const std::vector<std::uint64_t> &encoded_addresses,
    const std::vector<std::uint8_t> &config_bytes)  -> bool
{
    return impl_->configure_pmp(encoded_addresses, config_bytes);
}

auto Environment::set_page_based_memory_types(bool machine_enabled,
    bool hypervisor_enabled)  -> bool
{
    return impl_->set_page_based_memory_types(machine_enabled, hypervisor_enabled);
}

auto Environment::set_uncache_write_outstanding(bool enabled)  -> bool
{
    return impl_->set_uncache_write_outstanding(enabled);
}

auto Environment::pulse_sbuffer_flush()  -> bool
{
    return impl_->pulse_sbuffer_flush();
}

auto Environment::drive_l2_flush(bool enable,
    bool done)  -> void
{
    return impl_->drive_l2_flush(enable, done);
}

auto Environment::outer_l2_flush_enabled()  -> bool
{
    return impl_->outer_l2_flush_enabled();
}

auto Environment::backend_l2_flush_done()  -> bool
{
    return impl_->backend_l2_flush_done();
}

auto Environment::l2_flush_checks() const -> std::uint64_t
{
    return impl_->l2_flush_checks();
}

auto Environment::drive_top_controls(std::uint8_t hart_id,
    std::uint64_t reset_vector,
    bool power_down,
    bool cpu_halted,
    bool cpu_critical_error)  -> void
{
    return impl_->drive_top_controls(hart_id, reset_vector, power_down, cpu_halted, cpu_critical_error);
}

auto Environment::backend_hart_id()  -> std::uint8_t
{
    return impl_->backend_hart_id();
}

auto Environment::inner_reset_vector()  -> std::uint64_t
{
    return impl_->inner_reset_vector();
}

auto Environment::outer_power_down_enabled()  -> bool
{
    return impl_->outer_power_down_enabled();
}

auto Environment::outer_cpu_halted()  -> bool
{
    return impl_->outer_cpu_halted();
}

auto Environment::outer_cpu_critical_error()  -> bool
{
    return impl_->outer_cpu_critical_error();
}

auto Environment::top_control_checks() const -> std::uint64_t
{
    return impl_->top_control_checks();
}

auto Environment::drive_top_bridge_stimulus(const TopBridgeStimulus &stimulus)  -> void
{
    return impl_->drive_top_bridge_stimulus(stimulus);
}

auto Environment::top_bridge_checks() const -> std::uint64_t
{
    return impl_->top_bridge_checks();
}

auto Environment::drive_trace_bridge_stimulus(const TraceBridgeStimulus &stimulus)  -> void
{
    return impl_->drive_trace_bridge_stimulus(stimulus);
}

auto Environment::trace_bridge_checks() const -> std::uint64_t
{
    return impl_->trace_bridge_checks();
}

auto Environment::check_dft_bridge_space()  -> bool
{
    return impl_->check_dft_bridge_space();
}

auto Environment::dft_bridge_patterns() const -> std::uint64_t
{
    return impl_->dft_bridge_patterns();
}

auto Environment::dft_bridge_digest() const -> std::uint64_t
{
    return impl_->dft_bridge_digest();
}

auto Environment::set_sbuffer_timeout(std::uint32_t cycles)  -> bool
{
    return impl_->set_sbuffer_timeout(cycles);
}

auto Environment::set_mbmc(bool bitmap_enabled,
    bool confidential_mode,
    std::uint64_t bitmap_base)  -> bool
{
    return impl_->set_mbmc(bitmap_enabled, confidential_mode, bitmap_base);
}

auto Environment::pulse_mbmc_bitmap_clear()  -> bool
{
    return impl_->pulse_mbmc_bitmap_clear();
}

auto Environment::set_pointer_masking(const PointerMaskingConfig &config)  -> bool
{
    return impl_->set_pointer_masking(config);
}

auto Environment::issue_sfence(std::uint64_t address,
    std::uint16_t id,
    bool all_virtual_addresses,
    bool all_contexts,
    bool hypervisor_virtual,
    bool hypervisor_guest,
    bool flush_pipe)  -> bool
{
    return impl_->issue_sfence(address, id, all_virtual_addresses, all_contexts, hypervisor_virtual, hypervisor_guest, flush_pipe);
}

auto Environment::issue_sfence_with_redirect(std::uint8_t redirect_rob_value,
    bool redirect_rob_flag,
    bool redirect_flush_itself,
    std::uint64_t address,
    std::uint16_t id,
    bool all_virtual_addresses,
    bool all_contexts,
    bool hypervisor_virtual,
    bool hypervisor_guest,
    bool flush_pipe)  -> bool
{
    return impl_->issue_sfence_with_redirect(redirect_rob_value, redirect_rob_flag, redirect_flush_itself, address, id, all_virtual_addresses, all_contexts, hypervisor_virtual, hypervisor_guest, flush_pipe);
}

auto Environment::enable_misaligned_accesses(bool load,
    bool store)  -> bool
{
    return impl_->enable_misaligned_accesses(load, store);
}

auto Environment::set_rob_head(std::uint8_t value,
    bool flag)  -> bool
{
    return impl_->set_rob_head(value, flag);
}

auto Environment::pulse_pending_load(std::uint8_t value,
    bool flag)  -> bool
{
    return impl_->pulse_pending_load(value, flag);
}

auto Environment::wait_for_mmio_request(std::uint8_t value,
    bool flag,
    unsigned timeout)  -> bool
{
    return impl_->wait_for_mmio_request(value, flag, timeout);
}

auto Environment::pulse_pending_store(std::uint8_t value,
    bool flag)  -> bool
{
    return impl_->pulse_pending_store(value, flag);
}

auto Environment::pulse_store_commit_keep_pending(std::uint8_t value,
    bool flag)  -> bool
{
    return impl_->pulse_store_commit_keep_pending(value, flag);
}

auto Environment::wait_for_mmio_store_request(std::uint8_t value,
    bool flag,
    unsigned timeout)  -> bool
{
    return impl_->wait_for_mmio_store_request(value, flag, timeout);
}

auto Environment::set_wfi(bool enabled)  -> bool
{
    return impl_->set_wfi(enabled);
}

auto Environment::wfi_safe()  -> bool
{
    return impl_->wfi_safe();
}

auto Environment::require_wfi_unsafe(unsigned cycles)  -> bool
{
    return impl_->require_wfi_unsafe(cycles);
}

auto Environment::run_until_wfi_safe(unsigned timeout)  -> bool
{
    return impl_->run_until_wfi_safe(timeout);
}

auto Environment::enqueue_load(const LoadTransaction &transaction)  -> bool
{
    return impl_->enqueue_load(transaction);
}

auto Environment::enqueue_load_batch(const std::vector<LoadTransaction> &transactions,
    const std::vector<unsigned> &dispatch_lanes)  -> bool
{
    return impl_->enqueue_load_batch(transactions, dispatch_lanes);
}

auto Environment::issue_load(const LoadTransaction &transaction,
    unsigned timeout)  -> bool
{
    return impl_->issue_load(transaction, timeout);
}

auto Environment::issue_load_batch(const std::vector<LoadTransaction> &transactions,
    unsigned timeout,
    bool require_same_cycle)  -> bool
{
    return impl_->issue_load_batch(transactions, timeout, require_same_cycle);
}

auto Environment::expect_load(const LoadTransaction &transaction)  -> void
{
    return impl_->expect_load(transaction);
}

auto Environment::expect_load_data(const LoadTransaction &transaction,
    std::uint64_t data)  -> void
{
    return impl_->expect_load_data(transaction, data);
}

auto Environment::expect_prefetch(const PrefetchTransaction &transaction)  -> void
{
    return impl_->expect_prefetch(transaction);
}

auto Environment::enqueue_prefetch(const PrefetchTransaction &transaction)  -> bool
{
    return impl_->enqueue_prefetch(transaction);
}

auto Environment::enqueue_prefetch_batch(const std::vector<PrefetchTransaction> &transactions,
    const std::vector<unsigned> &dispatch_lanes)  -> bool
{
    return impl_->enqueue_prefetch_batch(transactions, dispatch_lanes);
}

auto Environment::issue_prefetch(const PrefetchTransaction &transaction,
    unsigned timeout)  -> bool
{
    return impl_->issue_prefetch(transaction, timeout);
}

auto Environment::issue_prefetch_batch_same_cycle(const std::vector<PrefetchTransaction> &transactions,
    unsigned timeout)  -> bool
{
    return impl_->issue_prefetch_batch_same_cycle(transactions, timeout);
}

auto Environment::expect_store(const StoreTransaction &transaction)  -> void
{
    return impl_->expect_store(transaction);
}

auto Environment::enqueue_store(const StoreTransaction &transaction) -> bool
{
    return impl_->enqueue_store(transaction);
}

auto Environment::enqueue_store(const StoreTransaction &transaction,
    std::uint8_t lq_value, bool lq_flag) -> bool
{
    return impl_->enqueue_store(transaction, lq_value, lq_flag);
}

auto Environment::enqueue_store_pressure(const StoreTransaction &transaction,
    std::uint8_t lq_value, bool lq_flag) -> bool
{
    return impl_->enqueue_store_pressure(transaction, lq_value, lq_flag);
}

auto Environment::expect_vector(const VectorMemoryTransaction &transaction)  -> void
{
    return impl_->expect_vector(transaction);
}

auto Environment::expect_vector_data(const VectorMemoryTransaction &transaction,
    const std::array<unsigned char, 16> &data)  -> void
{
    return impl_->expect_vector_data(transaction, data);
}

auto Environment::enqueue_vector(const VectorMemoryTransaction &transaction)  -> bool
{
    return impl_->enqueue_vector(transaction);
}

auto Environment::issue_vector(const VectorMemoryTransaction &transaction,
    unsigned timeout)  -> bool
{
    return impl_->issue_vector(transaction, timeout);
}

auto Environment::issue_vector_batch_same_cycle(const std::vector<VectorMemoryTransaction> &transactions,
    unsigned timeout)  -> bool
{
    return impl_->issue_vector_batch_same_cycle(transactions, timeout);
}

auto Environment::issue_load_vector_pair(const LoadTransaction &load,
    const VectorMemoryTransaction &vector,
    unsigned timeout)  -> bool
{
    return impl_->issue_load_vector_pair(load, vector, timeout);
}

auto Environment::issue_store_address(const StoreTransaction &transaction,
    unsigned timeout)  -> bool
{
    return impl_->issue_store_address(transaction, timeout);
}

auto Environment::issue_store_address_batch(const std::vector<StoreTransaction> &transactions,
    unsigned timeout)  -> bool
{
    return impl_->issue_store_address_batch(transactions, timeout);
}

auto Environment::issue_atomic(const AtomicTransaction &transaction,
    unsigned timeout)  -> bool
{
    return impl_->issue_atomic(transaction, timeout);
}

auto Environment::warm_store_translation(const StoreTransaction &transaction,
    unsigned timeout)  -> bool
{
    return impl_->warm_store_translation(transaction, timeout);
}

auto Environment::issue_store_address_until_tlb_hit(const StoreTransaction &transaction,
    unsigned timeout)  -> bool
{
    return impl_->issue_store_address_until_tlb_hit(transaction, timeout);
}

auto Environment::run_until_store_complete_with_replay(const StoreTransaction &transaction,
    unsigned timeout,
    bool hold_pending_store)  -> bool
{
    return impl_->run_until_store_complete_with_replay(transaction, timeout, hold_pending_store);
}

auto Environment::run_until_store_tlb_misses(std::uint64_t target,
    unsigned timeout)  -> bool
{
    return impl_->run_until_store_tlb_misses(target, timeout);
}

auto Environment::run_until_ptw_requests(std::uint64_t target,
    unsigned timeout)  -> bool
{
    return impl_->run_until_ptw_requests(target, timeout);
}

auto Environment::run_until_dcache_requests(std::uint64_t target,
    unsigned timeout)  -> bool
{
    return impl_->run_until_dcache_requests(target, timeout);
}

auto Environment::run_until_dcache_idle(unsigned timeout)  -> bool
{
    return impl_->run_until_dcache_idle(timeout);
}

auto Environment::run_until_dcache_release_idle(unsigned timeout)  -> bool
{
    return impl_->run_until_dcache_release_idle(timeout);
}

auto Environment::run_until_ptw_idle(unsigned timeout)  -> bool
{
    return impl_->run_until_ptw_idle(timeout);
}

auto Environment::wait_for_cmo_store_request(const StoreTransaction &transaction,
    std::uint64_t target,
    unsigned timeout)  -> bool
{
    return impl_->wait_for_cmo_store_request(transaction, target, timeout);
}

auto Environment::run_until_ptw_request_covering(std::uint64_t address,
    std::uint64_t first_request,
    unsigned timeout,
    unsigned min_response_delay)  -> bool
{
    return impl_->run_until_ptw_request_covering(address, first_request, timeout, min_response_delay);
}

auto Environment::run_until_uncache_requests(std::uint64_t target,
    unsigned timeout)  -> bool
{
    return impl_->run_until_uncache_requests(target, timeout);
}

auto Environment::run_until_uncache_drained(std::uint64_t request_target,
    unsigned timeout)  -> bool
{
    return impl_->run_until_uncache_drained(request_target, timeout);
}

auto Environment::issue_store_data(const StoreTransaction &transaction,
    unsigned timeout)  -> bool
{
    return impl_->issue_store_data(transaction, timeout);
}

auto Environment::run_until_store_complete(unsigned timeout)  -> bool
{
    return impl_->run_until_store_complete(timeout);
}

auto Environment::run_until_complete(unsigned timeout)  -> bool
{
    return impl_->run_until_complete(timeout);
}

auto Environment::run_until_load_complete(const LoadTransaction &transaction,
    unsigned timeout)  -> bool
{
    return impl_->run_until_load_complete(transaction, timeout);
}

auto Environment::run_until_load_writebacks(std::uint64_t target,
    unsigned timeout)  -> bool
{
    return impl_->run_until_load_writebacks(target, timeout);
}

auto Environment::run_until_vector_complete(unsigned timeout)  -> bool
{
    return impl_->run_until_vector_complete(timeout);
}

auto Environment::run_until_vector_complete_with_replays(const VectorMemoryTransaction &transaction,
    unsigned timeout,
    bool pulse_pending_store_after_replay,
    bool hold_pending_store)  -> bool
{
    return impl_->run_until_vector_complete_with_replays(transaction, timeout, pulse_pending_store_after_replay, hold_pending_store);
}

auto Environment::run_until_vector_complete_with_replays(const std::vector<VectorMemoryTransaction> &transactions,
    unsigned timeout,
    bool pulse_pending_store_after_replay)  -> bool
{
    return impl_->run_until_vector_complete_with_replays(transactions, timeout, pulse_pending_store_after_replay);
}

auto Environment::run_until_all_complete(unsigned timeout)  -> bool
{
    return impl_->run_until_all_complete(timeout);
}

auto Environment::run_until_lq_retired(unsigned timeout)  -> bool
{
    return impl_->run_until_lq_retired(timeout);
}

auto Environment::account_lq_cancellation(unsigned count)  -> bool
{
    return impl_->account_lq_cancellation(count);
}

auto Environment::account_sq_cancellation(unsigned count)  -> bool
{
    return impl_->account_sq_cancellation(count);
}

auto Environment::run_until_queues_retired(unsigned timeout)  -> bool
{
    return impl_->run_until_queues_retired(timeout);
}

auto Environment::commit_store(const StoreTransaction &transaction,
    unsigned timeout)  -> bool
{
    return impl_->commit_store(transaction, timeout);
}

auto Environment::commit_vector_store(const VectorMemoryTransaction &transaction,
    unsigned timeout)  -> bool
{
    return impl_->commit_vector_store(transaction, timeout);
}

auto Environment::record_committed_store(const StoreTransaction &transaction)  -> void
{
    return impl_->record_committed_store(transaction);
}

auto Environment::record_atomic_result(std::uint64_t address,
    std::uint64_t value, bool cacheable, bool identity_mapped)  -> void
{
    return impl_->record_atomic_result(
        address, value, cacheable, identity_mapped);
}

auto Environment::record_committed_vector_store(const VectorMemoryTransaction &transaction)  -> void
{
    return impl_->record_committed_vector_store(transaction);
}

auto Environment::commit_stores_through(const StoreTransaction &transaction,
    unsigned count)  -> bool
{
    return impl_->commit_stores_through(transaction, count);
}

auto Environment::run_until_sq_dequeued(std::uint64_t target,
    unsigned timeout)  -> bool
{
    return impl_->run_until_sq_dequeued(target, timeout);
}

auto Environment::run_until_release_data(unsigned timeout)  -> bool
{
    return impl_->run_until_release_data(timeout);
}

auto Environment::run_until_release_data_count(std::uint64_t target,
    unsigned timeout)  -> bool
{
    return impl_->run_until_release_data_count(target, timeout);
}

auto Environment::run_until_expected_release_data_count(std::uint64_t target,
    unsigned timeout)  -> bool
{
    return impl_->run_until_expected_release_data_count(target, timeout);
}

auto Environment::run_until_probe_responses(std::uint64_t target,
    unsigned timeout)  -> bool
{
    return impl_->run_until_probe_responses(target, timeout);
}

auto Environment::check_idle(unsigned cycles)  -> bool
{
    return impl_->check_idle(cycles);
}

auto Environment::run_cycles(unsigned cycles)  -> bool
{
    return impl_->run_cycles(cycles);
}

auto Environment::redirect_after(std::uint8_t rob_value,
    bool rob_flag,
    bool flush_itself,
    bool is_vls_exception)  -> bool
{
    return impl_->redirect_after(rob_value, rob_flag, flush_itself, is_vls_exception);
}

auto Environment::ok() const -> bool
{
    return impl_->ok();
}

auto Environment::error() const -> const std::string &
{
    return impl_->error();
}

auto Environment::failures() const -> const std::vector<Failure> &
{
    return impl_->failures();
}

std::string Environment::failure_report() const
{
    return impl_->failure_report();
}

} // namespace memblock

namespace memblock {
auto Environment::checkpoint_dcache_alias_reference(std::uint64_t base,
    std::uint64_t bytes) -> bool
{
    return impl_->checkpoint_dcache_alias_reference(base, bytes);
}
auto Environment::dcache_alias_recalls() const -> std::uint64_t
{
    return impl_->dcache_alias_recalls();
}
auto Environment::dcache_alias_recall_responses() const -> std::uint64_t
{
    return impl_->dcache_alias_recall_responses();
}
} // namespace memblock
