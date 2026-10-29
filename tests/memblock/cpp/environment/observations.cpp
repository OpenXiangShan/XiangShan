#include "implementation.hpp"

namespace memblock {

auto Environment::Impl::cycle() const -> std::uint64_t
{ return dut_.cycle(); }

auto Environment::Impl::tilelink_requests() const -> std::uint64_t
{ return memory_agent_.request_count(); }

auto Environment::Impl::dcache_last_request_address() const -> std::uint64_t
{
    return memory_agent_.last_request_address();
}

auto Environment::Impl::dcache_request_classes() const -> const std::array<std::uint64_t, 7> &
{
    return memory_agent_.request_classes();
}

auto Environment::Impl::dcache_request_summary_since(std::uint64_t first_request,
    unsigned maximum_records) const -> std::string
{
    return memory_agent_.request_summary_since(first_request, maximum_records);
}

auto Environment::Impl::dcache_faulting_load_requests_error_since(std::uint64_t target_line,
    bool target_read_allowed,
    std::uint64_t first_request) const -> const char *
{
    return memory_agent_.faulting_load_requests_error_since(
        target_line, target_read_allowed, first_request);
}

auto Environment::Impl::dcache_requests_covering_since(std::uint64_t address,
    std::uint64_t first_request) const -> std::uint64_t
{
    return memory_agent_.request_covering_count_since(
        address, first_request);
}

auto Environment::Impl::dcache_store_ntot_line_count_since(std::uint64_t line,
    std::uint64_t first_request) const -> std::uint64_t
{
    return memory_agent_.store_ntot_line_count_since(line, first_request);
}

auto Environment::Impl::dcache_requests_from_source_since(std::uint8_t request_source,
    std::uint64_t first_request) const -> std::uint64_t
{
    return memory_agent_.request_source_count_since(
        request_source, first_request);
}

auto Environment::Impl::dcache_requests_from_source_covering_since(std::uint8_t request_source,
    std::uint64_t address,
    std::uint64_t first_request) const -> std::uint64_t
{
    return memory_agent_.request_source_covering_count_since(
        request_source, address, first_request);
}

auto Environment::Impl::begin_dcache_release_observation()  -> void
{
    memory_agent_.begin_release_observation();
}

auto Environment::Impl::end_dcache_release_observation()  -> void
{
    memory_agent_.end_release_observation();
}

auto Environment::Impl::observed_dcache_releases_covering(std::uint64_t address) const -> std::uint64_t
{
    return memory_agent_.observed_release_covering_count(address);
}

auto Environment::Impl::observed_dcache_release_data_covering(std::uint64_t address) const -> std::uint64_t
{
    return memory_agent_.observed_release_covering_count(address, true);
}

auto Environment::Impl::observed_dcache_stalled_releases_covering(std::uint64_t address,
    bool data_only) const -> std::uint64_t
{
    return memory_agent_.observed_stalled_release_covering_count(
        address, data_only);
}

auto Environment::Impl::observed_dcache_release_stall_cycles_covering(std::uint64_t address,
    bool data_only) const -> std::uint64_t
{
    return memory_agent_.observed_release_stall_cycles_covering(
        address, data_only);
}

auto Environment::Impl::dcache_gets() const -> std::uint64_t
{ return memory_agent_.get_count(); }

auto Environment::Impl::dcache_refills() const -> std::uint64_t
{ return memory_agent_.refill_count(); }

auto Environment::Impl::dcache_keyword_refills() const -> std::uint64_t
{
    return memory_agent_.keyword_refill_count();
}

auto Environment::Impl::dcache_nonkeyword_refills() const -> std::uint64_t
{
    return memory_agent_.nonkeyword_refill_count();
}

auto Environment::Impl::dcache_acquire_perms() const -> std::uint64_t
{
    return memory_agent_.acquire_perm_count();
}

auto Environment::Impl::dcache_cmo_requests(StoreOp operation) const -> std::uint64_t
{
    return memory_agent_.cmo_count(cmo_operation_index(operation));
}

auto Environment::Impl::dcache_last_cmo_address() const -> std::uint64_t
{
    return memory_agent_.last_cmo_address();
}

auto Environment::Impl::dcache_last_cmo_source() const -> std::uint8_t
{
    return memory_agent_.last_cmo_source();
}

auto Environment::Impl::tilelink_releases() const -> std::uint64_t
{ return memory_agent_.release_count(); }

auto Environment::Impl::tilelink_release_data() const -> std::uint64_t
{
    return memory_agent_.release_data_count();
}

auto Environment::Impl::tilelink_release_data_verified() const -> std::uint64_t
{
    return memory_agent_.release_data_verified_count();
}

auto Environment::Impl::tilelink_expected_release_data_verified() const -> std::uint64_t
{
    return memory_agent_.expected_release_data_verified_count();
}

auto Environment::Impl::dcache_probes() const -> std::uint64_t
{
    return memory_agent_.probe_request_count();
}

auto Environment::Impl::dcache_probe_responses() const -> std::uint64_t
{
    return memory_agent_.probe_response_count();
}

auto Environment::Impl::dcache_probe_data() const -> std::uint64_t
{
    return memory_agent_.probe_data_count();
}

auto Environment::Impl::dcache_probe_sources() const -> std::uint64_t
{
    return memory_agent_.probe_source_count();
}

auto Environment::Impl::dcache_probe_source_reuses() const -> std::uint64_t
{
    return memory_agent_.probe_source_reuse_count();
}

auto Environment::Impl::dcache_probe_source_wraps() const -> std::uint64_t
{
    return memory_agent_.probe_source_wrap_count();
}

auto Environment::Impl::dcache_max_probe_outstanding() const -> std::uint64_t
{
    return memory_agent_.max_probe_outstanding();
}

auto Environment::Impl::dcache_probe_stalls() const -> std::uint64_t
{
    return memory_agent_.probe_stall_cycles();
}

auto Environment::Impl::dcache_grant_acks() const -> std::uint64_t
{
    return memory_agent_.grant_ack_count();
}

auto Environment::Impl::dcache_grant_data_beats() const -> std::uint64_t
{
    return memory_agent_.grant_data_beat_count();
}

auto Environment::Impl::dcache_denied_d_beats() const -> std::uint64_t
{
    return memory_agent_.denied_d_beat_count();
}

auto Environment::Impl::dcache_corrupt_d_beats() const -> std::uint64_t
{
    return memory_agent_.corrupt_d_beat_count();
}

auto Environment::Impl::dcache_error_response_requests() const -> std::uint64_t
{
    return memory_agent_.error_response_request_count();
}

auto Environment::Impl::dcache_error_response_refills() const -> std::uint64_t
{
    return memory_agent_.error_response_refill_count();
}

auto Environment::Impl::dcache_error_response_grant_acks() const -> std::uint64_t
{
    return memory_agent_.error_response_grant_ack_count();
}

auto Environment::Impl::dcache_last_error_response_address() const -> std::uint64_t
{
    return memory_agent_.last_error_response_address();
}

auto Environment::Impl::dcache_last_error_response_keyword() const -> bool
{
    return memory_agent_.last_error_response_keyword();
}

auto Environment::Impl::dcache_max_outstanding_requests() const -> std::uint64_t
{
    return memory_agent_.max_outstanding_requests();
}

auto Environment::Impl::dcache_outstanding_requests() const -> std::uint64_t
{
    return memory_agent_.outstanding_requests();
}

auto Environment::Impl::dcache_grant_ack_stalls() const -> std::uint64_t
{
    return memory_agent_.grant_ack_stall_cycles();
}

auto Environment::Impl::dcache_c_payload_stability_checks() const -> std::uint64_t
{
    return memory_agent_.c_payload_stability_checks();
}

auto Environment::Impl::dcache_release_target_payload_stability_checks() const -> std::uint64_t
{
    return memory_agent_.release_target_payload_stability_checks();
}

auto Environment::Impl::dcache_grants_drained() const -> bool
{
    return memory_agent_.grant_acks_idle();
}

auto Environment::Impl::dcache_responses_idle() const -> bool
{
    return memory_agent_.responses_idle();
}

auto Environment::Impl::dcache_release_channel_idle() const -> bool
{
    return memory_agent_.release_channel_idle();
}

auto Environment::Impl::ptw_requests() const -> std::uint64_t
{ return ptw_agent_.request_count(); }

auto Environment::Impl::ptw_error_response_requests() const -> std::uint64_t
{
    return ptw_agent_.error_response_requests();
}

auto Environment::Impl::ptw_denied_d_beats() const -> std::uint64_t
{
    return ptw_agent_.denied_d_beats();
}

auto Environment::Impl::ptw_corrupt_d_beats() const -> std::uint64_t
{
    return ptw_agent_.corrupt_d_beats();
}

auto Environment::Impl::ptw_last_error_response_address() const -> std::uint64_t
{
    return ptw_agent_.last_error_response_address();
}

auto Environment::Impl::ptw_last_error_response_source() const -> std::uint8_t
{
    return ptw_agent_.last_error_response_source();
}

auto Environment::Impl::ptw_requests_covering_since(std::uint64_t address,
    std::uint64_t first_request) const -> std::uint64_t
{
    return ptw_agent_.request_covering_count_since(address, first_request);
}

auto Environment::Impl::pending_ifetch_ptw_requests() const -> std::uint64_t
{
    return ifetch_ptw_pending_;
}

auto Environment::Impl::dcache_request_stalls() const -> std::uint64_t
{
    return memory_agent_.request_stall_cycles();
}

auto Environment::Impl::dcache_response_delays() const -> std::uint64_t
{
    return memory_agent_.response_delay_cycles();
}

auto Environment::Impl::dcache_response_latency_stats() const -> const ResponseLatencyStats &
{
    return memory_agent_.response_latency_stats();
}

auto Environment::Impl::ptw_request_stalls() const -> std::uint64_t
{
    return ptw_agent_.request_stall_cycles();
}

auto Environment::Impl::ptw_response_delays() const -> std::uint64_t
{
    return ptw_agent_.response_delay_cycles();
}

auto Environment::Impl::ptw_max_outstanding_requests() const -> std::uint64_t
{
    return ptw_agent_.max_outstanding_requests();
}

auto Environment::Impl::ptw_outstanding_requests() const -> std::uint64_t
{
    return ptw_agent_.outstanding_requests();
}

auto Environment::Impl::ptw_response_latency_stats() const -> const ResponseLatencyStats &
{
    return ptw_agent_.response_latency_stats();
}

auto Environment::Impl::exception_vaddr()  -> std::uint64_t
{
    return dut_.queues.observation.vaddr.value();
}

auto Environment::Impl::select_store_exception_address(bool store)  -> void
{
    dut_.backend.input.is_store_exception.set(store);
}

auto Environment::Impl::exception_gpaddr()  -> std::uint64_t
{
    return dut_.queues.observation.gpaddr.value();
}

auto Environment::Impl::exception_is_for_vs_nonleaf_pte()  -> bool
{
    return dut_.queues.observation.is_for_v_snon_leaf_pte.boolean();
}

auto Environment::Impl::uncache_requests() const -> std::uint64_t
{
    return uncache_agent_.request_count();
}

auto Environment::Impl::uncache_request_stalls() const -> std::uint64_t
{
    return uncache_agent_.request_stall_cycles();
}

auto Environment::Impl::uncache_response_delays() const -> std::uint64_t
{
    return uncache_agent_.response_delay_cycles();
}

auto Environment::Impl::uncache_max_outstanding_requests() const -> std::uint64_t
{
    return uncache_agent_.max_outstanding_requests();
}

auto Environment::Impl::uncache_outstanding_requests() const -> std::uint64_t
{
    return uncache_agent_.outstanding_requests();
}

auto Environment::Impl::uncache_error_response_requests() const -> std::uint64_t
{
    return uncache_agent_.error_response_requests();
}

auto Environment::Impl::uncache_denied_d_beats() const -> std::uint64_t
{
    return uncache_agent_.denied_d_beat_count();
}

auto Environment::Impl::uncache_corrupt_d_beats() const -> std::uint64_t
{
    return uncache_agent_.corrupt_d_beat_count();
}

auto Environment::Impl::uncache_response_latency_stats() const -> const ResponseLatencyStats &
{
    return uncache_agent_.response_latency_stats();
}

auto Environment::Impl::store_mmio_valid()  -> bool
{
    return dut_.queues.observation.store_mmio.boolean();
}

auto Environment::Impl::store_mmio_rob()  -> std::uint8_t
{
    return static_cast<std::uint8_t>(
        dut_.queues.observation.store_mmio_uop_rob_idx_value.value());
}

auto Environment::Impl::store_tlb_feedbacks() const -> std::uint64_t
{ return feedback_.store_tlb_feedbacks(); }

auto Environment::Impl::store_tlb_misses() const -> std::uint64_t
{ return feedback_.store_tlb_misses(); }

auto Environment::Impl::lq_allocated() const -> std::uint64_t
{ return queues_.lq_allocated(); }

auto Environment::Impl::lq_enqueued_observed() const -> std::uint64_t
{
    return queues_.lq_enqueued_observed();
}

auto Environment::Impl::lq_dequeued() const -> std::uint64_t
{ return queues_.lq_dequeued(); }

auto Environment::Impl::lq_canceled() const -> std::uint64_t
{ return queues_.lq_canceled(); }

auto Environment::Impl::lq_redirect_canceled_observed() const -> std::uint64_t
{
    return queues_.lq_redirect_canceled_observed();
}

auto Environment::Impl::lq_canceled_unobserved() const -> std::uint64_t
{
    return queues_.lq_canceled_unobserved();
}

auto Environment::Impl::sq_allocated() const -> std::uint64_t
{ return queues_.sq_allocated(); }

auto Environment::Impl::sq_enqueued_observed() const -> std::uint64_t
{
    return queues_.sq_enqueued_observed();
}

auto Environment::Impl::sq_dequeued() const -> std::uint64_t
{ return queues_.sq_dequeued(); }

auto Environment::Impl::sq_canceled() const -> std::uint64_t
{ return queues_.sq_canceled(); }

auto Environment::Impl::sq_redirect_canceled_observed() const -> std::uint64_t
{
    return queues_.sq_redirect_canceled_observed();
}

auto Environment::Impl::sq_canceled_unobserved() const -> std::uint64_t
{
    return queues_.sq_canceled_unobserved();
}

auto Environment::Impl::redirect_cancellation_events_observed() const -> std::uint64_t
{
    return queues_.redirect_cancellation_events_observed();
}

auto Environment::Impl::lsq_enqueue_widths_observed() const -> const std::array<std::uint64_t, generated::kLsqEnqueueLanes> &
{
    return queues_.lsq_enqueue_widths_observed();
}

auto Environment::Impl::lsq_enqueue_lanes_observed() const -> const std::array<std::uint64_t, generated::kLsqEnqueueLanes> &
{
    return queues_.lsq_enqueue_lanes_observed();
}

auto Environment::Impl::lsq_enqueue_cadence() const -> const std::array<std::uint64_t, 6> &
{
    return queues_.lsq_enqueue_cadence();
}

auto Environment::Impl::writebacks() const -> std::uint64_t
{ return scoreboard_.observed(); }

auto Environment::Impl::prefetch_writebacks() const -> std::uint64_t
{
    return scoreboard_.prefetch_observed();
}

auto Environment::Impl::store_writebacks() const -> std::uint64_t
{ return store_scoreboard_.observed(); }

auto Environment::Impl::vector_load_writebacks() const -> std::uint64_t
{
    return vector_scoreboard_.load_observed();
}

auto Environment::Impl::vector_store_writebacks() const -> std::uint64_t
{
    return vector_scoreboard_.store_observed();
}

auto Environment::Impl::vector_fof_fix_writebacks() const -> std::uint64_t
{
    return vector_scoreboard_.fof_fix_observed();
}

auto Environment::Impl::vector_page_fault_writebacks() const -> std::uint64_t
{
    return vector_scoreboard_.page_fault_observed();
}

auto Environment::Impl::pending_scalar_loads() const -> std::size_t
{ return scoreboard_.pending_load(); }

auto Environment::Impl::scalar_load_pending(const LoadTransaction &transaction) const -> bool
{
    return scoreboard_.pending(transaction);
}

auto Environment::Impl::scalar_store_pending(const StoreTransaction &transaction) const -> bool
{
    return store_scoreboard_.pending(transaction);
}

auto Environment::Impl::scalar_store_address_pending(const StoreTransaction &transaction) const -> bool
{ return store_scoreboard_.address_pending(transaction); }

auto Environment::Impl::prefetch_pending(const PrefetchTransaction &transaction) const -> bool
{
    return scoreboard_.pending(transaction);
}

auto Environment::Impl::vector_memory_pending(const VectorMemoryTransaction &transaction) const -> bool
{ return vector_scoreboard_.pending(transaction); }

auto Environment::Impl::cancel_scalar_load_for_reset(const LoadTransaction &transaction)  -> bool
{
    // Cancel only at the completed reset boundary. A normal functional
    // tick or a failed reset cannot justify dropping an obligation.
    if (!last_reset_completed_cycle_ ||
        *last_reset_completed_cycle_ != cycle()) {
        error_.record("scalar reset cancellation requires a just-completed reset");
        return false;
    }
    return scoreboard_.cancel_for_reset(transaction) && check_components();
}

auto Environment::Impl::cancel_scalar_load_after_redirect(const LoadTransaction &transaction) -> bool
{
    if (!scalar_redirect_boundary_ || scalar_redirect_boundary_->cycle != cycle() ||
        scalar_redirect_boundary_->remaining == 0) {
        error_.record("scalar redirect cancellation requires a just-observed LQ cancellation");
        return false;
    }
    const auto &boundary = *scalar_redirect_boundary_;
    if (!scoreboard_.cancel_for_redirect(transaction, boundary.rob, boundary.flag,
            boundary.flush_itself) || !check_components()) return false;
    --scalar_redirect_boundary_->remaining;
    return true;
}

auto Environment::Impl::scalar_load_redirect_canceled() const -> std::uint64_t
{ return scoreboard_.redirect_canceled(); }

auto Environment::Impl::scalar_load_reset_canceled() const -> std::uint64_t
{
    return scoreboard_.reset_canceled();
}

auto Environment::Impl::scalar_load_canceled_observed() const -> std::uint64_t
{
    return scoreboard_.canceled_observed();
}

auto Environment::Impl::cancel_scalar_store_for_reset(const StoreTransaction &transaction)  -> bool
{
    // Cancel only at the completed reset boundary. A normal functional tick
    // or a failed reset cannot justify dropping an outstanding store.
    if (!last_reset_completed_cycle_ ||
        *last_reset_completed_cycle_ != cycle()) {
        error_.record("scalar store reset cancellation requires a just-completed reset");
        return false;
    }
    return store_scoreboard_.cancel_for_reset(transaction) && check_components();
}

auto Environment::Impl::scalar_store_reset_canceled() const -> std::uint64_t
{
    return store_scoreboard_.reset_canceled();
}

auto Environment::Impl::pending_prefetches() const -> std::size_t
{ return scoreboard_.pending_prefetch(); }

auto Environment::Impl::pending_scalar_stores() const -> std::size_t
{ return store_scoreboard_.pending(); }

auto Environment::Impl::pending_vector_loads() const -> std::size_t
{
    return vector_scoreboard_.pending_loads();
}

auto Environment::Impl::pending_vector_stores() const -> std::size_t
{
    return vector_scoreboard_.pending_stores();
}

auto Environment::Impl::vector_replay_feedbacks() const -> std::uint64_t
{
    return feedback_.vector_replay_feedbacks();
}

auto Environment::Impl::pin_space_digest() const -> std::uint64_t
{ return pin_space_digest_; }

auto Environment::Impl::frontend_bridge_stats() const -> const FrontendBridgeStats &
{
    return frontend_bridge_stats_;
}

auto Environment::Impl::scalar_load_feedback_stats() const -> const ScalarLoadFeedbackStats &
{
    return feedback_.scalar_load_feedback_stats();
}

auto Environment::Impl::load_mmio_stats() const -> const LoadMmioStats &
{
    return feedback_.load_mmio_stats();
}

auto Environment::Impl::bus_error_stats() const -> const BusErrorStats &
{ return feedback_.bus_error_stats(); }

auto Environment::Impl::top_down_stats() const -> const TopDownStats &
{ return bridge_.top_down_stats(); }

auto Environment::Impl::iq_slow_feedback_stats() const -> const IqSlowFeedbackStats &
{
    return feedback_.iq_slow_feedback_stats();
}

auto Environment::Impl::memory_violation_stats() const -> const MemoryViolationStats &
{
    return feedback_.memory_violation_stats();
}

auto Environment::Impl::ifetch_prefetch_stats() const -> const IfetchPrefetchStats &
{
    return feedback_.ifetch_prefetch_stats();
}

auto Environment::Impl::hardware_prefetch_stats() const -> const HardwarePrefetchStats &
{
    return feedback_.hardware_prefetch_stats();
}

auto Environment::Impl::sbuffer_empty()  -> bool
{
    dut_.refresh_comb();
    return dut_.sample_sbuffer_empty();
}

auto Environment::Impl::drive_top_down_misses(bool l2_miss,
    bool l3_miss)  -> void
{
    dut_.top_down.input.l2_miss.set(l2_miss);
    dut_.top_down.input.l3_miss.set(l3_miss);
}

auto Environment::Impl::run_until_sbuffer_empty(unsigned timeout)  -> bool
{
    for (unsigned cycle = 0; cycle < timeout && !sbuffer_empty(); ++cycle) {
        tick();
        if (!check_components()) {
            return false;
        }
    }
    if (!sbuffer_empty()) {
        error_.record("timed out waiting for SBuffer to empty");
        return false;
    }
    return check_components();
}

} // namespace memblock

namespace memblock {
auto Environment::Impl::checkpoint_dcache_alias_reference(std::uint64_t base,
    std::uint64_t bytes) -> bool
{
    if (!sbuffer_empty()) {
        error_.record("DCache alias checkpoint requires an empty SBuffer");
        return false;
    }
    return memory_agent_.checkpoint_alias_reference(base, bytes);
}
auto Environment::Impl::dcache_alias_recalls() const -> std::uint64_t
{
    return memory_agent_.alias_recalls();
}
auto Environment::Impl::dcache_alias_recall_responses() const -> std::uint64_t
{
    return memory_agent_.alias_recall_responses();
}
} // namespace memblock
