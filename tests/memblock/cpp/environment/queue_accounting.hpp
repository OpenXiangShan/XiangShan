#pragma once
#include "memblock_env.hpp"
#include "dut_adapter.hpp"

namespace memblock {
// Owns queue conservation and transaction-to-retirement targets. The driver
// records accepted allocations; this monitor independently samples enqueues.
class QueueAccounting {
public:
    explicit QueueAccounting(FailureState &error) : error_(error) {}
    void sample_before_edge(DutAdapter &dut_);
    void sample_after_edge(DutAdapter &dut_);
    void check_driver_accounting();
    std::optional<bool> has_capacity(unsigned lq_needed, unsigned sq_needed);
    void allocate_load(std::uint64_t count) { lq_allocated_ += count; }
    void allocate_scalar_store(std::uint64_t key);
    void allocate_vector_store(std::uint64_t key, std::uint64_t count);
    bool account_lq_cancellation(unsigned count);
    bool account_sq_cancellation(unsigned count);
    bool observe_redirect_cancellation(std::uint64_t lq_canceled, std::uint64_t sq_canceled);
    std::optional<std::uint64_t> scalar_store_target(std::uint64_t key) const;
    std::optional<std::uint64_t> vector_store_target(std::uint64_t key) const;
    void forget_scalar_store(std::uint64_t key) { scalar_store_sq_targets_.erase(key); }
    void forget_vector_store(std::uint64_t key) { vector_store_sq_targets_.erase(key); }
    std::uint64_t lq_allocated() const { return lq_allocated_; }
    std::uint64_t lq_enqueued_observed() const { return lq_enqueued_observed_; }
    std::uint64_t lq_dequeued() const { return lq_dequeued_; }
    std::uint64_t lq_canceled() const { return lq_canceled_; }
    std::uint64_t lq_redirect_canceled_observed() const { return lq_redirect_canceled_observed_; }
    std::uint64_t lq_canceled_unobserved() const { return lq_canceled_unobserved_; }
    std::uint64_t sq_allocated() const { return sq_allocated_; }
    std::uint64_t sq_enqueued_observed() const { return sq_enqueued_observed_; }
    std::uint64_t sq_dequeued() const { return sq_dequeued_; }
    std::uint64_t sq_canceled() const { return sq_canceled_; }
    std::uint64_t sq_redirect_canceled_observed() const { return sq_redirect_canceled_observed_; }
    std::uint64_t sq_canceled_unobserved() const { return sq_canceled_unobserved_; }
    std::uint64_t redirect_cancellation_events_observed() const { return redirect_cancellation_events_observed_; }
    const auto &lsq_enqueue_widths_observed() const { return lsq_enqueue_widths_observed_; }
    const auto &lsq_enqueue_lanes_observed() const { return lsq_enqueue_lanes_observed_; }
private:
    FailureState &error_;
    std::uint64_t lq_allocated_ = 0;
    std::uint64_t lq_enqueued_observed_ = 0;
    std::uint64_t lq_dequeued_ = 0;
    std::uint64_t lq_canceled_ = 0;
    std::uint64_t lq_redirect_canceled_observed_ = 0;
    std::uint64_t lq_canceled_unobserved_ = 0;
    std::uint64_t sq_allocated_ = 0;
    std::uint64_t sq_enqueued_observed_ = 0;
    std::uint64_t sq_dequeued_ = 0;
    std::uint64_t sq_canceled_ = 0;
    std::uint64_t sq_redirect_canceled_observed_ = 0;
    std::uint64_t sq_canceled_unobserved_ = 0;
    std::uint64_t redirect_cancellation_events_observed_ = 0;
    std::array<std::uint64_t, generated::kLsqEnqueueLanes>
        lsq_enqueue_widths_observed_{};
    std::array<std::uint64_t, generated::kLsqEnqueueLanes>
        lsq_enqueue_lanes_observed_{};
    std::unordered_map<std::uint64_t, std::uint64_t>
        scalar_store_sq_targets_;
    std::unordered_map<std::uint64_t, std::uint64_t>
        vector_store_sq_targets_;
};
} // namespace memblock
