#include "queue_accounting.hpp"

namespace memblock {
void QueueAccounting::sample_before_edge(DutAdapter &dut_)
{
    unsigned lsq_enqueue_width = 0;
    for (unsigned lane = 0; lane < generated::kLsqEnqueueLanes;
         ++lane) {
        const auto enqueue =
            dut_.sample_lsq_enqueue(lane);
        if (!enqueue.valid) {
            continue;
        }
        ++lsq_enqueue_width;
        ++lsq_enqueue_lanes_observed_[lane];
        if ((enqueue.need_alloc != 1 && enqueue.need_alloc != 2) ||
            enqueue.num_ls_elem == 0) {
            std::ostringstream message;
            message << "invalid accepted LSQ enqueue on lane " << lane
                    << " need_alloc="
                    << static_cast<unsigned>(enqueue.need_alloc)
                    << " num_ls_elem="
                    << static_cast<unsigned>(enqueue.num_ls_elem);
            error_.record(message.str());
            continue;
        }
        if (enqueue.need_alloc == 1) {
            lq_enqueued_observed_ += enqueue.num_ls_elem;
        } else {
            sq_enqueued_observed_ += enqueue.num_ls_elem;
        }
    }
    if (lsq_enqueue_width != 0) {
        ++lsq_enqueue_widths_observed_.at(lsq_enqueue_width - 1);
        const bool older_lq = lq_allocated_ > lq_dequeued_ + lq_canceled_;
        const bool older_sq = sq_allocated_ > sq_dequeued_ + sq_canceled_;
        const bool adjacent = last_enqueue_edge_ &&
            *last_enqueue_edge_ + 1 == completed_edges_;
        ++lsq_enqueue_cadence_[0];
        lsq_enqueue_cadence_[1] += adjacent;
        lsq_enqueue_cadence_[2] += adjacent && (older_lq || older_sq);
        lsq_enqueue_cadence_[3] += older_lq;
        lsq_enqueue_cadence_[4] += older_sq;
        enqueue_run_ = adjacent ? enqueue_run_ + 1 : 1;
        lsq_enqueue_cadence_[5] = std::max(lsq_enqueue_cadence_[5], enqueue_run_);
        last_enqueue_edge_ = completed_edges_;
    }
}

void QueueAccounting::stage_admission_credit(std::uint64_t lq, std::uint64_t sq)
{
    if (lq != 0 || sq != 0) {
        admission_credits_.push_back({completed_edges_ + kLsqAdmissionCreditDelay, lq, sq});
    }
}

void QueueAccounting::sample_after_edge(DutAdapter &dut_, bool redirect)
{
    ++completed_edges_;
    const auto lq_dequeued = dut_.queues.retirement.lq_deq.value();
    const auto sq_dequeued = dut_.queues.retirement.sq_deq.value();
    lq_dequeued_ += lq_dequeued;
    sq_dequeued_ += sq_dequeued;
    stage_admission_credit(lq_dequeued, sq_dequeued);
    while (!admission_credits_.empty() &&
           admission_credits_.front().ready_edge <= completed_edges_) {
        lq_admission_retired_ += admission_credits_.front().lq;
        sq_admission_retired_ += admission_credits_.front().sq;
        admission_credits_.pop_front();
    }
    if (redirect) {
        admission_blocked_until_ = completed_edges_ + kLsqRedirectRecoveryEdges;
    }
}

void QueueAccounting::check_driver_accounting()
{
    if (lq_enqueued_observed_ != lq_allocated_ ||
        sq_enqueued_observed_ != sq_allocated_) {
        std::ostringstream message;
        message << "LSQ enqueue monitor disagrees with driver accounting"
                << " lq_observed=" << lq_enqueued_observed_
                << " lq_expected=" << lq_allocated_
                << " sq_observed=" << sq_enqueued_observed_
                << " sq_expected=" << sq_allocated_;
        error_.record(message.str());
    }
}

std::optional<bool> QueueAccounting::has_capacity(unsigned lq_needed, unsigned sq_needed)
{
    const std::uint64_t lq_retired = lq_dequeued_ + lq_canceled_;
    const std::uint64_t sq_retired = sq_dequeued_ + sq_canceled_;
    if (lq_retired > lq_allocated_ || sq_retired > sq_allocated_) {
        error_.record("LSQ software accounting moved past allocation");
        return std::nullopt;
    }
    // Correctness uses immediate retirement; admission only reuses matured
    // public credits. This permits back-to-back inputs using existing space.
    const std::uint64_t lq_outstanding = lq_allocated_ - lq_admission_retired_;
    const std::uint64_t sq_outstanding = sq_allocated_ - sq_admission_retired_;
    const bool capacity_ok = completed_edges_ >= admission_blocked_until_ &&
        lq_outstanding + lq_needed <=
            kVirtualLoadQueueEntries - kLqEnqueueHeadroom &&
        sq_outstanding + sq_needed <=
            kStoreQueueEntries - kSqEnqueueHeadroom;
    return capacity_ok;
}

bool QueueAccounting::observe_redirect_cancellation(std::uint64_t lq_canceled, std::uint64_t sq_canceled)
{
    if (lq_dequeued_ + lq_canceled_ + lq_canceled > lq_allocated_ ||
        sq_dequeued_ + sq_canceled_ + sq_canceled > sq_allocated_) {
        std::ostringstream message;
        message << "observed redirect cancellation exceeds queue allocation"
                << " lq=" << lq_dequeued_ << '+' << lq_canceled_ << '+'
                << lq_canceled << '/' << lq_allocated_
                << " sq=" << sq_dequeued_ << '+' << sq_canceled_ << '+'
                << sq_canceled << '/' << sq_allocated_;
        error_.record(message.str());
        return false;
    }
    ++redirect_cancellation_events_observed_;
    lq_redirect_canceled_observed_ += lq_canceled;
    sq_redirect_canceled_observed_ += sq_canceled;
    lq_canceled_ += lq_canceled;
    sq_canceled_ += sq_canceled;
    stage_admission_credit(lq_canceled, sq_canceled);
    return true;
}

auto QueueAccounting::account_lq_cancellation(unsigned count)  -> bool
{
    if (lq_dequeued_ + lq_canceled_ + count > lq_allocated_) {
        error_.record("LQ cancellation accounting exceeds allocated entries");
        return false;
    }
    lq_canceled_ += count;
    lq_canceled_unobserved_ += count;
    stage_admission_credit(count, 0);
    return true;
}

auto QueueAccounting::account_sq_cancellation(unsigned count)  -> bool
{
    if (sq_dequeued_ + sq_canceled_ + count > sq_allocated_) {
        error_.record("SQ cancellation accounting exceeds allocated entries");
        return false;
    }
    sq_canceled_ += count;
    sq_canceled_unobserved_ += count;
    stage_admission_credit(0, count);
    return true;
}

void QueueAccounting::allocate_scalar_store(std::uint64_t key)
{
    ++sq_allocated_;
    scalar_store_sq_targets_[key] = sq_allocated_ - sq_canceled_;
}
void QueueAccounting::allocate_vector_store(std::uint64_t key, std::uint64_t count)
{
    sq_allocated_ += count;
    vector_store_sq_targets_[key] = sq_allocated_ - sq_canceled_;
}
std::optional<std::uint64_t> QueueAccounting::scalar_store_target(std::uint64_t key) const
{
    const auto it = scalar_store_sq_targets_.find(key);
    if (it == scalar_store_sq_targets_.end()) return std::nullopt;
    return it->second;
}
std::optional<std::uint64_t> QueueAccounting::vector_store_target(std::uint64_t key) const
{
    const auto it = vector_store_sq_targets_.find(key);
    if (it == vector_store_sq_targets_.end()) return std::nullopt;
    return it->second;
}
} // namespace memblock
