#include "issue_payloads.hpp"

namespace memblock {

auto Environment::Impl::require_issue_session() -> bool
{
    if (ordinary_issue_driver_ && retained_issue_driver_ == ordinary_issue_driver_.get()) return true;
    error_.record("ordinary issue operation requires its persistent producer owner");
    return false;
}

auto Environment::Impl::begin_issue_session() -> bool
{
    if (retained_issue_driver_ || ordinary_issue_driver_) {
        error_.record("ordinary issue session conflicts with existing producer ownership");
        return false;
    }
    ordinary_issue_driver_ = std::make_unique<RetainedIssueDriver>();
    retained_issue_driver_ = ordinary_issue_driver_.get();
    RetainedIssueDriver::clear_pins(dut_);
    return true;
}

auto Environment::Impl::end_issue_session() -> bool
{
    if (!require_issue_session()) return false;
    if (!ordinary_issue_driver_->empty() || ordinary_issue_driver_->events_pending() ||
        !issue_acceptance_context_.empty()) {
        error_.record("ordinary issue session cannot end with offers or unconsumed events");
        return false;
    }
    retained_issue_driver_ = nullptr;
    ordinary_issue_driver_.reset();
    RetainedIssueDriver::clear_pins(dut_);
    return check_components();
}

auto Environment::Impl::issue_session_empty() const -> bool
{
    return ordinary_issue_driver_ && ordinary_issue_driver_->empty();
}

auto Environment::Impl::issue_session_available(OrdinaryIssueKind kind, unsigned lane) const -> bool
{
    if (!ordinary_issue_driver_) return false;
    return ordinary_issue_driver_->available(static_cast<RetainedIssueDriver::Kind>(kind), lane);
}

auto Environment::Impl::capture_issue_acceptance(std::uint64_t tag, std::uint64_t edge) -> bool
{
    OrdinaryIssueEvent context;
    context.edge = edge;
    context.feedback_cursor = feedback_.iq_slow_feedback_stats().sta_samples.size();
    context.committed_pending = queues_.store_retirement_stats().pending != 0;
    if (!issue_acceptance_context_.emplace(tag, context).second) {
        error_.record("ordinary producer accepted one attempt tag more than once");
        return false;
    }
    return true;
}

auto Environment::Impl::offer_load(std::uint64_t tag, const LoadTransaction &transaction) -> bool
{
    if (!require_issue_session()) return false;
    if (!scalar_immediate_is_valid(transaction.immediate) || transaction.lane >= kScalarLoadLanes ||
        transaction.rob >= kRobEntries) {
        error_.record("invalid ordinary load offer"); return false;
    }
    try {
        ordinary_issue_driver_->offer_load(tag, transaction.lane, ordinary_load_issue(transaction),
            [this, tag](std::uint64_t edge) { return capture_issue_acceptance(tag, edge); });
    } catch (const std::exception &failure) { error_.record(failure.what()); return false; }
    return true;
}

auto Environment::Impl::offer_prefetch(std::uint64_t tag, const PrefetchTransaction &transaction) -> bool
{
    if (!require_issue_session()) return false;
    if (!scalar_immediate_is_valid(transaction.immediate) || transaction.lane >= kScalarLoadLanes ||
        transaction.rob >= kRobEntries) {
        error_.record("invalid ordinary prefetch offer"); return false;
    }
    try {
        ordinary_issue_driver_->offer_load(tag, transaction.lane, ordinary_prefetch_issue(transaction),
            [this, tag](std::uint64_t edge) { return capture_issue_acceptance(tag, edge); });
    } catch (const std::exception &failure) { error_.record(failure.what()); return false; }
    return true;
}

auto Environment::Impl::offer_store_address(std::uint64_t tag, const StoreTransaction &transaction) -> bool
{
    if (!require_issue_session()) return false;
    if (!scalar_immediate_is_valid(transaction.immediate) || transaction.address_lane >= kScalarStoreLanes ||
        transaction.rob >= kRobEntries) {
        error_.record("invalid ordinary store address offer"); return false;
    }
    try {
        ordinary_issue_driver_->offer_store_address(tag, transaction.address_lane,
            ordinary_store_issue(transaction, false), [this, tag, transaction](std::uint64_t edge) {
                return capture_issue_acceptance(tag, edge) &&
                    store_scoreboard_.mark_address_issued(transaction, edge) &&
                    accept_release_store_input(scalar_store_key(transaction), false, true) &&
                    authorize_release_store_inputs();
            });
    } catch (const std::exception &failure) { error_.record(failure.what()); return false; }
    return true;
}

auto Environment::Impl::offer_store_data(std::uint64_t tag, const StoreTransaction &transaction) -> bool
{
    if (!require_issue_session()) return false;
    if (transaction.data_lane >= kScalarStoreLanes || transaction.rob >= kRobEntries) {
        error_.record("invalid ordinary store data offer"); return false;
    }
    try {
        ordinary_issue_driver_->offer_store_data(tag, transaction.data_lane,
            ordinary_store_issue(transaction, true), [this, tag, transaction](std::uint64_t edge) {
                return capture_issue_acceptance(tag, edge) &&
                    store_scoreboard_.mark_data_issued(transaction, transaction.data_lane, edge) &&
                    accept_release_store_input(scalar_store_key(transaction), false, false) &&
                    authorize_release_store_inputs();
            });
    } catch (const std::exception &failure) { error_.record(failure.what()); return false; }
    return true;
}

auto Environment::Impl::offer_vector(std::uint64_t tag, const VectorMemoryTransaction &transaction) -> bool
{
    if (!require_issue_session()) return false;
    if (transaction.lane >= kVectorMemoryLanes || transaction.rob >= kRobEntries) {
        error_.record("invalid ordinary vector offer"); return false;
    }
    try {
        ordinary_issue_driver_->offer_vector(tag, transaction.lane, make_vector_memory_issue(transaction),
            [this, tag, transaction](std::uint64_t edge) {
                return capture_issue_acceptance(tag, edge) && (!transaction.store ||
                    (accept_release_store_input(vector_store_key(transaction), true, true) &&
                        authorize_release_store_inputs()));
            });
    } catch (const std::exception &failure) { error_.record(failure.what()); return false; }
    return true;
}

auto Environment::Impl::take_issue_events() -> std::vector<OrdinaryIssueEvent>
{
    std::vector<OrdinaryIssueEvent> result;
    if (!require_issue_session()) return result;
    for (const auto &event : ordinary_issue_driver_->take_events()) {
        OrdinaryIssueEvent converted{event.tag, event.edge, static_cast<OrdinaryIssueKind>(event.kind),
            event.lane, static_cast<OrdinaryIssueOutcome>(event.outcome), event.killed};
        const auto context = issue_acceptance_context_.find(event.tag);
        if (event.outcome == RetainedIssueDriver::Outcome::accepted) {
            if (context == issue_acceptance_context_.end() || context->second.edge != event.edge) {
                error_.record("ordinary accepted event lost its pre-edge context"); return {};
            }
            converted.feedback_cursor = context->second.feedback_cursor;
            converted.committed_pending = context->second.committed_pending;
            issue_acceptance_context_.erase(context);
        } else if (context != issue_acceptance_context_.end()) {
            error_.record("ordinary unaccepted event has an acceptance context"); return {};
        }
        result.push_back(converted);
    }
    return result;
}

auto Environment::Impl::poll_vector_replay(const VectorMemoryTransaction &transaction,
    VectorMemoryTransaction &replay, bool &found) -> bool
{
    found = false;
    const auto request = feedback_.take_replay_for(transaction);
    if (!request) return check_components();
    if (!vector_scoreboard_.pending(transaction)) {
        error_.record("vector replay matched an already completed stream uop"); return false;
    }
    replay = transaction;
    replay.lane = request->lane;
    replay.is_part_replay = request->is_part_replay;
    replay.replay_mask = request->replay_mask;
    replay.replay_mb_index = request->replay_mb_index;
    found = true;
    return check_components();
}

} // namespace memblock
