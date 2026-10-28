#include "feedback_monitor.hpp"
#include "scoreboards.hpp"

namespace memblock {
void FeedbackMonitor::sample_before_edge(DutAdapter &dut_, std::uint64_t sample_cycle,
    LoadScoreboard &scoreboard_, StoreScoreboard &store_scoreboard_,
    VectorMemoryScoreboard &vector_scoreboard_, const WritebackTrace &trace)
{
    if (dut_.errors.dcache.ecc_error_valid.boolean()) {
        ++bus_error_stats_.dcache_reports;
        bus_error_stats_.last_dcache_address =
            dut_.errors.dcache.ecc_error_bits.value();
    }
    if (dut_.errors.uncache.ecc_error_valid.boolean()) {
        ++bus_error_stats_.uncache_reports;
        bus_error_stats_.last_uncache_address =
            dut_.errors.uncache.ecc_error_bits.value();
    }
    const auto memory_violation =
        dut_.sample_memory_violation();
    if (memory_violation.valid) {
        ++memory_violation_stats_.count;
        memory_violation_stats_.last = memory_violation;
        memory_violation_stats_.last_cycle = sample_cycle;
    }
    for (unsigned lane = 0; lane < kScalarLoadLanes; ++lane) {
        bool load_mmio = false;
        std::uint8_t load_mmio_rob = 0;
        switch (lane) {
        case 0:
            load_mmio = dut_.queues.observation.load_mmio_0.boolean();
            load_mmio_rob = static_cast<std::uint8_t>(
                dut_.queues.observation.load_mmio_uop_0_rob_idx_value.value());
            break;
        case 1:
            load_mmio = dut_.queues.observation.load_mmio_1.boolean();
            load_mmio_rob = static_cast<std::uint8_t>(
                dut_.queues.observation.load_mmio_uop_1_rob_idx_value.value());
            break;
        case 2:
            load_mmio = dut_.queues.observation.load_mmio_2.boolean();
            load_mmio_rob = static_cast<std::uint8_t>(
                dut_.queues.observation.load_mmio_uop_2_rob_idx_value.value());
            break;
        default:
            break;
        }
        if (load_mmio) {
            ++load_mmio_stats_.pulses[lane];
            load_mmio_stats_.last_rob[lane] = load_mmio_rob;
            load_mmio_stats_.last_cycle[lane] = sample_cycle;
        }
        const auto ifetch_prefetch =
            dut_.sample_ifetch_prefetch(lane);
        if (ifetch_prefetch.valid) {
            ++ifetch_prefetch_stats_.requests[lane];
            ifetch_prefetch_stats_.last_vaddr[lane] =
                ifetch_prefetch.vaddr;
            ifetch_prefetch_stats_.last_cycle[lane] = sample_cycle;
        }
        const auto wakeup =
            dut_.sample_scalar_load_wakeup(lane);
        if (wakeup.valid) {
            ++scalar_load_feedback_stats_.wakeups[lane];
            scalar_load_feedback_stats_.last_wakeup[lane] = {
                .valid = true,
                .rf_wen = wakeup.rf_wen,
                .fp_wen = wakeup.fp_wen,
                .pdest = wakeup.pdest,
                .cycle = sample_cycle,
            };
        }
        const bool canceled = dut_.sample_scalar_load_cancel(lane);
        if (canceled) {
            ++scalar_load_feedback_stats_.ld2_cancels[lane];
            scalar_load_feedback_stats_.last_cancel_cycle[lane] = sample_cycle;
        }
        const auto writeback =
            dut_.sample_scalar_load_writeback(lane);
        if (trace) trace(lane, writeback);
        scoreboard_.observe(lane, writeback, memory_violation);
    }
    const auto hardware_prefetch =
        dut_.sample_hardware_prefetch_outputs();
    if (hardware_prefetch.l2_valid) {
        ++hardware_prefetch_stats_.l2_requests;
        ++hardware_prefetch_stats_.l2_source_counts[
            hardware_prefetch.l2_source];
        hardware_prefetch_stats_.l2_addresses_by_source[
            hardware_prefetch.l2_source].push_back(
                hardware_prefetch.l2_addr);
        hardware_prefetch_stats_.last_l2_addr_by_source[
            hardware_prefetch.l2_source] = hardware_prefetch.l2_addr;
        hardware_prefetch_stats_.last_l2_cycle_by_source[
            hardware_prefetch.l2_source] = sample_cycle;
        hardware_prefetch_stats_.last_l2_addr =
            hardware_prefetch.l2_addr;
        hardware_prefetch_stats_.last_l2_source =
            hardware_prefetch.l2_source;
        hardware_prefetch_stats_.last_l2_cycle = sample_cycle;
    }
    if (hardware_prefetch.l3_valid) {
        ++hardware_prefetch_stats_.l3_requests;
        hardware_prefetch_stats_.last_l3_addr =
            hardware_prefetch.l3_addr;
        hardware_prefetch_stats_.last_l3_cycle = sample_cycle;
    }
    for (unsigned lane = 0; lane < kScalarStoreLanes; ++lane) {
        const auto address_writeback =
            dut_.sample_scalar_store_address_writeback(lane);
        const auto data_writeback =
            dut_.sample_scalar_store_data_writeback(lane);
        store_scoreboard_.observe_address(
            lane, address_writeback, sample_cycle);
        store_scoreboard_.observe_data(lane, data_writeback, sample_cycle);
    }
    for (unsigned lane = 0; lane < kVectorMemoryLanes; ++lane) {
        vector_scoreboard_.observe(
            lane, dut_.sample_vector_memory_writeback(lane));
    }
}

void FeedbackMonitor::sample_after_edge(DutAdapter &dut_, std::uint64_t sample_cycle)
{
    if (dut_.feedback.scalar_store.lane_0_feedback_slow_valid.boolean()) {
        const bool hit =
            dut_.feedback.scalar_store.lane_0_feedback_slow_hit.boolean();
        ++store_tlb_feedbacks_;
        store_tlb_misses_ += hit ? 0 : 1;
        ++iq_slow_feedback_stats_.sta_valid[0];
        ++(hit ? iq_slow_feedback_stats_.sta_hits[0]
               : iq_slow_feedback_stats_.sta_misses[0]);
        iq_slow_feedback_stats_.sta_samples.push_back(
            Environment::StoreSlowFeedbackSample{
                .lane = 0,
                .hit = hit,
                .sq_flag = dut_.feedback.scalar_store.lane_0_feedback_slow_sq_idx_flag.boolean(),
                .sq_value = static_cast<std::uint8_t>(
                    dut_.feedback.scalar_store.lane_0_feedback_slow_sq_idx_value.value()),
                .cycle = sample_cycle,
            });
    }
    if (dut_.feedback.scalar_store.lane_1_feedback_slow_valid.boolean()) {
        const bool hit =
            dut_.feedback.scalar_store.lane_1_feedback_slow_hit.boolean();
        ++store_tlb_feedbacks_;
        store_tlb_misses_ += hit ? 0 : 1;
        ++iq_slow_feedback_stats_.sta_valid[1];
        ++(hit ? iq_slow_feedback_stats_.sta_hits[1]
               : iq_slow_feedback_stats_.sta_misses[1]);
        iq_slow_feedback_stats_.sta_samples.push_back(
            Environment::StoreSlowFeedbackSample{
                .lane = 1,
                .hit = hit,
                .sq_flag = dut_.feedback.scalar_store.lane_1_feedback_slow_sq_idx_flag.boolean(),
                .sq_value = static_cast<std::uint8_t>(
                    dut_.feedback.scalar_store.lane_1_feedback_slow_sq_idx_value.value()),
                .cycle = sample_cycle,
            });
    }
    if (dut_.feedback.vector_store.lane_0_feedback_slow_valid.boolean()) {
        const Environment::VectorStoreSlowFeedbackSample sample{
            .lane = 0,
            .hit = dut_.feedback.vector_store.lane_0_feedback_slow_hit.boolean(),
            .lq_flag = dut_.feedback.vector_store.lane_0_feedback_slow_lq_idx_flag.boolean(),
            .lq_value = static_cast<std::uint8_t>(
                dut_.feedback.vector_store.lane_0_feedback_slow_lq_idx_value.value()),
            .sq_flag = dut_.feedback.vector_store.lane_0_feedback_slow_sq_idx_flag.boolean(),
            .sq_value = static_cast<std::uint8_t>(
                dut_.feedback.vector_store.lane_0_feedback_slow_sq_idx_value.value()),
            .is_part_replay = dut_.feedback.vector_store.lane_0_feedback_slow_is_vec_part_replay.boolean(),
            .replay_mask = static_cast<std::uint16_t>(
                dut_.feedback.vector_store.lane_0_feedback_slow_vec_replay_mask.value()),
            .replay_mb_index = static_cast<std::uint8_t>(
                dut_.feedback.vector_store.lane_0_feedback_slow_vec_replay_mb_idx.value()),
            .cycle = sample_cycle,
        };
        ++iq_slow_feedback_stats_.vstu_valid[0];
        ++(sample.hit ? iq_slow_feedback_stats_.vstu_hits[0]
                      : iq_slow_feedback_stats_.vstu_misses[0]);
        iq_slow_feedback_stats_.vstu_samples.push_back(sample);
        if (!sample.hit) {
            vector_replay_requests_.push_back(VectorReplayRequest{
                .lane = sample.lane,
                .lq_flag = sample.lq_flag,
                .lq_value = sample.lq_value,
                .sq_flag = sample.sq_flag,
                .sq_value = sample.sq_value,
                .is_part_replay = sample.is_part_replay,
                .replay_mask = sample.replay_mask,
                .replay_mb_index = sample.replay_mb_index,
            });
            ++vector_replay_feedbacks_;
        }
    }
    if (dut_.feedback.vector_store.lane_1_feedback_slow_valid.boolean()) {
        const Environment::VectorStoreSlowFeedbackSample sample{
            .lane = 1,
            .hit = dut_.feedback.vector_store.lane_1_feedback_slow_hit.boolean(),
            .lq_flag = dut_.feedback.vector_store.lane_1_feedback_slow_lq_idx_flag.boolean(),
            .lq_value = static_cast<std::uint8_t>(
                dut_.feedback.vector_store.lane_1_feedback_slow_lq_idx_value.value()),
            .sq_flag = dut_.feedback.vector_store.lane_1_feedback_slow_sq_idx_flag.boolean(),
            .sq_value = static_cast<std::uint8_t>(
                dut_.feedback.vector_store.lane_1_feedback_slow_sq_idx_value.value()),
            .is_part_replay = dut_.feedback.vector_store.lane_1_feedback_slow_is_vec_part_replay.boolean(),
            .replay_mask = static_cast<std::uint16_t>(
                dut_.feedback.vector_store.lane_1_feedback_slow_vec_replay_mask.value()),
            .replay_mb_index = static_cast<std::uint8_t>(
                dut_.feedback.vector_store.lane_1_feedback_slow_vec_replay_mb_idx.value()),
            .cycle = sample_cycle,
        };
        ++iq_slow_feedback_stats_.vstu_valid[1];
        ++(sample.hit ? iq_slow_feedback_stats_.vstu_hits[1]
                      : iq_slow_feedback_stats_.vstu_misses[1]);
        iq_slow_feedback_stats_.vstu_samples.push_back(sample);
        if (!sample.hit) {
            vector_replay_requests_.push_back(VectorReplayRequest{
                .lane = sample.lane,
                .lq_flag = sample.lq_flag,
                .lq_value = sample.lq_value,
                .sq_flag = sample.sq_flag,
                .sq_value = sample.sq_value,
                .is_part_replay = sample.is_part_replay,
                .replay_mask = sample.replay_mask,
                .replay_mb_index = sample.replay_mb_index,
            });
            ++vector_replay_feedbacks_;
        }
    }
}

void FeedbackMonitor::reset_observations()
{
    scalar_load_feedback_stats_ = {};
    load_mmio_stats_ = {};
    iq_slow_feedback_stats_ = {};
    memory_violation_stats_ = {};
    ifetch_prefetch_stats_ = {};
    hardware_prefetch_stats_ = {};
    bus_error_stats_ = {};
}

std::optional<VectorReplayRequest> FeedbackMonitor::take_replay_for(
    const VectorMemoryTransaction &transaction)
{
    // Only vector-store slow feedback creates these requests. LQ is a
    // snapshot, not the identity of an independently replaying vector load.
    if (!transaction.store) return std::nullopt;
    const auto replay_it = std::find_if(
        vector_replay_requests_.begin(), vector_replay_requests_.end(),
        [&](const VectorReplayRequest &request) {
            const unsigned entries = transaction.store
                ? kStoreQueueEntries : kVirtualLoadQueueEntries;
            const unsigned value = transaction.store
                ? request.sq_value : request.lq_value;
            const bool flag = transaction.store
                ? request.sq_flag : request.lq_flag;
            const unsigned base = transaction.store
                ? transaction.sq : transaction.lq;
            const bool base_flag = transaction.store
                ? transaction.sq_flag : transaction.lq_flag;
            const unsigned packed = value + (flag ? entries : 0);
            const unsigned packed_base = base + (base_flag ? entries : 0);
            const unsigned distance =
                (packed + 2 * entries - packed_base) % (2 * entries);
            return distance < transaction.flow_num;
    });
    if (replay_it == vector_replay_requests_.end()) return std::nullopt;
    const auto request = *replay_it;
    vector_replay_requests_.erase(replay_it);
    return request;
}

std::optional<VectorReplayRequest> FeedbackMonitor::take_next_replay()
{
    if (vector_replay_requests_.empty()) return std::nullopt;
    const auto request = vector_replay_requests_.front();
    vector_replay_requests_.pop_front();
    return request;
}
} // namespace memblock
