#pragma once
#include "memblock_env.hpp"
#include "dut_adapter.hpp"
#include <functional>

namespace memblock {
class LoadScoreboard;
class StoreScoreboard;
class VectorMemoryScoreboard;
struct VectorReplayRequest {
    unsigned lane;
    bool lq_flag;
    std::uint8_t lq_value;
    bool sq_flag;
    std::uint8_t sq_value;
    bool is_part_replay;
    std::uint16_t replay_mask;
    std::uint8_t replay_mb_index;
    std::uint64_t cycle = 0;
};


// Samples external feedback in its specified clock phase and owns replay
// records. Consumers remove only the replay selected by transaction identity.
class FeedbackMonitor {
public:
    using WritebackTrace = std::function<void(unsigned, const generated::ScalarLoadWriteback &)>;
    void sample_before_edge(DutAdapter &dut_, std::uint64_t sample_cycle,
        LoadScoreboard &scoreboard_, StoreScoreboard &store_scoreboard_,
        VectorMemoryScoreboard &vector_scoreboard_, const WritebackTrace &trace = {});
    void sample_after_edge(DutAdapter &dut_, std::uint64_t sample_cycle);
    void reset_observations();
    std::optional<VectorReplayRequest> take_replay_for(const VectorMemoryTransaction &transaction);
    std::optional<VectorReplayRequest> take_next_replay();
    std::size_t pending_replays() const { return vector_replay_requests_.size(); }
    const Environment::ScalarLoadFeedbackStats &scalar_load_feedback_stats() const { return scalar_load_feedback_stats_; }
    const Environment::LoadMmioStats &load_mmio_stats() const { return load_mmio_stats_; }
    const Environment::IqSlowFeedbackStats &iq_slow_feedback_stats() const { return iq_slow_feedback_stats_; }
    const Environment::MemoryViolationStats &memory_violation_stats() const { return memory_violation_stats_; }
    const Environment::IfetchPrefetchStats &ifetch_prefetch_stats() const { return ifetch_prefetch_stats_; }
    const Environment::HardwarePrefetchStats &hardware_prefetch_stats() const { return hardware_prefetch_stats_; }
    const Environment::BusErrorStats &bus_error_stats() const { return bus_error_stats_; }
    std::uint64_t store_tlb_feedbacks() const { return store_tlb_feedbacks_; }
    std::uint64_t store_tlb_misses() const { return store_tlb_misses_; }
    std::uint64_t vector_replay_feedbacks() const { return vector_replay_feedbacks_; }
private:
    Environment::ScalarLoadFeedbackStats scalar_load_feedback_stats_;
    Environment::LoadMmioStats load_mmio_stats_;
    Environment::IqSlowFeedbackStats iq_slow_feedback_stats_;
    Environment::MemoryViolationStats memory_violation_stats_;
    Environment::IfetchPrefetchStats ifetch_prefetch_stats_;
    Environment::HardwarePrefetchStats hardware_prefetch_stats_;
    Environment::BusErrorStats bus_error_stats_;
    std::uint64_t store_tlb_feedbacks_ = 0;
    std::uint64_t store_tlb_misses_ = 0;
    std::deque<VectorReplayRequest> vector_replay_requests_;
    std::uint64_t vector_replay_feedbacks_ = 0;
};
} // namespace memblock
