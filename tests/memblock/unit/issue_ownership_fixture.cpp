// Independent pin witnesses from the retained transport fixture. No DUT or
// full-CPU simulation: real helpers and tick must agree on exclusive ownership.
// PIN_WITNESSES
#include "environment/scoreboards.hpp"
#include <functional>

struct Adapter : WitnessAdapter {
    std::function<void()> on_edge;
    std::function<void()> after_edge;
    void clear_scalar_load_issue_valid(unsigned lane) { valid.at(lane) = false; }
    void step() { if (on_edge) on_edge(); WitnessAdapter::step(); if (after_edge) after_edge(); }
};
struct Agent {
    void drive(Adapter &) {}
    void capture_before_tick(Adapter &) {}
    void update_after_tick() {}
};
struct Bridge {
    void capture_inputs() {}
    void check_before_edge() {}
    void advance_after_edge() {}
};
struct Queues {
    void sample_before_edge(Adapter &) {}
    void sample_after_edge(Adapter &, bool) {}
};
struct VectorScores {
    bool pending(const VectorMemoryTransaction &) const { return true; }
};
struct Feedback {
    bool observe_std = false;
    bool replay_available = true;
    unsigned replay_takes = 0;
    struct Replay {
        unsigned lane = 0;
        bool is_part_replay = false;
        std::uint16_t replay_mask = 0;
        std::uint8_t replay_mb_index = 0;
    };
    std::optional<Replay> take_replay_for(const VectorMemoryTransaction &) {
        ++replay_takes;
        if (!replay_available) return std::nullopt;
        replay_available = false; return Replay{};
    }
    void sample_before_edge(Adapter &dut, std::uint64_t edge, int, StoreScoreboard &stores, VectorScores &) {
        if (!observe_std) return;
        for (unsigned lane = 0; lane < 2; ++lane) {
            if (!dut.valid[5U + lane] || !dut.ready[5U + lane]) continue;
            generated::ScalarStoreWriteback wb;
            wb.valid = true; wb.rob_value = dut.payload[5U + lane][6];
            stores.observe_data(lane, wb, edge);
        }
    }
    void sample_after_edge(Adapter &, std::uint64_t) {}
};
struct Harness {
    Adapter dut_;
    Driver *retained_issue_driver_ = nullptr;
    Agent memory_agent_, ptw_agent_, uncache_agent_;
    Bridge bridge_; Queues queues_; Feedback feedback_;
    int scoreboard_ = 0;
    VectorScores vector_scoreboard_;
    StoreScoreboard store_scoreboard_;
    FailureState error_{"issue-ownership", "contract"};
    bool authorize_release_store_inputs() { return true; }
    std::vector<std::uint64_t> data_callbacks;
    bool accept_release_store_input(std::uint64_t key, bool vector, bool address) {
        assert(!vector && !address);
        data_callbacks.push_back(key); return true;
    }
    static std::uint64_t scalar_store_key(const StoreTransaction &s) {
        return s.rob | (std::uint64_t{s.rob_flag} << 8U) |
            (std::uint64_t{s.sq} << 9U) | (std::uint64_t{s.sq_flag} << 17U);
    }
    static std::uint64_t vector_store_key(const VectorMemoryTransaction &s) {
        return s.rob | (std::uint64_t{s.rob_flag} << 8U) |
            (std::uint64_t{s.sq} << 9U) | (std::uint64_t{s.sq_flag} << 17U);
    }
    void set_failure_cycle() {}
    bool check_components() const { return error_.empty() && store_scoreboard_.ok(); }
    std::uint64_t cycle() const { return dut_.edge; }
    void tick(bool monitor = true);
    static generated::VectorMemoryIssue make_vector_memory_issue(const VectorMemoryTransaction &);
    bool issue_load(const LoadTransaction &, unsigned);
    bool issue_load_batch(const std::vector<LoadTransaction> &, unsigned, bool);
    bool issue_prefetch(const PrefetchTransaction &, unsigned);
    bool issue_prefetch_batch_same_cycle(const std::vector<PrefetchTransaction> &, unsigned);
    bool issue_vector(const VectorMemoryTransaction &, unsigned);
    bool issue_vector_batch_same_cycle(const std::vector<VectorMemoryTransaction> &, unsigned);
    bool issue_store_address_batch(const std::vector<StoreTransaction> &, unsigned);
    bool issue_atomic(const AtomicTransaction &, unsigned);
    bool issue_store_data(const StoreTransaction &, unsigned);
    bool service_vector_replay(const VectorMemoryTransaction &, bool &, unsigned);
};
void trace_vector_issue(const VectorMemoryTransaction &) {}
void trace_store_issue(const char *, const StoreTransaction &) {}
// PRODUCTION_METHODS

struct Inputs {
    LoadTransaction load;
    PrefetchTransaction prefetch;
    VectorMemoryTransaction vector;
    StoreTransaction store;
    AtomicTransaction atomic;
    Inputs() {
        load.address = prefetch.address = vector.address = store.address = atomic.address = 0x80200178;
        load.rob = prefetch.rob = vector.rob = store.rob = atomic.rob = 37;
        load.pdest = vector.pdest = atomic.pdest = 17;
        store.data = atomic.data = 0x8877665544332211ULL;
        vector.vl = 16; vector.flow_num = 16;
    }
};
bool call_helper(Harness &h, const Inputs &in, unsigned method) {
    switch (method) {
    case 0: return h.issue_load(in.load, 3);
    case 1: return h.issue_load_batch({in.load}, 3, false);
    case 2: return h.issue_load_batch({in.load}, 3, true);
    case 3: return h.issue_prefetch(in.prefetch, 3);
    case 4: return h.issue_prefetch_batch_same_cycle({in.prefetch}, 3);
    case 5: return h.issue_vector(in.vector, 3);
    case 6: return h.issue_vector_batch_same_cycle({in.vector}, 3);
    case 7: return h.issue_store_address_batch({in.store}, 3);
    case 8: return h.issue_atomic(in.atomic, 3);
    case 9: {
        bool replayed = true;
        const bool result = h.service_vector_replay(in.vector, replayed, 3);
        assert(replayed == result);
        return result;
    }
    }
    throw std::logic_error("unknown helper");
}
void nested_ownership() {
    unsigned checks = 0;
    for (bool empty : {false, true}) for (unsigned method = 0; method < 10; ++method) {
        Harness h; Driver owner; Inputs in;
        h.dut_.ready.fill(true);
        h.store_scoreboard_.expect(in.store);
        if (!empty) {
            generated::ScalarLoadIssue older;
            older.rob_value = 31; older.src = 0x80201200;
            owner.offer_load(9001, 2, older);
        }
        h.retained_issue_driver_ = &owner;
        owner.drive(h.dut_);
        const auto valid = h.dut_.valid, ready = h.dut_.ready;
        const auto payload = h.dut_.payload;
        assert(!call_helper(h, in, method));
        assert(!h.error_.empty() && h.cycle() == 0);
        assert(h.dut_.valid == valid && h.dut_.ready == ready && h.dut_.payload == payload);
        assert(owner.empty() == empty && owner.take_events().empty());
        assert(std::all_of(h.dut_.accepted.begin(), h.dut_.accepted.end(), [](auto count) { return count == 0; }));
        assert(h.data_callbacks.empty() && h.store_scoreboard_.pending(in.store));
        assert(h.feedback_.replay_available && h.feedback_.replay_takes == 0);
        ++checks;
    }
    std::cout << "ownership_rejections=" << checks << " pin_mutations=0 edges=0\n";
}
void unbound_controls() {
    unsigned checks = 0;
    for (unsigned method = 0; method < 10; ++method) {
        Harness h; Inputs in; h.dut_.ready.fill(true); h.store_scoreboard_.expect(in.store);
        assert(call_helper(h, in, method) && h.check_components());
        const unsigned count = std::accumulate(h.dut_.accepted.begin(), h.dut_.accepted.end(), 0U);
        assert(count == (method == 8 ? 2U : 1U));
        assert(h.cycle() == (method == 8 ? 2U : 1U));
        assert(std::none_of(h.dut_.valid.begin(), h.dut_.valid.end(), [](bool bit) { return bit; }));
        ++checks;
    }
    std::cout << "unbound_controls=" << checks << " actual_accepted_edges=11\n";
}
StoreTransaction store(unsigned position) {
    StoreTransaction t;
    t.rob = position % 160; t.rob_flag = position / 160;
    t.address_lane = t.data_lane = position % 2;
    t.sq = position % 56; t.sq_flag = position / 56 % 2;
    t.address = 0x80200178; t.data = 0x123456789abcdef0ULL + position;
    t.op = StoreOp::sd;
    return t;
}
void check_std_payload(const Harness &h, const StoreTransaction &t) {
    const auto &bits = h.dut_.payload[5U + t.data_lane];
    assert(bits[0] == kFuTypeStore && bits[1] == static_cast<unsigned>(t.op));
    assert(bits[5] == t.rob_flag && bits[6] == t.rob && bits[7] == t.sq_flag && bits[8] == t.sq && bits[9] == t.data);
}
void std_identity() {
    for (unsigned position = 0; position < 320; ++position) {
        Harness h; const auto t = store(position);
        h.store_scoreboard_.expect(t); h.feedback_.observe_std = true;
        h.dut_.ready[5U + t.data_lane] = true;
        h.dut_.on_edge = [&] { check_std_payload(h, t); };
        assert(h.issue_store_data(t, 1) && h.cycle() == 1);
        assert(h.data_callbacks == std::vector<std::uint64_t>{Harness::scalar_store_key(t)});
        assert(h.dut_.accepted[5U + t.data_lane] == 1);
        // The actual store scoreboard already consumed the same-edge STD.
        assert(h.store_scoreboard_.mark_address_issued(t, h.cycle()));
        generated::ScalarStoreWriteback wb;
        wb.valid = true; wb.rob_value = t.rob; wb.rob_flag = t.rob_flag;
        h.store_scoreboard_.observe_address(t.address_lane, wb, h.cycle());
        assert(h.store_scoreboard_.done() && h.store_scoreboard_.observed() == 1);
    }
    std::cout << "STD_full_identity_cases=320 exact_completions=320\n";
}
void std_redirect() {
    // Independent age ordinals: 161 is younger than 159; 319 is older than
    // 321 (encoded 1/false). Both pairs straddle a ROB wrap and differ by two.
    for (bool killed : {false, true}) for (bool accepts_tail : {false, true}) {
        Harness h; const auto t = store(killed ? 161 : 319);
        h.store_scoreboard_.expect(t); h.feedback_.observe_std = true;
        h.dut_.queues.redirect.valid.data = 1;
        h.dut_.queues.redirect.rob_idx_value.data = killed ? 159 : 1;
        h.dut_.queues.redirect.rob_idx_flag.data = 0;
        h.dut_.after_edge = [&] {
            h.dut_.queues.redirect.valid.data = 0;
            h.dut_.ready[5U + t.data_lane] = h.dut_.edge == 1 ? accepts_tail : true;
        };
        // Set readiness only after an edge so capture/accept witnesses agree.
        const bool result = h.issue_store_data(t, 5);
        assert(result == !killed && h.cycle() == (killed || accepts_tail ? 2U : 3U));
        const unsigned accepted = !killed || accepts_tail;
        assert(h.dut_.accepted[5U + t.data_lane] == accepted && h.data_callbacks.size() == accepted);
        assert(!h.retained_issue_driver_ && !h.dut_.valid[5U + t.data_lane]);
    }
    std::cout << "STD_wrapped_redirect_cases=4\n";
}
int main(int argc, char **argv) {
    assert(argc == 2);
    const std::string mode = argv[1];
    if (mode == "ownership") nested_ownership();
    else if (mode == "controls") unbound_controls();
    else if (mode == "identity") std_identity();
    else if (mode == "redirect") std_redirect();
    else assert(false);
}
