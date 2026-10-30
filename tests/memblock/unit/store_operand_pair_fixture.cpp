#include "environment/retained_issue.hpp"
#include "environment/scoreboards.hpp"
#include <cassert>
#include <functional>
#include <random>

using namespace memblock;
struct Pin {
    std::uint64_t bits = 0;
    bool boolean() const { return bits != 0; }
    std::uint64_t value() const { return bits; }
};
struct Adapter {
    struct { struct { Pin valid, rob_idx_value, rob_idx_flag, level; } redirect; } queues;
    std::uint64_t edge = 0;
    bool address_valid = false, data_valid = false;
    bool address_ready = false, data_ready = false;
    generated::ScalarStoreIssue address, data;
    std::vector<std::uint64_t> address_edges, data_edges;
    std::vector<std::array<bool, 2>> manager_readiness;
    std::function<void()> on_edge;
    void clear_scalar_load_issue_valids() {}
    void clear_vector_memory_issue_valids() {}
    void clear_scalar_store_issue_valids() { address_valid = data_valid = false; }
    void drive_scalar_load_issue(unsigned, const generated::ScalarLoadIssue &) { assert(false); }
    void drive_vector_memory_issue(unsigned, const generated::VectorMemoryIssue &) { assert(false); }
    void drive_scalar_store_address(unsigned lane, const generated::ScalarStoreIssue &s) { assert(lane == 1); address = s; address_valid = true; }
    void drive_scalar_store_data(unsigned lane, const generated::ScalarStoreIssue &s) { assert(lane == 1); data = s; data_valid = true; }
    bool scalar_load_issue_ready(unsigned) const { return false; }
    bool vector_memory_issue_ready(unsigned) const { return false; }
    bool scalar_store_address_ready(unsigned) const { return address_ready; }
    bool scalar_store_data_ready(unsigned) const { return data_ready; }
    void refresh_comb() {}
    void step() {
        if (address_valid && address_ready) address_edges.push_back(edge);
        if (data_valid && data_ready) data_edges.push_back(edge);
        if (on_edge) on_edge();
        ++edge;
    }
};
struct Agent {
    Adapter *dut = nullptr;
    void drive(Adapter &) {
        if (!dut) return;
        const auto &ready = dut->manager_readiness.at(dut->edge);
        dut->address_ready = ready[0]; dut->data_ready = ready[1];
    }
    void capture_before_tick(Adapter &) {}
    void update_after_tick() {}
};
struct Bridge {
    void capture_inputs() {}
    void check_before_edge() {}
    void advance_after_edge() {}
};
struct Queues {
    Environment::StoreRetirementStats stats;
    const auto &store_retirement_stats() const { return stats; }
    void sample_before_edge(Adapter &) {}
    void sample_after_edge(Adapter &, bool) {}
};
struct Feedback {
    unsigned data_pulses = 0;
    void sample_before_edge(Adapter &dut, std::uint64_t edge, int, StoreScoreboard &scoreboard, int) {
        if (!dut.data_valid || !dut.data_ready) return;
        generated::ScalarStoreWriteback data;
        data.valid = true; data.rob_value = dut.data.rob_value;
        scoreboard.observe_data(1, data, edge);
        ++data_pulses;
    }
    void sample_after_edge(Adapter &, std::uint64_t) {}
};
struct Harness {
    using StoreOperandAcceptances = Environment::StoreOperandAcceptances;
    Adapter dut_;
    FailureState error_{"store-pair-host", "contract"};
    RetainedIssueDriver *retained_issue_driver_ = nullptr;
    Agent memory_agent_{&dut_}, ptw_agent_, uncache_agent_;
    Bridge bridge_; Queues queues_; Feedback feedback_;
    StoreScoreboard store_scoreboard_;
    int scoreboard_ = 0, vector_scoreboard_ = 0;
    bool accepted_address = false, accepted_data = false, registered = false, authorized = false;
    bool registration_failure = false, authorization_failure = false;
    unsigned registrations = 0, authorizations = 0;
    std::vector<std::array<std::uint64_t, 2>> callbacks;
    static std::uint64_t scalar_store_key(const StoreTransaction &s) { return s.rob | (std::uint64_t(s.rob_flag) << 8); }
    bool accept_release_store_input(std::uint64_t key, bool vector, bool address) {
        assert(key == (139U | 256U) && !vector);
        callbacks.push_back({address, dut_.edge});
        (address ? accepted_address : accepted_data) = true;
        if (accepted_address && accepted_data && !registered) {
            if (registration_failure) { error_.record("input registration rejected"); return false; }
            registered = true; ++registrations;
        }
        return true;
    }
    bool authorize_release_store_inputs() {
        if (registered && !authorized) {
            if (authorization_failure) { error_.record("input authorization rejected"); return false; }
            authorized = true; ++authorizations;
        }
        return true;
    }
    std::uint64_t cycle() const { return dut_.edge; }
    void set_failure_cycle() {}
    bool check_components() const { return error_.empty() && store_scoreboard_.ok(); }
    bool issue_store_address(const StoreTransaction &, unsigned);
    bool issue_store_data(const StoreTransaction &, unsigned);
    bool issue_store_operands(const StoreTransaction &, StoreOperandAcceptances &, unsigned);
    void tick(bool monitor = true);
};
void trace_store_issue(const char *, const StoreTransaction &) {}
// PRODUCTION_METHODS

StoreTransaction transaction() {
    StoreTransaction t;
    t.rob = 139; t.rob_flag = true; t.sq = 55; t.sq_flag = true;
    t.address_lane = t.data_lane = 1;
    t.address = 0x80000178; t.immediate = -17; t.data = 0x8877665544332211ULL;
    return t;
}
void random_pairs() {
    unsigned together = 0, address_first = 0, data_first = 0, skew_sum = 0;
    for (unsigned seed = 0; seed < 256; ++seed) {
        std::mt19937 rng(seed);
        Harness h; const auto t = transaction();
        h.store_scoreboard_.expect(t);
        const unsigned a = rng() % 24U, d = rng() % 24U;
        h.dut_.manager_readiness.resize(24);
        for (unsigned e = 0; e < 24; ++e) h.dut_.manager_readiness[e] = {e >= a, e >= d};
        // Before manager drive, readiness predicts opposite public events.
        h.dut_.address_ready = !h.dut_.manager_readiness[0][0];
        h.dut_.data_ready = !h.dut_.manager_readiness[0][1];
        h.queues_.stats.pending = 1;
        h.dut_.on_edge = [&] {
            if (h.dut_.edge < std::max(a, d)) assert(!h.registered);
            else assert(h.registered && h.authorized);
            if (h.dut_.address_valid) {
                const auto &s = h.dut_.address;
                assert(s.rob_value == t.rob && s.rob_flag == t.rob_flag && s.sq_value == t.sq && s.sq_flag);
                assert(s.fu_type == kFuTypeStore && s.fu_op_type == unsigned(t.op));
                assert(s.src + t.immediate == t.address && std::int32_t(s.imm) == t.immediate);
            }
            if (h.dut_.data_valid) assert(h.dut_.data.src == t.data && h.dut_.data.rob_value == t.rob);
            if (h.dut_.edge == std::min(a, d)) h.queues_.stats.pending = 0;
        };
        Harness::StoreOperandAcceptances accepts;
        assert(h.issue_store_operands(t, accepts, 24));
        assert((accepts.edges == std::array<std::uint64_t, 2>{a, d}));
        assert(accepts.committed_pending == (a == d ? 2U : 1U));
        assert(h.dut_.address_edges == std::vector<std::uint64_t>{a});
        assert(h.dut_.data_edges == std::vector<std::uint64_t>{d});
        assert(h.callbacks.size() == 2 && h.feedback_.data_pulses == 1);
        assert(h.registrations == 1 && h.authorizations == 1 && !h.retained_issue_driver_);
        assert(!h.dut_.address_valid && !h.dut_.data_valid);
        // Actual StoreScoreboard must have consumed the same-edge STD already.
        // Complete only STA now; both obligations must disappear exactly once.
        generated::ScalarStoreWriteback wb;
        wb.valid = true; wb.rob_value = t.rob; wb.rob_flag = t.rob_flag;
        h.store_scoreboard_.observe_address(1, wb, h.cycle() + 3U);
        assert(h.store_scoreboard_.done() && h.store_scoreboard_.observed() == 1 && h.check_components());
        together += a == d; address_first += a < d; data_first += d < a;
        skew_sum += std::max(a, d) - std::min(a, d);
    }
    assert(together && address_first && data_first);
    std::cout << "pairs=256 together=" << together << " address_first=" << address_first
              << " data_first=" << data_first << " skew_sum=" << skew_sum << '\n';
}
void failures() {
    auto t = transaction();
    for (bool registration : {false, true}) {
        Harness h; h.store_scoreboard_.expect(t); h.dut_.manager_readiness = {{true, true}};
        h.registration_failure = registration; h.authorization_failure = !registration;
        Harness::StoreOperandAcceptances a;
        assert(!h.issue_store_operands(t, a, 1));
        assert(h.cycle() == 0 && !h.error_.empty() && !h.retained_issue_driver_);
        assert(h.dut_.address_edges.empty() && h.dut_.data_edges.empty());
        assert(!h.dut_.address_valid && !h.dut_.data_valid);
    }
    Harness timeout; timeout.store_scoreboard_.expect(t); timeout.dut_.manager_readiness = {{true,false},{true,false}};
    Harness::StoreOperandAcceptances a;
    assert(!timeout.issue_store_operands(t, a, 2));
    assert(timeout.dut_.address_edges.size() == 1 && timeout.dut_.data_edges.empty());
    assert(!timeout.retained_issue_driver_ && !timeout.dut_.address_valid && !timeout.dut_.data_valid);
    Harness invalid; t.data_lane = 0;
    assert(!invalid.issue_store_operands(t, a, 2) && invalid.cycle() == 0);
    std::cout << "failure_cases=4\n";
}
int main(int argc, char **argv) {
    assert(argc == 2);
    if (std::string(argv[1]) == "pairs") random_pairs();
    else if (std::string(argv[1]) == "failures") failures();
    else assert(false);
}
