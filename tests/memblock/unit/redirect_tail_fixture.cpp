// Script only public terminals and queue credits. The environment's tick,
// redirect, cancellation methods and all three scoreboards are production code.
// The independent obligation builders/checker are shared with the suffix tests.
// OBLIGATION_FIXTURE
#include <functional>
#include <map>
#include "scenarios/mixed/stream_recovery.hpp"
#include "scenarios/mixed/stream_cancellation.hpp"
#include "scenarios/mixed/store_address_replay.hpp"

struct Pin {
    std::uint64_t bits = 0;
    void set(std::uint64_t value) { bits = value; }
    auto value() const { return bits; }
    bool boolean() const { return bits != 0; }
};
struct Adapter {
    struct {
        struct { Pin valid, rob_idx_value, rob_idx_flag, level, is_vls_exception; } redirect;
        struct { Pin lq_cancel_cnt, sq_cancel_cnt; } retirement;
    } queues;
    std::uint64_t edge = 0;
    void refresh_comb() {}
    void step() { ++edge; }
};
struct Agent {
    FailureState error{"tail-manager", "contract"};
    bool reject = false;
    std::vector<std::uint64_t> canceled;
    void drive(Adapter &) {}
    void capture_before_tick(Adapter &) {}
    void update_after_tick() {}
    const auto &failures() const { return error; }
    bool cancel_release_stores(const std::vector<std::uint64_t> &orders) {
        if (reject) { error.record("manager rejected store cancellation"); return false; }
        canceled.insert(canceled.end(), orders.begin(), orders.end());
        return true;
    }
};
struct Queues {
    bool reject = false;
    unsigned observations = 0;
    void sample_before_edge(Adapter &) {}
    void sample_after_edge(Adapter &, bool) {}
    bool observe_redirect_cancellation(std::uint64_t lq, std::uint64_t sq) {
        ++observations;
        require(lq == 2 && sq == 1, "wrong scripted physical queue credits");
        return !reject;
    }
    std::uint64_t sq_allocated() const { return 7; }
};
struct Bridge {
    void capture_inputs() {}
    void check_before_edge() {}
    void advance_after_edge() {}
};
struct Feedback {
    std::function<void(std::uint64_t)> sample;
    std::function<void(std::uint64_t)> after_sample;
    std::vector<std::uint64_t> edges;
    void sample_before_edge(Adapter &, std::uint64_t edge, LoadScoreboard &,
                           StoreScoreboard &, VectorMemoryScoreboard &) {
        edges.push_back(edge);
        if (sample) sample(edge);
    }
    void sample_after_edge(Adapter &, std::uint64_t edge) { if (after_sample) after_sample(edge); }
};
// No active producer is needed to test terminal propagation. Retained-producer
// cancellation/acceptance is qualified separately by test_retained_issue.py.
struct IdleProducer {
    struct Redirect { bool valid; std::uint8_t rob; bool flag, self; };
    void drive(Adapter &) {}
    void capture_before_edge(Adapter &, std::uint64_t, Redirect) {}
    bool authorize_before_edge() { return true; }
    void advance_after_edge(Adapter &) {}
};
struct Harness : Fixture {
    Adapter dut_;
    Agent memory_agent_, ptw_agent_, uncache_agent_;
    Queues queues_; Bridge bridge_; Feedback feedback_;
    IdleProducer *retained_issue_driver_ = nullptr;
    FailureState error_{"redirect-tail", "contract"};
    LoadScoreboard &scoreboard_ = loads;
    StoreScoreboard &store_scoreboard_ = stores;
    VectorMemoryScoreboard &vector_scoreboard_ = vectors;
    // PRODUCTION_TYPES
    std::map<std::uint64_t, ReleaseStoreInputs> scalar_release_inputs_, vector_release_inputs_;
    std::optional<ScalarRedirectBoundary> scalar_redirect_boundary_;
    Environment::IqSlowFeedbackStats slow_feedback;
    const auto &iq_slow_feedback_stats() const { return slow_feedback; }
    std::uint64_t cycle() const { return dut_.edge; }
    void set_failure_cycle() {}
    bool authorize_release_store_inputs() { return true; }
    bool check_components() {
        error_.merge(memory_agent_.failures());
        error_.merge(loads.failures()); error_.merge(stores.failures()); error_.merge(vectors.failures());
        return error_.empty();
    }
    void tick(bool monitor = true);
    bool redirect_after(std::uint8_t, bool, bool, bool = false);
    bool cancel_pending_for_redirect(std::uint8_t, bool, bool);
    Harness() { dut_.queues.retirement.lq_cancel_cnt.set(2); dut_.queues.retirement.sq_cancel_cnt.set(1); }
};
// PRODUCTION_METHODS

static void install(Harness &h, Obligation entry)
{
    h.entries.push_back(entry);
    h.expect(entry);
}

static void tails()
{
    unsigned cases = 0;
    for (int anchor : {0, 159, 160, 319}) for (bool self : {false, true})
    for (unsigned terminal = 0; terminal < 7; ++terminal) for (unsigned edge = 0; edge < 3; ++edge) {
        Harness h;
        const Kind kind = terminal < 2 ? Kind::scalar : terminal == 2 ? Kind::prefetch
            : terminal < 5 ? Kind::store : terminal == 5 ? Kind::vector_load : Kind::vector_store;
        install(h, obligation(kind, anchor - 1));  // Obligatory survivor.
        install(h, obligation(kind, anchor + 1, 0, terminal == 1)); // Exact tail.
        install(h, obligation(kind, anchor + 2)); // Canceled without terminal.
        install(h, obligation(kind, anchor)); // Tests both redirect levels.
        auto &tail = h.entries[1];
        if (kind == Kind::store) {
            require(h.stores.mark_address_issued(tail.store, 0), h.stores.error());
            require(h.stores.mark_data_issued(tail.store, tail.store.data_lane, 0), h.stores.error());
            if (terminal == 3) {
                h.stores.observe_data(tail.store.data_lane, store_completion(tail), 0);
                tail.data_seen = true;
            } else {
                h.stores.observe_address(tail.store.address_lane, store_completion(tail), 0);
                tail.address_seen = true;
            }
        }
        if (kind == Kind::vector_load || kind == Kind::vector_store)
            install(h, obligation(kind, anchor + 1, 1)); // Pending sibling cancels by instruction.
        h.check_pending();
        Harness::ReleaseStoreInputs input{h.entries[1].load.rob, h.entries[1].load.rob_flag};
        input.registered = true; input.groups.push_back({42, 0, 0, {1}});
        h.scalar_release_inputs_.emplace(1, input);
        input.authorized = true; h.scalar_release_inputs_.emplace(2, input);
        h.feedback_.sample = [&](auto e) {
            require(h.scalar_release_inputs_.count(1) && h.scalar_release_inputs_.count(2) &&
                h.memory_agent_.canceled.empty(), "input journal canceled before completed redirect");
            if (e == edge) h.finish(h.entries[1]);
        };
        require(memblock::scenarios::mixed::return_ordinary_redirect(h, anchor % 160, anchor / 160, self),
                "legal exact redirect tail rejected");
        require(h.cycle() == 3 && h.queues_.observations == 1 &&
            h.feedback_.edges == std::vector<std::uint64_t>{0, 1, 2}, "redirect tail was not fully monitored");
        require(h.entries[1].completed, "tail terminal was not checked");
        require(!h.scalar_release_inputs_.count(1) && h.scalar_release_inputs_.count(2) &&
            h.memory_agent_.canceled == std::vector<std::uint64_t>{42}, "wrong input journal cancellation");
        for (unsigned i = 0; i < h.entries.size(); ++i)
            if (i == 2 || i == 4 || (i == 3 && self)) h.entries[i].canceled = true;
        h.check_pending();
        for (auto &entry : h.entries) if (!entry.completed && !entry.canceled) h.finish(entry);
        require(h.loads.done() && h.stores.done() && h.vectors.done() && h.check_components(),
                "surviving obligations did not finish exactly once");
        ++cases;
    }
    std::cout << "exact_tail_cases=" << cases << " monitored_edges=0,1,2\n";
}

static void boundaries()
{
    for (unsigned fault = 0; fault < 8; ++fault) {
        Harness h;
        install(h, obligation(Kind::scalar, 99));
        install(h, obligation(Kind::scalar, 101));
        if (fault != 0) require(h.redirect_after(100, false, true), "redirect setup failed");
        unsigned rob = 100; bool flag = false, self = true;
        if (fault == 1) rob = 101;
        if (fault == 2) flag = true;
        if (fault == 3) self = false;
        if (fault == 4) h.tick();
        if (fault == 5) rob = 160;
        if (fault == 6) {
            require(h.cancel_pending_for_redirect(100, false, true), "first cancellation failed");
            h.entries[1].canceled = true;
        }
        if (fault == 7) { h.queues_.reject = true; require(!h.redirect_after(100, false, true), "bad queue accounting accepted"); }
        require(!h.cancel_pending_for_redirect(rob, flag, self), "uncertified cancellation accepted");
        h.check_pending();
    }
    std::cout << "boundary_rejections=8\n";
}

static void corrupt()
{
    for (unsigned fault = 0; fault < 10; ++fault) for (unsigned edge = 0; edge < 3; ++edge) {
        Harness h;
        install(h, obligation(fault >= 6 ? Kind::store : fault >= 4 ? Kind::vector_load : Kind::scalar, 101));
        if (fault >= 6) {
            const auto &t = h.entries[0].store;
            require(h.stores.mark_address_issued(t, 0) && h.stores.mark_data_issued(t, t.data_lane, 0),
                    "store issue setup failed");
        }
        h.feedback_.sample = [&](auto e) {
            if (e != edge) return;
            if (fault < 4) {
                auto wb = completion(h.entries[0]);
                if (fault == 0) wb.data ^= 1;
                if (fault == 1) ++wb.pdest;
                if (fault == 2) wb.rob_flag = !wb.rob_flag;
                h.loads.observe(0, wb);
                if (fault == 3) h.loads.observe(0, wb);
            } else if (fault < 6) {
                auto wb = vector_completion(h.entries[0]);
                if (fault == 4) wb.data[0] ^= 1;
                h.vectors.observe(0, wb);
                if (fault == 5) h.vectors.observe(0, wb);
            } else {
                const auto &t = h.entries[0].store;
                auto wb = store_completion(h.entries[0]);
                if (fault == 6) wb.exception_mask = 1;
                if (fault <= 7) {
                    h.stores.observe_address(t.address_lane, wb, e);
                    if (fault == 7) h.stores.observe_address(t.address_lane, wb, e);
                } else {
                    h.stores.observe_data(fault == 8 ? (t.data_lane + 1) % 2 : t.data_lane, wb, e);
                    if (fault == 9) h.stores.observe_data(t.data_lane, wb, e);
                }
            }
        };
        require(!h.redirect_after(100, false, true) && !h.scalar_redirect_boundary_,
                "corrupt or duplicate tail escaped exact comparison");
    }
    for (Kind kind : {Kind::scalar, Kind::prefetch, Kind::store, Kind::vector_load, Kind::vector_store}) {
        Harness h; install(h, obligation(kind, 101));
        require(h.redirect_after(100, false, true) && h.cancel_pending_for_redirect(100, false, true), "cancel setup failed");
        const auto &entry = h.entries[0];
        if (kind == Kind::scalar) h.loads.observe(0, completion(entry));
        else if (kind == Kind::prefetch) h.loads.observe(0, prefetch_completion(entry));
        else if (kind == Kind::store) h.stores.observe_address(entry.store.address_lane, store_completion(entry), h.cycle());
        else h.vectors.observe(0, vector_completion(entry));
        require(!h.check_components(), "unknown post-cancellation terminal escaped");
    }
    std::cout << "corrupt_tail_cases=30 late_terminal_rejections=5 unmatched_STD_excluded=1\n";
}

static void inputs()
{
    for (bool vector : {false, true}) for (bool self : {false, true}) {
        Harness h;
        auto &tracked = vector ? h.vector_release_inputs_ : h.scalar_release_inputs_;
        for (unsigned n = 0; n < 5; ++n) {
            Harness::ReleaseStoreInputs input{n < 2 ? 99U + n : 101U, false};
            input.registered = n != 3; input.authorized = n == 4;
            input.groups.push_back({100 + n, 0x80200000, 0x80200000, {1, 2}});
            tracked.emplace(n, input);
        }
        install(h, obligation(Kind::store, 101));
        require(memblock::scenarios::mixed::return_ordinary_redirect(h, 100, false, self), "input cancellation failed");
        require(tracked.size() == (self ? 2U : 3U) && tracked.count(0) && tracked.count(4) &&
            tracked.count(1) == !self && !tracked.count(2) && !tracked.count(3), "wrong store inputs canceled");
        require(h.memory_agent_.canceled == (self ? std::vector<std::uint64_t>{101, 102}
            : std::vector<std::uint64_t>{102}), "wrong ReleaseData orders canceled");
        h.entries[0].canceled = true;
        h.check_pending();
    }
    for (bool vector : {false, true}) {
        Harness h;
        auto &tracked = vector ? h.vector_release_inputs_ : h.scalar_release_inputs_;
        Harness::ReleaseStoreInputs input{101, false};
        input.registered = true; input.groups.push_back({7, 0, 0, {1}}); tracked.emplace(1, input);
        h.memory_agent_.reject = true;
        require(!memblock::scenarios::mixed::return_ordinary_redirect(h, 100, false, true) &&
            tracked.size() == 1 && h.cycle() == 3 && !h.error_.empty(), "manager cancellation failure lost provenance");
    }
    Harness invalid;
    require(!invalid.cancel_pending_for_redirect(160, false, true), "invalid input anchor accepted");
    std::cout << "input_lifecycle_cases=7\n";
}

using namespace memblock::scenarios::mixed;
struct StoreWork {
    unsigned kind = 2, address_retries = 0;
    bool address_issued = true, data_issued = false, restart_flushed = false;
    bool address_source = false, data_source = false, address_consumed = false, data_consumed = false;
    std::vector<bool> issued;
    std::vector<VectorMemoryTransaction> vectors;
    StoreTransaction store;
    StoreAddressReplay address_replay;
    bool needs_restart_flush() const { return false; }
};
struct CancellationCoverage {
    std::array<std::uint64_t, 29> stream_cancel{};
    std::array<std::uint64_t, 11> stream_sta{};
};
static bool return_and_classify(Harness &environment, std::vector<StoreWork> &live,
                                CancellationCoverage &coverage)
{
    Environment::MemoryViolationStats violation;
    violation.last.rob_value = 100; violation.last.level = true;
    std::ostringstream detail; std::string phase;
    const auto kills = [](const auto &) { return true; };
    // PRODUCTION_RETURN_AND_CANCELLATION_LEDGER
    return true;
}

static void feedback_states()
{
    unsigned cases = 0;
    // None, each edge's hit/miss, unrelated feedback and duplicate matching
    // feedback. All use the actual stream return-and-classification block.
    for (unsigned pattern = 0; pattern < 9; ++pattern) {
        Harness h; CancellationCoverage coverage;
        std::vector<StoreWork> live(1);
        auto &work = live[0];
        work.store = obligation(Kind::store, 101).store;
        require(work.address_replay.accept(0, 0), "STA accept setup failed");
        install(h, obligation(Kind::store, 101));
        h.feedback_.after_sample = [&](std::uint64_t edge) {
            const unsigned selected_edge = pattern <= 6 ? (pattern + 1U) / 2U : 2U;
            if (!pattern || (edge != selected_edge && !(pattern == 8 && edge == 3))) return;
            Environment::StoreSlowFeedbackSample sample;
            sample.lane = work.store.address_lane;
            sample.sq_value = work.store.sq + (pattern == 7);
            sample.sq_flag = work.store.sq_flag;
            sample.hit = pattern % 2; sample.cycle = edge;
            h.slow_feedback.sta_samples.push_back(sample);
        };
        const bool result = return_and_classify(h, live, coverage);
        if (pattern == 8) {
            require(!result, "duplicate matching canceled STA feedback accepted");
        } else {
            require(result && h.cycle() == 3, "canceled STA feedback classification failed");
            std::array<std::uint64_t, 29> expected{};
            expected[0] = expected[3] = expected[7] = 1;
            const bool matched = pattern >= 1 && pattern <= 6;
            expected[matched ? (pattern % 2 ? 18 : 19) : 20] = 1;
            require(coverage.stream_cancel == expected, "final canceled STA state missed propagation feedback");
            require(coverage.stream_sta[2] == (matched && pattern % 2) &&
                coverage.stream_sta[3] == (matched && !(pattern % 2)), "canceled STA observation counters disagree");
            require(!h.stores.pending(work.store), "canceled STA terminal survived boundary");
        }
        ++cases;
    }
    std::cout << "propagation_STA_feedback_cases=" << cases << " sampled_after_edges=1,2,3\n";
}

int main(int argc, char **argv)
{
    try {
        require(argc == 2, "one test mode required");
        const std::string mode = argv[1];
        if (mode == "tails") tails();
        else if (mode == "boundaries") boundaries();
        else if (mode == "corrupt") corrupt();
        else if (mode == "inputs") inputs();
        else if (mode == "feedback") feedback_states();
        else require(false, "unknown mode");
    } catch (const std::exception &e) { std::cerr << e.what() << '\n'; return 1; }
}
