#include "memblock_env.hpp"
#include <functional>
#include <iostream>

// This shell supplies public ports and deterministic edges only. Queue
// accounting, admission, and the order of cycle callbacks are production code.
// Synthetic counts below test host contracts; they are not DUT reachability
// evidence or a full-CPU stimulus generator.
void require(bool value, const char *why) {
    if (!value) throw std::runtime_error(why);
}

namespace memblock {
struct FixturePort {
    std::uint64_t contents = 0;
    void set(std::uint64_t value) { contents = value; }
    std::uint64_t value() const { return contents; }
    bool boolean() const { return contents != 0; }
};
struct FixtureDut {
    struct {
        struct { FixturePort lq_deq, sq_deq; } retirement;
        struct { FixturePort valid; } redirect;
    } queues;
    std::array<generated::LsqEnqueueObservation, generated::kLsqEnqueueLanes> enqueues{};
    std::uint64_t edges = 0;
    std::function<void(FixtureDut &)> on_step;
    auto sample_lsq_enqueue(unsigned lane) const { return enqueues.at(lane); }
    void refresh_comb() {}
    void step() { ++edges; if (on_step) on_step(*this); }
};
// PRODUCTION_QUEUE_ACCOUNTING

struct NoopAgent {
    void drive(FixtureDut &) {}
    void capture_before_tick(FixtureDut &) {}
    void update_after_tick() {}
};
struct NoopBridge {
    void capture_inputs() {}
    void check_before_edge() {}
    void advance_after_edge() {}
};
struct NoopFeedback {
    template<class... Args> void sample_before_edge(Args &&...) {}
    template<class... Args> void sample_after_edge(Args &&...) {}
};
class AdmissionFixture {
public:
    FailureState error_{"enqueue-admission-fixture", "contract"};
    QueueAccounting queues_{error_};
    FixtureDut dut_;
    NoopAgent memory_agent_, ptw_agent_, uncache_agent_;
    NoopBridge bridge_;
    NoopFeedback feedback_;
    int scoreboard_ = 0, store_scoreboard_ = 0, vector_scoreboard_ = 0;
    std::uint64_t key = 0;
    std::uint64_t cycle() const { return dut_.edges; }
    void set_failure_cycle() { error_.set_cycle(cycle()); }
    bool authorize_release_store_inputs() { return true; }
    void tick(bool monitor = true);
    bool wait_for_enqueue_capacity(unsigned lq, unsigned sq, unsigned timeout = 32);
    bool check_components() {
        queues_.check_driver_accounting();
        return error_.empty();
    }
    void edge(unsigned lq_deq = 0, unsigned sq_deq = 0,
              bool redirect = false, bool monitor = true) {
        dut_.queues.retirement.lq_deq.set(lq_deq);
        dut_.queues.retirement.sq_deq.set(sq_deq);
        dut_.queues.redirect.valid.set(redirect);
        tick(monitor);
        dut_.queues.retirement.lq_deq.set(0);
        dut_.queues.retirement.sq_deq.set(0);
        dut_.queues.redirect.valid.set(0);
    }
    // Like production enqueue helpers, bookkeeping follows the sampling edge.
    void enqueue(std::initializer_list<std::array<unsigned, 3>> lanes,
                 unsigned lq_deq = 0, unsigned sq_deq = 0) {
        unsigned lq = 0, sq = 0;
        dut_.enqueues = {};
        for (const auto &lane : lanes) {
            dut_.enqueues.at(lane[0]) = {.valid=true,
                .need_alloc=static_cast<std::uint8_t>(lane[1]),
                .num_ls_elem=static_cast<std::uint8_t>(lane[2])};
            if (lane[1] == 1) lq += lane[2];
            if (lane[1] == 2) sq += lane[2];
        }
        edge(lq_deq, sq_deq);
        dut_.enqueues = {};
        queues_.allocate_load(lq);
        if (sq) queues_.allocate_vector_store(++key, sq);
        require(check_components(), "public enqueue disagreed with driver allocation");
    }
    void fill(unsigned lq = 66, unsigned sq = 52) {
        if (lq && sq) enqueue({{0,1,lq},{5,2,sq}});
        else if (lq) enqueue({{0,1,lq}});
        else if (sq) enqueue({{5,2,sq}});
    }
    bool capacity(unsigned lq, unsigned sq) {
        const auto result = queues_.has_capacity(lq, sq);
        require(result.has_value(), "capacity query unexpectedly reported conservation error");
        return *result;
    }
    void expect_cadence(std::array<std::uint64_t, 6> expected) const {
        require(queues_.lsq_enqueue_cadence() == expected,
                "enqueue cadence did not match independent edge schedule");
    }
};
// PRODUCTION_METHODS
} // namespace memblock
using namespace memblock;

void no_credit_consecutive() {
    AdmissionFixture env;
    for (unsigned i = 0; i < 12; ++i) {
        const auto before = env.cycle();
        require(env.wait_for_enqueue_capacity(1,0), "existing LQ space was refused");
        require(env.cycle() == before, "successful ordinary admission inserted a quiet edge");
        env.enqueue({{i % 6,1,1}});
    }
    require(env.cycle() == 12 && env.queues_.lq_dequeued() == 0,
            "consecutive admission depended on retirement or hidden edges");
    env.expect_cadence({12,11,11,11,0,12});
}

void watermarks() {
    AdmissionFixture env; env.fill(65,51);
    require(env.capacity(1,1), "exact 66/52 post-admission watermark was rejected");
    require(!env.capacity(2,0), "LQ reserved headroom was admitted");
    require(!env.capacity(0,2), "SQ reserved headroom was admitted");
    env.enqueue({{1,1,1},{4,2,1}});
    require(env.capacity(0,0), "empty request at exact watermarks was rejected");
    require(!env.capacity(1,0) && !env.capacity(0,1), "full watermark admitted another entry");
    AdmissionFixture high_sq; high_sq.fill(0,53);
    require(!high_sq.capacity(1,0) && !high_sq.capacity(0,0),
            "zero SQ need bypassed the opposite-queue watermark");
    AdmissionFixture high_lq; high_lq.fill(67,0);
    require(!high_lq.capacity(0,1) && !high_lq.capacity(0,0),
            "zero LQ need bypassed the opposite-queue watermark");
    AdmissionFixture oversize;
    require(!oversize.wait_for_enqueue_capacity(67,0) && oversize.cycle() == 0,
            "oversize LQ request waited or succeeded");
    AdmissionFixture oversize_sq;
    require(!oversize_sq.wait_for_enqueue_capacity(0,53) && oversize_sq.cycle() == 0,
            "oversize SQ request waited or succeeded");
}

void dequeue_boundary() {
    AdmissionFixture env; env.fill();
    env.edge(2,3);
    require(env.queues_.lq_dequeued() == 2 && env.queues_.sq_dequeued() == 3,
            "public dequeue totals were delayed with admission credits");
    require(!env.capacity(1,0) && !env.capacity(0,1),
            "dequeue credit reused on observation edge");
    for (unsigned later = 1; later <= 2; ++later) {
        env.edge();
        require(!env.capacity(1,0) && !env.capacity(0,1),
                "dequeue credit reused before three further edges");
    }
    env.edge();
    require(env.capacity(2,3), "dequeue credit missing after three further edges");
    require(!env.capacity(3,0) && !env.capacity(0,4), "dequeue credit value was inflated");
    require(env.check_components(), "dequeue conservation failed");
}

void distinct_credit_ages() {
    AdmissionFixture env; env.fill();
    env.edge(1,1); // First observation D.
    env.edge(2,3); // Independent second observation D+1.
    env.edge();
    require(!env.capacity(1,1), "a younger credit matured with an older observation");
    env.edge(); // D+3: only the first credit may be used.
    require(env.capacity(1,1), "first distinct credit failed to mature");
    require(!env.capacity(2,0) && !env.capacity(0,2), "pending credits were merged across ages");
    env.edge();
    require(env.capacity(3,4), "second distinct credit failed to mature");
    require(!env.capacity(4,0) && !env.capacity(0,5), "distinct credit totals were inflated");
}

void explicit_cancellation_boundary() {
    AdmissionFixture env; env.fill();
    require(env.queues_.account_lq_cancellation(2), "legal explicit LQ cancellation failed");
    require(env.queues_.account_sq_cancellation(3), "legal explicit SQ cancellation failed");
    require(env.queues_.lq_canceled() == 2 && env.queues_.sq_canceled() == 3 &&
            env.queues_.lq_canceled_unobserved() == 2 && env.queues_.sq_canceled_unobserved() == 3,
            "explicit cancellation totals were not immediate");
    require(!env.capacity(1,0) && !env.capacity(0,1),
            "cancellation credit reused at registration");
    for (unsigned later = 1; later <= 2; ++later) {
        env.edge(); require(!env.capacity(1,0) && !env.capacity(0,1),
                            "cancellation credit matured early");
    }
    env.edge();
    require(env.capacity(2,3), "explicit cancellation credit missing at its third edge");
    require(!env.capacity(3,0) && !env.capacity(0,4), "explicit cancellation credit was inflated");
}

void observed_cancellation_boundary() {
    AdmissionFixture env; env.fill();
    require(env.queues_.observe_redirect_cancellation(3,4), "legal observed cancellation failed");
    require(env.queues_.lq_canceled() == 3 && env.queues_.sq_canceled() == 4 &&
            env.queues_.lq_redirect_canceled_observed() == 3 &&
            env.queues_.sq_redirect_canceled_observed() == 4 &&
            env.queues_.redirect_cancellation_events_observed() == 1,
            "observed cancellation provenance was delayed or lost");
    require(!env.capacity(1,0) && !env.capacity(0,1),
            "observed cancellation was immediate admission credit");
    env.edge(); env.edge();
    require(!env.capacity(1,0) && !env.capacity(0,1),
            "observed cancellation credit matured early");
    env.edge();
    require(env.capacity(3,4), "observed cancellation credit did not mature");
    require(!env.capacity(4,0) && !env.capacity(0,5), "observed cancellation credit was inflated");
}

void cancellation_during_recovery() {
    AdmissionFixture env; env.fill();
    env.edge(0,0,true);
    for (unsigned later = 1; later <= 4; ++later) env.edge();
    require(env.queues_.observe_redirect_cancellation(1,1), "late recovery cancellation failed");
    env.edge(); // Fifth edge after high; credit age one.
    require(!env.capacity(1,1), "recovery admitted before sixth edge");
    env.edge(); // Sixth edge: recovery ended, but credit age is only two.
    require(!env.capacity(1,1), "guard expiry reused immature cancellation credit");
    env.edge();
    require(env.capacity(1,1), "matured cancellation remained blocked after recovery");
}

void redirect_restart() {
    AdmissionFixture env;
    env.edge(0,0,true);
    require(!env.capacity(0,0), "redirect-high edge did not block admission");
    for (unsigned later = 1; later <= 5; ++later) {
        env.edge(); require(!env.capacity(1,1), "redirect guard expired before six further edges");
    }
    env.edge(); require(env.capacity(1,1), "redirect guard did not expire at sixth further edge");
    env.edge(0,0,true);
    env.edge(0,0,true); // A held level restarts from its final high edge.
    for (unsigned later = 1; later <= 3; ++later) env.edge();
    env.edge(0,0,true); // A new pulse during recovery also restarts it.
    for (unsigned later = 1; later <= 5; ++later) {
        env.edge(); require(!env.capacity(0,0), "repeated redirect failed to restart recovery");
    }
    env.edge(); require(env.capacity(1,1), "restarted redirect recovery did not expire exactly");
}

void redirect_capture() {
    AdmissionFixture env;
    env.dut_.on_step = [](auto &dut) { dut.queues.redirect.valid.set(0); };
    env.edge(0,0,true);
    require(!env.capacity(1,0), "cycle sampled redirect after the consuming edge");
    for (unsigned later = 1; later <= 5; ++later) {
        env.edge(0,0,false,false);
        require(!env.capacity(1,0), "unmonitored tick shortened redirect recovery");
    }
    env.edge(0,0,false,false);
    require(env.capacity(1,0), "unmonitored tick failed to advance recovery");
}

void immediate_conservation() {
    for (bool sq : {false,true}) {
        AdmissionFixture env; env.fill(1,1);
        env.edge(sq ? 0 : 2, sq ? 2 : 0, true);
        const auto result = env.queues_.has_capacity(0,0);
        require(!result.has_value(), "redirect guard hid an immediate over-retire error");
        require(env.error_.message().find("moved past allocation") != std::string::npos,
                "over-retire error lacked conservation diagnostic");
    }
    AdmissionFixture explicit_bad; explicit_bad.fill();
    require(!explicit_bad.queues_.account_lq_cancellation(67) &&
            !explicit_bad.queues_.account_sq_cancellation(53), "excess explicit cancellation accepted");
    require(explicit_bad.queues_.lq_canceled() == 0 && explicit_bad.queues_.sq_canceled() == 0,
            "failed explicit cancellation changed totals");
    for (unsigned i = 0; i < 4; ++i) explicit_bad.edge();
    require(!explicit_bad.capacity(1,0) && !explicit_bad.capacity(0,1),
            "failed explicit cancellation created admission credit");
    for (bool sq : {false,true}) {
        AdmissionFixture observed_bad; observed_bad.fill();
        require(!observed_bad.queues_.observe_redirect_cancellation(sq ? 1 : 67, sq ? 53 : 1),
                "one invalid observed cancellation side was accepted");
        require(observed_bad.queues_.lq_canceled() == 0 && observed_bad.queues_.sq_canceled() == 0 &&
                observed_bad.queues_.redirect_cancellation_events_observed() == 0,
                "failed observed cancellation partially changed totals");
        for (unsigned i = 0; i < 4; ++i) observed_bad.edge();
        require(!observed_bad.capacity(1,0) && !observed_bad.capacity(0,1),
                "failed observed cancellation partially staged credit");
    }
}

void batch_holes_and_cadence() {
    AdmissionFixture env;
    env.enqueue({{0,1,2},{2,2,3},{5,1,1}});
    env.expect_cadence({1,0,0,0,0,1}); // Current inputs are not older work.
    require(env.queues_.lq_enqueued_observed() == 3 && env.queues_.sq_enqueued_observed() == 3,
            "batch accounting counted lanes instead of num_ls_elem");
    env.enqueue({{4,1,1}});
    env.expect_cadence({2,1,1,1,1,2});
    env.enqueue({{1,2,2}},4,3); // These retirements are observed after this input.
    env.expect_cadence({3,2,2,2,2,3});
    env.enqueue({{0,1,1}}); // Only older SQ work remains immediately outstanding.
    env.expect_cadence({4,3,3,2,3,4});
    env.edge(0,0,false,false); // Real edge, with the optional monitor disabled.
    env.enqueue({{5,2,1}});
    env.expect_cadence({5,3,3,3,4,4});
    require(env.queues_.lsq_enqueue_widths_observed() ==
            std::array<std::uint64_t,6>{4,0,1,0,0,0}, "holey batch width counted prefix or elements");
    require(env.queues_.lsq_enqueue_lanes_observed() ==
            std::array<std::uint64_t,6>{2,1,1,0,1,2}, "holey batch lane ownership was lost");
}

void retired_work_cadence() {
    AdmissionFixture env;
    env.enqueue({{0,1,1}});
    require(env.queues_.account_lq_cancellation(1), "cadence LQ cancellation setup failed");
    env.enqueue({{3,2,1}}); // LQ credit is immature, but no older work remains.
    env.expect_cadence({2,1,0,0,0,2});
    require(env.queues_.account_sq_cancellation(1), "cadence SQ cancellation setup failed");
    env.edge();
    env.enqueue({{2,1,1}});
    env.expect_cadence({3,1,0,0,0,2});
}

void wait_for_mature_credit() {
    AdmissionFixture env; env.fill();
    env.edge(1,1);
    const auto before = env.cycle();
    require(env.wait_for_enqueue_capacity(1,1,4), "wait did not observe matured public credit");
    require(env.cycle() == before + 3, "wait added quiet edges beyond the credit maturity boundary");
    AdmissionFixture conserved; conserved.fill(1,1); conserved.edge(2,0);
    const auto bad_before = conserved.cycle();
    require(!conserved.wait_for_enqueue_capacity(0,0,4) && conserved.cycle() == bad_before,
            "wait advanced after an immediate accounting error");
}

int main(int argc, char **argv) {
    try {
        const std::string mode = argc > 1 ? argv[1] : "";
        if (mode == "no_credit_consecutive") no_credit_consecutive();
        else if (mode == "watermarks") watermarks();
        else if (mode == "dequeue_boundary") dequeue_boundary();
        else if (mode == "distinct_credit_ages") distinct_credit_ages();
        else if (mode == "explicit_cancellation_boundary") explicit_cancellation_boundary();
        else if (mode == "observed_cancellation_boundary") observed_cancellation_boundary();
        else if (mode == "cancellation_during_recovery") cancellation_during_recovery();
        else if (mode == "redirect_restart") redirect_restart();
        else if (mode == "redirect_capture") redirect_capture();
        else if (mode == "immediate_conservation") immediate_conservation();
        else if (mode == "batch_holes_and_cadence") batch_holes_and_cadence();
        else if (mode == "retired_work_cadence") retired_work_cadence();
        else if (mode == "wait_for_mature_credit") wait_for_mature_credit();
        else throw std::runtime_error("unknown enqueue admission fixture mode");
        return 0;
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
