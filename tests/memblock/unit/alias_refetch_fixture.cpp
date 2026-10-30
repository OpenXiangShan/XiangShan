#include "scenarios/alias_refetch.hpp"

namespace memblock {
#include "environment/scoreboards.inc"
}

#include <cassert>
#include <iostream>
#include <string>

using namespace memblock;
using namespace memblock::scenarios;

// Independent public-event script. Values and producer events are fixed here;
// the actual LoadScoreboard still decides which completions are permissible.
struct AliasWitness {
    static constexpr std::uint64_t bytes = 0x8877665544332211ULL;
    LoadScoreboard scoreboard;
    Environment::MemoryViolationStats violation{};
    LoadTransaction original{
        .address = 0x30000200, .oracle_address = 0x500001200,
        .op = LoadOp::ld, .rob = 159, .rob_flag = true, .lq = 71, .sq = 5, .pdest = 154,
        .lane = 1, .predecode_rvc = true,
        .ftq_ptr = 63, .ftq_offset = 7,
    };
    std::string fault;
    bool refetch = true, cancel_lq = false, correct_first = false;
    unsigned expectations = 0, returns = 0, enqueues = 0;
    std::uint64_t now = 100, allocated = 71, dequeued = 71;
    std::uint64_t canceled = 0, sq_cancel = 0, unobserved = 0;
    std::vector<LoadTransaction> offered;
    std::vector<std::uint64_t> expected;

    bool tick(unsigned n = 1) { now += n; return scoreboard.ok(); }
    auto cycle() const { return now; }
    const auto &memory_violation_stats() const { return violation; }
    auto scalar_load_canceled_observed() const { return scoreboard.canceled_observed(); }
    auto writebacks() const { return scoreboard.observed() + (fault == "extra-count" && returns ? 1 : 0); }
    auto lq_allocated() const { return allocated; }
    auto lq_dequeued() const { return dequeued; }
    auto lq_canceled() const { return canceled; }
    auto lq_redirect_canceled_observed() const { return canceled; }
    auto lq_canceled_unobserved() const { return unobserved; }
    auto sq_canceled() const { return sq_cancel; }
    void expect_load_data(const LoadTransaction &t, std::uint64_t data) {
        ++expectations; expected.push_back(data); offered.push_back(t);
        scoreboard.expect(t, data);
    }
    bool set_rob_head_state(std::uint8_t rob, bool flag, bool pendingst) {
        assert(rob == original.rob && flag == original.rob_flag && !pendingst);
        return tick();
    }
    bool enqueue_load(const LoadTransaction &t) {
        const auto cursor = allocated - canceled;
        if (t.lq != lq_pointer_value(cursor) || t.lq_flag != lq_pointer_flag(cursor) ||
            t.rob != original.rob || t.rob_flag != original.rob_flag ||
            t.pdest != original.pdest || t.sq != original.sq ||
            t.ftq_ptr != original.ftq_ptr || t.ftq_offset != original.ftq_offset ||
            t.predecode_rvc != original.predecode_rvc) return false;
        ++allocated; ++enqueues;
        return tick();
    }
    bool issue_load(const LoadTransaction &, unsigned) { return tick(); }
    bool run_until_complete(unsigned) {
        const bool fresh = expectations == 2;
        auto t = offered.back();
        generated::ScalarLoadWriteback w{};
        w.valid = true; w.rob_value = t.rob; w.rob_flag = t.rob_flag;
        w.pdest = t.pdest; w.rf_wen = true; w.trigger = t.expected_trigger;
        w.data = (fresh || correct_first || !refetch) ? bytes : bytes ^ 0xffff;
        if (fault == "wrong-final" && fresh) w.data ^= 0xff;
        if (fault == "wrong-no-redirect" && !fresh) w.data ^= 0xff;
        if (fault == "missing-final" && fresh) return false;
        if (fault == "bad-metadata" && !fresh) w.pdest ^= 1;
        generated::MemoryViolation v{};
        if (!fresh && refetch) {
            v.valid = true; v.level = true; v.rob_value = original.rob;
            v.rob_flag = original.rob_flag; v.ftq_value = original.ftq_ptr;
            v.ftq_offset = original.ftq_offset; v.is_rvc = original.predecode_rvc;
            if (fault == "bad-valid") v.valid = false;
            if (fault == "bad-level") v.level = false;
            if (fault == "bad-rob") --v.rob_value;
            if (fault == "bad-rob-wrap") v.rob_flag = false;
            if (fault == "bad-ftq-wrap") v.ftq_flag = true;
            if (fault == "bad-ftq") --v.ftq_value;
            if (fault == "bad-offset") --v.ftq_offset;
            if (fault == "bad-rvc") v.is_rvc = false;
            violation.last = v; violation.last_cycle = now + 3;
            violation.count += fault == "multiple-redirects" ? 2 : v.valid;
        }
        now += 4;
        if (fault == "late-return") now += 2;
        if (fresh && fault == "final-redirect") {
            v = violation.last;
            w.data ^= 0xff; // A second matching redirect cannot excuse stale bytes.
        }
        scoreboard.observe(t.lane, w, v);
        if (!(cancel_lq && !fresh)) ++dequeued;
        if (fault == "extra-allocation" && fresh) { ++allocated; ++dequeued; }
        return scoreboard.ok() && scoreboard.done();
    }
    bool run_cycles(unsigned n) { return tick(n); }
    bool redirect_after(std::uint8_t rob, bool wrap, bool level) {
        assert(rob == original.rob && wrap == original.rob_flag && level);
        assert(now == violation.last_cycle + 2);
        ++returns;
        if (cancel_lq) ++canceled;
        return tick(3);
    }
    bool run_until_queues_retired(unsigned) {
        if (expectations == 2 || !refetch) {
            if (fault == "lost-entry") --dequeued;
            if (fault == "sq-cancel") ++sq_cancel;
            if (fault == "unobserved") ++unobserved;
            if (fault == "late-redirect") ++violation.count;
            if (fault == "duplicate") {
                generated::ScalarLoadWriteback w{};
                w.valid = true; w.rob_value = original.rob;
                w.rob_flag = original.rob_flag; w.pdest = original.pdest;
                w.rf_wen = true; w.data = bytes; w.trigger = original.expected_trigger;
                scoreboard.observe(original.lane, w);
            }
        }
        return tick();
    }
    bool run_until_sbuffer_empty(unsigned) { return tick(); }
};

static void load_case(const std::string &mode)
{
    AliasWitness h;
    if (mode == "direct") h.refetch = false;
    else if (mode == "correct-refetch") h.correct_first = true;
    else if (mode == "cancel-refetch") h.cancel_lq = true;
    else if (mode != "dequeued-refetch") {
        h.fault = mode;
        // Correct provisional bytes let the helper independently reject bad
        // redirect evidence rather than relying on a data mismatch rejection.
        if ((mode.starts_with("bad-") && mode != "bad-valid") ||
            mode == "multiple-redirects" || mode == "late-return")
            h.correct_first = true;
        if (mode == "wrong-no-redirect") h.refetch = false;
    }
    AliasRefetchResult result;
    const bool pass = complete_alias_load(h, h.original, AliasWitness::bytes, result);
    const bool valid = h.fault.empty();
    assert(pass == valid);
    if (valid) {
        assert(result.refetched == h.refetch && h.returns == unsigned(h.refetch));
        assert(result.discarded == unsigned(h.refetch && !h.correct_first));
        assert(h.expectations == 1U + h.refetch && h.enqueues == h.expectations);
        assert(h.expected == std::vector<std::uint64_t>(h.expectations, AliasWitness::bytes));
        assert(h.lq_dequeued() + h.lq_canceled() == h.lq_allocated());
        assert(h.offered.front().tolerate_canceled_result);
        if (h.refetch) {
            assert(!h.offered.back().tolerate_canceled_result);
            assert(h.offered.back().lq == (h.cancel_lq ? 71 : 0));
            assert(h.offered.back().lq_flag == !h.cancel_lq);
        }
    }
    std::cout << mode << " accepted=" << pass << " expectations=" << h.expectations
              << " returns=" << h.returns << '\n';
}

struct StoreWitness {
    StoreTransaction transaction;
    std::vector<std::string> events;
    bool head = false, enqueued = false, address = false, data = false, complete = false;
    unsigned guard = 0;
    void expect_store(const StoreTransaction &) { events.push_back("expect"); }
    bool set_rob_head_state(std::uint8_t r, bool f, bool pendingst) {
        if (events.back() == "retire") {
            const bool wraps = transaction.rob == 159;
            assert(r == (wraps ? 0 : transaction.rob + 1) &&
                f == (wraps ? !transaction.rob_flag : transaction.rob_flag) && !pendingst);
            head = false;
            events.push_back("next-head");
        } else {
            assert(r == transaction.rob && f == transaction.rob_flag && pendingst);
            head = true; events.push_back("head");
        }
        return true;
    }
    bool enqueue_store(const StoreTransaction &t) {
        assert(head && t.lq == 3 && t.sq == 1); enqueued = true;
        events.push_back("dispatch"); return true;
    }
    auto sq_allocated() const { return 2U; }
    auto sq_canceled() const { return 0U; }
    bool issue_store_address_until_tlb_hit(const StoreTransaction &, unsigned) {
        assert(enqueued); address = true; events.push_back("sta-hit"); return true;
    }
    bool issue_store_data(const StoreTransaction &, unsigned) {
        assert(enqueued); data = true; events.push_back("std"); return true;
    }
    bool run_until_store_complete(unsigned) {
        assert(address && data); complete = true; events.push_back("writeback"); return true;
    }
    bool run_cycles(unsigned n) { guard += n; events.push_back("guard"); return true; }
    bool retire_store(const StoreTransaction &) {
        assert(complete && head && guard >= 2); events.push_back("retire"); return true;
    }
    bool run_until_sq_dequeued(std::uint64_t target, unsigned) {
        assert(target == 2 && events.back() == "next-head" && !head);
        events.push_back("dequeue"); return true;
    }
};

int main(int argc, char **argv)
{
    assert(argc == 2);
    if (std::string(argv[1]) == "store") {
        for (unsigned flag = 0; flag < 2; ++flag) for (unsigned rob = 0; rob < 160; ++rob) {
            StoreWitness h;
            const StoreTransaction t{.rob = static_cast<std::uint8_t>(rob),
                .rob_flag = bool(flag), .lq = 3, .sq = 1};
            h.transaction = t;
            assert(retire_alias_store(h, t));
            assert(h.events == std::vector<std::string>({"expect", "head", "dispatch",
                "sta-hit", "std", "writeback", "guard", "retire", "next-head", "dequeue"}));
        }
        std::cout << "store legal head and retirement order: 320 identities\n";
    } else load_case(argv[1]);
}
