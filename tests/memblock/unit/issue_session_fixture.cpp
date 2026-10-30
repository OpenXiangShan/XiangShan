// Host contract witnesses for the persistent public issue API. Production
// transport, tick, payload mapping, replay selection and store scoreboard are
// compiled below; the synthetic manager controls only externally sampled pins.
// PIN_WITNESSES
#include "environment/scoreboards.hpp"
#include <cstdlib>
#include <functional>
#include <numeric>

struct Adapter : PinAdapter {
    struct Fire { std::uint64_t edge; unsigned slot; Bytes bits; };
    std::vector<Fire> fires;
    std::function<void()> on_edge, after_edge;
    struct {
        struct {
            Port priv_dmode, priv_imode, priv_virt, priv_sum, priv_mxr, priv_vsum, priv_vmxr;
            Port satp_ppn, vsatp_ppn, hgatp_ppn, satp_mode, vsatp_mode, hgatp_mode;
        } csr;
    } translation;
    void step() {
        if (on_edge) on_edge();
        for (unsigned slot = 0; slot < valid.size(); ++slot)
            if (valid[slot] && ready[slot]) fires.push_back({edge, slot, payload[slot]});
        PinAdapter::step();
        if (after_edge) after_edge();
    }
};
struct Agent {
    std::function<void()> drive_hook;
    void drive(Adapter &) { if (drive_hook) drive_hook(); }
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
struct VectorScores {
    bool active = true;
    bool pending(const VectorMemoryTransaction &) const { return active; }
};
// Production replay request layout and identity selection are inserted here.
// REPLAY_REQUEST
struct Feedback {
    Environment::IqSlowFeedbackStats stats;
    std::deque<VectorReplayRequest> vector_replay_requests_;
    unsigned std_pulses = 0, samples_per_tick = 1;
    bool observe_std = true;
    const auto &iq_slow_feedback_stats() const { return stats; }
    std::optional<VectorReplayRequest> take_replay_for(const VectorMemoryTransaction &);
    void sample_before_edge(Adapter &dut, std::uint64_t edge, int, StoreScoreboard &stores, VectorScores &) {
        // Deliberately update feedback after the acceptance callback. Event
        // cursors must point before feedback from this edge, not after it.
        for (unsigned n = 0; n < samples_per_tick; ++n) {
            Environment::StoreSlowFeedbackSample sample;
            sample.cycle = edge; stats.sta_samples.push_back(sample);
        }
        if (!observe_std) return;
        for (unsigned lane = 0; lane < 2; ++lane) {
            if (!dut.valid[5 + lane] || !dut.ready[5 + lane]) continue;
            generated::ScalarStoreWriteback wb;
            wb.valid = true; wb.rob_value = dut.payload[5 + lane][6];
            stores.observe_data(lane, wb, edge);
            ++std_pulses;
        }
    }
    void sample_after_edge(Adapter &, std::uint64_t) {}
};
struct Harness {
    Adapter dut_;
    Driver *retained_issue_driver_ = nullptr;
    std::unique_ptr<Driver> ordinary_issue_driver_;
    std::unordered_map<std::uint64_t, OrdinaryIssueEvent> issue_acceptance_context_;
    FailureState error_{"issue-session-host", "public-api-contract"};
    Agent memory_agent_, ptw_agent_, uncache_agent_;
    Bridge bridge_; Queues queues_; Feedback feedback_;
    int scoreboard_ = 0;
    VectorScores vector_scoreboard_;
    StoreScoreboard store_scoreboard_;
    struct Inputs {
        bool vector = false, address = false, data = false, head = false;
        bool registered = false, authorized = false;
        unsigned registrations = 0, authorizations = 0;
    };
    struct Callback { std::uint64_t key, edge; bool vector, address; };
    std::map<std::uint64_t, Inputs> release_inputs;
    std::vector<Callback> callbacks;
    bool reject_input = false, reject_authorization = false;
    bool accept_release_store_input(std::uint64_t key, bool vector, bool address) {
        const auto found = release_inputs.find(key);
        if (found == release_inputs.end() || found->second.vector != vector || reject_input) {
            error_.record("accepted input has no matching release declaration"); return false;
        }
        auto &in = found->second;
        (address ? in.address : in.data) = true;
        callbacks.push_back({key, cycle(), vector, address});
        if (!in.registered && in.address && (vector || in.data)) {
            in.registered = true; ++in.registrations;
        }
        return true;
    }
    bool authorize_release_store_inputs() {
        for (auto &[key, in] : release_inputs) {
            if (!in.registered || !in.head || in.authorized) continue;
            if (reject_authorization) { error_.record("head authorization rejected"); return false; }
            in.authorized = true; ++in.authorizations;
        }
        return true;
    }
    void expect_store(const StoreTransaction &t, bool head = true) {
        store_scoreboard_.expect(t); release_inputs.emplace(scalar_store_key(t), Inputs{false, false, false, head});
    }
    void expect_vector_store(const VectorMemoryTransaction &t, bool head = true) {
        release_inputs.emplace(vector_store_key(t), Inputs{true, false, false, head});
    }
    void set_failure_cycle() {}
    bool check_components() const { return error_.empty() && store_scoreboard_.ok(); }
    std::uint64_t cycle() const { return dut_.edge; }
    static std::uint64_t scalar_store_key(const StoreTransaction &);
    static std::uint64_t vector_store_key(const VectorMemoryTransaction &);
    static generated::VectorMemoryIssue make_vector_memory_issue(const VectorMemoryTransaction &);
    void tick(bool monitor = true, const std::function<void()> &before_edge = {});
    bool require_issue_session();
    bool capture_issue_acceptance(std::uint64_t, std::uint64_t);
    bool begin_issue_session();
    bool end_issue_session();
    bool issue_session_empty() const;
    bool issue_session_available(OrdinaryIssueKind, unsigned) const;
    bool offer_load(std::uint64_t, const LoadTransaction &);
    bool offer_prefetch(std::uint64_t, const PrefetchTransaction &);
    bool offer_store_address(std::uint64_t, const StoreTransaction &);
    bool offer_store_data(std::uint64_t, const StoreTransaction &);
    bool offer_vector(std::uint64_t, const VectorMemoryTransaction &);
    std::vector<OrdinaryIssueEvent> take_issue_events();
    bool poll_vector_replay(const VectorMemoryTransaction &, VectorMemoryTransaction &, bool &);
};
// PRODUCTION_METHODS

using PublicKind = OrdinaryIssueKind;
using PublicOutcome = OrdinaryIssueOutcome;
void event_is(const OrdinaryIssueEvent &e, std::uint64_t tag, std::uint64_t edge,
              PublicKind kind, unsigned lane, PublicOutcome outcome, bool killed = false) {
    assert(e.tag == tag && e.edge == edge && e.kind == kind && e.lane == lane);
    assert(e.outcome == outcome && e.killed == killed);
}
OrdinaryIssueEvent only_event(Harness &h) {
    const auto events = h.take_issue_events();
    assert(events.size() == 1 && h.take_issue_events().empty());
    return events.front();
}
LoadTransaction load(unsigned lane = 0, unsigned position = 31) {
    LoadTransaction t;
    t.address = 0x80200378; t.immediate = -23; t.pc = 0x1abcd;
    t.predecode_rvc = true; t.ftq_ptr = 31; t.ftq_offset = 5;
    t.op = LoadOp::lwu; t.rf_wen = false; t.fp_wen = true; t.pdest = 89;
    t.rob = position % 160; t.rob_flag = position / 160 % 2;
    t.lq = 71; t.lq_flag = true; t.sq = 55; t.sq_flag = true;
    t.store_set_hit = true; t.wait_for_rob_flag = true; t.wait_for_rob_value = 157;
    t.load_wait_bit = true; t.load_wait_strict = true; t.lane = lane;
    return t;
}
StoreTransaction store(unsigned position = 299) {
    StoreTransaction t;
    t.address = 0x80200178; t.immediate = -17; t.data = 0x8877665544332211ULL;
    t.rob = position % 160; t.rob_flag = position / 160 % 2;
    t.sq = 55; t.sq_flag = true; t.address_lane = t.data_lane = 1;
    return t;
}
VectorMemoryTransaction vector(unsigned lane = 0, unsigned position = 93) {
    VectorMemoryTransaction t;
    t.address = 0x80200280; t.rob = position % 160; t.rob_flag = position / 160 % 2;
    t.pdest = 57; t.lane = lane; t.lq = 70; t.lq_flag = true; t.sq = 54; t.sq_flag = true;
    t.eew = 1; t.vsew = 2; t.vl = 7; t.vstart = 1; t.vlmul = 0;
    t.vm = false; t.mask_bits = 0xa591; t.mask_high = {0x23456789abcd0000ULL, 0xfedcba9876543210ULL};
    t.vma = true; t.vta = true; t.ftq_ptr = 39; t.ftq_offset = 6;
    t.addressing = VectorAddressingMode::indexed_unordered;
    for (unsigned i = 0; i < 16; ++i) { t.data[i] = 3 + i; t.index[i] = 97 - i; }
    return t;
}
// Expected payloads below use public field meanings and byte encodings, not
// production payload constructors, retained slots or acceptance callbacks.
Bytes expected_load(const LoadTransaction &t) {
    return {*t.pc, 1, 31, 5, static_cast<unsigned>(LoadOp::lwu), 0, 1,
        std::uint32_t(-23), 89, t.rob_flag, t.rob, 1, 71, 1, 55, 1, 1, 157, 1, 1,
        t.address + 23};
}
Bytes expected_store(const StoreTransaction &t, bool data) {
    return {kFuTypeStore, static_cast<unsigned>(StoreOp::sd), data ? 0U : std::uint32_t(-17),
        0, 0, t.rob_flag, t.rob, 1, 55, data ? t.data : t.address + 17};
}
void check_vector_bits(const Bytes &bits, const VectorMemoryTransaction &t) {
    assert(bits.size() == 125);
    assert(bits[0] == 39 && bits[1] == 6 && bits[2] == (t.store ? kFuTypeVectorStore : kFuTypeVectorLoad));
    assert(bits[4] == !t.store && bits[9] == 2 && bits[10] == 0 && bits[11] == 0 && bits[12] == 1);
    assert(bits[18] == 57 && bits[19] == t.rob_flag && bits[20] == t.rob);
    assert(bits[21] == 1 && bits[22] == 70 && bits[23] == 1 && bits[24] == 54);
    assert(bits[26] == t.is_part_replay && bits[27] == t.replay_mask && bits[28] == t.replay_mb_index);
    for (unsigned i = 0; i < 16; ++i) {
        const unsigned mask = i < 2 ? (0xa591U >> (8 * i)) & 255U :
            i < 10 ? (0x23456789abcd0000ULL >> (8 * (i - 2))) & 255U :
            (0xfedcba9876543210ULL >> (8 * (i - 10))) & 255U;
        assert(bits[29 + i] == mask && bits[45 + 48 + i] == mask);
        assert(bits[45 + 16 + i] == 97 - i && bits[45 + 32 + i] == 3 + i);
        assert(bits[45 + i] == (i < 8 ? (t.address >> (8 * i)) & 255U : 0));
        assert(bits[45 + 64 + i] == (i == 0 ? 7U : 0U));
    }
}
void finish_store(Harness &h, const StoreTransaction &t) {
    generated::ScalarStoreWriteback wb;
    wb.valid = true; wb.rob_value = t.rob; wb.rob_flag = t.rob_flag;
    h.store_scoreboard_.observe_address(t.address_lane, wb, h.cycle());
    assert(h.store_scoreboard_.done() && h.store_scoreboard_.observed() == 1 && h.check_components());
}

// This table describes the externally scripted CSR pins, independently of the
// event type and the production acceptance callback. Distinct instruction/data
// modes and host/virtual bits make field substitutions observable.
struct ExpectedAcceptancePins {
    unsigned data_mode, instruction_mode;
    bool virtualized, sum, mxr, vsum, vmxr;
    std::array<std::uint64_t, 3> roots;
    std::array<unsigned, 3> modes;
};
void drive_acceptance_pins(Adapter &dut, const ExpectedAcceptancePins &pins) {
    auto &csr = dut.translation.csr;
    csr.priv_dmode.data = pins.data_mode; csr.priv_imode.data = pins.instruction_mode;
    csr.priv_virt.data = pins.virtualized; csr.priv_sum.data = pins.sum; csr.priv_mxr.data = pins.mxr;
    csr.priv_vsum.data = pins.vsum; csr.priv_vmxr.data = pins.vmxr;
    csr.satp_ppn.data = pins.roots[0]; csr.vsatp_ppn.data = pins.roots[1]; csr.hgatp_ppn.data = pins.roots[2];
    csr.satp_mode.data = pins.modes[0]; csr.vsatp_mode.data = pins.modes[1]; csr.hgatp_mode.data = pins.modes[2];
}
void translation_is(const OrdinaryIssueEvent &event, const ExpectedAcceptancePins &expected) {
    // Assert every input snapshot, including roots/modes that are inactive for
    // this edge's privilege. No expected field comes from the returned event.
    for (unsigned slot = 0; slot < 3; ++slot) {
        if (event.translation_roots[slot] != expected.roots[slot] ||
            event.translation_modes[slot] != expected.modes[slot]) {
            std::cerr << "issue-session-translation-check tag=" << event.tag << " edge=" << event.edge
                      << " slot=" << slot << '\n';
            std::abort();
        }
    }
}
void privilege_is(const OrdinaryIssueEvent &event, const ExpectedAcceptancePins &expected) {
    const auto &actual = event.privilege;
    if (actual.data_mode != expected.data_mode || actual.instruction_mode != expected.instruction_mode ||
        actual.virtualized != expected.virtualized || actual.sum != expected.sum || actual.mxr != expected.mxr ||
        actual.vsum != expected.vsum || actual.vmxr != expected.vmxr) {
        std::cerr << "issue-session-privilege-check tag=" << event.tag << " edge=" << event.edge << '\n';
        std::abort();
    }
}

void acceptance_privilege_capture() {
    Harness h;
    const auto l = load(0), younger = load(2, 32); const auto s = store(); const auto v = vector(0);
    PrefetchTransaction p; p.lane = 1;
    h.expect_store(s); assert(h.begin_issue_session());
    const ExpectedAcceptancePins offered{3, 3, false, false, false, false, false,
        {0x9aa00110ULL, 0x9bb00220ULL, 0x9cc00330ULL}, {0, 0, 0}};
    const ExpectedAcceptancePins pre_manager{3, 0, true, true, true, true, true,
        {0x8aa00110ULL, 0x8bb00220ULL, 0x8cc00330ULL}, {8, 0, 8}};
    const ExpectedAcceptancePins after_edge{3, 1, false, true, true, false, false,
        {0x7aa00110ULL, 0x7bb00220ULL, 0x7cc00330ULL}, {0, 8, 0}};
    // Offer and consumption deliberately have identical roots/modes; neither
    // can substitute for any of the actual manager-driven acceptance inputs.
    const ExpectedAcceptancePins consumed{3, 3, true, false, false, true, false,
        {0x9aa00110ULL, 0x9bb00220ULL, 0x9cc00330ULL}, {0, 0, 0}};
    const std::array<ExpectedAcceptancePins, 6> pins{{
        {1, 3, false, false, true, true, false,
            {0x123456610ULL, 0x234567620ULL, 0x345678630ULL}, {0, 8, 0}}, // Fully stalled edge.
        {0, 3, false, false, true, false, true,
            {0x123456710ULL, 0x234567720ULL, 0x345678730ULL}, {8, 0, 0}}, // U load.
        {1, 0, false, true, false, false, true,
            {0x123456810ULL, 0x234567820ULL, 0x345678830ULL}, {8, 0, 8}}, // S+SUM prefetch and STD.
        {0, 1, true, false, false, true, false,
            {0x123456910ULL, 0x234567920ULL, 0x345678930ULL}, {0, 8, 8}}, // VU vector load.
        {1, 3, true, false, true, true, true,
            {0x123456a10ULL, 0x234567a20ULL, 0x345678a30ULL}, {8, 8, 0}}, // VS+vSUM STA.
        {1, 0, false, false, false, false, false,
            {0x123456b10ULL, 0x234567b20ULL, 0x345678b30ULL}, {0, 0, 8}},
    }};
    drive_acceptance_pins(h.dut_, offered);
    assert(h.offer_load(201, l) && h.offer_prefetch(202, p));
    assert(h.offer_store_address(203, s) && h.offer_store_data(204, s));
    assert(h.offer_vector(205, v) && h.offer_load(206, younger));
    // A pre-tick sample sees a decoy. Later manager drive changes both ready
    // and privilege before the actual acceptance callback.
    h.memory_agent_.drive_hook = [&] { drive_acceptance_pins(h.dut_, pre_manager); };
    h.ptw_agent_.drive_hook = [&] {
        const auto edge = h.cycle(); assert(edge < pins.size());
        drive_acceptance_pins(h.dut_, pins[edge]); h.dut_.ready.fill(false);
        h.dut_.ready[0] = edge == 1; h.dut_.ready[1] = h.dut_.ready[6] = edge == 2;
        h.dut_.ready[7] = edge == 3; h.dut_.ready[4] = edge == 4; h.dut_.ready[2] = edge == 5;
        h.queues_.stats.pending = edge == 2 || edge == 4;
    };
    h.dut_.after_edge = [&] { drive_acceptance_pins(h.dut_, after_edge); };
    for (unsigned edge = 0; edge < pins.size(); ++edge) {
        drive_acceptance_pins(h.dut_, offered);
        h.tick(); assert(h.cycle() == edge + 1 && h.check_components());
        if (!edge) assert(h.take_issue_events().empty() && h.dut_.fires.empty());
    }
    // All accepted events stay queued across subsequent CSR changes and are
    // consumed together only after the last acceptance edge.
    drive_acceptance_pins(h.dut_, consumed);
    const auto events = h.take_issue_events();
    assert(events.size() == 6 && h.dut_.fires.size() == 6 && h.take_issue_events().empty());
    std::map<std::uint64_t, OrdinaryIssueEvent> seen;
    for (const auto &event : events) assert(seen.emplace(event.tag, event).second);
    event_is(seen.at(201), 201, 1, PublicKind::load, 0, PublicOutcome::accepted);
    event_is(seen.at(202), 202, 2, PublicKind::load, 1, PublicOutcome::accepted);
    event_is(seen.at(203), 203, 4, PublicKind::store_address, 1, PublicOutcome::accepted);
    event_is(seen.at(204), 204, 2, PublicKind::store_data, 1, PublicOutcome::accepted);
    event_is(seen.at(205), 205, 3, PublicKind::vector, 0, PublicOutcome::accepted);
    event_is(seen.at(206), 206, 5, PublicKind::load, 2, PublicOutcome::accepted);
    for (const auto &[tag, event] : seen) {
        privilege_is(event, pins[event.edge]);
        translation_is(event, pins[event.edge]);
        assert(event.feedback_cursor == event.edge);
        assert(event.committed_pending == (event.edge == 2 || event.edge == 4));
    }
    // Consumed value copies also survive subsequent public CSR pin changes.
    drive_acceptance_pins(h.dut_, offered);
    for (const auto &[tag, event] : seen) {
        privilege_is(event, pins[event.edge]); translation_is(event, pins[event.edge]);
    }
    finish_store(h, s); assert(h.issue_session_empty() && h.end_issue_session());
    std::cout << "privilege_acceptances=6 manager_pin_boundaries=6 stalled_edges=1 same_edge_contexts=2 "
                 "delayed_event_consumption=6 privilege_fields=7 translation_roots=3 translation_modes=3 "
                 "S_SUM_active_SATP=2 U=1 VU=1 S_SUM=2 VS_vSUM=1\n";
}

void progress_and_value_capture() {
    Harness h;
    const auto l = load(0), younger = load(2, 32); const auto s = store(); const auto v = vector(1);
    h.expect_store(s); assert(h.begin_issue_session());
    {
        auto local_l = l; auto local_s = s; auto local_v = v;
        assert(h.offer_load(11, local_l) && h.offer_store_address(12, local_s));
        assert(h.offer_store_data(13, local_s) && h.offer_vector(14, local_v));
        local_l = {}; local_s = {}; local_v = {};
    }
    PrefetchTransaction p;
    p.lane = 1; p.rob = 42; p.rob_flag = true; p.lq = 11; p.sq = 43;
    p.lq_flag = true; p.sq_flag = true; p.address = 0x80200a00; p.immediate = 2047;
    assert(h.offer_prefetch(15, p));
    assert(h.cycle() == 0 && h.dut_.fires.empty() && h.callbacks.empty() && h.take_issue_events().empty());
    assert(!h.issue_session_empty() && !h.issue_session_available(PublicKind::load, 0));
    assert(h.issue_session_available(PublicKind::load, 2));
    // Ready is rewritten by the manager during tick, after the retained owner
    // drives. A pre-tick ready peek predicts the wrong acceptance schedule.
    h.dut_.ready.fill(true);
    h.memory_agent_.drive_hook = [&] {
        h.dut_.ready.fill(false);
        const auto e = h.cycle();
        h.dut_.ready[0] = e >= 5; h.dut_.ready[1] = e == 0; h.dut_.ready[2] = e == 2;
        h.dut_.ready[4] = e >= 3; h.dut_.ready[6] = e >= 1; h.dut_.ready[8] = e >= 4;
        h.queues_.stats.pending = e < 3 ? 2 : 0;
    };
    std::map<std::uint64_t, OrdinaryIssueEvent> seen;
    for (unsigned e = 0; e < 6; ++e) {
        if (e == 2) { assert(h.offer_load(16, younger)); assert(h.cycle() == 2); }
        h.dut_.on_edge = [&] {
            if (h.dut_.valid[0]) assert(h.dut_.payload[0] == expected_load(l));
            if (h.dut_.valid[2]) assert(h.dut_.payload[2] == expected_load(younger));
            if (h.dut_.valid[4]) assert(h.dut_.payload[4] == expected_store(s, false));
            if (h.dut_.valid[6]) assert(h.dut_.payload[6] == expected_store(s, true));
            if (h.dut_.valid[8]) check_vector_bits(h.dut_.payload[8], v);
            const auto &in = h.release_inputs.at(Harness::scalar_store_key(s));
            assert(in.registered == (h.cycle() >= 3) && in.authorized == (h.cycle() >= 3));
        };
        h.tick(); assert(h.cycle() == e + 1 && h.check_components());
        for (const auto &event : h.take_issue_events()) assert(seen.emplace(event.tag, event).second);
        assert(h.take_issue_events().empty());
    }
    assert(seen.size() == 6 && h.dut_.fires.size() == 6);
    event_is(seen.at(11), 11, 5, PublicKind::load, 0, PublicOutcome::accepted);
    event_is(seen.at(12), 12, 3, PublicKind::store_address, 1, PublicOutcome::accepted);
    event_is(seen.at(13), 13, 1, PublicKind::store_data, 1, PublicOutcome::accepted);
    event_is(seen.at(14), 14, 4, PublicKind::vector, 1, PublicOutcome::accepted);
    event_is(seen.at(15), 15, 0, PublicKind::load, 1, PublicOutcome::accepted);
    event_is(seen.at(16), 16, 2, PublicKind::load, 2, PublicOutcome::accepted);
    for (const auto &[tag, event] : seen) {
        assert(event.feedback_cursor == event.edge);
        assert(event.committed_pending == (event.edge < 3));
    }
    const auto &prefetch = h.dut_.fires[0].bits;
    assert(prefetch[0] == 0x1800 + 42 * 4 && prefetch[5] == 0 && prefetch[6] == 0 && prefetch[8] == 0);
    assert(prefetch[7] == 2047 && prefetch[9] == 1 && prefetch[10] == 42);
    assert(prefetch[11] == 1 && prefetch[12] == 11 && prefetch[13] == 1 && prefetch[14] == 43);
    assert(prefetch[20] + 2047 == p.address);
    assert(h.callbacks.size() == 2 && h.feedback_.std_pulses == 1);
    assert(h.callbacks[0].edge == 1 && !h.callbacks[0].address && h.callbacks[1].edge == 3 && h.callbacks[1].address);
    assert(h.issue_session_empty() && h.issue_session_available(PublicKind::load, 0));
    finish_store(h, s);
    h.tick(); assert(h.dut_.fires.size() == 6); // Empty session still owns every pin.
    assert(std::none_of(h.dut_.valid.begin(), h.dut_.valid.end(), [](bool b) { return b; }));
    assert(h.end_issue_session() && !h.retained_issue_driver_ && !h.ordinary_issue_driver_);
    std::cout << "independent_acceptances=6 delayed_STA_edge=3 feedback_cursor=3 same_edge_STD=1 value_lifetimes=3\n";
}

void store_head_and_failures() {
    const auto s = store();
    for (bool head : {false, true}) {
        Harness h; h.expect_store(s, head); assert(h.begin_issue_session());
        assert(h.offer_store_address(1, s) && h.offer_store_data(2, s));
        h.dut_.ready[4] = h.dut_.ready[6] = true;
        h.dut_.on_edge = [&] {
            const auto &in = h.release_inputs.at(Harness::scalar_store_key(s));
            assert(in.registered && in.authorized == head && in.registrations == 1);
        };
        h.tick(); const auto events = h.take_issue_events();
        assert(events.size() == 2 && h.callbacks.size() == 2 && h.feedback_.std_pulses == 1);
        finish_store(h, s);
        if (!head) {
            h.dut_.on_edge = {};
            h.release_inputs.at(Harness::scalar_store_key(s)).head = true;
            h.tick();
        }
        const auto &in = h.release_inputs.at(Harness::scalar_store_key(s));
        assert(in.authorized && in.authorizations == 1 && h.end_issue_session());
    }
    for (bool reject_input : {false, true}) {
        Harness h; h.expect_store(s); h.reject_input = reject_input; h.reject_authorization = !reject_input;
        assert(h.begin_issue_session() && h.offer_store_address(1, s) && h.offer_store_data(2, s));
        h.dut_.ready[4] = h.dut_.ready[6] = true;
        h.tick();
        assert(h.cycle() == 0 && h.dut_.fires.empty() && !h.check_components());
    }
    std::cout << "same_edge_pair_cases=2 delayed_head_authorization=1 callback_rejections=2\n";
}

void wrapped_redirects() {
    unsigned cases = 0;
    // Age ordinals are chosen independently of the encoded ring comparison:
    // anchor 159 kills 161; anchor 321 preserves 319, across both wrap phases.
    for (bool killed : {false, true}) for (bool accepts_tail : {false, true}) {
        Harness h; const auto s = store(killed ? 161 : 319);
        h.expect_store(s); assert(h.begin_issue_session());
        assert(h.offer_store_address(21, s) && h.offer_store_data(22, s));
        h.dut_.queues.redirect.valid.data = 1;
        h.dut_.queues.redirect.rob_idx_value.data = killed ? 159 : 1;
        h.dut_.queues.redirect.rob_idx_flag.data = 0;
        h.tick(); assert(h.take_issue_events().empty() && h.callbacks.empty());
        h.dut_.queues.redirect.valid.data = 0;
        h.dut_.ready[4] = h.dut_.ready[6] = accepts_tail;
        h.tick();
        if (!killed && !accepts_tail) {
            assert(h.take_issue_events().empty() && h.callbacks.empty());
            h.dut_.ready[4] = h.dut_.ready[6] = true; h.tick();
        }
        const auto events = h.take_issue_events();
        assert(events.size() == 2);
        const auto edge = !killed && !accepts_tail ? 2U : 1U;
        const auto outcome = killed && !accepts_tail ? PublicOutcome::canceled : PublicOutcome::accepted;
        event_is(events[0], 21, edge, PublicKind::store_address, 1, outcome, killed);
        event_is(events[1], 22, edge, PublicKind::store_data, 1, outcome, killed);
        const bool accepted = outcome == PublicOutcome::accepted;
        assert(h.callbacks.size() == (accepted ? 2U : 0U) && h.dut_.fires.size() == h.callbacks.size());
        assert(h.feedback_.std_pulses == accepted);
        if (accepted) {
            assert(events[0].feedback_cursor == edge && events[1].feedback_cursor == edge);
            assert(h.release_inputs.at(Harness::scalar_store_key(s)).authorized);
            finish_store(h, s);
        } else {
            assert(events[0].feedback_cursor == 0 && events[1].feedback_cursor == 0);
            assert(h.store_scoreboard_.cancel_pending_for_redirect(159, false, false) == 1);
        }
        assert(h.take_issue_events().empty() && h.end_issue_session()); ++cases;
    }
    std::cout << "wrapped_redirect_cases=" << cases << " killed_accepted_callbacks=2 canceled_callbacks=0 survivor_acceptances=4\n";
}

void retries_and_replay() {
    Harness h; auto v = vector(); assert(h.begin_issue_session() && h.offer_vector(71, v));
    const auto younger = load(1);
    assert(h.offer_load(72, younger)); h.dut_.ready[1] = true;
    for (unsigned edge = 0; edge < 14; ++edge) {
        h.tick(); assert(h.cycle() == edge + 1);
        const auto events = h.take_issue_events();
        if (edge == 0) { assert(events.size() == 1); event_is(events[0], 72, 0, PublicKind::load, 1, PublicOutcome::accepted); }
        else if (edge == 13) {
            assert(events.size() == 1); event_is(events[0], 71, 13, PublicKind::vector, 0, PublicOutcome::retry);
            assert(events[0].feedback_cursor == 0 && !events[0].committed_pending);
        } else assert(events.empty());
        if (edge < 13) { assert(h.dut_.valid[7]); check_vector_bits(h.dut_.payload[7], v); }
    }
    assert(h.issue_session_empty() && h.dut_.fires.size() == 1);
    assert(h.offer_vector(73, v)); h.dut_.ready[7] = true; h.tick();
    event_is(only_event(h), 73, 14, PublicKind::vector, 0, PublicOutcome::accepted);
    assert(h.end_issue_session());

    // Production replay matching crosses SQ wrap and excludes unrelated SQ
    // identities, load requests and already completed vector uops.
    Harness r; v.store = true; v.flow_num = 4;
    r.feedback_.vector_replay_requests_.push_back({0, false, 5, false, 3, false, 0x1111, 2});
    r.feedback_.vector_replay_requests_.push_back({1, false, 6, false, 0, true, 0xa5c3, 17});
    VectorMemoryTransaction out; out.address = 0x1234; bool found = true;
    assert(r.poll_vector_replay(vector(), out, found) && !found && out.address == 0x1234);
    const auto before = r.feedback_.vector_replay_requests_.size();
    assert(r.poll_vector_replay(v, out, found) && found);
    assert(r.cycle() == 0 && r.dut_.fires.empty() && r.callbacks.empty());
    assert(r.feedback_.vector_replay_requests_.size() == before - 1 && r.feedback_.vector_replay_requests_.front().sq_value == 3);
    assert(out.lane == 1 && out.is_part_replay && out.replay_mask == 0xa5c3 && out.replay_mb_index == 17);
    assert(out.rob == v.rob && out.rob_flag == v.rob_flag && out.sq == 54 && out.sq_flag && out.flow_num == 4);
    assert(out.address == v.address && out.data == v.data && out.index == v.index && out.mask_high == v.mask_high);
    r.expect_vector_store(out); assert(r.begin_issue_session() && r.offer_vector(74, out));
    r.dut_.ready[8] = true; r.tick();
    event_is(only_event(r), 74, 0, PublicKind::vector, 1, PublicOutcome::accepted);
    check_vector_bits(r.dut_.fires.front().bits, out);
    assert(r.callbacks.size() == 1 && r.callbacks[0].vector && r.callbacks[0].address);
    assert(r.release_inputs.at(Harness::vector_store_key(out)).authorized && r.end_issue_session());
    found = true; assert(r.poll_vector_replay(v, out, found) && !found && r.cycle() == 1);
    Harness completed; completed.vector_scoreboard_.active = false;
    completed.feedback_.vector_replay_requests_.push_back({0, false, 0, false, 0, true, 3, 4});
    found = true; assert(!completed.poll_vector_replay(v, out, found) && !found && !completed.error_.empty());
    std::cout << "vector_stall_edges=14 retry_edge=13 independent_load_edge=0 replay_metadata=exact SQ_wrap_identity=1 completed_replay_rejected=1\n";
}

void misuse() {
    unsigned rejected = 0;
    const auto l = load(); const auto s = store(); const auto v = vector(); PrefetchTransaction p;
    auto reject = [&](const std::function<bool(Harness &)> &action) {
        Harness h; assert(!action(h) && h.cycle() == 0 && !h.error_.empty()); ++rejected;
    };
    reject([&](Harness &h) { return h.offer_load(1, l); });
    reject([&](Harness &h) { return h.offer_prefetch(1, p); });
    reject([&](Harness &h) { return h.offer_store_address(1, s); });
    reject([&](Harness &h) { return h.offer_store_data(1, s); });
    reject([&](Harness &h) { return h.offer_vector(1, v); });
    reject([&](Harness &h) { return h.end_issue_session(); });
    {
        Harness h; assert(h.take_issue_events().empty() && !h.error_.empty()); ++rejected;
    }
    for (bool external : {false, true}) {
        Harness h; Driver owner; h.dut_.valid.fill(true);
        if (external) h.retained_issue_driver_ = &owner;
        else assert(h.begin_issue_session());
        const auto valid = h.dut_.valid;
        auto *const pointer = h.retained_issue_driver_;
        assert(!h.begin_issue_session() && h.retained_issue_driver_ == pointer && h.dut_.valid == valid && h.cycle() == 0);
        ++rejected;
    }
    for (unsigned invalid = 0; invalid < 12; ++invalid) {
        Harness h; assert(h.begin_issue_session()); bool result = true;
        auto bad_l = l; auto bad_p = p; auto bad_s = s; auto bad_v = v;
        switch (invalid) {
        case 0: bad_l.lane = 3; result = h.offer_load(1, bad_l); break;
        case 1: bad_l.rob = 160; result = h.offer_load(1, bad_l); break;
        case 2: bad_l.immediate = -2049; result = h.offer_load(1, bad_l); break;
        case 3: bad_p.lane = 3; result = h.offer_prefetch(1, bad_p); break;
        case 4: bad_p.rob = 160; result = h.offer_prefetch(1, bad_p); break;
        case 5: bad_p.immediate = 2048; result = h.offer_prefetch(1, bad_p); break;
        case 6: bad_s.address_lane = 2; result = h.offer_store_address(1, bad_s); break;
        case 7: bad_s.immediate = 2048; result = h.offer_store_address(1, bad_s); break;
        case 8: bad_s.data_lane = 2; result = h.offer_store_data(1, bad_s); break;
        case 9: bad_s.rob = 160; result = h.offer_store_data(1, bad_s); break;
        case 10: bad_v.lane = 2; result = h.offer_vector(1, bad_v); break;
        case 11: bad_v.rob = 160; result = h.offer_vector(1, bad_v); break;
        }
        assert(!result && !h.error_.empty() && h.cycle() == 0 && h.issue_session_empty()); ++rejected;
    }
    for (unsigned which = 0; which < 4; ++which) {
        Harness h; assert(h.begin_issue_session() && h.offer_load(100, l));
        if (which == 0) assert(!h.end_issue_session());
        if (which == 1) { auto other = l; other.lane = 1; assert(!h.offer_load(100, other)); }
        if (which == 2) assert(!h.offer_load(101, l));
        if (which == 3) {
            h.dut_.ready[0] = true; h.tick();
            assert(h.issue_session_empty() && !h.end_issue_session() && h.retained_issue_driver_);
        }
        assert(!h.error_.empty() && h.retained_issue_driver_ && h.ordinary_issue_driver_); ++rejected;
    }
    {
        Harness h; assert(h.begin_issue_session()); h.dut_.valid.fill(true); h.dut_.ready.fill(true);
        h.tick(); assert(h.dut_.fires.empty() && h.issue_session_empty());
        assert(h.take_issue_events().empty() && h.end_issue_session());
        assert(h.begin_issue_session() && h.end_issue_session());
    }
    std::cout << "misuse_rejections=" << rejected << " empty_owner_clears_all_pins=1 reusable_clean_session=1\n";
}
int main(int argc, char **argv) {
    assert(argc == 2); const std::string mode = argv[1];
    if (mode == "progress") progress_and_value_capture();
    else if (mode == "privilege") acceptance_privilege_capture();
    else if (mode == "stores") store_head_and_failures();
    else if (mode == "redirect") wrapped_redirects();
    else if (mode == "replay") retries_and_replay();
    else if (mode == "misuse") misuse();
    else assert(false);
}
