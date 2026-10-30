#include "environment/retained_issue.hpp"
#include <cassert>
#include <map>
#include <random>
#include <tuple>

using namespace memblock;
using Driver = RetainedIssueDriver;
using Kind = Driver::Kind;
using Outcome = Driver::Outcome;

// Independent public-port observations. Every payload field participates in the
// witness; padding and implementation state never enter it.
using Bytes = std::vector<std::uint64_t>;
Bytes witness(const generated::ScalarLoadIssue &s) {
    return {s.pc, s.predecode_rvc, s.ftq_ptr, s.ftq_offset, s.fu_op_type,
        s.rf_wen, s.fp_wen, s.imm, s.pdest, s.rob_flag, s.rob_value,
        s.lq_flag, s.lq_value, s.sq_flag, s.sq_value, s.store_set_hit,
        s.wait_for_rob_flag, s.wait_for_rob_value, s.load_wait_bit, s.load_wait_strict, s.src};
}
Bytes witness(const generated::ScalarStoreIssue &s) {
    return {s.fu_type, s.fu_op_type, s.imm, s.pdest, s.rf_wen,
        s.rob_flag, s.rob_value, s.sq_flag, s.sq_value, s.src};
}
Bytes witness(const generated::VectorMemoryIssue &s) {
    Bytes result{s.ftq_ptr, s.ftq_offset, s.fu_type, s.fu_op_type, s.vec_wen,
        s.v0_wen, s.vl_wen, s.vma, s.vta, s.vsew, s.vlmul, s.vm, s.vstart,
        s.vuop_idx, s.last_uop, s.nf, s.veew, s.is_vleff, s.pdest,
        s.rob_flag, s.rob_value, s.lq_flag, s.lq_value, s.sq_flag, s.sq_value,
        s.flow_num, s.is_part_replay, s.replay_mask, s.replay_mb_index};
    for (auto byte : s.vmask) result.push_back(byte);
    for (const auto &source : s.src) for (auto byte : source) result.push_back(byte);
    return result;
}
constexpr std::array<Kind, 9> kinds{Kind::load, Kind::load, Kind::load,
    Kind::store_address, Kind::store_address, Kind::store_data, Kind::store_data, Kind::vector, Kind::vector};
constexpr std::array<unsigned, 9> lanes{0, 1, 2, 0, 1, 0, 1, 0, 1};
struct Port {
    std::uint64_t data = 0;
    bool boolean() const { return data != 0; }
    std::uint64_t value() const { return data; }
};
struct Adapter {
    std::array<bool, 9> valid{}, ready{};
    std::array<Bytes, 9> payload{};
    std::array<unsigned, 9> accepted{};
    std::uint64_t edge = 0;
    struct { struct { Port valid, rob_idx_value, rob_idx_flag, level; } redirect; } queues;
    void clear_scalar_load_issue_valids() { for (unsigned i : {0,1,2}) valid[i] = false; }
    void clear_scalar_store_issue_valids() { for (unsigned i : {3,4,5,6}) valid[i] = false; }
    void clear_vector_memory_issue_valids() { for (unsigned i : {7,8}) valid[i] = false; }
    void drive_scalar_load_issue(unsigned lane, const generated::ScalarLoadIssue &s) { valid.at(lane) = true; payload.at(lane) = witness(s); }
    void drive_scalar_store_address(unsigned lane, const generated::ScalarStoreIssue &s) { valid.at(3+lane) = true; payload.at(3+lane) = witness(s); }
    void drive_scalar_store_data(unsigned lane, const generated::ScalarStoreIssue &s) { valid.at(5+lane) = true; payload.at(5+lane) = witness(s); }
    void drive_vector_memory_issue(unsigned lane, const generated::VectorMemoryIssue &s) { valid.at(7+lane) = true; payload.at(7+lane) = witness(s); }
    bool scalar_load_issue_ready(unsigned lane) const { return ready.at(lane); }
    bool scalar_store_address_ready(unsigned lane) const { return ready.at(3+lane); }
    bool scalar_store_data_ready(unsigned lane) const { return ready.at(5+lane); }
    bool vector_memory_issue_ready(unsigned lane) const { return ready.at(7+lane); }
    void refresh_comb() {}
    void step() {
        for (unsigned i = 0; i < 9; ++i) accepted[i] += valid[i] && ready[i];
        ++edge;
    }
};
struct Obligation { std::uint64_t tag; Bytes payload; unsigned stalled = 0; };
Obligation offer(Driver &driver, unsigned slot, std::uint64_t tag) {
    const auto rob = tag % 160U;
    const bool flag = (tag / 160U) & 1U;
    if (slot < 3) {
        generated::ScalarLoadIssue s;
        s.pc = 0x1000 + tag * 4; s.predecode_rvc = tag & 1;
        s.ftq_ptr = tag % 64; s.ftq_offset = tag % 8;
        s.fu_op_type = tag % 7; s.rf_wen = !(tag & 1); s.fp_wen = !s.rf_wen;
        s.imm = tag & 0xfff; s.pdest = 1 + tag % 100;
        s.rob_value = rob; s.rob_flag = flag; s.lq_value = tag % 72; s.lq_flag = tag & 2;
        s.sq_value = tag % 56; s.sq_flag = tag & 4; s.store_set_hit = tag & 8;
        s.wait_for_rob_flag = tag & 16; s.wait_for_rob_value = (rob + 159) % 160;
        s.load_wait_bit = tag & 32; s.load_wait_strict = tag & 64; s.src = 0x80000000 + tag * 8;
        driver.offer_load(tag, lanes[slot], s);
        auto result = Obligation{tag, witness(s)};
        s.src = 0; // Caller mutation cannot change a retained offer.
        return result;
    }
    if (slot < 7) {
        generated::ScalarStoreIssue s;
        s.fu_type = kFuTypeStore; s.fu_op_type = tag % 4; s.imm = tag & 0xfff;
        s.pdest = 1 + tag % 100; s.rf_wen = tag & 1;
        s.rob_value = rob; s.rob_flag = flag; s.sq_value = tag % 56; s.sq_flag = tag & 4;
        s.src = slot < 5 ? 0x80001000 + tag * 8 : tag * 0x123456789ULL;
        if (slot < 5) driver.offer_store_address(tag, lanes[slot], s);
        else driver.offer_store_data(tag, lanes[slot], s);
        return {tag, witness(s)};
    }
    generated::VectorMemoryIssue s;
    s.ftq_ptr = tag % 64; s.ftq_offset = tag % 8;
    s.fu_type = kFuTypeVectorLoad; s.fu_op_type = kVectorLoadUnitStride;
    s.vec_wen = true; s.vma = tag & 1; s.vta = tag & 2; s.vsew = 3; s.veew = 3;
    s.vm = true; s.pdest = 1 + tag % 100; s.rob_value = rob; s.rob_flag = flag;
    s.lq_value = tag % 72; s.lq_flag = tag & 2; s.sq_value = tag % 56; s.sq_flag = tag & 4;
    s.vmask.fill(255);
    for (unsigned source = 0; source < 5; ++source)
        for (unsigned byte = 0; byte < 16; ++byte) s.src[source][byte] = tag + source * 17 + byte;
    s.src[4].fill(0); s.src[4][0] = 2;
    driver.offer_vector(tag, lanes[slot], s);
    return {tag, witness(s)};
}
void edge(Driver &driver, Adapter &dut, Driver::Redirect redirect = {}) {
    driver.drive(dut);
    driver.capture_before_edge(dut, dut.edge, redirect);
    dut.step();
    driver.advance_after_edge(dut);
}

void random_histories() {
    unsigned offers = 0, fires = 0, retries = 0, mixed_edges = 0;
    for (unsigned seed = 1; seed <= 128; ++seed) {
        std::mt19937 rng(seed);
        Driver driver; Adapter dut;
        std::array<std::optional<Obligation>, 9> live{};
        std::uint64_t tag = 1;
        for (unsigned cycle = 0; cycle < 192; ++cycle) {
            for (unsigned i = 0; i < 9; ++i) {
                assert(driver.available(kinds[i], lanes[i]) == !live[i]);
                if (!live[i] && cycle < 160 && rng() % 4 == 0) { live[i] = offer(driver, i, tag++); ++offers; }
                // Long stalls deliberately reach the vector holding timeout.
                dut.ready[i] = cycle >= 160 || rng() % 23 == 0;
            }
            driver.drive(dut);
            std::map<std::uint64_t, std::pair<unsigned, Outcome>> expected;
            unsigned on_edge = 0;
            for (unsigned i = 0; i < 9; ++i) {
                assert(dut.valid[i] == bool(live[i]));
                if (!live[i]) continue;
                assert(dut.payload[i] == live[i]->payload);
                if (dut.ready[i]) { expected[live[i]->tag] = {i, Outcome::accepted}; ++on_edge; }
                else if (i >= 7 && ++live[i]->stalled == 14) expected[live[i]->tag] = {i, Outcome::retry};
            }
            mixed_edges += on_edge >= 2;
            driver.capture_before_edge(dut, dut.edge, {});
            dut.step();
            driver.advance_after_edge(dut);
            const auto events = driver.take_events();
            assert(events.size() == expected.size());
            for (const auto &event : events) {
                const auto found = expected.find(event.tag);
                assert(found != expected.end());
                const auto [i, outcome] = found->second;
                assert(event.kind == kinds[i] && event.lane == lanes[i] && event.edge == cycle);
                assert(event.outcome == outcome && !event.killed);
                fires += outcome == Outcome::accepted; retries += outcome == Outcome::retry;
                live[i].reset(); expected.erase(found);
            }
            assert(expected.empty() && driver.take_events().empty());
            for (unsigned i = 0; i < 9; ++i) assert(dut.valid[i] == bool(live[i]));
        }
        assert(driver.empty());
    }
    assert(offers == fires + retries && retries > 100 && mixed_edges > 100);
    std::cout << "offers=" << offers << " accepted=" << fires << " retries=" << retries << " mixed_edges=" << mixed_edges << '\n';
}

void redirect_histories() {
    unsigned cases = 0;
    for (int anchor = 0; anchor < 320; ++anchor)
        for (int distance = -19; distance <= 19; ++distance)
            for (bool self : {false, true}) for (bool ready_in_tail : {false, true}) {
                Driver driver; Adapter dut;
                const unsigned absolute = (anchor + distance + 320) % 320;
                generated::ScalarLoadIssue s;
                s.rob_value = absolute % 160; s.rob_flag = absolute >= 160;
                driver.offer_load(17, 0, s);
                edge(driver, dut, {true, std::uint8_t(anchor % 160), anchor >= 160, self});
                assert(dut.valid[0] && driver.take_events().empty());
                const bool killed = distance > 0 || (distance == 0 && self);
                dut.ready[0] = ready_in_tail;
                edge(driver, dut);
                auto events = driver.take_events();
                if (killed || ready_in_tail) {
                    assert(events.size() == 1 && events[0].edge == 1 && events[0].killed == killed);
                    assert(events[0].outcome == (ready_in_tail ? Outcome::accepted : Outcome::canceled));
                    assert(driver.empty() && !dut.valid[0]);
                } else {
                    assert(events.empty() && dut.valid[0]);
                    dut.ready[0] = true; edge(driver, dut);
                    events = driver.take_events();
                    assert(events.size() == 1 && events[0].edge == 2 && !events[0].killed);
                    assert(events[0].outcome == Outcome::accepted && driver.empty());
                }
                ++cases;
            }
    // Two consecutive public redirects must each reach the delayed producer.
    Driver driver; Adapter dut; generated::ScalarLoadIssue s; s.rob_value = 11;
    driver.offer_load(1, 0, s);
    edge(driver, dut, {true, 12, false, false});
    edge(driver, dut, {true, 10, false, false});
    assert(driver.take_events().empty() && dut.valid[0]);
    edge(driver, dut);
    auto events = driver.take_events();
    assert(events.size() == 1 && events[0].outcome == Outcome::canceled && events[0].edge == 2);
    std::cout << "redirect_cases=" << cases + 1 << '\n';
}

void timeout_boundaries() {
    for (int ready_at : {-1, 12, 13}) for (bool redirect_on_12 : {false, true}) {
        Driver driver; Adapter dut; offer(driver, 7, 1);
        for (unsigned cycle = 0; cycle < 14; ++cycle) {
            dut.ready[7] = int(cycle) == ready_at;
            edge(driver, dut, {redirect_on_12 && cycle == 12, 0, false, false});
            auto events = driver.take_events();
            if (int(cycle) == ready_at || cycle == 13) {
                assert(events.size() == 1 && events[0].edge == cycle);
                const bool killed = redirect_on_12 && cycle == 13;
                assert(events[0].killed == killed);
                const auto expected = int(cycle) == ready_at ? Outcome::accepted : killed ? Outcome::canceled : Outcome::retry;
                assert(events[0].outcome == expected && driver.empty() && !dut.valid[7]);
                break;
            }
            assert(events.empty() && dut.valid[7]);
        }
    }
    Driver driver; Adapter dut; const auto original = offer(driver, 0, 9);
    for (unsigned cycle = 0; cycle < 41; ++cycle) {
        dut.ready[0] = cycle == 40;
        edge(driver, dut);
        assert(dut.payload[0] == original.payload);
        auto events = driver.take_events();
        if (cycle < 40) assert(events.empty() && dut.valid[0]);
        else assert(events.size() == 1 && events[0].outcome == Outcome::accepted && driver.empty());
    }
    std::cout << "timeout_boundary_cases=7\n";
}

unsigned misuse_rejections = 0;
template<class F> void rejects(F action) {
    bool rejected = false;
    try { action(); } catch (const std::logic_error &) { rejected = true; }
    assert(rejected);
    ++misuse_rejections;
}
void misuse() {
    Driver driver; Adapter dut; generated::ScalarLoadIssue s;
    rejects([&] { driver.offer_load(1, 3, s); });
    rejects([&] { driver.available(Kind(9), 0); });
    s.rob_value = 160; rejects([&] { driver.offer_load(1, 0, s); }); s.rob_value = 1;
    driver.offer_load(1, 0, s);
    rejects([&] { driver.offer_load(2, 0, s); });
    rejects([&] { driver.offer_load(1, 1, s); });
    driver.drive(dut); dut.ready[0] = true;
    rejects([&] { driver.advance_after_edge(dut); });
    driver.capture_before_edge(dut, 0, {});
    rejects([&] { driver.capture_before_edge(dut, 0, {}); });
    rejects([&] { driver.take_events(); });
    rejects([&] { driver.drive(dut); });
    rejects([&] { driver.offer_load(2, 1, s); });
    dut.step(); driver.advance_after_edge(dut);
    rejects([&] { driver.offer_load(1, 0, s); }); // Undelivered event still owns the tag.
    assert(driver.take_events().size() == 1);
    driver.offer_load(1, 0, s);
    rejects([&] { driver.capture_before_edge(dut, 2, {}); });
    rejects([&] { driver.capture_before_edge(dut, 1, {true, 160, false, false}); });
    edge(driver, dut);
    assert(driver.take_events().size() == 1 && driver.empty());
    std::cout << "misuse_rejections=" << misuse_rejections << '\n';
}

// PRODUCTION_TICK_FIXTURE

int main(int argc, char **argv) {
    assert(argc == 2);
    const std::string mode = argv[1];
    if (mode == "random") random_histories();
    else if (mode == "redirect") redirect_histories();
    else if (mode == "timeout") timeout_boundaries();
    else if (mode == "misuse") misuse();
    // PRODUCTION_TICK_MAIN
    else assert(false);
}
