// Host contract tests for the production scoreboards. Instruction ordinals and
// pending/completed events are independent of the scoreboard's ROB age helper.
#include "memblock_env.hpp"

namespace memblock {
#include "environment/scoreboards.inc"
}

using namespace memblock;

static void require(bool condition, const std::string &message)
{
    if (!condition) throw std::runtime_error(message);
}

enum class Kind { scalar, prefetch, store, vector_load, vector_store };
using Counts = std::array<std::uint64_t, 5>;

struct Obligation {
    Kind kind;
    int ordinal;
    unsigned uop = 0;
    bool completed = false, canceled = false;
    bool address_seen = false, data_seen = false;
    unsigned issue_state = 0;
    LoadTransaction load;
    PrefetchTransaction prefetch;
    StoreTransaction store;
    VectorMemoryTransaction vector;
    std::uint64_t scalar_data = 0;
    std::array<unsigned char, 16> vector_data{};
};

static Obligation obligation(Kind kind, int ordinal, unsigned uop = 0,
                             bool fp = false)
{
    Obligation result{};
    result.kind = kind; result.ordinal = ordinal; result.uop = uop;
    const unsigned position = (ordinal % 320 + 320) % 320;
    const std::uint8_t rob = position % 160;
    const bool flag = position / 160;
    const std::uint64_t address = 0x80200000ULL + 32 * (ordinal + 1024);
    result.load.address = address;
    result.load.rob = rob; result.load.rob_flag = flag;
    result.load.pdest = 31 + uop;
    result.load.rf_wen = !fp; result.load.fp_wen = fp;
    result.load.op = fp ? LoadOp::lw : LoadOp::ld;
    result.load.expected_debug_is_mmio = false;
    result.load.expected_debug_is_ncio = false;
    result.load.expected_debug_is_perf_cnt = false;
    result.prefetch.address = address;
    result.prefetch.rob = rob; result.prefetch.rob_flag = flag;
    result.store.address = address;
    result.store.rob = rob; result.store.rob_flag = flag;
    result.store.address_lane = position % 2;
    result.store.data_lane = (position + 1) % 2;
    result.store.expected_trigger = kStoreWritebackTriggerNone;
    result.vector.address = address;
    result.vector.rob = rob; result.vector.rob_flag = flag;
    result.vector.store = kind == Kind::vector_store;
    // One LMUL=4 byte instruction decomposes into four full register uops.
    result.vector.vl = 64; result.vector.vlmul = 2;
    result.vector.vuop_idx = uop; result.vector.last_uop = uop == 3;
    result.vector.pdest = 40 + uop;
    result.vector.expected_trigger = kVectorWritebackTriggerNone;
    result.scalar_data = fp ? 0xffffffff634217a9ULL
                           : 0x683719abc4000000ULL + address;
    for (unsigned byte = 0; byte < 16; ++byte)
        result.vector_data[byte] = (ordinal * 19 + uop * 37 + byte * 7) & 255;
    return result;
}

static generated::ScalarLoadWriteback completion(const Obligation &entry)
{
    generated::ScalarLoadWriteback result{};
    result.valid = true;
    result.rob_value = entry.load.rob; result.rob_flag = entry.load.rob_flag;
    result.pdest = entry.load.pdest;
    result.rf_wen = entry.load.rf_wen; result.fp_wen = entry.load.fp_wen;
    result.data = entry.scalar_data;
    result.trigger = entry.load.expected_trigger;
    return result;
}

static generated::ScalarLoadWriteback prefetch_completion(const Obligation &entry)
{
    generated::ScalarLoadWriteback result{};
    result.valid = true;
    result.rob_value = entry.prefetch.rob;
    result.rob_flag = entry.prefetch.rob_flag;
    result.trigger = entry.prefetch.expected_trigger;
    return result;
}

static generated::ScalarStoreWriteback store_completion(const Obligation &entry)
{
    generated::ScalarStoreWriteback result{};
    result.valid = true;
    result.rob_value = entry.store.rob; result.rob_flag = entry.store.rob_flag;
    result.trigger = entry.store.expected_trigger.value();
    result.flush_pipe = entry.store.expected_output_flush_pipe.value_or(false);
    return result;
}

static generated::VectorMemoryWriteback vector_completion(const Obligation &entry)
{
    generated::VectorMemoryWriteback result{};
    result.valid = true;
    result.rob_value = entry.vector.rob; result.rob_flag = entry.vector.rob_flag;
    result.pdest = entry.vector.pdest;
    result.vuop_idx = entry.uop;
    result.fu_op_type = entry.vector.store ? kVectorStoreUnitStride
                                         : kVectorLoadUnitStride;
    result.vec_wen = !entry.vector.store;
    result.trigger = kVectorWritebackTriggerNone;
    result.vl = 64; result.vlmul = 2;
    result.vd_index_in_field = entry.uop;
    result.data.assign(entry.vector_data.begin(), entry.vector_data.end());
    result.vmask.assign(16, 0);
    result.vmask[0] = 0xff; result.vmask[1] = 0xff;
    return result;
}

struct Fixture {
    LoadScoreboard loads;
    StoreScoreboard stores;
    VectorMemoryScoreboard vectors;
    std::vector<Obligation> entries;

    void expect(const Obligation &entry)
    {
        switch (entry.kind) {
        case Kind::scalar: loads.expect(entry.load, entry.scalar_data); break;
        case Kind::prefetch: loads.expect_prefetch(entry.prefetch); break;
        case Kind::store: stores.expect(entry.store); break;
        default: vectors.expect(entry.vector, entry.vector_data); break;
        }
    }
    void ok() const
    {
        require(loads.ok(), loads.error());
        require(stores.ok(), stores.error());
        require(vectors.ok(), vectors.error());
    }
    bool pending(const Obligation &entry) const
    {
        switch (entry.kind) {
        case Kind::scalar: return loads.pending(entry.load);
        case Kind::prefetch: return loads.pending(entry.prefetch);
        case Kind::store: return stores.pending(entry.store);
        default: return vectors.pending(entry.vector);
        }
    }
    void finish(Obligation &entry)
    {
        switch (entry.kind) {
        case Kind::scalar: loads.observe(0, completion(entry)); break;
        case Kind::prefetch: loads.observe(1, prefetch_completion(entry)); break;
        case Kind::store:
            if (!entry.address_seen) {
                require(stores.mark_address_issued(entry.store, 10), stores.error());
                stores.observe_address(entry.store.address_lane,
                                       store_completion(entry), 10);
                entry.address_seen = true;
            }
            if (!entry.data_seen) {
                require(stores.mark_data_issued(entry.store, entry.store.data_lane, 10),
                        stores.error());
                stores.observe_data(entry.store.data_lane, store_completion(entry), 10);
                entry.data_seen = true;
            }
            break;
        default: vectors.observe(0, vector_completion(entry)); break;
        }
        ok();
        entry.completed = true;
    }
    Counts observed() const
    {
        return {loads.observed() - loads.prefetch_observed(),
                loads.prefetch_observed(), stores.observed(),
                vectors.load_observed(), vectors.store_observed()};
    }
    Counts cancel(unsigned anchor, bool flag, bool self)
    {
        auto scalar = loads.cancel_pending_for_redirect(anchor, flag, self);
        const auto store = stores.cancel_pending_for_redirect(anchor, flag, self);
        const auto vector = vectors.cancel_pending_for_redirect(anchor, flag, self);
        return {scalar[0], scalar[1], store, vector[0], vector[1]};
    }
    void check_pending() const
    {
        Counts pending_counts{};
        for (const auto &entry : entries) {
            const bool should_remain = !entry.completed && !entry.canceled;
            require(pending(entry) == should_remain,
                    should_remain ? "redirect dropped a surviving obligation"
                                  : "redirect retained a canceled/completed obligation");
            pending_counts[static_cast<unsigned>(entry.kind)] += should_remain;
        }
        require(loads.pending_load() == pending_counts[0] &&
                loads.pending_prefetch() == pending_counts[1] &&
                stores.pending() == pending_counts[2] &&
                vectors.pending_loads() == pending_counts[3] &&
                vectors.pending_stores() == pending_counts[4],
                "pending class bookkeeping does not match independent obligations");
    }
};

static Fixture mixed_fixture(int anchor)
{
    Fixture f;
    auto add = [&](Kind kind, int delta, unsigned uop = 0, bool fp = false) -> Obligation & {
        f.entries.push_back(obligation(kind, anchor + delta, uop, fp));
        return f.entries.back();
    };
    add(Kind::scalar, -12); add(Kind::scalar, -11, 0, true);
    add(Kind::prefetch, -10); add(Kind::store, -9);
    for (unsigned uop = 0; uop < 4; ++uop) {
        add(Kind::vector_load, -8, uop);
        add(Kind::vector_store, -7, uop);
        add(Kind::vector_load, 0, uop).completed = uop % 2 == 0;
        add(Kind::vector_store, 7, uop).completed = uop == 1;
    }
    add(Kind::store, -6).completed = true;
    add(Kind::scalar, -5, 0, true).completed = true;
    add(Kind::prefetch, -4).completed = true;
    add(Kind::scalar, 1); add(Kind::scalar, 2, 0, true);
    add(Kind::prefetch, 3);
    add(Kind::store, 4).issue_state = 1;
    add(Kind::store, 5).issue_state = 2;
    add(Kind::store, 6); // Registered before any issue, like an unissued suffix.
    add(Kind::scalar, 9).completed = true;
    add(Kind::prefetch, 10).completed = true;
    add(Kind::store, 11).completed = true;
    add(Kind::vector_load, 12).completed = true;
    for (auto &entry : f.entries) {
        f.expect(entry);
        if (entry.completed) {
            f.finish(entry);
        } else if (entry.issue_state == 1) {
            require(f.stores.mark_address_issued(entry.store, 3), f.stores.error());
            f.stores.observe_address(entry.store.address_lane, store_completion(entry), 3);
            entry.address_seen = true;
        } else if (entry.issue_state == 2) {
            require(f.stores.mark_data_issued(entry.store, entry.store.data_lane, 3),
                    f.stores.error());
            f.stores.observe_data(entry.store.data_lane, store_completion(entry), 3);
            entry.data_seen = true;
        }
    }
    f.ok(); f.check_pending();
    return f;
}

static void mixed()
{
    unsigned cases = 0, canceled_total = 0;
    for (int anchor : {0, 1, 7, 158, 159, 160, 161, 317, 318, 319}) {
        for (bool self : {false, true}) {
            auto f = mixed_fixture(anchor);
            Counts expected{};
            for (auto &entry : f.entries) {
                // This oracle uses unwrapped instruction ordinals. It never
                // calls the production age predicate or examines its map.
                if (!entry.completed &&
                    (entry.ordinal > anchor || (self && entry.ordinal == anchor))) {
                    entry.canceled = true;
                    ++expected[static_cast<unsigned>(entry.kind)];
                }
            }
            const auto before = f.observed();
            const auto actual = f.cancel(anchor % 160, anchor / 160, self);
            require(actual == expected, "bulk cancellation counts disagree with instruction suffix");
            f.ok(); f.check_pending();
            require(f.observed() == before &&
                    f.loads.redirect_canceled() == expected[0] &&
                    f.loads.canceled_observed() == 0,
                    "bulk cancellation changed completion accounting");
            require(f.cancel(anchor % 160, anchor / 160, self) == Counts{},
                    "bulk cancellation counted an execution twice");
            f.check_pending();
            for (auto &entry : f.entries) {
                if (entry.completed) continue;
                if (entry.canceled) {
                    // Same architectural identity, new independent generation.
                    entry.scalar_data ^= 0x4152;
                    for (auto &byte : entry.vector_data) byte ^= 0x79;
                    entry.address_seen = false; entry.data_seen = false;
                    f.expect(entry);
                    entry.canceled = false;
                }
                f.finish(entry);
            }
            f.ok(); f.check_pending();
            require(f.loads.done() && f.stores.done() && f.vectors.done(),
                    "survivor/replayed generation did not drain");
            for (auto count : actual) canceled_total += count;
            ++cases;
        }
    }
    std::cout << "mixed_cases=" << cases << " canceled_executions=" << canceled_total << '\n';
}

static void ages()
{
    unsigned cases = 0;
    for (int anchor = 0; anchor < 320; ++anchor)
        for (int delta : {-159, -9, -1, 0, 1, 9, 159})
            for (bool self : {false, true})
                for (unsigned kind = 0; kind < 5; ++kind) {
                    Fixture f;
                    auto entry = obligation(static_cast<Kind>(kind), anchor + delta,
                                            kind >= 3 ? 3 : 0, (anchor & 1) != 0);
                    f.expect(entry);
                    const bool killed = delta > 0 || (self && delta == 0);
                    Counts expected{}; expected[kind] = killed;
                    require(f.cancel(anchor % 160, anchor / 160, self) == expected,
                            "bounded ROB wrap/redirect-level comparison is wrong");
                    require(f.pending(entry) != killed,
                            "bounded ROB comparison changed the wrong obligation");
                    f.ok(); ++cases;
                }
    std::cout << "age_class_cases=" << cases << '\n';
}

static void reject()
{
    unsigned cases = 0;
    for (unsigned kind = 0; kind < 5; ++kind) {
        for (unsigned bad_anchor : {160U, 161U, 65536U, ~0U}) {
            Fixture f;
            auto entry = obligation(static_cast<Kind>(kind), 8);
            f.expect(entry);
            require(f.cancel(bad_anchor, false, true) == Counts{},
                    "invalid ROB anchor returned cancellations");
            require(f.pending(entry), "invalid anchor partially canceled pending work");
            require(!f.loads.ok() && !f.stores.ok() && !f.vectors.ok(),
                    "invalid ROB anchor did not fail preflight");
            ++cases;
        }
        Fixture f;
        auto good = obligation(static_cast<Kind>(kind), 8);
        auto bad = obligation(static_cast<Kind>(kind), 9);
        bad.load.rob = bad.prefetch.rob = bad.store.rob = bad.vector.rob = 160;
        f.expect(good); f.expect(bad);
        require(f.cancel(7, false, true) == Counts{},
                "malformed pending ROB identity returned cancellations");
        require(f.pending(good) && f.pending(bad),
                "pending ROB preflight partially canceled good work");
        require(kind < 2 ? !f.loads.ok() : kind == 2 ? !f.stores.ok() : !f.vectors.ok(),
                "malformed pending ROB identity was not rejected");
        ++cases;
    }
    std::cout << "invalid_preflight_cases=" << cases << '\n';
}

static void stale_and_corrupt()
{
    unsigned cases = 0;
    for (unsigned kind = 0; kind < 5; ++kind) {
        auto old = obligation(static_cast<Kind>(kind), 160, 0, kind == 0);
        Fixture canceled;
        canceled.expect(old);
        require(canceled.cancel(159, false, false)[kind] == 1,
                "stale-output setup failed");
        switch (old.kind) {
        case Kind::scalar: canceled.loads.observe(0, completion(old)); break;
        case Kind::prefetch: canceled.loads.observe(0, prefetch_completion(old)); break;
        case Kind::store:
            canceled.stores.observe_address(old.store.address_lane, store_completion(old), 10);
            break;
        default: canceled.vectors.observe(0, vector_completion(old)); break;
        }
        require(kind < 2 ? !canceled.loads.ok() : kind == 2 ? !canceled.stores.ok()
                                                       : !canceled.vectors.ok(),
                "old writeback after bulk cancellation was accepted");
        require(canceled.cancel(159, false, true) == Counts{},
                "failed scoreboard changed after another cancellation");
        ++cases;
    }
    for (bool fp : {false, true}) {
        Fixture f;
        auto entry = obligation(Kind::scalar, 22, 0, fp);
        f.expect(entry);
        auto bad = completion(entry); bad.data ^= 1;
        f.loads.observe(1, bad);
        require(!f.loads.ok() && f.loads.pending(entry.load),
                "corrupt scalar/FP output was accepted");
        require(f.loads.cancel_pending_for_redirect(21, false, true) ==
                    std::array<std::uint64_t, 2>{} && f.loads.pending(entry.load),
                "bulk cancellation hid an earlier scalar/FP oracle failure");
        ++cases;
    }
    Fixture vector;
    auto entry = obligation(Kind::vector_load, 22, 3);
    vector.expect(entry);
    auto bad = vector_completion(entry); bad.data[13] ^= 0x20;
    vector.vectors.observe(0, bad);
    require(!vector.vectors.ok() && vector.vectors.pending(entry.vector),
            "corrupt vector output was accepted");
    require(vector.vectors.cancel_pending_for_redirect(21, false, true) ==
                std::array<std::uint64_t, 2>{} && vector.vectors.pending(entry.vector),
            "bulk cancellation hid an earlier vector oracle failure");
    Fixture store;
    auto se = obligation(Kind::store, 22);
    store.expect(se);
    require(store.stores.mark_address_issued(se.store, 3), store.stores.error());
    auto sw = store_completion(se); sw.exception_mask = kExceptionStorePageFault;
    store.stores.observe_address(se.store.address_lane, sw, 3);
    require(!store.stores.ok() && store.stores.pending(se.store),
            "corrupt scalar-store output was accepted");
    require(store.stores.cancel_pending_for_redirect(21, false, true) == 0 &&
                store.stores.pending(se.store),
            "bulk cancellation hid an earlier store oracle failure");
    std::cout << "stale_corrupt_cases=" << cases + 2 << '\n';
}

static void reuse()
{
    unsigned cases = 0;
    for (bool fp : {false, true}) {
        auto old = obligation(Kind::scalar, 160, 0, fp);
        auto fresh = old; fresh.scalar_data ^= 0x2511;
        for (bool stale : {false, true}) {
            Fixture f;
            f.expect(old);
            require(f.cancel(159, false, false)[0] == 1, "scalar reuse setup failed");
            f.expect(fresh);
            f.loads.observe(0, completion(stale ? old : fresh));
            require(stale ? !f.loads.ok() && f.loads.pending(fresh.load)
                          : f.loads.ok() && f.loads.done(),
                    stale ? "stale scalar generation data was accepted"
                          : "fresh scalar generation did not accept its independent data");
            ++cases;
        }
    }
    for (bool stale : {false, true}) {
        auto old = obligation(Kind::vector_load, 160, 3);
        auto fresh = old; fresh.vector_data[4] ^= 0x39;
        Fixture f;
        f.expect(old);
        require(f.cancel(159, false, false)[3] == 1, "vector reuse setup failed");
        f.expect(fresh);
        f.vectors.observe(0, vector_completion(stale ? old : fresh));
        require(stale ? !f.vectors.ok() && f.vectors.pending(fresh.vector)
                      : f.vectors.ok() && f.vectors.done(),
                stale ? "stale vector generation data was accepted"
                      : "fresh vector generation did not accept its independent data");
        ++cases;
    }
    std::cout << "same_identity_generation_cases=" << cases << '\n';
}

int main(int argc, char **argv)
{
    try {
        const std::string mode = argc > 1 ? argv[1] : "";
        if (mode == "mixed") mixed();
        else if (mode == "ages") ages();
        else if (mode == "reject") reject();
        else if (mode == "stale") stale_and_corrupt();
        else if (mode == "reuse") reuse();
        else return 2;
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
