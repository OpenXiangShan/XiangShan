#pragma once

#include "unresolved_graph.hpp"
#include <numeric>

namespace memblock::scenarios::mixed {

inline bool unresolved_overlap(const DependencyStore &store, const DependencyLoad &load)
{
    return store.offset < load.offset + load.width() && load.offset < store.offset + store.width;
}

struct UnresolvedLoadGroup {
    std::vector<DependencyLoad> loads;
    unsigned primary = 0;
    // loads.size() denotes an all-disjoint control.
    unsigned victim = 0;
    std::vector<unsigned> issue_order;
    std::vector<unsigned> issue_delays;
    std::vector<unsigned> refetch_order;
};

inline std::vector<unsigned> unresolved_random_order(std::mt19937_64 &random,
    unsigned begin, unsigned end)
{
    std::vector<unsigned> ready(end - begin), result;
    std::iota(ready.begin(), ready.end(), begin);
    while (!ready.empty()) {
        const unsigned choice = random() % ready.size();
        result.push_back(ready[choice]);
        ready.erase(ready.begin() + choice);
    }
    return result;
}

// This bounded direction has one recovery boundary. Older surviving loads are
// disjoint from every store. A store disjoint from the primary load is also
// disjoint from every other load, so no earlier STA can select a younger victim.
// Later loads may conflict or be disjoint; the whole suffix must be refetched.
inline UnresolvedLoadGroup make_unresolved_load_group(std::mt19937_64 &random,
    const UnresolvedStoreGraph &graph, const ScalarLoadPolicy &policy = {},
    unsigned maximum_loads = 4)
{
    if (maximum_loads == 0 || maximum_loads > 4 || graph.stores.empty())
        throw std::invalid_argument("invalid unresolved load group constraint");
    UnresolvedLoadGroup group;
    const unsigned count = 1U + random() % maximum_loads;
    group.primary = random() % count;
    group.victim = graph.conflict ? group.primary : count;
    for (unsigned index = 0; index < count; ++index) {
        if (index == group.primary) {
            group.loads.push_back(graph.load);
            continue;
        }
        const auto format = policy.choose(random);
        DependencyLoad load{0, format.op, format.fp};
        const bool want_overlap = graph.conflict && index > group.victim && (random() & 1U);
        std::array<std::vector<unsigned>, 2> candidates;
        for (unsigned offset = 0; offset + load.width() <= 64; offset += load.width()) {
            load.offset = offset;
            bool allowed = true, overlap = false;
            for (const auto &store : graph.stores) {
                const bool hit = unresolved_overlap(store.bytes, load);
                overlap |= hit;
                if (hit && (index < group.victim || !graph.conflict ||
                            !unresolved_overlap(store.bytes, graph.load))) allowed = false;
            }
            if (allowed) candidates[overlap].push_back(offset);
        }
        auto &choices = candidates[want_overlap && !candidates[1].empty()];
        if (choices.empty()) throw std::logic_error("unreachable unresolved load geometry");
        load.offset = choices[random() % choices.size()];
        group.loads.push_back(load);
    }
    group.issue_order = unresolved_random_order(random, 0, count);
    for (unsigned index = 0; index < count; ++index) group.issue_delays.push_back(random() % 8U);
    group.refetch_order = unresolved_random_order(random, group.victim, count);
    return group;
}

// Repair every conflicting load's final bytes using only the youngest input
// writer. Repairs cannot undo another load's distinction: a byte is changed
// only when it still equals the initial byte.
inline void distinguish_unresolved_load_group(UnresolvedStoreGraph &graph,
    const UnresolvedLoadGroup &group, const std::array<unsigned char, 64> &initial)
{
    for (const auto &load : group.loads) {
        auto view = graph;
        view.load = load;
        view.conflict = std::any_of(graph.stores.begin(), graph.stores.end(),
            [&](const auto &store) { return unresolved_overlap(store.bytes, load); });
        distinguish_unresolved_graph(view, initial);
        graph.stores = std::move(view.stores);
    }
}

// All execution scoreboards complete strictly before recovery. This ledger
// keeps their architectural obligations live until the surviving prefix and
// the *entire* invalidated suffix are resolved, then committed bytes read back.
class UnresolvedLoadGroupObligation {
public:
    template<class Load>
    UnresolvedLoadGroupObligation(const std::vector<Load> &loads, unsigned victim)
        : victim_(victim), provisional_(loads.size()), refetched_(loads.size())
    {
        if (loads.empty() || victim > loads.size()) { failed_ = true; return; }
        for (const auto &load : loads) identities_.push_back({load.rob, load.rob_flag,
            load.ftq_ptr, load.ftq_offset, load.predecode_rvc});
    }
    bool provisional(unsigned index)
    {
        if (failed_ || redirected_ || resolved_ || index >= provisional_.size() || provisional_[index])
            return fail();
        provisional_[index] = true;
        return true;
    }
    template<class Redirect>
    bool redirect(const Redirect &event)
    {
        if (failed_ || redirected_ || resolved_ || victim_ == identities_.size() ||
            !std::all_of(provisional_.begin(), provisional_.end(), [](bool done) { return done; }))
            return fail();
        const auto &id = identities_[victim_];
        if (!event.valid || !event.level || event.ftq_flag || event.rob_value != id.rob ||
            event.rob_flag != id.flag || event.ftq_value != id.ftq ||
            event.ftq_offset != id.offset || event.is_rvc != id.rvc) return fail();
        redirected_ = true;
        return true;
    }
    bool returned()
    {
        if (failed_ || !redirected_ || returned_) return fail();
        returned_ = true;
        return true;
    }
    bool refetched(unsigned index)
    {
        if (failed_ || !returned_ || resolved_ || index < victim_ ||
            index >= refetched_.size() || refetched_[index]) return fail();
        refetched_[index] = true;
        return true;
    }
    bool resolved()
    {
        if (failed_ || resolved_ ||
            !std::all_of(provisional_.begin(), provisional_.end(), [](bool done) { return done; }) ||
            (victim_ < identities_.size() && (!returned_ ||
             !std::all_of(refetched_.begin() + victim_, refetched_.end(), [](bool done) { return done; }))))
            return fail();
        resolved_ = true;
        return true;
    }
    bool readback()
    {
        if (failed_ || !resolved_ || complete_) return fail();
        complete_ = true;
        return true;
    }
    bool done() const { return !failed_ && complete_; }

private:
    struct Identity { unsigned rob; bool flag; std::uint64_t ftq; unsigned offset; bool rvc; };
    bool fail() { failed_ = true; return false; }
    unsigned victim_;
    std::vector<Identity> identities_;
    std::vector<bool> provisional_, refetched_;
    bool failed_ = false, redirected_ = false, returned_ = false, resolved_ = false, complete_ = false;
};

} // namespace memblock::scenarios::mixed
