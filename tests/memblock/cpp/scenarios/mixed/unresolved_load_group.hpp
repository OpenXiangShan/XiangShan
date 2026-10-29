#pragma once

#include "unresolved_graph.hpp"
#include <numeric>

namespace memblock::scenarios::mixed {

inline bool unresolved_overlap(const DependencyStore &store, const DependencyLoad &load)
{
    return store.offset < load.offset + load.width() && load.offset < store.offset + store.width;
}

struct UnresolvedRecoveryRound {
    unsigned store;
    unsigned victim;
    std::vector<unsigned> issue_order;
    std::vector<unsigned> issue_delays;
};
struct UnresolvedLoadGroup {
    std::vector<DependencyLoad> loads;
    unsigned primary = 0;
    // Earliest invalidated instruction across all rounds; size() for controls.
    unsigned victim = 0;
    std::vector<unsigned> issue_order;
    std::vector<unsigned> issue_delays;
    std::vector<UnresolvedRecoveryRound> rounds;
    std::optional<unsigned> unfinished;
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

// Program ages are independent of address-resolution order. Each successful
// conflicting STA gets its own oldest victim and fresh suffix execution; no
// geometry constraint suppresses an earlier, later or repeated victim.
inline UnresolvedLoadGroup make_unresolved_load_group(std::mt19937_64 &random,
    const UnresolvedStoreGraph &graph, const ScalarLoadPolicy &policy = {},
    unsigned maximum_loads = 4, bool inflight = false)
{
    if (maximum_loads == 0 || maximum_loads > 4 || graph.stores.empty())
        throw std::invalid_argument("invalid unresolved load group constraint");
    if (inflight && (!graph.conflict || maximum_loads < 2))
        throw std::invalid_argument("unfinished unresolved load needs a conflicting older witness");
    UnresolvedLoadGroup group;
    const unsigned warm_count = 1U + random() % (maximum_loads - unsigned(inflight));
    const unsigned count = warm_count + unsigned(inflight);
    group.primary = random() % warm_count;
    group.victim = count;
    for (unsigned index = 0; index < count; ++index) {
        if (inflight && index == warm_count) {
            const auto format = policy.choose(random);
            // A separate cache line, disjoint from every store in this graph.
            // The executor owns its independent reference image and response hold.
            group.loads.push_back({64, format.op, format.fp});
            group.unfinished = index;
            continue;
        }
        if (index == group.primary) {
            group.loads.push_back(graph.load);
            continue;
        }
        const auto format = policy.choose(random);
        DependencyLoad load{0, format.op, format.fp};
        const bool want_overlap = graph.conflict && (random() & 1U);
        std::array<std::vector<unsigned>, 2> candidates;
        for (unsigned offset = 0; offset + load.width() <= 64; offset += load.width()) {
            load.offset = offset;
            const bool overlap = std::any_of(graph.stores.begin(), graph.stores.end(),
                [&](const auto &store) { return unresolved_overlap(store.bytes, load); });
            candidates[overlap].push_back(offset);
        }
        auto &choices = candidates[want_overlap];
        if (choices.empty()) throw std::logic_error("unreachable unresolved load geometry");
        load.offset = choices[random() % choices.size()];
        group.loads.push_back(load);
    }
    group.issue_order = unresolved_random_order(random, 0, count);
    for (unsigned index = 0; index < count; ++index) group.issue_delays.push_back(random() % 8U);
    for (const auto &event : graph.after_provisional) {
        if (event.kind != UnresolvedOperandKind::address) continue;
        unsigned victim = count;
        for (unsigned index = 0; index < count; ++index)
            if (unresolved_overlap(graph.stores.at(event.store).bytes, group.loads[index])) {
                victim = index;
                break;
            }
        if (victim == count) continue;
        UnresolvedRecoveryRound round{event.store, victim,
            unresolved_random_order(random, victim, count), {}};
        for (unsigned index = victim; index < count; ++index) round.issue_delays.push_back(random() % 8U);
        group.rounds.push_back(std::move(round));
        group.victim = std::min(group.victim, victim);
    }
    return group;
}

// A flush-itself restarts fetch at the victim PC in the next FTQ entry.
// The bounded group fits one16-byte packet and deliberately avoids FTQ wrap.
template<class Load>
inline void reset_unresolved_ftq_suffix(std::vector<Load> &loads, unsigned victim)
{
    if (victim >= loads.size() || loads[victim].ftq_ptr >= 63)
        throw std::invalid_argument("invalid unresolved FTQ recovery boundary");
    unsigned halfwords = 0;
    for (unsigned index = victim; index < loads.size(); ++index)
        halfwords += loads[index].predecode_rvc ? 1U : 2U;
    if (halfwords > 8) throw std::invalid_argument("unresolved FTQ suffix exceeds packet");
    const auto next = loads[victim].ftq_ptr + 1U;
    unsigned offset = 0;
    for (unsigned index = victim; index < loads.size(); ++index) {
        loads[index].ftq_ptr = next;
        loads[index].ftq_offset = offset;
        offset += loads[index].predecode_rvc ? 1U : 2U;
    }
}

// Warm initial executions and every refetch complete before a new STA. A youngest
// held cold role instead owes an observed initial cancellation before refetch.
// Architectural obligations survive completion, cancellation and invalidation.
class UnresolvedLoadGroupObligation {
public:
    template<class Load>
    UnresolvedLoadGroupObligation(const std::vector<Load> &loads, unsigned maximum_rounds,
        std::optional<unsigned> unfinished = {})
        : maximum_rounds_(maximum_rounds), unfinished_(unfinished), executions_(loads.size()), pending_(loads.size(), true)
    {
        if (loads.empty() || maximum_rounds == 0 ||
            (unfinished && (*unfinished + 1U != loads.size() || loads.size() < 2))) { failed_ = true; return; }
        for (const auto &load : loads) identities_.push_back({load.rob, load.rob_flag,
            load.ftq_ptr, load.ftq_offset, load.predecode_rvc, load.pdest});
    }
    bool provisional(unsigned index)
    {
        if (failed_ || stage_ != Stage::initial || index >= pending_.size() ||
            (unfinished_ && index == *unfinished_) || !pending_[index]) return fail();
        pending_[index] = false;
        ++executions_[index];
        if (all_complete()) stage_ = Stage::ready;
        return true;
    }
    template<class Redirect>
    bool redirect(const Redirect &event, unsigned victim)
    {
        bool initial_unfinished = stage_ == Stage::initial && unfinished_ && rounds_ == 0 &&
            victim < *unfinished_ && pending_[*unfinished_];
        if (initial_unfinished)
            for (unsigned index = 0; index < pending_.size(); ++index)
                if (index != *unfinished_ && pending_[index]) initial_unfinished = false;
        if (failed_ || (stage_ != Stage::ready && !initial_unfinished) ||
            victim >= identities_.size() || rounds_ >= maximum_rounds_) return fail();
        const auto &id = identities_[victim];
        if (!event.valid || !event.level || event.ftq_flag || event.rob_value != id.rob ||
            event.rob_flag != id.flag || event.ftq_value != id.ftq ||
            event.ftq_offset != id.offset || event.is_rvc != id.rvc) return fail();
        victim_ = victim;
        for (unsigned index = victim; index < pending_.size(); ++index) pending_[index] = true;
        stage_ = Stage::redirected;
        return true;
    }
    bool initial_canceled(unsigned index)
    {
        if (failed_ || stage_ != Stage::redirected || rounds_ != 0 ||
            !unfinished_ || index != *unfinished_ || initial_canceled_) return fail();
        initial_canceled_ = true;
        return true;
    }
    bool returned()
    {
        if (failed_ || stage_ != Stage::redirected ||
            (rounds_ == 0 && unfinished_ && !initial_canceled_)) return fail();
        const auto next_ftq = identities_[victim_].ftq + 1U;
        if (next_ftq >= 64) return fail();
        unsigned offset = 0;
        for (unsigned index = victim_; index < identities_.size(); ++index) {
            identities_[index].ftq = next_ftq;
            identities_[index].offset = offset;
            offset += identities_[index].rvc ? 1U : 2U;
        }
        if (offset > 8) return fail();
        stage_ = Stage::returned;
        return true;
    }
    template<class Load>
    bool refetched(unsigned index, const Load &load)
    {
        if (failed_ || stage_ != Stage::returned || index < victim_ ||
            index >= pending_.size() || !pending_[index]) return fail();
        const auto &id = identities_[index];
        if (load.rob != id.rob || load.rob_flag != id.flag || load.pdest != id.pdest ||
            load.ftq_ptr != id.ftq || load.ftq_offset != id.offset || load.predecode_rvc != id.rvc)
            return fail();
        pending_[index] = false;
        ++executions_[index];
        return true;
    }
    bool finish_round()
    {
        if (failed_ || stage_ != Stage::returned || !all_complete()) return fail();
        ++rounds_;
        stage_ = Stage::ready;
        return true;
    }
    bool resolved()
    {
        if (failed_ || stage_ != Stage::ready || !all_complete()) return fail();
        stage_ = Stage::resolved;
        return true;
    }
    bool readback()
    {
        if (failed_ || stage_ != Stage::resolved) return fail();
        stage_ = Stage::complete;
        return true;
    }
    bool done() const { return !failed_ && stage_ == Stage::complete; }
    unsigned executions(unsigned index) const { return executions_.at(index); }
    unsigned rounds() const { return rounds_; }

private:
    struct Identity { unsigned rob; bool flag; std::uint64_t ftq; unsigned offset; bool rvc; unsigned pdest; };
    enum class Stage { initial, ready, redirected, returned, resolved, complete };
    bool fail() { failed_ = true; return false; }
    bool all_complete() const { return std::none_of(pending_.begin(), pending_.end(), [](bool pending) { return pending; }); }
    unsigned maximum_rounds_, victim_ = 0, rounds_ = 0;
    std::optional<unsigned> unfinished_;
    bool initial_canceled_ = false;
    std::vector<Identity> identities_;
    std::vector<unsigned> executions_;
    std::vector<bool> pending_;
    bool failed_ = false;
    Stage stage_ = Stage::initial;
};

} // namespace memblock::scenarios::mixed
