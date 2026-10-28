#pragma once

#include "scalar_dependency.hpp"
#include <stdexcept>
#include <utility>

namespace memblock::scenarios::mixed {

struct UnresolvedDependencyPlan {
    DependencyStore store;
    DependencyLoad load;
    bool conflict;
    bool data_before_load;
    unsigned resolve_delay;
};

inline UnresolvedDependencyPlan make_unresolved_dependency_plan(
    std::mt19937_64 &random, unsigned conflict_per_mille,
    const ScalarLoadPolicy &load_policy = {})
{
    if (conflict_per_mille > 1000) {
        throw std::invalid_argument("unresolved conflict rate exceeds 1000");
    }
    const unsigned store_width = 1U << (random() % 4U);
    const auto load_shape = load_policy.choose(random);
    const unsigned load_op = load_shape.op;
    const unsigned load_width = 1U << (load_op & 3U);
    const bool conflict = random() % 1000U < conflict_per_mille;
    // Keep both operands in one 16-byte comparison region. The disjoint
    // control tests byte-mask rejection rather than a different cache line.
    std::vector<std::pair<unsigned, unsigned>> candidates;
    for (unsigned s = 0; s + store_width <= 16; s += store_width) {
        for (unsigned l = 0; l + load_width <= 16; l += load_width) {
            const bool overlap = s < l + load_width && l < s + store_width;
            if (overlap == conflict) candidates.emplace_back(s, l);
        }
    }
    const auto [store_offset, load_offset] = candidates[random() % candidates.size()];
    const unsigned cell = (random() % 4U) * 16U;
    return {{cell + store_offset, store_width, random()},
        {cell + load_offset, load_op, load_shape.fp}, conflict, (random() & 1U) != 0,
        static_cast<unsigned>(random() % 8U)};
}

inline std::array<std::uint8_t, 3> unresolved_load_destinations(std::mt19937_64 &random,
    unsigned physical_registers = memblock::kIntegerPhysicalRegisters)
{
    // The warm load's mapping remains live across the held anchor, and the
    // terminal load's mapping remains live at readback. Refetch restores the
    // original free-list head, reusing the provisional destination. Reserve
    // distinct registers for warm, provisional/refetch, and readback roles.
    if (physical_registers < 4 || physical_registers > 256)
        throw std::invalid_argument("invalid unresolved load destination domain");
    const unsigned first = 1U + random() % (physical_registers - 3U);
    return {static_cast<std::uint8_t>(first), static_cast<std::uint8_t>(first + 1U),
        static_cast<std::uint8_t>(first + 2U)};
}

// This scenario-owned obligation supplements strict per-execution data and
// metadata checks. A completed speculative execution is not an architectural
// success until address resolution, required recovery and store readback finish.
class UnresolvedLoadObligation {
public:
    template<class Load>
    UnresolvedLoadObligation(const Load &load, bool conflict)
        : conflict_(conflict), rob_(load.rob), rob_flag_(load.rob_flag),
          ftq_(load.ftq_ptr), ftq_offset_(load.ftq_offset), rvc_(load.predecode_rvc)
    {}

    bool provisional() { return advance(Stage::initial, Stage::provisional); }

    template<class Redirect>
    bool redirect(const Redirect &event)
    {
        if (!conflict_ || !event.valid || !event.level || event.ftq_flag ||
            event.rob_value != rob_ || event.rob_flag != rob_flag_ ||
            event.ftq_value != ftq_ || event.ftq_offset != ftq_offset_ ||
            event.is_rvc != rvc_) return fail();
        return advance(Stage::provisional, Stage::redirected);
    }
    bool returned() { return advance(Stage::redirected, Stage::returned); }
    bool refetched() { return advance(Stage::returned, Stage::refetched); }
    bool no_conflict()
    {
        return !conflict_ ? advance(Stage::provisional, Stage::resolved) : fail();
    }
    bool readback()
    {
        return advance(conflict_ ? Stage::refetched : Stage::resolved, Stage::complete);
    }
    bool done() const { return !failed_ && stage_ == Stage::complete; }

private:
    enum class Stage { initial, provisional, redirected, returned, refetched, resolved, complete };
    bool fail() { failed_ = true; return false; }
    bool advance(Stage from, Stage to)
    {
        if (failed_ || stage_ != from) return fail();
        stage_ = to;
        return true;
    }
    bool conflict_;
    unsigned rob_;
    bool rob_flag_;
    std::uint64_t ftq_;
    unsigned ftq_offset_;
    bool rvc_;
    bool failed_ = false;
    Stage stage_ = Stage::initial;
};

} // namespace memblock::scenarios::mixed
