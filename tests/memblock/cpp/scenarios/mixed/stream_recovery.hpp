#pragma once

#include "unresolved_epochs.hpp"

namespace memblock::scenarios::mixed {

// One bounded speculative episode inside the ordinary architectural deque.
// Transaction ownership and final architectural observation remain in the
// stream; this object owns only the unresolved input image and load epochs.
struct StreamRecovery {
    enum class Phase { admission, initial, operands, refetch, quiet };
    unsigned id = 0;
    std::uint64_t anchor = 0, base = 0;
    UnresolvedStoreGraph plan;
    UnresolvedLoadGroup group;
    std::vector<DependencyStore> bytes;
    std::optional<UnresolvedEpochImage> image;
    std::optional<UnresolvedLoadEpochs> obligation;
    std::vector<memblock::StoreTransaction> stores;
    std::vector<memblock::LoadTransaction> loads;
    std::vector<bool> promoted, trained;
    unsigned operand = 0, issue = 0, rounds = 0;
    unsigned first_ftq = 0, halfword = 0;
    std::uint64_t pc = 0, ready_cycle = 0;
    std::uint64_t sq_dequeue_limit = 0, sq_cancel_before = 0, scalar_cancel_before = 0;
    Phase phase = Phase::admission;

    unsigned size() const { return plan.stores.size() + group.loads.size(); }
    const std::vector<unsigned> &issue_order() const {
        return phase == Phase::refetch ? group.rounds.at(rounds - 1U).issue_order : group.issue_order;
    }
    const std::vector<unsigned> &issue_delays() const {
        return phase == Phase::refetch ? group.rounds.at(rounds - 1U).issue_delays : group.issue_delays;
    }
};

} // namespace memblock::scenarios::mixed
