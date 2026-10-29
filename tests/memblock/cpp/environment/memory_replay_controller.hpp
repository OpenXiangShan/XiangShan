#pragma once
#include "memblock_env.hpp"
#include <limits>

namespace memblock {
// The opt-in owner supplies decoded instruction identities and calls plan once
// before each edge, including edges inside blocking helpers. This models only
// the uncontested memory-replay return path, with no EXU redirect or ROB flush.
// It neither derives architectural values nor certifies CPU walk completion.
class MemoryReplayController {
public:
    struct Instruction {
        std::uint64_t generation = 0;
        std::uint8_t rob = 0;
        bool flag = false;
        std::uint8_t ftq = 0, offset = 0;
        bool ftq_flag = false, rvc = false, replayable_load = true;
        std::optional<std::uint64_t> killed_by;
    };
    struct Return {
        std::uint64_t id, generation, request_edge, return_edge;
        generated::MemoryViolation request;
        std::vector<std::uint64_t> newly_killed;
        bool returned = false, cancellation_observed = false;
    };
    struct Actions {
        std::optional<Return> redirect;
        // Counts become available AFTER this edge. The owner samples once
        // after stepping and explicitly acknowledges the corresponding ID.
        std::optional<Return> sample_cancellation;
    };

    std::optional<std::uint64_t> admit(Instruction instruction) {
        if (!error_.empty()) return {};
        if (!returns_.empty() || instructions_.size() >= kRobEntries ||
            instruction.rob >= kRobEntries || instruction.ftq >= 64 || instruction.offset >= 8 ||
            next_generation_ == std::numeric_limits<std::uint64_t>::max()) {
            fail("memory replay instruction admission violates recovery, identity or capacity bounds");
            return {};
        }
        const auto packed = [](const Instruction &item) {
            return unsigned(item.rob) + (item.flag ? kRobEntries : 0U);
        };
        for (const auto &live : instructions_) {
            const auto distance = (packed(instruction) + 2U*kRobEntries - packed(live)) % (2U*kRobEntries);
            if (distance == 0 || distance >= kRobEntries) {
                fail("memory replay instruction admission is not a unique increasing live ROB age");
                return {};
            }
        }
        instruction.generation = ++next_generation_;
        instruction.killed_by.reset();
        instructions_.push_back(instruction);
        return instruction.generation;
    }

    bool retire(std::uint64_t generation) {
        if (!error_.empty()) return false;
        const auto found = std::find_if(instructions_.begin(), instructions_.end(),
            [&](const auto &item) { return item.generation == generation; });
        if (found == instructions_.end() || found != instructions_.begin() || found->killed_by) {
            fail("memory replay retirement is not an uncanceled architectural prefix");
            return false;
        }
        instructions_.erase(found);
        return true;
    }

    Actions plan(std::uint64_t edge, const generated::MemoryViolation &request = {}) {
        Actions action;
        if (!error_.empty()) return action;
        error_.set_cycle(edge);
        if (last_edge_ && (*last_edge_ == std::numeric_limits<std::uint64_t>::max() || edge != *last_edge_ + 1U)) {
            fail("memory replay controller skipped or repeated an edge");
            return action;
        }
        last_edge_ = edge;
        for (const auto &item : returns_) {
            if ((!item.returned && item.return_edge < edge) ||
                (!item.cancellation_observed && item.return_edge + 2U < edge)) {
                fail("memory replay owner missed a return or cancellation observation edge");
                return action;
            }
        }
        if (request.valid && !observe(edge, request)) return action;
        for (const auto &item : returns_) {
            if (item.return_edge == edge) action.redirect = item;
            if (item.return_edge + 2U == edge) action.sample_cancellation = item;
        }
        return action;
    }

    bool returned(std::uint64_t id, std::uint64_t edge) {
        if (!error_.empty()) return false;
        auto *item = lookup(id);
        if (!item || item->returned || item->return_edge != edge || !last_edge_ || *last_edge_ != edge) {
            fail("memory replay return acknowledgment has wrong identity or edge");
            return false;
        }
        item->returned = true;
        return true;
    }

    bool cancellation_observed(std::uint64_t id, std::uint64_t edge) {
        if (!error_.empty()) return false;
        auto *item = lookup(id);
        if (!item || !item->returned || item->cancellation_observed || item->return_edge + 2U != edge ||
            !last_edge_ || *last_edge_ != edge) {
            fail("memory replay cancellation acknowledgment has wrong identity or edge");
            return false;
        }
        item->cancellation_observed = true;
        return true;
    }

    bool recovery_ready(std::uint64_t edge) const {
        return error_.empty() && last_edge_ && *last_edge_ == edge && !returns_.empty() &&
            std::all_of(returns_.begin(), returns_.end(), [](const auto &item) {
                return item.returned && item.cancellation_observed;
            }) && edge >= returns_.back().return_edge + kLsqRedirectRecoveryEdges + 1U;
    }
    // The timer is a selected local minimum, not an assertion that full CPU
    // walk/RAB work or old memory responses have finished. The owner certifies
    // those additional producer/response obligations before reusing identities.
    bool finish_recovery(std::uint64_t edge, bool producer_recovery_complete) {
        if (!recovery_ready(edge) || !producer_recovery_complete) {
            fail("memory replay recovery lacks reconciled returns or certified producer completion");
            return false;
        }
        std::erase_if(instructions_, [](const auto &item) { return item.killed_by.has_value(); });
        returns_.clear();
        return true;
    }

    bool recovering() const { return !returns_.empty(); }
    bool will_cancel(std::uint64_t generation) const {
        const auto found = std::find_if(instructions_.begin(), instructions_.end(),
            [&](const auto &item) { return item.generation == generation; });
        return found != instructions_.end() && found->killed_by.has_value();
    }
    const auto &instructions() const { return instructions_; }
    const auto &returns() const { return returns_; }
    std::uint64_t selected() const { return next_return_; }
    std::uint64_t filtered() const { return filtered_; }
    const FailureState &failures() const { return error_; }
    bool ok() const { return error_.empty(); }

private:
    static bool kills(std::uint64_t victim, bool itself, std::uint64_t generation) {
        return generation > victim || (itself && generation == victim);
    }
    bool observe(std::uint64_t edge, const generated::MemoryViolation &request) {
        if (next_return_ == std::numeric_limits<std::uint64_t>::max() ||
            edge > std::numeric_limits<std::uint64_t>::max() - kLsqRedirectRecoveryEdges - 4U ||
            returns_.size() >= (1U << 16U)) {
            fail("memory replay controller history or edge budget exhausted");
            return false;
        }
        const auto found = std::find_if(instructions_.begin(), instructions_.end(), [&](const auto &item) {
            return item.rob == request.rob_value && item.flag == request.rob_flag;
        });
        if (found == instructions_.end() || !found->replayable_load ||
            found->ftq != request.ftq_value || found->ftq_flag != request.ftq_flag ||
            found->offset != request.ftq_offset || found->rvc != request.is_rvc) {
            fail("memory replay request does not match an independently registered live instruction");
            return false;
        }
        // RedirectGenerator retains the last selected redirect for four later
        // selection edges. Selection is one edge after this public request;
        // relative public-edge differences therefore use the same four edges.
        if (!returns_.empty()) {
            const auto &filter = returns_.back();
            if (edge <= filter.request_edge + 4U && kills(filter.generation, filter.request.level, found->generation)) {
                ++filtered_;
                return true;
            }
        }
        if (found->killed_by) {
            fail("memory replay request belongs to a canceled generation outside the retained filter");
            return false;
        }
        Return pending{++next_return_, found->generation, edge, edge + 2U, request, {}};
        for (auto &instruction : instructions_) {
            if (!instruction.killed_by && kills(found->generation, request.level, instruction.generation)) {
                instruction.killed_by = pending.id;
                pending.newly_killed.push_back(instruction.generation);
            }
        }
        returns_.push_back(std::move(pending));
        return true;
    }
    Return *lookup(std::uint64_t id) {
        const auto found = std::find_if(returns_.begin(), returns_.end(),
            [&](const auto &item) { return item.id == id; });
        return found == returns_.end() ? nullptr : &*found;
    }
    void fail(const char *message) { error_.record(message); }
    std::vector<Instruction> instructions_;
    std::vector<Return> returns_;
    std::optional<std::uint64_t> last_edge_;
    std::uint64_t next_generation_ = 0, next_return_ = 0, filtered_ = 0;
    FailureState error_{"memory-replay", "controller-contract"};
};
} // namespace memblock
