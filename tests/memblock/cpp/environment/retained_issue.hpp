#pragma once

#include "memblock_env.hpp"
#include <variant>

namespace memblock {

// Owns all ordinary producer issue pins during an explicitly bound interval.
// This is a pin driver, not an IQ, scoreboard, or cancellation authorization.
// Call capture_before_edge only after manager drive and combinational refresh;
// call advance_after_edge exactly once after that edge. Store acceptance still
// needs the environment's separate pre-edge scoreboard/release authorization.
class RetainedIssueDriver {
public:
    enum class Kind { load, store_address, store_data, vector };
    enum class Outcome { accepted, retry, canceled };
    struct Redirect {
        bool valid = false;
        std::uint8_t rob = 0;
        bool flag = false, flush_itself = false;
    };
    struct Event {
        std::uint64_t tag, edge;
        Kind kind;
        unsigned lane;
        Outcome outcome;
        // A public acceptance can coexist with delayed producer cancellation.
        // It remains an acceptance, without promising a later terminal result.
        bool killed;
    };

    bool empty() const {
        return std::none_of(slots_.begin(), slots_.end(), [](const auto &slot) { return slot.has_value(); });
    }
    bool available(Kind kind, unsigned lane) const { return !slots_[index(kind, lane)]; }

    void offer_load(std::uint64_t tag, unsigned lane, const generated::ScalarLoadIssue &issue) {
        offer(tag, Kind::load, lane, issue);
    }
    void offer_store_address(std::uint64_t tag, unsigned lane, const generated::ScalarStoreIssue &issue) {
        offer(tag, Kind::store_address, lane, issue);
    }
    void offer_store_data(std::uint64_t tag, unsigned lane, const generated::ScalarStoreIssue &issue) {
        offer(tag, Kind::store_data, lane, issue);
    }
    void offer_vector(std::uint64_t tag, unsigned lane, const generated::VectorMemoryIssue &issue) {
        offer(tag, Kind::vector, lane, issue);
    }

    template<class Adapter> static void clear_pins(Adapter &dut) {
        dut.clear_scalar_load_issue_valids();
        dut.clear_scalar_store_issue_valids();
        dut.clear_vector_memory_issue_valids();
    }
    template<class Adapter> void drive(Adapter &dut) const {
        if (sampled_) throw std::logic_error("producer drive between capture and edge");
        clear_pins(dut);
        for (const auto &slot : slots_) {
            if (!slot) continue;
            const auto &request = *slot;
            switch (request.kind) {
            case Kind::load:
                dut.drive_scalar_load_issue(request.lane, std::get<generated::ScalarLoadIssue>(request.payload)); break;
            case Kind::store_address:
                dut.drive_scalar_store_address(request.lane, std::get<generated::ScalarStoreIssue>(request.payload)); break;
            case Kind::store_data:
                dut.drive_scalar_store_data(request.lane, std::get<generated::ScalarStoreIssue>(request.payload)); break;
            case Kind::vector:
                dut.drive_vector_memory_issue(request.lane, std::get<generated::VectorMemoryIssue>(request.payload)); break;
            }
        }
    }

    template<class Adapter> void capture_before_edge(const Adapter &dut, std::uint64_t edge, Redirect redirect) {
        if (sampled_) throw std::logic_error("producer edge captured twice");
        if (last_edge_ && edge != *last_edge_ + 1U)
            throw std::logic_error("producer driver missed an edge");
        if (redirect.valid && redirect.rob >= kRobEntries)
            throw std::invalid_argument("producer redirect ROB out of range");
        sampled_events_.clear();
        for (const auto &slot : slots_) {
            if (!slot) continue;
            const auto &request = *slot;
            bool accepted = false;
            switch (request.kind) {
            case Kind::load: accepted = dut.scalar_load_issue_ready(request.lane); break;
            case Kind::store_address: accepted = dut.scalar_store_address_ready(request.lane); break;
            case Kind::store_data: accepted = dut.scalar_store_data_ready(request.lane); break;
            case Kind::vector: accepted = dut.vector_memory_issue_ready(request.lane); break;
            }
            const bool killed = kills(delayed_redirect_, request);
            const bool timeout = request.kind == Kind::vector && request.stalled_edges == 13U;
            if (accepted || killed || timeout)
                sampled_events_.push_back(Event{request.tag, edge, request.kind, request.lane,
                    accepted ? Outcome::accepted : killed ? Outcome::canceled : Outcome::retry, killed});
        }
        sampled_redirect_ = redirect;
        sampled_edge_ = edge;
        sampled_ = true;
    }

    template<class Adapter> void advance_after_edge(Adapter &dut) {
        if (!sampled_) throw std::logic_error("producer edge advanced without capture");
        for (const auto &event : sampled_events_) {
            slots_[index(event.kind, event.lane)].reset();
            events_.push_back(event);
        }
        for (auto &slot : slots_)
            if (slot && slot->kind == Kind::vector) ++slot->stalled_edges;
        delayed_redirect_ = sampled_redirect_;
        last_edge_ = sampled_edge_;
        sampled_ = false;
        drive(dut);
    }

    std::vector<Event> take_events() {
        if (sampled_) throw std::logic_error("producer events consumed before edge");
        auto result = std::move(events_);
        events_.clear();
        return result;
    }

private:
    using Payload = std::variant<generated::ScalarLoadIssue, generated::ScalarStoreIssue, generated::VectorMemoryIssue>;
    struct Request {
        std::uint64_t tag;
        Kind kind;
        unsigned lane;
        Payload payload;
        unsigned stalled_edges = 0;
    };
    static unsigned index(Kind kind, unsigned lane) {
        switch (kind) {
        case Kind::load:
            if (lane < kScalarLoadLanes) return lane;
            break;
        case Kind::store_address:
            if (lane < kScalarStoreLanes) return kScalarLoadLanes + lane;
            break;
        case Kind::store_data:
            if (lane < kScalarStoreLanes) return kScalarLoadLanes + kScalarStoreLanes + lane;
            break;
        case Kind::vector:
            if (lane < kVectorMemoryLanes) return kScalarLoadLanes + 2U * kScalarStoreLanes + lane;
            break;
        }
        throw std::invalid_argument("producer kind/lane out of range");
    }
    static bool kills(const Redirect &redirect, const Request &request) {
        if (!redirect.valid) return false;
        return std::visit([&](const auto &payload) {
            const bool same = payload.rob_value == redirect.rob && payload.rob_flag == redirect.flag;
            const bool younger = payload.rob_flag == redirect.flag ? payload.rob_value > redirect.rob : payload.rob_value < redirect.rob;
            return younger || (same && redirect.flush_itself);
        }, request.payload);
    }
    template<class Issue> void offer(std::uint64_t tag, Kind kind, unsigned lane, const Issue &issue) {
        if (sampled_) throw std::logic_error("producer offer between capture and edge");
        const auto position = index(kind, lane);
        if (issue.rob_value >= kRobEntries) throw std::invalid_argument("producer ROB out of range");
        if (slots_[position]) throw std::logic_error("producer lane already occupied");
        if (std::any_of(slots_.begin(), slots_.end(), [&](const auto &slot) { return slot && slot->tag == tag; }) ||
            std::any_of(events_.begin(), events_.end(), [&](const auto &event) { return event.tag == tag; }))
            throw std::logic_error("producer tag already outstanding");
        slots_[position] = Request{tag, kind, lane, issue};
    }
    std::array<std::optional<Request>, kScalarLoadLanes + 2U * kScalarStoreLanes + kVectorMemoryLanes> slots_{};
    std::vector<Event> events_, sampled_events_;
    Redirect delayed_redirect_{}, sampled_redirect_{};
    std::optional<std::uint64_t> last_edge_;
    std::uint64_t sampled_edge_ = 0;
    bool sampled_ = false;
};

} // namespace memblock
