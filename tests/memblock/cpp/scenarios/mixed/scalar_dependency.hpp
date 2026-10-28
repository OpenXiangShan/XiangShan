#pragma once

#include "memblock_env.hpp"
#include "scalar_load.hpp"
#include <algorithm>
#include <array>
#include <bit>
#include <cstdint>
#include <optional>
#include <random>
#include <vector>

namespace memblock::scenarios::mixed {

struct DependencyStore {
    unsigned offset;
    unsigned width;
    std::uint64_t data;
};

struct DependencyLoad {
    unsigned offset;
    unsigned op;
    bool fp = false;

    unsigned width() const { return 1U << (op & 3U); }
};

enum class DependencyIssueKind { address, data, load };

struct DependencyIssueEvent {
    DependencyIssueKind kind;
    unsigned index;
    unsigned delay;
};

struct ScalarDependencyPlan {
    std::vector<DependencyStore> older;
    std::vector<DependencyLoad> loads;
    std::optional<DependencyStore> younger;
    // Store indices retain architectural age. A load is ready after all older
    // STA events, while their STD and the younger store may still be pending.
    std::vector<DependencyIssueEvent> issue_events;
};

inline ScalarDependencyPlan make_scalar_dependency_plan(std::mt19937_64 &random,
    const ScalarLoadPolicy &load_policy = {})
{
    ScalarDependencyPlan plan;
    const unsigned stores = 1U + random() % 4U;
    const unsigned loads = 1U + random() % 3U;
    for (unsigned index = 0; index < stores; ++index) {
        const unsigned width = 1U << (random() % 4U);
        unsigned offset = (random() % (32U / width)) * width;
        if (index != 0 && random() % 3U != 0) {
            const auto &previous = plan.older[random() % index];
            offset = previous.offset & ~(width - 1U);
        }
        plan.older.push_back({offset, width, random()});
    }
    for (unsigned index = 0; index < loads; ++index) {
        const auto shape = load_policy.choose(random);
        const unsigned op = shape.op;
        const unsigned width = 1U << (op & 3U);
        const unsigned offset = random() % 3U == 0
            ? (random() % (32U / width)) * width
            : plan.older[random() % stores].offset & ~(width - 1U);
        plan.loads.push_back({offset, op, shape.fp});
    }
    if ((random() & 1U) != 0) {
        plan.younger = DependencyStore{
            plan.loads[0].offset, plan.loads[0].width(), random()};
    }
    const unsigned all_stores = stores + unsigned(plan.younger.has_value());
    std::vector<bool> address_done(all_stores, false);
    std::vector<bool> data_done(all_stores, false);
    std::vector<bool> load_done(loads, false);
    while (plan.issue_events.size() < all_stores * 2U + loads) {
        std::vector<DependencyIssueEvent> ready;
        for (unsigned store = 0; store < all_stores; ++store) {
            if (!address_done[store]) ready.push_back({DependencyIssueKind::address, store, 0});
            if (!data_done[store]) ready.push_back({DependencyIssueKind::data, store, 0});
        }
        if (std::all_of(address_done.begin(), address_done.begin() + stores,
                [](bool done) { return done; })) {
            for (unsigned load = 0; load < loads; ++load) {
                if (!load_done[load]) ready.push_back({DependencyIssueKind::load, load, 0});
            }
        }
        auto event = ready[random() % ready.size()];
        event.delay = random() % 8U;
        switch (event.kind) {
        case DependencyIssueKind::address: address_done[event.index] = true; break;
        case DependencyIssueKind::data: data_done[event.index] = true; break;
        case DependencyIssueKind::load: load_done[event.index] = true; break;
        }
        plan.issue_events.push_back(event);
    }
    return plan;
}

// The snapshot predates every store in this window. Apply only older stores,
// in program order, independently of issue order, forwarding path or commit.
inline std::uint64_t scalar_dependency_expected(
    const std::array<unsigned char, 64> &snapshot,
    const std::vector<DependencyStore> &older, const DependencyLoad &load)
{
    std::uint64_t value = 0;
    for (unsigned byte = 0; byte < load.width(); ++byte) {
        const unsigned offset = load.offset + byte;
        unsigned char expected = snapshot.at(offset);
        for (const auto &store : older) {
            if (offset >= store.offset && offset - store.offset < store.width) {
                expected = static_cast<unsigned char>(
                    store.data >> (8U * (offset - store.offset)));
            }
        }
        value |= std::uint64_t{expected} << (8U * byte);
    }
    return memblock::scalar_load_value(static_cast<memblock::LoadOp>(load.op), load.fp, value);
}

} // namespace memblock::scenarios::mixed
