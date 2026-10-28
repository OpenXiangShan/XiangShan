#pragma once

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

    unsigned width() const { return 1U << (op & 3U); }
};

struct ScalarDependencyPlan {
    std::vector<DependencyStore> older;
    std::vector<DependencyLoad> loads;
    std::optional<DependencyStore> younger;
    // Two events per store: even=address, odd=data. Store indices are in
    // architectural order; event order describes independent operand arrival.
    std::vector<unsigned> store_events;
    std::vector<unsigned> load_order;
    std::vector<unsigned> event_delays;
};

inline ScalarDependencyPlan make_scalar_dependency_plan(std::mt19937_64 &random)
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
        const unsigned op = random() % 7U;
        const unsigned width = 1U << (op & 3U);
        const unsigned offset = random() % 3U == 0
            ? (random() % (32U / width)) * width
            : plan.older[random() % stores].offset & ~(width - 1U);
        plan.loads.push_back({offset, op});
        plan.load_order.push_back(index);
    }
    if ((random() & 1U) != 0) {
        plan.younger = DependencyStore{
            plan.loads[0].offset, plan.loads[0].width(), random()};
    }
    const unsigned all_stores = stores + unsigned(plan.younger.has_value());
    for (unsigned event = 0; event < all_stores * 2U; ++event) {
        plan.store_events.push_back(event);
        plan.event_delays.push_back(random() % 8U);
    }
    std::shuffle(plan.store_events.begin(), plan.store_events.end(), random);
    std::shuffle(plan.load_order.begin(), plan.load_order.end(), random);
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
    const unsigned bits = load.width() * 8U;
    if (load.op < 3U && (value & (std::uint64_t{1} << (bits - 1U))) != 0) {
        value |= ~((std::uint64_t{1} << bits) - 1U);
    }
    return value;
}

} // namespace memblock::scenarios::mixed
