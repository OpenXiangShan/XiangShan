#pragma once

#include "unresolved_dependency.hpp"

namespace memblock::scenarios::mixed {

struct UnresolvedGraphStore {
    DependencyStore bytes;
    bool data_before_load = false;
};

enum class UnresolvedOperandKind { address, data };
struct UnresolvedOperandEvent {
    UnresolvedOperandKind kind;
    unsigned store;
    unsigned delay;
};

struct UnresolvedStoreGraph {
    std::vector<UnresolvedGraphStore> stores;
    DependencyLoad load;
    bool conflict = false;
    std::vector<UnresolvedOperandEvent> after_provisional;
};

// All older addresses are withheld through the provisional execution. The
// ready bag then contains each STA and every still-unissued STD exactly once;
// it imposes no artificial age order on the independent address producers.
inline UnresolvedStoreGraph make_unresolved_store_graph(std::mt19937_64 &random,
    unsigned conflict_per_mille, const ScalarLoadPolicy &load_policy = {},
    unsigned maximum_stores = 4)
{
    if (conflict_per_mille > 1000 || maximum_stores == 0 || maximum_stores > 4)
        throw std::invalid_argument("invalid unresolved store graph constraint");
    UnresolvedStoreGraph graph;
    const unsigned count = 1U + random() % maximum_stores;
    const auto format = load_policy.choose(random);
    graph.load = {0, format.op, format.fp};
    const unsigned width = graph.load.width();
    const unsigned cell = (random() % 4U) * 16U;
    graph.load.offset = cell + (random() % (16U / width)) * width;
    graph.conflict = random() % 1000U < conflict_per_mille;
    const unsigned forced = random() % count;
    for (unsigned index = 0; index < count; ++index) {
        const unsigned store_width = 1U << (random() % 4U);
        const bool overlap = graph.conflict && (index == forced || (random() & 1U));
        std::vector<unsigned> candidates;
        for (unsigned offset = cell; offset + store_width <= cell + 16U; offset += store_width) {
            const bool intersects = offset < graph.load.offset + width &&
                graph.load.offset < offset + store_width;
            if (intersects == overlap) candidates.push_back(offset);
        }
        if (candidates.empty()) throw std::logic_error("unreachable unresolved byte geometry");
        graph.stores.push_back({{candidates[random() % candidates.size()],
            store_width, random()}, (random() & 1U) != 0});
    }
    std::vector<UnresolvedOperandEvent> ready;
    for (unsigned index = 0; index < count; ++index) {
        ready.push_back({UnresolvedOperandKind::address, index, 0});
        if (!graph.stores[index].data_before_load)
            ready.push_back({UnresolvedOperandKind::data, index, 0});
    }
    while (!ready.empty()) {
        const unsigned choice = random() % ready.size();
        auto event = ready[choice];
        event.delay = random() % 8U;
        graph.after_provisional.push_back(event);
        ready.erase(ready.begin() + choice);
    }
    return graph;
}

inline std::vector<DependencyStore> unresolved_graph_bytes(const UnresolvedStoreGraph &graph)
{
    std::vector<DependencyStore> stores;
    for (const auto &store : graph.stores) stores.push_back(store.bytes);
    return stores;
}

// Make stale provisional data distinguishable even if a younger older-store
// overlay hides earlier writes. Only the youngest input writer of one loaded
// byte is changed; neither a DUT result nor a seeded failure selects it.
inline void distinguish_unresolved_graph(UnresolvedStoreGraph &graph,
    const std::array<unsigned char, 64> &initial)
{
    if (!graph.conflict) return;
    for (unsigned byte = graph.load.offset; byte < graph.load.offset + graph.load.width(); ++byte) {
        for (auto it = graph.stores.rbegin(); it != graph.stores.rend(); ++it) {
            auto &store = it->bytes;
            if (byte < store.offset || byte >= store.offset + store.width) continue;
            const unsigned shift = (byte - store.offset) * 8U;
            if (static_cast<unsigned char>(store.data >> shift) == initial[byte])
                store.data ^= std::uint64_t{1} << shift;
            return;
        }
    }
    throw std::logic_error("conflicting unresolved graph has no overlapping input byte");
}

inline std::vector<std::uint8_t> unresolved_graph_destinations(std::mt19937_64 &random,
    unsigned count, unsigned registers)
{
    if (count == 0 || registers > 256 || count >= registers)
        throw std::invalid_argument("invalid unresolved graph destination domain");
    const unsigned first = 1U + random() % (registers - count);
    std::vector<std::uint8_t> result;
    for (unsigned role = 0; role < count; ++role)
        result.push_back(static_cast<std::uint8_t>(first + role));
    return result;
}

} // namespace memblock::scenarios::mixed
