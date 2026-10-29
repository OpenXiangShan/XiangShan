#pragma once

#include "unresolved_load_group.hpp"

namespace memblock::scenarios::mixed {

// This is a speculative input image, not architectural committed memory.
// Unknown-address stores cannot supply bytes. Known-address stores overlay in
// program age even if their address/data issue order differs.
class UnresolvedEpochImage {
public:
    UnresolvedEpochImage(const std::array<unsigned char, 64> &snapshot,
        const std::vector<DependencyStore> &stores)
        : snapshot_(snapshot), stores_(stores), known_(stores.size()) {}
    bool resolve(unsigned store)
    {
        if (store >= known_.size() || known_[store]) return false;
        known_[store] = true;
        ++count_;
        return true;
    }
    std::uint64_t expected(const DependencyLoad &load) const
    {
        std::vector<DependencyStore> visible;
        for (unsigned index = 0; index < stores_.size(); ++index)
            if (known_[index]) visible.push_back(stores_[index]);
        return scalar_dependency_expected(snapshot_, visible, load);
    }
    unsigned known_count() const { return count_; }
private:
    std::array<unsigned char, 64> snapshot_;
    std::vector<DependencyStore> stores_;
    std::vector<bool> known_;
    unsigned count_ = 0;
};

// For each STA, distinguish every visible new writer from the preceding input
// image. If a younger *known* store hides this writer completely, unchanged data
// is legal and recovery is checked by identity/epoch obligations instead.
inline void distinguish_unresolved_epochs(UnresolvedStoreGraph &graph,
    const UnresolvedLoadGroup &group, const std::array<unsigned char, 64> &snapshot)
{
    std::vector<bool> known(graph.stores.size());
    for (const auto &event : graph.after_provisional) {
        if (event.kind != UnresolvedOperandKind::address) continue;
        const unsigned selected = event.store;
        if (selected >= known.size() || known[selected]) throw std::logic_error("duplicate unresolved address event");
        std::vector<DependencyStore> previous;
        for (unsigned index = 0; index < known.size(); ++index)
            if (known[index]) previous.push_back(graph.stores[index].bytes);
        known[selected] = true;
        auto &writer = graph.stores[selected].bytes;
        for (const auto &load : group.loads) {
            for (unsigned byte = load.offset; byte < load.offset + load.width(); ++byte) {
                if (byte < writer.offset || byte >= writer.offset + writer.width) continue;
                bool hidden = false;
                for (unsigned younger = selected + 1U; younger < known.size(); ++younger) {
                    const auto &store = graph.stores[younger].bytes;
                    hidden |= known[younger] && byte >= store.offset && byte < store.offset + store.width;
                }
                if (hidden) continue;
                const auto old = scalar_dependency_expected(snapshot, previous,
                    {byte, static_cast<unsigned>(memblock::LoadOp::lbu), false});
                const unsigned shift = (byte - writer.offset) * 8U;
                if (static_cast<unsigned char>(writer.data >> shift) == old)
                    writer.data ^= std::uint64_t{1} << shift;
                break;
            }
        }
    }
}

} // namespace memblock::scenarios::mixed
