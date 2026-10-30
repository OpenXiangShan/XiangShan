#pragma once
#include "stream_oracle.hpp"
#include <algorithm>
#include <set>

namespace memblock::scenarios::mixed {

// Declared physical cache-line footprint, independent of DUT execution.
// A whole line is deliberately reserved even for a sub-line store: this
// direction exercises unrelated accesses, leaving overlap/recovery to its
// own oracle and legal redirect contract. Inactive vector elements add none.
using StreamFootprint = std::set<std::uint64_t>;

inline StreamFootprint stream_footprint(const memblock::LoadTransaction &t)
{
    StreamFootprint lines;
    const unsigned width = 1U << (static_cast<unsigned>(t.op) & 3U);
    for (unsigned byte = 0; byte < width; ++byte)
        lines.insert(memblock::reference_byte_address(t, t.address + byte) / 64U);
    return lines;
}

inline StreamFootprint stream_footprint(const memblock::StoreTransaction &t)
{
    StreamFootprint lines;
    const unsigned width = 1U << static_cast<unsigned>(t.op);
    for (unsigned byte = 0; byte < width; ++byte)
        lines.insert(memblock::reference_byte_address(t, t.address + byte) / 64U);
    return lines;
}

inline StreamFootprint stream_footprint(const std::vector<memblock::VectorMemoryTransaction> &uops)
{
    StreamFootprint lines;
    for (const auto &t : uops) {
        const unsigned width = 1U << memblock::vector_data_eew(t);
        const auto active = memblock::active_vector_elements(t);
        for (unsigned element = 0; element < 16U / width; ++element)
            if ((active >> element) & 1U)
                for (unsigned byte = 0; byte < width; ++byte)
                    lines.insert(memblock::vector_byte_address(t, element, byte) / 64U);
    }
    return lines;
}

inline bool stream_disjoint(const StreamFootprint &a, const StreamFootprint &b)
{
    return std::none_of(a.begin(), a.end(), [&](auto line) { return b.count(line) != 0; });
}

} // namespace memblock::scenarios::mixed
