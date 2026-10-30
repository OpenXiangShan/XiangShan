#pragma once
#include <cstdint>
#include <stdexcept>

namespace memblock::scenarios::mixed {

// External integer ALU model over an independently declared mapped window.
// Guard pages remain available for vector expansion and negative strides.
// This selects a dependent effective address, not a split scalar operation.
inline std::uint64_t stream_dependency_address(std::uint64_t address,
    std::uint64_t result, std::uint64_t base, unsigned pages)
{
    if ((base & 4095U) || pages < 3 || address < base ||
        address - base >= std::uint64_t{pages} * 4096U)
        throw std::logic_error("dependent address outside declared mapped window");
    const auto page = (address - base) / 4096U;
    const auto interior = pages - 2U;
    const auto target = 1U + (page + interior - 1U + ((result >> 6U) % interior)) % interior;
    const auto offset = ((address & 4095U) + ((result & 63U) << 6U)) & 4095U;
    return base + target * 4096U + offset;
}

} // namespace memblock::scenarios::mixed
