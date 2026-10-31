#pragma once
#include <cstdint>
#include <random>
#include <stdexcept>

namespace memblock::scenarios::mixed {

// An omitted, independent nonmemory producer can wake each source later than
// memory admission. This deadline never substitutes for a checked memory
// dependency, and never changes/resamples on a memory retry.
struct StreamOperandReady {
    std::uint64_t cycle = 0;
    unsigned delay = 0;
    bool consumed = false;

    static StreamOperandReady choose(std::mt19937_64 &random, std::uint64_t admission,
        unsigned rate, unsigned maximum)
    {
        if (rate > 1000 || maximum == 0 || maximum > 4096)
            throw std::invalid_argument("invalid stream operand readiness constraints");
        const unsigned delay = rate != 0 && (rate == 1000 || random() % 1000U < rate)
            ? 1U + unsigned(random() % maximum) : 0U;
        return {admission + delay, delay, false};
    }

    bool ready(std::uint64_t edge) const { return edge >= cycle; }
    bool waiting(std::uint64_t edge) const { return delay != 0 && !consumed && edge < cycle; }
};

} // namespace memblock::scenarios::mixed
