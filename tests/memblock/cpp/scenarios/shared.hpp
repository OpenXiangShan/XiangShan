#pragma once

#include "memblock_env.hpp"
#include "alias_refetch.hpp"
#include "mixed/scalar_load.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <limits>
#include <numeric>
#include <optional>
#include <random>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace memblock::scenarios {

struct Options {
    std::string_view test = "single-load";
    bool list_tests = false;
    std::uint64_t seed = 1;
    unsigned transactions = 200;
    bool backpressure = true;
    std::string constraint_profile = "coverage";
    std::vector<std::string> constraint_overrides;
};

int run_mixed_scenario(int argc, char **argv, const Options &options);

// Keep long stress runs reproducible while avoiding accidental correlation
// between transaction shape, payload bytes, and issue scheduling.  The
// derived seeds are part of the scenario contract: changing one stream does
// not silently perturb all other dimensions.
struct StressRandom {
    static std::uint64_t splitmix64(std::uint64_t value)
    {
        value += 0x9e3779b97f4a7c15ULL;
        value = (value ^ (value >> 30)) * 0xbf58476d1ce4e5b9ULL;
        value = (value ^ (value >> 27)) * 0x94d049bb133111ebULL;
        return value ^ (value >> 31);
    }

    explicit StressRandom(std::uint64_t seed)
        : traffic(splitmix64(seed ^ 0x243f6a8885a308d3ULL)),
          shape(splitmix64(seed ^ 0x13198a2e03707344ULL)),
          payload(splitmix64(seed ^ 0xa4093822299f31d0ULL)),
          scheduler(splitmix64(seed ^ 0x082efa98ec4e6c89ULL))
    {}

    std::uint64_t operator()() { return traffic(); }
    std::uint64_t next_shape() { return shape(); }
    std::uint64_t next_payload() { return payload(); }
    std::uint64_t next_schedule() { return scheduler(); }

    std::mt19937_64 traffic;
    std::mt19937_64 shape;
    std::mt19937_64 payload;
    std::mt19937_64 scheduler;
};

inline std::uint64_t parse_u64(std::string_view text, const char *option)
{
    std::size_t consumed = 0;
    const std::string copy(text);
    const std::uint64_t value = std::stoull(copy, &consumed, 0);
    if (consumed != copy.size()) {
        throw std::invalid_argument(std::string("invalid value for ") + option);
    }
    return value;
}

#include "../random/constraints.inc"

struct TranslationContext {
    unsigned regime = RandomConstraints::translation_bare;
    unsigned stage1_mode = 0;
    unsigned vs_mode = 0;
    unsigned g_mode = 0;

    bool operator==(const TranslationContext &other) const
    {
        return regime == other.regime && stage1_mode == other.stage1_mode &&
            vs_mode == other.vs_mode && g_mode == other.g_mode;
    }

    bool operator!=(const TranslationContext &other) const
    {
        return !(*this == other);
    }
};

struct TranslationLeafTopology {
    bool stage1_napot = false;
    bool vs_napot = false;
    bool g_napot = false;

    unsigned nested_index() const
    {
        return static_cast<unsigned>(vs_napot) * 2U +
            static_cast<unsigned>(g_napot);
    }
};

#include "../random/coverage.inc"

} // namespace memblock::scenarios
