#pragma once

#include "shared.hpp"
#include <span>

namespace memblock::scenarios {
struct ScenarioEntry {
    std::string_view name;
    int (*run)(int argc, char **argv, const Options &options);
};

std::span<const ScenarioEntry> scenario_catalog();
} // namespace memblock::scenarios
