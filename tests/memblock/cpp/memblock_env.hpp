#pragma once

#include "generated_port_defaults.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <deque>
#include <iomanip>
#include <iostream>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace memblock {

#include "environment/model.inc"
#include "environment/dcache_agent.inc"
#include "environment/ptw_agent.inc"
#include "environment/uncache_agent.inc"
#include "environment/scoreboards.inc"
#include "environment/environment.inc"
} // namespace memblock
