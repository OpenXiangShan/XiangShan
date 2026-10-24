#pragma once

// Address-qualified transcript of architectural reference-memory updates and
// scalar load expectations. This is an opt-in localization aid for a failing
// seed; it never changes stimulus, DUT cycles, or the independent oracle.
//
// Enable with MEMBLOCK_TRACE_ADDR_LO and MEMBLOCK_TRACE_ADDR_HI (hex or
// decimal, half-open interval).

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <iomanip>
#include <sstream>
#include <string>

namespace memblock::debug_trace {

inline std::uint64_t env_address(const char *name, std::uint64_t fallback)
{
    const char *value = std::getenv(name);
    if (value == nullptr || value[0] == '\0') {
        return fallback;
    }
    char *end = nullptr;
    const std::uint64_t parsed = std::strtoull(value, &end, 0);
    return end != nullptr && *end == '\0' ? parsed : fallback;
}

inline bool enabled_for(std::uint64_t address, unsigned bytes)
{
    static const std::uint64_t low = env_address(
        "MEMBLOCK_TRACE_ADDR_LO", ~std::uint64_t{0});
    static const std::uint64_t high = env_address(
        "MEMBLOCK_TRACE_ADDR_HI", 0);
    static const bool enabled = low < high;
    if (!enabled || bytes == 0) {
        return false;
    }
    const std::uint64_t end = address + bytes;
    return address < high && low < end;
}

inline std::string hex_bytes(
    const std::uint8_t *bytes, unsigned count)
{
    std::ostringstream text;
    text << std::hex << std::setfill('0');
    for (unsigned index = 0; index < count; ++index) {
        text << std::setw(2) << static_cast<unsigned>(bytes[index]);
    }
    return text.str();
}

inline void emit(const std::string &text)
{
    std::fprintf(stderr, "MEMBLOCK_TRACE %s\n", text.c_str());
}

} // namespace memblock::debug_trace
