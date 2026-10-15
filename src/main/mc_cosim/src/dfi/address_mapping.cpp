#include "dfi/address_mapping.h"

#include <array>
#include <charconv>
#include <cctype>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <string_view>

namespace dfi {
namespace {

constexpr uint8_t kCachelineShift = 6U;
constexpr std::array<uint8_t, 6> kFieldWidths = {1U, 1U, 18U, 7U, 2U, 2U};
constexpr std::array<const char*, 6> kFieldNames = {
    "channel", "rank", "row", "col", "bank", "bg",
};

struct mapping_entry {
    address_field field;
    uint8_t lsb;
    uint8_t width;
};

struct parsed_mapping {
    std::array<mapping_entry, address_mapping_config::kMaxOps> entries{};
    uint8_t count = 0U;
};

std::string_view trim(std::string_view text) {
    while (!text.empty() && std::isspace(static_cast<unsigned char>(text.front())) != 0) {
        text.remove_prefix(1U);
    }
    while (!text.empty() && std::isspace(static_cast<unsigned char>(text.back())) != 0) {
        text.remove_suffix(1U);
    }
    return text;
}

[[noreturn]] void config_error(const std::string& path, uint64_t line, std::string_view message) {
    throw std::runtime_error(path + ':' + std::to_string(line) + ": " + std::string(message));
}

uint64_t parse_unsigned(std::string_view text, const std::string& path, uint64_t line) {
    text = trim(text);
    unsigned base = 10U;
    if (text.size() > 2U && text[0] == '0' && (text[1] == 'x' || text[1] == 'X')) {
        text.remove_prefix(2U);
        base = 16U;
    }
    uint64_t value = 0U;
    const auto result = std::from_chars(text.data(), text.data() + text.size(), value, base);
    if (result.ec != std::errc() || result.ptr != text.data() + text.size()) {
        config_error(path, line, "invalid unsigned integer");
    }
    return value;
}

uint64_t parse_size(std::string_view text, const std::string& path, uint64_t line) {
    text = trim(text);
    uint8_t shift = 0U;
    if (!text.empty() && std::isalpha(static_cast<unsigned char>(text.back())) != 0) {
        if (text.back() == 'B' || text.back() == 'b') {
            text.remove_suffix(1U);
        }
        if (text.empty()) {
            config_error(path, line, "invalid memory size");
        }
        const char suffix = static_cast<char>(std::tolower(static_cast<unsigned char>(text.back())));
        if (suffix == 'k' || suffix == 'm' || suffix == 'g') {
            shift = suffix == 'k' ? 10U : (suffix == 'm' ? 20U : 30U);
            text.remove_suffix(1U);
        }
    }
    const uint64_t value = parse_unsigned(text, path, line);
    if (shift != 0U && value > (~0ULL >> shift)) {
        config_error(path, line, "memory size overflows uint64");
    }
    return value << shift;
}

uint8_t field_index(address_field field) {
    return static_cast<uint8_t>(field);
}

address_field parse_field(std::string_view name, const std::string& path, uint64_t line) {
    name = trim(name);
    for (uint8_t i = 0U; i < kFieldNames.size(); ++i) {
        if (name == kFieldNames[i]) {
            return static_cast<address_field>(i);
        }
    }
    config_error(path, line, "unknown address field '" + std::string(name) + "'");
}

mapping_entry parse_entry(std::string_view token, const std::string& path, uint64_t line) {
    token = trim(token);
    const size_t open = token.find('[');
    if (open == std::string_view::npos || token.empty() || token.back() != ']') {
        config_error(path, line, "mapping field must use name[msb:lsb] syntax");
    }

    const address_field field = parse_field(token.substr(0U, open), path, line);
    const std::string_view range = trim(token.substr(open + 1U, token.size() - open - 2U));
    const size_t colon = range.find(':');
    const uint64_t msb = parse_unsigned(range.substr(0U, colon), path, line);
    const uint64_t lsb = colon == std::string_view::npos
                             ? msb
                             : parse_unsigned(range.substr(colon + 1U), path, line);
    const uint8_t index = field_index(field);
    if (msb < lsb || msb >= kFieldWidths[index]) {
        config_error(path, line, "address field range is out of bounds");
    }
    return {field, static_cast<uint8_t>(lsb), static_cast<uint8_t>(msb - lsb + 1U)};
}

parsed_mapping parse_order(std::string_view value, const std::string& path, uint64_t line) {
    parsed_mapping mapping{};
    while (!value.empty()) {
        const size_t comma = value.find(',');
        const std::string_view token = trim(value.substr(0U, comma));
        if (token.empty() || mapping.count == mapping.entries.size()) {
            config_error(path, line, token.empty() ? "empty address mapping field"
                                                   : "too many address mapping fields");
        }
        mapping.entries[mapping.count++] = parse_entry(token, path, line);
        if (comma == std::string_view::npos) {
            break;
        }
        value.remove_prefix(comma + 1U);
    }
    if (mapping.count == 0U) {
        config_error(path, line, "empty address mapping");
    }
    return mapping;
}

parsed_mapping default_mapping() {
    parsed_mapping mapping{};
    mapping.entries = {{
        {address_field::kRow, 4U, 13U},
        {address_field::kBank, 0U, 2U},
        {address_field::kBg, 1U, 1U},
        {address_field::kRow, 0U, 4U},
        {address_field::kCol, 0U, 7U},
        {address_field::kChannel, 0U, 1U},
    }};
    mapping.count = 6U;
    return mapping;
}

address_mapping_config compile_mapping(const parsed_mapping& mapping, uint64_t ram_size_bytes) {
    address_mapping_config config{};
    config.ram_size_bytes = ram_size_bytes;

    std::array<uint64_t, 6> used_bits{};
    uint8_t packed_bits = 0U;
    for (uint8_t i = 0U; i < mapping.count; ++i) {
        const mapping_entry& entry = mapping.entries[i];
        const uint8_t index = field_index(entry.field);
        const uint64_t mask = (1ULL << entry.width) - 1ULL;
        const uint64_t source_mask = mask << entry.lsb;
        if ((used_bits[index] & source_mask) != 0U) {
            throw std::runtime_error("address mapping overlaps field " +
                                     std::string(kFieldNames[index]));
        }
        used_bits[index] |= source_mask;
        if (packed_bits > 58U - entry.width) {
            throw std::runtime_error("address mapping exceeds uint64 byte address width");
        }
        packed_bits += entry.width;
    }

    uint8_t destination = packed_bits;
    for (uint8_t i = 0U; i < mapping.count; ++i) {
        const mapping_entry& entry = mapping.entries[i];
        destination -= entry.width;
        config.ops[i] = {
            (1ULL << entry.width) - 1ULL,
            field_index(entry.field),
            entry.lsb,
            destination,
        };
    }
    config.op_count = mapping.count;

    if (packed_bits < 64U - kCachelineShift) {
        const uint64_t address_space_bytes = 1ULL << (packed_bits + kCachelineShift);
        if (address_space_bytes > ram_size_bytes) {
            std::cerr << "[DDR4] warning: address mapping covers " << address_space_bytes
                      << " bytes, larger than virtual RAM " << ram_size_bytes << " bytes\n";
        }
    }

    std::cout << "[DDR4] address mapping high-to-low:";
    for (uint8_t i = 0U; i < mapping.count; ++i) {
        const mapping_entry& entry = mapping.entries[i];
        const uint8_t msb = entry.lsb + entry.width - 1U;
        std::cout << ' ' << kFieldNames[field_index(entry.field)] << '['
                  << static_cast<unsigned>(msb);
        if (entry.width != 1U) {
            std::cout << ':' << static_cast<unsigned>(entry.lsb);
        }
        std::cout << ']';
    }
    std::cout << " (" << static_cast<unsigned>(packed_bits)
              << " cacheline bits)\n";
    return config;
}

}  // namespace

address_mapping_config load_address_mapping_config(uint64_t default_ram_size_bytes) {
    const char* size_env = std::getenv("MC_DFI_RAM_SIZE");
    const bool size_overridden = size_env != nullptr && size_env[0] != '\0';
    uint64_t ram_size = size_overridden
                            ? parse_size(size_env, "MC_DFI_RAM_SIZE", 0U)
                            : default_ram_size_bytes;

    const char* path_env = std::getenv("MC_ADDRESS_MAPPING");
    const bool path_overridden = path_env != nullptr && path_env[0] != '\0';
    const std::string path = path_overridden ? path_env : "address_mapping_baiyang_dual.ini";
    std::ifstream file(path);
    if (!file.is_open()) {
        if (path_overridden) {
            throw std::runtime_error("cannot open MC_ADDRESS_MAPPING file '" + path + "'");
        }
        return compile_mapping(default_mapping(), ram_size);
    }

    std::string section;
    std::string line_text;
    parsed_mapping mapping{};
    bool have_order = false;
    uint64_t line_number = 0U;
    while (std::getline(file, line_text)) {
        ++line_number;
        const size_t comment = line_text.find_first_of("#;");
        const std::string_view line = trim(std::string_view(line_text).substr(0U, comment));
        if (line.empty()) {
            continue;
        }
        if (line.front() == '[' && line.back() == ']') {
            section = std::string(trim(line.substr(1U, line.size() - 2U)));
            if (section != "ram" && section != "address") {
                config_error(path, line_number, "unknown section '" + section + "'");
            }
            continue;
        }

        const size_t equal = line.find('=');
        if (equal == std::string_view::npos) {
            config_error(path, line_number, "expected key = value");
        }
        const std::string_view key = trim(line.substr(0U, equal));
        const std::string_view value = trim(line.substr(equal + 1U));
        if (section == "ram" && key == "size") {
            if (!size_overridden) {
                ram_size = parse_size(value, path, line_number);
            }
        } else if (section == "address" && key == "order") {
            if (have_order) {
                config_error(path, line_number, "duplicate address order");
            }
            mapping = parse_order(value, path, line_number);
            have_order = true;
        } else {
            config_error(path, line_number, "unknown key for current section");
        }
    }
    if (!have_order) {
        throw std::runtime_error(path + ": missing [address] order");
    }
    return compile_mapping(mapping, ram_size);
}

}  // namespace dfi
