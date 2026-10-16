#include "dfi/address_mapping.h"

#include <array>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <random>
#include <stdexcept>
#include <string>

namespace {

using dfi::address_mapping_config;
using dfi::dfi_address;

uint64_t field_value(uint8_t source, const dfi_address& address) {
    switch (source) {
        case 0U: return address.channel & 0x1U;
        case 1U: return address.rank & 0x1U;
        case 2U: return address.row & 0x3ffffU;
        case 3U: return (address.col >> 3U) & 0x7fU;
        case 4U: return address.bank & 0x3U;
        case 5U: return (address.bank >> 2U) & 0x3U;
        default: throw std::runtime_error("invalid mapping source");
    }
}

uint64_t reference_address(const address_mapping_config& config, const dfi_address& address) {
    uint64_t packed = 0U;
    for (uint8_t i = 0U; i < config.op_count; ++i) {
        const auto& op = config.ops[i];
        packed <<= op.mask == 0x1U ? 1U : static_cast<uint8_t>(__builtin_popcountll(op.mask));
        packed |= (field_value(op.source, address) >> op.source_shift) & op.mask;
    }
    return (packed << 6U) | ((address.col & 0x1U) << 5U);
}

void check(bool condition, const std::string& message) {
    if (!condition) {
        throw std::runtime_error(message);
    }
}

address_mapping_config load(const std::string& path) {
    setenv("MC_ADDRESS_MAPPING", path.c_str(), 1);
    unsetenv("MC_DFI_RAM_SIZE");
    return dfi::load_address_mapping_config(16ULL << 30U);
}

void test_mapping(const std::string& path, uint64_t expected_ram_size = 16ULL << 30U) {
    const address_mapping_config config = load(path);
    check(config.ram_size_bytes == expected_ram_size, path + ": wrong RAM size");

    std::mt19937_64 random(0x4d4353494dULL);
    for (uint32_t i = 0U; i < 100000U; ++i) {
        const dfi_address address{
            static_cast<uint8_t>(random()),
            static_cast<uint8_t>(random()),
            static_cast<uint8_t>(random()),
            static_cast<uint32_t>(random()),
            static_cast<uint32_t>(random()),
        };
        const uint64_t actual = dfi::compose_mapped_byte_address(config, address);
        const uint64_t expected = reference_address(config, address);
        check(actual == expected, path + ": random differential mismatch");
    }

    const dfi_address first{0U, 0U, 0U, 0U, 0U};
    dfi_address second = first;
    second.col = 1U;
    check(dfi::compose_mapped_byte_address(config, second) -
              dfi::compose_mapped_byte_address(config, first) ==
              32U,
          path + ": DFI beat offset is not 32 bytes");
}

void write_file(const std::string& path, const std::string& contents) {
    std::ofstream file(path);
    file << contents;
    check(file.good(), "failed to write temporary mapping");
}

void expect_failure(const std::string& name, const std::string& contents) {
    const std::string path = "/tmp/address_mapping_" + name + ".ini";
    write_file(path, contents);
    bool failed = false;
    try {
        static_cast<void>(load(path));
    } catch (const std::exception&) {
        failed = true;
    }
    check(failed, name + ": invalid mapping was accepted");
}

}  // namespace

int main() {
    try {
        const std::array<const char*, 5> mappings = {
            "src/main/baiyang/address_mapping_baiyang_dual.ini",
            "src/main/baiyang/address_mapping_bank_col_bg.ini",
            "src/main/baiyang/address_mapping_bank_col_bg_channel.ini",
            "src/main/baiyang/address_mapping_bank_col_bg_noch.ini",
            "src/main/baiyang/address_mapping_banklow.ini",
        };
        for (const char* path : mappings) {
            test_mapping(path);
        }

        const std::string split_path = "/tmp/address_mapping_split.ini";
        write_file(split_path,
                   "[ram]\nsize = 32G\n[address]\n"
                   "order = row[17:11], bank[1:0], row[10:0], col[6:0], bg[1:0]\n");
        test_mapping(split_path, 32ULL << 30U);

        setenv("MC_ADDRESS_MAPPING", mappings.front(), 1);
        setenv("MC_DFI_RAM_SIZE", "8G", 1);
        check(dfi::load_address_mapping_config(16ULL << 30U).ram_size_bytes == (8ULL << 30U),
              "MC_DFI_RAM_SIZE did not override the INI size");
        unsetenv("MC_DFI_RAM_SIZE");

        expect_failure("unknown_field",
                       "[ram]\nsize = 16G\n[address]\norder = row[15:0], typo[0]\n");
        expect_failure("overlap",
                       "[ram]\nsize = 16G\n[address]\norder = row[15:0], row[0]\n");
        expect_failure("bad_range",
                       "[ram]\nsize = 16G\n[address]\norder = bank[2:0]\n");
        expect_failure("duplicate",
                       "[address]\norder = row[15:0]\norder = bank[1:0]\n");
        expect_failure("missing_order", "[ram]\nsize = 16G\n");

        setenv("MC_ADDRESS_MAPPING", "/tmp/address_mapping_missing.ini", 1);
        bool missing_failed = false;
        try {
            static_cast<void>(dfi::load_address_mapping_config(16ULL << 30U));
        } catch (const std::exception&) {
            missing_failed = true;
        }
        check(missing_failed, "explicit missing file was accepted");
        std::cout << "address_mapping_test: PASS\n";
        return 0;
    } catch (const std::exception& error) {
        std::cerr << "address_mapping_test: FAIL: " << error.what() << '\n';
        return 1;
    }
}
