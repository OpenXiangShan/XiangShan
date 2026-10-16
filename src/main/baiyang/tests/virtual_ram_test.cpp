#include "dfi/virtual_ram.h"

#include <array>
#include <cstdint>
#include <iostream>
#include <stdexcept>

namespace {

struct backend {
    std::array<uint32_t, 16> words{};
    uint64_t last_offset = 0U;
    uint32_t last_mask = 0U;
};

bool read(void* context, uint64_t offset, uint32_t* data) {
    auto& memory = *static_cast<backend*>(context);
    memory.last_offset = offset;
    for (size_t i = 0U; i < 8U; ++i) {
        data[i] = memory.words[offset / sizeof(uint32_t) + i];
    }
    return true;
}

bool write(void* context, uint64_t offset, const uint32_t* data, uint32_t mask) {
    auto& memory = *static_cast<backend*>(context);
    memory.last_offset = offset;
    memory.last_mask = mask;
    for (size_t i = 0U; i < 8U; ++i) {
        memory.words[offset / sizeof(uint32_t) + i] = data[i];
    }
    return true;
}

void check(bool condition, const char* message) {
    if (!condition) {
        throw std::runtime_error(message);
    }
}

}  // namespace

int main() {
    try {
        constexpr uint64_t base = 0x80000000ULL;
        backend memory;
        dfi::virtual_ram ram(64U);
        check(!ram.ready(), "unbound RAM is ready");
        check(ram.bind_external(base, 64U, &memory, read, write), "bind failed");

        std::array<uint32_t, 8> input{};
        for (size_t i = 0U; i < input.size(); ++i) {
            input[i] = 0x100U + static_cast<uint32_t>(i);
        }
        check(ram.write_beat(base + 32U, input.data(), 0x55aa55aaU), "write failed");
        check(memory.last_offset == 32U, "write offset mismatch");
        check(memory.last_mask == 0x55aa55aaU, "write mask mismatch");

        std::array<uint32_t, 8> output{};
        check(ram.read_beat(base + 32U, output.data()), "read failed");
        check(output == input, "read data mismatch");
        check(!ram.read_beat(base + 33U, output.data()), "out-of-range beat was accepted");
        check(!ram.write_beat(base - 1U, input.data(), ~0U), "below-base write was accepted");
        std::cout << "virtual_ram_test: PASS\n";
        return 0;
    } catch (const std::exception& error) {
        std::cerr << "virtual_ram_test: FAIL: " << error.what() << '\n';
        return 1;
    }
}
