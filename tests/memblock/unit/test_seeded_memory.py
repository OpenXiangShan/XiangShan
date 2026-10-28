#!/usr/bin/env python3
"""Exercise lazy seeded initial data and production wrong-line detection."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]

CPP = r'''
#include "memblock_env.hpp"
namespace memblock {
#include "environment/scoreboards.inc"
}

using memblock::SparseMemory;
constexpr std::uint64_t base = 0x80200000ULL;
constexpr std::uint64_t seed = 0x0123456789abcdefULL;

void require(bool condition, const char *message)
{
    if (!condition) throw std::runtime_error(message);
}

template <typename Callback>
void require_invalid(Callback callback)
{
    try {
        callback();
    } catch (const std::invalid_argument &) {
        return;
    }
    throw std::runtime_error("invalid region definition was accepted");
}

void replay()
{
    SparseMemory first;
    SparseMemory second;
    constexpr std::uint64_t size = std::uint64_t{1} << 40;
    first.define_seeded_random(base, size, seed);
    second.define_seeded_random(base, size, seed);
    const std::array<std::uint64_t, 10> offsets{{
        0, 3, 8, 63, 64, 255, 256, 0x4000,
        std::uint64_t{1} << 32, std::uint64_t{1} << 36}};
    std::array<std::uint64_t, offsets.size()> words{};
    for (unsigned index = 0; index < offsets.size(); ++index) {
        words[index] = first.read_u64(base + offsets[index]);
    }
    for (unsigned index = offsets.size(); index-- > 0;) {
        require(second.read_u64(base + offsets[index]) == words[index],
            "replay depends on instance or read order");
        require(first.read_u64(base + offsets[index]) == words[index],
            "repeated read changed data");
    }
    require(first.read_u64(base) != first.read_u64(base + (std::uint64_t{1} << 32)),
        "high physical address bits do not affect the word");
    require(first.read_u64(base) != first.read_u64(base + (std::uint64_t{1} << 36)),
        "higher physical address bits do not affect the word");

    SparseMemory other_seed;
    SparseMemory high_seed;
    other_seed.define_seeded_random(base, size, seed + 1);
    high_seed.define_seeded_random(base, size, seed ^ (std::uint64_t{1} << 63));
    for (const auto offset : offsets) {
        require(first.read_u64(base + offset) != other_seed.read_u64(base + offset),
            "changing the seed did not change a sampled word");
        require(first.read_u64(base + offset) != high_seed.read_u64(base + offset),
            "high seed bit did not change a sampled word");
    }
}

void boundaries()
{
    SparseMemory whole;
    SparseMemory partial;
    whole.define_seeded_random(base, 0x1000, seed);
    partial.define_seeded_random(base + 3, 0x205, seed);
    require(partial.read_byte(base + 2) == 0, "region leaked below its start");
    require(partial.read_byte(base + 0x208) == 0, "region leaked above its end");
    for (std::uint64_t offset = 3; offset < 0x208; ++offset) {
        require(whole.read_byte(base + offset) == partial.read_byte(base + offset),
            "value depends on region start rather than absolute word address");
    }
    for (unsigned byte = 0; byte < 8; ++byte) {
        require(whole.read_byte(base + byte) ==
                static_cast<std::uint8_t>(whole.read_u64(base) >> (byte * 8)),
            "word byte order is inconsistent");
    }
    require_invalid([&] { partial.define_seeded_random(base, 0, seed); });
    require_invalid([&] {
        partial.define_seeded_random(~std::uint64_t{0} - 1, 3, seed);
    });
    SparseMemory last_byte;
    last_byte.define_seeded_random(~std::uint64_t{0}, 1, seed);
    (void)last_byte.read_byte(~std::uint64_t{0});
}

void overrides()
{
    SparseMemory reference;
    SparseMemory baseline;
    reference.write_reference_byte(base + 2, 0xa5);
    reference.define_seeded_random(base, 0x1000, seed);
    baseline.define_seeded_random(base, 0x1000, seed);
    require(reference.read_byte(base + 2) == 0xa5,
        "region replaced an earlier explicit byte");
    reference.write_reference_byte(base + 3, 0x5a);
    require(reference.read_byte(base + 3) == 0x5a,
        "explicit reference byte did not override lazy data");
    reference.write_u64(base + 7, 0x8877665544332211ULL);
    require(reference.read_u64(base + 7) == 0x8877665544332211ULL,
        "unaligned explicit word did not override lazy data");
    require(reference.read_byte(base + 6) == baseline.read_byte(base + 6) &&
            reference.read_byte(base + 15) == baseline.read_byte(base + 15),
        "explicit word changed neighboring bytes");

    SparseMemory changed_seed;
    changed_seed.define_seeded_random(base, 0x1000, seed + 1);
    reference.define_seeded_random(base, 0x1000, seed + 1);
    require(reference.read_u64(base + 16) == changed_seed.read_u64(base + 16),
        "last same-format definition did not win");
    require(reference.read_byte(base + 2) == 0xa5,
        "new region definition replaced an explicit byte");
}

void mirror()
{
    SparseMemory sink;
    SparseMemory bus(&sink);
    SparseMemory reference(&bus);
    reference.define_seeded_random(base, 0x1000, seed);
    require(reference.read_u64(base + 64) == bus.read_u64(base + 64),
        "initial region was not mirrored into bus memory");
    require(sink.read_u64(base + 64) == 0,
        "region definition propagated beyond the immediate mirror");
    const auto original = bus.read_byte(base + 1);
    reference.write_reference_byte(base + 1, original ^ 0xffU);
    require(bus.read_byte(base + 1) == original,
        "architectural reference update changed manager backing memory");
    const auto original_reference = reference.read_byte(base + 2);
    bus.write_reference_byte(base + 2, original_reference ^ 0xffU);
    require(reference.read_byte(base + 2) == original_reference,
        "manager byte update changed architectural reference memory");
    reference.write_byte(base + 3, 0x5a);
    require(bus.read_byte(base + 3) == 0x5a,
        "explicit setup byte failed to mirror");
    require(sink.read_byte(base + 3) == 0,
        "explicit setup byte propagated beyond the immediate mirror");
    reference.define_reference_seeded_random(base + 0x1000, 0x1000, seed + 1);
    require(bus.read_u64(base + 0x1000) == 0,
        "reference-only region definition changed bus memory");
}

void mixed_regions()
{
    SparseMemory seeded_first;
    seeded_first.define_seeded_random(base, 64, seed);
    require_invalid([&] {
        seeded_first.define_repeating_incrementing(base + 63, 64, 64, 0x41);
    });
    seeded_first.define_repeating_incrementing(base + 64, 64, 64, 0x41);
    require(seeded_first.read_byte(base + 64) == 0x41,
        "adjacent legacy region was changed");
    seeded_first.define_reference_repeating_incrementing(base + 64, 64, 64, 0x51);
    require(seeded_first.read_byte(base + 64) == 0x51,
        "legacy same-format replacement semantics changed");

    SparseMemory repeating_first;
    repeating_first.define_repeating_incrementing(base, 0x10000, 64, 0x41);
    require_invalid([&] { repeating_first.define_seeded_random(base + 63, 64, seed); });
    require_invalid([&] {
        repeating_first.define_reference_seeded_random(base, 64, seed);
    });
    require(repeating_first.read_byte(base) == 0x41 &&
            repeating_first.read_byte(base + 64) == 0x42 &&
            repeating_first.read_byte(base + 0x4000) == 0x41,
        "legacy repeating-region period/index semantics changed");

    SparseMemory bus;
    SparseMemory reference(&bus);
    bus.define_repeating_incrementing(base, 64, 64, 0x41);
    require_invalid([&] { reference.define_seeded_random(base, 64, seed); });
    require(reference.read_u64(base) == 0,
        "mirror collision left a partial seeded reference definition");
    bus.define_seeded_random(base + 64, 64, seed);
    require_invalid([&] {
        reference.define_repeating_incrementing(base + 64, 64, 64, 0x51);
    });
    require(reference.read_u64(base + 64) == 0,
        "mirror collision left a partial legacy reference definition");
}

bool scoreboard_accepts(const SparseMemory &memory,
    std::uint64_t requested_address, std::uint64_t returned_address)
{
    memblock::LoadTransaction load;
    load.address = requested_address;
    load.op = memblock::LoadOp::ld;
    load.rob = 3;
    load.pdest = 17;
    memblock::LoadScoreboard scoreboard;
    scoreboard.expect(load, memory.expected_load(load.address, load.op));
    memblock::generated::ScalarLoadWriteback response;
    response.valid = true;
    response.rf_wen = true;
    response.rob_value = load.rob;
    response.pdest = load.pdest;
    response.data = memory.read_u64(returned_address);
    scoreboard.observe(0, response);
    return scoreboard.error().empty() && scoreboard.done();
}

void wrong_line()
{
    SparseMemory eager_old;
    SparseMemory eager_new;
    eager_old.fill_incrementing(base, 0x10000, 0x43);
    eager_new.define_seeded_random(base, 0x10000, seed);
    require(scoreboard_accepts(eager_old, base, base + 256),
        "legacy eager wrong-line control should be invisible");
    require(scoreboard_accepts(eager_new, base, base),
        "seeded correct-address control was rejected");
    require(!scoreboard_accepts(eager_new, base, base + 256),
        "seeded data failed to expose an eager-pool wrong-line substitution");

    SparseMemory lazy_old;
    SparseMemory lazy_new;
    lazy_old.define_repeating_incrementing(base, 0x10000, 64, 0x41);
    lazy_new.define_seeded_random(base, 0x10000, seed);
    require(scoreboard_accepts(lazy_old, base, base + 0x4000),
        "legacy 256-period wrong-line control should be invisible");
    require(!scoreboard_accepts(lazy_new, base, base + 0x4000),
        "seeded data failed to expose a lazy-pool wrong-line substitution");
    require(scoreboard_accepts(lazy_old, base + 8, base + 8 * 64),
        "legacy period/offset alias control should be invisible");
    require(!scoreboard_accepts(lazy_new, base + 8, base + 8 * 64),
        "seeded data failed to expose a period/offset address substitution");
}

int main(int argc, char **argv)
{
    try {
        require(argc == 2, "one test case is required");
        const std::string name = argv[1];
        if (name == "replay") replay();
        else if (name == "boundaries") boundaries();
        else if (name == "overrides") overrides();
        else if (name == "mirror") mirror();
        else if (name == "mixed-regions") mixed_regions();
        else if (name == "wrong-line") wrong_line();
        else throw std::runtime_error("unknown test case");
        std::cout << name << " PASS\n";
        return 0;
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
'''


class SeededMemoryTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.workspace = tempfile.TemporaryDirectory(prefix="memblock-seeded-memory-")
        cls.addClassCleanup(cls.workspace.cleanup)
        root = Path(cls.workspace.name)
        source = root / "seeded_memory.cpp"
        cls.binary = root / "seeded_memory"
        source.write_text(CPP)
        subprocess.run(
            ["c++", "-std=c++20", "-O0", "-I", str(MEMBLOCK_ROOT / "cpp"),
             str(source), "-o", str(cls.binary)],
            check=True, capture_output=True, text=True, timeout=60,
        )

    def check_case(self, name: str) -> None:
        result = subprocess.run([str(self.binary), name], capture_output=True,
                                text=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_replay_read_order_seed_and_absolute_address(self) -> None:
        self.check_case("replay")

    def test_unaligned_region_boundaries_and_invalid_definitions(self) -> None:
        self.check_case("boundaries")

    def test_explicit_bytes_override_lazy_data(self) -> None:
        self.check_case("overrides")

    def test_initialization_mirroring_and_independent_reference_writes(self) -> None:
        self.check_case("mirror")

    def test_mixed_region_overlap_and_legacy_semantics(self) -> None:
        self.check_case("mixed-regions")

    def test_production_scoreboard_rejects_previously_invisible_wrong_lines(self) -> None:
        self.check_case("wrong-line")


if __name__ == "__main__":
    unittest.main()
