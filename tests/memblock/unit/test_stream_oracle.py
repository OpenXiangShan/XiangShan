#!/usr/bin/env python3
"""Compile and exercise the production architectural-prefix byte overlay."""
from __future__ import annotations

import hashlib
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

CPP = r'''
#include "scenarios/mixed/stream_oracle.hpp"
#include <iostream>
#include <string>
using namespace memblock;
using memblock::scenarios::mixed::StreamByteOracle;

static void require(bool value, const char *why) {
    if (!value) throw std::runtime_error(why);
}
template<class Action> static void reject(Action action, const char *why) {
    bool rejected = false;
    try { action(); } catch (const std::exception &) { rejected = true; }
    require(rejected, why);
}
// Expected payloads use only explicitly enumerated bytes and integer masks.
// No expected value calls a production load, address, or extension helper.
static std::uint64_t scalar(const std::vector<unsigned char> &bytes,
    unsigned offset, unsigned op, bool fp = false) {
    const unsigned width = 1U << (op % 4);
    std::uint64_t value = 0;
    for (unsigned byte = 0; byte < width; ++byte)
        value |= std::uint64_t(bytes.at(offset + byte)) << (8 * byte);
    if (width == 8) return value;
    const auto upper = ~((std::uint64_t{1} << (width * 8)) - 1);
    if (fp || (op < 4 && (bytes.at(offset + width - 1) & 0x80))) value |= upper;
    return value;
}
static LoadTransaction load(std::uint64_t address, unsigned op = 3, bool fp = false) {
    LoadTransaction t;
    t.address = address; t.op = static_cast<LoadOp>(op);
    t.fp_wen = fp; t.rf_wen = !fp;
    return t;
}
static StoreTransaction store(std::uint64_t address, unsigned width,
    std::uint64_t data) {
    StoreTransaction t;
    t.address = address; t.data = data;
    switch (width) {
    case 1: t.op = StoreOp::sb; break;
    case 2: t.op = StoreOp::sh; break;
    case 4: t.op = StoreOp::sw; break;
    case 8: t.op = StoreOp::sd; break;
    default: throw std::logic_error("fixture store width");
    }
    return t;
}
static void apply(std::vector<unsigned char> &bytes, unsigned offset,
    unsigned width, std::uint64_t data) {
    for (unsigned byte = 0; byte < width; ++byte)
        bytes.at(offset + byte) = static_cast<unsigned char>(data >> (8 * byte));
}
static void unchanged(const SparseMemory &memory, std::uint64_t address,
    const std::vector<unsigned char> &bytes, const char *why) {
    for (unsigned byte = 0; byte < bytes.size(); ++byte)
        require(memory.read_byte(address + byte) == bytes[byte], why);
}

static void scalar_prefix() {
    constexpr std::uint64_t address = 0x80001000;
    SparseMemory bus;
    SparseMemory base(&bus);
    std::vector<unsigned char> bytes(64);
    for (unsigned byte = 0; byte < bytes.size(); ++byte) {
        bytes[byte] = static_cast<unsigned char>(0x79 + byte * 31);
        base.write_byte(address + byte, bytes[byte]);
    }
    const auto initial = bytes;
    StreamByteOracle oracle(base);
    unsigned cases = 0;
    auto check = [&] {
        for (unsigned op = 0; op < 7; ++op) {
            for (unsigned offset = 0; offset < 32; ++offset) {
                require(oracle.scalar_load(load(address + offset, op)) ==
                    scalar(bytes, offset, op), "integer prefix/width/extension");
                ++cases;
            }
        }
        for (unsigned op : {1U, 2U, 3U}) {
            for (unsigned offset = 0; offset < 32; ++offset) {
                require(oracle.scalar_load(load(address + offset, op, true)) ==
                    scalar(bytes, offset, op, true), "FP prefix/width/boxing");
                ++cases;
            }
        }
    };
    check();
    const auto frozen = oracle.scalar_load(load(address + 8));
    for (const auto &[offset, width, data] :
        std::array<std::tuple<unsigned, unsigned, std::uint64_t>, 5>{{
            {8, 8, 0x8877665544332211ULL}, {10, 2, 0xbeef},
            {12, 1, 0x80}, {6, 4, 0x12345678}, {9, 8, 0xfedcba9876543210ULL}}}) {
        oracle.store(store(address + offset, width, data));
        apply(bytes, offset, width, data);
        check();
    }
    require(frozen == scalar(initial, 8, 3), "saved admission result changed");
    require(frozen != oracle.scalar_load(load(address + 8)), "younger writer not exercised");
    unchanged(base, address, initial, "overlay changed base");
    unchanged(bus, address, initial, "overlay changed mirrored bus");
    // Later commits to the reference must not undo a younger predicted writer.
    for (unsigned byte = 8; byte < 16; ++byte)
        base.write_reference_byte(address + byte, 0x11 + byte);
    check();
    unchanged(bus, address, initial, "reference commit escaped to bus");
    std::cout << "scalar_prefix_cases=" << cases << '\n';
}

static void lazy_and_aliases() {
    SparseMemory bus;
    SparseMemory base(&bus);
    base.define_repeating_incrementing(0x80000000, 1ULL << 32, 256, 0x20);
    StreamByteOracle oracle(base);
    const auto first = oracle.scalar_load(load(0x80000001, 4));
    require(first == 0x21, "lazy baseline byte");
    base.write_reference_byte(0x80000001, 0x91);
    base.write_reference_byte(0x80000002, 0x92);
    require(oracle.scalar_load(load(0x80000001, 4)) == 0x21,
        "materialized byte was refreshed from commit");
    require(oracle.scalar_load(load(0x80000002, 4)) == 0x92,
        "unseen byte was captured eagerly");
    require(bus.read_byte(0x80000001) == 0x21 && bus.read_byte(0x80000002) == 0x22,
        "private materialization changed mirror");
    auto mapped_store = store(0x1004, 4, 0x87654321);
    mapped_store.oracle_address = 0x80000004;
    oracle.store(mapped_store);
    require(oracle.scalar_load(load(0x80000004, 6)) == 0x87654321,
        "physical aliases did not share overlay");
    auto mapped_load = load(0x9004, 6);
    mapped_load.oracle_address = 0x80000004;
    require(oracle.scalar_load(mapped_load) == 0x87654321, "contiguous load alias");
    require(base.read_byte(0x80000004) == 0x24 && bus.read_byte(0x80000004) == 0x24,
        "aliased store changed underlying memory");
}

static void mapped_splits() {
    constexpr std::uint64_t va = 0x20000000;
    constexpr std::uint64_t pa0 = 0x81000000, pa1 = 0x93000000;
    auto pages = std::make_shared<const ReferencePageMap>(va,
        std::vector<std::uint64_t>{pa0, pa1});
    SparseMemory bus;
    SparseMemory base(&bus);
    std::vector<unsigned char> bytes(32);
    for (unsigned byte = 0; byte < bytes.size(); ++byte) {
        bytes[byte] = 0x40 + byte;
        const auto pa = byte < 8 ? pa0 + 0xff8 + byte : pa1 + byte - 8;
        base.write_byte(pa, bytes[byte]);
    }
    StreamByteOracle oracle(base);
    auto older = store(va + 0xffc, 8, 0x8877665544332211ULL);
    older.reference_pages = pages;
    oracle.store(older);
    apply(bytes, 4, 8, older.data);
    auto t = load(va + 0xffc);
    t.reference_pages = pages;
    require(oracle.scalar_load(t) == scalar(bytes, 4, 3), "split scalar byte mapping");
    VectorMemoryTransaction v;
    v.address = va + 0xff8; v.reference_pages = pages;
    v.data.fill(0xb5);
    const auto result = oracle.vector_load(v);
    for (unsigned byte = 0; byte < 16; ++byte)
        require(result[byte] == bytes[byte], "split vector byte mapping");
    auto vs = v;
    vs.store = true; vs.eew = 3; vs.vl = 2; vs.vstart = 1;
    for (unsigned byte = 0; byte < 16; ++byte) vs.data[byte] = 0xd0 + byte;
    oracle.store(vs);
    for (unsigned byte = 8; byte < 16; ++byte) bytes[byte] = vs.data[byte];
    const auto overlaid = oracle.vector_load(v);
    for (unsigned byte = 0; byte < 16; ++byte)
        require(overlaid[byte] == bytes[byte], "mapped vector store bytes");
    // One active 64-bit element itself crosses the discontinuous page edge.
    vs.address = va + 0xffc; vs.vstart = 0; vs.vl = 1;
    oracle.store(vs);
    for (unsigned byte = 0; byte < 8; ++byte) bytes[4 + byte] = vs.data[byte];
    require(oracle.scalar_load(t) == scalar(bytes, 4, 3), "split vector element store");
    for (unsigned byte = 0; byte < 32; ++byte) {
        const auto pa = byte < 8 ? pa0 + 0xff8 + byte : pa1 + byte - 8;
        require(base.read_byte(pa) == 0x40 + byte && bus.read_byte(pa) == 0x40 + byte,
            "mapped overlay changed base or mirror");
    }
}

static void vector_activity() {
    constexpr std::uint64_t address = 0x80002080;
    unsigned cases = 0;
    for (unsigned mode = 0; mode < 4; ++mode)
      for (unsigned eew = 0; eew < 4; ++eew)
       for (unsigned start : {0U, 1U, 4U, 16U})
        for (unsigned vl : {0U, 3U, 8U, 16U}) {
            SparseMemory base;
            std::vector<unsigned char> bytes(512);
            for (unsigned byte = 0; byte < bytes.size(); ++byte) {
                bytes[byte] = static_cast<unsigned char>(byte * 23 + 0x61);
                base.write_reference_byte(address - 256 + byte, bytes[byte]);
            }
            StreamByteOracle oracle(base);
            const unsigned width = 1U << eew;
            const unsigned count = 16U / width;
            VectorMemoryTransaction v;
            v.address = address;
            v.addressing = static_cast<VectorAddressingMode>(mode);
            v.eew = eew; v.vl = vl; v.vstart = start;
            v.vm = false; v.mask_bits = 0x6b5a; v.vma = v.vta = true;
            v.stride = -11;
            v.flow_num = count;
            for (unsigned byte = 0; byte < 16; ++byte) v.data[byte] = 0xa0 + byte;
            for (unsigned element = 0; element < count; ++element)
                v.index[element * width] = element * 9;
            auto expected = v.data;
            auto expected_address = [&](unsigned element) {
                return mode == 0 ? 256 + element * width :
                    mode == 1 ? 256 - element * 11 : 256 + element * 9;
            };
            for (unsigned element = 0; element < count; ++element) {
                const bool active = element >= start && element < vl &&
                    ((0x6b5aU >> element) & 1U);
                if (!active) continue;
                for (unsigned byte = 0; byte < width; ++byte)
                    expected[element * width + byte] = bytes.at(expected_address(element) + byte);
            }
            const auto frozen = oracle.vector_load(v);
            require(frozen == expected, "vector active/mask/vstart/tail/no-op load");
            auto s = v;
            s.store = true;
            for (unsigned byte = 0; byte < 16; ++byte) s.data[byte] = 0xd0 + byte;
            oracle.store(s);
            for (unsigned element = 0; element < count; ++element) {
                const bool active = element >= start && element < vl &&
                    ((0x6b5aU >> element) & 1U);
                if (!active) continue;
                for (unsigned byte = 0; byte < width; ++byte)
                    bytes.at(expected_address(element) + byte) = s.data[element * width + byte];
            }
            for (unsigned byte = 0; byte < bytes.size(); ++byte)
                require(oracle.scalar_load(load(address - 256 + byte, 4)) == bytes[byte],
                    "vector store touched wrong/masked/prestart/tail bytes");
            require(frozen == expected, "frozen vector result changed after younger store");
            ++cases;
        }
    std::cout << "vector_activity_cases=" << cases << '\n';
}

static void indexed_chains() {
    constexpr std::uint64_t address = 0x80004000;
    unsigned cases = 0;
    for (unsigned index_eew : {1U, 2U, 3U})
      for (bool noop : {false, true}) {
        SparseMemory bus;
        SparseMemory base(&bus);
        std::vector<unsigned char> bytes(64);
        for (unsigned byte = 0; byte < bytes.size(); ++byte) {
            bytes[byte] = 0x40 + byte;
            base.write_byte(address + byte, bytes[byte]);
        }
        StreamByteOracle oracle(base);
        oracle.store(store(address + 2, 8, 0xf1e2d3c4b5a69788ULL));
        apply(bytes, 2, 8, 0xf1e2d3c4b5a69788ULL);
        VectorMemoryTransaction pending;
        pending.store = true; pending.address = address;
        pending.vstart = 4; pending.vl = 13;
        pending.vm = false; pending.mask_bits = 0x5555;
        for (unsigned byte = 0; byte < 16; ++byte) pending.data[byte] = 0xc0 + byte;
        oracle.store(pending);
        for (unsigned byte = 4; byte < 13; byte += 2) bytes[byte] = pending.data[byte];
        const unsigned count = 1U << index_eew;
        const unsigned flow = 16U / count;
        std::vector<VectorMemoryTransaction> uops(count);
        std::array<unsigned char, 16> source{};
        for (unsigned byte = 0; byte < 16; ++byte) source[byte] = 0xa0 + byte;
        for (unsigned index = 0; index < count; ++index) {
            auto &v = uops[index];
            v.address = address; v.addressing = VectorAddressingMode::indexed_unordered;
            v.eew = index_eew; v.vsew = 0; v.flow_num = flow; v.vuop_idx = index;
            v.rob = 7; v.pdest = 41 + index; v.last_uop = index + 1 == count;
            v.vm = false; v.mask_bits = 0x6b5a;
            v.vl = 13; v.vstart = noop ? 13 : 2; v.vma = v.vta = true;
            v.data = source;
            if (index) v.data.fill(0x5c); // Preparation must replace stale operands.
            for (unsigned element = 0; element < flow; ++element)
                v.index[element * (1U << index_eew)] = index * flow + element;
        }
        oracle.prepare_vector_load(uops);
        for (unsigned index = 0; index < count; ++index) {
            const auto &v = uops[index];
            require(v.data == source, "repeated-Vd input failed prefix producer chain");
            auto expected = source;
            for (unsigned element = index * flow; element < (index + 1) * flow; ++element) {
                if (element >= v.vstart && element < 13 && ((0x6b5aU >> element) & 1U))
                    expected[element] = bytes[element];
            }
            require(oracle.vector_load(v) == expected,
                "repeated-Vd raw load omitted pending scalar/vector bytes");
            source = expected;
            if (!noop) {
                for (unsigned element = 0; element < 16; ++element) {
                    if (element >= 13 || (element >= 2 && !((0x6b5aU >> element) & 1U)))
                        source[element] = 0xff;
                }
            }
            ++cases;
        }
        for (unsigned byte = 0; byte < 64; ++byte) {
            require(base.read_byte(address + byte) == 0x40 + byte &&
                bus.read_byte(address + byte) == 0x40 + byte,
                "chain construction changed base or bus");
        }
    }
    std::cout << "indexed_chain_uops=" << cases << '\n';
}

static void rejection_atomicity() {
    constexpr std::uint64_t address = 0x80005000;
    SparseMemory base;
    StreamByteOracle oracle(base);
    auto bad_store = store(address, 1, 0xab);
    for (StoreOp op : {StoreOp::cbo_zero, StoreOp::cbo_clean, StoreOp::hsvd,
            static_cast<StoreOp>(0xffff)}) {
        bad_store.op = op;
        reject([&] { oracle.store(bad_store); }, "unsupported scalar store accepted");
    }
    auto bad_load = load(address, 0, true);
    reject([&] { oracle.scalar_load(bad_load); }, "FP byte load accepted");
    bad_load = load(address, 1, true); bad_load.rf_wen = true;
    reject([&] { oracle.scalar_load(bad_load); }, "both scalar destinations accepted");
    for (LoadOp op : {LoadOp::hlvd, static_cast<LoadOp>(7), static_cast<LoadOp>(0xffff)}) {
        bad_load = load(address); bad_load.op = op;
        reject([&] { oracle.scalar_load(bad_load); }, "unsupported scalar load accepted");
    }
    VectorMemoryTransaction v;
    v.address = address; v.store = true;
    reject([&] { oracle.vector_load(v); }, "store accepted as vector load");
    v.store = false;
    reject([&] { oracle.store(v); }, "load accepted as vector store");
    auto invalid_width = v; invalid_width.eew = 4;
    reject([&] { oracle.vector_load(invalid_width); }, "unsupported vector width accepted");
    auto invalid_mode = v;
    invalid_mode.addressing = static_cast<VectorAddressingMode>(255);
    reject([&] { oracle.vector_load(invalid_mode); }, "unsupported vector addressing accepted");
    auto invalid_lmul = v; invalid_lmul.vlmul = 4;
    reject([&] { oracle.vector_load(invalid_lmul); }, "reserved LMUL accepted");
    std::vector<VectorMemoryTransaction> mixed{v, v};
    mixed.back().store = true;
    const auto old_data = mixed[0].data;
    reject([&] { oracle.prepare_vector_load(mixed); }, "mixed directions prepared");
    require(mixed[0].data == old_data, "rejected preparation changed operands");
    // Missing second-page mappings must fail before updating/reading any prefix.
    auto pages = std::make_shared<const ReferencePageMap>(0x1000,
        std::vector<std::uint64_t>{address});
    auto split_store = store(0x1ffc, 8, 0xaabbccddeeff1234ULL);
    split_store.reference_pages = pages;
    reject([&] { oracle.store(split_store); }, "unmapped scalar store accepted");
    auto split_load = load(0x1ffc); split_load.reference_pages = pages;
    reject([&] { oracle.scalar_load(split_load); }, "unmapped scalar load accepted");
    auto split_vector = v;
    split_vector.address = 0x1ffc; split_vector.reference_pages = pages;
    reject([&] { oracle.vector_load(split_vector); }, "unmapped vector load accepted");
    split_vector.store = true; split_vector.data.fill(0xbc);
    reject([&] { oracle.store(split_vector); }, "unmapped vector store accepted");
    std::vector<VectorMemoryTransaction> split_uops{v, split_vector};
    split_uops.back().store = false;
    reject([&] { oracle.prepare_vector_load(split_uops); }, "unmapped vector preparation accepted");
    // If any rejected operation materialized bytes, these new base values would
    // be hidden. If any wrote bytes, its payload would survive instead.
    for (unsigned byte = 0; byte < 16; ++byte) base.write_reference_byte(address + byte, 0x50 + byte);
    for (unsigned byte = 0; byte < 8; ++byte) base.write_reference_byte(address + 0xff8 + byte, 0x70 + byte);
    require(oracle.scalar_load(load(address)) == 0x5756555453525150ULL,
        "rejected input changed first-byte state");
    require(oracle.scalar_load(load(address + 0xff8)) == 0x7776757473727170ULL,
        "rejected split input changed mapped prefix");
}

int main(int argc, char **argv) {
    require(argc == 2, "fixture case required");
    const std::string test = argv[1];
    if (test == "scalar") scalar_prefix();
    else if (test == "lazy") lazy_and_aliases();
    else if (test == "mapped") mapped_splits();
    else if (test == "activity") vector_activity();
    else if (test == "chains") indexed_chains();
    else if (test == "rejections") rejection_atomicity();
    else throw std::runtime_error("unknown fixture case");
}
'''


class StreamOracleTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        sources = (
            ROOT / "cpp/environment/model.inc",
            ROOT / "cpp/scenarios/mixed/vector_operands.hpp",
            ROOT / "cpp/scenarios/mixed/stream_oracle.hpp",
        )
        cls.source_hashes = {path: hashlib.sha256(path.read_bytes()).hexdigest() for path in sources}

        def check_sources():
            after = {path: hashlib.sha256(path.read_bytes()).hexdigest() for path in sources}
            if cls.source_hashes != after:
                raise AssertionError("oracle production inputs changed during fixture execution")

        cls.addClassCleanup(check_sources)
        cls.workspace = tempfile.TemporaryDirectory(prefix="memblock-stream-oracle-")
        cls.addClassCleanup(cls.workspace.cleanup)
        source = Path(cls.workspace.name) / "fixture.cpp"
        source.write_text(CPP)
        cls.binary = Path(cls.workspace.name) / "fixture"
        built = subprocess.run(
            ["c++", "-std=c++20", "-O0", "-Wall", "-Wextra", "-I", str(ROOT / "cpp"),
             str(source), "-o", str(cls.binary)],
            capture_output=True, text=True, timeout=60,
        )
        if built.returncode:
            raise RuntimeError(built.stdout + built.stderr)

    def check_case(self, name):
        ran = subprocess.run([str(self.binary), name], capture_output=True, text=True, timeout=20)
        self.assertEqual(ran.returncode, 0, ran.stdout + ran.stderr)
        print(ran.stdout, end="")

    def test_scalar_older_prefix_overlap_width_extension_and_boxing(self):
        self.check_case("scalar")

    def test_materialization_and_physical_aliases_survive_reference_commits(self):
        self.check_case("lazy")

    def test_scalar_vector_and_individual_element_page_splits(self):
        self.check_case("mapped")

    def test_vector_modes_masks_prestart_tail_and_noop(self):
        self.check_case("activity")

    def test_repeated_vd_sources_use_pending_scalar_and_vector_bytes(self):
        self.check_case("chains")

    def test_rejected_operations_leave_bytes_and_operands_untouched(self):
        self.check_case("rejections")


if __name__ == "__main__":
    unittest.main()
