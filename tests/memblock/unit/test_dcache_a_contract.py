#!/usr/bin/env python3
"""Compile and exercise the production DCache A checker without a DUT."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]

DRIVER = r'''
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <optional>
#include <string>
#include <unordered_map>
namespace memblock {
#include "environment/dcache_a_contract.inc"
}
using memblock::DcacheARequest;
using memblock::dcache_a_protocol_error;
using memblock::dcache_faulting_load_request_error;
using memblock::dcache_is_cpu_demand_acquire;
using memblock::dcache_release_backpressure_observation_valid;
using memblock::dcache_suppressed_demand_error;

static unsigned checks = 0;
static void check(const DcacheARequest &request, const char *expected = nullptr) {
    const char *actual = dcache_a_protocol_error(request, 16, 17, 36);
    ++checks;
    if ((actual == nullptr) != (expected == nullptr) ||
        (actual != nullptr && std::strcmp(actual, expected) != 0)) {
        std::cerr << "check " << checks << ": expected "
                  << (expected ? expected : "valid") << ", got "
                  << (actual ? actual : "valid") << '\n';
        std::exit(1);
    }
}
static DcacheARequest acquire() {
    DcacheARequest r;
    r.opcode = 6;
    r.size = 6;
    r.source = 15;
    r.address = 0x80000680;
    // VA 0x12342680: alias=2 differs from PA[13:12]=0.
    r.virtual_address = 0x48d09a;
    r.alias = 2;
    r.request_source = 2;
    r.mask = 0xffffffffU;
    return r;
}
static void legal_acquires() {
    DcacheARequest r = acquire();
    check(r);
    for (unsigned source : {2U, 3U, 4U, 6U}) {
        r.request_source = source;
        for (unsigned hint : {0U, 1U}) {
            r.need_hint = hint;
            for (unsigned keyword : {0U, 1U}) {
                r.keyword = keyword;
                for (unsigned grow : {0U, 1U, 2U}) {
                    r.opcode = 6;
                    r.param = grow;
                    check(r);
                    if (grow != 0) {
                        r.opcode = 7;
                        check(r);
                    }
                }
            }
        }
    }
    r = acquire();
    r.request_source = 6;
    // Hardware prefetch's synthetic VA 0x2680 has no virtual tag.
    r.virtual_address = 0x9a;
    r.cycle = 123456789;
    r.index = 82948;
    check(r);
}
static void acquire_mutants() {
    const DcacheARequest good = acquire();
    auto r = good; r.size = 5;
    check(r, "invalid DCache Acquire size");
    r = good; r.address |= 1;
    check(r, "unaligned DCache Acquire address");
    r = good; r.source = 16;
    check(r, "invalid DCache Acquire source");
    r = good; r.param = 3;
    check(r, "invalid DCache Acquire grow param");
    r = good; r.opcode = 7;
    check(r, "invalid DCache Acquire grow param");
    r = good; r.mask = 0xfffffffeU;
    check(r, "invalid DCache Acquire mask");
    for (unsigned source : {0U, 1U, 5U, 7U, 12U, 31U, 255U}) {
        r = good; r.request_source = source;
        check(r, "invalid DCache Acquire request source");
    }
    r = good; r.alias = 1;
    check(r, "DCache Acquire alias disagrees with virtual address");
    r = good; r.virtual_address ^= 0x40;
    check(r, "DCache Acquire alias disagrees with virtual address");
    r = good; r.address ^= 0x40;
    check(r, "DCache Acquire page offset disagrees with virtual address");
    r = good; r.virtual_address ^= 1;
    check(r, "DCache Acquire page offset disagrees with virtual address");
    r = good; r.corrupt = true;
    check(r, "DCache TileLink A request is corrupt");
}
static void cmo_records() {
    std::uint64_t random = 0xfedcba9876543210ULL;
    for (unsigned opcode : {12U, 13U, 14U}) {
        for (unsigned trial = 0; trial != 32; ++trial) {
            random ^= random << 13;
            random ^= random >> 7;
            random ^= random << 17;
            DcacheARequest r;
            r.opcode = opcode; r.size = 6; r.source = 17;
            r.address = 0x80002340;
            r.param = random & 7;
            r.request_source = (random >> 3) & 31;
            r.virtual_address = (random >> 8) & 0xfffffffffffULL;
            r.alias = (random >> 52) & 3;
            r.need_hint = (random >> 54) & 1;
            r.keyword = (random >> 55) & 1;
            r.mask = static_cast<std::uint32_t>(random);
            check(r);
        }
    }
    DcacheARequest r;
    r.opcode = 12; r.size = 6; r.source = 17; r.address = 0x80002340;
    auto bad = r; bad.size = 5;
    check(bad, "malformed DCache CMO request identity");
    bad = r; bad.source = 16;
    check(bad, "malformed DCache CMO request identity");
    bad = r; bad.address |= 1;
    check(bad, "malformed DCache CMO request identity");
    bad = r; bad.corrupt = true;
    check(bad, "DCache TileLink A request is corrupt");
}
static void get_records() {
    struct Record { std::uint8_t size; std::uint64_t address; std::uint32_t mask; };
    // Explicit byte masks, independent of the production MaskGen expression.
    const Record records[] = {
        {0, 0x80000000, 0x00000001}, {0, 0x8000001f, 0x80000000},
        {1, 0x8000001e, 0xc0000000}, {2, 0x8000000c, 0x0000f000},
        {3, 0x80000018, 0xff000000}, {4, 0x80000010, 0xffff0000},
        {5, 0x80000020, 0xffffffff}, {6, 0x80000040, 0xffffffff},
    };
    for (const auto &record : records) {
        DcacheARequest r;
        r.opcode = 4; r.size = record.size;
        r.address = record.address; r.mask = record.mask;
        // Generic Get uses the negotiated source range, independent of MSHRs.
        r.source = 35; r.request_source = 31;
        r.alias = 3; r.virtual_address = 0x12345;
        r.keyword = true; r.need_hint = true;
        check(r);
        auto bad = r; bad.source = 36;
        check(bad, "invalid DCache Get source");
        bad = r; bad.source = 63;
        check(bad, "invalid DCache Get source");
        bad = r; bad.source = 0;
        check(bad);
        bad = r; bad.mask ^= 1;
        check(bad, "invalid DCache Get mask");
        bad = r; bad.param = 1;
        check(bad, "invalid DCache Get param");
        if (record.size != 0) {
            bad = r; bad.address |= 1;
            check(bad, "unaligned DCache Get address");
        }
        bad = r; bad.corrupt = true;
        check(bad, "DCache TileLink A request is corrupt");
    }
}
static void invalid_op_and_size() {
    for (unsigned opcode : {0U, 1U, 2U, 3U, 5U, 8U, 9U, 10U, 11U, 15U, 255U}) {
        auto r = acquire(); r.opcode = opcode;
        check(r, "unsupported DCache TileLink A opcode");
    }
    for (unsigned opcode : {4U, 6U, 7U, 12U, 13U, 14U}) {
        for (unsigned size : {7U, 32U, 64U, 255U}) {
            auto r = acquire(); r.opcode = opcode; r.size = size;
            check(r, "oversized DCache TileLink A request");
        }
    }
}
static void suppressed_demand_attribution() {
    auto check_suppression = [](
        bool must_suppress, bool prefix_allowed, std::uint64_t demand,
        std::uint64_t target, const char *expected) {
        const char *actual = dcache_suppressed_demand_error(
            must_suppress, prefix_allowed, demand, target);
        ++checks;
        if ((actual == nullptr) != (expected == nullptr) ||
            (actual != nullptr && std::strcmp(actual, expected) != 0)) {
            std::cerr << "suppression check " << checks << ": expected "
                      << (expected ? expected : "valid") << ", got "
                      << (actual ? actual : "valid") << '\n';
            std::exit(1);
        }
    };
    const char *const rejected =
        "suppressed DCache operation issued a disallowed CPU demand request";
    check_suppression(false, false, 1, 0, nullptr);
    check_suppression(true, false, 0, 0, nullptr);
    // Unrelated store/prefetch traffic is absent from these demand-only counts.
    check_suppression(true, false, 1, 1, rejected);
    check_suppression(true, false, 1, 0, rejected);
    check_suppression(true, true, 0, 0, nullptr);
    check_suppression(true, true, 1, 1, nullptr);
    check_suppression(true, true, 1, 0, rejected);
    check_suppression(true, true, 2, 2, rejected);
}
static void demand_request_classification() {
    auto check_demand = [](const DcacheARequest &request, bool expected) {
        ++checks;
        const bool actual = dcache_is_cpu_demand_acquire(request);
        if (actual != expected) {
            std::cerr << "demand classification " << checks << ": expected "
                      << expected << ", got " << actual << '\n';
            std::exit(1);
        }
    };
    auto request = acquire();
    check_demand(request, true);
    request.opcode = 7;
    check_demand(request, true);
    request = acquire();
    request.request_source = 3;  // CPUStoreData
    check_demand(request, false);
    request.request_source = 6;  // L1DataPrefetch
    check_demand(request, false);
    // user.reqSource is unspecified for Get and CMO traffic. A coincidental
    // value of two must not make either opcode look like a demand Acquire.
    request.opcode = 4;
    request.request_source = 2;
    check_demand(request, false);
    request.opcode = 12;
    request.request_source = 2;
    check_demand(request, false);
}
static void faulting_load_requests() {
    auto verify = [](const DcacheARequest &request, bool allowed, bool valid) {
        ++checks;
        const bool actual = dcache_faulting_load_request_error(
            request, allowed, 0x80000680) == nullptr;
        if (actual != valid) {
            std::cerr << "fault request check " << checks << '\n';
            std::exit(1);
        }
    };
    auto r = acquire();
    verify(r, true, true);
    verify(r, false, false);
    // Coherence grants need not imply a store; allow legal load-class growth.
    for (unsigned param : {0U, 1U, 2U}) {
        r.param = param;
        verify(r, true, true);
    }
    r = acquire(); r.address += 64;
    verify(r, true, false);  // unrelated demand
    r = acquire(); r.request_source = 3;
    verify(r, true, false);  // store class cannot impersonate a read
    r = acquire(); r.opcode = 12;
    verify(r, true, false);  // CMO
    r = acquire(); r.opcode = 4;
    verify(r, true, false);  // unclassified Get
    r = acquire(); r.size = 5;
    verify(r, true, false);  // not the target full-line acquire
    r = acquire(); r.request_source = 6; r.address += 128;
    verify(r, false, true);  // legal independent background prefetch
    r.opcode = 4;
    verify(r, true, false);  // Get user source is unspecified
}
static void release_backpressure_observations() {
    auto check_observation = [](
        bool selected, std::uint64_t stalled, std::uint64_t cycles,
        std::uint64_t stability, unsigned forced, bool expected) {
        ++checks;
        const bool actual = dcache_release_backpressure_observation_valid(
            selected, stalled, cycles, stability, forced);
        if (actual != expected) {
            std::cerr << "release observation " << checks << ": expected "
                      << expected << ", got " << actual << '\n';
            std::exit(1);
        }
    };
    // A disabled endpoint may still observe legal incidental manager
    // backpressure, but no forced stalled-release or stall-cycle coverage.
    check_observation(false, 0, 0, 0, 16, true);
    check_observation(false, 0, 0, 1, 16, true);
    check_observation(false, 1, 0, 1, 16, false);
    check_observation(false, 0, 1, 1, 16, false);
    // An enabled endpoint must observe all 16 forced cycles and one stalled
    // target release; incidental additional stability is not silently allowed.
    check_observation(true, 1, 16, 16, 16, true);
    check_observation(true, 0, 16, 16, 16, false);
    check_observation(true, 1, 15, 15, 16, false);
    check_observation(true, 1, 16, 17, 16, false);
}
int main(int argc, char **argv) {
    if (argc != 2) return 2;
    const std::string name = argv[1];
    if (name == "acquire-valid") legal_acquires();
    else if (name == "acquire-mutants") acquire_mutants();
    else if (name == "cmo") cmo_records();
    else if (name == "get") get_records();
    else if (name == "invalid") invalid_op_and_size();
    else if (name == "suppression") suppressed_demand_attribution();
    else if (name == "demand-classification") demand_request_classification();
    else if (name == "faulting-load") faulting_load_requests();
    else if (name == "release-backpressure")
        release_backpressure_observations();
    else return 2;
    std::cout << name << " checks=" << checks << '\n';
}
'''


class DcacheAContractTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        artifact_root = MEMBLOCK_ROOT.parents[1] / "build/memblock/dcache-a-contract"
        artifact_root.mkdir(parents=True, exist_ok=True)
        cls.workspace = tempfile.TemporaryDirectory(prefix="unit-", dir=artifact_root)
        cls.addClassCleanup(cls.workspace.cleanup)
        root = Path(cls.workspace.name)
        source = root / "dcache_a_contract.cpp"
        source.write_text(DRIVER)
        cls.binary = root / "dcache_a_contract"
        subprocess.run(
            ["c++", "-std=c++20", "-O1", "-Wall", "-Wextra", "-Werror",
             "-fsanitize=undefined", "-fno-sanitize-recover=undefined",
             "-I", str(MEMBLOCK_ROOT / "cpp"), str(source), "-o", str(cls.binary)],
            check=True, capture_output=True, text=True, timeout=60,
        )

    def check_case(self, name: str) -> None:
        result = subprocess.run([str(self.binary), name], capture_output=True,
                                text=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("checks=", result.stdout)

    def test_legal_acquire_sources_alias_prefetch_hint_keyword_and_grow(self) -> None:
        self.check_case("acquire-valid")

    def test_acquire_single_field_mutations(self) -> None:
        self.check_case("acquire-mutants")

    def test_cmo_identity_and_unconstrained_sidebands(self) -> None:
        self.check_case("cmo")

    def test_get_masks_sizes_alignment_and_ignored_user_fields(self) -> None:
        self.check_case("get")

    def test_invalid_opcodes_and_oversizes_are_safe(self) -> None:
        self.check_case("invalid")

    def test_suppressed_demand_attribution_is_transaction_causal(self) -> None:
        self.check_case("suppression")

    def test_demand_classification_excludes_unspecified_user_fields(self) -> None:
        self.check_case("demand-classification")

    def test_faulting_load_accepts_only_qualified_read_or_background_prefetch(self) -> None:
        self.check_case("faulting-load")

    def test_release_backpressure_endpoints_separate_forced_coverage(self) -> None:
        self.check_case("release-backpressure")


if __name__ == "__main__":
    unittest.main()
