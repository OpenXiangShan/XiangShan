#!/usr/bin/env python3

from __future__ import annotations

import json
import sys
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
REPO_ROOT = MEMBLOCK_ROOT.parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import rtl_ports  # noqa: E402


FIXTURE = """
module Unrelated(input ignored);
endmodule

module MemBlock(
  input clock,
  input reset,
  output io_req_ready,
  input io_req_valid,
  input [7:0] io_req_bits_address,
  input auto_inner_dcache_client_out_a_ready,
  output auto_inner_dcache_client_out_a_valid,
  output [63:0] auto_inner_dcache_client_out_a_bits_data
);
endmodule
"""


def fixture_config() -> dict:
    return {
        "module": "MemBlock",
        "default_group": "misc",
        "group_rules": [
            {
                "name": "infrastructure",
                "exact": ["clock", "reset"],
            },
            {
                "name": "dcache_tilelink",
                "prefixes": ["auto_inner_dcache_client_"],
            },
        ],
    }


class PortParserTest(unittest.TestCase):
    def test_parses_only_named_module_and_literal_widths(self) -> None:
        ports = rtl_ports.parse_ports(FIXTURE, "MemBlock", fixture_config())
        self.assertEqual(8, len(ports))
        by_name = {port.name: port for port in ports}
        self.assertNotIn("ignored", by_name)
        self.assertEqual(8, by_name["io_req_bits_address"].width)
        self.assertEqual("[7:0]", by_name["io_req_bits_address"].packed_range)
        self.assertEqual(64, by_name["auto_inner_dcache_client_out_a_bits_data"].width)

    def test_classifies_decoupled_and_tilelink_signals(self) -> None:
        ports = rtl_ports.parse_ports(FIXTURE, "MemBlock", fixture_config())
        by_name = {port.name: port for port in ports}
        request = by_name["io_req_bits_address"]
        self.assertEqual("decoupled", request.protocol)
        self.assertEqual("io_req", request.interface)
        self.assertEqual("payload", request.role)

        tl_data = by_name["auto_inner_dcache_client_out_a_bits_data"]
        self.assertEqual("tilelink", tl_data.protocol)
        self.assertEqual("auto_inner_dcache_client_out", tl_data.interface)
        self.assertEqual("a", tl_data.channel)
        self.assertEqual("payload", tl_data.role)

    def test_assigns_safe_idle_input_policies(self) -> None:
        ports = rtl_ports.parse_ports(FIXTURE, "MemBlock", fixture_config())
        by_name = {port.name: port for port in ports}
        self.assertEqual("clock", by_name["clock"].input_policy)
        self.assertEqual("reset_active_high", by_name["reset"].input_policy)
        self.assertEqual("zero", by_name["io_req_valid"].input_policy)
        self.assertEqual(
            "one", by_name["auto_inner_dcache_client_out_a_ready"].input_policy
        )
        self.assertIsNone(by_name["io_req_ready"].input_policy)

    def test_rejects_unsupported_declaration(self) -> None:
        broken = "module MemBlock(ref logic bad); endmodule"
        with self.assertRaisesRegex(rtl_ports.ManifestError, "unsupported"):
            rtl_ports.parse_ports(broken, "MemBlock", fixture_config())

    def test_contract_detects_interface_drift(self) -> None:
        stats = {"port_count": 2, "by_direction": {"input": 2}}
        dimensions = {"issue": {"load": 3}}
        config = {
            "expected_stats": {"port_count": 3},
            "expected_dimensions": {"issue.load": 2},
        }
        with self.assertRaisesRegex(rtl_ports.ManifestError, "port_count"):
            rtl_ports.validate_contract(stats, dimensions, config)

    def test_fixed_dimensions_are_checked_manifest_data(self) -> None:
        config = fixture_config() | {
            "fixed_dimensions": {"queue.store_entries": 56}
        }
        ports = rtl_ports.parse_ports(FIXTURE, "MemBlock", config)
        dimensions = rtl_ports.derive_dimensions(ports, config)
        self.assertEqual(56, dimensions["queue"]["store_entries"])

        config["fixed_dimensions"]["queue.store_entries"] = 0
        with self.assertRaisesRegex(rtl_ports.ManifestError, "positive integer"):
            rtl_ports.derive_dimensions(ports, config)

    def test_complete_hash_changes_when_dependency_changes(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            top = root / "MemBlock.sv"
            dependency = root / "Dependency.sv"
            config = root / "config.json"
            top.write_text("module MemBlock(input clock); endmodule\n")
            dependency.write_text("module Dependency; endmodule\n")
            (root / "filelist.f").write_text("Dependency.sv\nMemBlock.sv\n")
            config.write_text(json.dumps({"module": "MemBlock"}))

            before = rtl_ports.build_manifest(top, config)
            dependency.write_text("module Dependency; wire changed; endmodule\n")
            after = rtl_ports.build_manifest(top, config)

            self.assertEqual(before["rtl_sha256"], after["rtl_sha256"])
            self.assertNotEqual(
                before["complete_rtl_sha256"], after["complete_rtl_sha256"]
            )


class CurrentRtlContractTest(unittest.TestCase):
    def test_fresh_generated_memblock_matches_contract(self) -> None:
        rtl = REPO_ROOT / "build/rtl/MemBlock.sv"
        if not rtl.exists():
            self.skipTest("run `make verilog` to generate build/rtl/MemBlock.sv")
        config_path = MEMBLOCK_ROOT / "config/memblock.json"
        manifest = rtl_ports.build_manifest(rtl, config_path)
        self.assertEqual(1335, manifest["stats"]["port_count"])
        self.assertEqual(3, manifest["dimensions"]["issue"]["scalar_load"])
        self.assertEqual(8, manifest["dimensions"]["identity"]["rob_value_bits"])
        self.assertEqual(56, manifest["dimensions"]["queue"]["store_entries"])

    def test_checked_in_manifest_is_current(self) -> None:
        rtl = REPO_ROOT / "build/rtl/MemBlock.sv"
        expected = MEMBLOCK_ROOT / "config/expected_ports.json"
        if not rtl.exists() or not expected.exists():
            self.skipTest("generated RTL or checked-in manifest is unavailable")
        manifest = rtl_ports.build_manifest(rtl, MEMBLOCK_ROOT / "config/memblock.json")
        self.assertEqual(
            rtl_ports.json_text(manifest), expected.read_text(encoding="utf-8")
        )


if __name__ == "__main__":
    unittest.main()
