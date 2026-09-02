#!/usr/bin/env python3

from __future__ import annotations

import json
import sys
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import generate_cpp  # noqa: E402


class GenerateCppTest(unittest.TestCase):
    def test_renders_scalar_lane_adapters_from_checked_manifest(self) -> None:
        manifest = json.loads(
            (MEMBLOCK_ROOT / "config" / "expected_ports.json").read_text()
        )
        rendered = generate_cpp.render(manifest)

        self.assertIn("kSweptInputCount = 747U", rendered)
        self.assertIn("kSampledOutputCount = 586U", rendered)
        self.assertIn("kSweptInputBitCount = 7153U", rendered)
        self.assertIn("kSampledOutputBitCount = 5434U", rendered)
        self.assertIn("kPinSpacePatternCount = 256U", rendered)
        self.assertIn("drive_pin_space_pattern", rendered)
        self.assertIn("verify_pin_space_pattern", rendered)
        self.assertIn("sample_all_outputs", rendered)
        self.assertIn("dut.auto_inner_buffers_out_a_ready.ImmSet(pin_pattern_u64", rendered)
        self.assertIn(
            "dut.auto_inner_dcache_client_out_d_bits_data.ImmSetBytes", rendered
        )

        for lane in range(6):
            self.assertIn(
                f"dut.io_ooo_to_mem_enqLsq_req_{lane}_valid.ImmSet", rendered
            )
        for lane in range(3):
            self.assertIn(
                f"dut.io_ooo_to_mem_issueLda_{lane}_valid.ImmSet", rendered
            )
            self.assertIn(
                f"result.valid = dut.io_mem_to_ooo_writebackLda_{lane}_valid.B()",
                rendered,
            )
        for lane in range(2):
            self.assertIn(
                f"dut.io_ooo_to_mem_issueSta_{lane}_valid.ImmSet", rendered
            )
            self.assertIn(
                f"dut.io_ooo_to_mem_issueStd_{lane}_valid.ImmSet", rendered
            )
            self.assertIn(
                f"result.valid = dut.io_mem_to_ooo_writebackSta_{lane}_valid.B()",
                rendered,
            )
            self.assertIn(
                f"result.valid = dut.io_mem_to_ooo_writebackStd_{lane}_valid.B()",
                rendered,
            )
            self.assertIn(
                f"dut.io_ooo_to_mem_issueVldu_{lane}_valid.ImmSet", rendered
            )
            self.assertIn(
                f"dut.io_ooo_to_mem_issueVldu_{lane}_bits_src_4.ImmSetBytes",
                rendered,
            )
            self.assertIn(
                f"result.data = dut.io_mem_to_ooo_writebackVldu_{lane}_bits_data.GetBytes()",
                rendered,
            )

    def test_renders_idle_drives_and_only_terminal_quiescence_checks(self) -> None:
        manifest = {
            "rtl_sha256": "abc",
            "ports": [
                {
                    "name": "clock",
                    "direction": "input",
                    "input_policy": "clock",
                    "role": "clock",
                    "protocol": "clock",
                },
                {
                    "name": "request_valid",
                    "direction": "input",
                    "input_policy": "zero",
                    "role": "valid",
                    "protocol": "decoupled",
                },
                {
                    "name": "bus_a_ready",
                    "direction": "input",
                    "input_policy": "one",
                    "role": "ready",
                    "protocol": "tilelink",
                },
                {
                    "name": "bus_a_valid",
                    "direction": "output",
                    "input_policy": None,
                    "role": "valid",
                    "protocol": "tilelink",
                },
                {
                    "name": "unrelated_valid",
                    "direction": "output",
                    "input_policy": None,
                    "role": "valid",
                    "protocol": "valid",
                },
            ],
        }
        rendered = generate_cpp.render(manifest)
        self.assertIn("dut.request_valid.ImmSet(std::uint64_t{0})", rendered)
        self.assertIn("dut.bus_a_ready.ImmSet(std::uint64_t{1})", rendered)
        self.assertIn("dut.bus_a_valid.B()", rendered)
        self.assertNotIn("dut.clock.ImmSet", rendered)
        self.assertNotIn("dut.unrelated_valid.B()", rendered)

    def test_rejects_missing_input_policy(self) -> None:
        manifest = {
            "rtl_sha256": "abc",
            "ports": [
                {
                    "name": "bad",
                    "direction": "input",
                    "input_policy": None,
                    "role": "value",
                    "protocol": "signal",
                }
            ],
        }
        with self.assertRaises(generate_cpp.CppGenerationError):
            generate_cpp.render(manifest)


if __name__ == "__main__":
    unittest.main()
