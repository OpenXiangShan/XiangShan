#!/usr/bin/env python3

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import prepare_picker_harness  # noqa: E402


class PreparePickerHarnessTest(unittest.TestCase):
    def test_packages_test_and_support_sources(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            picker = root / "picker"
            compiled = picker / "build" / "UT_MemBlock"
            generated_cpp = picker / "cpp"
            generated_cmake = generated_cpp / "cmake"
            compiled.mkdir(parents=True)
            generated_cmake.mkdir(parents=True)
            (compiled / "libUTMemBlock.so").write_bytes(b"model")
            for name in ("dut.cpp", "dut.hpp", "CMakeLists.txt", "Makefile"):
                (generated_cpp / name).write_text(name)
            (generated_cmake / "verilator.cmake").write_text("cmake")
            (picker / "generated.hpp").write_text("header")
            (picker / "MemBlock_offset.yaml").write_text("signals: []\n")

            sources = root / "sources"
            sources.mkdir()
            main = sources / "memblock_main.cpp"
            environment = sources / "memblock_env.hpp"
            defaults = sources / "generated_port_defaults.hpp"
            scenarios = sources / "scenarios"
            scenarios.mkdir()
            environment_parts = sources / "environment"
            environment_parts.mkdir()
            random_parts = sources / "random"
            random_parts.mkdir()
            main.write_text("main")
            environment.write_text("environment")
            defaults.write_text("defaults")
            (scenarios / "random_mixed.inc").write_text("random-mixed")
            (environment_parts / "model.inc").write_text("model")
            (random_parts / "constraints.inc").write_text("constraints")

            target = prepare_picker_harness.prepare(
                picker, main, environment, defaults
            )
            self.assertEqual((target / "example.cpp").read_text(), "main")
            self.assertEqual(
                (target / "memblock_env.hpp").read_text(), "environment"
            )
            self.assertEqual(
                (target / "generated_port_defaults.hpp").read_text(), "defaults"
            )
            self.assertEqual(
                (target / "scenarios/random_mixed.inc").read_text(), "random-mixed"
            )
            self.assertEqual(
                (target / "environment/model.inc").read_text(), "model"
            )
            self.assertEqual(
                (target / "random/constraints.inc").read_text(), "constraints"
            )
            self.assertEqual((target / "generated.hpp").read_text(), "header")
            self.assertEqual(
                (target / "MemBlock_offset.yaml").read_text(), "signals: []\n"
            )


if __name__ == "__main__":
    unittest.main()
