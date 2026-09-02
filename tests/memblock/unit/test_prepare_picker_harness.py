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

            sources = root / "sources"
            sources.mkdir()
            main = sources / "memblock_main.cpp"
            environment = sources / "memblock_env.hpp"
            defaults = sources / "generated_port_defaults.hpp"
            main.write_text("main")
            environment.write_text("environment")
            defaults.write_text("defaults")

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
            self.assertEqual((target / "generated.hpp").read_text(), "header")


if __name__ == "__main__":
    unittest.main()
