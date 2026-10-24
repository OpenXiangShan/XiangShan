#!/usr/bin/env python3

from __future__ import annotations

import json
import os
import sys
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import prepare_picker_harness  # noqa: E402


class PreparePickerHarnessTest(unittest.TestCase):
    def test_changed_old_source_invalidates_destination_timestamp(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            source = Path(temporary) / "source.cpp"
            destination = Path(temporary) / "copied.cpp"
            source.write_text("original")
            prepare_picker_harness.copy_required(source, destination)
            os.utime(destination, ns=(20_000_000_000, 20_000_000_000))
            source.write_text("restored changed source")
            os.utime(source, ns=(10_000_000_000, 10_000_000_000))
            prepare_picker_harness.copy_required(source, destination)
            self.assertEqual(destination.read_text(), "restored changed source")
            self.assertGreater(destination.stat().st_mtime_ns, 20_000_000_000)
            changed_mtime = destination.stat().st_mtime_ns
            prepare_picker_harness.copy_required(source, destination)
            self.assertEqual(destination.stat().st_mtime_ns, changed_mtime)

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
            (generated_cpp / "CMakeLists.txt").write_text(
                "add_library(example OBJECT example.cpp)\n"
            )
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
            (sources / "generated_interface_types.hpp").write_text("types")
            removed_header = sources / "removed_public.hpp"
            removed_header.write_text("old interface")
            (scenarios / "constrained_random.inc").write_text(
                "int run_constrained_random(int argc, char **argv) { return 0; }"
            )
            (scenarios / "catalog.json").write_text(json.dumps({
                "schema_version": 1,
                "scenarios": [{
                    "name": "constrained-random", "kind": "constrained-random", "group": "constrained_random",
                    "call": "run_constrained_random(argc, argv)",
                    "make_targets": [], "benchmark_order": 0,
                }],
            }))
            (environment_parts / "model.inc").write_text("model")
            (environment_parts / "environment.cpp").write_text("implementation")
            (environment_parts / "environment.hpp").write_text("interface")
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
                (target / "scenarios/constrained_random.inc").read_text(),
                "int run_constrained_random(int argc, char **argv) { return 0; }",
            )
            self.assertEqual(
                (target / "generated_interface_types.hpp").read_text(), "types"
            )
            self.assertEqual(
                (target / "environment/environment.hpp").read_text(), "interface"
            )
            cmake = (target / "CMakeLists.txt").read_text()
            self.assertIn("environment/environment.cpp", cmake)
            self.assertIn("scenarios/module_constrained_random.cpp", cmake)
            self.assertIn("scenarios/registry.cpp", cmake)
            self.assertNotIn("environment/model.inc", cmake)
            self.assertIn(
                '#include "constrained_random.inc"',
                (target / "scenarios/module_constrained_random.cpp").read_text(),
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
            # Re-preparation keeps compiler outputs and unchanged generated
            # inputs, but removes deleted source files from the private copy.
            build = target / "build"
            build.mkdir()
            object_file = build / "cached.o"
            object_file.write_bytes(b"object")
            unchanged = [target / "example.cpp", target / "CMakeLists.txt",
                         target / "scenarios/module_constrained_random.cpp"]
            mtimes = [path.stat().st_mtime_ns for path in unchanged]
            (target / "environment/obsolete.cpp").write_text("stale")
            removed_header.unlink()
            (environment_parts / "environment.cpp").write_text("changed implementation")
            prepare_picker_harness.prepare(picker, main, environment, defaults)
            self.assertEqual([path.stat().st_mtime_ns for path in unchanged], mtimes)
            self.assertEqual(object_file.read_bytes(), b"object")
            self.assertFalse((target / "environment/obsolete.cpp").exists())
            self.assertFalse((target / "removed_public.hpp").exists())
            self.assertEqual((target / "environment/environment.cpp").read_text(),
                             "changed implementation")


if __name__ == "__main__":
    unittest.main()
