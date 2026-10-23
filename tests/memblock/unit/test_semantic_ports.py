#!/usr/bin/env python3
"""Check the hardware boundary, port directions, and stale mapping rejection."""
from __future__ import annotations

import copy
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))
import generate_semantic_ports as generator


class SemanticPortsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.document = json.loads((ROOT / "config/semantic_ports.json").read_text())

    def load(self, document):
        with tempfile.TemporaryDirectory() as temporary:
            mapping = Path(temporary) / "mapping.json"
            mapping.write_text(json.dumps(document))
            return generator.load_mapping(mapping, ROOT / "config/expected_ports.json")

    def test_rejects_interface_and_semantic_mapping_drift(self) -> None:
        for field, value in (("raw", "missing_hardware_port"), ("direction", "output"),
                             ("width", 999), ("semantic", "class.value"),
                             ("semantic", "domain.__reserved")):
            document = copy.deepcopy(self.document)
            if field == "direction":
                value = "output" if document["ports"][0][field] == "input" else "input"
            document["ports"][0][field] = value
            with self.subTest(field=field, value=value), self.assertRaises(generator.SemanticPortError):
                self.load(document)
        document = copy.deepcopy(self.document)
        document["ports"][0]["semantic"] = "initialize_clock.value"
        with self.assertRaises(generator.SemanticPortError):
            generator.render(self.load(document), [])
        for field in ("raw", "semantic"):
            document = copy.deepcopy(self.document)
            document["ports"][1][field] = document["ports"][0][field]
            with self.subTest(duplicate=field), self.assertRaises(generator.SemanticPortError):
                self.load(document)

    def test_checked_adapter_is_current(self) -> None:
        entries = self.load(self.document)
        helpers = generator.helper_contracts((ROOT / "cpp/generated_port_defaults.hpp").read_text())
        header, source = generator.render(entries, helpers)
        self.assertEqual(header, (ROOT / "cpp/environment/dut_adapter.hpp").read_text())
        self.assertEqual(source, (ROOT / "cpp/environment/dut_adapter.cpp").read_text())

    def test_generated_names_and_picker_types_stop_at_adapter(self) -> None:
        import re
        forbidden = re.compile(r"\b(?:UTMemBlock|XData)\b|\bdut_?\.(?:io_|auto_)\w+")
        for path in (ROOT / "cpp/environment").iterdir():
            if path.suffix not in (".cpp", ".hpp", ".inc") or path.stem == "dut_adapter":
                continue
            with self.subTest(path=path.name):
                self.assertIsNone(forbidden.search(path.read_text()))

    @unittest.skipUnless(shutil.which("c++"), "C++ compiler required")
    def test_port_directions_and_domain_headers_compile_without_picker(self) -> None:
        # Compile the actual domain boundaries using only repository value
        # headers. Driving observed outputs must be rejected by the type system.
        source = '''
#include "environment/dut_adapter.hpp"
#include "environment/bridge_monitor.hpp"
#include "environment/feedback_monitor.hpp"
#include "environment/queue_accounting.hpp"
#include "environment/page_table_builder.hpp"
template<class T> concept CanDrive = requires(T &port) { port.set(0); };
static_assert(CanDrive<memblock::DrivenPort>);
static_assert(!CanDrive<memblock::ReadPort>);
static_assert(!CanDrive<decltype(memblock::DutAdapter::dcache.a.valid)>);
static_assert(CanDrive<decltype(memblock::DutAdapter::dcache.a.ready)>);
'''
        subprocess.run(["c++", "-std=c++20", "-fsyntax-only", "-x", "c++", "-",
                        "-I" + str(ROOT / "cpp")], input=source, text=True,
                       check=True, capture_output=True)


if __name__ == "__main__":
    unittest.main()
