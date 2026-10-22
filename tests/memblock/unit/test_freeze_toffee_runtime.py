#!/usr/bin/env python3

from __future__ import annotations

import json
import sys
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import freeze_toffee_runtime  # noqa: E402


class FreezeToffeeRuntimeTest(unittest.TestCase):
    def test_copies_package_and_records_files(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            package = root / "picker/MemBlock"
            package.mkdir(parents=True)
            (package / "__init__.py").write_text("", encoding="utf-8")
            (package / "libUT_MemBlock.py").write_text("", encoding="utf-8")
            environment = root / "environment.json"
            rtl = root / "rtl.json"
            lock = root / "toolchain.lock.json"
            environment.write_text("{}\n", encoding="utf-8")
            rtl.write_text("{}\n", encoding="utf-8")
            lock.write_text("{}\n", encoding="utf-8")
            project_source = root / "project/src"
            package_source = project_source / "memblock_toffee"
            package_source.mkdir(parents=True)
            (package_source / "__init__.py").write_text("", encoding="utf-8")
            port_manifest = root / "config/expected_ports.json"
            port_manifest.parent.mkdir()
            port_manifest.write_text("{}\n", encoding="utf-8")

            metadata = freeze_toffee_runtime.freeze(
                package,
                project_source,
                port_manifest,
                environment,
                rtl,
                lock,
                root,
                root / "runtime",
            )
            document = json.loads(metadata.read_text(encoding="utf-8"))
            self.assertEqual(document["profile"], "toffee")
            self.assertEqual(document["package_name"], "MemBlock")
            package_file = root / "runtime/picker/MemBlock/__init__.py"
            self.assertTrue(package_file.is_file())
            self.assertEqual(package_file.stat().st_mode & 0o777, 0o444)
            self.assertEqual(
                (root / "runtime/picker").stat().st_mode & 0o777,
                0o555,
            )
            self.assertTrue((root / "runtime/python/memblock_toffee/__init__.py").is_file())
            self.assertTrue((root / "runtime/config/expected_ports.json").is_file())

    def test_missing_package_file_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            package = root / "picker/MemBlock"
            package.mkdir(parents=True)
            with self.assertRaisesRegex(
                freeze_toffee_runtime.ToffeeRuntimeError,
                "missing",
            ):
                freeze_toffee_runtime.freeze(
                    package,
                    root / "project",
                    root / "config/expected_ports.json",
                    root / "environment.json",
                    root / "rtl.json",
                    root / "toolchain.lock.json",
                    root,
                    root / "runtime",
                )


if __name__ == "__main__":
    unittest.main()
