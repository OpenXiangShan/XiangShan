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
    def test_directory_exchange_is_atomic_when_supported(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            left = root / "left"
            right = root / "right"
            left.mkdir()
            right.mkdir()
            (left / "value").write_text("left\n", encoding="utf-8")
            (right / "value").write_text("right\n", encoding="utf-8")
            if not freeze_toffee_runtime.exchange_directories(left, right):
                self.skipTest("renameat2 exchange is unavailable")
            self.assertEqual((left / "value").read_text(encoding="utf-8"), "right\n")
            self.assertEqual((right / "value").read_text(encoding="utf-8"), "left\n")

    def test_copies_package_and_records_files(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            package = root / "picker/MemBlock"
            package.mkdir(parents=True)
            (package / "__init__.py").write_text("", encoding="utf-8")
            (package / "libUT_MemBlock.py").write_text("", encoding="utf-8")
            (package / "__pycache__").mkdir()
            (package / "__pycache__/stale.pyc").write_bytes(b"stale")
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
            self.assertEqual(document["package_root"], "picker")
            self.assertEqual(document["source_root"], "python")
            self.assertNotIn(str(root), json.dumps(document["artifacts"]))
            self.assertFalse(any(path.endswith(".pyc") for path in (
                record["path"] for record in document["artifacts"]
            )))

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

    def test_failure_preserves_the_previous_runtime(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            runtime = root / "runtime"
            runtime.mkdir()
            sentinel = runtime / "sentinel"
            sentinel.write_text("old\n", encoding="utf-8")
            package = root / "picker/MemBlock"
            package.mkdir(parents=True)
            (package / "__init__.py").write_text("", encoding="utf-8")
            (package / "libUT_MemBlock.py").write_text("", encoding="utf-8")
            project_source = root / "project/src/memblock_toffee"
            project_source.mkdir(parents=True)
            (project_source / "__init__.py").write_text("", encoding="utf-8")
            port_manifest = root / "config/expected_ports.json"
            port_manifest.parent.mkdir()
            port_manifest.write_text("{}\n", encoding="utf-8")
            rtl = root / "rtl.json"
            rtl.write_text("{}\n", encoding="utf-8")
            lock = root / "toolchain.lock.json"
            lock.write_text("{}\n", encoding="utf-8")
            invalid_environment = root / "environment.json"
            invalid_environment.write_text("not-json\n", encoding="utf-8")
            with self.assertRaises(json.JSONDecodeError):
                freeze_toffee_runtime.freeze(
                    package,
                    project_source.parent,
                    port_manifest,
                    invalid_environment,
                    rtl,
                    lock,
                    root,
                    runtime,
                )
            self.assertEqual(sentinel.read_text(encoding="utf-8"), "old\n")

    def test_non_atomic_replacement_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            runtime = root / "runtime"
            runtime.mkdir()
            sentinel = runtime / "sentinel"
            sentinel.write_text("old\n", encoding="utf-8")
            package = root / "picker/MemBlock"
            package.mkdir(parents=True)
            (package / "__init__.py").write_text("", encoding="utf-8")
            (package / "libUT_MemBlock.py").write_text("", encoding="utf-8")
            project_source = root / "project/src/memblock_toffee"
            project_source.mkdir(parents=True)
            (project_source / "__init__.py").write_text("", encoding="utf-8")
            port_manifest = root / "config/expected_ports.json"
            port_manifest.parent.mkdir()
            port_manifest.write_text("{}\n", encoding="utf-8")
            environment = root / "environment.json"
            environment.write_text("{}\n", encoding="utf-8")
            rtl = root / "rtl.json"
            rtl.write_text("{}\n", encoding="utf-8")
            lock = root / "toolchain.lock.json"
            lock.write_text("{}\n", encoding="utf-8")
            original = freeze_toffee_runtime.exchange_directories
            freeze_toffee_runtime.exchange_directories = lambda *_: False
            try:
                with self.assertRaisesRegex(
                    freeze_toffee_runtime.ToffeeRuntimeError,
                    "atomic directory exchange",
                ):
                    freeze_toffee_runtime.freeze(
                        package,
                        project_source.parent,
                        port_manifest,
                        environment,
                        rtl,
                        lock,
                        root,
                        runtime,
                    )
            finally:
                freeze_toffee_runtime.exchange_directories = original
            self.assertEqual(sentinel.read_text(encoding="utf-8"), "old\n")


if __name__ == "__main__":
    unittest.main()
