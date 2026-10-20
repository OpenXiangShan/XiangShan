#!/usr/bin/env python3

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import bootstrap_picker  # noqa: E402


class BootstrapPickerTest(unittest.TestCase):
    def test_missing_checkout_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            with self.assertRaisesRegex(bootstrap_picker.BootstrapError, "not a git"):
                bootstrap_picker.checkout_latest(Path(temporary) / "missing", "fixture")

    def test_dirty_checkout_is_not_updated(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            checkout = Path(temporary) / "checkout"
            subprocess.run(["git", "init", "-q", str(checkout)], check=True)
            subprocess.run(
                ["git", "config", "user.email", "memblock@example.invalid"],
                cwd=checkout,
                check=True,
            )
            subprocess.run(
                ["git", "config", "user.name", "MemBlock Test"],
                cwd=checkout,
                check=True,
            )
            (checkout / "tracked").write_text("content\n", encoding="utf-8")
            subprocess.run(["git", "add", "tracked"], cwd=checkout, check=True)
            subprocess.run(["git", "commit", "-qm", "fixture"], cwd=checkout, check=True)
            (checkout / "tracked").write_text("dirty\n", encoding="utf-8")
            with self.assertRaisesRegex(
                bootstrap_picker.BootstrapError, "dirty fixture checkout"
            ):
                bootstrap_picker.checkout_latest(checkout, "fixture")


if __name__ == "__main__":
    unittest.main()
