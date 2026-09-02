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
    def test_existing_commit_can_be_pinned_without_fetch(self) -> None:
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
            commit = subprocess.run(
                ["git", "rev-parse", "HEAD"],
                cwd=checkout,
                check=True,
                capture_output=True,
                text=True,
            ).stdout.strip()

            self.assertTrue(bootstrap_picker.commit_exists(checkout, commit))
            bootstrap_picker.checkout_pinned(checkout, commit, "fixture")
            self.assertEqual(
                commit,
                subprocess.run(
                    ["git", "rev-parse", "HEAD"],
                    cwd=checkout,
                    check=True,
                    capture_output=True,
                    text=True,
                ).stdout.strip(),
            )


if __name__ == "__main__":
    unittest.main()
