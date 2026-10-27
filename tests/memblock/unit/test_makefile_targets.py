#!/usr/bin/env python3

from __future__ import annotations

import shutil
import subprocess
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]


@unittest.skipUnless(shutil.which("make"), "make is required")
class FocusedTargetActionsTest(unittest.TestCase):
    def _dry_run(self, target: str) -> str:
        completed = subprocess.run(
            ["make", "-n", target],
            cwd=MEMBLOCK_ROOT,
            check=True,
            capture_output=True,
            text=True,
        )
        return completed.stdout

    def test_focused_strict_targets_use_4608_actions(self) -> None:
        for target in ("probe-strict-coverage", "balanced-strict-coverage"):
            with self.subTest(target=target):
                output = self._dry_run(target)
                run_lines = [
                    line for line in output.splitlines()
                    if "scripts/run_regression.py" in line
                ]
                self.assertTrue(run_lines, output)
                self.assertIn("--actions 4608", run_lines[0])
                self.assertNotIn("--actions 16384", run_lines[0])


if __name__ == "__main__":
    unittest.main()
