#!/usr/bin/env python3
"""Exercise the production manager's alias directory, protocol and byte oracle."""
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]

class DcacheManagerTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.workspace = tempfile.TemporaryDirectory(prefix="memblock-manager-")
        cls.addClassCleanup(cls.workspace.cleanup)
        cls.binary = Path(cls.workspace.name) / "manager"
        result = subprocess.run(["c++", "-std=c++20", "-O0", "-I", str(ROOT / "cpp"),
            str(ROOT / "unit/dcache_manager_fixture.cpp"), "-o", str(cls.binary)],
            text=True, capture_output=True, timeout=60)
        if result.returncode:
            raise RuntimeError(result.stdout + result.stderr)

    def test_grouped_release_versions_match_independent_forward_enumeration(self):
        result = subprocess.run([str(self.binary), "release-version-exhaustive"],
            text=True, capture_output=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_line_grant_lifecycle_uses_manager_transaction_provenance(self):
        for name in ("holds", "final-d-before-e", "e-before-final-d", "same-pa-pending",
                     "recall-pending", "acquire-perm", "scope", "reset"):
            with self.subTest(case=name):
                result = subprocess.run([str(self.binary), "line-grants-" + name],
                    text=True, capture_output=True, timeout=10)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_alias_protocol_and_independent_epoch_data(self):
        for name in ("dirty", "branch", "same-alias-write", "release-race", "ntonn-release",
                     "early-e", "final-d-before-e", "early-e-late-d", "serial-probes", "reset",
                     "bad-byte", "changed-source", "changed-report", "branch-upgrade",
                     "missing-data", "duplicate", "release-version-pending", "release-version-applied",
                     "release-version-midbeat", "release-version-late-value", "release-version-torn",
                     "release-version-overlap-order", "release-version-disjoint", "release-version-explicit"):
            with self.subTest(case=name):
                result = subprocess.run([str(self.binary), name], text=True,
                    capture_output=True, timeout=10)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

if __name__ == "__main__":
    unittest.main()
