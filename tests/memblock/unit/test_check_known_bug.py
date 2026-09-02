from __future__ import annotations

import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))

import check_known_bug  # noqa: E402


class KnownBugSentinelTest(unittest.TestCase):
    def test_classifies_historical_clean_failure(self) -> None:
        output = (
            "MEMBLOCK_VECTOR_GUEST_FAULT_FAIL cycle=152 "
            "phase=exception-metadata expected_gpaddr=0x94001800 "
            "actual_gpaddr=0x94001808 expected_vs_nonleaf=1 "
            "actual_vs_nonleaf=1\n"
        )
        self.assertEqual(check_known_bug.classify_output(1, output), "clean-failure")

    def test_classifies_repaired_pass(self) -> None:
        output = (
            "MEMBLOCK_VECTOR_GUEST_FAULT_PASS cycle=174 writebacks=1 "
            "vaddr=0x60000188 gpaddr=0x94001800 "
            "rtl_sha256=" + "0" * 64 + "\n"
        )
        self.assertEqual(check_known_bug.classify_output(0, output), "repaired-pass")

    def test_rejects_mismatched_state(self) -> None:
        output = (
            "MEMBLOCK_VECTOR_GUEST_FAULT_FAIL phase=exception-metadata "
            "expected_gpaddr=0x94001800 actual_gpaddr=0x94001800\n"
        )
        self.assertIsNone(check_known_bug.classify_output(1, output))


if __name__ == "__main__":
    unittest.main()
