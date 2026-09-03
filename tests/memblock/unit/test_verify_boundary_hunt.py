from __future__ import annotations

import json
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))

import verify_boundary_hunt  # noqa: E402


def result(seed: int, transactions: int = 4) -> dict[str, object]:
    return {
        "seed": seed,
        "scenario": "random-boundary-hunt",
        "status": "fail",
        "returncode": 1,
        "transactions": transactions,
        "failures": 2,
        "rtl_sha256": "a" * 64,
        "output": "MEMBLOCK_RANDOM_BOUNDARY_HUNT_SAMPLE_FAIL seed=1",
    }


class VerifyBoundaryHuntTest(unittest.TestCase):
    def test_accepts_continuous_seed_failures(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "hunt.json"
            path.write_text(json.dumps({"results": [result(8), result(7)]}))
            summary = verify_boundary_hunt.verify_boundary_hunt(
                path, min_seeds=2, transactions=4, rtl_sha256="a" * 64
            )
            self.assertEqual(summary["seeds"], 2)
            self.assertEqual(summary["samples"], 8)
            self.assertEqual(summary["failures"], 4)

    def test_rejects_seed_without_oracle_hit(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "hunt.json"
            bad = result(7)
            bad["failures"] = -1
            path.write_text(json.dumps({"results": [bad]}))
            with self.assertRaisesRegex(
                verify_boundary_hunt.BoundaryHuntError, "invalid failure count"
            ):
                verify_boundary_hunt.verify_boundary_hunt(
                    path, min_seeds=1, transactions=4
                )

    def test_all_pass_campaign_is_rejected_until_rtl_fix_is_observed(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "hunt.json"
            passing = result(7)
            passing["status"] = "pass"
            passing["failures"] = 0
            passing["returncode"] = 0
            passing["output"] = ""
            path.write_text(json.dumps({"results": [passing]}))
            with self.assertRaisesRegex(
                verify_boundary_hunt.BoundaryHuntError, "no oracle failure"
            ):
                verify_boundary_hunt.verify_boundary_hunt(
                    path, min_seeds=1, transactions=4
                )

    def test_all_pass_campaign_can_be_used_after_rtl_fix(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "hunt.json"
            passing = result(7)
            passing["status"] = "pass"
            passing["failures"] = 0
            passing["returncode"] = 0
            passing["output"] = ""
            path.write_text(json.dumps({"results": [passing]}))
            summary = verify_boundary_hunt.verify_boundary_hunt(
                path, min_seeds=1, transactions=4, require_failure=False
            )
            self.assertEqual(summary["failures"], 0)

    def test_rejects_status_returncode_mismatch(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "hunt.json"
            bad = result(7)
            bad["returncode"] = 0
            path.write_text(json.dumps({"results": [bad]}))
            with self.assertRaisesRegex(
                verify_boundary_hunt.BoundaryHuntError, "return code"
            ):
                verify_boundary_hunt.verify_boundary_hunt(
                    path, min_seeds=1, transactions=4, rtl_sha256="a" * 64
                )

    def test_rejects_boolean_returncode(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "hunt.json"
            bad = result(7)
            bad["returncode"] = True
            path.write_text(json.dumps({"results": [bad]}))
            with self.assertRaisesRegex(
                verify_boundary_hunt.BoundaryHuntError, "integer return code"
            ):
                verify_boundary_hunt.verify_boundary_hunt(
                    path, min_seeds=1, transactions=4, rtl_sha256="a" * 64
                )

    def test_rejects_malformed_metadata_hash(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            metadata = root / "rtl.json"
            metadata.write_text(json.dumps({"complete_rtl_sha256": "bad"}))
            path = root / "hunt.json"
            path.write_text(json.dumps({"results": [result(7)]}))
            with self.assertRaisesRegex(
                verify_boundary_hunt.BoundaryHuntError, "complete hash"
            ):
                verify_boundary_hunt.verify_boundary_hunt(
                    path,
                    min_seeds=1,
                    transactions=4,
                    rtl_metadata=metadata,
                )


if __name__ == "__main__":
    unittest.main()
