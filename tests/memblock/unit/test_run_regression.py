#!/usr/bin/env python3

from __future__ import annotations

import json
import os
import sys
import tempfile
import unittest
from unittest import mock
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import run_regression  # noqa: E402


class RunRegressionTest(unittest.TestCase):
    def test_timeout_bytes_are_recorded_without_type_error(self) -> None:
        with mock.patch.object(
            run_regression,
            "_run_process",
            return_value=(None, "partialdiagnostic", True),
        ):
            result = run_regression.run_seed(
                Path("/bin/true"), 7, "random-loads", 1, 1, 128, 1.0, True
            )
        self.assertEqual(result["status"], "timeout")
        self.assertIn("partialdiagnostic", result["output"])

    def test_default_worker_count_uses_measured_host_scaling(self) -> None:
        self.assertEqual(run_regression.DEFAULT_JOBS, 8)

    def test_mixed_minimum_leaves_random_tail_after_mandatory_phases(self) -> None:
        self.assertEqual(run_regression.MINIMUM_MIXED_TRANSACTIONS, 128)
        self.assertEqual(run_regression.DEFAULT_TRANSACTIONS, 16384)
        self.assertEqual(run_regression.DEFAULT_MIXED_TRANSACTIONS, 16384)
        self.assertEqual(run_regression.DEFAULT_TIMEOUT_SECONDS, 1800)

    def test_reads_complete_rtl_hash_from_metadata(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "rtl.json"
            value = "a" * 64
            path.write_text(json.dumps({"complete_rtl_sha256": value}))
            self.assertEqual(value, run_regression.read_complete_rtl_sha256(path))

            path.write_text(json.dumps({"complete_rtl_sha256": "short"}))
            with self.assertRaises(run_regression.RegressionError):
                run_regression.read_complete_rtl_sha256(path)

    def test_parses_passing_summary(self) -> None:
        parsed = run_regression.parse_summary(
            "noise\nMEMBLOCK_RANDOM_PASS seed=7 transactions=20 cycle=99 "
            "ops=1,2 rtl_sha256=abc\n"
        )
        self.assertEqual(parsed["status"], "pass")
        self.assertEqual(parsed["seed"], 7)
        self.assertEqual(parsed["transactions"], 20)
        self.assertEqual(parsed["ops"], "1,2")

    def test_parses_vector_load_summary(self) -> None:
        parsed = run_regression.parse_summary(
            "MEMBLOCK_RANDOM_VECTOR_PASS seed=11 transactions=200 cycle=2342 "
            "eews=1,2,3,4 rtl_sha256=abc\n"
        )
        self.assertEqual(parsed["status"], "pass")
        self.assertEqual(parsed["seed"], 11)
        self.assertEqual(parsed["transactions"], 200)
        self.assertEqual(parsed["eews"], "1,2,3,4")

    def test_parses_vector_forwarding_failure(self) -> None:
        parsed = run_regression.parse_summary(
            "MEMBLOCK_RANDOM_VECTOR_FORWARD_FAIL seed=5 transactions=24 "
            "reason=mismatch\n"
        )
        self.assertEqual(parsed["status"], "fail")
        self.assertEqual(parsed["transactions"], 24)

    def test_default_scenarios_cover_scalar_and_vector_memory(self) -> None:
        self.assertEqual(
            set(run_regression.DEFAULT_SCENARIOS),
            {
                "random-loads",
                "random-forwarding",
                "random-vector-loads",
                "random-vector-forwarding",
                "random-mixed",
            },
        )

    def test_both_forwarding_scenarios_use_forwarding_transaction_count(self) -> None:
        for scenario in ("random-forwarding", "random-vector-forwarding"):
            self.assertEqual(
                run_regression.transaction_count_for_scenario(scenario, 1000, 48),
                48,
            )
        self.assertEqual(
            run_regression.transaction_count_for_scenario("random-vector-loads", 1000, 48),
            1000,
        )
        self.assertEqual(
            run_regression.transaction_count_for_scenario(
                "random-mixed", 1000, 48, 72
            ),
            72,
        )
        self.assertEqual(
            run_regression.transaction_count_for_scenario(
                "random-boundary-hunt", 37, 48, 512
            ),
            37,
        )
        self.assertEqual(
            run_regression.transaction_count_for_scenario(
                run_regression.STRESS_SCENARIO, 1000, 48, 16384
            ),
            16384,
        )

    def test_parses_mixed_summary(self) -> None:
        parsed = run_regression.parse_summary(
            "MEMBLOCK_RANDOM_MIXED_PASS seed=9 transactions=64 cycle=1200 "
            "lq=55+1/56 sq=30+0/30 rtl_sha256=abc\n"
        )
        self.assertEqual(parsed["status"], "pass")
        self.assertEqual(parsed["transactions"], 64)
        self.assertEqual(parsed["lq"], "55+1/56")

    def test_parses_boundary_hunt_hash_and_failures(self) -> None:
        parsed = run_regression.parse_summary(
            "MEMBLOCK_RANDOM_BOUNDARY_HUNT_FAIL seed=9 transactions=37 "
            "failures=4 rtl_sha256=abc\n"
        )
        self.assertEqual(parsed["status"], "fail")
        self.assertEqual(parsed["transactions"], 37)
        self.assertEqual(parsed["failures"], 4)
        self.assertEqual(parsed["rtl_sha256"], "abc")

    def test_parses_stress_summary(self) -> None:
        parsed = run_regression.parse_summary(
            "MEMBLOCK_RANDOM_STRESS_PASS seed=9 transactions=16384 "
            "stress_max_outstanding=12 stress_combinations=1,2,3,4\n",
            expected_scenario=run_regression.STRESS_SCENARIO,
            expected_seed=9,
            expected_transactions=16384,
        )
        self.assertEqual(parsed["status"], "pass")
        self.assertEqual(parsed["stress_max_outstanding"], 12)

    def test_rejects_missing_summary(self) -> None:
        with self.assertRaises(run_regression.RegressionError):
            run_regression.parse_summary("ordinary simulator output")

    def test_rejects_conflicting_terminal_summaries(self) -> None:
        output = (
            "MEMBLOCK_RANDOM_FAIL seed=7 transactions=20\n"
            "MEMBLOCK_RANDOM_PASS seed=7 transactions=20\n"
        )
        with self.assertRaisesRegex(run_regression.RegressionError, "expected one"):
            run_regression.parse_summary(output, expected_scenario="random-loads")

    def test_rejects_summary_for_wrong_scenario(self) -> None:
        output = "MEMBLOCK_RANDOM_VECTOR_PASS seed=7 transactions=20\n"
        with self.assertRaisesRegex(run_regression.RegressionError, "does not match"):
            run_regression.parse_summary(
                output,
                expected_scenario="random-loads",
                expected_seed=7,
                expected_transactions=20,
            )

    def test_rejects_summary_without_expected_seed(self) -> None:
        output = "MEMBLOCK_RANDOM_PASS transactions=20\n"
        with self.assertRaisesRegex(run_regression.RegressionError, "seed"):
            run_regression.parse_summary(
                output,
                expected_scenario="random-loads",
                expected_seed=7,
                expected_transactions=20,
            )

    def test_verifies_complete_frozen_runtime(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            artifacts = []
            for role, name in (
                ("binary", "memblock_sim"),
                ("model", "libUTMemBlock.so"),
                ("xspcomm", "libxspcomm.so.0.0.1"),
            ):
                artifact = root / name
                artifact.write_bytes(role.encode())
                if role == "binary":
                    artifact.chmod(0o555)
                artifacts.append(
                    {
                        "role": role,
                        "path": name,
                        "sha256": run_regression.sha256(artifact),
                    }
                )
            dependency = root / "system-lib.so"
            dependency.write_bytes(b"system")
            metadata = root / "runtime.json"
            metadata.write_text(
                json.dumps(
                    {
                        "schema_version": 1,
                        "artifacts": artifacts,
                        "external_dependencies": [
                            {
                                "path": str(dependency.resolve()),
                                "sha256": run_regression.sha256(dependency),
                            }
                        ],
                    }
                )
            )

            verified = run_regression.verify_runtime_metadata(metadata)
            self.assertEqual(verified["binary"], root / "memblock_sim")
            self.assertEqual(set(verified["artifact_hashes"]), run_regression.RUNTIME_ROLES)

            (root / "libUTMemBlock.so").write_bytes(b"changed")
            with self.assertRaisesRegex(
                run_regression.RegressionError, "artifact hash mismatch"
            ):
                run_regression.verify_runtime_metadata(metadata)

    def test_runtime_rejects_artifact_path_escape(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            outside = root.with_name(root.name + "-outside")
            outside.write_bytes(b"outside")
            try:
                metadata = root / "runtime.json"
                metadata.write_text(
                    json.dumps(
                        {
                            "schema_version": 1,
                            "artifacts": [
                                {
                                    "role": "binary",
                                    "path": "../" + outside.name,
                                    "sha256": run_regression.sha256(outside),
                                }
                            ],
                            "external_dependencies": [],
                        }
                    )
                )
                with self.assertRaisesRegex(
                    run_regression.RegressionError, "local filename"
                ):
                    run_regression.verify_runtime_metadata(metadata)
            finally:
                os.unlink(outside)

    def test_hashes_controller_inputs(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "controller"
            path.write_bytes(b"before")
            before = run_regression.sha256(path)
            path.write_bytes(b"after")
            self.assertNotEqual(before, run_regression.sha256(path))

    def test_controller_file_arguments_are_recordable(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "source.cpp"
            path.write_text("source", encoding="utf-8")
            self.assertEqual(path.resolve(), path.resolve())
            self.assertEqual(len(run_regression.sha256(path)), 64)


if __name__ == "__main__":
    unittest.main()
