#!/usr/bin/env python3

from __future__ import annotations

import io
import json
import sys
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import verify_regression  # noqa: E402


RTL_HASH = "a" * 64


def mixed_result(seed: int) -> dict[str, object]:
    fields: dict[str, object] = {
        "seed": seed,
        "transactions": 64,
        "cycle": 100,
        "scalar_writebacks": 20,
        "prefetch_writebacks": 3,
        "store_writebacks": 12,
        "vector_load_writebacks": 8,
        "vector_store_writebacks": 4,
        "tilelink_requests": 30,
        "release_data": 2,
        "scalar_misaligned": 1,
        "ptw_requests": 3,
        "uncache_requests": 2,
        "lq": "31+1/32",
        "sq": "16+0/16",
        "load_ops": "1,1,1,1,1,1,1",
        "store_ops": "1,1,1,1",
        "scalar": "12,8",
        "vector": "8,4",
        "eew_load": "1,1,1,1",
        "eew_store": "1,1,1,1",
        "vec_load_modes": "1,1,1,1",
        "vec_store_modes": "1,1,1,1",
        "vec_load_stride": "1,1,1",
        "vec_store_stride": "1,1",
        "prefetch": "1,1,1",
        "masked": 2,
        "unmasked": 4,
        "vstart": "3,3",
        "vl": "3,3",
        "align": "3,3",
        "store_order": "3,3",
        "waves": 2,
        "coissue": 1,
        "forwarding": "1,1,1,1",
        "memory_types": "5,2",
        "dcache": "1,1",
        "dispatch_widths": "1,1,1,1,1,1",
        "dispatch_lanes": "1,1,1,1,1,1",
        "tlb_reuse": 1,
        "redirects": 1,
        "dirty": 1,
        "max_outstanding": 3,
        "concurrent_ops": "1,1,1,1,1",
        "concurrent": "4,20,4,5,3",
        "backpressure": "1,1,1,1,1,1",
        "rtl_sha256": RTL_HASH,
    }
    summary = "MEMBLOCK_RANDOM_MIXED_PASS " + " ".join(
        f"{name}={value}" for name, value in fields.items()
    )
    return {
        **fields,
        "status": "pass",
        "scenario": "random-mixed",
        "returncode": 0,
        "output": "",
        "elapsed_seconds": 0.1,
        "submitted_offset_seconds": float(seed - 7) / 10,
        "completed_offset_seconds": 4.5 + float(seed - 7) / 2,
        "command": [
            "/frozen/memblock_sim",
            "--test",
            "random-mixed",
            "--seed",
            str(seed),
            "--transactions",
            "64",
        ],
        "summary": summary,
    }


def regression_document(results: list[dict[str, object]]) -> dict[str, object]:
    runtime_hashes = {
        "binary": "b" * 64,
        "model": "c" * 64,
        "rtl_metadata": "2" * 64,
        "xspcomm": "d" * 64,
    }
    external_hashes = {"/lib/system.so": "e" * 64}
    controller_hashes = {"runner": "f" * 64, "rtl_metadata": "1" * 64}
    return {
        "schema_version": 2,
        "campaign_status": "complete",
        "run_id": "0123456789abcdef0123456789abcdef",
        "started_at": "2026-01-01T00:00:00+00:00",
        "finished_at": "2026-01-01T00:00:05+00:00",
        "elapsed_seconds": 5.0,
        "binary": "/frozen/memblock_sim",
        "binary_sha256": runtime_hashes["binary"],
        "complete_rtl_sha256": RTL_HASH,
        "configuration": {
            "backpressure": True,
            "duration_seconds": 4.0,
            "jobs": 8,
            "mixed_transactions_per_seed": 64,
            "scenarios": ["random-mixed"],
            "start_seed": 7,
        },
        "controller": {
            "hashes_before": controller_hashes,
            "hashes_after": controller_hashes,
            "unchanged": True,
            "error": None,
        },
        "runtime": {
            "metadata_sha256_before": "2" * 64,
            "metadata_sha256_after": "2" * 64,
            "artifact_hashes_before": runtime_hashes,
            "artifact_hashes_after": runtime_hashes,
            "external_dependency_hashes_before": external_hashes,
            "external_dependency_hashes_after": external_hashes,
            "unchanged": True,
            "error": None,
        },
        "summary": {
            "seeds_completed": len(results),
            "statuses": {"pass": len(results)},
            "transactions_completed": 64 * len(results),
            "rtl_sha256": [RTL_HASH],
            "rtl_hash_consistent": True,
            "runtime_unchanged": True,
            "controller_unchanged": True,
        },
        "results": results,
    }


class VerifyRegressionTest(unittest.TestCase):
    def test_historical_coverage_gate_is_independent_of_submission_minimum(self) -> None:
        self.assertEqual(
            verify_regression.ENHANCED_MIXED_COVERAGE_TRANSACTIONS, 128
        )
        self.assertLess(
            verify_regression.ENHANCED_MIXED_COVERAGE_TRANSACTIONS,
            verify_regression.run_regression.MINIMUM_MIXED_TRANSACTIONS,
        )

    def verify(self, document: dict[str, object]) -> dict[str, object]:
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "result.json"
            path.write_text(json.dumps(document, sort_keys=True), encoding="utf-8")
            return verify_regression.verify_regression(
                path,
                min_duration_seconds=4,
                min_results=2,
                expected_scenario="random-mixed",
                expected_transactions=64,
                expected_rtl_sha256=RTL_HASH,
                require_backpressure=True,
                require_frozen_runtime=True,
                expected_jobs=8,
                chunk_size=17,
            )

    def test_streams_and_verifies_complete_artifact(self) -> None:
        verified = self.verify(regression_document([mixed_result(7), mixed_result(8)]))
        self.assertEqual(verified["result_count"], 2)
        self.assertEqual(verified["transactions"], 128)
        self.assertEqual(verified["first_seed"], 7)
        self.assertEqual(verified["last_seed"], 8)

    def test_constraint_command_must_match_recorded_configuration(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["configuration"]["constraint_profile"] = "spec"
        document["configuration"]["constraint_overrides"] = ["tlb-flush=40"]
        for result in document["results"]:
            result["command"].extend(
                ("--constraints", "spec", "--constraint", "tlb-flush=40")
            )
        self.verify(document)

        document["results"][0]["command"][-1] = "tlb-flush=400"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "command options"
        ):
            self.verify(document)

    def test_allow_finite_campaign_without_duration_deadline(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["configuration"]["duration_seconds"] = None
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "finite.json"
            path.write_text(json.dumps(document), encoding="utf-8")
            verified = verify_regression.verify_regression(
                path,
                min_duration_seconds=4,
                min_results=2,
                expected_scenario="random-mixed",
                expected_transactions=64,
                expected_rtl_sha256=RTL_HASH,
                require_backpressure=True,
                require_frozen_runtime=True,
                expected_jobs=8,
                allow_finite=True,
            )
            self.assertEqual(verified["result_count"], 2)

    def test_controller_file_hash_is_checked_when_requested(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            source = Path(temporary) / "source.cpp"
            source.write_text("source", encoding="utf-8")
            document = regression_document([mixed_result(7), mixed_result(8)])
            document["controller"]["paths"] = {"source": str(source)}
            document["controller"]["hashes_before"] = {
                **document["controller"]["hashes_before"],
                "source": verify_regression.run_regression.sha256(source),
            }
            document["controller"]["hashes_after"] = document["controller"]["hashes_before"]
            with tempfile.NamedTemporaryFile(suffix=".json") as artifact:
                artifact_path = Path(artifact.name)
                artifact_path.write_text(json.dumps(document), encoding="utf-8")
                verified = verify_regression.verify_regression(
                    artifact_path,
                    min_duration_seconds=4,
                    min_results=2,
                    expected_scenario="random-mixed",
                    expected_transactions=64,
                    expected_rtl_sha256=RTL_HASH,
                    require_backpressure=True,
                    require_frozen_runtime=True,
                    runner=None,
                    controller_files=(source,),
                )
                self.assertEqual(verified["result_count"], 2)

    def test_rejects_wrong_worker_count(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["configuration"]["jobs"] = 1
        with self.assertRaisesRegex(verify_regression.VerificationError, "worker count"):
            with tempfile.TemporaryDirectory() as temporary:
                path = Path(temporary) / "result.json"
                path.write_text(json.dumps(document), encoding="utf-8")
                verify_regression.verify_regression(
                    path,
                    min_duration_seconds=4,
                    min_results=2,
                    expected_scenario="random-mixed",
                    expected_transactions=64,
                    expected_rtl_sha256=RTL_HASH,
                    expected_jobs=8,
                    require_backpressure=True,
                    require_frozen_runtime=True,
                )

    def test_rejects_running_campaign_marker(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["campaign_status"] = "running"
        with self.assertRaisesRegex(verify_regression.VerificationError, "not complete"):
            self.verify(document)

    def test_rejects_nonfinite_elapsed_time(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["elapsed_seconds"] = float("nan")
        with self.assertRaisesRegex(verify_regression.VerificationError, "not finite"):
            self.verify(document)

    def test_requires_a_result_after_duration_deadline(self) -> None:
        first = mixed_result(7)
        second = mixed_result(8)
        first["completed_offset_seconds"] = 3.0
        second["completed_offset_seconds"] = 3.9
        document = regression_document([first, second])
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "after the required duration"
        ):
            self.verify(document)

    def test_streaming_number_is_not_truncated_at_chunk_boundary(self) -> None:
        reader = verify_regression.StreamingJsonReader(io.StringIO("12345,"), chunk_size=2)
        self.assertEqual(reader.value(), 12345)
        reader.expect(",")
        reader.finish()

    def test_streaming_float_is_not_truncated_before_decimal_point(self) -> None:
        reader = verify_regression.StreamingJsonReader(io.StringIO("1234.5,"), chunk_size=5)
        self.assertEqual(reader.value(), 1234.5)
        reader.expect(",")
        reader.finish()

    def test_rejects_noncontinuous_seeds(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(9)])
        with self.assertRaisesRegex(verify_regression.VerificationError, "continuous"):
            self.verify(document)

    def test_rejects_missing_per_seed_coverage(self) -> None:
        result = mixed_result(7)
        result["eew_load"] = "1,1,1,0"
        result["summary"] = str(result["summary"]).replace(
            "eew_load=1,1,1,1", "eew_load=1,1,1,0"
        )
        document = regression_document([result, mixed_result(8)])
        with self.assertRaisesRegex(verify_regression.VerificationError, "uncovered class"):
            self.verify(document)

    def test_rejects_missing_vector_store_address_mode(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "transactions": 128,
                "store_misaligned": "1,1",
                "vector_replays": 1,
                "virtualization": 2,
                "exceptions": 2,
                "vec_store_modes": "1,1,1,0",
            }
        )
        with self.assertRaisesRegex(verify_regression.VerificationError, "vec_store_modes"):
            verify_regression._check_mixed_coverage(result)

    def test_stress_coverage_requires_combinations_and_outstanding_depth(self) -> None:
        result: dict[str, object] = {
            "stress_load_ops": "1,1,1,1,1,1,1",
            "stress_store_ops": "1,1,1,1",
            "stress_load_lanes": "1,1,1",
            "stress_address_lanes": "1,1",
            "stress_data_lanes": "1,1",
            "stress_store_order": "1,1",
            "stress_eew_load": "1,1,1,1",
            "stress_eew_store": "1,1,1,1",
            "stress_vec_load_modes": "1,1,1,0",
            "stress_vec_store_modes": "1,1,1,0",
            "stress_vec_lanes": "1,1",
            "stress_prefetch": "1,1,1",
            "stress_vstart": "1,1",
            "stress_vl": "1,1",
            "stress_alignment": "1,1",
            "stress_forwarding": "1,1",
            "stress_dcache": "1,1",
            "stress_combinations": "1,1,1,1",
            "stress_masked": 1,
            "stress_unmasked": 1,
            "stress_misaligned": 1,
            "stress_waves": 4,
            "stress_regions": 2,
            "stress_max_outstanding": 10,
            "stress_actions": 96,
            "transactions": 96,
            "stress_backpressure": "1,1,0,0,0,0",
        }
        verify_regression._check_stress_coverage(result, require_backpressure=True)
        result["stress_combinations"] = "1,1,1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "stress_combinations"
        ):
            verify_regression._check_stress_coverage(result)

    def test_rejects_missing_scalar_store_issue_order(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "transactions": 128,
                "store_order": "1,0",
                "store_misaligned": "1,1",
                "vector_replays": 1,
                "virtualization": 2,
                "exceptions": 2,
            }
        )
        with self.assertRaisesRegex(verify_regression.VerificationError, "store_order"):
            verify_regression._check_mixed_coverage(result)

    def test_enhanced_mixed_requires_replay_virtualization_and_exceptions(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "transactions": 128,
                "store_misaligned": "1,1",
                "vector_replays": 1,
                "virtualization": 2,
                "exceptions": 2,
            }
        )
        verify_regression._check_mixed_coverage(result)
        result["vector_replays"] = 0
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "vector_replays"
        ):
            verify_regression._check_mixed_coverage(result)

    def test_required_backpressure_must_be_observed_on_all_managers(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "transactions": 128,
                "store_misaligned": "1,1",
                "vector_replays": 1,
                "virtualization": 2,
                "exceptions": 2,
            }
        )
        verify_regression._check_mixed_coverage(result, require_backpressure=True)
        result["backpressure"] = "1,1,1,1,1,0"
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "uncovered class"
        ):
            verify_regression._check_mixed_coverage(
                result, require_backpressure=True
            )

    def test_mixed_requires_real_unresolved_overlap(self) -> None:
        result = mixed_result(7)
        result.update(
            {
                "transactions": 128,
                "store_misaligned": "1,1",
                "vector_replays": 1,
                "virtualization": 2,
                "exceptions": 2,
                "concurrent": "4,20,0,1,1",
            }
        )
        with self.assertRaisesRegex(
            verify_regression.VerificationError, "unresolved overlap"
        ):
            verify_regression._check_mixed_coverage(result)

    def test_rejects_runtime_hash_change(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["runtime"]["artifact_hashes_after"] = {"binary": "9" * 64}
        with self.assertRaisesRegex(verify_regression.VerificationError, "changed"):
            self.verify(document)

    def test_rejects_aggregate_count_mismatch(self) -> None:
        document = regression_document([mixed_result(7), mixed_result(8)])
        document["summary"]["transactions_completed"] = 64
        with self.assertRaisesRegex(verify_regression.VerificationError, "transactions_completed"):
            self.verify(document)

    def test_forwarding_command_request_and_bounded_result_are_distinct(self) -> None:
        result = {
            "seed": 7,
            "scenario": "random-forwarding",
            "transactions": 48,
            "rtl_sha256": RTL_HASH,
            "status": "pass",
            "returncode": 0,
            "output": "",
            "elapsed_seconds": 0.1,
            "submitted_offset_seconds": 0.0,
            "completed_offset_seconds": 5.0,
            "command": [
                "/frozen/memblock_sim",
                "--test",
                "random-forwarding",
                "--seed",
                "7",
                "--transactions",
                "64",
            ],
            "summary": (
                "MEMBLOCK_RANDOM_FORWARD_PASS seed=7 transactions=48 "
                f"rtl_sha256={RTL_HASH}"
            ),
        }
        checked = verify_regression._check_result(
            result,
            0,
            {"random-forwarding": 64},
            {"random-forwarding": 48},
        )
        self.assertEqual(checked[3], 48)

        result["transactions"] = 47
        result["summary"] = str(result["summary"]).replace(
            "transactions=48", "transactions=47"
        )
        with self.assertRaisesRegex(verify_regression.VerificationError, "expected 48"):
            verify_regression._check_result(
                result,
                0,
                {"random-forwarding": 64},
                {"random-forwarding": 48},
            )

    def test_forwarding_result_caps(self) -> None:
        self.assertEqual(
            verify_regression.completed_transaction_count("random-forwarding", 64),
            48,
        )
        self.assertEqual(
            verify_regression.completed_transaction_count(
                "random-vector-forwarding", 64
            ),
            24,
        )
        self.assertEqual(
            verify_regression.completed_transaction_count("random-loads", 1000),
            1000,
        )


if __name__ == "__main__":
    unittest.main()
