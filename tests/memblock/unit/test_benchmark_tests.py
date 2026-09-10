#!/usr/bin/env python3

from __future__ import annotations

import json
import re
import sys
import tempfile
import threading
import unittest
from pathlib import Path
from unittest import mock


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import benchmark_tests  # noqa: E402


class BenchmarkTestsTest(unittest.TestCase):
    def test_scenario_inventory_matches_cpp_dispatch(self) -> None:
        source = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text(encoding="utf-8")
        dispatched = set(re.findall(r'options\.test == "([^"]+)"', source))
        self.assertEqual(set(benchmark_tests.SCENARIOS), dispatched)
        self.assertEqual(len(benchmark_tests.SCENARIOS), len(dispatched))

    def test_parses_generic_terminal_summary(self) -> None:
        result = benchmark_tests.parse_terminal(
            "diagnostic\nMEMBLOCK_DCACHE_COHERENCE_PASS cycle=91 probes=3 "
            "rtl_sha256=abc\n"
        )
        self.assertEqual(result["status"], "pass")
        self.assertEqual(result["cycle"], 91)
        self.assertEqual(result["probes"], 3)

    def test_rejects_ambiguous_terminal_summaries(self) -> None:
        with self.assertRaises(benchmark_tests.BenchmarkError):
            benchmark_tests.parse_terminal(
                "MEMBLOCK_SMOKE_PASS cycle=1\nMEMBLOCK_SMOKE_PASS cycle=2\n"
            )

    def test_normalizes_mixed_request_metrics(self) -> None:
        metrics = benchmark_tests.normalized_metrics(
            {
                "cycle": 200,
                "transactions": 64,
                "scalar_writebacks": 30,
                "prefetch_writebacks": 4,
                "store_writebacks": 20,
                "vector_load_writebacks": 5,
                "vector_store_writebacks": 3,
                "tilelink_requests": 12,
                "dcache_refills": 9,
                "ptw_requests": 7,
                "uncache_requests": 2,
                "probes": 1,
                "release_data": 4,
            },
            "random-mixed",
        )
        self.assertEqual(metrics["scalar_loads"], 26)
        self.assertEqual(metrics["scalar_stores"], 20)
        self.assertEqual(metrics["dcache_a"], 12)
        self.assertEqual(metrics["dcache_refills"], 9)

    def test_vector_only_aliases_are_not_reported_as_scalar(self) -> None:
        metrics = benchmark_tests.normalized_metrics(
            {"writebacks": 7, "stores": 4, "loads": 4},
            "vector-store-forwarding",
        )
        self.assertIsNone(metrics["scalar_loads"])
        self.assertIsNone(metrics["scalar_stores"])
        self.assertEqual(metrics["vector_loads"], 7)
        self.assertEqual(metrics["vector_stores"], 4)

    def test_markdown_reports_parallel_worker_limit(self) -> None:
        document = {
            "created_at": "2026-09-10T00:00:00+00:00",
            "configuration": {"jobs": 8},
            "results": [
                {
                    "scenario": "smoke",
                    "status": "pass",
                    "elapsed_seconds": 0.25,
                    "metrics": {"cycles": 38},
                }
            ],
        }
        markdown = benchmark_tests.render_markdown(document)
        self.assertIn("using up to 8 processes", markdown)

    def test_main_runs_scenarios_concurrently_and_preserves_order(self) -> None:
        requested = ("smoke", "single-load", "load-feedback")
        rendezvous = threading.Barrier(len(requested), timeout=5.0)
        active = 0
        maximum_active = 0
        active_lock = threading.Lock()

        def fake_run_scenario(
            binary: Path,
            scenario: str,
            seed: int,
            transactions: int,
            timeout_seconds: float,
            environment: dict[str, str],
            constraint_profile: str,
            constraint_overrides: tuple[str, ...],
        ) -> dict[str, object]:
            nonlocal active, maximum_active
            with active_lock:
                active += 1
                maximum_active = max(maximum_active, active)
            try:
                # A serial implementation cannot bring all workers here and
                # will fail the barrier instead of silently passing this test.
                rendezvous.wait()
                return {
                    "scenario": scenario,
                    "status": "pass",
                    "elapsed_seconds": 0.01,
                    "metrics": {},
                }
            finally:
                with active_lock:
                    active -= 1

        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            metadata = root / "runtime.json"
            output = root / "benchmark.json"
            runtime = {
                "root": root,
                "binary": root / "memblock_sim",
                "metadata": metadata,
                "metadata_sha256": "metadata-hash",
                "artifact_hashes": {"binary": "binary-hash"},
            }
            argv = [
                "benchmark_tests.py",
                "--runtime-metadata",
                str(metadata),
                "--output",
                str(output),
                "--scenarios",
                ",".join(requested),
                "--transactions",
                "256",
                "--jobs",
                str(len(requested)),
            ]
            with (
                mock.patch.object(sys, "argv", argv),
                mock.patch.object(
                    benchmark_tests.run_regression,
                    "verify_runtime_metadata",
                    return_value=runtime,
                ),
                mock.patch.object(
                    benchmark_tests, "run_scenario", side_effect=fake_run_scenario
                ),
            ):
                self.assertEqual(benchmark_tests.main(), 0)

            document = json.loads(output.read_text(encoding="utf-8"))

        self.assertEqual(maximum_active, len(requested))
        self.assertEqual(
            [result["scenario"] for result in document["results"]],
            list(requested),
        )
        self.assertEqual(document["configuration"]["jobs"], len(requested))

    def test_parsed_failure_retains_bounded_output(self) -> None:
        output = "MEMBLOCK_SMOKE_FAIL cycle=17 phase=contract reason=bad-data\n"
        with mock.patch.object(
            benchmark_tests.run_regression,
            "_run_process",
            return_value=(1, output, False),
        ):
            result = benchmark_tests.run_scenario(
                Path("memblock_sim"),
                "smoke",
                1,
                256,
                1.0,
                {},
                "spec",
                (),
            )
        self.assertEqual(result["status"], "fail")
        self.assertEqual(result["output"], output)

if __name__ == "__main__":
    unittest.main()
