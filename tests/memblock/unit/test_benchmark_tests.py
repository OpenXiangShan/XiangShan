#!/usr/bin/env python3

from __future__ import annotations

import json
import os
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
    def test_make_target_defaults_to_eight_leaf_workers(self) -> None:
        makefile = (MEMBLOCK_ROOT / "Makefile").read_text(encoding="utf-8")
        self.assertEqual(benchmark_tests.MAX_PARALLEL_LEAVES, 8)
        self.assertRegex(makefile, r"(?m)^JOBS \?= 8$")
        benchmark_rule = makefile[
            makefile.index("benchmark-tests:"):
            makefile.index("analyze-spec-counters:")
        ]
        self.assertIn("--jobs $(or $(BENCHMARK_JOBS),$(JOBS))", benchmark_rule)
        self.assertIn(
            "$(if $(BENCHMARK_SCENARIOS),--scenarios $(BENCHMARK_SCENARIOS))",
            benchmark_rule,
        )

    def make_runtime(
        self,
        root: Path,
    ) -> tuple[Path, dict[str, object]]:
        return root / "runtime.json", {
            "root": root,
            "binary": root / "memblock_sim",
            "metadata": root / "runtime.json",
            "artifacts": {"binary": root / "memblock_sim"},
            "external_dependencies": ["/lib/libc.so.6"],
        }

    def test_scenario_inventory_matches_cpp_dispatch(self) -> None:
        source = (MEMBLOCK_ROOT / "cpp/memblock_main.cpp").read_text(encoding="utf-8")
        dispatched = set(re.findall(r'options\.test == "([^"]+)"', source))
        self.assertEqual(set(benchmark_tests.SCENARIOS), dispatched)
        self.assertEqual(len(benchmark_tests.SCENARIOS), len(dispatched))

    def test_parses_generic_terminal_summary(self) -> None:
        result = benchmark_tests.parse_terminal(
            "diagnostic\nMEMBLOCK_DCACHE_COHERENCE_PASS cycle=91 probes=3\n"
        )
        self.assertEqual(result["status"], "pass")
        self.assertEqual(result["cycle"], 91)
        self.assertEqual(result["probes"], 3)

    def test_rejects_ambiguous_terminal_summaries(self) -> None:
        with self.assertRaises(benchmark_tests.BenchmarkError):
            benchmark_tests.parse_terminal(
                "MEMBLOCK_SMOKE_PASS cycle=1\nMEMBLOCK_SMOKE_PASS cycle=2\n"
            )

    def test_rejects_terminal_for_wrong_scenario(self) -> None:
        with self.assertRaisesRegex(benchmark_tests.BenchmarkError, "does not match"):
            benchmark_tests.parse_terminal(
                "MEMBLOCK_SINGLE_LOAD_PASS cycle=22\n",
                expected_scenario="smoke",
            )

    def test_rejects_wrong_optional_identity_fields(self) -> None:
        with self.assertRaisesRegex(benchmark_tests.BenchmarkError, "seed is 2"):
            benchmark_tests.parse_terminal(
                "MEMBLOCK_RANDOM_MIXED_PASS seed=2 transactions=256\n",
                expected_scenario="random-mixed",
                expected_seed=1,
                expected_transactions=256,
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
            "status": "pass",
            "created_at": "2026-09-10T00:00:00+00:00",
            "configuration": {"jobs": 8},
            "runtime_metadata": "/tmp/runtime.json",
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
        requested = benchmark_tests.SCENARIOS[:benchmark_tests.MAX_PARALLEL_LEAVES]
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
            cancellation_event: threading.Event | None,
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
            rtl_metadata, runtime = self.make_runtime(root)
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
        self.assertEqual(
            document["configuration"]["effective_jobs"], len(requested)
        )
        self.assertEqual(document["status"], "pass")

    def test_main_rejects_more_than_eight_leaf_workers(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            argv = [
                "benchmark_tests.py",
                "--runtime-metadata",
                str(root / "runtime.json"),
                "--output",
                str(root / "benchmark.json"),
                "--transactions",
                "256",
                "--jobs",
                "9",
            ]
            with (
                mock.patch.object(sys, "argv", argv),
                mock.patch.object(
                    benchmark_tests.concurrent.futures,
                    "ThreadPoolExecutor",
                ) as executor,
                self.assertRaises(SystemExit) as context,
            ):
                benchmark_tests.main()
        self.assertEqual(context.exception.code, 2)
        executor.assert_not_called()

    def test_main_collects_worker_failure_and_completes_other_leaves(self) -> None:
        requested = ("smoke", "single-load", "load-feedback")
        completed: list[str] = []
        completed_lock = threading.Lock()

        def fake_run_scenario(
            binary: Path,
            scenario: str,
            seed: int,
            transactions: int,
            timeout_seconds: float,
            environment: dict[str, str],
            constraint_profile: str,
            constraint_overrides: tuple[str, ...],
            cancellation_event: threading.Event | None,
        ) -> dict[str, object]:
            with completed_lock:
                completed.append(scenario)
            if scenario == "single-load":
                raise RuntimeError("worker exploded")
            return {
                "scenario": scenario,
                "status": "pass",
                "elapsed_seconds": 0.01,
                "metrics": {},
            }

        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            output = root / "benchmark.json"
            rtl_metadata, runtime = self.make_runtime(root)
            argv = [
                "benchmark_tests.py",
                "--runtime-metadata",
                str(root / "runtime.json"),
                "--output",
                str(output),
                "--scenarios",
                ",".join(requested),
                "--transactions",
                "256",
                "--jobs",
                "3",
            ]
            with (
                mock.patch.object(sys, "argv", argv),
                mock.patch.object(
                    benchmark_tests.run_regression,
                    "verify_runtime_metadata",
                    return_value=runtime,
                ),
                mock.patch.object(
                    benchmark_tests,
                    "run_scenario",
                    side_effect=fake_run_scenario,
                ),
            ):
                self.assertEqual(benchmark_tests.main(), 1)

            document = json.loads(output.read_text(encoding="utf-8"))

        self.assertCountEqual(completed, requested)
        self.assertEqual(
            [result["scenario"] for result in document["results"]],
            list(requested),
        )
        self.assertEqual(document["status"], "fail")
        self.assertEqual(
            document["summary"],
            {
                "requested": 3,
                "completed": 3,
                "passed": 2,
                "failed": 1,
                "failed_scenarios": ["single-load"],
            },
        )
        failed = document["results"][1]
        self.assertEqual(failed["status"], "error")
        self.assertEqual(
            failed["error"],
            "benchmark worker failed: RuntimeError: worker exploded",
        )
        self.assertEqual(failed["returncode"], None)
        self.assertEqual(failed["output"], "")
        self.assertEqual(failed["metrics"], {})
        self.assertIn("--test", failed["command"])
        self.assertIn("single-load", failed["command"])
        self.assertIn("RuntimeError: worker exploded", failed["traceback"])
        self.assertGreaterEqual(failed["elapsed_seconds"], 0.0)


    def test_main_rejects_shared_mem_direct_trace_for_parallel_leaves(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            rtl_metadata, runtime = self.make_runtime(root)
            argv = [
                "benchmark_tests.py",
                "--runtime-metadata",
                str(root / "runtime.json"),
                "--output",
                str(root / "benchmark.json"),
                "--scenarios",
                "smoke,single-load",
                "--transactions",
                "256",
                "--jobs",
                "2",
            ]
            with (
                mock.patch.object(sys, "argv", argv),
                mock.patch.dict(
                    os.environ,
                    {"MEMBLOCK_MEM_DIRECT_TRACE_FILE": str(root / "trace.log")},
                ),
                mock.patch.object(
                    benchmark_tests.run_regression,
                    "verify_runtime_metadata",
                    return_value=runtime,
                ),
                mock.patch.object(benchmark_tests, "run_scenario") as run_scenario,
            ):
                self.assertEqual(benchmark_tests.main(), 2)
        run_scenario.assert_not_called()

    def test_keyboard_interrupt_cancels_all_workers(self) -> None:
        rendezvous = threading.Barrier(2, timeout=5.0)
        canceled = threading.Event()

        def interrupt_or_wait(
            binary: Path,
            scenario: str,
            seed: int,
            transactions: int,
            timeout_seconds: float,
            environment: dict[str, str],
            constraint_profile: str,
            constraint_overrides: tuple[str, ...],
            cancellation_event: threading.Event | None,
        ) -> dict[str, object]:
            self.assertIsNotNone(cancellation_event)
            rendezvous.wait()
            if scenario == "smoke":
                raise KeyboardInterrupt
            if cancellation_event.wait(timeout=2.0):
                canceled.set()
            return {
                "scenario": scenario,
                "status": "error",
                "elapsed_seconds": 0.01,
                "metrics": {},
            }

        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            rtl_metadata, runtime = self.make_runtime(root)
            output = root / "benchmark.json"
            argv = [
                "benchmark_tests.py",
                "--runtime-metadata",
                str(root / "runtime.json"),
                "--output",
                str(output),
                "--scenarios",
                "smoke,single-load",
                "--transactions",
                "256",
                "--jobs",
                "2",
            ]
            with (
                mock.patch.object(sys, "argv", argv),
                mock.patch.object(
                    benchmark_tests.run_regression,
                    "verify_runtime_metadata",
                    return_value=runtime,
                ),
                mock.patch.object(
                    benchmark_tests,
                    "run_scenario",
                    side_effect=interrupt_or_wait,
                ),
            ):
                self.assertEqual(benchmark_tests.main(), 130)
            self.assertFalse(output.exists())
        self.assertTrue(canceled.is_set())

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
