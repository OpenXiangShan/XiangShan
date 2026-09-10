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
    RTL_SHA256 = "a" * 64

    def make_runtime(
        self,
        root: Path,
        *,
        metadata_sha256: str = "metadata-hash",
        binary_sha256: str = "binary-hash",
    ) -> tuple[Path, dict[str, object]]:
        rtl_metadata = root / "rtl.json"
        rtl_metadata.write_text(
            json.dumps({"complete_rtl_sha256": self.RTL_SHA256}),
            encoding="utf-8",
        )
        return rtl_metadata, {
            "root": root,
            "binary": root / "memblock_sim",
            "metadata": root / "runtime.json",
            "metadata_sha256": metadata_sha256,
            "artifact_hashes": {
                "binary": binary_sha256,
                "rtl_metadata": benchmark_tests.run_regression.sha256(rtl_metadata),
            },
            "external_dependency_hashes": {"libc": "libc-hash"},
        }

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

    def test_rejects_terminal_for_wrong_scenario(self) -> None:
        with self.assertRaisesRegex(benchmark_tests.BenchmarkError, "does not match"):
            benchmark_tests.parse_terminal(
                "MEMBLOCK_SINGLE_LOAD_PASS cycle=22 rtl_sha256="
                + self.RTL_SHA256
                + "\n",
                expected_scenario="smoke",
                expected_rtl_sha256=self.RTL_SHA256,
            )

    def test_rejects_wrong_optional_identity_fields(self) -> None:
        with self.assertRaisesRegex(benchmark_tests.BenchmarkError, "seed is 2"):
            benchmark_tests.parse_terminal(
                "MEMBLOCK_RANDOM_MIXED_PASS seed=2 transactions=256 "
                "rtl_sha256="
                + self.RTL_SHA256
                + "\n",
                expected_scenario="random-mixed",
                expected_seed=1,
                expected_transactions=256,
                expected_rtl_sha256=self.RTL_SHA256,
            )

    def test_rejects_missing_or_wrong_rtl_identity_on_pass(self) -> None:
        for rtl_field in ("", " rtl_sha256=" + "b" * 64):
            with self.subTest(rtl_field=rtl_field):
                with self.assertRaisesRegex(
                    benchmark_tests.BenchmarkError, "rtl_sha256"
                ):
                    benchmark_tests.parse_terminal(
                        "MEMBLOCK_SMOKE_PASS cycle=38" + rtl_field + "\n",
                        expected_scenario="smoke",
                        expected_rtl_sha256=self.RTL_SHA256,
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
            "runtime": {"unchanged": True},
            "controller": {"unchanged": True},
            "rtl_identity": {"consistent": True},
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
            expected_rtl_sha256: str | None,
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
                    "rtl_sha256": expected_rtl_sha256,
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
                "--rtl-metadata",
                str(rtl_metadata),
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
        self.assertEqual(document["status"], "pass")
        self.assertTrue(document["runtime"]["unchanged"])
        self.assertTrue(document["controller"]["unchanged"])
        self.assertTrue(document["rtl_identity"]["consistent"])

    def test_main_rejects_runtime_change_during_benchmark(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            metadata = root / "runtime.json"
            output = root / "benchmark.json"
            rtl_metadata, runtime_before = self.make_runtime(
                root, metadata_sha256="before", binary_sha256="binary-before"
            )
            runtime_after = {
                **runtime_before,
                "metadata_sha256": "after",
                "artifact_hashes": {
                    **runtime_before["artifact_hashes"],
                    "binary": "binary-after",
                },
            }
            argv = [
                "benchmark_tests.py",
                "--runtime-metadata",
                str(metadata),
                "--rtl-metadata",
                str(rtl_metadata),
                "--output",
                str(output),
                "--scenarios",
                "smoke",
                "--transactions",
                "256",
            ]
            passing_result = {
                "scenario": "smoke",
                "status": "pass",
                "elapsed_seconds": 0.01,
                "rtl_sha256": self.RTL_SHA256,
                "metrics": {},
            }
            with (
                mock.patch.object(sys, "argv", argv),
                mock.patch.object(
                    benchmark_tests.run_regression,
                    "verify_runtime_metadata",
                    side_effect=(runtime_before, runtime_after),
                ),
                mock.patch.object(
                    benchmark_tests,
                    "run_scenario",
                    return_value=passing_result,
                ),
            ):
                self.assertEqual(benchmark_tests.main(), 1)

            document = json.loads(output.read_text(encoding="utf-8"))
        self.assertEqual(document["status"], "fail")
        self.assertFalse(document["runtime"]["unchanged"])
        self.assertEqual(
            document["runtime"]["error"],
            "runtime hashes changed during the benchmark",
        )

    def test_main_rejects_controller_change_during_benchmark(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            metadata = root / "runtime.json"
            output = root / "benchmark.json"
            markdown = root / "benchmark.md"
            controller = root / "controller.cpp"
            controller.write_text("before\n", encoding="utf-8")
            rtl_metadata, runtime = self.make_runtime(root)
            argv = [
                "benchmark_tests.py",
                "--runtime-metadata",
                str(metadata),
                "--rtl-metadata",
                str(rtl_metadata),
                "--controller-file",
                str(controller),
                "--output",
                str(output),
                "--markdown",
                str(markdown),
                "--scenarios",
                "smoke",
                "--transactions",
                "256",
            ]

            def mutate_controller(*args: object, **kwargs: object) -> dict[str, object]:
                controller.write_text("after\n", encoding="utf-8")
                return {
                    "scenario": "smoke",
                    "status": "pass",
                    "elapsed_seconds": 0.01,
                    "rtl_sha256": self.RTL_SHA256,
                    "metrics": {},
                }

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
                    side_effect=mutate_controller,
                ),
            ):
                self.assertEqual(benchmark_tests.main(), 1)

            document = json.loads(output.read_text(encoding="utf-8"))
            markdown_text = markdown.read_text(encoding="utf-8")
        self.assertEqual(document["status"], "fail")
        self.assertFalse(document["controller"]["unchanged"])
        self.assertEqual(
            document["controller"]["error"],
            "controller inputs changed during the benchmark",
        )
        self.assertIn("Overall status: **FAIL**", markdown_text)
        self.assertIn("controller inputs: **FAIL**", markdown_text)

    def test_main_rejects_rtl_metadata_outside_frozen_runtime(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            rtl_metadata, runtime = self.make_runtime(root)
            runtime["artifact_hashes"]["rtl_metadata"] = "b" * 64
            argv = [
                "benchmark_tests.py",
                "--runtime-metadata",
                str(root / "runtime.json"),
                "--rtl-metadata",
                str(rtl_metadata),
                "--output",
                str(root / "benchmark.json"),
                "--scenarios",
                "smoke",
                "--transactions",
                "256",
            ]
            with (
                mock.patch.object(sys, "argv", argv),
                mock.patch.object(
                    benchmark_tests.run_regression,
                    "verify_runtime_metadata",
                    return_value=runtime,
                ),
                mock.patch.object(benchmark_tests, "run_scenario") as run_scenario,
            ):
                self.assertEqual(benchmark_tests.main(), 2)
        run_scenario.assert_not_called()

    def test_main_rejects_shared_mem_direct_trace_for_parallel_leaves(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            rtl_metadata, runtime = self.make_runtime(root)
            argv = [
                "benchmark_tests.py",
                "--runtime-metadata",
                str(root / "runtime.json"),
                "--rtl-metadata",
                str(rtl_metadata),
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
            expected_rtl_sha256: str | None,
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
                "--rtl-metadata",
                str(rtl_metadata),
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
