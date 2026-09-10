#!/usr/bin/env python3

from __future__ import annotations

import re
import sys
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
