from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))

import analyze_spec_counters as analyzer  # noqa: E402


class AnalyzeSpecCountersTest(unittest.TestCase):
    def test_final_block_ignores_periodic_cumulative_dump(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "simulator_err.txt"
            path.write_text(
                "[PERF ][time=10] SimTop.cpu.l_soc.core.memBlock.inner.LoadUnit_0: "
                "s1_tlb_miss_first_issue, 3\n"
                "[PERF ][time=10] SimTop.cpu.l_soc.core.ctrlBlock.rob.rab: "
                "util_218_219, 0\n"
                "[PERF ][time=20] SimTop.cpu.l_soc.core.memBlock.inner.LoadUnit_0: "
                "s1_tlb_miss_first_issue, 7\n"
                "[PERF ][time=20] SimTop.cpu.l_soc.core.ctrlBlock.rob.rab: "
                "util_218_219, 0\n",
                encoding="utf-8",
            )
            timestamp, counters = analyzer.final_perf_block(path)
            self.assertEqual(timestamp, 20)
            self.assertEqual(counters[next(iter(counters))], 7)

    def test_final_block_ignores_truncated_latest_dump(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "simulator_err.txt"
            path.write_text(
                "[PERF ][time=10] core.rob: load_instr_cnt, 3\n"
                "[PERF ][time=10] core.rob: store_instr_cnt, 2\n"
                "[PERF ][time=10] core.ctrlBlock.rob.rab: util_218_219, 0\n"
                "[PERF ][time=20] core.rob: load_instr_cnt, 7\n",
                encoding="utf-8",
            )
            timestamp, counters = analyzer.final_perf_block(path)
            self.assertEqual(timestamp, 10)
            self.assertEqual(counters[("core.rob", "load_instr_cnt")], 3)
            self.assertEqual(counters[("core.rob", "store_instr_cnt")], 2)

    def test_final_block_rejects_lone_truncated_dump(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "simulator_err.txt"
            path.write_text(
                "[PERF ][time=20] core.rob: load_instr_cnt, 7\n",
                encoding="utf-8",
            )
            with self.assertRaisesRegex(ValueError, "no complete PERF"):
                analyzer.final_perf_block(path)

    def test_classifies_memblock_and_vector_events(self) -> None:
        counters = {
            (
                "SimTop.cpu.l_soc.core_with_l2.core.ctrlBlock.rob",
                "load_instr_cnt",
            ): 70,
            (
                "SimTop.cpu.l_soc.core_with_l2.core.ctrlBlock.rob",
                "store_instr_cnt",
            ): 30,
            (
                "SimTop.cpu.l_soc.core_with_l2.core.memBlock.inner.LoadUnit_0",
                "s1_tlb_miss_first_issue",
            ): 4,
            (
                "SimTop.cpu.l_soc.core_with_l2.core.memBlock.inner.LoadUnit_1",
                "s2_dcache_real_miss_first_issue",
            ): 9,
            (
                "SimTop.cpu.l_soc.core_with_l2.core.memScheduler.IssueQueueVlduVstu",
                "issue_instr_count",
            ): 11,
            (
                "SimTop.cpu.l_soc.core_with_l2.core.memScheduler.IssueQueueVlduVstuVseglduVsegstu",
                "issue_instr_count",
            ): 7,
            (
                "SimTop.cpu.l_soc.core_with_l2.core.memBlock.inner.dcache.dcache.missQueue",
                "miss_req_merge_load",
            ): 6,
            (
                "SimTop.cpu.l_soc.core_with_l2.core.memBlock.inner.dcache.dcache.ldu_0",
                "dcache_read_bank_conflict",
            ): 12,
            (
                "SimTop.cpu.l_soc.core_with_l2.core.memBlock.inner.uncache",
                "uncache_mmio_store",
            ): 2,
        }
        metrics = analyzer.classify_checkpoint(counters)
        self.assertEqual(metrics["scalar_load_instructions"], 70)
        self.assertEqual(metrics["scalar_store_instructions"], 30)
        self.assertEqual(metrics["load_unit_tlb_miss_first_issue"], 4)
        self.assertEqual(metrics["load_unit_dcache_real_miss_first_issue"], 9)
        self.assertEqual(metrics["vector_mem_issue_instructions"], 18)
        self.assertEqual(metrics["vector_ordinary_issue_instructions"], 11)
        self.assertEqual(metrics["vector_segment_issue_instructions"], 7)
        self.assertEqual(metrics["dcache_merged_loads"], 6)
        self.assertEqual(metrics["dcache_bank_conflicts"], 12)
        self.assertEqual(metrics["uncache_mmio_stores"], 2)

    def test_aggregate_reports_missing_logs_without_failing_valid_ones(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "good").mkdir()
            (root / "good" / "simulator_err.txt").write_text(
                "[PERF ][time=4] SimTop.cpu.l_soc.core.ctrlBlock.rob: load_instr_cnt, 1\n"
                "[PERF ][time=4] SimTop.cpu.l_soc.core.ctrlBlock.rob.rab: "
                "util_218_219, 0\n",
                encoding="utf-8",
            )
            (root / "bad").mkdir()
            (root / "bad" / "simulator_err.txt").write_text("not a counter\n", encoding="utf-8")
            report = analyzer.aggregate(root)
            self.assertEqual(report["files_discovered"], 2)
            self.assertEqual(report["checkpoints"], 1)
            self.assertEqual(report["totals"]["scalar_load_instructions"], 1)
            self.assertEqual(len(report["skipped"]), 1)


if __name__ == "__main__":
    unittest.main()
