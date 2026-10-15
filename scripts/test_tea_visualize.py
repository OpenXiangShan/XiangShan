"""Verify member-result joins and conservation of sampled time."""

from argparse import Namespace
from pathlib import Path
import sqlite3
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

from tea_visualize import annotate_elf, load_data


class TeaAnalysisTest(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.database = Path(self.directory.name) / "tea.db"
        fields = ["ID", "CYCLE", "STATE", "TARGETVALID", "TARGET_ALLOCATIONCYCLE",
                  "TARGET_ROBINDEX", "TARGET_RANK"]
        fields += [f"VALIDMASK_{i}" for i in range(8)]
        fields += [f"{prefix}_{i}" for prefix in ("PCVEC", "PSVVEC") for i in range(64)]
        with sqlite3.connect(self.database) as con:
            con.execute("CREATE TABLE Tea_0 (" + ",".join(f"{field} INTEGER" for field in fields) + ")")
            con.execute("CREATE TABLE TeaDelayedFinal_0 (CYCLE INTEGER, ISDRAIN INTEGER, "
                        "CANCELLED INTEGER, MEMBER_TARGET_ALLOCATIONCYCLE INTEGER, "
                        "MEMBER_TARGET_ROBINDEX INTEGER, MEMBER_TARGET_RANK INTEGER, "
                        "MEMBER_PC INTEGER, MEMBER_PSV INTEGER)")
            for cycle in range(9):
                row = dict.fromkeys(fields, 0)
                row.update(ID=cycle + 1, CYCLE=cycle, STATE=1)
                if cycle in (0, 8):
                    row.update(STATE=0, VALIDMASK_0=1, PCVEC_0=0x10000000)
                    if cycle == 0:
                        row.update(VALIDMASK_0=3, PCVEC_1=0xFFFFFFFF80001000 - (1 << 64))
                    else:
                        row.update(VALIDMASK_7=128, PCVEC_63=0x90000008)
                elif cycle in (1, 2, 3, 4):
                    row.update(TARGETVALID=1, TARGET_ALLOCATIONCYCLE={1: 10, 2: 10, 3: 20, 4: 30}[cycle],
                               TARGET_ROBINDEX=0, TARGET_RANK=1)
                elif cycle in (6, 7):
                    row.update(STATE=3)
                con.execute("INSERT INTO Tea_0 VALUES (" + ",".join("?" for _ in fields) + ")",
                            [row[field] for field in fields])
            con.executemany("INSERT INTO TeaDelayedFinal_0 VALUES (?,?,?,?,?,?,?,?)", [
                (5, 0, 0, 10, 0, 1, 0x90000004, 128),
                (5, 0, 1, 20, 0, 1, 0x90000004, 64),
                (10, 1, 0, 0, 0, 0, 0x90000010, 1),
            ])

    def load(self, **kwargs):
        args = Namespace(cycle_range=None, spec_checkpoint=False, bins=4)
        args.__dict__.update(kwargs)
        return load_data(self.database, "fixture", args)

    def test_full_time_is_preserved_including_incomplete_attribution(self):
        run = self.load()
        summary = run["summary"]
        self.assertEqual(summary["rows"], 9)
        self.assertAlmostEqual(summary["sample_weight"], 9)
        self.assertEqual([summary[field] for field in
                          ("cancelled_rows", "unresolved_rows", "unknown_rows")], [1, 1, 1])
        self.assertEqual(run["weights"][0x90000004][128], 2)
        self.assertEqual(run["weights"][0x90000004][-1], 1)
        self.assertEqual(run["weights"][0x90000008][0], 0.5)  # unsigned mask bit 63
        self.assertEqual(run["weights"][0x90000010][1], 2)
        self.assertEqual(sum(bucket["rows"] for bucket in run["timeline"]), 9)

    def test_roi_joins_final_results_outside_the_window(self):
        run = self.load(cycle_range=(1, 3))
        self.assertEqual(run["summary"]["unresolved_rows"], 0)
        self.assertEqual(run["weights"][0x90000004][128], 2)
        run = self.load(cycle_range=(6, 8))
        self.assertEqual(run["weights"][0x90000010][1], 2)

    def test_pc_exclusion_does_not_redistribute_computing_weight(self):
        run = self.load(spec_checkpoint=True)
        self.assertAlmostEqual(run["summary"]["sample_weight"], 8)
        self.assertEqual(run["summary"]["excluded_lanes"], 2)
        self.assertEqual(run["weights"][0xFFFFFFFF80001000][0], 0.5)

    def test_full_width_kernel_pc_matches_elf(self):
        pc = 0xFFFFFFFF80001000
        run = self.load()
        self.assertIn(pc, run["weights"])
        objdump = SimpleNamespace(stdout="ffffffff80001000:\t00000013\tnop\n")
        with patch("tea_visualize.subprocess.run", return_value=objdump):
            self.assertEqual(annotate_elf(Path("kernel.elf"), {pc}, "objdump"), {pc: "nop"})


if __name__ == "__main__":
    unittest.main()
