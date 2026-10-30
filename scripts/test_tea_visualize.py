"""Verify member-result joins and conservation of sampled time."""

from argparse import Namespace
import csv
import json
from pathlib import Path
import sqlite3
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

import tea_visualize as tea
from tea_visualize import annotate_elf, load_data, export_context, export_cycle_stats


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

    def write_spec_logs(self, name, start=1, end=3):
        directory = Path(self.directory.name) / name
        directory.mkdir()
        (directory / "simulator_out.txt").write_text("teaSamplePeriod0 = 1\nEXCEEDING CYCLE/INSTR LIMIT\n")
        lines = []
        for stamp, values in ((start - 1, (start, 20_000_000)),
                              (end - 1, (end - start, 19_999_986))):
            for counter, value in zip(("clock_cycle", "commitInstr"), values):
                lines.append(f"[PERF ][time= {stamp}] XSCore.rob: {counter}, {value}\n")
        (directory / "simulator_err.txt").write_text("".join(lines))
        return directory

    def test_spec_roi_uses_reset_counters_and_half_open_boundaries(self):
        directory = self.write_spec_logs("valid")
        self.assertEqual(tea.read_spec_roi(directory), ((1, 3), 19_999_986, 2))

    def test_spec_roi_rejects_failed_incomplete_and_inconsistent_checkpoints(self):
        changes = {
            "failed": ("simulator_out.txt", "EXCEEDING CYCLE/INSTR LIMIT", "ABORT at pc"),
            "incomplete": ("simulator_err.txt", "19999986", "1000000"),
            "short_warmup": ("simulator_err.txt", "20000000", "1000000"),
            "wrong_warmup": ("simulator_err.txt", "20000000", "40000000"),
            "wrong_measurement": ("simulator_err.txt", "19999986", "40000000"),
            "bad_reset": ("simulator_err.txt", "clock_cycle, 2", "clock_cycle, 3"),
            "bad_origin": ("simulator_err.txt", "clock_cycle, 1", "clock_cycle, 2"),
            "bad_stamp": ("simulator_err.txt", "time= 2] XSCore.rob: commitInstr",
                          "time= 3] XSCore.rob: commitInstr"),
        }
        for name, (filename, before, after) in changes.items():
            with self.subTest(name=name):
                directory = self.write_spec_logs(name)
                path = directory / filename
                path.write_text(path.read_text().replace(before, after))
                with self.assertRaises(ValueError):
                    tea.read_spec_roi(directory)
        directory = self.write_spec_logs("missing_dump")
        path = directory / "simulator_err.txt"
        path.write_text("".join(path.read_text().splitlines(keepends=True)[:2]))
        with self.assertRaises(ValueError):
            tea.read_spec_roi(directory)

    def test_cli_selects_each_spec_roi_and_keeps_late_final_joins(self):
        first = self.write_spec_logs("first", 1, 3)
        second = self.write_spec_logs("second", 6, 8)
        output = Path(self.directory.name) / "plots"
        command = ["tea_visualize.py", str(self.database), str(self.database),
                   "--spec-log-dir", str(first), str(second), "--output", str(output)]
        captured = []
        with patch.object(sys, "argv", command), \
             patch.object(tea, "plot_comparison", side_effect=lambda runs, *args: captured.extend(runs)), \
             patch.object(tea, "plot_pics"), patch.object(tea, "plot_timeline"):
            tea.main()
        self.assertEqual([run["summary"]["rows"] for run in captured], [2, 2])
        self.assertEqual(captured[0]["weights"][0x90000004][128], 2)
        self.assertEqual(captured[1]["weights"][0x90000010][1], 2)
        self.assertEqual([(run["summary"]["cycle_start"], run["summary"]["cycle_end"])
                          for run in captured], [(1, 3), (6, 8)])
        self.assertTrue(all(run["summary"]["measured_instructions"] == 19_999_986
                            and run["summary"]["measured_cycles"] == 2 for run in captured))
        self.assertTrue(all(run["summary"]["expected_sample_boundaries"] == 2
                            and run["summary"]["missing_interior_samples"] == 0
                            and run["summary"]["missing_endpoint_samples"] == 0 for run in captured))

    def test_cli_compare_first_exports_each_candidates_own_roi(self):
        first = self.write_spec_logs("first-pair", 1, 3)
        second = self.write_spec_logs("second-pair", 6, 8)
        output = Path(self.directory.name) / "delta-plots"
        command = ["tea_visualize.py", str(self.database), str(self.database),
                   "--labels", "baseline", "candidate", "--compare-first",
                   "--spec-log-dir", str(first), str(second), "--output", str(output)]
        with patch.object(sys, "argv", command), patch.object(tea, "plot_comparison"), \
             patch.object(tea, "plot_pics"), patch.object(tea, "plot_timeline"):
            tea.main()
        with (output / "hotspot_deltas.csv").open() as stream:
            rows = list(csv.DictReader(stream))
        self.assertEqual({row["baseline"] for row in rows}, {"baseline"})
        self.assertEqual({row["candidate"] for row in rows}, {"candidate"})
        self.assertTrue(any(row["pc"] == "0x90000004" and float(row["delta_sampled_cycles_per_ki"]) < 0
                            for row in rows))
        self.assertTrue(any(row["pc"] == "0x90000010" and float(row["delta_sampled_cycles_per_ki"]) > 0
                            for row in rows))

    def test_cli_rejects_empty_measured_tea_before_writing_a_report(self):
        directory = self.write_spec_logs("no_samples", 20, 30)
        output = Path(self.directory.name) / "empty-plots"
        command = ["tea_visualize.py", str(self.database), "--spec-log-dir", str(directory),
                   "--output", str(output)]
        with patch.object(sys, "argv", command), patch.object(sys, "stderr"), \
             patch.object(tea, "plot_comparison"), patch.object(tea, "plot_pics"), \
             patch.object(tea, "plot_timeline"):
            with self.assertRaises(SystemExit) as error:
                tea.main()
        self.assertEqual(error.exception.code, 2)
        self.assertFalse(output.exists())

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

    def test_sampling_coverage_separates_interior_loss_duplicates_and_endpoints(self):
        with sqlite3.connect(self.database) as con:
            con.execute("DELETE FROM Tea_0 WHERE CYCLE IN (3, 8)")
            con.execute("UPDATE Tea_0 SET CYCLE=2 WHERE CYCLE=4")
        coverage = tea.sampling_coverage(self.database, (0, 9), 1)
        self.assertEqual(coverage["observed_sample_rows"], 7)
        self.assertEqual(coverage["expected_sample_boundaries"], 9)
        self.assertEqual(coverage["duplicate_sample_rows"], 1)
        self.assertEqual(coverage["missing_interior_samples"], 2)
        self.assertEqual(coverage["missing_endpoint_samples"], 1)
        self.assertEqual(coverage["off_grid_sample_rows"], 0)
        self.assertEqual(tea.sampling_coverage(self.database, (20, 30), 1)["missing_endpoint_samples"], 10)
        with sqlite3.connect(self.database) as con:
            con.execute("DELETE FROM Tea_0 WHERE CYCLE % 2 = 0")
        self.assertEqual(tea.sampling_coverage(self.database, (0, 9), 2)["off_grid_sample_rows"], 3)

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

    def test_context_is_entry_scoped_and_does_not_change_weights(self):
        with sqlite3.connect(self.database) as con:
            for field in ("COMMITBLOCKMASK", "HEADREASONVALID", "HEADREASON", "HEADREADY"):
                con.execute(f"ALTER TABLE Tea_0 ADD COLUMN CONTEXT_{field} INTEGER DEFAULT 0")
            con.execute("UPDATE Tea_0 SET CONTEXT_HEADREASONVALID=1, CONTEXT_HEADREASON=58 WHERE CYCLE=1")
            con.execute("UPDATE Tea_0 SET CONTEXT_COMMITBLOCKMASK=12 WHERE CYCLE=2")
        run = self.load()
        self.assertAlmostEqual(sum(run["contexts"].values()), 9)
        export_context(run, Path(self.directory.name))
        with (Path(self.directory.name) / "head_context.csv").open() as source:
            rows = list(csv.DictReader(source))
        self.assertTrue(any(row["scope"] == "Entry" and row["pc"] == "0x90000004" for row in rows))
        self.assertTrue(any(row["scope"] == "Global" and row["commit_blocks"] == "WFI|redirect" for row in rows))
        filtered = self.load(cycle_range=(0, 3), spec_checkpoint=True)
        self.assertAlmostEqual(sum(filtered["contexts"].values()), 2.5)
        self.assertAlmostEqual(sum(filtered["contexts"].values()), filtered["summary"]["sample_weight"])

    def test_exact_summary_and_censored_interval_export(self):
        with sqlite3.connect(self.database) as con:
            fields = ["ID", "CYCLE", "RETIRED", "CENSOREDGAPS", "PENDING", "CLOSED"]
            fields += [f"STATES_{i}" for i in range(4)] + [f"GAPS_{i}" for i in range(5)]
            con.execute("CREATE TABLE TeaCycleSummary_0 (" + ",".join(f"{f} INTEGER" for f in fields) + ")")
            row = dict.fromkeys(fields, 0)
            row.update(ID=1, CYCLE=10, RETIRED=4, CENSOREDGAPS=1, CLOSED=1, STATES_0=2, STATES_1=5, STATES_2=3)
            con.execute("INSERT INTO TeaCycleSummary_0 VALUES (" + ",".join("?" for _ in fields) + ")", list(row.values()))
            con.execute("CREATE TABLE TeaNoCommit_0 (START INTEGER, END INTEGER, STATES_0 INTEGER, "
                        "STATES_1 INTEGER, STATES_2 INTEGER, LEFTCENSORED INTEGER, RIGHTCENSORED INTEGER)")
            con.execute("INSERT INTO TeaNoCommit_0 VALUES (2,10,5,3,0,0,1)")
        export_cycle_stats(self.database, Path(self.directory.name))
        result = json.loads((Path(self.directory.name) / "cycle_summary.json").read_text())
        self.assertEqual(result["cycles"], 10)
        self.assertEqual(result["cycles_per_kinst"]["Stalled"], 1250)
        self.assertEqual(result["ipc"], 0.4)
        self.assertTrue(result["collection_closed"])
        output = Path(self.directory.name) / "filtered-stats"
        command = ["tea_visualize.py", str(self.database), "--labels", "fixture",
                   "--cycle-range", "1", "3", "--spec-checkpoint", "--output", str(output)]
        with patch.object(sys, "argv", command), patch.object(tea, "plot_comparison"), \
             patch.object(tea, "plot_pics"), patch.object(tea, "plot_timeline"):
            tea.main()
        exported = json.loads((output / "01_fixture" / "cycle_summary.json").read_text())
        self.assertEqual(exported, result)
        sampled = json.loads((output / "summary.json").read_text())["slices"][0]
        self.assertEqual(sampled["selected_rows"], 2)
        with (output / "01_fixture" / "no_commit_intervals.csv").open() as stream:
            interval, = csv.DictReader(stream)
        self.assertEqual((interval["start"], interval["end"]), ("2", "10"))
        with sqlite3.connect(self.database) as con:
            con.execute("UPDATE TeaNoCommit_0 SET END=11")
        with self.assertRaisesRegex(ValueError, "inconsistent no-commit"):
            export_cycle_stats(self.database, Path(self.directory.name))

    def test_full_width_kernel_pc_matches_elf(self):
        pc = 0xFFFFFFFF80001000
        run = self.load()
        self.assertIn(pc, run["weights"])
        objdump = SimpleNamespace(stdout="ffffffff80001000:\t00000013\tnop\n")
        with patch("tea_visualize.subprocess.run", return_value=objdump):
            self.assertEqual(annotate_elf(Path("kernel.elf"), {pc}, "objdump"), {pc: "nop"})

    def test_pc_state_weights_preserve_distinct_causes_and_incomplete_time(self):
        with sqlite3.connect(self.database) as con:
            con.execute("UPDATE Tea_0 SET PCVEC_0=?, PSVVEC_0=128 WHERE ID=1", (0x90000004,))
            con.execute("UPDATE Tea_0 SET STATE=2, VALIDMASK_0=1, PCVEC_0=?, PSVVEC_0=8 WHERE ID=6",
                        (0x90000004,))
        run = self.load()
        states = run["state_weights"]
        self.assertEqual(states["Computing"][0x90000004][128], 0.5)
        self.assertEqual(states["Stalled"][0x90000004][128], 2)
        self.assertEqual(states["Stalled"][0x90000004][-1], 1)
        self.assertEqual(states["Stalled"][None][-2], 1)
        self.assertEqual(states["Flushed"][0x90000004][8], 1)
        self.assertEqual(states["Drained"][0x90000010][1], 2)
        for pc, combinations in run["weights"].items():
            for psv, weight in combinations.items():
                self.assertAlmostEqual(sum(mapping.get(pc, {}).get(psv, 0)
                                           for mapping in states.values()), weight)

    def test_weighted_events_use_time_weights_instead_of_lane_counts(self):
        with sqlite3.connect(self.database) as con:
            con.execute("UPDATE Tea_0 SET PSVVEC_1=64 WHERE ID=1")
        run = self.load()
        summary = run["summary"]
        self.assertEqual(summary["ST_L1"], 1)
        self.assertEqual(summary["ST_L1_weight"], 0.5)
        self.assertEqual(summary["ST_TLB_weight"], 2)
        self.assertEqual([summary[name + "_weight"] for name in
                          ("cancelled", "unresolved", "unknown")], [1, 1, 1])
        self.assertAlmostEqual(sum(bucket["sample_weight"] for bucket in run["timeline"]),
                               summary["sample_weight"])
        for event in ("ST_L1", "ST_TLB", "DR_L1"):
            self.assertAlmostEqual(sum(bucket[event + "_weight"] for bucket in run["timeline"]),
                                   summary[event + "_weight"])

    def test_pc_filter_preserves_state_weight_denominators(self):
        run = self.load(spec_checkpoint=True)
        self.assertEqual(run["summary"]["state_computing"], 2)
        self.assertEqual(run["summary"]["state_computing_weight"], 1)
        self.assertEqual(run["summary"]["excluded_weight"], 1)
        self.assertAlmostEqual(sum(run["summary"]["state_" + state.lower() + "_weight"]
                                   for state in run["state_weights"]),
                               run["summary"]["sample_weight"])

    def test_rendered_evidence_uses_weighted_events_and_exports_all_states(self):
        with sqlite3.connect(self.database) as con:
            con.execute("UPDATE Tea_0 SET PSVVEC_1=64 WHERE ID=1")
        run = self.load(spec_checkpoint=True)
        categories, labels, colors = tea.category_style([run])
        output = Path(self.directory.name)
        saved = {}
        def capture(fig, path):
            saved[path.name] = fig
            self.addCleanup(tea.plt.close, fig)
        with patch("tea_visualize.save_figure", side_effect=capture):
            tea.plot_comparison([run], output, categories, labels, colors)
            tea.plot_timeline(run, output)
            tea.plot_pics(run, output, categories, labels, colors, Namespace(top=2, objdump="objdump"), None)
        heatmap = saved["comparison"].axes[1].images[0].get_array()
        self.assertAlmostEqual(heatmap[tea.EVENTS.index("ST_L1"), 0], 100 * 0.5 / 8)
        with (output / "hotspot_states.csv").open() as stream:
            rows = list(csv.DictReader(stream))
        self.assertAlmostEqual(sum(float(row["sample_weight"]) for row in rows), 8)
        self.assertAlmostEqual(sum(float(row["selected_weight_pct"]) for row in rows), 100)
        self.assertTrue(any(row["state"] == "Stalled" and row["signature"] == "Unresolved"
                            for row in rows))
        self.assertTrue(any(row["state"] == "Drained" for row in rows))
        with (output / "timeline.csv").open() as stream:
            bins = list(csv.DictReader(stream))
        self.assertAlmostEqual(sum(float(row["sample_weight"]) for row in bins), 8)
        self.assertAlmostEqual(sum(float(row["Computing_weight"]) for row in bins), 1)

    def test_sampled_costs_preserve_excluded_and_incomplete_weights(self):
        runs = [self.load(spec_checkpoint=True), self.load(spec_checkpoint=True)]
        for run, label, instructions, cycles in zip(runs, ("baseline", "candidate"),
                                                    (10_000, 20_000), (9_000, 9_000)):
            run["summary"].update(label=label, measured_instructions=instructions,
                                  measured_cycles=cycles, sample_period=1000,
                                  cpi=cycles / instructions)
        categories, labels, colors = tea.category_style(runs)
        output = Path(self.directory.name)
        saved = {}
        def capture(fig, path):
            saved[path.name] = fig
            self.addCleanup(tea.plt.close, fig)
        with patch("tea_visualize.save_figure", side_effect=capture):
            tea.plot_comparison(runs, output, categories, labels, colors)
            tea.plot_pics(runs[0], output, categories, labels, colors,
                          Namespace(top=2, objdump="objdump"), None)
        with (output / "sampled_costs.csv").open() as stream:
            rows = list(csv.DictReader(stream))
        for label, scale in (("baseline", 100), ("candidate", 50)):
            selected = [row for row in rows if row["label"] == label]
            states = [row for row in selected if row["kind"] == "state"]
            self.assertAlmostEqual(sum(float(row["sampled_cycles_per_ki"]) for row in states),
                                   9 * scale)
            excluded, = [row for row in states if row["name"] == "Excluded PC"]
            self.assertAlmostEqual(float(excluded["sampled_cycles_per_ki"]), scale)
            unresolved, = [row for row in selected if row["name"] == "Unresolved"]
            self.assertAlmostEqual(float(unresolved["sampled_cycles_per_ki"]), scale)
        event_values = saved["sampled_costs"].axes[1].images[0].get_array()
        self.assertAlmostEqual(event_values[tea.EVENTS.index("ST_TLB"), 0], 200)
        self.assertAlmostEqual(event_values[tea.EVENTS.index("ST_TLB"), 1], 100)
        with (output / "hotspot_states.csv").open() as stream:
            hotspots = list(csv.DictReader(stream))
        self.assertAlmostEqual(sum(float(row["sampled_cycles_per_ki"]) for row in hotspots), 800)
        self.assertAlmostEqual(sum(float(row["selected_weight_pct"]) for row in hotspots), 100)

    def test_pair_deltas_keep_pc_state_causes_and_normalize_each_run(self):
        baseline = self.load(spec_checkpoint=True)
        with sqlite3.connect(self.database) as con:
            con.execute("UPDATE Tea_0 SET STATE=2, VALIDMASK_0=1, VALIDMASK_7=0, PCVEC_0=?, PSVVEC_0=8 WHERE ID=9",
                        (0x90000020,))
        candidate = self.load(spec_checkpoint=True)
        for run, label, instructions, period in zip((baseline, candidate), ("baseline", "candidate"),
                                                     (10_000, 40_000), (1000, 2000)):
            run["summary"].update(label=label, measured_instructions=instructions,
                                  measured_cycles=9000, sample_period=period, cpi=9000 / instructions)
        output = Path(self.directory.name)
        tea.write_deltas([baseline, candidate], output)
        with (output / "hotspot_deltas.csv").open() as stream:
            rows = list(csv.DictReader(stream))
        vanished, = [row for row in rows if row["pc"] == "0x90000008"]
        self.assertEqual(vanished["state"], "Computing")
        self.assertEqual(float(vanished["baseline_sampled_cycles_per_ki"]), 50)
        self.assertEqual(float(vanished["candidate_sampled_cycles_per_ki"]), 0)
        self.assertEqual(float(vanished["delta_sampled_cycles_per_ki"]), -50)
        added, = [row for row in rows if row["pc"] == "0x90000020"]
        self.assertEqual((added["state"], added["signature"]), ("Flushed", "FL_MB"))
        self.assertEqual(float(added["delta_sampled_cycles_per_ki"]), 50)
        unresolved, = [row for row in rows if row["signature"] == "Unresolved"]
        self.assertEqual(float(unresolved["baseline_sampled_cycles_per_ki"]), 100)
        self.assertEqual(float(unresolved["candidate_sampled_cycles_per_ki"]), 50)
        with (output / "sampled_cost_deltas.csv").open() as stream:
            costs = list(csv.DictReader(stream))
        excluded, = [row for row in costs if row["name"] == "Excluded PC"]
        self.assertEqual(float(excluded["delta_cycles_per_ki"]), -75)
        measured, = [row for row in costs if row["kind"] == "measured"]
        self.assertEqual(float(measured["delta_cycles_per_ki"]), -675)
        state_delta = sum(float(row["delta_cycles_per_ki"]) for row in costs
                          if row["kind"] == "state" and row["name"] != "Excluded PC")
        self.assertAlmostEqual(sum(float(row["delta_sampled_cycles_per_ki"]) for row in rows), state_delta)
        magnitude = [abs(float(row["delta_sampled_cycles_per_ki"])) for row in rows]
        self.assertEqual(magnitude, sorted(magnitude, reverse=True))

    def test_pair_deltas_reconcile_measured_time_and_captured_samples(self):
        baseline = self.load(spec_checkpoint=True)
        with sqlite3.connect(self.database) as con:
            con.execute("DELETE FROM Tea_0 WHERE CYCLE=5")
        candidate = self.load(spec_checkpoint=True)
        for run, label, instructions, cycles in zip(
                (baseline, candidate), ("baseline", "candidate"),
                (10_000, 20_000), (8_500, 12_000)):
            run["summary"].update(label=label, measured_instructions=instructions,
                                  measured_cycles=cycles, sample_period=1000,
                                  cpi=cycles / instructions)
        output = Path(self.directory.name)
        tea.write_deltas([baseline, candidate], output)
        with (output / "sampled_cost_deltas.csv").open() as stream:
            rows = list(csv.DictReader(stream))
        gap, = [row for row in rows if row["kind"] == "sampling"]
        # Keep signed endpoint error and missing samples; do not renormalize them away.
        self.assertAlmostEqual(float(gap["baseline_cycles_per_ki"]), -50)
        self.assertAlmostEqual(float(gap["candidate_cycles_per_ki"]), 200)
        self.assertAlmostEqual(float(gap["delta_cycles_per_ki"]), 250)
        for side in ("baseline", "candidate", "delta"):
            field = side + "_cycles_per_ki"
            measured, = [row for row in rows if row["kind"] == "measured"]
            captured = sum(float(row[field]) for row in rows if row["kind"] == "state")
            self.assertAlmostEqual(captured + float(gap[field]), float(measured[field]))

    def test_empty_window_exports_empty_evidence(self):
        run = self.load(cycle_range=(20, 30))
        self.assertEqual(run["summary"]["sample_weight"], 0)
        categories, labels, colors = tea.category_style([run])
        with patch("tea_visualize.save_figure", side_effect=lambda fig, path: tea.plt.close(fig)):
            tea.plot_pics(run, Path(self.directory.name), categories, labels, colors,
                          Namespace(top=2, objdump="objdump"), None)
        with (Path(self.directory.name) / "hotspot_states.csv").open() as stream:
            self.assertEqual(list(csv.DictReader(stream)), [])


if __name__ == "__main__":
    unittest.main()
