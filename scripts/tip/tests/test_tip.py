import csv
import io
import os
import sqlite3
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest.mock import Mock, patch

from scripts.tip import plot, queries, tip


TIP_SCHEMA = """
create table Tip_0(
  ID integer primary key autoincrement,
  STATE int not null,
  STAMP int not null,
  SITE text,
  COMMITS_ISCOMMIT int not null,
  COMMITS_ISWALK int not null,
  COMMITS_COMMITVALID_0 int not null,
  COMMITS_COMMITVALID_1 int not null,
  COMMITS_COMMITVALID_2 int not null,
  COMMITS_COMMITVALID_3 int not null,
  COMMITS_COMMITVALID_4 int not null,
  COMMITS_COMMITVALID_5 int not null,
  COMMITS_COMMITVALID_6 int not null,
  COMMITS_COMMITVALID_7 int not null,
  COMMITS_INFO_0_DEBUG_PC int not null,
  COMMITS_INFO_1_DEBUG_PC int not null,
  COMMITS_INFO_2_DEBUG_PC int not null,
  COMMITS_INFO_3_DEBUG_PC int not null,
  COMMITS_INFO_4_DEBUG_PC int not null,
  COMMITS_INFO_5_DEBUG_PC int not null,
  COMMITS_INFO_6_DEBUG_PC int not null,
  COMMITS_INFO_7_DEBUG_PC int not null,
  COMMITS_INFO_0_COMMITTYPE int not null,
  COMMITS_INFO_1_COMMITTYPE int not null,
  COMMITS_INFO_2_COMMITTYPE int not null,
  COMMITS_INFO_3_COMMITTYPE int not null,
  COMMITS_INFO_4_COMMITTYPE int not null,
  COMMITS_INFO_5_COMMITTYPE int not null,
  COMMITS_INFO_6_COMMITTYPE int not null,
  COMMITS_INFO_7_COMMITTYPE int not null,
  REDIRECT_VALID int not null,
  REDIRECT_PC int not null,
  REDIRECT_BITS_DEBUGISCTRL int not null,
  REDIRECT_BITS_DEBUGISMEMVIO int not null,
  REDIRECT_BITS_CFIUPDATE_TARGET int not null,
  REDIRECT_BITS_CFIUPDATE_ISMISPRED int not null,
  REDIRECT_BITS_CFIUPDATE_TAKEN int not null,
  REDIRECT_BITS_CFIUPDATE_PREDTAKEN int not null,
  DEBUGLSINFO_S3_ISREPLAY int not null,
  DEBUGLSINFO_S3_ISREPLAYFAST int not null,
  DEBUGLSINFO_S3_ISREPLAYSLOW int not null,
  DEBUGLSINFO_S3_ISREPLAYRS int not null,
  DEBUGLSINFO_S2_ISBANKCONFLICT int not null,
  DEBUGLSINFO_S2_ISDCACHEFIRSTMISS int not null,
  DEBUGLSINFO_S2_ISFORWARDFAIL int not null,
  DEBUGLSINFO_REPLAYCNT int not null,
  DEBUGLSINFO_REPLAYCAUSE_0 int not null,
  DEBUGLSINFO_REPLAYCAUSE_1 int not null,
  DEBUGLSINFO_REPLAYCAUSE_2 int not null,
  DEBUGLSINFO_REPLAYCAUSE_3 int not null,
  DEBUGLSINFO_REPLAYCAUSE_4 int not null,
  DEBUGLSINFO_REPLAYCAUSE_5 int not null,
  DEBUGLSINFO_REPLAYCAUSE_6 int not null,
  DEBUGLSINFO_REPLAYCAUSE_7 int not null,
  DEBUGLSINFO_REPLAYCAUSE_8 int not null,
  DEBUGLSINFO_REPLAYCAUSE_9 int not null
);
"""

TIP_COLUMNS = [
    "STATE", "STAMP", "SITE", "COMMITS_ISCOMMIT", "COMMITS_ISWALK",
    "COMMITS_COMMITVALID_0", "COMMITS_COMMITVALID_1", "COMMITS_COMMITVALID_2", "COMMITS_COMMITVALID_3",
    "COMMITS_COMMITVALID_4", "COMMITS_COMMITVALID_5", "COMMITS_COMMITVALID_6", "COMMITS_COMMITVALID_7",
    "COMMITS_INFO_0_DEBUG_PC", "COMMITS_INFO_1_DEBUG_PC", "COMMITS_INFO_2_DEBUG_PC", "COMMITS_INFO_3_DEBUG_PC",
    "COMMITS_INFO_4_DEBUG_PC", "COMMITS_INFO_5_DEBUG_PC", "COMMITS_INFO_6_DEBUG_PC", "COMMITS_INFO_7_DEBUG_PC",
    "COMMITS_INFO_0_COMMITTYPE", "COMMITS_INFO_1_COMMITTYPE", "COMMITS_INFO_2_COMMITTYPE", "COMMITS_INFO_3_COMMITTYPE",
    "COMMITS_INFO_4_COMMITTYPE", "COMMITS_INFO_5_COMMITTYPE", "COMMITS_INFO_6_COMMITTYPE", "COMMITS_INFO_7_COMMITTYPE",
    "REDIRECT_VALID", "REDIRECT_PC", "REDIRECT_BITS_DEBUGISCTRL", "REDIRECT_BITS_DEBUGISMEMVIO",
    "REDIRECT_BITS_CFIUPDATE_TARGET", "REDIRECT_BITS_CFIUPDATE_ISMISPRED", "REDIRECT_BITS_CFIUPDATE_TAKEN",
    "REDIRECT_BITS_CFIUPDATE_PREDTAKEN", "DEBUGLSINFO_S3_ISREPLAY", "DEBUGLSINFO_S3_ISREPLAYFAST",
    "DEBUGLSINFO_S3_ISREPLAYSLOW", "DEBUGLSINFO_S3_ISREPLAYRS", "DEBUGLSINFO_S2_ISBANKCONFLICT",
    "DEBUGLSINFO_S2_ISDCACHEFIRSTMISS", "DEBUGLSINFO_S2_ISFORWARDFAIL", "DEBUGLSINFO_REPLAYCNT",
    "DEBUGLSINFO_REPLAYCAUSE_0", "DEBUGLSINFO_REPLAYCAUSE_1", "DEBUGLSINFO_REPLAYCAUSE_2", "DEBUGLSINFO_REPLAYCAUSE_3",
    "DEBUGLSINFO_REPLAYCAUSE_4", "DEBUGLSINFO_REPLAYCAUSE_5", "DEBUGLSINFO_REPLAYCAUSE_6", "DEBUGLSINFO_REPLAYCAUSE_7",
    "DEBUGLSINFO_REPLAYCAUSE_8", "DEBUGLSINFO_REPLAYCAUSE_9",
]


def insert_row(conn: sqlite3.Connection, row):
    if len(row) != len(TIP_COLUMNS):
        raise ValueError("row length does not match Tip_0 columns")
    placeholders = ", ".join(["?"] * len(TIP_COLUMNS))
    columns = ", ".join(TIP_COLUMNS)
    conn.execute(f"insert into Tip_0({columns}) values ({placeholders})", row)


class TipScriptTest(unittest.TestCase):
    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmpdir.cleanup)
        self.base = Path(self.tmpdir.name)
        self.db = self.base / "tip.db"
        conn = sqlite3.connect(self.db)
        conn.execute(TIP_SCHEMA)
        insert_row(
            conn,
            (
                0, 0, "", 1, 0,
                1, 1, 0, 0, 0, 0, 0, 0,
                0x100, 0x104, 0, 0, 0, 0, 0, 0,
                2, 1, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0,
                0, 0, 0, 0,
                0, 0, 0, 0,
                0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ),
        )
        insert_row(
            conn,
            (
                1, 1, "", 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0,
                0x100, 0, 0, 0, 0, 0, 0, 0,
                2, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0,
                0, 0, 0, 0,
                1, 1, 0, 0,
                1, 0, 0, 2,
                0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
            ),
        )
        insert_row(
            conn,
            (
                2, 2, "", 0, 1,
                0, 0, 0, 0, 0, 0, 0, 0,
                0x200, 0, 0, 0, 0, 0, 0, 0,
                1, 0, 0, 0, 0, 0, 0, 0,
                1, 0x200, 1, 0,
                0x300, 1, 1, 0,
                0, 0, 0, 0,
                0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            ),
        )
        insert_row(
            conn,
            (
                0, 3, "", 1, 0,
                1, 0, 0, 0, 0, 0, 0, 0,
                0x108, 0, 0, 0, 0, 0, 0, 0,
                3, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0,
                0, 0, 0, 0,
                0, 0, 0, 0,
                0, 1, 0, 0,
                0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
            ),
        )
        conn.commit()
        conn.close()

    def test_latest_db_path_picks_newest(self):
        older = self.base / "older.db"
        older.write_text("", encoding="utf-8")
        os.utime(older, (1, 1))
        os.utime(self.db, None)
        self.assertEqual(queries.latest_db_path(str(self.base / "*.db")), self.db)

    def test_summary_queries(self):
        conn = sqlite3.connect(self.db)
        self.assertEqual(queries.tip_table_name(0), "Tip_0")
        queries.ensure_tip_table(conn, 0)
        state_rows = queries.fetch_state_summary(conn, 0)
        state_counts = {row["state_name"]: row["count"] for row in state_rows}
        self.assertEqual(state_counts["computing"], 2)
        self.assertEqual(state_counts["stalled"], 1)
        self.assertEqual(state_counts["walk"], 1)
        commit_rows = queries.fetch_commit_width_summary(conn, 0)
        commit_counts = {row["commit_width"]: row["count"] for row in commit_rows}
        self.assertEqual(commit_counts[0], 2)
        self.assertEqual(commit_counts[1], 1)
        self.assertEqual(commit_counts[2], 1)
        redirect_rows = queries.fetch_redirect_targets(conn, 0, top=5)
        self.assertEqual(redirect_rows[0]["target"], "0x300")
        replay_rows = queries.fetch_replay_hotspots(conn, 0, top=5)
        self.assertEqual(replay_rows[0]["pc"], "0x100")
        conn.close()

    def test_event_row_queries(self):
        with sqlite3.connect(self.db) as conn:
            conn.row_factory = sqlite3.Row

            commit_rows = queries.fetch_commit_event_rows(conn, 0)
            commit_by_pc = {row["pc"]: row for row in commit_rows}
            self.assertIn("0x100", commit_by_pc)
            self.assertEqual(commit_by_pc["0x100"]["commit_type_name"], "LOAD")

            redirect_rows = queries.fetch_redirect_event_rows(conn, 0, top=5)
            redirect_match = next(
                (
                    row
                    for row in redirect_rows
                    if (row["source_pc"], row["target_pc"]) == ("0x200", "0x300")
                ),
                None,
            )
            self.assertIsNotNone(redirect_match)
            self.assertEqual(redirect_match["ctrl_redirects"], 1)
            self.assertEqual(redirect_match["memvio_redirects"], 0)

            replay_rows = queries.fetch_replay_event_rows(conn, 0, top=5)
            replay_by_pc = {row["pc"]: row for row in replay_rows}
            self.assertIn("0x100", replay_by_pc)
            self.assertEqual(replay_by_pc["0x100"]["replay_cnt_sum"], 2)

    def test_symbolize_rows(self):
        rows = [{"pc": "0X100 ", "count": 3}]
        resolver = Mock()
        resolver.resolve_many.return_value = {
            "0x100": {
                "pc": "0x100",
                "function": "core_list_reverse",
                "file": "src/core_list_join.c",
                "line": 398,
                "location": "src/core_list_join.c:398",
                "symbol_offset": "0x1c",
            }
        }

        from scripts.tip import symbolize

        symbolized = symbolize.symbolize_rows(rows, resolver, pc_field="pc")

        resolver.resolve_many.assert_called_once_with(["0x100"])
        self.assertEqual(symbolized[0]["function"], "core_list_reverse")
        self.assertEqual(symbolized[0]["location"], "src/core_list_join.c:398")
        self.assertEqual(symbolized[0]["count"], 3)

    @patch("scripts.tip.symbolize.subprocess.run")
    def test_elf_resolver_resolve_many(self, run_mock):
        run_mock.side_effect = [
            Mock(stdout="0000000080001080 T core_list_reverse\n", returncode=0),
            Mock(
                stdout="core_list_reverse\nsrc/core_list_join.c:398\n",
                returncode=0,
            ),
        ]

        from scripts.tip import symbolize

        resolver = symbolize.ElfResolver(Path("/tmp/program.elf"))
        result = resolver.resolve_many(["0x80001080"])

        self.assertEqual(result["0x80001080"]["function"], "core_list_reverse")
        self.assertEqual(result["0x80001080"]["file"], "src/core_list_join.c")
        self.assertEqual(result["0x80001080"]["line"], 398)

    @patch("scripts.tip.symbolize.subprocess.run")
    def test_elf_resolver_missing_tools_degrades(self, run_mock):
        run_mock.side_effect = FileNotFoundError()

        from scripts.tip import symbolize

        resolver = symbolize.ElfResolver(Path("/tmp/program.elf"))
        result = resolver.resolve_many(["0x80001080"])

        self.assertEqual(result["0x80001080"]["function"], "<unknown>")
        self.assertEqual(result["0x80001080"]["file"], "<unknown>")
        self.assertEqual(result["0x80001080"]["location"], "<unknown>:0")

    @patch("scripts.tip.symbolize.subprocess.run")
    def test_elf_resolver_resolve_many_multi_pc(self, run_mock):
        run_mock.side_effect = [
            Mock(
                stdout="0000000000000100 T foo_func\n0000000000000200 T bar_func\n",
                returncode=0,
            ),
            Mock(
                stdout="foo_func\nsrc/foo.c:10\nbar_func\nsrc/bar.c:20\n",
                returncode=0,
            ),
        ]

        from scripts.tip import symbolize

        resolver = symbolize.ElfResolver(Path("/tmp/program.elf"))
        result = resolver.resolve_many(["0x100", "0x200"])

        self.assertEqual(result["0x100"]["function"], "foo_func")
        self.assertEqual(result["0x100"]["file"], "src/foo.c")
        self.assertEqual(result["0x100"]["line"], 10)
        self.assertEqual(result["0x200"]["function"], "bar_func")
        self.assertEqual(result["0x200"]["file"], "src/bar.c")
        self.assertEqual(result["0x200"]["line"], 20)

    def test_plot_helpers_create_pngs(self):
        out_dir = self.base / "plots"
        out_dir.mkdir()
        state_rows = [
            {"state_name": "computing", "count": 2},
            {"state_name": "stalled", "count": 1},
        ]
        commit_rows = [
            {"commit_width": 0, "count": 2},
            {"commit_width": 2, "count": 1},
        ]
        pc_rows = [{"pc": "0x100", "count": 3}, {"pc": "0x108", "count": 1}]
        redirect_rows = [{"target": "0x300", "count": 1}]
        plot.plot_state_distribution(state_rows, out_dir / "state.png")
        plot.plot_commit_width_distribution(commit_rows, out_dir / "commit.png")
        plot.plot_pc_hotspots(pc_rows, out_dir / "pc.png")
        plot.plot_redirect_targets(redirect_rows, out_dir / "redirect.png")
        for name in ["state.png", "commit.png", "pc.png", "redirect.png"]:
            path = out_dir / name
            self.assertTrue(path.exists(), name)
            self.assertGreater(path.stat().st_size, 0, name)

    @patch("scripts.tip.symbolize.ElfResolver")
    def test_cli_all_generates_outputs(self, resolver_ctor):
        out_dir = self.base / "results"
        buf = io.StringIO()
        with redirect_stdout(buf):
            rc = tip.main([
                "all",
                "--db",
                str(self.db),
                "--out-dir",
                str(out_dir),
                "--top",
                "5",
            ])
        self.assertEqual(rc, 0)
        resolver_ctor.assert_not_called()
        for name in [
            "state_summary.csv",
            "commit_width_summary.csv",
            "commit_type_summary.csv",
            "pc_hotspots.csv",
            "redirect_targets.csv",
            "replay_hotspots.csv",
            "state_distribution.png",
            "commit_width_distribution.png",
            "top_pc_hotspots.png",
            "redirect_targets.png",
        ]:
            self.assertTrue((out_dir / name).exists(), name)
        self.assertFalse((out_dir / "symbolized_commits.csv").exists())
        self.assertFalse((out_dir / "family_distribution.png").exists())
        with open(out_dir / "state_summary.csv", newline="", encoding="utf-8") as fp:
            rows = list(csv.DictReader(fp))
        self.assertEqual(rows[0]["state_name"], "computing")

    @patch("scripts.tip.symbolize.ElfResolver")
    def test_cli_symbols_generates_symbolized_csv(self, resolver_ctor):
        resolver_instance = Mock()
        resolver_instance.resolve_many.side_effect = lambda pcs: {
            pc: {
                "pc": pc,
                "function": f"func_{pc}",
                "file": "src/core_list_join.c",
                "line": 10,
                "location": "src/core_list_join.c:10",
                "symbol_offset": "0x0",
            }
            for pc in pcs
        }
        resolver_ctor.return_value = resolver_instance

        out_dir = self.base / "symbols"
        cache_path = self.base / "symbol-cache.json"
        rc = tip.main([
            "symbols",
            "--db",
            str(self.db),
            "--out-dir",
            str(out_dir),
            "--top",
            "5",
            "--elf",
            "/tmp/program.elf",
            "--sym-cache",
            str(cache_path),
        ])
        self.assertEqual(rc, 0)
        symbol_path = out_dir / "symbolized_commits.csv"
        self.assertTrue(symbol_path.exists(), symbol_path)
        with symbol_path.open(newline="", encoding="utf-8") as fp:
            rows = list(csv.DictReader(fp))
        self.assertTrue(rows)
        self.assertEqual(rows[0]["function"], f"func_{rows[0]['pc']}")

    @patch("scripts.tip.symbolize.ElfResolver")
    def test_cli_all_includes_symbolized_reports(self, resolver_ctor):
        resolver_instance = Mock()
        resolver_instance.resolve_many.side_effect = lambda pcs: {
            pc: {
                "pc": pc,
                "function": "main",
                "file": "src/core_main.c",
                "line": 15,
                "location": "src/core_main.c:15",
                "symbol_offset": "0x0",
            }
            for pc in pcs
        }
        resolver_ctor.return_value = resolver_instance

        out_dir = self.base / "all-symbols"
        cache_path = self.base / "all-cache.json"
        rc = tip.main([
            "all",
            "--db",
            str(self.db),
            "--out-dir",
            str(out_dir),
            "--top",
            "5",
            "--elf",
            "/tmp/program.elf",
            "--sym-cache",
            str(cache_path),
        ])
        self.assertEqual(rc, 0)
        self.assertTrue((out_dir / "symbolized_commits.csv").exists())
        self.assertFalse((out_dir / "family_distribution.png").exists())


if __name__ == "__main__":
    unittest.main()
