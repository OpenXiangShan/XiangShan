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
  COMMITS_INFO_0_COMMIT_V int not null,
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
  REDIRECT_BITS_TARGET int not null,
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
    "COMMITS_INFO_0_COMMIT_V",
    "COMMITS_COMMITVALID_0", "COMMITS_COMMITVALID_1", "COMMITS_COMMITVALID_2", "COMMITS_COMMITVALID_3",
    "COMMITS_COMMITVALID_4", "COMMITS_COMMITVALID_5", "COMMITS_COMMITVALID_6", "COMMITS_COMMITVALID_7",
    "COMMITS_INFO_0_DEBUG_PC", "COMMITS_INFO_1_DEBUG_PC", "COMMITS_INFO_2_DEBUG_PC", "COMMITS_INFO_3_DEBUG_PC",
    "COMMITS_INFO_4_DEBUG_PC", "COMMITS_INFO_5_DEBUG_PC", "COMMITS_INFO_6_DEBUG_PC", "COMMITS_INFO_7_DEBUG_PC",
    "COMMITS_INFO_0_COMMITTYPE", "COMMITS_INFO_1_COMMITTYPE", "COMMITS_INFO_2_COMMITTYPE", "COMMITS_INFO_3_COMMITTYPE",
    "COMMITS_INFO_4_COMMITTYPE", "COMMITS_INFO_5_COMMITTYPE", "COMMITS_INFO_6_COMMITTYPE", "COMMITS_INFO_7_COMMITTYPE",
    "REDIRECT_VALID", "REDIRECT_PC", "REDIRECT_BITS_DEBUGISCTRL", "REDIRECT_BITS_DEBUGISMEMVIO",
    "REDIRECT_BITS_TARGET", "DEBUGLSINFO_S3_ISREPLAY", "DEBUGLSINFO_S3_ISREPLAYFAST",
    "DEBUGLSINFO_S3_ISREPLAYSLOW", "DEBUGLSINFO_S3_ISREPLAYRS", "DEBUGLSINFO_S2_ISBANKCONFLICT",
    "DEBUGLSINFO_S2_ISDCACHEFIRSTMISS", "DEBUGLSINFO_S2_ISFORWARDFAIL", "DEBUGLSINFO_REPLAYCNT",
    "DEBUGLSINFO_REPLAYCAUSE_0", "DEBUGLSINFO_REPLAYCAUSE_1", "DEBUGLSINFO_REPLAYCAUSE_2", "DEBUGLSINFO_REPLAYCAUSE_3",
    "DEBUGLSINFO_REPLAYCAUSE_4", "DEBUGLSINFO_REPLAYCAUSE_5", "DEBUGLSINFO_REPLAYCAUSE_6", "DEBUGLSINFO_REPLAYCAUSE_7",
    "DEBUGLSINFO_REPLAYCAUSE_8", "DEBUGLSINFO_REPLAYCAUSE_9",
]


def insert_tip_row(conn: sqlite3.Connection, **values):
    row = dict.fromkeys(TIP_COLUMNS, 0)
    row.update(SITE="", COMMITS_INFO_0_COMMIT_V=1)
    row.update(values)
    placeholders = ", ".join(["?"] * len(TIP_COLUMNS))
    columns = ", ".join(TIP_COLUMNS)
    conn.execute(
        f"insert into Tip_0({columns}) values ({placeholders})",
        tuple(row[column] for column in TIP_COLUMNS),
    )


class TipScriptTest(unittest.TestCase):
    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmpdir.cleanup)
        self.base = Path(self.tmpdir.name)
        self.db = self.base / "tip.db"
        conn = sqlite3.connect(self.db)
        conn.execute(TIP_SCHEMA)
        insert_tip_row(
            conn,
            STATE=0,
            STAMP=0,
            COMMITS_ISCOMMIT=1,
            COMMITS_COMMITVALID_0=1,
            COMMITS_COMMITVALID_1=1,
            COMMITS_INFO_0_DEBUG_PC=0x100,
            COMMITS_INFO_1_DEBUG_PC=0x104,
            COMMITS_INFO_0_COMMITTYPE=2,
            COMMITS_INFO_1_COMMITTYPE=1,
        )
        insert_tip_row(
            conn,
            STATE=1,
            STAMP=1,
            COMMITS_INFO_0_DEBUG_PC=0x100,
            COMMITS_INFO_0_COMMITTYPE=2,
            DEBUGLSINFO_S3_ISREPLAY=1,
            DEBUGLSINFO_S3_ISREPLAYFAST=1,
            DEBUGLSINFO_S2_ISBANKCONFLICT=1,
            DEBUGLSINFO_REPLAYCNT=2,
            DEBUGLSINFO_REPLAYCAUSE_6=1,
        )
        insert_tip_row(
            conn,
            STATE=2,
            STAMP=2,
            COMMITS_ISWALK=1,
            COMMITS_INFO_0_DEBUG_PC=0x200,
            COMMITS_INFO_0_COMMITTYPE=1,
            REDIRECT_VALID=1,
            REDIRECT_PC=0x200,
            REDIRECT_BITS_DEBUGISCTRL=1,
            REDIRECT_BITS_TARGET=0x300,
        )
        insert_tip_row(
            conn,
            STATE=0,
            STAMP=3,
            COMMITS_ISCOMMIT=1,
            COMMITS_COMMITVALID_0=1,
            COMMITS_INFO_0_DEBUG_PC=0x108,
            COMMITS_INFO_0_COMMITTYPE=3,
            DEBUGLSINFO_S2_ISDCACHEFIRSTMISS=1,
            DEBUGLSINFO_REPLAYCAUSE_4=1,
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
        self.assertEqual(commit_counts[1], 1)
        self.assertEqual(commit_counts[2], 1)
        self.assertNotIn(0, commit_counts)
        redirect_rows = queries.fetch_redirect_targets(conn, 0, top=5)
        self.assertEqual(redirect_rows[0]["target"], "0x300")
        replay_rows = queries.fetch_replay_event_rows(conn, 0, top=5)
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

    def test_commit_width_ignores_non_commit_rows(self):
        with sqlite3.connect(self.db) as conn:
            insert_tip_row(
                conn,
                STATE=2,
                STAMP=4,
                COMMITS_ISCOMMIT=0,
                COMMITS_ISWALK=1,
                COMMITS_COMMITVALID_0=1,
                COMMITS_INFO_0_DEBUG_PC=0x400,
            )
            rows = queries.fetch_commit_width_summary(conn, 0)

        self.assertEqual(
            {row["commit_width"]: row["count"] for row in rows},
            {1: 1, 2: 1},
        )

    def test_replay_aggregation_counts_sticky_entries_once(self):
        with sqlite3.connect(self.db) as conn:
            for stamp, replay_cnt, is_commit in [(4, 1, 0), (5, 2, 0), (6, 2, 1)]:
                insert_tip_row(
                    conn,
                    STATE=1,
                    STAMP=stamp,
                    COMMITS_ISCOMMIT=is_commit,
                    COMMITS_COMMITVALID_0=is_commit,
                    COMMITS_INFO_0_DEBUG_PC=0x400,
                    DEBUGLSINFO_S3_ISREPLAY=1,
                    DEBUGLSINFO_S3_ISREPLAYFAST=1,
                    DEBUGLSINFO_S2_ISDCACHEFIRSTMISS=1,
                    DEBUGLSINFO_REPLAYCNT=replay_cnt,
                )
            for stamp, is_commit in [(7, 0), (8, 1)]:
                insert_tip_row(
                    conn,
                    STATE=1,
                    STAMP=stamp,
                    COMMITS_ISCOMMIT=is_commit,
                    COMMITS_COMMITVALID_0=is_commit,
                    COMMITS_INFO_0_DEBUG_PC=0x400,
                    DEBUGLSINFO_S3_ISREPLAY=1,
                    DEBUGLSINFO_S3_ISREPLAYFAST=1,
                    DEBUGLSINFO_REPLAYCNT=1,
                )
            rows = queries.fetch_replay_event_rows(conn, 0, top=None)

        replay = next(row for row in rows if row["pc"] == "0x400")
        self.assertEqual(replay["count"], 2)
        self.assertEqual(replay["fast_replay"], 2)
        self.assertEqual(replay["dcache_first_miss"], 1)
        self.assertEqual(replay["replay_cnt_sum"], 3)

    def test_replay_keeps_walk_residence_and_ignores_invalid_heads(self):
        with sqlite3.connect(self.db) as conn:
            conn.execute("delete from Tip_0")
            for stamp, state, valid, committed in [
                (4, 1, 1, 0),
                (5, 2, 1, 0),
                (6, 0, 1, 1),
                (7, 3, 1, 0),
                (8, 1, 0, 0),
            ]:
                insert_tip_row(
                    conn,
                    STAMP=stamp,
                    STATE=state,
                    COMMITS_INFO_0_COMMIT_V=valid,
                    COMMITS_ISCOMMIT=committed,
                    COMMITS_COMMITVALID_0=committed,
                    COMMITS_INFO_0_DEBUG_PC=0x400,
                    DEBUGLSINFO_S3_ISREPLAY=1,
                    DEBUGLSINFO_REPLAYCNT=2,
                )
            summary = queries.fetch_replay_summary(conn, 0)[0]
            hotspots = queries.fetch_replay_event_rows(conn, 0)

        self.assertEqual(summary["entries"], 1)
        self.assertEqual(summary["replay_cnt_sum"], 2)
        self.assertEqual(hotspots[0]["pc"], "0x400")
        self.assertEqual(hotspots[0]["count"], 1)

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
    def test_elf_resolver_sign_extends_truncated_kernel_pc(self, run_mock):
        run_mock.side_effect = [
            Mock(stdout="ffffffff80001000 00000010 T kernel_entry\n", returncode=0),
            Mock(stdout="kernel_entry\nkernel/entry.S:42\n", returncode=0),
        ]

        from scripts.tip import symbolize

        resolver = symbolize.ElfResolver(Path("/tmp/vmlinux"), pc_bits=39, pc_mode="sign")
        result = resolver.resolve_many(["0x7f80001000"])

        self.assertEqual(result["0x7f80001000"]["pc"], "0x7f80001000")
        self.assertEqual(result["0x7f80001000"]["function"], "kernel_entry")
        self.assertEqual(run_mock.call_args_list[1].args[0][-1], "0xffffffff80001000")

    def test_pc_lookup_zero_extends_unsigned_address(self):
        from scripts.tip import symbolize

        self.assertEqual(symbolize._lookup_pc("0x7f80001000", 39, "zero"), "0x7f80001000")

    @patch("scripts.tip.symbolize.subprocess.run")
    def test_elf_resolver_preserves_addr2line_discriminator(self, run_mock):
        run_mock.side_effect = [
            Mock(stdout="00000100 00000010 T optimized\n", returncode=0),
            Mock(stdout="optimized\nsrc/foo.c:42 (discriminator 3)\n", returncode=0),
        ]

        from scripts.tip import symbolize

        result = symbolize.ElfResolver(Path("/tmp/program.elf")).resolve_many(["0x100"])

        self.assertEqual(result["0x100"]["line"], 42)
        self.assertEqual(result["0x100"]["location"], "src/foo.c:42 (discriminator 3)")

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

    @patch("scripts.tip.symbolize.subprocess.run")
    def test_elf_resolver_cache_isolated_by_elf_identity(self, run_mock):
        elf_a = self.base / "a.elf"
        elf_b = self.base / "b.elf"
        cache = self.base / "symbols.json"
        elf_a.write_bytes(b"elf-a")
        elf_b.write_bytes(b"elf-b")
        run_mock.side_effect = [
            Mock(stdout="0000000000000100 T from_a\n", returncode=0),
            Mock(stdout="from_a\na.c:1\n", returncode=0),
            Mock(stdout="0000000000000100 T from_b\n", returncode=0),
            Mock(stdout="from_b\nb.c:2\n", returncode=0),
        ]

        from scripts.tip import symbolize

        result_a = symbolize.ElfResolver(elf_a, cache_path=cache).resolve_many(["0x100"])
        result_b = symbolize.ElfResolver(elf_b, cache_path=cache).resolve_many(["0x100"])

        self.assertEqual(result_a["0x100"]["function"], "from_a")
        self.assertEqual(result_b["0x100"]["function"], "from_b")
        self.assertEqual(run_mock.call_count, 4)

    @patch("scripts.tip.symbolize.subprocess.run")
    def test_elf_resolver_cache_isolated_by_pc_mode(self, run_mock):
        elf = self.base / "program.elf"
        cache = self.base / "symbols.json"
        elf.write_bytes(b"elf")
        run_mock.side_effect = [
            Mock(stdout="ffffffff80001000 00000010 T signed_pc\n", returncode=0),
            Mock(stdout="signed_pc\nsigned.c:1\n", returncode=0),
            Mock(stdout="0000007f80001000 00000010 T unsigned_pc\n", returncode=0),
            Mock(stdout="unsigned_pc\nunsigned.c:2\n", returncode=0),
        ]

        from scripts.tip import symbolize

        signed = symbolize.ElfResolver(
            elf, cache_path=cache, pc_bits=39, pc_mode="sign"
        ).resolve_many(["0x7f80001000"])
        unsigned = symbolize.ElfResolver(
            elf, cache_path=cache, pc_bits=39, pc_mode="zero"
        ).resolve_many(["0x7f80001000"])

        self.assertEqual(signed["0x7f80001000"]["function"], "signed_pc")
        self.assertEqual(unsigned["0x7f80001000"]["function"], "unsigned_pc")
        self.assertEqual(run_mock.call_count, 4)

    @patch("scripts.tip.symbolize.subprocess.run")
    def test_elf_resolver_limits_nm_fallback_to_symbol_range(self, run_mock):
        run_mock.side_effect = [
            Mock(stdout="00000100 00000010 T first\n00000200 00000010 T second\n"
                        "00000300 00000004 D data\n00000400 T size_less\n"),
            Mock(stdout="??\n??:0\n" * 6),
        ]
        from scripts.tip import symbolize

        resolved = symbolize.ElfResolver(self.base / "program.elf").resolve_many(
            ["0x104", "0x180", "0x300", "0x400", "0x404", "0x80000000"]
        )
        self.assertEqual(resolved["0x104"]["function"], "first")
        self.assertEqual(resolved["0x104"]["symbol_offset"], "0x4")
        self.assertEqual(resolved["0x400"]["function"], "size_less")
        for pc in ("0x180", "0x300", "0x404", "0x80000000"):
            self.assertEqual(resolved[pc]["function"], "<unknown>")

    def test_cli_options_follow_subcommand(self):
        parser = tip.build_parser()
        args = parser.parse_args([
            "symbols", "--db", str(self.db), "--hart", "1", "--top", "3",
            "--elf", "/tmp/program.elf", "--pc-bits", "48", "--pc-mode", "zero",
        ])
        self.assertEqual(args.db, str(self.db))
        self.assertEqual(args.hart, 1)
        self.assertEqual(args.top, 3)
        self.assertEqual(args.elf, "/tmp/program.elf")
        self.assertEqual(args.pc_bits, 48)
        self.assertEqual(args.pc_mode, "zero")
        with patch("sys.stderr", new=io.StringIO()):
            with self.assertRaises(SystemExit) as error:
                parser.parse_args(["--db", str(self.db), "summary"])
        self.assertEqual(error.exception.code, 2)

    def test_default_paths_are_user_owned(self):
        state_home = self.base / "state"
        cache_home = self.base / "cache"
        with patch.dict(
            os.environ,
            {"XDG_STATE_HOME": str(state_home), "XDG_CACHE_HOME": str(cache_home)},
        ):
            args = tip.build_parser().parse_args(["summary"])

        self.assertEqual(Path(args.out_dir), state_home / "xiangshan" / "tip")
        self.assertEqual(Path(args.sym_cache), cache_home / "xiangshan" / "tip" / "symbols.json")

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
            "--pc-bits",
            "48",
            "--pc-mode",
            "zero",
        ])
        self.assertEqual(rc, 0)
        resolver_ctor.assert_called_once_with(
            Path("/tmp/program.elf"),
            cache_path=cache_path,
            pc_bits=48,
            pc_mode="zero",
        )
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


if __name__ == "__main__":
    unittest.main()
