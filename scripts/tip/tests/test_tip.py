import csv
import io
import json
import os
import sqlite3
import tempfile
import unittest
from contextlib import redirect_stdout
from decimal import Decimal
from pathlib import Path
from unittest.mock import Mock, patch

from scripts.tip import plot, queries, tip


TIP_ATTRIBUTION_SCHEMA = ",\n".join(
    f"  COMMITPCS_{index} int not null,\n  COMMITVALID_{index} int not null"
    for index in range(64)
)

TIP_SCHEMA = f"""
create table Tip_0(
  ID integer primary key autoincrement,
  STATE int not null,
  STAMP int not null,
  SITE text,
{TIP_ATTRIBUTION_SCHEMA},
  ENTRYID int not null,
  ENTRYGENERATION int not null,
  ENTRYVALID int not null,
  FIRSTARCHPC int not null,
  FIRSTARCHVALID int not null,
  STALLPC int not null,
  STALLVALID int not null,
  OFFENDINGPC int not null,
  OFFENDINGVALID int not null,
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

def insert_tip_row(conn: sqlite3.Connection, **values):
    columns = [column[1] for column in conn.execute("pragma table_info(Tip_0)") if column[1] != "ID"]
    row = dict.fromkeys(columns, 0)
    row.update(SITE="", COMMITS_INFO_0_COMMIT_V=1)
    row.update(values)
    if "ENTRYVALID" not in values:
        row["ENTRYVALID"] = int(values.get("STATE", 0) != 3)
    if "FIRSTARCHPC" not in values:
        row["FIRSTARCHPC"] = values.get(
            "COMMITPCS_0",
            values.get("COMMITS_INFO_0_DEBUG_PC", values.get("STALLPC", 0)),
        )
    if "FIRSTARCHVALID" not in values:
        row["FIRSTARCHVALID"] = row["ENTRYVALID"]
    if "STALLPC" not in values:
        row["STALLPC"] = row["FIRSTARCHPC"]
    if "STALLVALID" not in values:
        row["STALLVALID"] = row["ENTRYVALID"]
    placeholders = ", ".join(["?"] * len(columns))
    column_list = ", ".join(columns)
    conn.execute(
        f"insert into Tip_0({column_list}) values ({placeholders})",
        tuple(row[column] for column in columns),
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
            COMMITPCS_0=0x100,
            COMMITVALID_0=1,
            COMMITPCS_1=0x104,
            COMMITVALID_1=1,
            STALLPC=0x100,
            STALLVALID=1,
            COMMITS_INFO_0_DEBUG_PC=0x100,
            COMMITS_INFO_1_DEBUG_PC=0x104,
            COMMITS_INFO_0_COMMITTYPE=2,
            COMMITS_INFO_1_COMMITTYPE=1,
        )
        insert_tip_row(
            conn,
            STATE=1,
            STAMP=1,
            STALLPC=0x100,
            STALLVALID=1,
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
            OFFENDINGPC=0x200,
            OFFENDINGVALID=1,
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
            COMMITPCS_0=0x108,
            COMMITVALID_0=1,
            STALLPC=0x108,
            STALLVALID=1,
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
        self.assertEqual(state_counts["flushed"], 1)
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

    def test_time_profile_attributes_every_cycle(self):
        with sqlite3.connect(self.db) as conn:
            rows, summary = queries.fetch_time_profile(conn, 0)

        by_pc = {row["pc"]: row for row in rows}
        self.assertEqual(by_pc["0x100"]["cycles"], 1.5)
        self.assertEqual(by_pc["0x100"]["computing_cycles"], 0.5)
        self.assertEqual(by_pc["0x100"]["stalled_cycles"], 1.0)
        self.assertEqual(by_pc["0x104"]["cycles"], 0.5)
        self.assertEqual(by_pc["0x200"]["flushed_cycles"], 1.0)
        self.assertEqual(by_pc["0x108"]["computing_cycles"], 1.0)
        self.assertEqual(summary["total_cycles"], 4)
        self.assertEqual(summary["attributed_cycles"], 4.0)
        self.assertEqual(summary["unattributed_cycles"], 0)
        self.assertEqual(summary["coverage_percent"], 100.0)

    def test_time_profile_assigns_drained_cycles_to_next_head(self):
        with sqlite3.connect(self.db) as conn:
            conn.execute("delete from Tip_0")
            insert_tip_row(conn, STATE=3, STAMP=0, COMMITS_INFO_0_COMMIT_V=0)
            insert_tip_row(conn, STATE=3, STAMP=1, COMMITS_INFO_0_COMMIT_V=0)
            insert_tip_row(
                conn,
                STATE=1,
                STAMP=2,
                FIRSTARCHPC=0x300,
                FIRSTARCHVALID=1,
                STALLPC=0x304,
                STALLVALID=1,
            )
            rows, summary = queries.fetch_time_profile(conn, 0)

        by_pc = {row["pc"]: row for row in rows}
        self.assertEqual(by_pc["0x300"]["drained_cycles"], 2.0)
        self.assertEqual(by_pc["0x304"]["stalled_cycles"], 1.0)
        self.assertEqual(summary["attributed_cycles"], 3.0)

    def test_time_profile_decimal_output_conserves_three_way_commit_cycle(self):
        with sqlite3.connect(self.db) as conn:
            conn.execute("delete from Tip_0")
            insert_tip_row(
                conn,
                STATE=0,
                STAMP=0,
                COMMITPCS_0=0x100,
                COMMITVALID_0=1,
                COMMITPCS_1=0x104,
                COMMITVALID_1=1,
                COMMITPCS_2=0x108,
                COMMITVALID_2=1,
                STALLPC=0x100,
                STALLVALID=1,
            )
            rows, summary = queries.fetch_time_profile(conn, 0)

        self.assertEqual(sum((row["cycles"] for row in rows), Decimal(0)), Decimal(1))
        self.assertEqual(sum((row["computing_cycles"] for row in rows), Decimal(0)), Decimal(1))
        self.assertEqual(summary["attributed_cycles"], 1.0)

    def test_architectural_commit_width_uses_tip_valids(self):
        with sqlite3.connect(self.db) as conn:
            conn.execute("delete from Tip_0")
            insert_tip_row(
                conn,
                STATE=0,
                STAMP=0,
                COMMITS_ISCOMMIT=1,
                COMMITS_COMMITVALID_0=1,
                COMMITPCS_0=0x100,
                COMMITVALID_0=1,
                COMMITPCS_1=0x104,
                COMMITVALID_1=1,
                COMMITPCS_2=0x108,
                COMMITVALID_2=1,
                STALLPC=0x100,
                STALLVALID=1,
            )
            rows = queries.fetch_commit_width_summary(conn, 0)

        self.assertEqual(rows, [{"commit_width": 3, "count": 1}])

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
                    ENTRYID=1,
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

    def test_replay_does_not_split_when_dynamic_stall_pc_changes(self):
        with sqlite3.connect(self.db) as conn:
            conn.execute("delete from Tip_0")
            for stamp, pc in [(0, 0x100), (1, 0x104)]:
                insert_tip_row(
                    conn,
                    STATE=1,
                    STAMP=stamp,
                    ENTRYID=7,
                    FIRSTARCHPC=0x100,
                    FIRSTARCHVALID=1,
                    STALLPC=pc,
                    STALLVALID=1,
                    DEBUGLSINFO_S3_ISREPLAY=1,
                    DEBUGLSINFO_REPLAYCNT=3,
                )
            rows = queries.fetch_replay_event_rows(conn, 0, top=None)

        self.assertEqual(rows, [{
            "pc_raw": 0x100,
            "pc": "0x100",
            "count": 1,
            "fast_replay": 0,
            "slow_replay": 0,
            "rs_replay": 0,
            "bank_conflict": 0,
            "dcache_first_miss": 0,
            "forward_fail": 0,
            "replay_cnt_sum": 3,
        }])

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
                    ENTRYVALID=valid,
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
    def test_elf_resolver_does_not_cache_tool_failures(self, run_mock):
        elf = self.base / "program.elf"
        cache = self.base / "symbols.json"
        elf.write_bytes(b"elf")
        run_mock.side_effect = FileNotFoundError()

        from scripts.tip import symbolize

        stderr = io.StringIO()
        with patch("sys.stderr", new=stderr):
            failed = symbolize.ElfResolver(elf, cache_path=cache).resolve_many(["0x100"])
        self.assertEqual(failed["0x100"]["function"], "<unknown>")
        self.assertIn("not cached", stderr.getvalue())

        run_mock.reset_mock()
        run_mock.side_effect = [
            Mock(stdout="00000100 00000010 T recovered\n", returncode=0),
            Mock(stdout="recovered\nsrc/recovered.c:7\n", returncode=0),
        ]
        recovered = symbolize.ElfResolver(elf, cache_path=cache).resolve_many(["0x100"])

        self.assertEqual(recovered["0x100"]["function"], "recovered")
        self.assertEqual(run_mock.call_count, 2)

    @patch("scripts.tip.symbolize.subprocess.run")
    def test_elf_resolver_caches_successful_unknown_result(self, run_mock):
        elf = self.base / "program.elf"
        cache = self.base / "symbols.json"
        elf.write_bytes(b"elf")
        run_mock.side_effect = [
            Mock(stdout="", returncode=0),
            Mock(stdout="??\n??:0\n", returncode=0),
        ]

        from scripts.tip import symbolize

        first = symbolize.ElfResolver(elf, cache_path=cache).resolve_many(["0x100"])
        run_mock.reset_mock()
        second = symbolize.ElfResolver(elf, cache_path=cache).resolve_many(["0x100"])

        self.assertEqual(first["0x100"]["function"], "<unknown>")
        self.assertEqual(second["0x100"]["function"], "<unknown>")
        run_mock.assert_not_called()

    @patch("scripts.tip.symbolize.subprocess.run")
    def test_elf_resolver_ignores_pre_failure_fix_cache(self, run_mock):
        elf = self.base / "program.elf"
        cache = self.base / "symbols.json"
        elf.write_bytes(b"elf")

        from scripts.tip import symbolize

        resolver = symbolize.ElfResolver(elf)
        old_identity = f"{resolver._elf_identity}|pc_bits=39|pc_mode=sign"
        cache.write_text(json.dumps({
            "version": 3,
            "elfs": {old_identity: {"0x100": {"function": "<unknown>"}}},
        }), encoding="utf-8")
        run_mock.side_effect = [
            Mock(stdout="00000100 00000010 T recovered\n", returncode=0),
            Mock(stdout="recovered\nsrc/recovered.c:7\n", returncode=0),
        ]

        recovered = symbolize.ElfResolver(elf, cache_path=cache).resolve_many(["0x100"])

        self.assertEqual(recovered["0x100"]["function"], "recovered")
        self.assertEqual(run_mock.call_count, 2)

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

        self.assertIsNone(args.out_dir)
        self.assertEqual(Path(args.sym_cache), cache_home / "xiangshan" / "tip" / "symbols.json")

    def test_default_output_dir_isolated_by_database_and_hart(self):
        state_home = self.base / "state"
        db_a = self.base / "a" / "tip.db"
        db_b = self.base / "b" / "tip.db"
        with patch.dict(os.environ, {"XDG_STATE_HOME": str(state_home)}):
            a_hart0 = tip.default_out_dir(db_a, 0)
            b_hart0 = tip.default_out_dir(db_b, 0)
            a_hart1 = tip.default_out_dir(db_a, 1)
            a_hart0_again = tip.default_out_dir(db_a, 0)

        self.assertNotEqual(a_hart0, b_hart0)
        self.assertNotEqual(a_hart0, a_hart1)
        self.assertEqual(a_hart0, a_hart0_again)
        self.assertEqual(a_hart0.parent.parent, state_home / "xiangshan" / "tip")

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
        time_rows = [{"pc": "0x100", "cycles": 2.5}, {"pc": "0x108", "cycles": 1.5}]
        redirect_rows = [{"target": "0x300", "count": 1}]
        plot.plot_state_distribution(state_rows, out_dir / "state.png")
        plot.plot_commit_width_distribution(commit_rows, out_dir / "commit.png")
        plot.plot_pc_hotspots(pc_rows, out_dir / "pc.png")
        plot.plot_time_profile(time_rows, out_dir / "time.png")
        plot.plot_redirect_targets(redirect_rows, out_dir / "redirect.png")
        for name in ["state.png", "commit.png", "pc.png", "time.png", "redirect.png"]:
            path = out_dir / name
            self.assertTrue(path.exists(), name)
            self.assertGreater(path.stat().st_size, 0, name)

    @patch("scripts.tip.symbolize.ElfResolver")
    def test_cli_all_generates_outputs(self, resolver_ctor):
        out_dir = self.base / "results"
        out_dir.mkdir()
        (out_dir / "symbolized_commits.csv").write_text("stale", encoding="utf-8")
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
            "time_profile.csv",
            "tip_attribution_summary.csv",
            "pc_hotspots.csv",
            "redirect_targets.csv",
            "replay_hotspots.csv",
            "state_distribution.png",
            "top_time_profile.png",
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
        symbol_path = out_dir / "symbolized_time_profile.csv"
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
        self.assertTrue((out_dir / "symbolized_time_profile.csv").exists())


if __name__ == "__main__":
    unittest.main()
