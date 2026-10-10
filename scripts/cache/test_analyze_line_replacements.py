import sqlite3
import unittest

from analyze_line_replacements import analyze


class LineReplacementAnalysisTest(unittest.TestCase):
    def setUp(self):
        self.db = sqlite3.connect(":memory:")
        self.db.execute(
            "CREATE TABLE L2LineReplacementTrace_hart0_slice1 "
            "(STAMP INTEGER, LINEADDR INTEGER, SETIDX INTEGER)"
        )
        self.db.executemany(
            "INSERT INTO L2LineReplacementTrace_hart0_slice1 VALUES (?, ?, ?)",
            [(10, 0x1040, 3), (11, 0x1040, 3), (12, 0x1080, 3), (13, 0x2000, 7)],
        )
        self.db.execute(
            "CREATE TABLE ZJ_LLC_Lifecycle_bank0 "
            "(STAMP INTEGER, HNFBANK INTEGER, DIRBANK INTEGER, SETIDX INTEGER, "
            "LINEADDR INTEGER, EVENTTYPE INTEGER, VICTIMVALID INTEGER)"
        )
        self.db.executemany(
            "INSERT INTO ZJ_LLC_Lifecycle_bank0 VALUES (?, ?, ?, ?, ?, ?, ?)",
            [
                (10, 0, 0, 5, 0x3000, 2, 1),
                (11, 0, 0, 5, 0x3000, 2, 1),
                (12, 1, 0, 5, 0x3000, 2, 1),
                (13, 0, 0, 5, 0x3040, 2, 0),
                (14, 0, 0, 5, 0x3080, 1, 1),
            ],
        )

    def tearDown(self):
        self.db.close()

    def test_repeated_lines_are_scoped_by_cache_bank(self):
        results = {result["layer"]: result for result in analyze(self.db, top=5)}
        l2 = results["L2"]
        self.assertEqual((l2["total"], l2["unique_lines"], l2["repeated_lines"]), (4, 3, 1))
        self.assertEqual(l2["window_cycles"], 4)
        self.assertEqual(l2["top_lines"][0]["per_million_cycles"], 500000.0)
        self.assertEqual((l2["top_lines"][0]["line_addr"], l2["top_lines"][0]["count"]), (0x1040, 2))
        self.assertEqual((l2["top_sets"][0]["set_idx"], l2["top_sets"][0]["count"]), (3, 3))

        l3 = results["L3"]
        self.assertEqual((l3["total"], l3["unique_lines"], l3["repeated_lines"]), (3, 2, 1))
        self.assertEqual((l3["top_lines"][0]["hnf_bank"], l3["top_lines"][0]["count"]), (0, 2))

    def test_cycle_window_filters_both_levels(self):
        results = {result["layer"]: result for result in analyze(self.db, start=11, end=12, top=1)}
        self.assertEqual(results["L2"]["total"], 2)
        self.assertEqual(results["L3"]["total"], 2)
        self.assertEqual(results["L3"]["repeated_lines"], 0)
        self.assertEqual(results["L3"]["window_cycles"], 2)


if __name__ == "__main__":
    unittest.main()
