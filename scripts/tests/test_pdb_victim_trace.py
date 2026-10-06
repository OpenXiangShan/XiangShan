import importlib.util
from pathlib import Path
import sqlite3
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "check_pdb_victim_trace", Path(__file__).resolve().parents[1] / "check_pdb_victim_trace.py")
checker = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checker)


class VictimTraceTest(unittest.TestCase):
    def database(self, path):
        base = dict(ID=0, STAMP=0, VICTIM_VALID=0, VICTIM_BITS_BLOCKADDR=0,
                    VICTIM_BITS_USED=0, VICTIM_BITS_STREAM=1, USEDHITS=0, UNUSEDHITS=0,
                    STREAMUSEDHITS=0, STREAMUNUSEDHITS=0, DUPLICATE=0, OCCUPANCY=0,
                    EVICTION_VALID=0, EVICTION_BITS_BLOCKADDR=0,
                    EVICTION_BITS_USED=0, EVICTION_BITS_STREAM=0)
        for i in range(3):
            base[f"DEMAND_{i}_VALID"] = 0
            base[f"DEMAND_{i}_BITS"] = 0
        # Deliberately hand-authored evidence, independent of the checker's model.
        events = [
            dict(VICTIM_VALID=1, VICTIM_BITS_BLOCKADDR=10, OCCUPANCY=1),
            dict(DEMAND_0_VALID=1, DEMAND_0_BITS=10, DEMAND_1_VALID=1, DEMAND_1_BITS=10,
                 UNUSEDHITS=1, STREAMUNUSEDHITS=1, VICTIM_VALID=1,
                 VICTIM_BITS_BLOCKADDR=11, VICTIM_BITS_USED=1, VICTIM_BITS_STREAM=0, OCCUPANCY=1),
            dict(VICTIM_VALID=1, VICTIM_BITS_BLOCKADDR=12, OCCUPANCY=2),
            dict(VICTIM_VALID=1, VICTIM_BITS_BLOCKADDR=13, VICTIM_BITS_USED=1,
                 EVICTION_VALID=1, EVICTION_BITS_BLOCKADDR=11, EVICTION_BITS_USED=1, OCCUPANCY=2),
            dict(DEMAND_0_VALID=1, DEMAND_0_BITS=12, DEMAND_2_VALID=1, DEMAND_2_BITS=13,
                 USEDHITS=1, UNUSEDHITS=1, STREAMUSEDHITS=1, STREAMUNUSEDHITS=1),
            dict(VICTIM_VALID=1, VICTIM_BITS_BLOCKADDR=14, OCCUPANCY=1),
            dict(VICTIM_VALID=1, VICTIM_BITS_BLOCKADDR=14, VICTIM_BITS_USED=1,
                 DUPLICATE=1, OCCUPANCY=1),
            dict(DEMAND_0_VALID=1, DEMAND_0_BITS=14, USEDHITS=1, STREAMUSEDHITS=1),
        ]
        with sqlite3.connect(path) as connection:
            connection.execute("CREATE TABLE PDBVictimObserver0 (" +
                               ",".join(f"{name} INTEGER" for name in base) + ")")
            for i, event in enumerate(events):
                row = dict(base, **event, ID=i + 1, STAMP=2 * i + 5)
                connection.execute("INSERT INTO PDBVictimObserver0 VALUES (" +
                                   ",".join("?" for _ in row) + ")", tuple(row.values()))

    def test_valid_trace(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "trace.db"
            self.database(path)
            result = checker.check_trace(path, entries=2)
            self.assertEqual(result["rows"], 8)
            self.assertEqual(result["usedVictimHits"], 2)
            self.assertEqual(result["unusedVictimHits"], 2)
            self.assertEqual(result["fifo_full_evictions"], 1)
            self.assertEqual(result["duplicate_victims"], 1)
            self.assertEqual(result["final_occupancy"], 0)

    def test_reject_count_or_fifo_corruption(self):
        for assignment in ("USEDHITS=1", "OCCUPANCY=0", "EVICTION_VALID=1"):
            with self.subTest(assignment=assignment), tempfile.TemporaryDirectory() as directory:
                path = Path(directory) / "trace.db"
                self.database(path)
                with sqlite3.connect(path) as connection:
                    connection.execute(f"UPDATE PDBVictimObserver0 SET {assignment} WHERE ID=1")
                with self.assertRaises(ValueError):
                    checker.check_trace(path, entries=2)

    def test_reject_empty_trace(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "trace.db"
            self.database(path)
            with sqlite3.connect(path) as connection:
                connection.execute("DELETE FROM PDBVictimObserver0")
            with self.assertRaisesRegex(ValueError, "empty trace"):
                checker.check_trace(path, entries=2)


if __name__ == "__main__":
    unittest.main()
