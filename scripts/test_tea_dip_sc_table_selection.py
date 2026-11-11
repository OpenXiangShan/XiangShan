"""Lock the actual emitted8/8/10 family widths and reject incomplete generated lists."""
from pathlib import Path
import tempfile
import unittest

from tea_dip_sc_table_selection import FAMILIES, generated_sc_tables


class ScTableSelectionTests(unittest.TestCase):
    def test_prediction_slots_eight_and_nine_are_not_dropped(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'chisel_db.cpp'
            expected = [prefix + str(i) for prefix, width in zip(FAMILIES, [8, 8, 10]) for i in range(width)]
            path.write_text('\n'.join('void init_db_' + name + '() {}' for name in expected))
            self.assertEqual(generated_sc_tables(path), expected)
            old_selection = {prefix + str(i) for prefix in FAMILIES for i in range(8)}
            self.assertEqual(set(generated_sc_tables(path)) - old_selection,
                             {'scPredictionOpposition_8', 'scPredictionOpposition_9'})

    def test_missing_family_or_interior_slot_is_not_silently_ignored(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'chisel_db.cpp'
            names = [prefix + str(i) for prefix, width in zip(FAMILIES, [8, 8, 10]) for i in range(width)]
            for excluded in [{'scPredictionOpposition_3'}, set(n for n in names if n.startswith('scCondTrace_'))]:
                path.write_text('\n'.join('void init_db_' + name + '() {}' for name in names if name not in excluded))
                with self.assertRaises(AssertionError):
                    generated_sc_tables(path)


if __name__ == '__main__':
    unittest.main()
