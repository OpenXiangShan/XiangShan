"""Exercise a real table/native merge with an event above the old eight-slot limit."""
from pathlib import Path
import sqlite3
import tempfile
import unittest

from tea_dip_sc_closed_guest import OUTCOMES, trace_phases
from tea_dip_sc_table_selection import FAMILIES, generated_sc_tables, selected_tables


class ScTableSelectionTests(unittest.TestCase):
    def test_native_cohort_includes_an_event_in_slot_nine(self):
        with sqlite3.connect(':memory:') as db:
            db.row_factory = sqlite3.Row
            fields = ['ID', 'STAMP', 'USESC', *OUTCOMES]
            for i in range(10):
                db.execute('CREATE TABLE scCondTrace_' + str(i) + ' (' +
                           ','.join(name + ' INT' for name in fields) + ')')
            db.execute('INSERT INTO scCondTrace_9 VALUES (1,10,1,1,0,0,0)')
            native = {'Top.sc.diag_sc_cond_trace_events': [1], 'Top.sc.t1_use_sc0': [1]}
            native.update({'Top.sc.' + name + '0': [int(name == 'sc_correct_tage_wrong')]
                           for name in OUTCOMES.values()})
            phases = trace_phases(db, [(20, native)])
            self.assertEqual(phases[0]['trace_events'], 1)
            self.assertEqual(phases[0]['selected_raw_provider_outcomes']['useSc'], 1)

    def test_generated_table_list_keeps_all_ten_slots_and_all_families(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'chisel_db.cpp'
            expected = [prefix + str(i) for prefix in FAMILIES for i in range(10)]
            path.write_text('\n'.join('void init_db_' + name + '() {}' for name in expected))
            self.assertEqual(generated_sc_tables(path), expected)
            path.write_text(path.read_text().replace('void init_db_scMatchedConditional_9() {}', ''))
            with self.assertRaises(AssertionError):
                generated_sc_tables(path)

    def test_selected_table_holes_are_rejected_and_numeric_order_is_retained(self):
        names = ['other', *['scCondTrace_' + str(i) for i in reversed(range(10))]]
        self.assertEqual(selected_tables(names, 'scCondTrace_'), ['scCondTrace_' + str(i) for i in range(10)])
        with self.assertRaises(AssertionError):
            selected_tables(['scCondTrace_0', 'scCondTrace_9'], 'scCondTrace_')


if __name__ == '__main__':
    unittest.main()
