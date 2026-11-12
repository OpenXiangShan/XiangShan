import sqlite3
import tempfile
import unittest
from pathlib import Path

from tea_dip_clock_alignment import core_native_alignment


class ClockAlignmentTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.native = Path(self.tmp.name) / 'native.txt'
        self.db = sqlite3.connect(':memory:')
        self.db.row_factory = sqlite3.Row

    def tearDown(self):
        self.db.close()
        self.tmp.cleanup()

    def native_frames(self, frames):
        self.native.write_text(''.join(
            f'[PERF ][time= {stamp}] core.rob: {name}, {value}\n'
            for stamp, fields in frames for name, value in fields.items()))

    def summaries(self, rows):
        self.db.execute('CREATE TABLE TeaCycleSummary_0(ID INT,CYCLE INT,STAMP INT,CLEARED INT,CLOSED INT,RETIRED INT,STATES_0 INT,STATES_1 INT,STATES_2 INT,STATES_3 INT)')
        self.db.executemany('INSERT INTO TeaCycleSummary_0 VALUES(?,?,?,?,?,?,?,?,?,?)', rows)

    def test_exact_dump_prefix_uses_exclusive_end_and_nonzero_reset_offset(self):
        self.native_frames([(103, {'tea_exact_endpoint_cycle': 101}), (203, {'tea_exact_endpoint_cycle': 201})])
        self.assertEqual(core_native_alignment(self.db, self.native)['core_to_native_offset'], 3)

    def test_zero_offset_is_measured(self):
        self.native_frames([(103, {'tea_exact_endpoint_cycle': 104})])
        self.assertEqual(core_native_alignment(self.db, self.native)['core_to_native_offset'], 0)

    def test_changed_offset_is_rejected(self):
        self.native_frames([(103, {'tea_exact_endpoint_cycle': 101}), (203, {'tea_exact_endpoint_cycle': 200})])
        with self.assertRaisesRegex(AssertionError, 'origin changes'):
            core_native_alignment(self.db, self.native)

    def test_clear_dump_roi_aligns_both_collection_edges(self):
        self.native_frames([(102, {'clock_cycle': 103}), (202, {'clock_cycle': 100})])
        self.summaries([(1, 101, 100, 0, 0, 0, 0, 1, 0, 0), (2, 200, 199, 0, 0, 10, 10, 90, 0, 0)])
        data = core_native_alignment(self.db, self.native)
        self.assertEqual(data['core_to_native_offset'], 3)
        self.assertEqual(len(data['anchors']), 2)

    def test_continuous_capture_uses_actual_terminal_dump(self):
        self.native_frames([(102, {'clock_cycle': 103})])
        self.summaries([(1, 1, 0, 0, 0, 0, 0, 0, 0, 1), (2, 100, 99, 0, 0, 10, 10, 89, 0, 1)])
        self.assertEqual(core_native_alignment(self.db, self.native)['core_to_native_offset'], 3)

    def test_own_stamp_equality_without_anchor_is_insufficient(self):
        self.native_frames([(102, {'clock_cycle': 103})])
        with self.assertRaisesRegex(AssertionError, 'No supported same-dump'):
            core_native_alignment(self.db, self.native)


if __name__ == '__main__':
    unittest.main()
