"""Regression for inactive CCT sequence IDs and distinct compressed-ROB member ranks."""
from pathlib import Path
import sqlite3
import tempfile
import unittest
from tea_dip_writeback_stage_audit import analyze

COLUMNS = ('ID STAMP ROBIDX WBSEQNUM SLOTSEQNUM SLOTFUTYPE SLOTISFORMER SLOTDISPATCHTIME '
           'WRITEBACKTIME ISSUETIME DISPATCHLATENCY ENQRSLATENCY SELECTLATENCY ISSUELATENCY '
           'WBTEARANK SLOTTEARANK WBROBFLAG SLOTROBFLAG WBUOPIDX SLOTUOPIDX').split()


def record(number, member, representative, slot_dispatch):
    # Timing pattern from a real failing row; rank assignments here are synthetic.
    row = dict.fromkeys(COLUMNS, 0)
    row.update(ID=number, STAMP=15764, ROBIDX=139, SLOTFUTYPE=2, SLOTISFORMER=1,
               SLOTDISPATCHTIME=slot_dispatch, WRITEBACKTIME=15764, ISSUETIME=15761,
               DISPATCHLATENCY=1, SELECTLATENCY=1, ISSUELATENCY=1,
               WBTEARANK=member, SLOTTEARANK=representative)
    return row


def audit(rows):
    with tempfile.TemporaryDirectory() as folder:
        path = Path(folder) / 'trace.db'
        with sqlite3.connect(path) as connection:
            connection.execute('CREATE TABLE InstTable0 (' + ','.join(c + ' INTEGER' for c in COLUMNS) + ')')
            connection.executemany('INSERT INTO InstTable0 VALUES (' + ','.join('?' for c in COLUMNS) + ')',
                                   [[r[c] for c in COLUMNS] for r in rows])
        return analyze(path)


class WritebackIdentityTests(unittest.TestCase):
    def test_zero_CCT_sequence_does_not_alias_different_TEA_members(self):
        result = audit([record(1, 1, 0, 15758), record(2, 0, 0, 15759), record(3, 1, 1, 15759)])
        self.assertEqual(result['counts']['representative_writebacks'], 2)
        self.assertEqual(result['counts']['other_member_writebacks'], 1)
        self.assertTrue(result['producer_ordering_passed'])
        self.assertTrue(result['representative_identity_and_ordering_passed'])

    def test_same_rank_other_uop_cannot_overwrite_first_uop_timeline(self):
        # Same-rank dispatch gaps survived the real GSIM rank repair.
        other = record(1, 0, 0, 15758)
        other['WBUOPIDX'] = 1
        result = audit([other, record(2, 0, 0, 15759), record(3, 1, 1, 15759)])
        self.assertTrue(result['representative_identity_and_ordering_passed'])
        self.assertEqual(result['counts']['representative_writebacks'], 2)
        self.assertEqual(result['counts']['same_member_other_uop_writebacks'], 1)
        self.assertEqual(result['counts']['nonzero_uop_index_writebacks'], 1)

    def test_matching_rank_with_wrong_representative_time_remains_red(self):
        result = audit([record(1, 1, 1, 15758)])
        self.assertEqual(result['counts']['invalid_representative_timestamps'], 1)
        self.assertFalse(result['representative_identity_and_ordering_passed'])

    def test_no_rank_diversity_is_insufficient_and_ROB_generation_is_part_of_identity(self):
        self.assertIsNone(audit([record(1, 0, 0, 15759)])['representative_identity_and_ordering_passed'])
        old_generation = record(1, 1, 1, 15758)
        old_generation['SLOTROBFLAG'] = 1
        result = audit([old_generation, record(2, 1, 1, 15759)])
        self.assertEqual(result['counts']['different_ROB_generation_writebacks'], 1)
        self.assertEqual(result['counts']['representative_writebacks'], 1)
        self.assertTrue(result['representative_identity_and_ordering_passed'])


if __name__ == '__main__':
    unittest.main()
