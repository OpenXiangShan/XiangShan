import copy
import unittest

from tea_dip_l2_merge_trace import attach_merges


def row(cycle, valid=0, merge=0, source=0, opcode=0, line=0, bank=0, slot=0):
    return dict(core_cycle=cycle,kind=1,bank=bank,mshr=slot,valid=valid,line=line,
                merge_active=merge,merge_source_id=source,merge_opcode=opcode)


def snapshots():
    return [row(0,bank=bank,slot=slot) for bank in range(4) for slot in range(16)]


def owner(begin,end):
    return dict(bank=0,slot=0,begin=begin,end=end,line=0x1000)


class MergeIdentity(unittest.TestCase):
    def test_same_block_slot_reuse_does_not_inherit_merged_source(self):
        owners=[owner(1,4),owner(5,8)]
        rows=snapshots()+[row(1,1,line=0x1000),row(2,1,1,2,6,0x1000),row(4),
                          row(5,1,line=0x1000),row(8)]
        self.assertEqual(attach_merges(rows,owners),1)
        self.assertEqual(owners[0]['merged_demand'],dict(source_id=2,opcode=6,first_observed=2,transition_observed=True))
        self.assertIsNone(owners[1]['merged_demand'])

    def test_same_time_wrong_block_rejected(self):
        rows=snapshots()+[row(1,1,line=0x1000),row(2,1,1,2,6,0x2000)]
        with self.assertRaisesRegex(AssertionError,'different block'):
            attach_merges(rows,[owner(1,4)])

    def test_saved_source_cannot_change_within_merged_generation(self):
        rows=snapshots()+[row(1,1,line=0x1000),row(2,1,1,2,6,0x1000),row(3,1,1,3,6,0x1000)]
        with self.assertRaisesRegex(AssertionError,'identity changed'):
            attach_merges(rows,[owner(1,4)])

    def test_initially_merged_snapshot_has_unknown_acceptance_edge(self):
        rows=snapshots();rows[0]=row(0,1,1,2,6,0x1000)
        owners=[owner(0,4)]
        self.assertEqual(attach_merges(copy.deepcopy(rows),owners),1)
        self.assertFalse(owners[0]['merged_demand']['transition_observed'])


if __name__=='__main__':
    unittest.main()
