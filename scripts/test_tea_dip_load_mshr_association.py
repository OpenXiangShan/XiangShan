"""Protect against realistic ROB reuse, fused micro-ops and MSHR edge aliases."""
import unittest
from tea_dip_load_mshr_association import live_owners, target_writebacks


class AssociationTests(unittest.TestCase):
    def test_reused_ROB_member_uses_target_lifetime_not_nearest_writeback(self):
        target = dict(TARGET_ALLOCATIONCYCLE=100, TARGET_ROBINDEX=4, TARGET_RANK=1)
        final = dict(CYCLE=200)
        def wb(stamp, uop=0):
            return dict(STAMP=stamp, ROBIDX=4, WBTEARANK=1, SLOTTEARANK=1,
                        WBROBFLAG=0, SLOTROBFLAG=0, WBUOPIDX=uop, SLOTUOPIDX=0)
        right = wb(150)
        rows = [wb(99), right, wb(201), wb(160, uop=1)]
        self.assertEqual(target_writebacks(target, final, rows), [right])

    def test_multiple_representative_uops_remain_ambiguous(self):
        target = dict(TARGET_ALLOCATIONCYCLE=100, TARGET_ROBINDEX=4, TARGET_RANK=1)
        rows = [dict(STAMP=cycle, ROBIDX=4, WBTEARANK=1, SLOTTEARANK=1,
                     WBROBFLAG=0, SLOTROBFLAG=0, WBUOPIDX=0, SLOTUOPIDX=0)
                for cycle in [150, 160]]
        self.assertEqual(len(target_writebacks(target, dict(CYCLE=200), rows)), 2)

    def test_mshr_state_is_before_edge_and_other_bank_does_not_alias(self):
        address = 0x80000140
        block = address >> 6
        old = dict(start=90, end=100)
        new = dict(start=100, end=None)
        episodes = {(block & 3, block >> 2): [old, new]}
        self.assertEqual(live_owners(episodes, address, 100), [old])
        self.assertEqual(live_owners(episodes, address, 101), [new])
        self.assertEqual(live_owners(episodes, address + 64, 100), [])


if __name__ == '__main__':
    unittest.main()
