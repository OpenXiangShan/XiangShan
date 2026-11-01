import unittest
from dip_analyze import analyze, killed


def row(cycle, sampled=True):
    data = {'ROB_CYCLE': cycle, 'ROB_RETIRED': 0, 'ROB_FLUSH_VALID': 0, 'DISPATCH_VALIDMASK': 0, 'SAMPLED': sampled}
    for lane in range(4):
        data[f'ROB_COMMITS_{lane}_VALID'] = 0
        data.update({f'SLOTS_{lane}_STATE': 2, f'SLOTS_{lane}_CAUSE': 0, f'SLOTS_{lane}_TARGETVALID': 0})
    return data


def ptr(data, prefix, index, former=True, flag=False):
    data.update({prefix + '_FLAG': int(flag), prefix + '_VALUE': index, prefix + '_ISFORMER': int(former)})


def sample(data, lane, state, index, pc, cause=0, former=True, flag=False):
    prefix = f'SLOTS_{lane}'
    data.update({prefix + '_STATE': state, prefix + '_CAUSE': cause, prefix + '_TARGETVALID': 1,
                 prefix + '_TARGET_PC': pc})
    ptr(data, prefix + '_TARGET_ROBIDX', index, former, flag)


def arrive(data, index, pc, psv=0, former=True, flag=False):
    data['DISPATCH_VALIDMASK'] = 1
    data['DISPATCH_UOPS_0_PC'] = pc
    data['DISPATCH_UOPS_0_PSV'] = psv
    ptr(data, 'DISPATCH_UOPS_0_ROBIDX', index, former, flag)


def commit(data, index, former=True, flag=False):
    data['ROB_COMMITS_0_VALID'] = 1
    ptr(data, 'ROB_COMMITS_0_BITS', index, former, flag)


class DipAnalysisTest(unittest.TestCase):
    def test_partial_dispatch_frontend_successor_and_final_censoring(self):
        first = row(0)
        sample(first, 0, 0, 0, 0x1000)
        sample(first, 1, 1, 1, 0x1004, cause=1 | 2)
        sample(first, 2, 1, 1, 0x1004, cause=1 | 2)
        sample(first, 3, 1, 1, 0x1004, cause=1 | 2)
        second = row(1)
        commit(second, 0)
        third = row(2, False)
        commit(third, 1)
        arrive(third, 2, 0x1008, psv=3)
        fourth = row(3, False)
        commit(fourth, 2)
        final = row(4)
        profile, summary = analyze([first, second, third, fourth, final], 4, 2)
        self.assertEqual(profile[(0, 0, 0x1000)], 1)
        self.assertEqual(profile[(1, 3, 0x1004)], 3)
        self.assertEqual(profile[(2, 3, 0x1008)], 4)
        self.assertEqual(summary['unresolved_slots'], 4)
        self.assertEqual(summary['sampled_cycles'], 3)
        self.assertEqual(sum(summary['state_cycles'].values()) + summary['unresolved_cycles'], 3)

    def test_wrong_path_all_states_reparent_to_flush_pc_and_ignore_rob_reuse(self):
        first = row(0)
        sample(first, 0, 0, 3, 0x2000)
        sample(first, 1, 1, 3, 0x2004, cause=2)
        # The frontend sample joins the next wrong-path uop before flush.
        second = row(1, False)
        arrive(second, 4, 0x2008)
        third = row(2, False)
        third.update({'ROB_FLUSH_VALID': 1, 'ROB_FLUSH_BITS_PC': 0x1000,
                      'ROB_FLUSH_BITS_CAUSE': 1, 'ROB_FLUSH_BITS_SELF': 0})
        ptr(third, 'ROB_FLUSH_BITS_ROBIDX', 2)
        fourth = row(3, False)
        commit(fourth, 3)
        fifth = row(4, False)
        commit(fifth, 2)
        profile, summary = analyze([first, second, third, fourth, fifth], 4, 2)
        self.assertEqual(dict(profile), {(3, 1, 0x1000): 4})
        self.assertEqual(summary['reclassified_slots'], 4)
        self.assertEqual(summary['unresolved_slots'], 0)

    def test_nested_flush_reparents_misspeculation_to_the_older_branch(self):
        first = row(0)
        for lane in range(4):
            sample(first, lane, 0, 3, 0x3000)
        second = row(1, False)
        second.update({'ROB_FLUSH_VALID': 1, 'ROB_FLUSH_BITS_PC': 0x2000,
                       'ROB_FLUSH_BITS_CAUSE': 1, 'ROB_FLUSH_BITS_SELF': 0})
        ptr(second, 'ROB_FLUSH_BITS_ROBIDX', 2)
        third = row(2, False)
        third.update({'ROB_FLUSH_VALID': 1, 'ROB_FLUSH_BITS_PC': 0x1000,
                      'ROB_FLUSH_BITS_CAUSE': 1, 'ROB_FLUSH_BITS_SELF': 0})
        ptr(third, 'ROB_FLUSH_BITS_ROBIDX', 1)
        fourth = row(3, False)
        commit(fourth, 1)
        profile, summary = analyze([first, second, third, fourth], 4, 2)
        self.assertEqual(dict(profile), {(3, 1, 0x1000): 4})
        self.assertEqual(summary['reclassified_slots'], 4)
        self.assertEqual(summary['unresolved_slots'], 0)

    def test_self_flushed_latter_settles_on_successor_commit_without_rob_alias(self):
        first = row(0)
        for lane in range(4):
            sample(first, lane, 0, 3, 0x3000)
        second = row(1, False)
        second.update({'ROB_FLUSH_VALID': 1, 'ROB_FLUSH_BITS_PC': 0x2000,
                       'ROB_FLUSH_BITS_CAUSE': 2, 'ROB_FLUSH_BITS_SELF': 1})
        ptr(second, 'ROB_FLUSH_BITS_ROBIDX', 2, former=False)
        third = row(2, False)
        commit(third, 3)
        fourth = row(3, False)
        fourth.update({'ROB_FLUSH_VALID': 1, 'ROB_FLUSH_BITS_PC': 0x4000,
                       'ROB_FLUSH_BITS_CAUSE': 1, 'ROB_FLUSH_BITS_SELF': 0})
        ptr(fourth, 'ROB_FLUSH_BITS_ROBIDX', 4, flag=True)
        profile, summary = analyze([first, second, third, fourth], 4, 2)
        self.assertEqual(dict(profile), {(3, 2, 0x2000): 4})
        self.assertEqual(summary['unresolved_slots'], 0)

    def test_recovery_samples_remain_resolved_after_culprit_retirement(self):
        first = row(0)
        first.update({'ROB_FLUSH_VALID': 1, 'ROB_FLUSH_BITS_PC': 0x1000,
                      'ROB_FLUSH_BITS_CAUSE': 1, 'ROB_FLUSH_BITS_SELF': 0})
        ptr(first, 'ROB_FLUSH_BITS_ROBIDX', 1)
        for lane in range(4):
            sample(first, lane, 3, 0, 0x1000, cause=1)
        second = row(1, False)
        commit(second, 1)
        third = row(2)
        for lane in range(4):
            sample(third, lane, 3, 0, 0x1000, cause=1)
        profile, summary = analyze([first, second, third], 4, 2)
        self.assertEqual(dict(profile), {(3, 1, 0x1000): 8})
        self.assertEqual(summary['unresolved_slots'], 0)

    def test_end_cycle_excludes_dump_tail_but_keeps_late_retirement_finalization(self):
        first = row(0)
        for lane in range(4):
            sample(first, lane, 0, 1, 0x1000)
        second = row(1, False)
        second['ROB_RETIRED'] = 2
        third = row(2)
        third['ROB_RETIRED'] = 3
        commit(third, 1)
        profile, summary = analyze([first, second, third], 4, 2, end_cycle=2)
        self.assertEqual(dict(profile), {(0, 0, 0x1000): 4})
        self.assertEqual(summary['roi_cycles'], 2)
        self.assertEqual(summary['roi_retired'], 2)
        self.assertEqual(summary['sampled_cycles'], 1)
        self.assertEqual(summary['unresolved_slots'], 0)

    def test_warmup_filters_samples_but_keeps_future_lifecycle_for_roi(self):
        first = row(0)
        first['ROB_RETIRED'] = 3
        second = row(1)
        second['ROB_RETIRED'] = 2
        third = row(2)
        third['ROB_RETIRED'] = 4
        for lane in range(4):
            sample(third, lane, 0, 2, 0x1000)
        fourth = row(3, False)
        fourth['ROB_RETIRED'] = 1
        commit(fourth, 2)
        profile, summary = analyze([first, second, third, fourth], 4, 2, warmup_instr=5)
        self.assertEqual(dict(profile), {(0, 0, 0x1000): 4})
        self.assertEqual(summary['sampled_cycles'], 1)
        self.assertEqual(summary['roi_begin_cycle'], 2)
        self.assertEqual(summary['roi_retired'], 5)
        self.assertEqual(summary['roi_ipc'], 2.5)

    def test_compressed_slot_identity_and_wrap(self):
        self.assertFalse(killed((0, 2, 1), (0, 2, 1), False))
        self.assertTrue(killed((0, 2, 0), (0, 2, 1), False))
        self.assertTrue(killed((1, 0, 1), (0, 7, 0), False))
        self.assertTrue(killed((0, 2, 1), (0, 2, 1), True))
        first = row(0)
        sample(first, 0, 0, 2, 0x1000, former=True)
        sample(first, 1, 0, 2, 0x1004, former=False)
        sample(first, 2, 0, 2, 0x1008, former=False)
        sample(first, 3, 0, 2, 0x100c, former=False)
        second = row(1, False)
        commit(second, 2, former=True)
        profile, summary = analyze([first, second], 4, 2)
        self.assertEqual(dict(profile), {(0, 0, 0x1000): 1})
        self.assertEqual(summary['unresolved_slots'], 3)


if __name__ == '__main__':
    unittest.main()
