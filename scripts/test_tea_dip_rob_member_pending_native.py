"""Reject lost/double-counted member classes; do not manufacture old-archive observations."""
import copy
import unittest

from tea_dip_rob_member_pending_native import summarize


class RobMemberPendingNativeTest(unittest.TestCase):
    def setUp(self):
        self.prefix = 'SimTop.cpu.l_soc.core_with_l2.core.backend.inner.ctrlBlock.rob.'
        values = {'waitAluCycle': 100, 'diag_rob_alu_head_former_only_cycles': 40,
                  'diag_rob_alu_head_latter_only_cycles': 50, 'diag_rob_alu_head_both_pending_cycles': 10}
        values.update({'diag_rob_alu_head_latter_only_' + name + '_cycles': value
                       for name, value in zip(['normal', 'branch', 'load', 'store'], [5, 10, 30, 5])})
        self.counters = {self.prefix + name: [value] for name, value in values.items()}

    def test_member_partition_and_nested_type_partition(self):
        row = summarize(self.counters)
        self.assertEqual(set(row['identities'].values()), {'matched'})
        self.assertEqual(row['latter_only_commit_types']['load']['value'], 30)

    def test_old_archive_has_unknown_members_not_zero(self):
        row = summarize({self.prefix + 'waitAluCycle': [100]})
        self.assertEqual(set(row['identities'].values()), {'unknown'})
        self.assertIsNone(row['pending_members']['latter_only']['value'])

    def test_missing_or_repeated_member_does_not_renormalize(self):
        key = self.prefix + 'diag_rob_alu_head_latter_only_cycles'
        for operation in ['missing', 'repeated']:
            counters = copy.deepcopy(self.counters)
            if operation == 'missing':
                del counters[key]
            else:
                counters[key] *= 2
            row = summarize(counters)
            self.assertEqual(row['identities']['old_ALU_condition'], 'unknown')
            self.assertIsNone(row['pending_members']['latter_only']['value'])

    def test_member_and_type_mismatches_are_rejected(self):
        for name in ['former_only', 'latter_only_load']:
            counters = copy.deepcopy(self.counters)
            counters[self.prefix + 'diag_rob_alu_head_' + name + '_cycles'][0] += 1
            with self.assertRaises(AssertionError):
                summarize(counters)


if __name__ == '__main__':
    unittest.main()
