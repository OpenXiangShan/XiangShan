"""Check carried-issue identity, WB timing/payload and unresolved fate."""
from collections import Counter
import json
import unittest

from tea_dip_load_writeback_trace import hist_stats, match_attempts


def event(cycle, kind, issue=8, pc=100):
    return dict(core_cycle=cycle, kind=kind, lane=0, issue_cycle=issue,
                rob_flag=0, rob_index=3, rob_former=1, rank=0, uop_index=0,
                pc=pc, virtual_address=200, physical_address=300,
                entrance=64, instr_type=1, cause_mask=0)


class WritebackTraceTest(unittest.TestCase):
    def test_histogram_library_result_survives_JSON_round_trip(self):
        output = hist_stats(Counter({2: 3, 10: 1}))
        self.assertEqual(json.loads(json.dumps(output)), output)

    def test_reused_ROB_slot_has_distinct_issue_identity(self):
        rows = [event(10, 1), event(11, 2), event(20, 1, issue=18), event(21, 2, issue=18),
                event(30, 1, issue=28)]
        attempts, completed = match_attempts(rows)
        self.assertEqual((len(attempts), len(completed)), (3, 2))

    def test_wrong_WB_clock_or_member_is_rejected(self):
        with self.assertRaisesRegex(AssertionError, 'preceding'):
            match_attempts([event(10, 1), event(12, 2)])
        with self.assertRaisesRegex(AssertionError, 'payload'):
            match_attempts([event(10, 1), event(11, 2, pc=101)])

    def test_duplicate_successful_WB_is_rejected(self):
        with self.assertRaisesRegex(AssertionError, 'duplicate'):
            match_attempts([event(10, 1), event(11, 2), event(12, 1), event(13, 2)])


if __name__ == '__main__':
    unittest.main()
