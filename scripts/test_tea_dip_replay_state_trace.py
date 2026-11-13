"""Exercise interval endpoints, concurrent entries and corrupted state transitions."""
import unittest

from tea_dip_gsim_replay_state_probe import FIELDS
from tea_dip_replay_state_trace import segments, union_length


def row(cycle, entry, allocated, **values):
    result = dict.fromkeys(FIELDS, 0)
    result.update(core_cycle=cycle, kind=1, entry=entry, allocated=allocated)
    if allocated:
        result.update(cause=64, issue_cycle=1)
    result.update(values)
    return result


class ReplayStateTest(unittest.TestCase):
    def test_interleaved_entry_changes_preserve_exclusive_end(self):
        spans = segments([row(2, 0, 1, blocking=1), row(3, 1, 1, eligible=1),
                          row(5, 0, 1, eligible=1), row(7, 1, 0), row(8, 0, 0)], 10)
        self.assertEqual([(lo, hi, r['entry']) for lo, hi, r in spans], [(2, 5, 0), (3, 7, 1), (5, 8, 0)])
        self.assertEqual(union_length([(lo, hi) for lo, hi, _ in spans]), 6)

    def test_open_tail_is_clipped_and_duplicate_cycle_rejected(self):
        self.assertEqual([(lo, hi) for lo, hi, _ in segments([row(4, 0, 1)], 9)], [(4, 9)])
        with self.assertRaisesRegex(AssertionError, 'Duplicate'):
            segments([row(4, 0, 1), row(4, 0, 0)], 9)
        with self.assertRaises(AssertionError):
            segments([row(4, 0, 1, eligible=1, scheduled=1)], 9)


if __name__ == '__main__':
    unittest.main()
