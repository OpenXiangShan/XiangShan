"""Check actual-width signs, epoch-reset priority and startup policy hysteresis."""
import unittest

from tea_dip_prefetch_epochs import ATTRIBUTES, mode2_scores, pbop_policy, signed_pe


class PrefetchEpochTests(unittest.TestCase):
    def test_physical_twenty_bits_are_not_abi_thirty_two_bits(self):
        self.assertEqual(signed_pe((1 << 20) - 1, 20), -1)
        self.assertEqual(signed_pe(1 << 19, 20), -(1 << 19))
        self.assertEqual(signed_pe((1 << 19) - 1, 20), (1 << 19) - 1)
        with self.assertRaises(AssertionError):
            signed_pe((1 << 32) - 1, 20)

    def test_epoch_reset_discards_the_pending_commit(self):
        def attribute(stamp, value):
            row = {name: 0 for name in ATTRIBUTES}
            row.update(STAMP=stamp, DELTADEMANDCACHEHIT=value)
            return row
        epochs = [dict(STAMP=4, EPOCHID=0), dict(STAMP=9, EPOCHID=1)]
        # +7 would be visible at5, exactly when epoch4 resets the PE state.
        # +3 is visible at9 and must be part of the epoch9 snapshot.
        values = mode2_scores(epochs, [attribute(1, 7), attribute(5, 3)], 20, 4)
        self.assertEqual(values, {0: 0, 1: 3})

    def test_negative_startup_and_positive_late_hit_reactivation(self):
        epochs = [dict(EPOCHID=i, STAMP=100 * (i + 1), LATENCYCURR=200) for i in range(5)]
        records = pbop_policy(epochs, dict(enumerate([-1, 0, 0, 0, 5])), 1)
        self.assertEqual([(r['post_epoch_active'], r['post_epoch_level']) for r in records],
                         [(False, 3), (False, 2), (False, 1), (False, 0), (True, 1)])


if __name__ == '__main__':
    unittest.main()
