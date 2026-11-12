"""Protect replay latency interpretation from overflow and missing/ambiguous counters."""
import unittest

from tea_dip_replay_native import summarize


class ReplayNativeTest(unittest.TestCase):
    def setUp(self):
        # Observed GCC expr4993 control1M ROI, source9dd, seed3767.
        self.prefix = 'SimTop.cpu.l_soc.core_with_l2.core.memBlock.inner.lsq.loadQueue.loadQueueReplay.replay_dcache_miss'
        self.counters = {self.prefix + suffix: [value] for suffix, value in [
            ('_release_count', 3518), ('_release_latency_cycles', 233020),
            ('_release_latency_overflow_count', 0), ('_scalar_rob_head_blocked_cycles', 30690)]}

    def row(self):
        return summarize(self.counters)['dcache_miss']

    def test_observed_release_and_head_populations_stay_separate(self):
        row = self.row()
        self.assertEqual(row['releases']['value'], 3518)
        self.assertEqual(row['scalar_ROB_head_blocked']['value'], 30690)
        self.assertAlmostEqual(row['mean_released_entry_residence_cycles'], 66.23649801023308)
        self.assertIsNone(summarize(self.counters)['bank_conflict']['releases']['value'])

    def test_saturation_suppresses_exact_mean(self):
        self.counters[self.prefix + '_release_latency_overflow_count'] = [1]
        self.assertIsNone(self.row()['mean_released_entry_residence_cycles'])

    def test_missing_overflow_is_unknown(self):
        del self.counters[self.prefix + '_release_latency_overflow_count']
        self.assertIsNone(self.row()['mean_released_entry_residence_cycles'])

    def test_repeated_native_name_is_not_collapsed(self):
        self.counters[self.prefix + '_release_count'] = [3518, 3518]
        row = self.row()
        self.assertEqual(row['releases']['status'], 'ambiguous')
        self.assertIsNone(row['mean_released_entry_residence_cycles'])

    def test_known_zero_releases_have_no_mean(self):
        self.counters[self.prefix + '_release_count'] = [0]
        self.counters[self.prefix + '_release_latency_cycles'] = [0]
        self.assertIsNone(self.row()['mean_released_entry_residence_cycles'])


if __name__ == '__main__':
    unittest.main()
