"""Keep retirement populations and unknown/mismatched native fields explicit."""
import unittest

from tea_dip_rob_compression_native import summarize


class RobCompressionNativeTest(unittest.TestCase):
    def setUp(self):
        # Source9dd formal libquantum86704, seed2721, closed20M ROI.
        observed = dict(RobEntriesCommited=8130905, commitInstr=19999992,
                        commitCompressCntAll=7581504, compressSize=19450604,
                        compressType_NORMAL=590189, compressType_CC=413543,
                        compressType_CS=1327116, compressType_SC=5800057,
                        waitAluCycle=1035697)
        observed.update({'commitCompressCnt' + str(i): count for i, count in
                         enumerate([549401, 3295345, 4284729, 1426, 3, 0, 0, 1], 1)})
        self.prefix = 'SimTop.cpu.l_soc.core_with_l2.core.backend.inner.ctrlBlock.rob.'
        self.counters = {self.prefix + name: [value] for name, value in observed.items()}

    def test_real_retirement_identities_preserve_registered_boundary(self):
        row = summarize(self.counters)
        self.assertEqual(set(row['identities'].values()), {'matched'})
        self.assertEqual(row['histogram_instruction_sum_minus_commitInstr'], 13)
        self.assertNotEqual(row['paired_entry_share'], row['multi_instruction_entry_share'])
        self.assertEqual(row['metrics']['waitAluCycle']['value'], 1035697)
        self.assertIsNone(row['metrics']['waitLoadCycle']['value'])

    def test_missing_bin_is_not_zero_or_renormalized(self):
        del self.counters[self.prefix + 'commitCompressCnt2']
        row = summarize(self.counters)
        self.assertEqual(row['identities']['size_histogram_entries'], 'unknown')
        self.assertIsNone(row['mean_recorded_instructions_per_entry'])

    def test_duplicate_name_is_not_summed_or_last_selected(self):
        self.counters[self.prefix + 'commitCompressCnt2'] *= 2
        row = summarize(self.counters)
        self.assertEqual(row['instructions_per_entry_histogram']['2']['status'], 'ambiguous')
        self.assertIsNone(row['histogram_instruction_sum_minus_commitInstr'])

    def test_inconsistent_aggregate_is_rejected(self):
        self.counters[self.prefix + 'compressSize'][0] += 1
        with self.assertRaises(AssertionError):
            summarize(self.counters)

    def test_nested_queue_does_not_match_rob_counter(self):
        self.counters[self.prefix + 'rab.RobEntriesCommited'] = [1]
        self.assertEqual(summarize(self.counters)['metrics']['RobEntriesCommited']['value'], 8130905)


if __name__ == '__main__':
    unittest.main()
