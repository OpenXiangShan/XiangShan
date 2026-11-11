"""Host reconstruction regressions; the real GSIM trace ABI remains separately calibrated."""
import unittest
from tea_dip_mshr_lifecycle import reconstruct


def row(cycle, bank=0, allocated=None, merged=None, freed=(), clean=False, dump=False):
    r = dict(NATIVECYCLE=cycle, EVENTS_BANK=bank, EVENTS_ALLOCATED_VALID=allocated is not None,
             EVENTS_MERGED_VALID=merged is not None, CLEAN=clean, DUMP=dump, LOGENABLE=True)
    for i in range(2):
        r[f'EVENTS_FREED_{i}_VALID'] = False
    for i, age in freed:
        r[f'EVENTS_FREED_{i}_VALID'] = True
        r[f'EVENTS_FREED_{i}_BITS'] = age
    if allocated is not None:
        entry, line, source, prefetch = allocated
        for name, value in dict(ENTRY=entry, LINE=line, SOURCE=source, CHANNEL=1, PREFETCH=prefetch, MISS=True).items():
            r['EVENTS_ALLOCATED_BITS_' + name] = value
    if merged is not None:
        for name, value in zip(['ENTRY', 'LINE', 'SOURCE'], merged):
            r['EVENTS_MERGED_BITS_' + name] = value
    return r


class LifecycleTests(unittest.TestCase):
    def test_concurrent_frees_and_slot_reuse_keep_owner_and_demand_distinct(self):
        r = reconstruct([row(3, allocated=(0, 10, 16, True)), row(4, allocated=(1, 11, 3, False)),
                         row(5, merged=(0, 10, 2)), row(10, freed=[(0, 7), (1, 6)], clean=True, dump=True),
                         row(11, allocated=(0, 12, 2, False)), row(14, freed=[(0, 3)], dump=True)], [0, 1])
        first = r['snapshots'][10][0]
        self.assertEqual(first['slot_counts'], {0: 1, 1: 1})
        self.assertEqual(first['slot_sums'], {0: 7, 1: 6})
        merged, = [g for g in first['completion_groups'] if g['merged_source']]
        self.assertEqual((merged['owner_source'], merged['merged_source']), ('Prefetch2L2CDP', 'CPULoadData'))
        self.assertEqual(merged['completed_owner_residence']['mean'], 7)
        self.assertEqual(merged['post_merge_to_free']['mean'], 5)
        self.assertEqual(r['snapshots'][14][0]['slot_sums'], {0: 3})
        self.assertEqual(r['simultaneous_free_batches'], 1)
        self.assertEqual(r['free_events'], 3)

    def test_cross_epoch_completion_and_censored_requests_are_visible(self):
        r = reconstruct([row(3, allocated=(0, 10, 16, True)), row(5, merged=(0, 10, 2)),
                         row(8, clean=True, dump=True), row(13, freed=[(0, 10)], dump=True)], [0, 1])
        old = r['snapshots'][8][0]
        self.assertEqual(old['completion_groups'], [])
        self.assertEqual(old['unfinished'][0]['observed_post_merge_age'], 3)
        group, = r['snapshots'][13][0]['completion_groups']
        self.assertEqual(group['completed_owner_residence']['cycle_sum'], 10)
        self.assertEqual(group['post_merge_to_free']['cycle_sum'], 8)
        self.assertEqual(group['post_merge_within_epoch']['cycle_sum'], 5)
        self.assertEqual(group['started_before_epoch'], 1)
        self.assertEqual(group['merged_before_epoch'], 1)

    def test_same_slot_in_different_banks_and_wrong_line_rejection(self):
        r = reconstruct([row(3, bank=0, allocated=(0, 10, 16, True)),
                         row(3, bank=1, allocated=(0, 20, 2, False)),
                         row(8, bank=0, freed=[(0, 5)], dump=True),
                         row(8, bank=1, freed=[(0, 5)], dump=True)], [0, 1])
        self.assertEqual(set(r['snapshots'][8]), {0, 1})
        with self.assertRaises(AssertionError):
            reconstruct([row(3, allocated=(0, 10, 16, True)), row(5, merged=(0, 11, 2))], [0, 1])
        with self.assertRaises(AssertionError):
            reconstruct([row(3, allocated=(0, 10, 16, True)), row(8, freed=[(0, 4)])], [0, 1])


if __name__ == '__main__':
    unittest.main()
