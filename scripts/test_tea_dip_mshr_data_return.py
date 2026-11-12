"""Protect owner reuse, simultaneous edges and the distinct downstream/CPU milestones."""
import unittest

from tea_dip_mshr_data_return import annotate, return_cycles


def event(cycle, *, allocate=None, returned=None, freed=None):
    row = dict(NATIVECYCLE=cycle, STAMP=cycle, EVENTS_BANK=1,
               EVENTS_ALLOCATED_VALID=allocate is not None, EVENTS_ALLOCATED_BITS_ENTRY=allocate or 0)
    for entry in range(16):
        row[f'EVENTS_RETURNED_{entry}_VALID'] = returned is not None and returned[0] == entry
        row[f'EVENTS_RETURNED_{entry}_BITS'] = returned[1] if returned else 0
        row[f'EVENTS_FREED_{entry}_VALID'] = freed is not None and freed[0] == entry
        row[f'EVENTS_FREED_{entry}_BITS'] = freed[1] if freed else 0
    return row


def sample(cycle, start=100):
    return dict(status='same_line_live_owner_candidate', sample_cycle=cycle,
                owner=dict(bank=1, entry=2, start=start))


class DataReturnTests(unittest.TestCase):
    def test_same_edge_return_free_reallocate_uses_old_generation(self):
        events = return_cycles([event(100, allocate=2), event(140, returned=(2, 40), freed=(2, 40), allocate=2),
                                event(180, returned=(2, 40))])
        self.assertEqual(events, {(1, 2, 100): [140], (1, 2, 140): [180]})

    def test_before_same_cycle_after_are_distinct_and_free_is_not_return(self):
        samples = [sample(cycle) for cycle in [139, 140, 141]]
        counts = annotate(samples, {(1, 2, 100): [140]})
        self.assertEqual(counts, dict(before_last_data=1, same_cycle_as_last_data=1,
                                     after_last_data_before_free=1))
        self.assertEqual(samples[-1]['last_downstream_data_cycle'], 140)

    def test_missing_multiple_and_unassociated_remain_explicit(self):
        rows = [sample(150), sample(150, 110), dict(status='no_unique_same_line_live_owner')]
        self.assertEqual(annotate(rows, {(1, 2, 110): [130, 140]}),
                         dict(no_logged_last_data_event=1, multiple_last_data_events=1))
        self.assertNotIn('downstream_data_stage', rows[-1])

    def test_wrong_owner_clock_age_rejects_instead_of_misattributing(self):
        with self.assertRaises(AssertionError):
            return_cycles([event(100, allocate=2), event(140, returned=(2, 39))])


if __name__ == '__main__':
    unittest.main()
