"""Prevent broadcast TLB responses from being attributed to an idle VBOP filter."""
import unittest
from tea_dip_prefetch_tlb_native import counters


def values(**overrides):
    raw = dict(tlb_req=0, tlb_miss=384268, entry_miss_first_replay=0,
               entry_miss_drop=0, tlb_excp=0, entry_excp=0)
    raw.update(overrides)
    return {'chip.l2cache.prefetcher.vbop.reqFilter.' + key: value for key, value in raw.items()}


class PrefetchTLBScopeTests(unittest.TestCase):
    def test_idle_vbop_does_not_own_broadcast_misses(self):
        row = counters(values())
        self.assertEqual(row['entry_attributed_miss_events'], 0)
        self.assertEqual(row['shared_response_miss_observations'], 384268)

    def test_tracked_misses_use_both_entry_partitions(self):
        row = counters(values(tlb_req=7068, tlb_miss=394959,
                              entry_miss_first_replay=3468, entry_miss_drop=1749))
        self.assertEqual(row['entry_attributed_miss_events'], 5217)
        self.assertEqual(row['request_valid_cycles'], 7068)

    def test_broadcast_exceptions_are_not_entry_drops(self):
        row = counters(values(tlb_excp=9, entry_excp=1))
        self.assertEqual(row['shared_response_exception_observations'], 9)
        self.assertEqual(row['entry_exception_drop_events'], 1)

    def test_epoch_boundary_does_not_invent_a_miss_rate(self):
        row = counters(values(tlb_req=0, entry_miss_first_replay=2, entry_miss_drop=1))
        self.assertEqual(row['entry_attributed_miss_events'], 3)
        self.assertNotIn('miss_rate', row)


if __name__ == '__main__':
    unittest.main()
