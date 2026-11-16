import unittest
from dip_analyze import analyze, killed


def reference(row, prefix, generation, index=0, former=True, flag=False):
    row.update({prefix + '_GENERATION': generation, prefix + '_ROBIDX_FLAG': int(flag),
                prefix + '_ROBIDX_VALUE': index, prefix + '_ROBIDX_ISFORMER': int(former)})


def sample(cycle, retired=0, width=4):
    row = {'CYCLE': cycle, 'TOTALRETIRED': retired, 'RETIRED': 0}
    for lane in range(width):
        slot(row, lane, 2, 2, cycle)
    return row


def slot(row, lane, state, kind, generation, index=0, pc=0, cause=0, former=True, flag=False):
    prefix = f'SLOTS_{lane}'
    row.update({prefix + '_STATE': state, prefix + '_KIND': kind,
                prefix + '_PC': pc, prefix + '_CAUSE': cause})
    reference(row, prefix + '_TARGET', generation, index, former, flag)


def event(cycle, kind, generation=0, index=0, pc=0, cause=0, self_flush=False, former=True, flag=False):
    row = {'CYCLE': cycle, 'KIND': kind, 'PC': pc, 'CAUSE': cause, 'SELF': int(self_flush)}
    reference(row, 'TARGET', generation, index, former, flag)
    reference(row, 'NEXT', 0)
    return row


class DipAnalysisTest(unittest.TestCase):
    def test_partial_dispatch_frontend_successor_and_final_censoring(self):
        first = sample(0)
        slot(first, 0, 0, 1, 0, 0, 0x1000)
        for lane in range(1, 4):
            slot(first, lane, 1, 1, 0, 1, 0x1004, cause=3)
        second = sample(1)
        final = sample(4)
        bind = event(2, 2, 1, pc=0x1008, cause=3)
        reference(bind, 'NEXT', 2, 2)
        events = [event(1, 0, 0, 0), event(2, 0, 0, 1), bind, event(3, 0, 2, 2)]
        profile, summary = analyze([first, second, final], events, 4)
        self.assertEqual(dict(profile), {(0, 0, 0x1000): 1, (1, 3, 0x1004): 3, (2, 3, 0x1008): 4})
        self.assertEqual(summary['unresolved_slots'], 4)
        self.assertEqual(summary['sampled_cycles'], 3)
        self.assertEqual(sum(summary['state_cycles'].values()) + summary['unresolved_cycles'], 3)

    def test_nested_flush_reparents_every_provisional_state(self):
        first = sample(0)
        slot(first, 0, 0, 1, 0, 3, 0x3000)
        slot(first, 1, 1, 1, 0, 3, 0x3004, cause=2)
        younger = event(1, 1, 1, 2, 0x2000, 1)
        older = event(2, 1, 2, 1, 0x1000, 4)
        profile, summary = analyze([first], [younger, older, event(3, 0, 2, 1)], 4)
        self.assertEqual(dict(profile), {(3, 4, 0x1000): 4})
        self.assertEqual(summary['reclassified_slots'], 4)
        self.assertEqual(summary['unresolved_slots'], 0)

    def test_self_flushed_latter_settles_at_successor_and_recovery_is_stable(self):
        first = sample(0)
        for lane in range(4):
            slot(first, lane, 0, 1, 0, 3, 0x3000)
        final = sample(3)
        for lane in range(4):
            slot(final, lane, 3, 0, 0, pc=0x2000, cause=2)
        events = [event(1, 1, 1, 2, 0x2000, 2, self_flush=True, former=False),
                  event(2, 3, index=3), event(4, 1, 4, 4, 0x4000, 1, flag=True)]
        profile, summary = analyze([first, final], events, 4)
        self.assertEqual(dict(profile), {(3, 2, 0x2000): 8})
        self.assertEqual(summary['unresolved_slots'], 0)

    def test_generations_and_former_latter_prevent_rob_alias(self):
        first, second = sample(0), sample(2)
        for lane in range(4):
            slot(first, lane, 0, 1, 0, 2, 0x1000, former=True)
            slot(second, lane, 0, 1, 2, 2, 0x2000, former=False, flag=True)
        profile, summary = analyze([first, second], [event(1, 0, 0, 2), event(3, 0, 0, 2)], 4)
        self.assertEqual(dict(profile), {(0, 0, 0x1000): 4})
        self.assertEqual(summary['unresolved_slots'], 4)

    def test_watermark_does_not_resolve_normal_pending_work(self):
        row = sample(0)
        for lane in range(4):
            slot(row, lane, 0, 1, 0, 2, 0x1000)
        profile, summary = analyze([row], [event(1, 3, index=3)], 4)
        self.assertFalse(profile)
        self.assertEqual(summary['unresolved_slots'], 4)

    def test_warmup_and_exclusive_end_filter_samples_but_keep_late_finals(self):
        before, inside, outside = sample(0, 3), sample(10, 5), sample(20, 9)
        inside['RETIRED'] = 2
        for row in [before, inside, outside]:
            for lane in range(4):
                slot(row, lane, 0, 1, row['CYCLE'], 2, 0x1000)
        profile, summary = analyze([before, inside, outside], [event(30, 0, 10, 2)], 4,
                                   warmup_instr=5, end_cycle=20)
        self.assertEqual(dict(profile), {(0, 0, 0x1000): 4})
        self.assertEqual(summary['sampled_cycles'], 1)
        self.assertEqual(summary['sampled_begin_cycle'], 10)
        self.assertEqual(summary['sampled_span_retired'], 2)
        self.assertEqual(summary['unresolved_slots'], 0)

    def test_successor_binding_precedes_new_waiting_at_same_cycle(self):
        first, second = sample(0), sample(1)
        bind = event(1, 2, 0, pc=0x1000, cause=4)
        reference(bind, 'NEXT', 1, 1)
        profile, summary = analyze([first, second], [bind, event(2, 0, 1, 1)], 4)
        self.assertEqual(dict(profile), {(2, 4, 0x1000): 4})
        self.assertEqual(summary['unresolved_slots'], 4)

    def test_compressed_slot_identity_and_wrap(self):
        self.assertFalse(killed((0, 2, 1), (0, 2, 1), False))
        self.assertTrue(killed((0, 2, 0), (0, 2, 1), False))
        self.assertTrue(killed((1, 0, 1), (0, 7, 0), False))
        self.assertTrue(killed((0, 2, 1), (0, 2, 1), True))


if __name__ == '__main__':
    unittest.main()
