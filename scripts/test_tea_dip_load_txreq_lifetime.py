import unittest
from tea_dip_load_txreq_lifetime import associate_lifetimes


def sample(cycle=50, **values):
    return dict(pc='0x108e4', physical_address=0x1048, allocation_cycle=20, issue_cycle=30,
                writeback_cycle=70, sample_cycle=cycle, writeback_id=11) | values


def trace(cycle, mask=1, ready=0, out_ready=1):
    row = dict(NATIVECYCLE=cycle, EVENTS_VALIDMASK=mask, EVENTS_READYMASK=ready, EVENTS_OUTREADY=out_ready)
    for port in range(4):
        row.update({f'EVENTS_REQUESTS_{port}_OPCODE': 1, f'EVENTS_REQUESTS_{port}_ADDRESS': 0x1040,
                    f'EVENTS_REQUESTS_{port}_TRANSACTION': port + 10})
    return row


class LoadLifetimeTests(unittest.TestCase):
    def test_earlier_wait_is_visible_without_a_sample_cycle_trace(self):
        row, = associate_lifetimes([sample()], [trace(40), trace(41, ready=1)], 3)
        self.assertEqual(row['lifetime_read_condition_cycles'], dict(other_port_served=1, served=1))
        self.assertEqual(row['waiting_cycles_before_first_sample'], 1)

    def test_repeated_samples_and_multiport_same_line_are_not_double_counted(self):
        rows = associate_lifetimes([sample(50), sample(60)], [trace(40, mask=3, ready=2)], 3)
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]['target_samples'], 2)
        self.assertEqual(rows[0]['lifetime_read_condition_cycles'], dict(served=1))
        self.assertEqual(len(rows[0]['observed_matching_events'][0]['ports']), 2)

    def test_reset_origin_and_half_open_lifetime_edges(self):
        row, = associate_lifetimes([sample()], [trace(t, out_ready=0) for t in [22, 23, 72, 73]], 3)
        self.assertEqual([r['cycle'] for r in row['observed_matching_events']], [23, 72])
        self.assertEqual(row['lifetime_read_condition_cycles'], dict(downstream_not_ready=2))

    def test_early_attempt_is_retained_but_finished_and_missing_loads_are_not(self):
        row, = associate_lifetimes([sample(25), sample(70), {'status': 'missing_writeback'}], [trace(26)], 3)
        self.assertEqual(row['target_samples'], 1)
        self.assertEqual(row['samples_before_final_issue'], 1)
        self.assertEqual(row['waiting_cycles_before_first_sample'], 1)


if __name__ == '__main__':
    unittest.main()
