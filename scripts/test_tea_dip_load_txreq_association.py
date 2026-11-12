"""Identity reuse and request-kind counterexamples for sampled load association."""
import unittest
from tea_dip_load_txreq_association import resolve_sample, egress_condition


class LoadTxreqTests(unittest.TestCase):
    def setUp(self):
        self.target = dict(CYCLE=50, TARGET_ALLOCATIONCYCLE=20, TARGET_ROBINDEX=7, TARGET_RANK=2)
        self.final = dict(CYCLE=90, MEMBER_PC=0x108e4, CANCELLED=0, ISDRAIN=0)
        self.wb = dict(ID=11, ROBIDX=7, WBTEARANK=2, SLOTTEARANK=2, WBROBFLAG=1, SLOTROBFLAG=1, WBUOPIDX=0, SLOTUOPIDX=0, SLOTFUTYPE=65536, DPADDR=0x1048, ISSUETIME=30, WRITEBACKTIME=70, STAMP=70)
        self.trace = dict(EVENTS_VALIDMASK=1, EVENTS_READYMASK=1, EVENTS_OUTREADY=1, EVENTS_HEADVALID=1, EVENTS_HEADADDRESS=0x1040, EVENTS_REQUESTS_0_OPCODE=1, EVENTS_REQUESTS_0_ADDRESS=0x1040, EVENTS_REQUESTS_1_OPCODE=1, EVENTS_REQUESTS_1_ADDRESS=0x2000)

    def resolve(self, rows=None, final=None, trace=None):
        return resolve_sample(self.target, [self.final if final is None else final], [self.wb] if rows is None else rows, self.trace if trace is None else trace)

    def test_old_future_and_foreign_uop_writebacks_are_excluded(self):
        rows = [dict(self.wb, STAMP=10, WRITEBACKTIME=10, ISSUETIME=5), dict(self.wb, STAMP=300, WRITEBACKTIME=300, ISSUETIME=290), dict(self.wb, WBUOPIDX=1), self.wb]
        self.assertEqual(self.resolve(rows)['writeback_id'], 11)

    def test_ambiguous_writeback_is_not_nearest_selected(self):
        self.assertEqual(self.resolve([self.wb, dict(self.wb, ID=12, STAMP=60, WRITEBACKTIME=60)])['status'], 'missing_or_ambiguous_representative_writeback')

    def test_missing_cancelled_and_drain_finals(self):
        self.assertEqual(resolve_sample(self.target, [], [self.wb], self.trace)['status'], 'missing_or_ambiguous_final')
        for key in ['CANCELLED', 'ISDRAIN']:
            self.assertEqual(self.resolve(final=dict(self.final, **{key: 1}))['status'], 'cancelled_or_drain_final')

    def test_nonload_zero_address_and_last_issue_boundary(self):
        for values, expected in [(dict(SLOTFUTYPE=1), 'non_load_representative'), (dict(DPADDR=0), 'zero_load_address'), (dict(ISSUETIME=55), 'outside_final_issue_writeback_interval')]:
            self.assertEqual(self.resolve([dict(self.wb, **values)])['status'], expected)

    def test_read_conditions_partition(self):
        for values, expected in [(dict(EVENTS_OUTREADY=0, EVENTS_READYMASK=0), 'matching_read_downstream_not_ready'), (dict(EVENTS_VALIDMASK=3, EVENTS_READYMASK=2), 'matching_read_other_port_served'), (dict(), 'matching_read_served')]:
            self.assertEqual(self.resolve(trace=dict(self.trace, **values))['status'], expected)

    def test_true_target_address_is_independent_of_old_debug_address(self):
        row = self.resolve(trace=dict(self.trace, EVENTS_HEADADDRESS=0x2000))
        self.assertEqual((row['status'], row['debug_head_address_relation']), ('matching_read_served', 'different_line'))

    def test_writes_and_mmio_are_not_bank_reads(self):
        for trace in [dict(self.trace, EVENTS_REQUESTS_0_OPCODE=29), dict(self.trace, EVENTS_VALIDMASK=16)]:
            self.assertEqual(self.resolve(trace=trace)['status'], 'active_egress_no_matching_read')

    def test_absent_trace_and_multiport_union(self):
        self.assertEqual(egress_condition(0x1048, None), ('no_active_egress_record', 'unobserved'))
        trace = dict(self.trace, EVENTS_VALIDMASK=3, EVENTS_READYMASK=2, EVENTS_REQUESTS_1_OPCODE=1, EVENTS_REQUESTS_1_ADDRESS=0x104f)
        self.assertEqual(egress_condition(0x1048, trace)[0], 'matching_read_served')


if __name__ == '__main__':
    unittest.main()
