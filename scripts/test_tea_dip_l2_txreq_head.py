import unittest

from tea_dip_l2_txreq_head import reconstruct


def row(cycle, valid=0, ready=0, out_ready=True, head=True, address=0x1040, requests=None, dump=False, clean=False):
    data = dict(NATIVECYCLE=cycle, NATIVEDUMP=dump, LOGENABLE=True, CLEAN=clean,
                EVENTS_VALIDMASK=valid, EVENTS_READYMASK=ready, EVENTS_OUTREADY=out_ready,
                EVENTS_HEADVALID=head, EVENTS_HEADADDRESS=address)
    for port, (addr, opcode, transaction) in (requests or {}).items():
        data.update({f'EVENTS_REQUESTS_{port}_ADDRESS': addr, f'EVENTS_REQUESTS_{port}_OPCODE': opcode,
                     f'EVENTS_REQUESTS_{port}_TRANSACTION': transaction})
    return data


class HeadAddressTest(unittest.TestCase):
    def test_served_other_grant_and_downstream_partition(self):
        requests = {0: (0x1040, 1, 9), 1: (0x2000, 1, 12)}
        snapshot = reconstruct([row(1, 3, 2, requests=requests), row(2, 1, 0, False, requests=requests),
                                row(3, 1, 1, requests=requests), row(4, dump=True)])[4]
        self.assertEqual(snapshot['conditions']['head_line_read_queued'], 3)
        for field in ['head_line_read_other_port_served', 'head_line_read_downstream_not_ready', 'head_line_read_served']:
            self.assertEqual(snapshot['conditions'][field], 1)
        self.assertEqual(snapshot['matching_read_grants'][0]['local_transaction'], 9)

    def test_read_class_and_line_offset(self):
        rows = [row(1, 1, 1, requests={0: (0x1078, 0x26, 7)}),
                row(2, 1, 1, requests={0: (0x1040, 0x1B, 8)}), row(3, dump=True)]
        result = reconstruct(rows)[3]['conditions']
        self.assertEqual(result['head_line_read_queued'], 1)
        self.assertEqual(result['active_head_address_without_matching_read'], 1)

    def test_invalid_head_and_nonbank_do_not_match(self):
        result = reconstruct([row(1, 1, 1, head=False), row(2, 16, 16), row(3, dump=True)])[3]['conditions']
        self.assertNotIn('head_line_read_queued', result)
        self.assertEqual(result['active_head_address_without_matching_read'], 1)

    def test_multiple_matching_ports_count_one_condition_cycle(self):
        result = reconstruct([row(1, 3, 2, requests={0: (0x1040, 1, 7), 1: (0x1040, 4, 7)}),
                              row(2, dump=True)])[2]
        self.assertEqual(result['conditions']['head_line_read_queued'], 1)
        self.assertEqual(result['matching_read_grants'][0]['port'], 1)

    def test_dump_excludes_current_cycle_clean_does_not_fill_gaps(self):
        request = {0: (0x1040, 1, 1)}
        snapshots = reconstruct([row(1, 1, 1, requests=request), row(2, 1, 1, requests=request, dump=True, clean=True),
                                 row(20, 1, 1, requests=request), row(21, dump=True)])
        self.assertEqual(snapshots[2]['conditions']['head_line_read_served'], 1)
        self.assertEqual(snapshots[21]['conditions']['head_line_read_served'], 1)
        self.assertEqual(snapshots[21]['matching_read_grants'][0]['native_cycle'], 20)

    def test_old_schema_is_rejected(self):
        old = row(1)
        del old['EVENTS_HEADADDRESS']
        with self.assertRaises(KeyError):
            reconstruct([old])


if __name__ == '__main__':
    unittest.main()
