import unittest
from tea_dip_sms_internal_trace import filter_owners, generation_commits


def state(cycle, kind=1, fresh=0, bits=1, sent=0, region=4):
    return dict(core_cycle=cycle, kind=kind, slot=0, new_owner=fresh, allocated=1,
                region_tag=7, region_address=region, region_bits=bits, filter_bits=sent,
                physical_valid=1, decreasing=0)


def request(cycle, hit=0, bits=1):
    return dict(core_cycle=cycle, kind=1, slot=0, hit=hit, region_tag=7,
                region_address=4, region_bits=bits, physical_valid=1, decreasing=0)


class SMSInternal(unittest.TestCase):
    def test_actual_allocation_edge_distinguishes_identical_slot_reuse(self):
        rows = [state(2, fresh=1), state(3, kind=2), state(5, fresh=1), state(6, kind=2)]
        owners, departures = filter_owners(rows, [dict(core_cycle=c, kind=1, address=4096) for c in [3, 6]])
        self.assertEqual([r['generation'] for r in departures], [1, 2])
        complete, pending = generation_commits([request(0), request(3)], owners, 6)
        self.assertEqual([r['generation'] for r in complete], [1, 2])
        self.assertFalse(pending)

    def test_wrong_actual_departure_address_is_rejected(self):
        with self.assertRaisesRegex(AssertionError, 'address mismatch'):
            filter_owners([state(2, fresh=1), state(3, kind=2)], [dict(core_cycle=3, kind=1, address=4160)])

    def test_missing_allocation_edge_is_not_inherited(self):
        with self.assertRaisesRegex(AssertionError, 'allocation edge'):
            filter_owners([state(2)], [])

    def test_accepted_bits_must_commit_but_capture_tail_remains_pending(self):
        owners, _ = filter_owners([state(2, fresh=1)], [])
        with self.assertRaisesRegex(AssertionError, 'bits missing'):
            generation_commits([request(0, bits=3)], owners, 2)
        _, pending = generation_commits([request(1)], owners, 2)
        self.assertEqual(len(pending), 1)


if __name__ == '__main__':
    unittest.main()
