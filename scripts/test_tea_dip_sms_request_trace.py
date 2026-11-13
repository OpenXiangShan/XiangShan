import unittest
from tea_dip_sms_request_trace import match_pipeline


def row(cycle,kind,address=0x1000):return dict(core_cycle=cycle,kind=kind,address=address)


class SMSPipeline(unittest.TestCase):
    def test_repeated_addresses_retain_distinct_FIFO_members(self):
        complete,pending=match_pipeline([row(1,1),row(2,1),row(3,2),row(4,2)])
        self.assertEqual([r['sequence']for r in complete],[1,2]);self.assertFalse(pending)

    def test_unfinished_tail_remains_pending(self):
        complete,pending=match_pipeline([row(1,1),row(2,1),row(3,2)])
        self.assertEqual(len(complete),1);self.assertEqual(pending,[dict(sequence=2,input_core=2,address=0x1000)])

    def test_wrong_phase_and_dropped_member_are_rejected(self):
        for rows in [[row(1,1),row(4,2)],[row(1,2)]]:
            with self.assertRaises(AssertionError):match_pipeline(rows)

    def test_old_missing_output_is_not_a_capture_end_pending_request(self):
        with self.assertRaisesRegex(AssertionError,'capture boundary'):
            match_pipeline([row(1,1),row(10,0,0)])

    def test_wrong_forwarded_address_is_rejected(self):
        with self.assertRaisesRegex(AssertionError,'address mismatch'):
            match_pipeline([row(1,1),row(3,2,0x2000)])


if __name__=='__main__':unittest.main()
