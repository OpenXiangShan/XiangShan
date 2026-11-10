"""Check saturation, recovery and rejection of corrupt phase observations."""
import unittest
from tea_dip_bop_gate_trace import audit_rows


def events(start, disabled, threshold):
    rows, before = [], start
    for i, bad in enumerate(disabled):
        after = min(15, before + 2) if bad else max(0, before - 1)
        rows.append(dict(ID=i+1, STAMP=100+i*20, SITE='vbopIssueGate', CONFBEFORE=before,
                         CONFAFTER=after, THRESHOLD=threshold, TEACHERDISABLED=bad,
                         COMMITTEDTEACHERENABLED=1-bad, STUDENTSELECTED=1, EXTERNALLYENABLED=1,
                         GATEBEFORE=int(before<threshold), GATEAFTER=int(after<threshold)))
        before = after
    return rows


class PhaseTraceTest(unittest.TestCase):
    def test_gate4_close_and_reopen(self):
        result = audit_rows(events(0, [1, 1, 1, 0, 0, 0], 4))
        self.assertEqual((result['closed_on_update'], result['opened_on_update']), (1, 1))

    def test_nonzero_first_row_and_saturation(self):
        rows = events(13, [1, 1, 0, 0], 14)
        self.assertEqual(audit_rows(rows)['opened_on_update'], 1)
        self.assertEqual(rows[1]['CONFAFTER'], 15)
        self.assertEqual(audit_rows(events(0, [0], 4))['closed_after'], 0)

    def test_bad_feedback_or_gate_is_rejected(self):
        for field in ['CONFAFTER', 'GATEAFTER']:
            rows = events(2, [1], 4)
            rows[0][field] = 1
            with self.assertRaises(AssertionError):
                audit_rows(rows)

    def test_missing_update_is_rejected(self):
        rows = events(0, [1, 1, 1], 4)
        del rows[1]
        with self.assertRaises(AssertionError):
            audit_rows(rows)

    def test_previous_phase_feedback_relation_is_reported(self):
        # Actual MCF six-update feedback/committed-quality sequence.
        rows = events(0, [1, 0, 0, 1, 1, 1], 4)
        for row, enabled in zip(rows, [1, 1, 0, 0, 0, 0]):
            row['COMMITTEDTEACHERENABLED'] = enabled
        self.assertEqual(audit_rows(rows)['previous_phase_feedback'],
                         dict(compared_pairs=5, matches=5, mismatches=0))
        rows[1]['COMMITTEDTEACHERENABLED'] = 0
        self.assertEqual(audit_rows(rows)['previous_phase_feedback']['mismatches'], 1)

    def test_missing_update_at_zero_confidence_is_rejected(self):
        rows = events(0, [0, 0, 0], 4)
        del rows[1]
        with self.assertRaises(AssertionError):
            audit_rows(rows)


if __name__ == '__main__':
    unittest.main()
