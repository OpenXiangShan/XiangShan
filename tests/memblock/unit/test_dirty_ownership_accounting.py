"""Schema 53 rejects missing cleanup/traffic and permits legal reacquisition."""
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'scripts'))
import verify_regression


class DirtyOwnershipAccountingTest(unittest.TestCase):
    @staticmethod
    def fields(q=None, m=None, orders=None):
        return {
            'actual_set_pressure_ownership': ','.join(map(str, q or [2,1,1,1,1,1,0,0,0])),
            'actual_set_pressure_rejected_manager': ','.join(map(str, m or [1,80,64,16,8,80,8,80,80,8,8,16,16,16,16,16,16,88])),
            'actual_set_pressure_rejected_issue_order': ','.join(map(str, orders or [40,40])),
        }

    def test_qualified_cleanup_and_no_rejection(self):
        check = verify_regression._check_dirty_pressure_ownership
        check(self.fields(), 1)
        check(self.fields(q=[1,1,1,0,0,0,0,0,0], m=[0]*18, orders=[0,0]), 1)
        check(self.fields(q=[0]*9, m=[0]*18, orders=[0,0]), 0)
        # Two failed construction reasons can overlap in one rejected attempt.
        check(self.fields(q=[2,1,1,1,1,1,0,1,0]), 1)

    def test_lost_or_duplicated_attempt_and_cleanup_are_rejected(self):
        q = [2,1,1,1,1,1,0,0,0]
        for i in range(9):
            bad = q[:]; bad[i] += 2 if i in (6, 7) else 1
            with self.subTest(field=i), self.assertRaises(verify_regression.VerificationError):
                verify_regression._check_dirty_pressure_ownership(self.fields(q=bad), 1)
        with self.assertRaises(verify_regression.VerificationError):
            verify_regression._check_dirty_pressure_ownership(self.fields(), 2)
        # Five attempts for one accepted action exceeds the four-attempt cap.
        with self.assertRaises(verify_regression.VerificationError):
            verify_regression._check_dirty_pressure_ownership(
                self.fields(q=[5,1,1,4,4,4,0,0,0]), 1)

    def test_missing_terminal_effect_or_byte_check_is_rejected(self):
        m = [1,80,64,16,8,80,8,80,80,8,8,16,16,16,16,16,16,88]
        for i in (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17):
            # C can legally include additional clean releases, and global
            # traffic may exceed target traffic; remove evidence here.
            bad=m[:]; bad[i] -= 1
            with self.subTest(field=i), self.assertRaises(verify_regression.VerificationError):
                verify_regression._check_dirty_pressure_ownership(self.fields(m=bad), 1)
        with self.assertRaises(verify_regression.VerificationError):
            verify_regression._check_dirty_pressure_ownership(self.fields(orders=[39,40]), 1)

    def test_rejected_acquisitions_are_observed_without_exact_one_gate(self):
        # Reacquisition and a second dirty release remain byte-checked by the
        # manager; immutable expectation/preserved-store counts are distinct.
        m = [1,80,64,16,8,82,8,80,80,8,8,20,18,22,22,16,16,92]
        verify_regression._check_dirty_pressure_ownership(self.fields(m=m), 1)


if __name__ == '__main__':
    unittest.main()
