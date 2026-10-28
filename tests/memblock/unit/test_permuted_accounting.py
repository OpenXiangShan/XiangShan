"""Reject inconsistent page-permutation observations without coverage gates."""
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
import verify_regression


class PermutedAccountingTest(unittest.TestCase):
    def test_sparse_and_disabled_runs_are_valid(self):
        base = dict(constraint_schema=57, target_translation_permuted=500,
                    permuted_contexts="1,0,0,0,0,1", permuted_kinds="1,0,1,0",
                    permuted_pages=3, permuted_cross_page_uops=1,
                    permuted_split_elements=1)
        verify_regression._check_permuted_translation_accounting(base)
        zero = dict(base, target_translation_permuted=0,
                    permuted_contexts="0,0,0,0,0,0", permuted_kinds="0,0,0,0",
                    permuted_pages=0, permuted_cross_page_uops=0,
                    permuted_split_elements=0)
        verify_regression._check_permuted_translation_accounting(zero)
        verify_regression._check_permuted_translation_accounting(
            dict(zero, target_translation_permuted=1000))
        for field, value in (("target_translation_permuted", True),
                             ("target_translation_permuted", 1001),
                             ("target_translation_permuted", 0),
                             ("permuted_contexts", "1,0,0,0,0,0"),
                             ("permuted_kinds", "1,0,0"),
                             ("permuted_pages", -1),
                             ("permuted_pages", True),
                             ("permuted_pages", 25),
                             ("permuted_pages", 0),
                             ("permuted_cross_page_uops", 3),
                             ("permuted_split_elements", 9)):
            with self.subTest(field=field, value=value), self.assertRaises(
                    verify_regression.VerificationError):
                verify_regression._check_permuted_translation_accounting(
                    dict(base, **{field: value}))
        for field in base:
            if field == "constraint_schema":
                continue
            partial = dict(base)
            del partial[field]
            with self.subTest(missing=field), self.assertRaises(
                    verify_regression.VerificationError):
                verify_regression._check_permuted_translation_accounting(partial)


if __name__ == "__main__":
    unittest.main()
