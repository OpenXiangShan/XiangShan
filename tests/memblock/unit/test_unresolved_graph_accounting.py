#!/usr/bin/env python3
"""Check multi-store recovery evidence without requiring positive sparse bins."""
import sys
import itertools
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'scripts'))
import verify_regression as verifier


def sample():
    return dict(constraint_schema=63, transactions=64, target_scalar_unresolved=100,
                target_scalar_unresolved_conflict=500,
                scalar_unresolved='4,2,2,4,2,2,2,4,2,9,0',
                scalar_unresolved_graph='10,3,4,16,5,4,6,6,6',
                scalar_unresolved_first_conflict='0,1,1,0')


class UnresolvedGraphAccountingTest(unittest.TestCase):
    def test_sparse_mixed_and_single_store_evidence(self):
        check = verifier._check_scalar_unresolved_accounting
        check(sample())
        check({**sample(), 'scalar_unresolved': '0,0,0,0,0,0,0,0,0,0,0',
               'scalar_unresolved_graph': '0,0,0,0,0,0,0,0,0',
               'scalar_unresolved_first_conflict': '0,0,0,0'})
        check({**sample(), 'scalar_unresolved': '1,1,0,1,1,1,1,1,0,1,0',
               'scalar_unresolved_graph': '1,0,0,2,0,1,1,1,0',
               'scalar_unresolved_first_conflict': '1,0,0,0'})
        old = sample(); old['constraint_schema'] = 62
        del old['scalar_unresolved_graph']; del old['scalar_unresolved_first_conflict']
        check(old)

    def test_window_conflict_endpoints_preserve_full_store_prefix(self):
        base = {**sample(), 'scalar_unresolved_graph': '5,1,2,8,3,4,0,3,2',
                'scalar_unresolved_first_conflict': '0,0,0,0'}
        verifier._check_scalar_unresolved_accounting({**base,
            'scalar_unresolved': '2,0,2,2,0,0,0,2,1,0,0',
            'target_scalar_unresolved_conflict': 0})
        verifier._check_scalar_unresolved_accounting({**base,
            'scalar_unresolved': '2,2,0,2,2,2,2,2,1,9,0',
            'scalar_unresolved_graph': '5,1,2,8,3,4,5,3,2',
            'scalar_unresolved_first_conflict': '1,0,0,1',
            'target_scalar_unresolved_conflict': 1000})

    def test_rejects_missing_malformed_and_unconserved_graph_fields(self):
        check = verifier._check_scalar_unresolved_accounting
        for field in ('scalar_unresolved_graph', 'scalar_unresolved_first_conflict'):
            missing = sample(); del missing[field]
            with self.subTest(missing=field), self.assertRaises(verifier.VerificationError): check(missing)
            for value in (None, True, [], '', '1,2', '-1', '1.0', 'True'):
                with self.subTest(field=field, value=value), self.assertRaises(verifier.VerificationError):
                    check({**sample(), field: value})
        for index, value in ((0, 3), (0, 17), (1, 0), (1, 5), (2, 1), (2, 9),
                             (3, 15), (4, 13), (5, 0), (5, 1), (5, 5),
                             (6, 1), (6, 9), (7, 3), (7, 9), (8, 13)):
            counts = sample()['scalar_unresolved_graph'].split(','); counts[index] = str(value)
            with self.subTest(index=index, value=value), self.assertRaises(verifier.VerificationError):
                check({**sample(), 'scalar_unresolved_graph': ','.join(counts)})
        for first in ('0,0,0,0', '0,1,1,1', '0,0,0,2'):
            with self.subTest(first=first), self.assertRaises(verifier.VerificationError):
                check({**sample(), 'scalar_unresolved_first_conflict': first})

    def test_joint_peak_and_early_window_feasibility(self):
        # Three multi-store windows cannot total six stores while one has four.
        impossible_peak = {**sample(), 'scalar_unresolved': '3,3,0,3,3,3,3,3,1,3,0',
            'scalar_unresolved_graph': '6,3,1,11,0,4,6,3,0',
            'scalar_unresolved_first_conflict': '3,0,0,0'}
        # Exactly one early window with one store cannot supply three STD.
        impossible_early = {**sample(), 'scalar_unresolved': '3,3,0,3,3,3,3,3,1,3,0',
            'scalar_unresolved_graph': '3,0,3,3,0,1,3,3,0',
            'scalar_unresolved_first_conflict': '3,0,0,0'}
        for fields in (impossible_peak, impossible_early):
            with self.subTest(fields=fields), self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting(fields)
        # Both ends remain legal when each event's supplying window exists.
        valid = {**sample(), 'scalar_unresolved': '3,3,0,3,3,3,3,3,1,3,0',
            'scalar_unresolved_graph': '6,1,4,8,0,4,6,3,0',
            'scalar_unresolved_first_conflict': '3,0,0,0'}
        verifier._check_scalar_unresolved_accounting(valid)  # The one early window contains four stores.

    def test_peak_correlates_with_prefix_and_pair_capacities(self):
        cases = [
            {**sample(), 'scalar_unresolved': '5,1,4,5,1,1,1,5,0,1,0',
             'scalar_unresolved_graph': '9,4,0,18,0,2,4,5,0',
             'scalar_unresolved_first_conflict': '1,0,0,0'},
            {**sample(), 'scalar_unresolved': '2,2,0,2,2,2,2,2,0,2,0',
             'scalar_unresolved_graph': '4,2,0,8,4,2,4,2,4',
             'scalar_unresolved_first_conflict': '2,0,0,0'},
        ]
        for fields in cases:
            with self.subTest(fields=fields), self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting(fields)

    def test_enumerated_realizable_store_depth_and_early_operand_layouts(self):
        # Enumerate explicit windows; do not recreate the aggregate bound.
        # Stores can occupy distinct bytes of one word under an 8-byte load,
        # making every window conflict without inter-store byte overlap.
        for windows in range(1, 5):
            for depths in itertools.product(range(1, 5), repeat=windows):
                stores = sum(depths); multi = sum(depth > 1 for depth in depths)
                for early_by_window in itertools.product(*(range(depth + 1) for depth in depths)):
                    early = sum(early_by_window); before = sum(count > 0 for count in early_by_window)
                    fields = {**sample(),
                        'scalar_unresolved': ','.join(map(str, (windows, windows, 0, windows,
                            windows, windows, windows, windows, before, stores, 0))),
                        'scalar_unresolved_graph': ','.join(map(str, (stores, multi, early,
                            2 * stores - early, 0, max(depths), stores, windows, 0))),
                        'scalar_unresolved_first_conflict': f'{windows},0,0,0'}
                    try:
                        verifier._check_scalar_unresolved_accounting(fields)
                    except verifier.VerificationError as error:
                        self.fail(f'legal depths={depths} early={early_by_window}: {error}')

    def test_counter_domain_and_completed_action_limit(self):
        for value in (None, True, '64', -1, 3, 1 << 64):
            with self.subTest(transactions=value), self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting({**sample(), 'transactions': value})
        # A scaling that preserves arithmetic must still fit actual uint64 counters.
        huge = 1 << 64
        fields = {**sample(), 'transactions': (1 << 64) - 1,
            'scalar_unresolved': f'{huge},0,{huge},{huge},0,0,0,{huge},0,0,0',
            'scalar_unresolved_graph': f'{huge},0,0,{2*huge},0,1,0,{huge},0',
            'scalar_unresolved_first_conflict': '0,0,0,0'}
        with self.assertRaises(verifier.VerificationError):
            verifier._check_scalar_unresolved_accounting(fields)

    def test_rejects_impossible_peak_and_rank_without_positive_coverage_gate(self):
        base = {**sample(), 'scalar_unresolved': '2,2,0,2,2,2,2,2,1,9,0',
                'scalar_unresolved_graph': '5,1,2,8,3,4,5,3,2',
                'scalar_unresolved_first_conflict': '1,0,0,1'}
        # Five stores in two windows cannot have an observed maximum of two.
        with self.assertRaises(verifier.VerificationError):
            verifier._check_scalar_unresolved_accounting({**base,
                'scalar_unresolved_graph': '5,1,2,8,3,2,5,3,2'})
        # A fourth-address redirect cannot occur in a two-store maximum run.
        with self.assertRaises(verifier.VerificationError):
            verifier._check_scalar_unresolved_accounting({**base,
                'scalar_unresolved_graph': '4,2,2,6,1,2,4,3,1'})


if __name__ == '__main__':
    unittest.main()
