#!/usr/bin/env python3
"""Check replay/survivor conservation against explicitly realizable load groups."""
import importlib.util
import itertools
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))
SPEC = importlib.util.spec_from_file_location("verify_regression", ROOT / "scripts/verify_regression.py")
verifier = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(verifier)


def population(layouts):
    # Each tuple is (load count, victim index or count for controls,
    # number of disjoint invalidated loads, issue inversions, observed LQ cancellations).
    windows = len(layouts)
    conflicts = sum(victim < count for count, victim, _, _, _ in layouts)
    controls = windows - conflicts
    loads = sum(count for count, *_ in layouts)
    invalidated = sum(count - victim for count, victim, _, _, _ in layouts)
    disjoint = sum(disjoint for _, _, disjoint, _, _ in layouts)
    victims = [sum(victim == age and victim < count for count, victim, _, _, _ in layouts)
               for age in range(4)]
    values = (loads, sum(count > 1 for count, *_ in layouts), loads, loads - invalidated,
              invalidated, invalidated, disjoint, invalidated - disjoint,
              invalidated - disjoint - conflicts, sum(i for _, _, _, i, _ in layouts),
              max((count for count, *_ in layouts), default=0))
    return dict(constraint_schema=64, transactions=max(windows, 1), target_scalar_unresolved=1000,
                target_scalar_unresolved_conflict=500,
                scalar_unresolved=','.join(map(str, (windows, conflicts, controls, windows,
                    conflicts, conflicts, conflicts, windows, 0, conflicts,
                    sum(canceled for *_, canceled in layouts)))),
                scalar_unresolved_graph=','.join(map(str, (windows, 0, 0, 2*windows, 0,
                    int(windows > 0), conflicts, windows, 0))),
                scalar_unresolved_first_conflict=f'{conflicts},0,0,0',
                scalar_unresolved_load_group=','.join(map(str, values)),
                scalar_unresolved_victim=','.join(map(str, victims)))


class UnresolvedLoadGroupAccountingTest(unittest.TestCase):
    def test_realizable_single_and_mixed_populations(self):
        layouts = []
        for count in range(1, 5):
            for victim in range(count+1):
                for disjoint in range(max(1, count-victim)):
                    for inversions in (0, count*(count-1)//2):
                        for canceled in (0, count-victim):
                            layout = (count, victim, disjoint, inversions, canceled)
                            verifier._check_scalar_unresolved_accounting(population([layout]))
                            layouts.append(layout)
        verifier._check_scalar_unresolved_accounting(population([]))
        for first, second in itertools.product(layouts, repeat=2):
            verifier._check_scalar_unresolved_accounting(population([first, second]))

    def test_all_four_window_age_count_profiles(self):
        types = [(count, victim, 0, 0, 0) for count in range(1, 5) for victim in range(count+1)]
        for layouts in itertools.product(types, repeat=4):
            verifier._check_scalar_unresolved_accounting(population(layouts))

    def test_multi_window_conflict_control_correlations(self):
        base = population([(2, 1, 0, 0, 0), (2, 1, 0, 0, 0), (3, 3, 0, 0, 0)])
        # Both age1 victims need separate multi-load windows. The control's
        # three loads need another, contradicting a claimed total of two.
        values = base['scalar_unresolved_load_group'].split(',')
        values[1] = '2'; values[10] = '4'
        with self.assertRaises(verifier.VerificationError):
            verifier._check_scalar_unresolved_accounting({**base,
                'scalar_unresolved_load_group': ','.join(values)})
        # Independent review's correlation: all four windows claim multi,
        # but its surviving prefix leaves only one load in the lone control.
        reviewed = population([(4, 0, 0, 0, 0), (4, 1, 1, 0, 0),
                               (4, 1, 1, 0, 0), (1, 1, 0, 0, 0)])
        values = reviewed['scalar_unresolved_load_group'].split(','); values[1] = '4'
        with self.assertRaises(verifier.VerificationError):
            verifier._check_scalar_unresolved_accounting({**reviewed,
                'scalar_unresolved_load_group': ','.join(values)})

        # Positive-age victims consume their own prefix extras. With exactly
        # one invalidated load per conflict, the age-zero victim is single.
        prefix = population([(1, 0, 0, 0, 0), (4, 3, 0, 0, 0), (4, 4, 0, 0, 0)])
        values = prefix['scalar_unresolved_load_group'].split(','); values[1] = '3'
        with self.assertRaises(verifier.VerificationError):
            verifier._check_scalar_unresolved_accounting({**prefix,
                'scalar_unresolved_load_group': ','.join(values)})

    def test_missing_malformed_overflow_fields(self):
        base = population([(4, 1, 1, 3, 2), (2, 2, 0, 1, 0)])
        for field in ('scalar_unresolved_load_group', 'scalar_unresolved_victim'):
            missing = dict(base); del missing[field]
            with self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting(missing)
            for bad in ('', '-1', '1,2', 'true', '0,0,0,0,0,0,0,0,0,0,18446744073709551616'):
                with self.assertRaises(verifier.VerificationError):
                    verifier._check_scalar_unresolved_accounting({**base, field: bad})

    def test_impossible_suffix_counts_and_victim_positions(self):
        base = population([(4, 1, 1, 3, 2), (2, 2, 0, 1, 0)])
        for index, value in ((0, 5), (1, 0), (2, 5), (3, 2), (4, 2), (5, 2),
                             (6, 4), (7, 0), (8, 3), (9, 20), (10, 2)):
            values = base['scalar_unresolved_load_group'].split(','); values[index] = str(value)
            with self.subTest(index=index), self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting({**base,
                    'scalar_unresolved_load_group': ','.join(values)})
        for bad in ('0,0,0,0', '0,0,0,1', '2,0,0,0'):
            with self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting({**base, 'scalar_unresolved_victim': bad})
        values = base['scalar_unresolved'].split(','); values[-1] = '4'
        with self.assertRaises(verifier.VerificationError):
            verifier._check_scalar_unresolved_accounting({**base, 'scalar_unresolved': ','.join(values)})


if __name__ == '__main__':
    unittest.main()
