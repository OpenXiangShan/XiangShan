#!/usr/bin/env python3
"""Reject corrupt stream evidence while allowing sparse legal observations."""
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'scripts'))
import verify_regression as verifier


def sample():
    return dict(constraint_schema=62, transactions=100, target_concurrent=500,
                target_concurrent_stream=750, stream='2,20,20,8,7,9,6,4,40,3,1,90',
                stream_ops='4,4,4,4,4', stream_issue='4,4,4,12,12,4',
                stream_vector_uops='12,12', stream_scalar_retries=2)


class StreamAccountingTest(unittest.TestCase):
    def test_current_sparse_and_old_schema(self):
        verifier._check_stream_accounting(sample())
        verifier._check_stream_accounting({'constraint_schema': 61})
        # One instruction can complete immediately; no overlap/replay is required.
        verifier._check_stream_accounting({**sample(), 'stream': '1,1,1,0,0,0,1,0,1,0,0,1',
            'stream_ops': '1,0,0,0,0', 'stream_issue': '1,0,0,0,0,0',
            'stream_vector_uops': '0,0', 'stream_scalar_retries': 0})
        empty = {**sample(), 'stream': ','.join(['0'] * 12),
                 'stream_ops': '0,0,0,0,0', 'stream_issue': '0,0,0,0,0,0',
                 'stream_vector_uops': '0,0', 'stream_scalar_retries': 0}
        for rate in (0, 750, 1000):
            verifier._check_stream_accounting({**empty, 'target_concurrent_stream': rate})
        verifier._check_stream_accounting({**empty, 'target_concurrent': 0})

    def test_missing_malformed_and_disabled_fields(self):
        for field in ('stream', 'stream_ops', 'stream_issue', 'stream_vector_uops',
                      'stream_scalar_retries', 'target_concurrent_stream'):
            missing = sample(); del missing[field]
            with self.subTest(field=field), self.assertRaises(verifier.VerificationError):
                verifier._check_stream_accounting(missing)
        for field, values in {
            'stream': (True, None, [], '', '1,2', ','.join(['-1'] * 12),
                       ','.join(['False'] * 12), ','.join(['1.0'] * 12)),
            'target_concurrent_stream': (True, -1, 1001, '750'),
            'stream_scalar_retries': (True, -1, '2'),
            'target_concurrent': (0,),
        }.items():
            for value in values:
                with self.subTest(field=field, value=value), self.assertRaises(verifier.VerificationError):
                    verifier._check_stream_accounting({**sample(), field: value})
        with self.assertRaises(verifier.VerificationError):
            verifier._check_stream_accounting({**sample(), 'target_concurrent_stream': 0})

    def test_conservation_bounds_and_placement(self):
        for index, value in ((0, 0), (0, 21), (1, 21), (2, 21), (3, 19),
                             (4, 19), (5, 19), (6, 9), (7, 7), (8, 39),
                             (10, 3), (11, 0), (11, 101)):
            counts = sample()['stream'].split(','); counts[index] = str(value)
            with self.subTest(index=index, value=value), self.assertRaises(verifier.VerificationError):
                verifier._check_stream_accounting({**sample(), 'stream': ','.join(counts)})
        for field, value in (('stream_ops', '4,4,5,4,4'),
                             ('stream_issue', '4,3,5,12,12,4'),
                             ('stream_vector_uops', '13,11'),
                             ('transactions', 19)):
            with self.subTest(field=field), self.assertRaises(verifier.VerificationError):
                verifier._check_stream_accounting({**sample(), field: value})
        single = {**sample(), 'stream': '1,1,1,0,0,0,1,0,1,1,0,1',
                  'stream_ops': '1,0,0,0,0', 'stream_issue': '1,0,0,0,0,0',
                  'stream_vector_uops': '0,0', 'stream_scalar_retries': 0}
        # A replay or STA retry must have a corresponding producer class.
        with self.assertRaises(verifier.VerificationError):
            verifier._check_stream_accounting(single)
        with self.assertRaises(verifier.VerificationError):
            verifier._check_stream_accounting({**single, 'stream': '1,1,1,0,0,0,1,0,1,0,0,1',
                                               'stream_scalar_retries': 1})

    def test_stream_only_disables_legacy_shape_targets_but_rejects_forged_windows(self):
        result = dict(target_concurrent=1000, target_concurrent_stream=1000,
                      actual_concurrent_vector_shape=','.join(['0'] * 16),
                      actual_concurrent_vector_uops='0,0', concurrent='0,0,0,0,0')
        targets = dict(addressing=[1] * 4, eew=[1] * 4, sew=[1] * 4,
                       lmul=[1] * 7, emul=[1] * 7)
        policies = dict(masked=500, partial_vl=500, nonzero_vstart=500)
        check = lambda fields: verifier._check_concurrent_vector_shapes(fields, [1] * 5, targets, policies)
        check(result)
        for field, value in (
                ('target_concurrent_stream', 0), ('target_concurrent_stream', 750),
                ('actual_concurrent_vector_shape', ','.join(['1'] * 16)),
                ('actual_concurrent_vector_uops', '1,1'), ('concurrent', '1,7,0,0,0')):
            with self.subTest(field=field, value=value), self.assertRaises(verifier.VerificationError):
                check({**result, field: value})

    def test_stream_only_strict_coverage_keeps_concurrency_requirements(self):
        # Existing broad fixture supplies the other strict bins independently.
        from test_verify_regression import mixed_result
        result = mixed_result(1)
        result.update(transactions=verifier.ENHANCED_MIXED_COVERAGE_TRANSACTIONS,
                      store_misaligned='1,1', vector_replays=0,
                      virtualization=1, exceptions=1, target_concurrent_stream=1000,
                      stream_ops='4,4,4,4,4', stream='4,20,20,2,2,3,4,2,40,0,1,20')
        verifier._check_mixed_coverage(result)
        for stream in ('0,0,0,0,0,0,0,0,0,0,0,0', '3,20,20,2,2,3,4,2,40,0,1,20',
                       '4,19,19,2,2,3,4,2,40,0,1,20', '4,20,20,0,2,3,4,2,40,0,1,20',
                       '4,20,20,2,2,3,4,1,40,0,1,20'):
            with self.subTest(stream=stream), self.assertRaises(verifier.VerificationError):
                verifier._check_mixed_coverage({**result, 'stream': stream})


if __name__ == '__main__':
    unittest.main()
