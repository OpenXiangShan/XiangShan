#!/usr/bin/env python3
"""Independent accepted-operand histories qualify schema76 pair/skew accounting."""
import copy
import random
import unittest

from test_stream_cancellation import csv, history, verifier


class StreamStorePairAccountingTest(unittest.TestCase):
    def test_accepted_operand_histories_and_disabled_controls(self):
        checked = 0
        for seed in range(256):
            result, records, _ = history(seed)
            result.update(constraint_schema=76, target_stream_store_pair=500)
            rng = random.Random(seed + 9000)
            # One retired ordinary store plus fully accepted canceled stores.
            # Pairing is assigned to actual instruction owners, never pooled
            # across partial operands or unresolved-graph stores.
            owners = [True] + [r[5] and r[6] for r in records if r[0] == 2]
            edge_pairs = [(rng.randrange(9), rng.randrange(9)) for complete in owners
                          if complete and rng.randrange(2)]
            together = sum(a == d for a, d in edge_pairs)
            a_first = sum(a < d for a, d in edge_pairs)
            d_first = sum(d < a for a, d in edge_pairs)
            skews = [abs(a - d) for a, d in edge_pairs]
            result['stream_store_pair'] = csv([together, a_first, d_first, sum(skews), max(skews, default=0)])
            verifier._check_stream_accounting(result)
            disabled = dict(result, target_stream_store_pair=0, stream_store_pair='0,0,0,0,0')
            verifier._check_stream_accounting(disabled)
            old = dict(result, constraint_schema=75)
            del old['stream_store_pair']; del old['target_stream_store_pair']
            verifier._check_stream_accounting(old)
            checked += 3
        print(f'stream_store_pair_accounting_histories={checked}')

    def test_impossible_pair_populations_and_skew_records_reject(self):
        base = history(19)[0]
        base.update(constraint_schema=76, target_stream_store_pair=500, stream_store_pair='0,0,0,0,0')
        verifier._check_stream_accounting(base)
        sta = list(map(int, base['stream_sta'].split(',')))
        bad = [
            dict(base, stream_store_pair=f'{sta[0]+1},0,0,0,0'),
            dict(base, stream_store_pair='0,0,0,1,1'),
            dict(base, stream_store_pair='0,1,0,0,0'),
            dict(base, stream_store_pair='0,1,0,1,2'),
            dict(base, stream_store_pair='0,1,0,2,1'),
            dict(base, stream_store_pair=f'0,1,0,{1 << 64},{1 << 64}'),
            dict(base, stream_store_pair='1,0,0,0,0', target_stream_store_pair=0),
            dict(base, stream_store_pair='1,0,0,-1,0'),
            dict(base, stream_store_pair='1,0,0,0'),
        ]
        for rate in (None, '500', -1, 1001, True):
            bad.append(dict(base, target_stream_store_pair=rate))
        for item in bad:
            with self.subTest(item=item.get('stream_store_pair'), rate=item.get('target_stream_store_pair')):
                with self.assertRaises(verifier.VerificationError):
                    verifier._check_stream_accounting(item)
        missing = copy.deepcopy(base); del missing['stream_store_pair']
        with self.assertRaises(verifier.VerificationError): verifier._check_stream_accounting(missing)


if __name__ == '__main__':
    unittest.main()
