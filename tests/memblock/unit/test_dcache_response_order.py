#!/usr/bin/env python3
"""Independent DCache accepted-A/C, D-message and E histories."""
from pathlib import Path
import random
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'scripts'))
import verify_regression as verifier


class DcacheResponseOrderTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tmp = tempfile.TemporaryDirectory(prefix='memblock-dcache-order-')
        cls.addClassCleanup(cls.tmp.cleanup)
        cls.fixture = (ROOT / 'unit/dcache_response_order_fixture.cpp').read_text()
        cls.production = (ROOT / 'cpp/environment/dcache_agent.inc').read_text()
        cls.binary = cls.compile('golden', cls.production)

    @classmethod
    def compile(cls, name, production):
        path = Path(cls.tmp.name) / name
        path.mkdir()
        inc = path / 'agent.inc'
        inc.write_text(production)
        source = path / 'fixture.cpp'
        source.write_text(cls.fixture.replace('"environment/dcache_agent.inc"', '"' + str(inc) + '"'))
        binary = path / 'fixture'
        r = subprocess.run(['c++', '-std=c++20', '-O0', '-I', str(ROOT / 'cpp'), str(source), '-o', str(binary)],
                           capture_output=True, text=True, timeout=60)
        if r.returncode:
            raise AssertionError(r.stdout + r.stderr)
        return binary

    def run_case(self, mode, binary=None):
        return subprocess.run([str(binary or self.binary), mode], capture_output=True, text=True, timeout=30)

    def test_mixed_namespaces_messages_snapshot_stability_and_structural_diversity(self):
        for mode in ['random', 'backpressure']:
            r = self.run_case(mode)
            self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
            self.assertIn('schedules=128 ', r.stdout)
            self.assertEqual(self.run_case(mode).stdout, r.stdout)
            print(mode + ': ' + r.stdout.strip())

    def test_disabled_direction_retains_fifo_and_exactly_once_ownership(self):
        r = self.run_case('fifo')
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
        self.assertIn('schedules=1 nonfront=0', r.stdout)

    def test_held_and_deferred_same_line_responses_keep_existing_eligibility(self):
        for mode in ['held', 'deferred', 'errors', 'config']:
            r = self.run_case(mode)
            self.assertEqual(r.returncode, 0, r.stdout + r.stderr)

    def test_private_order_snapshot_interleave_and_stalled_switch_faults_reject(self):
        faults = {
            'fifo': ('const auto selected = draw() % starts.size();', 'const auto selected = 0U;'),
            'payload': ('base + memory_beat * kBeatBytes', 'base + (memory_beat ^ 1U) * kBeatBytes'),
            'interleave': ('response_message_active_ = false;\n            }\n            d_beats_.pop_front();',
                           'response_message_active_ = false;\n            } else { response_message_active_ = false; }\n            d_beats_.pop_front();'),
            'stalled-switch': ('if (!response_message_active_) {', 'if (!response_message_active_ || d_presenting_) {'),
        }
        for name, (before, after) in faults.items():
            with self.subTest(name=name):
                self.assertEqual(self.production.count(before), 1)
                binary = self.compile(name, self.production.replace(before, after))
                r = self.run_case('random', binary)
                self.assertNotEqual(r.returncode, 0, 'private fault escaped: ' + name)
                print('dcache_order_mutant=' + name + ' diagnostic=' + r.stderr.strip())

    def test_parser_rate_endpoints_and_current_schema(self):
        for value in ['0', '1', '250', '999', '1000', '1001', '-1', '1.5', '1000x', '']:
            r = subprocess.run([str(self.binary), 'constraints', 'dcache-response-reorder=' + value],
                               capture_output=True, text=True, timeout=10)
            valid = value in ['0', '1', '250', '999', '1000']
            self.assertEqual(r.returncode == 0, valid, r.stdout + r.stderr)
            if valid:
                self.assertIn('target_dcache_response_reorder=' + value + ' ', r.stdout)
                self.assertIn('constraint_schema=83', r.stdout)

    def test_independent_deferred_and_release_histories_allow_distinct_inversion_populations(self):
        # Pending A may be delayed by same-line ownership or explicit holds;
        # ready D messages therefore need not follow accepted A order even at rate0.
        total_fifo_overtakes = 0
        for seed in range(128):
            for rate in [0, 250, 1000]:
                rng = random.Random(seed)
                pending, queued = {}, []
                accepted = c_releases = order = 0
                count = seed % 65
                row = [0] * 10
                for time in range(10000):
                    if accepted + c_releases < count:
                        for _ in range(min(count - accepted - c_releases, rng.randrange(1, 5))):
                            if rng.randrange(3):
                                pending[order] = time + rng.randrange(1, 8)
                                accepted += 1
                            else:
                                queued.append(('c', order))
                                c_releases += 1
                            order += 1
                    for age, ready in list(pending.items()):
                        if ready <= time:
                            queued.append(('a', age))
                            pending[age] = 100000  # remain outstanding until final D
                    if not queued:
                        if accepted + c_releases == count and not pending:
                            break
                        continue
                    row[0] += 1
                    row[1] += len(queued) > 1
                    row[5] = max(row[5], len(queued))
                    choose = len(queued) > 1 and rng.randrange(1000) < rate
                    row[2] += choose
                    index = rng.randrange(len(queued)) if choose else 0
                    row[3] += index != 0
                    kind, age = queued.pop(index)
                    if kind == 'a':
                        older = sum(other < age for other in pending)
                        row[6] += older != 0
                        row[7] = max(row[7], older)
                        row[9] += 1
                        del pending[age]
                    else:
                        row[8] += 1
                    row[4] += 1
                else:
                    self.fail('independent history did not drain')
                if rate == 0:
                    total_fifo_overtakes += row[6]
                verifier._check_dcache_response_order(dict(constraint_schema=83,
                    target_dcache_response_reorder=rate, tilelink_requests=accepted,
                    dcache_releases=c_releases, dcache_response_order=','.join(map(str, row))))
        self.assertGreater(total_fifo_overtakes, 0)
        print('dcache_independent_histories=384 fifo_accepted_age_overtakes=' + str(total_fifo_overtakes))

    def test_sparse_endpoints_and_corrupted_ledgers(self):
        base = dict(constraint_schema=83, target_dcache_response_reorder=1000,
                    tilelink_requests=6, dcache_releases=2, dcache_response_order='8,6,6,4,8,7,4,3,2,6')
        check = verifier._check_dcache_response_order
        check(base)
        check(dict(constraint_schema=83, target_dcache_response_reorder=0,
                   tilelink_requests=0, dcache_releases=0, dcache_response_order='0,0,0,0,0,0,0,0,0,0'))
        # Deferred accepted older A can be overtaken with one selectable queued message.
        check(dict(constraint_schema=83, target_dcache_response_reorder=0,
                   tilelink_requests=2, dcache_releases=0, dcache_response_order='2,0,0,0,2,1,1,1,0,2'))
        for index in range(10):
            row = [8,6,6,4,8,7,4,3,2,6]
            row[index] += 100
            with self.assertRaises(verifier.VerificationError):
                check(dict(base, dcache_response_order=','.join(map(str, row))))
        for missing in ['target_dcache_response_reorder','tilelink_requests','dcache_releases','dcache_response_order']:
            record = dict(base)
            del record[missing]
            with self.assertRaises(verifier.VerificationError):
                check(record)
        for invalid in ['', '1,2', '-1,0,0,0,0,0,0,0,0,0', '18446744073709551616,0,0,0,0,0,0,0,0,0']:
            with self.assertRaises(verifier.VerificationError):
                check(dict(base, dcache_response_order=invalid))
        for impossible in ['2,0,0,0,2,2,0,0,0,2', '2,2,2,0,2,2,0,0,0,2']:
            with self.assertRaises(verifier.VerificationError):
                check(dict(base, tilelink_requests=2,dcache_releases=0,dcache_response_order=impossible))
        with self.assertRaises(verifier.VerificationError):
            check(dict(base, tilelink_requests=8, dcache_releases=0,
                       dcache_response_order='8,1,1,1,8,7,0,0,0,8'))
        for rate in [True, -1, 1001, '250', None]:
            with self.assertRaises(verifier.VerificationError):
                check(dict(base, target_dcache_response_reorder=rate))
        check(dict(constraint_schema=81))
        # Current and preceding schemas reach coverage validation rather than
        # being rejected by a stale strict-mode schema allowlist.
        for schema in [80, 81, 82]:
            with self.assertRaisesRegex(verifier.VerificationError, "target_translation"):
                verifier._check_constraint_coverage(dict(constraint_schema=schema))


if __name__ == '__main__':
    unittest.main()
