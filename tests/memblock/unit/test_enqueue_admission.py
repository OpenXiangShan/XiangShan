#!/usr/bin/env python3
"""Host behavioral contracts for LSQ admission and sampled enqueue cadence.

The C++ shell has synthetic public ports and edges. It compiles the production
QueueAccounting declaration/methods and cycle/admission helpers, then checks an
independently specified edge schedule. Private mutations are compiled only in a
temporary directory; these tests never edit production or construct a DUT model.
Set MEMBLOCK_ENQUEUE_ADMISSION_ARTIFACT_DIR to retain commands and fault evidence.
"""
import hashlib
import importlib.util
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PRODUCTION_PATHS = (
    'cpp/environment/queue_accounting.hpp',
    'cpp/environment/queue_accounting.cpp',
    'cpp/environment/cycle.cpp',
    'cpp/environment/model.inc',
    'cpp/memblock_env.hpp',
    'cpp/generated_interface_types.hpp',
    'cpp/failure.hpp',
)


def production_hashes():
    return {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
            for path in PRODUCTION_PATHS}


def method(source, signature):
    start = source.index(signature)
    return source[start:source.index('\n}\n', start) + 3]


def fixture_source():
    source = (ROOT / 'unit/enqueue_admission_fixture.cpp').read_text()
    header = (ROOT / 'cpp/environment/queue_accounting.hpp').read_text()
    queue_class = header[header.index('class QueueAccounting {'):
                         header.index('\n} // namespace memblock')]
    queue = (ROOT / 'cpp/environment/queue_accounting.cpp').read_text()
    queue = queue[queue.index('void QueueAccounting::'):
                  queue.rindex('} // namespace memblock')]
    source = source.replace('// PRODUCTION_QUEUE_ACCOUNTING',
                            (queue_class + '\n' + queue).replace('DutAdapter', 'FixtureDut'))
    cycle = (ROOT / 'cpp/environment/cycle.cpp').read_text()
    helpers = [method(cycle, signature).replace('Environment::Impl::', 'AdmissionFixture::')
               for signature in ('auto Environment::Impl::wait_for_enqueue_capacity(',
                                 'auto Environment::Impl::tick(bool monitor)')]
    return source.replace('// PRODUCTION_METHODS', '\n'.join(helpers))


class EnqueueAdmissionTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.production_before = production_hashes()
        cls.workspace = tempfile.TemporaryDirectory(prefix='memblock-enqueue-admission-')
        cls.addClassCleanup(cls.workspace.cleanup)
        configured = os.environ.get('MEMBLOCK_ENQUEUE_ADMISSION_ARTIFACT_DIR')
        cls.artifacts = Path(configured).resolve() if configured else None
        if cls.artifacts:
            cls.artifacts.mkdir(parents=True, exist_ok=True)
        cls.evidence = []
        cls.source = fixture_source()
        cls.binary = cls.compile_fixture(cls.source, 'production')
        cls.assert_production_unchanged()

    @classmethod
    def assert_production_unchanged(cls):
        if production_hashes() != cls.production_before:
            raise AssertionError('production source changed during enqueue admission tests')

    @classmethod
    def compile_fixture(cls, source, name):
        path = Path(cls.workspace.name) / (name + '.cpp')
        path.write_text(source)
        binary = path.with_suffix('')
        command = ['c++', '-std=c++20', '-O0', '-I', str(ROOT / 'cpp'),
                   str(path), '-o', str(binary)]
        result = subprocess.run(command, text=True, capture_output=True, timeout=60)
        cls.evidence.append({'name': name, 'phase': 'compile', 'command': command,
                             'returncode': result.returncode,
                             'stdout': result.stdout, 'stderr': result.stderr})
        if result.returncode:
            raise RuntimeError(result.stdout + result.stderr)
        return binary

    @classmethod
    def tearDownClass(cls):
        if cls.artifacts:
            (cls.artifacts / 'evidence.json').write_text(json.dumps({
                'production_hashes': cls.production_before,
                'evidence': cls.evidence,
            }, indent=2) + '\n')
        cls.assert_production_unchanged()

    def check_case(self, name, binary=None, fault=None):
        command = [str(binary or self.binary), name]
        result = subprocess.run(command, text=True, capture_output=True, timeout=20)
        self.evidence.append({'name': name, 'phase': 'private-fault' if fault else 'baseline',
                              'fault': fault, 'command': command,
                              'returncode': result.returncode,
                              'stdout': result.stdout, 'stderr': result.stderr})
        if fault:
            self.assertNotEqual(result.returncode, 0,
                                f'private fault {fault} escaped {name}')
            self.assertTrue(result.stderr.strip(), f'private fault {fault} lacked diagnostic')
        else:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_existing_space_admits_consecutive_edges_without_retirement_or_settle(self):
        self.check_case('no_credit_consecutive')

    def test_both_post_admission_watermarks_apply_even_with_zero_opposite_need(self):
        self.check_case('watermarks')

    def test_public_dequeue_is_immediate_but_reuse_needs_three_more_edges(self):
        self.check_case('dequeue_boundary')

    def test_multiple_dequeues_keep_separate_credit_maturity_edges(self):
        self.check_case('distinct_credit_ages')

    def test_explicit_lq_and_sq_cancellation_credit_matures_at_third_edge(self):
        self.check_case('explicit_cancellation_boundary')

    def test_observed_redirect_cancellation_preserves_immediate_provenance_and_delayed_credit(self):
        self.check_case('observed_cancellation_boundary')

    def test_cancellation_during_guard_still_needs_its_own_credit_age(self):
        self.check_case('cancellation_during_recovery')

    def test_redirect_guard_is_six_further_edges_and_restarts_on_every_high(self):
        self.check_case('redirect_restart')

    def test_cycle_captures_redirect_before_edge_and_ages_unmonitored_ticks(self):
        self.check_case('redirect_capture')

    def test_immediate_conservation_errors_precede_guard_and_failed_cancellation_is_atomic(self):
        self.check_case('immediate_conservation')

    def test_holey_batches_count_elements_and_cadence_tracks_older_queue_work(self):
        self.check_case('batch_holes_and_cadence')

    def test_cadence_uses_immediate_outstanding_work_even_while_credit_is_immature(self):
        self.check_case('retired_work_cadence')

    def test_wait_returns_at_maturity_without_extra_edges_and_stops_on_accounting_error(self):
        self.check_case('wait_for_mature_credit')

    def test_private_faults_are_detected_without_editing_production(self):
        # Each textual transformation changes compiled production logic in a
        # private copy. Baseline expected schedules above remain unchanged.
        faults = (
            ('unconditional_settle', 'no_credit_consecutive',
             'if (*capacity_ok) return true;',
             'if (*capacity_ok) { tick(); return true; }'),
            ('reuse_immediate_lq_credit', 'dequeue_boundary',
             'lq_allocated_ - lq_admission_retired_', 'lq_allocated_ - lq_retired'),
            ('reuse_immediate_sq_credit', 'dequeue_boundary',
             'sq_allocated_ - sq_admission_retired_', 'sq_allocated_ - sq_retired'),
            ('credit_one_edge_early', 'dequeue_boundary',
             'completed_edges_ + kLsqAdmissionCreditDelay', 'completed_edges_ + 2'),
            ('credit_one_edge_late', 'dequeue_boundary',
             'completed_edges_ + kLsqAdmissionCreditDelay', 'completed_edges_ + 4'),
            ('lq_reserved_headroom_admitted', 'watermarks',
             'kVirtualLoadQueueEntries - kLqEnqueueHeadroom', 'kVirtualLoadQueueEntries'),
            ('sq_reserved_headroom_admitted', 'watermarks',
             'kStoreQueueEntries - kSqEnqueueHeadroom', 'kStoreQueueEntries'),
            ('skip_zero_need_lq_watermark', 'watermarks',
             'lq_outstanding + lq_needed <=\n            kVirtualLoadQueueEntries - kLqEnqueueHeadroom',
             '(lq_needed == 0 || lq_outstanding + lq_needed <=\n'
             '            kVirtualLoadQueueEntries - kLqEnqueueHeadroom)'),
            ('skip_zero_need_sq_watermark', 'watermarks',
             'sq_outstanding + sq_needed <=\n            kStoreQueueEntries - kSqEnqueueHeadroom',
             '(sq_needed == 0 || sq_outstanding + sq_needed <=\n'
             '            kStoreQueueEntries - kSqEnqueueHeadroom)'),
            ('redirect_one_edge_early', 'redirect_restart',
             'completed_edges_ + kLsqRedirectRecoveryEdges', 'completed_edges_ + 5'),
            ('held_redirect_does_not_restart', 'redirect_restart',
             'if (redirect) {', 'if (redirect && completed_edges_ >= admission_blocked_until_) {'),
            ('lq_cancellation_missing_credit', 'explicit_cancellation_boundary',
             'stage_admission_credit(count, 0);', 'stage_admission_credit(0, 0);'),
            ('sq_cancellation_missing_credit', 'explicit_cancellation_boundary',
             'stage_admission_credit(0, count);', 'stage_admission_credit(0, 0);'),
            ('observed_cancellation_missing_credit', 'observed_cancellation_boundary',
             'stage_admission_credit(lq_canceled, sq_canceled);', 'stage_admission_credit(0, 0);'),
            ('over_retire_check_disabled', 'immediate_conservation',
             'if (lq_retired > lq_allocated_ || sq_retired > sq_allocated_) {', 'if (false) {'),
            ('current_lq_counted_as_older', 'batch_holes_and_cadence',
             'const bool older_lq = lq_allocated_ >', 'const bool older_lq = lq_enqueued_observed_ >'),
            ('current_sq_counted_as_older', 'batch_holes_and_cadence',
             'const bool older_sq = sq_allocated_ >', 'const bool older_sq = sq_enqueued_observed_ >'),
            ('cadence_ignores_real_edge_gaps', 'batch_holes_and_cadence',
             '*last_enqueue_edge_ + 1 == completed_edges_', 'true'),
            ('unmonitored_tick_does_not_age', 'redirect_capture',
             'queues_.sample_after_edge(dut_, redirect);',
             'if (monitor) queues_.sample_after_edge(dut_, redirect);'),
            ('redirect_sampled_after_step', 'redirect_capture',
             'const bool redirect = dut_.queues.redirect.valid.boolean();\n    dut_.step();',
             'dut_.step();\n    const bool redirect = dut_.queues.redirect.valid.boolean();'),
        )
        for name, case, original, replacement in faults:
            with self.subTest(fault=name):
                count = self.source.count(original)
                # Watermark constants appear in both queue and wait helpers;
                # changing both still checks a single policy fault.
                self.assertGreater(count, 0, f'private fault anchor disappeared: {name}')
                mutated = self.source.replace(original, replacement)
                binary = self.compile_fixture(mutated, name)
                self.check_case(case, binary, name)
                self.assert_production_unchanged()


class EnqueueCadenceLedgerTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # Import the real verifier and its neighboring controller module.
        sys.path.insert(0, str(ROOT / 'scripts'))
        cls.addClassCleanup(sys.path.remove, str(ROOT / 'scripts'))
        spec = importlib.util.spec_from_file_location(
            'enqueue_admission_verify_regression', ROOT / 'scripts/verify_regression.py')
        cls.verifier = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(cls.verifier)
        cls.production_before = hashlib.sha256(
            (ROOT / 'scripts/verify_regression.py').read_bytes()).hexdigest()
        cls.evidence = []

    @classmethod
    def tearDownClass(cls):
        configured = os.environ.get('MEMBLOCK_ENQUEUE_ADMISSION_ARTIFACT_DIR')
        if configured:
            artifact = Path(configured).resolve()
            artifact.mkdir(parents=True, exist_ok=True)
            (artifact / 'ledger-evidence.json').write_text(json.dumps({
                'verifier_sha256': cls.production_before, 'evidence': cls.evidence,
            }, indent=2) + '\n')
        actual = hashlib.sha256((ROOT / 'scripts/verify_regression.py').read_bytes()).hexdigest()
        if actual != cls.production_before:
            raise AssertionError('production verifier changed during cadence ledger tests')

    @staticmethod
    def history(events):
        # Derive a ledger from a concrete schedule of (width, older LQ, older
        # SQ). Empty edges split runs. This is independent of parser formulas.
        counts = [0] * 6
        widths = [0] * 6
        previous = False
        run = 0
        for width, older_lq, older_sq in events:
            if width:
                counts[0] += 1
                counts[1] += previous
                counts[2] += previous and (older_lq or older_sq)
                counts[3] += older_lq
                counts[4] += older_sq
                run = run + 1 if previous else 1
                counts[5] = max(counts[5], run)
                widths[width - 1] += 1
            else:
                run = 0
            previous = bool(width)
        return {'lsq_enqueue_cadence': ','.join(map(str, counts)),
                'dispatch_widths': ','.join(map(str, widths))}

    def valid(self, result, mixed=False):
        checker = self.verifier._check_mixed_accounting if mixed else self.verifier._check_lsq_enqueue_cadence
        checker(result)
        self.evidence.append({'kind': 'valid-mixed' if mixed else 'valid-ledger', 'result': result})

    def invalid(self, result, mixed=False):
        checker = self.verifier._check_mixed_accounting if mixed else self.verifier._check_lsq_enqueue_cadence
        with self.assertRaises(self.verifier.VerificationError) as raised:
            checker(result)
        self.evidence.append({'kind': 'rejected-mixed' if mixed else 'rejected-ledger',
                              'result': result, 'diagnostic': str(raised.exception)})

    def test_concrete_empty_sparse_consecutive_and_split_run_histories_are_accepted(self):
        for events in (
            [],
            [(0,False,False)] * 7,
            [(1,False,False), (0,False,False), (6,True,False),
             (0,False,False), (3,False,True)],
            [(1,False,False)] + [(2,True,False)] * 11,
            [(1,False,False)] * 4 + [(0,False,False)] +
            [(3,True,True)] * 3 + [(0,False,False)] + [(6,False,True)] * 3,
        ):
            with self.subTest(events=events):
                self.valid(self.history(events))

    def test_edge_width_conservation_adjacent_and_older_work_corruption_is_rejected(self):
        valid = self.history([(1,False,False), (2,True,False), (3,False,True),
                              (0,False,False), (1,True,True)])
        self.valid(valid)
        for counts in (
            (5,2,2,2,2,3),  # Edge count differs from observed widths.
            (4,4,2,2,2,3),  # Adjacent edges cannot include the first input.
            (4,2,3,2,2,3),  # Live adjacency is a subset of adjacency.
            (4,2,2,5,2,3),  # Older-LQ edges exceed all input edges.
            (4,2,2,2,5,3),  # Older-SQ edges exceed all input edges.
            (4,2,2,0,1,3),  # Live adjacency requires some older work.
            (4,2,0,4,0,3),  # Every adjacent edge is among four older-LQ edges.
            (4,2,0,0,4,3),  # Symmetric bound for older-SQ work.
        ):
            with self.subTest(counts=counts):
                self.invalid(dict(valid, lsq_enqueue_cadence=','.join(map(str, counts))))

    def test_impossible_longest_run_understatement_and_overstatement_are_rejected(self):
        for edges, adjacent, longest in (
            (10,9,2),  # One run of ten cannot have a peak of two.
            (10,8,4),  # Two runs holding ten edges require peak at least five.
            (10,7,3),  # Three runs holding ten edges require peak at least four.
            (8,6,3),   # Two runs holding eight edges require peak at least four.
            (6,0,2),   # A run of two creates an adjacent pair.
            (6,2,4),   # A peak of four requires three adjacent pairs.
            (0,0,1),
            (1,0,0),
        ):
            with self.subTest(edges=edges, adjacent=adjacent, longest=longest):
                self.invalid({'dispatch_widths': f'{edges},0,0,0,0,0',
                              'lsq_enqueue_cadence': f'{edges},{adjacent},0,0,0,{longest}'})
        # Both tight bounds correspond to explicit possible run partitions.
        self.valid({'dispatch_widths': '10,0,0,0,0,0',
                    'lsq_enqueue_cadence': '10,8,0,0,0,5'})  # 5 + 5.
        self.valid({'dispatch_widths': '10,0,0,0,0,0',
                    'lsq_enqueue_cadence': '10,7,0,0,0,4'})  # 4 + 3 + 3.

    def test_older_work_intersection_cannot_understate_live_adjacent_edges(self):
        for counts in ('10,9,0,9,0,10', '10,9,7,0,9,10'):
            with self.subTest(counts=counts):
                self.invalid({'dispatch_widths': '10,0,0,0,0,0',
                              'lsq_enqueue_cadence': counts})
        self.valid({'dispatch_widths': '10,0,0,0,0,0',
                    'lsq_enqueue_cadence': '10,9,8,9,0,10'})

    def test_missing_malformed_and_negative_cadence_fields_are_rejected(self):
        valid = self.history([(1,False,False)])
        for value in (None, 0, [], '', '1,0,0,0,0', '1,0,0,0,0,1,0', '1,0,x,0,0,1'):
            with self.subTest(value=value):
                self.invalid(dict(valid, lsq_enqueue_cadence=value))
        for index in range(6):
            fields = [1,0,0,0,0,1]
            fields[index] = -1
            with self.subTest(negative_index=index):
                self.invalid(dict(valid, lsq_enqueue_cadence=','.join(map(str, fields))))
        self.invalid({'lsq_enqueue_cadence': '1,0,0,0,0,1'})
        self.invalid(dict(valid, dispatch_widths='0,0,0,0,0,0'))

    @staticmethod
    def mixed(schema):
        return {'lq': '2+1/3', 'sq': '3+2/5', 'lsq_monitor_schema': schema,
                'lsq_enqueued_observed': '3,5', 'redirect_cancels_observed': '1,1,2',
                'unobserved_cancels': '0,0'}

    def test_schema_three_requires_cadence_and_schema_two_omission_remains_compatible(self):
        self.valid(self.mixed(2), mixed=True)
        schema_three = self.mixed(3)
        self.invalid(schema_three, mixed=True)
        observed = dict(schema_three, **self.history([(2,False,False)]))
        self.valid(observed, mixed=True)
        without_width = dict(observed)
        del without_width['dispatch_widths']
        self.invalid(without_width, mixed=True)
        self.invalid(dict(observed, lsq_enqueue_cadence='2,0,0,0,0,1'), mixed=True)
        self.invalid(dict(observed, lsq_enqueued_observed='3,4'), mixed=True)
        self.invalid(dict(observed, redirect_cancels_observed='1,0,2'), mixed=True)
        self.invalid(dict(observed, lsq_monitor_schema=4), mixed=True)


if __name__ == '__main__':
    unittest.main()
