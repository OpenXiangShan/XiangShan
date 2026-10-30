#!/usr/bin/env python3
"""Behavioral host tests of production architectural store retirement.

Ports/edges are synthetic; actual production retirement, SQ accounting,
scoreboards, key helpers and reference-byte transitions are compiled unchanged.
Literal public-dequeue and byte schedules are the oracles. Private compiled
faults establish sensitivity without mutating production files. These tests do
not establish DUT behavior, full-CPU legal timing, or regression closure.
"""
import copy
import hashlib
import importlib.util
import json
import os
import shlex
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
METHODS = {
    'completion.cpp': (
        'auto Environment::Impl::retire_store(',
        'auto Environment::Impl::retire_vector_stores(',
        'auto Environment::Impl::record_committed_store(',
        'auto Environment::Impl::record_committed_vector_store(',
        'auto Environment::Impl::commit_stores_through(',
    ),
    'cycle.cpp': (
        'auto Environment::Impl::scalar_store_key(',
        'auto Environment::Impl::vector_store_key(',
    ),
}
DEPENDENCIES = (
    'cpp/memblock_env.hpp', 'cpp/environment/model.inc',
    'cpp/environment/scoreboards.inc', 'cpp/environment/queue_accounting.hpp',
    'cpp/environment/queue_accounting.cpp', 'cpp/generated_interface_types.hpp',
    'cpp/failure.hpp',
)
CASES = (
    'scalar_retained', 'scalar_already_dequeued', 'scalar_widths',
    'scalar_invalid', 'vector_retained', 'vector_already_dequeued',
    'vector_invalid', 'mixed_prefix', 'cancellation_retains',
    'cumulative_wrap_reuse', 'ledger_rejection', 'invalid_target_order',
)


def method(path, signature):
    source = (ROOT / 'cpp/environment' / path).read_text()
    start = source.index(signature)
    return source[start:source.index('\n}\n', start) + 3]


def production_hashes():
    result = {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
              for path in DEPENDENCIES}
    for path, signatures in METHODS.items():
        for signature in signatures:
            result[path + ':' + signature] = hashlib.sha256(
                method(path, signature).encode()).hexdigest()
    return result


def source_text():
    source = (ROOT / 'unit/store_retirement_fixture.cpp').read_text()
    header = (ROOT / 'cpp/environment/queue_accounting.hpp').read_text()
    queue_class = header[header.index('class QueueAccounting {'):
                         header.index('\n} // namespace memblock')]
    queue_source = (ROOT / 'cpp/environment/queue_accounting.cpp').read_text()
    queue_methods = queue_source[queue_source.index('void QueueAccounting::'):
                                 queue_source.rindex('} // namespace memblock')]
    source = source.replace('// PRODUCTION_QUEUE_ACCOUNTING',
                            (queue_class + '\n' + queue_methods).replace(
                                'DutAdapter', 'FixtureDut'))
    methods = [method(path, signature).replace(
        'Environment::Impl::', 'StoreRetirementFixture::')
        for path, signatures in METHODS.items() for signature in signatures]
    return source.replace('// PRODUCTION_METHODS', '\n'.join(methods))


def replace_once(source, before, after):
    if source.count(before) != 1:
        raise AssertionError('private fault anchor is no longer unique: ' + before)
    return source.replace(before, after, 1)


class StoreRetirementTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.before = production_hashes()
        artifact_parent = os.environ.get('MEMBLOCK_STORE_RETIREMENT_TEST_OUT')
        if artifact_parent:
            Path(artifact_parent).mkdir(parents=True, exist_ok=True)
            cls.workspace = Path(tempfile.mkdtemp(prefix='host-', dir=artifact_parent))
        else:
            temporary = tempfile.TemporaryDirectory(prefix='memblock-store-retirement-')
            cls.addClassCleanup(temporary.cleanup)
            cls.workspace = Path(temporary.name)
        cls.source = source_text()
        cls.commands = []
        cls.results = []
        cls.binary = cls.compile_source('production', cls.source)
        cls.assert_production_unchanged()

    @classmethod
    def assert_production_unchanged(cls):
        if production_hashes() != cls.before:
            raise AssertionError('production retirement inputs changed during host tests')

    @classmethod
    def compile_source(cls, name, source):
        path = cls.workspace / (name + '.cpp')
        binary = cls.workspace / name
        path.write_text(source)
        command = [os.environ.get('CXX', 'c++'), '-std=c++20', '-O0',
                   '-I', str(ROOT / 'cpp'), str(path), '-o', str(binary)]
        cls.commands.append(command)
        result = subprocess.run(command, text=True, capture_output=True, timeout=90)
        (cls.workspace / (name + '.compile.log')).write_text(result.stdout + result.stderr)
        if result.returncode:
            raise RuntimeError(result.stdout + result.stderr)
        return binary

    @classmethod
    def tearDownClass(cls):
        cls.assert_production_unchanged()
        (cls.workspace / 'summary.json').write_text(json.dumps({
            'production_sha256': cls.before,
            'compile_commands': cls.commands,
            'results': cls.results,
        }, indent=2) + '\n')

    def run_case(self, binary, case):
        command = [str(binary), case]
        result = subprocess.run(command, text=True, capture_output=True, timeout=20)
        self.results.append({'command': command, 'returncode': result.returncode,
                             'stdout': result.stdout, 'stderr': result.stderr})
        return result

    def test_production_behavior(self):
        for case in CASES:
            with self.subTest(case=case):
                result = self.run_case(self.binary, case)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_private_compiled_faults_are_detected(self):
        # Each fault changes a production decision or side effect in a private
        # translation unit. Require its designated assertion, not a compile or
        # fixture setup failure, to reject it.
        faults = (
            ('missing_scalar_pulse',
             'dut_.queues.backend.scommit.set(count);',
             'dut_.queues.backend.scommit.set(0);',
             'scalar_retained', 'scalar retirement edge lacked'),
            ('missing_scalar_reference',
             '    record_committed_store(transaction);',
             '    /* private fault: omit reference transition */',
             'scalar_widths', 'independent byte schedule mismatch'),
            ('duplicate_scalar_reference',
             '    record_committed_store(transaction);',
             '    record_committed_store(transaction); record_committed_store(transaction);',
             'scalar_retained', 'scalar reference transition was not exactly once'),
            ('missing_retained_ledger',
             '        committed_stores_.push_back({key, target, vector});',
             '        /* private fault: drop drain obligation */',
             'scalar_retained', 'scalar retirement failed'),
            ('premature_target_drain',
             'committed_stores_.front().target <= sq_dequeued_',
             'committed_stores_.front().target <= sq_dequeued_ + 1',
             'vector_retained', 'partial vector dequeue released wrong target prefix'),
            ('cancellation_as_drain',
             'committed_stores_.front().target <= sq_dequeued_',
             'committed_stores_.front().target <= sq_dequeued_ + sq_canceled_',
             'cancellation_retains', 'cancellation released committed store obligation'),
            ('vector_scalar_commit',
             '    // Preserve the old head for its retirement edge.',
             '    dut_.queues.backend.scommit.set(1);\n    // Preserve the old head for its retirement edge.',
             'vector_retained', 'vector retirement emitted scalar commit'),
            ('missing_vector_membership',
             '        !queues_.complete_vector_store_group(keys) ||',
             '        false ||',
             'vector_invalid', 'invalid or incomplete vector retirement accepted'),
            ('masked_byte_leak',
             '    const std::uint16_t active = active_vector_elements(transaction);',
             '    const std::uint16_t active = 0xffff;',
             'vector_retained', 'independent byte schedule mismatch'),
            ('commit_only_before_dequeue',
             '    if (!commit_stores_through(transaction, 1) ||',
             '    if ((queues_.sq_dequeued() < *target && !commit_stores_through(transaction, 1)) ||',
             'scalar_already_dequeued', 'scalar retirement did not take exactly one edge'),
            ('missing_scalar_target_preflight',
             '        !target || !queues_.valid_store_retirement_target(*target)) {',
             '        !target) {',
             'invalid_target_order', 'stale scalar target rejection emitted a commit'),
            ('missing_vector_target_preflight',
             '            !queues_.valid_store_retirement_target(*target)) {',
             '            false) {',
             'invalid_target_order', 'stale vector target rejection ticked or changed state'),
            ('dropped_preedge_pending_count',
             '    store_retirement_stats_.pending_before_last_edge = committed_stores_.size();',
             '    store_retirement_stats_.pending_before_last_edge = 0;',
             'scalar_retained', 'unmonitored edge lost retained scalar'),
        )
        for name, before, after, case, diagnostic in faults:
            with self.subTest(fault=name):
                source = replace_once(self.source, before, after)
                binary = self.compile_source(name, source)
                result = self.run_case(binary, case)
                self.assertNotEqual(result.returncode, 0, 'private fault escaped: ' + name)
                self.assertIn(diagnostic, result.stderr)
                self.assert_production_unchanged()


RETIREMENT_FIELDS = dict(stream=12, stream_ops=5, stream_issue=6,
                         stream_vector_uops=2, stream_store_retirement=10,
                         stream_recovery=20, stream_sta=11, stream_history=4,
                         stream_waiting_head=2, stream_dependencies=9)
RETIREMENT_PEAKS = {('stream', 6), ('stream', 7), ('stream_sta', 10),
                    ('stream_history', 1), ('stream_store_retirement', 5)}


def edge(action, age=None, slot=None, dequeue=0):
    return action, age, slot, dequeue


def retirement_rows(instructions, edges):
    """Count accepted edges and independently retained absolute SQ records.

    Instructions are (kind, tuple of per-uop SQ widths). Each retirement and
    subsequent head-output edge is explicit. Dequeue happens after the input
    edge snapshot and before post-edge retirement registration, so a final
    draining producer still counts as overlap. These are accounting histories,
    not a model of DUT latency or full-CPU instruction reachability.
    """
    rows = {name: [0] * width for name, width in RETIREMENT_FIELDS.items()}
    live, admitted, retired, completed = [], set(), set(), set()
    issued, targets, pending = {}, {}, []
    allocated = dequeued = retries = 0
    needs_head = False
    store = rows['stream_store_retirement']
    kinds = ('load', 'vector-load', 'scalar', 'vector', 'prefetch')
    for action, age, slot, dequeue in edges:
        before = bool(pending)
        # Real public dequeue advances SQ entries, not the number of uops.
        dequeued += dequeue
        assert 0 <= dequeued <= allocated
        removed = [target for target in pending if target <= dequeued]
        pending = [target for target in pending if target > dequeued]
        store[9] += len(removed)
        if action == 'admit':
            assert age == len(admitted) and len(live) < 8 and not needs_head
            rows['stream'][3] += bool(live)
            rows['stream'][4] += bool(live) and bool(retired)
            rows['stream_history'][2] += age >= 32 and bool(live)
            admitted.add(age); live.append(age); issued[age] = set()
            targets[age] = []
            for width in instructions[age][1]:
                assert width > 0
                allocated += width
                targets[age].append(allocated)
            store[6] += before
        elif action == 'producer':
            assert age in live and age not in completed and slot not in issued[age]
            kind, widths = instructions[age]
            if kind == 'scalar':
                assert slot in ('sta', 'std')
                rows['stream_issue'][1 if slot == 'sta' else 2] += 1
                rows['stream_sta'][0] += slot == 'sta'
            else:
                count = len(widths) if kind in ('vector', 'vector-load') else 1
                assert type(slot) is int and 0 <= slot < count
                rows['stream_issue'][{'load': 0, 'vector-load': 3,
                                      'vector': 4, 'prefetch': 5}[kind]] += 1
            issued[age].add(slot); store[7] += before
        elif action == 'retry':
            assert instructions[age][0] == 'scalar' and age in live and 'sta' in issued[age]
            retries += 1; rows['stream_sta'][1] += 1; rows['stream_sta'][3] += 1
            store[7] += before
        elif action == 'invalidate':
            assert instructions[age][0] == 'load' and age in completed and age in live
            completed.remove(age)
        elif action == 'refetch':
            assert instructions[age][0] == 'load' and age in live and age not in completed
            rows['stream_recovery'][8] += 1; store[7] += before
        elif action == 'complete':
            assert age in live and age not in completed
            kind, widths = instructions[age]
            expected = {'sta', 'std'} if kind == 'scalar' else set(range(
                len(widths) if kind in ('vector', 'vector-load') else 1))
            assert issued[age] == expected
            completed.add(age)
            if kind == 'scalar': rows['stream_sta'][2] += 1
        elif action == 'retire':
            assert not needs_head and live and live[0] == age and age in completed
            kind, widths = instructions[age]
            rows['stream'][5] += len(live) > 1
            rows['stream_ops'][kinds.index(kind)] += 1
            if kind == 'scalar': store[0] += 1
            elif kind == 'vector': store[1] += 1; store[2] += len(widths)
            if kind in ('scalar', 'vector'):
                for target in targets[age]:
                    if target <= dequeued:
                        store[4] += 1; store[9] += 1
                    else:
                        store[3] += 1; pending.append(target)
                        store[5] = max(store[5], len(pending))
                        assert len(pending) <= 52
            live.pop(0); retired.add(age); needs_head = True
        elif action == 'head':
            assert needs_head
            store[8] += before; needs_head = False
        else:
            assert action == 'idle'
        rows['stream'][6] = max(rows['stream'][6], len(live))
        rows['stream'][7] = max(rows['stream'][7], sum(item not in completed for item in live))
    assert len(admitted) == len(retired) == len(instructions) and not live and not pending and not needs_head
    for kind, widths in instructions:
        if kind in ('vector-load', 'vector'):
            rows['stream_vector_uops'][kind == 'vector'] += len(widths)
    count = len(instructions)
    rows['stream'][0:3] = [bool(count), count, count]
    rows['stream'][8] = sum(rows['stream_issue'])
    rows['stream'][11] = count
    rows['stream_history'][0:2] = [count > 32, count]
    rows['stream_sta'][10] = bool(rows['stream_sta'][0])
    rows['stream_scalar_retries'] = retries
    return rows


def mixed_retirement_history(*, partial_predequeue=False, extra_producers=False):
    instructions = [('scalar', (1,)), ('vector', (2, 3, 1)), ('load', ()),
                    ('scalar', (1,)), ('vector', (1, 1)), ('load', ())]
    events = [edge('admit', 0), edge('producer', 0, 'sta'), edge('producer', 0, 'std'),
              edge('complete', 0), edge('retire', 0), edge('head'), edge('admit', 1),
              edge('producer', 1, 0), edge('producer', 1, 1), edge('producer', 1, 2),
              edge('admit', 2), edge('producer', 2, 0), edge('complete', 1), edge('complete', 2),
              edge('idle', dequeue=3 if partial_predequeue else 2),
              edge('retire', 1), edge('head'), edge('admit', 3),
              edge('producer', 3, 'sta'), edge('producer', 3, 'std')]
    if extra_producers:
        events += [edge('retry', 3), edge('invalidate', 2), edge('refetch', 2), edge('complete', 2)]
    events += [edge('complete', 3), edge('retire', 2), edge('head'), edge('retire', 3), edge('head'),
               edge('admit', 4), edge('producer', 4, 0), edge('producer', 4, 1), edge('complete', 4),
               edge('idle', dequeue=4), edge('retire', 4), edge('head'), edge('admit', 5),
               # The final accepted producer is counted from pending before
               # the edge even though that edge drains every remaining record.
               edge('producer', 5, 0, dequeue=3 if partial_predequeue else 4),
               edge('complete', 5), edge('retire', 5), edge('head')]
    return retirement_rows(instructions, events)


def sequential_retirement_history(instructions, *, predequeue=False, release_before_last=0):
    events = []
    allocation = 0
    for age, (kind, widths) in enumerate(instructions):
        events.append(edge('admit', age))
        slots = ('sta', 'std') if kind == 'scalar' else range(
            len(widths) if kind in ('vector', 'vector-load') else 1)
        events.extend(edge('producer', age, slot) for slot in slots)
        events.append(edge('complete', age))
        allocation += sum(widths)
        if predequeue and widths: events.append(edge('idle', dequeue=sum(widths)))
        if age == len(instructions) - 1 and release_before_last:
            events.append(edge('idle', dequeue=release_before_last))
        events.extend((edge('retire', age), edge('head')))
    if not predequeue: events.append(edge('idle', dequeue=allocation-release_before_last))
    return retirement_rows(instructions, events)


def combine_retirement_rows(histories):
    result = {}
    for field, width in RETIREMENT_FIELDS.items():
        result[field] = [max(row[field][i] for row in histories)
                         if (field, i) in RETIREMENT_PEAKS else
                         sum(row[field][i] for row in histories) for i in range(width)]
    result['stream_scalar_retries'] = sum(row['stream_scalar_retries'] for row in histories)
    return result


REPORTER_CPP = r"""
#include "scenarios/shared.hpp"
int main() {
    auto constraints = memblock::scenarios::RandomConstraints::preset("coverage");
    constraints.apply("concurrent=1000"); constraints.apply("concurrent-stream=1000");
    constraints.apply("stream-min-instructions=1"); constraints.apply("stream-max-instructions=4096");
    constraints.validate();
    memblock::scenarios::MixedCoverage coverage;
    auto read = [&](auto &values) { for (auto &value : values) std::cin >> value; };
    read(coverage.stream); read(coverage.stream_ops); read(coverage.stream_issue);
    read(coverage.stream_vector_uops); read(coverage.stream_store_retirement);
    read(coverage.stream_recovery); read(coverage.stream_sta); read(coverage.stream_history);
    read(coverage.stream_waiting_head); read(coverage.stream_dependencies);
    coverage.stream_cancel[24] = coverage.stream_history[1];
    coverage.stream_cancel[25] = coverage.stream_history[0];
    std::uint64_t transactions;
    std::cin >> coverage.stream_scalar_retries >> transactions;
    std::string extra;
    if (std::cin.fail() || std::cin >> extra) return 1;
    std::cout << "MEMBLOCK_CONSTRAINED_RANDOM_PASS transactions=" << transactions
              << ' ' << constraints.summary() << ' ' << coverage.summary() << '\n';
}
"""


class StreamStoreRetirementAccountingTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        parent = os.environ.get('MEMBLOCK_STORE_RETIREMENT_TEST_OUT')
        if parent:
            Path(parent).mkdir(parents=True, exist_ok=True)
            cls.workspace = Path(tempfile.mkdtemp(prefix='accounting-', dir=parent))
        else:
            temporary = tempfile.TemporaryDirectory(prefix='memblock-store-retirement-accounting-')
            cls.addClassCleanup(temporary.cleanup); cls.workspace = Path(temporary.name)
        cls.records = []
        scripts = ROOT / 'scripts'
        sys.path.insert(0, str(scripts))
        cls.modules = {}
        for name in ('run_regression', 'verify_regression'):
            spec = importlib.util.spec_from_file_location('store_retirement_' + name, scripts / (name + '.py'))
            module = importlib.util.module_from_spec(spec); sys.modules[spec.name] = module
            spec.loader.exec_module(module); cls.modules[name] = module
        cls.verifier = cls.modules['verify_regression']
        source = cls.workspace / 'report.cpp'; source.write_text(REPORTER_CPP)
        cls.binary = cls.workspace / 'report'
        dependencies = cls.workspace / 'report.d'
        cls.command = [os.environ.get('CXX', 'c++'), '-std=c++20', '-O0', '-MMD', '-MF', str(dependencies),
                       '-I', str(ROOT / 'cpp'), str(source), '-o', str(cls.binary)]
        built = subprocess.run(cls.command, text=True, capture_output=True, timeout=90)
        (cls.workspace / 'compile.log').write_text(built.stdout + built.stderr)
        if built.returncode: raise RuntimeError(built.stdout + built.stderr)
        paths = [Path(name).resolve() for name in shlex.split(
            dependencies.read_text().replace('\\\n', ' ').split(':', 1)[1]) if Path(name).resolve() != source]
        paths += [scripts / (name + '.py') for name in ('run_regression', 'verify_regression')]
        cls.hashes = {str(path): hashlib.sha256(path.read_bytes()).hexdigest() for path in paths}

    @classmethod
    def tearDownClass(cls):
        after = {path: hashlib.sha256(Path(path).read_bytes()).hexdigest() for path in cls.hashes}
        (cls.workspace / 'summary.json').write_text(json.dumps({
            'compile_command': cls.command, 'production_sha256': cls.hashes,
            'hashes_after': after, 'accounting_cases': cls.records,
        }, indent=2) + '\n')
        if cls.hashes != after: raise AssertionError('production accounting inputs changed during tests')

    def report(self, rows):
        values = [str(int(value)) for field in RETIREMENT_FIELDS for value in rows[field]]
        values += [str(rows['stream_scalar_retries']), str(max(1, rows['stream'][1]))]
        ran = subprocess.run([str(self.binary)], input=' '.join(values) + '\n',
                             text=True, capture_output=True, timeout=20)
        self.assertEqual(ran.returncode, 0, ran.stderr)
        report = self.modules['run_regression'].parse_summary(ran.stdout)
        self.assertEqual(report['constraint_schema'], 76)
        for field in RETIREMENT_FIELDS:
            self.assertEqual(report[field], ','.join(map(lambda n: str(int(n)), rows[field])), field)
        self.records.append({'kind': 'serialized-history', 'store_retirement': report['stream_store_retirement'],
                             'summary': ran.stdout})
        return report

    def accepted(self, report, *, complete=True, label='valid'):
        self.verifier._check_stream_store_retirement(report)
        if complete: self.verifier._check_stream_accounting(report)
        self.records.append({'kind': label, 'accepted': True,
                             'store_retirement': report.get('stream_store_retirement')})

    def rejected(self, report, *, label='corruption'):
        with self.assertRaises(self.verifier.VerificationError):
            self.verifier._check_stream_store_retirement(report)
        self.records.append({'kind': label, 'accepted': False,
                             'store_retirement': report.get('stream_store_retirement')})

    @staticmethod
    def alter(report, field, index, value):
        result = copy.deepcopy(report)
        counts = result[field].split(','); counts[index] = str(value)
        result[field] = ','.join(counts)
        return result

    def test_independent_edge_ledgers_and_serializer_parser(self):
        mixed = mixed_retirement_history()
        partial = mixed_retirement_history(partial_predequeue=True)
        early = sequential_retirement_history([('scalar', (1,)), ('vector', (2, 1)), ('load', ())],
                                              predequeue=True)
        plain = sequential_retirement_history([('load', ()), ('load', ())])
        self.assertEqual(mixed['stream_store_retirement'], [2,2,5,7,0,4,5,9,5,7])
        self.assertEqual(partial['stream_store_retirement'], [2,2,5,6,1,3,5,9,5,7])
        self.assertEqual(early['stream_store_retirement'], [1,1,2,0,3,0,0,0,0,3])
        for name, rows in (('mixed', mixed), ('partial', partial), ('predequeued', early),
                           ('no-stores', plain), ('empty', retirement_rows([], []))):
            with self.subTest(history=name): self.accepted(self.report(rows), label=name)

    def test_nonzero_vstart_flush_accounting(self):
        rows = sequential_retirement_history([('vector-load', (1,)), ('vector', (1,))])
        report = self.report(rows)
        report['stream_vstart_flush'] = '1,1,2,1'
        self.accepted(report)
        for value in ('1,1,1,0', '1,1,2,3', '2,0,2,0', '0,2,2,0',
                      '-1,0,0,0', '0,0,0', 'x,0,0,0'):
            corrupt = copy.deepcopy(report); corrupt['stream_vstart_flush'] = value
            self.rejected(corrupt, label='invalid-vstart-flush-' + value)

    def test_multiple_epochs_keep_peaks_and_conserve_vector_uop_records(self):
        first = mixed_retirement_history()
        second = mixed_retirement_history(partial_predequeue=True)
        batch = sequential_retirement_history([('vector', (1,)*8), ('vector', (1,)*8), ('scalar', (1,))])
        combined = combine_retirement_rows([first, second, batch])
        self.assertEqual(batch['stream_store_retirement'], [1,2,16,17,0,17,2,10,3,17])
        self.assertEqual(combined['stream_store_retirement'], [5,6,26,30,1,17,12,28,13,31])
        self.accepted(self.report(combined), label='multi-epoch-maximum-peak')
        # A sum of plausible per-epoch peaks can still fit aggregate bounds.
        # The explicit ledger supplies the true maximum; the verifier alone
        # cannot reconstruct temporal peak truth from one aggregate row.

    def test_counter_conservation_and_pending_head_bounds(self):
        base = self.report(mixed_retirement_history())
        for index, value in ((0,1),(1,1),(2,4),(3,6),(4,1),(9,6),(5,0),(5,8),(8,0),(8,7)):
            with self.subTest(index=index,value=value):
                self.rejected(self.alter(base,'stream_store_retirement',index,value), label=f'counter-{index}')
        batch = self.report(sequential_retirement_history(
            [('vector',(1,)*8),('vector',(1,)*8),('scalar',(1,))]))
        self.rejected(self.alter(batch,'stream_store_retirement',8,2), label='head-undercount-multi-uop')
        capacity = self.report(sequential_retirement_history([('vector',(1,)*8)]*7, release_before_last=16))
        self.assertEqual(capacity['stream_store_retirement'], '0,7,56,56,0,48,6,48,7,56')
        self.accepted(capacity, label='absolute-queued-count-above-capacity')
        self.rejected(self.alter(capacity,'stream_store_retirement',5,53), label='physical-pending-capacity')
        self.rejected(self.alter(capacity,'stream_store_retirement',8,6), label='head-undercount-many-uops')

    def test_accepted_work_overlap_limits_include_retries_and_refetch(self):
        report = self.report(mixed_retirement_history(extra_producers=True))
        self.assertEqual(report['stream_store_retirement'], '2,2,5,7,0,4,5,11,5,7')
        self.accepted(report, label='retry-refetch-edges')
        # The aggregate ceiling allows eleven initial edges plus one retry
        # and one refetch; the explicit schedule above overlaps eleven.
        exact_limit = self.alter(report,'stream_store_retirement',7,13)
        self.accepted(exact_limit, complete=False, label='producer-bound-includes-retry-refetch')
        self.rejected(self.alter(report,'stream_store_retirement',7,14), label='producer-overcount')
        self.rejected(self.alter(report,'stream_store_retirement',6,6), label='admission-overcount')
        early = self.report(sequential_retirement_history([('scalar',(1,)),('vector',(1,))], predequeue=True))
        for index in (5,6,7,8):
            with self.subTest(empty_overlap=index):
                self.rejected(self.alter(early,'stream_store_retirement',index,1), label='overlap-without-pending')

    def test_malformed_retry_metadata_reports_verification_error(self):
        base = self.report(mixed_retirement_history())
        for value in (None, True, False, 'malformed', '1', -1):
            with self.subTest(retries=value), self.assertRaises(self.verifier.VerificationError):
                self.verifier._check_stream_accounting({**base, 'stream_scalar_retries':value})
            self.records.append({'kind':'malformed-retry-metadata','accepted':False,'value':value})
        missing = copy.deepcopy(base); del missing['stream_scalar_retries']
        with self.assertRaises(self.verifier.VerificationError):
            self.verifier._check_stream_accounting(missing)
        self.records.append({'kind':'missing-retry-metadata','accepted':False})

    def test_required_widths_negative_counts_and_schema73_omission(self):
        base = self.report(mixed_retirement_history())
        missing = copy.deepcopy(base); del missing['stream_store_retirement']
        self.rejected(missing, label='missing-schema74-field')
        with self.assertRaises(self.verifier.VerificationError):
            self.verifier._check_stream_accounting(missing)
        old = {**missing, 'constraint_schema': 73}
        self.verifier._check_stream_accounting(old)
        self.records.append({'kind':'schema73-omission','accepted':True})
        for value in (None, True, [], '', '1,2', '0,'*10, ','.join(['1.0']*10),
                      ','.join(['False']*10), ','.join(['0']*9), ','.join(['0']*11)):
            with self.subTest(malformed=value):
                self.rejected({**base,'stream_store_retirement':value}, label='malformed')
        for index in range(10):
            with self.subTest(negative=index):
                self.rejected(self.alter(base,'stream_store_retirement',index,-1), label=f'negative-{index}')


if __name__ == '__main__':
    unittest.main()
