#!/usr/bin/env python3
"""Check stream profile legality and schema-70 evidence against literal contracts.

The host helper includes production shared.hpp and calls the actual parser and
validator. Expected acceptance comes from executor endpoints, operation masks,
and fixed vector instructions, rather than a second copy of shape enumeration.
Set MEMBLOCK_STREAM_PROFILE_SOURCE_ROOT to an alternate tests/memblock tree;
MEMBLOCK_STREAM_PROFILE_REPORT retains commands, cases, and input hashes.
"""
from __future__ import annotations

import hashlib
import importlib.util
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
SOURCE_ROOT = Path(os.environ.get('MEMBLOCK_STREAM_PROFILE_SOURCE_ROOT', ROOT)).resolve()
OPERATIONS = (
    'scalar-load', 'scalar-store', 'vector-load', 'vector-store', 'vector-segment',
    'prefetch', 'atomic', 'nc', 'mmio', 'hypervisor', 'cmo', 'ptw-error',
    'load-merge', 'set-pressure', 'miss-burst',
)
STREAM_CLASSES = ('scalar-load', 'scalar-store', 'vector-load', 'vector-store', 'prefetch')
# Remove unrelated preset requirements so a rejected operation mask identifies
# concurrency policy. Operation-specific policies have their own unit tests.
NEUTRAL_POLICIES = tuple(f'{key}=0' for key in (
    'special-concurrent', 'vector-fof', 'vector-segment-fof',
    'vector-segment-high-va', 'probe', 'bank-conflict', 'stride-stream',
    'canonical-boundary', 'gstage-boundary',
    'uncache-error', 'atomic-error', 'cmo-error', 'dcache-load-error',
    'translation-stage1-napot', 'translation-vs-napot', 'translation-g-napot',
)) + ('latency=compact',)

HELPER_SOURCE = r'''
#include "scenarios/shared.hpp"
int main(int argc, char **argv)
{
    try {
        auto constraints = memblock::scenarios::RandomConstraints::preset(argv[1]);
        for (int index = 2; index < argc; ++index) {
            constraints.apply(argv[index]);
        }
        constraints.validate();
        std::cout << "accepted " << constraints.uses_legacy_concurrency() << ' '
                  << constraints.uses_stream_concurrency() << '\n';
        return 0;
    } catch (const std::exception &error) {
        std::cout << "rejected " << error.what() << '\n';
        return 2;
    }
}
'''


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def population(mask, extra=None):
    enabled = {name for bit, name in enumerate(STREAM_CLASSES) if mask & (1 << bit)}
    if extra:
        enabled.add(extra)
    return tuple(f'{name}={int(name in enabled)}' for name in OPERATIONS)


def strided_byte_shape(lmul):
    """VL=VLMAX byte striding: m2/m4/m8 move 32/64/128 queue flows.

    VLEN is 128 bits. These are complete instructions, with no mask, partial VL,
    or vstart shortening. No queue formula or reachability search is copied.
    """
    assignments = ['vector-masked=0', 'vector-partial-vl=0', 'vector-nonzero-vstart=0']
    for addressing in ('unit-stride', 'strided', 'indexed-unordered', 'indexed-ordered'):
        assignments.append(f'vector-{addressing}={int(addressing == "strided")}')
    for dimension in ('eew', 'sew'):
        assignments.extend(f'vector-{dimension}{width}={int(width == 8)}'
                           for width in (8, 16, 32, 64))
    for dimension in ('lmul', 'emul'):
        assignments.extend(f'vector-{dimension}-{size}={int(size == lmul)}'
                           for size in ('mf8', 'mf4', 'mf2', 'm1', 'm2', 'm4', 'm8'))
    return tuple(assignments)


def singleton(kind, schema=70, mask=31, stream_rate=1000):
    # Stream counters use instruction classes, whereas issue counters distinguish
    # scalar STA and STD. These literal one-instruction records are independent
    # of any producer, scheduler, or result fixture.
    observations = {
        'scalar-load': ('1,0,0,0,0', '1,0,0,0,0,0', '0,0', 1),
        'scalar-store': ('0,0,1,0,0', '0,1,1,0,0,0', '0,0', 2),
        'vector-load': ('0,1,0,0,0', '0,0,0,1,0,0', '1,0', 1),
        'vector-store': ('0,0,0,1,0', '0,0,0,0,1,0', '0,1', 1),
        'prefetch': ('0,0,0,0,1', '0,0,0,0,0,1', '0,0', 1),
    }
    operations, issues, vector_uops, events = observations[kind]
    enabled = {name for bit, name in enumerate(STREAM_CLASSES) if mask & (1 << bit)}
    return dict(
        constraint_schema=schema, transactions=1,
        target_concurrent=1000, target_concurrent_stream=stream_rate,
        target_ops=','.join(str(int(name in enabled)) for name in OPERATIONS),
        concurrent='0,0,0,0,0', concurrent_ops='0,0,0,0,0',
        stream=f'1,1,1,0,0,0,1,0,{events},0,0,1',
        stream_ops=operations, stream_issue=issues, stream_vector_uops=vector_uops,
        stream_scalar_retries=0, stream_waiting_head='0,0',
        stream_recovery=','.join(['0'] * 20),
    )


def empty_stream(**updates):
    record = singleton('scalar-load')
    record.update(stream=','.join(['0'] * 12), stream_ops='0,0,0,0,0',
                  stream_issue='0,0,0,0,0,0', stream_vector_uops='0,0')
    record.update(updates)
    return record


class StreamProfileConstraintsTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.records = []
        cls.tracked = {
            str(SOURCE_ROOT / path): digest(SOURCE_ROOT / path)
            for path in ('cpp/random/constraints.inc', 'cpp/scenarios/shared.hpp',
                         'cpp/memblock_env.hpp', 'cpp/generated_interface_types.hpp',
                         'scripts/verify_regression.py')
        }
        cls.pins_path = os.environ.get('MEMBLOCK_STREAM_PROFILE_PINS')
        cls.pins = json.loads(Path(cls.pins_path).read_text()) if cls.pins_path else None
        cls.check_pins()
        artifact_dir = os.environ.get('MEMBLOCK_STREAM_PROFILE_ARTIFACTS')
        if artifact_dir:
            Path(artifact_dir).mkdir(parents=True, exist_ok=True)
            cls.work = Path(tempfile.mkdtemp(prefix='host-', dir=artifact_dir))
        else:
            cls.temporary = tempfile.TemporaryDirectory(prefix='memblock-stream-profile-')
            cls.addClassCleanup(cls.temporary.cleanup)
            cls.work = Path(cls.temporary.name)
        cls.addClassCleanup(cls.finalize)
        path = cls.work / 'constraints.cpp'
        path.write_text(HELPER_SOURCE)
        cls.binary = cls.work / 'constraints'
        dependencies = cls.work / 'constraints.d'
        cls.compile_command = [
            'c++', '-std=c++20', '-O0', '-MMD', '-MF', str(dependencies),
            '-I', str(SOURCE_ROOT / 'cpp'), '-I', str(ROOT / 'cpp'),
            '-I', str(ROOT / 'cpp/scenarios'),
            str(path), '-o', str(cls.binary),
        ]
        result = subprocess.run(cls.compile_command, capture_output=True, text=True, timeout=60)
        (cls.work / 'compile.log').write_text(result.stdout + result.stderr)
        if result.returncode:
            raise RuntimeError(result.stdout + result.stderr)
        dependency_text = dependencies.read_text().replace('\\\n', ' ')
        for name in shlex.split(dependency_text.split(':', 1)[1]):
            dependency = Path(name).resolve()
            if dependency != path:
                cls.tracked[str(dependency)] = digest(dependency)
        # Import the requested implementation under its own name so test
        # discovery cannot silently reuse another suite's verifier module.
        sys.path.insert(0, str(ROOT / 'scripts'))
        spec = importlib.util.spec_from_file_location(
            'stream_profile_verifier', SOURCE_ROOT / 'scripts/verify_regression.py')
        cls.verifier = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(cls.verifier)
        cls.check_pins()

    @classmethod
    def check_pins(cls):
        if cls.pins is None:
            return
        snapshot_root = SOURCE_ROOT.parents[1]
        xiang_root = ROOT.parents[1]
        for relative, expected in cls.pins['hashes'].items():
            for label, base in (('snapshot', snapshot_root), ('live', xiang_root)):
                actual = digest(base / relative)
                if actual != expected:
                    raise AssertionError(f'{label} changed pinned input {relative}: {actual} != {expected}')

    @classmethod
    def finalize(cls):
        current = {path: digest(path) for path in cls.tracked}
        report = dict(
            source_root=str(SOURCE_ROOT), pins_path=cls.pins_path,
            pins=cls.pins, compile_command=cls.compile_command,
            helper_source=str(cls.work / 'constraints.cpp'),
            hashes_before=cls.tracked, hashes_after=current, cases=cls.records,
        )
        destination = os.environ.get('MEMBLOCK_STREAM_PROFILE_REPORT')
        if destination:
            Path(destination).write_text(json.dumps(report, indent=2) + '\n')
        if current != cls.tracked:
            raise AssertionError('production inputs changed during stream profile tests')
        cls.check_pins()

    def constraints(self, accepted, *assignments, preset='coverage', executors=None, reason=None):
        command = [str(self.binary), preset, *assignments]
        result = subprocess.run(command, capture_output=True, text=True, timeout=10)
        self.records.append(dict(test=self.id(), command=command, accepted=accepted,
                                 returncode=result.returncode, output=result.stdout + result.stderr))
        self.assertEqual(result.returncode, 0 if accepted else 2, result.stdout + result.stderr)
        if executors is not None:
            self.assertEqual(result.stdout.strip(), f'accepted {executors[0]} {executors[1]}')
        if reason is not None:
            self.assertIn(reason, result.stdout)

    def profile(self, accepted, mask, concurrent=1000, stream=1000, extra=None,
                shape=(), executors=None, reason=None):
        self.constraints(accepted, *NEUTRAL_POLICIES, *population(mask, extra),
                         f'concurrent={concurrent}', f'concurrent-stream={stream}',
                         *shape, executors=executors, reason=reason)

    def accounting(self, record, accepted, reason=None):
        observed = None
        try:
            self.verifier._check_stream_accounting(record)
        except self.verifier.VerificationError as error:
            observed = str(error)
        self.records.append(dict(test=self.id(), record=record, accepted=accepted, error=observed))
        self.assertEqual(observed is None, accepted, observed or 'corrupt accounting was accepted')
        if reason:
            self.assertIn(reason, observed or '')

    def test_all_31_stream_operation_masks_and_executor_endpoints(self):
        # Literal endpoint table: stream=0 selects legacy, intermediate rates
        # permit both, and stream=1000 selects stream. concurrent=0 selects neither.
        for concurrent, stream, executors in (
                (0, 0, (0, 0)), (0, 375, (0, 0)), (0, 1000, (0, 0)),
                (1, 0, (1, 0)), (1, 375, (1, 1)), (1, 1000, (0, 1)),
                (1000, 0, (1, 0)), (1000, 375, (1, 1)), (1000, 1000, (0, 1))):
            for mask in range(1, 32):
                accepted = concurrent == 0 or stream == 1000 or mask == 31
                with self.subTest(concurrent=concurrent, stream=stream, mask=mask):
                    self.profile(accepted, mask, concurrent, stream,
                                 executors=executors if accepted else None,
                                 reason=None if accepted else 'concurrent windows require')

    def test_empty_stream_population_rejects_even_with_special_work(self):
        for special in ('atomic', 'vector-segment'):
            for concurrent, stream in ((0, 0), (0, 375), (0, 1000),
                                       (1, 0), (1, 375), (1, 1000)):
                with self.subTest(special=special, concurrent=concurrent, stream=stream):
                    self.profile(concurrent == 0, 0, concurrent, stream, extra=special,
                                 reason='stream concurrency requires' if concurrent and stream == 1000 else None)
        self.profile(False, 0, concurrent=0, reason='cannot all be zero')
        # Even always-selected special windows do not turn an empty stream
        # population into a legal profile. NC-only serial control is reachable
        # using translated traffic (Bare is not a legal NC-only UT regime).
        assignments = (*NEUTRAL_POLICIES, *population(0, 'nc'),
                       'translation-bare=0', 'special-concurrent=1000',
                       'concurrent=1000', 'concurrent-stream=1000')
        self.constraints(False, *assignments, reason='stream concurrency requires')
        self.constraints(True, *assignments, 'special-concurrent=0', 'concurrent=0')

    def test_rate_near_endpoints_preserves_executor_selection(self):
        for concurrent in (1, 999, 1000):
            for stream, executors in ((1, (1, 1)), (999, (1, 1)), (1000, (0, 1))):
                with self.subTest(concurrent=concurrent, stream=stream):
                    self.profile(True, 31, concurrent, stream, executors=executors)
                    self.profile(stream == 1000, 1, concurrent, stream)

    def test_literal_vector_capacity_limits_and_disabled_directions(self):
        # Fixed full m4 strided-byte instruction: 64 load flows fit 72-6=66,
        # but not legacy's 63 load budget or either executor's store budget.
        m4 = strided_byte_shape('m4')
        self.profile(True, 4, shape=m4)  # Vector-load only.
        self.profile(False, 8, shape=m4, reason='stream LSQ budget')
        self.profile(False, 12, shape=m4, reason='stream LSQ budget')
        self.profile(False, 31, stream=0, shape=m4, reason='concurrent LSQ budget')
        self.profile(False, 31, stream=375, shape=m4, reason='concurrent LSQ budget')
        # Fixed full m8 strided-byte instruction: 128 flows exceed both queues.
        m8 = strided_byte_shape('m8')
        for mask in (4, 8, 12):
            with self.subTest(mask=mask, flows=128):
                self.profile(False, mask, shape=m8, reason='stream LSQ budget')
                self.profile(True, mask, concurrent=0, shape=m8)
        # Disabled vector directions impose no fit requirement, including when
        # both are disabled and the only enabled stream operation is prefetch.
        self.profile(True, 16, shape=m8)
        # Fixed m2 strided-byte instruction has 32 flows and fits the 52-store
        # stream budget and the legacy 51-store reserve.
        m2 = strided_byte_shape('m2')
        self.profile(True, 8, shape=m2)
        self.profile(True, 31, stream=0, shape=m2)
        self.profile(True, 31, stream=375, shape=m2)
        # A fitting instruction is sufficient; enabling a larger legal shape
        # must not require every instruction to fit one stream admission.
        self.profile(True, 8, shape=(*m8, 'vector-lmul-m2=1', 'vector-emul-m2=1'))

    def test_default_presets_still_validate(self):
        for preset in ('coverage', 'spec', 'corner', 'cmo-stress'):
            with self.subTest(preset=preset):
                self.constraints(True, preset=preset)

    def test_malformed_executor_rates_still_reject(self):
        for key in ('concurrent', 'concurrent-stream'):
            for value in ('1001', '-1', '1.5', 'True', '4294967296', ''):
                with self.subTest(key=key, value=value):
                    self.constraints(False, f'{key}={value}')

    def test_all_operation_masks_accept_sparse_enabled_stream_records(self):
        for mask in range(1, 32):
            for bit, kind in enumerate(STREAM_CLASSES):
                with self.subTest(mask=mask, kind=kind):
                    enabled = bool(mask & (1 << bit))
                    self.accounting(singleton(kind, mask=mask), enabled,
                                    None if enabled else 'disabled operation class')

    def test_segment_and_special_weights_cannot_authorize_stream_operations(self):
        for kind in STREAM_CLASSES:
            for special in ('vector-segment', 'atomic'):
                result = singleton(kind, mask=0)
                weights = [0] * 15
                weights[OPERATIONS.index(special)] = 1
                result['target_ops'] = ','.join(map(str, weights))
                with self.subTest(kind=kind, special=special):
                    self.accounting(result, False, 'disabled operation class')

    def test_stream_only_rejects_each_fabricated_legacy_observation(self):
        for field in ('concurrent', 'concurrent_ops'):
            for index in range(5):
                counts = [0] * 5
                counts[index] = 1
                result = singleton('scalar-load')
                result[field] = ','.join(map(str, counts))
                with self.subTest(field=field, index=index):
                    self.accounting(result, False, 'legacy window')
                    result['target_concurrent_stream'] = 375
                    self.accounting(result, True)
                    result['constraint_schema'] = 69
                    result['target_concurrent_stream'] = 1000
                    self.accounting(result, True)
        for concurrent, stream in ((0, 0), (0, 375), (0, 1000),
                                   (1000, 0), (1000, 375), (1000, 1000)):
            with self.subTest(empty=True, concurrent=concurrent, stream=stream):
                self.accounting(empty_stream(target_concurrent=concurrent,
                                              target_concurrent_stream=stream), True)

    def test_schema69_keeps_previous_operation_mask_compatibility(self):
        for kind in STREAM_CLASSES:
            result = singleton(kind, schema=69, mask=0)
            with self.subTest(kind=kind):
                self.accounting(result, True)
                del result['target_ops']
                del result['concurrent']
                del result['concurrent_ops']
                self.accounting(result, True)

    def test_future_schema_retains_operation_masks_and_stream_only_exclusion(self):
        self.accounting(singleton('prefetch', schema=71, mask=16), True)
        self.accounting(singleton('prefetch', schema=71, mask=1), False,
                        'disabled operation class')
        result = singleton('prefetch', schema=71, mask=16)
        result['concurrent_ops'] = '0,0,0,0,1'
        self.accounting(result, False, 'legacy window')

    def test_schema70_requires_valid_operation_and_stream_only_legacy_fields(self):
        for field in ('target_ops', 'concurrent', 'concurrent_ops'):
            result = singleton('scalar-load')
            del result[field]
            with self.subTest(missing=field):
                self.accounting(result, False)
        for malformed in (True, None, [], '', ','.join(['1'] * 14),
                          ','.join(['1'] * 16), ','.join(['-1'] * 15)):
            with self.subTest(target_ops=malformed):
                self.accounting({**singleton('scalar-load'), 'target_ops': malformed}, False)


if __name__ == '__main__':
    unittest.main()
