#!/usr/bin/env python3
"""Compile actual mixed schedulers against an independent obligation checker.

The shell gives accepted producers deterministic synthetic completion times. It
checks the legality of events the production scheduler selects; it never builds
its ready-candidate list. These tests establish scheduler contracts, not DUT
latency behavior or full-CPU input reachability.
"""
from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
INPUTS = (
    'cpp/scenarios/mixed_stream.cpp', 'cpp/scenarios/mixed_scheduler.cpp',
    'cpp/scenarios/mixed_initialization.cpp',
    'cpp/scenarios/mixed_transactions.cpp', 'cpp/scenarios/mixed_vectors.cpp',
    'cpp/scenarios/mixed/state.hpp', 'cpp/scenarios/shared.hpp',
    'cpp/scenarios/mixed/scalar_load.hpp', 'cpp/scenarios/mixed/vector_indices.hpp',
    'cpp/scenarios/mixed/vector_operands.hpp', 'cpp/scenarios/mixed/stream_oracle.hpp',
    'cpp/scenarios/mixed/stream_recovery.hpp', 'cpp/scenarios/mixed/unresolved_load_group.hpp',
    'cpp/scenarios/mixed/unresolved_epochs.hpp', 'unit/mixed_stream_fixture.cpp',
    'cpp/random/constraints.inc', 'cpp/random/coverage.inc',
    'cpp/environment/model.inc', 'cpp/memblock_env.hpp',
    'cpp/generated_interface_types.hpp',
)


def input_hashes():
    return {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest() for path in INPUTS}


def method(path, signature):
    text = (ROOT / path).read_text()
    start = text.index(signature)
    return text[start:text.index('\n}\n', start) + 3]


class MixedStreamTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.before = input_hashes()
        cls.runs = []
        cls.mutations = {}
        cls.replays = 0
        cls.constraint_case = None
        cls.reservations = []
        cls.recoveries = []
        cls.cold_recoveries = []
        cls.profiles = []
        cls.dependencies = []
        cls.workspace = tempfile.TemporaryDirectory(prefix='memblock-mixed-stream-')
        cls.addClassCleanup(cls.workspace.cleanup)
        cls.addClassCleanup(cls.finalize)
        source = (ROOT / 'unit/mixed_stream_fixture.cpp').read_text()
        builders = [method('cpp/scenarios/mixed_transactions.cpp', signature) for signature in (
            'memblock::LoadTransaction MixedScenario::make_load(',
            'memblock::StoreTransaction MixedScenario::make_store(',
            'memblock::PrefetchTransaction MixedScenario::make_prefetch(',
            'unsigned long MixedScenario::constrained_window_address(',
        )]
        vectors = [method('cpp/scenarios/mixed_vectors.cpp', signature) for signature in (
            'unsigned int MixedScenario::vector_shape_uop_count(',
            'unsigned int MixedScenario::vector_shape_flow_num(',
            'unsigned int MixedScenario::vector_shape_total_flows(',
            'unsigned int MixedScenario::vector_shape_deficit(',
            'const RandomVectorShape & MixedScenario::choose_vector_shape(',
            'std::vector<memblock::VectorMemoryTransaction> MixedScenario::make_random_vector_uops(',
            'void MixedScenario::sample_constrained_vector_instruction(',
        )]
        source = source.replace('// PRODUCTION_BUILDERS_AND_VECTOR_PRODUCERS', '\n'.join(builders + vectors))
        stream = (ROOT / 'cpp/scenarios/mixed_stream.cpp').read_text()
        stream = '\n'.join(line for line in stream.splitlines() if not line.startswith('#include'))
        # Observe selected edges and operand arithmetic, never the ready list.
        stream = stream.replace('const auto &source = *sources[random() % sources.size()];',
            'const auto &source = *sources[random() % sources.size()];\n'
            '                    environment.bind_dependency(admitted, source.load, data, source.expected_scalar);')
        stream = stream.replace('return (address & ~std::uint64_t{0xfff}) | offset;',
            'return environment.check_dependent_address(address, '
            '(address & ~std::uint64_t{0xfff}) | offset, *address_value);')
        source = source.replace('// PRODUCTION_STREAM', stream)
        execute = method('cpp/scenarios/mixed_scheduler.cpp', 'bool MixedScenario::execute()')
        source = source.replace('// PRODUCTION_EXECUTE', execute.replace('MixedScenario::', 'BudgetFixture::'))
        cls.source = source
        cls.binary = cls.compile(source, 'baseline')
        cls.check_inputs()

    @classmethod
    def compile(cls, source, name):
        path = Path(cls.workspace.name) / (name + '.cpp')
        path.write_text(source)
        binary = path.with_suffix('')
        result = subprocess.run(
            ['c++', '-std=c++20', '-O0', '-I', str(ROOT / 'cpp'), str(path), '-o', str(binary)],
            capture_output=True, text=True, timeout=60,
        )
        if result.returncode:
            raise RuntimeError(result.stdout + result.stderr)
        return binary

    @classmethod
    def check_inputs(cls):
        if input_hashes() != cls.before:
            raise AssertionError('production inputs changed during mixed scheduler tests')

    @classmethod
    def finalize(cls):
        cls.check_inputs()
        destination = os.environ.get('MEMBLOCK_MIXED_STREAM_REPORT')
        if destination:
            stream = [run for run in cls.runs if run['mode'] == 'stream']
            fixed = [run for run in stream if run['family'] == 1 and run['limit'] == 8 and run['timing'] == 0]
            mixed = [run for run in stream if run['family'] == 0 and run['limit'] == 32 and run['timing'] == 1]
            report = {
                'production_hashes': cls.before,
                'scope': 'compiled production scheduler decisions with synthetic completion times; no DUT',
                'successful_stream_runs': len(stream),
                'successful_budget_runs': sum(run['mode'] == 'budget' for run in cls.runs),
                'invariants_checked_per_successful_stream_run': [
                    'architectural-prefix admission and retirement', 'exactly-once producer acceptance',
                    'expectation/dispatch/retirement and LQ/SQ conservation',
                    'class-distinct destinations held until retirement',
                    'head-only scalar/vector split and ordered-load issue',
                    'known older-store addresses before younger loads',
                    'repeated-Vd producer completion before dependent issue',
                    'installed ROB head and pending-store level',
                    'production action/operation/overlap coverage matches independent observations'],
                'schedule_fields': 'admission kind/identity; producer class/uop/identity; architectural prefix retirement',
                'excluded_from_schedule': ['address', 'payload', 'cycle', 'delay'],
                'fixed_eight_load_structural_schedules': len({run['result']['schedule'] for run in fixed}),
                'fixed_eight_load_samples': len(fixed),
                'unique_structural_schedules': len({run['result']['schedule'] for run in mixed}),
                'unique_instruction_kind_sequences': len({tuple(run['result']['kinds']) for run in mixed}),
                'unique_producer_count_sequences': len({tuple(run['result']['producer_counts']) for run in mixed}),
                'epoch_lengths': sorted({run['result']['actions'] for run in mixed}),
                'observations': {field: sum(run['result'][field] for run in stream) for field in (
                    'overlap_admissions', 'continued_admissions', 'loads_before_older_std',
                    'head_checks', 'dependency_checks', 'admissions_with_completed_head',
                    'issues_with_completed_head')},
                'peak_live': max((run['result']['peak_live'] for run in mixed), default=0),
                'peak_accepted': max((run['result']['peak_accepted'] for run in mixed), default=0),
                'mutation_sensitivity': cls.mutations,
                'deterministic_replay_cases': cls.replays,
                'constraint_case': cls.constraint_case,
                'reservations': cls.reservations,
                'recovery_cases': cls.recoveries,
                'validated_profile_cases': cls.profiles,
                'dependency_cases': cls.dependencies,
                'validated_profile_masks': sorted({case['mask'] for case in cls.profiles}),
                'cold_recovery_cases': cls.cold_recoveries,
                'cold_structural_schedules': len({run['result']['schedule'] for run in cls.cold_recoveries}),
                'recovery_observations': {field: sum(run['result'][field] for run in cls.recoveries)
                    for field in ('recovery_windows', 'recovery_rounds', 'recovery_prefix',
                                  'refetch_issues', 'surviving_prefix_redirects', 'checked_scalar_values', 'lq_canceled')},
                'explicit_gaps': ['synthetic ports and completions', 'replay service reports no replay',
                                  'bare translation only', 'representative decoder-legal vector shapes'],
                'runs': cls.runs,
            }
            path = Path(destination)
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(json.dumps(report, indent=2) + '\n')

    def run_fixture(self, mode, *arguments, binary=None):
        self.check_inputs()
        result = subprocess.run([str(binary or self.binary), mode, *map(str, arguments)],
                                capture_output=True, text=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        return json.loads(result.stdout)

    def stream(self, seed, timing=1, family=0, limit=32, binary=None):
        result = self.run_fixture('stream', seed, timing, family, limit, binary=binary)
        if binary is None:
            self.runs.append({'mode': 'stream', 'seed': seed, 'timing': timing,
                              'family': family, 'limit': limit, 'result': result})
        return result

    def assert_sliding_observations(self, results):
        self.assertGreaterEqual(max(run['peak_live'] for run in results), 3)
        self.assertGreaterEqual(max(run['peak_accepted'] for run in results), 2)
        self.assertGreater(sum(run['overlap_admissions'] for run in results), 0)
        self.assertGreater(sum(run['continued_admissions'] for run in results), 0)

    def test_seed_changes_structural_instruction_and_producer_schedules(self):
        results = [self.stream(seed) for seed in range(1, 65)]
        self.assertGreaterEqual(len({run['schedule'] for run in results}), 32)
        self.assertGreaterEqual(len({tuple(run['kinds']) for run in results}), 16)
        self.assertGreaterEqual(len({tuple(run['producer_counts']) for run in results}), 16)
        self.assertGreaterEqual(len({run['actions'] for run in results}), 8)
        self.assertGreaterEqual(len({run['peak_live'] for run in results}), 3)
        self.assertEqual(set().union(*(set(run['kinds']) for run in results)), set(range(5)))
        self.assert_sliding_observations(results)
        self.assertGreater(sum(run['head_checks'] for run in results), 0)
        self.assertGreater(sum(run['dependency_checks'] for run in results), 0)

    def test_dependent_streams_gate_operands_and_vary_graph_structure(self):
        results = [self.run_fixture('dependency', seed, timing, 0, 32)
                   for timing in (0, 1) for seed in range(1, 65)]
        self.dependencies.extend(results)
        for run in results:
            counts = run['dependency_counts']
            self.assertEqual(counts[0:2], counts[2:4])
            self.assertEqual(run['dependency_edges'], sum(counts[:2]))
            self.assertGreaterEqual(run['dependency_operand_checks'], run['dependency_edges'])
        for index in (0, 1, 4, 5, 6, 7):
            self.assertGreater(sum(run['dependency_counts'][index] for run in results), 0)
        self.assertGreaterEqual(max(run['dependency_counts'][8] for run in results), 2)
        self.assertGreaterEqual(len({tuple(run['dependency_counts']) for run in results}), 32)

    def test_fixed_load_population_varies_dependency_edges_and_replays(self):
        results = [self.run_fixture('dependency', seed, 1, 1, 8) for seed in range(1, 65)]
        self.dependencies.extend(results)
        edges = [tuple(event for event in run['schedule'].split(',') if event.startswith('D'))
                 for run in results]
        # Same instruction count/class: compare actual consumer/source identities
        # and directions, excluding addresses, payloads, delays and event times.
        self.assertGreaterEqual(len(set(edges)), 24)
        for seed in (1, 7, 19):
            self.assertEqual(results[seed - 1], self.run_fixture('dependency', seed, 1, 1, 8))
        for seed in range(1, 17):
            control = self.run_fixture('independent', seed, 1, 0, 32)
            self.assertEqual(control['dependency_counts'], [0] * 9)
            self.assertEqual(control['dependency_edges'], 0)

    def test_private_wrong_dependency_operand_is_rejected(self):
        for name, original, replacement, message in (
            ('wrong-bound-source', '= source.expected_scalar;', '= source.expected_scalar ^ 1U;',
             'dependent address value disagrees with bound producer expectation'),
            ('wrong-address', '((*address_value & 63U) << 6U)', '((*address_value & 63U) << 5U)',
             'dependent ALU address disagrees'),
            ('wrong-data', 'data_value ? *data_value : random()', 'data_value ? (*data_value ^ 1U) : random()',
             'dependent store data disagrees'),
        ):
            self.assertIn(original, self.source)
            binary = self.compile(self.source.replace(original, replacement), name)
            rejected = 0
            for seed in range(1, 17):
                run = subprocess.run([str(binary), 'dependency', str(seed), '1', '0', '32'],
                                     capture_output=True, text=True, timeout=10)
                if run.returncode:
                    self.assertIn(message, run.stderr)
                    rejected += 1
            self.assertGreater(rejected, 0)

    def test_private_early_dependency_release_is_rejected(self):
        source = self.source
        # Release every live producer before the independent shell completes it.
        source = source.replace('work.issued[0] && !pending(work)', 'true')
        binary = self.compile(source, 'early-dependency')
        rejected = 0
        for seed in range(1, 33):
            run = subprocess.run([str(binary), 'dependency', str(seed), '1', '0', '32'],
                                 capture_output=True, text=True, timeout=10)
            if run.returncode:
                self.assertIn('consumer issued before independent producer completion', run.stderr)
                rejected += 1
        self.assertGreater(rejected, 0)

    def test_fixed_eight_load_population_varies_event_schedule(self):
        results = [self.stream(seed, timing=0, family=1, limit=8) for seed in range(1, 65)]
        for run in results:
            self.assertEqual(run['actions'], 8)
            self.assertEqual(run['kinds'], [0] * 8)
            self.assertEqual(run['producer_counts'], [1] * 8)
        self.assertGreaterEqual(len({run['schedule'] for run in results}), 32)
        self.assert_retirement_interleaving(results)

    def test_recovery_preserves_mixed_prefix_and_strict_epochs(self):
        results = []
        for conflict in (0, 500, 1000):
            for timing in (0, 1):
                for seed in range(1, 33):
                    with self.subTest(conflict=conflict, timing=timing, seed=seed):
                        result = self.run_fixture('recovery', seed, timing, 0, 32, conflict)
                        results.append(result)
                        self.recoveries.append(dict(seed=seed, timing=timing, conflict=conflict, result=result))
        self.assertGreater(sum(r['recovery_windows'] for r in results), 0)
        self.assertGreater(sum(r['recovery_rounds'] for r in results), 0)
        self.assertGreater(sum(r['recovery_prefix'] for r in results), 0)
        self.assertGreater(sum(r['refetch_issues'] for r in results), 0)
        self.assertGreater(sum(r['lq_canceled'] for r in results), 0)
        self.assertGreater(sum(r['surviving_prefix_redirects'] for r in results), 0)

    def test_unfinished_recovery_keeps_strict_cancellation_and_target_grant_boundary(self):
        results = []
        for timing in (0, 1):
            for seed in range(1, 65):
                result = self.run_fixture('recovery', seed, timing, 0, 32, 1000, 1000)
                results.append(result)
                self.cold_recoveries.append(dict(seed=seed, timing=timing, result=result))
                self.assertEqual(result['cold_selected'], result['cold_canceled'])
                self.assertEqual(result['cold_canceled'], result['old_released'])
                self.assertEqual(result['old_released'], result['cold_refetched'])
        self.assertGreater(sum(r['cold_selected'] for r in results), 32)
        self.assertGreater(sum(r['old_prefix_retirements'] for r in results), 0)
        self.assertGreater(sum(r['old_prefix_producers'] for r in results), 0)
        self.assertGreaterEqual(len({r['schedule'] for r in results if r['cold_selected']}), 32)
        for seed in (1, 7, 29):
            first = self.run_fixture('recovery', seed, 1, 0, 32, 1000, 1000)
            self.assertEqual(first, self.run_fixture('recovery', seed, 1, 0, 32, 1000, 1000))

    def test_unfinished_disabled_controls_and_unavailable_freshness_keep_completed_path(self):
        for conflict, rate, freshness in ((1000, 0, ()), (0, 1000, ()), (1000, 1000, ('unavailable',)),
                                          (1000, 1000, ('narrow-history',))):
            results = [self.run_fixture('recovery', seed, 1, 0, 32, conflict, rate, *freshness)
                       for seed in range(1, 17)]
            self.assertEqual(sum(r['cold_selected'] for r in results), 0)
            self.assertGreater(sum(r['recovery_windows'] for r in results), 0)
            self.assertEqual(sum(r['cold_canceled'] for r in results), 0)
            if freshness:
                self.assertGreater(sum(r['cold_unavailable'] for r in results), 0)
            else:
                self.assertEqual(sum(r['cold_attempts'] for r in results), 0)

    def test_narrow_prior_A_anywhere_in_line_prevents_cold_selection(self):
        old = 'environment.dcache_requests_covering_since(physical + byte, 0)'
        self.assertEqual(self.source.count(old), 1)
        binary = self.compile(self.source.replace(old, 'environment.dcache_requests_covering_since(physical, 0)'),
                              'cold_history_line_base_only')
        detected = None
        for seed in range(1, 17):
            result = subprocess.run([str(binary), 'recovery', str(seed), '1', '0', '32', '1000', '1000', 'narrow-history'],
                                    capture_output=True, text=True, timeout=10)
            if result.returncode:
                self.assertIn('cold hold ignored a narrow prior A', result.stderr)
                detected = dict(rejected=True, seed=seed, reason=result.stderr.strip())
                break
        self.assertIsNotNone(detected)
        self.mutations['cold_history_line_base_only'] = detected

    def test_successor_completed_between_idle_observations_restarts_quiet_time(self):
        results = [self.run_fixture('recovery', seed, 1, 0, 32, 1000, 1000, 'successor-history')
                   for seed in range(1, 17)]
        self.assertGreater(sum(r['cold_selected'] for r in results), 0)
        old = '(episode.old_quiet_since && requests != episode.old_quiet_requests)'
        self.assertEqual(self.source.count(old), 1)
        binary = self.compile(self.source.replace(old, 'false'), 'missed_successor_acquire')
        detected = None
        for seed in range(1, 17):
            result = subprocess.run([str(binary), 'recovery', str(seed), '1', '0', '32', '1000', '1000', 'successor-history'],
                                    capture_output=True, text=True, timeout=10)
            if result.returncode:
                self.assertIn('cold refetch preceded old Grant completion', result.stderr)
                detected = dict(rejected=True, seed=seed, reason=result.stderr.strip())
                break
        self.assertIsNotNone(detected)
        self.mutations['missed_successor_acquire'] = detected

    def test_private_cold_cancellation_lifecycle_and_data_faults_are_rejected(self):
        mutations = (
            ('late_cold_cancel', '!environment.cancel_scalar_load_after_redirect(cold)',
             '(!environment.run_cycles(1) || !environment.cancel_scalar_load_after_redirect(cold))',
             'missed exact scalar cancellation boundary'),
            ('lost_cold_cancel', '!environment.cancel_scalar_load_after_redirect(cold)',
             'false', 'surviving-prefix-queue-contract'),
            ('early_cold_refetch', '!environment.dcache_line_grants_idle(*episode.cold_physical)',
             'false', 'cold refetch preceded old Grant completion'),
            ('wrong_cold_data', 'episode.cold_expected = oracle.scalar_load(cold);',
             'episode.cold_expected = oracle.scalar_load(cold) ^ 1U;',
             'expected value disagreed with independent input byte history'),
        )
        for name, old, new, diagnostic in mutations:
            self.assertEqual(self.source.count(old), 1)
            mutated = self.source.replace(old, new)
            if name == 'early_cold_refetch':
                mutated = mutated.replace('!episode.old_quiet_since && environment.dcache_line_grants_idle(*episode.cold_physical)',
                                          '!episode.old_quiet_since')
            binary = self.compile(mutated, name)
            detected = None
            for seed in range(1, 65):
                result = subprocess.run([str(binary), 'recovery', str(seed), '1', '0', '32', '1000', '1000'],
                                        capture_output=True, text=True, timeout=10)
                if result.returncode:
                    self.assertIn(diagnostic, result.stderr)
                    detected = dict(rejected=True, seed=seed, reason=result.stderr.strip())
                    break
            self.assertIsNotNone(detected, name + ' escaped the independent cold checker')
            self.mutations[name] = detected

    def test_recovery_seed_replay_and_short_budgets(self):
        for limit in (0, 1, 2, 3, 7, 8, 31):
            for seed in (1, 7, 29):
                first = self.run_fixture('recovery', seed, 1, 0, limit, 1000)
                self.assertEqual(first, self.run_fixture('recovery', seed, 1, 0, limit, 1000))
                self.assertLessEqual(first['actions'], limit)
                self.assertEqual(first['recovery_windows'], 0 if limit < 2 else first['recovery_windows'])

    def test_private_recovery_deadline_snapshot_and_suffix_faults_are_rejected(self):
        mutations = (
            ('late_redirect', 'violation.last_cycle + 2U', 'violation.last_cycle + 3U',
             'missed backend redirect deadline'),
            ('lost_refetch_lq_rewind', 'lq_offset -= canceled;', '(void)canceled;',
             'refetch used a stale LQ cursor'),
            ('changed_refetch_destination', 'load_work.load = load;',
             'load.pdest = (load.pdest + 1U) % 191U + 1U; load_work.load = load;',
             'refetch changed architectural or frontend identity'),
            ('stale_prefix_snapshot', 'const auto snapshot = oracle.line_snapshot(episode.base, reference_pages_for(episode.base));',
             'std::array<unsigned char, 64> snapshot{}; for (unsigned byte=0; byte<64; ++byte) snapshot[byte] = environment.memory().read_byte(episode.base + byte);',
             'expected value disagreed with independent input byte history'),
        )
        for name, old, new, diagnostic in mutations:
            self.assertEqual(self.source.count(old), 1)
            binary = self.compile(self.source.replace(old, new), name)
            detected = None
            for seed in range(1, 257):
                result = subprocess.run([str(binary), 'recovery', str(seed), '1', '0', '32', '1000'],
                                        capture_output=True, text=True, timeout=10)
                if result.returncode:
                    self.assertIn(diagnostic, result.stderr)
                    detected = dict(rejected=True, seed=seed, reason=result.stderr.strip())
                    break
            self.assertIsNotNone(detected, name + ' escaped the independent recovery checker')
            self.mutations[name] = detected

    def assert_retirement_interleaving(self, results):
        self.assertGreater(sum(run['admissions_with_completed_head'] for run in results), 0)
        self.assertGreater(sum(run['issues_with_completed_head'] for run in results), 0)

    def test_admission_and_issue_can_precede_ready_head_retirement(self):
        results = [self.stream(seed, timing=0) for seed in range(1, 65)]
        self.assert_retirement_interleaving(results)

    def test_short_default_profiles_retain_concurrency_and_serial_budget(self):
        expected = {
            'coverage': (127, 511), 'corner': (127, 511),
            'spec': (64, 102), 'cmo-stress': (0, 0),
        }
        for profile, budgets in expected.items():
            for actions, expected_budget in zip((256, 1024), budgets):
                reservation = self.run_fixture('reservation', profile, actions - 2)
                self.assertEqual(reservation['budget'], expected_budget)
                self.reservations.append(dict(profile=profile, actions=actions, **reservation))
                for seed in (1, 7):
                    for rate in (0, 750, 1000):
                        result = self.run_fixture('budget', seed, actions - 2, expected_budget, rate)
                        self.runs.append({'mode': 'budget', 'seed': seed, 'profile': profile,
                                          'rate': rate, 'result': result})
                        used = result['concurrent']
                        self.assertLessEqual(used, expected_budget)
                        self.assertLess(expected_budget - used, 7 if rate == 0 else 1)
                        self.assertEqual(result['actions'], actions - 2)
                        self.assertIn('S', result['schedule'].split(','))

    def test_reservation_bounds_large_runs_and_integer_boundaries(self):
        for profile in ('coverage', 'spec', 'corner', 'cmo-stress'):
            floor = self.run_fixture('reservation', profile, 0)['floor']
            values = sorted({0, 1, 2, 55, 56, 63, 64, 254, 1022, 16382,
                             2 * floor - 1, 2 * floor, 2 * floor + 1, 2**32 - 1})
            previous = 0
            for available in values:
                if available < 0:
                    continue
                result = self.run_fixture('reservation', profile, available)
                budget, rate = result['budget'], result['rate']
                self.assertGreaterEqual(budget, previous)
                self.assertLessEqual(budget, available)
                self.assertGreaterEqual(available - budget, min(floor, available // 2))
                if rate:
                    self.assertGreater(budget, 0 if available else -1)
                else:
                    self.assertEqual(budget, 0)
                if available >= 2 * floor:
                    minimum = min(available, 64)  # All enabled standard presets include clean special traffic.
                    expected = min(max(available * rate // 1000, minimum), available - floor) if rate else 0
                    self.assertEqual(budget, expected)
                previous = budget

    def test_identical_seed_and_completion_pattern_replays_exactly(self):
        for seed in (1, 2, 7, 13, 31, 79):
            for timing in (0, 1):
                with self.subTest(seed=seed, timing=timing):
                    first = self.stream(seed, timing)
                    self.assertEqual(first, self.stream(seed, timing))
                    type(self).replays += 1

    def test_load_can_issue_after_older_address_before_older_scalar_data(self):
        results = [self.stream(seed, timing=0) for seed in range(1, 65)]
        self.assertGreater(sum(run['loads_before_older_std'] for run in results), 0)

    def test_production_validated_stream_profiles_respect_every_operation_subset(self):
        for mask in range(1, 32):
            results = []
            for seed in (1, 7, 29, 61):
                for timing in (0, 1):
                    result = self.run_fixture('profile', seed, timing, mask, 32)
                    self.profiles.append(dict(seed=seed, timing=timing, mask=mask, result=result))
                    results.append(result)
                    self.assertTrue(all(mask & (1 << kind) for kind in result['kinds']))
            observed = set().union(*(set(r['kinds']) for r in results))
            self.assertEqual(observed, {kind for kind in range(5) if mask & (1 << kind)})
            self.assertGreater(len({r['schedule'] for r in results}), 4)

    def test_obligations_hold_across_completion_patterns_and_operation_masks(self):
        for timing in (0, 1):
            for family in range(1, 6):
                for seed in range(1, 9):
                    with self.subTest(timing=timing, family=family, seed=seed):
                        result = self.stream(seed, timing, family)
                        self.assertEqual(set(result['kinds']), {family - 1})
                        expected = (2 if family == 3 else 1)
                        if family in (1, 3, 5):
                            self.assertEqual(set(result['producer_counts']), {expected})

    def test_zero_short_and_full_instruction_limits_conserve_work(self):
        for limit in (0, 1, 2, 3, 7, 8, 31, 32):
            for seed in (1, 7, 29):
                with self.subTest(limit=limit, seed=seed):
                    result = self.stream(seed, limit=limit)
                    self.assertLessEqual(result['actions'], limit)
                    if limit < 8:
                        self.assertEqual(result['actions'], limit)
                    self.assertEqual(sum(event.startswith('A') for event in result['schedule'].split(',')), result['actions'])
                    self.assertEqual(result['schedule'].count('R'), result['actions'])

    def test_real_constraint_presets_rate_boundaries_and_serialization(self):
        result = self.run_fixture('constraints')
        self.assertEqual(result, {'presets': 4, 'boundary_overrides': 16, 'out_of_range_rejections': 8})
        type(self).constraint_case = result

    def test_actual_execute_interleaves_serial_work_and_conserves_budgets(self):
        schedules = []
        legacy_sizes = set()
        for rate in (0, 500, 1000):
            for concurrent in (0, 1, 6, 7, 8, 9, 14, 15, 31, 97):
                for seed in range(1, 9):
                    with self.subTest(rate=rate, concurrent=concurrent, seed=seed):
                        result = self.run_fixture('budget', seed, 97, concurrent, rate)
                        self.runs.append({'mode': 'budget', 'seed': seed, 'rate': rate,
                                          'concurrent': concurrent, 'result': result})
                        self.assertEqual(result['actions'], 97)
                        used = result['concurrent']
                        self.assertLessEqual(used, concurrent)
                        if rate == 0:
                            self.assertLess(concurrent - used, 7)
                            self.assertNotIn('C', result['schedule'])
                        else:
                            self.assertEqual(used, concurrent)
                        events = result['schedule'].split(',')
                        for event in events:
                            if event.startswith('W'):
                                self.assertIn(int(event[1:]), (7, 8))
                                legacy_sizes.add(int(event[1:]))
                        if rate == 1000:
                            self.assertFalse(any(event.startswith('W') for event in events))
                        if rate == 1000 and concurrent == 31:
                            schedules.append(result['schedule'])
        self.assertEqual(legacy_sizes, {7, 8})
        self.assertGreaterEqual(len(set(schedules)), 4)
        self.assertTrue(any(',C' in schedule and ',S,' in schedule for schedule in schedules))

    def test_private_serial_admission_mutation_loses_required_overlap(self):
        old = 'admitted < length && live.size() < depth &&'
        self.assertEqual(self.source.count(old), 1)
        binary = self.compile(self.source.replace(old, 'admitted < length && live.empty() &&'), 'serial-admission')
        results = [self.stream(seed, binary=binary) for seed in range(1, 17)]
        with self.assertRaises(AssertionError):
            self.assert_sliding_observations(results)
        self.mutations['serial_admission'] = {'rejected': True, 'peak_live': max(run['peak_live'] for run in results),
                                             'overlap_admissions': sum(run['overlap_admissions'] for run in results)}

    def test_private_greedy_retirement_loses_ready_head_interleaving(self):
        old = 'const auto event = ready[random() % ready.size()];'
        self.assertEqual(self.source.count(old), 1)
        binary = self.compile(self.source.replace(old,
            'const auto event = completed_head ? Event{5, 0, 0} : ready[random() % ready.size()];'),
            'greedy-retirement')
        results = [self.stream(seed, timing=0, binary=binary) for seed in range(1, 17)]
        with self.assertRaises(AssertionError):
            self.assert_retirement_interleaving(results)
        self.mutations['greedy_retirement'] = {'rejected': True,
            'admissions_with_completed_head': sum(run['admissions_with_completed_head'] for run in results),
            'issues_with_completed_head': sum(run['issues_with_completed_head'] for run in results)}

    def test_private_missing_older_address_guard_is_rejected(self):
        scalar = '(work.kind == 4 || older_addresses_known)'
        vector = '(work.kind == 3 || older_addresses_known)'
        self.assertEqual(self.source.count(scalar), 1)
        self.assertEqual(self.source.count(vector), 1)
        binary = self.compile(self.source.replace(scalar, 'true').replace(vector, 'true'), 'unknown-address-load')
        rejected = None
        for seed in range(1, 33):
            result = subprocess.run([str(binary), 'stream', str(seed), '1', '0', '32'],
                                    capture_output=True, text=True, timeout=10)
            if result.returncode and 'older' in result.stderr and 'address' in result.stderr:
                rejected = {'rejected': True, 'seed': seed, 'reason': result.stderr.strip()}
                break
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIsNotNone(rejected, 'removing older-address guards escaped the independent event checker')
        self.mutations['missing_older_address_guard'] = rejected


if __name__ == '__main__':
    unittest.main()
