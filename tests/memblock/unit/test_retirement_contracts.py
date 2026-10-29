#!/usr/bin/env python3
"""Compile production retirement helpers around a deterministic scheduling shell.

These are helper/scoreboard contract tests, not DUT or full-CPU reachability
proofs. Scheduling and top-level ports are synthetic; decision methods, queue
accounting, transaction builders, and scoreboards are production code.
"""
import hashlib
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PRODUCTION_PATHS = (
    'cpp/environment/issue.cpp', 'cpp/environment/completion.cpp',
    'cpp/environment/control.cpp', 'cpp/environment/cycle.cpp',
    'cpp/environment/observations.cpp',
    'cpp/environment/feedback_monitor.cpp', 'cpp/environment/queue_accounting.hpp',
    'cpp/environment/queue_accounting.cpp', 'cpp/environment/scoreboards.inc',
    'cpp/environment/model.inc', 'cpp/scenarios/mixed_transactions.cpp',
    'cpp/memblock_env.hpp', 'cpp/generated_interface_types.hpp', 'cpp/failure.hpp',
)


def production_hashes():
    return {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
            for path in PRODUCTION_PATHS}


def method(path, signature):
    source = (ROOT / path).read_text()
    start = source.index(signature)
    # Observation helpers may keep their complete body on one line.
    body = source.index('{', start)
    line_end = source.index('\n', body)
    if source[body:line_end].rstrip().endswith('}'):
        return source[start:line_end + 1]
    return source[start:source.index('\n}\n', start) + 3]


class RetirementContractsTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.production_before = production_hashes()
        cls.workspace = tempfile.TemporaryDirectory(prefix='memblock-retirement-contracts-')
        cls.addClassCleanup(cls.workspace.cleanup)
        source = (ROOT / 'unit/retirement_contract_fixture.cpp').read_text()
        methods = []
        for path, signatures in {
            'issue.cpp': (
                'auto Environment::Impl::enqueue_store(const StoreTransaction &transaction) -> bool',
                'auto Environment::Impl::enqueue_store(const StoreTransaction &transaction,',
                'auto Environment::Impl::enqueue_store_pressure(',
                'auto Environment::Impl::issue_store_address_until_tlb_hit(',
                'auto Environment::Impl::run_until_store_complete_with_replay(',
            ),
            'completion.cpp': (
                'auto Environment::Impl::run_until_vector_complete_with_replays(const VectorMemoryTransaction &transaction,',
                'auto Environment::Impl::run_until_vector_complete_with_replays(const std::vector<VectorMemoryTransaction> &transactions,',
                'auto Environment::Impl::commit_vector_store(',
                'auto Environment::Impl::run_until_sq_dequeued(',
                'auto Environment::Impl::service_vector_replay(',
            ),
            'control.cpp': (
                'auto Environment::Impl::set_rob_head(',
                'auto Environment::Impl::set_rob_head_state(',
                'auto Environment::Impl::pulse_pending_store(',
                'auto Environment::Impl::pulse_store_commit_keep_pending(',
            ),
            'observations.cpp': (
                'auto Environment::Impl::scalar_store_address_pending(',
                'auto Environment::Impl::prefetch_pending(',
                'auto Environment::Impl::vector_memory_pending(',
            ),
            'cycle.cpp': (
                'auto Environment::Impl::scalar_store_key(',
                'auto Environment::Impl::vector_store_key(',
            ),
        }.items():
            methods.extend(method('cpp/environment/' + path, signature)
                           .replace('Environment::Impl::', 'RetirementFixture::')
                           for signature in signatures)
        source = source.replace('// PRODUCTION_METHODS', '\n'.join(methods))
        source = source.replace('// PRODUCTION_REPLAY_MATCH', method(
            'cpp/environment/feedback_monitor.cpp',
            'std::optional<VectorReplayRequest> FeedbackMonitor::take_replay_for('))
        header = (ROOT / 'cpp/environment/queue_accounting.hpp').read_text()
        queue_class = header[header.index('class QueueAccounting {'):header.index('\n} // namespace memblock')]
        queue_methods = (ROOT / 'cpp/environment/queue_accounting.cpp').read_text()
        queue_methods = queue_methods[queue_methods.index('void QueueAccounting::'):queue_methods.rindex('} // namespace memblock')]
        source = source.replace('// PRODUCTION_QUEUE_ACCOUNTING',
                                (queue_class + '\n' + queue_methods).replace('DutAdapter', 'FixtureDut'))
        builders = [method('cpp/scenarios/mixed_transactions.cpp', signature) for signature in (
            'memblock::LoadTransaction MixedScenario::make_load(',
            'memblock::StoreTransaction MixedScenario::make_store(',
        )]
        source = source.replace('// PRODUCTION_BUILDERS', '\n'.join(builders))
        path = Path(cls.workspace.name) / 'fixture.cpp'
        path.write_text(source)
        cls.binary = path.with_suffix('')
        result = subprocess.run(
            ['c++', '-std=c++20', '-O0', '-I', str(ROOT / 'cpp'), str(path), '-o', str(cls.binary)],
            text=True, capture_output=True, timeout=60)
        if result.returncode:
            raise RuntimeError(result.stdout + result.stderr)
        if production_hashes() != cls.production_before:
            raise RuntimeError('production source changed while compiling retirement fixture')

    @classmethod
    def tearDownClass(cls):
        if production_hashes() != cls.production_before:
            raise AssertionError('production source changed during retirement contract tests')

    def check_case(self, name):
        result = subprocess.run([str(self.binary), name], text=True, capture_output=True, timeout=20)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_scalar_wait_is_scoped_and_matches_rob_wrap(self):
        self.check_case('scalar_scoped')

    def test_completed_address_is_not_reissued_while_data_is_pending(self):
        self.check_case('scalar_data_pending')

    def test_scalar_timeout_retains_obligation_and_clears_pending_head(self):
        self.check_case('scalar_timeout')

    def test_sta_feedback_requires_new_matching_lane_sq_and_wrap(self):
        self.check_case('sta_feedback')

    def test_sta_unrelated_feedback_cannot_retry_or_complete(self):
        self.check_case('sta_timeout')

    def test_store_builder_freezes_dispatch_snapshot_before_younger_loads(self):
        self.check_case('dispatch_snapshot')

    def test_typed_enqueue_checks_all_cursors_after_cancellation_and_wrap(self):
        self.check_case('dispatch_cursors')

    def test_vector_commit_uses_recorded_target_and_preserves_reference_bytes(self):
        self.check_case('vector_commit')

    def test_vector_commit_timeout_retains_target_and_reference_bytes(self):
        self.check_case('vector_commit_timeout')

    def test_vector_list_wait_retains_unrelated_obligations_and_replays(self):
        self.check_case('vector_list')

    def test_vector_list_rejects_replay_of_already_completed_member(self):
        self.check_case('vector_completed_replay')

    def test_held_vector_replay_keeps_pending_head_without_scalar_commit(self):
        self.check_case('vector_held')

    def test_stream_queries_use_class_wrap_uop_and_destination_identity(self):
        self.check_case('stream_queries')

    def test_stream_replay_service_selects_one_circular_flow_without_retirement(self):
        self.check_case('stream_service')

    def test_stream_replay_service_rejects_completed_target(self):
        self.check_case('stream_service_completed')

    def test_stream_replay_service_reports_failed_issue_without_acceptance(self):
        self.check_case('stream_service_failed')

    def test_stream_head_state_is_atomic_and_preserves_pending_level(self):
        self.check_case('stream_head')


if __name__ == '__main__':
    unittest.main()
