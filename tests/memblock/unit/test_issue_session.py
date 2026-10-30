#!/usr/bin/env python3
"""Compile persistent API/tick against independent pins and production scoreboard."""
from pathlib import Path
import os
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


def method(file, name, owner='Environment::Impl'):
    source = (ROOT / 'cpp/environment' / file).read_text()
    begin = source.index('auto ' + owner + '::' + name + '(') if owner == 'Environment::Impl' else source.index(
        'std::optional<VectorReplayRequest> ' + owner + '::' + name + '(')
    return source[begin:source.index('\n}\n', begin) + 3]


class IssueSessionTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.directory = Path(tempfile.mkdtemp(prefix='memblock-issue-session-'))
        if not os.environ.get('MEMBLOCK_ISSUE_SESSION_KEEP_ARTIFACTS'):
            cls.addClassCleanup(shutil.rmtree, cls.directory)
        else:
            # Keep compiled private faults and source for reproducible review.
            print('issue_session_artifacts=' + str(cls.directory))
        pins = (ROOT / 'unit/retained_issue_fixture.cpp').read_text()
        pins = pins[:pins.index('struct Obligation {')].replace('struct Adapter {', 'struct PinAdapter {')
        replay = (ROOT / 'cpp/environment/feedback_monitor.hpp').read_text()
        begin = replay.index('struct VectorReplayRequest {')
        replay = replay[begin:replay.index('\n};', begin) + 3]
        source = (ROOT / 'unit/issue_session_fixture.cpp').read_text()
        source = source.replace('// PIN_WITNESSES', pins).replace('// REPLAY_REQUEST', replay)
        session = (ROOT / 'cpp/environment/issue_session.inc').read_text()
        session = session.split('namespace memblock {', 1)[1].rsplit('} // namespace memblock', 1)[0]
        methods = [session]
        methods.extend(method('cycle.cpp', name) for name in ('tick', 'scalar_store_key', 'vector_store_key'))
        methods.append(method('manager_control.cpp', 'make_vector_memory_issue'))
        methods.append(method('feedback_monitor.cpp', 'take_replay_for', 'FeedbackMonitor').replace('FeedbackMonitor::', 'Feedback::'))
        cls.source = '#include "environment/issue_payloads.hpp"\n' + source.replace(
            '// PRODUCTION_METHODS', '\n'.join(methods).replace('Environment::Impl::', 'Harness::'))
        cls.binary = cls.compile(cls.source, 'baseline')

    @classmethod
    def compile(cls, source, name, retained_source=None):
        directory = cls.directory / name
        directory.mkdir()
        if retained_source is not None:
            (directory / 'environment').mkdir()
            (directory / 'environment/retained_issue.hpp').write_text(retained_source)
        path = directory / 'fixture.cpp'
        path.write_text(source)
        binary = directory / 'fixture'
        result = subprocess.run(['c++', '-std=c++20', '-O0', '-g', '-I', str(directory),
                                 '-I', str(ROOT / 'cpp'), str(path), '-o', str(binary)],
                                capture_output=True, text=True, timeout=60)
        (directory / 'compile.log').write_text(result.stdout + result.stderr)
        if result.returncode:
            raise AssertionError(result.stdout + result.stderr)
        return binary

    def run_mode(self, mode, binary=None, passing=True):
        binary = binary or self.binary
        result = subprocess.run([str(binary), mode], cwd=binary.parent,
                                capture_output=True, text=True, timeout=20)
        (binary.parent / (mode + '.log')).write_text(result.stdout + result.stderr)
        if passing:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            print('issue_session_evidence=' + result.stdout.strip())
        else:
            self.assertNotEqual(result.returncode, 0, 'private persistent API fault escaped')

    def test_independent_lanes_captured_values_and_actual_edge_context(self):
        self.run_mode('progress')

    def test_same_edge_STD_head_authorization_and_callback_rejection(self):
        self.run_mode('stores')

    def test_wrapped_redirect_survivors_cancellation_and_killed_acceptance(self):
        self.run_mode('redirect')

    def test_vector_retry_and_identity_selected_replay_metadata(self):
        self.run_mode('replay')

    def test_invalid_nested_and_unconsumed_session_rejection(self):
        self.run_mode('misuse')

    def test_private_faults_are_detected(self):
        mutations = [
            ('stale_feedback_cursor', 'progress',
             'context.feedback_cursor = feedback_.iq_slow_feedback_stats().sta_samples.size();',
             'context.feedback_cursor = 0;'),
            ('missing_STD_declaration', 'stores',
             'store_scoreboard_.mark_data_issued(transaction, transaction.data_lane, edge) &&', ''),
            ('missing_acceptance_authorization', 'stores',
             '    if (retained_issue_driver_ && !retained_issue_driver_->authorize_before_edge()) return;', ''),
            ('lost_replay_mask', 'replay', 'replay.replay_mask = request->replay_mask;', 'replay.replay_mask = 0;'),
            ('premature_session_end', 'misuse',
             'if (!ordinary_issue_driver_->empty() || ordinary_issue_driver_->events_pending() ||\n'
             '        !issue_acceptance_context_.empty()) {', 'if (!ordinary_issue_driver_->empty()) {'),
        ]
        for name, mode, old, new in mutations:
            with self.subTest(name=name):
                self.assertEqual(self.source.count(old), 1)
                self.run_mode(mode, self.compile(self.source.replace(old, new), name), passing=False)
                print('issue_session_fault=' + name)
        retained = (ROOT / 'cpp/environment/retained_issue.hpp').read_text()
        old = 'if (event.outcome != Outcome::accepted) continue;'
        self.assertEqual(retained.count(old), 1)
        wrong = retained.replace(old, 'if (event.outcome != Outcome::accepted || event.killed) continue;')
        self.run_mode('redirect', self.compile(self.source, 'missing_killed_callback', wrong), passing=False)
        print('issue_session_fault=missing_killed_callback')


if __name__ == '__main__':
    unittest.main()
