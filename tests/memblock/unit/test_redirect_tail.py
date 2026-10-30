#!/usr/bin/env python3
"""Exercise actual environment redirect phases and strict scoreboards on host.

Scripted terminal times check the oracle contract and its sensitivity, not
native pipeline reachability. No universal MMIO/NC/AMO/segment tail is inferred.
"""
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


def method(path, signature):
    text = (ROOT / path).read_text()
    begin = text.index(signature)
    return text[begin:text.index('\n}\n', begin) + 3]


class RedirectTailTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.directory = tempfile.TemporaryDirectory(prefix='memblock-redirect-tail-')
        cls.addClassCleanup(cls.directory.cleanup)
        source = (ROOT / 'unit/redirect_tail_fixture.cpp').read_text()
        shared = (ROOT / 'unit/mixed_redirect_cancellation_fixture.cpp').read_text()
        source = source.replace('// OBLIGATION_FIXTURE', shared[:shared.index('static Fixture mixed_fixture(')])
        header = (ROOT / 'cpp/environment/implementation.hpp').read_text()
        types = []
        for name in ('ReleaseInputGroup', 'ReleaseStoreInputs', 'ScalarRedirectBoundary'):
            begin = header.index('    struct ' + name + ' {')
            types.append(header[begin:header.index('\n    };', begin) + 7])
        source = source.replace('// PRODUCTION_TYPES', '\n'.join(types))
        methods = [method('cpp/environment/observations.cpp', 'auto Environment::Impl::cancel_pending_for_redirect(')]
        methods += [method('cpp/environment/completion.cpp', 'auto Environment::Impl::redirect_after('),
                    method('cpp/environment/cycle.cpp', 'auto Environment::Impl::tick(')]
        cls.source = source.replace('// PRODUCTION_METHODS', '\n'.join(methods).replace('Environment::Impl::', 'Harness::'))
        stream = (ROOT / 'cpp/scenarios/mixed_stream.cpp').read_text()
        begin = stream.index('            if (!return_ordinary_redirect(')
        cls.ledger = stream[begin:stream.index('            canceled += killed;', begin)]
        cls.source = cls.source.replace('// PRODUCTION_RETURN_AND_CANCELLATION_LEDGER', cls.ledger)
        cls.binary = cls.compile(cls.source, 'baseline')

    @classmethod
    def compile(cls, source, name):
        path = Path(cls.directory.name) / (name + '.cpp')
        path.write_text(source)
        binary = path.with_suffix('')
        result = subprocess.run(['c++', '-std=c++20', '-I', str(ROOT / 'cpp'), str(path), '-o', str(binary)],
                                capture_output=True, text=True, timeout=60)
        if result.returncode:
            raise AssertionError(result.stdout + result.stderr)
        return binary

    def run_mode(self, mode, binary=None, passing=True):
        result = subprocess.run([str(binary or self.binary), mode], capture_output=True, text=True, timeout=20)
        if passing:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            print('redirect_tail_evidence=' + result.stdout.strip())
        else:
            self.assertNotEqual(result.returncode, 0, 'private redirect fault escaped')

    def test_exact_terminals_through_every_monitored_return_edge(self):
        self.run_mode('tails')

    def test_boundary_identity_freshness_and_exactly_once_consumption(self):
        self.run_mode('boundaries')

    def test_wrong_duplicate_and_post_cancellation_terminals_reject(self):
        self.run_mode('corrupt')

    def test_unauthorized_inputs_cancel_separately_and_committed_survive(self):
        self.run_mode('inputs')

    def test_final_canceled_STA_state_includes_every_return_feedback_edge(self):
        self.run_mode('feedback')

    def test_private_early_canceled_STA_accounting_fault_rejects(self):
        boundary = self.ledger.index('            ++coverage.stream_cancel[0];')
        early = self.ledger[boundary:] + self.ledger[:boundary]
        self.run_mode('feedback', self.compile(self.source.replace(self.ledger, early), 'early_feedback'), passing=False)
        print('redirect_tail_fault=early_feedback_accounting')

    def test_private_early_erasure_and_skipped_monitoring_faults_reject(self):
        anchor = '    dut_.queues.redirect.valid.set(std::uint64_t{1});'
        self.assertEqual(self.source.count(anchor), 1)
        early = ('    scoreboard_.cancel_pending_for_redirect(rob_value, rob_flag, flush_itself);\n'
                 '    store_scoreboard_.cancel_pending_for_redirect(rob_value, rob_flag, flush_itself);\n'
                 '    vector_scoreboard_.cancel_pending_for_redirect(rob_value, rob_flag, flush_itself);\n')
        faults = {'early_erasure': self.source.replace(anchor, early + anchor),
                  'unmonitored_tail': self.source.replace('    tick();', '    tick(false);'),
                  'wrong_suffix': self.source.replace('scoreboard_.cancel_pending_for_redirect(rob_value, rob_flag, flush_itself)',
                                                      'scoreboard_.cancel_pending_for_redirect(rob_value, rob_flag, !flush_itself)')}
        for name, source in faults.items():
            with self.subTest(name=name):
                self.run_mode('tails', self.compile(source, name), passing=False)
                print('redirect_tail_fault=' + name)

    def test_private_recovery_phase_reordering_rejects(self):
        header = (ROOT / 'cpp/scenarios/mixed/stream_recovery.hpp').read_text()
        original = ('return environment.redirect_after(rob, flag, flush_itself) &&\n'
                    '        environment.cancel_pending_for_redirect(rob, flag, flush_itself);')
        self.assertEqual(header.count(original), 1)
        faults = {
            'cancellation_before_return': ('return environment.cancel_pending_for_redirect(rob, flag, flush_itself) &&\n'
                                        '        environment.redirect_after(rob, flag, flush_itself);'),
        }
        for name, replacement in faults.items():
            with self.subTest(name=name):
                path = Path(self.directory.name) / (name + '.hpp')
                path.write_text(header.replace(original, replacement).replace(
                    '#include "unresolved_epochs.hpp"', '#include "scenarios/mixed/unresolved_epochs.hpp"'))
                source = self.source.replace('#include "scenarios/mixed/stream_recovery.hpp"', '#include "' + str(path) + '"')
                self.run_mode('tails', self.compile(source, name), passing=False)
                print('redirect_tail_fault=' + name)


if __name__ == '__main__':
    unittest.main()
