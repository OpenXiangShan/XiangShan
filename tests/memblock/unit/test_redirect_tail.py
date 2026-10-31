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
                    method('cpp/environment/completion.cpp', 'auto Environment::Impl::return_load_exception_after_nonmemory_wb('),
                    method('cpp/environment/cycle.cpp', 'auto Environment::Impl::tick(')]
        cls.source = source.replace('// PRODUCTION_METHODS', '\n'.join(methods).replace('Environment::Impl::', 'Harness::'))
        stream = (ROOT / 'cpp/scenarios/mixed_stream.cpp').read_text()
        begin = stream.index('            StreamCanceledGeneration record;')
        end = stream.index('        canceled += killed;', begin)
        cls.ledger = ('if (!return_ordinary_redirect(environment, rob, flag, flush_itself, source_hold)) return false;\n'
                      'if (!consume_issue_events()) return false;\n'
                      '            ++coverage.stream_cancel[0];\n'
                      'for (auto &item : live) { if (!kills(item)) continue;\n' + stream[begin:end])
        hold_begin = stream.index('        // A flush-self victim')
        hold_end = stream.index('        lq_offset -= lq_canceled;', hold_begin)
        cls.source = cls.source.replace('// PRODUCTION_SURVIVING_OPERAND_HOLD', stream[hold_begin:hold_end])
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

    def test_store_head_survives_ordinary_walk_with_registered_pending_suppression(self):
        self.run_mode('ordinary-walk')

    def test_private_ordinary_walk_pending_and_pointer_faults_reject(self):
        block = ('    if (ordinary_walk) {\n'
                 '        dut_.queues.backend.pendingst.set(std::uint64_t{0});\n'
                 '        dut_.queues.backend.pending_mmi_old.set(std::uint64_t{0});\n'
                 '        dut_.queues.backend.scommit.set(std::uint64_t{0});\n'
                 '    }\n')
        self.assertEqual(self.source.count(block), 1)
        faults = {
            'missing_pending_suppression': self.source.replace(block, ''),
            'early_pending_suppression': self.source.replace(block, '').replace('    tick(true, [&] {', block + '    tick(true, [&] {'),
            'late_pending_suppression': self.source.replace(block, '').replace('    tick();\n    // CSR consumes', '    tick();\n' + block + '    // CSR consumes'),
            'victim_as_head': self.source.replace(block, block + '    if (ordinary_walk) dut_.queues.backend.pending_ptr_value.set(rob_value);\n'),
            'commit_during_walk': self.source.replace(block, block + '    if (ordinary_walk) dut_.queues.backend.scommit.set(1);\n'),
        }
        for name, source in faults.items():
            with self.subTest(name=name):
                self.run_mode('ordinary-walk', self.compile(source, name), passing=False)

    def test_private_premature_backend_release_rejects(self):
        header = (ROOT / 'cpp/scenarios/mixed/stream_recovery.hpp').read_text()
        anchor = 'environment.run_cycles(memblock::kStreamOrdinaryRecoveryBusy + 1U)'
        self.assertEqual(header.count(anchor), 1)
        for name, replacement in [('short_walk', 'environment.run_cycles(6)'),
                                  ('missing_registered_edge', 'environment.run_cycles(memblock::kStreamOrdinaryRecoveryBusy)')]:
            path = Path(self.directory.name) / (name + '.hpp')
            path.write_text(header.replace(anchor, replacement).replace(
                '#include "unresolved_epochs.hpp"', '#include "scenarios/mixed/unresolved_epochs.hpp"'))
            source = self.source.replace('#include "scenarios/mixed/stream_recovery.hpp"', '#include "' + str(path) + '"')
            self.run_mode('ordinary-walk', self.compile(source, name), passing=False)

    def test_declared_anchor_exact_head_redirect_and_preedge_fault_capture(self):
        self.run_mode('exception-anchor')

    def test_private_anchor_timing_and_fault_capture_faults_reject(self):
        faults = {
            'observation_before_managers': self.source.replace('    if (before_edge) before_edge();', '').replace('    memory_agent_.drive(dut_);', '    if (before_edge) before_edge();\n    memory_agent_.drive(dut_);'),
            'early_anchor_redirect': self.source.replace('if (!run_cycles(4))', 'if (!run_cycles(3))'),
            'late_anchor_redirect': self.source.replace('if (!run_cycles(4))', 'if (!run_cycles(5))'),
            'late_fault_va': self.source.replace('*captured_vaddr = exception_vaddr();', 'tick(); *captured_vaddr = exception_vaddr();'),
            'wrong_public_head': self.source.replace('pending_ptr_value.set(fault.rob);', 'pending_ptr_value.set(anchor);'),
            'wrong_store_observation_edge': self.source.replace('*stores = {cycle(),', '*stores = {cycle() + 1U,'),
            'wrong_store_path_level': self.source.replace('!dut_.sample_sbuffer_empty()', 'dut_.sample_sbuffer_empty()'),
            'lost_committed_SQ': self.source.replace('retirement.pending, !dut_.sample_sbuffer_empty()',
                                                   '0, !dut_.sample_sbuffer_empty()'),
        }
        for name, source in faults.items():
            with self.subTest(name=name):
                self.run_mode('exception-anchor', self.compile(source, name), passing=False)

    def test_boundary_identity_freshness_and_exactly_once_consumption(self):
        self.run_mode('boundaries')

    def test_wrong_duplicate_and_post_cancellation_terminals_reject(self):
        self.run_mode('corrupt')

    def test_unauthorized_inputs_cancel_separately_and_committed_survive(self):
        self.run_mode('inputs')

    def test_final_canceled_STA_state_includes_every_return_feedback_edge(self):
        self.run_mode('feedback')

    def test_delayed_operand_cancellation_retains_surviving_anchor(self):
        self.run_mode('operand-cancellation')

    def test_private_skipped_surviving_operand_hold_rejects(self):
        original = 'if (!killed_anchor && environment.cycle() < first_killed->operand_anchor_release &&'
        self.assertEqual(self.source.count(original), 1)
        self.run_mode('operand-cancellation', self.compile(
            self.source.replace(original, 'if (false && environment.cycle() < first_killed->operand_anchor_release &&'),
            'skipped-operand-hold'), passing=False)

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
        original = ('return environment.redirect_after(rob, flag, flush_itself, false, ordinary_walk) &&\n'
                    '        environment.cancel_pending_for_redirect(rob, flag, flush_itself);')
        self.assertEqual(header.count(original), 1)
        faults = {
            'cancellation_before_return': ('return environment.cancel_pending_for_redirect(rob, flag, flush_itself) &&\n'
                                        '        environment.redirect_after(rob, flag, flush_itself, false, ordinary_walk);'),
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
