#!/usr/bin/env python3
"""Compile real store helpers, tick and scoreboard with independent edge schedules."""
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


def method(path, signature):
    text = (ROOT / path).read_text()
    begin = text.index(signature)
    return text[begin:text.index('\n}\n', begin) + 3]


class StoreOperandPairTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.directory = tempfile.TemporaryDirectory(prefix='memblock-store-pair-')
        cls.source = (ROOT / 'unit/store_operand_pair_fixture.cpp').read_text()
        methods = [method('cpp/environment/issue.cpp', 'auto Environment::Impl::' + name + '(')
                   for name in ['issue_store_address', 'issue_store_data', 'issue_store_operands']]
        methods.append(method('cpp/environment/cycle.cpp', 'auto Environment::Impl::tick('))
        cls.source = cls.source.replace('// PRODUCTION_METHODS', '\n'.join(methods).replace('Environment::Impl::', 'Harness::'))
        cls.binary = cls.compile(cls.source, 'baseline')

    @classmethod
    def tearDownClass(cls):
        cls.directory.cleanup()

    @classmethod
    def compile(cls, source, name):
        path = Path(cls.directory.name) / (name + '.cpp')
        path.write_text(source)
        binary = path.with_suffix('')
        compiled = subprocess.run(['c++', '-std=c++20', '-I', str(ROOT / 'cpp'), str(path), '-o', str(binary)],
                                  capture_output=True, text=True, timeout=60)
        if compiled.returncode:
            raise AssertionError(compiled.stdout + compiled.stderr)
        return binary

    def run_mode(self, mode, binary=None, passing=True):
        result = subprocess.run([str(binary or self.binary), mode], capture_output=True, text=True, timeout=20)
        if passing:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            print('store_operand_pair_evidence=' + result.stdout.strip())
        else:
            self.assertNotEqual(result.returncode, 0, 'private authorization fault escaped')

    def test_independent_handshakes_and_same_edge_data_completion(self):
        self.run_mode('pairs')

    def test_rejected_authorization_partial_timeout_and_lane_legality(self):
        self.run_mode('failures')

    def test_private_missing_or_late_acceptance_authorization_rejects(self):
        anchor = '    if (retained_issue_driver_ && !retained_issue_driver_->authorize_before_edge()) return;'
        self.assertEqual(self.source.count(anchor), 1)
        for name, before in [('missing', ''), ('late', '    if (retained_issue_driver_) retained_issue_driver_->authorize_before_edge();')]:
            with self.subTest(name=name):
                source = self.source.replace(anchor, '')
                if before:
                    source = source.replace('    dut_.step();', before + '\n    dut_.step();')
                binary = self.compile(source, name)
                self.run_mode('pairs', binary, passing=False)


if __name__ == '__main__':
    unittest.main()
