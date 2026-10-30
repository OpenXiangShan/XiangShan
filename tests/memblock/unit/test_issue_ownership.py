#!/usr/bin/env python3
"""Compile production issue helpers/tick; reject unowned or wrong-age traffic."""
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
METHODS = {
    'control.cpp': ('issue_load', 'issue_load_batch'),
    'issue.cpp': ('issue_prefetch', 'issue_prefetch_batch_same_cycle', 'issue_vector',
                  'issue_vector_batch_same_cycle', 'issue_store_address_batch', 'issue_atomic', 'issue_store_data'),
    'cycle.cpp': ('tick',),
    'manager_control.cpp': ('make_vector_memory_issue',),
    'completion.cpp': ('service_vector_replay',),
}


def method(file, name):
    text = (ROOT / 'cpp/environment' / file).read_text()
    begin = text.index('auto Environment::Impl::' + name + '(')
    return text[begin:text.index('\n}\n', begin) + 3]


class IssueOwnershipTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.directory = tempfile.TemporaryDirectory(prefix='memblock-issue-ownership-')
        cls.addClassCleanup(cls.directory.cleanup)
        pins = (ROOT / 'unit/retained_issue_fixture.cpp').read_text()
        pins = pins[:pins.index('struct Obligation {')].replace('Adapter', 'WitnessAdapter')
        source = (ROOT / 'unit/issue_ownership_fixture.cpp').read_text().replace('// PIN_WITNESSES', pins)
        methods = [method(file, name) for file, names in METHODS.items() for name in names]
        cls.source = source.replace('// PRODUCTION_METHODS', '\n'.join(methods).replace('Environment::Impl::', 'Harness::'))
        cls.binary = cls.compile(cls.source, 'baseline')

    @classmethod
    def compile(cls, source, name):
        path = Path(cls.directory.name) / (name + '.cpp'); path.write_text(source)
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
            print('issue_ownership_evidence=' + result.stdout.strip())
        else:
            self.assertNotEqual(result.returncode, 0, 'private issue-ownership fault escaped')

    def test_bound_manual_helpers_preserve_empty_and_occupied_owner(self):
        self.run_mode('ownership')

    def test_unbound_helpers_still_have_real_accepted_edge_controls(self):
        self.run_mode('controls')

    def test_STD_payload_and_same_edge_completion_keep_every_ROB_identity(self):
        self.run_mode('identity')

    def test_wrapped_STD_redirect_preserves_survivors_and_killed_tail(self):
        self.run_mode('redirect')

    def test_private_missing_owner_guards_reject(self):
        for name in ('issue_load', 'issue_load_batch', 'issue_prefetch', 'issue_prefetch_batch_same_cycle',
                     'issue_vector', 'issue_vector_batch_same_cycle', 'issue_store_address_batch', 'issue_atomic',
                     'service_vector_replay'):
            with self.subTest(name=name):
                guard = ('    if (retained_issue_driver_) {\n'
                         '        error_.record("legacy ' + name + ' conflicts with retained producer pin ownership");\n'
                         '        return false;\n    }\n')
                self.assertEqual(self.source.count(guard), 1)
                self.run_mode('ownership', self.compile(self.source.replace(guard, ''), name), passing=False)
                print('issue_ownership_fault=missing_' + name + '_guard')

    def test_private_missing_STD_wrap_rejects(self):
        original = method('issue.cpp', 'issue_store_data').replace('Environment::Impl::', 'Harness::')
        wrong = original.replace('    issue.rob_flag = transaction.rob_flag;\n', '')
        self.assertNotEqual(original, wrong)
        binary = self.compile(self.source.replace(original, wrong), 'missing_STD_wrap')
        self.run_mode('identity', binary, passing=False)
        self.run_mode('redirect', binary, passing=False)
        print('issue_ownership_fault=missing_STD_wrap')


if __name__ == '__main__':
    unittest.main()
