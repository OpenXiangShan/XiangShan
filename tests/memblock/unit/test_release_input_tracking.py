#!/usr/bin/env python3
"""Host behavior checks of production ordinary-store input tracking.

Compile the actual tracking/reference/retirement, issue, tick and reset methods.
Ports, unrelated agents, target lookup and manager decisions are synthetic. The
manager only records calls; literal bytes, public identities and edge schedules
are independent expectations. These tests establish no RTL or DUT coverage.
"""
import hashlib
import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
METHODS = {
    'completion.cpp': (
        'auto Environment::Impl::track_store_release_inputs(',
        'auto Environment::Impl::track_vector_release_inputs(',
        'auto Environment::Impl::accept_release_store_input(',
        'auto Environment::Impl::authorize_release_store_inputs(',
        'auto Environment::Impl::retire_store(',
        'auto Environment::Impl::retire_vector_stores(',
        'auto Environment::Impl::record_committed_store(',
        'auto Environment::Impl::record_committed_vector_store(',
        'auto Environment::Impl::commit_stores_through(',
    ),
    'issue.cpp': (
        'auto Environment::Impl::issue_store_address(',
        'auto Environment::Impl::issue_store_data(',
        'auto Environment::Impl::issue_store_operands(',
        'auto Environment::Impl::issue_vector(',
    ),
    'cycle.cpp': (
        'auto Environment::Impl::tick(',
        'auto Environment::Impl::scalar_store_key(',
        'auto Environment::Impl::vector_store_key(',
    ),
    'reset.cpp': ('auto Environment::Impl::reset(',),
}
DEPENDENCIES = (
    'cpp/memblock_env.hpp', 'cpp/environment/model.inc',
    'cpp/environment/dcache_a_contract.inc', 'cpp/generated_interface_types.hpp',
    'cpp/failure.hpp', 'cpp/environment/implementation.hpp', 'cpp/environment/retained_issue.hpp',
)
CASES = (
    'sta_first', 'std_first', 'pressure_replay', 'head_flags_wrap',
    'scalar_grouping', 'scalar_pages', 'vector_mask_replay',
    'vector_addresses', 'vector_misaligned_inactive',
    'retirement_keeps_groups', 'reset_legacy_rejection', 'manager_failures',
    'identity_reuse_absolute_order', 'vector_invalid_tracking',
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
    source = (ROOT / 'unit/release_input_tracking_fixture.cpp').read_text()
    header = (ROOT / 'cpp/environment/implementation.hpp').read_text()
    start = header.index('    struct ReleaseInputGroup {')
    end = header.index('    bool accept_release_store_input(', start)
    source = source.replace('// PRODUCTION_TRACKING_STRUCTS', header[start:end])
    methods = [method(path, signature).replace(
        'Environment::Impl::', 'ReleaseInputFixture::')
        for path, signatures in METHODS.items() for signature in signatures]
    return source.replace('// PRODUCTION_METHODS', '\n'.join(methods))


def replace_once(source, before, after):
    if source.count(before) != 1:
        raise AssertionError('private fault anchor is no longer unique: ' + before)
    return source.replace(before, after, 1)


class ReleaseInputTrackingTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.hashes = production_hashes()
        parent = os.environ.get('MEMBLOCK_RELEASE_INPUT_TEST_OUT')
        if parent:
            Path(parent).mkdir(parents=True, exist_ok=True)
            cls.workspace = Path(tempfile.mkdtemp(prefix='host-', dir=parent))
        else:
            temporary = tempfile.TemporaryDirectory(prefix='memblock-release-input-')
            cls.addClassCleanup(temporary.cleanup)
            cls.workspace = Path(temporary.name)
        cls.source = source_text()
        cls.commands, cls.results = [], []
        cls.binary = cls.compile_source('production', cls.source)
        cls.assert_unchanged()

    @classmethod
    def assert_unchanged(cls):
        if production_hashes() != cls.hashes:
            raise AssertionError('production input-tracking snapshot changed during host tests')

    @classmethod
    def compile_source(cls, name, source):
        path, binary = cls.workspace / (name + '.cpp'), cls.workspace / name
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
        after = production_hashes()
        (cls.workspace / 'summary.json').write_text(json.dumps({
            'production_sha256': cls.hashes,
            'production_unchanged': after == cls.hashes,
            'compile_commands': cls.commands, 'results': cls.results,
            'scope': 'host-only; actual methods and callbacks; synthetic ports, target lookup, recording manager',
        }, indent=2) + '\n')
        cls.assert_unchanged()

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

    def test_private_fault_sensitivity(self):
        faults = (
            ('register_one_operand',
             'if (inputs.address && inputs.data && !inputs.registered)',
             'if ((inputs.address || inputs.data) && !inputs.registered)',
             'sta_first', 'registered before both accepted inputs'),
            ('wrong_rob_flag',
             'dut_.queues.backend.pending_ptr_flag.value() != inputs.flag) continue;',
             'false) continue;',
             'head_flags_wrap', 'ROB flag mismatch authorized input'),
            ('gate_pendingst',
             'if (!inputs.registered || inputs.authorized ||',
             'if (!dut_.queues.backend.pendingst.boolean() || !inputs.registered || inputs.authorized ||',
             'sta_first', 'authorization missed accepted input edge'),
            ('authorize_after_edge',
             '    driver.offer_store_data(0, transaction.data_lane, issue, [&](std::uint64_t edge) {\n        return store_scoreboard_.mark_data_issued(transaction, transaction.data_lane, edge) &&\n            accept_release_store_input(scalar_store_key(transaction), false, false) &&\n            authorize_release_store_inputs();',
             '    driver.offer_store_data(0, transaction.data_lane, issue, [&](std::uint64_t edge) {\n        return store_scoreboard_.mark_data_issued(transaction, transaction.data_lane, edge) &&\n            accept_release_store_input(scalar_store_key(transaction), false, false);',
             'sta_first', 'authorization missed accepted input edge'),
            ('register_after_edge',
             '    driver.offer_store_data(0, transaction.data_lane, issue, [&](std::uint64_t edge) {\n        return store_scoreboard_.mark_data_issued(transaction, transaction.data_lane, edge) &&\n            accept_release_store_input(scalar_store_key(transaction), false, false) &&',
             '    driver.offer_store_data(0, transaction.data_lane, issue, [&](std::uint64_t edge) {\n        return store_scoreboard_.mark_data_issued(transaction, transaction.data_lane, edge) &&',
             'sta_first', 'registration missed second accepted input edge'),
            ('vector_callback_missing',
             '        if (ready && transaction.store &&\n            !accept_release_store_input(vector_store_key(transaction), true, true)) return false;',
             '        /* fault: omit vector acceptance callback */',
             'vector_mask_replay', 'vector registration missed accepted input edge'),
            ('register_replay_twice',
             'if (inputs.address && inputs.data && !inputs.registered)',
             'if (inputs.address && inputs.data)',
             'pressure_replay', 'duplicate input registration'),
            ('physical_sq_order',
             'ReleaseInputGroup group{*target * 1024U + offset,',
             'ReleaseInputGroup group{transaction.sq * 1024U + offset,',
             'sta_first', 'literal group order missing'),
            ('split_aligned_scalar',
             'const bool aligned = (transaction.address & (width - 1U)) == 0;',
             'const bool aligned = false;',
             'scalar_grouping', 'aligned scalar was split into byte groups'),
            ('retire_tracker_too_early',
             '    record_committed_store(transaction);\n    scalar_release_inputs_.erase(scalar_store_key(transaction));',
             '    scalar_release_inputs_.erase(scalar_store_key(transaction));\n    record_committed_store(transaction);',
             'retirement_keeps_groups', 'tracked scalar retirement duplicated legacy notification'),
            ('registration_provenance_missing',
             '                error_.merge(memory_agent_.failures());\n                return false;',
             '                /* fault: drop manager provenance */\n                return false;',
             'manager_failures', 'manager registration failure lost error provenance'),
            ('authorization_provenance_missing',
             '                    error_.merge(memory_agent_.failures());\n                    return false;',
             '                    /* fault: drop authorization provenance */\n                    return false;',
             'manager_failures', 'manager authorization rejection did not stop issue'),
            ('reset_keeps_inputs',
             '    scalar_release_inputs_.clear();\n    vector_release_inputs_.clear();',
             '    /* fault: retain reset input records */',
             'reset_legacy_rejection', 'reset retained transient input trackers'),
        )
        for name, before, after, case, decisive in faults:
            with self.subTest(fault=name):
                source = replace_once(self.source, before, after)
                binary = self.compile_source(name, source)
                result = self.run_case(binary, case)
                self.assertNotEqual(result.returncode, 0, 'private fault escaped: ' + name)
                self.assertIn(decisive, result.stderr, result.stdout + result.stderr)


if __name__ == '__main__':
    unittest.main()
