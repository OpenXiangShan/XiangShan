#!/usr/bin/env python3
"""Check actual translation helper pulses against independent CPU-input histories."""
from pathlib import Path
import hashlib
import json
import os
import re
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
METHODS = {
    'translation_mapping.cpp': ('configure_sv39', 'activate_stage_one'),
    'translation_context.cpp': ('activate_two_stage_modes', 'update_stage_one_context',
                                'update_vs_context', 'update_two_stage_context',
                                'update_g_context', 'set_hypervisor_access_permissions'),
}


def extract_method(path, name):
    text = path.read_text()
    start = text.index('auto Environment::Impl::' + name + '(')
    return text[start:text.index('\n}\n', start) + 3].replace('Environment::Impl::', 'Harness::')


class TranslationContextPulseTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.directory = Path(tempfile.mkdtemp(prefix='memblock-translation-context-pulses-'))
        if os.environ.get('MEMBLOCK_TRANSLATION_PULSE_KEEP_ARTIFACTS'):
            print('translation_context_pulse_artifacts=' + str(cls.directory))
        else:
            cls.addClassCleanup(shutil.rmtree, cls.directory)
        cls.methods = {name: extract_method(ROOT / 'cpp/environment' / file, name)
                       for file, names in METHODS.items() for name in names}
        cls.fixture = (ROOT / 'unit/translation_context_pulses_fixture.cpp').read_text()
        cls.source = cls.fixture.replace('// PRODUCTION_METHODS', '\n'.join(cls.methods.values()))
        paths = [ROOT / 'cpp/environment' / file for file in METHODS]
        paths += [ROOT / 'unit/translation_context_pulses_fixture.cpp', Path(__file__).resolve()]
        manifest = {'files_sha256': {str(path): hashlib.sha256(path.read_bytes()).hexdigest() for path in paths},
                    'methods_sha256': {name: hashlib.sha256(text.encode()).hexdigest()
                                       for name, text in cls.methods.items()}}
        (cls.directory / 'source-manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')
        cls.binary = cls.compile(cls.source, 'baseline')

    @classmethod
    def compile(cls, source, name):
        directory = cls.directory / name
        directory.mkdir()
        path = directory / 'fixture.cpp'
        path.write_text(source)
        binary = directory / 'fixture'
        result = subprocess.run(['c++', '-std=c++20', '-O0', '-g', '-I', str(ROOT / 'cpp'),
                                 str(path), '-o', str(binary)], capture_output=True, text=True, timeout=60)
        (directory / 'compile.log').write_text(result.stdout + result.stderr)
        if result.returncode:
            raise AssertionError(result.stdout + result.stderr)
        return binary

    def run_helper(self, helper, binary=None, passing=True):
        binary = binary or self.binary
        result = subprocess.run([str(binary), helper], cwd=binary.parent,
                                capture_output=True, text=True, timeout=20)
        (binary.parent / (helper + '.log')).write_text(result.stdout + result.stderr)
        if passing:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            print('translation_context_pulse_evidence=' + result.stdout.strip())
        else:
            self.assertNotEqual(result.returncode, 0, 'private translation pulse fault escaped')
            self.assertIn('translation-context-pulse-check', result.stdout + result.stderr,
                          'private fault failed outside the designated checker')

    def test_configuration_compares_ASID_and_dvirt_without_root_or_mode_flush(self):
        self.run_helper('configure_sv39')

    def test_stage_one_activation_compares_ASID_and_dvirt(self):
        self.run_helper('activate_stage_one')

    def test_nested_activation_compares_both_identities_and_dvirt_independently(self):
        self.run_helper('activate_two_stage_modes')

    def test_stage_one_update_compares_only_its_ASID(self):
        self.run_helper('update_stage_one_context')

    def test_VS_update_compares_only_its_ASID(self):
        self.run_helper('update_vs_context')

    def test_nested_update_compares_ASID_and_VMID_independently(self):
        self.run_helper('update_two_stage_context')

    def test_G_update_compares_only_its_VMID(self):
        self.run_helper('update_g_context')

    def test_hypervisor_permissions_change_dvirt_only_when_leaving_virtualization(self):
        self.run_helper('set_hypervisor_access_permissions')

    def mutated(self, helper, change):
        before = self.methods[helper]
        after = change(before)
        self.assertNotEqual(before, after)
        self.assertEqual(self.source.count(before), 1)
        return self.source.replace(before, after)

    def set_changed(self, helper, pin, index, value):
        def change(method):
            pattern = r'dut_\.translation\.csr\.' + re.escape(pin) + r'\.set\([^;]+\);'
            occurrences = list(re.finditer(pattern, method))
            self.assertEqual(len(occurrences), 2, (helper, pin))
            match = occurrences[index]
            replacement = 'dut_.translation.csr.' + pin + '.set(std::uint64_t{' + str(value) + '});'
            return method[:match.start()] + replacement + method[match.end():]
        return self.mutated(helper, change)

    def check_fault(self, name, helper, source):
        self.run_helper(helper, self.compile(source, name), passing=False)
        print('translation_context_pulse_fault=' + name)

    def test_private_always_high_and_uncleared_pulses_reject_for_every_helper(self):
        pins = {
            'configure_sv39': 'satp_changed', 'activate_stage_one': 'satp_changed',
            'activate_two_stage_modes': 'vsatp_changed', 'update_stage_one_context': 'satp_changed',
            'update_vs_context': 'vsatp_changed', 'update_two_stage_context': 'hgatp_changed',
            'update_g_context': 'hgatp_changed', 'set_hypervisor_access_permissions': 'priv_virt_changed',
        }
        for helper, pin in pins.items():
            for index, category in ((0, 'always_high'), (1, 'no_clear')):
                with self.subTest(helper=helper, category=category):
                    self.check_fault(category + '_' + helper, helper, self.set_changed(helper, pin, index, 1))

    def test_private_missed_ID_and_wrong_identity_register_reject(self):
        for helper, pin, identity, sibling in (
                ('update_stage_one_context', 'satp_changed', 'satp_asid', 'vsatp_asid'),
                ('update_vs_context', 'vsatp_changed', 'vsatp_asid', 'satp_asid'),
                ('update_g_context', 'hgatp_changed', 'hgatp_vmid', 'vsatp_asid')):
            with self.subTest(helper=helper, category='missed_ID'):
                self.check_fault('missed_ID_' + helper, helper, self.set_changed(helper, pin, 0, 0))
            with self.subTest(helper=helper, category='wrong_identity'):
                def change(method):
                    old = 'csr.' + identity + '.value()'
                    self.assertEqual(method.count(old), 1)
                    return method.replace(old, 'csr.' + sibling + '.value()')
                self.check_fault('wrong_identity_' + helper, helper, self.mutated(helper, change))

    def test_private_nested_identity_pulses_cannot_be_coupled(self):
        for helper in ('activate_two_stage_modes', 'update_two_stage_context'):
            for pin, sibling_decision in (('vsatp_changed', 'vmid_changed'),
                                          ('hgatp_changed', 'vs_asid_changed')):
                with self.subTest(helper=helper, pin=pin):
                    self.check_fault('coupled_' + pin + '_' + helper, helper,
                                     self.set_changed(helper, pin, 0, sibling_decision))

    def test_private_missed_dvirt_transitions_reject_for_every_writer(self):
        for helper in ('configure_sv39', 'activate_stage_one', 'activate_two_stage_modes',
                       'set_hypervisor_access_permissions'):
            with self.subTest(helper=helper):
                self.check_fault('missed_dvirt_' + helper, helper,
                                 self.set_changed(helper, 'priv_virt_changed', 0, 0))


if __name__ == '__main__':
    unittest.main()
