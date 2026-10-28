#!/usr/bin/env python3
"""Check production FP selectors, memory/forwarding oracles and result sensitivity."""
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT/'scripts'))
import verify_regression

class FpRandomTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.workspace=tempfile.TemporaryDirectory(prefix='memblock-fp-random-')
        cls.addClassCleanup(cls.workspace.cleanup)
        root=Path(cls.workspace.name)
        production=(ROOT/'cpp/scenarios/mixed_transactions.cpp').read_text()
        factory=production[production.index('memblock::LoadTransaction MixedScenario::make_load('):
                           production.index('memblock::StoreTransaction MixedScenario::make_store(')]
        source=root/'fixture.cpp'
        source.write_text((ROOT/'unit/fp_random_fixture.cpp').read_text().replace('// PRODUCTION_FACTORY',factory))
        cls.binary=root/'fixture'
        result=subprocess.run(['c++','-std=c++20','-O0','-I',str(ROOT/'cpp'),str(source),'-o',str(cls.binary)],
                              text=True,capture_output=True,timeout=60)
        if result.returncode:raise RuntimeError(result.stdout+result.stderr)

    def check_case(self,name):
        result=subprocess.run([str(self.binary),name],text=True,capture_output=True,timeout=15)
        self.assertEqual(result.returncode,0,result.stdout+result.stderr)
        return result.stdout

    def test_independent_literal_payloads_boxing_and_noncontiguous_pages(self): self.check_case('memory')
    def test_rates_width_controls_and_replay(self): self.check_case('policy')
    def test_actual_factory_flags_and_physical_destination_domains(self): self.check_case('factory')
    def test_partial_forwarding_refetch_versions_and_structural_variation(self):
        self.assertIn('unique_fp_dependency_topologies=',self.check_case('dependency'))
    def test_actual_scoreboard_rejects_payload_boxing_enable_and_identity_substitutions(self):
        self.check_case('scoreboard')

    def test_artifact_conservation_and_endpoint_controls(self):
        valid=dict(constraint_schema=59,target_scalar_fp=500,target_scalar_fp_widths='1,1,1',
                   scalar_format_choices='1,1,0,1,0,1,0,1',scalar_fp='1,2,2',
                   scalar_fp_sign='1,0,1,0,1,1',scalar_fp_origin='1,1,1,2',
                   scalar_fp_faults='0,1,0',scalar_fp_overlay='3,3',scalar_fp_misaligned=1,
                   load_ops='0,1,2,2,0,0,1',scalar='6,0')
        verify_regression._check_scalar_fp_accounting(valid)
        zero=dict(valid,target_scalar_fp=0,target_scalar_fp_widths='0,0,0',
                  scalar_format_choices='1,0,1,0,1,0,1,0',scalar_fp='0,0,0',
                  scalar_fp_sign='0,0,0,0,0,0',scalar_fp_origin='0,0,0,0',
                  scalar_fp_faults='0,0,0',scalar_fp_overlay='0,0',scalar_fp_misaligned=0)
        verify_regression._check_scalar_fp_accounting(zero)
        forced=dict(valid,target_scalar_fp=1000,scalar_format_choices='0,1,0,1,0,1,0,1')
        verify_regression._check_scalar_fp_accounting(forced)
        for field,value in [('target_scalar_fp',True),('target_scalar_fp',1001),
              ('target_scalar_fp_widths','0,0,0'),('target_scalar_fp_widths','0,1,1'),
              ('scalar_format_choices','1,2,0,1,0,1,0,1'),('scalar_fp','2,2,2'),
              ('scalar_fp_sign','1,0,2,0,1,1'),('scalar_fp_origin','1,1,1,3'),
              ('scalar_fp_faults','0,0,0'),('scalar_fp_overlay','25,0'),
              ('scalar_fp_misaligned',True),('scalar_fp_misaligned',6)]:
            with self.subTest(field=field,value=value),self.assertRaises(verify_regression.VerificationError):
                verify_regression._check_scalar_fp_accounting(dict(valid,**{field:value}))
        for field in valid:
            if field=='constraint_schema':continue
            partial=dict(valid);del partial[field]
            with self.subTest(missing=field),self.assertRaises(verify_regression.VerificationError):
                verify_regression._check_scalar_fp_accounting(partial)
        with self.assertRaises(verify_regression.VerificationError):
            verify_regression._check_scalar_fp_accounting(dict(valid,target_scalar_fp=1000))
        with self.assertRaises(verify_regression.VerificationError):
            verify_regression._check_scalar_fp_accounting(dict(valid,target_scalar_fp=0))

if __name__=='__main__':unittest.main()
