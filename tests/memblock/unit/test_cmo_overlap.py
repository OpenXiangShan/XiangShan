#!/usr/bin/env python3
"""Compile the actual CMO scenario with independent completion/cancellation histories."""
from pathlib import Path
import subprocess
import tempfile
import unittest
ROOT=Path(__file__).resolve().parents[1]

class CmoOverlapTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tmp=tempfile.TemporaryDirectory(prefix='memblock-cmo-overlap-')
        cls.addClassCleanup(cls.tmp.cleanup)
        text=(ROOT/'cpp/scenarios/mixed_maintenance.cpp').read_text()
        start=text.index('bool MixedScenario::issue_random_cmo(')
        cls.production=text[start:text.rindex('} // namespace')]
        cls.fixture=(ROOT/'unit/cmo_overlap_fixture.cpp').read_text()
        cls.binary=cls.compile('golden',cls.production)
    @classmethod
    def compile(cls,name,production):
        source=Path(cls.tmp.name)/(name+'.cpp');binary=source.with_suffix('')
        source.write_text(cls.fixture.replace('// PRODUCTION_CMO',production))
        r=subprocess.run(['c++','-std=c++20','-O0','-I',str(ROOT/'cpp'),str(source),'-o',str(binary)],text=True,capture_output=True,timeout=60)
        if r.returncode:raise AssertionError(r.stdout+r.stderr)
        return binary
    def run_case(self,binary=None,fault=0):
        return subprocess.run([str(binary or self.binary),str(fault)],text=True,capture_output=True,timeout=20)
    def test_actual_scenario_checks_early_during_and_canceled_results(self):
        r=self.run_case();self.assertEqual(r.returncode,0,r.stdout+r.stderr)
        self.assertIn('cmo_overlap_histories=576',r.stdout);print(r.stdout.strip())
    def test_production_scoreboard_rejects_wrong_duplicate_and_late_results(self):
        for fault in [1,2,3]:
            with self.subTest(fault=fault):
                r=self.run_case(fault=fault);self.assertNotEqual(r.returncode,0)
                self.assertIn('load writeback',r.stderr)
    def test_private_missing_expectation_cancel_rewind_and_fifo_assumption_reject(self):
        faults={
            'unregistered':('        environment.expect_load(*younger);','        /* private missing expectation */'),
            'uncanceled':('!environment.cancel_pending_for_redirect(cmo_transaction.rob,\n                cmo_transaction.rob_flag, error_denied.has_value())','false'),
            'rewind_completed':('lq_offset -= environment.lq_redirect_canceled_observed() - younger_cancels_before;','--lq_offset;'),
            'fixed_order':('            environment.pending_scalar_stores() != 1) {','            environment.pending_scalar_stores() != 1 || environment.writebacks() != younger_writebacks_before) {'),
        }
        for name,(before,after) in faults.items():
            with self.subTest(name=name):
                self.assertEqual(self.production.count(before),1)
                binary=self.compile(name,self.production.replace(before,after));r=self.run_case(binary)
                self.assertNotEqual(r.returncode,0,'escaped '+name)
                print('cmo_overlap_mutant='+name+' diagnostic='+r.stderr.strip())

if __name__=='__main__':unittest.main()
