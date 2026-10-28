#!/usr/bin/env python3
"""Check CPU-compatible vector destination and repeated-Vd source construction."""
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT/'scripts'))
import verify_regression

def method(path,signature):
    source=(ROOT/path).read_text()
    start=source.index(signature)
    end=source.index("\n}\n",start)+3
    return source[start:end]

class VectorOperandsTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.workspace=tempfile.TemporaryDirectory(prefix='memblock-vector-operands-')
        cls.addClassCleanup(cls.workspace.cleanup)
        root=Path(cls.workspace.name)
        methods=[]
        for signature in ('unsigned int MixedScenario::vector_shape_uop_count(',
                          'unsigned int MixedScenario::vector_shape_flow_num(',
                          'bool MixedScenario::choose_binary_class(',
                          'std::vector<memblock::VectorMemoryTransaction> MixedScenario::make_random_vector_uops('):
            methods.append(method('cpp/scenarios/mixed_vectors.cpp',signature))
        methods.append(method('cpp/scenarios/mixed_transactions.cpp','memblock::VectorMemoryTransaction MixedScenario::make_vector('))
        source=(ROOT/'unit/vector_operand_fixture.cpp').read_text().replace('// PRODUCTION_BUILDERS','\n'.join(methods))
        source=source.replace('// PRODUCTION_REPLAY_MATCH',method('cpp/environment/feedback_monitor.cpp',
            'std::optional<VectorReplayRequest> FeedbackMonitor::take_replay_for('))
        source=source.replace('// PRODUCTION_WAIT',method('cpp/environment/completion.cpp',
            'auto Environment::Impl::run_until_vector_complete_with_replays(const VectorMemoryTransaction &transaction,').replace('Environment::Impl::','CompletionFixture::'))
        path=root/'fixture.cpp';path.write_text(source)
        cls.binary=root/'fixture'
        result=subprocess.run(['c++','-std=c++20','-O0','-I',str(ROOT/'cpp'),str(path),'-o',str(cls.binary)],
                              text=True,capture_output=True,timeout=60)
        if result.returncode:raise RuntimeError(result.stdout+result.stderr)

    def check_case(self,name):
        result=subprocess.run([str(self.binary),name],text=True,capture_output=True,timeout=20)
        self.assertEqual(result.returncode,0,result.stdout+result.stderr)
        return result.stdout

    def test_real_builders_use_distinct_class_appropriate_destinations(self):
        self.assertIn('vector_builder_cases=4992',self.check_case('domains'))
    def test_repeated_vd_sources_preserve_prior_data_and_backend_policy(self):self.check_case('sources')
    def test_exact_predecessor_wait_retains_other_obligations_and_replays(self):self.check_case('waits')
    def test_unordered_issue_varies_and_preserves_real_dependencies(self):
        self.assertIn('unique_dependency_issue_orders=',self.check_case('schedules'))
    def test_noop_preservation_rejects_all_ones_with_any_policy(self):
        self.assertIn('noop_scoreboard_cases=192',self.check_case('noops'))
    def test_chain_accounting_requires_every_predecessor_wait(self):
        check=verify_regression._check_vector_operand_accounting
        good={'constraint_schema':60,'vector':'16,8','vector_operand_chains':'12,6,12'}
        check(good);check(dict(good,vector='0,0',vector_operand_chains='0,0,0'))
        for value in ('12,6,11','17,6,17','12,9,12','-1,0,-1','12,6','True,6,12'):
            with self.subTest(value=value),self.assertRaises(verify_regression.VerificationError):
                check(dict(good,vector_operand_chains=value))
        with self.assertRaises(verify_regression.VerificationError):check({'constraint_schema':60,'vector':'1,1'})

if __name__=='__main__':unittest.main()
