#!/usr/bin/env python3
"""Independent accepted-request/D-message histories for production PTW manager."""
from pathlib import Path
import subprocess
import tempfile
import unittest
import sys
import random
ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT/'scripts'))
import verify_regression

class PtwResponseOrderTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tmp=tempfile.TemporaryDirectory(prefix='memblock-ptw-order-');cls.addClassCleanup(cls.tmp.cleanup)
        cls.fixture=(ROOT/'unit/ptw_response_order_fixture.cpp').read_text()
        cls.production=(ROOT/'cpp/environment/ptw_agent.inc').read_text()
        cls.binary=cls.compile('golden',cls.production)
    @classmethod
    def compile(cls,name,production):
        path=Path(cls.tmp.name)/name;path.mkdir()
        inc=path/'agent.inc';inc.write_text(production)
        source=path/'fixture.cpp';source.write_text(cls.fixture.replace('"environment/ptw_agent.inc"','"'+str(inc)+'"'))
        binary=path/'fixture';r=subprocess.run(['c++','-std=c++20','-O0','-I',str(ROOT/'cpp'),str(source),'-o',str(binary)],capture_output=True,text=True,timeout=60)
        if r.returncode:raise AssertionError(r.stdout+r.stderr)
        return binary
    def run_case(self,mode,binary=None):
        return subprocess.run([str(binary or self.binary),mode],capture_output=True,text=True,timeout=30)
    def test_independent_messages_keep_payload_snapshot_and_stability_while_permuting(self):
        r=self.run_case('random');self.assertEqual(r.returncode,0,r.stdout+r.stderr)
        self.assertIn('schedules=127',r.stdout)
        self.assertEqual(self.run_case('random').stdout,r.stdout)
        print(r.stdout.strip())
    def test_disabled_direction_keeps_fifo_and_exactly_once_sources(self):
        r=self.run_case('fifo');self.assertEqual(r.returncode,0,r.stdout+r.stderr);self.assertIn('schedules=1 nonfront=0',r.stdout)
    def test_source_reuse_and_live_or_invalid_reconfiguration_reject(self):
        for mode in ['reuse','config','sameedge','errors']:
            r=self.run_case(mode);self.assertEqual(r.returncode,0,r.stdout+r.stderr)
    def test_private_order_payload_and_message_boundary_faults_reject(self):
        faults={
          'fifo-only':('const auto selected = draw() % starts.size();','const auto selected = 0U;'),
          'payload':('memory_.read_beat(base + beat * kBeatBytes, kBeatBytes)','memory_.read_beat(base + (beats - beat - 1U) * kBeatBytes, kBeatBytes)'),
          'interleave':('response_message_active_ = false;\n            }','response_message_active_ = false;\n            } else { response_message_active_ = false; }'),
          'stalled-switch':('if (!response_message_active_) {','if (!response_message_active_ || d_presenting_) {'),
        }
        for name,(before,after) in faults.items():
            with self.subTest(name=name):
                self.assertEqual(self.production.count(before),1)
                binary=self.compile(name,self.production.replace(before,after));r=self.run_case('random',binary)
                self.assertNotEqual(r.returncode,0,'private fault escaped: '+name)
                print('ptw_order_mutant='+name+' diagnostic='+r.stderr.strip())
    def test_parser_rate_endpoints_and_current_schema(self):
        for value in ['0','1','250','999','1000','1001','-1','1.5','1000x','']:
            r=subprocess.run([str(self.binary),'constraints','ptw-response-reorder='+value],capture_output=True,text=True,timeout=10)
            valid=value in ['0','1','250','999','1000']
            self.assertEqual(r.returncode==0,valid,r.stdout+r.stderr)
            if valid:
                self.assertIn('target_ptw_response_reorder='+value+' ',r.stdout)
                self.assertIn('constraint_schema=83',r.stdout)

    def test_independent_accepted_and_completion_histories_reduce_to_valid_ledgers(self):
        for seed in range(128):
            for rate in [0,250,1000]:
                rng=random.Random(seed);pending=[];next_order=0;count=seed%65;row=[0]*8
                while next_order<count or pending:
                    for _ in range(min(count-next_order,16-len(pending),rng.randrange(1,6))):
                        pending.append(next_order);next_order+=1
                    if not pending:continue
                    row[0]+=1;row[5]=max(row[5],len(pending));row[1]+=len(pending)>1
                    choose=len(pending)>1 and rng.randrange(1000)<rate
                    row[2]+=choose;index=rng.randrange(len(pending)) if choose else 0
                    row[3]+=index!=0;owner=pending.pop(index)
                    older=sum(order<owner for order in pending)
                    row[6]+=older!=0;row[7]=max(row[7],older);row[4]+=1
                verify_regression._check_ptw_response_order(dict(constraint_schema=81,
                    target_ptw_response_reorder=rate,ptw_requests=count,ptw_response_order=','.join(map(str,row))))
        for impossible in ['2,0,0,0,2,2,0,0','2,2,2,0,2,2,0,0','8,1,1,0,8,7,0,0']:
            with self.assertRaises(verify_regression.VerificationError):
                verify_regression._check_ptw_response_order(dict(constraint_schema=81,
                    target_ptw_response_reorder=1000,ptw_requests=int(impossible.split(',')[0]),ptw_response_order=impossible))

    def test_sparse_and_corrupted_order_ledgers(self):
        for rate in [0,1,250,1000]:
            for values in [[0,0,0,0,0,0,0,0],[1,0,0,0,1,1,0,0],[8,6,0,0,8,7,0,0] if rate==0 else [8,6,6,4,8,7,4,3]]:
                record=dict(constraint_schema=81,target_ptw_response_reorder=rate,ptw_response_order=','.join(map(str,values)),ptw_requests=values[4])
                verify_regression._check_ptw_response_order(record)
        base=dict(constraint_schema=81,target_ptw_response_reorder=1000,ptw_response_order='8,6,6,4,8,7,4,3',ptw_requests=8)
        for i in range(8):
            v=[8,6,6,4,8,7,4,3];v[i]+=100
            with self.assertRaises(verify_regression.VerificationError):verify_regression._check_ptw_response_order(dict(base,ptw_response_order=','.join(map(str,v))))
        for key in ['target_ptw_response_reorder','ptw_response_order','ptw_requests']:
            r=dict(base);del r[key]
            with self.assertRaises(verify_regression.VerificationError):verify_regression._check_ptw_response_order(r)
        for value in ['', '1,2','-1,0,0,0,0,0,0,0','18446744073709551616,0,0,0,0,0,0,0']:
            with self.assertRaises(verify_regression.VerificationError):
                verify_regression._check_ptw_response_order(dict(base,ptw_response_order=value))
        for rate in [-1,1001,True,None]:
            with self.assertRaises(verify_regression.VerificationError):
                verify_regression._check_ptw_response_order(dict(base,target_ptw_response_reorder=rate))
        verify_regression._check_ptw_response_order(dict(constraint_schema=80))

if __name__=='__main__':unittest.main()
