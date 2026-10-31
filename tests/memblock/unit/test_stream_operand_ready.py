#!/usr/bin/env python3
"""Independent source deadline and sparse lifecycle accounting contracts."""
from pathlib import Path
import importlib.util
import subprocess
import tempfile
import sys
import unittest

ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT/'scripts'))
spec=importlib.util.spec_from_file_location('verify_operand_ready',ROOT/'scripts/verify_regression.py')
verifier=importlib.util.module_from_spec(spec)
spec.loader.exec_module(verifier)

CPP=r'''
#include "scenarios/mixed/stream_operand_ready.hpp"
#include "scenarios/mixed/state.hpp"
#include <cassert>
#include <set>
using namespace memblock::scenarios::mixed;
int main(){
 for(unsigned maximum:{1U,2U,64U,4096U})for(unsigned rate:{0U,1U,250U,1000U}){
  std::mt19937_64 rng(8631),replay(8631);std::set<unsigned>delays;
  for(unsigned n=0;n<16384;++n){
   const auto before=rng;auto r=StreamOperandReady::choose(rng,100,rate,maximum);
   auto q=StreamOperandReady::choose(replay,100,rate,maximum);
   assert(r.cycle==100+r.delay&&r.delay<=maximum&&r.cycle==q.cycle&&r.delay==q.delay);
   if(!rate)assert(r.delay==0&&rng==before);
   if(rate==1000)assert(r.delay>=1);
   assert(!r.consumed&&r.ready(r.cycle)&&!r.waiting(r.cycle));
   if(r.delay){assert(!r.ready(r.cycle-1)&&r.waiting(r.cycle-1));delays.insert(r.delay);}
  }
  if(rate==1000){assert(delays.count(1)&&delays.count(maximum));if(maximum<=64)assert(delays.size()==maximum);}
 }
 for(auto name:{"spec","coverage","corner","cmo-stress"}){
  auto c=memblock::scenarios::RandomConstraints::preset(name);assert(c.stream_operand_delay_per_mille==250&&c.stream_operand_delay_max==64);
  for(auto rate:{0U,1U,999U,1000U})for(auto maximum:{1U,4096U}){
   c.apply("stream-operand-delay="+std::to_string(rate));c.apply("stream-operand-delay-max="+std::to_string(maximum));c.validate();
   assert(c.summary().find("target_stream_operand_delay="+std::to_string(rate)+" ")!=std::string::npos);
  }
  for(auto bad:{"stream-operand-delay=1001","stream-operand-delay-max=0","stream-operand-delay-max=4097"}){
   auto x=memblock::scenarios::RandomConstraints::preset(name);bool rejected=false;try{x.apply(bad);x.validate();}catch(const std::invalid_argument&){rejected=true;}assert(rejected);
  }
 }
}
'''

def csv(row):return ','.join(map(str,row))

class StreamOperandReadyTest(unittest.TestCase):
 def test_deadline_boundaries_replay_disable_and_constraint_endpoints(self):
  with tempfile.TemporaryDirectory(prefix='memblock-operand-ready-') as temp:
   p=Path(temp)/'fixture.cpp';p.write_text(CPP);binary=p.with_suffix('')
   r=subprocess.run(['c++','-std=c++20','-O2','-I',str(ROOT/'cpp'),str(p),'-o',str(binary)],capture_output=True,text=True,timeout=60)
   self.assertEqual(r.returncode,0,r.stdout+r.stderr)
   r=subprocess.run([str(binary)],capture_output=True,text=True,timeout=20);self.assertEqual(r.returncode,0,r.stdout+r.stderr)

 def record(self):
  return dict(constraint_schema=86,target_stream_operand_delay=1000,target_stream_operand_delay_max=64,
   stream_operand_ready=csv([2]*5+[1]*5+[1]*5+[2,1,1]+[1]*12),
   stream_ops=csv([1]*5),stream_cancel=csv([1]*29),stream_issue=csv([1]*6),
   stream_transport=csv([20,20]+[0]*8),stream=csv([0]*9+[2,0,0]))

 def test_lifecycle_corruptions_population_and_sparse_bounds(self):
  check=verifier._check_stream_operand_ready;r=self.record();check(r)
  for i in range(18):
   c=list(map(int,r['stream_operand_ready'].split(',')));c[i]+=1
   with self.subTest(counter=i),self.assertRaises(verifier.VerificationError):check(dict(r,stream_operand_ready=csv(c)))
  for i in range(18,30):
   c=list(map(int,r['stream_operand_ready'].split(',')));c[i]=30
   with self.subTest(overlap=i),self.assertRaises(verifier.VerificationError):check(dict(r,stream_operand_ready=csv(c)))
  for key in r:
   if key=='constraint_schema':continue
   bad=dict(r);del bad[key]
   with self.subTest(missing=key),self.assertRaises(verifier.VerificationError):check(bad)
  for key,values in [('target_stream_operand_delay',[True,-1,1001,'250']),('target_stream_operand_delay_max',[False,0,4097,'64'])]:
   for v in values:
    with self.subTest(key=key,value=v),self.assertRaises(verifier.VerificationError):check(dict(r,**{key:v}))
  for value in ['',csv([0]*29),csv([0]*31),csv([-1]+[0]*29),csv([1<<64]+[0]*29)]:
   with self.subTest(value=value),self.assertRaises(verifier.VerificationError):check(dict(r,stream_operand_ready=value))
  with self.assertRaises(verifier.VerificationError):check(dict(r,target_stream_operand_delay=0))
  check(dict(r,target_stream_operand_delay=0,stream_operand_ready=csv([0]*30)))
  check(dict(constraint_schema=85))

if __name__=='__main__':unittest.main()
