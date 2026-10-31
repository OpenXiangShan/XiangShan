#!/usr/bin/env python3
"""FP-load data dependency controls and independent lifecycle accounting."""
from pathlib import Path
import importlib.util
import subprocess
import tempfile
import sys
import unittest
ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT/'scripts'))
spec=importlib.util.spec_from_file_location('verify_fp_data',ROOT/'scripts/verify_regression.py')
verifier=importlib.util.module_from_spec(spec);spec.loader.exec_module(verifier)
def csv(row):return ','.join(map(str,row))
CPP=r'''
#include "scenarios/shared.hpp"
#include <cassert>
int main(){
 for(auto profile:{"spec","coverage","corner","cmo-stress"}){
  auto c=memblock::scenarios::RandomConstraints::preset(profile);
  assert(c.stream_fp_data_per_mille==250);
  for(auto rate:{0U,1U,250U,1000U}){
   c.apply("stream-fp-data="+std::to_string(rate));c.validate();
   assert(c.summary().find("target_stream_fp_data="+std::to_string(rate)+" ")!=std::string::npos);
  }
  c.apply("stream-fp-data=1001");bool failed=false;
  try{c.validate();}catch(const std::invalid_argument&){failed=true;}assert(failed);
 }
}
'''
class StreamFpDataTest(unittest.TestCase):
 def test_constraints_endpoints_presets_and_serialization(self):
  with tempfile.TemporaryDirectory(prefix='memblock-fp-data-') as d:
   p=Path(d)/'test.cpp';p.write_text(CPP);b=p.with_suffix('')
   r=subprocess.run(['c++','-std=c++20','-O2','-I',str(ROOT/'cpp'),str(p),'-o',str(b)],capture_output=True,text=True,timeout=60)
   self.assertEqual(r.returncode,0,r.stdout+r.stderr)
   r=subprocess.run([str(b)],capture_output=True,text=True,timeout=10);self.assertEqual(r.returncode,0,r.stdout+r.stderr)
 def record(self):
  return dict(constraint_schema=87,target_stream_fp_data=1000,target_scalar_fp=1000,target_stream_dependency=1000,
   target_scalar_fp_widths=csv([1,1,1]),stream_fp_data=csv([2]*3+[1]*3+[1]*3+[1]*3+[2,1]+[1,0,0,0,1,0,0,0,1]+[0]),
   stream_dependencies=csv([0,6,0,3,0,3,2,1,1]),stream_cancel=csv([6]*29))
 def test_lifecycle_corruptions_disabled_domains_and_legacy(self):
  check=verifier._check_stream_fp_data;r=self.record();check(r)
  for i in range(24):
   c=list(map(int,r['stream_fp_data'].split(',')));c[i]+=10
   with self.subTest(bin=i),self.assertRaises(verifier.VerificationError):check(dict(r,stream_fp_data=csv(c)))
  for key in r:
   if key=='constraint_schema':continue
   q=dict(r);del q[key]
   if key in ('target_scalar_fp','target_stream_dependency'):continue
   with self.subTest(missing=key),self.assertRaises(verifier.VerificationError):check(q)
  for rate in [-1,1001,True,'250']:
   with self.subTest(rate=rate),self.assertRaises(verifier.VerificationError):check(dict(r,target_stream_fp_data=rate))
  for k in ['target_stream_fp_data','target_scalar_fp','target_stream_dependency']:
   with self.subTest(disabled=k),self.assertRaises(verifier.VerificationError):check(dict(r,**{k:0}))
  with self.assertRaises(verifier.VerificationError):check(dict(r,target_scalar_fp_widths='1,0,1'))
  with self.assertRaises(verifier.VerificationError):check(dict(r,stream_fp_data=csv([0]*23)))
  check(dict(constraint_schema=86))
  zero=dict(r,stream_fp_data=csv([0]*24),stream_dependencies=csv([0]*9),target_stream_fp_data=0);check(zero)
if __name__=='__main__':unittest.main()
