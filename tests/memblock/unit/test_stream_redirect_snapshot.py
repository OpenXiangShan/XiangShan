#!/usr/bin/env python3
"""Compile the actual generic return decision against changing public feedback."""
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
CPP = r'''
#include <cstdint>
#include <string>
struct Request { unsigned rob_value=11,rob_flag=0,level=1; };
struct Stats { unsigned count=1;std::uint64_t last_cycle=20;Request last; };
struct Shell {
 Stats stats;std::uint64_t now=21;unsigned injection=0,returned=0;
 const Stats& memory_violation_stats()const{return stats;}
 auto cycle()const{return now;}
 bool run_cycles(unsigned n){now+=n;if(injection==1)++stats.count;return true;}
};
bool exercise(unsigned injection){
 Shell environment;environment.injection=injection;std::string phase;
 // PRODUCTION_SNAPSHOT
 const auto original_count=violation.count;
 const auto cancel_suffix=[&](unsigned rob,unsigned flag,unsigned level,bool trap){
  if(rob!=11||flag||level!=1||trap)return false;
  ++environment.returned;if(injection==2)++environment.stats.count;return true;
 };
 const auto tail=[&](){
 std::string detail;
 // PRODUCTION_RETURN
 return true;
 };
 const bool result=tail();
 if(injection==0)return result&&environment.returned==1&&expected_violations==original_count;
 return !result&&expected_violations==0&&(injection==1?environment.returned==0:environment.returned==1);
}
int main(){for(unsigned n=0;n<3;++n)if(!exercise(n))return 1;}
'''

FEEDBACK_CPP = r'''
#include "memblock_env.hpp"
#include "scenarios/mixed/store_address_replay.hpp"
#include <deque>
#include <string>
using namespace memblock;using namespace memblock::scenarios::mixed;
struct Work {unsigned kind=2,recovery_id=0;StoreTransaction store;StoreAddressReplay address_replay;};
struct Shell {Environment::IqSlowFeedbackStats stats;const auto& iq_slow_feedback_stats()const{return stats;}};
struct Coverage {std::array<std::uint64_t,12> stream_sta{};};
bool sample(std::deque<Work>&live,Shell&environment,Coverage&coverage){std::string phase;
// PRODUCTION_SURVIVOR_FEEDBACK
return true;}
int main(){for(bool hit:{false,true}){std::deque<Work>live(1);auto&w=live.front();
w.store.address_lane=1;w.store.sq=54;w.store.sq_flag=true;
if(!w.address_replay.accept(0,20))return 1;Shell env;Coverage cov;
Environment::StoreSlowFeedbackSample feedback;feedback.lane=1;feedback.sq_flag=true;
feedback.sq_value=54;feedback.cycle=24;feedback.hit=hit;env.stats.sta_samples.push_back(feedback);
if(!sample(live,env,cov)||w.address_replay.translated()!=hit||cov.stream_sta[hit?2:3]!=1)return 1;
if(!sample(live,env,cov)||cov.stream_sta[hit?2:3]!=1)return 1;
}return 0;}
'''

class StreamRedirectSnapshotTest(unittest.TestCase):
 @classmethod
 def setUpClass(cls):
  cls.directory=tempfile.TemporaryDirectory(prefix='memblock-redirect-snapshot-');cls.addClassCleanup(cls.directory.cleanup)
  s=(ROOT/'cpp/scenarios/mixed_stream.cpp').read_text()
  begin=s.index('            const auto violation = environment.memory_violation_stats();')
  snapshot=s[begin:s.index('\n',begin)]
  begin=s.index('            const auto return_cycle = violation.last_cycle + 2U;')
  end=s.index('            continue;',begin)
  cls.source=CPP.replace('// PRODUCTION_SNAPSHOT',snapshot+'\n unsigned expected_violations=0;').replace('// PRODUCTION_RETURN',s[begin:end].replace('detail.str()','detail'))
 @classmethod
 def compile(cls,source,name):
  p=Path(cls.directory.name)/(name+'.cpp');p.write_text(source);b=p.with_suffix('')
  r=subprocess.run(['c++','-std=c++20','-I',str(ROOT/'cpp'),str(p),'-o',str(b)],capture_output=True,text=True,timeout=60)
  if r.returncode:raise AssertionError(r.stderr)
  return b
 def test_actual_snapshot_rejects_requests_during_wait_and_recovery(self):
  r=subprocess.run([str(self.compile(self.source,'baseline'))],capture_output=True,text=True,timeout=20)
  self.assertEqual(r.returncode,0,r.stderr)
 def test_private_live_reference_and_missing_guard_faults_reject(self):
  mutants=[self.source.replace('const auto violation = environment.memory_violation_stats();','const auto &violation = environment.memory_violation_stats();'),
   self.source.replace('environment.memory_violation_stats().count != violation.count','false')]
  for n,source in enumerate(mutants):
   r=subprocess.run([str(self.compile(source,str(n)))],capture_output=True,text=True,timeout=20)
   self.assertNotEqual(r.returncode,0,'competing redirect was swallowed')
 def test_surviving_feedback_before_atomic_head_restoration(self):
  s=(ROOT/'cpp/scenarios/mixed_stream.cpp').read_text()
  begin=s.index('        for (auto &item : live) if (item.kind == 2 && !item.recovery_id) {')
  end=s.index('        if (!trap && !live.empty()',begin)
  source=FEEDBACK_CPP.replace('// PRODUCTION_SURVIVOR_FEEDBACK',s[begin:end])
  self.assertEqual(subprocess.run([str(self.compile(source,'feedback'))]).returncode,0)
  mutant=FEEDBACK_CPP.replace('// PRODUCTION_SURVIVOR_FEEDBACK','')
  self.assertNotEqual(subprocess.run([str(self.compile(mutant,'missing_feedback'))]).returncode,0)

if __name__=='__main__':unittest.main()
