#!/usr/bin/env python3
"""Literal accepted-edge histories for actual bypass temporal classification."""
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
CPP = r'''
#include "scenarios/mixed/stream_footprint.hpp"
#include "scenarios/mixed/store_address_replay.hpp"
#include <array>
#include <cassert>
#include <deque>
#include <map>
using namespace memblock;
using namespace memblock::scenarios::mixed;
struct Work {
 unsigned generation=0,kind=0,bypass_relations=0;
 bool disjoint_bypass=false,bypass_accepted=false,recovery_id=false;
 LoadTransaction load;StoreTransaction store;StreamFootprint footprint;
 std::optional<std::uint64_t> first_address_edge;
 std::vector<std::optional<std::uint64_t>> vector_first_edges;
 StoreAddressReplay address_replay;
 bool is_store() const {return kind==2||kind==3;}
};
struct Sample {unsigned lane;bool sq_flag;unsigned sq_value;std::uint64_t cycle;bool hit;};
struct EnvironmentShell {
 struct Stats {std::vector<Sample> sta_samples;} stats;
 const auto& iq_slow_feedback_stats()const{return stats;}
};
struct Coverage {std::array<std::uint64_t,18> stream_disjoint_bypass{};};
struct Attempt {unsigned generation,kind,uop;bool replay=false;};
struct Event {unsigned tag;std::uint64_t edge;OrdinaryIssueOutcome outcome=OrdinaryIssueOutcome::accepted;};
using IssueOutcome=OrdinaryIssueOutcome;
bool classify(std::deque<Work>&live,Coverage&coverage,EnvironmentShell&environment,
              const std::map<unsigned,Attempt>&attempts,const std::vector<Event>&events) {
 std::string phase;
 // PRODUCTION_PREPASS
 for(const auto&event:events) {
  if(event.outcome!=IssueOutcome::accepted)continue;
  const auto&attempt=attempts.at(event.tag);
  auto owner=std::find_if(live.begin(),live.end(),[&](const auto&w){return w.generation==attempt.generation;});
  assert(owner!=live.end());auto&work=*owner;unsigned initial_events=1;
  if(attempt.kind==2) {
   if(!work.address_replay.accept(environment.stats.sta_samples.size(),event.edge))return false;
  }
  if(attempt.replay)initial_events=0;
  // PRODUCTION_CLASSIFICATION
 }
 return true;
}
Work store(bool vector) {
 Work w;w.generation=1;w.kind=vector?3:2;w.footprint={0x20000};
 w.store.address_lane=1;w.store.sq=54;w.store.sq_flag=true;
 if(vector)w.vector_first_edges.resize(2);
 return w;
}
Work load(unsigned cls) {
 Work w;w.generation=2;w.kind=cls==2?1:0;w.load.fp_wen=cls==1;w.disjoint_bypass=true;w.footprint={0x20001};w.vector_first_edges.resize(2);return w;
}
void check(const Coverage&c,unsigned cls,std::array<unsigned,3> wanted) {
 std::array<std::uint64_t,18> expected{};expected[cls+3]=1;
 for(unsigned b=0;b<3;++b)expected[9+cls*3+b]=wanted[b];
 assert(c.stream_disjoint_bypass==expected);
}
int main() {
 for(unsigned cls=0;cls<3;++cls) {
  for(unsigned reverse=0;reverse<2;++reverse) {
   { // Both actual accepted events share E: no before-STA relation.
    std::deque<Work> live{store(false),load(cls)};Coverage c;EnvironmentShell env;
    std::map<unsigned,Attempt>a{{1,{1,2,0}},{2,{2,1,0}}};
    std::vector<Event>e{{1,20},{2,20}};if(reverse)std::reverse(e.begin(),e.end());
    assert(classify(live,c,env,a,e));check(c,cls,{0,1,0});
   }
   { // Both vector siblings accepted at E; iteration order cannot count missing sibling.
    std::deque<Work> live{store(true),load(cls)};Coverage c;EnvironmentShell env;
    std::map<unsigned,Attempt>a{{1,{1,4,0}},{2,{2,1,0}},{3,{1,4,1}}};
    std::vector<Event>e{{1,20},{2,20},{3,20}};if(reverse)std::reverse(e.begin(),e.end());
    assert(classify(live,c,env,a,e));check(c,cls,{0,0,0});
   }
  }
  for(unsigned kind=0;kind<3;++kind) {
   std::deque<Work>live{store(kind==2),load(cls)};Coverage c;EnvironmentShell env;
   if(kind==1){live[0].first_address_edge=10;assert(live[0].address_replay.accept(0,10));}
   std::map<unsigned,Attempt>a{{2,{2,1,0}},{3,{2,4,1}}};
   assert(classify(live,c,env,a,{{2,20},{3,21}}));
   check(c,cls,{unsigned(kind==0),unsigned(kind==1),unsigned(kind==2)}); // each relation once
  }
  for(unsigned delta=0;delta<3;++delta)for(unsigned consume=0;consume<2;++consume) {
   std::deque<Work>live{store(false),load(cls)};Coverage c;EnvironmentShell env;
   live[0].first_address_edge=10;env.stats.sta_samples.push_back({1,true,54,1,true});
   assert(live[0].address_replay.accept(1,10));
   env.stats.sta_samples.push_back({0,true,54,18,true}); // wrong lane
   env.stats.sta_samples.push_back({1,false,54,18,true}); // wrong wrap
   env.stats.sta_samples.push_back({1,true,53,18,true}); // wrong SQ
   env.stats.sta_samples.push_back({1,true,54,19+delta,true});
   if(consume)assert(live[0].address_replay.observe(env.stats.sta_samples,live[0].store)==StoreAddressReplay::Observation::hit);
   std::map<unsigned,Attempt>a{{2,{2,1,0}}};assert(classify(live,c,env,a,{{2,20}}));
   check(c,cls,{0,unsigned(delta==2),0}); // E-1/E already known; E+1 not known
  }
  { // Retry/killed store event cannot resolve first-issue state.
   std::deque<Work>live{store(false),load(cls)};Coverage c;EnvironmentShell env;
   std::map<unsigned,Attempt>a{{1,{1,2,0}},{2,{2,1,0}}};
   assert(classify(live,c,env,a,{{1,20,IssueOutcome::canceled},{2,20}}));check(c,cls,{1,0,0});
  }
 }
}
'''

class StreamDisjointHistoryTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tmp=tempfile.TemporaryDirectory(prefix='memblock-disjoint-history-');cls.addClassCleanup(cls.tmp.cleanup)
        stream=(ROOT/'cpp/scenarios/mixed_stream.cpp').read_text()
        a=stream.index('        for (const auto &event : events) {');b=stream.index('        for (const auto &event : events) {',a+1)
        prepass=stream[a:b]
        a=stream.index('            if (work.disjoint_bypass && initial_events) {');b=stream.index('            if (work.paired_initial',a)
        cls.source=CPP.replace('// PRODUCTION_PREPASS',prepass).replace('// PRODUCTION_CLASSIFICATION',stream[a:b])
        cls.binary=cls.compile(cls.source,'baseline')
    @classmethod
    def compile(cls,source,name):
        p=Path(cls.tmp.name)/(name+'.cpp');p.write_text(source);binary=p.with_suffix('')
        r=subprocess.run(['c++','-std=c++20','-O0','-I',str(ROOT/'cpp'),str(p),'-o',str(binary)],capture_output=True,text=True,timeout=60)
        if r.returncode:raise AssertionError(r.stdout+r.stderr)
        return binary
    def test_literal_same_edge_translation_and_multi_uop_histories(self):
        r=subprocess.run([str(self.binary)],capture_output=True,text=True,timeout=20)
        self.assertEqual(r.returncode,0,r.stdout+r.stderr)
    def test_private_temporal_classification_faults_reject(self):
        mutations=[('same_edge_scalar','*older->first_address_edge > event.edge','*older->first_address_edge >= event.edge'),
                   ('same_edge_vector','*edge > event.edge','*edge >= event.edge'),
                   ('missing_first_edges','if (event.outcome != IssueOutcome::accepted) continue;','continue;'),
                   ('repeated_relation','relations & ~work.bypass_relations','relations'),
                   ('missing_relation_update','work.bypass_relations |= relations;','')]
        for name,old,new in mutations:
            self.assertIn(old,self.source)
            binary=self.compile(self.source.replace(old,new),name)
            r=subprocess.run([str(binary)],capture_output=True,text=True,timeout=20)
            self.assertNotEqual(r.returncode,0,name+' escaped')

if __name__=='__main__':unittest.main()
