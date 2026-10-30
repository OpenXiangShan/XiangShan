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
 unsigned generation=0,kind=0,bypass_relations=0,bypass_spatial_relations=0;
 bool disjoint_bypass=false,byte_bypass=false,bypass_accepted=false,recovery_id=false;
 LoadTransaction load;StoreTransaction store;StreamFootprint footprint,byte_footprint;
 std::vector<VectorMemoryTransaction> vectors;
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
struct Coverage {std::array<std::uint64_t,18> stream_disjoint_bypass{},stream_byte_bypass{};std::array<std::uint64_t,9> stream_byte_bypass_granules{};};
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
 w.vectors.resize(2);for(auto&t:w.vectors){t.address=0x80000000;t.eew=0;t.vl=1;}
 w.store.address_lane=1;w.store.sq=54;w.store.sq_flag=true;
 if(vector)w.vector_first_edges.resize(2);
 return w;
}
Work load(unsigned cls) {
 Work w;w.generation=2;w.kind=cls==2?1:0;w.load.fp_wen=cls==1;w.disjoint_bypass=true;w.footprint={0x20001};w.vector_first_edges.resize(2);
 w.vectors.resize(2);for(auto&t:w.vectors){t.address=0x80000001;t.eew=0;t.vl=1;}return w;
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
 for(unsigned cls=0;cls<3;++cls) for(unsigned granule=0;granule<3;++granule) {
  std::deque<Work>live{store(false),load(cls)};Coverage c;EnvironmentShell env;
  live[1].disjoint_bypass=false;live[1].byte_bypass=true;live[1].footprint=live[0].footprint;
  live[0].byte_footprint={0x80000000};
  live[1].byte_footprint={0x80000000+std::array<unsigned,3>{32,8,1}[granule]};
  for(auto&t:live[1].vectors)t.address=*live[1].byte_footprint.begin();
  std::map<unsigned,Attempt>a{{2,{2,1,0}},{3,{2,4,1}}};
  assert(classify(live,c,env,a,{{2,20},{3,21}}));
  std::array<std::uint64_t,18>expected{};expected[cls+3]=1;expected[9+cls*3]=1;
  assert(c.stream_byte_bypass==expected);
  std::array<std::uint64_t,9>spatial{};spatial[cls*3]=1;spatial[cls*3+1]=granule>=1;spatial[cls*3+2]=granule>=2;
  assert(c.stream_byte_bypass_granules==spatial);
  // A still-unresolved store on a different line is not a same-line witness.
  live[0].footprint={0x20002};live[0].byte_footprint={0x80000080};live[1].bypass_accepted=false;live[1].bypass_relations=0;live[1].bypass_spatial_relations=0;c={};
  assert(classify(live,c,env,a,{{2,22}}));expected[9+cls*3]=0;
  assert((c.stream_byte_bypass==expected&&c.stream_byte_bypass_granules==std::array<std::uint64_t,9>{}));
 }
 for(unsigned sibling=0;sibling<2;++sibling)for(unsigned same_edge=0;same_edge<2;++same_edge) {
  // The current accepted load uop, and only still-unaccepted older store
  // siblings, supply actual byte witnesses. Future/resolved siblings do not.
  std::deque<Work>live{store(sibling),load(2)};Coverage c;EnvironmentShell env;
  live[1].disjoint_bypass=false;live[1].byte_bypass=true;
  live[0].footprint=live[1].footprint={0x2000000,0x2000001};
  live[0].byte_footprint={0x80000000,0x80000040};live[1].byte_footprint={0x80000001,0x80000041};
  if(!sibling) {
   live[0].byte_footprint={0x80000000};
   live[1].vectors[0].address=0x80000041;live[1].vectors[1].address=0x80000001;
  } else {
   live[0].vectors[0].address=0x80000000;live[0].vectors[1].address=0x80000040;
   live[0].vector_first_edges[0]=10; // sharing store sibling already accepted
   live[1].vectors[0].address=live[1].vectors[1].address=0x80000001;
  }
  std::map<unsigned,Attempt>a{{1,{1,2,0}},{2,{2,4,0}},{3,{2,4,1}},{4,{1,4,1}}};
  assert(classify(live,c,env,a,{{2,20}}));
  assert(c.stream_byte_bypass[5]==1);
  assert((c.stream_byte_bypass_granules==std::array<std::uint64_t,9>{}));
  for(unsigned i=9;i<18;++i)assert(c.stream_byte_bypass[i]==0);
  if(!sibling) {
   assert(classify(live,c,env,a,{{1,21}}));env.stats.sta_samples.push_back({1,true,54,22,true});
  } else assert(classify(live,c,env,a,{{4,22}}));
  assert(classify(live,c,env,a,{{3,same_edge?22U:23U}}));
  assert((c.stream_byte_bypass_granules==std::array<std::uint64_t,9>{}));
  for(unsigned i=9;i<18;++i)assert(c.stream_byte_bypass[i]==0);
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
        a=stream.index('            if ((work.disjoint_bypass || work.byte_bypass) && initial_events) {');b=stream.index('            if (work.paired_initial',a)
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
    def test_same_byte_mode_reuses_all_clock_and_lifetime_histories(self):
        source=self.source.replace('w.footprint={0x20000};',
            'w.footprint={0x20000};w.byte_footprint={0x80000000};')
        source=source.replace('w.disjoint_bypass=true;w.footprint={0x20001};',
            'w.byte_bypass=true;w.footprint={0x20000};w.byte_footprint={0x80000001};')
        source=source.replace('assert(c.stream_disjoint_bypass==expected);',
            'assert(c.stream_byte_bypass==expected);'
            'std::array<std::uint64_t,9>spatial{};'
            'for(unsigned b=0;b<3;++b)spatial[cls*3+b]=std::any_of(wanted.begin(),wanted.end(),[](auto n){return n!=0;});'
            'assert(c.stream_byte_bypass_granules==spatial);')
        r=subprocess.run([str(self.compile(source,'byte_temporal'))],capture_output=True,text=True,timeout=20)
        self.assertEqual(r.returncode,0,r.stdout+r.stderr)
    def test_private_temporal_classification_faults_reject(self):
        mutations=[('same_edge_scalar','*older->first_address_edge > event.edge','*older->first_address_edge >= event.edge'),
                   ('same_edge_vector','*edge <= event.edge','*edge < event.edge'),
                   ('missing_first_edges','if (event.outcome != IssueOutcome::accepted) continue;','continue;'),
                   ('repeated_relation','relations & ~work.bypass_relations','relations'),
                   ('missing_relation_update','work.bypass_relations |= relations;','')]
        mutations += [('missing_spatial_dedup','spatial & ~work.bypass_spatial_relations','spatial'),
                      ('count_unrelated_line','if (work.byte_bypass && !stream_shares_granule(accepted_bytes, unresolved_bytes, 64U)) continue;',''),
                      ('future_load_uop','work.byte_bypass && work.kind == 1','false'),
                      ('resolved_store_sibling','if (edge && *edge <= event.edge) continue;',''),
                      ('wrong_granule','unresolved_bytes, 8U','unresolved_bytes, 16U')]
        for name,old,new in mutations:
            self.assertIn(old,self.source)
            binary=self.compile(self.source.replace(old,new),name)
            r=subprocess.run([str(binary)],capture_output=True,text=True,timeout=20)
            self.assertNotEqual(r.returncode,0,name+' escaped')

if __name__=='__main__':unittest.main()
