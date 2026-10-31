#!/usr/bin/env python3
"""The actual scalar-error chooser must not assume generated lines are cold."""
from pathlib import Path
import subprocess,tempfile,unittest
ROOT=Path(__file__).resolve().parents[1]
CPP=r'''
#include <cstdint>
#include <vector>
#include <string>
#include <stdexcept>
struct Request {std::uint64_t address,bytes;};
struct Shell {
 std::vector<Request> requests; bool drained=false; unsigned queries=0;
 bool run_until_dcache_idle(unsigned){drained=true;return true;}
 bool run_until_queues_retired(unsigned){return true;}
 std::uint64_t dcache_requests_covering_since(std::uint64_t a,unsigned first){
  if(!drained||first)throw std::runtime_error("freshness checked before drain or omitted prior history");
  ++queries;unsigned n=0;for(auto r:requests)n+=a>=r.address&&a-r.address<r.bytes;return n;
 }
};
struct Chooser {static constexpr std::uint64_t dcache_error_base=0x85400000,dcache_error_span=256;
 static constexpr unsigned constrained_completion_timeout=2048;
 std::uint64_t dcache_error_line=0;Shell environment;std::string phase;
 bool choose(std::uint64_t&address){
 // PRODUCTION_CHOOSER
 return true;
 }};
int main(){
 for(unsigned offset=0;offset<64;++offset){
  Chooser c;c.environment.requests={{c.dcache_error_base+offset,1}};std::uint64_t a=0;
  if(!c.choose(a)||a!=c.dcache_error_base+64+56||c.dcache_error_line!=2)return 1;
 }
 for(unsigned offset=0;offset<64;++offset){
  Chooser c;c.environment.requests={{c.dcache_error_base+offset,1},{c.dcache_error_base+64+offset,1}};
  std::uint64_t a=0;if(!c.choose(a)||a!=c.dcache_error_base+3*64+24||c.dcache_error_line!=3)return 2;
 }
 Chooser all;all.environment.requests={{all.dcache_error_base,256}};std::uint64_t a=0;
 if(all.choose(a)||all.phase!="random-dcache-load-error-address-space-exhausted"||all.dcache_error_line!=4)return 3;
 Chooser bounds;bounds.environment.requests={{bounds.dcache_error_base-1,1},{bounds.dcache_error_base+64,1}};
 if(!bounds.choose(a)||a!=bounds.dcache_error_base+24||bounds.dcache_error_line!=1)return 4;
 Chooser fresh;if(!fresh.choose(a)||a!=fresh.dcache_error_base+24||fresh.environment.queries!=64)return 5;
}
'''
class ScalarErrorFreshnessTest(unittest.TestCase):
 @classmethod
 def setUpClass(cls):
  cls.d=tempfile.TemporaryDirectory(prefix='memblock-error-fresh-');cls.addClassCleanup(cls.d.cleanup)
  s=(ROOT/'cpp/scenarios/mixed_action_scalar.cpp').read_text();begin=s.index('            if (!environment.run_until_dcache_idle(constrained_completion_timeout)');end=s.index('            ordinary_leaf_addressed = true;',begin)
  cls.source=CPP.replace('// PRODUCTION_CHOOSER',s[begin:end])
 @classmethod
 def compile(cls,source,name):
  p=Path(cls.d.name)/(name+'.cpp');p.write_text(source);b=p.with_suffix('')
  r=subprocess.run(['c++','-std=c++20',str(p),'-o',str(b)],capture_output=True,text=True,timeout=60)
  if r.returncode:raise AssertionError(r.stderr)
  return b
 def test_every_byte_prefetch_history_and_bounded_exhaustion(self):
  r=subprocess.run([str(self.compile(self.source,'base'))],capture_output=True,text=True,timeout=10);self.assertEqual(r.returncode,0,r.stderr)
 def test_private_missing_history_and_line_base_only_mutants_reject(self):
  for name,old,new in [('no-history','fresh &= environment.dcache_requests_covering_since((address & ~std::uint64_t{63}) + byte, 0) == 0;','fresh &= true;'),('base-only','+ byte, 0) == 0;','+ 0, 0) == 0;'),('skip-endpoint','byte < 64 && fresh','byte < 63 && fresh')]:
   self.assertIn(old,self.source);r=subprocess.run([str(self.compile(self.source.replace(old,new),name))],capture_output=True,text=True,timeout=10)
   self.assertNotEqual(r.returncode,0,name+' escaped')
if __name__=='__main__':unittest.main()
