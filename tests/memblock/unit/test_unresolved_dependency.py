#!/usr/bin/env python3
"""Exercise the real delayed-address planner and mandatory recovery obligation."""
from __future__ import annotations
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))
import verify_regression

CPP = r'''
#include "scenarios/shared.hpp"
#include "scenarios/mixed/unresolved_dependency.hpp"
#include <set>
using namespace memblock::scenarios::mixed;
static void require(bool v) { if(!v) throw std::runtime_error("check failed"); }
static std::string signature(const UnresolvedDependencyPlan &p, bool all) {
    std::ostringstream out;
    out << p.store.offset << ':' << p.store.width << ':' << p.load.offset << ':' << p.load.op;
    if(all) out << ':' << p.store.data << ':' << p.data_before_load << ':' << p.resolve_delay;
    return out.str();
}
int main() {
    for(const auto &[name,rate]:std::array<std::pair<const char *,unsigned>,4>{{
        {"coverage",200},{"spec",100},{"corner",300},{"cmo-stress",0}}}) {
        auto c=memblock::scenarios::RandomConstraints::preset(name);
        require(c.scalar_unresolved_per_mille==rate && c.scalar_unresolved_conflict_per_mille==500);
        for(auto key:{"scalar-unresolved","scalar-unresolved-conflict"}) {
            for(auto n:{0U,1000U,1001U}) {
                auto copy=c;copy.apply(std::string(key)+'='+std::to_string(n));
                bool rejected=false;
                try { copy.validate(); } catch(const std::invalid_argument &) {rejected=true;}
                require(rejected==(n==1001));
            }
        }
    }
    std::mt19937_64 r(17), replay(17);
    std::set<std::string> shapes;
    unsigned conflicts=0, controls=0, before=0, partial=0;
    std::array<bool,4> stores{}, loads{};
    for(unsigned sample=0;sample<512;++sample) {
        auto p=make_unresolved_dependency_plan(r,500);
        require(signature(p,true)==signature(make_unresolved_dependency_plan(replay,500),true));
        shapes.insert(signature(p,false));
        require(p.store.offset%p.store.width==0 && p.load.offset%p.load.width()==0);
        require(p.store.offset+p.store.width<=64 && p.load.offset+p.load.width()<=64);
        require((p.store.offset>>4)==(p.load.offset>>4) && p.resolve_delay<8);
        bool overlap=p.store.offset<p.load.offset+p.load.width() && p.load.offset<p.store.offset+p.store.width;
        require(overlap==p.conflict);
        conflicts+=p.conflict; controls+=!p.conflict; before+=p.data_before_load;
        partial+=overlap && (p.store.offset!=p.load.offset || p.store.width!=p.load.width());
        stores[std::countr_zero(p.store.width)]=true;
        loads[p.load.op&3U]=true;
    }
    require(shapes.size()>150 && conflicts>100 && controls>100 && before>100 && before<400 && partial>100);
    for(bool v:stores) require(v);
    for(bool v:loads) require(v);
    for(unsigned rate:{0U,1000U}) for(unsigned n=0;n<128;++n)
        require(make_unresolved_dependency_plan(r,rate).conflict==(rate==1000));
    std::mt19937_64 destinations(19);
    for(unsigned n=0;n<512;++n) {
        const auto pdests=unresolved_load_destinations(destinations);
        std::set<unsigned> unique(pdests.begin(),pdests.end());
        require(unique.size()==3 && *unique.begin()>=1 && *unique.rbegin()<=223);
    }
    memblock::LoadTransaction load;
    load.rob=159;load.rob_flag=true;load.ftq_ptr=45;load.ftq_offset=7;load.predecode_rvc=true;
    memblock::generated::MemoryViolation redirect{};
    redirect.valid=redirect.level=true;redirect.rob_value=load.rob;redirect.rob_flag=load.rob_flag;
    redirect.ftq_value=load.ftq_ptr;redirect.ftq_offset=load.ftq_offset;redirect.is_rvc=load.predecode_rvc;
    for(unsigned mutation=0;mutation<8;++mutation) {
        auto bad=redirect;
        switch(mutation) {
          case 0:bad.valid=false;break;case 1:bad.level=false;break;
          case 2:++bad.rob_value;break;case 3:bad.rob_flag=false;break;
          case 4:bad.ftq_flag=true;break;case 5:++bad.ftq_value;break;
          case 6:--bad.ftq_offset;break;case 7:bad.is_rvc=false;break;
        }
        UnresolvedLoadObligation o(load,true);
        require(o.provisional() && !o.redirect(bad) && !o.done());
    }
    UnresolvedLoadObligation ok(load,true);
    require(!ok.done() && ok.provisional() && !ok.done() && ok.redirect(redirect) &&
        ok.returned() && !ok.done() && ok.refetched() && !ok.done() && ok.readback() && ok.done());
    require(!ok.refetched() && !ok.done()); // duplicate completion latches failure
    UnresolvedLoadObligation missing_redirect(load,true);
    require(missing_redirect.provisional() && !missing_redirect.returned() && !missing_redirect.done());
    UnresolvedLoadObligation missing_refetch(load,true);
    require(missing_refetch.provisional() && missing_refetch.redirect(redirect) && missing_refetch.returned() &&
        !missing_refetch.readback() && !missing_refetch.done());
    UnresolvedLoadObligation early(load,true);
    require(!early.refetched() && !early.provisional() && !early.done());
    UnresolvedLoadObligation control(load,false);
    require(control.provisional() && control.no_conflict() && control.readback() && control.done());
    UnresolvedLoadObligation spurious(load,false);
    require(spurious.provisional() && !spurious.redirect(redirect) && !spurious.done());
    std::cout << "unique_unresolved_topologies=" << shapes.size() << '\n';
}
'''

class UnresolvedDependencyTest(unittest.TestCase):
    def test_randomized_byte_overlap_and_recovery_obligation(self):
        with tempfile.TemporaryDirectory() as folder:
            source,binary=Path(folder)/"fixture.cpp",Path(folder)/"fixture"
            source.write_text(CPP)
            built=subprocess.run(["c++","-std=c++20","-O0","-I",str(ROOT/"cpp"),str(source),"-o",str(binary)],capture_output=True,text=True,timeout=60)
            self.assertEqual(built.returncode,0,built.stdout+built.stderr)
            ran=subprocess.run([str(binary)],capture_output=True,text=True,timeout=10)
            self.assertEqual(ran.returncode,0,ran.stdout+ran.stderr)
            self.assertIn("unique_unresolved_topologies=",ran.stdout)

    def test_recovery_accounting_rejects_missing_steps(self):
        check=verify_regression._check_scalar_unresolved_accounting
        base={"constraint_schema":56,"target_scalar_unresolved":100,
              "target_scalar_unresolved_conflict":500,
              "scalar_unresolved":"4,2,2,4,2,2,2,4,2,9,0"}
        check(base)
        check({**base,"scalar_unresolved":"0,0,0,0,0,0,0,0,0,0,0"})
        for field,value in (("target_scalar_unresolved",0),("target_scalar_unresolved",True),
            ("target_scalar_unresolved_conflict",0),("target_scalar_unresolved_conflict",1000),
            ("target_scalar_unresolved_conflict",1001),
            ("scalar_unresolved","4,2,1,4,2,2,2,4,2,9,0"),
            ("scalar_unresolved","4,2,2,3,2,2,2,4,2,9,0"),
            ("scalar_unresolved","4,2,2,4,1,2,2,4,2,9,0"),
            ("scalar_unresolved","4,2,2,4,2,1,2,4,2,9,0"),
            ("scalar_unresolved","4,2,2,4,2,2,1,4,2,9,0"),
            ("scalar_unresolved","4,2,2,4,2,2,2,3,2,9,0"),
            ("scalar_unresolved","4,2,2,4,2,2,2,4,5,9,0"),
            ("scalar_unresolved","4,2,2,4,2,2,2,4,2,1,0"),
            ("scalar_unresolved","4,2,2,4,2,2,2,4,2,17,0"),
            ("scalar_unresolved","4,2,2,4,2,2,2,4,2,9,3")):
            with self.subTest(field=field,value=value),self.assertRaises(verify_regression.VerificationError):
                check({**base,field:value})

if __name__=="__main__":unittest.main()
