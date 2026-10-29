#!/usr/bin/env python3
"""Compile production cancellation methods and the unfinished graph/epoch ledger.

The port/clock shell supplies controlled redirect propagation and completion
boundaries. It is a contract test, not a DUT or full-CPU timing model.
"""
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


def method(path, signature):
    text = (ROOT / path).read_text()
    start = text.index(signature)
    return text[start:text.index('\n}\n', start) + 3].replace('Environment::Impl::', 'CancelFixture::')


CPP = r'''
#include "memblock_env.hpp"
#include "scenarios/mixed/unresolved_epochs.hpp"
#include <functional>
#include <iostream>
#include <set>
namespace memblock {
#include "environment/scoreboards.inc"
struct Pin { std::uint64_t v=0; void set(std::uint64_t value){v=value;} std::uint64_t value()const{return v;} };
struct CancelFixture {
    struct { struct { struct { Pin rob_idx_flag,rob_idx_value,level,is_vls_exception,valid; } redirect;
        struct { Pin lq_cancel_cnt,sq_cancel_cnt; } retirement; } queues; void refresh_comb(){}; } dut_;
    struct { bool observe_redirect_cancellation(std::uint64_t,std::uint64_t){return true;} } queues_;
    FailureState error_{"fixture","contract"};
    LoadScoreboard scoreboard_;
    struct ScalarRedirectBoundary { unsigned rob;bool flag,flush_itself;std::uint64_t cycle,remaining; };
    std::optional<ScalarRedirectBoundary> scalar_redirect_boundary_;
    unsigned ticks=0,complete_tick=0;
    generated::ScalarLoadWriteback completion;
    std::uint64_t cycle()const{return ticks;}
    void tick(){++ticks;if(ticks==complete_tick)scoreboard_.observe(0,completion);}
    bool check_components(){error_.merge(scoreboard_.failures());return error_.empty();}
    bool redirect_after(std::uint8_t,bool,bool,bool=false);
    bool cancel_scalar_load_after_redirect(const LoadTransaction&);
};
// PRODUCTION_METHODS
}
using namespace memblock;
using namespace memblock::scenarios::mixed;
static void require(bool value,const char *message){if(!value)throw std::runtime_error(message);}
static LoadTransaction load(unsigned absolute=22){
    LoadTransaction t;t.address=0x80213038;t.op=LoadOp::ld;t.rob=absolute%160;t.rob_flag=(absolute/160)%2;
    t.pdest=47;t.lq=17;t.lq_flag=true;t.sq=13;t.sq_flag=true;t.ftq_ptr=11;t.ftq_offset=6;
    t.rf_wen=true;t.fp_wen=false;return t;
}
static generated::ScalarLoadWriteback wb(const LoadTransaction&t,std::uint64_t data=0x8877665544332211ULL){
    generated::ScalarLoadWriteback w{};w.valid=true;w.rob_value=t.rob;w.rob_flag=t.rob_flag;w.pdest=t.pdest;
    w.rf_wen=t.rf_wen;w.fp_wen=t.fp_wen;w.data=data;w.trigger=t.expected_trigger;return w;
}
static void boundaries(){
    auto t=load();CancelFixture f;f.scoreboard_.expect(t,0x8877665544332211ULL);
    f.dut_.queues.retirement.lq_cancel_cnt.set(1);
    require(f.redirect_after(21,false,true),"redirect helper failed");
    require(f.ticks==3&&f.scoreboard_.pending(t),"strict expectation removed before boundary");
    require(f.cancel_scalar_load_after_redirect(t)&&f.scoreboard_.done()&&f.scoreboard_.redirect_canceled()==1,"legal observed cancellation rejected");
    f.scoreboard_.observe(2,wb(t));
    require(!f.scoreboard_.ok(),"late canceled writeback accepted");
    for(unsigned tick:{1U,2U}){
        CancelFixture completed;completed.scoreboard_.expect(t,0x8877665544332211ULL);
        completed.complete_tick=tick;completed.completion=wb(t);completed.dut_.queues.retirement.lq_cancel_cnt.set(1);
        require(completed.redirect_after(21,false,true)&&completed.scoreboard_.done(),"legal propagation-boundary writeback rejected");
        require(!completed.cancel_scalar_load_after_redirect(t),"completed obligation was canceled");
    }
    for(unsigned bad=0;bad<4;++bad){
        CancelFixture invalid;invalid.scoreboard_.expect(t,0x8877665544332211ULL);
        invalid.dut_.queues.retirement.lq_cancel_cnt.set(bad==1?0:1);
        if(bad!=0)require(invalid.redirect_after(21,false,true),"boundary setup failed");
        if(bad==2)invalid.tick();
        if(bad==3)t=load(20);
        require(!invalid.cancel_scalar_load_after_redirect(t),"unobserved/stale/wrong obligation cancellation accepted");
        require(invalid.scoreboard_.pending(load()),"invalid cancellation dropped pending obligation");
    }
    CancelFixture duplicate;duplicate.dut_.queues.retirement.lq_cancel_cnt.set(2);
    t=load();duplicate.scoreboard_.expect(t,1);require(duplicate.redirect_after(21,false,true),"duplicate setup");
    require(duplicate.cancel_scalar_load_after_redirect(t),"first cancellation");
    require(!duplicate.cancel_scalar_load_after_redirect(t)&&duplicate.scoreboard_.redirect_canceled()==1,"duplicate cancellation accepted");
    std::cout<<"boundary_cases=11\n";
}
static void ages(){
    unsigned cases=0;
    for(unsigned boundary=0;boundary<320;++boundary)for(int delta=-7;delta<=7;++delta)for(bool itself:{false,true}){
        const unsigned absolute=(int(boundary)+delta+320)%320;const auto t=load(absolute);
        LoadScoreboard s;s.expect(t,1);
        const bool expected=delta>0||(delta==0&&itself);
        require(s.cancel_for_redirect(t,boundary%160,boundary/160,itself)==expected,"ROB age/wrap cancellation mismatch");
        require(s.done()==expected,"ROB age mismatch changed obligation");++cases;
    }
    for(unsigned field=0;field<12;++field){
        auto t=load();LoadScoreboard s;s.expect(t,1);
        switch(field){case 0:++t.pdest;break;case 1:++t.address;break;case 2:t.op=LoadOp::lw;break;
        case 3:++t.ftq_ptr;break;case 4:++t.ftq_offset;break;case 5:t.predecode_rvc=!t.predecode_rvc;break;
        case 6:++t.lq;break;case 7:++t.sq;break;case 8:t.lq_flag=!t.lq_flag;break;
        case 9:t.sq_flag=!t.sq_flag;break;case 10:t.rf_wen=false;break;case 11:t.fp_wen=true;break;}
        require(!s.cancel_for_redirect(t,21,false,true)&&!s.done(),"wrong cancellation metadata accepted");
    }
    std::cout<<"age_cases="<<cases<<" identity_faults=12\n";
}
static generated::MemoryViolation redirect(const LoadTransaction&t){
    generated::MemoryViolation r{};r.valid=true;r.level=true;r.rob_value=t.rob;r.rob_flag=t.rob_flag;
    r.ftq_value=t.ftq_ptr;r.ftq_offset=t.ftq_offset;r.is_rvc=t.predecode_rvc;return r;
}
static void ledger(){
    std::vector<LoadTransaction> loads{load(158),load(159),load(160)};
    loads[0].ftq_offset=0;loads[1].ftq_offset=2;loads[2].ftq_offset=4;
    UnresolvedLoadGroupObligation legal(loads,4,2);
    require(legal.provisional(0)&&legal.provisional(1)&&legal.redirect(redirect(loads[0]),0),"legal unfinished prefix rejected");
    require(legal.initial_canceled(2)&&legal.returned(),"observed initial cancellation rejected");
    reset_unresolved_ftq_suffix(loads,0);
    for(unsigned i:{2U,0U,1U})require(legal.refetched(i,loads[i]),"strict suffix rejected");
    require(legal.finish_round()&&legal.resolved()&&legal.readback()&&legal.done(),"unfinished epoch did not terminate");
    require(legal.executions(0)==2&&legal.executions(2)==1,"canceled execution counted as completion");
    for(unsigned fault=0;fault<6;++fault){
        auto original=std::vector<LoadTransaction>{load(158),load(159),load(160)};
        UnresolvedLoadGroupObligation bad(original,4,2);
        if(fault==0){require(!bad.provisional(2),"unfinished initial completion was waived");continue;}
        require(bad.provisional(0),"fault setup initial");
        if(fault!=1)require(bad.provisional(1),"fault setup second");
        if(fault==1){require(!bad.redirect(redirect(original[0]),0),"incomplete warm prefix accepted");continue;}
        if(fault==2){require(!bad.redirect(redirect(original[2]),2),"unfinished load became RAW victim");continue;}
        require(bad.redirect(redirect(original[0]),0),"fault setup redirect");
        if(fault==3){require(!bad.returned(),"missing initial cancellation accepted");continue;}
        if(fault==4){require(!bad.initial_canceled(1),"wrong initial canceled identity");continue;}
        require(bad.initial_canceled(2)&&!bad.initial_canceled(2),"duplicate initial cancellation");
    }
    std::cout<<"ledger_faults=6\n";
}
static void planner(){
    std::set<std::string> schedules;std::set<unsigned> counts,rounds,formats,first_orders;
    unsigned cases=0;
    for(unsigned seed=0;seed<1024;++seed){
        std::mt19937_64 random(seed);auto graph=make_unresolved_store_graph(random,1000,{500,{1,1,1}});
        auto group=make_unresolved_load_group(random,graph,{500,{1,1,1}},4,true);
        const unsigned n=group.loads.size();require(n>=2&&n<=4&&group.unfinished==n-1,"unfinished role geometry");
        require(group.primary<n-1&&!group.rounds.empty(),"missing completed conflict witness");
        for(const auto&store:graph.stores)require(!unresolved_overlap(store.bytes,group.loads.back()),"cold role overlaps store");
        std::string signature=std::to_string(n)+":";
        std::set<unsigned> issued(group.issue_order.begin(),group.issue_order.end());require(issued.size()==n&&*issued.begin()==0&&*issued.rbegin()==n-1,"initial issue order incomplete");
        for(auto i:group.issue_order)signature+=std::to_string(i);
        for(const auto&r:group.rounds){
            require(r.victim<n-1,"cold victim");std::set<unsigned> suffix(r.issue_order.begin(),r.issue_order.end());
            require(suffix.size()==n-r.victim&&*suffix.begin()==r.victim&&*suffix.rbegin()==n-1,"cold omitted from suffix");
            signature+='|';signature+=std::to_string(r.victim);for(auto i:r.issue_order)signature+=std::to_string(i);
        }
        schedules.insert(signature);counts.insert(n);rounds.insert(group.rounds.size());
        formats.insert(group.loads.back().op+8U*group.loads.back().fp);first_orders.insert(group.issue_order.front());++cases;
    }
    require(counts.size()==3&&rounds.size()==4&&formats.size()==10&&first_orders.size()==4,"unfinished shape/order variation missing");
    require(schedules.size()>300,"unfinished structural schedules collapsed");
    std::cout<<"planner_cases="<<cases<<" schedules="<<schedules.size()<<"\n";
}
int main(int argc,char**argv){try{
    const std::string mode=argc>1?argv[1]:"";
    if(mode=="boundaries")boundaries();else if(mode=="ages")ages();else if(mode=="ledger")ledger();else if(mode=="planner")planner();else return 2;
}catch(const std::exception&e){std::cerr<<e.what()<<'\n';return 1;}}
'''


class UnfinishedCancellationTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.directory = tempfile.TemporaryDirectory(prefix='memblock-unfinished-cancel-')
        cls.addClassCleanup(cls.directory.cleanup)
        text = CPP.replace('// PRODUCTION_METHODS', '\n'.join([
            method('cpp/environment/completion.cpp', 'auto Environment::Impl::redirect_after('),
            method('cpp/environment/observations.cpp', 'auto Environment::Impl::cancel_scalar_load_after_redirect('),
        ]))
        source = Path(cls.directory.name) / 'fixture.cpp'
        source.write_text(text)
        cls.binary = source.with_suffix('')
        built = subprocess.run(['c++', '-std=c++20', '-O0', '-I', str(ROOT / 'cpp'),
                                str(source), '-o', str(cls.binary)], capture_output=True, text=True, timeout=60)
        if built.returncode:
            raise AssertionError(built.stdout + built.stderr)

    def case(self, name):
        run = subprocess.run([str(self.binary), name], capture_output=True, text=True, timeout=15)
        self.assertEqual(run.returncode, 0, run.stdout + run.stderr)
        print('unfinished_cancellation_evidence=' + run.stdout.strip())

    def test_observed_redirect_boundary_and_late_writeback(self):
        self.case('boundaries')

    def test_rob_wrap_and_exact_pending_identity(self):
        self.case('ages')

    def test_unfinished_suffix_requires_explicit_initial_cancellation(self):
        self.case('ledger')

    def test_actual_builder_varies_unfinished_schedules(self):
        self.case('planner')


if __name__ == '__main__':
    unittest.main()
