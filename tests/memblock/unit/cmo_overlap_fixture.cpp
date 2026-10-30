#include "memblock_env.hpp"
#include "scenarios/shared.hpp"
#include <iostream>
#include <map>
namespace memblock {
#include "environment/scoreboards.inc"
}
using namespace memblock;
using memblock::scenarios::RandomConstraints;
static void require(bool v,const char *s){if(!v)throw std::runtime_error(s);}
struct Host {
    SparseMemory mem;
    enum Timing { before, during, pending } timing;
    unsigned fault=0;
    std::optional<LoadTransaction> role;
    LoadScoreboard scoreboard;
    LoadTransaction last;
    std::uint64_t wb=0,swb=0,lc=0,sc=0,canceled=0,req=0,cmo=0,errors=0,denied=0,corrupt=0;
    std::uint64_t probes=0,presponses=0,pdata=0,last_cmo=0;
    bool store_pending=false,is_cmo=false,error_on=false,error_denied=false,redirected=false,consumed=false;
    Host(Timing t,unsigned f):timing(t),fault(f){}
    auto &memory(){return mem;}
    void complete() {
        require(role.has_value(),"early CMO-overlap completion has no expectation");
        generated::ScalarLoadWriteback result;
        result.valid=true;result.rob_value=role->rob;result.rob_flag=role->rob_flag;result.pdest=role->pdest;
        result.rf_wen=true;result.data=mem.expected_load(*role) ^ (fault==1);
        scoreboard.observe(role->lane,result);
        if(fault==2)scoreboard.observe(role->lane,result);
        require(scoreboard.ok(),scoreboard.error().c_str());
        role.reset();++wb;
    }
    void expect_load(const LoadTransaction&t){require(!role,"duplicate load expectation");role=t;last=t;scoreboard.expect(t,mem.expected_load(t));}
    void expect_store(const StoreTransaction&t){is_cmo=unsigned(t.op)>=12;store_pending=true;last_cmo=t.address&~63ULL;}
    bool set_rob_head(unsigned,bool){return true;}
    bool enqueue_load(const LoadTransaction&){return true;}
    bool enqueue_store(const StoreTransaction&){return true;}
    bool issue_load(const LoadTransaction&,unsigned){++req;if(!is_cmo)complete();return true;}
    bool issue_store_address_until_tlb_hit(const StoreTransaction&,unsigned){return true;}
    bool issue_store_data(const StoreTransaction&,unsigned){if(is_cmo){++cmo;if(error_on){++errors;denied+=error_denied;corrupt+=!error_denied;}}return true;}
    bool run_until_all_complete(unsigned){return true;}
    bool run_until_complete(unsigned){return true;}
    bool run_until_lq_retired(unsigned){return true;}
    bool run_until_queues_retired(unsigned){return true;}
    bool run_until_sbuffer_empty(unsigned){return true;}
    bool run_until_dcache_idle(unsigned){return true;}
    bool run_until_dcache_requests(std::uint64_t,unsigned){if(timing==before)complete();return true;}
    bool run_until_store_complete(unsigned){store_pending=false;++swb;return true;}
    bool run_until_probe_responses(std::uint64_t n,unsigned){presponses=n;return true;}
    bool wait_for_cmo_store_request(const StoreTransaction&,std::uint64_t,unsigned){return true;}
    bool commit_store(const StoreTransaction&,unsigned){return true;}
    bool redirect_after(unsigned,bool,bool self){require(!redirected,"duplicate redirect");redirected=true;if(timing==during)complete();if(timing==pending)++lc;if(self)++sc;return true;}
    bool cancel_pending_for_redirect(unsigned,bool,bool){require(redirected&&!consumed,"terminal cancellation before exact return or duplicate");consumed=true;if(role){scoreboard.cancel_pending_for_redirect(role->rob,role->rob_flag,true);role.reset();++canceled;}return true;}
    bool run_cycles(unsigned){if(redirected&&fault==3){
        generated::ScalarLoadWriteback result;result.valid=true;result.rob_value=last.rob;result.rob_flag=last.rob_flag;
        result.pdest=last.pdest;result.rf_wen=true;result.data=mem.expected_load(last);
        scoreboard.observe(last.lane,result);require(scoreboard.ok(),scoreboard.error().c_str());}return true;}
    bool scalar_load_pending(const LoadTransaction&){return role.has_value();}
    std::uint64_t scalar_load_redirect_canceled(){return canceled;}
    std::uint64_t lq_redirect_canceled_observed(){return lc;}
    std::uint64_t sq_redirect_canceled_observed(){return sc;}
    std::uint64_t tilelink_requests(){return req;}
    std::uint64_t writebacks(){return wb;}
    std::uint64_t store_writebacks(){return swb;}
    unsigned pending_scalar_stores(){return store_pending;}
    bool sbuffer_empty(){return true;}
    void force_next_cmo_response_delay(unsigned){}
    void force_next_dcache_response_delay(unsigned){}
    void inject_dcache_cmo_response_error_at(std::uint64_t,bool d,bool){error_on=true;error_denied=d;}
    bool hold_dcache_c_until_probes(std::uint64_t){return true;}
    bool request_dcache_probe(std::uint64_t,std::uint8_t,bool,std::uint8_t,const std::vector<unsigned char>& image = {}){++probes;return true;}
    void discard_final_observation_range(std::uint64_t,unsigned){}
    std::uint64_t bus_expected_load(std::uint64_t,LoadOp){return 0;}
    std::uint64_t dcache_cmo_requests(StoreOp){return cmo;}
    std::uint64_t dcache_last_cmo_address(){return last_cmo;}
    unsigned dcache_last_cmo_source(){return kDcacheCmoSource;}
    std::uint64_t dcache_probe_responses(){return presponses;}
    std::uint64_t dcache_probe_data(){return pdata;}
    std::uint64_t dcache_probes(){return probes;}
    unsigned dcache_max_probe_outstanding(){return probes;}
    std::uint64_t dcache_error_response_requests(){return errors;}
    std::uint64_t dcache_denied_d_beats(){return denied;}
    std::uint64_t dcache_corrupt_d_beats(){return corrupt;}
};
struct MixedScenario {
    Host environment;
    std::mt19937_64 random;
    RandomConstraints constraints=RandomConstraints::preset("spec");
    std::string phase;
    unsigned constrained_completion_timeout=2048;
    std::array<std::uint64_t,3> cmo_locality_lines{};
    static constexpr std::uint64_t cmo_base=0x90000000,cmo_auxiliary_probe_span=0x10000,cmo_auxiliary_probe_base=0x92000000;
    std::uint64_t cmo_auxiliary_probe_group=0,cmo_overlap_line=0;
    std::uint64_t rob_offset=1,lq_offset=0,sq_offset=0;
    struct {std::array<unsigned,3> locality{};} constraint_coverage;
    MixedScenario(unsigned seed,Host::Timing t,unsigned f=0):environment(t,f),random(seed){}
    LoadTransaction make_load(std::uint64_t a,LoadOp op,unsigned lane){LoadTransaction t;t.address=a;t.op=op;t.lane=lane;t.rob=rob_pointer_value(rob_offset++);t.lq=lq_pointer_value(lq_offset++);return t;}
    StoreTransaction make_store(std::uint64_t a,std::uint64_t d,StoreOp op,unsigned lane){StoreTransaction t;t.address=a;t.data=d;t.op=op;t.address_lane=lane;t.rob=rob_pointer_value(rob_offset++);t.sq=sq_pointer_value(sq_offset++);return t;}
    bool issue_random_cmo(unsigned,bool,bool,std::optional<bool>,unsigned);
};
// PRODUCTION_CMO
int main(int argc,char**argv){try{
    unsigned count=0;const unsigned fault=argc>1?std::stoul(argv[1]):0;
    for(unsigned seed=0;seed<64;++seed)for(auto timing:{Host::before,Host::during,Host::pending})for(int error:{-1,0,1}){
        MixedScenario f(seed,timing,fault);const auto option=error<0?std::optional<bool>{}:std::optional<bool>{bool(error)};
        require(f.issue_random_cmo(seed%3,false,true,option,0),f.phase.c_str());
        require(f.environment.consumed&&!f.environment.role,"CMO overlap lost terminal ownership");
        require(f.environment.wb==unsigned(timing==Host::pending?1:2)&&f.environment.canceled==unsigned(timing==Host::pending),"CMO younger terminal not exactly checked or canceled");
        require(f.lq_offset==unsigned(timing==Host::pending?1:2),"CMO rewound completed physical allocation");++count;
    }
    std::cout<<"cmo_overlap_histories="<<count<<" early=192 during_return=192 pending_cancel=192\n";return 0;
}catch(const std::exception&e){std::cerr<<e.what()<<'\n';return 1;}}
