#include "scenarios/mixed/state.hpp"
#include "scenarios/mixed/vector_indices.hpp"
#include "scenarios/mixed/vector_operands.hpp"
#include <set>
namespace memblock {
#include "environment/scoreboards.inc"
}
using namespace memblock;
using namespace memblock::scenarios;
using namespace memblock::scenarios::mixed;
void require(bool value, const char *why) {
    if (!value) throw std::runtime_error(why);
}
namespace memblock::scenarios::mixed {
class MixedScenario {
public:
    RandomConstraints constraints = RandomConstraints::preset("coverage");
    ConstraintCoverage constraint_coverage;
    MixedCoverage coverage;
    std::mt19937_64 random{17};
    std::uint64_t rob_offset=0, sq_offset=0, lq_offset=0;
    struct {
        SparseMemory image;
        SparseMemory &memory() { return image; }
    } environment;
    std::shared_ptr<const ReferencePageMap> reference_pages_for(std::uint64_t) const { return {}; }
    static unsigned vector_shape_uop_count(const RandomVectorShape &);
    static unsigned vector_shape_flow_num(const RandomVectorShape &);
    bool choose_binary_class(unsigned, const std::array<std::uint64_t,2> &);
    std::vector<VectorMemoryTransaction> make_random_vector_uops(
        bool, std::uint64_t, const RandomVectorShape &, bool);
    VectorMemoryTransaction make_vector(bool, std::uint64_t, unsigned, unsigned,
        VectorAddressingMode);
};
// PRODUCTION_BUILDERS
}

void domains() {
    MixedScenario g;
    g.constraints.vector_masked_per_mille=0;
    g.constraints.vector_nonzero_vstart_per_mille=0;
    g.constraints.vector_partial_vl_per_mille=0;
    g.constraints.vector_vma_per_mille=g.constraints.vector_vta_per_mille=0;
    for(unsigned b=0;b<4096;++b) g.environment.image.write_byte(0x80001000+b,(b*37U+11U)&0xffU);
    unsigned checked=0;
    for(unsigned a=0;a<4;++a) for(unsigned e=0;e<4;++e)
      for(unsigned s=0;s<4;++s) for(int l=-3;l<=3;++l) {
        const int m=l+int(e)-int(s);
        if(l<int(s)-3 || m < -3 || m>3) continue;
        RandomVectorShape shape{a,e,s,l,m,1};
        for(unsigned iteration=0;iteration<8;++iteration) for(bool store:{false,true}) {
            auto uops=g.make_random_vector_uops(store,0x80001000,shape,false);
            std::set<unsigned> used;
            for(unsigned index=0;index<uops.size();++index) {
                const auto &u=uops[index];
                require(store ? u.pdest==0 : (u.pdest>0 && u.pdest<128),"illegal vector destination");
                if(!store) require(used.insert(u.pdest).second,"writing uops share a physical destination");
                if(index && a>=2 && m>l && index%(1U<<unsigned(m-l))) {
                    const auto &prior=uops[index-1U];
                    auto expected=prior.data;
                    if(!store) {
                        const unsigned width=1U<<s;
                        const auto active=active_vector_elements(prior);
                        for(unsigned element=0;element<16U/width;++element) {
                            if(!((active>>element)&1U)) continue;
                            for(unsigned byte=0;byte<width;++byte)
                                expected[element*width+byte]=g.environment.image.read_byte(
                                    vector_byte_address(prior,element,byte));
                        }
                    }
                    require(u.data==expected,"real builder omitted repeated-Vd source construction");
                }
            }
            ++checked;
        }
      }
    for(unsigned i=0;i<2048;++i) {
        auto load=g.make_vector(false,0x80001000,0,0,VectorAddressingMode::unit_stride);
        auto store=g.make_vector(true,0x80001000,0,0,VectorAddressingMode::unit_stride);
        require(load.pdest>0 && load.pdest<128 && store.pdest==0,"ordinary vector factory domain");
        auto vl=random_destination_block(g.random,32,1);
        require(vl>0 && vl<32,"illegal VL destination");
    }
    for(unsigned count:{0U,128U,129U}) {
        bool bad=false;
        try {random_destination_block(g.random,128,count);} catch(const std::invalid_argument &) {bad=true;}
        require(bad,"invalid destination allocation accepted");
    }
    std::cout<<"vector_builder_cases="<<checked<<"\n";
}

void sources() {
    SparseMemory memory;
    for(unsigned b=0;b<64;++b) memory.write_byte(0x80001000+b,0x40+b);
    VectorMemoryTransaction first{
        .address=0x80001000,.addressing=VectorAddressingMode::indexed_unordered,
        .eew=1,.vsew=0,.vl=16,.rob=7,.pdest=41,.flow_num=8,
        .vuop_idx=0,.last_uop=false};
    first.data.fill(0xa5);
    for(unsigned e=0;e<8;++e) first.index[2*e]=e;
    auto second=first;
    second.pdest=42; second.vuop_idx=1; second.last_uop=true;
    second.data.fill(0x5a);
    for(unsigned e=0;e<8;++e) second.index[2*e]=e+8;
    std::vector<VectorMemoryTransaction> uops{first,second};
    require(chain_indexed_vector_sources(uops,memory)==1,"missing load dependency");
    require(vector_load_depends_on_previous(uops[0],uops[1]),"missing same-Vd predecessor");
    for(unsigned b=0;b<16;++b) require(uops[1].data[b]==(b<8 ? 0x40+b : 0xa5),"source did not preserve earlier loaded bytes");
    auto final=memory.expected_vector_load(uops[1]);
    for(unsigned b=0;b<16;++b) require(final[b]==0x40+b,"combined indexed result lost an earlier chunk");
    auto store=uops;
    for(auto &u:store) {u.store=true;u.pdest=0;}
    store[0].data.fill(0x37);store[1].data.fill(0x99);
    require(chain_indexed_vector_sources(store,memory)==1 && store[1].data==store[0].data,"repeated store source differs");
    require(!vector_load_depends_on_previous(store[0],store[1]),"stores gained a false load dependency");
    auto changed=second;changed.rob_flag=true;
    require(!vector_load_depends_on_previous(first,changed),"ROB wrap ignored in predecessor identity");
    changed=second;changed.vuop_idx=2;
    require(!vector_load_depends_on_previous(first,changed),"independent Vd serialized");
    first.vm=false;first.mask_bits=0x5555;first.vstart=2;first.vl=13;
    first.vma=first.vta=true;
    auto producer=vector_load_backend_source(memory,first);
    for(unsigned b=0;b<16;++b) {
        unsigned expected=0xa5;
        if(b>=2 && b<8 && (b%2==0)) expected=0x40+b;
        if(b>=13 || (b>=2 && b<13 && b%2)) expected=0xff;
        require(producer[b]==expected,"mask/tail/prestart producer bytes wrong");
    }
    first.vstart=first.vl;
    require(vector_load_backend_source(memory,first)==first.data,"no-op changed agnostic bytes");
    // A discontinuous mapping still uses declared physical bytes.
    first.vm=true;first.vstart=0;first.vl=16;first.vma=first.vta=false;
    first.address=0x20000ffc;
    first.reference_pages=std::make_shared<const ReferencePageMap>(0x20000000,
        std::vector<std::uint64_t>{0x80000000,0x90000000});
    for(unsigned b=0;b<8;++b) memory.write_byte(b<4?0x80000ffc+b:0x90000000+b-4,0x70+b);
    producer=vector_load_backend_source(memory,first);
    for(unsigned b=0;b<8;++b) require(producer[b]==0x70+b,"source assumed contiguous physical pages");
}

void schedules() {
    std::vector<VectorMemoryTransaction> uops(8);
    for(unsigned i=0;i<uops.size();++i) {
        auto &u=uops[i];u.addressing=VectorAddressingMode::indexed_unordered;
        u.eew=1;u.vsew=0;u.vlmul=2;u.vl=64;u.flow_num=8;
        u.rob=7;u.vuop_idx=i;u.pdest=i+41;
    }
    std::mt19937_64 random(55),replay(55);
    std::set<std::vector<unsigned>> varied;
    for(unsigned sample=0;sample<512;++sample) {
        auto order=vector_uop_issue_order(uops,random);
        require(order==vector_uop_issue_order(uops,replay),"dependency schedule replay changed");
        std::set<unsigned> seen;
        for(auto i:order) {
            require(seen.insert(i).second,"duplicate uop in schedule");
            if(i%2) require(seen.count(i-1),"same-Vd load issued before predecessor");
        }
        require(seen.size()==8,"uop missing from schedule");
        varied.insert(order);
    }
    require(varied.size()>300,"unordered scheduling collapsed to forward/reverse templates");
    for(auto &u:uops)u.addressing=VectorAddressingMode::indexed_ordered;
    require(vector_uop_issue_order(uops,random)==std::vector<unsigned>({0,1,2,3,4,5,6,7}),"ordered indexed uops shuffled");
    std::cout<<"unique_dependency_issue_orders="<<varied.size()<<"/512\n";
}

static generated::VectorMemoryWriteback completion(const VectorMemoryTransaction &t) {
    generated::VectorMemoryWriteback wb;
    wb.valid=true;wb.rob_value=t.rob;wb.rob_flag=t.rob_flag;
    wb.pdest=t.pdest;wb.fu_op_type=vector_fu_op_type(t);wb.vec_wen=!t.store;
    wb.vsew=vector_vsew(t);wb.veew=t.eew;wb.vl=t.vl;wb.vlmul=t.vlmul;
    wb.vuop_idx=t.vuop_idx;wb.nf=t.nf;wb.vd_index_in_field=vector_indexed_vd_index(t);
    wb.data.assign(t.data.begin(),t.data.end());wb.vmask.assign(16,0);
    auto mask=vector_writeback_elements(t);wb.vmask[0]=mask;wb.vmask[1]=mask>>8;
    return wb;
}

// Minimal scheduling shell around the actual transaction-scoped wait method.
// The actual scoreboard still checks the completion before pending changes.
namespace memblock {
struct VectorReplayRequest {
    std::uint64_t cycle = 0;
    unsigned lane=0;bool lq_flag=false;std::uint8_t lq_value=0;
    bool sq_flag=false;std::uint8_t sq_value=0;bool is_part_replay=false;
    std::uint16_t replay_mask=0;std::uint8_t replay_mb_index=0;
};
class FeedbackMonitor {
public:
    std::deque<VectorReplayRequest> vector_replay_requests_;
    std::optional<VectorReplayRequest> take_replay_for(const VectorMemoryTransaction &);
    std::size_t pending_replays() const {return vector_replay_requests_.size();}
    unsigned vector_replay_feedbacks() const {return 0;}
};
// PRODUCTION_REPLAY_MATCH
class CompletionFixture {
public:
    VectorMemoryScoreboard vector_scoreboard_;
    FeedbackMonitor feedback_;
    FailureState error_{"fixture", "WAIT"};
    struct Port {void set(std::uint64_t) {}};
    struct {struct {struct {Port pending_ptr_value,pending_ptr_flag,pendingst;} backend;} queues;} dut_;
    struct Agent {unsigned request_count() const {return 0;}} ptw_agent_,memory_agent_;
    VectorMemoryTransaction completing;
    unsigned clock=0;
    unsigned cycle() const {return clock;}
    void tick() {if(clock++==0) vector_scoreboard_.observe(0,completion(completing));}
    bool check_components() {return vector_scoreboard_.ok() && error_.empty();}
    bool issue_vector(const VectorMemoryTransaction &,unsigned) {return true;}
    bool run_cycles(unsigned n) {for(unsigned i=0;i<n;++i)tick();return check_components();}
    bool pulse_store_commit_keep_pending(std::uint8_t,bool) {return true;}
    bool pulse_pending_store(std::uint8_t,bool) {return true;}
    bool run_until_vector_complete_with_replays(const VectorMemoryTransaction &,unsigned,bool=false,bool=false);
};
// PRODUCTION_WAIT
}

void noops() {
    unsigned cases=0;
    for(unsigned addressing=0;addressing<4;++addressing)
      for(unsigned width=0;width<4;++width)
      for(unsigned policy=0;policy<4;++policy)
      for(const auto &[vl,start]:std::array<std::pair<unsigned,unsigned>,3>{{{0,0},{1,1},{1,2}}}) {
        VectorMemoryTransaction t{
            .address=0x80001000,.addressing=static_cast<VectorAddressingMode>(addressing),
            .eew=static_cast<std::uint8_t>(width),.vsew=static_cast<std::uint8_t>(width),
            .vl=static_cast<std::uint8_t>(vl),.vstart=static_cast<std::uint8_t>(start),
            .vm=false,.mask_bits=0,.vma=bool(policy&1),.vta=bool(policy&2),
            .rob=7,.pdest=41,.flow_num=static_cast<std::uint8_t>(16U>>width)};
        for(unsigned byte=0;byte<16;++byte)t.data[byte]=0x40+byte;
        auto wb=completion(t);
        VectorMemoryScoreboard baseline;baseline.expect(t,t.data);baseline.observe(0,wb);
        require(baseline.done() && baseline.ok(),"no-op preserved result rejected");
        const unsigned element_bytes=1U<<width;
        for(unsigned element=0;element<16U/element_bytes;++element) {
            auto corrupt=wb;
            std::fill_n(corrupt.data.begin()+element*element_bytes,element_bytes,0xff);
            VectorMemoryScoreboard bad;bad.expect(t,t.data);bad.observe(0,corrupt);
            require(!bad.ok(),"no-op destination corruption accepted as agnostic data");
        }
        ++cases;
      }
    std::cout<<"noop_scoreboard_cases="<<cases<<"\n";
}

void waits() {
    CompletionFixture env;
    VectorMemoryTransaction first{.address=0x80000000,.eew=0,.vl=16,.rob=7,.pdest=41,.flow_num=2,.last_uop=false};
    auto next=first;next.vuop_idx=1;next.pdest=42;
    env.completing=first;
    env.vector_scoreboard_.expect(first,first.data);env.vector_scoreboard_.expect(next,next.data);
    require(env.vector_scoreboard_.pending(first) && env.vector_scoreboard_.pending(next),"expectations missing");
    require(env.run_until_vector_complete_with_replays(first,8),"predecessor waited for an unissued younger uop");
    require(!env.vector_scoreboard_.pending(first) && env.vector_scoreboard_.pending(next),"wait consumed the wrong obligation");
    require(!env.vector_scoreboard_.done(),"unrelated vector expectation dropped");
    auto wrong=next;wrong.rob_flag=true;
    require(!env.vector_scoreboard_.pending(wrong),"pending ignores ROB wrap");
    wrong=next;wrong.pdest=43;
    require(!env.vector_scoreboard_.pending(wrong),"pending ignores destination");
    FeedbackMonitor feedback;
    feedback.vector_replay_requests_.push_back({.lane=1,.lq_value=3,.sq_value=7,.is_part_replay=true,.replay_mask=5,.replay_mb_index=2});
    VectorMemoryTransaction load{.rob=8,.lq=3,.sq=7,.flow_num=2};
    require(!feedback.take_replay_for(load) && feedback.pending_replays()==1,"store feedback matched an LQ snapshot");
    auto store=load;store.store=true;store.sq=6;
    auto replay=feedback.take_replay_for(store);
    require(replay && replay->replay_mask==5 && feedback.pending_replays()==0,"store replay identity/fields lost");
}

int main(int argc,char **argv) {
    try {
        std::string mode=argc>1?argv[1]:"domains";
        if(mode=="domains")domains();else if(mode=="sources")sources();else if(mode=="waits")waits();else if(mode=="schedules")schedules();else if(mode=="noops")noops();
        else throw std::runtime_error("unknown fixture");
        return 0;
    }catch(const std::exception &e) {std::cerr<<e.what()<<'\n';return 1;}
}
