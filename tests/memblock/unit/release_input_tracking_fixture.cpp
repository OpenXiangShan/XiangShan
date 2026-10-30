#include "environment/retained_issue.hpp"
#include <functional>
#include <map>
#include <tuple>

// Host-only surroundings for extracted production methods. The manager records
// input groups; it supplies no bytes, age, issue acceptance, or expected result.
static void require(bool value, const char *reason) {
    if (!value) throw std::runtime_error(reason);
}
namespace memblock {
struct TrackingPort {
    std::uint64_t contents = 0;
    void set(std::uint64_t value) { contents = value; }
    auto value() const { return contents; }
    bool boolean() const { return contents != 0; }
};
struct TrackingDut {
    struct { struct { TrackingPort pending_ptr_value, pending_ptr_flag, pendingst, scommit; } backend;
        struct { TrackingPort valid, rob_idx_value, rob_idx_flag, level; } redirect; } queues;
    struct { struct { TrackingPort valid; } ifetch_request;
        struct { TrackingPort ready; } ifetch_response; } translation;
    struct { TrackingPort external, backend; } reset;
    std::deque<bool> sta_ready, std_ready, vec_ready;
    bool sta_valid = false, std_valid = false, vec_valid = false;
    std::function<void()> at_edge;
    static bool ready(const std::deque<bool> &values) { return values.empty() || values.front(); }
    void refresh_comb() {}
    void drive_scalar_load_issue(unsigned, const generated::ScalarLoadIssue &) { throw std::logic_error("unexpected load binding"); }
    bool scalar_load_issue_ready(unsigned) const { throw std::logic_error("unexpected load binding"); }
    void clear_scalar_load_issue_valids() {}
    void drive_scalar_store_address(unsigned, const generated::ScalarStoreIssue &) { sta_valid = true; }
    void drive_scalar_store_data(unsigned, const generated::ScalarStoreIssue &) { std_valid = true; }
    void drive_vector_memory_issue(unsigned, const generated::VectorMemoryIssue &) { vec_valid = true; }
    bool scalar_store_address_ready(unsigned) const { return ready(sta_ready); }
    bool scalar_store_data_ready(unsigned) const { return ready(std_ready); }
    bool vector_memory_issue_ready(unsigned) const { return ready(vec_ready); }
    void clear_scalar_store_issue_valids() { sta_valid = std_valid = false; }
    void clear_vector_memory_issue_valids() { vec_valid = false; }
    void step() {
        if (at_edge) at_edge();
        for (auto pair : {std::pair{sta_valid,&sta_ready}, {std_valid,&std_ready}, {vec_valid,&vec_ready}})
            if (pair.first && !pair.second->empty()) pair.second->pop_front();
    }
};
struct TrackingAgent {
    template<class... T> void drive(T &&...) {}
    template<class... T> void capture_before_tick(T &&...) {}
    template<class... T> void sample_before_edge(T &&...) {}
    template<class... T> void sample_after_edge(T &&...) {}
    void update_after_tick() {}
    void reset_link_state() {}
    void reset_observations() {}
    void capture_inputs() {}
    void check_before_edge() {}
    void advance_after_edge() {}
};
struct TrackingManager : TrackingAgent {
    struct Group {
        std::uint64_t physical, virtual_address;
        std::vector<unsigned char> bytes;
        bool authorized = false;
    };
    std::map<std::uint64_t, Group> groups;
    unsigned registrations = 0, authorizations = 0, legacy_stores = 0, legacy_bytes = 0, resets = 0;
    std::vector<std::string> *events = nullptr;
    FailureState failures_{"recording-manager", "contract"};
    unsigned fail_register_at = 0;
    bool fail_authorize = false;
    const FailureState &failures() const { return failures_; }
    bool register_release_store(std::uint64_t order, std::uint64_t physical,
        std::uint64_t virtual_address, const std::vector<unsigned char> &bytes) {
        if (fail_register_at && registrations + 1 == fail_register_at) { failures_.record("registered input rejected by fixture manager"); return false; }
        require(!groups.contains(order), "duplicate input registration");
        groups.emplace(order, Group{physical,virtual_address,bytes}); ++registrations;
        events->push_back("register"); return true;
    }
    bool authorize_release_store(std::uint64_t order) {
        if (fail_authorize) { failures_.record("authorized input rejected by fixture manager"); return false; }
        require(groups.contains(order), "authorization before registration");
        require(!groups.at(order).authorized, "duplicate input authorization");
        groups.at(order).authorized = true; ++authorizations;
        events->push_back("authorize"); return true;
    }
    void note_reference_store(std::uint64_t, std::uint64_t, const std::vector<unsigned char> &) { ++legacy_stores; }
    void note_reference_write(std::uint64_t, std::uint64_t, unsigned char) { ++legacy_bytes; }
    void drive(TrackingDut &) { events->push_back("manager-drive"); }
    void reset_link_state() { groups.clear(); ++resets; }
};
struct TrackingQueue : TrackingAgent {
    Environment::StoreRetirementStats stats;
    const Environment::StoreRetirementStats &store_retirement_stats() const { return stats; }
    std::map<std::uint64_t,std::uint64_t> scalar, vector;
    auto scalar_store_target(std::uint64_t key) const -> std::optional<std::uint64_t> {
        auto it=scalar.find(key); return it==scalar.end()?std::nullopt:std::optional{it->second};
    }
    auto vector_store_target(std::uint64_t key) const -> std::optional<std::uint64_t> {
        auto it=vector.find(key); return it==vector.end()?std::nullopt:std::optional{it->second};
    }
    bool valid_store_retirement_target(std::uint64_t value) const { return value != 0; }
    bool retire_scalar_store(std::uint64_t key) { return scalar.erase(key) == 1; }
    bool retire_vector_store(std::uint64_t key) { return vector.erase(key) == 1; }
    bool complete_vector_store_group(const std::set<std::uint64_t> &keys) const {
        return keys.size()==vector.size() && std::all_of(keys.begin(),keys.end(),[&](auto key){return vector.contains(key);});
    }
};
struct TrackingScoreboard {
    bool incomplete = false;
    template<class T> bool pending(const T &) const { return incomplete; }
    bool mark_address_issued(const StoreTransaction &, std::uint64_t) { return true; }
    bool mark_data_issued(const StoreTransaction &, unsigned, std::uint64_t) { return true; }
};
void trace_vector_issue(const VectorMemoryTransaction &) {}
void trace_store_issue(const char *, const StoreTransaction &) {}
generated::VectorMemoryIssue make_vector_memory_issue(const VectorMemoryTransaction &) { return {}; }
void trace_reference_store(const SparseMemory &, const char *, std::uint64_t, unsigned,
    std::uint64_t,unsigned,bool,unsigned,bool,const char *) {}

class ReleaseInputFixture {
public:
    using StoreOperandAcceptances = Environment::StoreOperandAcceptances;
    FailureState error_{"host-release-inputs", "contract"};
    TrackingDut dut_;
    RetainedIssueDriver *retained_issue_driver_ = nullptr;
    TrackingManager memory_agent_;
    TrackingAgent ptw_agent_, uncache_agent_, bridge_, feedback_;
    TrackingQueue queues_;
    TrackingScoreboard scoreboard_, store_scoreboard_, vector_scoreboard_;
    SparseMemory memory_;
    std::optional<std::uint64_t> last_reset_completed_cycle_;
    bool last_load_vector_pair_same_cycle_ = false;
    struct RedirectBoundary { unsigned rob; bool flag,flush_itself; std::uint64_t cycle,remaining,sq_allocation_limit; };
    std::optional<RedirectBoundary> scalar_redirect_boundary_;
    unsigned ifetch_ptw_pending_ = 0;
    // PRODUCTION_TRACKING_STRUCTS
    std::vector<std::string> events;
    std::function<void(ReleaseInputFixture &)> inspect_edge;
    std::uint64_t edges = 0;
    ReleaseInputFixture() {
        memory_agent_.events=&events;
        dut_.at_edge=[this] { events.push_back("edge"); if(inspect_edge) inspect_edge(*this); ++edges; };
    }
    std::uint64_t cycle() const { return edges; }
    void set_failure_cycle() {}
    bool check_components() const { return error_.empty(); }
    void head(unsigned rob,bool flag=false,bool pending=false) {
        dut_.queues.backend.pending_ptr_value.set(rob);
        dut_.queues.backend.pending_ptr_flag.set(flag);
        dut_.queues.backend.pendingst.set(pending);
    }
    void allocate(const StoreTransaction &t,std::uint64_t target) { queues_.scalar.emplace(scalar_store_key(t),target); }
    void allocate(const VectorMemoryTransaction &t,std::uint64_t target) { queues_.vector.emplace(vector_store_key(t),target); }
    void track_final_observation_bytes(std::uint64_t,unsigned,bool,bool) {}
    void discard_final_observation_range(std::uint64_t,unsigned) {}
    static std::uint64_t scalar_store_key(const StoreTransaction &);
    static std::uint64_t vector_store_key(const VectorMemoryTransaction &);
    bool track_store_release_inputs(const StoreTransaction &);
    bool track_vector_release_inputs(const VectorMemoryTransaction &);
    bool accept_release_store_input(std::uint64_t,bool,bool);
    bool authorize_release_store_inputs();
    bool issue_store_address(const StoreTransaction &,unsigned=10);
    bool issue_store_data(const StoreTransaction &,unsigned=10);
    bool issue_store_operands(const StoreTransaction &,StoreOperandAcceptances &,unsigned=10);
    bool issue_vector(const VectorMemoryTransaction &,unsigned=10);
    void tick(bool=true);
    bool reset();
    bool retire_store(const StoreTransaction &);
    bool retire_vector_stores(const std::vector<VectorMemoryTransaction> &);
    bool commit_stores_through(const StoreTransaction &,unsigned);
    void record_committed_store(const StoreTransaction &);
    void record_committed_vector_store(const VectorMemoryTransaction &);
};
// PRODUCTION_METHODS
} // namespace memblock
using namespace memblock;

static StoreTransaction scalar(unsigned rob=7,unsigned sq=0) {
    StoreTransaction t; t.address=0x80002000; t.data=0x8877665544332211ULL; t.rob=rob; t.sq=sq; return t;
}
static VectorMemoryTransaction vector(unsigned rob=20,unsigned sq=1) {
    VectorMemoryTransaction t; t.store=true; t.address=0x80002000; t.rob=rob; t.sq=sq;
    t.eew=1; t.vl=8; t.vstart=1; t.vm=false; t.mask_bits=0x2a; t.flow_num=1;
    for(unsigned i=0;i<16;++i)t.data[i]=0xa0+i;
    return t;
}
static void group(const ReleaseInputFixture &e,std::uint64_t order,std::uint64_t pa,
    std::uint64_t va,std::initializer_list<unsigned char> bytes,bool authorized) {
    require(e.memory_agent_.groups.contains(order),"literal group order missing");
    const auto &g=e.memory_agent_.groups.at(order);
    require(g.physical==pa && g.virtual_address==va,"literal mapped group address mismatch");
    require(g.bytes==std::vector<unsigned char>(bytes),"literal grouped byte mismatch");
    require(g.authorized==authorized,"public head authorization mismatch");
}
static void scalar_orders(bool data_first) {
    ReleaseInputFixture e; auto t=scalar(); e.allocate(t,13); e.head(7,false,false);
    e.memory_.write_reference_byte(t.address,0x5a);
    require(e.track_store_release_inputs(t),"scalar tracking rejected");
    unsigned step=0;
    e.inspect_edge=[&](auto &f) {
        if(step++==0) require(f.memory_agent_.groups.empty(),"registered before both accepted inputs");
        else {
            require(f.memory_agent_.registrations==1,"registration missed second accepted input edge");
            require(f.memory_agent_.authorizations==1,"authorization missed accepted input edge");
            group(f,13312,0x80002000,0x80002000,{0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88},true);
        }
        require(f.memory_.read_byte(t.address)==0x5a,"input tracking changed architectural reference early");
    };
    require(data_first?e.issue_store_data(t):e.issue_store_address(t),"first scalar issue rejected");
    require(data_first?e.issue_store_address(t):e.issue_store_data(t),"second scalar issue rejected");
    require(e.events==std::vector<std::string>{"manager-drive","edge","manager-drive","register","authorize","edge"},"before-edge registration/authorization order changed");
}
static void pressure_replay() {
    ReleaseInputFixture e; auto t=scalar(); e.allocate(t,1); e.head(7);
    e.dut_.sta_ready={false,false,true}; e.dut_.std_ready={false,true};
    require(e.track_store_release_inputs(t),"pressure setup rejected");
    e.inspect_edge=[&](auto &f) {
        if(f.edges<4) require(f.memory_agent_.groups.empty(),"backpressure registered unaccepted input");
        else require(f.memory_agent_.registrations==1,"accepted STD edge lacked registration");
    };
    require(e.issue_store_address(t) && e.issue_store_data(t),"backpressured issue rejected");
    e.inspect_edge={};
    require(e.issue_store_address(t) && e.issue_store_data(t),"reissued scalar input rejected");
    e.tick(); require(e.memory_agent_.registrations==1 && e.memory_agent_.authorizations==1,"replay duplicated manager group");
}
static void younger_head_flags() {
    ReleaseInputFixture e; auto older=scalar(255,62), younger=scalar(0,0); younger.rob_flag=true; younger.sq_flag=true;
    auto v=vector(0,1); v.rob_flag=true; v.sq_flag=true; v.vstart=0;
    e.allocate(older,64); e.allocate(younger,65); e.allocate(v,66); e.head(255);
    require(e.track_store_release_inputs(older)&&e.track_store_release_inputs(younger)&&e.track_vector_release_inputs(v),"head setup rejected");
    require(e.issue_store_address(younger)&&e.issue_store_data(younger)&&e.issue_vector(v),"younger input rejected");
    require(e.memory_agent_.authorizations==0,"younger input authorized before matching head");
    e.head(0,false); e.tick(); require(e.memory_agent_.authorizations==0,"ROB flag mismatch authorized input");
    e.head(1,true); e.tick(); require(e.memory_agent_.authorizations==0,"nonmatching younger head authorized input");
    e.head(0,true,false); e.tick();
    require(e.memory_agent_.authorizations==4,"matching low-pendingst head did not authorize groups");
    group(e,66560,0x80002000,0x80002000,{0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88},true);
    group(e,67616,0x80002002,0x80002002,{0xa2,0xa3},true);
    e.head(12,false,true); e.tick(); e.head(0,true,false); e.tick();
    require(e.memory_agent_.authorizations==4,"head revisit duplicated authorization");
}
static void scalar_grouping() {
    for(auto [op,width]:{std::pair{StoreOp::sb,1U},{StoreOp::sh,2U},{StoreOp::sw,4U},{StoreOp::sd,8U}}) {
        ReleaseInputFixture e; auto t=scalar(); t.op=op; e.allocate(t,17); e.head(7);
        require(e.track_store_release_inputs(t)&&e.issue_store_data(t)&&e.issue_store_address(t),"aligned scalar setup rejected");
        require(e.memory_agent_.groups.size()==1,"aligned scalar was split into byte groups");
        const auto &g=e.memory_agent_.groups.at(17408);
        require(g.bytes.size()==width,"scalar width mismatch");
        for(unsigned i=0;i<width;++i)require(g.bytes[i]==0x11*(i+1),"scalar little-endian byte mismatch");
    }
}
static void scalar_pages() {
    ReleaseInputFixture e; auto t=scalar(); t.address=0x80001ffd;
    t.reference_pages=std::make_shared<ReferencePageMap>(0x80001000,std::vector<std::uint64_t>{0x90005000,0xa0003000});
    e.allocate(t,9); e.head(7);
    require(e.track_store_release_inputs(t)&&e.issue_store_address(t)&&e.issue_store_data(t),"page-split scalar setup rejected");
    require(e.memory_agent_.groups.size()==8,"misaligned scalar byte grouping missing");
    group(e,9216,0x90005ffd,0x80001ffd,{0x11},true);
    group(e,9218,0x90005fff,0x80001fff,{0x33},true);
    group(e,9219,0xa0003000,0x80002000,{0x44},true);
    group(e,9223,0xa0003004,0x80002004,{0x88},true);
}
static void vector_mask_replay() {
    ReleaseInputFixture e; auto t=vector(); e.allocate(t,21); e.head(20,false,false);
    require(e.track_vector_release_inputs(t),"vector tracking rejected");
    e.inspect_edge=[&](auto &f) { require(f.memory_agent_.registrations==3,"vector registration missed accepted input edge"); };
    require(e.issue_vector(t),"vector issue rejected");
    group(e,21536,0x80002002,0x80002002,{0xa2,0xa3},true);
    group(e,21600,0x80002006,0x80002006,{0xa6,0xa7},true);
    group(e,21664,0x8000200a,0x8000200a,{0xaa,0xab},true);
    auto replay=t; replay.is_part_replay=true; replay.replay_mask=0x2;
    require(e.issue_vector(replay),"vector replay rejected"); e.tick();
    require(e.memory_agent_.registrations==3 && e.memory_agent_.authorizations==3,"vector replay duplicated group");
}
static void vector_addresses() {
    ReleaseInputFixture e; auto t=vector(); t.eew=2; t.vm=true; t.vstart=0; t.vl=4;
    t.address=0x80001ffc; t.stride=8; t.addressing=VectorAddressingMode::strided;
    t.reference_pages=std::make_shared<ReferencePageMap>(0x80001000,std::vector<std::uint64_t>{0x90005000,0xa0003000});
    e.allocate(t,33); e.head(20);
    require(e.track_vector_release_inputs(t)&&e.issue_vector(t),"strided vector setup rejected");
    require(e.memory_agent_.groups.size()==4,"aligned vector element grouping mismatch");
    group(e,33792,0x90005ffc,0x80001ffc,{0xa0,0xa1,0xa2,0xa3},true);
    group(e,33824,0xa0003004,0x80002004,{0xa4,0xa5,0xa6,0xa7},true);
    group(e,33888,0xa0003014,0x80002014,{0xac,0xad,0xae,0xaf},true);
    ReleaseInputFixture indexed; t.reference_pages.reset(); t.address=0x80002000;
    t.eew=1; t.vsew=1; t.vl=4; t.flow_num=4; t.addressing=VectorAddressingMode::indexed_ordered;
    t.index={8,0,2,0,8,0,6,0}; indexed.allocate(t,34); indexed.head(20);
    require(indexed.track_vector_release_inputs(t)&&indexed.issue_vector(t),"indexed vector setup rejected");
    group(indexed,34816,0x80002008,0x80002008,{0xa0,0xa1},true);
    group(indexed,34848,0x80002002,0x80002002,{0xa2,0xa3},true);
    group(indexed,34880,0x80002008,0x80002008,{0xa4,0xa5},true);
    group(indexed,34912,0x80002006,0x80002006,{0xa6,0xa7},true);
}
static void vector_misaligned_inactive() {
    ReleaseInputFixture e; auto t=vector(); t.address=0x80002001; t.vl=2; t.vstart=0; t.vm=true;
    e.allocate(t,4); e.head(20);
    require(e.track_vector_release_inputs(t)&&e.issue_vector(t),"unaligned vector setup rejected");
    require(e.memory_agent_.groups.size()==4,"misaligned vector element byte splitting missing");
    group(e,4096,0x80002001,0x80002001,{0xa0},true);
    group(e,4097,0x80002002,0x80002002,{0xa1},true);
    group(e,4128,0x80002003,0x80002003,{0xa2},true);
    ReleaseInputFixture inactive; t.vstart=t.vl; inactive.allocate(t,5); inactive.head(20);
    require(inactive.track_vector_release_inputs(t)&&inactive.issue_vector(t),"inactive vector rejected");
    auto key=ReleaseInputFixture::vector_store_key(t);
    require(inactive.vector_release_inputs_.at(key).registered && inactive.vector_release_inputs_.at(key).authorized,"inactive vector lifetime not advanced");
    require(inactive.memory_agent_.groups.empty(),"inactive vector registered bytes");
}
static void retirement_keeps_groups() {
    ReleaseInputFixture e; auto t=scalar(); t.reference_pages=std::make_shared<ReferencePageMap>(0x80002000,std::vector<std::uint64_t>{0x90005000});
    e.allocate(t,6); e.head(7,false,true); e.memory_.write_reference_byte(0x90005000,0x5a);
    require(e.track_store_release_inputs(t)&&e.issue_store_address(t)&&e.issue_store_data(t),"tracked retirement setup rejected");
    require(e.memory_.read_byte(0x90005000)!=0x11,"reference unexpectedly already committed");
    require(e.retire_store(t),"tracked scalar retirement rejected");
    require(e.scalar_release_inputs_.empty(),"retired scalar tracker leaked");
    group(e,6144,0x90005000,0x80002000,{0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88},true);
    require(e.memory_.read_byte(0x90005000)==0x11,"retirement reference transition missing");
    require(e.memory_agent_.legacy_stores==0 && e.memory_agent_.legacy_bytes==0,"tracked scalar retirement duplicated legacy notification");
    ReleaseInputFixture v; auto u=vector(); u.vstart=0; u.vm=true; u.vl=2;
    u.reference_pages=t.reference_pages; v.allocate(u,7); v.head(20,false,true);
    require(v.track_vector_release_inputs(u)&&v.issue_vector(u)&&v.retire_vector_stores({u}),"tracked vector retirement rejected");
    require(v.vector_release_inputs_.empty() && v.memory_agent_.groups.size()==2,"vector retirement lost persistent manager groups");
    require(v.memory_agent_.legacy_stores==0 && v.memory_agent_.legacy_bytes==0,"tracked vector retirement duplicated legacy notification");
    require(v.memory_.read_byte(0x90005000)==0xa0,"vector reference transition missing");
}
static void identity_reuse_absolute_order() {
    ReleaseInputFixture e; auto t=scalar(); e.allocate(t,1); e.head(7,false,true);
    require(e.track_store_release_inputs(t)&&e.issue_store_address(t)&&e.issue_store_data(t)&&e.retire_store(t),"old identity retirement rejected");
    // The same ROB/SQ value+flag recurs after 512 scalar-store allocations.
    // The target provider supplies that absolute history; this is not an RTL run.
    t.data=0x0807060504030201ULL; e.allocate(t,513);
    require(e.track_store_release_inputs(t)&&e.issue_store_data(t)&&e.issue_store_address(t),"reused identity tracking rejected");
    require(e.memory_agent_.groups.size()==2,"absolute target reuse collided with old manager group");
    group(e,1024,0x80002000,0x80002000,{0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88},true);
    group(e,525312,0x80002000,0x80002000,{1,2,3,4,5,6,7,8},true);
    require(e.memory_.read_byte(0x80002000)==0x11,"reused input changed reference before retirement");
    require(e.retire_store(t)&&e.memory_.read_byte(0x80002000)==1,"reused identity retirement did not update reference");
    require(e.memory_agent_.registrations==2&&e.memory_agent_.authorizations==2,"reused identity lifetime counts mismatch");
}
static void vector_invalid_tracking() {
    for(unsigned invalid=0;invalid<8;++invalid) {
        ReleaseInputFixture e; auto t=vector(); if(invalid)e.allocate(t,1);
        if(invalid==1)t.store=false; if(invalid==2)t.segment=true;
        if(invalid==3)t.cacheable=false; if(invalid==4)t.input_exception_mask=1;
        if(invalid==5)t.allowed_additional_exception_mask=1;
        if(invalid==6)t.exception_may_defer=true; if(invalid==7)t.check_data=false;
        require(!e.track_vector_release_inputs(t)&&e.vector_release_inputs_.empty(),"invalid vector tracker accepted");
        require(e.memory_agent_.groups.empty(),"invalid vector tracking registered manager data");
    }
}
static void manager_failures() {
    ReleaseInputFixture registration; auto t=scalar(); registration.allocate(t,1); registration.head(7);
    require(registration.track_store_release_inputs(t)&&registration.issue_store_address(t),"registration failure setup rejected");
    registration.memory_agent_.fail_register_at=1;
    require(!registration.issue_store_data(t),"manager registration rejection did not stop issue");
    require(!registration.error_.empty(),"manager registration failure lost error provenance");
    require(registration.edges==1,"manager registration failure advanced accepted edge");
    ReleaseInputFixture authorization; auto u=vector(); authorization.allocate(u,2); authorization.head(20);
    require(authorization.track_vector_release_inputs(u),"authorization failure setup rejected");
    authorization.memory_agent_.fail_authorize=true;
    require(!authorization.issue_vector(u),"manager authorization rejection did not stop issue");
    require(!authorization.error_.empty(),"manager authorization failure lost error provenance");
    require(authorization.edges==0,"manager authorization failure advanced accepted edge");
}
static void reset_legacy_rejection() {
    ReleaseInputFixture e; auto t=scalar(); e.allocate(t,1); e.head(7);
    require(e.track_store_release_inputs(t)&&e.issue_store_address(t)&&e.issue_store_data(t),"reset scalar setup rejected");
    auto u=vector(); e.allocate(u,2); require(e.track_vector_release_inputs(u),"reset vector setup rejected");
    require(e.reset(),"production reset rejected");
    require(e.scalar_release_inputs_.empty()&&e.vector_release_inputs_.empty()&&e.memory_agent_.groups.empty(),"reset retained transient input trackers");
    require(e.memory_agent_.resets==1,"manager reset missing");
    ReleaseInputFixture legacy; t.reference_pages=std::make_shared<ReferencePageMap>(0x80002000,std::vector<std::uint64_t>{0x90005000});
    require(legacy.issue_store_address(t)&&legacy.issue_store_data(t),"legacy untracked issue rejected");
    legacy.record_committed_store(t);
    require(legacy.memory_agent_.groups.empty()&&legacy.memory_agent_.legacy_stores==1,"legacy reference notification lost");
    for(unsigned invalid=0;invalid<5;++invalid) {
        ReleaseInputFixture bad; auto a=scalar(); if(invalid)bad.allocate(a,1);
        if(invalid==1)a.cacheable=false; if(invalid==2)a.op=StoreOp::cbo_zero;
        if(invalid==3)a.input_exception_mask=1; if(invalid==4)a.input_flush_pipe=true;
        require(!bad.track_store_release_inputs(a)&&bad.scalar_release_inputs_.empty(),"invalid scalar tracker accepted");
    }
}
int main(int argc,char **argv) {
    try {
        require(argc==2,"one fixture case required"); std::string name=argv[1];
        if(name=="sta_first")scalar_orders(false); else if(name=="std_first")scalar_orders(true);
        else if(name=="pressure_replay")pressure_replay(); else if(name=="head_flags_wrap")younger_head_flags();
        else if(name=="scalar_grouping")scalar_grouping(); else if(name=="scalar_pages")scalar_pages();
        else if(name=="vector_mask_replay")vector_mask_replay(); else if(name=="vector_addresses")vector_addresses();
        else if(name=="vector_misaligned_inactive")vector_misaligned_inactive();
        else if(name=="retirement_keeps_groups")retirement_keeps_groups();
        else if(name=="identity_reuse_absolute_order")identity_reuse_absolute_order();
        else if(name=="vector_invalid_tracking")vector_invalid_tracking(); else if(name=="manager_failures")manager_failures(); else if(name=="reset_legacy_rejection")reset_legacy_rejection(); else throw std::runtime_error("unknown case");
        std::cout<<"PASS "<<name<<'\n'; return 0;
    } catch(const std::exception &e) { std::cerr<<"ASSERT "<<e.what()<<'\n'; return 1; }
}
