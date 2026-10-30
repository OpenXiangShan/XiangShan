#include "memblock_env.hpp"
#include <functional>
#include <map>
#include <tuple>

// A host contract fixture. Only ports and edge scheduling are synthetic; the
// inserted retirement methods, ledger, scoreboards and reference model are the
// production code. The edge/byte expectations below are independently literal.
static void require(bool value, const char *reason) {
    if (!value) throw std::runtime_error(reason);
}

namespace memblock {
#include "environment/scoreboards.inc"

struct FixturePort {
    std::uint64_t contents = 0, writes = 0, nonzero_writes = 0;
    void set(std::uint64_t v) { contents = v; ++writes; nonzero_writes += v != 0; }
    std::uint64_t value() const { return contents; }
    bool boolean() const { return contents != 0; }
};
struct FixtureDut {
    struct {
        struct { FixturePort pending_ptr_value, pending_ptr_flag, pendingst, scommit; } backend;
        struct { FixturePort lq_deq, sq_deq; } retirement;
    } queues;
    std::array<generated::LsqEnqueueObservation, generated::kLsqEnqueueLanes> enqueues{};
    auto sample_lsq_enqueue(unsigned lane) const { return enqueues.at(lane); }
};
// PRODUCTION_QUEUE_ACCOUNTING

void trace_reference_store(const SparseMemory &, const char *, std::uint64_t,
    unsigned, std::uint64_t, unsigned, bool, unsigned, bool, const char *) {}

class StoreRetirementFixture {
public:
    FailureState error_{"host-store-retirement", "contract"};
    QueueAccounting queues_{error_};
    StoreScoreboard store_scoreboard_;
    VectorMemoryScoreboard vector_scoreboard_;
    FixtureDut dut_;
    SparseMemory memory_;
    std::map<std::uint64_t, bool> scalar_release_inputs_, vector_release_inputs_;
    struct RedirectBoundary { unsigned rob; bool flag, flush_itself; std::uint64_t cycle, remaining, sq_allocation_limit; };
    std::optional<RedirectBoundary> scalar_redirect_boundary_;
    std::uint64_t cycle() const { return samples.size() + 6U; }
    void recovered_restart(unsigned rob, bool flag = false) {
        scalar_redirect_boundary_ = RedirectBoundary{rob, flag, false, 0, 0, queues_.sq_allocated()};
    }
    struct Agent {
        void note_reference_store(std::uint64_t, std::uint64_t, const std::vector<unsigned char> &) {}
        void note_reference_write(std::uint64_t, std::uint64_t, unsigned char) {}
    } memory_agent_;
    struct Sample { std::uint64_t rob, flag, pending, commit; };
    std::vector<Sample> samples;
    std::vector<std::tuple<std::uint64_t, unsigned, bool, bool>> reference_ranges;
    std::function<void(StoreRetirementFixture &)> at_edge;
    void tick(bool monitor = true) {
        const auto &b = dut_.queues.backend;
        samples.push_back({b.pending_ptr_value.value(), b.pending_ptr_flag.value(),
            b.pendingst.value(), b.scommit.value()});
        if (monitor) queues_.sample_before_edge(dut_);
        if (at_edge) at_edge(*this);
        // The queue monitor is unconditional in production tick(false), too.
        queues_.sample_after_edge(dut_);
        dut_.queues.retirement.sq_deq.set(0);
        dut_.queues.retirement.lq_deq.set(0);
    }
    bool check_components() {
        queues_.check_driver_accounting();
        error_.merge(store_scoreboard_.failures());
        error_.merge(vector_scoreboard_.failures());
        return error_.empty();
    }
    void allocate(std::uint64_t key, unsigned count, bool vector) {
        if (vector) queues_.allocate_vector_store(key, count);
        else { require(count == 1, "scalar fixture allocation width"); queues_.allocate_scalar_store(key); }
        dut_.enqueues[0] = {.valid=true, .need_alloc=2,
            .num_ls_elem=static_cast<std::uint8_t>(count)};
        queues_.sample_before_edge(dut_);
        dut_.enqueues = {};
        require(check_components(), "allocation monitor mismatch");
    }
    void head(unsigned rob, bool flag, bool pending = true) {
        auto &b = dut_.queues.backend;
        b.pending_ptr_value.set(rob); b.pending_ptr_flag.set(flag); b.pendingst.set(pending);
    }
    void dequeue(unsigned count, bool monitor = true) {
        dut_.queues.retirement.sq_deq.set(count); tick(monitor);
    }
    void track_final_observation_bytes(std::uint64_t a, unsigned n, bool c, bool i) {
        reference_ranges.emplace_back(a,n,c,i);
    }
    void discard_final_observation_range(std::uint64_t, unsigned) {}
    static std::uint64_t scalar_store_key(const StoreTransaction &);
    static std::uint64_t vector_store_key(const VectorMemoryTransaction &);
    bool retire_store(const StoreTransaction &);
    bool retire_vector_stores(const std::vector<VectorMemoryTransaction> &);
    bool commit_stores_through(const StoreTransaction &, unsigned);
    void record_committed_store(const StoreTransaction &);
    void record_committed_vector_store(const VectorMemoryTransaction &);
};
// PRODUCTION_METHODS
} // namespace memblock

using namespace memblock;

static std::array<std::uint64_t,8> stats(const StoreRetirementFixture &e) {
    const auto &s=e.queues_.store_retirement_stats();
    return {s.scalar,s.vector_uops,s.pending_at_retirement,s.already_dequeued,
        s.drained,s.max_pending,s.pending,s.pending_before_last_edge};
}
static std::vector<std::uint64_t> snapshot(const StoreRetirementFixture &e) {
    const auto &b=e.dut_.queues.backend;
    auto s=stats(e);
    std::vector<std::uint64_t> result(s.begin(),s.end());
    for (const auto *p : {&b.pending_ptr_value,&b.pending_ptr_flag,&b.pendingst,&b.scommit}) {
        result.push_back(p->value()); result.push_back(p->writes); result.push_back(p->nonzero_writes);
    }
    result.insert(result.end(), {e.samples.size(),e.reference_ranges.size(),
        e.queues_.sq_allocated(),e.queues_.sq_dequeued(),e.queues_.sq_canceled()});
    for (unsigned i=0;i<64;++i) result.push_back(e.memory_.read_byte(0x80002000+i));
    return result;
}
static StoreTransaction scalar(unsigned rob=7, unsigned sq=0) {
    StoreTransaction t;
    t.address=0x80002000; t.data=0x8877665544332211ULL;
    t.rob=rob; t.sq=sq;
    return t;
}
static void scalar_expect(StoreRetirementFixture &e, const StoreTransaction &t,
    bool address=true, bool data=true) {
    e.store_scoreboard_.expect(t);
    require(e.store_scoreboard_.mark_address_issued(t,0),"scalar STA setup failed");
    require(e.store_scoreboard_.mark_data_issued(t,t.data_lane,0),"scalar STD setup failed");
    generated::ScalarStoreWriteback wb;
    wb.valid=true; wb.rob_value=t.rob; wb.rob_flag=t.rob_flag;
    wb.exception_mask=t.expected_exception_mask; wb.flush_pipe=t.input_flush_pipe;
    if (address) e.store_scoreboard_.observe_address(t.address_lane,wb,0);
    if (data) e.store_scoreboard_.observe_data(t.data_lane,wb,0);
    require(e.check_components(),"scalar scoreboard setup failed");
}
static void scalar_allocate(StoreRetirementFixture &e,const StoreTransaction &t) {
    e.allocate(StoreRetirementFixture::scalar_store_key(t),1,false);
}
static VectorMemoryTransaction vector(unsigned uop=0,unsigned sq=0) {
    VectorMemoryTransaction t;
    t.store=true; t.address=0x80002000; t.rob=20; t.sq=sq;
    t.pdest=40+uop; t.vuop_idx=uop; t.eew=1; t.vl=24; t.vstart=1;
    t.vm=false; t.mask_bits=0x858a; t.mask_high[0]=9;
    t.flow_num=1; t.last_uop=uop==2;
    for(unsigned i=0;i<16;++i)t.data[i]=0xa0+16*uop+i;
    return t;
}
static void vector_expect(StoreRetirementFixture &e,const VectorMemoryTransaction &t,bool complete=true) {
    e.vector_scoreboard_.expect(t,t.data);
    if (complete) {
        generated::VectorMemoryWriteback wb;
        wb.valid=true; wb.rob_value=t.rob; wb.rob_flag=t.rob_flag;
        wb.pdest=t.pdest; wb.fu_op_type=vector_fu_op_type(t); wb.vec_wen=!t.store;
        wb.vsew=vector_vsew(t); wb.veew=t.eew; wb.vl=t.vl; wb.vlmul=t.vlmul;
        wb.vuop_idx=t.vuop_idx; wb.nf=t.nf;
        e.vector_scoreboard_.observe(t.lane,wb);
    }
    require(e.check_components(),"vector scoreboard setup failed");
}
static void vector_allocate(StoreRetirementFixture &e,const VectorMemoryTransaction &t) {
    e.allocate(StoreRetirementFixture::vector_store_key(t),t.flow_num,true);
}
static std::vector<VectorMemoryTransaction> group(StoreRetirementFixture &e) {
    auto a=vector(0,0),b=vector(1,2),c=vector(2,5);
    a.flow_num=2;b.flow_num=3;
    for(const auto &t:{a,b,c}) {vector_allocate(e,t);vector_expect(e,t);}
    e.head(a.rob,a.rob_flag); e.recovered_restart(a.rob,a.rob_flag);
    return {a,b,c};
}
static void check_bytes(const StoreRetirementFixture &e,
    const std::map<unsigned,unsigned> &changed,unsigned size=64) {
    for(unsigned i=0;i<size;++i) {
        const auto found=changed.find(i);
        const unsigned expected=found==changed.end()?0xee:found->second;
        require(e.memory_.read_byte(0x80002000+i)==expected,"independent byte schedule mismatch");
    }
}
static void initial_bytes(StoreRetirementFixture &e) {
    for(unsigned i=0;i<64;++i)e.memory_.write_byte(0x80002000+i,0xee);
}
static void scalar_pulse(const StoreRetirementFixture &e,unsigned rob,bool flag,unsigned ticks=1) {
    require(e.samples.size()==ticks,"scalar retirement did not take exactly one edge");
    require(e.samples.back().rob==rob && e.samples.back().flag==flag &&
        e.samples.back().pending==1 && e.samples.back().commit==1,"scalar retirement edge lacked exact held head and one scommit");
    require(e.dut_.queues.backend.scommit.value()==0 &&
        e.dut_.queues.backend.scommit.nonzero_writes==1,"scalar commit pulse missing, repeated or left high");
}

static void scalar_retained() {
    StoreRetirementFixture e; initial_bytes(e);
    auto t=scalar(159,55);t.rob_flag=true;t.sq_flag=true;
    scalar_allocate(e,t);scalar_expect(e,t);e.head(t.rob,t.rob_flag);
    e.at_edge=[&](auto &f) {check_bytes(f,{});};
    require(e.retire_store(t),"scalar retirement failed");
    scalar_pulse(e,159,true);
    require(stats(e)==std::array<std::uint64_t,8>{1,0,1,0,0,1,1,0},"retained scalar statistics mismatch");
    check_bytes(e,{{0,0x11},{1,0x22},{2,0x33},{3,0x44},{4,0x55},{5,0x66},{6,0x77},{7,0x88}});
    require(e.reference_ranges.size()==1 && std::get<1>(e.reference_ranges[0])==8,"scalar reference transition was not exactly once");
    require(!e.queues_.scalar_store_target(StoreRetirementFixture::scalar_store_key(t)),"retired scalar target stayed allocatable");
    e.at_edge={}; e.dequeue(0,false);
    require(stats(e)==std::array<std::uint64_t,8>{1,0,1,0,0,1,1,1},"unmonitored edge lost retained scalar");
    e.dequeue(1,false);
    require(stats(e)==std::array<std::uint64_t,8>{1,0,1,0,1,1,0,1},"real unmonitored dequeue did not drain retained scalar");
    const auto before=snapshot(e);
    require(!e.retire_store(t),"duplicate scalar retirement accepted");
    require(snapshot(e)==before,"duplicate scalar retirement had a side effect");
}
static void scalar_already_dequeued() {
    for(bool on_retirement_edge:{false,true}) {
        StoreRetirementFixture e;initial_bytes(e);auto t=scalar();
        scalar_allocate(e,t);scalar_expect(e,t);e.head(t.rob,t.rob_flag);
        if(on_retirement_edge)e.at_edge=[](auto &f){f.dut_.queues.retirement.sq_deq.set(1);};
        else e.dequeue(1,false);
        require(e.retire_store(t),"already dequeued scalar retirement failed");
        scalar_pulse(e,t.rob,t.rob_flag,on_retirement_edge?1:2);
        require(stats(e)==std::array<std::uint64_t,8>{1,0,0,1,1,0,0,0},"already dequeued scalar was retained or lost");
        check_bytes(e,{{0,0x11},{1,0x22},{2,0x33},{3,0x44},{4,0x55},{5,0x66},{6,0x77},{7,0x88}});
        require(e.reference_ranges.size()==1,"already dequeued scalar reference recorded twice");
    }
}
static void scalar_widths() {
    const std::array<std::map<unsigned,unsigned>,4> expected{{
        {{3,0x11}},{{3,0x11},{4,0x22}},
        {{3,0x11},{4,0x22},{5,0x33},{6,0x44}},
        {{3,0x11},{4,0x22},{5,0x33},{6,0x44},{7,0x55},{8,0x66},{9,0x77},{10,0x88}}}};
    const std::array<StoreOp,4> ops{StoreOp::sb,StoreOp::sh,StoreOp::sw,StoreOp::sd};
    for(unsigned index=0;index<ops.size();++index) {
        StoreRetirementFixture e;initial_bytes(e);auto t=scalar();
        t.address=0x80007003;t.oracle_address=0x80002003;t.op=ops[index];
        scalar_allocate(e,t);scalar_expect(e,t);e.head(t.rob,t.rob_flag);
        require(e.retire_store(t),"normal scalar width retirement failed");
        check_bytes(e,expected[index]);
        require(e.memory_.read_byte(0x80007003)==0,"reference transition wrote virtual instead of oracle address");
        require(e.reference_ranges.size()==1,"scalar width reference event repeated");
    }
}
static void scalar_invalid() {
    for(unsigned fault=0;fault<16;++fault) {
        StoreRetirementFixture e;initial_bytes(e);auto t=scalar();
        if(fault!=11)scalar_allocate(e,t);
        scalar_expect(e,t,fault!=5,fault!=6);e.head(t.rob,t.rob_flag);
        switch(fault) {
        case 0:t.op=StoreOp::cbo_zero;break;case 1:t.cacheable=false;break;
        case 2:t.expected_exception_mask=1;break;case 3:t.input_exception_mask=1;break;
        case 4:t.input_flush_pipe=true;break;case 7:e.head(8,false);break;
        case 8:e.head(t.rob,true);break;case 9:e.head(t.rob,false,false);break;
        case 10:t.sq=1;break;case 12:t.input_trigger=kTriggerBreakpoint;break;
        case 13:t.expected_output_flush_pipe=true;break;case 14:t.expected_debug_is_mmio=true;break;
        case 15:t.expected_debug_is_ncio=true;break;default:break;
        }
        const auto before=snapshot(e);
        require(!e.retire_store(t),"invalid scalar retirement accepted");
        require(snapshot(e)==before,"invalid scalar retirement changed clock, ports, ledger or reference image");
        if(fault!=11 && fault!=10)require(e.queues_.scalar_store_target(StoreRetirementFixture::scalar_store_key(scalar()))==1,"scalar preflight lost allocation");
    }
}
static void vector_retained() {
    StoreRetirementFixture e;initial_bytes(e);const auto ts=group(e);
    e.at_edge=[](auto &f){check_bytes(f,{});};
    require(e.retire_vector_stores(ts),"complete vector instruction retirement failed");
    require(e.samples.size()==1 && e.samples[0].rob==20 && e.samples[0].pending==1 &&
        e.samples[0].commit==0 && e.dut_.queues.backend.scommit.writes==0,"vector retirement emitted scalar commit or extra edges");
    require(stats(e)==std::array<std::uint64_t,8>{0,3,3,0,0,3,3,0},"vector retirement did not retain all uops");
    check_bytes(e,{{2,0xa2},{3,0xa3},{6,0xa6},{7,0xa7},{14,0xae},{15,0xaf},
        {16,0xb0},{17,0xb1},{20,0xb4},{21,0xb5},{30,0xbe},{31,0xbf},
        {32,0xc0},{33,0xc1},{38,0xc6},{39,0xc7}});
    require(e.reference_ranges.size()==8,"masked vector reference element count mismatch");
    for(const auto &t:ts)require(!e.queues_.vector_store_target(StoreRetirementFixture::vector_store_key(t)),"retired vector allocation remains");
    e.at_edge={};
    // Absolute targets are 2,5,6. Crossing an interior flow cannot release a uop.
    const std::array<unsigned,5> deq{1,1,2,1,1};
    const std::array<std::array<std::uint64_t,3>,5> expected{{{0,3,3},{1,2,3},{1,2,2},{2,1,2},{3,0,1}}};
    for(unsigned i=0;i<deq.size();++i) {
        e.dequeue(deq[i],false);const auto &s=e.queues_.store_retirement_stats();
        require(std::array<std::uint64_t,3>{s.drained,s.pending,s.pending_before_last_edge}==expected[i],"partial vector dequeue released wrong target prefix");
        require(e.check_components(),"vector conservation check failed");
    }
    const auto before=snapshot(e);
    require(!e.retire_vector_stores(ts),"duplicate vector retirement accepted");
    require(snapshot(e)==before,"duplicate vector retirement had a side effect");
}
static void vector_already_dequeued() {
    StoreRetirementFixture e;initial_bytes(e);const auto ts=group(e);
    e.dequeue(2,false);
    e.at_edge=[](auto &f){f.dut_.queues.retirement.sq_deq.set(3);};
    require(e.retire_vector_stores(ts),"partly dequeued vector group retirement failed");
    require(stats(e)==std::array<std::uint64_t,8>{0,3,1,2,2,1,1,0},"vector previous/on-edge dequeue classification mismatch");
    e.at_edge={};e.dequeue(1,false);
    require(stats(e)==std::array<std::uint64_t,8>{0,3,1,2,3,1,0,1},"last vector target failed to drain");
    require(e.dut_.queues.backend.scommit.writes==0,"dequeued vector retirement emitted scalar commit");
}
static void vector_invalid() {
    for(unsigned fault=0;fault<35;++fault) {
        StoreRetirementFixture e;initial_bytes(e);auto ts=group(e);
        switch(fault) {
        case 0:ts.clear();break;case 1:ts.erase(ts.begin());break;
        case 2:ts.erase(ts.begin()+1);break;case 3:ts.pop_back();break;
        case 4:ts[1]=ts[0];break;case 5:std::swap(ts[0],ts[1]);break;
        case 6:ts[0].last_uop=true;break;case 7:ts[2].last_uop=false;break;
        case 8:ts[2].last_uop=false;ts[1].last_uop=true;break;
        case 9:ts[1].store=false;break;case 10:ts[1].segment=true;break;
        case 11:ts[1].cacheable=false;break;case 12:ts[1].expected_exception_mask=1;break;
        case 13:ts[1].allowed_additional_exception_mask=1;break;
        case 14:ts[1].input_exception_mask=1;break;case 15:ts[1].input_flush_pipe=true;break;
        case 16:ts[1].exception_may_defer=true;break;case 17:ts[1].rob=21;break;
        case 18:ts[1].rob_flag=true;break;case 19:vector_expect(e,ts[1],false);break;
        case 20:e.head(21,false);break;case 21:e.head(20,true);break;
        case 22:e.head(20,false,false);break;case 23:ts[1].sq=8;break;
        case 24:ts[1].input_trigger=kTriggerBreakpoint;break;case 25:ts[1].check_data=false;break;
        case 26:ts[1].expected_debug_is_mmio=true;break;case 27:ts[1].expected_debug_is_ncio=true;break;
        case 28:e.scalar_redirect_boundary_.reset();break;
        case 29:e.scalar_redirect_boundary_->rob++;break;
        case 30:e.scalar_redirect_boundary_->flag=true;break;
        case 31:e.scalar_redirect_boundary_->flush_itself=true;break;
        case 32:e.scalar_redirect_boundary_->remaining=1;break;
        case 33:e.scalar_redirect_boundary_->cycle=e.cycle();break;
        case 34:e.scalar_redirect_boundary_->sq_allocation_limit=0;break;
        }
        const auto before=snapshot(e);
        require(!e.retire_vector_stores(ts),"invalid or incomplete vector retirement accepted");
        require(snapshot(e)==before,"vector preflight had partial clock, port, ledger or reference side effects");
        for(const auto &t:{vector(0,0),vector(1,2),vector(2,5)})require(e.queues_.vector_store_target(StoreRetirementFixture::vector_store_key(t)).has_value(),"vector preflight lost allocation");
    }
}
static void mixed_prefix() {
    StoreRetirementFixture e;
    auto a=scalar(1,0),b=scalar(3,4);b.address+=32;
    auto v0=vector(0,1),v1=vector(1,3);v0.rob=v1.rob=2;
    v0.flow_num=2;v1.last_uop=true;
    scalar_allocate(e,a);scalar_expect(e,a);
    for(const auto &t:{v0,v1}){vector_allocate(e,t);vector_expect(e,t);}
    scalar_allocate(e,b);scalar_expect(e,b);
    e.head(a.rob,false);require(e.retire_store(a),"mixed first scalar failed");
    e.head(v0.rob,false);e.recovered_restart(v0.rob);require(e.retire_vector_stores({v0,v1}),"mixed vector failed");
    e.head(b.rob,false);require(e.retire_store(b),"mixed final scalar failed");
    require(stats(e)==std::array<std::uint64_t,8>{2,2,4,0,0,4,4,3},"mixed retirement obligations mismatch");
    const std::array<unsigned,4> deq{2,1,1,1};
    const std::array<std::array<std::uint64_t,3>,4> expected{{{1,3,4},{2,2,3},{3,1,2},{4,0,1}}};
    for(unsigned i=0;i<deq.size();++i){
        e.dequeue(deq[i],false);const auto &s=e.queues_.store_retirement_stats();
        require(std::array<std::uint64_t,3>{s.drained,s.pending,s.pending_before_last_edge}==expected[i],"mixed scalar/vector target prefix mismatch");
        require(e.check_components(),"mixed target conservation failed");
    }
    require(e.dut_.queues.backend.scommit.nonzero_writes==2,"mixed sequence scalar pulse count mismatch");
}
static void cancellation_retains() {
    StoreRetirementFixture e;auto a=scalar(1,0),b=scalar(2,1),c=scalar(3,2);
    for(const auto &t:{a,b,c})scalar_allocate(e,t);
    scalar_expect(e,a);e.head(a.rob,false);require(e.retire_store(a),"cancellation setup retirement failed");
    require(e.queues_.account_sq_cancellation(1),"younger software cancellation failed");
    require(e.queues_.observe_redirect_cancellation(0,1),"younger observed cancellation failed");
    e.dequeue(0,false);
    require(e.queues_.sq_canceled()==2 && e.queues_.sq_dequeued()==0,"cancellation fabricated public dequeue");
    require(stats(e)==std::array<std::uint64_t,8>{1,0,1,0,0,1,1,1},"cancellation released committed store obligation");
    e.dequeue(1,false);
    require(stats(e)==std::array<std::uint64_t,8>{1,0,1,0,1,1,0,1},"real dequeue after cancellation failed");
    require(e.check_components(),"post-cancellation conservation failed");
}
static void cumulative_wrap_reuse() {
    StoreRetirementFixture e;auto first=scalar(159,0);scalar_allocate(e,first);scalar_expect(e,first);
    require(e.queues_.scalar_store_target(StoreRetirementFixture::scalar_store_key(first))==1,"first absolute target mismatch");
    e.head(first.rob,false);require(e.retire_store(first),"first wrap retirement failed");e.dequeue(1,false);
    // Complete one SQ revolution, cross the flag boundary, then complete a
    // second revolution. Padding has its own vector key and real public deq.
    for(unsigned i=0;i<55;++i) {
        e.allocate(0xfffe,1,true);
        require(e.queues_.retire_vector_store(0xfffe),"first padding retirement failed");
        e.dequeue(1,false);
    }
    require(e.queues_.sq_dequeued()==56,"first padding absolute target mismatch");
    auto wrapped=scalar(0,0);wrapped.rob_flag=true;wrapped.sq_flag=true;
    scalar_allocate(e,wrapped);scalar_expect(e,wrapped);
    require(e.queues_.scalar_store_target(StoreRetirementFixture::scalar_store_key(wrapped))==57,"flag boundary target mismatch");
    e.head(wrapped.rob,true);require(e.retire_store(wrapped),"wrapped scalar retirement failed");e.dequeue(1,false);
    for(unsigned i=0;i<55;++i) {
        e.allocate(0xfffd,1,true);
        require(e.queues_.retire_vector_store(0xfffd),"second padding retirement failed");
        e.dequeue(1,false);
    }
    require(e.queues_.sq_dequeued()==112,"second padding absolute target mismatch");
    scalar_allocate(e,first);scalar_expect(e,first);
    require(e.queues_.scalar_store_target(StoreRetirementFixture::scalar_store_key(first))==113,"reused key reset absolute target");
    e.head(first.rob,false);require(e.retire_store(first),"reused scalar key retirement failed");
    e.dequeue(0,false);require(e.queues_.store_retirement_stats().pending==1,"reused key drained against old dequeue");
    e.dequeue(1,false);
    require(e.queues_.sq_dequeued()==113 && stats(e)==std::array<std::uint64_t,8>{3,110,113,0,113,1,0,1},"cumulative wrap/reuse accounting mismatch");
    require(e.check_components(),"wrap/reuse conservation failed");
}
static void invalid_target_order() {
    {
        StoreRetirementFixture e;initial_bytes(e);auto a=scalar(1,0),b=scalar(2,1);
        scalar_allocate(e,a);scalar_expect(e,a);scalar_allocate(e,b);
        require(e.queues_.retire_scalar_store(StoreRetirementFixture::scalar_store_key(b)),"stale scalar target setup failed");
        e.head(a.rob,false);const auto before=snapshot(e);
        require(!e.retire_store(a),"API accepted stale scalar allocation target");
        require(snapshot(e)==before && e.queues_.scalar_store_target(StoreRetirementFixture::scalar_store_key(a))==1,
            "stale scalar target rejection emitted a commit or changed allocation");
    }
    {
        StoreRetirementFixture e;initial_bytes(e);const auto ts=group(e);
        auto younger=scalar(21,6);scalar_allocate(e,younger);
        require(e.queues_.retire_scalar_store(StoreRetirementFixture::scalar_store_key(younger)),"stale vector target setup failed");
        const auto before=snapshot(e);
        require(!e.retire_vector_stores(ts),"API accepted stale vector allocation targets");
        require(snapshot(e)==before,"stale vector target rejection ticked or changed state");
        for(const auto &t:ts)require(e.queues_.vector_store_target(StoreRetirementFixture::vector_store_key(t)).has_value(),
            "stale vector target rejection consumed input allocation");
    }
}
static void redirect_targets() {
    unsigned cases = 0;
    for (unsigned anchor = 0; anchor < kRobEntries; ++anchor) for (bool flag : {false, true})
        for (bool self : {false, true}) {
            StoreRetirementFixture e;
            // Independent circular-age reference. Populate one ROB revolution
            // around each anchor; no half-ring ambiguous age is generated.
            struct Item { unsigned value; bool flag; std::uint64_t scalar, vector; bool killed; };
            std::vector<Item> items;
            for (int delta = -4; delta <= 4; ++delta) {
                const int raw = int(anchor) + delta;
                const unsigned value = (raw + kRobEntries) % kRobEntries;
                const bool wrap = flag ^ (raw < 0 || raw >= int(kRobEntries));
                const auto base = std::uint64_t{value} | (std::uint64_t{wrap} << 8U);
                const auto skey = base | (std::uint64_t{unsigned(delta+4)} << 9U);
                const auto vkey = base | (std::uint64_t{unsigned(delta+14)} << 9U);
                e.allocate(skey, 1, false);e.allocate(vkey, 2, true);
                items.push_back({value,wrap,skey,vkey,delta > 0 || (delta == 0 && self)});
            }
            const auto before = snapshot(e);
            require(e.queues_.cancel_store_targets_for_redirect(anchor,flag,self),"legal target cancellation failed");
            for (const auto &item : items) {
                require(e.queues_.scalar_store_target(item.scalar).has_value() == !item.killed &&
                    e.queues_.vector_store_target(item.vector).has_value() == !item.killed,
                    "redirect removed survivor or retained killed allocation target");
            }
            require(snapshot(e)==before,"target cleanup changed physical credits, bytes or committed obligations");
            ++cases;
        }
    StoreRetirementFixture e;
    auto committed=scalar(100,0);scalar_allocate(e,committed);scalar_expect(e,committed);e.head(100,false);
    require(e.retire_store(committed),"committed target setup failed");
    // Canceled old generation and a later full-identity ROB reuse have different
    // SQ targets. The canceled member must not pollute complete-group checking.
    const std::uint64_t old_key=101|(std::uint64_t{1}<<9U), new_key=101|(std::uint64_t{2}<<9U);
    e.allocate(old_key,1,true);
    require(e.queues_.observe_redirect_cancellation(0,1) &&
        e.queues_.cancel_store_targets_for_redirect(100,false,false),"redirect cleanup failed");
    require(!e.queues_.vector_store_target(old_key),"canceled vector target survived ROB reuse");
    require(e.queues_.store_retirement_stats().pending==1,"committed SQ obligation canceled");
    e.allocate(new_key,1,true);
    require(e.queues_.complete_vector_store_group({new_key}),"old canceled target polluted reused ROB group");
    // Repeated cleanup is idempotent. A previously canceled legacy allocation
    // ordinal may be below a later committed prefix and still is not committed.
    require(e.queues_.cancel_store_targets_for_redirect(100,false,false),"idempotent redirect cleanup failed");
    StoreRetirementFixture stale;
    stale.allocate(old_key,1,true);
    stale.allocate(50,1,false);
    require(stale.queues_.retire_scalar_store(50),"later committed-prefix setup failed");
    require(stale.queues_.cancel_store_targets_for_redirect(100,false,false) &&
        !stale.queues_.vector_store_target(old_key) && stale.queues_.store_retirement_stats().pending==1,
        "stale target ordinal confused with committed ownership");
    const auto before=snapshot(e);
    require(!e.queues_.cancel_store_targets_for_redirect(kRobEntries,false,true),"invalid target boundary accepted");
    require(snapshot(e)==before,"invalid target cancellation changed state");
    std::cout << "redirect_target_histories=" << cases << " reused_group=1 committed_survivor=1\n";
}

static void ledger_rejection() {
    StoreRetirementFixture e;
    e.allocate(0x101,1,false);e.allocate(0x102,1,false);
    require(e.queues_.retire_scalar_store(0x102),"ledger setup retirement failed");
    const auto before=snapshot(e);
    require(!e.queues_.retire_scalar_store(0x101),"ledger accepted decreasing target");
    require(snapshot(e)==before && e.queues_.scalar_store_target(0x101)==1,"ledger rejection changed retained records or allocation");
    require(!e.queues_.retire_scalar_store(0x102),"ledger accepted duplicated retirement");
    require(snapshot(e)==before,"duplicate ledger retirement changed counters");
    StoreRetirementFixture groups;
    groups.allocate(0x201,2,true);groups.allocate(0x401,1,true);
    groups.allocate(0x301,1,true); // Same ROB value with the other ROB flag.
    require(!groups.queues_.complete_vector_store_group({0x201}) &&
        groups.queues_.complete_vector_store_group({0x201,0x401}) &&
        !groups.queues_.complete_vector_store_group({0x201,0x401,0x301}) &&
        groups.queues_.complete_vector_store_group({0x301}),"complete vector membership ignored missing member or ROB flag");
}

int main(int argc,char **argv) {
    try {
        const std::map<std::string,void(*)()> cases{
            {"scalar_retained",scalar_retained},{"scalar_already_dequeued",scalar_already_dequeued},
            {"scalar_widths",scalar_widths},{"scalar_invalid",scalar_invalid},
            {"vector_retained",vector_retained},{"vector_already_dequeued",vector_already_dequeued},
            {"vector_invalid",vector_invalid},{"mixed_prefix",mixed_prefix},
            {"cancellation_retains",cancellation_retains},{"cumulative_wrap_reuse",cumulative_wrap_reuse},
            {"redirect_targets",redirect_targets},{"ledger_rejection",ledger_rejection},{"invalid_target_order",invalid_target_order}};
        require(argc==2,"one case name required");cases.at(argv[1])();return 0;
    } catch(const std::exception &error) {std::cerr<<error.what()<<'\n';return 1;}
}
