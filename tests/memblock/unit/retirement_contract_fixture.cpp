#include "memblock_env.hpp"
#include <functional>
#include <random>

void require(bool value, const char *why) {
    if (!value) throw std::runtime_error(why);
}

namespace memblock {
#include "environment/scoreboards.inc"

// Only ports and scheduling are synthetic. Enqueue/dequeue conservation below
// uses the production QueueAccounting class and monitor methods.
struct FixturePort {
    std::uint64_t contents = 0;
    unsigned nonzero_writes = 0;
    void set(std::uint64_t value) { contents = value; nonzero_writes += value != 0; }
    std::uint64_t value() const { return contents; }
};
struct FixtureDut {
    struct {
        struct { FixturePort pending_ptr_value, pending_ptr_flag, pendingst, scommit; } backend;
        struct { FixturePort lq_deq, sq_deq; } retirement;
    } queues;
    std::array<generated::LsqEnqueueObservation, generated::kLsqEnqueueLanes> enqueues{};
    generated::LsqEnqueue last_enqueue;
    unsigned enqueue_drives = 0;
    void drive_lsq_enqueue(unsigned lane, const generated::LsqEnqueue &enqueue) {
        last_enqueue = enqueue;
        ++enqueue_drives;
        enqueues.at(lane) = {.valid=true, .need_alloc=enqueue.need_alloc,
                            .num_ls_elem=enqueue.num_ls_elem};
    }
    void clear_lsq_enqueue_valids() { enqueues = {}; }
    auto sample_lsq_enqueue(unsigned lane) const { return enqueues.at(lane); }
};
// PRODUCTION_QUEUE_ACCOUNTING

struct VectorReplayRequest {
    unsigned lane = 0;
    bool lq_flag = false;
    std::uint8_t lq_value = 0;
    bool sq_flag = false;
    std::uint8_t sq_value = 0;
    bool is_part_replay = false;
    std::uint16_t replay_mask = 0;
    std::uint8_t replay_mb_index = 0;
};
class FeedbackMonitor {
public:
    Environment::IqSlowFeedbackStats stats;
    std::deque<VectorReplayRequest> vector_replay_requests_;
    const auto &iq_slow_feedback_stats() const { return stats; }
    unsigned vector_replay_feedbacks() const { return 0; }
    std::size_t pending_replays() const { return vector_replay_requests_.size(); }
    std::optional<VectorReplayRequest> take_replay_for(const VectorMemoryTransaction &);
};
// PRODUCTION_REPLAY_MATCH

// Debug and ownership recording do not decide the retirement result.
void trace_reference_store(SparseMemory &, const char *, std::uint64_t, unsigned,
    std::uint64_t, std::uint8_t, bool, std::uint8_t, bool, const char *) {}
class RetirementFixture {
public:
    FailureState error_{"retirement-fixture", "contract"};
    QueueAccounting queues_{error_};
    StoreScoreboard store_scoreboard_;
    VectorMemoryScoreboard vector_scoreboard_;
    FeedbackMonitor feedback_;
    FixtureDut dut_;
    SparseMemory memory_;
    struct Agent {
        unsigned request_count() const { return 0; }
        void note_reference_store(std::uint64_t, std::uint64_t, const std::vector<unsigned char> &) {}
        void note_reference_write(std::uint64_t, std::uint64_t, unsigned char) {}
    } memory_agent_, ptw_agent_;
    std::uint64_t clock = 0;
    std::vector<std::uint64_t> address_issue_cycles;
    std::vector<VectorMemoryTransaction> vector_issues;
    std::function<void(RetirementFixture &)> on_tick;
    std::function<void(RetirementFixture &, const StoreTransaction &)> on_address_issue;
    std::function<void(RetirementFixture &, const VectorMemoryTransaction &)> on_vector_issue;
    unsigned head_observations = 0;
    std::uint64_t cycle() const { return clock; }
    void tick() {
        queues_.sample_before_edge(dut_);
        if (on_tick) on_tick(*this);
        queues_.sample_after_edge(dut_);
        dut_.queues.retirement.lq_deq.set(0);
        dut_.queues.retirement.sq_deq.set(0);
        ++clock;
    }
    bool check_components() {
        queues_.check_driver_accounting();
        error_.merge(store_scoreboard_.failures());
        error_.merge(vector_scoreboard_.failures());
        return error_.empty();
    }
    bool run_cycles(unsigned count) {
        for (unsigned i=0; i<count; ++i) {
            tick();
            if (!check_components()) return false;
        }
        return true;
    }
    bool issue_store_address(const StoreTransaction &transaction, unsigned timeout) {
        require(timeout > 0, "STA retry received zero timeout");
        address_issue_cycles.push_back(cycle());
        if (store_scoreboard_.pending(transaction))
            require(store_scoreboard_.mark_address_issued(transaction,cycle()), "STA acceptance failed");
        if (on_address_issue) on_address_issue(*this, transaction);
        tick();
        return check_components();
    }
    bool issue_vector(const VectorMemoryTransaction &transaction, unsigned timeout) {
        require(timeout > 0, "vector replay received zero timeout");
        vector_issues.push_back(transaction);
        if (on_vector_issue) on_vector_issue(*this, transaction);
        tick();
        return check_components();
    }
    bool wait_for_enqueue_capacity(unsigned lq, unsigned sq) {
        const auto result = queues_.has_capacity(lq,sq);
        return result && *result;
    }
    void track_final_observation_bytes(std::uint64_t, unsigned, bool, bool) {}
    static std::uint64_t scalar_store_key(const StoreTransaction &);
    static std::uint64_t vector_store_key(const VectorMemoryTransaction &);
    bool enqueue_store(const StoreTransaction &);
    bool enqueue_store(const StoreTransaction &, std::uint8_t, bool);
    bool enqueue_store_pressure(const StoreTransaction &, std::uint8_t, bool);
    bool issue_store_address_until_tlb_hit(const StoreTransaction &, unsigned);
    bool run_until_store_complete_with_replay(const StoreTransaction &, unsigned, bool=false);
    bool run_until_vector_complete_with_replays(const VectorMemoryTransaction &, unsigned, bool=false, bool=false);
    bool run_until_vector_complete_with_replays(const std::vector<VectorMemoryTransaction> &, unsigned, bool=false);
    bool commit_vector_store(const VectorMemoryTransaction &, unsigned);
    bool run_until_sq_dequeued(std::uint64_t, unsigned);
    bool set_rob_head(std::uint8_t, bool);
    bool pulse_pending_store(std::uint8_t, bool);
    bool pulse_store_commit_keep_pending(std::uint8_t, bool);

    // Fixture setup records allocations through the real passive monitor too.
    void seed_loads(unsigned count, unsigned canceled=0, unsigned dequeued=0) {
        queues_.allocate_load(count);
        dut_.enqueues[0] = {.valid=true, .need_alloc=1, .num_ls_elem=static_cast<std::uint8_t>(count)};
        dut_.queues.retirement.lq_deq.set(dequeued);
        tick(); dut_.clear_lsq_enqueue_valids();
        require(queues_.account_lq_cancellation(canceled), "seed LQ cancellation failed");
        require(check_components(), "seed LQ accounting failed");
    }
    void seed_stores(std::uint64_t key, unsigned count, unsigned canceled=0, unsigned dequeued=0) {
        queues_.allocate_vector_store(key,count);
        dut_.enqueues[0] = {.valid=true, .need_alloc=2, .num_ls_elem=static_cast<std::uint8_t>(count)};
        dut_.queues.retirement.sq_deq.set(dequeued);
        tick(); dut_.clear_lsq_enqueue_valids();
        require(queues_.account_sq_cancellation(canceled), "seed SQ cancellation failed");
        require(check_components(), "seed SQ accounting failed");
    }
};
// PRODUCTION_METHODS
} // namespace memblock

namespace memblock::scenarios::mixed {
class MixedScenario {
public:
    std::uint64_t rob_offset=0, sq_offset=0, lq_offset=0;
    std::mt19937_64 random{17};
    std::shared_ptr<const ReferencePageMap> reference_pages_for(std::uint64_t) const { return {}; }
    LoadTransaction make_load(std::uint64_t, LoadOp, unsigned, bool);
    StoreTransaction make_store(std::uint64_t, std::uint64_t, StoreOp, unsigned);
};
// PRODUCTION_BUILDERS
} // namespace memblock::scenarios::mixed
using namespace memblock;

static generated::ScalarStoreWriteback scalar_wb(const StoreTransaction &transaction) {
    return {.valid=true, .rob_flag=transaction.rob_flag, .rob_value=transaction.rob};
}
static generated::VectorMemoryWriteback vector_wb(const VectorMemoryTransaction &t) {
    generated::VectorMemoryWriteback wb;
    wb.valid=true; wb.rob_value=t.rob; wb.rob_flag=t.rob_flag;
    wb.pdest=t.pdest; wb.fu_op_type=vector_fu_op_type(t); wb.vec_wen=!t.store;
    wb.vsew=vector_vsew(t); wb.veew=t.eew; wb.vl=t.vl; wb.vlmul=t.vlmul;
    wb.vuop_idx=t.vuop_idx; wb.nf=t.nf; wb.vd_index_in_field=vector_indexed_vd_index(t);
    wb.data.assign(t.data.begin(),t.data.end()); wb.vmask.assign(16,0);
    auto mask=vector_writeback_elements(t); wb.vmask[0]=mask; wb.vmask[1]=mask>>8;
    return wb;
}
static void accept_store(RetirementFixture &env, const StoreTransaction &t, bool data=true) {
    env.store_scoreboard_.expect(t);
    require(env.store_scoreboard_.mark_address_issued(t,env.cycle()), "initial STA acceptance failed");
    if (data) require(env.store_scoreboard_.mark_data_issued(t,t.data_lane,env.cycle()), "STD acceptance failed");
}
static void complete_vector(RetirementFixture &env, const VectorMemoryTransaction &t) {
    env.vector_scoreboard_.observe(t.lane,vector_wb(t));
}
static VectorMemoryTransaction vector_store(unsigned rob=9, unsigned sq=4) {
    VectorMemoryTransaction t{.store=true,.address=0x80001000,.eew=0,.vl=16,
        .rob=static_cast<std::uint8_t>(rob),.sq=static_cast<std::uint8_t>(sq),.flow_num=4};
    for(unsigned b=0;b<16;++b)t.data[b]=0x60+b;
    return t;
}

void scalar_scoped() {
    RetirementFixture env;
    StoreTransaction selected{.address=0x80000000,.rob=7};
    auto younger=selected; younger.rob_flag=true;
    accept_store(env,selected);
    env.store_scoreboard_.expect(younger);
    env.on_address_issue=[&](auto &e, const auto &replay) {
        require(replay.rob==selected.rob && replay.rob_flag==selected.rob_flag, "STA replay used wrong ROB identity");
        e.store_scoreboard_.observe_address(0,scalar_wb(selected),e.cycle());
    };
    env.on_tick=[&](auto &e) {
        if(e.cycle()==33)e.store_scoreboard_.observe_data(0,scalar_wb(selected),e.cycle());
    };
    require(env.run_until_store_complete_with_replay(selected,80), "selected wait blocked on younger store");
    require(env.address_issue_cycles.size()==1, "pending STA was not replayed exactly once");
    require(!env.store_scoreboard_.pending(selected) && env.store_scoreboard_.pending(younger), "wait confused ROB wrap identities");
    require(env.store_scoreboard_.observed()==1 && !env.store_scoreboard_.done(), "wait lost younger expectation");
}
void scalar_data_pending() {
    RetirementFixture env;
    StoreTransaction t{.address=0x80000000,.rob=7}; accept_store(env,t);
    env.store_scoreboard_.observe_address(0,scalar_wb(t),0);
    env.on_tick=[&](auto &e) {if(e.cycle()==79)e.store_scoreboard_.observe_data(0,scalar_wb(t),e.cycle());};
    require(env.run_until_store_complete_with_replay(t,96,true), "data-pending store failed");
    require(env.address_issue_cycles.empty(), "completed STA was replayed while only STD remained");
    require(env.dut_.queues.backend.pendingst.value()==0, "pendingst leaked after success");
    require(env.dut_.queues.backend.scommit.nonzero_writes==0, "wait emitted scalar commit");
}
void scalar_timeout() {
    RetirementFixture env;
    StoreTransaction t{.address=0x80000000,.rob=7,.rob_flag=true}; accept_store(env,t);
    env.store_scoreboard_.observe_address(0,scalar_wb(t),0);
    require(!env.run_until_store_complete_with_replay(t,17,true), "missing STD fabricated success");
    require(env.cycle()==17 && env.store_scoreboard_.pending(t), "timeout consumed outstanding store");
    require(env.address_issue_cycles.empty(), "timeout replayed completed address");
    require(env.dut_.queues.backend.pendingst.value()==0, "pendingst leaked after timeout");
    require(env.error_.message().find("timed out")!=std::string::npos, "timeout lacked diagnostic");
}
void sta_feedback() {
    RetirementFixture env;
    StoreTransaction t{.rob=7,.sq=3,.sq_flag=true,.address_lane=1};
    env.feedback_.stats.sta_samples.push_back({.lane=1,.hit=true,.sq_flag=true,.sq_value=3});
    env.on_tick=[&](auto &e) {
        const auto elapsed=e.cycle()-e.address_issue_cycles.back();
        if(e.address_issue_cycles.size()==1) {
            if(elapsed==0)e.feedback_.stats.sta_samples.push_back({.lane=0,.hit=true,.sq_flag=true,.sq_value=3});
            if(elapsed==1)e.feedback_.stats.sta_samples.push_back({.lane=1,.hit=true,.sq_flag=false,.sq_value=3});
            if(elapsed==2)e.feedback_.stats.sta_samples.push_back({.lane=1,.hit=true,.sq_flag=true,.sq_value=4});
            if(elapsed==4)e.feedback_.stats.sta_samples.push_back({.lane=1,.hit=false,.sq_flag=true,.sq_value=3});
        } else if(elapsed==2)e.feedback_.stats.sta_samples.push_back({.lane=1,.hit=true,.sq_flag=true,.sq_value=3});
    };
    require(env.issue_store_address_until_tlb_hit(t,40), "matching miss/hit feedback did not finish");
    require(env.address_issue_cycles==std::vector<std::uint64_t>({0,13}), "STA retry used stale or unrelated feedback");
}
void sta_timeout() {
    RetirementFixture env;
    StoreTransaction t{.rob=7,.sq=3,.sq_flag=true,.address_lane=1};
    env.feedback_.stats.sta_samples.push_back({.lane=1,.hit=true,.sq_flag=true,.sq_value=3});
    env.on_tick=[&](auto &e) {e.feedback_.stats.sta_samples.push_back({.lane=0,.hit=false,.sq_flag=true,.sq_value=3});};
    require(!env.issue_store_address_until_tlb_hit(t,11), "unrelated feedback fabricated hit");
    require(env.address_issue_cycles.size()==1 && env.cycle()==11, "unmatched miss triggered retry");
}
void dispatch_snapshot() {
    memblock::scenarios::mixed::MixedScenario g;
    g.rob_offset=kRobEntries; g.lq_offset=kVirtualLoadQueueEntries; g.sq_offset=kStoreQueueEntries;
    auto store=g.make_store(0x80000000,0x42,StoreOp::sd,1);
    for(unsigned i=0;i<5;++i)g.make_load(0x80000100+8*i,LoadOp::ld,0,false);
    require(store.lq==0 && store.lq_flag && store.sq==0 && store.sq_flag && store.rob==0 && store.rob_flag,
            "builder did not freeze its dispatch cursors");
    RetirementFixture env;
    env.seed_loads(kVirtualLoadQueueEntries,0,kVirtualLoadQueueEntries);
    env.seed_stores(0xffff,kStoreQueueEntries,0,kStoreQueueEntries);
    require(env.enqueue_store(store), "frozen store snapshot rejected before younger loads dispatch");
    const auto &driven=env.dut_.last_enqueue;
    require(driven.need_alloc==2 && driven.lq_value==0 && driven.lq_flag && driven.sq_value==0 && driven.sq_flag,
            "typed dispatch lost pointer flags");
    require(driven.rob_value==0 && driven.rob_flag && driven.fu_type==kFuTypeStore, "typed dispatch lost ROB identity");
    require(env.queues_.scalar_store_target(RetirementFixture::scalar_store_key(store))==kStoreQueueEntries+1,
            "typed dispatch omitted store target");
    RetirementFixture late;
    late.seed_loads(kVirtualLoadQueueEntries+5,0,kVirtualLoadQueueEntries+5);
    late.seed_stores(0xffff,kStoreQueueEntries,0,kStoreQueueEntries);
    require(!late.enqueue_store(store) && late.dut_.enqueue_drives==0, "late dispatch accepted stale frozen snapshot");
}
void dispatch_cursors() {
    for(unsigned corrupted=0;corrupted<5;++corrupted) {
        RetirementFixture env;
        env.seed_loads(kVirtualLoadQueueEntries+11,4,kVirtualLoadQueueEntries+6);
        env.seed_stores(0xffff,kStoreQueueEntries+11,6,kStoreQueueEntries+4);
        StoreTransaction t{.address=0x80000000,.rob=5,.rob_flag=true,.lq=7,.lq_flag=true,.sq=5,.sq_flag=true};
        if(corrupted==1)++t.lq;
        if(corrupted==2)t.lq_flag=false;
        if(corrupted==3)++t.sq;
        if(corrupted==4)t.sq_flag=false;
        require(env.enqueue_store(t)==(corrupted==0), "typed dispatch cursor validation mismatch");
        require(env.dut_.enqueue_drives==(corrupted==0?1U:0U), "invalid cursor was driven to DUT");
        if(corrupted==0) {
            require(env.queues_.scalar_store_target(RetirementFixture::scalar_store_key(t))==kStoreQueueEntries+6,
                    "SQ target ignored prior cancellation");
            require(env.dut_.last_enqueue.lq_value==7 && env.dut_.last_enqueue.lq_flag &&
                    env.dut_.last_enqueue.sq_value==5 && env.dut_.last_enqueue.sq_flag,
                    "allocation-minus-cancellation cursors were not driven");
        }
    }
}
void vector_commit() {
    for(unsigned prior_dequeued:{2U,4U}) {
        RetirementFixture env;
        auto t=vector_store(); t.vm=false;t.mask_bits=0x0555;t.vstart=3;t.vl=11;
        for(unsigned b=0;b<24;++b)env.memory_.write_byte(t.address+b,0xa5);
        env.seed_stores(0xffff,6,6);
        env.seed_stores(RetirementFixture::vector_store_key(t),4,0,prior_dequeued);
        auto younger=vector_store(10,8);
        env.seed_stores(RetirementFixture::vector_store_key(younger),3);
        auto before=env.cycle();
        env.on_tick=[&](auto &e) {
            require(e.dut_.queues.backend.pending_ptr_value.value()==t.rob &&
                    e.dut_.queues.backend.pending_ptr_flag.value()==t.rob_flag,"vector commit drove wrong ROB head");
            for(unsigned b=0;b<24;++b)require(e.memory_.read_byte(t.address+b)==0xa5,"reference bytes changed before SQ target");
            if(e.queues_.sq_dequeued()<4)e.dut_.queues.retirement.sq_deq.set(1);
        };
        require(env.commit_vector_store(t,4), "recorded vector target did not retire");
        require(env.queues_.sq_dequeued()==4, "vector commit waited beyond its recorded target");
        require((env.cycle()==before)==(prior_dequeued==4), "already dequeued commit unnecessarily moved ROB head");
        require(!env.queues_.vector_store_target(RetirementFixture::vector_store_key(t)), "successful vector target retained");
        require(env.queues_.vector_store_target(RetirementFixture::vector_store_key(younger)).has_value(),"younger target consumed");
        require(env.dut_.queues.backend.scommit.nonzero_writes==0,"vector commit emitted scalar scommit");
        for(unsigned b=0;b<24;++b) {
            bool active=b>=3 && b<11 && ((0x0555>>b)&1U);
            require(env.memory_.read_byte(t.address+b)==(active?0x60+b:0xa5),"vector commit corrupted inactive/adjacent bytes");
        }
    }
}
void vector_commit_timeout() {
    RetirementFixture env;
    auto t=vector_store();
    for(unsigned b=0;b<16;++b)env.memory_.write_byte(t.address+b,0xa5);
    env.seed_stores(RetirementFixture::vector_store_key(t),4,0,1);
    require(!env.commit_vector_store(t,3),"missing vector dequeue fabricated commit");
    require(env.queues_.vector_store_target(RetirementFixture::vector_store_key(t))==4,"failed commit lost retry target");
    for(unsigned b=0;b<16;++b)require(env.memory_.read_byte(t.address+b)==0xa5,"failed vector commit updated reference");
    require(env.dut_.queues.backend.scommit.nonzero_writes==0,"failed vector commit emitted scalar scommit");
}
void vector_list() {
    RetirementFixture env;
    auto first=vector_store(9,kStoreQueueEntries-2);
    auto second=vector_store(10,2);second.sq_flag=true;
    auto unrelated=vector_store(11,20);
    for(const auto &t:{first,second,unrelated})env.vector_scoreboard_.expect(t,t.data);
    env.feedback_.vector_replay_requests_.push_back({.sq_value=40});
    env.feedback_.vector_replay_requests_.push_back({.lane=1,.sq_flag=true,.sq_value=0,.is_part_replay=true,.replay_mask=5,.replay_mb_index=2});
    env.feedback_.vector_replay_requests_.push_back({.sq_flag=true,.sq_value=3,.replay_mask=9});
    env.on_vector_issue=[](auto &e,const auto &t) {complete_vector(e,t);};
    require(env.run_until_vector_complete_with_replays(std::vector<VectorMemoryTransaction>{first,second},100,true),"list wait blocked on unrelated uop");
    require(env.vector_scoreboard_.pending(unrelated) && !env.vector_scoreboard_.pending(first) && !env.vector_scoreboard_.pending(second),"list wait consumed wrong expectations");
    require(env.feedback_.pending_replays()==1 && env.feedback_.vector_replay_requests_.front().sq_value==40,"list wait consumed unrelated replay");
    require(env.vector_issues.size()==2 && env.vector_issues[0].rob==first.rob && env.vector_issues[0].lane==1 &&
            env.vector_issues[0].is_part_replay && env.vector_issues[0].replay_mask==5 && env.vector_issues[0].replay_mb_index==2,
            "circular-range replay identity or metadata lost");
    require(env.dut_.queues.backend.scommit.nonzero_writes==0,"list replay emitted scalar scommit");
}
void vector_completed_replay() {
    RetirementFixture env;
    auto first=vector_store(9,4),second=vector_store(10,8);
    env.vector_scoreboard_.expect(first,first.data);env.vector_scoreboard_.expect(second,second.data);
    complete_vector(env,first);
    env.feedback_.vector_replay_requests_.push_back({.sq_value=4});
    require(!env.run_until_vector_complete_with_replays(std::vector<VectorMemoryTransaction>{first,second},8),"completed list member replay was accepted");
    require(env.vector_issues.empty() && env.vector_scoreboard_.pending(second),"completed-member replay issued work or consumed pending member");
    require(env.error_.message().find("already completed")!=std::string::npos,"completed replay lacked diagnostic");
}
void vector_held() {
    RetirementFixture env;
    auto t=vector_store(); t.rob_flag=true;
    auto younger=vector_store(10,8);
    env.vector_scoreboard_.expect(t,t.data);env.vector_scoreboard_.expect(younger,younger.data);
    env.feedback_.vector_replay_requests_.push_back({.lane=1,.sq_value=4,.is_part_replay=true,.replay_mask=3});
    env.on_vector_issue=[](auto &e,const auto &replay) {complete_vector(e,replay);};
    env.on_tick=[&](auto &e) {
        ++e.head_observations;
        require(e.dut_.queues.backend.pendingst.value()==1 && e.dut_.queues.backend.pending_ptr_value.value()==t.rob &&
                e.dut_.queues.backend.pending_ptr_flag.value()==t.rob_flag,"held replay lost pending ROB head");
    };
    require(env.run_until_vector_complete_with_replays(t,80,true,true),"held vector replay failed");
    require(env.vector_issues.size()==1 && env.head_observations>1,"held replay pacing changed");
    require(env.vector_scoreboard_.pending(younger),"held replay consumed younger obligation");
    require(env.dut_.queues.backend.pendingst.value()==0 && env.dut_.queues.backend.scommit.nonzero_writes==0,
            "held replay leaked pendingst or emitted scalar scommit");
}
int main(int argc,char **argv) {
    try {
        const std::string mode=argc>1?argv[1]:"";
        if(mode=="scalar_scoped")scalar_scoped();
        else if(mode=="scalar_data_pending")scalar_data_pending();
        else if(mode=="scalar_timeout")scalar_timeout();
        else if(mode=="sta_feedback")sta_feedback();
        else if(mode=="sta_timeout")sta_timeout();
        else if(mode=="dispatch_snapshot")dispatch_snapshot();
        else if(mode=="dispatch_cursors")dispatch_cursors();
        else if(mode=="vector_commit")vector_commit();
        else if(mode=="vector_commit_timeout")vector_commit_timeout();
        else if(mode=="vector_list")vector_list();
        else if(mode=="vector_completed_replay")vector_completed_replay();
        else if(mode=="vector_held")vector_held();
        else throw std::runtime_error("unknown fixture mode");
        return 0;
    } catch(const std::exception &error) {std::cerr<<error.what()<<'\n';return 1;}
}
