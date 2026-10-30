#include "memblock_env.hpp"
#include "scenarios/shared.hpp"
#include <iostream>
#include <map>
#include <random>
#include <set>
namespace memblock {
struct Pin {
    std::uint64_t v = 0;
    std::vector<unsigned char> payload = std::vector<unsigned char>(32);
    void set(std::uint64_t value) { v = value; }
    void set_bytes(const std::vector<unsigned char> &value) { payload = value; }
    auto value() const { return v; }
    bool boolean() const { return v != 0; }
    auto bytes() const { return payload; }
};
struct DutAdapter {
    struct Channel { Pin valid, ready, opcode, param, size, source, address, mask, data,
        denied, corrupt, sink, echo_is_keyword, user_req_source, user_vaddr, user_alias, user_need_hint; };
    struct { Channel a, b, c, d, e; } dcache;
    std::uint64_t ticks = 0;
    auto cycle() const { return ticks; }
};
#include "environment/dcache_agent.inc"
}
using namespace memblock;
static void require(bool value, const std::string &why) { if (!value) throw std::runtime_error(why); }
struct Fixture {
    SparseMemory memory, reference;
    TileLinkMemoryAgent manager{memory, reference};
    DutAdapter dut;
    struct Expected {
        unsigned opcode, param, size, source, sink, order, next = 0, beats = 1;
        bool keyword = false, denied = false;
        std::optional<DcacheCorruptBeat> corrupt;
        std::vector<unsigned char> data;
    };
    // TileLink C and A source namespaces may contain the same numeric source.
    std::map<unsigned, Expected> a_expected, c_expected;
    std::optional<Expected> a_offer, c_offer;
    std::optional<unsigned> active;
    std::set<unsigned> acknowledgements;
    std::optional<unsigned> offered_ack;
    std::vector<unsigned> accepted, completed;
    std::vector<std::uint64_t> stalled;
    unsigned next_order = 0, inversions = 0, max_older = 0, checked_beats = 0;
    std::mt19937 rng;
    bool early_ack = true;
    Fixture(unsigned seed, unsigned rate, bool backpressure = false) : rng(seed) {
        manager.configure_backpressure(seed, backpressure);
        manager.configure_response_order(seed ^ 0x196dU, rate);
        dut.dcache.b.ready.set(1);
    }
    std::vector<std::uint64_t> witness() const {
        const auto &d=dut.dcache.d;
        std::vector<std::uint64_t> v{d.opcode.value(),d.param.value(),d.size.value(),d.source.value(),d.sink.value(),
            d.denied.value(),d.corrupt.value(),d.echo_is_keyword.value()};
        for (auto b:d.data.bytes()) v.push_back(b);
        return v;
    }
    void step(bool ready=true) {
        auto &d=dut.dcache.d; auto &e=dut.dcache.e;
        d.ready.set(ready); manager.drive(dut);
        if (!stalled.empty()) require(d.valid.boolean() && witness()==stalled,"D changed while stalled");
        stalled=d.valid.boolean()&&!ready?witness():std::vector<std::uint64_t>{};
        if (d.valid.boolean()&&ready) {
            const unsigned source=d.source.value(), opcode=d.opcode.value();
            const unsigned id=(opcode==6?256U:0U)+source;
            auto &owners=opcode==6?c_expected:a_expected;
            require(owners.count(source),"D has no independently accepted owner");
            auto &x=owners.at(source);
            require(!active||*active==id,"D messages interleaved"); active=id;
            require(opcode==x.opcode&&d.param.value()==x.param&&d.size.value()==x.size&&d.sink.value()==x.sink&&
                d.echo_is_keyword.boolean()==x.keyword&&d.denied.boolean()==x.denied,"D identity/namespace mismatch");
            const bool last=x.next+1==x.beats;
            const bool corrupt=opcode==5||opcode==1 ? x.denied||(x.corrupt&&(*x.corrupt==DcacheCorruptBeat::all||
                (*x.corrupt==DcacheCorruptBeat::first&&x.next==0)||(*x.corrupt==DcacheCorruptBeat::last&&last))) : false;
            require(d.corrupt.boolean()==corrupt,"D corrupt ownership changed");
            const auto data=d.data.bytes();
            if (opcode==5||opcode==1) {
                const unsigned beat=x.keyword&&opcode==5?x.next^1U:x.next;
                for (unsigned b=0;b<32;++b) require(data.at(b)==x.data.at(beat*32U+b),"D payload/snapshot/beat order mismatch");
            }
            if ((opcode==4||opcode==5)&&(early_ack?x.next==0:last)) acknowledgements.insert(x.sink);
            ++checked_beats;
            if (++x.next==x.beats) {
                if (opcode!=6) {
                    const unsigned older=std::count_if(a_expected.begin(),a_expected.end(),[&](const auto &other){return other.second.order<x.order;});
                    inversions+=older!=0;max_older=std::max(max_older,older);
                }
                completed.push_back(id);owners.erase(source);active.reset();
            }
        }
        if (!offered_ack&&!acknowledgements.empty()&&rng()%3U!=0) offered_ack=*acknowledgements.begin();
        e.valid.set(offered_ack.has_value());if(offered_ack)e.sink.set(*offered_ack);
        const bool ack_fire=offered_ack&&e.ready.boolean();
        const auto accept=[&](auto &channel,auto &offer,auto &owners,unsigned ns) {
            if(channel.valid.boolean()&&channel.ready.boolean()) {
                require(offer.has_value()&&!owners.count(channel.source.value()),"accepted source reuse/unowned request");
                auto x=*offer;x.order=next_order++;owners.emplace(x.source,std::move(x));accepted.push_back(ns+channel.source.value());offer.reset();
            }
        };
        accept(dut.dcache.a,a_offer,a_expected,0);
        accept(dut.dcache.c,c_offer,c_expected,256);
        manager.capture_before_tick(dut);manager.update_after_tick();++dut.ticks;
        require(manager.ok(),manager.error());
        if(ack_fire){acknowledgements.erase(*offered_ack);offered_ack.reset();}
    }
    void request(unsigned source,unsigned opcode=6,bool keyword=false,unsigned delay=0,
                 std::optional<std::uint64_t> target={},bool denied=false,std::optional<DcacheCorruptBeat> corrupt={}) {
        const auto address=target.value_or(0x91000000ULL+source*128U);
        for(unsigned b=0;b<64;++b)memory.write_byte(address+b,(source*37U+b*19U)^0x83U);
        Expected x{opcode==6?5U:opcode==7?4U:1U,0,6,source,opcode==6||opcode==7?1U+source:0U,0};
        x.keyword=keyword;x.denied=denied;x.corrupt=corrupt;x.beats=opcode==6?2U:1U;
        for(unsigned b=0;b<64;++b)x.data.push_back(memory.read_byte(address+b));
        a_offer=x;manager.force_next_response_delay(delay);
        manager.force_next_interbeat_delay(source%4U);
        if(denied||corrupt)manager.inject_next_response_error(denied,corrupt.has_value(),corrupt.value_or(DcacheCorruptBeat::all));
        auto &a=dut.dcache.a;a.valid.set(1);a.opcode.set(opcode);a.param.set(opcode==7?2:1);a.source.set(source);a.size.set(6);
        a.address.set(address);a.mask.set(0xffffffff);a.user_req_source.set(2);a.user_alias.set(0);a.user_vaddr.set((address&0xfff)>>6);a.echo_is_keyword.set(keyword);
        for(unsigned n=0;a_offer&&n<1000;++n)step(false);
        require(!a_offer,"A acceptance timed out");a.valid.set(0);
    }
    void release(unsigned source,unsigned n) {
        c_offer=Expected{6,0,6,source,0,0};
        auto &c=dut.dcache.c;c.valid.set(1);c.opcode.set(6);c.param.set(2);c.size.set(6);c.source.set(source);
        c.address.set(0x93000000ULL+n*64U);c.echo_is_keyword.set(0);
        for(unsigned edge=0;c_offer&&edge<1000;++edge)step(false);
        require(!c_offer,"C acceptance timed out");c.valid.set(0);
    }
    void drain() {
        for(unsigned n=0;n<20000&&(!manager.responses_idle()||!manager.grant_acks_idle());++n)step(rng()%4U!=0);
        require(manager.responses_idle()&&manager.grant_acks_idle()&&a_expected.empty()&&c_expected.empty()&&
            !active&&acknowledgements.empty()&&!offered_ack,"lost/duplicate/incomplete response or E");
        const auto &s=manager.response_order_stats();
        require(s[0]==accepted.size()&&s[4]==completed.size()&&s[6]==inversions&&s[7]==max_older,
            "manager counters differ from independent accepted/completion ledger");
    }
};
int main(int argc,char **argv) {
 try {
    require(argc>=2,"mode required");const std::string mode=argv[1];
    if(mode=="constraints") {
        auto c=memblock::scenarios::RandomConstraints::preset("spec");require(argc==3,"override required");c.apply(argv[2]);c.validate();std::cout<<c.summary()<<'\n';
    } else if(mode=="random"||mode=="fifo"||mode=="backpressure") {
        std::set<std::vector<unsigned>> schedules;unsigned nonfront=0,beats=0;
        for(unsigned seed=1;seed<=128;++seed) {
            Fixture f(seed,mode=="fifo"?0:1000,mode=="backpressure");f.early_ack=seed&1;
            for(unsigned source=0;source<8;++source) {
                f.request(source,source%3U?6:7,source%3U&&source%2U,source==0?5:source%4U);
                if(source<4)f.release(source,source);
            }
            // Freeze every queued data image even when the backing changes.
            for(unsigned source=0;source<8;++source)for(unsigned b=0;b<64;++b)f.memory.write_byte(0x91000000ULL+source*128U+b,0xff);
            f.drain();if(mode=="fifo")require(f.completed==f.accepted,"disabled queue ordering changed");
            require(f.completed.front()==0,"first-delay/stalled active message changed");
            schedules.insert(f.completed);nonfront+=f.manager.response_order_stats()[3];beats+=f.checked_beats;
        }
        require(mode=="fifo"?schedules.size()==1:schedules.size()>100,"structural source-order diversity missing");
        std::cout<<"schedules="<<schedules.size()<<" nonfront="<<nonfront<<" checked_beats="<<beats<<'\n';
    } else if(mode=="held") {
        Fixture f(51,1000);f.manager.hold_response_at(0x91000000);f.request(0);f.request(1);f.request(2);
        require(f.manager.response_held_at(0x91000000),"held message missing");
        for(unsigned n=0;n<1000&&f.a_expected.size()>1;++n)f.step();
        require(f.a_expected.size()==1&&f.a_expected.count(0),"held message escaped arbitration");
        require(f.manager.response_order_stats()[6]>0,"FIFO/deferred accepted-age distinction unobserved");
        f.manager.release_held_response_at(0x91000000);f.drain();
    } else if(mode=="errors") {
        for(unsigned seed=1;seed<=24;++seed) {
            Fixture f(seed,1000);
            f.request(0,6,false,2,{},true);
            f.request(1,6,true,0,{},false,DcacheCorruptBeat::first);
            f.request(2,6,false,0,{},false,DcacheCorruptBeat::last);
            f.request(3,6,true,0,{},false,DcacheCorruptBeat::all);
            f.request(4,7,false,0,{},true);f.release(0,0);f.drain();
        }
    } else if(mode=="config") {
        Fixture f(1,1000);bool bad=false;
        try{f.manager.configure_response_order(1,1001);}catch(const std::invalid_argument&){bad=true;}require(bad,"invalid rate accepted");
        f.request(0);bad=false;try{f.manager.configure_response_order(1,1);}catch(const std::invalid_argument&){bad=true;}require(bad,"live config accepted");
        f.manager.reset_link_state();require(f.manager.responses_idle()&&f.manager.grant_acks_idle(),"reset kept link obligations");
        f.manager.configure_response_order(1,1000);f.a_expected.clear();f.accepted.clear();f.stalled.clear();f.active.reset();f.request(1);f.drain();
    } else if(mode=="deferred") {
        Fixture f(63,1000);const auto line=0x91000000ULL;
        f.manager.hold_response_at(line);f.request(0,6,false,0,line);f.request(1,6,false,0,line);f.request(2);
        for(unsigned n=0;n<1000&&f.a_expected.count(2);++n)f.step();
        require(f.completed.size()==1&&f.completed[0]==2&&f.a_expected.size()==2,"same-line grant escaped final-D/E gate");
        f.manager.release_held_response_at(line);f.drain();
        require(f.completed[1]==0&&f.completed[2]==1,"same-line source order changed");
    } else throw std::runtime_error("unknown mode");
    std::cout<<"PASS "<<mode<<'\n';return 0;
 }catch(const std::exception&e){std::cerr<<e.what()<<'\n';return 1;}
}
