#include "memblock_env.hpp"
#include "scenarios/shared.hpp"
#include <algorithm>
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
    struct Channel { Pin valid, ready, opcode, param, size, source, address, data, denied, corrupt, sink; };
    struct { Channel a, d; } ptw_memory;
};
#include "environment/ptw_agent.inc"
}
using namespace memblock;
static void require(bool value, const std::string &why) { if (!value) throw std::runtime_error(why); }
struct Fixture {
    SparseMemory memory;
    PtwMemoryAgent manager{memory};
    DutAdapter dut;
    struct Expected {
        unsigned source, size, next = 0; std::uint64_t address;
        std::vector<unsigned char> data; bool denied = false;
        std::optional<PtwCorruptBeat> corrupt;
        unsigned order = 0;
    };
    std::map<unsigned, Expected> expected;
    std::optional<unsigned> active;
    std::vector<unsigned> accepted, completed;
    unsigned edge = 0, inversion_completions = 0, max_older = 0;
    bool inject_denied = false;
    std::optional<PtwCorruptBeat> inject_corrupt;
    std::vector<std::uint64_t> stalled;
    Fixture(unsigned seed, unsigned rate) {
        manager.configure_backpressure(seed, false);
        manager.configure_response_order(seed ^ 0x719U, rate);
    }
    std::vector<std::uint64_t> witness() const {
        const auto &d = dut.ptw_memory.d;
        std::vector<std::uint64_t> result{d.opcode.value(),d.param.value(),d.size.value(),d.source.value(),d.sink.value(),d.denied.value(),d.corrupt.value()};
        for (auto b : d.data.bytes()) result.push_back(b);
        return result;
    }
    void step(bool ready = true, bool golden = true) {
        auto &a = dut.ptw_memory.a; auto &d = dut.ptw_memory.d;
        d.ready.set(ready); manager.drive(dut);
        if (!stalled.empty()) require(d.valid.boolean() && witness() == stalled, "D changed while stalled");
        stalled = d.valid.boolean() && !ready ? witness() : std::vector<std::uint64_t>{};
        if (d.valid.boolean() && ready) {
            const unsigned source = d.source.value();
            require(expected.count(source), "D has no accepted request owner");
            auto &e = expected.at(source);
            require(!active || *active == source, "D messages interleaved"); active = source;
            require(d.opcode.value() == 1 && d.param.value() == 0 && d.size.value() == e.size &&
                d.sink.value() == 0 && d.denied.boolean() == e.denied, "D identity mismatch");
            const bool last = e.next + 1U == (e.size > 5 ? 2U : 1U);
            const bool corrupt = e.denied || (e.corrupt && (*e.corrupt == PtwCorruptBeat::all ||
                (*e.corrupt == PtwCorruptBeat::first && e.next == 0) ||
                (*e.corrupt == PtwCorruptBeat::last && last)));
            require(d.corrupt.boolean() == corrupt, "D corrupt beat selection changed");
            const auto data = d.data.bytes();
            for (unsigned b = 0; b < 32; ++b)
                require(data.at(b) == e.data.at(e.next * 32U + b), "D payload/snapshot/beat order mismatch");
            if (++e.next == (e.size > 5 ? 2U : 1U)) {
                const unsigned older = std::count_if(expected.begin(), expected.end(),
                    [&](const auto &entry) { return entry.second.order < e.order; });
                inversion_completions += older != 0; max_older = std::max(max_older, older);
                completed.push_back(source); expected.erase(source); active.reset();
            }
        }
        if (a.valid.boolean() && a.ready.boolean()) {
            const unsigned source = a.source.value(), size = a.size.value();
            if (golden) require(!expected.count(source), "accepted active source reuse");
            Expected e{source,size,0,a.address.value(),{},inject_denied,inject_corrupt,unsigned(accepted.size())};
            for (unsigned b = 0; b < (size > 5 ? 64U : 32U); ++b) e.data.push_back(memory.read_byte(e.address+b));
            expected[source] = e; accepted.push_back(source);
        }
        manager.capture_before_tick(dut); manager.update_after_tick(); ++edge;
        if (golden) require(manager.ok(), manager.error());
    }
    void request(unsigned source, unsigned size, unsigned delay = 0,
                 bool denied = false, std::optional<PtwCorruptBeat> corrupt = {}) {
        const auto address = 0x91000000ULL + source * 128U;
        for (unsigned b = 0; b < 64; ++b) memory.write_byte(address+b, (source*37U+b*19U)^0x83U);
        manager.force_next_response_delay(delay);
        inject_denied = denied; inject_corrupt = corrupt;
        if (denied || corrupt) manager.inject_response_error_after(0, denied, corrupt.has_value(),
            corrupt.value_or(PtwCorruptBeat::all));
        auto &a = dut.ptw_memory.a;
        a.valid.set(1); a.opcode.set(4); a.param.set(0); a.source.set(source); a.size.set(size); a.address.set(address);
        step(false); a.valid.set(0); inject_denied = false; inject_corrupt.reset();
        // Expected response must use captured bytes even if backing changes.
        for (unsigned b = 0; b < 64; ++b) memory.write_byte(address+b, 0xff);
    }
    void drain(unsigned seed = 0) {
        std::mt19937 rng(seed);
        for (unsigned i=0; i<20000 && !manager.idle(); ++i) step(rng()%4U != 0);
        require(manager.idle() && expected.empty() && !active, "lost/duplicate/incomplete response");
    }
};
int main(int argc, char **argv) {
    try {
        require(argc >= 2, "mode required"); const std::string mode = argv[1];
        if (mode == "constraints") {
            auto c = memblock::scenarios::RandomConstraints::preset("spec");
            require(argc == 3, "one override required");c.apply(argv[2]);c.validate();
            std::cout << c.summary() << '\n';
        } else if (mode == "random" || mode == "fifo") {
            std::set<std::vector<unsigned>> schedules; std::uint64_t reorders=0;
            const unsigned rate = mode == "fifo" ? 0 : 1000;
            for (unsigned seed=1;seed<=128;++seed) {
                Fixture f(seed,rate);
                for (unsigned source=0;source<8;++source) f.request(source, source%2U ? 5U:6U);
                f.drain(seed);
                require(f.completed.size()==8 && f.manager.response_order_stats()[0]==8 &&
                    f.manager.response_order_stats()[4]==8, "message-start/completion conservation");
                require(f.manager.response_order_stats()[6] == f.inversion_completions &&
                    f.manager.response_order_stats()[7] == f.max_older, "completion inversions differ from accepted-order ledger");
                require(f.completed.front()==0, "stalled active response was replaced");
                if (rate==0) require(f.completed==f.accepted,"disabled response order changed");
                schedules.insert(f.completed); reorders+=f.manager.response_order_stats()[3];
                // Final-D frees producer-owned source; resetting clears active
                // messages but keeps the selected distribution.
                f.request(0,6);f.drain(seed+1);f.manager.reset_link_state();
                require(f.manager.idle(),"reset retained response state");
            }
            require(rate==0 ? schedules.size()==1 && reorders==0 : schedules.size()>100 && reorders>0,
                "cross-source message permutations did not vary");
            std::cout<<"schedules="<<schedules.size()<<" nonfront="<<reorders<<"\n";
        } else if (mode == "errors") {
            for (unsigned seed=1;seed<=32;++seed) {
                Fixture f(seed,1000);
                f.request(0,6,2,true);f.request(1,6,3,false,PtwCorruptBeat::first);
                f.request(2,6,1,false,PtwCorruptBeat::last);f.request(3,5,0,false,PtwCorruptBeat::all);
                f.drain(seed);
                require(f.manager.denied_d_beats()==2 && f.manager.corrupt_d_beats()==5,
                    "error request/beat counts changed under permutation");
            }
        } else if (mode == "sameedge") {
            Fixture f(1,1000);f.request(0,6);f.step(true);
            auto &a=f.dut.ptw_memory.a;
            a.valid.set(1);a.opcode.set(4);a.param.set(0);a.source.set(0);a.size.set(6);a.address.set(0x91000000);
            f.step(true);a.valid.set(0);f.drain();
            require(f.completed==std::vector<unsigned>({0,0}),"final-D/new-A source reuse lost work");
        } else if (mode == "reuse") {
            Fixture f(1,1000);f.request(2,6);auto &a=f.dut.ptw_memory.a;
            a.valid.set(1);a.opcode.set(4);a.param.set(0);a.size.set(6);a.source.set(2);a.address.set(0x91000100);
            f.step(false,false);require(!f.manager.ok(),"active source reuse accepted");
        } else if (mode == "config") {
            Fixture f(1,0);bool rejected=false;try{f.manager.configure_response_order(1,1001);}catch(const std::invalid_argument &){rejected=true;}
            require(rejected,"invalid rate accepted");f.request(0,6);rejected=false;
            try{f.manager.configure_response_order(1,0);}catch(const std::invalid_argument &){rejected=true;}
            require(rejected,"live reconfiguration accepted");f.manager.reset_link_state();f.expected.clear();f.stalled.clear();
            f.request(1,5);f.drain();
        } else throw std::runtime_error("unknown mode");
        return 0;
    } catch(const std::exception &e) { std::cerr<<e.what()<<'\n';return 1; }
}
