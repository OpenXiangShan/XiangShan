#include "memblock_env.hpp"

namespace memblock {
struct TestSignal {
    std::uint64_t scalar = 0;
    std::vector<unsigned char> payload = std::vector<unsigned char>(32, 0);
    void set(std::uint64_t value) { scalar = value; }
    void set_bytes(const std::vector<unsigned char> &value) { payload = value; }
    std::uint64_t value() const { return scalar; }
    bool boolean() const { return scalar != 0; }
    std::vector<unsigned char> bytes() const { return payload; }
};
struct TestChannel {
    TestSignal ready, valid, opcode, param, size, source, address, mask, data,
        corrupt, sink, denied, echo_is_keyword, user_req_source, user_vaddr,
        user_alias, user_need_hint;
};
struct DutAdapter {
    struct { TestChannel a, b, c, d, e; } dcache;
    std::uint64_t ticks = 0;
    std::uint64_t cycle() const { return ticks; }
};
#include "environment/dcache_agent.inc"
}
using namespace memblock;
constexpr std::uint64_t line = 0x80200000;
void require(bool value, const std::string &message)
{
    if (!value) throw std::runtime_error(message);
}
struct Fixture {
    SparseMemory bus, reference;
    TileLinkMemoryAgent manager{bus, reference};
    DutAdapter dut;
    std::vector<unsigned char> old_image, new_image;
    Fixture() : old_image(64, 0), new_image(64, 0) {
        for (unsigned b=0; b<64; ++b) {
            old_image[b] = 0x31 + b;
            new_image[b] = 0xa0 ^ b;
            bus.write_byte(line+b, 0);
            reference.write_reference_byte(line+b, old_image[b]);
        }
        dut.dcache.b.ready.set(1);
        dut.dcache.d.ready.set(1);
    }
    void step() {
        manager.drive(dut);
        manager.capture_before_tick(dut);
        manager.update_after_tick();
        ++dut.ticks;
    }
    void ok() { require(manager.ok(), manager.error()); }
    void acquire(unsigned alias, unsigned source=0, unsigned grow=1,
                 std::uint64_t address=line) {
        auto &a = dut.dcache.a;
        a.valid.set(1); a.opcode.set(6); a.param.set(grow); a.size.set(6);
        a.source.set(source); a.address.set(address); a.mask.set(0xffffffff);
        a.user_req_source.set(2); a.user_alias.set(alias);
        a.user_vaddr.set(((address & 0xfff) | (alias << 12)) >> 6);
        step(); a.valid.set(0);
    }
    void ack(unsigned sink=1) {
        dut.dcache.e.sink.set(sink); dut.dcache.e.valid.set(1);
        step(); dut.dcache.e.valid.set(0);
    }
    void owner(unsigned grow=1) {
        acquire(1, 0, grow);
        step(); // First D
        ack(); // Final D and E
        ok();
        require(manager.responses_idle() && manager.grant_acks_idle(), "initial grant not drained");
        require(manager.checkpoint_alias_reference(line, 64), manager.error());
    }
    void next_alias() {
        // The new store's reference commit precedes its cache Acquire. Old
        // recall bytes must still compare with the independent old epoch.
        for (unsigned b=0; b<64; ++b) {
            reference.write_reference_byte(line+b, new_image[b]);
            manager.note_reference_write(line+b, line+0x2000+b, new_image[b]);
        }
        acquire(2);
        manager.drive(dut);
        require(dut.dcache.b.valid.boolean(), "recall B missing");
        require(dut.dcache.b.data.bytes()[0] == 3, "B did not use old VA alias/needData");
        require(!dut.dcache.d.valid.boolean(), "replacement D escaped before recall");
        step(); // Accept B
    }
    void c(unsigned opcode, unsigned report, unsigned beat=0, unsigned source=9,
           const std::vector<unsigned char> *image=nullptr) {
        auto &c = dut.dcache.c;
        c.valid.set(1); c.opcode.set(opcode); c.param.set(report); c.size.set(6);
        c.source.set(source); c.address.set(line);
        // C user fields deliberately carry no meaningful alias.
        c.user_alias.set(3); c.user_vaddr.set(0xdead);
        const auto &bytes = image ? *image : old_image;
        c.data.set_bytes(std::vector<unsigned char>(bytes.begin()+32*beat, bytes.begin()+32*(beat+1)));
        step(); c.valid.set(0);
    }
    void check_recovered_grant() {
        // A ReleaseAck may precede the new grant.
        for (unsigned n=0; n<4; ++n) {
            manager.drive(dut);
            if (dut.dcache.d.valid.boolean() && dut.dcache.d.opcode.value()==5) {
                require(dut.dcache.d.data.bytes() == std::vector<unsigned char>(old_image.begin(),old_image.begin()+32),
                        "replacement GrantData used stale backing/reference-ahead bytes");
                return;
            }
            step();
        }
        throw std::runtime_error("replacement grant missing");
    }
};

void exhaustive_versions()
{
    struct Update { unsigned offset, count; unsigned char value; };
    const std::array<Update,4> updates{{{8,8,0xa1},{8,2,0xb2},{40,8,0xc3},{44,2,0xd4}}};
    std::vector<std::vector<unsigned char>> candidates;
    std::set<std::vector<unsigned char>> legal;
    Fixture initial;
    // Independent forward enumeration of applied-store subsets. Preserve the
    // ordered overlap relation, and compare complete line images (a newer
    // overwrite can make distinct selected subsets observationally identical).
    for (unsigned mask=0; mask<16; ++mask) {
        auto image=initial.old_image;
        bool allowed=true;
        for (unsigned i=0; i<updates.size(); ++i) {
            if (((mask>>i)&1)==0) continue;
            const auto &u=updates[i];
            for (unsigned j=0; j<i; ++j) {
                const auto &v=updates[j];
                if (u.offset<v.offset+v.count && v.offset<u.offset+u.count &&
                    ((mask>>j)&1)==0) allowed=false;
            }
            std::fill(image.begin()+u.offset,image.begin()+u.offset+u.count,u.value);
        }
        candidates.push_back(image);
        if (allowed) legal.insert(image);
        auto torn=image; torn[10]^=0x80; candidates.push_back(torn);
    }
    for (const auto &image:candidates) {
        Fixture f; f.owner();
        for (const auto &u:updates) {
            std::vector<unsigned char> bytes(u.count,u.value);
            for (unsigned b=0;b<u.count;++b)
                f.reference.write_reference_byte(line+u.offset+b,u.value);
            f.manager.note_reference_store(line+u.offset,line+0x1000+u.offset,bytes);
        }
        f.c(7,1,0,9,&image); f.c(7,1,1,9,&image);
        require(f.manager.ok()==legal.contains(image),
                "production release oracle disagrees with independent forward version enumeration");
    }
}

int main(int argc, char **argv)
{
    try {
        const std::string test = argc>1 ? argv[1] : "dirty";
        if (test == "release-version-exhaustive") {
            exhaustive_versions(); return 0;
        }
        Fixture f;
        if (test == "early-e") {
            f.acquire(1);
            f.ack(); // E legal on first D
            f.acquire(2); // Final D plus new A, with no independent checkpoint
            require(!f.manager.ok(), "uncheckpointed synonym should be classified");
            require(f.manager.error().find("reference checkpoint") != std::string::npos,
                    "early E incorrectly rejected");
        } else if (test == "final-d-before-e") {
            f.acquire(1); f.step(); f.step();
            f.acquire(2);
            f.manager.drive(f.dut);
            require(!f.dut.dcache.b.valid.boolean(), "B escaped before old E");
            require(!f.dut.dcache.d.valid.boolean(), "sink reused before E");
            f.acquire(0, 1, 0, line+64); // unrelated line progresses
            f.manager.drive(f.dut);
            require(f.dut.dcache.d.valid.boolean() && f.dut.dcache.d.source.value()==1,
                    "unrelated PA blocked behind pending E");
            f.ok();
        } else if (test == "early-e-late-d") {
            f.acquire(1); f.ack();
            f.dut.dcache.d.ready.set(0);
            require(f.manager.request_probe(line,2,true,1,f.old_image), "probe enqueue failed");
            f.step();
            require(!f.dut.dcache.b.valid.boolean(), "B escaped before final D after early E");
            f.dut.dcache.d.ready.set(1); f.step(); f.step();
            require(f.dut.dcache.b.valid.boolean(), "B failed to follow final D and E");
            f.ok();
        } else if (test == "serial-probes") {
            f.owner();
            require(f.manager.request_probe(line,2,true,1,f.old_image), "first B failed");
            require(f.manager.request_probe(line,2,false,5), "second B failed");
            f.step(); f.step();
            require(!f.dut.dcache.b.valid.boolean(), "same PA second B presented before first C");
            f.c(5,1,0); f.c(5,1,1); f.step();
            require(f.dut.dcache.b.valid.boolean(), "second B did not follow completed first C");
            f.c(4,5); f.ok();
        } else if (test.starts_with("release-version-")) {
            f.owner();
            auto returned = f.old_image;
            const auto write = [&](unsigned offset, unsigned count, unsigned char value) {
                std::vector<unsigned char> bytes(count,value);
                for (unsigned b=0; b<count; ++b)
                    f.reference.write_reference_byte(line+offset+b, value);
                f.manager.note_reference_store(line+offset,line+0x1000+offset,bytes);
            };
            const bool midbeat = test == "release-version-midbeat" ||
                                 test == "release-version-late-value";
            if (!midbeat) write(8,8,0xa7);
            if (test == "release-version-applied" || test == "release-version-explicit")
                std::fill(returned.begin()+8,returned.begin()+16,0xa7);
            if (test == "release-version-torn")
                std::fill(returned.begin()+8,returned.begin()+12,0xa7);
            if (test == "release-version-overlap-order") {
                write(8,2,0xb8);
                std::fill(returned.begin()+8,returned.begin()+10,0xb8);
            }
            if (test == "release-version-disjoint") {
                write(40,8,0xc9);
                std::fill(returned.begin()+40,returned.begin()+48,0xc9);
            }
            if (test == "release-version-explicit")
                f.manager.expect_release_line(line,f.old_image);
            f.c(7,1,0,9,&returned);
            if (midbeat) {
                write(40,8,0xc9);
                if (test == "release-version-late-value")
                    std::fill(returned.begin()+40,returned.begin()+48,0xc9);
            }
            f.c(7,1,1,9,&returned);
            const bool reject = test == "release-version-torn" ||
                test == "release-version-overlap-order" ||
                test == "release-version-late-value" || test == "release-version-explicit";
            require(f.manager.ok() != reject, "voluntary release version decision incorrect: " + f.manager.error());
            if (!reject) {
                require(f.manager.responses_idle()==false, "missing ReleaseAck obligation");
                f.step(); f.ok();
                require(f.manager.responses_idle(), "versioned ReleaseAck did not complete");
            }
        } else if (test == "reset") {
            f.owner(); f.next_alias();
            f.manager.reset_link_state();
            require(f.manager.responses_idle() && f.manager.probes_idle() && f.manager.grant_acks_idle(),
                    "reset retained transient alias state");
            f.acquire(3); f.step(); f.ack(); f.ok();
        } else {
            f.owner(test == "branch" || test == "branch-upgrade" ? 0 : 1);
            if (test == "same-alias-write") {
                f.old_image[7] ^= 0x55;
                f.reference.write_reference_byte(line+7,f.old_image[7]);
                f.manager.note_reference_write(line+7,line+0x1007,f.old_image[7]);
                require(f.manager.checkpoint_alias_reference(line,64), "drained write checkpoint failed");
            }
            f.next_alias();
            if (test == "release-race" || test == "ntonn-release") {
                if (test == "ntonn-release") {
                    f.c(4,5); // Even an overtaking NtoN cannot discard a late release.
                    f.manager.drive(f.dut);
                    require(!f.dut.dcache.d.valid.boolean(), "NtoN prematurely discarded owner");
                }
                f.c(7,1,0); f.c(7,1,1);
                f.manager.drive(f.dut);
                require(f.dut.dcache.d.valid.boolean() && f.dut.dcache.d.opcode.value()==6,
                        "ReleaseAck held behind alias Acquire/Probe");
                if (test == "release-race") { f.step(); f.c(4,5); }
            } else if (test == "bad-byte") {
                auto bad=f.old_image; bad[31]^=1;
                f.c(5,1,0,9,&bad);
                require(!f.manager.ok(), "bad recall data escaped independent oracle");
                return 0;
            } else if (test == "changed-source" || test == "changed-report") {
                f.c(5,1,0);
                f.c(5,test == "changed-report" ? 2 : 1,1,test == "changed-source" ? 10 : 9);
                require(!f.manager.ok(), "changed multibeat identity escaped");
                return 0;
            } else if (test == "branch-upgrade" || test == "missing-data") {
                f.c(test == "missing-data" ? 4 : 5,1,0);
                require(!f.manager.ok(), "illegal automatic report/opcode escaped");
                return 0;
            } else {
                f.c(5,test == "branch" ? 2 : 1,0);
                f.manager.drive(f.dut);
                require(!f.dut.dcache.d.valid.boolean(), "replacement grant escaped after partial C");
                f.c(5,test == "branch" ? 2 : 1,1);
            }
            f.ok();
            f.check_recovered_grant();
            require(f.manager.alias_recalls()==1 && f.manager.alias_recall_responses()==1,
                    "alias recall not accounted exactly once");
            f.step(); f.ack(); f.ok();
            require(f.manager.responses_idle() && f.manager.probes_idle() && f.manager.grant_acks_idle(),
                    "recall did not drain");
            if (test == "duplicate") {
                f.c(4,5);
                require(!f.manager.ok(), "duplicate ProbeAck escaped");
            }
        }
        return 0;
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n'; return 1;
    }
}
