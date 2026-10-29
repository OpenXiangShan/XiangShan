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
                 std::uint64_t address=line, unsigned opcode=6) {
        auto &a = dut.dcache.a;
        a.valid.set(1); a.opcode.set(opcode); a.param.set(grow); a.size.set(6);
        a.source.set(source); a.address.set(address); a.mask.set(0xffffffff);
        a.user_req_source.set(2); a.user_alias.set(alias);
        a.user_vaddr.set(((address & 0xfff) | (alias << 12)) >> 6);
        step(); a.valid.set(0);
    }
    void ack(unsigned sink=1) {
        dut.dcache.e.sink.set(sink); dut.dcache.e.valid.set(1);
        step(); dut.dcache.e.valid.set(0);
    }
    void expect_line_idle(bool expected, const std::string &state,
                          std::uint64_t address=line) const {
        // Exercise physical-line normalization through the production query.
        for (unsigned offset : {0U, 1U, 31U, 63U}) {
            require(manager.line_grants_idle(address + offset) == expected,
                    state + " (offset " + std::to_string(offset) + ")");
        }
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

void line_grants_lifecycle(const std::string &test)
{
    Fixture f;
    f.expect_line_idle(true, "initial line was not idle");
    if (test == "line-grants-holds") {
        f.manager.hold_response_at(line + 13);
        f.expect_line_idle(false, "armed target hold reported idle");
        f.expect_line_idle(true, "unrelated armed hold blocked line", line + 64);
        f.manager.hold_response_at(line + 69);
        f.acquire(1);
        require(f.manager.response_held_at(line), "target response was not held");
        f.expect_line_idle(false, "held Grant reported idle");
        f.manager.schedule_held_response_release_at(line + 7, 2);
        f.step();
        require(f.manager.response_held_at(line), "scheduled hold released too early");
        f.expect_line_idle(false, "scheduled held Grant reported idle");
        f.step();
        require(!f.manager.response_held_at(line), "scheduled hold did not release");
        f.expect_line_idle(false, "released, queued Grant reported idle");
        f.ack(); // First D and E; final D is still queued.
        f.expect_line_idle(false, "held Grant retired before final D");
        f.step();
        f.expect_line_idle(true, "released held Grant did not retire");
        f.expect_line_idle(false, "unrelated hold was lost", line + 64);
        require(!f.manager.responses_idle(), "unrelated armed hold did not stay busy");
    } else if (test == "line-grants-final-d-before-e") {
        f.manager.force_next_response_delay(2);
        f.manager.force_next_interbeat_delay(1);
        f.acquire(1);
        f.expect_line_idle(false, "delayed Grant reported idle");
        f.step(); f.step();
        require(!f.dut.dcache.d.valid.boolean(), "first D delay was not exercised");
        f.expect_line_idle(false, "queued Grant reported idle");
        f.dut.dcache.d.ready.set(0); f.step();
        require(f.dut.dcache.d.valid.boolean() && f.manager.grant_data_beat_count() == 0,
                "first D stall was not exercised");
        f.expect_line_idle(false, "stalled first D reported idle");
        f.dut.dcache.d.ready.set(1); f.step();
        require(f.manager.grant_data_beat_count() == 1, "first D did not complete");
        f.expect_line_idle(false, "partial Grant reported idle");
        f.step(); // Interbeat delay.
        require(!f.dut.dcache.d.valid.boolean(), "interbeat delay was not exercised");
        f.dut.dcache.d.ready.set(0); f.step();
        f.expect_line_idle(false, "stalled final D reported idle");
        require(f.dut.dcache.d.valid.boolean() && f.manager.grant_data_beat_count() == 1,
                "final D stall was not exercised");
        f.dut.dcache.d.ready.set(1); f.step();
        require(f.manager.responses_idle() && !f.manager.grant_acks_idle(),
                "final-D-only state was not exercised");
        f.expect_line_idle(false, "final D before E retired Grant");
        f.ack();
        f.expect_line_idle(true, "final D then E did not retire Grant");
    } else if (test == "line-grants-e-before-final-d") {
        f.acquire(1); f.ack(); // Legal E with first accepted D.
        require(f.manager.grant_acks_idle() && !f.manager.responses_idle(),
                "E-first state was not exercised");
        f.expect_line_idle(false, "E before final D retired Grant");
        f.dut.dcache.d.ready.set(0); f.step();
        f.expect_line_idle(false, "early-E stalled final D reported idle");
        f.dut.dcache.d.ready.set(1); f.step();
        f.expect_line_idle(true, "E then final D did not retire Grant");
    } else if (test == "line-grants-same-pa-pending") {
        // Keep a distinct line's held Grant live throughout target completion.
        f.manager.hold_response_at(line + 64);
        f.acquire(1, 2, 1, line + 64);
        f.expect_line_idle(true, "unrelated held Grant blocked target");
        f.acquire(1);
        f.acquire(1, 1); // First target D; next same-PA A queues.
        require(f.manager.refill_count() == 2, "same-PA successor did not queue");
        f.expect_line_idle(false, "queued same-PA Acquire reported idle");
        f.ack(1); // Final old D/E immediately enables the successor Grant.
        require(f.manager.refill_count() == 3, "same-PA successor did not progress");
        f.expect_line_idle(false, "same-PA successor lost Grant lifetime");
        f.step(); f.ack(2);
        f.expect_line_idle(true, "same-PA Grants did not retire");
        f.expect_line_idle(false, "unrelated held Grant was lost", line + 64);
        require(!f.manager.responses_idle() && !f.manager.grant_acks_idle(),
                "independent line did not remain active");
    } else if (test == "line-grants-recall-pending") {
        f.owner(); f.next_alias();
        // No target Grant exists yet: only the accepted Acquire awaiting C.
        require(f.manager.grant_acks_idle() && !f.manager.response_held(),
                "pending-only Acquire state was not exercised");
        f.expect_line_idle(false, "Acquire awaiting alias recall reported idle");
        f.expect_line_idle(true, "target recall blocked unrelated line", line + 64);
        f.c(5, 1, 0);
        f.expect_line_idle(false, "Acquire awaiting partial C reported idle");
        f.c(5, 1, 1);
        f.expect_line_idle(false, "post-recall queued Grant reported idle");
        f.step(); f.ack();
        f.expect_line_idle(true, "recalled Acquire/Grant did not retire");
    } else if (test == "line-grants-acquire-perm") {
        f.owner(0); // First obtain B, then legally grow BtoT.
        f.acquire(1, 0, 2, line, 7);
        f.expect_line_idle(false, "AcquirePerm Grant reported idle");
        f.step();
        require(f.manager.responses_idle() && !f.manager.grant_acks_idle(),
                "data-less final-D-only Grant state was not exercised");
        f.expect_line_idle(false, "data-less Grant retired before E");
        f.ack();
        f.expect_line_idle(true, "data-less Grant did not retire");
    } else if (test == "line-grants-scope") {
        // An ordinary same-line Get, ReleaseData, ReleaseAck or standalone
        // Probe is outside the cacheable Acquire/Grant lifecycle query.
        auto &a = f.dut.dcache.a;
        a.valid.set(1); a.opcode.set(4); a.param.set(0); a.size.set(3);
        a.source.set(0); a.address.set(line); a.mask.set(0xff);
        f.dut.dcache.d.ready.set(0);
        f.step(); a.valid.set(0); f.step();
        require(!f.manager.responses_idle(), "Get response was not outstanding");
        f.expect_line_idle(true, "same-line Get blocked Grant query");
        f.dut.dcache.d.ready.set(1); f.step();
        f.c(7, 1, 0);
        require(!f.manager.responses_idle(), "partial ReleaseData was not active");
        f.expect_line_idle(true, "same-line ReleaseData blocked Grant query");
        f.c(7, 1, 1);
        f.expect_line_idle(true, "same-line ReleaseAck blocked Grant query");
        f.step();
        require(f.manager.request_probe(line, 2, false, 5), "standalone Probe failed");
        f.step();
        require(!f.manager.probes_idle(), "standalone Probe was not active");
        f.expect_line_idle(true, "same-line standalone Probe blocked Grant query");
        f.c(4, 5);
    } else if (test == "line-grants-reset") {
        f.manager.hold_response_at(line + 128);
        f.manager.hold_response_at(line + 192);
        f.dut.dcache.d.ready.set(0);
        f.acquire(1); f.acquire(1, 1);
        f.acquire(1, 2, 1, line + 64);
        f.acquire(1, 3, 1, line + 128);
        f.manager.schedule_held_response_release_at(line + 128, 10);
        f.expect_line_idle(false, "reset setup lacked target Grant/Acquire");
        f.expect_line_idle(false, "reset setup lacked unrelated Grant", line + 64);
        f.expect_line_idle(false, "reset setup lacked held response", line + 128);
        f.expect_line_idle(false, "reset setup lacked armed hold", line + 192);
        f.manager.reset_link_state();
        for (unsigned offset : {0U, 64U, 128U, 192U})
            f.expect_line_idle(true, "reset retained line state", line + offset);
        require(f.manager.responses_idle() && f.manager.grant_acks_idle(),
                "reset retained global Grant/response state");
        f.dut.dcache.d.ready.set(1);
        f.acquire(1); f.ack(); f.step();
        f.expect_line_idle(true, "post-reset Grant did not retire");
    } else {
        throw std::runtime_error("unknown line Grant lifecycle case: " + test);
    }
    f.ok();
}


static void input_owner(Fixture &f, bool initialize=true, unsigned alias=0) {
    if (initialize) for(unsigned b=0;b<64;++b)f.bus.write_byte(line+b,f.old_image[b]);
    f.acquire(alias);f.step();f.ack();f.ok();
}
static void input_group(Fixture &f,std::uint64_t order,unsigned offset,unsigned count,
    unsigned char value,bool authorize=true,unsigned alias=0) {
    require(f.manager.register_release_store(order,line+offset,line+(alias<<12)+offset,
        std::vector<unsigned char>(count,value)),f.manager.error());
    if(authorize)require(f.manager.authorize_release_store(order),f.manager.error());
}
static void input_release(Fixture &f,const std::vector<unsigned char> &image,bool accepted=true,unsigned report=1) {
    const auto before=f.bus.read_beat(line,64);
    f.c(7,report,0,9,&image);
    require(f.bus.read_beat(line,64)==before,"partial C changed backing before whole-line validation");
    f.c(7,report,1,9,&image);
    require(f.manager.ok()==accepted,"ordinary input release decision incorrect: "+f.manager.error());
    if(accepted){require(f.bus.read_beat(line,64)==image,"validated release did not update backing");f.step();f.ok();}
    else require(f.bus.read_beat(line,64)==before,"rejected whole-line C changed backing");
}
static void input_versions(const std::string &test) {
    Fixture f;
    if(test=="large-history") {
        input_owner(f);require(f.manager.begin_release_input_epoch(),f.manager.error());
        // Budget stress only: 4096 instructions' maximum128 element records,
        // distributed over two physical lines. This is not CPU timing evidence.
        constexpr unsigned groups=4096U*128U;
        for(unsigned order=0;order<groups;++order) {
            const auto address=line+(order%128U);
            require(f.manager.register_release_store(order+1,address,address,{0xa7}),f.manager.error());
            require(f.manager.authorize_release_store(order+1),f.manager.error());
        }
        auto image=f.old_image;std::fill(image.begin(),image.end(),0xa7);
        input_release(f,image);
        for(unsigned byte=0;byte<128;++byte)f.reference.write_reference_byte(line+byte,0xa7);
        require(f.manager.end_release_input_epoch(),f.manager.error());
        std::cout<<"ordinary input budget groups="<<groups<<'\n';return;
    }
    if(test=="epoch-legacy-history") {
        input_owner(f);require(f.manager.checkpoint_alias_reference(line,64),f.manager.error());
        f.reference.write_reference_byte(line+8,0xa7);f.manager.note_reference_store(line+8,line+8,{0xa7});
        f.reference.write_reference_byte(line+8,0xb8);f.manager.note_reference_store(line+8,line+8,{0xb8});
        require(f.manager.begin_release_input_epoch(),f.manager.error());
        input_group(f,1024,40,8,0xc9);
        auto stale=f.old_image;stale[8]=0xa7;
        input_release(f,stale,false);return;
    }
    if(test=="epoch-boundary") {
        input_owner(f);
        auto prior=f.old_image;prior[3]=0x9a;
        f.reference.write_reference_byte(line+3,0x9a); // prior unnotified legacy store, certified drained
        require(f.manager.begin_release_input_epoch(),f.manager.error());
        input_group(f,1024,8,8,0xa7);
        auto applied=prior;std::fill(applied.begin()+8,applied.begin()+16,0xa7);
        for(unsigned b=8;b<16;++b)f.reference.write_reference_byte(line+b,0xa7);
        input_release(f,prior); // legal old cache image; backing was still older
        input_owner(f,false);input_release(f,applied);
        require(f.manager.end_release_input_epoch(),f.manager.error());
        input_owner(f,false); // bare owner must retain legacy exact-reference behavior
        f.reference.write_reference_byte(line+20,0xbc);applied[20]=0xbc;
        input_release(f,applied);
        require(f.manager.begin_release_input_epoch(),f.manager.error());
        require(f.manager.end_release_input_epoch(),f.manager.error());
        return;
    }
    if(test=="epoch-noneligible") {
        input_owner(f);require(f.manager.begin_release_input_epoch(),f.manager.error());
        input_group(f,1024,8,8,0xa7,false);
        require(!f.manager.end_release_input_epoch(),"epoch silently discarded unaccepted future eligibility");return;
    }
    if(test=="before-acquire") {
        input_group(f,1024,8,8,0xa7);
        // Cold registration uses the independent backing supplied by the grant.
        // Initialize backing before acquire, as a real input image does.
        input_owner(f); // acquire refreshes the frozen input line's backing baseline
    } else input_owner(f);
    auto updated=f.old_image;std::fill(updated.begin()+8,updated.begin()+16,0xa7);
    if(test=="before-acquire" || test=="old" || test=="applied" || test=="torn" || test=="noneligible") {
        if(test!="before-acquire")input_group(f,1024,8,8,0xa7,test!="noneligible");
        for(unsigned b=8;b<16;++b)f.reference.write_reference_byte(line+b,0xa7);
        if(test=="torn") {auto torn=updated;std::fill(torn.begin()+12,torn.begin()+16,0x31+12);
            input_release(f,torn,false);return;}
        if(test=="noneligible"){input_release(f,updated,false);return;}
        input_release(f,test=="applied"?updated:f.old_image);
        if(test!="applied"){input_owner(f,false);input_release(f,updated);}
    } else if(test=="registration-order" || test=="overlap-torn") {
        input_group(f,20,8,2,0xb8);input_group(f,10,8,8,0xa7);
        updated[8]=updated[9]=0xb8;
        if(test=="overlap-torn")std::copy(f.old_image.begin()+10,f.old_image.begin()+16,updated.begin()+10);
        input_release(f,updated,test!="overlap-torn");
    } else if(test=="midbeat" || test=="late-value") {
        const auto before=f.bus.read_beat(line,64);
        input_group(f,10,8,8,0xa7);
        f.c(7,1,0,9,&f.old_image);
        input_group(f,20,40,8,0xc9);
        auto late=f.old_image;if(test=="late-value")std::fill(late.begin()+40,late.begin()+48,0xc9);
        f.c(7,1,1,9,&late);
        require(f.manager.ok()==(test=="midbeat"),"first-C eligibility snapshot changed between beats");
        if(test=="late-value"){require(f.bus.read_beat(line,64)==before,"late-version rejection changed backing");return;}
        f.step();input_owner(f,false);
        std::fill(updated.begin()+40,updated.begin()+48,0xc9);input_release(f,updated);
    } else if(test=="retained-owner-rollback") {
        input_group(f,10,8,8,0xa7);
        input_release(f,updated,true,0);
        f.acquire(0,0,2,line,7);f.step();f.ack();f.ok();
        input_release(f,f.old_image,false);
    } else if(test=="residual" || test=="rollback") {
        input_group(f,10,8,8,0xa7);input_group(f,20,40,8,0xc9);
        auto disjoint=f.old_image;std::fill(disjoint.begin()+40,disjoint.begin()+48,0xc9);
        input_release(f,disjoint);input_owner(f,false);
        if(test=="rollback")input_release(f,f.old_image,false);
        else {std::fill(updated.begin()+40,updated.begin()+48,0xc9);input_release(f,updated);}
    } else if(test=="aba") {
        input_group(f,10,8,1,0xa7);input_group(f,20,8,1,f.old_image[8]);
        input_release(f,f.old_image);input_owner(f,false);
        auto intermediate=f.old_image;intermediate[8]=0xa7;
        input_release(f,intermediate);input_owner(f,false);
        input_release(f,f.old_image);input_owner(f,false);
        input_release(f,intermediate,false);
    } else if(test=="explicit" || test=="explicit-midbeat") {
        input_group(f,10,8,8,0xa7);f.manager.expect_release_line(line,f.old_image);
        if(test=="explicit"){input_release(f,updated,false);return;}
        f.c(7,1,0,9,&f.old_image);f.manager.expect_release_line(line,updated);
        f.c(7,1,1,9,&f.old_image);f.ok();f.step();input_owner(f,false);
        input_release(f,updated);
    } else if(test=="checkpoint-protection") {
        input_group(f,10,8,8,0xa7);input_group(f,20,40,8,0xc9,false);
        input_group(f,30,56,8,0xd0,true,1);
        for(unsigned b=8;b<16;++b)f.reference.write_reference_byte(line+b,0xa7);
        require(f.manager.checkpoint_alias_reference(line,64),f.manager.error());
        require(f.manager.authorize_release_store(20),f.manager.error());
        std::fill(updated.begin()+40,updated.begin()+48,0xc9);input_release(f,updated);
        input_owner(f,false,1);std::fill(updated.begin()+56,updated.end(),0xd0);input_release(f,updated);
    } else if(test=="retired-order" || test=="late-older-order") {
        input_group(f,20,8,8,0xa7);input_release(f,updated);
        require(!f.manager.register_release_store(test=="retired-order"?20:10,line+8,line+8,{0xb8}),
            "registration reintroduced already observed same-byte history");
    } else if(test=="immutable-order") {
        input_group(f,20,8,8,0xa7);
        require(f.manager.register_release_store(20,line+8,line+8,std::vector<unsigned char>(8,0xa7)),"identical registration was not idempotent");
        require(!f.manager.register_release_store(20,line+8,line+8,{0xb8}),"input registration changed immutable bytes");
    } else if(test=="reset") {
        input_group(f,20,8,8,0xa7);f.manager.reset_link_state();
        input_group(f,20,8,8,0xa7);input_owner(f);input_release(f,updated);
    } else throw std::runtime_error("unknown input-version case");
}
static void exhaustive_input_versions() {
    struct Update {unsigned mask;unsigned char value;};
    std::uint64_t histories=0,transitions=0;
    for(unsigned code=0;code<729;++code) {
        unsigned value=code;std::array<Update,3> updates;
        for(auto &u:updates){const auto digit=value%9;value/=9;u={1+digit/3,static_cast<unsigned char>(digit%3==0?0x31:digit%3==1?0xa1:0xb2)};}
        Fixture baseline;
        std::vector<std::pair<unsigned,std::vector<unsigned char>>> states;
        for(unsigned selected=0;selected<8;++selected) {
            auto image=baseline.old_image;bool valid=true;
            for(unsigned i=0;i<3;++i)if((selected>>i)&1) {
                for(unsigned j=0;j<i;++j)if((updates[i].mask&updates[j].mask) && !((selected>>j)&1))valid=false;
                for(unsigned b=0;b<2;++b)if((updates[i].mask>>b)&1)image[b]=updates[i].value;
            }
            if(valid)states.push_back({selected,image});
        }
        std::set<std::vector<unsigned char>> images;
        for(const auto &[mask,image]:states)images.insert(image);
        for(const auto &first:images) {
            unsigned least=7;
            for(const auto &[mask,image]:states)if(image==first)least&=mask;
            for(const auto &second:images) {
                const bool expected=std::any_of(states.begin(),states.end(),[&](const auto &state){
                    return state.second==second && (state.first&least)==least;
                });
                Fixture f;input_owner(f);
                for(unsigned i=0;i<3;++i) {
                    if(updates[i].mask==3)input_group(f,i+1,0,2,updates[i].value);
                    else input_group(f,i+1,updates[i].mask==1?0:1,1,updates[i].value);
                }
                input_release(f,first,true,0);
                f.acquire(0,0,2,line,7);f.step();f.ack();f.ok();
                input_release(f,second,expected);
                ++transitions;
            }
        }
        ++histories;
    }
    std::cout<<"input-version histories="<<histories<<" transitions="<<transitions<<'\n';
}

int main(int argc, char **argv)
{
    try {
        const std::string test = argc>1 ? argv[1] : "dirty";
        if (test == "release-input-exhaustive") {exhaustive_input_versions();return 0;}
        if (test.starts_with("release-input-")) {input_versions(test.substr(14));return 0;}
        if (test == "release-version-exhaustive") {
            exhaustive_versions(); return 0;
        }
        if (test.starts_with("line-grants-")) {
            line_grants_lifecycle(test); return 0;
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
