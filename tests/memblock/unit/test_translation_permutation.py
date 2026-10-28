#!/usr/bin/env python3
"""Check declared page permutations and production byte-wise memory oracles."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]

CPP = r'''
#include "scenarios/mixed/translation_permutation.hpp"
#include <map>
#include <type_traits>
namespace memblock {
#include "environment/scoreboards.inc"

// Only the unused diagnostic sink is replaced. All address, byte-write and
// final-observation decisions below are extracted from production completion.cpp.
void trace_reference_store(const SparseMemory &, const char *, std::uint64_t,
    unsigned, std::uint64_t, unsigned, bool, unsigned, bool, const char *) {}
struct StoreFixture {
    SparseMemory bus_memory_;
    SparseMemory memory_{&bus_memory_};
    bool final_observation_tracking_ = true;
    std::set<std::uint64_t> final_observation_words_;
    std::set<std::uint64_t> final_observation_nc_words_;
    std::uint64_t final_observation_bytes_ = 0;
    std::uint64_t final_observation_nc_bytes_ = 0;
    std::uint64_t final_observation_alias_skips_ = 0;
    void discard_final_observation_range(std::uint64_t, unsigned);
    void track_final_observation_bytes(std::uint64_t, unsigned, bool, bool);
    void record_committed_store(const StoreTransaction &);
    void record_committed_vector_store(const VectorMemoryTransaction &);
};
// PRODUCTION_RECORD_METHODS
}

using namespace memblock;
using memblock::scenarios::mixed::OrdinaryPageMappings;
constexpr std::uint64_t va = OrdinaryPageMappings::virtual_base;
constexpr std::uint64_t pa = 0x80200000ULL;
constexpr std::uint64_t memory_seed = 0xd4b6753e9012ac8fULL;

void require(bool condition, const char *message)
{
    if (!condition) throw std::runtime_error(message);
}
template<class Exception, class Callback>
void require_throws(Callback callback, const char *message)
{
    try { callback(); }
    catch (const Exception &) { return; }
    throw std::runtime_error(message);
}

std::shared_ptr<const ReferencePageMap> literal_map()
{
    // Deliberately noncontiguous in both directions. The expected byte lists
    // in the cases below are declared independently of production helpers.
    return std::make_shared<const ReferencePageMap>(va,
        std::vector<std::uint64_t>{pa+0x5000, pa+0x1000, pa+0x9000, pa+0x3000}, 91);
}

std::string topology(const OrdinaryPageMappings &plan)
{
    std::ostringstream result;
    for (const auto *stages : {&plan.host, &plan.vs, &plan.g})
        for (const auto &mode : *stages) {
            for (auto page : mode) result << page << ',';
            result << ';';
        }
    return result.str();
}

void permutations()
{
    std::set<std::string> topologies, context_topologies;
    for (std::uint64_t seed=0; seed<128; ++seed) {
        OrdinaryPageMappings first(seed, pa);
        // Neither traffic, payload, scheduling nor reference-memory read order
        // may enter this structural fingerprint.
        memblock::scenarios::StressRandom noise(seed);
        SparseMemory memory;
        memory.define_seeded_random(pa, OrdinaryPageMappings::span, seed);
        for (unsigned n=0; n<seed; ++n) {
            (void)noise(); (void)noise.next_shape();
            (void)noise.next_payload(); (void)noise.next_schedule();
            (void)memory.read_byte(pa + (n*4103)%OrdinaryPageMappings::span);
        }
        OrdinaryPageMappings replay(seed, pa), relocated(seed, pa+0x200000);
        require(topology(first)==topology(replay), "mapping replay changed");
        require(topology(first)==topology(relocated), "physical base changed topology");
        require(topologies.insert(topology(first)).second, "mapping seeds repeat topology");
        for (const auto *stage : {&first.host, &first.vs, &first.g}) {
            require((*stage)[0]!=(*stage)[1], "stage modes share one permutation");
            for (const auto &mode : *stage) {
                require(mode.size()==132, "ordinary page count changed");
                std::set<unsigned> pages(mode.begin(), mode.end());
                require(pages.size()==132 && *pages.begin()==0 && *pages.rbegin()==131,
                    "stage map is not a complete permutation");
                unsigned moved=0, descents=0, discontinuities=0;
                for (unsigned i=0; i<132; ++i) {
                    moved += mode[i]!=i;
                    if (i) {
                        descents += mode[i]<mode[i-1];
                        discontinuities += mode[i]!=mode[i-1]+1;
                    }
                }
                require(moved>100 && descents && discontinuities>100,
                    "stage permutation lacks structural diversity");
            }
        }
        for (unsigned context=0; context<6; ++context) {
            auto map = context<2 ? first.host_maps[context]
                : first.nested_maps[(context-2)/2][(context-2)%2];
            auto relocated_map = context<2 ? relocated.host_maps[context]
                : relocated.nested_maps[(context-2)/2][(context-2)%2];
            require(map->context()==context, "translation context identity changed");
            std::ostringstream fingerprint;
            for (unsigned page=0; page<132; ++page) {
                // These stage arrays are the scenario-declared relations;
                // no PTEs, walk result or DUT translation are consulted.
                const unsigned physical_page = context<2 ? first.host[context][page]
                    : first.g[(context-2)%2][first.vs[(context-2)/2][page]];
                fingerprint << physical_page << ',';
                for (auto offset : {0U, 1U, 2047U, 4095U}) {
                    const auto address=va+page*4096ULL+offset;
                    const auto expected=pa+physical_page*4096ULL+offset;
                    require(map->physical_address(address)==expected,
                        "host or composed nested page differs from declared mapping");
                    require(((address ^ expected) & 0x3000U)==0,
                        "ordinary permutation exceeds manager cache-colour contract");
                    require(relocated_map->physical_address(address)==expected+0x200000,
                        "relocated map changed a page relation");
                }
            }
            require(context_topologies.insert(fingerprint.str()).second,
                "translation contexts repeat a sampled topology");
        }
    }
    std::cout << "unique_page_topologies=" << topologies.size()
              << " unique_context_topologies=" << context_topologies.size() << '\n';
}

template<class Transaction>
void ambiguous_mapping()
{
    Transaction tx;
    tx.address=va+7;
    tx.reference_pages=literal_map();
    tx.oracle_address=pa+7;
    require_throws<std::invalid_argument>([&] { (void)reference_byte_address(tx, tx.address); },
        "ambiguous legacy and page-map address forms were accepted");
}

void boundaries()
{
    static_assert(std::is_same_v<decltype(LoadTransaction{}.reference_pages),
        std::shared_ptr<const ReferencePageMap>>);
    const auto map=literal_map();
    require(map->contains(va) && map->contains(va+0x3fff), "map lost boundary bytes");
    require(!map->contains(va-1) && !map->contains(va+0x4000), "map bounds leaked");
    require(map->physical_address(va)==pa+0x5000 &&
            map->physical_address(va+0x3fff)==pa+0x3fff, "boundary offset changed");
    for (auto outside : {va-1, va+0x4000, ~std::uint64_t{0}})
        require_throws<std::out_of_range>([&] { (void)map->physical_address(outside); },
            "out-of-map byte was accepted");
    require_throws<std::invalid_argument>([] { ReferencePageMap(va+1, {pa}); },
        "unaligned virtual map base was accepted");
    require_throws<std::invalid_argument>([] { ReferencePageMap(va, {}); },
        "empty page map was accepted");
    require_throws<std::invalid_argument>([] { ReferencePageMap(va, {pa+1}); },
        "unaligned physical page was accepted");
    require_throws<std::invalid_argument>([] {
        ReferencePageMap(~std::uint64_t{0}-0xfff, {pa, pa+0x1000});
    }, "overflowing virtual map window was accepted");
    ambiguous_mapping<LoadTransaction>();
    ambiguous_mapping<StoreTransaction>();
    ambiguous_mapping<PrefetchTransaction>();
    ambiguous_mapping<VectorMemoryTransaction>();
    LoadTransaction legacy;
    legacy.address=va+0xffc;
    require(reference_byte_address(legacy, legacy.address+7)==legacy.address+7,
        "identity byte-address semantics changed");
    legacy.oracle_address=pa+0x2ffc;
    require(reference_byte_address(legacy, legacy.address+7)==pa+0x3003,
        "legacy oracle address no longer describes contiguous physical bytes");
    legacy.reference_pages=map;
    legacy.oracle_address.reset();
    require_throws<std::out_of_range>([&] {
        (void)reference_byte_address(legacy, va+0x4000);
    }, "transaction reference mapping ignored its bounds");
}

bool scalar_accepts(const SparseMemory &memory, const LoadTransaction &tx,
    std::uint64_t returned)
{
    LoadScoreboard scoreboard;
    scoreboard.expect(tx, memory.expected_load(tx));
    generated::ScalarLoadWriteback response;
    response.valid=true;
    response.rf_wen=true;
    response.rob_value=tx.rob;
    response.pdest=tx.pdest;
    response.data=returned;
    scoreboard.observe(0, response);
    return scoreboard.error().empty() && scoreboard.done();
}

void scalar_bytes()
{
    SparseMemory memory;
    memory.define_seeded_random(pa, 0x10000, memory_seed);
    LoadTransaction tx;
    tx.address=va+0xffc;
    tx.reference_pages=literal_map();
    tx.rob=3; tx.pdest=17;
    // The first four bytes end one physical page; the last four begin a
    // different, lower physical page. This is oracle-only byte mechanics.
    const std::array<std::uint64_t,8> physical{{pa+0x5ffc, pa+0x5ffd,
        pa+0x5ffe, pa+0x5fff, pa+0x1000, pa+0x1001, pa+0x1002, pa+0x1003}};
    for (unsigned b=0; b<8; ++b) {
        memory.write_reference_byte(physical[b], 0x11*(b+1));
        require(reference_byte_address(tx, tx.address+b)==physical[b],
            "scalar byte mapped through a contiguous physical page");
    }
    tx.op=LoadOp::ld;
    constexpr std::uint64_t expected=0x8877665544332211ULL;
    require(memory.expected_load(tx)==expected, "scalar page-crossing bytes changed");
    require(scalar_accepts(memory, tx, expected), "correct scalar completion rejected");
    require(!scalar_accepts(memory, tx, memory.read_u64(pa+0x5ffc)),
        "contiguous-PA substitution escaped scalar scoreboard");
    require(!scalar_accepts(memory, tx, memory.read_u64(pa+0x2ffc)),
        "wrong-page substitution escaped scalar scoreboard");
    struct WidthCase { LoadOp op; unsigned bytes; std::uint64_t expected; };
    for (const auto &test : std::array<WidthCase,7>{{
        {LoadOp::lb,1,0xffffffffffffff80ULL}, {LoadOp::lbu,1,0x80},
        {LoadOp::lh,2,0xffffffffffff8034ULL}, {LoadOp::lhu,2,0x8034},
        {LoadOp::lw,4,0xffffffff80345678ULL}, {LoadOp::lwu,4,0x80345678},
        {LoadOp::ld,8,expected}}}) {
        tx.op=test.op;
        tx.address=va+0x1000-test.bytes/2;
        if (test.bytes==1) tx.address=va+0xfff;
        const std::uint64_t raw = test.bytes==1 ? 0x80 : test.expected;
        for (unsigned b=0; b<test.bytes; ++b) {
            // A separate two-part boundary declaration, rather than the
            // production mapper, supplies the setup locations.
            const auto target = b<test.bytes/2 || test.bytes==1
                ? pa+0x6000-(test.bytes==1 ? 1 : test.bytes/2)+b
                : pa+0x1000+b-test.bytes/2;
            memory.write_reference_byte(target, static_cast<unsigned char>(raw>>(8*b)));
        }
        require(memory.expected_load(tx)==test.expected,
            "mapped scalar width or sign/zero extension changed");
        require(scalar_accepts(memory, tx, test.expected),
            "mapped scalar width completion rejected");
    }
}

struct VectorCase {
    VectorMemoryTransaction tx;
    std::vector<std::pair<unsigned,std::uint64_t>> bytes;
    std::uint16_t active;
};
std::vector<VectorCase> vector_cases()
{
    VectorMemoryTransaction tx;
    tx.reference_pages=literal_map();
    tx.rob=5; tx.pdest=19;
    for (unsigned b=0; b<16; ++b) tx.data[b]=0xc0+b;
    std::vector<VectorCase> cases;
    tx.address=va+0xffe; tx.eew=2; tx.vl=4;
    cases.push_back({tx, {{0,pa+0x5ffe},{1,pa+0x5fff},{2,pa+0x1000},
        {3,pa+0x1001},{4,pa+0x1002},{5,pa+0x1003},{6,pa+0x1004},
        {7,pa+0x1005},{8,pa+0x1006},{9,pa+0x1007},{10,pa+0x1008},
        {11,pa+0x1009},{12,pa+0x100a},{13,pa+0x100b},{14,pa+0x100c},
        {15,pa+0x100d}}, 0xf});
    tx.address=va+0xfff; tx.eew=1; tx.vl=3; tx.vstart=1;
    tx.addressing=VectorAddressingMode::strided; tx.stride=4096;
    tx.vm=false; tx.mask_bits=0x6;
    cases.push_back({tx, {{2,pa+0x1fff},{3,pa+0x9000},
        {4,pa+0x9fff},{5,pa+0x3000}}, 0x6});
    tx.address=va+0x2001; tx.eew=2; tx.vstart=0; tx.vm=true; tx.stride=-2049;
    cases.push_back({tx, {{0,pa+0x9001},{1,pa+0x9002},{2,pa+0x9003},
        {3,pa+0x9004},{4,pa+0x1800},{5,pa+0x1801},{6,pa+0x1802},
        {7,pa+0x1803},{8,pa+0x5fff},{9,pa+0x1000},{10,pa+0x1001},
        {11,pa+0x1002}}, 0x7});
    tx.address=va+0xffe; tx.eew=2; tx.vsew=1; tx.vl=4;
    tx.vstart=1; tx.vm=false; tx.mask_bits=0xc; tx.flow_num=4;
    // Masked and pre-vstart addresses deliberately lie outside the map.
    // Only the active elements may evaluate those address expressions.
    const std::array<unsigned,4> indices{{0x10000,0x20000,1,0x1003}};
    for (unsigned e=0; e<4; ++e) for (unsigned b=0; b<4; ++b)
        tx.index[e*4+b]=static_cast<unsigned char>(indices[e]>>(8*b));
    for (auto mode : {VectorAddressingMode::indexed_ordered,
                      VectorAddressingMode::indexed_unordered}) {
        tx.addressing=mode;
        cases.push_back({tx, {{4,pa+0x5fff},{5,pa+0x1000},
            {6,pa+0x9001},{7,pa+0x9002}}, 0xc});
    }
    return cases;
}

bool vector_accepts(const SparseMemory &memory, const VectorCase &test,
    const std::array<unsigned char,16> &returned)
{
    const auto &tx=test.tx;
    VectorMemoryScoreboard scoreboard;
    scoreboard.expect(tx, memory.expected_vector_load(tx));
    generated::VectorMemoryWriteback response;
    response.valid=true; response.vec_wen=true;
    response.rob_value=tx.rob; response.pdest=tx.pdest;
    response.fu_op_type=vector_fu_op_type(tx);
    response.vsew=vector_vsew(tx); response.veew=tx.eew;
    response.vlmul=tx.vlmul; response.vl=tx.vl;
    response.vuop_idx=tx.vuop_idx; response.nf=tx.nf;
    response.vd_index_in_field=vector_indexed_vd_index(tx);
    response.data.assign(returned.begin(), returned.end());
    response.vmask.assign(16,0);
    response.vmask[0]=test.active & 0xff;
    response.vmask[1]=test.active >> 8;
    scoreboard.observe(0,response);
    return scoreboard.error().empty() && scoreboard.done();
}

void vector_bytes()
{
    SparseMemory memory;
    memory.define_seeded_random(pa,0x10000,memory_seed);
    unsigned checked=0;
    for (const auto &test : vector_cases()) {
        const unsigned width=1U<<vector_data_eew(test.tx);
        require(active_vector_elements(test.tx)==test.active,
            "vector mask/vstart/vl selected wrong elements");
        auto expected=test.tx.data, wrong_page=expected, contiguous=expected;
        std::map<unsigned,std::uint64_t> first_byte;
        for (const auto &[slot,address] : test.bytes)
            if (slot%width==0) first_byte[slot/width]=address;
        for (const auto &[slot,address] : test.bytes) {
            require(vector_byte_address(test.tx,slot/width,slot%width)==address,
                "vector byte differs from declared physical location");
            expected[slot]=memory.read_byte(address);
            wrong_page[slot]=memory.read_byte(address^0x4000);
            contiguous[slot]=memory.read_byte(first_byte.at(slot/width)+slot%width);
        }
        require(memory.expected_vector_load(test.tx)==expected,
            "vector oracle changed mapped, masked or preserved bytes");
        require(vector_accepts(memory,test,expected), "correct vector completion rejected");
        require(!vector_accepts(memory,test,wrong_page),
            "wrong-page substitution escaped vector scoreboard");
        require(!vector_accepts(memory,test,contiguous),
            "contiguous element-PA substitution escaped vector scoreboard");
        ++checked;
    }
    std::cout << "vector_crossing_shapes_checked=" << checked << '\n';
}

void check_image(const StoreFixture &fixture,
    const std::map<std::uint64_t,unsigned char> &changed)
{
    SparseMemory baseline;
    baseline.define_seeded_random(pa,0x10000,memory_seed);
    for (std::uint64_t offset=0; offset<0x10000; ++offset) {
        const auto address=pa+offset;
        const auto found=changed.find(address);
        const auto expected=found==changed.end() ? baseline.read_byte(address) : found->second;
        require(fixture.memory_.read_byte(address)==expected,
            "reference store changed wrong mapped bytes or neighboring bytes");
        require(fixture.bus_memory_.read_byte(address)==baseline.read_byte(address),
            "record-only reference store changed manager backing memory");
    }
}

void scalar_stores()
{
    StoreFixture fixture;
    fixture.memory_.define_seeded_random(pa,0x10000,memory_seed);
    StoreTransaction tx;
    tx.address=va+0xffc; tx.reference_pages=literal_map();
    tx.data=0x8877665544332211ULL; tx.op=StoreOp::sd;
    // Exercising a record helper is not a claim that a one-uop scalar
    // cross-page store is legal runtime stimulus.
    fixture.record_committed_store(tx);
    const std::map<std::uint64_t,unsigned char> changed{{pa+0x5ffc,0x11},
        {pa+0x5ffd,0x22},{pa+0x5ffe,0x33},{pa+0x5fff,0x44},
        {pa+0x1000,0x55},{pa+0x1001,0x66},{pa+0x1002,0x77},{pa+0x1003,0x88}};
    check_image(fixture,changed);
    require(fixture.final_observation_words_==std::set<std::uint64_t>{pa+0x5ff8,pa+0x1000},
        "mapped scalar final words used a contiguous physical range");
    require(fixture.final_observation_bytes_==8 && fixture.final_observation_alias_skips_==0,
        "mapped scalar final-byte accounting changed");
    tx.cacheable=false;
    fixture.record_committed_store(tx);
    require(fixture.final_observation_nc_words_==fixture.final_observation_words_ &&
            fixture.final_observation_nc_bytes_==8,
        "mapped noncacheable final words changed");
    StoreFixture legacy;
    legacy.memory_.define_seeded_random(pa,0x10000,memory_seed);
    tx.reference_pages.reset(); tx.oracle_address=pa+0x7000;
    tx.cacheable=true; tx.op=StoreOp::sw;
    legacy.record_committed_store(tx);
    require(legacy.memory_.read_u64(pa+0x7000)%0x100000000ULL==0x44332211 &&
            legacy.final_observation_words_.empty() &&
            legacy.final_observation_alias_skips_==1,
        "legacy mapped-store alias policy changed");
    StoreFixture zero;
    zero.memory_.define_seeded_random(pa,0x10000,memory_seed);
    tx.reference_pages=literal_map(); tx.oracle_address.reset();
    tx.op=StoreOp::cbo_zero; tx.address=va+0x1fc7;
    zero.record_committed_store(tx);
    std::map<std::uint64_t,unsigned char> cleared;
    std::set<std::uint64_t> words;
    for(unsigned b=0;b<64;++b) cleared[pa+0x1fc0+b]=0;
    for(unsigned b=0;b<64;b+=8) words.insert(pa+0x1fc0+b);
    check_image(zero,cleared);
    require(zero.final_observation_words_==words && zero.final_observation_bytes_==64,
        "mapped CBO.ZERO did not register the aligned physical line");
    for (auto op : {StoreOp::cbo_clean,StoreOp::cbo_flush}) {
        tx.op=op; zero.record_committed_store(tx);
        require(zero.final_observation_words_==words,
            "mapped clean/flush removed final observation words");
    }
    tx.op=StoreOp::cbo_inval; zero.record_committed_store(tx);
    require(zero.final_observation_words_.empty(),
        "mapped invalidate retained discarded final words");
    check_image(zero,cleared);
}

void vector_stores()
{
    unsigned checked=0;
    auto cases=vector_cases();
    // Record unit stride, positive stride and both indexed modes. All active
    // store-byte locations are disjoint, including unordered indexed stores.
    for(unsigned index : {0U,1U,3U,4U}) {
        auto test=cases.at(index);
        test.tx.store=true;
        StoreFixture fixture;
        fixture.memory_.define_seeded_random(pa,0x10000,memory_seed);
        fixture.record_committed_vector_store(test.tx);
        std::map<std::uint64_t,unsigned char> changed;
        std::set<std::uint64_t> words;
        for(const auto &[slot,address] : test.bytes) {
            require(changed.emplace(address,test.tx.data[slot]).second,
                "store fixture unexpectedly overlaps active element bytes");
            words.insert(address & ~std::uint64_t{7});
        }
        check_image(fixture,changed);
        require(fixture.final_observation_words_==words &&
                fixture.final_observation_bytes_==test.bytes.size() &&
                fixture.final_observation_alias_skips_==0,
            "mapped vector final-word or active-byte registration changed");
        fixture.final_observation_tracking_=false;
        fixture.record_committed_vector_store(test.tx);
        require(fixture.final_observation_bytes_==test.bytes.size(),
            "disabled final tracking still counted vector bytes");
        ++checked;
    }
    std::cout << "vector_record_shapes_checked=" << checked << '\n';
}

int main(int argc,char **argv)
{
    try {
        require(argc==2,"one test case required");
        const std::string name=argv[1];
        if(name=="permutations") permutations();
        else if(name=="boundaries") boundaries();
        else if(name=="scalar-bytes") scalar_bytes();
        else if(name=="vector-bytes") vector_bytes();
        else if(name=="scalar-stores") scalar_stores();
        else if(name=="vector-stores") vector_stores();
        else throw std::runtime_error("unknown test case");
        std::cout << name << " PASS\n";
    } catch(const std::exception &error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
'''


def production_record_methods() -> str:
    """Keep production decision logic intact; replace only its owning class."""
    source = (ROOT / "cpp/environment/completion.cpp").read_text()
    methods = []
    for name in ("discard_final_observation_range", "track_final_observation_bytes",
                 "record_committed_store", "record_committed_vector_store"):
        begin = source.index(f"auto Environment::Impl::{name}(")
        end = source.index("\n}\n", begin) + 3
        methods.append(source[begin:end].replace("Environment::Impl::", "StoreFixture::"))
    return "\n".join(methods)


class TranslationPermutationTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.workspace = tempfile.TemporaryDirectory(prefix="memblock-page-permutation-")
        cls.addClassCleanup(cls.workspace.cleanup)
        folder = Path(cls.workspace.name)
        source = folder / "fixture.cpp"
        cls.binary = folder / "fixture"
        source.write_text(CPP.replace("// PRODUCTION_RECORD_METHODS", production_record_methods()))
        built = subprocess.run(
            ["c++", "-std=c++20", "-O0", "-I", str(ROOT / "cpp"),
             str(source), "-o", str(cls.binary)],
            capture_output=True, text=True, timeout=60,
        )
        if built.returncode:
            raise AssertionError(built.stdout + built.stderr)

    def check_case(self, name: str) -> str:
        ran = subprocess.run([str(self.binary), name], capture_output=True,
                             text=True, timeout=20)
        self.assertEqual(ran.returncode, 0, ran.stdout + ran.stderr)
        self.assertIn(f"{name} PASS", ran.stdout)
        return ran.stdout

    def test_permutation_replay_context_composition_and_structural_diversity(self) -> None:
        output = self.check_case("permutations")
        self.assertIn("unique_page_topologies=128 unique_context_topologies=768", output)

    def test_bounds_immutable_map_ambiguity_and_legacy_address_forms(self) -> None:
        self.check_case("boundaries")

    def test_scalar_cross_page_bytes_sign_and_scoreboard_substitution_sensitivity(self) -> None:
        self.check_case("scalar-bytes")

    def test_vector_stride_index_mask_crossing_and_scoreboard_substitution_sensitivity(self) -> None:
        output = self.check_case("vector-bytes")
        self.assertIn("vector_crossing_shapes_checked=5", output)

    def test_production_scalar_record_bytes_and_physical_final_words(self) -> None:
        self.check_case("scalar-stores")

    def test_production_vector_record_active_bytes_and_physical_final_words(self) -> None:
        output = self.check_case("vector-stores")
        self.assertIn("vector_record_shapes_checked=4", output)


if __name__ == "__main__":
    unittest.main()
