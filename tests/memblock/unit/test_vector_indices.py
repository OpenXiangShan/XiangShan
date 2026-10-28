#!/usr/bin/env python3
"""Check real index operands, CPU register sharing and byte-address oracles."""
from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))
import verify_regression

CPP = r'''
#include "scenarios/mixed/state.hpp"
#include "scenarios/mixed/vector_indices.hpp"
#include "scenarios/mixed/vector_operands.hpp"
#include <set>
using namespace memblock::scenarios;
namespace memblock::scenarios::mixed {
class MixedScenario {
public:
    RandomConstraints constraints = RandomConstraints::preset("coverage");
    ConstraintCoverage constraint_coverage;
    MixedCoverage coverage;
    struct { memblock::SparseMemory image; memblock::SparseMemory &memory() { return image; } } environment;
    std::mt19937_64 random{17};
    std::uint64_t rob_offset=0, sq_offset=0, lq_offset=0;
    std::shared_ptr<const memblock::ReferencePageMap> reference_pages_for(std::uint64_t) const { return {}; }
    static unsigned vector_shape_uop_count(const RandomVectorShape &);
    static unsigned vector_shape_flow_num(const RandomVectorShape &);
    bool choose_binary_class(unsigned, const std::array<std::uint64_t,2> &);
    std::vector<memblock::VectorMemoryTransaction> make_random_vector_uops(
        bool, std::uint64_t, const RandomVectorShape &, bool);
};
// PRODUCTION_METHODS
}
using namespace memblock::scenarios::mixed;
static void require(bool value) { if (!value) throw std::runtime_error("check failed"); }
static std::string topology(const RandomVectorIndices &p) {
    std::ostringstream out;
    for (auto v:p.offsets) out << v << ',';
    return out.str();
}
static std::uint64_t decode(const unsigned char *p, unsigned n) {
    std::uint64_t value=0;
    for(unsigned b=0;b<n;++b) value |= std::uint64_t(p[b]) << (b*8);
    return value;
}
static void plans() {
    std::mt19937_64 r(31), replay(31);
    std::set<std::string> topologies;
    unsigned repeats=0, descents=0;
    for(unsigned sample=0;sample<512;++sample) {
        auto p=make_random_vector_indices(r,16,2,1,1,false,250);
        require(p.bytes==make_random_vector_indices(replay,16,2,1,1,false,250).bytes);
        topologies.insert(topology(p));
        std::set<std::uint64_t> seen;
        for(unsigned e=0;e<16;++e) {
            repeats += !seen.insert(p.offsets[e]).second;
            descents += e && p.offsets[e]<p.offsets[e-1];
        }
    }
    require(topologies.size()==512 && repeats && descents);
    for(unsigned n:{1U,2U,4U,8U}) for(unsigned width:{1U,2U,4U,8U})
      for(unsigned fields=1;fields<=8;++fields) for(bool store:{false,true})
        for(unsigned repeat:{0U,250U,1000U}) {
            unsigned elements=std::min(128U/n,128U/(fields*width));
            auto p=make_random_vector_indices(r,elements,n,width,fields,store,repeat);
            std::set<std::uint64_t> touched;
            for(unsigned e=0;e<elements;++e) {
                require(decode(p.bytes.data()+e*n,n)==p.offsets[e]);
                require(p.offsets[e]%(fields*width)==0);
                require(p.offsets[e]+fields*width<=2048);
                if(n==1) require(p.offsets[e]<256);
                if(store || repeat==0) for(unsigned b=0;b<fields*width;++b)
                    require(touched.insert(p.offsets[e]+b).second);
                if(!store && repeat==1000) require(p.offsets[e]==p.offsets[0]);
            }
        }
    for(unsigned rate:{0U,1000U,1001U}) {
        auto c=RandomConstraints::preset("spec");
        c.apply("vector-index-repeat="+std::to_string(rate));
        bool rejected=false;
        try {c.validate();} catch(const std::invalid_argument &) {rejected=true;}
        require(rejected==(rate==1001));
        require(c.summary().find("target_vector_index_repeat="+std::to_string(rate))!=std::string::npos);
    }
    std::cout << "unique_index_topologies=" << topologies.size() << '\n';
}
static void packing() {
    MixedScenario generator;
    auto &c=generator.constraints;
    c.vector_masked_per_mille=c.vector_partial_vl_per_mille=0;
    c.vector_nonzero_vstart_per_mille=c.vector_vma_per_mille=c.vector_vta_per_mille=0;
    unsigned checked=0;
    for(unsigned eew=0;eew<4;++eew) for(unsigned sew=0;sew<4;++sew)
      for(int lmul=-3;lmul<=3;++lmul) {
        const int emul=lmul+int(eew)-int(sew);
        if(lmul<int(sew)-3 || emul < -3 || emul>3) continue;
        RandomVectorShape shape{2,eew,sew,lmul,emul,1};
        const auto uops=generator.make_random_vector_uops(true,0x80001000,shape,false);
        const unsigned data_width=1U<<sew, index_width=1U<<eew;
        const unsigned ratio=1U<<unsigned(std::abs(emul-lmul));
        const unsigned vl=(lmul<0 ? 16U>>unsigned(-lmul) : 16U<<unsigned(lmul))>>sew;
        std::set<std::uint64_t> touched;
        for(unsigned u=0;u<uops.size();++u) {
            // DecodeUnitComp: LMUL>EMUL repeats one VS2 register across ratio uops.
            if(lmul>emul && u%ratio) require(uops[u].index==uops[u-u%ratio].index);
            auto active=memblock::active_vector_elements(uops[u]);
            for(unsigned e=0;e<16U/data_width;++e) if((active>>e)&1U) {
                const unsigned global=u*uops[u].flow_num + e - memblock::vector_indexed_split_offset(uops[u]);
                const unsigned vs2_chunk=global/(16U/index_width);
                const unsigned producer=emul>lmul ? vs2_chunk : vs2_chunk*ratio;
                const unsigned local=global%(16U/index_width);
                const auto offset=decode(uops.at(producer).index.data()+local*index_width,index_width);
                const auto address=memblock::vector_element_address(uops[u],e);
                require(address==0x80001000+offset);
                require(touched.insert(address).second);
            }
        }
        require(touched.size()==vl);
        ++checked;
    }
    std::cout << "indexed_shapes_checked=" << checked << '\n';
}
int main() { plans(); packing(); }
'''


def production_methods() -> str:
    source = (ROOT / "cpp/scenarios/mixed_vectors.cpp").read_text()
    names = ("unsigned int MixedScenario::vector_shape_uop_count(",
             "unsigned int MixedScenario::vector_shape_flow_num(",
             "bool MixedScenario::choose_binary_class(",
             "std::vector<memblock::VectorMemoryTransaction> MixedScenario::make_random_vector_uops(")
    methods = []
    for name in names:
        begin = source.index(name)
        end = source.index("\n}\n", begin) + 3
        methods.append(source[begin:end])
    return "\n".join(methods)


class VectorIndicesTest(unittest.TestCase):
    def test_operand_diversity_and_real_uop_packing(self):
        with tempfile.TemporaryDirectory() as folder:
            source, binary = Path(folder) / "fixture.cpp", Path(folder) / "fixture"
            source.write_text(CPP.replace("// PRODUCTION_METHODS", production_methods()))
            built = subprocess.run(["c++", "-std=c++20", "-O0", "-I", str(ROOT / "cpp"),
                                    str(source), "-o", str(binary)],
                                   capture_output=True, text=True, timeout=60)
            self.assertEqual(built.returncode, 0, built.stdout + built.stderr)
            ran = subprocess.run([str(binary)], capture_output=True, text=True, timeout=10)
            self.assertEqual(ran.returncode, 0, ran.stdout + ran.stderr)
            self.assertIn("unique_index_topologies=512", ran.stdout)
            self.assertIn("indexed_shapes_checked=78", ran.stdout)

    def test_operand_accounting_rejects_invalid_counts(self):
        check = verify_regression._check_vector_index_accounting
        base = {"constraint_schema": 55, "target_vector_index_repeat": 250,
                "vector_indices": "1,1,1,1,16,4,6,2032"}
        check(base)
        check({**base, "vector_indices": "0,0,0,0,0,0,0,0"})
        for field, value in (("target_vector_index_repeat", True),
                             ("target_vector_index_repeat", 1001),
                             ("target_vector_index_repeat", 0),
                             ("vector_indices", "0,1,0,1,16,4,6,2032"),
                             ("vector_indices", "1,1,1,1,3,0,0,0"),
                             ("vector_indices", "1,1,1,1,513,0,0,0"),
                             ("vector_indices", "1,1,1,1,16,13,0,0"),
                             ("vector_indices", "1,1,1,1,16,0,13,0"),
                             ("vector_indices", "1,1,1,1,16,0,0,2048"),
                             ("vector_indices", "0,0,0,0,0,0,0,1")):
            with self.subTest(field=field, value=value), self.assertRaises(verify_regression.VerificationError):
                check({**base, field: value})


if __name__ == "__main__":
    unittest.main()
