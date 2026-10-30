#!/usr/bin/env python3
"""Independent physical-footprint and sparse lifecycle accounting contracts."""
from pathlib import Path
import importlib.util
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'scripts'))
spec = importlib.util.spec_from_file_location('verify_disjoint_bypass', ROOT / 'scripts/verify_regression.py')
verifier = importlib.util.module_from_spec(spec)
spec.loader.exec_module(verifier)

CPP = r'''
#include "scenarios/mixed/stream_footprint.hpp"
#include "scenarios/mixed/state.hpp"
#include <cassert>
using namespace memblock;
using namespace memblock::scenarios::mixed;
int main() {
    const std::uint64_t va = 0x2000000000ULL, pa = 0x80200000;
    auto pages = std::make_shared<const ReferencePageMap>(va, std::vector<std::uint64_t>{pa+0x1000,pa});
    for (unsigned op=0;op<7;++op) for (unsigned offset=0;offset<4096;++offset) {
        LoadTransaction t; t.address=va+offset; t.op=static_cast<LoadOp>(op); t.reference_pages=pages;
        StreamFootprint expected, exact;
        const unsigned width=1U<<(op%4U);
        for (unsigned b=0;b<width;++b) {
            const auto n=offset+b;
            expected.insert(((n<4096 ? pa+4096+n : pa+n-4096))/64U);
            exact.insert(n<4096 ? pa+4096+n : pa+n-4096);
        }
        assert(stream_footprint(t)==expected);
        assert(stream_byte_footprint(t)==exact);
    }
    LoadTransaction load; load.address=va+8; load.op=LoadOp::ld; load.reference_pages=pages;
    StoreTransaction store; store.address=pa+0x1000+56; store.op=StoreOp::sb;
    assert(!stream_disjoint(stream_footprint(load),stream_footprint(store))); // same physical line, disjoint bytes
    assert(stream_disjoint(stream_byte_footprint(load),stream_byte_footprint(store)));
    store.address=pa+0x1000+12;
    assert(!stream_disjoint(stream_byte_footprint(load),stream_byte_footprint(store)));
    for (unsigned a=0;a<64;++a) for(unsigned b=0;b<64;++b) {
        LoadTransaction l;l.address=pa+a;l.op=LoadOp::lbu;
        StoreTransaction s;s.address=pa+b;s.op=StoreOp::sb;
        const auto x=stream_byte_footprint(l),y=stream_byte_footprint(s);
        assert(stream_disjoint(x,y)==(a!=b));
        assert(stream_shares_granule(x,y,8)==(a/8==b/8));
        assert(stream_shares_granule(x,y,16)==(a/16==b/16));
    }
    store.address=pa+8;
    assert(stream_disjoint(stream_footprint(load),stream_footprint(store)));
    load.reference_pages.reset();load.oracle_address=pa+8;
    assert(!stream_disjoint(stream_footprint(load),stream_footprint(store))); // oracle alias
    VectorMemoryTransaction v;v.store=true;v.address=va+0xff8;v.eew=3;v.vl=2;v.reference_pages=pages;
    assert((stream_footprint(std::vector{v})==StreamFootprint{(pa+0x1ff8)/64U,pa/64U}));
    v.vm=false;v.mask_bits=1;
    assert((stream_footprint(std::vector{v})==StreamFootprint{(pa+0x1ff8)/64U}));
    StreamFootprint exact_vector;for(unsigned b=0;b<8;++b)exact_vector.insert(pa+0x1ff8+b);
    assert(stream_byte_footprint(std::vector{v})==exact_vector);
    v.mask_bits=0;assert(stream_byte_footprint(std::vector{v}).empty());
    v.vm=true;v.vstart=1;
    assert((stream_footprint(std::vector{v})==StreamFootprint{pa/64U}));
    exact_vector.clear();for(unsigned b=0;b<8;++b)exact_vector.insert(pa+b);
    assert(stream_byte_footprint(std::vector{v})==exact_vector);
    // Repeated indexed gathers deduplicate only the footprint. Every active
    // destination still receives its independent literal byte expectation.
    SparseMemory memory;
    const auto physical=[&](unsigned offset){return offset<4096?pa+4096+offset:pa+offset-4096;};
    for(unsigned i=0;i<8192;++i)memory.write_reference_byte(physical(i),static_cast<unsigned char>(i*37+11));
    const std::array<std::array<unsigned,8>,2> offsets{{{{4095,4095,4096,4103,0,7,7,1}},{{7,7,4095,4096,1,1,4097,0}}}};
    std::vector<VectorMemoryTransaction> gather(2);StreamFootprint gather_bytes;unsigned destinations=0;
    StreamByteOracle oracle(memory);
    for(unsigned u=0;u<2;++u) {
        auto&t=gather[u];t.address=va;t.reference_pages=pages;t.addressing=VectorAddressingMode::indexed_unordered;
        t.eew=1;t.vsew=0;t.flow_num=8;t.vuop_idx=u;t.vl=13;t.vstart=2;t.vm=false;t.mask_bits=0x15ff;
        t.vma=t.vta=false;t.data.fill(0xa5);auto expected=t.data;
        for(unsigned e=0;e<8;++e) {
            const auto off=offsets[u][e];t.index[e*2]=off&255;t.index[e*2+1]=off>>8;
            const auto global=u*8+e;
            if(global>=2&&global<13&&((0x15ffU>>global)&1U)) {
                gather_bytes.insert(physical(off));expected[global]=static_cast<unsigned char>(off*37+11);++destinations;
            }
        }
        assert(oracle.vector_load(t)==expected);
    }
    assert(stream_byte_footprint(gather)==gather_bytes);
    assert(destinations==9&&gather_bytes.size()==6);
    v={};v.address=va+4092;v.reference_pages=pages;v.eew=3;v.vl=1;
    exact_vector.clear();for(unsigned b=0;b<8;++b)exact_vector.insert(physical(4092+b));
    assert(stream_byte_footprint(std::vector{v})==exact_vector);
    for (auto name : {"coverage","spec","corner","cmo-stress"}) {
        auto c=memblock::scenarios::RandomConstraints::preset(name);
        assert(c.stream_byte_bypass_per_mille==250);
        for (auto rate : {0U,1U,250U,1000U}) {
            c.apply("stream-byte-bypass="+std::to_string(rate));c.validate();
            assert(c.summary().find("target_stream_byte_bypass="+std::to_string(rate)+" ")!=std::string::npos);
        }
        c.apply("stream-byte-bypass=1001");bool rejected=false;
        try {c.validate();}catch(const std::invalid_argument&){rejected=true;}assert(rejected);
    }
}
'''

class StreamByteBypassTest(unittest.TestCase):
    def test_physical_alias_masks_cross_page_and_constraint_endpoints(self):
        with tempfile.TemporaryDirectory(prefix='memblock-disjoint-footprint-') as tmp:
            p = Path(tmp) / 'fixture.cpp'; p.write_text(CPP); binary = p.with_suffix('')
            result = subprocess.run(['c++','-std=c++20','-O2','-I',str(ROOT/'cpp'),str(p),'-o',str(binary)],capture_output=True,text=True,timeout=60)
            self.assertEqual(result.returncode,0,result.stdout+result.stderr)
            result = subprocess.run([str(binary)],capture_output=True,text=True,timeout=20)
            self.assertEqual(result.returncode,0,result.stdout+result.stderr)

    def record(self):
        return dict(constraint_schema=85,target_stream_disjoint_bypass=0,target_stream_byte_bypass=1000,
                    stream_disjoint_bypass=','.join(['0']*18),
                    stream_byte_bypass='2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1',
                    stream_byte_bypass_granules='1,1,0,1,1,0,1,1,0',
                    stream_ops='2,1,0,0,0',stream_cancel=','.join(map(str,[1,2,1]+[0]*26)))

    def test_lifecycle_and_once_per_relation_sparse_bounds(self):
        check=verifier._check_stream_byte_bypass;r=self.record();check(r)
        for i in range(18):
            counts=list(map(int,r['stream_byte_bypass'].split(',')));counts[i]+=1
            with self.subTest(counter=i),self.assertRaises(verifier.VerificationError):
                check(dict(r,stream_byte_bypass=','.join(map(str,counts))))
        for k in ['target_stream_byte_bypass','stream_byte_bypass','stream_byte_bypass_granules','stream_disjoint_bypass','stream_ops','stream_cancel']:
            bad=dict(r);del bad[k]
            with self.subTest(missing=k),self.assertRaises(verifier.VerificationError):check(bad)
        for rate in [True,-1,1001,'250',None]:
            with self.subTest(rate=rate),self.assertRaises(verifier.VerificationError):check(dict(r,target_stream_byte_bypass=rate))
        with self.assertRaises(verifier.VerificationError):check(dict(r,target_stream_byte_bypass=0))
        with self.assertRaises(verifier.VerificationError):check(dict(r,stream_disjoint_bypass=r['stream_byte_bypass']))
        for c in ['0,0,0,1,1,0,1,1,0','1,0,1,1,1,0,1,1,0','1,2,0,1,1,0,1,1,0']:
            with self.assertRaises(verifier.VerificationError):check(dict(r,stream_byte_bypass_granules=c))
        check(dict(r,target_stream_byte_bypass=0,stream_byte_bypass=','.join(['0']*18),stream_byte_bypass_granules=','.join(['0']*9)))
        check(dict(constraint_schema=84))

if __name__=='__main__':unittest.main()
