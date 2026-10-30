#!/usr/bin/env python3
"""Independent address enumeration and lifetime checks for page dependencies."""
import importlib.util
from pathlib import Path
import subprocess
import tempfile
import sys
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT/'scripts'))
spec = importlib.util.spec_from_file_location('verify_dependency_pages', ROOT/'scripts/verify_regression.py')
verifier = importlib.util.module_from_spec(spec)
spec.loader.exec_module(verifier)


class DependencyPagesTest(unittest.TestCase):
    def test_literal_targets_guards_and_parser(self):
        source = r'''
#include "scenarios/mixed/stream_dependency.hpp"
#include "scenarios/mixed/state.hpp"
#include <cassert>
using namespace memblock::scenarios::mixed;
using memblock::scenarios::RandomConstraints;
int main() {
    const std::uint64_t b = 0x2000000000ULL;
    assert(stream_dependency_address(b + 0x1088, 0, b, 132) == b + 0x1088);
    assert(stream_dependency_address(b + 0x1088, 64, b, 132) == b + 0x2088);
    assert(stream_dependency_address(b + 0x1088, 65, b, 132) == b + 0x20c8);
    assert(stream_dependency_address(b + 0x82ff8, 1, b, 132) == b + 0x82038);
    assert(stream_dependency_address(b + 0x82ff8, 64, b, 132) == b + 0x1ff8);
    assert(stream_dependency_address(b + 8, 0, b, 132) == b + 0x82008);
    assert(stream_dependency_address(b + 0x83008, 0, b, 132) == b + 0x1008);
    unsigned cases = 0;
    for (unsigned pages : {3U, 16U, 132U}) {
        for (unsigned p = 0; p < pages; ++p) {
            for (unsigned v = 0; v < pages * 64U; ++v) {
                const auto a = b + p * 4096ULL + 4089;
                auto wanted_page = p == 0 ? pages - 2 : p == pages - 1 ? 1U : p;
                for (unsigned step = 0; step < v / 64U; ++step)
                    wanted_page = wanted_page == pages - 2 ? 1U : wanted_page + 1U;
                auto wanted_offset = 4089U;
                for (unsigned step = 0; step < v % 64U; ++step)
                    wanted_offset = (wanted_offset + 64U) % 4096U;
                const auto target = stream_dependency_address(a, v, b, pages);
                assert(target == b + wanted_page * 4096ULL + wanted_offset);
                assert(target % 64U == a % 64U);
                ++cases;
            }
        }
    }
    assert(cases > 1000000);
    for (auto address : {b - 1U, b + 132U * 4096U}) {
        bool rejected = false;
        try { stream_dependency_address(address, 0, b, 132); }
        catch (const std::logic_error &) { rejected = true; }
        assert(rejected);
    }
    for (auto preset : {"coverage", "spec", "corner", "cmo-stress"}) {
        auto c = RandomConstraints::preset(preset);
        assert(c.stream_dependency_pages_per_mille == 250);
        for (unsigned rate : {0U, 1U, 250U, 1000U}) {
            c.apply("stream-dependency-pages=" + std::to_string(rate));
            c.validate();
            assert(c.summary().find("target_stream_dependency_pages=" + std::to_string(rate)) != std::string::npos);
        }
        c.apply("stream-dependency-pages=1001");
        bool rejected = false;
        try { c.validate(); } catch (const std::invalid_argument &) { rejected = true; }
        assert(rejected);
    }
}
'''
        with tempfile.TemporaryDirectory(prefix='memblock-page-dependency-') as tmp:
            path = Path(tmp)/'check.cpp'; path.write_text(source)
            binary = path.with_suffix('')
            subprocess.run(['c++','-std=c++20','-O2','-I',str(ROOT/'cpp'),str(path),'-o',str(binary)], check=True, capture_output=True, text=True)
            subprocess.run([str(binary)], check=True, capture_output=True, text=True)

    def record(self):
        # Per class: one accepted edge and one independently canceled edge.
        return dict(constraint_schema=83, target_stream_dependency_pages=250,
            target_stream_dependency=1000, stream_dependency_pages=','.join(map(str,[2]*5+[1]*10)),
            stream_dependencies='10,0,5,0,0,0,5,0,1', stream_ops='1,1,1,1,1',
            stream_cancel=','.join(map(str,[1]+[1]*5+[0]*10+[5]+[0]*12)))

    def test_independent_accepted_and_canceled_lifetimes(self):
        check = verifier._check_stream_dependency_pages
        record = self.record(); check(record)
        for index in range(15):
            counts = [2]*5+[1]*10; counts[index] += 1
            with self.assertRaises(verifier.VerificationError):
                check(dict(record, stream_dependency_pages=','.join(map(str,counts))))
        for field in ('stream_dependency_pages','stream_dependencies','stream_ops','stream_cancel','target_stream_dependency_pages'):
            bad = dict(record); del bad[field]
            with self.assertRaises(verifier.VerificationError): check(bad)
        for rate in (True,-1,1001,'250',None):
            with self.assertRaises(verifier.VerificationError): check(dict(record,target_stream_dependency_pages=rate))
        for rate in (0,1000):
            check(dict(record,target_stream_dependency_pages=rate,stream_dependency_pages=','.join(['0']*15)))
        with self.assertRaises(verifier.VerificationError): check(dict(record,target_stream_dependency_pages=0))
        check(dict(constraint_schema=82))


if __name__ == '__main__': unittest.main()
