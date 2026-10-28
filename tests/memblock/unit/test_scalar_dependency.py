#!/usr/bin/env python3
"""Exercise production randomized dependency plans and byte-overlay oracles."""
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
#include "scenarios/mixed/scalar_dependency.hpp"
#include "scenarios/shared.hpp"
#include <iostream>
#include <set>
#include <sstream>
#include <stdexcept>
using namespace memblock::scenarios::mixed;
static void require(bool value) { if (!value) throw std::runtime_error("check failed"); }
static std::string signature(const ScalarDependencyPlan &plan, bool data) {
    std::ostringstream out;
    for (const auto &s : plan.older) {
        out << 's' << s.offset << ':' << s.width;
        if (data) out << ':' << s.data;
    }
    for (const auto &l : plan.loads) out << 'l' << l.offset << ':' << l.op;
    if (plan.younger) {
        out << 'y' << plan.younger->offset << ':' << plan.younger->width;
        if (data) out << ':' << plan.younger->data;
    }
    if (data) {
        for (auto e : plan.store_events) out << 'e' << e;
        for (auto l : plan.load_order) out << 'o' << l;
        for (auto delay : plan.event_delays) out << 'd' << delay;
    }
    return out.str();
}
int main() {
    for (const auto &[name, rate] :
         std::array<std::pair<const char *, unsigned>,4>{{
             {"coverage",350},{"spec",200},{"corner",500},{"cmo-stress",0}}}) {
        auto c = memblock::scenarios::RandomConstraints::preset(name);
        require(c.scalar_dependency_per_mille == rate);
        c.apply("scalar-dependency=0"); c.validate();
        require(c.scalar_dependency_per_mille==0);
        c.apply("scalar-dependency=1000"); c.validate();
        require(c.summary().find("target_scalar_dependency=1000")!=std::string::npos);
        c.apply("scalar-dependency=1001");
        bool rejected=false;
        try { c.validate(); } catch(const std::invalid_argument &) { rejected=true; }
        require(rejected);
    }
    std::array<unsigned char, 64> bytes{};
    for (unsigned byte = 0; byte < bytes.size(); ++byte) bytes[byte] = byte;
    std::vector<DependencyStore> stores{{8,8,0x8877665544332211ULL},
                                      {10,2,0xbeef}, {12,1,0x80}};
    require(scalar_dependency_expected(bytes, stores, {8,3}) == 0x88776680beef2211ULL);
    require(scalar_dependency_expected(bytes, stores, {12,0}) == 0xffffffffffffff80ULL);
    require(scalar_dependency_expected(bytes, stores, {12,4}) == 0x80);
    require(scalar_dependency_expected(bytes, stores, {10,1}) == 0xffffffffffffbeefULL);
    require(scalar_dependency_expected(bytes, stores, {10,5}) == 0xbeef);
    require(scalar_dependency_expected(bytes, stores, {8,2}) == 0xffffffffbeef2211ULL);
    require(scalar_dependency_expected(bytes, stores, {8,6}) == 0xbeef2211);
    require(scalar_dependency_expected(bytes, stores, {0,3}) == 0x0706050403020100ULL);
    require(scalar_dependency_expected(bytes, stores, {4,3}) == 0xbeef221107060504ULL);
    auto wrong_age = stores;
    wrong_age.push_back({8,8,0});
    require(scalar_dependency_expected(bytes, wrong_age, {8,3}) !=
            scalar_dependency_expected(bytes, stores, {8,3}));
    auto wrong_youngest = stores;
    std::reverse(wrong_youngest.begin(), wrong_youngest.end());
    require(scalar_dependency_expected(bytes, wrong_youngest, {8,3}) !=
            scalar_dependency_expected(bytes, stores, {8,3}));
    std::mt19937_64 first(17), replay(17), other(18);
    std::set<std::string> shapes;
    std::array<bool,4> depths{};
    unsigned younger=0, address_first=0, data_first=0, nontrivial_order=0;
    for (unsigned index=0; index<512; ++index) {
        auto plan=make_scalar_dependency_plan(first);
        require(signature(plan,true)==signature(make_scalar_dependency_plan(replay),true));
        require(signature(plan,true)!=signature(make_scalar_dependency_plan(other),true));
        require(plan.older.size()>=1 && plan.older.size()<=4);
        require(plan.loads.size()>=1 && plan.loads.size()<=3);
        depths[plan.older.size()-1]=true;
        younger += plan.younger.has_value();
        for (const auto &s:plan.older) {
            require(s.width==1 || s.width==2 || s.width==4 || s.width==8);
            require(s.offset % s.width==0 && s.offset+s.width<=32);
        }
        for (const auto &l:plan.loads) require(l.op<7 && l.offset%l.width()==0 && l.offset+l.width()<=32);
        const unsigned count=plan.older.size()+unsigned(plan.younger.has_value());
        std::set<unsigned> events(plan.store_events.begin(),plan.store_events.end());
        require(events.size()==count*2 && *events.begin()==0 && *events.rbegin()==count*2-1);
        for (unsigned s=0; s<count; ++s) {
            bool before=std::find(plan.store_events.begin(),plan.store_events.end(),s*2)<
                        std::find(plan.store_events.begin(),plan.store_events.end(),s*2+1);
            address_first+=before; data_first+=!before;
        }
        for (auto delay:plan.event_delays) require(delay<8);
        std::set<unsigned> loads(plan.load_order.begin(),plan.load_order.end());
        require(loads.size()==plan.loads.size() && *loads.begin()==0 && *loads.rbegin()==plan.loads.size()-1);
        nontrivial_order += !std::is_sorted(plan.load_order.begin(),plan.load_order.end());
        shapes.insert(signature(plan,false));
    }
    require(shapes.size()>350 && younger>100 && younger<400 && address_first && data_first && nontrivial_order);
    for(bool depth:depths) require(depth);
    std::cout << "unique_dependency_topologies=" << shapes.size() << '\n';
}
'''


class ScalarDependencyTest(unittest.TestCase):
    def test_real_generator_varies_topology_and_oracle_selects_youngest_older_bytes(self):
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory) / "fixture.cpp"
            binary = Path(directory) / "fixture"
            source.write_text(CPP)
            built = subprocess.run(
                ["c++", "-std=c++20", "-O0", "-I", str(ROOT / "cpp"),
                 str(source), "-o", str(binary)], capture_output=True, text=True,
                timeout=60,
            )
            self.assertEqual(built.returncode, 0, built.stdout + built.stderr)
            ran = subprocess.run([str(binary)], capture_output=True, text=True, timeout=10)
            self.assertEqual(ran.returncode, 0, ran.stdout + ran.stderr)
            self.assertIn("unique_dependency_topologies=", ran.stdout)

    def test_accounting_rejects_inconsistent_counts_without_sparse_bin_gate(self):
        check = verify_regression._check_scalar_dependency_accounting
        base = {"constraint_schema": 54, "target_scalar_dependency": 200,
                "scalar_dependency": "3,9,6,2,12,8,1"}
        check(base)
        check({**base, "scalar_dependency": "0,0,0,0,0,0,0"})
        for field, value in (
            ("target_scalar_dependency", True), ("target_scalar_dependency", 1001),
            ("target_scalar_dependency", 0),
            ("scalar_dependency", "3,9,6,4,12,8,1"),
            ("scalar_dependency", "3,9,6,2,12,8,7"),
            ("scalar_dependency", "3,4,6,2,12,8,1"),
            ("scalar_dependency", "3,15,6,2,12,8,1"),
            ("scalar_dependency", "3,9,2,2,12,8,1"),
            ("scalar_dependency", "3,9,10,2,12,8,1"),
            ("scalar_dependency", "3,9,6,2,1,1,1"),
            ("scalar_dependency", "3,9,6,2,25,24,1"),
        ):
            with self.subTest(field=field, value=value), self.assertRaises(verify_regression.VerificationError):
                check({**base, field: value})


if __name__ == "__main__":
    unittest.main()
