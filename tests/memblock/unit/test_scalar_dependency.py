#!/usr/bin/env python3
"""Exercise production randomized dependency plans and byte-overlay oracles."""
from __future__ import annotations

import hashlib
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
static std::string topology_signature(const ScalarDependencyPlan &plan, bool payload=false) {
    std::ostringstream out;
    for (const auto &s : plan.older) {
        out << 's' << s.offset << ':' << s.width;
        if (payload) out << ':' << s.data;
    }
    for (const auto &l : plan.loads) out << 'l' << l.offset << ':' << l.op << ':' << l.fp;
    if (plan.younger) {
        out << 'y' << plan.younger->offset << ':' << plan.younger->width;
        if (payload) out << ':' << plan.younger->data;
    }
    return out.str();
}
// Exclude payloads, byte widths, offsets, and delays from scheduler signatures.
static std::string schedule_signature(const ScalarDependencyPlan &plan, bool delays=false) {
    std::ostringstream out;
    for (const auto &event : plan.issue_events) {
        out << static_cast<unsigned>(event.kind) << ':' << event.index << ';';
        if (delays) out << 'd' << event.delay << ';';
    }
    return out.str();
}
static std::string replay_signature(const ScalarDependencyPlan &plan) {
    return topology_signature(plan,true) + schedule_signature(plan,true);
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
    require(scalar_dependency_expected(bytes, stores, {8,2,true}) == 0xffffffffbeef2211ULL);
    require(scalar_dependency_expected(bytes, stores, {8,3,true}) == 0x88776680beef2211ULL);
    std::mt19937_64 first(17), replay(17), other(18);
    std::set<std::string> topologies, schedules, fixed_count_schedules;
    std::array<bool,4> depths{}, widths{};
    std::array<bool,3> load_counts{};
    std::array<bool,7> load_ops{};
    std::array<bool,5> address_first{}, data_first{};
    std::array<bool,8> delays{};
    unsigned younger=0, seed_differences=0, nontrivial_load_order=0;
    unsigned early_loads=0, pending_sources=0, multiple_pending_sources=0;
    unsigned loads_before_younger_address=0, loads_before_younger_data=0, loads_after_younger=0;
    unsigned interleaved_windows=0, fixed_count_samples=0;
    constexpr unsigned samples=4096;
    for (unsigned sample=0; sample<samples; ++sample) {
        const auto plan=make_scalar_dependency_plan(first);
        require(replay_signature(plan)==replay_signature(make_scalar_dependency_plan(replay)));
        seed_differences += replay_signature(plan)!=replay_signature(make_scalar_dependency_plan(other));
        require(plan.older.size()>=1 && plan.older.size()<=4);
        require(plan.loads.size()>=1 && plan.loads.size()<=3);
        depths[plan.older.size()-1]=true;
        load_counts[plan.loads.size()-1]=true;
        younger += plan.younger.has_value();
        for (const auto &s:plan.older) {
            require(s.width==1 || s.width==2 || s.width==4 || s.width==8);
            require(s.offset % s.width==0 && s.offset+s.width<=32);
            widths[std::countr_zero(s.width)]=true;
        }
        for (const auto &l:plan.loads) {
            require(l.op<7 && !l.fp && l.offset%l.width()==0 && l.offset+l.width()<=32);
            load_ops[l.op]=true;
        }
        if(plan.younger) {
            require(plan.younger->offset==plan.loads.front().offset);
            require(plan.younger->width==plan.loads.front().width());
        }
        const unsigned older_count=plan.older.size();
        const unsigned count=older_count+unsigned(plan.younger.has_value());
        require(plan.issue_events.size()==count*2+plan.loads.size());
        std::vector<bool> seen_address(count,false), seen_data(count,false), seen_load(plan.loads.size(),false);
        std::vector<unsigned> load_order;
        bool window_interleaved=false;
        for (const auto &event:plan.issue_events) {
            require(event.delay<8);
            delays[event.delay]=true;
            switch(event.kind) {
            case DependencyIssueKind::address:
                require(event.index<count && !seen_address[event.index]);
                address_first[event.index] = address_first[event.index] || !seen_data[event.index];
                seen_address[event.index]=true;
                break;
            case DependencyIssueKind::data:
                require(event.index<count && !seen_data[event.index]);
                data_first[event.index] = data_first[event.index] || !seen_address[event.index];
                seen_data[event.index]=true;
                break;
            case DependencyIssueKind::load: {
                require(event.index<plan.loads.size() && !seen_load[event.index]);
                // Older STA is required; older STD and younger operands may wait.
                for(unsigned store=0;store<older_count;++store)require(seen_address[store]);
                seen_load[event.index]=true;
                load_order.push_back(event.index);
                bool early=false;
                for(unsigned store=0;store<count;++store)early |= !seen_address[store] || !seen_data[store];
                early_loads += early;
                window_interleaved |= early;
                const auto &load=plan.loads[event.index];
                unsigned pending=0;
                for(unsigned store=0;store<older_count;++store) {
                    const auto &source=plan.older[store];
                    pending += !seen_data[store] &&
                        source.offset<load.offset+load.width() && load.offset<source.offset+source.width;
                }
                pending_sources += pending>0;
                multiple_pending_sources += pending>1;
                if(plan.younger) {
                    loads_before_younger_address += !seen_address.back();
                    loads_before_younger_data += !seen_data.back();
                    loads_after_younger += seen_address.back() && seen_data.back();
                }
                break;
            }
            default: require(false);
            }
        }
        for(bool seen:seen_address)require(seen);
        for(bool seen:seen_data)require(seen);
        for(bool seen:seen_load)require(seen);
        interleaved_windows += window_interleaved;
        nontrivial_load_order += !std::is_sorted(load_order.begin(),load_order.end());
        topologies.insert(topology_signature(plan));
        schedules.insert(schedule_signature(plan));
        if(older_count==2 && plan.loads.size()==2 && plan.younger) {
            ++fixed_count_samples;
            fixed_count_schedules.insert(schedule_signature(plan));
        }
    }
    require(topologies.size()>1000 && schedules.size()>1000 && seed_differences>samples/2);
    require(fixed_count_samples>80 && fixed_count_schedules.size()>50);
    require(younger>samples/3 && younger<2*samples/3 && nontrivial_load_order>100);
    require(early_loads>100 && pending_sources>100 && multiple_pending_sources>10 && interleaved_windows>100);
    require(loads_before_younger_address>100 && loads_before_younger_data>100 && loads_after_younger>100);
    for(bool seen:depths)require(seen);
    for(bool seen:widths)require(seen);
    for(bool seen:load_counts)require(seen);
    for(bool seen:load_ops)require(seen);
    for(bool seen:address_first)require(seen);
    for(bool seen:data_first)require(seen);
    for(bool seen:delays)require(seen);
    std::cout << "unique_dependency_topologies=" << topologies.size() << '\n'
              << "unique_dependency_schedules_without_payload_or_delays=" << schedules.size() << '\n'
              << "fixed_2older_2load_younger_schedules=" << fixed_count_schedules.size()
              << "/" << fixed_count_samples << '\n'
              << "early_pending_multiple_loads=" << early_loads << ',' << pending_sources << ','
              << multiple_pending_sources << " interleaved_windows=" << interleaved_windows << '\n'
              << "younger_address_pending_data_pending_both_issued_loads=" << loads_before_younger_address
              << ',' << loads_before_younger_data << ',' << loads_after_younger << '\n';
}
'''


class ScalarDependencyTest(unittest.TestCase):
    def test_real_generator_varies_topology_and_oracle_selects_youngest_older_bytes(self):
        production_paths = (
            "cpp/scenarios/mixed/scalar_dependency.hpp", "cpp/scenarios/mixed/scalar_load.hpp",
            "cpp/scenarios/mixed_action_scalar.cpp", "cpp/random/constraints.inc",
            "cpp/random/coverage.inc", "scripts/verify_regression.py",
        )
        def hashes():
            return {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
                    for path in production_paths}
        before = hashes()
        self.addCleanup(lambda: self.assertEqual(before, hashes(), "production sources changed during tests"))
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
            self.assertIn("unique_dependency_schedules_without_payload_or_delays=", ran.stdout)
            self.assertIn("fixed_2older_2load_younger_schedules=", ran.stdout)
            self.assertIn("early_pending_multiple_loads=", ran.stdout)
            print(ran.stdout, end="")

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

    def test_schema61_interleaving_accepts_sparse_counts_without_nonzero_gate(self):
        check = verify_regression._check_scalar_dependency_accounting
        base = {"constraint_schema": 61, "target_scalar_dependency": 200,
                "scalar_dependency": "3,9,6,2,12,8,1"}
        for counts in ("0,0,0,0", "1,0,0,1", "1,1,0,1", "1,1,1,1", "4,3,1,2", "6,0,0,2"):
            with self.subTest(counts=counts):
                check({**base, "scalar_dependency_interleaving": counts})
        check({**base, "scalar_dependency": "0,0,0,0,0,0,0",
               "scalar_dependency_interleaving": "0,0,0,0"})
        # Schema 54 predates this field and must accept historical samples.
        check({**base, "constraint_schema": 54})

    def test_schema61_interleaving_rejects_missing_invalid_and_inconsistent_counts(self):
        check = verify_regression._check_scalar_dependency_accounting
        base = {"constraint_schema": 61, "target_scalar_dependency": 200,
                "scalar_dependency": "3,9,6,2,12,8,1"}
        with self.assertRaises(verify_regression.VerificationError):
            check(base)
        for counts in (
            None, True, 0, [4, 3, 1, 2], "True,0,0,0", "1,False,0,1",
            "4,3,1", "4,3,1,2,0", "", "4.0,3,1,2",
            "-1,0,0,0", "4,-1,1,2", "4,3,-1,2", "4,3,1,-1",
            "7,3,1,3", "4,5,1,2", "4,3,4,2", "4,3,1,4",
            "4,3,1,1", "1,0,0,2", "0,0,0,1", "1,0,0,0",
            "99999999999999999999,0,0,1", "4,99999999999999999999,1,2",
        ):
            with self.subTest(counts=counts), self.assertRaises(verify_regression.VerificationError):
                check({**base, "scalar_dependency_interleaving": counts})


if __name__ == "__main__":
    unittest.main()
