#!/usr/bin/env python3
"""Compile the production pair driver against scripted boundary readiness.

The fixture independently counts accepted edges and checks scalar metadata.
It does not model DUT execution, latency, or vector instruction legality.
"""
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


class LoadVectorPairTest(unittest.TestCase):
    def test_acceptance_edges_exactly_once_and_metadata(self):
        production = (ROOT / "cpp/environment/issue.cpp").read_text()
        start = production.index("auto Environment::Impl::issue_load_vector_pair(")
        method = production[start:production.index("\n}\n", start) + 3]
        method = method.replace("Environment::Impl::", "Harness::")
        source = r'''
#include "memblock_env.hpp"
#include <cassert>
#include <tuple>
using namespace memblock;
struct Adapter {
    std::vector<std::pair<bool, bool>> ready;
    unsigned cycle = 0, scalar_count = 0, vector_count = 0;
    std::vector<unsigned> scalar_edges, vector_edges;
    bool scalar_valid = false, vector_valid = false;
    generated::ScalarLoadIssue scalar;
    void drive_scalar_load_issue(unsigned lane, const generated::ScalarLoadIssue &issue) {
        assert(lane == 2);
        scalar = issue;
        scalar_valid = true;
    }
    void drive_vector_memory_issue(unsigned lane, const generated::VectorMemoryIssue &) {
        assert(lane == 1);
        vector_valid = true;
    }
    void refresh_comb() {}
    bool scalar_load_issue_ready(unsigned) const { return ready.at(cycle).first; }
    bool vector_memory_issue_ready(unsigned) const { return ready.at(cycle).second; }
    void clear_scalar_load_issue_valid(unsigned) { scalar_valid = false; }
    void clear_scalar_load_issue_valids() { scalar_valid = false; }
    void clear_vector_memory_issue_valids() { vector_valid = false; }
    void tick() {
        if (scalar_valid && ready.at(cycle).first) {
            ++scalar_count;
            scalar_edges.push_back(cycle);
        }
        if (vector_valid && ready.at(cycle).second) {
            ++vector_count;
            vector_edges.push_back(cycle);
        }
        ++cycle;
    }
};
struct Errors {
    bool failed = false;
    void record(const char *) { failed = true; }
};
struct Harness {
    Adapter dut_;
    Errors error_;
    bool last_load_vector_pair_same_cycle_ = false;
    bool issue_load_vector_pair(const LoadTransaction &, const VectorMemoryTransaction &, unsigned);
    generated::VectorMemoryIssue make_vector_memory_issue(const VectorMemoryTransaction &) { return {}; }
    void tick() { dut_.tick(); }
    bool check_components() { return !error_.failed; }
};
// PRODUCTION_METHOD
int main() {
    LoadTransaction load;
    load.lane = 2;
    load.address = 0x80000160;
    load.immediate = -17;
    load.pc = 0x1234;
    load.predecode_rvc = true;
    load.ftq_ptr = 27;
    load.ftq_offset = 5;
    load.rob = 47; load.rob_flag = true;
    load.lq = 33; load.lq_flag = true;
    load.sq = 12; load.sq_flag = true;
    load.rf_wen = false; load.fp_wen = true; load.pdest = 43;
    load.store_set_hit = true;
    load.wait_for_rob_flag = true; load.wait_for_rob_value = 46;
    load.load_wait_bit = true; load.load_wait_strict = true;
    VectorMemoryTransaction vector;
    vector.lane = 1;
    const std::vector<std::vector<std::pair<bool, bool>>> schedules = {
        {{true,true}}, {{true,false},{true,true}}, {{false,true},{true,true}},
        {{false,false},{false,false},{true,true}},
        {{false,false},{true,false},{false,false},{false,true}},
    };
    for (const auto &schedule : schedules) {
        Harness h;
        h.dut_.ready = schedule;
        assert(h.issue_load_vector_pair(load, vector, schedule.size()));
        assert(!h.dut_.scalar_valid && !h.dut_.vector_valid);
        assert(h.dut_.scalar_count == 1 && h.dut_.vector_count == 1);
        assert(h.last_load_vector_pair_same_cycle_ ==
            (h.dut_.scalar_edges.front() == h.dut_.vector_edges.front()));
        const auto &s = h.dut_.scalar;
        assert(s.pc == *load.pc && s.predecode_rvc && s.ftq_ptr == 27 && s.ftq_offset == 5);
        assert(s.rob_value == 47 && s.rob_flag && s.lq_value == 33 && s.lq_flag);
        assert(s.sq_value == 12 && s.sq_flag && !s.rf_wen && s.fp_wen && s.pdest == 43);
        assert(s.store_set_hit && s.wait_for_rob_flag && s.wait_for_rob_value == 46);
        assert(s.load_wait_bit && s.load_wait_strict);
        assert(s.src + load.immediate == load.address);
    }
    Harness reused;
    reused.dut_.ready = {{true,true},{true,false},{false,true}};
    assert(reused.issue_load_vector_pair(load, vector, 1));
    assert(reused.last_load_vector_pair_same_cycle_);
    assert(reused.issue_load_vector_pair(load, vector, 2));
    assert(!reused.last_load_vector_pair_same_cycle_);
    assert(reused.dut_.scalar_count == 2 && reused.dut_.vector_count == 2);
    reused.dut_.ready.push_back({false,false});
    assert(!reused.issue_load_vector_pair(load, vector, 1));
    assert(reused.error_.failed && !reused.last_load_vector_pair_same_cycle_);
    assert(!reused.dut_.scalar_valid && !reused.dut_.vector_valid);
}
'''.replace("// PRODUCTION_METHOD", method)
        with tempfile.TemporaryDirectory(prefix="memblock-load-vector-pair-") as directory:
            cpp = Path(directory) / "pair.cpp"
            binary = Path(directory) / "pair"
            cpp.write_text(source)
            compiled = subprocess.run(
                ["c++", "-std=c++20", "-I", str(ROOT / "cpp"), str(cpp), "-o", str(binary)],
                capture_output=True, text=True, timeout=60,
            )
            self.assertEqual(compiled.returncode, 0, compiled.stdout + compiled.stderr)
            result = subprocess.run([str(binary)], capture_output=True, text=True, timeout=10)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main()
