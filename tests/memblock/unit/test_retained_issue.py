#!/usr/bin/env python3
"""Exercise actual retained producer code with independent public-edge witnesses."""
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


class RetainedIssueTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.directory = tempfile.TemporaryDirectory(prefix="memblock-retained-issue-")
        cls.source = (ROOT / "unit/retained_issue_fixture.cpp").read_text()
        cls.binary = cls.compile(cls.source, "driver")

    @classmethod
    def tearDownClass(cls):
        cls.directory.cleanup()

    @classmethod
    def compile(cls, source, name):
        cpp = Path(cls.directory.name) / f"{name}.cpp"
        binary = cpp.with_suffix("")
        cpp.write_text(source)
        result = subprocess.run(["c++", "-std=c++20", "-I", str(ROOT / "cpp"),
                                 str(cpp), "-o", str(binary)], capture_output=True, text=True, timeout=60)
        if result.returncode:
            raise AssertionError(result.stdout + result.stderr)
        return binary

    def run_mode(self, mode, binary=None, passing=True):
        result = subprocess.run([str(binary or self.binary), mode], capture_output=True, text=True, timeout=30)
        if passing:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            print("retained_issue_evidence=" + result.stdout.strip())
        else:
            self.assertNotEqual(result.returncode, 0, "private driver fault escaped")

    def test_random_independent_lane_offers_and_exactly_once_events(self):
        self.run_mode("random")

    def test_circular_redirect_age_and_two_edge_tail(self):
        self.run_mode("redirect")

    def test_vector_fourteenth_stall_and_scalar_retention(self):
        self.run_mode("timeout")

    def test_binding_phase_and_identity_misuse_rejects(self):
        self.run_mode("misuse")

    def production_tick_source(self, mutation=None):
        production = (ROOT / "cpp/environment/cycle.cpp").read_text()
        start = production.index("auto Environment::Impl::tick(")
        method = production[start:production.index("\n}\n", start) + 3]
        method = method.replace("Environment::Impl::", "Harness::")
        if mutation:
            method = mutation(method)
        harness = r'''
struct Agent {
    Adapter *dut = nullptr;
    bool change_ready = false;
    std::array<bool, 9> driven_ready{};
    void drive(Adapter &) { if (change_ready) dut->ready = driven_ready; }
    void capture_before_tick(Adapter &) {}
    void update_after_tick() {}
};
struct Bridge {
    void capture_inputs() {}
    void check_before_edge() {}
    void advance_after_edge() {}
};
struct Queues {
    void sample_before_edge(Adapter &) {}
    void sample_after_edge(Adapter &, bool) {}
};
struct Feedback {
    void sample_before_edge(Adapter &, std::uint64_t, int, int, int) {}
    void sample_after_edge(Adapter &, std::uint64_t) {}
};
struct Harness {
    Adapter dut_;
    Driver *retained_issue_driver_ = nullptr;
    Agent memory_agent_{&dut_, true}, ptw_agent_, uncache_agent_;
    Bridge bridge_; Queues queues_; Feedback feedback_;
    int scoreboard_ = 0, store_scoreboard_ = 0, vector_scoreboard_ = 0;
    bool authorize_release_store_inputs() { return true; }
    void set_failure_cycle() {}
    std::uint64_t cycle() const { return dut_.edge; }
    void tick(bool monitor = true, const std::function<void()> &before_edge = {});
};
// TICK_METHOD
void production_tick_edges() {
    Harness h; Driver driver;
    h.retained_issue_driver_ = &driver;
    offer(driver, 0, 1); offer(driver, 1, 2); offer(driver, 7, 3);
    // Readiness immediately before manager drive predicts the opposite fires.
    h.dut_.ready[1] = h.dut_.ready[7] = true;
    h.memory_agent_.driven_ready[0] = true;
    h.tick();
    auto events = driver.take_events();
    assert(events.size() == 1 && events[0].tag == 1 && events[0].edge == 0);
    assert(h.dut_.accepted[0] == 1 && h.dut_.accepted[1] == 0 && h.dut_.accepted[7] == 0);
    // Ticks from another helper still retire a held offer exactly once.
    h.memory_agent_.driven_ready[0] = false;
    h.memory_agent_.driven_ready[7] = true;
    h.tick(false);
    events = driver.take_events();
    assert(events.size() == 1 && events[0].tag == 3 && events[0].edge == 1);
    assert(h.dut_.accepted[7] == 1 && !driver.empty());
    h.tick();
    assert(driver.take_events().empty() && h.dut_.accepted[7] == 1);
    h.dut_.queues.redirect.valid.data = 1;
    h.dut_.queues.redirect.rob_idx_value.data = 1;
    h.tick();
    assert(driver.take_events().empty() && h.dut_.valid[1]);
    h.dut_.queues.redirect.valid.data = 0;
    h.memory_agent_.driven_ready[1] = true;
    h.tick();
    events = driver.take_events();
    assert(events.size() == 1 && events[0].tag == 2 && events[0].edge == 4);
    assert(events[0].outcome == Outcome::accepted && events[0].killed);
    assert(driver.empty() && h.dut_.accepted[1] == 1);
    // An unbound legacy edge never acquires or clears producer pins.
    h.retained_issue_driver_ = nullptr;
    generated::ScalarLoadIssue s;
    h.dut_.drive_scalar_load_issue(2, s);
    h.memory_agent_.driven_ready[2] = true;
    h.tick();
    assert(h.dut_.accepted[2] == 1 && h.dut_.valid[2]);
    std::cout << "production_tick_edges=6\n";
}
'''.replace("// TICK_METHOD", method)
        return self.source.replace("// PRODUCTION_TICK_FIXTURE", harness).replace(
            "// PRODUCTION_TICK_MAIN", 'else if (mode == "tick") production_tick_edges();')

    def test_actual_tick_samples_after_manager_drive_and_tracks_other_helper_edges(self):
        self.run_mode("tick", self.compile(self.production_tick_source(), "tick"))

    def test_private_pre_manager_acceptance_fault_rejects(self):
        def before_manager(method):
            start = method.index("    if (retained_issue_driver_) retained_issue_driver_->capture_before_edge")
            end = method.index("\n    memory_agent_.capture_before_tick", start)
            capture = method[start:end]
            method = method[:start] + method[end:]
            anchor = "    memory_agent_.drive(dut_);"
            self.assertEqual(method.count(anchor), 1)
            return method.replace(anchor, capture + "\n" + anchor)
        binary = self.compile(self.production_tick_source(before_manager), "early-capture")
        self.run_mode("tick", binary, passing=False)

    def test_private_early_and_late_vector_timeout_faults_reject(self):
        header = (ROOT / "cpp/environment/retained_issue.hpp").read_text()
        anchor = "request.stalled_edges == 13U"
        self.assertEqual(header.count(anchor), 1)
        for boundary in (12, 14):
            with self.subTest(boundary=boundary):
                mutant = header.replace(anchor, f"request.stalled_edges == {boundary}U")
                # Private header text replaces the include; tracked source stays intact.
                source = self.source.replace('#include "environment/retained_issue.hpp"', mutant)
                binary = self.compile(source, f"timeout-{boundary}")
                self.run_mode("timeout", binary, passing=False)


if __name__ == "__main__":
    unittest.main()
