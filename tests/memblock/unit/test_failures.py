"""Exercise production failure aggregation without a DUT or Picker headers."""
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))
import run_regression
import benchmark_tests

CPP = r'''
#include "memblock_env.hpp"
namespace memblock {
#include "environment/scoreboards.inc"
}
int main(int argc, char **argv) {
    memblock::FailureState aggregate{"environment", "environment-contract"};
    if (argc > 1 && std::string(argv[1]) == "merge-overflow") {
        memblock::FailureState component{"dcache", "manager-contract"};
        for (unsigned index = 0; index < 100; ++index)
            component.record(std::to_string(index));
        aggregate.merge(component);
        aggregate.merge(component);
    } else if (argc > 1) {
        for (unsigned index = 0; index < 100; ++index)
            aggregate.record(std::to_string(index));
    } else {
        memblock::LoadScoreboard loads;
        memblock::StoreScoreboard stores;
        loads.set_failure_cycle(12);
        stores.set_failure_cycle(13);
        memblock::LoadTransaction load{};
        memblock::StoreTransaction store{};
        loads.expect(load, 123);
        loads.expect(load, 123);
        stores.expect(store);
        stores.expect(store);
        aggregate.merge(loads.failures());
        const std::string &first_message = aggregate.message();
        aggregate.merge(stores.failures());
        aggregate.merge(loads.failures());
        aggregate.merge(stores.failures());
        aggregate.record("quoted\"\\\n\t\001");
        if (&first_message != &aggregate.message() || first_message != loads.error()) return 2;
        if (aggregate.message() != loads.error() || aggregate.empty()) return 1;
    }
    std::cout << aggregate.json() << '\n';
}
'''


class FailureTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp = tempfile.TemporaryDirectory(prefix="memblock-failures-")
        cls.addClassCleanup(cls.temp.cleanup)
        source = Path(cls.temp.name) / "failures.cpp"
        cls.binary = source.with_suffix("")
        source.write_text(CPP)
        subprocess.run(["c++", "-std=c++20", "-I", str(ROOT / "cpp"),
                        str(source), "-o", str(cls.binary)], check=True,
                       capture_output=True, text=True, timeout=60)

    def run_case(self, *args):
        result = subprocess.run([str(self.binary), *args], check=True,
                                capture_output=True, text=True, timeout=10)
        return json.loads(result.stdout)

    def test_preserves_multiple_components_first_cause_and_provenance(self):
        report = self.run_case()
        self.assertEqual(report["schema_version"], 1)
        self.assertFalse(report["truncated"])
        failures = report["failures"]
        self.assertEqual(len(failures), 3)
        self.assertEqual([f["component"] for f in failures],
                         ["scalar-load", "scalar-store", "environment"])
        self.assertEqual([f["cycle"] for f in failures], [12, 13, None])
        for failure in failures:
            self.assertEqual(failure["classification"], "untriaged")
            self.assertGreater(failure["line"], 0)
            self.assertTrue(failure["file"])
        self.assertEqual(failures[2]["message"], 'quoted"\\\n\t\001')

    def test_bounds_failure_storm_and_reports_truncation(self):
        report = self.run_case("overflow")
        self.assertEqual(len(report["failures"]), 64)
        self.assertTrue(report["truncated"])
        self.assertEqual(report["failures"][0]["message"], "0")
        self.assertEqual(report["failures"][-1]["message"], "63")

    def test_component_truncation_survives_repeated_aggregation(self):
        report = self.run_case("merge-overflow")
        self.assertTrue(report["truncated"])
        self.assertEqual(len(report["failures"]), 64)
        self.assertTrue(all(f["component"] == "dcache" for f in report["failures"]))

    def test_public_facade_compiles_without_picker(self):
        # Compiling this fixture also verifies that no generated DUT definition
        # leaks through the public model/scoreboard/value-type boundary.
        facade = (ROOT / "cpp/memblock_env.hpp").read_text()
        self.assertNotIn('"generated_port_defaults.hpp"', facade)
        self.assertNotIn("UTMemBlock", facade)

    def test_controllers_retain_diagnostics_with_the_original_verdict(self):
        report = self.run_case()
        diagnostic = "\nMEMBLOCK_FAILURES " + json.dumps(report)
        for parse, terminal in (
            (run_regression.parse_summary, "MEMBLOCK_CONSTRAINED_RANDOM_FAIL seed=7 transactions=19"),
            (benchmark_tests.parse_terminal, "MEMBLOCK_SINGLE_LOAD_FAIL cycle=13"),
        ):
            fields = parse(terminal + diagnostic)
            self.assertEqual(fields["status"], "fail")
            self.assertEqual(fields["failure_reports"], [report])
            truncated = parse(terminal + '\nMEMBLOCK_FAILURES {"schema_version":')
            self.assertEqual(truncated["status"], "fail")
            self.assertTrue(truncated["failure_report_errors"])
