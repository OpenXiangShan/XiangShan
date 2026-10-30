"""Exercise the production alias round trip against independent event scripts."""

from pathlib import Path
import subprocess
import tempfile
import unittest


UT = Path(__file__).resolve().parents[1]


class AliasRefetchTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tmp = tempfile.TemporaryDirectory(prefix="memblock-alias-refetch-")
        cls.addClassCleanup(cls.tmp.cleanup)
        cls.binary = Path(cls.tmp.name) / "fixture"
        cls.compile(cls.binary)

    @classmethod
    def compile(cls, binary, include=None):
        result = subprocess.run(["c++", "-std=c++20", "-O0",
                        *(["-I", str(include)] if include else []),
                        "-I", str(UT / "cpp"), str(UT / "unit/alias_refetch_fixture.cpp"),
                        "-o", str(binary)], capture_output=True, text=True, timeout=60)
        if result.returncode:
            raise AssertionError(result.stdout + result.stderr)

    def run_case(self, case):
        p = subprocess.run([str(self.binary), case], capture_output=True, text=True, timeout=10)
        self.assertEqual(p.returncode, 0, p.stdout + p.stderr)

    def test_direct_and_correct_or_provisional_refetch(self):
        for case in ("direct", "correct-refetch", "dequeued-refetch", "cancel-refetch"):
            with self.subTest(case=case):
                self.run_case(case)

    def test_full_redirect_identity_and_return_latency(self):
        for case in ("bad-valid", "bad-level", "bad-rob", "bad-rob-wrap", "bad-ftq-wrap",
                     "bad-ftq", "bad-offset", "bad-rvc", "multiple-redirects", "late-return"):
            with self.subTest(case=case):
                self.run_case(case)

    def test_strict_data_and_terminal_accounting(self):
        for case in ("wrong-final", "wrong-no-redirect", "missing-final", "bad-metadata",
                     "final-redirect", "extra-allocation", "extra-count", "lost-entry",
                     "sq-cancel", "unobserved", "late-redirect", "duplicate"):
            with self.subTest(case=case):
                self.run_case(case)

    def test_held_store_head_completion_guard_and_separate_retirement(self):
        self.run_case("store")

    def test_private_recovery_fault_sensitivity(self):
        source = (UT / "cpp/scenarios/alias_refetch.hpp").read_text()
        faults = (
            ("strict", "load.tolerate_canceled_result = false;", "load.tolerate_canceled_result = true;", "dequeued-refetch"),
            ("cursor", "environment.lq_redirect_canceled_observed();", "environment.lq_dequeued();", "dequeued-refetch"),
            ("timing", "violation.last_cycle + 2U", "violation.last_cycle + 1U", "dequeued-refetch"),
            ("sq", "environment.sq_canceled() != sq_canceled_before ||", "false ||", "sq-cancel"),
            ("count", "environment.writebacks() != completed_before + 1U + result.refetched - result.discarded ||", "false ||", "extra-count"),
            ("guard", "environment.run_cycles(kStreamScalarWritebackToHead - 2U)", "environment.run_cycles(0)", "store"),
            ("head", "environment.set_rob_head_state(rob_pointer_value(next_rob),\n            rob_pointer_flag(next_rob), false)", "true", "store"),
        )
        for name, before, after, case in faults:
            with self.subTest(fault=name):
                self.assertEqual(source.count(before), 1)
                private = Path(self.tmp.name) / name
                (private / "scenarios").mkdir(parents=True)
                (private / "scenarios/alias_refetch.hpp").write_text(source.replace(before, after))
                binary = private / "fixture"
                self.compile(binary, private)
                p = subprocess.run([str(binary), case], capture_output=True, text=True, timeout=10)
                self.assertNotEqual(p.returncode, 0, p.stdout + p.stderr)


if __name__ == "__main__":
    unittest.main()
