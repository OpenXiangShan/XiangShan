#!/usr/bin/env python3
"""Check actual multi-load generation and architectural obligation transitions."""
import json
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


class UnresolvedLoadGroupTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.directory = tempfile.TemporaryDirectory(prefix="memblock-unresolved-load-group-")
        cls.addClassCleanup(cls.directory.cleanup)
        cls.binary = Path(cls.directory.name) / "fixture"
        command = ["c++", "-std=c++20", "-O0", "-I", str(ROOT / "cpp"),
                   str(ROOT / "unit/unresolved_load_group_fixture.cpp"), "-o", str(cls.binary)]
        built = subprocess.run(command, capture_output=True, text=True, timeout=60)
        if built.returncode:
            raise AssertionError(built.stdout + built.stderr)

    def run_mode(self, mode):
        ran = subprocess.run([str(self.binary), mode], capture_output=True, text=True, timeout=20)
        self.assertEqual(ran.returncode, 0, ran.stdout + ran.stderr)
        evidence = json.loads(ran.stdout)
        print("unresolved_load_group_evidence=" + json.dumps(evidence, sort_keys=True))
        return evidence

    def test_random_graphs_and_independent_byte_oracles(self):
        self.assertEqual(self.run_mode("planner")["samples"], 4608)

    def test_full_suffix_obligations_and_bad_transitions(self):
        self.assertEqual(self.run_mode("obligations")["invalid_transitions_rejected"], 18)


    def test_private_fixed_schedule_and_missing_suffix_mutants_are_detected(self):
        mutations = (
            ("fixed_issue_order", "const unsigned choice = random() % ready.size();",
             "const unsigned choice = 0;", "planner", "issue-order"),
            ("youngest_victim", "const auto &id = identities_[victim_];",
             "const auto &id = identities_.back();", "obligations", "legal redirect"),
            ("missing_suffix_obligations", "!std::all_of(refetched_.begin() + victim_, refetched_.end(), [](bool done) { return done; })",
             "!refetched_[victim_]", "obligations", "invalid obligation"),
        )
        for name, old, new, mode, diagnostic in mutations:
            with self.subTest(mutant=name):
                folder = Path(self.directory.name) / name
                includes = folder / "scenarios/mixed"
                includes.mkdir(parents=True)
                for source in (ROOT / "cpp/scenarios/mixed").glob("*.hpp"):
                    shutil.copyfile(source, includes / source.name)
                header = includes / "unresolved_load_group.hpp"
                text = header.read_text()
                self.assertEqual(text.count(old), 1)
                header.write_text(text.replace(old, new))
                binary = folder / "fixture"
                built = subprocess.run(["c++", "-std=c++20", "-O0", "-I", str(folder),
                                        "-I", str(ROOT / "cpp"),
                                        str(ROOT / "unit/unresolved_load_group_fixture.cpp"),
                                        "-o", str(binary)], capture_output=True, text=True, timeout=60)
                self.assertEqual(built.returncode, 0, built.stdout + built.stderr)
                ran = subprocess.run([str(binary), mode], capture_output=True, text=True, timeout=20)
                self.assertNotEqual(ran.returncode, 0, "private mutant escaped")
                self.assertIn(diagnostic, ran.stderr)


if __name__ == "__main__":
    unittest.main()
