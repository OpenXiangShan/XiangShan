#!/usr/bin/env python3
"""Check production bulk redirect cancellation and private semantic faults.

This host fixture tests scoreboard obligations at a caller-certified boundary;
its scripted outputs do not establish DUT or full-CPU redirect timing.
"""
from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


class MixedRedirectCancellationTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        parent = os.environ.get("MEMBLOCK_MIXED_REDIRECT_CANCELLATION_TEST_OUT")
        if parent:
            Path(parent).mkdir(parents=True, exist_ok=True)
            cls.work = Path(tempfile.mkdtemp(prefix="host-", dir=parent))
        else:
            cls.directory = tempfile.TemporaryDirectory(prefix="memblock-mixed-cancel-")
            cls.addClassCleanup(cls.directory.cleanup)
            cls.work = Path(cls.directory.name)
        cls.commands, cls.results = [], []
        cls.paths = [ROOT / path for path in (
            "cpp/environment/scoreboards.inc", "cpp/environment/model.inc",
            "cpp/memblock_env.hpp", "cpp/generated_interface_types.hpp", "cpp/failure.hpp",
            "unit/mixed_redirect_cancellation_fixture.cpp",
            "unit/test_mixed_redirect_cancellation.py")]
        cls.before = cls.hashes()
        cls.binary = cls.compile("mixed_redirect_cancellation",
                                 ROOT / "unit/mixed_redirect_cancellation_fixture.cpp")

    @classmethod
    def hashes(cls):
        return {str(path): hashlib.sha256(path.read_bytes()).hexdigest()
                for path in cls.paths}

    @classmethod
    def compile(cls, name, source):
        binary = cls.work / name
        command = [os.environ.get("CXX", "c++"), "-std=c++20", "-O0", "-I",
                   str(ROOT / "cpp"), str(source), "-o", str(binary)]
        cls.commands.append(command)
        result = subprocess.run(command, capture_output=True, text=True, timeout=60)
        (cls.work / (name + ".compile.log")).write_text(result.stdout + result.stderr)
        if result.returncode:
            raise AssertionError(result.stdout + result.stderr)
        return binary

    @classmethod
    def tearDownClass(cls):
        after = cls.hashes()
        (cls.work / "summary.json").write_text(json.dumps(dict(
            source_hashes_before=cls.before, source_hashes_after=after,
            compile_commands=cls.commands, results=cls.results), indent=2) + "\n")
        if cls.before != after:
            raise AssertionError("scoreboard host dependencies changed during tests")

    def run_case(self, name, *, binary=None):
        command = [str(binary or self.binary), name]
        result = subprocess.run(command, capture_output=True, text=True, timeout=20)
        self.results.append(dict(command=command, returncode=result.returncode,
                                 stdout=result.stdout, stderr=result.stderr))
        return result

    def check_case(self, name):
        result = self.run_case(name)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        print("mixed_redirect_cancellation_evidence=" + result.stdout.strip())

    def test_mixed_instruction_suffix_preserves_survivors_and_completed_siblings(self):
        self.check_case("mixed")

    def test_bounded_live_rob_ages_wrap_and_both_redirect_levels(self):
        self.check_case("ages")

    def test_invalid_anchor_and_pending_identity_preflight_is_atomic(self):
        self.check_case("reject")

    def test_canceled_stale_outputs_and_corrupt_completions_remain_strict(self):
        self.check_case("stale")

    def test_same_identity_new_generation_uses_fresh_independent_data(self):
        self.check_case("reuse")

    def test_private_cancellation_faults_are_detected(self):
        production = (ROOT / "cpp/environment/scoreboards.inc").read_text()
        fixture = (ROOT / "unit/mixed_redirect_cancellation_fixture.cpp").read_text()
        predicate = "return younger || (flush_itself && equal);"
        faults = (
            ("drop_survivors", [(predicate, "return true;")], "mixed",
             "bulk cancellation counts disagree with instruction suffix"),
            ("preserve_anchor_siblings", [(predicate, "return younger;")], "mixed",
             "bulk cancellation counts disagree with instruction suffix"),
            ("drop_flush_after_anchor", [(predicate, "return younger || equal;")], "mixed",
             "bulk cancellation counts disagree with instruction suffix"),
            ("forget_prefetch_bookkeeping", [
                ("++canceled[1];\n                --pending_prefetch_;",
                 "++canceled[1]; /* private fault: omit pending-prefetch decrement */")],
             "mixed", "pending class bookkeeping does not match independent obligations"),
            ("bad_wrap_comparison", [
                ("? identity.value > rob : identity.value < rob;",
                 "? identity.value > rob : identity.value > rob;")], "ages",
             "bounded ROB wrap/redirect-level comparison is wrong"),
            ("reuse_stale_scalar_expectation", [
                ("void expect(const LoadTransaction &transaction, std::uint64_t data)\n    {",
                 "void expect(const LoadTransaction &transaction, std::uint64_t data)\n    {\n"
                 "        if (const auto old = stale_redirect_data_.find(rob_identity(transaction.rob, transaction.rob_flag));\n"
                 "            old != stale_redirect_data_.end()) data = old->second;"),
                ("++redirect_canceled_;\n            }\n            it = expected_.erase(it);",
                 "++redirect_canceled_;\n                stale_redirect_data_[it->first] = it->second.data;\n"
                 "            }\n            it = expected_.erase(it);"),
                ("    std::uint64_t canceled_observed_ = 0;",
                 "    std::unordered_map<RobIdentity, std::uint64_t, RobIdentityHash> stale_redirect_data_;\n"
                 "    std::uint64_t canceled_observed_ = 0;")], "reuse",
             "fresh scalar generation did not accept its independent data"),
        )
        for name, replacements, case, diagnostic in faults:
            with self.subTest(fault=name):
                changed = production
                for before, after in replacements:
                    self.assertEqual(changed.count(before), 1,
                                     "private fault anchor changed: " + name)
                    changed = changed.replace(before, after, 1)
                mutant = self.work / (name + ".inc")
                mutant.write_text(changed)
                source = self.work / (name + ".cpp")
                source.write_text(fixture.replace(
                    '#include "environment/scoreboards.inc"',
                    '#include "' + str(mutant) + '"'))
                binary = self.compile(name, source)
                result = self.run_case(case, binary=binary)
                self.assertNotEqual(result.returncode, 0, "private fault escaped: " + name)
                self.assertIn(diagnostic, result.stderr)
                print("mixed_redirect_cancellation_fault=" + name + ": " + result.stderr.strip())


if __name__ == "__main__":
    unittest.main()
