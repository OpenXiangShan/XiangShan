#!/usr/bin/env python3
"""Exercise the production manager's alias directory, protocol and byte oracle."""
from pathlib import Path
import hashlib
import json
import os
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]

class DcacheManagerTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        parent = os.environ.get("MEMBLOCK_DCACHE_MANAGER_TEST_OUT")
        if parent:
            Path(parent).mkdir(parents=True, exist_ok=True)
            cls.work = Path(tempfile.mkdtemp(prefix="host-", dir=parent))
        else:
            cls.workspace = tempfile.TemporaryDirectory(prefix="memblock-manager-")
            cls.addClassCleanup(cls.workspace.cleanup)
            cls.work = Path(cls.workspace.name)
        cls.results, cls.commands = [], []
        cls.paths = [ROOT / "cpp/environment/dcache_agent.inc",
                     ROOT / "unit/dcache_manager_fixture.cpp", Path(__file__).resolve()]
        cls.before = {str(path): hashlib.sha256(path.read_bytes()).hexdigest() for path in cls.paths}
        cls.binary = cls.compile("manager", ROOT / "unit/dcache_manager_fixture.cpp")

    @classmethod
    def compile(cls, name, source):
        binary = cls.work / name
        command = [os.environ.get("CXX", "c++"), "-std=c++20", "-O0", "-I", str(ROOT / "cpp"),
                   str(source), "-o", str(binary)]
        cls.commands.append(command)
        result = subprocess.run(command, text=True, capture_output=True, timeout=60)
        (cls.work / (name + ".compile.log")).write_text(result.stdout + result.stderr)
        if result.returncode:
            raise RuntimeError(result.stdout + result.stderr)
        return binary

    @classmethod
    def tearDownClass(cls):
        after = {str(path): hashlib.sha256(path.read_bytes()).hexdigest() for path in cls.paths}
        (cls.work / "summary.json").write_text(json.dumps(dict(
            source_hashes_before=cls.before, source_hashes_after=after,
            compile_commands=cls.commands, results=cls.results), indent=2) + "\n")
        if cls.before != after:
            raise AssertionError("manager inputs changed during host tests")

    def run_case(self, name, *, binary=None, timeout=20):
        command = [str(binary or self.binary), name]
        result = subprocess.run(command, text=True, capture_output=True, timeout=timeout)
        self.results.append(dict(command=command, returncode=result.returncode,
                                 stdout=result.stdout, stderr=result.stderr))
        return result

    def test_grouped_release_versions_match_independent_forward_enumeration(self):
        result = self.run_case("release-version-exhaustive")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_ordinary_input_versions_and_lifetimes(self):
        for name in ("old", "applied", "torn", "before-acquire", "noneligible",
                     "registration-order", "overlap-torn", "midbeat", "late-value",
                     "residual", "rollback", "aba", "explicit", "explicit-midbeat",
                     "checkpoint-protection", "retired-order", "late-older-order",
                     "immutable-order", "reset", "epoch-boundary", "epoch-noneligible",
                     "epoch-legacy-history", "retained-owner-rollback"):
            with self.subTest(case=name):
                result = self.run_case("release-input-" + name)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_cancellation_removes_only_unapplied_groups_after_certified_redirect(self):
        for name in ("empty", "canceled-bytes", "removed-order", "numeric-reuse", "unaffected",
                     "multiple-groups", "multiple-lines", "frozen-first-c", "frozen-forbidden", "baseline",
                     "applied-floor", "retired-set", "older-unauthorized", "legacy", "epoch-end",
                     "reject-duplicate", "reject-unknown", "reject-authorized", "reject-retired"):
            with self.subTest(case=name):
                result = self.run_case("release-cancel-" + name)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_private_cancellation_faults_are_detected(self):
        production = (ROOT / "cpp/environment/dcache_agent.inc").read_text()
        fixture = (ROOT / "unit/dcache_manager_fixture.cpp").read_text()
        erase = "std::erase_if(inputs, [&](const auto &input) { return selected.contains(input.order); });"
        unindex = "for (const auto order : orders) release_input_orders_.erase(order);"
        faults = (
            ("cancel_drop_unselected", erase,
             "for (const auto &input : inputs) release_input_orders_.erase(input.order); inputs.clear();",
             "unaffected", "cancellation removed an unselected input"),
            ("cancel_keep_order_index", unindex,
             "/* private fault: keep canceled order indices */", "numeric-reuse", "immutable group"),
            ("cancel_drop_baseline", erase,
             erase + " release_input_lines_.at(line).baseline = reference_memory_.read_beat(line, kLineBytes);",
             "baseline", "ordinary input release decision incorrect"),
            ("cancel_drop_floor", erase,
             erase + " release_input_lines_.at(line).applied_order_mask = 0;",
             "applied-floor", "cancellation erased the applied-order floor"),
            ("cancel_drop_retired", unindex,
             unindex + " retired_release_input_orders_.clear();",
             "retired-set", "cancellation made an applied/retired order reusable"),
            ("cancel_drop_legacy", erase,
             erase + " release_input_lines_.at(line).legacy_updates.clear();",
             "legacy", "ordinary input release decision incorrect"),
            ("cancel_reset_frozen", unindex,
             unindex + " release_data_.reset();",
             "frozen-first-c", "cancellation discarded the active first-C snapshot"),
            ("cancel_allow_authorized", "input == inputs.end() || input->order != order || input->authorized",
             "input == inputs.end() || input->order != order",
             "reject-authorized", "invalid cancellation was accepted"),
            ("cancel_partial_preflight", "            lines.insert(found->second);",
             "            lines.insert(found->second); inputs.erase(input); release_input_orders_.erase(order);",
             "reject-unknown", "failed cancellation changed an earlier valid group"),
        )
        for name, before, after, case, diagnostic in faults:
            with self.subTest(fault=name):
                self.assertEqual(production.count(before), 1, "fault anchor changed: " + name)
                mutant = self.work / (name + ".inc")
                # The private partial-preflight fault intentionally mutates this
                # input vector; production keeps preflight access const.
                changed = production.replace(before, after, 1)
                if name == "cancel_partial_preflight":
                    changed = changed.replace("const auto &inputs = tracked->second.inputs;",
                                              "auto &inputs = tracked->second.inputs;", 1)
                mutant.write_text(changed)
                source = self.work / (name + ".cpp")
                source.write_text(fixture.replace('#include "environment/dcache_agent.inc"',
                                                 '#include "' + str(mutant) + '"'))
                binary = self.compile(name, source)
                result = self.run_case("release-cancel-" + case, binary=binary)
                self.assertNotEqual(result.returncode, 0, "private fault escaped: " + name)
                self.assertIn(diagnostic, result.stderr)

    def test_ordinary_input_compaction_matches_forward_histories(self):
        result = self.run_case("release-input-exhaustive", timeout=60)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("histories=729", result.stdout)

    def test_input_journal_sustains_full_epoch_element_budget(self):
        result = self.run_case("release-input-large-history", timeout=60)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("groups=524288", result.stdout)

    def test_private_input_version_faults_are_detected(self):
        production = (ROOT / "cpp/environment/dcache_agent.inc").read_text()
        fixture = (ROOT / "unit/dcache_manager_fixture.cpp").read_text()
        faults = (
            ("unauthorized_input", "if (input.authorized && ((input.virtual_address >> 12U) & 3U) == owner->second.alias)",
             "if (((input.virtual_address >> 12U) & 3U) == owner->second.alias)",
             "noneligible", "ordinary input release decision incorrect"),
            ("drop_residual_on_acquire", "if (tracked != release_input_lines_.end()) tracked->second.baseline = refill;",
             "if (tracked != release_input_lines_.end()) { tracked->second.baseline = refill; tracked->second.inputs.clear(); }",
             "residual", "ordinary input release decision incorrect"),
            ("forget_validated_progress", "            version.baseline = promoted;",
             "            /* private fault: retain historical baseline */",
             "retained-owner-rollback", "ordinary input release decision incorrect"),
            ("discard_ambiguous_aba", "            version.baseline = promoted;",
             "            version.baseline = promoted; if (included.empty()) version.inputs.clear();",
             "aba", "ordinary input release decision incorrect"),
            ("relax_exact_override", "(!state.exact.empty() && state.actual != state.exact)", "false",
             "explicit", "ordinary input release decision incorrect"),
            ("reverse_registered_order", "[](const auto &candidate, auto value) { return candidate.order < value; });\n        version.inputs.insert",
             "[](const auto &candidate, auto value) { return true; });\n        version.inputs.insert",
             "registration-order", "ordinary input release decision incorrect"),
            ("stale_epoch_backing", "? reference_memory_.read_beat(line, kLineBytes)",
             "? memory_.read_beat(line, kLineBytes)",
             "epoch-boundary", "ordinary input release decision incorrect"),
            ("import_pre_epoch_legacy", "if (!release_input_epoch_) version.legacy_updates = std::move(owner->second.updates);",
             "version.legacy_updates = std::move(owner->second.updates);",
             "epoch-legacy-history", "ordinary input release decision incorrect"),
            ("permit_late_older_input", "((version.applied_order_mask >> offset) & 1U) && order <= version.applied_order[offset]",
             "false", "late-older-order", "registration reintroduced already observed same-byte history"),
        )
        for name, before, after, case, diagnostic in faults:
            with self.subTest(fault=name):
                self.assertEqual(production.count(before), 1, "fault anchor changed: " + name)
                mutant = self.work / (name + ".inc")
                mutant.write_text(production.replace(before, after, 1))
                source = self.work / (name + ".cpp")
                source.write_text(fixture.replace('#include "environment/dcache_agent.inc"',
                                                 '#include "' + str(mutant) + '"'))
                binary = self.compile(name, source)
                result = self.run_case("release-input-" + case, binary=binary)
                self.assertNotEqual(result.returncode, 0, "private fault escaped: " + name)
                self.assertIn(diagnostic, result.stderr)

    def test_line_grant_lifecycle_uses_manager_transaction_provenance(self):
        for name in ("holds", "final-d-before-e", "e-before-final-d", "same-pa-pending",
                     "recall-pending", "acquire-perm", "scope", "reset"):
            with self.subTest(case=name):
                result = self.run_case("line-grants-" + name)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_alias_protocol_and_independent_epoch_data(self):
        for name in ("dirty", "branch", "same-alias-write", "release-race", "ntonn-release",
                     "early-e", "final-d-before-e", "early-e-late-d", "serial-probes", "reset",
                     "bad-byte", "changed-source", "changed-report", "branch-upgrade",
                     "missing-data", "duplicate", "release-version-pending", "release-version-applied",
                     "release-version-midbeat", "release-version-late-value", "release-version-torn",
                     "release-version-overlap-order", "release-version-disjoint", "release-version-explicit"):
            with self.subTest(case=name):
                result = self.run_case(name)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

if __name__ == "__main__":
    unittest.main()
