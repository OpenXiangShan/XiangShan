#!/usr/bin/env python3
"""Independently test the pure memory-replay controller's selected-path contract.

Public-edge pipelines, architectural ordinal groups and private semantic faults
provide host evidence; this does not certify DUT IO or full-CPU recovery.
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


class MemoryReplayControllerTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        parent = os.environ.get("MEMBLOCK_MEMORY_REPLAY_CONTROLLER_TEST_OUT")
        if parent:
            Path(parent).mkdir(parents=True, exist_ok=True)
            cls.work = Path(tempfile.mkdtemp(prefix="host-", dir=parent))
        else:
            cls.directory = tempfile.TemporaryDirectory(prefix="memblock-replay-controller-")
            cls.addClassCleanup(cls.directory.cleanup)
            cls.work = Path(cls.directory.name)
        cls.commands, cls.results = [], []
        cls.paths = [ROOT / path for path in (
            "cpp/environment/memory_replay_controller.hpp", "cpp/environment/model.inc",
            "cpp/memblock_env.hpp", "cpp/generated_interface_types.hpp", "cpp/failure.hpp",
            "unit/memory_replay_controller_fixture.cpp", "unit/test_memory_replay_controller.py")]
        cls.before = cls.hashes()
        cls.binary = cls.compile("memory_replay_controller",
                                 ROOT / "unit/memory_replay_controller_fixture.cpp")

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
        run = subprocess.run(command, capture_output=True, text=True, timeout=60)
        (cls.work / (name + ".compile.log")).write_text(run.stdout + run.stderr)
        if run.returncode:
            raise AssertionError(run.stdout + run.stderr)
        return binary

    @classmethod
    def tearDownClass(cls):
        after = cls.hashes()
        (cls.work / "summary.json").write_text(json.dumps(dict(
            source_hashes_before=cls.before, source_hashes_after=after,
            compile_commands=cls.commands, results=cls.results), indent=2) + "\n")
        if cls.before != after:
            raise AssertionError("controller host dependencies changed during tests")

    def run_case(self, name, *, binary=None):
        command = [str(binary or self.binary), name]
        run = subprocess.run(command, capture_output=True, text=True, timeout=30)
        self.results.append(dict(command=command, returncode=run.returncode,
                                 stdout=run.stdout, stderr=run.stderr))
        return run

    def check_case(self, name):
        run = self.run_case(name)
        self.assertEqual(run.returncode, 0, run.stdout + run.stderr)
        print("memory_replay_controller_evidence=" + run.stdout.strip())

    def test_isolated_public_return_and_count_edges(self):
        self.check_case("timeline")

    def test_adjacent_older_requests_filter_lifetime_levels_and_wrap(self):
        self.check_case("cross")

    def test_randomized_short_public_event_sequences(self):
        self.check_case("random")

    def test_exact_missing_wrong_duplicate_and_skipped_edge_acknowledgments(self):
        self.check_case("ack")

    def test_ftq_identity_unsupported_victims_and_admission_bounds(self):
        self.check_case("identity")

    def test_recovery_requires_stepped_edge_certification_and_fresh_generations(self):
        self.check_case("recovery")

    def test_architectural_prefix_retirement_excludes_killed_generations(self):
        self.check_case("retirement")

    def test_private_controller_faults_are_detected(self):
        production = (ROOT / "cpp/environment/memory_replay_controller.hpp").read_text()
        fixture = (ROOT / "unit/memory_replay_controller_fixture.cpp").read_text()
        faults = (
            ("early_return", "edge, edge + 2U, request, {}", "edge, edge + 1U, request, {}",
             "timeline", "return action appeared on the wrong public edge"),
            ("short_filter", "edge <= filter.request_edge + 4U", "edge <= filter.request_edge + 3U",
             "cross", "controller rejected a legal public edge"),
            ("long_filter", "edge <= filter.request_edge + 4U", "edge <= filter.request_edge + 5U",
             "cross", "controller accepted an invalid public edge"),
            ("lost_filter_age", "&& kills(filter.generation, filter.request.level, found->generation)",
             "&& false && kills(filter.generation, filter.request.level, found->generation)",
             "cross", "controller rejected a legal public edge"),
            ("preserve_anchor_siblings", "if (!instruction.killed_by && kills(found->generation, request.level, instruction.generation))",
             "if (!instruction.killed_by && instruction.generation != found->generation && kills(found->generation, request.level, instruction.generation))",
             "timeline", "generation cancellation differs from independent instruction suffix"),
            ("omit_return_ack", "(!item.returned && item.return_edge < edge) ||", "false ||",
             "ack", "missing return acknowledgment was accepted"),
            ("omit_count_ack", "(!item.cancellation_observed && item.return_edge + 2U < edge)", "false",
             "ack", "missing cancellation acknowledgment was accepted"),
            ("duplicate_return_ack", "!item || item->returned || item->return_edge != edge",
             "!item || item->return_edge != edge", "ack", "duplicate return acknowledgment was accepted"),
            ("future_recovery", "last_edge_ && *last_edge_ == edge && !returns_.empty()",
             "last_edge_ && !returns_.empty()", "recovery",
             "future unstepped edge certified recovery readiness"),
            ("timer_only_recovery", "!recovery_ready(edge) || !producer_recovery_complete",
             "!recovery_ready(edge)", "recovery", "timer alone certified producer recovery"),
        )
        for name, before, after, case, diagnostic in faults:
            with self.subTest(fault=name):
                self.assertEqual(production.count(before), 1,
                                 "private fault anchor changed: " + name)
                mutant = self.work / (name + ".hpp")
                mutant.write_text(production.replace(before, after, 1))
                source = self.work / (name + ".cpp")
                source.write_text(fixture.replace(
                    '#include "environment/memory_replay_controller.hpp"',
                    '#include "' + str(mutant) + '"'))
                binary = self.compile(name, source)
                run = self.run_case(case, binary=binary)
                self.assertNotEqual(run.returncode, 0, "private fault escaped: " + name)
                self.assertIn(diagnostic, run.stderr)
                print("memory_replay_controller_fault=" + name + ": " + run.stderr.strip())


if __name__ == "__main__":
    unittest.main()
