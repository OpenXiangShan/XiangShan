#!/usr/bin/env python3
"""Host contracts for per-store STA replay and schema73 stream accounting.

The production replay header is compiled alone, without the environment or DUT.
A separate architectural event ledger counts accepted work and actual feedback
intervals, then exercises the production serializer, parser, and verifier. The
ledger does not copy the verifier's aggregate inequalities. These tests qualify
host bookkeeping; they do not establish DUT timing or full-CPU reachability.

MEMBLOCK_STORE_ADDRESS_REPLAY_ARTIFACTS retains private helpers and mutations;
MEMBLOCK_STORE_ADDRESS_REPLAY_REPORT records commands, hashes, and case evidence.
"""
from __future__ import annotations

from collections import deque
from dataclasses import dataclass
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import random
import re
import shlex
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
SOURCE_ROOT = Path(os.environ.get("MEMBLOCK_STORE_ADDRESS_REPLAY_SOURCE_ROOT", ROOT)).resolve()
HEADER = "cpp/scenarios/mixed/store_address_replay.hpp"
FIELDS = dict(stream=12, stream_ops=5, stream_issue=6, stream_vector_uops=2,
              stream_waiting_head=2, stream_recovery=20, stream_dependencies=9,
              stream_history=4, stream_sta=11)
KINDS = ("load", "vload", "store", "vstore", "prefetch")
PEAKS = {("stream", 6), ("stream", 7), ("stream_history", 1),
         ("stream_dependencies", 8), ("stream_sta", 10)}


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def csv(values):
    return ",".join(map(str, values))


@dataclass(frozen=True)
class Instruction:
    kind: str
    uops: int = 1
    recovery: bool = False


@dataclass(frozen=True)
class History:
    instructions: tuple[Instruction, ...]
    # (cycle, event, instruction ordinal, uop ordinal)
    events: tuple[tuple[int, str, int, int], ...]

    def counts(self):
        """Count observable events and pair waits with their own feedback."""
        rows = {name: [0] * width for name, width in FIELDS.items()}
        if not self.instructions:
            assert not self.events
            return rows
        live, admitted, completed, retired = deque(), set(), set(), set()
        issued, attempts, waiting, translated, retry_after = {}, {}, {}, set(), {}
        intervals, accepted = [], []
        last_cycle = 0
        for cycle, event, age, slot in self.events:
            assert cycle >= last_cycle
            last_cycle = cycle
            item = self.instructions[age]
            required = ({("address", 0), ("data", 0)} if item.kind == "store" else
                        {("issue", index) for index in range(item.uops)})
            if event in ("admit", "address", "data", "issue"):
                if live and live[0] in completed:
                    rows["stream_waiting_head"][int(event != "admit")] += 1
            if event == "admit":
                assert age == len(admitted) and age not in admitted and len(live) < 8
                pending = any(issued[older] and older not in completed for older in live)
                rows["stream"][3] += pending
                rows["stream"][4] += bool(retired and live)
                live.append(age)
                admitted.add(age)
                issued[age] = set()
                rows["stream_ops"][KINDS.index(item.kind)] += 1
                accepted.append((cycle, 4, age))
            elif event in ("address", "data", "issue"):
                assert age in live and age not in completed and (event, slot) in required
                first = (event, slot) not in issued[age]
                assert first or (event == "address" and not item.recovery)
                if event == "address" and not item.recovery:
                    assert age not in waiting and age not in translated
                    if attempts.get(age, 0):
                        assert cycle >= retry_after[age]
                    waiting[age] = cycle
                    rows["stream_sta"][int(not first)] += 1
                    attempts[age] = attempts.get(age, 0) + 1
                issued[age].add((event, slot))
                bin_index = ({"load": 0, "vload": 3, "store": 1,
                              "vstore": 4, "prefetch": 5}[item.kind])
                if event == "data":
                    bin_index = 2
                if first:
                    rows["stream_issue"][bin_index] += 1
                    if item.kind in ("vload", "vstore"):
                        rows["stream_vector_uops"][int(item.kind == "vstore")] += 1
                overlap_bin = (6 if event == "address" else 7 if event == "data" else
                               5 if item.kind == "load" else 9 if item.kind == "prefetch" else 8)
                accepted.append((cycle, overlap_bin, age))
            elif event in ("hit", "miss"):
                assert item.kind == "store" and not item.recovery and age in waiting
                start = waiting.pop(age)
                assert cycle >= start
                intervals.append((age, start, cycle))
                rows["stream_sta"][2 + int(event == "miss")] += 1
                if event == "hit":
                    translated.add(age)
                else:
                    retry_after[age] = cycle + 8
            elif event == "complete":
                assert age in live and age not in completed and issued[age] == required
                assert item.kind != "store" or item.recovery or age in translated
                completed.add(age)
            elif event == "retire":
                assert live and live[0] == age and age in completed and age not in retired
                rows["stream"][5] += len(live) > 1
                rows["stream_recovery"][9] += item.kind == "store" and item.recovery
                live.popleft()
                retired.add(age)
            else:
                raise AssertionError(event)
            rows["stream"][6] = max(rows["stream"][6], len(live))
            rows["stream"][7] = max(rows["stream"][7], sum(
                bool(issued[older]) and older not in completed for older in live))
        assert not live and not waiting and len(retired) == len(self.instructions)
        # A feedback edge ends the wait even when the controller has not polled.
        # The issuing store itself never supplies an "other producer" overlap.
        for edge, bin_index, actor in accepted:
            rows["stream_sta"][bin_index] += any(
                owner != actor and start < edge < end for owner, start, end in intervals)
        rows["stream_sta"][10] = max((sum(start <= edge < end for _, start, end in intervals)
                                      for _, edge, _ in intervals), default=0)
        rows["stream"][0] = 1
        rows["stream"][1] = len(admitted)
        rows["stream"][2] = rows["stream"][11] = len(retired)
        rows["stream"][8] = sum(rows["stream_issue"])
        rows["stream_history"][0] = len(retired) > 32
        rows["stream_history"][1] = len(retired)
        return rows


def mixed_history(misses=(1, 1), *, recovery=True, vector_uops=(2, 3), offset=0):
    """Two slow stores coexist with every independent producer class."""
    instructions = [Instruction("store"), Instruction("store"), Instruction("load"),
                    Instruction("vload", vector_uops[0]), Instruction("vstore", vector_uops[1]),
                    Instruction("prefetch")]
    if recovery:
        instructions.append(Instruction("store", recovery=True))
    events = []
    def emit(cycle, event, age, slot=0):
        events.append((cycle + offset, event, age, slot))
    emit(1, "admit", 0)
    emit(2, "address", 0)
    emit(3, "admit", 1)
    emit(4, "address", 1)
    cycle = 5
    for age in range(2, len(instructions)):
        emit(cycle, "admit", age)
        cycle += 1
        item = instructions[age]
        stages = ([("address", 0), ("data", 0)] if item.kind == "store" else
                  [("issue", slot) for slot in range(item.uops)])
        for event, slot in stages:
            emit(cycle, event, age, slot)
            cycle += 1
    emit(cycle, "data", 0)
    emit(cycle + 1, "data", 1)
    last_feedback = cycle + 4
    for age, count in enumerate(misses):
        feedback = cycle + 4 + 3 * age
        for _ in range(count):
            emit(feedback, "miss", age)
            emit(feedback + 8, "address", age)
            feedback += 11
        emit(feedback, "hit", age)
        last_feedback = max(last_feedback, feedback)
    cycle = last_feedback + 1
    for age in range(len(instructions)):
        emit(cycle, "complete", age)
        emit(cycle + 1, "retire", age)
        cycle += 2
    return History(tuple(instructions), tuple(sorted(events)))


def population(histories):
    observations = [history.counts() for history in histories]
    rows = {name: [max((row[name][index] for row in observations), default=0)
                   if (name, index) in PEAKS else sum(row[name][index] for row in observations)
                   for index in range(width)] for name, width in FIELDS.items()}
    rows["stream"][11] = rows["stream"][1]
    return dict(transactions=max(1, rows["stream"][1]), stream_scalar_retries=rows["stream_sta"][1],
                **{name: csv(values) for name, values in rows.items()})


REPLAY_CPP = r'''
#include REPLAY_HEADER
#include <array>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
struct Sample { unsigned lane; bool sq_flag; unsigned sq_value; std::uint64_t cycle; bool hit; };
struct Transaction { unsigned address_lane = 0; bool sq_flag = false; unsigned sq = 0; };
int main() {
    using Replay = memblock::scenarios::mixed::StoreAddressReplay;
    std::array<Replay, 4> states;
    std::array<Transaction, 4> transactions;
    std::vector<Sample> samples;
    std::string line;
    while (std::getline(std::cin, line)) {
        std::istringstream input(line);
        std::string action;
        input >> action;
        unsigned id = 0;
        if (action == "S") {
            Sample sample;
            input >> sample.lane >> sample.sq_flag >> sample.sq_value >> sample.cycle >> sample.hit;
            samples.push_back(sample);
            std::cout << samples.size() << '\n';
        } else if (action == "C") {
            samples.clear();
            std::cout << "0\n";
        } else {
            input >> id;
            auto &state = states.at(id);
            if (action == "T") {
                auto &transaction = transactions.at(id);
                input >> transaction.address_lane >> transaction.sq_flag >> transaction.sq;
                std::cout << "ok\n";
            } else if (action == "A") {
                std::size_t cursor;
                std::uint64_t cycle;
                input >> cursor >> cycle;
                std::cout << state.accept(cursor, cycle) << '\n';
            } else if (action == "O") {
                auto result = state.observe(samples, transactions.at(id));
                std::cout << (result == Replay::Observation::none ? "none" :
                              result == Replay::Observation::hit ? "hit" :
                              result == Replay::Observation::miss ? "miss" : "invalid") << '\n';
            } else if (action == "Q") {
                std::cout << state.issued() << ' ' << state.waiting() << ' ' << state.translated() << '\n';
            } else if (action == "R") {
                std::uint64_t cycle; input >> cycle;
                std::cout << state.ready(cycle) << '\n';
            } else if (action == "W") {
                std::uint64_t cycle; input >> cycle;
                std::cout << state.waiting_at(cycle, samples, transactions.at(id)) << '\n';
            } else if (action == "H") {
                std::uint64_t cycle; input >> cycle;
                std::cout << state.translated_at(cycle, samples, transactions.at(id)) << '\n';
            } else if (action == "X") {
                std::uint64_t cycle; unsigned timeout; input >> cycle >> timeout;
                std::cout << state.timed_out(cycle, timeout) << '\n';
            } else return 2;
        }
        if (input.fail()) return 3;
    }
}
'''

REPORT_CPP = r'''
#include "scenarios/shared.hpp"
int main() {
    auto constraints = memblock::scenarios::RandomConstraints::preset("coverage");
    constraints.apply("concurrent=1000");
    constraints.apply("concurrent-stream=1000");
    constraints.apply("stream-min-instructions=1");
    constraints.apply("stream-max-instructions=4096");
    constraints.validate();
    std::string line;
    while (std::getline(std::cin, line)) {
        std::istringstream input(line);
        memblock::scenarios::MixedCoverage coverage;
        auto read = [&](auto &values) { for (auto &value : values) input >> value; };
        read(coverage.stream); read(coverage.stream_ops); read(coverage.stream_issue);
        read(coverage.stream_vector_uops); read(coverage.stream_waiting_head);
        read(coverage.stream_recovery); read(coverage.stream_dependencies);
        read(coverage.stream_history); read(coverage.stream_sta);
        coverage.stream_cancel[24] = coverage.stream_history[1];
        coverage.stream_cancel[25] = coverage.stream_history[0];
        std::uint64_t transactions;
        input >> transactions >> coverage.stream_scalar_retries;
        std::string extra;
        if (input.fail() || input >> extra) return 1;
        coverage.stream_store_retirement[0] = coverage.stream_ops[2];
        coverage.stream_store_retirement[1] = coverage.stream_ops[3];
        coverage.stream_store_retirement[2] = coverage.stream_vector_uops[1];
        coverage.stream_store_retirement[4] = coverage.stream_ops[2] + coverage.stream_vector_uops[1];
        coverage.stream_store_retirement[9] = coverage.stream_store_retirement[4];
        std::cout << "MEMBLOCK_CONSTRAINED_RANDOM_PASS transactions=" << transactions
                  << ' ' << constraints.summary() << ' ' << coverage.summary() << '\n';
    }
}
'''


class StoreAddressReplayTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.records, cls.mutations, cls.commands = [], [], []
        cls.hashes = {str(SOURCE_ROOT / path): digest(SOURCE_ROOT / path) for path in (
            HEADER, "cpp/random/constraints.inc", "cpp/random/coverage.inc",
            "scripts/run_regression.py", "scripts/verify_regression.py")}
        cls.revision = subprocess.run(["git", "-C", str(SOURCE_ROOT), "rev-parse", "HEAD"],
                                      capture_output=True, text=True, check=True).stdout.strip()
        artifacts = os.environ.get("MEMBLOCK_STORE_ADDRESS_REPLAY_ARTIFACTS")
        if artifacts:
            Path(artifacts).mkdir(parents=True, exist_ok=True)
            cls.work = Path(tempfile.mkdtemp(prefix="host-", dir=artifacts))
        else:
            temporary = tempfile.TemporaryDirectory(prefix="memblock-store-address-replay-")
            cls.addClassCleanup(temporary.cleanup)
            cls.work = Path(temporary.name)
        cls.addClassCleanup(cls.finalize)
        cls.replay = cls.compile(REPLAY_CPP.replace("REPLAY_HEADER", json.dumps(str(SOURCE_ROOT / HEADER))), "replay")
        cls.reporter = cls.compile(REPORT_CPP, "reporter", dependencies=True)
        sys.path.insert(0, str(SOURCE_ROOT / "scripts"))
        cls.parser = cls.load_module("run_regression", SOURCE_ROOT / "scripts/run_regression.py")
        cls.verifier = cls.load_module("verify_regression", SOURCE_ROOT / "scripts/verify_regression.py")
        cls.verifier.run_regression = cls.parser
        cls.check_inputs()

    @classmethod
    def load_module(cls, name, path):
        spec = importlib.util.spec_from_file_location("store_address_replay_" + name, path)
        module = importlib.util.module_from_spec(spec)
        sys.modules[spec.name] = module
        spec.loader.exec_module(module)
        return module

    @classmethod
    def compile(cls, source, name, *, dependencies=False):
        path = cls.work / (name + ".cpp")
        path.write_text(source)
        binary = path.with_suffix("")
        command = ["c++", "-std=c++20", "-O0", "-I", str(SOURCE_ROOT / "cpp")]
        if dependencies:
            command += ["-MMD", "-MF", str(path.with_suffix(".d"))]
        command += [str(path), "-o", str(binary)]
        ran = subprocess.run(command, capture_output=True, text=True, timeout=60)
        path.with_suffix(".compile.log").write_text(ran.stdout + ran.stderr)
        cls.commands.append(dict(command=command, returncode=ran.returncode))
        if ran.returncode:
            raise AssertionError("host helper compilation failed:\n" + ran.stdout + ran.stderr)
        if dependencies:
            for entry in shlex.split(path.with_suffix(".d").read_text().replace("\\\n", " ").split(":", 1)[1]):
                dependency = Path(entry).resolve()
                if dependency != path:
                    cls.hashes.setdefault(str(dependency), digest(dependency))
        return binary

    @classmethod
    def check_inputs(cls):
        if any(digest(path) != expected for path, expected in cls.hashes.items()):
            raise AssertionError("production inputs changed; refresh the independent test snapshot")

    @classmethod
    def finalize(cls):
        after = {path: digest(path) for path in cls.hashes}
        destination = os.environ.get("MEMBLOCK_STORE_ADDRESS_REPLAY_REPORT")
        if destination:
            Path(destination).parent.mkdir(parents=True, exist_ok=True)
            Path(destination).write_text(json.dumps(dict(revision=cls.revision, source_root=str(SOURCE_ROOT),
                scope="host replay state and synthetic accounting ledgers; no DUT or CPU legality evidence",
                hashes_before=cls.hashes, hashes_after=after, commands=cls.commands,
                cases=cls.records, private_mutations=cls.mutations), indent=2) + "\n")
        cls.check_inputs()

    def run_commands(self, commands, *, binary=None):
        self.check_inputs()
        ran = subprocess.run([str(binary or self.replay)], input="\n".join(commands) + "\n",
                             capture_output=True, text=True, timeout=10)
        self.assertEqual(ran.returncode, 0, ran.stdout + ran.stderr)
        return ran.stdout.splitlines()

    def check_commands(self, cases, *, binary=None):
        commands, expected = zip(*cases)
        actual = self.run_commands(commands, binary=binary)
        self.records.append(dict(test=self.id(), commands=commands, expected=expected, actual=actual))
        self.assertEqual(actual, list(expected))

    def reports(self, expected):
        text = "\n".join(" ".join(str(value) for field in FIELDS for value in map(int, row[field].split(","))) +
                         f" {row['transactions']} {row['stream_scalar_retries']}" for row in expected) + "\n"
        ran = subprocess.run([str(self.reporter)], input=text, capture_output=True, text=True, timeout=30)
        self.assertEqual(ran.returncode, 0, ran.stdout + ran.stderr)
        lines = ran.stdout.splitlines()
        self.assertEqual(len(lines), len(expected))
        reports = []
        for original, line in zip(expected, lines):
            report = self.parser.parse_summary(line)
            self.assertEqual(report["constraint_schema"], 85)
            # This independent event ledger describes synchronous producers.
            # Preserve its schema76 compatibility contract; persistent-session
            # offers and edge masks have their own independent ledger tests.
            line = re.sub(r"\bconstraint_schema=\d+\b", "constraint_schema=76", line)
            report = self.parser.parse_summary(line)
            for field, value in original.items():
                self.assertEqual(report[field], value, field)
            reports.append(report)
        self.records.append(dict(test=self.id(), serialized_rows=len(reports), ledgers=expected))
        return reports

    def accounting(self, report, *, accepted=True, reason=None, verifier=None):
        checker = verifier or self.verifier
        error = None
        try:
            checker._check_stream_accounting(report)
        except checker.VerificationError as caught:
            error = str(caught)
        self.records.append(dict(test=self.id(), accepted=accepted, error=error,
                                 stream_sta=report.get("stream_sta")))
        self.assertEqual(error is None, accepted, error or "corrupt evidence was accepted")
        if reason:
            self.assertIn(reason, error or "")

    @staticmethod
    def replace(report, field, index, value):
        values = report[field].split(",")
        values[index] = str(value)
        return dict(report, **{field: ",".join(values)})

    def baseline(self, **options):
        report = self.reports([population([mixed_history(**options)])])[0]
        self.accounting(report)
        return report

    def test_initial_state_and_only_one_outstanding_attempt(self):
        self.check_commands([
            ("T 0 1 1 47", "ok"), ("Q 0", "0 0 0"), ("O 0", "none"),
            ("R 0 0", "1"), ("A 0 0 10", "1"), ("Q 0", "1 1 0"),
            ("A 0 0 11", "0"), ("R 0 999", "0"), ("O 0", "none"),
        ])

    def test_feedback_matches_cursor_lane_sq_value_and_wrap(self):
        self.check_commands([
            ("T 0 1 1 47", "ok"), ("S 1 1 47 5 1", "1"), ("A 0 1 10", "1"),
            ("O 0", "none"), ("S 0 1 47 11 1", "2"), ("S 1 1 46 12 1", "3"),
            ("S 1 0 47 13 1", "4"), ("O 0", "none"), ("Q 0", "1 1 0"),
            ("S 1 1 47 30 1", "5"), ("O 0", "hit"), ("O 0", "none"),
            ("Q 0", "1 0 1"),
        ])

    def test_delayed_feedback_is_independent_for_two_stores(self):
        self.check_commands([
            ("T 0 0 0 47", "ok"), ("T 1 0 1 47", "ok"),
            ("A 0 0 10", "1"), ("A 1 0 12", "1"),
            ("S 0 1 47 18 1", "1"), ("O 1", "hit"), ("O 0", "none"),
            ("Q 0", "1 1 0"), ("Q 1", "1 0 1"),
            ("S 1 0 12 25 1", "2"), ("O 0", "none"),
            ("S 0 0 47 80 1", "3"), ("O 0", "hit"),
        ])

    def test_miss_retry_cooldown_and_unrelated_traffic(self):
        self.check_commands([
            ("T 0 1 0 3", "ok"), ("T 1 0 1 3", "ok"), ("A 0 0 10", "1"),
            ("S 1 0 3 30 0", "1"), ("O 0", "miss"), ("Q 0", "1 0 0"),
            ("R 0 30", "0"), ("R 0 37", "0"), ("A 0 1 37", "0"),
            ("A 1 1 32", "1"), ("S 0 1 3 35 1", "2"), ("O 1", "hit"),
            ("R 0 38", "1"), ("A 0 2 38", "1"), ("O 0", "none"),
            ("S 1 0 3 42 1", "3"), ("O 0", "hit"), ("R 0 1000", "0"),
        ])

    def test_hit_never_reissues_and_late_poll_does_not_extend_cooldown(self):
        self.check_commands([
            ("T 0 0 0 0", "ok"), ("A 0 0 5", "1"), ("S 0 0 0 8 1", "1"),
            ("O 0", "hit"), ("R 0 16", "0"), ("R 0 100000", "0"), ("A 0 1 100000", "0"),
            ("T 1 1 0 0", "ok"), ("A 1 1 10", "1"), ("S 1 0 0 11 0", "2"),
            ("O 1", "miss"), ("R 1 18", "0"), ("R 1 19", "1"), ("R 1 1000", "1"),
        ])

    def test_timeout_boundary_and_new_attempt_restarts_deadline(self):
        self.check_commands([
            ("T 0 0 0 0", "ok"), ("X 0 100 16", "0"), ("A 0 0 100", "1"),
            ("X 0 99 16", "0"), ("X 0 115 16", "0"), ("X 0 116 16", "1"),
            ("S 0 0 0 110 0", "1"), ("O 0", "miss"), ("X 0 500 16", "0"),
            ("A 0 1 118", "1"), ("X 0 133 16", "0"), ("X 0 134 16", "1"),
            ("S 0 0 0 130 1", "2"), ("O 0", "hit"), ("X 0 500 16", "0"),
        ])

    def test_duplicate_preaccept_feedback_and_reset_sample_cursor_are_invalid(self):
        histories = [
            [("T 0 0 0 1", "ok"), ("A 0 0 10", "1"),
             ("S 0 0 1 11 1", "1"), ("S 0 0 1 12 1", "2"), ("O 0", "invalid")],
            [("T 0 0 0 1", "ok"), ("A 0 0 10", "1"),
             ("S 0 0 1 9 1", "1"), ("O 0", "invalid")],
            [("T 0 0 0 1", "ok"), ("S 1 0 1 1 1", "1"),
             ("A 0 1 10", "1"), ("C", "0"), ("O 0", "invalid")],
            [("T 0 0 0 1", "ok"), ("A 0 0 10", "1"), ("S 0 0 1 11 0", "1"),
             ("O 0", "miss"), ("S 0 0 1 12 0", "2"), ("O 0", "invalid")],
            [("T 0 0 0 1", "ok"), ("A 0 0 10", "1"), ("S 0 0 1 11 1", "1"),
             ("O 0", "hit"), ("S 0 0 1 12 1", "2"), ("O 0", "invalid")],
        ]
        for index, cases in enumerate(histories):
            with self.subTest(history=index):
                self.check_commands(cases)

    def test_waiting_at_uses_acceptance_and_feedback_edges(self):
        self.check_commands([
            ("T 0 1 1 47", "ok"), ("W 0 9", "0"), ("A 0 0 10", "1"),
            ("W 0 9", "0"), ("W 0 10", "0"), ("W 0 11", "1"),
            ("S 1 0 47 12 1", "1"), ("W 0 13", "1"),
            ("S 1 1 47 20 1", "2"), ("W 0 19", "1"), ("W 0 20", "0"),
            ("W 0 21", "0"), ("O 0", "hit"), ("W 0 19", "0"),
        ])

    def test_translated_at_tracks_sample_boundary_and_accepted_lifetime(self):
        self.check_commands([
            ("T 0 1 1 47", "ok"), ("S 1 1 47 4 1", "1"),
            ("H 0 20", "0"), ("A 0 1 10", "1"), ("H 0 20", "0"),
            ("S 0 1 47 11 1", "2"), ("H 0 20", "0"),
            ("S 1 1 47 11 0", "3"), ("H 0 20", "0"), ("O 0", "miss"),
            ("A 0 3 19", "1"), ("S 1 1 47 21 1", "4"),
            ("H 0 20", "0"), ("H 0 21", "1"), ("O 0", "hit"),
            ("H 0 20", "0"), ("H 0 21", "1"), ("H 0 22", "1"),
        ])

    def test_private_replay_mutations_are_detected(self):
        original = (SOURCE_ROOT / HEADER).read_text()
        identity = ["T 0 1 1 47", "A 0 0 10"]
        cases = [
            ("wrong_lane", "sample.lane == transaction.address_lane &&", "",
             identity + ["S 0 1 47 11 1", "O 0"], ["ok", "1", "1", "none"]),
            ("wrong_wrap", "sample.sq_flag == transaction.sq_flag &&", "",
             identity + ["S 1 0 47 11 1", "O 0"], ["ok", "1", "1", "none"]),
            ("wrong_sq", "sample.sq_value == transaction.sq", "true",
             identity + ["S 1 1 46 11 1", "O 0"], ["ok", "1", "1", "none"]),
            ("stale_cursor", "cursor_ = first_sample;", "cursor_ = 0;",
             ["T 0 1 1 47", "S 1 1 47 5 1", "A 0 1 10", "O 0"], ["ok", "1", "1", "none"]),
            ("retry_after_hit", "!waiting_ && !translated_", "!waiting_",
             identity + ["S 1 1 47 11 1", "O 0", "R 0 100"], ["ok", "1", "1", "hit", "0"]),
            ("retry_early", "sample.cycle + 8U", "sample.cycle + 7U",
             identity + ["S 1 1 47 11 0", "O 0", "R 0 18"], ["ok", "1", "1", "miss", "0"]),
            ("retry_late", "sample.cycle + 8U", "sample.cycle + 9U",
             identity + ["S 1 1 47 11 0", "O 0", "R 0 19"], ["ok", "1", "1", "miss", "1"]),
            ("duplicate_accept", "if (!ready(cycle)) return false;", "if (translated_) return false;",
             identity + ["A 0 0 11"], ["ok", "1", "0"]),
            ("accept_edge_overlap", "edge <= accepted_cycle_", "edge < accepted_cycle_",
             identity + ["W 0 10"], ["ok", "1", "0"]),
            ("feedback_edge_overlap", "samples[index].cycle <= edge", "samples[index].cycle < edge",
             identity + ["S 1 1 47 11 1", "W 0 11"], ["ok", "1", "1", "0"]),
            ("late_timeout", ">= timeout", "> timeout",
             identity + ["X 0 26 16"], ["ok", "1", "1"]),
        ]
        for name, old, new, commands, expected in cases:
            with self.subTest(mutation=name):
                self.assertEqual(original.count(old), 1, old)
                self.assertEqual(self.run_commands(commands), expected)
                header = self.work / (name + ".hpp")
                header.write_text(original.replace(old, new))
                binary = self.compile(REPLAY_CPP.replace("REPLAY_HEADER", json.dumps(str(header))), name)
                actual = self.run_commands(commands, binary=binary)
                self.assertNotEqual(actual, expected, "private replay fault escaped the contract")
                self.mutations.append(dict(kind="replay", name=name, artifact=str(header),
                    commands=commands, expected=expected, observed=actual, detected=True))

    def test_actual_serializer_parser_and_independent_mixed_ledgers(self):
        chooser = random.Random(0x73A)
        histories = [mixed_history(misses=(0, 0), recovery=False), mixed_history()]
        histories += [mixed_history(misses=(chooser.randrange(4), chooser.randrange(4)),
                                    recovery=bool(chooser.randrange(2)),
                                    vector_uops=(chooser.randrange(1, 9), chooser.randrange(1, 9)),
                                    offset=chooser.randrange(1000)) for _ in range(32)]
        expected = [population([history]) for history in histories]
        expected.append(population(histories))
        for report in self.reports(expected):
            self.accounting(report)
            sta = list(map(int, report["stream_sta"].split(",")))
            if sta[6]:
                self.assertGreaterEqual(sta[10], 2, "other STA acceptance requires two simultaneous waits")
        first = histories[1].counts()
        self.assertEqual(first["stream_sta"][:4], [2, 2, 2, 2])
        self.assertGreater(first["stream_sta"][10], 1)
        self.assertTrue(all(first["stream_sta"][index] for index in range(4, 10)))
        self.assertEqual(first["stream_ops"][2], 3)
        self.assertEqual(first["stream_recovery"][9], 1)

    def test_independent_ledger_distinguishes_serial_and_overlapping_feedback_waits(self):
        serial = History((Instruction("store"), Instruction("store")), (
            (1, "admit", 0, 0), (2, "address", 0, 0), (3, "hit", 0, 0), (4, "data", 0, 0),
            (5, "admit", 1, 0), (6, "address", 1, 0), (7, "hit", 1, 0), (8, "data", 1, 0),
            (9, "complete", 0, 0), (10, "retire", 0, 0),
            (11, "complete", 1, 0), (12, "retire", 1, 0)))
        reports = self.reports([population([serial]), population([mixed_history(misses=(0, 0))])])
        for report in reports:
            self.accounting(report)
        serial_sta, overlap_sta = (list(map(int, report["stream_sta"].split(","))) for report in reports)
        self.assertEqual((serial_sta[6], serial_sta[10]), (0, 1))
        self.assertGreater(overlap_sta[6], 0)
        self.assertEqual(overlap_sta[10], 2)
        self.accounting(self.replace(reports[0], "stream_sta", 10, 0), accepted=False,
                        reason="eligible stores")
        self.accounting(self.replace(reports[1], "stream_sta", 10, 1), accepted=False,
                        reason="accepted overlap")

    def test_schema72_compatibility_omits_new_field(self):
        current = self.baseline()
        line = current["summary"].replace("constraint_schema=76", "constraint_schema=72")
        line = " ".join(word for word in line.split() if not word.startswith("stream_sta="))
        legacy = self.parser.parse_summary(line)
        self.assertNotIn("stream_sta", legacy)
        self.accounting(legacy)
        self.accounting(dict(legacy, stream_sta="old schema ignores the new field"))

    def test_lost_duplicate_feedback_and_attempt_counts_are_rejected(self):
        report = self.baseline()
        for index in range(4):
            original = int(report["stream_sta"].split(",")[index])
            for delta in (-1, 1):
                with self.subTest(bin=index, delta=delta):
                    self.accounting(self.replace(report, "stream_sta", index, original + delta),
                                    accepted=False, reason="not conserved")
        for delta in (-1, 1):
            self.accounting(dict(report, stream_scalar_retries=report["stream_scalar_retries"] + delta),
                            accepted=False, reason="not conserved")
        # Removing the graph-store attribution may not turn its synchronous STA
        # into an ordinary replay attempt without adding matching feedback.
        self.accounting(self.replace(report, "stream_recovery", 9, 0), accepted=False, reason="not conserved")

    def test_overlap_counts_cannot_invent_accepted_producers_or_waiters(self):
        report = self.baseline()
        ledger = mixed_history().counts()
        actual_producers = {4: len(mixed_history().instructions) - 1,
                            5: ledger["stream_issue"][0],
                            6: ledger["stream_sta"][0] + ledger["stream_sta"][1],
                            7: ledger["stream_issue"][2],
                            8: sum(ledger["stream_vector_uops"]),
                            9: ledger["stream_issue"][5]}
        for index, count in actual_producers.items():
            with self.subTest(bin=index):
                self.accounting(self.replace(report, "stream_sta", index, count + 1),
                                accepted=False, reason="overlaps exceed accepted work")
        self.accounting(self.replace(report, "stream_sta", 10, 3), accepted=False, reason="eligible stores")
        self.accounting(self.replace(report, "stream_sta", 10, 0), accepted=False, reason="eligible stores")

    def test_empty_no_store_and_malformed_feedback_accounting(self):
        empty = self.reports([population([])])[0]
        self.accounting(empty)
        singleton = History((Instruction("load"),), ((1, "admit", 0, 0), (2, "issue", 0, 0),
                                                     (3, "complete", 0, 0), (4, "retire", 0, 0)))
        no_store = self.reports([population([singleton])])[0]
        self.accounting(no_store)
        for report in (empty, no_store):
            for index in range(11):
                self.accounting(self.replace(report, "stream_sta", index, 1), accepted=False)
        report = self.baseline()
        missing = dict(report)
        del missing["stream_sta"]
        self.accounting(missing, accepted=False, reason="stream_sta")
        for bad in (None, True, 0, [], {}, "", "1,2", "0," * 11 + "0", "x," + "0," * 9 + "0"):
            with self.subTest(value=bad):
                self.accounting(dict(report, stream_sta=bad), accepted=False, reason="stream_sta")
        for index in range(11):
            self.accounting(self.replace(report, "stream_sta", index, -1), accepted=False, reason="negative count")

    def test_private_verifier_mutations_cannot_hide_corrupt_evidence(self):
        report = self.baseline()
        original = (SOURCE_ROOT / "scripts/verify_regression.py").read_text()
        cases = [
            ("lost_feedback", "sta[0] == ordinary + canceled[7] and sta[2] == ordinary + canceled[18] and\n                 sta[1] == retries and sta[3] == retries + canceled[19] and\n                 sta[0] + sta[1] == sta[2] + sta[3] + canceled[20] and\n                 (ordinary != 0 or retries == canceled[21])", "True",
             self.replace(report, "stream_sta", 2, 1)),
            ("invented_admission_overlap", "sta[4] <= admitted - epochs", "True",
             self.replace(report, "stream_sta", 4, len(mixed_history().instructions))),
            ("invented_waiter", "1 <= sta[10] <= min(eligible_stores, live)", "True",
             self.replace(report, "stream_sta", 10, 3)),
            ("lost_overlapping_waiter", "sta[6] == 0 or sta[10] >= 2", "True",
             self.replace(report, "stream_sta", 10, 1)),
        ]
        for name, old, new, corrupt in cases:
            with self.subTest(mutation=name):
                self.accounting(corrupt, accepted=False)
                self.assertEqual(original.count(old), 1, old)
                path = self.work / ("verify_" + name + ".py")
                path.write_text(original.replace(old, new))
                mutant = self.load_module(name, path)
                self.accounting(corrupt, verifier=mutant)
                self.mutations.append(dict(kind="verifier", name=name, artifact=str(path),
                    rejected_by_production=True, accepted_by_private_fault=True, detected=True))


if __name__ == "__main__":
    unittest.main()
