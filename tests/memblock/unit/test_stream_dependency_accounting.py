#!/usr/bin/env python3
"""Check schema71 dependency evidence against explicit ordinary-stream histories.

The independent event ledger admits architectural instructions, checks load
completion, consumes each operand, and retires an ordered prefix. It does not
model the DUT scheduler or establish full-CPU reachability. A host helper uses
the production constraints parser and coverage serializer; the real Python
summary parser and artifact checker then accept or reject their observations.

MEMBLOCK_STREAM_DEPENDENCY_ARTIFACTS retains the helper and command logs.
MEMBLOCK_STREAM_DEPENDENCY_REPORT writes case outcomes and input hashes.
"""
from __future__ import annotations

from dataclasses import dataclass
import hashlib
import importlib.util
import itertools
import json
import os
from pathlib import Path
import random
import shlex
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
SOURCE_ROOT = Path(os.environ.get("MEMBLOCK_STREAM_DEPENDENCY_SOURCE_ROOT", ROOT)).resolve()
FIELDS = dict(stream=12, stream_ops=5, stream_issue=6, stream_vector_uops=2,
              stream_waiting_head=2, stream_recovery=20, stream_dependencies=9)
PEAKS = {("stream", 6), ("stream", 7), ("stream_dependencies", 8)}
KINDS = ("integer-load", "vector-load", "scalar-store", "vector-store", "prefetch")


def csv(values):
    return ",".join(map(str, values))


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


@dataclass(frozen=True)
class Instruction:
    kind: str
    address_source: int | None = None
    data_source: int | None = None


@dataclass(frozen=True)
class History:
    instructions: tuple[Instruction, ...]
    events: tuple[tuple[str, int], ...]

    def counts(self):
        """Derive observations from legal admission/operand/response/retire events."""
        assert 1 <= len(self.instructions) <= 32
        rows = {field: [0] * width for field, width in FIELDS.items()}
        live, admitted, completed, retired = [], set(), set(), set()
        issued = {age: set() for age in range(len(self.instructions))}
        depth = {}
        dependency = rows["stream_dependencies"]
        overlap = sliding = prefix = peak_live = peak_pending = 0
        for event, age in self.events:
            instruction = self.instructions[age]
            required = {"address", "data"} if instruction.kind == "scalar-store" else {"address"}
            if live and live[0] in completed and event in ("admit", "address", "data"):
                rows["stream_waiting_head"][int(event != "admit")] += 1
            if event == "admit":
                assert age == len(admitted) and age not in admitted and len(live) < 8
                overlap += any(issued[older] and older not in completed for older in live)
                sliding += bool(retired and live)
                live.append(age)
                admitted.add(age)
                rows["stream_ops"][KINDS.index(instruction.kind)] += 1
                sources = (instruction.address_source, instruction.data_source)
                assert instruction.data_source is None or instruction.kind == "scalar-store"
                depth[age] = 0
                for slot, source in enumerate(sources):
                    if source is None:
                        continue
                    assert source in live and source < age
                    assert self.instructions[source].kind == "integer-load"
                    dependency[slot] += 1
                    dependency[4 + slot] += source not in completed
                    depth[age] = max(depth[age], depth[source] + 1)
                dependency[8] = max(dependency[8], depth[age])
            elif event in ("address", "data"):
                assert age in live and event in required and event not in issued[age]
                source = instruction.address_source if event == "address" else instruction.data_source
                if source is not None:
                    assert source in completed
                    dependency[2 + int(event == "data")] += 1
                    dependency[6 + int(source in retired)] += 1
                issued[age].add(event)
                issue_slot = (0, 3, 1, 4, 5)[KINDS.index(instruction.kind)]
                if event == "data":
                    issue_slot = 2
                rows["stream_issue"][issue_slot] += 1
                if instruction.kind.startswith("vector"):
                    rows["stream_vector_uops"][int(instruction.kind == "vector-store")] += 1
            elif event == "complete":
                assert age in live and age not in completed and issued[age] == required
                completed.add(age)
            elif event == "retire":
                assert live and live[0] == age and age in completed
                prefix += len(live) > 1
                live.pop(0)
                retired.add(age)
            else:
                raise AssertionError(event)
            peak_live = max(peak_live, len(live))
            peak_pending = max(peak_pending, sum(bool(issued[item]) and item not in completed for item in live))
        assert not live and len(retired) == len(self.instructions)
        count = len(retired)
        rows["stream"] = [1, count, count, overlap, sliding, prefix, peak_live,
                          peak_pending, sum(rows["stream_issue"]), 0, 0, count]
        return rows


def pair(kind, *, address=True, data=False, pending=True, after=False):
    instructions = (Instruction("integer-load"), Instruction(kind, 0 if address else None, 0 if data else None))
    events = [("admit", 0), ("address", 0)]
    if not pending:
        events.append(("complete", 0))
    events.append(("admit", 1))
    if pending:
        events.append(("complete", 0))
    if after:
        events.append(("retire", 0))
    events.append(("address", 1))
    if kind == "scalar-store":
        events.append(("data", 1))
    events.append(("complete", 1))
    if not after:
        events.append(("retire", 0))
    events.append(("retire", 1))
    return History(instructions, tuple(events))


def chain(length, *, pending, after):
    assert 1 <= length <= 32
    instructions = tuple(Instruction("integer-load", age - 1 if age else None) for age in range(length))
    events = [("admit", 0), ("address", 0)]
    for age in range(1, length):
        if not pending:
            events.append(("complete", age - 1))
        events.append(("admit", age))
        if pending:
            events.append(("complete", age - 1))
        if after:
            events.append(("retire", age - 1))
        events.append(("address", age))
        if not after:
            events.append(("retire", age - 1))
    events.extend((("complete", length - 1), ("retire", length - 1)))
    return History(instructions, tuple(events))


def split_sources():
    return History((Instruction("integer-load"), Instruction("integer-load"),
                    Instruction("scalar-store", 0, 1)),
                   (("admit", 0), ("address", 0), ("admit", 1), ("address", 1),
                    ("complete", 1), ("admit", 2), ("complete", 0),
                    ("address", 2), ("retire", 0), ("retire", 1),
                    ("data", 2), ("complete", 2), ("retire", 2)))



def fanout(*, after):
    instructions = (Instruction("integer-load"),) + tuple(
        Instruction(kind, 0, 0 if kind == "scalar-store" else None) for kind in KINDS)
    events = [("admit", 0), ("address", 0)]
    events.extend(("admit", age) for age in range(1, len(instructions)))
    events.append(("complete", 0))
    if after:
        events.append(("retire", 0))
    for age, instruction in enumerate(instructions[1:], 1):
        # Exercise independent STD-before-STA consumption as well.
        if instruction.kind == "scalar-store":
            events.append(("data", age))
        events.extend((("address", age), ("complete", age)))
    if not after:
        events.append(("retire", 0))
    events.extend(("retire", age) for age in range(1, len(instructions)))
    return History(instructions, tuple(events))


def population(histories, *, copies=1, rate=250, schema=71):
    assert copies >= 1
    observations = [history.counts() for history in histories]
    rows = {field: [max((row[field][index] for row in observations), default=0)
                    if (field, index) in PEAKS else
                    copies * sum(row[field][index] for row in observations)
                    for index in range(width)] for field, width in FIELDS.items()}
    rows["stream"][11] = rows["stream"][1]
    return dict(constraint_schema=schema, transactions=max(1, rows["stream"][1]),
                target_stream_dependency=rate, stream_scalar_retries=0,
                **{field: csv(values) for field, values in rows.items()})


CPP = r'''
#include "scenarios/shared.hpp"
int main(int argc, char **argv) {
    try {
        auto constraints = memblock::scenarios::RandomConstraints::preset("coverage");
        constraints.apply("concurrent=1000");
        constraints.apply("concurrent-stream=1000");
        for (int index = 2; index < argc; ++index) constraints.apply(argv[index]);
        constraints.validate();
        if (std::string(argv[1]) == "constraints") {
            std::cout << "MEMBLOCK_CONSTRAINED_RANDOM_PASS " << constraints.summary() << '\n';
            return 0;
        }
        std::string line;
        while (std::getline(std::cin, line)) {
            std::istringstream input(line);
            memblock::scenarios::MixedCoverage coverage;
            auto read = [&](auto &values) { for (auto &value : values) input >> value; };
            read(coverage.stream); read(coverage.stream_ops); read(coverage.stream_issue);
            read(coverage.stream_vector_uops); read(coverage.stream_waiting_head);
            read(coverage.stream_recovery); read(coverage.stream_dependencies);
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
        return 0;
    } catch (const std::exception &error) {
        std::cout << "rejected " << error.what() << '\n';
        return 2;
    }
}
'''


class StreamDependencyAccountingTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.records = []
        cls.revision = subprocess.run(["git", "-C", str(SOURCE_ROOT), "rev-parse", "HEAD"],
                                      capture_output=True, text=True, check=True).stdout.strip()
        cls.hashes = {str(SOURCE_ROOT / path): digest(SOURCE_ROOT / path) for path in
                      ("cpp/random/constraints.inc", "cpp/random/coverage.inc",
                       "scripts/run_regression.py", "scripts/verify_regression.py", "docs/ORACLES.md")}
        artifacts = os.environ.get("MEMBLOCK_STREAM_DEPENDENCY_ARTIFACTS")
        if artifacts:
            Path(artifacts).mkdir(parents=True, exist_ok=True)
            cls.work = Path(tempfile.mkdtemp(prefix="host-", dir=artifacts))
        else:
            cls.temporary = tempfile.TemporaryDirectory(prefix="memblock-stream-dependency-")
            cls.addClassCleanup(cls.temporary.cleanup)
            cls.work = Path(cls.temporary.name)
        source = cls.work / "report.cpp"
        source.write_text(CPP)
        cls.binary = source.with_suffix("")
        dependencies = cls.work / "report.d"
        cls.compile_command = ["c++", "-std=c++20", "-O0", "-MMD", "-MF", str(dependencies),
                               "-I", str(SOURCE_ROOT / "cpp"), str(source), "-o", str(cls.binary)]
        built = subprocess.run(cls.compile_command, capture_output=True, text=True, timeout=60)
        (cls.work / "compile.log").write_text(built.stdout + built.stderr)
        if built.returncode:
            raise AssertionError("production host reporter did not compile:\n" + built.stdout + built.stderr)
        for name in shlex.split(dependencies.read_text().replace("\\\n", " ").split(":", 1)[1]):
            path = Path(name).resolve()
            if path != source:
                cls.hashes.setdefault(str(path), digest(path))
        cls.addClassCleanup(cls.finalize)
        sys.path.insert(0, str(SOURCE_ROOT / "scripts"))
        cls.modules = {}
        for name in ("run_regression", "verify_regression"):
            spec = importlib.util.spec_from_file_location("stream_dependency_" + name, SOURCE_ROOT / "scripts" / (name + ".py"))
            module = importlib.util.module_from_spec(spec)
            sys.modules[spec.name] = module
            spec.loader.exec_module(module)
            cls.modules[name] = module
        cls.verifier = cls.modules["verify_regression"]
        # The verifier imports the canonical runner while loading. Keep this
        # suite on its private production parser even when other test modules
        # patch the canonical runner during full discovery.
        cls.verifier.run_regression = cls.modules["run_regression"]

    @classmethod
    def finalize(cls):
        after = {path: digest(path) for path in cls.hashes}
        destination = os.environ.get("MEMBLOCK_STREAM_DEPENDENCY_REPORT")
        if destination:
            Path(destination).write_text(json.dumps(dict(source_root=str(SOURCE_ROOT),
                revision=cls.revision, compile_command=cls.compile_command, helper_source=str(cls.work / "report.cpp"),
                hashes_before=cls.hashes, hashes_after=after, cases=cls.records), indent=2) + "\n")
        if cls.hashes != after:
            raise AssertionError("production inputs changed during dependency accounting tests; refresh the snapshot")

    def reports(self, expected):
        # All rows in a batch share the actual production policy knob.
        rate = expected[0]["target_stream_dependency"]
        self.assertTrue(all(row["target_stream_dependency"] == rate for row in expected))
        text = "\n".join(" ".join(str(value) for field in FIELDS
                                  for value in map(int, row[field].split(","))) +
                         f" {row['transactions']} {row['stream_scalar_retries']}" for row in expected) + "\n"
        command = [str(self.binary), "report", f"stream-dependency={rate}"]
        ran = subprocess.run(command, input=text, capture_output=True, text=True, timeout=30)
        self.assertEqual(ran.returncode, 0, ran.stdout[:1000] + ran.stderr)
        actual = [self.modules["run_regression"].parse_summary(line) for line in ran.stdout.splitlines()]
        self.assertEqual(len(actual), len(expected))
        for original, report in zip(expected, actual):
            for field in FIELDS:
                self.assertEqual(report[field], original[field], field)
            self.assertEqual(report["target_stream_dependency"], rate)
            self.assertEqual(report["constraint_schema"], 82)
            self.assertEqual(report["transactions"], original["transactions"])
            self.assertEqual(report["stream_scalar_retries"], original["stream_scalar_retries"])
            report["constraint_schema"] = original["constraint_schema"]
        self.records.append(dict(test=self.id(), command=command, serialized_rows=len(expected), returncode=ran.returncode))
        return actual

    def accounting(self, record, accepted=True, reason=None):
        error = None
        try:
            self.verifier._check_stream_accounting(record)
        except self.verifier.VerificationError as caught:
            error = str(caught)
        observed = {field: record[field] for field in
                    (*FIELDS, "constraint_schema", "transactions", "target_stream_dependency",
                     "target_concurrent", "target_concurrent_stream", "target_ops", "target_scalar_fp") if field in record}
        self.records.append(dict(test=self.id(), record=observed, accepted=accepted, error=error))
        self.assertEqual(error is None, accepted, error or "corrupt evidence was accepted")
        if reason:
            self.assertIn(reason, error or "")

    def checked(self, histories, **options):
        report = self.reports([population(histories, **options)])[0]
        self.accounting(report)
        return report

    @staticmethod
    def replace(report, index, value):
        values = report["stream_dependencies"].split(",")
        values[index] = str(value)
        return dict(report, stream_dependencies=",".join(values))

    def test_actual_constraint_parser_defaults_endpoints_and_rejections(self):
        for assignment, expected in ((None, 250), ("stream-dependency=0", 0),
                                     ("stream-dependency=1", 1), ("stream-dependency=1000", 1000),
                                     ("stream-dependency=1001", None), ("stream-dependency=-1", None),
                                     ("stream-dependency=true", None), ("stream-dependency=1.5", None)):
            command = [str(self.binary), "constraints"] + ([assignment] if assignment else [])
            ran = subprocess.run(command, capture_output=True, text=True, timeout=10)
            self.records.append(dict(test=self.id(), command=command, returncode=ran.returncode, output=ran.stdout))
            with self.subTest(assignment=assignment):
                self.assertEqual(ran.returncode, 0 if expected is not None else 2, ran.stdout + ran.stderr)
                if expected is not None:
                    report = self.modules["run_regression"].parse_summary(ran.stdout)
                    self.assertEqual(report["target_stream_dependency"], expected)
                    self.assertEqual(report["constraint_schema"], 82)

    def test_all_consumer_classes_pending_ready_and_source_lifetimes(self):
        histories = [pair(kind, pending=pending, after=after)
                     for kind, pending, after in itertools.product(KINDS, (False, True), (False, True))]
        histories += [pair("scalar-store", address=address, data=True, pending=pending, after=after)
                      for address, pending, after in itertools.product((False, True), repeat=3)]
        histories += [split_sources(), fanout(after=False), fanout(after=True)]
        for index, report in enumerate(self.reports([population([history]) for history in histories])):
            with self.subTest(history=index):
                self.accounting(report)
        # Distinct sources, completion out of age order, address before source
        # retirement and store data after its own source retirement.
        self.assertEqual(split_sources().counts()["stream_dependencies"], [1, 1, 1, 1, 1, 0, 1, 1, 1])
        self.assertEqual(fanout(after=False).counts()["stream_dependencies"], [5, 1, 5, 1, 5, 1, 6, 0, 1])
        self.assertEqual(fanout(after=True).counts()["stream_dependencies"], [5, 1, 5, 1, 5, 1, 0, 6, 1])

    def test_every_chain_depth_and_aggregate_peak_survives_serialization(self):
        histories = [chain(length, pending=pending, after=after)
                     for length, pending, after in itertools.product(range(1, 33), (False, True), (False, True))]
        expected = [population([history]) for history in histories]
        for report in self.reports(expected):
            self.accounting(report)
        self.assertEqual({int(row["stream_dependencies"].split(",")[8]) for row in expected}, set(range(32)))
        report = self.checked([chain(32, pending=True, after=True), split_sources()], copies=997)
        self.assertEqual(report["stream_dependencies"].split(",")[8], "31")
        self.assertEqual(report["stream"].split(",")[6:8], ["3", "2"])

    def test_seeded_aggregate_histories_and_sparse_rates(self):
        chooser = random.Random(0x71D3)
        templates = [pair(kind, pending=pending, after=after)
                     for kind, pending, after in itertools.product(KINDS, (False, True), (False, True))]
        templates += [split_sources(), pair("scalar-store", data=True), chain(32, pending=True, after=True)]
        expected = [population([chooser.choice(templates) for _ in range(chooser.randrange(1, 17))],
                               copies=chooser.randrange(1, 1001)) for _ in range(96)]
        for report in self.reports(expected):
            self.accounting(report)
        for rate in (1, 1000):
            self.checked([split_sources()], rate=rate)

    def test_empty_singletons_and_disabled_dependencies_allow_no_edges(self):
        for rate in (0, 1, 250, 1000):
            self.checked([], rate=rate)
            self.checked([chain(1, pending=True, after=False)], rate=rate)
            self.checked([pair("scalar-store", address=False)], rate=rate)
        enabled = self.checked([split_sources()])
        self.accounting(dict(enabled, target_stream_dependency=0), False, "disabled stream dependencies")

    def test_schema70_ignores_dependency_fields_and_does_not_require_new_policy(self):
        report = self.checked([split_sources()], schema=70)
        del report["stream_dependencies"]
        del report["target_stream_dependency"]
        self.accounting(report)
        self.accounting(dict(report, stream_dependencies="legacy ignored", target_stream_dependency=[]))

    def test_edges_require_ordinary_integer_load_producers_and_depth_capacity(self):
        # These legal streams contain only classes that cannot supply an
        # ordinary integer-load result. Add otherwise-conserved edge evidence
        # to show that issue/conservation checks alone cannot establish a source.
        for kind in KINDS[1:]:
            instructions = (Instruction(kind), Instruction("scalar-store"))
            events = [("admit", 0), ("address", 0)]
            if kind == "scalar-store":
                events.append(("data", 0))
            events.extend((("admit", 1), ("address", 1), ("data", 1),
                           ("complete", 0), ("complete", 1), ("retire", 0), ("retire", 1)))
            report = self.checked([History(instructions, tuple(events))])
            for counts in ("1,0,1,0,0,0,1,0,1", "0,1,0,1,0,0,1,0,1", "1,1,1,1,0,0,2,0,1"):
                with self.subTest(non_source=kind, counts=counts):
                    self.accounting(dict(report, stream_dependencies=counts), False)
        # A fanout has six edges but only two scalar loads. Claiming a chain
        # depth of three keeps old edge/depth conservation while inventing an
        # additional producer result.
        report = self.checked([fanout(after=False)])
        self.accounting(self.replace(report, 8, 3), False, "scalar load producers")
        self.accounting(dict(report, target_scalar_fp=1000), False)
        # The forced-FP policy permits traffic that does not claim a dependency.
        vector = History((Instruction("vector-load"),),
                         (("admit", 0), ("address", 0), ("complete", 0), ("retire", 0)))
        self.accounting(dict(self.checked([vector]), target_scalar_fp=1000))

    def test_missing_malformed_negative_counters_and_invalid_policy_are_rejected(self):
        base = self.checked([split_sources()])
        for field in ("stream_dependencies", "target_stream_dependency"):
            missing = dict(base)
            del missing[field]
            self.accounting(missing, False, field)
        for bad in (None, True, 0, [], {}, "", "true", "1.0", csv([0] * 8), csv([0] * 10),
                    "0,0,0,0,0,0,0,0,False", "0,0,0,0,0,0,0,0,1.0"):
            with self.subTest(array=bad):
                self.accounting(dict(base, stream_dependencies=bad), False, "stream_dependencies")
        for index in range(9):
            with self.subTest(negative_index=index):
                self.accounting(self.replace(base, index, -1), False, "negative count")
        for bad in (None, True, False, "250", -1, 1001, [], 250.0):
            with self.subTest(rate=bad):
                self.accounting(dict(base, target_stream_dependency=bad), False, "target_stream_dependency")

    def test_lost_duplicate_pending_lifetime_depth_and_eligible_capacity_corruptions(self):
        base = self.checked([split_sources()])
        for index, value, reason in ((0, 0, "exactly once"), (0, 2, "exactly once"),
            (1, 0, "exactly once"), (1, 2, "exactly once"),
            (2, 0, "exactly once"), (2, 2, "exactly once"),
            (3, 0, "exactly once"), (3, 2, "exactly once"),
            (4, 2, "eligible instructions"), (5, 2, "eligible instructions"),
            (6, 0, "source lifetimes"), (6, 2, "source lifetimes"),
            (7, 0, "source lifetimes"), (7, 2, "source lifetimes"),
            (8, 0, "chain depth"), (8, 3, "depth"), (8, 32, "depth")):
            with self.subTest(index=index, value=value):
                self.accounting(self.replace(base, index, value), False, reason)
        no_edges = self.checked([chain(1, pending=True, after=False)])
        self.accounting(self.replace(no_edges, 8, 1), False, "chain depth")
        # Keep consumption and lifetime conservation while exceeding each
        # eligibility limit, so the intended capacity assertion must reject it.
        self.accounting(dict(base, stream_dependencies="3,1,3,1,0,0,4,0,1"), False, "eligible instructions")
        self.accounting(dict(base, stream_dependencies="1,2,1,2,0,0,3,0,1"), False, "eligible instructions")


if __name__ == "__main__":
    unittest.main()
