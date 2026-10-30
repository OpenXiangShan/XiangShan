#!/usr/bin/env python3
"""Independently exercise schema72 stream history accounting and old schema71.

An architectural event ledger derives counts from legal admission, initial issue,
checked completion, operand consumption, and prefix retirement. It deliberately
contains no copy of the aggregate checker's inequalities. A small host reporter
uses the real constraints parser and MixedCoverage serializer; its output passes
through the real regression summary parser before the artifact checker runs.
These are host accounting tests, not DUT scheduling or full-CPU reachability
proofs. MEMBLOCK_STREAM_HISTORY_ARTIFACTS retains private helper artifacts and
MEMBLOCK_STREAM_HISTORY_REPORT writes case outcomes and dependency hashes.
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
import shlex
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
SOURCE_ROOT = Path(os.environ.get("MEMBLOCK_STREAM_HISTORY_SOURCE_ROOT", ROOT)).resolve()
FIELDS = dict(stream=12, stream_ops=5, stream_issue=6, stream_vector_uops=2,
              stream_waiting_head=2, stream_recovery=20, stream_dependencies=9,
              stream_history=4)
PEAKS = {("stream", 6), ("stream", 7), ("stream_dependencies", 8),
         ("stream_history", 1)}
KINDS = ("integer-load", "vector-load", "scalar-store", "vector-store", "prefetch")
# Public ROB identity is value plus wrap flag; reusing only the value is not reuse.
ROB_ENTRIES = 160


def csv(values):
    return ",".join(map(str, values))


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


@dataclass(frozen=True)
class Instruction:
    kind: str = "integer-load"
    uops: int = 1
    address_source: int | None = None
    data_source: int | None = None


@dataclass(frozen=True)
class Epoch:
    instructions: tuple[Instruction, ...]
    events: tuple[tuple[str, int, int], ...]
    rob_start: int = 0

    def counts(self):
        """Observe a legal event history without aggregate acceptance formulas."""
        assert 1 <= len(self.instructions) <= 4096
        rows = {field: [0] * width for field, width in FIELDS.items()}
        live, admitted, completed, retired = deque(), set(), set(), set()
        identities, live_identities, issued, depths = set(), set(), {}, {}
        dependencies, history, stream = (rows[key] for key in
                                         ("stream_dependencies", "stream_history", "stream"))
        for event, age, slot in self.events:
            item = self.instructions[age]
            vector = item.kind in ("vector-load", "vector-store")
            assert 1 <= item.uops <= 8 and (vector or item.uops == 1)
            required = ({("address", 0), ("data", 0)} if item.kind == "scalar-store" else
                        {("issue", index) for index in range(item.uops)})
            if live and live[0] in completed and event in ("admit", "issue", "address", "data"):
                rows["stream_waiting_head"][int(event != "admit")] += 1
            if event == "admit":
                assert age == len(admitted) and age not in admitted and len(live) < 8
                identity = ((self.rob_start + age) % ROB_ENTRIES,
                            ((self.rob_start + age) // ROB_ENTRIES) % 2)
                assert identity not in live_identities
                pending = any(issued[older] and older not in completed for older in live)
                stream[3] += pending
                history[2] += len(admitted) >= 32 and pending
                stream[4] += bool(retired and live)
                history[3] += identity in identities
                identities.add(identity)
                live_identities.add(identity)
                live.append(age)
                admitted.add(age)
                issued[age] = set()
                rows["stream_ops"][KINDS.index(item.kind)] += 1
                depths[age] = 0
                assert item.data_source is None or item.kind == "scalar-store"
                for operand, source in enumerate((item.address_source, item.data_source)):
                    if source is None:
                        continue
                    assert source in live and source < age
                    assert self.instructions[source].kind == "integer-load"
                    dependencies[operand] += 1
                    dependencies[4 + operand] += source not in completed
                    depths[age] = max(depths[age], depths[source] + 1)
                dependencies[8] = max(dependencies[8], depths[age])
            elif event in ("issue", "address", "data"):
                assert age in live and (event, slot) in required and (event, slot) not in issued[age]
                source = item.data_source if event == "data" else item.address_source
                if source is not None:
                    assert source in completed
                    dependencies[2 + int(event == "data")] += 1
                    dependencies[6 + int(source in retired)] += 1
                issued[age].add((event, slot))
                index = (0, 3, 1, 4, 5)[KINDS.index(item.kind)]
                if event == "data":
                    index = 2
                rows["stream_issue"][index] += 1
                if vector:
                    rows["stream_vector_uops"][int(item.kind == "vector-store")] += 1
            elif event == "complete":
                assert age in live and age not in completed and issued[age] == required
                completed.add(age)
            elif event == "retire":
                assert live and live[0] == age and age in completed and age not in retired
                stream[5] += len(live) > 1
                live.popleft()
                retired.add(age)
                live_identities.remove(((self.rob_start + age) % ROB_ENTRIES,
                                        ((self.rob_start + age) // ROB_ENTRIES) % 2))
            else:
                raise AssertionError(event)
            stream[6] = max(stream[6], len(live))
            stream[7] = max(stream[7], sum(bool(issued[older]) and older not in completed for older in live))
        assert not live and len(retired) == len(self.instructions)
        stream[0] = 1
        stream[1] = len(admitted)
        stream[2] = stream[11] = len(retired)
        stream[8] = sum(rows["stream_issue"])
        history[0] = len(retired) > 32
        history[1] = len(retired)
        return rows


def window(length, *, depth=4, mixed=False, serial=False, rob_start=0):
    """A bounded sliding window; only the final suffix drains all live work."""
    assert 1 <= length <= 4096 and 2 <= depth <= 8
    instructions = tuple(Instruction(KINDS[age % len(KINDS)] if mixed else "integer-load",
                                     1 + age % 8 if mixed and age % len(KINDS) in (1, 3) else 1)
                         for age in range(length))
    events, live = [], deque()
    def finish():
        oldest = live.popleft()
        events.extend((("complete", oldest, 0), ("retire", oldest, 0)))
    for age, item in enumerate(instructions):
        if len(live) == depth:
            finish()
        events.append(("admit", age, 0))
        live.append(age)
        if item.kind == "scalar-store":
            # Alternate operand order without creating duplicated accepted work.
            events.extend((stage, age, 0) for stage in
                          (("data", "address") if age % 2 else ("address", "data")))
        else:
            events.extend(("issue", age, slot) for slot in range(item.uops))
        if serial:
            finish()
    while live:
        finish()
    return Epoch(instructions, tuple(events), rob_start)


def chain(length):
    """Each checked integer load supplies the next admitted address operand."""
    instructions = tuple(Instruction(address_source=age - 1 if age else None) for age in range(length))
    events = [("admit", 0, 0), ("issue", 0, 0)]
    for age in range(1, length):
        events.extend((("admit", age, 0), ("complete", age - 1, 0),
                       ("retire", age - 1, 0), ("issue", age, 0)))
    events.extend((("complete", length - 1, 0), ("retire", length - 1, 0)))
    return Epoch(instructions, tuple(events))


def population(epochs, *, minimum=8, maximum=256, copies=1, schema=72):
    assert copies >= 1
    observations = [epoch.counts() for epoch in epochs]
    rows = {field: [max((row[field][index] for row in observations), default=0)
                    if (field, index) in PEAKS else
                    copies * sum(row[field][index] for row in observations)
                    for index in range(width)] for field, width in FIELDS.items()}
    rows["stream"][11] = rows["stream"][1]
    return dict(constraint_schema=schema, transactions=max(1, rows["stream"][1]),
                target_stream_min_instructions=minimum, target_stream_max_instructions=maximum,
                stream_scalar_retries=0, **{field: csv(values) for field, values in rows.items()})


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
            read(coverage.stream_recovery); read(coverage.stream_dependencies); read(coverage.stream_history);
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


class StreamHistoryAccountingTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.records = []
        cls.revision = subprocess.run(["git", "-C", str(SOURCE_ROOT), "rev-parse", "HEAD"],
                                      capture_output=True, text=True, check=True).stdout.strip()
        cls.hashes = {str(SOURCE_ROOT / path): digest(SOURCE_ROOT / path) for path in
                      ("cpp/random/constraints.inc", "cpp/random/coverage.inc",
                       "scripts/run_regression.py", "scripts/verify_regression.py")}
        artifacts = os.environ.get("MEMBLOCK_STREAM_HISTORY_ARTIFACTS")
        if artifacts:
            Path(artifacts).mkdir(parents=True, exist_ok=True)
            cls.work = Path(tempfile.mkdtemp(prefix="host-", dir=artifacts))
        else:
            temporary = tempfile.TemporaryDirectory(prefix="memblock-stream-history-")
            cls.addClassCleanup(temporary.cleanup)
            cls.work = Path(temporary.name)
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
            spec = importlib.util.spec_from_file_location("stream_history_" + name,
                                                        SOURCE_ROOT / "scripts" / (name + ".py"))
            module = importlib.util.module_from_spec(spec)
            sys.modules[spec.name] = module
            spec.loader.exec_module(module)
            cls.modules[name] = module
        cls.verifier = cls.modules["verify_regression"]
        cls.verifier.run_regression = cls.modules["run_regression"]

    @classmethod
    def finalize(cls):
        after = {path: digest(path) for path in cls.hashes}
        destination = os.environ.get("MEMBLOCK_STREAM_HISTORY_REPORT")
        if destination:
            Path(destination).write_text(json.dumps(dict(source_root=str(SOURCE_ROOT), revision=cls.revision,
                compile_command=cls.compile_command, helper_source=str(cls.work / "report.cpp"),
                hashes_before=cls.hashes, hashes_after=after, cases=cls.records), indent=2) + "\n")
        if cls.hashes != after:
            raise AssertionError("production inputs changed during history tests; refresh the snapshot")

    def reports(self, expected):
        bounds = [(row["target_stream_min_instructions"], row["target_stream_max_instructions"]) for row in expected]
        self.assertEqual(len(set(bounds)), 1)
        minimum, maximum = bounds[0]
        text = "\n".join(" ".join(str(value) for field in FIELDS for value in map(int, row[field].split(","))) +
                         f" {row['transactions']} {row['stream_scalar_retries']}" for row in expected) + "\n"
        command = [str(self.binary), "report", f"stream-min-instructions={minimum}",
                   f"stream-max-instructions={maximum}"]
        ran = subprocess.run(command, input=text, capture_output=True, text=True, timeout=30)
        self.assertEqual(ran.returncode, 0, ran.stdout[:1000] + ran.stderr)
        actual = [self.modules["run_regression"].parse_summary(line) for line in ran.stdout.splitlines()]
        self.assertEqual(len(actual), len(expected))
        for original, report in zip(expected, actual):
            for field in FIELDS:
                self.assertEqual(report[field], original[field], field)
            for field in ("transactions", "stream_scalar_retries", "target_stream_min_instructions",
                          "target_stream_max_instructions"):
                self.assertEqual(report[field], original[field], field)
            self.assertEqual(report["constraint_schema"], 77)
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
                    (*FIELDS, "constraint_schema", "transactions", "target_stream_min_instructions",
                     "target_stream_max_instructions") if field in record}
        self.records.append(dict(test=self.id(), record=observed, accepted=accepted, error=error))
        self.assertEqual(error is None, accepted, error or "corrupt evidence was accepted")
        if reason:
            self.assertIn(reason, error or "")

    def checked(self, epochs, **options):
        report = self.reports([population(epochs, **options)])[0]
        self.accounting(report)
        return report

    @staticmethod
    def replace(report, field, index, value):
        values = report[field].split(",")
        values[index] = str(value)
        return dict(report, **{field: ",".join(values)})

    def test_actual_parser_defaults_fixed_lengths_endpoints_and_bad_literals(self):
        valid = [([], (8, 256))]
        valid += [([f"stream-min-instructions={length}", f"stream-max-instructions={length}"],
                   (length, length)) for length in (1, 32, 33, 256, 4096)]
        valid += [(["stream-min-instructions=1", "stream-max-instructions=4096"], (1, 4096))]
        invalid = [["stream-min-instructions=0"], ["stream-max-instructions=0"],
                   ["stream-min-instructions=33", "stream-max-instructions=32"],
                   ["stream-min-instructions=4097"], ["stream-max-instructions=4097"]]
        invalid += [[f"{key}={value}"] for key in ("stream-min-instructions", "stream-max-instructions")
                    for value in ("", "true", "1.5", "4096x", "-1", "4294967296", "18446744073709551616")]
        for arguments, expected in valid + [(arguments, None) for arguments in invalid]:
            command = [str(self.binary), "constraints", *arguments]
            ran = subprocess.run(command, capture_output=True, text=True, timeout=10)
            self.records.append(dict(test=self.id(), command=command, returncode=ran.returncode, output=ran.stdout))
            with self.subTest(arguments=arguments):
                self.assertEqual(ran.returncode, 0 if expected else 2, ran.stdout + ran.stderr)
                if expected:
                    report = self.modules["run_regression"].parse_summary(ran.stdout)
                    self.assertEqual(report["constraint_schema"], 77)
                    self.assertEqual((report["target_stream_min_instructions"], report["target_stream_max_instructions"]), expected)

    def test_empty_singleton_clipped_and_default_bounds(self):
        for minimum, maximum in ((8, 256), (1, 1), (32, 32), (33, 33), (256, 256), (4096, 4096)):
            with self.subTest(minimum=minimum, maximum=maximum):
                self.checked([], minimum=minimum, maximum=maximum)
                self.checked([window(1)], minimum=minimum, maximum=maximum)
                if minimum > 1:
                    self.checked([window(minimum - 1, mixed=True)], minimum=minimum, maximum=maximum)
        for rate in (0, 750, 1000):
            report = self.checked([])
            self.accounting(dict(report, target_concurrent_stream=rate))
        self.accounting(dict(self.checked([]), target_concurrent=0))

    def test_boundary_lengths_live_depths_and_mixed_issue_counts(self):
        for maximum in (32, 33, 256, 4096):
            lengths = sorted({1, maximum, max(1, maximum - 1), min(32, maximum)})
            expected = [population([window(length, depth=depth, mixed=True, rob_start=159)],
                                   minimum=maximum, maximum=maximum)
                        for length in lengths for depth in (2, 4, 8)]
            for report in self.reports(expected):
                self.accounting(report)
        for serial in (False, True):
            report = self.checked([window(33, serial=serial)])
            self.assertEqual(report["stream_history"], "1,33,0,0" if serial else "1,33,1,0")
        self.assertEqual(self.checked([window(4096)], maximum=4096)["stream_history"], "1,4096,4064,3776")

    def test_full_rob_identity_reuse_and_epoch_local_history(self):
        for length, reused in ((159, 0), (160, 0), (161, 0), (319, 0), (320, 0), (321, 1), (641, 321)):
            report = self.checked([window(length, rob_start=159)], maximum=4096)
            self.assertEqual(int(report["stream_history"].split(",")[3]), reused)
        report = self.checked([window(320), window(320)], maximum=4096)
        self.assertEqual(report["stream_history"], "2,320,576,0")
        # Maxima remain maxima when many distinct legal epochs are aggregated.
        report = self.checked([window(1), window(32), window(33), window(256), window(321)],
                              maximum=4096, copies=997)
        self.assertEqual(report["stream_history"], csv([3 * 997, 321, (1 + 224 + 289) * 997, 997]))

    def test_mixed_long_short_and_seeded_epoch_populations(self):
        chooser = random.Random(0x7257)
        expected = []
        for _ in range(64):
            lengths = [chooser.choice((1, 2, 7, 8, 31, 32, 33, 64, 127, 255, 256))
                       for _ in range(chooser.randrange(1, 9))]
            expected.append(population([window(length, depth=chooser.choice((2, 4, 8)),
                                                      mixed=True, serial=chooser.choice((False, True)))
                                        for length in lengths], copies=chooser.randrange(1, 101)))
        for report in self.reports(expected):
            self.accounting(report)

    def test_dependency_depth_uses_schema72_maximum_and_schema71_stays_bounded(self):
        for length in (1, 2, 32, 33, 256, 4096):
            report = self.checked([chain(length)], minimum=length, maximum=length)
            self.assertEqual(int(report["stream_dependencies"].split(",")[8]), length - 1)
            # An enlarged history cannot forgive a missing or duplicated edge consumption.
            if length > 1:
                for used in (length - 2, length):
                    corrupt = self.replace(report, "stream_dependencies", 2, used)
                    self.accounting(corrupt, False, "exactly once")
                self.accounting(self.replace(report, "stream_dependencies", 8, length), False)
        # Aggregate producers and edges span two epochs, but neither actual
        # epoch contains enough instructions to support a depth2 chain.
        report = self.checked([chain(2), chain(2)], maximum=4096)
        self.accounting(self.replace(report, "stream_dependencies", 8, 2), False)
        legacy = self.checked([chain(32)], schema=71, minimum=1, maximum=32)
        for field in ("target_stream_min_instructions", "target_stream_max_instructions", "stream_history"):
            del legacy[field]
        self.accounting(legacy)
        self.accounting(dict(legacy, target_stream_min_instructions=True,
                             target_stream_max_instructions=[], stream_history="old ignored"))
        long_legacy = self.checked([chain(33)], minimum=1, maximum=33)
        long_legacy["constraint_schema"] = 71
        for field in ("target_stream_min_instructions", "target_stream_max_instructions", "stream_history"):
            del long_legacy[field]
        self.accounting(long_legacy, False, "epoch counts")
        # Two short epochs satisfy legacy total length but may not invent a depth32 chain.
        legacy = self.checked([chain(32), chain(2)], schema=71, minimum=1, maximum=32)
        self.accounting(self.replace(legacy, "stream_dependencies", 8, 32), False, "chain depth")

    def test_missing_malformed_negative_and_out_of_range_fields(self):
        base = self.checked([window(33)])
        for field in ("target_stream_min_instructions", "target_stream_max_instructions", "stream_history"):
            missing = dict(base)
            del missing[field]
            self.accounting(missing, False)
        for field in ("target_stream_min_instructions", "target_stream_max_instructions"):
            for bad in (None, True, False, "8", -1, 0, 4097, [], {}, 8.0):
                with self.subTest(field=field, value=bad):
                    self.accounting(dict(base, **{field: bad}), False, "instruction bounds")
        self.accounting(dict(base, target_stream_min_instructions=34,
                             target_stream_max_instructions=33), False, "instruction bounds")
        for bad in (None, True, 0, [], {}, "", "1,33,1", "1,33,1,0,0", "true,33,1,0", "1,33,1.0,0"):
            with self.subTest(history=bad):
                self.accounting(dict(base, stream_history=bad), False, "stream_history")
        for index in range(4):
            self.accounting(self.replace(base, "stream_history", index, -1), False, "negative count")

    def test_impossible_history_aggregates_and_empty_observations(self):
        empty = self.checked([])
        for index in range(4):
            self.accounting(self.replace(empty, "stream_history", index, 1), False)
        singleton = self.checked([window(1)])
        for counts in ("1,1,0,0", "0,0,0,0", "0,2,0,0", "0,1,1,0", "0,1,0,1"):
            self.accounting(dict(singleton, stream_history=counts), False)
        short = self.checked([window(32)])
        self.accounting(dict(short, stream_history="1,32,0,0"), False, "extended epoch")
        long = self.checked([window(33)])
        for counts in ("2,33,1,0", "0,33,0,0", "1,257,1,0", "1,34,1,0", "1,33,2,0", "1,33,1,33"):
            self.accounting(dict(long, stream_history=counts), False)
        serial = self.checked([window(33, serial=True)])
        self.accounting(dict(serial, stream_history="1,33,1,0"), False, "eligible admissions")
        # Actual mixed histories isolate each aggregate length relationship.
        for lengths, corrupt in (((33, 1), "2,33,0,0"), ((33, 33), "1,33,0,0"),
                                 ((33, 32), "1,32,0,0"), ((64, 1, 1), "1,21,0,0"),
                                 ((64, 32), "1,64,33,0")):
            report = self.checked([window(length, serial=True) for length in lengths])
            self.accounting(dict(report, stream_history=corrupt), False)

    def test_each_aggregate_length_and_late_tail_bound_rejects_impossible_evidence(self):
        # Two extended epochs require at least one longest epoch plus another
        # 33 instructions, even when the total fits max * epoch_count.
        report = self.checked([window(64, serial=True), window(32, serial=True)])
        self.accounting(dict(report, stream_history="2,64,0,0"), False, "lengths disagree")
        # An observed longest epoch plus a second length33 epoch cannot be
        # described as just one extended epoch (the other would exceed32).
        report = self.checked([window(64, serial=True), window(33, serial=True)])
        self.accounting(dict(report, stream_history="1,64,0,0"), False, "lengths disagree")
        # A claimed tail count34 fits 2 * (64 - 32), but the actual total
        # permits only33 ordinal>=32 admissions across these two epochs.
        report = self.checked([window(64), window(33)])
        self.assertEqual(report["stream_history"], "2,64,33,0")
        self.accounting(dict(report, stream_history="2,64,34,0"), False, "extended tails")
        # These two short epochs raise total admissions and overlap, but do not
        # create any extra late admissions beyond the longest epoch's32 tail.
        report = self.checked([window(64), window(32), window(32)])
        self.assertEqual(report["stream_history"], "1,64,32,0")
        self.accounting(dict(report, stream_history="1,64,33,0"), False, "extended tails")

    def test_larger_source_limit_never_weakens_exactly_once_or_issue_conservation(self):
        for maximum in (33, 256, 4096):
            base = self.checked([window(33, mixed=True)], maximum=maximum)
            for field, index in (("stream", 1), ("stream", 2), ("stream_ops", 0),
                                 ("stream_issue", 0), ("stream_issue", 1), ("stream_issue", 2),
                                 ("stream_issue", 3), ("stream_issue", 4), ("stream_issue", 5),
                                 ("stream_vector_uops", 0), ("stream_vector_uops", 1)):
                original = int(base[field].split(",")[index])
                for delta in (-1, 1):
                    with self.subTest(maximum=maximum, field=field, index=index, delta=delta):
                        self.accounting(self.replace(base, field, index, original + delta), False)


if __name__ == "__main__":
    unittest.main()
