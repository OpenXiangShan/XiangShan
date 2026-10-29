#!/usr/bin/env python3
"""Check schema68 accounting against explicit older-store/load histories.

These are serializer and artifact-checker tests, not DUT reachability evidence.
Every initial load completes before each older STA reveals its byte address.
An overlapping STA invalidates the oldest matching load and its entire younger
suffix; every invalidated execution is strictly checked again before the next
STA. Architectural instruction selection and issue happen only once.
"""
from __future__ import annotations

from dataclasses import dataclass
import hashlib
import itertools
import random
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))
import run_regression  # noqa: E402
import verify_regression as verifier  # noqa: E402

UINT64_MAX = (1 << 64) - 1
FIELDS = {
    "stream": 12,
    "stream_ops": 5,
    "stream_issue": 6,
    "stream_vector_uops": 2,
    "stream_waiting_head": 2,
    "stream_recovery": 20,
    "stream_recovery_shapes": 13,
    "stream_recovery_victims": 4,
    "scalar_format_choices": 8,
    "scalar_fp": 3,
    "scalar_fp_sign": 6,
    "scalar_fp_origin": 4,
    "scalar_fp_faults": 3,
    "scalar_fp_overlay": 2,
    "load_ops": 7,
    "scalar": 2,
}
PEAKS = {("stream", 6), ("stream", 7),
         ("stream_recovery", 16), ("stream_recovery", 17), ("stream_recovery", 18)}


def csv(values):
    return ",".join(str(int(value)) for value in values)


@dataclass(frozen=True)
class History:
    """Distinct eight-byte load slots and older byte stores in program order.

    A store target of -1 selects a disjoint byte. Other targets overlap exactly
    that load, including FP loads. Prefix and continuation instructions are
    independent integer loads outside this graph; they never add recovery
    roles. The prefix retires during the barrier, while continuation admission
    occurs after barrier release with at least one older graph role still live.
    """
    loads: int
    targets: tuple[int, ...]
    fp_mask: int = 0
    prefix: int = 0
    continued: int = 0
    cancellation_cap: int = 0
    waiting_while_prefix: bool = False

    def counts(self):
        n, stores = self.loads, len(self.targets)
        assert 1 <= n <= 4 and 1 <= stores <= 4
        assert all(-1 <= target < n for target in self.targets)
        assert 0 <= self.fp_mask < 1 << n
        assert 0 <= self.prefix <= 3 and 0 <= self.continued <= 3
        assert self.cancellation_cap >= 0
        assert not self.waiting_while_prefix or self.prefix == 1
        rows = {name: [0] * width for name, width in FIELDS.items()}
        executions = []
        selected = []
        victims = []
        canceled = 0
        memory = bytearray(128)
        widths = [1 + age % 3 if self.fp_mask & (1 << age) else 0 for age in range(n)]

        def check(age):
            width = widths[age]
            address, size = 8 * age, 1 << width
            value = int.from_bytes(memory[address:address + size], "little")
            executions.append((age, width, value))

        # Graph load roles are chosen once, regardless of execution versions.
        for age in range(n):
            selected.append(bool(self.fp_mask & (1 << age)))
            check(age)
        for store, target in enumerate(self.targets):
            address = 64 + store if target == -1 else 8 * target
            memory[address] = store + 1
            overlaps = [age for age in range(n)
                        if 8 * age <= address < 8 * age + (1 << widths[age])]
            if not overlaps:
                continue
            victim = min(overlaps)
            victims.append(victim)
            suffix = list(range(victim, n))
            canceled += min(self.cancellation_cap, len(suffix))
            for age in suffix:
                check(age)

        rounds = len(victims)
        invalidated = len(executions) - n
        invalidated_fp = sum(bool(self.fp_mask & (1 << age))
                             for age, _width, _value in executions[n:])
        rows["stream_recovery"] = [1, bool(rounds), not rounds, stores, n, n,
            rounds, invalidated, invalidated, stores, n, bool(self.prefix),
            self.prefix, self.continued, canceled, invalidated_fp,
            stores, n, rounds, 1]
        rows["stream_recovery_shapes"][stores - 1] = 1
        rows["stream_recovery_shapes"][4 + n - 1] = 1
        rows["stream_recovery_shapes"][8 + rounds] = 1
        rows["stream_recovery_victims"] = [victims.count(age) for age in range(4)]
        ordinary = self.prefix + self.continued
        architectural_loads = n + ordinary
        architectural_work = stores + architectural_loads
        rows["stream_ops"] = [architectural_loads, 0, stores, 0, 0]
        rows["stream_issue"] = [architectural_loads, stores, stores, 0, 0, 0]
        events = sum(rows["stream_issue"])
        sliding = max(0, architectural_work - 8)
        rows["stream"] = [1, architectural_work, architectural_work,
            architectural_work - 1, sliding, max(sliding, self.prefix),
            min(8, architectural_work), min(8, architectural_work), events,
            0, 0, architectural_work]
        if self.waiting_while_prefix:
            # The already-complete older prefix head can wait while every
            # remaining initial/refetch producer is selected, then retire.
            rows["stream_waiting_head"][1] = events - 1 + invalidated
        rows["scalar_format_choices"][2] = selected.count(False) + ordinary
        rows["scalar_format_choices"][3] = selected.count(True)
        rows["load_ops"][0] = ordinary
        for age, width, value in executions:
            rows["load_ops"][width] += 1
            if self.fp_mask & (1 << age):
                rows["scalar_fp"][width - 1] += 1
                rows["scalar_fp_origin"][1] += 1
                sign = (value >> ((1 << width) * 8 - 1)) & 1
                rows["scalar_fp_sign"][2 * (width - 1) + sign] += 1
        rows["scalar"] = [len(executions) + ordinary, stores]
        return rows


def population(histories, *, copies=1, rate=1000, conflict_rate=500,
               stream_rate=1000, concurrent_rate=1000, fp_rate=500, schema=68):
    """Aggregate independent histories; only peak counters avoid summation."""
    assert copies >= 1
    observations = [history.counts() for history in histories]
    rows = {name: [max((row[name][index] for row in observations), default=0)
                   if (name, index) in PEAKS else
                   copies * sum(row[name][index] for row in observations)
                   for index in range(width)]
            for name, width in FIELDS.items()}
    architectural = sum(rows["stream_ops"])
    rows["stream"][11] = architectural
    return dict(constraint_schema=schema, transactions=max(1, architectural),
        target_stream_unresolved=rate, target_scalar_unresolved_conflict=conflict_rate,
        target_concurrent_stream=stream_rate, target_concurrent=concurrent_rate,
        target_scalar_fp=fp_rate, target_scalar_fp_widths="1,1,1",
        stream_scalar_retries=0, scalar_fp_misaligned=0,
        **{name: csv(values) for name, values in rows.items()})


CPP = r'''
#include "memblock_env.hpp"
#include <numeric>
// PRODUCTION_MIXED_COVERAGE

int main() {
    std::string line;
    while (std::getline(std::cin, line)) {
        std::istringstream input(line);
        MixedCoverage coverage;
        auto read = [&](auto &values) { for (auto &value : values) input >> value; };
        read(coverage.stream);
        read(coverage.stream_ops);
        read(coverage.stream_issue);
        read(coverage.stream_vector_uops);
        read(coverage.stream_waiting_head);
        read(coverage.stream_recovery);
        read(coverage.stream_recovery_shapes);
        read(coverage.stream_recovery_victims);
        read(coverage.scalar_format_choices);
        read(coverage.scalar_fp);
        read(coverage.scalar_fp_sign);
        read(coverage.scalar_fp_origin);
        read(coverage.scalar_fp_faults);
        read(coverage.scalar_fp_overlay);
        read(coverage.load_ops);
        input >> coverage.scalar_loads >> coverage.scalar_stores;
        input >> coverage.stream_scalar_retries >> coverage.scalar_fp_misaligned;
        std::string extra;
        if (input.fail() || input >> extra) return 1;
        std::istringstream summary(coverage.summary());
        std::cout << "MEMBLOCK_CONSTRAINED_RANDOM_PASS";
        std::string token;
        while (summary >> token) std::cout << ' ' << token;
        std::cout << '\n';
    }
}
'''


class StreamRecoveryAccountingTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source_paths = (ROOT / "cpp/random/coverage.inc", ROOT / "cpp/random/constraints.inc",
                            ROOT / "scripts/verify_regression.py")
        cls.before = cls._hashes()
        cls.workspace = tempfile.TemporaryDirectory(prefix="memblock-stream-recovery-accounting-")
        cls.addClassCleanup(cls.workspace.cleanup)
        cls.addClassCleanup(cls._unchanged_sources)
        production = cls.source_paths[0].read_text()
        mixed = "struct MixedCoverage {" + production.split("struct MixedCoverage {", 1)[1].split("\nstruct ", 1)[0]
        source = Path(cls.workspace.name) / "report.cpp"
        source.write_text(CPP.replace("// PRODUCTION_MIXED_COVERAGE", mixed))
        cls.binary = source.with_suffix("")
        built = subprocess.run(["c++", "-std=c++20", "-O0", "-I", str(ROOT / "cpp"),
                                str(source), "-o", str(cls.binary)],
                               capture_output=True, text=True, timeout=60)
        if built.returncode:
            raise AssertionError("production coverage reporter did not compile:\n" + built.stdout + built.stderr)

    @classmethod
    def _hashes(cls):
        return {str(path): hashlib.sha256(path.read_bytes()).hexdigest() for path in cls.source_paths}

    @classmethod
    def _unchanged_sources(cls):
        if cls.before != cls._hashes():
            raise AssertionError("production coverage/constraints/verifier changed during accounting tests")

    def reports(self, expected):
        text = "\n".join(" ".join(str(value) for field in FIELDS
                                  for value in map(int, report[field].split(","))) +
                         f" {report['stream_scalar_retries']} {report['scalar_fp_misaligned']}"
                         for report in expected) + "\n"
        ran = subprocess.run([str(self.binary)], input=text, capture_output=True, text=True, timeout=30)
        self.assertEqual(ran.returncode, 0, ran.stdout[:1000] + ran.stderr)
        actual = [run_regression.parse_summary(line) for line in ran.stdout.splitlines()]
        self.assertEqual(len(actual), len(expected))
        for original, report in zip(expected, actual):
            for field in FIELDS:
                self.assertEqual(report[field], original[field], field)
            for field in ("stream_scalar_retries", "scalar_fp_misaligned"):
                self.assertEqual(report[field], original[field], field)
            report.update({key: value for key, value in original.items() if key not in FIELDS})
        return actual

    @staticmethod
    def check(report):
        verifier._check_stream_accounting(report)
        verifier._check_stream_recovery_accounting(report)
        verifier._check_scalar_fp_accounting(report)

    def checked(self, histories, **options):
        result = self.reports([population(histories, **options)])[0]
        self.check(result)
        return result

    @staticmethod
    def replace(report, field, index, value):
        values = report[field].split(",")
        values[index] = str(value)
        return dict(report, **{field: ",".join(values)})

    def test_explicit_store_byte_histories_all_loads_rounds_victims_and_fp_masks(self):
        expected = []
        reached_rounds, reached_victims, reached_widths = set(), set(), set()
        for loads in range(1, 5):
            for stores in range(1, 5):
                for targets in itertools.product(range(-1, loads), repeat=stores):
                    for fp_mask in range(1 << loads):
                        ordinal = len(expected)
                        history = History(loads, targets, fp_mask,
                            prefix=ordinal % 4, continued=(ordinal // 4) % 4,
                            cancellation_cap=(ordinal // 16) % 5)
                        row = population([history])
                        counts = history.counts()
                        reached_rounds.add(counts["stream_recovery"][6])
                        reached_victims.update(age for age, count in enumerate(
                            counts["stream_recovery_victims"]) if count)
                        reached_widths.update(width for width, count in enumerate(counts["scalar_fp"]) if count)
                        expected.append(row)
        self.assertEqual(len(expected), 15740)
        self.assertEqual(reached_rounds, set(range(5)))
        self.assertEqual(reached_victims, set(range(4)))
        self.assertEqual(reached_widths, set(range(3)))
        # Bound process output while still using the actual production reporter.
        for start in range(0, len(expected), 500):
            for index, report in enumerate(self.reports(expected[start:start + 500]), start):
                try:
                    self.check(report)
                except verifier.VerificationError as error:
                    self.fail(f"legal store/load history {index} rejected: {expected[index]}: {error}")

    def test_seeded_aggregate_populations_preserve_shapes_peaks_and_fp_executions(self):
        chooser = random.Random(0x6841)
        expected = []
        for _ in range(192):
            histories = []
            for _ in range(chooser.randrange(1, 17)):
                loads, stores = chooser.randrange(1, 5), chooser.randrange(1, 5)
                histories.append(History(loads,
                    tuple(chooser.randrange(-1, loads) for _ in range(stores)),
                    fp_mask=chooser.randrange(1 << loads), prefix=chooser.randrange(4),
                    continued=chooser.randrange(4), cancellation_cap=chooser.randrange(5)))
            expected.append(population(histories, copies=chooser.randrange(1, 1001)))
        for index, report in enumerate(self.reports(expected)):
            with self.subTest(population=index):
                self.check(report)

    def test_literal_four_round_suffixes_and_architectural_issue_are_distinct(self):
        report = self.checked([History(4, (2, 0, 0, 1), fp_mask=0b1010,
                                      prefix=1, continued=2, cancellation_cap=4)])
        self.assertEqual(report["stream_recovery"], "1,1,0,4,4,4,4,13,13,4,4,1,1,2,13,7,4,4,4,1")
        self.assertEqual(report["stream_recovery_shapes"], "0,0,0,1,0,0,0,1,0,0,0,0,1")
        self.assertEqual(report["stream_recovery_victims"], "2,1,1,0")
        self.assertEqual(report["stream_ops"], "7,0,4,0,0")
        self.assertEqual(report["stream_issue"], "7,4,4,0,0,0")
        self.assertEqual(report["scalar_format_choices"], "0,0,5,2,0,0,0,0")
        self.assertEqual(report["scalar_fp_origin"], "0,9,0,0")
        self.assertEqual(report["scalar"], "20,4")

    def test_zero_sparse_disabled_controls_and_aggregate_populations(self):
        for rate, conflict, stream, concurrent, fp in itertools.product((0, 1, 1000), repeat=5):
            self.checked([], rate=rate, conflict_rate=conflict, stream_rate=stream,
                         concurrent_rate=concurrent, fp_rate=fp)
        controls = [History(n, (-1,) * stores, fp_mask=(1 << n) - 1)
                    for n, stores in itertools.product(range(1, 5), repeat=2)]
        self.checked(controls, conflict_rate=0, fp_rate=1000)
        sparse = History(1, (0,), fp_mask=1)
        self.checked([sparse], rate=1, conflict_rate=1, fp_rate=1)
        mixed = self.checked(controls + [sparse,
            History(4, (3, 2, 1, 0), fp_mask=0b1010, prefix=3, continued=3),
            History(2, (-1, 1, -1, 0), fp_mask=1, prefix=1)])
        self.assertEqual(mixed["stream_recovery"].split(",")[16:19], ["4", "4", "4"])
        for options in ({"rate": 0}, {"stream_rate": 0}, {"concurrent_rate": 0},
                        {"conflict_rate": 0}, {"fp_rate": 0}):
            with self.subTest(options=options), self.assertRaises(verifier.VerificationError):
                self.check(population([sparse], **options))
        with self.assertRaises(verifier.VerificationError):
            self.check(population([controls[0]], conflict_rate=1000))

    def test_completed_head_issue_budget_includes_refetch_but_architecture_does_not(self):
        report = self.checked([History(4, (0,), fp_mask=0b0101, prefix=1,
                                      waiting_while_prefix=True)])
        self.assertEqual(report["stream_waiting_head"], "0,10")
        self.assertEqual(report["stream_issue"], "5,1,1,0,0,0")
        self.assertEqual(report["stream"].split(",")[8], "7")
        bad = self.replace(report, "stream_waiting_head", 1, 11)
        with self.assertRaises(verifier.VerificationError):
            self.check(bad)
        bad = self.replace(report, "stream_issue", 0, 9)
        bad = self.replace(bad, "stream", 8, 11)
        with self.assertRaises(verifier.VerificationError):
            self.check(bad)

    def test_schema67_ignores_new_fields_and_keeps_old_fp_execution_relation(self):
        report = self.checked([History(2, (-1,), fp_mask=1)], schema=67)
        for name in ("stream_recovery", "stream_recovery_shapes", "stream_recovery_victims",
                     "target_stream_unresolved"):
            del report[name]
        self.check(report)
        report.update(stream_recovery="ignored legacy field", stream_recovery_shapes=None,
                      stream_recovery_victims=True, target_stream_unresolved=[])
        self.check(report)
        with self.assertRaises(verifier.VerificationError):
            self.check(population([History(1, (0,), fp_mask=1)], schema=67))

    def test_uint64_large_repeated_populations_preserve_per_window_maxima(self):
        report = self.checked([History(1, (-1,), fp_mask=1)], copies=UINT64_MAX // 3,
                              conflict_rate=0, fp_rate=1000)
        self.assertEqual(report["stream"].split(",")[8], str(UINT64_MAX))
        self.assertEqual(report["stream_recovery"].split(",")[16:19], ["1", "1", "0"])
        self.checked([History(4, (0, 1, 2, 3), fp_mask=15)],
                     copies=UINT64_MAX // 18, conflict_rate=1000, fp_rate=1000)

    def test_consistent_populations_cannot_exceed_emitter_uint64_domain(self):
        # Deliberately bypass C++: no emitter can serialize these otherwise
        # conserved populations without wrapping its uint64_t counters.
        for history, copies in ((History(1, (-1,), fp_mask=1), UINT64_MAX),
                                (History(1, (-1,)), UINT64_MAX + 1)):
            with self.subTest(copies=copies), self.assertRaises(verifier.VerificationError):
                self.check(population([history], copies=copies, conflict_rate=0))

    def test_missing_malformed_negative_and_invalid_rates_are_rejected(self):
        base = self.checked([History(4, (3, 2, 1, 0), fp_mask=15, prefix=1, continued=1)])
        for field, width in ((name, FIELDS[name]) for name in
                             ("stream_recovery", "stream_recovery_shapes", "stream_recovery_victims")):
            missing = dict(base)
            del missing[field]
            with self.subTest(missing=field), self.assertRaises(verifier.VerificationError):
                self.check(missing)
            for value in (None, True, 0, [], "", "true", "1.0", csv([0] * (width - 1)),
                          csv([0] * (width + 1))):
                with self.subTest(field=field, value=value), self.assertRaises(verifier.VerificationError):
                    self.check(dict(base, **{field: value}))
            for index in range(width):
                with self.subTest(field=field, index=index), self.assertRaises(verifier.VerificationError):
                    self.check(self.replace(base, field, index, -1))
        for field in ("target_stream_unresolved", "target_scalar_unresolved_conflict"):
            missing = dict(base)
            del missing[field]
            with self.subTest(missing=field), self.assertRaises(verifier.VerificationError):
                self.check(missing)
            for value in (None, True, "500", -1, 1001, []):
                with self.subTest(field=field, value=value), self.assertRaises(verifier.VerificationError):
                    self.check(dict(base, **{field: value}))

    def test_lost_duplicate_and_forged_observations_are_rejected(self):
        base = self.checked([History(4, (3, 2, 1, 0), fp_mask=15, prefix=1, continued=1,
                                     cancellation_cap=4)], fp_rate=500)
        corrupt = [("stream_recovery", index, value) for index, value in
                   ((0, 0), (0, 2), (1, 0), (2, 1), (3, 3), (4, 3), (5, 3),
                    (6, 3), (7, 9), (8, 9), (9, 3), (10, 3), (11, 2),
                    (12, 11), (13, 3), (14, 11), (15, 11), (16, 3), (17, 3),
                    (18, 3), (19, 0), (19, 2))]
        corrupt += [("stream_recovery_shapes", 0, 1), ("stream_recovery_shapes", 4, 1),
                    ("stream_recovery_shapes", 8, 1), ("stream_recovery_victims", 0, 2)]
        for field, index, value in corrupt:
            with self.subTest(field=field, index=index, value=value), self.assertRaises(verifier.VerificationError):
                self.check(self.replace(base, field, index, value))

    def test_graph_roles_must_fit_observed_peak_live_instructions(self):
        for history in (History(4, (0, 1, 2, 3)), History(1, (0, 0, 0, 0)),
                        History(4, (0,)), History(2, (0, 1))):
            report = self.checked([history])
            minimum = history.loads + len(history.targets)
            for peak in (1, minimum - 1):
                forged = self.replace(report, "stream", 6, peak)
                forged = self.replace(forged, "stream", 7, min(peak, int(report["stream"].split(",")[7])))
                with self.subTest(history=history, peak=peak), self.assertRaises(verifier.VerificationError):
                    self.check(forged)

    def test_every_round_invalidates_its_complete_suffix(self):
        report = self.checked([History(4, (0,))])
        report = self.replace(report, "stream_recovery", 7, 1)
        report = self.replace(report, "stream_recovery", 8, 1)
        with self.assertRaises(verifier.VerificationError):
            self.check(report)

    def test_victims_need_enough_windows_with_that_load_age(self):
        report = self.checked([History(1, (0,)), History(4, (3,))])
        report["stream_recovery_victims"] = "0,0,0,2"
        with self.assertRaises(verifier.VerificationError):
            self.check(report)

    def test_round_histogram_needs_matching_per_window_store_capacity(self):
        histories = [History(1, (-1,)) for _ in range(8)]
        histories += [History(1, (0,)), History(1, (0, 0, 0, 0))]
        report = self.checked(histories)
        # Nine one-store windows and one four-store window cannot contain two
        # four-round windows, despite a sufficient aggregate store total.
        report["stream_recovery_shapes"] = "9,0,0,1,10,0,0,0,8,0,0,0,2"
        for index, value in ((6, 8), (7, 8), (8, 8), (18, 4)):
            report = self.replace(report, "stream_recovery", index, value)
        report["stream_recovery_victims"] = "8,0,0,0"
        with self.assertRaises(verifier.VerificationError):
            self.check(report)

    def test_all_fp_architecture_cannot_refetch_integer_executions(self):
        report = self.checked([History(4, (0,), fp_mask=15)], fp_rate=1000)
        report = self.replace(report, "stream_recovery", 15, 1)
        # Keep the existing selected+invalidated FP execution conservation;
        # the forged suffix silently changes three FP refetches to integers.
        report["scalar_fp"] = "2,1,2"
        report["scalar_fp_sign"] = "2,0,1,0,2,0"
        report["scalar_fp_origin"] = "0,5,0,0"
        report["load_ops"] = "3,2,1,2,0,0,0"
        with self.assertRaises(verifier.VerificationError):
            self.check(report)

    def test_fp_selection_happens_once_and_each_invalidated_fp_is_rechecked(self):
        report = self.checked([History(4, (0, 0, 0, 0), fp_mask=15)], fp_rate=1000)
        self.assertEqual(report["scalar_format_choices"], "0,0,0,4,0,0,0,0")
        self.assertEqual(report["scalar_fp_origin"], "0,20,0,0")
        for field, index, value in (("scalar_format_choices", 3, 20),
                                    ("scalar_fp_origin", 1, 4),
                                    ("stream_recovery", 15, 15),
                                    ("stream_recovery", 15, 17)):
            with self.subTest(field=field, index=index), self.assertRaises(verifier.VerificationError):
                self.check(self.replace(report, field, index, value))


if __name__ == "__main__":
    unittest.main()
