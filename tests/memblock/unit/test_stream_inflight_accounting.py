#!/usr/bin/env python3
"""Check schema69 unfinished-stream accounting against explicit load histories.

These are accounting and production-serializer tests, not DUT reachability
proof. The youngest cold load is disjoint from all stores. Its initial response
is held and canceled once; all warm initial executions complete, and every
returned recovery round strictly completes the entire invalidated suffix.
Architectural format selection and initial issue happen once per instruction.
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
import test_stream_recovery_accounting as completed  # noqa: E402

UINT64_MAX = (1 << 64) - 1
FIELDS = dict(completed.FIELDS, stream_recovery_inflight=11)
PEAKS = completed.PEAKS
csv = completed.csv
# Reuse only the schema68 test's reporter plumbing, never its history model.
CPP = completed.CPP.replace(
    "input >> coverage.stream_scalar_retries",
    "read(coverage.stream_recovery_inflight);\n        input >> coverage.stream_scalar_retries")


@dataclass(frozen=True)
class History:
    """Byte-addressed stores, checked execution versions, and a canceled role.

    targets names a warm load age, or -1 for a disjoint byte. When selected,
    the final load lives on a separate cold line and cannot be a store target.
    Prefix and continued instructions are independent integer loads. The two
    old-response observations select/retire ordinary prefix work while the
    canceled cold response drains; neither requires all other work to drain.
    """
    loads: int
    targets: tuple[int, ...]
    fp_mask: int = 0
    selected: bool = True
    unavailable: bool = False
    prefix: int = 0
    continued: int = 0
    cancellation_cap: int = 0
    release_gap: int = 0
    old_producers: int = 0
    old_retire: int = 0

    def counts(self):
        n, stores = self.loads, len(self.targets)
        assert 1 <= n <= 4 and 1 <= stores <= 4
        assert not (self.selected and self.unavailable)
        assert not self.selected or n >= 2
        assert all(-1 <= target < n - int(self.selected) for target in self.targets)
        assert 0 <= self.fp_mask < 1 << n
        assert 0 <= self.prefix <= 3 and 0 <= self.continued <= 3
        assert self.cancellation_cap >= 0 and 0 <= self.release_gap <= 7
        assert 0 <= self.old_producers <= self.prefix and 0 <= self.old_retire <= self.prefix
        assert self.selected or self.release_gap == self.old_producers == self.old_retire == 0
        rows = {name: [0] * width for name, width in FIELDS.items()}
        widths = [1 + age % 3 if self.fp_mask & (1 << age) else 0 for age in range(n)]
        addresses = [8 * age for age in range(n)]
        if self.selected:
            addresses[-1] = 128
        memory = bytearray(192)
        executions = []
        checked_versions = [False] * n
        victims = []
        invalidated = fp_invalidated = lq_canceled = cold_canceled = cold_refetched = 0

        def strict_check(age):
            address, width = addresses[age], widths[age]
            value = int.from_bytes(memory[address:address + (1 << width)], "little")
            executions.append((age, width, value))
            checked_versions[age] = True

        for age in range(n - int(self.selected)):
            strict_check(age)
        initial = len(executions)
        for store, target in enumerate(self.targets):
            address = 64 + store if target == -1 else addresses[target]
            memory[address] = store + 1
            overlaps = [age for age, start in enumerate(addresses)
                        if start <= address < start + (1 << widths[age])]
            if not overlaps:
                continue
            victim = min(overlaps)
            victims.append(victim)
            suffix = list(range(victim, n))
            observed = min(self.cancellation_cap, len(suffix))
            if self.selected and cold_canceled == 0:
                observed = max(1, observed)
            lq_canceled += observed
            for age in suffix:
                if checked_versions[age]:
                    invalidated += 1
                    fp_invalidated += bool(self.fp_mask & (1 << age))
                else:
                    assert self.selected and age == n - 1 and cold_canceled == 0
                    cold_canceled += 1
                checked_versions[age] = False
            for age in suffix:
                strict_check(age)
                cold_refetched += self.selected and age == n - 1
        rounds = len(victims)
        assert not (self.selected or self.unavailable) or rounds
        assert cold_canceled == int(self.selected)
        refetched = len(executions) - initial
        rows["stream_recovery"] = [1, bool(rounds), not rounds, stores, n, initial,
            rounds, invalidated, refetched, stores, n, bool(self.prefix), self.prefix,
            self.continued, lq_canceled, fp_invalidated, stores, n, rounds, 1]
        rows["stream_recovery_shapes"][stores - 1] = 1
        rows["stream_recovery_shapes"][4 + n - 1] = 1
        rows["stream_recovery_shapes"][8 + rounds] = 1
        rows["stream_recovery_victims"] = [victims.count(age) for age in range(4)]
        rows["stream_recovery_inflight"] = [self.selected or self.unavailable,
            self.unavailable, self.selected, self.selected, cold_canceled, self.selected,
            cold_refetched, self.selected and bool(self.fp_mask & (1 << (n - 1))),
            self.release_gap, self.old_producers, self.old_retire]
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
        rows["scalar_format_choices"][2] = n - self.fp_mask.bit_count() + ordinary
        rows["scalar_format_choices"][3] = self.fp_mask.bit_count()
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
               inflight_rate=250, stream_rate=1000, concurrent_rate=1000,
               fp_rate=500, schema=69):
    assert copies >= 1
    observations = [history.counts() for history in histories]
    rows = {field: [max((row[field][index] for row in observations), default=0)
                    if (field, index) in PEAKS else
                    copies * sum(row[field][index] for row in observations)
                    for index in range(width)] for field, width in FIELDS.items()}
    architectural = sum(rows["stream_ops"])
    rows["stream"][11] = architectural
    return dict(constraint_schema=schema, transactions=max(1, architectural),
        target_stream_unresolved=rate, target_scalar_unresolved_conflict=conflict_rate,
        target_stream_unresolved_inflight=inflight_rate,
        target_concurrent_stream=stream_rate, target_concurrent=concurrent_rate,
        target_scalar_fp=fp_rate, target_scalar_fp_widths="1,1,1",
        stream_scalar_retries=0, scalar_fp_misaligned=0,
        **{field: csv(values) for field, values in rows.items()})


class StreamInflightAccountingTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source_paths = (ROOT / "cpp/random/coverage.inc", ROOT / "cpp/random/constraints.inc",
                            ROOT / "scripts/verify_regression.py", Path(completed.__file__))
        cls.before = cls._hashes()
        cls.workspace = tempfile.TemporaryDirectory(prefix="memblock-stream-inflight-accounting-")
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
            raise AssertionError("production or shared test helper changed during accounting tests")

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
            report.update({key: value for key, value in original.items() if key not in FIELDS})
        return actual

    @staticmethod
    def check(report):
        verifier._check_stream_accounting(report)
        verifier._check_stream_recovery_accounting(report)
        verifier._check_scalar_fp_accounting(report)

    def checked(self, histories, **options):
        report = self.reports([population(histories, **options)])[0]
        self.check(report)
        return report

    replace = staticmethod(completed.StreamRecoveryAccountingTest.replace)

    def test_explicit_cold_histories_all_warm_victims_rounds_and_fp_masks(self):
        expected = []
        seen_rounds, seen_victims, seen_gaps, seen_widths, seen_cold_fp = set(), set(), set(), set(), set()
        for loads in range(2, 5):
            for stores in range(1, 5):
                for targets in itertools.product(range(-1, loads - 1), repeat=stores):
                    if all(target == -1 for target in targets):
                        continue
                    for fp_mask in range(1 << loads):
                        ordinal = len(expected)
                        prefix = ordinal % 4
                        history = History(loads, targets, fp_mask, prefix=prefix,
                            continued=(ordinal // 4) % 4, cancellation_cap=(ordinal // 16) % 5,
                            release_gap=ordinal % 8, old_producers=prefix,
                            old_retire=prefix if ordinal % 2 else 0)
                        rows = history.counts()
                        seen_rounds.add(rows["stream_recovery"][6])
                        seen_victims.update(age for age, count in enumerate(rows["stream_recovery_victims"]) if count)
                        seen_gaps.add(history.release_gap)
                        seen_widths.update(width for width, count in enumerate(rows["scalar_fp"]) if count)
                        seen_cold_fp.add(bool(rows["stream_recovery_inflight"][7]))
                        expected.append(population([history]))
        self.assertEqual(len(expected), 6408)
        self.assertEqual(seen_rounds, {1, 2, 3, 4})
        self.assertEqual(seen_victims, {0, 1, 2})
        self.assertEqual(seen_gaps, set(range(8)))
        self.assertEqual(seen_widths, {0, 1, 2})
        self.assertEqual(seen_cold_fp, {False, True})
        for start in range(0, len(expected), 250):
            for index, report in enumerate(self.reports(expected[start:start + 250]), start):
                try:
                    self.check(report)
                except verifier.VerificationError as error:
                    self.fail(f"explicit cold history {index} rejected: {expected[index]}: {error}")

    def test_literal_four_round_history_cancels_once_and_rechecks_whole_suffix(self):
        report = self.checked([History(4, (2, 0, 0, 1), fp_mask=0b1010,
            prefix=1, continued=2, cancellation_cap=4, release_gap=7, old_producers=1, old_retire=1)])
        self.assertEqual(report["stream_recovery"], "1,1,0,4,4,3,4,12,13,4,4,1,1,2,13,6,4,4,4,1")
        self.assertEqual(report["stream_recovery_inflight"], "1,0,1,1,1,1,4,1,7,1,1")
        self.assertEqual(report["stream_recovery_victims"], "2,1,1,0")
        self.assertEqual(report["stream_ops"], "7,0,4,0,0")
        self.assertEqual(report["stream_issue"], "7,4,4,0,0,0")
        self.assertEqual(report["scalar_format_choices"], "0,0,5,2,0,0,0,0")
        self.assertEqual(report["scalar_fp_origin"], "0,8,0,0")
        self.assertEqual(report["scalar"], "19,4")

    def test_seeded_mixed_aggregates_preserve_cold_versions_and_completed_windows(self):
        chooser = random.Random(0x6941)
        expected = []
        for _ in range(160):
            histories = []
            for _ in range(chooser.randrange(1, 17)):
                selected = bool(chooser.randrange(2))
                loads = chooser.randrange(2 if selected else 1, 5)
                targets = tuple(chooser.randrange(-1, loads - int(selected))
                                for _ in range(chooser.randrange(1, 5)))
                if selected and all(target < 0 for target in targets):
                    targets = (0,) + targets[1:]
                unavailable = not selected and any(target >= 0 for target in targets) and bool(chooser.randrange(2))
                prefix = chooser.randrange(4)
                histories.append(History(loads, targets, fp_mask=chooser.randrange(1 << loads),
                    selected=selected, unavailable=unavailable, prefix=prefix,
                    continued=chooser.randrange(4), cancellation_cap=chooser.randrange(5),
                    release_gap=chooser.randrange(8) if selected else 0,
                    old_producers=chooser.randrange(prefix + 1) if selected else 0,
                    old_retire=chooser.randrange(prefix + 1) if selected else 0))
            expected.append(population(histories, copies=chooser.randrange(1, 1001)))
        for index, report in enumerate(self.reports(expected)):
            with self.subTest(population=index):
                self.check(report)

    def test_zero_sparse_disabled_unavailable_and_multiple_selected_windows(self):
        for rate, conflict, inflight, stream, concurrent, fp in itertools.product((0, 1, 1000), repeat=6):
            self.checked([], rate=rate, conflict_rate=conflict, inflight_rate=inflight,
                         stream_rate=stream, concurrent_rate=concurrent, fp_rate=fp)
        controls = [History(n, (-1,), selected=False) for n in range(1, 5)]
        self.checked(controls, conflict_rate=0, inflight_rate=1000)
        completed_history = History(4, (3, 2, 1, 0), selected=False, cancellation_cap=4)
        self.checked([completed_history], inflight_rate=0)
        unavailable = [History(n, (0,), selected=False, unavailable=True) for n in range(1, 5)]
        report = self.checked(unavailable, inflight_rate=1000)
        self.assertEqual(report["stream_recovery_inflight"], "4,4,0,0,0,0,0,0,0,0,0")
        cold = History(2, (0,), fp_mask=2)
        self.checked([cold], rate=1, conflict_rate=1, inflight_rate=1)
        mixed = self.checked(controls + unavailable + [completed_history, cold,
            History(4, (2, 0, 1, 0), release_gap=7, prefix=3, old_producers=2, old_retire=1)])
        self.assertEqual(mixed["stream_recovery_inflight"], "6,4,2,2,2,2,5,1,7,2,1")
        for options in ({"rate": 0}, {"conflict_rate": 0}, {"inflight_rate": 0},
                        {"stream_rate": 0}, {"concurrent_rate": 0}):
            with self.subTest(options=options), self.assertRaises(verifier.VerificationError):
                self.check(population([cold], **options))
        with self.assertRaises(verifier.VerificationError):
            self.check(population(unavailable, inflight_rate=0))

    def test_schema68_ignores_new_fields_and_requires_all_initial_completions(self):
        histories = [History(4, (2, 0, 0, 3), selected=False, fp_mask=0b1010, cancellation_cap=4),
                     History(1, (-1,), selected=False)]
        report = self.checked(histories, schema=68)
        del report["stream_recovery_inflight"]
        del report["target_stream_unresolved_inflight"]
        self.check(report)
        self.check(dict(report, stream_recovery_inflight="ignored legacy field",
                        target_stream_unresolved_inflight=None))
        with self.assertRaises(verifier.VerificationError):
            self.check(population([History(2, (0,))], schema=68))

    def test_uint64_repetitions_keep_peaks_and_reject_overdomain_counters(self):
        cold = History(2, (0,), fp_mask=3)
        self.checked([cold], copies=UINT64_MAX // 5)
        # Repeated unavailable histories preserve per-window shape maxima.
        unavailable = History(1, (0,), selected=False, unavailable=True)
        self.checked([unavailable], copies=UINT64_MAX // 3)
        base = self.checked([cold])
        for index in range(11):
            with self.subTest(index=index), self.assertRaises(verifier.VerificationError):
                self.check(self.replace(base, "stream_recovery_inflight", index, UINT64_MAX + 1))

    def test_missing_malformed_negative_inflight_fields_and_invalid_rates(self):
        base = self.checked([History(4, (2, 0, 0, 1), fp_mask=0b1010, release_gap=7)])
        missing = dict(base)
        del missing["stream_recovery_inflight"]
        with self.assertRaises(verifier.VerificationError):
            self.check(missing)
        for bad in (None, True, 0, [], "", "true", "1,2", csv([0] * 10), csv([0] * 12)):
            with self.subTest(bad=bad), self.assertRaises(verifier.VerificationError):
                self.check(dict(base, stream_recovery_inflight=bad))
        for index in range(11):
            with self.subTest(index=index), self.assertRaises(verifier.VerificationError):
                self.check(self.replace(base, "stream_recovery_inflight", index, -1))
        for bad in (None, True, "250", -1, 1001, []):
            with self.subTest(rate=bad), self.assertRaises(verifier.VerificationError):
                self.check(dict(base, target_stream_unresolved_inflight=bad))
        missing_rate = dict(base)
        del missing_rate["target_stream_unresolved_inflight"]
        with self.assertRaises(verifier.VerificationError):
            self.check(missing_rate)

    def test_lost_duplicate_and_impossible_cold_lifecycle_counts_are_rejected(self):
        base = self.checked([History(4, (2, 0, 0, 1), fp_mask=0b1010, release_gap=7)])
        corrupt = [("stream_recovery_inflight", index, value) for index, value in
                   ((0, 0), (0, 2), (1, 1), (2, 0), (2, 2), (3, 0), (3, 2),
                    (4, 0), (4, 2), (5, 0), (5, 2), (6, 0), (6, 5), (7, 2),
                    (8, 8), (9, 100), (10, 1))]
        corrupt += [("stream_recovery", 5, 4), ("stream_recovery", 5, 2),
                    ("stream_recovery", 7, 13), ("stream_recovery", 8, 12),
                    ("stream_recovery", 14, 0)]
        for index, (field, slot, value) in enumerate(corrupt):
            with self.subTest(corruption=index, field=field, slot=slot), self.assertRaises(verifier.VerificationError):
                self.check(self.replace(base, field, slot, value))
        one_round = self.checked([History(4, (0,))])
        with self.assertRaises(verifier.VerificationError):
            self.check(self.replace(one_round, "stream_recovery_inflight", 6, 2))
        unavailable = self.checked([History(2, (0,), selected=False, unavailable=True)])
        for index in range(2, 11):
            with self.subTest(unavailable_counter=index), self.assertRaises(verifier.VerificationError):
                self.check(self.replace(unavailable, "stream_recovery_inflight", index, 1))

    def test_cold_requires_an_older_warm_victim_and_complete_suffix(self):
        base = self.checked([History(2, (0,))])
        # Preserve old suffix equations for a last-load victim, but remove the
        # required warm victim. A disjoint youngest cold load cannot trigger it.
        impossible = dict(base, stream_recovery_victims="0,1,0,0")
        for field, index, value in (("stream_recovery", 7, 0), ("stream_recovery", 8, 1),
                                    ("scalar", 0, 2), ("load_ops", 0, 2)):
            impossible = self.replace(impossible, field, index, value)
        with self.assertRaises(verifier.VerificationError):
            self.check(impossible)
        # Keep canceled-vs-completed conservation while omitting a replayed
        # warm role. The victim still requires the entire two-load suffix.
        incomplete = self.replace(base, "stream_recovery", 7, 0)
        incomplete = self.replace(incomplete, "stream_recovery", 8, 1)
        with self.assertRaises(verifier.VerificationError):
            self.check(incomplete)

    def test_fp_cold_keeps_format_and_canceled_initial_is_not_strictly_checked(self):
        integer = self.checked([History(2, (0,))])
        all_fp = self.checked([History(2, (0,), fp_mask=3)])
        for report, claimed_fp in ((integer, 1), (all_fp, 0)):
            with self.subTest(claimed_fp=claimed_fp), self.assertRaises(verifier.VerificationError):
                self.check(self.replace(report, "stream_recovery_inflight", 7, claimed_fp))
        cold_fp = self.checked([History(2, (0,), fp_mask=2)])
        forged = cold_fp
        # Add the canceled initial cold FP response consistently to all strict
        # execution observations while retaining the architectural selection.
        for field, index in (("scalar", 0), ("scalar_fp", 1), ("scalar_fp_sign", 2),
                             ("scalar_fp_origin", 1), ("load_ops", 2)):
            forged = self.replace(forged, field, index, int(forged[field].split(",")[index]) + 1)
        with self.assertRaises(verifier.VerificationError):
            self.check(forged)
        # Counting canceled initial FP as an invalidated checked execution
        # must fail completed-versus-canceled conservation as well.
        with self.assertRaises(verifier.VerificationError):
            self.check(self.replace(cold_fp, "stream_recovery", 15, 1))


if __name__ == "__main__":
    unittest.main()
