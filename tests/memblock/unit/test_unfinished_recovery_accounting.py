#!/usr/bin/env python3
"""Exercise schema66 accounting with independently enumerated byte histories."""
from __future__ import annotations

from dataclasses import dataclass
import hashlib
import itertools
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))
import run_regression  # noqa: E402
import verify_regression as verifier  # noqa: E402

FIELDS = {
    "scalar_unresolved": 11,
    "scalar_unresolved_graph": 9,
    "scalar_unresolved_first_conflict": 4,
    "scalar_unresolved_load_group": 11,
    "scalar_unresolved_victim": 4,
    "scalar_unresolved_epochs": 13,
    "scalar_unresolved_round_victim": 4,
    "scalar_unresolved_victim_movement": 3,
    "scalar_unresolved_inflight": 9,
}
PEAKS = {("scalar_unresolved_graph", 5), ("scalar_unresolved_load_group", 10),
         ("scalar_unresolved_epochs", 11)}
UINT64_MAX = (1 << 64) - 1


def csv(values):
    return ",".join(str(int(value)) for value in values)


def inversions(order):
    return sum(left > right for index, left in enumerate(order) for right in order[index + 1:])


@dataclass(frozen=True)
class ByteHistory:
    """Resolve older byte stores in age order against explicit input images.

    Warm byte loads have distinct offsets 0..warm_loads-1. An optional youngest
    eight-byte cold load reads offset 64 on another line and is initially canceled.
    Each store writes a unique nonzero byte. Address disclosure can therefore
    change its target warm byte but can never change the cold input image.
    """
    warm_loads: int
    store_offsets: tuple[int, ...]
    unfinished: bool = True
    unavailable: bool = False
    early_data: bool = False
    reverse_issue: bool = False
    cancellation_cap: int = 0
    cold_fp: bool = False
    release_gap: int = 0

    def counts(self):
        assert 1 <= self.warm_loads <= (3 if self.unfinished else 4)
        assert 1 <= len(self.store_offsets) <= 4
        assert all(0 <= offset < 8 for offset in self.store_offsets)
        assert not (self.unfinished and self.unavailable)
        assert self.unfinished or (not self.cold_fp and self.release_gap == 0)
        assert 0 <= self.release_gap <= 7 and self.cancellation_cap >= 0
        loads = [(age, 1) for age in range(self.warm_loads)]
        if self.unfinished:
            loads.append((64, 8))
        n, store_count = len(loads), len(self.store_offsets)
        memory = bytearray(128)
        previous = [bytes(memory[offset:offset + width]) for offset, width in loads]
        versions = [1] * n
        data_ready = [self.early_data] * store_count
        trained = set()
        victims = []
        movement = [0] * 3
        changed = unchanged = speculative = promoted = trained_hits = canceled = known_sum = replay_pairs = 0
        first_rank = None
        for store, offset in enumerate(self.store_offsets):
            memory[offset] = store + 1
            # A cold role is never a victim: every store belongs to the warm line.
            affected = [age for age, (address, width) in enumerate(loads)
                        if address <= offset < address + width]
            if not affected:
                continue
            victim = min(affected)
            if first_rank is None:
                first_rank = store
            if victims:
                movement[0 if victim < victims[-1] else 1 if victim == victims[-1] else 2] += 1
            victims.append(victim)
            trained.add(victim)
            for known in range(store + 1):
                if not data_ready[known]:
                    data_ready[known] = True
                    promoted += 1
            image = [bytes(memory[address:address + width]) for address, width in loads]
            assert previous[:victim] == image[:victim]
            suffix = list(range(victim, n))
            for age in suffix:
                versions[age] += 1
                changed += previous[age] != image[age]
                unchanged += previous[age] == image[age]
                speculative += store + 1 < store_count
                trained_hits += age in trained
                previous[age] = image[age]
            replay_pairs += inversions(suffix[::-1] if self.reverse_issue else suffix)
            observed = min(self.cancellation_cap, len(suffix))
            # The pending cold entry must contribute a positive first-round
            # LQ cancellation, independently of already-completed warm entries.
            canceled += max(1, observed) if self.unfinished and len(victims) == 1 else observed
            known_sum += store + 1
        rounds = len(victims)
        assert not (self.unfinished or self.unavailable) or rounds
        invalidated = [age for age, executions in enumerate(versions) if executions > 1]
        disjoint = sum(not any(address <= offset < address + width for offset in self.store_offsets)
                       for age in invalidated for address, width in [loads[age]])
        refetches = sum(executions - 1 for executions in versions)
        conflict = int(bool(rounds))
        earliest = min(victims, default=n)
        early = store_count if self.early_data else 0
        initial_order = list(range(n - 1, -1, -1)) if self.reverse_issue else list(range(n))
        return {
            "scalar_unresolved": (1, conflict, 1 - conflict, 1, conflict, conflict,
                conflict, 1, bool(early), conflict, canceled),
            "scalar_unresolved_graph": (store_count, store_count > 1, early,
                2 * store_count - early, 0, store_count, store_count if rounds else 0, 1,
                sum(left == right for index, left in enumerate(self.store_offsets)
                    for right in self.store_offsets[index + 1:])),
            "scalar_unresolved_first_conflict": tuple(int(first_rank == rank) for rank in range(4)),
            "scalar_unresolved_load_group": (n, n > 1, n - self.unfinished,
                n - len(invalidated), len(invalidated), len(invalidated), disjoint,
                len(invalidated) - disjoint, 0, inversions(initial_order), n),
            "scalar_unresolved_victim": tuple(int(rounds > 0 and earliest == age) for age in range(4)),
            "scalar_unresolved_epochs": (rounds, rounds > 1, refetches, refetches,
                sum(max(0, executions - 2) for executions in versions), speculative,
                changed, unchanged, known_sum, promoted, replay_pairs, rounds, trained_hits),
            "scalar_unresolved_round_victim": tuple(victims.count(age) for age in range(4)),
            "scalar_unresolved_victim_movement": tuple(movement),
            "scalar_unresolved_inflight": (self.unfinished or self.unavailable, self.unavailable,
                self.unfinished, self.unfinished, self.unfinished, self.unfinished,
                rounds if self.unfinished else 0, self.cold_fp, self.release_gap),
        }


def population(histories, *, rate=1000, conflict_rate=500, inflight_rate=250, schema=66, copies=1):
    observations = [history.counts() for history in histories]
    totals = {field: csv(max((row[field][index] for row in observations), default=0)
                         if (field, index) in PEAKS else
                         copies * sum(row[field][index] for row in observations)
                         for index in range(width))
              for field, width in FIELDS.items()}
    return dict(constraint_schema=schema, transactions=max(1, len(observations) * copies),
                target_scalar_unresolved=rate, target_scalar_unresolved_conflict=conflict_rate,
                target_scalar_unresolved_inflight=inflight_rate, **totals)


CPP = r'''
#include "memblock_env.hpp"
#include <numeric>
// PRODUCTION_MIXED_COVERAGE

int main() {
    std::string line;
    while (std::getline(std::cin, line)) {
        std::istringstream input(line);
        MixedCoverage coverage;
        std::array<std::uint64_t *, 11> scalar{
            &coverage.scalar_unresolved_windows, &coverage.scalar_unresolved_conflicts,
            &coverage.scalar_unresolved_controls, &coverage.scalar_unresolved_provisional,
            &coverage.scalar_unresolved_redirects, &coverage.scalar_unresolved_returned,
            &coverage.scalar_unresolved_refetched, &coverage.scalar_unresolved_readbacks,
            &coverage.scalar_unresolved_data_before, &coverage.scalar_unresolved_overlap_bytes,
            &coverage.scalar_unresolved_lq_canceled};
        for (auto *value : scalar) input >> *value;
        auto read = [&](auto &values) { for (auto &value : values) input >> value; };
        read(coverage.scalar_unresolved_graph);
        read(coverage.scalar_unresolved_first_conflict);
        read(coverage.scalar_unresolved_load_group);
        read(coverage.scalar_unresolved_victim);
        read(coverage.scalar_unresolved_epochs);
        read(coverage.scalar_unresolved_round_victim);
        read(coverage.scalar_unresolved_victim_movement);
        read(coverage.scalar_unresolved_inflight);
        std::string extra;
        if (input.fail() || input >> extra) return 1;
        std::istringstream summary(coverage.summary());
        std::cout << "MEMBLOCK_CONSTRAINED_RANDOM_PASS";
        std::string token;
        while (summary >> token) {
            if (token.starts_with("scalar_unresolved")) std::cout << ' ' << token;
        }
        std::cout << '\n';
    }
}
'''


class UnfinishedRecoveryAccountingTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source_paths = (ROOT / "cpp/random/coverage.inc", ROOT / "scripts/verify_regression.py")
        cls.before = cls._hashes()
        cls.workspace = tempfile.TemporaryDirectory(prefix="memblock-unfinished-accounting-")
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
            raise AssertionError("production coverage/verifier changed during accounting tests")

    def reports(self, expected):
        text = "\n".join(" ".join(str(value) for field in FIELDS
                                  for value in map(int, report[field].split(",")))
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

    def checked(self, histories, **options):
        result = self.reports([population(histories, **options)])[0]
        verifier._check_scalar_unresolved_accounting(result)
        return result

    @staticmethod
    def replace(result, field, index, value):
        values = result[field].split(",")
        values[index] = str(value)
        return dict(result, **{field: ",".join(values)})

    def test_realizable_store_targets_with_issue_data_and_cancellation_variants(self):
        expected = []
        observed_rounds, observed_victims, observed_gaps, observed_fp = set(), set(), set(), set()
        for warm in range(1, 4):
            for stores in range(1, 5):
                for targets in itertools.product(range(-1, warm), repeat=stores):
                    if all(target < 0 for target in targets):
                        continue
                    offsets = tuple(warm + index if target < 0 else target
                                    for index, target in enumerate(targets))
                    for early, reverse in itertools.product((False, True), repeat=2):
                        for cap in range(warm + 2):
                            ordinal = len(expected)
                            history = ByteHistory(warm, offsets, early_data=early,
                                reverse_issue=reverse, cancellation_cap=cap,
                                cold_fp=bool(ordinal % 2), release_gap=ordinal % 8)
                            row = population([history])
                            counts = history.counts()
                            observed_rounds.add(counts["scalar_unresolved_inflight"][6])
                            observed_victims.update(age for age, count in enumerate(
                                counts["scalar_unresolved_round_victim"]) if count)
                            observed_gaps.add(history.release_gap)
                            observed_fp.add(history.cold_fp)
                            expected.append(row)
        self.assertEqual(observed_rounds, {1, 2, 3, 4})
        self.assertEqual(observed_victims, {0, 1, 2})
        self.assertEqual(observed_gaps, set(range(8)))
        self.assertEqual(observed_fp, {False, True})
        self.assertEqual(len(expected), 8888)
        for index, report in enumerate(self.reports(expected)):
            try:
                verifier._check_scalar_unresolved_accounting(report)
            except verifier.VerificationError as error:
                self.fail(f"realizable unfinished history {index} rejected: {expected[index]}: {error}")

    def test_literal_four_round_suffixes_cancel_once_and_refetch_cold_every_round(self):
        result = self.checked([ByteHistory(3, (2, 0, 0, 1), reverse_issue=True,
            cancellation_cap=4, cold_fp=True, release_gap=7)])
        self.assertEqual(result["scalar_unresolved_inflight"], "1,0,1,1,1,1,4,1,7")
        self.assertEqual(result["scalar_unresolved_load_group"], "4,1,3,0,4,4,1,3,0,6,4")
        self.assertEqual(result["scalar_unresolved_epochs"], "4,1,13,13,9,10,4,9,10,4,16,4,7")
        self.assertEqual(result["scalar_unresolved_round_victim"], "2,1,1,0")
        self.assertEqual(result["scalar_unresolved_victim_movement"], "1,1,1")
        self.assertEqual(result["scalar_unresolved_victim"], "1,0,0,0")
        self.assertEqual(result["scalar_unresolved"], "1,1,0,1,1,1,1,1,0,1,13")
        self.assertEqual(result["scalar_unresolved_graph"], "4,1,0,8,0,4,4,1,1")

    def test_zero_sparse_disabled_unavailable_and_mixed_populations(self):
        for rate, conflicts, inflight in itertools.product((0, 1, 1000), repeat=3):
            with self.subTest(rate=rate, conflicts=conflicts, inflight=inflight):
                self.checked([], rate=rate, conflict_rate=conflicts, inflight_rate=inflight)
        controls = [ByteHistory(n, (n,), unfinished=False) for n in range(1, 5)]
        self.checked(controls, conflict_rate=0, inflight_rate=1000)
        completed = ByteHistory(4, (3, 2, 1, 0), unfinished=False, cancellation_cap=4)
        self.checked([completed], conflict_rate=1000, inflight_rate=0)
        unavailable = [ByteHistory(n, (n, 0), unfinished=False, unavailable=True,
                       early_data=bool(n % 2)) for n in range(1, 5)]
        only_unavailable = self.checked(unavailable, inflight_rate=1000)
        self.assertEqual(only_unavailable["scalar_unresolved_inflight"], "4,4,0,0,0,0,0,0,0")
        pending = ByteHistory(1, (0,), cold_fp=True)
        self.checked([pending], rate=1, conflict_rate=1, inflight_rate=1)
        mixed = self.checked(controls + unavailable + [completed, pending,
            ByteHistory(3, (2, 0, 1, 0), release_gap=7, cancellation_cap=4)])
        self.assertEqual(mixed["scalar_unresolved_inflight"], "6,4,2,2,2,2,5,1,7")
        for options in ({"inflight_rate": 0}, {"rate": 0}, {"conflict_rate": 0}):
            with self.subTest(options=options), self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting(population([pending], **options))
        with self.assertRaises(verifier.VerificationError):
            verifier._check_scalar_unresolved_accounting(population(unavailable, inflight_rate=0))

    def test_schema65_keeps_completed_initial_obligations_and_ignores_new_fields(self):
        histories = [ByteHistory(4, (2, 0, 0, 3), unfinished=False, cancellation_cap=4),
                     ByteHistory(1, (1,), unfinished=False)]
        report = self.checked(histories, schema=65)
        del report["scalar_unresolved_inflight"]
        del report["target_scalar_unresolved_inflight"]
        verifier._check_scalar_unresolved_accounting(report)
        verifier._check_scalar_unresolved_accounting(dict(report,
            scalar_unresolved_inflight="ignored legacy field", target_scalar_unresolved_inflight=None))
        with self.assertRaises(verifier.VerificationError):
            verifier._check_scalar_unresolved_accounting(population([ByteHistory(1, (0,))], schema=65))

    def test_uint64_boundary_populations_are_valid_without_overflow(self):
        # These are repetitions of realizable histories; peaks remain per-window.
        unavailable = ByteHistory(1, (0,), unfinished=False, unavailable=True, early_data=True)
        report = self.checked([unavailable], copies=UINT64_MAX, conflict_rate=1000)
        self.assertEqual(report["scalar_unresolved_inflight"], f"{UINT64_MAX},{UINT64_MAX},0,0,0,0,0,0,0")
        self.checked([ByteHistory(1, (0,), early_data=True, cold_fp=True)], copies=UINT64_MAX // 2)

    def test_missing_malformed_negative_and_overflow_inflight_counters(self):
        base = self.checked([ByteHistory(3, (2, 0, 0, 1), cold_fp=True, release_gap=7)])
        missing = dict(base)
        del missing["scalar_unresolved_inflight"]
        with self.assertRaises(verifier.VerificationError):
            verifier._check_scalar_unresolved_accounting(missing)
        for bad in (None, True, 0, [], "", "true", "1,2", csv([0] * 8), csv([0] * 10)):
            with self.subTest(bad=bad), self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting(dict(base, scalar_unresolved_inflight=bad))
        for index in range(9):
            for bad in (-1, UINT64_MAX + 1):
                with self.subTest(index=index, bad=bad), self.assertRaises(verifier.VerificationError):
                    verifier._check_scalar_unresolved_accounting(
                        self.replace(base, "scalar_unresolved_inflight", index, bad))
        for bad in (None, True, "250", -1, 1001, []):
            with self.subTest(rate=bad), self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting(dict(base, target_scalar_unresolved_inflight=bad))

    def test_lost_duplicate_and_impossible_inflight_observations_are_rejected(self):
        base = self.checked([ByteHistory(3, (2, 0, 0, 1), cold_fp=True, release_gap=7)])
        corrupt = [("scalar_unresolved_inflight", index, value) for index, value in
                   ((0, 0), (0, 2), (1, 1), (2, 0), (3, 0), (3, 2), (4, 0), (4, 2),
                    (5, 0), (5, 2), (6, 0), (6, 5), (7, 2), (8, 8))]
        corrupt += [("scalar_unresolved_load_group", 2, 4),
                    ("scalar_unresolved_load_group", 2, 2),
                    ("scalar_unresolved", 10, 0)]
        reports = [self.replace(base, field, index, value) for field, index, value in corrupt]
        # Preserve the old load-group conservation while removing the mandatory
        # disjoint invalidation; only the new cold-role relationship is broken.
        no_disjoint = self.replace(base, "scalar_unresolved_load_group", 6, 0)
        reports.append(self.replace(no_disjoint, "scalar_unresolved_load_group", 7, 4))
        # Preserve total refetched versions, but claim fewer unchanged inputs
        # than strict cold executions. Every cold input image is unchanged.
        too_few_unchanged = self.replace(base, "scalar_unresolved_epochs", 6, 10)
        reports.append(self.replace(too_few_unchanged, "scalar_unresolved_epochs", 7, 3))
        one_round = self.checked([ByteHistory(3, (0,))])
        reports.append(self.replace(one_round, "scalar_unresolved_inflight", 6, 2))
        for index, report in enumerate(self.reports(reports)):
            with self.subTest(corruption=index), self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting(report)
        unavailable = self.checked([ByteHistory(2, (0,), unfinished=False, unavailable=True)])
        for index in (6, 7, 8):
            with self.subTest(unavailable_counter=index), self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting(
                    self.replace(unavailable, "scalar_unresolved_inflight", index, 1))


if __name__ == "__main__":
    unittest.main()
