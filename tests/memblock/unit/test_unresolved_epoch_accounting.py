#!/usr/bin/env python3
"""Check compiled schema65 reports against explicit byte-store/load histories."""
from __future__ import annotations

from dataclasses import dataclass
import hashlib
import itertools
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

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
}
PEAKS = {("scalar_unresolved_graph", 5), ("scalar_unresolved_load_group", 10),
         ("scalar_unresolved_epochs", 11)}
EPOCH_FIELDS = tuple(FIELDS)[-3:]


def csv(values):
    return ",".join(str(int(value)) for value in values)


def inversions(order):
    return sum(older > younger for i, older in enumerate(order) for younger in order[i + 1:])


@dataclass(frozen=True)
class History:
    """All stores are older byte stores; all distinct byte loads complete first.

    STAs resolve in program age, then any remaining STD events occur. A recovery
    pulls pending known-store STD ahead of its original event. Every refetch is
    completed before the next STA. Initial bytes are zero and store i writes
    i+1, so every newly visible writer differs without consulting DUT state.
    """
    loads: int
    store_offsets: tuple[int, ...]
    early_std: bool = False
    reverse_issue: bool = False
    cancellation_cap: int = 0

    def counts(self):
        n, stores = self.loads, self.store_offsets
        assert 1 <= n <= 4 and 1 <= len(stores) <= 4
        assert all(0 <= offset < 8 for offset in stores)
        current = [0] * n
        executions = [1] * n
        data_ready = [self.early_std] * len(stores)
        known = set()
        trained = set()
        round_victims, movements = [0] * 4, [0] * 3
        victims = []
        canceled = changed = unchanged = speculative = known_sum = promoted = trained_hits = 0
        replay_inversions = 0
        first_rank = None
        for index, offset in enumerate(stores):
            known.add(index)
            # Assemble the independent speculative byte image by store age.
            image = bytearray(64)
            for store in sorted(known):
                image[stores[store]] = store + 1
            affected = [age for age in range(n) if age == offset]
            if not affected:
                continue
            victim = affected[0]
            if first_rank is None:
                first_rank = index
            if victims:
                previous = victims[-1]
                movements[0 if victim < previous else 1 if victim == previous else 2] += 1
            victims.append(victim)
            round_victims[victim] += 1
            trained.add(victim)
            for store in known:
                if not data_ready[store]:
                    data_ready[store] = True
                    promoted += 1
            suffix = list(range(victim, n))
            assert all(current[age] == image[age] for age in range(victim))
            for age in suffix:
                executions[age] += 1
                changed += current[age] != image[age]
                unchanged += current[age] == image[age]
                current[age] = image[age]
                speculative += len(known) < len(stores)
                trained_hits += age in trained
            order = suffix[::-1] if self.reverse_issue else suffix
            replay_inversions += inversions(order)
            canceled += min(self.cancellation_cap, len(suffix))
            known_sum += len(known)
        rounds = len(victims)
        invalidated = [age for age, count in enumerate(executions) if count > 1]
        disjoint = sum(age not in stores for age in invalidated)
        refetches = sum(count - 1 for count in executions)
        conflicts = int(bool(rounds))
        initial_order = list(reversed(range(n))) if self.reverse_issue else list(range(n))
        earliest = min(victims, default=n)
        early = len(stores) if self.early_std else 0
        return {
            "scalar_unresolved": (1, conflicts, 1 - conflicts, 1, conflicts, conflicts,
                                  conflicts, 1, bool(early), conflicts, canceled),
            "scalar_unresolved_graph": (len(stores), len(stores) > 1, early,
                2 * len(stores) - early, 0, len(stores), len(stores) if rounds else 0,
                1, sum(a == b for i, a in enumerate(stores) for b in stores[i + 1:])),
            "scalar_unresolved_first_conflict": tuple(int(first_rank == rank) for rank in range(4)),
            "scalar_unresolved_load_group": (n, n > 1, n, n - len(invalidated),
                len(invalidated), len(invalidated), disjoint, len(invalidated) - disjoint,
                0, inversions(initial_order), n),
            "scalar_unresolved_victim": tuple(int(rounds > 0 and earliest == age) for age in range(4)),
            "scalar_unresolved_epochs": (rounds, rounds > 1, refetches, refetches,
                sum(max(0, count - 2) for count in executions), speculative, changed,
                unchanged, known_sum, promoted, replay_inversions, rounds, trained_hits),
            "scalar_unresolved_round_victim": tuple(round_victims),
            "scalar_unresolved_victim_movement": tuple(movements),
        }


def population(histories, *, rate=1000, conflict_rate=500, schema=65):
    observations = [history.counts() for history in histories]
    totals = {}
    for field, width in FIELDS.items():
        totals[field] = csv(max((row[field][index] for row in observations), default=0)
                            if (field, index) in PEAKS
                            else sum(row[field][index] for row in observations)
                            for index in range(width))
    return dict(constraint_schema=schema, transactions=max(1, len(observations)),
                target_scalar_unresolved=rate, target_scalar_unresolved_conflict=conflict_rate,
                **totals)


CPP = r'''
#include "memblock_env.hpp"
#include <numeric>
// PRODUCTION_MIXED_COVERAGE

static void read_counts(std::istream &input, auto &values) {
    for (auto &value : values) input >> value;
}
int main() {
    try {
        std::string line;
        while (std::getline(std::cin, line)) {
            std::istringstream input(line);
            char mode = 0;
            input >> mode;
            MixedCoverage coverage;
            if (mode == 'U') {
                std::array<std::uint64_t *, 11> scalar{
                    &coverage.scalar_unresolved_windows, &coverage.scalar_unresolved_conflicts,
                    &coverage.scalar_unresolved_controls, &coverage.scalar_unresolved_provisional,
                    &coverage.scalar_unresolved_redirects, &coverage.scalar_unresolved_returned,
                    &coverage.scalar_unresolved_refetched, &coverage.scalar_unresolved_readbacks,
                    &coverage.scalar_unresolved_data_before, &coverage.scalar_unresolved_overlap_bytes,
                    &coverage.scalar_unresolved_lq_canceled};
                for (auto *value : scalar) input >> *value;
                read_counts(input, coverage.scalar_unresolved_graph);
                read_counts(input, coverage.scalar_unresolved_first_conflict);
                read_counts(input, coverage.scalar_unresolved_load_group);
                read_counts(input, coverage.scalar_unresolved_victim);
                read_counts(input, coverage.scalar_unresolved_epochs);
                read_counts(input, coverage.scalar_unresolved_round_victim);
                read_counts(input, coverage.scalar_unresolved_victim_movement);
            } else if (mode == 'F') {
                unsigned origin = 0, executions = 0;
                input >> origin >> executions;
                coverage.sample_scalar_format_choice(origin, true);
                memblock::LoadTransaction load;
                load.op = memblock::LoadOp::ld;
                load.fp_wen = true;
                for (unsigned execution = 0; execution < executions; ++execution) {
                    coverage.sample(load);
                    coverage.sample_fp_load(load, 17, origin, 0, origin >= 2 ? 8 : 0);
                }
            } else throw std::runtime_error("unknown report fixture mode");
            if (input.fail()) throw std::runtime_error("incomplete report fixture input");
            // Use the actual complete reporter, retaining only the fields under test
            // so exhaustive populations do not create megabytes of unrelated bins.
            std::istringstream summary(coverage.summary());
            std::cout << "MEMBLOCK_CONSTRAINED_RANDOM_PASS";
            std::string token;
            while (summary >> token) {
                if (token.starts_with("scalar_unresolved") || token.starts_with("scalar_fp") ||
                    token.starts_with("scalar_format_choices=") || token.starts_with("scalar=") ||
                    token.starts_with("load_ops=")) std::cout << ' ' << token;
            }
            std::cout << '\n';
        }
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
'''


class UnresolvedEpochAccountingTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source_paths = (ROOT / "cpp/random/coverage.inc", ROOT / "scripts/verify_regression.py")
        cls.before = cls._hashes()
        cls.workspace = tempfile.TemporaryDirectory(prefix="memblock-unresolved-epochs-")
        cls.addClassCleanup(cls.workspace.cleanup)
        cls.addClassCleanup(cls._unchanged_sources)
        folder = Path(cls.workspace.name)
        production = (ROOT / "cpp/random/coverage.inc").read_text()
        mixed = "struct MixedCoverage {" + production.split("struct MixedCoverage {", 1)[1].split("\nstruct ", 1)[0]
        source = folder / "report.cpp"
        source.write_text(CPP.replace("// PRODUCTION_MIXED_COVERAGE", mixed))
        cls.binary = folder / "report"
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
        lines = ["U " + " ".join(str(value) for field in FIELDS
                                 for value in map(int, report[field].split(",")))
                 for report in expected]
        actual = self.run_reporter("\n".join(lines) + "\n")
        self.assertEqual(len(actual), len(expected))
        for original, report in zip(expected, actual):
            for field in FIELDS:
                self.assertEqual(report[field], original[field], field)
            report.update({key: value for key, value in original.items() if key not in FIELDS})
        return actual

    def run_reporter(self, text):
        ran = subprocess.run([str(self.binary)], input=text, capture_output=True,
                             text=True, timeout=30)
        self.assertEqual(ran.returncode, 0, ran.stdout[:1000] + ran.stderr)
        return [run_regression.parse_summary(line) for line in ran.stdout.splitlines()]

    def checked_population(self, histories, **options):
        result = self.reports([population(histories, **options)])[0]
        verifier._check_scalar_unresolved_accounting(result)
        return result

    def test_all_byte_store_histories_with_data_issue_and_cancellation_variants(self):
        expected = []
        # 1,270 address histories; two STD schedules, two issue orders, and
        # caps 0..N exercise every possible per-round cancellation observation.
        for n in range(1, 5):
            for store_count in range(1, 5):
                for targets in itertools.product(range(-1, n), repeat=store_count):
                    offsets = tuple(n + index if age < 0 else age for index, age in enumerate(targets))
                    for early, reverse in itertools.product((False, True), repeat=2):
                        for cap in range(n + 1):
                            expected.append(population([History(n, offsets, early, reverse, cap)]))
        self.assertEqual(len(expected), 22720)
        for index, report in enumerate(self.reports(expected)):
            try:
                verifier._check_scalar_unresolved_accounting(report)
            except verifier.VerificationError as error:
                self.fail(f"realizable history {index} rejected: {expected[index]}: {error}")

    def test_literal_four_round_suffix_versions_and_earliest_victim(self):
        report = self.checked_population([History(4, (2, 0, 0, 3), reverse_issue=True, cancellation_cap=4)])
        self.assertEqual(report["scalar_unresolved_epochs"], "4,1,11,11,7,10,4,7,10,4,13,4,6")
        self.assertEqual(report["scalar_unresolved_round_victim"], "2,0,1,1")
        self.assertEqual(report["scalar_unresolved_victim_movement"], "1,1,1")
        self.assertEqual(report["scalar_unresolved_victim"], "1,0,0,0")
        self.assertEqual(report["scalar_unresolved_load_group"], "4,1,4,0,4,4,1,3,0,6,4")
        self.assertEqual(report["scalar_unresolved"], "1,1,0,1,1,1,1,1,0,1,11")
        self.assertEqual(report["scalar_unresolved_graph"], "4,1,0,8,0,4,4,1,1")

    def test_zero_sparse_endpoint_controls_and_mixed_populations(self):
        self.checked_population([], rate=0, conflict_rate=0)
        self.checked_population([], conflict_rate=1000)
        controls = [History(n, tuple(n + index for index in range(s)), early_std=bool(s % 2))
                    for n in range(1, 5) for s in range(1, 5)]
        self.checked_population(controls, conflict_rate=0)
        conflicts = [History(4, (3, 2, 1, 0), cancellation_cap=4), History(1, (0,)),
                     History(3, (0, 0, 0, 0), early_std=True)]
        self.checked_population(conflicts, conflict_rate=1000)
        self.checked_population(controls + conflicts)
        for histories, options in ((controls, {"conflict_rate": 1000}),
                                   (conflicts, {"conflict_rate": 0}),
                                   (controls, {"rate": 0})):
            with self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting(population(histories, **options))

    def test_schema64_reports_need_no_epoch_fields(self):
        report = self.checked_population([History(4, (4, 2, 6, 7), cancellation_cap=4)], schema=64)
        for field in EPOCH_FIELDS:
            del report[field]
        verifier._check_scalar_unresolved_accounting(report)
        # Schema64 still permits only one unresolved-FP refetch, checked below.

    def test_aggregate_peak_needs_a_real_window_and_round_distribution(self):
        # Six rounds split across three multi-round windows cannot include a
        # three-round window: each of the other windows owns at least two.
        histories = [History(4, (0, 1, 6, 7)), History(2, (1, 0, 4, 5)),
                     History(3, (0, 2, 5, 6))]
        base = self.checked_population(histories)
        self.assertEqual(base["scalar_unresolved_epochs"].split(",")[:2], ["6", "3"])
        for peak in (3, 4):
            values = base["scalar_unresolved_epochs"].split(",")
            values[11] = str(peak)
            with self.subTest(peak=peak), self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting(dict(base, scalar_unresolved_epochs=",".join(values)))

    def test_missing_malformed_negative_and_overflow_epoch_observations(self):
        base = self.checked_population([History(4, (2, 0, 0, 3))])
        for field in EPOCH_FIELDS:
            missing = dict(base)
            del missing[field]
            with self.subTest(field=field, missing=True), self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting(missing)
            width = FIELDS[field]
            for bad in (None, True, 0, [], "", "true", "1,2", csv([0] * (width + 1)),
                        csv([-1] + [0] * (width - 1))):
                with self.subTest(field=field, bad=bad), self.assertRaises(verifier.VerificationError):
                    verifier._check_scalar_unresolved_accounting(dict(base, **{field: bad}))
            for index in range(width):
                values = base[field].split(",")
                values[index] = str(1 << 64)
                with self.subTest(field=field, overflow=index), self.assertRaises(verifier.VerificationError):
                    verifier._check_scalar_unresolved_accounting(dict(base, **{field: ",".join(values)}))

    def test_lost_duplicate_and_impossible_round_observations_are_rejected(self):
        base = self.checked_population([History(4, (2, 0, 0, 3), reverse_issue=True, cancellation_cap=4)])
        corrupt_epochs = ((0, 0), (1, 0), (2, 10), (3, 10), (4, 6), (5, 0), (5, 12),
                          (6, 0), (7, 8), (8, 4), (8, 17), (9, 5), (10, 17), (11, 5), (12, 0), (12, 12))
        corrupt = [("scalar_unresolved_epochs", index, value) for index, value in corrupt_epochs]
        corrupt += [("scalar_unresolved_round_victim", 0, 1),
                    ("scalar_unresolved_victim_movement", 1, 0),
                    ("scalar_unresolved", 10, 12),
                    ("scalar_unresolved", 6, 4),
                    ("scalar_unresolved_load_group", 4, 11)]
        for field, index, value in corrupt:
            values = base[field].split(",")
            values[index] = str(value)
            with self.subTest(field=field, index=index, value=value), self.assertRaises(verifier.VerificationError):
                verifier._check_scalar_unresolved_accounting(dict(base, **{field: ",".join(values)}))
        small = self.checked_population([History(2, (0, 1))])
        with self.assertRaises(verifier.VerificationError):
            verifier._check_scalar_unresolved_accounting(dict(small, scalar_unresolved_round_victim="1,0,1,0"))

    def test_compiled_fp_selected_instruction_execution_bound_and_legacy_limit(self):
        reports = self.run_reporter("".join(f"F {origin} {executions}\n"
                                           for origin in range(4) for executions in range(1, 7)))
        for origin in range(4):
            for executions in range(1, 7):
                actual = reports[origin * 6 + executions - 1]
                base = dict(actual, constraint_schema=65, target_scalar_fp=500,
                            target_scalar_fp_widths="1,1,1")
                self.assertEqual(base["scalar_fp_origin"], csv(executions if item == origin else 0 for item in range(4)))
                self.assertEqual(base["scalar_fp"], f"0,0,{executions}")
                with self.subTest(origin=origin, executions=executions, schema=65):
                    if executions <= (5 if origin == 3 else 1):
                        verifier._check_scalar_fp_accounting(base)
                    else:
                        with self.assertRaises(verifier.VerificationError):
                            verifier._check_scalar_fp_accounting(base)
                legacy = dict(base, constraint_schema=64)
                with self.subTest(origin=origin, executions=executions, schema=64):
                    if executions <= (2 if origin == 3 else 1):
                        verifier._check_scalar_fp_accounting(legacy)
                    else:
                        with self.assertRaises(verifier.VerificationError):
                            verifier._check_scalar_fp_accounting(legacy)


if __name__ == "__main__":
    unittest.main()
