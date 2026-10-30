#!/usr/bin/env python3
"""Qualify schema78 access accounting with independent source event histories.

The ledger owns epoch context, admitted instruction identities, offered source
attempts, physical acceptances, completion, and ordered prefix retirement.
It does not call the producer or claim DUT/full-CPU reachability. A private host
helper exercises the current production constraints and coverage serializers.

MEMBLOCK_STREAM_ACCESS_SOURCE_ROOT selects an isolated production source tree.
MEMBLOCK_STREAM_ACCESS_ARTIFACTS retains the host helper and compilation log.
MEMBLOCK_STREAM_ACCESS_REPORT records commands, cases, and input hashes.
"""
from __future__ import annotations

from collections import Counter
import copy
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
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
SOURCE_ROOT = Path(os.environ.get("MEMBLOCK_STREAM_ACCESS_SOURCE_ROOT", ROOT)).resolve()
FIELDS = dict(stream=12, stream_transport=10, stream_issue_ports=16,
              stream_cancel=29, stream_recovery=20, stream_access_epochs=9,
              stream_access_issue=18, stream_access_overlap=6)
ACCESS = ("S-data", "U-data", "S-SUM-user-page")
REGIMES = ("Bare", "stage1", "nested")
CLASSES = ("scalar-load", "STA", "STD", "vector-load", "vector-store", "prefetch")
PORTS = dict(zip(CLASSES, (0, 1, 2, 3, 3, 0)))
UINT64_MAX = (1 << 64) - 1


def load_module(name):
    sys.path.insert(0, str(SOURCE_ROOT / "scripts"))
    spec = importlib.util.spec_from_file_location("stream_access_" + name,
                                                SOURCE_ROOT / "scripts" / (name + ".py"))
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


runner = load_module("run_regression")
verifier = load_module("verify_regression")
verifier.run_regression = runner


def csv(values):
    return ",".join(map(str, values))


def values(record, field):
    return list(map(int, record[field].split(",")))


def change(record, field, index, value):
    record = copy.deepcopy(record)
    row = values(record, field)
    row[index] = value
    record[field] = csv(row)
    return record


def move(record, field, source, destination):
    row = values(record, field)
    row[destination] += row[source]
    row[source] = 0
    return dict(record, **{field: csv(row)})


class SourceHistory:
    """Record source facts first; derive observations only when replayed."""

    def __init__(self):
        self.events = []
        self.next_owner = self.next_token = 0

    def epoch(self, access, regime):
        self.events.append(("epoch", access, regime))

    def admit(self):
        owner = self.next_owner
        self.next_owner += 1
        self.events.append(("admit", owner))
        return owner

    def offer(self, owner, *classes, category="initial"):
        tokens = tuple(range(self.next_token, self.next_token + len(classes)))
        self.next_token += len(classes)
        self.events.append(("offer", owner, classes, tokens, category))
        return tokens

    def edge(self, *tokens):
        self.events.append(("edge", tokens))

    def timeout(self, token):
        self.events.append(("timeout", token))

    def redirect(self, anchor):
        self.events.append(("redirect", anchor))

    def complete(self, owner):
        self.events.append(("complete", owner))

    def retire(self, owner):
        self.events.append(("retire", owner))

    def helper(self, *, refetch=False):
        self.events.append(("helper", refetch))

    def result(self, weights=(1, 1, 1), translation=(1, 1, 1)):
        rows = {field: [0] * width for field, width in FIELDS.items()}
        stream, transport = rows["stream"], rows["stream_transport"]
        live, completed, accepted, pending, stalled = [], set(), {}, {}, set()
        retries = 0
        access = None
        for event in self.events:
            if event[0] == "epoch":
                assert not live and not pending
                access, regime = ACCESS.index(event[1]), REGIMES.index(event[2])
                rows["stream_access_epochs"][3 * access + regime] += 1
                stream[0] += 1
            elif event[0] == "admit":
                owner = event[1]
                assert access is not None and owner not in live and len(live) < 8
                overlap = any(accepted.get(older) and older not in completed for older in live)
                rows["stream_access_overlap"][2 * access] += overlap
                stream[3] += overlap
                transport[6] += bool(stalled)
                live.append(owner)
                stream[1] += 1
                stream[6] = max(stream[6], len(live))
            elif event[0] == "offer":
                _, owner, classes, tokens, category = event
                assert owner in live and owner not in completed
                assert len(classes) == 1 or classes == ("STA", "STD")
                assert category in ("initial", "retry", "replay", "refetch")
                assert category != "retry" or classes == ("STA",)
                assert category != "replay" or classes == ("vector-load",)
                transport[9] += 1
                for kind, token in zip(classes, tokens):
                    assert kind in CLASSES and token not in pending
                    pending[token] = (owner, kind, category)
                    transport[0] += 1
                assert len(pending) <= 9
                transport[5] = max(transport[5], len(pending))
            elif event[0] == "edge":
                tokens = event[1]
                assert len(set(tokens)) == len(tokens)
                lanes = Counter(PORTS[pending[token][1]] for token in tokens)
                assert all(lanes[kind] <= limit for kind, limit in enumerate((3, 2, 2, 2)))
                if tokens:
                    rows["stream_issue_ports"][sum(1 << kind for kind in lanes)] += 1
                for token in tokens:
                    owner, kind, category = pending.pop(token)
                    accepted.setdefault(owner, []).append(token)
                    stalled.discard(token)
                    transport[1] += 1
                    rows["stream_access_issue"][6 * access + CLASSES.index(kind)] += 1
                    if category == "retry":
                        retries += 1
                    elif category == "replay":
                        stream[9] += 1
                    elif category == "refetch":
                        rows["stream_recovery"][8] += 1
                    else:
                        stream[8] += 1
                stalled.update(pending)
            elif event[0] == "timeout":
                token = event[1]
                assert PORTS[pending[token][1]] == 3
                del pending[token]
                stalled.discard(token)
                transport[2] += 1
            elif event[0] == "redirect":
                anchor = event[1]
                assert anchor in live and any(owner > anchor for owner in live)
                rows["stream_cancel"][0] += 1
                for token, (owner, _, _) in list(pending.items()):
                    if owner > anchor:
                        del pending[token]
                        stalled.discard(token)
                        transport[3] += 1
                for owner in list(accepted):
                    if owner > anchor:
                        transport[4] += len(accepted.pop(owner))
                live = [owner for owner in live if owner <= anchor]
            elif event[0] == "complete":
                owner = event[1]
                assert owner in live and accepted.get(owner) and owner not in completed
                assert not any(attempt[0] == owner for attempt in pending.values())
                completed.add(owner)
            elif event[0] == "retire":
                owner = event[1]
                assert live[0] == owner and owner in completed
                overlap = len(live) > 1
                rows["stream_access_overlap"][2 * access + 1] += overlap
                stream[5] += overlap
                transport[7] += any(pending[token][0] > owner for token in stalled)
                live.pop(0)
                stream[2] += 1
            elif event[0] == "helper":
                transport[8] += 1
                if event[1]:
                    rows["stream_recovery"][8] += 1
                else:
                    stream[8] += 1
            else:
                raise AssertionError(event)
        assert not live and not pending
        return dict(constraint_schema=78, target_stream_access=csv(weights),
                    target_translation=csv(translation), stream_scalar_retries=retries,
                    **{field: csv(row) for field, row in rows.items()})


def append_epoch(history, access, regime, *, rich=False):
    history.epoch(access, regime)
    head = history.admit()
    history.edge(*history.offer(head, "scalar-load"))
    live = [head]
    if rich:
        store = history.admit()
        live.append(store)
        history.edge(*history.offer(store, "STA", "STD"))
        history.edge(*history.offer(store, "STA", category="retry"))
        vector = history.admit()
        live.append(vector)
        first = history.offer(vector, "vector-load")[0]
        history.edge()
        history.timeout(first)
        history.edge(*history.offer(vector, "vector-load"))
        history.edge(*history.offer(vector, "vector-load", category="replay"))
        for kind in ("vector-store", "prefetch"):
            owner = history.admit()
            live.append(owner)
            history.edge(*history.offer(owner, kind))
        accepted_tail = history.admit()
        history.edge(*history.offer(accepted_tail, "scalar-load"))
        unaccepted_tail = history.admit()
        history.offer(unaccepted_tail, "STA", "STD")
        history.edge()
        history.redirect(live[-1])
        refetch = history.admit()
        live.append(refetch)
        history.edge(*history.offer(refetch, "scalar-load", category="refetch"))
        history.helper()
        history.helper(refetch=True)
    for owner in live:
        history.complete(owner)
        history.retire(owner)


def history(access="U-data", regime="stage1", *, rich=False, weights=(1, 1, 1),
            translation=(1, 1, 1)):
    source = SourceHistory()
    append_epoch(source, access, regime, rich=rich)
    return source.result(weights, translation)


def append_accepted_edge(source, access, counts, *, load="scalar-load", vector="vector-load"):
    """Observe one edge, pairing STA/STD sources on common store owners."""
    source.epoch(access, "stage1")
    owners, tokens = [], []
    paired = min(counts[1], counts[2])
    for _ in range(paired):
        owner = source.admit()
        owners.append(owner)
        tokens.extend(source.offer(owner, "STA", "STD"))
    for kind, count in enumerate(counts):
        for _ in range(count - (paired if kind in (1, 2) else 0)):
            owner = source.admit()
            owners.append(owner)
            tokens.extend(source.offer(owner, (load, "STA", "STD", vector)[kind]))
    source.edge(*tokens)
    for owner in owners:
        source.complete(owner)
        source.retire(owner)


class StreamAccessAccountingTest(unittest.TestCase):
    def checked(self, record):
        verifier._check_stream_transport(record)
        verifier._check_stream_access(record)
        return record

    def test_empty_sparse_disabled_and_all_profile_regime_histories(self):
        self.checked(SourceHistory().result())
        seen = set()
        count = 0
        for access in ACCESS:
            for regime in REGIMES:
                weights = tuple(int(value == access) for value in ACCESS)
                translation = tuple(int(value == regime) for value in REGIMES)
                self.checked(history(access, regime, weights=weights, translation=translation))
                self.checked(history(access, regime, rich=True))
                seen.add((access, regime))
                count += 2
        rng = random.Random(7801)
        for _ in range(256):
            weights = [rng.randrange(4) for _ in ACCESS]
            translation = [rng.randrange(4) for _ in REGIMES]
            if not any(weights):
                weights[rng.randrange(3)] = 1
            if not any(translation):
                translation[rng.randrange(3)] = 1
            source = SourceHistory()
            for _ in range(rng.randrange(9)):
                access = rng.choice([name for name, weight in zip(ACCESS, weights) if weight])
                regime = rng.choice([name for name, weight in zip(REGIMES, translation) if weight])
                append_epoch(source, access, regime, rich=bool(rng.randrange(2)))
            self.checked(source.result(weights, translation))
            count += 1
        self.assertEqual(len(seen), 9)
        print(f"stream_access_source_histories={count + 1}")

    def test_actual_acceptance_classes_include_retry_replay_refetch_exclude_helpers(self):
        source = SourceHistory()
        append_epoch(source, "U-data", "nested", rich=True)
        record = self.checked(source.result())
        self.assertEqual(values(record, "stream_access_issue")[6:12], [3, 2, 1, 2, 1, 1])
        self.assertEqual(values(record, "stream_access_overlap")[2:4], [7, 5])
        self.assertEqual(values(record, "stream_transport")[1:5], [10, 1, 2, 1])
        self.assertEqual(values(record, "stream_transport")[8], 2)
        for _ in range(500):
            source.helper()
            source.helper(refetch=True)
        helpers = self.checked(source.result())
        for field in ("stream_access_epochs", "stream_access_issue", "stream_access_overlap"):
            self.assertEqual(helpers[field], record[field], field)

    def test_actual_kind_masks_allow_each_lane_boundary_and_shared_classes(self):
        checked = 0
        rng = random.Random(7812)
        for mask in range(1, 16):
            for full in (False, True):
                counts = tuple((capacity if full else 1) if mask & (1 << kind) else 0
                               for kind, capacity in enumerate((3, 2, 2, 2)))
                for load in ("scalar-load", "prefetch"):
                    for vector in ("vector-load", "vector-store"):
                        source = SourceHistory()
                        append_accepted_edge(source, ACCESS[checked % 3], counts, load=load, vector=vector)
                        record = self.checked(source.result())
                        self.assertEqual(values(record, "stream_issue_ports")[mask], 1)
                        checked += 1
        for _ in range(64):
            source = SourceHistory()
            for access in ACCESS:
                counts = tuple(rng.randrange(capacity + 1) for capacity in (3, 2, 2, 2))
                if not any(counts):
                    counts = (1, 0, 0, 0)
                append_accepted_edge(source, access, counts,
                    load=rng.choice(("scalar-load", "prefetch")),
                    vector=rng.choice(("vector-load", "vector-store")))
            self.checked(source.result())
            checked += 1
        print(f"stream_access_kind_edge_histories={checked}")

    def test_class_corruptions_preserve_totals_and_global_transport_bounds(self):
        # Each case changes one class ledger only. The actual edge histogram,
        # total acceptances, other access rows, and architectural ledger remain
        # intact, including sufficient GLOBAL physical lane capacity.
        cases = (
            ((2, 0, 0, 0), 0, 1, 1),  # An absent STA kind acquires an acceptance.
            ((3, 2, 0, 0), 1, 0, 0),  # Load exceeds its three physical lanes.
            ((2, 2, 0, 0), 0, 1, 1),  # STA exceeds its two physical lanes.
            ((2, 0, 2, 0), 0, 2, 2),  # STD exceeds its two physical lanes.
            ((2, 0, 0, 2), 0, 3, 3),  # Vector exceeds its two physical lanes.
            ((1, 1, 0, 0), 0, 1, 0),  # A present load kind has no acceptance.
            ((1, 1, 0, 0), 1, 0, 1),  # A present STA kind has no acceptance.
            ((1, 0, 1, 0), 2, 0, 2),  # A present STD kind has no acceptance.
            ((1, 0, 0, 1), 3, 0, 3),  # A present vector kind has no acceptance.
        )
        for access, name in enumerate(ACCESS):
            for counts, donor, recipient, violated_kind in cases:
                source = SourceHistory()
                append_accepted_edge(source, name, counts)
                base = self.checked(source.result())
                issues = values(base, "stream_access_issue")
                issues[access * 6 + donor] -= 1
                issues[access * 6 + recipient] += 1
                bad = dict(base, stream_access_issue=csv(issues))
                self.assertEqual(sum(issues), sum(values(base, "stream_access_issue")))
                for unchanged in FIELDS.keys() - {"stream_access_issue"}:
                    self.assertEqual(bad[unchanged], base[unchanged], unchanged)
                verifier._check_stream_transport(bad)
                with self.subTest(access=name, counts=counts, donor=donor, recipient=recipient):
                    with self.assertRaisesRegex(verifier.VerificationError, f"stream access kind {violated_kind} acceptance"):
                        verifier._check_stream_access(bad)

    def test_conservation_faults_preserve_other_ledgers(self):
        base = self.checked(history(rich=True))
        faults = (("stream_access_epochs", "epoch count"),
                  ("stream_access_issue", "session acceptances"),
                  ("stream_access_overlap", "admissions/retirements"))
        for field, reason in faults:
            for index, count in enumerate(values(base, field)):
                for value in (count + 1, count - 1) if count else (1,):
                    bad = change(base, field, index, value)
                    for unchanged in FIELDS.keys() - {field}:
                        self.assertEqual(bad[unchanged], base[unchanged], unchanged)
                    verifier._check_stream_transport(bad)
                    with self.subTest(field=field, index=index, value=value):
                        with self.assertRaisesRegex(verifier.VerificationError, reason):
                            verifier._check_stream_access(bad)

    def test_same_total_wrong_profile_or_disabled_regime_rejects(self):
        base = self.checked(history(rich=True))
        for field, source, destination in (("stream_access_issue", 6, 0),
                                           ("stream_access_overlap", 2, 0),
                                           ("stream_access_overlap", 3, 1)):
            with self.subTest(field=field, source=source):
                bad = move(base, field, source, destination)
                verifier._check_stream_transport(bad)
                with self.assertRaisesRegex(verifier.VerificationError, "empty stream access profile 0"):
                    verifier._check_stream_access(bad)
        disabled = dict(base, target_stream_access="0,1,0")
        for field, source, destination in (("stream_access_epochs", 4, 1),
                                           ("stream_access_issue", 6, 0),
                                           ("stream_access_overlap", 2, 0)):
            with self.subTest(field=field), self.assertRaisesRegex(verifier.VerificationError, "disabled stream access profile 0"):
                verifier._check_stream_access(move(disabled, field, source, destination))
        stage1 = history(translation=(0, 1, 0))
        for destination in (3, 5):
            with self.assertRaisesRegex(verifier.VerificationError, "disabled translation regime"):
                verifier._check_stream_access(move(stage1, "stream_access_epochs", 4, destination))
        source = SourceHistory()
        append_epoch(source, "S-data", "Bare")
        append_epoch(source, "U-data", "nested")
        no_acceptance = move(source.result(), "stream_access_issue", 0, 6)
        with self.assertRaisesRegex(verifier.VerificationError, "nonempty stream access profile 0 has no session acceptances"):
            verifier._check_stream_access(no_acceptance)

    def test_uint64_domain_and_malformed_arrays_and_weights(self):
        base = history()
        maximum = copy.deepcopy(base)
        for field, index in (("stream_access_epochs", 4), ("stream_access_issue", 6),
                             ("stream", 0), ("stream", 1), ("stream", 2), ("stream", 8),
                             ("stream_transport", 0), ("stream_transport", 1),
                             ("stream_transport", 9), ("stream_issue_ports", 1)):
            maximum = change(maximum, field, index, UINT64_MAX)
        self.checked(maximum)
        for field, width in (("stream_access_epochs", 9), ("stream_access_issue", 18),
                             ("stream_access_overlap", 6), ("target_stream_access", 3)):
            for value in (None, True, [], 0, "", "x", "True,1,1", "1.0,1,1",
                          csv([0] * (width - 1)), csv([0] * (width + 1)),
                          csv([-1] + [0] * (width - 1))):
                with self.subTest(field=field, value=value), self.assertRaises(verifier.VerificationError):
                    verifier._check_stream_access(dict(base, **{field: value}))
            missing = dict(base)
            del missing[field]
            with self.subTest(missing=field), self.assertRaises(verifier.VerificationError):
                verifier._check_stream_access(missing)
            for index in range(width):
                limit = (1 << 32) - 1 if field == "target_stream_access" else UINT64_MAX
                reason = "uint32" if field == "target_stream_access" else "uint64"
                with self.subTest(field=field, index=index), self.assertRaisesRegex(verifier.VerificationError, reason):
                    verifier._check_stream_access(change(base, field, index, limit + 1))
        with self.assertRaisesRegex(verifier.VerificationError, "nonempty uint32"):
            verifier._check_stream_access(dict(base, target_stream_access="0,0,0"))
        verifier._check_stream_access(dict(base, target_stream_access=csv([(1 << 32) - 1] * 3)))
        # Summation uses unbounded arithmetic, so two maxima cannot wrap to a
        # matching small acceptance total.
        with self.assertRaisesRegex(verifier.VerificationError, "session acceptances"):
            verifier._check_stream_access(change(maximum, "stream_access_issue", 7, UINT64_MAX))

    def test_schema77_compatibility_and_both_checker_entries(self):
        for schema in range(78):
            verifier._check_stream_access(dict(constraint_schema=schema,
                target_stream_access=True, stream_access_epochs=None,
                stream_access_issue=-1, stream_access_overlap=[]))
        base = history()
        verifier._check_mixed_accounting(dict(base, lq="1+0/1", sq="0+0/0"))
        for checker in (verifier._check_mixed_accounting, verifier._check_mixed_coverage):
            missing = dict(base)
            del missing["stream_access_issue"]
            for bad, reason in ((missing, "stream_access_issue"),
                                (change(base, "stream_access_epochs", 4, 2), "epoch count")):
                with mock.patch.object(verifier, "_check_constraint_coverage"), self.subTest(mode=checker.__name__), self.assertRaisesRegex(verifier.VerificationError, reason):
                    checker(bad)
        with self.assertRaisesRegex(verifier.VerificationError, "target_translation"):
            verifier._check_constraint_coverage(dict(constraint_schema=78))
        old = dict(base, constraint_schema=77, lq="1+0/1", sq="0+0/0")
        for field in ("target_stream_access", "stream_access_epochs", "stream_access_issue", "stream_access_overlap"):
            del old[field]
        verifier._check_mixed_accounting(old)


CPP = r'''
#include "scenarios/shared.hpp"
int main()
{
    std::string line;
    while (std::getline(std::cin, line)) {
        std::istringstream input(line);
        auto constraints = memblock::scenarios::RandomConstraints::preset("coverage");
        memblock::scenarios::MixedCoverage coverage;
        auto read = [&](auto &row) { for (auto &value : row) input >> value; };
        read(constraints.stream_access_weights); read(constraints.translation_weights);
        // READ_COVERAGE
        input >> coverage.stream_scalar_retries;
        std::string extra;
        if (input.fail() || input >> extra) return 1;
        std::cout << "MEMBLOCK_CONSTRAINED_RANDOM_PASS " << constraints.summary()
                  << ' ' << coverage.summary() << '\n';
    }
}
'''.replace("// READ_COVERAGE", "\n".join(f"read(coverage.{field});" for field in FIELDS))


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


class StreamAccessSerializationTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.records = []
        cls.hashes = {str(SOURCE_ROOT / path): digest(SOURCE_ROOT / path) for path in
                      ("cpp/random/constraints.inc", "cpp/random/coverage.inc",
                       "scripts/run_regression.py", "scripts/verify_regression.py")}
        artifacts = os.environ.get("MEMBLOCK_STREAM_ACCESS_ARTIFACTS")
        if artifacts:
            Path(artifacts).mkdir(parents=True, exist_ok=True)
            cls.work = Path(tempfile.mkdtemp(prefix="host-", dir=artifacts))
        else:
            temporary = tempfile.TemporaryDirectory(prefix="memblock-stream-access-")
            cls.addClassCleanup(temporary.cleanup)
            cls.work = Path(temporary.name)
        source = cls.work / "report.cpp"
        source.write_text(CPP)
        cls.binary = source.with_suffix("")
        dependencies = cls.work / "report.d"
        cls.command = ["c++", "-std=c++20", "-O0", "-MMD", "-MF", str(dependencies),
                       "-I", str(SOURCE_ROOT / "cpp"), str(source), "-o", str(cls.binary)]
        built = subprocess.run(cls.command, capture_output=True, text=True, timeout=60)
        (cls.work / "compile.log").write_text(built.stdout + built.stderr)
        if built.returncode:
            raise AssertionError("production host reporter did not compile:\n" + built.stdout + built.stderr)
        for name in shlex.split(dependencies.read_text().replace("\\\n", " ").split(":", 1)[1]):
            path = Path(name).resolve()
            if path != source:
                cls.hashes.setdefault(str(path), digest(path))
        cls.addClassCleanup(cls.finalize)

    @classmethod
    def finalize(cls):
        after = {path: digest(path) for path in cls.hashes}
        destination = os.environ.get("MEMBLOCK_STREAM_ACCESS_REPORT")
        if destination:
            Path(destination).write_text(json.dumps(dict(source_root=str(SOURCE_ROOT),
                compile_command=cls.command, helper_source=str(cls.work / "report.cpp"),
                hashes_before=cls.hashes, hashes_after=after, cases=cls.records), indent=2) + "\n")
        if cls.hashes != after:
            raise AssertionError("production inputs changed during access accounting tests; refresh the snapshot")

    def test_current_serialization_roundtrip_retains_profile_class_units(self):
        expected = [SourceHistory().result()]
        for index, access in enumerate(ACCESS):
            for regime in REGIMES:
                weights = tuple((index + 2) if value == access else 0 for value in ACCESS)
                translation = tuple(int(value == regime) for value in REGIMES)
                expected.append(history(access, regime, rich=True, weights=weights, translation=translation))
        source = SourceHistory()
        for index, access in enumerate(ACCESS):
            for _ in range(index + 1):
                append_epoch(source, access, REGIMES[index], rich=bool(index % 2))
        expected.append(source.result((2, 7, 1), (3, 1, 9)))
        text = "\n".join(" ".join(str(value) for field in
            ("target_stream_access", "target_translation", *FIELDS)
            for value in values(record, field)) + f" {record['stream_scalar_retries']}"
            for record in expected) + "\n"
        ran = subprocess.run([str(self.binary)], input=text, capture_output=True, text=True, timeout=30)
        self.assertEqual(ran.returncode, 0, ran.stdout[:1000] + ran.stderr)
        actual = [runner.parse_summary(line) for line in ran.stdout.splitlines()]
        self.assertEqual(len(actual), len(expected))
        for original, emitted in zip(expected, actual):
            self.assertEqual(emitted["constraint_schema"], 79)
            for field in ("target_stream_access", "target_translation", *FIELDS, "stream_scalar_retries"):
                self.assertEqual(emitted[field], original[field], field)
            verifier._check_stream_transport(emitted)
            verifier._check_stream_access(emitted)
        self.records.append(dict(test=self.id(), command=[str(self.binary)],
                                 serialized_rows=len(actual), returncode=ran.returncode))


if __name__ == "__main__":
    unittest.main()
