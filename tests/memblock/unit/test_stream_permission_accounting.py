#!/usr/bin/env python3
"""Independent event histories for mixed permission accounting (host only)."""
from collections import Counter
import copy
import importlib.util
import json
from pathlib import Path
import random
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'scripts'))
import run_regression
import verify_regression

FIELDS = dict(stream_permission=22, stream_permission_stores=12, stream_access_epochs=9, stream=12,
              stream_cancel=29, stream_ops=5, stream_issue=6)


def csv(row):
    return ','.join(map(str, row))


def history(seed, epochs=8, rate=1000, mxr=500):
    """Generate owned events first, then independently reduce their outcomes."""
    rng = random.Random(seed)
    events = []
    for epoch in range(epochs):
        regime = rng.randrange(3)
        enabled = regime != 0 and rate != 0
        permitted = enabled and mxr != 0 and (mxr == 1000 or rng.randrange(2) == 0)
        events.append(('epoch', epoch, regime, permitted))
        for probe in range(rng.randrange(5) if enabled else 0):
            accepted = bool(rng.randrange(2))
            checked = accepted and bool(rng.randrange(2))
            outcome = 'cancel'
            if checked and rng.randrange(2):
                outcome = 'retire' if permitted else 'trap'
            prehead = checked and not permitted and bool(rng.randrange(2))
            older_store = prehead and bool(rng.randrange(2))
            suffix = [rng.randrange(5) for _ in range(rng.randrange(8))] if outcome == 'trap' else []
            events.append(('probe', epoch, permitted, accepted, checked, outcome,
                           prehead, older_store, bool(rng.randrange(2)), suffix))

    rows = {key: [0] * width for key, width in FIELDS.items()}
    p = rows['stream_permission']
    admitted_per_epoch = Counter()
    for event in events:
        if event[0] == 'epoch':
            _, epoch, regime, permitted = event
            rows['stream'][0] += 1
            rows['stream_access_epochs'][regime] += 1
            p[18] += permitted
            continue
        _, epoch, permitted, accepted, checked, outcome, prehead, older_store, draining, suffix = event
        admitted_per_epoch[epoch] += 1
        p[0] += 1
        p[2 if permitted else 1] += 1
        if accepted:
            p[4 if permitted else 3] += 1
            rows['stream_issue'][0] += 1
        if checked:
            p[21 if permitted else 5] += 1
        p[11] += prehead
        p[12] += older_store
        if outcome == 'retire':
            p[6] += 1
            rows['stream_ops'][0] += 1
        elif outcome == 'trap':
            p[7] += 1
            p[16] += 1
            p[13] += draining
            p[14] += bool(suffix)
            p[15] += len(suffix)
            # Independent before/after edge facts: an already-retired SQ
            # obligation can drain during return; public path busy may reflect
            # an unrelated younger SQ store and need not imply a retired store.
            retired = bool(rng.randrange(2)) or draining
            sq_at_redirect = retired and (draining or bool(rng.randrange(2)))
            path_at_redirect = bool(rng.randrange(2))
            path_at_return = bool(rng.randrange(2))
            stores = rows['stream_permission_stores']
            stores[0] += 1; stores[1] += retired; stores[2] += sq_at_redirect
            stores[3] += path_at_redirect; stores[4] += sq_at_redirect and path_at_redirect
            stores[5] += draining; stores[6] += path_at_return; stores[7] += 1
            # Select independent witness/terminal events before reduction.
            witness = retired and bool(rng.randrange(2))
            witness_canceled = witness and bool(rng.randrange(2))
            stores[8] += witness; stores[9] += witness
            stores[10] += witness and not witness_canceled; stores[11] += witness_canceled
            rows['stream_ops'][0] += witness and not witness_canceled
            rows['stream_cancel'][1] += witness_canceled
        else:
            p[9 if permitted else 8] += 1
            p[10] += checked and not permitted
        if outcome != 'retire':
            rows['stream_cancel'][0] += 1
            rows['stream_cancel'][1] += 1
            for kind in suffix:
                rows['stream_cancel'][1 + kind] += 1
    p[19] = max(admitted_per_epoch.values(), default=0)
    p[20] = len(admitted_per_epoch)
    p[17] = (p[5] + p[7] + 3) if p[1] else 0
    record = dict(constraint_schema=80, target_stream_permission_load=rate, target_stream_mxr=mxr, target_stream_arena=750)
    record.update({field: csv(row) for field, row in rows.items()})
    return record


class StreamPermissionAccountingTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temporary = tempfile.TemporaryDirectory(prefix='memblock-permission-accounting-')
        cls.addClassCleanup(cls.temporary.cleanup)
        source = Path(cls.temporary.name) / 'report.cpp'
        source.write_text(r'''
#include "scenarios/shared.hpp"
int main(int argc, char **argv) {
    auto constraints = memblock::scenarios::RandomConstraints::preset("spec");
    try {
        for (int i=1; i<argc; ++i) constraints.apply(argv[i]);
        constraints.validate();
    } catch (const std::invalid_argument &) { return 2; }
    std::string line;
    while (std::getline(std::cin, line)) {
        std::istringstream input(line);
        memblock::scenarios::MixedCoverage coverage;
        input >> constraints.stream_permission_load_per_mille >> constraints.stream_mxr_per_mille;
        auto read = [&](auto &row) { for (auto &value : row) input >> value; };
        // READ_ROWS
        std::string extra;
        if (input.fail() || input >> extra) return 1;
        std::cout << "MEMBLOCK_CONSTRAINED_RANDOM_PASS " << constraints.summary() << ' ' << coverage.summary() << '\n';
    }
}
'''.replace('// READ_ROWS', '\n'.join('read(coverage.' + field + ');' for field in FIELDS)))
        cls.binary = source.with_suffix('')
        built = subprocess.run(['c++', '-std=c++20', '-O0', '-I', str(ROOT / 'cpp'), str(source), '-o', str(cls.binary)],
                               capture_output=True, text=True, timeout=60)
        if built.returncode:
            raise AssertionError(built.stdout + built.stderr)

    def test_independent_histories_roundtrip_current_serializer_and_parser(self):
        records = [history(seed, epochs=seed % 9, rate=rate, mxr=mxr)
                   for seed in range(64) for rate, mxr in ((0, 0), (1000, 0), (1000, 500), (1000, 1000))]
        text = '\n'.join(f"{r['target_stream_permission_load']} {r['target_stream_mxr']} " +
                         ' '.join(r[field].replace(',', ' ') for field in FIELDS) for r in records) + '\n'
        ran = subprocess.run([str(self.binary)], input=text, capture_output=True, text=True, timeout=30)
        self.assertEqual(ran.returncode, 0, ran.stderr)
        reports = [run_regression.parse_summary(line) for line in ran.stdout.splitlines()]
        self.assertEqual(len(reports), len(records))
        for expected, report in zip(records, reports):
            for field in (*FIELDS, 'constraint_schema', 'target_stream_permission_load', 'target_stream_mxr', 'target_stream_arena'):
                self.assertEqual(report[field], expected[field], field)
            verify_regression._check_stream_permission(report)
        print('permission_accounting_serialized_histories=' + str(len(records)))

    def test_missing_malformed_overflow_and_outcome_corruptions_reject(self):
        base = history(42, epochs=8)
        verify_regression._check_stream_permission(base)
        corruptions = []
        for field in ('stream_permission', 'target_stream_permission_load', 'target_stream_mxr', 'target_stream_arena'):
            missing = dict(base); del missing[field]; corruptions.append(missing)
        for value in ('', '1,2', 'x,' * 21 + '1', '-1,' + ','.join(['0'] * 21)):
            corruptions.append(dict(base, stream_permission=value))
        for index in (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 21):
            row = list(map(int, base['stream_permission'].split(','))); row[index] += 10000
            corruptions.append(dict(base, stream_permission=csv(row)))
        overflow = [0] * 22; overflow[17] = 1 << 64
        corruptions.append(dict(base, stream_permission=csv(overflow)))
        for record in corruptions:
            with self.subTest(record=record):
                with self.assertRaises(verify_regression.VerificationError):
                    verify_regression._check_stream_permission(record)
        print('permission_accounting_rejected_corruptions=' + str(len(corruptions)))

    def test_legacy_and_disabled_endpoint_domains(self):
        verify_regression._check_stream_permission(dict(constraint_schema=78))
        old = history(3); old['constraint_schema'] = 79
        del old['stream_permission_stores']
        verify_regression._check_stream_permission(old)
        for key in ('stream-permission-load', 'stream-mxr', 'stream-arena'):
            for value in ('0', '1', '999', '1000', '1001', '-1', '1.5', '1000x', ''):
                ran = subprocess.run([str(self.binary), key + '=' + value], capture_output=True, text=True)
                self.assertEqual(ran.returncode, 0 if value in ('0', '1', '999', '1000') else 2, ran.stderr)
        for rate, mxr in ((0, 0), (0, 1000), (1000, 0), (1000, 1000)):
            verify_regression._check_stream_permission(history(3, rate=rate, mxr=mxr))

    def test_store_boundary_populations_require_owned_samples_and_intersections(self):
        base = history(42)
        verify_regression._check_stream_permission(base)
        for index in range(12):
            row = list(map(int, base['stream_permission_stores'].split(',')))
            row[index] += 10000
            with self.subTest(index=index), self.assertRaises(verify_regression.VerificationError):
                verify_regression._check_stream_permission(dict(base, stream_permission_stores=csv(row)))
        for value in ('', '1,2', '-1,' + ','.join(['0'] * 11), csv([1 << 64] * 12)):
            with self.subTest(value=value), self.assertRaises(verify_regression.VerificationError):
                verify_regression._check_stream_permission(dict(base, stream_permission_stores=value))
        missing = dict(base); del missing['stream_permission_stores']
        with self.assertRaises(verify_regression.VerificationError):
            verify_regression._check_stream_permission(missing)


if __name__ == '__main__':
    unittest.main()
