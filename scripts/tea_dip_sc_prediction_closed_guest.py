"""Audit speculative saturated-SC opposition opportunities without training-selection filters."""
from collections import Counter, defaultdict
from pathlib import Path
import argparse
import heapq
import json
import sqlite3
from native_perf import read_blocks, unique_counters
from tea_dip_query_endpoint_evidence import digest
from tea_dip_sc_closed_guest import analyze as selected_analysis

COLUMNS = ['ID', 'HIGHTHRESHOLD', 'SUMMAGNITUDE', 'USESC', 'SCPRED', 'PROVIDERCTR',
           'CFIPOSITION', 'STARTPC_ADDR', 'STAMP', 'SITE']


def analyze(folder, receipt_path):
    # Preserve all existing closed-guest, file-integrity and selected-trace checks.
    selected_analysis(folder, receipt_path)
    command = json.loads((folder / 'command.json').read_text())
    selection = command[command.index('--dump-select-db') + 1].split()
    tables = sorted((name for name in selection if name.startswith('scPredictionOpposition_')),
                    key=lambda name: int(name.rsplit('_', 1)[1]))
    assert tables and tables == ['scPredictionOpposition_' + str(i) for i in range(len(tables))]
    blocks = sorted(read_blocks(folder / 'simulator_err.txt').items())
    phases = []
    table_counts = {}
    with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        for table in tables:
            assert [row['name'] for row in db.execute('PRAGMA table_info(' + table + ')')] == COLUMNS

        def rows(lane, table):
            previous = -1
            table_counts[table] = 0
            for ordinal, row in enumerate(db.execute('SELECT * FROM ' + table + ' ORDER BY ID'), 1):
                assert row['ID'] == ordinal and row['STAMP'] > previous
                previous = row['STAMP']
                assert row['SCPRED'] in [0, 1] and row['USESC'] in [0, 1]
                assert row['PROVIDERCTR'] in [0, 7]
                assert row['SCPRED'] != int(row['PROVIDERCTR'] >= 4)
                assert row['SUMMAGNITUDE'] > row['HIGHTHRESHOLD'] >= 0
                table_counts[table] = ordinal
                yield row['STAMP'], lane, row

        merged = iter(heapq.merge(*[rows(i, table) for i, table in enumerate(tables)]))
        pending = next(merged, None)
        labels = ['warmup', 'ROI'] if len(blocks) == 2 else ['coremark']
        for label, (stamp, block) in zip(labels, blocks):
            native = unique_counters(block)
            amount, = [value for name, value in native.items()
                       if name.endswith('.sc.diag_sc_prediction_opposition_events')]
            counts = Counter(rows=0, useSc=0, policy_suppressed=0, within_2x_threshold=0, above_2x_threshold=0)
            locations = defaultdict(Counter)
            first = last = None
            for _ in range(amount):
                assert pending is not None, 'Native prediction count exceeds recorded opportunities'
                cycle, lane, row = pending
                first = cycle if first is None else first
                last = cycle
                margin = 'within_2x_threshold' if row['SUMMAGNITUDE'] <= 2 * row['HIGHTHRESHOLD'] else 'above_2x_threshold'
                change = dict(rows=1, useSc=row['USESC'], policy_suppressed=1-row['USESC'])
                change[margin] = 1
                counts.update(change)
                locations[(row['STARTPC_ADDR'], row['CFIPOSITION'])].update(change)
                pending = next(merged, None)
            if pending is not None and last is not None:
                assert last < pending[0], 'Native phase cut splits simultaneous prediction lanes'
            phases.append(dict(phase=label, native_print_stamp=stamp, first_private_stamp=first,
                               last_private_stamp=last, counts=dict(counts),
                               by_location=[dict(start_pc=hex(pc), cfi_position=position, **dict(value))
                                            for (pc, position), value in sorted(locations.items())]))
        assert pending is None, 'Prediction opportunities remain after all native counts'
    receipt = json.loads(receipt_path.read_text())
    return dict(passed=True, point=folder.name, source=receipt['source'], tables=table_counts, phases=phases,
                source_sha256={str(p): digest(p) for p in [receipt_path, Path(__file__)]},
                scope='s2_fire-qualified conditional MBTB/provider-hit predictions with saturated provider, '
                      'original SC confidence satisfied and opposite directions, before a policy can suppress useSc. '
                      'These are speculative prediction entries, including opportunities that never reach accepted '
                      'training. Magnitude and threshold come from the same prediction stage. Native ordinals '
                      'define phases; private STAMP is not calibrated to TEA. No actual outcome, unique dynamic '
                      'prediction/training join, final BPU accuracy, wrong-path classification or CPU saving is inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    data = analyze(args.folder.resolve(), args.receipt.resolve())
    args.output.write_text(json.dumps(data, indent=2) + '\n')
    print(json.dumps(dict(passed=True, point=data['point'], phases=data['phases']), indent=2))


if __name__ == '__main__':
    main()
