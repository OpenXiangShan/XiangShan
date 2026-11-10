"""Resolve closed no-retirement intervals into duration bands with conservative ROI bounds.

Reuse the report's existing audited gap-bound calculation. Bands classify the
whole gap duration, not a PC's service latency; ROI edges and tails stay unknown.
"""
import argparse
import json
from pathlib import Path
import sqlite3
import sys

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_wb_protocol_micro import write


THRESHOLDS = [128, 256, 512, 1024]


def analyze(folder, receipt_path, gap_bounds):
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    verify(folder, receipt['files'])
    blocks = read_blocks(folder / 'simulator_err.txt')
    assert len(blocks) == 2
    begin, end = min(blocks) + 1, max(blocks) + 1
    values = blocks[max(blocks)]
    def rob(name):
        matches, = [v for k, v in values.items() if k.endswith('.rob.' + name)]
        value, = matches
        return value
    cycles, retired = rob('clock_cycle'), rob('commitInstr')
    assert cycles == end - begin and retired > 0
    with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        rows = list(db.execute('SELECT * FROM TeaNoCommit_0 ORDER BY START,ID'))
        first = db.execute('SELECT * FROM TeaCycleSummary_0 WHERE CYCLE>=? AND CYCLE<=? ORDER BY CYCLE,ID LIMIT 1', (begin, end)).fetchone()
        last = db.execute('SELECT * FROM TeaCycleSummary_0 WHERE CYCLE>=? AND CYCLE<=? ORDER BY CYCLE DESC,ID DESC LIMIT 1', (begin, end)).fetchone()
    previous = 0
    for row in rows:
        assert row['ENDCYCLE'] > row['START'] >= previous
        assert sum(row['STATES_' + str(i)] for i in range(3)) == row['ENDCYCLE'] - row['START']
        assert row['LEFTCENSORED'] or row['RIGHTCENSORED'] or row['ENDCYCLE'] - row['START'] >= 128
        previous = row['ENDCYCLE']
    noncommit = sum(last['STATES_' + str(i)] - first['STATES_' + str(i)] for i in range(1, 4))
    edges = cycles - (last['CYCLE'] - first['CYCLE'])
    assert edges >= 0
    thresholds = {t: gap_bounds(rows, begin, end, last['CYCLE'] - last['PENDING'], noncommit,
                                noncommit + edges, t) for t in THRESHOLDS}
    bands = []
    for i, lower in enumerate(THRESHOLDS):
        upper = THRESHOLDS[i + 1] if i + 1 < len(THRESHOLDS) else None
        a, b = thresholds[lower], thresholds[upper] if upper is not None else None
        low = max(0, a['long_cycles_lower'] - b['long_cycles_upper']) if b else a['long_cycles_lower']
        high = a['long_cycles_upper'] - b['long_cycles_lower'] if b else a['long_cycles_upper']
        complete = [r for r in rows if not r['LEFTCENSORED'] and not r['RIGHTCENSORED']
                    and r['START'] >= begin and r['ENDCYCLE'] <= end
                    and r['ENDCYCLE'] - r['START'] >= lower
                    and (upper is None or r['ENDCYCLE'] - r['START'] < upper)]
        observed = sum(r['ENDCYCLE'] - r['START'] for r in complete)
        assert 0 <= low <= observed <= high <= cycles
        bands.append(dict(min_duration=lower, max_duration_exclusive=upper, complete_intervals=len(complete),
                          complete_cycles=observed, contribution_lower=low / retired,
                          contribution_upper=high / retired))
    return dict(point=folder.name, source=receipt['source'], native_cycles=cycles, native_retired=retired,
                roi_begin=begin, roi_end=end, bands=bands, threshold_bounds=thresholds,
                receipt_sha256=digest(receipt_path), tool_sha256=digest(Path(__file__)),
                scope='Complete interval counts exclude censored and ROI-crossing gaps. Contribution bounds '
                      'retain ROI edges and unreported tails. Band extrema are marginal, not simultaneous or '
                      'confidence intervals. Gaps shorter than128 cycles are not individually logged. No PC '
                      'latency, additive TEA/DIP budget, causal counterfactual or hardware expansion is inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    sys.path.insert(0, str(args.report_dir.resolve()))
    from analyze_gaps import bounds
    result = analyze(args.folder.resolve(), args.receipt.resolve(), bounds)
    result['gap_bounds_source_sha256'] = digest(args.report_dir / 'analyze_gaps.py')
    write(args.output, result)
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
