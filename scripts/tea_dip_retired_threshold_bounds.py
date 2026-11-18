"""Tighten retirement-crossing bounds using closed DIP prefixes and TEA snapshots."""
import argparse
from bisect import bisect_left
from contextlib import closing
import json
from pathlib import Path
import re
import sqlite3

from tea_dip_query_endpoint_evidence import digest, endpoint, verify


def crossings(rows, targets):
    """Return conservative cycle brackets for monotone cumulative retirement."""
    assert len(rows) >= 2
    assert all(a[0] < b[0] and a[1] <= b[1] for a, b in zip(rows, rows[1:])), 'Nonmonotonic retirement prefix'
    counts = [r[1] for r in rows]
    result = []
    for target in targets:
        index = bisect_left(counts, target)
        assert 0 < index < len(rows), 'Retirement threshold lacks two snapshots'
        before, after = rows[index - 1:index + 1]
        result.append(dict(retired=target, cycle_bounds=[before[0], after[0]],
                           retired_bounds=[before[1], after[1]]))
    return result


def analyze(folder, receipt_path, retired_step):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    verify(folder, receipt['files'])
    if 'runner_success_lines' in receipt:
        assert receipt['run_id'] > 0 and any(
            re.search(r'(?<!\S)' + re.escape(folder.name) + r' succeeded\b', line)
            for line in receipt['runner_success_lines']), 'Missing successful CI point evidence'
    else:
        assert 'result.json' in receipt['files'], 'Manual result must be bound to the closure receipt'
        result = json.loads((folder / 'result.json').read_text())
        assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
        assert result['source'] == receipt['source']
    assert retired_step > 0
    stdout = (folder / 'simulator_out.txt').read_text()
    assert 'Difftest enabled' in stdout
    for name in ['enableTea0', 'enableDip0']:
        assert re.search(r'^' + name + r' = 1$', stdout, re.M)
    periods = [int(re.search(r'^' + name + r' = (\d+)$', stdout, re.M)[1])
               for name in ['teaSamplePeriod0', 'dipSamplePeriod0']]
    assert periods[0] == periods[1] and periods[0] > 0
    exact = endpoint(folder)
    assert exact['native_roi_cycle_duration_matches']
    first, last = exact['frames']
    begin, end = first['tea_cycle'], last['tea_cycle']
    with closing(sqlite3.connect((folder / 'simulator.db').as_uri() + '?mode=ro', uri=True)) as db:
        summaries = db.execute('SELECT CYCLE,RETIRED FROM TeaCycleSummary_0 WHERE CYCLE>? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end)).fetchall()
        dip = db.execute('SELECT CYCLE,TOTALRETIRED,RETIRED FROM Dip_0 WHERE CYCLE>=? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end)).fetchall()
        tea = db.execute('SELECT CYCLE,CONTEXT_RETIRED FROM Tea_0 WHERE CYCLE>=? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end)).fetchall()
    expected = list(range(((begin + periods[0] - 1) // periods[0]) * periods[0], end, periods[0]))
    assert expected and [r[0] for r in dip] == [r[0] for r in tea] == expected, 'Incomplete source grids'
    assert all(d[2] == t[1] for d, t in zip(dip, tea, strict=True)), 'Current-cycle retirement fields disagree'
    assert all(a[1] + a[2] <= b[1] for a, b in zip(dip, dip[1:])), 'DIP prefix excludes prior current-cycle retirement'
    summary_rows = [(begin, first['tea_retired']), *summaries, (end, last['tea_retired'])]
    merged = dict(summary_rows)
    assert len(merged) == len(summary_rows), 'Duplicate summary boundary'
    for cycle, total, current in dip:
        assert cycle not in merged or merged[cycle] == total, 'DIP/TEA retirement epochs disagree'
        merged[cycle] = total
    rows = sorted(merged.items())
    targets = list(range(first['tea_retired'] + retired_step, last['tea_retired'], retired_step))
    coarse, tight = crossings(summary_rows, targets), crossings(rows, targets)
    lower = dict(retired=first['tea_retired'], cycle_bounds=[begin, begin])
    upper = dict(retired=last['tea_retired'], cycle_bounds=[end, end])
    phases = []
    for index, (a, b, x, y) in enumerate(zip([lower, *coarse], [*coarse, upper], [lower, *tight], [*tight, upper], strict=True)):
        bounds = [y['cycle_bounds'][0] - x['cycle_bounds'][1], y['cycle_bounds'][1] - x['cycle_bounds'][0]]
        old = [b['cycle_bounds'][0] - a['cycle_bounds'][1], b['cycle_bounds'][1] - a['cycle_bounds'][0]]
        assert old[0] <= bounds[0] <= bounds[1] <= old[1]
        phases.append(dict(index=index, relative_retired_start=x['retired'] - first['tea_retired'],
                           relative_retired_end=y['retired'] - first['tea_retired'],
                           cycles_bounds=bounds, summary_only_cycles_bounds=old))
    return dict(passed=True, point=folder.name, source=receipt['source'], retired_step=retired_step,
                native_retired=exact['roi']['native_reported_retired'], native_cycles=end - begin,
                retirement_counter_difference=exact['roi']['retirement_counter_difference'],
                sample_period=periods[0], sampled_cycles=len(dip),
                thresholds=[dict(relative_retired=r['retired'] - first['tea_retired'], **r,
                                 summary_only_cycle_bounds=c['cycle_bounds']) for c, r in zip(coarse, tight, strict=True)],
                phases=phases, receipt_sha256=digest(receipt_path),
                tool_sha256={name: digest(Path(__file__).with_name(name)) for name in
                             [Path(__file__).name, 'tea_dip_query_endpoint_evidence.py']},
                scope='Before-cycle cumulative DIP TOTALRETIRED and TEA summary/native endpoint prefixes. '
                      'Tea CONTEXT_RETIRED and DIP RETIRED are current-cycle counts, never prefixes. '
                      'No interpolation or exact CPU-state allocation at DIP-only boundaries. '
                      'Bounds are conservative crossing ranges, not confidence intervals or identical dynamic prefixes; '
                      'adjacent phases share threshold uncertainty, so independent extremes cannot be added. '
                      'No new simulation, state or suite score.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ['folder', 'receipt', 'output']:
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--retired-step', type=int, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    value = analyze(args.folder, args.receipt, args.retired_step)
    args.output.write_text(json.dumps(value, indent=2) + '\n')
    print(json.dumps(dict(passed=True, point=value['point'], thresholds=len(value['thresholds']))))


if __name__ == '__main__':
    main()
