"""Compare closed39a/8f8 formal points at common cumulative retirement milestones."""
from pathlib import Path
import argparse
import json
import sqlite3
import sys

from tea_dip_fdiv_ready_batch_evidence import closed
from tea_dip_query_endpoint_evidence import digest, pair
from tea_dip_retired_progress import brackets, TARGETS, STATES
from tea_dip_retired_dispatch import sample_bands


def analyze(root, point):
    items = [closed(root, 'formal', 'candidate', point, turnover=value) for value in [False, True]]
    if any(item is None for item in items):
        return dict(pending=True, database_read=False, point=point, reason='Both original closed formal points required')
    if not all(item['valid'] for item in items):
        return dict(pending=False, database_read=False, validation_passed=False, point=point, comparison=None, performance_speedup=None)
    assert [item['result']['source'] for item in items] == ['39a163eb3910ff9ecab02df4ac8e3c99d772feeb', '8f8ece60ac8b08b358743bc9db15a5a227dde0b8']
    commands = [item['case']['command'] for item in items]
    for flag in ['-W', '-I', '-C', '-s', '-i', '--diff', '--cst-file', '--dump-select-db']:
        assert commands[0][commands[0].index(flag) + 1] == commands[1][commands[1].index(flag) + 1]
    assert commands[0][commands[0].index('-W') + 1] == '20000000'
    assert commands[0][commands[0].index('-I') + 1] == '40000000'
    sys.path.insert(0, str(root))
    from analyze_windows import DIP
    windows = []
    for label, item in zip(['control39a', 'candidate8f8'], items):
        path = item['folder'] / 'simulator.db'
        with sqlite3.connect(path.resolve().as_uri() + '?mode=ro', uri=True) as db:
            db.row_factory = sqlite3.Row
            snapshots = list(db.execute('SELECT * FROM TeaCycleSummary_0 ORDER BY CYCLE,ID'))
            samples = list(db.execute('SELECT * FROM Dip_0 ORDER BY CYCLE,ID'))
            finals = list(db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'))
            width = sum(row['name'].startswith('SLOTS_') and row['name'].endswith('_STATE') for row in db.execute('PRAGMA table_info(Dip_0)'))
            matches = list(db.execute('SELECT s.RETIRED,d.TOTALRETIRED FROM TeaCycleSummary_0 s JOIN Dip_0 d ON s.CYCLE=d.CYCLE'))
        assert width > 0 and matches and all(row['RETIRED'] == row['TOTALRETIRED'] for row in matches)
        windows.append(dict(label=label, database_sha256=digest(path), dispatch_width=width,
                            milestones=brackets(snapshots), same_cycle_retirement_matches=len(matches),
                            **sample_bands(DIP, samples, finals, width, 1000)))
    assert windows[0]['dispatch_width'] == windows[1]['dispatch_width']
    segments = []
    for index, (low, high) in enumerate(zip(TARGETS, TARGETS[1:])):
        count = high - low
        x, y = [window['milestones'][index:index + 2] for window in windows]
        lower = (y[1]['cycle_lower'] - y[0]['cycle_upper'] - x[1]['cycle_upper'] + x[0]['cycle_lower']) / count
        upper = (y[1]['cycle_upper'] - y[0]['cycle_lower'] - x[1]['cycle_lower'] + x[0]['cycle_upper']) / count
        states = {state: dict(lower=(y[1]['states_lower'][state] - y[0]['states_upper'][state] - x[1]['states_upper'][state] + x[0]['states_lower'][state]) / count,
                              upper=(y[1]['states_upper'][state] - y[0]['states_lower'][state] - x[1]['states_lower'][state] + x[0]['states_upper'][state]) / count) for state in STATES}
        assert lower <= upper and all(row['lower'] <= row['upper'] for row in states.values())
        bands = [next(b for b in window['bands'] if b['retired_start'] == low and b['retired_end'] == high) for window in windows]
        def bins(band):
            return {(r['state'], r['mask']): r['slots'] for r in band['exclusive_state_mask']}
        a, b = map(bins, bands)
        scale = 1000 / (windows[0]['dispatch_width'] * count)
        masks = [dict(state=state, mask=mask, delta=(b.get((state, mask), 0) - a.get((state, mask), 0)) * scale)
                 for state, mask in sorted(a.keys() | b.keys())]
        total = (bands[1]['sampled_cycles'] - bands[0]['sampled_cycles']) * 1000 / count
        assert abs(sum(row['delta'] for row in masks) - total) < 1e-12
        segments.append(dict(retired_start=low, retired_end=high, exact_delta_cpi_lower=lower,
                             exact_delta_cpi_upper=upper, exact_state_delta_cpi_bounds=states,
                             dip_sampled_equivalent_delta=total,
                             dip_exclusive_state_delta={state: sum(row['delta'] for row in masks if row['state'] == state)
                                                        for state in ['Base', 'Stall', 'FrontEnd', 'Misspeculation', 'Unresolved']},
                             dip_exclusive_state_mask_delta=masks))
    helpers = ['tea_dip_fdiv_ready_batch_evidence.py', 'tea_dip_query_endpoint_evidence.py',
               'tea_dip_retired_progress.py', 'tea_dip_retired_dispatch.py']
    return dict(pending=False, database_read=True, point=point,
                sources=[item['result']['source'] for item in items],
                source_receipt_sha256=[item['receipt_sha256'] for item in items],
                native_comparison=pair(items[0]['folder'], items[1]['folder'], expected_instructions=20000000),
                windows=windows, segments=segments, consumer_sha256=digest(Path(__file__)),
                helper_sha256={name: digest(Path(__file__).with_name(name)) for name in helpers},
                scope='Original closed39a/8f8 formal pair only. Common5M-35M cumulative TEA retirement milestones are bracketed without interpolation. Exact state bounds are marginal conservative intervals, not confidence intervals or a jointly additive budget. DIP deferred attribution reconstructs all source bands, with period1000 sampled-equivalent deltas distinct from exact bounds. Native warmup/ROI edges differ slightly from20M;0-5M/35M-end uncovered. Not independent repetitions, matched dynamic-uop identity, unique causality or suite performance.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--point', required=True)
    parser.add_argument('--output-dir', type=Path, required=True)
    args = parser.parse_args()
    result = analyze(args.report_dir.resolve(), args.point)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    path = args.output_dir / (args.point + '-retired-progress.json')
    assert not path.exists(), 'Preserve the existing analysis snapshot'
    path.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps({key: result[key] for key in ['point', 'pending', 'database_read', 'reason', 'segments'] if key in result}, indent=2))


if __name__ == '__main__':
    main()
