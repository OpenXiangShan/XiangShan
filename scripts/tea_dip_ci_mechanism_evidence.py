"""Explain individually closed CI windows without estimating a partial suite score."""
from pathlib import Path
import argparse
import csv
import hashlib
import json
import sqlite3
import sys


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def gap_window(directory, window, bounds):
    interior = window['exact_tea_interior']
    with sqlite3.connect((directory / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        intervals = list(db.execute('SELECT * FROM TeaNoCommit_0 ORDER BY START,ID'))
        first = db.execute('SELECT * FROM TeaCycleSummary_0 WHERE CYCLE=? ORDER BY ID LIMIT 1', (interior['begin'],)).fetchone()
        last = db.execute('SELECT * FROM TeaCycleSummary_0 WHERE CYCLE=? ORDER BY ID DESC LIMIT 1', (interior['end'],)).fetchone()
    previous_end = 0
    for interval in intervals:
        assert interval['ENDCYCLE'] > interval['START'] >= previous_end
        assert sum(interval[f'STATES_{n}'] for n in range(3)) == interval['ENDCYCLE'] - interval['START']
        previous_end = interval['ENDCYCLE']
    noncommit = sum(last[f'STATES_{n}'] - first[f'STATES_{n}'] for n in range(1, 4))
    states = {state: last[f'STATES_{i}'] - first[f'STATES_{i}']
              for i, state in enumerate(['Computing', 'Stalled', 'Flushed', 'Drained'])}
    assert min(states.values()) >= 0 and sum(states.values()) == interior['cycles']
    assert interior['cycles'] + interior['omitted_edge_cycles'] == window['cycles']
    return ({threshold: bounds(intervals, window['roi_start'], window['roi_end'], last['CYCLE'] - last['PENDING'],
                              noncommit, noncommit + interior['omitted_edge_cycles'], threshold) for threshold in [128, 512]}, states)


def state_delta_bounds(counts, windows):
    # Each omitted cycle can belong to any one state. These marginal bounds
    # use native ROI denominators; their extrema cannot all occur together.
    a, b = windows
    assert all(window['exact_tea_interior']['omitted_edge_cycles'] >= 0 for window in windows)
    return {state: dict(lower=counts[1][state] / b['instructions'] -
                       (counts[0][state] + a['exact_tea_interior']['omitted_edge_cycles']) / a['instructions'],
                       upper=(counts[1][state] + b['exact_tea_interior']['omitted_edge_cycles']) / b['instructions'] -
                       counts[0][state] / a['instructions']) for state in counts[0]}


def state_direction_audit(state_bounds, comparison):
    audit = {}
    for state, interval in state_bounds.items():
        sampled = comparison['delta_TEA_' + state]
        sampled_direction = 'decrease' if sampled < 0 else 'increase' if sampled > 0 else 'unchanged'
        direction = ('decrease' if interval['upper'] < 0 else 'increase' if interval['lower'] > 0
                     else 'unchanged' if interval['lower'] == interval['upper'] == 0 else 'undetermined')
        audit[state] = dict(sampled_direction=sampled_direction, native_roi_direction=direction,
                            strict_sign_conflict=None if direction == 'undetermined' else
                            {sampled_direction, direction} == {'increase', 'decrease'})
    return audit


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--ci-evidence', choices=['ci-readiness', 'ci-credit-div', 'ci-fdiv-credit', 'fdiv-wb-query-paired-ci'], required=True)
    parser.add_argument('--point', action='append', help='Analyze only these already audited points')
    args = parser.parse_args()
    root = args.report_dir.resolve()
    sys.path.insert(0, str(root))
    from analyze_gaps import bounds
    from analyze_queue_observation import compare_windows
    from native_perf import read_blocks, counter_multisets
    namespace = 'ci' if args.ci_evidence == 'ci-readiness' else args.ci_evidence
    out = root / namespace / 'partial-analysis'
    identity = json.loads((root / 'evidence' / args.ci_evidence / 'archive-identity.json').read_text())
    run = json.loads((root / 'evidence' / args.ci_evidence / 'run.json').read_text())
    assert identity['source'] == run['head_sha']
    files = [out / (point + '.json') for point in args.point] if args.point else sorted(out.glob('*.json'))
    rows = []
    for path in files:
        record = json.loads(path.read_text())
        if 'comparison' not in record:
            continue
        point = record['comparison']['point']
        receipt_path = root / namespace / 'raw/closed-receipts' / (point + '.json')
        receipt = json.loads(receipt_path.read_text())
        assert receipt['run_id'] == record['run_id'] == run['id'], 'CI run mismatch'
        assert receipt['source'] == record['source'] == identity['source'], 'CI source mismatch'
        assert not receipt['original_process_live'] and receipt['runner_success_lines'], 'CI point has no closed original-process proof'
        assert digest(receipt_path) == record['closed_receipt_sha256']
        directories = [root.parents[1] / 'ci-41413249290/raw/results' / point, root / namespace / 'raw/results' / point]
        for name, entry in receipt['files'].items():
            raw = directories[1] / name
            assert raw.stat().st_size == entry['bytes'] and digest(raw) == entry['sha256']
        windows = [record[key] for key in ['control', 'candidate']]
        phases = [sorted(read_blocks(directory / 'simulator_err.txt').items()) for directory in directories]
        assert all(len(phase) == 2 for phase in phases)
        native_changes = []
        for label, ((at, ab), (bt, bb)) in zip(['warmup', 'roi'], zip(*phases)):
            a, b = [counter_multisets(block) for block in [ab, bb]]
            names = sorted(key for key in a.keys() | b.keys() if a.get(key) != b.get(key))
            native_changes.append(dict(phase=label, stamps=[at, bt], changed_names=len(names),
                                       differences=[dict(name=key, control=a.get(key), candidate=b.get(key)) for key in names]))
        equality = None
        if record['comparison']['speedup'] == 0:
            profiles = [[{**row, 'cpi': float(row['cpi'])} for row in csv.DictReader((out / (point + '-' + label + '-pcs.csv')).open())]
                        for label in ['control', 'candidate']]
            equality = compare_windows(*windows, *directories, *profiles)
            assert equality['checks']['all_old_native_warmup_and_roi_equal'] == all(not phase['changed_names'] for phase in native_changes)
        gap_windows, state_counts = zip(*(gap_window(directory, window, bounds)
                                          for directory, window in zip(directories, windows)))
        roi_state_bounds = state_delta_bounds(state_counts, windows)
        deltas = []
        for threshold in [128, 512]:
            delta = dict(threshold=threshold)
            for kind in ['long', 'short']:
                delta[kind + '_lower'] = gap_windows[1][threshold][kind + '_cycles_lower'] / windows[1]['instructions'] - gap_windows[0][threshold][kind + '_cycles_upper'] / windows[0]['instructions']
                delta[kind + '_upper'] = gap_windows[1][threshold][kind + '_cycles_upper'] / windows[1]['instructions'] - gap_windows[0][threshold][kind + '_cycles_lower'] / windows[0]['instructions']
                assert delta[kind + '_lower'] <= delta[kind + '_upper']
            deltas.append(delta)
        exact = {state: windows[1]['exact_tea_interior']['exact_state_cpi'][state] - windows[0]['exact_tea_interior']['exact_state_cpi'][state]
                 for state in ['Computing', 'Stalled', 'Flushed', 'Drained']}
        dip = {state: windows[1]['metrics']['DIP_' + state] - windows[0]['metrics']['DIP_' + state]
               for state in ['Base', 'Stall', 'FrontEnd', 'Misspeculation', 'Unresolved']}
        assert abs(sum(dip.values()) - record['comparison']['delta_cpi']) < 1e-9
        pc_rows = list(csv.DictReader((out / (point + '-pc-deltas.csv')).open()))
        top_pcs = {view: sorted([{**row, 'delta_cpi': float(row['delta_cpi'])} for row in pc_rows if row['view'] == view],
                                key=lambda row: (-abs(row['delta_cpi']), row['pc'], row['state'], row['signature'], row['labels']))[:8]
                   for view in ['TEA', 'DIP']}
        rows.append(dict(point=point, comparison=record['comparison'], native_phase_differences=native_changes, equality=equality,
                         exact_delta_cpi=exact, exact_omitted_edges=[window['exact_tea_interior']['omitted_edge_cycles'] for window in windows],
                         native_roi_state_delta_cpi_bounds=roi_state_bounds,
                         native_roi_state_direction_audit=state_direction_audit(roi_state_bounds, record['comparison']),
                         dip_exclusive_delta_cpi=dip, gap_delta_cpi_bounds=deltas,
                         counters={key: [window['counters'][key] for window in windows] for key in windows[0]['counters']},
                         top_sampled_pc_deltas=top_pcs, source_window_sha256=digest(path)))
    result = dict(run_id=run['id'], source=identity['source'], ci_evidence=args.ci_evidence, rows=rows,
                  source_sha256={str(Path(__file__).name): digest(Path(__file__)),
                                 **{name: digest(root / name) for name in ['analyze_gaps.py', 'native_perf.py', 'analyze_queue_observation.py']}},
                  scope='Individually closed CI windows. Exact interior TEA keeps its own retired denominator. Native-ROI state bounds assign omitted edge cycles conservatively to each state and use each native retired denominator; marginal extremes are not jointly additive. Exclusive DIP closes each full ROI. State and gap bounds are conservative intervals, not confidence intervals. State direction is strict only when the whole native-ROI bound has that sign; a sampled sign conflict is explicit and a bound crossing zero remains undetermined. Zero CPI still requires all old native and six execution categories. PC evidence is sampled attribution, not unique dynamic causality. No partial-suite score.')
    (out / 'mechanism-evidence.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(dict(run_id=result['run_id'], points=len(rows), rows=[dict(point=row['point'], speedup=row['comparison']['speedup'],
        native_changed_names=[phase['changed_names'] for phase in row['native_phase_differences']],
        zero_cpi_full_neutral=row['equality']['neutral'] if row['equality'] is not None else None) for row in rows]), indent=2))


if __name__ == '__main__':
    main()
