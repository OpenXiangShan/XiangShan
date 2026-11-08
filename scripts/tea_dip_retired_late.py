"""Resolve the35M–39M tail missed by the original closed-CI retirement comparisons."""
from bisect import bisect_left
from collections import Counter
from pathlib import Path
import argparse
import json
import sqlite3
import sys

from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_retired_dispatch import STATES as DIP_STATES, summarize
from tea_dip_retired_progress import STATES, brackets
from tea_dip_turnover_ci_point import NAMESPACE, SOURCE, RUN, context

LOW, HIGH = 35000000, 39000000


def upper_milestone(rows):
    index = bisect_left([row['RETIRED'] for row in rows], HIGH)
    assert 0 < index < len(rows), '39M retirement must have actual snapshots on both sides'
    before, after = rows[index-1:index+1]
    assert before['RETIRED'] < HIGH <= after['RETIRED']
    return dict(retired=HIGH, cycle_lower=before['CYCLE'], cycle_upper=after['CYCLE'],
                retired_lower=before['RETIRED'], retired_upper=after['RETIRED'],
                states_lower={state: before['STATES_'+str(i)] for i, state in enumerate(STATES)},
                states_upper={state: after['STATES_'+str(i)] for i, state in enumerate(STATES)})


def analyze(root, point):
    ns = root / NAMESPACE
    record_path = ns / 'partial-analysis' / (point+'.json')
    original_path = ns / 'partial-analysis' / (point+'-retired-progress.json')
    receipt_path = ns / 'raw/closed-receipts' / (point+'.json')
    record, original, receipt = [json.loads(p.read_text()) for p in [record_path, original_path, receipt_path]]
    assert record['run_id'] == receipt['run_id'] == original['run_id'] == RUN
    assert record['source'] == receipt['source'] == original['source'] == SOURCE
    assert not receipt['original_process_live'] and receipt['runner_success_lines']
    assert digest(receipt_path) == record['closed_receipt_sha256']
    _, baseline, _, manifest = context(root)
    sys.path.insert(0, str(root))
    from analyze_windows import DIP
    windows = []
    for index, (label, folder) in enumerate([('control', baseline/'raw/results'/point), ('candidate', ns/'raw/results'/point)]):
        path = folder / 'simulator.db'
        entry = manifest['files']['raw/results/'+point+'/simulator.db'] if index == 0 else receipt['files']['simulator.db']
        verify(folder, {'simulator.db': entry})
        assert original['windows'][index]['database_sha256'] == entry['sha256']
        with sqlite3.connect(path.resolve().as_uri()+'?mode=ro', uri=True) as db:
            db.row_factory = sqlite3.Row
            rows = list(db.execute('SELECT * FROM TeaCycleSummary_0 ORDER BY CYCLE,ID'))
            samples = list(db.execute('SELECT * FROM Dip_0 ORDER BY CYCLE,ID'))
            finals = list(db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'))
            width = sum(row['name'].startswith('SLOTS_') and row['name'].endswith('_STATE') for row in db.execute('PRAGMA table_info(Dip_0)'))
            matches = list(db.execute('SELECT s.RETIRED,d.TOTALRETIRED FROM TeaCycleSummary_0 s JOIN Dip_0 d ON s.CYCLE=d.CYCLE'))
        assert width > 0 and matches and all(row['RETIRED'] == row['TOTALRETIRED'] for row in matches)
        left = brackets(rows)[-1]
        assert left['retired'] == LOW and left == original['windows'][index]['milestones'][-1]
        right = upper_milestone(rows)
        native = dict(cycles_lower=right['cycle_lower']-left['cycle_upper'], cycles_upper=right['cycle_upper']-left['cycle_lower'],
                      states_lower={state: right['states_lower'][state]-left['states_upper'][state] for state in STATES},
                      states_upper={state: right['states_upper'][state]-left['states_lower'][state] for state in STATES})
        assert all(b['CYCLE']-a['CYCLE'] == 1000 and b['TOTALRETIRED'] >= a['TOTALRETIRED'] for a, b in zip(samples, samples[1:]))
        whole_profile, whole, whole_bins = summarize(DIP, samples, finals, width)
        joined, joined_bins = Counter(), Counter()
        parts, late_profile = [], None
        for start, end in [(0, LOW), (LOW, HIGH), (HIGH, None)]:
            selected = [row for row in samples if row['TOTALRETIRED'] >= start and (end is None or row['TOTALRETIRED'] < end)]
            profile, summary, bins = summarize(DIP, selected, finals, width)
            joined.update(profile); joined_bins.update(bins)
            parts.append(dict(retired_start=start, retired_end=end, source_samples=len(selected), sampled_slots=summary['sampled_slots'],
                              unresolved_slots=summary['unresolved_slots'], states={state: sum(count for (name, mask), count in bins.items() if name == state) for state in DIP_STATES}))
            if start == LOW:
                late_profile = profile
        assert joined == whole_profile and joined_bins == whole_bins
        assert sum(part['unresolved_slots'] for part in parts) == whole['unresolved_slots']
        windows.append(dict(label=label, database_sha256=entry['sha256'], dispatch_width=width, milestones=[left, right], native=native,
                            whole_source_samples=len(samples), whole_unresolved_slots=whole['unresolved_slots'], source_partitions=parts,
                            all_three_partitions_reconstruct_full_pc_mask_profile=True, same_cycle_retirement_matches=len(matches), late_profile=late_profile))
    a, b = [window['native'] for window in windows]
    count = HIGH-LOW
    native_bounds = dict(lower=(b['cycles_lower']-a['cycles_upper'])/count, upper=(b['cycles_upper']-a['cycles_lower'])/count)
    state_bounds = {state: dict(lower=(b['states_lower'][state]-a['states_upper'][state])/count,
                                upper=(b['states_upper'][state]-a['states_lower'][state])/count) for state in STATES}
    assert native_bounds['lower'] <= native_bounds['upper'] and all(v['lower'] <= v['upper'] for v in state_bounds.values())
    assert windows[0]['dispatch_width'] == windows[1]['dispatch_width']
    scale = 1000/(windows[0]['dispatch_width']*count)
    parts = [window['source_partitions'][1] for window in windows]
    dip_delta = {state: (parts[1]['states'][state]-parts[0]['states'][state])*scale for state in DIP_STATES}
    x, y = [window.pop('late_profile') for window in windows]
    pc_delta = [dict(state=DIP.STATES[state], mask=mask, pc=hex(pc), delta_sampled_equivalent_cpi=(y.get((state,mask,pc),0)-x.get((state,mask,pc),0))*scale)
                for state,mask,pc in x.keys() | y.keys()]
    pc_delta.sort(key=lambda row: -abs(row['delta_sampled_equivalent_cpi']))
    sources = [record_path, original_path, receipt_path, baseline/'analysis/manifest.json', Path(__file__),
               Path(__file__).with_name('tea_dip_retired_progress.py'), Path(__file__).with_name('tea_dip_retired_dispatch.py')]
    return dict(passed=True, run_id=RUN, source=SOURCE, point=point, retired_start=LOW, retired_end=HIGH, retired_count=count,
                windows=windows, delta_cpi_bounds=native_bounds, tea_state_delta_cpi_bounds=state_bounds,
                dip_exclusive_state_delta=dip_delta, dip_total_sampled_equivalent_delta=sum(dip_delta.values()),
                top_sampled_pc_deltas=pc_delta[:20], source_sha256={str(p): digest(p) for p in sources},
                scope='Closed CI35M–39M tail only. Original5M–35M helpers/results unchanged;35M crossing exactly matches the archived analysis.39M uses real bracketing snapshots without interpolation/native endpoint replacement. Full DIP deferred finals remain ordered; before/late/after partitions reconstruct all PC/mask counts and unresolved budget. Native state bounds are marginal, not confidence intervals or additive extrema; DIP is sampled attribution, not exact critical savings.0–5M and39M–end remain uncovered. No stage or suite score.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--point', required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    data = analyze(args.report_dir.resolve(), args.point)
    args.output.write_text(json.dumps(data, indent=2)+'\n')
    print(json.dumps({key:data[key] for key in ['passed','point','delta_cpi_bounds','tea_state_delta_cpi_bounds','dip_exclusive_state_delta','dip_total_sampled_equivalent_delta']}, indent=2))


if __name__ == '__main__':
    main()
