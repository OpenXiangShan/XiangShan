"""Partition a closed guest's exact TEA and sampled PC/fate budgets by retired instructions."""
import argparse
from bisect import bisect_left, bisect_right
from collections import Counter
from contextlib import closing
import json
import math
from pathlib import Path
import re
import sqlite3
from types import SimpleNamespace

import dip_analyze as dip
import tea_visualize as tea
from tea_dip_query_endpoint_evidence import digest, endpoint, verify


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
    retired = exact['roi']['native_reported_retired']
    cpi = (end - begin) / retired

    def anchor(cycle, count, states, target):
        return dict(cycle=cycle, retired=count, states=states, target_relative_retired=target,
                    actual_relative_retired=count - first['tea_retired'], threshold_cycle_bounds=[cycle, cycle],
                    threshold_state_bounds={s: [v, v] for s, v in states.items()})

    lower = anchor(begin, first['tea_retired'], first['states'], 0)
    upper = anchor(end, last['tea_retired'], last['states'], retired)
    database = folder / 'simulator.db'
    with closing(sqlite3.connect(database.as_uri() + '?mode=ro', uri=True)) as db:
        db.row_factory = sqlite3.Row
        snapshots = list(db.execute('SELECT * FROM TeaCycleSummary_0 WHERE CYCLE>? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end)))
        candidates = [anchor(r['CYCLE'], r['RETIRED'], {s: r['STATES_' + str(i)] for i, s in enumerate(tea.STATES)}, None)
                      for r in snapshots] + [upper]
        previous = lower
        for row in candidates:
            assert row['cycle'] > previous['cycle'], 'Duplicate or unordered summary cycle'
            assert row['retired'] >= previous['retired'], 'Nonmonotonic summary retirement'
            assert all(row['states'][s] >= previous['states'][s] for s in tea.STATES)
            assert sum(row['states'][s] - previous['states'][s] for s in tea.STATES) == row['cycle'] - previous['cycle'], 'Inconsistent cumulative TEA states'
            previous = row
        boundaries = [lower]
        for target in range(retired_step, last['tea_retired'] - first['tea_retired'], retired_step):
            position = bisect_left([r['retired'] for r in candidates], first['tea_retired'] + target)
            row = candidates[position]
            if row['cycle'] == end:
                break
            if row['cycle'] != boundaries[-1]['cycle']:
                prior = candidates[position - 1] if position else lower
                boundaries.append(dict(row, target_relative_retired=target,
                                       threshold_cycle_bounds=[prior['cycle'], row['cycle']],
                                       threshold_state_bounds={s: [prior['states'][s], row['states'][s]] for s in tea.STATES}))
        boundaries.append(upper)
        expected = list(range(((begin + periods[0] - 1) // periods[0]) * periods[0], end, periods[0]))
        for table in ['Tea_0', 'Dip_0']:
            assert [r[0] for r in db.execute('SELECT CYCLE FROM ' + table + ' WHERE CYCLE>=? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end))] == expected, 'Incomplete ' + table + ' source grid'
        samples = list(db.execute('SELECT * FROM Dip_0 WHERE CYCLE>=? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end)))
        width = sum(r['name'].startswith('SLOTS_') and r['name'].endswith('_STATE') for r in db.execute('PRAGMA table_info(Dip_0)'))
        def resolve(rows):
            return dip.analyze(rows, db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'), width)
        full_profile, full = resolve(samples)
        assert full['sampled_cycles'] == len(expected) > 0
        tea_profiles = [Counter() for _ in boundaries[1:]]
        tea_counts = [0] * len(tea_profiles)
        def sample(cycle, state, lanes, weight):
            index = bisect_right([r['cycle'] for r in boundaries], cycle) - 1
            tea_counts[index] += 1
            for pc, signature in lanes:
                tea_profiles[index][state, pc, signature] += weight
        parsed = tea.load_data(database, folder.name, SimpleNamespace(cycle_range=(begin, end), spec_checkpoint=False, bins=1), sample_callback=sample)
        assert sum(tea_counts) == parsed['summary']['rows'] == len(expected)
        combined = Counter()
        for profile in tea_profiles:
            combined.update(profile)
        reference = {(state, pc, signature): value for state, pcs in parsed['state_weights'].items()
                     for pc, signatures in pcs.items() for signature, value in signatures.items()}
        assert combined.keys() == reference.keys() and all(math.isclose(v, reference[k], abs_tol=1e-9) for k, v in combined.items())
        phases, dip_profiles, unresolved, reclassified = [], Counter(), 0, 0
        for index, (a, b) in enumerate(zip(boundaries, boundaries[1:])):
            rows = [row for row in samples if a['cycle'] <= row['CYCLE'] < b['cycle']]
            profile, summary = resolve(rows)
            assert len(rows) == tea_counts[index]
            dip_profiles.update(profile)
            unresolved += summary['unresolved_slots']
            reclassified += summary['reclassified_slots']
            exact_states = {state: b['states'][state] - a['states'][state] for state in tea.STATES}
            profiles = [dict(view='TEA', state=state, pc=hex(pc) if pc is not None else 'unknown', signature=signature,
                             roi_scaled_CPI=value / len(expected) * cpi) for (state, pc, signature), value in tea_profiles[index].items()]
            profiles += [dict(view='DIP', state=dip.STATES[state], pc=hex(pc), signature=signature,
                              roi_scaled_CPI=value / full['sampled_slots'] * cpi) for (state, signature, pc), value in profile.items()]
            phases.append(dict(index=index, begin=a, end=b, cycles=b['cycle'] - a['cycle'],
                               tea_retired=b['retired'] - a['retired'], exact_TEA_states=exact_states,
                               nominal_cycles_bounds=[b['threshold_cycle_bounds'][0] - a['threshold_cycle_bounds'][1],
                                                      b['threshold_cycle_bounds'][1] - a['threshold_cycle_bounds'][0]],
                               nominal_TEA_roi_scaled_CPI_bounds={s: [(b['threshold_state_bounds'][s][0] - a['threshold_state_bounds'][s][1]) / retired,
                                                                    (b['threshold_state_bounds'][s][1] - a['threshold_state_bounds'][s][0]) / retired] for s in tea.STATES},
                               exact_TEA_roi_scaled_CPI={s: v / retired for s, v in exact_states.items()},
                               sampled_cycles=len(rows), profiles=profiles,
                               DIP_unresolved_slots=summary['unresolved_slots'],
                               DIP_unresolved_roi_scaled_CPI=summary['unresolved_slots'] / full['sampled_slots'] * cpi))
    assert dip_profiles == full_profile and unresolved == full['unresolved_slots'] and reclassified == full['reclassified_slots']
    assert sum(p['cycles'] for p in phases) == exact['roi']['native_cycles']
    assert all(sum(p['exact_TEA_states'][s] for p in phases) == exact['roi']['states'][s] for s in tea.STATES)
    assert math.isclose(sum(sum(p['exact_TEA_roi_scaled_CPI'].values()) for p in phases), cpi, abs_tol=1e-12)
    for view in ['TEA', 'DIP']:
        total = sum(r['roi_scaled_CPI'] for p in phases for r in p['profiles'] if r['view'] == view)
        if view == 'DIP':
            total += sum(p['DIP_unresolved_roi_scaled_CPI'] for p in phases)
        assert math.isclose(total, cpi, abs_tol=1e-9)
    return dict(passed=True, point=folder.name, source=receipt['source'], retired_step=retired_step,
                native_CPI=cpi, native_retired=retired, native_cycles=end - begin,
                retirement_counter_difference=exact['roi']['retirement_counter_difference'], phases=phases,
                all_phase_profiles_equal_full=True, unresolved_DIP_slots=unresolved,
                receipt_sha256=digest(receipt_path), tool_sha256={name: digest(Path(__file__).with_name(name)) for name in
                    [Path(__file__).name, 'tea_visualize.py', 'dip_analyze.py', 'tea_dip_query_endpoint_evidence.py']},
                scope='Exact native endpoints and first existing summary at/after each relative retirement threshold. '
                      'Actual boundary retirement/overshoot and conservative snapshot brackets retained; no interpolation '
                      'or dynamic-prefix equivalence. Nominal phase bounds may share boundary uncertainty and '
                      'their independent extremes must not be added as a single feasible scenario. '
                      'All phase contributions use the whole native ROI denominator. Sampled TEA/DIP PC contributions '
                      'are not per-phase CPI, unique saved CPU cycles or full-suite gains. All later DIP finals and '
                      'unresolved slots retained; phase profiles exactly reproduce the full parser outputs.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ['folder', 'receipt', 'output']:
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--retired-step', type=int, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    value = analyze(args.folder, args.receipt, args.retired_step)
    args.output.write_text(json.dumps(value, indent=2) + '\n')
    print(json.dumps(dict(passed=True, point=value['point'], phases=len(value['phases']), native_CPI=value['native_CPI'])))


if __name__ == '__main__':
    main()
