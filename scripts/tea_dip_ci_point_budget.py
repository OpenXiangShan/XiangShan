"""Reconcile a closed default20M+20M CI pair's native, exact TEA and sampled budgets."""
import argparse
from fractions import Fraction
import json
from pathlib import Path
import sqlite3

import dip_analyze as dip
from native_perf import read_blocks
from tea_dip_ci_repeat_evidence import point
from tea_dip_clock_alignment import core_native_alignment
from tea_dip_query_endpoint_evidence import STATES, digest, endpoint


def budget(folder, receipt_path):
    """Use the existing formal-CI closure/config gate before any SQLite reads."""
    _, native = point(folder, receipt_path)
    exact = endpoint(folder)
    frames = read_blocks(folder / 'simulator_err.txt')
    roi = native['phases']['ROI']
    cycles, retired = roi['cycles'], roi['retired']
    assert abs(retired - 20000000) < 128 and abs(native['phases']['warmup']['retired'] - 20000000) < 128
    assert exact['native_roi_cycle_duration_matches'] and sum(exact['roi']['states'].values()) == cycles
    with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        clock = core_native_alignment(db, folder / 'simulator_err.txt')
        offset = clock['core_to_native_offset']
        begin, end = min(frames) + 1 - offset, max(frames) + 1 - offset
        assert [begin, end] == [frame['tea_cycle'] for frame in exact['frames']]
        expected = list(range(((begin + 999) // 1000) * 1000, end, 1000))
        for table in ['Tea_0', 'Dip_0']:
            observed = [row[0] for row in db.execute(
                'SELECT CYCLE FROM ' + table + ' WHERE CYCLE>=? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end))]
            assert observed == expected, 'Incomplete or duplicate ' + table + ' source sampling grid'
        width = sum(row['name'].startswith('SLOTS_') and row['name'].endswith('_STATE')
                    for row in db.execute('PRAGMA table_info(Dip_0)'))
        _, summary = dip.analyze(
            db.execute('SELECT * FROM Dip_0 WHERE CYCLE>=? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end)),
            db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'),
            width)
        tea = dict(db.execute('SELECT STATE,count(*) FROM Tea_0 WHERE CYCLE>=? AND CYCLE<? GROUP BY STATE', (begin, end)))
    count = sum(tea.values())
    assert count == summary['sampled_cycles'] and count > 0
    cpi = float(Fraction(cycles, retired))
    sampled_dip = {name: summary['state_cycles'][name] / count * cpi for name in dip.STATES}
    sampled_dip['Unresolved'] = summary['unresolved_slots'] / summary['sampled_slots'] * cpi
    assert abs(sum(sampled_dip.values()) - cpi) < 1e-9
    return dict(native=native, exact_TEA=exact, sampled_cycles=count,
                sampled_TEA_CPI={name: tea.get(i, 0) / count * cpi for i, name in enumerate(STATES)},
                DIP_CPI=sampled_dip, clock_alignment=clock)


def compare(before, after):
    """Retain different actual default seeds; check inputs and conserve exact budgets."""
    assert before['native']['checkpoint'] == after['native']['checkpoint'], 'Checkpoint mismatch'
    assert all(before['native']['artifact_sha256'][key] == after['native']['artifact_sha256'][key]
               for key in ['reference', 'cst']), 'Reference/CST mismatch'
    a, b = [Fraction(side['native']['phases']['ROI']['cycles'], side['native']['phases']['ROI']['retired'])
            for side in [before, after]]
    exact = {state: Fraction(after['exact_TEA']['roi']['states'][state], after['native']['phases']['ROI']['retired'])
             - Fraction(before['exact_TEA']['roi']['states'][state], before['native']['phases']['ROI']['retired'])
             for state in STATES}
    assert sum(exact.values()) == b - a
    sampled = {state: after['sampled_TEA_CPI'][state] - before['sampled_TEA_CPI'][state] for state in STATES}
    return dict(control=before, candidate=after, speedup=float(a / b - 1), delta_CPI=float(b - a),
                exact_TEA_delta_CPI={state: float(value) for state, value in exact.items()},
                sampled_TEA_delta_CPI=sampled,
                sampled_opposite_exact_TEA_states=[state for state in STATES if sampled[state] * exact[state] < 0],
                DIP_delta_CPI={state: after['DIP_CPI'][state] - before['DIP_CPI'][state] for state in before['DIP_CPI']},
                seeds_equal=before['native']['actual_seed'] == after['native']['actual_seed'])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ['control', 'control-receipt', 'candidate', 'candidate-receipt', 'output']:
        parser.add_argument('--' + name, type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists(), 'Keep the previous evidence; choose a new output path'
    value = compare(budget(args.control, args.control_receipt), budget(args.candidate, args.candidate_receipt))
    result = dict(passed=True, comparison=value,
                  tool_sha256={name: digest(Path(__file__).with_name(name)) for name in
                               [Path(__file__).name, 'native_perf.py', 'dip_analyze.py',
                                'tea_dip_ci_repeat_evidence.py', 'tea_dip_query_endpoint_evidence.py',
                                'tea_dip_clock_alignment.py']},
                  scope='Closed default20M+20M CI point pair only. Own actual retirement denominators and core/native alignment. '
                        'DIP uses source ROI and all later final records. Sampled-versus-exact sign differences are deterministic '
                        'measurement discrepancies, not confidence intervals. Full1000cycle grids are required; completeness does not prove representative attribution. DIP and TEA describe different populations. '
                        'Different actual default seeds do not establish unique architecture causality. Native stage latency '
                        'validity is retained; invalid elapsed sums cannot explain stages. No partial suite score or adoption.')
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(dict(passed=True, speedup=value['speedup'], seeds_equal=value['seeds_equal'],
                          sampled_opposite_exact_TEA_states=value['sampled_opposite_exact_TEA_states'])))


if __name__ == '__main__':
    main()
