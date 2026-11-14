"""Locate sampled TEA state errors between exact cumulative prefixes in a closed guest."""
import argparse
from bisect import bisect_right
from fractions import Fraction
import json
from pathlib import Path
import sqlite3

from tea_dip_clock_alignment import core_native_alignment
from tea_dip_query_endpoint_evidence import STATES, digest, endpoint, verify


def partition(prefixes, samples, retired):
    blocks = []
    for before, after in zip(prefixes, prefixes[1:]):
        duration = after['cycle'] - before['cycle']
        exact = {state: after['states'][state] - before['states'][state] for state in STATES}
        assert duration > 0 and min(exact.values()) >= 0 and sum(exact.values()) == duration, 'Invalid cumulative TEA prefix interval'
        blocks.append(dict(begin=before['cycle'], end=after['cycle'], exact_state_cycles=exact,
                           sampled_state_rows={state: 0 for state in STATES}))
    assert blocks and samples and retired > 0
    boundaries = [prefix['cycle'] for prefix in prefixes]
    previous = None
    for cycle, state in samples:
        assert previous is None or cycle > previous, 'Duplicate or unordered TEA sample'
        previous = cycle
        assert boundaries[0] <= cycle < boundaries[-1] and 0 <= state < len(STATES), 'Sample outside prefix window or invalid state'
        blocks[bisect_right(boundaries, cycle) - 1]['sampled_state_rows'][STATES[state]] += 1
    duration = boundaries[-1] - boundaries[0]
    scale = Fraction(duration, retired * len(samples))
    for block in blocks:
        exact = {state: Fraction(value, retired) for state, value in block['exact_state_cycles'].items()}
        sampled = {state: value * scale for state, value in block['sampled_state_rows'].items()}
        block.update(exact_contribution_CPI={state: float(value) for state, value in exact.items()},
                     sampled_contribution_CPI={state: float(value) for state, value in sampled.items()},
                     sample_minus_exact_CPI={state: float(sampled[state] - exact[state]) for state in STATES})
    exact = {state: sum(block['exact_state_cycles'][state] for block in blocks) for state in STATES}
    sampled = {state: sum(block['sampled_state_rows'][state] for block in blocks) for state in STATES}
    assert sum(exact.values()) == duration and sum(sampled.values()) == len(samples)
    return dict(begin=boundaries[0], end=boundaries[-1], native_cycles=duration, native_retired=retired,
                sampled_cycles=len(samples), exact_state_cycles=exact, sampled_state_rows=sampled,
                exact_TEA_CPI={state: float(Fraction(value, retired)) for state, value in exact.items()},
                sampled_TEA_CPI={state: float(value * scale) for state, value in sampled.items()},
                sample_minus_exact_CPI={state: float(sampled[state] * scale - Fraction(exact[state], retired)) for state in STATES},
                blocks=blocks)


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    verify(folder, receipt['files'])
    result = json.loads((folder / 'result.json').read_text())
    assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
    assert result['source'] == receipt['source']
    evidence = endpoint(folder)
    assert evidence['native_roi_cycle_duration_matches']
    first, last = evidence['frames']
    begin, end = first['tea_cycle'], last['tea_cycle']
    with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        clock = core_native_alignment(db, folder / 'simulator_err.txt')
        assert all(frame['observed_epoch_offset'] == clock['core_to_native_offset'] for frame in evidence['frames'])
        rows = list(db.execute('SELECT * FROM TeaCycleSummary_0 WHERE CYCLE>? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end)))
        samples = [(row['CYCLE'], row['STATE']) for row in db.execute('SELECT CYCLE,STATE FROM Tea_0 WHERE CYCLE>=? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end))]
    prefixes = [dict(cycle=begin, states=first['states'])]
    prefixes += [dict(cycle=row['CYCLE'], states={state: row['STATES_' + str(i)] for i, state in enumerate(STATES)}) for row in rows]
    prefixes.append(dict(cycle=end, states=last['states']))
    data = partition(prefixes, samples, evidence['roi']['native_reported_retired'])
    assert data['exact_state_cycles'] == evidence['roi']['states']
    data.update(point=result['point'], source=result['source'], clock_alignment=clock,
                receipt_sha256=digest(receipt_path), tool_sha256=digest(Path(__file__)),
                scope='Closed single guest, exact native endpoints plus cumulative SQL prefixes. Samples use half-open source core-cycle intervals. Every sampled contribution uses the full ROI sample denominator and native CPI; every exact contribution uses its actual native retirement denominator. Empty sample blocks remain explicit. Block errors sum to whole-ROI sample-minus-exact state errors. These deterministic discrepancies are not confidence intervals, matched dynamic prefixes, critical-path identity or saved CPU cycles. No RTL state, build or simulation changes.')
    return data


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt), indent=2) + '\n')


if __name__ == '__main__':
    main()
