"""Measure phase sensitivity on a closed, sampler-neutral dense/sparse pair."""
import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
from types import SimpleNamespace
import sqlite3

from native_perf import read_blocks, unique_counters
import tea_visualize as tea
import dip_analyze as dip
from tea_dip_query_endpoint_evidence import endpoint, verify
from tea_dip_clock_alignment import core_native_alignment
from tea_dip_wb_protocol_micro import digest, write


def closed(root):
    receipt = json.loads((root / 'closure-receipt.json').read_text())
    assert receipt['ready'] and receipt['all_original_guests_closed'] and not receipt['controller_live']
    verify(root, receipt['files'])
    return receipt


def dip_measure(dense, sparse, start, end, period, phase, reference_cpi):
    profiles, results = [], []
    for folder, subset in [(dense, False), (dense, True), (sparse, False)]:
        with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
            db.row_factory = sqlite3.Row
            width = sum(row['name'].startswith('SLOTS_') and row['name'].endswith('_STATE')
                        for row in db.execute('PRAGMA table_info(Dip_0)'))
            query = 'SELECT * FROM Dip_0 WHERE CYCLE>=? AND CYCLE<?'
            params = [start, end]
            if subset:
                query += ' AND CYCLE%?=?'
                params += [period, phase]
            samples = db.execute(query + ' ORDER BY CYCLE,ID', params)
            finals = db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID')
            profile, summary = dip.analyze(samples, finals, width)
        profiles.append(profile)
        state_cpi = {state: value / summary['sampled_cycles'] * reference_cpi
                     for state, value in summary['state_cycles'].items()}
        state_cpi['Unresolved'] = summary['unresolved_cycles'] / summary['sampled_cycles'] * reference_cpi
        results.append(dict(sampled_cycles=summary['sampled_cycles'], unresolved_slots=summary['unresolved_slots'],
                            state_cpi=state_cpi))
    return dict(dense=results[0], dense_actual_phase=results[1], actual_sparse=results[2],
                actual_phase_profile_reproduced=profiles[1] == profiles[2] and results[1] == results[2],
                scope='DIP budgets and actual-phase resolved joint PC/cause profile only; other DIP phases are not enumerated.')


def measure(dense, sparse, period, calibration_path=None, *, full_roi=False):
    calibration = json.loads((calibration_path or dense / 'sampling-calibration.json').read_text())
    assert calibration['passed'] and calibration['all_native_multivalues_equal']
    assert calibration['terminal_counts_equal'] and all(calibration['auxiliary_tables_equal'].values())
    blocks = read_blocks(dense / 'simulator_err.txt'); assert len(blocks) == 2
    stamps = sorted(blocks)
    native = unique_counters(blocks[stamps[-1]])
    def native_value(suffix):
        values = [v for k, v in native.items() if k.endswith('.rob.' + suffix)]
        assert len(values) == 1
        return values[0]
    with sqlite3.connect((dense / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        clock = core_native_alignment(db, dense / 'simulator_err.txt')
        offset = clock['core_to_native_offset']
        if not full_roi:
            snapshots = db.execute('SELECT * FROM TeaCycleSummary_0 WHERE CYCLE>=? AND CYCLE<=? ORDER BY CYCLE',
                                   (stamps[0] + 1 - offset, stamps[-1] + 1 - offset)).fetchall()
    if full_roi:
        evidence = endpoint(dense)
        assert evidence['native_roi_cycle_duration_matches']
        start, end = [frame['tea_cycle'] for frame in evidence['frames']]
        retired = evidence['roi']['native_reported_retired']
        exact = evidence['roi']['states']
    else:
        first, last = snapshots[0], snapshots[-1]
        start, end = first['CYCLE'], last['CYCLE']
        retired = last['RETIRED'] - first['RETIRED']
        exact = {state: last['STATES_' + str(i)] - first['STATES_' + str(i)] for i, state in enumerate(tea.STATES)}
    assert retired > 0 and end > start
    assert sum(exact.values()) == end - start
    counts = [Counter() for _ in range(period)]; rows = [0] * period
    hashes = [hashlib.sha256() for _ in range(period)]; states = Counter()
    def receive(cycle, state, lanes, weight):
        phase = cycle % period; rows[phase] += 1; states[state] += 1
        hashes[phase].update(json.dumps([cycle, state, lanes, weight], separators=(',', ':')).encode() + b'\n')
        for pc, _ in lanes:
            counts[phase][state, pc] += weight
    args = SimpleNamespace(cycle_range=(start, end), spec_checkpoint=False, bins=1)
    data = tea.load_data((dense / 'simulator.db').resolve(), 'dense', args, sample_callback=receive)
    coverage = tea.sampling_coverage((dense / 'simulator.db').resolve(), (start, end), 1)
    assert sum(rows) == end - start and min(rows) > 0
    assert all(states[state] == exact[state] for state in tea.STATES)
    assert coverage['duplicate_sample_rows'] == coverage['missing_interior_samples'] == coverage['missing_endpoint_samples'] == 0
    assert abs(data['summary']['sample_weight'] - sum(rows)) < 1e-7
    observed = Counter(); sparse_hash = hashlib.sha256(); actual_phases = set(); sparse_rows = 0
    def receive_sparse(cycle, state, lanes, weight):
        nonlocal sparse_rows
        sparse_rows += 1; actual_phases.add(cycle % period)
        sparse_hash.update(json.dumps([cycle, state, lanes, weight], separators=(',', ':')).encode() + b'\n')
        for pc, _ in lanes:
            observed[state, pc] += weight
    sparse_data = tea.load_data((sparse / 'simulator.db').resolve(), 'sparse', args, sample_callback=receive_sparse)
    assert len(actual_phases) == 1
    actual_phase = next(iter(actual_phases))
    reproduced = hashes[actual_phase].hexdigest() == sparse_hash.hexdigest() and counts[actual_phase] == observed and rows[actual_phase] == sparse_rows
    assert reproduced, 'Dense subset did not reproduce actual sparse per-cycle resolved attribution'
    total = Counter()
    for counter in counts:
        total.update(counter)
    cpi = (end - start) / retired
    return dict(clock_alignment=clock, reference=dict(window='full_native_ROI' if full_roi else 'SQL_summary_interior',
                               start=start, end=end, cycles=end-start, retired=retired, cpi=cpi,
                               native_ROI_cpi=native_value('clock_cycle')/native_value('commitInstr'),
                               omitted_native_edge_cycles=native_value('clock_cycle')-(end-start)),
                actual_phase=actual_phase, actual_sparse_resolved_rows_reproduced=reproduced,
                DIP=dip_measure(dense, sparse, start, end, period, actual_phase, cpi),
                actual_sparse_sample_rows=sparse_rows, dense_summary=data['summary'], sparse_summary=sparse_data['summary']), counts, rows, total


def direction(value):
    return 0 if abs(value) < 1e-12 else 1 if value > 0 else -1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--dense', type=Path, required=True)
    parser.add_argument('--sparse', type=Path, required=True)
    parser.add_argument('--period', type=int, default=1000)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--full-roi', action='store_true',
                        help='Use exact native dump endpoints and retirement count instead of the summary interior')
    args = parser.parse_args()
    for root in [args.dense, args.sparse]:
        closed(root)
    runs = [measure(args.dense / v, args.sparse / v, args.period, full_roi=args.full_roi) for v in ['control', 'candidate']]
    assert runs[0][0]['actual_phase'] == runs[1][0]['actual_phase']
    def costs(run, counter, nrows):
        scale = run[0]['reference']['cpi'] / nrows
        return {key: value * scale for key, value in counter.items()}
    dense = [costs(run, run[3], sum(run[2])) for run in runs]
    phases = [[costs(run, run[1][p], run[2][p]) for p in range(args.period)] for run in runs]
    keys = dense[0].keys() | dense[1].keys()
    pcs = []
    for state, pc in sorted(keys, key=lambda k:(k[0], k[1] is None, k[1] or 0)):
        key = state, pc
        values = [phases[1][p].get(key, 0)-phases[0][p].get(key, 0) for p in range(args.period)]
        delta = dense[1].get(key, 0)-dense[0].get(key, 0)
        pcs.append(dict(state=state, pc=hex(pc) if pc is not None else 'unknown', dense_delta_cpi=delta,
                        actual_phase_delta_cpi=values[runs[0][0]['actual_phase']],
                        phase_delta_min=min(values), phase_delta_max=max(values),
                        opposite_sign_phases=sum(direction(v)*direction(delta)<0 for v in values)))
    state_rows = []
    for state in tea.STATES:
        delta = sum(v for (s,_),v in dense[1].items() if s==state)-sum(v for (s,_),v in dense[0].items() if s==state)
        values = [sum(v for (s,_),v in phases[1][p].items() if s==state)-sum(v for (s,_),v in phases[0][p].items() if s==state) for p in range(args.period)]
        state_rows.append(dict(state=state, dense_delta_cpi=delta, actual_phase_delta_cpi=values[runs[0][0]['actual_phase']],
                               phase_delta_min=min(values), phase_delta_max=max(values),
                               opposite_sign_phases=sum(direction(v)*direction(delta)<0 for v in values), phase_deltas=values))
    write(args.output, dict(period=args.period, runs=[run[0] for run in runs], states=state_rows, pc_state_sensitivity=pcs,
                            receipt_sha256=[digest(root/'closure-receipt.json')for root in [args.dense,args.sparse]],
                            tool_sha256=digest(Path(__file__)), parser_sha256=digest(Path(tea.__file__)),
                            DIP_parser_sha256=digest(Path(dip.__file__)),
                            scope='Deterministic phase enumeration over the explicitly selected exact native ROI or TeaCycleSummary interior. Reference windows and own retirement counts differ between variants; omitted edges remain explicit. Dense and sparse same-binary neutrality is required. Phase ranges are not confidence intervals, unique critical paths or suite scores. Sign epsilon1e-12 handles floating-point zero only.'))


if __name__ == '__main__':
    main()
