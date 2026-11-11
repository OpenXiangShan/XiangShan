"""Summarize audited INT windows against their complete formal profile."""
from pathlib import Path
import argparse
import hashlib
import json
import math


def summarize(formal, analyses):
    assert formal['passed']
    programs = {p['benchmark']: p for p in formal['programs']}
    points = {p['point']: p for p in formal['points']}
    rows = []
    outside = []
    for path, analysis in analyses:
        assert analysis['passed']
        name = analysis['point']
        if name not in points:
            outside.append(dict(analysis=str(path), point=name))
            continue
        point = points[name]
        program = programs[point['benchmark']]
        control, candidate = [analysis['outputs'][side]['audit'] for side in ['control', 'candidate']]
        for audit in [control, candidate]:
            assert audit['instructions'] > 0 and audit['cycles'] > 0
            assert audit['cpi'] == audit['cycles'] / audit['instructions']
        observed = control['cpi'] / candidate['cpi'] - 1
        assert math.isclose(observed, analysis['observed_speedup'], abs_tol=1e-14)
        contribution = point['weight'] * point['candidate_cpi']
        hypothetical_program_cpi = program['candidate_cpi'] - contribution * (1 - 1 / 1.01)
        sensitivity = (program['candidate_cpi'] / hypothetical_program_cpi) ** (1 / len(programs)) - 1
        rows.append(dict(analysis=str(path), point=name, benchmark=point['benchmark'],
                         weight=point['weight'], program_cycle_share=contribution / program['candidate_cpi'],
                         formal_CPI=point['candidate_cpi'], local_control_CPI=control['cpi'],
                         local_to_formal_CPI_ratio=control['cpi'] / point['candidate_cpi'],
                         warmup_retired=control['warmup_instructions'], ROI_retired=control['instructions'],
                         samples=control['samples'], local_seed=control['seed'], formal_seed=point['actual_seed'],
                         seeds_equal=control['seed'] == point['actual_seed'],
                         observed_speedup=observed, hypothetical_INT_gain_if_point_1pct_faster=sensitivity,
                         formal_load_head_CPI=point['candidate_counters']['waitLoadCycle_cpi'],
                         local_load_head_CPI=control['counters']['waitLoadCycle_cpi'],
                         formal_branch_mispredicts_per_kinst=point['candidate_counters']['commit_branch_mispredicts_per_kinst'],
                         local_branch_mispredicts_per_kinst=control['counters']['commit_branch_mispredicts_per_kinst']))
    selected = {row['point'] for row in rows}
    coverage = []
    missing = []
    for name, program in programs.items():
        all_points = [p for p in points.values() if p['benchmark'] == name]
        assert math.isclose(sum(p['weight'] * p['candidate_cpi'] for p in all_points), program['candidate_cpi'], rel_tol=1e-12)
        chosen = [p for p in all_points if p['point'] in selected]
        coverage.append(dict(benchmark=name, selected_points=len(chosen), formal_points=len(all_points),
                             selected_profile_weight=sum(p['weight'] for p in chosen),
                             selected_program_cycle_share=sum(p['weight'] * p['candidate_cpi'] for p in chosen) / program['candidate_cpi']))
        if not chosen:
            point = max(all_points, key=lambda p: p['weight'] * p['candidate_cpi'])
            missing.append(dict(benchmark=name, largest_cycle_share_point=point['point'],
                                program_cycle_share=point['program_cycle_share']))
    return dict(rows=rows, outside_INT_profile=outside, coverage=coverage, uncovered_programs=missing,
                selected_unique_INT_points=len(selected), covered_INT_programs=sum(c['selected_points'] > 0 for c in coverage),
                formal_INT_points=len(points), formal_INT_programs=len(programs),
                scope='Window-selection evidence from previously audited summaries, not a new raw-result audit. '
                      'CPI differences may reflect warmup, phase and seeds; a short prefix is not the formal ROI. '
                      'Profile weights and sensitivity describe importance, never predicted candidate gains. '
                      'Coverage is reported without reweighting missing points into a suite score.')


def branch_pressure(formal, branch_summary, rows):
    assert branch_summary['passed']
    assert (branch_summary['source'], branch_summary['run_id']) == (formal['source'], formal['run_id'])
    assert {p['point'] for p in branch_summary['points']} == {p['point'] for p in formal['points']}
    selected = {r['point'] for r in rows}
    guards = {}
    for row in rows:
        if row['local_branch_mispredicts_per_kinst'] == 0:
            guards.setdefault(row['benchmark'], set()).add(row['point'])
    targets = []
    for benchmark, zero_windows in sorted(guards.items()):
        ranked = []
        for point in branch_summary['points']:
            if point['benchmark'] != benchmark:
                continue
            frame = next(f for f in point['frames'] if f['phase'] == 'ROI')
            rate = frame['conditional_mispredict_reasons']['SC'] * 1000 / frame['retired']
            ranked.append((point['weight'] * rate, point['point'], rate))
        priority, name, rate = max(ranked)
        targets.append(dict(benchmark=benchmark, zero_mispredict_windows=sorted(zero_windows),
                            largest_weighted_SC_mispredict_point=name, formal_SC_mispredicts_per_kinst=rate,
                            formal_weighted_SC_mispredicts_per_kinst=priority, point_already_selected=name in selected))
    return targets


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--formal', type=Path, required=True)
    parser.add_argument('--analyses', type=Path, nargs='+', required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--branch-summary', type=Path,
                        help='Audited complete formal branch summary for zero-mispredict window checks')
    args = parser.parse_args()
    data = summarize(json.loads(args.formal.read_text()), [(p, json.loads(p.read_text())) for p in args.analyses])
    sources = [args.formal, *args.analyses, Path(__file__)]
    if args.branch_summary:
        data['branch_pressure_checks'] = branch_pressure(
            json.loads(args.formal.read_text()), json.loads(args.branch_summary.read_text()), data['rows'])
        sources.append(args.branch_summary)
        data['scope'] += (' Zero-mispredict windows remain valid guards. Target ranking is formal SC-tagged '
                          'conditional-mispredict exposure, not accuracy, saved cycles or a run instruction.')
    digest = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
    data['source_sha256'] = {str(p): digest(p) for p in sources}
    args.output.write_text(json.dumps(data, indent=2) + '\n')
    print(json.dumps({key: data[key] for key in ['selected_unique_INT_points', 'covered_INT_programs', 'formal_INT_points', 'formal_INT_programs']}))


if __name__ == '__main__':
    main()
