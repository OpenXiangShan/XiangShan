"""Audit one closed turnover CI point against841; never estimate a partial suite score."""
from pathlib import Path
import argparse
import csv
import json
import re
import sys

from native_perf import read_blocks, counter_multisets
from tea_dip_ci_mechanism_evidence import gap_window, state_delta_bounds, state_direction_audit
from tea_dip_query_endpoint_evidence import digest, old_execution, verify

NAMESPACE = 'fdiv-turnover-ci-20261108'
SOURCE = '8130b0ab046467542cc7c274076c436488cb86fd'
RUN = 42468823909
VARIANT = 'fdiv-turnover'
RAW_FILES = ['simulator_out.txt', 'simulator_err.txt', 'simulator.db']


def context(root):
    evidence = root / 'evidence' / NAMESPACE
    identity = json.loads((evidence / 'archive-identity.json').read_text())
    run = json.loads((evidence / 'run.json').read_text())
    artifact = json.loads((evidence / 'actual-artifact-verification.json').read_text())
    assert identity['run_id'] == run['id'] == artifact['run_id'] == RUN
    assert identity['source'] == run['head_sha'] == artifact['source'] == SOURCE
    assert artifact['passed'] and artifact['expected_points'] == 145
    assert artifact['artifact_sha256'] == dict(emu=identity['emu_sha256'], reference=identity['reference_sha256'], cst=identity['cst_sha256'])
    baseline = root.parents[1] / 'ci-41413249290'
    manifest = json.loads((baseline / 'analysis/manifest.json').read_text())
    before_run = json.loads((baseline / 'raw/run.json').read_text())
    assert before_run['id'] == 41413249290 and before_run['head_sha'] == manifest['source_sha'] == '84112d0b65231ca690a4ef2fea4b5169f2146e10'
    assert before_run['status'] == 'completed' and before_run['conclusion'] == 'success'
    names = ['analysis/points.csv', 'raw/profile.json']
    verify(baseline, {name: manifest['files'][name] for name in names})
    rows = list(csv.DictReader((baseline / 'analysis/points.csv').open()))
    seeds = json.loads((evidence / 'inputs/spec06-cov03-seeds.json').read_text())
    assert len(rows) == 145 and seeds == {row['point']: int(row['seed']) for row in rows}
    assert artifact['selected_profile'] == json.loads((baseline / 'raw/profile.json').read_text())
    return identity, baseline, rows, manifest


def closed_pair(before, after, point, variant):
    """Numerical analysis only; the caller must prove original closure and raw hashes."""
    from analyze_windows import extract
    from analyze_gaps import bounds
    windows, profiles = zip(*(extract(folder, label) for folder, label in [(before, 'control841'), (after, variant)]))
    a, b = windows
    assert a['point'] == b['point'] == point and a['seed'] == b['seed']
    for window in windows:
        assert abs(window['warmup_instructions'] - 20000000) < 128 and abs(window['instructions'] - 20000000) < 128
    stdout = [(folder / 'simulator_out.txt').read_text() for folder in [before, after]]
    assert re.search(r'^The image is (.+)$', stdout[0], re.M)[1] == re.search(r'^The image is (.+)$', stdout[1], re.M)[1]
    assert all(not any(marker in text for marker in ['ABORT', 'HIT BAD TRAP', 'HIT CRITICAL']) for text in stdout)
    comparison = dict(point=point, variant=variant, control_cpi=a['cpi'], candidate_cpi=b['cpi'],
                      speedup=a['cpi'] / b['cpi'] - 1, delta_cpi=b['cpi'] - a['cpi'])
    comparison.update({'delta_' + key: b['metrics'][key] - a['metrics'][key] for key in a['metrics']})
    for view, states in [('TEA', ['Computing', 'Stalled', 'Flushed', 'Drained']),
                         ('DIP', ['Base', 'Stall', 'FrontEnd', 'Misspeculation', 'Unresolved'])]:
        assert abs(sum(comparison['delta_' + view + '_' + state] for state in states) - comparison['delta_cpi']) < 1e-9
    phases = [sorted(read_blocks(folder / 'simulator_err.txt').items()) for folder in [before, after]]
    native = []
    for phase, ((at, ab), (bt, bb)) in zip(['warmup', 'roi'], zip(*phases)):
        ac, bc = counter_multisets(ab), counter_multisets(bb)
        common_changed = sorted(name for name in ac.keys() & bc.keys() if ac[name] != bc[name])
        added, removed = sorted(bc.keys() - ac.keys()), sorted(ac.keys() - bc.keys())
        native.append(dict(phase=phase, stamps=[at, bt], added_names=added, removed_names=removed,
                           changed_common_names=len(common_changed),
                           differences=[dict(name=name, control=ac.get(name), candidate=bc.get(name))
                                        for name in sorted(set(added + removed + common_changed))]))
    gap_windows, counts = zip(*(gap_window(folder, window, bounds) for folder, window in zip([before, after], windows)))
    intervals = state_delta_bounds(counts, windows)
    direction = state_direction_audit(intervals, comparison)
    error_bounds = {state: dict(lower=comparison['delta_TEA_' + state] - interval['upper'],
                                upper=comparison['delta_TEA_' + state] - interval['lower'])
                    for state, interval in intervals.items()}
    gaps = []
    for threshold in [128, 512]:
        delta = dict(threshold=threshold)
        for kind in ['long', 'short']:
            delta[kind + '_lower'] = gap_windows[1][threshold][kind + '_cycles_lower'] / b['instructions'] - gap_windows[0][threshold][kind + '_cycles_upper'] / a['instructions']
            delta[kind + '_upper'] = gap_windows[1][threshold][kind + '_cycles_upper'] / b['instructions'] - gap_windows[0][threshold][kind + '_cycles_lower'] / a['instructions']
            assert delta[kind + '_lower'] <= delta[kind + '_upper']
        gaps.append(delta)
    def profile(rows):
        return {(r['view'], r['pc'], r['state'], r['signature'], r['labels']): r['cpi'] for r in rows}
    ac, bc = map(profile, profiles)
    pcs = [dict(view=key[0], point=point, pc=key[1], state=key[2], signature=key[3], labels=key[4],
                control_cpi=ac.get(key, 0), candidate_cpi=bc.get(key, 0), delta_cpi=bc.get(key, 0) - ac.get(key, 0))
           for key in sorted(ac.keys() | bc.keys(), key=str)]
    pcs.sort(key=lambda row: (-abs(row['delta_cpi']), row['pc'], row['state'], str(row['signature'])))
    equality = old_execution(before, after, set()) if comparison['speedup'] == 0 else None
    return dict(comparison=comparison, control=a, candidate=b, native_phase_differences=native,
                complete_observation_equality=equality, native_roi_state_delta_cpi_bounds=intervals,
                native_roi_state_direction_audit=direction, sampled_minus_native_state_delta_cpi_bounds=error_bounds,
                gap_delta_cpi_bounds=gaps, coverage=[w['coverage'] for w in windows],
                host_elapsed_seconds={label: int(re.search(r'Host time spent: ([\d,]+)ms', text)[1].replace(',', '')) / 1000
                                      for label, text in zip(['control', 'candidate'], stdout)},
                top_sampled_pc_deltas={view: [p for p in pcs if p['view'] == view][:8] for view in ['TEA', 'DIP']}), pcs


def analyze(root, point):
    identity, baseline, rows, manifest = context(root)
    expected, = [row for row in rows if row['point'] == point]
    namespace = root / NAMESPACE
    receipt_path = namespace / 'raw/closed-receipts' / (point + '.json')
    if not receipt_path.exists():
        return dict(pending=True, point=point, database_read=False, reason='Original closed receipt is unavailable')
    receipt = json.loads(receipt_path.read_text())
    assert receipt['run_id'] == RUN and receipt['source'] == SOURCE
    assert not receipt['original_process_live'] and receipt['runner_success_lines']
    assert receipt['artifact_sha256'] == dict(emu=identity['emu_sha256'], reference=identity['reference_sha256'], cst=identity['cst_sha256'])
    after = namespace / 'raw/results' / point
    assert set(receipt['files']) == set(RAW_FILES)
    verify(after, receipt['files'])
    before = baseline / 'raw/results' / point
    names = ['raw/results/' + point + '/' + name for name in RAW_FILES]
    verify(baseline, {name: manifest['files'][name] for name in names})
    assert 'The reference model is ' + identity['report'] + '/riscv64-nemu-interpreter-so' in (after / 'simulator_out.txt').read_text()
    result, pcs = closed_pair(before, after, point, VARIANT)
    assert abs(result['control']['cpi'] - float(expected['cpi'])) < 1e-12
    assert result['control']['seed'] == result['candidate']['seed'] == int(expected['seed'])
    symbols = {}
    symbol_sources = {}
    for name in ['dip_instruction_profiles.csv', 'hotspots.csv']:
        path = baseline / 'analysis' / name
        symbol_sources[name] = digest(path)
        for row in csv.DictReader(path.open()):
            if row['workload'] == expected['workload']:
                symbols[row['pc']] = {key: row[key] for key in ['function', 'instruction']}
    for pc in pcs:
        pc.update(symbols.get(pc['pc'], dict(function='unknown', instruction='unknown')))
    result.update(pending=False, database_read=True, run_id=RUN, source=SOURCE,
                  closed_receipt_sha256=digest(receipt_path), baseline_manifest_sha256=digest(baseline / 'analysis/manifest.json'),
                  symbol_source_sha256=symbol_sources, analysis_source_sha256=digest(Path(__file__)),
                  helper_sha256={name: digest(Path(__file__).with_name(name)) for name in ['tea_dip_ci_mechanism_evidence.py', 'tea_dip_query_endpoint_evidence.py', 'native_perf.py']},
                  report_helper_sha256={name: digest(root / name) for name in ['analyze_windows.py', 'analyze_gaps.py']},
                  scope='One original closed turnoverCI window against841. Native CPI and separate exclusive DIP budgets; TEA full-ROI state bounds retain unknown edge cycles and marginal extrema are not jointly additive or confidence intervals. Added/removed counter names stay explicit and cannot prove execution changes or be silently excluded as diagnostics. PC evidence is sampled attribution, not a unique dynamic cause. No partial-suite score, isolated host cost or adoption claim.')
    out = namespace / 'partial-analysis'
    out.mkdir(parents=True, exist_ok=True)
    from analyze_windows import csvout
    csvout(out / (point + '-pc-deltas.csv'), pcs)
    (out / (point + '.json')).write_text(json.dumps(result, indent=2) + '\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('point')
    args = parser.parse_args()
    root = args.report_dir.resolve()
    sys.path.insert(0, str(root))
    result = analyze(root, args.point)
    print(json.dumps({key: result[key] for key in ['pending', 'database_read', 'point', 'reason'] if key in result}
                     if result['pending'] else dict(pending=False, run_id=RUN, **result['comparison']), indent=2))


if __name__ == '__main__':
    main()
