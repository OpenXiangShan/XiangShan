"""Audit original closed formal WB-query endpoints against immutable 841/b0a windows."""
from pathlib import Path
import argparse
import hashlib
import importlib.util
import json
import sys

POINTS = [
    'gromacs_105_0.206090',
    'lbm_6382_0.375486',
    'sphinx3_157921_0.136728',
]
SOURCES = {
    'control_endpoint': 'addd8cc4d35a5a3f5f3fa3cfe8371280ff787807',
    'query_endpoint': '2915a6b022bd957570bd961a01611026c2220fb7',
}


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def verified_files(base, entries):
    for name, entry in entries.items():
        path = base / name
        assert path.stat().st_size == entry['bytes'] and digest(path) == entry['sha256'], path


def helper(root):
    path = Path(__file__).with_name('tea_dip_query_endpoint_evidence.py')
    spec = importlib.util.spec_from_file_location('closed_query_endpoint_formal_helper', path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module, path


def analyze(root):
    stage = root / 'wb-query-endpoint-formal'
    receipt_path = stage / 'closure-receipt.json'
    if not receipt_path.exists():
        return dict(pending=True, database_read=False, completed=0, expected=3,
                    reason='Original formal endpoint controller and six guests are not archived closed')
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['controller_live'] and receipt['all_original_guests_closed']
    assert receipt['controller']['pid'] == 1783927 and receipt['controller']['start_ticks'] == '62152493'
    assert receipt['controller']['phase'] == 'complete' and receipt['controller']['passed']
    assert receipt['controller']['completed'] == receipt['controller']['total'] == 6
    verified_files(stage, receipt['files'])
    spec = json.loads((stage / 'experiment.json').read_text())
    assert len(spec['cases']) == 6 and spec['builds']['control_endpoint']['source'] == SOURCES['control_endpoint']
    assert spec['builds']['query_endpoint']['source'] == SOURCES['query_endpoint']
    assert spec['builds']['control_endpoint']['binary_sha256'] == 'c136ef1df8210da3f5668276355b36d85b0f4465cf868edfe55fa3c6f663e496'
    assert spec['builds']['query_endpoint']['binary_sha256'] == '769581c904127d6e5ea88b14afb9b123098bedc03cae2317d1e24ceae73206d9'
    assert spec['runner_sha256'] == digest(root / 'evidence/query-endpoint-validation/tea_dip_query_endpoint_formal.py')
    short_path = root / 'wb-query-endpoint-focused/analysis/evidence.json'
    short_receipt = root / 'wb-query-endpoint-focused/closure-receipt.json'
    short = json.loads(short_path.read_text())
    assert short['all_observation_pairs_neutral'] and short['completed'] == short['expected'] == 3
    assert spec['focused_evidence_sha256'] == digest(short_path)
    assert spec['focused_receipt_sha256'] == digest(short_receipt) == short['closed_receipt_sha256']
    assert spec['gate_consumer_sha256'] == short['consumer_sha256']
    baseline_build_dir = root / 'evidence/rebuild-control'
    baseline_manifest = json.loads((baseline_build_dir / 'build-coremark-manifest.json').read_text())
    verified_files(baseline_build_dir, {'build-state.json': baseline_manifest['build-state.json']})
    baseline_build = json.loads((baseline_build_dir / 'build-state.json').read_text())
    assert baseline_build['phase'] == 'complete'
    assert baseline_build['source'] == '84112d0b65231ca690a4ef2fea4b5169f2146e10'
    assert baseline_build['binary_sha256'] == '02207e4f4165926a87a67e282c3e0623910dc10a8d6cef1bd3dd60c6e5244def'
    baseline_dir = root / 'formal'
    candidate_dir = root / 'wb-query-formal'
    baseline_manifest = json.loads((baseline_dir / 'manifest.json').read_text())
    candidate_manifest = json.loads((candidate_dir / 'manifest.json').read_text())
    original = json.loads((candidate_dir / 'analysis/mechanism-evidence.json').read_text())
    assert original['completed'] == 3 and len(original['rows']) == 3
    case_rows = {}
    for case in spec['cases']:
        variant, point = case['variant'], case['point']
        assert variant in SOURCES and point in POINTS
        current = stage / variant / point
        result = json.loads((current / 'result.json').read_text())
        process = json.loads((current / 'process.json').read_text())
        launch = json.loads((current / 'launch-observation.json').read_text())
        assert result['source'] == SOURCES[variant]
        assert result['binary_sha256'] == spec['builds'][variant]['binary_sha256']
        assert result['process'] == process and result['original_process_closed']
        assert result['exitcode'] == 0 and result['difftest_passed'] and all(result['sampler_flags'].values())
        assert launch['live'] and all(launch[name] == value for name, value in process.items())
        assert (current / 'exitcode').read_text().strip() == '0'
        assert case['seed'] == dict(zip(POINTS, [9457, 7058, 4410]))[point]
        assert case['reference_sha256'] == '9188e7dda30709962f0bfbc1dd87cea73d88a18c92c30892f93ed529c22def43'
        assert case['cst_sha256'] == '70c449a30aa94a9527d725bf888783a897cf896e9d7df19937fb0f56daed746d'
        case_rows[variant, point] = current
    assert set(case_rows) == {(variant, point) for variant in SOURCES for point in POINTS}
    for point in POINTS:
        baseline = baseline_dir / 'rebuild-control/points' / point
        candidate = candidate_dir / 'fdiv-wb-query/points' / point
        for base, entries, prefix in [
            (baseline_dir, baseline_manifest, f'rebuild-control/points/{point}/'),
            (candidate_dir, candidate_manifest, f'fdiv-wb-query/points/{point}/'),
        ]:
            selected = {name: row for name, row in entries.items() if name.startswith(prefix)}
            assert all(prefix + name in selected for name in
                       ['command.json', 'result.json', 'exitcode', 'simulator_out.txt', 'simulator_err.txt', 'simulator.db', 'process.json'])
            verified_files(base, selected)
        assert (baseline / 'exitcode').read_text().strip() == '0'
        assert (candidate / 'exitcode').read_text().strip() == '0'
        b = json.loads((baseline / 'result.json').read_text())
        c = json.loads((candidate / 'result.json').read_text())
        assert b['binary'] == baseline_build['binary'] and b['binary_sha256'] == baseline_build['binary_sha256']
        assert c['source'] == 'b0a22b93a867c5dff56b1b3fb32708c9e6363b01'
        assert c['binary_sha256'] == '401a3d8b0258a8d91fadf9fc1fae8dd207c6508938b146ff589f8f0f0247bb9e'
        old_receipt = json.loads((candidate_dir / 'closed-receipts' / (point + '.json')).read_text())
        assert not old_receipt['original_process_live'] and old_receipt['exitcode'] == '0'
        assert old_receipt['source'] == c['source'] and old_receipt['binary_sha256'] == c['binary_sha256']
        assert all(digest(candidate / name) == row['sha256'] and (candidate / name).stat().st_size == row['bytes']
                   for name, row in old_receipt['files'].items())
        commands = [json.loads((folder / 'command.json').read_text()) for folder in
                    [baseline, candidate, case_rows['control_endpoint', point], case_rows['query_endpoint', point]]]
        for flag in ['-W', '-I', '-C', '-s', '-i', '--diff', '--dump-select-db']:
            assert len({command[command.index(flag) + 1] for command in commands}) == 1, (point, flag)
        assert commands[0][commands[0].index('-W') + 1] == '20000000'
        assert commands[0][commands[0].index('-I') + 1] == '40000000'
    sys.path.insert(0, str(root))
    from native_perf import read_blocks
    tools, helper_path = helper(root)
    rows = []
    for point in POINTS:
        old_control = baseline_dir / 'rebuild-control/points' / point
        old_query = candidate_dir / 'fdiv-wb-query/points' / point
        control = case_rows['control_endpoint', point]
        query = case_rows['query_endpoint', point]
        before = read_blocks(old_control / 'simulator_err.txt')
        after = read_blocks(control / 'simulator_err.txt')
        original_names = {name for block in before.values() for name in block}
        observed_names = {name for block in after.values() for name in block}
        assert original_names <= observed_names
        new_names = observed_names - original_names
        control_neutrality = tools.old_execution(old_control, control, new_names)
        query_neutrality = tools.old_execution(old_query, query, new_names)
        comparison = tools.pair(control, query, expected_instructions=20000000)
        prior = next(row for row in original['rows'] if row['point'] == point)['comparison']
        original_cpi_reproduced = all(comparison[key] == prior[old_key] for key, old_key in
                                      [('control_cpi', 'control_cpi'), ('candidate_cpi', 'candidate_cpi'),
                                       ('observed_native_speedup', 'speedup'), ('delta_cpi', 'delta_cpi')])
        neutral = control_neutrality['passed'] and query_neutrality['passed'] and original_cpi_reproduced
        rows.append(dict(point=point, control_instrumentation_neutrality=control_neutrality,
                         query_instrumentation_neutrality=query_neutrality,
                         original_formal_cpi_reproduced=original_cpi_reproduced,
                         observation_neutrality_valid=neutral,
                         performance_speedup=comparison['observed_native_speedup'] if neutral else None,
                         comparison=comparison))
    return dict(pending=False, database_read=True, completed=len(rows), expected=3,
                all_observation_pairs_neutral=all(row['observation_neutrality_valid'] for row in rows),
                consumer_sha256=digest(Path(__file__)), helper_sha256=digest(helper_path),
                original_focused_gate_sha256=digest(short_path),
                closed_receipt_sha256=digest(receipt_path),
                original_formal_source_sha256=digest(candidate_dir / 'analysis/mechanism-evidence.json'),
                rows=rows,
                performance_causality_validated=None,
                scope='Three existing formal20M+20M g105/lbm/sphinx selected pairs. Six original new GSIM guests and immutable old841/b0a controls are source/binary/config/closure/SHA-verified before DB. Exact diagnostic name set comes from same-point old841/newaddd difference; no wildcard exclusions. All old native multivalues and six complete tables must match on both sides, with original CPI reproduced, before speedup is valid. Exact TEA state and edges use native ROI retirement only after observed duration match; DIP exclusive and native overlap remain separate. This is neither a full suite score nor adoption proof.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    args = parser.parse_args()
    root = args.report_dir.resolve()
    result = analyze(root)
    if not result['pending']:
        output = root / 'wb-query-endpoint-formal/analysis'
        output.mkdir(exist_ok=True)
        (output / 'evidence.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
