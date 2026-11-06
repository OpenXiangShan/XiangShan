"""Compare closed WB-query formal windows at common retirement milestones."""
from pathlib import Path
import argparse, hashlib, importlib.util, json, sqlite3, sys


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def helper(root, folder, filename, expected):
    path = root / 'evidence' / folder / filename
    assert digest(path) == expected, 'Retirement helper source changed'
    spec = importlib.util.spec_from_file_location(filename.removesuffix('.py'), path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module, path


def analyze(root, point):
    sys.path.insert(0, str(root))
    from analyze_queue_observation import verify_files
    evidence = root / 'evidence/fdiv-wb-query'
    spec_path = evidence / 'formal-experiment.json'
    spec = json.loads(spec_path.read_text())
    assert point in {case['point'] for case in spec['cases']}
    record_path = root / 'wb-query-formal/analysis/mechanism-evidence.json'
    record = json.loads(record_path.read_text())
    rows = [row for row in record['rows'] if row['point'] == point]
    if not rows:
        return dict(point=point, pending=True, database_read=False,
                    reason='No original closed formal mechanism row')
    row, = rows
    source_path = evidence / 'source.json'
    build_path = evidence / 'build-state.json'
    source, build = [json.loads(path.read_text()) for path in [source_path, build_path]]
    assert source['head'] == record['source'] == build['source'] and build['phase'] == 'complete'
    assert source['base'] == record['base'] == spec['base'] == '84112d0b65231ca690a4ef2fea4b5169f2146e10'
    receipt_path = root / 'wb-query-formal/closed-receipts' / (point + '.json')
    receipt = json.loads(receipt_path.read_text())
    assert row['closed_receipt_sha256'] == digest(receipt_path)
    assert not receipt['original_process_live'] and receipt['exitcode'] == '0'
    assert receipt['source'] == source['head'] and receipt['binary_sha256'] == build['binary_sha256']
    assert receipt['last_live_observation']['live'] and all(receipt['process'][key] == receipt['last_live_observation'][key]
                                                        for key in ['pid', 'start_ticks', 'host', 'core'])
    locations = [('control', 'formal', 'rebuild-control/points/' + point + '/'),
                 ('candidate', 'wb-query-formal', 'fdiv-wb-query/points/' + point + '/')]
    paths, sources = [], [record_path, receipt_path, spec_path, source_path, build_path]
    for label, stage, prefix in locations:
        manifest_path = root / stage / 'manifest.json'
        manifest = json.loads(manifest_path.read_text())
        identity_path = root / stage / prefix / 'result.json'
        identity = json.loads(identity_path.read_text())
        assert identity['binary_sha256'] == (spec['control_binary_sha256'] if label == 'control' else build['binary_sha256'])
        if label == 'candidate':
            assert identity['source'] == source['head']
            assert all(manifest[prefix + name] == entry for name, entry in receipt['files'].items())
        verify_files(root / stage, {key: entry for key, entry in manifest.items() if key.startswith(prefix)})
        database = root / stage / prefix / 'simulator.db'
        assert database.stat().st_size == manifest[prefix + 'simulator.db']['bytes']
        assert digest(database) == manifest[prefix + 'simulator.db']['sha256']
        paths.append((label, database))
        sources.extend([manifest_path, identity_path])
    exact, exact_path = helper(root, 'retired-progress-tools', 'tea_dip_retired_progress.py',
                               'eecb561994f7e5d3028ac0b5530e46b6a92406eb0b6570580a8fe62c18bf8337')
    dispatch, dispatch_path = helper(root, 'retired-dispatch-tools', 'tea_dip_retired_dispatch.py',
                                     '34cf81b765e2606fc6685c74c40df8ac391c3ae20a4953ecd346bc3b194546f2')
    from analyze_windows import DIP
    windows = []
    for label, path in paths:
        with sqlite3.connect(path.resolve().as_uri() + '?mode=ro', uri=True) as db:
            db.row_factory = sqlite3.Row
            snapshots = list(db.execute('SELECT * FROM TeaCycleSummary_0 ORDER BY CYCLE,ID'))
            samples = list(db.execute('SELECT * FROM Dip_0 ORDER BY CYCLE,ID'))
            finals = list(db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'))
            width = sum(c['name'].startswith('SLOTS_') and c['name'].endswith('_STATE') for c in db.execute('pragma table_info(Dip_0)'))
            matches = list(db.execute('SELECT s.RETIRED,d.TOTALRETIRED FROM TeaCycleSummary_0 s JOIN Dip_0 d ON s.CYCLE=d.CYCLE'))
        assert width > 0 and matches and all(m['RETIRED'] == m['TOTALRETIRED'] for m in matches)
        windows.append(dict(label=label, database_sha256=digest(path), dispatch_width=width,
                            milestones=exact.brackets(snapshots), same_cycle_retirement_matches=len(matches),
                            **dispatch.sample_bands(DIP, samples, finals, width, spec['period'])))
    a, b = windows
    assert a['dispatch_width'] == b['dispatch_width']
    segments = []
    for i, (low, high) in enumerate(zip(exact.TARGETS, exact.TARGETS[1:])):
        count = high - low
        x, y = [window['milestones'][i:i + 2] for window in windows]
        lower = (y[1]['cycle_lower'] - y[0]['cycle_upper'] - x[1]['cycle_upper'] + x[0]['cycle_lower']) / count
        upper = (y[1]['cycle_upper'] - y[0]['cycle_lower'] - x[1]['cycle_lower'] + x[0]['cycle_upper']) / count
        states = {state: dict(lower=(y[1]['states_lower'][state] - y[0]['states_upper'][state] - x[1]['states_upper'][state] + x[0]['states_lower'][state]) / count,
                              upper=(y[1]['states_upper'][state] - y[0]['states_lower'][state] - x[1]['states_lower'][state] + x[0]['states_upper'][state]) / count)
                  for state in exact.STATES}
        assert lower <= upper and all(v['lower'] <= v['upper'] for v in states.values())
        before, after = [next(band for band in window['bands'] if band['retired_start'] == low and band['retired_end'] == high) for window in windows]
        def bins(band):
            return {(v['state'], v['mask']): v['slots'] for v in band['exclusive_state_mask']}
        old, new = bins(before), bins(after)
        scale = spec['period'] / (a['dispatch_width'] * count)
        masks = [dict(state=state, mask=mask, delta=(new.get((state, mask), 0) - old.get((state, mask), 0)) * scale)
                 for state, mask in sorted(old.keys() | new.keys())]
        total = (after['sampled_cycles'] - before['sampled_cycles']) * spec['period'] / count
        assert abs(sum(m['delta'] for m in masks) - total) < 1e-12
        segments.append(dict(retired_start=low, retired_end=high, exact_delta_cpi_lower=lower,
                             exact_delta_cpi_upper=upper, exact_state_delta_cpi_bounds=states,
                             dip_sampled_equivalent_delta=total,
                             dip_exclusive_state_delta={state: sum(m['delta'] for m in masks if m['state'] == state) for state in dispatch.STATES},
                             dip_exclusive_state_mask_delta=masks))
    return dict(point=point, source=source['head'], stage='wb-query-formal', pending=False,
                database_read=True, native_comparison=row['comparison'], windows=windows, segments=segments,
                source_sha256={str(path.relative_to(root)): digest(path) for path in [*sources, exact_path, dispatch_path]},
                consumer_sha256=digest(Path(__file__)),
                scope='Original closed WB-query formal candidate and calibrated841 control only. Reuses exact snapshot bracketing and full deferred DIP source-band reconstruction.5M–35M common retirement, no interpolation;0–5M/35M-to-end uncovered. Exact ranges are conservative, DIP values sampled equivalent, neither service time, confidence interval, unique dynamic uop nor partial-suite score.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--point', required=True)
    args = parser.parse_args()
    root = args.report_dir.resolve()
    result = analyze(root, args.point)
    if not result['pending']:
        (root / 'wb-query-formal/analysis' / (args.point + '-retired-phases.json')).write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps({key: value for key, value in result.items() if key in ['point', 'pending', 'database_read', 'reason', 'native_comparison', 'segments']}, indent=2))


if __name__ == '__main__':
    main()
