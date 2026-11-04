"""Audit a closed CI point without computing or publishing a partial suite score."""
import argparse
import csv
import hashlib
import json
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parent


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('point')
    parser.add_argument('--report-dir', type=Path, default=ROOT)
    parser.add_argument('--ci-evidence', choices=['ci-readiness','ci-credit-div','ci-fdiv-credit'], default='ci-readiness')
    args = parser.parse_args();point = args.point
    root = args.report_dir.resolve();base = root.parents[1]/'ci-41413249290'
    sys.path.insert(0, str(root))
    from analyze_windows import csvout, extract
    namespace, variant, expected_run = {
        'ci-readiness': ('ci','fpiq24',41654009482),
        'ci-credit-div': ('ci-credit-div','fp-credit-div',41811820348),
        'ci-fdiv-credit': ('ci-fdiv-credit','fp-credit-fdiv-only',41823116823),
    }[args.ci_evidence]
    rows = list(csv.DictReader((base / 'analysis/points.csv').open()))
    expected = next(row for row in rows if row['point'] == point)
    raw = root / namespace / 'raw'
    run = json.loads((raw / 'run.json').read_text())
    identity = json.loads((root / 'evidence' / args.ci_evidence / 'archive-identity.json').read_text())
    receipt = json.loads((raw / 'closed-receipts' / (point + '.json')).read_text())
    assert len(rows) == 145 and run['id'] == receipt['run_id'] == expected_run
    assert run['head_sha'] == receipt['source'] == identity['source']
    assert not receipt['original_process_live'] and receipt['runner_success_lines']
    assert receipt['artifact_sha256']['emu'] == identity['emu_sha256']
    assert receipt['artifact_sha256']['reference'] == identity['reference_sha256']
    assert receipt['artifact_sha256']['cst'] == '70c449a30aa94a9527d725bf888783a897cf896e9d7df19937fb0f56daed746d'
    candidate = raw / 'results' / point
    for name, entry in receipt['files'].items():
        body = (candidate / name).read_bytes()
        assert len(body) == entry['bytes'] and hashlib.sha256(body).hexdigest() == entry['sha256'], name
    stdout = (candidate / 'simulator_out.txt').read_text()
    assert 'The reference model is ' + identity['report'] + '/riscv64-nemu-interpreter-so' in stdout
    a, ap = extract(base / 'raw/results' / point, 'control')
    b, bp = extract(candidate, variant)
    assert abs(a['cpi'] - float(expected['cpi'])) < 1e-12
    assert a['seed'] == b['seed'] == int(expected['seed'])
    for audit in [a, b]:
        assert abs(audit['warmup_instructions'] - 20000000) < 128
        assert abs(audit['instructions'] - 20000000) < 128
    baseline_stdout = (base / 'raw/results' / point / 'simulator_out.txt').read_text()
    before_image = re.search(r'^The image is (.+)$', baseline_stdout, re.M)[1]
    assert re.search(r'^The image is (.+)$', stdout, re.M)[1] == before_image
    comparison = dict(point=point, variant=variant, control_cpi=a['cpi'], candidate_cpi=b['cpi'],
                      speedup=a['cpi'] / b['cpi'] - 1, delta_cpi=b['cpi'] - a['cpi'])
    for key in a['metrics']:
        comparison['delta_' + key] = b['metrics'][key] - a['metrics'][key]
    for view, states in [('TEA', ['Computing', 'Stalled', 'Flushed', 'Drained']),
                         ('DIP', ['Base', 'Stall', 'FrontEnd', 'Misspeculation', 'Unresolved'])]:
        assert abs(sum(comparison['delta_' + view + '_' + state] for state in states) - comparison['delta_cpi']) < 1e-9
    out = root / namespace / 'partial-analysis'
    out.mkdir(parents=True, exist_ok=True)
    symbols = {}
    symbol_sources = {}
    for name in ['dip_instruction_profiles.csv', 'hotspots.csv']:
        source = base / 'analysis' / name
        symbol_sources[name] = hashlib.sha256(source.read_bytes()).hexdigest()
        for row in csv.DictReader(source.open()):
            if row['workload'] == expected['workload']:
                symbols[row['pc']] = {key: row[key] for key in ['function', 'instruction']}
    def profiles(records):
        return {(row['view'], row['pc'], row['state'], row['signature'], row['labels']): row['cpi'] for row in records}
    ac, bc = profiles(ap), profiles(bp)
    deltas = []
    for key in ac.keys() | bc.keys():
        view, pc, state, signature, labels = key
        deltas.append(dict(view=view, point=point, pc=pc, state=state, signature=signature, labels=labels,
                           **symbols.get(pc, dict(function='unknown', instruction='unknown')),
                           control_cpi=ac.get(key, 0), candidate_cpi=bc.get(key, 0), delta_cpi=bc.get(key, 0)-ac.get(key, 0)))
    csvout(out / (point + '-pc-deltas.csv'), sorted(deltas, key=lambda row: -abs(row['delta_cpi'])))
    host_seconds = {label: int(re.search(r'Host time spent: ([\d,]+)ms', text)[1].replace(',', '')) / 1000
                    for label, text in [('control', baseline_stdout), ('candidate', stdout)]}
    result = dict(run_id=run['id'], source=run['head_sha'], official_status_at_collection=run['status'],
                  scope='One independently audited closed CI window only; no partial suite score, coverage adoption, or unmeasured-point equivalence claim.',
                  analysis_source_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(), comparison=comparison, control=a, candidate=b, host_elapsed_seconds=host_seconds, symbol_source_sha256=symbol_sources, closed_receipt_sha256=hashlib.sha256((raw / 'closed-receipts' / (point + '.json')).read_bytes()).hexdigest())
    (out / (point + '.json')).write_text(json.dumps(result, indent=2) + '\n')
    csvout(out / (point + '-control-pcs.csv'), ap)
    csvout(out / (point + '-candidate-pcs.csv'), bp)
    print(json.dumps(dict(scope=result['scope'], **comparison), indent=2))


if __name__ == '__main__':
    main()
