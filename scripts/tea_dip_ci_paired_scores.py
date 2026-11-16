"""Score two complete current-format CIs using the original145point SPEC profile.

Both sides require closed CI receipts and native exact TEA endpoints. The legacy
841 score entry remains separate. No CI dispatch or scheduling is performed.
"""
import argparse
from concurrent.futures import ThreadPoolExecutor
import csv
import json
from pathlib import Path

from tea_dip_ci_point_budget import budget, compare
from tea_dip_query_ci_scores import aggregate, scores
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_turnover_ci_scores import score_snapshot


VARIANT = 'paired-default-ci'


def audit_pair(control, candidate, point, control_run, candidate_run):
    values, receipts = [], []
    for root, run in [(control, control_run), (candidate, candidate_run)]:
        receipt_path = root / 'raw/closed-receipts' / (point + '.json')
        receipt = json.loads(receipt_path.read_text())
        assert receipt['run_id'] == run['id'] and receipt['source'] == run['head_sha']
        assert receipt['runner_success_lines']
        value = budget(root / 'raw/results' / point, receipt_path)
        assert value['native']['run_id'] == run['id'] and value['native']['source'] == run['head_sha']
        values.append(value)
        receipts.append(receipt)
    result = compare(*values)
    result.update(point=point,
                  candidate_report=str(Path(receipts[1]['last_live_process_observation']['args'][0]).parent),
                  raw_sha256={str(root / 'raw/results' / point / name): entry['sha256']
                              for root, receipt in zip([control, candidate], receipts)
                              for name, entry in receipt['files'].items()})
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ['control-ci', 'candidate-ci', 'candidate-run', 'profile-dir', 'output-dir']:
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--archive-host', default='node032', help='Read-only access to the existing score, not CI selection')
    args = parser.parse_args()
    control, candidate = args.control_ci.resolve(), args.candidate_ci.resolve()
    control_run_path = control / 'analysis/official-run.json'
    control_run = json.loads(control_run_path.read_text())
    candidate_run = json.loads(args.candidate_run.read_text())
    for role, run in [('control', control_run), ('candidate', candidate_run)]:
        if run['status'] != 'completed':
            print(json.dumps(dict(pending=True, score_computed=False, side=role, run_id=run['id'],
                                  reason='Official CI is not terminal')))
            return
        assert run['conclusion'] == 'success', role + ' official CI did not succeed'
    profile_root = args.profile_dir.resolve()
    manifest = json.loads((profile_root / 'analysis/manifest.json').read_text())
    names = ['analysis/points.csv', 'raw/profile.json']
    verify(profile_root, {name: manifest['files'][name] for name in names})
    metadata = list(csv.DictReader((profile_root / 'analysis/points.csv').open()))
    expected = {row['point'] for row in metadata}
    assert len(metadata) == len(expected) == 145
    for role, root in [('control', control), ('candidate', candidate)]:
        closed = {p.stem for p in (root / 'raw/closed-receipts').glob('*.json')}
        assert closed <= expected, role + ' has an unexpected point'
        if closed != expected:
            print(json.dumps(dict(pending=True, score_computed=False, side=role, closed_points=len(closed),
                                  expected=145, missing=sorted(expected - closed))))
            return
    output = args.output_dir.resolve()
    assert not output.exists(), 'Preserve the previous score evidence'
    with ThreadPoolExecutor(max_workers=3) as pool:
        details = list(pool.map(lambda row: audit_pair(control, candidate, row['point'], control_run, candidate_run), metadata))
    for role in ['control', 'candidate']:
        assert len({json.dumps(row[role]['native']['artifact_sha256'], sort_keys=True) for row in details}) == 1
    reports = {row['candidate_report'] for row in details}
    assert len(reports) == 1
    before_path = control / 'analysis/official-score.txt'
    before_receipt_path = control / 'analysis/official-score-receipt.json'
    before_receipt = json.loads(before_receipt_path.read_text())
    assert before_receipt['run_id'] == control_run['id'] and before_receipt['source'] == control_run['head_sha']
    assert digest(before_path) == before_receipt['sha256']
    baseline, comparisons = [], []
    for meta, detail in zip(metadata, details):
        before = detail['control']['native']['phases']['ROI']['CPI']
        after = detail['candidate']['native']['phases']['ROI']['CPI']
        baseline.append(dict(point=meta['point'], workload=meta['workload'], benchmark=meta['benchmark'],
                             group=meta['group'], weight=meta['weight'], cpi=before))
        comparisons.append(dict(point=meta['point'], variant=VARIANT, control_cpi=before, candidate_cpi=after))
    # No score fetch, output directory or aggregation before both full closure/TEA/DIP gates.
    official = score_snapshot(args.archive_host,
                              dict(report=next(iter(reports)),
                                   emu_sha256=details[0]['candidate']['native']['artifact_sha256']['emu']))
    output.mkdir(parents=True)
    after_path = output / 'official-score.txt'
    after_path.write_text(official['text'])
    assert digest(after_path) == official['sha256']
    receipt = {key: value for key, value in official.items() if key != 'text'}
    receipt.update(run_id=candidate_run['id'], source=candidate_run['head_sha'], official=candidate_run)
    (output / 'official-score-receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    result = aggregate(baseline, comparisons, json.loads((profile_root / 'raw/profile.json').read_text()),
                       scores(before_path), scores(after_path), variant=VARIANT)
    by_point = {row['point']: row for row in details}
    for row in result['points']:
        detail = by_point[row['point']]
        row.update(control_seed=detail['control']['native']['actual_seed'],
                   candidate_seed=detail['candidate']['native']['actual_seed'], seeds_equal=detail['seeds_equal'])
    for key in ['points', 'benchmarks']:
        rows = result.pop(key)
        with (output / (key + '.csv')).open('w', newline='') as stream:
            writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
            writer.writeheader()
            writer.writerows(rows)
    (output / 'point-budgets.json').write_text(json.dumps(details, indent=2) + '\n')
    source_files = [control_run_path, args.candidate_run, before_path, before_receipt_path,
                    profile_root / 'analysis/manifest.json', *[profile_root / name for name in names]]
    result.update(passed=True, points=145, benchmarks=29, control_run=control_run, candidate_run=candidate_run,
                  point_budgets_sha256=digest(output / 'point-budgets.json'), official_scores_reproduced=True,
                  source_sha256={str(p): digest(p) for p in source_files},
                  tool_sha256={name: digest(Path(__file__).with_name(name)) for name in
                               [Path(__file__).name, 'tea_dip_ci_point_budget.py', 'tea_dip_ci_repeat_evidence.py',
                                'tea_dip_query_ci_scores.py', 'tea_dip_query_endpoint_evidence.py',
                                'tea_dip_clock_alignment.py', 'native_perf.py', 'dip_analyze.py',
                                'tea_dip_turnover_ci_scores.py']},
                  scope='Complete145point current-format default CIs, original profile and29official scores. '
                        'Both exact TEA/native endpoints and full clock-aligned DIP grids/future finals checked. '
                        'Actual seeds and negative contributions retained. No partial score, isolated stable gain, '
                        'host speedup, physical PPA, CI scheduling change or automatic adoption claim.')
    (output / 'summary.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(dict(passed=True, points=145, benchmarks=29, suite=result['suite']), indent=2))


if __name__ == '__main__':
    main()
