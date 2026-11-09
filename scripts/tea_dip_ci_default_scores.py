"""Score default-seed SPEC06 CI only after official success and all145 closed points.

This project uses archived841 and the report's matching global-trace parser.
No CI scheduling is performed. The archive host only reads the existing score.
"""
from pathlib import Path
import argparse
import csv
import hashlib
import json
import re
import sys

from tea_dip_ci_repeat_evidence import point as closed_point
from tea_dip_query_ci_scores import aggregate, scores
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_turnover_ci_scores import score_snapshot


VARIANT = 'default-seed-ci'
CST = 'source/scripts/ci/tea-dip-period1000.cst'


def audit_point(root, namespace, base, manifest, run):
    """Audit a single closed point; this never computes a suite score."""
    from analyze_windows import extract
    baseline = root.parents[1] / 'ci-41413249290'
    point = base['point']
    before = baseline / 'raw/results' / point
    after = namespace / 'raw/results' / point
    receipt_path = namespace / 'raw/closed-receipts' / (point + '.json')
    receipt, native = closed_point(after, receipt_path)
    assert receipt['run_id'] == run['id'] and receipt['source'] == run['head_sha']
    assert receipt['runner_success_lines']
    assert receipt['artifact_sha256']['reference'] == manifest['reference_so_sha256']
    assert receipt['artifact_sha256']['cst'] == manifest['files'][CST]['sha256']
    names = ['raw/results/' + point + '/' + n for n in ['simulator_out.txt', 'simulator_err.txt', 'simulator.db']]
    verify(baseline, {n: manifest['files'][n] for n in names})
    image = re.search(r'The image is ([^\r\n]+)', (before / 'simulator_out.txt').read_text())
    assert image and image[1] == native['checkpoint']
    a, b = [extract(folder, label)[0] for folder, label in [(before, 'baseline841'), (after, VARIANT)]]
    for window in [a, b]:
        assert abs(window['warmup_instructions'] - 20000000) < 128
        assert abs(window['instructions'] - 20000000) < 128
    assert a['seed'] == int(base['seed'])
    assert b['seed'] == native['actual_seed']
    assert abs(a['cpi'] - float(base['cpi'])) < 1e-12
    assert b['cycles'] == native['phases']['ROI']['cycles']
    assert b['instructions'] == native['phases']['ROI']['retired']
    comparison = dict(point=point, variant=VARIANT, control_cpi=a['cpi'], candidate_cpi=b['cpi'])
    detail = dict(point=point, baseline=a, candidate=b, actual_seed=native['actual_seed'],
                  baseline_seed=a['seed'], seeds_equal=a['seed'] == native['actual_seed'],
                  artifact_sha256=receipt['artifact_sha256'],
                  report=str(Path(receipt['last_live_process_observation']['args'][0]).parent),
                  closed_receipt_sha256=digest(receipt_path),
                  native_stage_latency_validity=native['native_stage_latency_validity'])
    return comparison, detail


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--ci-dir', type=Path, required=True)
    parser.add_argument('--run-id', type=int, required=True)
    parser.add_argument('--source', required=True)
    parser.add_argument('--official-run', type=Path, required=True)
    parser.add_argument('--archive-host', default='node032', help='Read-only SSH access to the shared existing score; no CI dispatch')
    args = parser.parse_args()
    root, namespace = args.report_dir.resolve(), args.ci_dir.resolve()
    official_body = args.official_run.read_bytes()
    run = json.loads(official_body)
    official_hash = hashlib.sha256(official_body).hexdigest()
    assert run['id'] == args.run_id and run['head_sha'] == args.source
    receipts = namespace / 'raw/closed-receipts'
    closed = {p.stem for p in receipts.glob('*.json')}
    if run['status'] != 'completed':
        print(json.dumps(dict(pending=True, score_computed=False, run_id=args.run_id,
                              official_status=run['status'], closed_points=len(closed), expected=145,
                              reason='Official CI is not terminal')))
        return
    assert run['conclusion'] == 'success', 'Official failed/cancelled CI cannot pass performance acceptance'
    baseline = root.parents[1] / 'ci-41413249290'
    manifest = json.loads((baseline / 'analysis/manifest.json').read_text())
    names = ['analysis/points.csv', 'raw/profile.json', 'raw/run.json', CST,
             'source/scripts/tea_visualize.py', 'source/scripts/dip_analyze.py']
    verify(baseline, {n: manifest['files'][n] for n in names})
    before_run = json.loads((baseline / 'raw/run.json').read_text())
    assert before_run['head_sha'] == manifest['source_sha'] == '84112d0b65231ca690a4ef2fea4b5169f2146e10'
    assert before_run['status'] == 'completed' and before_run['conclusion'] == 'success'
    rows = list(csv.DictReader((baseline / 'analysis/points.csv').open()))
    expected = {r['point'] for r in rows}
    assert len(rows) == len(expected) == 145
    assert closed <= expected, 'Unexpected closed point in this CI archive'
    if closed != expected:
        print(json.dumps(dict(pending=True, score_computed=False, run_id=args.run_id,
                              closed_points=len(closed), expected=145, missing=sorted(expected - closed),
                              reason='All145 original closed points are required')))
        return
    output = namespace / 'analysis'
    assert not output.exists(), 'Preserve and inspect the existing score evidence'
    sys.path.insert(0, str(root))
    comparisons, details = [], []
    for i, base in enumerate(rows, 1):
        comparison, detail = audit_point(root, namespace, base, manifest, run)
        comparisons.append(comparison)
        details.append(detail)
        if i % 10 == 0:
            print(json.dumps(dict(phase='auditing_closed_points', completed=i, expected=145)), flush=True)
    assert len({json.dumps(d['artifact_sha256'], sort_keys=True) for d in details}) == 1
    assert len({d['report'] for d in details}) == 1
    # No score fetch or aggregation occurs before official/full-set/raw/TEA/DIP checks.
    official_score = score_snapshot(args.archive_host, dict(report=details[0]['report'],
                                                          emu_sha256=details[0]['artifact_sha256']['emu']))
    output.mkdir()
    score_path = output / 'official-score.txt'
    score_path.write_text(official_score['text'])
    assert digest(score_path) == official_score['sha256']
    score_receipt = {k: v for k, v in official_score.items() if k != 'text'}
    score_receipt.update(run_id=args.run_id, source=args.source, official=run,
                         official_run_sha256=official_hash, archive_host=args.archive_host)
    (output / 'official-score-receipt.json').write_text(json.dumps(score_receipt, indent=2) + '\n')
    before_score = 'raw/score/score-spec06-rva23-novec-gcc16-0.3c.txt'
    verify(baseline, {before_score: manifest['files'][before_score]})
    result = aggregate(rows, comparisons, json.loads((baseline / 'raw/profile.json').read_text()),
                       scores(baseline / before_score), scores(score_path), variant=VARIANT)
    by_point = {d['point']: d for d in details}
    for point in result['points']:
        d = by_point[point['point']]
        point.update(baseline_seed=d['baseline_seed'], actual_seed=d['actual_seed'], seeds_equal=d['seeds_equal'])
    for key in ['points', 'benchmarks']:
        data = result.pop(key)
        with (output / (key + '.csv')).open('w', newline='') as stream:
            writer = csv.DictWriter(stream, fieldnames=list(data[0]))
            writer.writeheader()
            writer.writerows(data)
    (output / 'point-audits.json').write_text(json.dumps(details, indent=2) + '\n')
    (output / 'official-run.json').write_bytes(official_body)
    result.update(complete=True, run_id=args.run_id, source=args.source, points=145, benchmarks=29,
                  official_conclusion=run['conclusion'], official_scores_reproduced=True,
                  matched_seed_points=sum(d['seeds_equal'] for d in details),
                  point_audits_sha256=digest(output / 'point-audits.json'),
                  baseline_manifest_sha256=digest(baseline / 'analysis/manifest.json'),
                  official_score_sha256=digest(score_path),
                  source_sha256={p.name: digest(p) for p in [Path(__file__), root / 'analyze_windows.py',
                      Path(__file__).with_name('tea_dip_ci_repeat_evidence.py'),
                      Path(__file__).with_name('tea_dip_query_ci_scores.py'),
                      Path(__file__).with_name('tea_dip_turnover_ci_scores.py')]},
                  scope='Complete default-seed SPEC06 cov0.3 global-trace CI versus archived841. All145 closed native/TEA/DIP windows and official29 scores are checked. Actual seeds are retained, not a scoring gate. Negative diagnostic latencies do not invalidate independently measured native CPI. No partial-suite score, isolated causal/host/PPA or automatic adoption claim.')
    (output / 'summary.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
