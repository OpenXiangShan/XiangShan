"""Score the current WB-query CI only after official success and all145 paired windows.

The live original watcher supplies the official run/head/status. This command is
pending while the CI runs and never computes a score from a point subset.
"""
import argparse
import csv
import hashlib
import json
import math
from pathlib import Path
import re


def scores(path):
    rows = {}
    for line in path.read_text().splitlines():
        match = re.match(r'\d+\.(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)', line)
        if match:
            name, time, reference, score, coverage = match.groups()
            rows[name] = dict(time=float(time), reference=float(reference),
                              score=float(score), coverage=float(coverage))
    return rows


def aggregate(baseline, comparisons, profile, before_scores, after_scores):
    expected = {r['point']: r for r in baseline}
    paired = {r['point']: r for r in comparisons}
    assert len(expected) == len(baseline) == len(paired) == len(comparisons) == 145
    assert paired.keys() == expected.keys(), 'Complete paired point set required'
    points = []
    for name, base in expected.items():
        row = paired[name]
        assert row['variant'] == 'fdiv-wb-query'
        workload = base['workload']; benchmark = base['benchmark']
        checkpoint = name[len(workload) + 1:].rsplit('_', 1)[0]
        inputs = [w for w in profile if w.split('_')[0] == benchmark]
        length = sum(float(profile[w]['insts']) for w in inputs)
        weight = (float(profile[workload]['insts']) / length
                  * float(profile[workload]['points'][checkpoint])
                  / sum(map(float, profile[workload]['points'].values())))
        assert abs(weight - float(base['weight'])) < 1e-12
        before, after = float(row['control_cpi']), float(row['candidate_cpi'])
        assert abs(before - float(base['cpi'])) < 1e-12
        delta = after - before
        points.append(dict(point=name, benchmark=benchmark, group=base['group'],
                           weight=weight, control_cpi=before, candidate_cpi=after,
                           delta_cpi=delta, speedup=before / after - 1,
                           weighted_delta_cpi=weight * delta))
    names = {r['benchmark'] for r in points}
    assert names == before_scores.keys() == after_scores.keys() and len(names) == 29
    benchmarks = []
    for name in sorted(names):
        selected = [r for r in points if r['benchmark'] == name]
        assert abs(sum(r['weight'] for r in selected) - 1) < 1e-12
        before = sum(r['weight'] * r['control_cpi'] for r in selected)
        after = sum(r['weight'] * r['candidate_cpi'] for r in selected)
        delta = sum(r['weighted_delta_cpi'] for r in selected)
        assert abs(delta - (after - before)) < 1e-12
        length = sum(float(d['insts']) for w, d in profile.items()
                     if w.split('_')[0] == name)
        reference = before_scores[name]['reference']
        assert reference == after_scores[name]['reference']
        assert before_scores[name]['coverage'] == after_scores[name]['coverage']
        values = {}
        for label, cpi, official in [('control', before, before_scores[name]),
                                     ('candidate', after, after_scores[name])]:
            elapsed = cpi * length / 3e9
            score = reference / elapsed / 3
            assert abs(elapsed - official['time']) <= 0.000501, (name, label, 'time')
            assert abs(score - official['score']) <= 0.000501, (name, label, 'score')
            values[label + '_score'] = score
        ratio = before / after
        benchmarks.append(dict(benchmark=name, group=selected[0]['group'], points=len(selected),
                               control_cpi=before, candidate_cpi=after, delta_cpi=delta,
                               speedup=ratio - 1, overall_log_contribution=math.log(ratio) / 29,
                               **values))
    suite = {}
    for group in ['INT', 'FP', 'ALL']:
        selected = [r for r in benchmarks if group == 'ALL' or r['group'] == group]
        before = math.exp(sum(math.log(r['control_score']) for r in selected) / len(selected))
        after = math.exp(sum(math.log(r['candidate_score']) for r in selected) / len(selected))
        suite[group] = dict(benchmarks=len(selected), control_score=before,
                            candidate_score=after, speedup=after / before - 1)
    positive = sum(max(0, r['overall_log_contribution']) for r in benchmarks)
    negative = sum(min(0, r['overall_log_contribution']) for r in benchmarks)
    assert abs(math.expm1(positive + negative) - suite['ALL']['speedup']) < 1e-12
    return dict(points=points, benchmarks=benchmarks, suite=suite,
                positive_log_contribution=positive, negative_log_contribution=negative)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--score-file', type=Path, required=True)
    args = parser.parse_args()
    root = args.report_dir.resolve()
    identity = json.loads((root / 'evidence/fdiv-wb-query-paired-ci/archive-identity.json').read_text())
    watcher = json.loads((root / 'evidence/fdiv-wb-query-paired-ci/process-watch-state.json').read_text())
    official = watcher['last_observation'].get('official')
    assert identity['run_id'] == watcher['run_id'] == 42159911532
    assert identity['source'] == watcher['source'] == 'c600630b787e3f0461fcf6104f46c5801a2b015b'
    if official is None:
        print(json.dumps(dict(pending=True, score_computed=False, reason='Official CI status is unavailable')))
        return
    assert official['id'] == identity['run_id'] and official['head_sha'] == identity['source']
    if official['status'] != 'completed':
        print(json.dumps(dict(pending=True, score_computed=False, run_id=identity['run_id'],
                              official_status=official['status'], reason='Original CI has no successful official terminal state')))
        return
    assert official['conclusion'] == 'success', 'Failed/cancelled CI cannot pass overall performance acceptance'
    baseline_root = root.parents[1] / 'ci-41413249290'
    baseline = list(csv.DictReader((baseline_root / 'analysis/points.csv').open()))
    namespace = root / 'fdiv-wb-query-paired-ci'
    point_dir = namespace / 'partial-analysis'
    paths = [point_dir / (row['point'] + '.json') for row in baseline]
    available = sum(path.exists() for path in paths)
    if available != 145:
        print(json.dumps(dict(pending=True, score_computed=False, completed=available, expected=145,
                              reason='All145 independently audited closed query points are required')))
        return
    comparisons = []
    sources = {}
    consumer = Path(__file__).with_name('tea_dip_analyze_ci_closed_point.py')
    expected_consumer = hashlib.sha256(consumer.read_bytes()).hexdigest()
    for base, path in zip(baseline, paths):
        record = json.loads(path.read_text())
        assert record['run_id'] == identity['run_id'] and record['source'] == identity['source']
        assert record['analysis_source_sha256'] == expected_consumer
        point = record['comparison']['point']
        assert point == base['point'] and record['control']['seed'] == record['candidate']['seed'] == int(base['seed'])
        receipt_path = namespace / 'raw/closed-receipts' / (point + '.json')
        receipt = json.loads(receipt_path.read_text())
        assert hashlib.sha256(receipt_path.read_bytes()).hexdigest() == record['closed_receipt_sha256']
        assert receipt['run_id'] == identity['run_id'] and receipt['source'] == identity['source']
        assert not receipt['original_process_live'] and receipt['runner_success_lines']
        assert receipt['artifact_sha256'] == dict(emu=identity['emu_sha256'], reference=identity['reference_sha256'], cst=identity['cst_sha256'])
        for name, entry in receipt['files'].items():
            raw = namespace / 'raw/results' / point / name
            with raw.open('rb') as stream:
                assert raw.stat().st_size == entry['bytes'] and hashlib.file_digest(stream, 'sha256').hexdigest() == entry['sha256']
        comparisons.append(record['comparison'])
        sources[str(path.relative_to(root))] = hashlib.sha256(path.read_bytes()).hexdigest()
    profile = json.loads((baseline_root / 'raw/profile.json').read_text())
    before = scores(baseline_root / 'raw/score/score-spec06-rva23-novec-gcc16-0.3c.txt')
    after = scores(args.score_file)
    result = aggregate(baseline, comparisons, profile, before, after)
    output = namespace / 'analysis'
    output.mkdir(exist_ok=True)
    for key in ['points', 'benchmarks']:
        rows = result.pop(key)
        with (output / ('query-ci-' + key + '.csv')).open('w', newline='') as stream:
            writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
            writer.writeheader()
            writer.writerows(rows)
    result.update(complete=True, points=145, benchmarks=29, run_id=identity['run_id'], source=identity['source'],
                  official_conclusion=official['conclusion'], official_scores_reproduced=True,
                  point_source_sha256=sources,
                  score_file_sha256=hashlib.sha256(args.score_file.read_bytes()).hexdigest(),
                  consumer_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                  scope='Complete currentqueryCI cov0.3 only, against archived841 baseline. Exact paired145points with original closed receipts/rawSHA, complete29benchmark profile and official successful terminal required. Signed point/benchmark contributions retained; no partial-suite scoring or default adoption claim.')
    (output / 'query-ci-summary.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
