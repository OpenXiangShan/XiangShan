"""Score turnoverCI only after official success, all145 closed audits and official score reproduction."""
from pathlib import Path
import argparse
import csv
import json
import subprocess

from tea_dip_query_ci_scores import aggregate, scores
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_turnover_ci_point import NAMESPACE, RUN, SOURCE, VARIANT, context



def score_snapshot(host, identity):
    """Read the actual score and bind its report to the verified executable."""
    params = dict(report=identity['report'], emu_sha256=identity['emu_sha256'])
    code = 'params=' + repr(params) + '\n' + r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
root=Path(params['report']);path=root/'score-spec06-rva23-novec-gcc16-0.3c.txt'
with(root/'emu-gsim').open('rb')as stream:emu=hashlib.file_digest(stream,'sha256').hexdigest()
assert emu==params['emu_sha256']
body=path.read_bytes()
print(json.dumps(dict(utc=datetime.now(timezone.utc).isoformat(),report=str(root),remote_score_path=str(path),emu_sha256=emu,text=body.decode(),sha256=hashlib.sha256(body).hexdigest())))
'''
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', host, 'python3', '-'],
                           input=code, text=True, capture_output=True, check=True, timeout=50)
    return json.loads(reply.stdout)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    args = parser.parse_args()
    root = args.report_dir.resolve()
    identity, baseline, baseline_rows, manifest = context(root)
    evidence = root / 'evidence' / NAMESPACE
    watcher = json.loads((evidence / 'process-watch-state.json').read_text())
    assert watcher['run_id'] == RUN and watcher['source'] == SOURCE
    official = watcher['last_observation'].get('official')
    if official is not None:
        assert official['id'] == RUN and official['head_sha'] == SOURCE
    if official is None or official['status'] != 'completed':
        print(json.dumps(dict(pending=True, score_computed=False, run_id=RUN,
                              reason='Original CI has no official terminal state', official=official)))
        return
    assert official['id'] == RUN and official['head_sha'] == SOURCE and official['conclusion'] == 'success'
    namespace = root / NAMESPACE
    paths = [namespace / 'partial-analysis' / (row['point'] + '.json') for row in baseline_rows]
    available = sum(path.exists() for path in paths)
    if available != 145:
        print(json.dumps(dict(pending=True, score_computed=False, run_id=RUN, completed=available, expected=145,
                              reason='All145 original closed and independently audited windows required')))
        return
    comparisons, sources = [], {}
    consumer_sha = digest(Path(__file__).with_name('tea_dip_turnover_ci_point.py'))
    for expected, path in zip(baseline_rows, paths):
        record = json.loads(path.read_text())
        assert record['run_id'] == RUN and record['source'] == SOURCE and not record['pending']
        assert record['analysis_source_sha256'] == consumer_sha
        for name, expected_sha in record['helper_sha256'].items():
            assert digest(Path(__file__).with_name(name)) == expected_sha
        for name, expected_sha in record['report_helper_sha256'].items():
            assert digest(root / name) == expected_sha
        assert record['baseline_manifest_sha256'] == digest(baseline / 'analysis/manifest.json')
        point = record['comparison']['point']
        assert point == expected['point'] and record['comparison']['variant'] == VARIANT
        assert record['control']['seed'] == record['candidate']['seed'] == int(expected['seed'])
        receipt_path = namespace / 'raw/closed-receipts' / (point + '.json')
        assert digest(receipt_path) == record['closed_receipt_sha256']
        receipt = json.loads(receipt_path.read_text())
        assert receipt['run_id'] == RUN and receipt['source'] == SOURCE
        assert not receipt['original_process_live'] and receipt['runner_success_lines']
        assert receipt['artifact_sha256'] == dict(emu=identity['emu_sha256'], reference=identity['reference_sha256'], cst=identity['cst_sha256'])
        verify(namespace / 'raw/results' / point, receipt['files'])
        comparisons.append(record['comparison'])
        sources[str(path.relative_to(root))] = digest(path)
    # Fetch the actual workflow score only after the complete point/official gates.
    jobs = json.loads((evidence / 'jobs.json').read_text())['jobs']
    host = jobs[0]['runner_name'].removeprefix('xs-perf-').rsplit('-', 1)[0]
    official_score = score_snapshot(host, identity)
    output = namespace / 'analysis'
    output.mkdir(exist_ok=True)
    score_path = output / 'official-score.txt'
    score_path.write_text(official_score['text'])
    assert digest(score_path) == official_score['sha256']
    score_receipt = {key: value for key, value in official_score.items() if key != 'text'}
    score_receipt.update(run_id=RUN, source=SOURCE, host=host, official=official)
    (output / 'official-score-receipt.json').write_text(json.dumps(score_receipt, indent=2) + '\n')
    before_path = 'raw/score/score-spec06-rva23-novec-gcc16-0.3c.txt'
    verify(baseline, {before_path: manifest['files'][before_path]})
    result = aggregate(baseline_rows, comparisons, json.loads((baseline / 'raw/profile.json').read_text()),
                       scores(baseline / before_path), scores(score_path), variant=VARIANT)
    for key in ['points', 'benchmarks']:
        rows = result.pop(key)
        with (output / (key + '.csv')).open('w', newline='') as stream:
            writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
            writer.writeheader()
            writer.writerows(rows)
    result.update(complete=True, run_id=RUN, source=SOURCE, points=145, benchmarks=29,
                  official=official, official_score_sha256=digest(score_path),
                  official_scores_reproduced=True, official_score_receipt_sha256=digest(output / 'official-score-receipt.json'), point_source_sha256=sources,
                  consumer_sha256=digest(Path(__file__)), aggregate_helper_sha256=digest(Path(__file__).with_name('tea_dip_query_ci_scores.py')),
                  scope='Complete turnoverCI0.3 versus archived841 only. Official success, all145 original closed/rawSHA audited points and reproduced29benchmark scores required. Signed weighted point and benchmark contributions retained. No partial-suite scoring, physical timing or automatic adoption claim.')
    (output / 'summary.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
