"""Bound phase changes at common cumulative retirement milestones in closed CI windows."""
from pathlib import Path
from bisect import bisect_left
import argparse
import hashlib
import json
import sqlite3

STATES = ['Computing', 'Stalled', 'Flushed', 'Drained']
TARGETS = list(range(5000000, 35000001, 5000000))


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def brackets(rows):
    assert rows
    for before, after in zip(rows, rows[1:]):
        assert before['CYCLE'] <= after['CYCLE'] and before['RETIRED'] <= after['RETIRED'], 'Nonmonotonic cycle or retirement counter'
        assert all(before['STATES_' + str(i)] <= after['STATES_' + str(i)] for i in range(4)), 'Nonmonotonic TEA state counter'
        assert sum(after['STATES_' + str(i)] - before['STATES_' + str(i)] for i in range(4)) == after['CYCLE'] - before['CYCLE'], 'TEA state increments do not cover elapsed cycles'
    retired = [row['RETIRED'] for row in rows]
    result = []
    for target in TARGETS:
        index = bisect_left(retired, target)
        assert 0 < index < len(rows), 'Common retirement milestone lacks two snapshots'
        lower, upper = rows[index - 1:index + 1]
        assert lower['RETIRED'] < target <= upper['RETIRED']
        result.append(dict(retired=target, cycle_lower=lower['CYCLE'], cycle_upper=upper['CYCLE'],
                           retired_lower=lower['RETIRED'], retired_upper=upper['RETIRED'],
                           states_lower={state: lower['STATES_' + str(i)] for i, state in enumerate(STATES)},
                           states_upper={state: upper['STATES_' + str(i)] for i, state in enumerate(STATES)}))
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--ci-evidence', required=True, choices=['ci-readiness', 'ci-credit-div', 'ci-fdiv-credit','fdiv-wb-query-paired-ci','fdiv-turnover-ci-20261108'])
    parser.add_argument('--point', required=True)
    args = parser.parse_args()
    root = args.report_dir.resolve()
    namespace = 'ci' if args.ci_evidence == 'ci-readiness' else args.ci_evidence
    out = root / namespace / 'partial-analysis'
    record_path = out / (args.point + '.json')
    record = json.loads(record_path.read_text())
    receipt_path = root / namespace / 'raw/closed-receipts' / (args.point + '.json')
    receipt = json.loads(receipt_path.read_text())
    run = json.loads((root / 'evidence' / args.ci_evidence / 'run.json').read_text())
    assert record['run_id'] == receipt['run_id'] == run['id'] and record['source'] == receipt['source'] == run['head_sha']
    assert not receipt['original_process_live'] and receipt['runner_success_lines'], 'Original CI process has no closure proof'
    assert digest(receipt_path) == record['closed_receipt_sha256']
    baseline = root.parents[1] / 'ci-41413249290'
    manifest_path = baseline / 'analysis/manifest.json'
    manifest = json.loads(manifest_path.read_text())
    assert manifest['source_sha'] == '84112d0b65231ca690a4ef2fea4b5169f2146e10'
    paths = [baseline / 'raw/results' / args.point / 'simulator.db', root / namespace / 'raw/results' / args.point / 'simulator.db']
    expected = [manifest['files']['raw/results/' + args.point + '/simulator.db'], receipt['files']['simulator.db']]
    windows = []
    for label, path, entry in zip(['control', 'candidate'], paths, expected):
        assert path.stat().st_size == entry['bytes'] and digest(path) == entry['sha256']
        with sqlite3.connect(path.resolve().as_uri() + '?mode=ro', uri=True) as db:
            db.row_factory = sqlite3.Row
            rows = list(db.execute('SELECT * FROM TeaCycleSummary_0 ORDER BY CYCLE,ID'))
        windows.append(dict(label=label, database_sha256=entry['sha256'], snapshot_count=len(rows), milestones=brackets(rows)))
    segments = []
    for index in range(len(TARGETS) - 1):
        start, end = TARGETS[index:index + 2]
        count = end - start
        ranges = []
        for window in windows:
            a, b = window['milestones'][index:index + 2]
            ranges.append(dict(cycles_lower=b['cycle_lower'] - a['cycle_upper'], cycles_upper=b['cycle_upper'] - a['cycle_lower'],
                               states_lower={state: b['states_lower'][state] - a['states_upper'][state] for state in STATES},
                               states_upper={state: b['states_upper'][state] - a['states_lower'][state] for state in STATES}))
        a, b = ranges
        low = (b['cycles_lower'] - a['cycles_upper']) / count
        high = (b['cycles_upper'] - a['cycles_lower']) / count
        state_bounds = {state: dict(lower=(b['states_lower'][state] - a['states_upper'][state]) / count,
                                    upper=(b['states_upper'][state] - a['states_lower'][state]) / count) for state in STATES}
        assert low <= high and all(bound['lower'] <= bound['upper'] for bound in state_bounds.values())
        segments.append(dict(retired_start=start, retired_end=end, retired_count=count, windows=ranges,
                             delta_cpi_lower=low, delta_cpi_upper=high, tea_state_delta_cpi_bounds=state_bounds))
    sources = [record_path, receipt_path, manifest_path, baseline / 'source/src/main/scala/xiangshan/TeaCycleStats.scala',
               baseline / 'source/src/main/scala/xiangshan/backend/rob/Rob.scala']
    result = dict(run_id=run['id'], source=run['head_sha'], point=args.point, native_roi_speedup=record['comparison']['speedup'],
                  windows=windows, segments=segments,
                  source_sha256={str(path.relative_to(root)) if path.is_relative_to(root) else str(path): digest(path) for path in sources},
                  consumer_sha256=digest(Path(__file__)),
                  scope='Common cumulative TEA retirement milestones,5M through35M, from closed archived DBs only. Each crossing is bracketed by the preceding/following snapshot; no interpolation or native endpoint substitution. Segment ranges are conservative bounds, not confidence intervals, exact phase measurements or unique dynamic-uop identity. Native ROI boundaries differ slightly from20M;35M-to-end and0-to5M remain uncovered. Per-state ranges are separately conservative and must not be summed as an exact CPI budget. No partial-suite score.')
    target = out / (args.point + '-retired-progress.json')
    target.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(dict(run_id=run['id'], point=args.point, snapshot_count=[w['snapshot_count'] for w in windows],
                         max_crossing_width=[max(m['cycle_upper'] - m['cycle_lower'] for m in w['milestones']) for w in windows],
                         segments=[{key: row[key] for key in ['retired_start', 'retired_end', 'delta_cpi_lower', 'delta_cpi_upper']} for row in segments]), indent=2))


if __name__ == '__main__':
    main()
