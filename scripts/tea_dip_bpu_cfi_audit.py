"""Add chosen-CFI target relations to a validated, closed BPU event audit."""
from collections import Counter
from pathlib import Path
import argparse
import json
import sqlite3

import tea_dip_bpu_commit_trace as trace
from tea_dip_query_endpoint_evidence import digest, verify

RELATIONS = ['position_before', 'position_after', 'direction_diff', 'taken_target_diff',
             'not_taken_target_unused', 'metadata_equal_taken']


def relation(row, stage):
    predicted, actual = row[stage+'_CFIPOSITION'], row['BRANCH_CFIPOSITION']
    if predicted != actual:
        return 'position_before' if predicted < actual else 'position_after'
    if row[stage+'_TAKEN'] != row['BRANCH_TAKEN']:
        return 'direction_diff'
    if not row['BRANCH_TAKEN']:
        return 'not_taken_target_unused'
    return 'metadata_equal_taken' if row[stage+'_TARGET_ADDR'] == row['BRANCH_TARGET_ADDR'] else 'taken_target_diff'


def analyze(root, audit_path):
    receipt_path = root/'closure-receipt.json'
    receipt, audit = [json.loads(p.read_text()) for p in [receipt_path, audit_path]]
    assert receipt['ready'] and not receipt['original_process_live']
    verify(root, receipt['files'])
    assert audit['passed'] and audit['unassigned_tail_events'] == 0
    assert audit['closure_receipt_sha256'] == digest(receipt_path)
    assert audit['consumer_sha256'] == digest(Path(trace.__file__))
    assert all(audit['native_six_table_neutrality']['checks'].values())
    assert all(p['passed'] and all(p['checks'].values()) for p in audit['phases'])
    spec = json.loads((root/'experiment.json').read_text())
    assert digest(root/'experiment.json') == receipt['spec_sha256']
    assert audit['source'] == spec['source'] == receipt['source']
    assert audit['point'] == spec['case']['point']
    folder = root/spec['case']['point']
    phases = []
    with sqlite3.connect((folder/'simulator.db').resolve().as_uri()+'?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        assert {r['name'] for r in db.execute('PRAGMA table_info(BpuCommitMispredict)')} == trace.COLUMNS
        rows = iter(db.execute('SELECT * FROM BpuCommitMispredict ORDER BY ID'))
        for original in audit['phases']:
            counts, stages, reasons, joint = Counter(), Counter(), Counter(), Counter()
            for _ in range(original['rows']):
                row = next(rows)
                stage = 'S3' if row['SOURCE_S3OVERRIDE'] else 'S2' if row['SOURCE_S2OVERRIDE'] else 'S1'
                category, s1_category = relation(row, stage), relation(row, 'S1')
                reason = trace.REASONS[row['REASON']]
                counts[category] += 1
                stages[stage] += 1
                reasons[reason] += 1
                joint[row['BRANCH_DEBUG_REALCFIPC'], reason, stage, category, s1_category] += 1
            expected_stages = Counter()
            for group in original['joint']:
                expected_stages[group['selected_stage']] += group['count']
            assert stages == expected_stages and reasons == Counter(original['native']['partitions']['reason'])
            assert sum(counts.values()) == sum(joint.values()) == original['rows']
            phases.append(dict(phase=original['phase'], rows=original['rows'], selected_relation_counts={k:counts[k] for k in RELATIONS},
                               joint=[dict(pc=hex(pc), reason=reason, selected_stage=stage, selected_relation=category,
                                           s1_relation=s1_category, count=count)
                                      for (pc,reason,stage,category,s1_category),count in sorted(joint.items())]))
        assert next(rows, None) is None
    return dict(passed=True, point=audit['point'], source=audit['source'], phases=phases,
                input_sha256={str(audit_path):digest(audit_path), str(receipt_path):digest(receipt_path),
                              str(folder/'simulator.db'):digest(folder/'simulator.db')},
                consumer_sha256=digest(Path(__file__)),
                scope='Closed, strictly neutral observer only; ordinal phases and selected stages reproduce the validated event audit. '
                      'Position is compared first, then direction; target is compared only at the same CFI position with both predictions taken. '
                      'These are exclusive metadata relations, not exclusive causal reasons or entire-fetch correctness. '
                      'All rows are committed mispredicts: no denominator for predictor accuracy or override opportunity benefit. '
                      'STAMP remains uncalibrated to TEA/DIP cycles; no window join, saved cycles or performance gain inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir', type=Path, required=True)
    parser.add_argument('--event-audit', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    data = analyze(args.run_dir.resolve(), args.event_audit.resolve())
    args.output.write_text(json.dumps(data,indent=2)+'\n')
    print(json.dumps(dict(passed=data['passed'], point=data['point'], phases=[{k:p[k] for k in ['phase','rows','selected_relation_counts']} for p in data['phases']]),indent=2))


if __name__ == '__main__':
    main()
