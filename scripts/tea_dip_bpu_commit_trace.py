"""Audit closed BPU joint events against the same run's native mispredict counters."""
from collections import Counter
from pathlib import Path
import argparse
import json
import sqlite3

from native_perf import read_blocks, unique_counters
from tea_dip_bpu_native_cohort import cohorts
from tea_dip_query_endpoint_evidence import digest, verify

S1 = ['Ubtb', 'Abtb', 'UbtbUtage', 'AbtbUtage', 'Fallthrough']
S3 = ['Ras', 'ITTage', 'Sc', 'Tage', 'Mbtb', 'Fallthrough']
REASONS = ['BTB', 'TAGE', 'RAS', 'ITTAGE', 'SC']
TYPES = {1: 'conditional', 2: 'direct', 3: 'indirect'}
COLUMNS = {'ID', 'USEDSC', 'MBTBMATCH', 'REASON', 'BRANCH_DEBUG_REALCFIPC', 'BRANCH_MISPREDICT',
           'BRANCH_ATTRIBUTE_RASACTION', 'BRANCH_ATTRIBUTE_BRANCHTYPE', 'BRANCH_CFIPOSITION',
           'BRANCH_TAKEN', 'BRANCH_TARGET_ADDR', 'SOURCE_S3OVERRIDE', 'SOURCE_S2OVERRIDE',
           'SOURCE_S3SOURCE', 'SOURCE_S1SOURCE', 'STARTPC_ADDR', 'BPID', 'STAMP', 'SITE'}
COLUMNS |= {stage + '_' + field for stage in ['S1', 'S2', 'S3'] for field in
            ['TAKEN', 'ATTRIBUTE_RASACTION', 'ATTRIBUTE_BRANCHTYPE', 'TARGET_ADDR', 'CFIPOSITION']}


def summarize(rows, expected):
    marginals = {key: Counter() for key in ['reason', 's1_source', 's3_source', 'types', 'no_override', 'conditional_reason']}
    joint, pcs = Counter(), Counter()
    stamps, count = [], 0
    for row in rows:
        reason, s1, s3 = REASONS[row['REASON']], S1[row['SOURCE_S1SOURCE']], S3[row['SOURCE_S3SOURCE']]
        kind = TYPES[row['BRANCH_ATTRIBUTE_BRANCHTYPE']]
        assert row['BRANCH_MISPREDICT'] == 1 and row['SOURCE_S2OVERRIDE'] in [0, 1] and row['SOURCE_S3OVERRIDE'] in [0, 1]
        assert row['USEDSC'] in [0, 1] and row['MBTBMATCH'] in [0, 1]
        assert row['MBTBMATCH'] or not row['USEDSC']
        marginals['reason'][reason] += 1
        marginals['s1_source'][s1] += 1
        marginals['s3_source'][s3] += 1
        marginals['types'][kind] += 1
        ras = row['BRANCH_ATTRIBUTE_RASACTION']
        marginals['types']['call'] += ras == 2
        marginals['types']['ret'] += ras == 1
        marginals['types']['indirect_retcall'] += kind == 'indirect' and ras == 3
        if kind == 'conditional':
            marginals['conditional_reason'][reason] += 1
        if not (row['SOURCE_S2OVERRIDE'] or row['SOURCE_S3OVERRIDE']):
            marginals['no_override'][s1] += 1
        stage = 'S3' if row['SOURCE_S3OVERRIDE'] else 'S2' if row['SOURCE_S2OVERRIDE'] else 'S1'
        def relation(prefix):
            a, b = row[prefix + '_CFIPOSITION'], row['BRANCH_CFIPOSITION']
            return '<' if a < b else '=' if a == b else '>'
        key = (reason, s1, s3, row['SOURCE_S2OVERRIDE'], row['SOURCE_S3OVERRIDE'],
               row['MBTBMATCH'], row['USEDSC'], kind, stage, relation('S1'),
               row['S1_TAKEN'] == row['BRANCH_TAKEN'], relation(stage),
               row[stage + '_TAKEN'] == row['BRANCH_TAKEN'])
        joint[key] += 1
        pcs[row['BRANCH_DEBUG_REALCFIPC'], reason, stage] += 1
        count += 1
        if not stamps:
            stamps.append(row['STAMP'])
        last_stamp = row['STAMP']
    if stamps:
        stamps.append(last_stamp)
    native = dict(reason=expected['partitions']['reason'], s1_source=expected['partitions']['s1_source'],
                  s3_source=expected['partitions']['s3_source'], types=expected['mispredict_types'],
                  no_override=expected['no_later_override_by_s1_source'],
                  conditional_reason=expected['conditional_mispredict_reasons'])
    checks = {key: Counter(value) == marginals[key] for key, value in native.items()}
    checks['event_count'] = count == expected['committed_mispredict_entries']
    fields = ['reason', 's1_source', 's3_source', 's2_override', 's3_override', 'mbtb_match', 'used_sc',
              'branch_type', 'selected_stage', 's1_position_relation', 's1_taken_equal',
              'selected_position_relation', 'selected_taken_equal']
    return dict(passed=all(checks.values()), checks=checks, native=expected, observed_marginals=marginals,
                rows=count, observed_stamp_range=stamps,
                joint=[dict(zip(fields, key), count=value) for key, value in sorted(joint.items())],
                top_pc_reason_stage=[dict(pc=hex(key[0]), reason=key[1], selected_stage=key[2], count=value)
                                     for key, value in pcs.most_common(20)])


def analyze(root):
    receipt = json.loads((root / 'closure-receipt.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    verify(root, receipt['files'])
    spec = json.loads((root / 'experiment.json').read_text())
    assert digest(root / 'experiment.json') == receipt['spec_sha256']
    result = json.loads((root / spec['case']['point'] / 'result.json').read_text())
    assert result['original_process_closed'] and result['exitcode'] == 0 and result['difftest_passed']
    assert result['source'] == spec['source'] == receipt['source']
    assert result['process'] == receipt['process'] and result['spec_sha256'] == receipt['spec_sha256']
    neutrality = json.loads((root / 'neutrality.json').read_text())
    assert neutrality['passed'] and json.loads((root / 'resource-audit.json').read_text())['passed']
    folder = root / spec['case']['point']
    blocks = sorted(read_blocks(folder / 'simulator_err.txt').items())
    assert len(blocks) == (2 if '-W' in spec['case']['command'] else 1)
    phases = []
    with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        columns = {row['name'] for row in db.execute('PRAGMA table_info(BpuCommitMispredict)')}
        assert columns == COLUMNS
        cursor = iter(db.execute('SELECT * FROM BpuCommitMispredict ORDER BY ID'))
        previous_id, previous_stamp = 0, -1
        def take(amount):
            nonlocal previous_id, previous_stamp
            for _ in range(amount):
                row = next(cursor)
                assert row['ID'] == previous_id + 1 and row['STAMP'] > previous_stamp
                previous_id, previous_stamp = row['ID'], row['STAMP']
                yield row
        labels = ['warmup', 'roi'] if len(blocks) == 2 else ['coremark']
        for label, (stamp, block) in zip(labels, blocks):
            expected = cohorts(unique_counters(block))
            data = summarize(take(expected['committed_mispredict_entries']), expected)
            phases.append(dict(phase=label, native_print_stamp=stamp, **data))
        tail = sum(1 for _ in cursor)
    passed = all(phase['passed'] for phase in phases) and tail == 0
    return dict(passed=passed, source=spec['source'], point=spec['case']['point'], phases=phases,
                unassigned_tail_events=tail, native_six_table_neutrality=neutrality,
                consumer_sha256=digest(Path(__file__)), closure_receipt_sha256=digest(root / 'closure-receipt.json'),
                scope='One closed observer run. Native event counts define ordinal phase cuts; all native mispredict margins must match. STAMP is not calibrated to native/TEA cycles. Position/taken equality describes the chosen CFI only, not entire fetch correctness; target equality is not inferred. Joint groups establish co-occurrence, not a unique performance cause or saved-cycle budget.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    data = analyze(args.run_dir.resolve())
    args.output.write_text(json.dumps(data, indent=2) + '\n')
    print(json.dumps(dict(passed=data['passed'], point=data['point'], phase_rows={p['phase']: p['rows'] for p in data['phases']},
                         unassigned_tail_events=data['unassigned_tail_events']), indent=2))


if __name__ == '__main__':
    main()
