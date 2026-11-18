"""Join existing load-service identities to TEA source-cycle ROB context.

Head context describes a ROB entry. It does not supply the sampled member's
issue state, retirement ownership, or an exclusive CPU delay attribution.
"""
import argparse
from bisect import bisect_right
from collections import Counter, defaultdict
import json
from pathlib import Path
import sqlite3

from tea_dip_load_writeback_trace import KEY
from tea_dip_query_endpoint_evidence import digest, verify
from tea_visualize import normalize_pc


def measure(rows, finals, identity, profile):
    records = {r['sample_cycle']: (r, 'matched') for r in identity['matches']}
    for record in identity['unknown']:
        assert record['sample_cycle'] not in records, 'Duplicate TEA source identity'
        records[record['sample_cycle']] = record, record['reason']
    assert len(records) == len(identity['matches']) + len(identity['unknown']) == identity['Stalled_samples']
    phases = profile['phases']
    boundaries = [p['begin']['cycle'] for p in phases] + [phases[-1]['end']['cycle']]
    counts = [Counter() for _ in phases]
    entry_counts = [Counter() for _ in phases]
    keys = [defaultdict(set) for _ in phases]
    examples, seen = [], set()
    for row in rows:
        cycle = row['CYCLE']
        assert cycle not in seen and cycle in records, 'Mismatched TEA source cycles'
        seen.add(cycle)
        record, resolution = records[cycle]
        target = tuple(row['TARGET_' + k] for k in ['ALLOCATIONCYCLE', 'ROBINDEX', 'RANK'])
        assert target == tuple(record['target'][k] for k in ['allocation_cycle', 'rob_index', 'rank'])
        final = finals.get(target)
        pc = None if not row['TARGETVALID'] or final is None else normalize_pc(final['MEMBER_PC'])
        signature = (-3 if not row['TARGETVALID'] else -2 if final is None
                     else -1 if final['CANCELLED'] else final['MEMBER_PSV'])
        phase = bisect_right(boundaries, cycle) - 1
        assert 0 <= phase < len(phases)
        stage, owner, entry_owner = 'no_qualified_WB', 'no_qualified_WB', 'no_qualified_WB'
        if resolution == 'matched':
            assert pc == record['architectural_PC']
            stage = ('WB_not_after_sample' if record['WB'] <= cycle else
                     'before_first_S2' if record['first_S2'] > cycle else
                     'at_first_S2' if record['first_S2'] == cycle else 'after_first_S2_before_WB')
            member = record['identity']
            same_head = (row['CONTEXT_HEAD_VALUE'], row['CONTEXT_HEAD_FLAG'], row['CONTEXT_HEAD_ISFORMER']) == (
                member['rob_index'], member['rob_flag'], member['rob_former'])
            owner = ('head_invalid' if not row['CONTEXT_HEADVALID'] else
                     'same_full_head_pointer' if same_head else 'different_full_head_pointer')
            same_entry = (row['CONTEXT_HEAD_VALUE'], row['CONTEXT_HEAD_FLAG']) == (
                member['rob_index'], member['rob_flag'])
            entry_owner = ('head_invalid' if not row['CONTEXT_HEADVALID'] else
                           'same_entry' if same_entry else 'different_entry')
            if owner == 'same_full_head_pointer' and record['first_S2'] < cycle:
                examples.append(dict(sample_cycle=cycle, architectural_PC=hex(pc), final_PSV=signature,
                                     first_S2=record['first_S2'], WB=record['WB'], identity=member,
                                     head_reason_valid=row['CONTEXT_HEADREASONVALID'],
                                     head_reason_id=row['CONTEXT_HEADREASON'],
                                     commitBlockMask=row['CONTEXT_COMMITBLOCKMASK'], head_ready=row['CONTEXT_HEADREADY']))
        key = (pc, signature, row['CONTEXT_COMMITBLOCKMASK'], row['CONTEXT_HEADVALID'], row['CONTEXT_HEADREADY'],
               row['CONTEXT_HEADREASONVALID'], row['CONTEXT_HEADREASON'], resolution, stage, owner)
        counts[phase][key] += 1
        entry_counts[phase][pc, signature, resolution, stage, entry_owner, owner] += 1
        if resolution == 'matched':
            keys[phase][key].add(tuple(record['identity'][name] for name in KEY))
    assert seen == records.keys(), 'Missing TEA source contexts'
    output = []
    for phase, groups, unique, entries in zip(phases, counts, keys, entry_counts):
        rows = [dict(architectural_PC='unknown' if pc is None else hex(pc), final_PSV=signature,
                     commitBlockMask=mask, head_valid=valid, head_ready=ready,
                     head_reason_valid=reason_valid, head_reason_id=reason, resolution=resolution,
                     qualified_service_stage=stage, qualified_WB_head_relation=owner,
                     samples=count, unique_qualified_WB_keys=len(unique[key]))
                for key, count in groups.items()
                for pc, signature, mask, valid, ready, reason_valid, reason, resolution, stage, owner in [key]]
        projection = Counter()
        for row in rows:
            projection[row['architectural_PC'], str(row['final_PSV'])] += row['samples']
        expected = Counter()
        for row in phase['profiles']:
            if row['view'] == 'TEA' and row['state'] == 'Stalled':
                expected[row['pc'], str(row['signature'])] += row['roi_scaled_CPI']
        assert projection.keys() == expected.keys()
        for key, count in projection.items():
            assert abs(count / sum(p['sampled_cycles'] for p in phases) * profile['native_CPI'] - expected[key]) < 1e-12
        entry_rows = [dict(architectural_PC='unknown' if pc is None else hex(pc), final_PSV=signature,
                           resolution=resolution, qualified_service_stage=stage,
                           qualified_WB_head_entry_relation=entry_owner,
                           qualified_WB_head_relation=owner, samples=count)
                      for (pc, signature, resolution, stage, entry_owner, owner), count in entries.items()]
        assert sum(r['samples'] for r in entry_rows) == sum(r['samples'] for r in rows)
        output.append(dict(index=phase['index'], begin=phase['begin'], end=phase['end'], groups=rows,
                           head_entry_groups=entry_rows))
    return output, examples


def analyze(folder, receipt_path, retired_service_path):
    folder, receipt_path, retired_service_path = map(lambda p: Path(p).resolve(),
                                                    [folder, receipt_path, retired_service_path])
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    verify(folder, receipt['files'])
    result = json.loads((folder / 'result.json').read_text())
    assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
    service = json.loads(retired_service_path.read_text())
    assert service['passed']
    identity, profile = service['TEA_load_identity'], service['retired_phase_profile']
    assert service['source'] == receipt['source'] == result['source'] == identity['source'] == profile['source']
    assert profile['receipt_sha256'] == digest(receipt_path)
    for name, expected in identity['input_sha256'].items():
        assert digest(Path(name)) == expected, name
    assert (identity['roi_begin'], identity['roi_end']) == (
        profile['phases'][0]['begin']['cycle'], profile['phases'][-1]['end']['cycle'])
    with sqlite3.connect((folder / 'simulator.db').as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        finals = {tuple(row['MEMBER_TARGET_' + k] for k in ['ALLOCATIONCYCLE', 'ROBINDEX', 'RANK']): row
                  for row in db.execute('SELECT * FROM TeaDelayedFinal_0 WHERE ISDRAIN=0')}
        phases, examples = measure(db.execute('SELECT * FROM Tea_0 WHERE STATE=1 AND CYCLE>=?AND CYCLE<?ORDER BY CYCLE,ID',
                                             (identity['roi_begin'], identity['roi_end'])), finals, identity, profile)
    return dict(passed=True, source=receipt['source'], phases=phases, qualified_same_head_S2_before_sample=examples,
                input_sha256={str(p): digest(p) for p in [receipt_path, retired_service_path, folder / 'simulator.db']},
                tool_sha256={Path(__file__).name: digest(Path(__file__))},
                scope='Existing closed retired-service identities, all future TEA finals and raw source-cycle context. '
                      'Every Stalled PC/PSV phase margin is retained, including unknown and cancelled targets. '
                      'Raw head-reason IDs require the actual producer enum; they are ROB-entry context, not per-member causes. '
                      'Full head-pointer agreement uses WB flag/former/index but does not restore missing target allocation flags. '
                      'Separate entry relations use RobPtr.isSameEntry value/flag, preserving former/latter differences in the full-pointer relation. '
                      'Same-cycle S2 is separate from earlier S2; no issue-state correctness, unique critical path or saved cycles inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ['folder', 'receipt', 'retired-service', 'output']:
        parser.add_argument('--' + name, type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt, args.retired_service), indent=2) + '\n')


if __name__ == '__main__':
    main()
