"""Attach registered merged-demand source identities to actual L2 owner generations."""
import argparse
from bisect import bisect_right
from collections import defaultdict
import json
from pathlib import Path

from tea_dip_l2_return_trace import read
from tea_dip_memory_return_trace import analyze as memory_analyze


def attach_merges(rows, owners):
    indexed = defaultdict(list)
    for owner in owners:
        indexed[owner['bank'], owner['slot']].append(owner)
        owner['merge_states'] = []
        owner['merged_demand'] = None
    for entries in indexed.values():
        entries.sort(key=lambda owner: owner['begin'])
    current = {}
    previous_cycle = -1
    for row in rows:
        cycle = row['core_cycle']
        assert cycle >= previous_cycle, 'Out-of-order L2 merge state'
        previous_cycle = cycle
        if row['kind'] == 0:
            continue
        assert row['kind'] == 1 and 0 <= row['bank'] < 4 and 0 <= row['mshr'] < 16
        assert row['valid'] in [0,1] and row['merge_active'] in [0,1]
        key = row['bank'], row['mshr']
        if key in current:
            assert cycle > current[key]['core_cycle'], 'Duplicate L2 merge state cycle'
        current[key] = row
        entries = indexed.get(key, [])
        at = bisect_right([owner['begin'] for owner in entries], cycle) - 1
        owner = entries[at] if at >= 0 and cycle < entries[at]['end'] else None
        assert bool(row['valid']) == (owner is not None), 'Merge/owner lifetime mismatch'
        if owner is None:
            assert not any(row[name] for name in ['line','merge_active','merge_source_id','merge_opcode'])
            continue
        assert row['line'] == owner['line'], 'Merged source bound to a different block'
        states = owner['merge_states']
        if states:
            if states[-1]['merge_active']:
                assert row['merge_active'], 'Merged demand disappeared within an owner'
                assert all(row[name] == states[-1][name] for name in ['merge_source_id','merge_opcode']), 'Merged demand identity changed'
        else:
            assert cycle == owner['begin'], 'Missing initial merge state for an owner'
        if row['merge_active'] and owner['merged_demand'] is None:
            owner['merged_demand'] = dict(source_id=row['merge_source_id'], opcode=row['merge_opcode'],
                                         first_observed=cycle, transition_observed=bool(states))
        if not row['merge_active']:
            assert row['merge_source_id'] == row['merge_opcode'] == 0
        states.append(row)
    assert len(current) == 64, 'Missing L2 merge slot snapshots'
    assert all(owner['merge_states'] for owner in owners), 'Missing owner merge provenance'
    return sum(owner['merged_demand'] is not None for owner in owners)


def analyze(folder, receipt):
    folder = Path(folder)
    result = memory_analyze(folder, receipt)
    proof = json.loads((folder/'probe-build-proof.json').read_text())['L2_merge_binding']
    assert proof['banks'] == 4 and proof['entries_per_bank'] == 16 and proof['next_register_commit_asserted']
    rows = read(folder/'load-l2-merge.csv')
    boundary = lambda records: [(row['core_cycle'],row['clean'],row['native_dump']) for row in records if row['kind']==0]
    assert boundary(rows) == boundary(read(folder/'load-l2-mshr.csv')), 'L2 merge boundary mismatch'
    count = attach_merges(rows, result['L2_owners'])
    result.update(L2_merged_owner_generations=count, L2_merge_state_rows=len(rows),
                  L2_merge_scope='Registered mergeA and saved demand source/opcode within the same bank/slot/block/lifetime. Observed state is after the accepting edge. No source is inherited across owner reuse; cross-LLC memory identity and CPU retirement are not supplied.')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    args.output.write_text(json.dumps(analyze(args.folder,args.receipt),indent=2)+'\n')


if __name__ == '__main__':
    main()
