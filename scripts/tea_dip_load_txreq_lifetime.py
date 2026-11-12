"""Inspect earlier same-line TXREQ conditions in sampled loads' allocation/WB lifetimes.

Addresses are retrospective and requests can be unrelated CPU or prefetch traffic.
Lifetimes can overlap: their conditions are not additive CPU lost cycles.
"""
from bisect import bisect_left
from collections import Counter, defaultdict
from pathlib import Path
import argparse
import hashlib
import json
import sqlite3

from tea_dip_load_txreq_association import analyze as associations
from tea_dip_l2_txreq_head import READ_OPCODES


def associate_lifetimes(samples, trace_rows, offset):
    """Consume native-cycle-ordered rows; deduplicate TEA samples by WB ID."""
    groups, indexed = defaultdict(list), defaultdict(dict)
    for row in samples:
        if 'physical_address' in row and row['allocation_cycle'] <= row['sample_cycle'] < row['writeback_cycle']:
            groups[row['writeback_id']].append(row)
    for row in trace_rows:
        cycle = row['NATIVECYCLE']
        for port in range(4):
            if row['EVENTS_VALIDMASK'] & (1 << port) and row[f'EVENTS_REQUESTS_{port}_OPCODE'] in READ_OPCODES:
                line = row[f'EVENTS_REQUESTS_{port}_ADDRESS'] >> 6
                item = indexed[line].setdefault(cycle, dict(cycle=cycle, out_ready=bool(row['EVENTS_OUTREADY']), served=False, ports=[]))
                item['served'] |= bool(row['EVENTS_READYMASK'] & (1 << port))
                item['ports'].append(dict(port=port, local_transaction=row[f'EVENTS_REQUESTS_{port}_TRANSACTION'], opcode=row[f'EVENTS_REQUESTS_{port}_OPCODE']))
    sequences = {line: (list(rows), list(rows.values())) for line, rows in indexed.items()}
    records = []
    for wb, targets in groups.items():
        first = targets[0]
        start, end = first['allocation_cycle'] + offset, first['writeback_cycle'] + offset
        cycles, entries = sequences.get(first['physical_address'] >> 6, ([], []))
        selected = entries[bisect_left(cycles, start):bisect_left(cycles, end)]
        classify = lambda e: 'downstream_not_ready' if not e['out_ready'] else 'served' if e['served'] else 'other_port_served'
        first_sample = min(r['sample_cycle'] + offset for r in targets)
        records.append(dict(pc=first['pc'], writeback_id=wb, physical_address=first['physical_address'],
            allocation_core_cycle=first['allocation_cycle'], final_issue_core_cycle=first['issue_cycle'], writeback_core_cycle=first['writeback_cycle'],
            target_samples=len(targets), samples_before_final_issue=sum(r['sample_cycle'] < r['issue_cycle'] for r in targets),
            lifetime_read_condition_cycles=dict(Counter(classify(e) for e in selected)),
            matching_cycles_before_first_sample=sum(e['cycle'] < first_sample for e in selected),
            waiting_cycles_before_first_sample=sum(e['cycle'] < first_sample and classify(e) != 'served' for e in selected), observed_matching_events=selected))
    return records


def analyze(folder, receipt):
    data = associations(folder, receipt)
    with sqlite3.connect((Path(folder).resolve() / 'simulator.db').as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        loads = associate_lifetimes(data['samples'], db.execute('SELECT * FROM L2TxreqArbTrace ORDER BY NATIVECYCLE'), data['clock_alignment']['core_to_native_offset'])
    data['lifetime_egress'] = dict(unique_sampled_loads=len(loads), sampled_targets=sum(r['target_samples'] for r in loads),
        samples_before_final_issue=sum(r['samples_before_final_issue'] for r in loads),
        loads_with_lifetime_same_line_read=sum(bool(r['lifetime_read_condition_cycles']) for r in loads),
        loads_with_waiting_before_first_sample=sum(r['waiting_cycles_before_first_sample'] > 0 for r in loads), loads=loads)
    data['source_sha256'][str(Path(__file__).resolve())] = hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
    data['scope'] += (' Allocation-to-WB inspection includes earlier attempts, excludes samples at/after WB and deduplicates WB IDs. '
                      'Earlier same-line bank-head conditions expose a sampling blind spot, not unique CPU request identity or recoverable delay. '
                      'Overlapping lifetimes and possible prefetch/line/txn reuse prohibit summing their conditions as CPU lost cycles.')
    return data


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', required=True, type=Path)
    parser.add_argument('--receipt', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    assert not args.output.exists()
    data = analyze(args.folder, args.receipt)
    args.output.write_text(json.dumps(data, indent=2) + '\n')
    print(json.dumps(dict(point=data['point'], **{k: v for k, v in data['lifetime_egress'].items() if k != 'loads'})))


if __name__ == '__main__':
    main()
