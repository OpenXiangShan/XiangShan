"""Validate closed LLPTW state/request traces against native counts and source time.

Slot metadata and native-qualified memory requests are observations, not unique
load lifetimes. A VPN or source match does not establish a CPU critical path.
"""
import argparse
from collections import Counter
import csv
from itertools import groupby
import json
from pathlib import Path
import sqlite3

from native_perf import read_blocks
from tea_dip_clock_alignment import core_native_alignment
from tea_dip_query_endpoint_evidence import digest, verify


def measure(rows, blocks):
    dumps = [row['core_cycle'] for row in rows if row['kind'] == 0 and row['native_dump']]
    assert len(dumps) == len(blocks) and len(set(dumps)) == len(dumps)
    offset, = {stamp - cycle for stamp, cycle in zip(sorted(blocks), dumps)}
    assert all(a['core_cycle'] <= b['core_cycle'] for a, b in zip(rows, rows[1:]))
    owners = [None] * 8
    intervals, requests = [], []
    groups = [(cycle, list(group)) for cycle, group in groupby(rows, lambda row: row['core_cycle'])]
    for index, (cycle, group) in enumerate(groups):
        changed = set()
        for row in group:
            assert row['kind'] in (0, 1, 2)
            if row['kind'] != 2:
                continue
            slot = row['slot']
            assert 0 <= slot < 8 and slot not in changed and 0 <= row['state'] < 14
            changed.add(slot)
            owners[slot] = {key: row[key] for key in ['slot', 'state', 'virtual_page', 'source', 'translation_stage', 'wait_id']}
        assert all(owner is not None for owner in owners), 'Initial LLPTW slot snapshot incomplete'
        for row in group:
            if row['kind'] != 1:
                continue
            assert row['slot'] == 8 and row['state'] == 255 and row['wait_id'] == 8 and row['source'] in range(3)
            candidates = [owner['slot'] for owner in owners if owner['state'] == 4 and all(
                owner[key] == row[key] for key in ['virtual_page', 'source', 'translation_stage'])]
            assert candidates, 'Memory request has no current mem-request owner'
            requests.append(dict(**row, matching_current_slots=candidates))
        stop = groups[index + 1][0] if index + 1 < len(groups) else cycle + 1
        active = [dict(owner) for owner in owners if owner['state'] != 0]
        intervals.append(dict(core_begin=cycle, core_exclusive_end=stop, entries=active))
    assert intervals and intervals[0]['core_begin'] <= 0
    boundaries = [row for row in rows if row['kind'] == 0]
    phases = []
    for (stamp, block), end in zip(sorted(blocks.items()), dumps):
        begin = max((row['core_cycle'] for row in boundaries if row['clean'] and row['core_cycle'] < end), default=-1)
        low, high = begin + 1, end + 1
        assert intervals[-1]['core_exclusive_end'] >= high
        counts = Counter()
        waiting_sources = Counter()
        waiting_source_masks = Counter()
        for interval in intervals:
            duration = min(high, interval['core_exclusive_end']) - max(low, interval['core_begin'])
            if duration <= 0:
                continue
            active = interval['entries']
            waiting = [owner for owner in active if owner['state'] == 5]
            counts['llptw_entry_occupancy_cycle'] += duration * len(active)
            counts['llptw_mem_wait_entry_cycle'] += duration * len(waiting)
            counts['llptw_mem_busy_cycle'] += duration * bool(waiting)
            mask = 0
            for owner in waiting:
                assert owner['source'] in range(3)
                waiting_sources[str(owner['source'])] += duration
                mask |= 1 << owner['source']
            waiting_source_masks[str(mask)] += duration
        selected = [row for row in requests if low <= row['core_cycle'] < high]
        counts['llptw_mem_req_count'] = len(selected)
        for source in range(3):
            counts['llptw_mem_req_count_source' + str(source)] = sum(row['source'] == source for row in selected)
        marginals = {}
        for metric, value in counts.items():
            values, = [values for name, values in block.items() if name.endswith('.ptw.ptw.llptw.' + metric)]
            expected, = values
            assert value == expected, (stamp, metric, value, expected)
            marginals[metric] = value
        assert sum(waiting_sources.values()) == counts['llptw_mem_wait_entry_cycle']
        assert sum(waiting_source_masks.values()) == high - low
        phases.append(dict(begin_core_inclusive=low, end_core_exclusive=high, native_dump_stamp=stamp,
                           native_marginals=marginals, waiting_entry_cycles_by_source=dict(waiting_sources),
                           waiting_source_mask_cycles=dict(waiting_source_masks)))
    return dict(passed=True, core_to_native_offset=offset, phases=phases,
                active_intervals=[row for row in intervals if row['entries']], memory_requests=requests,
                recorded_state_changes=sum(row['kind'] == 2 for row in rows),
                coverage=dict(core_begin=intervals[0]['core_begin'], core_exclusive_end=intervals[-1]['core_exclusive_end']),
                scope='Half-open source-cycle intervals reconstructed from complete initial snapshots and state/owner changes. '
                      'Only active entries are emitted; coverage includes idle intervals. State5 is LLPTW memory waiting, '
                      'state4 is its memory-request state and can still be masked. Matching slot metadata is not an arbiter grant; '
                      'wait_id can share a memory episode and slots can be reused. Source1 aggregates L1-TLB requests, '
                      'source2 is L2-TLB prefetch. Entry-time and overlapping source activity are not CPU lost cycles, '
                      'DRAM-only latency, demand-load identity, or recoverable CPI.')


def analyze(folder, receipt_path):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and result['original_process_closed']
    assert result['difftest_passed'] and result['exitcode'] == 0 and result['source'] == receipt['source']
    assert receipt['point'] == result['point'] == folder.name and all(result['sampler_flags'].values())
    assert result['process'] == receipt['process']
    verify(folder, receipt['files'])
    probe = json.loads((folder / 'probe-receipt.json').read_text())
    assert probe['closure_receipt_sha256'] == digest(receipt_path) and probe['source'] == receipt['source']
    verify(folder, probe['files'])
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    assert proof['passed'] and proof['original_files_unchanged'] and proof['binary_sha256'] == result['binary_sha256']
    binding = proof['binding']['LLPTW_binding']
    assert binding['new_RTL_state_bits'] == 0 and binding['per_cycle_native_request_partition_and_waiting_asserted']
    assert binding['next_register_commit_asserted']
    with (folder / 'llptw-probe.csv').open() as stream:
        rows = [{key: int(value) for key, value in row.items()} for row in csv.DictReader(stream)]
    output = measure(rows, read_blocks(folder / 'simulator_err.txt'))
    with sqlite3.connect((folder / 'simulator.db').as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        clock = core_native_alignment(db, folder / 'simulator_err.txt')
    assert output['core_to_native_offset'] == clock['core_to_native_offset']
    output.update(point=folder.name, source=receipt['source'], clock_alignment=clock,
                  receipt_sha256=digest(receipt_path), trace_sha256=digest(folder / 'llptw-probe.csv'),
                  build_proof_sha256=digest(folder / 'probe-build-proof.json'), tool_sha256=digest(Path(__file__)))
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', required=True, type=Path)
    parser.add_argument('--receipt', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    result = analyze(args.folder, args.receipt)
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(dict(passed=True, point=result['point'], memory_requests=len(result['memory_requests']),
                          state_changes=result['recorded_state_changes'], phases=result['phases']), indent=2))


if __name__ == '__main__':
    main()
