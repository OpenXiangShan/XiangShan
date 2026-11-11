"""Reconstruct owner and merged-demand residence from a closed L2MshrLifecycle batch trace.

Native clean/dump markers define the same completion cohorts as existing
MSHR histograms. Owner residence and post-merge tail overlap CPU execution;
neither is CPU critical latency or recoverable cycles.
"""
import argparse
from collections import Counter, defaultdict
import hashlib
import json
import math
from pathlib import Path
import re
import sqlite3

from native_perf import read_blocks, unique_counters
from tea_dip_l2_traffic_native import residence

SOURCES = ('NoWhere CPUInst CPULoadData CPUStoreData CPUAtomicData L1InstPrefetch '
           'L1DataPrefetch PTW Prefetch2L2BOP Prefetch2L2PBOP Prefetch2L2SMS '
           'Prefetch2L2Stream Prefetch2L2Stride Prefetch2L2TP Prefetch2L2Berti '
           'Prefetch2L2NL Prefetch2L2CDP Prefetch2L2Unknown Prefetch2L3Stream '
           'Prefetch2L3Stride Prefetch2L3Berti Prefetch2L3Unknown Prefetch2L2Matrix '
           'MatrixRead MatrixWrite').split()


def moments(samples):
    values = sorted(samples)
    count = len(values)
    return dict(count=count, cycle_sum=sum(values), mean=sum(values) / count if count else None,
                p50=values[math.ceil(count * .5) - 1] if count else None,
                p90=values[math.ceil(count * .9) - 1] if count else None,
                p99=values[math.ceil(count * .99) - 1] if count else None,
                maximum=values[-1] if count else None)


def groups(episodes, begin):
    grouped = defaultdict(list)
    for item in episodes:
        key = (item['source'], item['channel'], item['prefetch'], item['miss'], item.get('merge_source'))
        grouped[key].append(item)
    result = []
    for key, items in sorted(grouped.items(), key=lambda x: str(x[0])):
        source, channel, prefetch, miss, merge_source = key
        merged = [x for x in items if 'merge_cycle' in x]
        result.append(dict(owner_source=SOURCES[source], owner_channel=channel,
                           owner_prefetch=prefetch, owner_directory_miss=miss,
                           merged_source=SOURCES[merge_source] if merge_source is not None else None,
                           completed_owner_residence=moments([x['age'] for x in items]),
                           post_merge_to_free=moments([x['end'] - x['merge_cycle'] for x in merged]),
                           post_merge_within_epoch=moments([x['end'] - max(begin, x['merge_cycle']) for x in merged]),
                           started_before_epoch=sum(x['start'] <= begin for x in items),
                           merged_before_epoch=sum(x['merge_cycle'] <= begin for x in merged)))
    return result


def reconstruct(rows, slots):
    active = {}
    completed = defaultdict(list)
    starts = defaultdict(int)
    counts = defaultdict(Counter)
    totals = defaultdict(Counter)
    allocations = defaultdict(Counter)
    merges = Counter()
    snapshots = defaultdict(dict)
    allocation_count = merge_count = free_count = simultaneous_free_batches = 0
    for row in rows:
        cycle, bank = row['NATIVECYCLE'], row['EVENTS_BANK']
        freed = [i for i in slots if row[f'EVENTS_FREED_{i}_VALID']]
        simultaneous_free_batches += len(freed) > 1
        for entry in freed:
            key = bank, entry
            item = active.pop(key)
            age = row[f'EVENTS_FREED_{entry}_BITS']
            assert age == cycle - item['start'], (key, cycle, age, item)
            item.update(end=cycle, age=age)
            completed[bank].append(item)
            counts[bank][entry] += 1
            totals[bank][entry] += age
            free_count += 1
        if row['EVENTS_ALLOCATED_VALID']:
            entry = row['EVENTS_ALLOCATED_BITS_ENTRY']
            key = bank, entry
            assert entry in slots and key not in active, (key, cycle)
            source = row['EVENTS_ALLOCATED_BITS_SOURCE']
            assert 0 <= source < len(SOURCES)
            item = dict(start=cycle, line=row['EVENTS_ALLOCATED_BITS_LINE'], source=source,
                        channel=row['EVENTS_ALLOCATED_BITS_CHANNEL'],
                        prefetch=bool(row['EVENTS_ALLOCATED_BITS_PREFETCH']),
                        miss=bool(row['EVENTS_ALLOCATED_BITS_MISS']))
            active[key] = item
            if item['channel'] & 1 and item['miss']:
                allocations[bank]['Prefetch' if item['prefetch'] else 'CPU'] += 1
            allocation_count += 1
        if row['EVENTS_MERGED_VALID']:
            entry = row['EVENTS_MERGED_BITS_ENTRY']
            item = active[bank, entry]
            assert item['line'] == row['EVENTS_MERGED_BITS_LINE'] and 'merge_cycle' not in item
            assert item['prefetch'] and item['miss'], item
            source = row['EVENTS_MERGED_BITS_SOURCE']
            assert 0 <= source < len(SOURCES)
            item.update(merge_cycle=cycle, merge_source=source)
            merges[bank] += 1
            merge_count += 1
        if row['DUMP'] and row['LOGENABLE']:
            assert bank not in snapshots[cycle], (bank, cycle)
            unfinished = [dict(entry=e, owner_source=SOURCES[x['source']],
                               merged_source=SOURCES[x['merge_source']] if 'merge_source' in x else None,
                               observed_owner_age=cycle - x['start'],
                               observed_post_merge_age=cycle - x['merge_cycle'] if 'merge_cycle' in x else None)
                          for (b, e), x in active.items() if b == bank]
            snapshots[cycle][bank] = dict(begin_after_clean=starts[bank],
                slot_counts=dict(counts[bank]), slot_sums=dict(totals[bank]),
                allocation_counts=dict(allocations[bank]), merge_pulses=merges[bank],
                completion_groups=groups(completed[bank], starts[bank]), unfinished=unfinished)
        if row['CLEAN']:
            starts[bank] = cycle
            completed[bank].clear()
            counts[bank].clear()
            totals[bank].clear()
            allocations[bank].clear()
            merges[bank] = 0
    return dict(snapshots=dict(snapshots), allocation_events=allocation_count, merge_events=merge_count,
                free_events=free_count, simultaneous_free_batches=simultaneous_free_batches,
                unfinished_at_trace_end=len(active))


def analyze(database, native_log):
    frames = read_blocks(native_log)
    last = max(frames)
    with sqlite3.connect(database.resolve().as_uri() + '?mode=ro', uri=True) as connection:
        connection.row_factory = sqlite3.Row
        columns = [r[1] for r in connection.execute('PRAGMA table_info(L2MshrLifecycle)')]
        slots = sorted(int(m[1]) for c in columns if (m := re.fullmatch(r'EVENTS_FREED_(\d+)_VALID', c)))
        assert slots and slots == list(range(len(slots))), slots
        rows = connection.execute('SELECT * FROM L2MshrLifecycle WHERE NATIVECYCLE <= ? '
                                  'ORDER BY NATIVECYCLE, EVENTS_BANK', (last,))
        result = reconstruct(rows, slots)
    assert result['snapshots'].keys() == frames.keys()
    phases = []
    for cycle, banks in sorted(result.pop('snapshots').items()):
        native = residence(frames[cycle])
        expected = {}
        for name, values in native['slots'].items():
            match = re.fullmatch(r'.*\.slices_(\d+)\.mshrCtl\.mshr_latency_(\d+)', name)
            assert match, name
            expected[int(match[1]), int(match[2])] = values
        assert set(banks) == {b for b, e in expected}
        for (bank, entry), values in expected.items():
            snapshot = banks[bank]
            assert snapshot['slot_counts'].get(entry, 0) == values['sampled'], (cycle, bank, entry, 'count')
            assert snapshot['slot_sums'].get(entry, 0) == values['sum'], (cycle, bank, entry, 'sum')
        scalars = unique_counters(frames[cycle])
        legacy = {}
        for name, value in scalars.items():
            match = re.fullmatch(r'.*\.slices_(\d+)\.reqBuf\.req_buffer_mergeA', name)
            if match:
                legacy[int(match[1])] = value
        assert set(legacy) == set(banks)
        for bank, value in legacy.items():
            assert value <= banks[bank]['merge_pulses'], (cycle, bank, value)
            banks[bank]['legacy_filtered_merge_condition'] = value
        observed = {kind: sum(x['allocation_counts'].get(kind, 0) for x in banks.values())
                    for kind in ['CPU', 'Prefetch']}
        observed['All'] = sum(observed.values())
        for kind, value in observed.items():
            counters = [v for k, v in scalars.items() if k.endswith('.topDown.mshr_count_' + kind)]
            assert counters == [value], (cycle, kind, counters, value)
        phases.append(dict(native_dump_cycle=cycle, exact_native_completion_count_and_sum_equal=True,
                           exact_native_A_miss_allocation_partition_equal=True, banks=banks))
    result.update(passed=True, phases=phases, database_sha256=hashlib.sha256(database.read_bytes()).hexdigest(),
                  native_log_sha256=hashlib.sha256(native_log.read_bytes()).hexdigest(),
                  scope='Captured allocation/merge/free batches through the last printed native dump. '
                        'Owner age equals its original full-width timer. Native epoch boundaries and '
                        'per-slot completion count/sum are checked exactly. Post-merge-to-free includes '
                        'protocol completion and may extend beyond CPU data return. Censored requests '
                        'are listed separately; no CPU critical delay or unique DRAM cause is inferred.')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('database', type=Path, help='Collected, closed simulator.db')
    parser.add_argument('native_log', type=Path, help='The same guest\'s closed simulator_err.txt')
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    body = json.dumps(analyze(args.database, args.native_log), indent=2) + '\n'
    if args.output:
        args.output.write_text(body)
    else:
        print(body, end='')


if __name__ == '__main__':
    main()
