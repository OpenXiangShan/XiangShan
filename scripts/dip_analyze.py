#!/usr/bin/env python3
"""Finalize sparse DIP samples using their selected-target events."""
import argparse
from collections import Counter, defaultdict
import csv
import json
from pathlib import Path
import sqlite3

STATES = ('Base', 'Stall', 'FrontEnd', 'Misspeculation')
CAUSES = {
    0: (),
    1: ('ROB', 'IntIQ', 'FpIQ', 'VecIQ', 'MemIQ', 'LQ', 'SQ', 'Rename',
        'Serialization', 'LSQRecovery', 'DispatchPolicy', 'LSQ'),
    2: ('ICacheMiss', 'ITLBMiss', 'Bubble'),
    3: ('BranchMispredict', 'Exception', 'MemoryOrder', 'Flush', 'Interrupt'),
}


def pointer(row, prefix):
    return (row[prefix + '_FLAG'], row[prefix + '_VALUE'], row[prefix + '_ISFORMER'])


def killed(target, culprit, flush_self):
    flag, index, former = target
    cause_flag, cause_index, cause_former = culprit
    same_entry = flag == cause_flag and index == cause_index
    after = (flag != cause_flag) ^ (index > cause_index)
    after |= same_entry and not former and cause_former
    return after or (flush_self and target == culprit)


def cause_name(state, cause):
    return '+'.join(name for bit, name in enumerate(CAUSES[state]) if cause & (1 << bit)) or 'None'


def target(row, prefix):
    return (row[prefix + '_GENERATION'], *pointer(row, prefix + '_ROBIDX'))


def analyze(samples, finals, width, warmup_instr=0, end_cycle=None):
    # Events precede the sample at the same cycle, just as in the RTL tracker.
    from heapq import merge
    pending = defaultdict(Counter)
    waiting = defaultdict(Counter)
    profile = Counter()
    sampled_cycles = reclassified = sample_rows = final_rows = 0
    first = last = None
    records = merge(((r['CYCLE'], 1, r) for r in samples),
                    ((r['CYCLE'], 0, r) for r in finals), key=lambda x: x[:2])
    for cycle, is_sample, row in records:
        if not is_sample:
            final_rows += 1
            kind = row['KIND']
            ref = target(row, 'TARGET')
            if kind == 0:  # selected ROB slot committed
                profile.update(pending.pop(ref, Counter()))
            elif kind == 1:  # selected work was squashed: keep the cause provisional
                lost = Counter()
                for counts in waiting.values():
                    lost.update(counts)
                waiting.clear()
                for old in list(pending):
                    if killed(old[1:], ref[1:], row['SELF']):
                        lost.update(pending.pop(old))
                reclassified += sum(n for (state, _, _), n in lost.items() if state != 3)
                if lost:
                    pending[ref][(3, row['CAUSE'], row['PC'] & ((1 << 64) - 1))] += sum(lost.values())
            elif kind == 2:  # the first Dispatch successor resolves frontend/rename waiting
                successor = target(row, 'NEXT')
                pc = row['PC'] & ((1 << 64) - 1)
                for (state, cause, _), count in waiting.pop(ref[0], Counter()).items():
                    pending[successor][(state, row['CAUSE'] if state == 2 else cause, pc)] += count
            elif kind == 3:  # a commit at/beyond a self-flushed cause settles it
                for old in list(pending):
                    if killed(ref[1:], old[1:], True):
                        settled = Counter({k: n for k, n in pending[old].items() if k[0] == 3})
                        profile.update(settled)
                        pending[old] -= settled
                        if not pending[old]:
                            del pending[old]
            continue

        sample_rows += 1
        if row['TOTALRETIRED'] < warmup_instr or end_cycle is not None and cycle >= end_cycle:
            continue
        sampled_cycles += 1
        if first is None:
            first = row
        last = row
        for lane in range(width):
            prefix = f'SLOTS_{lane}'
            key = (row[prefix + '_STATE'], row[prefix + '_CAUSE'], row[prefix + '_PC'] & ((1 << 64) - 1))
            kind = row[prefix + '_KIND']
            ref = target(row, prefix + '_TARGET')
            if kind == 0:
                profile[key] += 1
            elif kind == 1:
                pending[ref][key] += 1
            else:
                waiting[ref[0]][key] += 1

    if first is None:
        raise ValueError('DIP contains no samples inside the requested measurement window')
    unresolved = sum(sum(c.values()) for c in waiting.values()) + sum(sum(c.values()) for c in pending.values())
    resolved = sum(profile.values())
    total = sampled_cycles * width
    if resolved + unresolved != total:
        raise ValueError('DIP slot accounting does not conserve dispatch capacity')
    states = Counter()
    for (state, cause, pc), count in profile.items():
        states[STATES[state]] += count
    span = last['CYCLE'] - first['CYCLE'] + 1
    retired = last['TOTALRETIRED'] + last['RETIRED'] - first['TOTALRETIRED']
    summary = {
        'dispatch_width': width,
        'warmup_instr': warmup_instr,
        'end_cycle': end_cycle,
        'sampled_begin_cycle': first['CYCLE'],
        'sampled_end_cycle': last['CYCLE'],
        'sampled_span_cycles': span,
        'sampled_span_retired': retired,
        'sampled_span_ipc': retired / span,
        'sampled_cycles': sampled_cycles,
        'sampled_slots': total,
        'resolved_slots': resolved,
        'unresolved_slots': unresolved,
        'unresolved_cycles': unresolved / width,
        'reclassified_slots': reclassified,
        'state_cycles': {state: states[state] / width for state in STATES},
        'sample_rows': sample_rows,
        'final_rows': final_rows,
        'trace_rows': sample_rows + final_rows,
        'weight_unit': 'sampled cycles (one slot = 1 / dispatch_width)',
    }
    return profile, summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('database', type=Path)
    parser.add_argument('--hart', type=int, default=0)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--warmup-instr', type=int, default=0, help='exclude samples whose preceding retirement prefix is below this count')
    parser.add_argument('--end-cycle', type=int, help='exclusive measurement end; later final records still finalize samples')
    args = parser.parse_args()
    with sqlite3.connect(f'file:{args.database.resolve()}?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        table = f'Dip_{args.hart}'
        columns = [row[1] for row in db.execute(f'PRAGMA table_info({table})')]
        width = sum(column.endswith('_STATE') and column.startswith('SLOTS_') for column in columns)
        final_table = f'DipDelayedFinal_{args.hart}'
        if not width or not db.execute('SELECT 1 FROM sqlite_master WHERE name=?', (final_table,)).fetchone():
            raise ValueError(f'record both {table} and {final_table} with WITH_DIP=1')
        profile, summary = analyze(
            db.execute(f'SELECT * FROM {table} ORDER BY CYCLE, ID'),
            db.execute(f'SELECT * FROM {final_table} ORDER BY CYCLE, CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END, ID'),
            width, args.warmup_instr, args.end_cycle)
    args.output.mkdir(parents=True, exist_ok=True)
    with (args.output / 'pics.csv').open('w', newline='') as stream:
        writer = csv.writer(stream)
        writer.writerow(('pc', 'state', 'cause_mask', 'causes', 'slots', 'sampled_cycles'))
        for (state, cause, pc), count in profile.most_common():
            writer.writerow((f'0x{pc:016x}', STATES[state], cause, cause_name(state, cause), count, count / width))
    (args.output / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
    print(json.dumps(summary, indent=2))


if __name__ == '__main__':
    main()
