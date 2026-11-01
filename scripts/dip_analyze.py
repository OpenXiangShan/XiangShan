#!/usr/bin/env python3
"""Finalize dispatch samples using the complete Dip_<hart> lifecycle trace."""
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


def analyze(rows, width, commit_width, warmup_instr=0, end_cycle=None):
    # Key includes wrap flag and Former/Latter. All members/uops sharing a ROB
    # slot share retirement fate, but retain their own static PC in the Counter.
    pending = defaultdict(Counter)
    waiting = Counter()
    profile = Counter()
    sampled_cycles = 0
    trace_rows = 0
    reclassified = 0
    retired = 0
    retired_at_begin = 0
    begin_cycle = None
    last_cycle = None
    last_flush_target = None
    last_flush_settled = False

    for row in rows:
        trace_rows += 1
        cycle = row['ROB_CYCLE']
        inside_end = end_cycle is None or cycle < end_cycle
        if inside_end:
            last_cycle = cycle
        if begin_cycle is None and warmup_instr == 0:
            begin_cycle = cycle
        if inside_end:
            retired += row['ROB_RETIRED']
        if begin_cycle is None and retired >= warmup_instr:
            begin_cycle = cycle + 1
            retired_at_begin = retired
        flush = row['ROB_FLUSH_VALID']
        if flush:
            culprit = pointer(row, 'ROB_FLUSH_BITS_ROBIDX')
            last_flush_target = culprit
            last_flush_settled = False
            pc = row['ROB_FLUSH_BITS_PC'] & ((1 << 64) - 1)
            cause = row['ROB_FLUSH_BITS_CAUSE']
            lost = sum(waiting.values())
            newly_reclassified = lost
            waiting.clear()
            for target in list(pending):
                if target is not None and killed(target, culprit, row['ROB_FLUSH_BITS_SELF']):
                    counts = pending.pop(target)
                    lost += sum(counts.values())
                    newly_reclassified += sum(count for (state, _, _), count in counts.items() if state != 3)
            if lost:
                pending[culprit][(3, cause, pc)] += lost
            reclassified += newly_reclassified

        for lane in range(2 * commit_width):
            prefix = f'ROB_COMMITS_{lane}'
            if row[prefix + '_VALID']:
                target = pointer(row, prefix + '_BITS')
                profile.update(pending.pop(target, Counter()))
                # A self-flushed exception/load need not retire at its old
                # slot. An in-order commit at or beyond it settles the older
                # context before ROB indices can be reused.
                for cause_target in list(pending):
                    if cause_target is not None and killed(target, cause_target, True):
                        counts = pending[cause_target]
                        settled = Counter({key: count for key, count in counts.items() if key[0] == 3})
                        profile.update(settled)
                        counts -= settled
                        if not counts:
                            del pending[cause_target]
                if last_flush_target is not None and killed(target, last_flush_target, True):
                    last_flush_settled = True

        if row['DISPATCH_VALIDMASK'] and not flush:
            first = (row['DISPATCH_VALIDMASK'] & -row['DISPATCH_VALIDMASK']).bit_length() - 1
            prefix = f'DISPATCH_UOPS_{first}'
            target = pointer(row, prefix + '_ROBIDX')
            pc = row[prefix + '_PC'] & ((1 << 64) - 1)
            frontend_cause = row[prefix + '_PSV'] & 3 or 4
            for (state, cause), count in waiting.items():
                pending[target][(state, frontend_cause if state == 2 else cause, pc)] += count
            waiting.clear()

        if not row['SAMPLED'] or begin_cycle is None or cycle < begin_cycle or not inside_end:
            continue
        sampled_cycles += 1
        for lane in range(width):
            prefix = f'SLOTS_{lane}'
            state = row[prefix + '_STATE']
            cause = row[prefix + '_CAUSE']
            if row[prefix + '_TARGETVALID']:
                pc = row[prefix + '_TARGET_PC'] & ((1 << 64) - 1)
                if state == 3:
                    # A younger mispredicted branch can itself be squashed by
                    # an older redirect. Keep its slots provisional too.
                    if last_flush_settled:
                        profile[(state, cause, pc)] += 1
                    else:
                        pending[last_flush_target][(state, cause, pc)] += 1
                else:
                    target = pointer(row, prefix + '_TARGET_ROBIDX')
                    pending[target][(state, cause, pc)] += 1
            else:
                waiting[(state, cause)] += 1

    if begin_cycle is None or last_cycle is None or last_cycle < begin_cycle:
        raise ValueError('trace does not contain a measurement window after warmup')
    roi_cycles = last_cycle - begin_cycle + 1
    unresolved = sum(waiting.values()) + sum(sum(counts.values()) for counts in pending.values())
    resolved = sum(profile.values())
    total = sampled_cycles * width
    if resolved + unresolved != total:
        raise ValueError('DIP slot accounting does not conserve dispatch capacity')
    states = Counter()
    for (state, cause, pc), count in profile.items():
        states[STATES[state]] += count
    summary = {
        'dispatch_width': width,
        'roi_begin_cycle': begin_cycle,
        'roi_end_cycle': last_cycle,
        'roi_cycles': roi_cycles,
        'roi_retired': retired - retired_at_begin,
        'roi_ipc': (retired - retired_at_begin) / roi_cycles,
        'sampled_cycles': sampled_cycles,
        'sampled_slots': total,
        'resolved_slots': resolved,
        'unresolved_slots': unresolved,
        'unresolved_cycles': unresolved / width,
        'reclassified_slots': reclassified,
        'state_cycles': {state: states[state] / width for state in STATES},
        'trace_rows': trace_rows,
        'weight_unit': 'sampled cycles (one slot = 1 / dispatch_width)',
    }
    return profile, summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('database', type=Path)
    parser.add_argument('--hart', type=int, default=0)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--warmup-instr', type=int, default=0, help='exclude cycles up to the first retirement boundary reaching this count')
    parser.add_argument('--end-cycle', type=int, help='exclusive measurement end; later lifecycle records still finalize samples')
    args = parser.parse_args()
    with sqlite3.connect(f'file:{args.database.resolve()}?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        table = f'Dip_{args.hart}'
        columns = [row[1] for row in db.execute(f'PRAGMA table_info({table})')]
        width = sum(column.endswith('_STATE') and column.startswith('SLOTS_') for column in columns)
        commit_width = sum(column.endswith('_VALID') and column.startswith('ROB_COMMITS_') for column in columns) // 2
        if not width or not commit_width:
            raise ValueError(f'{table} is missing; build WITH_TEA=1 and record --dump-select-db {table}')
        profile, summary = analyze(db.execute(f'SELECT * FROM {table} ORDER BY ROB_CYCLE, ID'), width, commit_width, args.warmup_instr, args.end_cycle)
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
