"""Reconstruct canonical L2 bank/MMIO/stash service from a closed six-port trace.

All active cycles are recorded, so inactive gaps reset ArbPerf's wait timers.
Histogram snapshots exclude the dump cycle; accepted wait buckets include it.
The per-port metrics use the histogram window and do not identify CPU requests.
"""
import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import sqlite3

from native_perf import read_blocks, unique_counters

PORTS = ['bank0', 'bank1', 'bank2', 'bank3', 'MMIO', 'stash']


def reconstruct(rows):
    snapshots = {}
    pending = [0] * 6
    previous = None
    multiplicity = Counter()
    masks = Counter()
    active, accepted, local_wait, output_wait = ([0] * 6 for _ in range(4))
    accepted_wait = Counter()
    for row in rows:
        cycle = row['NATIVECYCLE']
        assert previous is None or cycle > previous, 'Duplicate or unordered trace cycles'
        if previous is None or cycle != previous + 1:
            pending = [0] * 6
        previous = cycle
        valid, ready, out_ready = row['EVENTS_VALIDMASK'], row['EVENTS_READYMASK'], row['EVENTS_OUTREADY']
        assert 0 <= valid < 64 and 0 <= ready < 64
        fire = valid & ready
        assert fire.bit_count() == int(bool(valid) and bool(out_ready)), 'Non-work-conserving handshake'
        snapshot = None
        if row['NATIVEDUMP'] and row['LOGENABLE']:
            snapshot = dict(active_input_multiplicity={i: multiplicity[i] for i in range(1, 7)},
                mask_ready_counts=[dict(valid_mask=v, output_ready=r, cycles=n)
                                   for (v, r), n in sorted(masks.items())],
                ports={name: dict(active_cycles=active[i], accepted_requests=accepted[i],
                    ready_qualified_wait_cycles=local_wait[i], output_not_ready_cycles=output_wait[i])
                    for i, name in enumerate(PORTS)})
        if valid:
            multiplicity[valid.bit_count()] += 1
            masks[valid, bool(out_ready)] += 1
        for i in range(6):
            v, f = bool(valid & (1 << i)), bool(fire & (1 << i))
            active[i] += v
            accepted[i] += f
            local_wait[i] += v and not f and bool(out_ready)
            output_wait[i] += v and not out_ready
            if f:
                accepted_wait[min(pending[i], 12)] += 1
            if not v or f:
                pending[i] = 0
            elif out_ready:
                pending[i] += 1
        if snapshot is not None:
            snapshot['native_next_accepted_wait_buckets'] = {i: accepted_wait[i] for i in range(13)}
            assert cycle not in snapshots
            snapshots[cycle] = snapshot
        if row['CLEAN']:
            multiplicity.clear()
            masks.clear()
            active, accepted, local_wait, output_wait = ([0] * 6 for _ in range(4))
            accepted_wait.clear()
    return snapshots


def analyze(database, native_log):
    frames = read_blocks(native_log)
    assert frames, 'No native PERF frames'
    with sqlite3.connect(Path(database).resolve().as_uri() + '?mode=ro', uri=True) as connection:
        connection.row_factory = sqlite3.Row
        snapshots = reconstruct(connection.execute(
            'SELECT * FROM L2TxreqArbTrace WHERE NATIVECYCLE <= ? ORDER BY NATIVECYCLE', (max(frames),)))
    assert snapshots.keys() == frames.keys(), 'Trace/native dump coverage differs'
    phases = []
    for cycle, snapshot in sorted(snapshots.items()):
        values = unique_counters(frames[cycle])
        prefix, = [name.removesuffix('ArbConflict_sampled') for name in values
                   if name.endswith('.l2cache.txreq_arbArbConflict_sampled')]
        histogram = snapshot['active_input_multiplicity']
        assert values[prefix + 'ArbConflict_sampled'] == sum(histogram.values())
        assert values[prefix + 'ArbConflict_sum'] == sum(i * n for i, n in histogram.items())
        assert values[prefix + 'ArbConflict_underflow'] == values[prefix + 'ArbConflict_overflow'] == 0
        for i, count in histogram.items():
            assert values[f'{prefix}ArbConflict_{i}_{i + 1}'] == count
        for i, count in snapshot['native_next_accepted_wait_buckets'].items():
            assert values[prefix + 'ArbWaitTime_' + str(i)] == count, (cycle, i, 'wait cohort')
        for port in snapshot['ports'].values():
            assert port['active_cycles'] == sum(port[k] for k in [
                'accepted_requests', 'ready_qualified_wait_cycles', 'output_not_ready_cycles'])
        phases.append(dict(native_dump_cycle=cycle, exact_native_histogram_and_wait_cohorts_equal=True,
                           **snapshot))
    return dict(passed=True, phases=phases,
                database_sha256=hashlib.sha256(Path(database).read_bytes()).hexdigest(),
                native_log_sha256=hashlib.sha256(Path(native_log).read_bytes()).hexdigest(),
                scope='Canonical six-port egress after the per-bank TXREQ FIFOs. Per-port conditions '
                      'and mask/ready counts use the histogram window; native accepted wait buckets '
                      'include the dump handshake and may carry wait across clean boundaries. '
                      'Input wait cycles overlap across ports and CPU execution. There is no CPU '
                      'ROB/uop/request identity, data-return latency or unique credit/DRAM cause.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('database', type=Path, help='Collected, closed simulator.db')
    parser.add_argument('native_log', type=Path, help='The same closed guest native PERF log')
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    body = json.dumps(analyze(args.database, args.native_log), indent=2) + '\n'
    if args.output:
        args.output.write_text(body)
    else:
        print(body, end='')


if __name__ == '__main__':
    main()
