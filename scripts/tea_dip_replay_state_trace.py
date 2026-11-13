"""Join LRQ entry-state spans to validated S2/successful-WB carried identities.

Queue entry-cycles include release/cancel cycles. S0 eligibility does not prove
first data arrival, retirement or exclusive CPU loss.
"""
import argparse
from collections import Counter, defaultdict
import csv
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_gsim_replay_state_probe import FIELDS
from tea_dip_load_writeback_trace import KEY, analyze as writebacks, match_attempts

STATES = ['waiting', 'ready', 'scheduled']


def segments(rows, end):
    current, result = {}, []
    last_cycle = -1
    for row in rows:
        cycle = row['core_cycle']
        assert cycle >= last_cycle
        last_cycle = cycle
        if row['kind'] == 0:
            continue
        assert row['kind'] == 1 and 0 <= row['entry'] < 120
        entry = row['entry']
        if entry in current:
            begin, previous = current[entry]
            assert begin < cycle, 'Duplicate same-entry cycle'
            if previous['allocated']:
                result.append((begin, min(cycle, end), previous))
        if row['allocated']:
            assert row['allocated'] == 1 and row['scheduled'] in [0, 1] and row['blocking'] in [0, 1]
            assert row['eligible'] in [0, 1] and not (row['eligible'] and row['scheduled'])
            assert 0 < row['cause'] and row['cause'] & (row['cause'] - 1) == 0
            assert 0 < row['issue_cycle'] <= cycle
        else:
            assert all(row[name] == 0 for name in FIELDS)
        current[entry] = cycle, row
    for begin, row in current.values():
        if row['allocated']:
            result.append((begin, end, row))
    return [(begin, finish, row) for begin, finish, row in result if begin < finish]


def state(row):
    return 'scheduled' if row['scheduled'] else 'ready' if row['eligible'] else 'waiting'


def union_length(intervals):
    count, previous_end = 0, None
    for begin, end in sorted(intervals):
        count += max(0, end - max(begin, previous_end if previous_end is not None else begin))
        previous_end = max(end, previous_end if previous_end is not None else end)
    return count


def analyze(folder, receipt):
    folder = Path(folder)
    output = writebacks(folder, receipt)
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    assert proof['replay_state_binding']['entries'] == 120
    assert proof['replay_state_binding']['next_register_commit_asserted']
    assert proof['replay_state_binding']['eligible_definition_asserted']
    probe = json.loads((folder / 'probe-receipt.json').read_text())
    assert 'load-replay-state.csv' in probe['files']
    with (folder / 'load-replay-state.csv').open() as stream:
        rows = [{name: int(value) for name, value in row.items()} for row in csv.DictReader(stream)]
    with (folder / 'load-writeback-probe.csv').open() as stream:
        joint = [{name: int(value) for name, value in row.items()} for row in csv.DictReader(stream)]
    assert [(r['core_cycle'], r['clean'], r['native_dump']) for r in rows if r['kind'] == 0] == [
        (r['core_cycle'], r['clean'], r['native_dump']) for r in joint if r['kind'] == 0]
    attempts, completed = match_attempts(joint)
    end = max(phase['end_core_inclusive'] for phase in output['phases']) + 1
    spans = segments(rows, end)
    by_key = defaultdict(list)
    for begin, finish, row in spans:
        if row['is_vector']:
            continue
        key = tuple(row[name] for name in KEY)
        assert key in attempts, 'LRQ scalar issue identity absent from fired S2 trace'
        assert all(a['pc'] == row['pc'] for a in attempts[key]), 'LRQ/S2 PC mismatch'
        assert attempts[key][0]['core_cycle'] <= begin, 'LRQ precedes initial S2'
        by_key[key].append((begin, finish, row))
    blocks = read_blocks(folder / 'simulator_err.txt')
    for phase in output['phases']:
        lo, hi = phase['begin_core_exclusive'] + 1, phase['end_core_inclusive'] + 1
        selected = [(max(lo, begin), min(hi, finish), row) for begin, finish, row in spans
                    if begin < hi and finish > lo]
        total = Counter()
        for begin, finish, row in selected:
            total[state(row)] += finish - begin
        measured = union_length([(begin, finish) for begin, finish, _ in selected])
        native, = [value for name, value in blocks[phase['native_dump']].items()
                   if name.endswith('.loadQueueReplay.replay_allocate')]
        assert native == [measured], (native, measured)
        phase['LRQ_state_entry_cycles'] = dict(total)
        phase['LRQ_any_allocated_native_cycles'] = measured
        profiles, services = {}, []
        for key, wb in completed.items():
            if not lo <= wb['core_cycle'] < hi:
                continue
            first = attempts[key][0]['core_cycle']
            counts = Counter()
            entries = set()
            for begin, finish, row in by_key.get(key, []):
                duration = min(wb['core_cycle'], finish) - max(first, begin)
                if duration > 0:
                    counts[state(row)] += duration
                    entries.add(row['entry'])
            profile = profiles.setdefault(hex(wb['pc']), dict(successful_loads=0, with_LRQ=0,
                                                              LRQ_entry_cycles={name: 0 for name in STATES}))
            profile['successful_loads'] += 1
            profile['with_LRQ'] += bool(counts)
            for name, count in counts.items():
                profile['LRQ_entry_cycles'][name] += count
            services.append(dict(identity=dict(zip(KEY, key)), pc=hex(wb['pc']), first_S2=first,
                                 successful_WB=wb['core_cycle'], service_cycles=wb['core_cycle'] - first,
                                 LRQ_entry_cycles={name: counts[name] for name in STATES},
                                 LRQ_entries=sorted(entries)))
        phase['completed_scalar_PC_LRQ'] = profiles
        phase['longest_completed_scalar_services'] = sorted(services, key=lambda row: row['service_cycles'],
                                                          reverse=True)[:20]
    output.update(LRQ_state_change_rows=sum(row['kind'] == 1 for row in rows), LRQ_scalar_keys=len(by_key),
                  scope=output['scope'] + ' LRQ state spans use actual allocated/scheduled/eligible arrays, runtime next-register commitments and eligibility assertions. Any-allocated cycles reproduce native replay_allocate. Waiting means not scheduled and not S0-eligible; ready includes bank/arbitration waits and selection. Entry-cycles include release/cancel cycles and may overlap for members; they are not exact CPU loss or first data arrival. MSHR generation and return identity are not captured.')
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt), indent=2) + '\n')


if __name__ == '__main__':
    main()
