"""Bind scalar DM waits to qualified LRQ tags and actual L1 MSHR generations.

Data grant boundaries describe observed service, not retirement or exclusive CPU
loss. A released slot is never rebound during an uninterrupted waiting episode.
"""
import argparse
from bisect import bisect_right
from collections import Counter, defaultdict
import csv
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_gsim_mshr_return_probe import MSHR_FIELDS, TAG_FIELDS
from tea_dip_gsim_replay_state_probe import FIELDS
from tea_dip_load_writeback_trace import KEY, match_attempts
from tea_dip_replay_state_trace import analyze as replay, segments as replay_segments

PARTS = ['before_recorded_acquire', 'acquired_before_first_data',
         'between_first_and_last_data', 'at_or_after_last_data', 'unknown']


def coalesce(spans, group, fields):
    """Remove forced boundary snapshots without creating a new owner binding."""
    grouped = defaultdict(list)
    for begin, end, row in spans:
        grouped[row[group]].append((begin, end, row))
    result = []
    for records in grouped.values():
        current = None
        for begin, end, row in sorted(records, key=lambda item: item[0]):
            if current and current[1] == begin and all(current[2][k] == row[k] for k in fields):
                current = current[0], end, current[2]
            else:
                if current:
                    result.append(current)
                current = begin, end, row
        if current:
            result.append(current)
    return result


def generations(rows, end):
    """Reconstruct generations from actual valid edges, preserving inactive flags."""
    current, owners, epochs, spans = {}, defaultdict(list), Counter(), []
    active, grants, previous_cycle = {}, [], -1
    for row in rows:
        cycle, kind, slot = row['core_cycle'], row['kind'], row['mshr']
        assert cycle >= previous_cycle
        previous_cycle = cycle
        if kind == 0:
            continue
        assert kind in [1, 2] and 0 <= slot < 16
        if kind == 2:
            assert slot in active, 'Grant without an active MSHR generation'
            owner = active[slot]
            state = current[slot][1]
            assert all(row[k] == state[k] for k in MSHR_FIELDS), 'Grant/state payload mismatch'
            assert state['valid'] and state['acquired'] and not state['last']
            assert row['grant_has_data'] in [0, 1] and row['grant_complete'] in [0, 1]
            assert all(g['core_cycle'] != cycle for g in owner['grants']), 'Duplicate grant'
            owner['grants'].append(row)
            grants.append(row)
            continue
        assert row['valid'] in [0, 1]
        assert all(row[k] in [0, 1] for k in ['acquired', 'first', 'last', 'hint', 'has_data'])
        assert not row['grant_has_data'] and not row['grant_complete']
        previous = current.get(slot)
        if previous:
            begin, state = previous
            assert begin < cycle, 'Duplicate MSHR state cycle'
            spans.append((begin, min(cycle, end), state))
        else:
            state = None
        if row['valid']:
            assert row['line'] & 63 == 0
            if not state or not state['valid']:
                epochs[slot] += 1
                owner = dict(slot=slot, generation=epochs[slot], begin=cycle, end=end,
                             line=row['line'], primary_source=row['primary_source'],
                             allocation_observed=state is not None, grants=[], states=[])
                if state is not None:
                    assert not row['first'] and not row['last'], 'New generation already returned'
                active[slot] = owner
                owners[slot].append(owner)
            else:
                owner = active[slot]
                assert row['line'] == owner['line'], 'MSHR line changed within a generation'
                for flag in ['first', 'last']:
                    if row[flag] and not state[flag]:
                        assert any(g['core_cycle'] == cycle - 1 and (flag == 'first' or g['grant_complete'])
                                   for g in owner['grants']), 'Return flag has no preceding grant'
            active[slot]['states'].append(row)
        elif slot in active:
            active.pop(slot)['end'] = cycle
        current[slot] = cycle, row
    assert set(current) == set(range(16)), 'Missing MSHR slot snapshots'
    for begin, row in current.values():
        spans.append((begin, end, row))
    spans = [(a, b, r) for a, b, r in spans if a < b]
    by_slot = defaultdict(list)
    for begin, finish, row in spans:
        by_slot[row['mshr']].append((begin, row))
    for grant in grants:
        cycle = grant['core_cycle'] + 1
        if cycle >= end:
            continue
        records = by_slot[grant['mshr']]
        index = bisect_right([a for a, _ in records], cycle) - 1
        assert index >= 0
        following = records[index][1]
        assert following['first'] and following['has_data'] == grant['grant_has_data'], 'Grant data/next-state mismatch'
        assert following['last'] == grant['grant_complete'], 'Grant completion/next-state mismatch'
    for records in owners.values():
        for owner in records:
            data = [g['core_cycle'] for g in owner['grants'] if g['grant_has_data']]
            last = [g['core_cycle'] for g in owner['grants'] if g['grant_has_data'] and g['grant_complete']]
            assert len(last) <= 1
            owner['first_data'] = min(data) if data else None
            owner['last_data'] = last[0] if last else None
            owner['acquire_recorded'] = next((r['core_cycle'] for r in owner['states'] if r['acquired']), None)
            owner['hint_recorded'] = next((r['core_cycle'] for r in owner['states'] if r['hint']), None)
    return dict(owners), spans, grants


def tag_segments(rows, end):
    current, spans, previous_cycle = {}, [], -1
    for row in rows:
        cycle = row['core_cycle']
        assert cycle >= previous_cycle
        previous_cycle = cycle
        if row['kind'] == 0:
            continue
        assert row['kind'] == 1 and 0 <= row['entry'] < 120
        entry = row['entry']
        if entry in current:
            begin, old = current[entry]
            assert begin < cycle, 'Duplicate LRQ tag cycle'
            if old['valid']:
                spans.append((begin, min(cycle, end), old))
        assert row['valid'] in [0, 1]
        if row['valid']:
            assert row['handled'] in [0, 1] and 0 <= row['mshr_id'] < 32
            assert not row['handled'] or row['mshr_id'] < 16
            assert 0 < row['issue_cycle'] <= cycle
        else:
            assert all(row[k] == 0 for k in TAG_FIELDS)
        current[entry] = cycle, row
    for begin, row in current.values():
        if row['valid']:
            spans.append((begin, end, row))
    return coalesce([(a, b, r) for a, b, r in spans if a < b], 'entry', TAG_FIELDS)


def partition_wait(begin, end, tag, tag_end, owners, line):
    """Bind once at episode start; retain missing/reused-owner tails as unknown."""
    counts = Counter({name: 0 for name in PARTS})
    binding = dict(begin=begin, end=end, unknown_reason=None)
    if tag is None or not tag['handled']:
        counts['unknown'] = end - begin
        binding['unknown_reason'] = 'no_qualified_tag'
        return dict(counts), binding
    records = owners.get(tag['mshr_id'], [])
    index = bisect_right([r['begin'] for r in records], begin) - 1
    owner = records[index] if index >= 0 and begin < records[index]['end'] else None
    if owner is None or not owner['allocation_observed'] or owner['line'] != line:
        counts['unknown'] = end - begin
        binding['unknown_reason'] = 'no_matching_current_generation'
        return dict(counts), binding
    binding.update(mshr=owner['slot'], generation=owner['generation'], line=hex(line),
                   owner_begin=owner['begin'], owner_end=owner['end'],
                   acquire_recorded=owner['acquire_recorded'], first_data=owner['first_data'],
                   last_data=owner['last_data'], hint_recorded=owner['hint_recorded'])
    stop = min(end, tag_end, owner['end'])
    first, last, acquired = owner['first_data'], owner['last_data'], owner['acquire_recorded']
    if first is None or last is None:
        counts['unknown'] = end - begin
        binding['unknown_reason'] = 'no_complete_observed_data_return'
        return dict(counts), binding
    assert acquired is not None and acquired <= first <= last < owner['end']
    cuts = sorted({begin, stop, *[x for x in [acquired, first, last] if begin < x < stop]})
    for a, b in zip(cuts, cuts[1:]):
        part = ('before_recorded_acquire' if a < acquired else 'acquired_before_first_data' if a < first
                else 'between_first_and_last_data' if a < last else 'at_or_after_last_data')
        counts[part] += b - a
    counts['unknown'] += end - stop
    if stop < end:
        binding['unknown_reason'] = 'owner_or_tag_ended_during_continuous_wait'
    assert sum(counts.values()) == end - begin
    return dict(counts), binding


def read_rows(path):
    with path.open() as stream:
        return [{k: int(v) for k, v in row.items()} for row in csv.DictReader(stream)]


def analyze(folder, receipt):
    folder = Path(folder)
    output = replay(folder, receipt)
    proof = json.loads((folder / 'probe-build-proof.json').read_text())['MSHR_return_binding']
    assert proof['entries'] == 16 and proof['LRQ_entries'] == 120
    assert proof['next_register_commit_asserted'] and proof['grant_qualification_asserted']
    assert proof['LRQ_handled_provenance_asserted']
    probe = json.loads((folder / 'probe-receipt.json').read_text())
    assert all(name in probe['files'] for name in ['load-mshr-state.csv', 'load-lrq-mshr-tags.csv'])
    rows, tags, joint, lrq = [read_rows(folder / name) for name in [
        'load-mshr-state.csv', 'load-lrq-mshr-tags.csv', 'load-writeback-probe.csv', 'load-replay-state.csv']]
    boundaries = lambda records: [(r['core_cycle'], r['clean'], r['native_dump']) for r in records if r['kind'] == 0]
    assert boundaries(rows) == boundaries(tags) == boundaries(joint), 'New trace boundary mismatch'
    end = max(p['end_core_inclusive'] for p in output['phases']) + 1
    owners, states, grants = generations(rows, end)
    tag_spans = tag_segments(tags, end)
    lrq_spans = coalesce(replay_segments(lrq, end), 'entry', FIELDS)
    by_entry = defaultdict(list)
    for begin, finish, row in tag_spans:
        by_entry[row['entry']].append((begin, finish, row))
    # Prove both traces cover the same DM members, beyond merely matching hashes.
    coverage = Counter()
    for begin, finish, row in lrq_spans:
        if row['is_vector'] or row['cause'] != 64:
            continue
        matched = 0
        for a, b, tag in by_entry[row['entry']]:
            overlap = min(finish, b) - max(begin, a)
            if overlap <= 0:
                continue
            assert all(tag[k] == row[k] for k in [*KEY, 'pc']), 'LRQ tag member mismatch'
            matched += overlap
            coverage[row['entry']] += overlap
        assert matched == finish - begin, 'Missing LRQ DM tag coverage'
    assert sum(coverage.values()) == sum(b - a for a, b, _ in tag_spans), 'Extra LRQ DM tags'
    attempts, completed = match_attempts(joint)
    waits = defaultdict(list)
    for begin, finish, row in lrq_spans:
        if not row['is_vector'] and row['cause'] == 64 and not row['scheduled'] and not row['eligible']:
            assert row['blocking']
            waits[tuple(row[k] for k in KEY)].append((begin, finish, row))
    blocks = read_blocks(folder / 'simulator_err.txt')
    for phase in output['phases']:
        lo, hi = phase['begin_core_exclusive'] + 1, phase['end_core_inclusive'] + 1
        native_marginals = []
        for slot in range(16):
            count = sum(max(0, min(hi, b) - max(lo, a)) for a, b, r in states
                        if r['mshr'] == slot and r['acquired'] and not r['last'])
            count -= sum(lo <= r['core_cycle'] < hi for r in grants if r['mshr'] == slot)
            native, = [v for k, v in blocks[phase['native_dump']].items()
                       if k.endswith(f'.missQueue.entries_{slot}.penalty_waiting_for_channel_D')]
            assert native == [count], ('MSHR native D-wait mismatch', slot, native, count)
            native_marginals.append(dict(mshr=slot, waiting_for_D=count))
        profiles, services = {}, []
        for key, wb in completed.items():
            if not lo <= wb['core_cycle'] < hi:
                continue
            first = attempts[key][0]['core_cycle']
            counts, bindings = Counter({name: 0 for name in PARTS}), []
            for a, b, row in waits.get(key, []):
                begin, finish = max(first, a), min(wb['core_cycle'], b)
                if begin >= finish:
                    continue
                # Bind at the original episode start, before clipping to WB/ROI.
                tag, tag_end = None, finish
                for x, y, candidate in by_entry[row['entry']]:
                    if x <= a < y:
                        tag, tag_end = candidate, y
                        break
                result, binding = partition_wait(a, finish, tag, tag_end, owners, wb['physical_address'] & ~63)
                assert begin == a
                counts.update(result)
                bindings.append(binding)
            profile = profiles.setdefault(hex(wb['pc']), dict(successful_loads=0, with_DM_wait=0,
                                                              DM_wait_entry_cycles={name: 0 for name in PARTS}))
            profile['successful_loads'] += 1
            profile['with_DM_wait'] += bool(bindings)
            for name, count in counts.items():
                profile['DM_wait_entry_cycles'][name] += count
            services.append(dict(identity=dict(zip(KEY, key)), pc=hex(wb['pc']), first_S2=first,
                                 successful_WB=wb['core_cycle'], service_cycles=wb['core_cycle']-first,
                                 DM_wait_entry_cycles=dict(counts), DM_wait_episodes=bindings))
        phase['MSHR_native_D_wait'] = native_marginals
        phase['completed_scalar_PC_MSHR_wait'] = profiles
        phase['longest_completed_scalar_MSHR_services'] = sorted(services, key=lambda r: r['service_cycles'], reverse=True)[:20]
    output.update(MSHR_generation_count=sum(map(len, owners.values())), MSHR_grant_events=len(grants),
                  MSHR_state_change_rows=sum(r['kind'] == 1 for r in rows),
                  LRQ_MSHR_tag_change_rows=sum(r['kind'] == 1 for r in tags),
                  scope=output['scope'] + ' L1 MSHR generations follow valid edges; qualified LRQ tags retain actual enqueue handledByMSHR provenance. Each uninterrupted scalar DM waiting episode binds once to its current slot generation and matching WB physical block. Missing/incomplete returns and released-owner tails remain unknown, without rebinding reused slots. Complete-return waits are partitioned at recorded acquire, first-data and last-data cycles; at/after-last includes the response cycle and is not proven wakeup overhead. Cohorts use successful WB, may start before clean, overlap, and are not retirement or exclusive CPU loss. Per-slot native D-wait is independently preserved.')
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
