"""Reconstruct observed L2 residency from write ports and validate real read snapshots.

Removal sources are observed MainPipe tasks, not exclusive causes or saved cycles.
Unknown initial tags/state, merged requests and unobserved prior removals stay explicit.
"""
import argparse
from bisect import bisect_left
from collections import Counter, defaultdict
import csv
from itertools import groupby
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_gsim_l2_directory_probe import FIELDS
from tea_dip_query_endpoint_evidence import digest, verify


def replay(path, route_path):
    queries, stages = defaultdict(list), {}
    with Path(route_path).open() as stream:
        for raw in csv.DictReader(stream):
            if int(raw['kind']) != 4 or not int(raw['valid']):
                continue
            row = {k: int(v) for k, v in raw.items()}
            key = row['core_cycle'], row['bank']
            assert key not in stages
            stages[key] = row
            if not row['mshr_task'] and row['opcode'] in (6, 7) and not row['dir_hit']:
                queries[row['core_cycle']].append(row)
    shadow, reads = {}, {}
    totals = {bank: Counter() for bank in range(4)}
    boundaries, trailers, events, requests = [], set(), [], []
    removals, removal_cycles = defaultdict(list), defaultdict(list)
    checked = Counter()
    first_cycle = previous = None

    def line(bank, set_index, tag):
        return (tag << 18) | (set_index << 8) | (bank << 6)

    def actor(row):
        return ({k: row[k] for k in ['req_source', 'channel', 'opcode', 'mshr_task', 'merge_a']}
                if row['source_valid'] else None)

    with Path(path).open() as stream:
        rows = ({k: int(v) for k, v in raw.items()} for raw in csv.DictReader(stream))
        for cycle, group in groupby(rows, key=lambda row: row['core_cycle']):
            assert previous is None or cycle > previous, 'Directory cycles are not ordered'
            first_cycle = cycle if first_cycle is None else first_cycle
            previous = cycle
            batch = list(group)
            assert len({(r['kind'], r['bank']) for r in batch}) == len(batch), 'Duplicate directory event'
            responses, updates = {}, {}
            for row in batch:
                kind, bank = row['kind'], row['bank']
                assert 0 <= bank < 4 and 0 <= kind <= 5
                if kind in (0, 5):
                    for name, event in [('read_count', 1), ('tag_count', 2), ('meta_count', 3), ('response_count', 4)]:
                        # Counts are checked after this cycle's event records below.
                        assert row[name] >= totals[bank][event]
                    continue
                assert 0 <= row['set'] < 1024
                totals[bank][kind] += 1
                if kind == 4:
                    responses[bank] = row
                    expected = reads.pop((bank, cycle - 2), None)
                    if expected is None:
                        assert cycle < first_cycle + 2, 'Response without its accepted read'
                        checked['left_censored_responses'] += 1
                    else:
                        set_index, snapshot = expected
                        assert set_index == row['set'], 'Directory response set mismatch'
                        for way, (tag, state) in enumerate(snapshot):
                            if state is None:
                                checked['unknown_initial_state_lanes'] += 1
                                continue
                            assert row['state_' + str(way)] == state, ('Directory state mismatch', cycle, bank, way)
                            checked['state_lanes'] += 1
                            if state and tag is not None:
                                assert row['tag_' + str(way)] == tag, ('Directory tag mismatch', cycle, bank, way)
                                checked['valid_tag_lanes'] += 1
                elif kind in (2, 3):
                    assert row['source_valid'] in (0, 1)
                    if row['source_valid'] and row['channel'] & 1:
                        source = stages.get((cycle, bank))
                        assert source is not None, 'Write task lacks its existing A/S3 record'
                        assert all(row[a] == source[b] for a, b in
                                   [('req_source', 'req_source'), ('opcode', 'opcode'), ('mshr_task', 'mshr_task')]), 'Write source differs from same-cycle A/S3'
                        assert row['set'] == (source['line'] >> 8) & 1023
                        if kind == 2:
                            assert row['tag'] == source['line'] >> 18
                        checked['A_write_sources'] += 1
                    mask = row['way_mask']
                    assert 0 < mask < 256
                    assert kind != 2 or mask.bit_count() == 1, 'Tag write must be one-hot'
                    assert 0 <= row['state'] < 4 and 0 <= row['tag'] < (1 << 30)
                    for way in range(8):
                        if mask & (1 << way):
                            entry = updates.setdefault((bank, row['set'], way), {})
                            assert kind not in entry
                            entry[kind] = row
            for request in queries.pop(cycle, []):
                bank, address = request['bank'], request['line']
                row = responses.get(bank)
                assert row is not None, 'Initial Acquire miss has no directory snapshot'
                set_index, tag = (address >> 8) & 1023, address >> 18
                assert row['set'] == set_index
                assert not any(row['state_' + str(i)] and row['tag_' + str(i)] == tag for i in range(8)), 'Actual miss matches a valid directory way'
                read_cycle = cycle - 2
                index = bisect_left(removal_cycles[address], read_cycle) - 1
                event = removals[address][index] if index >= 0 else None
                requests.append(dict(core_cycle=cycle, accepted_directory_read_cycle=read_cycle,
                                     bank=bank, line=address, source_id=request['source_id'],
                                     req_source=request['req_source'], opcode=request['opcode'],
                                     last_observed_removal_event=event,
                                     status='observed_prior_removal' if event is not None else 'no_observed_prior_removal'))
            for row in batch:
                if row['kind'] == 1:
                    bank, set_index = row['bank'], row['set']
                    reads[bank, cycle] = (set_index, [(shadow.get((bank, set_index, i), {}).get('tag'),
                                                     shadow.get((bank, set_index, i), {}).get('state')) for i in range(8)])
            for key, update in updates.items():
                bank, set_index, way = key
                old = shadow.get(key, {})
                now = dict(old)
                if 2 in update:
                    now['tag'] = update[2]['tag']
                if 3 in update:
                    now.update({k: update[3][k] for k in ['state', 'prefetch', 'prefetch_source', 'accessed']})
                old_valid = old.get('state') not in (None, 0) and old.get('tag') is not None
                new_valid = now.get('state') not in (None, 0) and now.get('tag') is not None
                changed = old_valid and (not new_valid or old['tag'] != now['tag'])
                if changed:
                    cause = update.get(2) if new_valid and 2 in update else update.get(3)
                    event = dict(index=len(events), core_cycle=cycle, bank=bank, set=set_index, way=way,
                                 kind='replace' if new_valid else 'invalidate',
                                 removed_line=line(bank, set_index, old['tag']),
                                 incoming_line=line(bank, set_index, now['tag']) if new_valid else None,
                                 actor=actor(cause), removed_meta={k: old.get(k) for k in ['state', 'prefetch', 'prefetch_source', 'accessed']})
                    events.append(event)
                    address = event['removed_line']
                    removals[address].append(event['index'])
                    removal_cycles[address].append(cycle)
                elif new_valid and (not old_valid or old.get('tag') != now['tag']):
                    cause = update.get(2, update.get(3))
                    events.append(dict(index=len(events), core_cycle=cycle, bank=bank, set=set_index, way=way,
                                       kind='fill' if 2 in update else 'observed_validity_update',
                                       incoming_line=line(bank, set_index, now['tag']), actor=actor(cause),
                                       prior_state_known=old.get('state') is not None))
                shadow[key] = now
            for row in batch:
                if row['kind'] in (0, 5):
                    bank = row['bank']
                    for name, event in [('read_count', 1), ('tag_count', 2), ('meta_count', 3), ('response_count', 4)]:
                        assert row[name] == totals[bank][event], ('Directory observer marginal mismatch', cycle, bank, name)
                    if row['kind'] == 5:
                        trailers.add(bank)
                    else:
                        boundaries.append(row)
    assert trailers == set(range(4)), 'Missing final directory margins'
    assert not queries, 'Acquire misses remain without directory snapshots'
    assert all(cycle >= previous - 1 for _, cycle in reads), 'Unmatched interior directory read'
    counts = Counter(r['kind'] for r in events)
    source_counts = Counter()
    for request in requests:
        index = request['last_observed_removal_event']
        if index is not None:
            source = events[index]['actor']
            source_counts[str(source['req_source']) if source else 'unknown'] += 1
    return dict(passed=True, directory_event_counts={str(bank): dict(v) for bank, v in totals.items()},
                snapshot_checks=dict(checked), boundaries=boundaries, right_censored_reads=len(reads),
                lifecycle_counts=dict(counts), prior_removal_source_counts=dict(source_counts),
                events=events, Acquire_miss_requests=requests,
                scope='All captured tag/meta write ports including reset and invalidations. Earlier accepted reads '
                      'are checked against actual all-way response snapshots. Uninitialized state/tag remains unknown. '
                      'Last observed removal precedes that request read; it is not an exclusive cause, a retired-load '
                      'association, a cross-run identity, or a recoverable CPU budget. Merged-A tasks remain explicit.')


def analyze(folder, receipt_path):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and result['original_process_closed']
    assert result['difftest_passed'] and result['exitcode'] == 0 and all(result['sampler_flags'].values())
    assert receipt['source'] == result['source']
    verify(folder, receipt['files'])
    probe = json.loads((folder / 'probe-receipt.json').read_text())
    assert probe['closure_receipt_sha256'] == digest(receipt_path)
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    assert proof['passed'] and proof['original_files_unchanged']
    assert proof['binary_sha256'] == result['binary_sha256']
    assert proof['L2_directory_binding']['fields'] == FIELDS
    output = replay(folder / 'load-l2-directory.csv', folder / 'load-l2-demand-route.csv')
    frames = read_blocks(folder / 'simulator_err.txt')
    margins = []
    dumps = sorted({row['core_cycle'] for row in output['boundaries'] if row['native_dump']})
    assert len(dumps) == len(frames)
    previous_counts = {bank: 0 for bank in range(4)}
    for (stamp, block), cycle in zip(sorted(frames.items()), dumps):
        rows = [r for r in output['boundaries'] if r['native_dump'] and r['core_cycle'] == cycle]
        assert {r['bank'] for r in rows} == set(range(4))
        for row in rows:
            bank = row['bank']
            values, = [v for name, v in block.items() if name.endswith(f'.l2cache.slices_{bank}.directory.dirRead_cnt')]
            count = row['read_count'] - previous_counts[bank]
            assert values == [count], ('Directory native read mismatch', stamp, bank, count, values)
            previous_counts[bank] = row['read_count']
            margins.append(dict(native_stamp=stamp, bank=bank, directory_reads=count))
    output.update(source=receipt['source'], native_read_marginals=margins,
                  receipt_sha256=digest(receipt_path), probe_sha256=digest(folder / 'load-l2-directory.csv'),
                  tool_sha256=digest(Path(__file__)))
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt), indent=2) + '\n')


if __name__ == '__main__':
    main()
