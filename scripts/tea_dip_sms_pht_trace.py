"""Reconstruct physical PHT queue generations, overwrite fates and joint service blockers."""
import argparse
from collections import Counter, defaultdict
from itertools import groupby
import json
from pathlib import Path
import sqlite3

import dip_analyze as dip

from native_perf import read_blocks
from tea_dip_l2_return_trace import read
from tea_dip_load_writeback_trace import hist_stats
from tea_dip_sms_observer import analyze as targeted

PAYLOAD = ['index', 'tag', 'offset', 'region']
PIPE = ['s1_valid', 's1_update', 's1_index', 's2_valid', 's2_update', 's2_index', 's3_valid', 's3_update', 'raw_hazard', 'write_busy']


def queue_owners(rows, policy, read_policy='hold_empty'):
    assert policy in ['update_priority', 'existing_S1_type_alternation']
    assert read_policy in ['hold_empty', 'advance_empty', 'advance_empty_ready', 'advance_nearest_ready', 'lookup_nearest_ready']
    if read_policy in ['advance_empty_ready', 'advance_nearest_ready', 'lookup_nearest_ready']:
        assert policy == 'update_priority'
    slots, reads, writes, sequence = [[None] * 4 for _ in range(2)], [0, 0], [0, 0], [0, 0]
    owners, observations = [], []
    previous = -1
    for cycle, group in groupby(rows, lambda r: r['core_cycle']):
        assert cycle > previous, 'PHT cycles are unordered'
        previous = cycle
        group = list(group)
        assert all(r['kind'] in [0, 1] and r['queue'] in [0, 1] for r in group)
        assert all([r[k] for k in PIPE] == [group[0][k] for k in PIPE] for r in group), 'PHT pipeline snapshot mismatch'
        pipe = group[0]
        raw = int(bool(pipe['s2_valid'] and pipe['s2_update'] and pipe['s2_index'] == pipe['s1_index']))
        busy = int(bool(pipe['s3_valid'] and pipe['s3_update']))
        assert pipe['raw_hazard'] == raw and pipe['write_busy'] == busy, 'PHT hazard equation mismatch'
        ready = not pipe['s1_valid'] or not (raw or busy)
        valid = [slots[q][reads[q]] is not None for q in range(2)]
        selected_update = valid[1] and (policy == 'update_priority' or not valid[0] or not pipe['s1_valid'] or not pipe['s1_update'])
        expected_outputs = [int(valid[0] and ready and not selected_update), int(valid[1] and ready and selected_update)]
        active = {}
        for row in group:
            q = row['queue']
            mask = sum(1 << i for i, owner in enumerate(slots[q]) if owner is not None)
            assert row['valid_mask'] == mask and row['read_slot'] == reads[q] and row['write_slot'] == writes[q], 'PHT physical queue transition mismatch'
            assert row['input'] in [0, 1] and row['output'] == expected_outputs[q], 'PHT actual service differs from qualified arbitration'
            head = slots[q][reads[q]]
            expected_head = head['payload'] if head else dict.fromkeys(PAYLOAD, 0)
            assert {k: row['head_' + k] for k in PAYLOAD} == expected_head, 'PHT head payload has wrong generation'
            if not row['input']:
                assert all(row['input_' + k] == 0 for k in PAYLOAD), 'Payload without queue input'
            if row['kind'] == 0:
                assert row['clean'] or row['native_dump'], 'PHT boundary without perf control'
            else:
                assert q not in active, 'Duplicate PHT queue cycle'
                active[q] = row
        for q in range(2):
            row = active.get(q)
            if row is None:
                assert not any(owner is not None for owner in slots[q]) and not expected_outputs[q] and not pipe['s1_valid'], 'Missing active PHT queue row'
                continue
            head = slots[q][reads[q]]
            blocker = 0
            if head and not row['output']:
                if not ready:
                    blocker = int(raw) | (int(busy) << 1)
                else:
                    blocker = 4
                assert blocker
            before = sum(owner is not None for owner in slots[q])
            queue_policy = ('advance_nearest_ready' if q == 0 else 'hold_empty') if read_policy == 'lookup_nearest_ready' else read_policy
            advance_empty = queue_policy in ['advance_empty', 'advance_empty_ready', 'advance_nearest_ready'] and before > 0 and head is None
            if queue_policy in ['advance_empty_ready', 'advance_nearest_ready']:
                advance_empty = advance_empty and ready and (q == 1 or not valid[1])
            departed, overwritten = None, None
            if row['output']:
                assert head is not None
                departed = head['id']
                head.update(fate='served', end_cycle=cycle, residence_cycles=cycle - head['input_cycle'])
                slots[q][reads[q]] = None
                reads[q] = (reads[q] + 1) & 3
            elif advance_empty:
                step = next(step for step in range(1, 4) if slots[q][(reads[q] + step) & 3] is not None) if queue_policy == 'advance_nearest_ready' else 1
                reads[q] = (reads[q] + step) & 3
            if row['input']:
                slot = writes[q]
                prior = slots[q][slot]
                if prior is not None:
                    overwritten = prior['id']
                    prior.update(fate='overwritten', end_cycle=cycle, residence_cycles=cycle - prior['input_cycle'])
                sequence[q] += 1
                owner = dict(id=str(q) + ':' + str(sequence[q]), queue=q, generation=sequence[q], slot=slot, input_cycle=cycle,
                             payload={k: row['input_' + k] for k in PAYLOAD}, fate='pending_at_capture_end')
                assert 0 <= owner['payload']['index'] < 32 and 0 <= owner['payload']['tag'] < 8192 and 0 <= owner['payload']['offset'] < 16
                assert 0 <= owner['payload']['region'] < 1 << 40 and (q == 0 or owner['payload']['region'] == 0)
                slots[q][slot] = owner
                owners.append(owner)
                writes[q] = (writes[q] + 1) & 3
            after = sum(owner is not None for owner in slots[q])
            assert row['input'] - row['output'] == int(overwritten is not None) + after - before
            observations.append(dict(core_cycle=cycle, queue=q, input=row['input'], output=row['output'], overwritten=overwritten,
                                     served=departed, occupancy_before=before, occupancy_after=after, blocked_reason_mask=blocker,
                                     occupied_with_invalid_head=bool(before and head is None), empty_head_advance=bool(advance_empty)))
    for q in range(2):
        members = [o for o in owners if o['queue'] == q]
        counts = Counter(o['fate'] for o in members)
        assert len(members) == counts['served'] + counts['overwritten'] + counts['pending_at_capture_end']
        assert counts['pending_at_capture_end'] == sum(o is not None for o in slots[q])
    return owners, observations


def analyze(folder, receipt):
    folder = Path(folder)
    output = targeted(folder, receipt)
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    binding = proof['SMS_PHT_binding']
    assert binding['queue_slots'] == 4 and binding['current_NEXT_and_queue_equations_asserted'] and binding['service_selection_and_hazards_asserted'] and binding['new_RTL_state_bits'] == 0
    rows = read(folder / 'sms-pht-queues.csv')
    expected = [(r['core_cycle'], r['clean'], r['native_dump']) for r in read(folder / 'load-service-probe.csv') if r['lane'] == 3]
    for q in range(2):
        assert [(r['core_cycle'], r['clean'], r['native_dump']) for r in rows if r['kind'] == 0 and r['queue'] == q] == expected, 'PHT boundary differs from native/S2'
    # Existing abf captures used the original hold-on-empty read-pointer policy.
    read_policy = binding.get('queue_read_policy', 'hold_empty')
    owners, observations = queue_owners(rows, binding['policy'], read_policy)
    native = read_blocks(folder / 'simulator_err.txt')
    for phase in output['phases']:
        begin, end = phase['begin_core_exclusive'], phase['end_core_inclusive']
        result = {}
        for q, name in enumerate(['lookup', 'evict']):
            selected = [r for r in observations if r['queue'] == q and begin < r['core_cycle'] <= end]
            members = [o for o in owners if o['queue'] == q]
            inserted = [o for o in members if begin < o['input_cycle'] <= end]
            exited = [o for o in members if 'end_cycle' in o and begin < o['end_cycle'] <= end]
            served = [o for o in exited if o['fate'] == 'served']
            lost = [o for o in exited if o['fate'] == 'overwritten']
            resident = lambda t: [o['id'] for o in members if o['input_cycle'] <= t and ('end_cycle' not in o or o['end_cycle'] > t)]
            start, finish = resident(begin), resident(end)
            for event, count in [('in', len(inserted)), ('out', len(served))]:
                values, = [v for k, v in native[phase['native_dump']].items() if k.endswith('.sms_pht_' + name + '_' + event)]
                assert values == [count], 'PHT native marginal differs from queue generations'
            assert sum(r['input'] for r in selected) == len(inserted) and sum(r['output'] for r in selected) == len(served)
            assert sum(r['overwritten'] is not None for r in selected) == len(lost)
            assert len(inserted) - len(served) == len(lost) + len(finish) - len(start), 'PHT ROI conservation failed'
            masks = Counter(r['blocked_reason_mask'] for r in selected if r['blocked_reason_mask'])
            hist = Counter(o['residence_cycles'] for o in served)
            cohorts = Counter('pending_at_capture_end' if 'end_cycle' not in o else o['fate'] + ('_inside_window' if o['end_cycle'] <= end else '_after_window') for o in inserted)
            assert sum(cohorts.values()) == len(inserted)
            result[name] = dict(inputs=len(inserted), outputs=len(served), exact_overwrites=len(lost), occupancy_before=len(start), occupancy_after=len(finish),
                                start_owner_ids=start, finish_owner_ids=finish, overwritten_owner_ids=[o['id'] for o in lost],
                                jointly_partitioned_head_block_cycles={str(k): v for k, v in sorted(masks.items())},
                                occupied_with_invalid_head_cycles=sum(r['occupied_with_invalid_head'] for r in selected),
                                empty_head_pointer_advance_cycles=sum(r['empty_head_advance'] for r in selected),
                                served_request_queue_residence=hist_stats(hist) if hist else dict(count=0, histogram={}),
                                served_owners_inserted_at_or_before_window=sum(o['input_cycle'] <= begin for o in served), input_cohort_fates=dict(cohorts),
                                conservation_passed=True)
        phase['PHT_queues'] = result
        phase['PHT_native_marginals'] = 4
        exposure = defaultdict(int)
        for row in observations:
            if begin < row['core_cycle'] <= end:
                exposure[row['core_cycle']] |= row['blocked_reason_mask'] << (3 * row['queue'])
                exposure[row['core_cycle']] |= int(row['occupied_with_invalid_head']) << (6 + row['queue'])
        values, = [v for k, v in native[phase['native_dump']].items() if k.endswith('.rob.clock_cycle')]
        retired, = [v for k, v in native[phase['native_dump']].items() if k.endswith('.rob.commitInstr')]
        cpi = values[0] / retired[0]
        with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
            db.row_factory = sqlite3.Row
            samples = list(db.execute('SELECT*FROM Dip_0 WHERE CYCLE>? AND CYCLE<=? ORDER BY CYCLE,ID', (begin, end)))
            tea = {r['CYCLE']: r['STATE'] for r in db.execute('SELECT CYCLE,STATE FROM Tea_0 WHERE CYCLE>? AND CYCLE<=?', (begin, end))}
            assert len(samples) == len(tea) and {r['CYCLE'] for r in samples} == set(tea), 'PHT joint TEA/DIP sample populations differ'
            width = sum(r['name'].startswith('SLOTS_') and r['name'].endswith('_STATE') for r in db.execute('PRAGMA table_info(Dip_0)'))
            groups = defaultdict(list)
            for sample in samples:
                groups[exposure[sample['CYCLE']]].append(sample)
            finals = 'SELECT*FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'
            _, full = dip.analyze(iter(samples), db.execute(finals), width)
            partitions = {}
            for mask, group in sorted(groups.items()):
                _, summary = dip.analyze(iter(group), db.execute(finals), width)
                states = Counter(tea[r['CYCLE']] for r in group)
                partitions[str(mask)] = dict(sampled_cycles=len(group), TEA_state_counts={str(k): v for k, v in sorted(states.items())},
                                             DIP_state_cycles=summary['state_cycles'], DIP_unresolved_slots=summary['unresolved_slots'],
                                             TEA_CPI_contribution={str(k): v / len(samples) * cpi for k, v in sorted(states.items())},
                                             DIP_CPI_contribution={k: v / len(samples) * cpi for k, v in summary['state_cycles'].items()},
                                             DIP_Unresolved_CPI_contribution=summary['unresolved_slots'] / (len(samples) * width) * cpi)
            assert sum(p['sampled_cycles'] for p in partitions.values()) == full['sampled_cycles']
            assert sum(p['DIP_unresolved_slots'] for p in partitions.values()) == full['unresolved_slots']
            for state in dip.STATES:
                assert sum(p['DIP_state_cycles'][state] for p in partitions.values()) == full['state_cycles'][state]
            assert abs(sum(sum(p['TEA_CPI_contribution'].values()) for p in partitions.values()) - cpi) < 1e-9
            assert abs(sum(sum(p['DIP_CPI_contribution'].values()) + p['DIP_Unresolved_CPI_contribution'] for p in partitions.values()) - cpi) < 1e-9
        phase['PHT_sampled_joint'] = dict(native_CPI=cpi, sampled_cycles=len(samples), partitions=partitions,
                                         mask_semantics='Lookup blockers bits0..2,update blockers bits3..5,occupied-invalid-head bits6/7; zero includes idle or unblocked service. Same-source-cycle association only; full later DIP finals are replayed independently for every group. TEA and DIP budgets each sum to own CPI and must not be added.')
    output.update(PHT_policy=binding['policy'], PHT_queue_read_policy=read_policy, PHT_queue_owners=owners,
                  PHT_scope='Every physical slot write starts a distinct host generation. Same-cycle departures precede writes; overwritten owners and capture-end pending owners remain distinct. Joint blocker bits:1=S2 same-index RAW,2=S3 RAM write busy,4=other queue selected with pipeline ready. Module cycles and request residence are not CPU lost cycles; served events do not prove useful prefetches or unique training/load causality.')
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
