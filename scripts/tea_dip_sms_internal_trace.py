"""Join actual SMS training/generation/filter events, preserving suppressed payloads and tails."""
import argparse
from bisect import bisect_right
from collections import Counter
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_l2_return_trace import read
from tea_dip_sms_request_trace import analyze as requests

FILTER_PAYLOAD = ['allocated', 'region_tag', 'region_address', 'region_bits',
                  'filter_bits', 'physical_valid', 'decreasing']


def filter_owners(rows, sms_rows):
    """Use observed allocation edges and chosen-arbiter departures, never address-only ownership."""
    active, counts, owners, departures = {}, Counter(), [], []
    sms = {row['core_cycle']: row for row in sms_rows if row['kind'] == 1}
    assert len(sms) == sum(row['kind'] == 1 for row in sms_rows)
    previous = -1
    for row in rows:
        cycle, slot, kind = row['core_cycle'], row['slot'], row['kind']
        assert cycle >= previous, 'Filter cycle moved backwards'
        previous = cycle
        if kind == 0:
            continue
        assert kind in [1, 2] and 0 <= slot < 16
        if kind == 1:
            if row['new_owner']:
                assert row['allocated'], 'Allocation has no valid filter owner'
                if slot in active:
                    active.pop(slot)['end'] = cycle
                counts[slot] += 1
                owner = dict(slot=slot, generation=counts[slot], begin=cycle, end=None, states=[])
                owners.append(owner)
                active[slot] = owner
            if not row['allocated']:
                assert all(row[name] == 0 for name in FILTER_PAYLOAD)
                if slot in active:
                    active.pop(slot)['end'] = cycle
            else:
                assert slot in active, 'Filter state has no observed allocation edge'
                owner = active[slot]
                if owner['states']:
                    assert cycle > owner['states'][-1]['core_cycle'], 'Duplicate filter state cycle'
                owner['states'].append(row)
        else:
            assert not row['new_owner'] and slot in active, 'Departure has no active filter owner'
            owner = active[slot]
            assert all(row[name] == owner['states'][-1][name] for name in FILTER_PAYLOAD), 'Departure state differs from its own slot'
            pending = row['region_bits'] & ~row['filter_bits'] & 65535
            assert row['physical_valid'] and pending, 'Departure is not a pending physical prediction'
            offset = pending.bit_length() - 1 if row['decreasing'] else (pending & -pending).bit_length() - 1
            address = row['region_address'] << 10 | offset << 6
            assert cycle in sms and sms[cycle]['address'] == address, 'Chosen filter owner/SMS address mismatch'
            departures.append(dict(core_cycle=cycle, address=address, slot=slot, generation=owner['generation']))
    assert len(departures) == len(sms), 'Missing chosen-owner SMS departure'
    return owners, departures


def generation_commits(generated, owners, capture_end):
    complete, pending = [], []
    for request in generated:
        if request['kind'] == 0:
            continue
        assert request['kind'] in [1, 2]
        if request['kind'] == 2:
            assert all(request[k] == 0 for k in ['slot', 'hit', 'region_tag', 'region_address', 'region_bits', 'physical_valid', 'decreasing'])
            continue  # Explicit consecutive-tag suppression; incoming payload was not captured.
        at = request['core_cycle'] + 2
        if at > capture_end:
            pending.append(request)
            continue
        matching = [o for o in owners if o['slot'] == request['slot'] and o['begin'] <= at and (o['end'] is None or at < o['end'])]
        assert len(matching) == 1, 'Accepted generation has no unique same-slot commit'
        owner, = matching
        state = owner['states'][bisect_right([r['core_cycle'] for r in owner['states']], at) - 1]
        assert state['region_tag'] == request['region_tag'], 'Generation/filter tag mismatch'
        assert state['region_bits'] & request['region_bits'] == request['region_bits'], 'Generation bits missing from committed filter state'
        if not request['hit']:
            assert owner['begin'] == at, 'New generation did not allocate at the observed pipeline boundary'
            assert all(state[k] == request[k] for k in ['region_address', 'physical_valid', 'decreasing']), 'Allocation payload mismatch'
        complete.append(dict(core_cycle=request['core_cycle'], slot=owner['slot'], generation=owner['generation'], hit=request['hit']))
    return complete, pending


def analyze(folder, receipt):
    folder = Path(folder)
    output = requests(folder, receipt)
    binding = json.loads((folder / 'probe-build-proof.json').read_text())['SMS_internal_binding']
    assert binding['training_entries'] == 8 and binding['filter_entries'] == 16 and binding['new_RTL_state_bits'] == 0
    assert binding['current_NEXT_commits_asserted'] and binding['actual_filter_allocation_edges'] and binding['actual_arbiter_departure_owner_asserted']
    training, generated, states = [read(folder / name) for name in ['sms-training.csv', 'sms-generation.csv', 'sms-filter-state.csv']]
    old = read(folder / 'sms-l2-requests.csv')
    boundary = lambda rows: [(r['core_cycle'], r['clean'], r['native_dump']) for r in rows if r['kind'] == 0]
    assert boundary(training) == boundary(generated) == boundary(states) == boundary(old), 'SMS internal phase boundary mismatch'
    owners, departures = filter_owners(states, old)
    commits, pending = generation_commits(generated, owners, max(r['core_cycle'] for r in states))
    blocks = read_blocks(folder / 'simulator_err.txt')
    for phase in output['phases']:
        lo, hi = phase['begin_core_exclusive'] + 1, phase['end_core_inclusive'] + 1
        train = [r for r in training if r['kind'] == 1 and lo <= r['core_cycle'] < hi]
        gen = [r for r in generated if r['kind'] in [1, 2] and lo <= r['core_cycle'] < hi]
        selected = [r for r in gen if r['kind'] == 1]
        native = lambda suffix: next(values for name, values in blocks[phase['native_dump']].items() if name.endswith(suffix))
        assert native('.smsOpt.train_filter.SMS_train_filter_deq') == [len(train)], 'SMS training dequeue marginal mismatch'
        assert native('.smsOpt.pf_filter.sms_pf_filter_recv_req') == [len(gen)], 'SMS raw generation marginal mismatch'
        ways = Counter(r['slot'] for r in selected)
        for slot in range(16):
            assert native(f'.smsOpt.pf_filter.sms_pf_filter_access_way_{slot}') == [ways[slot]], 'SMS accepted-generation way marginal mismatch'
        phase['SMS_internal'] = dict(training_dequeues=len(train), raw_generated=len(gen), accepted_generated=len(selected),
                                     consecutive_tag_suppressed=len(gen) - len(selected),
                                     actual_filter_departures=sum(lo <= r['core_cycle'] < hi for r in departures))
    output.update(SMS_training_events=[r for r in training if r['kind'] == 1],
                  SMS_generation_events=[r for r in generated if r['kind'] in [1, 2]],
                  SMS_filter_owners=owners, SMS_filter_departures=departures,
                  SMS_generation_commits=commits, SMS_generation_pending_at_capture_end=pending,
                  SMS_internal_scope='Training PC is a20-bit tag. Generation rows are actual raw arrivals, with explicit unknown payload for consecutive-tag suppression. Accepted request commits use the two-stage filter update and actual slot allocation edges; chosen arbiter departures reproduce every old SMS fire/address. No training-to-prediction causal identity, retirement equivalence, cache admission/residency or recoverable CPU cycles are inferred.')
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
