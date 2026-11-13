"""Associate physical PHT lookup generations with pipeline hits, region buffers and filter arrivals."""
import argparse
from collections import Counter
from itertools import groupby
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_l2_return_trace import read
from tea_dip_sms_pht_trace import analyze as parent, queue_owners

FIELDS = ['s1_valid', 's1_update', 's1_index', 's1_tag',
          's2_valid', 's2_update', 's2_index', 's2_tag', 's2_hits',
          's3_valid', 's3_update', 's3_index', 's3_tag', 's3_hits',
          's3_pattern', 's3_mask', 's3_region', 's4_valid_mask', 's4_next_mask',
          'pht_output', 'pht_enabled', 'filter_input', 'filter_accepted',
          'output_tag', 'output_region', 'output_bits', 'output_physical', 'output_decreasing']
BRANCHES = ['current', 'increment', 'decrement']


def replay(rows, owners, observations, generated):
    owners = {owner['id']: owner for owner in owners}
    services = {}
    for row in observations:
        if row['served'] is not None:
            assert row['core_cycle'] not in services, 'Two PHT queue departures in one cycle'
            services[row['core_cycle']] = owners[row['served']]
    generated = {row['core_cycle']: row for row in generated if row['kind'] in [1, 2]}
    s1 = s2 = s3 = None
    s3_hits = 0
    banks = [None] * 3
    predictions, packets, operations, seen_s2 = [], [], [], set()
    prior = -1
    seen_input_cycles = set()
    for cycle, raw_group in groupby(rows, lambda r: r['core_cycle']):
        group = list(raw_group)
        assert cycle > prior and all(r['kind'] in [0, 1] for r in group), 'Pipeline cycles unordered'
        assert all([r[k] for k in FIELDS] == [group[0][k] for k in FIELDS] for r in group), 'Pipeline boundary snapshot differs'
        assert len(group) == len({r['kind'] for r in group}), 'Duplicate pipeline row'
        row = group[0]
        if prior >= 0 and cycle > prior + 1:
            assert s1 is None and s2 is None and (s3 is None or not s3_hits) and not any(banks), 'Missing active PHT pipeline tail'
            s3 = None
        prior = cycle
        for stage, owner in [(1, s1), (2, s2)]:
            assert bool(row[f's{stage}_valid']) == (owner is not None), 'PHT pipeline owner/valid mismatch'
            if owner:
                assert (row[f's{stage}_update'], row[f's{stage}_index'], row[f's{stage}_tag']) == (
                    owner['queue'], owner['payload']['index'], owner['payload']['tag']), 'PHT pipeline owner payload mismatch'
        assert row['s2_hits'] in [0, 1, 2] and row['s3_hits'] in [0, 1, 2], 'PHT hit is not one-hot'
        if row['s3_valid']:
            assert s3 is not None and row['s3_hits'] == s3_hits, 'S3 has no matching S2 response'
            assert (row['s3_update'], row['s3_index'], row['s3_tag']) == (
                s3['queue'], s3['payload']['index'], s3['payload']['tag']), 'S3 owner payload mismatch'
            if not s3['queue']:
                assert row['s3_region'] == s3['payload']['region'], 'S3 changed lookup physical region'
        else:
            assert s3 is None or not s3_hits, 'A hit response disappeared before S3'
        mask = sum(1 << i for i, value in enumerate(banks) if value is not None)
        assert row['s4_valid_mask'] == mask and bool(row['pht_output']) == bool(mask), 'Prediction buffer ownership mismatch'
        assert not row['pht_enabled'] or row['pht_output']
        assert not row['filter_input'] or row['pht_enabled']
        assert not row['filter_accepted'] or row['filter_input']
        if row['pht_output']:
            branch = (mask & -mask).bit_length() - 1
            origin = banks[branch]
            assert origin is not None
            origin.update(fate='served', end_cycle=cycle)
            banks[branch] = None
            packet = dict(sequence=len(packets) + 1, core_cycle=cycle, lookup_owner_id=origin['lookup_owner_id'],
                          prediction_id=origin['id'], branch=BRANCHES[branch],
                          payload={name: row['output_' + key] for name, key in
                                   [('region_tag', 'tag'), ('region_address', 'region'), ('region_bits', 'bits'),
                                    ('physical_valid', 'physical'), ('decreasing', 'decreasing')]})
            assert packet['payload']['region_bits']
            if row['filter_input']:
                assert cycle in generated and generated[cycle]['pht_valid'], 'PHT/filter arrival mismatch'
                request = generated[cycle]
                assert (request['kind'] == 1) == bool(row['filter_accepted']), 'Filter acceptance differs from old CSV'
                if row['filter_accepted']:
                    assert all(request[k] == v for k, v in packet['payload'].items()), 'PHT output/filter payload mismatch'
                packet['filter_fate'] = 'accepted' if row['filter_accepted'] else 'consecutive_tag_suppressed'
                seen_input_cycles.add(cycle)
            else:
                packet['filter_fate'] = 'feedback_disabled' if row['pht_enabled'] else 'PHT_disabled'
            packets.append(packet)
        else:
            assert all(row['output_' + key] == 0 for key in ['tag', 'region', 'bits', 'physical', 'decreasing'])
        prediction = row['s3_valid'] and not row['s3_update'] and row['s3_hits']
        pattern, update_mask = row['s3_pattern'], row['s3_mask']
        fresh = [bool(prediction and pattern & update_mask),
                 bool(prediction and (pattern >> 15) & ~(update_mask >> 15) & 32767),
                 bool(prediction and pattern & ~update_mask & 32767)]
        for branch, valid in enumerate(fresh):
            if not valid:
                continue
            assert s3 is not None and not s3['queue']
            if banks[branch]:
                banks[branch].update(fate='overwritten', end_cycle=cycle + 1)
            origin = dict(id=len(predictions) + 1, lookup_owner_id=s3['id'], branch=BRANCHES[branch],
                          created_cycle=cycle + 1, fate='pending_at_capture_end')
            predictions.append(origin)
            banks[branch] = origin
        next_mask = sum(1 << i for i, value in enumerate(banks) if value is not None)
        assert next_mask == row['s4_next_mask'], 'Prediction NEXT/owner transition mismatch'
        if s2:
            assert s2['id'] not in seen_s2, 'Queue owner reached S2 twice'
            seen_s2.add(s2['id'])
            operations.append(dict(core_cycle=cycle, queue_owner_id=s2['id'], update=bool(s2['queue']),
                                   hit=bool(row['s2_hits']), hit_way=(row['s2_hits'].bit_length() - 1) if row['s2_hits'] else None))
        raw_hazard = row['s2_valid'] and row['s2_update'] and row['s2_index'] == row['s1_index']
        busy = row['s3_valid'] and row['s3_update']
        ready = not row['s1_valid'] or not (raw_hazard or busy)
        service = services.get(cycle)
        assert service is None or ready, 'Queue departure while S1 blocked'
        s3, s3_hits = s2, row['s2_hits'] if s2 else 0
        s2 = s1 if row['s1_valid'] and not (raw_hazard or busy) else None
        s1 = service if ready else s1
    assert seen_input_cycles == set(generated), 'Missing filter input or late PHT output'
    pending_before_s2 = {owner['id'] for owner in [s1, s2] if owner is not None}
    assert {owner['id'] for owner in services.values()} == seen_s2 | pending_before_s2, 'Served queue owner disappeared before S2'
    assert not seen_s2 & pending_before_s2
    assert sum(p['fate'] == 'served' for p in predictions) == len(packets)
    assert sum(p['fate'] == 'pending_at_capture_end' for p in predictions) == sum(p is not None for p in banks)
    return dict(operations=operations, predictions=predictions, packets=packets,
                pending_before_S2=sorted(pending_before_s2), pending_S3_owner=s3['id'] if s3 else None)


def analyze(folder, receipt):
    folder = Path(folder)
    output = parent(folder, receipt)
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    binding = proof['SMS_PHT_pipeline_binding']
    assert binding['PHT_only_filter_source'] and binding['all_inflight_tail_stages_captured'] and binding['new_RTL_state_bits'] == 0
    assert binding['current_NEXT_and_prediction_arbiter_asserted'] and binding['accepted_payload_asserted']
    rows = read(folder / 'sms-pht-pipeline.csv')
    queue_rows = read(folder / 'sms-pht-queues.csv')
    owners, observations = queue_owners(queue_rows, output['PHT_policy'], output['PHT_queue_read_policy'])
    expected = [(r['core_cycle'], r['clean'], r['native_dump']) for r in queue_rows if r['kind'] == 0 and r['queue'] == 0]
    assert [(r['core_cycle'], r['clean'], r['native_dump']) for r in rows if r['kind'] == 0] == expected
    generated = read(folder / 'sms-generation.csv')
    flow = replay(rows, owners, observations, generated)
    blocks = read_blocks(folder / 'simulator_err.txt')
    for phase in output['phases']:
        begin, end = phase['begin_core_exclusive'], phase['end_core_inclusive']
        operations = [r for r in flow['operations'] if begin < r['core_cycle'] <= end]
        packets = [r for r in flow['packets'] if begin < r['core_cycle'] <= end]
        counts = {name: sum(r['hit'] and r['update'] == update for r in operations)
                  for name, update in [('lookup_hit', False), ('update_hit', True)]}
        counts['pf_gen'] = len(packets)
        for name, count in counts.items():
            native, = [v for k, v in blocks[phase['native_dump']].items() if k.endswith('.sms_pht_' + name)]
            assert native == [count], 'PHT pipeline/native marginal mismatch'
        fates = Counter(p['filter_fate'] for p in packets)
        arrivals = [r for r in generated if r['kind'] in [1, 2] and begin < r['core_cycle'] <= end]
        assert len(arrivals) == phase['SMS']['raw_generated']
        assert fates['accepted'] == sum(r['kind'] == 1 for r in arrivals)
        assert fates['consecutive_tag_suppressed'] == sum(r['kind'] == 2 for r in arrivals)
        phase['PHT_pipeline'] = dict(S2_operations=len(operations), S2_lookup_hit=counts['lookup_hit'], S2_update_hit=counts['update_hit'],
                                     prediction_outputs=len(packets), filter_fates=dict(fates), native_marginals=3,
                                     output_branches=dict(Counter(p['branch'] for p in packets)),
                                     output_lookup_owners=len({p['lookup_owner_id'] for p in packets}))
    output.update(PHT_pipeline_flow=flow, PHT_pipeline_scope='Physical queue-generation identity through S1/S2/S3 and three region buffers. New buffer writes can overwrite held outputs; served, overwritten and capture-end pending fates are separate. Actual PHT-only filter source and accepted payload are source-bound. This is not PHT RAM-learning version, full-PC/retired identity, useful prefetch, critical-load or CPU-causal proof.')
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
