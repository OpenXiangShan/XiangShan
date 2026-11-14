"""Locate a physical line in calibrated SMS events without guessing untranslated payloads."""
from bisect import bisect_right


def summarize(audit, line, demand_core):
    """Consume an SMS internal audit; earlier events are strictly before first demand S2."""
    assert audit['passed'] and line & 63 == 0
    region, bit = line >> 10, 1 << ((line >> 6) & 15)

    def physical(row):
        return row['physical_valid'] and row['region_address'] == region

    generated = audit['SMS_generation_events']
    accepted = [r for r in generated if r['kind'] == 1]
    owners = []
    for owner in audit['SMS_filter_owners']:
        states = owner['states']
        matching = [r for r in states if physical(r)]
        if not matching:
            continue
        before = bisect_right([r['core_cycle'] for r in states], demand_core) - 1
        at = states[before] if before >= 0 and (owner['end'] is None or demand_core < owner['end']) else None
        owners.append(dict(slot=owner['slot'], generation=owner['generation'], begin=owner['begin'],
                           end=owner['end'], physical_region_states=matching,
                           target_bit_states=[r for r in matching if r['region_bits'] & bit],
                           target_pending_states=[r for r in matching if r['region_bits'] & ~r['filter_bits'] & bit],
                           state_at_demand=at,
                           generation_commits=[r for r in audit['SMS_generation_commits']
                                               if (r['slot'], r['generation']) == (owner['slot'], owner['generation'])]))
    departures = [r for r in audit['SMS_filter_departures'] if r['address'] == line]
    packets = [r for r in audit['SMS_packets'] + audit['SMS_pending_at_capture_end'] if r['address'] == line]
    assert [(r['core_cycle'], r['address']) for r in departures] == [(r['input_core'], r['address']) for r in packets]
    for departure in departures:
        owner, = [o for o in owners if (o['slot'], o['generation']) == (departure['slot'], departure['generation'])]
        assert owner['begin'] <= departure['core_cycle'] and (owner['end'] is None or departure['core_cycle'] < owner['end'])
        assert any(r['core_cycle'] <= departure['core_cycle'] for r in owner['target_pending_states'])
    selected = [r for r in accepted if physical(r) and r['region_bits'] & bit]
    return dict(physical_line=hex(line), demand_first_S2_core=demand_core,
                physical_target_generations=selected, physical_region_filter_owners=owners,
                target_training_dequeues=[r for r in audit['SMS_training_events'] if r['physical_address'] & ~63 == line],
                target_departures=departures, target_packets=packets,
                before_demand=dict(physical_target_generations=sum(r['core_cycle'] < demand_core for r in selected),
                                   owners_with_target_pending_state=sum(any(r['core_cycle'] < demand_core for r in o['target_pending_states']) for o in owners),
                                   target_departures=sum(r['core_cycle'] < demand_core for r in departures),
                                   target_pipeline_outputs=sum('output_core' in r and r['output_core'] < demand_core for r in packets),
                                   suppressed_unknown_payloads=sum(r['kind'] == 2 and r['core_cycle'] < demand_core for r in generated),
                                   untranslated_accepted_payloads=sum(not r['physical_valid'] and r['core_cycle'] < demand_core for r in accepted)),
                scope='Physical membership only; filter slot/generation and actual departure preserve ownership. '
                      'State-at-demand includes that cycle; earlier counts use strict less-than. '
                      'Suppressed payload and untranslated accepted PA remain unknown. Training PA does not establish '
                      'prediction causality; SMS output does not prove L2 admission, residency, demand usefulness or saved CPU cycles.')


def pht_predictions(audit, line, demand_core):
    """Query the source-bound PHT output before filter suppression, including disabled fates."""
    assert audit['passed'] and line & 63 == 0
    packets = audit['PHT_pipeline_flow']['packets']
    region, bit = line >> 10, 1 << ((line >> 6) & 15)
    fates = ['accepted', 'consecutive_tag_suppressed', 'feedback_disabled', 'PHT_disabled']
    assert all(p['filter_fate'] in fates for p in packets)
    physical_region = [p for p in packets if p['payload']['physical_valid']
                       and p['payload']['region_address'] == region]
    target = [p for p in physical_region if p['payload']['region_bits'] & bit]
    earlier = [p for p in packets if p['core_cycle'] < demand_core]
    return dict(physical_line=hex(line), demand_first_S2_core=demand_core,
                physical_region_predictions=physical_region, physical_target_predictions=target,
                before_demand=dict(PHT_outputs=len(earlier),
                                   target_by_filter_fate={f: sum(p['core_cycle'] < demand_core and p['filter_fate'] == f
                                                               for p in target) for f in fates},
                                   untranslated_by_filter_fate={f: sum(not p['payload']['physical_valid']
                                                                      and p['filter_fate'] == f for p in earlier)
                                                                 for f in fates}),
                scope='Actual PHT lookup/prediction owners and three output buffers before filter suppression. '
                      'Filter-disabled outputs remain separate from raw filter arrivals. Untranslated PA remains '
                      'unknown; zero visible physical target predictions does not prove no virtual target prediction. '
                      'No L2 residency, retired-instruction identity, training-version causality or saved cycles are inferred.')
