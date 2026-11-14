"""Join one recorded L1 generation to source-qualified L2 and memory generations.

The caller supplies source bases and a request-reservation lower bound proven
for this model. The recorded entry-valid edge can follow an early Acquire. A physical line
or lifetime overlap alone never establishes a request association. Returned
service identities do not assign exclusive CPU loss or a counterfactual saving.
"""


def identity(owner):
    return {key: owner[key] for key in ['bank', 'slot', 'generation', 'begin', 'end']}


def join(l1, l2_owners, memory, source_bases, request_begin_lower):
    result = dict(L1={key: l1[key] for key in ['slot', 'generation', 'begin', 'end', 'line']},
                  request_begin_lower=request_begin_lower, global_source=None, L2_candidates=[], L2=None, L2_relationship=None,
                  memory_candidates=[], memory=None, unknown_reason=None)
    if not l1['allocation_observed'] or l1['first_data'] is None:
        result['unknown_reason'] = 'L1_allocation_or_first_data_not_observed'
        return result
    bank = (l1['line'] >> 6) & 3
    source = source_bases['channel' + str(bank & 1)] + l1['slot']
    result['global_source'] = source
    candidates = []
    for owner in l2_owners:
        if not (owner['allocation_observed'] and owner['bank'] == bank and owner['line'] == l1['line']
                and owner['begin'] < l1['first_data'] and request_begin_lower < owner['end']):
            continue
        state = owner['states'][0]
        if state['channel'] != 1:
            continue
        merged = owner['merged_demand']
        if state['opcode'] == 6 and state['source_id'] == source:
            relationship = 'primary_AcquireBlock'
        elif (merged is not None and merged['transition_observed'] and merged['opcode'] == 6
              and merged['source_id'] == source
              and max(request_begin_lower, owner['begin']) <= merged['first_observed']
              < min(l1['first_data'], owner['end'])):
            relationship = 'registered_merged_AcquireBlock'
        else:
            continue
        candidates.append((owner, relationship))
        result['L2_candidates'].append(dict(**identity(owner), relationship=relationship))
    if len(candidates) != 1:
        result['unknown_reason'] = 'no_unique_source_qualified_L2_generation'
        return result
    owner, relationship = candidates[0]
    result.update(L2=identity(owner), L2_relationship=relationship)
    requests = [request for request in memory if not request['is_write']
                and request['lineage']['L2'] == result['L2']]
    result['memory_candidates'] = [dict(id=r['id'], generation=r['generation'],
                                        accepted_core=r['accepted_core']) for r in requests]
    if len(requests) != 1:
        result['unknown_reason'] = 'no_unique_memory_request_for_L2_generation'
        return result
    request, = requests
    result['memory'] = dict(id=request['id'], generation=request['generation'],
                           accepted_core=request['accepted_core'],
                           response_core=request.get('response_core'), lineage=request['lineage'])
    if 'response_core' not in request:
        result['unknown_reason'] = 'memory_response_not_observed'
    return result
