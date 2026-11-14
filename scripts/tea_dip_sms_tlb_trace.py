"""Join SMS translation arbitration to actual filter generations and preserve all later fates."""
import argparse
from bisect import bisect_right
from collections import Counter
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_l2_return_trace import read
from tea_dip_sms_observer import analyze as sms
from tea_dip_query_endpoint_evidence import digest


def audit(rows, owners, allow_hit):
    frames = {r['core_cycle']:r for r in rows if r['kind'] == 1}
    assert len(frames) == sum(r['kind'] == 1 for r in rows), 'Duplicate TLB frame'
    assert all(r['kind'] in [0,1] for r in rows)
    end = max(r['core_cycle'] for r in rows)
    histories = {slot:sorted([o for o in owners if o['slot'] == slot],key=lambda o:o['begin']) for slot in range(16)}

    def owner_at(slot, cycle):
        candidates = histories[slot]
        at = bisect_right([o['begin'] for o in candidates], cycle)-1
        assert at >= 0, 'Translation has no observed filter allocation'
        owner = candidates[at]
        assert owner['end'] is None or cycle < owner['end'], 'Translation uses an expired owner'
        return owner

    events = []
    for cycle,row in sorted(frames.items()):
        evicted = row['replace_mask'] if row['s1_valid'] else 0
        expected = row['base_mask'] & (~evicted | (65535 if allow_hit and row['s1_hit'] else 0)) & 65535
        assert row['eligible_mask'] == expected, 'TLB eligibility differs from source policy'
        chosen = row['chosen_mask']
        assert not chosen or (chosen & (chosen-1) == 0 and chosen & expected), 'Chosen TLB slot is not eligible'
        assert row['request_fire'] == bool(row['stage1_mask'] and not(row['stage1_mask'] & row['replace_mask'] and row['s1_valid'] and not row['s1_hit'])), 'Registered request cancellation differs'
        for delay in (1,2,3):
            stage = row['stage'+str(delay)+'_mask']
            earlier = frames.get(cycle-delay)
            assert stage == (earlier['chosen_mask'] if earlier else 0), 'Translation stage lost its arbiter owner'
        if not chosen:
            continue
        slot = chosen.bit_length()-1
        owner = owner_at(slot, cycle)
        state = owner['states'][bisect_right([s['core_cycle'] for s in owner['states']],cycle)-1]
        assert not state['physical_valid'] and state['region_tag'] == row['selected_tag'] and state['region_address'] == row['selected_region'], 'Translation payload differs from its filter owner'
        event = dict(arbitration_core=cycle,slot=slot,generation=owner['generation'],virtual_region=row['selected_region'],
                     selected_during_S1_hit=bool(chosen & evicted and row['s1_hit']))
        if cycle+3 > end:
            event['fate'] = 'capture_tail'
            events.append(event)
            continue
        req,resp,update = [frames.get(cycle+i) for i in (1,2,3)]
        assert all(r is not None for r in (req,resp,update)), 'Missing translation pipeline frame'
        assert req['stage1_mask'] == resp['stage2_mask'] == update['stage3_mask'] == chosen
        event.update(request_core=cycle+1,request_fire=bool(req['request_fire']),response_core=cycle+2,
                     response_fire=bool(resp['response_fire']),update_core=cycle+3)
        if req['request_fire']:
            assert req['request_va'] == row['selected_region'] << 10, 'Request VA differs from chosen generation'
        assert not resp['response_fire'] or req['request_fire'], 'Response has no actual accepted request'
        assert update['registered_response_fire'] == resp['response_fire'], 'Response register lost its event'
        if resp['response_fire']:
            assert update['registered_response_miss'] == resp['response_miss'] and update['registered_response_pa'] == resp['response_pa'], 'Response payload changed before update'
        assert update['update_valid'] == bool(resp['response_fire'] and not resp['response_miss'])
        assert not update['drop'] or update['update_valid']
        event['fate'] = ('cancelled_before_request' if not req['request_fire'] else 'no_response_observed'
                         if not resp['response_fire'] else 'miss' if resp['response_miss'] else 'drop' if update['drop'] else 'physical_update')
        if event['fate'] == 'physical_update':
            event['physical_region'] = update['registered_response_pa'] >> 10
            if cycle+4 > end:
                event['fate'] = 'translation_hit_commit_tail'
            elif owner['end'] is not None and owner['end'] <= cycle+4:
                event['fate'] = 'translation_hit_owner_replaced'
            else:
                committed = owner['states'][bisect_right([s['core_cycle'] for s in owner['states']],cycle+4)-1]
                assert committed['physical_valid'] and committed['region_address'] == event['physical_region'], 'Translation did not commit to its original filter generation'
        events.append(event)
    assert sum(r['request_fire'] for r in frames.values()) == sum(e.get('request_fire',False) for e in events) + sum(frames.get(e['arbitration_core']+1,{}).get('request_fire',0) for e in events if e['fate']=='capture_tail')
    return events


def analyze(folder, receipt):
    folder = Path(folder)
    output = sms(folder, receipt)
    proof = json.loads((folder/'probe-build-proof.json').read_text())
    binding = proof['SMS_TLB_binding']
    assert binding['actual_eligibility_and_three_stage_commits_asserted'] and binding['new_RTL_state_bits'] == 0
    probe = json.loads((folder/'probe-receipt.json').read_text())
    assert 'sms-tlb.csv' in probe['files'], 'TLB CSV is not covered by the verified probe receipt'
    rows = read(folder/'sms-tlb.csv')
    expected = [(r['core_cycle'],r['clean'],r['native_dump']) for r in read(folder/'sms-filter-state.csv') if r['kind']==0]
    assert [(r['core_cycle'],r['clean'],r['native_dump']) for r in rows if r['kind']==0] == expected, 'TLB phase boundary mismatch'
    events = audit(rows,output['SMS_filter_owners'],binding['allow_TLB_during_S1_hit'])
    blocks = read_blocks(folder/'simulator_err.txt');phases=[]
    for phase in output['phases']:
        lo,hi = phase['begin_core_exclusive'],phase['end_core_inclusive']
        selected = [r for r in rows if r['kind']==1 and lo < r['core_cycle'] <= hi]
        marginals = dict(tlb_req=sum(r['request_fire'] for r in selected),
                         tlb_resp_miss=sum(r['response_fire'] and r['response_miss'] for r in selected),
                         tlb_resp_drop=sum(r['drop'] for r in selected))
        for name,value in marginals.items():
            native, = [v for n,v in blocks[phase['native_dump']].items() if n.endswith('.sms_pf_filter_'+name)]
            assert native == [value], 'TLB native marginal mismatch: '+name
        lifetimes = [e for e in events if lo < e['arbitration_core'] <= hi]
        phases.append(dict(begin_core_exclusive=lo,end_core_inclusive=hi,native_marginals=marginals,
                           arbitration_fates=dict(Counter(e['fate'] for e in lifetimes)),
                           S1_hit_opportunity_entry_cycles=sum((r['base_mask'] & r['replace_mask']).bit_count() for r in selected if r['s1_valid'] and r['s1_hit']),
                           arbitrations_during_S1_hit=sum(e['selected_during_S1_hit'] for e in lifetimes)))
    return dict(passed=True,source=proof['source'],allow_TLB_during_S1_hit=binding['allow_TLB_during_S1_hit'],
                phases=phases,transactions=events,receipt_sha256=digest(receipt),TLB_csv_sha256=digest(folder/'sms-tlb.csv'),
                tool_sha256=digest(Path(__file__)),scope='Translation lifetimes start at actual arbitration and retain same-slot allocation generation. '
                'Request/miss/drop counters use their own event phase; a lifetime may cross warmup reset. Opportunity entry-cycles '
                'are neither distinct requests nor CPU loss. Physical updates are not cache residency or proof of useful prefetches. '
                'Successful S2/WB cohorts are not automatically retired dynamic instructions; lower-layer service remains unknown.')


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder',type=Path,required=True)
    parser.add_argument('--receipt',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args();assert not args.output.exists()
    args.output.write_text(json.dumps(analyze(args.folder,args.receipt),indent=2)+'\n')


if __name__=='__main__':
    main()
