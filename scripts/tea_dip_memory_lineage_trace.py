"""Join memory full-ID generations to NCB, LLC and originating L2 generations."""
import argparse
from bisect import bisect_right
from collections import Counter, defaultdict
import json
from pathlib import Path

from tea_dip_gsim_memory_lineage_probe import NCB_FIELDS, LLC_FIELDS
from tea_dip_l2_merge_trace import analyze as merge_analyze
from tea_dip_l2_return_trace import read


def generations(rows, kind):
    fields = NCB_FIELDS if kind=='ncb' else LLC_FIELDS
    stable = fields[1:5] if kind=='ncb' else fields[1:]
    previous, active, epochs = {}, {}, Counter()
    result = []
    last_cycle = -1
    for row in rows:
        cycle = row['core_cycle']
        assert cycle>=last_cycle, 'Lineage state cycle moved backwards'
        last_cycle = cycle
        if row['kind']==0:
            continue
        assert row['kind']==1 and row['valid'] in [0,1]
        key = row['bank'], row['slot']
        assert (key[0]==0 and 0<=key[1]<64) if kind=='ncb' else (0<=key[0]<4 and 0<=key[1]<16)
        first = key not in previous
        assert first or cycle>previous[key], 'Duplicate lineage slot cycle'
        previous[key] = cycle
        if not row['valid']:
            assert not any(row[field] for field in fields[1:]), 'Inactive lineage payload is not zero'
            if key in active:
                owner = active.pop(key)
                owner.update(end=cycle,right_censored=False)
            continue
        if key not in active:
            epochs[key]+=1
            owner = dict(bank=key[0],slot=key[1],generation=epochs[key],begin=cycle,
                         left_censored=first,states=[])
            owner.update({field:row[field] for field in fields[1:]})
            active[key] = owner
            result.append(owner)
        owner = active[key]
        assert all(owner[field]==row[field] for field in stable), 'Lineage identity changed within a generation'
        owner['states'].append(row)
    assert len(previous)==64, 'Missing lineage slot snapshots'
    for owner in active.values():
        owner.update(end=last_cycle+1,right_censored=True)
    return result


def attach_lineage(memory, ncb, llc, l2):
    def index(owners, key):
        indexed=defaultdict(list)
        for owner in owners:
            indexed[key(owner)].append(owner)
        for values in indexed.values():
            values.sort(key=lambda owner:owner['begin'])
        return indexed

    ncb_by_slot=index(ncb,lambda owner:owner['slot'])
    llc_by_id=index(llc,lambda owner:owner['req_id'])
    l2_by_slot=index(l2,lambda owner:(owner['bank'],owner['slot']))

    def at(indexed, key, cycle):
        entries=indexed.get(key,[])
        # IDs may wrap across slots; all live matches must still be unique.
        matches=[owner for owner in entries if owner['begin']<=cycle<owner['end']]
        assert len(matches)<=1, 'Ambiguous live lineage identity'
        return matches[0] if matches else None

    def identity(owner):
        return {field:owner[field] for field in ['bank','slot','generation','begin','end']}

    counts=Counter()
    for request in memory:
        lineage=dict(NCB=None,LLC=None,L2=None,unknown_reason=None)
        request['lineage']=lineage
        if request['is_write']:
            lineage['unknown_reason']='write_API_not_joined'
        elif request['id'] & ~63 != 0x20000:
            lineage['unknown_reason']='other_AXI_master_namespace'
        else:
            cycle=request['accepted_core']
            owner=at(ncb_by_slot,request['id'] & 63,cycle)
            assert owner is not None, 'Memory read has no live NCB generation'
            assert owner['line']==request['address'] & ~63, 'Memory/NCB block mismatch'
            state=owner['states'][bisect_right([r['core_cycle'] for r in owner['states']],cycle)-1]
            assert state['r_pending'], 'Memory read without NCB read-data operation'
            lineage['NCB']=identity(owner)
            # The SN link replaces cacheable SrcID with the actual LLC route ID2.
            # ResponseUnit.tgtID retains the original RN target; it is not the LLC ID.
            origin=at(llc_by_id,owner['txn_id'],cycle) if owner['src_id']==2 else None
            if origin is None:
                lineage['unknown_reason']='non_LLC_NCB_source' if owner['src_id']!=2 else 'no_live_LLC_reqid_owner'
            else:
                assert origin['line']==owner['line'], 'NCB/LLC block mismatch'
                lineage['LLC']=identity(origin)
                txn=origin['txn_id']
                bank,slot=(txn>>9)&3,txn&255
                if origin['src_id']!=0 or txn & ~0x6ff or slot>=16:
                    lineage['unknown_reason']='nonlocal_RN_or_non_MSHR_transaction'
                else:
                    source=at(l2_by_slot,(bank,slot),origin['begin'])
                    if source is None:
                        lineage['unknown_reason']='no_live_L2_owner_at_LLC_allocation'
                    else:
                        assert source['line']==origin['line'], 'LLC/L2 block mismatch'
                        lineage['L2']=identity(source)
        counts[lineage['unknown_reason'] or 'complete_L2_LLC_NCB_memory_chain']+=1
    return dict(counts)


def analyze(folder, receipt):
    folder=Path(folder)
    result=merge_analyze(folder,receipt)
    proof=json.loads((folder/'probe-build-proof.json').read_text())['memory_lineage_binding']
    assert proof['next_register_commit_asserted'] and proof['new_RTL_state_bits']==0
    assert proof['NCB_slots']==64 and proof['LLC_banks']==4 and proof['LLC_slots_per_bank']==16
    assert proof['AXI_ID_route']['NCB_read_API_namespace']==0x20000 and proof['AXI_ID_route']['slot_mask']==63
    ncb_rows,llc_rows=read(folder/'load-ncb-state.csv'),read(folder/'load-llc-response.csv')
    boundary=lambda rows:[(r['core_cycle'],r['clean'],r['native_dump']) for r in rows if r['kind']==0]
    assert boundary(ncb_rows)==boundary(llc_rows)==boundary(read(folder/'load-l2-mshr.csv')), 'Lineage boundary mismatch'
    ncb,llc=generations(ncb_rows,'ncb'),generations(llc_rows,'llc')
    memory=result['memory_transactions']+result['memory_incomplete']
    counts=attach_lineage(memory,ncb,llc,result['L2_owners'])
    assert sum(counts.values())==len(memory)
    for phase in result['phases']:
        lo,hi=phase['begin_core_exclusive']+1,phase['end_core_inclusive']+1
        phase['memory_lineage_by_acceptance']=dict(Counter(r['lineage']['unknown_reason'] or 'complete_L2_LLC_NCB_memory_chain'
            for r in memory if lo<=r['accepted_core']<hi))
    result.update(NCB_owner_generations=ncb,LLC_response_generations=llc,memory_lineage_counts=counts,
                  memory_lineage_scope='Actual RAM full-ID namespace, registered NCB generation and saved CHI identity, LLC SN route ID2/reqID and saved RN/txnID, originating L2 bank/slot/lifetime/block. Generation reuse and capture boundaries remain explicit. This does not assign exclusive CPU cycles or infer speedup; unresolved routing and non-MSHR origins are retained.')
    return result


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder',type=Path,required=True)
    parser.add_argument('--receipt',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args()
    args.output.write_text(json.dumps(analyze(args.folder,args.receipt),indent=2)+'\n')


if __name__=='__main__':
    main()
