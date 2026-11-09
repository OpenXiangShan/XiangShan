"""Compare closed dense micro traces at verified identical retirement-PC frontiers."""
from collections import Counter
from pathlib import Path
import argparse
import json
import sqlite3

import dip_analyze as dip
from native_perf import read_blocks,unique_counters
from tea_dip_query_endpoint_evidence import STATES,digest,verify


def window(root,case,payload_pc,kind):
    folder=root/case['variant'];result=json.loads((folder/'result.json').read_text())
    assert result['passed'] and result['original_process_closed'] and result['source']==case['source']
    calibration=json.loads((folder/'sampling-calibration.json').read_text())
    if kind=='sampler':assert calibration['passed'] and calibration['all_native_multivalues_equal'] and all(calibration['auxiliary_tables_equal'].values())
    else:assert calibration['required'] is False
    block,=read_blocks(folder/'simulator_err.txt').values();native=unique_counters(block)
    def rob(suffix):
        value,=[v for n,v in native.items()if n.endswith('.rob.'+suffix)];return value
    endpoint=rob('tea_exact_endpoint_cycle');native_states={s:rob('tea_exact_endpoint_state_'+str(i))for i,s in enumerate(STATES)}
    stream=[];prefix=[[0,0,0,0]];commit_cycles=[]
    with sqlite3.connect((folder/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True)as db:
        db.row_factory=sqlite3.Row
        names=[r['name']for r in db.execute('PRAGMA table_info(Tea_0)')]
        masks=sorted((n for n in names if n.startswith('VALIDMASK_')),key=lambda n:int(n.rsplit('_',1)[1]))
        width=sum(n.startswith('PCVEC_')for n in names)//len(masks)
        for index,row in enumerate(db.execute('SELECT * FROM Tea_0 ORDER BY CYCLE,ID')):
            assert row['CYCLE']==index and 0<=row['STATE']<4
            states=prefix[-1].copy();states[row['STATE']]+=1;prefix.append(states)
            if row['CONTEXT_RETIRED']:
                assert row['STATE']==0;commit_cycles.append(row['CYCLE'])
                pcs=[row['PCVEC_'+str(rank*width+bit)]for rank,name in enumerate(masks)for bit in range(width)if(row[name]>>bit)&1]
                assert len(pcs)==row['CONTEXT_RETIRED']
                stream.extend(dict(pc=pc,cycle=row['CYCLE'])for pc in pcs)
        assert len(prefix)-1==endpoint and dict(zip(STATES,prefix[-1]))==native_states and len(stream)==rob('tea_exact_endpoint_retired')
        end_indices=[i for i,x in enumerate(stream)if x['pc']==payload_pc];assert len(end_indices)==1
        payload_count=end_indices[0]+1;payload_end=stream[payload_count-1]['cycle']+1
        samples=list(db.execute('SELECT * FROM Dip_0 ORDER BY CYCLE,ID'))
        finals=list(db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'))
        dispatch_width=sum(r['name'].startswith('SLOTS_')and r['name'].endswith('_STATE')for r in db.execute('PRAGMA table_info(Dip_0)'))
        assert [r['CYCLE']for r in samples]==list(range(endpoint))
        _,full_dip=dip.analyze(samples,finals,dispatch_width,unresolved_details=True)
        profile,roi_dip=dip.analyze(samples,finals,dispatch_width,end_cycle=payload_end,unresolved_details=True)
        assert full_dip['sampled_cycles']==endpoint and roi_dip['sampled_cycles']==payload_end
        assert sum(roi_dip['state_cycles'].values())+roi_dip['unresolved_cycles']==payload_end
    gaps=[b-a-1 for a,b in zip(commit_cycles,commit_cycles[1:])]
    data=dict(variant=case['variant'],source=case['source'],binary_sha256=result['binary_sha256'],native_exact_cycles=endpoint,
              native_exact_retired=len(stream),trap_instructions=result['instructions'],trap_cycles=result['cycles'],native_exact_states=native_states,
              complete_TEA_states_and_retired_match_native=True,payload_retirement_members=payload_count,payload_endpoint_cycle=payload_end,
              payload_native_TEA_states=dict(zip(STATES,prefix[payload_end])),payload_cycle_full_retirement_batch=sum(x['cycle']==payload_end-1 for x in stream),
              full_DIP=full_dip,payload_DIP=roi_dip,commit_cycle_count=len(commit_cycles),closed_gap_cycles=sum(gaps),
              short_closed_gap_cycles=sum(x for x in gaps if x<128),long_closed_gap_lengths=[x for x in gaps if x>=128],
              short_gap_histogram=[dict(noncommit_cycles=n,gaps=c)for n,c in sorted(Counter(x for x in gaps if x<128).items())],
              host_wall_seconds=result['host_wall_seconds'],host_CPU_seconds=result['host_rusage']['utime']+result['host_rusage']['stime'])
    return data,stream,prefix


def analyze(root,payload_pc,anchor_pc,kind):
    receipt=json.loads((root/'closure-receipt.json').read_text());assert receipt['ready'] and receipt['all_original_guests_closed'];verify(root,receipt['files'])
    spec=json.loads((root/'experiment.json').read_text());assert digest(root/'experiment.json')==receipt['spec_sha256'] and spec['period']==1
    if kind=='padding':assert spec['image_identity']['original_payload_code_prefix_and_data_identical']
    cases={x['variant']:x for x in spec['cases']};assert set(cases)=={'stage_control','candidate'}
    results=[window(root,cases[v],payload_pc,kind)for v in ['stage_control','candidate']]
    (a,x,px),(b,y,py)=results
    assert [r['pc']for r in x]==[r['pc']for r in y] and a['payload_retirement_members']==b['payload_retirement_members']
    count=a['payload_retirement_members'];frontiers=[]
    for i,(left,right)in enumerate(zip(x[:count],y[:count])):
        delta=right['cycle']-left['cycle'];states={s:py[right['cycle']+1][j]-px[left['cycle']+1][j]for j,s in enumerate(STATES)}
        assert sum(states.values())==delta
        frontiers.append(dict(ordinal=i+1,pc=hex(left['pc']),control_cycle=left['cycle'],candidate_cycle=right['cycle'],delta_cycles=delta,state_delta=states))
    anchors=[r for r in frontiers if int(r['pc'],16)==anchor_pc]
    for i,r in enumerate(anchors):r['anchor_occurrence']=i+1
    deltas=[r['delta_cycles']for r in frontiers]
    first=next((r for r in frontiers if r['delta_cycles']),None)
    state_delta={s:b['native_exact_states'][s]-a['native_exact_states'][s]for s in STATES}
    dip_delta={s:b['payload_DIP']['state_cycles'][s]-a['payload_DIP']['state_cycles'][s]for s in dip.STATES}
    dip_delta['Unresolved']=b['payload_DIP']['unresolved_cycles']-a['payload_DIP']['unresolved_cycles']
    payload_delta=b['payload_endpoint_cycle']-a['payload_endpoint_cycle'];assert sum(dip_delta.values())==payload_delta
    return dict(passed=True,kind=kind,windows=[a,b],identical_complete_retirement_PC_stream=True,payload_end_pc=hex(payload_pc),anchor_pc=hex(anchor_pc),
                payload_cycle_delta=payload_delta,full_native_cycle_delta=b['native_exact_cycles']-a['native_exact_cycles'],full_native_state_delta=state_delta,
                payload_DIP_delta=dip_delta,payload_frontier_min=min(deltas),payload_frontier_max=max(deltas),first_time_difference=first,
                differing_payload_frontiers=sum(n!=0 for n in deltas),retirement_frontiers=frontiers,anchor_frontiers=anchors,
                source_sha256={str(root/'closure-receipt.json'):digest(root/'closure-receipt.json'),str(Path(__file__)):digest(Path(__file__)),str(Path(dip.__file__)):digest(Path(dip.__file__))},
                scope='Dense micro only. Grid0..nativeExactCycle-1, TEA state counts and retirement-member count must exactly match the native prefix. '
                      'Ordinal matching requires identical complete observed retirement-PC streams. It is a retirement frontier, not per-instruction execution latency or a unique critical cause. '
                      'The payload endpoint is the cycle containing its last selected PC; other members can retire in that same cycle. '
                      'DIP source samples stop at that endpoint, while all later recorded finals remain available. Unresolved stays explicit. '
                      'Changed post-payload programs are separate boundary experiments; their fates never overwrite the original run. No suite score or isolated host-cost claim.')


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir',type=Path,required=True);parser.add_argument('--kind',choices=['sampler','padding'],required=True)
    parser.add_argument('--payload-end-pc',type=lambda x:int(x,0),required=True);parser.add_argument('--anchor-pc',type=lambda x:int(x,0),required=True)
    parser.add_argument('--output',type=Path,required=True);args=parser.parse_args();assert not args.output.exists()
    data=analyze(args.run_dir.resolve(),args.payload_end_pc,args.anchor_pc,args.kind);args.output.write_text(json.dumps(data,indent=2)+'\n')
    print(json.dumps({k:data[k]for k in ['passed','kind','payload_cycle_delta','full_native_cycle_delta','payload_DIP_delta','payload_frontier_min','payload_frontier_max','first_time_difference']},indent=2))


if __name__=='__main__':main()
