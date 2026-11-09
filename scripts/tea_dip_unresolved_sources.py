"""Keep source-cycle/slot identity while replaying the unchanged DIP fate rules."""
from collections import Counter,defaultdict
from heapq import merge
from pathlib import Path
import argparse
import json
import sqlite3

import dip_analyze as dip
from tea_dip_query_endpoint_evidence import digest,verify


def replay(samples,finals,width):
    pending,waiting=defaultdict(Counter),defaultdict(Counter)
    profile=Counter();source_count=0
    records=merge(((r['CYCLE'],1,r)for r in samples),((r['CYCLE'],0,r)for r in finals),key=lambda r:r[:2])
    for cycle,is_sample,row in records:
        if is_sample:
            source_count+=width
            for lane in range(width):
                prefix='SLOTS_'+str(lane)
                key=(row[prefix+'_STATE'],row[prefix+'_CAUSE'],row[prefix+'_PC']&((1<<64)-1),cycle,lane)
                kind=row[prefix+'_KIND'];ref=dip.target(row,prefix+'_TARGET')
                if kind==0:profile[key]+=1
                elif kind==1:pending[ref][key]+=1
                else:waiting[ref[0]][key]+=1
            continue
        kind=row['KIND'];ref=dip.target(row,'TARGET')
        if kind==0:
            profile.update(pending.pop(ref,Counter()))
        elif kind==1:
            lost=Counter()
            for counts in waiting.values():lost.update(counts)
            waiting.clear()
            for old in list(pending):
                if dip.killed(old[1:],ref[1:],row['SELF']):lost.update(pending.pop(old))
            for (_,_,_,origin,lane),count in lost.items():
                pending[ref][(3,row['CAUSE'],row['PC']&((1<<64)-1),origin,lane)]+=count
        elif kind==2:
            successor=dip.target(row,'NEXT')
            for (state,cause,_,origin,lane),count in waiting.pop(ref[0],Counter()).items():
                pending[successor][(state,row['CAUSE']if state==2 else cause,row['PC']&((1<<64)-1),origin,lane)]+=count
        elif kind==3:
            for old in list(pending):
                if dip.killed(ref[1:],old[1:],True):
                    settled=Counter({k:v for k,v in pending[old].items()if k[0]==3});profile.update(settled);pending[old]-=settled
                    if not pending[old]:del pending[old]
    aggregate=Counter()
    origins=Counter()
    for (state,cause,pc,cycle,lane),count in profile.items():aggregate[state,cause,pc]+=count;origins[cycle,lane]+=count
    for counts in [*pending.values(),*waiting.values()]:
        for (_,_,_,cycle,lane),count in counts.items():origins[cycle,lane]+=count
    assert len(origins)==source_count and all(n==1 for n in origins.values()),'Each source slot must have exactly one fate or unresolved location'
    def describe(counts):
        cycles=Counter();provisional=Counter()
        for (state,cause,pc,cycle,_),count in counts.items():cycles[cycle]+=count;provisional[state,cause,pc]+=count
        return dict(slots=sum(counts.values()),source_cycle_range=[min(cycles),max(cycles)],source_cycles=[dict(cycle=c,slots=n)for c,n in sorted(cycles.items())],
                    provisional=[dict(state=dip.STATES[s],cause_mask=c,pc=hex(pc),slots=n)for(s,c,pc),n in sorted(provisional.items())])
    selected=[dict(target=dict(generation=ref[0],rob_flag=ref[1],rob_value=ref[2],rob_isformer=ref[3]),**describe(c))for ref,c in sorted(pending.items())if sum(c.values())]
    successors=[dict(generation=g,**describe(c))for g,c in sorted(waiting.items())if sum(c.values())]
    unresolved=sum(x['slots']for x in selected+successors)
    return aggregate,dict(source_slots=source_count,unresolved_slots=unresolved,source_identity_conserved=True,pending_selected_target=selected,waiting_for_dispatch_successor=successors)


def analyze(root,variant):
    receipt=json.loads((root/'closure-receipt.json').read_text());assert receipt['ready']and receipt['all_original_guests_closed'];verify(root,receipt['files'])
    spec=json.loads((root/'experiment.json').read_text());assert digest(root/'experiment.json')==receipt['spec_sha256']
    case,=[x for x in spec['cases']if x['variant']==variant]
    folder=root/variant;result=json.loads((folder/'result.json').read_text());assert result['passed']and result['original_process_closed']and result['source']==case['source']
    with sqlite3.connect((folder/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True)as db:
        db.row_factory=sqlite3.Row
        samples=list(db.execute('SELECT * FROM Dip_0 ORDER BY CYCLE,ID'))
        finals=list(db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'))
        width=sum(r['name'].startswith('SLOTS_')and r['name'].endswith('_STATE')for r in db.execute('PRAGMA table_info(Dip_0)'))
    old,summary=dip.analyze(samples,finals,width,unresolved_details=True)
    observed,data=replay(samples,finals,width)
    assert observed==old and data['unresolved_slots']==summary['unresolved_slots'] and data['source_slots']==summary['sampled_slots']
    return dict(passed=True,variant=variant,source=case['source'],dispatch_width=width,original_summary=summary,unresolved_origin=data,
                consumer_sha256=digest(Path(__file__)),dip_consumer_sha256=digest(Path(dip.__file__)),closure_receipt_sha256=digest(root/'closure-receipt.json'),
                scope='Replays identical DIP fate/flush rules with a unique source-cycle/slot identity. Full PC/state/mask classification and unresolved count must match the original consumer. '
                      'Provisional unresolved labels remain provisional; no missing final, counterfactual retirement or saved cycle is synthesized. Source ranges identify where censored mass came from, not a unique hardware cause.')


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir',type=Path,required=True);parser.add_argument('--variant',required=True);parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args();assert not args.output.exists()
    data=analyze(args.run_dir.resolve(),args.variant);args.output.write_text(json.dumps(data,indent=2)+'\n')
    print(json.dumps(dict(passed=data['passed'],variant=args.variant,unresolved_slots=data['unresolved_origin']['unresolved_slots'],
                         groups=[{k:x[k]for k in ['slots','source_cycle_range','provisional']}for x in data['unresolved_origin']['pending_selected_target']]),indent=2))


if __name__=='__main__':main()
