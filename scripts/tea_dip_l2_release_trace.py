"""Join full release flags to actual L2 MSHR generations and report unfinished work."""
import argparse
from bisect import bisect_right
from collections import Counter, defaultdict
import json
from pathlib import Path

from tea_dip_gsim_l2_release_probe import FIELDS, SCHEDULE, WAIT
from tea_dip_l2_return_trace import owners, read
from tea_dip_query_endpoint_evidence import verify


def reconstruct(original, release, end, requests=()):
    epochs,_,_=owners(original,end)
    boundary=lambda rows:[(r['core_cycle'],r['clean'],r['native_dump'])for r in rows if r['kind']==0]
    assert boundary(original)==boundary(release),'Release boundary mismatch'
    groups=defaultdict(list)
    for row in release:
        if row['kind']==0:continue
        assert row['kind']==1 and 0<=row['bank']<4 and 0<=row['mshr']<16
        group=groups[row['bank'],row['mshr']]
        assert not group or group[-1]['core_cycle']<row['core_cycle'],'Duplicate release state'
        assert all(row[n]in[0,1]for n in FIELDS)
        assert row['will_free']==int(row['valid']and all(row[n]for n in SCHEDULE+WAIT)),'Wrong release conjunction'
        assert row['valid'] or not any(row[n]for n in FIELDS),'Unnormalized invalid owner'
        group.append(row)
    assert len(groups)==64
    old_groups=defaultdict(list)
    for row in original:
        if row['kind']==1:old_groups[row['bank'],row['mshr']].append(row)
    def at(rows,cycles,cycle):
        index=bisect_right(cycles,cycle)-1
        assert index>=0,'Missing initial state'
        return rows[index]
    checked=0
    for key,rows in groups.items():
        old=old_groups[key]
        old_cycles=[r['core_cycle']for r in old]
        release_cycles=[r['core_cycle']for r in rows]
        for cycle in sorted(set(old_cycles+release_cycles)):
            a,b=at(old,old_cycles,cycle),at(rows,release_cycles,cycle)
            assert a['valid']==b['valid'],'Release owner lifetime mismatch'
            if a['valid']:
                assert (a['refill'],a['grant'],a['last'])==(b['s_refill'],b['w_grant'],b['w_grantlast']),'Release projection mismatch'
            checked+=1
    patterns=Counter();result=[]
    for key,records in epochs.items():
        rows=groups[key]
        for owner in records:
            selected=[r for r in rows if owner['begin']<=r['core_cycle']<owner['end']]
            assert selected and selected[0]['core_cycle']==owner['begin'] and all(r['valid']for r in selected)
            if owner['end']<end:
                assert selected[-1]['will_free'] and selected[-1]['core_cycle']==owner['end']-1,'Release edge without will_free'
            segments=[];budget=Counter()
            for i,row in enumerate(selected):
                finish=selected[i+1]['core_cycle']if i+1<len(selected)else owner['end']
                pending=[n for n in SCHEDULE+WAIT if not row[n]]
                label=','.join(pending)or'ready_to_free'
                duration=finish-row['core_cycle'];assert duration>0
                budget[label]+=duration;patterns[label]+=duration
                segments.append(dict(begin=row['core_cycle'],end=finish,cycles=duration,pending=pending,will_free=row['will_free']))
            assert sum(budget.values())==owner['end']-owner['begin']
            result.append(dict(bank=key[0],slot=key[1],generation=owner['generation'],begin=owner['begin'],end=owner['end'],line=owner['line'],closed=owner['end']<end,unfinished_flag_cycles=dict(budget),segments=segments))
    queried=[]
    for bank,slot,begin in requests:
        matches=[o for o in result if(o['bank'],o['slot'],o['begin'])==(bank,slot,begin)]
        assert len(matches)==1,'Requested owner is not uniquely observed'
        queried.append(matches[0])
    return dict(passed=True,owners=len(result),closed_owners=sum(o['closed']for o in result),projection_checks=checked,
                unfinished_flag_cycles=dict(patterns),requests=queried,
                scope='Flag combinations partition actual owner residence. They do not measure CPU lost cycles; source numbers alone never join owners.')


def analyze(folder,receipt,requests=()):
    folder=Path(folder);closure=json.loads(Path(receipt).read_text());assert closure['ready']and not closure['original_process_live']
    verify(folder,closure['files'])
    probe=json.loads((folder/'probe-receipt.json').read_text());verify(folder,probe['files'])
    proof=json.loads((folder/'probe-build-proof.json').read_text());assert proof['L2_release_binding']['fields']==FIELDS
    original,release=read(folder/'load-l2-mshr.csv'),read(folder/'load-l2-release.csv')
    end=max(r['core_cycle']for r in original)+1
    return reconstruct(original,release,end,requests)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder',type=Path,required=True);parser.add_argument('--receipt',type=Path,required=True)
    parser.add_argument('--owner',nargs=3,type=int,action='append',default=[],metavar=('BANK','SLOT','BEGIN'))
    parser.add_argument('--output',type=Path)
    args=parser.parse_args();value=analyze(args.folder,args.receipt,args.owner);text=json.dumps(value,indent=2)+'\n'
    if args.output:args.output.write_text(text)
    else:print(text,end='')


if __name__=='__main__':main()
