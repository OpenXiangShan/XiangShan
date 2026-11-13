"""Match SMS fire to the observed L2 pipeline output, preserving pending capture tails."""
import argparse
from collections import deque
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_l2_return_trace import read
from tea_dip_memory_lineage_trace import analyze as lineage_analyze


def match_pipeline(rows):
    pending=deque();complete=[];previous=-1;input_cycle=-1;output_cycle=-1;sequence=0
    for row in rows:
        cycle=row['core_cycle'];assert cycle>=previous,'SMS cycle moved backwards';previous=cycle
        if row['kind']==0:continue
        assert row['kind']in[1,2]and row['address'] & 63==0
        if row['kind']==1:
            assert cycle>input_cycle,'Duplicate SMS input cycle';input_cycle=cycle;sequence+=1
            pending.append(dict(sequence=sequence,input_core=cycle,address=row['address']))
        else:
            assert cycle>output_cycle,'Duplicate SMS output cycle';output_cycle=cycle
            assert pending,'SMS output without an observed input'
            owner=pending.popleft();assert owner['address']==row['address'],'SMS FIFO address mismatch'
            assert cycle==owner['input_core']+2,'SMS pipeline stage latency mismatch'
            owner['output_core']=cycle;complete.append(owner)
    assert all(owner['input_core']+2>previous for owner in pending),'Missing SMS output before the capture boundary'
    return complete,list(pending)


def analyze(folder,receipt):
    folder=Path(folder);result=lineage_analyze(folder,receipt)
    binding=json.loads((folder/'probe-build-proof.json').read_text())['SMS_request_binding']
    assert binding['stages']==2 and binding['source_id']==10 and binding['actual_pipeline_commit_and_forward_asserted']and binding['new_RTL_state_bits']==0
    rows=read(folder/'sms-l2-requests.csv');boundary=lambda rows:[(r['core_cycle'],r['clean'],r['native_dump'])for r in rows if r['kind']==0]
    assert boundary(rows)==boundary(read(folder/'load-l2-mshr.csv')),'SMS phase boundary mismatch'
    complete,pending=match_pipeline(rows);blocks=read_blocks(folder/'simulator_err.txt')
    for phase in result['phases']:
        lo,hi=phase['begin_core_exclusive']+1,phase['end_core_inclusive']+1
        observed=sum(r['kind']==1 and lo<=r['core_cycle']<hi for r in rows)
        native,=[v for k,v in blocks[phase['native_dump']].items()if k.endswith('.prefetcher.SMS_fire_l2')]
        assert native==[observed],('SMS native fire mismatch',phase['native_dump'],observed,native)
        blocked,=[v for k,v in blocks[phase['native_dump']].items()if k.endswith('.prefetcher.SMS_block_l2')]
        phase['SMS_pipeline']=dict(input_fires=observed,native_arb_blocked=blocked,observed_outputs=sum(r['kind']==2 and lo<=r['core_cycle']<hi for r in rows))
    assert len(complete)+len(pending)==sum(r['kind']==1 for r in rows)
    result.update(SMS_packets=complete,SMS_pending_at_capture_end=pending,SMS_input_fires=len(complete)+len(pending),SMS_scope='Actual native SMS-fire predicate and registered two-stage L2 pipeline with current/NEXT/forwarding assertions. Raw repeated addresses retain FIFO sequence identity and exact two-cycle passage. The output is a forwarded SMS request, not proof of L2 cache admission or residency. Missing input does not distinguish SMS training/generation/filter/translation; native arb blocked cycles are retained.')
    return result


def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--folder',type=Path,required=True);p.add_argument('--receipt',type=Path,required=True);p.add_argument('--output',type=Path,required=True);args=p.parse_args();args.output.write_text(json.dumps(analyze(args.folder,args.receipt),indent=2)+'\n')


if __name__=='__main__':main()
