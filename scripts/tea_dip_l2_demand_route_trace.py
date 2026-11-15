"""Validate L2 request routes and query buffer generations by bank/source/line/time.

Buffer residence and overlapping wait masks are observations, not CPU lost cycles.
A same-address prefetch owner is never substituted for the request's actual source.
"""
import argparse
from collections import Counter
import csv
from itertools import groupby
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_gsim_l2_demand_route_probe import FIELDS
from tea_dip_load_writeback_trace import analyze as writebacks
from tea_dip_query_endpoint_evidence import digest

PAYLOAD = ['line', 'source_id', 'opcode', 'param', 'req_source']


def reconstruct(rows, end):
    current, active, epochs = {}, {}, Counter()
    allocations, releases, owners = {}, {}, []
    previous = -1
    for cycle, records in groupby(rows, key=lambda r:r['core_cycle']):
        assert cycle > previous
        previous = cycle
        records = list(records)
        for row in records:
            if row['kind'] != 2:
                continue
            key = row['bank'],row['entry']
            assert 0 <= key[0] < 4 and 0 <= key[1] < 4
            old = current.get(key)
            assert old is None or old['core_cycle'] < cycle
            if row['valid']:
                assert row['line'] & 255 == key[0]*64
                if old is None or not old['valid']:
                    allocation = allocations.get((*key,cycle-1))
                    assert old is None or allocation is not None, 'Buffer owner has no preceding allocation'
                    if allocation:
                        assert all(allocation[n]==row[n]for n in PAYLOAD), 'Allocated payload changed'
                    epochs[key] += 1
                    owner = dict(bank=key[0],entry=key[1],generation=epochs[key],begin=cycle,end=end,
                                 allocation=allocation,output=None,states=[],initially_active=old is None)
                    active[key] = owner
                    owners.append(owner)
                else:
                    assert all(row[n]==old[n]for n in PAYLOAD), 'Buffer generation changed payload'
                active[key]['states'].append(row)
            elif key in active:
                owner = active.pop(key)
                owner['end'] = cycle
                owner['output'] = releases.get((*key,cycle-1))
                assert owner['output'] is not None, 'Buffer released without actual output handshake'
            current[key] = row
        for row in records:
            kind,bank = row['kind'],row['bank']
            assert kind in range(6)
            if kind in [0,2]:
                continue
            assert 0 <= bank < 4 and row['valid']
            if kind == 1 and row['alloc_mask']:
                mask = row['alloc_mask']
                assert row['ready'] and mask & (mask-1)==0 and mask<16
                slot = mask.bit_length()-1
                assert not current[bank,slot]['valid']
                allocations[bank,slot,cycle] = row
            if kind == 3 and row['ready']:
                # canFlow excludes chosenQValid, but does not exclude dup. An
                # input flow may share a buffered task's payload without issuing it.
                flowing = [r for r in records if r['kind']==1 and r['bank']==bank and r['flow']
                           and all(r[n]==row[n]for n in PAYLOAD)]
                if flowing:
                    assert len(flowing)==1
                    continue
                matches = [(k,o) for k,o in active.items() if k[0]==bank
                           and all(o['states'][-1][n]==row[n]for n in PAYLOAD)]
                assert len(matches)==1, 'Output is neither unique buffer nor flow'
                key,owner = matches[0]
                assert current[key]['ready'], 'Output without ready buffer owner'
                assert owner['output'] is None, 'Buffer issued twice'
                owner['output'] = row
                releases[*key,cycle] = row
    assert len(current)==16
    observed = {(o['bank'],o['entry'],o['allocation']['core_cycle'])for o in owners if o['allocation']}
    assert all(key in observed or key[2]+1>=end for key in allocations), 'Allocation without owner edge'
    for owner in owners:
        waits = Counter()
        states = owner['states']
        for index,row in enumerate(states):
            finish = states[index+1]['core_cycle'] if index+1<len(states)else owner['end']
            duration = finish-row['core_cycle']
            assert duration>=0
            label = f"rdy{row['ready']}/mp{row['wait_mp']}/ms{row['wait_ms']}"
            waits[label] += duration
        assert sum(waits.values())==owner['end']-owner['begin']
        owner['wait_state_cycles'] = dict(waits)
        owner['closed'] = owner['output'] is not None
    return owners


def analyze(folder, receipt, requests=()):
    folder = Path(folder)
    baseline = writebacks(folder,receipt)
    proof = json.loads((folder/'probe-build-proof.json').read_text())
    assert proof['L2_demand_route_binding']['fields']==FIELDS
    probe = json.loads((folder/'probe-receipt.json').read_text())
    assert 'load-l2-demand-route.csv'in probe['files']
    with(folder/'load-l2-demand-route.csv').open()as stream:
        rows = [{k:int(v)for k,v in r.items()}for r in csv.DictReader(stream)]
    boundary = [(r['core_cycle'],r['clean'],r['native_dump'])for r in rows if r['kind']==0]
    with(folder/'load-mshr-state.csv').open()as stream:
        expected = [(int(r['core_cycle']),int(r['clean']),int(r['native_dump']))
                    for r in csv.DictReader(stream)if r['kind']=='0']
    assert boundary==expected, 'Request route boundaries differ from calibrated L1 observer'
    end = max(p['end_core_inclusive']for p in baseline['phases'])+1
    owners = reconstruct(rows,end)
    blocks = read_blocks(folder/'simulator_err.txt')
    marginals=[]
    for phase in baseline['phases']:
        selected=[r for r in rows if phase['begin_core_exclusive']<r['core_cycle']<=phase['end_core_inclusive']]
        for bank in range(4):
            incoming=[r for r in selected if r['kind']==1 and r['bank']==bank]
            pipe=[r for r in selected if r['kind']==4 and r['bank']==bank and not r['mshr_task']]
            counts={'reqBuf.recv_prefetch':sum(r['ready']and r['opcode']==5 for r in incoming),
                    'reqBuf.recv_normal':sum(r['ready']and r['opcode']!=5 for r in incoming),
                    'reqBuf.req_buffer_alloc':sum(bool(r['alloc_mask'])for r in incoming),
                    'reqBuf.req_buffer_flow':sum(r['flow']for r in incoming),
                    'reqBuf.drop_prefetch':sum(r['duplicate']for r in incoming),
                    'mainPipe.a_req_hit':sum(r['dir_hit']for r in pipe),
                    'mainPipe.a_req_miss':sum(not r['dir_hit']for r in pipe),
                    'mainPipe.acquire_hit':sum(r['dir_hit']and r['opcode']in[6,7]for r in pipe),
                    'mainPipe.acquire_miss':sum(not r['dir_hit']and r['opcode']in[6,7]for r in pipe)}
            for suffix,value in counts.items():
                native,=[v for n,v in blocks[phase['native_dump']].items()
                         if n.endswith(f'.l2cache.slices_{bank}.'+suffix)]
                assert native==[value], ('Route/native mismatch',phase['native_dump'],bank,suffix,value,native)
                marginals.append(dict(native_dump=phase['native_dump'],bank=bank,counter=suffix,value=value))
    requested=[]
    for bank,source,line,lo,hi in requests:
        assert 0<=bank<4 and lo<hi and line&255==bank*64
        selected=[r for r in rows if r['kind']in[1,3,4,5] and r['bank']==bank
                  and r['source_id']==source and lo<=r['core_cycle']<hi and (r['kind']==5 or r['line']==line)]
        cohort=[o for o in owners if o['bank']==bank and o['begin']<hi and o['end']>lo
                and o['states'][0]['source_id']==source and o['states'][0]['line']==line]
        requested.append(dict(bank=bank,source_id=source,line=line,begin=lo,end=hi,
                              address_qualified_request_events=[r for r in selected if r['kind']!=5],
                              source_only_D_events=[r for r in selected if r['kind']==5],
                              buffer_generations=cohort,
                              scope='Exact bank/source and line for address-bearing events; D has source only and retains all beats in the query interval. Dynamic instruction and D ownership require the existing L1 lifetime evidence.'))
    return dict(passed=True,source=baseline['source'],receipt_sha256=digest(Path(receipt)),
                probe_sha256=digest(folder/'load-l2-demand-route.csv'),
                phase_bounds=[{k:p[k]for k in ['begin_core_exclusive','end_core_inclusive','native_dump']}
                              for p in baseline['phases']],native_marginals=marginals,
                buffer_generations=len(owners),closed_buffer_generations=sum(o['closed']for o in owners),
                unfinished_buffer_generations=[o for o in owners if not o['closed']],requested=requested,
                event_counts=dict(Counter(r['kind']for r in rows)),
                scope='Observed buffer entry generations, overlapping mask residence and actual protocol events. No address-only memory join, unique whole-ROI cause, retirement equivalence or recoverable performance claimed.')


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder',type=Path,required=True)
    parser.add_argument('--receipt',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    parser.add_argument('--request',nargs=5,type=lambda s:int(s,0),action='append',default=[],
                        metavar=('BANK','SOURCE','LINE','BEGIN','END'))
    args=parser.parse_args()
    args.output.write_text(json.dumps(analyze(args.folder,args.receipt,args.request),indent=2)+'\n')


if __name__=='__main__':main()
