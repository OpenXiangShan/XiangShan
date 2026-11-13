"""Reconstruct actual L2 owner generations and FIFO TXREQ-to-RXDAT stages."""
import argparse
from bisect import bisect_right
from collections import Counter, defaultdict, deque
import csv
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_gsim_l2_return_probe import FIELDS
from tea_dip_mshr_return_trace import analyze as l1_analyze


def read(path):
    with Path(path).open() as stream:
        return [{k: int(v) for k, v in row.items()} for row in csv.DictReader(stream)]


def owners(rows, end):
    current, active, epochs, records = {}, {}, Counter(), defaultdict(list)
    allocations, returns = [], []
    allocation_cycles, observed_edges = set(), set()
    previous_cycle = -1
    for row in rows:
        cycle, kind, bank, slot = [row[k] for k in ['core_cycle','kind','bank','mshr']]
        assert cycle >= previous_cycle
        previous_cycle = cycle
        if kind == 0:
            continue
        key = bank, slot
        assert kind in [1,2,3] and 0 <= bank < 4 and 0 <= slot < 16
        if kind == 3:
            assert not row['valid'], 'L2 allocation into a valid owner'
            allocations.append(row)
            allocation_cycles.add((bank,slot,cycle))
            continue
        if kind == 2:
            assert key in active and row['valid'], 'L2 RXDAT without owner'
            owner = active[key]
            assert all(row[k] == current[key][k] for k in FIELDS), 'L2 RXDAT/state mismatch'
            assert row['rxdat_data_id'] in [0,2], 'Unqualified L2 RXDAT beat'
            assert all(r['core_cycle'] != cycle for r in owner['returns']), 'Duplicate L2 RXDAT'
            owner['returns'].append(row)
            returns.append(row)
            continue
        if key in current:
            assert current[key]['core_cycle'] < cycle, 'Duplicate L2 state cycle'
        old = current.get(key)
        if row['valid']:
            assert row['line'] & 255 == bank * 64, 'L2 bank/address mismatch'
            if old is None or not old['valid']:
                epochs[key] += 1
                owner = dict(bank=bank,slot=slot,generation=epochs[key],begin=cycle,end=end,
                             line=row['line'],allocation_observed=old is not None,states=[],returns=[],TXREQ=[])
                if old is not None:
                    assert (bank,slot,cycle-1) in allocation_cycles, 'L2 valid edge without allocation'
                    observed_edges.add((bank,slot,cycle-1))
                active[key] = owner
                records[key].append(owner)
            else:
                assert row['line'] == active[key]['line'], 'L2 owner line changed'
                if row['first'] and not old['first'] and row['has_data']:
                    assert any(r['core_cycle']==cycle-1 for r in active[key]['returns']), 'L2 return flag without RXDAT'
            active[key]['states'].append(row)
        elif key in active:
            active.pop(key)['end'] = cycle
        current[key] = row
    assert len(current) == 64, 'Missing L2 slot snapshots'
    for allocation in allocations:
        assert (allocation['bank'],allocation['mshr'],allocation['core_cycle']) in observed_edges, 'L2 allocation has no valid edge'
    for entries in records.values():
        for owner in entries:
            rs = owner['returns']
            if rs:
                assert len(rs) <= 2 and len({r['rxdat_data_id']for r in rs}) == len(rs), 'L2 RXDAT beat reused'
            cycles=[r['core_cycle']for r in owner['states']]
            for response in rs:
                following=response['core_cycle']+1
                if following>=end:
                    continue
                state=owner['states'][bisect_right(cycles,following)-1]
                assert state['first'] and state['has_data'], 'L2 RXDAT missing next data state'
            owner['first_data'] = rs[0]['core_cycle'] if rs else None
            owner['last_data'] = rs[-1]['core_cycle'] if len(rs)==2 else None
    return dict(records), allocations, returns


def queues(rows):
    previous, pending, result = {}, defaultdict(deque), []
    groups = defaultdict(list)
    last = -1
    for row in rows:
        assert row['core_cycle'] >= last
        last = row['core_cycle']
        if row['kind']:
            assert 0 <= row['bank'] < 4 and row['kind'] in [1,2,3]
            groups[row['core_cycle'],row['bank']].append(row)
    for (cycle,bank), group in groups.items():
        states = [r for r in group if r['kind']==1]
        assert len(states)==1, 'Missing TXREQ state'
        state = states[0]
        before=[state[k]for k in ['enq_ptr','deq_ptr','full']]
        after=[state[k]for k in ['next_enq_ptr','next_deq_ptr','next_full']]
        count=lambda p: (16 if p[2]else 0)if p[0]==p[1]else((p[0]-p[1])&15)
        assert count(before)==state['count']and count(after)==state['next_count']
        if bank in previous:
            assert before==previous[bank], 'TXREQ next pointer did not commit'
        else:
            assert not state['count'], 'TXREQ started with unknown queued members'
        enq=(after[0]-before[0])&15;deq=(after[1]-before[1])&15
        assert enq<=1 and deq<=1 and state['count']+enq==state['next_count']+deq
        incoming=[r for r in group if r['kind']==2];outgoing=[r for r in group if r['kind']==3]
        assert len(incoming)==enq and len(outgoing)==deq, 'TXREQ pointer/event mismatch'
        for row in group:
            assert all(row[k]==state[k]for k in ['core_cycle','bank','enq_ptr','deq_ptr','full','next_enq_ptr','next_deq_ptr','next_full','count','next_count'])
        # flow=false: dequeue is always an older queued request.
        if outgoing:
            assert pending[bank], 'TXREQ dequeue without queued owner'
            start=pending[bank].popleft();finish=outgoing[0]
            assert all(start[k]==finish[k]for k in ['txn_id','opcode','line']), 'TXREQ FIFO member mismatch'
            result.append(dict(bank=bank,txn_id=finish['txn_id'],opcode=finish['opcode'],line=finish['line'],enqueue=start['core_cycle'],dequeue=cycle,queue_cycles=cycle-start['core_cycle']))
        pending[bank].extend(incoming)
        assert len(pending[bank])==state['next_count'], 'TXREQ FIFO occupancy mismatch'
        previous[bank]=after
    assert set(previous)==set(range(4))
    return result, {bank:list(q)for bank,q in pending.items()}


def analyze(folder, receipt):
    folder=Path(folder)
    result=l1_analyze(folder,receipt)
    binding=json.loads((folder/'probe-build-proof.json').read_text())['L2_return_binding']
    assert binding['banks']==4 and binding['entries_per_bank']==16 and binding['TXREQ_entries']==16
    rows, qrows=[read(folder/name)for name in ['load-l2-mshr.csv','load-l2-txreq.csv']]
    boundary=lambda records:[(r['core_cycle'],r['clean'],r['native_dump'])for r in records if r['kind']==0]
    assert boundary(rows)==boundary(qrows)==boundary(read(folder/'load-mshr-state.csv')), 'L2 trace boundary mismatch'
    end=max(p['end_core_inclusive']for p in result['phases'])+1
    records,allocations,returns=owners(rows,end)
    transactions,pending=queues(qrows)
    unmatched=[]
    for tx in transactions:
        entries=records.get((tx['bank'],tx['txn_id']),[])
        index=bisect_right([o['begin']for o in entries],tx['enqueue'])-1
        owner=entries[index]if index>=0 and tx['enqueue']<entries[index]['end']else None
        if owner is not None and owner['line']==tx['line']:
            owner['TXREQ'].append(tx)
        else:
            unmatched.append(tx)
    blocks=read_blocks(folder/'simulator_err.txt')
    native_marginals=0
    for phase in result['phases']:
        lo,hi=phase['begin_core_exclusive']+1,phase['end_core_inclusive']+1
        counts=[]
        for bank in range(4):
            histogram=Counter(r['mshr']for r in allocations if r['bank']==bank and lo<=r['core_cycle']<hi)
            for slot in range(16):
                suffix=f'.l2cache.slices_{bank}.mshrCtl.mshr_alloc_{slot}_{slot+1}'
                values,=[value for name,value in blocks[phase['native_dump']].items()if name.endswith(suffix)]
                assert values==[histogram[slot]], ('L2 allocation/native mismatch',phase['native_dump'],bank,slot,values,histogram[slot])
                counts.append(dict(bank=bank,mshr=slot,allocations=histogram[slot]))
                native_marginals+=1
        phase['L2_native_allocations']=counts
        phase['L2_RXDAT_events']=sum(lo<=r['core_cycle']<hi for r in returns)
        phase['L2_TXREQ_dequeues']=sum(lo<=r['dequeue']<hi for r in transactions)
    result.update(L2_generations=sum(map(len,records.values())),L2_allocation_events=len(allocations),
                  L2_RXDAT_events=len(returns),L2_TXREQ_transactions=len(transactions),
                  L2_native_allocation_marginals=native_marginals,
                  L2_TXREQ_without_matching_request_owner=unmatched,L2_pending_TXREQ={str(k):v for k,v in pending.items()},
                  L2_owners=[o for entries in records.values()for o in entries],
                  L2_scope='Actual per-bank allocation/RXDAT owners and FIFO TXREQ handshakes. Nonmatching/release/pipeline addresses stay unmatched. Stage records do not identify DRAM alone, retirement, or exclusive CPU loss; cross-L1 source binding is separate.')
    return result


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder',type=Path,required=True)
    parser.add_argument('--receipt',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args()
    result=analyze(args.folder,args.receipt)
    args.output.write_text(json.dumps(result,indent=2)+'\n')


if __name__=='__main__':
    main()
