"""Replay bank prefetch FIFO generations, overwritten victims and port delivery."""
import argparse
from collections import Counter, deque
import csv
from itertools import groupby
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_gsim_prefetch_queue_probe import FIELDS
from tea_dip_pbop_issue_trace import replay as replay_pbop
from tea_dip_query_endpoint_evidence import digest, verify


def payload(row, stage):
    return tuple(row[stage + '_' + name] for name in ['line', 'source_id', 'req_source', 'need_t'])


def replay(path):
    queues, pipes = [deque() for _ in range(4)], [None] * 4
    states, counts = [None] * 4, [[0] * 4 for _ in range(4)]
    records, boundaries, port_waits = [], [], Counter()
    previous, final_banks = None, set()
    with Path(path).open() as stream:
        reader = csv.DictReader(stream)
        assert reader.fieldnames == ['core_cycle', 'clean', 'native_dump', 'kind', 'bank', *FIELDS]
        rows = ({k: int(v) for k, v in row.items()} for row in reader)
        for cycle, group in groupby(rows, lambda r:r['core_cycle']):
            assert previous is None or cycle > previous, 'Prefetch queue cycles are unordered'
            previous = cycle
            seen = set()
            for row in group:
                bank, kind = row['bank'], row['kind']
                assert 0 <= bank < 4 and kind in [0, 1, 2]
                assert (bank,kind) not in seen and bank not in final_banks
                seen.add((bank,kind))
                queue = queues[bank]
                if kind == 1:
                    h,t,m = [row[n] for n in ['head','tail','maybe_full']]
                    assert 0 <= h < 32 and 0 <= t < 32 and m in [0,1]
                    if states[bank] is None:
                        assert h == t and not m and not row['pipe_valid'], 'Capture starts after live owners'
                    else:
                        assert [h,t,m] == states[bank], 'Prefetch FIFO pointers lost a transition'
                    empty, full = not queue, len(queue)==32
                    assert empty == (h==t and not m) and full == (h==t and bool(m))
                    if queue:
                        assert payload(row,'head') == tuple(queue[0]['payload']), 'FIFO head payload changed'
                    assert bool(row['pipe_valid']) == (pipes[bank] is not None), 'Pipeline owner lost'
                    assert row['prefetch_ready'] == int(bool(row['reqbuf_ready'] and not row['a_valid']))
                    assert row['pipe_ready'] == int(bool(row['prefetch_ready'] or not row['pipe_valid']))
                    if pipes[bank] is not None:
                        assert payload(row,'pipe') == tuple(pipes[bank]['payload']), 'Pipeline payload changed'
                        if row['prefetch_ready']:
                            pipes[bank].update(port_cycle=cycle, outcome='port_fire')
                            pipes[bank] = None
                            counts[bank][3] += 1
                        else:
                            port_waits[(bank, row['a_valid'], row['reqbuf_ready'])] += 1
                    enqueue = bool(row['enqueue'])
                    deq = bool((not empty or enqueue) and row['pipe_ready'])
                    flow = bool(empty and enqueue and row['pipe_ready'])
                    overwrite = bool(full and enqueue and not deq)
                    assert row['flow'] == flow and row['overwrite'] == overwrite
                    new_owner = None
                    if enqueue:
                        value = payload(row,'input')
                        assert value[0] % 64 == 0 and (value[0]>>6)&3 == bank
                        assert 0 <= value[1] < 256 and 0 <= value[2] < 32 and value[3] in [0,1]
                        new_owner = dict(bank=bank, generation=counts[bank][0], enqueue_cycle=cycle,
                                         payload=list(value), outcome='pending_FIFO')
                        records.append(new_owner)
                        counts[bank][0] += 1
                    if deq:
                        owner = new_owner if flow else queue.popleft()
                        assert owner is not None and pipes[bank] is None
                        owner.update(dequeue_cycle=cycle, flow=flow, outcome='pending_pipeline')
                        pipes[bank] = owner
                        counts[bank][1] += 1
                    if overwrite:
                        victim = queue.popleft()
                        victim.update(overwrite_cycle=cycle, overwriter_generation=new_owner['generation'],
                                      overwriter_payload=new_owner['payload'], outcome='overwritten')
                        counts[bank][2] += 1
                    if enqueue and not flow:
                        queue.append(new_owner)
                    states[bank] = [(h+int(deq or overwrite))%32, (t+int(enqueue))%32,
                                    1 if overwrite else int(enqueue) if enqueue != deq else m]
                    assert len(queue) <= 32
                elif kind == 0:
                    boundaries.append(row)
                else:
                    final_banks.add(bank)
                assert [row[n] for n in FIELDS[-4:]] == counts[bank], 'Prefetch event footer lost records'
    assert final_banks == set(range(4)), 'Missing closed prefetch queue footer'
    outcomes = Counter(r['outcome'] for r in records)
    assert sum(outcomes.values()) == sum(c[0] for c in counts)
    return dict(passed=True, records=records, counts=counts, outcomes=dict(outcomes), boundaries=boundaries,
                right_censored_FIFO=[len(q) for q in queues],
                right_censored_pipeline=[int(p is not None) for p in pipes],
                port_blocked_bank_cycles=[dict(bank=k[0],a_valid=k[1],reqbuf_ready=k[2],cycles=v)
                                          for k,v in sorted(port_waits.items())])


def analyze(folder, receipt_path):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder/'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and result['original_process_closed']
    assert result['exitcode']==0 and result['difftest_passed'] and all(result['sampler_flags'].values())
    assert receipt['source']==result['source']
    verify(folder,receipt['files'])
    probe=json.loads((folder/'probe-receipt.json').read_text())
    assert probe['closure_receipt_sha256']==digest(receipt_path)
    proof=json.loads((folder/'probe-build-proof.json').read_text())
    assert proof['passed'] and proof['original_files_unchanged']
    assert proof['binary_sha256']==result['binary_sha256']
    assert proof['prefetch_queue_binding']['fields']==FIELDS
    output=replay(folder/'prefetch-queue.csv')
    pbop=replay_pbop(folder/'pbop-train-issue.csv')
    observed=sorted((r['enqueue_cycle'],r['bank'],r['payload'][0],r['payload'][1],r['payload'][3])
                    for r in output['records'] if r['payload'][2]==9)
    expected=sorted((r['exit_cycle'],(r['target_line']>>6)&3,r['target_line'],r['source_id'],r['need_t'])
                    for r in pbop['records'] if r['outcome']=='emitted')
    assert observed==expected, 'PBOP fire-to-FIFO payload or event mismatch'
    port_observed=sorted((r['port_cycle'],r['bank'],r['payload'][0],r['payload'][1],r['payload'][3])
                         for r in output['records'] if r['payload'][2]==9 and 'port_cycle' in r)
    port_expected=[]
    with (folder/'load-l2-demand-route.csv').open() as stream:
        for row in csv.DictReader(stream):
            if int(row['kind'])==1 and int(row['req_source'])==9 and int(row['ready']):
                assert int(row['opcode'])==5 and int(row['param']) in [0,1]
                port_expected.append((int(row['core_cycle']),int(row['bank']),int(row['line']),
                                      int(row['source_id']),int(row['param'])))
    assert port_observed==sorted(port_expected), 'PBOP port-to-ReqBuf accepted payload mismatch'
    dumps=sorted({r['core_cycle'] for r in output['boundaries'] if r['native_dump']})
    frames=read_blocks(folder/'simulator_err.txt')
    assert len(dumps)==len(frames)
    margins,previous=[], -1
    for (stamp,block),end in zip(sorted(frames.items()),dumps):
        enqueued=[r for r in output['records'] if previous < r['enqueue_cycle'] <= end]
        by_source=Counter(r['payload'][2] for r in enqueued)
        selectors={name:next(v for k,v in block.items() if k.endswith('.prefetcher.prefetch_req_select'+name))
                   for name in ['L1','VBOP','PBOP','TP','NL','CDP']}
        assert all(len(v)==1 for v in selectors.values())
        assert len(enqueued)==sum(v[0] for v in selectors.values()), 'Native enqueue total mismatch'
        assert by_source[9]==selectors['PBOP'][0], 'Native PBOP enqueue mismatch'
        port_counts=[]
        for bank in range(4):
            count=sum(r['bank']==bank and 'port_cycle' in r and previous < r['port_cycle'] <= end
                      for r in output['records'])
            actual,= [v for k,v in block.items() if k.endswith(f'.slices_{bank}.sinkA.sinkA_prefetch_req')]
            assert actual==[count], ('Native prefetch port mismatch',stamp,bank,count,actual)
            port_counts.append(count)
        margins.append(dict(native_stamp=stamp,source_cycle_end=end,enqueues=len(enqueued),
                            by_source=dict(by_source),native_selectors=selectors,port_fires=port_counts))
        previous=end
    output.update(source=receipt['source'],native_phase_marginals=margins,
        PBOP_engine_to_FIFO_payloads_equal=len(observed),
        PBOP_port_to_ReqBuf_payloads_equal=len(port_observed),
        tool_sha256={name:digest(Path(__file__).parent/name) for name in
                     ['tea_dip_prefetch_queue_trace.py','tea_dip_gsim_prefetch_queue_probe.py']},
        scope='Exact FIFO order/payload generations through overwrite/dequeue/pipeline/port fire. '
              'Full capture includes warmup; right-censored owners remain. Port fire does not prove S3, '
              'useful data or exclusive CPU savings. Counts and blocked bank-cycles cannot be added to TEA/DIP.')
    return output


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder',type=Path,required=True)
    parser.add_argument('--receipt',type=Path,required=True)
    parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args()
    result=analyze(args.folder,args.receipt)
    assert not args.output.exists()
    args.output.parent.mkdir(parents=True,exist_ok=True)
    args.output.write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps({k:result[k] for k in ['passed','counts','outcomes','PBOP_engine_to_FIFO_payloads_equal']}))


if __name__=='__main__':
    main()
