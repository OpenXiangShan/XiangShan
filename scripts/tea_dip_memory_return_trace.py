"""Explain memory API latency with actual DRAM commands and rank-refresh windows."""
import argparse
from collections import Counter, defaultdict
from pathlib import Path
import json

from tea_dip_l2_return_trace import analyze as l2_analyze, read


def transactions(rows):
    active, epochs, pending = {}, Counter(), defaultdict(list)
    complete, commands = [], []
    controller_channels, previous_clock = {}, {}
    previous_step = -1
    for row in rows:
        assert row['step'] >= previous_step, 'Memory step moved backwards'
        previous_step = row['step']
        kind = row['kind']
        if kind == 0:
            continue
        assert kind in [1,2,3]
        if kind == 3:
            controller, cycle = row['controller'], row['dram_cycle']
            if controller in previous_clock:
                assert cycle >= previous_clock[controller], 'DRAM command clock moved backwards'
            previous_clock[controller] = cycle
            channel = row['channel']
            if channel >= 0:
                assert controller_channels.setdefault(controller,channel)==channel, 'Controller/channel mismatch'
            commands.append(row)
            if row['command'] in [0,1]:
                # The real controller issues one READ for all currently pending
                # reads to this address. Later accepted reads are separate.
                owners = [r for r in active.values() if not r['is_write'] and r['address']==row['address'] and r['read_command'] is None]
                assert owners, 'DRAM READ without an accepted request'
                for owner in owners:
                    owner['read_command'] = row
            continue
        key = row['is_write'], row['id']
        if kind == 1:
            assert key not in active, 'Memory id reused before its response'
            if not row['accepted']:
                pending[key].append(row)
                continue
            epochs[key] += 1
            failures = pending.pop(key,[])
            assert all(r['address']==row['address'] and r['axi_id']==row['axi_id'] for r in failures), 'Retried memory request changed identity'
            active[key] = dict(is_write=row['is_write'],id=row['id'],generation=epochs[key],
                               address=row['address'],axi_id=row['axi_id'],
                               first_attempt=failures[0]['core_cycle']if failures else row['core_cycle'],
                               accepted_core=row['core_cycle'],accepted_step=row['step'],
                               rejected_attempts=len(failures),read_command=None)
        else:
            assert key in active, 'Memory response without an accepted owner'
            owner = active.pop(key)
            if not row['is_write']:
                assert row['address']==owner['address'] and row['axi_id']==owner['axi_id'], 'Memory response/read-tracker identity mismatch'
            assert row['core_cycle'] >= owner['accepted_core'], 'Memory completion before acceptance'
            owner.update(response_core=row['core_cycle'],response_step=row['step'],
                         API_latency=row['core_cycle']-owner['accepted_core'])
            complete.append(owner)
    for command in commands:
        assert command['controller'] in controller_channels, 'DRAM controller has no observed physical channel'
        if command['channel']<0:
            command['resolved_channel']=controller_channels[command['controller']]
        else:
            command['resolved_channel']=command['channel']
    return complete, list(active.values()), commands, sum(map(len,pending.values()))


def analyze(folder, receipt):
    folder=Path(folder)
    result=l2_analyze(folder,receipt)
    proof=json.loads((folder/'probe-build-proof.json').read_text())['memory_return_binding']
    assert proof['command_call_is_external'] and proof['model_objects']==577 and proof['new_RTL_state_bits']==0
    timing=json.loads((folder/'memory-timing.json').read_text())
    stdout=(folder/'simulator_out.txt').read_text()
    assert 'DRAMSIM3 config: '+timing['config_path'] in stdout
    assert f"CPU_FREQ: {timing['CPU_FREQ']} DRAM_FREQ: {timing['DRAM_FREQ']}" in stdout
    assert timing['refresh_policy']=='RANK_LEVEL_STAGGERED'
    assert timing['tRFC']*timing['CPU_FREQ']%timing['DRAM_FREQ']==0
    refresh_core_cycles=timing['tRFC']*timing['CPU_FREQ']//timing['DRAM_FREQ']
    rows=read(folder/'load-memory-events.csv')
    boundary=lambda records:[(r['core_cycle'],r['clean'],r['native_dump'])for r in records if r['kind']==0]
    assert boundary(rows)==boundary(read(folder/'load-l2-mshr.csv')), 'Memory trace boundary mismatch'
    complete, pending, commands, failed_pending=transactions(rows)
    accepted=sum(r['kind']==1 and r['accepted']for r in rows)
    assert accepted==len(complete)+len(pending), 'Memory accepted/response conservation'
    refresh=[r for r in commands if r['command']==7]
    # Actual source calls DRAM clock before the next model step. The captured
    # outside-step command therefore carries the last completed core cycle.
    for owner in complete:
        command=owner['read_command']
        if owner['is_write'] or command is None:
            owner['read_stage_unknown_reason']='write_API_or_no_READ_command_observed'
            continue
        command_core=command['core_cycle']+1
        assert owner['accepted_core']<=command_core<=owner['response_core']
        same_rank=[r for r in refresh if r['resolved_channel']==command['resolved_channel'] and r['rank']==command['rank']]
        overlapping=[r for r in same_rank if r['core_cycle']+1<command_core and owner['accepted_core']<r['core_cycle']+1+refresh_core_cycles]
        # 560 DRAM ticks *3000/1600=1050 core cycles. This is a measured
        # command-window overlap, not a counterfactual speedup or full root cause.
        blocked=sum(max(0,min(command_core,r['core_cycle']+1+refresh_core_cycles)-max(owner['accepted_core'],r['core_cycle']+1))for r in overlapping)
        owner.update(read_command_core=command_core,accepted_to_READ=command_core-owner['accepted_core'],READ_to_API_response=owner['response_core']-command_core,same_rank_refresh_overlap_core_cycles=blocked,overlapping_refresh_commands=overlapping,read_stage_unknown_reason=None)
    for phase in result['phases']:
        lo,hi=phase['begin_core_exclusive']+1,phase['end_core_inclusive']+1
        members=[r for r in complete if lo<=r['response_core']<hi]
        reads=[r for r in members if not r['is_write']]
        measured=[r for r in reads if r['read_command']is not None]
        phase['memory_API']=dict(accepted=sum(r['kind']==1 and r['accepted']and lo<=r['core_cycle']<hi for r in rows),rejected=sum(r['kind']==1 and not r['accepted']and lo<=r['core_cycle']<hi for r in rows),read_responses=len(reads),write_responses=len(members)-len(reads),read_responses_with_actual_READ=len(measured),read_responses_without_observed_READ=len(reads)-len(measured),DRAM_command_counts=dict(Counter(str(r['command'])for r in commands if lo<=r['core_cycle']+1<hi)),completed_read_API_latency_sum=sum(r['API_latency']for r in reads),completed_READ_refresh_overlap_sum=sum(r['same_rank_refresh_overlap_core_cycles']for r in measured),longest_completed_reads=sorted(reads,key=lambda r:r['API_latency'],reverse=True)[:20])
    result.update(memory_timing=timing,memory_accepted=accepted,memory_completed=len(complete),memory_pending=len(pending),memory_pending_rejected_attempts=failed_pending,DRAM_command_events=len(commands),DRAM_refresh_events=len(refresh),memory_transactions=complete,memory_incomplete=pending,DRAM_commands=commands,memory_scope='Actual memory API accepted-id generations, model read-tracker address/AXI identity and issued DRAM READ/REFRESH events. A READ may serve multiple accepted reads; absence of READ is unknown (write forwarding is possible). Command outside-step core labels are normalized using actual emulator order; actual DRAM clocks are preserved. Refresh overlap is not exclusive CPU loss, a counterfactual speedup or proven cross-LLC L2 lineage. No physical-address-only cross-level association.')
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
