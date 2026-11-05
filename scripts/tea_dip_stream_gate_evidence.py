"""Validate existing FP gate instrumentation at CST32 before comparing its phase populations."""
from pathlib import Path
import argparse,hashlib,json,sys


def digest(path):
    with path.open('rb') as stream:return hashlib.file_digest(stream,'sha256').hexdigest()


def native_without_instruments(directory, allowed):
    from native_perf import read_blocks,counter_multisets
    return {stamp:counter_multisets({k:v for k,v in block.items() if k not in allowed}) for stamp,block in read_blocks(directory/'simulator_err.txt').items()}


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',type=Path,required=True);args=parser.parse_args();root=args.report_dir.resolve();sys.path.insert(0,str(root))
    from analyze_queue_observation import compare_windows,verify_files
    from analyze_windows import extract
    from analyze_probe_evidence_v2 import analyze
    from analyze_load_replay_wait import NEW_NAMES
    from native_perf import read_blocks,unique_counters
    evidence=root/'evidence/stream-policy-gate-observe';source=json.loads((evidence/'source.json').read_text());spec=json.loads((evidence/'experiment.json').read_text());verify_files(evidence,json.loads((evidence/'source-launch-coremark-manifest.json').read_text()));assert json.loads((evidence/'coremark-neutrality.json').read_text())['passed']
    assert source['compiled_source']=='7efc7832caaf32fda099e53528cc6badc7054cb6' and source['policy_source']=='b4fe7f07ace4e11b5b3672cc48afe57d4dac9d0c' and source['binary_sha256']=='03489138cbd2c5ccc92b6f36ceb721b676f9537ce40ff6bd41c1aac37c547081'
    stage=root/'stream-policy-gate-observation';point=spec['cases'][0]['point'];directory=stage/'gate-observe-v2/points'/point;rows=[]
    if (directory/'result.json').exists():
        receipt_path=stage/'closed-receipts'/(point+'.json');receipt=json.loads(receipt_path.read_text());assert not receipt['original_process_live'] and receipt['exitcode']=='0' and receipt['process']['pid']==3670603 and receipt['process']['start_ticks']=='26336161' and receipt['source']==source['compiled_source'] and receipt['policy_source']==source['policy_source']
        manifest=json.loads((stage/'manifest.json').read_text());prefix='gate-observe-v2/points/'+point+'/';verify_files(stage,{k:v for k,v in manifest.items() if k.startswith(prefix)})
        for n,e in receipt['files'].items():assert manifest[prefix+n]==e
        result=json.loads((directory/'result.json').read_text());assert result['source']==source['compiled_source'] and result['policy_source']==source['policy_source'] and result['binary_sha256']==source['binary_sha256'] and result['cst_sha256']==source['cst_sha256']
        record=json.loads((root/'stream-depth-policy/analysis/mechanism-evidence.json').read_text());policy_row=next(row for row in record['rows'] if row['point']==point);bee_directory=root/'stream-depth-policy/stream-l1-depth32/points'/point;bee_manifest=json.loads((root/'stream-depth-policy/manifest.json').read_text());verify_files(root/'stream-depth-policy',{k:v for k,v in bee_manifest.items() if k.startswith('stream-l1-depth32/points/'+point+'/')})
        gate64=root/'gate-observation-v2/gate-observe-v2/points'/point;gate_manifest=json.loads((root/'gate-observation-v2/manifest.json').read_text());verify_files(root/'gate-observation-v2',{k:v for k,v in gate_manifest.items() if k.startswith('gate-observe-v2/points/'+point+'/')});old_neutral=json.loads((root/'gate-observation-v2/analysis/neutrality.json').read_text());assert next(r for r in old_neutral['rows'] if r['point']==point)['neutral']
        known_gate_names={key for block in read_blocks(gate64/'simulator_err.txt').values() for key in block if key.rsplit('.',1)[-1].startswith('diag_')};known_replay_names={key for block in read_blocks(bee_directory/'simulator_err.txt').values() for key in block if key.rsplit('.',1)[-1] in NEW_NAMES};allowed=known_gate_names|known_replay_names
        windows=[];profiles=[]
        for folder,label in [(bee_directory,'bee4-cst32'),(directory,'7efc-cst32')]:
            command=json.loads((folder/'command.json').read_text());stdout=(folder/'simulator_out.txt').read_text();assert '\nstreamL1Depth0 = 32\n' in stdout
            for k,v in [('-s','4410'),('-W','1000000'),('-I','2000000'),('--diff',spec['reference']),('--dump-select-db','Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0')]:assert command[command.index(k)+1]==v
            window,pc=extract(folder,label,1000);windows.append(window);profiles.append(pc)
        assert windows[0]==policy_row['candidate'] or all(windows[0][k]==policy_row['candidate'][k] for k in ['cycles','instructions','roi_start','roi_end','metrics','exact_tea_interior','counters'])
        equality=compare_windows(*windows,bee_directory,directory,*profiles);old=[native_without_instruments(folder,allowed) for folder in [bee_directory,directory]];equality['checks']['all_old_native_warmup_and_roi_equal']=old[0]==old[1];keys={k for phases in old for block in phases.values() for k in block};equality['changed_old_native_names']=sorted(k for k in keys if [b.get(k) for _,b in sorted(old[0].items())]!=[b.get(k) for _,b in sorted(old[1].items())]);equality['neutral']=all(equality['checks'].values())
        probes=[analyze(folder,root/'evidence/gate-observe-v2/source') for folder in [gate64,directory]];phases=[]
        for side,folder,probe in zip(['control64','candidate32'],[gate64,directory],probes):
            for phase,(_,block) in zip(['warmup','roi'],sorted(read_blocks(folder/'simulator_err.txt').items())):
                counters=unique_counters(block);clock=next(v for k,v in counters.items() if k.endswith('.rob.clock_cycle'));retired=next(v for k,v in counters.items() if k.endswith('.rob.commitInstr'));p=probe['phases'][phase];assert p['diagnostic_availability']=='present' and len(p['fp_issue_stages'])==4
                for queue in p['fp_issue_stages']:
                    e=queue['entry_cycles'];assert e['live']==e['issued']+e['blocked']+e['src_wait']+e['eligible'] and e['live']<=20*clock
                phases.append(dict(side=side,phase=phase,clock_cycles=clock,instructions=retired,gate_histograms=p['gate_histograms'],fp_issue_stages=p['fp_issue_stages'],diagnostic_counters=p['diagnostic_counters']))
        rows.append(dict(point=point,observation_neutrality=equality,allowed_diagnostic_full_names=sorted(allowed),phases=phases,usable_for_mechanism=equality['neutral'],policy_performance=policy_row['comparison'],closed_receipt_sha256=digest(receipt_path),same_binary64_control_source=source['compiled_source']))
    output=dict(completed=len(rows),expected=1,complete=len(rows)==1,rows=rows,compiled_source=source['compiled_source'],policy_source=source['policy_source'],all_collected_observers_neutral=all(r['observation_neutrality']['neutral'] for r in rows) if rows else None,performance_causality_validated=None,consumer_sha256=digest(Path(__file__)),source_sha256={name:digest(root/name) for name in ['analyze_probe_evidence_v2.py','analyze_queue_observation.py','analyze_windows.py','analyze_load_replay_wait.py','native_perf.py']},scope='Existing7efc gate/FP observer on rejected CST32. Validate every old native and all7 execution categories against originalbee4 CST32, excluding only exact full names from the validated64 instrumentation sources; unlisted diagnostics remain protected. Then compare7efc64 vs32 phase populations. Entry-cycles, gate histogram samples and CPU cycles remain separate; no PC/producer identity, criticality, service latency or unique performance causality. No new candidate, suite score or independent repeat.')
    out=stage/'analysis';out.mkdir(parents=True,exist_ok=True);(out/'mechanism-evidence.json').write_text(json.dumps(output,indent=2)+'\n');print(json.dumps(dict(completed=len(rows),neutral=output['all_collected_observers_neutral'],rows=[dict(point=r['point'],usable_for_mechanism=r['usable_for_mechanism']) for r in rows]),indent=2))

if __name__=='__main__':main()
