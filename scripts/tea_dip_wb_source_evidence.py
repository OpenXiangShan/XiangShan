"""Audit existing eligible-entry WB conflicts by port, source and FU without changing policy."""
from pathlib import Path
import argparse,hashlib,json,sys


def digest(path):
    with path.open('rb') as stream:return hashlib.file_digest(stream,'sha256').hexdigest()


def opportunity_block(block,schema):
    from native_perf import unique_counters
    counters=unique_counters(block);suffix='.diag_fp_wb_source_raw_blocked_entry_cycles';prefixes=sorted(k[:-len(suffix)] for k in counters if k.endswith(suffix));clock=next((v for k,v in counters.items() if k.endswith('.rob.clock_cycle')),None);units=[]
    for prefix in prefixes:
        index=int(prefix.rsplit('issueQueues',1)[1].removeprefix('_') or '0');unit_schema=schema[index];values={name:counters.get(prefix+'.'+name) for name in unit_schema['fields']};old_blocked=counters.get(prefix+'.diag_fp_wb_conflict_entry_cycles');old_eligible=counters.get(prefix+'.diag_fp_eligible_entry_cycles');present=clock is not None and old_blocked is not None and old_eligible is not None and all(v is not None for v in values.values());checks=None;classification=None
        if present:
            def count(name):return values['diag_fp_wb_source_'+name+'_entry_cycles']
            raw,live,eligible,unknown=[count(n) for n in ['raw_blocked','live_blocked','live_eligible','unclassified_fu']];cells={source:{fu:count(source+'_'+fu) for fu in unit_schema['fu_types']} for source in unit_schema['source_groups']}
            checks=dict(nonnegative=clock>0 and all(v>=0 for v in values.values()),raw_matches_old_wb_conflict=raw==old_blocked,live_eligible_matches_old=eligible==old_eligible,live_within_raw_and_eligible=live<=raw and live<=eligible,entry_cycle_bounds=raw<=20*clock and eligible<=20*clock and all(v<=live for name,v in values.items() if name not in ['diag_fp_wb_source_raw_blocked_entry_cycles','diag_fp_wb_source_live_eligible_entry_cycles']),source_fu_partition=sum(v for by_fu in cells.values() for v in by_fu.values())+unknown==live);classification=unknown==0
        else:cells=None
        units.append(dict(unit=prefix,queue=index,values=values,cells=cells,old_wb_conflict=old_blocked,old_eligible=old_eligible,all_fields_present=present,checks=checks,observed_checks_passed=all(checks.values()) if checks else None,classification_complete=classification))
    present=len(units)==4 and all(u['all_fields_present'] for u in units)
    return dict(clock_cycles=clock,units=units,all_fields_present=present,observed_checks_passed=all(u['observed_checks_passed'] is True for u in units) if present else None,classification_complete=all(u['classification_complete'] is True for u in units) if present else None,scope='Per physical FP issue queue entry-cycles. Raw reproduces old pre-minus-post WB count; live excludes needFlush. Source/FU exclusive cells plus unclassified reconstruct live blocked. No source/destination PC, dynamic producer edge, CPU criticality, service time or exclusive CPI.')


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',type=Path,required=True);args=parser.parse_args();root=args.report_dir.resolve();sys.path.insert(0,str(root))
    from analyze_queue_observation import verify_files,compare_windows
    from analyze_windows import extract
    from native_perf import read_blocks,counter_multisets
    evidence=root/'evidence/fp-wb-source-observe';source=json.loads((evidence/'source.json').read_text());verify_files(evidence,json.loads((evidence/'source-launch-manifest.json').read_text()));assert source['head']=='7df1bde530e640f5a6a976c11ac48fc244f9ae06' and source['base']=='7efc7832caaf32fda099e53528cc6badc7054cb6' and source['baseline_reversal_proof'] and source['new_physical_accumulators']==68
    names=set(source['new_exact_counter_names']);stage=root/'fp-wb-source-observation';manifest=json.loads((stage/'manifest.json').read_text()) if (stage/'manifest.json').exists() else {};rows=[];expected=0
    for variant in ['fp-wb-source-observe','fp-wb-source-observe-depth32']:
        spec=json.loads((evidence/'stage'/variant/'experiment.json').read_text());expected+=len(spec['cases']);depth=spec['stream_depth'];control_stage=root/('gate-observation-v2' if depth==64 else 'stream-policy-gate-observation');control_manifest=json.loads((control_stage/'manifest.json').read_text())
        for case in spec['cases']:
            point=case['point'];directory=stage/variant/'points'/point
            if not(directory/'result.json').exists():continue
            receipt_path=stage/'closed-receipts'/(variant+'-'+point+'.json');receipt=json.loads(receipt_path.read_text());assert not receipt['original_process_live'] and receipt['exitcode']=='0' and receipt['source']==source['head'] and receipt['policy_source']==spec['policy_source']
            prefix=variant+'/points/'+point+'/';entries={k:v for k,v in manifest.items() if k.startswith(prefix)};verify_files(stage,entries)
            for n,e in receipt['files'].items():assert manifest[prefix+n]==e
            result=json.loads((directory/'result.json').read_text());assert result['source']==source['head'] and result['policy_source']==spec['policy_source'] and result['cst_sha256']==digest(evidence/'stage'/variant/'tea-dip-period1000.cst')
            build=json.loads((evidence/'build-state.json').read_text());assert build['phase']=='complete' and build['source']==source['head'] and result['binary_sha256']==build['binary_sha256']
            control=control_stage/'gate-observe-v2/points'/point;control_identity=json.loads((control/'result.json').read_text());assert control_identity['source']==source['base'] and control_identity['binary_sha256']=='03489138cbd2c5ccc92b6f36ceb721b676f9537ce40ff6bd41c1aac37c547081'
            prefix_control='gate-observe-v2/points/'+point+'/';verify_files(control_stage,{k:v for k,v in control_manifest.items() if k.startswith(prefix_control)})
            windows=[];profiles=[]
            for folder,label in [(control,'control7efc'),(directory,variant)]:
                command=json.loads((folder/'command.json').read_text());stdout=(folder/'simulator_out.txt').read_text();assert '\nstreamL1Depth0 = '+str(depth)+'\n' in stdout
                for flag,value in [('-s',str(case['seed'])),('-W','1000000'),('-I','2000000'),('--diff',spec['reference']),('--dump-select-db','Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0')]:assert command[command.index(flag)+1]==value
                window,pc=extract(folder,label,1000);assert abs(window['instructions']-1000000)<128 and abs(window['warmup_instructions']-1000000)<128;windows.append(window);profiles.append(pc)
            equality=compare_windows(*windows,control,directory,*profiles)
            def old(folder):return {stamp:counter_multisets({k:v for k,v in block.items() if k.rsplit('.',1)[-1] not in names}) for stamp,block in read_blocks(folder/'simulator_err.txt').items()}
            a,b=old(control),old(directory);equality['checks']['all_old_native_warmup_and_roi_equal']=a==b;keys={k for group in [a,b] for block in group.values() for k in block};equality['changed_old_native_names']=sorted(k for k in keys if [x.get(k) for _,x in sorted(a.items())]!=[x.get(k) for _,x in sorted(b.items())]);equality['neutral']=all(equality['checks'].values())
            phases=[dict(phase=name,stamp=stamp,**opportunity_block(block,source['queue_schema'])) for name,(stamp,block) in zip(['warmup','roi'],sorted(read_blocks(directory/'simulator_err.txt').items()))]
            rows.append(dict(variant=variant,depth=depth,point=point,comparison=equality,phases=phases,observed_checks_passed=all(p['observed_checks_passed'] is True for p in phases),classification_complete=all(p['classification_complete'] is True for p in phases),closed_receipt_sha256=digest(receipt_path)))
    result=dict(completed=len(rows),expected=expected,complete=len(rows)==expected,rows=rows,all_collected_execution_pairs_neutral=all(r['comparison']['neutral'] for r in rows) if rows else None,all_collected_partition_checks_passed=all(r['observed_checks_passed'] for r in rows) if rows else None,all_collected_classifications_complete=all(r['classification_complete'] for r in rows) if rows else None,performance_causality_validated=None,consumer_sha256=digest(Path(__file__)),scope='Diagnostic observer only. Each policy checked against original closed7efc at the same CST64/32; exactly35 new counter names excepted, every old native and seven execution categories protected. Missing fields unknown; bad partitions/classification preserved. Source/FU populations are entry-cycles, not critical CPU losses or optimization gains; no suite score.')
    out=stage/'analysis';out.mkdir(parents=True,exist_ok=True);(out/'neutrality.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps({k:result[k] for k in ['completed','expected','all_collected_execution_pairs_neutral','all_collected_partition_checks_passed','performance_causality_validated']},indent=2))

if __name__=='__main__':main()
