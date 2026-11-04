"""Audit same841 LRQ order observations; bank selections are not saved CPU cycles."""
from pathlib import Path
import argparse,hashlib,json,sys

FIELDS=['eligible_banks','multi_offset_banks','order_diff_banks','accept_banks',
        'order_diff_accept_banks','order_diff_stalled_banks','any_accept_cycles','order_diff_accept_cycles']
NEW_NAMES={'diag_lrq_nearhead_'+name for name in FIELDS}
BANKS=3

def old_native_blocks(directory):
    from native_perf import read_blocks,counter_multisets
    return {stamp:counter_multisets({k:v for k,v in block.items() if k.rsplit('.',1)[-1] not in NEW_NAMES})
            for stamp,block in read_blocks(directory/'simulator_err.txt').items()}

def opportunity_block(block):
    from native_perf import unique_counters
    counters=unique_counters(block)
    def value(suffix):return next((v for k,v in counters.items() if k.endswith('.'+suffix)),None)
    values={name:value('diag_lrq_nearhead_'+name) for name in FIELDS};cycles=next((v for k,v in counters.items() if k.endswith('.rob.clock_cycle')),None)
    complete=cycles is not None and all(v is not None for v in values.values());checks=None
    if complete:
        e,m,d,a,da,ds,ac,dc=[values[name] for name in FIELDS]
        checks=dict(nonnegative=cycles>0 and all(v>=0 for v in values.values()),eligible_bank_bound=e<=BANKS*cycles,
                    multi_within_eligible=m<=e,different_within_multi=d<=m,different_partition=da+ds==d,
                    accept_within_eligible=a<=e,different_accept_within_accept=da<=a,
                    accepted_bank_cycle_bounds=ac<=a<=BANKS*ac,different_bank_cycle_bounds=dc<=da<=BANKS*dc,
                    different_cycles_within_accept=dc<=ac<=cycles)
    passed=all(checks.values()) if checks is not None else None
    return dict(all_fields_present=complete,clock_cycles=cycles,values=values,checks=checks,observed_checks_passed=passed,
                accepted_order_difference_observed=values['order_diff_accept_banks']>0 if passed else None,
                scope='s0 selection accept is s1_can_go, before later cancellation and request success. Hardware LQ-pointer offsets, not ROB-head or debug-sequence identity. No criticality or CPI conversion.')

def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',type=Path,required=True);args=parser.parse_args();root=args.report_dir.resolve();sys.path.insert(0,str(root))
    from analyze_queue_observation import verify_files,compare_windows
    from analyze_windows import extract
    from native_perf import read_blocks
    evidence=root/'evidence/lrq-nearhead-observe';stage=root/'lrq-nearhead-observation';variant='lrq-nearhead-observe'
    verify_files(evidence,json.loads((evidence/'source-launch-manifest.json').read_text()));source=json.loads((evidence/'source.json').read_text());proof=json.loads((evidence/'source-resource-check.json').read_text());spec=json.loads((evidence/'experiment.json').read_text())
    assert source['head']==proof['head']=='81222189fcba3d4b753d06e0f45f8a478a4b7097' and source['base']=='84112d0b65231ca690a4ef2fea4b5169f2146e10'
    assert proof['baseline_reversal_proof'] and proof['parameters_and_dependency_gitlinks_unchanged'] and set(source['counter_fields'])==NEW_NAMES
    manifests=[json.loads((root/'raw/manifest.json').read_text()),json.loads((stage/'manifest.json').read_text()) if (stage/'manifest.json').exists() else {}];rows=[]
    for case in spec['cases']:
        point=case['point'];control=root/'raw/control/short'/point;directory=stage/variant/'points'/point
        if not (directory/'result.json').exists():continue
        build=json.loads((evidence/'build-state.json').read_text());smoke=json.loads((evidence/'coremark-audit.json').read_text());verify_files(evidence,json.loads((evidence/'build-coremark-manifest.json').read_text()))
        assert build['phase']=='complete' and build['source']==source['head'] and smoke['passed'] and smoke['binary_sha256']==build['binary_sha256']
        windows=[];profiles=[]
        for label,folder,parent,manifest in [('control',control,root/'raw',manifests[0]),(variant,directory,stage,manifests[1])]:
            prefix=f'{label}/'+('short/' if label=='control' else 'points/')+point+'/'
            files={n:e for n,e in manifest.items() if n.startswith(prefix)}
            assert all(prefix+n in files for n in ['result.json','command.json','exitcode','simulator_out.txt','simulator_err.txt','simulator.db','process.json']);verify_files(parent,files)
            command=json.loads((folder/'command.json').read_text());result=json.loads((folder/'result.json').read_text())
            for flag,v in [('--diff',spec['reference']),('-s',str(case['seed'])),('-W',str(spec['warmup'])),('-I',str(spec['max_instr']))]:assert command[command.index(flag)+1]==v
            assert result['binary_sha256']==(spec['baseline_sha256'] if label=='control' else build['binary_sha256']) and (folder/'exitcode').read_text().strip()=='0'
            w,p=extract(folder,label,spec['period']);assert abs(w['instructions']-1000000)<128 and abs(w['warmup_instructions']-1000000)<128;windows.append(w);profiles.append(p)
        comparison=compare_windows(*windows,control,directory,*profiles);old=[old_native_blocks(d) for d in [control,directory]]
        comparison['unfiltered_native_equal']=comparison['checks']['all_old_native_warmup_and_roi_equal'];comparison['checks']['all_old_native_warmup_and_roi_equal']=old[0]==old[1]
        keys={k for phases in old for block in phases.values() for k in block};comparison['changed_old_native_names']=sorted(k for k in keys if [b.get(k) for _,b in sorted(old[0].items())]!=[b.get(k) for _,b in sorted(old[1].items())]);comparison['neutral']=all(comparison['checks'].values())
        phases=[dict(phase=label,stamp=stamp,**opportunity_block(block)) for label,(stamp,block) in zip(['warmup','roi'],sorted(read_blocks(directory/'simulator_err.txt').items()))]
        rows.append(dict(point=point,comparison=comparison,phases=phases,observed_checks_passed=all(p['observed_checks_passed'] is True for p in phases)))
    result=dict(completed=len(rows),expected=len(spec['cases']),complete=len(rows)==len(spec['cases']),rows=rows,
                all_collected_execution_pairs_neutral=all(r['comparison']['neutral'] for r in rows) if rows else None,
                all_collected_opportunity_checks_passed=all(r['observed_checks_passed'] for r in rows) if rows else None,
                source_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),performance_causality_validated=None,
                scope='Same841 observer only; exactly8 new names ignored. All old native multivalues and six functional categories protected. Different order at accepted bank selections may be cancelled later and may be noncritical. No performance candidate score or unique CPI cause.')
    out=stage/'analysis';out.mkdir(parents=True,exist_ok=True);(out/'neutrality.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps({k:result[k] for k in ['completed','expected','all_collected_execution_pairs_neutral','all_collected_opportunity_checks_passed','performance_causality_validated']},indent=2))

if __name__=='__main__':main()
