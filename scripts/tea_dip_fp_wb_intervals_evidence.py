"""Audit same-cohort FP-pipe WB intervals without reinterpreting old ROB sums."""
from pathlib import Path
import argparse,hashlib,json,sys

SUM_NAMES=['diag_fp_wb_iq_to_select_sum','diag_fp_wb_select_to_ex0_sum','diag_fp_wb_ex0_to_wb_sum','diag_fp_wb_iq_to_wb_sum']
CHECKED='.diag_fp_timestamp_checked_wb'
FAILED='.diag_fp_timestamp_order_fail'


def old_native_blocks(directory):
    from native_perf import read_blocks,counter_multisets
    return {stamp:counter_multisets({key:values for key,values in block.items()
            if key.rsplit('.',1)[-1] not in SUM_NAMES})
            for stamp,block in read_blocks(directory/'simulator_err.txt').items()}


def interval_block(block):
    from native_perf import unique_counters
    counters=unique_counters(block)
    prefixes=sorted(key[:-len(CHECKED)] for key in counters if key.endswith(CHECKED))
    expected={prefix+'.'+name for prefix in prefixes for name in SUM_NAMES}
    observed={key for key in counters if key.rsplit('.',1)[-1] in SUM_NAMES}
    complete=len(prefixes)==4 and expected==observed and all(prefix+FAILED in counters for prefix in prefixes)
    pipes=[]
    for prefix in prefixes:
        checked=counters[prefix+CHECKED];failures=counters.get(prefix+FAILED)
        population=checked-failures if failures is not None else None
        sums={name:counters.get(prefix+'.'+name) for name in SUM_NAMES}
        present=all(value is not None for value in sums.values())
        nonnegative=(population>=0 and all(value>=0 for value in sums.values())) if present and population is not None else None
        conserved=(sum(sums[name] for name in SUM_NAMES[:3])==sums[SUM_NAMES[3]]) if present else None
        empty_zero=(population!=0 or all(value==0 for value in sums.values())) if present and population is not None else None
        pipes.append(dict(pipe=prefix,checked_wb=checked,order_failures=failures,ordered_wb=population,
                          interval_sums=sums,nonnegative=nonnegative,subinterval_sum_equals_total=conserved,
                          empty_cohort_sums_zero=empty_zero,observed_wb_population=population is not None and population>0))
    has_events=sum(row['checked_wb'] for row in pipes)>0
    ordered=(all(row['order_failures']==0 for row in pipes)) if complete and has_events else None
    numeric=(all(row['nonnegative'] and row['subinterval_sum_equals_total'] and row['empty_cohort_sums_zero'] for row in pipes)) if complete else None
    return dict(all_four_pipe_fields_present=complete,observed_fp_writebacks=has_events,
                all_checked_writebacks_ordered=ordered,observed_interval_checks_passed=numeric,pipes=pipes)


def compare_interval_windows(a,b,ad,bd,ap,bp):
    from analyze_queue_observation import compare_windows
    result=compare_windows(a,b,ad,bd,ap,bp)
    result['unfiltered_native_equal']=result['checks']['all_old_native_warmup_and_roi_equal']
    old=[old_native_blocks(directory) for directory in [ad,bd]]
    result['checks']['all_old_native_warmup_and_roi_equal']=old[0]==old[1]
    names=sorted({key for phases in old for block in phases.values() for key in block})
    result['changed_old_native_names']=[key for key in names if
        [block.get(key) for stamp,block in sorted(old[0].items())] !=
        [block.get(key) for stamp,block in sorted(old[1].items())]]
    result['neutral']=all(result['checks'].values())
    return result


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir',type=Path,required=True);args=parser.parse_args()
    root=args.report_dir.resolve();sys.path.insert(0,str(root))
    from analyze_queue_observation import verify_files
    from analyze_windows import extract
    from native_perf import read_blocks
    variant='fp-wb-intervals-observe';parent='fp-timestamps-observe'
    evidence=root/'evidence'/variant;stage=root/'fp-wb-interval-observation'
    parent_evidence=root/'evidence'/parent;parent_stage=root/'fp-timestamp-observation'
    verify_files(evidence,json.loads((evidence/'source-launch-manifest.json').read_text()))
    source=json.loads((evidence/'source.json').read_text());proof=json.loads((evidence/'source-resource-check.json').read_text())
    launch=json.loads((evidence/'launch.json').read_text());spec=json.loads((evidence/'experiment.json').read_text())
    assert source['head']==proof['head']==launch['source']=='6a91bc8006ab084c87c22585fbf63a751f86aeb4'
    assert source['parent']==json.loads((parent_evidence/'source.json').read_text())['head'] and proof['parameters_and_queue_identical_to_parent']
    assert spec['controls']=={variant:parent}
    manifest=json.loads((stage/'manifest.json').read_text()) if (stage/'manifest.json').exists() else {}
    parent_manifest=json.loads((parent_stage/'manifest.json').read_text());rows=[]
    for case in spec['cases']:
        point=case['point'];directory=stage/variant/'points'/point
        if not (directory/'result.json').exists():continue
        parent_directory=parent_stage/parent/'points'/point
        for folder,name,entries in [(stage,variant,manifest),(parent_stage,parent,parent_manifest)]:
            prefix=f'{name}/points/{point}/';files={key:value for key,value in entries.items() if key.startswith(prefix)}
            assert all(prefix+name in files for name in ['command.json','result.json','exitcode','simulator_out.txt','simulator_err.txt','simulator.db','probe-evidence.json'])
            verify_files(folder,files)
        windows=[];profiles=[]
        for name,folder,identity in [(parent,parent_directory,parent_evidence),(variant,directory,evidence)]:
            verify_files(identity,json.loads((identity/'build-coremark-manifest.json').read_text()))
            build=json.loads((identity/'build-state.json').read_text());smoke=json.loads((identity/'coremark-audit.json').read_text())
            result=json.loads((folder/'result.json').read_text());command=json.loads((folder/'command.json').read_text())
            assert build['phase']=='complete' and smoke['passed'] and smoke['binary_sha256']==build['binary_sha256']==result['binary_sha256']
            assert build['source']==result['source'] and build['binary'] in command
            assert result['source']==(source['head'] if name==variant else source['parent'])
            for flag,value in [('--diff',spec['reference']),('-W',str(spec['warmup'])),('-I',str(spec['max_instr'])),('-s',str(case['seed']))]:assert command[command.index(flag)+1]==value
            assert (folder/'exitcode').read_text().strip()=='0'
            window,profile=extract(folder,name,spec['period'])
            assert abs(window['warmup_instructions']-spec['warmup'])<128 and abs(window['instructions']-(spec['max_instr']-spec['warmup']))<128 and window['seed']==case['seed']
            windows.append(window);profiles.append(profile)
        comparison=compare_interval_windows(*windows,parent_directory,directory,*profiles)
        phases=[dict(phase=phase,stamp=stamp,**interval_block(block)) for phase,(stamp,block) in zip(['warmup','roi'],sorted(read_blocks(directory/'simulator_err.txt').items()))]
        quality=all(row['all_checked_writebacks_ordered'] is True and row['observed_interval_checks_passed'] is True for row in phases)
        rows.append(dict(point=point,parent_cpi=windows[0]['cpi'],observer_cpi=windows[1]['cpi'],comparison=comparison,
                         phases=phases,observed_interval_quality_passed=quality,result_sha256=hashlib.sha256((directory/'result.json').read_bytes()).hexdigest()))
    out=stage/'analysis';out.mkdir(exist_ok=True)
    result=dict(completed=len(rows),expected=len(spec['cases']),complete=len(rows)==len(spec['cases']),rows=rows,
                all_collected_execution_pairs_neutral=all(row['comparison']['neutral'] for row in rows) if rows else None,
                all_collected_interval_checks_passed=all(row['observed_interval_quality_passed'] for row in rows) if rows else None,
                source_event_applicability_validated=None,source_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                scope='Same FP-pipe output-valid cohort, including later-flushed writes; denominator checked-order_fail. Four new sums only, zero old-native exceptions against parent3f7. Positivity/conservation do not prove source/event applicability, retirement latency, FU service time, exclusive CPI or criticality. Parent3f7 ROB-stage failures remain failed; no averages inferred.')
    (out/'neutrality.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps({key:result[key] for key in ['completed','expected','all_collected_execution_pairs_neutral','all_collected_interval_checks_passed','source_event_applicability_validated']},indent=2))


if __name__=='__main__':main()
