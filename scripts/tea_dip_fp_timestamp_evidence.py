"""Audit FP event timestamps separately from baseline functional performance."""
from pathlib import Path
import argparse,hashlib,json,re,sys

FP_SUM = re.compile(r'\.rob\.(fmul|fdiv_fsqrt)_latency_(enq_rs|select|issue|execute|enq_rs_execute)$')
STAGES = ['enq_rs','select','issue','execute','enq_rs_execute']


def compare_timestamp_windows(a,b,ad,bd,ap,bp):
    from analyze_queue_observation import compare_windows
    from native_perf import read_blocks,counter_multisets
    result=compare_windows(a,b,ad,bd,ap,bp)
    original=result['checks'].pop('all_old_native_warmup_and_roi_equal')
    filtered=[];expected=[]
    for d in [ad,bd]:
        blocks=read_blocks(d/'simulator_err.txt')
        filtered.append({stamp:counter_multisets({k:v for k,v in block.items() if
            not k.rsplit('.',1)[-1].startswith('diag_') and not FP_SUM.search(k)}) for stamp,block in blocks.items()})
        expected.append({stamp:counter_multisets({k:v for k,v in block.items() if FP_SUM.search(k)}) for stamp,block in blocks.items()})
    result['checks']['all_other_native_warmup_and_roi_equal']=filtered[0]==filtered[1]
    result['unfiltered_native_equal']=original
    result['target_timestamp_sums_equal']=expected[0]==expected[1]
    names=sorted({k for group in filtered for block in group.values() for k in block})
    result['unexpected_changed_native_names']=[k for k in names if [filtered[0][stamp].get(k) for stamp in sorted(filtered[0])] != [filtered[1][stamp].get(k) for stamp in sorted(filtered[1])]]
    result.pop('changed_old_native_names')
    result['neutral_except_documented_timestamp_sums']=all(result['checks'].values())
    result.pop('neutral')
    return result


def timestamp_phases(directory):
    from native_perf import read_blocks,unique_counters
    rows=[]
    for phase,(stamp,block) in zip(['warmup','roi'],sorted(read_blocks(directory/'simulator_err.txt').items())):
        counters=unique_counters(block)
        checked={k:v for k,v in counters.items() if k.endswith('.diag_fp_timestamp_checked_wb')}
        failures={k:v for k,v in counters.items() if k.endswith('.diag_fp_timestamp_order_fail')}
        prefixes=lambda values,suffix:{k[:-len(suffix)] for k in values}
        complete=len(checked)==len(failures)==4 and prefixes(checked,'.diag_fp_timestamp_checked_wb')==prefixes(failures,'.diag_fp_timestamp_order_fail')
        ordered=(sum(failures.values())==0) if complete and sum(checked.values())>0 else None
        groups=[]
        for fu in ['fmul','fdiv_fsqrt']:
            fields={k:v for k,v in counters.items() if FP_SUM.search(k) and '.rob.'+fu+'_latency_' in k}
            counts=[v for k,v in counters.items() if k.endswith('.rob.'+fu+'_instr_cnt')]
            count=counts[0] if len(counts)==1 else None
            negative={k:v for k,v in fields.items() if v<0}
            groups.append(dict(fu=fu,retired_debug_slot_count=count,stage_sum_fields=fields,
                rejected_elapsed_sums=negative,all_five_sums_present=len(fields)==len(STAGES),
                scope='Raw interval sums only until execution neutrality, source applicability and observed WB order are verified. No exclusive CPI or criticality claim.'))
        rows.append(dict(phase=phase,stamp=stamp,checked_writeback_counters=checked,order_failure_counters=failures,
            expected_four_fp_pipes_present=complete,all_observed_writebacks_ordered=ordered,groups=groups))
    return rows


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir',type=Path,required=True)
    args=parser.parse_args();root=args.report_dir.resolve();sys.path.insert(0,str(root))
    from analyze_queue_observation import verify_files
    from analyze_windows import extract
    evidence=root/'evidence/fp-timestamps-observe';stage=root/'fp-timestamp-observation';variant='fp-timestamps-observe'
    verify_files(evidence,json.loads((evidence/'source-launch-manifest.json').read_text()))
    spec=json.loads((evidence/'experiment.json').read_text());source=json.loads((evidence/'source.json').read_text())
    proof=json.loads((evidence/'source-resource-check.json').read_text())
    assert source['head']==proof['head']=='3f7fb4f44ec13ad5bd6161b92cb50b163da9b8aa' and proof['parameters_identical']
    rows=[];manifest=json.loads((stage/'manifest.json').read_text()) if (stage/'manifest.json').exists() else {}
    for case in spec['cases']:
        point=case['point'];d=stage/variant/'points'/point
        if not (d/'result.json').exists():continue
        prefix=f'{variant}/points/{point}/';files={k:v for k,v in manifest.items() if k.startswith(prefix)}
        assert all(prefix+n in files for n in ['command.json','result.json','exitcode','simulator_out.txt','simulator_err.txt','simulator.db','probe-evidence.json'])
        verify_files(stage,files)
        build=json.loads((evidence/'build-state.json').read_text());smoke=json.loads((evidence/'coremark-audit.json').read_text())
        assert build['phase']=='complete' and build['source']==source['head'] and smoke['passed'] and smoke['binary_sha256']==build['binary_sha256']
        ad=root/'raw/rebuild-control/short'/point
        a,ap=extract(ad,'rebuild-control',spec['period']);b,bp=extract(d,variant,spec['period'])
        result=json.loads((d/'result.json').read_text());command=json.loads((d/'command.json').read_text())
        assert result['source']==build['source'] and result['binary_sha256']==build['binary_sha256'] and build['binary'] in command
        for flag,value in [('--diff',spec['reference']),('-W',str(spec['warmup'])),('-I',str(spec['max_instr'])),('-s',str(case['seed']))]:assert command[command.index(flag)+1]==value
        for w in [a,b]:assert abs(w['warmup_instructions']-spec['warmup'])<128 and abs(w['instructions']-(spec['max_instr']-spec['warmup']))<128 and w['seed']==case['seed']
        comparison=compare_timestamp_windows(a,b,ad,d,ap,bp);phases=timestamp_phases(d)
        quality=all(phase['all_observed_writebacks_ordered'] is True and all(group['all_five_sums_present'] and not group['rejected_elapsed_sums'] for group in phase['groups']) for phase in phases)
        rows.append(dict(point=point,control_cpi=a['cpi'],observer_cpi=b['cpi'],comparison=comparison,phases=phases,
            observed_fp_timestamp_quality_passed=quality,result_sha256=hashlib.sha256((d/'result.json').read_bytes()).hexdigest()))
    out=stage/'analysis';out.mkdir(exist_ok=True)
    result=dict(completed=len(rows),expected=len(spec['cases']),rows=rows,complete=len(rows)==len(spec['cases']),
        all_collected_execution_pairs_neutral=all(row['comparison']['neutral_except_documented_timestamp_sums'] for row in rows) if rows else None,
        all_collected_fp_order_and_sums_passed=all(row['observed_fp_timestamp_quality_passed'] for row in rows) if rows else None,
        source_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        scope='Baseline841 observer only; ten named latency sums may change. Other native multisets and seven functional evidence categories must match. No performance gain, all-input equivalence, service latency or dynamic critical-path claim.')
    (out/'neutrality.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(dict(completed=result['completed'],expected=result['expected'],neutral=result['all_collected_execution_pairs_neutral'],fp_quality=result['all_collected_fp_order_and_sums_passed']),indent=2))


if __name__=='__main__':main()
