"""Audit last-WB ownership at retired FMUL debug slots, preserving every old counter."""
from pathlib import Path
import argparse,hashlib,json,sys

FIELDS=['slots','enq_nonnegative_sum','enq_negative_slots','enq_negative_gap_sum',
        'negative_last_wb_seen','negative_last_wb_diff_owner','negative_last_wb_zero_time',
        'negative_last_wb_nonfp','negative_last_wb_nonfp_zero_diff','negative_last_wb_nonfp_zero_diff_gap_sum']
NEW_NAMES={'diag_retired_fmul_'+name for name in FIELDS}

def old_native_blocks(directory):
    from native_perf import read_blocks,counter_multisets
    return {stamp:counter_multisets({key:values for key,values in block.items() if key.rsplit('.',1)[-1] not in NEW_NAMES})
            for stamp,block in read_blocks(directory/'simulator_err.txt').items()}

def owner_block(block):
    from native_perf import unique_counters
    native=unique_counters(block)
    def value(suffix):return next((value for key,value in native.items() if key.endswith('.'+suffix)),None)
    v={name:value('diag_retired_fmul_'+name) for name in FIELDS}
    legacy=dict(slots=value('fmul_instr_cnt'),enqueue_sum=value('fmul_latency_enq_rs'))
    complete=all(value is not None for value in [*v.values(),*legacy.values()]);checks=None
    if complete:
        neg=v['enq_negative_slots'];gap=v['enq_negative_gap_sum'];seen=v['negative_last_wb_seen']
        joint=v['negative_last_wb_nonfp_zero_diff'];joint_gap=v['negative_last_wb_nonfp_zero_diff_gap_sum']
        checks=dict(nonnegative_observer_values=all(value>=0 for value in v.values()),
            legacy_retired_slot_population_equal=v['slots']==legacy['slots'],
            signed_sum_reconstructed=v['enq_nonnegative_sum']-gap==legacy['enqueue_sum'],
            negative_slots_bounded=0<=neg<=v['slots'],seen_bounded=0<=seen<=neg,
            last_writer_flags_within_seen=all(0<=v[name]<=seen for name in ['negative_last_wb_diff_owner','negative_last_wb_zero_time','negative_last_wb_nonfp']),
            joint_bounded=0<=joint<=min(v[name] for name in ['negative_last_wb_diff_owner','negative_last_wb_zero_time','negative_last_wb_nonfp']),
            gap_bounds=gap>=neg and joint<=joint_gap<=gap,
            empty_negative_set_zero=neg!=0 or all(v[name]==0 for name in FIELDS if name not in ['slots','enq_nonnegative_sum']),
            empty_joint_gap_zero=joint!=0 or joint_gap==0,
            empty_retired_set_zero=v['slots']!=0 or v['enq_nonnegative_sum']==0)
    numeric=all(checks.values()) if checks is not None else None
    coverage=(v['negative_last_wb_seen']==v['enq_negative_slots']) if numeric and v['enq_negative_slots']>0 else None
    sufficient=(coverage and v['negative_last_wb_nonfp_zero_diff']==v['enq_negative_slots'] and
                v['negative_last_wb_nonfp_zero_diff_gap_sum']==v['enq_negative_gap_sum']) if coverage is not None else None
    return dict(all_fields_present=complete,values=v,legacy=legacy,checks=checks,observed_checks_passed=numeric,
                negative_retired_slots_observed=v['enq_negative_slots']>0 if complete else None,
                all_negative_slots_have_last_writer_provenance=coverage,
                joint_accounts_for_all_recorded_negative_gaps=sufficient,
                joint_share_of_negative_gap_magnitude=v['negative_last_wb_nonfp_zero_diff_gap_sum']/v['enq_negative_gap_sum'] if numeric and v['enq_negative_gap_sum']>0 else None,
                old_negative_retired_sum_present=legacy['enqueue_sum']<0 if legacy['enqueue_sum'] is not None else None)

def compare_owner_windows(a,b,ad,bd,ap,bp):
    from analyze_queue_observation import compare_windows
    result=compare_windows(a,b,ad,bd,ap,bp);old=[old_native_blocks(d) for d in [ad,bd]]
    result['unfiltered_native_equal']=result['checks']['all_old_native_warmup_and_roi_equal']
    result['checks']['all_old_native_warmup_and_roi_equal']=old[0]==old[1]
    keys={key for phases in old for block in phases.values() for key in block}
    result['changed_old_native_names']=sorted(key for key in keys if [block.get(key) for _,block in sorted(old[0].items())]!=[block.get(key) for _,block in sorted(old[1].items())])
    result['neutral']=all(result['checks'].values());return result

def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',required=True,type=Path);args=parser.parse_args()
    root=args.report_dir.resolve();sys.path.insert(0,str(root))
    from analyze_queue_observation import verify_files
    from analyze_windows import extract
    from native_perf import read_blocks
    variant='rob-retired-owner-observe';parent='rob-wb-sequence-observe';stage=root/'rob-retired-owner-observation';control=root/'rob-wb-sequence-observation';evidence=root/'evidence'/variant
    verify_files(evidence,json.loads((evidence/'source-launch-manifest.json').read_text()))
    source=json.loads((evidence/'source.json').read_text());proof=json.loads((evidence/'source-resource-check.json').read_text());spec=json.loads((evidence/'experiment.json').read_text());launch=json.loads((evidence/'launch.json').read_text())
    assert source['head']==proof['head']==launch['source']=='f7a2e9be62af362b332eb5edc5bc2c22619aba3b' and source['parent']==proof['execution_control']=='a63f487b2670df03355172440d091c931148de95'
    assert spec['controls']=={variant:parent} and proof['removing_exact_five_observer_blocks_recovers_parent_rob'] and set(proof['added_counter_fields'])==NEW_NAMES
    manifests=[json.loads((control/'manifest.json').read_text()),json.loads((stage/'manifest.json').read_text()) if (stage/'manifest.json').exists() else {}];rows=[]
    for case in spec['cases']:
        point=case['point'];directory=stage/variant/'points'/point
        if not (directory/'result.json').exists():continue
        controls=control/parent/'points'/point;windows=[];profiles=[]
        for name,folder,run_stage,manifest in [(parent,controls,control,manifests[0]),(variant,directory,stage,manifests[1])]:
            prefix=f'{name}/points/{point}/';files={key:value for key,value in manifest.items() if key.startswith(prefix)}
            assert all(prefix+n in files for n in ['command.json','result.json','exitcode','simulator_out.txt','simulator_err.txt','simulator.db','probe-evidence.json']);verify_files(run_stage,files)
            build_evidence=root/'evidence'/name;verify_files(build_evidence,json.loads((build_evidence/'build-coremark-manifest.json').read_text()))
            build=json.loads((build_evidence/'build-state.json').read_text());smoke=json.loads((build_evidence/'coremark-audit.json').read_text());result=json.loads((folder/'result.json').read_text());command=json.loads((folder/'command.json').read_text())
            assert build['phase']=='complete' and smoke['passed'] and smoke['binary_sha256']==build['binary_sha256']==result['binary_sha256']
            assert result['source']==build['source']==(source['head'] if name==variant else source['parent'])
            for flag,value in [('--diff',spec['reference']),('-W',str(spec['warmup'])),('-I',str(spec['max_instr'])),('-s',str(case['seed']))]:assert command[command.index(flag)+1]==value
            assert (folder/'exitcode').read_text().strip()=='0'
            window,profile=extract(folder,name,spec['period']);assert window['seed']==case['seed'] and abs(window['instructions']-1000000)<128 and abs(window['warmup_instructions']-1000000)<128
            windows.append(window);profiles.append(profile)
        comparison=compare_owner_windows(*windows,controls,directory,*profiles)
        phases=[dict(phase=label,stamp=stamp,**owner_block(block)) for label,(stamp,block) in zip(['warmup','roi'],sorted(read_blocks(directory/'simulator_err.txt').items()))]
        rows.append(dict(point=point,parent_cpi=windows[0]['cpi'],observer_cpi=windows[1]['cpi'],comparison=comparison,phases=phases,
                         observed_checks_passed=all(phase['observed_checks_passed'] is True for phase in phases)))
    result=dict(completed=len(rows),expected=len(spec['cases']),complete=len(rows)==len(spec['cases']),rows=rows,
        all_collected_execution_pairs_neutral=all(row['comparison']['neutral'] for row in rows) if rows else None,
        all_collected_reconstruction_checks_passed=all(row['observed_checks_passed'] for row in rows) if rows else None,
        retired_negative_sum_cause_validated=None,source_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        scope='Retired FMUL debug slots, not all source instructions or raw WB. Last-writer markers match original metadata/WB priority, including same-cycle ownership changes; perf-clean retains marker state. Exactly10 new names ignored; all28 old provenance and every other native field protected. Reconstruction/coverage are separate from physical latency and critical-path causality. Old failures remain failed.')
    out=stage/'analysis';out.mkdir(exist_ok=True);(out/'neutrality.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps({key:result[key] for key in ['completed','expected','all_collected_execution_pairs_neutral','all_collected_reconstruction_checks_passed','retired_negative_sum_cause_validated']},indent=2))

if __name__=='__main__':main()
