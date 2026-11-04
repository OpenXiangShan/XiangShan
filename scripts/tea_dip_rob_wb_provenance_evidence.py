"""Read closed ROB pre-write provenance observations; preserve all old native fields."""
from pathlib import Path
import argparse,hashlib,json,sys

FIELDS=['checked','filtered_out','ptr_different','seq_different','seq_same_uop_different',
        'seq_same_enq_before_dispatch','seq_same_gap_one','seq_same_gap_gt_one',
        'seq_different_enq_before_dispatch','seq_same_enq_gap_sum','seq_different_enq_gap_sum',
        'seq_different_slot_alu','filtered_out_seq_different']
NEW_NAMES={f'diag_rob_wb_{group}_{name}' for group in ['fp','other'] for name in FIELDS}

def old_native_blocks(directory, *, sequence=False):
    excluded=NEW_NAMES | ({f'diag_rob_wb_{group}_seq_nonzero_pair' for group in ['fp','other']} if sequence else set())
    from native_perf import read_blocks,counter_multisets
    return {stamp:counter_multisets({key:values for key,values in block.items()
            if key.rsplit('.',1)[-1] not in excluded}) for stamp,block in read_blocks(directory/'simulator_err.txt').items()}

def provenance_block(block, *, sequence=False, identity_available=False):
    from native_perf import unique_counters
    counters=unique_counters(block);groups=[]
    fields=FIELDS+(['seq_nonzero_pair'] if sequence else [])
    for group in ['fp','other']:
        values={name:next((value for key,value in counters.items() if key.endswith('.diag_rob_wb_'+group+'_'+name)),None) for name in fields}
        complete=all(value is not None for value in values.values());checks=None
        if complete:
            v=values;total=v['checked'];different=v['seq_different'];same=total-different
            checks=dict(nonnegative=all(value>=0 for value in v.values()),
                event_bounds=all(v[key]<=total for key in fields if not key.endswith('_sum')),
                same_literal_sequence_bounds=v['seq_same_uop_different']<=same and v['seq_same_enq_before_dispatch']<=same,
                different_literal_sequence_bounds=v['seq_different_enq_before_dispatch']<=different and v['seq_different_slot_alu']<=different,
                same_before_partition=v['seq_same_enq_before_dispatch']==v['seq_same_gap_one']+v['seq_same_gap_gt_one'],
                filtered_intersection=v['filtered_out_seq_different']<=min(v['filtered_out'],different),
                same_gap_lower_bound=v['seq_same_enq_gap_sum']>=v['seq_same_gap_one']+2*v['seq_same_gap_gt_one'],
                different_gap_lower_bound=v['seq_different_enq_gap_sum']>=v['seq_different_enq_before_dispatch'],
                empty_subcohort_sums_zero=(v['seq_same_enq_before_dispatch']!=0 or v['seq_same_enq_gap_sum']==0) and (v['seq_different_enq_before_dispatch']!=0 or v['seq_different_enq_gap_sum']==0),
                empty_event_set_zero=total!=0 or all(value==0 for value in v.values()))
        covered=values.get('seq_nonzero_pair')
        full_identity=(identity_available is True and complete and all(checks.values()) and values['checked']>0 and covered==values['checked'])
        groups.append(dict(group=group,instruction_identity_cohort_interpretation_validated=full_identity,
                           nonzero_identity_pair_events=covered,all_fields_present=complete,values=values,checks=checks,
                           observed_checks_passed=all(checks.values()) if checks is not None else None))
    legacy={key:value for key,value in counters.items() if key.endswith(('.rob.fmul_latency_enq_rs','.rob.fdiv_fsqrt_latency_enq_rs'))}
    complete=all(group['all_fields_present'] for group in groups)
    return dict(all_required_fields_present=complete,all_26_fields_present=all(all(g['values'][name] is not None for name in FIELDS) for g in groups),groups=groups,
                instruction_sequence_identity_available=identity_available,
                observed_checks_passed=all(group['observed_checks_passed'] for group in groups) if complete else None,
                fp_writeback_events_observed=groups[0]['values']['checked']>0 if complete else None,
                old_retired_fp_enqueue_sums=legacy,old_negative_retired_sum_present=any(value<0 for value in legacy.values()))

def compare_provenance_windows(a,b,ad,bd,ap,bp, *, sequence=False):
    from analyze_queue_observation import compare_windows
    result=compare_windows(a,b,ad,bd,ap,bp)
    old=[old_native_blocks(directory,sequence=sequence) for directory in [ad,bd]]
    result['unfiltered_native_equal']=result['checks']['all_old_native_warmup_and_roi_equal']
    result['checks']['all_old_native_warmup_and_roi_equal']=old[0]==old[1]
    keys={key for phases in old for block in phases.values() for key in block}
    result['changed_old_native_names']=sorted(key for key in keys if
        [block.get(key) for stamp,block in sorted(old[0].items())]!=[block.get(key) for stamp,block in sorted(old[1].items())])
    result['neutral']=all(result['checks'].values());return result

def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',type=Path,required=True);parser.add_argument('--sequence',action='store_true',help='Audit a63 sequence tool against execution control6a; Git parentff24 is not the execution control');args=parser.parse_args()
    root=args.report_dir.resolve();sys.path.insert(0,str(root))
    from analyze_queue_observation import verify_files
    from analyze_windows import extract
    from native_perf import read_blocks
    sequence=args.sequence;variant='rob-wb-sequence-observe' if sequence else 'rob-wb-provenance-observe';parent='fp-wb-intervals-observe';evidence=root/'evidence'/variant
    stage=root/('rob-wb-sequence-observation' if sequence else 'rob-wb-provenance-observation');control_stage=root/'fp-wb-interval-observation'
    verify_files(evidence,json.loads((evidence/'source-launch-manifest.json').read_text()))
    source=json.loads((evidence/'source.json').read_text());proof=json.loads((evidence/'source-resource-check.json').read_text())
    launch=json.loads((evidence/'launch.json').read_text());spec=json.loads((evidence/'experiment.json').read_text())
    expected_head='a63f487b2670df03355172440d091c931148de95' if sequence else 'ff24eede48a2e385ccbffcccff24e1299ac82afb'
    execution_control='6a91bc8006ab084c87c22585fbf63a751f86aeb4'
    assert source['head']==proof['head']==launch['source']==expected_head and spec['controls']=={variant:parent}
    assert source['parent']==('ff24eede48a2e385ccbffcccff24e1299ac82afb' if sequence else execution_control)
    assert proof['restoring_observer_only_diff_recovers_control_rob' if sequence else 'restoring_observer_only_diff_recovers_parent_rob']
    assert set(proof['counter_fields'])==set(FIELDS+(['seq_nonzero_pair'] if sequence else [])) and proof['observer_counter_count']==(28 if sequence else 26)
    if sequence:
        emitted=evidence/'emitted-sequence-check.json'
        identity_available=json.loads(emitted.read_text())['actual_emitted_cpp_matches_tested_fix'] if emitted.exists() else None
    else:
        assessment=json.loads((evidence/'sequence-identity-assessment.json').read_text())
        verify_files(evidence/'sequence-identity-source',json.loads((evidence/'sequence-identity-manifest.json').read_text()))
        assert assessment['source']==expected_head and assessment['instruction_sequence_identity_available'] is False
        identity_available=False
    manifests=[json.loads((control_stage/'manifest.json').read_text()),json.loads((stage/'manifest.json').read_text()) if (stage/'manifest.json').exists() else {}]
    rows=[]
    for case in spec['cases']:
        point=case['point'];directory=stage/variant/'points'/point
        if not (directory/'result.json').exists():continue
        controls=control_stage/parent/'points'/point;windows=[];profiles=[]
        for name,folder,run_stage,manifest in [(parent,controls,control_stage,manifests[0]),(variant,directory,stage,manifests[1])]:
            prefix=f'{name}/points/{point}/';files={key:value for key,value in manifest.items() if key.startswith(prefix)}
            assert all(prefix+n in files for n in ['command.json','result.json','exitcode','simulator_out.txt','simulator_err.txt','simulator.db','probe-evidence.json'])
            verify_files(run_stage,files)
            build_evidence=root/'evidence'/name;verify_files(build_evidence,json.loads((build_evidence/'build-coremark-manifest.json').read_text()))
            build=json.loads((build_evidence/'build-state.json').read_text());smoke=json.loads((build_evidence/'coremark-audit.json').read_text())
            result=json.loads((folder/'result.json').read_text());command=json.loads((folder/'command.json').read_text())
            assert build['phase']=='complete' and smoke['passed'] and smoke['binary_sha256']==build['binary_sha256']==result['binary_sha256']
            assert result['source']==build['source']==(source['head'] if name==variant else execution_control)
            for flag,value in [('--diff',spec['reference']),('-W',str(spec['warmup'])),('-I',str(spec['max_instr'])),('-s',str(case['seed']))]:assert command[command.index(flag)+1]==value
            assert (folder/'exitcode').read_text().strip()=='0'
            window,profile=extract(folder,name,spec['period']);assert window['seed']==case['seed'] and abs(window['instructions']-1000000)<128 and abs(window['warmup_instructions']-1000000)<128
            windows.append(window);profiles.append(profile)
        comparison=compare_provenance_windows(*windows,controls,directory,*profiles,sequence=sequence)
        phases=[dict(phase=label,stamp=stamp,**provenance_block(block,sequence=sequence,identity_available=identity_available)) for label,(stamp,block) in zip(['warmup','roi'],sorted(read_blocks(directory/'simulator_err.txt').items()))]
        rows.append(dict(point=point,parent_cpi=windows[0]['cpi'],observer_cpi=windows[1]['cpi'],comparison=comparison,phases=phases,
                         observed_provenance_checks_passed=all(phase['observed_checks_passed'] is True for phase in phases)))
    result=dict(instruction_sequence_identity_available=identity_available,
                instruction_identity_scope='Producer availability and nonzero pair coverage are separate; literal seq equality alone is insufficient. All printed sequence cohorts remain literal unless their entire checked population has nonzero pairs.',
                completed=len(rows),expected=len(spec['cases']),complete=len(rows)==len(spec['cases']),rows=rows,
                all_collected_execution_pairs_neutral=all(row['comparison']['neutral'] for row in rows) if rows else None,
                all_collected_provenance_checks_passed=all(row['observed_provenance_checks_passed'] for row in rows) if rows else None,
                retired_negative_sum_cause_validated=None,source_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                scope=f'Pre-write stored ROB-slot metadata versus raw WB tags/time. Producer identity availability and full nonzero-pair coverage gate dynamic identity interpretation. Literal values, wrong-path and retired/unallocated slots do not equal retired latency, criticality or exclusive CPI. Exactly {28 if sequence else 26} new names ignored; all old native values protected. Old3f7 failures remain failed.')
    out=stage/'analysis';out.mkdir(exist_ok=True);(out/'neutrality.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps({key:result[key] for key in ['completed','expected','all_collected_execution_pairs_neutral','all_collected_provenance_checks_passed','instruction_sequence_identity_available','retired_negative_sum_cause_validated']},indent=2))

if __name__=='__main__':main()
