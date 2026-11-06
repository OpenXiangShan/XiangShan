"""Analyze the closed query endpoint focused experiment against original same-hardware evidence."""
from pathlib import Path
import argparse,hashlib,json,sqlite3,sys

TABLES=['Tea_0','TeaDelayedFinal_0','TeaCycleSummary_0','TeaNoCommit_0','Dip_0','DipDelayedFinal_0']
STATES=['Computing','Stalled','Flushed','Drained']
POINTS=['gromacs_105_0.206090','lbm_6382_0.375486','sphinx3_157921_0.136728']
PROBES=['diag_fp_src_wait_entry_cycles','diag_fp_eligible_entry_cycles','diag_fp_wb_conflict_entry_cycles','diag_fp_only_wb_conflict_cycles','diag_dispatch_iq_capacity_gate_cycles','diag_dispatch_iq_bandwidth_gate_cycles','diag_rename_head_blocked_cycles']


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream,'sha256').hexdigest()


def verify(folder,files):
    for name,entry in files.items():
        path=folder/name
        assert path.stat().st_size==entry['bytes'] and digest(path)==entry['sha256']


def old_execution(before,after,extra_names):
    from native_perf import read_blocks,counter_multisets
    native=[read_blocks(folder/'simulator_err.txt') for folder in [before,after]]
    original={stamp:counter_multisets(block) for stamp,block in native[0].items()}
    observed={stamp:counter_multisets({name:values for name,values in block.items() if name not in extra_names}) for stamp,block in native[1].items()}
    checks=dict(all_old_native_multivalues_equal=original==observed)
    tables={}
    for table in TABLES:
        values=[]
        for folder in [before,after]:
            with sqlite3.connect((folder/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True) as db:
                values.append(db.execute('SELECT * FROM '+table+' ORDER BY ID').fetchall())
        equal=values[0]==values[1];checks[table+'_equal']=equal
        tables[table]=dict(rows=[len(rows) for rows in values],all_rows_equal=equal)
    return dict(passed=all(checks.values()),checks=checks,tables=tables,exact_removed_new_native_names=sorted(extra_names))


def endpoint(folder):
    from native_perf import read_blocks,unique_counters
    blocks=read_blocks(folder/'simulator_err.txt');assert len(blocks)==2
    frames=[];native_retired=0
    for stamp,block in blocks.items():
        counters=unique_counters(block)
        def counter(suffix):
            value,=[value for name,value in counters.items() if name.endswith('.rob.'+suffix)]
            return value
        cycle=counter('tea_exact_endpoint_cycle');states={state:counter('tea_exact_endpoint_state_'+str(i)) for i,state in enumerate(STATES)}
        assert sum(states.values())==cycle
        native_retired+=counter('commitInstr')
        frames.append(dict(native_print_stamp=stamp,native_interval_cycles=counter('clock_cycle'),native_interval_retired=counter('commitInstr'),native_cumulative_retired=native_retired,tea_cycle=cycle,tea_retired=counter('tea_exact_endpoint_retired'),states=states,observed_epoch_offset=stamp+1-cycle))
    before,after=frames;duration=after['tea_cycle']-before['tea_cycle'];states={state:after['states'][state]-before['states'][state] for state in STATES}
    assert sum(states.values())==duration and all(value>=0 for value in states.values())
    matched=duration==after['native_print_stamp']-before['native_print_stamp']==after['native_interval_cycles']
    with sqlite3.connect((folder/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True) as db:
        db.row_factory=sqlite3.Row
        summaries=list(db.execute('SELECT * FROM TeaCycleSummary_0 WHERE CYCLE>=? AND CYCLE<=? ORDER BY CYCLE,ID',(before['tea_cycle'],after['tea_cycle'])))
    first,last=summaries[0],summaries[-1]
    interior={state:last['STATES_'+str(i)]-first['STATES_'+str(i)] for i,state in enumerate(STATES)}
    edges={state:states[state]-interior[state] for state in STATES}
    assert sum(interior.values())==last['CYCLE']-first['CYCLE'] and all(value>=0 for value in edges.values()) and sum(edges.values())+sum(interior.values())==duration
    retired=after['native_interval_retired'];budget={state:value/retired for state,value in states.items()} if matched else None
    if matched:assert abs(sum(budget.values())-after['native_interval_cycles']/retired)<1e-12
    block=blocks[max(blocks)]
    probes={name:values[0] if len(values)==1 else None for name,values in block.items() if name.rsplit('.',1)[-1] in PROBES}
    return dict(frames=frames,native_roi_cycle_duration_matches=matched,state_event_phase_alignment_verified=None,roi=dict(tea_cycles=duration,native_cycles=after['native_interval_cycles'],native_reported_retired=retired,tea_retired_difference=after['tea_retired']-before['tea_retired'],retirement_counter_difference=after['tea_retired']-before['tea_retired']-retired,states=states,coarse_interior=dict(begin=first['CYCLE'],end=last['CYCLE'],cycles=sum(interior.values()),states=interior),newly_visible_edges=dict(cycles=sum(edges.values()),states=edges),state_budget_per_native_reported_retired=budget),native_probe_counts=probes,native_probe_unit='Per-queue entry-cycles or boolean cycles as named; overlapping observations, not additive CPU critical cycles.')


def pair(before,after):
    from analyze_windows import extract
    audits=[extract(folder,variant)[0] for folder,variant in [(before,'control_endpoint'),(after,'query_endpoint')]]
    for audit in audits:assert abs(audit['warmup_instructions']-1000000)<128 and abs(audit['instructions']-1000000)<128
    assert audits[0]['seed']==audits[1]['seed']
    endpoints=[endpoint(folder) for folder in [before,after]]
    delta=audits[1]['cpi']-audits[0]['cpi'];dip={state:audits[1]['metrics']['DIP_'+state]-audits[0]['metrics']['DIP_'+state] for state in ['Base','Stall','FrontEnd','Misspeculation','Unresolved']}
    assert abs(sum(dip.values())-delta)<1e-9
    mapped=all(window['native_roi_cycle_duration_matches'] for window in endpoints)
    exact={state:endpoints[1]['roi']['state_budget_per_native_reported_retired'][state]-endpoints[0]['roi']['state_budget_per_native_reported_retired'][state] for state in STATES} if mapped else None
    if mapped:assert abs(sum(exact.values())-delta)<1e-12
    return dict(control_cpi=audits[0]['cpi'],candidate_cpi=audits[1]['cpi'],observed_native_speedup=audits[0]['cpi']/audits[1]['cpi']-1,delta_cpi=delta,exact_state_delta_cpi=exact,dip_exclusive_delta_cpi=dip,native_roi_counter_rates=[audit['counters'] for audit in audits],windows=endpoints,scope='Native CPI is measured from each reported ROI denominator. Exact TEA states sum to the native CPI delta only when both observed dump durations match. DIP is a separate exclusive budget; native counters overlap. Event phase/prefix identity and unique cause remain unverified.')


def analyze(root):
    stage=root/'wb-query-endpoint-focused';receipt_path=stage/'closure-receipt.json'
    if not receipt_path.exists():return dict(pending=True,database_read=False,completed=0,expected=3,reason='Original four focused guests/controller closure not archived')
    receipt=json.loads(receipt_path.read_text());assert receipt['ready'] and not receipt['controller_live'] and receipt['all_original_guests_closed'] and receipt['controller']['completed']==receipt['controller']['total']==4
    verify(stage,receipt['files']);experiment=json.loads((stage/'experiment.json').read_text());gate_path=root/'wb-query-endpoint-validation/analysis/coremark-gate.json';gate=json.loads(gate_path.read_text())
    assert gate['passed'] and not gate['pending'] and gate['source']=='2915a6b022bd957570bd961a01611026c2220fb7' and gate['control_source']=='addd8cc4d35a5a3f5f3fa3cfe8371280ff787807' and digest(gate_path)==experiment['gate_sha256']
    assert experiment['builds']['query_endpoint']['source']==gate['source'] and experiment['builds']['query_endpoint']['binary_sha256']==gate['binary_sha256']
    assert experiment['builds']['control_endpoint']['source']==gate['control_source'] and experiment['builds']['control_endpoint']['binary_sha256']==gate['control_binary_sha256']
    assert experiment['runner_sha256']==digest(root/'evidence/query-endpoint-validation/tea_dip_query_endpoint_focused.py')
    control_stage=root/'tea-exact-endpoint-spec';control_receipt=json.loads((control_stage/'closure-receipt.json').read_text());assert control_receipt['ready'] and not control_receipt['controller_live'] and control_receipt['all_original_guests_closed'];verify(control_stage,control_receipt['files'])
    raw_manifest=json.loads((root/'raw/manifest.json').read_text())
    folders={}
    # Establish all source/config/closure/raw contracts before opening any database.
    for case in experiment['cases']:
        variant,point=case['variant'],case['point'];folder=stage/variant/point;result=json.loads((folder/'result.json').read_text());process=json.loads((folder/'process.json').read_text());launch=json.loads((folder/'launch-observation.json').read_text())
        assert result['original_process_closed'] and result['exitcode']==0 and result['difftest_passed'] and all(result['sampler_flags'].values()) and result['process']==process
        assert launch['live'] and all(launch[key]==value for key,value in process.items())
        build=experiment['builds'][variant];assert result['source']==build['source'] and result['binary_sha256']==build['binary_sha256']
        assert case['cst_sha256']=='70c449a30aa94a9527d725bf888783a897cf896e9d7df19937fb0f56daed746d' and case['reference_sha256']=='9188e7dda30709962f0bfbc1dd87cea73d88a18c92c30892f93ed529c22def43'
        folders[variant,point]=folder
    assert set(folders)=={('query_endpoint',point) for point in POINTS}|{('control_endpoint',POINTS[2])}
    controls={point:control_stage/'points'/point for point in POINTS[:2]};controls[POINTS[2]]=folders['control_endpoint',POINTS[2]]
    originals={}
    for point in POINTS:
        control_result=json.loads((controls[point]/'result.json').read_text());assert control_result['source']==gate['control_source'] and control_result['binary_sha256']==gate['control_binary_sha256']
        old=root/'wb-query/fdiv-wb-query/points'/point;old_receipt=json.loads((root/'wb-query/closed-receipts'/(point+'.json')).read_text());assert not old_receipt['original_process_live'] and old_receipt['exitcode']=='0' and old_receipt['source']=='b0a22b93a867c5dff56b1b3fb32708c9e6363b01' and old_receipt['binary_sha256']=='401a3d8b0258a8d91fadf9fc1fae8dd207c6508938b146ff589f8f0f0247bb9e';verify(old,old_receipt['files']);originals[point]=old
        prefix='rebuild-control/short/'+point+'/'
        selected={name:entry for name,entry in raw_manifest.items() if name.startswith(prefix)};assert all(prefix+name in selected for name in ['command.json','result.json','exitcode','simulator_out.txt','simulator_err.txt','simulator.db']);verify(root/'raw',selected)
        raw_control=root/'raw/rebuild-control/short'/point
        assert json.loads((raw_control/'result.json').read_text())['source']=='84112d0b65231ca690a4ef2fea4b5169f2146e10'
        commands=[json.loads((folder/'command.json').read_text()) for folder in [raw_control,controls[point],old,folders['query_endpoint',point]]]
        for flag in ['-W','-I','-C','-s','-i','--diff','--dump-select-db']:
            assert len({command[command.index(flag)+1] for command in commands})==1
        for folder in [controls[point],old,folders['query_endpoint',point]]:assert (folder/'exitcode').read_text().strip()=='0'
    sys.path.insert(0,str(root));from native_perf import read_blocks
    rows=[]
    for point in POINTS:
        before,after=controls[point],folders['query_endpoint',point]
        plain=read_blocks(root/'raw/rebuild-control/short'/point/'simulator_err.txt');observed=read_blocks(before/'simulator_err.txt')
        old_names={name for block in plain.values() for name in block};new_names={name for block in observed.values() for name in block};assert old_names<=new_names
        exact_new_names=new_names-old_names
        control_neutrality=old_execution(root/'raw/rebuild-control/short'/point,before,exact_new_names)
        query_neutrality=old_execution(originals[point],after,exact_new_names)
        comparison=pair(before,after);valid=control_neutrality['passed'] and query_neutrality['passed']
        rows.append(dict(point=point,control_instrumentation_neutrality=control_neutrality,query_instrumentation_neutrality=query_neutrality,observation_neutrality_valid=valid,performance_speedup=comparison['observed_native_speedup'] if valid else None,comparison=comparison))
    return dict(pending=False,database_read=True,completed=3,expected=3,consumer_sha256=digest(Path(__file__)),closed_receipt_sha256=digest(receipt_path),coremark_gate_sha256=digest(gate_path),helper_sha256={name:digest(root/name) for name in ['native_perf.py','analyze_windows.py']},rows=rows,all_observation_pairs_neutral=all(row['observation_neutrality_valid'] for row in rows),scope='Existing candidate only: original b0a vs instrumented2915 full-old-native and six complete table neutrality, plus addd baseline calibration against841. Exact new diagnostic name set derived from calibrated same-point baseline; no wildcard exclusions or lost duplicate native values. Focused native/exact TEA/DIP/native-probe budgets retain retirement offsets and unknown event phase. Three selected windows are not a suite score or default adoption.')


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',required=True,type=Path);args=parser.parse_args();root=args.report_dir.resolve();result=analyze(root)
    if not result['pending']:
        output=root/'wb-query-endpoint-focused/analysis';output.mkdir(exist_ok=True);(output/'evidence.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps(result,indent=2))


if __name__=='__main__':
    main()
