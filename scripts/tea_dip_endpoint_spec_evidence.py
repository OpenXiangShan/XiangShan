"""Audit the original two endpoint SPEC windows, preserving native and TEA clock domains."""
from pathlib import Path
import argparse, hashlib, json, sqlite3, sys

TABLES = ['Tea_0','TeaDelayedFinal_0','TeaCycleSummary_0','TeaNoCommit_0','Dip_0','DipDelayedFinal_0']
STATES = ['Computing','Stalled','Flushed','Drained']


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream,'sha256').hexdigest()


def _compare_pair(before_folder, after_folder):
    from native_perf import read_blocks, unique_counters, counter_multisets
    blocks = [read_blocks(folder/'simulator_err.txt') for folder in [before_folder,after_folder]]
    assert all(len(block) == 2 for block in blocks)
    frames, extra = [], set()
    native_retired = 0
    for stamp, block in blocks[1].items():
        counters = unique_counters(block)
        def counter(suffix):
            value, = [value for name,value in counters.items() if name.endswith('.rob.'+suffix)]
            return value
        suffixes = ['tea_exact_endpoint_cycle','tea_exact_endpoint_retired',*['tea_exact_endpoint_state_'+str(i) for i in range(4)]]
        names = {name for name in block if any(name.endswith('.rob.'+suffix) for suffix in suffixes)}
        assert len(names) == 6
        extra.update(names)
        states = {name:counter('tea_exact_endpoint_state_'+str(i)) for i,name in enumerate(STATES)}
        cycle = counter('tea_exact_endpoint_cycle')
        assert sum(states.values()) == cycle
        native_retired += counter('commitInstr')
        frames.append(dict(native_print_stamp=stamp,native_interval_cycles=counter('clock_cycle'),native_interval_retired=counter('commitInstr'),native_cumulative_retired=native_retired,tea_cycle=cycle,tea_retired=counter('tea_exact_endpoint_retired'),states=states,observed_epoch_offset=stamp+1-cycle))
    original = {stamp:counter_multisets(block) for stamp,block in blocks[0].items()}
    observed = {stamp:counter_multisets({name:values for name,values in block.items() if name not in extra}) for stamp,block in blocks[1].items()}
    checks = dict(all_old_native_multivalues_equal=original == observed)
    tables = {}
    for table in TABLES:
        records = []
        for folder in [before_folder,after_folder]:
            with sqlite3.connect((folder/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True) as db:
                records.append(db.execute('SELECT * FROM '+table+' ORDER BY ID').fetchall())
        equal = records[0] == records[1]
        checks[table+'_equal'] = equal
        tables[table] = dict(rows=[len(rows) for rows in records],all_rows_equal=equal)
    before, after = frames
    states = {state:after['states'][state]-before['states'][state] for state in STATES}
    duration = after['tea_cycle']-before['tea_cycle']
    assert duration > 0 and after['native_interval_retired'] > 0 and all(value >= 0 for value in states.values()) and sum(states.values()) == duration
    with sqlite3.connect((after_folder/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True) as db:
        db.row_factory = sqlite3.Row
        summaries = list(db.execute('SELECT * FROM TeaCycleSummary_0 WHERE CYCLE>=? AND CYCLE<=? ORDER BY CYCLE,ID',(before['tea_cycle'],after['tea_cycle'])))
    first,last = summaries[0],summaries[-1]
    coarse = {state:last['STATES_'+str(i)]-first['STATES_'+str(i)] for i,state in enumerate(STATES)}
    edges = {state:states[state]-coarse[state] for state in STATES}
    assert sum(coarse.values()) == last['CYCLE']-first['CYCLE'] and all(value >= 0 for value in edges.values()) and sum(coarse.values())+sum(edges.values()) == duration
    matched = duration == after['native_print_stamp']-before['native_print_stamp'] == after['native_interval_cycles']
    return dict(neutrality=dict(passed=all(checks.values()),checks=checks,tables=tables,exact_new_native_names=sorted(extra)),frames=frames,native_roi_cycle_duration_matches=matched,state_event_phase_alignment_verified=None,roi=dict(tea_cycles=duration,native_cycles=after['native_interval_cycles'],native_reported_retired=after['native_interval_retired'],tea_retired_difference=after['tea_retired']-before['tea_retired'],retirement_counter_difference=after['tea_retired']-before['tea_retired']-after['native_interval_retired'],states=states,coarse_interior=dict(begin=first['CYCLE'],end=last['CYCLE'],cycles=sum(coarse.values()),states=coarse),newly_visible_edges=dict(cycles=sum(edges.values()),states=edges),state_budget_per_native_reported_retired={state:value/after['native_interval_retired'] for state,value in states.items()},native_reported_cpi=after['native_interval_cycles']/after['native_interval_retired']),performance_speedup=None)


def analyze(root):
    stage = root/'tea-exact-endpoint-spec'
    receipt_path = stage/'closure-receipt.json'
    if not receipt_path.exists():
        return dict(pending=True,database_read=False,completed=0,expected=2,reason='Original endpoint SPEC closure not archived')
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['controller_live'] and receipt['all_original_guests_closed']
    assert receipt['controller']['pid'] == 4157318 and receipt['controller']['start_ticks'] == '61270844'
    assert receipt['controller']['completed'] == receipt['controller']['total'] == 2
    for name,entry in receipt['files'].items():
        path = stage/name
        assert path.stat().st_size == entry['bytes'] and digest(path) == entry['sha256']
    experiment = json.loads((stage/'experiment.json').read_text())
    assert experiment['build']['source'] == 'addd8cc4d35a5a3f5f3fa3cfe8371280ff787807'
    assert experiment['build']['binary_sha256'] == 'c136ef1df8210da3f5668276355b36d85b0f4465cf868edfe55fa3c6f663e496'
    assert [(case['point'],case['seed']) for case in experiment['cases']] == [('gromacs_105_0.206090',9457),('lbm_6382_0.375486',7058)]
    control_stage = root/'gate-observation-v2'
    manifest = json.loads((control_stage/'manifest.json').read_text())
    control_receipt = json.loads((root/'evidence/gate-observe-v2/spec-closure-receipt.json').read_text())
    assert not control_receipt['controller_live']
    # Validate every pair before opening any database, including its reused control.
    for case in experiment['cases']:
        point = case['point'];folder = stage/'points'/point;old = control_stage/'gate-observe-v2/points'/point
        result = json.loads((folder/'result.json').read_text());process = json.loads((folder/'process.json').read_text());launch = json.loads((folder/'launch-observation.json').read_text())
        assert process == result['process'] and result['original_process_closed'] and result['exitcode'] == 0 and result['difftest_passed'] and all(result['sampler_flags'].values())
        assert launch['live'] and all(launch[key] == value for key,value in process.items())
        assert result['source'] == experiment['build']['source'] and result['binary_sha256'] == experiment['build']['binary_sha256']
        closed, = [row for row in control_receipt['rows'] if row['point'] == point]
        assert not closed['guest_live'] and closed['exitcode'] == '0' and closed['process'] == json.loads((old/'process.json').read_text())
        for name,entry in manifest.items():
            if name.startswith('gate-observe-v2/points/'+point+'/'):
                path = control_stage/name
                assert path.stat().st_size == entry['bytes'] and digest(path) == entry['sha256']
        assert digest(old/'command.json') == case['control_command_sha256'] and digest(old/'result.json') == case['control_result_sha256']
        control = json.loads((old/'result.json').read_text())
        assert control['source'] == '7efc7832caaf32fda099e53528cc6badc7054cb6' and control['binary_sha256'] == '03489138cbd2c5ccc92b6f36ceb721b676f9537ce40ff6bd41c1aac37c547081'
        before,after = [json.loads((path/'command.json').read_text()) for path in [old,folder]]
        for flag in ['-I','-W','-C','-s','-i','--diff','--cst-file','--dump-select-db']:
            assert before[before.index(flag)+1] == after[after.index(flag)+1]
        assert after[after.index('-s')+1] == str(case['seed']) and after[after.index('--dump-select-db')+1].split() == TABLES
    sys.path.insert(0,str(root))
    rows = []
    for case in experiment['cases']:
        point = case['point']
        rows.append(dict(point=point,**_compare_pair(control_stage/'gate-observe-v2/points'/point,stage/'points'/point)))
    return dict(pending=False,database_read=True,completed=len(rows),expected=2,consumer_sha256=digest(Path(__file__)),closed_receipt_sha256=digest(receipt_path),native_parser_sha256=digest(root/'native_perf.py'),rows=rows,all_pairs_neutral=all(row['neutrality']['passed'] for row in rows),scope='Two original closed g105/lbm1M+1M SPEC attempts compared with original7efc controls. All old native multivalues and six complete tables; two-dump TEA state differences, clock duration match and retirement offsets remain separate. Duration matching does not prove event phase or architectural prefix identity. No hard-coded offset, throughput gain, full-suite score or default adoption claim.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir',required=True,type=Path)
    args = parser.parse_args();root = args.report_dir.resolve();result = analyze(root)
    if not result['pending']:
        output = root/'tea-exact-endpoint-spec/analysis';output.mkdir(exist_ok=True)
        (output/'evidence.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps(result,indent=2))


if __name__ == '__main__':
    main()
