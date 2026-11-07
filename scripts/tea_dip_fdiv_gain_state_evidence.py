"""Archive matched gain-cancellation phase observers and audit lifetime response bins."""
from pathlib import Path
import argparse
import importlib.util
import json
import os
import shlex
import subprocess
import sys
import tarfile

from tea_dip_query_cohort_evidence import digest, frame_counts, gain_span
from tea_dip_fdiv_unit_state_evidence import unit_counts, PARTITION

FILES = ['command.json', 'result.json', 'exitcode', 'simulator_out.txt', 'simulator_err.txt',
         'simulator.db', 'process.json', 'launch-observation.json']



def import_launch_metadata(root):
    evidence = root / 'evidence/fdiv-gain-state-observe'
    descriptor = evidence / 'launch-gate.json'
    if not descriptor.exists():
        return dict(pending=True, ready=False, database_read=False, rows=[],
                    reason='No original gain-state observer launch archived; collection and analysis pending')
    gate = json.loads(descriptor.read_text())
    assert digest(evidence / 'launch-gate-config.json') == gate['config_sha256']
    code = r'''from pathlib import Path
import hashlib,json
record=__RECORD__;root=Path(record['run_dir'])
def digest(path):
 with path.open('rb')as stream:return hashlib.file_digest(stream,'sha256').hexdigest()
def live(process):
 path=Path('/proc',str(process['pid']),'stat')
 return path.exists()and path.read_text().rsplit(')',1)[1].split()[19]==process['start_ticks']
assert digest(root/'config.json')==record['config_sha256']
assert digest(root/'gate.py')==record['gate_script_sha256']
state_path=root/'gate-state.json'
if not state_path.exists():
 assert live(record['process'])
 print(json.dumps(dict(ready=False,phase='starting',original_gate_live=True,database_read=False)))
else:
 state=json.loads(state_path.read_text());assert all(state[key]==record['process'][key]for key in ['pid','start_ticks'])
 assert state['config_sha256']==record['config_sha256']and state['source']=='6cbc9752d352b43b15fc2177b9451caccbfe74f9'
 assert state['phase']!='failed',state
 if state['phase']!='launched':
  assert live(record['process'])
  print(json.dumps(dict(ready=False,gate=state,original_gate_live=True,database_read=False)))
 else:
  cfg=json.loads((root/'config.json').read_text());observer=Path(cfg['node_run_dir'])
  files={name:(observer/name).read_text()for name in ['launch.json','experiment.json','run.py','native_perf.py','tea_dip_query_endpoint_evidence.py','parent-closed-inputs.json']}
  files['parent-gate-validation.json']=(root/'parent-gate-validation.json').read_text()
  launch=json.loads(files['launch.json']);spec=json.loads(files['experiment.json']);parent=json.loads(files['parent-closed-inputs.json'])
  assert launch['process']==state['observer']and digest(root/'observer-launch.json')==state['launch_sha256']
  assert launch['source']==spec['source']==state['source']and launch['parent_source']==spec['parent_source']==cfg['parent_source']
  assert launch['spec_sha256']==hashlib.sha256(files['experiment.json'].encode()).hexdigest()
  assert launch['runner_sha256']==cfg['parent_runner_sha256']==hashlib.sha256(files['run.py'].encode()).hexdigest()
  assert digest(root/'parent-gate-validation.json')==parent['gate_validation_sha256']==launch['parent_gate_validation_sha256']
  assert spec['parent_receipt_sha256']==hashlib.sha256(files['parent-closed-inputs.json'].encode()).hexdigest()
  if not(observer/'run-state.json').exists():
   assert live(launch['process'])
   print(json.dumps(dict(ready=False,gate=state,observer_starting=True,original_gate_live=live(record['process']),database_read=False)))
  else:
   print(json.dumps(dict(ready=True,gate=state,original_gate_live=live(record['process']),files=files,database_read=False)))
'''.replace('__RECORD__', repr(gate))
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', 'node031', 'python3', '-'],
                           input=code, text=True, capture_output=True, check=True, timeout=50)
    value = json.loads(reply.stdout)
    if not value['ready']:
        return dict(pending=True, ready=False, database_read=False, rows=[], launch_gate=value,
                    reason='Original launch metadata is not ready for collection')
    for name in [name for name in value['files'] if name != 'launch.json'] + ['launch.json']:
        path = evidence / name
        temporary = path.with_name(path.name + '.' + str(os.getpid()) + '.tmp')
        temporary.write_text(value['files'][name])
        temporary.replace(path)
    return dict(pending=False, ready=True, database_read=False)


def metadata(root):
    evidence = root / 'evidence/fdiv-gain-state-observe'
    spec = json.loads((evidence / 'experiment.json').read_text())
    launch = json.loads((evidence / 'launch.json').read_text())
    source = json.loads((evidence / 'source.json').read_text())
    assert digest(evidence / 'experiment.json') == launch['spec_sha256']
    assert digest(evidence / 'run.py') == launch['runner_sha256']
    assert source['head'] == spec['source'] == launch['source'] and source['base'] == spec['parent_source'] == launch['parent_source']
    assert source['source_reversal_byte_exact'] and source['new_exact_counter_names'] == spec['new_exact_counter_names']
    for name, value in source['source_sha256'].items():
        assert digest(evidence / 'source' / name) == value
    assert source['head'] == '6cbc9752d352b43b15fc2177b9451caccbfe74f9'
    assert source['new_exact_counter_names'] == ['diag_fdiv_gain_cancel_ex0_phase_' + name + '_total' for name in PARTITION]
    validation = json.loads((evidence / 'source-validation.json').read_text())
    assert validation['passed'] and validation['source'] == source['head']
    assert validation['source_sha256'] == source['source_sha256']
    return evidence, spec, launch, source


def collect(root, point=None):
    evidence, spec, launch, source = metadata(root)
    cases = [case for case in [spec['coremark'], *spec['short_cases']] if point is None or case['point'] == point]
    assert cases and (point is None or len(cases) == 1)
    code = r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
root=Path(RUN_DIR);spec=SPEC;launch=LAUNCH;cases=CASES;point=POINT
assert hashlib.sha256((root/'experiment.json').read_bytes()).hexdigest()==launch['spec_sha256']
assert hashlib.sha256((root/'run.py').read_bytes()).hexdigest()==launch['runner_sha256']
state=json.loads((root/'run-state.json').read_text());assert all(state[key]==launch['process'][key]for key in ['pid','start_ticks'])
def live(p):
 path=Path('/proc',str(p['pid']),'stat')
 return path.exists()and path.read_text().rsplit(')',1)[1].split()[19]==p['start_ticks']
if point:
 folder=root/point
 if not(folder/'process.json').exists():
  print(json.dumps(dict(ready=False,point=point,phase=state['phase'],database_read=False)))
  raise SystemExit(0)
 process=json.loads((folder/'process.json').read_text());start=json.loads((folder/'launch-observation.json').read_text())
 assert start['original_process_live']and start['process']==process and start['source']==spec['source']
 if live(process)or not(folder/'result.json').exists():
  print(json.dumps(dict(ready=False,point=point,phase=state['phase'],process=process,original_process_live=live(process),database_read=False)))
  raise SystemExit(0)
if not point and(live(state)or state['phase']!='complete'):
 print(json.dumps(dict(ready=False,controller=state,controller_live=live(state),database_read=False)))
else:
 if not point:assert state['completed']==3 and state['all_short_pairs_neutral']and state['coremark_gate_passed']
 build=json.loads(Path(spec['build_state']).read_text());assert build['phase']=='complete'and build['source']==spec['source'];guests=[]
 for case in cases:
  folder=root/case['point'];p=json.loads((folder/'process.json').read_text());start=json.loads((folder/'launch-observation.json').read_text());result=json.loads((folder/'result.json').read_text())
  assert not live(p)and start['original_process_live']and start['process']==result['process']==p
  assert result['source']==spec['source']and result['binary_sha256']==build['binary_sha256']and result['spec_sha256']==launch['spec_sha256']and result['runner_sha256']==launch['runner_sha256']
  assert result['original_process_closed']and result['exitcode']==0 and result['difftest_passed']and result['observation_neutrality']['passed']
  guests.append(dict(point=case['point'],process=p,original_process_live=False))
 names=[case['point']+'/'+name for case in cases for name in FILES]+['experiment.json','run.py','native_perf.py','tea_dip_query_endpoint_evidence.py','parent-closed-inputs.json']
 if not point:names+=['run-state.json','short-results.json']
 files={}
 for name in names:
  p=root/name
  with p.open('rb')as stream:sha=hashlib.file_digest(stream,'sha256').hexdigest()
  files[name]=dict(bytes=p.stat().st_size,sha256=sha)
 print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),root=str(root),controller=state,controller_live=live(state),point=point,build=build,guests=guests,files=files,database_read=False)))
'''.replace('RUN_DIR', repr(spec['node_run_dir'])).replace('SPEC', repr(spec)).replace('LAUNCH', repr(launch)).replace('CASES', repr(cases)).replace('POINT', repr(point)).replace('FILES', repr(FILES))
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=15', 'node031', 'python3', '-'],
                           input=code, text=True, capture_output=True, check=True, timeout=50)
    receipt = json.loads(reply.stdout)
    (evidence / (point + '-collection-status.json' if point else 'collection-status.json')).write_text(json.dumps(receipt, indent=2) + '\n')
    if not receipt['ready']:
        return receipt
    stage = evidence / 'closed-points' / point if point else root / 'fdiv-gain-state-observation'
    assert not stage.exists(), 'Analyze the existing closed gain-state archive'
    stage.mkdir(parents=True)
    command = 'tar -C ' + shlex.quote(receipt['root']) + ' -czf - ' + ' '.join(map(shlex.quote, receipt['files']))
    with (stage / 'originals.tar.gz').open('wb') as stream:
        subprocess.run(['proxychains', '-q', 'ssh', 'node031', command], stdout=stream, check=True, timeout=60)
    with tarfile.open(stage / 'originals.tar.gz') as package:
        assert {row.name for row in package.getmembers()} == set(receipt['files'])
        package.extractall(stage, filter='data')
    for name, row in receipt['files'].items():
        assert (stage / name).stat().st_size == row['bytes'] and digest(stage / name) == row['sha256']
    (stage / 'closure-receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    return receipt


def cause_frame(block, fields, cohort_fields):
    from native_perf import unique_counters
    queues = frame_counts(block, cohort_fields)
    values = {name: value for name, value in unique_counters(block).items() if name.rsplit('.', 1)[-1] in fields}
    assert len(values) == 22 and {name.rsplit('.', 1)[0] for name in values} == set(queues), 'Missing matched cancellation phase fields'
    for queue, counts in queues.items():
        phase_counts = {name.removeprefix('diag_fdiv_gain_').removesuffix('_total'): values[queue + '.' + name] for name in fields}
        assert min(phase_counts.values()) >= 0 and sum(phase_counts.values()) == counts['cancel_ex0_no_src']
        counts.update(phase_counts)
    return queues


def cause_span(before, after):
    span = gain_span(before, after)
    selected = span['delta']['selected']
    counts = {name: span['delta']['cancel_ex0_phase_' + name] for name in PARTITION}
    responses = span['delta']['cancel_ex0_no_src']
    assert sum(counts.values()) == responses <= span['delta']['cancel_ex0']
    bounds = {name: dict(lower=max(0, count - span['pending_begin']), upper=min(selected, count + span['pending_end']))
              for name, count in counts.items()}
    assert all(0 <= interval['lower'] <= interval['upper'] <= selected for interval in bounds.values())
    span.update(matched_ex0_no_src_response_phase_counts=counts,
                response_phase_fractions={name: count / responses for name, count in counts.items()} if responses else None,
                selected_origin_phase_count_bounds=bounds,
                selected_origin_phase_fraction_bounds={name: {key: value / selected for key, value in interval.items()}
                    for name, interval in bounds.items()} if selected else None)
    return span


def analyze(root, point=None):
    evidence, spec, launch, source = metadata(root)
    stage = evidence / 'closed-points' / point if point else root / 'fdiv-gain-state-observation'
    if not (stage / 'closure-receipt.json').exists():
        return dict(pending=True, database_read=False, rows=[], reason='Original gain-state guests are not archived closed')
    receipt = json.loads((stage / 'closure-receipt.json').read_text())
    assert receipt['ready'] and all(receipt['controller'][key] == launch['process'][key] for key in ['pid', 'start_ticks'])
    assert receipt['build']['source'] == spec['source'] and all(not row['original_process_live'] for row in receipt['guests'])
    assert (receipt['point'] == point and len(receipt['guests']) == 1) if point else (not receipt['controller_live'] and receipt['controller']['phase'] == 'complete')
    for name, row in receipt['files'].items():
        assert (stage / name).stat().st_size == row['bytes'] and digest(stage / name) == row['sha256']
    assert digest(stage / 'experiment.json') == launch['spec_sha256'] and digest(stage / 'run.py') == launch['runner_sha256']
    for name, key in [('native_perf.py', 'native_reader'), ('tea_dip_query_endpoint_evidence.py', 'helper'), ('parent-closed-inputs.json', 'parent_receipt')]:
        assert digest(stage / name) == spec[key + '_sha256']
    parent = json.loads((stage / 'parent-closed-inputs.json').read_text())
    assert parent['ready'] and parent['all_selected_original_guests_closed'] and parent['source'] == spec['parent_source'] and parent['binary_sha256'] == spec['parent_binary_sha256']
    parent_stage = root / 'fdiv-unit-state-observation'
    for name, row in parent['files'].items():
        assert (parent_stage / name).stat().st_size == row['bytes'] and digest(parent_stage / name) == row['sha256']
    assert digest(root / 'native_perf.py') == spec['native_reader_sha256']
    sys.path.insert(0, str(root))
    from native_perf import read_blocks, unique_counters
    import tea_dip_query_cohort_evidence as cohort
    assert digest(Path(cohort.__file__)) == '077fdeed1a24ce30cb92d0cdb86dadd7964112ccc3ae200d7100a56874b6b548'
    cohort_fields = json.loads((root / 'evidence/query-cohort-observe/experiment.json').read_text())['new_exact_counter_names']
    unit_spec = json.loads((root / 'evidence/fdiv-unit-state-observe/experiment.json').read_text())
    assert unit_spec['source'] == spec['parent_source']
    helper_spec = importlib.util.spec_from_file_location('closed_unit_state_helper', stage / 'tea_dip_query_endpoint_evidence.py')
    helper = importlib.util.module_from_spec(helper_spec)
    helper_spec.loader.exec_module(helper)
    rows = []
    for case in [case for case in [spec['coremark'], *spec['short_cases']] if point is None or case['point'] == point]:
        folder = stage / case['point']
        result = json.loads((folder / 'result.json').read_text())
        assert result['source'] == spec['source'] and result['binary_sha256'] == receipt['build']['binary_sha256']
        assert result['original_process_closed'] and result['exitcode'] == 0 and result['difftest_passed']
        command = json.loads((folder / 'command.json').read_text())
        for flag, value in [('-s', str(case['seed'])), ('-i', case['image']), ('-I', str(case['max_instr'])), ('--diff', spec['reference']), ('--cst-file', spec['cst'])]:
            assert command[command.index(flag) + 1] == value
        if case.get('warmup'):
            assert command[command.index('-W') + 1] == str(case['warmup'])
        blocks = read_blocks(folder / 'simulator_err.txt')
        assert len(blocks) == (2 if case.get('warmup') else 1)
        phases, previous = [], {}
        for label, (stamp, block) in zip(['warmup', 'roi'] if case.get('warmup') else ['coremark'], sorted(blocks.items())):
            counters = unique_counters(block)
            clocks, = [v for name, v in counters.items() if name.endswith('.rob.clock_cycle')]
            queues = cause_frame(block, spec['new_exact_counter_names'], cohort_fields)
            queue_rows = []
            for queue, counts in queues.items():
                before = previous.get(queue, {key: 0 for key in counts})
                span = cause_span(before, counts)
                selected_interval = counters[queue + '.diag_fdiv_query_gain_selected_uops']
                span['native_interval_gain_selected'] = selected_interval
                span['selection_interval_alignment_verified'] = span['delta']['selected'] == selected_interval
                queue_rows.append(dict(queue=queue, lifetime=counts, span=span))
            phases.append(dict(phase=label, native_print_stamp=stamp, native_clock_cycles=clocks, queues=queue_rows,
                               units=unit_counts(block, unit_spec['new_exact_counter_names'], clocks)))
            previous = queues
        old = read_blocks(parent_stage / case['point'] / 'simulator_err.txt')
        added = {name for block in blocks.values() for name in block} - {name for block in old.values() for name in block}
        assert len(added) == 22 and {name.rsplit('.', 1)[-1] for name in added} == set(spec['new_exact_counter_names'])
        neutral = helper.old_execution(parent_stage / case['point'], folder, added)
        assert neutral['passed']
        rows.append(dict(point=case['point'], parent_native_six_complete_table_neutrality=neutral, phases=phases))
    return dict(pending=False, database_read=True, completed=len(rows), source=source['head'], rows=rows,
                consumer_sha256=digest(Path(__file__)), closed_receipt_sha256=digest(stage / 'closure-receipt.json'),
                scope='Matched EX0 no-srcCancel gain response events classified by same-cycle phases. Lifetime counters are differenced across warmup/ROI; response phase fractions and selection-origin bounds have distinct denominators. Pending boundaries prevent equating response and selection populations. Inherited per-unit occupancy remains a separate view. Result-blocked includes normal publication delay; no unique instruction, service-time, critical-cycle, recoverable-CPI, suite score or adoption claim.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--action', choices=['collect', 'analyze'], required=True)
    parser.add_argument('--point', help='Use one closed point from the original four cases')
    args = parser.parse_args()
    root = args.report_dir.resolve()
    result = import_launch_metadata(root) if not (root / 'evidence/fdiv-gain-state-observe/launch.json').exists() else None
    if (root / 'evidence/fdiv-gain-state-observe/launch.json').exists():
        result = collect(root, args.point) if args.action == 'collect' else analyze(root, args.point)
    if args.action == 'analyze' and not result['pending']:
        folder = (root / 'evidence/fdiv-gain-state-observe/closed-points' / args.point if args.point else root / 'fdiv-gain-state-observation') / 'analysis'
        folder.mkdir(exist_ok=True)
        (folder / 'evidence.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
