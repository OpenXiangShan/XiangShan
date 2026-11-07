"""Archive closed internal divide/sqrt observers and audit unit-cycle partitions."""
from pathlib import Path
import argparse
import importlib.util
import json
import shlex
import subprocess
import sys
import tarfile

from tea_dip_query_cohort_evidence import digest

FILES = ['command.json', 'result.json', 'exitcode', 'simulator_out.txt', 'simulator_err.txt',
         'simulator.db', 'process.json', 'launch-observation.json']
PARTITION = ['div_prepare', 'div_iterate', 'div_denormal_post', 'div_result_blocked', 'div_result_ready',
             'sqrt_prepare', 'sqrt_iterate', 'sqrt_result_blocked', 'sqrt_result_ready', 'both_active', 'other']


def metadata(root):
    evidence = root / 'evidence/fdiv-unit-state-observe'
    spec = json.loads((evidence / 'experiment.json').read_text())
    launch = json.loads((evidence / 'launch.json').read_text())
    source = json.loads((evidence / 'source.json').read_text())
    assert digest(evidence / 'experiment.json') == launch['spec_sha256']
    assert digest(evidence / 'run.py') == launch['runner_sha256']
    assert source['head'] == spec['source'] == launch['source'] and source['base'] == spec['parent_source'] == launch['parent_source']
    assert source['source_reversal_byte_exact'] and source['new_exact_counter_names'] == spec['new_exact_counter_names']
    for name, value in source['source_sha256'].items():
        assert digest(evidence / 'source' / name) == value
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
    stage = evidence / 'closed-points' / point if point else root / 'fdiv-unit-state-observation'
    assert not stage.exists(), 'Analyze the existing closed unit-state archive'
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


def unit_counts(block, fields, clock_cycles):
    from native_perf import unique_counters
    values = {name: value for name, value in unique_counters(block).items() if name.rsplit('.', 1)[-1] in fields}
    assert len(values) == 32, 'Missing or ambiguous internal divide/sqrt fields'
    prefixes = sorted({name.rsplit('.', 1)[0] for name in values})
    assert len(prefixes) == 2
    rows = []
    for prefix in prefixes:
        counts = {name.removeprefix('diag_fdiv_unit_'): values[prefix + '.' + name] for name in fields}
        busy = counts['busy_cycles']
        assert min(counts.values()) >= 0 and sum(counts[name + '_cycles'] for name in PARTITION) == busy <= clock_cycles
        assert counts['busy_wakeup_denied'] <= busy and counts['busy_flush'] <= busy
        assert counts['start_accepted'] <= clock_cycles and counts['finish_handshake'] <= clock_cycles
        rows.append(dict(unit=prefix, counts=counts, busy_fraction=busy / clock_cycles,
                         phase_fraction_of_busy={name: counts[name + '_cycles'] / busy for name in PARTITION} if busy else None))
    return rows


def analyze(root, point=None):
    evidence, spec, launch, source = metadata(root)
    stage = evidence / 'closed-points' / point if point else root / 'fdiv-unit-state-observation'
    if not (stage / 'closure-receipt.json').exists():
        return dict(pending=True, database_read=False, rows=[], reason='Original unit-state guests are not archived closed')
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
    parent_stage = root / 'query-cohort-observation'
    for name, row in parent['files'].items():
        assert (parent_stage / name).stat().st_size == row['bytes'] and digest(parent_stage / name) == row['sha256']
    sys.path.insert(0, str(root))
    from native_perf import read_blocks, unique_counters
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
        phases = []
        for label, (stamp, block) in zip(['warmup', 'roi'] if case.get('warmup') else ['coremark'], sorted(blocks.items())):
            counters = unique_counters(block)
            clocks, = [v for name, v in counters.items() if name.endswith('.rob.clock_cycle')]
            phases.append(dict(phase=label, native_print_stamp=stamp, native_clock_cycles=clocks, units=unit_counts(block, spec['new_exact_counter_names'], clocks)))
        old = read_blocks(parent_stage / case['point'] / 'simulator_err.txt')
        added = {name for block in blocks.values() for name in block} - {name for block in old.values() for name in block}
        assert len(added) == 32 and {name.rsplit('.', 1)[-1] for name in added} == set(spec['new_exact_counter_names'])
        neutral = helper.old_execution(parent_stage / case['point'], folder, added)
        assert neutral['passed']
        rows.append(dict(point=case['point'], parent_native_six_complete_table_neutrality=neutral, phases=phases))
    return dict(pending=False, database_read=True, completed=len(rows), source=source['head'], rows=rows,
                consumer_sha256=digest(Path(__file__)), closed_receipt_sha256=digest(stage / 'closure-receipt.json'),
                scope='Per-unit cycle occupancy. Busy phase classes partition each unit, annotations overlap. Result-blocked includes normal publication latency. Starts/finishes span interval boundaries; no service-time quotient, matched gain-cancel linkage, unique instruction, critical-cycle or CPI conversion. No suite score or adoption.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--action', choices=['collect', 'analyze'], required=True)
    parser.add_argument('--point', help='Use one closed point from the original four cases')
    args = parser.parse_args()
    root = args.report_dir.resolve()
    result = collect(root, args.point) if args.action == 'collect' else analyze(root, args.point)
    if args.action == 'analyze' and not result['pending']:
        folder = (root / 'evidence/fdiv-unit-state-observe/closed-points' / args.point if args.point else root / 'fdiv-unit-state-observation') / 'analysis'
        folder.mkdir(exist_ok=True)
        (folder / 'evidence.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
