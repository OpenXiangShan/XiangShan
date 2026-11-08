"""Collect the original FDIV-ready validation only after closure, then analyze its fixed controls."""
from pathlib import Path
import argparse
import json
import re
import shlex
import subprocess
import sys
import tarfile

from tea_dip_wb_protocol_micro import digest, write
from tea_dip_query_endpoint_evidence import old_execution, pair, verify

SOURCE = '39a163eb3910ff9ecab02df4ac8e3c99d772feeb'
CONTROL = '2915a6b022bd957570bd961a01611026c2220fb7'


def metadata(root, turnover=False):
    evidence = root / 'evidence' / ('fdiv-turnover-20261108' if turnover else 'fdiv-ready-ahead')
    expected_source = '8f8ece60ac8b08b358743bc9db15a5a227dde0b8' if turnover else SOURCE
    expected_control = SOURCE if turnover else CONTROL
    launch = json.loads((evidence / 'validation-launch.json').read_text())
    source = json.loads((evidence / 'source.json').read_text())
    assert source['head'] == launch['spec']['source'] == expected_source and source['base'] == expected_control
    assert launch['spec']['control_source'] == expected_control
    return evidence, launch


def collect(root, point=None, turnover=False):
    evidence, launch = metadata(root, turnover)
    stage_name = Path(launch['run_dir']).name
    cases = [launch['spec']['coremark'], *launch['spec']['short_cases']]
    assert point is None or point in {case['point'] for case in cases}
    stage = root / stage_name if point is None else root / (stage_name + '-points') / point
    if (stage / 'closure-receipt.json').exists():
        return json.loads((stage / 'closure-receipt.json').read_text())
    code = r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
launch=__LAUNCH__;root=Path(launch['run_dir'])
def live(row):
 p=Path('/proc',str(row['pid']),'stat')
 return p.exists()and p.read_text().rsplit(')',1)[1].split()[19]==row['start_ticks']
assert hashlib.sha256((root/'experiment.json').read_bytes()).hexdigest()==launch['spec_sha256']
state=json.loads((root/'run-state.json').read_text())
assert all(state[key]==launch['process'][key]for key in ['pid','start_ticks'])
controller_live=live(state)
if controller_live or state['phase']not in ['complete','failed']:
 print(json.dumps(dict(ready=False,utc=datetime.now(timezone.utc).isoformat(),controller=state,controller_live=controller_live,database_read=False)))
else:
 processes=[json.loads(p.read_text())for p in root.rglob('process.json')]
 child=root/'micro-controller.json'
 if child.exists():processes.append(json.loads(child.read_text()))
 live_processes=[p for p in processes if live(p)]
 if live_processes:
  print(json.dumps(dict(ready=False,utc=datetime.now(timezone.utc).isoformat(),controller=state,controller_live=False,live_processes=live_processes,database_read=False)))
 else:
  files={}
  for p in root.rglob('*'):
   if p.is_file():
    with p.open('rb')as stream:sha=hashlib.file_digest(stream,'sha256').hexdigest()
    files[str(p.relative_to(root))]=dict(bytes=p.stat().st_size,sha256=sha)
  print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),root=str(root),controller=state,controller_live=False,all_original_processes_closed=True,processes=processes,files=files,database_read=False)))
'''.replace('__LAUNCH__', repr(launch))
    if point is not None:
        code = r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
launch=__LAUNCH__;point=__POINT__;root=Path(launch['run_dir']);folder=root/point
assert hashlib.sha256((root/'experiment.json').read_bytes()).hexdigest()==launch['spec_sha256']
state=json.loads((root/'run-state.json').read_text());assert all(state[k]==launch['process'][k]for k in ['pid','start_ticks'])
def live(row):
 p=Path('/proc',str(row['pid']),'stat');return p.exists()and p.read_text().rsplit(')',1)[1].split()[19]==row['start_ticks']
if not(folder/'result.json').exists():
 print(json.dumps(dict(ready=False,point=point,controller_live=live(state),database_read=False)))
else:
 result=json.loads((folder/'result.json').read_text());process=json.loads((folder/'process.json').read_text())
 if live(process):print(json.dumps(dict(ready=False,point=point,original_process_live=True,database_read=False)))
 else:
  build=json.loads((root/'build-identity.json').read_text());assert build['phase']=='complete'and build['source']==result['source']==launch['spec']['source']and result['binary_sha256']==build['binary_sha256']
  assert result['process']==process and result['original_process_closed']and result['exitcode']==0 and result['difftest_passed']
  names=['command.json','result.json','exitcode','process.json','launch-observation.json','simulator_out.txt','simulator_err.txt','simulator.db']
  files={n:dict(bytes=(folder/n).stat().st_size,sha256=hashlib.sha256((folder/n).read_bytes()).hexdigest())for n in names}
  print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),root=str(folder),point=point,controller=state,controller_live=live(state),original_process_live=False,process=process,build=build,spec_sha256=launch['spec_sha256'],files=files,database_read=False)))
'''.replace('__LAUNCH__',repr(launch)).replace('__POINT__',repr(point))
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', 'node031', 'python3', '-'],
                           input=code, text=True, capture_output=True, check=True, timeout=50)
    receipt = json.loads(reply.stdout)
    if not receipt['ready']:
        return receipt
    assert not stage.exists(), 'Inspect the existing partial archive before collecting'
    stage.mkdir(parents=True)
    command = 'tar -C ' + shlex.quote(receipt['root']) + ' -czf - ' + ' '.join(map(shlex.quote, receipt['files']))
    with (stage / 'originals.tar.gz').open('wb') as stream:
        subprocess.run(['proxychains', '-q', 'ssh', 'node031', command], stdout=stream, check=True, timeout=60)
    with tarfile.open(stage / 'originals.tar.gz') as archive:
        assert {entry.name for entry in archive.getmembers()} == set(receipt['files'])
        archive.extractall(stage, filter='data')
    verify(stage, receipt['files'])
    write(stage / 'closure-receipt.json', receipt)
    return receipt


def analyze(root, turnover=False):
    evidence, launch = metadata(root, turnover)
    source, control_source = launch['spec']['source'], launch['spec']['control_source']
    stage = root / Path(launch['run_dir']).name
    receipt_path = stage / 'closure-receipt.json'
    if not receipt_path.exists():
        return dict(pending=True, database_read=False, reason='Original validation closure not archived')
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['controller_live'] and receipt['all_original_processes_closed']
    assert all(receipt['controller'][key] == launch['process'][key] for key in ['pid', 'start_ticks'])
    verify(stage, receipt['files'])
    assert digest(stage / 'experiment.json') == launch['spec_sha256']
    spec = json.loads((stage / 'experiment.json').read_text())
    assert spec == launch['spec'] and spec['source'] == source and spec['control_source'] == control_source
    if receipt['controller']['phase'] == 'failed':
        return dict(pending=False, database_read=False, validation_passed=False,
                    original_controller=receipt['controller'], performance_speedup=None,
                    reason='Original controller failed; inspect archived stderr and individual results')
    assert receipt['controller']['phase'] == 'complete' and receipt['controller']['completed'] == 3
    build = json.loads((stage / 'build-identity.json').read_text())
    assert build['source'] == source and build['phase'] == 'complete'
    for name in ['tea_dip_fdiv_ready_run.py', 'tea_dip_wb_protocol_micro.py', 'native_perf.py', 'tea_dip_query_endpoint_evidence.py']:
        assert digest(stage / name) == spec['input_hashes'][str(Path(launch['run_dir']) / name)]
    controls = root / ('fdiv-ready-validation' if turnover else 'wb-query-endpoint-focused')
    control_receipt = json.loads((controls / 'closure-receipt.json').read_text())
    assert control_receipt['ready'] and not control_receipt['controller_live'] and control_receipt['all_original_processes_closed' if turnover else 'all_original_guests_closed']
    verify(controls, control_receipt['files'])
    coremark_receipt = json.loads((stage / 'control-coremark-closure.json').read_text())
    archived_coremark = root / ('fdiv-ready-validation' if turnover else 'wb-query-endpoint-validation')
    assert coremark_receipt['ready'] and not coremark_receipt['original_guest_live']
    verify(archived_coremark, coremark_receipt['files'])
    assert coremark_receipt['build']['source'] == control_source
    assert digest(root / 'native_perf.py') == digest(stage / 'native_perf.py')
    assert digest(Path(__file__).with_name('tea_dip_query_endpoint_evidence.py')) == digest(stage / 'tea_dip_query_endpoint_evidence.py')
    # Validate every identity, result and command before the first DB read.
    cases = [spec['coremark'], *spec['short_cases']]
    for case in cases:
        folder = stage / case['point']
        result = json.loads((folder / 'result.json').read_text())
        process = json.loads((folder / 'process.json').read_text())
        started = json.loads((folder / 'launch-observation.json').read_text())
        assert result['source'] == source and result['binary_sha256'] == build['binary_sha256']
        assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
        assert (folder / 'exitcode').read_text().strip() == '0' and all(result['sampler_flags'].values())
        assert result['process'] == started['process'] == process and process in receipt['processes']
        assert started['original_process_live'] and started['source'] == source and started['binary_sha256'] == build['binary_sha256']
        command = json.loads((folder / 'command.json').read_text())
        assert command == started['command'] and command[6] == build['binary']
        for flag in ['-I', '-C', '-s', '-i', '--diff', '--cst-file', '--dump-select-db']:
            assert command[command.index(flag) + 1] == case['command'][case['command'].index(flag) + 1]
        if '-W' in case['command']:
            assert command[command.index('-W') + 1] == case['command'][case['command'].index('-W') + 1]
    micro = stage / 'protocol-micro'
    micro_spec = json.loads((micro / 'experiment.json').read_text())
    micro_results = json.loads((micro / 'results.json').read_text())
    assert micro_spec['binaries']['candidate']['source'] == source and micro_spec['binaries']['stage_control']['source'] == control_source
    assert micro_spec['binaries']['candidate']['sha256'] == build['binary_sha256']
    assert micro_spec['binaries']['stage_control']['sha256'] == coremark_receipt['build']['binary_sha256']
    assert micro_spec['runner_sha256'] == digest(stage / 'tea_dip_wb_protocol_micro.py')
    assert len(micro_results) == (18 if turnover else 12) and all(row['passed'] and row['original_process_closed'] and row['exitcode'] == 0 for row in micro_results)
    for row in micro_results:
        assert row['process'] in receipt['processes']
        identity = micro_spec['binaries'][row['variant']]
        assert row['binary_sha256'] == identity['sha256']
        folder = micro / 'runs' / row['variant'] / row['case']
        case = next(case for case in micro_spec['cases'] if case['case'] == row['case'])
        assert row['image_sha256'] == case['image_sha256']
        text = re.sub(r'\x1b\[[0-9;]*m', '', (folder / 'simulator_out.txt').read_text())
        trap = re.search(r'HIT GOOD TRAP at pc = (0x[0-9a-fA-F]+)', text)
        counts = re.search(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)', text)
        assert (folder / 'exitcode').read_text().strip() == '0' and 'Difftest enabled' in text
        assert trap and int(trap[1],16) == int(case['pass_pc'],16)
        assert counts and [int(v.replace(',','')) for v in counts.groups()] == [row['instructions'],row['cycles']]
        assert 1700 < row['instructions'] < 3000 and not any(v in text for v in ['ABORT','HIT CRITICAL','HIT BAD TRAP'])
        command = json.loads((folder / 'command.json').read_text())
        assert command[6] == identity['binary']
        for flag,value in [('-i',case['image']),('-s','99'),('-I','100000'),('-C','200000'),('--diff',micro_spec['reference']),('--dump-select-db',micro_spec['selectors'])]:
            assert command[command.index(flag)+1] == value
        for name,value in [('enableTea0',1),('enableDip0',1),('teaSamplePeriod0',1000),('dipSamplePeriod0',1000)]:
            assert re.search(r'^'+name+' = '+str(value)+'$',text,re.M)
    for case in spec['short_cases']:
        before = controls / case['point'] if turnover else controls / 'query_endpoint' / case['point']
        baseline = json.loads((before / 'result.json').read_text())
        assert baseline['source'] == control_source and baseline['binary_sha256'] == coremark_receipt['build']['binary_sha256']
        assert baseline['original_process_closed'] and baseline['exitcode'] == 0 and baseline['difftest_passed']
        command = json.loads((before / 'command.json').read_text())
        for flag in ['-W','-I','-C','-s','-i','--diff','--dump-select-db']:
            assert command[command.index(flag)+1] == case['command'][case['command'].index(flag)+1]
    sys.path.insert(0, str(root))
    coremark = old_execution(archived_coremark / 'coremark', stage / 'coremark', set())
    assert coremark['passed']
    micro_pairs = []
    for case in micro_spec['cases']:
        rows = [row for row in micro_results if row['case'] == case['case']]
        assert len(rows) == 2 and all(row['image_sha256'] == case['image_sha256'] for row in rows)
        by_variant = {row['variant']: row for row in rows}
        before, after = [micro / 'runs' / name / case['case'] for name in ['stage_control', 'candidate']]
        equality = old_execution(before, after, set())
        if case['case'] == 'fixed_only':
            assert equality['passed']
        micro_pairs.append(dict(case=case['case'],native_six_table_equality=equality,
              control_trap_counts=[by_variant['stage_control'][key] for key in ['instructions','cycles']],
              candidate_trap_counts=[by_variant['candidate'][key] for key in ['instructions','cycles']],
              trap_cycle_change=by_variant['candidate']['cycles']-by_variant['stage_control']['cycles'],performance_speedup=None))
    comparisons = []
    for case in spec['short_cases']:
        before = controls / case['point'] if turnover else controls / 'query_endpoint' / case['point']
        after = stage / case['point']
        comparison = pair(before, after)
        comparisons.append(dict(point=case['point'],comparison=comparison,
              native_six_table_equality=old_execution(before,after,set())))
    return dict(pending=False,database_read=True,validation_passed=True,source=source,control_source=control_source,
                coremark_gate=coremark,micro_pairs=micro_pairs,short_pairs=comparisons,
                closed_receipt_sha256=digest(receipt_path),consumer_sha256=digest(Path(__file__)),
                scope=f'One closed candidate validation: CoreMark neutral gate, {"nine" if turnover else "six"} focused FP protocols and three1M+1M SPEC windows. Micro trap counts are diagnostic progress, not intended-work CPI or a suite score. Short native CPI uses each own retirement denominator; exact TEA budgets require observed endpoint alignment. No formal/full-suite result, physical timing or adoption claim.')



def analyze_point(root, point, turnover=False):
    evidence, launch = metadata(root, turnover)
    source, control_source = launch['spec']['source'], launch['spec']['control_source']
    stage_name = Path(launch['run_dir']).name
    cases = [launch['spec']['coremark'], *launch['spec']['short_cases']]
    case, = [case for case in cases if case['point'] == point]
    folder = root / (stage_name + '-points') / point
    if not (folder / 'closure-receipt.json').exists():
        return dict(pending=True,database_read=False,point=point,reason='Original point closure not archived')
    receipt = json.loads((folder / 'closure-receipt.json').read_text())
    assert receipt['ready'] and receipt['point'] == point and not receipt['original_process_live']
    assert receipt['spec_sha256'] == launch['spec_sha256']
    assert all(receipt['controller'][k] == launch['process'][k] for k in ['pid','start_ticks'])
    verify(folder,receipt['files'])
    row = json.loads((folder / 'result.json').read_text())
    started = json.loads((folder / 'launch-observation.json').read_text())
    assert row['source'] == source and row['binary_sha256'] == receipt['build']['binary_sha256']
    assert row['original_process_closed'] and row['difftest_passed'] and row['exitcode'] == 0 and (folder / 'exitcode').read_text().strip() == '0'
    assert row['process'] == started['process'] == receipt['process'] and started['original_process_live']
    command = json.loads((folder / 'command.json').read_text())
    assert command == started['command'] and command[6] == receipt['build']['binary']
    for flag in ['-I','-C','-s','-i','--diff','--cst-file','--dump-select-db']:
        assert command[command.index(flag)+1] == case['command'][case['command'].index(flag)+1]
    assert digest(root / 'native_perf.py') == launch['spec']['input_hashes'][str(Path(launch['run_dir'])/'native_perf.py')]
    assert digest(Path(__file__).with_name('tea_dip_query_endpoint_evidence.py')) == launch['spec']['input_hashes'][str(Path(launch['run_dir'])/'tea_dip_query_endpoint_evidence.py')]
    control_root = root / ('fdiv-ready-validation' if turnover else ('wb-query-endpoint-validation' if point == 'coremark' else 'wb-query-endpoint-focused'))
    control_receipt = json.loads((control_root / 'closure-receipt.json').read_text())
    if turnover:assert control_receipt['ready'] and not control_receipt['controller_live'] and control_receipt['all_original_processes_closed']
    elif point == 'coremark':assert control_receipt['ready'] and not control_receipt['original_guest_live']
    else:assert control_receipt['ready'] and not control_receipt['controller_live'] and control_receipt['all_original_guests_closed']
    verify(control_root,control_receipt['files'])
    control = control_root / point if turnover or point == 'coremark' else control_root / 'query_endpoint' / point
    baseline = json.loads((control / 'result.json').read_text())
    assert baseline['source'] == control_source and baseline['original_process_closed'] and baseline['exitcode'] == 0 and baseline['difftest_passed']
    control_command = json.loads((control / 'command.json').read_text())
    for flag in ['-I','-C','-s','-i','--diff','--dump-select-db']:
        assert control_command[control_command.index(flag)+1] == command[command.index(flag)+1]
    if point != 'coremark':
        assert control_command[control_command.index('-W')+1] == command[command.index('-W')+1] == case['command'][case['command'].index('-W')+1]
    sys.path.insert(0,str(root))
    equality = old_execution(control,folder,set())
    comparison = None if point == 'coremark' else pair(control,folder)
    return dict(pending=False,database_read=True,point=point,source=source,control_source=control_source,
                parent_controller_live_at_collection=receipt['controller_live'],original_point_closed=True,
                native_six_table_equality=equality,comparison=comparison,closed_receipt_sha256=digest(folder/'closure-receipt.json'),
                consumer_sha256=digest(Path(__file__)),scope='One closed point only. The original parent and other points may still be live; their databases are not opened. No whole-stage gate, suite score or adoption claim.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--collect', action='store_true')
    parser.add_argument('--turnover', action='store_true', help='Use the registered 8f8/39a turnover experiment and its closed controls')
    parser.add_argument('--point', help='Collect/analyze one closed point while the original parent continues')
    args = parser.parse_args(); root = args.report_dir.resolve()
    _, launch = metadata(root, args.turnover)
    stage_name = Path(launch['run_dir']).name
    if args.collect:
        receipt = collect(root, args.point, args.turnover)
        if not receipt['ready']:
            print(json.dumps(dict(pending=True,database_read=False,observation=receipt),indent=2));return
    value = analyze_point(root,args.point,args.turnover) if args.point else analyze(root,args.turnover)
    if not value['pending']:
        output = (root / (stage_name+'-points') / args.point if args.point else root / stage_name) / 'analysis'; output.mkdir(exist_ok=True)
        write(output / 'evidence.json', value)
    print(json.dumps(value,indent=2))


if __name__ == '__main__':
    main()
