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
STAGE = 'fdiv-ready-validation'


def metadata(root):
    evidence = root / 'evidence/fdiv-ready-ahead'
    launch = json.loads((evidence / 'validation-launch.json').read_text())
    source = json.loads((evidence / 'source.json').read_text())
    assert source['head'] == launch['spec']['source'] == SOURCE and source['base'] == CONTROL
    assert launch['spec']['control_source'] == CONTROL
    return evidence, launch


def collect(root):
    evidence, launch = metadata(root)
    stage = root / STAGE
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
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', 'node031', 'python3', '-'],
                           input=code, text=True, capture_output=True, check=True, timeout=50)
    receipt = json.loads(reply.stdout)
    if not receipt['ready']:
        return receipt
    assert not stage.exists(), 'Inspect the existing partial archive before collecting'
    stage.mkdir()
    command = 'tar -C ' + shlex.quote(receipt['root']) + ' -czf - ' + ' '.join(map(shlex.quote, receipt['files']))
    with (stage / 'originals.tar.gz').open('wb') as stream:
        subprocess.run(['proxychains', '-q', 'ssh', 'node031', command], stdout=stream, check=True, timeout=60)
    with tarfile.open(stage / 'originals.tar.gz') as archive:
        assert {entry.name for entry in archive.getmembers()} == set(receipt['files'])
        archive.extractall(stage, filter='data')
    verify(stage, receipt['files'])
    write(stage / 'closure-receipt.json', receipt)
    return receipt


def analyze(root):
    evidence, launch = metadata(root)
    stage = root / STAGE
    receipt_path = stage / 'closure-receipt.json'
    if not receipt_path.exists():
        return dict(pending=True, database_read=False, reason='Original validation closure not archived')
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['controller_live'] and receipt['all_original_processes_closed']
    assert all(receipt['controller'][key] == launch['process'][key] for key in ['pid', 'start_ticks'])
    verify(stage, receipt['files'])
    assert digest(stage / 'experiment.json') == launch['spec_sha256']
    spec = json.loads((stage / 'experiment.json').read_text())
    assert spec == launch['spec'] and spec['source'] == SOURCE and spec['control_source'] == CONTROL
    if receipt['controller']['phase'] == 'failed':
        return dict(pending=False, database_read=False, validation_passed=False,
                    original_controller=receipt['controller'], performance_speedup=None,
                    reason='Original controller failed; inspect archived stderr and individual results')
    assert receipt['controller']['phase'] == 'complete' and receipt['controller']['completed'] == 3
    build = json.loads((stage / 'build-identity.json').read_text())
    assert build['source'] == SOURCE and build['phase'] == 'complete'
    for name in ['tea_dip_fdiv_ready_run.py', 'tea_dip_wb_protocol_micro.py', 'native_perf.py', 'tea_dip_query_endpoint_evidence.py']:
        assert digest(stage / name) == spec['input_hashes'][str(Path(launch['run_dir']) / name)]
    controls = root / 'wb-query-endpoint-focused'
    control_receipt = json.loads((controls / 'closure-receipt.json').read_text())
    assert control_receipt['ready'] and not control_receipt['controller_live'] and control_receipt['all_original_guests_closed']
    verify(controls, control_receipt['files'])
    coremark_receipt = json.loads((stage / 'control-coremark-closure.json').read_text())
    archived_coremark = root / 'wb-query-endpoint-validation'
    assert coremark_receipt['ready'] and not coremark_receipt['original_guest_live']
    verify(archived_coremark, coremark_receipt['files'])
    assert coremark_receipt['build']['source'] == CONTROL
    assert digest(root / 'native_perf.py') == digest(stage / 'native_perf.py')
    assert digest(Path(__file__).with_name('tea_dip_query_endpoint_evidence.py')) == digest(stage / 'tea_dip_query_endpoint_evidence.py')
    # Validate every identity, result and command before the first DB read.
    cases = [spec['coremark'], *spec['short_cases']]
    for case in cases:
        folder = stage / case['point']
        result = json.loads((folder / 'result.json').read_text())
        process = json.loads((folder / 'process.json').read_text())
        started = json.loads((folder / 'launch-observation.json').read_text())
        assert result['source'] == SOURCE and result['binary_sha256'] == build['binary_sha256']
        assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
        assert (folder / 'exitcode').read_text().strip() == '0' and all(result['sampler_flags'].values())
        assert result['process'] == started['process'] == process and process in receipt['processes']
        assert started['original_process_live'] and started['source'] == SOURCE and started['binary_sha256'] == build['binary_sha256']
        command = json.loads((folder / 'command.json').read_text())
        assert command == started['command'] and command[6] == build['binary']
        for flag in ['-I', '-C', '-s', '-i', '--diff', '--cst-file', '--dump-select-db']:
            assert command[command.index(flag) + 1] == case['command'][case['command'].index(flag) + 1]
        if '-W' in case['command']:
            assert command[command.index('-W') + 1] == case['command'][case['command'].index('-W') + 1]
    micro = stage / 'protocol-micro'
    micro_spec = json.loads((micro / 'experiment.json').read_text())
    micro_results = json.loads((micro / 'results.json').read_text())
    assert micro_spec['binaries']['candidate']['source'] == SOURCE and micro_spec['binaries']['stage_control']['source'] == CONTROL
    assert micro_spec['binaries']['candidate']['sha256'] == build['binary_sha256']
    assert micro_spec['binaries']['stage_control']['sha256'] == coremark_receipt['build']['binary_sha256']
    assert micro_spec['runner_sha256'] == digest(stage / 'tea_dip_wb_protocol_micro.py')
    assert len(micro_results) == 12 and all(row['passed'] and row['original_process_closed'] and row['exitcode'] == 0 for row in micro_results)
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
        before = controls / 'query_endpoint' / case['point']
        baseline = json.loads((before / 'result.json').read_text())
        assert baseline['source'] == CONTROL and baseline['binary_sha256'] == coremark_receipt['build']['binary_sha256']
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
        before, after = controls / 'query_endpoint' / case['point'], stage / case['point']
        comparison = pair(before, after)
        comparisons.append(dict(point=case['point'],comparison=comparison,
              native_six_table_equality=old_execution(before,after,set())))
    return dict(pending=False,database_read=True,validation_passed=True,source=SOURCE,control_source=CONTROL,
                coremark_gate=coremark,micro_pairs=micro_pairs,short_pairs=comparisons,
                closed_receipt_sha256=digest(receipt_path),consumer_sha256=digest(Path(__file__)),
                scope='One closed candidate validation: CoreMark neutral gate, six focused FP protocols and three1M+1M SPEC windows. Micro trap counts are diagnostic progress, not intended-work CPI or a suite score. Short native CPI uses each own retirement denominator; exact TEA budgets require observed endpoint alignment. No formal/full-suite result, physical timing or adoption claim.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--collect', action='store_true')
    args = parser.parse_args(); root = args.report_dir.resolve()
    if args.collect:
        receipt = collect(root)
        if not receipt['ready']:
            print(json.dumps(dict(pending=True,database_read=False,observation=receipt),indent=2));return
    value = analyze(root)
    if not value['pending']:
        output = root / STAGE / 'analysis'; output.mkdir(exist_ok=True)
        write(output / 'evidence.json', value)
    print(json.dumps(value,indent=2))


if __name__ == '__main__':
    main()
