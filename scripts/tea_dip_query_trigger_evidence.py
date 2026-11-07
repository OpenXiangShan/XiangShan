"""Collect closed original query diagnostics and audit exact counter populations offline."""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import importlib.util
import json
import shlex
import subprocess
import sys
import tarfile

FILES = ['command.json', 'result.json', 'exitcode', 'simulator_out.txt', 'simulator_err.txt',
         'simulator.db', 'process.json', 'launch-observation.json']


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def collect(root):
    evidence = root / 'evidence/query-trigger-observe'
    spec = json.loads((evidence / 'experiment.json').read_text())
    launch = json.loads((evidence / 'launch.json').read_text())
    original = next(row for row in launch['children'] if row['mode'] == 'observation')
    code = r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
root=Path(RUN_DIR);spec=SPEC;original=ORIGINAL
assert hashlib.sha256((root/'experiment.json').read_bytes()).hexdigest()==SPEC_SHA
assert hashlib.sha256((root/'run.py').read_bytes()).hexdigest()==RUNNER_SHA
state=json.loads((root/'run-state.json').read_text())
assert state['pid']==original['pid'] and state['start_ticks']==original['start_ticks']
def live(process):
 p=Path('/proc',str(process['pid']),'stat')
 return p.exists() and p.read_text().rsplit(')',1)[1].split()[19]==process['start_ticks']
controller_live=live(state)
if controller_live or state['phase']!='complete':
 print(json.dumps(dict(ready=False,phase=state['phase'],controller=state,controller_live=controller_live,database_read=False)))
else:
 build=json.loads(Path(spec['build_state']).read_text());assert build['source']==spec['source'] and build['phase']=='complete'
 rows=[]
 for case in [spec['coremark'],*spec['short_cases'],spec['formal_case']]:
  folder=root/case['point'];process=json.loads((folder/'process.json').read_text());start=json.loads((folder/'launch-observation.json').read_text());result=json.loads((folder/'result.json').read_text())
  assert not live(process) and start['original_process_live'] and start['process']==result['process']==process
  assert result['source']==spec['source'] and result['binary_sha256']==build['binary_sha256']
  assert result['original_process_closed'] and result['exitcode']==0 and result['difftest_passed']
  rows.append(dict(point=case['point'],process=process,original_process_live=False))
 files={}
 for name in [case['point']+'/'+name for case in [spec['coremark'],*spec['short_cases'],spec['formal_case']] for name in FILES]+['experiment.json','run.py','native_perf.py','tea_dip_query_endpoint_evidence.py','run-state.json','short-results.json','formal-result.json']:
  path=root/name
  with path.open('rb') as stream:sha=hashlib.file_digest(stream,'sha256').hexdigest()
  files[name]=dict(bytes=path.stat().st_size,sha256=sha)
 print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),root=str(root),controller=state,controller_live=False,guests=rows,build=build,files=files,database_read=False)))
'''.replace('RUN_DIR', repr(launch['run_dir'])).replace('SPEC_SHA', repr(launch['spec_sha256'])).replace('RUNNER_SHA', repr(launch['runner_sha256'])).replace('SPEC', repr(spec)).replace('ORIGINAL', repr(original)).replace('FILES', repr(FILES))
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=15', 'node031', 'python3', '-'],
                           input=code, text=True, capture_output=True, timeout=50, check=True)
    receipt = json.loads(reply.stdout)
    (evidence / 'collection-status.json').write_text(json.dumps(receipt, indent=2) + '\n')
    if not receipt['ready']:
        return receipt
    stage = root / 'query-trigger-observation'
    assert not stage.exists(), 'Use the existing closed archive for offline analysis'
    stage.mkdir()
    command = 'tar -C ' + shlex.quote(receipt['root']) + ' -czf - ' + ' '.join(map(shlex.quote, receipt['files']))
    with (stage / 'originals.tar.gz').open('wb') as stream:
        subprocess.run(['proxychains', '-q', 'ssh', 'node031', command], stdout=stream, check=True, timeout=60)
    with tarfile.open(stage / 'originals.tar.gz') as package:
        assert {row.name for row in package.getmembers()} == set(receipt['files'])
        package.extractall(stage, filter='data')
    for name, row in receipt['files'].items():
        path = stage / name
        assert path.stat().st_size == row['bytes'] and digest(path) == row['sha256']
    (stage / 'closure-receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    return receipt


def analyze(root):
    evidence = root / 'evidence/query-trigger-observe'
    stage = root / 'query-trigger-observation'
    receipt_path = stage / 'closure-receipt.json'
    if not receipt_path.exists():
        return dict(pending=True, database_read=False, rows=[], reason='Original diagnostic controller and guests not archived closed')
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['controller_live'] and receipt['controller']['phase'] == 'complete'
    launch = json.loads((evidence / 'launch.json').read_text())
    original = next(row for row in launch['children'] if row['mode'] == 'observation')
    assert all(receipt['controller'][key] == original[key] for key in ['pid', 'start_ticks'])
    spec_path = evidence / 'experiment.json'
    spec = json.loads(spec_path.read_text())
    source = json.loads((evidence / 'source.json').read_text())
    assert source['head'] == spec['source'] == receipt['build']['source']
    assert digest(spec_path) == launch['spec_sha256'] == digest(stage / 'experiment.json')
    assert digest(stage / 'run.py') == launch['runner_sha256']
    for name, row in receipt['files'].items():
        path = stage / name
        assert path.stat().st_size == row['bytes'] and digest(path) == row['sha256'], name
    helper_path = stage / 'tea_dip_query_endpoint_evidence.py'
    assert digest(helper_path) == spec['helper_sha256'] and digest(stage / 'native_perf.py') == spec['native_reader_sha256']
    # Complete parent archives must be verified before any parent or diagnostic DB is opened.
    parent_locations = [root / 'wb-query-endpoint-validation', root / 'wb-query-endpoint-focused']
    for folder, key in zip(parent_locations, ['coremark_receipt', 'focused_receipt']):
        parent_receipt = folder / 'closure-receipt.json'
        assert digest(parent_receipt) == spec[key + '_sha256']
        parent = json.loads(parent_receipt.read_text())
        assert parent['ready']
        for name, row in parent['files'].items():
            path = folder / name
            assert path.stat().st_size == row['bytes'] and digest(path) == row['sha256'], name
    sys.path.insert(0, str(root))
    from native_perf import read_blocks, unique_counters
    module_spec = importlib.util.spec_from_file_location('closed_query_trigger_helper', helper_path)
    helper = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(helper)
    fields = source['new_exact_counter_names']
    cases = [spec['coremark'], *spec['short_cases'], spec['formal_case']]
    rows = []
    for case in cases:
        point = case['point']
        folder = stage / point
        record = json.loads((folder / 'result.json').read_text())
        assert record['source'] == spec['source'] and record['binary_sha256'] == receipt['build']['binary_sha256']
        assert record['spec_sha256'] == launch['spec_sha256'] and record['runner_sha256'] == launch['runner_sha256']
        assert record['original_process_closed'] and record['exitcode'] == 0 and record['difftest_passed']
        command = json.loads((folder / 'command.json').read_text())
        for flag, value in [('-s', str(case['seed'])), ('-i', case['image']), ('-I', str(case['max_instr'])), ('--diff', spec['reference']), ('--cst-file', spec['cst'])]:
            assert command[command.index(flag) + 1] == value
        if case.get('warmup'):
            assert command[command.index('-W') + 1] == str(case['warmup'])
        blocks = read_blocks(folder / 'simulator_err.txt')
        assert len(blocks) == (2 if case.get('warmup') else 1)
        original_neutrality = None
        if 'control' in case:
            control = parent_locations[0] / 'coremark' if point == 'coremark' else parent_locations[1] / 'query_endpoint' / point
            old = read_blocks(control / 'simulator_err.txt')
            old_names = {name for block in old.values() for name in block}
            names = {name for block in blocks.values() for name in block}
            new_names = names - old_names
            assert old_names <= names and len(new_names) == 22
            assert {name.rsplit('.', 1)[-1] for name in new_names} == set(fields)
            original_neutrality = helper.old_execution(control, folder, new_names)
            assert original_neutrality['passed']
        phases = []
        for phase, (stamp, block) in zip(['warmup', 'roi'] if case.get('warmup') else ['coremark'], sorted(blocks.items())):
            counters = unique_counters(block)
            values = {name: value for name, value in counters.items() if name.rsplit('.', 1)[-1] in fields}
            assert len(values) == 22
            queues = sorted({name.rsplit('.', 1)[0] for name in values})
            results = []
            for queue in queues:
                short = {name.removeprefix('diag_fdiv_query_'): values[queue + '.' + name] for name in fields}
                assert min(short.values()) >= 0
                ready = short['ready_entry_cycles']
                checks = dict(ready_partition=ready == short['eligible_entry_cycles'] + short['new_blocked_entry_cycles'],
                              old_new_partition=short['old_blocked_entry_cycles'] + short['loss_entry_cycles'] == short['new_blocked_entry_cycles'] + short['gain_entry_cycles'],
                              gain_selection_partition=short['gain_entry_cycles'] == short['gain_selected_uops'] + short['gain_unselected_entry_cycles'],
                              gained_selection_subset=short['gain_selected_uops'] <= short['selected_uops'])
                assert all(checks.values()), (point, phase, queue, checks)
                results.append(dict(queue=queue, counts=short, checks=checks))
            phases.append(dict(phase=phase, native_print_stamp=stamp, queues=results))
        assert len(phases) == (2 if case.get('warmup') else 1)
        rows.append(dict(point=point, parent_native_six_table_neutrality=original_neutrality, phases=phases,
                         formal_instrumentation_neutrality=None if 'control' not in case else original_neutrality['passed']))
    return dict(pending=False, database_read=True, completed=len(rows), rows=rows,
                source=source['head'], consumer_sha256=digest(Path(__file__)), closed_receipt_sha256=digest(receipt_path),
                performance_causality_validated=None,
                scope='Actual current-state ready FDIV entry-cycle and selected-uop intersections; repeated cycles and multiple entries may overlap. Response populations occur later and are not a partition of selections. Short parent native/six-table neutrality and numeric partitions do not establish formal soplex neutrality or dynamic criticality. No opportunity-to-CPI conversion or suite score.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--action', choices=['collect', 'analyze'], required=True)
    options = parser.parse_args()
    root = options.report_dir.resolve()
    result = collect(root) if options.action == 'collect' else analyze(root)
    if options.action == 'analyze' and not result['pending']:
        out = root / 'query-trigger-observation/analysis'
        out.mkdir(exist_ok=True)
        (out / 'evidence.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
