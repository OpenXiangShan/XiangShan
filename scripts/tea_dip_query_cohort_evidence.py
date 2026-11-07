"""Archive closed gain-attempt observers and check lifetime/ROI-boundary conservation."""
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
    evidence = root / 'evidence/query-cohort-observe'
    spec = json.loads((evidence / 'experiment.json').read_text())
    launch = json.loads((evidence / 'launch.json').read_text())
    code = r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
root=Path(RUN_DIR);spec=SPEC;launch=LAUNCH
assert hashlib.sha256((root/'experiment.json').read_bytes()).hexdigest()==launch['spec_sha256']
assert hashlib.sha256((root/'run.py').read_bytes()).hexdigest()==launch['runner_sha256']
state=json.loads((root/'run-state.json').read_text())
assert all(state[key]==launch['process'][key]for key in ['pid','start_ticks'])
def live(p):
 path=Path('/proc',str(p['pid']),'stat')
 return path.exists()and path.read_text().rsplit(')',1)[1].split()[19]==p['start_ticks']
if live(state)or state['phase']!='complete':
 print(json.dumps(dict(ready=False,controller=state,controller_live=live(state),database_read=False)))
else:
 assert state['completed']==3 and state['all_short_pairs_neutral']and state['coremark_gate_passed']
 build=json.loads(Path(spec['build_state']).read_text());assert build['source']==spec['source']and build['phase']=='complete'
 cases=[spec['coremark'],*spec['short_cases']];guests=[]
 for case in cases:
  folder=root/case['point'];p=json.loads((folder/'process.json').read_text());start=json.loads((folder/'launch-observation.json').read_text());result=json.loads((folder/'result.json').read_text())
  assert not live(p)and start['original_process_live']and start['process']==result['process']==p
  assert result['source']==spec['source']and result['binary_sha256']==build['binary_sha256']
  assert result['original_process_closed']and result['exitcode']==0 and result['difftest_passed']and result['observation_neutrality']['passed']
  guests.append(dict(point=case['point'],process=p,original_process_live=False))
 names=[case['point']+'/'+name for case in cases for name in FILES]+['experiment.json','run.py','native_perf.py','tea_dip_query_endpoint_evidence.py','parent-closed-inputs.json','run-state.json','short-results.json']
 files={}
 for name in names:
  p=root/name
  with p.open('rb')as stream:sha=hashlib.file_digest(stream,'sha256').hexdigest()
  files[name]=dict(bytes=p.stat().st_size,sha256=sha)
 print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),root=str(root),controller=state,controller_live=False,guests=guests,build=build,files=files,database_read=False)))
'''.replace('RUN_DIR', repr(launch['run_dir'])).replace('SPEC', repr(spec)).replace('LAUNCH', repr(launch)).replace('FILES', repr(FILES))
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=15', 'node031', 'python3', '-'],
                           input=code, capture_output=True, text=True, check=True, timeout=50)
    receipt = json.loads(reply.stdout)
    (evidence / 'collection-status.json').write_text(json.dumps(receipt, indent=2) + '\n')
    if not receipt['ready']:
        return receipt
    stage = root / 'query-cohort-observation'
    assert not stage.exists(), 'Analyze the existing closed archive'
    stage.mkdir()
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


def frame_counts(block, fields):
    from native_perf import unique_counters
    values = {name: value for name, value in unique_counters(block).items() if name.rsplit('.', 1)[-1] in fields}
    assert len(values) == 2 * len(fields), 'Missing or ambiguous gain cohort fields'
    queues = sorted({name.rsplit('.', 1)[0] for name in values})
    assert len(queues) == 2
    result = {}
    for queue in queues:
        counts = {name.removeprefix('diag_fdiv_gain_').removesuffix('_total'): values[queue + '.' + name] for name in fields}
        assert min(counts.values()) >= 0 and counts['error'] == 0 and counts['pending'] <= 20
        assert counts['selected'] == counts['success'] + counts['cancel'] + counts['flush'] + counts['pending']
        assert counts['cancel'] == sum(counts['cancel_' + stage] for stage in ['is0', 'is1', 'ex0', 'ex1', 'other'])
        assert counts['cancel_is0_no_src'] <= counts['cancel_is0'] and counts['cancel_ex0_no_src'] <= counts['cancel_ex0']
        result[queue] = counts
    return result


def gain_span(before, after):
    delta = {key: after[key] - before[key] for key in after if key != 'pending'}
    assert min(delta.values()) >= 0
    assert delta['selected'] + before['pending'] == delta['success'] + delta['cancel'] + delta['flush'] + after['pending']
    count = delta['selected']
    bounds = {outcome: dict(lower=max(0, delta[outcome] - before['pending']), upper=min(count, delta[outcome] + after['pending']))
              for outcome in ['success', 'cancel', 'flush']}
    assert all(0 <= row['lower'] <= row['upper'] <= count for row in bounds.values())
    return dict(delta=delta, pending_begin=before['pending'], pending_end=after['pending'],
                selected_origin_eventual_outcome_bounds=bounds,
                success_fraction_bounds={key: value / count for key, value in bounds['success'].items()} if count else None)


def analyze(root):
    evidence = root / 'evidence/query-cohort-observe'
    stage = root / 'query-cohort-observation'
    receipt_path = stage / 'closure-receipt.json'
    if not receipt_path.exists():
        return dict(pending=True, database_read=False, rows=[], reason='Original cohort controller and four guests not archived closed')
    receipt = json.loads(receipt_path.read_text())
    launch = json.loads((evidence / 'launch.json').read_text())
    assert receipt['ready'] and not receipt['controller_live'] and receipt['controller']['phase'] == 'complete'
    assert all(receipt['controller'][key] == launch['process'][key] for key in ['pid', 'start_ticks'])
    for name, row in receipt['files'].items():
        assert (stage / name).stat().st_size == row['bytes'] and digest(stage / name) == row['sha256'], name
    spec = json.loads((evidence / 'experiment.json').read_text())
    source = json.loads((evidence / 'source.json').read_text())
    assert spec['source'] == source['head'] == receipt['build']['source']
    assert digest(evidence / 'experiment.json') == digest(stage / 'experiment.json') == launch['spec_sha256']
    assert digest(stage / 'run.py') == launch['runner_sha256']
    for name, key in [('native_perf.py', 'native_reader'), ('tea_dip_query_endpoint_evidence.py', 'helper'), ('parent-closed-inputs.json', 'parent_receipt')]:
        assert digest(stage / name) == spec[key + '_sha256']
    parent = json.loads((stage / 'parent-closed-inputs.json').read_text())
    assert parent['ready'] and parent['all_selected_original_guests_closed']
    assert parent['source'] == spec['parent_source'] and parent['binary_sha256'] == spec['parent_binary_sha256']
    parent_archives = root / 'evidence/query-trigger-observe'
    for name, row in parent['files'].items():
        if '/' not in name:
            path = parent_archives / name
        else:
            point, filename = name.split('/', 1)
            path = parent_archives / 'coremark-closed' / filename if point == 'coremark' else parent_archives / 'short-closed' / point / point / filename
        assert path.stat().st_size == row['bytes'] and digest(path) == row['sha256'], name
    sys.path.insert(0, str(root))
    from native_perf import read_blocks, unique_counters
    module_spec = importlib.util.spec_from_file_location('closed_gain_cohort_helper', stage / 'tea_dip_query_endpoint_evidence.py')
    helper = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(helper)
    rows = []
    for case in [spec['coremark'], *spec['short_cases']]:
        point = case['point']
        folder = stage / point
        result = json.loads((folder / 'result.json').read_text())
        assert result['original_process_closed'] and result['exitcode'] == 0 and result['difftest_passed']
        assert result['source'] == spec['source'] and result['binary_sha256'] == receipt['build']['binary_sha256']
        assert result['spec_sha256'] == launch['spec_sha256'] and result['runner_sha256'] == launch['runner_sha256']
        command = json.loads((folder / 'command.json').read_text())
        for flag, value in [('-s', str(case['seed'])), ('-i', case['image']), ('-I', str(case['max_instr'])), ('--diff', spec['reference']), ('--cst-file', spec['cst'])]:
            assert command[command.index(flag) + 1] == value
        if case.get('warmup'):
            assert command[command.index('-W') + 1] == str(case['warmup'])
        control = parent_archives / 'coremark-closed' if point == 'coremark' else parent_archives / 'short-closed' / point / point
        blocks = read_blocks(folder / 'simulator_err.txt')
        old = read_blocks(control / 'simulator_err.txt')
        old_names = {name for block in old.values() for name in block}
        names = {name for block in blocks.values() for name in block}
        new_names = names - old_names
        assert old_names <= names and len(new_names) == 26
        assert {name.rsplit('.', 1)[-1] for name in new_names} == set(source['new_exact_counter_names'])
        neutral = helper.old_execution(control, folder, new_names)
        assert neutral['passed']
        assert len(blocks) == (2 if case.get('warmup') else 1)
        phases, previous = [], {}
        for label, (stamp, block) in zip(['warmup', 'roi'] if case.get('warmup') else ['coremark'], sorted(blocks.items())):
            queues = frame_counts(block, source['new_exact_counter_names'])
            counters = unique_counters(block)
            queue_rows = []
            for queue, counts in queues.items():
                before = previous.get(queue, {key: 0 for key in counts})
                span = gain_span(before, counts)
                selected_interval = counters[queue + '.diag_fdiv_query_gain_selected_uops']
                span['native_interval_gain_selected'] = selected_interval
                span['selection_interval_alignment_verified'] = span['delta']['selected'] == selected_interval
                queue_rows.append(dict(queue=queue, lifetime=counts, span=span))
            phases.append(dict(phase=label, native_print_stamp=stamp, queues=queue_rows))
            previous = queues
        rows.append(dict(point=point, parent_native_six_table_neutrality=neutral, phases=phases))
    return dict(pending=False, database_read=True, completed=len(rows), rows=rows, consumer_sha256=digest(Path(__file__)),
                closed_receipt_sha256=digest(receipt_path), source=source['head'],
                scope='Gain-selected issue attempts, not unique retired instructions. One pending tag follows entry movement. Lifetime and dump-span conservation include pending boundaries; eventual outcome bounds are conservative, not confidence intervals. Success means IQ issue-response success, not final writeback/retirement. Cancellation stage and no-src flags do not prove unique FU-busy cause or criticality. Old/new shadow predicates are in the current machine state. No opportunity-to-CPI conversion or suite score.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--action', choices=['collect', 'analyze'], required=True)
    args = parser.parse_args()
    root = args.report_dir.resolve()
    result = collect(root) if args.action == 'collect' else analyze(root)
    if args.action == 'analyze' and not result['pending']:
        out = root / 'query-cohort-observation/analysis'
        out.mkdir(exist_ok=True)
        (out / 'evidence.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
