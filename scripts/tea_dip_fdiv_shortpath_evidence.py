"""Collect one original closed short-path observer, including diagnostic failures."""
from pathlib import Path
import argparse
import json
import shlex
import subprocess
import tarfile

from native_perf import read_blocks
from tea_dip_fdiv_shortpath_counts import FIELDS, shortpath_counts
from tea_dip_query_endpoint_evidence import old_execution, verify
from tea_dip_wb_protocol_micro import digest, write

FILES = ['command.json', 'result.json', 'exitcode', 'process.json', 'launch-observation.json',
         'simulator_out.txt', 'simulator_err.txt', 'simulator.db']
TOOLS = ['tea_dip_fdiv_shortpath_run.py', 'tea_dip_fdiv_shortpath_counts.py', 'tea_dip_fdiv_ready_run.py',
         'tea_dip_fdiv_turnover_observer_run.py', 'tea_dip_fdiv_turnover_counts.py',
         'tea_dip_wb_protocol_micro.py', 'tea_dip_query_endpoint_evidence.py', 'native_perf.py']


def metadata(root, point):
    evidence = root / 'evidence/fdiv-shortpath-exposure-20261108'
    spec = json.loads((evidence / 'experiment.json').read_text())
    launch = json.loads((evidence / 'launch.json').read_text())
    source = json.loads((evidence / 'source.json').read_text())
    assert source['head'] == spec['source'] == launch['source'] == 'b1f250da4abe0fb53f63b345f4f32450f9b01c11'
    assert source['base'] == spec['parent_source'] == '6fde9c2fddf57062d43b93f971ec42d75022fc3f'
    assert digest(evidence / 'experiment.json') == launch['spec_sha256']
    assert digest(evidence / 'run.py') == launch['runner_sha256']
    assert point in {case['point'] for case in [spec['coremark'], *spec['short_cases']]}
    return evidence, spec, launch


def collect(root, point):
    evidence, spec, launch = metadata(root, point)
    stage = evidence / 'closed-points' / point
    if (stage / 'closure-receipt.json').exists():
        return json.loads((stage / 'closure-receipt.json').read_text())
    params = dict(spec=spec, launch=launch, point=point, files=FILES, tools=TOOLS)
    code = 'params=' + repr(params) + '\n' + r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
spec=params['spec'];launch=params['launch'];point=params['point'];root=Path(launch['run_dir']);folder=root/point
def sha(p):
 with p.open('rb')as stream:return hashlib.file_digest(stream,'sha256').hexdigest()
def live(row):
 p=Path('/proc',str(row['pid']),'stat');return p.exists()and p.read_text().rsplit(')',1)[1].split()[19]==str(row['start_ticks'])
assert sha(root/'experiment.json')==launch['spec_sha256']and sha(root/'tea_dip_fdiv_shortpath_run.py')==launch['runner_sha256']
state=json.loads((root/'run-state.json').read_text());assert all(state[k]==launch['process'][k]for k in ['pid','start_ticks'])
result=json.loads((folder/'result.json').read_text())if(folder/'result.json').exists()else None
if result is None or live(result['process']):
 print(json.dumps(dict(ready=False,point=point,controller=state,controller_live=live(state),database_read=False)))
else:
 process=json.loads((folder/'process.json').read_text());assert process==result['process']and not live(process)and result['original_process_closed']
 build=json.loads(Path(spec['build_state']).read_text());assert build['phase']=='complete'and build['source']==result['source']==spec['source']and build['binary_sha256']==result['binary_sha256']
 names=[point+'/'+name for name in params['files']]+params['tools']+['experiment.json','build-identity.json','parent-closed-inputs.json']
 files={name:dict(bytes=(root/name).stat().st_size,sha256=sha(root/name))for name in names}
 print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),root=str(root),point=point,controller=state,controller_live=live(state),process=process,original_process_live=False,build=build,files=files,database_read=False)))
'''
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', 'node031', 'python3', '-'],
                           input=code, text=True, capture_output=True, check=True, timeout=50)
    receipt = json.loads(reply.stdout)
    if not receipt['ready']:
        return receipt
    assert not stage.exists(), 'Inspect the partial archive before collecting'
    stage.mkdir(parents=True)
    write(stage / 'collection-receipt.json', receipt)
    command = 'tar -C ' + shlex.quote(receipt['root']) + ' -czf - ' + ' '.join(map(shlex.quote, receipt['files']))
    with (stage / 'originals.tar.gz').open('wb') as stream:
        subprocess.run(['proxychains', '-q', 'ssh', 'node031', command], stdout=stream, check=True, timeout=60)
    with tarfile.open(stage / 'originals.tar.gz') as archive:
        assert {row.name for row in archive.getmembers()} == set(receipt['files'])
        archive.extractall(stage, filter='data')
    verify(stage, receipt['files'])
    write(stage / 'closure-receipt.json', receipt)
    return receipt


def analyze(root, point):
    evidence, spec, launch = metadata(root, point)
    stage = evidence / 'closed-points' / point
    if not (stage / 'closure-receipt.json').exists():
        return dict(pending=True, point=point, database_read=False)
    receipt = json.loads((stage / 'closure-receipt.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    verify(stage, receipt['files'])
    assert digest(stage / 'experiment.json') == launch['spec_sha256']
    for name in TOOLS:
        assert digest(stage / name) == spec['input_hashes'][str(Path(launch['run_dir']) / name)]
    for name in ['native_perf.py', 'tea_dip_query_endpoint_evidence.py', 'tea_dip_fdiv_shortpath_counts.py', 'tea_dip_fdiv_turnover_counts.py']:
        assert digest(Path(__file__).with_name(name)) == digest(stage / name)
    control_stage = root / 'fdiv-early-ready-validation-20261108'
    control_receipt = json.loads((control_stage / 'closure-receipt.json').read_text())
    assert control_receipt['ready'] and not control_receipt['controller_live'] and control_receipt['all_original_processes_closed']
    assert all(control_receipt['controller'][k] == spec['parent_process'][k] for k in ['pid', 'start_ticks'])
    control_build = json.loads((control_stage / 'build-identity.json').read_text())
    assert control_build['source'] == spec['parent_source'] and control_build['binary_sha256'] == spec['parent_binary_sha256']
    verify(control_stage, control_receipt['files'])
    parent = json.loads((stage / 'parent-closed-inputs.json').read_text())
    for name in FILES:
        assert parent['files'][point + '/' + name] == control_receipt['files'][point + '/' + name]
    folder = stage / point
    result = json.loads((folder / 'result.json').read_text())
    started = json.loads((folder / 'launch-observation.json').read_text())
    assert result['process'] == started['process'] == receipt['process'] and result['original_process_closed']
    assert result['source'] == started['source'] == receipt['build']['source'] == spec['source']
    assert result['binary_sha256'] == started['binary_sha256'] == receipt['build']['binary_sha256']
    command = json.loads((folder / 'command.json').read_text())
    previous = json.loads((control_stage / point / 'command.json').read_text())
    assert command == started['command'] and command[6] == receipt['build']['binary']
    for flag in ['-I', '-C', '-s', '-i', '--diff', '--cst-file', '--dump-select-db', *(['-W'] if '-W' in previous else [])]:
        assert command[command.index(flag) + 1] == previous[previous.index(flag) + 1]
    text = (folder / 'simulator_out.txt').read_text()
    correct = result['exitcode'] == 0 and result['difftest_passed'] and all(result['sampler_flags'].values())
    correct = correct and (folder / 'exitcode').read_text().strip() == '0' and 'Difftest enabled' in text and 'EXCEEDING CYCLE/INSTR LIMIT' in text
    correct = correct and ('-W' not in previous or 'Warmup finished' in text)
    correct = correct and not any(marker in text for marker in ['ABORT', 'HIT CRITICAL', 'HIT BAD TRAP'])
    if not correct:
        value = dict(pending=False, point=point, source=spec['source'], parent_source=spec['parent_source'],
                     correct=False, database_read=False, observation_neutrality=None,
                     event_interpretation_valid=False, phases=None, performance_speedup=None,
                     closed_receipt_sha256=digest(stage / 'closure-receipt.json'), consumer_sha256=digest(Path(__file__)),
                     scope='Closed original correctness failure archived; no DB comparison or event interpretation.')
        output = stage / 'analysis'
        output.mkdir(exist_ok=True)
        write(output / 'evidence.json', value)
        return value
    blocks = read_blocks(folder / 'simulator_err.txt')
    names = {name for block in blocks.values() for name in block if name.rsplit('.', 1)[-1] in FIELDS}
    assert len(names) == 22
    neutrality = old_execution(control_stage / point, folder, names)
    counts, error = None, None
    if neutrality['passed'] and correct:
        try:
            counts = [dict(native_print_stamp=stamp, modules=shortpath_counts(block)) for stamp, block in sorted(blocks.items())]
        except AssertionError as failure:
            error = str(failure) or 'Short-path forecast partition conservation failed'
    value = dict(pending=False, point=point, source=spec['source'], parent_source=spec['parent_source'],
                 correct=correct, database_read=True, observation_neutrality=neutrality, performance_speedup=None,
                 event_interpretation_valid=correct and neutrality['passed'] and counts is not None,
                 phases=counts, event_validation_error=error, exact_added_native_names=sorted(names),
                 closed_receipt_sha256=digest(stage / 'closure-receipt.json'), consumer_sha256=digest(Path(__file__)),
                 scope='One original closed diagnostic pair. Effective extra wrapper-ready forecasts are partitioned by the concurrent next-IQ selection and IS0/IS1 masks. No raw internal short-predicate or t+3 acceptance/retirement linkage is measured. Not global request absence, unique dynamic cause, saved critical cycles or isolated host overhead.')
    output = stage / 'analysis'
    output.mkdir(exist_ok=True)
    write(output / 'evidence.json', value)
    return value


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--point', required=True)
    parser.add_argument('--action', choices=['collect', 'analyze'], required=True)
    args = parser.parse_args()
    value = collect(args.report_dir.resolve(), args.point) if args.action == 'collect' else analyze(args.report_dir.resolve(), args.point)
    print(json.dumps(value, indent=2))


if __name__ == '__main__':
    main()
