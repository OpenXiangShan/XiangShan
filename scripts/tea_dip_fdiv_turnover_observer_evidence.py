"""Collect one closedcd5 observer point and require complete8f8 execution neutrality."""
from pathlib import Path
import argparse
import json
import re
import shlex
import subprocess
import tarfile

from native_perf import read_blocks
from tea_dip_fdiv_turnover_counts import FIELDS, turnover_counts
from tea_dip_query_endpoint_evidence import old_execution, verify
from tea_dip_wb_protocol_micro import digest, write

FILES = ['command.json', 'result.json', 'exitcode', 'process.json', 'launch-observation.json',
         'simulator_out.txt', 'simulator_err.txt', 'simulator.db']
TOOLS = ['tea_dip_fdiv_turnover_observer_run.py', 'tea_dip_fdiv_turnover_counts.py',
         'tea_dip_fdiv_ready_run.py', 'tea_dip_wb_protocol_micro.py',
         'tea_dip_query_endpoint_evidence.py', 'native_perf.py']


def metadata(root, point):
    evidence = root / 'evidence/fdiv-turnover-observe-20261108'
    source = json.loads((evidence / 'source.json').read_text())
    spec = json.loads((evidence / 'experiment.json').read_text())
    launch = json.loads((evidence / 'launch.json').read_text())
    assert source['head'] == spec['source'] == launch['source'] == 'cd5fc32b2e13ed46f50d6ab3b2f5655a4d1cc70a'
    assert source['base'] == spec['parent_source'] == launch['parent_source'] == '8f8ece60ac8b08b358743bc9db15a5a227dde0b8'
    assert source['source_reversal_byte_exact'] and source['fields'] == list(FIELDS)
    assert digest(evidence / 'proposed.scala') == source['source_sha256']
    assert digest(evidence / 'experiment.json') == launch['spec_sha256']
    assert digest(evidence / 'run.py') == launch['runner_sha256']
    assert point in {case['point'] for case in [spec['coremark'], *spec['short_cases']]}
    return evidence, spec, launch


def collect(root, point):
    evidence, spec, launch = metadata(root, point)
    stage = evidence / 'closed-points' / point
    if (stage / 'closure-receipt.json').exists():
        return json.loads((stage / 'closure-receipt.json').read_text())
    code = r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
launch=LAUNCH;spec=SPEC;point=POINT;root=Path(launch['run_dir']);folder=root/point
def sha(p):
 with p.open('rb')as f:return hashlib.file_digest(f,'sha256').hexdigest()
def live(row):
 p=Path('/proc',str(row['pid']),'stat');return p.exists()and p.read_text().rsplit(')',1)[1].split()[19]==str(row['start_ticks'])
assert sha(root/'experiment.json')==launch['spec_sha256']and sha(root/'tea_dip_fdiv_turnover_observer_run.py')==launch['runner_sha256']
state=json.loads((root/'run-state.json').read_text());assert all(state[k]==launch['process'][k]for k in ['pid','start_ticks'])
build=json.loads(Path(spec['build_state']).read_text());assert build['source']==spec['source']
result=json.loads((folder/'result.json').read_text())if(folder/'result.json').exists()else None
if result is None or'observation_neutrality'not in result:
 print(json.dumps(dict(ready=False,point=point,controller=state,controller_live=live(state),build=build,database_read=False)))
else:
 process=json.loads((folder/'process.json').read_text());assert not live(process)and result['process']==process and result['original_process_closed']
 assert build['phase']=='complete'and result['source']==spec['source']and result['binary_sha256']==build['binary_sha256']
 assert result['spec_sha256']==launch['spec_sha256']and result['runner_sha256']==launch['runner_sha256']
 names=[point+'/'+name for name in FILES]+TOOLS+['experiment.json','build-identity.json','parent-closed-inputs.json','exact-counter-names.json']
 files={n:dict(bytes=(root/n).stat().st_size,sha256=sha(root/n))for n in names}
 print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),root=str(root),point=point,controller=state,controller_live=live(state),process=process,original_process_live=False,build=build,files=files,database_read=False)))
'''.replace('LAUNCH', repr(launch)).replace('SPEC', repr(spec)).replace('POINT', repr(point)).replace('FILES', repr(FILES)).replace('TOOLS', repr(TOOLS))
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', 'node031', 'python3', '-'],
                           input=code, text=True, capture_output=True, check=True, timeout=50)
    receipt = json.loads(reply.stdout)
    if not receipt['ready']:
        return receipt
    assert not stage.exists(), 'Inspect the existing partial archive before collecting'
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
        return dict(pending=True, database_read=False, point=point)
    receipt = json.loads((stage / 'closure-receipt.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == point
    assert all(receipt['controller'][key] == launch['process'][key] for key in ['pid', 'start_ticks'])
    verify(stage, receipt['files'])
    assert digest(stage / 'experiment.json') == launch['spec_sha256']
    for name in TOOLS:
        assert digest(stage / name) == spec['input_hashes'][str(Path(launch['run_dir']) / name)]
    for name in ['native_perf.py', 'tea_dip_query_endpoint_evidence.py', 'tea_dip_fdiv_turnover_counts.py']:
        assert digest(Path(__file__).with_name(name)) == digest(stage / name)
    control = root / (Path(spec['parent_root']).name + '-points') / point
    control_receipt = json.loads((control / 'closure-receipt.json').read_text())
    assert control_receipt['ready'] and not control_receipt['original_process_live']
    assert control_receipt['build']['source'] == spec['parent_source'] and control_receipt['build']['binary_sha256'] == spec['parent_binary_sha256']
    verify(control, control_receipt['files'])
    parent = json.loads((stage / 'parent-closed-inputs.json').read_text())
    assert parent['all_selected_original_guests_closed'] and parent['source'] == spec['parent_source']
    for name, value in control_receipt['files'].items():
        assert parent['files'][point + '/' + name] == value
    folder = stage / point
    result = json.loads((folder / 'result.json').read_text())
    started = json.loads((folder / 'launch-observation.json').read_text())
    assert result['process'] == started['process'] == receipt['process'] and result['original_process_closed']
    assert result['source'] == started['source'] == receipt['build']['source'] == spec['source']
    assert result['binary_sha256'] == started['binary_sha256'] == receipt['build']['binary_sha256']
    assert result['exitcode'] == 0 and result['difftest_passed'] and all(result['sampler_flags'].values())
    assert result['spec_sha256'] == launch['spec_sha256'] and result['runner_sha256'] == launch['runner_sha256']
    command = json.loads((folder / 'command.json').read_text())
    assert command == started['command'] and command[6] == receipt['build']['binary']
    previous = json.loads((control / 'command.json').read_text())
    for flag in ['-I', '-C', '-s', '-i', '--diff', '--cst-file', '--dump-select-db']:
        assert command[command.index(flag) + 1] == previous[previous.index(flag) + 1]
    if '-W' in previous:
        assert command[command.index('-W') + 1] == previous[previous.index('-W') + 1]
    text = (folder / 'simulator_out.txt').read_text()
    assert 'Difftest enabled' in text and (folder / 'exitcode').read_text().strip() == '0'
    for name, value in [('enableTea0', 1), ('enableDip0', 1), ('teaSamplePeriod0', 1000), ('dipSamplePeriod0', 1000)]:
        assert re.search(r'^' + name + ' = ' + str(value) + '$', text, re.M)
    names = set(json.loads((stage / 'exact-counter-names.json').read_text()))
    assert len(names) == 8 and {name.rsplit('.', 1)[-1] for name in names} == set(FIELDS)
    neutrality = old_execution(control, folder, names)
    assert neutrality == result['observation_neutrality']
    counts = [dict(native_print_stamp=stamp, modules=turnover_counts(block))
              for stamp, block in sorted(read_blocks(folder / 'simulator_err.txt').items())] if neutrality['passed'] else None
    assert counts is None or counts == result['phases']
    return dict(pending=False, database_read=True, point=point, source=spec['source'], parent_source=spec['parent_source'],
                observation_neutrality=neutrality, event_interpretation_valid=neutrality['passed'], phases=counts,
                exact_added_native_names=sorted(names), closed_receipt_sha256=digest(stage / 'closure-receipt.json'),
                consumer_sha256=digest(Path(__file__)), scope='One original closed observer/control pair. Non-flushed interface start/finish/turnover counts and cross-format subsets; per-wrapper events can overlap in global time. Not retired/unique requests, saved critical cycles, CPU CPI, isolated host cost or full-suite causality.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--point', required=True)
    parser.add_argument('--action', required=True, choices=['collect', 'analyze'])
    args = parser.parse_args()
    root = args.report_dir.resolve()
    value = collect(root, args.point) if args.action == 'collect' else analyze(root, args.point)
    if args.action == 'analyze' and not value['pending']:
        output = root / 'evidence/fdiv-turnover-observe-20261108/closed-points' / args.point / 'analysis'
        output.mkdir(parents=True, exist_ok=True)
        write(output / 'evidence.json', value)
    print(json.dumps(value, indent=2))


if __name__ == '__main__':
    main()
