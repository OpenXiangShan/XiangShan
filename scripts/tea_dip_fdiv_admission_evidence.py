"""Archive one closed admission observer and separate WB eligibility from FU gating."""
from pathlib import Path
import argparse
import json
import re
import shlex
import subprocess
import sys
import tarfile

from tea_dip_query_endpoint_evidence import old_execution, verify
from tea_dip_wb_protocol_micro import digest, write

SOURCE = '273f25d8a9a02a671a8eacaed4c61586fd932ed2'
PARENT = '39a163eb3910ff9ecab02df4ac8e3c99d772feeb'
FILES = ['command.json', 'result.json', 'exitcode', 'process.json', 'launch-observation.json',
         'simulator_out.txt', 'simulator_err.txt', 'simulator.db']


def metadata(root, point):
    evidence = root / 'evidence/fdiv-ready-admission-observe'
    config = json.loads((evidence / 'launch-v2/config.json').read_text())
    launch = json.loads((evidence / 'launch-v2/observer-launch.json').read_text())
    source = json.loads((evidence / 'source.json').read_text())
    assert source['head'] == config['source'] == launch['source'] == SOURCE
    assert source['base'] == config['parent_source'] == launch['parent_source'] == PARENT
    assert source['source_reversal_byte_exact'] and source['new_exact_counter_names'] == config['new_exact_counter_names']
    assert source['source_sha256']['src/main/scala/xiangshan/backend/float/FltIssueQueue.scala'] == digest(evidence / 'proposed.scala')
    assert launch['runner_sha256'] == config['input_hashes']['run.py'] == digest(evidence / 'launch-v2/run.py')
    assert point in {c['point'] for c in [config['parent_launch']['spec']['coremark'], *config['parent_launch']['spec']['short_cases']]}
    return evidence, config, launch


def collect(root, point):
    evidence, config, launch = metadata(root, point)
    stage = evidence / 'closed-points' / point
    if (stage / 'closure-receipt.json').exists():
        return json.loads((stage / 'closure-receipt.json').read_text())
    code = r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
launch=__LAUNCH__;config=__CONFIG__;point=__POINT__;root=Path(launch['run_dir']);folder=root/point
def sha(p):
 with p.open('rb')as stream:return hashlib.file_digest(stream,'sha256').hexdigest()
def live(row):
 p=Path('/proc',str(row['pid']),'stat');return p.exists()and p.read_text().rsplit(')',1)[1].split()[19]==row['start_ticks']
assert sha(root/'experiment.json')==launch['spec_sha256']and sha(root/'run.py')==launch['runner_sha256']
spec=json.loads((root/'experiment.json').read_text());state=json.loads((root/'run-state.json').read_text())
assert all(state[k]==launch['process'][k]for k in ['pid','start_ticks'])
assert spec['source']==config['source']and spec['parent_source']==config['parent_source']and spec['new_exact_counter_names']==config['new_exact_counter_names']
if not(folder/'result.json').exists():
 print(json.dumps(dict(ready=False,point=point,controller=state,controller_live=live(state),database_read=False)))
else:
 process=json.loads((folder/'process.json').read_text())
 if live(process):print(json.dumps(dict(ready=False,point=point,original_process_live=True,database_read=False)))
 else:
  result=json.loads((folder/'result.json').read_text());build=json.loads(Path(spec['build_state']).read_text())
  assert result['process']==process and result['original_process_closed']and result['exitcode']==0 and result['difftest_passed']and result['observation_neutrality']['passed']
  assert build['phase']=='complete'and build['source']==result['source']==spec['source']and build['binary_sha256']==result['binary_sha256']
  assert result['spec_sha256']==launch['spec_sha256']and result['runner_sha256']==launch['runner_sha256']
  names=[point+'/'+name for name in __FILES__]+['experiment.json','run.py','native_perf.py','tea_dip_query_endpoint_evidence.py','parent-closed-inputs.json','launch.json']
  files={name:dict(bytes=(root/name).stat().st_size,sha256=sha(root/name))for name in names}
  print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),root=str(root),point=point,controller=state,controller_live=live(state),process=process,original_process_live=False,build=build,files=files,database_read=False)))
'''.replace('__LAUNCH__', repr(launch)).replace('__CONFIG__', repr(config)).replace('__POINT__', repr(point)).replace('__FILES__', repr(FILES))
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', 'node032', 'python3', '-'],
                           input=code, text=True, capture_output=True, check=True, timeout=50)
    receipt = json.loads(reply.stdout)
    if not receipt['ready']:
        return receipt
    assert not stage.exists(), 'Inspect the existing partial archive before collecting'
    stage.mkdir(parents=True)
    command = 'tar -C ' + shlex.quote(receipt['root']) + ' -czf - ' + ' '.join(map(shlex.quote, receipt['files']))
    with (stage / 'originals.tar.gz').open('wb') as stream:
        subprocess.run(['proxychains', '-q', 'ssh', 'node032', command], stdout=stream, check=True, timeout=60)
    with tarfile.open(stage / 'originals.tar.gz') as archive:
        assert {entry.name for entry in archive.getmembers()} == set(receipt['files'])
        archive.extractall(stage, filter='data')
    verify(stage, receipt['files'])
    write(stage / 'closure-receipt.json', receipt)
    return receipt


def admission_counts(block, fields, clocks):
    from native_perf import unique_counters
    values = {name: value for name, value in unique_counters(block).items() if name.rsplit('.', 1)[-1] in fields}
    assert len(values) == 14, 'Missing or ambiguous admission fields'
    prefixes = sorted({name.rsplit('.', 1)[0] for name in values})
    assert len(prefixes) == 2
    rows = []
    for prefix in prefixes:
        counts = {name.removeprefix('diag_fdiv_admission_'): values[prefix + '.' + name] for name in fields}
        eligible, selected, blocked = [counts[name + '_entry_cycles'] for name in ['wb_eligible', 'select_eligible', 'fu_blocked']]
        assert min(counts.values()) >= 0 and eligible == selected + blocked
        assert counts['fu_blocked_cycles'] <= min(blocked, clocks)
        rows.append(dict(queue=prefix, counts=counts, fu_blocked_fraction_of_wb_eligible=blocked / eligible if eligible else None,
                         fu_blocked_fraction_of_clock=counts['fu_blocked_cycles'] / clocks))
    return rows


def analyze(root, point):
    evidence, config, launch = metadata(root, point)
    stage = evidence / 'closed-points' / point
    if not (stage / 'closure-receipt.json').exists():
        return dict(pending=True, database_read=False, point=point)
    receipt = json.loads((stage / 'closure-receipt.json').read_text())
    assert receipt['ready'] and receipt['point'] == point and not receipt['original_process_live']
    assert all(receipt['controller'][key] == launch['process'][key] for key in ['pid', 'start_ticks'])
    verify(stage, receipt['files'])
    assert json.loads((stage / 'launch.json').read_text()) == launch and digest(stage / 'experiment.json') == launch['spec_sha256']
    spec = json.loads((stage / 'experiment.json').read_text())
    assert spec['source'] == SOURCE and spec['parent_source'] == PARENT and spec['new_exact_counter_names'] == config['new_exact_counter_names']
    for name in ['run.py', 'native_perf.py', 'tea_dip_query_endpoint_evidence.py']:
        assert digest(stage / name) == config['input_hashes'][name]
    assert digest(root / 'native_perf.py') == digest(stage / 'native_perf.py')
    assert digest(Path(__file__).with_name('tea_dip_query_endpoint_evidence.py')) == digest(stage / 'tea_dip_query_endpoint_evidence.py')
    parent = json.loads((stage / 'parent-closed-inputs.json').read_text())
    assert digest(stage / 'parent-closed-inputs.json') == spec['parent_receipt_sha256']
    assert parent['ready'] and not parent['original_controller_live'] and parent['all_selected_original_guests_closed'] and parent['source'] == PARENT
    parent_stage = root / 'fdiv-ready-validation'
    verify(parent_stage, parent['files'])
    folder = stage / point
    result = json.loads((folder / 'result.json').read_text())
    started = json.loads((folder / 'launch-observation.json').read_text())
    assert result['source'] == SOURCE and result['binary_sha256'] == receipt['build']['binary_sha256'] and receipt['build']['source'] == SOURCE
    assert result['process'] == started['process'] == receipt['process'] and started['original_process_live']
    assert started['source'] == SOURCE and started['binary_sha256'] == result['binary_sha256']
    assert result['spec_sha256'] == launch['spec_sha256'] and result['runner_sha256'] == launch['runner_sha256']
    assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0 and (folder / 'exitcode').read_text().strip() == '0'
    case, = [c for c in [spec['coremark'], *spec['short_cases']] if c['point'] == point]
    command = json.loads((folder / 'command.json').read_text())
    assert command == started['command'] and command[6] == receipt['build']['binary']
    for flag, value in [('-s', str(case['seed'])), ('-i', case['image']), ('-I', str(case['max_instr'])), ('--diff', spec['reference']), ('--cst-file', spec['cst'])]:
        assert command[command.index(flag) + 1] == value
    if case.get('warmup'):
        assert command[command.index('-W') + 1] == str(case['warmup'])
    text = (folder / 'simulator_out.txt').read_text()
    assert 'Difftest enabled' in text
    for name, value in [('enableTea0', 1), ('enableDip0', 1), ('teaSamplePeriod0', 1000), ('dipSamplePeriod0', 1000)]:
        assert re.search(r'^' + name + ' = ' + str(value) + '$', text, re.M)
    sys.path.insert(0, str(root))
    from native_perf import read_blocks, unique_counters
    blocks = read_blocks(folder / 'simulator_err.txt')
    old = read_blocks(parent_stage / point / 'simulator_err.txt')
    added = {name for block in blocks.values() for name in block} - {name for block in old.values() for name in block}
    assert len(added) == 14 and {name.rsplit('.', 1)[-1] for name in added} == set(spec['new_exact_counter_names'])
    neutral = old_execution(parent_stage / point, folder, added)
    assert neutral['passed']
    assert len(blocks) == (2 if case.get('warmup') else 1)
    phases = []
    for label, (stamp, block) in zip(['warmup', 'roi'] if case.get('warmup') else ['coremark'], sorted(blocks.items())):
        clocks, = [value for name, value in unique_counters(block).items() if name.endswith('.rob.clock_cycle')]
        phases.append(dict(phase=label, native_print_stamp=stamp, native_clock_cycles=clocks,
                           queues=admission_counts(block, spec['new_exact_counter_names'], clocks)))
    return dict(pending=False, database_read=True, point=point, source=SOURCE, binary_sha256=receipt['build']['binary_sha256'],
                parent_native_six_complete_table_neutrality=neutral, phases=phases, exact_added_native_names=sorted(added),
                closed_receipt_sha256=digest(stage / 'closure-receipt.json'), consumer_sha256=digest(Path(__file__)),
                scope='Native interval counters, each dump used directly. WB eligibility partitions into select eligibility and marginal FU blocking after WB. Selected attempts and EX0 response-cycle events have different boundary cohorts; no equality or cancellation-per-selection inference. No FU internal subreason, unique/retired instruction, PC producer, critical-cycle or recoverable-CPI claim.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--point', required=True)
    parser.add_argument('--action', required=True, choices=['collect', 'analyze'])
    args = parser.parse_args()
    root = args.report_dir.resolve()
    result = collect(root, args.point) if args.action == 'collect' else analyze(root, args.point)
    if args.action == 'analyze' and not result['pending']:
        output = root / 'evidence/fdiv-ready-admission-observe/closed-points' / args.point / 'analysis'
        output.mkdir(exist_ok=True)
        write(output / 'evidence.json', result)
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
