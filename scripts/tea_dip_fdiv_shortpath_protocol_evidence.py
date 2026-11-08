"""Collect the closed short-path observer protocol child without waiting for SPEC siblings."""
from pathlib import Path
import argparse
import json
import re
import shlex
import subprocess
import tarfile

from tea_dip_fdiv_shortpath_evidence import metadata, TOOLS
from tea_dip_fdiv_shortpath_counts import FIELDS, shortpath_counts
from tea_dip_query_endpoint_evidence import digest, old_execution, verify
from native_perf import read_blocks


def collect(root):
    evidence, specification, launch = metadata(root, 'coremark')
    stage = evidence / 'protocol-closure'
    if (stage / 'closure-receipt.json').exists():
        return json.loads((stage / 'closure-receipt.json').read_text())
    code = 'launch=' + repr(launch) + '\ntools=' + repr(TOOLS) + '\n' + r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
root=Path(launch['run_dir']);micro=root/'protocol-micro'
def sha(p):
 with p.open('rb')as stream:return hashlib.file_digest(stream,'sha256').hexdigest()
def live(row):
 p=Path('/proc',str(row['pid']),'stat');return p.exists()and p.read_text().rsplit(')',1)[1].split()[19]==str(row['start_ticks'])
assert sha(root/'experiment.json')==launch['spec_sha256']
parent=json.loads((root/'run-state.json').read_text());assert all(parent[k]==launch['process'][k]for k in ['pid','start_ticks'])
if not(root/'micro-controller.json').exists():
 print(json.dumps(dict(ready=False,database_read=False,reason='Original protocol child not launched')))
else:
 child=json.loads((root/'micro-controller.json').read_text());state=json.loads((micro/'run-state.json').read_text());assert all(child[k]==state[k]for k in ['pid','start_ticks','host'])
 guests=[json.loads(p.read_text())for p in micro.rglob('process.json')]
 if live(child)or state['phase']not in ['complete','failed']or any(live(p)for p in guests):
  print(json.dumps(dict(ready=False,database_read=False,reason='Original protocol child or guests not closed')))
 else:
  names=[str(p.relative_to(root))for p in micro.rglob('*')if p.is_file()and'__pycache__'not in p.parts]
  names+=tools+['experiment.json','build-identity.json','parent-build-identity.json','parent-closed-inputs.json','micro-controller.json','exact-counter-names.json','coremark/result.json','micro.stdout','micro.stderr']
  if(root/'micro-neutrality.json').exists():names.append('micro-neutrality.json')
  files={n:dict(bytes=(root/n).stat().st_size,sha256=sha(root/n))for n in names}
  print(json.dumps(dict(utc=datetime.now(timezone.utc).isoformat(),ready=True,root=str(root),parent=parent,parent_live=live(parent),original_micro_controller_closed=True,all_original_guests_closed=True,child=child,state=state,files=files,database_read=False)))
'''
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', 'node031', 'python3', '-'],
                           input=code, text=True, capture_output=True, check=True, timeout=50)
    receipt = json.loads(reply.stdout)
    if not receipt['ready']:
        return receipt
    assert not stage.exists(), 'Inspect partial protocol archive before collecting'
    stage.mkdir()
    command = 'tar -C ' + shlex.quote(receipt['root']) + ' -czf - ' + ' '.join(map(shlex.quote, receipt['files']))
    with (stage / 'originals.tar.gz').open('wb') as stream:
        subprocess.run(['proxychains', '-q', 'ssh', 'node031', command], stdout=stream, check=True, timeout=60)
    with tarfile.open(stage / 'originals.tar.gz') as archive:
        assert {row.name for row in archive.getmembers()} == set(receipt['files'])
        archive.extractall(stage, filter='data')
    verify(stage, receipt['files'])
    (stage / 'closure-receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    return receipt


def analyze(root):
    evidence, specification, launch = metadata(root, 'coremark')
    stage = evidence / 'protocol-closure'
    if not (stage / 'closure-receipt.json').exists():
        return dict(pending=True, database_read=False)
    receipt = json.loads((stage / 'closure-receipt.json').read_text())
    assert receipt['ready'] and receipt['original_micro_controller_closed'] and receipt['all_original_guests_closed']
    assert all(receipt['parent'][k] == launch['process'][k] for k in ['pid', 'start_ticks'])
    verify(stage, receipt['files'])
    assert digest(stage / 'experiment.json') == launch['spec_sha256']
    if receipt['state']['phase'] == 'failed':
        return dict(pending=False, database_read=False, validation_passed=False, performance_speedup=None)
    spec = json.loads((stage / 'protocol-micro/experiment.json').read_text())
    rows = json.loads((stage / 'protocol-micro/results.json').read_text())
    if any(not row['passed'] or row['exitcode'] != 0 for row in rows):
        return dict(pending=False, database_read=False, validation_passed=False, performance_speedup=None)
    assert len(rows) == 18 and len(spec['cases']) == 9
    build = json.loads((stage / 'build-identity.json').read_text())
    control = json.loads((stage / 'parent-build-identity.json').read_text())
    assert build['source'] == specification['source'] and build['phase'] == 'complete'
    assert spec['binaries']['candidate']['source'] == build['source'] and spec['binaries']['candidate']['sha256'] == build['binary_sha256']
    assert spec['binaries']['stage_control']['source'] == control['source'] == specification['parent_source']
    assert spec['binaries']['stage_control']['sha256'] == control['binary_sha256']
    for name in TOOLS:
        assert digest(stage / name) == specification['input_hashes'][str(Path(launch['run_dir']) / name)]
    assert spec['runner_sha256'] == digest(stage / 'tea_dip_wb_protocol_micro.py')
    assert read_json(stage / 'coremark/result.json')['observation_neutrality']['passed']
    exact = set(read_json(stage / 'exact-counter-names.json'))
    assert len(exact) == 22 and {n.rsplit('.', 1)[-1] for n in exact} == set(FIELDS)
    for row in rows:
        folder = stage / 'protocol-micro/runs' / row['variant'] / row['case']
        case, = [case for case in spec['cases'] if case['case'] == row['case']]
        identity = spec['binaries'][row['variant']]
        assert row['process'] == read_json(folder / 'process.json') and row['original_process_closed']
        assert row['binary_sha256'] == identity['sha256'] and row['image_sha256'] == case['image_sha256']
        assert digest(stage / 'protocol-micro/images' / case['case'] / 'program.bin') == case['image_sha256']
        assert not row['difftest_mismatches'] and not row['cycle_or_instruction_limit'] and all(row['sampler_flags'].values())
        assert (folder / 'exitcode').read_text().strip() == '0'
        text = re.sub(r'\x1b\[[0-9;]*m', '', (folder / 'simulator_out.txt').read_text())
        trap = re.search(r'HIT GOOD TRAP at pc = (0x[0-9a-fA-F]+)', text)
        counts = re.search(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)', text)
        assert trap and int(trap[1], 16) == int(case['pass_pc'], 16)
        assert counts and [int(v.replace(',', '')) for v in counts.groups()] == [row['instructions'], row['cycles']]
        assert 1700 < row['instructions'] < 3000 and 'Difftest enabled' in text
        assert not any(s in text for s in ['ABORT', 'HIT CRITICAL', 'HIT BAD TRAP'])
        command = read_json(folder / 'command.json')
        assert command[6] == identity['binary']
        for flag, value in [('-s', '99'), ('-I', '100000'), ('-C', '200000'), ('-i', case['image']), ('--diff', spec['reference']), ('--dump-select-db', spec['selectors'])]:
            assert command[command.index(flag) + 1] == value
        for name, value in [('enableTea0', 1), ('enableDip0', 1), ('teaSamplePeriod0', 1000), ('dipSamplePeriod0', 1000)]:
            assert re.search(r'^' + name + ' = ' + str(value) + '$', text, re.M)
    baseline = root / 'fdiv-early-ready-validation-20261108'
    verify(baseline, read_json(baseline / 'closure-receipt.json')['files'])
    comparisons = []
    for case in spec['cases']:
        name = case['case']
        folders = [stage / 'protocol-micro/runs' / variant / name for variant in ['stage_control', 'candidate']]
        prior = baseline / 'protocol-micro/runs/candidate' / name
        assert old_execution(prior, folders[0], set())['passed']
        equality = old_execution(*folders, exact)
        assert equality['passed']
        values = [read_json(folder / 'result.json') for folder in folders]
        assert values[0]['instructions'] == values[1]['instructions'] and values[0]['cycles'] == values[1]['cycles']
        phases = [dict(native_print_stamp=stamp, modules=shortpath_counts(block))
                  for stamp, block in sorted(read_blocks(folders[1] / 'simulator_err.txt').items())]
        assert {module['module'] + '.' + field for phase in phases for module in phase['modules'] for field in FIELDS} == exact
        comparisons.append(dict(case=name, control_trap_instructions=values[0]['instructions'], candidate_trap_instructions=values[1]['instructions'], control_trap_cycles=values[0]['cycles'], candidate_trap_cycles=values[1]['cycles'], native_six_table_equality=equality, original6fde_control_reproduced=True, phases=phases, performance_speedup=None))
    return dict(pending=False, database_read=True, validation_passed=True, source=build['source'], control_source=control['source'], binary_sha256=build['binary_sha256'], all18_Difftest_passed=True, all9_controls_reproduce_original6fde=True, all9_observation_pairs_full_neutral=True, nonzero_extra_forecast_observed=any(m['counts']['extra_ahead_cycles'] > 0 for r in comparisons for phase in r['phases'] for m in phase['modules']), rows=comparisons, closed_files=len(receipt['files']), parent_live_at_collection=receipt['parent_live'], closed_receipt_sha256=digest(stage / 'closure-receipt.json'), consumer_sha256=digest(Path(__file__)), scope='Closed short-path observer protocols only. Exact22 diagnostic names removed for full old-native/six-table neutrality; original6fde controls rechecked. Effective wrapper extra forecasts describe concurrent next-IQ eligibility, not raw internal predicates or t+3 acceptance/retirement. Whole-boot trap counts are diagnostic progress, not SPEC intended-work CPI, unique critical-chain savings, suite score, physical timing or adoption proof.')


def read_json(path):
    return json.loads(path.read_text())


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--collect', action='store_true')
    args = parser.parse_args()
    root = args.report_dir.resolve()
    if args.collect:
        receipt = collect(root)
        if not receipt['ready']:
            print(json.dumps(receipt, indent=2))
            return
    result = analyze(root)
    if not result['pending']:
        evidence, _, _ = metadata(root, 'coremark')
        (evidence / 'protocol-closure/analysis.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
