"""Collect the closed early-ready protocol child without waiting for SPEC siblings."""
from pathlib import Path
import argparse
import json
import re
import shlex
import sqlite3
import subprocess
import sys
import tarfile

from tea_dip_fdiv_ready_evidence import metadata
from tea_dip_query_endpoint_evidence import digest, old_execution, verify


def collect(root):
    evidence, launch = metadata(root, early_ready=True)
    stage = evidence / 'protocol-closure'
    if (stage / 'closure-receipt.json').exists():
        return json.loads((stage / 'closure-receipt.json').read_text())
    code = 'launch=' + repr(launch) + '\n' + r'''from pathlib import Path
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
  names+=['experiment.json','build-identity.json','control-coremark-closure.json','micro-controller.json','tea_dip_fdiv_ready_run.py','tea_dip_wb_protocol_micro.py','native_perf.py','tea_dip_query_endpoint_evidence.py','coremark-gate.json','micro.stdout','micro.stderr']
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
    evidence, launch = metadata(root, early_ready=True)
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
    control = json.loads((stage / 'control-coremark-closure.json').read_text())
    assert build['source'] == launch['spec']['source'] and build['phase'] == 'complete'
    assert spec['binaries']['candidate']['source'] == build['source'] and spec['binaries']['candidate']['sha256'] == build['binary_sha256']
    assert spec['binaries']['stage_control']['source'] == control['build']['source'] == launch['spec']['control_source']
    assert spec['binaries']['stage_control']['sha256'] == control['build']['binary_sha256']
    for name in ['tea_dip_fdiv_ready_run.py', 'tea_dip_wb_protocol_micro.py', 'native_perf.py', 'tea_dip_query_endpoint_evidence.py']:
        assert digest(stage / name) == launch['spec']['input_hashes'][str(Path(launch['run_dir']) / name)]
    assert spec['runner_sha256'] == digest(stage / 'tea_dip_wb_protocol_micro.py')
    assert read_json(stage / 'coremark-gate.json')['passed']
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
    baseline = root / 'fdiv-turnover-validation-20261108'
    verify(baseline, read_json(baseline / 'closure-receipt.json')['files'])
    sys.path.insert(0, str(root))
    from native_perf import read_blocks
    from analyze_windows import DIP
    observations = {}
    for row in rows:
        folder = stage / 'protocol-micro/runs' / row['variant'] / row['case']
        with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
            db.row_factory = sqlite3.Row
            tea = list(db.execute('SELECT CYCLE FROM Tea_0 ORDER BY CYCLE,ID'))
            dip = [dict(r) for r in db.execute('SELECT * FROM Dip_0 ORDER BY CYCLE,ID')]
            finals = list(db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'))
            snapshots = list(db.execute('SELECT * FROM TeaCycleSummary_0 ORDER BY CYCLE,ID'))
            width = sum(r['name'].startswith('SLOTS_') and r['name'].endswith('_STATE') for r in db.execute('PRAGMA table_info(Dip_0)'))
            _, stats = DIP.analyze(dip, finals, width)
            tables = {name: db.execute('SELECT count(*) FROM ' + name).fetchone()[0] for name in spec['selectors'].split()}
        assert tables == row['trace_table_rows'] and all(tables.values())
        assert [r['CYCLE'] for r in tea] == [r['CYCLE'] for r in dip] == list(range(0, row['cycles'] + 1, 1000))
        assert stats['resolved_slots'] + stats['unresolved_slots'] == stats['sampled_slots']
        for a, b in zip(snapshots, snapshots[1:]):
            assert sum(b['STATES_' + str(i)] - a['STATES_' + str(i)] for i in range(4)) == b['CYCLE'] - a['CYCLE']
        blocks = read_blocks(folder / 'simulator_err.txt')
        assert len(blocks) == 1
        stamp, native = next(iter(blocks.items()))
        def value(suffix):
            values = [v for k, v in native.items() if k.endswith('.' + suffix)]
            assert len(values) == 1 and len(values[0]) == 1
            return values[0][0]
        states = {state: value('tea_exact_endpoint_state_' + str(i)) for i, state in enumerate(['Computing', 'Stalled', 'Flushed', 'Drained'])}
        cycle = value('tea_exact_endpoint_cycle')
        assert sum(states.values()) == cycle
        counters = {suffix: sum(v[0] for k, v in native.items() if k.endswith('.' + suffix)) for suffix in ['diag_fp_cancelled_entries', 'diag_fp_eligible_entry_cycles', 'diag_fp_wb_conflict_entry_cycles', 'diag_fp_success_responses', 'waitfDivSqrtCycle']}
        observations[row['variant'], row['case']] = dict(native_print_stamp=stamp, endpoint_cycle=cycle, endpoint_retired=value('tea_exact_endpoint_retired'), endpoint_states=states, trap_minus_endpoint_cycles=row['cycles']-cycle, snapshot_last_cycle=snapshots[-1]['CYCLE'], exact_prefix_tail_cycles=cycle-snapshots[-1]['CYCLE'], dip=stats, overlapping_native_counters=counters)
    comparisons = []
    for case in spec['cases']:
        name = case['case']
        folders = [stage / 'protocol-micro/runs' / variant / name for variant in ['stage_control', 'candidate']]
        prior = root / 'fdiv-turnover-validation-20261108/protocol-micro/runs/candidate' / name
        assert old_execution(prior, folders[0], set())['passed']
        equality = old_execution(*folders, set())
        if name == 'fixed_only':
            assert equality['passed']
        values = [read_json(folder / 'result.json') for folder in folders]
        a, b = [observations[variant, name] for variant in ['stage_control', 'candidate']]
        delta = values[1]['cycles'] - values[0]['cycles']
        comparisons.append(dict(case=name, control_trap_instructions=values[0]['instructions'], candidate_trap_instructions=values[1]['instructions'], trap_instruction_change=values[1]['instructions']-values[0]['instructions'], control_trap_cycles=values[0]['cycles'], candidate_trap_cycles=values[1]['cycles'], trap_cycle_change=delta, native_six_table_equality=equality, original8f8_control_reproduced=True, observations=[a, b], endpoint_state_delta={state: b['endpoint_states'][state]-a['endpoint_states'][state] for state in a['endpoint_states']}, endpoint_cycle_delta=b['endpoint_cycle']-a['endpoint_cycle'], endpoint_cycle_delta_matches_trap_delta=b['endpoint_cycle']-a['endpoint_cycle']==delta, performance_speedup=None))
    return dict(pending=False, database_read=True, validation_passed=True, source=build['source'], control_source=control['build']['source'], binary_sha256=build['binary_sha256'], all18_Difftest_passed=True, all9_controls_reproduce_original8f8=True, rows=comparisons, closed_files=len(receipt['files']), parent_live_at_collection=receipt['parent_live'], closed_receipt_sha256=digest(stage / 'closure-receipt.json'), consumer_sha256=digest(Path(__file__)), scope='Closed early-ready protocol child only. Exact native endpoint-prefix states and independent DIP samples; original8f8 control full-native/six-table calibration. Whole-boot trap counts are diagnostic progress, not SPEC intended-work CPI, unique critical-chain savings, suite score, physical timing or adoption proof.')


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
        evidence, _ = metadata(root, early_ready=True)
        (evidence / 'protocol-closure/analysis.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
