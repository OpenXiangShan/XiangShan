"""Close the original363 soplex observer, then compare it with its original82 formal."""
from pathlib import Path
import argparse
import importlib.util
import json
import shlex
import subprocess
import sys
import tarfile

from tea_dip_query_cohort_evidence import digest, frame_counts, gain_span, cancel_subsets, cancel_source_contract

POINT = 'soplex_pds-50_9466_0.126190'
FILES = ['command.json', 'result.json', 'exitcode', 'simulator_out.txt', 'simulator_err.txt',
         'simulator.db', 'process.json', 'launch-observation.json', 'controller-state.json']


def verify(folder, files):
    for name, row in files.items():
        path = folder / name
        assert path.stat().st_size == row['bytes'] and digest(path) == row['sha256'], name


def metadata(root):
    evidence = root / 'evidence/query-cohort-formal-observe'
    spec = json.loads((evidence / 'experiment.json').read_text())
    launch = json.loads((evidence / 'launch.json').read_text())
    first = json.loads((evidence / 'first-live.json').read_text())
    assert digest(evidence / 'experiment.json') == launch['spec_sha256']
    assert digest(evidence / 'runner.py') == launch['runner_sha256']
    assert digest(Path(__file__).with_name('tea_dip_query_cohort_evidence.py')) == spec['cohort_helper_sha256']
    assert spec['source_identity'] == dict(source='3630fe3ca743aa00a26c2183d1a03c54c80844bd', run_id=None)
    assert spec['binary_sha256'] == 'ff94da430681963b4d60ae582136b207e2dacf418aef67f4f90ada822f68759c'
    assert spec['parent_source'] == '82fbe9ef23eb39eb0a2eb3b3b214d50f69d3825b'
    assert len(spec['cases']) == 1 and spec['cases'][0]['point'] == POINT and spec['cases'][0]['seed'] == 4360
    assert spec['max_instr'] == 40000000 and spec['warmup'] == 20000000
    assert first['original_guest_live'] and all(first['controller'][key] == launch['process'][key] for key in ['pid', 'start_ticks'])
    assert first['process'] == first['launch_observation']['process']
    gate_path = evidence / 'short-gate-audit.json'
    assert digest(gate_path) == spec['short_gate_sha256']
    gate = json.loads(gate_path.read_text())
    assert gate['passed'] and gate['coremark_neutral'] and gate['spec_neutrality_pairs'] == gate['spec_expected'] == 3
    assert digest(root / 'query-cohort-observation/closure-receipt.json') == gate['source_sha256']['receipt'] == spec['short_closed_receipt_sha256']
    assert digest(root / 'query-cohort-observation/analysis/evidence.json') == gate['source_sha256']['analysis']
    return evidence, spec, launch, first


def collect(root):
    evidence, spec, launch, first = metadata(root)
    code = r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
root=Path(RUN_DIR);spec=SPEC;launch=LAUNCH;first=FIRST;folder=root/'points'/POINT
assert hashlib.sha256((root/'experiment.json').read_bytes()).hexdigest()==launch['spec_sha256']
assert hashlib.sha256((root/'runner.py').read_bytes()).hexdigest()==launch['runner_sha256']
state=json.loads((folder/'controller-state.json').read_text());process=json.loads((folder/'process.json').read_text())
assert all(state[key]==launch['process'][key]for key in ['pid','start_ticks'])and process==first['process']
def live(row):
 p=Path('/proc',str(row['pid']),'stat')
 return p.exists()and p.read_text().rsplit(')',1)[1].split()[19]==row['start_ticks']
controller_live=live(state);guest_live=live(process)
if controller_live or guest_live or state['phase']!='complete':
 print(json.dumps(dict(ready=False,utc=datetime.now(timezone.utc).isoformat(),controller=state,controller_live=controller_live,original_process_live=guest_live,database_read=False)))
else:
 result=json.loads((folder/'result.json').read_text());start=json.loads((folder/'launch-observation.json').read_text())
 assert start['original_process_live']and start['process']==process and result['source']==spec['source_identity']['source']
 assert result['binary_sha256']==spec['binary_sha256']and result['reference_sha256']==spec['reference_sha256']and result['cst_sha256']==spec['cst_sha256']
 assert result['spec_sha256']==launch['spec_sha256']and result['runner_sha256']==launch['runner_sha256']and(folder/'exitcode').read_text().strip()=='0'
 names=['points/'+POINT+'/'+name for name in FILES]+['experiment.json','runner.py','native_perf.py','short-gate-audit.json','short-closed-receipt.json'];files={}
 for name in names:
  p=root/name
  with p.open('rb')as stream:sha=hashlib.file_digest(stream,'sha256').hexdigest()
  files[name]=dict(bytes=p.stat().st_size,sha256=sha)
 print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),root=str(root),controller=state,process=process,controller_live=False,original_process_live=False,files=files,database_read=False)))
'''.replace('RUN_DIR', repr(spec['node_run_dir'])).replace('SPEC', repr(spec)).replace('LAUNCH', repr(launch)).replace('FIRST', repr(first)).replace('POINT', repr(POINT)).replace('FILES', repr(FILES))
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=15', 'node031', 'python3', '-'],
                           input=code, text=True, capture_output=True, check=True, timeout=50)
    receipt = json.loads(reply.stdout)
    (evidence / 'collection-status.json').write_text(json.dumps(receipt, indent=2) + '\n')
    if not receipt['ready']:
        return receipt
    stage = root / 'query-cohort-formal-observation'
    assert not stage.exists(), 'Analyze the existing closed formal archive'
    stage.mkdir()
    command = 'tar -C ' + shlex.quote(receipt['root']) + ' -czf - ' + ' '.join(map(shlex.quote, receipt['files']))
    with (stage / 'originals.tar.gz').open('wb') as stream:
        subprocess.run(['proxychains', '-q', 'ssh', 'node031', command], stdout=stream, check=True, timeout=60)
    with tarfile.open(stage / 'originals.tar.gz') as package:
        assert {row.name for row in package.getmembers()} == set(receipt['files'])
        package.extractall(stage, filter='data')
    verify(stage, receipt['files'])
    (stage / 'closure-receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    return receipt


def analyze(root):
    evidence, spec, launch, first = metadata(root)
    stage = root / 'query-cohort-formal-observation'
    parent = root / 'query-trigger-observation'
    missing = [name for name, folder in [('new363', stage), ('parent82', parent)] if not (folder / 'closure-receipt.json').exists()]
    if missing:
        return dict(pending=True, database_read=False, missing_closed_archives=missing)
    receipt = json.loads((stage / 'closure-receipt.json').read_text())
    previous = json.loads((parent / 'closure-receipt.json').read_text())
    assert receipt['ready'] and not receipt['controller_live'] and not receipt['original_process_live'] and receipt['controller']['phase'] == 'complete'
    assert receipt['process'] == first['process'] and all(receipt['controller'][key] == launch['process'][key] for key in ['pid', 'start_ticks'])
    assert previous['ready'] and not previous['controller_live'] and previous['controller']['phase'] == 'complete'
    assert previous['build']['source'] == spec['parent_source'] and previous['build']['binary_sha256'] == spec['parent_binary_sha256']
    for folder, proof in [(stage, receipt), (parent, previous)]:
        verify(folder, proof['files'])
    contract = cancel_source_contract(root / 'evidence/query-cohort-observe', spec['source_identity']['source'])
    folder = stage / 'points' / POINT
    record = json.loads((folder / 'result.json').read_text())
    assert record['source'] == spec['source_identity']['source'] and record['binary_sha256'] == spec['binary_sha256']
    assert record['spec_sha256'] == launch['spec_sha256'] and record['runner_sha256'] == launch['runner_sha256']
    for directory in [folder, parent / POINT]:
        command = json.loads((directory / 'command.json').read_text())
        for flag, value in [('-s', '4360'), ('-W', '20000000'), ('-I', '40000000'), ('-i', spec['cases'][0]['image']), ('--diff', spec['reference']), ('--cst-file', spec['cst'])]:
            assert command[command.index(flag) + 1] == value
    sys.path.insert(0, str(root))
    from native_perf import read_blocks, unique_counters
    module_spec = importlib.util.spec_from_file_location('closed_cohort_formal_helper', parent / 'tea_dip_query_endpoint_evidence.py')
    helper = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(helper)
    blocks = read_blocks(folder / 'simulator_err.txt')
    old = read_blocks(parent / POINT / 'simulator_err.txt')
    added = {name for block in blocks.values() for name in block} - {name for block in old.values() for name in block}
    assert len(added) == 26 and {name.rsplit('.', 1)[-1] for name in added} == set(spec['new_exact_counter_names'])
    neutrality = helper.old_execution(parent / POINT, folder, added)
    phases, prior = [], {}
    assert len(blocks) == 2
    for label, (stamp, block) in zip(['warmup', 'roi'], sorted(blocks.items())):
        counters = unique_counters(block)
        queues = frame_counts(block, spec['new_exact_counter_names'])
        rows = []
        for queue, counts in queues.items():
            span = gain_span(prior.get(queue, {key: 0 for key in counts}), counts)
            span['native_interval_gain_selected'] = counters[queue + '.diag_fdiv_query_gain_selected_uops']
            span['selection_interval_alignment_verified'] = span['delta']['selected'] == span['native_interval_gain_selected']
            span['source_verified_rejection_subsets'] = cancel_subsets(span)
            rows.append(dict(queue=queue, lifetime=counts, span=span))
        phases.append(dict(phase=label, native_print_stamp=stamp, queues=rows))
        prior = queues
    return dict(pending=False, database_read=True, point=POINT, parent_native_six_complete_table_neutrality=neutrality,
                cause_interpretation_valid=neutrality['passed'] and all(q['span']['selection_interval_alignment_verified'] for p in phases for q in p['queues']),
                phases=phases, source=spec['source_identity']['source'], cancel_source_contract=contract,
                consumer_sha256=digest(Path(__file__)), cohort_helper_sha256=spec['cohort_helper_sha256'],
                closure_receipt_sha256=digest(stage / 'closure-receipt.json'), parent_receipt_sha256=digest(parent / 'closure-receipt.json'),
                scope='One original363/82 paired formal only. Gain-selected issue attempts, not unique retired instructions or final WB. Pending-origin bounds retained. Nonneutral old execution or interval misalignment invalidates cause interpretation. No internal-FU service split, CPI conversion, suite score or adoption claim.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--action', choices=['collect', 'analyze'], required=True)
    options = parser.parse_args()
    root = options.report_dir.resolve()
    result = collect(root) if options.action == 'collect' else analyze(root)
    if options.action == 'analyze' and not result['pending']:
        folder = root / 'query-cohort-formal-observation/analysis'
        folder.mkdir(exist_ok=True)
        (folder / 'evidence.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
