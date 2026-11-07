"""Compare the one original non-PGO soplex query repeat with its closed PGO CI window."""
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

POINT = 'soplex_pds-50_9466_0.126190'
FILES = ['command.json', 'result.json', 'exitcode', 'simulator_out.txt', 'simulator_err.txt',
         'simulator.db', 'process.json', 'launch-observation.json', 'controller-state.json']


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def verify(folder, files):
    for name, row in files.items():
        path = folder / name
        assert path.stat().st_size == row['bytes'] and digest(path) == row['sha256'], name


def experiment(root):
    evidence = root / 'evidence/query-soplex-pgo-check'
    spec = json.loads((evidence / 'experiment.json').read_text())
    launch = json.loads((evidence / 'launch.json').read_text())
    assert digest(evidence / 'experiment.json') == launch['spec_sha256']
    assert digest(evidence / 'runner.py') == launch['runner_sha256']
    assert spec['source_identity']['source'] == 'b0a22b93a867c5dff56b1b3fb32708c9e6363b01'
    assert spec['source_identity']['run_id'] == 42159911532
    assert spec['binary_sha256'] == '401a3d8b0258a8d91fadf9fc1fae8dd207c6508938b146ff589f8f0f0247bb9e'
    assert launch['controller']['pid'] == 3828135 and launch['controller']['start_ticks'] == '46891515'
    assert launch['guest']['pid'] == 3828138 and launch['guest']['start_ticks'] == '46891537'
    assert len(spec['cases']) == 1 and spec['cases'][0]['point'] == POINT and spec['cases'][0]['seed'] == 4360
    return evidence, spec, launch


def collect(root):
    evidence, spec, launch = experiment(root)
    code = r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
spec=SPEC;launch=LAUNCH;root=Path(spec['node_run_dir']);folder=root/'points'/POINT
assert hashlib.sha256((root/'experiment.json').read_bytes()).hexdigest()==launch['spec_sha256']
assert hashlib.sha256((root/'runner.py').read_bytes()).hexdigest()==launch['runner_sha256']
controller=json.loads((folder/'controller-state.json').read_text());process=json.loads((folder/'process.json').read_text())
assert all(controller[key]==launch['controller'][key] for key in ['pid','start_ticks']) and process==launch['guest']
def live(row):
 p=Path('/proc',str(row['pid']),'stat')
 return p.exists() and p.read_text().rsplit(')',1)[1].split()[19]==row['start_ticks']
controller_live=live(controller);guest_live=live(process)
if controller_live or guest_live or controller['phase']!='complete':
 print(json.dumps(dict(ready=False,utc=datetime.now(timezone.utc).isoformat(),controller=controller,controller_live=controller_live,original_process_live=guest_live,database_read=False)))
else:
 result=json.loads((folder/'result.json').read_text());start=json.loads((folder/'launch-observation.json').read_text())
 assert start['original_process_live'] and start['process']==process
 assert result['source']==spec['source_identity']['source'] and result['binary_sha256']==spec['binary_sha256']
 assert (folder/'exitcode').read_text().strip()=='0'
 files={}
 for name in FILES:
  path=folder/name
  with path.open('rb')as stream:sha=hashlib.file_digest(stream,'sha256').hexdigest()
  files[name]=dict(bytes=path.stat().st_size,sha256=sha)
 print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),root=str(folder),controller=controller,process=process,controller_live=False,original_process_live=False,files=files,database_read=False)))
'''.replace('POINT', repr(POINT)).replace('SPEC', repr(spec)).replace('LAUNCH', repr(launch)).replace('FILES', repr(FILES))
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=15', 'node031', 'python3', '-'],
                           input=code, text=True, capture_output=True, timeout=50, check=True)
    receipt = json.loads(reply.stdout)
    (evidence / 'collection-status.json').write_text(json.dumps(receipt, indent=2) + '\n')
    if not receipt['ready']:
        return receipt
    stage = root / 'query-soplex-pgo-check'
    assert not stage.exists(), 'Analyze the existing original closed archive'
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
    stage = root / 'query-soplex-pgo-check'
    receipt_path = stage / 'closure-receipt.json'
    if not receipt_path.exists():
        return dict(pending=True, database_read=False, reason='Original soplex non-PGO repeat is not archived closed')
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['controller_live'] and not receipt['original_process_live']
    assert receipt['controller']['phase'] == 'complete'
    evidence, spec, launch = experiment(root)
    assert receipt['process'] == launch['guest']
    assert all(receipt['controller'][key] == launch['controller'][key] for key in ['pid', 'start_ticks'])
    verify(stage, receipt['files'])
    record = json.loads((stage / 'result.json').read_text())
    assert record['source'] == spec['source_identity']['source'] and record['binary_sha256'] == spec['binary_sha256']
    assert record['spec_sha256'] == launch['spec_sha256'] and record['runner_sha256'] == launch['runner_sha256']
    ci_receipt_path = root / 'fdiv-wb-query-paired-ci/raw/closed-receipts' / (POINT + '.json')
    assert digest(ci_receipt_path) == spec['original_ci_closed_receipt_sha256']
    ci_receipt = json.loads(ci_receipt_path.read_text())
    assert ci_receipt['run_id'] == 42159911532 and ci_receipt['source'] == spec['source_identity']['ci_source']
    assert not ci_receipt['original_process_live'] and ci_receipt['runner_success_lines']
    assert ci_receipt['artifact_sha256'] == dict(emu=spec['source_identity']['ci_binary_sha256'], reference=spec['reference_sha256'], cst=spec['cst_sha256'])
    ci = root / 'fdiv-wb-query-paired-ci/raw/results' / POINT
    verify(ci, ci_receipt['files'])
    command = json.loads((stage / 'command.json').read_text())
    previous = ci_receipt['last_live_process_observation']['args']
    for flag, value in [('-s', '4360'), ('-i', spec['cases'][0]['image']), ('-W', '20000000'), ('-I', '40000000'), ('--diff', spec['reference']), ('--cst-file', spec['cst']), ('--dump-select-db', 'Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0')]:
        assert command[command.index(flag) + 1] == previous[previous.index(flag) + 1] == value
    helper_path = Path(__file__).with_name('tea_dip_query_endpoint_evidence.py')
    sys.path.insert(0, str(root))
    helper_spec = importlib.util.spec_from_file_location('closed_query_soplex_helper', helper_path)
    helper = importlib.util.module_from_spec(helper_spec)
    helper_spec.loader.exec_module(helper)
    from analyze_windows import extract
    equality = helper.old_execution(ci, stage, set())
    windows = [extract(folder, label)[0] for folder, label in [(ci, 'query-ci-pgo'), (stage, 'query-replay-nonpgo')]]
    for window in windows:
        assert window['seed'] == 4360 and abs(window['warmup_instructions'] - 20000000) < 128 and abs(window['instructions'] - 20000000) < 128
    original = json.loads((root / 'fdiv-wb-query-paired-ci/partial-analysis' / (POINT + '.json')).read_text())
    assert original['run_id'] == ci_receipt['run_id'] and original['source'] == ci_receipt['source']
    assert original['closed_receipt_sha256'] == digest(ci_receipt_path)
    assert original['analysis_source_sha256'] == digest(Path(__file__).with_name('tea_dip_analyze_ci_closed_point.py'))
    assert windows[0]['cpi'] == original['comparison']['candidate_cpi']
    return dict(pending=False, database_read=True, point=POINT, pgo_vs_nonpgo_all_native_six_complete_table_equality=equality,
                original_ci_cpi_reproduced=windows[1]['cpi'] == windows[0]['cpi'], windows=windows,
                speedup_vs_archived841=original['comparison']['control_cpi'] / windows[1]['cpi'] - 1,
                consumer_sha256=digest(Path(__file__)), helper_sha256=digest(helper_path), closed_receipt_sha256=digest(receipt_path),
                performance_causality_validated=None,
                scope='One original same-seed formal query repeat across independentPGO/nonPGO binaries. No native-name exclusions; six full tables compared. Mismatches retained rather than relabelled neutral. Equality limits build-sensitive explanations to this window, not all inputs or host-time cause. No suite score or default adoption.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--action', choices=['collect', 'analyze'], required=True)
    options = parser.parse_args()
    root = options.report_dir.resolve()
    result = collect(root) if options.action == 'collect' else analyze(root)
    if options.action == 'analyze' and not result['pending']:
        output = root / 'query-soplex-pgo-check/analysis'
        output.mkdir(exist_ok=True)
        (output / 'evidence.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
