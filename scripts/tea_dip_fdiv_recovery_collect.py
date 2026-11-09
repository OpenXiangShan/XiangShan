"""Collect a closed point from the registered 4339/edba reboot recovery.

The parent may still be running. An absent result after a different boot is an
interruption, not success. No live SQLite file is copied or queried.
"""
import argparse
import json
from pathlib import Path
import subprocess
import tarfile

from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_wb_protocol_micro import write

FILES = ['command.json', 'result.json', 'exitcode', 'process.json',
         'launch-observation.json', 'simulator_out.txt', 'simulator_err.txt', 'simulator.db']
OBSERVER = '4339dea45cf776f469c366d147bdf720d03aa79b'
CANDIDATE = 'edba344edde71f1babbd021176d5b61da1819209'


def metadata(evidence, case_name):
    launch = json.loads((evidence / 'launch.json').read_text())
    spec = json.loads((evidence / 'experiment.json').read_text())
    assert digest(evidence / 'experiment.json') == launch['spec_sha256']
    assert spec['run_dir'] == launch['run_dir']
    assert digest(evidence / 'run.py') == launch['runner_sha256']
    assert spec['builds']['observer']['source'] == OBSERVER
    assert spec['builds']['candidate']['source'] == CANDIDATE
    cases = {spec['context_case']['point']: ('observer', spec['context_case'])}
    cases.update({'candidate-' + c['point']: ('candidate', c) for c in spec['candidate_cases']})
    role, case = cases[case_name]
    return dict(launch=launch, spec=spec, case_name=case_name, role=role, case=case)


def collect(evidence, case_name, check_only=False):
    params = metadata(evidence, case_name)
    launch = params['launch']
    stage = evidence / 'closed-points' / case_name
    receipt_file = stage / 'closure-receipt.json'
    if receipt_file.exists():
        receipt = json.loads(receipt_file.read_text())
        assert receipt['case_name'] == case_name and receipt['spec_sha256'] == launch['spec_sha256']
        verify(stage, receipt['files'])
        return receipt
    host = launch['process']['host'].split('.')[0]
    code = 'params=' + repr(params) + '\n' + r'''from datetime import datetime,timezone
from pathlib import Path
import hashlib,json
launch=params['launch'];root=Path(launch['run_dir']);spec=params['spec']
case=params['case'];name=params['case_name'];folder=root/name
boot=Path('/proc/sys/kernel/random/boot_id').read_text().strip()
same_boot=boot==launch['process']['boot_id']
def sha(p):
 with p.open('rb')as stream:return hashlib.file_digest(stream,'sha256').hexdigest()
def live(process,known_boot=None):
 if known_boot is not None and known_boot!=boot:return False
 p=Path('/proc',str(process['pid']),'stat')
 return p.exists()and p.read_text().rsplit(')',1)[1].split()[19]==str(process['start_ticks'])
assert sha(root/'experiment.json')==launch['spec_sha256']
assert json.loads((root/'experiment.json').read_text())==spec
assert sha(root/'run.py')==launch['runner_sha256']
state=json.loads((root/'run-state.json').read_text())
assert all(state[k]==launch['process'][k]for k in ['pid','start_ticks','boot_id'])
parent_live=live(launch['process'],launch['process']['boot_id'])
if not(folder/'result.json').exists():
 print(json.dumps(dict(ready=False,case_name=name,controller_live=parent_live,controller=state,
  observed_boot_id=boot,launch_boot_id=launch['process']['boot_id'],
  interrupted=not same_boot,original_process_closed=None,database_read=False)))
 raise SystemExit(0)
process=json.loads((folder/'process.json').read_text())
if live(process,launch['process']['boot_id']):
 print(json.dumps(dict(ready=False,case_name=name,original_process_live=True,database_read=False)))
 raise SystemExit(0)
if not(root/'context-opportunity.json').exists():
 print(json.dumps(dict(ready=False,case_name=name,original_process_live=False,
  context_audit_pending=True,database_read=False)))
 raise SystemExit(0)
result=json.loads((folder/'result.json').read_text());build=spec['builds'][params['role']]
assert result['process']==process and result['original_process_closed']
assert result['source']==build['source']and result['binary_sha256']==build['binary_sha256']
assert result['exitcode']==0 and result['difftest_passed']and all(result['sampler_flags'].values())
assert int((folder/'exitcode').read_text())==0 and sha(Path(build['binary']))==build['binary_sha256']
command=json.loads((folder/'command.json').read_text())
assert command[6]==build['binary']and command[command.index('-c')+1]==str(case['core'])
for flag in ['-I','-W','-C','-s','-i','--diff','--cst-file','--dump-select-db']:
 assert command[command.index(flag)+1]==case['command'][case['command'].index(flag)+1]
for flag in ['-i','--diff','--cst-file']:
 p=Path(command[command.index(flag)+1]);assert sha(p)==spec['input_hashes'][str(p)]
if params['role']=='observer':
 control=Path(spec['context_gate']['control_root'])/case['point']
 control_source='ec964c8a623c8f72d32970cf984a8c8be8708d03'
else:
 paths=[Path(p)for p in spec['input_hashes']if p.endswith('/fdiv-turnover-validation-20261108/build-identity.json')]
 control_build_path,=paths
 assert sha(control_build_path)==spec['input_hashes'][str(control_build_path)]
 control_build=json.loads(control_build_path.read_text())
 assert control_build['source']=='8f8ece60ac8b08b358743bc9db15a5a227dde0b8'
 control=control_build_path.parent/case['point'];control_source=control_build['source']
 gate=json.loads((root/'context-opportunity.json').read_text())
 assert not gate['pending']and gate['supported']and gate['old_native_six_tables_neutral']and gate['context_mask6_cycles']>0
before=json.loads((control/'result.json').read_text());before_process=json.loads((control/'process.json').read_text())
assert before['process']==before_process and before['original_process_closed']and not live(before_process)
assert before['source']==control_source and before['exitcode']==0 and before['difftest_passed']and all(before['sampler_flags'].values())
if params['role']=='candidate':assert before['binary_sha256']==control_build['binary_sha256']
old_command=json.loads((control/'command.json').read_text())
for flag in ['-I','-W','-C','-s','-i','--diff','--cst-file','--dump-select-db']:
 assert old_command[old_command.index(flag)+1]==command[command.index(flag)+1]
paths={str(Path('measured')/n):folder/n for n in __FILES__}
paths.update({str(Path('control')/n):control/n for n in __FILES__})
# The running parent's mutable state is captured in the receipt, not the archive.
for n in ['experiment.json','launch.json','run.py','build-identity.json',*launch['tools']]:paths[n]=root/n
p=root/'context-opportunity.json'
if p.exists():paths[p.name]=p
files={n:dict(path=str(p),bytes=p.stat().st_size,sha256=sha(p))for n,p in paths.items()}
for p,expected in spec['input_hashes'].items():
 if p in [str(q)for q in paths.values()]:assert sha(Path(p))==expected
print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),case_name=name,
 role=params['role'],point=case['point'],root=str(root),spec_sha256=launch['spec_sha256'],
 controller=state,controller_live=parent_live,original_process_live=False,process=process,
 launch_boot_id=launch['process']['boot_id'],observed_boot_id=boot,files=files,database_read=False)))
'''.replace('__FILES__', repr(FILES))
    command = ['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=10', host, 'python3', '-']
    reply = subprocess.run(command, input=code, text=True, capture_output=True, check=True, timeout=50)
    receipt = json.loads(reply.stdout)
    if not receipt['ready'] or check_only:
        return receipt
    assert not stage.exists(), 'Inspect the partial local collection before continuing'
    stage.mkdir(parents=True)
    write(stage / 'collection-receipt.json', receipt)
    stream_code = 'files=' + repr(receipt['files']) + '\n' + r'''import sys,tarfile
with tarfile.open(fileobj=sys.stdout.buffer,mode='w|gz')as archive:
 for name,entry in files.items():archive.add(entry['path'],arcname=name,recursive=False)
'''
    with (stage / 'originals.tar.gz').open('wb') as stream:
        subprocess.run(command, input=stream_code.encode(), stdout=stream, check=True, timeout=55)
    with tarfile.open(stage / 'originals.tar.gz') as archive:
        assert {m.name for m in archive.getmembers()} == set(receipt['files'])
        archive.extractall(stage, filter='data')
    verify(stage, receipt['files'])
    assert digest(stage / 'experiment.json') == launch['spec_sha256']
    assert digest(stage / 'run.py') == launch['runner_sha256']
    receipt['collector_sha256'] = digest(Path(__file__))
    write(receipt_file, receipt)
    return receipt


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--evidence-dir', type=Path, required=True)
    parser.add_argument('--case', required=True, help='Context point name or candidate-<point> from the frozen experiment')
    parser.add_argument('--check-only', action='store_true')
    args = parser.parse_args()
    receipt = collect(args.evidence_dir.resolve(), args.case, args.check_only)
    print(json.dumps({k: v for k, v in receipt.items() if k != 'files'}, indent=2))


if __name__ == '__main__':
    main()
