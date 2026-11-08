"""Archive one registered BPU observer guest as soon as its original process closes."""
from pathlib import Path
import argparse
import hashlib
import json
import shlex
import subprocess
import tarfile

from tea_dip_query_endpoint_evidence import digest, verify

CODE = r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json,socket,subprocess
launch=LAUNCH
root=Path(launch['run_dir'])
assert socket.gethostname().split('.')[0]==launch['process']['host'].split('.')[0]
def sha(path):
 with path.open('rb') as stream:return hashlib.file_digest(stream,'sha256').hexdigest()
def live(process):
 path=Path('/proc',str(process['pid']),'stat')
 return path.exists()and path.read_text().rsplit(')',1)[1].split()[19]==process['start_ticks']
assert sha(root/'experiment.json')==launch['spec_sha256']
assert sha(root/'tea_dip_bpu_commit_run.py')==launch['runner_sha256']
spec=json.loads((root/'experiment.json').read_text());point=spec['case']['point'];folder=root/point
state=json.loads((root/'run-state.json').read_text())
assert all(state[key]==launch['process'][key]for key in ['pid','start_ticks'])
if not(folder/'process.json').exists()or not(folder/'result.json').exists():
 print(json.dumps(dict(ready=False,point=point,phase=state['phase'],controller_live=live(state),database_read=False)));raise SystemExit(0)
process=json.loads((folder/'process.json').read_text());result=json.loads((folder/'result.json').read_text())
if live(process):
 print(json.dumps(dict(ready=False,point=point,original_process_live=True,database_read=False)));raise SystemExit(0)
assert result['original_process_closed']and result['process']==process
assert result['source']==spec['source']==launch['spec']['source']
build=json.loads((root/'build-identity.json').read_text())
assert build['phase']=='complete'and build['source']==spec['source']and build['binary_sha256']==result['binary_sha256']
if result['difftest_passed']and 'spec_sha256'not in result:
 print(json.dumps(dict(ready=False,point=point,original_process_live=False,phase='finalizing_result',database_read=False)));raise SystemExit(0)
if result['difftest_passed']:
 assert result['spec_sha256']==launch['spec_sha256']and result['runner_sha256']==launch['runner_sha256']
names=[point+'/'+name for name in ['command.json','result.json','exitcode','process.json','launch-observation.json','simulator_out.txt','simulator_err.txt','simulator.db']]
names+=['experiment.json','tea_dip_bpu_commit_run.py','tea_dip_fdiv_ready_run.py','tea_dip_fir_registers.py','tea_dip_query_endpoint_evidence.py','tea_dip_wb_protocol_micro.py','native_perf.py','control-closure-receipt.json','build-identity.json','resource-audit.json']
files={name:dict(bytes=(root/name).stat().st_size,sha256=sha(root/name))for name in names}
archive=root/'bpu-closed-guest.tar.gz'
if not archive.exists():subprocess.run(['tar','-czf',str(archive),'-C',str(root),*names],check=True)
print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),point=point,source=spec['source'],root=str(root),process=process,original_process_live=False,controller_live=live(state),spec_sha256=launch['spec_sha256'],runner_sha256=launch['runner_sha256'],files=files,archive=str(archive),archive_sha256=sha(archive),database_read=False)))
'''


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--launch', type=Path, required=True)
    parser.add_argument('--output-dir', type=Path, required=True)
    args = parser.parse_args()
    out = args.output_dir.resolve()
    if (out / 'closure-receipt.json').exists():
        receipt = json.loads((out / 'closure-receipt.json').read_text())
        assert receipt['launch_sha256'] == digest(args.launch)
        verify(out, receipt['files'])
        print(json.dumps(dict(ready=True, already_collected=True, point=receipt['point'])))
        return
    launch = json.loads(args.launch.read_text())
    host = launch['process']['host'].split('.')[0]
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=12', host, 'python3', '-'],
                           input=CODE.replace('LAUNCH', repr(launch)), capture_output=True, text=True, check=True, timeout=50)
    receipt = json.loads(reply.stdout)
    if not receipt['ready']:
        print(json.dumps(receipt, indent=2))
        return
    assert not out.exists()
    out.mkdir(parents=True)
    archive = out / 'originals.tar.gz'
    subprocess.run(['proxychains', '-q', 'scp', host + ':' + shlex.quote(receipt['archive']), str(archive)],
                   check=True, capture_output=True, text=True, timeout=90)
    assert digest(archive) == receipt['archive_sha256']
    with tarfile.open(archive, 'r:gz') as packed:
        assert {item.name for item in packed.getmembers()} == set(receipt['files'])
        for name, entry in receipt['files'].items():
            raw = packed.extractfile(name).read()
            assert len(raw) == entry['bytes'] and hashlib.sha256(raw).hexdigest() == entry['sha256']
            target = out / name
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(raw)
    receipt.update(launch_sha256=digest(args.launch), collector_sha256=digest(Path(__file__)))
    (out / 'closure-receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print(json.dumps(dict(ready=True, point=receipt['point'], files=len(receipt['files']),
                         controller_live=receipt['controller_live'], database_read=False), indent=2))


if __name__ == '__main__':
    main()
