"""Verify original query-candidate closure before issuing receipts for collected raw files."""
from pathlib import Path
import argparse
import hashlib
import json
import subprocess


REMOTE = '/nfs/home/wujiabin/work/xs-env/rtl-dip-experiments-20261103'


def closure_script(stage, observed):
    return r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json,socket
root=Path(ROOT);stage=STAGE;observed=OBSERVED;variant='fdiv-wb-query'
folder=root/stage/variant
build=json.loads((root/variant/'build-state.json').read_text())
controller=json.loads((folder/'run-state.json').read_text())
spec=json.loads((folder/'experiment.json').read_text())
rows=[];pending=[]
def live(process):
 path=Path('/proc',str(process['pid']),'stat')
 return path.exists() and path.read_text().rsplit(')',1)[1].split()[19]==process['start_ticks']
for case in spec['cases']:
 point=case['point'];directory=folder/'points'/point;old=observed.get(str(directory))
 if old is None:
  pending.append(dict(point=point,reason='No original live observation',database_read=False));continue
 process=json.loads((directory/'process.json').read_text())
 assert process=={key:old[key] for key in ['pid','start_ticks','host','core']}
 assert process['host']==socket.gethostname()
 if live(process):
  pending.append(dict(point=point,reason='Original process still live',database_read=False));continue
 if not (directory/'result.json').exists():
  pending.append(dict(point=point,reason='Original process closed without successful result',database_read=False));continue
 assert (directory/'exitcode').read_text().strip()=='0'
 result=json.loads((directory/'result.json').read_text())
 assert build['phase']=='complete' and result['source']==build['source'] and result['binary_sha256']==build['binary_sha256']
 files={}
 for name in ['command.json','result.json','counters.json','exitcode','simulator_out.txt','simulator_err.txt','simulator.db','process.json']:
  path=directory/name
  with path.open('rb') as stream:sha=hashlib.file_digest(stream,'sha256').hexdigest()
  files[name]=dict(bytes=path.stat().st_size,sha256=sha)
 rows.append(dict(utc=datetime.now(timezone.utc).isoformat(),stage=stage,variant=variant,point=point,process=process,original_process_live=False,last_live_observation=old,exitcode='0',source=build['source'],binary_sha256=build['binary_sha256'],controller=controller,controller_live=live(controller),files=files))
print(json.dumps(dict(utc=datetime.now(timezone.utc).isoformat(),rows=rows,pending=pending)))
'''.replace('ROOT', repr(REMOTE)).replace('STAGE', repr(stage)).replace('OBSERVED', repr(observed))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--stage', choices=['wb-query', 'wb-query-formal'], required=True)
    args = parser.parse_args()
    root = args.report_dir.resolve(); stage = root/args.stage
    watcher = 'wb-query-process-watch-tools' if args.stage == 'wb-query' else 'wb-query-formal-tools'
    observed = {}
    for line in (root/'evidence'/watcher/'process-events.jsonl').read_text().splitlines():
        event = json.loads(line)
        for guest in event.get('guests', []):
            if guest['live']:
                observed[guest['directory']] = dict(guest, observation_utc=event['utc'])
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', 'node031', 'python3', '-'],
                           input=closure_script(args.stage, observed), text=True, capture_output=True, timeout=50, check=True)
    result = json.loads(reply.stdout)
    manifest_path = stage/'manifest.json'
    manifest = json.loads(manifest_path.read_text()) if manifest_path.exists() else {}
    written = []; existing = []
    for row in result['rows']:
        receipt = stage/'closed-receipts'/(row['point']+'.json')
        prefix = row['variant']+'/points/'+row['point']+'/'
        for name, entry in row['files'].items():
            assert manifest[prefix+name] == entry, 'Run collect_completed.py for this stage before creating receipts'
            path = stage/prefix/name
            with path.open('rb') as stream:sha = hashlib.file_digest(stream, 'sha256').hexdigest()
            assert path.stat().st_size == entry['bytes'] and sha == entry['sha256']
        if receipt.exists():
            old = json.loads(receipt.read_text())
            for key in ['stage', 'variant', 'point', 'process', 'original_process_live', 'exitcode', 'source', 'binary_sha256', 'files']:
                assert old[key] == row[key], ('Existing original closure changed', row['point'], key)
            existing.append(row['point'])
        else:
            receipt.parent.mkdir(parents=True, exist_ok=True)
            receipt.write_text(json.dumps(row, indent=2)+'\n'); written.append(row['point'])
    result.update(stage=args.stage, written=written, already_verified=existing,
                  consumer_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest())
    stage.mkdir(parents=True, exist_ok=True)
    (stage/'closure-status.json').write_text(json.dumps(result, indent=2)+'\n')
    print(json.dumps(dict(stage=args.stage, written=written, already_verified=existing, pending=result['pending']), indent=2))


if __name__ == '__main__':
    main()
