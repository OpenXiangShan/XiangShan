"""Collect closed points from either active credit CI, without reading live databases."""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import json
import re
import shlex
import subprocess
import tarfile


def remote(host, code):
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', host, 'python3', '-'],
                           input=code, text=True, capture_output=True, timeout=50, check=True)
    return json.loads(reply.stdout)


def closure_script(identity, point, old):
    """The file hashes are evaluated only after runner success and original PID closure."""
    return r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
identity=IDENTITY;point=POINT;old=OBSERVATION;root=Path(identity['report'])
proc=Path('/proc',str(old['pid']),'stat')
live=proc.exists() and proc.read_text().rsplit(')',1)[1].split()[19]==old['start_ticks']
lines=[line for line in (root/identity['runner_log']).read_text().splitlines() if point+' succeeded' in line]
if live or not lines:
 print(json.dumps(dict(ready=False,point=point,original_process_live=live,runner_success_observed=bool(lines),database_read=False)))
else:
 def digest(p):
  with p.open('rb') as f:return hashlib.file_digest(f,'sha256').hexdigest()
 argv=old['args'];assert argv[0]==str(root/'emu-gsim')
 assert argv[argv.index('--db-path')+1]==str(root/point/'simulator.db')
 files={name:dict(bytes=(root/point/name).stat().st_size,sha256=digest(root/point/name)) for name in ['simulator_out.txt','simulator_err.txt','simulator.db']}
 artifacts=dict(emu=root/'emu-gsim',reference=root/'riscv64-nemu-interpreter-so',cst=Path(argv[argv.index('--cst-file')+1]))
 print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),point=point,host=old['host'],run_id=identity['run_id'],source=identity['source'],original_pid=old['pid'],original_start_ticks=old['start_ticks'],original_process_live=False,last_live_process_observation=old,runner_success_lines=lines,files=files,artifact_sha256={k:digest(p) for k,p in artifacts.items()})))
'''.replace('IDENTITY', repr(identity)).replace('POINT', repr(point)).replace('OBSERVATION', repr(old))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--ci-evidence', required=True, choices=['ci-credit-div', 'ci-fdiv-credit', 'fdiv-wb-query-paired-ci','fdiv-turnover-ci-20261108'])
    parser.add_argument('--point', help='Check one observed point, including a still-running point')
    parser.add_argument('--check-only', action='store_true', help='Verify closure and hashes without copying raw files')
    args = parser.parse_args()
    root = args.report_dir.resolve(); evidence = root/'evidence'/args.ci_evidence
    identity = json.loads((evidence/'archive-identity.json').read_text())
    run = json.loads((evidence/'run.json').read_text())
    assert identity['run_id'] == run['id'] and identity['source'] == run['head_sha']
    jobs = json.loads((evidence/'jobs.json').read_text())['jobs']
    runner_host = re.search(r'node\d+', jobs[0]['runner_name'])[0]
    scan = r'''from pathlib import Path
import json,re
root=Path(REPORT);text=(root/RUNNER).read_text()
print(json.dumps(dict(succeeded=sorted(set(re.findall(r'([A-Za-z0-9_.-]+) succeeded',text))))))
'''.replace('REPORT', repr(identity['report'])).replace('RUNNER', repr(identity['runner_log']))
    succeeded = remote(runner_host, scan)['succeeded']
    observations = {}
    for line in (evidence/'resource-events.jsonl').read_text().splitlines():
        event = json.loads(line)
        assert event['run_id'] == identity['run_id'] and event['source'] == identity['source']
        for node in event['nodes']:
            for guest in node['processes']:
                if guest['role'] == 'guest':
                    observations[guest['point']] = dict(guest, host=node['host'], observation_utc=event['utc'])
    points = [args.point] if args.point else succeeded
    raw = root/args.ci_evidence/'raw'; collected = []; pending = []; existing = []
    for point in points:
        if (raw/'closed-receipts'/(point+'.json')).exists():
            existing.append(point); continue
        if point not in observations:
            pending.append(dict(point=point,reason='No original process observation; closure unproven',database_read=False)); continue
        old = observations[point]
        receipt = remote(old['host'].split('.')[0], closure_script(identity, point, old))
        if not receipt['ready']:
            pending.append(receipt); continue
        expected = dict(emu=identity['emu_sha256'],reference=identity['reference_sha256'],cst=identity['cst_sha256'])
        assert receipt['run_id'] == run['id'] and receipt['source'] == run['head_sha'] and receipt['artifact_sha256'] == expected
        if args.check_only:
            collected.append(dict(point=point,closure_verified=True,files_copied=False,receipt=receipt)); continue
        assert not (raw/'results'/point).exists(), 'Existing raw files have no closure receipt; inspect them before collecting'
        for name in ['archives','results','closed-receipts']:(raw/name).mkdir(parents=True,exist_ok=True)
        names = [point+'/'+name for name in receipt['files']]
        archive = raw/'archives'/(datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%SZ')+'-'+point+'.tar.gz')
        archive.with_suffix('.receipt.json').write_text(json.dumps(receipt,indent=2)+'\n')
        command = 'tar -C '+shlex.quote(identity['report'])+' -czf - '+' '.join(map(shlex.quote,names))
        with archive.open('wb') as stream:
            subprocess.run(['proxychains','-q','ssh',old['host'].split('.')[0],command],stdout=stream,check=True,timeout=50)
        with tarfile.open(archive) as package:
            assert {m.name for m in package.getmembers()} == set(names)
            package.extractall(raw/'results',filter='data')
        manifest_path = raw/'manifest.json'
        manifest = json.loads(manifest_path.read_text()) if manifest_path.exists() else {}
        for name, entry in receipt['files'].items():
            body = (raw/'results'/point/name).read_bytes()
            assert len(body) == entry['bytes'] and hashlib.sha256(body).hexdigest() == entry['sha256'], f'{point}/{name}: copied data differ from archived closure receipt {archive.with_suffix(".receipt.json")}'
            manifest['results/'+point+'/'+name] = entry
        manifest_path.write_text(json.dumps(manifest,indent=2)+'\n')
        (raw/'closed-receipts'/(point+'.json')).write_text(json.dumps(receipt,indent=2)+'\n')
        (raw/'run.json').write_text(json.dumps(run,indent=2)+'\n')
        collected.append(dict(point=point,closure_verified=True,files_copied=True))
    status = dict(utc=datetime.now(timezone.utc).isoformat(),run_id=run['id'],source=run['head_sha'],ci_evidence=args.ci_evidence,
                  runner_succeeded_points=succeeded,collected=collected,pending=pending,already_collected=existing,
                  source_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                  scope='Closed per-point raw collection only. No dispatch, server selection, live DB reads, partial-suite score or adoption claim.')
    target = root/args.ci_evidence; target.mkdir(exist_ok=True)
    (target/'collection-status.json').write_text(json.dumps(status,indent=2)+'\n')
    print(json.dumps(status,indent=2))


if __name__ == '__main__':
    main()
