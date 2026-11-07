"""Recover current-CI closed windows using runner exit and current point-path absence.

Missing historical PID identities stay missing. This adds an explicit alternative
closure proof; it never changes old observations or restarts a simulation.
"""
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import json
import re
import shlex
import subprocess
import tarfile

METHOD = 'runner_exit_and_point_path_absence'


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def completed_attempt(lines, point):
    started = [line for line in lines if re.search(r'\bStarted ' + re.escape(point) + r' on (node\d+)\b', line)]
    finished = [line for line in lines if re.search(r'\b' + re.escape(point) + r' succeeded\s+on (node\d+)\b', line)]
    assert len(started) == len(finished) == 1, 'A single complete runner attempt is required'
    assert lines.index(started[0]) < lines.index(finished[0])
    host = re.search(r' on (node\d+)\b', started[0])[1]
    assert re.search(r'\bon (node\d+)\b', finished[0])[1] == host
    assert not any(point + ' failed' in line or point + ' is RUNNING' in line for line in lines)
    return dict(host=host, started=started[0], succeeded=finished[0])


def remote(host, code):
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=15', host, 'python3', '-'],
                           input=code, capture_output=True, text=True, timeout=55, check=True)
    return json.loads(reply.stdout)


def scan_code(report, point):
    return r'''from pathlib import Path
from datetime import datetime,timezone
import json,pwd,socket
report=Path(REPORT);point=POINT;target=str(report/point/'simulator.db');matches=[];unreadable=[];uid=pwd.getpwnam('cirunner').pw_uid
for proc in Path('/proc').glob('[0-9]*'):
 try:
  owner=proc.stat().st_uid
  try:args=proc.joinpath('cmdline').read_bytes().decode(errors='replace').split(chr(0))[:-1]
  except PermissionError:
   if owner==uid:unreadable.append(int(proc.name))
   continue
  if '--db-path'in args and args[args.index('--db-path')+1]==target:
   stat=proc.joinpath('stat').read_text().rsplit(')',1)[1].split()
   matches.append(dict(pid=int(proc.name),start_ticks=stat[19],args=args))
 except (FileNotFoundError,ProcessLookupError):continue
print(json.dumps(dict(utc=datetime.now(timezone.utc).isoformat(),host=socket.gethostname(),point=point,matches=matches,unreadable_runner_processes=unreadable,database_read=False)))
'''.replace('REPORT', repr(report)).replace('POINT', repr(point))


def collect(root, point, check_only):
    evidence = root / 'evidence/fdiv-wb-query-paired-ci'
    contract = root / 'evidence/query-ci-completion-contract'
    identity = json.loads((evidence / 'archive-identity.json').read_text())
    run = json.loads((evidence / 'run.json').read_text())
    assert run['id'] == identity['run_id'] == 42159911532
    assert run['head_sha'] == identity['source'] == 'c600630b787e3f0461fcf6104f46c5801a2b015b'
    seeds = json.loads((evidence / 'inputs/spec06-cov03-seeds.json').read_text())
    assert point in seeds
    raw = root / 'fdiv-wb-query-paired-ci/raw'
    receipt_path = raw / 'closed-receipts' / (point + '.json')
    assert not receipt_path.exists(), 'Preserve the existing closure proof'
    poll_path = contract / 'runner-poll-exit-validation.json'
    poll = json.loads(poll_path.read_text())
    assert poll['passed']
    source = json.loads((contract / 'actual-runner-source.json').read_text())
    assert poll['runner_server_sha256'] == source['files']['modules/server.py']['sha256']
    source_files = {name: row['sha256'] for name, row in source['files'].items()}
    for name, sha in source_files.items():
        assert digest(contract / name) == sha
    code = r'''from pathlib import Path
import json,hashlib
identity=IDENTITY;source=SOURCE;root=Path(identity['report']);files={}
for name,sha in source['files'].items():
 path=Path(source['root'])/name
 assert hashlib.sha256(path.read_bytes()).hexdigest()==sha['sha256']
lines=(root/identity['runner_log']).read_text().splitlines()
print(json.dumps(dict(lines=[line for line in lines if 'Pending tasks on 'not in line],workers=[line.split('Server list: ',1)[1].split(', ')for line in lines if 'Server list: 'in line])))
'''.replace('IDENTITY', repr(identity)).replace('SOURCE', repr(source))
    runner = remote('node029', code)
    assert len(runner['workers']) == 1
    attempt = completed_attempt(runner['lines'], point)
    workers = runner['workers'][0]
    assert attempt['host'] in workers
    with ThreadPoolExecutor(max_workers=len(workers)) as pool:
        scans = list(pool.map(lambda host: remote(host, scan_code(identity['report'], point)), workers))
    if any(scan['matches'] or scan['unreadable_runner_processes'] for scan in scans):
        return dict(ready=False, point=point, reason='Point process still live or process inspection incomplete', scans=scans, database_read=False)
    code = r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json,re
identity=IDENTITY;point=POINT;seed=SEED;root=Path(identity['report']);folder=root/point;text=(folder/'simulator_out.txt').read_text()
assert all(marker in text for marker in ['Difftest enabled','Warmup finished','EXCEEDING CYCLE/INSTR LIMIT','saving memdb to','Host time spent:'])
assert not any(marker in text for marker in ['HIT CRITICAL','HIT BAD TRAP','ABORT'])
assert re.search(r'Seed='+str(seed)+r'\b',text)
for name,value in [('enableTea0',1),('enableDip0',1),('teaSamplePeriod0',1000),('dipSamplePeriod0',1000)]:assert re.search(r'^'+name+' = '+str(value)+'$',text,re.M)
assert 'The reference model is '+str(root/'riscv64-nemu-interpreter-so')in text
assert 'saving memdb to '+str(folder/'simulator.db')in text
def sha(path):
 with path.open('rb')as stream:return hashlib.file_digest(stream,'sha256').hexdigest()
artifacts=dict(emu=root/'emu-gsim',reference=root/'riscv64-nemu-interpreter-so',cst=Path(CST))
files={name:dict(bytes=(folder/name).stat().st_size,sha256=sha(folder/name))for name in ['simulator_out.txt','simulator_err.txt','simulator.db']}
print(json.dumps(dict(utc=datetime.now(timezone.utc).isoformat(),files=files,artifact_sha256={name:sha(path)for name,path in artifacts.items()},stdout_complete=True,database_read=False)))
'''.replace('IDENTITY', repr(identity)).replace('POINT', repr(point)).replace('SEED', repr(seeds[point])).replace('CST', repr('/nfs/home/wujiabin/work/xs-env/rtl-dip-ci-inputs-20261103/tea-dip-period1000.cst'))
    snapshot = remote(attempt['host'], code)
    assert snapshot['artifact_sha256'] == dict(emu=identity['emu_sha256'], reference=identity['reference_sha256'], cst=identity['cst_sha256'])
    receipt = dict(ready=True, utc=snapshot['utc'], run_id=run['id'], source=identity['source'], point=point, host=attempt['host'],
                   closure_method=METHOD, original_process_identity_available=False, original_pid=None, original_start_ticks=None,
                   original_process_live=False, runner_success_lines=[attempt['succeeded']],
                   runner_started_line=attempt['started'], runner_source_sha256=source_files,
                   worker_absence_checks=scans, completed_stdout_verified=True,
                   runner_poll_validation_sha256=digest(poll_path),
                   files=snapshot['files'], artifact_sha256=snapshot['artifact_sha256'],
                   collector_sha256=digest(Path(__file__)),
                   scope='Runner poll returned0 for the sole logged attempt; all auto-selected workers have no process with this exact DB path and no unreadable runner process. Complete stdout checked before raw hashing. Historical original PID/startticks remain unknown; this is an alternative closure proof, not a reconstructed live observation.')
    if check_only:
        return dict(ready=True, files_copied=False, receipt=receipt, database_read=False)
    assert not (raw / 'results' / point).exists()
    for name in ['archives', 'results', 'closed-receipts']:
        (raw / name).mkdir(parents=True, exist_ok=True)
    names = [point + '/' + name for name in receipt['files']]
    archive = raw / 'archives' / ('runner-exit-' + point + '.tar.gz')
    command = 'tar -C ' + shlex.quote(identity['report']) + ' -czf - ' + ' '.join(map(shlex.quote, names))
    with archive.open('wb') as stream:
        subprocess.run(['proxychains', '-q', 'ssh', attempt['host'], command], stdout=stream, check=True, timeout=60)
    with tarfile.open(archive) as package:
        assert {row.name for row in package.getmembers()} == set(names)
        package.extractall(raw / 'results', filter='data')
    manifest_path = raw / 'manifest.json'
    manifest = json.loads(manifest_path.read_text())
    for name, row in receipt['files'].items():
        path = raw / 'results' / point / name
        assert path.stat().st_size == row['bytes'] and digest(path) == row['sha256']
        manifest['results/' + point + '/' + name] = row
    manifest_path.write_text(json.dumps(manifest, indent=2) + '\n')
    receipt_path.write_text(json.dumps(receipt, indent=2) + '\n')
    return dict(ready=True, point=point, files_copied=True, original_process_identity_available=False, database_read=False)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--point', required=True)
    parser.add_argument('--check-only', action='store_true')
    args = parser.parse_args()
    print(json.dumps(collect(args.report_dir.resolve(), args.point, args.check_only), indent=2))


if __name__ == '__main__':
    main()
