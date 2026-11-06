"""Follow only the original six formal guests through genuine closure and offline audit."""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import json
import os
import subprocess
import time

ORIGINAL = {
    'control_endpoint': {
        'gromacs_105_0.206090': 1784084,
        'lbm_6382_0.375486': 1784085,
        'sphinx3_157921_0.136728': 1784087,
    },
    'query_endpoint': {
        'gromacs_105_0.206090': 1784088,
        'lbm_6382_0.375486': 1784086,
        'sphinx3_157921_0.136728': 1784089,
    },
}


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def write(path, value):
    temporary = path.with_suffix('.tmp')
    temporary.write_text(json.dumps(value, indent=2) + '\n')
    temporary.replace(path)


def remote():
    code = r'''from pathlib import Path
from datetime import datetime,timezone
import json
root=Path('/nfs/home/wujiabin/work/xs-env/rtl-dip-experiments-20261103/wb-query-endpoint-formal')
state=json.loads((root/'run-state.json').read_text())
assert state['pid']==1783927 and state['start_ticks']=='62152493'
original=ORIGINAL
def observe(row):
 p=Path('/proc',str(row['pid']),'stat')
 if not p.exists():return dict(live=False,cpu_ticks=None)
 stat=p.read_text().rsplit(')',1)[1].split()
 if stat[19]!=row['start_ticks']:return dict(live=False,cpu_ticks=None)
 return dict(live=True,cpu_ticks=int(stat[11])+int(stat[12]))
rows=[]
for variant,points in original.items():
 for point,pid in points.items():
  folder=root/variant/point
  process=json.loads((folder/'process.json').read_text())
  assert process['pid']==pid and process['start_ticks']=='62152522'
  result_path=folder/'result.json'
  rows.append(dict(variant=variant,point=point,pid=pid,start_ticks=process['start_ticks'],**observe(process),result=json.loads(result_path.read_text()) if result_path.exists() else None))
print(json.dumps(dict(utc=datetime.now(timezone.utc).isoformat(),controller=state,controller_process=observe(state),guests=rows,database_read=False)))
'''.replace('ORIGINAL', repr(ORIGINAL))
    result = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=15', 'node032', 'python3', '-'],
                            input=code, text=True, capture_output=True, timeout=55, check=True)
    return json.loads(result.stdout)


def run_once(name, command, evidence):
    with (evidence / (name + '.stdout')).open('w') as stdout, (evidence / (name + '.stderr')).open('w') as stderr:
        result = subprocess.run(command, stdout=stdout, stderr=stderr, timeout=900)
    return result.returncode


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--archive-script', required=True, type=Path)
    args = parser.parse_args()
    root = args.report_dir.resolve()
    archive = args.archive_script.resolve()
    evidence = root / 'evidence/query-endpoint-validation'
    consumer = evidence / 'tea_dip_query_endpoint_formal_evidence.py'
    source = json.loads((evidence / 'formal-consumer-source.json').read_text())
    assert source['head'] == '4b054ff2dcb6cba6d23bfe15179533bad83e18ad'
    assert digest(consumer) == source['files']['scripts/tea_dip_query_endpoint_formal_evidence.py']['sha256']
    state_path = evidence / 'formal-watch-state.json'
    assert not state_path.exists(), 'Keep following the original observer; do not start another'
    assert not (root / 'wb-query-endpoint-formal').exists()
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 utc=datetime.now(timezone.utc).isoformat(), phase='observing', captures=0,
                 observation_failures=0, original_controller_pid=1783927,
                 original_controller_start_ticks='62152493', consumer_sha256=digest(consumer),
                 archive_script_sha256=digest(archive), source_sha256=digest(Path(__file__)))
    write(state_path, state)
    while True:
        try:
            observation = remote()
        except (subprocess.SubprocessError, OSError, json.JSONDecodeError) as error:
            state.update(utc=datetime.now(timezone.utc).isoformat(), observation_failures=state['observation_failures'] + 1,
                         last_error=repr(error))
            write(state_path, state)
            time.sleep(60)
            continue
        state.update(utc=observation['utc'], captures=state['captures'] + 1, last_observation=observation)
        write(state_path, state)
        original = observation['controller']
        if original['phase'] == 'failed' or (not observation['controller_process']['live'] and original['phase'] != 'complete'):
            state.update(phase='original_failed_or_missing', utc=datetime.now(timezone.utc).isoformat())
            write(state_path, state)
            return
        if original['phase'] == 'complete' and not observation['controller_process']['live']:
            if all(not row['live'] and row['result'] is not None for row in observation['guests']):
                break
        time.sleep(60)
    state.update(phase='archiving', utc=datetime.now(timezone.utc).isoformat())
    write(state_path, state)
    archive_returncode = run_once('formal-watch-archive', ['python3', str(archive)], evidence)
    receipt_path = root / 'wb-query-endpoint-formal/closure-receipt.json'
    if archive_returncode != 0 or not receipt_path.exists():
        state.update(phase='archive_failed', archive_returncode=archive_returncode)
        write(state_path, state)
        return
    state.update(phase='analyzing', closed_receipt_sha256=digest(receipt_path))
    write(state_path, state)
    analysis_returncode = run_once('formal-watch-analysis',
                                   ['python3', str(consumer), '--report-dir', str(root)], evidence)
    result_path = root / 'wb-query-endpoint-formal/analysis/evidence.json'
    if analysis_returncode != 0 or not result_path.exists():
        state.update(phase='analysis_failed', analysis_returncode=analysis_returncode)
        write(state_path, state)
        return
    result = json.loads(result_path.read_text())
    assert not result['pending'] and result['database_read'] and result['completed'] == 3
    state.update(phase='analysis_complete', analysis_returncode=0,
                 analysis_sha256=digest(result_path), all_observation_pairs_neutral=result['all_observation_pairs_neutral'],
                 utc=datetime.now(timezone.utc).isoformat())
    write(state_path, state)


if __name__ == '__main__':
    main()
