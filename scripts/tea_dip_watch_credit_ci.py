"""Keep original CI process observations across analysis sessions without touching simulations."""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import json
import os
import subprocess
import sys
import time


def utc():
    return datetime.now(timezone.utc).isoformat()


def observe_once(root, namespace):
    folder = root / 'evidence' / namespace
    identity = json.loads((folder / 'archive-identity.json').read_text())
    runtime = Path(__file__).with_name('tea_dip_credit_ci_runtime.py')
    command = [sys.executable, str(runtime), '--report-dir', str(root), '--ci-evidence', namespace]
    result = dict(utc=utc(), run_id=identity['run_id'], source=identity['source'], capture_passed=False,
                  ci_terminal=False, runtime_sha256=hashlib.sha256(runtime.read_bytes()).hexdigest())
    try:
        reply = subprocess.run(command, text=True, capture_output=True, timeout=120)
        result.update(runtime_returncode=reply.returncode, runtime_stdout=reply.stdout[-3000:], runtime_stderr=reply.stderr[-3000:])
        if reply.returncode == 0:
            # The validated runtime checks actual controller/guest argv and appends original PID/startticks.
            result.update(capture_passed=True, capture=json.loads(reply.stdout))
    except subprocess.TimeoutExpired as error:
        result.update(runtime_returncode=None, runtime_error=str(error))
    # Check the official run even after a valid process snapshot; cancelled runs may retain remote guests.
    api = 'repos/OpenXiangShan/XiangShan/actions/runs/' + str(identity['run_id'])
    try:
        reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=15', 'node032',
                                'proxychains', '-q', '/nfs/home/wujiabin/.local/bin/gh', 'api', api],
                               text=True, capture_output=True, timeout=55)
        if reply.returncode == 0:
            official = json.loads(reply.stdout)
            assert official['id'] == identity['run_id'] and official['head_sha'] == identity['source']
            result['official'] = {key: official[key] for key in ['id', 'head_sha', 'status', 'conclusion']}
            result['ci_terminal'] = official['status'] == 'completed'
        else:
            result['official_observation_error'] = reply.stderr[-2000:]
    except subprocess.TimeoutExpired as error:
        result['official_observation_error'] = str(error)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--ci-evidence', choices=['ci-credit-div', 'ci-fdiv-credit', 'fdiv-wb-query-paired-ci'], required=True)
    args = parser.parse_args()
    root = args.report_dir.resolve()
    folder = root / 'evidence' / args.ci_evidence
    path = folder / 'process-watch-state.json'
    process = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19])
    state = dict(utc=utc(), **process, ci_evidence=args.ci_evidence, phase='starting', captures=0, observation_failures=0,
                 consumer_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                 scope='Read-only process/stdout observations every60 seconds. Original auto-scheduled CI only; no dispatch, server selection, load/idle sampling, live DB reads, simulation restart or performance inference.')
    while True:
        observation = observe_once(root, args.ci_evidence)
        state.update(utc=utc(), run_id=observation['run_id'], source=observation['source'], last_observation=observation,
                     phase='ci_terminal' if observation['ci_terminal'] else 'observing')
        if observation['capture_passed']:
            state['captures'] += 1
            state['last_success_utc'] = observation['capture']['utc']
        else:
            state['observation_failures'] += 1
        temporary = path.with_suffix('.tmp')
        temporary.write_text(json.dumps(state, indent=2) + '\n')
        temporary.replace(path)
        print(json.dumps(dict(utc=state['utc'], run_id=state['run_id'], phase=state['phase'],
                              captures=state['captures'], observation_failures=state['observation_failures'])), flush=True)
        if observation['ci_terminal']:
            return
        time.sleep(60)


if __name__ == '__main__':
    main()
