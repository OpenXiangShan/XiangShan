"""Wait for an automatically assigned CI runner, then hand off to the existing follower.

Only observe official metadata and the assigned workspace. No workflow dispatch,
worker selection, build, simulation, cancellation or unfinished database access.
"""
from datetime import datetime, timezone
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import time


def remote(host, code):
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o',
                            'ConnectTimeout=15', host, 'python3', '-'],
                           input=code, text=True, capture_output=True, timeout=50, check=True)
    return json.loads(reply.stdout)


def observe(identity):
    code = '''import json,subprocess
result={}
for name,path in [('run','actions/runs/RUN_ID'),('jobs','actions/runs/RUN_ID/jobs')]:
 result[name]=json.loads(subprocess.check_output(['proxychains','-q','gh','api','repos/OpenXiangShan/XiangShan/'+path],text=True))
print(json.dumps(result))
'''.replace('RUN_ID', str(identity['run_id']))
    official = remote('node032', code)
    run, jobs = official['run'], official['jobs']['jobs']
    assert run['id'] == identity['run_id'] and run['head_sha'] == identity['source']
    result = dict(ci_status=run['status'], ci_conclusion=run['conclusion'], phase='waiting_runner')
    if run['status'] == 'completed':
        return dict(result, phase='terminal')
    job, = jobs
    assert job['run_id'] == run['id']
    runner = job['runner_name']
    if not runner:
        return result
    host = runner.removeprefix('xs-perf-').rsplit('-', 1)[0]
    workspace = '/nfs/home/cirunner/ci-runner-xs/' + runner + '/_work/XiangShan/XiangShan'
    code = '''import json,subprocess
workspace=WORKSPACE
reply=subprocess.run(['git','-c','safe.directory='+workspace,'-C',workspace,'rev-parse','HEAD'],text=True,capture_output=True)
print(json.dumps(dict(source=reply.stdout.strip() if reply.returncode==0 else None,git_returncode=reply.returncode)))
'''.replace('WORKSPACE', repr(workspace))
    checkout = remote(host, code)
    result.update(runner_name=runner, workspace=workspace, checkout=checkout,
                  phase='ready' if checkout['source'] == identity['source'] else 'waiting_checkout')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--ci-evidence', required=True)
    parser.add_argument('--probe', action='store_true', help='Read one real state without writing or starting a follower')
    args = parser.parse_args()
    root = args.report_dir.resolve()
    evidence = root / 'evidence' / args.ci_evidence
    identity = json.loads((evidence / 'archive-identity.json').read_text())
    if args.probe:
        print(json.dumps(observe(identity), indent=2))
        return
    pid = os.getpid()
    ticks = Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19]
    source = hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
    while True:
        state = dict(pid=pid, start_ticks=ticks, run_id=identity['run_id'], source=identity['source'],
                     utc=datetime.now(timezone.utc).isoformat(), tool_sha256=source)
        try:
            state.update(observe(identity))
        except (OSError, ValueError, AssertionError, subprocess.SubprocessError) as error:
            state.update(phase='observation_error', error=str(error))
        (evidence / 'queue-state.json').write_text(json.dumps(state, indent=2) + '\n')
        print(json.dumps(state), flush=True)
        if state['phase'] == 'ready':
            identity['workspace'] = state['workspace']
            (evidence / 'archive-identity.json').write_text(json.dumps(identity, indent=2) + '\n')
            os.execv(sys.executable, [sys.executable, str(Path(__file__).with_name('tea_dip_ci_run_follow.py')),
                                    '--report-dir', str(root), '--ci-evidence', args.ci_evidence, '--watch'])
        if state['phase'] == 'terminal':
            return
        time.sleep(60)


if __name__ == '__main__':
    main()
