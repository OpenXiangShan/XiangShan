"""Observe one existing CI run and archive closed points, recording actual seeds."""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import json
import os
import subprocess
import sys
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--ci-evidence', required=True)
    parser.add_argument('--watch', action='store_true')
    args = parser.parse_args()
    root = args.report_dir.resolve()
    evidence = root / 'evidence' / args.ci_evidence
    output = evidence / 'auto-follow'
    output.mkdir(exist_ok=True)
    identity = json.loads((evidence / 'archive-identity.json').read_text())
    scripts = Path(__file__).resolve().parent
    tools = [Path(__file__), scripts / 'tea_dip_ci_run_runtime.py', scripts / 'tea_dip_ci_run_collect.py']
    digest = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
    frozen = {str(p): digest(p) for p in tools}
    pid = os.getpid()
    ticks = Path(f'/proc/{pid}/stat').read_text().rsplit(')', 1)[1].split()[19]
    iteration = 0
    while True:
        iteration += 1
        state = dict(pid=pid, start_ticks=ticks, run_id=identity['run_id'], source=identity['source'],
                     utc=datetime.now(timezone.utc).isoformat(), iteration=iteration,
                     tool_sha256=frozen, phase='observing')
        try:
            assert all(digest(Path(p)) == value for p, value in frozen.items()), 'Live observer sources changed'
            code = '''import json,shutil,subprocess
run_id=RUN_ID
exe=shutil.which('gh')
def get(path):
 p=subprocess.run(['proxychains','-q',exe,'api','repos/OpenXiangShan/XiangShan/'+path],capture_output=True,text=True,check=True)
 return json.loads(p.stdout)
print(json.dumps({'run':get('actions/runs/'+str(run_id)), 'jobs':get('actions/runs/'+str(run_id)+'/jobs')}))
'''.replace('RUN_ID', str(identity['run_id']))
            reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o',
                                    'ConnectTimeout=15', 'node032', 'python3', '-'],
                                   input=code, text=True, capture_output=True, timeout=50, check=True)
            official = json.loads(reply.stdout)
            run = official['run']
            assert run['id'] == identity['run_id'] and run['head_sha'] == identity['source']
            state.update(ci_status=run['status'], ci_conclusion=run['conclusion'])
            with (evidence / 'official-events.jsonl').open('a') as stream:
                stream.write(json.dumps(dict(utc=state['utc'], **official)) + '\n')
            for name, data in [('run.json', run), ('jobs.json', official['jobs'])]:
                (evidence / name).write_text(json.dumps(data, indent=2) + '\n')
            if run['status'] != 'completed':
                subprocess.run([sys.executable, str(scripts / 'tea_dip_ci_run_runtime.py'),
                                '--report-dir', str(root), '--ci-evidence', args.ci_evidence],
                               check=True, timeout=180)
            current = json.loads((evidence / 'archive-identity.json').read_text())
            if current['emu_sha256'] is not None and current['runner_log'] is not None:
                subprocess.run([sys.executable, str(scripts / 'tea_dip_ci_run_collect.py'),
                                '--report-dir', str(root), '--ci-evidence', args.ci_evidence],
                               check=True)
                collection = json.loads((root / args.ci_evidence / 'collection-status.json').read_text())
                state.update(collected_points=len(collection['already_collected']) + len(collection['collected']),
                             pending_points=collection['pending'])
            if run['status'] == 'completed':
                state['phase'] = 'terminal'
        except (OSError, ValueError, AssertionError, subprocess.SubprocessError) as error:
            state.update(phase='observation_error', error=str(error))
        state['updated_utc'] = datetime.now(timezone.utc).isoformat()
        (output / 'state.json').write_text(json.dumps(state, indent=2) + '\n')
        with (output / 'events.jsonl').open('a') as stream:
            stream.write(json.dumps(state) + '\n')
        print(json.dumps({k: v for k, v in state.items() if k != 'tool_sha256'}), flush=True)
        if not args.watch or state['phase'] == 'terminal':
            break
        time.sleep(60)


if __name__ == '__main__':
    main()
