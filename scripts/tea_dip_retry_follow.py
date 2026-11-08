"""Harvest original4339 and edba closures; never launch or restart a simulation."""
from pathlib import Path
from datetime import datetime, timezone
import argparse
import fcntl
import hashlib
import json
import os
import subprocess
import sys
import time

POINTS = ['gromacs_105_0.206090', 'lbm_6382_0.375486', 'sphinx3_157921_0.136728']


def jobs(root, tools):
    result = []
    def add(name, target, script, args, family, raw=None):
        result.append(dict(name=name, target=root/target, command=[sys.executable, str(tools/script), '--report-dir', str(root), *args], family=family, raw=raw))
    for point in POINTS:
        add('context:'+point, 'evidence/fdiv-predict-context-20261108/closed-points/'+point+'/analysis/evidence.json', 'tea_dip_fdiv_predict_evidence.py', ['--point', point, '--action', 'collect'], 'context')
    stage = 'fdiv-retry-ready-validation-20261108'
    add('retry:coremark', stage+'-points/coremark/analysis/evidence.json', 'tea_dip_fdiv_ready_evidence.py', ['--retry-ready', '--point', 'coremark', '--collect'], 'retry', 'coremark/result.json')
    add('retry:protocol', 'evidence/fdiv-retry-ready-20261108/protocol-closure/analysis.json', 'tea_dip_early_ready_protocol_evidence.py', ['--retry-ready', '--collect'], 'retry', 'micro-controller.json')
    add('retry:stage', stage+'/analysis/evidence.json', 'tea_dip_fdiv_ready_evidence.py', ['--retry-ready', '--collect'], 'stage')
    for point in POINTS:
        add('retry:'+point, stage+'-points/'+point+'/analysis/evidence.json', 'tea_dip_fdiv_ready_evidence.py', ['--retry-ready', '--point', point, '--collect'], 'retry', point+'/result.json')
    return result


def attempt(job, root):
    target = job['target']
    if target.exists():
        return dict(status='preserved', evidence=str(target.relative_to(root)))
    whole = root/'fdiv-retry-ready-validation-20261108/analysis/evidence.json'
    if job['family'] == 'retry' and whole.exists():
        stage = json.loads(whole.read_text())
        if (stage.get('short_skipped') or stage.get('validation_passed') is False) and not (whole.parents[1]/job['raw']).exists():
            return dict(status='not_launched', reason='Original stage ended without this input', stage_evidence=str(whole.relative_to(root)))
    commands = [job['command']]
    if job['family'] == 'context':
        commands.append([*job['command'][:-1], 'analyze'])
    for command in commands:
        reply = subprocess.run(command, text=True, capture_output=True)
        if reply.returncode:
            return dict(status='error', returncode=reply.returncode, stderr=reply.stderr[-3000:])
        value = json.loads(reply.stdout)
        if value.get('pending') or value.get('ready') is False:
            return dict(status='pending')
    assert target.exists(), 'Successful consumer did not write the expected analysis'
    return dict(status='collected', evidence=str(target.relative_to(root)), evidence_sha256=hashlib.sha256(target.read_bytes()).hexdigest())


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--output-dir', required=True, type=Path)
    parser.add_argument('--once', action='store_true')
    args = parser.parse_args()
    root, out = args.report_dir.resolve(), args.output_dir.resolve()
    out.mkdir(parents=True, exist_ok=True)
    with (out/'follow.lock').open('w') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        tasks = jobs(root, Path(__file__).resolve().parent)
        process = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')',1)[1].split()[19])
        state = dict(**process, phase='observing', jobs={}, scope='Only original4339/edba closed collectors. Existing analyses preserved; no simulation launch, restart, affinity/load change, report republishing, CI action or suite score.')
        while True:
            for job in tasks:
                previous = state['jobs'].get(job['name'], {})
                if previous.get('status') in ['preserved', 'collected', 'not_launched']:
                    continue
                state['jobs'][job['name']] = attempt(job, root)
            statuses = [item['status'] for item in state['jobs'].values()]
            state.update(utc=datetime.now(timezone.utc).isoformat(), phase='complete' if all(s in ['preserved','collected','not_launched'] for s in statuses) else 'observing')
            temporary = out/'state.tmp'
            temporary.write_text(json.dumps(state, indent=2)+'\n')
            temporary.replace(out/'state.json')
            print(json.dumps(dict(utc=state['utc'], phase=state['phase'], statuses={name:value['status'] for name,value in state['jobs'].items()})), flush=True)
            if args.once or state['phase'] == 'complete':
                return
            time.sleep(60)


if __name__ == '__main__':
    main()
