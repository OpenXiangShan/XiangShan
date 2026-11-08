"""Follow one original BPU observer through independent closure collection and event audit."""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import json
import os
import subprocess
import sys
import time

from tea_dip_query_endpoint_evidence import digest
from tea_dip_wb_protocol_micro import write


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ['launch', 'control-dir', 'output-dir', 'audit', 'state-dir']:
        parser.add_argument('--' + name, type=Path, required=True)
    args = parser.parse_args()
    root = args.state_dir.resolve()
    root.mkdir(parents=True, exist_ok=True)
    assert not (root / 'state.json').exists(), 'Follow the original watcher'
    tools = Path(__file__).resolve().parent
    frozen = {tools / name: digest(tools / name) for name in
              ['tea_dip_bpu_commit_collect.py', 'tea_dip_bpu_commit_trace.py', 'tea_dip_query_endpoint_evidence.py',
               'tea_dip_bpu_native_cohort.py', 'native_perf.py']}
    launch_sha = digest(args.launch)
    launch = json.loads(args.launch.read_text())
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 phase='observing', attempts=0, launch_sha256=launch_sha,
                 tool_sha256={str(path): value for path, value in frozen.items()})

    def save(**values):
        state.update(values, utc=datetime.now(timezone.utc).isoformat())
        write(root / 'state.json', state)

    save()
    try:
        while True:
            assert digest(args.launch) == launch_sha
            assert all(digest(path) == value for path, value in frozen.items()), 'Preserve the running watcher tools'
            if args.audit.exists():
                audit = json.loads(args.audit.read_text())
                assert audit['source'] == launch['spec']['source']
                assert audit['closure_receipt_sha256'] == digest(args.output_dir / 'closure-receipt.json')
                save(phase='complete' if audit['passed'] else 'failed', event_audit_passed=audit['passed'])
                return
            attempt = state['attempts'] + 1
            command = [sys.executable, str(tools / 'tea_dip_bpu_commit_collect.py'), '--launch', str(args.launch),
                       '--output-dir', str(args.output_dir)]
            reply = subprocess.run(command, capture_output=True, text=True)
            (root / f'attempt-{attempt:04d}.stdout').write_text(reply.stdout)
            (root / f'attempt-{attempt:04d}.stderr').write_text(reply.stderr)
            save(attempts=attempt, collect_returncode=reply.returncode)
            if reply.returncode:
                assert not args.output_dir.exists(), 'Inspect partial collection; preserve its original files'
                save(phase='observation_error')
            else:
                status = json.loads(reply.stdout)
                if status['ready']:
                    save(phase='analyzing')
                    command = [sys.executable, str(tools / 'tea_dip_bpu_commit_trace.py'), '--run-dir', str(args.output_dir),
                               '--control-dir', str(args.control_dir), '--output', str(args.audit)]
                    reply = subprocess.run(command, capture_output=True, text=True)
                    (root / 'analysis.stdout').write_text(reply.stdout)
                    (root / 'analysis.stderr').write_text(reply.stderr)
                    assert reply.returncode == 0, 'Inspect the closed event audit failure'
                    continue
                save(phase='observing', last_status=status)
            time.sleep(60)
    except BaseException as error:
        save(phase='failed', error=repr(error))
        raise


if __name__ == '__main__':
    main()
