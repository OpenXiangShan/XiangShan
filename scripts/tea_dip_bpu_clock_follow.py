"""Collect and audit one original shared-cycle observer without restarting its simulation."""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import json
import os
import subprocess
import sys
import time

from tea_dip_bpu_clock_trace import analyze
from tea_dip_query_endpoint_evidence import digest
from tea_dip_wb_protocol_micro import write


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ['launch','control-dir','prior-observer-dir','output-dir','audit','state-dir']:
        parser.add_argument('--'+name,type=Path,required=True)
    parser.add_argument('--retired-edges',type=int,nargs='+',required=True)
    args = parser.parse_args()
    root = args.state_dir.resolve()
    root.mkdir(parents=True,exist_ok=True)
    assert not (root/'state.json').exists() and not args.audit.exists(), 'Follow the original watcher'
    tools = Path(__file__).resolve().parent
    frozen = {tools/n:digest(tools/n) for n in ['tea_dip_bpu_commit_collect.py','tea_dip_bpu_clock_trace.py',
              'tea_dip_bpu_commit_trace.py','tea_dip_query_endpoint_evidence.py','tea_dip_bpu_native_cohort.py','native_perf.py']}
    launch_sha = digest(args.launch)
    state = dict(pid=os.getpid(),start_ticks=Path('/proc/self/stat').read_text().rsplit(')',1)[1].split()[19],
                 phase='observing',attempts=0,launch_sha256=launch_sha,tool_sha256={str(p):v for p,v in frozen.items()})
    def save(**values):
        state.update(values,utc=datetime.now(timezone.utc).isoformat());write(root/'state.json',state)
    save()
    try:
        while True:
            assert digest(args.launch)==launch_sha and all(digest(p)==v for p,v in frozen.items())
            attempt = state['attempts']+1
            command = [sys.executable,str(tools/'tea_dip_bpu_commit_collect.py'),'--launch',str(args.launch),'--output-dir',str(args.output_dir)]
            reply = subprocess.run(command,capture_output=True,text=True)
            (root/f'attempt-{attempt:04d}.stdout').write_text(reply.stdout)
            (root/f'attempt-{attempt:04d}.stderr').write_text(reply.stderr)
            save(attempts=attempt,collect_returncode=reply.returncode)
            if reply.returncode:
                assert not args.output_dir.exists(), 'Inspect partial collection; preserve its original files'
                save(phase='observation_error')
            else:
                status = json.loads(reply.stdout)
                if status['ready']:
                    save(phase='analyzing')
                    audit = analyze(args.output_dir.resolve(),args.control_dir.resolve(),args.prior_observer_dir.resolve(),args.retired_edges)
                    write(args.audit,audit);save(phase='complete',event_audit_passed=audit['passed'])
                    return
                save(phase='observing',last_status=status)
                if status.get('phase')=='failed' and status.get('controller_live') is False:
                    save(phase='failed',error='Original controller terminated before a guest result; inspect its recorded failure')
                    return
            time.sleep(60)
    except BaseException as error:
        save(phase='failed',error=repr(error));raise


if __name__=='__main__':
    main()
