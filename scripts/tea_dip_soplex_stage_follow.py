"""Collect each original Soplex12322 stage side as soon as its guest closes."""
from pathlib import Path
from datetime import datetime, timezone
import argparse
import fcntl
import json
import os
import sys
import time
from tea_dip_retry_follow import attempt


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--output-dir', required=True, type=Path)
    parser.add_argument('--once', action='store_true')
    args = parser.parse_args()
    root, out = args.report_dir.resolve(), args.output_dir.resolve()
    out.mkdir(parents=True, exist_ok=True)
    point = 'soplex_pds-50_12322_0.090454'
    script = Path(__file__).with_name('tea_dip_fdiv_ready_batch_evidence.py')
    jobs = [dict(name=variant, family='context', target=root/'fdiv-turnover-soplex12322-stage-20261108-points'/variant/point/'analysis/evidence.json',
                 command=[sys.executable, str(script), '--report-dir', str(root), '--batch', 'soplex12322', '--turnover', '--variant', variant, '--point', point, '--action', 'collect'])
            for variant in ['control', 'candidate']]
    with (out/'follow.lock').open('w') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19], jobs={},
                     scope='Only the original39a/8f8 Soplex12322 pair. Independent close/archive/analyze per side; no simulation launch, restart, pair gain, CI action or report change. Collected means archived, not validation passed.')
        while True:
            for job in jobs:
                if state['jobs'].get(job['name'], {}).get('status') not in ['collected', 'preserved']:
                    state['jobs'][job['name']] = attempt(job, root)
            state.update(utc=datetime.now(timezone.utc).isoformat(), phase='complete' if all(row['status'] in ['collected', 'preserved'] for row in state['jobs'].values()) else 'observing')
            temporary = out/'state.tmp'
            temporary.write_text(json.dumps(state, indent=2) + '\n')
            temporary.replace(out/'state.json')
            print(json.dumps(state), flush=True)
            if args.once or state['phase'] == 'complete':
                return
            time.sleep(60)


if __name__ == '__main__':
    main()
