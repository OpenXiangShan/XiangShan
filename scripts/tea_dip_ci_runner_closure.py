"""Certify a closed CI point through its audited foreground runner completion.

This is an explicit alternative to observed-PID closure. It never invents a
PID/start observation and leaves the existing observed-PID collector unchanged.
Only the archived perf_trigger protocol audited for run43120494741 is accepted.
"""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import json
import re

from tea_dip_query_endpoint_evidence import digest
from tea_dip_wb_protocol_micro import write


PROTOCOL = {
    'main.py': '3385a5ae2607d6c50fd1c055540612618346bcacc50a8fa922932f7f5a4e1411',
    'modules/server.py': '735bada5caba714db2928505de9749e62d83d53b2727e70e0927f9351367ca24',
    'modules/gcpt.py': 'a5046ff985333dc6f008506dd1de1491bd9f0b90ade88b7485bf7a592c81aec7',
}


def runner_completion(identity, run, job, point, runner_text):
    assert identity['run_id'] == run['id'] == job['run_id']
    assert identity['source'] == run['head_sha'] == job['head_sha']
    assert run['run_attempt'] == job['run_attempt']
    assert identity['protocol_sha256'] == PROTOCOL, 'Runner protocol has not been audited'
    for state in [run, job]:
        if state['status'] not in ['in_progress', 'completed'] or (
                state['status'] == 'completed' and state['conclusion'] != 'success'):
            return dict(ready=False, point=point, reason='CI run or job is not eligible', database_read=False)
    pattern = re.compile(r'\] (?:Started (?P<started>' + re.escape(point) + r') on (?P<start_host>\S+)|'
                         + re.escape(point) + r' (?P<outcome>succeeded|failed)\s+.*?on (?P<host>\S+))')
    events = [(line, match) for line in runner_text.splitlines() if (match := pattern.search(line))]
    if not events or events[-1][1]['outcome'] != 'succeeded':
        return dict(ready=False, point=point, reason='Latest point attempt has no runner success', database_read=False)
    starts = [(line, match) for line, match in events[:-1] if match['started']]
    assert starts, 'Runner start of this point was not observed in the archived log'
    started, start = starts[-1]
    succeeded, finish = events[-1]
    assert start['start_host'] == finish['host']
    return dict(ready=True, point=point, host=finish['host'], run_id=identity['run_id'],
                source=identity['source'], run_attempt=run['run_attempt'],
                closure_method='audited_foreground_runner_success', original_process_observed=False,
                runner_start_line=started, runner_success_lines=[succeeded],
                runner_log_sha256=hashlib.sha256(runner_text.encode()).hexdigest(),
                run_status_at_collection=run['status'], job_status_at_collection=job['status'])


def closed_receipt(identity, run, job, point, runner_text):
    receipt = runner_completion(identity, run, job, point, runner_text)
    if not receipt['ready']:
        return receipt
    protocol = Path(identity['protocol_dir'])
    for name, expected in PROTOCOL.items():
        assert digest(protocol / name) == expected, name
    root = Path(identity['report'])
    assert list(root.glob('runner*.log')) == [root / identity['runner_log']], 'Ambiguous runner session'
    for name, expected in [('emu-gsim', identity['emu_sha256']),
                           ('riscv64-nemu-interpreter-so', identity['reference_sha256'])]:
        assert digest(root / name) == expected, name
    assert digest(Path(identity['cst_file'])) == identity['cst_sha256']
    folder = root / point
    stdout = (folder / 'simulator_out.txt').read_text()
    assert 'Difftest enabled' in stdout and 'Warmup finished' in stdout
    assert 'EXCEEDING CYCLE/INSTR LIMIT' in stdout
    assert not any(marker in stdout for marker in ['ABORT at pc', 'FATAL:', 'SOME SIGNAL STOPS THE PROGRAM'])
    flags = {name: bool(re.search(r'^' + name + r'\s*=\s*' + str(value) + r'\s*$', stdout, re.M))
             for name, value in [('enableTea0', 1), ('enableDip0', 1),
                                 ('teaSamplePeriod0', 1000), ('dipSamplePeriod0', 1000)]}
    assert all(flags.values()), flags
    seed, = re.findall(r'Using seed = (\d+)', stdout)
    files = {}
    for name in ['simulator_out.txt', 'simulator_err.txt', 'simulator.db']:
        path = folder / name
        before = path.stat()
        fingerprint = digest(path)
        after = path.stat()
        assert (before.st_ino, before.st_size, before.st_mtime_ns) == (after.st_ino, after.st_size, after.st_mtime_ns)
        files[name] = dict(bytes=after.st_size, sha256=fingerprint)
    return dict(receipt, utc=datetime.now(timezone.utc).isoformat(), original_process_live=False,
                actual_seed=int(seed), sampler_flags=flags, difftest_passed=True, files=files,
                artifact_sha256=dict(emu=identity['emu_sha256'], reference=identity['reference_sha256'],
                                     cst=identity['cst_sha256']), protocol_sha256=PROTOCOL,
                tool_sha256=digest(Path(__file__)), database_opened=False,
                scope='Closure is proved by the audited foreground SSH/emu runner returning zero before '
                      'its succeeded log event, together with the pinned official run/job and artifact hashes. '
                      'No original PID/start observation exists or is claimed. Only this closed point is '
                      'fingerprinted; pending points return before any guest-file access. The whole CI may '
                      'still be running. This receipt is not whole-CI success, complete coverage or a suite score.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--identity', type=Path, required=True)
    parser.add_argument('--run', type=Path, required=True)
    parser.add_argument('--jobs', type=Path, required=True)
    parser.add_argument('--runner-log', type=Path, required=True)
    parser.add_argument('--point', required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    jobs = json.loads(args.jobs.read_text())['jobs']
    assert len(jobs) == 1
    assert not args.output.exists()
    write(args.output, closed_receipt(json.loads(args.identity.read_text()), json.loads(args.run.read_text()),
                                     jobs[0], args.point, args.runner_log.read_text()))


if __name__ == '__main__':
    main()
