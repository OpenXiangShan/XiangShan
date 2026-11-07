"""Sample host PMU and scheduler counters for one original simulator process."""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import json
import os
import socket
import subprocess
import time

EVENTS = ['task-clock', 'cycles', 'instructions', 'branches', 'branch-misses', 'cache-references', 'cache-misses']


def snapshot(pid, start_ticks):
    begin = time.monotonic_ns()
    root = Path('/proc', str(pid))
    stat = (root / 'stat').read_text().rsplit(')', 1)[1].split()
    assert stat[19] == start_ticks, 'Original process identity changed'
    run, wait, slices = map(int, (root / 'schedstat').read_text().split())
    return dict(begin_monotonic_ns=begin, end_monotonic_ns=time.monotonic_ns(),
                cpu_ticks=int(stat[11]) + int(stat[12]), minor_faults=int(stat[7]), major_faults=int(stat[9]),
                sched_run_ns=run, sched_wait_ns=wait, sched_slices=slices)


def parse_perf(text):
    result = {}
    for line in text.splitlines():
        fields = line.split(',')
        if len(fields) < 5 or fields[2] not in EVENTS:
            continue
        value, unit, event, runtime, percent = fields[:5]
        assert event not in result, 'Expected one interval per event'
        result[event] = dict(value=None if value.startswith('<') else float(value), unit=unit,
                             event_runtime_ns=int(runtime), running_percent=float(percent), raw_value=value)
    return result


def metrics(events):
    def ratio(numerator, denominator):
        a = events.get(numerator, {}).get('value')
        b = events.get(denominator, {}).get('value')
        return a / b if a is not None and b is not None and b > 0 else None
    clock = events.get('task-clock')
    if clock and clock['value'] is not None:
        assert clock['unit'] == 'msec', 'Unexpected host task-clock unit'
    rate = ratio('cycles', 'task-clock')
    return dict(host_ipc=ratio('instructions', 'cycles'),
                effective_host_GHz=rate / 1e6 if rate is not None else None,
                host_branch_miss_fraction=ratio('branch-misses', 'branches'),
                generic_host_cache_miss_fraction=ratio('cache-misses', 'cache-references'))


def sample(pid, start_ticks, seconds):
    before = snapshot(pid, start_ticks)
    command_line = (Path('/proc', str(pid)) / 'cmdline').read_bytes().decode().split('\0')[:-1]
    command = ['perf', 'stat', '-x,', '-p', str(pid), '-e', ','.join(EVENTS), '--', 'sleep', str(seconds)]
    result = subprocess.run(command, text=True, capture_output=True, env={**os.environ, 'LC_ALL': 'C'})
    after = snapshot(pid, start_ticks)
    counters = parse_perf(result.stderr)
    delta = {key: after[key] - before[key] for key in ['cpu_ticks', 'minor_faults', 'major_faults', 'sched_run_ns', 'sched_wait_ns', 'sched_slices']}
    return dict(utc=datetime.now(timezone.utc).isoformat(), host=socket.gethostname(), pid=pid, start_ticks=start_ticks,
                original_process_live_before_and_after=True, command_line=command_line, affinity=sorted(os.sched_getaffinity(pid)),
                clock_ticks_per_second=os.sysconf('SC_CLK_TCK'), before=before, after=after, delta=delta,
                perf_command=command, perf_returncode=result.returncode, perf_stdout=result.stdout, perf_stderr=result.stderr,
                events=counters, metrics=metrics(counters) if result.returncode == 0 else None,
                tool_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(), database_read=False,
                scope='One live original-process host interval only. PMU events and cache definitions depend on the host; values are not guest counters. Raw event running percentages preserve multiplexing evidence. Scheduler snapshots bracket the PMU command and are not atomic with it. Unsupported/missing/zero-denominator metrics remain null. Different guest phases are not matched cost comparisons, isolated diagnostic overhead or historical frequency/IPC evidence.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('pid', type=int)
    parser.add_argument('start_ticks')
    parser.add_argument('--seconds', type=float, default=5)
    args = parser.parse_args()
    print(json.dumps(sample(args.pid, args.start_ticks, args.seconds), indent=2))


if __name__ == '__main__':
    main()
