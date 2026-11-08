"""Measure one complete simulator process, with a bracketing host PMU attachment."""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import json
import os
import signal
import socket
import subprocess
import time
from tea_dip_host_counters import EVENTS, metrics, parse_perf, snapshot


def measure(command, output):
    output = output.resolve()
    output.mkdir(parents=True)
    started = time.monotonic_ns()
    record = dict(utc=datetime.now(timezone.utc).isoformat(), host=socket.gethostname(), command=command,
                  tool_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                  host_counter_helper_sha256=hashlib.sha256(Path(__file__).with_name('tea_dip_host_counters.py').read_bytes()).hexdigest())
    with (output / 'stdout.txt').open('w') as stdout, (output / 'stderr.txt').open('w') as stderr:
        child = subprocess.Popen(command, cwd=output, stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr)
        ticks = Path('/proc', str(child.pid), 'stat').read_text().rsplit(')', 1)[1].split()[19]
        record['process'] = dict(pid=child.pid, start_ticks=ticks)
        (output / 'process.json').write_text(json.dumps(record, indent=2) + '\n')
        first = last = snapshot(child.pid, ticks)
        perf_command = ['perf', 'stat', '-x,', '-p', str(child.pid), '-e', ','.join(EVENTS)]
        perf_started = time.monotonic_ns()
        perf = subprocess.Popen(perf_command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
                                env={**os.environ, 'LC_ALL': 'C'})
        while True:
            pid, status, usage = os.wait4(child.pid, os.WNOHANG)
            if pid:
                child.returncode = os.waitstatus_to_exitcode(status)
                break
            try:
                last = snapshot(child.pid, ticks)
            except FileNotFoundError:
                pass  # The original child can exit between wait4 and its /proc snapshot.
            time.sleep(0.2)
        ended = time.monotonic_ns()
        stop_requested = perf.poll() is None
        if stop_requested:
            perf.send_signal(signal.SIGINT)
        perf_stdout, perf_stderr = perf.communicate()
    (output / 'perf.stdout').write_text(perf_stdout)
    (output / 'perf.stderr').write_text(perf_stderr)
    wall = (ended - started) / 1e9
    cpu = usage.ru_utime + usage.ru_stime
    events = parse_perf(perf_stderr)
    record.update(original_process_closed=True, exitcode=child.returncode, wall_seconds=wall,
                  poll_interval_seconds=0.2, process_cpu_seconds=cpu, cpu_seconds_per_wall_second=cpu / wall,
                  rusage={name: getattr(usage, 'ru_' + name) for name in
                          ['utime', 'stime', 'minflt', 'majflt', 'nvcsw', 'nivcsw', 'inblock', 'oublock', 'maxrss']},
                  scheduler_interval=dict(first=first, last=last,
                      delta={key: last[key] - first[key] for key in
                             ['cpu_ticks', 'minor_faults', 'major_faults', 'sched_run_ns', 'sched_wait_ns', 'sched_slices']}),
                  perf=dict(command=perf_command, pid=perf.pid, returncode=perf.returncode,
                            stop_requested=stop_requested, launch_delay_seconds=(perf_started - started) / 1e9, events=events,
                            metrics=metrics(events) if perf.returncode == 0 or
                                (stop_requested and perf.returncode == -signal.SIGINT) else None),
                  host_platform=dict(cpu_model=next(line.split(':', 1)[1].strip() for line in
                      Path('/proc/cpuinfo').read_text().splitlines() if line.startswith('model name')),
                      perf_version=subprocess.check_output(['perf', '--version'], text=True).strip()),
                  database_read=False,
                  scope='wait4 reports lifetime CPU, faults and context switches of this child, including descendants it reaped. Wall includes launch and polling delay after exit. Scheduler snapshots cover only their recorded interval. Attached PMU excludes startup before attachment and has asynchronous shutdown; raw running percentages preserve coverage and multiplexing. Host IPC/GHz are not guest CPI or isolated tool cost. Missing PMU metrics remain null.')
    (output / 'result.json').write_text(json.dumps(record, indent=2) + '\n')
    return record


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output-dir', required=True, type=Path)
    parser.add_argument('command', nargs=argparse.REMAINDER)
    args = parser.parse_args()
    command = args.command[1:] if args.command[:1] == ['--'] else args.command
    assert command, 'Specify the simulator command after --'
    result = measure(command, args.output_dir)
    print(json.dumps(result, indent=2))
    raise SystemExit(result['exitcode'])


if __name__ == '__main__':
    main()
