"""Gate one FDIV-ready architecture build through CoreMark, FP protocols and three short SPEC windows."""
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path
import argparse
import json
import os
import re
import socket
import subprocess
import time

from tea_dip_wb_protocol_micro import digest, write
from tea_dip_query_endpoint_evidence import old_execution


def completed_guest(folder, build, case):
    folder.mkdir()
    command = case['command'].copy()
    command[6] = build['binary']
    for flag, value in [('-c', str(case['core'])), ('--dramsim3-outdir', str(folder)),
                        ('--db-path', str(folder / 'simulator.db'))]:
        command[command.index(flag) + 1] = value
    write(folder / 'command.json', command)
    began = time.monotonic()
    with (folder / 'simulator_out.txt').open('w') as stdout, (folder / 'simulator_err.txt').open('w') as stderr:
        child = subprocess.Popen(command, cwd=folder, stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr)
        process = dict(pid=child.pid, start_ticks=Path('/proc', str(child.pid), 'stat').read_text().rsplit(')', 1)[1].split()[19],
                       host=socket.gethostname(), core=case['core'])
        write(folder / 'process.json', process)
        write(folder / 'launch-observation.json', dict(process=process, command=command, source=build['source'],
              binary_sha256=build['binary_sha256'], original_process_live=True, utc=datetime.now(timezone.utc).isoformat()))
        _, status, usage = os.wait4(child.pid, 0)
        child.returncode = os.waitstatus_to_exitcode(status)
        rc = child.returncode
    (folder / 'exitcode').write_text(str(rc))
    text = (folder / 'simulator_out.txt').read_text()
    flags = {name: bool(re.search(r'^' + name + ' = ' + str(value) + '$', text, re.M))
             for name, value in [('enableTea0', 1), ('enableDip0', 1), ('teaSamplePeriod0', 1000), ('dipSamplePeriod0', 1000)]}
    counts = re.search(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)', text)
    passed = rc == 0 and 'Difftest enabled' in text and 'EXCEEDING CYCLE/INSTR LIMIT' in text and all(flags.values())
    passed = passed and not any(marker in text for marker in ['ABORT', 'HIT CRITICAL', 'HIT BAD TRAP'])
    if '-W' in command:
        passed = passed and 'Warmup finished' in text
    result = dict(point=case['point'], source=build['source'], binary_sha256=build['binary_sha256'], process=process,
                  original_process_closed=True, exitcode=rc, difftest_passed=passed, sampler_flags=flags,
                  trap_counts=[int(v.replace(',', '')) for v in counts.groups()] if counts else None,
                  host_wall_seconds=time.monotonic() - began,
                  host_rusage={name: getattr(usage, 'ru_' + name) for name in
                               ['utime', 'stime', 'minflt', 'majflt', 'nvcsw', 'nivcsw', 'inblock', 'oublock', 'maxrss']},
                  performance_speedup=None)
    write(folder / 'result.json', result)
    assert passed, result
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir', required=True, type=Path)
    root = parser.parse_args().run_dir.resolve()
    spec = json.loads((root / 'experiment.json').read_text())
    assert socket.gethostname().startswith('node')
    assert not (root / 'run-state.json').exists(), 'Follow the original controller; elapsed time never authorizes restart'
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 host=socket.gethostname(), phase='waiting_build', completed=0)

    def save(**values):
        state.update(values, utc=datetime.now(timezone.utc).isoformat())
        write(root / 'run-state.json', state)

    save()
    try:
        for name, expected in spec['input_hashes'].items():
            assert digest(Path(name)) == expected, name
        receipt = json.loads(Path(spec['control_coremark_receipt']).read_text())
        assert receipt['ready'] and not receipt['original_guest_live'] and not receipt['original_smoke_controller_live']
        assert receipt['build']['source'] == spec['control_source']
        for name, entry in receipt['files'].items():
            path = Path(receipt['root']) / name
            assert path.stat().st_size == entry['bytes'] and digest(path) == entry['sha256'], name
        while True:
            build = json.loads(Path(spec['build_state']).read_text())
            assert build['source'] == spec['source'] and build['phase'] != 'failed', build
            if build['phase'] == 'complete':
                break
            process = Path('/proc', str(build['pid']), 'stat')
            assert process.exists() and process.read_text().rsplit(')', 1)[1].split()[19] == build['start_ticks']
            time.sleep(15)
        assert digest(Path(build['binary'])) == build['binary_sha256']
        write(root / 'build-identity.json', build)
        save(phase='coremark')
        smoke = root / 'coremark'
        completed_guest(smoke, build, spec['coremark'])
        gate = old_execution(Path(spec['control_coremark']), smoke, set())
        write(root / 'coremark-gate.json', gate)
        assert gate['passed'], gate
        save(phase='protocol_micro', coremark_gate_passed=True)
        command = ['python3', '-u', spec['micro_runner'], '--output-dir', str(root / 'protocol-micro'),
                   '--candidate-build', str(root / 'build-identity.json'), '--control-build', spec['control_build'], '--iterative']
        if spec.get('turnover_micro'):
            command.append('--turnover')
        with (root / 'micro.stdout').open('w') as stdout, (root / 'micro.stderr').open('w') as stderr:
            child = subprocess.Popen(command, cwd=root, stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr)
            write(root / 'micro-controller.json', dict(command=command, pid=child.pid,
                  start_ticks=Path('/proc', str(child.pid), 'stat').read_text().rsplit(')', 1)[1].split()[19], host=socket.gethostname()))
            rc = child.wait()
        assert rc == 0, 'Inspect original protocol-micro output'
        rows = json.loads((root / 'protocol-micro/results.json').read_text())
        assert len(rows) == (18 if spec.get('turnover_micro') else 12) and all(row['passed'] for row in rows), rows
        save(phase='short', micro_gate_passed=True, micro_completed=len(rows))
        with ThreadPoolExecutor(max_workers=3) as pool:
            results = list(pool.map(lambda case: completed_guest(root / case['point'], build, case), spec['short_cases']))
        write(root / 'short-results.json', results)
        save(phase='complete', completed=len(results), all_short_guests_passed=True)
    except BaseException as error:
        save(phase='failed', error=repr(error))
        raise


if __name__ == '__main__':
    main()
