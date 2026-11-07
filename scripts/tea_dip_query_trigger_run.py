"""Build-gated FDIV query diagnostics: CoreMark and three neutral short windows, then soplex."""
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import json
import os
import re
import socket
import subprocess
import time


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def write(path, data):
    temporary = path.with_suffix('.tmp')
    temporary.write_text(json.dumps(data, indent=2) + '\n')
    temporary.replace(path)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir', type=Path, required=True)
    options = parser.parse_args()
    root = options.run_dir.resolve()
    spec = json.loads((root / 'experiment.json').read_text())
    assert socket.gethostname().startswith('node')
    state_path = root / 'run-state.json'
    assert not state_path.exists(), 'Follow the original diagnostic controller'
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 host=socket.gethostname(), phase='waiting_build', completed=0)
    def save(**values):
        state.update(values, utc=datetime.now(timezone.utc).isoformat())
        write(state_path, state)
    save()
    try:
        while True:
            build_path = Path(spec['build_state'])
            if build_path.exists():
                build = json.loads(build_path.read_text())
                assert build['source'] == spec['source']
                assert build['phase'] != 'failed', build
                if build['phase'] == 'complete':
                    break
                process = Path('/proc', str(build['pid']), 'stat')
                assert process.exists() and process.read_text().rsplit(')', 1)[1].split()[19] == build['start_ticks']
            time.sleep(15)
        assert digest(Path(build['binary'])) == build['binary_sha256']
        for name in ['reference', 'cst', 'native_reader', 'helper']:
            assert digest(Path(spec[name])) == spec[name + '_sha256'], name
        assert digest(Path(spec['focused_receipt'])) == spec['focused_receipt_sha256']
        receipt = json.loads(Path(spec['focused_receipt']).read_text())
        assert receipt['ready'] and not receipt['controller_live'] and receipt['all_original_guests_closed']
        for name, row in receipt['files'].items():
            path = Path(receipt['root']) / name
            assert path.stat().st_size == row['bytes'] and digest(path) == row['sha256']
        coremark_receipt = json.loads(Path(spec['coremark_receipt']).read_text())
        assert digest(Path(spec['coremark_receipt'])) == spec['coremark_receipt_sha256']
        assert coremark_receipt['ready'] and not any(coremark_receipt[key] for key in
            ['original_build_controller_live', 'original_smoke_controller_live', 'original_guest_live'])
        for name, row in coremark_receipt['files'].items():
            path = Path(coremark_receipt['root']) / name
            assert path.stat().st_size == row['bytes'] and digest(path) == row['sha256']
        from native_perf import read_blocks
        from tea_dip_query_endpoint_evidence import old_execution
        def execute(case):
            folder = root / case['point']
            folder.mkdir()
            command = ['nice', '-n', '10', 'taskset', '-c', str(case['core']), build['binary'],
                       '-I', str(case['max_instr']), '-C', '100000000', '-s', str(case['seed']), '-i', case['image'],
                       '--diff', spec['reference'], '--cst-file', spec['cst'], '--dramsim3-outdir', str(folder),
                       '--dump-select-db', 'Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0',
                       '--db-path', str(folder / 'simulator.db')]
            if case.get('warmup'):
                command += ['-W', str(case['warmup'])]
            write(folder / 'command.json', command)
            with (folder / 'simulator_out.txt').open('w') as stdout, (folder / 'simulator_err.txt').open('w') as stderr:
                child = subprocess.Popen(command, cwd=folder, stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr)
                process = dict(pid=child.pid, start_ticks=Path('/proc', str(child.pid), 'stat').read_text().rsplit(')', 1)[1].split()[19],
                               host=socket.gethostname(), core=case['core'])
                write(folder / 'process.json', process)
                write(folder / 'launch-observation.json', dict(utc=datetime.now(timezone.utc).isoformat(), process=process,
                      original_process_live=True, command=command, source=spec['source'], binary_sha256=build['binary_sha256']))
                returncode = child.wait()
            (folder / 'exitcode').write_text(str(returncode))
            text = (folder / 'simulator_out.txt').read_text()
            assert returncode == 0 and 'Difftest enabled' in text and 'EXCEEDING CYCLE/INSTR LIMIT' in text
            assert not any(marker in text for marker in ['HIT CRITICAL', 'HIT BAD TRAP', 'ABORT'])
            for key, value in [('enableTea0', 1), ('enableDip0', 1), ('teaSamplePeriod0', 1000), ('dipSamplePeriod0', 1000)]:
                assert re.search(r'^' + key + ' = ' + str(value) + '$', text, re.M)
            neutrality = None
            if 'control' in case:
                control = Path(case['control'])
                previous = json.loads((control / 'result.json').read_text())
                assert previous['source'] == spec['parent_source'] and previous['binary_sha256'] == spec['parent_binary_sha256']
                before, after = [read_blocks(path / 'simulator_err.txt') for path in (control, folder)]
                old_names = {name for block in before.values() for name in block}
                names = {name for block in after.values() for name in block}
                new_names = names - old_names
                assert old_names <= names and len(new_names) == 22
                assert {name.rsplit('.', 1)[-1] for name in new_names} == set(spec['new_exact_counter_names'])
                neutrality = old_execution(control, folder, new_names)
                assert neutrality['passed'], neutrality
            result = dict(point=case['point'], source=spec['source'], binary_sha256=build['binary_sha256'],
                          original_process_closed=True, exitcode=returncode, process=process, difftest_passed=True,
                          observation_neutrality=neutrality, performance_speedup=None,
                          spec_sha256=digest(root / 'experiment.json'), runner_sha256=digest(Path(__file__)))
            write(folder / 'result.json', result)
            return result
        save(phase='coremark')
        smoke = execute(spec['coremark'])
        save(phase='short', coremark_gate_passed=smoke['observation_neutrality']['passed'])
        with ThreadPoolExecutor(max_workers=3) as pool:
            short = list(pool.map(execute, spec['short_cases']))
        write(root / 'short-results.json', short)
        save(phase='formal', completed=3, all_short_pairs_neutral=True)
        formal = execute(spec['formal_case'])
        write(root / 'formal-result.json', formal)
        save(phase='complete', completed=4)
    except BaseException as error:
        save(phase='failed', error=repr(error))
        raise


if __name__ == '__main__':
    main()
