"""Gate a diagnostic-only turnover build against closed8f8 controls and FP protocols."""
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path
import argparse
import json
import os
import socket
import subprocess
import time

from native_perf import read_blocks
from tea_dip_fdiv_ready_run import completed_guest
from tea_dip_fdiv_turnover_counts import FIELDS, turnover_counts
from tea_dip_query_endpoint_evidence import old_execution
from tea_dip_wb_protocol_micro import digest, write


def live(row):
    path = Path('/proc', str(row['pid']), 'stat')
    return path.exists() and path.read_text().rsplit(')', 1)[1].split()[19] == str(row['start_ticks'])


def closed_parent(spec):
    root = Path(spec['parent_root'])
    assert digest(root / 'experiment.json') == spec['parent_spec_sha256']
    parent_build = json.loads((root / 'build-identity.json').read_text())
    assert parent_build['source'] == spec['parent_source'] and parent_build['binary_sha256'] == spec['parent_binary_sha256']
    cases = [spec['coremark'], *spec['short_cases']]
    while True:
        state = json.loads((root / 'run-state.json').read_text())
        assert all(state[key] == spec['parent_process'][key] for key in ['pid', 'start_ticks'])
        if all((root / case['point'] / 'result.json').exists() for case in cases):
            break
        assert live(state), 'Inspect original parent termination and missing point results'
        time.sleep(15)
    guests = []
    names = ['experiment.json', 'build-identity.json']
    for case in cases:
        folder = root / case['point']
        result = json.loads((folder / 'result.json').read_text())
        process = json.loads((folder / 'process.json').read_text())
        assert not live(process) and result['process'] == process and result['original_process_closed']
        assert result['source'] == spec['parent_source'] and result['binary_sha256'] == spec['parent_binary_sha256']
        assert result['exitcode'] == 0 and result['difftest_passed'] and all(result['sampler_flags'].values())
        command = json.loads((folder / 'command.json').read_text())
        assert command[6] == parent_build['binary']
        for flag in ['-I', '-C', '-s', '-i', '--diff', '--cst-file', '--dump-select-db']:
            assert command[command.index(flag) + 1] == case['command'][case['command'].index(flag) + 1]
        if '-W' in case['command']:
            assert command[command.index('-W') + 1] == case['command'][case['command'].index('-W') + 1]
        guests.append(result)
        names += [case['point'] + '/' + name for name in ['command.json', 'result.json', 'exitcode', 'process.json',
                  'launch-observation.json', 'simulator_out.txt', 'simulator_err.txt', 'simulator.db']]
    files = {name: dict(bytes=(root / name).stat().st_size, sha256=digest(root / name)) for name in names}
    return dict(utc=datetime.now(timezone.utc).isoformat(), ready=True, root=str(root), source=spec['parent_source'],
                binary_sha256=spec['parent_binary_sha256'], all_selected_original_guests_closed=True,
                parent=state, parent_live=live(state), guests=guests, files=files, database_read=False)


def phases(folder):
    blocks = sorted(read_blocks(folder / 'simulator_err.txt').items())
    return [dict(native_print_stamp=stamp, modules=turnover_counts(block)) for stamp, block in blocks]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir', required=True, type=Path)
    root = parser.parse_args().run_dir.resolve()
    spec = json.loads((root / 'experiment.json').read_text())
    assert socket.gethostname().startswith('node') and not (root / 'run-state.json').exists()
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 host=socket.gethostname(), phase='waiting_build', completed=0)

    def save(**values):
        state.update(values, utc=datetime.now(timezone.utc).isoformat())
        write(root / 'run-state.json', state)

    save()
    try:
        for name, expected in spec['input_hashes'].items():
            assert digest(Path(name)) == expected, name
        while True:
            build = json.loads(Path(spec['build_state']).read_text())
            assert build['source'] == spec['source'] and build['phase'] != 'failed', build
            if build['phase'] == 'complete':
                break
            assert live(build), 'Follow the original build identity'
            time.sleep(15)
        assert digest(Path(build['binary'])) == build['binary_sha256']
        write(root / 'build-identity.json', build)
        save(phase='waiting_parent_closed_points')
        parent = closed_parent(spec)
        write(root / 'parent-closed-inputs.json', parent)
        names = None

        def execute(case):
            nonlocal names
            folder = root / case['point']
            result = completed_guest(folder, build, case)
            control = Path(spec['parent_root']) / case['point']
            before, after = [read_blocks(p / 'simulator_err.txt') for p in [control, folder]]
            old_names = {name for block in before.values() for name in block}
            actual_names = {name for block in after.values() for name in block}
            added = actual_names - old_names
            assert old_names <= actual_names and len(added) == 8
            counts = phases(folder)
            exact = {row['module'] + '.' + field for row in counts[0]['modules'] for field in FIELDS}
            assert added == exact
            if names is None:
                names = added
                write(root / 'exact-counter-names.json', sorted(names))
            assert names == added
            neutrality = old_execution(control, folder, names)
            result.update(observation_neutrality=neutrality, phases=counts,
                          exact_added_native_names=sorted(names), spec_sha256=digest(root / 'experiment.json'),
                          runner_sha256=digest(Path(__file__)))
            write(folder / 'result.json', result)
            assert neutrality['passed'], neutrality
            return result

        save(phase='coremark')
        execute(spec['coremark'])
        save(phase='protocol_micro', coremark_gate_passed=True)
        write(root / 'parent-build-identity.json', json.loads((Path(spec['parent_root']) / 'build-identity.json').read_text()))
        command = ['python3', '-u', str(root / 'tea_dip_wb_protocol_micro.py'), '--output-dir', str(root / 'protocol-micro'),
                   '--candidate-build', str(root / 'build-identity.json'), '--control-build', str(root / 'parent-build-identity.json'),
                   '--iterative', '--turnover']
        with (root / 'micro.stdout').open('w') as out, (root / 'micro.stderr').open('w') as err:
            child = subprocess.Popen(command, cwd=root, stdin=subprocess.DEVNULL, stdout=out, stderr=err)
            identity = dict(pid=child.pid, start_ticks=Path('/proc', str(child.pid), 'stat').read_text().rsplit(')', 1)[1].split()[19], host=socket.gethostname())
            write(root / 'micro-controller.json', identity)
            rc = child.wait()
        assert rc == 0, 'Inspect original protocol child output'
        micro = root / 'protocol-micro'
        rows = json.loads((micro / 'results.json').read_text())
        assert len(rows) == 18 and all(row['passed'] and row['original_process_closed'] and row['exitcode'] == 0 for row in rows)
        comparisons = []
        for case in json.loads((micro / 'experiment.json').read_text())['cases']:
            before, after = [micro / 'runs' / variant / case['case'] for variant in ['stage_control', 'candidate']]
            equality = old_execution(before, after, names)
            counts = phases(after)
            comparisons.append(dict(case=case['case'], observation_neutrality=equality, phases=counts))
        write(root / 'micro-neutrality.json', comparisons)
        assert all(row['observation_neutrality']['passed'] for row in comparisons)
        save(phase='short', micro_gate_passed=True, micro_completed=18)
        with ThreadPoolExecutor(max_workers=3) as pool:
            results = list(pool.map(execute, spec['short_cases']))
        write(root / 'short-results.json', results)
        save(phase='complete', completed=3, all_short_pairs_neutral=True)
    except BaseException as error:
        save(phase='failed', error=repr(error))
        raise


if __name__ == '__main__':
    main()
