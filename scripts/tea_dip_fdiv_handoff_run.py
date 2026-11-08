"""Gate the diagnostic handoff build against closed cd5 controls and FP protocols."""
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
from tea_dip_fdiv_handoff_counts import FIELDS, handoff_counts
from tea_dip_fdiv_ready_run import completed_guest
from tea_dip_fdiv_turnover_observer_run import closed_parent, live
from tea_dip_query_endpoint_evidence import old_execution
from tea_dip_wb_protocol_micro import digest, write


def phases(folder):
    return [dict(native_print_stamp=stamp, modules=handoff_counts(block))
            for stamp, block in sorted(read_blocks(folder / 'simulator_err.txt').items())]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir', type=Path, required=True)
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
        for path, expected in spec['input_hashes'].items():
            assert digest(Path(path)) == expected, path
        while True:
            build = json.loads(Path(spec['build_state']).read_text())
            assert build['source'] == spec['source'] and build['phase'] != 'failed', build
            if build['phase'] == 'complete':
                break
            assert live(build), 'Follow the original build identity'
            time.sleep(15)
        assert digest(Path(build['binary'])) == build['binary_sha256']
        write(root / 'build-identity.json', build)
        write(root / 'parent-closed-inputs.json', closed_parent(spec))
        exact = None

        def execute(case):
            nonlocal exact
            folder = root / case['point']
            result = completed_guest(folder, build, case)
            control = Path(spec['parent_root']) / case['point']
            before, after = [read_blocks(p / 'simulator_err.txt') for p in [control, folder]]
            old_names = {name for block in before.values() for name in block}
            actual_names = {name for block in after.values() for name in block}
            added = actual_names - old_names
            assert old_names <= actual_names and len(added) == 34
            counts = phases(folder)
            names = {row['module'] + '.' + field for row in counts[0]['modules'] for field in FIELDS}
            assert added == names
            if exact is None:
                exact = names
                write(root / 'exact-counter-names.json', sorted(exact))
            assert names == exact
            neutrality = old_execution(control, folder, names)
            result.update(observation_neutrality=neutrality, phases=counts, exact_added_native_names=sorted(names),
                          spec_sha256=digest(root / 'experiment.json'), runner_sha256=digest(Path(__file__)))
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
            write(root / 'micro-controller.json', dict(pid=child.pid, start_ticks=Path('/proc', str(child.pid), 'stat').read_text().rsplit(')', 1)[1].split()[19], host=socket.gethostname()))
            rc = child.wait()
        assert rc == 0, 'Inspect original protocol child output'
        micro = root / 'protocol-micro'
        results = json.loads((micro / 'results.json').read_text())
        assert len(results) == 18 and all(row['passed'] and row['original_process_closed'] for row in results)
        comparisons = []
        for case in json.loads((micro / 'experiment.json').read_text())['cases']:
            before, after = [micro / 'runs' / variant / case['case'] for variant in ['stage_control', 'candidate']]
            comparisons.append(dict(case=case['case'], observation_neutrality=old_execution(before, after, exact), phases=phases(after)))
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
