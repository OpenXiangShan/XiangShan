"""Run one controlled policy pair with existing GSIM binaries and closed-window analysis."""
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path
import argparse
import json
import os
import re
import socket

from tea_dip_cache_reservation_run import closed_budget
from tea_dip_fdiv_ready_run import completed_guest
from tea_dip_wb_protocol_micro import digest, write


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir', required=True, type=Path)
    root = parser.parse_args().run_dir.resolve()
    spec = json.loads((root / 'experiment.json').read_text())
    assert socket.gethostname().startswith('node') and not (root / 'run-state.json').exists()
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 host=socket.gethostname(), boot_id=Path('/proc/sys/kernel/random/boot_id').read_text().strip(),
                 phase='preflight', completed=0)

    def save(**values):
        state.update(values, utc=datetime.now(timezone.utc).isoformat())
        write(root / 'run-state.json', state)

    save()
    try:
        for name, expected in spec['input_sha256'].items():
            assert digest(name) == expected, name
        for variant in ['control', 'candidate']:
            (root / variant).mkdir()
        save(phase='running')

        def execute(variant):
            case = dict(spec['case'], core=spec['case'][variant + '_core'])
            folder = root / variant / case['point']
            result = completed_guest(folder, spec['builds'][variant], case)
            text = (folder / 'simulator_out.txt').read_text()
            for name, value in spec['policy'].items():
                assert re.search(r'^' + re.escape(name) + ' = ' + str(value) + '$', text, re.M), name
            return dict(result=result, budget=closed_budget(folder, result))

        with ThreadPoolExecutor(max_workers=2) as pool:
            pair = dict(zip(['control', 'candidate'], pool.map(execute, ['control', 'candidate'])))
        before, after = [pair[variant]['budget']['native_CPI'] for variant in ['control', 'candidate']]
        pair.update(delta_CPI=after-before, observed_speedup=before/after-1, policy=spec['policy'])
        write(root / 'results.json', pair)
        save(phase='complete', completed=2, all_guests_Difftest_passed=True)
    except BaseException as error:
        save(phase='failed', error=repr(error))
        raise


if __name__ == '__main__':
    main()
