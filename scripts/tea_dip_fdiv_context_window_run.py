"""Compare one matched SPEC window on the existing ec964 and 4339 diagnostic binaries."""
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path
import argparse
import json
import os
import socket

from native_perf import read_blocks
from tea_dip_fdiv_predict_counts import FIELDS, predict_counts
from tea_dip_fdiv_ready_run import completed_guest
from tea_dip_query_endpoint_evidence import old_execution
from tea_dip_wb_protocol_micro import digest, write


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir', type=Path, required=True)
    root = parser.parse_args().run_dir.resolve()
    assert socket.gethostname().startswith('node') and not (root / 'run-state.json').exists()
    spec = json.loads((root / 'experiment.json').read_text())
    assert spec['builds']['control']['source'] == 'ec964c8a623c8f72d32970cf984a8c8be8708d03'
    assert spec['builds']['observer']['source'] == '4339dea45cf776f469c366d147bdf720d03aa79b'
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 host=socket.gethostname(), phase='checking_inputs', completed=0)

    def save(**values):
        state.update(values, utc=datetime.now(timezone.utc).isoformat())
        write(root / 'run-state.json', state)

    save()
    try:
        for path, expected in spec['input_hashes'].items():
            assert digest(Path(path)) == expected, path
        for build in spec['builds'].values():
            assert build['phase'] == 'complete' and digest(Path(build['binary'])) == build['binary_sha256']
        save(phase='paired_window')

        def execute(variant):
            case = dict(spec['case'], core=spec['cores'][variant])
            return completed_guest(root / variant, spec['builds'][variant], case)

        with ThreadPoolExecutor(max_workers=2) as pool:
            results = dict(zip(['control', 'observer'], pool.map(execute, ['control', 'observer'])))
        write(root / 'results.json', results)
        blocks = read_blocks(root / 'observer/simulator_err.txt')
        names = {name for block in blocks.values() for name in block if name.rsplit('.', 1)[-1] in FIELDS}
        assert len(names) == 16
        equality = old_execution(root / 'control', root / 'observer', names)
        phases = [dict(native_print_stamp=stamp, modules=predict_counts(block))
                  for stamp, block in sorted(blocks.items())] if equality['passed'] else None
        value = dict(observation_neutrality=equality, phases=phases,
                     event_interpretation_valid=equality['passed'], performance_speedup=None,
                     spec_sha256=digest(root / 'experiment.json'), runner_sha256=digest(Path(__file__)),
                     scope='This matched diagnostic window only. Context mask6 is necessary context, '
                           'not proof of late-ready state, criticality or saved cycles. '
                           'Does not replace the original SPEC context gate or formal performance tests.')
        write(root / 'window-analysis.json', value)
        assert equality['passed'], equality
        save(phase='complete', completed=2, all_old_native_six_tables_neutral=True)
    except BaseException as error:
        save(phase='failed', error=repr(error))
        raise


if __name__ == '__main__':
    main()
