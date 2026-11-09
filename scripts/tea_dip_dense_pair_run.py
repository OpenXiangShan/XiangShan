"""Measure dense-sampling neutrality against a closed sparse pair using identical binaries."""
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path
import argparse
import json
import os
import socket
import sqlite3

from native_perf import read_blocks, counter_multisets
from tea_dip_fdiv_ready_run import completed_guest
from tea_dip_query_endpoint_evidence import TABLES, verify
from tea_dip_wb_protocol_micro import digest, write


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir', type=Path, required=True)
    root = parser.parse_args().run_dir.resolve()
    spec = json.loads((root / 'experiment.json').read_text())
    assert socket.gethostname().startswith('node') and not (root / 'run-state.json').exists()
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 host=socket.gethostname(), boot_id=Path('/proc/sys/kernel/random/boot_id').read_text().strip(),
                 phase='preflight', completed=0)

    def save(**values):
        state.update(values, utc=datetime.now(timezone.utc).isoformat())
        write(root / 'run-state.json', state)

    def live(process):
        path = Path('/proc', str(process['pid']), 'stat')
        return path.exists() and path.read_text().rsplit(')', 1)[1].split()[19] == str(process['start_ticks'])

    save()
    try:
        receipt = spec['baseline_receipt']
        assert receipt['ready'] and receipt['all_original_guests_closed']
        assert not live(receipt['controller']) and all(not live(p) for p in receipt['processes'].values())
        baseline = Path(receipt['root'])
        verify(baseline, receipt['files'])
        for name, value in spec['input_sha256'].items():
            assert digest(name) == value, name
        cst = root / 'tea-dip-period1.cst'
        cst.write_text('enableTea0 1\nteaSamplePeriod0 1\nenableDip0 1\ndipSamplePeriod0 1\n')
        save(phase='running')

        def execute(case):
            variant = case['variant']
            build = case['build']
            assert digest(build['binary']) == build['binary_sha256']
            command = case['command'].copy()
            command[command.index('--cst-file') + 1] = str(cst)
            result = completed_guest(root / variant, build, dict(case, command=command), sample_period=1)
            old = baseline / variant
            before = {stamp: counter_multisets(block) for stamp, block in read_blocks(old / 'simulator_err.txt').items()}
            after = {stamp: counter_multisets(block) for stamp, block in read_blocks(root / variant / 'simulator_err.txt').items()}
            auxiliary = {}
            rows = {}
            with sqlite3.connect((old / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as left, \
                 sqlite3.connect((root / variant / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as right:
                for table in TABLES:
                    rows[table] = [db.execute('SELECT count(*) FROM ' + table).fetchone()[0] for db in [left, right]]
                for table in ['TeaCycleSummary_0', 'TeaNoCommit_0']:
                    auxiliary[table] = (left.execute('SELECT * FROM ' + table + ' ORDER BY ID').fetchall() ==
                                        right.execute('SELECT * FROM ' + table + ' ORDER BY ID').fetchall())
            terminal = json.loads((old / 'result.json').read_text())['trap_counts'] == result['trap_counts']
            calibration = dict(passed=before == after and all(auxiliary.values()) and terminal,
                               all_native_multivalues_equal=before == after, auxiliary_tables_equal=auxiliary,
                               terminal_counts_equal=terminal, table_rows_sparse_dense=rows,
                               baseline_receipt_sha256=spec['baseline_receipt_sha256'],
                               scope='Same binary and declared window, changing only sampler1000 to1. Sampled tables intentionally differ. This does not prove dynamic-prefix identity between architecture variants or phase-subset equivalence.')
            write(root / variant / 'sampling-calibration.json', calibration)
            assert calibration['passed'], calibration
            return dict(result=result, calibration=calibration)

        with ThreadPoolExecutor(max_workers=2) as pool:
            results = dict(zip([case['variant'] for case in spec['cases']], pool.map(execute, spec['cases'])))
        write(root / 'results.json', results)
        save(phase='complete', completed=len(results), native_sampling_neutrality_verified=True)
    except BaseException as error:
        save(phase='failed', error=repr(error))
        raise


if __name__ == '__main__':
    main()
