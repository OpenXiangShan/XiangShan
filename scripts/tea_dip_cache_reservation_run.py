"""Validate a built cache candidate with fresh pairs and analyze each closed stage."""
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import json
import os
import re
import socket
import sqlite3
import time

import dip_analyze as dip
from native_perf import read_blocks
from tea_dip_fdiv_ready_run import completed_guest
from tea_dip_memory_native import analyze as memory_evidence
from tea_dip_query_endpoint_evidence import TABLES, endpoint
from tea_dip_wb_protocol_micro import digest, write


def closed_budget(folder, result):
    assert result['original_process_closed'] and result['difftest_passed']
    process = result['process']
    stat = Path('/proc', str(process['pid']), 'stat')
    assert not stat.exists() or stat.read_text().rsplit(')', 1)[1].split()[19] != process['start_ticks']
    files = {}
    for name in ['command.json', 'process.json', 'result.json', 'simulator_out.txt', 'simulator_err.txt', 'simulator.db']:
        size, hasher = 0, hashlib.sha256()
        with (folder / name).open('rb') as stream:
            for block in iter(lambda: stream.read(1024 * 1024), b''):
                size += len(block)
                hasher.update(block)
        files[name] = dict(bytes=size, sha256=hasher.hexdigest())
    receipt = folder / 'closure-receipt.json'
    write(receipt, dict(ready=True, point=folder.name, source=result['source'], original_process_live=False,
                        process=process, files=files))
    blocks = read_blocks(folder / 'simulator_err.txt')
    command = json.loads((folder / 'command.json').read_text())
    assert len(blocks) == (2 if '-W' in command else 1)
    actual_seed = int(re.search(r'Using seed = (\d+)', (folder / 'simulator_out.txt').read_text())[1])
    assert actual_seed == int(command[command.index('-s') + 1])
    last = blocks[max(blocks)]

    def rob(suffix, block=last):
        value, = [v for name, values in block.items() if name.endswith('.rob.' + suffix) for v in values]
        return value

    cycles, retired = rob('clock_cycle'), rob('commitInstr')
    if '-W' in command:
        warmup = int(command[command.index('-W') + 1])
        assert abs(rob('commitInstr', blocks[min(blocks)]) - warmup) < 128
        assert abs(retired - (int(command[command.index('-I') + 1]) - warmup)) < 128
    cpi = cycles / retired
    begin = min(blocks) + 1 if len(blocks) == 2 else 0
    end = max(blocks) + 1
    with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        table_rows = {table: db.execute('SELECT count(*) FROM ' + table).fetchone()[0] for table in TABLES}
        assert all(table_rows.values()), table_rows
        counts = {row[0]: row[1] for row in db.execute(
            'SELECT STATE,count(*) FROM Tea_0 WHERE CYCLE>=? AND CYCLE<? GROUP BY STATE', (begin, end))}
        width = sum(row['name'].startswith('SLOTS_') and row['name'].endswith('_STATE')
                    for row in db.execute('pragma table_info(Dip_0)'))
        _, summary = dip.analyze(
            db.execute('SELECT * FROM Dip_0 WHERE CYCLE>=? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end)),
            db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'), width)
    samples = sum(counts.values())
    assert samples == summary['sampled_cycles'] and samples > 0
    tea = {state: counts.get(i, 0) / samples * cpi for i, state in enumerate(
        ['Computing', 'Stalled', 'Flushed', 'Drained'])}
    dip_budget = {state: summary['state_cycles'][state] / samples * cpi for state in dip.STATES}
    dip_budget['Unresolved'] = summary['unresolved_slots'] / summary['sampled_slots'] * cpi
    assert abs(sum(tea.values()) - cpi) < 1e-9 and abs(sum(dip_budget.values()) - cpi) < 1e-9
    return dict(native_cycles=cycles, native_retired=retired, native_CPI=cpi, actual_seed=actual_seed, table_rows=table_rows,
                sampled_TEA_CPI=tea, DIP_CPI=dip_budget,
                exact_TEA=endpoint(folder) if len(blocks) == 2 else None,
                memory=memory_evidence(folder, receipt) if len(blocks) == 2 else None,
                scope='Each native denominator retained. Sampled budgets are rescaled to native CPI; closure does not prove representative sampling, dynamic-prefix identity or unique causal attribution.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir', required=True, type=Path)
    root = parser.parse_args().run_dir.resolve()
    spec = json.loads((root / 'experiment.json').read_text())
    assert socket.gethostname().startswith('node') and not (root / 'run-state.json').exists()
    boot = Path('/proc/sys/kernel/random/boot_id').read_text().strip()
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 host=socket.gethostname(), boot_id=boot, phase='waiting_build', completed=0)

    def save(**values):
        state.update(values, utc=datetime.now(timezone.utc).isoformat())
        write(root / 'run-state.json', state)

    save()
    try:
        for name, expected in spec['input_sha256'].items():
            assert digest(name) == expected, name
        while True:
            build = json.loads(Path(spec['build_state']).read_text())
            assert build['source'] == spec['source'] and build['cache_source'] == spec['cache_source']
            assert build['boot_id'] == boot and build['phase'] != 'failed', build
            if build['phase'] == 'complete':
                break
            stat = Path('/proc', str(build['pid']), 'stat')
            assert stat.exists() and stat.read_text().rsplit(')', 1)[1].split()[19] == build['start_ticks']
            time.sleep(15)
        assert digest(build['binary']) == build['binary_sha256']
        resource = json.loads(Path(spec['FIR_comparison']).read_text())
        assert resource['all_register_declarations_equal']
        write(root / 'build-identity.json', build)
        builds = dict(control=spec['control_build'], candidate=build)
        for variant in builds:
            (root / variant).mkdir()
        results = {}
        for case in [spec['coremark'], *spec['cases']]:
            save(phase=case['point'])

            def execute(variant):
                folder = root / variant / case['point']
                result = completed_guest(folder, builds[variant], dict(case, core=case[variant + '_core']))
                return dict(result=result, budget=closed_budget(folder, result))

            with ThreadPoolExecutor(max_workers=2) as pool:
                pair = dict(zip(builds, pool.map(execute, builds)))
            before, after = [pair[v]['budget'] for v in builds]
            pair['delta_CPI'] = after['native_CPI'] - before['native_CPI']
            pair['observed_speedup'] = before['native_CPI'] / after['native_CPI'] - 1
            pair['DIP_delta_CPI'] = {k: after['DIP_CPI'][k] - before['DIP_CPI'][k] for k in after['DIP_CPI']}
            results[case['point']] = pair
            write(root / 'results.json', results)
            save(completed=len(results))
        save(phase='complete', all_guests_Difftest_passed=True)
    except BaseException as error:
        save(phase='failed', error=repr(error))
        raise


if __name__ == '__main__':
    main()
