"""Validate a BPU event observer against one closed same-RTL control on a node."""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import json
import os
import socket
import sqlite3
import time

from tea_dip_fdiv_ready_run import completed_guest
from tea_dip_fir_registers import register_inventory
from tea_dip_query_endpoint_evidence import TABLES, digest, old_execution, verify
from tea_dip_wb_protocol_micro import write


def resources(control, observed):
    inventories = [register_inventory(path) for path in [control, observed]]
    before, after = [item['modules'] for item in inventories]
    added, removed, changed = [], [], []
    for module in before.keys() | after.keys():
        a, b = before.get(module, {}), after.get(module, {})
        added.extend(dict(module=module, name=name, **b[name]) for name in b.keys() - a.keys())
        removed.extend(dict(module=module, name=name, **a[name]) for name in a.keys() - b.keys())
        changed.extend(dict(module=module, name=name, before=a[name], after=b[name])
                       for name in a.keys() & b.keys() if a[name] != b[name])
    passed = not removed and not changed and len(added) == 1 and added[0]['module'] == 'Ftq'
    passed = passed and added[0]['type'] == 'UInt<64>'
    return dict(passed=passed, inventories=[dict(path=x['path'], sha256=x['sha256'],
                modules=len(x['modules']), register_declarations=x['register_declarations']) for x in inventories],
                added=added, removed=removed, changed=changed,
                diagnostic_bits=64 if passed else None, physical_area_and_timing='unmeasured')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir', type=Path, required=True)
    root = parser.parse_args().run_dir.resolve()
    spec = json.loads((root / 'experiment.json').read_text())
    assert socket.gethostname().startswith('node')
    assert not (root / 'run-state.json').exists(), 'Follow the original controller'
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 host=socket.gethostname(), phase='waiting_build')

    def save(**values):
        state.update(values, utc=datetime.now(timezone.utc).isoformat())
        write(root / 'run-state.json', state)

    save()
    try:
        for name, expected in spec['input_hashes'].items():
            assert digest(Path(name)) == expected, name
        receipt = json.loads(Path(spec['control_receipt']).read_text())
        assert receipt['ready'] and not receipt['original_process_live']
        verify(Path(spec['control']), receipt['files'])
        gate = None
        if spec['case']['point'] == 'coremark':
            assert receipt['build']['source'] == spec['control_source']
        else:
            assert receipt['source'] == spec['control_source']
            gate = json.loads(Path(spec['coremark_gate']).read_text())
            assert gate['passed'] and gate['source'] == spec['source']
            assert gate['native_six_table_neutrality']['passed'] and gate['event_audit']['passed']
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
        if gate is None:
            budget = resources(Path(spec['control_fir']), Path(spec['observer_fir']))
        else:
            assert gate['binary_sha256'] == build['binary_sha256']
            budget = gate['resource_audit']
        write(root / 'resource-audit.json', budget)
        assert budget['passed'], budget
        case = spec['case']
        command = case['command']
        command[command.index('--dump-select-db') + 1] = ' '.join([*TABLES, 'BpuCommitMispredict'])
        save(phase='guest')
        folder = root / case['point']
        result = completed_guest(folder, build, case)
        result.update(spec_sha256=digest(root / 'experiment.json'), runner_sha256=digest(Path(__file__)))
        write(folder / 'result.json', result)
        neutrality = old_execution(Path(spec['control']), folder, set())
        write(root / 'neutrality.json', neutrality)
        assert neutrality['passed'], neutrality
        with sqlite3.connect((folder / 'simulator.db').as_uri() + '?mode=ro', uri=True) as database:
            schema = database.execute('PRAGMA table_info(BpuCommitMispredict)').fetchall()
            assert schema
            row_count = database.execute('SELECT count(*) FROM BpuCommitMispredict').fetchone()[0]
        write(root / 'event-schema.json', dict(columns=schema, rows=row_count))
        save(phase='complete', native_six_table_neutrality_passed=True, event_rows=row_count,
             event_interpretation_valid=None)
    except BaseException as error:
        save(phase='failed', error=repr(error))
        raise


if __name__ == '__main__':
    main()
