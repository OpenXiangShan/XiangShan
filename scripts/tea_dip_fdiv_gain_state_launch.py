"""Launch the original gain-state observer only after the original parent gate closes."""
from pathlib import Path
from datetime import datetime, timezone
import argparse
import importlib.util
import json
import os
import re
import socket
import subprocess
import sys
import time

from tea_dip_query_cohort_evidence import digest, frame_counts, gain_span
from tea_dip_fdiv_unit_state_evidence import FILES, unit_counts


def write(path, value):
    temporary = path.with_suffix('.tmp')
    temporary.write_text(json.dumps(value, indent=2) + '\n')
    temporary.replace(path)


def live(process):
    path = Path('/proc', str(process['pid']), 'stat')
    return path.exists() and path.read_text().rsplit(')', 1)[1].split()[19] == process['start_ticks']


def context(config):
    parent = Path(config['parent_root'])
    assert digest(parent / 'experiment.json') == config['parent_spec_sha256']
    assert digest(parent / 'run.py') == config['parent_runner_sha256']
    spec = json.loads((parent / 'experiment.json').read_text())
    assert spec['source'] == config['parent_source']
    for name in ['native_reader', 'helper', 'parent_receipt', 'reference', 'cst']:
        assert digest(Path(spec[name])) == spec[name + '_sha256']
    source_path = Path(config['source_record'])
    assert digest(source_path) == config['source_record_sha256']
    source = json.loads(source_path.read_text())
    build = json.loads(Path(config['build_state']).read_text())
    assert source['head'] == build['source'] == '6cbc9752d352b43b15fc2177b9451caccbfe74f9'
    assert source['base'] == spec['source'] and build['phase'] == 'complete'
    assert build['binary_sha256'] == config['binary_sha256']
    return parent, spec, source, build


def parent_status(parent, config):
    state = json.loads((parent / 'run-state.json').read_text())
    assert all(state[key] == config['parent_controller'][key] for key in ['pid', 'start_ticks'])
    assert state['phase'] != 'failed', state
    active = live(config['parent_controller'])
    if active:
        return dict(parent_closed=False, original_controller_live=True, phase=state['phase'], database_read=False)
    assert state['phase'] == 'complete' and state['completed'] == 3
    assert state['all_short_pairs_neutral'] and state['coremark_gate_passed']
    for guest in config['parent_guests']:
        process = json.loads((parent / guest['point'] / 'process.json').read_text())
        assert process == guest['process'] and not live(process)
    return dict(parent_closed=True, original_controller_live=False, phase=state['phase'], database_read=False)


def validate_parent(parent, spec, config):
    assert parent_status(parent, config)['parent_closed']
    grand = json.loads(Path(spec['parent_receipt']).read_text())
    assert grand['ready'] and grand['all_selected_original_guests_closed']
    assert grand['source'] == spec['parent_source'] and grand['binary_sha256'] == spec['parent_binary_sha256']
    assert all(not live(guest['process']) for guest in grand['guests'])
    for name, item in grand['files'].items():
        path = Path(grand['root']) / name
        assert path.stat().st_size == item['bytes'] and digest(path) == item['sha256']
    for name, sha in config['shared_dependency_sha256'].items():
        assert digest(Path(__file__).parent / name) == sha
    sys.path.insert(0, str(parent))
    from native_perf import read_blocks, unique_counters
    helper_spec = importlib.util.spec_from_file_location('closed_parent_gate_helper', spec['helper'])
    helper = importlib.util.module_from_spec(helper_spec)
    helper_spec.loader.exec_module(helper)
    parent_build = json.loads(Path(spec['build_state']).read_text())
    assert parent_build['phase'] == 'complete' and parent_build['source'] == spec['source']
    assert parent_build['binary_sha256'] == config['parent_binary_sha256']
    assert digest(Path(parent_build['binary'])) == parent_build['binary_sha256']
    rows = []
    cases = [spec['coremark'], *spec['short_cases']]
    assert {case['point'] for case in cases} == {guest['point'] for guest in config['parent_guests']}
    for case in cases:
        folder = parent / case['point']
        result = json.loads((folder / 'result.json').read_text())
        process = json.loads((folder / 'process.json').read_text())
        assert result['process'] == process and not live(process)
        assert result['original_process_closed'] and result['exitcode'] == 0 and result['difftest_passed']
        assert result['source'] == spec['source'] and result['binary_sha256'] == parent_build['binary_sha256']
        assert result['spec_sha256'] == config['parent_spec_sha256'] and result['runner_sha256'] == config['parent_runner_sha256']
        text = (folder / 'simulator_out.txt').read_text()
        assert 'Difftest enabled' in text and 'EXCEEDING CYCLE/INSTR LIMIT' in text
        for key, value in [('enableTea0', 1), ('enableDip0', 1), ('teaSamplePeriod0', 1000), ('dipSamplePeriod0', 1000)]:
            assert re.search(r'^' + key + ' = ' + str(value) + '$', text, re.M)
        blocks = read_blocks(folder / 'simulator_err.txt')
        old = read_blocks(Path(case['control']) / 'simulator_err.txt')
        added = {key for b in blocks.values() for key in b} - {key for b in old.values() for key in b}
        assert len(added) == 32 and {key.rsplit('.', 1)[-1] for key in added} == set(spec['new_exact_counter_names'])
        neutral = helper.old_execution(Path(case['control']), folder, added)
        assert neutral['passed'] and len(blocks) == (2 if case.get('warmup') else 1)
        phases, previous = [], {}
        for stamp, block in sorted(blocks.items()):
            counters = unique_counters(block)
            clock, = [value for key, value in counters.items() if key.endswith('.rob.clock_cycle')]
            units = unit_counts(block, spec['new_exact_counter_names'], clock)
            queues = frame_counts(block, config['cohort_fields'])
            spans = {queue: gain_span(previous.get(queue, {key: 0 for key in counts}), counts) for queue, counts in queues.items()}
            assert all(span['delta']['cancel_ex0_no_src'] <= span['delta']['cancel_ex0'] for span in spans.values())
            phases.append(dict(native_print_stamp=stamp, units=units, gain_spans=spans))
            previous = queues
        rows.append(dict(point=case['point'],neutrality=neutral,phases=phases))
    names = [case['point'] + '/' + name for case in cases for name in FILES]
    names += ['experiment.json', 'run.py', 'native_perf.py', 'tea_dip_query_endpoint_evidence.py', 'parent-closed-inputs.json', 'run-state.json', 'short-results.json']
    files = {name: dict(bytes=(parent / name).stat().st_size, sha256=digest(parent / name)) for name in names}
    assert len(files) == 39
    return dict(ready=True, utc=datetime.now(timezone.utc).isoformat(), all_selected_original_guests_closed=True,
                root=str(parent), source=spec['source'], binary_sha256=parent_build['binary_sha256'],
                guests=config['parent_guests'], files=files), rows


def launch_observer(gate, config, parent, spec, source, build, receipt, rows):
    assert parent_status(parent, config)['parent_closed']
    assert os.getpriority(os.PRIO_PROCESS, 0) == 0, 'The unchanged runner adds nice10; gate must retain nice0'
    root = Path(config['node_run_dir'])
    assert not root.exists(), 'Follow the original gain-state observer'
    assert digest(Path(build['binary'])) == build['binary_sha256']
    source_root = Path(source['root'])
    assert subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=source_root, text=True).strip() == source['head']
    assert not subprocess.check_output(['git', 'diff', 'HEAD', '--name-only'], cwd=source_root, text=True).strip()
    validation = dict(utc=datetime.now(timezone.utc).isoformat(), ready=True, parent_source=spec['source'], rows=rows,
                      scope='Actual closed parent four-input old-native/six-table neutrality and32-field unit partitions/gain ledger gate. No new6cbc runtime result.')
    write(gate / 'parent-gate-validation.json', validation)
    receipt['gate_validation_sha256'] = digest(gate / 'parent-gate-validation.json')
    root.mkdir()
    for name in ['run.py', 'native_perf.py', 'tea_dip_query_endpoint_evidence.py']:
        (root / name).write_bytes((parent / name).read_bytes())
    write(root / 'parent-closed-inputs.json', receipt)
    new = dict(source=source['head'], parent_source=spec['source'], parent_binary_sha256=receipt['binary_sha256'],
               build_state=config['build_state'], node_run_dir=str(root), reference=spec['reference'], reference_sha256=spec['reference_sha256'],
               cst=spec['cst'], cst_sha256=spec['cst_sha256'], native_reader=str(root / 'native_perf.py'),
               native_reader_sha256=digest(root / 'native_perf.py'), helper=str(root / 'tea_dip_query_endpoint_evidence.py'),
               helper_sha256=digest(root / 'tea_dip_query_endpoint_evidence.py'), parent_receipt=str(root / 'parent-closed-inputs.json'),
               parent_receipt_sha256=digest(root / 'parent-closed-inputs.json'), new_exact_counter_names=source['new_exact_counter_names'],
               coremark={**spec['coremark'], 'control': str(parent / spec['coremark']['point'])},
               short_cases=[{**case, 'control': str(parent / case['point'])} for case in spec['short_cases']],
               scope='One diagnostic6cbc observer gated by four actual closed34ee parents, CoreMark then3 same-seed short GSIM+Difftest+TEA/DIP1000 inputs. No architecture resource increase, formal run, CI or suite score.')
    write(root / 'experiment.json', new)
    command = ['python3', '-u', str(root / 'run.py'), '--run-dir', str(root)]
    with (root / 'controller.stdout').open('w') as out, (root / 'controller.stderr').open('w') as err:
        child = subprocess.Popen(command, cwd=root, stdin=subprocess.DEVNULL, stdout=out, stderr=err, start_new_session=True)
    process = dict(pid=child.pid, start_ticks=Path('/proc', str(child.pid), 'stat').read_text().rsplit(')', 1)[1].split()[19], host=socket.gethostname())
    launch = dict(utc=datetime.now(timezone.utc).isoformat(), run_dir=str(root), process=process, command=command,
                  observer_controller_nice=os.getpriority(os.PRIO_PROCESS, child.pid),
                  spec_sha256=digest(root / 'experiment.json'), runner_sha256=digest(root / 'run.py'), source=source['head'],
                  parent_source=spec['source'], parent_gate_validation_sha256=receipt['gate_validation_sha256'], database_read=False)
    write(root / 'launch.json', launch)
    write(gate / 'observer-launch.json', launch)
    return launch


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--gate-dir', type=Path, required=True)
    parser.add_argument('--check-only', action='store_true', help='Check original parent closure without reading DBs or launching')
    args = parser.parse_args()
    gate = args.gate_dir.resolve()
    config = json.loads((gate / 'config.json').read_text())
    assert socket.gethostname().startswith('node')
    parent, spec, source, build = context(config)
    check = parent_status(parent, config)
    if args.check_only:
        check['gate_nice'] = os.getpriority(os.PRIO_PROCESS, 0)
        print(json.dumps(check))
        return
    assert not (gate / 'gate-state.json').exists(), 'Follow the original launch gate'
    assert not Path(config['node_run_dir']).exists(), 'Follow the original gain-state observer'
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 host=socket.gethostname(), nice=os.getpriority(os.PRIO_PROCESS, 0),
                 config_sha256=digest(gate / 'config.json'), source=source['head'])
    def save(**values):
        state.update(values, utc=datetime.now(timezone.utc).isoformat())
        write(gate / 'gate-state.json', state)
    try:
        while not check['parent_closed']:
            save(phase='waiting_parent', parent_status=check)
            time.sleep(15)
            check = parent_status(parent, config)
        save(phase='validating_closed_parent')
        receipt, rows = validate_parent(parent, spec, config)
        launch = launch_observer(gate, config, parent, spec, source, build, receipt, rows)
        save(phase='launched', observer=launch['process'], launch_sha256=digest(gate / 'observer-launch.json'))
    except BaseException as error:
        save(phase='failed', error=repr(error))
        raise


if __name__ == '__main__':
    main()
