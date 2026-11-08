"""Gate retry-ready validation on correctness and an original closed SPEC context."""
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
from tea_dip_fdiv_predict_counts import FIELDS, predict_counts
from native_perf import read_blocks


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
        rc = child.wait()
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
                  host_wall_seconds=time.monotonic() - began, performance_speedup=None)
    write(folder / 'result.json', result)
    assert passed, result
    return result



def context_gate(spec):
    gate = spec['context_gate']
    root, control = Path(gate['root']), Path(gate['control_root'])
    assert digest(root/'experiment.json') == gate['spec_sha256']
    build = json.loads((root/'build-identity.json').read_text())
    assert build['source'] == gate['source'] and build['binary_sha256'] == gate['binary_sha256']
    folder, before = root/gate['point'], control/gate['point']
    if not (folder/'result.json').exists():
        state = json.loads((root/'run-state.json').read_text())
        assert all(state[key] == gate['process'][key] for key in ['pid', 'start_ticks'])
        proc = Path('/proc', str(state['pid']), 'stat')
        assert proc.exists() and proc.read_text().rsplit(')',1)[1].split()[19] == state['start_ticks'], 'Inspect original context termination'
        return dict(pending=True, database_read=False)
    result = json.loads((folder/'result.json').read_text())
    process = json.loads((folder/'process.json').read_text())
    proc = Path('/proc',str(process['pid']),'stat')
    if proc.exists() and proc.read_text().rsplit(')',1)[1].split()[19] == process['start_ticks']:
        return dict(pending=True, database_read=False)
    assert result['process'] == process and result['original_process_closed']
    assert result['source'] == gate['source'] and result['binary_sha256'] == gate['binary_sha256']
    assert result['exitcode'] == 0 and result['difftest_passed'] and all(result['sampler_flags'].values())
    old = json.loads((before/'result.json').read_text())
    assert old['original_process_closed'] and old['exitcode'] == 0 and old['difftest_passed']
    assert old['source'] == 'ec964c8a623c8f72d32970cf984a8c8be8708d03'
    commands = [json.loads((path/'command.json').read_text()) for path in [before,folder]]
    for flag in ['-W','-I','-C','-s','-i','--diff','--cst-file','--dump-select-db']:
        assert commands[0][commands[0].index(flag)+1] == commands[1][commands[1].index(flag)+1]
    blocks = read_blocks(folder/'simulator_err.txt')
    names = {name for block in blocks.values() for name in block if name.rsplit('.',1)[-1] in FIELDS}
    assert len(names) == 16
    equality = old_execution(before,folder,names)
    assert equality['passed']
    phases = [dict(native_print_stamp=stamp, modules=predict_counts(block)) for stamp,block in sorted(blocks.items())]
    assert len(phases) == 2
    count = sum(module['counts']['context_mask6_cycles'] for phase in phases for module in phase['modules'])
    files = {str(path):dict(bytes=path.stat().st_size,sha256=digest(path)) for directory in [before,folder] for path in [directory/name for name in ['result.json','process.json','command.json','simulator_out.txt','simulator_err.txt','simulator.db']]}
    return dict(pending=False, supported=count>0, context_mask6_cycles=count, source=gate['source'], point=gate['point'], original_context_closed=True, old_native_six_tables_neutral=True, phases=phases, files=files, scope='Necessary SPEC context for testing a late-ready hypothesis. Nonflush accepted0/attempt1/permission1 is not sufficient proof of raw no-start, late state, criticality or saved cycles.')


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
        save(phase='waiting_context_gate', micro_gate_passed=True, micro_completed=len(rows))
        while True:
            context = context_gate(spec)
            if not context['pending']:
                break
            time.sleep(15)
        write(root/'context-opportunity.json', context)
        if not context['supported']:
            save(phase='complete', completed=0, context_gate_passed=False, short_skipped=True)
            return
        save(phase='short', context_gate_passed=True)
        with ThreadPoolExecutor(max_workers=3) as pool:
            results = list(pool.map(lambda case: completed_guest(root / case['point'], build, case), spec['short_cases']))
        write(root / 'short-results.json', results)
        save(phase='complete', completed=len(results), all_short_guests_passed=True)
    except BaseException as error:
        save(phase='failed', error=repr(error))
        raise


if __name__ == '__main__':
    main()
