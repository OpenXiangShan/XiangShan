"""Run one independent CI repeat, recording its actual process before waiting."""
from pathlib import Path
from datetime import datetime, timezone
import argparse
import hashlib
import json
import os
import re
import socket
import sqlite3
import subprocess
import sys
import time


TABLES = ['Tea_0', 'TeaDelayedFinal_0', 'TeaCycleSummary_0', 'TeaNoCommit_0', 'Dip_0', 'DipDelayedFinal_0']


def digest(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def write(path, value):
    temp = path.with_suffix('.tmp')
    temp.write_text(json.dumps(value, indent=2)+'\n'); temp.replace(path)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir', type=Path, required=True)
    parser.add_argument('--point', required=True)
    options = parser.parse_args(); root = options.run_dir
    spec = json.loads((root/'experiment.json').read_text())
    case = next(case for case in spec['cases'] if case['point'] == options.point)
    assert socket.gethostname().startswith('node')
    directory = root/'points'/case['point']; directory.mkdir(parents=True)
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 host=socket.gethostname(), stage=spec['stage'], point=case['point'])
    def save(**values):
        state.update(values, utc=datetime.now(timezone.utc).isoformat()); write(directory/'controller-state.json', state)
    try:
        save(phase='validating')
        for name in ['binary', 'reference', 'cst']:
            assert digest(spec[name]) == spec[name+'_sha256'], name
        assert digest(root/'native_perf.py') == spec['native_reader_sha256']
        sys.path.insert(0, str(root))
        from native_perf import read_blocks, unique_counters
        core = case['core']; assert core in os.sched_getaffinity(0)
        command = ['nice', '-n', '10', 'taskset', '-c', str(core), spec['binary'], '-I', str(spec['max_instr']),
                   '-W', str(spec['warmup']), '-C', str(spec['max_cycles']), '-s', str(case['seed']), '-i', case['image'],
                   '--diff', spec['reference'], '--dramsim3-outdir', str(directory), '--cst-file', spec['cst'],
                   '--dump-select-db', ' '.join(TABLES), '--db-path', str(directory/'simulator.db')]
        write(directory/'command.json', command); started = time.monotonic()
        with (directory/'simulator_out.txt').open('w') as stdout, (directory/'simulator_err.txt').open('w') as stderr:
            child = subprocess.Popen(command, cwd=directory, stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr)
            ticks = Path('/proc', str(child.pid), 'stat').read_text().rsplit(')', 1)[1].split()[19]
            process = dict(pid=child.pid, start_ticks=ticks, host=socket.gethostname(), core=core)
            write(directory/'process.json', process)
            write(directory/'launch-observation.json', dict(utc=datetime.now(timezone.utc).isoformat(), process=process,
                  original_process_live=True, controller=dict(state), command=command,
                  spec_sha256=digest(root/'experiment.json'), runner_sha256=digest(__file__)))
            save(phase='running', guest=process)
            rc = child.wait()
        (directory/'exitcode').write_text(str(rc)); assert rc == 0
        text = (directory/'simulator_out.txt').read_text()
        assert 'Difftest enabled' in text and 'EXCEEDING CYCLE/INSTR LIMIT' in text and 'HIT CRITICAL' not in text
        for key, value in [('enableTea0', 1), ('enableDip0', 1), ('teaSamplePeriod0', spec['period']), ('dipSamplePeriod0', spec['period']), ('streamL1Depth0', 64)]:
            assert re.search(r'^'+key+' = '+str(value)+'$', text, re.M)
        assert 'CPU_FREQ: 3000 DRAM_FREQ: 1600' in text
        assert re.search(r'^The image is (.+)$', text, re.M)[1] == case['image']
        blocks = read_blocks(directory/'simulator_err.txt'); assert len(blocks) == 2
        stamps = sorted(blocks); warm, roi = [unique_counters(blocks[stamp]) for stamp in stamps]
        def rob(block, name):
            values = [value for key, value in block.items() if key.endswith('.rob.'+name)]
            assert len(values) == 1; return values[0]
        cycles, instructions = rob(roi, 'clock_cycle'), rob(roi, 'commitInstr')
        assert cycles == stamps[1]-stamps[0]
        assert abs(rob(warm, 'commitInstr')-spec['warmup']) < 128 and abs(instructions-(spec['max_instr']-spec['warmup'])) < 128
        with sqlite3.connect((directory/'simulator.db').as_uri()+'?mode=ro', uri=True) as database:
            assert all(database.execute('SELECT count(*) FROM '+table).fetchone()[0] > 0 for table in TABLES)
        record = dict(**case, source=spec['source_identity']['source'], original_ci_run_id=spec['source_identity']['run_id'],
                      cycles=cycles, instructions=instructions, cpi=cycles/instructions, roi_start=stamps[0]+1, roi_end=stamps[1]+1,
                      elapsed_seconds=time.monotonic()-started, binary=spec['binary'], binary_sha256=spec['binary_sha256'],
                      reference_sha256=spec['reference_sha256'], cst_sha256=spec['cst_sha256'],
                      spec_sha256=digest(root/'experiment.json'), runner_sha256=digest(__file__), tea=True, dip=True, gsim=True)
        write(directory/'result.json', record); save(phase='complete')
    except BaseException as error:
        save(phase='failed', error=repr(error)); raise


if __name__ == '__main__':
    main()
