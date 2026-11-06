"""Run a single CoreMark check after the original independent TEA endpoint build."""
from datetime import datetime, timezone
from pathlib import Path
import hashlib, json, os, re, socket, subprocess, time


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def write(path, value):
    temporary = path.with_suffix('.tmp')
    temporary.write_text(json.dumps(value, indent=2) + '\n')
    temporary.replace(path)


def main():
    assert socket.gethostname().startswith('node032')
    environment = Path('/nfs/home/wujiabin/work/xs-env/rtl-dip-experiments-20261103')
    output = environment / 'tea-exact-endpoint-analysis-tools'
    stage = output / 'coremark'
    state_path = output / 'smoke-state.json'
    assert not state_path.exists() and not stage.exists(), 'Observe the original attempt instead of restarting'
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19], host=socket.gethostname(), phase='waiting_build', runner_sha256=digest(Path(__file__)))
    write(state_path, state)
    try:
        while True:
            build = json.loads((output / 'build-state.json').read_text())
            assert build['source'] == 'addd8cc4d35a5a3f5f3fa3cfe8371280ff787807'
            if build['phase'] == 'complete':
                break
            assert build['phase'] == 'building'
            proc = Path('/proc', str(build['pid']), 'stat')
            assert proc.exists() and proc.read_text().rsplit(')', 1)[1].split()[19] == build['start_ticks']
            time.sleep(15)
        binary = Path(build['binary'])
        spec = json.loads((environment / 'experiment.json').read_text())
        assert digest(binary) == build['binary_sha256'] and digest(Path(spec['reference'])) == spec['reference_sha256']
        root = Path(build['root'])
        assert subprocess.check_output(['git', '-C', str(root), 'rev-parse', 'HEAD'], text=True).strip() == build['source']
        stage.mkdir()
        image = Path('/nfs/home/wujiabin/work/xs-env/XiangShan-wt-rtl-fpiq24-20261103/ready-to-run/coremark-2-iteration.bin')
        cst = environment / 'tea-dip-period1000.cst'
        selectors = 'Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0'
        command = ['nice', '-n', '10', 'taskset', '-c', '120', str(binary), '-I', '100000', '-C', '100000000', '-s', '99', '-i', str(image), '--diff', spec['reference'], '--dramsim3-outdir', str(stage), '--cst-file', str(cst), '--dump-select-db', selectors, '--db-path', str(stage / 'simulator.db')]
        write(stage / 'command.json', command)
        state.update(phase='coremark', source=build['source'], binary_sha256=build['binary_sha256'], image_sha256=digest(image), cst_sha256=digest(cst), utc=datetime.now(timezone.utc).isoformat())
        write(state_path, state)
        with (stage / 'simulator_out.txt').open('w') as stdout, (stage / 'simulator_err.txt').open('w') as stderr:
            child = subprocess.Popen(command, cwd=stage, stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr)
            process = dict(pid=child.pid, start_ticks=Path('/proc', str(child.pid), 'stat').read_text().rsplit(')', 1)[1].split()[19], host=socket.gethostname(), core=120)
            write(stage / 'process.json', process)
            write(stage / 'launch-observation.json', dict(**process, live=True, args=[x for x in Path('/proc', str(child.pid), 'cmdline').read_bytes().decode().split(chr(0)) if x], utc=datetime.now(timezone.utc).isoformat()))
            rc = child.wait()
        (stage / 'exitcode').write_text(str(rc))
        text = (stage / 'simulator_out.txt').read_text()
        assert rc == 0 and 'Difftest enabled' in text and 'EXCEEDING CYCLE/INSTR LIMIT' in text
        assert not any(marker in text for marker in ['HIT CRITICAL', 'ABORT', 'HIT BAD TRAP'])
        counts = re.search(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)', text)
        instructions, cycles = [int(v.replace(',', '')) for v in counts.groups()]
        assert 100000 <= instructions < 100128
        for name, value in [('enableTea0', 1), ('enableDip0', 1), ('teaSamplePeriod0', 1000), ('dipSamplePeriod0', 1000)]:
            assert re.search(r'^' + name + ' = ' + str(value) + '$', text, re.M)
        result = dict(source=build['source'], binary_sha256=build['binary_sha256'], process=process, original_process_closed=True, exitcode=rc, difftest_passed=True, instructions=instructions, cycles=cycles, functional_neutrality_validated=None, endpoint_semantics_validated=None, scope='One closed100kCoreMark execution; endpoint alignment and full-old-native/six-table neutrality need independent offline audit. No live database read, no FP coverage/performance adoption claim.')
        write(stage / 'result.json', result)
        state.update(phase='complete', utc=datetime.now(timezone.utc).isoformat())
        write(state_path, state)
        print(json.dumps(result), flush=True)
    except BaseException as error:
        state.update(phase='failed', error=repr(error), utc=datetime.now(timezone.utc).isoformat())
        write(state_path, state)
        raise


if __name__ == '__main__':
    main()
