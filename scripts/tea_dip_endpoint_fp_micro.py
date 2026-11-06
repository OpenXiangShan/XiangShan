"""Validate unchanged execution and exact TEA tail visibility on two existing FP programs."""
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
from pathlib import Path
import hashlib, json, os, re, socket, subprocess


def digest(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def write(path, value):
    temporary = path.with_suffix('.tmp')
    temporary.write_text(json.dumps(value, indent=2) + '\n')
    temporary.replace(path)


def main():
    assert socket.gethostname().startswith('node032')
    environment = Path('/nfs/home/wujiabin/work/xs-env/rtl-dip-experiments-20261103')
    output = environment / 'tea-exact-endpoint-fp-micro'
    assert not output.exists(), 'Observe the original attempts instead of restarting'
    prior = json.loads((environment / 'wb-query-recovery-micro/experiment.json').read_text())
    builds = {name: json.loads((environment / path / 'build-state.json').read_text()) for name, path in [('control7efc', 'gate-observe-v2'), ('endpoint', 'tea-exact-endpoint-analysis-tools')]}
    assert [build['source'] for build in builds.values()] == ['7efc7832caaf32fda099e53528cc6badc7054cb6', 'addd8cc4d35a5a3f5f3fa3cfe8371280ff787807']
    assert all(build['phase'] == 'complete' and digest(build['binary']) == build['binary_sha256'] for build in builds.values())
    assert all(digest(case['image']) == case['image_sha256'] for case in prior['cases'])
    assert digest(prior['reference']) == prior['reference_sha256']
    cst = environment / 'tea-dip-period1000.cst'
    assert digest(cst) == prior['cst_sha256']
    output.mkdir()
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19], host=socket.gethostname(), phase='running', completed=0, total=4)
    write(output / 'run-state.json', state)
    spec = dict(utc=datetime.now(timezone.utc).isoformat(), builds=builds, cases=prior['cases'], reference=prior['reference'], reference_sha256=prior['reference_sha256'], cst=str(cst), cst_sha256=digest(cst), seed=99, period=1000, selectors=prior['selectors'], cores=[120,121,122,123], runner_sha256=digest(Path(__file__)), scope='Same7efc hardware before/after read-only endpoint references, two existing FP programs, no new RTL/binary/resources or performance candidate. Compare every old native multivalue and six complete tables after original closure; TEA/native/trap endpoints remain separate.')
    write(output / 'experiment.json', spec)

    def execute(job):
        (variant, build, case), core = job
        folder = output / 'runs' / variant / case['case']
        folder.mkdir(parents=True)
        command = ['nice','-n','10','taskset','-c',str(core),build['binary'],'-I','100000','-C','200000','-s','99','-i',case['image'],'--diff',prior['reference'],'--dramsim3-outdir',str(folder),'--cst-file',str(cst),'--dump-select-db',prior['selectors'],'--db-path',str(folder/'simulator.db')]
        write(folder / 'command.json', command)
        with (folder/'simulator_out.txt').open('w') as stdout, (folder/'simulator_err.txt').open('w') as stderr:
            child = subprocess.Popen(command, cwd=folder, stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr)
            process = dict(pid=child.pid, start_ticks=Path('/proc',str(child.pid),'stat').read_text().rsplit(')',1)[1].split()[19], host=socket.gethostname(), core=core)
            write(folder/'process.json', process)
            write(folder/'launch-observation.json', dict(**process, live=True, args=Path('/proc',str(child.pid),'cmdline').read_bytes().decode().split(chr(0))[:-1], utc=datetime.now(timezone.utc).isoformat()))
            print(json.dumps(dict(event='guest_started',variant=variant,case=case['case'],**process)),flush=True)
            rc = child.wait()
        (folder/'exitcode').write_text(str(rc))
        text = re.sub(r'\x1b\[[0-9;]*m','',(folder/'simulator_out.txt').read_text())
        trap = re.search(r'HIT GOOD TRAP at pc = (0x[0-9a-fA-F]+)',text)
        counts = re.search(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)',text)
        flags = {name:bool(re.search(r'^'+name+' = '+str(value)+'$',text,re.M)) for name,value in [('enableTea0',1),('enableDip0',1),('teaSamplePeriod0',1000),('dipSamplePeriod0',1000)]}
        passed = rc == 0 and 'Difftest enabled' in text and trap is not None and int(trap[1],16) == int(case['pass_pc'],16) and all(flags.values()) and not any(marker in text for marker in ['ABORT','HIT CRITICAL','HIT BAD TRAP','EXCEEDING CYCLE/INSTR LIMIT'])
        result = dict(variant=variant,case=case['case'],source=build['source'],binary_sha256=build['binary_sha256'],image_sha256=case['image_sha256'],process=process,original_process_closed=True,exitcode=rc,difftest_passed=passed,sampler_flags=flags,trap_counts=[int(v.replace(',','')) for v in counts.groups()] if counts else None,performance_speedup=None)
        write(folder/'result.json',result)
        return result

    jobs = [(variant,build,case) for variant,build in builds.items() for case in prior['cases']]
    rows = []
    with ThreadPoolExecutor(max_workers=4) as pool:
        for future in as_completed([pool.submit(execute, job) for job in zip(jobs, spec['cores'])]):
            rows.append(future.result())
            write(output/'results.json', rows)
            state.update(completed=len(rows), utc=datetime.now(timezone.utc).isoformat())
            write(output/'run-state.json', state)
    state.update(phase='complete', passed=all(row['difftest_passed'] for row in rows), utc=datetime.now(timezone.utc).isoformat())
    write(output/'run-state.json', state)


if __name__ == '__main__':
    main()
