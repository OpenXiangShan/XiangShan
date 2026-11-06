"""Compare two native dump boundaries with existing endpoint and 7efc binaries."""
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
from pathlib import Path
import hashlib, json, os, re, socket, subprocess


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
    output = environment / 'tea-exact-endpoint-two-dumps'
    assert not output.exists(), 'Observe original two-dump attempts instead of restarting'
    spec = json.loads((environment / 'experiment.json').read_text())
    builds = {'control7efc': json.loads((environment / 'gate-observe-v2/build-state.json').read_text()), 'endpoint': json.loads((environment / 'tea-exact-endpoint-analysis-tools/build-state.json').read_text())}
    assert builds['control7efc']['source'] == '7efc7832caaf32fda099e53528cc6badc7054cb6'
    assert builds['endpoint']['source'] == 'addd8cc4d35a5a3f5f3fa3cfe8371280ff787807'
    assert all(b['phase'] == 'complete' and digest(Path(b['binary'])) == b['binary_sha256'] for b in builds.values())
    assert digest(Path(spec['reference'])) == spec['reference_sha256']
    image = Path('/nfs/home/wujiabin/work/xs-env/XiangShan-wt-rtl-fpiq24-20261103/ready-to-run/coremark-2-iteration.bin')
    cst = environment / 'tea-dip-period1000.cst'
    selectors = 'Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0'
    output.mkdir()
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19], host=socket.gethostname(), phase='running', completed=0, total=2)
    write(output / 'run-state.json', state)
    write(output / 'experiment.json', dict(utc=datetime.now(timezone.utc).isoformat(), builds=builds, reference=spec['reference'], reference_sha256=spec['reference_sha256'], image=str(image), image_sha256=digest(image), cst=str(cst), cst_sha256=digest(cst), selectors=selectors, period=1000, seed=99, warmup=50000, max_instr=100000, max_cycles=100000000, cores=[120,121], runner_sha256=digest(Path(__file__)), scope='Two original CoreMark runs, unchanged binaries, two dumps each. Test local/native clock epoch mapping and retirement endpoint differences, not SPEC throughput, FP coverage or a hard-coded cycle correction.'))
    def execute(job):
        (variant, build), core = job
        folder = output / variant
        folder.mkdir()
        command = ['nice','-n','10','taskset','-c',str(core),build['binary'],'-I','100000','-W','50000','-C','100000000','-s','99','-i',str(image),'--diff',spec['reference'],'--dramsim3-outdir',str(folder),'--cst-file',str(cst),'--dump-select-db',selectors,'--db-path',str(folder/'simulator.db')]
        write(folder / 'command.json', command)
        with (folder/'simulator_out.txt').open('w') as stdout,(folder/'simulator_err.txt').open('w') as stderr:
            child = subprocess.Popen(command, cwd=folder, stdin=subprocess.DEVNULL, stdout=stdout, stderr=stderr)
            process = dict(pid=child.pid, start_ticks=Path('/proc', str(child.pid), 'stat').read_text().rsplit(')',1)[1].split()[19], host=socket.gethostname(),core=core)
            write(folder/'process.json',process)
            write(folder/'launch-observation.json',dict(**process,live=True,args=[a for a in Path('/proc',str(child.pid),'cmdline').read_bytes().decode().split(chr(0)) if a],utc=datetime.now(timezone.utc).isoformat()))
            print(json.dumps(dict(event='guest_started',variant=variant,**process)),flush=True)
            rc = child.wait()
        (folder/'exitcode').write_text(str(rc))
        text = (folder/'simulator_out.txt').read_text()
        counts = re.search(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)',text)
        samplers = {name:bool(re.search(r'^'+name+' = '+str(value)+'$',text,re.M)) for name,value in [('enableTea0',1),('enableDip0',1),('teaSamplePeriod0',1000),('dipSamplePeriod0',1000)]}
        passed = rc==0 and 'Difftest enabled' in text and 'EXCEEDING CYCLE/INSTR LIMIT' in text and 'Warmup finished' in text and all(samplers.values()) and not any(s in text for s in ['ABORT','HIT CRITICAL','HIT BAD TRAP'])
        result = dict(variant=variant,process=process,source=build['source'],binary_sha256=build['binary_sha256'],original_process_closed=True,exitcode=rc,difftest_passed=passed,sampler_flags=samplers,counts=[int(v.replace(',','')) for v in counts.groups()] if counts else None,performance_cpi=None,performance_speedup=None)
        write(folder/'result.json',result)
        return result
    rows=[]
    with ThreadPoolExecutor(max_workers=2) as pool:
        for future in as_completed([pool.submit(execute,job) for job in zip(builds.items(),[120,121])]):
            row=future.result();rows.append(row);write(output/'results.json',rows);state.update(completed=len(rows),utc=datetime.now(timezone.utc).isoformat());write(output/'run-state.json',state)
    state.update(phase='complete',passed=all(r['difftest_passed'] for r in rows),utc=datetime.now(timezone.utc).isoformat())
    write(output/'run-state.json',state)


if __name__=='__main__':
    main()
