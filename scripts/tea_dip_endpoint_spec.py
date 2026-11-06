"""Calibrate endpoint visibility on two existing SPEC controls without changing hardware."""
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
    output = environment/'tea-exact-endpoint-spec'
    assert not output.exists(), 'Observe original endpoint SPEC attempts instead of restarting'
    build = json.loads((environment/'tea-exact-endpoint-analysis-tools/build-state.json').read_text())
    assert build['phase'] == 'complete' and build['source'] == 'addd8cc4d35a5a3f5f3fa3cfe8371280ff787807' and digest(build['binary']) == build['binary_sha256']
    old = environment/'gate-observation-v2/gate-observe-v2/points'
    cases = []
    for point, core in [('gromacs_105_0.206090',120),('lbm_6382_0.375486',121)]:
        command = json.loads((old/point/'command.json').read_text())
        result = json.loads((old/point/'result.json').read_text())
        assert result['source'] == '7efc7832caaf32fda099e53528cc6badc7054cb6' and result['binary_sha256'] == '03489138cbd2c5ccc92b6f36ceb721b676f9537ce40ff6bd41c1aac37c547081' and (old/point/'exitcode').read_text().strip() == '0'
        assert command[command.index('-I')+1] == '2000000' and command[command.index('-W')+1] == '1000000'
        cases.append(dict(point=point,core=core,seed=result['seed'],control_directory=str(old/point),control_command_sha256=digest(old/point/'command.json'),control_result_sha256=digest(old/point/'result.json'),control_command=command,reference_sha256=digest(command[command.index('--diff')+1]),cst_sha256=digest(command[command.index('--cst-file')+1])))
    output.mkdir()
    state = dict(pid=os.getpid(),start_ticks=Path('/proc/self/stat').read_text().rsplit(')',1)[1].split()[19],host=socket.gethostname(),phase='running',completed=0,total=2)
    write(output/'run-state.json',state)
    write(output/'experiment.json',dict(utc=datetime.now(timezone.utc).isoformat(),build=build,cases=cases,period=1000,runner_sha256=digest(Path(__file__)),scope='Read-only endpoint instrumentation on baseline7efc hardware,1M+1M g105/lbm fixed original seeds9457/7058, unchanged inputs/REF/CST/sixselectors. Reuse completed original7efc controls; expected full old multivalue native and six complete tables neutrality. Validate paired native/TEA dump deltas per point and retain retirement offsets/edge states; no fixed correction or candidate performance claim.'))

    def execute(case):
        folder=output/'points'/case['point'];folder.mkdir(parents=True)
        command=case['control_command'].copy()
        command[command.index('-c')+1]=str(case['core'])
        command[6]=build['binary']
        for flag,value in [('--dramsim3-outdir',str(folder)),('--db-path',str(folder/'simulator.db'))]:
            command[command.index(flag)+1]=value
        write(folder/'command.json',command)
        with (folder/'simulator_out.txt').open('w') as stdout,(folder/'simulator_err.txt').open('w') as stderr:
            child=subprocess.Popen(command,cwd=folder,stdin=subprocess.DEVNULL,stdout=stdout,stderr=stderr)
            process=dict(pid=child.pid,start_ticks=Path('/proc',str(child.pid),'stat').read_text().rsplit(')',1)[1].split()[19],host=socket.gethostname(),core=case['core'])
            write(folder/'process.json',process)
            write(folder/'launch-observation.json',dict(**process,live=True,args=Path('/proc',str(child.pid),'cmdline').read_bytes().decode().split(chr(0))[:-1],utc=datetime.now(timezone.utc).isoformat()))
            print(json.dumps(dict(event='guest_started',point=case['point'],**process)),flush=True)
            rc=child.wait()
        (folder/'exitcode').write_text(str(rc))
        text=(folder/'simulator_out.txt').read_text()
        flags={name:bool(re.search(r'^'+name+' = '+str(value)+'$',text,re.M)) for name,value in [('enableTea0',1),('enableDip0',1),('teaSamplePeriod0',1000),('dipSamplePeriod0',1000)]}
        passed=rc==0 and all(flags.values()) and 'Difftest enabled' in text and 'Warmup finished' in text and 'EXCEEDING CYCLE/INSTR LIMIT' in text and not any(marker in text for marker in ['ABORT','HIT CRITICAL','HIT BAD TRAP'])
        result=dict(point=case['point'],source=build['source'],binary_sha256=build['binary_sha256'],process=process,original_process_closed=True,exitcode=rc,difftest_passed=passed,sampler_flags=flags,performance_speedup=None)
        write(folder/'result.json',result)
        return result

    rows=[]
    with ThreadPoolExecutor(max_workers=2) as pool:
        for future in as_completed([pool.submit(execute,case) for case in cases]):
            rows.append(future.result());write(output/'results.json',rows)
            state.update(completed=len(rows),utc=datetime.now(timezone.utc).isoformat());write(output/'run-state.json',state)
    state.update(phase='complete',passed=all(row['difftest_passed'] for row in rows),utc=datetime.now(timezone.utc).isoformat());write(output/'run-state.json',state)


if __name__=='__main__':
    main()
