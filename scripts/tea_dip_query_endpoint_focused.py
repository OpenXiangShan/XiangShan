"""Run the preregistered query endpoint gain, negative and guard after the closed CoreMark gate."""
from concurrent.futures import ThreadPoolExecutor,as_completed
from datetime import datetime,timezone
from pathlib import Path
import argparse,hashlib,json,os,re,socket,subprocess


def digest(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream,'sha256').hexdigest()


def write(path,value):
    temporary=path.with_suffix('.tmp');temporary.write_text(json.dumps(value,indent=2)+'\n');temporary.replace(path)


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--gate',required=True,type=Path)
    args=parser.parse_args()
    gate=json.loads(args.gate.read_text())
    assert not gate['pending'] and gate['passed'] and gate['database_read'], 'Closed original CoreMark native/six-table gate is required'
    assert gate['consumer_sha256']==digest(Path(__file__).with_name('tea_dip_query_endpoint_coremark.py'))
    assert socket.gethostname().startswith('node032')
    assert gate['source']=='2915a6b022bd957570bd961a01611026c2220fb7' and gate['control_source']=='addd8cc4d35a5a3f5f3fa3cfe8371280ff787807'
    for input_set in gate['closed_input_sets']:
        for name,entry in input_set['files'].items():
            path=Path(input_set['root'])/name
            assert path.stat().st_size==entry['bytes'] and digest(path)==entry['sha256']
    environment=Path('/nfs/home/wujiabin/work/xs-env/rtl-dip-experiments-20261103')
    output=environment/'wb-query-endpoint-focused'
    assert not output.exists(), 'Observe the original focused validation instead of restarting'
    builds={name:json.loads((environment/path/'build-state.json').read_text()) for name,path in [('control_endpoint','tea-exact-endpoint-analysis-tools'),('query_endpoint','wb-query-endpoint-validation')]}
    assert builds['control_endpoint']['binary_sha256']==gate['control_binary_sha256'] and builds['query_endpoint']['binary_sha256']==gate['binary_sha256']
    assert all(build['phase']=='complete' and digest(build['binary'])==build['binary_sha256'] for build in builds.values())
    old=environment/'gate-observation-v2/gate-observe-v2/points'
    jobs=[('query_endpoint','gromacs_105_0.206090',9457,124),('query_endpoint','lbm_6382_0.375486',7058,125),('control_endpoint','sphinx3_157921_0.136728',4410,126),('query_endpoint','sphinx3_157921_0.136728',4410,127)]
    cases=[]
    for variant,point,seed,core in jobs:
        command=json.loads((old/point/'command.json').read_text())
        assert command[command.index('-I')+1]=='2000000' and command[command.index('-W')+1]=='1000000' and command[command.index('-C')+1]=='100000000' and command[command.index('-s')+1]==str(seed)
        assert command[command.index('--dump-select-db')+1]=='Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0'
        cases.append(dict(variant=variant,point=point,seed=seed,core=core,original_command=command,original_command_sha256=digest(old/point/'command.json'),reference_sha256=digest(command[command.index('--diff')+1]),cst_sha256=digest(command[command.index('--cst-file')+1])))
    output.mkdir()
    state=dict(pid=os.getpid(),start_ticks=Path('/proc/self/stat').read_text().rsplit(')',1)[1].split()[19],host=socket.gethostname(),phase='running',completed=0,total=4)
    write(output/'run-state.json',state)
    write(output/'experiment.json',dict(utc=datetime.now(timezone.utc).isoformat(),builds=builds,cases=cases,gate_sha256=digest(args.gate),gate_consumer_sha256=gate['consumer_sha256'],runner_sha256=digest(Path(__file__)),reused_controls={'gromacs_105_0.206090':str(environment/'tea-exact-endpoint-spec/points/gromacs_105_0.206090'),'lbm_6382_0.375486':str(environment/'tea-exact-endpoint-spec/points/lbm_6382_0.375486')},scope='Existingb0a candidate plus unchanged addd read-only diagnostics. Four GSIM+Difftest1M+1M attempts, original seeds/REF/CST/sixselectors, singlecore/nice10, no new FU/ports/resources/build/CI. Reuse the two closed addd controls and add missing exact-endpoint sphinx control. Offline full native/six-table versus original b0a and exact ROI state comparison required; no partial-suite score.'))

    def execute(case):
        build=builds[case['variant']];folder=output/case['variant']/case['point'];folder.mkdir(parents=True)
        command=case['original_command'].copy();command[command.index('-c')+1]=str(case['core']);command[6]=build['binary']
        for flag,value in [('--dramsim3-outdir',str(folder)),('--db-path',str(folder/'simulator.db'))]:command[command.index(flag)+1]=value
        write(folder/'command.json',command)
        with (folder/'simulator_out.txt').open('w') as stdout,(folder/'simulator_err.txt').open('w') as stderr:
            child=subprocess.Popen(command,cwd=folder,stdin=subprocess.DEVNULL,stdout=stdout,stderr=stderr)
            process=dict(pid=child.pid,start_ticks=Path('/proc',str(child.pid),'stat').read_text().rsplit(')',1)[1].split()[19],host=socket.gethostname(),core=case['core'])
            write(folder/'process.json',process);write(folder/'launch-observation.json',dict(**process,live=True,args=Path('/proc',str(child.pid),'cmdline').read_bytes().decode().split(chr(0))[:-1],utc=datetime.now(timezone.utc).isoformat()))
            print(json.dumps(dict(event='guest_started',variant=case['variant'],point=case['point'],**process)),flush=True)
            rc=child.wait()
        (folder/'exitcode').write_text(str(rc));text=(folder/'simulator_out.txt').read_text()
        flags={name:bool(re.search(r'^'+name+' = '+str(value)+'$',text,re.M)) for name,value in [('enableTea0',1),('enableDip0',1),('teaSamplePeriod0',1000),('dipSamplePeriod0',1000)]}
        passed=rc==0 and 'Difftest enabled' in text and 'Warmup finished' in text and 'EXCEEDING CYCLE/INSTR LIMIT' in text and all(flags.values()) and not any(marker in text for marker in ['ABORT','HIT CRITICAL','HIT BAD TRAP'])
        result=dict(variant=case['variant'],point=case['point'],source=build['source'],binary_sha256=build['binary_sha256'],process=process,original_process_closed=True,exitcode=rc,difftest_passed=passed,sampler_flags=flags,performance_cpi=None,performance_speedup=None)
        write(folder/'result.json',result);return result

    rows=[]
    with ThreadPoolExecutor(max_workers=4) as pool:
        for future in as_completed([pool.submit(execute,case) for case in cases]):
            rows.append(future.result());write(output/'results.json',rows);state.update(completed=len(rows),utc=datetime.now(timezone.utc).isoformat());write(output/'run-state.json',state)
    state.update(phase='complete',passed=all(row['difftest_passed'] for row in rows),utc=datetime.now(timezone.utc).isoformat());write(output/'run-state.json',state)


if __name__=='__main__':
    main()
