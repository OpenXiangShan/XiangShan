"""Measure the existing query candidate's formal phases after the actual short-window neutrality gate."""
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
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--focused-evidence',required=True,type=Path);parser.add_argument('--focused-receipt',required=True,type=Path)
    args=parser.parse_args();gate=json.loads(args.focused_evidence.read_text());receipt=json.loads(args.focused_receipt.read_text())
    assert not gate['pending'] and gate['completed']==gate['expected']==3 and gate['all_observation_pairs_neutral'] and all(row['observation_neutrality_valid'] for row in gate['rows'])
    assert gate['consumer_sha256']==digest(Path(__file__).with_name('tea_dip_query_endpoint_evidence.py')) and gate['closed_receipt_sha256']==digest(args.focused_receipt)
    assert receipt['ready'] and not receipt['controller_live'] and receipt['all_original_guests_closed'] and receipt['controller']['pid']==1071055 and receipt['controller']['start_ticks']=='61850026'
    for name,entry in receipt['files'].items():
        path=Path(receipt['root'])/name;assert path.stat().st_size==entry['bytes'] and digest(path)==entry['sha256']
    assert socket.gethostname().startswith('node032')
    environment=Path('/nfs/home/wujiabin/work/xs-env/rtl-dip-experiments-20261103');output=environment/'wb-query-endpoint-formal'
    assert not output.exists(), 'Observe the original formal endpoint attempts instead of restarting'
    builds={variant:json.loads((environment/stage/'build-state.json').read_text()) for variant,stage in [('control_endpoint','tea-exact-endpoint-analysis-tools'),('query_endpoint','wb-query-endpoint-validation')]}
    assert builds['control_endpoint']['source']=='addd8cc4d35a5a3f5f3fa3cfe8371280ff787807' and builds['query_endpoint']['source']=='2915a6b022bd957570bd961a01611026c2220fb7'
    assert all(build['phase']=='complete' and digest(build['binary'])==build['binary_sha256'] for build in builds.values())
    cases=[]
    for point,seed in [('gromacs_105_0.206090',9457),('lbm_6382_0.375486',7058),('sphinx3_157921_0.136728',4410)]:
        original=environment/'wb-query-formal/fdiv-wb-query/points'/point;command=json.loads((original/'command.json').read_text());old=json.loads((original/'result.json').read_text())
        assert old['source']=='b0a22b93a867c5dff56b1b3fb32708c9e6363b01' and old['binary_sha256']=='401a3d8b0258a8d91fadf9fc1fae8dd207c6508938b146ff589f8f0f0247bb9e' and (original/'exitcode').read_text().strip()=='0'
        assert command[command.index('-W')+1]=='20000000' and command[command.index('-I')+1]=='40000000' and command[command.index('-C')+1]=='100000000' and command[command.index('-s')+1]==str(seed)
        assert command[command.index('--dump-select-db')+1]=='Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0'
        for variant in builds:
            cases.append(dict(variant=variant,point=point,seed=seed,core=124+len(cases),original_command=command,original_command_sha256=digest(original/'command.json'),original_result_sha256=digest(original/'result.json'),reference_sha256=digest(command[command.index('--diff')+1]),cst_sha256=digest(command[command.index('--cst-file')+1])))
    output.mkdir();state=dict(pid=os.getpid(),start_ticks=Path('/proc/self/stat').read_text().rsplit(')',1)[1].split()[19],host=socket.gethostname(),phase='running',completed=0,total=6)
    write(output/'run-state.json',state)
    write(output/'experiment.json',dict(utc=datetime.now(timezone.utc).isoformat(),builds=builds,cases=cases,focused_evidence_sha256=digest(args.focused_evidence),focused_receipt_sha256=digest(args.focused_receipt),gate_consumer_sha256=gate['consumer_sha256'],runner_sha256=digest(Path(__file__)),preregistered_hypotheses=['If g105 original formal gain persists, exact full Stalled decreases; compare old native/six tables to establish instrumentation neutrality before interpreting diagnostics.','If lbm weak formal regression persists, exact full state differences may show compensating causes unlike sampled DIP or interior-only states; retain any reversal and retirement gaps.','If sphinx short neutrality is phase-local, formal late changes can recur despite a neutral1M window; inspect full endpoint/phase evidence without inventing unique dynamic causality.'],scope='Six paired20M+20M g105/lbm/sphinx original formal seeds/REF/CST/100Mcyclecap/dual1000/sixselectors, singlecore/nice10, existing binaries and841functionalresources unchanged. Newly measured exact-endpoint controls are necessary because old formal controls lack terminal references. Require full old native/six-table neutrality versus original841/b0a formal archives, then separate exact states/edges/DIP/native units. No new CI or partial-suite/default claim.'))

    def execute(case):
        identity=builds[case['variant']];folder=output/case['variant']/case['point'];folder.mkdir(parents=True)
        command=case['original_command'].copy();command[command.index('-c')+1]=str(case['core']);command[6]=identity['binary']
        for flag,value in [('--dramsim3-outdir',str(folder)),('--db-path',str(folder/'simulator.db'))]:command[command.index(flag)+1]=value
        write(folder/'command.json',command)
        with (folder/'simulator_out.txt').open('w') as stdout,(folder/'simulator_err.txt').open('w') as stderr:
            child=subprocess.Popen(command,cwd=folder,stdin=subprocess.DEVNULL,stdout=stdout,stderr=stderr)
            process=dict(pid=child.pid,start_ticks=Path('/proc',str(child.pid),'stat').read_text().rsplit(')',1)[1].split()[19],host=socket.gethostname(),core=case['core'])
            write(folder/'process.json',process);write(folder/'launch-observation.json',dict(**process,live=True,args=Path('/proc',str(child.pid),'cmdline').read_bytes().decode().split(chr(0))[:-1],utc=datetime.now(timezone.utc).isoformat()))
            print(json.dumps(dict(event='guest_started',variant=case['variant'],point=case['point'],**process)),flush=True);rc=child.wait()
        (folder/'exitcode').write_text(str(rc));text=(folder/'simulator_out.txt').read_text();flags={name:bool(re.search(r'^'+name+' = '+str(value)+'$',text,re.M)) for name,value in [('enableTea0',1),('enableDip0',1),('teaSamplePeriod0',1000),('dipSamplePeriod0',1000)]}
        passed=rc==0 and 'Difftest enabled' in text and 'Warmup finished' in text and 'EXCEEDING CYCLE/INSTR LIMIT' in text and all(flags.values()) and not any(marker in text for marker in ['ABORT','HIT CRITICAL','HIT BAD TRAP'])
        result=dict(variant=case['variant'],point=case['point'],source=identity['source'],binary_sha256=identity['binary_sha256'],process=process,original_process_closed=True,exitcode=rc,difftest_passed=passed,sampler_flags=flags,performance_cpi=None,performance_speedup=None);write(folder/'result.json',result);return result

    rows=[]
    with ThreadPoolExecutor(max_workers=6) as pool:
        for future in as_completed([pool.submit(execute,case) for case in cases]):
            rows.append(future.result());write(output/'results.json',rows);state.update(completed=len(rows),utc=datetime.now(timezone.utc).isoformat());write(output/'run-state.json',state)
    state.update(phase='complete',passed=all(row['difftest_passed'] for row in rows),utc=datetime.now(timezone.utc).isoformat());write(output/'run-state.json',state)


if __name__=='__main__':
    main()
