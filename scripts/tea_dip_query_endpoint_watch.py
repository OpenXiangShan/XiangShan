"""Continue the original query validation through closure, neutral CoreMark and focused launch."""
from datetime import datetime,timezone
from pathlib import Path
import argparse,hashlib,importlib.util,json,os,shlex,subprocess,tarfile,time


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream,'sha256').hexdigest()


def write(path,value):
    temporary=path.with_suffix('.tmp');temporary.write_text(json.dumps(value,indent=2)+'\n');temporary.replace(path)


def remote(code):
    result=subprocess.run(['proxychains','-q','ssh','-o','BatchMode=yes','-o','ConnectTimeout=15','node032','python3','-'],input=code,text=True,capture_output=True,timeout=50,check=True)
    return json.loads(result.stdout)


def observe():
    return remote(r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json,subprocess
root=Path('/nfs/home/wujiabin/work/xs-env/rtl-dip-experiments-20261103/wb-query-endpoint-validation')
build=json.loads((root/'build-state.json').read_text());smoke=json.loads((root/'smoke-state.json').read_text())
assert build['pid']==431191 and build['start_ticks']=='61520314' and smoke['pid']==444711 and smoke['start_ticks']=='61524832'
def live(row):
 p=Path('/proc',str(row['pid']),'stat');return p.exists() and p.read_text().rsplit(')',1)[1].split()[19]==row['start_ticks']
if build['phase']=='failed' or smoke['phase']=='failed':print(json.dumps(dict(ready=False,failed=True,build=build,smoke=smoke,database_read=False)))
elif build['phase']!='complete' or smoke['phase']!='complete' or live(build) or live(smoke):
 print(json.dumps(dict(ready=False,failed=False,build=build,smoke=smoke,build_live=live(build),smoke_live=live(smoke),database_read=False)))
else:
 guest=json.loads((root/'coremark/process.json').read_text());launch=json.loads((root/'coremark/launch-observation.json').read_text());result=json.loads((root/'coremark/result.json').read_text());assert not live(guest) and guest==result['process'] and result['original_process_closed'] and result['exitcode']==0 and launch['live'] and all(launch[key]==value for key,value in guest.items())
 assert build['source']==result['source']=='2915a6b022bd957570bd961a01611026c2220fb7'
 with Path(build['binary']).open('rb') as stream:assert hashlib.file_digest(stream,'sha256').hexdigest()==build['binary_sha256']==result['binary_sha256']
 files={}
 for path in root.rglob('*'):
  if path.is_file():
   with path.open('rb') as stream:sha=hashlib.file_digest(stream,'sha256').hexdigest()
   files[str(path.relative_to(root))]=dict(bytes=path.stat().st_size,sha256=sha)
 print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),root=str(root),build=build,smoke=smoke,guest=guest,original_build_controller_live=False,original_smoke_controller_live=False,original_guest_live=False,files=files,source_head=subprocess.check_output(['git','-C',build['root'],'rev-parse','HEAD'],text=True).strip())))
''')


def archive(root,receipt):
    target=root/'wb-query-endpoint-validation';target.mkdir()
    package=target/'closed-originals.tar.gz'
    command='tar -C '+shlex.quote(receipt['root'])+' -czf - '+' '.join(map(shlex.quote,receipt['files']))
    with package.open('wb') as stream:
        subprocess.run(['proxychains','-q','ssh','-o','BatchMode=yes','node032',command],stdout=stream,check=True,timeout=50)
    with tarfile.open(package) as bundle:
        assert {entry.name for entry in bundle.getmembers()}==set(receipt['files']);bundle.extractall(target,filter='data')
    for name,entry in receipt['files'].items():
        path=target/name;assert path.stat().st_size==entry['bytes'] and digest(path)==entry['sha256']
    write(target/'closure-receipt.json',receipt)


def launch(gate,consumer_sha,runner_sha):
    return remote(r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json,subprocess
root=Path('/nfs/home/wujiabin/work/xs-env/rtl-dip-experiments-20261103');tree=root.parent/'XiangShan-wt-rtl-tea-dip-tools-v2-20261104/scripts';gate=GATE
assert gate['passed'] and not gate['pending'] and gate['consumer_sha256']==CONSUMER_SHA
assert hashlib.sha256((tree/'tea_dip_query_endpoint_coremark.py').read_bytes()).hexdigest()==CONSUMER_SHA
assert hashlib.sha256((tree/'tea_dip_query_endpoint_focused.py').read_bytes()).hexdigest()==RUNNER_SHA
path=root/'wb-query-endpoint-focused-launch.json'
if path.exists():
 record=json.loads(path.read_text());assert record['consumer_sha256']==CONSUMER_SHA and record['runner_sha256']==RUNNER_SHA
 print(json.dumps(record))
else:
 assert not (root/'wb-query-endpoint-focused').exists()
 gate_path=root/'wb-query-endpoint-validation/coremark-gate.json';gate_path.write_text(json.dumps(gate,indent=2)+chr(10))
 with (root/'wb-query-endpoint-focused-controller.stdout').open('w') as stdout,(root/'wb-query-endpoint-focused-controller.stderr').open('w') as stderr:
  child=subprocess.Popen(['nice','-n','10','python3',str(tree/'tea_dip_query_endpoint_focused.py'),'--gate',str(gate_path)],cwd=root,stdin=subprocess.DEVNULL,stdout=stdout,stderr=stderr,start_new_session=True)
  record=dict(utc=datetime.now(timezone.utc).isoformat(),pid=child.pid,start_ticks=Path('/proc',str(child.pid),'stat').read_text().rsplit(')',1)[1].split()[19],live=True,args=Path('/proc',str(child.pid),'cmdline').read_bytes().decode().split(chr(0))[:-1],consumer_sha256=CONSUMER_SHA,runner_sha256=RUNNER_SHA,gate_sha256=hashlib.sha256(gate_path.read_bytes()).hexdigest())
 path.write_text(json.dumps(record,indent=2)+chr(10));print(json.dumps(record))
'''.replace('GATE',repr(gate)).replace('CONSUMER_SHA',repr(consumer_sha)).replace('RUNNER_SHA',repr(runner_sha)))


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',required=True,type=Path)
    args=parser.parse_args();root=args.report_dir.resolve();evidence=root/'evidence/query-endpoint-validation';state_path=evidence/'original-build-gate-watch-state.json'
    assert not state_path.exists(), 'Observe the existing original controller instead of restarting it'
    state=dict(pid=os.getpid(),start_ticks=Path('/proc/self/stat').read_text().rsplit(')',1)[1].split()[19],phase='observing_original_build',captures=0,observation_failures=0,source_sha256=digest(Path(__file__)))
    consumer=Path(__file__).with_name('tea_dip_query_endpoint_coremark.py');runner=Path(__file__).with_name('tea_dip_query_endpoint_focused.py')
    state.update(consumer_sha256=digest(consumer),runner_sha256=digest(runner));write(state_path,state)
    while True:
        try:observation=observe()
        except (subprocess.SubprocessError,OSError,json.JSONDecodeError) as error:
            state.update(observation_failures=state['observation_failures']+1,last_observation_error=repr(error),utc=datetime.now(timezone.utc).isoformat());write(state_path,state);time.sleep(30);continue
        state.update(captures=state['captures']+1,last_observation=observation,utc=datetime.now(timezone.utc).isoformat());write(state_path,state)
        if observation.get('failed'):
            state.update(phase='original_build_or_smoke_failed');write(state_path,state);return
        if observation['ready']:break
        time.sleep(30)
    archive(root,observation)
    specification=importlib.util.spec_from_file_location('query_endpoint_gate',consumer);module=importlib.util.module_from_spec(specification);specification.loader.exec_module(module)
    gate=module.analyze(root);output=root/'wb-query-endpoint-validation/analysis';output.mkdir();write(output/'coremark-gate.json',gate)
    state.update(phase='coremark_gate_passed' if gate['passed'] else 'coremark_gate_rejected',gate_sha256=digest(output/'coremark-gate.json'));write(state_path,state)
    if not gate['passed']:return
    while True:
        try:record=launch(gate,state['consumer_sha256'],state['runner_sha256']);break
        except (subprocess.SubprocessError,OSError,json.JSONDecodeError) as error:
            state.update(last_launch_observation_error=repr(error),utc=datetime.now(timezone.utc).isoformat());write(state_path,state);time.sleep(30)
    write(evidence/'focused-launch.json',record);state.update(phase='focused_controller_started',focused_launch=record,utc=datetime.now(timezone.utc).isoformat());write(state_path,state)


if __name__=='__main__':
    main()
