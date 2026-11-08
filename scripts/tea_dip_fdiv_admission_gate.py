"""Coordinate the original admission observer from the login host over direct node SSH hops."""
from pathlib import Path
from datetime import datetime,timezone
import argparse,hashlib,json,os,subprocess,time
parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--run-dir',required=True,type=Path)
ROOT=parser.parse_args().run_dir.resolve();cfg=json.loads((ROOT/'config.json').read_text())
state=dict(pid=os.getpid(),start_ticks=Path('/proc/self/stat').read_text().rsplit(')',1)[1].split()[19],phase='waiting_original_parent',captures=0,observation_failures=0)
assert not(ROOT/'gate-state.json').exists()
def save(**values):
 state.update(values,utc=datetime.now(timezone.utc).isoformat());p=ROOT/'gate-state.tmp';p.write_text(json.dumps(state,indent=2)+'\n');p.replace(ROOT/'gate-state.json')
def sha(p):
 with Path(p).open('rb')as f:return hashlib.file_digest(f,'sha256').hexdigest()
def remote(host,code):
 result=subprocess.run(['proxychains','-q','ssh','-o','BatchMode=yes','-o','ConnectTimeout=15',host,'python3','-'],input=code,text=True,capture_output=True,timeout=45)
 if result.returncode==1:raise AssertionError(result.stderr[-1600:])
 result.check_returncode();return json.loads(result.stdout)
parent_code=r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
launch=__PARENT__;root=Path(launch['run_dir'])
def live(row):
 p=Path('/proc',str(row['pid']),'stat');return p.exists()and p.read_text().rsplit(')',1)[1].split()[19]==row['start_ticks']
def digest(p):
 with p.open('rb')as f:return hashlib.file_digest(f,'sha256').hexdigest()
assert digest(root/'experiment.json')==launch['spec_sha256']
spec=json.loads((root/'experiment.json').read_text());state=json.loads((root/'run-state.json').read_text())
assert spec==launch['spec']and spec['source']=='39a163eb3910ff9ecab02df4ac8e3c99d772feeb'
assert all(state[k]==launch['process'][k]for k in ['pid','start_ticks'])
if state['phase']=='failed':print(json.dumps(dict(ready=False,failed=True,state=state,original_controller_live=live(state),database_read=False)))
elif state['phase']!='complete'or live(state):print(json.dumps(dict(ready=False,failed=False,state=state,original_controller_live=live(state),database_read=False)))
else:
 assert state['completed']==3 and state['coremark_gate_passed']and state['micro_gate_passed']and state['micro_completed']==12 and state['all_short_guests_passed']
 build=json.loads((root/'build-identity.json').read_text());assert build['source']==spec['source']and build['phase']=='complete'and digest(Path(build['binary']))==build['binary_sha256']
 names=['experiment.json','run-state.json','build-identity.json','coremark-gate.json','short-results.json'];guests=[]
 for case in [spec['coremark'],*spec['short_cases']]:
  point=case['point'];folder=root/point;process=json.loads((folder/'process.json').read_text());row=json.loads((folder/'result.json').read_text())
  assert not live(process)and process==row['process']and row['original_process_closed']and row['difftest_passed']and row['exitcode']==0
  assert row['source']==spec['source']and row['binary_sha256']==build['binary_sha256'];guests.append(row)
  names.extend(point+'/'+n for n in ['command.json','result.json','process.json','launch-observation.json','exitcode','simulator_out.txt','simulator_err.txt','simulator.db'])
 files={name:dict(bytes=(root/name).stat().st_size,sha256=digest(root/name))for name in names}
 print(json.dumps(dict(ready=True,failed=False,utc=datetime.now(timezone.utc).isoformat(),root=str(root),controller=state,original_controller_live=False,all_selected_original_guests_closed=True,source=spec['source'],binary_sha256=build['binary_sha256'],guests=guests,files=files,database_read=False)))
'''.replace('__PARENT__',repr(cfg['parent_launch']))
build_code=r'''from pathlib import Path
import json
cfg=__CONFIG__;row=json.loads(Path(cfg['build_state']).read_text());process=cfg['observer_build_launch']['process']
assert row['source']==cfg['source']and all(row[k]==process[k]for k in ['pid','start_ticks'])
p=Path('/proc',str(row['pid']),'stat');live=p.exists()and p.read_text().rsplit(')',1)[1].split()[19]==row['start_ticks']
print(json.dumps(dict(build=row,original_build_live=live)))
'''.replace('__CONFIG__',repr(cfg))
launch_code=r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json,subprocess,socket
cfg=__CONFIG__;parent=__PARENT_RECEIPT__;files=__FILES__;out=Path(cfg['node_run_dir'])
assert socket.gethostname().startswith('node032')and not out.exists()
def sha(p):
 with Path(p).open('rb')as f:return hashlib.file_digest(f,'sha256').hexdigest()
for name,row in parent['files'].items():
 p=Path(parent['root'])/name;assert p.stat().st_size==row['bytes']and sha(p)==row['sha256']
assert parent['ready']and not parent['original_controller_live']and parent['source']==cfg['parent_source']
out.mkdir()
for name,text in files.items():assert hashlib.sha256(text.encode()).hexdigest()==cfg['input_hashes'][name];(out/name).write_text(text)
(out/'parent-closed-inputs.json').write_text(json.dumps(parent,indent=2)+'\n')
original=cfg['parent_launch']['spec'];cases=[]
for i,case in enumerate([original['coremark'],*original['short_cases']]):
 command=case['command'];value=dict(point=case['point'],core=164+(i-1 if i else 0),max_instr=int(command[command.index('-I')+1]),seed=int(command[command.index('-s')+1]),image=command[command.index('-i')+1],control=str(Path(parent['root'])/case['point']))
 if '-W'in command:value['warmup']=int(command[command.index('-W')+1])
 cases.append(value)
command=original['coremark']['command'];reference=command[command.index('--diff')+1];cst=command[command.index('--cst-file')+1]
spec=dict(source=cfg['source'],parent_source=parent['source'],parent_binary_sha256=parent['binary_sha256'],build_state=cfg['build_state'],node_run_dir=str(out),reference=reference,reference_sha256=sha(reference),cst=cst,cst_sha256=sha(cst),native_reader=str(out/'native_perf.py'),native_reader_sha256=sha(out/'native_perf.py'),helper=str(out/'tea_dip_query_endpoint_evidence.py'),helper_sha256=sha(out/'tea_dip_query_endpoint_evidence.py'),parent_receipt=str(out/'parent-closed-inputs.json'),parent_receipt_sha256=sha(out/'parent-closed-inputs.json'),new_exact_counter_names=cfg['new_exact_counter_names'],coremark=cases[0],short_cases=cases[1:])
(out/'experiment.json').write_text(json.dumps(spec,indent=2)+'\n')
command=['python3','-u',str(out/'run.py'),'--run-dir',str(out)]
with(out/'controller.stdout').open('w')as so,(out/'controller.stderr').open('w')as se:
 child=subprocess.Popen(command,cwd=out,stdin=subprocess.DEVNULL,stdout=so,stderr=se,start_new_session=True)
process=dict(pid=child.pid,start_ticks=Path('/proc',str(child.pid),'stat').read_text().rsplit(')',1)[1].split()[19],host=socket.gethostname())
launch=dict(utc=datetime.now(timezone.utc).isoformat(),run_dir=str(out),process=process,command=command,source=cfg['source'],parent_source=parent['source'],spec_sha256=sha(out/'experiment.json'),runner_sha256=sha(out/'run.py'),database_read=False)
(out/'launch.json').write_text(json.dumps(launch,indent=2)+'\n');print(json.dumps(launch))
'''
save(config_sha256=sha(ROOT/'config.json'))
try:
 for name,value in cfg['input_hashes'].items():assert sha(ROOT/name)==value,name
 while True:
  try:build=remote('node032',build_code);parent=remote('node031',parent_code)
  except(subprocess.SubprocessError,OSError,json.JSONDecodeError)as error:
   save(observation_failures=state['observation_failures']+1,last_observation_error=repr(error));time.sleep(20);continue
  save(captures=state['captures']+1,parent=parent,observer_build=build)
  assert build['build']['phase']!='failed',build
  assert build['build']['phase']=='complete'or build['original_build_live'],build
  assert not parent['failed'],'Original parent correctness validation failed; no observer launch'
  if parent['ready']:break
  time.sleep(20)
 files={name:(ROOT/name).read_text()for name in ['run.py','native_perf.py','tea_dip_query_endpoint_evidence.py']}
 launch=remote('node032',launch_code.replace('__CONFIG__',repr(cfg)).replace('__PARENT_RECEIPT__',repr(parent)).replace('__FILES__',repr(files)))
 (ROOT/'observer-launch.json').write_text(json.dumps(launch,indent=2)+'\n');save(phase='launched',observer=launch)
except BaseException as error:
 save(phase='failed',error=repr(error));raise
