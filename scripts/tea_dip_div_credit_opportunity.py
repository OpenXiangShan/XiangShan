"""Observe, collect and validate the isolated FDIV credit opportunity diagnostic."""
from pathlib import Path
from datetime import datetime,timezone
import argparse,hashlib,json,os,re,shlex,subprocess,sys,tarfile,time


VARIANT='fdiv-credit-opportunity-observe'
CONTROL='fdiv-credit-opportunity-control'
STAGE='div-credit-opportunity-observation'
REMOTE='/nfs/home/wujiabin/work/xs-env/rtl-dip-experiments-20261103'
FILES=['command.json','process.json','launch-observation.json','result.json','counters.json','exitcode','simulator_out.txt','simulator_err.txt','simulator.db']


def digest(path):
    with path.open('rb') as stream:return hashlib.file_digest(stream,'sha256').hexdigest()


def request(code):
    reply=subprocess.run(['proxychains','-q','ssh','-o','BatchMode=yes','node031','python3','-'],input=code,text=True,capture_output=True,timeout=55,check=True)
    return json.loads(reply.stdout)


def snapshot(evidence):
    launch=json.loads((evidence/'launch.json').read_text())
    return request(r'''from pathlib import Path
from datetime import datetime,timezone
import json,os,socket
root=Path(ROOT);launch=LAUNCH;states=[];guests=[]
def observe(process):
 path=Path('/proc',str(process['pid']),'stat');fields=path.read_text().rsplit(')',1)[1].split() if path.exists() else None;row=dict(process,live=fields is not None and fields[19]==process['start_ticks'])
 if row['live']:
  row['cpu_ticks']=int(fields[11])+int(fields[12]);row['affinity']=sorted(os.sched_getaffinity(process['pid']))
 return row
for original in launch['children']:
 path=root/'fdiv-credit-opportunity-observe/build-state.json' if original['mode']=='build' else root/'div-credit-opportunity-observation'/original['mode']/'run-state.json'
 state=json.loads(path.read_text());assert state['pid']==original['pid'] and state['start_ticks']==original['start_ticks'] and state['host']==socket.gethostname();states.append(dict(mode=original['mode'],**observe(state)))
for path in (root/'div-credit-opportunity-observation').glob('*/**/process.json'):
 process=observe(json.loads(path.read_text()));process['directory']=str(path.parent)
 if process['live']:process['args']=Path('/proc',str(process['pid']),'cmdline').read_bytes().decode().split('\0')[:-1]
 guests.append(process)
print(json.dumps(dict(utc=datetime.now(timezone.utc).isoformat(),states=states,guests=guests,database_read=False)))
'''.replace('ROOT',repr(REMOTE)).replace('LAUNCH',repr(launch)))


def observe(evidence,watch):
    state=dict(pid=os.getpid(),start_ticks=Path('/proc/self/stat').read_text().rsplit(')',1)[1].split()[19],captures=0,observation_failures=0,source_sha256=digest(Path(__file__)))
    while True:
        try:
            event=snapshot(evidence);event['capture_passed']=True;exited=all(not row['live'] for row in event['states']);state['captures']+=1
        except (subprocess.SubprocessError,ValueError,KeyError,AssertionError) as error:
            event=dict(utc=datetime.now(timezone.utc).isoformat(),capture_passed=False,error=repr(error),database_read=False);exited=False;state['observation_failures']+=1
        with (evidence/'process-events.jsonl').open('a') as stream:stream.write(json.dumps(event)+'\n')
        state.update(utc=event['utc'],phase='original_processes_exited' if exited else 'observing',last_observation=event)
        path=evidence/'process-watch-state.json';temp=path.with_suffix('.tmp');temp.write_text(json.dumps(state,indent=2)+'\n');temp.replace(path)
        print(json.dumps({key:state[key] for key in ['utc','phase','captures','observation_failures']}),flush=True)
        if not watch or exited:return
        time.sleep(60)


def collect(root,evidence):
    observed={}
    for line in (evidence/'process-events.jsonl').read_text().splitlines():
        event=json.loads(line)
        for guest in event.get('guests',[]):
            if guest['live']:observed[guest['directory']]=dict(guest,observation_utc=event['utc'])
    result=request(r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json,socket
root=Path(ROOT);observed=OBSERVED;rows=[];pending=[];metadata={}
def live(process):
 path=Path('/proc',str(process['pid']),'stat')
 return path.exists() and path.read_text().rsplit(')',1)[1].split()[19]==process['start_ticks']
build_path=root/'fdiv-credit-opportunity-observe/build-state.json';build=json.loads(build_path.read_text())
if build['phase']=='complete' and not live(build):
 frozen=root/'div-credit-opportunity-observation/fdiv-credit-opportunity-observe/build-state.json'
 if not frozen.exists():frozen.write_bytes(build_path.read_bytes())
 assert frozen.read_bytes()==build_path.read_bytes();metadata['fdiv-credit-opportunity-observe/build-state.json']=frozen
smoke=root/'div-credit-opportunity-observation/fdiv-credit-opportunity-observe/coremark'
if (smoke/'result.json').exists():
 smoke_process=json.loads((smoke/'process.json').read_text())
 if not live(smoke_process) and (smoke/'exitcode').read_text().strip()=='0':
  for name in NAMES:
   path=smoke/name
   if path.exists():metadata['fdiv-credit-opportunity-observe/coremark/'+name]=path
meta_files={}
for name,path in metadata.items():
 with path.open('rb') as stream:sha=hashlib.file_digest(stream,'sha256').hexdigest()
 meta_files[name]=dict(bytes=path.stat().st_size,sha256=sha)
for variant in ['fdiv-credit-opportunity-observe','fdiv-credit-opportunity-control']:
 folder=root/'div-credit-opportunity-observation'/variant
 for directory in (folder/'points').glob('*'):
  old=observed.get(str(directory))
  if old is None:pending.append(dict(variant=variant,point=directory.name,reason='No original live observation',database_read=False));continue
  process=json.loads((directory/'process.json').read_text());assert process=={key:old[key] for key in ['pid','start_ticks','host','core']} and process['host']==socket.gethostname()
  if live(process) or not (directory/'result.json').exists():pending.append(dict(variant=variant,point=directory.name,reason='Original point not closed successfully',database_read=False));continue
  assert (directory/'exitcode').read_text().strip()=='0';files={}
  for name in NAMES:
   path=directory/name
   with path.open('rb') as stream:sha=hashlib.file_digest(stream,'sha256').hexdigest()
   files[name]=dict(bytes=path.stat().st_size,sha256=sha)
  rows.append(dict(utc=datetime.now(timezone.utc).isoformat(),variant=variant,point=directory.name,process=process,last_live_observation=old,original_process_live=False,exitcode='0',result=json.loads((directory/'result.json').read_text()),files=files))
print(json.dumps(dict(utc=datetime.now(timezone.utc).isoformat(),rows=rows,pending=pending,metadata_files=meta_files)))
'''.replace('ROOT',repr(REMOTE)).replace('OBSERVED',repr(observed)).replace('NAMES',repr(FILES)))
    stage=root/STAGE;manifest_path=stage/'manifest.json';manifest=json.loads(manifest_path.read_text()) if manifest_path.exists() else {};new={}
    for name,entry in result['metadata_files'].items():
        if name in manifest:assert manifest[name]==entry
        else:new[name]=entry
    for row in result['rows']:
        for name,entry in row['files'].items():
            key=row['variant']+'/points/'+row['point']+'/'+name
            if key in manifest:assert manifest[key]==entry,('Closed raw changed',key)
            else:new[key]=entry
    if new:
        archives=stage/'archives';archives.mkdir(parents=True,exist_ok=True);archive=archives/(datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%SZ')+'.tar.gz');command='tar -C '+shlex.quote(REMOTE+'/'+STAGE)+' -czf - '+' '.join(map(shlex.quote,new))
        with archive.open('wb') as stream:subprocess.run(['proxychains','-q','ssh','node031',command],stdout=stream,check=True,timeout=55)
        with tarfile.open(archive) as package:assert {member.name for member in package.getmembers()}==set(new);package.extractall(stage,filter='data')
        for name,entry in new.items():assert (stage/name).stat().st_size==entry['bytes'] and digest(stage/name)==entry['sha256']
        manifest.update(new);manifest_path.write_text(json.dumps(manifest,indent=2)+'\n')
    for row in result['rows']:
        for name,entry in row['files'].items():
            path=stage/row['variant']/'points'/row['point']/name;assert path.stat().st_size==entry['bytes'] and digest(path)==entry['sha256']
        receipt=stage/'closed-receipts'/(row['variant']+'-'+row['point']+'.json');receipt.parent.mkdir(parents=True,exist_ok=True)
        if not receipt.exists():receipt.write_text(json.dumps(row,indent=2)+'\n')
        else:
            old=json.loads(receipt.read_text());assert all(old[key]==row[key] for key in ['variant','point','process','files','result'])
    stage.mkdir(exist_ok=True);(stage/'collection-status.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(dict(closed=len(result['rows']),new_files=len(new),pending=result['pending']),indent=2))


def phase_counters(block,fields):
    from native_perf import unique_counters
    counters=unique_counters(block);suffix='.diag_div_credit_success_raw_entries';prefixes=sorted(key[:-len(suffix)] for key in counters if key.endswith(suffix));units=[];clock=next(value for name,value in counters.items() if name.endswith('.rob.clock_cycle'))
    for prefix in prefixes:
        queue=int(prefix.rsplit('issueQueues',1)[1].removeprefix('_') or '0');values={name:counters.get(prefix+'.'+name) for name in fields};present=all(value is not None for value in values.values());checks=None
        if present:
            def count(name):return values['diag_div_credit_'+name]
            checks=dict(nonnegative=all(value>=0 for value in values.values()),success_bounds=count('success_flushed_entries')<=count('success_valid_entries')<=count('success_raw_entries')<=18*clock,success_cycles=count('success_cycles')<=count('success_valid_entries') and count('success_cycles')<=clock,reuse_bounds=count('reused_entries')<=count('success_valid_entries') and count('reused_entries')<=count('transfer_entries'),reuse_partition=sum(count('reused_'+fu+'_entries') for fu in ['falu','fmul','fdiv','other'])==count('reused_entries'),transfer_gain_bounds=count('transfer_gain_entries')<=count('transfer_entries')<=2*clock,admission_bounds=count('admission_gain_with_input_cycles')<=count('admission_gain_cycles')<=clock,admission_input_bounds=count('admission_gain_with_input_cycles')<=count('admission_gain_input_entries')<=2*count('admission_gain_with_input_cycles') and count('admission_gain_input_entries')<=count('input_entries')<=2*clock)
        units.append(dict(queue=queue,values=values,all_fields_present=present,checks=checks,checks_passed=all(checks.values()) if checks else None))
    present=len(units)==2 and {unit['queue'] for unit in units}=={1,2} and all(unit['all_fields_present'] for unit in units)
    return dict(clock_cycles=clock,units=units,all_fields_present=present,checks_passed=all(unit['checks_passed'] for unit in units) if present else None)


def analyze(root,evidence):
    sys.path.insert(0,str(root))
    from analyze_windows import extract
    from analyze_queue_observation import compare_windows,verify_files
    from native_perf import read_blocks,counter_multisets
    source=json.loads((evidence/'source.json').read_text());spec=json.loads((evidence/VARIANT/'experiment.json').read_text());fields=set(source['new_exact_counter_names']);stage=root/STAGE;manifest_path=stage/'manifest.json';manifest=json.loads(manifest_path.read_text()) if manifest_path.exists() else {};rows=[];pending=[]
    for case in spec['cases']:
        point=case['point'];directory=stage/VARIANT/'points'/point;control=stage/CONTROL/'points'/point if point.startswith('bwaves_') else root/'raw/fp-credit-fdiv-only/short'/point
        if not (directory/'result.json').exists() or not (control/'result.json').exists():pending.append(point);continue
        receipt_path=stage/'closed-receipts'/(VARIANT+'-'+point+'.json');receipt=json.loads(receipt_path.read_text());assert not receipt['original_process_live'] and receipt['last_live_observation']['live'] and receipt['exitcode']=='0' and receipt['result']['source']==source['head']
        assert receipt['process']==json.loads((directory/'process.json').read_text())=={key:receipt['last_live_observation'][key] for key in ['pid','start_ticks','host','core']}
        build=json.loads((stage/VARIANT/'build-state.json').read_text());smoke=json.loads((stage/VARIANT/'coremark/result.json').read_text());assert build['phase']=='complete' and build['source']==source['head'] and receipt['result']['binary_sha256']==build['binary_sha256']==smoke['binary_sha256'] and smoke['full_old_native_and_six_complete_tables_neutral']
        verify_files(stage,{key:entry for key,entry in manifest.items() if key.startswith(VARIANT+'/points/'+point+'/') or key.startswith(VARIANT+'/coremark/') or key==VARIANT+'/build-state.json'})
        for name,entry in receipt['files'].items():assert manifest[VARIANT+'/points/'+point+'/'+name]==entry
        if point.startswith('bwaves_'):
            control_receipt=json.loads((stage/'closed-receipts'/(CONTROL+'-'+point+'.json')).read_text());assert not control_receipt['original_process_live'] and control_receipt['exitcode']=='0';verify_files(stage,{key:entry for key,entry in manifest.items() if key.startswith(CONTROL+'/points/'+point+'/')})
        else:verify_files(root/'raw',{key:entry for key,entry in json.loads((root/'raw/manifest.json').read_text()).items() if key.startswith('fp-credit-fdiv-only/short/'+point+'/')})
        control_identity=json.loads((control/'result.json').read_text());known_control=json.loads((root/'raw/fp-credit-fdiv-only/short/gromacs_105_0.206090/result.json').read_text());assert control_identity['source']==source['base'] and control_identity['binary_sha256']==known_control['binary_sha256']
        windows=[];pcs=[]
        for folder,label in [(control,'controlc4d'),(directory,VARIANT)]:
            command=json.loads((folder/'command.json').read_text())
            for flag,value in [('-s',str(case['seed'])),('-W',str(spec['warmup'])),('-I',str(spec['max_instr'])),('--diff',spec['reference']),('--dump-select-db','Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0')]:assert command[command.index(flag)+1]==value
            window,profile=extract(folder,label,spec['period']);windows.append(window);pcs.append(profile)
        native=[];names=[]
        for folder in [control,directory]:native.append({stamp:counter_multisets({key:value for key,value in block.items() if key.rsplit('.',1)[-1] not in fields}) for stamp,block in read_blocks(folder/'simulator_err.txt').items()})
        equality=compare_windows(*windows,control,directory,*pcs);equality['checks']['all_old_native_warmup_and_roi_equal']=native[0]==native[1];equality['neutral']=all(equality['checks'].values());equality['changed_old_native_names']=sorted({key for phase in native for block in phase.values() for key in block if [v.get(key) for v in native[0].values()]!=[v.get(key) for v in native[1].values()]})
        phases=[dict(phase=name,stamp=stamp,**phase_counters(block,fields)) for name,(stamp,block) in zip(['warmup','roi'],sorted(read_blocks(directory/'simulator_err.txt').items()))]
        rows.append(dict(point=point,neutrality=equality,phases=phases,diagnostic_valid=equality['neutral'] and all(phase['checks_passed'] is True for phase in phases),control=windows[0],observer=windows[1],closed_receipt_sha256=digest(receipt_path)))
    result=dict(completed=len(rows),expected=len(spec['cases']),rows=rows,pending=pending,all_collected_pairs_neutral=all(row['neutrality']['neutral'] for row in rows) if rows else None,hardware_validity=all(row['diagnostic_valid'] for row in rows) if len(rows)==len(spec['cases']) else None,performance_causality_validated=None,source=source['head'],consumer_sha256=digest(Path(__file__)),scope='Read-only15exactcounter names/2queues on c4d. Actual success/replacement versus same-state transfer/admission opportunity; request/FU partitions retained. Full old-native except15exactnames and seven execution contracts required. No CPUcriticality, unique producer, observableCPIgain or new performancecandidate claim.')
    out=stage/'analysis';out.mkdir(parents=True,exist_ok=True);(out/'evidence.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps({key:result[key] for key in ['completed','expected','pending','all_collected_pairs_neutral','hardware_validity']},indent=2))


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',type=Path,required=True);parser.add_argument('--action',choices=['observe','collect','analyze'],required=True);parser.add_argument('--watch',action='store_true');args=parser.parse_args();root=args.report_dir.resolve();evidence=root/'evidence/div-credit-opportunity-observe'
    for name,entry in json.loads((evidence/'source-launch-manifest.json').read_text()).items():assert (evidence/name).stat().st_size==entry['bytes'] and digest(evidence/name)==entry['sha256']
    if args.action=='observe':observe(evidence,args.watch)
    elif args.action=='collect':collect(root,evidence)
    else:analyze(root,evidence)


if __name__=='__main__':main()
