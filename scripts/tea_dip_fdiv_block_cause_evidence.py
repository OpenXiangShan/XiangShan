"""Collect closed FU-block-cause observers and audit their joint entry-cycle partition."""
from pathlib import Path
import argparse,json,re,shlex,subprocess,sys,tarfile
from tea_dip_fdiv_admission_evidence import admission_counts
from tea_dip_query_endpoint_evidence import old_execution,verify
from tea_dip_wb_protocol_micro import digest,write

SOURCE='825ec4d777955fa621bc6bb2547f6f1f515af8df'
PARENT='273f25d8a9a02a671a8eacaed4c61586fd932ed2'
FILES=['command.json','result.json','exitcode','process.json','launch-observation.json','simulator_out.txt','simulator_err.txt','simulator.db']


def metadata(root,point):
    evidence=root/'evidence/fdiv-ready-block-cause-observe'
    source=json.loads((evidence/'source.json').read_text());spec=json.loads((evidence/'experiment.json').read_text());launch=json.loads((evidence/'launch.json').read_text());build_launch=json.loads((evidence/'build-launch.json').read_text())
    assert source['head']==spec['source']==launch['source']==build_launch['source']==SOURCE
    assert source['base']==spec['parent_source']==launch['parent_source']==PARENT and source['source_reversal_byte_exact']
    assert source['new_exact_counter_names']==spec['new_exact_counter_names']and len(spec['new_exact_counter_names'])==3
    assert digest(evidence/'experiment.json')==launch['spec_sha256']and digest(evidence/'run.py')==launch['runner_sha256']
    for name,sha in source['source_sha256'].items():assert digest(evidence/'proposed'/name)==sha
    assert point in {c['point']for c in [spec['coremark'],*spec['short_cases']]}
    return evidence,spec,launch,build_launch


def collect(root,point):
    evidence,spec,launch,build_launch=metadata(root,point);stage=evidence/'closed-points'/point
    if(stage/'closure-receipt.json').exists():return json.loads((stage/'closure-receipt.json').read_text())
    code=r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json
spec=__SPEC__;launch=__LAUNCH__;build_launch=__BUILD__;point=__POINT__;root=Path(launch['run_dir']);folder=root/point
def sha(p):
 with p.open('rb')as stream:return hashlib.file_digest(stream,'sha256').hexdigest()
def live(row):
 p=Path('/proc',str(row['pid']),'stat');return p.exists()and p.read_text().rsplit(')',1)[1].split()[19]==row['start_ticks']
assert sha(root/'experiment.json')==launch['spec_sha256']and sha(root/'run.py')==launch['runner_sha256']and json.loads((root/'experiment.json').read_text())==spec
state=json.loads((root/'run-state.json').read_text());assert all(state[k]==launch['process'][k]for k in ['pid','start_ticks'])
build=json.loads(Path(spec['build_state']).read_text());assert build['source']==spec['source']and all(build[k]==build_launch['process'][k]for k in ['pid','start_ticks'])
if not(folder/'result.json').exists():print(json.dumps(dict(ready=False,point=point,controller=state,controller_live=live(state),build=build,original_build_live=live(build),database_read=False)))
else:
 process=json.loads((folder/'process.json').read_text());assert not live(process)
 result=json.loads((folder/'result.json').read_text())
 assert result['process']==process and result['original_process_closed']and result['exitcode']==0 and result['difftest_passed']and result['observation_neutrality']['passed']
 assert build['phase']=='complete'and result['source']==spec['source']and result['binary_sha256']==build['binary_sha256']
 assert result['spec_sha256']==launch['spec_sha256']and result['runner_sha256']==launch['runner_sha256']
 names=[point+'/'+n for n in __FILES__]+['experiment.json','run.py','native_perf.py','tea_dip_query_endpoint_evidence.py','parent-closed-inputs.json','launch.json']
 files={n:dict(bytes=(root/n).stat().st_size,sha256=sha(root/n))for n in names}
 print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),root=str(root),point=point,controller=state,controller_live=live(state),process=process,original_process_live=False,build=build,files=files,database_read=False)))
'''.replace('__SPEC__',repr(spec)).replace('__LAUNCH__',repr(launch)).replace('__BUILD__',repr(build_launch)).replace('__POINT__',repr(point)).replace('__FILES__',repr(FILES))
    p=subprocess.run(['proxychains','-q','ssh','-o','BatchMode=yes','node032','python3','-'],input=code,text=True,capture_output=True,check=True,timeout=50);receipt=json.loads(p.stdout)
    if not receipt['ready']:return receipt
    assert not stage.exists(),'Inspect partial archive before collecting';stage.mkdir(parents=True)
    command='tar -C '+shlex.quote(receipt['root'])+' -czf - '+' '.join(map(shlex.quote,receipt['files']))
    with(stage/'originals.tar.gz').open('wb')as stream:subprocess.run(['proxychains','-q','ssh','node032',command],stdout=stream,check=True,timeout=60)
    with tarfile.open(stage/'originals.tar.gz')as archive:
        assert{row.name for row in archive.getmembers()}==set(receipt['files']);archive.extractall(stage,filter='data')
    verify(stage,receipt['files']);write(stage/'closure-receipt.json',receipt);return receipt


def reason_counts(block,admission_fields,cause_fields,clocks):
    from native_perf import unique_counters
    rows=admission_counts(block,admission_fields,clocks);values={name:value for name,value in unique_counters(block).items()if name.rsplit('.',1)[-1]in cause_fields}
    assert len(values)==6,'Missing or ambiguous joint-cause fields'
    assert {name.rsplit('.',1)[0]for name in values}=={row['queue']for row in rows}
    for row in rows:
        counts={name.removeprefix('diag_fdiv_admission_'):values[row['queue']+'.'+name]for name in cause_fields};blocked=row['counts']['fu_blocked_entry_cycles']
        assert min(counts.values())>=0 and sum(counts.values())==blocked
        row['joint_cause_entry_cycles']=counts
        row['fraction_of_fu_blocked_entry_cycles']={name:value/blocked for name,value in counts.items()}if blocked else None
    return rows


def analyze(root,point):
    evidence,spec,launch,build_launch=metadata(root,point);stage=evidence/'closed-points'/point
    if not(stage/'closure-receipt.json').exists():return dict(pending=True,database_read=False,point=point)
    receipt=json.loads((stage/'closure-receipt.json').read_text());assert receipt['ready']and receipt['point']==point and not receipt['original_process_live']
    assert all(receipt['controller'][k]==launch['process'][k]for k in ['pid','start_ticks'])and all(receipt['build'][k]==build_launch['process'][k]for k in ['pid','start_ticks'])
    verify(stage,receipt['files']);assert digest(stage/'experiment.json')==launch['spec_sha256']and json.loads((stage/'launch.json').read_text())==launch
    assert json.loads((stage/'experiment.json').read_text())==spec and digest(stage/'run.py')==launch['runner_sha256']
    for name,key in [('native_perf.py','native_reader'),('tea_dip_query_endpoint_evidence.py','helper'),('parent-closed-inputs.json','parent_receipt')]:assert digest(stage/name)==spec[key+'_sha256']
    assert digest(root/'native_perf.py')==digest(stage/'native_perf.py')and digest(Path(__file__).with_name('tea_dip_query_endpoint_evidence.py'))==digest(stage/'tea_dip_query_endpoint_evidence.py')
    parent=json.loads((stage/'parent-closed-inputs.json').read_text());assert parent['ready']and not parent['original_controller_live']and parent['all_selected_original_guests_closed']and parent['source']==PARENT and parent['binary_sha256']==spec['parent_binary_sha256']
    parent_stage=root/'evidence/fdiv-ready-admission-observe/closed-points'/point;parent_receipt=json.loads((parent_stage/'closure-receipt.json').read_text());assert parent_receipt['ready']and not parent_receipt['original_process_live']and parent_receipt['build']['source']==PARENT
    assert all(parent['files'][n]==v for n,v in parent_receipt['files'].items());verify(parent_stage,parent_receipt['files'])
    folder=stage/point;result=json.loads((folder/'result.json').read_text());started=json.loads((folder/'launch-observation.json').read_text())
    assert result['source']==started['source']==receipt['build']['source']==SOURCE and result['binary_sha256']==started['binary_sha256']==receipt['build']['binary_sha256']
    assert result['process']==started['process']==receipt['process']and started['original_process_live']and result['original_process_closed']and result['exitcode']==0 and result['difftest_passed']
    assert result['spec_sha256']==launch['spec_sha256']and result['runner_sha256']==launch['runner_sha256']and(folder/'exitcode').read_text().strip()=='0'
    command=json.loads((folder/'command.json').read_text());assert command==started['command']and command[6]==receipt['build']['binary']
    case,=[c for c in [spec['coremark'],*spec['short_cases']]if c['point']==point]
    for flag,value in [('-I',str(case['max_instr'])),('-s',str(case['seed'])),('-i',case['image']),('--diff',spec['reference']),('--cst-file',spec['cst'])]:assert command[command.index(flag)+1]==value
    text=(folder/'simulator_out.txt').read_text();assert 'Difftest enabled'in text
    if case.get('warmup'):assert command[command.index('-W')+1]==str(case['warmup'])and 'Warmup finished'in text
    for name,value in [('enableTea0',1),('enableDip0',1),('teaSamplePeriod0',1000),('dipSamplePeriod0',1000)]:assert re.search(r'^'+name+' = '+str(value)+'$',text,re.M)
    sys.path.insert(0,str(root));from native_perf import read_blocks,unique_counters
    blocks=read_blocks(folder/'simulator_err.txt');old=read_blocks(parent_stage/point/'simulator_err.txt');added={n for block in blocks.values()for n in block}-{n for block in old.values()for n in block}
    assert len(added)==6 and{n.rsplit('.',1)[-1]for n in added}==set(spec['new_exact_counter_names'])and len(blocks)==(2 if case.get('warmup')else 1)
    admission_fields=json.loads((root/'evidence/fdiv-ready-admission-observe/source.json').read_text())['new_exact_counter_names'];phases=[]
    for label,(stamp,block)in zip(['warmup','roi']if case.get('warmup')else['coremark'],sorted(blocks.items())):
        clocks,=[value for name,value in unique_counters(block).items()if name.endswith('.rob.clock_cycle')]
        phases.append(dict(phase=label,native_print_stamp=stamp,native_clock_cycles=clocks,queues=reason_counts(block,admission_fields,spec['new_exact_counter_names'],clocks)))
    neutrality=old_execution(parent_stage/point,folder,added);assert neutrality['passed']
    return dict(pending=False,database_read=True,point=point,source=SOURCE,parent_source=PARENT,parent_native_six_complete_table_neutrality=neutrality,phases=phases,exact_added_native_names=sorted(added),consumer_sha256=digest(Path(__file__)),closed_receipt_sha256=digest(stage/'closure-receipt.json'),scope='Native interval entry-cycles used directly. Unit-only, IS0/IS1-only and both partition existing FU blocking per queue. Unit forecast includes current EX0 start exclusion. Not internal arithmetic phases, matched attempts/responses, unique or retired instructions, CPU critical time or recoverable CPI; cross-queue cycles overlap.')


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',required=True,type=Path);parser.add_argument('--point',required=True);parser.add_argument('--action',required=True,choices=['collect','analyze']);args=parser.parse_args();root=args.report_dir.resolve()
    result=collect(root,args.point)if args.action=='collect'else analyze(root,args.point)
    if args.action=='analyze'and not result['pending']:
        output=root/'evidence/fdiv-ready-block-cause-observe/closed-points'/args.point/'analysis';output.mkdir(parents=True,exist_ok=True);write(output/'evidence.json',result)
    print(json.dumps(result,indent=2))


if __name__=='__main__':main()
