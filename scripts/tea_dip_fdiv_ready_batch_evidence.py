"""Collect closed FDIV-ready batch points independently and analyze only validated pairs."""
from pathlib import Path
import argparse,json,re,shlex,subprocess,sys,tarfile
from tea_dip_query_endpoint_evidence import endpoint,old_execution,pair,verify
from tea_dip_wb_protocol_micro import digest,write

FILES=['command.json','result.json','exitcode','process.json','launch-observation.json','simulator_out.txt','simulator_err.txt','simulator.db']
TOOLS=['tea_dip_fdiv_ready_batch.py','tea_dip_fdiv_ready_run.py','tea_dip_wb_protocol_micro.py','tea_dip_query_endpoint_evidence.py','native_perf.py']
SOURCES={'control':'2915a6b022bd957570bd961a01611026c2220fb7','candidate':'39a163eb3910ff9ecab02df4ac8e3c99d772feeb'}


def metadata(root,batch,variant,point,turnover=False):
    evidence=root/'evidence'/('fdiv-turnover-extended-20261108'if turnover else'fdiv-ready-extended')
    sources={'control':'39a163eb3910ff9ecab02df4ac8e3c99d772feeb','candidate':'8f8ece60ac8b08b358743bc9db15a5a227dde0b8'}if turnover else SOURCES
    launch=json.loads((evidence/('fdiv-'+('turnover-'if turnover else'ready-')+batch+'-launch.json')).read_text());spec=launch['spec']
    assert all(spec['builds'][v]['source']==s for v,s in sources.items())
    case,=[c for c in spec['cases']if c['point']==point and c['variant']==variant]
    assert launch['runner_sha256']==digest(evidence/'tea_dip_fdiv_ready_batch.py')
    return launch,case,root/(Path(launch['run_dir']).name+'-points')/variant/point


def collect(root,batch,variant,point,turnover=False):
    launch,case,stage=metadata(root,batch,variant,point,turnover)
    if(stage/'closure-receipt.json').exists():return json.loads((stage/'closure-receipt.json').read_text())
    host=launch['process']['host'].split('.')[0]
    code=r'''from datetime import datetime,timezone
from pathlib import Path
import hashlib,json
launch=__LAUNCH__;case=__CASE__;root=Path(launch['run_dir']);folder=root/case['variant']/case['point']
def sha(p):
 with p.open('rb')as stream:return hashlib.file_digest(stream,'sha256').hexdigest()
def live(row):
 p=Path('/proc',str(row['pid']),'stat');return p.exists()and p.read_text().rsplit(')',1)[1].split()[19]==row['start_ticks']
assert sha(root/'experiment.json')==launch['spec_sha256']and json.loads((root/'experiment.json').read_text())==launch['spec']
assert sha(root/'tea_dip_fdiv_ready_batch.py')==launch['runner_sha256']
state=json.loads((root/'run-state.json').read_text());assert all(state[k]==launch['process'][k]for k in ['pid','start_ticks'])
if not(folder/'result.json').exists():print(json.dumps(dict(ready=False,point=case['point'],variant=case['variant'],controller=state,controller_live=live(state),database_read=False)))
else:
 process=json.loads((folder/'process.json').read_text())
 if live(process):print(json.dumps(dict(ready=False,point=case['point'],variant=case['variant'],original_process_live=True,database_read=False)))
 else:
  result=json.loads((folder/'result.json').read_text());build=launch['spec']['builds'][case['variant']]
  assert result['process']==process and result['original_process_closed']and result['source']==build['source']and result['binary_sha256']==build['binary_sha256']
  names=[str(Path(case['variant'])/case['point']/n)for n in __FILES__]+['experiment.json','short-gate.json',*__TOOLS__]
  files={n:dict(bytes=(root/n).stat().st_size,sha256=sha(root/n))for n in names}
  print(json.dumps(dict(ready=True,utc=datetime.now(timezone.utc).isoformat(),root=str(root),point=case['point'],variant=case['variant'],controller=state,controller_live=live(state),original_process_live=False,process=process,files=files,database_read=False)))
'''.replace('__LAUNCH__',repr(launch)).replace('__CASE__',repr(case)).replace('__FILES__',repr(FILES)).replace('__TOOLS__',repr(TOOLS))
    reply=subprocess.run(['proxychains','-q','ssh','-o','BatchMode=yes',host,'python3','-'],input=code,text=True,capture_output=True,check=True,timeout=50)
    receipt=json.loads(reply.stdout)
    if not receipt['ready']:return receipt
    assert not stage.exists(),'Inspect partial archive before collecting'
    stage.mkdir(parents=True)
    write(stage/'collection-receipt.json',receipt)
    command='tar -C '+shlex.quote(receipt['root'])+' -czf - '+' '.join(map(shlex.quote,receipt['files']))
    with(stage/'originals.tar.gz').open('wb')as stream:subprocess.run(['proxychains','-q','ssh',host,command],stdout=stream,check=True,timeout=60)
    with tarfile.open(stage/'originals.tar.gz')as archive:
        assert{row.name for row in archive.getmembers()}==set(receipt['files']);archive.extractall(stage,filter='data')
    verify(stage,receipt['files']);write(stage/'closure-receipt.json',receipt)
    return receipt


def closed(root,batch,variant,point,turnover=False):
    if turnover and variant=='control'and batch=='guards':
        launch,_,_=metadata(root,batch,'candidate',point,True)
        item=closed(root,batch,'candidate',point)
        assert item is not None and item['valid']and item['receipt_sha256']==launch['spec']['reused_control_receipts'][point]
        assert item['result']['source']==launch['spec']['builds']['control']['source']and item['result']['binary_sha256']==launch['spec']['builds']['control']['binary_sha256']
        return item
    launch,case,stage=metadata(root,batch,variant,point,turnover)
    if not(stage/'closure-receipt.json').exists():return None
    receipt=json.loads((stage/'closure-receipt.json').read_text());assert receipt['ready']and not receipt['original_process_live']
    assert receipt['point']==point and receipt['variant']==variant and all(receipt['controller'][k]==launch['process'][k]for k in ['pid','start_ticks'])
    verify(stage,receipt['files']);assert digest(stage/'experiment.json')==launch['spec_sha256']
    assert json.loads((stage/'experiment.json').read_text())==launch['spec']
    for name in TOOLS:assert digest(stage/name)==launch['spec']['input_hashes'][str(Path(launch['run_dir'])/name)]
    assert digest(root/'native_perf.py')==digest(stage/'native_perf.py')
    assert digest(Path(__file__).with_name('tea_dip_query_endpoint_evidence.py'))==digest(stage/'tea_dip_query_endpoint_evidence.py')
    gate=json.loads((stage/'short-gate.json').read_text());assert digest(stage/'short-gate.json')==launch['spec']['short_gate_sha256']and gate['validation_passed']and not gate['pending']
    folder=stage/variant/point;result=json.loads((folder/'result.json').read_text());started=json.loads((folder/'launch-observation.json').read_text());build=launch['spec']['builds'][variant]
    assert result['source']==started['source']==build['source']and result['binary_sha256']==started['binary_sha256']==build['binary_sha256']
    assert result['process']==started['process']==receipt['process']and started['original_process_live']and result['original_process_closed']
    command=json.loads((folder/'command.json').read_text());assert command==started['command']and command[6]==build['binary']
    for flag in ['-I','-W','-C','-s','-i','--diff','--cst-file','--dump-select-db']:
        assert command[command.index(flag)+1]==case['command'][case['command'].index(flag)+1]
    exitcode=int((folder/'exitcode').read_text());assert exitcode==result['exitcode']
    text=(folder/'simulator_out.txt').read_text()
    valid=exitcode==0 and result['difftest_passed']and all(result['sampler_flags'].values())and 'Difftest enabled'in text and 'Warmup finished'in text and 'EXCEEDING CYCLE/INSTR LIMIT'in text
    valid=valid and not any(s in text for s in ['ABORT','HIT CRITICAL','HIT BAD TRAP'])
    if valid:
        for name,value in [('enableTea0',1),('enableDip0',1),('teaSamplePeriod0',1000),('dipSamplePeriod0',1000)]:assert re.search(r'^'+name+' = '+str(value)+'$',text,re.M)
    return dict(folder=folder,case=case,result=result,valid=bool(valid),receipt_sha256=digest(stage/'closure-receipt.json'))


def analyze(root,batch,variant,point,turnover=False):
    item=closed(root,batch,variant,point,turnover)
    if item is None:return dict(pending=True,database_read=False,point=point,variant=variant)
    if not item['valid']:return dict(pending=False,database_read=False,validation_passed=False,result=item['result'],performance_speedup=None)
    sys.path.insert(0,str(root));from analyze_windows import extract
    metrics,_=extract(item['folder'],variant);window=endpoint(item['folder']);command=item['case']['command'];warmup=int(command[command.index('-W')+1]);total=int(command[command.index('-I')+1])
    assert abs(metrics['warmup_instructions']-warmup)<128 and abs(metrics['instructions']-(total-warmup))<128
    return dict(pending=False,database_read=True,validation_passed=True,point=point,variant=variant,result=item['result'],metrics=metrics,endpoint=window,closed_receipt_sha256=item['receipt_sha256'],consumer_sha256=digest(Path(__file__)),performance_speedup=None,scope='One closed registered batch point. Native CPI and endpoint budget, no unpaired performance gain or suite score.')


def guard_pair(root,point,turnover=False):
    items=[closed(root,'guards',variant,point,turnover)for variant in ['control','candidate']]
    if any(item is None for item in items):return dict(pending=True,database_read=False,point=point,missing_variants=[v for v,item in zip(['control','candidate'],items)if item is None])
    if not all(item['valid']for item in items):return dict(pending=False,database_read=False,validation_passed=False,point=point,performance_speedup=None)
    a,b=items
    for flag in ['-I','-W','-s','-i','--diff','--cst-file','--dump-select-db']:
        commands=[item['case']['command']for item in items];assert commands[0][commands[0].index(flag)+1]==commands[1][commands[1].index(flag)+1]
    sys.path.insert(0,str(root))
    from analyze_windows import extract
    from tea_dip_sampling_concordance import concordance
    comparison=pair(a['folder'],b['folder'])
    sampling=concordance([extract(item['folder'],variant)[0] for item,variant in zip(items,['control','candidate'])],comparison)
    return dict(pending=False,database_read=True,validation_passed=True,point=point,comparison=comparison,tea_sampling_concordance=sampling,full_native_six_tables=old_execution(a['folder'],b['folder'],set()),closed_receipt_sha256=[item['receipt_sha256']for item in items],consumer_sha256=digest(Path(__file__)),scope='Registered same-window stage-control/candidate short guard. Exact TEA conditional on observed endpoint alignment; no suite score or physical timing claim.')


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',required=True,type=Path);parser.add_argument('--batch',choices=['guards','formal','soplex12322'],required=True);parser.add_argument('--point',required=True);parser.add_argument('--variant',choices=['control','candidate']);parser.add_argument('--turnover',action='store_true',help='Use registered8f8/39a batches and closed39a guard controls');parser.add_argument('--action',choices=['collect','analyze','pair'],required=True);args=parser.parse_args();root=args.report_dir.resolve()
    if args.action=='pair':
        assert args.batch=='guards'and args.variant is None;result=guard_pair(root,args.point,args.turnover)
    else:
        assert args.variant is not None;result=collect(root,args.batch,args.variant,args.point,args.turnover)if args.action=='collect'else analyze(root,args.batch,args.variant,args.point,args.turnover)
    if args.action!='collect'and not result['pending']:
        launch,_,stage=metadata(root,args.batch,'candidate'if args.action=='pair'else args.variant,args.point,args.turnover);output=root/(Path(launch['run_dir']).name+'-pairs')/args.point if args.action=='pair'else stage/'analysis';output.mkdir(parents=True,exist_ok=True);write(output/'evidence.json',result)
    print(json.dumps(result,indent=2))


if __name__=='__main__':main()
