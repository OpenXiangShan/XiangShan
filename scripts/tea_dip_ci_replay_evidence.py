"""Collect and compare independent same-binary repeats without changing original CI evidence."""
from pathlib import Path
from datetime import datetime, timezone
import argparse
import hashlib
import importlib.util
import json
import shlex
import subprocess
import sys
import tarfile


FILES = ['command.json','result.json','exitcode','simulator_out.txt','simulator_err.txt','simulator.db','process.json','launch-observation.json','controller-state.json']

def digest(path):
    with path.open('rb') as stream:return hashlib.file_digest(stream, 'sha256').hexdigest()


def collect(root, evidence, spec):
    initial = json.loads((evidence/'launch-live.json').read_text())
    source = json.loads((evidence/'source-runner.json').read_text())
    code = r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json,socket
spec=SPEC;initial=INITIAL;runner_sha=RUNNER;spec_sha=SPEC_SHA;root=Path(spec['node_run_dir']);rows=[];pending=[]
assert hashlib.sha256((root/'experiment.json').read_bytes()).hexdigest()==spec_sha
def live(process):
 path=Path('/proc',str(process['pid']),'stat')
 return path.exists() and path.read_text().rsplit(')',1)[1].split()[19]==process['start_ticks']
for case in spec['cases']:
 point=case['point'];directory=root/'points'/point;old=next(row for row in initial['guests'] if row['point']==point);old_controller=next(row for row in initial['controllers'] if row['point']==point)
 assert old['live'] and old_controller['live'] and old['host']==socket.gethostname()
 process=json.loads((directory/'process.json').read_text());controller=json.loads((directory/'controller-state.json').read_text())
 assert process=={key:old[key] for key in ['pid','start_ticks','host','core']}
 assert all(controller[key]==old_controller[key] for key in ['pid','start_ticks','host'])
 guest_live=live(process);controller_live=live(controller)
 if guest_live or controller_live:
  pending.append(dict(point=point,original_process_live=guest_live,controller_live=controller_live,phase=controller['phase'],database_read=False));continue
 if controller['phase']!='complete':
  pending.append(dict(point=point,phase=controller['phase'],error=controller.get('error'),database_read=False));continue
 assert (directory/'exitcode').read_text().strip()=='0'
 result=json.loads((directory/'result.json').read_text());observation=json.loads((directory/'launch-observation.json').read_text())
 assert result['source']==spec['source_identity']['source'] and result['binary_sha256']==spec['binary_sha256'] and result['spec_sha256']==observation['spec_sha256']==spec_sha and result['runner_sha256']==observation['runner_sha256']==runner_sha and observation['original_process_live'] and observation['process']==process
 files={}
 for name in NAMES:
  path=directory/name
  with path.open('rb') as stream:sha=hashlib.file_digest(stream,'sha256').hexdigest()
  files[name]=dict(bytes=path.stat().st_size,sha256=sha)
 rows.append(dict(utc=datetime.now(timezone.utc).isoformat(),point=point,process=process,controller=controller,original_process_live=False,controller_live=False,last_live_observation=old,last_controller_observation=old_controller,exitcode='0',source=result['source'],binary_sha256=result['binary_sha256'],spec_sha256=spec_sha,files=files))
print(json.dumps(dict(utc=datetime.now(timezone.utc).isoformat(),rows=rows,pending=pending)))
'''.replace('NAMES', repr(FILES)).replace('SPEC_SHA', repr(digest(evidence/'experiment.json'))).replace('SPEC', repr(spec)).replace('INITIAL', repr(initial)).replace('RUNNER', repr(source['source_sha256']))
    response = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', 'node031', 'python3', '-'], input=code, text=True, capture_output=True, timeout=50, check=True)
    result = json.loads(response.stdout); stage = root/'ci-fdiv-replay'; manifest_path = stage/'manifest.json'
    manifest = json.loads(manifest_path.read_text()) if manifest_path.exists() else {}; new = {}
    for row in result['rows']:
        for name, entry in row['files'].items():
            key = 'points/'+row['point']+'/'+name
            if key in manifest:assert manifest[key] == entry, ('Closed replay raw changed', key)
            else:new[key] = entry
    if new:
        archives = stage/'archives'; archives.mkdir(parents=True, exist_ok=True)
        archive = archives/(datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%SZ')+'.tar.gz')
        command = 'tar -C '+shlex.quote(spec['node_run_dir'])+' -czf - '+' '.join(map(shlex.quote, new))
        with archive.open('wb') as stream:subprocess.run(['proxychains', '-q', 'ssh', 'node031', command], stdout=stream, check=True, timeout=50)
        with tarfile.open(archive) as package:
            assert {member.name for member in package.getmembers()} == set(new); package.extractall(stage, filter='data')
        manifest.update(new); manifest_path.write_text(json.dumps(manifest, indent=2)+'\n')
    for row in result['rows']:
        for name, entry in row['files'].items():
            path = stage/'points'/row['point']/name; assert path.stat().st_size == entry['bytes'] and digest(path) == entry['sha256']
        receipt = stage/'closed-receipts'/(row['point']+'.json')
        if receipt.exists():
            old = json.loads(receipt.read_text())
            for key in ['process', 'controller', 'source', 'binary_sha256', 'spec_sha256', 'files']:assert old[key] == row[key]
        else:receipt.parent.mkdir(parents=True, exist_ok=True); receipt.write_text(json.dumps(row, indent=2)+'\n')
    stage.mkdir(exist_ok=True); (stage/'collection-status.json').write_text(json.dumps(result, indent=2)+'\n')
    print(json.dumps(dict(closed=len(result['rows']), pending=result['pending']), indent=2))


def analyze(root, evidence, spec):
    sys.path.insert(0, str(root))
    from analyze_windows import extract, csvout
    from analyze_queue_observation import compare_windows, verify_files
    from analyze_gaps import bounds
    from native_perf import read_blocks, counter_multisets
    helper_path = root/'evidence/ci-mechanism-tools/tea_dip_ci_mechanism_evidence.py'
    module_spec = importlib.util.spec_from_file_location('replay_gaps', helper_path); helper = importlib.util.module_from_spec(module_spec); module_spec.loader.exec_module(helper)
    stage = root/'ci-fdiv-replay'; manifest_path = stage/'manifest.json'; manifest = json.loads(manifest_path.read_text()) if manifest_path.exists() else {}; rows = []
    source = json.loads((evidence/'source-runner.json').read_text()); initial = json.loads((evidence/'launch-live.json').read_text())
    assert digest(root/'native_perf.py') == spec['native_reader_sha256']
    for case in spec['cases']:
        point = case['point']; directory = stage/'points'/point
        if not (directory/'result.json').exists():continue
        receipt_path = stage/'closed-receipts'/(point+'.json'); receipt = json.loads(receipt_path.read_text())
        assert not receipt['original_process_live'] and not receipt['controller_live'] and receipt['last_live_observation']['live'] and receipt['last_controller_observation']['live'] and receipt['exitcode']=='0'
        record = json.loads((directory/'result.json').read_text()); observation = json.loads((directory/'launch-observation.json').read_text())
        old = next(row for row in initial['guests'] if row['point']==point); old_controller = next(row for row in initial['controllers'] if row['point']==point)
        assert receipt['process']==observation['process']=={key:old[key] for key in ['pid','start_ticks','host','core']} and all(receipt['controller'][key]==old_controller[key] for key in ['pid','start_ticks','host']) and set(receipt['files'])==set(FILES)
        assert record['original_ci_run_id']==spec['source_identity']['run_id'] and record['reference_sha256']==spec['reference_sha256'] and record['cst_sha256']==spec['cst_sha256']
        assert receipt['source']==record['source']==spec['source_identity']['source'] and receipt['binary_sha256']==record['binary_sha256']==spec['binary_sha256']
        assert receipt['spec_sha256']==record['spec_sha256']==observation['spec_sha256']==digest(evidence/'experiment.json') and record['runner_sha256']==observation['runner_sha256']==source['source_sha256']
        prefix = 'points/'+point+'/'; verify_files(stage, {key:entry for key,entry in manifest.items() if key.startswith(prefix)})
        for name, entry in receipt['files'].items():assert manifest[prefix+name] == entry
        command = json.loads((directory/'command.json').read_text())
        for flag, value in [('-s', str(case['seed'])), ('-i', case['image']), ('-I', str(spec['max_instr'])), ('-W', str(spec['warmup'])), ('--diff', spec['reference']), ('--dump-select-db', 'Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0')]:assert command[command.index(flag)+1] == value
        control = root.parents[1]/'ci-41413249290/raw/results'/point
        windows = []; profiles = []
        for folder, label in [(control, 'control841'), (directory, spec['variant'])]:
            window, pcs = extract(folder, label, spec['period']); assert window['seed']==case['seed'] and abs(window['warmup_instructions']-spec['warmup'])<128 and abs(window['instructions']-(spec['max_instr']-spec['warmup']))<128; windows.append(window); profiles.append(pcs)
        a,b = windows; delta = b['cpi']-a['cpi']; dip = {state:b['metrics']['DIP_'+state]-a['metrics']['DIP_'+state] for state in ['Base','Stall','FrontEnd','Misspeculation','Unresolved']}; assert abs(sum(dip.values())-delta)<1e-9
        native = [sorted(read_blocks(folder/'simulator_err.txt').items()) for folder in [control,directory]]; changes = []
        for phase, ((at,ab),(bt,bb)) in zip(['warmup','roi'],zip(*native)):
            x,y = [counter_multisets(block) for block in [ab,bb]]; names = sorted(key for key in x.keys()|y.keys() if x.get(key)!=y.get(key)); changes.append(dict(phase=phase,stamps=[at,bt],differences=[dict(name=key,control=x.get(key),candidate=y.get(key)) for key in names]))
        equality = None
        if delta==0:
            equality = compare_windows(a,b,control,directory,*profiles); equality['checks']['all_native_without_name_exemptions'] = not any(row['differences'] for row in changes); equality['neutral'] = all(equality['checks'].values())
        gaps = [helper.gap_window(folder, window, bounds) for folder,window in zip([control,directory],windows)]; gap_delta = []
        for threshold in [128,512]:
            row = dict(threshold=threshold)
            for kind in ['long','short']:
                row[kind+'_lower'] = gaps[1][threshold][kind+'_cycles_lower']/b['instructions']-gaps[0][threshold][kind+'_cycles_upper']/a['instructions']; row[kind+'_upper'] = gaps[1][threshold][kind+'_cycles_upper']/b['instructions']-gaps[0][threshold][kind+'_cycles_lower']/a['instructions']
            gap_delta.append(row)
        def keyed(records):return {(r['view'],r['pc'],r['state'],r['signature'],r['labels']):r['cpi'] for r in records}
        x,y = [keyed(pcs) for pcs in profiles]; pc_delta = [dict(view=key[0],pc=key[1],state=key[2],signature=key[3],labels=key[4],control_cpi=x.get(key,0),candidate_cpi=y.get(key,0),delta_cpi=y.get(key,0)-x.get(key,0)) for key in x.keys()|y.keys()]
        out = stage/'analysis'; out.mkdir(parents=True,exist_ok=True); csvout(out/(point+'-pc-deltas.csv'),sorted(pc_delta,key=lambda r:(r['view'],-abs(r['delta_cpi']),str(r['pc']),r['state'],str(r['signature']))))
        rows.append(dict(point=point,control=a,candidate=b,comparison=dict(control_cpi=a['cpi'],candidate_cpi=b['cpi'],speedup=a['cpi']/b['cpi']-1,delta_cpi=delta),exact_delta_cpi={state:b['exact_tea_interior']['exact_state_cpi'][state]-a['exact_tea_interior']['exact_state_cpi'][state] for state in ['Computing','Stalled','Flushed','Drained']},exact_omitted_edges=[w['exact_tea_interior']['omitted_edge_cycles'] for w in windows],dip_exclusive_delta_cpi=dip,native_phase_differences=changes,zero_cpi_equality=equality,gap_delta_cpi_bounds=gap_delta,closed_receipt_sha256=digest(receipt_path)))
    out = stage/'analysis'; out.mkdir(parents=True,exist_ok=True)
    result = dict(stage=spec['stage'],completed=len(rows),expected=len(spec['cases']),complete=len(rows)==len(spec['cases']),rows=rows,source=spec['source_identity']['source'],original_ci_run_id=spec['source_identity']['run_id'],consumer_sha256=digest(Path(__file__)),helper_sha256=digest(helper_path),performance_causality_validated=None,scope='Independent same-binary repeats only. Original four missing-observation CI points remainunknown/noDB. Compare to841, preserving all native fields and seven execution contracts, exactTEAedges, exclusiveDIP, PC deltas and128/512gapbounds. No original closure restoration, partial-suite score or adoption claim.')
    (out/'mechanism-evidence.json').write_text(json.dumps(result,indent=2)+'\n'); print(json.dumps(dict(completed=len(rows),expected=len(spec['cases']),comparisons=[dict(point=row['point'],**row['comparison']) for row in rows]),indent=2))


def main():
    parser = argparse.ArgumentParser(description=__doc__); parser.add_argument('--report-dir',type=Path,required=True); parser.add_argument('--action',choices=['collect','analyze'],required=True); options=parser.parse_args(); root=options.report_dir.resolve(); evidence=root/'evidence/ci-fdiv-replay'
    spec = json.loads((evidence/'experiment.json').read_text()); assert spec['source_identity']==json.loads((root/'evidence/ci-fdiv-credit/archive-identity.json').read_text())
    for name,entry in json.loads((evidence/'source-launch-manifest.json').read_text()).items():assert (evidence/name).stat().st_size==entry['bytes'] and digest(evidence/name)==entry['sha256']
    if options.action=='collect':collect(root,evidence,spec)
    else:analyze(root,evidence,spec)


if __name__=='__main__':main()
