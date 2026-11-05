"""Evaluate the isolated query-only FDIV WB eligibility candidate against841 short controls."""
from pathlib import Path
import argparse,hashlib,importlib.util,json,sys


def digest(path):
    with path.open('rb') as stream:return hashlib.file_digest(stream,'sha256').hexdigest()


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',type=Path,required=True);parser.add_argument('--stage',choices=['wb-query','wb-query-formal'],default='wb-query');args=parser.parse_args();root=args.report_dir.resolve();sys.path.insert(0,str(root))
    from analyze_windows import extract
    from analyze_queue_observation import verify_files,compare_windows
    from analyze_gaps import bounds
    from native_perf import read_blocks,counter_multisets
    helper_path=root/'evidence/ci-mechanism-tools/tea_dip_ci_mechanism_evidence.py';spec_helper=importlib.util.spec_from_file_location('wb_class_gaps',helper_path);helper=importlib.util.module_from_spec(spec_helper);spec_helper.loader.exec_module(helper)
    evidence=root/'evidence/fdiv-wb-query';source=json.loads((evidence/'source.json').read_text());verify_files(evidence,json.loads((evidence/'source-launch-manifest.json').read_text()));spec=json.loads((evidence/('stage/experiment.json' if args.stage=='wb-query' else 'formal-experiment.json')).read_text());assert source['head']=='b0a22b93a867c5dff56b1b3fb32708c9e6363b01' and source['base']=='84112d0b65231ca690a4ef2fea4b5169f2146e10'
    stage=root/args.stage;variant='fdiv-wb-query';manifest=json.loads((stage/'manifest.json').read_text()) if (stage/'manifest.json').exists() else {};control_root=root/('raw' if args.stage=='wb-query' else 'formal');control_manifest=json.loads((control_root/'manifest.json').read_text());rows=[]
    for case in spec['cases']:
        point=case['point'];directory=stage/variant/'points'/point
        if not(directory/'result.json').exists():continue
        receipt_path=stage/'closed-receipts'/(point+'.json');receipt=json.loads(receipt_path.read_text());assert not receipt['original_process_live'] and receipt['exitcode']=='0' and receipt['source']==source['head'],'Original candidate process/source is not proven closed'
        prefix=variant+'/points/'+point+'/';entries={n:e for n,e in manifest.items() if n.startswith(prefix)};verify_files(stage,entries)
        for n,e in receipt['files'].items():assert manifest[prefix+n]==e
        control_prefix=('control/short/' if args.stage=='wb-query' else 'rebuild-control/points/')+point+'/';control=control_root/control_prefix;verify_files(control_root,{n:e for n,e in control_manifest.items() if n.startswith(control_prefix)})
        build=json.loads((evidence/'build-state.json').read_text());assert build['phase']=='complete' and build['source']==source['head']
        identity=json.loads((directory/'result.json').read_text());control_identity=json.loads((control/'result.json').read_text());assert identity['source']==source['head'] and identity['binary_sha256']==build['binary_sha256']==receipt['binary_sha256'] and control_identity['binary_sha256']==spec.get('control_binary_sha256',spec['baseline_sha256'])
        assert identity['cst_sha256']==digest(evidence/'stage/tea-dip-period1000.cst')
        micro=json.loads((evidence/'micro-gate.json').read_text());assert micro['passed'] and micro['candidate_source']==source['head'] and micro['candidate_binary_sha256']==build['binary_sha256'] and identity['micro_gate_sha256']==digest(evidence/'micro-gate.json')
        gate_audit=json.loads((evidence/'micro-gate-audit.json').read_text());assert gate_audit['passed'] and gate_audit['candidate_source']==source['head'] and gate_audit['candidate_binary_sha256']==build['binary_sha256'] and gate_audit['micro_gate_sha256']==digest(evidence/'micro-gate.json') and gate_audit['manifest_sha256']==digest(evidence/'micro-gate-manifest.json') and gate_audit['closed_receipt_sha256']==digest(evidence/'micro-gate-closure-receipt.json')
        windows=[];profiles=[];native=[]
        for folder,label in [(control,'control841'),(directory,variant)]:
            command=json.loads((folder/'command.json').read_text());stdout=(folder/'simulator_out.txt').read_text();assert '\nstreamL1Depth0 = 64\n' in stdout
            for flag,value in [('-s',str(case['seed'])),('-W',str(spec['warmup'])),('-I',str(spec['max_instr'])),('--diff',spec['reference']),('--dump-select-db','Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0')]:assert command[command.index(flag)+1]==value
            window,pc=extract(folder,label,1000);assert abs(window['instructions']-(spec['max_instr']-spec['warmup']))<128 and abs(window['warmup_instructions']-spec['warmup'])<128;windows.append(window);profiles.append(pc);native.append(sorted(read_blocks(folder/'simulator_err.txt').items()))
        a,b=windows;delta=b['cpi']-a['cpi'];dip={state:b['metrics']['DIP_'+state]-a['metrics']['DIP_'+state] for state in ['Base','Stall','FrontEnd','Misspeculation','Unresolved']};assert abs(sum(dip.values())-delta)<1e-9
        changes=[]
        for phase,((at,ab),(bt,bb)) in zip(['warmup','roi'],zip(*native)):
            x,y=[counter_multisets(block) for block in [ab,bb]];names=sorted(k for k in x.keys()|y.keys() if x.get(k)!=y.get(k));changes.append(dict(phase=phase,stamps=[at,bt],differences=[dict(name=k,control=x.get(k),candidate=y.get(k)) for k in names]))
        equality=None
        if delta==0:
            equality=compare_windows(*windows,control,directory,*profiles);equality['checks']['all_old_native_warmup_and_roi_equal']=all(not r['differences'] for r in changes);equality['neutral']=all(equality['checks'].values())
        gap_windows=[helper.gap_window(folder,window,bounds) for folder,window in zip([control,directory],windows)];gaps=[]
        for threshold in [128,512]:
            d=dict(threshold=threshold)
            for kind in ['long','short']:
                d[kind+'_lower']=gap_windows[1][threshold][kind+'_cycles_lower']/b['instructions']-gap_windows[0][threshold][kind+'_cycles_upper']/a['instructions'];d[kind+'_upper']=gap_windows[1][threshold][kind+'_cycles_upper']/b['instructions']-gap_windows[0][threshold][kind+'_cycles_lower']/a['instructions'];assert d[kind+'_lower']<=d[kind+'_upper']
            gaps.append(d)
        def prof(rows):return {(r['view'],r['pc'],r['state'],r['signature'],r['labels']):r['cpi'] for r in rows}
        pc_a,pc_b=[prof(p) for p in profiles];pc_delta=[dict(view=k[0],pc=k[1],state=k[2],signature=k[3],labels=k[4],control_cpi=pc_a.get(k,0),candidate_cpi=pc_b.get(k,0),delta_cpi=pc_b.get(k,0)-pc_a.get(k,0)) for k in pc_a.keys()|pc_b.keys()]
        rows.append(dict(point=point,comparison=dict(control_cpi=a['cpi'],candidate_cpi=b['cpi'],speedup=a['cpi']/b['cpi']-1,delta_cpi=delta),control=a,candidate=b,exact_delta_cpi={state:b['exact_tea_interior']['exact_state_cpi'][state]-a['exact_tea_interior']['exact_state_cpi'][state] for state in ['Computing','Stalled','Flushed','Drained']},exact_omitted_edges=[w['exact_tea_interior']['omitted_edge_cycles'] for w in windows],dip_exclusive_delta_cpi=dip,retirement_gap_delta_cpi_bounds=gaps,native_phase_differences=changes,zero_cpi_equality=equality,top_sampled_pc_deltas={view:sorted([r for r in pc_delta if r['view']==view],key=lambda r:(-abs(r['delta_cpi']),str(r['pc']),r['state'],str(r['signature'])))[:8] for view in ['TEA','DIP']},closed_receipt_sha256=digest(receipt_path)))
    result=dict(stage=args.stage,completed=len(rows),expected=len(spec['cases']),complete=len(rows)==len(spec['cases']),source=source['head'],base=source['base'],rows=rows,performance_causality_validated=None,consumer_sha256=digest(Path(__file__)),helper_sha256=digest(helper_path),scope='Only original closed fixed-resource query-only candidate with all6directedmicro gates windows versus same-seed841 controls at the explicitly selected short or formal stage. Whole-ROI CPI, exact TEA with recorded edges, exclusive DIP budget, raw multivalue native phases, conservative128/512 gap bounds and sampled PC changes. ZeroCPI requires every native and seven execution categories; no name exemptions. No partial-suite score, missing-point interpolation, unique cause or default/CI adoption claim.')
    out=stage/'analysis';out.mkdir(parents=True,exist_ok=True);(out/'mechanism-evidence.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(dict(completed=len(rows),expected=len(spec['cases']),rows=[dict(point=r['point'],**r['comparison']) for r in rows]),indent=2))

if __name__=='__main__':main()
