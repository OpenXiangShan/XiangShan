"""Audit closed fixed-distance policy windows against the same compiled binary."""
from pathlib import Path
import argparse,hashlib,json,re,sys


def digest(path):
    with path.open('rb') as stream:return hashlib.file_digest(stream,'sha256').hexdigest()


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',type=Path,required=True);args=parser.parse_args();root=args.report_dir.resolve();sys.path.insert(0,str(root))
    from analyze_windows import extract
    from analyze_queue_observation import verify_files,compare_windows
    from analyze_load_replay_wait import opportunity_block
    from analyze_gaps import bounds
    from native_perf import read_blocks,counter_multisets,unique_counters
    import importlib.util
    helper_path=root/'evidence/ci-mechanism-tools/tea_dip_ci_mechanism_evidence.py';loader=importlib.util.spec_from_file_location('closed_ci_mechanism',helper_path);helper=importlib.util.module_from_spec(loader);loader.loader.exec_module(helper)
    evidence=root/'evidence/stream-l1-depth32';source=json.loads((evidence/'source.json').read_text());spec=json.loads((evidence/'experiment.json').read_text());stage=root/'stream-depth-policy';variant='stream-l1-depth32'
    verify_files(evidence,json.loads((evidence/'source-launch-coremark-manifest.json').read_text()))
    assert source['compiled_source']=='bee4d455716025e156e3d1ddafd8910f0df2b461' and source['head']=='b4fe7f07ace4e11b5b3672cc48afe57d4dac9d0c'
    assert (evidence/'tea-dip-period1000.cst').read_bytes()==(evidence/'baseline.cst').read_bytes()+b'streamL1Depth0 32\n'
    neutral=json.loads((root/'load-replay-wait-observation/analysis/neutrality.json').read_text());control_stage=root/'load-replay-wait-observation'
    manifests=[json.loads((control_stage/'manifest.json').read_text()),json.loads((stage/'manifest.json').read_text()) if (stage/'manifest.json').exists() else {}];rows=[]
    for case in spec['cases']:
        point=case['point'];candidate=stage/variant/'points'/point
        if not (candidate/'result.json').exists():continue
        receipt_path=stage/'closed-receipts'/(point+'.json');receipt=json.loads(receipt_path.read_text())
        assert receipt['point']==point and receipt['source']==source['compiled_source'] and receipt['policy_source']==source['head'] and not receipt['original_process_live'], 'Original policy guest is not proven closed'
        assert receipt['binary_sha256']==source['binary_sha256'] and receipt['exitcode']=='0'
        assert next(r for r in neutral['rows'] if r['point']==point)['comparison']['neutral']
        control=control_stage/'load-replay-wait-observe/points'/point;directories=[control,candidate];windows=[];profiles=[];phases=[]
        for index,(folder,label,parent,manifest,depth) in enumerate(zip(directories,['load-replay-wait-observe',variant],[control_stage,stage],manifests,[64,32])):
            prefix=label+'/points/'+point+'/';entries={name:entry for name,entry in manifest.items() if name.startswith(prefix)}
            assert all(prefix+name in entries for name in ['result.json','command.json','exitcode','simulator_out.txt','simulator_err.txt','simulator.db','process.json']);verify_files(parent,entries)
            if index==1:
                assert receipt['process']==json.loads((folder/'process.json').read_text())
                for name,entry in receipt['files'].items():assert manifest[prefix+name]==entry
            result=json.loads((folder/'result.json').read_text());command=json.loads((folder/'command.json').read_text());stdout=(folder/'simulator_out.txt').read_text()
            assert result['source']==source['compiled_source'] and result['binary_sha256']==source['binary_sha256'] and result['point']==point and result['seed']==case['seed'] and (folder/'exitcode').read_text().strip()=='0'
            if index==1:assert result['policy_source']==source['head'] and result['cst_sha256']==source['cst_sha256']
            assert re.search(r'^streamL1Depth0 = '+str(depth)+'$',stdout,re.M)
            for flag,value in [('-s',str(case['seed'])),('-I','2000000'),('-W','1000000'),('--diff',spec['reference']),('--dump-select-db','Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0')]:assert command[command.index(flag)+1]==value
            assert Path(command[command.index('-i')+1]).parent==Path(spec['checkpoint_root'])/case['workload']/str(case['checkpoint'])
            window,pc=extract(folder,label,1000);assert abs(window['instructions']-1000000)<128 and abs(window['warmup_instructions']-1000000)<128;windows.append(window);profiles.append(pc)
            phases.append(sorted(read_blocks(folder/'simulator_err.txt').items()))
        a,b=windows;delta=b['cpi']-a['cpi'];dip={state:b['metrics']['DIP_'+state]-a['metrics']['DIP_'+state] for state in ['Base','Stall','FrontEnd','Misspeculation','Unresolved']};assert abs(sum(dip.values())-delta)<1e-9
        gap_windows=[helper.gap_window(folder,window,bounds) for folder,window in zip(directories,windows)];gap=[]
        for threshold in [128,512]:
            item=dict(threshold=threshold)
            for kind in ['long','short']:
                item[kind+'_lower']=gap_windows[1][threshold][kind+'_cycles_lower']/b['instructions']-gap_windows[0][threshold][kind+'_cycles_upper']/a['instructions']
                item[kind+'_upper']=gap_windows[1][threshold][kind+'_cycles_upper']/b['instructions']-gap_windows[0][threshold][kind+'_cycles_lower']/a['instructions']
            gap.append(item)
        observations=[]
        for index,phase_list in enumerate(phases):
            for phase_name,(stamp,block) in zip(['warmup','roi'],phase_list):
                observed=opportunity_block(block);assert observed['observed_checks_passed'] is True and observed['classification_complete'] is True
                c=unique_counters(block);selected={key:value for key,value in c.items() if key.endswith(('.mainpipe_tag_write','.prefetch_write_valid_pf','.fake_tag_write_intend','.StreamStride_fire_l1','.StreamStride_block_l1','.berti_fire_l1','.berti_block_l1','.dcache_miss','.dcache_miss_first_issue'))}
                observations.append(dict(side=['control','candidate'][index],phase=phase_name,stamp=stamp,observation=observed,native_events=selected))
        left,right=[{(r['view'],r['pc'],r['state'],r['signature'],r['labels']):r['cpi'] for r in profile} for profile in profiles];pc_deltas=[dict(view=k[0],pc=k[1],state=k[2],signature=k[3],labels=k[4],control_cpi=left.get(k,0),candidate_cpi=right.get(k,0),delta_cpi=right.get(k,0)-left.get(k,0)) for k in left.keys()|right.keys()]
        changes=[]
        for phase_name,((_,x),(_,y)) in zip(['warmup','roi'],zip(*phases)):
            x,y=[counter_multisets(block) for block in [x,y]];names=sorted(k for k in x.keys()|y.keys() if x.get(k)!=y.get(k));changes.append(dict(phase=phase_name,differences=[dict(name=k,control=x.get(k),candidate=y.get(k)) for k in names]))
        rows.append(dict(point=point,control=a,candidate=b,comparison=dict(control_cpi=a['cpi'],candidate_cpi=b['cpi'],speedup=a['cpi']/b['cpi']-1,delta_cpi=delta),exact_delta_cpi={state:b['exact_tea_interior']['exact_state_cpi'][state]-a['exact_tea_interior']['exact_state_cpi'][state] for state in ['Computing','Stalled','Flushed','Drained']},exact_omitted_edges=[w['exact_tea_interior']['omitted_edge_cycles'] for w in windows],dip_exclusive_delta_cpi=dip,gap_delta_cpi_bounds=gap,phases=observations,native_phase_differences=changes,zero_cpi_equality=compare_windows(*windows,*directories,*profiles) if delta==0 else None,top_sampled_pc_deltas=sorted(pc_deltas,key=lambda r:(-abs(r['delta_cpi']),str(r['pc']),r['view'],r['state'],str(r['signature'])))[:16],closed_receipt_sha256=digest(receipt_path)))
    output=dict(completed=len(rows),expected=len(spec['cases']),complete=len(rows)==len(spec['cases']),compiled_source=source['compiled_source'],policy_source=source['head'],binary_sha256=source['binary_sha256'],rows=rows,performance_causality_validated=None,source_sha256={Path(__file__).name:digest(Path(__file__)),**{name:digest(root/name) for name in ['analyze_windows.py','analyze_queue_observation.py','analyze_load_replay_wait.py','analyze_gaps.py','native_perf.py']},'ci_mechanism_helper':digest(helper_path)},scope='Individually closed same-binary CST64 vs32 windows only. Full native and six execution categories used for zero-CPI checks; exact TEA retains edges, DIP exclusive full-ROI budget, port-cycle partitions remain distinct from CPU loss. No partial-suite score, default adoption or unique causality claim.')
    out=stage/'analysis';out.mkdir(parents=True,exist_ok=True);(out/'mechanism-evidence.json').write_text(json.dumps(output,indent=2)+'\n');print(json.dumps(dict(completed=len(rows),expected=len(spec['cases']),rows=[dict(point=r['point'],**r['comparison']) for r in rows]),indent=2))

if __name__=='__main__':main()
