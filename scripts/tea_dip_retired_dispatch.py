"""Resolve DIP source samples in common retirement bands, preserving deferred attribution."""
from pathlib import Path
import argparse,hashlib,importlib.util,json,sqlite3,sys
from collections import Counter

STATES=['Base','Stall','FrontEnd','Misspeculation','Unresolved']
EDGES=[0,5000000,10000000,15000000,20000000,25000000,30000000,35000000]


def digest(path):
    with path.open('rb') as stream:return hashlib.file_digest(stream,'sha256').hexdigest()


def summarize(dip, samples, finals, width):
    profile,summary=dip.analyze(iter(samples),iter(finals),width)
    bins=Counter({(STATES[4],0):summary['unresolved_slots']})
    for (state,mask,pc),count in profile.items():bins[dip.STATES[state],mask]+=count
    assert sum(bins.values())==summary['sampled_slots']
    return profile,summary,bins


def sample_bands(dip,samples,finals,width,period):
    assert samples and period==1000
    for before,after in zip(samples,samples[1:]):
        assert after['CYCLE']-before['CYCLE']==period,'Duplicate, off-grid or missing DIP sample'
        assert after['TOTALRETIRED']>=before['TOTALRETIRED'],'Nonmonotonic DIP cumulative retirement'
    for row in samples:assert row['CYCLE']%period==0 and row['TOTALRETIRED']>=0 and row['RETIRED']>=0
    complete,whole,whole_bins=summarize(dip,samples,finals,width);parts=[];joined=Counter();joined_bins=Counter();unresolved=0
    for low,high in zip(EDGES,[*EDGES[1:],None]):
        selected=[row for row in samples if row['TOTALRETIRED']>=low and (high is None or row['TOTALRETIRED']<high)]
        if not selected:continue
        profile,summary,bins=summarize(dip,selected,finals,width);joined.update(profile);joined_bins.update(bins);unresolved+=summary['unresolved_slots']
        rows=[dict(state=state,mask=mask,slots=count,sampled_fraction=count/summary['sampled_slots']) for (state,mask),count in sorted(bins.items())]
        parts.append(dict(retired_start=low,retired_end=high,first_source_sample_cycle=selected[0]['CYCLE'],last_source_sample_cycle=selected[-1]['CYCLE'],sampled_cycles=summary['sampled_cycles'],sampled_slots=summary['sampled_slots'],resolved_slots=summary['resolved_slots'],unresolved_slots=summary['unresolved_slots'],reclassified_slots=summary['reclassified_slots'],exclusive_state_mask=rows,exclusive_states={state:sum(n for (name,mask),n in bins.items() if name==state) for state in STATES}))
    assert joined==complete and joined_bins==whole_bins and unresolved==whole['unresolved_slots'] and sum(p['sampled_slots'] for p in parts)==whole['sampled_slots'],'DIP retirement partition changed delayed attribution'
    return dict(samples=len(samples),slots=whole['sampled_slots'],unresolved_slots=whole['unresolved_slots'],all_source_bands_reconstruct_full_pc_mask_profile=True,bands=parts)


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',type=Path,required=True);parser.add_argument('--ci-evidence',choices=['ci-readiness','ci-credit-div','ci-fdiv-credit'],required=True);parser.add_argument('--point',required=True);args=parser.parse_args();root=args.report_dir.resolve();sys.path.insert(0,str(root))
    from analyze_windows import DIP
    namespace='ci' if args.ci_evidence=='ci-readiness' else args.ci_evidence;out=root/namespace/'partial-analysis';record_path=out/(args.point+'.json');record=json.loads(record_path.read_text());receipt_path=root/namespace/'raw/closed-receipts'/(args.point+'.json');receipt=json.loads(receipt_path.read_text());run=json.loads((root/'evidence'/args.ci_evidence/'run.json').read_text())
    assert record['run_id']==receipt['run_id']==run['id'] and record['source']==receipt['source']==run['head_sha']
    assert not receipt['original_process_live'] and receipt['runner_success_lines'],'Original CI guest closure is unproven'
    assert digest(receipt_path)==record['closed_receipt_sha256']
    baseline=root.parents[1]/'ci-41413249290';manifest_path=baseline/'analysis/manifest.json';manifest=json.loads(manifest_path.read_text());assert manifest['source_sha']=='84112d0b65231ca690a4ef2fea4b5169f2146e10'
    windows=[]
    for label,path,entry in [('control',baseline/'raw/results'/args.point/'simulator.db',manifest['files']['raw/results/'+args.point+'/simulator.db']),('candidate',root/namespace/'raw/results'/args.point/'simulator.db',receipt['files']['simulator.db'])]:
        assert path.stat().st_size==entry['bytes'] and digest(path)==entry['sha256']
        with sqlite3.connect(path.resolve().as_uri()+'?mode=ro',uri=True) as db:
            db.row_factory=sqlite3.Row;samples=list(db.execute('SELECT * FROM Dip_0 ORDER BY CYCLE,ID'));finals=list(db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'));width=sum(row['name'].startswith('SLOTS_') and row['name'].endswith('_STATE') for row in db.execute('pragma table_info(Dip_0)'));alignment=list(db.execute('SELECT s.CYCLE,s.RETIRED,d.TOTALRETIRED FROM TeaCycleSummary_0 s JOIN Dip_0 d ON s.CYCLE=d.CYCLE'))
        assert width>0 and len(alignment)>0 and all(row['RETIRED']==row['TOTALRETIRED'] for row in alignment),'TEA snapshot and DIP prefix retirement differ at matching cycles'
        windows.append(dict(label=label,database_sha256=entry['sha256'],dispatch_width=width,same_cycle_snapshot_retirement_matches=len(alignment),**sample_bands(DIP,samples,finals,width,1000)))
    a,b=[{row['retired_start']:row for row in window['bands']} for window in windows];comparisons=[]
    for low,high in zip(EDGES[1:-1],EDGES[2:]):
        assert low in a and low in b,'Common5M retirement band lacks sampled coverage'
        def bins(row):return {(v['state'],v['mask']):v['slots'] for v in row['exclusive_state_mask']}
        x,y=bins(a[low]),bins(b[low]);scale=1000/(windows[0]['dispatch_width']*(high-low));assert windows[0]['dispatch_width']==windows[1]['dispatch_width']
        rows=[dict(state=state,mask=mask,control_slots=x.get((state,mask),0),candidate_slots=y.get((state,mask),0),delta_sampled_equivalent_cycles_per_retired=(y.get((state,mask),0)-x.get((state,mask),0))*scale) for state,mask in sorted(x.keys()|y.keys())]
        total=(b[low]['sampled_cycles']-a[low]['sampled_cycles'])*1000/(high-low);assert abs(sum(row['delta_sampled_equivalent_cycles_per_retired'] for row in rows)-total)<1e-12
        comparisons.append(dict(retired_start=low,retired_end=high,control_sampled_cycles=a[low]['sampled_cycles'],candidate_sampled_cycles=b[low]['sampled_cycles'],delta_sampled_equivalent_cycles_per_retired=total,exclusive_state_delta={state:sum(row['delta_sampled_equivalent_cycles_per_retired'] for row in rows if row['state']==state) for state in STATES},exclusive_state_mask_delta=rows))
    sources=[Path(__file__),baseline/'source/scripts/dip_analyze.py',baseline/'source/src/main/scala/xiangshan/Dip.scala',baseline/'source/src/main/scala/xiangshan/TeaCycleStats.scala',baseline/'source/src/main/scala/xiangshan/backend/rob/Rob.scala',record_path,receipt_path,manifest_path]
    result=dict(run_id=run['id'],source=run['head_sha'],point=args.point,windows=windows,comparisons=comparisons,source_sha256={str(path):digest(path) for path in sources},consumer_sha256=digest(Path(__file__)),scope='DIP original source sample cycles are partitioned by exact TOTALRETIRED prefix in5M retirement bands. Deferred finals may resolve later and are processed in full original order; reclassification stays with the source sample band. All bands including outside5M–35M exactly reconstruct full PC/mask profile and unresolved budget. Period1000 expansion is sampled-equivalent attribution, not exact CPI, a confidence/conservative bound, service time, same-uop identity or unique causality. Do not force equality to separately bracketed exact TEA segments/native ROI or add overlapping mask marginals. No partial-suite score.')
    (out/(args.point+'-retired-dispatch.json')).write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(dict(run_id=run['id'],point=args.point,windows=[{key:window[key] for key in ['samples','slots','unresolved_slots','all_source_bands_reconstruct_full_pc_mask_profile','same_cycle_snapshot_retirement_matches']} for window in windows],comparisons=[{key:row[key] for key in ['retired_start','retired_end','delta_sampled_equivalent_cycles_per_retired','exclusive_state_delta']} for row in comparisons]),indent=2))

if __name__=='__main__':main()
