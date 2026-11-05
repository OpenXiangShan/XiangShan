"""Audit the closed fixed/variable FP micro experiment without converting failures into performance."""
from collections import Counter
from pathlib import Path
import argparse
import hashlib
import json
import re
import sqlite3
import sys


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',type=Path,required=True);args=parser.parse_args();root=args.report_dir.resolve();sys.path.insert(0,str(root))
    from analyze_windows import DIP
    from native_perf import read_blocks,counter_multisets
    stage=root/'wb-protocol-micro';receipt_path=stage/'closure-receipt.json';receipt=json.loads(receipt_path.read_text());assert not receipt['controller_live'] and receipt['all_original_guests_closed'] and receipt['controller']['completed']==6
    manifest=json.loads((stage/'manifest.json').read_text());assert manifest==receipt['files']
    for name,entry in manifest.items():
        path=stage/name;assert path.stat().st_size==entry['bytes'] and digest(path)==entry['sha256']
    spec=json.loads((stage/'experiment.json').read_text());assert spec['binaries']['candidate1f45']['source']=='1f45c4ec4b47c1e35c6b57b0ed2caa3349201c40';rows=[]
    for result in json.loads((stage/'results.json').read_text()):
        variant,case=result['variant'],result['case'];folder=stage/'runs'/variant/case;process=json.loads((folder/'process.json').read_text());assert process==result['process'] and process in receipt['processes'] and result['original_process_closed']
        assert any(e['event']=='guest_started' and all(e[k]==process[k] for k in ['pid','start_ticks','host','core']) for e in receipt['events'])
        command=json.loads((folder/'command.json').read_text());identity=spec['binaries'][variant];case_identity=next(c for c in spec['cases'] if c['case']==case)
        assert identity['binary'] in command and result['binary_sha256']==identity['sha256'] and result['image_sha256']==case_identity['image_sha256']
        for flag,value in [('-s','99'),('-I','100000'),('-C','200000'),('-i',case_identity['image']),('--diff',spec['reference']),('--dump-select-db',spec['selectors'])]:assert command[command.index(flag)+1]==value
        text=re.sub(r'\x1b\[[0-9;]*m','',(folder/'simulator_out.txt').read_text());native=read_blocks(folder/'simulator_err.txt');trap=re.search(r'HIT GOOD TRAP at pc = (0x[0-9a-fA-F]+)',text)
        actual_pass=int((folder/'exitcode').read_text())==0 and trap is not None and int(trap[1],16)==int(case_identity['pass_pc'],16) and 1700<result['instructions']<3000 and not any(s in text for s in ['ABORT','HIT CRITICAL','HIT BAD TRAP','pma check failed'])
        assert actual_pass==result['passed'] and all(result['sampler_flags'].values()) and result['performance_speedup'] is None
        with sqlite3.connect((folder/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True) as db:
            db.row_factory=sqlite3.Row;tea=[dict(r) for r in db.execute('SELECT * FROM Tea_0 ORDER BY CYCLE,ID')];dip=[dict(r) for r in db.execute('SELECT * FROM Dip_0 ORDER BY CYCLE,ID')];snapshots=[dict(r) for r in db.execute('SELECT * FROM TeaCycleSummary_0 ORDER BY CYCLE,ID')];width=sum(r['name'].startswith('SLOTS_') and r['name'].endswith('_STATE') for r in db.execute('PRAGMA table_info(Dip_0)'));finals=list(db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'));profile,stats=DIP.analyze(dip,finals,width)
            tables={name:db.execute('SELECT count(*) FROM '+name).fetchone()[0] for name in spec['selectors'].split()}
        assert tables==result['trace_table_rows'] and all(tables.values()) and [r['CYCLE'] for r in tea]==[r['CYCLE'] for r in dip]==list(range(0,result['cycles']+1,1000))
        assert stats['resolved_slots']+stats['unresolved_slots']==stats['sampled_slots']
        for before,after in zip(snapshots,snapshots[1:]):assert sum(after['STATES_'+str(i)]-before['STATES_'+str(i)] for i in range(4))==after['CYCLE']-before['CYCLE']
        tail=[r for r in dip if r['CYCLE']>=9000];target=Counter((r['SLOTS_'+str(i)+'_PC'],r['SLOTS_'+str(i)+'_CAUSE'],r['SLOTS_'+str(i)+'_STATE']) for r in tail for i in range(width));head=Counter((r['CONTEXT_HEADREASON'],r['CONTEXT_HEADREASONVALID']) for r in tea if r['CYCLE']>=9000);stuck=re.search(r'\[CROB-STUCK\].*?headPC=([0-9a-fA-F]+)',(folder/'simulator_err.txt').read_text())
        rows.append(dict(variant=variant,case=case,passed=actual_pass,exitcode=result['exitcode'],instructions=result['instructions'],cycles=result['cycles'],good_trap_pc=trap[1] if trap else None,difftest_mismatches=result['difftest_mismatches'],native_perf_blocks=len(native),six_tables=tables,dip_budget={k:stats[k] for k in ['sampled_slots','resolved_slots','unresolved_slots']},exact_prefix=[{k:r[k] for k in ['CYCLE','RETIRED','PENDING','STATES_0','STATES_1','STATES_2','STATES_3']} for r in snapshots],dip_retired_prefix=[dict(cycle=r['CYCLE'],total_retired=r['TOTALRETIRED']) for r in dip],tail_original_sample_targets=[dict(pc=hex(pc),cause_mask=mask,state=state,labels=DIP.cause_name(state,mask),slots=n) for (pc,mask,state),n in sorted(target.items())],tail_raw_head_reasons=[dict(raw_reason=reason,raw_valid=valid,samples=n) for (reason,valid),n in sorted(head.items())],stuck_rob_head_pc='0x'+stuck[1] if stuck else None,performance_cpi=None,performance_speedup=None))
    fixed=[stage/'runs'/v/'fixed_only' for v in ['control841','candidate1f45']];blocks=[{t:counter_multisets(b) for t,b in read_blocks(d/'simulator_err.txt').items()} for d in fixed];checks=dict(all_old_native_equal=blocks[0]==blocks[1]);tables={}
    for name in spec['selectors'].split():
        values=[]
        for d in fixed:
            with sqlite3.connect((d/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True) as db:values.append(db.execute('SELECT * FROM '+name+' ORDER BY ID').fetchall())
        checks[name+'_equal']=values[0]==values[1];tables[name]=dict(rows=[len(v) for v in values],all_rows_equal=values[0]==values[1])
    assert all(checks.values())
    result=dict(completed=len(rows),rows=rows,fixed_only_full_native_and_six_table_equality=dict(passed=True,checks=checks,tables=tables),consumer_sha256=digest(Path(__file__)),closed_receipt_sha256=digest(receipt_path),scope='Six closed diagnostic attempts including boot, no SPEC ROI. Failed-attempt cycles/retired prefix are progress evidence, never performance CPI/speedup. DIP finalization preserves unresolved budget; raw selected target, raw head fallback, and CROB head are different observations, not a proven producer chain.')
    out=stage/'analysis';out.mkdir(exist_ok=True);(out/'evidence.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(dict(completed=len(rows),fixed_only_full_contract=True,rows=[{k:r[k] for k in ['variant','case','passed','instructions','cycles','native_perf_blocks','dip_budget','stuck_rob_head_pc']} for r in rows]),indent=2))


if __name__=='__main__':main()
