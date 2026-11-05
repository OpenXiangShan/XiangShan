"""Independently verify six closed FP micro attempts before consuming query-only SPEC results."""
from pathlib import Path
import argparse
import hashlib
import json
import re
import sqlite3
import subprocess
import sys


def digest(path):
    with path.open('rb') as stream:return hashlib.file_digest(stream,'sha256').hexdigest()


def compare_complete(folders):
    from native_perf import read_blocks,counter_multisets
    native=[{stamp:counter_multisets(block) for stamp,block in read_blocks(folder/'simulator_err.txt').items()} for folder in folders]
    checks=dict(all_old_native_equal=native[0]==native[1]);tables={}
    for name in ['Tea_0','TeaDelayedFinal_0','TeaCycleSummary_0','TeaNoCommit_0','Dip_0','DipDelayedFinal_0']:
        values=[]
        for folder in folders:
            with sqlite3.connect((folder/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True) as db:values.append(db.execute('SELECT * FROM '+name+' ORDER BY ID').fetchall())
        checks[name+'_equal']=values[0]==values[1];tables[name]=dict(rows=[len(v) for v in values],equal=values[0]==values[1])
    return dict(passed=all(checks.values()),checks=checks,tables=tables)


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--report-dir',type=Path,required=True);args=parser.parse_args();root=args.report_dir.resolve();sys.path.insert(0,str(root))
    evidence=root/'evidence/fdiv-wb-query';receipt_path=evidence/'micro-gate-closure-receipt.json';receipt=json.loads(receipt_path.read_text())
    assert not receipt['micro_controller_live'] and receipt['all_original_micro_guests_closed'],'Original micro processes must be closed'
    source=json.loads((evidence/'source.json').read_text());build=json.loads((evidence/'build-state.json').read_text());assert build['phase']=='complete' and build['source']==source['head']==receipt['build']['source']=='b0a22b93a867c5dff56b1b3fb32708c9e6363b01'
    gate_path=evidence/'micro-gate.json';gate=json.loads(gate_path.read_text());assert gate==receipt['gate'] and gate['passed'] and gate['attempts']==6 and gate['candidate_source']==source['head'] and gate['candidate_binary_sha256']==build['binary_sha256']
    manifest_path=evidence/'micro-gate-manifest.json';manifest=json.loads(manifest_path.read_text());assert manifest==receipt['files']
    for name,entry in manifest.items():
        path=evidence/name;assert path.stat().st_size==entry['bytes'] and digest(path)==entry['sha256']
    micro=evidence/'protocol-micro';spec=json.loads((micro/'experiment.json').read_text());stage_spec=json.loads((evidence/'stage/experiment.json').read_text());assert spec['binaries']['candidate']['source']==source['head'] and spec['binaries']['candidate']['sha256']==build['binary_sha256'] and spec['binaries']['control841']['sha256']==stage_spec['baseline_sha256']
    assert spec['reference']==stage_spec['reference'] and spec['reference_sha256']==stage_spec['reference_sha256'] and spec['cst_sha256']==digest(evidence/'stage/tea-dip-period1000.cst')==digest(micro/'tea-dip-period1000.cst')
    results_path=micro/'results.json';results=json.loads(results_path.read_text());assert digest(results_path)==gate['results_sha256'] and len(results)==6 and {(r['variant'],r['case']) for r in results}=={(v,c) for v in ['control841','candidate'] for c in ['fixed_only','sqrt_div_mixed','div_fli_mixed']}
    symbols={}
    for case in spec['cases']:
        image_dir=micro/'images'/case['case'];assert digest(image_dir/'program.bin')==case['image_sha256'] and digest(image_dir/'program.S')==case['source_sha256']
        table=subprocess.check_output(['/usr/bin/riscv64-linux-gnu-objdump','-t',str(image_dir/'program.elf')],text=True)
        symbols[case['case']]=int(next(line.split()[0] for line in table.splitlines() if line.split() and line.split()[-1]=='_pass'),16);assert symbols[case['case']]==int(case['pass_pc'],16)
    rows=[]
    for result in results:
        folder=micro/'runs'/result['variant']/result['case'];process=json.loads((folder/'process.json').read_text());assert process==result['process'] and result['original_process_closed'] and any(e.get('event')=='guest_started' and all(e[k]==process[k] for k in ['pid','start_ticks','host','core']) for e in receipt['events'])
        command=json.loads((folder/'command.json').read_text());case=next(c for c in spec['cases'] if c['case']==result['case']);identity=spec['binaries'][result['variant']];assert identity['binary'] in command and result['binary_sha256']==identity['sha256'] and result['image_sha256']==case['image_sha256']
        for flag,value in [('-s','99'),('-I','100000'),('-C','200000'),('-i',case['image']),('--diff',spec['reference']),('--dump-select-db',spec['selectors'])]:assert command[command.index(flag)+1]==value
        text=re.sub(r'\x1b\[[0-9;]*m','',(folder/'simulator_out.txt').read_text());trap=re.search(r'HIT GOOD TRAP at pc = (0x[0-9a-fA-F]+)',text);assert (folder/'exitcode').read_text().strip()=='0' and result['passed'] and trap is not None and int(trap[1],16)==symbols[result['case']]
        assert 'Difftest enabled' in text and not any(marker in text for marker in ['ABORT','HIT CRITICAL','HIT BAD TRAP','pma check failed','EXCEEDING CYCLE/INSTR LIMIT']) and 1700<result['instructions']<3000
        counts=re.search(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)',text);assert [int(v.replace(',','')) for v in counts.groups()]==[result['instructions'],result['cycles']]
        for name,value in [('enableTea0',1),('enableDip0',1),('teaSamplePeriod0',1000),('dipSamplePeriod0',1000)]:assert re.search(r'^'+name+' = '+str(value)+'$',text,re.M)
        with sqlite3.connect((folder/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True) as db:
            tables={name:db.execute('SELECT count(*) FROM '+name).fetchone()[0] for name in spec['selectors'].split()}
            for name in ['Tea_0','Dip_0']:assert [r[0] for r in db.execute('SELECT CYCLE FROM '+name+' ORDER BY CYCLE,ID')]==list(range(0,result['cycles']+1,1000))
        assert tables==result['trace_table_rows'] and all(tables.values());rows.append(dict(variant=result['variant'],case=result['case'],process=process,passed=True,instructions=result['instructions'],cycles=result['cycles'],pass_pc=hex(symbols[result['case']]),six_tables=tables,performance_speedup=None))
    fixed=compare_complete([micro/'runs'/v/'fixed_only' for v in ['control841','candidate']]);assert fixed['passed']
    old=root/'wb-protocol-micro';old_receipt=json.loads((old/'closure-receipt.json').read_text());assert not old_receipt['controller_live'] and old_receipt['all_original_guests_closed']
    for name,entry in old_receipt['files'].items():assert (old/name).stat().st_size==entry['bytes'] and digest(old/name)==entry['sha256']
    repeats=[dict(case=case,**compare_complete([old/'runs/control841'/case,micro/'runs/control841'/case])) for case in ['fixed_only','sqrt_div_mixed','div_fli_mixed']];assert all(r['passed'] for r in repeats)
    result=dict(passed=True,candidate_source=source['head'],candidate_binary_sha256=build['binary_sha256'],attempts=6,rows=rows,fixed_only_full_native_and_six_tables=fixed,old_baseline_repeated_full_native_and_six_tables=repeats,micro_gate_sha256=digest(gate_path),manifest_sha256=digest(manifest_path),closed_receipt_sha256=digest(receipt_path),consumer_sha256=digest(Path(__file__)),scope='Original six micro attempts independently checked against actual stdout/ELF passPC/retirement floor/argv/source/binary/reference/CST/closed provenance/six complete tables. Fixed-only full equality and all three841 repeated controls protected. Gate supports starting original SPEC windows, not general FP correctness, a performance CPI/speedup or suite adoption.')
    (evidence/'micro-gate-audit.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(dict(passed=True,attempts=6,fixed_only_equal=True,three_repeated_baseline_controls_equal=True,rows=[{k:r[k] for k in ['variant','case','instructions','cycles']} for r in rows]),indent=2))


if __name__=='__main__':main()
