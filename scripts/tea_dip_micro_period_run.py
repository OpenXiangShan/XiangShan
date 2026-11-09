"""Calibrate cycle-by-cycle TEA/DIP on an already validated short protocol pair."""
from concurrent.futures import ThreadPoolExecutor,as_completed
from datetime import datetime,timezone
from pathlib import Path
import argparse
import json
import os
import re
import socket
import sqlite3
import subprocess
import time

from native_perf import read_blocks,counter_multisets
from tea_dip_query_endpoint_evidence import TABLES,verify
from tea_dip_wb_protocol_micro import digest,write


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir',type=Path,required=True)
    root=parser.parse_args().run_dir.resolve()
    assert socket.gethostname().startswith('node') and not(root/'run-state.json').exists()
    spec=json.loads((root/'experiment.json').read_text())
    assert spec['period']==1 and len(spec['cases'])==2
    state=dict(pid=os.getpid(),start_ticks=Path('/proc/self/stat').read_text().rsplit(')',1)[1].split()[19],host=socket.gethostname(),phase='preflight',completed=0,total=2)
    def save(**values):
        state.update(values,utc=datetime.now(timezone.utc).isoformat());write(root/'run-state.json',state)
    save()
    try:
        assert spec['baseline_closed']['ready'] and spec['baseline_closed']['all_original_guests_closed']
        verify(Path(spec['baseline_closed']['root']),spec['baseline_files'])
        for path,value in spec['input_sha256'].items():assert digest(path)==value,path
        cst=root/'tea-dip-period1.cst'
        cst.write_text('enableDip0 1\ndipSamplePeriod0 1\nenableTea0 1\nteaSamplePeriod0 1\n')
        save(phase='running')
        def run(case):
            variant=case['variant'];folder=root/variant;folder.mkdir()
            command=case['command'].copy()
            for flag,value in [('-c',str(case['core'])),('--dramsim3-outdir',str(folder)),('--db-path',str(folder/'simulator.db')),('--cst-file',str(cst))]:command[command.index(flag)+1]=value
            assert command[6]==case['binary'] and digest(case['binary'])==case['binary_sha256']
            write(folder/'command.json',command);began=time.monotonic()
            with(folder/'simulator_out.txt').open('w')as so,(folder/'simulator_err.txt').open('w')as se:
                child=subprocess.Popen(command,cwd=folder,stdin=subprocess.DEVNULL,stdout=so,stderr=se)
                process=dict(pid=child.pid,start_ticks=Path('/proc',str(child.pid),'stat').read_text().rsplit(')',1)[1].split()[19],host=socket.gethostname(),core=case['core'])
                write(folder/'process.json',process)
                write(folder/'launch-observation.json',dict(process=process,original_process_live=True,source=case['source'],binary_sha256=case['binary_sha256'],command=command,period=1))
                _,status,usage=os.wait4(child.pid,0);child.returncode=os.waitstatus_to_exitcode(status);rc=child.returncode
            (folder/'exitcode').write_text(str(rc))
            text=re.sub(r'\x1b\[[0-9;]*m','',(folder/'simulator_out.txt').read_text())
            counts=re.search(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)',text)
            instructions,cycles=[int(x.replace(',',''))for x in counts.groups()]if counts else[None,None]
            trap=re.search(r'HIT GOOD TRAP at pc = (0x[0-9a-fA-F]+)',text)
            flags={n:bool(re.search(r'^'+n+' = 1$',text,re.M))for n in ['enableTea0','enableDip0','teaSamplePeriod0','dipSamplePeriod0']}
            passed=rc==0 and trap is not None and int(trap[1],16)==int(case['pass_pc'],16) and instructions is not None and 1700<instructions<3000
            passed=passed and 'Difftest enabled'in text and all(flags.values()) and not any(x in text for x in ['ABORT','HIT CRITICAL','HIT BAD TRAP','pma check failed','EXCEEDING CYCLE/INSTR LIMIT'])
            result=dict(variant=variant,source=case['source'],binary_sha256=case['binary_sha256'],period=1,process=process,original_process_closed=True,exitcode=rc,passed=passed,difftest_passed=passed,sampler_flags=flags,instructions=instructions,cycles=cycles,observed_good_trap_pc=trap[1]if trap else None,host_wall_seconds=time.monotonic()-began,host_rusage={n:getattr(usage,'ru_'+n)for n in ['utime','stime','minflt','majflt','nvcsw','nivcsw','inblock','oublock','maxrss']},spec_sha256=digest(root/'experiment.json'),runner_sha256=digest(Path(__file__)),performance_speedup=None)
            write(folder/'result.json',result);assert passed,result
            before={stamp:counter_multisets(block)for stamp,block in read_blocks(Path(case['baseline'])/'simulator_err.txt').items()}
            after={stamp:counter_multisets(block)for stamp,block in read_blocks(folder/'simulator_err.txt').items()}
            auxiliary={};tables={}
            with sqlite3.connect((folder/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True)as new,sqlite3.connect((Path(case['baseline'])/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True)as old:
                for table in TABLES:tables[table]=[old.execute('SELECT count(*) FROM '+table).fetchone()[0],new.execute('SELECT count(*) FROM '+table).fetchone()[0]]
                for table in ['TeaCycleSummary_0','TeaNoCommit_0']:auxiliary[table]=old.execute('SELECT * FROM '+table+' ORDER BY ID').fetchall()==new.execute('SELECT * FROM '+table+' ORDER BY ID').fetchall()
            calibration=dict(passed=before==after and all(auxiliary.values()) and instructions==case['original_instructions'] and cycles==case['original_cycles'],all_native_multivalues_equal=before==after,auxiliary_tables_equal=auxiliary,original_instructions=case['original_instructions'],observed_instructions=instructions,original_cycles=case['original_cycles'],observed_cycles=cycles,table_row_counts_period1000_period1=tables,scope='Only this micro and exact binary: runtime sampler changes1000→1. Native multi-counters and independent summary/gap tables must be identical. Other sampled tables intentionally differ; no suite score or isolated host-overhead claim.')
            write(folder/'sampling-calibration.json',calibration);assert calibration['passed'],calibration
            return dict(variant=variant,passed=True,native_sampling_neutrality=True,instructions=instructions,cycles=cycles)
        rows=[]
        with ThreadPoolExecutor(max_workers=2)as pool:
            for future in as_completed([pool.submit(run,case)for case in spec['cases']]):
                rows.append(future.result());write(root/'results.json',rows);save(completed=len(rows))
        save(phase='complete',all_guests_Difftest_and_sampling_neutral=True)
    except BaseException as error:save(phase='failed',error=repr(error));raise


if __name__=='__main__':
    main()
