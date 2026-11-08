"""Run the registered formal and guard batches after the actual FU-ready short-window gate."""
from concurrent.futures import ThreadPoolExecutor,as_completed
from datetime import datetime,timezone
from pathlib import Path
import argparse,json,os,queue,socket,threading
from tea_dip_wb_protocol_micro import digest,write
from tea_dip_fdiv_ready_run import completed_guest


def main():
    parser=argparse.ArgumentParser(description=__doc__);parser.add_argument('--run-dir',required=True,type=Path)
    root=parser.parse_args().run_dir.resolve();spec=json.loads((root/'experiment.json').read_text())
    assert socket.gethostname().startswith('node')and not(root/'run-state.json').exists()
    gate=json.loads((root/'short-gate.json').read_text())
    assert digest(root/'short-gate.json')==spec['short_gate_sha256']and gate['validation_passed']and not gate['pending']
    sources=('8f8ece60ac8b08b358743bc9db15a5a227dde0b8','39a163eb3910ff9ecab02df4ac8e3c99d772feeb')if spec.get('turnover')else('39a163eb3910ff9ecab02df4ac8e3c99d772feeb','2915a6b022bd957570bd961a01611026c2220fb7')
    assert gate['source']==spec['builds']['candidate']['source']==sources[0]
    assert gate['control_source']==spec['builds']['control']['source']==sources[1]
    assert len(gate['short_pairs'])==3 and any(r['comparison']['observed_native_speedup']>0 for r in gate['short_pairs'])
    for path,sha in spec['input_hashes'].items():assert digest(Path(path))==sha,path
    for build in spec['builds'].values():assert build['phase']=='complete'and digest(Path(build['binary']))==build['binary_sha256']
    state=dict(pid=os.getpid(),start_ticks=Path('/proc/self/stat').read_text().rsplit(')',1)[1].split()[19],host=socket.gethostname(),phase='running',completed=0,total=len(spec['cases']))
    def save(**values):
        state.update(values,utc=datetime.now(timezone.utc).isoformat());write(root/'run-state.json',state)
    save();slots=queue.Queue()
    for core in spec['cores']:slots.put(core)
    worker=threading.local()
    def init():worker.core=slots.get_nowait()
    def execute(case):
        value=dict(case,core=worker.core);folder=root/case['variant'];folder.mkdir(exist_ok=True)
        result=completed_guest(folder/case['point'],spec['builds'][case['variant']],value)
        return dict(variant=case['variant'],**result)
    try:
        rows=[]
        with ThreadPoolExecutor(max_workers=len(spec['cores']),initializer=init)as pool:
            for future in as_completed([pool.submit(execute,case)for case in spec['cases']]):
                rows.append(future.result());write(root/'results.json',rows);save(completed=len(rows))
        save(phase='complete',all_guests_difftest_passed=True)
    except BaseException as error:
        save(phase='failed',error=repr(error));raise


if __name__=='__main__':main()
