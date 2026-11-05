"""Compare fixed and variable-latency FP protocols with existing GSIM binaries, not suite scores."""
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import json
import os
import queue
import re
import socket
import sqlite3
import subprocess
import threading


def digest(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def write(path, value):
    temporary = path.with_suffix('.tmp')
    temporary.write_text(json.dumps(value, indent=2) + '\n')
    temporary.replace(path)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output-dir', required=True, type=Path)
    args = parser.parse_args(); out = args.output_dir.resolve()
    assert socket.gethostname().startswith('node') and not out.exists()
    environment = Path('/nfs/home/wujiabin/work/xs-env/rtl-dip-experiments-20261103')
    spec = json.loads((environment / 'experiment.json').read_text())
    build = json.loads((environment / 'fdiv-wb-class/build-state.json').read_text())
    assert build['source'] == '1f45c4ec4b47c1e35c6b57b0ed2caa3349201c40' and build['phase'] == 'complete'
    binaries = {'control841': dict(binary=spec['baseline'], sha256=spec['baseline_sha256'], source=spec['base']),
                'candidate1f45': dict(binary=build['binary'], sha256=build['binary_sha256'], source=build['source'])}
    assert all(digest(v['binary']) == v['sha256'] for v in binaries.values())
    assert digest(spec['reference']) == spec['reference_sha256']
    out.mkdir(parents=True); cst = out / 'tea-dip-period1000.cst'; cst.write_bytes((environment / 'tea-dip-period1000.cst').read_bytes())
    selectors = 'Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0'
    tools = Path('/nfs/home/wujiabin/software/riscv-gcc16/bin')
    bodies = {
        'fixed_only': '  fadd.d fs0, ft0, ft1\n  fmul.d fs1, fs0, ft0\n  fli.d fs2, 1.5\n  fsub.d fs3, fs1, fs2\n  fsd fs3, 0(t1)\n',
        'sqrt_div_mixed': '  fsqrt.d fs0, ft1\n  fdiv.d fs0, ft0, fs0\n  fmadd.d fs1, fs0, ft1, ft0\n  fsd fs1, 0(t1)\n  fadd.d fs2, fs0, ft0\n',
        'div_fli_mixed': '  fdiv.d fs0, ft1, ft0\n  fli.d fs1, 1.5\n  fadd.d fs2, fs0, fs1\n  fmul.d fs3, fs2, ft0\n  fsd fs3, 0(t1)\n',
    }
    cases = []
    for name, body in bodies.items():
        folder = out / 'images' / name; folder.mkdir(parents=True)
        assembly = '.section .text\n.globl _start\n.option norelax\n_start:\n  li t0, 0x6000\n  csrs mstatus, t0\n  la t0, values\n  fld ft0, 0(t0)\n  fld ft1, 8(t0)\n  la t1, sink\n  li s0, 256\nloop:\n' + body + '  addi s0, s0, -1\n  bnez s0, loop\n  li a0, 0\n_pass:\n  .word 0x0000006b\n.section .data\n.balign 8\nvalues:\n  .dword 0x3ff0000000000000, 0x4010000000000000\nsink:\n  .zero 8\n'
        source = folder / 'program.S'; source.write_text(assembly); elf = folder / 'program.elf'; image = folder / 'program.bin'
        command = [str(tools / 'riscv64-unknown-linux-gnu-gcc'), '-march=rv64gc_zfa', '-mabi=lp64d', '-mcmodel=medany', '-nostdlib', '-nostartfiles', '-static', '-Wl,--no-relax', '-Wl,-Ttext=0x80000000', '-Wl,-e,_start', str(source), '-o', str(elf)]
        subprocess.run(command, check=True, capture_output=True)
        subprocess.run([str(tools / 'riscv64-unknown-linux-gnu-objcopy'), '-O', 'binary', str(elf), str(image)], check=True)
        symbols = subprocess.check_output([str(tools / 'riscv64-unknown-linux-gnu-nm'), str(elf)], text=True)
        pass_pc = int(next(line.split()[0] for line in symbols.splitlines() if line.split()[-1] == '_pass'), 16)
        case = dict(case=name,image=str(image),image_sha256=digest(image),source_sha256=digest(source),pass_pc=hex(pass_pc),build_command=command)
        write(folder / 'source.json', case); cases.append(case)
    state = dict(pid=os.getpid(),start_ticks=Path('/proc/self/stat').read_text().rsplit(')',1)[1].split()[19],host=socket.gethostname(),phase='running',completed=0,total=6)
    write(out/'run-state.json',state)
    write(out/'experiment.json',dict(utc=datetime.now(timezone.utc).isoformat(),binaries=binaries,reference=spec['reference'],reference_sha256=spec['reference_sha256'],cst_sha256=digest(cst),cases=cases,cores=[132,133,134,135],seed=99,instruction_limit=100000,cycle_limit=200000,period=1000,selectors=selectors,runner_sha256=digest(Path(__file__)),scope='Six focused diagnostic attempts with existing binaries and unchanged functional resources. Each has256iterations, bounded200kcycles, exact passPC and1700–3000 retired requirement; failed/limited attempts have no intended-work CPI or suite score.'))
    slots = queue.Queue()
    for core in [132,133,134,135]:slots.put(core)
    worker = threading.local()
    def init():worker.core=slots.get_nowait()
    def run(job):
        variant,case=job;identity=binaries[variant];folder=out/'runs'/variant/case['case'];folder.mkdir(parents=True)
        command=['nice','-n','10','taskset','-c',str(worker.core),identity['binary'],'-I','100000','-C','200000','-s','99','-i',case['image'],'--diff',spec['reference'],'--dramsim3-outdir',str(folder),'--cst-file',str(cst),'--dump-select-db',selectors,'--db-path',str(folder/'simulator.db')]
        write(folder/'command.json',command)
        with (folder/'simulator_out.txt').open('w') as stdout,(folder/'simulator_err.txt').open('w') as stderr:
            child=subprocess.Popen(command,cwd=folder,stdin=subprocess.DEVNULL,stdout=stdout,stderr=stderr)
            process=dict(pid=child.pid,start_ticks=Path('/proc',str(child.pid),'stat').read_text().rsplit(')',1)[1].split()[19],host=socket.gethostname(),core=worker.core)
            write(folder/'process.json',process); print(json.dumps(dict(event='guest_started',variant=variant,case=case['case'],**process)),flush=True); rc=child.wait()
        (folder/'exitcode').write_text(str(rc));text=re.sub(r'\x1b\[[0-9;]*m','',(folder/'simulator_out.txt').read_text());match=re.search(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)',text)
        instructions,cycles=[int(v.replace(',','')) for v in match.groups()] if match else [None,None]
        trap=re.search(r'HIT GOOD TRAP at pc = (0x[0-9a-fA-F]+)',text)
        passed=rc==0 and trap is not None and int(trap[1],16)==int(case['pass_pc'],16) and 1700<instructions<3000 and not any(s in text for s in ['ABORT','HIT CRITICAL','HIT BAD TRAP','pma check failed'])
        flags={name:bool(re.search(r'^'+name+' = '+str(value)+'$',text,re.M)) for name,value in [('enableTea0',1),('enableDip0',1),('teaSamplePeriod0',1000),('dipSamplePeriod0',1000)]};assert all(flags.values()) and 'Difftest enabled' in text
        tables=None
        if (folder/'simulator.db').exists():
            with sqlite3.connect((folder/'simulator.db').as_uri()+'?mode=ro',uri=True) as db:tables={name:db.execute('SELECT count(*) FROM '+name).fetchone()[0] for name in selectors.split()}
        result=dict(variant=variant,case=case['case'],process=process,original_process_closed=True,exitcode=rc,instructions=instructions,cycles=cycles,pass_pc=case['pass_pc'],observed_good_trap_pc=trap[1] if trap else None,passed=passed,difftest_mismatches=re.findall(r'\w+ different at pc = [^\n]+',text),cycle_or_instruction_limit='EXCEEDING CYCLE/INSTR LIMIT' in text,sampler_flags=flags,trace_table_rows=tables,binary_sha256=identity['sha256'],image_sha256=case['image_sha256'],performance_speedup=None)
        write(folder/'result.json',result);return result
    rows=[]
    with ThreadPoolExecutor(max_workers=4,initializer=init) as pool:
        for future in as_completed([pool.submit(run,(variant,case)) for variant in binaries for case in cases]):
            row=future.result();rows.append(row);write(out/'results.json',rows);state.update(completed=len(rows),utc=datetime.now(timezone.utc).isoformat());write(out/'run-state.json',state);print(json.dumps(dict(event='guest_closed',variant=row['variant'],case=row['case'],passed=row['passed'],exitcode=row['exitcode'],instructions=row['instructions'],cycles=row['cycles'])),flush=True)
    state.update(phase='complete',utc=datetime.now(timezone.utc).isoformat());write(out/'run-state.json',state)


if __name__=='__main__':main()
