#!/usr/bin/env python3

import sqlite3 as sql
import argparse
import subprocess
import tempfile

class dasm_query:
    def __init__(self, cmd='riscv64-linux-gnu-objdump', enable_cache=True):
        if cmd.endswith('objdump'):
            self.cmd = cmd
            self.mode = 'objdump'
        elif cmd == 'spike-dasm':
            self.cmd = cmd
            self.mode = 'spike-dasm'
            self.spike = subprocess.Popen(
                [self.cmd],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
        if enable_cache:
            self.cache = {}
        else:
            self.cache = None
    def __del__(self):
        if self.mode == 'spike-dasm':
            self.spike.terminate()
            self.spike.wait()
    def dasm(self, instr):
        if self.cache is not None and instr in self.cache:
            return self.cache[instr]
        if self.mode == 'spike-dasm':
            self.spike.stdin.write(f"DASM(0x{instr:x})\n")
            self.spike.stdin.flush()
            res = self.spike.stdout.readline().strip()
            if self.cache is not None:
                self.cache[instr] = res
            return res
        elif self.mode == 'objdump':
            # write instr.to_bytes(4, byteorder='little')
            with tempfile.NamedTemporaryFile() as f:
                f.write(instr.to_bytes(4, byteorder='little'))
                f.flush()
                res = subprocess.run(
                    [self.cmd, '-b', 'binary', '-m', 'riscv:rv64', '-M,max', '-D', f.name],
                    capture_output=True,
                    text=True
                )
                lines = res.stdout.splitlines()
                for line in lines:
                    if line.startswith("   0:\t"):
                        res = "\t".join(line.split("\t")[2:])
                        if self.cache is not None:
                            self.cache[instr] = res
                        return res
                return ""

parser = argparse.ArgumentParser()
parser.add_argument('sqldb')
parser.add_argument('-v', '--visual', action='store_true', default=False)
parser.add_argument('-z', '--zoom', action='store', type=float, default=1)
parser.add_argument('-p', '--period', action='store', default=1)
parser.add_argument('-d', '--dasm', action='store', help='dasm command, can be "spike-dasm" or "riscv64-linux-gnu-objdump"', default=None)
parser.add_argument('-s', '--spec', action='store_true', default=False, help='Show speculative execution')
parser.add_argument('-l', '--singleline', action='store_true', default=False, help='Single line per instruction (only with -v)')
parser.add_argument('-c', '--cycle', action='store_true', default=False, help='Show cycle number (only with -v)')

args = parser.parse_args()
singleline = args.singleline
showcycle = args.cycle
sqldb = args.sqldb
dasm = None
if args.dasm is not None:
    dasm = dasm_query(args.dasm)

tick_per_cycle = int(args.period)
cycle_per_line = int(100 * args.zoom)

stages = {
    'AtFetch': 'f',
    'AtDecode': 'd',
    'AtRename': 'r',
    'AtDispQue': 'D',
    'AtIssueQue': 'i',
    'AtIssueArb': 'a',
    'AtIssueReadReg': 'g',
    'AtFU': 'e',
    'AtBypassVal': 'b',
    'AtWriteVal': 'w',
    'AtCommit': 'c'
}

def non_stage():
    return '.'

def stage(x):
    return stages[x]

def dump_visual(pos, records):
    empty_line = list(f"[{non_stage() * cycle_per_line}]")
    line_buffer = empty_line.copy()
    line = ""
    last_key = None
    last_line = None # Only used for multi-line mode
    def fill_line_pos(pos, char):
        nonlocal line_buffer
        line_buffer[1 + pos] = char
    def check_should_commit_last_line(pos = None):
        nonlocal line, line_buffer
        # pos is None means force commit
        if pos is None or (last_line is not None and \
                           pos // cycle_per_line != last_line and \
                           line_buffer != empty_line):
            line_number = f"{last_line * cycle_per_line:07d}:" if showcycle else ""
            line += line_number + "".join(line_buffer) + "\n"
            line_buffer = empty_line.copy()
    for key, value in sorted(pos.items(), key=lambda item: item[1]):
        assert key in stages
        if value == 0:
            continue
        if last_key is not None:
            if singleline:
                for i in range(pos[last_key], value):
                    fill_line_pos(i % cycle_per_line, stage(last_key))
            else:
                # multi-line mode
                # if current pos brings a new line, draw last line and clear buffer
                for i in range(pos[last_key], value):
                    check_should_commit_last_line(i)
                    fill_line_pos(i % cycle_per_line, stage(last_key))
                    last_line = i // cycle_per_line
        last_key = key
    # draw last stage
    if not singleline and last_line is not None:
        check_should_commit_last_line(pos[last_key])
    else:
        # fill last_line in single line mode
        # and for multi-line mode but has only fetch stage
        last_line = pos[last_key] // cycle_per_line
    fill_line_pos(pos[last_key] % cycle_per_line, stage(last_key))
    check_should_commit_last_line()
    line = line.strip() # remove last \n
    instr = records['DisAsm']
    pc = records['PC']
    if dasm is not None and type(instr) == int:
        instr = f"{instr:08x} {dasm.dasm(instr)}"
    line += f"{pc}: {instr}"
    print(line)


def dump_txt(pos, records):
    for key in pos:
        assert key in stages
        print(f'{stage(key)}{pos[key]}', end=' ')
    print(records)


dump = dump_txt
if args.visual:
    dump = dump_visual

with sql.connect(sqldb) as con:
    cur = con.cursor()
    cur.execute(f"SELECT * FROM LifeTimeCommitTrace {'WHERE AtCommit != 0' if not args.spec else ''}")
    col_name = [i[0] for i in cur.description]
    col_name = col_name[1:] # Remove ID
    rows = cur.fetchall()
    for row in rows:
        row = row[1:] # Remove ID
        pos = dict()
        records = dict()
        i = 0
        for val in row:
            if col_name[i].startswith('At'):
                pos[col_name[i]] = val // tick_per_cycle
            elif col_name[i].startswith('PC'):
                if val < 0:
                    val = val + 1 << 64
                records[col_name[i]] = f"{val:016x}"
            else:
                records[col_name[i]] = val
            i += 1
        dump(pos, records)
