"""Compare closed GSIM ROI retirement counters with terminal instruction counts.

Usage: python3 scripts/tea_dip_retirement_boundary.py CONTROL_DIR CANDIDATE_DIR
The input folders contain result.json, simulator_out.txt and simulator_err.txt.
"""
import argparse
import hashlib
import json
import re
from pathlib import Path

PERF_COMMIT = re.compile(r'^\[PERF\s*\]\[time=\s*(\d+)\] .*\.rob: commitInstr, (\d+)$', re.M)
PERF_CLOCK = re.compile(r'^\[PERF\s*\]\[time=\s*(\d+)\] .*\.rob: clock_cycle, (\d+)$', re.M)
PERF_TEA_RETIRED = re.compile(r'^\[PERF\s*\]\[time=\s*(\d+)\] .*\.rob: tea_exact_endpoint_retired, (\d+)$', re.M)
TERMINAL = re.compile(r'EXCEEDING CYCLE/INSTR LIMIT at pc = (0x[0-9a-fA-F]+)')
TOTALS = re.compile(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)')


def inspect(folder):
    result_path = folder / 'result.json'
    stdout_path = folder / 'simulator_out.txt'
    stderr_path = folder / 'simulator_err.txt'
    result = json.loads(result_path.read_text())
    if not (result['original_process_closed'] and result['exitcode'] == 0 and result['difftest_passed']):
        raise ValueError(f'{folder}: run is not closed and correct')
    stdout = stdout_path.read_text()
    pc = TERMINAL.findall(stdout)
    totals = TOTALS.findall(stdout)
    stderr = stderr_path.read_text()
    native = [(int(stamp), int(count)) for stamp, count in PERF_COMMIT.findall(stderr)]
    clocks = [(int(stamp), int(count)) for stamp, count in PERF_CLOCK.findall(stderr)]
    tea = [(int(stamp), int(count)) for stamp, count in PERF_TEA_RETIRED.findall(stderr)]
    stamps = [stamp for stamp, _ in native]
    if len(pc) != 1 or len(totals) != 1 or len(native) != 2 or native[0][0] >= native[1][0] or stamps != [stamp for stamp, _ in clocks] or stamps != [stamp for stamp, _ in tea]:
        raise ValueError(f'{folder}: expected one terminal print and two ordered native dumps')
    terminal_instr, terminal_cycle = (int(value.replace(',', '')) for value in totals[0])
    if result['trap_counts'] != [terminal_instr, terminal_cycle]:
        raise ValueError(f'{folder}: runner and terminal counts disagree')
    native_total = native[0][1] + native[1][1]
    return dict(point=result['point'], final_pc=pc[0], terminal_instrCnt=terminal_instr,
                terminal_cycleCnt=terminal_cycle, native_dumps=[dict(stamp=stamp, commitInstr=count, clock_cycle=clocks[i][1], tea_exact_endpoint_retired=tea[i][1]) for i, (stamp, count) in enumerate(native)],
                native_total_commitInstr=native_total, native_minus_terminal_instrCnt=native_total-terminal_instr,
                tea_final_minus_native_total=tea[-1][1]-native_total,
                tea_final_minus_terminal_instrCnt=tea[-1][1]-terminal_instr,
                final_native_stamp_minus_terminal_cycleCnt=native[-1][0]-terminal_cycle,
                stdout_sha256=hashlib.sha256(stdout_path.read_bytes()).hexdigest(),
                stderr_sha256=hashlib.sha256(stderr_path.read_bytes()).hexdigest())


def compare(control, candidate):
    rows = dict(control=inspect(control), candidate=inspect(candidate))
    a, b = rows['control'], rows['candidate']
    if a['point'] != b['point']:
        raise ValueError('control and candidate are different points')
    return dict(point=a['point'], rows=rows,
                native_roi_retired_delta=b['native_dumps'][1]['commitInstr']-a['native_dumps'][1]['commitInstr'],
                terminal_instrCnt_delta=b['terminal_instrCnt']-a['terminal_instrCnt'],
                tea_final_retired_delta=b['native_dumps'][-1]['tea_exact_endpoint_retired']-a['native_dumps'][-1]['tea_exact_endpoint_retired'],
                final_pc_equal=a['final_pc'] == b['final_pc'], database_read=False,
                scope='Count and final-PC cross-check only. Matching endpoints do not prove an identical dynamic instruction prefix; no ROI denominator is silently adjusted.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('control', type=Path)
    parser.add_argument('candidate', type=Path)
    args = parser.parse_args()
    print(json.dumps(compare(args.control, args.candidate), indent=2))


if __name__ == '__main__':
    main()
