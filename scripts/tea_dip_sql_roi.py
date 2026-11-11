"""Recover a warmed ROI from current clear-reset TEA SQL with shared perf clean/dump controls."""
from pathlib import Path
import argparse
import hashlib
import json
import sqlite3

from native_perf import read_blocks, unique_counters
from tea_dip_query_endpoint_evidence import verify

STATES = ['Computing', 'Stalled', 'Flushed', 'Drained']


def reconstruct(warmup_cycles, roi_cycles, rows):
    assert rows and roi_cycles > 0
    origin = rows[0]['CYCLE'] - sum(rows[0]['STATES_' + str(i)] for i in range(4))
    previous_cycle = origin
    previous = [0] * 4
    for row in rows:
        counts = [row['STATES_' + str(i)] for i in range(4)]
        assert not row['CLEARED'] and not row['CLOSED'], 'Expected one continuously enabled, warmup-cleared ROI'
        assert row['CYCLE'] > previous_cycle and sum(counts) == row['CYCLE'] - origin
        assert all(a <= b for a, b in zip(previous, counts)), 'TEA counter prefix changed within ROI'
        previous_cycle, previous = row['CYCLE'], counts
    assert sum(previous) == roi_cycles, 'SQL does not cover the complete reported native ROI'
    epoch_offset = warmup_cycles - origin
    return dict(SQL_begin=origin, SQL_end=rows[-1]['CYCLE'], native_epoch_offset=epoch_offset,
                cycles=roi_cycles, states=dict(zip(STATES, previous)), SQL_retired=rows[-1]['RETIRED'])


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
    assert all(result['sampler_flags'].values())
    assert result['source'] == receipt['source'] and result['process'] == receipt['process']
    verify(folder, receipt['files'])
    command = json.loads((folder / 'command.json').read_text())
    assert '-W' in command, 'This reader requires a warmup-cleared ROI, not a single CoreMark interval'
    blocks = sorted(read_blocks(folder / 'simulator_err.txt').items())
    assert len(blocks) == 2
    frames = []
    prefix = 0
    for stamp, block in blocks:
        counters = unique_counters(block)
        def scalar(suffix):
            value, = [v for k, v in counters.items() if k.endswith('.rob.' + suffix)]
            return value
        cycles, retired = scalar('clock_cycle'), scalar('commitInstr')
        prefix += cycles
        assert prefix == stamp + 1, 'Unexpected native clock prefix'
        frames.append(dict(print_stamp=stamp, cycles=cycles, retired=retired))
    with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        rows = [dict(row) for row in db.execute('SELECT * FROM TeaCycleSummary_0 ORDER BY CYCLE,ID')]
    warmup, roi = frames
    data = reconstruct(warmup['cycles'], roi['cycles'], rows)
    retired = roi['retired']
    data.update(native_retired=retired, SQL_minus_native_retired=data['SQL_retired'] - retired,
                native_CPI=roi['cycles'] / retired,
                state_CPI={state: count / retired for state, count in data['states'].items()})
    digest = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
    return dict(passed=True, point=folder.name, source=result['source'], frames=frames, ROI=data,
                source_sha256={str(p): digest(p) for p in [receipt_path, Path(__file__)]},
                scope='Current warmup-cleared ROI-only SQL, source-audited shared perf clean/dump controls. '
                      'Counter prefixes and native interval duration must agree; epoch offset is measured, '
                      'not assumed to be three. State cycles cover this collection interval. '
                      'Retirement/dynamic-prefix identity and unique causal attribution remain unverified.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    data = analyze(args.folder.resolve(), args.receipt.resolve())
    args.output.write_text(json.dumps(data, indent=2) + '\n')
    print(json.dumps(dict(passed=True, point=data['point'], ROI=data['ROI']), indent=2))


if __name__ == '__main__':
    main()
