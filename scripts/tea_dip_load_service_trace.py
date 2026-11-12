"""Validate closed S2 attempts against native marginals, retaining per-PC cause sets.

An attempt is not a unique load lifetime or a CPU lost cycle. In particular,
member identifiers captured at S2 do not establish final writeback association.
"""
import argparse
from collections import Counter, defaultdict
import csv
import json
from pathlib import Path
import sqlite3

from native_perf import read_blocks
from tea_dip_clock_alignment import core_native_alignment
from tea_dip_query_endpoint_evidence import digest, verify

METRICS = ['valid', 'fire', 'fire_first_issue', 'dcache_miss', 'dcache_miss_first_issue',
           'store_full_forward', 'full_forward']
CAUSES = ['C_UNCACHE', 'C_SMF', 'C_MA', 'C_TM', 'C_FF', 'C_DR', 'C_DM',
          'C_WF', 'C_BC', 'C_RAR', 'C_RAW', 'C_NK', 'C_MF']


def measure(events, boundaries, blocks):
    dumps = [row['core_cycle'] for row in boundaries if row['native_dump']]
    assert len(dumps) == len(blocks) and len(set(dumps)) == len(dumps)
    offsets = {native - core for native, core in zip(sorted(blocks), dumps)}
    offset, = offsets
    phases = []
    for (end, block), core_end in zip(sorted(blocks.items()), dumps):
        begin = max([row['core_cycle'] for row in boundaries if row['clean'] and row['core_cycle'] < core_end], default=-1)
        selected = [row for row in events if begin < row['core_cycle'] <= core_end]
        counts, profiles = [Counter() for _ in range(3)], defaultdict(Counter)
        cause_sets = Counter()
        for row in selected:
            lane = counts[row['lane']]
            lane['valid'] += 1
            if not row['fire']:
                continue
            first = bool(row['entrance'] & (32 | 64))
            lane.update(fire=1, fire_first_issue=int(first), dcache_miss=row['raw_miss'],
                        dcache_miss_first_issue=int(first and row['raw_miss']),
                        store_full_forward=row['store_full_forward'], full_forward=row['full_forward'])
            if row['instr_type'] != 1:
                continue
            profile = profiles[hex(row['pc'])]
            profile.update(attempts=1, raw_miss=row['raw_miss'], full_forward=row['full_forward'],
                           store_full_forward=row['store_full_forward'], need_dcache=row['need_dcache'])
            cause_sets[str(row['cause_mask'])] += 1
            for bit, name in enumerate(CAUSES):
                profile['cause_' + name] += bool(row['cause_mask'] & (1 << bit))
        marginals = []
        for lane in range(3):
            for metric in METRICS:
                values, = [values for name, values in block.items() if name.endswith(f'.LoadUnit_{lane}.s2.{metric}')]
                expected, = values
                actual = counts[lane][metric]
                assert actual == expected, (end, lane, metric, actual, expected)
                marginals.append(dict(lane=lane, metric=metric, value=actual))
        phases.append(dict(begin_core_exclusive=begin, end_core_inclusive=core_end, native_dump=end,
                           events=len(selected), native_marginals=marginals,
                           scalar_PC_attempts=dict(profiles), scalar_cause_sets=dict(cause_sets)))
    assigned = sum(phase['events'] for phase in phases)
    assert assigned <= len(events)
    return dict(passed=True, core_to_native_offset=offset, phases=phases, events=len(events),
                events_outside_native_phases=len(events) - assigned, native_marginals_equal=True,
                WB_identity='not_captured',
                scope='Actual S2 attempt cycles and qualified events. Per-PC scalar counts preserve multi-bit cause combinations and do not identify unique dynamic loads, first data arrival, final-WB lifetimes, CPU lost cycles or recoverable CPI. Core/native offset is measured from this capture; missing unique WB association remains unclaimed.')


def analyze(folder, receipt_path):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and result['original_process_closed']
    assert result['difftest_passed'] and result['exitcode'] == 0 and all(result['sampler_flags'].values())
    assert result['process'] == receipt['process'] and result['source'] == receipt['source']
    stat = Path('/proc', str(result['process']['pid']), 'stat')
    assert not stat.exists() or stat.read_text().rsplit(')', 1)[1].split()[19] != str(result['process']['start_ticks'])
    verify(folder, receipt['files'])
    probe = json.loads((folder / 'probe-receipt.json').read_text())
    assert probe['closure_receipt_sha256'] == digest(receipt_path)
    verify(folder, probe['files'])
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    assert proof['passed'] and proof['original_files_unchanged'] and proof['binary_sha256'] == result['binary_sha256']
    assert len(proof['binding']['lanes']) == 3 and proof['binding']['WB_identity'] == 'not_captured'
    events, boundaries = [], []
    with (folder / 'load-service-probe.csv').open() as stream:
        for raw in csv.DictReader(stream):
            row = {key: int(value) for key, value in raw.items()}
            if row['lane'] == 3:
                assert row['clean'] or row['native_dump']
                boundaries.append(row)
                continue
            assert 0 <= row['lane'] < 3 and row['rob_index'] < 160 and row['rank'] < 8
            assert row['instr_type'] in (1, 2, 4) and 0 <= row['cause_mask'] < 8192
            assert all(row[key] in (0, 1) for key in ['clean', 'native_dump', 'fire', 'raw_miss', 'full_forward', 'store_full_forward', 'need_dcache', 'rob_flag', 'rob_former'])
            assert not row['store_full_forward'] or row['full_forward']
            assert not events or (row['core_cycle'], row['lane']) > (events[-1]['core_cycle'], events[-1]['lane'])
            events.append(row)
    output = measure(events, boundaries, read_blocks(folder / 'simulator_err.txt'))
    with sqlite3.connect((folder / 'simulator.db').as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        alignment = core_native_alignment(db, folder / 'simulator_err.txt')
    assert output['core_to_native_offset'] == alignment['core_to_native_offset'], 'S2 clock differs from independent SQL/native dump anchors'
    output['clock_alignment'] = alignment
    output.update(source=result['source'], receipt_sha256=digest(receipt_path), probe_sha256=digest(folder / 'load-service-probe.csv'))
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt), indent=2) + '\n')


if __name__ == '__main__':
    main()
