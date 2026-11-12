"""Validate a closed GSIM queue probe against its native-cycle L2 trace.

The four queues have capacity 16. A queue occupancy below 11 excludes the
TXREQ credit gate whose pipeline plus reserved-credit contribution is at most 5.
This is a capacity bound, not a critical CPU cycle or performance estimate.
"""
import argparse
from collections import Counter
import csv
from itertools import zip_longest
import json
from pathlib import Path
import sqlite3

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import digest, verify


def analyze(folder, receipt_path):
    folder = Path(folder).resolve()
    receipt = json.loads(Path(receipt_path).read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    process = result['process']
    stat = Path('/proc', str(process['pid']), 'stat')
    assert not stat.exists() or stat.read_text().rsplit(')', 1)[1].split()[19] != str(process['start_ticks'])
    assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
    assert receipt['source'] == result['source']
    verify(folder, receipt['files'])
    probe = json.loads((folder / 'probe-receipt.json').read_text())
    verify(folder, probe['files'])
    assert probe['closure_receipt_sha256'] == digest(Path(receipt_path))
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    assert proof['passed'] and proof['original_files_unchanged']
    assert proof['binary_sha256'] == result['binary_sha256']
    blocks = read_blocks(folder / 'simulator_err.txt')
    begin, end = (min(blocks) + 1 if len(blocks) == 2 else 0), max(blocks) + 1
    summaries = {name: {'rows': 0, 'peak': [0] * 4, 'histogram': [Counter() for _ in range(4)]}
                 for name in ['whole_capture', 'scored_ROI']}
    fields = ['native_cycle', 'clean', 'native_dump', 'log_enable', 'valid_mask', 'ready_mask']
    query = 'SELECT NATIVECYCLE,CLEAN,NATIVEDUMP,LOGENABLE,EVENTS_VALIDMASK,EVENTS_READYMASK FROM L2TxreqArbTrace ORDER BY ID'
    rows = 0
    with (folder / 'txreq-queue-probe.csv').open() as stream, sqlite3.connect((folder / 'simulator.db').as_uri() + '?mode=ro', uri=True) as db:
        for raw, expected in zip_longest(csv.DictReader(stream), db.execute(query)):
            assert raw is not None and expected is not None, 'Probe and SQL trace lengths differ'
            row = {key: int(value) for key, value in raw.items()}
            assert tuple(row[field] for field in fields) == expected, (rows, row, expected)
            assert rows == 0 or row['native_cycle'] > previous_cycle
            previous_cycle = row['native_cycle']
            for bank in range(4):
                before, after = row[f'q{bank}'], row[f'next_q{bank}']
                enq, deq = (row['enq_mask'] >> bank) & 1, (row['deq_mask'] >> bank) & 1
                assert 0 <= before <= 16 and 0 <= after <= 16
                assert bool(before) == bool(row['valid_mask'] & (1 << bank))
                assert deq == bool(row['ready_mask'] & (1 << bank))
                assert after + deq == before + enq
                for name, summary in summaries.items():
                    if name == 'whole_capture' or begin <= row['native_cycle'] < end:
                        summary['peak'][bank] = max(summary['peak'][bank], before, after)
                        summary['histogram'][bank][before] += 1
            summaries['whole_capture']['rows'] += 1
            summaries['scored_ROI']['rows'] += begin <= row['native_cycle'] < end
            rows += 1
    assert rows > 0
    for summary in summaries.values():
        assert all(sum(hist.values()) == summary['rows'] for hist in summary['histogram'])
        summary['queue_plus_pipeline_upper_bound'] = [peak + 5 for peak in summary['peak']]
        summary['capacity_gate_excluded_by_upper_bound'] = all(peak < 11 for peak in summary['peak'])
    return dict(passed=True, native_ROI=dict(begin=begin, end=end), summaries=summaries,
                probe_rows_equal_SQL=True, every_observed_transition_conserved=True,
                scope='Same-native-cycle CSV/SQL and pointer transition calibration. The wrapped step asserts inactive steps have empty old queues. Histograms cover emitted trace rows, not all CPU cycles. Capacity exclusion uses all capture peaks; it does not establish a unique critical request or saved cycles.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    args = parser.parse_args()
    print(json.dumps(analyze(args.folder, args.folder / 'closure-receipt.json'), indent=2))


if __name__ == '__main__':
    main()
