"""Count distinct sampled TEA targets and repeated source samples inside a core ROI.

These are length-biased samples, not all dynamic instructions, cache misses,
service latencies, cross-run instruction matches, or recoverable CPU cycles.
"""
import argparse
from collections import Counter, defaultdict
import json
from pathlib import Path
import sqlite3

from tea_dip_fate_state_joint import analyze as validate
from tea_dip_query_endpoint_evidence import digest
from tea_visualize import normalize_pc, target_key


def analyze(folder, receipt_path, *, source_range=None):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    checked = validate(folder, receipt_path)
    begin, end = source_range if source_range is not None else (checked['roi_begin'], checked['roi_end'])
    assert checked['roi_begin'] <= begin < end <= checked['roi_end'], 'Source range must lie inside the core ROI'
    groups = defaultdict(list)
    with sqlite3.connect((folder / 'simulator.db').as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        finals = {target_key(row, 'MEMBER_TARGET_'): row
                  for row in db.execute('SELECT * FROM TeaDelayedFinal_0 WHERE ISDRAIN=0')}
        samples = list(db.execute('SELECT * FROM Tea_0 WHERE STATE=1 AND CYCLE>=? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end)))
        for sample in samples:
            key = target_key(sample, 'TARGET_') if sample['TARGETVALID'] else None
            final = finals.get(key)
            status = ('invalid_target' if key is None else 'unresolved' if final is None
                      else 'cancelled' if final['CANCELLED'] else 'resolved')
            pc = normalize_pc(final['MEMBER_PC']) if final is not None else None
            groups[pc, status].append(dict(sample_cycle=sample['CYCLE'], target=key,
                                          final_cycle=final['CYCLE'] if final is not None else None))
    profiles = []
    for (pc, status), rows in sorted(groups.items(), key=lambda item: (-len(item[1]), str(item[0]))):
        targets = defaultdict(list)
        for row in rows:
            if row['target'] is not None:
                targets[row['target']].append(row)
        records = [dict(allocation_cycle=key[0], rob_index=key[1], rank=key[2],
                        sample_cycles=[row['sample_cycle'] for row in selected], final_cycle=selected[0]['final_cycle'])
                   for key, selected in sorted(targets.items())]
        histogram = Counter(len(rows) for rows in targets.values())
        valid = sum(size * count for size, count in histogram.items())
        assert valid == sum(row['target'] is not None for row in rows)
        profiles.append(dict(architectural_PC=pc, status=status, source_samples=len(rows),
                             distinct_sampled_targets=len(targets), repeated_source_samples=valid-len(targets),
                             samples_per_target_histogram={str(k): histogram[k] for k in sorted(histogram)}, targets=records,
                             unidentified_sample_cycles=[row['sample_cycle'] for row in rows if row['target'] is None]))
    assert sum(row['source_samples'] for row in profiles) == len(samples)
    database = folder / 'simulator.db'
    return dict(passed=True, point=folder.name, source=checked['source'], source_begin=begin, source_end=end,
                roi_begin=checked['roi_begin'], roi_end=checked['roi_end'], sample_period=checked['sample_period'],
                Stalled_source_samples=len(samples), profiles=profiles,
                input_sha256={str(p): digest(p) for p in [receipt_path, database]},
                tool_sha256={name: digest(Path(__file__).parent / name) for name in
                             [Path(__file__).name, 'tea_dip_fate_state_joint.py', 'tea_visualize.py']},
                scope='Distinct allocation-cycle/ROB-index/rank targets within this run and selected source interval. '
                      'All later non-drain finals are retained. Repetition counts extra selected samples after the first '
                      'for each target; this is not complete target lifetime or a cross-run identity. Invalid targets, '
                      'unresolved and cancelled samples remain separate. Sampling is length-biased: target counts are '
                      'not instruction frequency, miss counts, memory latency, exclusive CPU cost or saved cycles.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--source-range', type=lambda value: int(value, 0), nargs=2, metavar=('BEGIN', 'END'))
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt, source_range=args.source_range), indent=2) + '\n')


if __name__ == '__main__':
    main()
