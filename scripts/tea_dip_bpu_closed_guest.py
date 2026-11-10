"""Read BPU joint mispredict events from one closed guest and audit native margins."""
from collections import Counter
from pathlib import Path
import argparse
import json
import sqlite3

from native_perf import read_blocks, unique_counters
from tea_dip_bpu_commit_trace import COLUMNS, S1, S3, REASONS, summarize
from tea_dip_bpu_native_cohort import FTQ, cohorts
from tea_dip_query_endpoint_evidence import digest, verify


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    assert result['original_process_closed'] and result['exitcode'] == 0 and result['difftest_passed']
    assert all(result['sampler_flags'].values())
    assert result['source'] == receipt['source'] and result['process'] == receipt['process']
    verify(folder, receipt['files'])
    command = json.loads((folder / 'command.json').read_text())
    assert 'BpuCommitMispredict' in command[command.index('--dump-select-db') + 1].split()
    blocks = sorted(read_blocks(folder / 'simulator_err.txt').items())
    assert len(blocks) == (2 if '-W' in command else 1)
    phases = []
    with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        assert {r['name'] for r in db.execute('PRAGMA table_info(BpuCommitMispredict)')} == COLUMNS | {'SOURCE_S2SOURCE'}
        cursor = iter(db.execute('SELECT * FROM BpuCommitMispredict ORDER BY ID'))
        previous_id, previous_stamp = 0, -1

        def take(amount):
            nonlocal previous_id, previous_stamp
            for _ in range(amount):
                row = next(cursor)
                assert row['ID'] == previous_id + 1 and row['STAMP'] > previous_stamp
                previous_id, previous_stamp = row['ID'], row['STAMP']
                yield row

        labels = ['warmup', 'ROI'] if len(blocks) == 2 else ['coremark']
        for label, (stamp, block) in zip(labels, blocks):
            counters = unique_counters(block)
            expected = cohorts(counters)
            s1_names = S1 + ['UbtbUras', 'AbtbUras']
            s2_names = ['Tage', 'Mbtb', 'Fallthrough']
            s2_sources, selected_s2, selected = Counter(), Counter(), Counter()

            def events():
                for row in take(expected['committed_mispredict_entries']):
                    source = s2_names[row['SOURCE_S2SOURCE']]
                    s2_sources[source] += 1
                    if row['SOURCE_S3OVERRIDE']:
                        stage, name = 'S3', S3[row['SOURCE_S3SOURCE']]
                    elif row['SOURCE_S2OVERRIDE']:
                        stage, name = 'S2', source
                        selected_s2[source] += 1
                    else:
                        stage, name = 'S1', s1_names[row['SOURCE_S1SOURCE']]
                    selected[stage, name, REASONS[row['REASON']]] += 1
                    yield row

            summary = summarize(events(), expected, s1_names=s1_names)
            for counter, prefix in [(s2_sources, 'commit_branch_mispredicts_s2_source_'),
                                    (selected_s2, 'commit_branch_mispredicts_s2_mispred_s2_source_')]:
                native = {k[len(FTQ + prefix):]: v for k, v in counters.items() if k.startswith(FTQ + prefix)}
                assert native and counter == Counter(native), prefix
            summary['s2_source_and_selected_native_margins_match'] = True
            summary['selected_stage_source_reason'] = [dict(stage=k[0], source=k[1], reason=k[2], events=v)
                                                      for k, v in sorted(selected.items())]
            assert summary['passed'], summary['checks']
            retired, = [v for k, v in counters.items() if k.endswith('.rob.commitInstr')]
            phases.append(dict(phase=label, native_print_stamp=stamp, native_retired=retired,
                               mispredict_entries_per_kinst=1000 * summary['rows'] / retired, **summary))
        assert next(cursor, None) is None, 'Unassigned BPU events after native phase cuts'
    return dict(passed=True, point=receipt['point'], source=receipt['source'], phases=phases,
                source_sha256={str(p): digest(p) for p in [receipt_path, Path(__file__)]},
                scope='Closed guest; native event counts define ordinal phases and all six mispredict margins must match. '
                      'STAMP is not calibrated to TEA/native cycles. Stage/position/taken groups describe the selected CFI; '
                      'S3 source metadata alone does not imply S3 was selected. Mispredict-only events do not establish '
                      'per-PC accuracy, resolution/recovery latency, a TEA/DIP join, unique causality or saved cycles.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    data = analyze(args.folder.resolve(), args.receipt.resolve())
    args.output.write_text(json.dumps(data, indent=2) + '\n')
    print(json.dumps(dict(passed=data['passed'], point=data['point'],
                          phase_rows={p['phase']: p['rows'] for p in data['phases']}), indent=2))


if __name__ == '__main__':
    main()
