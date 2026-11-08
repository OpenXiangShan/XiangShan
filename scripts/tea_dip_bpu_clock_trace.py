"""Audit shared TEA-cycle BPU events without treating event time as prediction time."""
from bisect import bisect_left, bisect_right
from collections import Counter
from itertools import zip_longest
from pathlib import Path
import argparse
import json
import sqlite3

import tea_dip_bpu_commit_trace as trace
from native_perf import read_blocks, unique_counters
from tea_dip_bpu_native_cohort import cohorts
from tea_dip_query_endpoint_evidence import digest, old_execution, verify


def progress_band(snapshots, cycles, cycle, edges):
    low, high = bisect_right(cycles, cycle)-1, bisect_left(cycles, cycle)
    if low < 0 or high == len(snapshots):
        return ('unbracketed',)
    lower, upper = snapshots[low]['RETIRED'], snapshots[high]['RETIRED']
    a, b = bisect_right(edges, lower), bisect_right(edges, upper)
    return ('certain', a) if a == b else ('ambiguous', *range(a,b+1))


def closed(root):
    receipt_path = root/'closure-receipt.json'
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    verify(root, receipt['files'])
    spec = json.loads((root/'experiment.json').read_text())
    assert digest(root/'experiment.json') == receipt['spec_sha256']
    result = json.loads((root/spec['case']['point']/'result.json').read_text())
    assert result['original_process_closed'] and result['exitcode'] == 0 and result['difftest_passed']
    assert result['source'] == spec['source'] == receipt['source']
    assert result['process'] == receipt['process'] and result['spec_sha256'] == receipt['spec_sha256']
    assert json.loads((root/'resource-audit.json').read_text())['passed']
    return spec, root/spec['case']['point']


def analyze(root, control, prior_root, edges):
    assert all(a < b for a,b in zip([0]+edges, edges))
    spec, folder = closed(root)
    prior_spec, prior = closed(prior_root)
    assert prior_spec['case']['point'] == spec['case']['point']
    assert prior_spec['control_source'] == spec['control_source']
    for flag in ['-I','-C','-s','-i','--diff','--cst-file','--dump-select-db']:
        commands = [json.loads((p/'command.json').read_text()) for p in [prior,folder]]
        assert len({c[c.index(flag)+1] for c in commands}) == 1
    assert ('-W' in commands[0]) == ('-W' in commands[1])
    if '-W' in commands[0]:
        assert commands[0][commands[0].index('-W')+1] == commands[1][commands[1].index('-W')+1]
    control_receipt = json.loads((root/'control-closure-receipt.json').read_text())
    assert control_receipt['ready'] and not control_receipt['original_process_live']
    control_source = control_receipt['build']['source'] if spec['case']['point'] == 'coremark' else control_receipt['source']
    assert control_source == spec['control_source']
    verify(control, control_receipt['files'])
    neutrality = old_execution(control, folder, set())
    prior_neutrality = old_execution(prior, folder, set())
    assert neutrality['passed'] and prior_neutrality['passed']
    blocks = sorted(read_blocks(folder/'simulator_err.txt').items())
    assert len(blocks) == (2 if '-W' in spec['case']['command'] else 1)
    phases = []
    with sqlite3.connect((folder/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True) as db, sqlite3.connect((prior/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True) as old:
        db.row_factory = sqlite3.Row
        assert {r['name'] for r in db.execute('PRAGMA table_info(BpuCommitMispredict)')} == trace.COLUMNS | {'TEACYCLE'}
        assert {r[1] for r in old.execute('PRAGMA table_info(BpuCommitMispredict)')} == trace.COLUMNS
        projection = ','.join(sorted(trace.COLUMNS))
        previous_rows = old.execute('SELECT '+projection+' FROM BpuCommitMispredict ORDER BY ID')
        new_rows = db.execute('SELECT '+projection+' FROM BpuCommitMispredict ORDER BY ID')
        projected_count = 0
        for before, after in zip_longest(previous_rows,new_rows):
            assert before is not None and after is not None and tuple(before) == tuple(after)
            projected_count += 1
        snapshots = list(db.execute('SELECT CYCLE,RETIRED FROM TeaCycleSummary_0 ORDER BY CYCLE,ID'))
        cycles = [r['CYCLE'] for r in snapshots]
        assert len(snapshots) >= 2 and all(a['CYCLE'] < b['CYCLE'] and a['RETIRED'] <= b['RETIRED'] for a,b in zip(snapshots,snapshots[1:]))
        cursor = iter(db.execute('SELECT * FROM BpuCommitMispredict ORDER BY ID'))
        previous_id, previous_cycle, previous_stamp, previous_endpoint = 0, -1, -1, None
        for label,(stamp,block) in zip(['warmup','roi'] if len(blocks)==2 else ['coremark'],blocks):
            counters = unique_counters(block)
            endpoint, = [value for name,value in counters.items() if name.endswith('.rob.tea_exact_endpoint_cycle')]
            expected = cohorts(counters)
            bands, offsets, event_cycles = Counter(), Counter(), []
            after_endpoint, before_previous_endpoint = 0, 0
            def take():
                nonlocal previous_id,previous_cycle,previous_stamp,after_endpoint,before_previous_endpoint
                for _ in range(expected['committed_mispredict_entries']):
                    row = next(cursor)
                    assert row['ID'] == previous_id+1 and row['TEACYCLE'] > previous_cycle and row['STAMP'] > previous_stamp
                    previous_id,previous_cycle,previous_stamp = row['ID'],row['TEACYCLE'],row['STAMP']
                    if not event_cycles:event_cycles.append(row['TEACYCLE'])
                    bands[progress_band(snapshots,cycles,row['TEACYCLE'],edges)] += 1
                    offsets[row['STAMP']-row['TEACYCLE']] += 1
                    after_endpoint += row['TEACYCLE'] > endpoint
                    before_previous_endpoint += previous_endpoint is not None and row['TEACYCLE'] <= previous_endpoint
                    yield row
            summary = trace.summarize(take(),expected)
            if event_cycles:event_cycles.append(previous_cycle)
            assert sum(bands.values()) == sum(offsets.values()) == summary['rows']
            phases.append(dict(phase=label,native_print_stamp=stamp,native_tea_endpoint=endpoint,shared_tea_cycle_range=event_cycles,
                               ordinal_events_after_native_endpoint=after_endpoint,ordinal_events_at_or_before_previous_endpoint=before_previous_endpoint,
                               stamp_minus_shared_tea_cycle=[dict(offset=k,events=v) for k,v in sorted(offsets.items())],
                               retirement_progress=[dict(classification=k[0],possible_band_indices=list(k[1:]),events=v) for k,v in sorted(bands.items())],
                               **summary))
            previous_endpoint = endpoint
        assert next(cursor,None) is None
    assert all(p['passed'] for p in phases) and sum(p['rows'] for p in phases) == projected_count
    sources = [root/'closure-receipt.json',prior_root/'closure-receipt.json',root/'resource-audit.json',Path(__file__),Path(trace.__file__)]
    return dict(passed=True,point=spec['case']['point'],source=spec['source'],prior_observer_source=prior_spec['source'],
                native_six_table_neutrality=neutrality,prior_observer_native_six_table_neutrality=prior_neutrality,all_prior_BPU_columns_equal=True,prior_BPU_projected_rows=projected_count,
                retirement_edges=edges,phases=phases,source_sha256={str(p):digest(p) for p in sources},
                scope='New TEACYCLE is the existing ROB TeaSelector cycle forwarded combinationally to FTQ. '
                      'It timestamps the committed-mispredict observer, not original prediction, resolution, recovery or exact branch retirement. '
                      'Original BPU columns, native counters and six tables must remain identical. '
                      'Ordinal native phases and shared-cycle endpoint discrepancies are retained separately. '
                      'Retirement progress uses real bracketing TeaCycleSummary snapshots only: crossing bands stay ambiguous and outer edges unbracketed. '
                      'No interpolation, inferred old-run offset, accuracy denominator, unique causal attribution or saved-cycle claim.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--run-dir',type=Path,required=True)
    parser.add_argument('--control-dir',type=Path,required=True)
    parser.add_argument('--prior-observer-dir',type=Path,required=True)
    parser.add_argument('--retired-edges',type=int,nargs='+',required=True)
    parser.add_argument('--output',type=Path,required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    data = analyze(args.run_dir.resolve(),args.control_dir.resolve(),args.prior_observer_dir.resolve(),args.retired_edges)
    args.output.write_text(json.dumps(data,indent=2)+'\n')
    print(json.dumps(dict(passed=data['passed'],point=data['point'],prior_BPU_projected_rows=data['prior_BPU_projected_rows'],
                         phases=[{k:p[k] for k in ['phase','shared_tea_cycle_range','retirement_progress']} for p in data['phases']]),indent=2))


if __name__ == '__main__':
    main()
