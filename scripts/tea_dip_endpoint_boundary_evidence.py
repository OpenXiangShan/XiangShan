"""Audit clock-matched TEA prefix differences at two actual native PERF dumps."""
from pathlib import Path
import argparse, hashlib, json, sqlite3, sys


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def analyze(root):
    stage = root / 'tea-exact-endpoint-two-dumps'
    receipt_path = stage / 'closure-receipt.json'
    if not receipt_path.exists():
        return dict(pending=True, database_read=False, reason='Original two-dump closure not archived')
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['controller_live'] and receipt['all_original_guests_closed']
    assert receipt['controller']['completed'] == receipt['controller']['total'] == 2
    for name, entry in receipt['files'].items():
        path = stage / name
        assert path.stat().st_size == entry['bytes'] and digest(path) == entry['sha256']
    spec = json.loads((stage / 'experiment.json').read_text())
    assert spec['warmup'] == 50000 and spec['max_instr'] == 100000 and spec['period'] == 1000 and spec['seed'] == 99
    assert spec['builds']['control7efc']['source'] == '7efc7832caaf32fda099e53528cc6badc7054cb6'
    assert spec['builds']['endpoint']['source'] == 'addd8cc4d35a5a3f5f3fa3cfe8371280ff787807'
    sys.path.insert(0, str(root))
    from native_perf import read_blocks, unique_counters, counter_multisets
    blocks = []
    for variant in ['control7efc', 'endpoint']:
        folder = stage / variant
        result = json.loads((folder / 'result.json').read_text())
        process = json.loads((folder / 'process.json').read_text())
        launch = json.loads((folder / 'launch-observation.json').read_text())
        assert process == result['process'] and result['original_process_closed'] and result['exitcode'] == 0 and result['difftest_passed']
        assert launch['live'] and all(launch[k] == v for k, v in process.items())
        assert all(result['sampler_flags'].values())
        assert result['source'] == spec['builds'][variant]['source'] and result['binary_sha256'] == spec['builds'][variant]['binary_sha256']
        command = json.loads((folder / 'command.json').read_text())
        for name, value in [('-I', '100000'), ('-W', '50000'), ('-s', '99'), ('-i', spec['image']), ('--diff', spec['reference']), ('--cst-file', spec['cst']), ('--dump-select-db', spec['selectors'])]:
            assert command[command.index(name) + 1] == value
        block = read_blocks(folder / 'simulator_err.txt')
        assert len(block) == 2
        blocks.append(block)
    frames, extra_names = [], set()
    native_total_retired = 0
    for stamp, block in blocks[1].items():
        unique = unique_counters(block)
        def counter(suffix):
            value, = [v for name, v in unique.items() if name.endswith('.rob.' + suffix)]
            return value
        expected = ['tea_exact_endpoint_cycle', 'tea_exact_endpoint_retired', *['tea_exact_endpoint_state_' + str(i) for i in range(4)]]
        names = {name for name in block if any(name.endswith('.rob.' + suffix) for suffix in expected)}
        assert len(names) == 6
        extra_names.update(names)
        native_total_retired += counter('commitInstr')
        states = {state: counter('tea_exact_endpoint_state_' + str(i)) for i, state in enumerate(['Computing', 'Stalled', 'Flushed', 'Drained'])}
        cycle = counter('tea_exact_endpoint_cycle')
        assert sum(states.values()) == cycle
        frames.append(dict(native_print_stamp=stamp,native_exclusive_stamp=stamp+1,native_interval_cycles=counter('clock_cycle'),native_interval_retired=counter('commitInstr'),native_cumulative_retired=native_total_retired,tea_cycle=cycle,tea_retired=counter('tea_exact_endpoint_retired'),states=states,observed_epoch_offset=stamp+1-cycle,tea_retired_minus_native_cumulative=counter('tea_exact_endpoint_retired')-native_total_retired))
    original = {t: counter_multisets(b) for t, b in blocks[0].items()}
    observed = {t: counter_multisets({name: values for name, values in b.items() if name not in extra_names}) for t, b in blocks[1].items()}
    checks = dict(all_old_native_multivalues_equal=original == observed)
    tables = {}
    for table in spec['selectors'].split():
        values = []
        for variant in ['control7efc', 'endpoint']:
            with sqlite3.connect((stage / variant / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
                values.append(db.execute('SELECT * FROM ' + table + ' ORDER BY ID').fetchall())
        equal = values[0] == values[1]
        checks[table + '_equal'] = equal
        tables[table] = dict(rows=[len(v) for v in values], all_rows_equal=equal)
    before, after = frames
    duration = after['tea_cycle'] - before['tea_cycle']
    states = {name: after['states'][name] - count for name, count in before['states'].items()}
    assert all(v >= 0 for v in states.values()) and sum(states.values()) == duration
    native_duration = after['native_print_stamp'] - before['native_print_stamp']
    clock_aligned = duration == native_duration == after['native_interval_cycles']
    same_epoch_offset = before['observed_epoch_offset'] == after['observed_epoch_offset']
    with sqlite3.connect((stage / 'endpoint/simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        summaries = list(db.execute('SELECT * FROM TeaCycleSummary_0 WHERE CYCLE>=? AND CYCLE<=? ORDER BY CYCLE,ID',(before['tea_cycle'],after['tea_cycle'])))
    first, last = summaries[0], summaries[-1]
    coarse = {state: last['STATES_' + str(i)] - first['STATES_' + str(i)] for i, state in enumerate(states)}
    assert sum(coarse.values()) == last['CYCLE'] - first['CYCLE']
    edges = {state: states[state] - count for state, count in coarse.items()}
    assert all(v >= 0 for v in edges.values()) and sum(edges.values()) == duration - sum(coarse.values())
    retired = after['native_interval_retired']
    budget = {state: count / retired for state, count in states.items()}
    if clock_aligned:
        assert abs(sum(budget.values()) - after['native_interval_cycles'] / retired) < 1e-12
    return dict(pending=False,database_read=True,consumer_sha256=digest(Path(__file__)),closed_receipt_sha256=digest(receipt_path),
                neutrality=dict(passed=all(checks.values()),checks=checks,tables=tables,exact_new_native_names=sorted(extra_names)),
                frames=frames,observed_epoch_offsets_equal=same_epoch_offset,boundary_cycle_differences_aligned=clock_aligned,
                exact_native_roi_cycle_accounting_valid=all(checks.values()) and clock_aligned,
                roi=dict(cycles=duration,native_reported_cycles=after['native_interval_cycles'],native_reported_retired=retired,tea_retired_difference=after['tea_retired']-before['tea_retired'],retirement_counter_difference=after['tea_retired']-before['tea_retired']-retired,states=states,state_budget_per_native_reported_retired=budget,native_reported_cpi=after['native_interval_cycles']/retired,coarse_interior=dict(begin=first['CYCLE'],end=last['CYCLE'],cycles=sum(coarse.values()),states=coarse),formerly_omitted_edges=dict(cycles=sum(edges.values()),states=edges)),
                performance_speedup=None,scope='One original50k+50kCoreMark pair, all old multivalue native and six complete tables. Actual paired dump timestamps validate cycle duration without a fixed offset correction. Exact state budget uses the reported native ROI retirement denominator; TEA architectural prefix difference is separate and is not assumed equal. Only this measured interval is calibrated; no FP/SPEC or dynamic cause/adoption claim.')


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir',required=True,type=Path)
    args=parser.parse_args();root=args.report_dir.resolve();result=analyze(root)
    if not result['pending']:
        out=root/'tea-exact-endpoint-two-dumps/analysis';out.mkdir(exist_ok=True)
        (out/'evidence.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps(result,indent=2))


if __name__=='__main__':
    main()
