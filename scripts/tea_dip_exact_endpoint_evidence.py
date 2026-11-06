"""Audit the original closed exact-endpoint CoreMark against its unchanged 7efc control."""
from pathlib import Path
import argparse, hashlib, json, re, sqlite3, sys


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def analyze(root):
    stage = root / 'tea-exact-endpoint'
    receipt_path = stage / 'closure-receipt.json'
    if not receipt_path.exists():
        return dict(pending=True, database_read=False, reason='Original build and smoke closure not archived')
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not any(receipt[key] for key in ['original_build_controller_live', 'original_smoke_controller_live', 'original_guest_live'])
    source = json.loads((root / 'evidence/tea-exact-endpoint-analysis-tools/source.json').read_text())
    assert receipt['source_head'] == source['head'] == receipt['build']['source'] == 'addd8cc4d35a5a3f5f3fa3cfe8371280ff787807'
    assert receipt['build']['phase'] == receipt['smoke']['phase'] == 'complete'
    for name, entry in receipt['files'].items():
        path = stage / name
        assert path.stat().st_size == entry['bytes'] and digest(path) == entry['sha256']
    old = root / 'evidence/gate-observe-v2'
    old_receipt = json.loads((old / 'build-closure-receipt.json').read_text())
    assert old_receipt['ready'] and not old_receipt['build_live'] and not old_receipt['smoke_live']
    assert old_receipt['build']['source'] == '7efc7832caaf32fda099e53528cc6badc7054cb6'
    for name, entry in json.loads((old / 'build-coremark-manifest.json').read_text()).items():
        path = old / name
        assert path.stat().st_size == entry['bytes'] and digest(path) == entry['sha256']
    folder = stage / 'coremark'
    result = json.loads((folder / 'result.json').read_text())
    process = json.loads((folder / 'process.json').read_text())
    launch = json.loads((folder / 'launch-observation.json').read_text())
    assert result['source'] == source['head'] and result['process'] == process == receipt['guest']
    assert result['original_process_closed'] and launch['live'] and all(launch[key] == value for key, value in process.items())
    assert result['binary_sha256'] == receipt['build']['binary_sha256']
    before, after = [json.loads(path.read_text()) for path in [old / 'coremark/command.json', folder / 'command.json']]
    for flag in ['-I', '-C', '-s', '-i', '--diff', '--dump-select-db']:
        assert before[before.index(flag) + 1] == after[after.index(flag) + 1]
    assert receipt['smoke']['cst_sha256'] == digest(old / 'tea-dip-period1000.cst')
    text = re.sub(r'\x1b\[[0-9;]*m', '', (folder / 'simulator_out.txt').read_text())
    assert (folder / 'exitcode').read_text().strip() == '0' and 'Difftest enabled' in text and 'EXCEEDING CYCLE/INSTR LIMIT' in text
    assert not any(marker in text for marker in ['HIT CRITICAL', 'HIT BAD TRAP', 'ABORT'])
    counts = re.search(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)', text)
    assert [int(v.replace(',', '')) for v in counts.groups()] == [result['instructions'], result['cycles']]
    assert 100000 <= result['instructions'] < 100128
    for name, value in [('enableTea0', 1), ('enableDip0', 1), ('teaSamplePeriod0', 1000), ('dipSamplePeriod0', 1000)]:
        assert re.search(r'^' + name + ' = ' + str(value) + '$', text, re.M)
    sys.path.insert(0, str(root))
    from native_perf import read_blocks, unique_counters, counter_multisets
    blocks = [read_blocks(path / 'simulator_err.txt') for path in [old / 'coremark', folder]]
    assert all(len(block) == 1 for block in blocks)
    stamp, native = next(iter(blocks[1].items()))
    unique = unique_counters(native)
    def counter(suffix):
        value, = [value for name, value in unique.items() if name.endswith('.rob.' + suffix)]
        return value
    names = ['tea_exact_endpoint_cycle', 'tea_exact_endpoint_retired', *['tea_exact_endpoint_state_' + str(i) for i in range(4)]]
    values = {name: counter(name) for name in names}
    extra_names = {name for name in native if any(name.endswith('.rob.' + expected) for expected in names)}
    assert len(extra_names) == 6
    original = {cycle: counter_multisets(block) for cycle, block in blocks[0].items()}
    observed = {cycle: counter_multisets({name: values for name, values in block.items() if name not in extra_names}) for cycle, block in blocks[1].items()}
    checks = dict(all_old_native_multivalues_equal=original == observed)
    tables = {}
    for name in after[after.index('--dump-select-db') + 1].split():
        records = []
        for path in [old / 'coremark/simulator.db', folder / 'simulator.db']:
            with sqlite3.connect(path.resolve().as_uri() + '?mode=ro', uri=True) as db:
                records.append(db.execute('SELECT * FROM ' + name + ' ORDER BY ID').fetchall())
        equal = records[0] == records[1]
        tables[name] = dict(rows=[len(rows) for rows in records],all_rows_equal=equal)
        checks[name + '_equal'] = equal
    with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        last = dict(db.execute('SELECT * FROM TeaCycleSummary_0 ORDER BY CYCLE DESC,ID DESC LIMIT 1').fetchone())
    states = {name: values['tea_exact_endpoint_state_' + str(i)] for i, name in enumerate(['Computing', 'Stalled', 'Flushed', 'Drained'])}
    cycle, retired = values['tea_exact_endpoint_cycle'], values['tea_exact_endpoint_retired']
    assert sum(states.values()) == cycle
    tail = {name: value - last['STATES_' + str(i)] for i, (name, value) in enumerate(states.items())}
    assert all(value >= 0 for value in tail.values()) and sum(tail.values()) == cycle - last['CYCLE']
    assert retired >= last['RETIRED'] and counter('clock_cycle') == stamp + 1
    clock_aligned = cycle == counter('clock_cycle')
    return dict(pending=False, database_read=True, source=source['head'], consumer_sha256=digest(Path(__file__)),
                closed_receipt_sha256=digest(receipt_path), original_control_source=old_receipt['build']['source'],
                neutrality=dict(passed=all(checks.values()), checks=checks, exact_new_native_names=sorted(extra_names), tables=tables),
                endpoint=dict(clock_aligned_to_native=clock_aligned,cycle=cycle,states=states,retired=retired,native_cycles=counter('clock_cycle'),native_retired=counter('commitInstr'),retired_minus_native=retired-counter('commitInstr'),native_clock_delta=cycle-counter('clock_cycle'),emulator_trap_instructions=result['instructions'],emulator_trap_cycles=result['cycles']),
                last_coarse_snapshot=last,tail=dict(cycles=cycle-last['CYCLE'],retired=retired-last['RETIRED'],states=tail),
                endpoint_state_clock_valid=all(checks.values()) and clock_aligned,
                performance_speedup=None,
                scope='One original closed100kCoreMark against original7efc, removing exactly six new names from full multivalue native and comparing all six complete tables. Endpoint counts are cumulative, not warmup-reset ROI. Native/trap/TEA retirement values remain explicit. Only clock-aligned endpoint states may support exact tail accounting. No FP coverage, SPEC gain, general causality or suite adoption claim.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    args = parser.parse_args()
    root = args.report_dir.resolve()
    result = analyze(root)
    if not result['pending']:
        out = root / 'tea-exact-endpoint/analysis'
        out.mkdir(exist_ok=True)
        (out / 'coremark.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps({key: value for key, value in result.items() if key != 'last_coarse_snapshot'}, indent=2))


if __name__ == '__main__':
    main()
