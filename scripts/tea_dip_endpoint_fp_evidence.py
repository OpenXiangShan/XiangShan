"""Check exact endpoint FP coverage against same-hardware full native and six-table controls."""
from pathlib import Path
import argparse, hashlib, json, sqlite3, sys


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def analyze(root):
    stage = root / 'tea-exact-endpoint-fp-micro'
    receipt_path = stage / 'closure-receipt.json'
    if not receipt_path.exists():
        return dict(pending=True, database_read=False, reason='Original FP closure not archived')
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['controller_live'] and receipt['all_original_guests_closed']
    assert receipt['controller']['completed'] == receipt['controller']['total'] == 4
    for name, entry in receipt['files'].items():
        path = stage / name
        assert path.stat().st_size == entry['bytes'] and digest(path) == entry['sha256']
    spec = json.loads((stage / 'experiment.json').read_text())
    assert spec['seed'] == 99 and spec['period'] == 1000
    assert [build['source'] for build in spec['builds'].values()] == ['7efc7832caaf32fda099e53528cc6badc7054cb6', 'addd8cc4d35a5a3f5f3fa3cfe8371280ff787807']
    assert spec['selectors'].split() == ['Tea_0','TeaDelayedFinal_0','TeaCycleSummary_0','TeaNoCommit_0','Dip_0','DipDelayedFinal_0']
    sys.path.insert(0, str(root))
    from native_perf import read_blocks, unique_counters, counter_multisets
    pairs = []
    for case in spec['cases']:
        blocks = []
        results = []
        for variant in ['control7efc','endpoint']:
            folder = stage / 'runs' / variant / case['case']
            result = json.loads((folder/'result.json').read_text())
            process = json.loads((folder/'process.json').read_text())
            launch = json.loads((folder/'launch-observation.json').read_text())
            assert process == result['process'] and result['original_process_closed'] and result['exitcode'] == 0 and result['difftest_passed'] and all(result['sampler_flags'].values())
            assert launch['live'] and all(launch[k] == v for k, v in process.items())
            assert result['source'] == spec['builds'][variant]['source'] and result['binary_sha256'] == spec['builds'][variant]['binary_sha256'] and result['image_sha256'] == case['image_sha256']
            command = json.loads((folder/'command.json').read_text())
            for flag, value in [('-I','100000'),('-C','200000'),('-s','99'),('-i',case['image']),('--diff',spec['reference']),('--cst-file',spec['cst']),('--dump-select-db',spec['selectors'])]:
                assert command[command.index(flag)+1] == value
            block = read_blocks(folder/'simulator_err.txt')
            assert len(block) == 1
            blocks.append(block)
            results.append(result)
        stamp, block = next(iter(blocks[1].items()))
        counters = unique_counters(block)
        def counter(suffix):
            value, = [v for name, v in counters.items() if name.endswith('.rob.'+suffix)]
            return value
        suffixes = ['tea_exact_endpoint_cycle','tea_exact_endpoint_retired',*['tea_exact_endpoint_state_'+str(i) for i in range(4)]]
        names = {name for name in block if any(name.endswith('.rob.'+suffix) for suffix in suffixes)}
        assert len(names) == 6
        original = {t:counter_multisets(b) for t,b in blocks[0].items()}
        observed = {t:counter_multisets({name:values for name,values in b.items() if name not in names}) for t,b in blocks[1].items()}
        checks = dict(all_old_native_multivalues_equal=original == observed)
        tables = {}
        for table in spec['selectors'].split():
            values = []
            for variant in ['control7efc','endpoint']:
                with sqlite3.connect((stage/'runs'/variant/case['case']/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True) as db:
                    values.append(db.execute('SELECT * FROM '+table+' ORDER BY ID').fetchall())
            equal = values[0] == values[1]
            checks[table+'_equal'] = equal
            tables[table] = dict(rows=[len(v) for v in values],all_rows_equal=equal)
        with sqlite3.connect((stage/'runs/endpoint'/case['case']/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True) as db:
            db.row_factory = sqlite3.Row
            last = dict(db.execute('SELECT * FROM TeaCycleSummary_0 ORDER BY CYCLE DESC,ID DESC LIMIT 1').fetchone())
        cycle, retired = counter('tea_exact_endpoint_cycle'), counter('tea_exact_endpoint_retired')
        states = {state:counter('tea_exact_endpoint_state_'+str(i)) for i,state in enumerate(['Computing','Stalled','Flushed','Drained'])}
        tail = {state:count-last['STATES_'+str(i)] for i,(state,count) in enumerate(states.items())}
        assert sum(states.values()) == cycle and all(v >= 0 for v in tail.values()) and sum(tail.values()) == cycle-last['CYCLE'] and retired >= last['RETIRED']
        pairs.append(dict(case=case['case'],neutrality=dict(passed=all(checks.values()),checks=checks,tables=tables,exact_new_native_names=sorted(names)),native=dict(print_stamp=stamp,cycles=counter('clock_cycle'),retired=counter('commitInstr')),trap_counts=[row['trap_counts'] for row in results],tea_endpoint=dict(cycle=cycle,retired=retired,states=states,cycle_minus_native=cycle-counter('clock_cycle'),retired_minus_native=retired-counter('commitInstr')),last_coarse_snapshot=dict(cycle=last['CYCLE'],retired=last['RETIRED']),newly_visible_tea_tail=dict(cycles=cycle-last['CYCLE'],retired=retired-last['RETIRED'],states=tail),native_roi_exact_state_accounting_valid=None,performance_speedup=None))
    return dict(pending=False,database_read=True,consumer_sha256=digest(Path(__file__)),closed_receipt_sha256=digest(receipt_path),attempts=4,pairs=pairs,passed=all(pair['neutrality']['passed'] for pair in pairs),scope='Two original same-hardware FP programs; four original guests closed before offline DB access. Removing exactly six references, compare every old native multivalue and all six complete tables. Newly visible tail is exact in TEA own clock/retirement domain; single native dump cannot calibrate paired native ROI boundaries. No SPEC improvement, unique cause, universal correction, or resource increase claimed.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir',required=True,type=Path)
    args = parser.parse_args()
    root = args.report_dir.resolve()
    result = analyze(root)
    if not result['pending']:
        output = root/'tea-exact-endpoint-fp-micro/analysis'
        output.mkdir(exist_ok=True)
        (output/'evidence.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps(result,indent=2))


if __name__ == '__main__':
    main()
