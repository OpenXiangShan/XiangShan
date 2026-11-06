"""Audit closed FP recovery tests with native retirement and explicit sampling limits."""
from pathlib import Path
import argparse
import hashlib
import json
import re
import sqlite3
import subprocess
import sys


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def analyze(root):
    stage = root / 'wb-query-recovery-micro'
    receipt_path = stage / 'closure-receipt.json'
    receipt = json.loads(receipt_path.read_text())
    assert not receipt['controller_live'] and receipt['all_original_guests_closed']
    assert receipt['controller']['phase'] == 'complete' and receipt['controller']['completed'] == 4
    for name, entry in receipt['files'].items():
        path = stage / name
        assert path.stat().st_size == entry['bytes'] and digest(path) == entry['sha256']
    spec = json.loads((stage / 'experiment.json').read_text())
    build = json.loads((root / 'evidence/fdiv-wb-query/build-state.json').read_text())
    assert spec['binaries']['candidate']['source'] == build['source'] == 'b0a22b93a867c5dff56b1b3fb32708c9e6363b01'
    assert spec['binaries']['candidate']['sha256'] == build['binary_sha256']
    assert spec['binaries']['control841']['source'] == '84112d0b65231ca690a4ef2fea4b5169f2146e10'
    assert spec['cst_sha256'] == digest(stage / 'tea-dip-period1000.cst')
    assert spec['period'] == 1000 and spec['seed'] == 99
    assert spec['selectors'].split() == ['Tea_0', 'TeaDelayedFinal_0', 'TeaCycleSummary_0', 'TeaNoCommit_0', 'Dip_0', 'DipDelayedFinal_0']
    results = json.loads((stage / 'results.json').read_text())
    assert len(results) == 4 and {(r['variant'], r['case']) for r in results} == {(v, c) for v in ['control841', 'candidate'] for c in ['consecutive_variable_mixed', 'variable_branch_recovery']}
    sys.path.insert(0, str(root))
    from analyze_windows import DIP, TEA
    from native_perf import read_blocks, unique_counters
    rows = []
    for result in results:
        variant, name = result['variant'], result['case']
        folder = stage / 'runs' / variant / name
        case = next(c for c in spec['cases'] if c['case'] == name)
        image = stage / 'images' / name
        assert digest(image / 'program.bin') == case['image_sha256'] == result['image_sha256']
        assert digest(image / 'program.S') == case['source_sha256']
        symbols = subprocess.check_output(['/usr/bin/riscv64-linux-gnu-objdump', '-t', str(image / 'program.elf')], text=True)
        pass_pc = int(next(line.split()[0] for line in symbols.splitlines() if line.split() and line.split()[-1] == '_pass'), 16)
        assert pass_pc == int(case['pass_pc'], 16)
        process = json.loads((folder / 'process.json').read_text())
        launch = json.loads((folder / 'launch-observation.json').read_text())
        assert process == result['process'] and result['original_process_closed'] and launch['live']
        assert all(launch[key] == value for key, value in process.items())
        assert result['passed'] and not result['difftest_mismatches'] and not result['cycle_or_instruction_limit']
        command = json.loads((folder / 'command.json').read_text())
        identity = spec['binaries'][variant]
        assert identity['binary'] in command and identity['binary'] in launch['args']
        assert result['binary_sha256'] == identity['sha256']
        for flag, value in [('-s', '99'), ('-I', '100000'), ('-C', '200000'), ('-i', case['image']), ('--diff', spec['reference']), ('--dump-select-db', spec['selectors'])]:
            assert command[command.index(flag) + 1] == value
        text = re.sub(r'\x1b\[[0-9;]*m', '', (folder / 'simulator_out.txt').read_text())
        trap = re.search(r'HIT GOOD TRAP at pc = (0x[0-9a-fA-F]+)', text)
        assert (folder / 'exitcode').read_text().strip() == '0' and trap is not None and int(trap[1], 16) == pass_pc
        assert 'Difftest enabled' in text and not any(s in text for s in ['ABORT', 'HIT CRITICAL', 'HIT BAD TRAP', 'pma check failed', 'EXCEEDING CYCLE/INSTR LIMIT'])
        counts = re.search(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)', text)
        assert [int(v.replace(',', '')) for v in counts.groups()] == [result['instructions'], result['cycles']]
        assert 2500 < result['instructions'] < 8192
        for flag, value in [('enableTea0', 1), ('enableDip0', 1), ('teaSamplePeriod0', 1000), ('dipSamplePeriod0', 1000)]:
            assert re.search(r'^' + flag + ' = ' + str(value) + '$', text, re.M)
        blocks = read_blocks(folder / 'simulator_err.txt')
        assert len(blocks) == 1
        stamp, block = next(iter(blocks.items()))
        counters = unique_counters(block)
        def rob(key):
            value, = [value for key_name, value in counters.items() if key_name.endswith('.rob.' + key)]
            return value
        native_cycles, native_retired = rob('clock_cycle'), rob('commitInstr')
        assert native_cycles == stamp + 1
        branch = [v for key, v in counters.items() if key.endswith('.commit_branch_mispredicts')]
        assert branch
        selected = {key: rob(key) for key in ['waitfDivSqrtCycle', 'waitfmulCycle', 'waitfaluCycle', 'waitLoadCycle', 'waitStoreCycle']}
        database = folder / 'simulator.db'
        with sqlite3.connect(database.resolve().as_uri() + '?mode=ro', uri=True) as db:
            db.row_factory = sqlite3.Row
            tea = list(db.execute('SELECT * FROM Tea_0 ORDER BY CYCLE,ID'))
            dip = list(db.execute('SELECT * FROM Dip_0 ORDER BY CYCLE,ID'))
            finals = list(db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'))
            snapshots = list(db.execute('SELECT * FROM TeaCycleSummary_0 ORDER BY CYCLE,ID'))
            width = sum(r['name'].startswith('SLOTS_') and r['name'].endswith('_STATE') for r in db.execute('pragma table_info(Dip_0)'))
            pc_profile, stats = DIP.analyze(dip, finals, width)
            tables = {name: db.execute('SELECT count(*) FROM ' + name).fetchone()[0] for name in spec['selectors'].split()}
        assert tables == result['trace_table_rows'] and all(tables.values())
        assert [r['CYCLE'] for r in tea] == [r['CYCLE'] for r in dip] == list(range(0, result['cycles'] + 1, spec['period']))
        assert stats['sampled_cycles'] == len(dip) and stats['resolved_slots'] + stats['unresolved_slots'] == stats['sampled_slots']
        assert sum(pc_profile.values()) == stats['resolved_slots']
        assert sum(stats['state_cycles'].values()) * width == stats['resolved_slots']
        assert len(snapshots) >= 2
        first, last = snapshots[0], snapshots[-1]
        states = {name: last['STATES_' + str(i)] - first['STATES_' + str(i)] for i, name in enumerate(TEA.STATES)}
        span = last['CYCLE'] - first['CYCLE']
        assert sum(states.values()) == span and span > 0
        rows.append(dict(variant=variant, case=name, process=process, passed=True,
                         emulator_trap_instructions=result['instructions'], emulator_trap_cycles=result['cycles'],
                         native_dump_cycles=native_cycles, native_dump_retired=native_retired,
                         native_minus_trap_instructions=native_retired - result['instructions'],
                         native_minus_trap_cycles=native_cycles - result['cycles'],
                         native_branch_mispredicts=sum(branch), native_head_wait_cycles=selected,
                         six_tables=tables, dip_budget={key: stats[key] for key in ['sampled_cycles', 'sampled_slots', 'resolved_slots', 'unresolved_slots']},
                         dip_state_equivalent_cycles={**{key: value * spec['period'] for key, value in stats['state_cycles'].items()},
                                                      'Unresolved': stats['unresolved_slots'] / width * spec['period']},
                         dip_pc_mask_slots=[dict(state=DIP.STATES[state], mask=mask, pc=hex(pc), labels=DIP.cause_name(state, mask), slots=value)
                                            for (state, mask, pc), value in sorted(pc_profile.items())],
                         exact_prefix=dict(begin=first['CYCLE'], end=last['CYCLE'], cycles=span,
                                           retired=last['RETIRED'] - first['RETIRED'], states=states,
                                           omitted_native_cycles=native_cycles - span,
                                           retired_fraction_at_last_snapshot=last['RETIRED'] / native_retired),
                         performance_cpi=None, performance_speedup=None))
    pairs = []
    for case in spec['cases']:
        before, after = [next(r for r in rows if r['case'] == case['case'] and r['variant'] == v) for v in ['control841', 'candidate']]
        assert before['native_dump_retired'] == after['native_dump_retired']
        if case['case'] == 'variable_branch_recovery':
            assert min(before['native_branch_mispredicts'], after['native_branch_mispredicts']) > 0
        total = after['native_dump_cycles'] - before['native_dump_cycles']
        equivalent = {state: after['dip_state_equivalent_cycles'][state] - value for state, value in before['dip_state_equivalent_cycles'].items()}
        assert abs(sum(equivalent.values()) - (after['dip_budget']['sampled_cycles'] - before['dip_budget']['sampled_cycles']) * spec['period']) < 1e-9
        pairs.append(dict(case=case['case'], native_retirement_equal=True, native_retired=before['native_dump_retired'],
                          native_cycle_delta=total, emulator_trap_cycle_delta=after['emulator_trap_cycles'] - before['emulator_trap_cycles'],
                          native_head_wait_delta={key: after['native_head_wait_cycles'][key] - value for key, value in before['native_head_wait_cycles'].items()},
                          branch_mispredict_delta=after['native_branch_mispredicts'] - before['native_branch_mispredicts'],
                          exact_common_prefix_equal=all(before['exact_prefix'][key] == after['exact_prefix'][key] for key in ['begin', 'end', 'cycles', 'retired', 'states']),
                          dip_sample_equivalent_cycle_delta=equivalent, delta_smaller_than_sampling_period=abs(total) < spec['period'],
                          performance_cpi=None, performance_speedup=None))
    dependencies = [root / 'native_perf.py', root / 'analyze_windows.py', root.parent.parent / 'ci-41413249290/source/scripts/dip_analyze.py']
    return dict(passed=True, attempts=len(rows), rows=rows, pairs=pairs, candidate_source=build['source'],
                consumer_sha256=digest(Path(__file__)), closed_receipt_sha256=digest(receipt_path),
                runtime_dependency_sha256={str(p.relative_to(root.parent.parent)): digest(p) for p in dependencies},
                scope='Four closed focused tests including boot and terminal trap, not a SPEC ROI or suite score. Native retirement and trap-event endpoints are distinct; no silent denominator replacement. Exact prefix is clock matched and omits the terminal tail. DIP state/PC/mask including unresolved conserves sampled-equivalent cycles, not exact total-cycle changes, critical producer latency or unique recovery cause. Native head waits overlap and must not be summed.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    args = parser.parse_args()
    root = args.report_dir.resolve()
    result = analyze(root)
    out = root / 'wb-query-recovery-micro/analysis'
    out.mkdir(exist_ok=True)
    (out / 'evidence.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps({key: result[key] for key in ['passed', 'attempts', 'pairs']}, indent=2))


if __name__ == '__main__':
    main()
