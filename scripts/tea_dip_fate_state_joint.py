"""Join source-cycle TEA groups with final DIP slot fates using the unchanged consumers."""
from collections import Counter
from pathlib import Path
from types import SimpleNamespace
import argparse
import json
import re
import sqlite3

import dip_analyze as dip
import tea_visualize as tea
from native_perf import read_blocks, unique_counters
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_wb_protocol_micro import write

GROUPS = ('Computing', 'Stalled_memory', 'Stalled_other_recorded', 'Stalled_incomplete', 'Flushed', 'Drained')


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    verify(folder, receipt['files'])
    stdout = (folder / 'simulator_out.txt').read_text()
    assert 'Difftest enabled' in stdout
    for name in ['enableTea0', 'enableDip0']:
        assert re.search(r'^' + name + ' = 1$', stdout, re.M)
    periods = [int(re.search(r'^' + name + r' = (\d+)$', stdout, re.M)[1])
               for name in ['teaSamplePeriod0', 'dipSamplePeriod0']]
    assert periods[0] == periods[1]
    blocks = read_blocks(folder / 'simulator_err.txt')
    assert len(blocks) == 2
    counters = unique_counters(blocks[max(blocks)])

    def rob(name):
        value, = [v for k, v in counters.items() if k.endswith('.rob.' + name)]
        return value

    begin, end = min(blocks) + 1, max(blocks) + 1
    cycles, retired = rob('clock_cycle'), rob('commitInstr')
    assert cycles == end - begin and retired > 0
    cpi = cycles / retired
    groups = {}

    def sample(cycle, state, lanes, weight):
        assert cycle not in groups and abs(weight * len(lanes) - 1) < 1e-12
        if state == 'Stalled':
            assert len(lanes) == 1
            psv = lanes[0][1]
            state = ('Stalled_incomplete' if psv < 0 else 'Stalled_memory' if psv & 192
                     else 'Stalled_other_recorded')
        groups[cycle] = state

    database = folder / 'simulator.db'
    coverage = tea.sampling_coverage(database, (begin, end), periods[0])
    assert not any(coverage[k] for k in ['duplicate_sample_rows', 'off_grid_sample_rows', 'missing_interior_samples'])
    run = tea.load_data(database, folder.name,
                        SimpleNamespace(cycle_range=(begin, end), spec_checkpoint=False, bins=1), sample_callback=sample)
    with sqlite3.connect(database.resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        width = sum(r['name'].startswith('SLOTS_') and r['name'].endswith('_STATE')
                    for r in db.execute('pragma table_info(Dip_0)'))
        samples = list(db.execute('SELECT * FROM Dip_0 WHERE CYCLE>=? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end)))
        assert len(samples) == len(groups) == run['summary']['rows'] and {r['CYCLE'] for r in samples} == groups.keys()

        def resolve(rows):
            finals = db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID')
            return dip.analyze(rows, finals, width)

        profile, full = resolve(samples)
        summed, unresolved, reclassified, joint = Counter(), 0, 0, []
        for group in GROUPS:
            selected = [row for row in samples if groups[row['CYCLE']] == group]
            if not selected:
                continue
            partition, summary = resolve(selected)
            summed.update(partition)
            unresolved += summary['unresolved_slots']
            reclassified += summary['reclassified_slots']
            slots = {state: sum(count for (kind, _, _), count in partition.items()
                                if dip.STATES[kind] == state) for state in dip.STATES}
            slots['Unresolved'] = summary['unresolved_slots']
            assert sum(slots.values()) == len(selected) * width
            joint.append(dict(TEA_source_group=group, sampled_cycles=len(selected), DIP_final_slots=slots,
                              joint_cost_per_native_retired={k: v / full['sampled_slots'] * cpi for k, v in slots.items()}))
    assert summed == profile and unresolved == full['unresolved_slots'] and reclassified == full['reclassified_slots']
    assert sum(row['sampled_cycles'] for row in joint) == full['sampled_cycles']
    assert abs(sum(sum(row['joint_cost_per_native_retired'].values()) for row in joint) - cpi) < 1e-9
    return dict(point=folder.name, source=receipt['source'], native_CPI=cpi, native_cycles=cycles,
                native_retired=retired, sample_period=periods[0], dispatch_width=width,
                sampled_cycles=full['sampled_cycles'], joint=joint,
                final_DIP_cost={state: full['state_cycles'][state] / full['sampled_cycles'] * cpi for state in dip.STATES},
                unresolved_DIP_slots=unresolved, partition_profiles_exactly_equal=True,
                partition_reclassified_slots=reclassified, coverage=coverage,
                receipt_sha256=digest(receipt_path), tool_sha256=digest(Path(__file__)),
                consumer_sha256={name: digest(Path(__file__).parent / name)
                                 for name in ['dip_analyze.py', 'tea_visualize.py']},
                scope='Same sampled source cycles, TEA resolved PSV and final DIP fate. All later final records retained; group profiles exactly sum to the full profile. Joint sampled slot cost is rescaled to native CPI, not an exact ROI event budget, critical path, saved cycles or proof of a branch/cache causal interaction.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', required=True, type=Path)
    parser.add_argument('--receipt', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    write(args.output, analyze(args.folder.resolve(), args.receipt.resolve()))


if __name__ == '__main__':
    main()
