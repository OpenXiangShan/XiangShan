"""Join real S2 attempts to successful load-stage WB using the carried issue identity.

Completed WB cohorts are not retired instructions or exclusive CPU loss. S2
keys without a successful WB retain an unknown fate, including cancellations.
"""
import argparse
from collections import Counter, defaultdict
import csv
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_load_cause_details import details

KEY = ['issue_cycle', 'rob_flag', 'rob_index', 'rob_former', 'rank', 'uop_index']
PAYLOAD = ['rob_flag', 'rob_index', 'rob_former', 'rank', 'uop_index', 'issue_cycle',
           'pc', 'virtual_address', 'physical_address', 'entrance', 'instr_type']


def hist_stats(hist):
    count = sum(hist.values())
    total = sum(value * n for value, n in hist.items())
    percentiles = {}
    for percent in [50, 90, 99]:
        threshold = (count * percent + 99) // 100
        cumulative = 0
        for value, n in sorted(hist.items()):
            cumulative += n
            if cumulative >= threshold:
                percentiles['p' + str(percent)] = value
                break
    return dict(count=count, sum=total, mean=total / count,
                histogram={str(value): n for value, n in sorted(hist.items())}, **percentiles)


def match_attempts(rows):
    attempts, completed, prior = defaultdict(list), {}, {}
    for row in rows:
        kind = row['kind']
        if kind == 0:
            continue
        key = tuple(row[name] for name in KEY)
        if kind == 1:
            prior[row['core_cycle'], row['lane']] = row
            if row['instr_type'] == 1:
                assert 0 < row['issue_cycle'] <= row['core_cycle'], 'Invalid initial issue clock'
                attempts[key].append(row)
        else:
            assert kind == 2
            previous = prior.get((row['core_cycle'] - 1, row['lane']))
            assert previous is not None, 'WB has no preceding same-lane S2'
            assert all(previous[name] == row[name] for name in PAYLOAD), 'WB payload differs from preceding S2'
            if row['instr_type'] != 1:
                continue
            assert key in attempts and key not in completed, 'Missing or duplicate scalar WB identity'
            assert all(a['pc'] == row['pc'] for a in attempts[key]), 'Mixed PC in scalar issue identity'
            completed[key] = row
    assert all(key not in completed or group[-1]['core_cycle'] < completed[key]['core_cycle']
               for key, group in attempts.items()), 'S2 attempt after successful scalar WB'
    return attempts, completed


def analyze(folder, receipt):
    folder = Path(folder)
    output = details(folder, receipt)
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    assert proof['binding']['writeback_capture'] and proof['binding']['S2_to_WB_one_cycle_asserted']
    assert len(proof['binding']['writeback_bindings']) == 3
    probe = json.loads((folder / 'probe-receipt.json').read_text())
    assert 'load-writeback-probe.csv' in probe['files']
    with (folder / 'load-writeback-probe.csv').open() as stream:
        rows = [{name: int(value) for name, value in row.items()} for row in csv.DictReader(stream)]
    with (folder / 'load-service-probe.csv').open() as stream:
        service = [{name: int(value) for name, value in row.items()} for row in csv.DictReader(stream)]
    old = [row for row in service if row['lane'] < 3 and row['fire']]
    joined_s2 = [row for row in rows if row['kind'] == 1]
    columns = ['core_cycle', 'lane', 'cause_mask', *PAYLOAD]
    assert [{name: row[name] for name in columns} for row in joined_s2] == [
        {name: row[name] for name in columns} for row in old], 'Joint S2 rows differ from the calibrated trace'
    assert [(r['core_cycle'], r['clean'], r['native_dump']) for r in rows if r['kind'] == 0] == [
        (r['core_cycle'], r['clean'], r['native_dump']) for r in service if r['lane'] == 3]
    attempts, completed = match_attempts(rows)
    blocks = read_blocks(folder / 'simulator_err.txt')
    for phase in output['phases']:
        begin, end = phase['begin_core_exclusive'], phase['end_core_inclusive']
        selected = [row for row in rows if row['kind'] == 2 and begin < row['core_cycle'] <= end]
        marginals = []
        for lane in range(3):
            measured = sum(row['lane'] == lane for row in selected)
            values, = [values for name, values in blocks[phase['native_dump']].items()
                       if name.endswith(f'.LoadUnit_{lane}.s3.load_successful_writeback')]
            expected, = values
            assert measured == expected, (lane, measured, expected)
            marginals.append(dict(lane=lane, successful_load_WB=measured))
        groups = defaultdict(lambda: dict(first=Counter(), issue=Counter(), crossed_clean=0))
        for key, wb in completed.items():
            if not begin < wb['core_cycle'] <= end:
                continue
            group = attempts[key]
            first = group[0]
            cohorts = ['all_scalar']
            if any(row['cause_mask'] & 64 and not row['cause_mask'] & 63 for row in group):
                cohorts.append('had_priority_S2_DM')
            for cohort in cohorts:
                stats = groups[hex(wb['pc']), cohort]
                stats['first'][wb['core_cycle'] - first['core_cycle']] += 1
                stats['issue'][wb['core_cycle'] - wb['issue_cycle']] += 1
                stats['crossed_clean'] += first['core_cycle'] <= begin
        profiles = defaultdict(dict)
        for (pc, cohort), stats in groups.items():
            profiles[pc][cohort] = dict(first_S2_to_successful_WB=hist_stats(stats['first']),
                                        initial_issue_to_successful_WB=hist_stats(stats['issue']),
                                        first_S2_at_or_before_clean=stats['crossed_clean'])
        phase['successful_WB_native_marginals'] = marginals
        phase['completed_scalar_PC_cohorts'] = dict(profiles)
    unresolved = Counter(hex(group[0]['pc']) for key, group in attempts.items() if key not in completed)
    output.update(joint_S2_events=len(joined_s2), joint_successful_WB_events=sum(row['kind'] == 2 for row in rows),
                  completed_scalar_keys=len(completed), scalar_keys_without_successful_WB=sum(unresolved.values()),
                  scalar_unresolved_PC=dict(unresolved), successful_WB_identity='previous_S2_payload_and_carried_issue_member',
                  scope=output['scope'] + ' Joint WB timing/payload reproduces the preceding same-lane S2 and every existing native successful-WB counter. Issue identity is checked for scalar uniqueness. Completed cohorts include pre-clean service and are not retired loads or first-data-arrival times. Unmatched S2 keys have unknown fate; latencies are not additive CPU losses.')
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
