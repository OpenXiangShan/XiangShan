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


def analyze(folder, receipt, *, pcs=None):
    folder = Path(folder)
    output = details(folder, receipt)
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    assert proof['binding']['writeback_capture'] and proof['binding']['S2_to_WB_one_cycle_asserted']
    assert len(proof['binding']['writeback_bindings']) == 3
    probe = json.loads((folder / 'probe-receipt.json').read_text())
    assert 'load-writeback-probe.csv' in probe['files']
    blocks = read_blocks(folder / 'simulator_err.txt')
    selected_pcs = None if pcs is None else set(pcs)
    rows, boundaries, service_boundaries, prior = [], [], [], {}
    wb_counts = [Counter() for _ in output['phases']]
    s2_count, wb_count, previous_cycle = 0, 0, -1
    columns = ['core_cycle', 'lane', 'cause_mask', *PAYLOAD]
    with (folder / 'load-service-probe.csv').open() as service, (folder / 'load-writeback-probe.csv').open() as joint:
        def service_events():
            for raw in csv.DictReader(service):
                row = {name: int(value) for name, value in raw.items()}
                if row['lane'] == 3:
                    service_boundaries.append((row['core_cycle'], row['clean'], row['native_dump']))
                elif row['fire']:
                    yield row
        events = service_events()
        for raw in csv.DictReader(joint):
            row = {name: int(value) for name, value in raw.items()}
            assert row['core_cycle'] >= previous_cycle, 'Joint event clock moved backwards'
            if row['core_cycle'] > previous_cycle:
                prior = {key: value for key, value in prior.items() if key[0] >= row['core_cycle'] - 1}
            previous_cycle = row['core_cycle']
            if row['kind'] == 0:
                boundaries.append((row['core_cycle'], row['clean'], row['native_dump']))
                continue
            if row['kind'] == 1:
                original = next(events, None)
                assert original is not None and all(row[name] == original[name] for name in columns), 'Joint S2 rows differ from the calibrated trace'
                prior[row['core_cycle'], row['lane']] = row
                s2_count += 1
            else:
                assert row['kind'] == 2
                previous = prior.get((row['core_cycle'] - 1, row['lane']))
                assert previous is not None, 'WB has no preceding same-lane S2'
                assert all(previous[name] == row[name] for name in PAYLOAD), 'WB payload differs from preceding S2'
                wb_count += 1
                for phase, counts in zip(output['phases'], wb_counts):
                    if phase['begin_core_exclusive'] < row['core_cycle'] <= phase['end_core_inclusive']:
                        counts[row['lane']] += 1
            if selected_pcs is None or row['pc'] in selected_pcs:
                rows.append(row)
        assert next(events, None) is None, 'Joint trace is missing calibrated S2 rows'
    assert boundaries == service_boundaries
    attempts, completed = match_attempts(rows)
    for phase, counts in zip(output['phases'], wb_counts):
        begin, end = phase['begin_core_exclusive'], phase['end_core_inclusive']
        marginals = []
        for lane in range(3):
            measured = counts[lane]
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
    output.update(joint_S2_events=s2_count, joint_successful_WB_events=wb_count,
                  completed_scalar_keys=len(completed), scalar_keys_without_successful_WB=sum(unresolved.values()),
                  scalar_unresolved_PC=dict(unresolved), successful_WB_identity='previous_S2_payload_and_carried_issue_member',
                  scope=output['scope'] + ' Joint WB timing/payload reproduces the preceding same-lane S2 and every existing native successful-WB counter. Issue identity is checked for scalar uniqueness. Completed cohorts include pre-clean service and are not retired loads or first-data-arrival times. Unmatched S2 keys have unknown fate; latencies are not additive CPU losses.')
    if selected_pcs is not None:
        output['WB_cohort_PCs'] = [hex(pc) for pc in sorted(selected_pcs)]
        output['scope'] += ' WB identity/cohort and unresolved-key statistics cover only WB_cohort_PCs; S2 profiles, per-event S2/WB payload checks and native marginals still cover the full trace.'
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--pc', type=lambda value: int(value, 0), action='append',
                        help='Select WB identity/cohort PCs; full-trace/native checks remain enabled')
    args = parser.parse_args()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt, pcs=args.pc), indent=2) + '\n')


if __name__ == '__main__':
    main()
