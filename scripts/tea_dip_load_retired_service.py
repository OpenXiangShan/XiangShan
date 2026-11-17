"""Partition sampled load targets and completed S2/WB services by retired phase.

WB completion time selects its phase; sampled targets use their source cycle.
These are different populations, and neither is an exclusive CPU loss budget.
"""
import argparse
from bisect import bisect_right
from collections import Counter, defaultdict
import csv
import json
from pathlib import Path
from types import SimpleNamespace

import tea_visualize as tea
from tea_dip_load_writeback_trace import KEY, hist_stats, match_attempts
from tea_dip_load_service_trace import CAUSES
from tea_dip_query_endpoint_evidence import digest
from tea_dip_retired_phase_profile import analyze as retired_profile
from tea_dip_tea_load_identity import analyze as load_identity, instructions


def partition(attempts, completed, identity, profile, samples):
    phases = profile['phases']
    boundaries = [p['begin']['cycle'] for p in phases] + [phases[-1]['end']['cycle']]
    assert all(a < b for a, b in zip(boundaries, boundaries[1:]))
    assert all(a['end'] == b['begin'] for a, b in zip(phases, phases[1:]))
    assert boundaries[0] == identity['roi_begin'] and boundaries[-1] == identity['roi_end']

    def phase_at(cycle):
        return bisect_right(boundaries, cycle) - 1

    wb_groups = [defaultdict(lambda: defaultdict(Counter)) for _ in phases]
    s2_groups = [defaultdict(Counter) for _ in phases]
    target_groups = [defaultdict(Counter) for _ in phases]
    unique_matches = [defaultdict(set) for _ in phases]
    prior_causes = [defaultdict(Counter) for _ in phases]
    unresolved = [Counter() for _ in phases]
    outside = Counter()
    for key, group in attempts.items():
        for row in group:
            index = phase_at(row['core_cycle'])
            if 0 <= index < len(phases):
                s2_groups[index][hex(row['pc'])][str(row['cause_mask'])] += 1
            else:
                outside['S2_attempts'] += 1
        wb = completed.get(key)
        if wb is None:
            index = phase_at(group[-1]['core_cycle'])
            if 0 <= index < len(phases):
                unresolved[index][hex(group[-1]['pc'])] += 1
            else:
                outside['scalar_keys_without_successful_WB'] += 1
            continue
        index = phase_at(wb['core_cycle'])
        if not 0 <= index < len(phases):
            outside['completed_scalar_keys'] += 1
            continue
        stats = wb_groups[index][hex(wb['pc'])]
        stats['first_S2_to_WB'][wb['core_cycle'] - group[0]['core_cycle']] += 1
        stats['initial_issue_to_WB'][wb['core_cycle'] - wb['issue_cycle']] += 1
        stats['S2_attempts_per_completed_key'][len(group)] += 1
        union = 0
        for row in group:
            union |= row['cause_mask']
        stats['service_cause_union'][str(union)] += 1
        stats['boundary']['first_S2_before_phase'] += group[0]['core_cycle'] < boundaries[index]
    records = [(r, 'matched') for r in identity['matches']]
    records += [(r, r['reason']) for r in identity['unknown']]
    assert len(records) == identity['Stalled_samples'] == len(samples)
    for record, reason in records:
        index = phase_at(record['sample_cycle'])
        assert 0 <= index < len(phases)
        pc, signature = samples[record['sample_cycle']]
        group = hex(pc) if pc is not None else 'unknown', str(signature), reason
        stats = target_groups[index][group]
        stats['samples'] += 1
        if reason == 'matched':
            key = tuple(record['identity'][name] for name in KEY)
            assert key in completed and completed[key]['core_cycle'] == record['WB']
            assert pc == record['architectural_PC']
            unique_matches[index][group].add(key)
            stats['unfinished_at_sample'] += record['unfinished_at_sample']
            stats['initial_issue_after_sample'] += record['initial_issue_after_sample']
            union = 0
            for row in attempts[key]:
                if row['core_cycle'] <= record['sample_cycle']:
                    union |= row['cause_mask']
            prior_causes[index][group][str(union)] += 1
    output = []
    for index, phase in enumerate(phases):
        cohorts = {}
        for pc, stats in wb_groups[index].items():
            cohorts[pc] = {name: hist_stats(stats[name]) for name in
                          ['first_S2_to_WB', 'initial_issue_to_WB', 'S2_attempts_per_completed_key']}
            cohorts[pc].update(service_cause_union=dict(stats['service_cause_union']),
                               first_S2_before_phase=stats['boundary']['first_S2_before_phase'])
        targets = []
        for group, stats in target_groups[index].items():
            pc, signature, reason = group
            keys = unique_matches[index][group]
            row = dict(architectural_PC=pc, TEA_signature=signature, resolution=reason,
                       **stats, unique_matched_WB_keys=len(keys))
            if keys:
                row['unique_matched_first_S2_to_WB'] = hist_stats(Counter(
                    completed[key]['core_cycle'] - attempts[key][0]['core_cycle'] for key in keys))
                row['unique_matched_initial_issue_to_WB'] = hist_stats(Counter(
                    completed[key]['core_cycle'] - completed[key]['issue_cycle'] for key in keys))
                row['sample_weighted_causes_at_or_before_sample'] = dict(prior_causes[index][group])
            targets.append(row)
        output.append(dict(index=phase['index'], begin=phase['begin'], end=phase['end'],
                           exact_TEA_roi_scaled_CPI=phase['exact_TEA_roi_scaled_CPI'],
                           completed_WB_by_carried_PC=cohorts, S2_cause_sets_by_carried_PC=dict(s2_groups[index]),
                           scalar_keys_without_WB_by_last_S2=dict(unresolved[index]), sampled_targets=targets))
    assert sum(sum(r['samples'] for r in p['sampled_targets']) for p in output) == identity['Stalled_samples']
    assert sum(sum(c['first_S2_to_WB']['count'] for c in p['completed_WB_by_carried_PC'].values())
               for p in output) + outside['completed_scalar_keys'] == len(completed)
    assert sum(sum(p['scalar_keys_without_WB_by_last_S2'].values()) for p in output) + outside['scalar_keys_without_successful_WB'] == len(attempts) - len(completed)
    assert sum(sum(sum(c.values()) for c in p['S2_cause_sets_by_carried_PC'].values()) for p in output) + outside['S2_attempts'] == sum(map(len, attempts.values()))
    return output, dict(outside)


def analyze(folder, receipt, elf, pc_proof, retired_step, objdump='riscv64-unknown-linux-gnu-objdump', *, pcs):
    folder, receipt, elf, pc_proof = map(Path, [folder, receipt, elf, pc_proof])
    identity = load_identity(folder, receipt, elf, pc_proof, objdump, pcs=pcs)
    profile = retired_profile(folder, receipt, retired_step)
    assert identity['source'] == profile['source']
    decoded = instructions(elf, objdump)
    selected = set(pcs)
    assert selected <= decoded.keys(), 'Selected architectural PC is absent from ELF'
    carried = {decoded[pc]['last_halfword_pc'] for pc in selected}
    with (folder / 'load-writeback-probe.csv').open() as stream:
        rows = ({name: int(value) for name, value in raw.items()} for raw in csv.DictReader(stream))
        attempts, completed = match_attempts(row for row in rows if row['pc'] in carried)
    assert len(completed) == identity['validated_completed_scalar_keys']
    samples = {}

    def sample(cycle, state, lanes, weight):
        if state == 'Stalled':
            lane, = lanes
            assert cycle not in samples
            samples[cycle] = lane

    tea.load_data((folder / 'simulator.db').resolve(), folder.name,
                  SimpleNamespace(cycle_range=(identity['roi_begin'], identity['roi_end']),
                                  spec_checkpoint=False, bins=1), sample_callback=sample)
    phases, outside = partition(attempts, completed, identity, profile, samples)
    return dict(passed=True, source=identity['source'], TEA_load_identity=identity,
                retired_phase_profile=profile, phases=phases, selected_events_outside_ROI=outside,
                PC_mapping={hex(pc): decoded[pc] for pc in sorted(selected)}, S2_cause_bit_names=CAUSES,
                input_sha256={str(receipt.resolve()): digest(receipt)},
                tool_sha256={Path(__file__).name: digest(Path(__file__))},
                scope='Existing full-trace S2/WB/native validation, ELF last-halfword mapping and TEA target qualification retained. '
                      'Half-open retired-snapshot phases partition WB completion cycles, actual S2 attempts and source-sampled TEA targets separately. '
                      'Completed services retain their full pre-phase attempt history; unresolved keys are assigned by last S2 and have unknown fate. '
                      'Completed-service cause unions retain full history; sampled-target cause unions stop at the source sample. '
                      'Neither denotes cause duration. Sample counts and unique matched service histograms are distinct. '
                      'TEA targets lack full allocation flags; successful WB does not prove retirement ownership or criticality. '
                      'Snapshot thresholds do not establish equal dynamic prefixes; no exclusive CPU loss or complete-suite gain inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ['folder', 'receipt', 'elf', 'pc-proof', 'output']:
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument('--retired-step', type=int, required=True)
    parser.add_argument('--objdump', default='riscv64-unknown-linux-gnu-objdump')
    parser.add_argument('--pc', type=lambda value: int(value, 0), action='append', required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    value = analyze(args.folder, args.receipt, args.elf, args.pc_proof, args.retired_step, args.objdump, pcs=args.pc)
    args.output.write_text(json.dumps(value, indent=2) + '\n')


if __name__ == '__main__':
    main()
