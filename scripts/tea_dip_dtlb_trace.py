"""Join bounded TLB provenance to actual S2 attempts, retaining unobserved history."""
import argparse
from collections import Counter, defaultdict
import csv
import json
from pathlib import Path

from tea_dip_gsim_dtlb_probe import COLUMNS
from tea_dip_load_writeback_trace import KEY
from tea_dip_query_endpoint_evidence import digest, verify

ENTRY = ['entry_valid', 'entry_tag', 'entry_asid', 'entry_vmid', 'entry_s2xlate',
         'entry_level', 'entry_global', 'valididx', 'pteidx']


def analyze(folder, proof_path):
    folder, proof_path = Path(folder).resolve(), Path(proof_path).resolve()
    receipt = json.loads((folder / 'closure-receipt.json').read_text())
    result = json.loads((folder / 'result.json').read_text())
    proof = json.loads(proof_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and result['original_process_closed']
    assert result['exitcode'] == 0 and result['difftest_passed'] and all(result['sampler_flags'].values())
    assert result['process'] == receipt['process'] and result['source'] == receipt['source'] == proof['source']
    assert proof['passed'] and proof['original_files_unchanged'] and proof['binary_sha256'] == result['binary_sha256']
    verify(folder, receipt['files'])
    binding = proof['binding']['DTLB_binding']
    assert binding['columns'] == COLUMNS
    cycles, requests = {}, []
    with (folder / 'load-dtlb-probe.csv').open() as stream:
        for raw in csv.DictReader(stream):
            row = {name: int(value) for name, value in raw.items()}
            cycle, kind = row['core_cycle'], row['kind']
            group = cycles.setdefault(cycle, dict(entries={}, requests=[]))
            if kind == 0:
                assert 'control' not in group
                group['control'] = row
            elif kind == 1:
                assert 0 <= row['lane'] < 3
                assert bool(row['combined_hit']) == bool(row['storage_hit_mask'] or row['ptw_bypass_hit'])
                group['requests'].append(row)
                requests.append(row)
            else:
                assert kind == 2 and 0 <= row['way'] < binding['ways'] and row['way'] not in group['entries']
                group['entries'][row['way']] = {name: row[name] for name in ENTRY}
    assert cycles, 'No bounded translation observations'
    for group in cycles.values():
        assert 'control' in group and set(group['entries']) == set(range(binding['ways']))
    wanted = {(row['core_cycle'] + 1, row['lane']) for row in requests}
    s2, last = defaultdict(list), -1
    with (folder / 'load-service-probe.csv').open() as stream:
        for raw in csv.DictReader(stream):
            cycle, lane = int(raw['core_cycle']), int(raw['lane'])
            last = max(last, cycle)
            if (cycle, lane) in wanted:
                s2[cycle, lane].append({name: int(value) for name, value in raw.items()})
    expected = {c for begin, end in binding['core_ranges'] for c in range(begin, min(end, last + 1))}
    assert set(cycles) == expected, 'Missing bounded cycle or unexpected range'
    outcomes, provenance = Counter(), Counter()
    for row in requests:
        matches = [r for r in s2[row['core_cycle'] + 1, row['lane']]
                   if all(r[name] == row[name] for name in KEY + ['pc', 'virtual_address'])]
        assert len(matches) <= 1, 'Ambiguous next-stage carried identity'
        if not matches:
            row['S2_status'] = 'no_qualified_next_S2'
            outcomes[row['S2_status']] += 1
            continue
        matched, = matches
        expected_result = (1 if row['no_query'] else
                           2 if row['request_valid'] and (row['combined_hit'] or not row['translate']) else 4)
        assert matched['tlb_access_result'] == expected_result, (row, matched)
        row['S2_status'] = 'matched_next_S2'
        row['S2_core_cycle'] = matched['core_cycle']
        row['S2_tlb_access_result'] = matched['tlb_access_result']
        row['S2_instr_type'] = matched['instr_type']
        outcomes[row['S2_status']] += 1
        label = ('no_query' if row['no_query'] else 'storage_and_bypass' if row['storage_hit_mask'] and row['ptw_bypass_hit'] else
                 'storage_only' if row['storage_hit_mask'] else 'bypass_only' if row['ptw_bypass_hit'] else
                 'translation_disabled' if not row['translate'] else 'miss')
        row['hit_provenance'] = label
        provenance[label] += 1
    changes = []
    for cycle, group in sorted(cycles.items()):
        previous = cycles.get(cycle - 1)
        if previous is None:
            continue
        for way, entry in group['entries'].items():
            before = previous['entries'][way]
            if entry != before:
                changes.append(dict(core_cycle=cycle, way=way, before=before, after=entry,
                                    preceding_refill_mask=previous['control']['refill_mask'],
                                    preceding_sfence=previous['control']['sfence'],
                                    preceding_satp_changed=previous['control']['satp_changed']))
    transitions, prior_hits = [], {}
    for row in requests:
        if (row.get('S2_status') != 'matched_next_S2' or row['S2_instr_type'] != 1 or
                not row['issue_cycle'] or row['no_query'] or not row['translate']):
            continue
        key = tuple(row[name] for name in KEY + ['virtual_address'])
        if row['combined_hit']:
            prior_hits[key] = row
        elif key in prior_hits:
            hit = prior_hits.pop(key)
            begin, end = hit['core_cycle'], row['core_cycle']
            covered = all(c in cycles for c in range(begin, end + 1))
            transition = dict(identity={name: row[name] for name in KEY}, virtual_address=row['virtual_address'],
                              hit_core=begin, miss_core=end, previous_hit_provenance=hit['hit_provenance'],
                              previous_storage_hit_mask=hit['storage_hit_mask'], interval_covered=covered)
            if covered:
                transition['writes_to_previous_hit_ways'] = [c for c in range(begin, end)
                    if cycles[c]['control']['refill_mask'] & hit['storage_hit_mask']]
                transition['context_events'] = [c for c in range(begin, end)
                    if cycles[c]['control']['sfence'] or cycles[c]['control']['satp_changed']]
            transitions.append(transition)
    files = [folder / 'load-dtlb-probe.csv', folder / 'load-service-probe.csv',
             folder / 'closure-receipt.json', proof_path]
    return dict(passed=True, source=result['source'], core_ranges=binding['core_ranges'],
                observed_cycles=len(cycles), ways=binding['ways'], S1_requests=len(requests),
                S2_outcomes=dict(outcomes), matched_provenance=dict(provenance),
                requests=requests, entry_changes=changes, hit_then_miss=transitions,
                input_sha256={str(p): digest(p) for p in files}, tool_sha256=digest(Path(__file__)),
                scope='Exact carried key/PC/VA and next-cycle same-lane S2 joins. Missing S2 stays unknown '
                      '(including cancellation or non-acceptance). Registered hit masks describe the preceding '
                      'lookup; current entry snapshots and next-write refill masks retain their phases. '
                      'Window-external history is unknown; hit-to-miss sequences require scalar S2 type and nonzero issue identity. '
                      'Writes/context events are observations, not unique '
                      'causes or exclusive CPU loss; bypass-only hit does not prove later residency.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--build-proof', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    result = analyze(args.folder, args.build_proof)
    assert not args.output.exists()
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps({k: result[k] for k in ['passed', 'observed_cycles', 'S1_requests', 'S2_outcomes', 'matched_provenance']}))


if __name__ == '__main__':
    main()
