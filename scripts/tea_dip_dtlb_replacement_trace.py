"""Replay actual PLRU transitions and diagnose pending-refill victim selection."""
import argparse
from collections import Counter
import csv
import json
from pathlib import Path

from tea_dip_dtlb_trace import analyze as analyze_dtlb
from tea_dip_gsim_dtlb_replacement_probe import COLUMNS
from tea_dip_query_endpoint_evidence import digest


def next_state(state, way, ways):
    if ways <= 1:
        return 0
    if ways == 2:
        return 1 - (way & 1)
    right = 1 << ((ways - 1).bit_length() - 1)
    left = ways - right
    choose_left = bool(way & right)
    ls, rs = (state >> (right - 1)) & ((1 << (left - 1)) - 1), state & ((1 << (right - 1)) - 1)
    if choose_left:
        ls = next_state(ls, way & ((1 << (left - 1).bit_length()) - 1), left)
    else:
        rs = next_state(rs, way & (right - 1), right)
    return (int(not choose_left) << (ways - 2)) | (ls << (right - 1)) | rs


def victim(state, ways):
    if ways <= 1:
        return 0
    if ways == 2:
        return state & 1
    right = 1 << ((ways - 1).bit_length() - 1)
    left = ways - right
    choose_left = (state >> (ways - 2)) & 1
    if choose_left:
        ls = (state >> (right - 1)) & ((1 << (left - 1)) - 1)
        return right | victim(ls, left)
    return victim(state & ((1 << (right - 1)) - 1), right)


def analyze(folder, proof_path):
    folder, proof_path = Path(folder).resolve(), Path(proof_path).resolve()
    dtlb = analyze_dtlb(folder, proof_path)
    proof = json.loads(proof_path.read_text())
    binding = proof['binding']['DTLB_replacement_binding']
    assert binding['columns'] == COLUMNS and binding['core_ranges'] == proof['binding']['DTLB_binding']['core_ranges']
    selector_state = binding.get('victim_state', 'state')
    assert selector_state in ['state', 'next_state'], 'Unknown victim-state selection'
    controls = {}
    with (folder / 'load-dtlb-probe.csv').open() as stream:
        for raw in csv.DictReader(stream):
            if raw['kind'] == '0':
                controls[int(raw['core_cycle'])] = {k: int(v) for k, v in raw.items()}
    records = {}
    with (folder / 'load-dtlb-replacement-probe.csv').open() as stream:
        reader = csv.DictReader(stream)
        assert reader.fieldnames == ['core_cycle', 'clean', 'native_dump'] + COLUMNS
        for raw in reader:
            row = {k: int(v) for k, v in raw.items()}
            cycle = row['core_cycle']
            assert cycle not in records, 'Duplicate replacement cycle'
            records[cycle] = row
    assert set(records) == set(controls), 'Missing replacement cycle'
    counts = Counter()
    origins = {name: dict(observed_cycles=0, touch_cycles_by_port=[0] * binding['ports'])
               for name in ['refill_override', 'no_refill_override', 'unknown_previous_cycle']}
    for cycle, row in sorted(records.items()):
        control = controls[cycle]
        assert all(row[k] == control[k] for k in ['clean', 'native_dump']), 'Boundary phase mismatch'
        assert row['victim'] == victim(row[selector_state], binding['ways']) == control['refill_way'], 'Actual selector victim mismatch'
        expected = row['state']
        for i in range(binding['ports']):
            assert row[f'touch_valid_{i}'] in (0, 1)
            if row[f'touch_valid_{i}']:
                way = row[f'touch_way_{i}']
                assert 0 <= way < binding['ways']
                expected = next_state(expected, way, binding['ways'])
        assert row['next_state'] == expected, 'Pending touches do not reproduce NEXT'
        row['forwarded_victim'] = victim(expected, binding['ways'])
        if selector_state == 'next_state':
            row['registered_state_victim'] = victim(row['state'], binding['ways'])
        row['write_valid'] = bool(control['refill_mask'])
        row['pending_refill'] = None
        previous = records.get(cycle - 1)
        if previous is not None:
            assert row['state'] == previous['next_state'], 'Registered state transition mismatch'
            row['pending_refill'] = previous['write_valid']
            if row['pending_refill']:
                assert all(row[f'touch_valid_{i}'] and row[f'touch_way_{i}'] == previous['victim']
                           for i in range(binding['ports'])), 'Delayed refill touch mismatch'
                assert row['forwarded_victim'] != previous['victim'], 'PLRU touch should protect the preceding refill'
        active = any(row[f'touch_valid_{i}'] for i in range(binding['ports']))
        group = 'unknown_prior_cycle' if row['pending_refill'] is None else 'pending_refill' if row['pending_refill'] else 'hit_touches_only' if active else 'idle'
        counts[group] += 1
        origin = origins['unknown_previous_cycle' if row['pending_refill'] is None else
                         'refill_override' if row['pending_refill'] else 'no_refill_override']
        origin['observed_cycles'] += 1
        for i in range(binding['ports']):
            origin['touch_cycles_by_port'][i] += row[f'touch_valid_{i}']
        if row['write_valid']:
            counts['writes'] += 1
            counts['writes_with_selector_change'] += row['victim'] != row['forwarded_victim']
            if selector_state == 'next_state':
                counts['writes_with_register_decode_difference'] += row['victim'] != row['registered_state_victim']
            if row['pending_refill']:
                counts['back_to_back_writes'] += 1
                counts['back_to_back_same_victim'] += row['victim'] == previous['victim']
    result = dict(passed=True, source=dtlb['source'], core_ranges=binding['core_ranges'],
                observed_cycles=len(records), counts=dict(counts), records=list(records.values()),
                touch_origin=dict(groups=origins, scope='Actual valid-touch port cycles partitioned by the preceding write. '
                                  'Refill overrides are not lookup-client requests. Gap history is unknown; '
                                  'port cycles are not exclusive CPU loss.'),
                input_sha256=dict(dtlb['input_sha256'], **{str(folder / 'load-dtlb-replacement-probe.csv'): digest(folder / 'load-dtlb-replacement-probe.csv')}),
                tool_sha256=digest(Path(__file__)), scope='Source-bound actual PLRU recurrence and registered transitions. '
                'All four delayed-refill touches are checked against the preceding actual write. '
                'Forwarded victim is a local counterfactual on the observed state; subsequent executions, '
                'other victims and performance can change and require a real candidate experiment. '
                'First-cycle history remains unknown. Shared prefix does not explain a relative long-window regression.')
    if selector_state == 'next_state':
        result.update(victim_state=selector_state,
                      scope='Source-bound actual victim decodes NEXT from the recorded pending touches. '
                            'registered_state_victim decodes the old state only on this same observed trajectory; '
                            'it is not another execution or a performance estimate. All delayed refill touches and '
                            'registered transitions are checked; first-cycle history is unknown.')
    return result


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--folder', type=Path, required=True)
    p.add_argument('--build-proof', type=Path, required=True)
    p.add_argument('--output', type=Path, required=True)
    a = p.parse_args()
    result = analyze(a.folder, a.build_proof)
    assert not a.output.exists()
    a.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps({k: result[k] for k in ['passed', 'observed_cycles', 'counts']}))


if __name__ == '__main__':
    main()
