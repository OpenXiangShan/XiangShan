"""Reconcile buffered SMS training admission with native phase counters and old observations."""
import argparse
from collections import Counter, defaultdict
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_l2_return_trace import read
from tea_dip_sms_internal_trace import analyze as internal


def summarize(rows, qualified):
    by_cycle = defaultdict(list)
    counts = Counter()
    for row in rows:
        lane = row['lane']
        assert row['kind'] == 1 and 0 <= lane < 3 and 0 <= row['occupancy'] <= 8
        assert row['entry_same_pc_mask'] & ~row['entry_mask'] == row['prev_same_pc_mask'] & ~row['prev_mask'] == 0
        assert row['entry_mask'] < 256 and row['prev_mask'] < (1 << lane)
        em = row['entry_same_pc_mask'] if qualified else row['entry_mask']
        pm = row['prev_same_pc_mask'] if qualified else row['prev_mask']
        assert row['need_alloc'] == int(not (em or pm)) and row['can_alloc'] in [0, 1] and row['can_alloc'] <= row['need_alloc']
        by_cycle[row['core_cycle']].append(row)
        if not row['need_alloc']:
            reason = 'entry_and_previous_duplicate' if em and pm else 'entry_duplicate' if em else 'previous_duplicate'
        else:
            reason = 'allocated' if row['can_alloc'] else 'capacity_blocked'
        counts[reason] += 1
        counts['offers'] += 1
        if row['entry_mask'] or row['prev_mask']:
            counts['hash_duplicate_offers'] += 1
            counts['different_PC_only_hash_duplicates'] += int(not (row['entry_same_pc_mask'] or row['prev_same_pc_mask']))
    patterns = {name: Counter() for name in ['raw', 'filtered', 'actual']}
    for group in by_cycle.values():
        lanes = [r['lane'] for r in group]
        assert len(lanes) == len(set(lanes)), 'Duplicate SMS offer cycle/lane'
        assert len({r['occupancy'] for r in group}) == 1
        patterns['raw'][sum(1 << (4 - r['lane']) for r in group)] += 1
        patterns['filtered'][sum(r['need_alloc'] << (4 - r['lane']) for r in group)] += 1
        patterns['actual'][sum(r['can_alloc'] << (4 - r['lane']) for r in group)] += 1
    assert sum(counts[k] for k in ['allocated', 'capacity_blocked', 'entry_duplicate', 'previous_duplicate', 'entry_and_previous_duplicate']) == len(rows)
    return dict(counts), patterns


def analyze(folder, receipt):
    folder = Path(folder)
    output = internal(folder, receipt)
    binding = json.loads((folder / 'probe-build-proof.json').read_text())['SMS_admission_binding']
    assert binding['input_lanes'] == 5 and binding['active_load_lanes'] == 3 and binding['new_RTL_state_bits'] == 0
    assert binding['actual_masks_asserted'] and binding['hash_and_PC_match_predicates_asserted']
    rows = read(folder / 'sms-training-admission.csv')
    boundary = lambda rs: [(r['core_cycle'], r['clean'], r['native_dump']) for r in rs if r['kind'] == 0]
    assert boundary(rows) == boundary(read(folder / 'sms-training.csv')), 'Training admission phase boundary mismatch'
    events = [r for r in rows if r['kind'] == 1]
    assert all(a['core_cycle'] <= b['core_cycle'] for a, b in zip(events, events[1:]))
    blocks = read_blocks(folder / 'simulator_err.txt')
    for phase in output['phases']:
        begin, end = phase['begin_core_exclusive'], phase['end_core_inclusive']
        selected = [r for r in events if begin < r['core_cycle'] <= end]
        counts, patterns = summarize(selected, binding['PC_qualified'])
        native = lambda suffix: next(values for name, values in blocks[phase['native_dump']].items() if name.endswith('.smsOpt.train_filter.SMS_train_filter_' + suffix))
        assert native('enq_valid') == [len(selected)], 'SMS offer native marginal mismatch'
        assert native('enq_fire') == [counts.get('allocated', 0)], 'SMS allocation native marginal mismatch'
        duration, = [values[0] for name, values in blocks[phase['native_dump']].items() if name.endswith('.rob.clock_cycle')]
        startup_prefix = duration - (end - begin)
        assert startup_prefix >= 0 and (begin == -1 or startup_prefix == 0)
        active_cycles = len({r['core_cycle'] for r in selected})
        for kind, hist in patterns.items():
            hist[0] += duration - active_cycles
            for pattern in range(32):
                assert native(kind + '_enq_pattern_' + format(pattern, 'b')) == [hist[pattern]], 'SMS admission pattern marginal mismatch'
        phase['SMS_training_admission'] = dict(counts=counts, native_marginals=98, native_cycle_duration=duration, uncaptured_startup_prefix_cycles=startup_prefix)
    output.update(SMS_training_offers=events, SMS_training_admission_scope='Actual buffered offers partition into duplicate/capacity/allocated classes. Hash matches may alias addresses; PC is a20-bit tag. Different-PC-only matches predict dedup eligibility, not benefit, training-to-PHT ownership, full retired-PC identity or savings. Zero patterns use the actual native cycle budget; startup prefix cycles before the observed core domain remain explicit.')
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
