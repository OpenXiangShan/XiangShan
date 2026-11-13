"""Validate the targeted S2/WB/SMS capture without requiring unrelated memory traces."""
import argparse
from collections import Counter
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_l2_return_trace import read
from tea_dip_load_writeback_trace import analyze as writebacks
from tea_dip_sms_request_trace import match_pipeline
from tea_dip_sms_internal_trace import filter_owners, generation_commits
from tea_dip_sms_training_admission import summarize


def analyze(folder, receipt):
    folder = Path(folder)
    output = writebacks(folder, receipt)
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    assert proof['SMS_request_binding']['actual_pipeline_commit_and_forward_asserted']
    assert proof['SMS_internal_binding']['actual_filter_allocation_edges'] and proof['SMS_internal_binding']['actual_arbiter_departure_owner_asserted']
    binding = proof['SMS_admission_binding']
    assert binding['actual_masks_asserted'] and binding['hash_and_PC_match_predicates_asserted'] and binding['input_lanes'] == 5
    data = {name: read(folder / ('sms-' + name + '.csv')) for name in ['l2-requests', 'training', 'generation', 'filter-state', 'training-admission']}
    boundary = lambda rows: [(r['core_cycle'], r['clean'], r['native_dump']) for r in rows if r['kind'] == 0]
    expected = [(r['core_cycle'], r['clean'], r['native_dump']) for r in read(folder / 'load-service-probe.csv') if r['lane'] == 3]
    assert all(boundary(rows) == expected for rows in data.values()), 'Targeted SMS boundary differs from S2/WB'
    packets, tail = match_pipeline(data['l2-requests'])
    owners, departures = filter_owners(data['filter-state'], data['l2-requests'])
    commits, pending = generation_commits(data['generation'], owners, max(r['core_cycle'] for r in data['filter-state']))
    blocks = read_blocks(folder / 'simulator_err.txt')
    for phase in output['phases']:
        begin, end = phase['begin_core_exclusive'], phase['end_core_inclusive']
        selected = {name: [r for r in rows if r['kind'] != 0 and begin < r['core_cycle'] <= end] for name, rows in data.items()}
        native = lambda suffix: next(v for k, v in blocks[phase['native_dump']].items() if k.endswith(suffix))
        assert native('.prefetcher.SMS_fire_l2') == [sum(r['kind'] == 1 for r in selected['l2-requests'])]
        assert native('.smsOpt.train_filter.SMS_train_filter_deq') == [len(selected['training'])]
        assert native('.smsOpt.pf_filter.sms_pf_filter_recv_req') == [len(selected['generation'])]
        ways = Counter(r['slot'] for r in selected['generation'] if r['kind'] == 1)
        for slot in range(16):
            assert native(f'.smsOpt.pf_filter.sms_pf_filter_access_way_{slot}') == [ways[slot]]
        counts, patterns = summarize(selected['training-admission'], binding['PC_qualified'])
        assert native('.smsOpt.train_filter.SMS_train_filter_enq_valid') == [len(selected['training-admission'])]
        assert native('.smsOpt.train_filter.SMS_train_filter_enq_fire') == [counts.get('allocated', 0)]
        duration, = native('.rob.clock_cycle')
        prefix = duration - (end - begin)
        assert prefix >= 0 and (begin == -1 or prefix == 0)
        active_cycles = len({r['core_cycle'] for r in selected['training-admission']})
        for kind, hist in patterns.items():
            hist[0] += duration - active_cycles
            for value in range(32):
                assert native('.smsOpt.train_filter.SMS_train_filter_' + kind + '_enq_pattern_' + format(value, 'b')) == [hist[value]], 'SMS admission pattern marginal mismatch'
        phase['SMS'] = dict(admission_counts=counts, training_dequeues=len(selected['training']), raw_generated=len(selected['generation']),
                            input_fires=sum(r['kind'] == 1 for r in selected['l2-requests']), arbiter_blocked=native('.prefetcher.SMS_block_l2'),
                            uncaptured_startup_prefix_cycles=prefix, SMS_native_marginals=117)
    output.update(SMS_packets=packets, SMS_pending_at_capture_end=tail, SMS_training_events=[r for r in data['training'] if r['kind'] == 1],
                  SMS_training_offers=[r for r in data['training-admission'] if r['kind'] == 1], SMS_generation_events=[r for r in data['generation'] if r['kind'] != 0],
                  SMS_filter_owners=owners, SMS_filter_departures=departures, SMS_generation_commits=commits, SMS_generation_pending_at_capture_end=pending,
                  SMS_scope='Source-qualified targeted observations. Same-slot allocation/arbiter identities, native patterns and actual S2/WB remain checked. No training-to-PHT causality, L2 residency or DRAM service is inferred; those require their own observers. PC tags remain20bit and missing WB fate remains unknown.')
    return output


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--folder', type=Path, required=True)
    p.add_argument('--receipt', type=Path, required=True)
    p.add_argument('--output', type=Path, required=True)
    args = p.parse_args()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt), indent=2) + '\n')


if __name__ == '__main__':
    main()
