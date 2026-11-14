"""Audit source-bound SMS filter hazards and the bits carried by same-tag arrivals."""
import argparse
from bisect import bisect_right
from collections import Counter
import json
from pathlib import Path
import re

from tea_dip_gsim_lsu_s1_probe import digest
from tea_dip_l2_return_trace import read
from tea_dip_sms_pht_pipeline_trace import analyze as pipeline


def guard_policy(source):
    text = re.sub(r'\s+', '', source.split('class PrefetchFilter(', 1)[1].split('\nclass ', 1)[0])
    for expression in ['valprev_gen_req=RegEnable(io.gen_req.bits,io.gen_req.valid)',
                       'vals0_gen_req_valid=io.gen_req.valid&&!s0_match_prev',
                       'vals1_valid_r=GatedValidRegNext(s0_gen_req_valid,false.B)',
                       's1_valid:=s1_valid_r']:
        assert expression in text, 'Filter stage equation differs: ' + expression
    guards = re.findall(r'vals0_match_prev=(\w+)&&\(s0_gen_req.region_tag===prev_gen_req.region_tag\)', text)
    assert len(guards) == 1 and guards[0] in ['prev_valid', 's1_valid'], 'Unsupported filter hazard guard'
    if guards[0] == 'prev_valid':
        assert 'valprev_valid=GatedValidRegNext(io.gen_req.valid,false.B)' in text
    return 'raw' if guards[0] == 'prev_valid' else 'accepted'


def audit(rows, states, policy):
    arrivals = [r for r in rows if r['kind'] == 1 and r['filter_input']]
    assert all(r['kind'] in [0, 1] for r in rows)
    assert len({r['core_cycle'] for r in arrivals}) == len(arrivals), 'Duplicate filter input'
    histories = {slot: [r for r in states if r['kind'] == 1 and r['slot'] == slot] for slot in range(16)}
    cycles = {slot: [r['core_cycle'] for r in history] for slot, history in histories.items()}
    events, previous = [], None
    for row in arrivals:
        cycle = row['core_cycle']
        assert previous is None or cycle > previous['core_cycle'], 'Filter input cycles moved backwards'
        adjacent = previous is not None and cycle == previous['core_cycle'] + 1
        same = adjacent and row['output_tag'] == previous['output_tag']
        protected = bool(same and previous['filter_accepted'])
        blocked = same and (policy == 'raw' or previous['filter_accepted'])
        assert bool(row['filter_accepted']) == (not blocked), 'Actual acceptance differs from bound guard'
        reason = 'prior_accepted_same_tag' if protected else 'prior_suppressed_same_tag' if same else 'no_same_tag_hazard'
        event = dict(core_cycle=cycle, accepted=bool(row['filter_accepted']), reason=reason)
        if reason == 'prior_suppressed_same_tag':
            matches = []
            for slot, history in histories.items():
                at = bisect_right(cycles[slot], cycle) - 1
                if at >= 0:
                    current = history[at]
                    if current['allocated'] and current['region_tag'] == row['output_tag']:
                        matches.append((slot, current))
            assert len(matches) <= 1, 'Multiple same-tag filter owners'
            event['bit_fate'] = 'owner_or_address_unknown'
            if matches:
                slot, current = matches[0]
                if row['output_physical'] and current['physical_valid'] and current['region_address'] == row['output_region']:
                    fresh = row['output_bits'] & ~current['region_bits'] & 65535
                    event.update(bit_fate='adds_bits' if fresh else 'already_present', slot=slot,
                                 incoming_bits=row['output_bits'], observed_bits=current['region_bits'], fresh_bits=fresh)
        events.append(event)
        previous = row
    return events


def summarize(events):
    reasons = Counter(r['reason'] for r in events)
    bits = Counter(r['bit_fate'] for r in events if 'bit_fate' in r)
    assert sum(reasons.values()) == len(events)
    assert sum(bits.values()) == reasons['prior_suppressed_same_tag']
    return dict(raw=len(events), accepted=sum(r['accepted'] for r in events), suppressed=sum(not r['accepted'] for r in events),
                reasons=dict(reasons), prior_suppressed_bit_fates=dict(bits),
                fresh_bit_occurrences=sum(r.get('fresh_bits', 0).bit_count() for r in events),
                examples=[r for r in events if r.get('bit_fate') == 'adds_bits'][:5])


def analyze(folder, receipt, source):
    folder, receipt, source = Path(folder), Path(receipt), Path(source)
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    expected, = {h for name, h in proof['SMS_PHT_pipeline_binding']['source_sha256'].items()
                 if name.endswith('/SMSPrefetcher.scala')}
    assert digest(source) == expected, 'Filter source differs from captured build'
    policy = guard_policy(source.read_text())
    old = pipeline(folder, receipt)
    events = audit(read(folder / 'sms-pht-pipeline.csv'), read(folder / 'sms-filter-state.csv'), policy)
    assert len(events) == len(old['SMS_generation_events']), 'Missing filter input'
    phases = []
    for phase in old['phases']:
        begin, end = phase['begin_core_exclusive'], phase['end_core_inclusive']
        counts = summarize([r for r in events if begin < r['core_cycle'] <= end])
        assert counts['raw'] == phase['SMS']['raw_generated']
        assert counts['accepted'] == phase['PHT_pipeline']['filter_fates'].get('accepted', 0)
        phases.append(dict(begin_core_exclusive=begin, end_core_inclusive=end, counts=counts))
    return dict(passed=True, source=proof['source'], source_sha256=expected, receipt_sha256=digest(receipt),
                guard_policy=policy, full_capture=summarize(events), phases=phases,
                new_RTL_state_bits=0, new_guests=0,
                scope='Actual raw/accepted previous-cycle guard and existing two-cycle commit/native margins checked. '
                      'Bit partitions use the observed same-cycle filter state. On raw-guard captures these are suppressed '
                      'arrivals, not a replay of changed execution; on accepted-guard captures these are actual releases. '
                      'Repeated bit occurrences are not unique useful lines, cache residency, critical loads or saved CPU cycles.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--source', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt, args.source), indent=2) + '\n')


if __name__ == '__main__':
    main()
