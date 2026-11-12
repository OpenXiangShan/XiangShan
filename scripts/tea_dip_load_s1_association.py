"""Associate TEA-selected unfinished loads with same-uop actual S1 attempts.

This records first/last successful-address observations and TLB-miss attempts,
not the legacy TLBLATENCY field. A noQuery address is not a translation hit.
Intervals bound observed stages; they do not identify individual PTW requests,
unique L2/CHI transactions or additive CPU cycles that an optimization can save.
"""
import argparse
from collections import Counter,defaultdict
import csv
import json
from pathlib import Path
import sqlite3

from tea_dip_load_txreq_association import analyze as load_targets
from tea_dip_lsu_s1_trace import analyze as audit_s1


def associate(load, events):
    """Keep all observed scalar attempts in this load's allocation-to-WB life."""
    selected = [event for event in events
                if load['allocation_cycle'] <= event['core_cycle'] < load['writeback_cycle']
                and event['instr_type'] == 1
                and event['virtual_address'] == load['virtual_address']]
    selected.sort(key=lambda event:(event['core_cycle'],event['lane']))
    alive = [event for event in selected if not event['killed']]
    hits = [event for event in alive if event['tlb_hit'] and not event['exception'] and not event['special']]
    addresses = [event for event in alive if event['paddr_effective'] and not event['exception'] and not event['special']]
    row = dict(load, observed_attempts=len(selected), killed_attempts=len(selected)-len(alive),
               nonkilled_attempts=len(alive), tlb_miss_attempts=sum(event['tlb_miss']for event in alive),
               no_query_attempts=sum(event['no_query']for event in alive), successful_translation_attempts=len(hits),
               successful_address_attempts=len(addresses), attempts=selected)
    if not alive:
        return dict(row,status='no_matching_nonkilled_scalar_attempt')
    if not addresses:
        return dict(row,status='no_successful_address_observed')
    first, last = addresses[0], addresses[-1]
    first_attempt = alive[0]['core_cycle']
    later_misses = [event for event in alive if event['core_cycle'] > first['core_cycle'] and event['tlb_miss']]
    row['TLB_miss_attempts_after_first_address'] = len(later_misses)
    row['TLB_miss_cycles_after_first_address'] = [event['core_cycle']for event in later_misses]
    row.update(status='observed_address_stage',first_attempt_core_cycle=first_attempt,
               first_address_core_cycle=first['core_cycle'],last_address_core_cycle=last['core_cycle'],
               first_address_kind='no_query'if first['no_query']else'translation_hit',
               attempts_to_first_address_cycles=first['core_cycle']-first_attempt,
               first_address_to_WB_cycles=load['writeback_cycle']-first['core_cycle'],
               later_address_retry_span_cycles=last['core_cycle']-first['core_cycle'],
               last_address_to_WB_cycles=load['writeback_cycle']-last['core_cycle'],
               first_address_before_first_TEA_sample=first['core_cycle']<min(load['sample_cycles']),
               first_address_physical_line_matches_WB=first['physical_address']>>6==load['physical_address']>>6)
    assert row['attempts_to_first_address_cycles']+row['first_address_to_WB_cycles']==load['writeback_cycle']-first_attempt
    return row


def analyze(folder, receipt):
    folder = Path(folder).resolve()
    audit = audit_s1(folder,receipt)
    targets = load_targets(folder,receipt)
    assert targets['clock_alignment']['core_to_native_offset']==audit['core_to_native_offset']
    rows, events, groups = [],defaultdict(list),{}
    with (folder/'lsu-s1-probe.csv').open() as stream:
        for raw in csv.DictReader(stream):
            event = {key:int(value)for key,value in raw.items()}
            if event['lane']<3:
                events[event['rob_index'],event['rank'],event['rob_flag'],event['uop_index']].append(event)
    with sqlite3.connect((folder/'simulator.db').as_uri()+'?mode=ro',uri=True) as db:
        db.row_factory = sqlite3.Row
        writebacks = {row['ID']:dict(row)for row in db.execute('SELECT ID,ROBIDX,WBROBFLAG,WBTEARANK,WBUOPIDX,DVADDR FROM InstTable0')}
    for sample in targets['samples']:
        if 'writeback_id'not in sample or sample['sample_cycle']>=sample['writeback_cycle']:
            continue
        wb = writebacks[sample['writeback_id']]
        load = groups.setdefault(wb['ID'],dict(writeback_id=wb['ID'],pc=sample['pc'],
            allocation_cycle=sample['allocation_cycle'],writeback_cycle=sample['writeback_cycle'],
            final_issue_core_cycle=sample['issue_cycle'],physical_address=sample['physical_address'],
            virtual_address=wb['DVADDR']&((1<<50)-1),sample_cycles=[],
            identity=(wb['ROBIDX'],wb['WBTEARANK'],wb['WBROBFLAG'],wb['WBUOPIDX'])))
        assert load['allocation_cycle']==sample['allocation_cycle']
        load['sample_cycles'].append(sample['sample_cycle'])
    for load in groups.values():
        rows.append(associate(load,events[load['identity']]))
    statuses = Counter(row['status']for row in rows)
    assert sum(statuses.values())==len(groups)
    pcs = {}
    for pc in sorted({row['pc']for row in rows}):
        cohort = [row for row in rows if row['pc']==pc]
        pcs[pc] = dict(unique_sampled_loads=len(cohort),statuses=dict(Counter(row['status']for row in cohort)),
            loads_with_TLB_miss_attempts=sum(row['tlb_miss_attempts']>0 for row in cohort),
            loads_with_multiple_successful_addresses=sum(row['successful_address_attempts']>1 for row in cohort),
            first_address_before_first_sample=sum(row.get('first_address_before_first_TEA_sample',False)for row in cohort),
            loads_with_TLB_miss_after_first_address=sum(row.get('TLB_miss_attempts_after_first_address',0)>0 for row in cohort))
    return dict(passed=True,source=targets['source'],unique_sampled_loads=len(rows),statuses=statuses,
                core_to_native_offset=audit['core_to_native_offset'],PCs=pcs,loads=rows,
                excluded_target_samples=targets['sampled_stalled_targets']-sum(len(row['sample_cycles'])for row in rows),
                scope='Same-ROB-flag/index/rank/uop and virtual-address scalar S1 events inside TEA allocation-to-unique-WB lifetime. Repeated samples deduplicated by WB ID, killed/noQuery/unknown and later retries retained. First observed successful address can predate final issue and is not a unique PTW/CHI transaction. Conditional load cohorts differ between versions; observed intervals cannot be summed or converted to recoverable CPI. No legacy TLBLATENCY or shared lsInfo attribution.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder',type=Path,required=True)
    args = parser.parse_args()
    print(json.dumps(analyze(args.folder,args.folder/'closure-receipt.json'),indent=2))


if __name__ == '__main__':
    main()
