"""Associate sampled unfinished loads with existing writeback addresses and L2 owners.

A bounded TEA allocation-to-final lifetime and unique representative uop recover
an address. A same-line live MSHR is a candidate association, not request identity
or proof of CPU-critical delay. No CPU cycles are attributed to these samples.
"""
from collections import Counter, defaultdict
from pathlib import Path
import argparse
import hashlib
import json
import sqlite3

from tea_dip_clock_alignment import core_native_alignment
from tea_dip_query_endpoint_evidence import verify
from tea_dip_mshr_lifecycle import analyze as lifecycle


def representative(row):
    return (row['WBROBFLAG'] == row['SLOTROBFLAG'] and
            row['WBTEARANK'] == row['SLOTTEARANK'] and
            row['WBUOPIDX'] == row['SLOTUOPIDX'])


def target_writebacks(target, final, rows):
    """Keep a precise lifetime; never choose the closest writeback on a reused ROB slot."""
    return [row for row in rows if representative(row) and
            row['ROBIDX'] == target['TARGET_ROBINDEX'] and
            row['WBTEARANK'] == target['TARGET_RANK'] and
            target['TARGET_ALLOCATIONCYCLE'] <= row['STAMP'] <= final['CYCLE']]


def live_owners(episodes, physical_address, cycle):
    # CoupledL2 restores tag/set, two bank bits, then six block-offset bits.
    block = physical_address >> 6
    return [owner for owner in episodes.get((block & 3, block >> 2), [])
            if owner['start'] < cycle and (owner['end'] is None or cycle <= owner['end'])]


def analyze(folder, receipt_path):
    folder, receipt_path = folder.resolve(), receipt_path.resolve()
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
    assert all(result['sampler_flags'].values()) and receipt['source'] == result['source']
    assert receipt['process'] == result['process']
    verify(folder, receipt['files'])
    database = folder / 'simulator.db'
    native = lifecycle(database, folder / 'simulator_err.txt')
    assert native['passed']
    writebacks, finals, episodes = (defaultdict(list) for _ in range(3))
    counts = Counter()
    rows = []
    with sqlite3.connect(database.as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        clock = core_native_alignment(db, folder / 'simulator_err.txt')
        offset = clock['core_to_native_offset']
        for item in db.execute('SELECT * FROM InstTable0 ORDER BY STAMP,ID'):
            item = dict(item)
            assert item['WRITEBACKTIME'] == item['STAMP'], 'WB/core table clock alignment differs'
            counts['all_writebacks'] += 1
            counts['nonzero_writeback_addresses'] += bool(item['DPADDR'])
            writebacks[item['ROBIDX'], item['WBTEARANK']].append(item)
        for item in db.execute('SELECT * FROM TeaDelayedFinal_0'):
            assert item['CYCLE'] == item['STAMP'], 'TEA/core table clock alignment differs'
            key = tuple(item['MEMBER_TARGET_' + suffix] for suffix in ['ALLOCATIONCYCLE', 'ROBINDEX', 'RANK'])
            finals[key].append(dict(item))
        active = {}
        for item in db.execute('SELECT * FROM L2MshrLifecycle ORDER BY NATIVECYCLE,EVENTS_BANK'):
            cycle, bank = item['NATIVECYCLE'], item['EVENTS_BANK']
            assert cycle == item['STAMP'] and 0 <= bank < 4
            for entry in range(16):
                if item[f'EVENTS_FREED_{entry}_VALID']:
                    owner = active.pop((bank, entry))
                    assert cycle - owner['start'] == item[f'EVENTS_FREED_{entry}_BITS']
                    owner['end'] = cycle
                    episodes[bank, owner['line']].append(owner)
            if item['EVENTS_ALLOCATED_VALID']:
                entry = item['EVENTS_ALLOCATED_BITS_ENTRY']
                assert (bank, entry) not in active
                active[bank, entry] = dict(bank=bank, entry=entry, start=cycle,
                    line=item['EVENTS_ALLOCATED_BITS_LINE'], source=item['EVENTS_ALLOCATED_BITS_SOURCE'],
                    prefetch=bool(item['EVENTS_ALLOCATED_BITS_PREFETCH']),
                    channel=item['EVENTS_ALLOCATED_BITS_CHANNEL'], miss=bool(item['EVENTS_ALLOCATED_BITS_MISS']))
            if item['EVENTS_MERGED_VALID']:
                owner = active[bank, item['EVENTS_MERGED_BITS_ENTRY']]
                assert owner['line'] == item['EVENTS_MERGED_BITS_LINE']
                owner.update(merge_cycle=cycle, merge_source=item['EVENTS_MERGED_BITS_SOURCE'])
        for (bank, entry), owner in active.items():
            owner['end'] = None
            episodes[bank, owner['line']].append(owner)
        for target in db.execute('SELECT * FROM Tea_0 WHERE STATE=1 AND TARGETVALID ORDER BY CYCLE,ID'):
            cycle = target['CYCLE']
            assert cycle == target['STAMP']
            key = tuple(target['TARGET_' + suffix] for suffix in ['ALLOCATIONCYCLE', 'ROBINDEX', 'RANK'])
            row = dict(sample_cycle=cycle, allocation_cycle=key[0], rob_index=key[1], rank=key[2])
            endings = finals.get(key, [])
            if len(endings) != 1:
                row['status'] = 'missing_or_ambiguous_final'
            elif endings[0]['CANCELLED'] or endings[0]['ISDRAIN']:
                row['status'] = 'cancelled_or_drain_final'
            else:
                final = endings[0]
                candidates = target_writebacks(target, final, writebacks.get((key[1], key[2]), []))
                row['pc'] = hex(final['MEMBER_PC'])
                if len(candidates) != 1:
                    row['status'] = 'missing_or_ambiguous_representative_writeback'
                else:
                    wb = candidates[0]
                    if wb['SLOTFUTYPE'] != 1 << 16:
                        row['status'] = 'non_load_representative'
                    elif not wb['DPADDR']:
                        row['status'] = 'zero_load_address'
                    else:
                        counts['unique_load_addresses'] += 1
                        row.update(physical_address=wb['DPADDR'], writeback_id=wb['ID'],
                                   issue_cycle=wb['ISSUETIME'], writeback_cycle=wb['WRITEBACKTIME'])
                        if not wb['ISSUETIME'] <= cycle < wb['WRITEBACKTIME']:
                            row['status'] = 'outside_final_issue_writeback_interval'
                        else:
                            owners = live_owners(episodes, wb['DPADDR'], cycle + offset)
                            if len(owners) != 1:
                                row['status'] = 'no_unique_same_line_live_owner'
                            else:
                                owner = owners[0]
                                cpu = bool(owner['channel'] & 1 and owner['miss'] and
                                    (owner['source'] == 2 or (owner.get('merge_source') == 2 and
                                                            owner['merge_cycle'] < cycle + offset)))
                                row.update(status='same_line_live_owner_candidate', owner=owner,
                                           CPU_load_owner_or_preexisting_merge=cpu)
            row['native_sample_cycle'] = cycle + offset
            rows.append(row)
    assert counts['all_writebacks'] > 0
    statuses = dict(Counter(row['status'] for row in rows))
    assert sum(statuses.values()) == len(rows)
    return dict(passed=True, point=folder.name, source=result['source'], counts=dict(counts),
                clock_alignment=clock, sampled_stalled_targets=len(rows), statuses=statuses, samples=rows,
                native_lifecycle_cohorts_passed=True,
                source_sha256={str(p): hashlib.sha256(p.read_bytes()).hexdigest()
                               for p in [receipt_path, database, Path(__file__), Path(__file__).with_name('tea_dip_clock_alignment.py')]},
                scope='Captured valid Stalled TEA samples only. ROB index/rank plus allocation-to-final '
                      'lifetime and unique representative generation/member/uop recover a load address. '
                      'Core samples are mapped to native owner timestamps using same-dump clock anchors. '
                      'Same-line active MSHRs are candidates; no end-to-end CPU request tag, data-return '
                      'timestamp, unique critical delay, CPU-cycle attribution or full sampling coverage '
                      'is established. Four-bank64B DefaultConfig only. Ambiguous, cancelled, missing '
                      'and boundary cases remain explicit; completed-owner duration is not load latency.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    data = analyze(args.folder, args.receipt)
    args.output.write_text(json.dumps(data, indent=2) + '\n')
    print(json.dumps(dict(passed=data['passed'], point=data['point'],
                         sampled_stalled_targets=data['sampled_stalled_targets'], statuses=data['statuses'])))


if __name__ == '__main__':
    main()
