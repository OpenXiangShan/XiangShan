"""Locate sampled unfinished load addresses among actual L2 TXREQ FIFO heads.

A unique qualifying representative writeback inside the TEA lifetime recovers
an address retrospectively. Same-line egress is still not an end-to-end request
identity, a CPU lost-cycle budget or a predictor using information available at
that time. Missing/ambiguous/late data remain explicit.
"""
import argparse
from collections import Counter, defaultdict
import hashlib
import json
from pathlib import Path
import sqlite3

from tea_dip_clock_alignment import core_native_alignment
from tea_dip_load_mshr_association import target_writebacks
from tea_dip_l2_txreq_head import READ_OPCODES, analyze as audit_head
from tea_dip_query_endpoint_evidence import verify

WB_COLUMNS = ['ID', 'ROBIDX', 'WBTEARANK', 'SLOTTEARANK', 'WBROBFLAG', 'SLOTROBFLAG',
              'WBUOPIDX', 'SLOTUOPIDX', 'SLOTFUTYPE', 'DPADDR', 'ISSUETIME', 'WRITEBACKTIME', 'STAMP']


def egress_condition(address, row):
    if row is None or not row['EVENTS_VALIDMASK']:
        return 'no_active_egress_record', 'unobserved'
    relation = ('same_line' if row['EVENTS_HEADADDRESS'] >> 6 == address >> 6 else 'different_line') if row['EVENTS_HEADVALID'] else 'invalid_debug_address'
    matching = [port for port in range(4) if row['EVENTS_VALIDMASK'] & (1 << port)
                and row[f'EVENTS_REQUESTS_{port}_OPCODE'] in READ_OPCODES
                and row[f'EVENTS_REQUESTS_{port}_ADDRESS'] >> 6 == address >> 6]
    if not matching:
        return 'active_egress_no_matching_read', relation
    if not row['EVENTS_OUTREADY']:
        return 'matching_read_downstream_not_ready', relation
    if any(row['EVENTS_READYMASK'] & (1 << port) for port in matching):
        return 'matching_read_served', relation
    return 'matching_read_other_port_served', relation


def resolve_sample(target, endings, writebacks, trace):
    cycle = target['CYCLE']
    row = dict(sample_cycle=cycle, allocation_cycle=target['TARGET_ALLOCATIONCYCLE'],
               rob_index=target['TARGET_ROBINDEX'], rank=target['TARGET_RANK'])
    if len(endings) != 1:
        return dict(row, status='missing_or_ambiguous_final')
    final, = endings
    row['pc'] = hex(final['MEMBER_PC'])
    if final['CANCELLED'] or final['ISDRAIN']:
        return dict(row, status='cancelled_or_drain_final')
    candidates = target_writebacks(target, final, writebacks)
    if len(candidates) != 1:
        return dict(row, status='missing_or_ambiguous_representative_writeback')
    wb, = candidates
    if wb['SLOTFUTYPE'] != 1 << 16:
        return dict(row, status='non_load_representative')
    if not wb['DPADDR']:
        return dict(row, status='zero_load_address')
    row.update(physical_address=wb['DPADDR'], writeback_id=wb['ID'],
               issue_cycle=wb['ISSUETIME'], writeback_cycle=wb['WRITEBACKTIME'])
    if not wb['ISSUETIME'] <= cycle < wb['WRITEBACKTIME']:
        return dict(row, status='outside_final_issue_writeback_interval')
    status, relation = egress_condition(wb['DPADDR'], trace)
    return dict(row, status=status, debug_head_address_relation=relation)


def analyze(folder, receipt_path):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
    assert all(result['sampler_flags'].values()) and result['source'] == receipt['source']
    assert result['process'] == receipt['process']
    verify(folder, receipt['files'])
    with sqlite3.connect((folder / 'simulator.db').as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        names = {r[0] for r in db.execute("SELECT name FROM sqlite_master WHERE type='table'")}
        if 'InstTable0' not in names:
            raise ValueError('InstTable0 was not captured; missing writebacks are not zero activity')
        columns = {r[1] for r in db.execute('PRAGMA table_info(InstTable0)')}
        if set(WB_COLUMNS) - columns:
            raise ValueError('InstTable0 lacks required identity fields: ' + ', '.join(sorted(set(WB_COLUMNS) - columns)))
        clock = core_native_alignment(db, folder / 'simulator_err.txt')
        offset = clock['core_to_native_offset']
        checked = audit_head(folder, receipt_path)
        end = checked['phases'][-1]['native_dump_cycle']
        begin = db.execute('SELECT max(NATIVECYCLE) FROM L2TxreqArbTrace WHERE CLEAN AND NATIVECYCLE<?', (end,)).fetchone()[0]
        begin = -1 if begin is None else begin
        targets = [dict(r) for r in db.execute('SELECT * FROM Tea_0 WHERE STATE=1 AND TARGETVALID AND CYCLE>? AND CYCLE<? ORDER BY CYCLE', (begin - offset, end - offset))]
        assert len({r['CYCLE'] for r in targets}) == len(targets)
        assert all(r['CYCLE'] == r['STAMP'] for r in targets)
        finals, writebacks = defaultdict(list), defaultdict(list)
        for item in db.execute('SELECT * FROM TeaDelayedFinal_0'):
            assert item['CYCLE'] == item['STAMP']
            finals[tuple(item['MEMBER_TARGET_' + field] for field in ['ALLOCATIONCYCLE', 'ROBINDEX', 'RANK'])].append(dict(item))
        total_wb = 0
        for item in db.execute('SELECT ' + ','.join(WB_COLUMNS) + ' FROM InstTable0 ORDER BY STAMP,ID'):
            assert item['WRITEBACKTIME'] == item['STAMP']
            writebacks[item['ROBIDX'], item['WBTEARANK']].append(dict(item))
            total_wb += 1
        assert total_wb > 0
        cycles, traces = {r['CYCLE'] for r in targets}, {}
        for item in db.execute('SELECT * FROM L2TxreqArbTrace ORDER BY NATIVECYCLE'):
            assert item['NATIVECYCLE'] == item['STAMP']
            if item['NATIVECYCLE'] - offset in cycles:
                assert item['NATIVECYCLE'] - offset not in traces
                traces[item['NATIVECYCLE'] - offset] = dict(item)
        rows = [resolve_sample(r, finals[tuple(r['TARGET_' + field] for field in ['ALLOCATIONCYCLE', 'ROBINDEX', 'RANK'])], writebacks[r['TARGET_ROBINDEX'], r['TARGET_RANK']], traces.get(r['CYCLE'])) for r in targets]
    for row in rows:
        row['native_sample_cycle'] = row['sample_cycle'] + offset
    statuses = dict(Counter(r['status'] for r in rows))
    assert sum(statuses.values()) == len(targets)
    relations = dict(Counter(r['debug_head_address_relation'] for r in rows if 'debug_head_address_relation' in r))
    paths = [receipt_path, folder / 'result.json', folder / 'simulator.db', folder / 'simulator_err.txt', Path(__file__), Path(__file__).with_name('tea_dip_clock_alignment.py'), Path(__file__).with_name('tea_dip_load_mshr_association.py'), Path(__file__).with_name('tea_dip_l2_txreq_head.py')]
    return dict(passed=True, point=receipt['point'], source=result['source'], native_begin_exclusive=begin,
                native_dump_exclusive=end, clock_alignment=clock, sampled_stalled_targets=len(targets), all_writebacks=total_wb,
                unique_load_address_samples=sum('physical_address' in r for r in rows), statuses=statuses,
                debug_head_address_relations=relations, samples=rows, source_sha256={str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in paths},
                scope='Last native-clean scoring interval; only valid Stalled TEA source samples. All subsequent finals are kept. Unique qualifying representative WB inside the allocation/final lifetime recovers a future address; final issue/WB bounds select the association cohort. Sample/final/WB use the core clock; egress uses the native endpoint clock. Same-dump anchors measure their reset-origin offset, which shifts only cross-domain joins and native ROI selection. Each table is separately checked against its own STAMP. Same64B-line bank0..3 reads remain candidates, not unique CPU-to-CHI identities, lost cycles or information known by the core at that sample. No active-egress gaps are extrapolated. TEA target selection does not prove a unique CPU-critical ROB head. Repeated samples may refer to the same uop; no sampling-based CPI estimate or performance acceptance follows.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    data = analyze(args.folder, args.receipt)
    args.output.write_text(json.dumps(data, indent=2) + '\n')
    print(json.dumps(dict(passed=True, point=data['point'], samples=data['sampled_stalled_targets'], statuses=data['statuses'])))


if __name__ == '__main__':
    main()
