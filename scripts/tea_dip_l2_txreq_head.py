"""Partition address-matched read requests at the canonical L2 TXREQ FIFO heads.

The existing ROB debug address is not an end-to-end load identity. Counts are
conditions during active egress cycles, not CPU lost cycles or saved CPI.
"""
import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import sqlite3

from native_perf import read_blocks
from tea_dip_l2_txreq_trace import analyze as audit_egress

# HasCHIOpcodes: ReadShared/Clean/Once/NoSnp/Unique and extended read variants.
READ_OPCODES = {0x01, 0x02, 0x03, 0x04, 0x07, 0x24, 0x25, 0x26}


def reconstruct(rows):
    counts = Counter()
    grants = []
    snapshots = {}
    for row in rows:
        cycle = row['NATIVECYCLE']
        # Read even at an inactive boundary: older schemas cannot answer this query.
        head_valid, address = row['EVENTS_HEADVALID'], row['EVENTS_HEADADDRESS']
        if row['NATIVEDUMP'] and row['LOGENABLE']:
            snapshots[cycle] = dict(conditions=dict(counts), matching_read_grants=list(grants))
        valid, ready = row['EVENTS_VALIDMASK'], row['EVENTS_READYMASK']
        matching = []
        if valid and head_valid:
            counts['active_egress_with_head_address'] += 1
            for port in range(4):
                if not valid & (1 << port):
                    continue
                prefix = 'EVENTS_REQUESTS_' + str(port) + '_'
                if row[prefix + 'OPCODE'] in READ_OPCODES and row[prefix + 'ADDRESS'] >> 6 == address >> 6:
                    matching.append(port)
        if matching:
            counts['head_line_read_queued'] += 1
            served = [port for port in matching if ready & (1 << port)]
            if not row['EVENTS_OUTREADY']:
                counts['head_line_read_downstream_not_ready'] += 1
            elif served:
                counts['head_line_read_served'] += 1
                port, = served
                prefix = 'EVENTS_REQUESTS_' + str(port) + '_'
                grants.append(dict(native_cycle=cycle, port=port, local_transaction=row[prefix + 'TRANSACTION'],
                                   opcode=row[prefix + 'OPCODE'], request_address=row[prefix + 'ADDRESS'],
                                   head_address=address))
            else:
                counts['head_line_read_other_port_served'] += 1
        elif valid and head_valid:
            counts['active_head_address_without_matching_read'] += 1
        if row['CLEAN']:
            counts.clear()
            grants.clear()
    for snapshot in snapshots.values():
        values = snapshot['conditions']
        value = lambda name: values.get(name, 0)
        assert value('head_line_read_queued') == sum(value(name) for name in [
            'head_line_read_served', 'head_line_read_other_port_served', 'head_line_read_downstream_not_ready'])
        assert value('active_egress_with_head_address') == value('head_line_read_queued') + value('active_head_address_without_matching_read')
        assert value('head_line_read_served') == len(snapshot['matching_read_grants'])
    return snapshots


def analyze(folder, receipt_path):
    folder = Path(folder)
    receipt = json.loads(Path(receipt_path).read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    database, native = folder / 'simulator.db', folder / 'simulator_err.txt'
    for path in [database, native]:
        assert hashlib.sha256(path.read_bytes()).hexdigest() == receipt['files'][path.name]['sha256']
    old = audit_egress(database, native)
    with sqlite3.connect(database.resolve().as_uri() + '?mode=ro', uri=True) as connection:
        connection.row_factory = sqlite3.Row
        snapshots = reconstruct(connection.execute('SELECT * FROM L2TxreqArbTrace ORDER BY NATIVECYCLE'))
    frames = read_blocks(native)
    assert snapshots.keys() == frames.keys()
    return dict(passed=True, point=receipt['point'], source=receipt['source'],
                old_egress_native_cohorts_passed=old['passed'],
                phases=[dict(native_dump_cycle=cycle, **data) for cycle, data in sorted(snapshots.items())],
                source_sha256={str(path): hashlib.sha256(path.read_bytes()).hexdigest()
                               for path in [database, native, Path(receipt_path), Path(__file__)]},
                scope='Only canonical bank0..3 FIFO-head read requests with the same64B line as the existing '
                      'ROB debug physical address. Head address validity does not prove load/not-WB/uop identity. '
                      'No active-TXREQ gaps are extrapolated. Local transaction IDs can be reused and are not '
                      'global CPU identities. Conditions overlap execution, are not saved cycles, and do not '
                      'identify prefetch ownership or post-issue data latency. Dump/clean cohorts follow the '
                      'existing native-calibrated egress histogram.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('folder', type=Path)
    parser.add_argument('receipt', type=Path)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt), indent=2) + '\n')


if __name__ == '__main__':
    main()
