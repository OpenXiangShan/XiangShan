"""Validate L2 SinkA eligibility masks and retain overlapping blocker combinations."""
import argparse
from collections import Counter
import csv
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_gsim_l2_arb_probe import CAUSES, FIELDS
from tea_dip_query_endpoint_evidence import digest, verify


def analyze(folder, receipt_path, requests=()):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder/'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and result['original_process_closed']
    assert result['difftest_passed'] and result['exitcode'] == 0 and result['process'] == receipt['process']
    assert result['source'] == receipt['source']
    verify(folder, receipt['files'])
    proof = json.loads((folder/'probe-build-proof.json').read_text())
    probe = json.loads((folder/'probe-receipt.json').read_text())
    assert probe['ready'] and probe['closure_receipt_sha256'] == digest(receipt_path)
    verify(folder, probe['files'])
    binding = proof['L2_arbitration_binding']
    assert binding['fields'] == FIELDS and binding['causes'] == CAUSES and binding['ready_and_s2_formula_asserted']
    def read(name):
        with (folder/name).open() as stream:
            return [{key: int(value) for key, value in row.items()} for row in csv.DictReader(stream)]
    rows, route = read('l2-request-arbitration.csv'), read('load-l2-demand-route.csv')
    boundaries = [row for row in rows if row['kind'] == 0]
    assert [(r['core_cycle'], r['clean'], r['native_dump']) for r in boundaries] == [
        (r['core_cycle'], r['clean'], r['native_dump']) for r in route if r['kind'] == 0]
    events = [row for row in rows if row['kind'] == 1]
    columns = ['core_cycle', 'bank', *FIELDS[:6]]
    assert [{name: row[name] for name in columns} for row in events] == [
        {name: row[name] for name in columns} for row in route if row['kind'] == 3], 'SinkA presentations differ from route'
    for row in events:
        assert row['cause_mask'] < 1 << len(CAUSES)
        assert bool(row['ready']) == (row['cause_mask'] == 0), 'Ready differs from complete eligibility mask'
    blocks = read_blocks(folder/'simulator_err.txt')
    dumps = [row['core_cycle'] for row in boundaries if row['native_dump']]
    assert len(dumps) == len(blocks)
    offsets = {native-core for native, core in zip(sorted(blocks), dumps)}
    offset, = offsets
    phases, marginals = [], []
    for (native_end, block), core_end in zip(sorted(blocks.items()), dumps):
        lo = max([r['core_cycle'] for r in boundaries if r['clean'] and r['core_cycle'] < core_end], default=-1)
        selected = [r for r in events if lo < r['core_cycle'] <= core_end]
        hist = Counter(r['cause_mask'] for r in selected)
        assert sum(hist.values()) == len(selected)
        for bank in range(4):
            counts = Counter(dict.fromkeys([
                'sinkA_req', 'sinkA_stall', 'sinkA_stall_by_mshrFull',
                'sinkA_stall_by_mainpipe_conflict', 'sinkA_stall_by_grantBuf',
                'sinkA_stall_by_dir', 'sinkA_stall_by_mshrTask',
                'sinkA_stall_by_mcp2', 'sinkA_stall_by_sinkB', 'sinkA_stall_by_sinkC'], 0))
            for row in selected:
                if row['bank'] != bank:
                    continue
                c = [bool(row['cause_mask'] & 1 << bit) for bit in range(len(CAUSES))]
                block_a = c[5] or c[6]
                basic = not any(c[:5])
                counts.update(sinkA_req=row['ready'], sinkA_stall=not row['ready'],
                              sinkA_stall_by_mshrFull=c[5], sinkA_stall_by_mainpipe_conflict=0,
                              sinkA_stall_by_grantBuf=not c[5] and c[6],
                              sinkA_stall_by_dir=not block_a and c[0],
                              sinkA_stall_by_mshrTask=not block_a and not c[0] and c[2],
                              sinkA_stall_by_mcp2=not block_a and not c[0] and not c[2] and c[3],
                              sinkA_stall_by_sinkB=basic and not block_a and c[7] and not c[8],
                              sinkA_stall_by_sinkC=basic and not block_a and c[8])
            for suffix, value in counts.items():
                expected, = [v for n, v in block.items() if n.endswith(f'.l2cache.slices_{bank}.reqArb.'+suffix)]
                assert expected == [value], (native_end, bank, suffix, value, expected)
                marginals.append(dict(native_dump=native_end, bank=bank, counter=suffix, value=value))
        phases.append(dict(begin_core_exclusive=lo, end_core_inclusive=core_end, native_dump=native_end,
                           SinkA_valid_bank_cycles=len(selected), actual_handshakes=hist[0],
                           blocked_bank_cycles=len(selected)-hist[0],
                           blocker_combinations={str(mask): count for mask, count in sorted(hist.items())},
                           blocker_marginals={name: sum(count for mask, count in hist.items() if mask & 1 << i)
                                              for i, name in enumerate(CAUSES)},
                           hint_only_bank_cycles=hist[1 << 4]))
    queried = []
    for bank, source, line, lo, hi in requests:
        assert 0 <= bank < 4 and lo < hi
        selected = [r for r in events if r['bank'] == bank and r['source_id'] == source
                    and r['line'] == line and lo <= r['core_cycle'] < hi]
        queried.append(dict(bank=bank, source_id=source, line=line, begin=lo, end=hi,
                            events=[dict(row, causes=[name for i, name in enumerate(CAUSES)
                                                     if row['cause_mask'] & 1 << i]) for row in selected]))
    return dict(passed=True, source=result['source'], receipt_sha256=digest(receipt_path),
                arbitration_CSV_sha256=digest(folder/'l2-request-arbitration.csv'),
                core_to_native_offset=offset, events=len(events), phases=phases,
                native_marginals=marginals, requests=queried,
                tool_sha256={Path(__file__).name: digest(Path(__file__))},
                scope='SinkA valid presentations and complete overlapping eligibility masks, not CPU lost cycles. '
                      'Native counters retain their actual predicates; their sum is not assumed a partition. '
                      'Target queries need the separately validated request generation/load ownership. '
                      'No unique ROI cause or recoverable performance is inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--request', nargs=5, type=lambda value: int(value, 0), action='append', default=[])
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt, args.request), indent=2)+'\n')


if __name__ == '__main__':
    main()
