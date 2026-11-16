"""Partition closed DTLB filter waiting into selection, arbitration and backpressure."""
import argparse
from collections import Counter
import csv
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_gsim_ptw_arb_probe import COLUMNS
from tea_dip_query_endpoint_evidence import digest, verify


def measure(rows, blocks):
    boundaries = [r for r in rows if r['kind'] == 0]
    events = [r for r in rows if r['kind'] == 1]
    assert len(boundaries) + len(events) == len(rows)
    assert all(r['clean'] or r['native_dump'] for r in boundaries)
    assert all(a['core_cycle'] < b['core_cycle'] for a, b in zip(events, events[1:]))
    dumps = [r['core_cycle'] for r in boundaries if r['native_dump']]
    assert len(dumps) == len(blocks) and len(set(dumps)) == len(dumps)
    offset, = {stamp - cycle for stamp, cycle in zip(sorted(blocks), dumps)}
    phases = []
    for (stamp, block), end in zip(sorted(blocks.items()), dumps):
        begin = max((r['core_cycle'] for r in boundaries if r['clean'] and r['core_cycle'] < end), default=-1)
        counts = {g: Counter(ptw_req_count=0, ptw_req_blocked=0) for g in ['load', 'store', 'prefetch']}
        counts['load'].update(entry_cycle=0, sent_entry_cycle=0)
        load, head = Counter(), Counter()
        for row in events:
            if not begin < row['core_cycle'] <= end:
                continue
            valid, ready, downstream, chosen = [row[k] for k in ['valid_mask', 'ready_mask', 'downstream_ready', 'chosen']]
            assert 0 <= valid < 8 and 0 <= ready < 8 and downstream in (0, 1)
            if valid:
                assert 0 <= chosen < 3 and valid & (1 << chosen)
            assert valid & ready == ((1 << chosen) if valid and downstream else 0)
            entries, sent, match = [row[k] for k in ['load_valid_mask', 'load_sent_mask', 'head_match_mask']]
            assert 0 <= entries < 65536 and 0 <= sent < 65536 and 0 <= match < 65536
            assert row['head_address_valid'] in (0, 1)
            expected_match = sum(1 << i for i in range(16) if row['head_address_valid'] and entries & (1 << i) and row['entry_vpn' + str(i)] == row['head_virtual_page'])
            assert match == expected_match, 'Head-page membership mismatch'
            unsent = entries & ~sent
            assert bool(valid & 1) == bool(unsent)
            if unsent:
                assert row['load_issue_index'] == unsent.bit_length() - 1
                assert row['load_request_vpn'] == row['entry_vpn' + str(row['load_issue_index'])]
            counts['load']['entry_cycle'] += entries.bit_count()
            counts['load']['sent_entry_cycle'] += (entries & sent).bit_count()
            for i, group in enumerate(counts):
                counts[group]['ptw_req_count'] += bool(valid & ready & (1 << i))
                counts[group]['ptw_req_blocked'] += bool(valid & (1 << i)) and not bool(ready & (1 << i))
            if valid & 1:
                status = ('downstream_backpressure' if not downstream else 'handshake' if chosen == 0
                          else 'lost_to_store' if chosen == 1 else 'lost_to_prefetch')
                load[status] += 1
                if match & ~sent:
                    selected_head = bool(match & (1 << row['load_issue_index']))
                    head[status if selected_head else 'local_selector_chose_other_page'] += 1
            if match and not match & ~sent:
                head['same_page_entries_already_sent'] += 1
        for group, metrics in counts.items():
            for key, value in metrics.items():
                values, = [v for name, v in block.items() if name.endswith('.dtlbRepeater.' + group + '_filter_' + group + '_entry.' + key)]
                expected, = values
                assert value == expected, (stamp, group, key, value, expected)
        assert sum(load.values()) == counts['load']['ptw_req_count'] + counts['load']['ptw_req_blocked']
        assert load['handshake'] == counts['load']['ptw_req_count']
        assert sum(load[k] for k in ['downstream_backpressure', 'lost_to_store', 'lost_to_prefetch']) == counts['load']['ptw_req_blocked']
        phases.append(dict(native_print_stamp=stamp, begin_core_exclusive=begin, end_core_inclusive=end,
                           native_margins={g: dict(c) for g, c in counts.items()}, load_valid_cycles=dict(load),
                           head_page_candidate_cycles=dict(head)))
    return dict(passed=True, core_to_native_offset=offset, recorded_cycles=len(events), phases=phases)


def analyze(folder, receipt_path):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and result['original_process_closed']
    assert result['exitcode'] == 0 and result['difftest_passed'] and all(result['sampler_flags'].values())
    assert result['process'] == receipt['process'] and result['source'] == receipt['source']
    verify(folder, receipt['files'])
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    assert proof['passed'] and proof['original_files_unchanged'] and proof['binary_sha256'] == result['binary_sha256']
    assert proof['binding']['kind'] == 'DTLB_filter_arbitration' and proof['binding']['columns'] == COLUMNS
    with (folder / 'ptw-arb-probe.csv').open() as stream:
        reader = csv.DictReader(stream)
        assert reader.fieldnames == ['core_cycle', 'clean', 'native_dump'] + COLUMNS
        rows = [{k: int(v) for k, v in raw.items()} for raw in reader]
    value = measure(rows, read_blocks(folder / 'simulator_err.txt'))
    value.update(point=folder.name, source=receipt['source'], receipt_sha256=digest(receipt_path),
                 trace_sha256=digest(folder / 'ptw-arb-probe.csv'), consumer_sha256=digest(Path(__file__)),
                 scope='Closed per-cycle ready/valid and load-filter occupancy validated against native phase margins. Head membership is independently reconstructed from raw entry VPNs. ROB debug head page is a candidate proxy, not unique dynamic request identity, CPU stall or recoverable CPI. Load group includes assigned prefetch clients. Execution neutrality is a separate bare/observer check.')
    return value


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    value = analyze(args.folder, args.receipt)
    args.output.write_text(json.dumps(value, indent=2) + '\n')
    print(json.dumps(value, indent=2))


if __name__ == '__main__':
    main()
