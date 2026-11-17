"""Join selected/consumed/admitted prefetches with the existing exact FIFO replay."""
import argparse
from collections import Counter
import csv
import json
from pathlib import Path
from tea_dip_gsim_prefetch_admission_probe import ADMISSION_FIELDS
from tea_dip_prefetch_queue_trace import replay, payload
from tea_dip_query_endpoint_evidence import digest, verify
from native_perf import read_blocks


def admission(path, queue_path, policy):
    counts = [[0] * 4 for _ in range(4)]
    events, boundaries, finals = [], [], set()
    with Path(queue_path).open() as qstream, Path(path).open() as stream:
        qr = ({k: int(v) for k, v in row.items()} for row in csv.DictReader(qstream))
        qrows = (r for r in qr if r['kind'] == 1)
        current = next(qrows, None)
        reader = csv.DictReader(stream)
        assert reader.fieldnames == ['core_cycle', 'clean', 'native_dump', 'kind', 'bank', *ADMISSION_FIELDS]
        previous = None
        for raw in reader:
            row = {k: int(v) for k, v in raw.items()}
            bank, kind = row['bank'], row['kind']; key = row['core_cycle'], bank
            assert 0 <= bank < 4 and kind in [0, 1, 2] and bank not in finals
            if kind == 1:
                assert previous is None or key > previous, 'Admission events unordered or duplicated'
                previous = key
                while current is not None and (current['core_cycle'], current['bank']) < key:
                    current = next(qrows, None)
                assert current is not None and (current['core_cycle'], current['bank']) == key, 'Selected request lacks queue snapshot'
                assert row['enqueue'] == current['enqueue']
                assert tuple(row[n] for n in ['line', 'source_id', 'req_source', 'need_t']) == payload(current, 'input')
                full = current['head'] == current['tail'] and current['maybe_full']
                assert row['full'] == int(bool(full))
                ready = policy == 'overwrite' or not full or bool(current['pipe_ready'])
                assert row['enqueue'] == int(bool(ready)), 'Actual queue admission contradicts policy'
                assert row['source_fire'] == int(bool(ready if policy == 'backpressure' else True))
                assert row['enqueue'] <= row['source_fire']
                drop = row['source_fire'] and not row['enqueue']
                counts[bank][0] += 1; counts[bank][1] += row['source_fire']
                counts[bank][2] += row['enqueue']; counts[bank][3] += drop
                row['outcome'] = 'dropped_new' if drop else 'admitted' if row['enqueue'] else 'not_consumed_condition'
                events.append(row)
            elif kind == 0:
                boundaries.append(row)
            else:
                finals.add(bank)
            assert [row[n] for n in ADMISSION_FIELDS[-4:]] == counts[bank], 'Admission footer lost or corrupted records'
    assert finals == set(range(4)), 'Missing closed admission footer'
    return dict(events=events, counts=counts, boundaries=boundaries,
                outcomes=dict(Counter(r['outcome'] for r in events)))


def analyze(folder, receipt_path):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text()); result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and result['original_process_closed']
    assert result['exitcode'] == 0 and result['difftest_passed'] and all(result['sampler_flags'].values())
    verify(folder, receipt['files'])
    proof = json.loads((folder / 'probe-build-proof.json').read_text()); binding = proof['binding']
    assert proof['passed'] and proof['original_files_unchanged'] and binding['kind'] == 'prefetch_admission'
    assert proof['source'] == receipt['source'] == result['source'] and proof['binary_sha256'] == result['binary_sha256']
    policy = binding['queue']['policy']; assert policy in ['overwrite', 'backpressure', 'drop_new']
    fifo = replay(folder / 'prefetch-queue.csv')
    selected = admission(folder / 'prefetch-admission.csv', folder / 'prefetch-queue.csv', policy)
    assert [r[2] for r in selected['counts']] == [r[0] for r in fifo['counts']]
    if policy != 'overwrite': assert not fifo['outcomes'].get('overwritten', 0)
    if policy != 'drop_new': assert not selected['outcomes'].get('dropped_new', 0)
    native = read_blocks(folder / 'simulator_err.txt')
    dumps = sorted({r['core_cycle'] for r in selected['boundaries'] if r['native_dump']})
    assert len(dumps) == len(native)
    margins = []; previous = -1
    for (stamp, block), end in zip(sorted(native.items()), dumps):
        consumed = [r for r in selected['events'] if r['source_fire'] and previous < r['core_cycle'] <= end]
        actual = [r for r in fifo['records'] if previous < r['enqueue_cycle'] <= end]
        scalar = lambda suffix: next(v[0] for k, v in block.items() if k.endswith(suffix) and len(v) == 1)
        selectors = {s: scalar('.prefetcher.prefetch_req_select' + s) for s in ['L1', 'VBOP', 'PBOP', 'TP', 'NL', 'CDP']}
        sources = Counter(r['req_source'] for r in consumed)
        assert len(consumed) == sum(selectors.values()), ('Native source consumption mismatch', stamp)
        for engine, source in [('VBOP', 8), ('PBOP', 9), ('TP', 13), ('NL', 15), ('CDP', 16)]:
            assert sources[source] == selectors[engine], (engine, stamp)
        assert len(consumed) - sum(sources[s] for s in [8, 9, 13, 15, 16]) == selectors['L1']
        ports = []
        for bank in range(4):
            count = sum(r['bank'] == bank and previous < r.get('port_cycle', -1) <= end for r in fifo['records'])
            assert count == scalar(f'.slices_{bank}.sinkA.sinkA_prefetch_req')
            ports.append(count)
        dropped = [r for r in selected['events'] if r['outcome'] == 'dropped_new' and previous < r['core_cycle'] <= end]
        assert len(consumed) == len(actual) + len(dropped)
        margins.append(dict(native_stamp=stamp, source_cycle_end=end, selected_source_handshakes=len(consumed),
                            FIFO_admissions_including_flow=len(actual), dropped_new=len(dropped),
                            native_selectors=selectors, port_fires=ports))
        previous = end
    return dict(passed=True, point=folder.name, source=receipt['source'], policy=policy,
                fifo=fifo, admission=selected, native_phase_marginals=margins,
                tool_sha256={n: digest(Path(__file__).parent / n) for n in
                    ['tea_dip_prefetch_admission_trace.py', 'tea_dip_gsim_prefetch_admission_probe.py', 'tea_dip_gsim_prefetch_queue_probe.py', 'tea_dip_prefetch_queue_trace.py']},
                scope='All selected bank inputs, source consumption, FIFO ready/valid admission including flow, '
                      'actual overwritten-old and dropped-new identities. Unconsumed input-valid conditions can '
                      'repeat a held request or lose a Valid beat; not unique useful lines. Full warmup/ROI and '
                      'right-censored owners retained. No S3/useful-fill/critical-load causality or saved CPU cycles.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args(); assert not args.output.exists()
    value = analyze(args.folder, args.receipt)
    args.output.write_text(json.dumps(value, indent=2) + '\n')
    print(json.dumps(dict(passed=True, policy=value['policy'], outcomes=value['admission']['outcomes'])))


if __name__ == '__main__':
    main()
