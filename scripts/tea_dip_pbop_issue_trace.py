"""Replay accepted PBOP training through its S1 register and one-entry flow queue.

UInt7 offsets and int8_t-backed SInt7 fields are decoded according to their actual
producer types. Raw captures remain unchanged, including negative host encodings.
"""
import argparse
from collections import Counter, deque
import csv
from itertools import groupby
import json
from pathlib import Path
import re

from native_perf import read_blocks
from tea_dip_gsim_pbop_issue_probe import FIELDS
from tea_dip_query_endpoint_evidence import digest, verify


def signed_offset(value):
    assert 0 <= value < 128, 'PBOP offset exceeds its actual seven bits'
    return value - 128 if value & 64 else value


def host_signed_bits(value):
    """GSIM stores SInt7 in an int8_t-backed public register."""
    assert 0 <= value < 256, 'PBOP signed host field exceeds one byte'
    signed = value - 256 if value & 128 else value
    assert -64 <= signed < 64, 'PBOP host sign extension exceeds SInt7'
    return signed & 127


def replay(path):
    records, phases, boundaries = [], [], []
    pending, queue = None, deque()
    counts = Counter()
    previous, final = None, False
    payload = ['target_line', 'issue_offset', 'source_id', 'need_t']
    count_names = ['train', 'enq', 'pop', 'emit', 'phase']
    with Path(path).open() as stream:
        reader = csv.DictReader(stream)
        assert reader.fieldnames == ['core_cycle', 'clean', 'native_dump', 'kind', *FIELDS]
        rows = ({k: int(v) for k, v in row.items()} for row in reader)
        for cycle, group in groupby(rows, lambda row: row['core_cycle']):
            assert previous is None or cycle > previous, 'PBOP cycles are not ordered'
            previous = cycle
            rows_this_cycle = list(group)
            for row in rows_this_cycle:
                if row['kind'] == 1:
                    row['student_offset'] = host_signed_bits(row['student_offset'])
                elif row['kind'] in [2, 3]:
                    row['issue_offset'] = host_signed_bits(row['issue_offset'])
            batch = {row['kind']: row for row in rows_this_cycle}
            assert len(batch) == len(rows_this_cycle), 'Duplicate PBOP event kind in one cycle'
            assert all(0 <= kind <= 5 for kind in batch)
            # An old S1 entry enters the queue before this cycle's new training.
            if 2 in batch:
                row = batch[2]
                assert pending is not None, 'Enqueue lacks its accepted eligible training'
                assert pending['core_cycle'] < cycle, 'S1 generation did not cross a register'
                assert all(row[k] == pending[k] for k in payload), 'PBOP S1 payload differs from its training'
                assert not queue, 'PBOP non-pipelined queue accepted while full'
                pending['enqueue_cycle'] = cycle
                queue.append(pending)
                pending = None
                counts['enq'] += 1
            if 3 in batch:
                row = batch[3]
                assert queue, 'PBOP exit lacks its observed queue owner'
                owner = queue.popleft()
                assert all(row[k] == owner[k] for k in payload), 'PBOP queue exit payload changed'
                assert row['emitted'] in [0, 1] and row['enable'] in [0, 1] and 0 <= row['degree'] < 4
                assert row['emitted'] == int(bool(row['enable'] and row['degree'])), 'PBOP output gate mismatch'
                owner.update(exit_cycle=cycle, exit_enable=row['enable'], exit_degree=row['degree'],
                             outcome='emitted' if row['emitted'] else 'discarded_at_output_gate')
                counts['pop'] += 1
                counts['emit'] += row['emitted']
            if 1 in batch:
                row = batch[1]
                assert pending is None, 'New training overwrote an unconsumed S1 owner'
                assert all(row[k] in [0, 1] for k in ['need_t', 'enable', 'issue_enable', 'student_selected',
                                                   'teacher_disabled', 'cross_page'])
                assert 0 <= row['degree'] < 4 and 0 <= row['source_id'] < 256
                assert row['base_line'] & 63 == row['target_line'] & 63 == 0
                expected = (row['base_line'] + 64 * signed_offset(row['issue_offset'])) & ((1 << 48) - 1)
                assert row['target_line'] == expected, 'PBOP target differs from the observed signed offset'
                assert row['cross_page'] == int(row['base_line'] >> 12 != row['target_line'] >> 12)
                selected = row['student_offset'] if row['student_selected'] else row['teacher_offset']
                assert row['issue_offset'] == selected, 'PBOP selected offset/source mismatch'
                signed_offset(row['student_offset'])
                signed_offset(row['teacher_offset'])
                record = {k: row[k] for k in ['core_cycle', *FIELDS[:13]]}
                record['index'] = len(records)
                record['suppression'] = [name for name, suppressed in
                                         [('external_disabled', not row['enable']),
                                          ('cross_page', row['cross_page']),
                                          ('issue_disabled', not row['issue_enable'])] if suppressed]
                record['outcome'] = 'generation_suppressed' if record['suppression'] else 'pending_S1'
                records.append(record)
                if not record['suppression']:
                    pending = record
                counts['train'] += 1
            if 4 in batch:
                row = batch[4]
                signed_offset(row['teacher_offset'])
                assert row['issue_enable'] in [0, 1]
                phases.append(dict(core_cycle=cycle, teacher_offset=row['teacher_offset'],
                                   teacher_enabled=row['issue_enable']))
                counts['phase'] += 1
            for kind in [0, 5]:
                if kind not in batch:
                    continue
                row = batch[kind]
                for name in count_names:
                    assert row[name + '_count'] == counts[name], ('PBOP observer count mismatch', cycle, name)
                if kind == 0:
                    boundaries.append(row)
                else:
                    assert not final, 'Duplicate PBOP final margins'
                    final = True
    assert final, 'Missing final PBOP observer margins'
    assert len(queue) <= 1
    for record in queue:
        record['outcome'] = 'pending_skid_queue'
    outcomes = Counter(r['outcome'] for r in records)
    assert sum(outcomes.values()) == counts['train']
    return dict(passed=True, counts=dict(counts), outcome_counts=dict(outcomes),
                right_censored_S1=int(pending is not None), right_censored_queue=len(queue),
                records=records, phases=phases, boundaries=boundaries,
                scope='Same-run accepted PBOP train to S1/skid exit. Flow-through is replayed; '
                      'gate-discarded exits and right-censored pending requests remain explicit. '
                      'Source_id is the carried request source ID, not MemReqSource. Emission does not '
                      'prove L2 S3 arrival, useful data, retirement, or exclusive CPU savings.')


def analyze(folder, receipt_path):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and result['original_process_closed']
    assert result['exitcode'] == 0 and result['difftest_passed'] and all(result['sampler_flags'].values())
    assert receipt['source'] == result['source']
    verify(folder, receipt['files'])
    probe = json.loads((folder / 'probe-receipt.json').read_text())
    assert probe['closure_receipt_sha256'] == digest(receipt_path)
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    assert proof['passed'] and proof['original_files_unchanged']
    assert proof['binary_sha256'] == result['binary_sha256']
    assert proof['PBOP_train_issue_binding']['fields'] == FIELDS
    output = replay(folder / 'pbop-train-issue.csv')
    frames = read_blocks(folder / 'simulator_err.txt')
    dumps = sorted(row['core_cycle'] for row in output['boundaries'] if row['native_dump'])
    assert len(dumps) == len(frames)
    margins, previous = [], -1
    for (stamp, block), end in zip(sorted(frames.items()), dumps):
        trains = [r for r in output['records'] if previous < r['core_cycle'] <= end]
        emitted = [r for r in output['records'] if r['outcome'] == 'emitted' and previous < r['exit_cycle'] <= end]
        phases = [r for r in output['phases'] if previous < r['core_cycle'] <= end]
        expected = {'bop_train': len(trains), 'bop_req': len(emitted),
                    'bop_drop_for_cross_page': sum(r['cross_page'] for r in trains),
                    'bop_drop_for_external_disable': sum(not r['enable'] for r in trains),
                    'bop_drop_for_auto_disable': sum(r['enable'] and not r['issue_enable'] for r in trains),
                    'bop_student_takeover': sum(r['student_selected'] for r in trains)}
        for name, count in expected.items():
            value, = [v for k, v in block.items() if k.endswith('.prefetcher.pbop.' + name)]
            assert value == [count], ('PBOP native count mismatch', stamp, name, count, value)
        histograms = {}
        for prefix, observed in [('prefetch_sent_issue_offset', Counter(signed_offset(r['issue_offset']) for r in emitted)),
                                 ('scoreTable.teacher_best_offset', Counter(signed_offset(r['teacher_offset']) for r in phases))]:
            native = {}
            for name, value in block.items():
                suffix = name.split('.prefetcher.pbop.' + prefix + '_')
                if len(suffix) != 2:
                    continue
                match = re.fullmatch(r'(pos|neg)_(\d+)', suffix[1])
                assert match
                offset = int(match[2]) * (-1 if match[1] == 'neg' else 1)
                count, = value
                assert observed[offset] == count, ('PBOP native offset mismatch', stamp, prefix, offset)
                native[offset] = count
            assert native and set(observed) <= set(native), 'PBOP observed unsupported offset'
            assert sum(native.values()) == sum(observed.values()), 'PBOP offset totals differ'
            histograms[prefix] = native
        margins.append(dict(native_stamp=stamp, source_cycle_end=end, counts=expected, offset_histograms=histograms))
        previous = end
    output.update(source=receipt['source'], native_phase_marginals=margins,
                  receipt_sha256=digest(receipt_path), trace_sha256=digest(folder / 'pbop-train-issue.csv'),
                  tool_sha256=digest(Path(__file__)))
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt), indent=2) + '\n')


if __name__ == '__main__':
    main()
