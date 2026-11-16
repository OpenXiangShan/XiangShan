"""Audit closed backend-redirect events against native resolve-time phase margins."""
import argparse
from collections import Counter
import csv
import json
from pathlib import Path

from native_perf import read_blocks, unique_counters
from tea_dip_bpu_commit_trace import S1, S3, TYPES
from tea_dip_bpu_host_trace import annotate_pcs
from tea_dip_bpu_native_cohort import redirect_feedback
from tea_dip_gsim_bpu_redirect_probe import CAUSES, COLUMNS
from tea_dip_query_endpoint_evidence import digest, verify


def measure(rows, blocks):
    boundaries = [row for row in rows if row['kind'] == 0]
    events = [row for row in rows if row['kind'] == 1]
    assert len(boundaries) + len(events) == len(rows)
    assert all(row['clean'] or row['native_dump'] for row in boundaries)
    assert all(a['core_cycle'] < b['core_cycle'] for a, b in zip(events, events[1:]))
    dumps = [row['core_cycle'] for row in boundaries if row['native_dump']]
    assert len(dumps) == len(blocks) and len(set(dumps)) == len(dumps)
    offset, = {native - core for native, core in zip(sorted(blocks), dumps)}
    phases = []
    for (native, block), end in zip(sorted(blocks.items()), dumps):
        begin = max([row['core_cycle'] for row in boundaries
                     if row['clean'] and row['core_cycle'] < end], default=-1)
        expected = redirect_feedback(unique_counters(block))
        causes, stages, s1_sources, s3_sources, targets, joint = [Counter() for _ in range(6)]
        total = mispredict = 0
        for row in events:
            if not begin < row['core_cycle'] <= end:
                continue
            total += 1
            assert row['is_mispredict'] in [0, 1]
            mask = row['priority_cause_mask']
            assert 0 <= mask < 16 and (not mask or mask & (mask - 1) == 0)
            if not row['is_mispredict']:
                assert mask == 0
                continue
            mispredict += 1
            assert row['s2_override'] in [0, 1] and row['s3_override'] in [0, 1]
            assert row['is_rvc'] in [0, 1] and row['actual_taken'] in [0, 1]
            assert row['cfi_position'] == (((row['fetch_start_pc'] >> 1) & 15) + row['ftq_offset']) & 31
            cause = CAUSES[mask.bit_length() - 1] if mask else 'unclassified'
            stage = 'S3' if row['s3_override'] else 'S2' if row['s2_override'] else 'S1'
            source = S1[row['s1_source']] if stage == 'S1' else S3[row['s3_source']] if stage == 'S3' else 'unavailable'
            kind = TYPES.get(row['actual_type'], 'unclassified')
            causes[cause] += 1
            stages[stage] += 1
            if stage == 'S1':
                s1_sources[source] += 1
            elif stage == 'S3':
                s3_sources[source] += 1
            if cause == 'wrong_target':
                targets[kind] += 1
            joint[(row['fetch_start_pc'], row['ftq_offset'], row['cfi_position'], row['is_rvc'],
                   kind, row['actual_taken'], cause, stage, source)] += 1
        assert total == expected['redirect_valid_cycles'], ('redirect total', total, expected)
        assert mispredict == expected['mispredict_redirect_valid_cycles'], ('mispredict', mispredict, expected)
        assert total - mispredict == expected['other_redirect_valid_cycles']
        margins = dict(priority_causes=causes, selected_stages=stages, selected_S1_sources=s1_sources,
                       selected_S3_sources=s3_sources, wrong_target_types=targets)
        for name, observed in margins.items():
            assert observed == Counter(expected[name]), (name, observed, expected[name])
        assert sum(joint.values()) == mispredict
        phases.append(dict(native_print_stamp=native, begin_core_exclusive=begin, end_core_inclusive=end,
                           redirect_valid_cycles=total, mispredict_redirect_valid_cycles=mispredict,
                           native_margins=expected,
                           joint=[dict(fetch_start_PC=hex(k[0]), ftq_offset=k[1], cfi_position=k[2], raw_redirect_is_rvc=k[3],
                                       branch_type=k[4], actual_taken=k[5], priority_cause=k[6], selected_stage=k[7],
                                       selected_source=k[8], valid_cycles=count) for k, count in sorted(joint.items())]))
    return dict(passed=True, core_to_native_offset=offset, recorded_redirect_valid_cycles=len(events), phases=phases)


def annotate_redirect_pcs(phases, elf):
    """Resolve final-halfword PCs with the ELF; the old redirect RVC field is not authoritative."""
    projected = []
    for phase in phases:
        joint = []
        for row in phase['joint']:
            start = int(row['fetch_start_PC'], 16)
            assert ((start >> 1) & 15) + row['ftq_offset'] < 32, 'Redirect CFI outside aligned fetch block'
            joint.append(dict(fetch_pc_halfword=hex(start >> 1), actual_position=row['cfi_position'],
                              branch_type=row['branch_type'], entries=row['valid_cycles']))
        projected.append(dict(native_print_stamp=phase['native_print_stamp'],
                              mispredict_entries=phase['mispredict_redirect_valid_cycles'], joint=joint))
    evidence = annotate_pcs(projected, elf)
    raw_rvc_agreement = []
    for phase, decoded in zip(phases, projected):
        counts = Counter()
        for row, instruction in zip(phase['joint'], decoded['joint']):
            row['ELF_status'] = instruction['ELF_status']
            if row['ELF_status'] == 'not_in_ELF':
                continue
            expected_end = int(row['fetch_start_PC'], 16) + 2 * row['ftq_offset']
            actual_end = int(instruction['architectural_PC'], 16) + len(instruction['opcode']) // 2 - 2
            assert actual_end == expected_end, ('Redirect final-halfword mismatch', row, instruction)
            agrees = (len(instruction['opcode']) == 4) == bool(row['raw_redirect_is_rvc'])
            counts['agrees' if agrees else 'disagrees'] += row['valid_cycles']
            row.update({key: instruction[key] for key in ['architectural_PC', 'opcode', 'assembly']})
        raw_rvc_agreement.append(dict(native_print_stamp=phase['native_print_stamp'], **counts))
    evidence['raw_redirect_isRVC_vs_ELF'] = raw_rvc_agreement
    evidence['scope'] = ('Matched ELF, source32-byte half alignment and final-halfword position; redirect offset checked '
                         'separately. The old redirect isRVC field is raw metadata, not an instruction-length authority. '
                         'Coverage counts qualified mispredict redirect cycles. Unmapped addresses '
                         'remain unknown; no unique-branch identity, prediction accuracy or recovery cost is inferred.')
    return evidence


def analyze(folder, receipt_path, *, elf=None, elf_sha256=None):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and result['original_process_closed']
    assert result['exitcode'] == 0 and result['difftest_passed'] and all(result['sampler_flags'].values())
    assert result['process'] == receipt['process'] and result['source'] == receipt['source']
    verify(folder, receipt['files'])
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    assert proof['passed'] and proof['original_files_unchanged'] and proof['binary_sha256'] == result['binary_sha256']
    assert proof['binding']['redirect_binding']['columns'] == COLUMNS
    with (folder / 'bpu-redirect-probe.csv').open() as stream:
        reader = csv.DictReader(stream)
        assert reader.fieldnames == ['core_cycle', 'clean', 'native_dump'] + COLUMNS
        rows = [{key: int(value) for key, value in row.items()} for row in reader]
    value = measure(rows, read_blocks(folder / 'simulator_err.txt'))
    if elf is not None:
        assert digest(elf) == elf_sha256, 'ELF identity mismatch'
        value['ELF'] = annotate_redirect_pcs(value['phases'], elf)
    value.update(point=folder.name, source=receipt['source'], receipt_sha256=digest(receipt_path),
                 consumer_sha256=digest(Path(__file__)),
                 scope='Closed backendRedirect.valid cycles with same-event priority cause and selected stage/source. '
                       'Direction precedes position, attribute, then target. S2 provider remains unavailable. '
                       'Not unique dynamic branches or commit-time entries; FTQ pointers are not lifetime identities. '
                       'Separate margins do not establish per-provider accuracy or recovery duration. '
                       'Execution neutrality is a separate bare/observer check.')
    return value


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--elf', type=Path)
    parser.add_argument('--elf-sha256')
    args = parser.parse_args()
    assert not args.output.exists()
    value = analyze(args.folder, args.receipt, elf=args.elf, elf_sha256=args.elf_sha256)
    args.output.write_text(json.dumps(value, indent=2) + '\n')
    print(json.dumps(dict(passed=True, phase_redirect_valid_cycles=[row['redirect_valid_cycles'] for row in value['phases']])))


if __name__ == '__main__':
    main()
