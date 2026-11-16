"""Audit FTQ host-probe joint metadata against complete native phase margins."""
import argparse
from collections import Counter
import csv
import json
from pathlib import Path
import re
import subprocess

from native_perf import read_blocks, unique_counters
from tea_dip_bpu_commit_trace import S1, S3, TYPES
from tea_dip_bpu_native_cohort import FTQ, cohorts
from tea_dip_gsim_bpu_probe import COLUMNS, REASONS
from tea_dip_query_endpoint_evidence import digest, verify


def measure(rows, blocks):
    boundaries = [r for r in rows if r['kind'] == 0]
    events = [r for r in rows if r['kind'] == 1]
    assert len(boundaries) + len(events) == len(rows)
    assert all(r['clean'] or r['native_dump'] for r in boundaries)
    assert all(a['core_cycle'] < b['core_cycle'] for a, b in zip(events, events[1:]))
    dumps = [r['core_cycle'] for r in boundaries if r['native_dump']]
    assert len(dumps) == len(blocks) and len(set(dumps)) == len(dumps)
    offset, = {native - core for native, core in zip(sorted(blocks), dumps)}
    phases = []
    for (native, block), end in zip(sorted(blocks.items()), dumps):
        begin = max([r['core_cycle'] for r in boundaries if r['clean'] and r['core_cycle'] < end], default=-1)
        counters = unique_counters(block)
        expected = cohorts(counters)
        branches, types = Counter(), Counter()
        margins = {key: Counter() for key in ['reason', 's1_source', 's3_source', 'no_override', 'conditional_reason']}
        joint = Counter()
        count = entries = 0
        for row in events:
            if not begin < row['core_cycle'] <= end:
                continue
            entries += 1
            assert row['cfi_mask'].bit_count() == sum(row[k] for k in ['conditional', 'direct', 'indirect'])
            branches.update({k: row[k] for k in expected['branch_types']})
            if not row['mispredict']:
                assert row['reason_mask'] == 0
                continue
            mask = row['reason_mask']
            assert mask and mask & (mask - 1) == 0 and mask < 32
            reason = REASONS[mask.bit_length() - 1]
            s1, s3 = S1[row['s1_source']], S3[row['s3_source']]
            kind = TYPES[row['actual_type']]
            types[kind] += 1
            types['call'] += row['actual_ras'] == 2
            types['ret'] += row['actual_ras'] == 1
            types['indirect_retcall'] += kind == 'indirect' and row['actual_ras'] == 3
            for name, label in [('reason', reason), ('s1_source', s1), ('s3_source', s3)]:
                margins[name][label] += 1
            if kind == 'conditional':
                margins['conditional_reason'][reason] += 1
            if not (row['s2_override'] or row['s3_override']):
                margins['no_override'][s1] += 1
            stage = 's3' if row['s3_override'] else 's2' if row['s2_override'] else 's1'

            def relation(stage):
                predicted, actual = row[stage + '_position'], row['actual_position']
                if predicted != actual:
                    return 'position_before' if predicted < actual else 'position_after'
                return 'direction_diff' if row[stage + '_taken'] != row['actual_taken'] else 'position_and_direction_equal'

            selected_source = s1 if stage == 's1' else s3 if stage == 's3' else 'unavailable'
            joint[row['fetch_pc_halfword'], row['actual_position'], kind, reason, stage, relation(stage), relation('s1'), selected_source] += 1
            count += 1
        assert entries == counters[FTQ + 'total_commits'], ('FTQ commits', entries, counters[FTQ + 'total_commits'])
        assert count == expected['committed_mispredict_entries'], ('mispredict total', count, expected)
        assert branches == Counter(expected['branch_types']), ('branch types', branches, expected['branch_types'])
        assert sum(branches[k] for k in ['conditional', 'direct', 'indirect']) == expected['committed_branches']
        assert types == Counter(expected['mispredict_types']), ('mispredict types', types, expected['mispredict_types'])
        for key in ['reason', 's1_source', 's3_source']:
            assert margins[key] == Counter(expected['partitions'][key]), (key, margins[key], expected['partitions'][key])
        assert margins['no_override'] == Counter(expected['no_later_override_by_s1_source'])
        assert margins['conditional_reason'] == Counter(expected['conditional_mispredict_reasons'])
        assert sum(joint.values()) == count
        phases.append(dict(native_print_stamp=native, begin_core_exclusive=begin, end_core_inclusive=end,
                           committed_entries=entries, mispredict_entries=count, native_margins=expected,
                           joint=[dict(fetch_pc_halfword=hex(k[0]), actual_position=k[1], branch_type=k[2],
                                       reason=k[3], selected_stage=k[4], selected_position_direction=k[5],
                                       s1_position_direction=k[6], selected_source=k[7], entries=v) for k, v in sorted(joint.items())]))
    return dict(passed=True, core_to_native_offset=offset, recorded_entries=len(events), phases=phases)



def annotate_pcs(phases, elf):
    """Old DefaultConfig: 32-byte half alignment, RVC halfword units, final-halfword CFI positions."""
    targets = {((int(row['fetch_pc_halfword'], 16) << 1) & ~31) + (row['actual_position'] << 1)
               for phase in phases for row in phase['joint']}
    instructions = {}
    with subprocess.Popen(['riscv64-linux-gnu-objdump', '-d', '-M', 'no-aliases', str(elf)],
                          stdout=subprocess.PIPE, text=True) as process:
        for line in process.stdout:
            match = re.match(r'^\s*([0-9a-f]+):\s+([0-9a-f]{4}|[0-9a-f]{8})\s+(.+)$', line)
            if not match:
                continue
            pc, opcode, assembly = int(match[1], 16), match[2], match[3]
            end = pc + len(opcode) // 2 - 2
            if end not in targets:
                continue
            mnemonic = assembly.split()[0]
            kind = ('conditional' if mnemonic in ['beq', 'bne', 'blt', 'bge', 'bltu', 'bgeu', 'c.beqz', 'c.bnez']
                    else 'direct' if mnemonic in ['jal', 'c.j']
                    else 'indirect' if mnemonic in ['jalr', 'c.jr', 'c.jalr'] else 'other')
            instructions[end] = dict(architectural_PC=hex(pc), opcode=opcode, assembly=assembly, branch_type=kind)
        assert process.wait() == 0
    coverage = []
    for phase in phases:
        counts = Counter()
        for row in phase['joint']:
            end = ((int(row['fetch_pc_halfword'], 16) << 1) & ~31) + (row['actual_position'] << 1)
            instruction = instructions.get(end)
            status = 'not_in_ELF' if instruction is None else 'matched_type'
            if instruction is not None:
                assert instruction['branch_type'] == row['branch_type'], ('ELF CFI type mismatch', row, instruction)
                row.update({k: instruction[k] for k in ['architectural_PC', 'opcode', 'assembly']})
            row['ELF_status'] = status
            counts[status] += row['entries']
        assert sum(counts.values()) == phase['mispredict_entries']
        coverage.append(dict(native_print_stamp=phase['native_print_stamp'], **counts))
    return dict(file=str(elf), sha256=digest(elf), half_alignment_bytes=32, instruction_unit_bytes=2,
                phase_coverage=coverage,
                scope='Matched ELF and source-specific final-halfword CFI mapping. Unmapped addresses remain unknown. '
                      'Instruction type is checked for every mapped event; no target, prediction accuracy or recovery cost is inferred.')


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
    assert proof['binding']['kind'] == 'FTQ_commit_metadata' and proof['binding']['columns'] == COLUMNS
    with (folder / 'bpu-commit-probe.csv').open() as stream:
        reader = csv.DictReader(stream)
        assert reader.fieldnames == ['core_cycle', 'clean', 'native_dump'] + COLUMNS
        rows = [{k: int(v) for k, v in raw.items()} for raw in reader]
    value = measure(rows, read_blocks(folder / 'simulator_err.txt'))
    if elf is not None:
        assert digest(elf) == elf_sha256, 'ELF identity mismatch'
        value['ELF'] = annotate_pcs(value['phases'], elf)
    value.update(point=folder.name, source=receipt['source'], receipt_sha256=digest(receipt_path),
                 consumer_sha256=digest(Path(__file__)),
                 scope='Closed FTQ commit capture with native branch, mispredict, type, reason, source and override margins. '
                       'S2 selected-source identity is unavailable in these old models. Position/direction equality does not establish target correctness. GuardedPc.addr remains a raw '
                       'halfword address with guard bit. Optional matched-ELF annotation resolves instruction PCs and preserves unknowns. Entries and reason '
                       'metadata are not per-branch accuracy, resolution latency, saved cycles or unique CPI causality. '
                       'Execution neutrality is a separate bare/observer check.')
    return value


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--elf', type=Path, help='Matching old DefaultConfig program ELF for CFI PC annotation')
    parser.add_argument('--elf-sha256', help='Expected identity from the checkpoint/profile')
    args = parser.parse_args()
    assert not args.output.exists()
    value = analyze(args.folder, args.receipt, elf=args.elf, elf_sha256=args.elf_sha256)
    args.output.write_text(json.dumps(value, indent=2) + '\n')
    print(json.dumps(dict(passed=True, phase_mispredict_entries=[r['mispredict_entries'] for r in value['phases']])))


if __name__ == '__main__':
    main()
