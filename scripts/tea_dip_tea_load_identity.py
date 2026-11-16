"""Associate sampled TEA targets with carried scalar load WBs using ELF PC semantics.

The calibrated load uop PC names the instruction's last halfword; TEA uses
its architectural start. Preserve both. This does not infer a missing ROB
flag/former bit, retirement ownership for ambiguous WBs, or CPU cycle savings.
"""
import argparse
import csv
from collections import Counter, defaultdict
import json
from pathlib import Path
import re
import sqlite3
import subprocess

from tea_dip_load_writeback_trace import KEY, analyze as writeback, match_attempts
from tea_dip_query_endpoint_evidence import digest, endpoint


def normalize_pc(pc):
    return None if pc is None else pc & ((1 << 64) - 1)


def instructions(elf, objdump):
    text = subprocess.check_output([objdump, '-d', str(elf)], text=True)
    decoded = {}
    for line in text.splitlines():
        match = re.match(r'^\s*([0-9a-f]+):\s+([0-9a-f]{4}(?:[0-9a-f]{4})?)\s+(.+)$', line)
        if match:
            pc, encoding, asm = match.groups()
            size = len(encoding) // 2
            assert size == (4 if int(encoding, 16) & 3 == 3 else 2)
            pc = int(pc, 16)
            assert pc not in decoded
            decoded[pc] = dict(bytes=size, last_halfword_pc=pc + size - 2, assembly=asm.strip())
    assert decoded, 'No supported RISC-V instructions in ELF'
    return decoded


def analyze(folder, receipt_path, elf, pc_proof, objdump='riscv64-unknown-linux-gnu-objdump', *, pcs=None):
    folder, receipt_path, elf, pc_proof = map(lambda p: Path(p).resolve(),
                                             [folder, receipt_path, elf, pc_proof])
    decoded = instructions(elf, objdump)
    selected_pcs = None if pcs is None else set(pcs)
    carried_pcs = None if selected_pcs is None else {ins['last_halfword_pc']
        for pc, ins in decoded.items() if pc in selected_pcs}
    validated = writeback(folder, receipt_path, pcs=carried_pcs)
    build = json.loads((folder / 'probe-build-proof.json').read_text())
    source = json.loads(pc_proof.read_text())
    assert source['head'] == build['source'] == validated['source']
    required = {
        'src/main/scala/xiangshan/backend/Backend.scala':
            'source.bits.data.pc.get + (source.bits.ctrl.ftqOffset.get << instOffsetBits)',
        'src/main/scala/xiangshan/frontend/ibuffer/Bundles.scala':
            'cf.ftqOffset                                     := instrEndOffset',
        'src/main/scala/xiangshan/frontend/ifu/InstrCompact.scala':
            'Mux(rawIsRvc(i), twoFetchInstrOffset(i), twoFetchInstrOffset(i + 1))',
        'src/main/scala/xiangshan/backend/rename/Rename.scala':
            'members(i).bits.pc := io.in(i).bits.debug.get.pc',
    }
    for name, expression in required.items():
        assert expression in '\n'.join(row['text'] for row in source['source_files'][name]['lines'])
    for binding in build['binding']['writeback_bindings']:
        assert binding['fields'][6].endswith('s2__DOT__pipeOutBits__DOT__uop__DOT__pc')
    with (folder / 'load-writeback-probe.csv').open() as stream:
        rows = ({name: int(value) for name, value in raw.items()}
                for raw in csv.DictReader(stream))
        attempts, completed = match_attempts(row for row in rows
            if carried_pcs is None or row['pc'] in carried_pcs)
    by_member = defaultdict(list)
    for key, wb in completed.items():
        by_member[wb['rob_index'], wb['rank']].append((key, wb))
    by_end = {ins['last_halfword_pc']: (pc, ins) for pc, ins in decoded.items()}
    assert len(by_end) == len(decoded)
    wb_profiles = Counter((wb['pc'], by_end[wb['pc']][0] if wb['pc'] in by_end else None,
                           by_end[wb['pc']][1]['bytes'] if wb['pc'] in by_end else None)
                          for wb in completed.values())
    lo, hi = [frame['tea_cycle'] for frame in endpoint(folder)['frames']]
    finals, counts, matches, unknown = {}, Counter(), [], []
    with sqlite3.connect((folder / 'simulator.db').as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        for row in db.execute('SELECT * FROM TeaDelayedFinal_0 WHERE ISDRAIN=0'):
            target = tuple(row['MEMBER_TARGET_' + name] for name in ['ALLOCATIONCYCLE', 'ROBINDEX', 'RANK'])
            assert target not in finals, 'Duplicate final member identity'
            finals[target] = dict(row)
        samples = list(db.execute('SELECT * FROM Tea_0 WHERE STATE=1 AND CYCLE>=? AND CYCLE<? ORDER BY CYCLE,ID', (lo, hi)))
    for sample in samples:
        target = tuple(sample['TARGET_' + name] for name in ['ALLOCATIONCYCLE', 'ROBINDEX', 'RANK'])
        record = dict(sample_cycle=sample['CYCLE'], target=dict(zip(['allocation_cycle', 'rob_index', 'rank'], target)))
        final = finals.get(target)
        reason = None
        if not sample['TARGETVALID']:
            reason = 'invalid_target'
        elif final is None or final['CANCELLED']:
            reason = 'no_uncancelled_final'
        else:
            pc = normalize_pc(final['MEMBER_PC'])
            record.update(architectural_PC=pc, final_cycle=final['CYCLE'])
            instruction = decoded.get(pc)
            if selected_pcs is not None and pc not in selected_pcs:
                reason = 'target_PC_not_selected'
            elif instruction is None:
                reason = 'target_PC_not_in_ELF'
            else:
                record['instruction'] = instruction
                candidates = [(key, wb) for key, wb in by_member[target[1:]]
                              if target[0] <= wb['issue_cycle'] < wb['core_cycle'] <= final['CYCLE']]
                record['identity_time_candidates'] = len(candidates)
                exact = [(key, wb) for key, wb in candidates if normalize_pc(wb['pc']) == pc]
                counts['raw_PC_equal_candidates'] += len(exact)
                qualified = [(key, wb) for key, wb in candidates
                             if normalize_pc(wb['pc']) == instruction['last_halfword_pc']]
                if len(qualified) != 1:
                    reason = 'no_unique_PC_qualified_WB'
                    record['PC_qualified_candidates'] = len(qualified)
                else:
                    key, wb = qualified[0]
                    record.update(identity=dict(zip(KEY, key)), observed_load_PC=wb['pc'],
                                  first_S2=attempts[key][0]['core_cycle'], WB=wb['core_cycle'],
                                  virtual_address=wb['virtual_address'], physical_address=wb['physical_address'],
                                  unfinished_at_sample=wb['core_cycle'] > sample['CYCLE'],
                                  initial_issue_after_sample=wb['issue_cycle'] > sample['CYCLE'])
                    counts['matched_samples'] += 1
                    counts['matched_' + str(instruction['bytes']) + '_byte_instruction_samples'] += 1
                    counts['unfinished_at_sample'] += record['unfinished_at_sample']
                    matches.append(record)
        if reason:
            counts[reason] += 1
            unknown.append(dict(record, reason=reason))
    assert len(matches) + len(unknown) == len(samples)
    output = dict(passed=True, source=validated['source'], roi_begin=lo, roi_end=hi,
                Stalled_samples=len(samples), counts=dict(counts), matches=matches, unknown=unknown,
                validated_completed_scalar_keys=validated['completed_scalar_keys'],
                completed_WB_PC_profiles=[dict(observed_PC=raw, architectural_PC=pc, instruction_bytes=size, WBs=n)
                                          for (raw, pc, size), n in wb_profiles.most_common()],
                successful_WB_native_marginals=[p['successful_WB_native_marginals'] for p in validated['phases']],
                input_sha256={str(p): digest(p) for p in [receipt_path, elf, pc_proof,
                    folder / 'probe-build-proof.json', folder / 'probe-receipt.json',
                    folder / 'simulator.db', folder / 'load-writeback-probe.csv']},
                tool_sha256={Path(__file__).name: digest(Path(__file__))},
                scope='Sampled Stalled TEA targets with same-member allocation/index/rank and issue/WB/final time bounds. '
                      'Actual source binds load PC to FTQ instruction end; ELF length determines the last halfword. '
                      'Raw PC and architectural PC are retained. Unknown/ambiguous targets remain unmatched; '
                      'TEA target lacks flag/former, so PC/time qualification is not an independent full allocation identity. '
                      'Successful WB need not be unfinished at sample; initial issue may also follow the sample. '
                      'No load criticality, memory owner, exclusive CPU budget or recoverable performance is inferred.')
    if selected_pcs is not None:
        output['architectural_PCs'] = [hex(pc) for pc in sorted(selected_pcs)]
        output['scope'] += (' WB profiles/keys and identity-time candidate counts cover only selected architectural PCs; all TEA Stalled sources remain '
                            'and unselected uncancelled targets are explicit target_PC_not_selected. '
                            'Native marginals and per-event S2/WB validation still cover the full trace.')
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--elf', type=Path, required=True)
    parser.add_argument('--pc-proof', type=Path, required=True)
    parser.add_argument('--objdump', default='riscv64-unknown-linux-gnu-objdump')
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--pc', type=lambda value: int(value, 0), action='append',
                        help='Select architectural start PCs; ELF maps carried WB PCs and full native checks remain')
    args = parser.parse_args()
    assert not args.output.exists()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt, args.elf, args.pc_proof, args.objdump, pcs=args.pc), indent=2) + '\n')


if __name__ == '__main__':
    main()
