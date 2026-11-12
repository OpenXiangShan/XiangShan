"""Decompose closed S2 cause bits using their same-attempt physical conditions."""
import argparse
from collections import Counter, defaultdict
import csv
import json
from pathlib import Path

from tea_dip_load_service_trace import analyze


def decompose(row):
    assert row['tlb_access_result'] in (0, 1, 2, 4)
    assert all(row[key] in (0, 1) for key in ['trouble_maker', 'special_request', 'unaligned_replay', 'read_read_conflict', 'read_write_conflict', 'readline_conflict'])
    trouble = row['trouble_maker']
    translation = bool(trouble and row['tlb_access_result'] == 4)
    special = bool(row['special_request'])
    physical = bool(trouble and row['need_dcache'])
    bank_mask = sum(int(physical and row[key]) << bit for bit, key in enumerate(
        ['read_read_conflict', 'read_write_conflict', 'readline_conflict']))
    unaligned = bool(trouble and row['unaligned_replay'])
    assert bool(row['cause_mask'] & (1 << 3)) == (translation or special), 'C_TM decomposition differs'
    assert bool(row['cause_mask'] & (1 << 8)) == bool(bank_mask or unaligned), 'C_BC decomposition differs'
    return bank_mask, unaligned, translation, special


def details(folder, receipt):
    folder = Path(folder)
    output = analyze(folder, receipt)
    proof = json.loads((folder / 'probe-build-proof.json').read_text())
    assert proof['binding']['cause_details_asserted'] and len(proof['binding']['detail_bindings']) == 3
    with (folder / 'load-service-probe.csv').open() as stream:
        rows = [{key: int(value) for key, value in row.items()} for row in csv.DictReader(stream)]
    for phase in output['phases']:
        profiles, partitions = defaultdict(Counter), Counter()
        for row in rows:
            if row['lane'] == 3 or not phase['begin_core_exclusive'] < row['core_cycle'] <= phase['end_core_inclusive']:
                continue
            bank, unaligned, translation, special = decompose(row)
            if not row['fire'] or row['instr_type'] != 1:
                continue
            profile = profiles[hex(row['pc'])]
            values = dict(attempts=1, bank_physical=int(bool(bank)), bank_unaligned=int(unaligned),
                          translation_miss=int(translation), special_request=int(special))
            profile.update(values)
            if bank or unaligned:
                partitions[f'BC_bank{bank}_unaligned{int(unaligned)}'] += 1
            if translation or special:
                partitions[f'TM_translation{int(translation)}_special{int(special)}'] += 1
        assert sum(v['attempts'] for v in profiles.values()) == sum(v['attempts'] for v in phase['scalar_PC_attempts'].values())
        assert sum(v for k, v in partitions.items() if k.startswith('BC_')) == sum(v['cause_C_BC'] for v in phase['scalar_PC_attempts'].values())
        assert sum(v for k, v in partitions.items() if k.startswith('TM_')) == sum(v['cause_C_TM'] for v in phase['scalar_PC_attempts'].values())
        phase['scalar_PC_physical_causes'] = dict(profiles)
        phase['scalar_cause_partitions'] = dict(partitions)
    output['scope'] += ' Physical RR/RW/RRL masks retain overlaps and reproduce C_BC; translation/special partitions reproduce C_TM. These are same-S2 conditions, not final service decisions, miss latency or CPU lost cycles.'
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    args.output.write_text(json.dumps(details(args.folder, args.receipt), indent=2) + '\n')


if __name__ == '__main__':
    main()
