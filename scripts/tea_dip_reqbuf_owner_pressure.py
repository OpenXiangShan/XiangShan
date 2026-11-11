"""Read diagnostic prefetch pressure from one closed DefaultConfig guest, without inferring CPU savings."""
import argparse
import json
from pathlib import Path
import re

from native_perf import read_blocks, unique_counters
from tea_dip_query_endpoint_evidence import digest, verify

FIELDS = ('diag_reqbuf_pf_mshr_blocked_input_cycles',
          'diag_reqbuf_pf_owner_only_input_cycles',
          'diag_reqbuf_pf_owner_only_ready_entry_cycles')


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    verify(folder, receipt['files'])
    text = (folder / 'simulator_out.txt').read_text()
    assert 'Difftest enabled' in text and 'EXCEEDING CYCLE/INSTR LIMIT' in text
    for name, value in [('enableTea0', 1), ('enableDip0', 1),
                        ('teaSamplePeriod0', 1000), ('dipSamplePeriod0', 1000)]:
        assert re.search(r'^' + name + ' = ' + str(value) + '$', text, re.M)
    blocks = sorted(read_blocks(folder / 'simulator_err.txt').items())
    assert len(blocks) in (1, 2)
    labels = ['warmup', 'ROI'] if len(blocks) == 2 else ['coremark']
    phases = []
    for label, (stamp, block) in zip(labels, blocks):
        counters = unique_counters(block)

        def scalar(suffix):
            value, = [value for name, value in counters.items() if name.endswith(suffix)]
            return value

        cycles, retired = scalar('.rob.clock_cycle'), scalar('.rob.commitInstr')
        banks = {}
        for name, value in counters.items():
            match = re.search(r'\.l2cache\.slices_(\d+)\.reqBuf\.(.+)$', name)
            if match and match[2] in FIELDS:
                banks.setdefault(int(match[1]), {})[match[2]] = value
        assert banks and set(banks) == set(range(len(banks)))
        for bank, values in banks.items():
            assert set(values) == set(FIELDS)
            blocked, owner_input, owner_entries = [values[name] for name in FIELDS]
            assert 0 <= owner_input <= blocked <= cycles
            assert 0 <= owner_entries <= 4 * cycles  # DefaultConfig has four RequestBuffer entries per bank.
            values['prefetch_occ_condition_cycles'] = scalar(
                '.l2cache.slices_' + str(bank) + '.reqBuf.req_buffer_pf_block_occ')
        totals = {name: sum(row[name] for row in banks.values()) for name in FIELDS}
        phases.append(dict(phase=label, native_print_stamp=stamp, native_cycles=cycles,
                           native_retired=retired, banks=banks, totals=totals,
                           owner_input_share_of_mshr_blocked=(totals[FIELDS[1]] / totals[FIELDS[0]]
                                                             if totals[FIELDS[0]] else None),
                           per_retired={name: value / retired for name, value in totals.items()}))
    return dict(passed=True, point=folder.name, source=receipt['source'], phases=phases,
                source_sha256={str(path): digest(path) for path in [receipt_path, Path(__file__)]},
                scope='Diagnostic gate uses the same previous-cycle threshold, excluding pipeline tasks already '
                      'owning an MSHR. Input opportunities require an actually blocked valid prefetch and no '
                      'occupancy throttle; duplicate/merge requests already accepted are excluded by !ready. '
                      'Ready-entry counts are entry-cycles under this pressure difference, not selected issues '
                      'or complete chosenQ coverage. Cross-bank cycles and native conditions overlap. '
                      'Counts are local counterfactual opportunities, not transactions, CPU loss, demand-load '
                      'criticality or a demonstrated benefit from relaxing prefetch throttling.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    result = analyze(args.folder.resolve(), args.receipt.resolve())
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(dict(passed=result['passed'], point=result['point'], phases=result['phases']), indent=2))


if __name__ == '__main__':
    main()
