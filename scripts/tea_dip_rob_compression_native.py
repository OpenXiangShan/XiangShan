"""Audit existing DefaultConfig ROB retirement/compression counts from a closed point.

waitAluCycle labels whole-entry pending by the first member's FU type. It cannot
identify the pending member of a compressed entry or measure ALU utilization.
"""
import argparse
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_wb_protocol_micro import write


SCALARS = ('clock_cycle', 'commitInstr', 'commitInstrFused', 'RobEntriesCommited',
           'commitCompressCntAll', 'compressSize', 'waitAluCycle', 'waitLoadCycle',
           'waitBranchCycle', 'waitStoreCycle', 'full', 'utilization')
TYPES = ('NORMAL', 'CC', 'CS', 'SC')


def summarize(counters):
    def metric(name):
        matches = {key: values for key, values in counters.items() if key.endswith('.rob.' + name)}
        values = next(iter(matches.values())) if len(matches) == 1 else []
        status = 'available' if len(values) == 1 else 'ambiguous' if matches else 'unavailable'
        value = values[0] if status == 'available' else None
        assert value is None or value >= 0, (name, value)
        return dict(status=status, value=value, native_matches=matches)

    metrics = {name: metric(name) for name in SCALARS}
    histogram = {str(i): metric('commitCompressCnt' + str(i)) for i in range(1, 9)}
    types = {name: metric('compressType_' + name) for name in TYPES}
    counts = [histogram[str(i)]['value'] for i in range(1, 9)]
    typed = [types[name]['value'] for name in TYPES]
    values = {name: item['value'] for name, item in metrics.items()}
    identities = {}

    def check(name, terms, expected):
        known = expected is not None and all(value is not None for value in terms)
        if known:
            assert sum(terms) == expected, (name, sum(terms), expected)
        identities[name] = 'matched' if known else 'unknown'

    check('size_histogram_entries', counts, values['RobEntriesCommited'])
    check('size_histogram_multi_instruction_entries', counts[1:], values['commitCompressCntAll'])
    check('size_histogram_compressed_instruction_sum',
          [i * value if value is not None else None for i, value in enumerate(counts, 1)][1:],
          values['compressSize'])
    check('pair_types_entries', typed, values['RobEntriesCommited'])
    entries = values['RobEntriesCommited']
    histogram_complete = entries and all(value is not None for value in counts)
    types_complete = entries and all(value is not None for value in typed)
    recorded = sum(i * count for i, count in enumerate(counts, 1)) if histogram_complete else None
    return dict(metrics=metrics, instructions_per_entry_histogram=histogram, pair_types=types,
                identities=identities,
                mean_recorded_instructions_per_entry=recorded / entries if histogram_complete else None,
                multi_instruction_entry_share=values['commitCompressCntAll'] / entries
                if entries and values['commitCompressCntAll'] is not None else None,
                paired_entry_share=sum(typed[1:]) / entries if types_complete else None,
                histogram_instruction_sum_minus_commitInstr=recorded - values['commitInstr']
                if recorded is not None and values['commitInstr'] is not None else None)


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    verify(folder, receipt['files'])
    blocks = read_blocks(folder / 'simulator_err.txt')
    assert len(blocks) == 2
    return dict(point=receipt['point'], source=receipt['source'],
                phases={phase: summarize(blocks[stamp])
                        for phase, stamp in zip(['warmup', 'ROI'], sorted(blocks))},
                receipt_sha256=digest(receipt_path), native_log_sha256=digest(folder / 'simulator_err.txt'),
                tool_sha256=digest(Path(__file__)),
                scope='DefaultConfig RenameWidth8. Histogram bins count committed ROB entries by '
                      'recorded instruction count, not per-cycle retirement width. CC/CS/SC count '
                      'paired entries; NORMAL can still encode several simple instructions. '
                      'commitInstr uses a registered count, so its boundary delta is preserved. '
                      'waitAluCycle is whole-entry pending with ALU first-member metadata; the pending '
                      'member, FU occupancy and dependency cause remain unknown. Retirement shares '
                      'do not describe blocked-entry shares. Head conditions are not added to TEA/DIP '
                      'or interpreted as exclusive CPU losses. Missing/ambiguous fields remain unknown. '
                      'Legacy per-FU stage latency counters are not consumed.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    write(args.output, analyze(args.folder.resolve(), args.receipt.resolve()))


if __name__ == '__main__':
    main()
