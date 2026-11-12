"""Read a member-pending partition of the existing ALU-labelled ROB head condition."""
import argparse
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_wb_protocol_micro import write


PENDING = ('former_only', 'latter_only', 'both_pending')
LATTER_TYPES = ('normal', 'branch', 'load', 'store')


def summarize(counters):
    def metric(name):
        matches = {key: values for key, values in counters.items() if key.endswith('.rob.' + name)}
        values = next(iter(matches.values())) if len(matches) == 1 else []
        status = 'available' if len(values) == 1 else 'ambiguous' if matches else 'unavailable'
        value = values[0] if status == 'available' else None
        assert value is None or value >= 0, (name, value)
        return dict(status=status, value=value, native_matches=matches)

    old = metric('waitAluCycle')
    pending = {name: metric('diag_rob_alu_head_' + name + '_cycles') for name in PENDING}
    types = {name: metric('diag_rob_alu_head_latter_only_' + name + '_cycles') for name in LATTER_TYPES}
    checks = {}
    for label, parts, target in [('old_ALU_condition', pending, old),
                                 ('latter_only_types', types, pending['latter_only'])]:
        values = [part['value'] for part in parts.values()]
        known = target['value'] is not None and all(value is not None for value in values)
        if known:
            assert sum(values) == target['value'], (label, sum(values), target['value'])
        checks[label] = 'matched' if known else 'unknown'
    return dict(legacy_ALU_label=old, pending_members=pending, latter_only_commit_types=types,
                identities=checks)


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    verify(folder, receipt['files'])
    blocks = read_blocks(folder / 'simulator_err.txt')
    assert len(blocks) in (1, 2)
    phases = ['ROI'] if len(blocks) == 1 else ['warmup', 'ROI']
    return dict(point=receipt['point'], source=receipt['source'],
                phases={phase: summarize(blocks[stamp]) for phase, stamp in zip(phases, sorted(blocks))},
                receipt_sha256=digest(receipt_path), native_log_sha256=digest(folder / 'simulator_err.txt'),
                tool_sha256=digest(Path(__file__)),
                scope='Partitions the legacy valid, not-writebacked, ALU-first-member entry condition; '
                      'it is not restricted to TEA Stalled cycles. Former/latter are packed ROB members, '
                      'not necessarily one architectural instruction each. Latter-only commit type comes '
                      'from its debug metadata. Neither the partition nor a load label proves cache, '
                      'operand, FU, or scheduler cause. Missing and ambiguous fields remain unknown. '
                      'No member conditions are added to CPU-state or DIP budgets.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    write(args.output, analyze(args.folder.resolve(), args.receipt.resolve()))


if __name__ == '__main__':
    main()
