"""Partition actual scalar ROB-head replay entry-cycles in one closed guest."""
from pathlib import Path
import argparse
import json

from native_perf import read_blocks, unique_counters
from tea_dip_query_endpoint_evidence import digest, verify

STATES = ['total', 'waiting', 'scheduled', 'ready_selected', 'ready_bank_blocked', 'ready_arbitration']
NAMES = ['diag_lrqh_' + state + '_entry_cycles' for state in STATES]



def waiting_tag_bounds(total, waiting, active_cycles):
    """OR cycles lower-bound tagged entry-cycles within the same resident cohort."""
    assert 0 <= waiting <= total and 0 <= active_cycles <= total
    # Tagged entries are at least active_cycles, so at most total-active_cycles
    # resident entries can be untagged. OR=0 establishes that no entry was tagged.
    return dict(active_cycles=active_cycles,
                lower_entry_cycles=max(0, waiting + active_cycles - total),
                upper_entry_cycles=waiting if active_cycles else 0)


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    assert result['original_process_closed'] and result['exitcode'] == 0 and result['difftest_passed']
    assert all(result['sampler_flags'].values())
    assert result['source'] == receipt['source'] and result['process'] == receipt['process']
    verify(folder, receipt['files'])
    command = json.loads((folder / 'command.json').read_text())
    blocks = sorted(read_blocks(folder / 'simulator_err.txt').items())
    labels = ['warmup', 'ROI'] if '-W' in command else ['coremark']
    assert len(blocks) == len(labels)
    phases = []
    for label, (stamp, block) in zip(labels, blocks):
        counters = unique_counters(block)

        def get(suffix):
            value, = [value for name, value in counters.items() if name.endswith(suffix)]
            return value

        values = {state: get('.loadQueueReplay.' + name) for state, name in zip(STATES, NAMES)}
        assert all(value >= 0 for value in values.values())
        assert values['total'] == sum(values[state] for state in STATES[1:])
        retired = get('.rob.commitInstr')
        cycles = get('.rob.clock_cycle')
        assert retired > 0 and cycles > 0
        phases.append(dict(phase=label, native_print_stamp=stamp, cycles=cycles, retired=retired,
                           entry_cycles=values,
                           entry_cycles_per_retired={state: value / retired for state, value in values.items()},
                           fraction_of_head_entry_cycles={state: value / values['total'] for state, value in values.items()}
                           if values['total'] else None))
        prefix = '.loadQueueReplay.replay_'
        suffix = '_scalar_rob_head_blocked_cycles'
        causes = {name.split(prefix)[1].removesuffix(suffix): value for name, value in counters.items()
                  if prefix in name and name.endswith(suffix)}
        assert causes and all(0 <= value <= cycles for value in causes.values())
        phases[-1]['waiting_cause_bounds'] = {
            cause: waiting_tag_bounds(values['total'], values['waiting'], active)
            for cause, active in causes.items()
        }
    return dict(passed=True, point=receipt['point'], source=receipt['source'], phases=phases,
                source_sha256={str(path): digest(path) for path in [receipt_path, Path(__file__)]},
                scope='Actual allocated scalar entries matching the ROB-head pointer, excluding cancel and normal '
                      'release. The five states partition entry-cycles, not CPU cycles; several entries can share '
                      'one ROB instruction. Waiting means outside the current eligible mask; scheduled includes '
                      'selection through return. Neither identifies DRAM service. Eligible includes current fast-TLB '
                      'wakeups; selected uses the actual s0 choice and s1 acceptance. No unique load lifetime, '
                      'causal saved cycles or performance gain claim. Cause-active counters OR the same resident entries '
                      'per cycle. They lower-bound tagged entry-cycles; the waiting/tag intersection lower bound '
                      'is max(0, waiting + active_cycles - total). Bounds are not measured joint counts and do not '
                      'identify memory hierarchy service or latency.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    data = analyze(args.folder.resolve(), args.receipt.resolve())
    args.output.write_text(json.dumps(data, indent=2) + '\n')
    print(json.dumps(data, indent=2))


if __name__ == '__main__':
    main()
