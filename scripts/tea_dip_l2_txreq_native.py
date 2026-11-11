"""Recover bounded L2-egress backpressure from existing DefaultConfig ArbPerf counters.

This requires the six-input, work-conserving TwoLevelRRArbiter. Histogram
registers print before the current sample; XSPerfAccumulate prints next_counter.
The accepted count therefore includes up to one extra handshake. Keep that
endpoint uncertainty instead of reporting an exact subtraction or clamping it
silently. These interface cycles are not CPU loss or a credit-specific cause.
"""
import argparse
import hashlib
import json
from pathlib import Path

from native_perf import read_blocks, unique_counters


def summarize(values, retired):
    roots = [name.removesuffix('ArbConflict_sampled') for name in values
             if name.endswith('.l2cache.txreq_arbArbConflict_sampled')]
    if not roots:
        return dict(status='unavailable', reason='No unique L2-egress ArbPerf counters')
    prefix, = roots
    multiplicity = {i: values[f'{prefix}ArbConflict_{i}_{i + 1}'] for i in range(1, 7)}
    active = values[prefix + 'ArbConflict_sampled']
    offered = values[prefix + 'ArbConflict_sum']
    assert values[prefix + 'ArbConflict_underflow'] == values[prefix + 'ArbConflict_overflow'] == 0
    assert sum(multiplicity.values()) == active
    assert sum(i * count for i, count in multiplicity.items()) == offered
    wait = {i: values[prefix + 'ArbWaitTime_' + str(i)] for i in range(13)}
    accepted = sum(wait.values())
    accepted_bounds = dict(lower=max(0, accepted - 1), upper=min(active, accepted))
    assert accepted_bounds['lower'] <= accepted_bounds['upper'], 'Inconsistent counter windows'
    blocked = dict(lower=active - accepted_bounds['upper'], upper=active - accepted_bounds['lower'])
    waiting = dict(lower=offered - accepted_bounds['upper'], upper=offered - accepted_bounds['lower'])
    return dict(status='available', prefix=prefix, active_output_cycles=active,
                valid_input_request_cycles=offered, printed_accepted_requests=accepted,
                histogram_window_accepted_request_bounds=accepted_bounds,
                output_not_ready_cycle_bounds=blocked,
                unaccepted_input_request_cycle_bounds=waiting,
                output_not_ready_cycles_per_retired_bounds={k: v / retired for k, v in blocked.items()},
                unaccepted_input_request_cycles_per_retired_bounds={k: v / retired for k, v in waiting.items()},
                excess_valid_input_request_cycles=offered - active,
                active_input_multiplicity=multiplicity,
                three_or_more_active_input_cycles=sum(count for i, count in multiplicity.items() if i >= 3),
                four_or_more_active_input_cycles=sum(count for i, count in multiplicity.items() if i >= 4),
                accepted_ready_wait_sum_lower_bound=sum(i * count for i, count in wait.items()),
                accepted_ready_wait_overflow_requests=wait[12])


def analyze(native_log):
    blocks = []
    for stamp, frame in sorted(read_blocks(native_log).items()):
        values = unique_counters(frame)
        retired, = [v for k, v in values.items() if k.endswith('.rob.commitInstr')]
        cycles, = [v for k, v in values.items() if k.endswith('.rob.clock_cycle')]
        blocks.append(dict(native_print_stamp=stamp, native_cycles=cycles, native_retired=retired,
                           egress=summarize(values, retired)))
    assert blocks, 'No native PERF frames'
    return dict(blocks=blocks, native_log_sha256=hashlib.sha256(Path(native_log).read_bytes()).hexdigest(),
                scope='Histogram snapshot windows; printed accepted counts may include one later handshake. '
                      'Bounds retain that one-cycle uncertainty. Multiplicity lacks source identity and readiness. '
                      'Input-request cycles overlap; output-not-ready cycles are interface conditions, not CPU '
                      'loss or uniquely credit/NoC/DRAM latency. Accepted-ready wait is a completed cohort '
                      'that can cross clean boundaries; its difference from total waiting is not a cause partition.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('native_log', type=Path, help='Collected, closed simulator_err.txt')
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    body = json.dumps(analyze(args.native_log), indent=2) + '\n'
    if args.output:
        args.output.write_text(body)
    else:
        print(body, end='')


if __name__ == '__main__':
    main()
