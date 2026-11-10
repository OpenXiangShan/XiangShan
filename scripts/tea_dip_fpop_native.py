"""Read existing FPOP feedback proxies from closed single-core GSIM artifacts.

Definitions follow XSCache a041f075 PrefetchController. Component sums are
weighted controller estimates, not CPU cycles or the epoch PE accumulator.
"""
import argparse
import json
from pathlib import Path
import re

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_wb_protocol_micro import write

ROLES = ['Stream', 'Stride', 'Berti', 'SMS', 'VBOP', 'PBOP', 'TP']
BENEFITS = ['mshrHit', 'demandCacheHit', 'l1PrefetchCacheHit']
COSTS = ['pollutionHold', 'pfMshrHold', 'pfReqBufferHold']
PREFIX = '.l2cache.prefetcher.prefetchController.'


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    verify(folder, receipt['files'])
    stdout = (folder / 'simulator_out.txt').read_text()
    constants = {}
    for name in ['l2pf_controlMode0', 'l2pf_tlow0', 'l2pf_peHoldWeightLog0', 'l2pf_peL1PfCacheHitWeightLog0']:
        value, = re.findall(r'^' + name + r' = (\d+)$', stdout, re.M)
        constants[name] = int(value)
    blocks = read_blocks(folder / 'simulator_err.txt')
    command = json.loads((folder / 'command.json').read_text())
    assert len(blocks) == (2 if '-W' in command else 1)
    labels = ['warmup', 'ROI'] if '-W' in command else ['window']
    frames = []
    for label, (stamp, block) in zip(labels, sorted(blocks.items())):
        def scalar(suffix):
            matches = [values for name, values in block.items() if name.endswith(suffix)]
            values, = matches
            value, = values
            assert value >= 0, suffix
            return value
        roles = {}
        for role in ROLES:
            components = {name: dict(events=scalar(PREFIX + 'stat_' + name + 'Count' + role),
                                      proxy_sum=scalar(PREFIX + 'delta_' + name + 'Sum' + role))
                          for name in BENEFITS + COSTS}
            positive = sum(components[name]['proxy_sum'] for name in BENEFITS)
            negative = sum(components[name]['proxy_sum'] for name in COSTS)
            roles[role] = dict(components=components,
                               active_epochs=scalar(PREFIX + 'epoch_activeEpochCount' + role),
                               inactive_epochs=scalar(PREFIX + 'epoch_inactiveEpochCount' + role),
                               reactivated_by_prior_hits=scalar(PREFIX + 'epoch_pfHitLagActiveCount' + role),
                               reactivated_by_latency_drop=scalar(PREFIX + 'epoch_latencyDownActiveCount' + role),
                               PE_overflow_events=scalar(PREFIX + 'other_peOverflowCount' + role),
                               proxy_positive_sum=positive, proxy_negative_sum=negative,
                               proxy_balance=positive - negative)
        top = '.l2cache.prefetcher.'
        training = {}
        for role in ['VBOP', 'PBOP']:
            generator = role.lower()
            training[role] = dict(
                buffer_accepted=scalar(top + generator + '_train_accept'),
                buffer_drop_valid_cycles=scalar(top + generator + '_train_drop'),
                learner_consumed=scalar(top + generator + '.bop_train'),
                generated_requests=scalar(top + generator + '.bop_req'),
                table_not_ready_cycles=scalar(top + generator + '.bop_train_stall_for_st_not_ready'),
                student_not_ready_cycles=scalar(top + generator + '.bop_train_stall_for_stu_not_ready'),
                feedback_zero_s1_valid_cycles=scalar(top + generator + '.bop_l2_feedback_control_drop'))
        frames.append(dict(phase=label, native_print_stamp=stamp,
                           native_cycles=scalar('.rob.clock_cycle'), native_retired=scalar('.rob.commitInstr'),
                           epoch_end_events=scalar(PREFIX + 'epochCount'), roles=roles,
                           BOP_training=dict(incoming_valid_cycles=scalar(top + 'prefetch_train_valid'),
                                             generators=training)))
    return dict(point=folder.name, source=receipt['source'], runtime_constants=constants, frames=frames,
                receipt_sha256=digest(receipt_path), native_log_sha256=digest(folder / 'simulator_err.txt'),
                stdout_sha256=digest(folder / 'simulator_out.txt'), tool_sha256=digest(Path(__file__)),
                scope='Printed frames retained without cumulative differencing. Controller classes are not '
                      'all traffic sources, nor exclusively demand loads. Component event populations overlap. '
                      'Proxy sums use configured latency weights; their balance is not saved CPU cycles, '
                      'a critical-load latency, the internal signed PE accumulator or an epoch decision. '
                      'PE resets, pipeline timing, overflow and history prevent reconstructing decisions from '
                      'whole-window sums. Epoch end and active/inactive counters use different pipeline enables; '
                      'their equality is not assumed. Active epochs do not prove the feedback mode was enabled. '
                      'BOP buffer acceptance, input-drop conditions and learner-consumed events are separate stages. '
                      'The total incoming train-valid population may include inputs filtered before BOP; '
                      'it is not assumed to partition into either generator buffer acceptance and drops. '
                      'Table/student not-ready conditions omit other downstream blockers. '
                      'Feedback-zero s1-valid counts are condition cycles, not unique dropped requests. '
                      'No request-level causality, full-suite gain or live database read is inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    result = analyze(args.folder.resolve(), args.receipt.resolve())
    write(args.output, result)
    print(json.dumps(dict(point=result['point'], runtime_constants=result['runtime_constants'],
                          frames=len(result['frames'])), indent=2))


if __name__ == '__main__':
    main()
