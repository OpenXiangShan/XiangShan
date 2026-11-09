"""Supplement TEA/DIP with typed native memory evidence from a closed default-CI point.

L2 definitions audited against XSCache a041f075 TopDownMonitor.scala; PTW
handshake/stall definitions against XiangShan8f8 Repeater.scala. Ratios are
observations, not CPU critical-path costs or a recommendation to expand queues.
"""
import argparse
import json
from pathlib import Path
import re

from native_perf import read_blocks, counter_multisets
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_wb_protocol_micro import write


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    verify(folder, receipt['files'])
    blocks = read_blocks(folder / 'simulator_err.txt')
    assert len(blocks) == 2
    counters = counter_multisets(blocks[max(blocks)])

    def scalar(suffix, unit):
        matches = {name: values for name, values in counters.items() if name.endswith(suffix)}
        values = next(iter(matches.values())) if len(matches) == 1 else []
        status = 'available' if len(values) == 1 else 'ambiguous' if matches else 'unavailable'
        return dict(status=status, value=values[0] if status == 'available' else None,
                    unit=unit, native_matches=matches)

    def number(item):
        return item['value'] if item['status'] == 'available' else None

    cycles = scalar('.rob.clock_cycle', 'cycles')
    retired = scalar('.rob.commitInstr', 'native retired instructions')
    assert number(cycles) is not None and number(retired) is not None and number(retired) > 0
    parallel = {}
    for kind in ['CPU', 'Prefetch', 'All']:
        prefix = '.l2cache.topDown.mshr_cycles_' + kind
        total = scalar(prefix + '_sum', 'valid A-channel miss entry-cycles')
        samples = scalar(prefix + '_sampled', 'cycle samples')
        count = scalar('.l2cache.topDown.mshr_count_' + kind, 'A-channel miss allocation events')
        average = number(total) / number(samples) if number(total) is not None and number(samples) else None
        parallel[kind] = dict(entry_cycles=total, samples=samples, allocations=count,
                              mean_parallel_misses=average)
    cpu, prefetch, all_misses = [parallel[k] for k in ['CPU', 'Prefetch', 'All']]
    totals = [number(row['entry_cycles']) for row in [cpu, prefetch, all_misses]]
    samples = [number(row['samples']) for row in [cpu, prefetch, all_misses]]
    conserved = (samples[0] == samples[1] == samples[2]
                 and all(v is not None for v in totals) and totals[0] + totals[1] == totals[2])
    head = scalar('.l2cache.topDown.RobBlockByL2Miss', 'ROB-head address match cycles')
    load_wait = scalar('.rob.waitLoadCycle', 'ROB-head not-writeback LOAD cycles')
    pressure = {}
    for name, values in counters.items():
        if re.search(r'\.l2cache\.slices_\d+\.reqArb\.sinkA_stall_by_mshrFull$', name):
            pressure[name] = dict(values=values, unit='per-bank condition cycles')
    ptw = {}
    for role, entry in [('load', 'load_filter_load_entry'), ('store', 'store_filter_store_entry'),
                        ('prefetch', 'prefetch_filter_prefetch_entry')]:
        prefix = '.dtlbRepeater.' + entry
        ptw[role] = dict(accepted=scalar(prefix + '.ptw_req_count', 'PTW request handshakes'),
                         blocked=scalar(prefix + '.ptw_req_blocked', 'valid and not-ready cycles'))
    return dict(point=receipt['point'], source=receipt['source'], native_cycles=number(cycles),
                native_retired=number(retired),native_CPI=number(cycles)/number(retired),
                L2_parallel_misses=parallel,L2_entry_cycle_partition_conserved=conserved,
                prefetch_marked_entry_time_share=totals[1]/totals[2] if conserved and totals[2] else None,
                ROB_head_L2_miss_match=head,
                ROB_head_L2_miss_match_per_retired=number(head)/number(retired) if number(head) is not None else None,
                ROB_head_load_wait=load_wait,
                ROB_head_load_wait_per_retired=number(load_wait)/number(retired) if number(load_wait) is not None else None,
                L2_sinkA_mshrFull=pressure,PTW=ptw,
                receipt_sha256=digest(receipt_path),native_log_sha256=digest(folder/'simulator_err.txt'),
                tool_sha256=digest(Path(__file__)),
                scope='ROB-head L2 address match is not qualified by not-writeback or LOAD type; '
                      'it is not a subset of native load-head wait. '
                      'CPU means fromA/is_miss/!is_prefetch, not exclusively demand loads. '
                      'Prefetch share is miss-entry time, not useless traffic or CPU-loss share. '
                      'Bank stall conditions can overlap and propagate; PTW handshakes and blocked '
                      'cycles have different units. Absent/ambiguous counters remain unknown; '
                      'these observations do not prove capacity, bandwidth or a causal performance gain.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    write(args.output, analyze(args.folder.resolve(), args.receipt.resolve()))


if __name__ == '__main__':
    main()
