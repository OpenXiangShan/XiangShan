"""Bound PTW-filter full-input conditions that occurred with unused global entries.

The calibrated old GSIM layout has sixteen load-filter entries. PTWFilterEntry
accepts at most four input ports; each native full condition is a port-cycle,
not a unique request or CPU stall. Existing arbitration validation establishes
complete active-entry occupancy before using the aggregate bound.
"""
import argparse
from collections import Counter
import csv
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_ptw_arb_trace import analyze as arbitration
from tea_dip_query_endpoint_evidence import digest


def measure(rows, blocks, phases):
    histograms = [Counter() for _ in phases]
    upper_quarter = [Counter() for _ in phases]
    for row in rows:
        if row['kind'] != 1:
            continue
        occupancy = row['load_valid_mask'].bit_count()
        for histogram, quarter, phase in zip(histograms, upper_quarter, phases):
            if phase['begin_core_exclusive'] < row['core_cycle'] <= phase['end_core_inclusive']:
                histogram[occupancy] += 1
                quarter[(row['load_valid_mask'] >> 12).bit_count()] += 1
    output = []
    for histogram, quarter, phase in zip(histograms, upper_quarter, phases):
        observed = sum(occupancy * cycles for occupancy, cycles in histogram.items())
        assert observed == phase['native_margins']['load']['entry_cycle'], 'Active-entry coverage mismatch'
        block = blocks[phase['native_print_stamp']]
        values, = [v for name, v in block.items() if name.endswith('.dtlbRepeater.load_filter_load_entry.tlb_req_full_count')]
        full, = values
        below_four_free = sum(cycles for occupancy, cycles in histogram.items() if occupancy > 12)
        output.append(dict(native_print_stamp=phase['native_print_stamp'],
                           native_full_input_port_cycles=full,
                           observed_occupancy_histogram=dict(sorted(histogram.items())),
                           entries12_to15_occupancy_histogram=dict(sorted(quarter.items())),
                           maximum_observed_occupancy=max(histogram, default=0),
                           globally_full_cycles=histogram[16],
                           cycles_with_less_than_four_free_entries=below_four_free,
                           full_input_port_cycles_with_at_least_four_free_entries_lower_bound=max(0, full - 4 * below_four_free)))
    return output


def analyze(folder, receipt_path):
    folder = Path(folder).resolve()
    base = arbitration(folder, receipt_path)
    blocks = read_blocks(folder / 'simulator_err.txt')
    with (folder / 'ptw-arb-probe.csv').open() as stream:
        rows = ({k: int(v) for k, v in row.items()} for row in csv.DictReader(stream))
        phases = measure(rows, blocks, base['phases'])
    return dict(passed=True, point=base['point'], source=base['source'],
                receipt_sha256=base['receipt_sha256'], trace_sha256=base['trace_sha256'],
                entry_capacity=16, maximum_input_ports=4, phases=phases,
                consumer_sha256=digest(Path(__file__)),
                scope='Closed source-specific sixteen-entry load-filter occupancy, already reconciled against native entry-cycles. Each cycle with fewer than four global free entries can contain at most four full-input port conditions. Subtracting this maximum yields a lower bound on full conditions while at least four entries were free. Counts can repeat requests and include assigned prefetch clients. The bound proves allocation opportunity, not request criticality, additional unique allocations, saved CPU cycles or performance gain. Occupancy histogram contains recorded active cycles, including load-empty cycles when another group was active; omitted idle zero-occupancy cycles do not change the bound.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', required=True, type=Path)
    parser.add_argument('--receipt', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    assert not args.output.exists()
    result = analyze(args.folder, args.receipt)
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
