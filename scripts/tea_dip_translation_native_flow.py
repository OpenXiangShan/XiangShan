"""Reconcile closed native translation traffic before attributing shared LLPTW sources.

The L1 data-TLB port merges load/store/prefetch filter groups. The load group
can itself include prefetch clients; none of these counts is a unique load ID.
"""
import argparse
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import digest, verify


def measure(blocks):
    phases = []
    for stamp, block in sorted(blocks.items()):
        def scalar(suffix):
            values, = [values for name, values in block.items() if name.endswith(suffix)]
            value, = values
            return value

        filters = {name: scalar('.dtlbRepeater.' + name + '_filter_' + name + '_entry.ptw_req_count')
                   for name in ['load', 'store', 'prefetch']}
        shared = scalar('.ptw.ptw.req_count1')
        assert sum(filters.values()) == shared, (stamp, 'filter outputs do not conserve shared L2 input')
        llptw = {}
        for metric in ['in_count', 'mem_req_count']:
            total = scalar('.ptw.ptw.llptw.llptw_' + metric)
            by_source = {str(source): scalar('.ptw.ptw.llptw.llptw_' + metric + '_source' + str(source))
                         for source in range(3)}
            assert sum(by_source.values()) == total, (stamp, metric, 'LLPTW source partition incomplete')
            llptw[metric] = dict(total=total, by_source=by_source)
        phases.append(dict(native_dump_stamp=stamp, native_cycles=scalar('.rob.clock_cycle'),
                           native_retired=scalar('.rob.commitInstr'), filter_output_fire_counts=filters,
                           shared_L2_source1_input_fire_count=shared, LLPTW=llptw,
                           LLPTW_memory_busy_cycles=scalar('.ptw.ptw.llptw.llptw_mem_busy_cycle'),
                           LLPTW_memory_wait_entry_cycles=scalar('.ptw.ptw.llptw.llptw_mem_wait_entry_cycle')))
    return dict(passed=True, phases=phases,
                scope='Native perf-reset intervals with their own cycle/retirement denominators. Filter outputs are '
                      'ready/valid handshakes and exactly partition L2 source1 input. The load filter includes any '
                      'prefetch clients assigned to the load TLB. LLPTW source0 is instruction, source1 aggregates '
                      'the three L1 data-TLB filter groups, source2 is the separate L2-TLB prefetcher. '
                      'No downstream LLPTW request is assigned to an upstream filter by these totals. '
                      'Waiting entry-cycles overlap and are not CPU lost cycles or per-request latency.')


def analyze(folder):
    folder = Path(folder).resolve()
    receipt_path = folder / 'closure-receipt.json'
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and result['original_process_closed']
    assert result['difftest_passed'] and result['exitcode'] == 0 and all(result['sampler_flags'].values())
    assert result['source'] == receipt['source'] and result['process'] == receipt['process']
    assert result['point'] == receipt['point'] == folder.name
    verify(folder, receipt['files'])
    output = measure(read_blocks(folder / 'simulator_err.txt'))
    output.update(point=folder.name, source=result['source'], receipt_sha256=digest(receipt_path),
                  native_sha256=digest(folder / 'simulator_err.txt'), tool_sha256=digest(Path(__file__)))
    return output


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    output = analyze(args.folder)
    args.output.write_text(json.dumps(output, indent=2) + '\n')
    print(json.dumps(output, indent=2))


if __name__ == '__main__':
    main()
