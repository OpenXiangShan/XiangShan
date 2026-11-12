"""Read typed LRQ replay evidence from a naturally closed GSIM point.

Cause definitions and timer semantics: LoadQueueReplay.scala in XiangShan
9dd0d563/78ce2391. Normal releases use the final one-hot cause; scalar ROB-head
occupancy uses the current cause and is not an additive CPU-state partition.
"""
import argparse
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_wb_protocol_micro import write


CAUSES = ('uncache', 'storeQueue_multi_match', 'mem_amb', 'tlb_miss', 'forward_fail',
          'dcache_replay', 'dcache_miss', 'wpu_fail', 'bank_conflict', 'rar_nack',
          'raw_nack', 'nuke', 'misalign_buffer_full')


def summarize(counters):
    def metric(suffix, unit):
        matches = {name: values for name, values in counters.items() if name.endswith(suffix)}
        values = next(iter(matches.values())) if len(matches) == 1 else []
        status = 'available' if len(values) == 1 else 'ambiguous' if matches else 'unavailable'
        value = values[0] if status == 'available' else None
        if value is not None:
            assert value >= 0, (suffix, value)
        return dict(status=status, value=value, unit=unit, native_matches=matches)

    causes = {}
    for cause in CAUSES:
        prefix = '.loadQueueReplay.replay_' + cause
        count = metric(prefix + '_release_count', 'normally released LRQ entries')
        residence = metric(prefix + '_release_latency_cycles', 'released-entry recorded residence cycles')
        overflow = metric(prefix + '_release_latency_overflow_count', 'released entries with saturated timer')
        head = metric(prefix + '_scalar_rob_head_blocked_cycles', 'unreleased scalar ROB-head condition cycles')
        n, total, saturated = [item['value'] for item in [count, residence, overflow]]
        if n is not None and saturated is not None:
            assert saturated <= n, cause
        mean = total / n if n and total is not None and saturated == 0 else None
        causes[cause] = dict(releases=count, recorded_residence=residence,
                             saturated_releases=overflow, scalar_ROB_head_blocked=head,
                             mean_released_entry_residence_cycles=mean)
    return causes


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
                scope='Release residence is allocation-to-normal-release, attributed to the final cause; '
                      'it can start before ROI and excludes cancelled and still-live entries. '
                      'A mean is emitted only for nonzero releases with known zero saturation. '
                      'Scalar ROB-head conditions use the current cause, OR entries per cause per cycle, '
                      'exclude cancel/release cycles, and do not require an unready ROB entry. '
                      'Different causes may overlap within one compressed ROB entry. '
                      'These are not CPU-exclusive lost cycles, cache service latency, or unique critical loads. '
                      'Missing and ambiguous native counters remain unknown; no causes are summed into CPU time.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    write(args.output, analyze(args.folder.resolve(), args.receipt.resolve()))


if __name__ == '__main__':
    main()
