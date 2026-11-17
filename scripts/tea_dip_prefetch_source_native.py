"""Read all nine native L2 prefetch sources and preserve hit/overlap populations.

Cache Late_HitDemand and Late_HitL1Prefetch are the same NoWhere metadata
predicate. Legacy LateInMSHR includes duplicates against MSHRs and RequestBuffer;
HitInMSHR uses qualified acquire matches against actual prefetch MSHRs.
"""
import argparse
import json
from pathlib import Path

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_wb_protocol_micro import write

SOURCES = ['VBOP', 'PBOP', 'SMS', 'Stride', 'Stream', 'NextLine', 'TP', 'Berti', 'CDP']
METRICS = ['Sent', 'SentToPipe', 'Hit', 'HitInCache', 'HitInMSHR',
           'Late', 'LateInCache', 'LateInMSHR']
PREFIX = '.l2cache.topDown.l2prefetch'


def frame(block):
    def scalar(suffix):
        values, = [v for name, v in block.items() if name.endswith(suffix)]
        value, = values
        assert value >= 0, suffix
        return value

    totals = {metric: scalar(PREFIX + metric) for metric in METRICS}
    sources = {}
    for source in SOURCES:
        counts = {metric: scalar(PREFIX + metric + source) for metric in METRICS}
        assert counts['Hit'] == counts['HitInCache'] + counts['HitInMSHR'], source
        assert counts['Late'] == counts['LateInCache'] + counts['LateInMSHR'], source
        matrices = {}
        for stage in ['Cache', 'MSHR']:
            raw = {hit: scalar(PREFIX + 'LateIn' + stage + source + '_Hit' + hit)
                   for hit in ['Demand', 'L1Prefetch', *SOURCES]}
            if stage == 'Cache':
                assert raw['Demand'] == raw['L1Prefetch'], 'Cache NoWhere aliases disagree'
                row = {'NoWhere': raw['Demand'], **{s: raw[s] for s in SOURCES}}
            else:
                row = raw
            residual = counts['LateIn' + stage] - sum(row.values())
            assert residual >= 0, ('Overlap row exceeds native marginal', source, stage)
            row['Unclassified'] = residual
            assert sum(row.values()) == counts['LateIn' + stage]
            matrices[stage] = row
        sources[source] = dict(condition_counts=counts, overlap_hit_sources=matrices)
    for metric in METRICS:
        assert sum(s['condition_counts'][metric] for s in sources.values()) == totals[metric], metric
    assert totals['Hit'] == totals['HitInCache'] + totals['HitInMSHR']
    assert totals['Late'] == totals['LateInCache'] + totals['LateInMSHR']
    cycles, retired = scalar('.rob.clock_cycle'), scalar('.rob.commitInstr')
    misses = {name: scalar('.l2cache.topDown.' + name)
              for name in ['l2demandMiss', 'l1prefetchMiss', 'l2prefetchMiss']}
    return dict(native_cycles=cycles, native_retired=retired, native_CPI=cycles / retired,
                totals=totals, sources=sources, lookup_misses=misses,
                lookup_misses_per_kinst={name: value / retired * 1000 for name, value in misses.items()},
                condition_counts_per_kinst={source: {metric: value / retired * 1000
                    for metric, value in row['condition_counts'].items()}
                    for source, row in sources.items()})


def analyze(folder, receipt_path):
    folder, receipt_path = Path(folder), Path(receipt_path)
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    verify(folder, receipt['files'])
    result = json.loads((folder / 'result.json').read_text())
    assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
    assert result['source'] == receipt['source']
    command = json.loads((folder / 'command.json').read_text())
    blocks = read_blocks(folder / 'simulator_err.txt')
    labels = ['warmup', 'ROI'] if '-W' in command else ['window']
    assert len(blocks) == len(labels)
    phases = [dict(phase=label, native_print_stamp=stamp, **frame(block))
              for label, (stamp, block) in zip(labels, sorted(blocks.items()), strict=True)]
    return dict(passed=True, point=folder.name, source=receipt['source'], phases=phases,
                receipt_sha256=digest(receipt_path), native_log_sha256=digest(folder / 'simulator_err.txt'),
                tool_sha256=digest(Path(__file__)),
                scope='Existing native conditions only; frames use their actual retired denominators. '
                      'Sent counts downstream prefetch port fires, not source consumption or useful fills. '
                      'Cache Hit requires CPU load/store/atomic request, directory hit and prefetch metadata. '
                      'Legacy HitInMSHR counts qualified AcquireBlock/AcquirePerm matches to real prefetch '
                      'MSHRs with adjacent same-address suppression, not exclusively CPU loads or fire events. '
                      'Legacy LateInCache means incoming prefetch hits a resident cache line. Its Demand '
                      'and L1Prefetch columns both test resident PfSource.NoWhere, so are represented once '
                      'as NoWhere; this does not identify who originally fetched that line. '
                      'Legacy LateInMSHR means incoming-prefetch duplicate conditions against MSHRs or '
                      'RequestBuffer; its pending-request source classes are distinct. Unclassified '
                      'residuals are retained. Late is not a CPU timing delay and held valid can repeat. '
                      'Hits and sends are different cohorts; no same-window accuracy, unique request, '
                      'retired-load service, saved CPU cycles or causal bottleneck is inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    write(args.output, analyze(args.folder.resolve(), args.receipt.resolve()))


if __name__ == '__main__':
    main()
