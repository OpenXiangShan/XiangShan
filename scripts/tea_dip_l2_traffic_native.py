"""Read L2 directory traffic and completed MSHR residence from closed GSIM evidence.

Definitions: XSCache a041f075/c6d2c88a and utility 002ca4ad. Three adjacent
histograms repeat each residence sum/sample count; validate and count it once.
The mixed request population and completion window are not CPU load latency.
"""
import argparse
import json
from pathlib import Path
import re

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_wb_protocol_micro import write


RANGES = [(lo, lo + step) for start, stop, step in
          [(0, 300, 10), (300, 1000, 50), (1000, 5000, 200)]
          for lo in range(start, stop, step)]


def residence(counters):
    groups = {}
    for name, values in counters.items():
        match = re.fullmatch(r'(.*\.l2cache\.slices_\d+\.mshrCtl\.mshr_latency_\d+)_(.+)', name)
        if match:
            groups.setdefault(match[1], {})[match[2]] = values
    assert groups, 'No completed-residence histograms'
    histogram = {f'{lo}_{hi}': 0 for lo, hi in RANGES}
    slots = {}
    for name, fields in sorted(groups.items()):
        totals = {}
        for field in ['sum', 'sampled']:
            values = fields[field]
            assert len(values) == 3 and len(set(values)) == 1, (name, field, values)
            totals[field] = values[0]
        bins = {key: values for key, values in fields.items() if re.fullmatch(r'\d+_\d+', key)}
        assert bins.keys() == histogram.keys(), name
        assert all(len(values) == 1 and values[0] >= 0 for values in bins.values()), name
        assert sum(values[0] for values in bins.values()) == totals['sampled'], name
        assert totals['sum'] >= sum(int(key.split('_')[0]) * values[0] for key, values in bins.items()), name
        for key, values in bins.items():
            histogram[key] += values[0]
        slots[name] = totals
    count = sum(row['sampled'] for row in slots.values())
    total = sum(row['sum'] for row in slots.values())
    return dict(completed_episodes=count, residence_cycle_sum=total,
                mean_completed_residence=total / count if count else None,
                bins=[dict(lower=lo, upper_exclusive=hi if hi < 5000 else None,
                           episodes=histogram[f'{lo}_{hi}']) for lo, hi in RANGES],
                slots=slots,
                scope='Allocation-to-will_free timers for all MSHR request classes, sampled on free. '
                      'Timers can start before ROI; still-active requests are absent. '
                      'The final bin includes overflow. Not demand-load or CPU critical-path latency.')


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    verify(folder, receipt['files'])
    blocks = read_blocks(folder / 'simulator_err.txt')
    assert len(blocks) == 2
    counters = blocks[max(blocks)]

    def scalar(suffix):
        matches = {key: values for key, values in counters.items() if key.endswith(suffix)}
        assert len(matches) == 1, suffix
        name, values = next(iter(matches.items()))
        assert len(values) == 1 and values[0] >= 0, (name, values)
        return name, values[0]

    cycles = scalar('.rob.clock_cycle')[1]
    retired = scalar('.rob.commitInstr')[1]
    assert retired > 0
    traffic = {}
    names = ['l2demandMiss', 'l1prefetchMiss', 'l2prefetchMiss', 'l2prefetchSent',
             'l2prefetchSentToPipe', 'l2prefetchHit', 'l2prefetchHitInCache',
             'l2prefetchHitInMSHR', 'l2prefetchLate', 'l2prefetchLateInCache', 'l2prefetchLateInMSHR']
    for metric in names:
        name, value = scalar('.l2cache.topDown.' + metric)
        traffic[metric] = dict(native_name=name, events=value, per_kinst=1000 * value / retired)
    for total, parts in [('l2prefetchHit', ['l2prefetchHitInCache', 'l2prefetchHitInMSHR']),
                         ('l2prefetchLate', ['l2prefetchLateInCache', 'l2prefetchLateInMSHR'])]:
        assert traffic[total]['events'] == sum(traffic[key]['events'] for key in parts)
    sources = {}
    for name, values in counters.items():
        match = re.search(r'\.l2cache\.topDown\.E2_L2AReqSource_(.+)_(Total|Miss)$', name)
        if match:
            assert len(values) == 1 and values[0] >= 0, (name, values)
            sources.setdefault(match[1], {})[match[2]] = values[0]
    assert sources
    for row in sources.values():
        assert row['Miss'] <= row['Total']
    return dict(point=receipt['point'], source=receipt['source'], native_cycles=cycles,
                native_retired=retired, traffic=traffic, directory_request_sources=sources,
                completed_MSHR_residence=residence(counters),
                receipt_sha256=digest(receipt_path), native_log_sha256=digest(folder/'simulator_err.txt'),
                tool_sha256=digest(Path(__file__)),
                scope='Directory counts are channel-A lookup events, not unique requests or DRAM traffic. '
                      'l2demandMiss includes CPU load/store/atomic, excludes PTW and L1 prefetch. '
                      'HitInCache counts CPU hits on prefetch-marked lines; HitInMSHR is an incoming '
                      'Acquire address match to a prefetch MSHR, not a confirmed merge handshake. '
                      'LateInCache is an L2 prefetch hitting cache; LateInMSHR is a duplicate L2 '
                      'prefetch matching an MSHR OR request-buffer entry. Neither measures CPU waiting. '
                      'Window events are not matched prefetch lifetimes or disjoint saved-cycle budgets.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    write(args.output, analyze(args.folder.resolve(), args.receipt.resolve()))


if __name__ == '__main__':
    main()
