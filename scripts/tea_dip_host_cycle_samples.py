"""Analyze closed perf cycle samples with exact period weights and source coverage."""
from pathlib import Path
import argparse
import hashlib
import json
import re
from tea_dip_query_endpoint_evidence import verify

HEADER = re.compile(r'\s*(\d+)/(\d+)\s+(\d+\.\d+):\s+(\d+)\s*')
FRAME = re.compile(r'\s*([0-9a-f]+)\s+(.+?)(?:\s+\(([^()]*)\))?')


def parse_samples(text):
    rows = []
    for block in re.split(r'\n\s*\n', text.strip()) if text.strip() else []:
        lines = block.splitlines()
        header = HEADER.fullmatch(lines[0])
        assert header and len(lines) > 1, 'Unsupported or incomplete perf sample header'
        leaf = FRAME.fullmatch(lines[1])
        assert leaf, 'Missing perf leaf frame'
        frames = [FRAME.fullmatch(line) for line in lines[1:]]
        frames = [dict(ip=m[1], symbol=m[2], dso=m[3] or '[unresolved-inline]') for m in frames if m]
        location = lines[2].strip() if len(lines) > 2 and not FRAME.fullmatch(lines[2]) else ''
        rows.append(dict(pid=int(header[1]), tid=int(header[2]), sample_time=float(header[3]),
                         period=int(header[4]), ip=leaf[1], symbol=leaf[2],
                         dso=leaf[3] or '[unresolved-inline]', source_location=location,
                         source_line_resolved=bool(re.search(r':\d+(?:\s+\(inlined\))?$', location)), frames=frames))
    return rows


def summary(rows, executable):
    total = sum(row['period'] for row in rows)
    fraction = lambda value, denominator: value/denominator if denominator else None
    dsos, symbols = {}, {}
    for row in rows:
        for table, key in [(dsos, row['dso']), (symbols, (row['symbol'], row['dso']))]:
            value = table.setdefault(key, dict(samples=0, period_sum=0))
            value['samples'] += 1
            value['period_sum'] += row['period']
    def aggregate(table, symbol=False):
        result = []
        for key, value in sorted(table.items(), key=lambda item: item[1]['period_sum'], reverse=True):
            identity = dict(symbol=key[0], dso=key[1]) if symbol else dict(dso=key)
            result.append(dict(**identity, **value, sample_fraction=fraction(value['samples'], len(rows)), period_weight_fraction=fraction(value['period_sum'], total)))
        return result
    target = [row for row in rows if row['dso'] == executable]
    target_period = sum(row['period'] for row in target)
    known = [row for row in target if row['source_line_resolved']]
    unknown_leaf = [row for row in rows if row['symbol'] == '[unknown]' or row['dso'].startswith('[')]
    return dict(samples=len(rows), sampled_period_sum=total, distinct_pids=sorted({r['pid'] for r in rows}),
                distinct_tids=sorted({r['tid'] for r in rows}), min_sample_period=min((r['period'] for r in rows), default=None),
                max_sample_period=max((r['period'] for r in rows), default=None),
                sampled_timestamp_span_seconds=max(r['sample_time'] for r in rows)-min(r['sample_time'] for r in rows) if rows else None,
                leaf_by_dso=aggregate(dsos), leaf_by_symbol=aggregate(symbols, True),
                unresolved_leaf_samples=len(unknown_leaf), unresolved_leaf_period_fraction=fraction(sum(r['period'] for r in unknown_leaf), total),
                target_executable=executable, target_leaf_samples=len(target), target_leaf_period_sum=target_period,
                target_source_line_samples=len(known), target_source_line_period_fraction=fraction(sum(r['period'] for r in known), target_period))


def analyze(root):
    read = lambda p: json.loads(p.read_text())
    original = read(root/'closure-receipt.json')
    verify(root, original['files'])
    export_root = root/'identity-export'
    export = read(export_root/'closure-receipt.json')
    verify(export_root, export['files'])
    assert export['perf_data_sha256'] == original['files']['perf.data']['sha256']
    command = read(export_root/'identity-script-command.json')
    assert command['exitcode'] == 0 and command['perf_data_sha256'] == export['perf_data_sha256']
    assert command['command'][command['command'].index('-F')+1] == 'pid,tid,time,period,ip,sym,dso,srcline'
    metadata = read(root/'metadata.json')
    assert metadata['record_exitcode'] == metadata['report_exitcode'] == 0
    assert metadata['command'][metadata['command'].index('-e')+1] == 'cycles:u'
    assert metadata['command'][metadata['command'].index('-p')+1] == str(metadata['process']['pid'])
    rows = parse_samples((export_root/'identity-script.stdout').read_text())
    recorded = re.search(r'\((\d+) samples\)', (root/'record.stderr').read_text())
    assert recorded and len(rows) == int(recorded[1]), 'Recorded and exported sample populations differ'
    assert all(row['pid'] == metadata['process']['pid'] for row in rows), 'Export has a different target PID'
    result = summary(rows, metadata['before']['args'][0])
    report = (root/'report.stdout').read_text()
    counted = re.search(r'# Event count \(approx\.\): (\d+)', report)
    lost = re.search(r'# Total Lost Samples: (\d+)', report)
    assert counted and result['sampled_period_sum'] == int(counted[1]), 'Raw period weights differ from perf report'
    assert lost, 'Missing lost-sample evidence'
    before, after = metadata['before'], metadata['after']
    interval = None
    if before['original_live'] and after['original_live']:
        wall = after['monotonic']-before['monotonic']
        cpu = (after['cpu_ticks']-before['cpu_ticks'])/metadata['clock_ticks_per_second']
        assert wall > 0 and cpu >= 0
        interval = dict(bracket_wall_seconds=wall, bracket_cpu_seconds=cpu, cpu_seconds_per_wall_second=cpu/wall)
    sources = {name: row['sha256'] for name, row in original['files'].items()}
    sources.update({'identity-export/'+name: row['sha256'] for name, row in export['files'].items()})
    return dict(passed=True, process=metadata['process'], build=metadata['build'], summary=result,
                lost_samples=int(lost[1]), original_live_both_ends=bool(before['original_live'] and after['original_live']),
                snapshot_interval=interval, samples=rows, source_sha256=sources,
                consumer_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                scope='One attached cycles:u profile, normalized by sampled period, with sample counts separately retained. Raw leaf PID, recorder count, perf-report period sum and source hashes cross-checked. Callchains can contain unresolved/inline frames; missing source lines are retained. Perf timestamps are not aligned to the outer monotonic CPU bracket here. No IPC/frequency, whole-run overhead, guest-performance, RTL-unit or unique critical-path attribution is inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--profile-dir', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    result = analyze(args.profile_dir.resolve())
    assert not args.output.exists(), 'Preserve the original sampled-profile analysis'
    args.output.write_text(json.dumps(result, indent=2)+'\n')
    print(json.dumps(dict(passed=result['passed'], lost_samples=result['lost_samples'], snapshot_interval=result['snapshot_interval'], summary={k:v for k,v in result['summary'].items() if k not in ['leaf_by_symbol','leaf_by_dso']}, top_symbols=result['summary']['leaf_by_symbol'][:5]), indent=2))


if __name__ == '__main__':
    main()
