"""Measure saved CPU delivery for the closed, neutral cd5/ec964 SPEC observers."""
from pathlib import Path
from datetime import datetime
import argparse
import hashlib
import json
from tea_dip_query_endpoint_evidence import verify


def analyze(root, clock_ticks):
    sha = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
    read = lambda p: json.loads(p.read_text())
    rows, sources = [], {}
    for point in ['gromacs_105_0.206090', 'lbm_6382_0.375486', 'sphinx3_157921_0.136728']:
        hstage = root / 'evidence/fdiv-handoff-observe-20261108/closed-points' / point
        analysis_path = hstage / 'analysis/evidence.json'
        analysis = read(analysis_path)
        assert analysis['correct'] and analysis['observation_neutrality']['passed']
        assert analysis['closed_receipt_sha256'] == sha(hstage / 'closure-receipt.json')
        observations, commands = [], []
        for variant, family, stage_name in [('control_cd5', 'fdiv-turnover-observe-20261108', 'fdiv-turnover-observe-validation-20261108'), ('observer_ec964', 'fdiv-handoff-observe-20261108', 'fdiv-handoff-validation-20261108')]:
            stage = root / 'evidence' / family / 'closed-points' / point
            receipt = read(stage / 'closure-receipt.json')
            assert receipt['ready'] and not receipt['original_process_live']
            verify(stage, receipt['files'])
            folder = stage / point
            result = read(folder / 'result.json')
            identity = result['process']
            assert result['original_process_closed'] and result['exitcode'] == 0 and result['difftest_passed']
            assert result['process'] == receipt['process']
            command = read(folder / 'command.json')
            commands.append(command)
            samples = []
            for path in sorted((root / 'evidence' / family).glob('runtime-*.json')):
                document = read(path)
                node = document['node031'] if variant == 'control_cd5' else document
                for guest in node['runs'].get(stage_name, {}).get('guests', []):
                    if guest['folder'] != point:
                        continue
                    process = guest if variant == 'control_cd5' else guest['process']
                    recorded = guest['process'] if variant == 'control_cd5' else guest['identity']
                    if not process['original_live'] or 'cpu_ticks' not in process:
                        continue
                    assert all(recorded[k] == identity[k] for k in ['pid', 'start_ticks', 'host', 'core'])
                    samples.append(dict(utc=node['utc'], cpu_ticks=process['cpu_ticks'], file=str(path.relative_to(root))))
                    sources[str(path.relative_to(root))] = sha(path)
            samples.sort(key=lambda row: row['utc'])
            assert len(samples) >= 2
            intervals = []
            for a, b in zip(samples, samples[1:]):
                wall = (datetime.fromisoformat(b['utc']) - datetime.fromisoformat(a['utc'])).total_seconds()
                cpu = (b['cpu_ticks'] - a['cpu_ticks']) / clock_ticks
                assert wall > 0 and cpu >= 0
                intervals.append(dict(utc_start=a['utc'], utc_end=b['utc'], wall_seconds=wall, cpu_seconds=cpu, cpu_seconds_per_wall_second=cpu/wall))
            wall = sum(v['wall_seconds'] for v in intervals)
            cpu = sum(v['cpu_seconds'] for v in intervals)
            observations.append(dict(variant=variant, identity=identity, samples=samples, intervals=intervals, covered_wall_seconds=wall, covered_cpu_seconds=cpu, cpu_seconds_per_wall_second=cpu/wall, observed_lifetime_wall_seconds=result['host_wall_seconds'], wall_fraction_covered=wall/result['host_wall_seconds']))
            for path in [folder/'result.json', folder/'command.json', stage/'closure-receipt.json']:
                sources[str(path.relative_to(root))] = sha(path)
        assert commands[0][:6] == commands[1][:6]
        assert observations[0]['identity']['host'] == observations[1]['identity']['host']
        for flag in ['-W', '-I', '-C', '-s', '-i', '--diff', '--cst-file', '--dump-select-db']:
            assert commands[0][commands[0].index(flag)+1] == commands[1][commands[1].index(flag)+1]
        sources[str(analysis_path.relative_to(root))] = sha(analysis_path)
        rows.append(dict(point=point, same_host_core_nice_and_guest_execution=True, command_prefix=commands[0][:6], observations=observations, observed_lifetime_wall_change=observations[1]['observed_lifetime_wall_seconds']/observations[0]['observed_lifetime_wall_seconds']-1))
    return dict(clock_ticks_per_second=clock_ticks, rows=rows, source_sha256=sources, consumer_sha256=sha(Path(__file__)), scope='Closed cd5/ec964 diagnostic pairs with exact old-native/six-table neutrality. Saved original PID/startticks CPU intervals have partial coverage; they do not measure whole-run rusage, PMU, unique wall-time causes or isolated diagnostic overhead. No SQLite queries, load sampling, dispatch or simulation restart.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--clock-ticks', type=int, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    result = analyze(args.report_dir.resolve(), args.clock_ticks)
    assert not args.output.exists(), 'Preserve the historical CPU interval result'
    args.output.write_text(json.dumps(result, indent=2)+'\n')
    print(json.dumps([dict(point=row['point'], observed_lifetime_wall_change=row['observed_lifetime_wall_change'], observations=[{k:v for k,v in observation.items() if k not in ['samples', 'intervals']} for observation in row['observations']]) for row in result['rows']], indent=2))


if __name__ == '__main__':
    main()
