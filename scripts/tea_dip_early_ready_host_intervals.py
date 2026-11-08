"""Compare saved original-process CPU intervals for the neutral8f8/6fde short runs."""
from pathlib import Path
from datetime import datetime
import argparse
import hashlib
import json


def analyze(root, clock_ticks):
    rows = []
    sources = {}
    sha = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
    for point in ['gromacs_105_0.206090', 'lbm_6382_0.375486', 'sphinx3_157921_0.136728']:
        observations = []
        commands = []
        for variant, family, stage_name in [('control8f8', 'fdiv-turnover-20261108', 'fdiv-turnover-validation-20261108'), ('candidate6fde', 'fdiv-early-ready-20261108', 'fdiv-early-ready-validation-20261108')]:
            folder = root / stage_name / point
            result = json.loads((folder / 'result.json').read_text())
            identity = result['process']
            command = json.loads((folder / 'command.json').read_text())
            commands.append(command)
            samples = []
            paths = list((root / 'evidence' / family).glob('runtime-*.json'))
            if variant == 'control8f8':
                paths += list((root / 'evidence/fdiv-turnover-observe-20261108').glob('runtime-*.json'))
            for path in sorted(paths):
                document = json.loads(path.read_text())
                node = document.get('node031', {}) if variant == 'control8f8' else document
                guests = node.get('runs', {}).get(stage_name, {}).get('guests', [])
                for guest in guests:
                    if guest['folder'] != point:
                        continue
                    process = guest if variant == 'control8f8' else guest['process']
                    recorded = guest['process'] if variant == 'control8f8' else guest['identity']
                    if not process['original_live'] or 'cpu_ticks' not in process:
                        continue
                    assert all(recorded[k] == identity[k] for k in ['pid', 'start_ticks'])
                    samples.append(dict(utc=node['utc'], cpu_ticks=process['cpu_ticks'], file=str(path.relative_to(root))))
                    sources[str(path.relative_to(root))] = sha(path)
            samples.sort(key=lambda r: r['utc'])
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
            sources[str((folder/'result.json').relative_to(root))] = sha(folder/'result.json')
            sources[str((folder/'command.json').relative_to(root))] = sha(folder/'command.json')
        assert commands[0][:6] == commands[1][:6]
        assert observations[0]['identity']['host'] == observations[1]['identity']['host']
        rows.append(dict(point=point, same_host_core_and_nice=True, command_prefix=commands[0][:6], observations=observations))
    return dict(clock_ticks_per_second=clock_ticks, rows=rows, source_sha256=sources, consumer_sha256=sha(Path(__file__)), scope='Saved originalPID/startticks process CPU delivery in explicitly covered UTC intervals only. Same host/core/nice and complete guest-neutrality established separately. Coverage is partial, not whole-run rusage or unique cause of lifetime wall-time change. No live DB, load/idle sampling, restart or guest speedup inference.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('--clock-ticks', required=True, type=int)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    result = analyze(args.report_dir.resolve(), args.clock_ticks)
    assert not args.output.exists(), 'Preserve the original host interval analysis'
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps([dict(point=row['point'], observations=[{k:v for k,v in o.items() if k not in ['samples','intervals']} for o in row['observations']]) for row in result['rows']], indent=2))


if __name__ == '__main__':
    main()
