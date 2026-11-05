"""Preserve original WB experiment process observations across analysis sessions."""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import json
import os
import subprocess
import time


def observe_once(root, query=False, formal=False):
    runtime = root / ('evidence/wb-query-formal-tools/tea_dip_experiment_runtime.py' if formal else 'evidence/wb-query-analysis-tools/tea_dip_experiment_runtime.py' if query else 'evidence/wb-class-candidate-tools/tea_dip_experiment_runtime.py')
    record = dict(utc=datetime.now(timezone.utc).isoformat(), capture_passed=False,
                  original_processes_exited=False, runtime_sha256=hashlib.sha256(runtime.read_bytes()).hexdigest())
    try:
        reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=15',
                                'node031', 'python3', '-'], input=runtime.read_text(), text=True,
                               capture_output=True, timeout=55)
        record.update(returncode=reply.returncode, stderr=reply.stderr[-2000:])
        if reply.returncode:
            return record
        snapshot = json.loads(reply.stdout)
        states, guests = [], []
        experiments = [('fdiv-wb-query', ['fdiv-wb-query'], 'wb-query-formal')] if formal else [('fdiv-wb-query', ['fdiv-wb-query'], 'wb-query')] if query else [('fp-wb-source-observe', ['fp-wb-source-observe', 'fp-wb-source-observe-depth32'], 'fp-wb-source-observation'), ('fdiv-wb-class', ['fdiv-wb-class'], 'wb-classification')]
        for evidence, variants, stage in experiments:
            folder = root / 'evidence' / evidence
            source = json.loads((folder / 'source.json').read_text())
            launch = json.loads((folder / ('formal-launch.json' if formal else 'launch.json')).read_text())
            children = [dict(pid=launch['controller_pid'], start_ticks=launch['controller_start_ticks'])] if formal else launch['children']
            modes = [(variants[0], stage)] if formal else [(evidence, 'build'), *[(v, stage) for v in variants]]
            for original, (variant, mode) in zip(children, modes):
                state = next(s for s in snapshot['states'] if s['variant'] == variant and s['mode'] == mode)
                assert state['pid'] == original['pid'] and state['start_ticks'] == original['start_ticks']
                if mode == 'build':
                    assert state['source'] == source['head'] and state['root'] == launch['root']
                states.append(state)
            guests += [g for g in snapshot['guests'] if any('/' + stage + '/' + v + '/' in g['directory'] for v in variants)]
        assert len(states) == (1 if formal else 2 if query else 5)
        record.update(capture_passed=True, host=snapshot['host'], states=states, guests=guests,
                      original_processes_exited=all(not s['live'] for s in states))
    except (AssertionError, KeyError, ValueError, StopIteration, subprocess.TimeoutExpired) as error:
        record['observation_error'] = type(error).__name__ + ': ' + str(error)
    return record


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--watch', action='store_true', help='Repeat every60seconds until all original handles exit')
    parser.add_argument('--query', action='store_true', help='Observe only the independently launched query-only candidate')
    parser.add_argument('--query-formal', action='store_true', help='Observe the separately launched three formal windows')
    args = parser.parse_args(); root = args.report_dir.resolve()
    folder = root / ('evidence/wb-query-formal-tools' if args.query_formal else 'evidence/wb-query-process-watch-tools' if args.query else 'evidence/wb-process-watch-tools'); folder.mkdir(exist_ok=True)
    path = folder / 'process-watch-state.json'
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')', 1)[1].split()[19],
                 captures=0, observation_failures=0, source_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                 scope='Original node031 WB build/controllers and live guest process observations only. No database, load/idle sampling, simulation restart or performance inference. An observation failure cannot establish termination.')
    while True:
        observation = observe_once(root, args.query, args.query_formal)
        with (folder / 'process-events.jsonl').open('a') as stream:
            stream.write(json.dumps(observation) + '\n')
        state.update(utc=observation['utc'], phase='original_processes_exited' if observation['original_processes_exited'] else 'observing', last_observation=observation)
        state['captures' if observation['capture_passed'] else 'observation_failures'] += 1
        temporary = path.with_suffix('.tmp'); temporary.write_text(json.dumps(state, indent=2) + '\n'); temporary.replace(path)
        print(json.dumps({k:state[k] for k in ['utc', 'phase', 'captures', 'observation_failures']}), flush=True)
        if not args.watch or observation['original_processes_exited']:
            return
        time.sleep(60)


if __name__ == '__main__':
    main()
