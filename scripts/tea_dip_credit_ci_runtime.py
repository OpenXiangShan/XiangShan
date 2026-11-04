"""Observe one of the two fixed-resource credit CIs without reading live DBs."""
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path
import argparse
import json
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--report-dir', type=Path, required=True)
parser.add_argument('--ci-evidence', choices=['ci-credit-div','ci-fdiv-credit'], required=True)
args = parser.parse_args()
R = args.report_dir.resolve()
O = R / 'evidence' / args.ci_evidence
identity = json.loads((O / 'archive-identity.json').read_text())
run = json.loads((O / 'run.json').read_text())
jobs = json.loads((O / 'jobs.json').read_text())
assert identity['run_id'] == run['id'] and identity['source'] == run['head_sha']
assert len(jobs['jobs']) == 1 and jobs['jobs'][0]['run_id'] == run['id']
runner_host = jobs['jobs'][0]['runner_name'].removeprefix('xs-perf-').rsplit('-', 1)[0]
head = identity['source']
code = r'''from pathlib import Path
import json
root=Path(REPORT)
text=(root/RUNNER).read_text()
print(json.dumps(dict(tail=text[-8000:],server_lines=[line for line in text.splitlines() if 'Server list:' in line],retry_lines=[line for line in text.splitlines() if any(s in line for s in ['failed','retrying','resetting'])])))
'''.replace('REPORT', repr(identity['report'])).replace('RUNNER', repr(identity['runner_log']))

def remote(host, script):
    result = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', '-o', 'ConnectTimeout=15', host, 'python3', '-'],
                            input=script, text=True, capture_output=True, timeout=50, check=True)
    return json.loads(result.stdout)

runner = remote(runner_host, code)
workers = runner['server_lines'][-1].split('Server list: ', 1)[1].split(', ')
code = r'''"""Read the current CI's actual SPEC guest/controller processes on one node."""
from pathlib import Path
from datetime import datetime, timezone
import json
import os
import re
import socket

run=dict(id=RUN_ID)
report=Path(REPORT)
binary = str(report / 'emu-gsim')
processes = []
for proc in Path('/proc').glob('[0-9]*'):
    try:
        args = (proc / 'cmdline').read_bytes().decode(errors='replace').split('\0')[:-1]
        guest = bool(args) and args[0] == binary
        controller = '--result-path' in args and str(report) in args and '--run' in args
        if not guest and not controller:
            continue
        stat = (proc / 'stat').read_text().rsplit(')', 1)[1].split()
        row = dict(pid=int(proc.name), start_ticks=stat[19], state=stat[0],
                   cpu_ticks=int(stat[11]) + int(stat[12]), affinity=sorted(os.sched_getaffinity(int(proc.name))),
                   role='guest' if guest else 'controller', args=args)
        if guest:
            point = Path(args[args.index('--db-path') + 1]).parent
            output = point / 'simulator_out.txt'
            text = output.read_text(errors='replace')
            row.update(point=point.name, stdout_bytes=output.stat().st_size,
                       difftest_requested='--diff' in args, difftest_announced='Difftest enabled' in text,
                       tea_enabled=bool(re.search(r'^enableTea0 = 1$', text, re.M)),
                       dip_enabled=bool(re.search(r'^enableDip0 = 1$', text, re.M)),
                       warmup_announced='Warmup finished' in text,
                       limit_announced='EXCEEDING CYCLE/INSTR LIMIT' in text, critical='HIT CRITICAL' in text)
        processes.append(row)
    except (FileNotFoundError, PermissionError, ProcessLookupError):
        continue
print(json.dumps(dict(utc=datetime.now(timezone.utc).isoformat(), run_id=run['id'],
                      host=socket.gethostname(),
                      processes=processes, scope='Live argv and process identity; no unfinished database reads'), indent=2))
'''.replace('REPORT', repr(identity['report'])).replace('RUN_ID', repr(identity['run_id']))
code = code.replace("warmup_announced='Warmup finished' in text,", "tea_period_1000=bool(re.search(r'^teaSamplePeriod0 = 1000$', text, re.M)), dip_period_1000=bool(re.search(r'^dipSamplePeriod0 = 1000$', text, re.M)), warmup_announced='Warmup finished' in text,")
with ThreadPoolExecutor(max_workers=len(workers) + 1) as pool:
    observations = list(pool.map(lambda host: remote(host, code), [*workers, runner_host]))
stamp = datetime.now(timezone.utc).isoformat()
result = dict(utc=stamp, run_id=identity['run_id'], source=head, nodes=observations[:-1], controller=observations[-1],
              scope='Worker list observed from the original auto-scheduled runner. Original process identities and stdout flags only; no host-load sampling, unfinished DB reads or inferred guest CPI.')
seeds = json.loads((O / 'inputs/spec06-cov03-seeds.json').read_text())
controllers = result['controller']['processes']
assert len(controllers) == 1
controller = controllers[0]['args']
assert controller[controller.index('--threads') + 1] == '1' and controller[controller.index('--server-list') + 1] == 'node'
checks = []
for node in result['nodes']:
    for guest in node['processes']:
        if guest['role'] != 'guest':
            continue
        argv = guest['args']
        for option, value in [('-W', '20000000'), ('-I', '40000000'), ('-s', str(seeds[guest['point']])),
                              ('--diff', identity['report'] + '/riscv64-nemu-interpreter-so')]:
            assert argv[argv.index(option) + 1] == value
        assert set(argv[argv.index('--dump-select-db') + 1].split()) == {'Tea_0','TeaDelayedFinal_0','TeaCycleSummary_0','TeaNoCommit_0','Dip_0','DipDelayedFinal_0'}
        assert len(guest['affinity']) == 1 and all(guest[k] for k in ['tea_enabled','dip_enabled','tea_period_1000','dip_period_1000'])
        checks.append(dict(host=node['host'], point=guest['point'], pid=guest['pid'], start_ticks=guest['start_ticks'], requested_configuration_matches=True,
                           difftest_announced=guest['difftest_announced']))
(O / 'runtime-check.json').write_text(json.dumps(dict(utc=stamp, run_id=identity['run_id'], checks=checks,
    controller_single_thread_and_default_node_pool=True, completed_spec_difftest_validated=None,
    scope='Observed argv, affinity and sampler stdout only. Difftest is requested; a missing announcement in buffered live stdout is unknown, not a pass or failure. No SPEC DB read.'), indent=2) + '\n')
(O / 'runtime-live.json').write_text(json.dumps(result, indent=2) + '\n')
(O / 'runner-last-tail.txt').write_text(runner['tail'])
(O / 'runner-network-retries.json').write_text(json.dumps(dict(utc=stamp, run_id=identity['run_id'], lines=runner['retry_lines'],
    scope='Original platform transport failures and automatic retries; preserved as infrastructure evidence, not new user dispatches or measured RTL failures.'), indent=2) + '\n')
with (O / 'resource-events.jsonl').open('a') as output:
    output.write(json.dumps(result) + '\n')
print(json.dumps(dict(utc=stamp, workers=[dict(host=n['host'], guests=len([p for p in n['processes'] if p['role']=='guest'])) for n in result['nodes']],
                     retry_lines=len(runner['retry_lines']), controller_processes=len(result['controller']['processes'])), indent=2))
