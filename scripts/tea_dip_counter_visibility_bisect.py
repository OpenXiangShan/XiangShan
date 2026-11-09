"""Bisect one native counter's visibility using a frozen, validated micro runner."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import socket
import subprocess


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--plan', type=Path, required=True)
    args = parser.parse_args()
    plan = json.loads(args.plan.read_text())
    root = args.plan.resolve().parent
    assert socket.gethostname().startswith('node') and not (root/'run-state.json').exists()
    template = Path(plan['periodic_run'])
    upper_run = Path(plan['upper_run'])
    runner = (template/'run.py').read_bytes()
    assert sha(template/'run.py') == plan['runner_sha256']
    spec = json.loads((template/'experiment.json').read_text())
    audits = [json.loads((folder/'audit.json').read_text()) for folder in [template, upper_run]]
    for folder, audit in zip([template, upper_run], audits):
        state = json.loads((folder/'run-state.json').read_text())
        p = Path('/proc', str(state['pid']), 'stat')
        assert state['phase'] == 'complete' and (not p.exists() or p.read_text().rsplit(')',1)[1].split()[19] != state['start_ticks'])
        assert audit['passed'] and audit['complete_TEA_DIP_tables_equal'] and audit['diagnostic_totals_preserved']
        assert audit['totals'][plan['counter']] == 1
    index = spec['case']['command'].index('--stat-cycles') + 1
    periodic = audits[0]['new_diagnostics']
    first = next(i for i,row in enumerate(periodic) if row['counters'][plan['counter']])
    lower = periodic[first-1]['native_print_stamp']
    upper = next(row['native_print_stamp'] for row in audits[1]['new_diagnostics'] if row['counters'][plan['counter']])
    offset = periodic[0]['native_print_stamp'] - int(spec['case']['command'][index])
    assert first > 0 and lower < upper and offset >= 0
    state = dict(pid=os.getpid(), start_ticks=Path('/proc/self/stat').read_text().rsplit(')',1)[1].split()[19],
                 host=socket.gethostname(), phase='running', lower=lower, upper=upper, offset=offset, probes=[])
    def save(**values):
        state.update(values, utc=datetime.now(timezone.utc).isoformat())
        (root/'run-state.json').write_text(json.dumps(state,indent=2)+'\n')
    save()
    try:
        while upper-lower > 1:
            stamp = (lower+upper)//2
            folder = root/('stamp-'+str(stamp)); folder.mkdir()
            (folder/'run.py').write_bytes(runner)
            case = json.loads(json.dumps(spec)); case['run_dir'] = str(folder)
            case['case']['command'][index] = str(stamp-offset)
            case['scope'] = 'Independent counter-visibility probe; same frozen binary, image and dense sampler. No RTL/CI change.'
            (folder/'experiment.json').write_text(json.dumps(case,indent=2)+'\n')
            with (folder/'controller.stdout').open('w') as out, (folder/'controller.stderr').open('w') as err:
                child = subprocess.Popen(['python3',str(folder/'run.py')],cwd=folder,stdin=subprocess.DEVNULL,stdout=out,stderr=err)
                identity = dict(pid=child.pid,start_ticks=Path('/proc',str(child.pid),'stat').read_text().rsplit(')',1)[1].split()[19])
                save(current_probe=dict(stamp=stamp,folder=str(folder),process=identity))
                assert child.wait() == 0, str(folder)
            audit = json.loads((folder/'audit.json').read_text())
            assert audit['passed'] and audit['complete_TEA_DIP_tables_equal'] and audit['diagnostic_totals_preserved']
            observation = audit['new_diagnostics'][0]
            assert observation['native_print_stamp'] == stamp
            count = observation['counters'][plan['counter']]
            assert count in (0,1)
            if count: upper=stamp
            else: lower=stamp
            state['probes'].append(dict(stamp=stamp,count=count,folder=str(folder),process=identity,
                                        spec_sha256=sha(folder/'experiment.json'),audit_sha256=sha(folder/'audit.json')))
            save(lower=lower,upper=upper)
        save(phase='complete',current_probe=None,scope='Adjacent native print stamps bound counter visibility; not an exact IQ event cycle, FU acceptance or causal latency.')
    except BaseException as error:
        save(phase='failed',error=repr(error))
        raise


if __name__ == '__main__':
    main()
