"""Observe only the declared Soplex stage cases, preserving original process identities."""
from pathlib import Path
from datetime import datetime, timezone
import argparse
import json
import subprocess
from tea_dip_fdiv_ready_batch_evidence import metadata
from tea_dip_query_endpoint_evidence import digest


def observe(root):
    launch, _, _ = metadata(root, 'soplex12322', 'candidate', 'soplex_pds-50_12322_0.090454', True)
    host = launch['process']['host'].split('.')[0]
    code = 'launch=' + repr(launch) + '\n' + r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json,socket
root=Path(launch['run_dir'])
assert socket.gethostname().split('.')[0]==launch['process']['host'].split('.')[0]
assert hashlib.sha256((root/'experiment.json').read_bytes()).hexdigest()==launch['spec_sha256']
assert json.loads((root/'experiment.json').read_text())==launch['spec']
def process(identity):
 p=Path('/proc',str(identity['pid']),'stat')
 if not p.exists():return dict(original_live=False)
 fields=p.read_text().rsplit(')',1)[1].split()
 return dict(original_live=fields[19]==str(identity['start_ticks']),cpu_ticks=int(fields[11])+int(fields[12]),state=fields[0])
state=json.loads((root/'run-state.json').read_text())
assert all(str(state[k])==str(launch['process'][k])for k in ['pid','start_ticks'])
cases=[]
for case in launch['spec']['cases']:
 folder=root/case['variant']/case['point'];row=dict(variant=case['variant'],point=case['point'],started=(folder/'process.json').exists())
 if row['started']:
  identity=json.loads((folder/'process.json').read_text());row.update(identity=identity,process=process(identity),result_exists=(folder/'result.json').exists())
  if row['result_exists']:row['result']=json.loads((folder/'result.json').read_text())
 cases.append(row)
print(json.dumps(dict(utc=datetime.now(timezone.utc).isoformat(),state=state,controller=process(launch['process']),cases=cases,stderr=(root/'controller.stderr').read_text()[-3000:],database_read=False)))
'''
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', host, 'python3', '-'],
                           input=code, capture_output=True, text=True, check=True, timeout=50)
    value = json.loads(reply.stdout)
    value.update(tool_sha256=digest(Path(__file__)), experiment_sha256=launch['spec_sha256'],
                 scope='Only the two manifest-declared cases are observed. Auxiliary process records are not RTL guests. No live SQLite, simulation restart or CI action.')
    return value


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    args = parser.parse_args()
    root = args.report_dir.resolve()
    value = observe(root)
    target = root/'evidence/fdiv-turnover-ci-soplex-20261108'/('stage-registered-runtime-' + datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%S') + '.json')
    assert not target.exists()
    target.write_text(json.dumps(value, indent=2) + '\n')
    print(target)
    print(json.dumps(value, indent=2))


if __name__ == '__main__':
    main()
