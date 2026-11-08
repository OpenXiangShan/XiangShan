"""Bind the running turnover CI to its actual GSIM build and complete input profile."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import subprocess


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    args = parser.parse_args()
    root = args.report_dir.resolve()
    evidence = root / 'evidence/fdiv-turnover-ci-20261108'
    identity_path = evidence / 'archive-identity.json'
    identity = json.loads(identity_path.read_text())
    source = json.loads((root / 'evidence/fdiv-turnover-20261108/source.json').read_text())
    runtime = json.loads((evidence / 'runtime-live.json').read_text())
    assert identity['run_id'] == runtime['run_id'] == 42468823909
    assert identity['source'] == runtime['source'] == '8130b0ab046467542cc7c274076c436488cb86fd'
    controllers = runtime['controller']['processes']
    assert len(controllers) == 1 and controllers[0]['role'] == 'controller'
    controller = controllers[0]
    profile_path = root.parents[1] / 'ci-41413249290/raw/profile.json'
    profile = json.loads(profile_path.read_text())
    seeds = json.loads((evidence / 'inputs/spec06-cov03-seeds.json').read_text())
    points = {f'{name}_{point}_{float(weight):.6f}'
              for name, workload in profile.items() for point, weight in workload['points'].items()}
    assert len(points) == 145 and points == seeds.keys()
    code = r'''from pathlib import Path
from datetime import datetime,timezone
import hashlib,json,subprocess
identity=IDENTITY;source=SOURCE;controller=CONTROLLER;profile=PROFILE
root=Path(identity['report']);workspace=Path(identity['workspace'])
def git(path,*args):
 return subprocess.check_output(['git','-c','safe.directory='+str(path),'-C',str(path),*args],text=True).strip()
def digest(path):
 with path.open('rb')as stream:return hashlib.file_digest(stream,'sha256').hexdigest()
assert git(workspace,'rev-parse','HEAD')==identity['source']
assert not git(workspace,'diff',source['head'],'--','src')
assert git(workspace/'yunsuan','rev-parse','HEAD')==source['yunsuan_head']
assert not git(workspace/'yunsuan','diff','HEAD','--','src')
proc=Path('/proc',str(controller['pid']))
assert (proc/'stat').read_text().rsplit(')',1)[1].split()[19]==controller['start_ticks']
argv=(proc/'cmdline').read_bytes().decode().split(chr(0))[:-1]
assert argv==controller['args']
selected=Path(argv[argv.index('--json-path')+1])
assert json.loads(selected.read_text())==profile
assert argv[argv.index('--emu-path')+1]==str(root/'emu-gsim')
artifacts=dict(emu=digest(root/'emu-gsim'),reference=digest(root/'riscv64-nemu-interpreter-so'),cst=digest(Path(argv[argv.index('--cst-file')+1])))
assert artifacts['emu']==digest(workspace/'build/gsim-compile/emu')==digest(workspace/'build/emu')
assert artifacts['reference']==identity['reference_sha256']and artifacts['cst']==identity['cst_sha256']
assert identity['emu_sha256']in [None,artifacts['emu']]
audit_path=root/'pgo-training-audit.json';audit=json.loads(audit_path.read_text())
assert audit['source']==identity['source']and audit['periods']==dict(tea=1000,dip=1000)
assert set(audit['tables'])==set(argv[argv.index('--dump-select-db')+1].split())and all(audit['tables'].values())
assert audit['stdout_sha256']==digest(root/'pgo-training.log')
print(json.dumps(dict(utc=datetime.now(timezone.utc).isoformat(),passed=True,run_id=identity['run_id'],source=identity['source'],workspace=str(workspace),RTL_matches_tested_source=source['head'],actual_yunsuan_head=source['yunsuan_head'],controller=controller,artifact_sha256=artifacts,selected_profile_path=str(selected),selected_profile_sha256=digest(selected),selected_profile=profile,pgo_training_audit=audit,pgo_training_audit_sha256=digest(audit_path),database_read=False)))
'''.replace('IDENTITY', repr(identity)).replace('SOURCE', repr(source)).replace('CONTROLLER', repr(controller)).replace('PROFILE', repr(profile))
    host = runtime['controller']['host'].split('.')[0]
    reply = subprocess.run(['proxychains', '-q', 'ssh', '-o', 'BatchMode=yes', host, 'python3', '-'],
                           input=code, text=True, capture_output=True, check=True, timeout=50)
    result = json.loads(reply.stdout)
    assert result['passed'] and result['selected_profile'] == profile
    result.update(expected_points=145, actual_profile_matches_baseline=True,
                  all_profile_points_have_fixed_seeds=True,
                  consumer_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                  scope='Actual running CI controller, GSIM executable, source and complete input profile only. PGO sampler audit is separate from SPEC completion; no live SQLite reads, suite score or adoption claim.')
    # Preserve the original unbound identity before adding the observed executable SHA.
    before = evidence / 'archive-identity-before-artifact-verification.json'
    if not before.exists():
        before.write_bytes(identity_path.read_bytes())
    (evidence / 'actual-artifact-verification.json').write_text(json.dumps(result, indent=2) + '\n')
    identity['emu_sha256'] = result['artifact_sha256']['emu']
    identity_path.write_text(json.dumps(identity, indent=2) + '\n')
    print(json.dumps({key: result[key] for key in ['utc', 'passed', 'run_id', 'artifact_sha256', 'expected_points', 'scope']}, indent=2))


if __name__ == '__main__':
    main()
