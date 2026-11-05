"""Copy only closed, successful node runs; preserve raw bytes and compare hashes."""
import argparse,hashlib,json,shlex,subprocess,tarfile
from datetime import datetime,timezone
from pathlib import Path
HERE=Path(__file__).resolve().parent
parser=argparse.ArgumentParser();parser.add_argument('--stage',choices=['short','formal','repeat','micro','micro-v2','sampling','confirm','queue-observation','ci-followup','soplex-followup','gate-observation','gate-observation-v2','fp-timestamp-observation','fp-wb-interval-observation','rob-wb-provenance-observation','rob-wb-sequence-observation','rob-retired-owner-observation','lrq-nearhead-observation','lrq-stall-observation','load-replay-wait-observation','stream-depth-policy','stream-policy-gate-observation','fp-wb-source-observation'],default='short');parser.add_argument('--report-dir',type=Path,default=HERE);args=parser.parse_args();HERE=args.report_dir.resolve()
DEST=HERE/('raw' if args.stage=='short' else args.stage)
REMOTE='/nfs/home/wujiabin/work/xs-env/rtl-dip-experiments-20261103'
if args.stage!='short':REMOTE+='/'+args.stage
FILES=('command.json','result.json','counters.json','exitcode','simulator_out.txt','simulator_err.txt','simulator.db','process.json','run-state.json','probe-evidence.json')
manifest_path=DEST/'manifest.json'
manifest=json.loads(manifest_path.read_text()) if manifest_path.exists() else {}
code='''from pathlib import Path
import hashlib,json
E=Path(REMOTE)
rows={}
for v in ['control','fpiq24','fprank','fpreg288','fpiq24-fpreg288','fpiq24-fpreg320','fcvt-fp2','rebuild-control','fpiq24-div','fpiq24-fpreg288-rab384','fpiq24-fex123','fpiq24-div-observe','fpiq24-fex123-observe','gate-observe','gate-observe-v2','fp-credit','fp-credit-guard','fp-credit-div','fp-credit-fex123','fp-timestamps-observe','fp-credit-fdiv-only','fp-wb-intervals-observe','rob-wb-provenance-observe','rob-wb-sequence-observe','rob-retired-owner-observe','lrq-nearhead-order','lrq-nearhead-observe','lrq-stall-observe','load-replay-wait-observe','stream-l1-depth32','fp-wb-source-observe','fp-wb-source-observe-depth32']:
 if OBSERVER:
  receipt=E/v/'results.json'
  ready_points={row['point'] for row in json.loads(receipt.read_text())} if receipt.exists() else set()
 for p in ([E/v] if MICRO else (E/v/SUBDIR).glob('*')):
  if OBSERVER and p.name not in ready_points:continue
  closed=p/('run-state.json' if MICRO else 'result.json')
  if not closed.exists() or (MICRO and json.loads(closed.read_text())['phase']!='complete'):continue
  if (p/'exitcode').read_text().strip()!='0':continue
  for name in FILES:
   f=p/name
   if f.exists():
    with f.open('rb') as stream:digest=hashlib.file_digest(stream,'sha256').hexdigest()
    rows[str(f.relative_to(E))]=dict(bytes=f.stat().st_size,sha256=digest)
print(json.dumps(rows))
'''.replace('REMOTE',repr(REMOTE)).replace('FILES',repr(FILES)).replace('SUBDIR',repr('short' if args.stage=='short' else 'points')).replace('MICRO',repr(args.stage in ('micro','micro-v2'))).replace('OBSERVER',repr(args.stage in ('gate-observation','gate-observation-v2','fp-timestamp-observation','fp-wb-interval-observation','rob-wb-provenance-observation','rob-wb-sequence-observation','rob-retired-owner-observation','lrq-nearhead-observation','lrq-stall-observation','load-replay-wait-observation','stream-depth-policy','stream-policy-gate-observation','fp-wb-source-observation')))
response=subprocess.run(['proxychains','-q','ssh','-o','BatchMode=yes','node032','python3 -'],input=code,capture_output=True,text=True,check=True)
rows=json.loads(response.stdout)
new=[]
for name,entry in rows.items():
 if name in manifest:
  assert entry==manifest[name],('closed raw changed',name)
 else:new.append(name)
if new:
 stamp=datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%SZ')
 archives=DEST/'archives';archives.mkdir(parents=True,exist_ok=True)
 archive=archives/(stamp+'.tar.gz')
 command='tar -C '+shlex.quote(REMOTE)+' -czf - '+' '.join(shlex.quote(p) for p in new)
 with archive.open('wb') as output:subprocess.run(['proxychains','-q','ssh','node032',command],stdout=output,check=True)
 with tarfile.open(archive) as tf:
  assert {m.name for m in tf.getmembers()}==set(new)
  tf.extractall(DEST,filter='data')
 for name in new:
  p=DEST/name
  with p.open('rb') as stream:digest=hashlib.file_digest(stream,'sha256').hexdigest()
  assert digest==rows[name]['sha256'] and p.stat().st_size==rows[name]['bytes'],name
 manifest.update(rows);manifest_path.write_text(json.dumps(manifest,indent=2))
 print('Collected',len(new),'files in',archive.name)
else:print('No newly completed files')
print('Closed cases:',sorted({str(Path(p).parent) for p in rows if Path(p).name in ('result.json','run-state.json')}))
