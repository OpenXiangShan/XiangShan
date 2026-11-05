from pathlib import Path
import json,os,socket,subprocess,time
E=Path('/nfs/home/wujiabin/work/xs-env/rtl-dip-experiments-20261103')
result=dict(host=socket.gethostname(),states=[],guests=[])
for v in ['control','fpiq24','fprank','fpreg288','fpiq24-fpreg288','fpiq24-fpreg320','fcvt-fp2','rebuild-control','fpiq24-div','fpiq24-fpreg288-rab384','fpiq24-fex123','fpiq24-div-observe','fpiq24-fex123-observe','gate-observe','gate-observe-v2','fp-credit','fp-credit-guard','fp-credit-div','fp-credit-fex123','fp-timestamps-observe','fp-credit-fdiv-only','fp-wb-intervals-observe','rob-wb-provenance-observe','rob-wb-sequence-observe','rob-retired-owner-observe','lrq-nearhead-order','lrq-nearhead-observe','lrq-stall-observe','load-replay-wait-observe','fp-wb-source-observe']:
 for mode in ['build','short']:
  p=E/v/(mode+'-state.json')
  if not p.exists():continue
  s=json.loads(p.read_text())
  if s['host']!=socket.gethostname():continue
  proc=Path('/proc',str(s['pid']),'stat')
  s['live']=proc.exists() and proc.read_text().rsplit(')',1)[1].split()[19]==s['start_ticks']
  s['variant']=v;s['mode']=mode
  logfile=E/v/('build.stdout' if mode=='build' else 'short-controller.log')
  if logfile.exists():s['log_tail']=logfile.read_text(errors='replace')[-1400:]
  result['states'].append(s)
 for p in (E/v).glob('**/process.json'):
  s=json.loads(p.read_text())
  if s['host']!=socket.gethostname():continue
  proc=Path('/proc',str(s['pid']),'stat')
  fields=proc.read_text().rsplit(')',1)[1].split() if proc.exists() else None
  s['live']=fields is not None and fields[19]==s['start_ticks']
  if s['live']:
   s['cpu_ticks']=int(fields[11])+int(fields[12])
   s['directory']=str(p.parent);s['affinity']=sorted(os.sched_getaffinity(s['pid']));s['ps']=subprocess.check_output(['ps','-p',str(s['pid']),'-o','pid,ppid,pcpu,pmem,etime,args'],text=True).strip()
   result['guests'].append(s)
for path in [*(E/'formal').glob('*/run-state.json'),*(E/'repeat').glob('*/run-state.json'),*(E/'sampling').glob('*/run-state.json'),*(E/'confirm').glob('*/run-state.json'),*(E/'queue-observation').glob('*/run-state.json'),*(E/'ci-followup').glob('*/run-state.json'),*(E/'soplex-followup').glob('*/run-state.json'),*(E/'gate-observation').glob('*/run-state.json'),*(E/'gate-observation-v2').glob('*/run-state.json'),*(E/'fp-timestamp-observation').glob('*/run-state.json'),*(E/'fp-wb-interval-observation').glob('*/run-state.json'),*(E/'rob-wb-provenance-observation').glob('*/run-state.json'),*(E/'rob-wb-sequence-observation').glob('*/run-state.json'),*(E/'rob-retired-owner-observation').glob('*/run-state.json'),*(E/'lrq-nearhead-observation').glob('*/run-state.json'),*(E/'lrq-stall-observation').glob('*/run-state.json'),*(E/'load-replay-wait-observation').glob('*/run-state.json'),*(E/'stream-depth-policy').glob('*/run-state.json'),*(E/'stream-policy-gate-observation').glob('*/run-state.json'),*(E/'fp-wb-source-observation').glob('*/run-state.json')]:
 s=json.loads(path.read_text())
 if s['host']!=socket.gethostname():continue
 proc=Path('/proc',str(s['pid']),'stat')
 s['live']=proc.exists() and proc.read_text().rsplit(')',1)[1].split()[19]==s['start_ticks']
 s['mode']=path.parent.parent.name;s['variant']=path.parent.name
 log=path.parent/'controller.log'
 if log.exists():s['log_tail']=log.read_text(errors='replace')[-1400:]
 result['states'].append(s)
 for p in [*(path.parent/'points').glob('*/process.json'),*(path.parent/'coremark').glob('process.json')]:
  guest=json.loads(p.read_text());proc=Path('/proc',str(guest['pid']),'stat')
  fields=proc.read_text().rsplit(')',1)[1].split() if proc.exists() else None
  guest['live']=fields is not None and fields[19]==guest['start_ticks']
  if guest['live']:
   guest['cpu_ticks']=int(fields[11])+int(fields[12])
   guest['directory']=str(p.parent);guest['affinity']=sorted(os.sched_getaffinity(guest['pid']))
   guest['ps']=subprocess.check_output(['ps','-p',str(guest['pid']),'-o','pid,ppid,pcpu,pmem,etime,args'],text=True).strip()
   result['guests'].append(guest)
for path in [*(E/'micro').glob('*/run-state.json'),*(E/'micro-v2').glob('*/run-state.json')]:
 s=json.loads(path.read_text())
 if s['host']!=socket.gethostname():continue
 proc=Path('/proc',str(s['pid']),'stat')
 s['live']=proc.exists() and proc.read_text().rsplit(')',1)[1].split()[19]==s['start_ticks']
 s['mode']=path.parent.parent.name;result['states'].append(s)
 if 'guest_pid' in s:
  proc=Path('/proc',str(s['guest_pid']),'stat')
  if proc.exists() and proc.read_text().rsplit(')',1)[1].split()[19]==s['guest_start_ticks']:
   fields=proc.read_text().rsplit(')',1)[1].split()
   result['guests'].append(dict(pid=s['guest_pid'],start_ticks=s['guest_start_ticks'],cpu_ticks=int(fields[11])+int(fields[12]),directory=str(path.parent),live=True,ps=subprocess.check_output(['ps','-p',str(s['guest_pid']),'-o','pid,ppid,pcpu,pmem,etime,args'],text=True).strip()))
result['clock_ticks_per_second']=os.sysconf('SC_CLK_TCK')
result['monotonic_seconds']=time.monotonic()
print(json.dumps(result,indent=2))
