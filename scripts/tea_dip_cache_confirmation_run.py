"""Confirm the cache pressure gain and WRF regression with longer, fresh paired windows."""
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from datetime import datetime,timezone
import json,os,socket,subprocess
from tea_dip_fdiv_ready_run import completed_guest
from tea_dip_cache_reservation_run import closed_budget
from tea_dip_wb_protocol_micro import digest,write
root=Path(__file__).resolve().parent
spec=json.loads((root/'experiment.json').read_text())
assert socket.gethostname().startswith('node')and not(root/'run-state.json').exists()
state=dict(pid=os.getpid(),start_ticks=Path('/proc/self/stat').read_text().rsplit(')',1)[1].split()[19],host=socket.gethostname(),boot_id=Path('/proc/sys/kernel/random/boot_id').read_text().strip(),phase='preflight',completed_guests=0,completed_pairs=0)
def save(**values):
 state.update(values,utc=datetime.now(timezone.utc).isoformat());write(root/'run-state.json',state)
save()
try:
 for name,expected in spec['input_sha256'].items():assert digest(name)==expected,name
 for variant,build in spec['builds'].items():
  assert digest(build['binary'])==build['binary_sha256']
  assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=build['root'],text=True).strip()==build['source']
  (root/variant).mkdir()
 assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=Path(spec['builds']['candidate']['root'])/'XSCache',text=True).strip()==spec['cache_source']
 for point,item in spec['prerequisites'].items():
  audit=json.loads((root/item['analysis']).read_text());receipt=json.loads((root/item['receipt']).read_text());assert audit['passed']and receipt['ready']and receipt['all_stage_guests_closed']
  assert digest(root/item['receipt'])==audit['stage_receipt_sha256']
  for name,entry in receipt['files'].items():
   path=Path(receipt['root'])/name;assert digest(path)==entry['sha256'],path
 assert json.loads((root/'prerequisites/mcf12886-stage-validation.json').read_text())['observed_ROI_speedup']>0
 assert json.loads((root/'prerequisites/wrf20161-stage-validation.json').read_text())['observed_ROI_speedup']<0
 save(phase='two_paired_1m_windows')
 def execute(job):
  variant,case=job;folder=root/variant/case['point'];result=completed_guest(folder,spec['builds'][variant],dict(case,core=case[variant+'_core']));budget=closed_budget(folder,result)
  return variant,case['point'],dict(result=result,budget=budget)
 jobs=[(variant,case)for case in spec['cases']for variant in spec['builds']];results={};pairs={}
 with ThreadPoolExecutor(max_workers=4)as pool:
  for future in as_completed([pool.submit(execute,job)for job in jobs]):
   variant,point,item=future.result();results.setdefault(point,{})[variant]=item
   write(root/'closed-guest-budgets.json',results);save(completed_guests=state['completed_guests']+1)
   if len(results[point])==2:
    before,after=[results[point][v]['budget']for v in ['control','candidate']]
    pairs[point]=dict(rows=results[point],observed_speedup=before['native_CPI']/after['native_CPI']-1,delta_CPI=after['native_CPI']-before['native_CPI'],DIP_delta_CPI={k:after['DIP_CPI'][k]-before['DIP_CPI'][k]for k in after['DIP_CPI']})
    write(root/'pair-results.json',pairs);save(completed_pairs=len(pairs))
 save(phase='complete',all_guests_Difftest_passed=True)
except BaseException as error:
 save(phase='failed',error=repr(error));raise
