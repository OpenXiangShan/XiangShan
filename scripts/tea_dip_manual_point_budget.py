"""Budget naturally closed manual GSIM windows at the default1000-cycle sampling.

Keep the strict20M+20M formal-CI gate in tea_dip_ci_point_budget unchanged.
This is the existing focused-window budget exported for conditional experiments.
"""
from contextlib import closing
from fractions import Fraction
import json
import sqlite3

import dip_analyze as dip
from native_perf import read_blocks
from tea_dip_clock_alignment import core_native_alignment
from tea_dip_query_endpoint_evidence import verify, endpoint


def budget(folder):
 receipt=json.loads((folder/'closure-receipt.json').read_text());result=json.loads((folder/'result.json').read_text());assert receipt['ready']and not receipt['original_process_live']and result['original_process_closed']and result['difftest_passed']and result['exitcode']==0;verify(folder,receipt['files'])
 blocks=read_blocks(folder/'simulator_err.txt');roi=blocks[max(blocks)]
 def scalar(suffix):values,=[v for k,v in roi.items()if k.endswith(suffix)];value,=values;return value
 cycles,retired=scalar('.rob.clock_cycle'),scalar('.rob.commitInstr');cpi=Fraction(cycles,retired);exact=endpoint(folder)if len(blocks)==2 else None
 if exact:assert sum(exact['roi']['states'].values())==cycles
 with closing(sqlite3.connect((folder/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True))as db:
  db.row_factory=sqlite3.Row;clock=core_native_alignment(db,folder/'simulator_err.txt');offset=clock['core_to_native_offset'];begin=min(blocks)+1-offset if len(blocks)==2 else-offset;end=max(blocks)+1-offset
  expected=list(range(max(0,((begin+999)//1000)*1000),end,1000))
  for table in ['Tea_0','Dip_0']:assert[r[0]for r in db.execute('SELECT CYCLE FROM '+table+' WHERE CYCLE>=?AND CYCLE<? ORDER BY CYCLE',(begin,end))]==expected
  width=sum(r['name'].startswith('SLOTS_')and r['name'].endswith('_STATE')for r in db.execute('PRAGMA table_info(Dip_0)'))
  _,summary=dip.analyze(db.execute('SELECT*FROM Dip_0 WHERE CYCLE>=?AND CYCLE<? ORDER BY CYCLE,ID',(begin,end)),db.execute('SELECT*FROM DipDelayedFinal_0 ORDER BY CYCLE,CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID'),width)
  tea=dict(db.execute('SELECT STATE,count(*) FROM Tea_0 WHERE CYCLE>=?AND CYCLE<? GROUP BY STATE',(begin,end)))
 count=sum(tea.values());assert count==summary['sampled_cycles']and count>0
 states=['Computing','Stalled','Flushed','Drained'];sampled={k:summary['state_cycles'][k]/count*float(cpi)for k in dip.STATES};sampled['Unresolved']=summary['unresolved_slots']/summary['sampled_slots']*float(cpi);assert abs(sum(sampled.values())-float(cpi))<1e-9
 return dict(cycles=cycles,retired=retired,CPI=float(cpi),exact_TEA_states=exact['roi']['states']if exact else None,DIP_CPI=sampled,sampled_TEA_CPI={s:tea.get(i,0)/count*float(cpi)for i,s in enumerate(states)},sampled_cycles=count,clock_alignment=clock),receipt
