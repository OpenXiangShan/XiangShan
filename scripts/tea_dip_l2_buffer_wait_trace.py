"""Attribute closed ReqBuf waitMS intervals to full MSHR release states.

Normal A requests are not necessarily ROB-head loads. Request cycles, CPU-time
unions, NoCommit overlap and sampled CPU states are distinct observations.
"""
import argparse
from bisect import bisect_left, bisect_right
from collections import Counter, defaultdict
import json
from pathlib import Path
import sqlite3

from native_perf import read_blocks
from tea_dip_clock_alignment import core_native_alignment
from tea_dip_gsim_l2_demand_route_probe import FIELDS as ROUTE_FIELDS
from tea_dip_gsim_l2_release_probe import FIELDS as RELEASE_FIELDS, SCHEDULE, WAIT
from tea_dip_l2_demand_route_trace import reconstruct
from tea_dip_l2_return_trace import read, owners
from tea_dip_query_endpoint_evidence import digest, verify


def analyze(folder, receipt_path):
  f, receipt_path = Path(folder), Path(receipt_path)
  receipt = json.loads(receipt_path.read_text())
  assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == f.name
  verify(f, receipt['files'])
  probe = json.loads((f/'probe-receipt.json').read_text())
  assert probe['closure_receipt_sha256'] == digest(receipt_path)
  verify(f, probe['files'])
  binding = json.loads((f/'probe-build-proof.json').read_text())
  assert binding['L2_release_binding']['fields'] == RELEASE_FIELDS
  assert binding['L2_demand_route_binding']['fields'] == ROUTE_FIELDS
  route,release,original=[read(f/n)for n in ['load-l2-demand-route.csv','load-l2-release.csv','load-l2-mshr.csv']];end=max(r['core_cycle']for r in original)+1
  buffers=reconstruct(route,end);mshrs,_,_=owners(original,end)
  def groups(rows,kind):
    out=defaultdict(list)
    for r in rows:
      if r['kind']==kind:out[r['bank'],r['mshr']].append(r)
    return {k:([r['core_cycle']for r in values],values)for k,values in out.items()}
  rg,og=groups(release,1),groups(original,1);assert len(rg)==len(og)==64
  mg={k:([o['begin']for o in epochs],epochs)for k,epochs in mshrs.items()}
  def at(group,cycle):
    times,rows=group;i=bisect_right(times,cycle)-1;assert i>=0;return rows[i]
  def merge(intervals):
    result=[]
    for lo,hi in sorted(intervals):
      assert lo<hi
      if result and lo<=result[-1][1]:result[-1][1]=max(result[-1][1],hi)
      else:result.append([lo,hi])
    return result
  def intersection(left,right):
    result=[];i=j=0
    while i<len(left)and j<len(right):
      a,b=left[i],right[j];lo=max(a[0],b[0]);hi=min(a[1],b[1])
      if lo<hi:result.append([lo,hi])
      if a[1]<=b[1]:i+=1
      else:j+=1
    return result
  def length(intervals):return sum(hi-lo for lo,hi in intervals)
  with sqlite3.connect((f/'simulator.db').resolve().as_uri()+'?mode=ro',uri=True)as db:
    db.row_factory=sqlite3.Row;clock=core_native_alignment(db,f/'simulator_err.txt');noc=merge([(r['START'],r['ENDCYCLE'])for r in db.execute('SELECT*FROM TeaNoCommit_0 ORDER BY START,ID')]);tea=list(db.execute('SELECT CYCLE,STATE FROM Tea_0 ORDER BY CYCLE,ID'))
  frames=read_blocks(f/'simulator_err.txt');assert len(frames)==2;roi_begin=min(frames)+1-clock['core_to_native_offset'];roi_end=max(frames)+1-clock['core_to_native_offset'];assert roi_end==end
  begin=min(r['core_cycle']for r in route if r['kind']==2);phases=[('warmup_observed',begin,roi_begin),('ROI',roi_begin,end)];release_flags={'w_replResp','s_release','w_releaseack','s_cbwrdata'}
  records={name:dict(begin=lo,end=hi,request_cycles=Counter(),normal_waitMP_and_waitMS=0,replacement_only_with_waitMP_request_cycles=0,replacement_only_intervals=[],free_edge_only_intervals=[],examples=[],unknown=[])for name,lo,hi in phases}
  for owner in buffers:
    states=owner['states'];is_hint=states[0]['opcode']==5
    for i,state in enumerate(states):
      mask=state['wait_ms']
      if not mask:continue
      lo=state['core_cycle'];hi=states[i+1]['core_cycle']if i+1<len(states)else owner['end'];assert lo<hi
      keys=[(owner['bank'],slot)for slot in range(16)if mask>>slot&1];cuts={lo,hi}
      for key in keys:
        for indexed in [rg[key],og[key]]:
          times=indexed[0];cuts.update(times[bisect_right(times,lo):bisect_left(times,hi)])
      cuts=sorted(cuts)
      for lo,hi in zip(cuts,cuts[1:]):
        labels=[];blockers=[]
        for key in keys:
          flags,old=at(rg[key],lo),at(og[key],lo);assert flags['valid']==old['valid']
          if flags['valid']:assert(old['refill'],old['grant'],old['last'])==(flags['s_refill'],flags['w_grant'],flags['w_grantlast'])
          pending=[n for n in SCHEDULE+WAIT if not flags[n]]
          if not flags['valid']:label='unknown_owner'
          elif not(old['has_data']and flags['w_grant']and flags['w_grantlast']and flags['s_refill']):label='grant_data_or_refill_pending'
          elif not pending:label='will_free_edge'
          elif set(pending)<=release_flags:label='replacement_pending'
          else:label='other_after_refill'
          labels.append(label)
          if flags['valid']:
            epoch=at(mg[key],lo);assert epoch['begin']<=lo<epoch['end']and epoch['line']==old['line']
            blockers.append(dict(bank=key[0],slot=key[1],generation=epoch['generation'],request_line=old['line'],same_request_line=old['line']==state['line'],is_prefetch=old['channel']==1 and old['opcode']==5,pending=pending))
        kinds=set(labels)
        category='replacement_only'if kinds<={'replacement_pending','will_free_edge'}and'replacement_pending'in kinds else'free_edge_only'if kinds=={'will_free_edge'}else'+'.join(sorted(kinds))
        role='Hint'if is_hint else'normal_A'
        for phase,low,high in phases:
          a,b=max(low,lo),min(high,hi)
          if a>=b:continue
          r=records[phase];r['request_cycles'][role+'/'+category]+=b-a
          if not is_hint:
            if state['wait_mp']:r['normal_waitMP_and_waitMS']+=b-a
            if category in ['replacement_only','free_edge_only']:r[category+'_intervals'].append([a,b])
            if category=='replacement_only'and state['wait_mp']:r['replacement_only_with_waitMP_request_cycles']+=b-a
            if category=='replacement_only':r['examples'].append(dict(begin=a,end=b,cycles=b-a,bank=owner['bank'],entry=owner['entry'],generation=owner['generation'],line=state['line'],blockers=blockers))
          if'unknown_owner'in kinds:r['unknown'].append(dict(begin=a,end=b,bank=owner['bank'],entry=owner['entry'],mask=mask))
  for phase,lo,hi in phases:
    r=records[phase];r['request_cycles']=dict(r['request_cycles']);expected=Counter()
    for owner in buffers:
      states=owner['states'];role='Hint'if states[0]['opcode']==5 else'normal_A'
      for i,state in enumerate(states):
        if state['wait_ms']:
          finish=states[i+1]['core_cycle']if i+1<len(states)else owner['end'];expected[role]+=max(0,min(hi,finish)-max(lo,state['core_cycle']))
    assert all(sum(n for k,n in r['request_cycles'].items()if k.startswith(role+'/'))==n for role,n in expected.items())
    r['total_waitMS_request_cycles']=dict(expected)
    for category in ['replacement_only','free_edge_only']:
      key=category+'_intervals';r[key]=merge(r[key]);r[category+'_CPU_union_cycles']=length(r[key]);overlap=intersection(r[key],noc);r[category+'_NoCommit_overlap_cycles']=length(overlap)
      starts=[a for a,b in r[key]];counts=Counter()
      for sample in tea:
        c=sample['CYCLE'];i=bisect_right(starts,c)-1
        if lo<=c<hi and i>=0 and c<r[key][i][1]:counts[str(sample['STATE'])]+=1
      r[category+'_TEA_sample_states']=dict(counts)
    r['observed_NoCommit_union_cycles']=length(intersection([[lo,hi]],noc));r['examples']=sorted(r['examples'],key=lambda x:x['cycles'],reverse=True)[:20]
  names = ['load-l2-demand-route.csv', 'load-l2-release.csv', 'load-l2-mshr.csv',
    'simulator.db', 'simulator_err.txt', 'probe-build-proof.json', 'probe-receipt.json']
  return dict(passed=True, point=f.name, source=receipt['source'], clock_alignment=clock,
    buffers=len(buffers), phases=records, receipt_sha256=digest(receipt_path),
    input_sha256={n:digest(f/n) for n in names}, tool_sha256=digest(Path(__file__)),
    scope='All waitMS request cycles partitioned by actual owner generations. Replacement-only '
    'qualifies waitMS owners; simultaneous waitMP and other ready conditions can remain. '
    'CPU unions and NoCommit intersections measure concurrency, not causal lost cycles. '
    's_refill means scheduled, not S3/data-write completion. Normal A inputs are not '
    'necessarily ROB-head loads. The warmup interval starts at the first captured core edge; '
    'unknown owners and unfinished generations are retained. No suite score is inferred.')


def main():
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument('--folder', type=Path, required=True)
  parser.add_argument('--receipt', type=Path, required=True)
  parser.add_argument('--output', type=Path)
  args = parser.parse_args()
  text = json.dumps(analyze(args.folder, args.receipt), indent=2)+'\n'
  if args.output: args.output.write_text(text)
  else: print(text, end='')


if __name__ == '__main__': main()
