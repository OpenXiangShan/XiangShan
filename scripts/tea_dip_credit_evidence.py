"""Keep each closed slot-reuse result, its pressure shifts and evidence limits."""
from pathlib import Path
import argparse
import csv
import hashlib
import json
import re

from native_perf import read_blocks, unique_counters

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--variant', choices=['fp-credit', 'fp-credit-guard', 'fp-credit-div', 'fp-credit-fex123', 'fp-credit-fdiv-only'], default='fp-credit')
parser.add_argument('--report-dir', type=Path, help='Experiment report containing closed raw/analysis inputs')
args = parser.parse_args()
variant = args.variant
ROOT = args.report_dir if args.report_dir is not None else Path(__file__).resolve().parent
OUT = ROOT / 'raw/analysis'
summary = json.loads((OUT / 'summary.json').read_text())
gaps = list(csv.DictReader((OUT / 'retirement-gap-deltas.csv').open()))
pcs = list(csv.DictReader((OUT / 'pc-deltas.csv').open()))
rows = []
for pair in summary['comparisons']:
    if pair['variant'] != variant:
        continue
    point = pair['point']
    a, b = [json.loads((OUT / (observed_variant + '-' + point + '.json')).read_text()) for observed_variant in ['control', variant]]
    exclusive = {state: b['metrics']['DIP_' + state] - a['metrics']['DIP_' + state]
                 for state in ['Base', 'Stall', 'FrontEnd', 'Misspeculation', 'Unresolved']}
    assert abs(sum(exclusive.values()) - pair['delta_cpi']) < 1e-12
    context = []
    for observed_variant in ['control', variant]:
        path = ROOT / 'raw' / observed_variant / 'short' / point / 'simulator_err.txt'
        blocks = read_blocks(path)
        native = unique_counters(blocks[max(blocks)])
        entry_exposure = {}
        for suffix in ['load_miss_penalty_to_use', 'penalty_waiting_for_channel_D']:
            selected = {key:value for key,value in native.items() if re.search(r'\.missQueue\.entries_\d+\.'+suffix+'$',key)}
            assert len(selected) == 16, (point, variant, suffix, len(selected))
            entry_exposure[suffix] = dict(instance_values=selected, total_entry_cycles=sum(selected.values()))
        replay = {key:value for key,value in native.items() if key.endswith('.loadQueueReplay.replay_allocate')}
        assert len(replay) == 1
        prefetch_labels = {key:value for key,value in native.items() if '.topDown.E2_L2AReqSource_' in key and
                           any(key.endswith(source+'_'+kind) for source in ['Prefetch2L2Stream','Prefetch2L2CDP','CPULoadData'] for kind in ['Miss','Total'])}
        context.append(dict(variant=observed_variant,entry_exposure=entry_exposure,replay_allocate=next(iter(replay.values())),
                            printed_prefetch_labels=prefetch_labels,raw_sha256=hashlib.sha256(path.read_bytes()).hexdigest()))
    selected_pcs = [{**row, **{key:float(row[key]) for key in ['control_cpi','candidate_cpi','delta_cpi']}}
                    for row in pcs if row['variant']==variant and row['point']==point]
    interior = [window['exact_tea_interior'] for window in [a,b]]
    rows.append(dict(comparison=pair,cycles=[a['cycles'],b['cycles']],instructions=[a['instructions'],b['instructions']],
                     exact_interiors=interior,exact_state_delta_cpi={state:interior[1]['exact_state_cpi'][state]-interior[0]['exact_state_cpi'][state]
                                                                  for state in ['Computing','Stalled','Flushed','Drained']},
                     dip_exclusive_delta_cpi=exclusive,
                     native={key:dict(control=value,candidate=b['counters'][key],delta=b['counters'][key]-value if value is not None and b['counters'][key] is not None else None)
                             for key,value in a['counters'].items()},memory_context=context,
                     gap_bounds=[{key:float(value) if key not in ['point','variant'] else value for key,value in row.items()}
                                 for row in gaps if row['variant']==variant and row['point']==point],
                     top_pc_changes={view:sorted([row for row in selected_pcs if row['view']==view],key=lambda row:(-abs(row['delta_cpi']), row['pc'], row['state'], row['signature'], row['labels']))[:12] for view in ['TEA','DIP']}))
for row in rows:
    thresholds = {int(item['threshold']): item for item in row['gap_bounds']}
    row['gap_band_128_to_511_delta_cpi'] = dict(
        lower=thresholds[128]['delta_long_cpi_lower'] - thresholds[512]['delta_long_cpi_upper'],
        upper=thresholds[128]['delta_long_cpi_upper'] - thresholds[512]['delta_long_cpi_lower'])
result = dict(completed=len(rows),expected=summary['expected_points'],rows=rows,
              limits=['ROB waitLoad/fmul/fcvt count a not-written-back ROB head of that type, not global FU or memory service latency.',
                      'Miss-entry cycle sums can overlap between entries and cross ROI boundaries; not elapsed CPI or average request latency.',
                      'Printed E2 prefetch labels are retained as raw context, without an accuracy, criticality or causal interpretation.',
                      'Each TEA interior retains its own retirement denominator and omitted edges; gap bounds may be inconclusive.',
                      'No measured per-PC producer edges or credit-hit/request intersection. No suite or adoption claim.'],
              source_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest())
(OUT / (variant + '-evidence.json')).write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps(dict(completed=len(rows),results=[row['comparison'] for row in rows]),indent=2))
