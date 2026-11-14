#!/usr/bin/env python3
"""Summarize final XSPerf dumps without treating candidate blocks as F/useful.

Multiple periodic dumps: keep the final value per fully qualified counter path.
MSHR entries are distinct paths and are summed. Provide one complete simulator
stdout per invocation; do not concatenate independent reset windows/runs.
"""
import argparse
import json
import re
from pathlib import Path

ap = argparse.ArgumentParser()
ap.add_argument('log', type=Path)
ap.add_argument('--require-smoke', action='store_true')
ap.add_argument('--perf-log', type=Path, help='Separate stderr/performance log')
ap.add_argument('--json', type=Path)
a = ap.parse_args()
text = a.log.read_text(errors='replace')
perf_log = a.perf_log
if perf_log is None and a.log.name in ('simulator-out.txt', 'simulator_out.txt'):
    sibling = a.log.with_name(a.log.name.replace('out', 'err'))
    if sibling.exists():
        perf_log = sibling
if perf_log is not None:
    text += '\n' + perf_log.read_text(errors='replace')
last = {}
for line in text.splitlines():
    match = re.search(r'(\S*?)(history_[A-Za-z0-9_]+),\s*(\d+)\s*$', line)
    if match:
        # Printed hierarchy precedes the counter and is needed for MSHR sums.
        key = line[:match.start(2)]
        key = re.sub(r'^.*?\[PERF\s*\]\s*\[time=\s*\d+\s*\]\s*', '', key)
        last[(key, match.group(2))] = int(match.group(3))
sums = {}
for (_, name), value in last.items():
    sums[name] = sums.get(name, 0) + value
levels = {}
for level in range(1, 4):
    prefix = f'history_l{level}_'
    candidate = sums.get(prefix+'candidate_blocks', 0)
    blocked = sums.get(prefix+'blocked_blocks', 0)
    allowed = sums.get(prefix+'allowed_blocks', 0)
    levels[f'L{level}'] = dict(candidate_blocks=candidate, blocked_blocks=blocked, allowed_blocks=allowed,
        blocked_candidate_fraction=blocked/candidate if candidate else None,
        shadow_later_demand_proxy=sums.get(prefix+'shadow_demand_blocks'),
        conservation_pass=candidate == blocked + allowed)
report = dict(levels=levels, counters=sums, smoke_checksum='STREAM_HISTORY_SMOKE PASS' in text,
    good_trap='HIT GOOD TRAP' in text, causal_weight=None,
    note='Candidate-event fractions are not refill-F boundary/false-filter rates. Shadow demand is a bounded VA proxy.')
if a.require_smoke:
    errors = []
    if not report['smoke_checksum'] or not report['good_trap']:
        errors.append('missing checksum marker or GOOD TRAP')
    for name in ('history_train', 'history_allocate', 'history_complete', 'history_seed', 'history_stream_refill_success'):
        if not sums.get(name): errors.append(f'no exercise of {name}')
    for level, metrics in levels.items():
        if not metrics['candidate_blocks'] or not metrics['blocked_blocks'] or not metrics['allowed_blocks']:
            errors.append(f'{level} missing candidate/blocked/allowed exercise')
        if not metrics['conservation_pass']: errors.append(f'{level} candidate conservation failed')
    report['errors'] = errors
    report['pass'] = not errors
if a.json:
    a.json.parent.mkdir(parents=True, exist_ok=True)
    a.json.write_text(json.dumps(report, indent=2) + '\n')
print(json.dumps(report, indent=2))
if a.require_smoke and not report['pass']:
    raise SystemExit(1)
