#!/usr/bin/env python3
"""Differential vectors from the frozen, independently trace-audited V19 model.

Candidates are evaluated before the current edge's training update. A training
accepted at S0 is queried at S2 (two cycles). Three levels use independent target
addresses; L3 reuses the L2 decision rule. No external trace/db or /tmp needed.
"""
import argparse
import copy
import json
import random
from pathlib import Path
from reference.compact import CompactConfig, CompactPolicy

ap = argparse.ArgumentParser()
ap.add_argument('--policy', type=int, required=True, choices=(1, 2, 3))
ap.add_argument('--output', type=Path, required=True)
args = ap.parse_args()
selected = json.loads((Path(__file__).parent / 'reference/selection.json').read_text())
cfg = CompactConfig(**selected[args.policy - 1]['config'])
model = CompactPolicy(cfg)
rng = random.Random(7931)
events = []

def train(pc, region, offset=0, eligible=True):
    events.append((True, pc, region * 16 + offset, eligible, False))

def segment(pc, base, length, reverse=False, eligible=True):
    direction = -1 if reverse else 1
    for r in range(length):
        reg = base + direction * r
        for b in (range(15, -1, -1) if reverse else range(16)):
            train(pc, reg, b, eligible)
        if r and r % 3 == 0:
            train(pc, reg - direction, 8, eligible)  # tolerated one-region revisit
            train(pc, reg, 9 if not reverse else 6, eligible)

# Closed-form checks of learners, left-censor exclusion and near-max evidence.
for i, length in enumerate([8, 8, 8, 8, 4, 4, 12, 12, 6, 6, 8, 8]):
    segment(0x10000, 0x1000 + i * 2048, length)
for i in range(5):
    segment(0x20000, 0x30000 + i * 2048, 8, reverse=True)
# Same PC direction changes; initial direction cannot be inferred from repeats.
for i in range(6):
    segment(0x30000, 0x50000 + i * 4096, 5, reverse=bool(i & 1))
for _ in range(7):
    train(0x40000, 0x60000, 4)
train(0x40000, 0x60000, 3)
train(0x40000, 0x60000, 2)
# Exact even rounding edge, saturating span, unrepresentable close.
for length in (3, 5, 253, 255, 256, 1023, 1025):
    segment(0x50000, 0x100000 + length * 2048, length)
# Ineligible allocation prohibited; hits must still advance on ineligible demand.
segment(0x60000, 0x800000, 8, eligible=False)
segment(0x60000, 0x810000, 1)
segment(0x60000, 0x820000, 8, eligible=False)
segment(0x60000, 0x830000, 8)
# Full/partial PC-tag placement conflicts, replacement and in-flight ABA guard.
for n in range(2500):
    train((0x100 + n % 127) << 13, 0xa00000 + (n % 127) * 2048 + (n // 127) % 12, n % 16)
# Full-tag versus partial-tag identity at high PC bits (same hashed set).
for i in range(8):
    segment(0x71000 | ((i % 2) << 45), 0xc00000 + i * 2048, 4 + 2 * (i % 2))
# Flush with active pipeline, then immediately admit more samples.
for i in range(16):
    events.append((True, 0x10000, 0x40000 + i, True, i % 7 == 0))
# Random, interleaved PCs and stalls exercise S0/S2 on adjacent cycles.
positions = [0xb00000 + i * 4096 for i in range(36)]
for i in range(14000):
    pcidx = rng.randrange(len(positions))
    positions[pcidx] += rng.choice([0, 1, 1, 1, -1, -16, 16, 256])
    events.append((rng.random() < .87, 0x100000 + pcidx * 4, positions[pcidx], rng.random() < .6, rng.random() < .001))
# Boundary addresses including underflow/wrap discontinuity.
for region in [0, 1, 0, (1 << 40) - 1, (1 << 40) - 2, 0]:
    for b in [0, 2, 15]:
        train(0x123456, region, b)
events += [(False, 0x10000, 0, False, False)] * 4

args.output.parent.mkdir(parents=True, exist_ok=True)
previous_flush = False
triggers = {}
blocks = [0, 0, 0]
with args.output.open('w') as out:
    for cycle, (valid, pc, block, eligible, flush) in enumerate(events):
        setidx, tag = model.address(pc)
        base = setidx * cfg.ways
        way = next((w for w in range(cfg.ways) if model.array[base + w].valid and model.array[base + w].tag == tag), None)
        if way is None:
            way = next((w for w in range(cfg.ways) if not model.array[base + w].valid), model.replacement[setidx].victim())
        selected_idx = base + way
        context = model.pipeline.get(cycle - 2)
        trigger = triggers.get(cycle - 2, (0, 0))
        if context and context[1] >= 0:
            e = model.array[context[1]]
            pred = model.predicted_length(e)
            # In range / just outside / far outside, with independent per-level targets.
            regions = [e.start, e.start + (e.hist_direction or 1) * max(0, pred - 1),
                       e.start + (e.hist_direction or 1) * pred]
            rng.shuffle(regions)
            blocks = [(r & ((1 << 40) - 1)) * 16 for r in regions]
        else:
            blocks = [block + 64, block + 640, block + 960]
        masks = [0x3, 0xf0, 0xff00]
        permits = []
        for level in range(3):
            allow = True if flush or previous_flush else bool(model.decide(
                cycle, cycle - 2, trigger[0], blocks[level], masks[level], min(level + 1, 2),
                True, trigger_block=trigger[1]))
            permits.append(int(allow))
        before = copy.copy(model.array[selected_idx])
        if flush or previous_flush:
            state = before  # inspect is combinational pre-edge during flush
            model.flush()
        else:
            if valid:
                model.observe(cycle, cycle, pc, block, eligible)
            state = model.array[selected_idx]
        triggers[cycle] = (pc, block)
        fields = [valid, eligible, pc, block, flush]
        for target, bitmap, allow in zip(blocks, masks, permits):
            fields += [target & ((1 << 44) - 1), bitmap, allow]
        fields += [state.valid, state.frontier, state.span, state.offset,
                   2 if state.direction < 0 else state.direction, state.censored,
                   state.overflow, state.epoch, state.length, state.second,
                   state.hist_direction < 0, state.confidence, state.tag]
        out.write(' '.join(str(int(x)) for x in fields) + '\n')
        previous_flush = flush
print(json.dumps({'policy': args.policy, 'cycles': len(events), 'capacity_bits': cfg.capacity()['total_bits'],
                  'reference_events': model.stats}, indent=2))
