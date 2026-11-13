# DIP: dispatch-slot profiling

DIP samples dispatch capacity and attributes each slot to an instruction. It uses
[shared performance observations](profiling-events.md), ChiselDB, and Constantin.
Its attribution rules follow Figure 3 and Sections 3.1–3.2 of the ASPLOS 2026 DIP
author manuscript. Hardware sampling CSRs and interrupts are not implemented.
The simulation-only native sampler is not a hardware area estimate.

## Attribution

Dispatch width is `RenameWidth`, measured in uops. Each sampled slot contributes
`1 / RenameWidth` sampled cycles. Holes left by partial dispatch do not reduce
the capacity being accounted for.

| State | Initial attribution |
| --- | --- |
| Base | A dispatched uop's own PC; fusion uses the leader uop |
| Stall | Remaining slots belong to the oldest waiting uop with resource causes |
| FrontEnd | Empty slots wait for the next dispatch successor and its frontend events |
| Misspeculation | Flushed provisional work is reattributed to the recovery cause |

A fused uop combines both source instructions' frontend PSV. Its surviving
leader therefore retains a fetch event originally attached to the follower.

Stall causes are multi-hot: ROB admission, integer/FP/vector/memory IQ, LQ/SQ,
rename freelists, serialization, LSQ recovery, LSQ allocation, and dispatch
policy/bandwidth. `ROB` means `!enqRob.canAcceptForDispatch`; this can include RAB,
VTypeBuffer, and serialization limits, not only physical ROB capacity. Rename
starvation waits for the next uop entering Dispatch.

FrontEnd distinguishes ICache miss, ITLB miss, and bubble using the shared
frontend signature's first two bits. It does not require a TEA member tracker,
selector, sample table, or analysis script.

All initial states are provisional until their target becomes stable. A flush
moves affected work, including pending frontend waits, to the cause PC and
classifies control misprediction, exception, memory order, other flush, or
interrupt. A later older redirect can reparent that work again. Recovery context
persists until a new uop enters Dispatch, excluding old valid lanes cancelled by
the redirect edge and the delayed echo of a ROB-originated flush.

Correct-path work resolves when its ROB slot actually commits. The identity
contains index, wrap flag, Former/Latter, and a generation cycle to distinguish
slot reuse. Compressed members and multiple uops share their slot's retirement
fate but keep their own PCs. A commit at or beyond a self-flushed cause provides
a watermark that can settle the cause without requiring it to commit again.

Unresolved work is retained at the end of the recorded window:

```text
resolved_slots + unresolved_slots = sampled_cycles * dispatch_width
```

Multi-hot cause combinations remain one profile item and are not counted once
per set bit. Weights are sampled cycles, not an estimate of all execution cycles
or directly recoverable stalls.

## Sparse records

Only sampled work creates target history. The sampler maintains references for
selected ROB slots and one generation for pending frontend successors. Arbitrary
numbers of samples can reference the same target without a per-sample hardware
queue. Disabling new sampling still resolves pending work.

For hart 0, collect both tables:

| Table | Content |
| --- | --- |
| `Dip_0` | Sample cycle, preceding cumulative retirement count, current retirement count, and dispatch slots |
| `DipDelayedFinal_0` | Selected commits, relevant flushes, successor binding, and stability watermarks |

Non-sampled cycles do not provide a complete retirement prefix. The analyzer's
`sampled_span_*` fields describe only the interval between the first and last
selected sample, inclusive. Use native simulator counters for the full ROI.
The simulator's final performance report has pipeline latency; use its
`cycleCnt` as an exclusive analysis endpoint when needed.

## Build and collect independently

```bash
NOOP_HOME="$PWD" make emu WITH_DIP=1 WITH_TEA=0 WITH_TEA_STATS=0 \
  WITH_CHISELDB=1 WITH_CONSTANTIN=1 -j8
```

GSIM uses `make gsim GSIM=1` with the same feature options. Size parallelism for
the machine. DIP has its own `WITH_DIP=1` elaboration switch and needs the normal
debug metadata (`--enable-difftest` or `--full-basicdiff`).

Create `build/dip.cst`:

```text
enableDip0 1
dipSamplePeriod0 256
```

The default period is 256. Period 1 is useful for short detailed runs. Runtime
sampling enable is separate from elaboration.

```bash
build/emu -i ready-to-run/coremark-2-iteration.bin \
  --diff ready-to-run/riscv64-nemu-interpreter-so --max-instr 5000 \
  --cst-file build/dip.cst --dump-select-db 'Dip_0 DipDelayedFinal_0' \
  --db-path build/dip.db
python3 scripts/dip_analyze.py build/dip.db --output build/dip-profile
```

DIP maintains target and recovery history during warmup. The difftest
measurement-window support deletes profiling records after the existing warmup
performance dump/clean cycle, while retaining history, cycles, sampling phase,
and cumulative retirement numbering. Exiting before warmup completes exports
empty profiling tables.

The database already excludes warmup records, so keep `--warmup-instr 0` for
ordinary analysis. A nonzero value further filters by cumulative retirement from
simulation start. `--end-cycle C` excludes samples at or after C; later final
records still resolve selected samples. Output consists of `pics.csv` and
`summary.json`, including resolved/unresolved capacity, reclassification counts,
state weights, and the selected sample span.
