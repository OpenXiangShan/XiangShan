# TEA: instruction-level time attribution

TEA (Time-Proportional Event Analysis) attributes sampled execution cycles to
architectural instructions. It preserves the functional ROB's compression,
execution, and commit decisions. Shared raw event metadata and ROB observations
are described in [Shared instruction profiling observations](profiling-events.md).

## Data flow

Rename captures architectural members before fusion removes its follower from
the uop stream. A packet sideband follows rename and dispatch into
`TeaMemberTracker`. The tracker maintains member completion, accumulated event
signatures, retirement, and cancellation. `TeaSampleSelector` records each due
boundary immediately; delayed final records resolve selected targets offline.

The tracker uses a simulation-only native implementation with Verilator DPI and
GSIM wrappers. It is not a hardware area estimate or a synthesizable profiling
implementation.

## Compressed ROB members

A physical ROB entry has Former and Latter slots. A simple slot may contain
multiple architectural instructions, and one instruction may produce several
uops. Fusion and move elimination must not remove architectural members from the
profile.

Each member records its slot pointer, rank within the entry, execution owner,
original PC, PSV, and expected writeback count. A fused follower uses its leader
as the execution owner. Members are captured once, on `firstUop`, using the
original instruction's debug PC. PCs are extended to XLEN using the active
translation mode: sign extension for Sv39/Sv48 and zero extension for bare/x4.

The complete member packet is installed when the physical entry is first
allocated, including members still waiting in partial dispatch. Actual accepted
uops subsequently contribute their enqueue signatures. Writeback rank travels
through the existing `PerfDebugInfo` path.

A multi-member slot tracks each owner's remaining accepted writebacks; functional
slot completion also marks members complete. A singleton slot uses functional
ROB completion so an early writeback from a multi-uop instruction cannot finish
it prematurely. Moves eliminated at rename need no separate execution writeback.

Retirement expands the actual committed slots into their live architectural
members. The expanded count is checked against the functional architectural
retirement count when profiling history exists. Redirects cancel members using
the complete slot pointer, including the wrap flag and Former/Latter identity.

A delayed Stalled target is identified by `(allocationCycle, robIndex, rank)`.
The allocation cycle prevents an old sample from matching a reused ROB entry.

## Performance signatures

A PSV is an OR-accumulated signature, not a counter or a latency:

| Bit | Name | Meaning |
| --- | --- | --- |
| 0 | `DR_L1` | Fetch recorded an ICache miss |
| 1 | `DR_TLB` | Fetch recorded an ITLB miss |
| 2 | `DR_SQ` | The oldest waiting store was blocked by SQ space while ROB was empty |
| 3 | `FL_MB` | Control misprediction |
| 4 | `FL_EX` | Exception flush, excluding interrupt priority |
| 5 | `FL_MO` | Memory-order redirect |
| 6 | `ST_L1` | Data-cache miss |
| 7 | `ST_TLB` | Any-issue data-TLB miss |

Frontend packet events belong only to the earliest valid instruction. Fusion
preserves each architectural member's original frontend PSV. Enqueue adds only
DR_SQ to the actual dispatch member, so a fused output's combined frontend PSV
does not duplicate the follower's event onto the leader. Execution events match the complete slot
identity; enqueue events additionally identify member rank. Same-cycle events
are visible to retirement, recovery snapshots, and delayed final records.

`PSV=0` means **No recorded event**. A long arithmetic stall can have no recorded
cache, TLB, or recovery event. ST_LLC is not implemented. Event bits can overlap;
their weights must not be summed as a disjoint partition of stall cycles.

## Sampling and attribution

The selector uses the shared ROB cycle number. Sampling advances at a fixed
cycle interval without waiting for previously selected targets. The default
period is 256; period 1 samples every cycle, and period 0 also samples every
cycle. Re-enabling sampling starts with an immediately due boundary.

The cycle-state priority is:

1. Any real commit: **Computing**.
2. ROB walk, redirect, or an empty ROB with a remembered recovery cause: **Flushed**.
3. Empty ROB: **Drained**.
4. Otherwise: **Stalled**.

| State | Target | Weight per boundary |
| --- | --- | --- |
| Computing | All N architectural instructions actually retired this cycle | `1/N` each |
| Stalled | Oldest unfinished member in the head physical entry | 1 |
| Flushed | Remembered recovery cause | 1 |
| Drained | First architectural instruction of the next real commit | 1 |

Computing takes priority over concurrent recovery conditions. Members allocated
before profiling history was enabled retain an unknown share rather than
transferring that share to known members.

The Stalled policy adapts next-committing attribution to compressed entries: if
member A is complete and B is not, B is selected. It does not prove the critical
path or identify the member that will ultimately finish last. A full head entry
with no selectable unfinished member produces an unknown target.

Stalled samples record the target identity immediately and use its final PSV at
retirement. If the target is flushed, its final record marks it cancelled. Many
samples can share one final record. Drained boundaries wait for the first real
successor commit, including when new sampling has since been disabled.

Flushed samples use a retained cause PC/PSV snapshot. It spans walk and the empty
recovery interval until the recovery path allocates a ROB entry. Unsupported
recovery events invalidate the old cause. A ROB flush's delayed redirect echo
does not create a new cause. Flushed attribution does not wait for a final PSV.

After TEA is first enabled, the tracker keeps outstanding targets alive when new
sampling is disabled. No sampled-target hardware queue is allocated per sample.

## Database format

For hart 0, retain both tables:

| Table | Content |
| --- | --- |
| `Tea_0` | Sample cycle/state, direct PC/PSV vectors or delayed target, and ROB context |
| `TeaDelayedFinal_0` | Selected member retirement/cancellation or a Drained successor |

The context records the current architectural retirement count, raw overlapping
commit-block conditions, head readiness, and the existing entry-level Top-down
stall reason. That reason describes a physical entry, not a selected member.

`VALIDMASK_n` divides the member mask into one chunk per physical commit lane,
keeping each SQLite integer below the unsigned 64-bit boundary. `PCVEC_n` and
`MEMBER_PC` preserve full XLEN addresses. The analysis restores negative SQLite
integers to unsigned 64-bit PCs before aggregation, filtering, or ELF matching.
`CYCLE` identifies sampling time; `STAMP` is not used for target resolution.

Cancelled, unresolved, and unknown target/cause samples retain their weight and
are reported separately from PSV zero. End-of-run targets without a final record
remain unresolved. Final records outside an analysis window may still resolve
samples inside it.

## Cycle statistics and no-commit intervals

`WITH_TEA_STATS=1` enables `TeaCycleStats`, independently of the member tracker.
It counts the four TEA states, retired instructions, and complete no-commit gap
buckets of 1–7, 8–31, 32–127, 128–511, and at least 512 cycles. It also reports
left/right-censored intervals and pending tails.

`TeaCycleSummary_0` contains periodic and dump/clean/disable snapshots;
`TeaNoCommit_0` contains long or censored no-commit intervals. Summary collection
is enabled by `enableTeaStats0` or `enableTea0` and reuses the existing XSLog
performance dump/clean controls. It has fixed-size counters and no per-member
history. `teaGapThreshold0` defaults to 128.

Statistics are exact only over the observed enabled prefix. An unclosed
collection does not establish that the final summary covers the full simulation
window. Match its endpoint to the simulator's final performance dump.

## Build and collect

From the repository root, build with the normal debug metadata, ChiselDB, and
Constantin support:

```bash
NOOP_HOME="$PWD" make emu WITH_TEA=1 WITH_CHISELDB=1 WITH_CONSTANTIN=1 -j8
```

For GSIM, use `make gsim GSIM=1` with the same feature options. Build parallelism
must fit the available memory and CPUs. `WITH_TEA_STATS` defaults to `WITH_TEA`;
set it to zero to omit statistics. Summary-only builds use
`WITH_TEA=0 WITH_TEA_STATS=1` and omit the tracker and frontend PSV metadata.

Example runtime configuration:

```text
enableTea0 1
teaSamplePeriod0 256
```

For summary-only collection, use `enableTea0 0` and `enableTeaStats0 1` instead.

```bash
build/emu -i ready-to-run/coremark-2-iteration.bin \
  --diff ready-to-run/riscv64-nemu-interpreter-so --max-instr 5000 \
  --cst-file build/tea.cst \
  --dump-select-db 'Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0' \
  --db-path build/tea.db
python3 scripts/tea_visualize.py build/tea.db --labels coremark --output build/tea-profile
```

The difftest measurement-window support clears profiling records after the
existing warmup dump/clean cycle. Exiting before warmup finishes exports empty
profiling tables. Target history, cycle numbering, and sampling phase continue
across the boundary; clearing records must not reset them.

See [TEA visualization](tea-visualization.md) for filtering, PICS weights,
comparison output, and analysis limitations. Attribution estimates where sampled
time is spent; it does not directly measure recoverable stall cycles.
