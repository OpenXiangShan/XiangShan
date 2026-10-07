# H3: competitive Stream depth, release only

DefaultConfig selects PDB64/LRU with competitive L1 Stream depth, initially 16.
The seven levels are 4, 8, 16, 24, 32, 48 and 64 cache blocks. Each control window
contains exactly 500 successful clean Stream refills into the PDB. Every decision
can move at most one level. Outer prefetch levels retain their existing behavior.
The policy is implemented in `PrefetcherMonitor.scala`, inside the existing
Stream `L1PrefetchMonitor`. DCache provides events through `bufferinfo` and
`missinfo`; the normal `pf_ctrl` output is the sole depth control path.
Physical used/unused capacity moves and background load promotion remain off;
correctness-required store transfers keep the existing protocol.

## Evidence and decisions

- `streamRefill` counts the actual accepted PDB data transfer, using the original
  source retained in its reservation. Allocation, Acquire, wrong-owner/not-ready
  refill attempts, denied data and corrupt data do not advance the window.
- Late evidence has two classes: a terminal Stream PF attempt matching an MSHR
  that already contains demand, and the first accepted demand merging into a
  Stream PF lifetime. PF attempts against a demand MSHR count even though MQ drops
  them. Unrelated sources, cache hits, unmatched full-MSHR drops, and rejected or
  cancelled demands do not produce these events. Multiple accepted lanes for one
  lifetime count once. Compressed demands with a Stream allocation count once on
  that allocation edge. Separate MSHR lifetimes can each count on the same edge.
- The existing MSHR statistics are reused; there is no separate LateTracker or
  extra per-MSHR state. `hit_pf_in_mshr` is a lane bitmap, with multiple accepted
  demands for the same lifetime counted once. Its source comes from the actual
  matched entry/pipeline slot. Cancelled or rejected queries do not consume the
  existing prefetch flag/source. Compressed prefetch+demand allocation and a
  demand accepted while allocation is in the pipeline clear the source exactly
  once, preventing a second count after the handoff. Terminal PF hits use the
  existing `pf_late_in_mshr` and matched source, without requiring MQ ready.
- Demand-only allocations retain `L1_HW_PREFETCH_NULL`, including compressed
  demands and later merges during the allocation handoff. `L1_HW_PREFETCH_CLEAR`
  is written only when an actual prefetch is consumed by an accepted demand;
  ordinary demand refills must not acquire prefetch-related L1 metadata.
  The software-prefetch source ambiguity is outside this correction: the target
  workloads are assumed to contain no software data prefetches.
- Stream unused victim hits come from the persistent 256-entry FIFO observer.
  Used hits only update the used-move shadow; they never reduce depth. FIFO entries
  remain observable until first demand hit, duplicate refresh or full FIFO
  replacement. There is no epoch, age limit or window clear.
- A two-edge adapter aligns raw refill/late events to completed observer hits.
  All events on the closing edge belong to the closing window. Sixteen-bit event
  counters saturate, so a long interval without refills cannot wrap to low pressure.

Late thresholds are 20/40 and unused thresholds 40/80 events per window. A count
at or below the lower threshold gives pressure 0; strictly between thresholds
gives 0.5; at or above the upper threshold gives 1. Late minus unused pressure
adds credit in its direction and clears opposite credit. Ties clear both. At
credit 1, depth moves one level and credit resets. Saturated boundaries clear
credit too. A changed depth then spends one full observation window settling;
statistics and shadow states continue, but no depth decision is made in that
window. Pressure and credit use integer half units.

Shadow used move turns on after three consecutive windows with at least 20 used
hits, and off after two with at most five. Shadow unused move turns on only after
a complete depth4 window with positive unused pressure at least as large as late
pressure; it turns off after eight consecutive zero-unused-pressure windows.
The shadows do not send moves, consume unused pressure or increase the depth4
up-credit threshold. They remain observable in the fixed-depth control mode.
These initial RTL parameter choices have not been tuned or performance-validated.

## Same-DefaultConfig controls

The compiled defaults select the dynamic candidate. With Constantin enabled,
the following initialization selects the fixed64 release control in the same
binary, retaining all H3 observation and shadow logic:

```text
enablePDBAutoDepth0 0
pdbFixedDepth0 64
enablePDBVictimObserver0 1
enablePDBMoveToDCache0 0
```

Use `enablePDBAutoDepth0 1` for the dynamic candidate. These are initialization
controls, not a live runtime switching interface. `pdbFixedDepth0` must select
one of the seven levels. In H3 DefaultConfig, this control chooses the fixed
fallback; the historical `streamL1Depth0` does not select the monitor's output.
An initialization file must actually reach emu via `--cst-file`; merely copying
`constantin.txt` into a result directory does not load it. The current
`xs_autorun_multiServer.py` wrapper used by `tmp/cr-run.sh` does not forward that
argument. Use a runner that supplies it (or an explicit emu invocation) for a
same-binary control, and verify the printed initialization. The candidate itself
uses compiled defaults and needs no initialization file.
The named historical PDB/NoPDB diagnostic configs retain their fixed64 behavior.
The user only needs DefaultConfig for H3 A/B testing.
The original `tmp/cr-run.sh` still selects `PDB64LRUConfig`; change the config
selection in the user's run copy to `DefaultConfig` when building the H3
candidate. The original script was not modified by this work.

Victim observation remains enabled by default; the user-requested full victim
trace default from H2 is retained. `tracePDBDepth0` defaults on and logs one
`PDBDepthWindow0` row per completed window, including counts, pressures, old/new
depth, credits, settle and shadow decisions. ChiselDB capture still requires a
simulation build and the runner's database option. Performance-only runs can
disable trace dumping. Check the actual run initialization and preserve the
binary hash, checkpoint profile, warmup/ROI and DRAM/reference settings for A/B.

Counters now reside under `prefetcherMonitor.StreamMonitor`, including refills,
late events, both reuse classes, windows, depth changes/residency and shadows.
The two existing per-Stream MSHR metrics are `l1prefetchHitInMSHRStream` and
`l1prefetchLateInMSHRStream_HitDemand`. Their events feed competitive late pressure.
The separate `pdb_stream_*` MissQueue counters were removed with the tracker.
Legacy aggregate `l1prefetchLate` also includes cache hits and must not be used
as the competitive late count. Existing power-of-two `Stream_depth*` counters
now reflect the selected output depth; `cycles_at_depth_*` covers all seven levels.
`PDBDepthWindow0` keeps its schema; its trace site is now `StreamMonitor0`.
Performance dump/reset does not reset the controller or FIFO. A window may
cross the warmup/ROI boundary; use the window trace when attributing its counts
to a phase rather than assuming each phase starts at an empty control window.

## Validation and requested next data

The preceding monitor integration revision (`69f3a563a`) passed 37 tests in nine suites (252 seconds),
`xiangshan.checkFormat`, debug DefaultConfig generation (9m31s), and fresh release
`make verilog` (6m37s). All 561 Scala source hashes remained unchanged throughout
generation. Generated RTL confirms inline StreamMonitor control and the original
`pf_ctrl` path, with no separate tracker, monitor or controller instance.
The revision removes 82 production source lines net relative to its parent.
Results are recorded in `h3-monitor-regression.log`, `h3-monitor-sim-verilog.log`,
`h3-monitor-verilog.log` and `tmp/h3-monitor-validation.json`.
The demand-source correction passed eight relevant tests across three suites,
format checks and fresh DefaultConfig `make verilog` (6m26s). Coverage includes
load/store demand-only allocation, compression and pending/resident merges,
consumed hardware-prefetch CLEAR tags, PB protocol and monitor-to-Stream depth.
The pre-fix regression observed CLEAR (1) where NULL (0) was required. One initial
test incorrectly retained a probing PF as the demand's compression leader; after
correcting that test setup, the MQ suite passed. Production source was unchanged.
Evidence is in `h3-demand-source-tests.log`, `h3-demand-source-mq-tests.log`,
`h3-demand-source-verilog.log` and `tmp/h3-demand-source-validation.json`.
All 561 source hashes were checked; H1 references are unchanged. This correction
adds no state or interface and does not change depth policy or parameters.
Coverage includes exact thresholds/window boundaries, saturation, all seven
levels, settle 0/1, credit resets, fixed-mode observation and shadow isolation;
real MissQueue lane bitmap/source/acceptance/compression/pipe handoff; real PDB
refills; FIFO persistence; and actual PrefetcherMonitor-to-Stream address generation.
Stride/Berti and the explicit legacy Stream branch are checked separately.

The repository formatter covers configured frontend/utils paths; changed backend
files also require manual review and `git diff --check`. The predecessor's H2
bwaves validation is in `pdb-h2-bwaves-validation.md`.

Generation uses the existing `build/` directory, with `NOOP_HOME` set to this
repository root. Required commands are:

```sh
make sim-verilog CONFIG=DefaultConfig JVM_XMX=40G WITH_CONSTANTIN=1 WITH_CHISELDB=1
make -W src/main/scala/xiangshan/mem/prefetch/PrefetcherMonitor.scala verilog CONFIG=DefaultConfig JVM_XMX=40G
```

The release command is forced because simulation generation also writes
`build/rtl/XSTop.sv`. Generated integration checks must follow the existing
PrefetcherMonitor/StreamMonitor hierarchy; there is no standalone PDBDepthMonitor
or PDBDepthController instance. Debug-only shadow state may be optimized away
in release, while the functional competitive state remains in StreamMonitor.

The shared `build/emu` still refers to the H2 executable. Rebuild the H3 emulator
from the delivered commit before running data; successful RTL generation alone
does not update that executable. Preserve the result directory's H2 executable
and validation database for the predecessor evidence.

After reviewing the commit, first run the dynamic DefaultConfig on bwaves_13153
with the existing 20M warmup + 20M ROI profile and window trace. Then expand to
the planned representative P set using the user's actual RTL checkpoint mapping.
The user has already run R2 (PDB64/LRU, fixed64, release) at H1: reuse those results
as the primary performance reference after checking checkpoint/profile/configuration.
There is no default requirement to rerun R2. Same-binary fixed controls above
remain available if a discrepancy requires isolating the bookkeeping fixes from
dynamic depth, or if historical run settings do not match.

Verify difftest/RTL assertions, exact 500-refill windows, pressure/depth transitions,
and zero policy/background moves. Compare IPC, demand misses and depth residency.
H1 comparisons include the subsequent MSHR statistic/source fixes; they alone
cannot attribute every difference to depth. Full victim trace is useful for a
focused diagnostic, not required for every performance run. Broader A/C runs and
parameter changes depend on user-run results.

The controller is a sideband implementation. Successful generation and directed
simulation do not prove physical timing/area or a workload performance benefit.
