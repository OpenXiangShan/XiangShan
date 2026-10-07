# H3: competitive Stream depth, release only

DefaultConfig selects PDB64/LRU with competitive L1 Stream depth, initially 16.
The seven levels are 4, 8, 16, 24, 32, 48 and 64 cache blocks. Each control window
contains exactly 500 successful clean Stream refills into the PDB. Every decision
can move at most one level. Outer prefetch levels retain their existing behavior.
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
- The late tracker keeps two bits per MSHR (original Stream and demand present),
  covering the allocation pipeline and the actual entry. It uses existing owner
  address matches. It does not use or modify legacy prefetch flags, which can be
  cleared by raw rejected queries. Subsequent terminal Stream PF requests against
  a demand owner each count as a new PF attempt.
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

Counters under `depthMonitor` include refills, both reuse classes, late events,
completed windows, increases/decreases, depth residency in cycles/windows and
shadow-active windows. MissQueue separately reports
`pdb_stream_demand_hit_prefetch_mshr` and
`pdb_stream_prefetch_hit_demand_mshr`. Legacy `l1prefetchLate` is a different
counter and must not be substituted for competitive late evidence.
Performance dump/reset does not reset the controller or FIFO. A window may
cross the warmup/ROI boundary; use the window trace when attributing its counts
to a phase rather than assuming each phase starts at an empty control window.

## Validation and requested next data

Local regression passed all 36 tests in 10 suites (`h3-regression-tests.log`).
Coverage includes exact threshold and window boundaries; no-refill intervals and
saturation; all seven depth levels and settle 0/1; tie/direction credit resets;
fixed-mode observation; shadow isolation; real MissQueue compression, pending
allocation, cancellation, rejection and source ownership; clean accepted PDB
refills; FIFO persistence; and actual Stream addresses at every depth.

The repository `mill -i xiangshan.checkFormat` passed. Its configured scope is
frontend/utils, so this is not an automated backend format certification. The
changed backend files were reviewed manually and `git diff --check` passed.
The H2 predecessor's complete bwaves validation is in
`pdb-h2-bwaves-validation.md`. H3 has not run a performance workload.

Generation uses the existing `build/` directory and `NOOP_HOME` set to this
repository root. The simulation command passed in 11m00s:

```sh
make sim-verilog CONFIG=DefaultConfig JVM_XMX=40G WITH_CONSTANTIN=1 WITH_CHISELDB=1
```

Generated RTL was checked for the PB refill/MissQueue late/FIFO reuse inputs,
the controller-to-Stream depth output, compiled Constantin defaults, and the
complete `PDBDepthWindow0` writer/schema. Selected generated evidence is under
`tmp/h3-sim-rtl-evidence/`; all 563 main/test Scala file hashes match
`tmp/h3-source-manifest.json`. Final release generation passed in 6m39s, recorded
in `h3-verilog.log` and `tmp/h3-validation.json`. It was explicitly forced because
simulation generation also writes `build/rtl/XSTop.sv`:

```sh
make -W src/main/scala/xiangshan/mem/prefetch/PDBDepthController.scala verilog CONFIG=DefaultConfig JVM_XMX=40G
```

Release RTL retains the functional competitive depth path. Debug layers are
disabled in this build, so unused shadow/debug-only state is optimized away.
The simulation build retains it for observation. Release evidence and hashes are
under `tmp/h3-release-rtl-evidence/`; source hashes still match the frozen set.

The shared `build/emu` still refers to the H2 executable. Rebuild the H3 emulator
from the delivered commit before running data; successful RTL generation alone
does not update that executable. Preserve the result directory's H2 executable
and validation database for the predecessor evidence.

After reviewing the commit, run the fixed64 and dynamic configurations above
with DefaultConfig, the same emulator and the same representative checkpoint
profile. Start with the planned P set: bwaves, GemsFDTD, dealII, wrf, xalancbmk
and leslie3d examples, using the user's actual RTL checkpoint mapping. Verify
difftest/RTL assertions, exact 500-refill windows, pressure/depth transitions,
and zero policy/background moves. Compare IPC and demand misses, then compare
depth residency with the fixed-depth oracle where available. Broader A/C runs
and parameter changes depend on those user-run results.

The controller is a sideband implementation. Successful generation and directed
simulation do not prove physical timing/area or a workload performance benefit.
