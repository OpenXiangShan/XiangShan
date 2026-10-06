# H1: PDB capacity, replacement, and Stream depth configuration

Base: `feat-pdb` at `b086e6e465075cfa098ef348f87a6e37e88e972b`.
This handoff prepares the fixed-depth baseline for user-run RTL experiments.
Victim-reuse observation, competitive depth control, and capacity-victim move are
later handoffs. No RTL performance workload has been run for H1.

## Build configurations

`DefaultConfig` selects the user's baseline: PDB64, LRU, fixed L1 depth64, and
background move off. This supports runners that only accept `DefaultConfig`;
`PDB64LRUConfig` remains an equivalent named preset. The diagnostic presets below
override capacity/replacement explicitly. The core, four-way DCache geometry,
L2/L3, and other prefetchers retain their original settings. PDB capacities count 64-byte blocks.
`enablePDBMoveToDCache<hartId>` remains false by default; Store-required movement
is still available. Use the same Constantin file and workload setup across A/B.

| CONFIG | PDB blocks | Release selection | L1 depth source/default |
|---|---:|---|---|
| `DefaultConfig` | 64 | LRU | fixed / 64 |
| `PDB16Config` | 16 | RR | fixed / 64 |
| `PDB64Config` | 64 | RR | fixed / 64 |
| `PDB16LRUConfig` | 16 | LRU | fixed / 64 |
| `PDB64LRUConfig` | 64 | LRU | fixed / 64 |
| `NoPDBConfig` | 0 | not instantiated | fixed / 64 |
| `PDB64MonitorDepthConfig` | 64 | LRU | monitor / 16 |

Example release-RTL generation:

```sh
export NOOP_HOME=/path/to/XiangShan
make verilog CONFIG=DefaultConfig BUILD_DIR=build/h1-default/DefaultConfig JVM_XMX=40G
```

Use a distinct `BUILD_DIR` for each configuration. The Makefile's file target does
not track a changed `CONFIG` variable, so reusing a previously generated output
directory can incorrectly skip generation. The build prints PDB capacity,
replacement policy, and Stream depth source. `make verilog` is an elaboration and
SystemVerilog-generation check, not a functional full-system or timing result.
`NOOP_HOME` must point to this checkout for Difftest auxiliary-file generation.
After a failed generation, remove that configuration's generated output directory
before retrying: a partial `XSTop.sv` can otherwise satisfy the Makefile target.

For the existing simulation runner, keep `CONFIG=DefaultConfig` in its normal
`make emu` or `make simv` build. Capacity/replacement are selected at build time,
so an already built simulator must be rebuilt to acquire the new default.

## Depth source

Fixed mode uses `streamL1Depth<hartId>`, default 64 blocks. A Scala overlay can set
`new WithStreamDepth(StreamDepthParameters(fixedL1 = 4))`; a Constantin-enabled
simulator can override the existing fixed-depth record without another RTL edit.

Monitor mode makes Stream consume `pf_ctrl.dynamic_depth`. The H1 monitor preset
holds `Stream_depth<hartId>`, initially 16. It is a wiring check, not the competitive
algorithm. L2/L3 remain fixed at `streamL2Depth<hartId>` / `streamL3Depth<hartId>`
(640/960); changing L1 no longer implicitly changes outer-level distances.

`StreamDepthParameters(enableLegacyControl = true, useMonitor = true)` explicitly
enables the pre-existing late/useless monitor update logic. Automatic prefetcher
shutoff remains controlled independently by `Stream_enableDynamicPrefetcher` and
defaults off. This legacy algorithm is not an H1 performance candidate and is not
the H3 500-refill, seven-level competitive controller. Stride/Berti monitor defaults
are unchanged.
The legacy controller requires a power-of-two initial depth at most 2048; retain
this restriction if overriding `Stream_depth` through Constantin in that mode.

In fixed mode the monitor's historical `Stream_depth*` counters still describe its
unused monitor register, not the actual fixed address distance. Read the fixed
Constantin record/configuration and actual generated addresses when validating H1.

## PDB replacement contract

The experiment default is LRU. `PDB16Config` and `PDB64Config` explicitly retain
RR as optional diagnostics. LRU uses one recency bit per
unordered entry pair (120 bits at 16 entries; 2016 at 64). It does not store cycle
timestamps. Successful, non-denied refill and valid S2 demand consumption refresh
recency, including repeat use. Simultaneous touches are ordered by entry index,
highest index newest; a denied refill does not refresh recency.

Selection chooses the oldest eligible entry using registered recency. Eligibility,
capacity pressure (`refillWait && no free slot`), Probe/Store exclusion, selected
candidate registers, WBQueue handoff, and completion rules are preserved. A use in
the selection cycle affects the next selection. Once a request is selected, later
Load use does not retarget it while WBQueue stalls. Cancellation by withdrawn
pressure or protocol ownership changes still follows the existing PB protocol.

Thus this is exact recency at the selection boundary with a held RTL transaction;
it does not promise the instantaneous eviction ordering of the Gem5 model. The
victim-history FIFO discussed for H2 is a separate structure and is absent here.
LRU hardware area/frequency is not established by `make verilog`.

## User-selected baseline

Run `DefaultConfig`: PDB64/LRU, fixed depth64, only release. This is the baseline
for subsequent dynamic-depth and move experiments. The user does not need to run
the RR, PDB16, no-PDB, or monitor presets to proceed. Later compare fixed-depth
release with dynamic-depth release first, then isolate used/unused move gains.

Suggested diagnostic workloads are bwaves, GemsFDTD, dealII, wrf, xalancbmk, and
leslie3d, using the user's RTL-compatible checkpoint set and runner. Gem5 checkpoint
IDs and absolute scores must not be silently substituted for RTL equivalents.
Please keep warmup/ROI, prefetch enables and other knobs identical. Start with the
user's normal difftest/smoke procedure, then collect IPC/cycles, demand miss, PB
refill/use/release/full occupancy and background move count. Background move must
remain zero; Store-required moves should be distinguished. Observer hit counters
are unavailable until H2.

The mainline replacement policy is LRU. RR presets remain available for optional
diagnosis. Hardware timing/area is still unvalidated by this handoff.

## Directed verification

```sh
mill -i xiangshan.test.testOnly cache.PDBConfigurationTest cache.PDBLRUTest cache.PDBCapacityPolicyTest cache.StreamDepthConfigTest
mill -i xiangshan.test.testOnly cache.PrefetchDataBufferTest
```

The new tests cover configuration isolation on two cores, randomized LRU versus a
software recency list with masked eligibility, full PDB16/64 allocation, three-lane
use, repeated use, stalled-release stability, cancellation and slot reuse, actual
Stream addresses at all seven depths, unchanged outer-level distances, and explicit
legacy monitor control independent of shutoff. Existing PB protocol tests supplement
these checks. Build/test/review evidence is recorded in the selected H1 plan.

## H1 validation record (2026-10-06)

At commit `9ed6c9c1b`, all six named PDB presets passed `make verilog` with
`NOOP_HOME` pointing to this checkout, `JVM_XMX=40G`, and a fresh
`BUILD_DIR=build/h1-final/<Config>`. Generated module presence and the printed
capacity/replacement/depth settings were checked against each preset.

The 16 new directed cases and all 8 cases in the imported
`PrefetchDataBufferTest` suite passed. Another 14 existing local protocol cases
also passed; those supplemental local suites are not included in this commit.
The committed test commands above reproduce the 24 delivered cases.

Source review covered replacement ordering, Load feedback, protocol ownership,
depth-source isolation, and parameter bounds. The repository formatting task
passed via `mill -i xiangshan.checkFormat` after its daemon-mode invocation
stalled. Backend files are outside the repository formatter's configured scope;
they were reviewed manually, and `git diff --cached --check` passed.

Logs and command/output hashes are retained locally in `.planning/h1-config/`.
No full-system difftest, performance workload, or physical timing/area experiment
was run. User review and the comparisons above are the next gate before H2.

## DefaultConfig entry-point validation (2026-10-06)

After selecting PDB64/LRU in `DefaultConfig`, all 10 configuration and PB protocol
cases passed. The tests verify named preset overrides, two-core configuration
isolation, and the default fixed L1 depth64 with monitor control disabled.
The corrupt-refill test reuses legal MSHR IDs when filling more PB entries than
there are MSHRs.

The `make verilog CONFIG=DefaultConfig` command above passed in a fresh output
directory. Its log confirms 64 entries, LRU, background move default-off and fixed
L1 depth64; generated RTL includes `PDBLRU`. The repository formatting task and
source review also passed. Validation logs and source/output hashes are retained
in `.planning/h1-default/`. This verifies generation and directed behavior;
workload validation remains with the user after rebuilding the simulator.
