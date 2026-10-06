# H1: PDB capacity, replacement, and Stream depth configuration

Base: `feat-pdb` at `b086e6e465075cfa098ef348f87a6e37e88e972b`.
This handoff prepares the fixed-depth baseline for user-run RTL experiments.
Victim-reuse observation, competitive depth control, and capacity-victim move are
later handoffs. No RTL performance workload has been run for H1.

## Build configurations

All configurations inherit `DefaultConfig`: the core, four-way DCache geometry,
L2/L3, and other prefetchers are unchanged. PDB capacities count 64-byte blocks.
`enablePDBMoveToDCache<hartId>` remains false by default; Store-required movement
is still available. Use the same Constantin file and workload setup across A/B.

| CONFIG | PDB blocks | Release selection | L1 depth source/default |
|---|---:|---|---|
| `PDB16Config` | 16 | RR | fixed / 64 |
| `PDB64Config` | 64 | RR | fixed / 64 |
| `PDB16LRUConfig` | 16 | LRU | fixed / 64 |
| `PDB64LRUConfig` | 64 | LRU | fixed / 64 |
| `NoPDBConfig` | 0 | not instantiated | fixed / 64 |
| `PDB64MonitorDepthConfig` | 64 | LRU | monitor / 16 |

Example release-RTL generation:

```sh
export NOOP_HOME=/path/to/XiangShan
make verilog CONFIG=PDB64LRUConfig BUILD_DIR=build/h1/PDB64LRUConfig JVM_XMX=40G
```

Use a distinct `BUILD_DIR` for each configuration. The Makefile's file target does
not track a changed `CONFIG` variable, so reusing a previously generated output
directory can incorrectly skip generation. The build prints PDB capacity,
replacement policy, and Stream depth source. `make verilog` is an elaboration and
SystemVerilog-generation check, not a functional full-system or timing result.
`NOOP_HOME` must point to this checkout for Difftest auxiliary-file generation.
After a failed generation, remove that configuration's generated output directory
before retrying: a partial `XSTop.sv` can otherwise satisfy the Makefile target.

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

The default remains RR. LRU is an explicit alternative using one recency bit per
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

## Recommended first user-run comparisons

1. `PDB16Config` vs `PDB64Config`, both fixed depth64 and background move off:
   isolate capacity while retaining the original RR policy.
2. `PDB64Config` vs `PDB64LRUConfig`: isolate replacement at the target capacity.
3. `PDB16LRUConfig` vs `PDB64LRUConfig`: isolate capacity under demand-use-aware LRU.

Suggested diagnostic workloads are bwaves, GemsFDTD, dealII, wrf, xalancbmk, and
leslie3d, using the user's RTL-compatible checkpoint set and runner. Gem5 checkpoint
IDs and absolute scores must not be silently substituted for RTL equivalents.
Please keep warmup/ROI, prefetch enables and other knobs identical. Start with the
user's normal difftest/smoke procedure, then collect IPC/cycles, demand miss, PB
refill/use/release/full occupancy and background move count. Background move must
remain zero; Store-required moves should be distinguished. Observer hit counters
are unavailable until H2.

Freeze the PDB replacement policy after this comparison and before H2. The working
target is LRU; RR remains the controlled reference until user results and hardware
cost justify the choice.

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

All six presets in the configuration table passed `make verilog` with
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
