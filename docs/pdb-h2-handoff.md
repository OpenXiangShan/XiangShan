# H2: passive PDB victim observation

This handoff adds observation only, on top of `1c5183e45`.
`DefaultConfig` remains PDB64/LRU, fixed L1 depth64, with background move off.
The observer has no output connected to cache arbitration, ownership, request
ready/valid, prefetcher feedback, depth or move control. Store-required movement
continues to use the existing protocol. H3 is not implemented here.

## Events

- **Victim:** a clean resident PB entry accepted by WBQueue for a capacity
  release (`releaseReq.fire`). Waiting with valid asserted is not an event.
  Probe, cancellation, corrupt/poison exit and Store/background promotion do not
  enter the table. The existing background promotion is not a capacity move;
  H4 must connect its future capacity-move path to this same event contract.
- **Used classification:** include any valid S2 consumption before retirement.
  A Load may obtain S1 authorization on the Release edge and report use in S2
  one cycle later. The victim is therefore published one cycle after the
  release, incorporating that last use while the PB slot is still `released`.
  An assertion checks that the slot has not been reallocated. This does not
  change PB metadata or its pre-existing first-use/unused-exit feedback.
- **Demand:** all three LoadPipe S2 lanes, `s2_valid && !s2_kill &&
  instrtype == LOAD_SOURCE`, using the physical block address. S1-killed requests
  do not reach valid S2. Hardware/software prefetches are excluded. There is no
  DCache, PB, MSHR, miss, bank-conflict, first-issue or successful-consumption
  qualification: replays can query, and first-hit deletion deduplicates them.
  These are demand attempts, not retired architectural loads. Stores are not
  demand-query sources in H2.
- **Source:** all clean capacity victims remain observable. One `stream` bit
  separates Stream-only reuse from other prefetch sources without extra tables.

The final authorized use of a retiring PB allocation and victim publication can
coincide. Queries see the pre-insertion table, so this use cannot consume the
newly published victim. Subsequent demands can consume it.

## FIFO and pipeline

The default table has 256 entries. `PDBVictimObserverKey` controls `entries` and
`bankEntries` (default 32) for cost studies. It does not change PB capacity.
Each entry holds valid, physical block address, used and stream. Physical order
represents FIFO insertion order, ignoring invalid holes.

Each input cycle accepts one victim and three independent queries; there is no
ready signal or input queue that can overflow. The stages are:

1. Capture victim and all three demand lanes together.
2. Compare each demand against **all** valid entries in parallel. Merge hits by
   entry, count their old used/source classification, and clear them. Remove any
   duplicate of the incoming victim. Reuse the first invalid position by shifting
   its successors toward it and appending the new victim at the tail. Only if
   there is no invalid position after deletions does insertion evict the head.
3. Expose registered per-bank hit counts and the matching completed input batch.
   The reported occupancy is the table occupancy after that batch.

This is a pipelined parallel CAM, not time-multiplexed scanning. Grouping into
32-entry count banks does not reduce the number of address comparators. Throughput
is three queries per cycle, and result latency is two capture edges. Lookup and
deletion occur atomically at the same table snapshot; only counts/events continue
downstream. No delayed result addresses a slot that could have moved or been
reused, eliminating the need for allocation tags or an ABA recovery mechanism.

Within each batch, queries precede insertion. An old copy can be consumed while a
new victim of the same address is appended, and the new lifetime remains valid.
Multiple same-address lanes and consecutive queries consume each lifetime once.
A query cannot match a victim from the same or a later input cycle.

The table persists until global reset. Statistics reset, window boundaries and
depth changes do not clear it. There is no depth epoch, age, timestamp, observation
timeout or maximum number of intervening victims. FIFO ordering has no wrapping
sequence counter.

## Build and user-run controls

Use `DefaultConfig` throughout; the runner does not need a new configuration name.
Generate a fresh output directory to avoid Makefile reuse of an old `XSTop.sv`:

```sh
export NOOP_HOME=/nfs/home/yuwenlong/work_pfbuffer/base/XiangShan
make verilog CONFIG=DefaultConfig BUILD_DIR=build/h2/DefaultConfig JVM_XMX=40G
```

`make verilog` uses the FPGA/release target and disables Constantin/ChiselDB,
even if their Make variables are supplied. To generate the simulation RTL with
the observation switches and trace, use a separate fresh output directory:

```sh
make sim-verilog CONFIG=DefaultConfig BUILD_DIR=build/h2-sim/DefaultConfig JVM_XMX=40G WITH_CONSTANTIN=1 WITH_CHISELDB=1
```

When building the user's emulator, retain `CONFIG=DefaultConfig` and use
`WITH_CONSTANTIN=1` for a same-binary observer off/on comparison. Add
`WITH_CHISELDB=1` if event trace is needed. Without these optional flags,
the Scala defaults still select observer on and trace off.

The existing Constantin file format is `name unsigned_decimal_value` per line;
pass the file via the runner's `--cst-file` argument. For hart 0:

```text
enablePDBVictimObserver0 1
tracePDBVictimObserver0 0
enablePDBMoveToDCache0 0
streamL1Depth0 64
```

For observer off, change only `enablePDBVictimObserver0` to 0. This gates observer
input events, not PB or LoadPipe. Constantin is read at initialization; no live
switching or table clearing is promised. Preserve all other Constantin settings
between runs. Other harts use their corresponding numeric suffix.

## Counters and trace

Counters are under the `PDBVictimMonitor` instance; they count completed observer
batches, after the fixed observation delay. They do not feed the existing
PrefetcherMonitor. A performance reset clears accumulated counters using the
repository's normal machinery without clearing FIFO contents.

| Counter | Meaning |
|---|---|
| `victims`, `used_victims`, `unused_victims` | Published clean capacity victims and their final usage class |
| `usedVictimHits`, `unusedVictimHits` | Distinct used/unused victim lifetimes first touched by demand |
| `streamUsedVictimHits`, `streamUnusedVictimHits` | The same hits restricted to original Stream source |
| `demand_queries` | Accepted demand lanes, including same-address lanes; throughput diagnostic |
| `duplicate_victims` | Incoming victim whose address existed before this batch's deletions |
| `fifo_full_evictions` | Insertions that discard the oldest valid victim after hit/duplicate deletion |
| `fifo_entry_cycles` | Sum of FIFO occupancy over cycles |
| `fifo_occupancy_window_start/end` | Occupancy at performance reset/dump boundaries |

No table-miss rate is used or counted. `duplicate_victims` can coincide with a hit
of the old lifetime in the same batch, so it is not always an extra deletion in
an occupancy conservation equation. Reuse is independent of whether the demand
hit DCache or merged into an MSHR; these counters do not prove an extra miss.

For a short diagnostic run, enable `tracePDBVictimObserver0 1` and ChiselDB dumping
(`--dump-db`, selecting `PDBVictimObserver0` with the existing runner facilities).
The table records each completed event batch: input victim, all demand lanes,
used/unused and Stream-only hits, duplicate flag, FIFO eviction identity and final
occupancy. Idle batches are omitted. Its `STAMP` is debug metadata only and is
never stored in a victim entry or used for expiration.

Capture the complete history from reset with observer and trace on; an ROI-only
trace does not reconstruct victims already in the FIFO. Check it read-only with:

```sh
python3 scripts/check_pdb_victim_trace.py /path/to/run.db --table PDBVictimObserver0 --entries 256
```

The checker uses an independent logical FIFO model. It verifies every recorded
hit count, source classification, duplicate, full eviction identity and occupancy,
and returns summary JSON. Missing tables, empty traces and mismatches fail.
Capture completeness still depends on the runner; a valid replay cannot prove
that the runner recorded every event.

## Validation and next user data

Directed tests cover FIFO holes/full eviction, duplicate refresh, same-cycle
ordering, slot reuse, back-to-back queries, all 256 entries, prolonged idle
residency, and sustained three-hit cycles. Random traffic compares capacities
1/7/64/256 against a software list. PB integration tests cover stalled Release,
last S2 use, corrupt/Probe/Store/cancel exclusions and source preservation. A test
of the actual LoadPipe covers hit/miss/replay attempts, prefetch exclusion and
S1/S2 kills. The trace parser has independent valid/corrupted/empty fixtures.

Reproduce the directed checks with:

```sh
mill -i xiangshan.test.testOnly cache.PDBVictimObserverTest cache.PDBVictimIntegrationTest cache.PDBVictimDemandTest cache.PDBConfigurationTest cache.PrefetchDataBufferTest
python3 -m unittest discover -s scripts/tests -p test_pdb_victim_trace.py
```

After source review and successful Verilog generation, the user runs:

1. Functional smoke/difftest with observer off and on, using the same binary,
   PDB64/LRU, fixed64/release settings. Compare architectural results and cycles.
2. A short complete trace on representative capacity-pressure workloads and
   run the checker. Include used and unused victims and full-table replacement.
3. Synthesis/STA for the 256-entry observer; optional 64/128 entries show cost
   sensitivity. The three-lane CAM, insertion encoder and FIFO shift fanout are
   the main paths to examine. Count banking alone does not prove timing closure.

The planned PPA cost matrix is PB16/64 crossed with observer64/128/256, all using
LRU, fixed64 and release. PB16 is a hardware-cost diagnostic, not an additional
required performance baseline. The primary H2 handoff remains PB64/observer256.
The selected synthesis/STA flow and frequency/area limits must come from the user;
no timing-closure claim is made from RTL generation.
For H2 area/STA, retain the observer result ports in a module-level synthesis or
explicitly preserve the passive instance. A later whole-chip synthesis can prune
an observer with no functional consumer; that is not evidence of zero hardware
cost. H3 will supply a functional consumer separately.

No RTL performance workload, system difftest or PPA experiment is run by the
agent. Verilog generation and directed tests do not establish physical frequency,
area or workload noninterference. H3 waits for user review and the necessary
functional/timing evidence.

## Delivery validation (2026-10-06)

All 21 Scala cases above and all 3 Python checker tests passed. The repository
format task (`mill -i xiangshan.checkFormat`), source self-review and staged
whitespace checks passed. Backend files outside the formatter scope were reviewed
manually.

Fresh `DefaultConfig` generation passed for both `make verilog` (536.9 s) and
`make sim-verilog` with Constantin/ChiselDB (817.7 s). Release RTL retains all
256 valid entries and 768 demand/address comparisons. Simulation RTL contains the
observer enable/trace readers and the expected `PDBVictimObserver0Writer` fields.
Logs confirm PDB64/LRU, fixed64, background move off and observer256/three lanes.
Source hashes match the test/build snapshot; logs, commands and generated hashes
are retained locally in `.planning/h2-observer/`.
