# DCache victim based Stream depth experiments

Both experiments start at `c3ba20f01` and use `DefaultConfig`. The hardware PAB is
removed: its old classes remain only for baseline directed tests. Real cache
geometry, replacement, admission, coherence and prefetch enables are unchanged.
Only the Stream L1 distance consumes feedback; L2/L3 remain 640/960 blocks.

## Shared observation contract

- Only committed voluntary removal of a valid old DCache line is observed.
  Capture the old physical block address and its prefetch/access metadata;
  clean victims count too. Invalid-way fills, same-line permission upgrades,
  probes and speculative/replayed replacement attempts do not allocate history.
- The 256-entry victim FIFO admits **Stream and not used** only. Demand blocks,
  used Stream blocks and other prefetch sources cannot displace its history.
  Concurrent demand first use is included in the outgoing used state, including
  demands while MainPipe waits for completion.
- Scalar physical demand queries retain the original observation definition.
  Multi-lane hits on one record count once and consume it once. Queries precede
  insertion; a same-cycle demand cannot consume the newly inserted record.
- Any successful physical installation removes an older record for that block.
  Installation takes priority over a query of the old lifetime. An installation
  and victim for the same address do not retain a stale victim record.
- Keep the existing independent MSHR late tracker. Only unused victim reuse
  generates downward pressure. Never-reused prefetch waste and demand victims
  evicted by prefetches are outside this signal's scope.
- A Stream fill means the existing `pabRefill` successful-install observation:
  standalone MainPipe read-prefetch origin, Stream source, not consumed before
  the S2-to-S3 origin snapshot, valid coherent result and no denied/corrupt refill.
  There is no virtual-PAB duplicate filter; each qualifying real installation
  counts again. The retained `pab*` IO/parameter names are compatibility names.

## Experiment A: fill500

Branch `feat-dcache-victim-fill500`: close a window on the 500th qualifying Stream
fill. Closing-edge fill/late/reuse events all belong to that window. Retain the
original 16-bit saturating event counters and all original decision parameters:

- initial depth16; levels4,8,16,24,32,48,64,128;
- late<=20 /21..39 />=40 maps to pressure0 /0.5 /1;
- unused<=40 /41..79 />=80 maps to pressure0 /0.5 /1;
- net directional credit1 changes one level, then one whole settling window;
- ties clear credits; no automatic return to the initial depth;
- automatic prefetcher shutdown remains disabled by default.

## Experiment B: cycle16384

Branch `feat-dcache-victim-cycle16384`: check every16384 cycles. Close an
observation window only after at least128 qualifying Stream fills have accumulated
since the last close; otherwise retain fills, late, reuse, credits and settling
state until a later check. This is a minimum-sample period policy, not an absolute
wall-clock timeout. If Stream produces no more fills, it will not change depth.

Compare exact rates against the same per500-fill thresholds: `late*500` versus
`fills*20/40`, and `reuse*500` versus `fills*40/80`. No integer division or rounding
is used. All events on the closing edge are included. Every eligible closed
window permits at most one decision; a settling window is an eligible closed
window, not a skipped period. This deliberately changes cadence and sample size
relative to fill500 while retaining the rate boundaries and competitive policy.
32-bit saturating accumulators protect sparse windows from the old16-bit limit.
The trace additionally records actual window fill count and elapsed cycles.

Both configurations are available in both code trees as `DCacheVictimFillConfig`
and `DCacheVictimCycleConfig`. The branches differ only in which window policy
`DefaultConfig` enables, so ordinary DefaultConfig CI runs select the intended
variant. Compatibility configuration `PABFixedDepthConfig` keeps fixed64.

## Running and interpretation

Use `DefaultConfig` on each branch and a fresh build directory. Both retain
`enablePABAutoDepth0`, `pabFixedDepth0`, `tracePABDepth0` for existing initialization
scripts. Fixed mode preserves the same observations. Controller and history state
survive PERF dump/reset, so compare warmup as well as the formal window.

New counters include `stream_fills`, `victim_inserts`,
`victim_unused_stream_reuse`, `victim_fifo_evictions`, `victim_refill_removals`,
`victim_rejected_used`, `victim_rejected_other_source` and `victim_entry_cycles`.
`victim_cycle_checks`, `victim_deferred_checks`, `victim_window_fills` and
`victim_window_cycles` describe sampling and cadence.
Existing `pab_late`, `pab_windows`, depth residency and transition counters remain;
`pab_refills` now aliases physical qualifying Stream fills. A victim reuse event
is historical evidence, not a proof of a critical-path cache miss.

No SPEC performance claim is made. Use identical checkpoint sets, seeds, emulator
backend, DRAM configuration, initialization files and warmup/measurement limits
for the two manual performance runs.

## Directed validation

The suites `DCacheVictimTableTest`, `DCacheVictimMainPipeTest`,
`DCacheVictimFeedbackTest` and `DCacheVictimCycleControllerTest` cover selective
admission, FIFO replacement/deduplication, refill lifetime removal, clean and dirty
old-line addresses, pipeline stalls/concurrent first use, probe exclusion,
closing-edge alignment, exact normalized thresholds, deferred sample windows,
credit/settling and long sparse-window counters. Existing PAB controller, actual
Stream address-generation, configuration, MQ late and shutdown-default tests
provide regression coverage. These tests are not SPEC performance runs.
