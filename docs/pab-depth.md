# PAB-based Stream depth control

`feat-pab-depth` is based on `6891f912cddf49318ca89d69a37da44dda49c882`.
DefaultConfig enables a metadata-only 64-entry prefetch address buffer (PAB),
a persistent 256-entry victim FIFO, and competitive L1 Stream distance control.
There is no PDB data array, cache-line ownership, virtual data response, or
PDB-to-DCache move. The original DCache geometry, replacement, miss admission,
merge/refill/coherence paths, prefetch-source flags and manual prefetcher enables
remain those of the base. Automatic prefetcher shutdown is opt-in, matching the
lx comparison; its default is off for Stream, Stride and Berti. PAB dynamic depth
control remains enabled independently of that shutdown setting.

## Observation contract

PAB observes completed, non-denied/non-corrupt DCache installations originating
from standalone MainPipe hardware read-prefetch MSHRs. A passive MQ tracker keeps
original physical block/source and accepted-demand consumption per MSHR lifetime,
including the one-cycle allocation handoff. Canceled, blocked and rejected demand
queries do not consume that observation. A demand compressed with a PF allocation,
or accepted before its installation, disqualifies that lifetime from PAB insertion.
Software prefetches entering through the load lanes do not allocate PAB records.
Origin is snapshotted at the MainPipe S2-to-S3 handoff so a stalled installation
cannot observe a retired/reused MSHR slot. Existing MQ prefetch statistics and flags are left unchanged; the new tracker
avoids their count/bitmap and acceptance ambiguities without repairing other logic.

PAB keeps all qualifying prefetch sources competing for capacity. It inserts
unique block addresses at successful installation, with source and used state.
Repeated physical refills of a resident shadow block do not allocate, count a
Stream insertion, or refresh LRU. Valid scalar physical Load demands from S2,
excluding `s2_kill`, query PAB regardless of actual DCache hit/miss. They mark the
virtual block used and refresh exact LRU on every access. These are virtual demand
observations, not a claim that the actual no-PDB load consumed buffered data.
Completed Store/AMO/Probe observations retire the matching virtual entry without
a capacity-victim event. Physical DCache replacements do not remove PAB entries.

When full, PAB replaces its registered oldest eligible entry immediately. A
same-edge demand use is included in the published victim's final used state.
Simultaneous touches order higher entry indices as newer. PAB registers each
victim/insert/demand batch once. It has no ready output and cannot block the cache.
It models post-refill residency/capacity, not the original PDB's reservations,
coherence locks, release latency, backpressure or changed execution timeline.

The victim FIFO records every virtual capacity victim, with source and final used
state. All scalar physical demands query it, including DCache hits. A hit consumes
the old record once; same-block lanes deduplicate. Within a batch, queries precede
insertion. Duplicate insertion refreshes FIFO position and metadata; only a full
FIFO discards its oldest remaining record. There is no age cutoff, epoch or window
clear. Stream unused-victim reuse feeds downward depth pressure. Used reuse is
observed separately and does not change depth. Victim lookup has two capture edges;
insert and late events are delayed equally before control-window accounting.

Late evidence follows the two classes used by feat-pdb-all: the first accepted
demand merging a Stream PF lifetime (deduplicated across request lanes), and a
terminal Stream attempt matching a demand or consumed-PF lifetime. The latter
counts even when MQ is not ready, but canceled/unmatched attempts do not count.
Original request type is tracked separately from the mutable source tag: NULL-tagged
SMS/software prefetches are not mistaken for demand before a demand merge.
Other prefetch sources do not contribute Stream late pressure. Reusing an MSHR
replaces the passive origin/consumed state; it never changes real MSHR fields.

## Depth policy

The release-only competitive policy follows feat-pdb-all's H3 principle, with an
additional128-block level for comparison with lx:

- Initial depth16; levels4,8,16,24,32,48,64,128, measured in cache blocks.
- A window closes at500 successful Stream PAB insertions. All closing-edge events
  belong to that window. Events accumulate during refill gaps; counters saturate
  at65535. Partial windows and FIFO history survive performance dump/reset.
- Late count<=20 gives pressure0,21..39 gives0.5,>=40 gives1.
  Unused count<=40 gives0,41..79 gives0.5,>=80 gives1.
- The pressure difference accumulates credit in its direction and clears the
  opposite credit. Ties clear both. At credit1, change at most one level and clear
  credit; saturated boundaries clear credit too.
- After a level change, one complete observation window settles without a depth
  decision. Statistics continue. Default spacing under strong pressure is thus
  one decision window followed by one settling window.
- There are no H4/H5 physical moves, unused-pressure suppression or depth4 special
  credit threshold. Those couplings require an action absent from this design.

Only L1 Stream consumes the selected monitor depth. L2/L3 distances remain640/960.
PAB64 is a virtual prefetch residency budget; the pressure distribution and actual
performance can differ from both real PDB and real DCache eviction feedback.
Matching parameters do not establish equivalent performance or a tuned optimum.

## Comparison with feat-pdbbase-lx

The reference is `feat-pdbbase-lx` at
`d1faf1ae82d86dc6b3c2e3ae8595480a4c15a7bd`. Its production changes relative to the same base affect
only `PrefetcherMonitor.scala` and `L1StreamPrefetcher.scala`; it has no PDB.
It enables the existing monitor's doubling/halving updates, selects the monitor
distance in Stream, and disables automatic prefetcher shutdown by default.
lx fixes L2/L3 distances to640/960, starts Stream at16, and saturates Stream's
doubling/halving updates at4 and128. Stride/Berti keep their existing initial
monitor distance32 and control behavior.

| Property | lx reference | PAB candidate defaults |
| --- | --- | --- |
| Initial L1 distance, in blocks | 16 | 16 |
| Distance choices | 4, 8, 16, 32, 64, 128 | 4, 8, 16, 24, 32, 48, 64, 128 |
| Upward evidence | Stream PF request matches any existing MSHR, including another unconsumed PF | first accepted demand into a Stream PF lifetime, plus Stream PF matching demand/consumed PF |
| Downward evidence | unused Stream line overwritten in the real DCache | demand reuses an unused Stream virtual capacity victim |
| Windows | independent: 1000 PF observations for timeliness, 1000 cache-use/useless observations for validity | one window of 500 successful Stream PAB insertions |
| Upward decision | MSHR-match count at least 200: double distance | late pressure exceeds unused pressure; accumulate directional credit |
| Downward decision | useless count at least 400: halve distance | unused pressure exceeds late pressure; accumulate directional credit |
| Simultaneous decisions | upward assignment wins | compare pressures; ties clear credit |
| Stabilization | no explicit settling window | one complete settling window after a change |
| L2/L3 distances | fixed 640/960 | fixed 640/960 |
| Automatic shutdown default | off | off |

lx's timeliness window also closes if cache-hit PF observations reach1000.
Its PF-to-MSHR matches are not the same event as demand waiting for a prefetch.
Its validity window uses equality with1000, so multiple observations on one edge
can skip the intended boundary. These remaining reference implementation details
are preserved by the bounds and initialization changes.

The default comparison now has common outer distances640/960, initial Stream
distance16, bounds4..128, and Stream/Stride/Berti automatic shutdown disabled.
The existing Constantin records `Stream_enableDynamicPrefetcher0`,
`Stride_enableDynamicPrefetcher0` and `Berti_enableDynamicPrefetcher0` default to0
in both branches and permit explicit opt-in. No performance run is supplied here.

Keep each full policy's observation definitions, window, thresholds and step rule
for the primary policy comparison. If needed, separately test common depth
choices/windows to identify the contribution of the steps and observation
cadence. Those are modified policies and must be
labeled separately. Do not transfer the numerical thresholds between windows:
PF attempts, successful insertions and later victim reuse are different event
populations, and reuse can refer to insertions from an earlier window.

Use fixed-distance16/32/64 runs as controls, with identical checkpoint/warmup,
measurement length and aggregation weights. In addition to IPC, compare actual
distance residency and transitions, accepted demand/PF merges and wait cycles,
real unused-PF replacements, virtual victim reuse, MSHR occupancy, and L2/memory
traffic. Collect common passive observations in both candidates if building a
same-tree controller comparison. A victim query must include DCache hits: a
virtual victim can still be physically cached. Wrong PFs never revisited do not
produce victim-reuse pressure, so that metric alone does not measure bandwidth
waste. PAB may retire a line before the real cache does, but a downward reuse
event still waits for a later demand; shorter feedback latency is not guaranteed.

## Controls and verification

DefaultConfig selects the dynamic candidate. PABFixedDepthConfig retains identical
observations while fixing depth64. A WithPABDepth overlay can change PAB/FIFO sizes
or disable the extension; disabling restores the base's fixed-distance path.
With Constantin enabled, initialization controls for hart0 are:

```text
enablePABAutoDepth0 1
pabFixedDepth0 64
tracePABDepth0 1
```

Setting enablePABAutoDepth0=0 selects the fixed fallback and preserves observation.
Controls require the actual initialization file to reach the simulator; they are
not a supported live-switch protocol. PABDepthWindow0 logs old/new depth, counts,
pressures, resulting credits and settling state. Pressure/credit fields use integer
half units (1=0.5,2=1). Counters include PAB insert/victim/occupancy/reuse and, under
StreamMonitor, pab_refills/pab_late/pab_windows/pab_depth_increases/decreases and
pab_cycles_at_depth_<level>. Trace capture uses the existing ChiselDB build/run path.

Directed tests cover the metadata model, FIFO, passive MQ accounting, controller,
and actual monitor-to-Stream address generation. Build/test results are recorded
in the implementation plan; no SPEC performance claim follows from these checks.

Reproduce directed checks from this worktree:

```sh
mill -i xiangshan.test.testOnly cache.PrefetchAddrBufferTest cache.PABVictimTableTest cache.PABDepthControllerTest cache.PABMissQueueTest cache.PABMissTrackerTest cache.PABConfigurationTest cache.PABDepthIntegrationTest cache.PABMainPipeTest cache.PrefetchShutdownDefaultsTest
mill -i xiangshan.checkFormat
NOOP_HOME="$PWD" make verilog CONFIG=DefaultConfig BUILD_DIR=build/pab-depth JVM_XMX=40G
```

Use a fresh BUILD_DIR for each configuration; the existing Makefile output target
does not encode the CONFIG argument. Directed simulation and RTL generation do
not establish SPEC benefit or physical timing/area.
