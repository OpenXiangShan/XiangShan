# L2 Stride/PHT hint offload to L3

## Reference and scope

Reference: OpenXiangShan/GEM5 `l2-stream-offload-default`, commit
`8f30f6a382a8d1f26bbc1937b500fd9edc2f5eeb`, especially
`L2CompositeWithWorkerPrefetcher::rxHint` and `configs/example/kmhv3.py`.
XiangShan base: `d48cd2dd79726c2d7a1264b3c7a50679c77461e2` (`kunminghu-v3`).
XSCache base pinned by that commit: `300515bc7b2f65ca988041a984204489d80ec2fb`.
No alternative XiangShan offload branch was used as a reference.

The implementation targets performance evaluation. It redirects selected
upstream hints before L2 allocation; it does not disable prefetch generation,
change CDP degree, or send an L2 demand read on behalf of an offloaded hint.
The existing CHI `StashOnceShared` path brings the line into OpenLLC without
allocating it in the requesting L2. A later demand can fetch it from L3.

## Policy and source identity

For each eligible source independently:

```
CDP / all_L2_dequeues > 0.2 && useful[source] / L2_dequeues[source] < 0.8
```

The hardware evaluates `5 * CDP > total` and `5 * useful < 4 * sent`.
Both inequalities are strict. Zero total or source counts disable offload.
Counters are cumulative 64-bit counters since reset, not a moving window;
products retain their full width. The policy result is registered to keep the
wide comparison out of the hint routing critical path. Statistics have the
normal pipeline/feedback delay, unlike GEM5's instantaneous function call.

Despite the GEM5 branch/parameter name, its allowlist is **SStride and SPht**,
not SStream:

| GEM5 source | XiangShan source | Eligible |
|---|---|---|
| SStride | Prefetch2L2Stride | Yes |
| SPht | Prefetch2L2SMS, PHT predictions only | Yes |
| SStream / AGT | Prefetch2L2Stream | No |
| Berti, BOP, PBOP, TP, NL, CDP | Corresponding existing source | No |

SMS previously labelled all AGT/PHT/internal-stride predictions as SMS.
`SMSPrefetcher` now preserves the generating source through its region filter:
AGT maps to Stream, internal stride maps to Stride, and PHT maps to SMS.
Provenance is stored **per block**, because predictions from different engines
can merge into the same region. The first producer of a block retains ownership;
merging new blocks does not relabel already-pending or already-issued blocks.
This adds 4 bits per block to the region-filter entry in place of a single
4-bit region source (960 extra bits for the default 16 x 16-block filter).

L2 denominator counts are `io.req(bank).fire` after the prefetch queue and
pipeline, across every bank and every source. They do not count generated,
blocked, overwritten, or L3-offloaded requests. Per-source useful counts use
CPU demand directory hits on an unaccessed prefetched line. They deliberately
exclude `pfStatInMSHR.hitPf`: the existing CDP monitor combines directory hits
with late MSHR hits, which is not the GEM5 `pfUseful` numerator.
The RTL uses its existing directory metadata and CPU request classification;
this is an architectural mapping, not a claim of cycle-by-cycle simulator
identity. Coherence/replay and upstream-prefetch semantics should be checked
against traces when comparing GEM5 and RTL performance.

## Routing and contention

- Routing happens after the existing two-cycle receiver delay and respects both
  the CSR receiver gate and `pfRcv_enable<hart>`.
- An accepted offload enters a separate 16-entry, non-overwriting FIFO. It is
  suppressed from the L2 receiver in the same cycle (with an assertion).
- If that FIFO cannot accept, the hint takes the original L2 path. The hint-only
  source cannot be stalled. This fallback preserves progress but is an explicit
  finite-hardware difference from GEM5's hint forwarding.
- Native L3 hints retain their existing queue. A round-robin arbiter shares the
  16 existing stash entries between native and offloaded traffic. Accepted
  offloads cannot be overwritten by native requests.
- The native queue can still overwrite on sustained overload, as before; a new
  counter makes this possible performance cost visible.
- With elaboration-time offload disabled, the original native stash allocator
  path is retained and no offload FIFO/arbiter is instantiated.

The cumulative policy intentionally follows the reference: once traffic is
redirected, fewer new L2 samples are collected for it. No periodic probes,
hysteresis, recovery sampling, or hidden warm-up threshold have been added.

## Build and A/B evaluation

Fetch the pinned XSCache commit with the XiangShan branch:

```sh
git submodule update --init --recursive
mill -i xiangshan.compile
mill -i XSCache.offloadTest.runMain xscache.coupledL2.prefetch.L2HintOffloadTest
mill -i XSCache.offloadTest.runMain xscache.coupledL2.prefetch.L2HintOffloadElaborate \
  --target-dir build/l2-offload --target systemverilog
mill -i xiangshan.offloadTest.runMain cache.SMSOffloadSourceTest
```

The Scala `L2CacheConfig(..., hintOffload = true)` option enables the hardware;
`DefaultConfig` opts in. Other configurations retain the default `false`.
The feature also requires CDP and an upstream receiver. XSCache's corresponding
parameter is `L2Param.enableL2HintOffload`.

For a single generated `DefaultConfig` emulator, create `baseline.cst`:

```
l2pf_hintOffloadEnable0 0
```

Create `offload.cst`:

```
l2pf_hintOffloadEnable0 1
```

Run each with `--cst-file=baseline.cst` or `--cst-file=offload.cst`, using the
same checkpoint, warm-up, measured instruction count, seeds and memory model.
For multiple harts, set every hart's suffix explicitly. Constantin is loaded
at startup; this is not a mid-run CSR. Use this same-branch disabled baseline:
source attribution has changed relative to the parent commit.

## Performance evidence to collect

| Counter suffix | Meaning |
|---|---|
| hint_offload_l2_total_sent / hint_offload_l2_cdp_sent | Actual L2 dequeues and CDP share |
| hint_offload_{stride,sms}_l2_sent / _l2_useful | Per-source policy denominator/numerator |
| hint_offload_{stride,sms}_eligible_cycles | Time the per-source policy is active |
| hint_offload_{stride,sms}_accepted | Accepted redirects by source |
| hint_offload_candidate / _accepted / _fallback_l2 | Eligible hints, redirects, congestion fallback |
| l3_offload_accept / l3_offload_alloc / l3_offload_queue_full | Redirect queue pressure and progress |
| l3_prefetch_native_overwrite | Native hints lost to queue overwrite |
| l3_prefetch_txreq_fire / _blocked / l3_prefetch_rxrsp_fire | Shared stash CHI traffic and backpressure |
| OpenLLC stashOnceSharedReq / stashOnceSharedDirMiss / stashOnceSharedRefill | Downstream traffic and memory fills |
| OpenLLC l3prefetchHitInCache / l3prefetchUseless | Combined native + offload usefulness/pollution |

`candidate = accepted + fallback_l2` should hold for identical measurement
intervals. L3 alloc/completion counts can lag accepted counts by outstanding
queue and transaction occupancy; do not compare them as exact equalities
without draining. L3 hit/usefulness counters combine native and redirected
traffic, so they cannot alone measure offload-specific accuracy.

Compare per-slice IPC/cycles first, then L2 demand misses and MSHR pressure,
CDP useful hits, L3 demand latency, native prefetch overwrite, and memory
bandwidth. The intended gain is less L2 pollution/competition while retaining
L3 coverage. Extra L3 traffic, delayed prefetch arrival and displaced native
hints are plausible regressions; report improvements and regressions separately.
No workload speedup is claimed without these paired runs.

## Validation

Verified on the final source state:

- XiangShan Scala compilation: passed.
- `SMSOffloadSourceTest` under Verilator 5.38: passed.
- `L2HintOffloadTest` under Verilator 5.38: passed.
- Four-bank L2 SystemVerilog generation: passed. The elaborator reports three
  existing index-width warnings in GrantBuffer, MSHRBuffer and SourceB.
- Git whitespace checks: passed (respecting the existing CRLF Prefetcher file).

The directed RTL tests cover strict thresholds, zero samples, independent
source decisions, full-width arithmetic, reset, FIFO saturation, retention,
round-robin progress and randomized backpressure. The integration target
elaborates the actual four-bank L2 with CDP and receiver enabled to SystemVerilog.
The SMS filter test exercises mixed PHT/AGT/stride predictions in one region,
first-producer ownership, output backpressure and duplicate suppression.

A full SPEC/checkpoint performance sweep and whole-system differential
simulation remain necessary before claiming a performance improvement or
merging the feature into the mainline.
