# MemBlock Verification Results

This file is the compact current evidence index for the MemBlock UT. Current
contracts, acceptance rules, and verification-point status remain authoritative
in [`VERIFICATION_PLAN.md`](VERIFICATION_PLAN.md), [`ORACLES.md`](ORACLES.md),
and [`IO_COVERAGE.md`](IO_COVERAGE.md). Confirmed CPU bug details remain in the
individual `CPU_BUG_*.md` documents.

## Test Taxonomy

Only two stimulus families exist:

| Family | Scenarios | Description |
| --- | ---: | --- |
| `directed-tests` | 84 | Fixed semantic test sequences; a seed may randomize data, addresses, lanes, and timing, but not the test identity. |
| `constrained-random` | 1 | `constrained-random`, driven by the common constraint interface and `coverage`/`spec`/`corner` presets. |

There is no third scenario family. `make benchmark-tests` runs all 85 scenarios
once (one seed, one action budget per scenario); it is a benchmark command, not
a test family or a new mode. `make regression`, `make final-regression`, and
`make endurance-regression` run `constrained-random` with different seed and
duration budgets.

The constraint interface version is metadata (`constraint_schema=53`), not a
scenario or a work-round name.

## Current Baseline

### 2026-10-24

- Inventory: **85 scenarios** = **84 `directed-tests` + 1 `constrained-random`**.
- `make benchmark-tests` with 8 workers: **85/85 pass in 781.752 s**.
- `constrained-random` seed 1/16,384: **760.884 s**, 1,840,675 DUT cycles,
  ~21.5 actions/s, ~2,419 cycles/s.
- Next longest scenarios: `vector-segment` 386.257 s,
  `vector-guest-fault-boundary` 53.948 s.
- Unit tests: **282 pass**.
- `make check-rtl`: pass.
- Frozen runtime binary SHA-256:
  `4185c7ce787c05ac309a05fea58850175689e3a7d603bbd53c4af2a99c9b2404`;
  model SHA-256:
  `d659328ca82904a6ee33d3c5bc4ec32202a81e004c38e8d2fad5b57606b60d3f`.
- Evidence: `build/memblock/taxonomy-20261024/benchmark.json`,
  `benchmark.md`, and 85 per-run coverage reports under `coverage-reports/`.

This is a capability/performance baseline, not closure. The 48-hour
`constrained-random` campaign and the 200-hour endurance campaign have not run
under the current taxonomy.

### 2026-10-24 mixed-window legality correction

A 16-seed `constrained-random --constraints coverage` run found two failures
that were reproduced deterministically at seeds 5 and 7. Neither was a CPU bug.
Seed 5 failed in heterogeneous window 950: an older `lw` at `0x8027afff`
returned byte `0x67` from a younger `sh` at the same address instead of the
pre-store byte `0x43`.

Internal observation localized the cause to the testbench, not the DUT. The
window's scalar store crossed a 4-KiB page. To make its split-store replay
progress, the window made that younger store the backend ROB head and pulsed
`scommit`. The older load was still pending. The StoreQueue therefore committed
the store (`sq_cmt=25, committed[24]=1`) while the older load was outstanding;
the split load's second part then read the now-visible store byte. A real
backend cannot make a store the ROB head or commit it before an older load
retires. Seed 7's earlier miss-burst accounting failure belongs to the separate
grant-pairing class described below; it passed after the mixed-window
correction, and the grant-pairing correction then addresses that class directly.

The heterogeneous window now keeps its scalar store within one 4-KiB page.
Page-crossing scalar-store replay and exact readback remain covered by
`scalar-misaligned` and `misaligned-stores`, so the mixed window still owns
line-crossing misalignment but never manufactures an illegal commit order.
`single-load` also gained two directed younger-store ordering cases: an older
aligned load miss and an older split cross-line load, each with a younger store
issued while the older refill is held. Both require the load to retain its
pre-store value and pass.

A second false-positive class surfaced after that correction. Seeds 8 and 10
failed the `random-load-merge-manager` and `random-miss-burst-manager` gates
with one more GrantAck than GrantData refills. The extra acknowledgement was a
legal background `AcquirePerm` (data-less `Grant`) from the hardware prefetcher,
which the gates did not include in their grant pairing. Both gates now compare
the GrantAck delta against the sum of GrantData refills and `AcquirePerm`
grants; a genuinely duplicate or lost acknowledgement still fails. Because the
artifact manager tuple records raw refills and acknowledgements, the streaming
verifier also allows the artifact-global difference up to the recorded
`dcache_acquire_perms`; the action-local gate still requires exact pairing.
Seeds 8 and 10 then complete all 16,384 actions.

With the corrected harness, seeds 5, 7, 8, and 10 all complete all 16,384
actions (`MEMBLOCK_CONSTRAINED_RANDOM_PASS`). The frozen runtime was regenerated
as binary SHA-256
`98b1260afec033e6358ebc52171de7bf6b537ffd27565d3fa16c8ee34b20ae17` (model
SHA-256 `d659328ca82904a6ee33d3c5bc4ec32202a81e004c38e8d2fad5b57606b60d3f`).
The previous 16-seed artifacts and their pre-correction runtimes are superseded
for campaign evidence; their failures are harness-provenance findings, not CPU
bugs.

The final 16-seed `constrained-random --constraints coverage` capability run
passes **16/16 seeds**, **262,144 actions**, in 2,227 s
(artifact `build/memblock/regression-20261024-final-coverage-16.json`) on the
runtime frozen at that point (binary SHA-256 `98b1260a...`). This is a capability
run under the 16,384-action budget, not the 48-hour or 200-hour closure campaigns
required by
[`VERIFICATION_PLAN.md#regression-acceptance`](VERIFICATION_PLAN.md#regression-acceptance).

The directed `dcache-release` additions changed only the test scenario, not the
constrained-random generator. The harness was re-frozen for those scenario
updates as binary SHA-256
`4375bf5ea7a6c155cb0570b0c4d979b8fe671258b664bcf54a7116fd5c6a85b9`; a 4-seed
4,096-action `coverage` smoke run passes 4/4 (16,384 actions, 685 s,
`build/memblock/regression-20261024-round5-smoke.json`). The larger 16-seed
artifact remains prior-runtime evidence; the constrained-random code path is
unchanged. The later replacement+atomic scenario addition re-froze the runtime
as binary SHA-256
`e4ffdd74675f0ed8fea2e790d86bbc3a527b9a5c9d9742beb80c4f7f0389a1b9`; the same
`constrained-random` source is unchanged and the frozen binary includes the
atomic composition.

### 2026-10-24 single-set replacement and Probe composition

`dcache-release` was corrected from stale 128-set/8-way constants to the
generated 256-set/4-way geometry. Its first phase now drives six dirty lines
through one set, requiring two attributed dirty replacements and exact
`ReleaseData` images.

A second phase composes replacement with a same-set Probe. Four dirty stores
fill all four ways; a fifth store forces one dirty victim release. The C channel
is held until a Probe to the just-refilled fifth line is accepted, so the
victim release and the `ProbeAckData` overlap. The test requires exactly one
victim `ReleaseData` with its immutable line image, one dirty ProbeAckData with
the fifth line image, and five exact readbacks. The scenario passes
(`probe_overlap_release_data=1 probe_overlap_victim_releases=1
probe_overlap_probe_data=1 probe_overlap_readbacks=5`).

A third phase composes the held victim release with a `CBO.CLEAN` on the same
just-refilled dirty line. The manager delays `CBOAck` by 1,024 cycles so the
CMO's dirty ProbeAckData and the older victim ReleaseData complete while the
CMO is pending. The test requires the CMO request count, `flushPipe`
completion, the victim image, the CMO probe image, and five readbacks to be
exact (`cmo_overlap_release_data=1 cmo_overlap_victim_releases=1
cmo_overlap_probe_data=1 cmo_overlap_requests=1 cmo_overlap_readbacks=5`). No
new CPU bug was found; atomic replacement composition remains a gap.

### 2026-10-24 PMP TOR lock and NAPOT size matrix

`pmp-contracts` now covers the architectural rule that a TOR entry's base
address is locked when the following entry is a locked TOR entry. After
installing `[lower, upper)` through entry 1 with entry 1 locked, the test
attempts to move `pmpaddr0` beyond `upper`; the write must be ignored and the
original range must remain readable. A write that incorrectly took effect
would make the TOR range empty and deny the old range; the independent load
oracle detects that without reading DUT state. The same scenario then
exercises NAPOT sizes 4 KiB, 8 KiB, 16 KiB, 64 KiB, 256 KiB, 1 MiB, and 4 MiB
at allowed first/last and denied immediately-below/above doublewords. The full
scenario passes 49 cases with `tor_next_lock=1` and `napot_sizes=7`.

### 2026-10-24 LR reservation invalidation by Probe

`atomic-contracts` now checks that a manager Probe to the LR line invalidates
the LR/SC reservation. After a warmed `LR.D`, a toN dirty ProbeAckData returns
the exact line image and invalidates the line; the following `SC.D` must fail
with result `1`, issue no new DCache request, and leave the backing memory
unchanged. A final readback returns the original value. The scenario passes
with `reservation_probe_invalidated=1`. This observes the architectural
reservation outcome, not the internal reservation bit.

### 2026-10-24 PMP W/R/X permission WARL

`pmp-contracts` adds a reserved `W=1,R=0` NAPOT entry and an `X`-only NAPOT
entry. The reserved entry must behave as a data-deny entry for a store, and the
execute-only entry must deny data loads as well as stores. Both cases require
the access fault and no DCache/Uncache data request. The scenario passes 56
cases with `warl_permission=2`; this checks the architectural W-implies-R WARL
and the distinction between execute and data-read permission.

### 2026-10-24 PMP-before-PBMT priority

`pmp-contracts` adds two Sv39 cases whose page-table pages are PMP-allowed but
whose final physical page is PMP-denied, once with PBMT=NC and once with
PBMT=IO. Each translated load must terminate with `LoadAccessFault` and must issue no
DCache or Uncache data request; a TLB-hit store variant must terminate with
`StoreAccessFault` and issue no data request. PTW traffic remains allowed. The
positive PBMT/Uncache routing is covered separately by `translation-pbmt` and
`uncache-widths`. The scenario passes 53 cases with `pbmt_pmp_cases=2` and
`pbmt_pmp_stores=2`. Misalignment priority remains a gap.

### 2026-10-24 replacement composed with an atomic refill

`dcache-release` adds an atomic composition. Five dirty stores occupy one
four-way set and the fifth forces a held dirty victim release. A cold
`AMOADD.D` in a different set is issued while C is held; its refill response is
delayed, and a Probe to the just-refilled fifth line releases C. The test
requires the atomic's exact old-value writeback, no early writeback while the
refill is held, the exact final memory/readback, and the victim/probe C
payloads. The scenario passes with `atomic_overlap_release_data=1
atomic_overlap_victim_releases=1 atomic_overlap_probe_data=1
atomic_overlap_readbacks=1`. No new CPU bug was found.

### 2026-10-24 replacement composed with a denied CMO response

`dcache-release` adds a denied-CBOAck composition. Four dirty ways and a held
fifth-store victim release are combined with a `CBO.CLEAN` on the dirty
just-refilled line. The CMO's CBOAck is delayed and denied while two Probes
keep C stalled: one returns the dirty fifth-line image and one invalidates an
unrelated clean line. The error must not cancel the older committed victim
`ReleaseData`, must not suppress either Probe response, and must leave the
backing bytes and all five readbacks exact. The scenario passes
(`cmo_error_release_data=1 cmo_error_victim_releases=1
cmo_error_probe_data=1 cmo_error_requests=1 cmo_error_readbacks=5`). Up to two
youngest committed LQ entries remain at the directed-drain lookahead boundary
and are reported as `cmo_error_lq_pending=2`; their RF data is checked. No new
CPU bug was found.

### 2026-10-24 clean CMO reset cancellation

`reset-partial-load` now also resets while a clean `CBO.CLEAN` has an accepted
manager request and a delayed `CBOAck`. The test cancels only that CMO store
identity at the completed reset boundary, rejects any stale address/data
writeback, and requires a fresh CMO on the same line plus a readback to
complete with the backing image unchanged. The scenario passes with
`cmo_reset=1 cmo_canceled=1 cmo_fresh=1 cmo_readbacks=1`. This uses a
store-reset cancellation API; dirty-cache persistence remains unclaimed.

### 2026-10-24 corner-preset campaign

The `corner` preset was exercised on the frozen runtime. A 4-seed x 4,096-action
run passes **4/4** (16,384 actions, 1,625 s, artifact
`build/memblock/regression-20261024-corner-smoke-4.json`). An 8-seed x
16,384-action run passes **6/8**; seeds 2 and 3 hit the ad-hoc 3,600-second
per-seed runner timeout, while their 4,096-action versions pass in the smoke
run. The timed-out seeds emitted no `MEMBLOCK_*_FAIL` or terminal summary; the
corner preset's rare error/retry traffic reduces throughput to roughly 2.5
actions/s on this host, so 16,384 actions can exceed one hour per seed. This is
a time-budget/throughput observation, not a functional failure; a future 16k
corner campaign needs a larger per-seed timeout or more than eight workers. It
is not closure evidence for the 48-hour or 200-hour campaigns.

### 2026-10-24 out-of-order load D responses

`single-load` now holds two cold-load refills for distinct sources, releases
the younger load's response first, and requires the younger identity to
complete with its own line data while the older load remains pending. Releasing
the older response then produces both exact writebacks and LQ retirement. The
scenario passes with `reverse_order=1 reverse_writebacks=2`; D-response
ordering across distinct sources is therefore matched by identity rather than
arrival order.

## Current Verification Position

- `directed-tests` cover the focused protocol, translation, vector, cache,
  reset, protection, and top-level contracts registered in the catalog.
- `constrained-random` samples and reports coverage; coverage is observational
  and is not a PASS gate.
- The redirected-load epilogue in `constrained-random` randomizes supported
  fields (address, opcode, lane, delay) while keeping redirect semantics fixed.
- `frontend-bridge` is a `directed-tests` scenario. It is not part of the
  `constrained-random` regression.

## Remaining Gaps

- Malformed or duplicate manager responses and broader PMP/PMA/hypervisor
  crosses remain. Replacement composed with a same-set Probe, a single CMO, a
  denied CMO response, and a cold atomic refill is covered by
  `dcache-release`.
- Single-set associativity coverage and some vector-segment fault/progress
  crosses remain.
- Constrained-random mutation sensitivity is not yet demonstrated.
- The 48-hour and 200-hour campaigns, and any closure claim, remain future work.

## Evidence Policy

- Acceptance uses architectural observations and externally visible protocols,
  not DUT-internal state.
- A passing finite capability run is not a closure campaign.
- Failed or repaired historical investigations remain in Git history and the
  `CPU_BUG_*.md` / `HISTORICAL_BUG_AUDIT.md` provenance documents; they are not
  current regression results.
- Only a deterministic reproducer that excludes harness, model, scoreboard,
  stale-build, and provenance errors establishes a CPU RTL bug.
- The authoritative closure duration, seed count, worker count, and artifact
  checks are in
  [`VERIFICATION_PLAN.md#regression-acceptance`](VERIFICATION_PLAN.md#regression-acceptance).
