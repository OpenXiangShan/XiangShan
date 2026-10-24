# MemBlock Verification Results

This file is the compact evidence index for the MemBlock UT. Current contracts,
acceptance rules, and verification-point status remain authoritative in
[`VERIFICATION_PLAN.md`](VERIFICATION_PLAN.md), [`ORACLES.md`](ORACLES.md), and
[`IO_COVERAGE.md`](IO_COVERAGE.md). CPU bug details remain authoritative in the
individual `CPU_BUG_*.md` documents.

> **Contract change (2026-10-24):** random scenarios no longer require a
> coverage prefix, a minimum action count, or coverage completeness for PASS.
> Coverage is sampled and written to per-run reports, but it is observational.
> Historical rows below were produced under the old coverage-gated contract and
> are not directly comparable to new runs.

## Evidence Policy

- Acceptance is based on architectural observations and externally visible
  protocols, not DUT-internal state.
- A passing finite capability run is not a closure campaign.
- Source revisions identify provenance. The UT does not gate acceptance on RTL,
  binary, controller, artifact, or metadata SHA-256 values. Hashes in retained
  artifacts are historical provenance only.
- A historical mutation is evidence that the current tests can detect a known
  bug class. A mutation failure is not itself a new CPU bug claim.
- Only a deterministic reproducer that excludes harness, model, scoreboard,
  stale-build, and provenance errors establishes a CPU RTL bug.
- The authoritative closure duration, seed count, worker count, and artifact
  checks are in
  [`VERIFICATION_PLAN.md#regression-acceptance`](VERIFICATION_PLAN.md#regression-acceptance).

## Current Baseline

### 2026-10-24 Random-Only Rebaseline

- Contract: random scenarios have no mandatory coverage prefix, no minimum
  action count, and no coverage-completeness PASS gate. Coverage is sampled and
  written to one report per run by the regression/benchmark drivers.
- Inventory: 85 scenarios (84 directed + 1 constrained-random). Legacy
  unconstrained random scenarios and the separate stress generator were
  removed; the VS-non-leaf boundary cases became the directed
  `vector-guest-fault-boundary` scenario; `random-mixed` was renamed
  `constrained-random`; `random-mixed-hunt` was removed.
- Full all-scenario benchmark: **85/85 pass in 781.752 s** (previous
  gated-generator baseline: 3,750.501 s). `constrained-random` seed 1/16,384:
  **760.884 s**, 1,840,675 DUT cycles, ~21.5 actions/s, ~2,419 cycles/s.
- Frozen runtime binary SHA-256:
  `4185c7ce787c05ac309a05fea58850175689e3a7d603bbd53c4af2a99c9b2404`;
  model SHA-256 unchanged at
  `d659328ca82904a6ee33d3c5bc4ec32202a81e004c38e8d2fad5b57606b60d3f`.
- Evidence: `build/memblock/taxonomy-20261024/benchmark.json`,
  `benchmark.md`, and 85 per-run reports under `coverage-reports/`.
- Historical rows below use the old `random-mixed`, `90-leaf`, and `matrix`
  names; they are provenance, not current scenario or campaign names.
- This is a capability rebaseline, not closure. The 48 h/200 h campaigns were
  not rerun under the new contract.

Evidence snapshot: 2026-10-23.

- UT implementation revision: `c81aa3d65`; documentation-only commits may follow.
- Kunminghu-v2 source baseline: `fe2288cb09361b65afa26be3f403520c4b496dbf`,
  merged into the UT branch as `4bd72b3a0`.
- Picker: `7499f797b56be57cf950cd40f3d14e9eba133449` (v2.1.0).
- xcomm: `a5f5dae14f3cdc8f7b35dab7b5dc8c21576b4dbd` (v1.0.0,
  native ABI 3).
- Toolchain lock: `tests/memblock/toolchain.lock.json`.
- Bootstrap provenance: `build/memblock/tools/picker.json`.
- General frozen runtime manifest: `build/memblock/runtime/runtime.json`.
- Architecture refactor runtime:
  `build/memblock/architecture-stage2/runtime/runtime.json`; base `7f83653dc`
  plus the reviewed architecture changes and bounded differential evidence below.
- Post-fix schema-53 high-finding remediation runtime:
  `build/memblock/review-schema53-20261023/runtime/runtime.json`; binary SHA
  `6e5a8e5cc03b4d0677026fbd66d38f8d432a105f2712b6b552b56a10a50c49f4`.
  Post-fix 90-leaf (90/90) and matrix (12/12) evidence is recorded below.
- Round-5 manager-response runtime:
  `build/memblock/review-schema53-20261023/runtime-round5/runtime.json`; binary
  SHA `e375c12d6b242660f7dd066d442f3ee611e75adc4d8dd9b3e7d635d7fdf36a50`.
  Stale-response `redirect` plus 90/90 and 8-job 12/12 gates are recorded below.
- Latest H fault oracle runtime:
  `build/memblock/hypervisor-oracle-runtime/runtime.json`; independently
  reviewed correction and finite replay evidence are recorded below.
- Latest schema-53 dirty-overlap qualification runtime:
  `build/memblock/dirty-ownership-runtime/runtime.json`; review, construction
  accounting and focused evidence are recorded below. The first complete
  schema-53 capability campaigns are
  `build/memblock/dirty-ownership-prototype/inventory90.json` and
  `matrix.json`; `campaign-failure-summary.json` retains the compact failure
  evidence.
- Pre-fix schema-53 prefetch-quiesce and release-index runtime:
  `build/memblock/prefetch-index-runtime/runtime.json`; retained as provenance
  for the earlier replay attempt. The current focused replay uses the
  `architecture-stage2/runtime` above; see the current-head note below the
  capability table.
- Retained schema-52 mixed capability runtime:
  `build/memblock/schema52-runtime-v3/runtime.json`.
- Latest directed reset runtime:
  `build/memblock/partial-load-merge-reset-runtime/runtime.json`; provenance:
  `build/memblock/partial-load-merge-reset-provenance.json`.
- Pressure-scheduling UT correction runtime:
  `build/memblock/pressure-scheduling-fix-runtime/runtime.json`; review and
  exact failure classification are recorded below. The preceding clean-only
  runtime remains `build/memblock/clean-release-overlap-fix-runtime/runtime.json`.
- Latest DebugModule guard extension runtime:
  `build/memblock/debug-guard-runtime/runtime.json`; focused validation and
  independent review are recorded below.
- Mixed diagnostic replay runtime:
  `build/memblock/mixed-progress-runtime/runtime.json` (frozen before the
  partial-LR extension; the mixed generator and diagnostics are identical).
- Retained schema-51 focused runtime:
  `build/memblock/segment-high-va-verified-runtime/runtime.json`.
- Latest schema-52 focused provenance:
  `build/memblock/schema52-focused-v3/provenance.json`.
- Retained schema-51 evidence provenance:
  `build/memblock/segment-high-va-development/verified/provenance.json`.

The frozen runtime manifests record artifact paths, sizes, and resolved external
libraries. Those values are reproducibility data, not hidden acceptance hashes.

## Current Capability and Closure Status

The current generator schema is 53. `constrained-random` remains the canonical mixed
workload; the schemas below add independent semantic classes and conservation
checks. Schema 48-50 have three recorded 6,144-action profile runs per schema,
but each passing set totals less than three simulator hours. They are capability
evidence, not closure evidence.

| Schema | Implemented capability | Passing evidence | Remaining status |
| --- | --- | --- | --- |
| 48 | Scalar canonical access-range faults in common mixed traffic, with 24 bins and separate fault/DCache accounting. | Focused seeds 201-205 pass all bins; SPEC seed 211 and coverage seed 212 pass 6,144 actions. Evidence: `build/memblock/canonical-focused/`, `build/memblock/canonical-regression/`. | Corner seed 213 originally stopped at action 5,948 because a global accepted-A window attributed an unrelated CPU-store request to a denied hypervisor load. Fresh review classified it as a harness attribution defect, and the causal demand-A guard now lets schema-52 `corner` seed 213/6144 pass at cycle 8,083,144. Old evidence: `build/memblock/schema48-corner-triage/`, `build/memblock/schema48-corner-diagnostic/`; fixed replay: `build/memblock/schema52-corner-replay/`. |
| 49 | Singleton vector-store canonical faults in mixed traffic, with 36 bins, exact fault VA, queue conservation, edge controls, and recovery. | Focused seeds 221-228 pass; standard SPEC/coverage/corner seeds 231-233 pass 6,144 actions each. The profiles total 9,652.961 simulator seconds (2.681 hours). Evidence: `build/memblock/canonical-vector-focused/`, `build/memblock/canonical-vector-regression/`. | Multi-active-element progress and vector loads remain gaps. No closure claim. |
| 50 | G-only scalar and singleton vector-store upper-limit faults, with 12 scalar and 18 vector bins, exact VA/GPA/cause/marker, controls, and recovery. | Focused seeds 241-251 pass; standard SPEC/coverage/corner seeds 261-263 pass 18,432 total actions. The profiles total 9,338.358 simulator seconds (2.594 hours). Evidence: `build/memblock/gstage-range-runtime/`, `build/memblock/gstage-mixed-focused/`, `build/memblock/gstage-mixed-regression/`. | Nested boundary composition and GPA wrap remain gaps. No closure claim. |
| 51 | Successful high-half canonical segment load/store traffic, with Sv39/Sv48 x direction x alignment bins and independent full-width VA walks. | `vector-segment-canonical` passes all 12 shared cases; full `vector-segment` passes in 380.498 seconds; focused mixed seeds 281-286 pass. Standard coverage seed 291, SPEC seed 292, and corner seed 293 each pass 6,144 actions with all eight bins populated; total 8,503.323 simulator seconds (2.362 hours). Evidence: `build/memblock/segment-high-va-verified-runtime/`, `build/memblock/segment-high-va-development/verified/`, `build/memblock/segment-high-va-regression/`. | The three-profile capability matrix is complete but remains below the closure duration and seed count. Nested high-half composition, segment noncanonical faults, arbitrary partial-fault progress, and broader vector progress/shape crosses remain gaps. |
| 52 | Schema-51 high-half segment coverage plus all four nested VS/G pairs crossed with load/store and aligned/misaligned bins, using per-byte independent two-stage walks. | `vector-segment-canonical` passes 12/12. Focused mixed seed 286 passes all 24 bins with 150 segment actions, 300 data uops, and 356 active elements. Coverage seed 301 now passes 6,144 actions in 1,481.949 seconds with all 24 bins; its direct replay and controller terminal line are byte-identical. Evidence: `build/memblock/schema52-runtime-v3/`, `build/memblock/schema52-focused-v3/`, `build/memblock/schema52-regression-v3/`. | SPEC seed 302 and corner seed 303 were explicitly stopped at about 40 minutes on user request; neither is claimed as a pass. The prior v1 runtime SPEC/corner runs passed, but coverage exposed the release-window oracle defect fixed here. High-half FOF, segment noncanonical faults, arbitrary partial-fault progress, and broader vector progress/shape crosses remain gaps. |
| 53 | Dirty held-refill overlap requires externally proven retained baseline coherence responsibility; up to four fresh construction attempts preserve the selected profile tuple, normal rejected cleanup retains every functional oracle, and separate accounting conserves attempts and traffic. | Reviewed private compact 424/512 and DCache-SPEC 425/512 each qualify 411 actions; forced rejection crosses all 384 enabled bins with 411 cleaned rejections and 411 accepted retries. V2 compact 424/512 and forced eight-window 424/128 pass. Evidence: `build/memblock/dirty-ownership-prototype/`. | Independent review v2 passes with gaps; focused controls and 278 units/RTL/docs pass. Integrated compact 424/512 passes with terminal byte-identical to reviewed private v2. The completed 90-leaf inventory passes 89/90: SPEC `random-mixed` seed 1 stops after 4,916 of 16,384 actions with 12 dirty-ownership construction rejections, every one an early target Release with zero acquisition or error reasons. The two-seed six-scenario matrix passes 10/12: SPEC `random-mixed` seeds 531 and 532 stop after 4,151 and 4,215 of 6,144 actions. Rejected-manager tuples show six- and eight-window depth-9/10 baseline actions, while forced one-window dirty probes complete 1,365 actions. A diagnostic replay places the first early target Release after the baseline snapshot (`after_baseline=0`) during held-refill setup, with the target line's only A request being its baseline store NtoT; the global A summary shows a background L1 stride prefetch to a different line in the target set evicting it during hold setup. The UT now quiesces the L1 stride prefetcher through its external CSR for the set-pressure action, runs 4 + 8 + 4 = 16 DUT cycles and drains DCache idle, restores the prior enable after the action, and counts releases through an O(1) full-line index with a non-full fallback. A focused prefetch replay and full campaign reruns were stopped by operator request before completion; no acceptance claim follows. This is a UT stimulus-precondition failure under mixed traffic, not a functional RTL bug. Forced rejection is harness injection; no natural ownership-loss, historical sensitivity, or closure claim. |

> **Current-head schema-53 focused replay (2026-10-23):** the `89/90`,
> `10/12`, `4,916`, `4,151`, and `4,215` values in the schema-53 row above are
> the pre-fix `dirty-ownership-runtime` record. Focused replays on the current
> `architecture-stage2/runtime` (binary SHA-256
> `624547e5bcf4ee5ae3f8fa8176ae0e25c422570f0b8852a15a0a2af0987126d3`)
> now pass seed 1/16,384 (3,586.58 s) and seeds 531/6,144 and 532/6,144
> (3,199.39 s and 3,192.78 s, run concurrently), each with
> `actual_set_pressure_ownership=384,384,384,0,0,0,0,0,0` and zero
> construction rejects. The per-bin proof is the terminal
> `actual_set_pressure_cross`: 1,536/1,536 cells are nonzero and all 384
> dirty-overlap cells are exactly 1; the ownership tuple alone is only a
> counter. These are focused replays, not the full 90-leaf benchmark or the
> 12/12 matrix; the full current-head gate runs are recorded next. Evidence:
> `build/memblock/review-schema53-20261023/`.

> **Pre-fix integrated gate (2026-10-23):** the full 90-leaf benchmark
> passes 90/90 with zero leaf failures in 3,748.449 s; the two-seed
> six-scenario matrix passes 12/12 in 3,415.183 s. The `random-mixed` leaves
> are 3,721.019 s (seed 1/16,384) and 3,242.321 / 3,225.725 s (seeds
> 531/532, 6,144 each). Evidence: `benchmark90.json`, `benchmark90.md`, and
> `matrix-531-532.json` under `build/memblock/review-schema53-20261023/`.
> These are gate-2/gate-3 capability results on the pre-fix runtime, not
> closure: the independent review records two High false-pass gaps below.

> **Schema-53 high-finding remediation (2026-10-23):** the current source adds
> a release-channel drain before the ownership snapshot and after selected C
> backpressure, requires dirty target Releases to be ReleaseData, and provides
> the harness-only `MEMBLOCK_FORCE_DIRTY_OWNERSHIP_REJECT_ONCE` hook. On the
> rebuilt private runtime (binary SHA
> `6e5a8e5cc03b4d0677026fbd66d38f8d432a105f2712b6b552b56a10a50c49f4`), normal
> seed 531/6,144 passes in 3,241.51 s with 384/384 accepted and zero rejects.
> The forced run rejects exactly once at action 4,018 and exercises cleanup
> (`actual_set_pressure_ownership=385,384,384,1,1,0,0,0,0`, nonzero
> `actual_set_pressure_rejected_manager`); its terminal then fails only the
> coverage gate because the injected retry duplicates one set-pressure bin
> (`actual_set_pressure_cross` sum 1,537). That is harness-injection
> perturbation, not a functional RTL failure. A second forced run with seed
> 1/16,384 rejects once at action 4,727 and likewise fails only the coverage
> gate from duplicated bins; it is retained as cleanup evidence, not a pass.
>
> **Post-fix integrated gate:** 90/90 leaves pass in 3,771.461 s; the two-seed
> six-scenario matrix passes 12/12 in 3,467.092 s. The final 8-job matrix gate
> also passes 12/12 in 3,255.288 s (`matrix-8.json`; `random-mixed`
> 531/532 = 3,239.406/3,253.553 s). The post-fix `random-mixed` leaves are
> 3,741.241 s (seed 1/16,384) and 3,256.142 / 3,260.364 s (seeds 531/532,
> 6,144). Evidence: `benchmark90-fixed.json`, `benchmark90-fixed.md`,
> `matrix-fixed.json`, and `matrix-8.json`.
> This closes the accepted-path mitigation for the tested seeds; the forced
> rejection is still a harness-injection terminal with a coverage-only failure,
> and no closure claim follows.
>
> **Round-5 current-head gate:** the stale/canceled-D-response test in
> `redirect` passes (`redirect_cancels_observed=2,2,0`,
> `surviving_writebacks=2`). On the rebuilt `runtime-round5` (binary SHA
> `e375c12d6b242660f7dd066d442f3ee611e75adc4d8dd9b3e7d635d7fdf36a50`), the
> 90-leaf benchmark passes 90/90 in 3,750.501 s and the 8-job matrix passes
> 12/12 in 3,287.102 s (`matrix-round5.json`; `random-mixed` 531/532 =
> 3,287.100/3,273.502 s); `verify-matrix` passes. This is the current
> manager-response late/canceled coverage; malformed/duplicate manager
> responses remain a planned gap.

Supporting oracle and diagnostic improvements retained in this evidence set:

- MMIO FP payloads distinguish NaN boxing from integer sign extension, and the
  queue oracle requires observed recovery for denied/corrupt FP loads.
- The accepted-A DCache monitor checks opcode, grow, size, source, alignment,
  mask, corrupt, and defined user-address relationships without filtering legal
  prefetch or demand-merged traffic.
- `redirect` now holds a cold D response, redirects the load, releases the
  stale D beat, and requires no writeback plus a fresh successful load:
  `MEMBLOCK_REDIRECT_PASS ... surviving_writebacks=2
  redirect_cancels_observed=2,2,0`. This covers late/canceled manager
  responses; malformed/duplicate responses remain a planned gap.
- The latest complete Python unit evidence is 305 tests (303 at the architecture
  refactor; two release-index edge tests added below); the `278` and `283` counts in older schema records are historical
  snapshots. The Picker 2.1.0 / xcomm 1.0.0 migration also rebuilt the full C++ model and passed `check-rtl`
  and `smoke` (`cycle=38`, 748 driven inputs, 20 checked outputs); the final
  harness requires `libxspcomm.so.3`.
- The reviewed Debug guard runtime passes all 90 registered leaves, including
  SPEC seed 1/16,384 (`build/memblock/debug-guard-expansion/inventory90.json`).
  This inventory predates schema 53 and does not replace its runtime checks.
- The schema-52 pre-decision runtime passed all 89 benchmark leaves
  (`build/memblock/schema52-runtime/benchmark.json`, 1,934.016 seconds). After
  the release-window decision fix, the affected 16,384-action `random-mixed`
  leaf passes under the current runtime in 1,701.059 seconds
  (`build/memblock/schema52-runtime-v3/benchmark-random-mixed.json`).

No current closure campaign has completed. The gate-2 90-leaf benchmark and
gate-3 two-seed matrix pass on the pre-fix, post-fix, and round-5 runtimes, but
they are not the duration campaign. The historical 8-hour/128-seed and
8-hour/1M-action budgets were recalibrated to **48 h/128 seeds** and **200 h/8 x
1,000,000 actions** using the measured 3,721.673 s fast seed, the 6,144-action
531/532 slow-seed rates (~8,765 s extrapolated for 16,384 actions), and
~148.6 h extrapolated for 1M at the slow rate. The material acceptance change is
independently reviewed (`pass-with-gaps`), and the required 200 h endurance
timeout, matrix gate, timeout/provenance checks, and exact-eight check are
applied; no 48-hour or 200-hour campaign has been run.

## Confirmed CPU Bugs

All entries below have deterministic reproducers and externally observable
architectural or protocol contracts. The linked bug documents own the symptom,
root cause, impact, fix description, and post-fix verification.

| CPU bug | Violated contract | Fix commit | Result |
| --- | --- | --- | --- |
| [Uncache D-channel error](CPU_BUG_UNCACHE_DCHANNEL_ERROR.md) | Denied/corrupt NC responses must terminate the load with the correct access fault or hardware error. | `f8bb99518` | Pre-fix contract failed; repaired RTL passes denied and corrupt cases. |
| [Atomic exception RF write](CPU_BUG_ATOMIC_EXCEPTION_RF_WEN.md) | An exceptional atomic must not enable integer-register writeback. | `e1424686a` | Pre-fix `rfWen=1` on exception; repaired RTL suppresses writeback. |
| [Exceptional FP load FP write](CPU_BUG_FP_EXCEPTION_FP_WEN.md) | An exceptional FP load must not enable FP-register writeback; early faults issue no data-manager request. | `7045fa175` | Pre-fix MMIO exceptional completion enabled `fpWen`; repaired RTL passes exact suppression. |
| [CMO D-channel error](CPU_BUG_CMO_DCHANNEL_ERROR.md) | `CBOAck` denied/corrupt must propagate as store access fault or hardware error. | `42152f6ba` | Pre-fix CMO completed successfully; repaired RTL passes all operation/error cases. |
| [PTW D-channel error](CPU_BUG_PTW_DCHANNEL_ERROR_IGNORED.md) | A denied/corrupt page-table read must terminate the walk and must not refill the PTE cache. | `39a7b9629` | Pre-fix walk ignored errors; repaired RTL passes Sv39/Sv48 depth and error matrices. |
| [Vector split guest fault GPA](CPU_BUG_VECTOR_GUEST_FAULT_SPLIT.md) | A G-stage fault during a VS non-leaf walk must report the exact GPA and non-leaf marker. | `f5b553973` | Pre-fix split-vector GPA failed; repaired RTL passes the exact reproducer while preserving scalar and aligned-vector controls. |
| [Vector segment trigger address lag](CPU_BUG_VECTOR_SEGMENT_TRIGGER_ADDRESS_LAG.md) | A segment trigger must compare the current field address and report `vstart` in whole-segment units. | `d159ebdbd` | Pre-fix field-1 trigger reported `vstart=1`; repaired RTL reports field-1 address and `vstart=0`. |

## Historical Mutation and Sensitivity Evidence

The audit classification rules are in
[`HISTORICAL_BUG_AUDIT.md#scope-and-evidence-rules`](HISTORICAL_BUG_AUDIT.md#scope-and-evidence-rules).
The table below keeps the decisive external result and either a retained artifact
path or an explicit missing-artifact status.

| Historical fix or experiment | Expected contract | Observed sensitivity | Retained evidence |
| --- | --- | --- | --- |
| `222f993e`, VLS exception redirect residual drain | Redirect must not lose required vector-store drain effects. | No distinguishing legal mutation yet; this remains a mutation gap. | Reproducer context is in `HISTORICAL_BUG_AUDIT.md`; no sensitivity artifact. |
| `4b976f156`, upstream PR #6548 | A redirected exception buffer must return only the fault matching its ROB/uop identity. | Upstream-guided reproduction: stale VLEFF and scalar-store fault addresses before repair; `vector-fof`, `vector-segment-fof`, and `trigger-contracts` pass after repair. | Artifact not retained; commands and classification are in `HISTORICAL_BUG_AUDIT.md#upstream-pr-6548-vls-exception-buffer-redirect`. |
| `e541289b`, SQ address state on TLB miss | An allocated identity-stable SQ entry retains address-valid state across a miss. | Reproduced: mutant violates the identity-preserving SVA at tick 283. | Mutant artifact not retained. |
| `e12436c7`, SQ read-data order | NC stores consume committed SQ data in program order. | Reproduced: mutant returns wrong NC store data at cycle 309. | Mutant artifact not retained. |
| `45318c5d`, scalar load exception writeback | An exceptional scalar load must not request an RF write. | Reproduced: mutant asserts RF write enable on the page fault. | Mutant artifact not retained. |
| `856b821f`, VS-non-leaf metadata | A G-stage fault while walking a VS non-leaf PTE reports the exact VA/GPA and non-leaf marker. | Reproduced: mutant reports marker 0 at cycle 152 while VA/GPA remain correct. | Mutant artifact not retained. |
| `49b6a414f`, late vector DCache exception | A cached poisoned-line vector load preserves its denied/corrupt terminal exception. | Reproduced: mutant writeback reports `exception=0x0` for both cases. | Mutant artifact not retained; mutation expression is in `HISTORICAL_BUG_AUDIT.md`, and the focused reproducer is `make -C tests/memblock dcache-errors`. |
| `39ad0d589`, full access-range check | The final byte of an access crossing canonical or pointer-masking limits must be checked. | Reproduced: mutant returns scalar data instead of a page fault on the first two-byte boundary case. | `build/memblock/canonical-mutation/fullrange/`. |
| `872fa9be3`, vector-store TLB access size | A crossing vector store must use the correct access size and produce the identified terminal fault. | Reproduced: mutant remains pending after replay instead of producing the expected vector writeback. | `build/memblock/canonical-mutation/vector_store_size/`. |
| `756bbf59`, MMIO half-precision FP boxing | A successful MMIO FLH must NaN-box, not integer-sign-extend, the halfword payload. | Reproduced: mutant returns `0x0` instead of `0xffffffffffff0000` at cycle 128 for payload `0x0000`. | `build/memblock/fp-boxing-mutation/`. |
| `39824ca68394`, full segment VA capture | Misaligned high-half segment traffic must retain the complete VA when translation is re-requested. | Reproduced: directed mutant faults legal Sv39 high-half VA at cycle 754; schema-51 mixed seed 282 fails at action 99; schema-52 focused seed 286 fails at action 104, nested `vs=1,g=0` cross 17, cycle 4741 while the clean runtime passes. | `build/memblock/segment-va-mutation/`, `build/memblock/schema52-segment-va-mutation/`. |
| `9ee7b335`, misaligned vector-store progress | Misaligned vector stores must complete, dequeue, and read back exactly. | Not reproduced: the available legal sequence passes on the mutant. | Negative evidence is summarized in [HISTORICAL_BUG_AUDIT.md](HISTORICAL_BUG_AUDIT.md); artifact not retained. |
| `fbb1e349`, cross-page vector-store progress | A translated cross-page vector store must complete, dequeue, and read back exactly. | Not reproduced: expanded legal progress sequence passes on the rebuilt mutant. | Negative evidence is summarized in [HISTORICAL_BUG_AUDIT.md](HISTORICAL_BUG_AUDIT.md); artifact not retained. |

The vector guest-fault patch is separately validated in
`CPU_BUG_VECTOR_GUEST_FAULT_SPLIT.md`; its clean scalar and aligned-vector
controls remain required and must not be replaced by the failing split-vector
case as the expected value.

## Independent Review Records

Records below retain reviewed inputs, findings/dispositions, verdicts, decisive
artifacts, and remaining gaps. Unless explicitly stated otherwise, each review
used a fresh-context non-author and the actual diff plus normative contracts in
`ORACLES.md` and `VERIFICATION_PLAN.md`. Paths are relative to `build/memblock/`.
Superseded implementation narratives and intermediate attempts are available via
`git log --follow -- tests/memblock/docs/RESULTS.md`.

### C++ Component Architecture Refactor

- Base: `7f83653dc`; implementation: `c81aa3d65`. The architecture overview is in
  [README.md#harness-layout](../README.md#harness-layout).
- Reviews: `architecture-stage1-20261023` (facade, diagnostics, packaging),
  `architecture-stage2-domains-20261023` (state ownership),
  `architecture-stage2-adapter-20261023` (semantic DUT boundary),
  `architecture-stage2-mixed-20261023` (mixed extraction), and
  `architecture-stage2-inventory-20261023` (tests/inventory/docs).
  Authors were the main agent, adapter author, and mixed author; reviewers were
  non-authors for their respective scope. Final verdicts: stage 1 and inventory
  `pass`; domains, adapter and mixed `pass-with-gaps` pending runtime breadth.
  Reports: `architecture-stage1/review/independent-review.txt` and
  `architecture-stage2/review/{domain,adapter,mixed,test-doc}-review.txt`.
- All findings accepted, fixed and rereviewed: failure truncation propagation
  and stable first-message reference; fresh timestamps for changed copies and
  deletion of obsolete owned headers; actual Make include-edge checking and
  whole-generator Probe-assignment uniqueness. Final indentation cleanup and
  whitespace-tolerant source assertions passed non-author follow-up review.
- Validation: 303 Python/host-C++ tests, `make -C tests/memblock check-rtl`, and
  the four-job real Picker harness build pass. Logs:
  `architecture-stage2/{unit-final,check-rtl,build-formatted}.log`.
  No-op compilation builds zero objects; changing/restoring one private domain
  implementation or mixed action builds one object each (`incremental.json`,
  `mixed-incremental.json` in that directory).
- Baseline `architecture-stage1/baseline-runtime/runtime.json` versus final
  `architecture-stage2/runtime/runtime.json`: 20 directed/focused random pairs
  pass with identical terminal summaries and exits (`architecture-stage2/replay/`).
  The final rebuilt binary is byte-identical to that runtime
  (`architecture-stage2/final-runtime-identity.json`). Structured failure capture
  passes (`architecture-stage2/diagnostic/result.json`); its deliberate manager
  misuse after reset is a harness fixture, not a CPU bug.
- Coverage/SPEC/corner, seeds 1 and 7, 256 actions each: all six mixed terminal
  summaries, checkpoints and exits match, but all fail the unchanged coverage
  gates (`architecture-stage2/mixed-replay/`). Earlier 6,144-action comparisons
  all timed out symmetrically at 180 seconds; none counts as a pass.
  No full mixed coverage, historical sensitivity, or closure claim follows.

### Agent Workflow

- Review `agent-workflow-20261021-01`, base `3e5dc4029674`, covered the actual
  workflow/docs/Make/controller diff. Final verdict `pass` after seven accepted
  findings were fixed: overridable boundary result paths, frozen worker
  runtimes, unique trace paths, no nested delegation, one authoritative closure
  threshold, read-only audit scope, and complete review records.
- Validation: 269 units, `check-rtl`, and `git diff --check` pass;
  `agent-workflow-review-20261021-01/{unit,check-rtl,diff-check}.log`.
  Workspace-root `AGENTS.md` remains outside the XiangShan Git repository.

### Schema 52 Nested High-Half And Demand Attribution

- Review `schema52-nested-high-va-20261021`, final verdict `pass`. Three accepted
  findings fixed: demand classification requires Acquire/AcquirePerm plus
  demand source, docs name all 24 stage-1/nested bins, and synthetic requests
  exercise the actual classifier. Reviewed source and contracts are retained
  with `schema52-runtime-v3/` and `schema52-focused-v3/`.
- Validation: 273 units and RTL/docs checks pass. `vector-segment-canonical`
  and mixed seed 286 pass; the same input rejects the isolated full-segment-VA
  mutant at nested cross 17 (`schema52-segment-va-mutation/`). The bin set does
  not guarantee cross-page placement for every seed; no closure claim.

### DCache Release-Window Quiescence

- Review `dcache-release-quiescence-20261021`, final verdict `pass`. Accepted
  fixes include probe holds in quiescence, require two sampled quiet cycles,
  and allow incidental stability observations at a disabled stall endpoint
  while retaining enabled-stall requirements. Scope: manager, release-idle
  driver, pressure generator, and associated oracle tests.
- Exact coverage seed 301/6,144 passes at cycle 3,453,254 on
  `schema52-runtime-v3`; offline accounting passes. Evidence:
  `schema52-regression-v3/coverage-replay{,-verified}.log`.

### Atomic Cold-Refill Reset

- Review `atomic-reset-recovery-20261021`, base `32eb013e0`, snapshot
  `atomic-reset-review-v3.patch`; verdict `pass-with-gaps`. Accepted finding:
  remove three unmonitored post-reset CSR cycles from six atomic cases.
- Six LR/AMOADD/AMOCAS x W/D cases observe one cold request and zero D beats
  before reset, then check distinct survivors, independent data, request/queue
  conservation, and a quiet tail. Focused replay passes at cycle 10,865; four
  controls, 273 units and RTL/docs checks pass. Runtime and commands:
  `atomic-reset-runtime-v3/runtime.json`, `atomic-reset-evidence.json`.
  A monitored-wait negative control rejects stale writeback
  (`atomic-reset-fault-injection/`); this is harness sensitivity.
- No partial modifying-atomic/error-refill/CMO reset, reservation persistence,
  dirty-cache persistence or full-line DUT-readback claim. Reset assertion and
  synchronized release remain unmonitored.

### Partial LR Refill Reset and Mixed Progress Diagnostics

- Review `partial-lr-reset-20261021`, base `32eb013e0`, snapshot
  `partial-lr-reset-review.patch`; verdict `pass-with-gaps`, no required fixes.
- Four LR.W/LR.D x half-line cases reset after one D beat. Exact pending
  identities may be canceled only immediately after reset; legal completed
  identities remain validated. Focused replay passes at cycle 17,981, with
  partial-LR completed/canceled `0,4` and nonkeyword/keyword `4,0`.
  Five controls, 274 units and RTL/docs checks pass. Runtime and commands:
  `partial-lr-reset-runtime/runtime.json`, `partial-lr-reset-evidence.json`.
- Early-completed and keyword partial-LR paths lack integrated evidence;
  ordinary-load early writeback is checked separately by `single-load`.
  Other reset exclusions remain as above; no closure claim.
- Optional progress checkpoints preserve RNG/cycle/oracle behavior. Seed 284's
  focused terminal matches with diagnostics off/on and the prior runtime
  (`mixed-progress-equivalence/`). SPEC 411 and corner 412, 6,144 actions each,
  initially time out at 1,800 seconds but pass with the 7,200-second bound:
  3,254.850 and 3,569.371 seconds (`schema52-progress-replay/`). These are finite
  results, not closure or a deadlock claim.

### Partial Scalar Load Reset

- Reviews `partial-load-reset-20261021` and `partial-load-merge-reset-20261021`,
  base `32eb013e0`, snapshots `partial-load-reset-review-v3.patch` and
  `partial-load-merge-reset-review.patch`; both `pass-with-gaps`. Accepted
  findings clarified manager cancellation versus scalar/LQ disposition and
  exposed missing pending cancellation; paired cases now exercise it.
- Twelve singleton/opposite-half-pair cases cover three issue lanes and two
  target halves. Focused replay passes at cycle 18,397; singleton scalar/LQ
  outcomes are `6,0`, pairs `6,6`, request orders `3,3` for each. Seven controls,
  274 units and RTL/docs checks pass. Runtime and commands:
  `partial-load-merge-reset-runtime/runtime.json`,
  `partial-load-merge-reset-evidence.json`. A delayed-cancellation negative
  control rejects the boundary violation (`partial-load-merge-reset-negative/`).
- The 90-leaf inventory passes, including SPEC 1/16,384
  (`partial-load-merge-reset-benchmark.json`). The following matrix is 11/12;
  exact SPEC 422 replay repeats action 3,436/cycle 3,136,937, classified below
  as a UT construction precondition failure (`reset-current-preflight/`).
  Singleton-only cancellation, other legal retirement permutations, aggregate
  LQ observability and the reset exclusions above remain gaps.

### Clean Replacement Overlap UT Correction

- Reviews `clean-release-overlap-fix-20261021` and
  `pressure-scheduling-fix-20261021`, actual source/contract snapshots in
  `clean-release-overlap-fix/` and `pressure-scheduling-fix/`; both
  `pass-with-gaps`. Stale geometry/closure text and missing overlap qualifiers
  were accepted and corrected; integration had no required fixes.
- Exact SPEC 422 failure came from assuming unproven baseline residency.
  Clean overlap now establishes holds before issuing older targets; dirty
  overlap preallocates SQ entries before younger held loads. Data, identity,
  Release and queue checks remain active. Geometry is four ways/256 sets;
  the 8-KiB stride spans two sets, so single-set associativity remains a gap.
- Combined runtime `pressure-scheduling-fix-runtime/runtime.json` passes 274
  units, RTL/docs checks and eight controls. Clean/dirty compact 512-action
  runs pass all 384 enabled bins and offline accounting; SPEC 422/6,144 passes
  at cycle 7,497,708 (`pressure-scheduling-fix/`). These are finite results;
  dirty baseline ownership required the separate qualification below.

### Hypervisor Fault Oracle Correction

- Review `hypervisor-fault-oracles-20261022`, final `pass-with-gaps`. Accepted
  findings remove an unsupported maximum-one target-read rule and correct a
  command rejected before simulation. Snapshot/dispositions:
  `hypervisor-oracle-correction/{v1-dispositions.json,v2-review-delta.patch,review.json}`.
- Exact fault/identity, RF suppression, legal manager traffic and queue recovery
  decide H fault correctness; wakeup/cancel cardinalities are diagnostic.
  HLVX PMP R-allow/X-deny permits independently qualified target demand Acquires
  without requiring their presence/count. Integrated validation passes 279
  units, RTL/docs checks, six controls, all 82 directed H cases, and focused
  mixed 551/552 at 256 actions with all 42 relation bins. Candidate/integrated
  terminals match; `hypervisor-oracle-runtime/runtime.json` and
  `hypervisor-oracle-correction/integrated-mixed-validation.json` retain evidence.
- Bounded audit of `04c0d157`, `22cc0b6b`, `3f00c595` is in
  `hypervisor-bounded-audit/`. The private same-lane H/vector prototype passes
  64 offset/mode cases; review `hypervisor-vector-prototype-20261022` is
  `pass-with-gaps` (`hypervisor-vector-prototype/review.json`). Internal overlap
  and historical sensitivity remain unproven; no production coverage upgrade
  follows from the offset sweep. Permission reverts have no completed evidence.

### DebugModule Access Guard Matrix

- Review `debug-guard-expansion-20261021`, actual final v5 diff/spec/evidence in
  `debug-guard-expansion/`; verdict `pass-with-gaps`. Accepted limitations:
  the original mutant stops before added bins and inherited store-output
  filtering remains. Protected map is configuration-specific, not a universal
  RISC-V map; normative provenance is in that directory's `audit/`.
- All 24 LD/SD x mode/exit x edge cases pass, including twelve readbacks.
  Faulted stores stay uncommitted and are explicitly redirected. Clean/mutant
  repeated controls establish guard sensitivity at the existing interior case.
  Review `debug-guard-eight-selected-bin-independent-review-2026-10-22` passes:
  all eight selected edge/exit bins pass twice clean and fail twice mutant
  (`debug-guard-bin-sensitivity/summary.json`). These reset-isolated fixtures
  do not establish full-stream equivalence; failures are missing-writeback
  timeouts, not observed forbidden accepted traffic or a new CPU bug.
- Integrated 274 units, RTL/docs checks, seven controls and the 90-leaf inventory
  pass (`debug-guard-runtime/runtime.json`,
  `debug-guard-expansion/inventory90.json`). Misaligned crossings, fetch,
  restrictive-PMP override and store-output observability remain gaps.

### Dirty Ownership Qualification and Rejected Construction

- Review `dirty-ownership-qualification-20261022`, final `pass-with-gaps` after
  three accepted v1 findings: require a covering A for each fresh rejected
  target, allow repeated independently checked ReleaseData, and synchronize
  schema declarations. Snapshot, delta and dispositions:
  `dirty-ownership-prototype/{review-input.json,v2-review-delta.patch,v1-dispositions.json,review.json}`.
- Qualification drains baseline SBuffer/D/E and checks covering store NtoT
  acquisitions, unchanged error counters, and zero completed target Releases
  after establishing holds. Retained responsibility need not mean array
  residency. Rejected construction legally completes every preallocated store,
  preserves all functional oracles, fully drains and retries fresh addresses;
  four rejected attempts fail explicitly. Rejected traffic remains accounted
  separately from accepted logical actions/bins.
- Compact 424/512 and DCache-SPEC 425/512 each qualify 411 actions across all
  384 enabled bins. Forced rejection checks 411 cleaned retries; the eight-window
  control checks 27. Exhaustion and hidden-target-A harness negatives reject as
  required. Integrated 278 units, RTL/docs checks, five controls and compact
  replay pass; terminal matches reviewed v2 (`dirty-ownership-prototype/`).
- These injected controls are harness evidence, not natural ownership-loss or
  mutation sensitivity. The subsequent schema-53 inventory is 89/90 and matrix
  10/12; construction exhaustion remains unaccepted, as described below.

### Schema 53 Prefetch Interference in Dirty Ownership

- Record `schema53-prefetch-quiesce-20261022` was **self-reviewed only**; the
  earlier independent review and campaign replays were stopped by operator
  request. Current-head focused replays now pass seed 1/16,384 and seeds
  531/532 at 6,144 actions; see the current-head note above. An independent
  review at `c81aa3d65` completed with verdict `pass-with-gaps`; findings are
  recorded below.
- SPEC 531 diagnostic shows a legal L1 stride prefetch evicting a baseline
  target during hold setup, with zero bad acquisitions/error-state changes.
  This is a UT ownership-precondition defect, not an RTL data/ordering bug.
  Evidence: `dirty-fast-diag2/fastdiag2-531.log`.
- The implementation quiesces that prefetcher through its external CSR for
  ownership-sensitive actions, runs 4 + 8 + 4 = 16 DUT cycles, drains DCache idle and
  restores the prior enable afterward. Ordinary mixed traffic retains prefetch.
  A full-line Release index falls back to the original range scan for non-full
  or unaligned records. Five directed terminals and the index-only failing
  seed-1 replay match the prior runtime (`dirty-index-probe/inventory-index.json`).
- 283 units and RTL/docs checks passed at the time; current unit evidence is 305.
  Runtime `prefetch-index-runtime/runtime.json` retains the earlier attempt;
  the current-head focused replay uses `architecture-stage2/runtime` and is
  recorded above. Training state, other background agents and mixed interaction
  after this restricted quiesce remain unproven. Architecture refactor checks do
  not supply independent functional review or closure evidence.

### Schema 53 Current-Head Independent Review

- Review `independent-review-schema53-20261023`, base `c81aa3d65`, current
  `architecture-stage2` runtime, fresh-context non-author, verdict
  **`pass-with-gaps`**. Report:
  `review-schema53-20261023/independent-review-schema53.txt`.
- Confirmed evidence: the three focused terminals contain 1,536 nonzero
  `actual_set_pressure_cross` cells; all 384 dirty-overlap cells are exactly 1;
  PASS is gated by `constraint_coverage.complete`. The
  `ownership=384,384,384,...` tuple alone is a counter, not the bin proof.
- Accepted findings (must be fixed before closure):
  1. **High** — the ownership proof can pass with a baseline line evicted during
     hold setup when its ReleaseData is still in flight; `target_releases`
     counts only completed C, and the accepted-path drain does not call
     `run_until_dcache_release_idle`.
  2. **High** — the rejected-attempt cleanup/accounting path is not exercised at
     `c81aa3d65`; all three current focused seeds have zero rejects, and the
     only forced-rejection evidence predates the refactor.
  3. **Medium** — the quiesce changes the measured stimulus; actual cost is
     4 + 8 + 4 = 16 DUT cycles per set-pressure action, not “advances eight
     cycles”; training state and later-seed interaction are not restored/proven.
  4. **Medium** — dirty target Releases are not constrained to ReleaseData; a
     clean Release of a dirty target could pass if other lines satisfy the
     minimum. The rejected validator also accepts zero target ReleaseData.
  5. **Low** — the release-index fallback is point-cover for non-full records;
     unaligned records, duplicate ReleaseData, and fallback attribution are
     untested. O(1) applies to the covering count, not stalled/cycle scans.
- Disposition: all findings accepted. The 384/384 focused replay is valid bin
  evidence for the quiesced stimulus, but it is not full-campaign acceptance and
  does not close the two High gaps. No CPU RTL bug is claimed by this review.
- Remediation (2026-10-23, round 2): High 1 is mitigated by a release-channel
  drain before the ownership snapshot and again after selected C backpressure;
  High 2 now has a harness-only forced-rejection path with current-revision
  runtime evidence (`385,384,384,1,1,...`, one reject at action 4,018); Medium 4
  now rejects clean Releases of dirty target lines. Normal seed 531/6,144 passes
  on the rebuilt private runtime. The forced terminal fails only its coverage
  gate because the injected retry duplicates one bin; that is harness
  perturbation, not a functional failure. Post-fix 90-leaf (90/90) and matrix
  (12/12) pass; release-index edge tests for unaligned record and duplicate
  ReleaseData are added (305 units); fallback attribution remains point-cover,
  and mutation sensitivity remains pending.

### Acceptance Recalibration Review

- Review `acceptance-recalibration-review-20261023`, fresh-context non-author,
  scope limited to acceptance arithmetic/consistency, verdict
  **`pass-with-gaps`**. Report:
  `review-schema53-20261023/acceptance-recalibration-review.txt`.
- Confirmed measured basis: 90-leaf 3,771.461 s (jobs=8); matrix 3,467.092 s
  (jobs=4) and 3,255.288 s (final 8-job gate); `random-mixed` seed 1/16,384
  3,741.241 s; seeds 531/532 at 6,144 actions 3,256.142/3,260.364 s (focused)
  and 3,239.406/3,253.553 s (8-job gate); worst 16,384 extrapolation 8,694.3 s;
  worst 1M extrapolation 147.41 h.
- Accepted findings and dispositions:
  1. **High** — 168 h endurance left only ~14 h (9.7% with the load proxy);
     `ENDURANCE_TIMEOUT_SECONDS` changed to **720000 (200 h)**.
  2. **Medium** — the docs still cited pre-fix timings; all current-head text now
     uses the post-fix JSONs and notes that the matrix used jobs=4.
  3. **Medium** — the verifier did not enforce the recorded timeout;
     `--min-timeout-seconds` is now required for final (12000) and endurance
     (720000), and the recorded runtime artifact/root/external dependencies must
     match the supplied frozen runtime metadata.
  4. **Medium** — the six-scenario matrix was not an executable final gate;
     `matrix-regression` and `verify-matrix` are now invoked by
     `final-acceptance`.
  5. **Low** — `verify-final-results` used a misleading 4096 transaction value;
     changed to 16384.
  6. **Low** — endurance now requires exactly eight results in addition to
     `--min-results 8`.
- Residual risk: no 48 h/200 h campaign has run; the 1M time is a linear
  extrapolation from 6,144-action seeds; 48 h/128 has limited margin under a
  >20% host slowdown (56 h is the documented optional robustness value).
