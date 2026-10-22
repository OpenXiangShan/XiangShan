# MemBlock Verification Results

This file is the compact evidence index for the MemBlock UT. Current contracts,
acceptance rules, and verification-point status remain authoritative in
[`VERIFICATION_PLAN.md`](VERIFICATION_PLAN.md), [`ORACLES.md`](ORACLES.md), and
[`IO_COVERAGE.md`](IO_COVERAGE.md). CPU bug details remain authoritative in the
individual `CPU_BUG_*.md` documents.

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

Evidence snapshot: 2026-10-22.

- UT base revision: `32eb013e0` plus reviewed reset, pressure scheduling,
  DebugModule guard, schema-53 dirty ownership, H fault oracle, and mixed-progress
  changes below.
- Kunminghu-v2 source baseline: `fe2288cb09361b65afa26be3f403520c4b496dbf`,
  merged into the UT branch as `4bd72b3a0`.
- Picker: `7499f797b56be57cf950cd40f3d14e9eba133449` (v2.1.0).
- xcomm: `a5f5dae14f3cdc8f7b35dab7b5dc8c21576b4dbd` (v1.0.0,
  native ABI 3).
- Toolchain lock: `tests/memblock/toolchain.lock.json`.
- Bootstrap provenance: `build/memblock/tools/picker.json`.
- General frozen runtime manifest: `build/memblock/runtime/runtime.json`.
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
- Latest schema-53 prefetch-quiesce and release-index runtime:
  `build/memblock/prefetch-index-runtime/runtime.json`; the focused replay,
  inventory, matrix, and review evidence are recorded below.
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

The current generator schema is 53. `random-mixed` remains the canonical mixed
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
| 53 | Dirty held-refill overlap requires externally proven retained baseline coherence responsibility; up to four fresh construction attempts preserve the selected profile tuple, normal rejected cleanup retains every functional oracle, and separate accounting conserves attempts and traffic. | Reviewed private compact 424/512 and DCache-SPEC 425/512 each qualify 411 actions; forced rejection crosses all 384 enabled bins with 411 cleaned rejections and 411 accepted retries. V2 compact 424/512 and forced eight-window 424/128 pass. Evidence: `build/memblock/dirty-ownership-prototype/`. | Independent review v2 passes with gaps; focused controls and 278 units/RTL/docs pass. Integrated compact 424/512 passes with terminal byte-identical to reviewed private v2. The completed 90-leaf inventory passes 89/90: SPEC `random-mixed` seed 1 stops after 4,916 of 16,384 actions with 12 dirty-ownership construction rejections, every one an early target Release with zero acquisition or error reasons. The two-seed six-scenario matrix passes 10/12: SPEC `random-mixed` seeds 531 and 532 stop after 4,151 and 4,215 of 6,144 actions. Rejected-manager tuples show six- and eight-window depth-9/10 baseline actions, while forced one-window dirty probes complete 1,365 actions. A diagnostic replay places the first early target Release after the baseline snapshot (`after_baseline=0`) during held-refill setup, with the target line's only A request being its baseline store NtoT; the global A summary shows a background L1 stride prefetch to a different line in the target set evicting it during hold setup. The UT now quiesces the L1 stride prefetcher through its external CSR for the set-pressure action, advances eight cycles and drains DCache idle, restores the prior enable after the action, and counts releases through an O(1) full-line index with a non-full fallback. A focused prefetch replay and full campaign reruns were stopped by operator request before completion; no acceptance claim follows. This is a UT stimulus-precondition failure under mixed traffic, not a functional RTL bug. Forced rejection is harness injection; no natural ownership-loss, historical sensitivity, or closure claim. |

Supporting oracle and diagnostic improvements retained in this evidence set:

- MMIO FP payloads distinguish NaN boxing from integer sign extension, and the
  queue oracle requires observed recovery for denied/corrupt FP loads.
- The accepted-A DCache monitor checks opcode, grow, size, source, alignment,
  mask, corrupt, and defined user-address relationships without filtering legal
  prefetch or demand-merged traffic.
- The latest complete Python unit evidence is 292 tests. The Picker 2.1.0 /
  xcomm 1.0.0 migration also rebuilt the full C++ model and passed `check-rtl`
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

No current closure campaign has completed. In particular, the current records
do not establish the eight-hour/128-seed breadth campaign or the 8,000,000-action
endurance campaign required by `Regression Acceptance`.

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

### C++ Verification Environment Decision

- Review ID: `verification-architecture-20261023-01`.
- The team evaluated a Python/Toffee orchestration layer and retained the
  existing C++ harness as the implementation path. The two Python vertical-slice
  tests were removed; `smoke` and `l2-flush-contracts` remain C++ scenarios.
- The architecture review reinforced the existing boundaries: typed drivers,
  passive observations, independent models and scoreboards, immutable runtime
  provenance, isolated build ownership, and run-specific result paths.
- No Toffee dependency, migration manifest, or `x/90` migration claim remains.
  All 90 registered leaves continue to use the C++ control path.

### Agent Workflow

Review ID `agent-workflow-20261021-01` covers the material workflow, review-gate,
and parallel-resource changes made against UT revision `3e5dc4029674`. The
review rules are in
[`VERIFICATION_PLAN.md#independent-review-and-no-progress-audits`](VERIFICATION_PLAN.md#independent-review-and-no-progress-audits).

- Trigger: changed agent delegation, independent-review gate, shared build
  ownership, result-path overrides, and trace isolation.
- Reviewed state: the dirty-file diff in `AGENTS.md`, `README.md`,
  `VERIFICATION_PLAN.md`, `HISTORICAL_BUG_AUDIT.md`, `RESULTS.md`, `Makefile`,
  `scripts/run_regression.py`, and their unit tests.
- Independence: primary author plus a fresh-context, non-author reviewer with
  no author conversation history.
- Verdict: initial `fail`; final focused re-review `pass` after all findings
  were fixed.
- Residual gap: workspace-root `AGENTS.md` is outside the `XiangShan` Git
  repository and is not versioned with the tracked UT documents.

| ID | Review finding | Stable evidence | Review finding disposition |
| --- | --- | --- | --- |
| R1 | Boundary regression result paths were hard-coded. | [`README.md#parallel-work-and-shared-state`](../README.md#parallel-work-and-shared-state), [`Makefile`](../Makefile), `unit/test_memblock_env_contract.py`. | Accepted and fixed with `HUNT_RESULT` and `BOUNDARY_HUNT_RESULT` overrides plus contract tests. |
| R2 | Worker Make targets could re-enter `freeze-runtime`. | [`AGENTS.md#subagent-workflow`](../../../../AGENTS.md#subagent-workflow), [`README.md#parallel-work-and-shared-state`](../README.md#parallel-work-and-shared-state). | Accepted and fixed: workers use a published binary or `run_regression.py --runtime-metadata`. |
| R3 | Parallel jobs could share one debug trace. | `scripts/run_regression.py`, `unit/test_run_regression.py`. | Accepted and fixed: `--jobs > 1` with one shared direct trace path is rejected before output creation. |
| R4 | Nested subagent fan-out was ambiguous. | [`AGENTS.md#subagent-workflow`](../../../../AGENTS.md#subagent-workflow). | Accepted and fixed: workers cannot create further subagents. |
| R5 | Agent guidance and acceptance plan stated different closure durations. | [`VERIFICATION_PLAN.md#regression-acceptance`](VERIFICATION_PLAN.md#regression-acceptance). | Accepted and fixed: the verification plan is the sole closure threshold. |
| R6 | The no-progress audit was called read-only while requesting writes. | [`VERIFICATION_PLAN.md#independent-review-and-no-progress-audits`](VERIFICATION_PLAN.md#independent-review-and-no-progress-audits). | Accepted and fixed: bounded audit is read-only; implementation is a separate task. |
| R7 | The initial review record omitted required fields. | This section and the verification-plan review protocol. | Accepted and fixed with review ID, evidence, dispositions, verdict, and residual gap. |

Verification evidence for this workflow change:

```text
make -C XiangShan/tests/memblock check-rtl
  build/memblock/agent-workflow-review-20261021-01/check-rtl.log
make -C XiangShan/tests/memblock unit                 (269 tests)
  build/memblock/agent-workflow-review-20261021-01/unit.log
git -C XiangShan diff --check
  build/memblock/agent-workflow-review-20261021-01/diff-check.log
```

No RTL bug candidate was produced by this workflow review, so no CPU bug
document or DingTalk notification was created.

### Schema 52 Nested High-Half And Demand Attribution

- Review ID: `schema52-nested-high-va-20261021`.
- Reviewed state: the schema-52 24-bin high-half segment generator,
  coverage/verifier accounting, independent stage-1/two-stage walks, and the
  causal DCache suppression check used by denied hypervisor loads.
- Independence: primary author plus a fresh-context, non-author reviewer with
  the normative contracts and actual diff, but no author conclusions.
- Initial verdict: `fail`; final focused re-review verdict: `pass`.

| ID | Review finding | Stable evidence | Review finding disposition |
| --- | --- | --- | --- |
| S1 | Demand attribution counted any history record with `user.reqSource=2`, although that field is unspecified for CMO/Get traffic. | `environment/dcache_a_contract.inc`, `environment/dcache_agent.inc`, `unit/test_dcache_a_contract.py`. | Accepted and fixed: a shared predicate requires Acquire/AcquirePerm opcode 6/7 plus demand source 2; CMO/Get, stores, and prefetch are excluded. |
| S2 | README and the reporter table still described schema 51 and eight high-VA bins. | `README.md`, `docs/CONSTRAINED_RANDOM.md`. | Accepted and fixed: current schema is 52 and the reporter contract names all 24 stage-1/nested bins. |
| S3 | The suppression test exercised only precomputed counts, not request classification. | `unit/test_dcache_a_contract.py`. | Accepted and fixed: synthetic demand, store, prefetch, Get, and CMO requests now test the shared predicate directly. |

Residual risk at review time: no three-profile schema-52 matrix or closure
campaign had completed, and the 24 bins constrain mode/direction/alignment but
do not guarantee a cross-page placement for every seed.

Verification for the reviewed change: `make -C tests/memblock unit` passed 273
tests in `build/memblock/schema52-runtime-v3/unit.log`; `check-rtl` passed in
`build/memblock/schema52-runtime-v3/check-rtl.log`; the schema-52 runtime passed
`vector-segment-canonical` and focused mixed seed 286 in
`build/memblock/schema52-focused-v3/`. The same focused input deterministically
fails on the retained full-segment-VA mutant at nested `vs=1,g=0` cross 17
(exit 123), recorded in `build/memblock/schema52-segment-va-mutation/`.

### DCache Release-Window Quiescence

- Review ID: `dcache-release-quiescence-20261021`.
- Trigger: schema-52 coverage seed 301 reached all 24 high-VA bins, then stopped
  at action 5,067 on `selected=0:stalled=0:cycles=0:stable=1`. The original
  `run_until_dcache_idle` did not drain a pre-window C transaction, so the
  readiness window could attribute prior manager activity to its own target.
  Exact replay after quiescence also showed that a disabled endpoint can legally
  observe an incidental one-cycle stability check without a forced stall.
- Independence: primary author plus a fresh-context non-author reviewer.
- Initial verdict: `fail`; final focused re-review verdict: `pass`.

| ID | Review finding | Stable evidence | Review finding disposition |
| --- | --- | --- | --- |
| Q1 | Probe-driven C backpressure was absent from the quiescence predicate. | `environment/dcache_agent.inc` C ready generation and release-idle state. | Accepted and fixed: an active probe hold prevents idle accumulation. |
| Q2 | Checking sampled monitor state once could return before an active ready C was observed. | `environment/environment.inc` `run_until_dcache_release_idle`. | Accepted and fixed: every tick samples C valid and requires two consecutive complete quiet samples with no stalled C, release data, readiness window, or probe hold. |
| Q3 | The disabled endpoint incorrectly required zero payload-stability checks rather than zero forced stall coverage. | `environment/dcache_a_contract.inc` release-backpressure predicate, `scenarios/random_mixed.inc`, `unit/test_dcache_a_contract.py`. | Accepted and fixed: disabled endpoints require zero stalled releases/cycles but allow incidental stability; enabled endpoints retain the exact one-release/16-cycle/16-stability contract. Manager accounting records forced cycles only. |

The exact schema-52 coverage seed 301 replay now passes all 6,144 actions on
`schema52-runtime-v3` at cycle 3,453,254. The direct terminal and independent
coverage verifier pass in
`build/memblock/schema52-regression-v3/coverage-replay{,-verified}.log`; the
forced backpressure manager tuple is `781,781,12496,12496,1566`.

### Atomic Cold-Refill Reset

- Review ID: `atomic-reset-recovery-20261021`.
- Trigger and snapshot: material reset stimulus/oracle/coverage extension against
  `32eb013e0`; the exact reviewed six-file diff is
  `build/memblock/atomic-reset-review-v3.patch`.
- Independence: primary author plus fresh-context non-author
  `atomic_reset_review`; the preceding read-only design audit was separate.
- Initial verdict: `pass-with-gaps` with one medium monitor finding; final
  focused re-review: `pass-with-gaps`, no new findings.
- Disposition R1: accepted and fixed. `activate_bare` used three unmonitored
  post-reset CSR setup cycles. The six atomic cases now use the legal default
  M-mode Bare controls and monitor every functional tick after `reset()` returns.
  Reset assertion and synchronized release remain explicitly unmonitored.

`reset-recovery` now covers six LR/AMOADD/AMOCAS x W/D cases before the first D
beat. Each observes one accepted outstanding cold request, zero accepted
GrantData beats, and zero premature writebacks before common DUT/manager reset.
A distinct same-family survivor checks exact old data and W sign extension;
full-doubleword reads check unchanged canceled-target data and the independent
survivor update, including neighboring-word preservation. A tail longer than the
original response delay rejects late/duplicate completion. Exact requests,
AcquireBlock/refill-request counts, writebacks, queues, response drain, and
GrantAck drain are checked; both backing lines are checked byte for byte.

The focused terminal passes at cycle 10,865: nine outstanding resets and canceled
operations, nine survivor operation writebacks, and twelve atomic readbacks
(21 total scalar completions across the nine reset cases). The initial
`atomic-reset-focused.log` failure was a UT counter error: `dcache_refills` counts
accepted AcquireBlock requests, including reset-canceled requests, rather than
completed D refills. The corrected guard separately requires zero accepted
GrantData beats before reset; no RTL defect was established.

Validation and retained artifacts:

```text
build/memblock/atomic-reset-runtime-v3/runtime.json
build/memblock/atomic-reset-focused-v3.log
build/memblock/atomic-reset-controls-v3.json       (4/4 controls pass)
build/memblock/atomic-reset-unit-v3.log           (273 tests pass)
build/memblock/atomic-reset-check-rtl-v3.log       (check-rtl/check-docs pass)
build/memblock/atomic-reset-evidence.json
build/memblock/atomic-reset-provenance.json
```

Commands: `make -C tests/memblock picker-harness JOBS=8`, explicit
`freeze_runtime.py` to the unique v3 runtime, the frozen binary's
`--test reset-recovery`, `benchmark_tests.py` with
`atomic-contracts,atomic-dchannel-errors,reset-tree-contracts,frontend-reset-recovery`,
and `make -C tests/memblock unit check-rtl`. Exact frozen command and artifact
paths are retained in the evidence JSON. The actual unit and RTL checks ran
separately, after the source writer stopped.

An isolated harness negative control replaces only the repeated atomic reset
with a monitored wait past the original response deadline. It fails as required
at cycle 1,314 on `unexpected load writeback lane=0 rob=0 pdest=146`, before any
survivor is issued. Evidence and compile command are in
`build/memblock/atomic-reset-fault-injection/{negative-control.log,injection.json}`.
This validates stale-identity rejection; it is a harness negative control, not
an RTL mutation or CPU bug.

Residual gaps at this six-case snapshot: no partial-D-beat or CMO
reset coverage, no reservation-reset/persistence claim, and no full cache-line
readback. Initialized backing memory persists, and manager responses are canceled
with their reset domain rather than reinjected. No closure-scale campaign or new
CPU bug claim follows from these finite tests.

### Partial LR Refill Reset and Mixed Progress Diagnostics

- Review ID: `partial-lr-reset-20261021`.
- Trigger and snapshot: material pending-load cancellation and partial-refill
  reset extension against `32eb013e0`; actual dirty-file snapshot:
  `build/memblock/partial-lr-reset-review.patch`.
- Independence: primary author plus fresh-context non-author
  `partial_lr_reset_review`, following a separate read-only design audit.
- Scope: exact pending scalar cancellation, four partial LR cases, associated
  unit tests and reset contracts, and optional mixed-progress noninterference.
- Verdict: `pass-with-gaps`; no findings requiring changes. The primary retained
  the review's early-completion/keyword limitations as evidence gaps below.

Four LR.W/LR.D x first/second-half target cases now reset after exactly one
accepted GrantData beat, with one outstanding request and a delayed final beat.
The scoreboard validates any legal early result normally; only a still-pending
exact identity can be canceled at the just-completed reset boundary. Cancellation
preserves unrelated obligations and earlier errors and is counted separately
from completion. Distinct survivor identities, two doubleword readbacks per case,
full backing-line byte checks, exact D/E/request/queue conservation, and a quiet
tail longer than the original interbeat delay check recovery and stale work.

The focused replay passes at cycle 17,981: 13 outstanding resets, 13 canceled
operations, 13 survivor operation writebacks, 12 atomic and eight partial-LR
readbacks. Its partial-LR disposition is `0,4` (completed,canceled), and request
order is `4,0` (nonkeyword,keyword). Thus all four cases exercise pending
cancellation and nonkeyword requests; varying target half alone does not prove
keyword coverage. Scalar units cover legal early completion and rejection of
subsequent cancellation, but early-completed LR and keyword partial-reset paths
lack integrated replay evidence. The existing `single-load` control separately
passes its ordinary-load early-writeback/GrantAck-before-final-D check.

Validation and retained artifacts:

```text
build/memblock/partial-lr-reset-runtime/runtime.json
build/memblock/partial-lr-reset-focused.log
build/memblock/partial-lr-reset-controls.json      (5/5 controls pass)
build/memblock/partial-lr-reset-unit.log          (274 tests pass)
build/memblock/partial-lr-reset-check-rtl.log     (check-rtl/check-docs pass)
build/memblock/partial-lr-reset-evidence.json
build/memblock/partial-lr-reset-provenance.json
```

Commands: `make -C tests/memblock picker-harness JOBS=8`, explicit
`freeze_runtime.py` to the unique partial-LR runtime, the frozen binary's
`--test reset-recovery`, `benchmark_tests.py` with
`single-load,atomic-contracts,atomic-dchannel-errors,reset-tree-contracts,frontend-reset-recovery`,
and separate `make -C tests/memblock unit` and `check-rtl` invocations after source
edits stopped. The initial compile error was a local `cycle` name shadowing the
method; it was corrected to `this->cycle()` before the successful build.

Two preceding schema-52 finite runs, SPEC seed 411 and corner seed 412 at 6,144
actions, both exceeded their 1,800-second timeout without a terminal summary.
`build/memblock/schema52-continuation-20261021/{spec,corner}.json` retains these
timeouts; neither is a pass or evidence of an RTL defect. Optional
`MEMBLOCK_MIXED_PROGRESS_ACTIONS` checkpoints now expose actions, cycles, and the
last phase without drawing randomness or advancing the DUT. Seed 284's focused
256-action command produces byte-identical passing terminal summaries with the
diagnostic disabled/enabled and with the preceding frozen binary; evidence:
`build/memblock/mixed-progress-equivalence/{command,results}.json`.

Exact profile/seed/action replays with diagnostics and a 7,200-second bound
both passed using `build/memblock/mixed-progress-runtime/runtime.json`: SPEC
seed 411 in 3,254.850 seconds and corner seed 412 in 3,569.371 seconds. Each
completed all 6,144 actions. Their finite frozen-runtime verifiers also pass;
artifacts are `build/memblock/schema52-progress-replay/{spec,corner}.json` and
`{spec,corner}-verified.log`. These terminal results supersede the earlier
liveness observations; they establish finite capability, not closure.

Residual gaps: reset assertion/synchronized release outputs remain unmonitored;
partial modifying atomics, partial error refills, CMO reset, reservation reset,
dirty-cache persistence, and full cache-line DUT readback remain excluded. No new
CPU bug or closure claim follows from this evidence.

### Partial Scalar Load Reset

- Review IDs: `partial-load-reset-20261021` (six singleton cases), then
  `partial-load-merge-reset-20261021` (six additional opposite-half pairs).
- Trigger and snapshots: material scalar partial-refill reset extensions against
  `32eb013e0`; reviewed diffs are
  `build/memblock/partial-load-reset-review-v3.patch` and
  `build/memblock/partial-load-merge-reset-review.patch`.
- Independence: primary author and fresh-context non-author reviewers
  `partial_load_reset_review` and `merged_load_reset_review`, respectively.
- Scope: scalar identity/completion and LQ disposition, independent data and
  recovery accounting, leaf dispatch/inventory, and associated contracts.
- Final verdicts: `pass-with-gaps` from both reviews; no blocking oracle findings.
- Disposition S1: accepted and fixed in the singleton review. The Known Boundary
  Gaps inventory now distinguishes outstanding manager cancellation from retained
  scalar completion/LQ dequeue.
- Disposition S2: the initial singleton run did not exercise pending scalar or
  old-LQ cancellation. Outcome counters exposed this gap. The new paired cases
  now exercise both; singleton-only cancellation branches and other legal
  completion/retirement permutations remain unobserved.

A bounded RTL reachability audit explains why standalone clean LR cases do not
reach keyword or pre-final-D writeback on this revision: LR uses AMO_SOURCE,
both Acquire allocation paths select keyword only for LOAD_SOURCE, and the LR
terminal response requires final refill data. Early GrantAck remains possible.
This is implementation reachability evidence, not an oracle timing requirement;
the LR oracle still permits legal early completion and either order. References
and exclusions are in `build/memblock/partial-lr-reset-design-audit.json`.

`reset-partial-load` crosses singleton and same-line opposite-half LD pairs over
all three primary issue lanes and both target halves (twelve cases). Each resets
with one accepted D beat and one manager credit still outstanding. Every old
identity is registered before issue; completed results remain validated, and
only exact pending expectations can be canceled at the immediate reset boundary.
Observed LQ dequeues are retained separately from reset queue cancellations.
A distinct survivor and readbacks of every old target plus the survivor check
independent data; five accepted D beats, request/GrantAck drain, complete backing
line bytes, queue conservation, and a quiet tail beyond the interbeat gap check
stale or lost work.

The latest focused replay passes at cycle 18,397: twelve outstanding manager
resets, twelve survivor writebacks and thirty readbacks. Singleton scalar/LQ
outcomes remain `6,0` (completed,canceled / dequeued,reset-canceled), with request
orders `3,3` (nonkeyword,keyword). Paired scalar and LQ outcomes are both `6,6`,
with request orders `3,3`; all six pair cases have both a completed and pending
identity at reset. Thus this runtime now demonstrates ordinary pending-load
cancellation and nonzero old-LQ reset cancellation, while retaining completed
neighbors. It does not establish every legal completion or retirement ordering.
The previous six-singleton terminal at cycle 8,838 remains in
`build/memblock/partial-load-reset-controls-v3.json`.

An initial combined 19-context `reset-recovery` attempt failed in the DPI dynamic
loader with `cannot allocate memory in static TLS block`; the bind-now replay
failed identically. Logs remain in
`build/memblock/partial-load-reset-focused{,-bind-now}.log`. Scalar cases now run
as a separate leaf with twelve sequential contexts; the original thirteen-context
scenario independently passes. The loader failure was not a DUT functional verdict.

Validation and retained artifacts:

```text
build/memblock/partial-load-merge-reset-runtime/runtime.json
build/memblock/partial-load-merge-reset-focused.json   (12-case leaf pass)
build/memblock/partial-load-merge-reset-controls.json  (7/7 controls pass)
build/memblock/partial-load-merge-reset-checks.log     (274 tests and check-rtl pass)
build/memblock/partial-load-merge-reset-check-rtl-final.log
build/memblock/partial-load-merge-reset-evidence.json
build/memblock/partial-load-merge-reset-provenance.json
```

Commands: `make -C tests/memblock picker-harness JOBS=8`, explicit
`freeze_runtime.py` to the unique runtime, `benchmark_tests.py --jobs 1
--timeout-seconds 120 --scenarios reset-partial-load`, seven related controls
with `--jobs 3`, and `make -C tests/memblock unit check-rtl`. Exact commands and
outputs are retained in the evidence JSON. The preceding partial-LR runtime
passed all 89 registered leaves in 3,878.128 seconds
(`build/memblock/partial-lr-reset-benchmark.{json,md}`). The current runtime
passed all 90 registered leaves in 3,739.958 seconds with six processes and a
7,200-second per-leaf bound
(`build/memblock/partial-load-merge-reset-benchmark.{json,md}`). Its SPEC
`random-mixed` seed 1 completed 16,384 actions at cycle 8,526,761. This is
complete registered-leaf evidence, not closure-scale random acceptance.

The subsequent two-seed/six-scenario preflight on the current runtime produced
11 passes and one error (`build/memblock/reset-current-preflight/matrix.json`).
SPEC `random-mixed` seed 422 stopped after 3,436 of 6,144 requested actions at
cycle 3,136,937 in `random-set-pressure-clean-release-overlap`, with reason
`mixed_coverage_or_accounting_gate_failed`. The controller reports `error`
because the terminal action count differs from the request; this classification
does not identify the cause of the underlying guard failure. Exact replay on
the same frozen runtime reproduced the byte-identical failure terminal in
1,303.704 seconds at the same action and cycle. Evidence:
`build/memblock/reset-current-preflight/seed422-replay.json` and
`seed422-exact-replay-comparison.json`. Failure-only per-window diagnostics reproduced all original summary fields
apart from the deliberately expanded phase text. They establish the UT
stimulus-precondition defect described below. The matrix remains unaccepted;
this is not a CPU RTL bug.

An isolated harness negative control inserts one monitored tick after successful
reset but before canceling a paired pending identity. It fails as required at
case 6, cycle 214 on `scalar reset cancellation requires a just-completed reset`.
Command, private source, frozen runtime path, and result are retained in
`build/memblock/partial-load-merge-reset-negative/{injection.json,negative-control.log}`.
This checks the cancellation boundary guard, not an RTL mutation or CPU bug.

Residual gaps include singleton-only cancellation branches, other completion/LQ
retirement permutations, and the reset-domain, modifying/error-refill, CMO,
reservation, dirty persistence, and full-line DUT-readback exclusions already
stated. LQ retirement remains checked through aggregate top-level counts. No new
CPU RTL bug or closure acceptance is established.

### Clean Replacement Overlap UT Correction

- Review ID: `clean-release-overlap-fix-20261021`; fresh non-author source and
  contract review finished `pass-with-gaps`. The medium finding on stale plan
  geometry/closure descriptions and low missing-overlap qualifier were accepted
  and corrected. Evidence: `build/memblock/clean-release-overlap-fix/review.json`.
- Exact seed 422 replay reproduces action 3,436/cycle 3,136,937. Failure-only
  diagnostics show window 5 had one target Release before the holds and two
  total, with all eight responses held and all eight load identities pending.
  Subtracting the earlier Release and requiring two further Releases relied on
  unproven baseline residency. This is a UT stimulus precondition defect.
  `build/memblock/seed422-overlap-diagnostic/diagnosis.json` retains every
  window and confirms unchanged original summary fields apart from phase text.
- Clean overlap now allocates four older targets per window without issuing
  them, establishes all younger held responses, issues the older targets, and
  requires their exact writebacks and observed LQ dequeues before allocating
  remaining targets. Required Releases, exact data/identity checks, and final
  disposition conservation are preserved. Initial/tail LQ maxima are 40/56
  below the 66-flow watermark. C backpressure starts before target issue.
- The generated geometry is four ways and 256 sets. The existing 8-KiB stride
  visits two physical sets of total capacity eight per window; authoritative
  documents now describe that scope. Single-set associativity remains a gap.
  Field layout and dimensions stay schema 52; creation order and metadata RNG
  assignment change, so replay requires the corresponding frozen runtime.
- The isolated candidate passed 128 total actions, including 27 clean/Bare/
  depth10/eight-window overlap/backpressure actions, 216 held loads, 432
  attributed Releases during holds, and 4,536 exact writebacks/dequeues.
  `build/memblock/clean-release-order-candidate/focused-{command,result}.json`
  and `focused.log` retain the command and terminal evidence. Original scheduling
  also passes this short constraint profile, so it is not claimed as a minimized
  original-failure reproducer.
- Integrated runtime: `build/memblock/clean-release-overlap-fix-runtime/runtime.json`.
  Build, frozen-runtime command, checks, and focused runs are under
  `build/memblock/clean-release-overlap-fix/`. All 274 unit tests, RTL/docs
  checks, and eight related directed controls pass. The integrated 128-action
  terminal is byte-identical to the reviewed candidate. A 512-action seed 423
  compact-latency clean-overlap run passes all 384 enabled window x depth x
  width x translation x C-backpressure bins: 411 accepted pressure actions,
  1,862 held loads, 2,790 during-hold target Releases, and 37,234 exact target/
  overlap writebacks and dequeues. Both focused terminals pass the offline
  schema-52 constraint/accounting checker (`focused-offline-verification.json`).
  Frozen-runtime SPEC 422/6,144 passes in 3,416.363 seconds at cycle 7,507,380,
  with matching finite artifact verification (`spec422-verified.log`). The
  private O2 candidate passes at the same cycle in 3,117.072 seconds, its
  terminal passes offline schema-52 accounting, and its terminal is byte-identical
  to the clean-only integrated runtime (`pressure-scheduling-fix/spec422-comparison.json`). No new full-inventory or
  closure result is claimed.
- Dirty overflow stores now allocate their SQ entries before dispatch of the
  younger held loads. Address/data issue and commit remain deferred until all
  holds exist; no-overlap scheduling, RNG draws, and identity creation are
  unchanged. Maximum baseline/overflow SQ batches are 32/16 below the usable
  52 entries. The private candidate passed seed 424/512 with all 384 enabled
  dirty overlap bins: 411 pressure actions, 1,849 held loads, 2,749 attributed
  target ReleaseData completions, and 17,541 exact store writebacks/SQ dequeues.
  Offline schema-52 constraint/accounting checks pass. Evidence:
  `build/memblock/dirty-pressure-dispatch-candidate/{focused-result,focused-summary,review}.json`.
- Combined integration review `pressure-scheduling-fix-20261021` by fresh-context
  non-author `/root/clean_release_order_review` finished `pass-with-gaps`, with
  no findings requiring changes. It checked the actual integration diff and
  contracts, byte equality to the reviewed dirty candidate, preserved
  data/identity/release/queue checks, and the private focused terminal.
  `build/memblock/pressure-scheduling-fix/{review.json,review.patch,source.patch}`
  retains the snapshot and verdict. Dirty baseline ownership and the existing
  output-only store filtering limitation remain explicit gaps.
- Latest combined runtime:
  `build/memblock/pressure-scheduling-fix-runtime/runtime.json`. Build/freeze
  commands, source snapshot and checks are under
  `build/memblock/pressure-scheduling-fix/`. All 274 unit tests, RTL/docs checks,
  and eight related directed controls pass (`checks.log`, `controls.json`).
  Clean/dirty 512-action focused runs each pass all 384 enabled overlap bins
  and offline schema-52 constraint/accounting checks; both terminals are
  byte-identical to the corresponding preceding reviewed runs
  (`{clean,dirty}-crosses-{result,summary}.json`, `focused-comparison.json`).
  SPEC 422/6,144 passes in 3,262.579 seconds at cycle 7,497,708; the matching
  finite artifact verifier requires the exact action count, backpressure and
  frozen runtime (`spec422.json`, `spec422-verified.log`). This is finite
  validation, not duration acceptance. Dirty baseline ownership remains an
  unproven stimulus precondition; populated dirty bins do not establish closure. No CPU bug or
  closure follows from these UT corrections.

The subsequent read-only dirty ownership diagnostic retains the original
seed 424/512 compact terminal byte-identically. All 411 dirty overlap actions
have exactly one store-class NtoT baseline acquisition per line, unchanged
error counters, and zero completed target Releases at the proposed qualification
point. In 372 actions the baseline D/E links are not yet drained before hold
setup; only 39 satisfy that additional prerequisite at the existing boundary.
This motivates an explicit baseline D/E drain before qualification, not an RTL
bug conclusion. The corresponding DCache-SPEC seed 425/512 diagnostic also
passes: 401 of 411 actions have undrained baseline D/E; none has a bad
acquisition, changed error counter, or completed target Release. Explicit
baseline D/E drain then qualifies all 411 actions on each of compact 424 and
DCache-SPEC 425 (14,792/14,616 baseline lines, cycles 588,286/1,620,743), with
matching offline constraint checks. These are private measurements; canonical
ownership checking and rejected-attempt cleanup remain unimplemented. Evidence:
`build/memblock/dirty-ownership-qualification/{compact424,dcache-spec425,drained-compact424,drained-dcache-spec425}-measurement.json`,
`diagnostic.patch`, `drain-candidate.patch`, and `cleanup-audit.json` in that tree.

### Hypervisor Fault Oracle Correction

- Bounded fresh audit `/root/hypervisor_historical_audit` covers `04c0d157`,
  `22cc0b6b`, and `3f00c595`; snapshot and dispositions are retained under
  `build/memblock/hypervisor-bounded-audit/{input.json,handoff.json}`.
- H fault correctness now uses exact exception/identity, RF suppression,
  manager restrictions, and queue recovery. Directed HLVX physical PMP R-allow/
  X-deny faults permit target-line demand-load Acquires without requiring their
  presence or cardinality; each accepted request is independently qualified.
  Unrelated demand/store/CMO/Get and all Uncache activity remain forbidden;
  legal hardware prefetch stays separate. Directed and mixed H faults no longer
  require nonzero/equal or lane-last-cycle wakeup/cancel pulse observations.
- Review ID `hypervisor-fault-oracles-20261022`; fresh non-author
  `/root/hypervisor_oracle_review` returns v2 `pass-with-gaps`. Both v1 findings
  were accepted: remove an unsupported new maximum-one target-read limit, and
  correct a finite-run argument combination rejected before simulation.
  Original artifacts, dispositions, and exact changed-logic re-review are in
  `build/memblock/hypervisor-oracle-correction/{review-v1/,v1-dispositions.json,v2-review-delta.patch,review.json}`.
- Private candidate passes nine production DCache A checker unit tests, all
  82 directed H cases, and mixed seeds 551/552 with 256 actions each, compact
  versus SPEC DCache/PTW latency and compact Uncache latency. Both reach all
  42 H/PMP relation cross bins with zero unobserved cancellations. Evidence:
  `unit.log`, `validation.json`, `directed.json`, `mixed-551.json`, and
  `mixed-552.json` under the same candidate directory. This is finite evidence.
- Integrated validation passes all 279 unit tests and RTL/docs checks, six
  related directed scenarios, and frozen-runtime replays of both 256-action
  mixed seeds. Their complete terminal outputs match the reviewed private
  candidate byte-for-byte; independent constraint accounting also passes.
  Evidence: `integrated-checks.log`, `integrated-controls.json`,
  `integrated-mixed-validation.json`, `integrated-551.json`, and
  `integrated-552.json`. Frozen runtime:
  `build/memblock/hypervisor-oracle-runtime/runtime.json`.
- Source and unit integration match the reviewed snapshot byte-for-byte;
  `integration-provenance.json` records the document merge with separately
  reviewed Debug sensitivity evidence. `3f00c595` is a **Stimulus gap**: H
  producers exist, but same-lane scalar H/vector split-flow overlap remains
  unproven. The historical permission reverts remain unbuilt; no new CPU RTL
  bug or regression closure is claimed.

- A separate private same-lane H/vector prototype passes 64 cold cases across
  SPVP=U/S, LDU=0/1, HLV.D/HLVX.WU, and scalar issue offsets 0-7 relative to
  a 16-flow strided byte vector load. Ordinary host address A and guest alias
  A map to different initialized images; the scalar maps C to D, and the guest
  alias lacks X for the HLVX discriminator. Exact terminal data/identity and
  17 LQ retirements decide correctness. Fresh non-author review
  `hypervisor-vector-prototype-20261022` returns `pass-with-gaps`: stimulus
  legality passes, internal overlap and historical sensitivity remain unproven.
  Evidence: `build/memblock/hypervisor-vector-prototype/{input.json,reviewed-input.json,source.patch,clean.json,review.json,review-dispositions.json,runtime-resolution.json}`.
  The runtime-path metadata discrepancy was corrected after preserving the
  reviewed input; all three referenced model images are identical and explicit
  library selection is recorded. The isolated equivalent `3f00c595` model
  build is in progress under `mutant/`; no production integration or coverage
  upgrade is claimed from the offset sweep alone.

### DebugModule Access Guard Matrix

- Review ID `debug-guard-expansion-20261021`; fresh non-author
  `/root/debug_guard_expansion_review` inspected the actual final v5 source
  diff, contract diff, active SoC map, retained specification, and finished
  clean/mutant evidence. Verdict: `pass-with-gaps`. Both low findings were
  accepted: the mutant stops before the new bins, and inherited unmatched
  store-data pulse filtering remains. Evidence:
  `build/memblock/debug-guard-expansion/review.json` and `source.patch`.
- `mmio-contracts` adds 24 LD/SD x initial-nondebug/Debug-Mode/exit x aligned-edge
  cases at `0x3801fff8`, `0x38020000`, `0x38020ff8`, and `0x38021000`. Each of
  four DUT contexts survives mode transitions. The configured protected region
  is `[0x38020000,0x38021000)`; adjacent PMA intervals remain uncached RW.
  Expectations come from `top/Configs.scala`, `PMParameters.scala`, active
  `system/SoC.scala`, and the official execution-based Debug implementation
  appendix, which leaves the address map implementation-specific. The retained
  official source and content provenance are
  `build/memblock/debug-guard-expansion/audit/{implementations-main.adoc,normative-source.json}`;
  upstream URL is `https://raw.githubusercontent.com/riscv/riscv-debug-spec/main/implementations.adoc`.
  This is not a universal RISC-V address-map requirement.
- Protected nondebug accesses require exact access faults and zero memory-manager
  requests. Successful accesses require exact Uncache traffic and independent
  bytes. Faulted stores stay younger than the retirement frontier, are never
  authorized to retire, and receive an explicit top-level redirect; exactly one
  observed SQ dequeue or cancellation is accepted. Twelve store cases read back
  through the DUT after completion or fault. Existing denied-load feedback
  counts remain diagnostic; their old cardinality gate conflicted with the
  authoritative anti-oracle rule and was removed. The denied-load wait additionally
  observes manager request deltas while preserving legal early completion.
- Private clean v5 passes 24 cases, four denied loads, four denied stores,
  eight exit cases and twelve readbacks, with four observed fault-store SQ
  cancellations (`clean-v5.json`). The isolated generated-RTL equivalent guard
  revert disables only the shared DebugModule range predicate and its 64 PMP/PMA
  guard uses. Two matched mutant runs fail byte-identically at the existing
  interior denied-load case, cycle 8408: expected fault completion is missing
  until the bounded timeout. No forbidden accepted manager traffic is observed.
  The same harness binary and command arguments are used;
  `sensitivity-verification.json` and `mutant/{provenance.json,mutation.patch,wrapper-comparison.json}`
  retain the checks. The mutant does not reach the added matrix, so this is
  historical guard sensitivity, not per-new-bin mutation coverage or a new CPU bug.
- Review ID `debug-guard-eight-selected-bin-independent-review-2026-10-22`;
  fresh non-author `/root/debug_guard_bin_review` returns `pass` with no
  findings. A private fixture selects each of the eight protected first/last
  doubleword x LD/SD x initial-nondebug/real-Debug-exit bins from the canonical
  matrix without changing its access lambda or functional oracles. All eight
  pass twice on clean RTL and fail twice on the existing equivalent historical
  mutant; same-case outputs are identical. The exact same harness binary and
  arguments run with independently checked clean/mutant library resolution.
  These selected fixtures reset before entry and do not establish full original
  stream equivalence. Mutant failures are bounded missing-writeback timeouts,
  not accepted forbidden traffic or a new CPU RTL bug. Evidence:
  `build/memblock/debug-guard-bin-sensitivity/{input.json,fixture.patch,summary.json,review.json}`,
  model provenance remains `debug-guard-expansion/mutant/provenance.json`.
- The reviewed candidate is integrated byte-identically. All 274 unit tests,
  RTL/docs checks, and seven related directed controls pass. The integrated
  `mmio-contracts` terminal is byte-identical to the private O0 candidate
  (`integrated-comparison.json`). Current frozen runtime:
  `build/memblock/debug-guard-runtime/runtime.json`. The 90-leaf inventory is
  running with six processes and a 7,200-second per-leaf bound; build, freeze,
  checks and result artifacts are under
  `build/memblock/debug-guard-expansion/`. Misaligned region crossings,
  instruction fetch, restrictive-PMP override, and the
  inherited store-output monitor gap remain. No full-inventory or closure
  acceptance is claimed for this new harness.

### Schema 53 Prefetch Interference in Dirty Ownership

- Review ID: `schema53-prefetch-quiesce-20261022`; independent review was not performed because the operator stopped verification work.
- Trigger: the first complete schema-53 90-leaf inventory is 89/90 and the
  two-seed six-scenario matrix is 10/12, both from `random-mixed` construction
  exhaustion.
- Diagnostic replay: SPEC seed 531 stops at action 4,148 of 6,144. The
  ownership check sees zero bad acquisitions, zero manager error-state changes,
  and one early Release with `after_baseline=0`. Window 4 has `set=125`; the
  released target line is `0xc5583f40` in set 253. The global A summary during
  hold setup contains a background L1 stride prefetch
  (`opcode=6,size=6,address=0xc5593f40,source=4,reqSource=6`) at cycle
  5,960,407, in target set 253; the target ReleaseData follows at cycle
  5,960,465. All six held loads are `reqSource=2` and in other sets. A
  prefetch to a different line in the same set is a legal eviction, so this is
  a UT ownership-precondition defect, not an RTL data/ordering bug.
- Reverted candidate: a bounded wait for every baseline line's store NtoT
  acquisition was implemented and replayed on the same seed; it still rejected
  at action 4,148 (two attempts, then exhaustion). It was removed and is not
  the fix.
- Fix: quiesce the L1 stride prefetcher through its external CSR enable,
  advance eight cycles, and drain DCache idle before the set-pressure action;
  restore the prior enable after the action. The prefetcher remains enabled for
  ordinary mixed traffic; only the ownership-sensitive action is quiesced.
  The quiesce consumes no RNG or architectural action.
- Release monitor: full-line, line-aligned Release/ReleaseData records are
  counted through a line-granular index; any non-full or unaligned record
  disables the index and the original overlapping-range scan runs. Five
  directed terminal summaries (`dcache-errors`, `cmo-contracts`,
  `dcache-coherence`, `dcache-release`, `sbuffer-flush`) are byte-identical to
  the frozen runtime; four new unit cases cover full-line/data counts, non-full
  fallback, unaligned fallback, and clear. The index-only 16,384-action seed-1
  replay fails at the same action 4,916, cycle 6,480,472, and ownership tuple
  as the frozen runtime (`build/memblock/dirty-index-probe/inventory-index.json`),
  so the index changes counting cost, not decisions.
- Evidence so far: 283 Python units and `check-rtl` pass; the pre-fix
  diagnostic is `build/memblock/dirty-fast-diag2/fastdiag2-531.log`, and the
  frozen runtime `build/memblock/prefetch-index-runtime/runtime.json` was
  built. Focused replay and campaign reruns were stopped by operator request
  before completion; no independent review or acceptance claim follows.
- Residual gaps: the prefetcher's training state and any non-prefetch
  background agent remain unproven; the quiesce deliberately removes one
  legal background source only for this ownership-sensitive action. No new CPU
  RTL bug or closure claim follows.

### Documentation Cleanup

- Review IDs: `doc-cleanup-semantics-20261021`,
  `doc-cleanup-structure-20261021`, and `doc-cleanup-evidence-20261021`.
- Reviewed state: the compacted README, Plan, ORACLES, constrained-random
  contract, Results, historical audit, CPU bug reports, and removal of the
  verbatim prompt log.
- Independence: three fresh-context, non-author reviewers checked semantic
  preservation, references/structure, and historical/bug evidence separately.
- Initial findings: missing constrained-random gaps and reporter contracts,
  one malformed oracle table, stale links/wording, and an incorrect branch-local
  commit description. All findings were accepted and fixed.
- Final verdict: `pass` from all three focused re-reviews after the fixes.

Verification: `make -C tests/memblock unit` passed 269 tests,
`make -C tests/memblock check-rtl` passed, the local-link/table scan passed 50
links, and `git diff --check` passed. Evidence is in
`build/memblock/documentation-cleanup-review-20261021/` (`unit.log`,
`check-rtl.log`, and `diff-check.log`).

## Historical Archive

Detailed superseded campaign narratives, stale artifacts, schema 11-47 closure
logs, and older current-harness snapshots were removed from this index because
they no longer define current evidence. The pre-compression history remains in
Git:

```sh
git show 3e5dc4029:tests/memblock/docs/RESULTS.md
git log --follow -- tests/memblock/docs/RESULTS.md
git show 3e5dc4029:tests/memblock/docs/USER_PROMPTS.md
```

Use that history only for provenance or failure archaeology. A historical pass,
hash, seed, or artifact does not become current acceptance evidence without an
explicit replay under the current runtime and contracts.

The reviewed Debug guard runtime subsequently passes all 90 registered leaf
scenarios (`debug-guard-expansion/inventory90.json`, `inventory90.md`). The
seed 1 SPEC random-mixed leaf completes all 16,384 actions at cycle
8,681,175 in 3,622.765 seconds; its schema-52 terminal passes
the offline constraint/accounting checks (`inventory90-verification.json`).
Inventory elapsed time is 3,671.691 seconds with six jobs. This finite leaf
inventory is not the eight-hour breadth or eight-million-action endurance
acceptance. Artifact paths are under `build/memblock/`.

### Dirty Ownership Qualification and Rejected Construction

- Review ID `dirty-ownership-qualification-20261022`, fresh non-author
  `/root/dirty_ownership_review`. V1 failed for three accepted findings:
  per-target missing A could be hidden by retry, repeated ReleaseData
  transactions were equated with distinct immutable images, and schema
  declarations remained stale. V2 requires at least one accepted covering A
  per fresh target after cleanup, permits repeated byte-checked ReleaseData,
  separates immutable images/preserved distinct stores from transaction counts,
  and synchronizes schema 53. Changed decision logic was re-reviewed;
  verdict `pass-with-gaps`, no findings. Exact manifest, actual patches,
  dispositions and verdicts: `review-input.json`, `review-v1/`,
  `v2-review-delta.patch`, `v1-dispositions.json`, `review.json` under
  `build/memblock/dirty-ownership-prototype/`.
- Qualification drains committed baseline SBuffer and D/E, allocates all older
  overflow stores, establishes all younger architectural holds, then checks
  accepted full-line store NtoT acquisitions, unchanged error counters, and
  zero completed target Releases before arming C backpressure without another
  DUT cycle. Retained coherence responsibility can be in a writeback queue;
  no array-residency claim is made. Rejected attempts release holds and finish
  each preallocated store exactly once through legal commit and independent
  reference updates; all data/identity/protocol checks stay active. Full
  WB/queue/SBuffer/D/E/C drain precedes fresh-address retry. Unqualified
  overlap/stall minima alone are inapplicable. Accepted crossbins and logical
  action counts exclude rejected construction; actual traffic and latency
  samples remain counted. Four unsuccessful attempts explicitly fail as
  `stimulus_construction_exhausted`.
- Private normal compact 424/512 and DCache-SPEC 425/512 each pass all 384
  enabled dirty-overlap bins and 411 qualifications with zero rejection, at
  cycles 588,286 and 1,620,743. All prior drained-measurement terminal fields
  agree except schema; qualification adds no DUT cycle or RNG draw.
  Artificially rejecting the first otherwise qualified attempt passes the
  complete 384-bin compact run: 822 attempts, 411 accepted plus 411 cleaned
  rejections, 17,590 rejected stores and 1,850 rejected held loads, at cycle
  1,152,779. Separate eight-window forced 128-action v2 run passes 27 cleaned
  rejections plus 27 accepted retries at cycle 138,902. These injections test
  UT construction/cleanup, not a CPU mutation or naturally observed loss of
  ownership (`candidate-verification.json`, `forced-crosses-summary.json`,
  `forced-v2-summary.json`).
- V2 normal compact 424/512 passes with terminal byte-identical to v1
  (`normal-v2-summary.json`). The four-attempt harness exhaustion check reports
  the expected nonzero exit and all four attempts cleaned (`exhaust-result.json`).
  A v2 harness observation fault hiding one target A is rejected at
  `random-set-pressure-dirty-rejected-missing-target-request`, retains in-flight
  accounting, and cannot become a retry PASS (`missing-a-v2-result.json`).
  Repeated ReleaseData has source review and C++/offline accounting-rule
  evidence, not an RTL runtime reproducer.
- All 278 units and RTL/docs checks pass after integration
  (`integrated-checks-v2.log`); the initial two obsolete emitted-schema literal
  assertions and their mechanical corrections are preserved separately
  (`integrated-checks.log`, `mechanical-integration-adjustments.json`). Candidate
  docs validation is retained in `v2-docs-command.json`, `v2-docs-check.log`.
  Four related directed controls pass on the private candidate (`controls.json`).
  Source integration is byte-identical to the reviewed v2 manifest; canonical
  build/freeze provenance is retained. Integrated compact 424/512 passes at
  cycle 588,286 in 264.292 seconds; offline checks pass and the entire terminal
  is byte-identical to reviewed private v2 (`integrated-result.json`,
  `integrated-summary.json`). Five related directed controls also pass,
  including `mmio-contracts` (`integrated-controls.json`). Current frozen-runtime
  `inventory90` and the two-seed six-scenario finite matrix remain running with
  independent outputs. No CPU bug or closure conclusion follows from these UT
  changes.
