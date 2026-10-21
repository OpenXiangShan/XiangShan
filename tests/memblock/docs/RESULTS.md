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

Evidence snapshot: 2026-10-21.

- UT revision: `3e5dc4029674415fc3eb605d59a8fc0c8f079cba`.
- Kunminghu-v2 source baseline: `fe2288cb09361b65afa26be3f403520c4b496dbf`,
  merged into the UT branch as `4bd72b3a0`.
- Picker: `add7b522fd58ac82599f22b9fa280003728f41ea` (v2.0.1).
- xcomm: `3e4bccb73f08310c08382d3987ba2d98b83df4d6` (v0.2.0).
- Bootstrap provenance: `build/memblock/tools/picker.json`.
- General frozen runtime manifest: `build/memblock/runtime/runtime.json`.
- Latest schema-52 focused runtime:
  `build/memblock/schema52-runtime-v3/runtime.json`.
- Retained schema-51 focused runtime:
  `build/memblock/segment-high-va-verified-runtime/runtime.json`.
- Latest schema-52 focused provenance:
  `build/memblock/schema52-focused-v3/provenance.json`.
- Retained schema-51 evidence provenance:
  `build/memblock/segment-high-va-development/verified/provenance.json`.

The frozen runtime manifests record artifact paths, sizes, and resolved external
libraries. Those values are reproducibility data, not hidden acceptance hashes.

## Current Capability and Closure Status

The current generator schema is 52. `random-mixed` remains the canonical mixed
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

Supporting oracle and diagnostic improvements retained in this evidence set:

- MMIO FP payloads distinguish NaN boxing from integer sign extension, and the
  queue oracle requires observed recovery for denied/corrupt FP loads.
- The accepted-A DCache monitor checks opcode, grow, size, source, alignment,
  mask, corrupt, and defined user-address relationships without filtering legal
  prefetch or demand-merged traffic.
- The latest complete Python unit evidence is 269 tests in
  `build/memblock/agent-workflow-review-20261021-01/unit.log`.
- `check-rtl` and `check-docs` pass in
  `build/memblock/agent-workflow-review-20261021-01/check-rtl.log`.
- The schema-52 pre-decision runtime passed all 89 benchmark leaves
  (`build/memblock/schema52-runtime/benchmark.json`, 1,934.016 seconds). After
  the release-window decision fix, the affected 16,384-action `random-mixed`
  leaf passes under the current runtime in 1,701.059 seconds
  (`build/memblock/schema52-runtime-v3/benchmark-random-mixed.json`).

No schema-52 closure campaign has completed. In particular, the current records
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
