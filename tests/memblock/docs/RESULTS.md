# Current MemBlock Qualification

## Scope

This file describes current readiness and its qualification boundary. It is
not a chronological log. Capability inventory comes from the current catalog,
producer/check implementations and the current-state documents, not preserved
campaign claims.

The catalog registers 91 scenarios: 90 directed-tests plus constrained-random.
The default benchmark selects 85; six additional directed contracts require
explicit execution. All claimed behavior remains limited by IO_COVERAGE.md
and ORACLES.md. There is no defensible overall functional-coverage percentage.

## CPU Bug Records

None.

## Current Changes Qualified Below

- Regression and benchmark sidecars use `coverage-reports/<run_id>/`; both
  aggregate and per-case documents record the campaign ID. Concurrent
  controllers cannot overwrite each other's sidecars if a parent is shared,
  and reruns retain previous sidecars. Operational isolation still requires
  unique parent output directories per concurrent controller. A subprocess integration test exercises
  two regressions plus a benchmark with identical scenario/seed/index and an
  aggregate-path rerun.
- Constrained-random finishes with a drain and an independent word readback of
  cacheable identity-mapped committed stores/atomics; permitted/aligned
  cacheable hypervisor stores add a same-context HLV readback.
- DCache E GrantAck is admitted only after a first Grant/GrantData D beat (or in
  that same cycle), with sink lifetime tracked through E.
- PTW and Uncache reject source reuse before final D and D completion of an
  inactive source; same-cycle final-D/source reuse is allowed.
- Successful CBO.CLEAN/FLUSH lines stay in the final readback; only CBO.INVAL
  and errored CMO lines are excluded. The observed-word count is incremented
  per completed batch and gated against the registered count.
- `atomic-contracts` forces a ProbeAckData on the dirty AMO line and checks the
  updated data bytes plus the manager image, so AMO post-state is not observed
  only through a cache-resident load.
- DCache C-channel ReleaseData now checks raw address/source/param/size/beat
  identity; ProbeAckData checks source/report/size/address identity; data-less
  Release checks this DUT's full-line size, line alignment and shrink param
  subset 0..2. `corrupt` is per-beat and not a data-less Release validity field;
  TileLink user/echo fields are DUT-specific, not stability fields.
- First-element FOF completions now carry an explicit exceptional `vstart`
  expectation. `cbo-zero-contracts` also contains sequential, same-line
  repeated and three-line consecutive CBO.ZERO retirement, both same-line
  CBO.ZERO/ordinary-store orders, a dirty-line zero, a younger load issued
  while the zero is still completing, and an
  older-load/younger-store/CBO age-direction control. Each driver advances the
  ROB pending pointer only after the preceding deferred CBO completion.
- Vector transactions model and drive mask bits 0..127 (`mask_bits` plus
  `mask_high`). `vector-addressing` checks high-element unit-stride load/store,
  readback, LMUL8 full-range masks, and ordinary/special indexed shifts; mixed
  vector generation draws high mask bits when `vm=0` and reports a `high_mask`
  count of masked uops with an enabled global element >=16 (seed 214: 56 in
  1024 actions; seed 215: 432 in 4608 actions). Four other 4608-action seeds
  pass with that generator, in addition to the directed cases.
- DCache ProbeAck requires an accepted B expectation for the same probe
  address; a C before B, a B without an expectation, and duplicate B
  acceptance fail. C's client-owned source is not tied to B source, so
  same-address attribution remains address-keyed. Directed coherence, release,
  atomic and a mixed run exercise the checker.
- `translation-faults` adds a two-uop vector page fault: uop0 reads the mapped
  tail of a page, uop1 faults at the next page with `vstart=8`. The exceptional
  writeback is checked for exact `vstart=8` and fault address `page+0x1000`.
- `translation-faults` also checks two single-uop page-crossing vector loads:
  one with element 0 mapped and element 1 in an unmapped page, and one with
  only element 1 active (mask bit 0 clear). Both require a load page fault and
  exact `vstart=1`; the masked case proves `vstart` is the architectural
  element index, not the active-element count. A third all-masked load at an
  unmapped VA must complete without a fault; the same run observes zero PTW
  and zero DCache requests for it. Both faulting cases require the exact
  faulting-element address in the separate exception-address payload; the
  sample follows a four-cycle settle because the vector feedback reaches the
  queue one cycle after the writeback. A vector store whose second element
  lands in a read-only page must fault with exact `vstart=1` and address; its
  read-only page is read back unchanged, while the writable first element may
  legally have been written before the fault. A masked variant activates only
  element 1 and requires both the inactive writable page and the read-only
  page to stay unchanged. A first-element variant puts element 0 itself in a
  read-only page and requires `vstart=0` with neither that page nor the
  following writable page written. A partially masked load with only element
  0 active must complete even though inactive element 1 points into an
  unmapped page.
- `vector-segment-noncanonical` issues a legal `nf=1` two-uop segment. It
  permits the first data uop to complete before the fault but requires the later
  uop of the same instruction to expose the exact noncanonical-address page
  fault, with no DCache/Uncache request or SBuffer side effect.
- `translation-alias` maps two Sv39 virtual pages to one physical page and
  proves a committed store through one VA is visible through the other, both
  after drain and with SBuffer non-empty at an in-flight load issue. A cold
  first access that raises `ld2Cancel` is recognized as an opt-in legal
  canceled attempt and its LQ cancellation is accounted; exact replay remains
  outside the UT.
- Mixed finish also checks every committed non-cacheable store/atomic word
  against the manager bus image after drain. This observes the bytes carried by
  accepted A requests; it is not a DUT uncache readback, and device/CMO
  semantics remain scenario-owned.
- `translation-permissions` reads the target word back after a permission-fault
  store, using the same readable mapping or a Bare identity fallback. This
  checks that a faulting store did not leak bytes into the cache or backing
  image; ten fault-store cases are covered. `pmp-contracts` adds a PMP-denied
  store, reconfigures the region to allow reads, and verifies the original
  physical word by Bare identity readback.

## Current Qualification

The current qualification commands are:

- `make -C tests/memblock prepare-rtl check-ports check-rtl unit`
- `make -C tests/memblock picker-harness JOBS=8`
- All 90 directed catalog entries through the rebuilt harness
- A `coverage` constrained-random seed with 4608 actions

RTL generation and model rebuild were run after overlaying the current source
with public `origin/kunminghu-v2` revision
`a4eeaabb4c12349259094a8b55f3ba1c03a9d993`. The working tree also retains the
current branch's later MemBlock fixes. Artifacts are local under
`build/memblock/verify-kunminghu-v2-a4eeaabb4` and are not guaranteed by a clean
clone. Runtime metadata records layout, not source-to-model identity.

The current qualification is:

| Check | Current result | Scope limit |
| --- | --- | --- |
| Infrastructure unit | 294 tests pass | Unit checks do not establish DUT functional coverage |
| Campaign report isolation | Concurrent two-regression/one-benchmark fixture and aggregate rerun pass | Fresh run-ID directory and matching document ID; previous sidecar bytes retained; artifact `build/memblock/report-namespace-20261028/check-unit.log` |
| check-rtl | Pass | Inventory/schema/generated-file consistency, not full prose or RTL-source provenance |
| Restored-original directed | 90/90 pass | Every catalogued directed test through the corrected harness and runtime from RTL `43b971f2`; artifact `build/memblock/verify-restored-rtl-20261028/directed` |
| Restored-original random | 4/4 seeds pass, 4608 actions each | `coverage` profile; artifact `build/memblock/verify-restored-rtl-20261028/random-4x4608`; development validation, not closure |
| Latest-RTL directed | 90/90 pass | `origin/kunminghu-v2` `a4eeaabb4` overlaid on the current branch, regenerated and rebuilt; artifact `build/memblock/verify-kunminghu-v2-a4eeaabb4/final2-directed` |
| Latest-RTL random | 1/1 seed passes, 4608 actions | `coverage` profile; artifact `build/memblock/verify-kunminghu-v2-a4eeaabb4/final2-random-1x4608`; development validation, not closure |
| Strict coverage audit | 2/2 seeds functionally pass, but strict coverage rejects the campaign | 2×4608 coverage, seeds 1..2; artifact `build/memblock/strict-coverage-results.json`; enabled probe-depth classes 4..7 were unobserved (`actual_probe_depth=[4,1,1,1,0,0,0,0]`); this is a sparse-coverage finding, not a functional failure |
| Strict coverage 16384 audit | 1/1 seed functionally passes at 16384 actions, but strict coverage still rejects | Seed 1; artifact `build/memblock/agent-campaign-20/strict-16384.json`; only 6 standalone probe sequences occurred (`actual_probe_depth=[2,2,0,0,0,0,0,2]`), so increasing action count alone does not select the enabled depth classes; a probe-weight override is the focused lever |
| Probe-heavy strict audit | 1/1 seed functionally passes at 4608 actions with `probe=500`; all eight probe-depth classes occur, but strict coverage still rejects | Artifact `build/memblock/agent-campaign-21/probe-strict-4608.json`; `actual_probe_sequences=150`, `actual_probe_depth=[82,33,6,4,6,11,4,4]`; strict now fails on enabled `actual_probe_cross` classes with zero observations, so the next lever is more probe sequences or a focused cross distribution |
| Balanced strict audit | 1/1 seed functionally passes at 4608 actions with `probe=1000 canonical-boundary=500`; vector canonical-boundary class 0 is now covered, but probe-cross classes re-sparsify | Artifact `build/memblock/agent-campaign-23/balanced-4608.json`; `actual_probe_sequences=168`; strict rejects enabled probe-cross bins at indices 13,14,18,21; demonstrates a distribution trade-off rather than a depth-only gap |
| Probe=1000 strict audit | 1/1 seed functionally passes at 4608 actions; all probe depth and probe-cross classes occur, but strict coverage rejects a vector canonical-boundary cross | Artifact `build/memblock/agent-campaign-22/probe1000-4608.json`; `actual_probe_sequences=317`, `actual_probe_depth=[149,75,15,15,17,18,14,14]`, all 32 probe-cross bins nonzero; the remaining gap is `actual_vector_canonical_boundary_cross` class (invalid_first=0, mode=0, addressing=1, eew=1). That behavior is already exercised by the directed `translation-faults` canonical-range vector-store cases (Sv39 strided eew=1 with a valid first byte); the strict rejection is a mixed-generator sparse-bin issue, not absent directed coverage |
| Deep probe window | `dcache-coherence` passes with 8 accepted B probes, 8 C responses and `deep_probe_depth=8` | Directed eight-line window; four resident clean lines report permission 2 and four absent lines report the DUT's no-permission encoding 5; the report values are scenario-owned, not an architectural permission oracle |
| Same-address B/C attribution | Two simultaneous probes to one resident line cannot be uniquely matched by address/opcode/report; a data-less C with report 5 appeared while the accepted expectation expected report 2, and C was observed before the manager's second B acceptance | C's client-owned source is not tied to B; this is an oracle gap, not a confirmed RTL violation. Source/ownership attribution remains open |
| Accepted-B probe gate | 1/1 1024-action mixed seed passes with 407 probes, plus directed coherence/release/atomic | Seed 207; artifact `agent-campaign-17`; no dedicated counter for pre-B C because an illegal C fails immediately |
| Cross-alias store/load | `translation-alias` passes at cycle 447 and also observes a first-access `ld2Cancel` (`canceled=1`), log in `agent-campaign-17/translation-alias.log` | One PA, two Sv39 VAs; after-drain and an in-flight store with SBuffer non-empty at load issue. The canceled attempt is LQ-accounted but not replayed by the UT; exact backend replay remains outside |
| CBO.ZERO ordering | Corrected sequence passes on both restored-original and latest-overlay RTL with `sequential_zero=2 consecutive_zero=2 following_store=1`, repeated same-line and three-line consecutive zero cases, both zero/store age directions, a dirty-line zero and an early younger load | CBO.ZERO remains the ROB head until deferred SBuffer completion; no simultaneous CBO completion window is legal or claimed |
| Segment noncanonical fault | Corrected legal two-uop `nf=1` segment load/store cases pass on both restored-original and latest-overlay RTL; the later uop exposes the exact page fault with no DCache/Uncache request or SBuffer side effect | The first data uop may defer the fault and is not an independent architectural oracle |
| Final word readback | observed words equal registered words in every passing mixed case | Cacheable identity path; aliases and CBO.INVAL/errored CMO lines excluded; successful CLEAN/FLUSH retained |
| High-mask driver mutation | Zeroing `src(3)` bytes 2..15 makes `vector-addressing` fail at `phase=high-mask-load`; the restored driver passes | Harness mutation only; it demonstrates directed-test sensitivity, not RTL mutation coverage |
| Vector fault vstart | Two-uop e16/LMUL2 page fault passes with `vstart=8` and exact fault VA. Two single-uop page-crossing faults pass with exact `vstart=1` and exact faulting-element addresses; the all-masked unmapped control passes and observes zero PTW and zero DCache requests; the partially masked load with an inactive unmapped element completes on the mapped element; `translation-faults` PASS log at `agent-campaign-18/translation-faults.log` | Broader fault-element/FOF progress remains open |
| Vector segment store fault side effect | `translation-faults` segment-store cases pass: field-1 fault with exact `vstart=0` and unchanged read-only page; element-1 fault with exact `vstart=1`, exact element-1 field-0 address, and unchanged read-only element page; first-field fault with `vstart=0` and both the read-only field-0 page and following writable field-1 page unchanged; writable earlier fields may be partially written | Three `nf=1` unit-stride segment shapes; other field/element combinations remain open |
| Vector PMP load fault | `translation-faults` PMP load cases pass: a vector load from a PMP no-access page raises load access fault at the element address, issues no DCache/uncache request, and retires without a data writeback; the masked variant activates only element 1, faults at element 1 with `vstart=1`, and issues no request | Bare direct-map; multi-element PMP loads remain open |
| Vector PMP store fault side effect | `translation-faults` PMP cases pass: a vector store to a PMP read-only page raises store access fault at the element address, issues no DCache/uncache request, and the page reads back unchanged; the masked variant activates only element 1, faults at element 1 with `vstart=1`, and both element addresses read back unchanged; the cross-page variant faults at element 1 when element 0 is in a read/write page and element 1 in a read-only page, with the read-only page unchanged; the segment variant faults at field 1 with `vstart=0`, no request and the read-only field page unchanged; the first-field segment variant faults at field 0 and leaves the following writable field page unchanged; EEW 1 and EEW 2 single-element stores also fault with no request and unchanged page | Bare direct-map; wider segment field/element PMP shapes remain open |
| Vector store fault side effect | `translation-faults` unmasked vector-store case passes with exact `vstart=1` and fault address; its read-only target page word is unchanged. The masked variant activates only element 1 and leaves both the inactive writable page and the read-only page unchanged. The first-element variant passes with exact `vstart=0` and both pages unchanged | Writable first-element visibility and other fault-path targets remain partial |
| Permission-fault store side effects | `translation-permissions` passes with 10/10 fault-store readbacks (readable mapping or Bare fallback); `pmp-contracts` adds one PMP-denied store readback | Device fault paths still need another observation route |
| Non-cacheable final effects | committed NC store/atomic words match the manager bus image in passing mixed runs; 2/2 4608-action seeds (210/211) check 141/142 words, seed 215 checks 146, and smaller seeds cover the uncache-drain path | Manager bus image, not a DUT uncache read; device side effects, aliases and CMO remain outside |
| Atomic dirty-line persistence | `atomic-contracts` passes; ProbeAckData carries the updated line and updates the manager image | AMO target only; cross-hart/concurrent eviction not covered |

Queue accounting is exact in the mixed results, the observed-word count equals
the registered count, and per-case report commands and summaries agree with the
aggregate results. Full commands/runtime identity are in the local result
artifacts. Execution of all catalog entries is not proof of all legal input
combinations or missing oracle coverage.

## Readiness

Readiness is partial. Functional PASS means active checks accepted generated
traffic, not all legal classes occurred or every external effect was checked.
Current missing rules include remaining aliased/device final-byte observation,
uniform fault forbidden-demand checks, same-address B/C
attribution and complete C multibeat identity, wider same-line CBO.ZERO
forward-timing variants beyond the older-store/younger-zero/younger-load case,
and independent verifier schema/backpressure checks.
The accepted-B-before-C gate, full 128-bit mask operand and explicit
first-access cross-alias cancellation accounting are implemented, but the UT
does not model the backend replay that follows `ld2Cancel`.
Boundary faults, concurrency and reset/protection crosses also remain partial. PBMT=NC `CBO.ZERO` was probed and the DUT reports a store access fault
before an uncache request; the architecture leaves CBO behavior for
non-cacheable memory unspecified, so neither behavior is claimed. Four-state/
physical arrays and full-core/multi-hart behavior need other verification
boundaries.

An opt-in strict coverage check on 2×4608 coverage seeds rejects the campaign
because enabled probe-depth classes 4..7 were not observed, even though both
seeds pass functionally and the default accounting verifier accepts them. This
confirms that
the strict gate is active and that 4608 actions is not a closure-scale coverage
budget. The current 4×4608 run is a development validation, not the configured
8×16384 regression. Configured 48-hour breadth and million-action endurance
targets are not completed. Their current budgets and automated-check limitations
are defined in VERIFICATION_PLAN.md. No closure claim follows from a short
capability run or a controller/verifier return code alone.

## Current Review

The campaign report namespace change has a fresh-context, non-author APPROVE
review with no findings. The review checks both controller call sites, run-ID
propagation, the subprocess collision/rerun test and preservation of the
unique-parent operational rule. Independent focused rerun: 19 unit tests pass;
full local infrastructure qualification: 294 tests and `check-rtl` pass.
Review artifact: `build/memblock/report-namespace-20261028/review.txt`.

Temporary CPU-bug classifications for CBO.ZERO and noncanonical vector
segments were withdrawn after full-CPU ordering review. The CBO reproducer
advanced the ROB pending head while the first deferred completion was still
outstanding; the directed driver now follows actual ROB retirement order.
The segment reproducer now models a legal two-uop `nf=1` segment and permits
the later uop to expose the instruction-level fault instead of requiring the
first data uop to report it independently. Both cases are treated as UT
stimulus/oracle corrections, not CPU RTL bugs.

A fresh-context independent review rejected earlier revisions for remaining
CBO pre-completion commits, an impossible m2 segment uop sequence, a stale
oracle statement, incomplete store forbidden-traffic observation, and a
segment-decomposition guard gap. After those findings were fixed, the final
review verdict is approve with no remaining findings. The review covered the
actual diff, ROB/pending-head timing, complete-CPU segment decomposition and
the terminal fault/no-side-effect contract.

The independent atomic dirty-line `ProbeAckData` persistence review, corrected
C multibeat source/param/size/raw-address monitor and data-less Release
full-line/shrink check remain approved. Same-address ProbeAck lookup remains
by base and is a documented residual limitation.

This qualification confirms the stated capability and limitations; it does not
establish verification closure or clean RTL elaboration provenance. The oracle,
protocol lifecycle, remaining alias/non-cacheable final-byte observation,
first-access replay, vector-fault metadata and integration gaps described in the
current documents remain open.
