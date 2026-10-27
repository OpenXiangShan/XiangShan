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

## Confirmed CPU Bug Records

- [CPU_BUG_VECTOR_SEGMENT_NONCANONICAL.md](CPU_BUG_VECTOR_SEGMENT_NONCANONICAL.md):
  one-uop vector segment with a noncanonical VA failed to fault; fixed by
  `d7d096e6e`, covered by the extra `vector-segment-noncanonical` contract.
- [CPU_BUG_CBO_ZERO_BACK_TO_BACK.md](CPU_BUG_CBO_ZERO_BACK_TO_BACK.md):
  back-to-back cacheable `CBO.ZERO` lost one store completion; fixed by
  `f0e5ba75b`, covered by `cbo-zero-contracts`.

## Current Changes Qualified Below

- Constrained-random finishes with a drain and an independent word readback of
  cacheable identity-mapped committed stores/atomics; permitted/aligned
  cacheable hypervisor stores add a same-context HLV readback.
- DCache E GrantAck is admitted only after a first Grant/GrantData D beat (or in
  that same cycle), with sink lifetime tracked through E.
- PTW and Uncache reject source reuse before final D and D completion of an
  inactive source; same-cycle final-D/source reuse is allowed.
- Back-to-back cacheable `CBO.ZERO` no longer loses a store completion,
  including a following ordinary store behind a blocked zero. This is a
  confirmed RTL bug, documented and fixed in
  [CPU_BUG_CBO_ZERO_BACK_TO_BACK.md](CPU_BUG_CBO_ZERO_BACK_TO_BACK.md).
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
  expectation. `cbo-zero-contracts` also contains sequential and back-to-back
  zero controls, a same-line repeated CBO.ZERO pair, a three-line
  back-to-back zero burst, both same-line CBO.ZERO/ordinary-store orders, a
  dirty-line zero, a younger load issued
  while the zero is still in the SBuffer pipeline, and an
  older-load/younger-store/CBO age-direction control.
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

- make -C tests/memblock unit
- make -C tests/memblock check-rtl
- make -C tests/memblock picker-harness JOBS=16
- A frozen-runtime benchmark with all 85 selected entries and 256 random actions.
- Explicit execution of all six default-benchmark exclusions, including
  `translation-alias`.
- Finite coverage and corner campaigns with 4608 actions per seed.
- Full-mask directed and mixed campaigns described below.
- A current-tree `translation-faults` run with the single-uop/masked
  later-element faults and the all-masked control.
- An 8-seed spec confidence run with 16384 actions per seed.
- The configured 8-seed, 16384-action coverage regression.

The qualification artifacts are local under
`build/memblock/agent-campaign-12` through `agent-campaign-20` (not guaranteed
by a clean clone). The full 6x4608/4x4608 campaigns are from
`agent-campaign-12`; the older frozen benchmark is `agent-campaign-16`; the
8x16384 spec and coverage runs are from `agent-campaign-13` and
`agent-campaign-14`. The full-mask and accepted-B logs are in
`agent-campaign-17`; the earlier current-tree benchmark, private runtime,
`translation-faults` log and vector fault evidence are in
`agent-campaign-18`; the post-change benchmark and private runtime are in
`agent-campaign-20`.
The runtime was frozen after
a fresh root `build/rtl` generation, `prepare-rtl`, and Picker/model rebuild for
the `StoreQueue.scala` fix; the previous model was not reused.

The current qualification is:

| Check | Current result | Scope limit |
| --- | --- | --- |
| Infrastructure unit | 290 tests pass | Unit checks do not establish DUT functional coverage |
| check-rtl | Pass | Inventory/schema/generated-file consistency, not full prose or RTL-source provenance |
| Selected benchmark | 85/85 pass, 377.7 s | Seed 1; random budget 256; catalog-specific action overrides retained; `agent-campaign-16` |
| Current selected benchmark | 85/85 pass, 437.96 s | Seed 1; random budget 256; private runtime; artifact `agent-campaign-18/benchmark.json`; run before the four-cycle vector-feedback settle, with the settled `translation-faults` case passing separately; run alongside six CMO-stress seeds, so wall time is not a clean solo measurement |
| Post-change selected benchmark | 85/85 pass, 390.75 s | Seed 1; random budget 256; current harness and private runtime; artifact `agent-campaign-20/benchmark.json`; solo run after the vector-fault, CBO-ordering, deep-probe and strict-verifier changes |
| Extra directed contracts | 6/6 pass | cmo-store-flush, redirect-store-flush, tag-ecc-ways, vector-fof-high, vector-segment-noncanonical, translation-alias |
| All directed scenarios | 90/90 pass | Current-tree sweep of every catalogued directed test (constrained-random excluded); artifact `agent-campaign-17/directed-logs` |
| Coverage campaign | 6/6 seeds pass, 4608 actions each (27648 actions) | Seeds 101..106; artifact `agent-campaign-12`; 1029.0 s; capability only |
| Corner campaign | 4/4 seeds pass, 4608 actions each (18432 actions) | Seeds 101..104; artifact `agent-campaign-12`; 1985.0 s; capability only |
| CMO stress campaign | 6/6 seeds pass, 4608 actions each (27648 actions) | Seeds 401..406; artifact `agent-campaign-18/cmo`; the binary predates later directed-only test additions, which do not change the constrained-random CMO path; capability only |
| Current 4x4608 corner | 4/4 seeds pass, 4608 actions each (18432 actions) | Seeds 501..504; artifact `agent-campaign-19/corner`; directed-only additions after launch do not change the constrained-random path; capability only |
| Strict coverage audit | 2/2 seeds functionally pass, but strict coverage rejects the campaign | 2×4608 coverage, seeds 1..2; artifact `build/memblock/strict-coverage-results.json`; enabled probe-depth classes 4..7 were unobserved (`actual_probe_depth=[4,1,1,1,0,0,0,0]`); this is a sparse-coverage finding, not a functional failure |
| Strict coverage 16384 audit | 1/1 seed functionally passes at 16384 actions, but strict coverage still rejects | Seed 1; artifact `build/memblock/agent-campaign-20/strict-16384.json`; only 6 standalone probe sequences occurred (`actual_probe_depth=[2,2,0,0,0,0,0,2]`), so increasing action count alone does not select the enabled depth classes; a probe-weight override is the focused lever |
| Probe-heavy strict audit | 1/1 seed functionally passes at 4608 actions with `probe=500`; all eight probe-depth classes occur, but strict coverage still rejects | Artifact `build/memblock/agent-campaign-21/probe-strict-4608.json`; `actual_probe_sequences=150`, `actual_probe_depth=[82,33,6,4,6,11,4,4]`; strict now fails on enabled `actual_probe_cross` classes with zero observations, so the next lever is more probe sequences or a focused cross distribution |
| Balanced strict audit | 1/1 seed functionally passes at 4608 actions with `probe=1000 canonical-boundary=500`; vector canonical-boundary class 0 is now covered, but probe-cross classes re-sparsify | Artifact `build/memblock/agent-campaign-23/balanced-4608.json`; `actual_probe_sequences=168`; strict rejects enabled probe-cross bins at indices 13,14,18,21; demonstrates a distribution trade-off rather than a depth-only gap |
| Probe=1000 strict audit | 1/1 seed functionally passes at 4608 actions; all probe depth and probe-cross classes occur, but strict coverage rejects a vector canonical-boundary cross | Artifact `build/memblock/agent-campaign-22/probe1000-4608.json`; `actual_probe_sequences=317`, `actual_probe_depth=[149,75,15,15,17,18,14,14]`, all 32 probe-cross bins nonzero; the remaining gap is `actual_vector_canonical_boundary_cross` class (invalid_first=0, mode=0, addressing=1, eew=1). That behavior is already exercised by the directed `translation-faults` canonical-range vector-store cases (Sv39 strided eew=1 with a valid first byte); the strict rejection is a mixed-generator sparse-bin issue, not absent directed coverage |
| Deep probe window | `dcache-coherence` passes with 8 accepted B probes, 8 C responses and `deep_probe_depth=8` | Directed eight-line window; four resident clean lines report permission 2 and four absent lines report the DUT's no-permission encoding 5; the report values are scenario-owned, not an architectural permission oracle |
| Regression acceptance | 8/8 seeds pass, 16384 actions each (131072 actions) | Seeds 1..8; artifact `agent-campaign-14`; 1003.2 s; default coverage profile |
| Current 8x16384 coverage | 8/8 seeds pass, 16384 actions each (131072 actions); per-seed high_mask 1436..1916 | Seeds 301..308; artifact `agent-campaign-17`; current mixed generator; not the 48h final campaign |
| Spec confidence run | 8/8 seeds pass, 16384 actions each (131072 actions) | Seeds 1..8; artifact `agent-campaign-13`; 845.6 s; spec latency |
| Data-less Release check | 2/2 coverage and 2/2 corner seeds pass, 4608 actions each | Seeds 121..122; artifact `agent-campaign-15`; final C-release monitor |
| Full high-mask campaign | 4/4 coverage seeds pass, 4608 actions each (18432 actions); directed LMUL8 full-range case passes; seed 215 reports 432 masked uops with an enabled element >=16 | Seeds 201..204; artifact `agent-campaign-17`; the first four use a binary that predates the accepted-B probe gate |
| Current full-mask + probe gate | 2/2 coverage seeds pass, 4608 actions each (9216 actions) | Seeds 205..206; artifact `agent-campaign-17`; current tree |
| Same-address B/C attribution | Two simultaneous probes to one resident line cannot be uniquely matched by address/opcode/report; a data-less C with report 5 appeared while the accepted expectation expected report 2, and C was observed before the manager's second B acceptance | C's client-owned source is not tied to B; this is an oracle gap, not a confirmed RTL violation. Source/ownership attribution remains open |
| Accepted-B probe gate | 1/1 1024-action mixed seed passes with 407 probes, plus directed coherence/release/atomic | Seed 207; artifact `agent-campaign-17`; no dedicated counter for pre-B C because an illegal C fails immediately |
| Cross-alias store/load | `translation-alias` passes at cycle 447 and also observes a first-access `ld2Cancel` (`canceled=1`), log in `agent-campaign-17/translation-alias.log` | One PA, two Sv39 VAs; after-drain and an in-flight store with SBuffer non-empty at load issue. The canceled attempt is LQ-accounted but not replayed by the UT; exact backend replay remains outside |
| CBO.ZERO ordering | Fixed model passes; pre-fix frozen model fails deterministically; same-line repeated zero, a three-line back-to-back zero burst, same-line zero/store both orders, an older-store/younger-zero/younger-load forward window, a zero/younger-store/younger-load age control, a dirty-line zero, an early younger load and an older-load/younger-zero age control pass; log at `agent-campaign-18/cbo-zero-contracts.log` | Directed `cbo-zero-contracts`; see CPU bug document |
| Final word readback | observed words equal registered words in every passing mixed case | Cacheable identity path; aliases and CBO.INVAL/errored CMO lines excluded; successful CLEAN/FLUSH retained |
| High-mask driver mutation | Zeroing `src(3)` bytes 2..15 makes `vector-addressing` fail at `phase=high-mask-load`; the restored driver passes | Harness mutation only; it demonstrates directed-test sensitivity, not RTL mutation coverage |
| Vector fault vstart | Two-uop e16/LMUL2 page fault passes with `vstart=8` and exact fault VA. Two single-uop page-crossing faults pass with exact `vstart=1` and exact faulting-element addresses; the all-masked unmapped control passes and observes zero PTW and zero DCache requests; the partially masked load with an inactive unmapped element completes on the mapped element; `translation-faults` PASS log at `agent-campaign-18/translation-faults.log` | Broader fault-element/FOF progress remains open |
| Vector segment store fault side effect | `translation-faults` segment-store cases pass: field-1 fault with exact `vstart=0` and unchanged read-only page; element-1 fault with exact `vstart=1`, exact element-1 field-0 address, and unchanged read-only element page; first-field fault with `vstart=0` and both the read-only field-0 page and following writable field-1 page unchanged; writable earlier fields may be partially written | Three `nf=1` unit-stride segment shapes; other field/element combinations remain open |
| Vector PMP store fault side effect | `translation-faults` PMP cases pass: a vector store to a PMP read-only page raises store access fault at the element address, issues no DCache/uncache request, and the page reads back unchanged; the masked variant activates only element 1, faults at element 1 with `vstart=1`, and both element addresses read back unchanged | One page, Bare direct-map; larger/vector-segment PMP shapes remain open |
| Vector store fault side effect | `translation-faults` unmasked vector-store case passes with exact `vstart=1` and fault address; its read-only target page word is unchanged. The masked variant activates only element 1 and leaves both the inactive writable page and the read-only page unchanged. The first-element variant passes with exact `vstart=0` and both pages unchanged | Writable first-element visibility and other fault-path targets remain partial |
| Permission-fault store side effects | `translation-permissions` passes with 10/10 fault-store readbacks (readable mapping or Bare fallback); `pmp-contracts` adds one PMP-denied store readback | Device fault paths still need another observation route |
| Non-cacheable final effects | committed NC store/atomic words match the manager bus image in passing mixed runs; 2/2 4608-action seeds (210/211) check 141/142 words, seed 215 checks 146, and smaller seeds cover the uncache-drain path | Manager bus image, not a DUT uncache read; device side effects, aliases and CMO remain outside |
| Atomic dirty-line persistence | `atomic-contracts` passes; ProbeAckData carries the updated line and updates the manager image | AMO target only; cross-hart/concurrent eviction not covered |

Rows through `agent-campaign-16` were produced before the current harness-only
full-mask, accepted-B, cross-alias and fault-store changes.  `agent-campaign-17`
qualifies those new checks on the current tree, including a fresh 8x16384
coverage regression (seeds 301..308).  `agent-campaign-18` reruns the selected
85-entry benchmark on the current harness and records the new single-uop/masked
vector fault cases; it is not the 48-hour final acceptance campaign.

The CBO.ZERO pre-fix replay fails at cycle 33333 with
`timed out waiting for scalar store writebacks sq=3/6
store_writebacks=5 sbuffer_empty=0`; the fixed directed case passes at cycle
676 with `sequential_zero=2 back_to_back_zero=2 following_store=1`. This is
externally visible, deterministic and independent of testbench reference-state
errors.

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
budget. The configured 8-seed, 16384-action coverage regression is completed in
this snapshot. Configured 48-hour breadth and million-action endurance targets are
not completed. Their current budgets and automated-check limitations are defined in
VERIFICATION_PLAN.md. No closure claim follows from a short capability run or a
controller/verifier return code alone.

## Current Review

A fresh-context review rejected the first per-lane CBO.ZERO gate because it
could let a following store bypass a blocked lane of the
`DatamoduleResultBuffer` prefix FIFO. The fix now propagates the block as a
prefix, and `cbo-zero-contracts` exercises CBO.ZERO, CBO.ZERO, ordinary store.
The final re-review verdict is approve-with-findings for the code logic:
F1/F2 are fixed and directly evidenced, and the campaign-artifact finding is
closed by the completed `agent-campaign-12` coverage/corner artifacts. The sole
remaining finding is documentation-only: the CLEAN/FLUSH sentence now states
that retention observes a stale/corrupted visible data byte, not post-CMO
clean/dirty state or persistence of a dirty cache-resident line. The later
`atomic-contracts` dirty-line `ProbeAckData` persistence addition was
independently reviewed and approved; its only request was a comment cleanup,
which is applied. The corrected C multibeat source/param/size/raw-address
monitor was also approved; same-address ProbeAck lookup remains by base and is
documented as a residual limitation. The data-less Release full-line/shrink
check was corrected to ignore `corrupt` and approved.

The later non-cacheable bus-image final check, the opt-in `ld2Cancel` canceled
attempt handling, the same-line CBO/LMUL8 directed additions, and the
single-uop/masked later-element vector fault additions were not part of that
independent review; they are self-reviewed for this snapshot. The earlier
unattributed intra-uop exception-address observation was root-caused to
sampling before the vector feedback reached the LqExceptionBuffer; the
four-cycle settle now checks the exact address and the DUT payload is correct.

This qualification confirms the stated capability and limitations; it does not
establish verification closure or clean RTL elaboration provenance. The oracle,
protocol lifecycle, remaining alias/non-cacheable final-byte observation,
first-access replay, vector-fault metadata and integration gaps described in the
current documents remain open.
