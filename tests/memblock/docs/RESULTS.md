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

- Canonical ordinary, heterogeneous-window, SQ-dependency and unresolved/refetch
  loads now select integer or FLH/FLW/FLD formats from seeded class/width weights.
  Independent mapped bytes and older input-store overlays determine FP payloads
  and NaN boxing; the scoreboard checks class, destination, identity and data.
  Integer/FP numeric destination domains are 1..223/1..191, with distinct live
  roles and the restored original destination on refetch. Format choices,
  execution epochs, payload signs, declared faults, misalignment and semantic
  byte sources have conserved counters. A topology-only FP dependency sample
  contains 509 distinct plans out of 512, excluding payload and timing/order.
  Complete rename history, FP physical-zero selection, auxiliary helper formats
  and exceptional misaligned FP destination-enable semantics remain unqualified.
- `translation-permuted` shuffles all 132 pages for ordinary host/VS/G traffic,
  allowing different VA/PA cache colours. A single-client PA/alias directory
  recalls the old alias and waits for final C before capturing replacement grant
  data. Prior final D and E, same-PA B serialization and racing ReleaseAck are
  modeled separately. Context/final-observation drains freeze independent owner
  images. Voluntary releases use grouped independent update histories so pending
  SBuffer stores do not force a newer cache image; aligned update atomicity and
  overlapping order remain checked. Automatic drained recalls and final physical
  readbacks stay exact. Immutable per-byte page maps support discontinuous pages;
  root-reuse fences, legal tail branch/refetch and integer destination bounds
  remain enforced. Arbitrary simultaneous synonyms and CMO/error-grant ownership
  remain outside this direction.
- `scalar-unresolved` adds randomized one-store/one-load delayed-address
  windows with partial byte conflicts and same-region disjoint controls.
  Strict provisional loads use the pre-store snapshot; conflicts require the
  exact flush-itself identity, two-stage return and strict program-order refetch
  while the older store remains uncommitted. Store commit/drain and DUT readback
  complete the obligation. The separate store TLB uses legal miss reissue.
  Warm, provisional/refetch and readback roles have distinct live destinations;
  refetch reuses the free-list-restored original pdest with a new LQ/scoreboard
  epoch. The same-pdest correction also applies to directed alias refetch.
  The incomplete head is a non-memory ROB group, whose operand-producing member
  can finish before an independent FP member; its source witness does not prove
  cycle-accurate full-CPU timing for every manager delay.
- Ordinary and segment indexed operands now vary bounded offset spacing,
  permutation and repeated gathers. Each instruction generates one complete
  architectural index vector before CPU-compatible uop slicing. Scatters use
  disjoint whole-field groups, so reference byte expectations remain exact.
  `vector-index-repeat` selects the load reuse rate, with strict endpoint
  controls; generated operand counters include inactive elements explicitly.
  A topology-only sample has 512 distinct offset vectors out of 512 plans.
  Production builder checks cover all 78 ordinary indexed shapes, and vector
  scoreboard mutations reject swapped/missing repeated-element data for
  ordinary and segment loads.
- The scalar alias-refetch oracle requires a simultaneous, fully identified
  flush-itself request before discarding data, preserves all non-data checks,
  returns the isolated backend redirect after two registered stages, and
  requires a fresh correct-data completion. The directed case reports
  `canceled=1 refetched=1 completed=1 lq=2+0/2`: one data pulse discarded,
  both LQ entries dequeued, no fabricated LQ cancellation.
- The canonical generator includes `scalar-dependency`: 1..4 older stores,
  1..3 younger loads and optional younger-store exclusion, with randomized byte
  overlap, width/sign, operand arrival, issue order and delays. Expected bytes
  come from a pre-window snapshot overlaid only by older store inputs. The common
  ordinary-store factory preserves STA/STD lane pairing; simultaneous dependency
  loads have distinct valid destinations within their integer/FP domains. A topology-only unit sample
  (excluding data and timing/order) contains 510 distinct plans out of 512.
- Ordinary random bare/cache/atomic/NAPOT and merge/miss/bank pools use lazy
  seed-and-physical-address initial words. Reference and manager images share
  initialization while later writes remain independent. Six production-model
  unit tests pass, including wrong-address data substitutions accepted by old
  repeating patterns and rejected by the actual load scoreboard with new data.
  Fresh-context independent review approves this scoped memory change with no
  findings; artifact `build/memblock/scalar-dependency-20261028/memory-review.txt`.
- The PMP page-crossing vector load now checks accepted external requests
  through its vector-exception redirect recovery and includes a cold
  both-readable data/demand control. The single-uop shape uses LMUL1.
  `translation-faults` passes with one fault, one request-history check and
  one permissive control in `build/memblock/pmp-cross-load-20261028/directed-v3`.
- Regression and benchmark sidecars use `coverage-reports/<run_id>/`; both
  aggregate and per-case documents record the campaign ID. Concurrent
  controllers cannot overwrite each other's sidecars if a parent is shared,
  and reruns retain previous sidecars. Operational isolation still requires
  unique parent output directories per concurrent controller. A subprocess integration test exercises
  two regressions plus a benchmark with identical scenario/seed/index and an
  aggregate-path rerun.
- Constrained-random finishes with a drain and an independent word readback of
  cacheable identity-mapped store/atomic and declared permuted-page store effects;
  permitted/aligned
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
  same-address attribution uses serialized B presentation through final C.
  The client-owned C source stays fixed across beats; it need not echo B source.
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
  first access that requests flush-itself must match ROB/FTQ/RVC metadata,
  recover with actual queue deq/cancel observations, and refetch to exact correct
  data. No LQ cancellation is inferred from `ld2Cancel`.
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
| Infrastructure unit | 324 tests pass | Unit checks do not establish DUT functional coverage |
| Canonical randomized FP | Frozen runtime-v1: 30/30 finite random/control seeds pass 6656 actions; all retained artifact verifiers accept | `build/memblock/fp-random-20261029/qualification.json`; focused ordinary/dependency/unresolved, coverage/spec/corner, mixed, integer-only, single-width and aligned injected-error controls. 2023 FP checker executions (649 FLH, 700 FLW, 674 FLD), 146 heterogeneous windows, 2281/2281 final tracked words; development qualification, no closure claim |
| FP oracle and structural sensitivity | Six production helper/factory/scoreboard/accounting tests pass; 509/512 unique FP dependency topologies | Literal narrow/64-bit payloads, discontinuous pages, older-store/refetch versions and destination domains; fresh non-author review detects private no-boxing, wrong-domain and wrong-class mutations. These are helper/unit mutations, not RTL mutation coverage |
| FP adjacent directed checks | 13/13 selected scenarios pass through runtime-v1 | `build/memblock/fp-random-20261029/directed/results.json`; scalar/FP, misalignment, forwarding/recovery, translation/alias/faults, atomic and coherence; selected set, not full catalog. Exceptional misaligned FP enable semantics remain unqualified |
| Ordinary nonidentity maps | Runtime-v3: 15/15 seeds pass 6403 actions; 4408 mapped samples across all six contexts/four kinds, 1138/1138 automatic recalls, 74 cross-page uops, 4182/4182 tracked final words | `build/memblock/alias-manager-20261029/{qualification-v3.json,runtime-v3}`; diverse, coverage/spec/corner, identity controls, mixed and one-store diagnostic; finite artifact verifiers accept. Static maps and drained context transitions only; zero runtime intra-element splits; no closure claim |
| Nonidentity oracle mechanics | Six map/oracle tests plus two production-manager tests pass; 128 seeded topologies/768 context maps; 24 manager lifecycle/version cases and 32 independently enumerated complete-line candidates | Wrong-page/contiguous-PA substitutions, old dirty alias data, D/E ordering, release races and torn/reordered/late versions are checked. Fresh review independently checks 5408 images across 96 update histories; uncheckpointed fallback and long histories remain unqualified |
| Nonidentity adjacent directed checks | 19/19 selected scenarios pass | Runtime-v3, native commands/logs at `build/memblock/alias-manager-20261029/directed-native-v3`; scalar/FP, forwarding/misalignment, full vector/segment, translation/alias/faults, CBO.ZERO, atomic, coherence/release and CMO; selected set, not full catalog |
| Mixed development validation | 2/2 coverage seeds 731..732 pass at 4608 actions; artifact verifier accepts | Frozen pre-PMP runtime; 33/32 heterogeneous windows and 2338/2288 final observed words; `build/memblock/report-namespace-20261028/random`; no closure claim |
| Unresolved-address focused | Corrected runtime-v4: 4/4 seeds 901..904 pass 128 actions each, with 504 windows and 260 correct-data refetches | `build/memblock/scalar-unresolved-20261029/focused-v4`; 244 disjoint-byte controls, all 504 committed store readbacks; zero observed LQ cancellations |
| Unresolved-address controls | Conflict rate 0 and 1000 each pass 2×128 actions | `build/memblock/scalar-unresolved-20261029/{controls-v4,conflicts-v4}`; 252 disjoint controls and 252 matching redirect/return/refetch obligations; finite artifact verifiers accept |
| Unresolved-address mixed/profile | 14 random seeds pass 5120 actions in total; 1092 unresolved windows, 562 correct-data refetches, 530 controls and 1092 DUT store readbacks | Runtime-v4 only; default-timing spec 2×512, compact corner 2×512, coverage 1×1024 and custom mixed 1×1024; latter has 46 heterogeneous windows and 1204/1204 final words. No closure claim |
| Random indexed operands | 4/4 focused seeds 861..864 pass at 128 actions each; repeat endpoints 0/1000 pass 2 seeds each | Frozen runtime `build/memblock/vector-indices-20261029/runtime`; ordinary/segment directions, repeats and descending offsets observed. All tracked final words read back; finite accounting verifiers accept. Overlapping scatter/device ordering remain excluded |
| Indexed mixed development | Custom mixed seed897 passes 1024 actions with 46 heterogeneous windows and 1313/1313 final words | Artifact `build/memblock/vector-indices-20261029/mixed`; 208 indexed instructions, 327 generated repeated offsets, 1172 descents; these operand counts include inactive elements |
| Indexed broad development | Seed891 passes 4608 coverage actions; finite artifact verifier accepts | `build/memblock/vector-indices-20261029/coverage-replay`; 32 heterogeneous windows, 113 dependency windows, 488 indexed instructions and 2325/2325 final words. Exact runtime/seed replay completes in 687 seconds after a 300-second budget timeout; no closure claim |
| Indexed profile compatibility | 2/2 spec and 2/2 corner seeds pass at 512 actions with compact latency; seed892 passes 512 with default spec timing | Artifacts `build/memblock/vector-indices-20261029/{spec,corner,spec-timing}`; seeds871/872 have zero indexed instructions and establish compatibility only. Seed892 executes one indexed load/store pair; corner seeds exercise both ordinary/segment directions. Strict coverage rejects unrelated sparse prefetch/probe bins; no closure claim |
| Ordinary dependency direction | Corrected runtime: 4/4 focused seeds 811..814 pass at 128 actions each; 453 windows | 243 younger-store exclusion controls and 122 multiple-older-store-source loads; final observed/registered words match; `build/memblock/scalar-dependency-20261028/focused-v2` |
| Ordinary mixed development | Corrected generator seed821 passes 4608 actions with `latency=compact`; artifact verifier accepts | 32 heterogeneous windows, 101 dependency windows and 2068/2068 final observed/registered words; `build/memblock/scalar-dependency-20261028/coverage-v2`; no closure claim |
| Refetch/forwarding regression | Corrected runtime-v4: translation-alias, memory-violation and store-forwarding each pass | `build/memblock/scalar-unresolved-20261029/directed-v4`; same-pdest alias refetch and CPU-paired STA/STD lanes; isolated scalar RAW directed coverage still checks source/metadata rather than general refetch arbitration |
| Ordinary-profile development | Corrected runtime: 2/2 spec and 2/2 corner seeds pass at 512 actions each, all with `latency=compact`; artifact verifiers accept | 53/27 dependency windows; short runs have zero heterogeneous windows because of the serial reservation; `build/memblock/scalar-dependency-20261028/{spec-v2,corner-v2}`; no closure claim |
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
| Same-address B/C attribution | B presentation to a PA waits for previous final C, final D and E; accepted C is matched to that one eligible PA expectation | Client C sources are independent of B sources and fixed across data beats; manager fixture and selected coherence/release/atomic/CMO DUT checks pass |
| Accepted-B probe gate | 1/1 1024-action mixed seed passes with 407 probes, plus directed coherence/release/atomic | Seed 207; artifact `agent-campaign-17`; no dedicated counter for pre-B C because an illegal C fails immediately |
| Cross-alias store/load | `translation-alias` passes after-drain/in-flight visibility and identified correct-data frontend refetch; `refetched=1 completed=1 lq=2+0/2` | One PA, two Sv39 VAs; zero-cancellation refetch observed. Nonzero LQ cancellation, arbitrary IQ/LQ replay and redirect arbitration remain unqualified; `build/memblock/scalar-unresolved-20261029/directed-v4/translation-alias.log` |
| CBO.ZERO ordering | Corrected sequence passes on both restored-original and latest-overlay RTL with `sequential_zero=2 consecutive_zero=2 following_store=1`, repeated same-line and three-line consecutive zero cases, both zero/store age directions, a dirty-line zero and an early younger load | CBO.ZERO remains the ROB head until deferred SBuffer completion; no simultaneous CBO completion window is legal or claimed |
| Segment noncanonical fault | Corrected legal two-uop `nf=1` segment load/store cases pass on both restored-original and latest-overlay RTL; the later uop exposes the exact page fault with no DCache/Uncache request or SBuffer side effect | The first data uop may defer the fault and is not an independent architectural oracle |
| Final word readback | observed words equal registered words in every passing mixed case | Cacheable identity path; aliases and CBO.INVAL/errored CMO lines excluded; successful CLEAN/FLUSH retained |
| High-mask driver mutation | Zeroing `src(3)` bytes 2..15 makes `vector-addressing` fail at `phase=high-mask-load`; the restored driver passes | Harness mutation only; it demonstrates directed-test sensitivity, not RTL mutation coverage |
| Vector fault vstart | Two-uop e16/LMUL2 page fault passes with `vstart=8` and exact fault VA. Two single-uop page-crossing faults pass with exact `vstart=1` and exact faulting-element addresses; the all-masked unmapped control passes and observes zero PTW and zero DCache requests; the partially masked load with an inactive unmapped element completes on the mapped element; `translation-faults` PASS log at `agent-campaign-18/translation-faults.log` | Broader fault-element/FOF progress remains open |
| Vector segment store fault side effect | `translation-faults` segment-store cases pass: field-1 fault with exact `vstart=0` and unchanged read-only page; element-1 fault with exact `vstart=1`, exact element-1 field-0 address, and unchanged read-only element page; first-field fault with `vstart=0` and both the read-only field-0 page and following writable field-1 page unchanged; writable earlier fields may be partially written | Three `nf=1` unit-stride segment shapes; other field/element combinations remain open |
| Vector PMP load fault | Single-element and masked no-access-page cases check the fault metadata and no requests. The page-crossing load requires exact element-1 access fault/VA/`vstart=1`; accepted request history through redirect recovery allows only the readable prefix line plus independent prefetch, with no uncache request. Its cold both-readable control checks both data elements and second-page demand. | Bare direct-map; wider shapes and concurrent PMP reconfiguration remain open |
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
uniform fault forbidden-demand checks, arbitrary simultaneous synonym/version
handling and broader C lifecycle/reset combinations, wider same-line CBO.ZERO
forward-timing variants beyond the older-store/younger-zero/younger-load case,
and independent verifier schema/backpressure checks.
The accepted-B-before-C gate, full 128-bit mask operand and explicit
first-access cross-alias redirect/refetch recovery are implemented. General IQ/LQ
replay, concurrent refetch epochs and backend redirect arbitration remain open.
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

Fresh-context non-author review approves canonical randomized FP formats,
independent byte/boxing and older-store expectations, class/destination domains,
strict refetch and schema-59 accounting within the four implemented builders.
The reviewer independently passes six FP and four dependency/recovery tests,
detects private no-boxing/wrong-domain/wrong-class mutations, and reruns the
three focused retained-artifact verifier commands. The low documentation finding
is resolved: adjacent destination descriptions now use the class-appropriate
domains and retain integer auxiliary roles. Only documentation and source
comments changed after the reviewed functional snapshot; disposition and evidence
are `build/memblock/fp-random-20261029/{review.txt,review-disposition.json}`.
No CPU RTL bug is confirmed.

Fresh-context review also approves the PA/alias manager and grouped
release-version oracle within the drained static-map scope. It independently
checks 5408 complete-line candidates across 96 update histories; all findings are
dispositioned in
`build/memblock/alias-manager-20261029/review-disposition.json`.

The structural generator audit finds real local variation in dependency graphs,
vector shapes, indexed operands and page maps, but fixed global scheduling:
five-producer windows precede serial helpers and drain between windows. Canonical
FP formats now vary across ordinary, heterogeneous, dependency and unresolved
loads. Multiple unresolved stores and variable producer counts/ages with sliding
admission are the next normal-workload priorities. The source review and 509/512
FP topology sample do not establish unique complete-run schedules or guarantee a
design bug. Full rename history, exceptional misaligned FP enable semantics,
uncheckpointed current-reference release fallback, pre-notification segment
releases, long version histories, simultaneous synonym/CMO/error ownership and
native-model full source provenance remain explicit limits. Source audit:
`build/memblock/alias-manager-20261029/random-structure-audit.txt`.
