# V3 e5c70547f / frontend-bt 1916b7615 Coverage Migration Gate

Date: 2026-08-31

## Decision

The focused migration gate is complete. Exact-target backannotation and status
promotion may resume only for eligible current-baseline artifacts; historical
HIT rows are not bulk-promoted or treated as revalidation. A clean DUT has now
been built from reviewed
`frontend-bt` descendant `1a32a9056d993233fa1bf3a394b16e8a762abf52`, which
contains integration merge `1916b7615a8d057cbef6862ca94f4c4794f26b8b` and
reviewed V3 design tip `e5c70547f3a966accf20a4b065ec1d8e33443180`.
Existing old-baseline HIT rows and artifacts remain immutable historical
evidence; they do not by themselves prove the current integrated mainline.

No denominator changes are part of this migration:

- Current canonical four-block strict denominator: `343`, currently
  `299 HIT / 42 MODELED / 1 PARTIAL / 1 BLOCKED` (`87.17% HIT`). The four
  blocks are `111/139`, `76/81`, `42/42`, and `70/81` HIT respectively.
- Historical strict view: `305`, currently `266 HIT / 37 MODELED / 1 PARTIAL /
  1 BLOCKED` (`87.21% HIT`). This view uses only 43 canonical InstrUncache
  leaves and is retained for comparison, not substituted for the current
  343-leaf contract.
- Global denominator: `879`, unchanged.

## Baseline Reconciliation

The read-only V3 reference used for the original frontend impact review was
`/tmp/design-refs-20260831-90yFFZ/v3`, which is clean at
`859389870811614a2c3d8708c8bd3bfea558d476`. The existing design reference
repositories were not fetched, checked out, cleaned, or modified.

Subsequent remote reconciliation found V3 design tip
`e5c70547f3a966accf20a4b065ec1d8e33443180` and integrated verification tip
`1916b7615a8d057cbef6862ca94f4c4794f26b8b`. The latter is the merge of the
former into `frontend-bt`, and the local verification branch contains that
integration commit. Compared with `859389870`, the design tip adds
`3e6839bca` (CI container migration) and `e5c70547f` (F-POP L2 prefetcher).
There is no delta under `src/main/scala/xiangshan/frontend`; `MemBlock.scala`
only gains L2-prefetch feedback wiring, with the remaining source changes in
the memory prefetch implementation. The frontend probe and directed-scenario
impact analysis below therefore remains valid.

The active Verilator manifest is now valid for the integrated mainline:

- `dut_source_sha` and `implementation_sha` are `1a32a9056d993233fa1bf3a394b16e8a762abf52`.
- `design_baseline_sha` is `e5c70547f3a966accf20a4b065ec1d8e33443180`.
- The build recorded `source_tree_dirty=false`, no source delta, and no
  manifest rejection reasons.
- The DUT artifact SHA-256 is
  `3269730cddcd752d4e9dff6cad3c7d4f4248ff14de510c66b613f5a271cfa395`.

Therefore:

- `RTL_REVIEW:c0ca46459` entries are old semantic review evidence only.
- Existing old DUT artifacts remain evidence only for their own manifests;
  only newly generated artifacts carrying the identities above can support
  current-mainline claims.
- The post-build signal inventory has been generated and used for the probe
  migration below. Building directly from a design checkout remains an
  unacceptable substitute for the verification integration.

## Migration Execution Checkpoint

- ICache `info(0/1)`, top-level range/maybeRvc, reconstructed MainPipe data,
  explicit half-RVI bundles, PredChecker `blockSel/isCrossBlockInstr`, and
  registered predecode `rasAction` probes have been migrated.
- Focused migration unit/contract validation passed (`149 passed, 68 skipped`)
  before the final PredChecker cross-block correction. The corrected
  PredChecker subset passes (`9 passed`).
- Current-DUT exact targets BIN-1103, BIN-1084, and BIN-1067 pass. BIN-1103
  was rerun after the final sampler revision and its replacement artifact is
  eligible with no provenance rejection. BIN-909's
  second-line-only stimulus also passes functionally but continues to expose
  only the merged exception, so it remains `BLOCKED`.
- A two-fetch backpressure smoke passes (`1 passed`). Initial current-DUT
  cross-block JAL/JALR smoke exposed two stale sampler assumptions: the
  optimized PredChecker jump-offset input alias and old cross-block end-offset
  interpretation. After switching to registered
  `s2_alignedJumpOffsetVec` and the current cross-block representation, both
  exact targets pass (`2 passed`).
- The strict FrontendTrigger sampler and directed test now pass on the current
  DUT (`37 passed` for the complete compact unit file and `1 passed` for the
  exact-target DUT). BIN-996 through BIN-1002 each have one checked hit in
  `ctrl_frontend_trigger_e5c70547_20260831_03`; pytest and checker pass, the
  artifact is eligible, and no FrontendTrigger compare-risk observation is
  present. The sampler follows the RTL's inclusive GE contract (`pc >=
  tdata2`) and verifies the equality boundary.
- BIN-1003 now has an eligible exact-target HIT in
  `ctrl_frontend_trigger_bin1003_e5c70547_20260831_02`: a held triggered
  payload was observed under IBuffer backpressure, and the same PC/FTQ
  identities remained visible while backend redirect asserted `s2_flush` and
  forced `toIBuffer.valid=0`. BIN-1004 remains MODELED. Current Scala connects
  `data` to constant zero
  and `pds` to the registered predecode vector, while FrontendTrigger currently
  compares PC only; unused `data/pds` are consequently optimized out of the
  generated DUT inventory. Source review and signal absence are retained as
  review evidence, not converted into a DUT HIT.

## Impact Classification

### Must migrate probes

1. ICache to IFU response payload

   The old response was a `Vec[MainPipeToIfuReq]`. The new response is one
   `MainPipeToIfuReq` containing top-level `firstRange`, `totalRange`, and one
   fetch-coordinate-aligned `maybeRvcMap`, plus `info(0/1)` records.

   Expected migration, subject to generated-inventory confirmation:

   | Old generated signal | New structure / semantic source |
   | --- | --- |
   | `io_toIfu_req_bits_0_valid` | `io_toIfu_req_bits_info_0_valid` |
   | `io_toIfu_req_bits_1_valid` | `io_toIfu_req_bits_info_1_valid` |
   | `io_toIfu_req_bits_0_startVAddr_*` | `io_toIfu_req_bits_info_0_startVAddr_*` |
   | `io_toIfu_req_bits_[01]_{ftqIdx,size,takenCfiOffset,data,icacheMeta,perf_isCrossLine}` | corresponding `io_toIfu_req_bits_info_[01]_*` |
   | `io_toIfu_req_bits_0_maybeRvcMap` | top-level `io_toIfu_req_bits_maybeRvcMap` |
   | old per-block `range` | top-level `firstRange` and `totalRange` |

   Directly affected code includes `zz_frontend_funcov_bind.sv`,
   `ftq/two_fetch_funcov.py`, cacheable-pipeline tests, and the MainPipe
   miss-response test. The SV bind currently has six direct old-payload
   references; these cannot compile against the new interface.

2. Half-RVI redirect state

   `s1_prevEndHalfRviData` and `s1_prevEndHalfRviPc_*` are replaced by the
   valid bundle `s1_prevEndHalfRviInfo.{valid,bits.data,bits.pc}`. Redirect
   payloads similarly replace `isHalfInstr/halfPc/halfData` with
   `halfRviInfo`. Update the compact sampler, its unit tests, the
   InstrUncache signal contract, and the boundary DUT helper only after exact
   generated names are known. The new `valid` bit is part of the contract;
   zero data/PC is not a valid substitute for an invalid half state.

3. PredChecker block ownership

   `checkerRedirect.bits.selectBlock` is renamed to `blockSel`, and
   `isCrossBlockInstr` is added. For FTQ pointer and instruction-source
   ownership, the effective selector is `blockSel || isCrossBlockInstr`.
   Preserve raw `blockSel` separately for checks that describe the instruction
   start block. A blind rename would misattribute an RVI that starts at the end
   of block 0 and completes in block 1.

4. Predecode timing contract

   `s2_alignedPdInfoVec` still exists in Scala, but it is now a register of
   combinational `s1_alignedPdInfoVec`; predecode and jump-offset generation
   moved from s2 combinational logic to s1 combinational logic. Existing
   `s2_alignedPdInfoVec_*` aliases may survive, but samplers must pair them with
   the registered s2 instruction, PC, valid, flush, PredChecker request, and
   FrontendTrigger transaction. Same-cycle assumptions inherited from the old
   implementation must be removed or explicitly re-proved.

5. Shared prefetch depth

   `BpRunAheadDistance` and `WayLookupSize` are removed. FTQ admission and
   ICache WayLookup both use `FrontendParameters.PrefetchDepth=32`. Source and
   parameter contracts must refer to the shared parameter. Runtime probes
   should continue observing pointer distance, occupancy, ready/valid, flush,
   and wrap rather than relying on a removed parameter symbol.

### Must rerun scenarios on the current integrated mainline

- ICache/IFU maybeRvc alignment: single and dual fetch blocks; same line and
  cross line; SRAM-only, MSHR-only, and mixed SRAM/MSHR returns; invalid req1
  masking; taken truncation; response stall; and flush. Check top-level
  `maybeRvcMap`, `firstRange`, `totalRange`, per-slot index/data, and IBuffer
  output together.
- Cross-block RVI: `isCrossBlockInstr`, invalidTaken, PredChecker second-block
  ownership, FTQ pointer selection, first-fault priority, and halfRviInfo
  redirect/resume. Include JAL and JALR witnesses and a non-CFI RVI stitch.
- Predecode/FrontendTrigger: registered s1-to-s2 instruction/PC/predecode
  coherence under normal flow, stall, flush, illegal RVC, and trigger match.
- FTQ/WayLookup: shared depth boundaries 31 and 32, full backpressure and
  recovery, single/dual write wrap, read wrap, global/BPU flush, and two-write
  atomicity. Existing model tests cover much of this shape, but their DUT
  evidence predates the shared-parameter RTL.
- BPU override and BTB comparison: both s1 and s3 all-not-taken must not assert
  `s3_override`; MBTB cfi/attribute changes matter only for a taken s3 result;
  BTB target differences only above the storable lower target must not
  override; lower-bit or enabled carry differences must override; ITTAGE and
  RAS retain full-target comparison. The existing generic
  `RFR_bpu_s3_override_cp` is positive-only and is insufficient for these
  negative contracts.
- CSR-change/PTW: a CSR change with an old translation/PTW request in flight,
  aligned frontend/MemBlock flush observation, rejection of the old epoch,
  no PTW-filter in-flight overflow, and clean refetch. Keep the sfence path as
  a separate unchanged-latency control.
- All exact-target tests used to revalidate affected historical HITs, including
  BIN-1067 and BIN-1084 below.

### Provenance-only updates

- Regenerate the build manifest and signal inventory from the integrated
  `frontend-bt` build.
- Update active contract/tool baseline declarations only after the RTL review
  is accepted. Do not rewrite old `RTL_REVIEW:c0ca46459` evidence strings or old
  artifact manifests; their old identities are the audit trail.
- Record V2 `5d3fe8d1e4d319693e8a27c0299be3a23f2e41af` as reviewed with no frontend
  Scala delta. It does not require this V3 probe migration or the V3 targeted
  rerun set.

### No direct impact for now

- InstrUncache TileLink A/D protocol fields and the BIN-1103 redirected-wait-D
  episode are not changed by the listed V3 commits. BIN-1103 has a clean exact
  hit on the old DUT, but promotion remains paused and the scenario must be
  rerun after the new manifest/contract gate because the surrounding IFU flush
  path is part of the observed episode.
- The fixed coverage denominators and existing CSV status values are not
  migration inputs. They remain unchanged while evidence is historical.
- The V3 reference checkout and existing V2/V3 design reference repositories
  are read-only inputs, not migration worktrees.

## Focus BIN Assessment

| BIN | Current canonical status | e5c70547f / 1916b7615 assessment | Required directed evidence |
| --- | --- | --- | --- |
| BIN-908 | `PARTIAL` | State does not improve. MainPipe still ORs both WayLookup ITLB exceptions into `s0_hasItlbException` and suppresses req1, while PMP remains a single req0-address check. A legal natural second-only ITLB stimulus has not been found: paired prefetches share one iTLB result and must be on one virtual page; WayLookup stores only the write-head exception; FTQ disables req1 across pages and for either matching backend-fault entry. The defensive MainPipe state is unit-observable, but that is not natural DUT evidence. | Keep the unit contract and review evidence. Do not promote unless design review identifies a legal producer sequence that drives req1 with only block-1 ITLB exception and proves `s0_hasItlbException=1`, `s0_realTwoFetchValid=0`, and new `info(1).valid=0`. Preserve the absent per-block PMP interface limitation. |
| BIN-909 | `BLOCKED` | Remains blocked. Per-line TL corrupt/denied is reduced into `s1_exceptionOut`, copied into both `info(i).icacheMeta`, and IFU exception delivery still keys off `s1_icacheMeta(0)`. The alignment change does not create second-lane fault attribution. | Re-run second-line-only TL denied/corrupt and ECC cases with stall/flush to document the current integrated behavior, but do not claim HIT unless RTL later exposes and preserves precise lane ownership. |
| BIN-1067 | `HIT` on old DUT | Contract remains structurally reachable: wbRedirect still flushes s1 unconditionally, flushes s2 unless `s2_wbNotFlush`, and drives `uncacheUnit.io.flush`. The new checker ownership fields can change which FTQ entry creates the older redirect, so the old HIT does not prove the current integration. | Use an older cacheable checker redirect A and younger NC B with distinct identities. Cover B in s1, s2, and an exact same-cycle internal `uncacheUnit.req.fire`/flush race; prove flush wins and old B causes no InstrUncache request, TL A, response, or IBuffer delivery; then prove only a new recovery identity completes. No backend redirect. |
| BIN-1084 | `HIT` on old DUT | The first-page permission-fault/no-uncache-request contract remains reachable, but IFU raw-data indexing, range handling, cross-block ownership, and halfRviInfo changed. The old page-tail identity evidence is historical only. | Re-run page-tail 2B PBMT.NC with first-page PF and AF variants. Require `s2_reqIsUncache=1`, `s2_useUncacheFetch=0`, no IfuUncache/InstrUncache/TL A request, correct IBuffer exception, and preserved ftqPtr/ftqOffset/backend FTQ identity. Include stall/flush control cases. |

## Ordered Migration Gate

1. **Complete.** Obtain a clean DUT build from the current reviewed `frontend-bt` integration
   commit or a reviewed descendant. Both `dut_source_sha` and
   `implementation_sha` must identify that integration commit (or descendant),
   and Git ancestry must prove that it contains reviewed design tip
   `e5c70547f`. A build checked out exactly at the merge point has both fields
   equal to `1916b7615`; a build at a reviewed descendant must record that exact
   descendant instead. Preferably record `design_baseline_sha=e5c70547f`
   explicitly via `FRONTEND_DESIGN_BASELINE_SHA`. If the build helper leaves
   the design field equal to the integration source by default, verify the
   design-tip ancestry separately. Do not require all three manifest fields to
   equal a design-branch SHA, and reject a locally overridden, divergent, or
   dirty manifest.
2. **Complete.** Export the new signal inventory. Resolve exact generated aliases for the
   new ICache payload, halfRviInfo, blockSel/isCrossBlockInstr, predecode, and
   shared-depth state before editing bind/samplers.
3. **Complete for the affected implemented samplers.** Migrate aliases, SV bind, sampler semantics, fake-DUT unit fixtures, and
   signal-contract tests. Keep negative checks at least as strict as today.
4. **Complete for the focused migration set.** Run unit and contract checks. Do not run full DUT regression yet.
5. **Complete for the focused migration gate.** Run the implemented affected
   scenario groups with explicit target bins and valid current integration
   provenance. Remaining unclosed leaves retain their documented MODELED,
   PARTIAL, or BLOCKED status and require their own exact-target evidence.
6. **Active.** Resume exact-target coverage climbing and backannotation using
   only artifacts accepted by the evidence gate. MODELED/PARTIAL or old-build
   HIT evidence must never be promoted as a new-mainline DUT HIT.
