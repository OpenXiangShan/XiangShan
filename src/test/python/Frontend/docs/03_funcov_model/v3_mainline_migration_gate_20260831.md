# V3 859389870 Frontend Coverage Migration Gate

Date: 2026-08-31

## Decision

Coverage backannotation and status promotion are paused until a DUT built from
V3 `859389870811614a2c3d8708c8bd3bfea558d476` has a valid build manifest and a
new signal inventory. Existing HIT rows and artifacts remain immutable
historical evidence; they do not prove the new mainline.

No denominator changes are part of this migration:

- Current canonical four-block strict denominator: `343`, currently
  `290 HIT / 51 MODELED / 1 PARTIAL / 1 BLOCKED` (`84.55% HIT`).
- Historical strict view: `305`, currently `258 HIT / 45 MODELED / 1 PARTIAL /
  1 BLOCKED` (`84.59% HIT`). This view uses only 43 canonical InstrUncache
  leaves and is retained for comparison, not substituted for the current
  343-leaf contract.
- Global denominator: `879`, unchanged.

## Baseline Reconciliation

The read-only V3 reference used for this review is
`/tmp/design-refs-20260831-90yFFZ/v3`, which is clean at
`859389870811614a2c3d8708c8bd3bfea558d476`. The existing design reference
repositories were not fetched, checked out, cleaned, or modified.

The active Verilator build is not the new mainline. Its manifest reports all
three source identities as `7afd6f737d9e3ddc881bda5523e854f07ef5e246`, with
`source_tree_dirty=false` and a 2026-08-25 build timestamp. Commit `7afd6f737`
and contract baseline `c0ca46459` diverge after merge-base `6891f912c`; neither
is an ancestor of the other. `c0ca46459` is an ancestor of `859389870`, while
`7afd6f737` is not. Therefore:

- `RTL_REVIEW:c0ca46459` entries are old semantic review evidence only.
- Existing DUT artifacts are evidence for their manifest source, normally
  `7afd6f737`, not for `859389870`.
- A build from `859389870` is required before any new-mainline DUT claim.
- Generated signal names below are expected from Chisel structure, but must be
  confirmed against the post-build signal inventory before aliases are edited.

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

### Must rerun scenarios on 859389870

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

- Regenerate the build manifest and signal inventory from the 859 build.
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

| BIN | Current canonical status | 859389870 assessment | Required directed evidence |
| --- | --- | --- | --- |
| BIN-908 | `PARTIAL` | State does not improve. The new MainPipe still ORs both WayLookup ITLB exceptions into `s0_hasItlbException` and suppresses req1, while PMP remains a single req0-address check. Second-block ITLB suppression is reachable; independent second-block PMP attribution is still absent. | Drive req1 valid with only block-1 ITLB exception; prove `s0_hasItlbException=1`, `s0_realTwoFetchValid=0`, and new `info(1).valid=0`. Preserve the PMP limitation as review evidence. |
| BIN-909 | `BLOCKED` | Remains blocked. Per-line TL corrupt/denied is reduced into `s1_exceptionOut`, copied into both `info(i).icacheMeta`, and IFU exception delivery still keys off `s1_icacheMeta(0)`. The alignment change does not create second-lane fault attribution. | Re-run second-line-only TL denied/corrupt and ECC cases with stall/flush to document the 859 behavior, but do not claim HIT unless RTL later exposes and preserves precise lane ownership. |
| BIN-1067 | `HIT` on old DUT | Contract remains structurally reachable: wbRedirect still flushes s1 unconditionally, flushes s2 unless `s2_wbNotFlush`, and drives `uncacheUnit.io.flush`. The new checker ownership fields can change which FTQ entry creates the older redirect, so the old HIT does not prove 859. | Use an older cacheable checker redirect A and younger NC B with distinct identities. Cover B in s1, s2, and an exact same-cycle internal `uncacheUnit.req.fire`/flush race; prove flush wins and old B causes no InstrUncache request, TL A, response, or IBuffer delivery; then prove only a new recovery identity completes. No backend redirect. |
| BIN-1084 | `HIT` on old DUT | The first-page permission-fault/no-uncache-request contract remains reachable, but IFU raw-data indexing, range handling, cross-block ownership, and halfRviInfo changed. The old page-tail identity evidence is historical only. | Re-run page-tail 2B PBMT.NC with first-page PF and AF variants. Require `s2_reqIsUncache=1`, `s2_useUncacheFetch=0`, no IfuUncache/InstrUncache/TL A request, correct IBuffer exception, and preserved ftqPtr/ftqOffset/backend FTQ identity. Include stall/flush control cases. |

## Ordered Migration Gate

1. Obtain a clean DUT build whose manifest source, implementation, and design
   baseline all identify `859389870`; reject a locally overridden or divergent
   manifest.
2. Export the new signal inventory. Resolve exact generated aliases for the
   new ICache payload, halfRviInfo, blockSel/isCrossBlockInstr, predecode, and
   shared-depth state before editing bind/samplers.
3. Migrate aliases, SV bind, sampler semantics, fake-DUT unit fixtures, and
   signal-contract tests. Keep negative checks at least as strict as today.
4. Run unit and contract checks. Do not run full DUT regression yet.
5. Run the targeted scenario groups above with explicit target bins and valid
   859 provenance. Review failures before broadening the run set.
6. Only after affected probes and scenarios pass, resume exact-target coverage
   climbing and backannotation. MODELED/PARTIAL or old-build HIT evidence must
   never be promoted as a new-mainline DUT HIT.
