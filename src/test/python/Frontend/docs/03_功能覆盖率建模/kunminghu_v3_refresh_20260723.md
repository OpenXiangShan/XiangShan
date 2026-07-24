# kunminghu-v3 Frontend coverage refresh audit (2026-07-23)

## 1. Scope and frozen revisions

This file is a design-refresh audit note, not a second coverage registry. The only
canonical bin definitions remain in
`frontend_bt_functional_coverage_pilot.csv`, and the only canonical testpoint
status/evidence table remains
`../02_测试点分解/Frontend_testpoint_0525_coverage_backannotated.csv`.
Do not copy this matrix back as a whole-table rewrite. Apply only reviewed leaf
updates after the signal mapping, sampler, checker, and testcase changes are
complete.

- Remote `kunminghu-v3` observed at refresh start:
  `06f4a74041023799b9ed32c0447c7d55c762f999`.
- Frozen local design baseline, including #6221 and #6219:
  `29c99bba49cd8d0d086ee5bebe631e75a1378136`.
- #6221 commit: `2b5769e8b2e3bb0fe63dc8294b9f5db70988567d`.
- #6219 commit: `79ce25ebee89ca0a0db6ed029aaa303fbaaf9269`.
- Refresh implementation SHA: `PENDING_FINAL_COMMIT`.

The two local documentation-only commits after the frozen design baseline do not
change the design SHA. They must not be used as a substitute for the manifest's
`dut_source_sha`.

## 2. Merged design changes

### #6221: ICache two-fetch retiming

#6221 changes the ownership of the fetch handshake and the final two-fetch
decision. FTQ now sends `toMainPipe` directly, MainPipe combines the FTQ request
with WayLookup metadata, and MainPipe returns `realTwoFetchValid` and the
prioritized failure reason to FTQ. `bankSel` and `isCrossLine` are stored in each
WayLookup entry instead of travelling on the FTQ-to-WayLookup request.

Affected design files are:

- `frontend/Frontend.scala`, `frontend/TwoFetch.scala`, and
  `frontend/Bundles.scala`;
- `frontend/ftq/Bundles.scala` and `frontend/ftq/Ftq.scala`;
- `frontend/icache/Bundles.scala`, `ICacheImp.scala`,
  `ICacheMainPipe.scala`, `ICacheWayLookup.scala`,
  `ICachePrefetchPipe.scala`, and `ICacheDataArray.scala`.

### #6219: ICache miss response and IFU compact retiming

#6219 adds a register on the ICache miss-response data/valid path, moves IFU
compaction and `invalidTaken` work from s0 to s1, removes range reduction from
`InstrBoundary`, and feeds PredChecker with the aligned raw instruction metadata
instead of the expanded-instruction bundle. It also changes the bookkeeping used
to restore a previous half RVI on cache/uncache boundaries.

Affected design files are:

- `frontend/icache/ICacheMainPipe.scala`;
- `frontend/ifu/Helpers.scala`, `Ifu.scala`, `InstrBoundary.scala`, and
  `PredChecker.scala`.

## 3. Required signal remapping

The current `test_two_fetch_signal_map_matches_current_frontend_offset` check
fails against the generated baseline inventory. The following mappings are
required before DUT evidence can be accepted.

| Old sampler concept/path | Baseline signal or required observation | Rule |
| --- | --- | --- |
| `wayLookup.io_fromFtq_valid` | `mainPipe.io_fromFtq_valid` or top-level `__Vtogcov__io_fromFtq_toMainPipe_valid` | Direct rename. |
| `wayLookup.io_fromFtq_ready` | `mainPipe.io_fromFtq_ready` or top-level `__Vtogcov__io_fromFtq_toMainPipe_ready` | Direct rename; this is now the accepted FTQ transaction. |
| `wayLookup.io_fromFtq_bits_req_1_valid` | `mainPipe.io_fromFtq_bits_req_1_valid` or top-level `__Vtogcov__io_fromFtq_toMainPipe_bits_req_1_valid` | Direct rename for FTQ eligibility only. Do not use it as the final service width. |
| `io_fromFtq_toWayLookup_bits_req_*_takenCfiOffset_bits` | `__Vtogcov__io_fromFtq_toMainPipe_bits_req_*_takenCfiOffset_bits` | Direct rename. Candidate start PCs still require `fetchPtr`-indexed FTQ entries because no equivalent direct start-PC probe is registered. |
| `io_fromFtq_toWayLookup_bits_req_0_hasBackendException` | `__Vtogcov__io_fromFtq_toMainPipe_bits_req_0_hasBackendException` | Direct rename, but req1 exception is not observable on this interface; use the FTQ exception pointer/state for either-entry coverage. |
| `io_toFtq_fromWayLookup_realTwoFetchValid` | `__Vtogcov__io_toFtq_fromMainPipe_realTwoFetchValid` | Direct rename. Correlate it with the same MainPipe fire that advances `fetchPtr`. |
| WayLookup `isDataSramReadConflict_*` | MainPipe `s0_dataSramReadConflict` semantics | No registered direct signal exists in the current inventory. Expose a stable contract signal or strictly reconstruct all four cross-request line pairs from active WayLookup entries. |
| WayLookup-derived no-meta/MMIO/ITLB reason | MainPipe `TwoFetchFailReason` plus `wayLookupInfo(1).valid` | Prefer the returned prioritized reason. The enum is not currently registered in the signal inventory, so observability must be added or proven by complete aligned inputs. |
| IFU `s0_invalidTaken_0` | IFU `s1_invalidTaken_0` | Stage rename plus one-cycle transaction correlation. |
| IFU `s0_fixedFetchBlock_1_valid` | No equivalent signal | Semantic change. #6219 keeps the second fetch-block metadata and trims `s1_instrCount`/aligned valid masks; BIN-532 must check that no second-block instruction is enabled or delivered. |

The four MainPipe fallback reasons are `NoMeta`, `DataConflict`, `HasMmio`, and
`HasItlbException`, corresponding to BIN-515 through BIN-518. They must be
sampled at the MainPipe acceptance point. Reading an uncorrelated WayLookup queue
entry after the transaction is insufficient.

## 4. Design-to-coverage impact matrix

| Design file and behavior | Affected TP/BIN leaves | Sampler/checker impact | Existing testcase and required extension |
| --- | --- | --- | --- |
| `Ftq.scala`: `rawTwoFetchValid`, direct `toMainPipe` fire, `fetchPtr +1/+2`, BPU s3 rollback | FTQ request eligibility and pointer/flush leaves, BIN-501..509 | Remap the handshake; separately observe all four FTQ blockers (runahead, size, cross-page, either-entry backend exception). Bind pointer before/after and `realTwoFetchValid` to one transaction; clear pending state on BPU/backend flush. | Extend `fe_2fetch_trained_short_blocks.S`, `fe_2fetch_size_blocked.S`, and `fe_2fetch_cross_page_blocked.S`. No real case currently implements blocked-runahead, either-entry backend exception, deterministic single advance, wrap assertion, or BPU-s3 competition. |
| `ICachePrefetchPipe.scala`, `WayLookupEntry`: `takenCfiOffset`, stored `bankSel`/`isCrossLine`, two-prefetch layout | two-prefetch layout leaves, BIN-510..513 | Existing sampler only reads the layout enum. Revalidate addresses, cross-line flags, stored metadata, and both write ports on the accepted transaction. | Reuse the address layouts in the four existing 2-fetch assembly streams where possible. Registry testcase names for same-line/overlap1/overlap2/interleave do not currently resolve to real files or Python tests. |
| `ICacheMainPipe.scala` and `ICacheWayLookup.scala`: combined FTQ/meta acceptance and four fallback reasons | WayLookup/MainPipe service-width leaves, BIN-514..519 | Replace old WayLookup fire with MainPipe fire. `dual_served` requires req1 candidate, second metadata, final `realTwoFetchValid`, and req1 data request in the same transaction. Each single fallback requires one observed reason; absence of another signal is not evidence. | `fe_2fetch_trained_short_blocks.S` can retain the dual-served path. Extend current ICache/Frontend directed infrastructure for metadata shortage, bank conflict, MMIO, and ITLB exception; the registry `tc_2fetch_waylookup_*` names are not implemented. |
| `ICacheMainPipe.scala`: registered miss response and held s1 request | dual hit/miss and refill completion leaves, BIN-520..524 | Sample the initial four-way hit/miss classification once per `(ftqIdx0, ftqIdx1)` transaction. Track required lines, miss requests, registered refill acceptance, `s1_fetchFinish`, stall, flush, and final dual `toIfu.fire`. The current global `_two_fetch_waiting_refill` boolean can associate a later unrelated dual response and can mark multiple hit patterns as one request changes state. | Keep hit-hit in `fe_2fetch_trained_short_blocks.S`. Extend the existing configurable `ICacheAgent` directed tests for hit-miss, miss-hit, miss-miss, delayed refill, stall, and flush. Registry `tc_2fetch_mainpipe_*` cases are not implemented. |
| `Ifu.scala`: dual window, compaction, aligned valid/PC/FTQ source | IFU dual-window/source/delivery leaves, BIN-525..528 and BIN-537..539 | Check the exact `blockSel` transition at `req0.size`, aligned valid/enq masks, PC progression, and both FTQ pointers. A single nonzero `blockSel` or a cfVec observed within eight cycles is not sufficient transaction correlation. | Extend `fe_2fetch_trained_short_blocks.S` and `fe_2fetch_mixed_rvc_rvi.S` with exact payload/source assertions. Reuse `test_baremode_backend_short_backpressure_pilot` or `test_baremode_queue_near_full_backpressure_pilot` to hold a known dual payload. |
| `InstrBoundary.scala` and `Ifu.scala`: no range reduction, RVC/RVI boundaries, cross-block RVI stitch | taken separation and RVC/RVI boundary leaves, BIN-527 and BIN-529..531 | For RVI stitch, check data composition, first-block PC, one source transition, and absence of duplicate second-block halfword. For RVC/RVI mixing, check every valid/enabled slot and PC step, not merely one `isCrossBlockInstr` bit or a mixed cfVec pair. | Keep `fe_2fetch_mixed_rvc_rvi.S` for mixed widths and use the executable `fe_2fetch_rvi_cross_block.S` owner for BIN-529; its three pinned sites span both 32-byte FTQ alignment boundaries and the DefaultConfig 64-byte fetch boundary. |
| `Ifu.scala`: `invalidTaken` moved to s1 and first-block clipping changed | first-block invalidTaken leaf, BIN-532 | Sample `s1_invalidTaken_0` with the matching dual request, then prove the aligned/enabled valid range contains no second-block instruction and that redirect/half-RVI recovery is correct. The deleted `s0_fixedFetchBlock_1_valid == 0` checkpoint is invalid. | Extend the mixed/boundary stream or an existing CFI directed test. `fe_2fetch_first_invalid_taken` is not implemented. |
| `PredChecker.scala`: aligned raw `instrVec`, redirect selection and earliest fault | second invalidTaken and checker leaves, BIN-533..536 and BIN-541 | Retain redirect valid/invalidTaken/selectBlock sampling, but correlate it by FTQ source and verify target/endOffset plus the full `fixedInstrValid` mask. BIN-534 requires at least two observed faults; selectBlock alone must not imply priority. | Extend existing CFI/predecode directed infrastructure. `fe_2fetch_second_invalid_taken`, `fe_2fetch_first_fault_priority`, and `fe_2fetch_second_fault_after_first` are not implemented. |
| `Ifu.scala`: s2 hold, IBuffer handshake, backend redirect priority | dual delivery/backpressure/redirect leaves, BIN-537..540 | During `ready=0`, snapshot and compare valid, instructions, enable mask, PC, FTQ pointers, RVC flags, and end offsets on every cycle. On recovery, require exactly one fire. On backend/BPU redirect, require the old transaction never fires and the new target is the first accepted path. The current BIN-539 predicate records only one stalled cycle; BIN-540 uses an unkeyed recent-dual window. | Extend the existing backend backpressure and redirect pilots around a trained dual-block stream. Add miss-response-at-redirect timing to the same tests. `fe_2fetch_backend_redirect_flush` is not implemented. |
| `Ifu.scala`: `s2_prevEndIsHalfRvi`, `uncacheRedirect`, `uncachePc`, half-data recovery | uncache page-tail leaves, BIN-416/417 | Existing cfVec/request sampling proves the external result but not the retimed recovery state. Correlate first-page low half, `needResend`, next-page request, saved half PC/data, assembled RVI, and state clear; reject duplicate requests or delivery. | Extend `test_uncache_page_tail_rvi_need_resend_rechecks_next_page` and keep `test_uncache_page_tail_rvc_does_not_fetch_next_page_before_delivery` as the negative pair. These are real DUT tests and should remain the owners of BIN-416/417. |

The corresponding canonical TP paths (look them up in the pilot CSV rather than
duplicating their leaf text here) are:

- BIN-416/417: `地址翻译与权限检查/跨页取指/页尾RVC` and
  `地址翻译与权限检查/跨页取指/页尾RVI`.
- BIN-501..507: `2-fetch双取指/FTQ双fetch请求生成/资格判定/rawTwoFetchValid条件组合`;
  BIN-508/509 use the sibling `指针与flush/fetchPtr推进和旧路径清理` path.
- BIN-510..513: `2-fetch双取指/2-prefetch与WayLookup准备/TwoPrefetchCase地址布局/双块meta读取映射`.
- BIN-514..519: `2-fetch双取指/2-prefetch与WayLookup准备/WayLookup实际双取指宽度/realTwoFetchValid门控`.
- BIN-520..524: `2-fetch双取指/ICache MainPipe双块数据返回/双请求hit与refill组合/两块完成后统一交付IFU`.
- BIN-525..528 and BIN-537: `2-fetch双取指/IFU双块合并与预译码/IfuData双块窗口/range与来源映射`.
- BIN-527 and BIN-529..531: the sibling `InstrBoundary跨块边界/RVC_RVI组合` path under the same IFU branch.
- BIN-532: `2-fetch双取指/IFU双块合并与预译码/InstrBoundary跨块边界/RVC_RVI组合`.
- BIN-533..536 and BIN-541: `2-fetch双取指/PredChecker双块修正/错误位置与selectBlock/首个错误裁剪`.
- BIN-538..540: `2-fetch双取指/双块交付与清理/IBuffer握手和redirect/在途双块生命周期`.

## 5. Testcase inventory and assertion gaps

The repository currently has four 2-fetch assembly streams:

- `fe_2fetch_trained_short_blocks.S`;
- `fe_2fetch_size_blocked.S`;
- `fe_2fetch_cross_page_blocked.S`;
- `fe_2fetch_mixed_rvc_rvi.S`.

`test_two_fetch_functional_coverage.py` exercises the sampler with a fake DUT.
Those tests are model/contract checks, not real DUT evidence. In particular, the
current fake-DUT tests do not prove payload stability, transaction identity,
refill timing, flush suppression, exact boundary data, or redirect target
correctness.

Prefer extending these existing long-lived tests before adding a new test:

- `test_baremode_backend_short_backpressure_pilot` and
  `test_baremode_queue_near_full_backpressure_pilot` for BIN-539;
- the existing backend redirect pilots for BIN-540 and redirect/miss-response
  competition;
- `test_uncache_page_tail_rvi_need_resend_rechecks_next_page` for BIN-417;
- `test_uncache_page_tail_rvc_does_not_fetch_next_page_before_delivery` for the
  BIN-416 negative comparison.

A new testcase is justified only where these owners cannot encode a stable
semantic contract, such as a deterministic WayLookup bank conflict or a precise
first/second-block PredChecker fault ordering.

## 6. Evidence invalidation and refresh gates

All pre-refresh evidence touching BIN-501..541 or BIN-416/417 is diagnostic
until it is reproduced with the final mapping/checker implementation and a clean
DUT build. An artifact remains `PARTIAL`, even if it contains a historical hit,
when any of the following is true:

- `dut_source_sha` is not the frozen design baseline;
- `source_tree_dirty` is not exactly `false`;
- DUT, Python extension, generated RTL, signal-contract, registry, sampler, or
  build-manifest hash is missing or fails runtime revalidation;
- pytest fails, exits nonzero, or has missing outcome metadata;
- monitor, checker, assertion, or golden/reference comparison reports an error;
- waveform, raw `.dat`, case log, funcov, provenance, testcase input, binary, or
  golden trace is missing, empty where disallowed, outside the run root, or hash
  mismatched;
- the evidence was produced with a different DUT build or compatibility
  signature.

Do not merge funcov JSON or code-coverage `.dat` across DUT builds. Run the
registry/mapping/schema/model tests, full signal-contract tests, affected
#6219/#6221 directed tests, all 64 active bins, independent funcov and codecov
generation, and strict back-annotation over the entire active registry in that
order. `CLOSED` remains a manual review state only.

## 7. Unmerged PR watchlist

The following PRs were not part of the frozen baseline. They are watchlist items
only; do not change the canonical baseline, registry denominator, or accepted
evidence for them before merge.

| PR | Watch item | Refresh trigger after merge |
| --- | --- | --- |
| #6253 | `takenCfiOffset` becomes `taken/endPosition` | Revisit BIN-503, BIN-510..514, and IFU/PredChecker BIN-525..536/541; rebuild signal mappings and directed boundary cases. |
| #6220 | IFU/ICache alignment, predecode, and `maybeRvcMap` refactor | Revisit BIN-520..541 and BIN-416/417, especially RVC/RVI stitch, hit/refill data, invalidTaken, and exception alignment. |
| #6263 | PHR s1/s3 delayed write and redirect priority | Revisit BPU lookahead/flush timing for BIN-501/502/509/540 and add the new redirect race to the directed refresh. |
| #6237 | uBTB resolve plus fast train | Revisit trained dual-fetch availability, runahead, BPU-s3 override, and redirect behavior for BIN-501/502/509/540. |
| #6207 | second fetch-block `exceptionMask` | Revisit second-block source, clipping, PredChecker, and delivery leaves BIN-525..541, plus exception/flush competition. |
| #6187 | SC `writeValidVec` | Revisit predictor training stability and any resulting dual-fetch/redirect timing for BIN-501/502/509/540. |

For each merged watchlist item, record the new design SHA, build a new clean DUT,
refresh the affected signal contract and leaves, and generate a new non-merged
evidence set.

## 8. Closure execution result (2026-07-24)

This closure touched verification assets only. No `src/main/scala` or generated
design source was edited. Relative to the frozen local design SHA
`29c99bba49cd8d0d086ee5bebe631e75a1378136`, the final implementation HEAD has no
design-source diff.

### 8.1 Final revisions and DUT manifest

- Final verification implementation SHA:
  `681ed1d9119c8cb5d22441b5dfc388bad8a2072e`.
- Frozen DUT source SHA:
  `29c99bba49cd8d0d086ee5bebe631e75a1378136`.
- Remote kunminghu-v3 reference SHA:
  `06f4a74041023799b9ed32c0447c7d55c762f999`.
- Build command:
  `make frontend CONFIG=DefaultConfig ISSUE=E.b NUM_CORES=1 CHISEL_TARGET=systemverilog FRONTEND_WAVEFORM_FORMAT=fst`.
- Required environment activation was:
  `deactivate 2>/dev/null || true; source /nfs/share/unitychip/activate`.

`build-frontend/frontend_build_manifest.json` runtime validation is valid:

| Field | Value |
| --- | --- |
| `dut_source_sha` | `29c99bba49cd8d0d086ee5bebe631e75a1378136` |
| `implementation_sha` | `681ed1d9119c8cb5d22441b5dfc388bad8a2072e` |
| `design_baseline_sha` | `06f4a74041023799b9ed32c0447c7d55c762f999` |
| `source_tree_dirty` | `false` |
| `dut_build_sha256` | `7e78bf59c8540806f896d8afec38c9bd325ecc559b380742edc9f04482b1b29c` |
| `dut_python_extension_sha256` | `edde3e89475de8ffd6ad38ef25601f16c50ac9b6dcd3c179d5184fd25b4bd1d9` |
| `generated_rtl_sha256` | `b53d2e11575b9d96d5bb67941c9453f9e3472af8d424001100fa813bb9e78528` |
| `signal_contract_sha256` | `229aee1975415243bfc8deb3847e2fdf58c5a2134fb24bb959cc893132c0fc7d` |

### 8.2 Verification-side implementation changes

Canonical assets were updated in place; no second registry or sampler was
introduced.

- `env/funcov.py`
  - remapped two-fetch final service width to
    `fromMainPipe.realTwoFetchValid`;
  - removed nonexistent `s0_twoFetchFailReason` and old
    `WayLookup.io_fromFtq_*` assumptions;
  - reconstructed observable MainPipe fallback reasons only where current RTL
    signals provide a legal same-transaction relation;
  - moved `invalidTaken` sampling to current s1/rawInstrVec semantics;
  - made pointer-advance bins require a real two-fetch candidate instead of
    inferring step-one from ordinary single fetches;
  - fixed backend-redirect in-flight tag association and allowed the recovery
    path to be single-width while still proving old dual tags were dropped;
  - kept unobservable conditions diagnostic instead of using default zero.
- `env/functional_coverage.py`
  - kept IBuffer payload-stability observations as diagnostics; checker/testcase
    assertions remain responsible for pass/fail, while refill/redirect ownership
    mismatches still block HIT.
- `tests/`
  - extended model, signal-contract, IBuffer backpressure, backend redirect,
    BPU s3, ICache refill/fault/flush, and uncache prev-half directed checks.
- Canonical CSVs:
  - testpoint wording for #6219/#6221 was refreshed earlier for BIN-416/417 and
    BIN-501..541;
  - this run strictly backannotated the entire active registry, not just BIN-5xx.

### 8.3 Tests and regressions

| Step | Result |
| --- | --- |
| Registry/mapping/schema/model unit tests | `148 passed` |
| Signal-contract tests | `6 passed` |
| #6219/#6221 affected directed, single run diagnostic | `10 passed, 1 skipped, 2 failed` |
| #6219/#6221 affected directed, unique run_id per item | `11 passed, 1 skipped, 2 failed` |
| Active asm/bin-trace suite | `4 passed, 5 failed` after resolving `NEMU_EXEC` to `/nfs/home/jiabowen/ai_workspace/NEMU/build/riscv64-nemu-interpreter` |
| Additional active uncache tests | `5 passed, 1 skipped` |
| Strict full-registry backannotation | `hit=28, partial=16, model=19, failed=16, closed_preserved=0`; one BLOCKED row remains BLOCKED |
| Strict codecov over eligible passing `.dat` only | Passed, 19 unique run IDs |
| Strict codecov over all active `.dat` | Correctly rejected failed sidecar evidence |

The main artifact roots are:

- `src/test/python/Frontend/data/runs/frontend_refresh_20260724_active64_directed_*_681ed1d91/`
- `src/test/python/Frontend/data/runs/frontend_refresh_20260724_active64_uncache_*_681ed1d91/`
- `src/test/python/Frontend/data/runs/frontend_refresh_20260724_active64_asm2_681ed1d91_*/`
- `src/test/python/Frontend/data/runs/frontend_refresh_20260724_active64_report_681ed1d91/`

### 8.4 Current active registry status

After applying the strict backannotation to
`../02_测试点分解/Frontend_testpoint_0525_coverage_backannotated.csv`, the 64
active bins are:

| Status | Count |
| --- | ---: |
| HIT | 28 |
| PARTIAL | 16 |
| MODELED | 19 |
| BLOCKED | 1 |
| CLOSED | 0 |

Strict functional coverage is therefore `28 / 64` for automatic HIT closure.
Observed diagnostic funcov, including failed or otherwise rejected artifacts, saw
`43 / 64` active bins; those extra hits were not allowed to upgrade status.

New current-version HIT evidence includes:

- BIN-401..415 from the four passing IFU asm/bin-trace cases;
- BIN-416/417 and BIN-418..421/423 from uncache directed tests;
- BIN-520/521/523/524 from ICache MainPipe hit/refill directed tests;
- BIN-538/539 from IBuffer backpressure and dual delivery tests.

Current-version rejected or partial evidence includes:

- BIN-509: BPU s3 competition testcase failed and the target bin was not hit.
- BIN-529: RVI cross-block asm/bin-trace failed before a valid closure.
- BIN-540: backend redirect testcase hit the target bin, but monitor/checker
  failed, so it remains PARTIAL.
- Several two-fetch asm bins were observed in failed bin-trace artifacts and
  remain PARTIAL until the checker/backend recovery issue is resolved.

### 8.5 Code coverage

Code coverage was generated from 19 passing, provenance-clean `.dat` files with
unique run IDs. Failed or skipped test `.dat` files were not merged.

| Kind | Hit | Total | Percent |
| --- | ---: | ---: | ---: |
| line | 2696 | 26042 | 10.35% |
| branch | 14513 | 30302 | 47.89% |
| expr | 21497 | 44249 | 48.58% |
| toggle | 314812 | 954708 | 32.97% |

The strict all-active codecov attempt rejected the failed backend-redirect
sidecar, proving that failed artifacts were not mixed into the accepted code
coverage denominator.

### 8.6 Remaining blockers and bugs

- Backend redirect recovery mismatch:
  `test_backend_redirect_drops_dual_miss_and_ignores_delayed_old_response`
  drives target `0x80000100`; the coverage sampler now observes BIN-540, but
  the monitor reports `REDIRECT_RECOVERY_TARGET_MISMATCH` at cycle 694
  (`expected=0x80000100`, `actual=0x80000000`). This blocks HIT.
- BPU s3 redirect competition:
  `test_bpu_s3_override_drops_stalled_dual_request_before_mainpipe_fire` did not
  find a rollback-eligible stalled dual FTQ collision in 4000 cycles. It also
  reports repeated `PC_MISMATCH` errors (`expected=0x800001c0`,
  `actual=0x80000180`) at cycles 945, 1713, 2993, 3761, and 4529.
- Five two-fetch asm/bin-trace cases fail on the backend model's
  `golden_first_mismatch_redirect` recovery assertion. Example failures include
  `actual_pc=0x80001110 target_pc=0x80000fe0` and
  `actual_pc=0x80000160 target_pc=0x80000020`. These artifacts remain
  diagnostic only.
- BIN-422 remains BLOCKED because the current generated DUT does not expose
  `io_tlbCsr_mPBMTE`.
- `rawInstrVec[31].isCrossBlockInstr` is still missing from the current signal
  inventory; bins that require this exact flag cannot use a default-zero hit.
- MainPipe/WayLookup data-bank conflict and some prioritized failure reasons
  still lack a complete same-s0 observable relation. BIN-515..518 remain modeled
  unless additional observability or deterministic legal stimulus is added.

No `CLOSED` status was written by automation.
