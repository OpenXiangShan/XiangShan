# 2026 MemBlock History Audit

## Scope and Evidence Rules

The initial audit covered commits returned by:

```sh
git log origin/kunminghu-v2 --since=2026-01-01 -- \
  src/main/scala/xiangshan/mem
```

The initial snapshot used baseline
`0fa7bb8259a7922481289d8d5932797afce84030` and classified 58 commits through
2026-09-01. This is an initial-audit count, not a permanent bound. On
2026-10-21 the same query at `origin/kunminghu-v2` revision `fe2288cb0936`
returned 75 commits; selected later fixes are classified below through bounded
reviews rather than a complete rewrite. The status vocabulary is deliberately
strict:

- **Reproduced**: an independently generated RTL mutation that reverts the fix
  passes on clean RTL and fails the same test on the mutant.
- **Covered, not mutated**: a current test directly exercises the affected
  architectural/protocol behavior, but no independent revert was built.
- **Covered, mutation did not fail**: a revert was built and the most relevant
  current test still passed. This is not sensitivity evidence.
- **Stimulus gap**: the boundary models the required producer and observation,
  but no current test demonstrates the discriminating combination or overlap.
- **Boundary gap**: the required legal producer, consumer, or architectural
  observation is not modeled at the current MemBlock boundary.
- **Non-functional/outside MemBlock UT**: performance attribution, X-only,
  trace, reset plumbing, feature/refactor, or an effect without a stable
  architectural oracle at this boundary.

Structural pin stimulation is never used as evidence of semantic coverage.

### Bounded Review Workflow

A bounded no-progress review is a focused slice of this audit, not a rewrite of
the complete table. Its input snapshot must identify the current UT revision,
the `origin/kunminghu-v2` revision, the three to five selected fixes or one
coherent bug theme, the reviewed files, and the existing test/artifact paths.

For each selected fix, record:

- the fixed behavior and the violated architectural or external-interface
  contract;
- the legal producer and stimulus dimensions needed at the MemBlock boundary;
- the independent oracle and observable completion/protocol effect;
- whether the current UT is `Reproduced`, `Covered, not mutated`,
  `Covered, mutation did not fail`, `Stimulus gap`, `Boundary gap`, or
  `Non-functional/outside MemBlock UT`;
- the exact test, mutation, or evidence path, plus the missing coverage point
  when sensitivity is absent.

Use the current generated RTL and harness for a mutation experiment unless the
task explicitly asks for historical replay. Keep mutated RTL, Picker exports,
models, and runtimes in a private worktree or output tree so the canonical build
and any active frozen-runtime campaign remain unchanged. A historical commit or
old hash is provenance; it is not the expected answer or a current acceptance
gate.

The review stops when the selected commits are classified and each result has a
testable next action, an explicit boundary reason, or a recorded non-applicable
decision. It must not expand into an unrestricted search of the complete Git
history. Return concise commit-level findings and artifacts; do not paste commit
logs or full simulator traces.

All seven confirmed-fix commits indexed in `RESULTS.md` are on the current
verification branch and are not ancestors of `origin/kunminghu-v2` at the
current snapshot. This is commit topology, not independent-discovery
attribution; several correspond to upstream findings or equivalent repairs.
`f8bb99518` preserves Uncache response exception bits through the LoadUnit S1
NC path; its finding is not associated with one of the 58 initial commits and is documented in
[`CPU_BUG_UNCACHE_DCHANNEL_ERROR.md`](CPU_BUG_UNCACHE_DCHANNEL_ERROR.md).
`f5b553973` fixes the VS non-leaf fault-GPA offset exercised by the
`5b82411e` historical row below and is documented in
[`CPU_BUG_VECTOR_GUEST_FAULT_SPLIT.md`](CPU_BUG_VECTOR_GUEST_FAULT_SPLIT.md).
The local fixes are excluded from the upstream commit count.

The current branch also contains an upstream-guided reproduction of PR
[#6548](https://github.com/OpenXiangShan/XiangShan/pull/6548), which fixes a
regression introduced by applying the historical `222f993ed` VLS
`flushAfter` policy to every MemBlock consumer. This is an upstream-guided
historical reproduction, not a new bug independently discovered by this UT.
The retained reproducer and evidence are consolidated below.

## Upstream PR #6548: VLS Exception-Buffer Redirect

### Classification and provenance

The focused VLS reproducer is `3abfd81e6`; the locally applied,
upstream-equivalent repair is `4b976f156`. The test demonstrates that the UT
can reproduce this historical bug class, but it is not evidence of independent
discovery. PR #6548 also references upstream issues #6399, #6482, and #6540;
this audit does not infer untested symptoms from those links.

### Reproducer and external oracle

The load and store directions are reproduced with:

```sh
make -C tests/memblock vector-fof JOBS=8
make -C tests/memblock trigger-contracts JOBS=8
```

Both drive a top-level ROB redirect with `level=flush`,
`isVlsException=1`, and the identified faulting vector uop. The load sequence
then issues a differently addressed, identity-qualified scalar page fault; the
store sequence does the same with a scalar-store breakpoint. Pre-fix RTL
returned stale vector addresses:

```text
MEMBLOCK_VECTOR_FOF_FAIL cycle=134
phase=subsequent-scalar-fault-address
expected_vaddr=0x53003000 actual_vaddr=0x53000180

MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle=100
phase=post-vector-store-fault-address
expected_vaddr=0x80630180 actual_vaddr=0x80630020
```

The exported exception-address payload has no valid bit or transaction
identity, so the oracle deliberately ignores it between exception epochs. It
samples the address only when an exact ROB/writeback identity establishes the
fault consumed by Backend, then requires the later same-direction fault to
publish its own VA. No fixed cycle, ExceptionBuffer state, replay behavior, or
other internal implementation signal decides PASS/FAIL. Read-only Picker
`mem_direct` observations may localize a retained entry but remain debug only.

### Root cause, impact, and fix

Historical fix `222f993ed` changed every VLS exception redirect from `flush`
to `flushAfter` before distributing it through MemBlock. StoreQueue recovery
and StoreMisalignBuffer need that view so the faulting vector store can drain
residual side effects, but load/store ExceptionBuffers must see the original
`flush`. Seeing `flushAfter` retained the faulting entry, which continued to
win oldest selection over later same-direction faults.

This stale address propagates through `LSQWrapper` and the MemBlock exception
mux into Backend and CSR trap handling, where memory faults and breakpoints can
write it to `mtval`, `stval`, or `vstval`. The dynamic reproducers establish
the stale VA in both directions. A retained guest-fault record could also
carry stale GPA/non-leaf metadata toward `mtval2`/`htval` and instruction
markers, but that consequence was not dynamically reproduced and is not part
of the confirmed evidence.

Repair `4b976f156`, patch-equivalent to PR #6548, preserves a raw redirect for
ExceptionBuffers and ordinary consumers while sending the `flushAfter` view
only to vector-store drain consumers. Post-fix `vector-fof`,
`vector-segment-fof`, and `trigger-contracts` pass the stale-address contracts
described above.

These tests prove stale-address removal. They do not prove the original
residual vector-store drain behavior protected by `222f993ed`; a legal
store-progress/memory-effect mutation that distinguishes a revert remains a
verification gap.

## Post-Baseline RTL Audit

The September 1 baseline did not include the fixes merged later in September
and October. The selected additions below were reviewed against the same
black-box acceptance rule; this table is a bounded sample of the additional
upstream commits visible in the current `origin/kunminghu-v2` query, not a
claim that every later commit has already been classified.

| Date | Commit | Fix | Status | Evidence or missing contract |
| --- | --- | --- | --- | --- |
| 2026-10-08 | `c098f996b` | Preserve self-flushing replay information when RAW and RAR redirects share a ROB index | Boundary gap | `memory-violation` and `rar-violation` exercise RAW and RAR separately, but no legal load currently raises both at the same ROB index. The missing acceptance is one selected self-flushing redirect followed by a replay with the final store-to-load value. |
| 2026-09-24 | `49b6a414f` | Preserve a late vector-load DCache exception in the final vector writeback | **Reproduced** | `dcache-errors` primes one line with scalar denied and last-beat-corrupt responses, then requires vector loads to hit the poisoned line with the corresponding terminal exception. An independent mutation restoring `vecldout.hasException := s3_exception` passes the old cold-miss stimulus but fails the poisoned-line case with `exception=0x0`; clean RTL passes both cases. |
| 2026-09-15 | `872fa9be3` | Pass vector store access size to the TLB | **Reproduced** | The clean 1,798-transaction `translation-faults` control passes. An isolated exact revert of the generated `StoreUnit.sv` TLB-size mux (`alignedType` removed) deterministically fails the first Sv39 strided vector-store crossing: a 2-byte store at `0x3fffffffff` must produce identified `StorePageFault` at `0x4000000000`, but after one replay feedback it remains pending past the 16,384-cycle bounded wait with no terminal vector writeback (`vector_pending=1`, `pending_replays=0`). The frozen mutant replay repeats the same externally visible failure. Stage-2 limit crossings remain a gap. |
| 2026-09-15 | `39ad0d589` | Validate the full access range against pointer masking and canonical limits | **Reproduced** | `translation-faults` covers both canonical crossings, every 2/4/8-byte distance, scalar loads/stores and strided/indexed vector stores. An isolated final-byte-check mutation deterministically returns normal scalar data instead of a page fault; clean RTL passes all 1,798 transactions. Schema 48 adds scalar mixed bins. Stage-2 and pointer-masking/trigger combinations remain gaps. |
| 2026-09-15 | `db2a9c26b` | Avoid vector-state dirtiness for vector-store helper uops | Non-functional/outside MemBlock UT | Rename/commit `mstatus.VS` is not observable at the MemBlock boundary. Memory-effect and writeback checks cannot establish CSR dirtiness. |
| 2026-09-17 | `f86f50b83` | Initialize SBuffer forward tag state | Boundary gap | The affected X-only state is not observable in the current two-state Verilator configuration; functional first-forward traffic passes. |

## Canonical Access-Range Mutation

On 2026-10-21, an isolated equivalent mutation of `39ad0d589` was detected by
`translation-faults`. In `TLBNonBlock.sv`, `TLBNonBlock_1.sv`, and
`TLBNonBlock_2.sv`, the seven generated 64-bit `_lastVa_T_*` definitions were
changed from `EffectiveVa_i + ((1 << size) - 1)` to `EffectiveVa_i`. This
preserves first-byte checking while removing the final-byte check. The
mutated copies were substituted into a separate filelist and exported and
compiled into a separate Picker directory; the normal RTL, Picker model, and
ongoing frozen-runtime regression were unchanged.

The same testbench snapshot passes on clean RTL: 220 range faults, 440 alias
controls, 440 unchanged-edge readbacks, and 220 committed store/load recovery
pairs, plus the pre-existing 258 transactions. On the mutant, a two-byte
scalar load at `0x3fffffffff` should fault on `0x4000000000`; instead it
returns `0x3938`, exception mask zero, and RF write enable one. The ordinary
load scoreboard rejects that identified completion against the independently
expected load-page-fault mask `0x2000` and RF write enable zero. Replaying the
same frozen mutant repeats the mismatch. The exception-address payload is
irrelevant once the mutant returns no exception and is not used to support
the finding.

The experiment uses Verilator 5.052 and the existing resolved Picker/xcomm
builds. Its script, source snapshot/diff, exact before/after expressions,
commands, dependency resolution, and clean/mutant/replay results are under
`build/memblock/canonical-mutation/`. Replay the retained artifacts with:

```sh
LD_LIBRARY_PATH=build/memblock/canonical-mutation/clean/runtime \
  build/memblock/canonical-mutation/clean/runtime/memblock_sim --test translation-faults
LD_LIBRARY_PATH=build/memblock/canonical-mutation/fullrange/runtime \
  build/memblock/canonical-mutation/fullrange/runtime/memblock_sim --test translation-faults
```

For a fresh experiment, follow the README build workflow, make the generated
expression substitutions described above in isolated copies, replace only
those three filelist entries, and export/build that separate model with the
same current harness. This is historical-mutation sensitivity evidence, not
a new CPU RTL finding.

The separate `872fa9be3` vector-store-size mutation is also **Reproduced**.
Only the generated `StoreUnit.sv` TLB-size expression is changed:

```text
before: s0_out_isvec ? io_vecstin_bits_alignedType
                    : s0_use_non_prf_flow ? s0_uop_fuOpType[2:0] : 3'h0;
after:  s0_use_non_prf_flow ? s0_uop_fuOpType[2:0] : 3'h0;
```

The scalar cases pass before the first strided vector-store case fails. The
identified two-byte store at `0x3fffffffff` must complete with
`StorePageFault` (`0x8000`), reporting `0x4000000000`. Instead, it remains
pending after the 16,384-cycle replay-aware wait: one replay feedback has
been handled, no replay is pending, and the identified vector scoreboard
entry is still outstanding. Replaying the frozen mutant repeats the same
external completion failure. No terminal fault was observed, so the printed
exception-address payload is undefined and is not evidence of a wrong VA.
The clean control passes the same full 1,798-transaction snapshot. The
normal TLB modules remain unchanged in this experiment.

The exact patch, isolated filelist, dependency-resolution logs, command logs,
manifest, and both results are in
`build/memblock/canonical-mutation/vector_store_size/`. Replay it with:

```sh
LD_LIBRARY_PATH=build/memblock/canonical-mutation/vector_store_size/runtime \
  build/memblock/canonical-mutation/vector_store_size/runtime/memblock_sim --test translation-faults
```

For a fresh model, substitute only the isolated `StoreUnit.sv` copy with the
expression change above, then follow the same separate Picker export/build
workflow as the full-range mutation. This experiment establishes sensitivity
to the historical mux revert; it is not a newly discovered CPU RTL bug.

## Audit

| Date | Commit | Fix | Status | Evidence or missing contract |
| --- | --- | --- | --- | --- |
| 2026-08-20 | `53ace33f` | Kill in-flight prefetch on VSegment | Boundary gap | Needs a legal segment-unit producer concurrent with a DCache S1 prefetch. |
| 2026-08-14 | `e12436c7` | Prevent StoreQueue `rdataPtr` advancing out of order | **Reproduced** | `store-rdata-order`: clean ordered NC data; the mutant returns data from the wrong SQ entry at cycle 309. |
| 2026-08-11 | `856b821f` | Propagate vector VS-non-leaf-PTE metadata | **Reproduced** | `vector-guest-fault`: clean `isForVSnonLeafPTE=1`; the mutant reports marker 0 at cycle 152 while the exact VA/GPA remain correct. |
| 2026-08-11 | `2daff48c` | Prefetch-buffer valid-state replacement check | Non-functional/outside MemBlock UT | Hardware-prefetch replacement policy has no stable correctness oracle in this harness. |
| 2026-08-10 | `9ee7b335` | Misaligned vector store progress | **Covered, mutation did not fail** | The independently reverted mutant still passed `misaligned-stores` with completion, dequeue, and exact readback. A reverse element-order trigger also fails on clean RTL and is therefore not a valid mutation oracle. |
| 2026-08-10 | `e541289b` | Preserve SQ address-valid state on TLB miss | **Reproduced** | `store-tlb-miss-preserve`: the clean oracle requires an allocated, identity-stable SQ entry to retain address-valid state across a TLB miss; the mutant violates the all-entry identity-preserving SVA at tick 283. |
| 2026-08-04 | `2cac7a0d` | Strict ordering of misaligned vector elements | Boundary gap | Current tests generate vector misaligned replays, but the distinguishing reverse-order sequence is not legal/stable with the current dispatch contract. |
| 2026-07-30 | `7754c3a8` | Memory-stall top-down attribution | Non-functional/outside MemBlock UT | Performance-counter attribution, not an architectural memory result. |
| 2026-07-28 | `222f993e` | VLS exception redirect must not flush itself | Covered, not mutated | `vector-fof`, `vector-segment-fof`, and `trigger-contracts` drive a `flush` redirect with `isVlsException=1` for the identified vector uop. They reproduce the distinct stale-address regression from [upstream PR #6548](#upstream-pr-6548-vls-exception-buffer-redirect), caused by applying `flushAfter` globally. They do not yet prove that reverting `222f993e` loses required residual vector-store side effects; a distinguishing legal mutation remains required. |
| 2026-07-21 | `a4047e5a` | Misaligned vector store progress | Covered, not mutated | `misaligned-stores` and mandatory `random-mixed` vector-store replay/readback phases. |
| 2026-07-21 | `04c0d157` | SPVP mode for HLV/HLVX/HSV PMP checks | Covered, not mutated | `hypervisor-contracts` implements 18 M-mode HLV/HLVX/HSV cases crossing SPVP=U/S with R/X/RW/RX and locked R/RWX PMP regions, using independent R, R+X, and W permission rules. The 82-case directed run passes; constrained mixed traffic also closes all 42 family x SPVP x PMP-relation bins. An independent SPVP-mode revert and broader PMP size/TOR/permission/edge crosses remain. |
| 2026-07-21 | `fbb1e349` | Cross-page vector misaligned store `s_block` progress | **Covered, mutation did not fail** | The rebuilt pre-fix mutant still passed the expanded translated cross-page vector-store progress sequence with exact writeback, dequeue, and readback. |
| 2026-07-10 | `52262f30` | Blocking issue logic for misaligned vector stores | Boundary gap | Needs controlled overlap of scalar-store issue, vector-store dequeue, and misalign-buffer occupancy. |
| 2026-07-10 | `9b926b6b` | Vector misaligned store must block in SMB | Covered, not mutated | Translated cross-page vector store checks writeback, commit, SQ drain, and exact readback. |
| 2026-07-10 | `50bd7957` | Vector exception GPA width | Covered, not mutated | Scalar/vector guest-page-fault tests compare the complete reported GPA with an independent walk. |
| 2026-06-24 | `2acbf327` | VSegment ready connection | Covered, not mutated | `vector-segment` drives lane-0 segment issue with ready handshake and exact complete response/data checks across all four address modes; the expanded full legal matrix passes 1,352 configurations. An independent ready-connection revert and dedicated arbitration against backpressured ordinary vector traffic remain untested. |
| 2026-06-24 | `098fa583` | VSplit must use vector uop index | Covered, not mutated | `vector-addressing` covers split flows and all address modes, but not a multi-uop segment stream. |
| 2026-06-20 | `5b82411e` | Misaligned vector cross-page exception address | Covered, not mutated | Clean pre-fix RTL reproduced the split VS-non-leaf GPA offset (`0x94001808` vs `0x94001800`). Branch fix `f5b553973` suppresses the element offset when `isForVSnonLeafPTE=1`; deterministic and randomized control evidence is in [`CPU_BUG_VECTOR_GUEST_FAULT_SPLIT.md`](CPU_BUG_VECTOR_GUEST_FAULT_SPLIT.md). No independent revert mutation was retained. |
| 2026-06-20 | `756bbf59` | NaN-box half FP MMIO loads | Reproduced | A same-source clean harness passes 24 writebacks and 13 Uncache requests, including five FLH and five FLW payloads with sign-bit-clear/set bins 3/2. An isolated equivalent generated-RTL revert removes the MMIO LH `fpWen` boxing mux in the shared `LoadUnit`; it fails twice identically at cycle 128 on the first sign-clear FLH (`0x0000`), returning `0x0` instead of `0xffffffffffff0000`. Exact lane 2/ROB 0/pdest 21, successful FP-only destination, MMIO classification, zero DCache requests, and one Uncache request identify the data contract. Source snapshot, one-file replacement, toolchain, local dependency resolution, clean/mutant/replay logs and results are retained under `build/memblock/fp-boxing-mutation/`. |
| 2026-06-20 | `9045e063` | Store exception priority by ROB/uop, not SQ index | Covered, not mutated | `exception-contracts` issues two simultaneous PBMT-NC misaligned stores whose SQ and wrapped ROB orders disagree, then requires the exported VA of the oldest ROB. An independent revert RTL has not been built. |
| 2026-06-20 | `7ce99d4a` | VSplit threshold must not use LSQ pointer | Covered, not mutated | Split vector traffic plus LQ/SQ and ROB wrap/pressure are checked, but the exact threshold mutation was not built. |
| 2026-06-20 | `e42d4b51` | Cross-page SMB matching uses SQ `rdataPtr` | Covered, not mutated | Translated cross-page vector store requires exact completion, SQ drain, and readback. |
| 2026-06-20 | `9f988c01` | Non-debug mode cannot access debug memory | Reproduced | `mmio-contracts` checks 24 LD/SD x initial-nondebug/Debug-Mode/exit x aligned-edge cases at `0x3801fff8`, `0x38020000`, `0x38020ff8`, and `0x38021000`. Protected accesses fault outside Debug Mode with zero manager requests; allowed operations require exact Uncache traffic, and all twelve store cases use DUT readback. Faulted stores finish with exactly one observed dequeue/cancellation and are explicitly redirected. An isolated equivalent range-guard revert passes on clean RTL and fails twice identically at the existing interior denied-LD case (cycle 8408, missing expected fault completion and bounded timeout; no accepted forbidden manager request). `build/memblock/debug-guard-expansion/sensitivity-verification.json` records matched binaries and commands. The full scenario mutant stops before the added matrix. A separately selected fixture then checks all eight protected first/last-doubleword x LD/SD x initial-nondebug/real-exit bins: each passes twice on clean RTL and fails twice with identical same-case output on the same mutant. Fixture and functional oracles, exact shared binary/arguments, model selection, and limits were independently reviewed (`build/memblock/debug-guard-bin-sensitivity/{summary.json,review.json}`). The selected fixture resets before entry and does not claim original-stream equivalence. Failures are missing expected writeback timeouts, not proof of accepted forbidden traffic. Misaligned crossings, fetch, and restrictive-PMP override remain gaps. |
| 2026-06-17 | `22cc0b6b` | HLVX final PA execute permission | Covered, not mutated | `hypervisor-contracts` includes HLVX PMP execute denial, the SPVP permission matrix, and fixed-PMA execute denial with exact access-fault checking; the 82-case run passes. Legal speculative target-line reads may occur before permission denial; neither their presence nor cardinality is an oracle. An independent execute-permission revert remains unbuilt. |
| 2026-06-09 | `be3e3761` | VSegment load S1 data-read kill | Boundary gap | Segment takeover and DCache bank-read observation are absent. |
| 2026-06-04 | `d1cb3398` | Misaligned vector store split logic | Covered, not mutated | `misaligned-stores`, `vector-addressing`, and mandatory mixed replay/readback. |
| 2026-06-01 | `b28e7b40` | Killed loads must not read DCache bank | Non-functional/outside MemBlock UT | Architectural cancellation is checked; internal bank-read power behavior is not externally observable. |
| 2026-05-26 | `6af73d2b` | Disallow speculation for split misaligned loads | Covered, not mutated | Scalar misaligned cache-line/page splits plus redirect recovery and exact data checks. |
| 2026-05-26 | `5d886c6f` | Ordered-index vector load split order | Covered, not mutated | `vector-addressing` checks indexed-ordered data against independently decoded indices. |
| 2026-05-26 | `3f00c595` | HLV/HLVX arbitration metadata | Stimulus gap | HLV/HLVX producers and scalar/vector coissue exist, but no current test demonstrates scalar H metadata remaining valid on the same LoadUnit lane while vector split flows are selected. Separate H and vector tests do not cover this cross. The independent discriminator is ordinary vector host mapping/data versus distinct guest alias mapping/data or HLVX execute permissions; legal arbitration ordering must remain unconstrained. Audit: `build/memblock/hypervisor-bounded-audit/input.json`. |
| 2026-05-20 | `d38fc34d` | Misaligned loads filling LQ RAR can deadlock | Boundary gap | Needs multiple simultaneous split misaligned loads under RAR pressure, not sequential completion. |
| 2026-05-20 | `2751f4ec` | Misalign VSplit ROB-age comparison | Boundary gap | Needs concurrent wrapped ROB ages in the split buffer. |
| 2026-05-19 | `f2bbca15` | Avoid X in LoadQueueRAR | Non-functional/outside MemBlock UT | The two-state Verilator model cannot prove four-state X behavior. |
| 2026-04-29 | `0d4264d5` | Misalignment exception logic | Covered, not mutated | Enabled/disabled scalar and vector misalignment, PBMT-NC misalignment, and exact exception bits. |
| 2026-04-29 | `c45f5372` | CMO execution under PBMT | Boundary gap | The CBO/CMO request-response model is executable, but the PBMT-mode CMO combination is not. |
| 2026-05-05 | `7a25d9c9` | Atomic cache-error lifetime | **Covered, not mutated** | `atomic-dchannel-errors` executes a clean cold AMO and warm readback after each complete denied/corrupt batch, proving that prior AtomicsUnit error state does not leak into a later clean operation. |
| 2026-05-05 | `74479de4` | Misaligned vector store progress | Covered, not mutated | Focused and random mixed vector-store split/replay tests. |
| 2026-05-05 | `7a5c5213` | Prefetch must not raise an exception | Covered, not mutated | `exception-contracts` issues an unmapped software prefetch and requires non-exceptional, no-RF-write completion. |
| 2026-05-02 | `2ed71b93` | `mtvec` reset-valid plumbing | Non-functional/outside MemBlock UT | CSR reset integration outside memory transaction semantics. |
| 2026-04-25 | `92cbd40f` | `xepc` update logic | Non-functional/outside MemBlock UT | CSR state update is not an owned MemBlock architectural result. |
| 2026-04-28 | `d3c68834` | `mtvec` reset value | Non-functional/outside MemBlock UT | CSR reset integration. |
| 2026-04-27 | `5fedf66d` | Missing reset value causing X state | Non-functional/outside MemBlock UT | Reset/quiescence is checked, but a two-state model cannot reproduce an X-only failure. |
| 2026-04-20 | `45318c5d` | Load exception must clear scalar RF write enable | **Reproduced** | `exception-contracts`: the mutant asserts RF write enable on a load page fault instead of suppressing the register write. |
| 2026-04-01 | `54944b02` | Initialize trace pipeline `RegNext` | Non-functional/outside MemBlock UT | Trace/X initialization only. |
| 2026-03-26 | `4d640694` | Prefetcher PC `RegEnable` control | Non-functional/outside MemBlock UT | Hardware-prefetch training metadata lacks an architectural oracle. |
| 2026-03-16 | `82bd98e0` | Store misalign-buffer revoke | Covered, not mutated | Misaligned store split/replay and redirect are covered separately; exact simultaneous revoke mutation remains unbuilt. |
| 2026-03-16 | `f1c259a3` | MMIO store marks ROB | Boundary gap | MMIO classification/ROB protocol is absent. |
| 2026-03-16 | `aa92243e` | Vector store partial replay | Covered, not mutated | Focused and random vector-store tests consume lane/mask/index partial replay feedback. |
| 2026-03-06 | `350d979e` | StoreQueue NC behavior | Covered, not mutated | Ordered PBMT-NC store/load round trip with SQ data identity and uncache response checks. |
| 2026-03-06 | `b83d7d2b` | `sqNeedDeq` at split-store end | Covered, not mutated | Cross-page vector-store completion requires eventual exact SQ dequeue count. |
| 2026-03-06 | `5fd79dbb` | SMB timing optimization | Non-functional/outside MemBlock UT | Timing-only change with no intended functional delta. |
| 2026-03-06 | `1493f6a0` | `needGpa` translation freeze | Covered, not mutated | Sv39x4 cold/warm translation and scalar/vector VS non-leaf guest faults run with PTW backpressure. |
| 2026-03-06 | `29c0c967` | Misaligned vector store progress | Covered, not mutated | Focused and mixed vector-store replay/readback. |
| 2026-02-27 | `ef980dec` | Vector-store event refactor | Non-functional/outside MemBlock UT | Feature/refactor rather than a stated functional bug fix. |
| 2026-01-28 | `607f09d9` | SBuffer timeout CSR feature | Non-functional/outside MemBlock UT | Feature addition; SBuffer timeout CSR programming is outside the current contract. |
| 2026-01-19 | `fb9fdc12` | Load exception priority by ROB/uop, not LQ index | Covered, not mutated | `exception-contracts` issues three simultaneous load page faults across ROB 158, 159, and 0 with opposite LQ order, then requires the exported VA of ROB 158. An independent revert RTL has not been built. |
| 2026-01-13 | `f9fd32a0` | Connect vector exception LQ/SQ indices | Boundary gap | LQ/SQ index is not exposed in the architectural exception-address result; concurrent priority test is still needed. |
| 2026-01-04 | `fa34af08` | Scalar misaligned store progress | Covered, not mutated | `misaligned-stores` checks 16-byte, cache-line, and translated page splits with exact readback. |

`vector-guest-fault-split` is intentionally not part of an all-green regression
on pre-fix RTL. VA `0x60000188` produced GPA `0x94001808`; the independent page
walk and scalar/aligned-vector controls produce `0x94001800`. The finding was
not weakened to match the design. Branch fix `f5b553973` adds the conditional
GPA handling in `VMergeBuffer.scala`; see
[`CPU_BUG_VECTOR_GUEST_FAULT_SPLIT.md`](CPU_BUG_VECTOR_GUEST_FAULT_SPLIT.md)
for the reproducer and landing evidence.


## Segment Full-VA Capture Mutation

The 2025-08-18 fix `39824ca68394b819515643924a93003a084e2aaa`
(`#4954`, an ancestor of `origin/kunminghu-v2`) widens `latchVaddr` and
`latchVaddrDup` to XLEN and captures the complete segment VA. It completes the
earlier `71802692d97cd368f6a3c18255d49a12058bb06d` adjustment. Low-half-only
segment addresses did not distinguish losing the upper VA bits when a
misaligned beat requests translation again.

An isolated equivalent mutation changes only the two generated capture
assignments in `VSegmentUnit.sv` to `{14'b0, _vaddr_T[49:0]}`. The 2,007
actual RTL input files are copied into a private snapshot; the prepared base
metadata is preserved unchanged even though its historical count says 2,006.
The isolated model, matching generated wrapper, CPP snapshot, mutation patch,
build commands and runtime linkage are retained under
`build/memblock/segment-va-mutation/`. The normal model and generated RTL are
unchanged. This is a full-VA-loss equivalent mutation, not a wholesale revert
of all later segment RTL changes.

With identical prepared CPP sources, clean `vector-segment-canonical` passes
all 12 cases. The mutant passes the low-half and aligned high-half controls,
then reports unexpected load-page-fault mask `0x2000` at cycle 754 for Sv39 VA
`0xffffffc04000018f`, where the independent mapping requires success. Two
mutant runs have byte-identical simulator output. Reproduce with:

```sh
LD_LIBRARY_PATH=build/memblock/segment-va-mutation/runtime \
  build/memblock/segment-va-mutation/runtime/memblock_sim \
  --test vector-segment-canonical
```

The same focused `random-mixed` seed 282 passes 256 actions on clean RTL and
fails the mutant twice with byte-identical output at action 99, cycle 3677:
VA `0xffffffc080210186` is independently mapped to PA `0x80210186`, but its
ordinary segment load returns `0x2000` instead of zero exception. The exact
constraint command is retained in `load-split-mutant-1.json` and
`segment-high-va-development/refined/load-split.json`. This establishes mixed
stimulus and external scoreboard sensitivity to the historical bug class;
it is not a new CPU bug or a claim that the unmodified broad default
profiles have already completed schema-51 closure stress. Schema 52 composes
all four high-half nested VS/G pairs into common traffic; segment noncanonical
faults, high-half FOF composition and partial-fault progress remain gaps.
