# 2026 MemBlock History Audit

## Scope and Evidence Rules

This audit covers every commit returned by:

```sh
git log kunminghu-v2 --since=2026-01-01 -- \
  src/main/scala/xiangshan/mem
```

The baseline is `0fa7bb8259a7922481289d8d5932797afce84030`. There are
58 commits in scope as of 2026-09-01. The status vocabulary is deliberately
strict:

- **Reproduced**: an independently generated RTL mutation that reverts the fix
  passes on clean RTL and fails the same test on the mutant.
- **Covered, not mutated**: a current test directly exercises the affected
  architectural/protocol behavior, but no independent revert was built.
- **Covered, mutation did not fail**: a revert was built and the most relevant
  current test still passed. This is not sensitivity evidence.
- **Boundary gap**: the required legal producer, consumer, or architectural
  observation is not modeled at the current MemBlock boundary.
- **Non-functional/outside MemBlock UT**: performance attribution, X-only,
  trace, reset plumbing, feature/refactor, or an effect without a stable
  architectural oracle at this boundary.

Structural pin stimulation is never used as evidence of semantic coverage.

The current worktree has one additional RTL finding that is not yet associated
with a committed historical change: Uncache denied/corrupt D-channel exception
bits were discarded by the LoadUnit S1 NC path. The deterministic
reproducer, pre-fix output, root-cause analysis, and repaired evidence are in
[`CPU_BUG_UNCACHE_DCHANNEL_ERROR.md`](CPU_BUG_UNCACHE_DCHANNEL_ERROR.md). It is classified as
confirmed/fixed current-worktree evidence rather than added to the 58-commit
table below.

The current branch also replays upstream PR
[#6548](https://github.com/OpenXiangShan/XiangShan/pull/6548), which fixes a
regression introduced by applying the historical `222f993ed` VLS
`flushAfter` policy to every MemBlock consumer. This is an upstream-guided
historical reproduction, not a new bug independently discovered by this UT.
The retained reproducer and evidence are consolidated below.

## Upstream PR #6548: VLS Exception-Buffer Redirect

### Classification and provenance

The sequence is unambiguous: commit `a2b4c3a35` first isolated an unrelated
exception-address oracle false positive at 02:11:39 CST; upstream PR #6548 was
then fetched as `origin/pr-6548` at `f67f9e78f` at 02:21:52; the focused VLS
reproducer was committed as `3abfd81e6` at 02:32:09; and the
upstream-equivalent fix was cherry-picked as `4b976f156` at 02:32:40. The test
therefore demonstrates that the UT can reproduce this historical bug class,
but it is not evidence of independent discovery.

The pre-fix generated MemBlock has complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`;
the post-fix RTL has
`356025f4a7472e978800120eee3f0b78832939b9c3b6b6ad07fc2f2b32fb60d3`.
PR #6548 also references upstream issues #6399, #6482, and #6540; this audit
does not infer untested symptoms from those links.

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

Repair `4b976f156`, patch-equivalent to PR #6548 at `f67f9e78f`, preserves a
raw redirect for ExceptionBuffers and ordinary consumers while sending the
`flushAfter` view only to vector-store drain consumers. The frozen schema-2
artifact `build/memblock/vls-postfix.json` has SHA-256
`e2a84f4422adfe8ff77da91dc418902a5662d9bec1370d4dd9d1c0333dc52c69`
and records post-fix passes for `vector-fof`, `vector-segment-fof`, and
`trigger-contracts` with unchanged runtime/controller hashes.

These tests prove stale-address removal. They do not prove the original
residual vector-store drain behavior protected by `222f993ed`; a legal
store-progress/memory-effect mutation that distinguishes a revert remains a
verification gap.

## Audit

| Date | Commit | Fix | Status | Evidence or missing contract |
| --- | --- | --- | --- | --- |
| 2026-08-20 | `53ace33f` | Kill in-flight prefetch on VSegment | Boundary gap | Needs a legal segment-unit producer concurrent with a DCache S1 prefetch. |
| 2026-08-14 | `e12436c7` | Prevent StoreQueue `rdataPtr` advancing out of order | **Reproduced** | `store-rdata-order`: clean ordered NC data; mutant returns data from the wrong SQ entry. |
| 2026-08-11 | `856b821f` | Propagate vector VS-non-leaf-PTE metadata | **Reproduced** | `vector-guest-fault`: clean `isForVSnonLeafPTE=1`; mutant returns 0 for the same exact VA/GPA. |
| 2026-08-11 | `2daff48c` | Prefetch-buffer valid-state replacement check | Non-functional/outside MemBlock UT | Hardware-prefetch replacement policy has no stable correctness oracle in this harness. |
| 2026-08-10 | `9ee7b335` | Misaligned vector store progress | **Covered, mutation did not fail** | Independent mutant hash `9000f90a...`; `misaligned-stores` passed. A reverse element-order trigger also fails on clean RTL and is therefore not a valid mutation oracle. |
| 2026-08-10 | `e541289b` | Preserve SQ address-valid state on TLB miss | **Reproduced** | `store-tlb-miss-preserve`: mutant violates the all-entry identity-preserving SVA one cycle after a miss. |
| 2026-08-04 | `2cac7a0d` | Strict ordering of misaligned vector elements | Boundary gap | Current tests generate vector misaligned replays, but the distinguishing reverse-order sequence is not legal/stable with the current dispatch contract. |
| 2026-07-30 | `7754c3a8` | Memory-stall top-down attribution | Non-functional/outside MemBlock UT | Performance-counter attribution, not an architectural memory result. |
| 2026-07-28 | `222f993e` | VLS exception redirect must not flush itself | Producer implemented; mutation gap | `vector-fof`, `vector-segment-fof`, and `trigger-contracts` now drive a `flush` redirect with `isVlsException=1` for the identified vector uop. They reproduce the distinct stale-address regression from [upstream PR #6548](#upstream-pr-6548-vls-exception-buffer-redirect), caused by applying `flushAfter` globally. They do not yet prove that reverting `222f993e` loses required residual vector-store side effects; that original SQ-drain behavior still needs a distinguishing legal mutation experiment. |
| 2026-07-21 | `a4047e5a` | Misaligned vector store progress | Covered, not mutated | `misaligned-stores` and mandatory `random-mixed` vector-store replay/readback phases. |
| 2026-07-21 | `04c0d157` | SPVP mode for HLV/HLVX/HSV PMP checks | Boundary gap | HLV, HLVX, HSV and SPVP-specific PMP reference checks are not modeled. |
| 2026-07-21 | `fbb1e349` | Cross-page vector misaligned store `s_block` progress | **Covered, mutation did not fail** | Rebuilt pre-fix mutant hash `5bfe14db...`; the expanded translated cross-page vector-store progress sequence still passed with exact writeback, dequeue, and readback. |
| 2026-07-10 | `52262f30` | Blocking issue logic for misaligned vector stores | Boundary gap | Needs controlled overlap of scalar-store issue, vector-store dequeue, and misalign-buffer occupancy. |
| 2026-07-10 | `9b926b6b` | Vector misaligned store must block in SMB | Covered, not mutated | Translated cross-page vector store checks writeback, commit, SQ drain, and exact readback. |
| 2026-07-10 | `50bd7957` | Vector exception GPA width | Covered, not mutated | Scalar/vector guest-page-fault tests compare the complete reported GPA with an independent walk. |
| 2026-06-24 | `2acbf327` | VSegment ready connection | Boundary gap | Segment issue/response agent is absent. |
| 2026-06-24 | `098fa583` | VSplit must use vector uop index | Covered, not mutated | `vector-addressing` covers split flows and all address modes, but not a multi-uop segment stream. |
| 2026-06-20 | `5b82411e` | Misaligned vector cross-page exception address | **Reproduced and fixed** | Clean RTL reproduced the split VS-non-leaf GPA offset (`0x94001808` vs `0x94001800`); `VMergeBuffer` now suppresses the offset for `isForVSnonLeafPTE`, and the repaired deterministic plus randomized boundary tests pass. |
| 2026-06-20 | `756bbf59` | NaN-box half FP MMIO loads | Boundary gap | MMIO and FP-destination load contracts are not modeled. |
| 2026-06-20 | `9045e063` | Store exception priority by ROB/uop, not SQ index | Covered, not mutated | `exception-contracts` issues two simultaneous PBMT-NC misaligned stores whose SQ and wrapped ROB orders disagree, then requires the exported VA of the oldest ROB. Independent revert RTL is deferred until the active endurance campaign releases elaboration resources. |
| 2026-06-20 | `7ce99d4a` | VSplit threshold must not use LSQ pointer | Covered, not mutated | Split vector traffic plus LQ/SQ and ROB wrap/pressure are checked, but the exact threshold mutation was not built. |
| 2026-06-20 | `e42d4b51` | Cross-page SMB matching uses SQ `rdataPtr` | Covered, not mutated | Translated cross-page vector store requires exact completion, SQ drain, and readback. |
| 2026-06-20 | `9f988c01` | Non-debug mode cannot access debug memory | Boundary gap | Debug-region PMP/PMA configuration and architectural debug-mode transitions are absent. |
| 2026-06-17 | `22cc0b6b` | HLVX final PA execute permission | Boundary gap | HLVX and independent execute-permission PMP oracle are absent. |
| 2026-06-09 | `be3e3761` | VSegment load S1 data-read kill | Boundary gap | Segment takeover and DCache bank-read observation are absent. |
| 2026-06-04 | `d1cb3398` | Misaligned vector store split logic | Covered, not mutated | `misaligned-stores`, `vector-addressing`, and mandatory mixed replay/readback. |
| 2026-06-01 | `b28e7b40` | Killed loads must not read DCache bank | Non-functional/outside MemBlock UT | Architectural cancellation is checked; internal bank-read power behavior is not externally observable. |
| 2026-05-26 | `6af73d2b` | Disallow speculation for split misaligned loads | Covered, not mutated | Scalar misaligned cache-line/page splits plus redirect recovery and exact data checks. |
| 2026-05-26 | `5d886c6f` | Ordered-index vector load split order | Covered, not mutated | `vector-addressing` checks indexed-ordered data against independently decoded indices. |
| 2026-05-26 | `3f00c595` | HLV/HLVX arbitration priority | Boundary gap | Hypervisor load instruction producer is absent. |
| 2026-05-20 | `d38fc34d` | Misaligned loads filling LQ RAR can deadlock | Boundary gap | Needs multiple simultaneous split misaligned loads under RAR pressure, not sequential completion. |
| 2026-05-20 | `2751f4ec` | Misalign VSplit ROB-age comparison | Boundary gap | Needs concurrent wrapped ROB ages in the split buffer. |
| 2026-05-19 | `f2bbca15` | Avoid X in LoadQueueRAR | Non-functional/outside MemBlock UT | The two-state Verilator model cannot prove four-state X behavior. |
| 2026-04-29 | `0d4264d5` | Misalignment exception logic | Covered, not mutated | Enabled/disabled scalar and vector misalignment, PBMT-NC misalignment, and exact exception bits. |
| 2026-04-29 | `c45f5372` | CMO execution under PBMT | Boundary gap | CBO/CMO request-response model is absent. |
| 2026-05-05 | `7a25d9c9` | Atomic cache-error lifetime | **Covered, not mutated** | `atomic-dchannel-errors` executes a clean cold AMO and warm readback after each complete denied/corrupt batch, proving that prior AtomicsUnit error state does not leak into a later clean operation. |
| 2026-05-05 | `74479de4` | Misaligned vector store progress | Covered, not mutated | Focused and random mixed vector-store split/replay tests. |
| 2026-05-05 | `7a5c5213` | Prefetch must not raise an exception | Covered, not mutated | `exception-contracts` issues an unmapped software prefetch and requires non-exceptional, no-RF-write completion. |
| 2026-05-02 | `2ed71b93` | `mtvec` reset-valid plumbing | Non-functional/outside MemBlock UT | CSR reset integration outside memory transaction semantics. |
| 2026-04-25 | `92cbd40f` | `xepc` update logic | Non-functional/outside MemBlock UT | CSR state update is not an owned MemBlock architectural result. |
| 2026-04-28 | `d3c68834` | `mtvec` reset value | Non-functional/outside MemBlock UT | CSR reset integration. |
| 2026-04-27 | `5fedf66d` | Missing reset value causing X state | Non-functional/outside MemBlock UT | Reset/quiescence is checked, but a two-state model cannot reproduce an X-only failure. |
| 2026-04-20 | `45318c5d` | Load exception must clear scalar RF write enable | **Reproduced** | `exception-contracts`: reverted RTL writes the scalar RF on a load page fault. |
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
| 2026-01-19 | `fb9fdc12` | Load exception priority by ROB/uop, not LQ index | Covered, not mutated | `exception-contracts` issues three simultaneous load page faults across ROB 158, 159, and 0 with opposite LQ order, then requires the exported VA of ROB 158. Independent revert RTL is deferred until the active endurance campaign releases elaboration resources. |
| 2026-01-13 | `f9fd32a0` | Connect vector exception LQ/SQ indices | Boundary gap | LQ/SQ index is not exposed in the architectural exception-address result; concurrent priority test is still needed. |
| 2026-01-04 | `fa34af08` | Scalar misaligned store progress | Covered, not mutated | `misaligned-stores` checks 16-byte, cache-line, and translated page splits with exact readback. |

## Mutation Evidence

| Reverted fix | Clean oracle | Mutant result | Complete mutant RTL SHA-256 |
| --- | --- | --- | --- |
| `e541289b` | Allocated, identity-stable SQ entry retains address-valid state across a TLB miss | SVA failure at tick 283 | `c525bef7dbb231ace69254e57b6fc7d7069dcb10ddea17176d548c9277565b8e` |
| `e12436c7` | NC stores consume committed SQ data in program order | Wrong store data at cycle 309 | `757dfc00827ed605d63db23a2d9bc995732628abf18b4b6cc203ef8190c49a30` |
| `45318c5d` | Exceptional scalar loads never request an RF write | Reverted RTL asserts RF write enable | See `RESULTS.md` for the independently generated runtime record. |
| `856b821f` | A G-stage fault while walking a VS non-leaf PTE reports the marker and exact VA/GPA | Marker is 0 at cycle 152; VA/GPA remain correct | `3abf90d701fdb3252ec68d1426707affe4d0606bb002a1b02d710be577baa5a7` |
| `9ee7b335` | Misaligned vector stores eventually complete, dequeue, and read back exactly | Mutant passed the available legal sequence | `9000f90adc416d1de6c7b4e8ce2b0129cf98564431ae835bcfc6d857f89e4df2` |
| `fbb1e349` | A translated cross-page vector store eventually completes, dequeues, and reads back exactly | Rebuilt pre-fix mutant passed the expanded progress sequence | `5bfe14db09a494c64415ee9a4d330ddbae9e704fbd18970d3b127ccde7d7b19` |

`vector-guest-fault-split` is intentionally not part of an all-green regression.
On clean RTL, VA `0x60000188` produces GPA `0x94001808`; the independent page
walk and scalar/aligned-vector controls produce `0x94001800`. This is retained
as a deterministic RTL candidate rather than weakened to match the design.
`CPU_BUG_VECTOR_GUEST_FAULT_SPLIT.md` records the reproducer and localizes the exact
`+8` to the unconditional unit-stride GPA offset in `VMergeBuffer.scala`. A
conditional GPA patch generated in an independent worktree makes the exact
reproducer pass without weakening the oracle; it is validation evidence, not
a modification to the baseline CPU tree.
