# V3 e5c70547f Runtime Risk Re-audit

Date: 2026-09-02

## Scope and provenance

This re-audit is bound to verification HEAD `aefd4696c51c71adaac434e5e6d8f085c73e187a`
and the clean matching Verilator manifest:

- implementation/source: `1a32a9056d993233fa1bf3a394b16e8a762abf52`
- design baseline: `e5c70547f3a966accf20a4b065ec1d8e33443180`
- manifest SHA-256: `d2fbabf8640ae4033b2c4ddfebabdcc76e9b7c66709ae234eb7eb6edd4e6e2fa`
- configuration: `DefaultConfig`

The review covers #6220, #6398, #6417, #6425, #5898, and #6431. It does
not infer behavior from the manifest and does not revalidate historical DUT
artifacts. The existing BIN-1104 checkpoint and all user-owned worktree
changes were left untouched.

## Contract impact

No active 343-leaf contract or runtime producer still uses the obsolete
#6220 interfaces. The current signal inventory and fail-closed tests confirm:

- ICache acceptance uses one `mainPipe.io_toIfu_req` transaction. Block
  identity comes from `io_toIfu_req_bits_info_0/1_*`; coordinate state comes
  from the top-level `firstRange`, `totalRange`, and `maybeRvcMap` fields.
- S1 observations use registered raw cache-line data, index-based selection,
  cross-block stitching, `s1_alignedInstrVec`, and atomic
  `s1_prevEndHalfRviInfo.{valid,bits.data,bits.pc}`.
- S2 observations pair `s2_alignedInstrVec`, `s2_alignedInstrPcVec`,
  `s2_alignedPdInfoVec`, and `s2_alignedJumpOffsetVec` from the same
  registered transaction. The retained `s2_alignedPdInfoVec` name is a
  registered S2 payload, not the obsolete same-cycle S2 predecode contract.
- PredChecker and writeback retain raw `blockSel` and
  `isCrossBlockInstr`. Producers calculate effective ownership as
  `blockSel | isCrossBlockInstr`; a missing cross-block probe fails closed
  instead of defaulting to zero.
- The BPU/WayLookup contract uses shared `PrefetchDepth=32`; no runtime
  contract depends on `BpRunAheadDistance` or `WayLookupSize`.

The main affected canonical leaves and producers are:

- aggregate coordinate and boundary: BIN-807/808/828/829/902/911-919 and
  BIN-844-847, produced by `cacheable_pipeline_funcov.py` and
  `two_fetch_funcov.py`;
- S1 index/stitch and registered S2 coherence: BIN-848-851, BIN-870-876,
  produced by `cacheable_pipeline_funcov.py`, `compact_funcov.py`, and
  `two_fetch_funcov.py`;
- raw/effective owner and redirect payload: BIN-427/872/897/941/944-950/
  976/982, produced by `two_fetch_funcov.py` and `compact_funcov.py`;
- atomic half-RVI state: BIN-858-867/920-922/940/948/950/951/953/960,
  produced by the cacheable, compact, NC/MMIO, and InstrUncache samplers;
- non-CFI training: BIN-934, produced only after checked
  `notCfiTaken -> wbRedirect.canTrain -> FTQ ifuResolve` identity matching.

The architectural InstrUncache protocol checkers remain behavior-level and
are not directly changed by the aggregate cacheable payload. They still need
current-provenance reruns whenever their surrounding IFU redirect or
half-RVI identity is part of the checkpoint.

## Blocked and review-only leaves

- BIN-908 remains `PARTIAL`: no legal natural producer proves independent
  second-block PMP ownership, and second-only ITLB production remains under
  design review.
- BIN-909 remains `BLOCKED`: the current RTL still does not preserve precise
  second-cacheline late parity/TL-fault lane ownership through the merged IFU
  exception path.
- BIN-1004 remains `BLOCKED`: FrontendTrigger does not consume the required
  `data/pds` semantics and the standalone DUT has no equivalent optimized
  probes.

Signal absence, a default value, a static review, or an indirect observation
is not a HIT for any of these leaves.

## Validation results

The focused unit and signal-contract gate passed `223` tests. It includes
negative tests for missing aggregate/index/cross-owner/predecode probes,
transaction mismatch and timeout, all-not-taken override, shared-depth
observation, MBTB compareBits, and BTB versus ITTAGE/RAS target comparison.

Current-DUT runs:

- `ctrl_6220_risk_reaudit_aefd_20260902_01`: 11 passed, 1 failed.
  The passing cases cover asymmetric hit/refill, IBuffer backpressure,
  backend redirect at held/late-response boundaries, invalidTaken,
  cross-block JAL/JALR ownership, all-not-taken, PrefetchDepth full/wrap/
  flush, MBTB compareBits, and BTB lower-target differences.
- `ctrl_6220_risk_reaudit_aefd_20260902_02`: 3 passed. It independently
  covers trained hit-hit followed by fence.i miss-miss, non-CFI false-hit
  training through BIN-934, and BIN-1067's older checker redirect versus a
  younger NC internal-request race.

All 11 passing artifacts with canonical targets are gate-eligible with empty
rejection reasons and positive exact targets. The all-not-taken, MBTB, and BTB
negative canaries intentionally have no canonical bin target; they are
behavioral risk evidence only and cannot be backannotated.

For BIN-1067, the checked witness contains a younger NC transaction in S2
while the older cacheable checker redirect and internal request ready occur.
Flush wins, the old identity emits no InstrUncache request, TL A request,
response, or IBuffer delivery, and a distinct recovery identity completes.

The sole failed case is the user-owned
`test_icache_cacheable_same_line_sram_hit`. Its second initialization reuses
the fixture's cumulative `resp_line_count`; `>= 1` is already true before the
new target line refills. The immediate redirect therefore does not revisit a
resident target line, and the subsequent `miss_rate=1.0` stream times out.
Checker and monitor errors are zero. The protected testcase was not edited;
the independent ctrl-owned hit-hit/fence.i test above supplies the current DUT
hit/refill risk check. This is a test-harness diagnostic, not an RTL failure or
coverage promotion.

## Coverage disposition

No status was changed by this re-audit. The fixed strict view remains:

`304 HIT / 35 MODELED / 1 PARTIAL / 3 BLOCKED = 343` (`88.63%`)

The four canonical sections remain `115/139`, `77/81`, `42/42`, and `70/81`
HIT. The global denominator remains `879`. Further promotion still requires
current provenance, an explicit target, checker pass, zero monitor/contract
errors, and a positive exact-target observation.
