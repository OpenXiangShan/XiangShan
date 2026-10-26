# CPU Bug: Vector Segment Noncanonical VA Does Not Fault

## Status

Confirmed RTL bug, fixed. The fix is `d7d096e6e` and is an ancestor of the
current HEAD. The historical bug record was removed during the `d504ba3ca`
documentation consolidation; this file restores the current-state summary
without reopening the bug.

## Symptom

A one-uop vector segment load/store whose full Sv39 virtual address is
noncanonical completes with `exception = 0x0` instead of raising a page fault.
The DUT also skips the DTLB request and returns the old destination data. For a
store, the same defect can turn a noncanonical address into an unintended
memory side effect.

The focused reproducer is:

```sh
cd XiangShan
build/memblock/picker/UT_MemBlock/build/UTMemBlock_example \
  --test vector-segment-noncanonical
```

Pre-fix failure from the historical revision:

```text
MEMBLOCK_VECTOR_SEGMENT_NONCANONICAL_FAIL cycle=43 phase=first-fault index=0
reason=mismatched vector memory metadata ... exception=0x0 ...
expected_exception=0x2000 address=0xffffff8040000000
actual_data=e0e1e2e3e4e5e6e7e8e9eaebecedeeef
expected_data=e0e1e2e3e4e5e6e7e8e9eaebecedeeef
```

The expected cause is `LoadPageFault` (`0x2000`); the DUT reports no exception.
The scenario covers both high-half and low-half noncanonical addresses and
contains a noncanonical segment-store control that must produce
`StorePageFault` with no store/SBuffer side effect.

Current fixed result:

```text
MEMBLOCK_VECTOR_SEGMENT_NONCANONICAL_PASS cycle=109 cases=2
high_low=1 vstart0_fault=1 fix_vl=1
```

## Oracle and Contract

A noncanonical virtual address must raise a page fault before any memory access.
For this path the vector segment load/store must:

- issue the DTLB request with the full VA and `checkfullva`;
- raise the exact `LoadPageFault` / `StorePageFault` at `vstart`;
- suppress the data/register writeback and any SBuffer/DCache/Uncache side
  effect.

That is the same architectural contract already checked for scalar
loads/stores by `translation-faults`.

## Root Cause

The failure was upstream of the DTLB canonical check. `VSegmentUnit` latched a
zero `instMicroOp.mask` for the first (and, for a one-uop segment, only) uop:

- `srcMask` was computed from the registered `instMicroOp.vl/uop.vpu.vm/
  uop.vpu.vstart`, not from the incoming `io.in.bits` fields;
- the first-fire `instMicroOpValid := true.B` was overridden by the
  `stateNext === s_idle` clear in the same cycle;
- therefore `instMicroOp.mask` stayed zero, and
  `segmentActive = (mask & UIntToOH(segmentIdx)).orR` was zero;
- `io.dtlb.req.valid` was gated by `segmentActive`, so the full VA and
  `checkfullva` were never sampled;
- the FSM skipped the TLB/cache path and wrote back the old destination data
  with `exception = 0`.

`vector-segment-canonical` did not expose this because it issues two uops per
segment; the second uop recomputes the mask from the now-latched fields. The
noncanonical reproducer issues one uop, so there is no second chance.

## Impact

A noncanonical vector segment address can alias a valid low address instead of
faulting. This violates the architectural page-fault requirement and can cause
wrong loads or stores. For a store, the data side effect could reach an
unintended valid address, making this a memory-safety and isolation issue.

## Fix

`d7d096e6e fix(memblock): latch first segment uop mask from incoming fields`

In `VSegmentUnit.scala`:

- build `firstUopMask` from `io.in.bits.src_vl`, `io.in.bits.uop.vpu.vm`,
  `io.in.bits.uop.vpu.vstart` and `io.in.bits.src_mask`;
- use it when `instMicroOpValid` is false;
- qualify the `stateNext === s_idle` clear with `!io.in.fire`, so the first-fire
  set is not overridden.

The fix makes the first uop visible to `segmentActive`, issues the DTLB request,
and propagates the canonical page fault to the vector writeback.

## Post-Fix Verification

- `vector-segment-noncanonical` passes with both high-half/low-half cases,
  `vstart0_fault=1` and `fix_vl=1`.
- `vector-segment-canonical` (12 cases), `vector-segment`, `vector-fof-high`
  and `translation-faults` (2568 cases) pass with the fix.
- The fix is included in the current verified benchmark/runtime and the
  scenario is part of the extra directed contracts.
- The default benchmark excludes this case with `benchmark_order: null`; it is
  run explicitly as an extra directed contract.

## Provenance

- Historical bug record:
  `git show e3e05a6d6:tests/memblock/docs/CPU_BUG_VECTOR_SEGMENT_NONCANONICAL.md`.
- Pre-fix RTL revision: `e3e05a6d6`.
- Fix: `d7d096e6e`.
- Current failure/pass evidence:
  `build/memblock/current-docs-validation/vector-segment-noncanonical.log`
  and `build/memblock/agent-campaign-3/extra-vector-segment-noncanonical.log`.
- The reproducer supplies the noncanonical address and expected fault; no DUT
  internal signal is used as an expected-value source.
