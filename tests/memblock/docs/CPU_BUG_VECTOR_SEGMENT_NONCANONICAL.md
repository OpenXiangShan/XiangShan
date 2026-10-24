# CPU Bug: Vector Segment Noncanonical VA Does Not Fault

## Status

Confirmed RTL bug, open. The focused reproducer is
`make -C tests/memblock vector-segment-noncanonical` (or
`UTMemBlock_example --test vector-segment-noncanonical`). The scenario is
excluded from the default benchmark with `benchmark_order: null`.

## Summary

A vector segment load whose full Sv39 virtual address is noncanonical
(0xffffff8040000000: bit 38 differs from bits 63:39) completes without a
page fault and writes the input vector data. The matching scalar noncanonical
cases in `translation-faults` raise the expected page fault. The scalar and
the vector segment paths therefore disagree on the canonical-address contract.

The vector segment request sends both a truncated `vaddr` and the full
`fullva` with `checkfullva := true`:

```scala
io.dtlb.req.bits.vaddr       := tlbReqVaddr(VAddrBits - 1, 0)
io.dtlb.req.bits.fullva      := tlbReqVaddr
io.dtlb.req.bits.checkfullva := true.B
```

The line-455 truncation alone therefore does **not** explain the missing
fault: the DTLB receives the full address and its canonical check, and
`VSegmentUnit` imports the DTLB `excp(0).pf` result into
`exceptionVec(loadPageFault/storePageFault)`. The observed failure means the
DTLB `checkfullva`/`pregpf` path or the `VSegmentUnit`
`s_wait_tlb_resp`/`s_pm` exception propagation is not producing the page fault
for this vector segment request. The exact root remains under focused review;
the verified facts are the noncanonical vector segment load completing with
`exception=0x0` while the scalar noncanonical cases fault.

## Reproduction

From `XiangShan/tests/memblock`:

```sh
make vector-segment-noncanonical
```

Deterministic failure across repeated runs:

```text
MEMBLOCK_VECTOR_SEGMENT_NONCANONICAL_FAIL cycle=43 phase=first-fault index=0
  reason=mismatched vector memory metadata ... exception=0x0 ...
  expected_exception=0x2000 ... address=0xffffff8040000000
  actual_data=e0e1e2e3e4e5e6e7e8e9eaebecedeeef
  expected_data=e0e1e2e3e4e5e6e7e8e9eaebecedeeef
```

The expected cause is `LoadPageFault` (0x2000); the DUT reports no exception.
Two consecutive runs produce the same `exception=0x0` at the same phase.

Controls:

- `translation-faults` passes, including four noncanonical scalar load/store
  cases (Sv39/Sv48, both sign-bit directions);
- `vector-segment-canonical` passes its 12 canonical split cases;
- the ordinary `vector-segment` passes.

The high-half and low-half noncanonical addresses are both exercised by the
new scenario; the first high-half case already fails. The same scenario also
contains a noncanonical segment **store** control: it must require
`StorePageFault`, no store writeback, and no SBuffer/memory side effect before
its fix-VL completion. The load case fails first; the store control is the
post-fix guard for the shared `VSegmentUnit` truncation.

## Oracle and contract

A noncanonical virtual address must raise a page fault before any memory
access. This is the contract already checked for scalar loads/stores by
`translation-faults`: no DCache/Uncache request, no register write, exact fault
VA. The vector segment load/store must apply the same canonical check at
`vstart`, suppress the data side effect, and produce the exact fault VA. The
store path is expected to be affected by the same truncation; its post-fix
control is included in the verification plan.

## Root cause

The failure is upstream of the DTLB canonical check. `VSegmentUnit` latches a
zero `instMicroOp.mask` for the first (and, for a one-uop segment, only) uop:

- `VSegmentUnit.scala:377-380` computes `srcMask` from the registered
  `instMicroOp.vl/uop.vpu.vm/uop.vpu.vstart`, not from the incoming
  `io.in.bits`.
- `:387-394` sets `instMicroOpValid`/`instMicroOp` and also updates
  `instMicroOp.vl` in the same block.
- `:901-903` clears `instMicroOpValid` when `stateNext === s_idle`; at the
  first fire `enqPtr == deqPtr`, so the clear overrides the set. A one-uop
  segment has no second fire to repair the mask.
- `:215` computes `segmentActive = (mask & UIntToOH(segmentIdx)).orR`, which is
  zero.
- `:453` gates `io.dtlb.req.valid := state === s_tlb_req && segmentActive` to
  zero, so `fullva/checkfullva` is never sampled and no `pf39`/`pregpf`/`excp(0).pf`
  exists.
- `:285` then takes `s_tlb_req -> s_latch_and_merge_data` (or `s_send_data`),
  skips the cache, and writes back the old destination data with exception 0.

Decisive `mem_direct` trace from the private worktree:

```text
cycle=35 state=0 next=1 op_vl=2 op_vm=1 op_vstart=0 mask=0x0
cycle=37 state=3 next=9 op_vl=2 mask=0x0 req_valid=0
  tlbReqVaddr=0xffffff8040000000 pregpf=0x0 pf39=0x0
cycle=41 state=12 exc13=0 wb_exc13=0
```

`vector-segment-canonical` passes because it issues two uops per segment; the
second uop recomputes the mask from the now-latched `vl`/`vm`/`vstart`. The
noncanonical reproducer issues one uop, so the mask stays zero. This is the
complete explanation for `exception=0x0`; the line-455 truncation is not the
root (the fullva/checkfullva path is never reached).

The same root can affect any legal one-uop segment access: it bypasses the DTLB,
returns stale destination data, and suppresses the required page fault.

## Impact

A noncanonical vector segment address can alias a valid low address instead of
faulting. This violates the architectural page-fault requirement and can cause
wrong loads or stores. For a store, the data side effect can reach an
unintended valid address, making this a memory-safety and isolation issue.

## Proposed fix

1. In the first-uop path, compute `srcMask` from the incoming
   `io.in.bits.src_vl.asTypeOf(VConfig()).vl`,
   `io.in.bits.uop.vpu.vm`, `io.in.bits.uop.vpu.vstart`, and
   `io.in.bits.src_mask`, rather than from the registered zero
   `instMicroOp` fields.
2. Give the `instMicroOpValid := true.B` set at `:387` priority over the
   `stateNext === s_idle` clear at `:901-903` (move the clear above the set,
   or qualify it with `!io.in.fire`).
3. Optionally `RegInit` the `VSegmentBundle`; this alone is not sufficient
   because a stale zero `vl` still produces a zero mask.

The fix must be followed by a full RTL rebuild/model export because it changes
the `MemBlock` RTL; this is a legitimate RTL-fix rebuild, not a UT-only rebuild.

## Post-fix verification

After the RTL fix:

- `vector-segment-noncanonical` must issue the DTLB request (`req_valid=1`,
  `fullva=0xffffff8040000000`, `checkfullva=1`) and raise the expected
  `LoadPageFault`/`StorePageFault` with no data/SBuffer side effect;
- `vector-segment-canonical` (12 cases), `vector-segment`, `vector-fof-high`,
  `translation-faults` (2568 cases), and the directed benchmark must still pass;
- the one-uop segment path must be covered by a control that checks a normal
  mapped one-uop segment load/store, not only the fault case;
- re-run the constrained-random vector/segment directions and a multi-seed
  regression.

## Provenance

- RTL revision: `e3e05a6d6` (main checkout); the relevant
  `VSegmentUnit.scala` lines are unchanged.
- Reproducer source:
  `tests/memblock/cpp/scenarios/vector_memory.inc`,
  `run_vector_segment_noncanonical`; catalog entry
  `vector-segment-noncanonical`.
- Private debug worktree (no main RTL/Picker/runtime/harness change):
  `/home/xuyinan/paseo-workspace/kmhv2-ut-memblock-vseg-root`.
- Root-cause report:
  `/home/xuyinan/paseo-workspace/kmhv2-ut-memblock-vseg-root/VSEG_ROOT_CAUSE_REPORT.md`.
- `mem_direct` traces:
  `full-trace3.log`, `vseg-decisive-trace.txt` in the private worktree.
- Controls in the private build: `vector-segment-noncanonical` FAIL
  `exception=0x0`; `vector-segment-canonical` PASS 12 cases;
  `translation-faults` PASS 2568 cases including scalar noncanonical.
- The default benchmark excludes the failing scenario until the RTL fix.
