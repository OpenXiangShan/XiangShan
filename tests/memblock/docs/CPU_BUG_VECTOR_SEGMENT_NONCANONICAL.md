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

`VSegmentUnit.scala` computes the segment vaddr and truncates it at the DTLB
request:

```scala
io.dtlb.req.bits.vaddr := tlbReqVaddr(VAddrBits - 1, 0)
```

`VAddrBits` is narrower than the full 64-bit vaddr, so the noncanonical upper
bits disappear before the TLB/canonical check. The vector segment unit does not
perform the scalar LoadUnit's canonical check and does not set the page-fault
exception itself. The low 39-bit residue is a canonical address, so the TLB
successfully translates it and the vector load returns data.

The bug affects both segment loads and stores; the load reproducer is in the
tree, and the store path uses the same `tlbReqVaddr` truncation.

## Impact

A noncanonical vector segment address can alias a valid low address instead of
faulting. This violates the architectural page-fault requirement and can cause
wrong loads or stores. For a store, the data side effect can reach an
unintended valid address, making this a memory-safety and isolation issue.

## Proposed fix

1. Trace the DTLB `checkfullva`/`pregpf` result for this VSegmentUnit request
   and confirm whether the TLB raises the page fault for the full noncanonical
   address.
2. If the TLB raises it, fix the VSegmentUnit `s_wait_tlb_resp`/`s_pm`
   exception propagation so `exceptionVec` publishes the page fault with the
   exact full `exception_vaddr`, suppresses the vector data/SBuffer side
   effect, and preserves `vstart`.
3. If the TLB does not raise it, fix the fullva/checkfullva path (including
   the line-455 truncation) before the TLB. Do not treat the truncation alone
   as the root cause.

## Post-fix verification

After the RTL fix:

- `vector-segment-noncanonical` must pass for high-half and low-half addresses,
  including the fix-VL path;
- the noncanonical segment **store** control already in the reproducer must
  produce `StorePageFault`, no DCache/Uncache write, no store writeback, and
  unchanged backing memory;
- `translation-faults`, `vector-segment-canonical`, `vector-segment`, and
  `vector-fof-high` must still pass;
- re-run the directed benchmark and a multi-seed regression.

## Provenance

- RTL revision: the `VSegmentUnit.scala:455` truncation is present at the
  current XiangShan checkout (`218aa15b4` before this test commit; the RTL
  source is unchanged).
- Reproducer source:
  `tests/memblock/cpp/scenarios/vector_memory.inc`,
  `run_vector_segment_noncanonical`; catalog entry
  `vector-segment-noncanonical`.
- Observed logs: `/tmp/repro-vseg-noncanon-1.log` and
  `/tmp/repro-vseg-noncanon-2.log` (local runs; the deterministic command
  above is authoritative).
- The default benchmark excludes the failing scenario; `vector-segment`,
  `vector-segment-canonical`, `translation-faults`, and `vector-fof-high`
  remain green.
