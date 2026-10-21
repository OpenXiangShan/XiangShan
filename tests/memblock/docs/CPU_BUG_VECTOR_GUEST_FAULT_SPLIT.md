# CPU Bug: Vector Split Guest-Page-Fault GPA

## Status

Confirmed RTL bug, fixed by commit `f5b553973`. The focused contract remains
executable as `make -C tests/memblock vector-guest-fault-split`.

## Reproducer

The pre-fix RTL produced the failure below. Run the same contract on fixed RTL
with:

```sh
make -C tests/memblock vector-guest-fault-split
```

The deterministic pre-fix result is:

```text
MEMBLOCK_VECTOR_GUEST_FAULT_FAIL cycle=152 phase=exception-metadata
expected_vaddr=0x60000188 actual_vaddr=0x60000188
expected_gpaddr=0x94001800 actual_gpaddr=0x94001808
expected_vs_nonleaf=1 actual_vs_nonleaf=1
```

`vector-guest-fault` is the aligned vector control and passes with GPA
`0x94001800`. `scalar-guest-fault` uses fault VA `0x60000188` and also passes
with GPA `0x94001800`. All three use independently constructed Sv39/Sv39x4
tables and deterministic PTW backpressure.

## Independent Address Calculation

The VS L1 page-table page is at guest physical address `0x94001000`. For
guest VA `0x60000188`, VPN[1] is 256, so the PTE access that faults in G-stage
is:

```text
0x94001000 + 256 * 8 = 0x94001800
```

The G-stage maps the VS root page but intentionally does not map this next-level
VS page-table page. The architectural result must therefore identify the PTE
guest physical address, not a data-element address.

## CPU Impact

The stale `+8` reaches the externally visible guest-fault GPA together with the
VS-non-leaf marker. Guest fault handling can therefore receive a false physical
address for the page-table walk, even though the vector address and fault class
are correct.

## Root-Cause Localization

Before the fix, `src/main/scala/xiangshan/mem/vector/VMergeBuffer.scala`
computed:

```scala
val firstUnmask = genVFirstUnmask(selPort(0).mask).asUInt
val addrOffset = Mux(entryIsUS, firstUnmask, 0.U)
val vaddr = selVaddr + addrOffset
val gpaddr = selPort(0).gpaddr + addrOffset
```

For the split test, `firstUnmask` is 8. Adding it to `vaddr` selects the first
active vector byte correctly. Adding it to `gpaddr` is correct for an ordinary
guest data-access fault whose pipeline GPA is the base address, but incorrect
when `isForVSnonLeafPTE=1`: in that case the PTW already reports the exact
faulting PTE GPA. The observed error is therefore exactly `+8`.

The unconditional GPA offset was introduced by 2025 commit `5b35b0ec0746` to
fix ordinary unit-stride exception GPA calculation. A narrow candidate fix is
to retain that behavior except for a VS non-leaf PTE access:

```scala
val gpaddr = Mux(
  selPort(0).isForVSnonLeafPTE,
  selPort(0).gpaddr,
  selPort(0).gpaddr + addrOffset
)
```

## Fix and Validation

The narrow fix above landed as commit `f5b553973`. Pre-landing validation used
an isolated worktree with the independently reverted `fbb1e349` fix; that work
did not modify the canonical CPU tree.

At landing time, the repaired model produced:

```text
MEMBLOCK_VECTOR_GUEST_FAULT_PASS cycle=152 writebacks=1 vector_replays=0
ptw_requests=6 vaddr=0x60000188 gpaddr=0x94001800
```

Both controls still report the same independently calculated PTE GPA:

- scalar fault at VA `0x60000188`: GPA `0x94001800`, cycle 142;
- aligned vector fault at VA `0x60000180`: GPA `0x94001800`, cycle 152.

All 20 focused scenarios passed on the patched model, including scalar/vector
misalignment, translated cross-page stores, exceptions, DCache dirty release,
redirect, and queue pressure. An eight-seed matrix over all five random
scenarios passed 40/40 invocations and 17,344 completed transactions. A
separate duration-mode artifact passed the independent streaming verifier with
21 round-robin invocations and 1,184 completed transactions.

This validated the fix against the block-level suite at landing time. The
source contains the conditional GPA expression and the focused reproducer
remains available for regression.
