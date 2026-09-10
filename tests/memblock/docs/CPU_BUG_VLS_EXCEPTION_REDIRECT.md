# CPU Bug: Stale VLS Exception Address After Redirect

## Status

**Confirmed RTL bug, fixed and focused-test validated on the current branch.**
The pre-fix generated MemBlock has complete RTL SHA-256
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`;
the clean post-fix RTL has
`356025f4a7472e978800120eee3f0b78832939b9c3b6b6ad07fc2f2b32fb60d3`.
Commit `3abfd81e6` contains the initial reproducer. The reusable oracle is the
subsequent version described below: it does not sample an unqualified address
payload between identified exception epochs. The RTL fix is the separate
commit `4b976f156`, patch-equivalent to upstream PR
[#6548](https://github.com/OpenXiangShan/XiangShan/pull/6548) at
`f67f9e78f9d1a758fe599fb73080cc8ab2c9a357`.

The upstream fix commit also references issues
[#6399](https://github.com/OpenXiangShan/XiangShan/issues/6399),
[#6482](https://github.com/OpenXiangShan/XiangShan/issues/6482), and
[#6540](https://github.com/OpenXiangShan/XiangShan/issues/6540). This report
does not infer additional symptoms from those issue links.

## Reproducer

The focused load-direction reproducer is:

```sh
make -C tests/memblock vector-fof JOBS=8
```

It issues an unmapped first-element VLEFF load, checks its identified
page-fault writeback and exact fault VA, completes the fix-VL uop, and drives
the ROB contract at the MemBlock boundary:

```text
redirect.valid=1
redirect.level=flush
redirect.isVlsException=1
redirect.robIdx=<faulting vector uop>
```

After the VLS redirect, the test deliberately ignores the unqualified address
payload until it issues a later, identity-qualified scalar load page fault at
a different VA. The pre-fix RTL reports:

```text
MEMBLOCK_VECTOR_FOF_FAIL cycle=134
phase=subsequent-scalar-fault-address
expected_vaddr=0x53003000 actual_vaddr=0x53000180
```

The independent store-direction reproducer is part of:

```sh
make -C tests/memblock trigger-contracts JOBS=8
```

It issues a vector-store breakpoint, drives the same authentic VLS redirect,
then issues a scalar-store breakpoint at another address. The pre-fix RTL
reports:

```text
MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle=100
phase=post-vector-store-fault-address
expected_vaddr=0x80630180 actual_vaddr=0x80630020
```

Both sequences use only top-level transactions and externally visible
writeback/exception-address behavior. Picker `mem_direct` reads may localize
the retained ExceptionBuffer entry, but no internal signal decides the pass or
fail result.

## Oracle And Architectural Impact

The exported exception-address payload has no valid bit or transaction
identity, so it is not a sound oracle in unconstrained free-running mixed
traffic. The focused oracle samples it only when an exact ROB/writeback
identity establishes which fault Backend is consuming. A matching VLS
redirect then delimits that exception epoch. The test makes no assertion about
the payload after the redirect and before the next identified fault; it only
requires that the later same-direction fault publish its own exact VA. There
is no fixed-cycle retention requirement in this oracle.

The architectural propagation is direct:

1. The load/store ExceptionBuffer selection reaches
   [`LSQWrapper.scala`](../../../src/main/scala/xiangshan/mem/lsqueue/LSQWrapper.scala#L253)
   and the MemBlock exception-address mux in
   [`MemBlock.scala`](../../../src/main/scala/xiangshan/mem/MemBlock.scala#L1917).
2. `XSCore` wires the MemBlock VA/GPA/non-leaf marker to Backend in
   [`XSCore.scala`](../../../src/main/scala/xiangshan/XSCore.scala#L183), and
   Backend forwards them to CSR in
   [`Backend.scala`](../../../src/main/scala/xiangshan/backend/Backend.scala#L617).
3. The CSR wrapper and NewCSR trap input retain that path in
   [`wrapper/CSR.scala`](../../../src/main/scala/xiangshan/backend/fu/wrapper/CSR.scala#L125)
   and
   [`NewCSR.scala`](../../../src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala#L849).
4. Trap delegation selects M, HS, or VS entry in
   [`NewCSR.scala`](../../../src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala#L802).
   A memory fault or memory breakpoint selects the propagated VA for
   [`mtval`](../../../src/main/scala/xiangshan/backend/fu/NewCSR/CSREvents/TrapEntryMEvent.scala#L88),
   [`stval`](../../../src/main/scala/xiangshan/backend/fu/NewCSR/CSREvents/TrapEntryHSEvent.scala#L90),
   or
   [`vstval`](../../../src/main/scala/xiangshan/backend/fu/NewCSR/CSREvents/TrapEntryVSEvent.scala#L92).

The two dynamic reproducers establish stale load- and store-direction VAs.
Static propagation also shows that a retained guest-fault record can carry a
stale GPA and `isForVSnonLeafPTE` marker into `mtval2`/`htval` and the
`mtinst`/`htinst` pseudo-instruction marker. Those guest-metadata effects are
plausible consequences of the same record but have not been dynamically
reproduced by this UT, so they are not part of the confirmed symptom.

## Root Cause

Historical fix `222f993ed` added `isVlsException` and changed
[`MemBlock.scala`](../../../src/main/scala/xiangshan/mem/MemBlock.scala) to rewrite
every VLS exception redirect from `flush` to `flushAfter` before distributing
it through MemBlock:

```scala
val redirectModifyLevel = WireInit(io.redirect)
redirectModifyLevel.bits.level := Mux(
  io.redirect.bits.isVlsException,
  RedirectLevel.flushAfter,
  io.redirect.bits.level
)
val redirect = RegNextWithEnable(redirectModifyLevel)
```

The conversion is required for StoreQueue recovery and StoreMisalignBuffer so
the faulting vector store is not killed before its residual side effects can
drain. It is incorrect for the load and store ExceptionBuffers. Their
`needFlush` tests see `flushAfter`, retain the faulting entry as the oldest
record, and that retained record continues to win oldest selection over
subsequent same-direction faults.

## Fix

The fix keeps two views of the same registered redirect:

- `rawRedirect` preserves the ROB's original `flush` level and is sent to the
  ExceptionBuffers and ordinary MemBlock consumers;
- `flushAfterRedirect` is a combinational view that changes only a VLS
  exception to `flushAfter` and is sent to StoreQueue recovery and
  StoreMisalignBuffer.

`LSQWrapper` therefore carries both views, and `StoreQueue` gains a dedicated
raw `exceptionRedirect` input for its ExceptionBuffer. This preserves the
vector-store drain contract without allowing stale exception-address records
to survive into the next trap.

## Validation

The pre-fix failures above are deterministic on a freshly compiled harness and
the recorded generated RTL identity. Post-fix validation used newly generated
XiangShan split RTL and a clean Picker model, not the pre-fix artifact:

- Picker: `794e2d9085cf7eae31638119d15a976558ba9490`
- xcomm: `29c290bb1f14fa2a4a72c01ab746a10cff504b2c`
- frozen binary: `f2b2d160bb5a4820ca81cea0babac10d775739ec2bf2dc1c4eb0132b7804f330`
- frozen model: `101de148c6912a0138b6471d51e3329afbed0c7306ebc12c6df35619f4a1b02d`
- frozen xspcomm: `1ed4e5013658fa557c6c055afa6242d272377d2a3564b18a0a31bf4147ae6f52`

The schema-2 focused artifact `build/memblock/vls-postfix.json` has SHA-256
`e2a84f4422adfe8ff77da91dc418902a5662d9bec1370d4dd9d1c0333dc52c69`.
It verifies unchanged runtime/controller hashes and records all three passes:

```text
MEMBLOCK_VECTOR_FOF_PASS cycle=137 ... first_fault_vls_redirects=1 subsequent_scalar_faults=1
MEMBLOCK_VECTOR_SEGMENT_FOF_PASS cycle=304 ... first_fault_vls_redirects=1
MEMBLOCK_TRIGGER_CONTRACTS_PASS cycle=1521 ... vector_stores=4
```

These tests validate stale-address removal in both load and store directions.
They do not prove the original residual vector-store drain behavior protected
by historical commit `222f993ed`; a legal store-progress/memory-effect mutation
that distinguishes a revert of that historical fix remains a verification
gap. Multi-seed `random-mixed --constraints spec` is complementary regression
evidence, not a replacement for that missing mutation oracle.
