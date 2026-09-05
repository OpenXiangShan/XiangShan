# CPU Bug: Atomic Exception Writeback Must Not Enable `rfWen`

## Summary

`AtomicsUnit` propagated the input atomic uop's `rfWen` unchanged when the
atomic completed with an address or access exception.  For an exceptional
atomic writeback, the exception metadata was correct, but `uop.rfWen` stayed
high.  This is inconsistent with the scalar LoadUnit contract, which masks
`rfWen` whenever `exceptionVec` is nonzero, and can make the backend treat an
exceptional atomic as a register-writing completion.

## Reproduction

The MemBlock UT reproduces this with `make -C tests/memblock atomic-contracts`:

```text
MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle=602 phase=misaligned-d
  ... exception=0x40 ... rf_wen=1 ... expected_exception=0x40 expected_rf_wen=0
```

The stimulus is an 8-byte `AMOADD.D` at `address + 1`.  The test also checks
that no new DCache request is emitted.  The same contract is exercised for a
4-byte `AMOOR.W` at `address + 2`.

## Root cause

`AtomicsUnit.scala` drove `io.out.bits.uop := uop` and overwrote only the
exception vector.  Unlike `LoadUnit.scala`, it did not derive `rfWen` from the
absence of exceptions.  The issue affects alignment exceptions and applies to
other atomic TLB/PMP/cache-error paths that finish through the same output.

## Fix and regression

The output now drives `rfWen := uop.rfWen && !exceptionVec.asUInt.orR`.
`atomic-contracts` requires both misaligned D/W cases to produce the expected
exception, suppress integer writeback, and emit no DCache transaction.

On the current complete RTL, the contract passes as:

```text
MEMBLOCK_ATOMIC_CONTRACTS_PASS cycle=1216 amo_d_variants=9 amo_w_variants=9
amocas_variants=4 lr_sc=1 misaligned_d_offsets=7 misaligned_w_offsets=3
misaligned=10 final=0xfeedfacecafebeef
tilelink_requests=1
rtl_sha256=670cf5d399c55e40c9d51c70183315b4cdd730e73843543aec5789414558b846
```

The current UT executable for this result has SHA-256
`2eac0cce350f1a60aea68cda47baca18be6ce73d2d50912d1837a812128a2821`.

## Scope

This is separate from the previously documented PBMT=NC Uncache exception-loss
bug in `CPU_BUG_UNCACHE_DCHANNEL_ERROR.md`.  It does not change the atomic memory
operation result or reservation behavior; it fixes only exceptional writeback
metadata.
