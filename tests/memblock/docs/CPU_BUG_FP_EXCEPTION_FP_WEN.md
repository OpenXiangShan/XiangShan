# CPU Bug: Exceptional FP Load Must Not Enable `fpWen`

## Summary

`LoadUnit` suppressed `rfWen` on an exceptional scalar load writeback but
propagated the decoded `fpWen` unchanged. An FLW or FLD that completed with a
page, access, or hardware exception could therefore assert `fpWen` together
with a nonzero exception vector.

This is an RTL defect, not an MMIO-model artifact. The backend copies the
MemBlock writeback's `uop.fpWen` into its execution output, and
`WbArbiter.scala` uses that bit directly to select the floating-point physical
register write port. It does not add an exception-vector gate.

## Reproduction

The MemBlock UT reproduces the issue with a PBMT=IO FLW whose Uncache response
has TileLink `denied` asserted:

```sh
make -C tests/memblock fp-loads
```

The unmodified RTL, with complete RTL SHA-256
`774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`,
produced:

```text
MEMBLOCK_FP_LOADS_FAIL cycle=217 phase=mmio-completion
reason=mismatched load writeback lane=2 rob=2 pdest=23
data=0xffffffffacabaaa9 exception=0x20 replay=0 rf_wen=0 fp_wen=1
expected_exception=0x20 expected_rf_wen=0 expected_fp_wen=0
mmio=1 ncio=0 perf_cnt=0 dcache_requests=0 uncache_requests=3
```

The exception (`LoadAccessFault`) and MMIO metadata were correct. The only
architectural-side-effect mismatch was `fpWen=1`.

## Root Cause

At the final scalar load output in
`src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala`, the two register-file
enables were handled inconsistently:

```scala
io.ldout.bits.uop.rfWen := s3_rfWen && !io.ldout.bits.uop.exceptionVec.asUInt.orR
io.ldout.bits.uop.fpWen := s3_fpWen
```

Both ordinary DCache completions and the direct three-cycle MMIO metadata path
merge at this output, so the missing gate is not specific to TLB behavior. The
MMIO test made it directly observable by injecting a late Uncache D-channel
error after the request had already been classified and issued.

The downstream floating-point writeback arbiter accepts an execution result
when `in.valid && in.bits.fpWen`; it does not suppress that request when the
exception vector is nonzero. Thus the stale enable can write and wake a
speculative FP physical destination even though the instruction traps.

## Fix

Apply the same final exception-vector gate to `fpWen` that already protects
`rfWen`:

```scala
io.ldout.bits.uop.fpWen :=
  s3_fpWen && !io.ldout.bits.uop.exceptionVec.asUInt.orR
```

The change is deliberately confined to the final scalar load output. It does
not alter the three-cycle MMIO metadata pipeline, exception construction,
normal FLH/FLW/FLD NaN-boxing, or vector writeback behavior.

## Regression Contract

`fp-loads` covers normal cacheable FLH/FLW/FLD plus PBMT=IO FLH/FLW/FLD. The
MMIO portion crosses normal, denied, and corrupt responses and requires exact
FP destination data for successful operations. An independent empty-page-table
FLW covers an early translation exception. Every exceptional operation must
retain its exact exception and metadata while deasserting both `rfWen` and
`fpWen`; every MMIO operation must bypass DCache.

After full DefaultConfig re-elaboration, the repaired complete RTL SHA-256 is
`4d3f33202176692516f83069c08568f7efa46d466699504851961d4ccd6218e4`.
The focused regression passes as:

```text
MEMBLOCK_FP_LOADS_PASS cycle=895 writebacks=9 fp_destinations=3
mmio_fp=5 mmio_flh=1 mmio_flw=2 mmio_fld=2 mmio_faults=2 page_faults=1
mmio_dcache_requests=0 mmio_uncache_requests=5
rtl_sha256=4d3f33202176692516f83069c08568f7efa46d466699504851961d4ccd6218e4
```
