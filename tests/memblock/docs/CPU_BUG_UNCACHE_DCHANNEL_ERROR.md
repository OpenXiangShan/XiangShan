# CPU Bug: Uncache D-Channel Error Propagation

## Status

**Confirmed RTL bug, fixed in the current worktree.** This finding is separate
from the historical commit audit because the `LoadUnit.scala` fix is currently
an uncommitted source delta. The complete pre-fix generated RTL was recorded as
`0b6c6aa34dc8aa148d0f6da91091df58e6622415136e60baf985d7560824e7e3`; the
isolated regenerated NC-fix RTL was
`c97a89cd517e32cca52fc8f2dde38c539e9628209f69377c420d8fbcfb978084`.
After the later atomic exceptional-writeback fix, the current complete RTL is
`670cf5d399c55e40c9d51c70183315b4cdd730e73843543aec5789414558b846`.

## Reproducer

Run the deterministic reproducer from the MemBlock UT binary:

```sh
make -C tests/memblock uncache-errors
```

The test creates an Sv39 4-KiB PBMT=NC mapping, issues two scalar `ld`
transactions, and injects one error into each Uncache D-channel response. The
first transaction uses `denied=1`; the second uses `corrupt=1, denied=0`.
Request and response backpressure are enabled so the check also exercises the
ready/valid path.

On the pre-fix RTL the denied transaction failed as follows:

```text
MEMBLOCK_UNCACHE_ERRORS_FAIL cycle=134 phase=denied
reason=mismatched load writeback lane=1 rob=0 pdest=104 data=0x6b6a696867666564
exception=0x0 ... expected_exception=0x20
uncache_requests=1 dcache_requests=0
```

The data value is present, but the architectural load-access-fault bit is
missing from scalar writeback. The failure was reproduced on freshly generated
RTL, so it was not caused by a stale Verilated model or a stale port manifest.

The isolated NC-fixed RTL produced:

```text
MEMBLOCK_UNCACHE_ERRORS_PASS cycle=388 denied=1 corrupt=1 uncache_requests=2
rtl_sha256=c97a89cd517e32cca52fc8f2dde38c539e9628209f69377c420d8fbcfb978084
```

The same reproducer on the current complete RTL (which also includes the
atomic exceptional-writeback fix) produces the identical pass contract with
`rtl_sha256=670cf5d399c55e40c9d51c70183315b4cdd730e73843543aec5789414558b846`.

## Architectural Contract

For a legal Uncache load response:

- `denied=1` maps to `loadAccessFault`;
- `corrupt=1` with `denied=0` maps to `hardwareError`;
- the response remains associated with the requesting load identity;
- an exceptional scalar load must not request integer or FP register writeback;
- the request must use the Uncache path and must not create a cacheable DCache
  refill.

The independent C++ oracle checks all of these observable properties. It does
not accept a matching data value when the D-channel error flags or writeback
exception are wrong.

## Root Cause

`LoadQueueUncache.scala` correctly attaches the D-channel error to the
`LsPipelineBundle`:

```scala
io.ncOut.bits.uop.exceptionVec(hardwareError) := corrupt && !denied
io.ncOut.bits.uop.exceptionVec(loadAccessFault) := denied
```

The old `LoadUnit` S1 exception merge then replaced the incoming page/access
exception members with TLB-only values. PBMT=NC requests have no TLB query, but
they do enter the ordinary S1/S2 load flow; the false TLB result therefore
erased the response-generated bit before writeback. Because those exception
vector members were no longer consumed by the generated normal-TLB path,
FIRRTL also pruned the corresponding fields from the old flattened generated
boundary, making the loss visible in the top-level model.

MMIO is a distinct path in this RTL. `s0_mmio_fire` accepts the raw
`io.lsq.uncache` bundle without setting `s1_valid`; `s2_mmio_req` captures that
bundle with `RegNextN(..., 2)` and `s3_mmio_req` adds the final pipeline
register. Its exception metadata is therefore carried directly for the
three-cycle writeback and is not rewritten by the S1 TLB merge. The reproducer
and the functional bug are NC-specific; the explicit MMIO source assignments
are defensive and do not claim an MMIO failure.

This is a functional CPU bug: the external memory error is transformed into an
architecturally incorrect successful load result rather than an exception.

## Fix

The current [LoadUnit.scala](../../src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala)
change has two parts:

1. `fromMmioSource` and `fromNcSource` explicitly preserve the source
   `exceptionVec` members for address-misaligned, access-fault, page-fault,
   guest-page-fault, and hardware-error conditions.
2. The S1 merge ORs incoming exception members with the TLB response instead of
   assigning only the TLB response. TLB-generated faults retain their behavior,
   while NC response errors survive the stage. MMIO continues to use its
   independent three-cycle metadata path.

The RTL, port manifest, generated SVA, generated C++ defaults, and Verilated
model were regenerated after the source change. The generated MemBlock boundary
now retains the exception-vector fields needed by the NC writeback path and the
defensive MMIO source adapter.

## Validation

The focused test is implemented in
[`memblock_main.cpp`](../cpp/memblock_main.cpp) as `run_uncache_errors` and is
dispatched as `--test uncache-errors`. It checks denied and corrupt responses
separately, uses independent ROB/LQ identities, verifies scalar writeback
exception masks and RF suppression, and accounts for the automatic Uncache LQ
dequeue when present.

The separate `mmio-contracts` scenario passes one normal, one denied, and one
corrupt PBMT=IO load. It observes zero DCache requests, three Uncache requests,
and exact `isMMIO=1/isNCIO=0` plus exception metadata at scalar writeback.

The current regenerated RTL also passes:

- all deterministic translation, cache, queue, atomic, exception, and
  misalignment scenarios;
- the complete port/SVA checks and 98 Python unit tests;
- current 32,768-action and 65,536-action stress/mixed direct runs on the same
  complete RTL hash, with queue-conservation and backpressure gates passing.

## Scope and Remaining Boundary

This reproducer covers scalar NC load error propagation; the separate MMIO
scenario covers scalar PBMT=IO metadata and response errors. It does not yet
prove vector NC/MMIO error handling, a dedicated MMIO device-side-effect model, or malformed,
duplicate, early, or late external responses. Those remain explicit partial
items in `ORACLES.md` and `VERIFICATION_PLAN.md`; they must not be inferred from
this scalar pass.
