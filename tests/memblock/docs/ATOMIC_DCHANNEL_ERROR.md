# Atomic D-Channel Errors Must Not Apply Atomic Side Effects

## Status

**Confirmed RTL bug; fixed and covered by a deterministic regression.**

The RTL fix is commit `8eedb3ad0`. The exceptional atomic register-write fix
used by the same regression is the independent commit `e1424686a`.

A denied D-channel response for an AMO was reported as an atomic access fault,
but MainPipe still wrote the AMO result into the DCache. A later load hit that
line and observed the AMO operand rather than the pre-operation value. The same
hit also exposed the denied response's stale error metadata.

## Reproducer

Run:

```sh
make -C tests/memblock atomic-dchannel-errors
```

The deterministic contract crosses both D-channel error kinds with every
refill-capable atomic encoding at W and D width: LR, AMOSWAP, AMOADD, AMOXOR,
AMOAND, AMOOR, AMOMIN, AMOMAX, AMOMINU, AMOMAXU, and AMOCAS. These 22
operations produce 44 independent cold-miss error cases under randomized
request/response backpressure.

For each denied case, the atomic writeback must report `loadAccessFault` for LR
or `storeAccessFault` for AMO/AMOCAS, suppress `rfWen`, and leave no installed
line. A legal scalar readback must issue exactly one new TileLink request and
return the original manager bytes. For each corrupt case, the writeback must
report `hardwareError` and suppress `rfWen`; the scalar readback must hit
without a request, re-report `hardwareError` even after generic cache-error
reporting is disabled, and expose only the unmodified refill through the
diagnostic exceptional-data oracle.

SC has a separate reachable-path contract. MainPipe immediately returns SC
failure when either the reservation or cache hit is absent, so SC cannot
receive a cold-miss D-channel response. The test therefore requires a denied
LR.W/LR.D not to establish a reservation (the following SC returns `1` without
traffic), and requires SC.W/SC.D on the cached line left by a corrupt LR to
report `hardwareError` without traffic or a register write.

The original failing observation was:

```text
MEMBLOCK_ATOMIC_CONTRACTS_DEBUG phase=dcache-error-drain cycle=1319
pending_dcache_responses=0 tilelink_requests=3
MEMBLOCK_ATOMIC_CONTRACTS_FAIL cycle=1341 phase=dcache-error-readback
reason=mismatched load writeback lane=0 rob=73 pdest=161
data=0x102030405060708 exception=0x20
expected_exception=0x0 expected_data=0x0
```

The returned value is exactly the denied AMO operand. The readback did not
increase the DCache request count, proving this was a cache hit rather than a
late external response.

## Architectural Contract

For a denied atomic D-channel response:

- LR reports `loadAccessFault`; AMO/AMOCAS reports `storeAccessFault`;
- integer register writeback is suppressed;
- no data, tag, coherence, replacement, access, or prefetch state is installed;
- a later legal access refetches and observes the pre-operation value.

For a corrupt atomic D-channel response:

- the operation reports `hardwareError` and suppresses register writeback;
- the existing DCache policy may install the raw refill with corrupt metadata;
- no AMO/CAS/SC update or LR reservation may be created from corrupt data;
- a later hit re-reports the delayed TileLink error under the current LoadUnit
  contract, even when generic cache-error reporting is disabled;
- an implementation-level diagnostic may inspect the exceptional writeback's
  non-architectural data field to prove that it contains only the unmodified
  refill value.

SC reports its ordinary success/failure result only on a cache hit with a live
reservation. It has no cold-miss response-error case at this boundary. Cached
corrupt metadata is still architectural error input to SC and must produce
`hardwareError` with no state change.

## Root Cause

MainPipe retained `tl_denied` and `tl_corrupt` for the atomic response and the
extra metadata array, but its cache-update controls did not consume those bits.
On every miss, `update_data` selected a full-line write. For AMOs the data mux
then selected `s3_amo_data_merged_reg`, even when the refill had been denied or
corrupt. LR reservation and atomic hit update controls likewise depended only
on hit/miss and operation type, not the response error.

The error metadata itself is intentional. Commit `026615fc2` introduced it so
later accesses to a corrupt cache line continue to report the L2 error. The fix
therefore must not discard corrupt lines indiscriminately.

## Fix

`MainPipe.scala` now separates three decisions:

- `s3_refill_denied_wb` blocks all cache-line installation for denied refills;
- an errored refill data write selects `s3_store_data_merged`, which is the raw
  refill for atomics, while corrupt metadata is retained;
- `s3_error_wb` gates AMO writes, coherence updates, and LR reservation state,
  covering both a newly errored refill and a hit on an already marked line.

The atomic response path is unchanged, so `AtomicsUnit` still maps denied to
the appropriate access fault and corrupt-only to `hardwareError`. Its separate
exceptional-`rfWen` fix guarantees that neither response commits a register
write.

## Fixed-RTL Evidence

The Picker model was rebuilt from the repaired source. The focused reproducer
then passed on complete ordered RTL SHA-256
`b69e387eb081a3f311311079ade435206817c7c6a20bd8f3a5f11889ec1dcbf4`:

```text
MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_PASS cycle=5019 denied_cases=22 corrupt_cases=22 readbacks=44 lr_reservation_checks=2 sc_corrupt_hit_checks=2 tilelink_requests=66 rtl_sha256=b69e387eb081a3f311311079ade435206817c7c6a20bd8f3a5f11889ec1dcbf4
```

The neighboring `atomic-contracts`, `dcache-errors`, `uncache-errors`,
`mmio-contracts`, `cbo-zero-contracts`, and `reset-recovery` scenarios also
passed on that hash. Two 65,536-action constrained-random runs subsequently
completed 3,288,307 combined DUT cycles without a scoreboard, assertion,
timeout, or queue-accounting failure.

## Scope and Remaining Coverage

The regression now covers the complete legal refill-error encoding matrix for
W/D LR, AMO ALU, and AMOCAS operations, plus both reachable SC error-adjacent
paths. Remaining atomic work is cross-hart reservation interference, ordering
with concurrent memory traffic, the full operation-by-misaligned-offset cross,
and physical tag/data-array ECC injection. Manager-originated probes and their
reservation invalidation behavior also remain outside this MemBlock manager
model.
