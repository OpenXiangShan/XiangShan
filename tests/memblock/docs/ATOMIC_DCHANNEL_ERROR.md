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

The deterministic contract performs four checks:

1. Inject `denied=1` on an `AMOADD.D` miss. The writeback must contain
   `storeAccessFault`, suppress `rfWen`, and leave no installed cache line.
2. Inject `corrupt=1` on another `AMOADD.D` miss. The writeback must contain
   `hardwareError`; the cache may retain the refill only with its D-channel
   error metadata and without applying the AMO operation.
3. Issue a second `AMOADD.D` to the corrupt line with error reporting enabled.
   It must hit without a new TileLink request, report `hardwareError`, suppress
   `rfWen`, and perform no atomic state change.
4. Disable generic cache-error reporting and read both addresses. The denied
   address must miss and fetch the pre-operation value. The corrupt address
   must hit without a new TileLink request and re-report `hardwareError` because
   LoadUnit classifies delayed TileLink errors independently of that CSR bit.
   A diagnostic-only scoreboard option also checks the otherwise
   non-architectural writeback data field against the original refill, proving
   that neither AMO modified it.

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

- LR reports `loadAccessFault`; AMO/SC reports `storeAccessFault`;
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
MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_PASS cycle=437 denied_amo=1 corrupt_amo=1 corrupt_hit_amo=1 rtl_sha256=b69e387eb081a3f311311079ade435206817c7c6a20bd8f3a5f11889ec1dcbf4
```

The neighboring `atomic-contracts`, `dcache-errors`, `uncache-errors`,
`mmio-contracts`, `cbo-zero-contracts`, and `reset-recovery` scenarios also
passed on that hash. Two 65,536-action constrained-random runs subsequently
completed 3,288,307 combined DUT cycles without a scoreboard, assertion,
timeout, or queue-accounting failure.

## Scope and Remaining Coverage

The regression directly covers denied and corrupt `AMOADD.D` misses plus a
corrupt-line `AMOADD.D` hit. The shared MainPipe gates also protect LR, SC, and
AMOCAS, but their complete denied/corrupt matrix and explicit failed-reservation
checks remain verification work and are listed as gaps in the verification
plan.
