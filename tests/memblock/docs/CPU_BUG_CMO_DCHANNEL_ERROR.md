# CPU Bug: CMO D-Channel Error Propagation

## Status

**Confirmed RTL bug, fixed and dynamically validated.** The deterministic MemBlock
reproducer fails on complete generated RTL hash
`97b1339a74d458a48a1c58fad766a22cc9dac000cb297e303501311bf47d3b39`.
The source fix moves CMO error capture under the handshake of the response
channel that actually carries those errors. The fixed complete RTL hash is
`27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057`.

## Reproducer

Run the focused scenario:

```sh
make -C tests/memblock cmo-contracts
```

The scenario issues `CBO.CLEAN`, `CBO.FLUSH`, and `CBO.INVAL` through
StoreQueue and DCache, then injects both legal error forms on the custom
TileLink `CBOAck` response:

- `denied=1` must become `storeAccessFault`;
- `corrupt=1, denied=0` must become `hardwareError`.

On the pre-fix RTL, the first denied case failed with:

```text
MEMBLOCK_CMO_CONTRACTS_FAIL cycle=302 phase=clean-denied-execution
reason=mismatched store-address writeback lane=0 rob=0 rob_flag=0
exception=0x0 flush_pipe=1 expected_exception=0x80
```

The CMO completed and retained the required `flushPipe=1`, but its exception
vector was empty.

## Root Cause

`CMOUnit` correctly captures the TileLink D-channel `denied` and `corrupt`
members and presents them to StoreQueue on `io.cmoOpResp`. StoreQueue also
contained expressions for both CMO error bits, but they were nested inside:

```scala
when (io.uncache.resp.fire && !io.uncache.resp.bits.nc) {
  when (io.uncache.resp.bits.denied || io.cmoOpResp.bits.denied) { ... }
  when (io.uncache.resp.bits.corrupt || io.cmoOpResp.bits.corrupt) { ... }
}
```

CMO does not use the Uncache response channel. It waits for the independent
`io.cmoOpResp.fire` handshake, so the outer Uncache condition prevents both CMO
error expressions from executing. The later CMO state transition consequently
writes back the operation as successful.

## CPU Impact

This is an architectural functional bug. A lower-cache or interconnect denial
can mean the requested clean, flush, or invalidate did not complete. Reporting
success lets software continue as if the cache-block operation took effect,
without the intended store access fault. Independent corruption is similarly
lost instead of reporting `hardwareError`.

The consequence is not limited to stale internal bookkeeping. Software may
use CMO completion as an ordering and visibility boundary for DMA,
non-coherent agents, persistence, device ownership transfer, or cache-line
invalidation. If the lower hierarchy rejected the operation, subsequent
software or devices can observe stale data or an unexpected resident line.
The exact system-level manifestation depends on the manager that asserted the
error, but the missing architectural exception is directly ISA-visible.

## Fix

The StoreQueue response logic now handles each source under its own valid
handshake:

- Uncache `denied/corrupt` remains under `io.uncache.resp.fire`;
- CMO `denied/corrupt` is captured under `io.cmoOpResp.fire`, alongside the
  CMO response state transition;
- denied retains priority over corrupt, matching the existing Uncache and
  DCache error convention.

No CMO success-path state transition or Uncache behavior is changed.

## Validation Scope

The fixed RTL produces:

```text
MEMBLOCK_CMO_CONTRACTS_PASS operations=3 line_state_cases=6
dirty_probe_data=3 automatic_sbuffer_drains=3 retained_hits=2
invalidation_refills=4 flushed_younger_loads=1 concurrent_cycles=5214
denied_cases=3 corrupt_cases=3 positive_cycles=8167 error_cycles=2455
cmo_ack_delay=1024
rtl_sha256=27a5f512452d7e60401b611dd30c0b8316de81c4415d9bde4c058dc35ef2f057
```

The executable oracle also covers the success path before testing errors:

- dirty `CBO.CLEAN` returns exact `ProbeAckData`, downgrades to Branch, and
  permits a zero-refill resident read;
- dirty `CBO.FLUSH` returns exact `ProbeAckData`, invalidates the line, and
  requires a cold refill whose data includes the dirty writeback;
- dirty `CBO.INVAL` likewise returns exact data before invalidation, while the
  clean form returns a no-data `ProbeAck`;
- all three operations are also checked on a clean line, including CLEAN's
  no-data `BtoB` retention and FLUSH/INVAL's no-data `BtoN` invalidation;
- all three dirty operations begin with a committed store still buffered, use no
  direct testbench flush, and therefore check CMO-driven SBuffer draining;
- clean `CBO.INVAL` returns a no-data `ProbeAck`, invalidates the line, and
  requires a cold refill;
- every CMO uses line-aligned TileLink size 64 and fixed source 17, bypasses
  Uncache, waits behind SBuffer drain, and completes with `flushPipe=1`;
- a younger cold load occupies another MSHR while CBOAck is pending, then the
  CMO `flushAfter` cancels its LQ entry and suppresses its delayed writeback;
- all three operations are crossed with denied and independent-corrupt
  `CBOAck`, unchanged backing memory, exact exception class, and SQ
  conservation.

On the same regenerated model, `mmio-contracts`, `uncache-errors`,
`cbo-zero-contracts`, and `dcache-coherence` also pass. These neighboring
checks protect the shared StoreQueue state machine, Uncache response behavior,
CBO.ZERO's separate SBuffer path, and the Probe model used by the new oracle.

Malformed, duplicate, or unsolicited CMO responses and full-system effects
outside MemBlock remain separate verification items.
