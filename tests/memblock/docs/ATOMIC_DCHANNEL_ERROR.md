# Atomic D-Channel Error-Line Contract

## Status

**Not an RTL bug. The original report was a UT oracle error and has been
retracted.**

The design contract was clarified after review: when an atomic miss receives a
denied or corrupt D-channel response, MainPipe installs the resulting cache line
and writes the corresponding `DCacheExtraMeta.error` bits. A later load may hit
that poisoned line without issuing another TileLink request and must report the
stored error again. For denied metadata that exception is `loadAccessFault`; for
corrupt-only metadata it is `hardwareError`.

The local RTL change `8eedb3ad0` incorrectly changed this policy by rejecting a
denied refill and suppressing atomic cache updates on response errors. It was
reverted in the standalone commit `db6f6d844`. The independent fix
`e1424686a`, which suppresses register-file write enable on any exceptional
atomic writeback, remains valid.

## Correct Contract

For a cold LR, AMO, or AMOCAS miss whose response is denied:

- LR reports `loadAccessFault`; AMO and AMOCAS report `storeAccessFault`;
- the exceptional writeback has `rfWen=0`;
- MainPipe installs the line and records `tl_denied` in its extra metadata;
- a later scalar load hits the line, issues no new TileLink request, and reports
  `loadAccessFault` with `rfWen=0`.

For a corrupt-only response:

- the atomic operation reports `hardwareError` with `rfWen=0`;
- MainPipe installs the line and records `tl_corrupt`;
- a later scalar load hits without a new TileLink request and reports
  `hardwareError` with `rfWen=0`.

The data carried by an exceptional writeback is not architectural and is not a
valid value oracle. In particular, observing the AMO result in the data array of
a line that is also marked denied does not establish an architectural memory
side effect: the checked load observation faults instead of returning a usable
value. The UT
therefore checks exception class, register-write suppression, line-hit request
count, and subsequent clean recovery, but deliberately does not compare data on
an exceptional readback.

SC has no cold-miss D-channel response path in this implementation. If the line
or usable reservation is absent, MainPipe returns SC failure before issuing a
TileLink request. The executable test instead issues SC.W/SC.D after delayed
readback of the poisoned LR lines and checks that the cache hit reports the
stored denied/corrupt error without traffic or a register write. The delay also
allows the reservation timer to expire, so this is an error-metadata check and
does not claim to observe internal reservation creation.

## RTL Basis

The behavior follows directly from the existing cache pipeline:

- `DCacheExtraMeta.error` is documented as marking a cache line denied or
  corrupted;
- MainPipe treats every miss as a metadata and data update and writes
  `s3_l2_error_wb` through `error_flag_write`;
- LoadPipe returns the stored `tl_denied`/`tl_corrupt` bits as
  `tl_error_delayed` on a later hit;
- LoadUnit maps those delayed bits to `loadAccessFault` or `hardwareError`.

This is distinct from the historical AtomicsUnit error-lifetime bug fixed by
`7a25d9c9d`. That bug retained an old exception bit inside AtomicsUnit and made
a later clean AMO fail. The current regression performs a clean cold
`AMOSWAP.D` and a warm scalar readback after each complete denied/corrupt batch,
which directly checks that operation-local error state does not leak into a new
line.

## Why the Original Oracle Was Wrong

The first version correctly observed that the faulting AMO's data appeared in
the installed line and that the next load did not refetch. It then assumed:

- denied refill data must never be installed;
- the later load must issue a new request and return clean data;
- exceptional writeback data was architecturally meaningful.

Those assumptions conflict with the intentional poisoned-line policy. The
later `loadAccessFault` was not stale AtomicsUnit state; it came from the error
metadata attached to the cache line. Changing RTL to make the old oracle pass
would discard that policy and hide the error from later accesses.

## Executable Coverage

Run:

```sh
make -C tests/memblock atomic-dchannel-errors
```

The deterministic test crosses denied and corrupt with every refill-capable W
and D atomic encoding: LR, AMOSWAP, AMOADD, AMOXOR, AMOAND, AMOOR, AMOMIN,
AMOMAX, AMOMINU, AMOMAXU, and AMOCAS. These 22 operations produce 44 independent
cold-miss error cases under randomized request/response backpressure.

Each case checks the initial atomic exception and suppressed `rfWen`, followed
by a same-line scalar hit with the expected persistent exception and no new
TileLink request. LR.W/LR.D additionally lead to SC.W/SC.D poisoned-line checks.
Two clean recovery sequences cover AtomicsUnit error lifetime.

The regenerated unmodified MainPipe RTL passed on complete ordered RTL SHA-256
`774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`:

```text
MEMBLOCK_ATOMIC_DCHANNEL_ERRORS_PASS cycle=7108 denied_cases=22 corrupt_cases=22 readbacks=44 denied_line_hits=22 corrupt_line_hits=22 sc_denied_hit_checks=2 sc_corrupt_hit_checks=2 clean_recoveries=2 tilelink_requests=46 rtl_sha256=774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9
```

The 46 requests are exactly 44 error-producing cold misses plus two clean
recovery misses. All 44 exceptional readbacks and all four SC checks are cache
hits and add no request.

## Remaining Coverage

The regression covers the complete legal refill-error encoding matrix visible
at this boundary and clean error-state recovery. It does not prove the contents
of internal reservation state, manager-originated probe invalidation, physical
tag/data-array ECC behavior, cross-hart reservation interference, or ordering
against concurrent memory traffic. Those remain separate verification points.
