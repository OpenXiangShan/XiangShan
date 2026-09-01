# BIN-814 BPU S3 Flush Reachability Review

Date: 2026-09-02

## Scope And Provenance

This review is limited to BIN-814 on the current clean Verilator DUT:

- implementation and DUT source: `1a32a9056d993233fa1bf3a394b16e8a762abf52`
- design baseline: `e5c70547f3a966accf20a4b065ec1d8e33443180`
- build manifest SHA-256:
  `d2fbabf8640ae4033b2c4ddfebabdcc76e9b7c66709ae234eb7eb6edd4e6e2fa`
- DUT build SHA-256:
  `3269730cddcd752d4e9dff6cad3c7d4f4248ff14de510c66b613f5a271cfa395`
- generated RTL SHA-256:
  `e59a3a2a795f053d6d57138fa23aa08545c7c4fccb20dfdb3cde79944627c9a4`
- signal contract SHA-256:
  `0d285e37d4e3dd814981b3bb5ccbdb7bc7708310750cf6a1f31b6853536be831`

The manifest establishes build identity only. The reachability conclusion
below is independently supported by source RTL, generated RTL, signal
inventory, and current-DUT diagnostics.

## Required Event

BIN-814 currently requires IFU to observe a valid aggregate ICache response
whose block-0 FTQ identity is matched by a BPU stage-3 flush. The producer then
requires `req_valid=1`, `s0_flushFromBpu=1`, `s0_flush=1`, and `s0_fire=0` in
the same IFU sampling cycle. Missing or zero-valued probes are not accepted as
substitutes for this event.

## RTL Reachability

`ICacheMainPipe.scala` computes:

```text
s1_flush := io.flush || io.flushFromBpu.shouldFlushByStage3(s1_ftqIdx, s1_valid)
io.toIfu.req.valid := s1_valid && s1_fetchFinish && !s1_flush
```

`Ifu.scala` then defines `s0_valid` directly from
`io.fromICache.req.valid` and qualifies `s0_flushFromBpu` with that validity.
Therefore, when the BPU stage-3 flush matches the ICache MainPipe transaction,
`s1_flush=1` suppresses the response valid before it reaches IFU. IFU cannot
simultaneously observe the BIN-814 `req_valid=1 && s0_flushFromBpu=1`
condition on this path.

Generated `ICacheMainPipe.sv` independently preserves the exclusion:

```text
assign io_toIfu_req_valid = _s1_fire_T & ~s1_flush;
```

All BIN-814 observation groups exist in the current signal inventory. The
result is thus an RTL reachability issue, not a missing-probe, alias, or
sampling-default issue.

## Current-DUT Diagnostics

Three diagnostics were retained under `/tmp` and are not promotion evidence:

- `/tmp/ctrl-bin814-bpu-nonmatch-current` observed a younger non-matching BPU
  flush and passed the existing non-match canary. It correctly did not hit
  BIN-814.
- `/tmp/ctrl-bin814-pulse-current-01` used an all-off/all-on predictor pulse.
  The exact target remained unhit and the run is rejected as evidence.
- `/tmp/ctrl-bin814-stage1off-current-01` disabled stage-1 predictors while
  leaving stage-3 predictors trained. It produced repeatable real stage-3
  overrides, but the smallest observed BPU lead was two FTQ entries. The exact
  target remained unhit and the pytest/checker outcome is rejected.

No sampler condition was weakened and no HIT was manufactured from these
observations.

## Disposition

BIN-814 changes from `MODELED` to `BLOCKED` for this exact RTL and DUT
baseline. Closure requires a design/testpoint disposition or an RTL change
that makes the requested IFU-side conjunction observable. The defensive
runtime producer may remain so a future RTL change is detected, but it cannot
support a current-baseline HIT.

The strict and global denominators remain `343` and `879`. HIT count remains
`302`; the status split becomes
`302 HIT / 37 MODELED / 1 PARTIAL / 3 BLOCKED` (`88.05% HIT`).
