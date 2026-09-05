# Constrained-Random Command Replay Validation Bug

## Classification

This was a UT offline-verifier bug, not an XiangShan RTL bug. It was found on
2026-09-05 while verifying the frozen minimum-budget `spec` run.

## Symptom

The simulator and regression controller passed a `random-mixed` run using
`--constraints spec`, but `verify_regression.py` rejected the valid artifact
with `command does not replay its recorded case`.

## Root Cause And Fix

The verifier still required the historical fixed command tail ending at
`--transactions`. The unified constraint interface legitimately appends a
profile, zero or more overrides, and optional backpressure or boundary-hunt
flags.

The verifier now checks the invariant seed, scenario, and transaction prefix
while streaming each result, retains only the distinct option tuples, and then
requires those tuples to exactly match the top-level recorded configuration.
This accepts reproducible constrained commands without allowing unrecorded
options to escape artifact validation. Historical artifacts without constraint
metadata remain valid.

## Regression Guard

A unit test accepts a matching `spec` profile plus `tlb-flush=40` override and
rejects a per-result override that differs from the campaign configuration.
The frozen 256-action `spec` artifact is also checked end to end.
