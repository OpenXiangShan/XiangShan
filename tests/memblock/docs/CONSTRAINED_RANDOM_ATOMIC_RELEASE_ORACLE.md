# Constrained-Random Atomic Release Oracle Bug

## Classification

This was a UT framework bug, not an XiangShan RTL bug. It was found on
2026-09-05 after adding AMOs to the common `random-mixed` constrained tail.

## Reproducer

Two independent directions failed on the same dirty atomic line:

```text
random-mixed --seed 31 --transactions 16384 --constraints spec
random-mixed --seed 37 --transactions 4096 --constraints corner
```

Both reported a byte mismatch when DCache evicted line `0x802a0100` through
TileLink ReleaseData. The first run reached action 3,499; the second reached
action 3,172.

## Root Cause

The mixed AMO path correctly checked the old-value writeback and updated its
per-line `atomic_values` model. It did not update the separate architectural
reference memory with the AMO result. DCache therefore contained the correct
new dirty value while the ReleaseData oracle still expected the initialized
value. Ordinary committed scalar and vector stores already performed this
reference-only update.

Updating TileLink backing memory at AMO completion would be incorrect: dirty
data must reach that memory only when the DUT emits ReleaseData. The fix adds
`Environment::record_atomic_result()`, which changes only the architectural
reference image after a successful AMO completion. ReleaseData remains
byte-checked against that image before the bus model accepts the writeback.

## Regression Guard

The environment contract test requires both the reference-update API and its
use in the constrained AMO path. The original seeds are rerun after the fix;
their results are recorded in `RESULTS.md`.
