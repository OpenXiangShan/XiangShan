# Duration Regression Provenance Failure

## Status

**UT framework/environment issue; fixed. This is not a CPU RTL bug.**

On 2026-09-05, a one-hour frozen `random-stress` campaign completed every DUT
invocation successfully but correctly returned a nonzero aggregate result
because one provenance input disappeared during the run. The follow-up
verifier also exposed an independent Makefile count mismatch.

## Observation

The campaign ran eight workers with 16,384 actions per seed and completed 191
continuous seeds in 3,741.172444 seconds:

```text
MEMBLOCK_REGRESSION_FAIL seeds=191 transactions=3129344
elapsed_seconds=3741.172
```

All 191 result objects have `status=pass`, `returncode=0`, empty failure output,
balanced queue accounting, nonzero required coverage, backpressure, and the
same complete RTL SHA-256
`b69e387eb081a3f311311079ade435206817c7c6a20bd8f3a5f11889ec1dcbf4`.
The frozen executable, model, xspcomm, and system-library hashes were unchanged.

The aggregate rejection was:

```text
controller.error=[Errno 2] No such file or directory:
/home/xuyinan/xs4/XiangShan/build/memblock/rtl.json
controller_unchanged=false
```

Regenerating the missing file produced the exact launch-time SHA-256
`f3b17cc5d056ec9055970874ae306d28ccb7d082ded369c1e326081107873df9`,
confirming that RTL content had not changed. The original artifact remains
rejected because its before/after provenance record is incomplete; it is not
rewritten to manufacture a pass.

Running the independent verifier with the original target then exposed a
second issue before it reached that recorded provenance rejection:

```text
verify_regression.py: error:
configured transaction count differs from verifier expectation
```

`stress-regression` recorded the runner's 16,384 default in both
`transactions_per_seed` and `mixed_transactions_per_seed`, while
`verify-stress-results` expected 4,096 for the generic field and 16,384 for the
stress field. Passing 16,384 explicitly let the verifier parse and validate all
per-seed results before it rejected the artifact's recorded
`controller_unchanged=false` summary.

Artifact: `build/memblock/stress-frozen-a32d74a61-1h-16384.json` (generated,
not tracked), SHA-256
`0c4d8a1a68bc9325df35bd465f60a0a0ab4ed3cd368e7f80cd03ef7372ae04e1`.

## Root Cause

The runtime freezer copied the executable and shared libraries, but the long
runner still referenced the mutable preparation file
`build/memblock/rtl.json`. Its hash was captured at launch, yet a concurrent
build-tree operation could remove that generated file before the shutdown
hash. The controller therefore did exactly what it should and refused the
artifact, but the acceptance input was unnecessarily fragile.

Separately, the stress verification target used a historical 4,096 default for
`--transactions`, even though both the runner default and
`STRESS_TRANSACTIONS` were 16,384.

## Fix

- `freeze_runtime.py` now copies `rtl.json` into the read-only runtime directory
  and records it as a required `rtl_metadata` artifact.
- Duration runners and verifiers use `build/memblock/runtime/rtl.json`.
- The frozen RTL metadata is a Make target, so a missing copy triggers a new
  complete runtime freeze.
- `verify-stress-results` derives both transaction-count arguments from
  `STRESS_TRANSACTIONS`.
- Unit tests require all four frozen roles, the read-only metadata copy, and the
  matching stress target arguments.

This hardening does not relax controller-source checks. Live C++/Python/SVA/
configuration changes during a campaign still make the artifact non-accepting.
