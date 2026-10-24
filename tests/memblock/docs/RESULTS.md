# MemBlock Verification Results

This file is the compact current evidence index for the MemBlock UT. Current
contracts, acceptance rules, and verification-point status remain authoritative
in [`VERIFICATION_PLAN.md`](VERIFICATION_PLAN.md), [`ORACLES.md`](ORACLES.md),
and [`IO_COVERAGE.md`](IO_COVERAGE.md). Confirmed CPU bug details remain in the
individual `CPU_BUG_*.md` documents.

## Test Taxonomy

Only two stimulus families exist:

| Family | Scenarios | Description |
| --- | ---: | --- |
| `directed-tests` | 84 | Fixed semantic test sequences; a seed may randomize data, addresses, lanes, and timing, but not the test identity. |
| `constrained-random` | 1 | `constrained-random`, driven by the common constraint interface and `coverage`/`spec`/`corner` presets. |

There is no third scenario family. `make benchmark-tests` runs all 85 scenarios
once (one seed, one action budget per scenario); it is a benchmark command, not
a test family or a new mode. `make regression`, `make final-regression`, and
`make endurance-regression` run `constrained-random` with different seed and
duration budgets.

The constraint interface version is metadata (`constraint_schema=53`), not a
scenario or a work-round name.

## Current Baseline

### 2026-10-24

- Inventory: **85 scenarios** = **84 `directed-tests` + 1 `constrained-random`**.
- `make benchmark-tests` with 8 workers: **85/85 pass in 781.752 s**.
- `constrained-random` seed 1/16,384: **760.884 s**, 1,840,675 DUT cycles,
  ~21.5 actions/s, ~2,419 cycles/s.
- Next longest scenarios: `vector-segment` 386.257 s,
  `vector-guest-fault-boundary` 53.948 s.
- Unit tests: **282 pass**.
- `make check-rtl`: pass.
- Frozen runtime binary SHA-256:
  `4185c7ce787c05ac309a05fea58850175689e3a7d603bbd53c4af2a99c9b2404`;
  model SHA-256:
  `d659328ca82904a6ee33d3c5bc4ec32202a81e004c38e8d2fad5b57606b60d3f`.
- Evidence: `build/memblock/taxonomy-20261024/benchmark.json`,
  `benchmark.md`, and 85 per-run coverage reports under `coverage-reports/`.

This is a capability/performance baseline, not closure. The 48-hour
`constrained-random` campaign and the 200-hour endurance campaign have not run
under the current taxonomy.

## Current Verification Position

- `directed-tests` cover the focused protocol, translation, vector, cache,
  reset, protection, and top-level contracts registered in the catalog.
- `constrained-random` samples and reports coverage; coverage is observational
  and is not a PASS gate.
- The redirected-load epilogue in `constrained-random` randomizes supported
  fields (address, opcode, lane, delay) while keeping redirect semantics fixed.
- `frontend-bridge` is a `directed-tests` scenario. It is not part of the
  `constrained-random` regression.

## Remaining Gaps

- Malformed or duplicate manager responses, replacement combined with
  Probe/CMO/atomic traffic, and broader PMP/PMA/hypervisor crosses remain.
- Single-set associativity coverage and some vector-segment fault/progress
  crosses remain.
- Constrained-random mutation sensitivity is not yet demonstrated.
- The 48-hour and 200-hour campaigns, and any closure claim, remain future work.

## Evidence Policy

- Acceptance uses architectural observations and externally visible protocols,
  not DUT-internal state.
- A passing finite capability run is not a closure campaign.
- Failed or repaired historical investigations remain in Git history and the
  `CPU_BUG_*.md` / `HISTORICAL_BUG_AUDIT.md` provenance documents; they are not
  current regression results.
- Only a deterministic reproducer that excludes harness, model, scoreboard,
  stale-build, and provenance errors establishes a CPU RTL bug.
- The authoritative closure duration, seed count, worker count, and artifact
  checks are in
  [`VERIFICATION_PLAN.md#regression-acceptance`](VERIFICATION_PLAN.md#regression-acceptance).
