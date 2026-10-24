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

### 2026-10-24 mixed-window legality correction

A 16-seed `constrained-random --constraints coverage` run found two failures
that were reproduced deterministically at seeds 5 and 7. Neither was a CPU bug.
Seed 5 failed in heterogeneous window 950: an older `lw` at `0x8027afff`
returned byte `0x67` from a younger `sh` at the same address instead of the
pre-store byte `0x43`.

Internal observation localized the cause to the testbench, not the DUT. The
window's scalar store crossed a 4-KiB page. To make its split-store replay
progress, the window made that younger store the backend ROB head and pulsed
`scommit`. The older load was still pending. The StoreQueue therefore committed
the store (`sq_cmt=25, committed[24]=1`) while the older load was outstanding;
the split load's second part then read the now-visible store byte. A real
backend cannot make a store the ROB head or commit it before an older load
retires. Seed 7's earlier miss-burst accounting failure belongs to the separate
grant-pairing class described below; it passed after the mixed-window
correction, and the grant-pairing correction then addresses that class directly.

The heterogeneous window now keeps its scalar store within one 4-KiB page.
Page-crossing scalar-store replay and exact readback remain covered by
`scalar-misaligned` and `misaligned-stores`, so the mixed window still owns
line-crossing misalignment but never manufactures an illegal commit order.
`single-load` also gained two directed younger-store ordering cases: an older
aligned load miss and an older split cross-line load, each with a younger store
issued while the older refill is held. Both require the load to retain its
pre-store value and pass.

A second false-positive class surfaced after that correction. Seeds 8 and 10
failed the `random-load-merge-manager` and `random-miss-burst-manager` gates
with one more GrantAck than GrantData refills. The extra acknowledgement was a
legal background `AcquirePerm` (data-less `Grant`) from the hardware prefetcher,
which the gates did not include in their grant pairing. Both gates now compare
the GrantAck delta against the sum of GrantData refills and `AcquirePerm`
grants; a genuinely duplicate or lost acknowledgement still fails. Because the
artifact manager tuple records raw refills and acknowledgements, the streaming
verifier also allows the artifact-global difference up to the recorded
`dcache_acquire_perms`; the action-local gate still requires exact pairing.
Seeds 8 and 10 then complete all 16,384 actions.

With the corrected harness, seeds 5, 7, 8, and 10 all complete all 16,384
actions (`MEMBLOCK_CONSTRAINED_RANDOM_PASS`). The frozen runtime was regenerated
as binary SHA-256
`98b1260afec033e6358ebc52171de7bf6b537ffd27565d3fa16c8ee34b20ae17` (model
SHA-256 `d659328ca82904a6ee33d3c5bc4ec32202a81e004c38e8d2fad5b57606b60d3f`).
The previous 16-seed artifacts and their pre-correction runtimes are superseded
for campaign evidence; their failures are harness-provenance findings, not CPU
bugs.

The final 16-seed `constrained-random --constraints coverage` capability run on
this frozen runtime passes **16/16 seeds**, **262,144 actions**, in 2,227 s
(artifact `build/memblock/regression-20261024-final-coverage-16.json`). This is a
capability run under the 16,384-action budget, not the 48-hour or 200-hour
closure campaigns required by
[`VERIFICATION_PLAN.md#regression-acceptance`](VERIFICATION_PLAN.md#regression-acceptance).

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
