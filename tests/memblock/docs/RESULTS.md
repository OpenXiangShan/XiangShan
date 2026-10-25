# Current MemBlock Qualification

## Scope

This file describes current readiness and its qualification boundary. It is
not a chronological log. Capability inventory comes from the current catalog,
producer/check implementations and the current-state documents, not preserved
campaign claims.

The catalog registers 90 scenarios: 89 directed-tests plus constrained-random.
The default benchmark selects 85; five additional directed contracts require
explicit execution. All claimed behavior remains limited by IO_COVERAGE.md
and ORACLES.md. There is no defensible overall functional-coverage percentage.

## Current Qualification

The exceptional segment data/fix uops are counted as two direction-specific
completions. The scenario checks exact expected page faults, full exception
VA, zero DCache requests and its queue disposition; neither exception is
counted as a successful FOF fix completion. An infrastructure test exercises
this scoreboard distinction for both load and store. Canonical-alias mutation
sensitivity is still incomplete because that scenario uses an empty root.

The current qualification commands are:

- make -C tests/memblock unit
- make -C tests/memblock check-rtl
- make -C tests/memblock picker-harness JOBS=8
- A frozen-runtime benchmark with all 85 selected entries and 256 random actions.
- Explicit execution of all five default-benchmark exclusions.
- A two-seed spec run with 4608 actions per seed and observed overlap analysis.

Build evidence is kept in build/memblock/current-docs-validation, including
runtime/runtime.json, benchmark/results.json, catalog-extras.json,
fix-focused.json and spec-overlap/results.json. These are local generated
artifacts, not files guaranteed by a clean clone. Results must be replayed or
checked at those paths; absent artifacts do not supply qualification evidence.
The runtime reuses the available generated RTL model. A successful C++ rebuild
and manifest/filelist check do not establish clean RTL elaboration provenance.

The current qualification is:

| Check | Current result | Scope limit |
| --- | --- | --- |
| Infrastructure unit | 283 tests pass | Unit checks do not establish DUT functional coverage |
| check-rtl | Pass | Inventory/schema/generated-file consistency, not full prose or RTL-source provenance |
| Selected benchmark | 85/85 pass | Seed 1; random budget 256; catalog-specific action overrides retained |
| Extra directed contracts | 5/5 pass | All 90 catalog entries have a successful execution in this qualification |
| Spec overlap | 2/2 seeds pass, 4608 actions each | 9216 actions, 1005842 cycles, 102 heterogeneous windows; capability only |
| Runtime source copies | All 80 compared C++ source copies match | Existing Verilated model reused; no fresh root RTL generation |

Queue accounting is exact in both mixed results, and all six reported manager
stall/delay counters are nonzero. Per-case report commands and summaries agree
with aggregate results. Full commands/runtime identity are in the local
qualification.json and result artifacts. Execution of all catalog entries is
not proof of all legal input combinations or missing oracle coverage.

## Readiness

Readiness is partial. Functional PASS means active checks accepted generated
traffic, not all legal classes occurred or every external effect was checked.
Current missing rules include guaranteed final random atomic/vector-store
observation, uniform fault vstart/forbidden-demand checks, complete TileLink
lifecycle/credit legality and independent verifier schema/backpressure checks.
Masks, boundary faults, concurrency and reset/protection crosses also remain
partial. Four-state/physical arrays and full-core/multi-hart behavior need
other verification boundaries.

Configured 48h breadth and million-action endurance targets are not completed
qualification in this snapshot. Their current budgets and automated-check
limitations are defined in VERIFICATION_PLAN.md. No closure claim follows
from a short capability run or a controller/verifier return code alone.

## Current Review

The fresh-context independent review verdict is approve for the current
documentation and exceptional-segment completion-accounting fix. The review
checks the actual diff against implementation, supported input values, IO
identity/check rules, effective regression budgets, local links and the
qualification artifacts. No material finding remains open in that scope.

This verdict confirms the accuracy of the stated capability and limitations;
it does not establish verification closure or clean RTL elaboration provenance.
The oracle, protocol lifecycle, vector-mask, final-byte observation and
integration gaps described in the current documents remain open.
