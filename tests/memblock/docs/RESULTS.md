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

## Current Changes Qualified Below

- Constrained-random finishes with a drain and an independent word readback of
  cacheable identity-mapped committed stores/atomics; permitted/aligned
  cacheable hypervisor stores add a same-context HLV readback.
- DCache E GrantAck is admitted only after a first Grant/GrantData D beat (or in
  that same cycle), with sink lifetime tracked through E.
- PTW and Uncache reject source reuse before final D and D completion of an
  inactive source; same-cycle final-D/source reuse is allowed.
- Back-to-back cacheable `CBO.ZERO` no longer loses a store completion,
  including a following ordinary store behind a blocked zero. This is a
  confirmed RTL bug, documented and fixed in
  [CPU_BUG_CBO_ZERO_BACK_TO_BACK.md](CPU_BUG_CBO_ZERO_BACK_TO_BACK.md).
- Successful CBO.CLEAN/FLUSH lines stay in the final readback; only CBO.INVAL
  and errored CMO lines are excluded. The observed-word count is incremented
  per completed batch and gated against the registered count.
- `atomic-contracts` forces a ProbeAckData on the dirty AMO line and checks the
  updated data bytes plus the manager image, so AMO post-state is not observed
  only through a cache-resident load.
- DCache C-channel ReleaseData now checks raw address/source/param/size/beat
  identity; ProbeAckData checks source/report/size/address identity; data-less
  Release checks this DUT's full-line size, line alignment and shrink param
  subset 0..2. `corrupt` is per-beat and not a data-less Release validity field;
  TileLink user/echo fields are DUT-specific, not stability fields.
- First-element FOF completions now carry an explicit exceptional `vstart`
  expectation. The `cbo-zero-contracts` scenario also contains a sequential
  control and a back-to-back zero case.

## Current Qualification

The current qualification commands are:

- make -C tests/memblock unit
- make -C tests/memblock check-rtl
- make -C tests/memblock picker-harness JOBS=16
- A frozen-runtime benchmark with all 85 selected entries and 256 random actions.
- Explicit execution of all five default-benchmark exclusions.
- Finite coverage and corner campaigns with 4608 actions per seed.
- An 8-seed spec confidence run with 16384 actions per seed.
- The configured 8-seed, 16384-action coverage regression.

The qualification artifacts are local under
`build/memblock/agent-campaign-12`, `agent-campaign-13`,
`agent-campaign-14`, `agent-campaign-15` and `agent-campaign-16` (not guaranteed
by a clean clone). The full 6x4608/4x4608 campaigns are from
`agent-campaign-12`; the final benchmark is `agent-campaign-16`; the 8x16384
spec and coverage runs are from `agent-campaign-13` and `agent-campaign-14`.
The runtime was frozen after
a fresh root `build/rtl` generation, `prepare-rtl`, and Picker/model rebuild for
the `StoreQueue.scala` fix; the previous model was not reused.

The current qualification is:

| Check | Current result | Scope limit |
| --- | --- | --- |
| Infrastructure unit | 284 tests pass | Unit checks do not establish DUT functional coverage |
| check-rtl | Pass | Inventory/schema/generated-file consistency, not full prose or RTL-source provenance |
| Selected benchmark | 85/85 pass, 377.7 s | Seed 1; random budget 256; catalog-specific action overrides retained; `agent-campaign-16` |
| Extra directed contracts | 5/5 pass | cmo-store-flush, redirect-store-flush, tag-ecc-ways, vector-fof-high, vector-segment-noncanonical |
| Coverage campaign | 6/6 seeds pass, 4608 actions each (27648 actions) | Seeds 101..106; artifact `agent-campaign-12`; 1029.0 s; capability only |
| Corner campaign | 4/4 seeds pass, 4608 actions each (18432 actions) | Seeds 101..104; artifact `agent-campaign-12`; 1985.0 s; capability only |
| Regression acceptance | 8/8 seeds pass, 16384 actions each (131072 actions) | Seeds 1..8; artifact `agent-campaign-14`; 1003.2 s; default coverage profile |
| Spec confidence run | 8/8 seeds pass, 16384 actions each (131072 actions) | Seeds 1..8; artifact `agent-campaign-13`; 845.6 s; spec latency |
| Data-less Release check | 2/2 coverage and 2/2 corner seeds pass, 4608 actions each | Seeds 121..122; artifact `agent-campaign-15`; final C-release monitor |
| Back-to-back CBO.ZERO | Fixed model passes; pre-fix frozen model fails deterministically | Directed `cbo-zero-contracts`; see CPU bug document |
| Final word readback | observed words equal registered words in every passing mixed case | Cacheable identity path; aliases/non-cacheable effects and CBO.INVAL/errored CMO lines excluded; successful CLEAN/FLUSH retained |
| Atomic dirty-line persistence | `atomic-contracts` passes; ProbeAckData carries the updated line and updates the manager image | AMO target only; cross-hart/concurrent eviction not covered |

The CBO.ZERO pre-fix replay fails at cycle 33333 with
`timed out waiting for scalar store writebacks sq=3/6
store_writebacks=5 sbuffer_empty=0`; the fixed directed case passes at cycle
676 with `sequential_zero=2 back_to_back_zero=2 following_store=1`. This is
externally visible, deterministic and independent of testbench reference-state
errors.

Queue accounting is exact in the mixed results, the observed-word count equals
the registered count, and per-case report commands and summaries agree with the
aggregate results. Full commands/runtime identity are in the local result
artifacts. Execution of all catalog entries is not proof of all legal input
combinations or missing oracle coverage.

## Readiness

Readiness is partial. Functional PASS means active checks accepted generated
traffic, not all legal classes occurred or every external effect was checked.
Current missing rules include aliased/non-cacheable final-byte observation,
uniform fault forbidden-demand checks, B/C acceptance provenance, the
same-line CBO.ZERO forward window before Sbuffer s2, and independent verifier
schema/backpressure checks.
Masks, boundary faults, concurrency and reset/protection crosses also remain
partial. PBMT=NC `CBO.ZERO` was probed and the DUT reports a store access fault
before an uncache request; the architecture leaves CBO behavior for
non-cacheable memory unspecified, so neither behavior is claimed. Four-state/
physical arrays and full-core/multi-hart behavior need other verification
boundaries.

The configured 8-seed, 16384-action coverage regression is completed in this
snapshot. Configured 48 h breadth and million-action endurance targets are not
completed. Their current budgets and automated-check limitations are defined in
VERIFICATION_PLAN.md. No closure claim follows from a short capability run or a
controller/verifier return code alone.

## Current Review

A fresh-context review rejected the first per-lane CBO.ZERO gate because it
could let a following store bypass a blocked lane of the
`DatamoduleResultBuffer` prefix FIFO. The fix now propagates the block as a
prefix, and `cbo-zero-contracts` exercises CBO.ZERO, CBO.ZERO, ordinary store.
The final re-review verdict is approve-with-findings for the code logic:
F1/F2 are fixed and directly evidenced, and the campaign-artifact finding is
closed by the completed `agent-campaign-12` coverage/corner artifacts. The sole
remaining finding is documentation-only: the CLEAN/FLUSH sentence now states
that retention observes a stale/corrupted visible data byte, not post-CMO
clean/dirty state or persistence of a dirty cache-resident line. The later
`atomic-contracts` dirty-line `ProbeAckData` persistence addition was
independently reviewed and approved; its only request was a comment cleanup,
which is applied. The corrected C multibeat source/param/size/raw-address
monitor was also approved; same-address ProbeAck lookup remains by base and is
documented as a residual limitation. The data-less Release full-line/shrink
check was corrected to ignore `corrupt` and approved.

This qualification confirms the stated capability and limitations; it does not
establish verification closure or clean RTL elaboration provenance. The oracle,
protocol lifecycle, vector-mask, aliased/non-cacheable final-byte observation
and integration gaps described in the current documents remain open.
