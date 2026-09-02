# BIN-940 Current-DUT Reachability Review

Date: 2026-09-03

## Scope and provenance

This review covers `BIN-940` (`owner_leaf_042`), whose contract requires an
`invalidTaken` PredChecker event for a block-end RVI conditional branch while
the registered half-RVI state is valid.  The strict four-section denominator
remains `343`; this review does not change CSV status or denominator.

The DUT artifacts below use the matching current Verilator build:

- implementation/source: `1a32a9056d993233fa1bf3a394b16e8a762abf52`
- design baseline: `e5c70547f3a966accf20a4b065ec1d8e33443180`
- configuration: `DefaultConfig`
- build manifest: `d2fbabf8640ae4033b2c4ddfebabdcc76e9b7c66709ae234eb7eb6edd4e6e2fa`

## Directed results

1. The existing `test_fe_ifu_predchecker_invalid_taken` run passed pytest and
   checker/monitor gates (`error_count=0`) and observed the invalidTaken
   source event.  It did not observe `s2_prevEndHalfRviInfo.valid=1`, so the
   BIN-940 producer did not emit `owner_leaf_042`.
2. An isolated cross-block RVI variant was run twice after redirect-and-fence
   replacement.  The runs were:

   - `data/runs/frontend_pytest_20260902_205338_996061_2451913`
   - `data/runs/frontend_pytest_20260902_205535_674287_2470166`

   Both traces showed the generic cross-block half-RVI observations, but the
   exact BIN-940 owner event was absent.  The test waited for a condition that
   was not reached and therefore has `outcome.status=unknown`; these artifacts
   are diagnostic only.
3. A third variant trained the branch before replacing it with a conditional
   RVI (`data/runs/frontend_pytest_20260902_210235_176453_2528553`).  It also
   failed to produce the required exact event.  Monitor error count remained
   zero, but the pytest outcome was a failure and the artifact is ineligible.

The runs show that the current fixture can expose valid half-RVI state and can
exercise invalidTaken independently, but they do not establish the required
same-transaction conjunction.  No artifact is eligible for backannotation.

## Disposition

Keep `BIN-940` as `MODELED` and retain strict coverage at `306/343` (`89.21%`).
The result is a reachability/producer diagnostic, not evidence of an RTL bug
or a HIT.  Closure requires a legal current-DUT producer that preserves a
valid half-RVI state into the invalidTaken request and passes the normal exact
target, checker, and monitor gates.
