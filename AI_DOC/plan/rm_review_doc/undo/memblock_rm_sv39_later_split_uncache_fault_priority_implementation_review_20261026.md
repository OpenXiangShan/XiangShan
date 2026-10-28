# Sv39 later-split production-only mother integration review

## Review Object

- Target worktree: `.codex_mother_integration_20261028`
- Target branch: `codex/memblock-csr-prefix-fix-20261025`
- Integration source: selected production hunks from `52f2d0a57`
- Excluded scope: all `soft_test` testcase edits and soft-test-only helpers
- DUT scope: no `src/main/scala` or generated RTL changes
- Current status: **REVIEW_PASS**

## Production Scope

The patch transports real split/LU1/mabuf facts through the L2TLB agent, records a UID/dynamic-epoch/ROB-bound
later-split attempt ledger, binds aggregate L2 request tokens and exact completion generations, exposes readonly
views to RM, and applies the LoadMisalignBuffer-compatible exception priority only when real response provenance is
proven.

The patch does not change DUT stimulus, generic issue scheduling, PMA/PMP CSR FIFO ownership, or software-only
testcase behavior. The existing mother reset-epoch/revision/prefix-commit CSR consumer remains authoritative.

## Review Findings and Closure

1. **Observation before readiness gate**: initial merge observed split facts before sample-anchor and producer-ready
   checks. Fixed by moving ledger mutation after sample validity, readiness, CSR history, and same-sample event/barrier
   reconciliation.
2. **Canceled old-token completion**: initial merge could fatal after redirect canceled an old dynamic attempt. Fixed
   by dropping only a canceled completion whose UID is no longer the same active dynamic epoch; same-epoch or other
   provenance mismatches remain fatal.
3. **Soft-test helper leakage**: removed `memblock_later_split_tlb_record::copy_from()` and `state_equals()` because
   they had no production caller.
4. **Dynamic-array compile error**: replaced invalid `status_by_uid.exists(uid)` with the existing `is_valid_uid(uid)`
   bounds check before direct dynamic-array access.
5. **Same-epoch canceled completion drop**: independent review found that the canceled-owner fast path also treated
   `canonical != owner` as stale, which could silently hide an illegal same-epoch owner replacement. The fast path now
   drops only an invalid/inactive UID or a completion from an older dynamic epoch. Same-epoch replacement must satisfy
   the exact replay provenance branch; otherwise it remains fatal.

## Required Gates Before Approval

- Full VCS compile from the mother worktree.
- Known seeds: `212889620,399662848,1969964361,449212646,1642701870`.
- Fresh random30 listed in the Bug record/formal plan.
- Version-controlled closure10 seed file, with result `seeds.txt` byte-identical to the input artifact.
- Adjacent `pbmt0_non_nc` and `strict_nc`, seed `666666`.
- Every result: `PASS/EXIT_CONFIRMED/rc=0/uvm_count=0`, normal `$finish`, and zero raw/caught/demoted
  `UVM_ERROR/UVM_FATAL`.
- Independent final review of the exact staged diff and resulting evidence.

## 2026-10-28 Mother Integration Evidence

- Full VCS compile passed in mode `mother_sv39_split_prod_20261028`.
- Original failure seed `212889620`: 1/1 strict PASS.
- Known5 plus fresh random30: 35/35 strict PASS.
- Version-controlled closure10: 10/10 strict PASS; result `seeds.txt` is byte-identical to
  `scripts/closure_seed_lists/tc_dispatch_real_mmu_sv39_smoke.seeds`.
- Adjacent `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k` and
  `tc_dispatch_real_mmu_sv39_strict_nc_100k`, seed `666666`: 1/1 PASS each.
- After the same-epoch canceled-completion review fix, a new full compile and focused seeds
  `212889620,399662848,1969964361,449212646,1642701870` passed 5/5 in mode
  `mother_sv39_split_reviewfix_20261028`.
- Every audited seed has `rc=0`, exactly one `TEST_PASS`, one normal `$finish called` and `$finish at simulation time`,
  UVM summary `ERROR=0/FATAL=0`, and caught/demoted `UVM_ERROR/UVM_FATAL=0`.
- Staged scope contains no `soft_test`, `src/main/scala`, or `build/rtl` path.

## Independent Final Review

- Reviewer: `Dewey`, agent/session `01a1815b-d437-7d31-822f-fdf7e2e98355`.
- First conclusion: `REVIEW_FAIL`, identifying the same-epoch canceled-completion silent-drop blocker.
- After the narrowed stale-drop rule and focused compile/5-seed rerun, final conclusion: `REVIEW_PASS`.

Final approval: **REVIEW_PASS**. The production-only mother integration is approved for its single-Bug commit.
