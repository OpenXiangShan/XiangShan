# Memblock mother integration status - 2026-10-28

## Mother

- Worktree: `.codex_mother_integration_20261028`
- Branch: `codex/memblock-csr-prefix-fix-20261025`
- Current HEAD: `5c131a18d`
- Worktree status: clean

## Integrated Bug Commits

| Commit | Bug | Mother verification | Source worktree |
|---|---|---|---|
| `91d403eb1` | PMA/PMP CSR raw FIFO continuous prefix service | integrated before this continuation; later PBMT integration preserved ticket/revision/prefix semantics | preserved WIP source worktree remains untouched |
| `fb9f04abe` | L2TLB PPN reuse smoke determinism | specialized 22/22 plus adjacent Sv39 1/1 PASS | deleted; branch retained |
| `a13d10814` | Sv39 later-split mabuf response provenance | compile; original1; known5+random30 35/35; closure10 10/10; adjacent2; review-fix5; post-commit original1, all strict PASS | deleted; branch retained |
| `5c131a18d` | PBMT response-fault directed audit | compile; main production 44/44 strict PASS; NC/IO gate 20/20 clean legacy PASS; post-commit original1 strict PASS | deleted; branch retained |

Sv39 source commit `52f2d0a57` and PBMT source commit `43669b2d3` were not cherry-picked because each included
excluded software-only changes. Their production logic was reconstructed on the mother branch and independently
reviewed before commit.

## Worktree Cleanup

Deleted after integration/retest/review:

- `.codex_sv39_split_uncache_rm_fix_20261025`
- `.codex_pbmt_response_fault_fix_20261025`
- `.codex_fifo_cached_verify_20261027`
- `.codex_fifo_cached_verify_20261027_r11`
- `.codex_fifo_cached_verify_20261027_r12`
- `.codex_fifo_cached_verify_20261027_r13`

The four FIFO verify worktrees were clean detached cached-tree checkpoints. Their final production files are
byte-identical to integrated commit `91d403eb1`; intermediate revisions were not merged.

## Preserved / Not Merged

- `.codex_csr_prefix_fix_20261025`: explicitly preserved dirty worktree; do not modify or delete.
- `.codex_csr_scalar_matrix_20261025`: not merged; lacks the required original-matrix/real-EDA justification.
- `.codex_l2tlb_stage_verify`, `_r3`, `_r4`, `_r7`, `_r8`: dirty historical adaptation worktrees; not part of the
  current Bug integration and therefore not deleted.
- `.codex_sparse_ppn_isolation_20261025`: dirty detached worktree with soft-test/cfg changes; not deleted.

## Current Decision

The two completed 2026-10-25 source Bugs are fully integrated and closed. No currently preserved dirty worktree is
authorized for automatic merge or deletion. The next mother-task step is a fresh regression-status scan from the
documented testcase/cfg matrix, not merging `soft_test` or CSR scalar work.
