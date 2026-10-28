# PBMT response-fault production-only mother integration plan

## Scope

- Mother worktree: `.codex_mother_integration_20261028`
- Mother branch: `codex/memblock-csr-prefix-fix-20261025`
- Source commit: `43669b2d3 memblock: make PBMT response-fault audit atomic`
- Production testcase: `tc=basicTest`, `ts=memblock_l2tlb_pbmt_response_fault_vseq`,
  `cfg=tc_l2tlb_pbmt_response_fault`
- Excluded: software-only caught-fatal atomicity sequence/vseq, `seq_pkg` includes, RTL, Scala and RM changes.

## Integration Rules

1. Reconstruct PBMT phase/readiness/audit logic on top of the mother CSR replay-ticket and raw-prefix commit model.
2. Preserve raw CSR `reset_epoch`, produced count, FIFO revision and atomic prefix consumption.
3. Preserve Sv39 later-split sample ordering, token binding, flush/reset cancellation and completion provenance.
4. Keep B Store and C Load on independent DTLB requestors with exactly two accepted/completed L2TLB tokens.
5. Commit this Bug independently only after compile, original4, authoritative10, fresh30, NC10, IO10 and independent review.

## Acceptance

- Main production scenario: 44/44 strict PASS with `rc=0`, one `TEST_PASS`, normal `$finish`, and zero
  raw/caught/demoted `UVM_ERROR/UVM_FATAL`.
- Adjacent gate NC/IO: 20/20 clean legacy PASS (`rc=0`, normal `$finish`, zero raw/caught/demoted errors/fatals).
  These soft-test testcase wrappers do not emit `TEST_PASS`; they are not reported as strict PASS.
- Staged diff contains no `soft_test`, atomicity vseq, `seq_pkg`, RTL, Scala or RM path.
- Independent final result: `REVIEW_PASS`.

Status: completed on 2026-10-28.
