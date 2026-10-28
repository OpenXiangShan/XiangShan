# PBMT response-fault production-only mother integration review

## Integrated Behavior

- Adds the PBMT response-fault phase graph, PMP bootstrap observation, B/C readiness and two-slot request/completion audit.
- Builds B as Store under disabled PBMTE and C as an independent Load after PBMTE enable, sharing one canonical
  level-1 entry while requiring two real L2TLB tokens.
- Keeps mother CSR replay tickets and prefix commits. The PBMT monitor wrapper sets `reset_epoch` and preserves
  produced-count/FIFO-revision checks before the generic event is committed.
- Keeps the previously integrated Sv39 later-split observer, token, redirect/flush cancellation and exact completion
  provenance.
- Excludes all software-only atomicity testcase/vseq code and all RTL/Scala/RM changes.

## Mother Evidence

Mode: `mother_pbmt_response_fault_prod_20261028`.

| Batch | Result | Result directory |
|---|---:|---|
| Original failure seeds | 4/4 strict PASS | `sim/mother_pbmt_response_fault_prod_20261028_results/original4_r1` |
| Authoritative seeds | 10/10 strict PASS | `sim/mother_pbmt_response_fault_prod_20261028_results/authoritative10` |
| Fresh random seeds | 30/30 strict PASS | `sim/mother_pbmt_response_fault_prod_20261028_results/random30` |
| Adjacent PBMT gate NC | 10/10 clean legacy PASS | `sim/mother_pbmt_response_fault_prod_20261028_results/gate_nc10` |
| Adjacent PBMT gate IO | 10/10 clean legacy PASS | `sim/mother_pbmt_response_fault_prod_20261028_results/gate_io10` |

The 44 main-scenario logs each have `rc=0`, exactly one `TEST_PASS`, normal `$finish`, UVM summary ERROR/FATAL=0,
and caught/demoted ERROR/FATAL=0. The 20 adjacent gate logs have the same zero-error and normal-finish evidence,
but their soft-test testcase wrapper does not print `TEST_PASS`; no soft-test change was made to alter that behavior.

## Independent Review

- Reviewer: `Boyle`, agent/session `01a181e7-0c4e-73c2-bfc4-1c79601e2f2f`.
- Review focus: CSR watermark/FIFO atomicity, mother replay-ticket preservation, later-split provenance preservation,
  global helper impact, B/C phase/request/completion/final audit, and forbidden paths.
- Result: `REVIEW_PASS`; no P0/P1/P2 blocker.

Final status: **REVIEW_PASS**.
