# Frontend Debugging Guide

Treat the project root as `$NOOP_HOME`.

## Default Method

For frontend mismatches or stalls, reconstruct the failing window from observed DUT signals and trace inputs before forming a hypothesis.

Use this order:

1. Identify the failing test and exact reproduction command.
2. Reconstruct the relevant cycle window from DUT-observed signals in the frontend environment.
3. Align that window with the trace input under `NEMU/logs/` and the test binary under `ready-to-run/`.
4. Verify the payload semantics in generated artifacts such as `build-frontend/pylib/Frontend/Frontend_top.sv`.
5. Only then decide whether the issue is in the DUT, the monitor, the environment model, or the test expectation.

Do not start from an internal-model assumption and search for evidence to fit it.

## Ground Truth Rules

- Prefer observed DUT IO, generated `Frontend_top.sv`, `signals.json`, and built frontend shared libraries as the runtime ground truth.
- Do not infer runtime DUT behavior from host-side files such as `difftest/src/test/csrc/plugin/simfrontend/ftq.cpp` unless you have independently proven that the built DUT artifacts include that path.
- If you claim a timing or delay issue, show cycle-accurate evidence that the payload is already correct and only arrives late.
- When DUT behavior depends on env-driven `redirect`, `commit`, or other backend-agent stimuli, do not jump to a DUT-bug conclusion first. Prefer checking whether env stimulus timing or stimulus-generation logic is still incorrect before blaming the DUT.
- When you regenerate waveforms for debugging, store them under a date-stamped subdirectory of `src/test/python/Frontend/data/` and give each waveform a logical filename derived from the exact case, seed, and purpose.

## Bin-Case Observability Rule

For DUT bin-trace cases, observability is mandatory, not optional.

Every bin-trace run must leave behind:

- an FST waveform artifact
- a paired log artifact

And every bin-trace run must expose runtime progress information, such as:

- progress checkpoints
- stall snapshots
- or an equivalent mechanism that can explicitly tell whether the run is still
  moving forward

Do not accept a bin case that can only:

- print nothing while it runs
- appear stuck indefinitely
- and reveal no structured information until an external timeout or manual kill

If a bin-trace case stops making progress, the run should make that visible in
its own logs instead of degenerating into an opaque hang.

## Address And Payload Facts

These frontend facts are easy to get wrong and should be checked early:

- `io_backend_toFtq_resolve_{0..2}_bits_pc_addr` and `bits_target_addr` use half-word units.
- Redirect `bits_pc` and `bits_target` use byte addresses.
- `resolve.pc_addr` should track the FTQ-entry start PC from `io_backend_fromFtq_startPc_addr`, not a recomputed instruction PC.
- `resolve.ftqOffset` is the half-word offset within a 64-byte FTQ fetch block.
- If FTQ index packing or unpacking is involved, verify assumptions against DUT-observed behavior before hard-coding them in the model or monitor.

## Current microbench.bin Constraints

For the current `microbench.bin` investigation, treat the following as established until new DUT evidence disproves them:

- `redirect.level` is fixed to `0` (`flushAfter`).
- When `level == 0`, do not assume the redirecting instruction itself is flushed. Verify the actual flush scope from DUT behavior.
- Do not treat resolve or redirect delay as the default root cause.
- The likely failure surface is payload semantics, FTQ mapping, or monitor expectations unless cycle evidence proves otherwise.

## Backend Reconstruction Rule

For frontend functional verification, use
`docs/agents/frontend-backend-agent.md` as the semantic contract.

When debugging backend-agent behavior, apply these rules:

- Treat DUT `cfVec` as a logical instruction queue ordered by observation.
- Compare that logical queue against golden trace in program order.
- If queue head matches the current golden-trace entry, mark it as correct path;
  do not dequeue it immediately. Correct-path instructions remain in queue until
  a later FTQ-entry-granular `commit` removes them from queue head.
- The first mismatch marks the beginning of a wrong path; that first mismatched
  instruction does not have to be a CFI, but the immediately preceding
  correct-path instruction should normally be the redirecting CFI in the
  mispredict case. That wrong-path region remains live until a later
  `redirect` recovers the correct path and flushes the wrong-path instructions.
- After the first mismatch, continue accepting later `cfVec` packets into the
  logical queue in observation order. Do not pause queue construction merely
  because the expected golden PC has not reappeared yet.
- `redirect` must clear the current wrong-path suffix using the first mismatch
  position as the semantic flush origin. Keep any older correct-path prefix in
  queue so it can still retire by later `commit`.
- If post-redirect stale packets still expose pieces of the old wrong path for
  a few cycles, treat them as recovery residuals from the same redirect event.
  Do not immediately open a brand-new mismatch episode unless you have evidence
  that a new wrong path has actually begun.
- If the recovery target PC is observed but does not sit at queue head, treat
  the older queued prefix ahead of it as recovery residual from the previous
  redirect, not as a new correct-path prefix.
- If the same FTQ pointer appears again while queue still contains older
  instructions from that FTQ entry, interpret the new observation as part of
  the same active FTQ entry, not as a brand-new entry.
- Every correct-path CFI must eventually produce `resolve`.
- Wrong-path CFI may produce `resolve` before being flushed, but are not
  required to do so.
- `commit` is FTQ-entry-granular, must remain strictly in order, and means the
  corresponding entry's instructions are at the logical queue head and have all
  been ROB-committed on the correct path.
- In queue semantics, an FTQ entry leaves queue only in two legal ways:
  by its in-order `commit`, or by being flushed as wrong-path content under a
  later `redirect`.
- If later observations can only be explained by reviving an FTQ entry that is
  already older than current `commit_ptr`, diagnose the earlier `commit` as
  premature instead of treating the old entry as legitimately active again.
- A committable FTQ entry must not still owe a `redirect`; if a redirect for
  that entry is still semantically pending, treat the entry as not yet
  committable even if other bookkeeping appears ready.
- `callRetCommit` is instruction-granular and may appear as soon as the
  corresponding instruction has ROB-committed; only `call` / `ret` carry
  meaningful `rasAction`.
- Any modeled delay applies only after the corresponding behavior is already
  eligible to send; delay does not create eligibility by itself.
- If DUT recovery appears wrong after env-driven `redirect`, first verify that
  env-side `redirect` / `commit` generation and their timing still satisfy the
  semantic contract. Treat env stimulus bugs as more likely than an obvious DUT
  bug until the stimulus path has been ruled out with waveform evidence.

If a failing reproduction can be explained by env-side commit bookkeeping
driving FTQ pointer relationships that suppress new IFU requests, do not call
it a DUT-only bug yet.

## Current 0x80004a82 Signature

For the current `microbench.bin` DUT failure window around `0x80004a82`, treat the following as established until new DUT evidence disproves them:

- The first env-side false positives around stale commit-visibility state were in the Python env, and have already been removed from the critical path. The remaining failure is not explained by that earlier env bug.
- The stable failing golden-trace window is around trace cursor `1088`, where the golden trace expects:
  `0x80004aaa -> 0x80004a82`.
- In the failing DUT runs, the frontend can keep producing wrong-path `cfVec` packets after the backend redirect to `0x80004a82`, including windows rooted at `0x80004a9a`, `0x80004852`, `0x800047fa`, and `0x80003a44`.
- When the failure finally surfaces, the monitor reports `REDIRECT_TIMEOUT(expected_pc=0x80004a82)`, but by then backend-model pending work is already quiescent. Do not treat this as proof that the monitor timeout itself is the root cause.
- The relevant topdown stall code seen at the final stuck window is `9`, which maps to `LoadVioReplayStall`, not a redirect/recovery bubble.
- In the stuck window, `FTQ` still points `ifuPtr` at an entry whose `startPc` is `0x80004a82`, but the DUT does not converge to that target.
- In the final timeout window, no new env-driven redirect remains active: `checkerRedirect_valid`, IFU `wbRedirect_valid`, backend redirect valid, and FTQ redirect valid can all be `0` while the DUT stays stuck.
- In the latest timeout snapshot, the most recent DUT redirect to `0x80004a82` is still classified as a control redirect (`debugIsCtrl = 1`, `debugIsMemVio = 0`), so do not assume the final bad state is caused by a mem-vio redirect payload being driven with wrong metadata.
- In that same snapshot, `s1_fetchBlock_0.startVAddr` is already `0x80004a82`, but `s2_fetchBlock_0.startVAddr`, `s3_alignFetchBlock_0.startVAddr`, and visible IBuffer lane 0 still point at `0x800047fa`, so the current high-value question is stale wrong-path stage recovery rather than FTQ target selection alone.
- Source-plus-wave correlation now adds an important qualifier: in the current reproduction, backend commit input advances FTQ commit state across `(1,60) -> (1,61) -> (1,62) -> (1,63) -> (0,0)` immediately after the redirect to `0x80004a82`. This drives FTQ into a state where `distanceBetween(ifuPtr, commitPtr)` is too large, so `io.toIfu.req.valid` drops to `0` by construction. Treat the current terminal timeout as env-commit dominated unless that over-commit is removed first.
- After gating off that over-commit path, the reproduction still stalls earlier around cursor `374` and target `0x800047fa`, with `ifuPtr = (1,8)`, env-side `commit_ptr = (0,8)`, and no new IFU stage fire. Treat that earlier stop as the opposite env-side failure mode: commit starvation / commit-coverage mismatch for truncated FTQ entries, not yet clean DUT-only evidence.
- A later env bug remained on the golden-trace commit path: when semantic commit reached the pending level-0 target entry, it updated `commit_ptr` but did not clear `pending_level0_target_ftq`. That created a self-deadlock where later commits were blocked forever by the stale pending target. This bug has now been fixed.
- In the latest seed-locked rerun after that fix, the stop still happens around cursor `245` / target `0x80003a84`, but the env state is different: `commit_ptr = (0,31)`, `pending_level0_target_ftq = none`, and the only remaining backend-model work is FTQ entry `(0,32)`. Treat the previous stale-pending-target explanation as closed; the next env-first question is why the post-redirect FTQ entry containing the wait PC never becomes visible/committable.
## Debugging Checklist

- Confirm the exact binary and trace input in use.
- Check whether the failing expectation is in the DUT, the backend model, or the test monitor.
- Cross-check signal names and units against `build-frontend/pylib/Frontend/signals.json`.
- Verify whether the environment is using generated DUT artifacts that match the current source tree.
- Inspect local diffs before changing shared frontend files such as `backend_model.py` or `test_multi_branch.py`.
- When the same FTQ pointer is observed again, first check whether queue still
  contains older instructions from that FTQ entry before inventing a “new
  entry” explanation.
- When the env appears to need an already-committed older FTQ entry to explain
  later observations, first audit whether the earlier `commit` was issued too
  early.
## Related References

- `docs/agents/frontend-verification.md`
- `docs/testbench/Guide_Doc/dut_bug_analysis.md`
- `build-frontend/pylib/Frontend/Frontend_top.sv`
- `build-frontend/pylib/Frontend/signals.json`
