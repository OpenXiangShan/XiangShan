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
Do not work around the failure by switching to partial execution, reduced stepping, or any other path that avoids the failing window unless the user explicitly asks for a temporary observability run; such runs do not count as a fix.
Do not hide a real error just to get a passing test. If the failure cause is not yet established, keep debugging until it is, and if you still cannot prove the cause, stop and discuss with the user before making speculative changes.

For every DUT bin-trace failure, treat the following as mandatory:

1. Use the generated FST waveform to check whether env-driven `commit` and `redirect` were actually sent, and whether their FTQ / PC / target context matches the expected failing window.
2. State the concrete env bug location, suspected function, or unresolved proof gap before changing behavior.
3. Fix the root cause directly; do not present a bypass as resolution.

## Ground Truth Rules

- Prefer observed DUT IO, generated `Frontend_top.sv`, `signals.json`, and built frontend shared libraries as the runtime ground truth.
- Do not infer runtime DUT behavior from host-side files such as `difftest/src/test/csrc/plugin/simfrontend/ftq.cpp` unless you have independently proven that the built DUT artifacts include that path.
- If you claim a timing or delay issue, show cycle-accurate evidence that the payload is already correct and only arrives late.
- When DUT behavior depends on env-driven `redirect`, `commit`, or other backend-agent stimuli, do not jump to a DUT-bug conclusion first. Prefer checking whether env stimulus timing or stimulus-generation logic is still incorrect before blaming the DUT.
- When you regenerate waveforms for debugging, store them under a date-stamped subdirectory of `src/test/python/Frontend/data/` and give each waveform a logical filename derived from the exact case, seed, and purpose.

## Bin-Case Runtime Requirements

For DUT bin-trace runtime bounds, required artifacts, and observability
requirements, follow `docs/agents/frontend-verification.md` section
`Bin-Trace Run Requirements`.

This debugging guide assumes those run requirements are already in force and
focuses only on how to analyze the failing window once the case is reproducible.

## Address And Payload Facts

These frontend facts are easy to get wrong and should be checked early:

- `io_backend_toFtq_resolve_{0..2}_bits_pc_addr` and `bits_target_addr` use half-word units.
- Redirect `bits_pc` and `bits_target` use byte addresses.
- `resolve.pc_addr` should track the FTQ-entry start PC from `io_backend_fromFtq_startPc_addr`, not a recomputed instruction PC.
- `resolve.ftqOffset` is the half-word offset within a 64-byte FTQ fetch block.
- If FTQ index packing or unpacking is involved, verify assumptions against DUT-observed behavior before hard-coding them in the model or monitor.

## Time-Sensitive Case Notes

Keep this file focused on stable debugging method and semantic rules.

When an investigation produces cursor-specific, PC-specific, rerun-specific, or
binary-specific notes, move them into a separate case-note or spec document
instead of extending this guide with live incident state.

## Backend Reconstruction Rule

For frontend functional verification, use
`docs/agents/frontend-backend-agent.md` as the semantic contract.

When debugging backend-agent behavior, apply these rules:

- Use a two-queue model as the default mental model:
  `cfVec_queue` for path classification and wrong-path flush,
  `commit_queue` for retire semantics.
- Treat DUT `cfVec` as a logical instruction queue ordered by observation.
- Compare that logical queue against golden trace in program order.
- If queue head matches the current golden-trace entry, mark it as correct path;
  do not dequeue it immediately from `cfVec_queue`. Those correct-path
  instructions enter `commit_queue` semantics, and only a later FTQ-entry-
  granular `commit` removes the corresponding head span.
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
  corresponding instruction has ROB-committed in `commit_queue`; only `call` /
  `ret` carry meaningful `rasAction`.
- Any modeled delay applies only after the corresponding behavior is already
  eligible to send; delay does not create eligibility by itself.
- If DUT recovery appears wrong after env-driven `redirect`, first verify that
  env-side `redirect` / `commit` generation and their timing still satisfy the
  semantic contract. Treat env stimulus bugs as more likely than an obvious DUT
  bug until the stimulus path has been ruled out with waveform evidence.
- When doing implementation review, run through
  `frontend-backend-agent.md` section `实现一致性最小检查项` in order:
  `必须项` first, then `建议项`.

If a failing reproduction can be explained by env-side commit bookkeeping
driving FTQ pointer relationships that suppress new IFU requests, do not call
it a DUT-only bug yet.

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
