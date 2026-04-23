# Frontend Debugging Guide

Treat the project root as `$NOOP_HOME`.

## Default Method

For frontend mismatches or stalls, reconstruct the failing window from
observed DUT signals and trace inputs before forming a hypothesis.

Use this order:

1. Identify the failing test and exact reproduction command.
2. Reconstruct the relevant cycle window from DUT-observed signals in the
   frontend environment.
3. Align that window with the trace input under `NEMU/logs/` and the test
   binary under `ready-to-run/`.
4. Verify the payload semantics in generated artifacts such as
   `build-frontend/pylib/Frontend/Frontend_top.sv`.
5. Only then decide whether the issue is in the DUT, the monitor, the
   environment model, or the test expectation.

Do not start from an internal-model assumption and search for evidence to fit it.
Do not work around the failure by switching to partial execution, reduced
stepping, or any other path that avoids the failing window unless the user
explicitly asks for a temporary observability run; such runs do not count as a
fix.
Do not hide a real error just to get a passing test. If the failure cause is
not yet established, keep debugging until it is, and if you still cannot prove
the cause, stop and discuss with the user before making speculative changes.
When a test is failing, analyze and state the root cause before changing behavior.
Do not make speculative or “try-and-see” behavioral fixes just to move the testcase forward.

For every DUT bin-trace failure, treat the following as mandatory:

1. Use the generated waveform artifact to check whether env-driven `commit`
   and `redirect` were actually sent, and whether their FTQ / PC / target
   context matches the expected failing window.
2. State the concrete env bug location, suspected function, or unresolved proof
   gap before changing behavior.
3. Fix the root cause directly; do not present a bypass as resolution.

## Ground Truth Rules

- If you claim a timing or delay issue, show cycle-accurate evidence that the
  payload is already correct and only arrives late.
- For runtime truth, prefer DUT-observed signals, generated artifacts under
  `build-frontend/pylib/Frontend/`, and trace inputs used by the failing case.
- Do not infer runtime DUT behavior from host-side implementation files unless
  you have independently proven that the built DUT artifacts include that path.

## Bin-Case Runtime Requirements

For DUT bin-trace runtime bounds, required artifacts, and observability
requirements, follow `docs/agents/frontend-verification.md` section
`Bin-Trace Run Requirements`.

This debugging guide assumes those run requirements are already in force and
focuses only on how to analyze the failing window once the case is reproducible.

## Address And Payload Facts

These frontend facts are easy to get wrong and should be checked early:

- `io_backend_toFtq_resolve_{0..2}_bits_pc_addr` and `bits_target_addr` use
  half-word units.
- Redirect `bits_pc` and `bits_target` use byte addresses.
- `resolve.ftqOffset` is the half-word offset within a 64-byte FTQ fetch block.

## Time-Sensitive Case Notes

Keep this file focused on stable debugging method and harness-facing facts.

When an investigation produces cursor-specific, PC-specific, rerun-specific, or
binary-specific notes, move them into a separate case-note or spec document
instead of extending this guide with live incident state.

## Backend Reconstruction Rule

For frontend functional verification, use
`docs/agents/frontend-backend-agent.md` as the semantic contract.

This debugging guide does not restate the backend-agent queue semantics.
When the failure depends on `resolve`, `redirect`, `commit`, or
`callRetCommit` behavior, first prove the observed DUT-facing symptom in
waveforms or monitor output, then consult:

- `docs/agents/frontend-backend-agent.md` for the normative semantic rules
- `docs/agents/frontend-backend-model-review.md` for current implementation
  hotspots in `backend_model.py`
- `docs/agents/frontend-backend-controlflow/README.md` for RTL/control-flow
  background

## Debugging Checklist

- Confirm the exact binary and trace input in use.
- Check whether the failing expectation is in the DUT, the backend model, or
  the test monitor.
- Cross-check signal names and units against
  `build-frontend/pylib/Frontend/signals.json`.
- Confirm that the waveform, trace, and generated DUT artifacts all come from
  the same current reproduction.

## Related References

- `docs/agents/frontend-verification.md`
- `docs/agents/frontend-backend-agent.md`
- `docs/agents/frontend-backend-model-review.md`
- `docs/agents/frontend-backend-controlflow/README.md`
- `docs/testbench/Guide_Doc/dut_bug_analysis.md`
- `build-frontend/pylib/Frontend/Frontend_top.sv`
- `build-frontend/pylib/Frontend/signals.json`
