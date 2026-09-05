# Constrained-Random Minimum Budget Bug

## Classification

This was a UT controller/validation bug, not an XiangShan RTL bug. It was found
on 2026-09-05 while validating the newly frozen constrained-random runtime.

## Symptom

`random-mixed --seed 1 --transactions 128 --constraints spec` completed all
128 requested actions and drained both LSQ scoreboards, but failed the final
coverage gate. Only two heterogeneous overlap windows fit after the mandatory
architectural prefix; the common mixed contract requires at least four.

## Root Cause And Fix

The old 128-action lower bound predated configurable operation floors and the
SPEC-like overlap budget. It rejected obviously tiny runs but no longer
guaranteed enough random tail for the coverage contract it advertised.

The simulator and Python regression controller now enforce the same minimum of
256 actions. This fails undersized jobs before an expensive simulation is
accepted and gives all shipped presets room for the mandatory prefix, four
overlap windows, and every enabled constrained class. The normal and long-run
defaults remain 16,384 or larger.

The offline verifier retains a separate 128-action compatibility threshold.
Historical enhanced-format artifacts at that size already contain replay,
virtualization, exception, concurrency, and backpressure fields, so raising the
new-run minimum must not weaken validation of those existing results.

## Regression Guard

The controller unit test pins the shared 256-action lower bound, while verifier
tests pin the independent 128-action compatibility threshold and all enhanced
coverage gates. A frozen runtime `spec` run at exactly 256 actions is used as
the boundary acceptance check.
