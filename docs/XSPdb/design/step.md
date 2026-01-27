# Stepping (Design)

## Purpose
Advance the DUT clock while integrating breakpoints, xbreak triggers, and fork-backup tick logic.

## Notes
- `xstep` steps N cycles with periodic break checks.
- `xistep` steps one instruction (ISA step) when supported.
- Breakpoints and xbreak triggers are checked at step boundaries.

## Related commands
- `xstep`, `xistep`
- `xpc` (view PC)
