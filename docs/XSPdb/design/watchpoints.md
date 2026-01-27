# Watchpoints and Visibility (Design)

## Purpose
Track signal changes or state in real time and inspect/modify signals.

## Notes
- `xwatch`/`xunwatch` for arbitrary signals.
- `xwatch_commit_pc` for commit PC monitoring.
- `xprint`/`xset` for ad-hoc inspection and control.
- `xpc` and related helpers provide a software-synchronized view of hardware state.

## Related commands
- `xwatch`, `xunwatch`, `xwatch_commit_pc`, `xunwatch_commit_pc`
- `xprint`, `xset`, `xpc`
