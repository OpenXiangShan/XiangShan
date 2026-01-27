# Difftest & Snapshot (Design)

## Purpose
Enable difftest-assisted debug workflows and simulator snapshot save/restore.

## Notes
- Snapshot is provided by the simulator when built with snapshot support.
- Snapshot workflows are configured in the simulator layer (see `difftest/README.md`).

## Related commands/flags
- `xexpdiffstate`
- `--fork-interval` (emu.py)
