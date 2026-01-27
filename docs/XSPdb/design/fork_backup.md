# Fork Backup Waveform (Design)

## Purpose
Keep a "lagging" child process so that when an xbreak fires, a waveform window around the bug is dumped without heavy overhead during normal execution.

## Notes
- The child process sleeps until woken by an xbreak in the parent.
- The child enables waveform, runs until it reaches the same break, then flushes and exits.
- Logs are written to `fork_backup.log` and `fork_backup_parent.log` by default.
- Waveforms are written to the current working directory by default, and only the latest 2 files are kept.
- Works on the `xstep` path (interactive/TUI), not the `emu.py` batch loop.

## Related commands
- `xfork_backup_on`, `xfork_backup_off`, `xfork_backup_status`
