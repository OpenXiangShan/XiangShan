# Waveform Control (Design)

## Purpose
Enable/disable waveform dump with minimal overhead, and support copying/continuing wavefiles.

## Notes
- Waveform is paused by default during initialization.
- A default waveform filename is generated at init and printed in the log.
- `xwave_on` (no argument) resumes to the current/default file.
- `xwave_on <file>` sets a new file and resumes dump.
- `xwave_off` pauses dump but does not remove the file.
- `xwave_continue <src>` copies `src` to the current default file, then resumes (requires default file set).

## Related commands
- `xwave_on`, `xwave_off`, `xwave_flush`, `xwave_continue`
