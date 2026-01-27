# CLI and Session Control (Design)

## Purpose
Provide a GDB-like interactive debugging interface based on Python `pdb`, with XiangShan-specific commands.

## Notes
- Commands are available as `x*` verbs.
- `xcmds` and `xapis` list available commands and APIs.
- `xui` opens a simple TUI.
- `xreset` resets DUT.

## Related commands
- `xcmds`, `xapis`, `xui`
- `xstep`, `xistep`, `xreset`
- `xpc`, `xexpdiffstate`, `xexportself`
- `xlist_xclock_cb`
