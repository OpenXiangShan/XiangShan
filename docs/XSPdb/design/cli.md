# CLI and Session Control (Design)

## Purpose
Provide a GDB-like interactive debugging interface based on Python `pdb`, with XiangShan-specific commands.

## Notes
- Commands are available as `x*` verbs.
- `xcmds` and `xapis` list available commands and APIs.
- `xui` opens the Textual-based TUI (memory disassembly + summary on top, console + stdout/stderr stream on bottom).
- Use `xui save` / `xui load` to persist and restore TUI layout/theme (stored in `~/.xspdb/xui.json`).
- Use `xui goto <addr>` to set the forced memory address for the disassembly view.
- Use `xtheme` to list/cycle/set themes while TUI is active.
- `xreset` resets DUT.

## Related commands
- `xcmds`, `xapis`, `xui`
- `xstep`, `xistep`, `xreset`
- `xpc`, `xexpdiffstate`, `xexportself`
- `xlist_xclock_cb`
