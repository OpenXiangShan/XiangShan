# XSPdb

XSPdb is a Python `pdb`-based debugging tool for RISC-V IPs, customized for XiangShan’s difftest interface. It provides a GDB-like interactive CLI, waveform control, flexible breakpoints/triggers, and scripts for reproducible debugging. It also integrates signal-level debugging with software execution state for hardware/software co-verification.

## Most used features

- **Breakpoints & triggers**: `xbreak`, `xbreak_expr`, `xbreak_fsm`
- **Waveform control**: `xwave_on/off/flush`, `xwave_continue`
- **Fork backup waveform**: `xfork_backup_*` for xbreak-triggered wave capture
- **Visibility**: `xprint`, `xwatch`, `xset`, `xpc`

## Layout

- `design/`  Design and specification notes
  - `design/cli.md`
  - `design/step.md`
  - `design/breakpoints.md`
  - `design/trigger_expr_spec.md`
  - `design/trigger_fsm_spec.md`
  - `design/waveform.md`
  - `design/fork_backup.md`
  - `design/watchpoints.md`
  - `design/disasm.md`
  - `design/registers.md`
  - `design/data_io.md`
  - `design/difftest_snapshot.md`
- `examples/`  Usage examples and sample trigger files
  - `examples/cli.md`
  - `examples/step.md`
  - `examples/breakpoints.md`
  - `examples/waveform.md`
  - `examples/fork_backup.md`
  - `examples/watchpoints.md`
  - `examples/disasm.md`
  - `examples/registers.md`
  - `examples/data_io.md`
  - `examples/difftest_snapshot.md`
  - `examples/xbreak_expr_examples.md`
  - `examples/xspdb_script_example.txt`
  - `examples/csr_trace.fsm`
  - `examples/pc_sequence.fsm`

## Quick start (interactive)

```text
xcmds
xpc
xload /abs/path/to/bin
xwave_on
xstep 1000
xwave_off
```

## Quick start (batch)

```text
python3 scripts/pdb-run.py --script /abs/path/to/docs/XSPdb/examples/xspdb_script_example.txt
```

## Notes

- `xfork_backup_*` works on the `xstep` path (interactive/TUI), not the `emu.py` batch loop.
- Disassembly uses `spike-dasm` first, with a `capstone` fallback.
- Use `xcmds` and `xapis` to list available commands/APIs and their modules.
