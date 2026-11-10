# Difftest & Snapshot (Design)

## Purpose
Enable difftest-assisted debug workflows and simulator snapshot save/restore for efficient bug hunting and state reproduction.

## Design Overview

### Difftest Integration
- XSPdb integrates with the difftest framework to compare RTL simulation results against a reference model
- Provides real-time state comparison at instruction commit boundaries
- Allows exporting current difftest state for analysis

### Snapshot Functionality
- Simulator supports checkpoint-based snapshot save/restore when built with snapshot support
- Enables fast rollback to previous simulation states without re-running from start
- Useful for exploring different execution paths from a common point

### Workflow Integration
1. **Debug Phase**: Use difftest to identify divergence points
2. **Snapshot Phase**: Save simulator state before critical regions
3. **Analysis Phase**: Restore snapshots and explore alternative scenarios
4. **Verification Phase**: Re-run tests from saved states

## Notes
- Snapshot is provided by the simulator when built with snapshot support
- Snapshot workflows are configured in the simulator layer (see `difftest/README.md`)
- Difftest comparison happens automatically during instruction execution
- State export includes register values, memory state, and internal signals

## Related commands/flags
- `xexpdiffstate` - Export current difftest state
- `--diff` (CLI) - Path to REF shared object for difftest testing
- `--diff-first-inst-address` (CLI) - First instruction address for difftest
- `--pc-commits` (CLI) - Run until specified number of commits
- `--fork-interval` (emu.py) - Fork interval for snapshot workflows

## Implementation Details
- Difftest state is managed by the `df` (difftest) object
- Snapshot save/restore uses simulator-level checkpointing
- State export format is JSON for easy parsing and analysis
