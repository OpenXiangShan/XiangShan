# Watchpoints and Visibility (Design)

## Purpose
Track signal changes or state in real time during simulation and provide ad-hoc inspection and modification capabilities for debugging and verification.

## Design Overview

### Watchpoint Mechanism
Watchpoints monitor signals and report changes during simulation:
- **Signal Watchpoints**: Monitor any RTL signal for value changes
- **Commit PC Watchpoints**: Track program counter changes at instruction commit boundaries
- **Real-time Monitoring**: Watchpoints are checked on every simulation step
- **Automatic Reporting**: Changes are reported automatically when detected

### Visibility Tools
XSPdb provides multiple ways to inspect and control simulation state:
- **xprint**: Read and display signal values
- **xset**: Modify signal values dynamically
- **xpc**: Get current program counter with software-synchronized view

### Software-Hardware Synchronization
- Hardware state is synchronized with software view
- PC and registers reflect accurate simulation state
- Watchpoints provide real-time visibility into internal signals
- Enables correlation between software execution and hardware behavior

### Performance Considerations
- Watchpoints add minimal overhead when actively monitored
- Multiple watchpoints can be set simultaneously
- Unwatch removes monitoring overhead completely
- Commit PC monitoring is optimized for performance

## Notes
- `xwatch`/`xunwatch` for arbitrary signals
- `xwatch_commit_pc` for commit PC monitoring
- `xprint`/`xset` for ad-hoc inspection and control
- `xpc` and related helpers provide a software-synchronized view of hardware state
- Watchpoints persist until explicitly removed with `xunwatch`
- Signal changes are reported at the moment they occur
- Nested signal hierarchies can be watched using full path names

## Related commands
- `xwatch` - Set signal watchpoint
- `xunwatch` - Remove signal watchpoint
- `xwatch_commit_pc` - Enable commit PC monitoring
- `xunwatch_commit_pc` - Disable commit PC monitoring
- `xprint` - Print signal value
- `xset` - Set signal value
- `xpc` - Get current program counter

## Implementation Details
- Watchpoints use signal path matching for identification
- Value comparison is done on each simulation cycle
- Commit PC watchpoints use hardware commit signals
- Signal modification through `xset` takes effect immediately
