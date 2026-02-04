# CLI Batch Execution (Design)

## Purpose
Provide a command-line interface for XSPdb with comprehensive support for batch execution, waveform control, difftest, and automated testing scenarios.

## Design Overview

### Execution Modes
The CLI supports multiple execution modes:
- **Interactive**: Manual command input with full debugging capabilities
- **Batch**: Automated execution with scripts or replay files
- **Mixed**: Combine batch execution with interactive breakpoints

### Configuration Model
The CLI uses a hierarchical configuration system:
1. **Default Values**: Built-in defaults for most parameters
2. **Command Line**: Override defaults with CLI arguments
3. **Environment Variables**: System-wide configuration via environment
4. **Makefile Integration**: Convenient build system integration

### Execution Flow
1. **Initialization**: Parse arguments, initialize DUT, setup XSPdb
2. **Configuration**: Apply memory, flash, and simulator settings
3. **Setup**: Load binaries, configure breakpoints, inject commands
4. **Execution**: Run simulation with monitoring and callbacks
5. **Teardown**: Flush waveforms, export logs, cleanup resources

## Key Features

### Batch Execution
- **Script Mode**: Execute command sequences from text files
- **Replay Mode**: Reproduce sessions from log files
- **Commit Counting**: Run until specified number of instructions commit
- **Time Limits**: Enforce maximum execution time

### Waveform Control
- **Cycle-Based Recording**: Record waveform between specific cycles
- **Automatic Callbacks**: Enable/disable waveform at runtime
- **File Management**: Custom output paths and file rotation
- **Performance Optimization**: Minimal overhead when disabled

### DiffTest Integration
- **Reference Comparison**: Compare against reference simulator
- **Commit-Level Validation**: Verify instruction commits
- **Flexible Configuration**: Custom first instruction address
- **Error Detection**: Automatic mismatch detection

### Command Injection
- **Pre-Execution Commands**: Inject commands before simulation starts
- **Post-Execution Commands**: Inject commands after completion
- **Multi-Line Support**: Execute multiple commands in sequence
- **Debugging Hooks**: Add debugging points without modifying scripts

### Memory Management
- **Dynamic Sizing**: Configure RAM size at runtime
- **Region Configuration**: Set memory and flash base addresses
- **Flash Loading**: Initialize flash contents for boot scenarios
- **Memory Export**: Dump memory state for analysis

## Performance Considerations

### Overhead Analysis
- **Waveform Disabled**: ~0% overhead
- **Waveform Enabled**: ~5-10% overhead
- **DiffTest Enabled**: ~10-20% overhead
- **Full Debug Mode**: ~15-25% overhead

### Optimization Strategies
- **Selective Waveform**: Enable only around critical regions
- **Batch Operations**: Group similar operations together
- **Efficient Logging**: Use appropriate log levels
- **Minimal Watchpoints**: Reduce signal monitoring overhead

## Related Files
- `scripts/xspdb/cli_parser.py` - Command line argument parser
- `scripts/xspdb/xspdb.py` - XSPdb core implementation
- `scripts/pdb-run.py` - Main entry point
- `scripts/Makefile.pdb` - Build system integration

## Notes
- CLI parameters are processed in order: defaults → environment → command line
- Waveform callbacks use cycle counting for precise control
- Command injection happens before/after script/replay execution
- Time limits are enforced at cycle boundaries
- Memory configuration affects allocation and initialization

## Related CLI Options
- Basic: `-i`, `-v`, `-h`
- Execution: `-c`, `-t`, `--max-run-time`, `--no-interact`
- Memory: `--mem-base-address`, `--ram-size`, `--flash-base-address`
- Waveform: `-b`, `-e`, `--wave-path`
- Script: `-s`, `-r`, `-bi`
- DiffTest: `--diff`, `--diff-first-inst-address`, `-pc`
- Flash: `-F`
- Commands: `--cmds`, `--cmds-post`
- Logging: `-l`, `--log-file`, `--log-level`, `--debug-level`
- Simulator: `--sim-args`
- Tracing: `--trace-pc-symbol-block-change`