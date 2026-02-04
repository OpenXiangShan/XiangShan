# Waveform Control (Design)

## Purpose
Enable/disable waveform dump with minimal overhead, support file management, and provide waveform continuation capabilities for efficient debugging.

## Design Overview

### Waveform States
Waveform control manages three primary states:
- **Off**: Waveform dump is disabled, no overhead
- **On**: Waveform dump is active, signals are recorded
- **Paused**: Dump is temporarily suspended but file remains open

### File Management
- **Default File**: Automatically generated at initialization with timestamp
- **Custom Files**: Users can specify custom waveform file paths
- **File Continuation**: Copy existing waveforms to continue recording
- **Flush**: Force write pending waveform data to disk

### Performance Characteristics
- **Off State**: Zero performance overhead
- **On State**: Minimal overhead (~5-10% performance impact)
- **Paused State**: No overhead but maintains file handle
- **Flush Operation**: Brief pause while data is written to disk

### Waveform Format
- Uses `.fst` (Fast Signal Trace) format for efficiency
- Supports signal hierarchy and value changes
- Compatible with standard waveform viewers (GTKWave, etc.)
- Automatic compression for reduced disk usage

## Notes
- Waveform is paused by default during initialization
- A default waveform filename is generated at init and printed in the log
- `xwave_on` (no argument) resumes to the current/default file
- `xwave_on <file>` sets a new file and resumes dump
- `xwave_off` pauses dump but does not remove the file
- `xwave_continue <src>` copies `src` to the current default file, then resumes (requires default file set)
- Waveform files can grow large; use `xwave_off` when not needed
- Use `xwave_flush` to ensure data is written to disk before long pauses

## Related commands
- `xwave_on` - Enable waveform dump
- `xwave_off` - Disable waveform dump
- `xwave_flush` - Flush pending waveform data to disk
- `xwave_continue` - Continue from existing waveform file
- CLI options: `-b`, `-e`, `--wave-path` for batch mode waveform control

## Implementation Details
- Waveform recording uses simulator-level signal tracing
- File operations are buffered for performance
- Flush ensures data persistence across system failures
- File continuation preserves signal hierarchy and timing
