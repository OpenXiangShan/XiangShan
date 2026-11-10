# Waveform Examples

```text
# use default file (generated at init)
xwave_on
xstep 1000
xwave_off

# switch to a specific file
xwave_on /abs/path/to/run1.fst
xstep 200
xwave_off

# continue by copying a fork-backup waveform into your default file
xwave_on /abs/path/to/own.fst
xwave_continue /abs/path/to/fork_wave_YYYYMMDD_HHMMSS_PID.fst
xstep 100
xwave_flush
xwave_off
```
