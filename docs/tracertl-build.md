# Building the TraceRTL emulator

TraceRTL runs XiangShan from a recorded instruction trace. The `TraceRTL`
submodule builds the trace-mode emu against the parent XiangShan workspace,
leaving the XiangShan Makefile and difftest unchanged.

## Prerequisites

- Initialize the submodule: `git submodule update --init TraceRTL`
- A GSIM install on `PATH` (or point `GSIM_BIN` at it) for `SIM=gsim`, or
  Verilator and clang++ for `SIM=verilator`
- Optionally DRAMsim3 (`WITH_DRAMSIM3=1 DRAMSIM3_HOME=/abs/path`)

## Build

From the XiangShan root:

```bash
# GSIM (default): sim-chirrtl -> gsim -> clang++
make -C TraceRTL MODE=xiangshan SIM=gsim emu -j64 \
  WITH_DRAMSIM3=1 DRAMSIM3_HOME=/abs/path/to/DRAMsim3 \
  EMU_THREADS=1 GSIM_BIN=/abs/path/to/gsim

# Verilator: sim-verilog -> verilator
make -C TraceRTL MODE=xiangshan SIM=verilator emu -j64 \
  WITH_DRAMSIM3=1 DRAMSIM3_HOME=/abs/path/to/DRAMsim3 EMU_THREADS=1
```

`NOOP_HOME` defaults to the parent of TraceRTL (the XiangShan root); set it
explicitly when building from elsewhere. The emu is written to
`$NOOP_HOME/build/emu`; both simulators elaborate into `$NOOP_HOME/build`, so
switching `SIM` in one workspace removes and regenerates the RTL.

Forwarded variables: `CONFIG`, `NUM_CORES`, `WITH_DRAMSIM3`, `DRAMSIM3_HOME`,
`WITH_CHISELDB`, `WITH_CONSTANTIN`, `EMU_THREADS`, `EMU_OPTIMIZE`, `GSIM_BIN`,
`VERILATOR_CXX` (default `clang++`) and `XS_SIM_ARGS` (extra elaboration
arguments next to `--trace-rtl`). Other make variables are not passed down.

## Run

```bash
$NOOP_HOME/build/emu --no-diff --tracertl-file /abs/path/to/trace.zst
```

A fully committed trace exits 0 after `TraceRTL Dump`; a trace-check failure
aborts with exit code 1. Additional options include `--tracept-file`,
`--gen-paddr`, `--fast-warmup`, `--skip-traceinstr`, `--max-cycles` and
`--max-instr`.

See `TraceRTL/README.md` for the standalone Hollow build and the full runtime
option list.
