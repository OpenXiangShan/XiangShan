# Building the TraceRTL emulator

TraceRTL runs XiangShan from a recorded instruction trace. The `TraceRTL`
submodule builds the trace-mode emulator against the parent XiangShan workspace
without changing XiangShan's Makefile or difftest sources.

TraceRTL's XiangShan build invokes upstream difftest's makefiles and reuses its
emulator sources, replacing only the emulator entry point and adding the
TraceRTL runtime. It does not require a difftest fork or a TraceRTL-specific
difftest revision; use the difftest commit pinned by XiangShan.

## Prerequisites

From the XiangShan root, initialize the pinned submodules:

```bash
make init
```

Install Mill and either:

- GSIM for `SIM=gsim` (the default), with `gsim` on `PATH` or `GSIM_BIN` set to
  its absolute path; or
- Verilator and clang++ for `SIM=verilator`.

DRAMsim3 is optional. To enable it, set `WITH_DRAMSIM3=1` and
`DRAMSIM3_HOME=/absolute/path/to/DRAMsim3` in the build and runtime environment.

## Build

Build from the XiangShan root:

```bash
# GSIM: sim-chirrtl -> gsim -> clang++
make -C TraceRTL MODE=xiangshan SIM=gsim emu -j64 \
  WITH_DRAMSIM3=1 DRAMSIM3_HOME=/absolute/path/to/DRAMsim3 \
  EMU_THREADS=1 GSIM_BIN=/absolute/path/to/gsim

# Verilator: sim-verilog -> verilator -> clang++
make -C TraceRTL MODE=xiangshan SIM=verilator emu -j64 \
  WITH_DRAMSIM3=1 DRAMSIM3_HOME=/absolute/path/to/DRAMsim3 \
  EMU_THREADS=1
```

`NOOP_HOME` defaults to the parent of TraceRTL, which is the XiangShan root. Set
it explicitly when building from another location. The emulator is written to
`$NOOP_HOME/build/emu`.

Both simulators elaborate into `$NOOP_HOME/build`. TraceRTL stamps the
elaboration arguments and regenerates stale RTL when the simulator or arguments
change. Do not run exec-mode and trace-mode builds concurrently in the same
worktree; preserve the resulting emulator before switching modes.

Forwarded variables are `CONFIG`, `NUM_CORES`, `WITH_DRAMSIM3`,
`DRAMSIM3_HOME`, `WITH_CHISELDB`, `WITH_CONSTANTIN`, `EMU_THREADS`,
`EMU_OPTIMIZE`, `GSIM_BIN`, `VERILATOR_CXX` (default `clang++`), and
`XS_SIM_ARGS` (additional elaboration arguments next to `--trace-rtl`). Other
make variables are not forwarded.

For example, a GSIM build without DRAMsim3, ChiselDB, or Constantin is:

```bash
make -C TraceRTL MODE=xiangshan SIM=gsim emu -j64 \
  WITH_CONSTANTIN=0 WITH_CHISELDB=0 EMU_THREADS=1 EMU_OPTIMIZE=-O3
```

## Run

Run a trace with difftest disabled:

```bash
$NOOP_HOME/build/emu --no-diff \
  --tracertl-file /absolute/path/to/trace.zst \
  -W 20000000 -I 40000000
```

A fully committed trace exits with status 0 after `TraceRTL Dump`. A TraceRTL
check failure exits nonzero. Additional options include `--tracept-file`,
`--gen-paddr`, `--fast-warmup`, `--skip-traceinstr`, `--max-cycles`, and
`--max-instr`. Normal regression replay should not use `--fast-warmup` or
`--skip-traceinstr`.

See `TraceRTL/README.md` for the standalone Hollow build and the complete
runtime option list.
