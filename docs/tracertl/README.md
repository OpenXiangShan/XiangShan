# TraceRTL

[TraceRTL](https://ieeexplore.ieee.org/document/11408626)[^1] runs the XiangShan RTL on an **instruction trace** instead of a program
image. The trace records, for every instruction on the correct path, what decides
performance: the PC, the encoding, the control-flow outcome, and the memory address.
The RTL replays these records cycle by cycle. Register values and execution results
are not computed faithfully. However, every decision that affects performance takes the
traced value, so the pipeline behaves as it would on the real program.

This page covers the idea, the high-level design and the basic usage. The
implementation, with the module map and how the Scala and C++ sides fit together,
is in [design/implementation.md](design/implementation.md).

## 1. Motivation

XiangShan performance work happens in two places:

- **Architecture exploration (gem5).** New ideas are modelled and measured on SPEC
  slices quickly. However, callibration with RTL takes a lot of effort.
- **RTL implementation.** The real design is measured by booting checkpoints with a
  full, functionally correct system: memory image, devices and difftest. A
  performance feature can only be measured once everything around it executes
  correctly, which may also be a time consuming process.

TraceRTL bridges the two: it enables feature exploration directly on the RTL, and
classifies modules as either *performance correct* or *functionally correct*. The
RTL stays performance correct, while functional correctness is relaxed.

## 2. Core idea: relax functional correctness, keep performance correctness

An out-of-order core's performance depends on a small set of facts about each instruction.
TraceRTL takes those facts from the trace. Everything else runs as real RTL.

| What performance depends on | Where it comes from in TraceRTL |
| --- | --- |
| Instruction stream (PC, encoding) | trace, injected at the IFU |
| Branch direction and jump target | trace |
| Exceptions, interrupts, trap and xret targets | trace |
| Load, store and AMO addresses (VA and PA) | trace |
| Data-dependent latency (div, fdiv/fsqrt operands) | trace |
| Register dependencies, structural hazards, queues | real RTL: the encodings are real, so rename and issue see the true dependencies |
| Branch prediction, caches, TLBs, page walks, prefetchers, DRAM | real RTL, driven by the traced addresses |
| Register values, load data, store data | don't care |

### What the trace records

The trace is produced by NEMU's trace writer (`--tracefile`) and compressed with
zstd. It holds one record per instruction on the correct path:

| Field | Meaning |
| --- | --- |
| `pcVA`, `pcPA` | instruction address |
| `inst` | instruction encoding (RVC or RVI) |
| `branchType`, `branchTaken`, `target` | control flow: is it a CFI, is it taken, where it goes (also the trap / xret target) |
| `exception` | non-zero if the instruction traps; bit 7 marks an interrupt or synthetic force-jump |
| `memoryType`, `memorySize`, `memoryAddrVA`, `memoryAddrPA` | load / store / AMO / LR / SC and their address. For div and fdiv, the two address words carry the source operands instead |
| `InstID` | sequence number, starting at 1 |

## 3. High-level design

TraceRTL addresses five problems. The rest of the core stays unchanged.

### 3.1 Injecting the trace into the pipeline

TraceRTL hijacks the **IFU**. A `TraceRTL` module inside the IFU builds, from the
trace, the fetch packet the IFU would otherwise build from ICache data. From then on
the IFU talks to the IBuffer and the backend exactly as in normal execution.

1. **Read.** A 128-entry trace buffer is refilled from the C++ runtime over DPI-C,
   32 instructions at a time.
2. **Align.** The BPU and FTQ still predict fetch blocks as usual. For each predicted
   block (up to two per cycle), TraceRTL maps the next trace instructions onto the
   block's halfword slots, following the traced PCs.
3. **Check the prediction.** The trace equivalent of the IFU's PredChecker compares
   the prediction with the traced instructions. For example, it catches a `jal` that
   was not predicted taken, or a predicted-taken slot that holds no branch. When the
   prediction is wrong, it redirects the BPU, as a predecode fault would.
4. **Deliver.** The checked packet replaces the IFU's own aligned packet. The trace
   record of each instruction (`traceInfo`) travels with it down the pipeline.

The trace contains only the correct path. When the frontend runs down a wrong path,
the fetch block no longer matches the trace, and the IFU **stalls** until the
redirect arrives. Wrong-path instructions therefore become bubbles instead of being
executed.

### 3.2 Control flow from the trace

Without real register values, the core cannot compute branch outcomes, so they are
replaced by the traced ones:

- **Conditional branches.** The BranchUnit takes `taken` from the trace and
  mispredicts when the prediction disagrees.
- **Jumps.** The JumpUnit, and the IFU checker when it redirects a jump, use the
  traced `target` instead of computing it.
- **Exceptions.** A traced exception is raised at decode as an illegal instruction.
  The CSR unit sends the trap to the traced target. All other exception sources
  (TLB, PMP, CSR permission, misalignment) are silenced, so the RTL traps exactly
  where the trace did. `xret` targets come from the trace too.
- **Force jumps.** Records with `exception` bit 7 set (interrupts, and the
  synthetic jump from the reset vector to the first traced PC) pass through the
  pipeline like a trapping instruction.

Every redirect carries the `traceInfo` of the instruction that caused it. The trace
reader then **rewinds** to the right instruction:

- to `InstID` if the redirect re-executes the instruction itself (a load replay, for
  example);
- otherwise to `InstID + 1`.

### 3.3 Memory from the trace

- **Addresses.** Load, store and AMO units take the traced VA and PA instead of
  computing them.
- **Translation.** The TLBs still perform their lookups, misses and page walks, so
  translation latency is kept. The translated address comes from the trace's own
  VA→PA map. The page walker reads PTEs from a page table that the runtime builds
  from that map (or loads from a `--tracept-file`).
- **Faults.** Translation and PMP/PMA faults are disabled.

### 3.4 Checking the replay instead of difftest

There is no reference model. The trace is the reference, and the C++ runtime checks
three things every cycle:

- **commit check:** every instruction the ROB commits must be the next one in the
  trace;
- **drive check:** every instruction the IBuffer sends to decode must be the next one
  the frontend was given;
- **progress check:** if nothing commits for 15000 cycles, the run is reported as
  stuck.

A run that commits the whole trace (or reaches `-I`) ends with `TraceRTL Dump` and
exit code 0. A failed check prints the recent trace and DUT history and exits with 1.

### 3.5 Keeping execution mode unchanged

TraceRTL is selected at elaboration by `--trace-rtl`. Every hook is gated:

- through `TraceRTLChoose(default, trace)` or `TraceRTLDontCareValue(x)`;
- through `traceInfo` fields that are `Option`s and are absent in execution mode;
- or through a plain `if (env.TraceRTLMode)`.

Without the flag, the generated Verilog is the same as mainline.

## 4. Usage

### Build

From the XiangShan root, with the `TraceRTL` submodule initialised:

```bash
# through xiangshan.py (GSIM or Verilator)
python3 scripts/xiangshan.py --build --tracertl --emulator gsim \
  --with-dramsim3 --dramsim3 /path/to/DRAMsim3

# or directly
make -C TraceRTL MODE=xiangshan SIM=gsim emu -j64 \
  WITH_DRAMSIM3=1 DRAMSIM3_HOME=/path/to/DRAMsim3 EMU_THREADS=1 GSIM_BIN=/path/to/gsim
make -C TraceRTL MODE=xiangshan SIM=verilator emu -j64 \
  WITH_DRAMSIM3=1 DRAMSIM3_HOME=/path/to/DRAMsim3 EMU_THREADS=1
```

The emu is written to `build/emu`. The build elaborates XiangShan with `--trace-rtl`
and uses difftest's own makefiles. Only `main.cpp` is replaced by the TraceRTL
harness.

The TraceRTL build removes `build/rtl` if that RTL was not elaborated by it. To
keep both an exec emu and a TraceRTL emu, build them one after the other and copy
`build/emu` away in between, as the CI job does.

### Run

```bash
./build/emu --no-diff --tracertl-file /path/to/workload.trace.zstd \
  [-W <warmup insts>] [-I <max insts>] [--tracept-file /path/to/pt.zstd]
```

| Option | Meaning |
| --- | --- |
| `--tracertl-file` | the trace (required) |
| `--tracept-file` | page table recorded with the trace; without it, one is generated from the trace's VA→PA map |
| `--fast-warmup N` (not supported now) | squash duplicate instructions in the first N instructions to warm up faster; `-W` / `-I` are adjusted by the squashed count |
| `--skip-traceinstr N` | start from instruction N of the trace |
| `--gen-paddr` | allocate physical addresses for traces that do not record them |

All other difftest emu options (`-W`, `-I`, `-C`, `--dump-wave`, …) work as usual.
Performance counters are printed at the end, as in a normal run.

### Tests and CI

- **Ready-made traces** are in `/nfs/share/tracertl` (see its `README.txt`):
  - `tests/*.trace.zstd`: exception, privilege and FP-context corner cases;
  - `coremark/coremark-riscv64-xs.trace.zstd`: CoreMark.
- **Run them** with `python3 scripts/xiangshan.py --tracertl --ci tracertl-tests`
  (or `--ci coremark`).
- **The CI job** `EMU - TraceRTL` in `.github/workflows/emu.yml` runs both suites.
  It also runs CoreMark on an exec emu built with the same flags, and fails if the
  TraceRTL IPC drifts too far from the exec IPC.

## 5. Known limitations

- Wrong-path instructions are not executed (see 3.1). Their effects on the backend
  and on the data caches are therefore not modelled.
- NEMU's interrupt records are not replayed yet.
- CSR state that a program loads from memory is not modelled, since there is no
  program image. One example is restoring `mstatus.FS` from a saved context.
- AMOs take their address from the trace but do not access the DCache.
- CI covers scalar integer and FP traces only.

[^1]: Z. Zhang, Y. Xu, S. Wang, D. Tang and Y. Bao, "TraceRTL: Agile Performance Evaluation for Microarchitecture Exploration," 2026 IEEE International Symposium on High Performance Computer Architecture (HPCA), Sydney, Australia, 2026, pp. 1-15, doi: 10.1109/HPCA68181.2026.11408626. keywords: {Performance evaluation;Out of order;Couplings;Microarchitecture;Accuracy;Source coding;Computer architecture;Internet;Chip scale packaging},

