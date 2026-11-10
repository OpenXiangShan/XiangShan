# TraceRTL

## 1. Overview

[TraceRTL](https://ieeexplore.ieee.org/document/11408626)[^1] introduces an agile,
trace-driven performance evaluation methodology that decouples the functional and
performance components of a CPU RTL design. Following this idea, we adapted XiangShan
to support both trace-driven and execution-driven simulation, with minimal switching
overhead and zero side effects on the mainline: trace-driven mode is selected at
elaboration by `--trace-rtl`, and without the flag the generated Verilog is the same
as mainline. Below, we call trace-driven XiangShan simply TraceRTL.

TraceRTL is a performance exploration framework that bypasses full functional
correctness while preserving performance accuracy. It does so in two steps.

First, TraceRTL runs on an **instruction trace stream** instead of a program image.
The trace is a sequence of records, one per instruction on the correct path, captured
by NEMU/QEMU while running the program. Each record holds the information that may
affect performance: the PC, the encoding, the control-flow outcome and target,
exceptions and interrupts, and memory addresses.

Then, TraceRTL injects the golden data from the trace into the modules that produce
performance-critical events. Everything else (rename, issue, branch prediction,
caches, TLBs, prefetchers, DRAM) still runs as real RTL, while register values and
load / store data are don't care.

- **Execution flow.** The PC and instruction encoding come directly from the trace.
  A module inside the IFU aligns trace instructions onto the fetch blocks that the
  BPU and FTQ predict, and its packet replaces the one built from ICache data. Since
  the encodings are real, rename and issue see the true dependencies.
- **Control flow.** The BranchUnit takes the taken bit from the trace and the
  JumpUnit takes the target. The IFU checks each prediction against the trace and
  redirects the BPU when it is wrong. The trace holds only the correct path, so on a
  wrong path the IFU stalls until the backend redirect arrives, and wrong-path
  instructions become bubbles. After every redirect, the trace reader rewinds to the
  instruction that caused it.
- **Exceptions and interrupts.** A traced exception is raised at decode as an
  illegal instruction, and the CSR unit sends traps and `xret`s to the traced target.
  All other exception sources (TLB, PMP, CSR permission, misalignment) are silenced,
  so the RTL traps exactly where the trace did. Interrupts, and the jump from the
  reset vector to the first traced PC, are force-jump records that pass through the
  pipeline like a trapping instruction.
- **Memory access.** Load, store and AMO units take the traced VA and PA. The TLBs
  still look up, miss and walk, so translation latency is kept, but the translated
  address comes from the trace's VA→PA map, and the page walker reads PTEs from a page
  table built from that map. Translation and PMP/PMA faults are disabled.
- **Data-dependent latency.** Div and fdiv/fsqrt take their source operands from the
  trace, so their latency matches the real program.

Finally, since there is no reference model, the trace is also the reference. Every
cycle the C++ runtime checks that the ROB commits exactly the traced instructions in
order, and reports the run as stuck if nothing commits for 15000 cycles. A run that
commits the whole trace (or reaches `-I`) exits with 0; a failed check prints the
recent trace and DUT history and exits with 1.

The module map and how the Scala and C++ sides fit together are described in
[design/implementation.md](design/implementation.md).

## 2. Usage

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

## 3. Known limitations

- Wrong-path instructions are not executed (see §1). Their effects on the backend
  and on the data caches are therefore not modelled.
- NEMU's interrupt records are not replayed yet.
- CSR state that a program loads from memory is not modelled, since there is no
  program image. One example is restoring `mstatus.FS` from a saved context.
- AMOs take their address from the trace but do not access the DCache.
- CI covers scalar integer and FP traces only.

[^1]: Z. Zhang, Y. Xu, S. Wang, D. Tang and Y. Bao, "TraceRTL: Agile Performance Evaluation for Microarchitecture Exploration," 2026 IEEE International Symposium on High Performance Computer Architecture (HPCA), Sydney, Australia, 2026, pp. 1-15, doi: 10.1109/HPCA68181.2026.11408626. keywords: {Performance evaluation;Out of order;Couplings;Microarchitecture;Accuracy;Source coding;Computer architecture;Internet;Chip scale packaging},

