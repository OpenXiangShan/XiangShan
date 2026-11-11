# TraceRTL implementation

TraceRTL replaces every value that XiangShan cannot compute without real register
contents with a value taken from an instruction trace. The rest of the RTL keeps its
own cycle behaviour. [The overview](../README.md) explains why. This document explains how:

- for each intention in the overview, which code implements it;
- how the Scala hardware, the DPI-C boundary and the C++ runtime fit together.

Sections follow the order of the overview:

1. the layout;
2. the switch that keeps exec mode intact;
3. the trace record;
4. how the trace enters the IFU;
5. how it travels through the core;
6. how the backend, memory and CSR use it;
7. the C++ runtime that serves and checks it;
8. the build.

## 1. Layout

TraceRTL has four parts. Arrows show who calls whom during a simulation cycle.

```
 XiangShan RTL (elaborated with --trace-rtl)              TraceRTL submodule (C++)
 ────────────────────────────────────────────            ─────────────────────────────────────
 frontend/tracertl/*.scala                                src/test/csrc/xiangshan/main.cpp
   TraceRTL (inside Ifu)                                    emu main loop: prepare_read → tick
     TraceReader ─────── TraceReaderHelper ──DPI──▶          → check_commit / check_drive
     TraceAlignParallel  TraceRedirectHelper ─DPI──▶
     TracePreDecoder                                      src/test/csrc/tracertl/
     TracePredictChecker                                    tracertl.{h,cpp}   DPI + emu API
     TraceDriver                                            TraceReader        trace lists, checks
 hooks across the core (traceInfo carried along)            TraceICache        VA→PA map, page table
   IBuffer ─── TraceDriveCollectorHelper ──DPI──▶           trace_fastsim, dedupers, decompress …
   Rob ─────── TraceCollectorHelper ───────DPI──▶
   TLB ─────── TraceFakeMMU (TraceATSHelper) ──DPI──▶
   L2TLB ───── TraceFakeDynPageTable ──────DPI──▶
   CSR/L2TLB ─ TraceFakeSatpPpn ───────────DPI──▶
```

| Part | Where | Role |
| --- | --- | --- |
| Trace frontend | `src/main/scala/xiangshan/frontend/tracertl/` | builds IFU packets from the trace |
| Hooks | about 40 files across `frontend/`, `backend/`, `mem/`, `cache/mmu/` | carry `traceInfo` and use the traced values |
| DPI boundary | `tracertl/TraceHelper.scala` | ExtModules with an inline SV DPI-C body (Verilator) and a C++ body (GSIM) |
| Runtime | `TraceRTL/src/test/csrc/tracertl/` | loads the trace, serves reads and rewinds, translates addresses, checks the DUT |
| Harness | `TraceRTL/src/test/csrc/xiangshan/main.cpp` | replaces difftest's `emu/main.cpp` |
| Build | `TraceRTL/xiangshan.mk`, `scripts/xiangshan.py --tracertl` | elaborates with `--trace-rtl` and links the harness into difftest's emu |

The trace frontend files:

| File | Content |
| --- | --- |
| `TraceParameters.scala` | `TraceRTLParameters` (`TraceRTLParamKey`) |
| `TraceUtil.scala` | the gating helpers `TraceRTLChoose`, `TraceRTLDontCareValue`, `TraceInfo` |
| `TraceBundle.scala` | the trace record `TraceInstrBundle` and the intermediate bundles |
| `TraceRTL.scala` | the top module inside the IFU; wires the five stages below |
| `TraceReader.scala` | trace buffer, refill, rewind |
| `TraceAlignParallel.scala` | maps trace instructions onto the predicted fetch blocks |
| `TracePreDecoder.scala` | predecode info from the traced encodings |
| `TracePredictChecker.scala` | the trace equivalent of `PredChecker` |
| `TraceDriver.scala` | how many trace instructions the packet consumed |
| `TraceHelper.scala` | all DPI-C ExtModules and the fake MMU / page table / satp wrappers |

The TraceRTL repository also has a *standalone* mode with a minimal Chisel DUT
named Hollow (`TraceRTL/src/main/scala/hollow`). It exercises the runtime without
XiangShan and is not covered here.

## 2. The switch: keep exec mode identical

**Problem.** TraceRTL hooks touch the IFU, decode, the execution units, the LSU, the
MMU and the CSRs. Mainline execution mode must not change, and generated C++ must not
be patched. Both GSIM and Verilator must keep working.

**Method.**

1. **One flag.** `--trace-rtl` (`top/ArgParser.scala`) sets
   `DebugOptions.TraceRTLMode`. Modules read it as `env.TraceRTLMode` or
   `p(DebugOptionsKey).TraceRTLMode`.
2. **Gate every hook** with one of three idioms:
   - `TraceRTLChoose(default, trace)`: an elaboration-time choice. Both arguments are
     by-name, so a trace-only operand such as `traceInfo.get` is never elaborated in
     exec mode.
   - `TraceRTLDontCareValue(x)`: `x` in exec mode and 0 in trace mode. It is used to
     silence exception and fault sources.
   - `TraceInfo()`: returns `Option[TraceInstrBundle]`, which is `None` in exec mode.
     Every bundle that carries the trace record declares `val traceInfo = TraceInfo()`
     and assigns it with `traceInfo.foreach(_ := …)`.
3. **Use `if (env.TraceRTLMode) { … }`** for blocks that only exist in trace mode,
   such as instantiating DPI helpers or overriding a register's reset value.

The parameters in `TraceRTLParameters` are:

| Parameter | Value | Meaning |
| --- | --- | --- |
| `TracePredictWidth` | 32 | halfword slots of the two-block fetch window (64 B) |
| `TraceFetchWidth` | 32 | instructions per DPI refill |
| `TraceBufferSize` | 128 | `4 × TraceFetchWidth` (the source comment saying 64 is stale) |
| `TraceVAddrWidth` / `TracePAddrWidth` | 50 / 48 | address widths of the record |
| `TraceInstIDWidth` | 64 | |
| `TraceOverrideTarget` | true | the IFU checker redirects to the traced target, not PC + offset |

## 3. The trace record

The same record exists on both sides of the DPI boundary:

- Scala: `TraceInstrBundle` in `TraceBundle.scala`;
- C++: `TraceInstruction`, the file format, and `Instruction`, the in-memory form,
  in `trace_format.h`.

| Field | Use |
| --- | --- |
| `pcVA`, `pcPA`, `inst` | the packet the IFU delivers; commit and drive checks |
| `branchType`, `branchTaken`, `target` | `isTaken`, jump / trap / xret targets |
| `exception` | `hasException = exception != 0`; `isForceJump = exception(7)` |
| `memoryType`, `memorySize`, `memoryAddrVA`, `memoryAddrPA` | LSU and AMO addresses. For div / fdiv the two address words are the operands (`arthiSrc0/1`) |
| `fastSimulation` | fast-warmup flag set by the runtime |
| `InstID` | position in the trace. 1 is the first instruction; the rewind target |

The helpers `isRVC` and `nextPc` are derived from `inst` and `pcVA`.

The C++ `Instruction` adds `inst_id` and `is_squashed` (used by fast warmup). If the
trace does not start at `0x80000000`, `pre_readfile` inserts a synthetic force jump
(`setToForceJump`, `exception = 0x81`) from the reset vector to the first traced PC.
In trace mode, `SimTop` resets the core to `0x80000000`.

## 4. Injecting the trace at the IFU

**Problem.** The IFU must deliver the traced instructions. Meanwhile the BPU, FTQ,
ICache and IBuffer must keep their own cycle behaviour, and the IFU's interfaces must not
change.

**Solution.** The `TraceRTL` module is instantiated inside the IFU
(`ifu/Ifu.scala`):

```scala
traceRTL = Option.when(env.TraceRTLMode)(Module(new TraceRTL))
```

It sees the same two predicted fetch blocks as the IFU. It produces three things:

- `aligned`, the packet the IFU would have built;
- `checker`, the checker response;
- two stall signals.

Naming: TraceRTL names its stages one higher than the IFU. TraceRTL's S2 is IFU `s1`
and its S3 is IFU `s2`. The IFU stage names are used below.

### 4.1 Supplying instructions: `TraceReader`

1. **Refill.** When the 128-entry circular buffer has at least 32 free entries, the
   trace has warmed up (`TraceWarmUp`, 5 cycles after reset) and there is no
   redirect, `TraceReaderHelper` calls `trace_read_one_instr` 32 times on the
   negative clock edge. The buffer then advances `enqPtr` by 32.
2. **Expose.** The 32 entries starting at `deqPtr` are shown to the aligner. They are
   valid only when 32 entries are buffered. When the IFU consumes a packet this
   cycle, a look-ahead pointer (`visibleDeqPtr = deqPtr + recv.instNum`) already
   shows the next 32. This avoids a bubble between packets.
3. **Consume.** `deqPtr` advances by `TraceDriver`'s `recv.instNum`.
4. **Rewind on a backend redirect.**
   - The buffer is cleared.
   - `TraceRedirectHelper` calls `trace_redirect` with the redirect's `traceInfo.InstID`,
     or with `InstID + 1` if the redirect does not flush the instruction itself, or
     if the instruction is a trap or a force jump. A trap transfers control *after*
     the trapping record, even though the ROB flushes it.
   - The runtime then re-serves the trace from that instruction (§7.2).
5. **Self-checks** (`XSError`): consecutive `InstID`s, the first PC is
   `0x80000000`, and the first `InstID` is 1.

IFU-internal redirects (predecode / checker faults) do **not** rewind the trace.
The packet only consumed the instructions it actually delivered, so the rest is
still at the head of the buffer.

### 4.2 Aligning onto the fetch window: `TraceAlignParallel`

**Problem.** The predicted window is up to two fetch blocks, of up to 32 halfwords in
total. The trace is a list of instructions. TraceRTL must decide which trace
instructions belong to the window, at which slot each instruction ends, and where
the trace leaves the predicted path.

**Steps.** These run for each of the 32 trace instructions in parallel; each
instruction depends only on the previous one.

1. **Expected PC.** Instruction 0 must be at `block0.startAddr`, or at
   `startAddr - 2` if an RVI half is inherited (§4.5). Instruction *i* must be at
   `nextPc` of instruction *i − 1*. When instruction *i − 1* ends block 0 (at the
   predicted taken offset, or at the block's size), instruction *i* must instead be
   at `block1.startAddr`.
2. **Consecutive prefix.** `stillConsecutive(i)` holds while every PC so far matches.
   The first mismatch is where the trace leaves the predicted path.
3. **Slot.** From the PC offset to the selected block, compute the logical start slot,
   the block-local end offset, and whether the instruction is an RVI crossing from
   block 0 into block 1 (`isCrossBlockInstr`).
4. **Range check.** Keep the instruction only if it starts inside the selected
   block's predicted range.
5. **Incomplete RVI.** An RVI whose second half is past the end of the window is
   marked and left out of the packet; this sets `concede2Bytes` (§4.5).
6. **Compact.** One-hot slot positions are merged into `traceRange`, the occupied
   halfwords. Valid instructions are packed into `compact` entries carrying
   `traceInfo`, `instrEndOffset`, `selectBlock`, `isPrevEndHalfRvi` and
   `isCrossBlockInstr`.
7. **Force jump.** If the head instruction is a force jump, it is always valid on its
   own (`traceForceJump`).

The outputs are `compact`, `traceRange`, `candidateCount`, `concede2Bytes` and
`traceForceJump`.

### 4.3 Checking the prediction: `TracePreDecoder` and `TracePredictChecker`

**Problem.** The real `PredChecker` finds BPU mistakes that predecode can prove, from
the fetched bytes. The trace knows more, but TraceRTL must report faults in the same
form so that the FTQ and BPU train and recover as usual.

1. **Predecode.** `TracePreDecoder` derives `PreDecodeInfo` / `brAttribute` and the
   jump offset from each traced encoding. It sets `isPredTaken` when the block's
   predicted taken offset (`ftqOffset`) equals the instruction's end offset.
2. **Find faults** (`TracePredictChecker`, stage 1). The faults are:
   - `jal`, `jalr` or `ret` not predicted taken;
   - a non-CFI predicted taken;
   - **invalidTaken**: a predicted taken offset where no traced instruction ends.

   `fixedRange` keeps the instructions up to and including the first fault.
3. **Pick the target.**
   - With `TraceOverrideTarget`, a jump redirects to the traced `target`.
   - A non-jump fault redirects to the sequential PC.
   - invalidTaken redirects to the predicted PC + 2.
4. **Stage 2.** On `wbEnable` (IFU s2 fire), register the result and emit
   `checkerRedirect` and `perfFaultType`.

Like the upstream checker, it does not report a wrong conditional-branch direction or
a wrong `jalr` target. Those reach the backend and are resolved there with the traced
outcome (§6.1).

### 4.4 Consuming: `TraceDriver`

1. Consume `PopCount(fixedRange)` instructions, or only the head one when it is a
   force jump. This is `recv.instNum`, which advances `TraceReader.deqPtr`.
2. `endWithCFI`: is the last consumed instruction a taken CFI?
3. `block`: nothing was consumed and no RVI half is pending.

### 4.5 An RVI split across packets: concede-2-bytes

**Problem.** An RVI can start on the last halfword of a window. The IFU convention is
that such a half is dropped from this packet and fetched again at the start of the
next one, flagged `isPrevEndHalfRvi`. TraceRTL must carry that state across packets.

1. The aligner marks the incomplete RVI and sets `concede2Bytes`. The instruction
   is left out of the packet.
2. `TraceRTL` keeps `pendingConcede2Bytes`. It is set when IFU `s2` fires with a
   detected concede, unless the packet ends with a taken CFI in block 0. It is
   cleared on IFU flushes and checker redirects, except an invalidTaken redirect,
   which keeps it.
3. `visibleConcede2Bytes` applies the same look-ahead as `visibleDeqPtr`, so the next
   packet sees the state the current one produces.
4. `effectiveConcede2Bytes` also requires that the trace head is an RVI at
   `block0.startAddr - 2`. It drives the aligner's `lastHalfValid`, which places the
   inherited RVI at slot 0 with end offset 0.

### 4.6 The IFU hooks

All hooks are in `ifu/Ifu.scala`. Each one is a `TraceRTLChoose` or a `foreach` on
`traceRTL`.

| Hook | Effect |
| --- | --- |
| `checkerResp` | TraceRTL's checker response replaces `PredChecker`'s |
| `traceS1Stall = io.s2Block` in `s1_fire` | hold `s1` until the trace has instructions for the window |
| `s2AlignedReg(...)` | the `s2` packet fields (`instrVec`, `instrPcVec`, `instrValid`, `instrCount`, `foldPc`, `pdInfoVec`, `jumpOffsetVec`) come from `traceRTL.io.aligned` |
| `traceS2Stall = io.block && s2_valid` | hold `s2_ready` and `toIBuffer.valid` while nothing matches |
| `s2CandidateCount` | replaces `s1_instrCount` / `s1_specInstrCount` in the IBuffer enqueue-pointer prediction |
| `toIBuffer.bits.traceInfo` | the trace records of the packet |
| `rvcExpander.fsIsOff := false` | expand compressed FP regardless of `mstatus.FS` |
| `fromIFU.*`, `ifuS1PredInfo` | IFU `s1`/`s2` valid / fire / flush, `wbEnable`, the alignment shift, and per block `startAddr`, `instRange`, `size`, `ftqOffset` (= `takenCfiOffset`) |
| `redirect.fromBackend := fromFtq.redirect` | the FTQ forwards the full `backendRedirect.bits` in trace mode (`ftq/Ftq.scala`), so `traceInfo` arrives |
| `redirect.fromIFUBPU := wbRedirect.valid` | the IFU's own redirect, for the concede state |

`shiftToIBuffer` shifts the packet by the IFU's IBuffer alignment shift (0..3), as the
IFU does with its own packet.

### 4.7 Walk-through: a mispredicted conditional branch

Suppose the BPU predicts a `beq` not taken, but the trace says taken.

1. The aligner follows the trace: after the `beq`, the expected PC is its `target`,
   which is not the next sequential slot. The packet ends with the `beq`.
2. No checker fault is raised, since a direction fault is the backend's job. The
   packet is delivered, and `TraceDriver` consumes the instructions up to the `beq`.
3. The IFU moves on to the predicted fall-through blocks. Their start PC does not
   match the trace head (the branch target), so `candidateCount = 0`. `s2Block`
   holds the IFU: **no wrong-path instruction is delivered**.
4. The BranchUnit resolves the `beq` with `traceInfo.isTaken` and mispredicts. The
   redirect carries the `beq`'s `traceInfo`.
5. The FTQ forwards it to the IFU. `TraceReader` clears its buffer and calls
   `trace_redirect(InstID + 1)`, since a branch redirect does not flush itself. The
   runtime re-serves from the target. The IBuffer is flushed too (§5).
6. The frontend refetches at the target, and the trace matches again.

## 5. Carrying `traceInfo` through the core

**Problem.** The backend needs the trace record of each instruction, and every
redirect must name the instruction it came from.

**Method.** A `traceInfo = TraceInfo()` field is added along the instruction path:

- `FetchToIBuffer` → `IBufEntry` / `IBufOutEntry` → `CtrlFlow` → decode in/out →
  `DynInst`;
- → IQ / Exu / FU bundles (scalar, and the vector-path fdiv);
- → `Redirect`, `LoadNukeQueryReq` and the `LoadQueueRAW` entries.

Every redirect producer fills it:

| Producer | Source of `traceInfo` |
| --- | --- |
| BranchUnit, JumpUnit, CSR | the uop |
| ROB `flushOut` (exceptions, interrupts, flush pipe) | the excepting uop |
| LoadQueueRAW rollback | the RAW entry |
| LoadQueueUncache rollback, LoadUnit S3 rollback | the uop |

Two hooks keep the frontend and the runtime in sync on a backend redirect:

- **IBuffer flush.** `Frontend.scala` also flushes the IBuffer on
  `backend.toFtq.redirect`, so it never holds instructions that the runtime has
  returned to the trace.
- **Decode.** `DecodeStage` drops its outputs in the redirect cycle.

## 6. Replacing computed values in the backend

### 6.1 Control flow

| Unit | Change |
| --- | --- |
| `BranchUnit` | `taken = traceInfo.isTaken`; `mispredict = predTaken =/= taken` |
| `JumpUnit` | `jumpTarget = traceInfo.target`; IAF / IPF / IGPF checks silenced |
| `CtrlBlock` | IAF / IPF / IGPF of the redirect to the frontend silenced |

### 6.2 Exceptions only from the trace

**Problem.** With wrong register values, the core would trap in places the program
never trapped, and miss the traps it did take.

1. **Silence every native source.**
   - Writeback `exceptionVec` is zeroed (`Backend.scala`).
   - TLB pf / af / gpf are zeroed.
   - PMP / PMA responses are zeroed (`PMP.scala`).
   - Load / store / AMO exceptions are zeroed.
   - CSR permission: `EX_II` / `EX_VI` are forced false and `xret` is always legal
     (`CSRPermitModule`).
2. **Raise the traced ones.** `DecodeStage` sets
   `exceptionVec(illegalInstr) := traceInfo.hasException`. The ROB then takes the
   exception when the instruction reaches the head, at the normal point in the
   pipeline.
3. **Trap to the traced target.** `NewCSR` takes `trapTargetPc` from the trapping
   uop's `traceInfo.target`, and `xretTargetPc` from the xret's `traceInfo.target`.
4. **Neutralise CSR state that would block execution.**
   - `mstatus` / `vsstatus` FS and VS reset to Clean (`MachineLevel.scala`,
     `VirtualSupervisorLevel.scala`), and their Off asserts are relaxed.
   - `mnstatus.NMIE` is false for the interrupt module, and the `csr_dbltrp_inMN`
     critical error is suppressed. NMIE is not traced, and its reset value would
     latch a false double trap.

### 6.3 Data-dependent latency

`DivUnit` and the `FDivSqrt` wrappers take their operands from
`traceInfo.arthiSrc0/1`. This keeps the early-exit latency of the real operands.

### 6.4 Memory and address translation

| Unit | Change |
| --- | --- |
| `NewLoadUnit` | S0 vaddr = `memoryAddrVA`; S1 paddr / gpaddr = `memoryAddrPA`; exceptions silenced; rollback carries `traceInfo` |
| `NewStoreUnit` | same address replacement |
| `AtomicsUnit` | rs1 = traced VA, paddr = traced PA; skips the DCache (straight to `s_cache_resp_latch` with zero data); exceptions silenced |
| `TLB` | a `TraceFakeMMU` per port replaces the paddr, only when translation is enabled (bare mode keeps vaddr as paddr); faults zeroed |
| `L2TLB` | satp forced to Sv39, ASID 0, root `TraceFakeSatpPpn`. PTW / LLPTW / HPTW refills take their data from `TraceFakeDynPageTable`; bitmap and MPT traffic keeps real memory data |
| `CSR` wrapper | the TLB satp is forced the same way |

Walk latency is real: the PTW issues its requests to the memory system as usual. Only
the returned PTE beats are replaced, so the walk reaches the page table the runtime
built (§7.4).

## 7. The C++ runtime

### 7.1 Loading the trace (`TraceReader` constructor)

1. **`pre_readfile`.**
   - Read and zstd-decompress the file, and check that it divides into whole
     records.
   - Apply `--skip-traceinstr`.
   - Prepend the reset force jump when needed, and assign `inst_id`s.
2. **`mid_construct`.**
   - Optionally allocate PAs (`--gen-paddr`).
   - Reject illegal records, and run `TraceLegalFlowChecker` on the control flow.
   - Clamp the instruction limit to the trace length.
   - Build `TraceICache` (§7.4).
3. **`post_opt`.** Fast-warmup preparation (§7.5).

### 7.2 Serving reads and rewinds

The runtime keeps the whole trace in `instList_preread` and tracks the DUT with
these lists:

| List | Holds |
| --- | --- |
| `readBuffer[32]` | the next refill, prepared before the tick |
| `pendingInstList` | instructions given to the DUT and not yet committed |
| `driveInstInput` | instructions given to the DUT and not yet driven to decode |
| `redirectInstList` | rewound instructions, served again before the rest of the trace |
| `committedInstList`, `dutCommittedInstList`, `driveInstDecoded` | recent history for error dumps |

- **`read()`.** Take the next instruction from `redirectInstList`, or else from
  `instList_preread` (skipping squashed ones), and append it to `pendingInstList`
  and `driveInstInput`.
- **`prepareRead()`.** Called by the harness before every tick. If the last
  `readBuffer` was consumed, it reads 32 more. During the tick,
  `trace_read_one_instr(idx)` only copies `readBuffer[idx]`. The DPI calls therefore
  never modify the lists while the model is running.
- **`redirect(inst_id)`.**
  - Check that `inst_id` lies inside `pendingInstList`.
  - Move every pending instruction with `inst_id` or later to the front of
    `redirectInstList`.
  - Clear `driveInstInput` and mark the read buffer for reload.

### 7.3 Checks

- **`checkCommit`** consumes `trace_collect_commit` (from the ROB: pc, inst and
  `instNum`, per commit slot; a compressed ROB entry commits several instructions).
  1. Pop leading trap / force-jump records: they never commit.
  2. The head of `pendingInstList` must match the pc and inst.
  3. Retire `instNum` instructions.
  4. A mismatch sets `ERROR`.
  5. With no commit for `BLOCK_THREASHOLD` (15000) cycles, set `STUCK`.
- **`checkDrive`** consumes `trace_collect_drive` (from the IBuffer outputs that fire
  without a flush). Each must match the head of `driveInstInput`; a mismatch sets
  `ERROR_DRIVE`.
- **Progress.** Simulation speed is printed every 100000 instructions. Setting
  `TRACERTL_ABORT_ON_SLOW_SIM` turns a persistently slow run into `EMU_CONFLICT`.

### 7.4 Address translation services: `TraceICache`

1. **Soft TLB.** While loading, it records VA page → PA page from every instruction
   fetch and data access. Page-fault records are skipped.
2. **Page table.** It generates an Sv39 page table from that map with
   `DynamicSoftPageTable`, or loads the PTEs and satp from `--tracept-file`.
3. **DPI services.**
   - `trace_tlb_ats(vaddr)`: the PA for `TraceFakeMMU`. Pages outside the map get
     fresh PAs.
   - `trace_dyn_pt_dword_helper(paddr)`: PTE dwords for the PTW refill.
   - `trace_get_satp_ppn()`: the table root.

It also keeps a halfword "icache" of the traced encodings, which is updated on every
read.

### 7.5 Fast warmup

`--fast-warmup N` makes `TraceFastSimManager` and the instruction deduper squash
duplicate instructions in the first N, so the caches and predictors warm up on fewer
cycles. The harness subtracts the squashed count from `-W` and `-I`.

### 7.6 The harness: `xiangshan/main.cpp`

1. Strip the TraceRTL options. Peek at `-W` / `-I` on a copy of argv, then construct
   difftest's `Emulator` with the rest.
2. Set the first instruction address to `0x80000000`. Initialise fast sim,
   `TraceICache` and `TraceReader`.
3. Loop:
   1. `tracertl_prepare_read()`;
   2. `emu->tick()`;
   3. `tracertl_check_commit(cycles)` and `tracertl_check_drive()`;
   4. check the status.
4. Stop:
   - **trace over:** `TraceRTL Dump`, exit 0;
   - **stuck / error / drive error / conflict:** the matching dump, exit 1;
   - otherwise, the Emulator's own ending (`-I`, `-C`, …).

## 8. DPI helpers and the two simulators

**Problem.** GSIM compiles the CHIRRTL directly to C++. It cannot run inline
SystemVerilog DPI-C, and it has stricter port rules.

**Method.** Every helper in `TraceHelper.scala` provides both forms:

- an inline SV module that imports the DPI-C function, for Verilator;
- `createCppExtModule(...)` with a C++ body that calls the same function from
  `tracertl.h`, for GSIM.

The GSIM rules shape the ports:

- **Widths.** Ports are widened to 8 / 16 / 32 / 64 bits. GSIM does not clear the
  unused upper bits of a narrow C++ output, so the full scalar is received and
  narrowed in RTL.
- **No array ports.** `TraceDynPageTableHelper` has eight scalar data ports, which
  `TraceFakeDynPageTable` rebuilds into a vector.

| Helper | DPI-C function | Instantiated in |
| --- | --- | --- |
| `TraceReaderHelper(32)` | `trace_read_one_instr` | `TraceReader` |
| `TraceRedirectHelper` | `trace_redirect` | `TraceReader` |
| `TraceCollectorHelper(CommitWidth)` | `trace_collect_commit` | `Rob` |
| `TraceDriveCollectorHelper(DecodeWidth)` | `trace_collect_drive` | `IBuffer` |
| `TraceATSHelper` (in `TraceFakeMMU`) | `trace_tlb_ats`, `trace_tlb_ats_hit` | `TLB` |
| `TraceSatpPpnHelper` (in `TraceFakeSatpPpn`) | `trace_get_satp_ppn` | `L2TLB`, `CSR` |
| `TraceDynPageTableHelper` (in `TraceFakeDynPageTable`) | `trace_dyn_pt_dword_helper` | `L2TLB` |

`gsim_bridges.cpp` supplies the GSIM bodies of XiangShan's own DPI black boxes
(PerfCCT, ChiselDB), which are always linked.

## 9. The build

`make -C TraceRTL MODE=xiangshan SIM=gsim|verilator emu` runs `xiangshan.mk`:

1. **Elaborate.**
   - Run `make -C $NOOP_HOME sim-chirrtl` (GSIM) or `sim-verilog` (Verilator) with
     `SIM_ARGS="--trace-rtl $(XS_SIM_ARGS)"`.
   - An elaboration stamp removes `build/rtl` whenever it was produced with other
     arguments, such as an exec build or the other simulator.
2. **Stage the harness.** Link difftest's `emu/*` (all but `main.cpp`), TraceRTL's
   `xiangshan/` and `tracertl/` sources into `$NOOP_HOME/build/tracertl-csrc`.
3. **Build with difftest's own makefiles**, with `EMU_CSRC_DIR` pointing to that
   directory.
   - GSIM: the model is regenerated whenever `SimTop.fir` is newer than the last
     complete one, and GSIM runs under `pipefail`. Changed headers drop the
     non-model objects.
4. **Output:** `$NOOP_HOME/build/emu`.

Only `CONFIG`, `NUM_CORES`, `WITH_DRAMSIM3`, `DRAMSIM3_HOME`, `WITH_CHISELDB`,
`WITH_CONSTANTIN`, `EMU_THREADS`, `EMU_OPTIMIZE`, `GSIM_BIN`, `VERILATOR_CXX` and
`XS_SIM_ARGS` are passed down.

`scripts/xiangshan.py --build --tracertl` calls this target. `--tracertl` with
`--ci tracertl-tests` or `--ci tracertl-coremark` runs the traces in
`/nfs/home/share/ci-workloads/tracertl` with `--tracertl-file … --no-diff`.

## 10. Debugging

- **RTL asserts.** `TraceReader` (`InstID` continuity, first PC / ID, buffer
  over-read) and `TraceAlignParallel` (window wider than 32 halfwords, two
  instructions in one slot) report through `XSError`.
- **Runtime trace.** Set `TRACERTL_DEBUG_READER=1` to print every read head, drive
  and redirect with the list sizes.
- **Dumps.** On a failed check, the dump shows:
  - the last committed trace instructions next to the DUT commits;
  - the instruction that failed;
  - the uncommitted instructions;
  - the drive history.

  Start from the `InstID` there, and dump a wave window around it with
  `-b/-e --dump-wave`.
- **Perf counters.** `TraceReaderValid` and `TraceReaderNotValid` measure how often
  the trace starved the IFU.
