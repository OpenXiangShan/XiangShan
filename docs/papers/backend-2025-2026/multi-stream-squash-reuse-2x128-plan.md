# Multi-Stream Squash Reuse 2x128 Implementation Plan

## Objective

Reproduce the paper's multi-stream squash reuse mechanism in XiangShan for a conservative first implementation:

- two historical streams;
- 128 Squash Log instruction entries per stream;
- strict `streamId + instructionOffset` candidate alignment instead of associative PC-only matching;
- 6-bit Rename Mapping Generation IDs with non-rollback per-architectural-register allocation and safe overflow recovery;
- actual reuse of completed, deterministic, single-uop integer ALU results;
- no load, store, branch, CSR, atomic, floating-point, vector, multi-uop, exception, trigger, or other side-effecting reuse;
- local commits only, with no remote push.

The implementation must preserve precise exceptions, redirect recovery, physical-register ownership, normal in-order commit, and difftest behavior.

## Existing Baseline

- Branch: `feat-redirec2-64`
- Baseline commit: `af67b4a39`
- Existing implementation is profiling-only in `Rob.scala`.
- Existing baseline data: `2*64cnt.log`.
- Existing matching scans Squash Log entries by PC and instruction bits and therefore may pair different dynamic loop iterations.
- Existing 32-bit shadow RGID is allocated globally and does not model the paper's per-architectural-register 6-bit allocator, RAT checkpoints, null generation, or overflow reset.
- Existing code does not hold physical registers or eliminate execution.

## Scope And Safety Boundary

Only an instruction satisfying all of the following may reuse:

```text
candidate stream and instruction offset are aligned
&& old entry is valid and belongs to the selected stream generation
&& static instruction identity matches
&& old instruction completed and its result PReg is held
&& current and logged source-used masks match
&& every used integer source RGID matches
&& instruction is a deterministic, single-uop, single-destination integer ALU operation
&& instruction has no exception, flush, single-step, trigger, side effect, or special-state dependency
```

All other instructions follow the unmodified pipeline.

## Phase 1: Formal RGID State

1. Add a 6-bit RGID type and a reserved null/invalid encoding.
2. Extend integer rename mappings with RGID state at the rename boundary.
3. Maintain one non-speculative `NextRGID` allocator per integer architectural register; it must not roll back on redirect.
4. Allocate generations for every new integer mapping, not only reusable instructions.
5. Reused mappings inherit the logged destination RGID and do not allocate a new generation.
6. Carry RGIDs through all RAT snapshots, redirect restore paths, and same-cycle rename bypass paths.
7. Correctly allocate distinct generations for multiple same-cycle definitions of the same architectural register.
8. Define move-elimination behavior explicitly; a move-created architectural mapping must not silently inherit an incompatible generation.
9. On generation exhaustion, assign null RGID and reject reuse. Add overflow counters.
10. Implement a global reset/quarantine protocol that invalidates stale reuse state, releases held PRegs, and prevents pre-reset RGIDs from entering post-reset Squash Logs until a ROB-sized drain condition is satisfied.

Acceptance criteria:

- redirect restores the checkpointed RAT RGID while `NextRGID` remains monotonic;
- no live Squash Log entry can alias a newly allocated generation after wrap/reset;
- all used-source comparisons reject null RGID;
- assertions cover duplicate generation allocation and incorrect snapshot restore.

## Phase 2: Two Streams With Strict Position Alignment

1. Replace the profiling-only associative matcher with two stream records and 128 ordered instruction entries per stream.
2. Record a stable stream generation, instruction offset, PC/instruction identity, source RGIDs, destination RGID, destination PReg, completion status, and reusable class.
3. Preserve ROB age order when capturing squashed instructions.
4. Add or reuse frontend/FTQ metadata to detect static reconvergence and produce `candidateValid`, `streamId`, `streamGeneration`, and `instructionOffset` for each decoded/renamed instruction.
5. Once a stream is selected, advance a cursor in instruction order. Do not search arbitrary entries with the same PC.
6. Reject stale candidates after stream replacement using the stream generation.
7. Handle RVC instruction boundaries, fetch-block boundaries, back-to-back redirects, nested redirects, and stream replacement deterministically.
8. Do not mix entries from different streams inside a continuation chain.

Acceptance criteria:

- repeated loop iterations with identical PC and instruction bits cannot match unless the selected stream and offset align;
- every captured entry is consumed or released at most once;
- a stream replacement atomically invalidates old candidates and initiates release of all unclaimed held PRegs;
- assertions cover cursor bounds, stale stream generations, and cross-stream mixing.

## Phase 3: PReg Ownership And Execution Elimination

1. Add explicit integer PReg ownership sufficient to distinguish free/current allocation/Squash Log hold.
2. Capture only completed eligible results into hold ownership before normal redirect recovery can make them allocatable.
3. Mask held PRegs from freelist allocation.
4. Define atomic hold, claim, release, and stream-eviction transitions with one-owner assertions.
5. On a successful reuse claim:
   - suppress normal integer PReg allocation;
   - map the destination to the held PReg and logged destination RGID;
   - record the current mapping's old PReg for normal commit-time release;
   - create a new current-path ROB entry marked complete;
   - keep the held PReg ready in the integer BusyTable;
   - suppress dispatch, issue, execution, and writeback for the reused operation.
6. Preserve same-cycle producer-consumer forwarding of both PReg and RGID.
7. Define recovery if a claimed current-path instruction is itself squashed by a later redirect; no PReg may leak, become double-owned, or be freed while referenced.
8. Add a conservative admission policy and low-watermark handling so held registers cannot deadlock rename.

Acceptance criteria:

- `FREE`, current-speculative ownership, and Squash Log hold are mutually exclusive;
- no held or claimed PReg is allocated by the ordinary freelist;
- reused instructions still commit in order and release the correct old mapping;
- dependent instructions observe the reused value as ready without waiting for a nonexistent writeback;
- difftest reports no architectural mismatch.

## Phase 4: Instrumentation And Verification

Add counters for:

- streams created, replaced, and truncated;
- entries captured, held, claimed, rejected by class, rejected by RGID, released on mismatch, and evicted unused;
- semantic position-aligned reuse hits;
- active and peak held-PReg occupancy and held-PReg cycles;
- hold admission failures and freelist low-watermark stalls;
- RGID overflow, global reset, and reuse-quarantine cycles;
- reused instructions later squashed;
- saved dispatch, issue, execute, and writeback operations.

Add directed assertions/tests for:

- same PC in different loop iterations;
- one changed source RGID;
- same-cycle producer-consumer chains;
- two writes to the same architectural register in one rename group;
- writeback coincident with redirect;
- stream replacement coincident with claim;
- claim followed by another redirect;
- generation overflow and reset;
- move elimination and x0 sources;
- non-reusable instruction classes.

## Build And Run

1. Run formatting/diff checks and focused Scala compilation during implementation.
2. Build the emulator exactly with:

```bash
make emu -j96
```

3. Run CoreMark exactly with:

```bash
./build/emu -i ready-to-run/coremark-2-iteration.bin \
  --diff ready-to-run/riscv64-nemu-interpreter-so
```

4. Save the complete run output locally without overwriting `2*64cnt.log`.
5. Compare the new 2x128 position-aligned counters with `2*64cnt.log`, while clearly identifying counters whose semantics changed and therefore are not directly comparable.

## Completion Criteria

- `make emu -j96` exits successfully.
- CoreMark reaches `HIT GOOD TRAP` with no difftest mismatch or assertion failure.
- The implementation uses 2x128 entries and 6-bit RGIDs.
- Candidate matching is indexed by selected stream and instruction offset, not arbitrary PC search.
- Actual eligible instruction execution is eliminated, not merely counted.
- PReg and RGID ownership invariants are asserted.
- Relevant performance-counter values and comparison against `2*64cnt.log` are reported.
- All implementation and plan changes are committed locally.
- No commits are pushed.
