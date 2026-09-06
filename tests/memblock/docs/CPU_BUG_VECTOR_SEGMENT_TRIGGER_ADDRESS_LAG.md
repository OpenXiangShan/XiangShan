# CPU Bug: Vector Segment Trigger Address Lag

## Reproducer

The repaired baseline before this fix has complete ordered RTL SHA-256
`4d3f33202176692516f83069c08568f7efa46d466699504851961d4ccd6218e4`.
Extending `trigger-contracts` with a legal two-field ordered-indexed segment
load produced this deterministic failure:

```text
MEMBLOCK_TRIGGER_CONTRACTS_FAIL cycle=292
phase=vector-segment-indexed-load-breakpoint dcache_requests=0->1
reason=mismatched vector memory metadata lane=0 rob=0 op=0xe0
exception=0x8 trigger=0 vstart=1 expected_exception=0x8
expected_trigger=0 expected_writeback_vstart=0
```

The trigger address is field 1 of segment 0. Field 0 completes first and causes
one cold-line refill. The trigger then raises the correct breakpoint action and
exception, but reports `vstart=1` instead of `vstart=0`.

## Architectural Requirement

The ratified RISC-V Vector specification states that segment-load/store
`vstart` values are measured in whole segments. It also permits an
implementation-defined subset of fields in the faulting segment to complete
before a trap. Therefore, a trigger on any field of segment 0 must report
`vstart=0`; completing field 0 before a field-1 trigger does not advance the
architectural segment index.

The RISC-V Debug specification additionally requires instructions with
multiple memory accesses, including vector loads and stores, to be matched as
individual accesses.

- Vector segment rule: <https://docs.riscv.org/reference/isa/unpriv/v-st-ext#_vector_loadstore_segment_instructions>
- Combined-access trigger rule: <https://docs.riscv.org/reference/debug/Sdtrig.html#_combined_accesses>

## Root Cause

`VSegmentUnit` presents `latchVaddr` to `VSegmentTrigger` while in
`s_tlb_req`:

```scala
segmentTrigger.io.fromLoadStore.vaddr :=
  Mux(isMisalignReg, misalignVaddr, latchVaddr)
```

For an ordinary access, `latchVaddr` is updated from the current `vaddr` in
that same state. Chisel register semantics mean the trigger sees the previous
access address. The observed sequence is consequently:

1. field 0 of segment 0 is accessed normally;
2. field 1 of segment 0 is compared against field 0's address and misses;
3. field 0 of segment 1 is compared against field 1 of segment 0's address;
4. the trigger fires one access late and captures `segmentIdx=1`.

The first access after reset is compared against the reset value, and the last
access in an instruction has no following access on which its address can
fire. This affects both breakpoint and DebugMode actions and all vector
segment addressing modes.

## Repair

The DTLB request already uses `tlbReqVaddr`, which selects the current normal
or misaligned-split address. The trigger must compare the same address:

```scala
segmentTrigger.io.fromLoadStore.vaddr := tlbReqVaddr
```

This is a one-line timing repair. It does not change address generation,
translation, PMP checks, or the trigger match/action logic.

## Validation Status

The pre-fix failure and root cause are confirmed. Post-fix validation requires
fresh RTL elaboration and Picker model rebuild. The acceptance checks are:

- the reproducer reports breakpoint and `vstart=0` after exactly one prefix
  refill;
- segment store and DebugMode-action cases match the current access rather
  than the following access;
- the existing `vector-segment`, `vector-segment-fof`, and non-segment trigger
  controls continue to pass;
- the complete ordered RTL hash and frozen runtime hashes are recorded after
  rebuilding.
