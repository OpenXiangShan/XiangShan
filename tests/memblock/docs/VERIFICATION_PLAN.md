# MemBlock Verification Plan

## Objective

Verify the generated Kunminghu-v2 `MemBlock` against contracts that remain
valid across internal pipeline, queue, and state-machine refactors. Functional
acceptance is based on RISC-V-visible data/exception behavior and external
ready-valid protocols. Internal implementation signals may be used to localize
a historical mutation, but they are not substitutes for an architectural
oracle.

The current repaired RTL under test is tied to the complete ordered RTL
file-list SHA-256, not only `MemBlock.sv`. Historical clean-baseline and
mutant hashes are recorded separately. A failure is reported as an RTL
candidate only after a
deterministic replay excludes driver, manager, reference-model, and scoreboard
errors.

## Stable Oracles

| Contract | Independent oracle | DUT obligation |
| --- | --- | --- |
| Scalar load | Byte-addressed sparse memory plus ISA width/sign extension | Exactly one completion with the expected ROB/destination, data, and exception bits |
| Scalar store | ISA store width/mask applied to sparse memory after legal commit | Both address/data acknowledgments occur; committed bytes are recovered by an independent load |
| Vector load | Independent unit/strided/indexed address decoder, `vl`/`vstart`/mask rules, and old destination | Exact 128-bit result and active-element mask for EEW 8/16/32/64 |
| Vector store | The same independent address/mask decoder applied to source bytes | Eventual completion/commit and exact scalar or vector readback of every active byte |
| Address translation | Software Sv39 and Sv39x4 page-table construction and reference walk | Accesses reach the independently calculated PA; invalid walks report the specified page/guest-page fault |
| Guest-fault metadata | Reference VS/G-stage walk from PTE addresses | Exact fault VA, faulting PTE GPA, and VS-non-leaf-PTE marker |
| Misalignment | Byte concatenation/splitting across 16-byte, line, and page boundaries | Exact value/bytes when enabled; specified address-misaligned exception when disallowed by memory type/control |
| Exception side effects | RISC-V exception contract | Exact exception bit; exceptional scalar load has no integer/FP RF write; software prefetch never raises a load exception or writes an RF |
| Redirect | ROB age and redirect level supplied by a legal backend transaction | Redirected younger work has no terminal writeback; surviving work completes with the same data |
| Cache coherence boundary | TileLink opcode/source/size/mask/data reference agent | Stable producer payload while stalled, complete refill, ReleaseAck, and byte-exact dirty ReleaseData |
| PTW/uncache boundary | TileLink and uncache ready-valid agents with deterministic memory | Stable request/response while stalled, legal source/opcode, ordered NC store data, exact load response; each mixed seed exercises uncache request/response stalls |
| Progress | Manager fairness: every accepted request is eventually made ready and answered | Every accepted nonredirected operation terminates before the generous scenario deadline |
| Resource conservation | Accepted LSQ allocation, architectural commit, and redirect events | Allocated entries equal dequeued plus explicitly canceled entries at scenario end |

The page-table builder, scalar formatter, vector element-address calculator,
mask merge, store-forward overlay, and sparse memory are C++ reference code;
they do not sample internal RTL decisions. Cache hit/miss classification is
derived from external TileLink traffic and PTW refill counts. A warm access
requiring no new PTW request is useful coverage, but translation correctness is
still decided by the returned data or exact exception, not by the hit itself.

## Stimulus Matrix

| Area | Required stimulus | Acceptance checks |
| --- | --- | --- |
| Reset | External reset, internal reset drain, idle interval | No terminal completion while idle |
| Scalar loads | `lb/lh/lw/ld/lbu/lhu/lwu`; all three lanes; aligned and misaligned | ISA extension, exact data, metadata, replay, exception, LQ drain |
| Scalar stores | `sb/sh/sw/sd`; both address/data lanes and both issue orders | Both completions, exact byte mask/readback, SQ drain |
| Vector loads | EEW 8/16/32/64; both lanes; unit, strided, indexed unordered/ordered; mask, `vstart`, partial `vl`; split windows | Exact 128-bit result, active mask, metadata, replay, LQ drain; each address mode counted independently |
| Vector stores | All EEWs and address modes; mask, `vstart`, partial `vl`; misaligned and cross-page split/replay | Exact active-byte readback, completion, commit, SQ drain; each address mode counted independently |
| Software prefetch | `prefetch.i/r/w`, all scalar issue lanes, mapped and unmapped VAs | Completion without RF write or exception; LQ drain |
| Sv39 | Cold mapped access, warm reuse, invalid PTE, PBMT-NC | PA-derived data, exact page fault, PTW activity/reuse, ordered uncache traffic |
| Sv39x4 | Cold/warm two-stage access and G-stage fault during VS non-leaf walk | PA-derived data; exact guest-page-fault bit, VA, GPA, and marker |
| DCache | Cold miss, warm hit, same-set pressure beyond eight ways, dirty eviction | Refill correctness, no extra miss for mandatory warm control, dirty ReleaseData preservation |
| Forwarding | Scalar-to-scalar, vector-to-vector, scalar-to-vector, vector-to-scalar; masks and widths | Byte-accurate overlay before store commit |
| Mixed pressure | Constrained-random windows enqueue scalar load, scalar store, vector load, vector store, and prefetch together; issue order, store address/data order, vector mode, and manager delays vary before a bounded drain | Every window records at least two unresolved classes (normally all five); all scoreboards drain; per-class coverage gates and exact LQ/SQ accounting |
| Redirect | Younger cold miss redirected while traffic is outstanding | No stale writeback and legal pointer reuse |
| Queue pressure/wrap | Two 60-entry LQ waves; more than 72 LQ and 160 ROB positions over long runs | Every accepted item retires or is explicitly canceled; flag/value identity remains continuous |
| Backpressure | Independent deterministic gaps on DCache A/D, PTW A/D, and uncache request/response | Ready-valid stability plus eventual progress |

Every `random-mixed` seed contains mandatory phases before constrained-random traffic. The tail is made of rolling five-class windows, not isolated tests:

- all scalar load and store widths and all scalar issue lanes;
- all vector EEWs and unit/strided/indexed-unordered/indexed-ordered modes;
- masked/unmasked, zero/nonzero `vstart`, full/partial `vl`, aligned/split data;
- scalar and vector misaligned stores with replay and exact readback;
- Sv39 cold/warm translation, Sv39x4 cold/warm translation, and an exact vector
  VS-non-leaf guest-page fault;
- mapped/unmapped software prefetch, PBMT-NC store/load, DCache dirty eviction,
  redirect recovery, simultaneous heterogeneous issue, and both cross-type
  forwarding directions.

The seed fails if any required class has a zero count, fewer than four mixed
windows, no sample with two unresolved classes, or if final queue conservation
fails. Each window first enqueues all five producer classes, then varies issue
order, scalar store address/data order, vector address mode, mask, alignment,
cache residency, translation state, and manager delay while scoreboards remain
outstanding. A bounded drain occurs only after the window, preserving real
heterogeneous overlap without allowing unbounded pointer reuse.

## Interface Assumptions

| Interface family | Environment assumption | Enforced rule |
| --- | --- | --- |
| Issue | Legal operation encoding and LSQ/ROB pointer; payload held until `ready` | Typed drivers retain `valid` and payload through acceptance |
| LSQ dispatch/commit | Allocations and commits are in legal backend order | Drivers allocate the correct scalar/vector count and never commit unallocated work |
| Redirect | ROB pointer/flag and level describe a legal backend redirect | The scoreboard removes only architecturally younger work |
| DCache manager | Coherent 64-byte lines on a 256-bit bus; finite randomized delay | TileLink agent checks A/C/E requests and supplies legal D responses |
| PTW manager | PTE memory and response source/size match the programmed roots | Reference page tables and PTW agent share only sparse memory, not DUT state |
| Uncache manager | Only modeled Get/Put requests receive AccessAck/Data | Ordered byte-level memory update and exact response identity |

These are simulation driver guarantees, not assumptions about an internal FSM.
Generated SVA checks ready-valid payload stability on DUT-produced channels.
Every backpressure-enabled mixed seed records six observed manager effects:
DCache request stall and response delay, PTW request stall and response delay,
and uncache request stall and response delay. The verifier requires all six to
be nonzero, so a manager cannot be accidentally left at always-ready while the
campaign is reported as backpressure-tested.
The StoreQueue TLB-miss property is retained as targeted historical mutation
evidence; it is secondary to architectural store/readback and progress checks.

## Complete Pin Audit

`config/expected_ports.json` is the checked inventory of every top-level pin.
With primary reset asserted, `pin-space` drives every other input bit to both
values and applies 254 additional deterministic per-pin mixed patterns. It
reads back every driven input and samples every output into a digest. Normal
reset and clock operation cover the excluded clock/reset inputs.

This proves manifest completeness, wrapper connectivity, and broad raw value
space. It does not claim semantic coverage for protocol-invalid combinations.
Semantic coverage comes only from the legal typed agents and the oracles above.

## Known Boundary Gaps

The current harness does not yet own a complete legal model for:

- VSegment issue and takeover;
- HLV, HLVX, HSV and SPVP-specific PMP/execute permission;
- MMIO, including FP NaN-boxed MMIO loads;
- atomics plus cache-error injection;
- CBO/CMO request-response behavior;
- manager-originated TileLink probes;
- concurrent wrapped-age load/store exception priority;
- multi-uop vector segment streams beyond one 128-bit architectural operation.

These gaps are reported per historical commit in
`HISTORICAL_BUG_AUDIT.md`; a reset-held pin toggle is not counted as closing
them. The historical `vector-guest-fault-split` mismatch is now fixed and is
included in the repaired sentinel and boundary-hunt gates.

## Regression Acceptance

Before a duration run:

1. `check-ports`, `check-rtl`, and all Python unit tests pass.
2. Every green focused scenario passes on the current complete RTL hash.
3. A multi-seed, five-scenario matrix passes with backpressure enabled.
4. The executable, Verilated model, xspcomm, resolved system libraries, runner,
   and RTL metadata are frozen and hashed.

The final campaign runs at least 21,600 monotonic seconds (six hours) with eight
workers and the `random-mixed` scenario. Each seed requests at least 4096 constrained-random
actions, including hundreds of five-class overlap windows. Every window
randomizes producer parameters, legal alignment class, data, masks, vector
shape, issue order, store half order, and manager delay; mandatory sanity waves
are kept only where the interface has a proven legal encoding. The artifact
records the requested command count and completed summary count, and the
independent verifier checks both. Work already submitted at the deadline is
allowed to finish. Any nonzero return, timeout, assertion, scoreboard error,
coverage-gate failure, provenance change, or discontinuous seed range fails
acceptance.

The separate `random-boundary-hunt` campaign is a diagnostic and repair gate:
it creates a fresh Sv39x4 environment for each sample, randomizes the faulting
VA offset, EEW, `vl/vstart`, mask, lane, data, and backpressure, then compares
the exact VS-non-leaf GPA and first-active-element VA against the software
oracle. On the historical clean RTL it must produce at least one mismatch;
after the fix it must produce an all-pass campaign with the same oracle.

An independent streaming verifier checks the result artifact, duration,
scenario set, transaction counts, per-seed coverage, aggregate counts,
continuous seeds, complete RTL identity, and before/after frozen-runtime hashes.

## Failure Triage

1. Replay the exact scenario, seed, transaction count, and frozen hashes.
2. Reduce the transaction prefix while preserving memory and backpressure RNG
   streams.
3. Check the external request/response transcript against the reference model.
4. Add a focused architecture/protocol reproducer before inspecting internal
   state.
5. If historical sensitivity matters, regenerate a separate revert RTL tree
   and require clean pass plus mutant fail under identical test sources.
