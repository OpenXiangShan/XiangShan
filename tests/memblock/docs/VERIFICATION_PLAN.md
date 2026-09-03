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

| Contract | Independent oracle | DUT obligation | Status |
| --- | --- | --- | --- |
| Scalar load | Byte-addressed sparse memory plus ISA width/sign extension | Exactly one completion with the expected ROB/destination, data, and exception bits | Implemented for modeled scalar loads |
| Scalar store | ISA store width/mask applied to sparse memory after legal commit | Both address/data acknowledgments occur; committed bytes are recovered by an independent load | Partial: commit/readback model needs an immutable pre/post snapshot |
| Vector load | Independent unit/strided/indexed address decoder, `vl`/`vstart`/mask rules, and old destination | Exact 128-bit result and active-element mask for EEW 8/16/32/64 | Implemented for modeled 128-bit operations |
| Vector store | The same independent address/mask decoder applied to source bytes | Eventual completion/commit and exact scalar or vector readback of every active byte | Partial: independent post-commit memory checking is being strengthened |
| Address translation | Software Sv39 and Sv39x4 page-table construction and reference walk | Accesses reach the independently calculated PA; invalid walks report the specified page/guest-page fault | Partial: permission/A-D/SUM/MXR matrix is not complete |
| Guest-fault metadata | Reference VS/G-stage walk from PTE addresses | Exact fault VA, faulting PTE GPA, and VS-non-leaf-PTE marker | Partial: current cases are covered, broader fault classes are planned |
| Misalignment | Byte concatenation/splitting across 16-byte, line, and page boundaries | Exact value/bytes when enabled; specified address-misaligned exception when disallowed by memory type/control | Partial: common scalar/vector splits are covered |
| Exception side effects | RISC-V exception contract | Exact exception bit; exceptional scalar load has no integer/FP RF write; software prefetch never raises a load exception or writes an RF | Partial: concurrent priority and full cause matrix are planned |
| Redirect | ROB age and redirect level supplied by a legal backend transaction | Redirected younger work has no terminal writeback; surviving work completes with the same data | Partial: basic redirect is covered; cancellation observation is driver-accounted |
| Cache coherence boundary | TileLink opcode/source/size/mask/data reference agent | Stable producer payload while stalled, complete refill, ReleaseAck, and byte-exact dirty ReleaseData | Partial: response legality and independent full-line release snapshot are planned |
| PTW/uncache boundary | TileLink and uncache ready-valid agents with deterministic memory | Stable request/response while stalled, legal source/opcode, ordered NC store data, exact load response; each mixed seed exercises uncache request/response stalls | Partial: malformed/denied/corrupt response checks are planned |
| Progress | Manager fairness: every observed request is eventually made ready and answered | Every non-canceled modeled operation terminates before the generous scenario deadline | Partial: enqueue/cancel acceptance is currently driver-accounted |
| Resource conservation | Accepted LSQ allocation, architectural commit, and redirect events | Allocated entries equal dequeued plus explicitly canceled entries at scenario end | Partial: allocation and cancellation events are not all independently observable at this boundary |

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

Every `random-mixed` seed contains mandatory phases before constrained-random traffic. The tail is made of rolling five-class windows, not isolated tests. The
requested transaction count is the total action budget, including the mandatory
prefix; only the tail is constrained-random:

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

## Complete Verification-Point Inventory

The following inventory is the closure target for the MemBlock boundary. Each
row is classified as `implemented`, `partial`, or `planned`; a planned row is a
known gap and must not be reported as green merely because the surrounding
cacheable tests pass.

### Instruction and data-shape points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| Integer loads | `lb/lbu/lh/lhu/lw/lwu/ld`, all destination classes, all issue lanes, zero/sign-extension patterns | Implemented |
| Integer stores | `sb/sh/sw/sd`, address-first/data-first, byte masks, all issue lanes, commit timing | Implemented |
| Floating loads/stores | FLW/FLD and narrow/widening formats, NaN-boxing, FP exception bits, integer/FP destination separation | Planned at MemBlock boundary |
| Vector unit stride | EEW 8/16/32/64, `vl=0..VLEN`, `vstart` at start/middle/end, `vm`, mask holes, `vma/vta` | Implemented for modeled 128-bit operations |
| Vector strided | Positive, zero, and negative legal strides; element overlap and gaps; line/page crossings | Partial; negative-stride and overlap crosses planned |
| Vector indexed unordered | Repeated indices, aliasing, non-monotonic indices, all EEWs, masked elements | Partial; basic unordered mode implemented |
| Vector indexed ordered | Strict element order, repeated/aliasing indices, split beats/pages | Partial; basic ordered mode implemented |
| Vector segmented/whole-register | NF/segment count, multi-uop streams, fault-only-first and partial completion | Planned |
| Vector data patterns | all zero/one, ramps, alternating bits, random bytes, same-byte aliases, old-destination merge | Implemented/partial by operation class |
| Software prefetch | `prefetch.i/r/w`, mapped/unmapped, cacheable/NC, all lanes, duplicate and outstanding requests | Implemented for modeled software prefetch |
| Atomics | LR/SC, AMOADD/XOR/AND/OR/MIN/MAX and signed/unsigned variants, reservation loss, alignment | Planned |
| CBO/CMO/fences | clean/invalidate/flush/zero, `fence`, `fence.i`, `sfence.vma`, ordering with outstanding traffic | Planned |
| Hypervisor memory ops | HLV/HLVX/HSV, effective privilege/SPVP, execute permission, guest/host faults | Planned |

### Address, translation, and protection points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| Alignment | every byte offset for widths 1/2/4/8/16, beat boundary, 64-byte line, 4 KiB page, two-page and two-line splits | Partial; common scalar/vector split classes implemented |
| Virtual address classes | low/high canonical addresses, sign extension, page offset boundaries, aliasing VAs, VA wraparound | Partial |
| Physical address classes | cacheable, uncached, device, reserved, high PA bits, line/set/way aliases | Partial; cacheable and PBMT-NC modeled |
| Sv39 walk | Bare/Sv39 mode, L2/L1/L0 leafs, superpages, invalid/non-leaf, permission and A/D combinations | Partial |
| Sv39x4 walk | VS-stage hit/fault, G-stage hit/fault, non-leaf GPA, nested page offsets, warm/cold reuse | Implemented for current core cases; matrix expansion planned |
| TLB behavior | cold miss, hit, refill, duplicate miss, replay, invalidation, `sfence.vma`, concurrent page walks | Partial |
| Page permissions | R/W/X/U, SUM/MXR, read-only store, execute-only, access/dirty bit updates, privilege transitions | Planned/partial |
| PMP/PMA | TOR/NA4/NAPOT, overlap priority, lock, M/R/W/X, cacheability, atomic/MMIO permissions, exact region edges | Planned |
| Fault classes | load/store/instruction access fault, page fault, guest-page fault, address-misaligned, access-denied, bus/ECC error | Partial; load/store/page/misaligned core cases implemented |
| Fault metadata | exact VA, GPA/PTE address, level, guest marker, cause priority, single reporting and replay suppression | Partial; VS-non-leaf path implemented |

### Cache, memory-system, and coherence points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| DCache lookup | warm hit, cold miss, same-line merge, bank conflict, set pressure beyond associativity, synonym/alias | Partial; cold/warm and dirty set pressure implemented |
| Refill/replay | delayed A/D responses, beat reordering where legal, partial refill, killed request, replay after miss | Partial |
| Eviction | clean release, dirty ReleaseData, partial byte masks, replacement under pressure, release backpressure | Partial; whole-line pre-eviction snapshot is planned |
| TileLink coherence | Probe/B/C/E traffic, source reuse, denied/corrupt/error responses, manager ordering | Planned/partial; probe/error injection absent |
| Uncache/MMIO | Get/Put widths, byte enables, side effects, ordering, response delay, denied/error response | Partial; generic uncache/PBMT-NC only |
| ECC/cache errors | correctable/uncorrectable data, error lifetime, retry or architectural exception | Planned |
| PTW manager | request/response backpressure, source reuse, malformed/denied response, concurrent walks | Partial; legal backpressure implemented |

### Queue, ordering, and control points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| LQ/SQ occupancy | empty/near-full/full, wrap flags, simultaneous enqueue/dequeue, same-slot reuse | Implemented/partial |
| ROB age | ordinary and wrapped pointers, flag transitions, same-cycle issue/commit/redirect | Partial |
| Store-to-load forwarding | scalar-scalar, vector-vector, scalar-vector, vector-scalar, partial overlap, byte masks, older/younger stores | Implemented for modeled classes |
| Exception priority | multiple legal faults, differing ROB vs LQ/SQ age, load/store/vector competition | Planned |
| Redirect/recovery | kill each producer class, in-flight miss/replay, canceled prefetch, pointer reuse, survivor data | Implemented for basic redirect; VLS/segment cases planned |
| Fence/commit ordering | outstanding cache/uncache/PTW traffic across commit and fence boundaries | Planned |
| Backpressure | every producer/consumer ready-low pattern, long stalls, alternating stalls, response delay cross-product | Implemented for DCache/PTW/uncache; probes/errors planned |
| Reset/quiescence | reset asserted/deasserted at legal boundaries, idle cycles, reset with outstanding traffic, repeated reset | Partial; initial reset/quiescence implemented |

### Protocol and robustness points

| Point family | Required checks | Current status |
| --- | --- | --- |
| Ready/valid stability | Every DUT producer holds payload and sideband stable while stalled | Implemented with generated SVA |
| Exactly-once identity | No lost, duplicated, or mismatched response; source/ROB/uop/queue identity preserved | Partial; some output-only pulses and response fields are not independently observable |
| Manager legality | Opcode/size/mask/address/data combinations are legal for each channel | Partial; current modeled channels covered |
| Fairness/progress | Every accepted non-canceled request eventually terminates under bounded fair delays | Partial; fair delays are supplied, but all accepted events are not independently observed |
| Four-state robustness | Unknown/reset values, uninitialized arrays, X-sensitive assertions | Planned; requires four-state simulator/formal check |
| Malformed external responses | denied/corrupt/early/late/duplicate response handling | Planned |
| Long-run reproducibility | Frozen binary/model/libraries, deterministic seed replay, artifact and controller hashes | Implemented |

## Constrained-Random Distribution

The intended closure design is a legal-value generator followed by weighted
constraints, not a fixed list of replay cases. The current implementation uses
one seeded `std::mt19937_64` stream and deterministic mandatory phases followed
by a randomized tail. It does not yet provide independently hashed substreams,
an independent starvation monitor, or general cross-coverage collection; the
reported mandatory counters are partly generator bookkeeping. These stronger
mechanisms are planned and must be implemented before claiming distribution
closure.

The planned generator must include both common traffic and rare boundary bins,
with a starvation monitor that fails a seed if a required bin is never
observed. Planned cross coverage includes:

- operation class x issue lane x alignment class;
- vector EEW x address mode x mask/vstart/vl class;
- cache residency x translation state x memory type;
- producer overlap x issue order x backpressure class;
- fault cause x privilege/translation stage x queue age;
- store width x byte overlap x forwarding direction;
- redirect point x outstanding request type x pointer-wrap state.

Weights may be changed to accelerate rare bins, but the oracle and legal-value
constraints must remain unchanged. A generated value that cannot be explained
by the reference model is a generator failure, not an RTL failure. Until the
planned independent monitors exist, current coverage fields are evidence of
stimulus intent and observed writebacks where available, not proof of all
listed crosses.

## Coverage Closure and Exit Criteria

Coverage is reported at three levels:

1. **Structural**: all manifest inputs/outputs are inventoried and exercised by
   legal or explicitly labeled invalid patterns.
2. **Functional**: every implemented row above has nonzero observations for all
   required values and its specified crosses meet the minimum count. Rows marked
   partial are reported separately and cannot be promoted by a green result
   artifact alone.
3. **Oracle/protocol**: every observed modeled operation has a reference result,
   terminal disposition, and protocol-stability check. Partial rows and
   driver-accounted events remain explicit limitations.

The campaign is not considered complete when a scenario merely returns zero.
It is complete only when the artifact records the requested seed/action counts,
all required functional bins, exact queue conservation, full runtime/RTL
provenance, and no unclassified failure. Planned rows remain visible in the
artifact and in reports until their independent model and observation path are
implemented.

## Interface Assumptions

| Interface family | Environment assumption | Enforced rule |
| --- | --- | --- |
| Issue | Legal operation encoding and LSQ/ROB pointer; payload held until `ready` | Typed drivers retain `valid` and payload through acceptance |
| LSQ dispatch/commit | Allocations and commits are in legal backend order | Drivers allocate the correct scalar/vector count and never commit unallocated work |
| Redirect | ROB pointer/flag and level describe a legal backend redirect | The scoreboard removes only architecturally younger work |
| DCache manager | Coherent 64-byte lines on a 256-bit bus; finite randomized delay | Agent drives/observes modeled A/C traffic and timing; independent E/response legality checks are planned |
| PTW manager | PTE memory and response source/size match the programmed roots | Reference page tables and PTW agent share sparse memory; malformed response validation is planned |
| Uncache manager | Only modeled Get/Put requests receive AccessAck/Data | Ordered byte-level update is modeled; independent response identity/denied/corrupt checks are planned |

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

The current harness does not yet own a complete legal producer, independent
reference model, or externally observable contract for the following. These
are planned work items, not silently accepted coverage:

- floating-point load/store formatting, NaN-boxed MMIO loads, and FP exception
  side effects;
- MMIO/device reads and writes with side effects, ordering, denied responses,
  and ROB marking;
- LR/SC reservations, all AMO operations, atomic ordering, and cache-error
  injection;
- CBO/CMO line operations, `fence`, `fence.i`, and `sfence.vma` ordering;
- VSegment/VFOF takeover, multi-uop segment streams, fault-only-first, and
  segment-specific redirect behavior;
- HLV, HLVX, HSV, SPVP, final physical execute permission, and hypervisor PMP;
- complete PMP/PMA TOR/NA4/NAPOT, lock, priority, cacheability, and region-edge
  matrices;
- Sv39 superpages, A/D updates, SUM/MXR combinations, invalidation races, and
  concurrent page walks;
- manager-originated TileLink probes, probe acknowledgements, denied/corrupt
  responses, ECC errors, and coherence error recovery;
- simultaneous malformed/duplicate/early/late PTW and uncache responses;
- concurrent wrapped-age load/store/vector exception priority;
- multiple simultaneous split misaligned loads under LQ-RAR pressure;
- multi-uop vector streams and negative/overlapping strided operations;
- four-state/X behavior and reset-sensitive uninitialized storage;
- full reset with outstanding requests and repeated reset/recovery cycles;
- performance-counter/top-down attribution and hardware-prefetch training
  metadata, which have no stable architectural MemBlock oracle.

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
   streaming verifier, controller sources, and RTL metadata are frozen and
   hashed. The verifier script is passed as a controller input so its acceptance
   logic is part of the recorded provenance.

The final campaign runs at least 21,600 monotonic seconds (six hours) with eight
workers and the `random-mixed` scenario. Each seed requests at least 4096 total
actions, including the mandatory coverage prefix and a constrained-random tail
with five-class overlap windows. Every window
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

An independent streaming verifier (whose own SHA-256 is recorded in the
controller inputs) checks the result artifact, duration,
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
