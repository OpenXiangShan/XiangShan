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

## Translation Specification Scope

The Kunminghu-v2 parameter set used by this MemBlock build is RV64 with the H
extension and `EnableSv48=true` (`src/main/scala/xiangshan/Parameters.scala`). The translation
closure target is therefore the following implementation-defined subset of the
ratified RISC-V privileged and hypervisor specifications:

| CSR/stage | Supported modes in this build | Address shape | Required page-table depth |
| --- | --- | --- | --- |
| `satp`, HS/S/U stage | Bare, Sv39 (MODE=8), Sv48 (MODE=9) | 39-bit or 48-bit canonical VA; 12-bit page offset | 3 or 4 levels |
| `vsatp`, VS/VU stage | Bare, Sv39 (MODE=8), Sv48 (MODE=9) | Guest virtual address with the same canonicality rules | 3 or 4 levels |
| `hgatp`, G stage | Bare, Sv39x4 (MODE=8), Sv48x4 (MODE=9) | 41-bit or 50-bit zero-extended GPA; 12-bit page offset | 3 or 4 levels; 16-KiB root |

Sv57/Sv57x4 are not claimed for this configuration: the source parameters
expose Sv48 and the generated RTL does not implement a fifth walk level. The
four non-Bare nested combinations `vsatp={Sv39,Sv48}` x
`hgatp={Sv39x4,Sv48x4}` are separate coverage points, not aliases for one
generic "two-stage" point. The plan also includes the effective Bare/one-stage
degenerations (`vsatp=Bare`, `hgatp=Bare`) and stage-2-only translation used by
hypervisor accesses where that boundary is observable.

The normative rules are: Sv39/Sv48 canonical VA checking and leaf alignment;
Sv48's fourth level and 512-GiB leaf; x4's widened root index, 16-KiB root
alignment, and 41/50-bit GPA high-bit checks; VS-stage followed by G-stage
translation when `V=1`; G-stage permissions treating page-table accesses as
U-mode accesses; guest-page-fault rather than page-fault reporting for G-stage
failures; and `SFENCE.VMA`, `HFENCE.VVMA`, and `HFENCE.GVMA` ordering and VMID/
ASID scoping. These rules are from the [RISC-V Privileged Architecture,
Sv39/Sv48 sections](https://docs.riscv.org/reference/isa/v20240411/_attachments/riscv-privileged.pdf)
and [Hypervisor Extension two-stage translation](https://docs.riscv.org/reference/isa/priv/hypervisor.html).

This MemBlock boundary can currently drive data accesses and observe selected
fault metadata, but it cannot by itself prove instruction-fetch trap entry,
`mtval2/htval`, `htinst`, or execution of fence instructions. Those remain
explicit integration tests unless the corresponding architectural observation
is added to the harness. It also receives an already-decoded `TlbCsrBundle`,
not architectural CSR write/readback traffic. The harness can therefore verify
translation under legal Bare/Sv39/Sv48 and Bare/Sv39x4/Sv48x4 input modes, but
CSR WARL filtering and software-visible exposure of those modes belong to a
CSR or full-core integration test.

## Stable Oracles

| Contract | Independent oracle | DUT obligation | Status |
| --- | --- | --- | --- |
| Scalar load | Byte-addressed sparse memory plus ISA width/sign extension | Exactly one completion with the expected ROB/destination, data, and exception bits | Implemented for modeled scalar loads |
| Scalar store | ISA store width/mask applied to an architectural reference separate from bus backing memory | Address/data writebacks compare exception/ROB/flush/debug metadata; committed bytes are recovered by an independent load. PBMT=IO stores must use Uncache, avoid DCache, complete the response/writeback sequence, and retire the SQ entry | Implemented for modeled scalar stores and scalar PBMT=IO contract |
| Vector load | Independent unit/strided/indexed address decoder, `vl`/`vstart`/mask rules, old destination, and legal `vma/vta` agnostic values | Exact active data and active-element mask for EEW 8/16/32/64; inactive data is constrained by RVV policy | Implemented for modeled 128-bit operations |
| Vector store | The same independent address/mask decoder applied to source bytes | Eventual completion/commit, exact vector readback of every active byte, RF write-enable/flush metadata, and optional trigger/debug metadata | Implemented for modeled 128-bit stores |
| Address translation | Mode-parameterized software walk for Bare/Sv39/Sv48 and Sv39x4/Sv48x4; independent canonicality, PTE validity, leaf level, alignment, permission, PBMT/N/reserved, and A/D checks | Accesses reach the independently calculated PA; invalid walks report the access-specific page/access/guest-page fault without a data-manager request | Partial: generic walker, Bare degenerations, all four 4-KiB nested paths, superpage leaves, Sv39/Sv48 high-half canonical and both noncanonical sign-extension directions, permission cases, the complete valid two-stage PBMT matrix, and 52 stage-1/G-stage invalid/reserved/PBMT/NAPOT encodings are implemented for both scalar loads and stores; remaining fault crosses remain |
| Nested translation | Independent VS-stage walk followed by independent G-stage walk for all four `vsatp` x `hgatp` mode pairs, including implicit page-table accesses and VS-over-G PBMT priority | Exact host PA or stage-specific fault; no stage may be skipped or silently treated as Bare; final PMA/NC/IO class follows architectural composition | Partial: all four 4-KiB pairs, VS/G/Bare degenerations, all 36 valid PBMT combinations, VS/G context switches, host/nested `V` transitions, same-ID root reuse with targeted fences, global/selective stale-response races, co-issued distinct-page walks, and redirected root/ASID/VMID/MODE/`V` changes with a delayed walk are covered for isolated stage-1 plus all four fully nested mode pairs; the common random tail weights and gates every pair with cold/reuse observations |
| L2-to-L1 DTLB boundary | Drive all retained `io_l2_tlb_req_req_*` fields, including ordinary and prefetch requests, kill/no-translate controls, and response timing | Legal response valid/miss/PBMT/fault fields and exported PMP/MMIO classification; cold misses are delegated to the external L2 TLB | Implemented for ordinary and prefetch miss responses in `l2-tlb-contracts`; the MemBlock boundary has no L2 refill response input, so hit refill and external retry remain integration-level tests |
| IFU-to-Mem PTW bridge | Typed `PtwReq` driver over `io_fetch_to_mem_itlb_*` with an independent page-table image | Exact Sv39/Sv48 and nested sector response metadata; request acceptance and response payload stability under backpressure | Implemented: `ifetch-ptw-bridge` covers valid Sv39/Sv48, all four Sv39/Sv48 x Sv39x4/Sv48x4 4-KiB pairs, Sv39/Sv48 VS-only and G-only degenerations, PBMT=NC/IO leaves, invalid L0 leaves, a delayed IFU walk overlapped with a cold scalar DTLB walk, and two same-VPN IFU requests coalesced into one three-request Sv39 walk with two exact responses. Every nested pair is crossed with VS-leaf, final-G-leaf, and implicit VS-page-table G-stage faults. Delayed stage-1 and nested walks are invalidated by root/mode/ASID/VMID changes plus global and selective `SFENCE.VMA`, `HFENCE.VVMA`, and `HFENCE.GVMA`; each requires stale-response suppression and the exact replacement mapping. The active-stage ASID/VMID, permission, PPN, index, PBMT, fault level, fault bits, both leaf PTE addresses, and cross-source PTW outstanding depth are checked |
| L2 hint propagation | Valid/invalid `io_l2_hint`, all `sourceId` values, and `isKeyword` polarity at an idle/no-matching-MSHR boundary | Hint is registered and distributed without producing a ghost writeback, queue corruption, or protocol error; matching-MSHR replay semantics are integration-tested with L2 | Implemented for both keyword polarities and all 16 source IDs with an idle no-MSHR safety oracle in `l2-tlb-contracts`; matching-MSHR replay remains an L2 integration scenario |
| Frontend bridge | Independent request/response sequences for ICache, ICache-control, and instruction-Uncache, including the fields synthesized or narrowed by their diplomacy edges | Every accepted A request emerges once and in order with exact opcode/size/source/address/mask/data/user fields; every accepted D beat returns once with exact observable fields; a source is not reused before its response completes; payload remains stable under request and response stalls | Implemented in `frontend-bridge`: all 88 top-level frontend TileLink fields are explicitly driven or checked, the three paths run concurrently, source credits are enforced and observed, ICache uses two response beats, and the ICache-control path exercises Get/PutFull/PutPartial, sizes 1-8 bytes, all 32 sources, masks, and D-channel backpressure |
| Guest-fault metadata | Reference VS/G-stage walk from PTE addresses, including explicit data faults and implicit VS-page-table faults | Exact fault VA, faulting PTE GPA, shifted `htval`-class value where observable, and VS-non-leaf-PTE marker | Partial: current VA/GPA/marker cases are covered; broader fault classes are planned |
| Misalignment | Byte concatenation/splitting across 16-byte, line, and page boundaries | Exact value/bytes when enabled; specified address-misaligned exception when disallowed by memory type/control | Partial: common scalar/vector splits are covered |
| Exception side effects | RISC-V exception contract | Exact exception bit; exceptional scalar load has no integer/FP RF write; software prefetch never raises a load exception or writes an RF | Partial: concurrent priority and full cause matrix are planned |
| Redirect | ROB age and redirect level supplied by a legal backend transaction | Redirected younger work has no terminal writeback; surviving work completes with the same data | Partial: basic redirect is covered; cancellation observation is driver-accounted |
| Cache coherence boundary | TileLink opcode/source/size/mask/data reference agent with separate bus and architectural memories | Stable producer payload while stalled, complete refill, sink-exact GrantAck, clean/dirty ProbeAck(Data), ReleaseAck, byte-exact dirty data, atomic refill/update, and denied/corrupt D-channel handling | Partial: `dcache-coherence` checks clean ProbeAck, requested clean ProbeAckData, mandatory dirty ProbeAckData, invalidation/refill, and E-channel GrantAck with forced backpressure. The common random tail generates byte-exact dirty toB/toN Probes and crosses requested/mandatory data; toB retention is followed by a checked toN cleanup. DCache denied/corrupt load errors and all 22 refill-capable W/D LR/AMO/AMOCAS error paths are also executable. Probe overlap with unrelated traffic, malformed responses, multiple Probe sources, and broader source reuse remain planned |
| PTW/uncache boundary | TileLink and uncache ready-valid agents with deterministic memory | Stable request/response while stalled, legal source/opcode/size/address/mask, ordered NC/MMIO store data, exact beat/lane load response, denied/corrupt error propagation, and SQ retirement | Partial: legal backpressure, response identity, scalar Uncache width/lane and denied/corrupt propagation are executable (`uncache-widths`, `uncache-errors`); PBMT=IO direct MMIO load/store bypass, metadata/error path, and SQ retirement are executable (`mmio-contracts`); malformed/duplicate/early/late response injection remains planned |
| Trigger and DynInst sidebands | Independent CSR trigger update plus explicit LSQ enqueue exception/trigger/flush and scalar issue RVC/FTQ/store-set/load-wait fields | Trigger breakpoint cause/action, exception-vector mapping, and no dropped issue sideband | Partial: scalar load breakpoint and scalar issue RVC/FTQ/store-set/load-wait paths are executable; generated enqueue exception-vector mapping is unit-tested, while the top-level issueLda boundary does not expose that vector; debug-mode, chained trigger, and broad sideband randomization remain |
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
| Frontend bridge | Concurrent ICache, ICache-control, and instruction-Uncache A/D traffic; credit-safe source wrap; Get/PutFull/PutPartial; 1/2/4/8-byte control requests; full/partial masks; two-beat ICache responses; corrupt instruction data; randomized producer/consumer stalls | Exact in-order field transformation across both buffer stages, no loss/duplication, request/response/source-credit stall coverage, SVA stability, and final quiescence |
| Scalar loads | `lb/lh/lw/ld/lbu/lhu/lwu`; all three lanes; aligned and misaligned | ISA extension, exact data, metadata, replay, exception, LQ drain |
| Scalar stores | `sb/sh/sw/sd`; both address/data lanes and both issue orders | Both completions, exact byte mask/readback, SQ drain |
| Vector loads | EEW 8/16/32/64; both lanes; unit, strided, indexed unordered/ordered; mask, `vstart`, partial `vl`; split windows | Exact 128-bit result, active mask, metadata, replay, LQ drain; each address mode counted independently |
| Vector stores | All EEWs and address modes; mask, `vstart`, partial `vl`; misaligned and cross-page split/replay | Exact active-byte readback, completion, commit, SQ drain; each address mode counted independently |
| Software prefetch | `prefetch.i/r/w`, all scalar issue lanes, mapped and unmapped VAs | Completion without RF write or exception; LQ drain |
| Hardware data prefetch | Fixed-PC cold loads with a 128-byte stride, weighted by the common `stride-stream` constraint and mixed with cache/TLB/manager pressure | Focused exact L2 target/source/confidence checks; random-mixed source-12 observation; no output after CSR disable; L3 idle in the current disabled configuration |
| Translation mode selection | Legal decoded `satp` Bare/Sv39/Sv48, `vsatp` Bare/Sv39/Sv48, and `hgatp` Bare/Sv39x4/Sv48x4 inputs | Selected mode is reflected after the required flush; no stale translation from the previous legal mode; architectural CSR WARL write/readback behavior is integration-level |
| Sv39 | 3-level walk; 4-KiB, 2-MiB, and 1-GiB leaves; low and high canonical VAs; both noncanonical sign-extension directions; cold/warm reuse | PA-derived data, exact page/access fault, PTW activity/reuse, leaf alignment; high-half canonical and both noncanonical directions are executable |
| Sv48 | 4-level walk; 4-KiB, 2-MiB, 1-GiB, and 512-GiB leaves; L3 non-leaf and leaf faults; low and high canonical VAs; both noncanonical sign-extension directions | PA-derived data, exact page/access fault, fourth-level walk, canonicality fault; 4-KiB and superpage paths, high-half canonical, and both noncanonical directions are executable |
| G-stage Sv39x4 | 3-level walk with 16-KiB root and 41-bit GPA; 4-KiB/2-MiB/1-GiB leaves; high-GPA overflow | Host PA-derived data or guest-page fault; exact GPA and root/index alignment; high-GPA execution is covered by `translation-faults` |
| G-stage Sv48x4 | 4-level walk with 16-KiB root and 50-bit GPA; 4-KiB/2-MiB/1-GiB/512-GiB leaves; high-GPA overflow | Host PA-derived data or guest-page fault; exact GPA and fourth-level walk; 4-KiB and superpage paths are implemented |
| Nested translation | `Sv39 -> Sv39x4`, `Sv39 -> Sv48x4`, `Sv48 -> Sv39x4`, `Sv48 -> Sv48x4`; VS-only and G-only Bare degenerations; cold/warm reuse | Correct stage composition, stage-specific permissions/faults, exact VS VA and G-stage GPA metadata; Bare and high-GPA cases are executable |
| Translation faults | Invalid/non-leaf-at-level-0, W=1/R=0, invalid PTE, misaligned superpage, reserved/N/PBMT bits, stage-1 vs G-stage access fault | First-fault level and cause are independent of DUT internals; no data or forbidden RF/store side effect; scalar store against a read-only leaf must produce `StorePageFault` without a DCache/Uncache request |
| Translation context | HS/VS/VU privilege, ASID/VMID changes, same VA with different roots, `V` transition, `MXR/SUM` at the correct stage | Context-tagged translations do not alias; stage-specific permission rules and flush behavior hold |
| Translation fences | `SFENCE.VMA`, `HFENCE.VVMA`, `HFENCE.GVMA`, root/mode/ASID/VMID changes with outstanding requests | Global/selective leaf-update invalidation, same-ID host-ASID/VS-ASID/VMID root reuse with targeted fences, delayed stale responses, and co-issued distinct-page stage-1/nested walks across both stage-1 modes, both G-stage modes, and all four fully nested pairs are implemented at global/selective scope by `translation-fence-all`; `translation-inflight-context-all` separately covers redirected root/ID changes while a PTW response is delayed |
| DCache | Cold miss, warm hit, same-set pressure beyond eight ways, dirty eviction | Refill correctness, no extra miss for mandatory warm control, dirty ReleaseData preservation |
| Forwarding | Scalar-to-scalar, vector-to-vector, scalar-to-vector, vector-to-scalar; masks and widths | Byte-accurate overlay before store commit |
| Mixed pressure | Constrained-random windows enqueue scalar load, scalar store, vector load, vector store, and prefetch together; issue order, store address/data order, vector modes, alignment, and manager delays vary before a bounded drain | Every window records at least two unresolved classes (normally all five); all scoreboards drain; per-class coverage gates and exact LQ/SQ accounting |
| Redirect | Younger cold miss redirected while traffic is outstanding | No stale writeback and legal pointer reuse |
| Queue pressure/wrap | Two 60-entry LQ waves; more than 72 LQ and 160 ROB positions over long runs | Every accepted item retires or is explicitly canceled; flag/value identity remains continuous |
| Backpressure | Independent deterministic gaps on DCache A/D, PTW A/D, and uncache request/response | Ready-valid stability plus eventual progress |

Every `random-mixed` seed contains mandatory phases before constrained-random traffic. The tail is made of rolling five-class windows, not isolated tests. The
requested transaction count is the total action budget, including the mandatory
prefix; only the tail is constrained-random:

- all scalar load and store widths and all scalar issue lanes;
- LSQ dispatch widths one through six and every physical dispatch lane;
- all vector EEWs and unit/strided/indexed-unordered/indexed-ordered modes;
- masked/unmasked, zero/nonzero `vstart`, full/partial `vl`, aligned/split data;
- scalar and vector misaligned stores with replay and exact readback;
- Sv39/Sv48 and Sv39x4/Sv48x4 cold/warm translation through all four nested
  mode pairs (the deterministic `translation-matrix` covers the same matrix),
  plus an exact vector VS-non-leaf guest-page fault;
- mapped/unmapped software prefetch, PBMT-NC store/load, DCache dirty eviction,
  redirect recovery, simultaneous heterogeneous issue, and both cross-type
  forwarding directions;
- fixed-PC stride streams mixed with scalar/vector/atomic/NC/MMIO traffic,
  translation changes, cache refills, Probes, and variable manager latency.

There is one canonical mixed generator. Realistic traffic, balanced coverage,
and corner pressure are constraint sets over that generator, not independently
maintained scenario implementations. `--constraints coverage|spec|corner`
selects a baseline and repeatable `--constraint key=value` arguments override
operation mix, address locality, heterogeneous overlap, translation regime and
Sv39/Sv48/VS/G modes, context-switch and legal fence kind/scope rates,
misalignment, vector corner bias, atomic family/width, NC/MMIO direction, legal
special overlap, hardware stride-stream pressure, and independent
DCache/PTW/Uncache response latency. The complete interface
and performance-counter calibration are specified in
[`CONSTRAINED_RANDOM.md`](CONSTRAINED_RANDOM.md).

Constraint-interface closure is tracked separately from architectural prefix
coverage. Atomic AMO/LRSC/AMOCAS family and W/D width, NC/MMIO load/store
ratios, legal NC/MMIO load overlap, translation regime/mode/fence selection,
and per-manager response latency are now validated, replayable fields with
observed coverage counters. Atomic operations
remain serializing actions because the MemBlock boundary explicitly blocks the
pipeline while LR/SC/AMO is active. Random error injection remains interface
work; it must be added to this generator, not as a separately maintained random
scenario.

Every result records both resolved targets and observed counts. Each nonzero
operation/locality and enabled atomic family/width or NC/MMIO direction is a
per-seed coverage obligation. The same applies to enabled translation regimes,
stage-1 modes, nested VS/G crosses, compatible fence kind/scope crosses, cold
walk/reuse, legal-special-overlap, TLB flush, and each manager's `spec` latency
constraint. This
prevents a valid constraint set from producing an accidentally untested short
seed.

The seed fails if any required class has a zero count, fewer than four mixed
windows, no sample with two unresolved classes, or if final queue conservation
fails. Each window first enqueues all five producer classes and may add an NC or
MMIO load, then varies issue
order, scalar store address/data order, vector address mode, mask, alignment,
cache residency, translation state, and manager delay while scoreboards remain
outstanding. A bounded drain occurs only after the window, preserving real
heterogeneous overlap without allowing unbounded pointer reuse.

### Translation Closure Phases

Translation coverage is closed in phases so a long green cacheable run cannot
mask an untested mode or a self-consistent reference-model bug:

| Phase | Required implementation | Exit criterion |
| --- | --- | --- |
| T0: mode contract | Enumerate legal decoded `satp`, `vsatp`, and `hgatp` MODE values at the MemBlock input; test CSR WARL writes at the owning CSR/full-core boundary | Bare/Sv39/Sv48 and Bare/Sv39x4/Sv48x4 MemBlock transitions pass; unsupported architectural writes retain the old CSR value in a separate integration test |
| T1: independent walks | Parameterized Sv39/Sv48 and Sv39x4/Sv48x4 builders, canonical/high-bit checks, root alignment, all leaf levels, superpage alignment | Four-level 4-KiB builders/walks, all supported superpage leaf levels, valid high halves, both noncanonical sign-extension directions, and G-stage high-bit overflow are executable; broader boundary crosses remain |
| T2: nested composition | Independent VS walk plus G walk for all four mode pairs, plus VS-only/G-only/Bare degenerations | Four-pair 4-KiB matrix has cold and warm PA checks, implicit page-table accesses, and no stage elision; Bare degenerations execute in `translation-bare` |
| T3: protection/faults | PTE V/R/W/X/U/G/A/D, PBMT/N/reserved bits, SUM/MXR, stage-specific access type, noncanonical VA, high-GPA overflow | Each invalid class produces the correct stage/cause/VA/GPA and no forbidden side effect |
| T4: context and fences | ASID/VMID reuse, root changes, `V` transitions, `SFENCE.VMA`, `HFENCE.VVMA`, `HFENCE.GVMA`, outstanding walks | Host/VS/G root and ID switches, host/nested `V` transitions, global/selective leaf-update fences, targeted same-ID host-ASID/VS-ASID/VMID root reuse, global/selective stale-response races, co-issued distinct-page walks, and redirected root/ASID/VMID/MODE/`V` changes with a delayed walk execute through both stage-1 modes and both G-stage modes |
| T5: MemBlock stress | Mix all closed translation modes with LSQ wrap, split accesses, cache misses, redirect, and manager backpressure | The common random tail weights Bare/Sv39/Sv48 and all four nested pairs, switches only at drained boundaries, emits context-compatible global/selective fences, and requires every enabled mode/pair plus cold/reuse observations before a seed passes; long campaign evidence remains the acceptance criterion |

T1-T4 are required before the corresponding rows can be marked implemented.
T5 is the stress layer, not a substitute for deterministic mode/fault tests.

### High-Pressure Constrained-Random UT

`random-mixed` with configurable constraints is the primary campaign path.
`random-stress` is retained for compatibility with historical artifacts and its
burst-specific acceptance gates; new workload directions are added as common
constraint dimensions rather than new generators. A legacy stress burst
contains one or two transaction groups. Each
group is enqueued before issue and includes independent scalar/vector accesses,
store-to-load byte overlays, and prefetch traffic. The issue scheduler chooses
random legal candidates while preserving only the scalar and vector forwarding
dependencies; completion and ROB/LSQ retirement are delayed until the burst is
fully populated.

The stress gate is deliberately combination-based rather than only marginal:
all scalar load/store operations, all vector EEWs, unit/strided/indexed-unordered
vector modes, both vector lanes, mask and unmask, zero and nonzero `vstart`,
full and partial `vl`, aligned and split addresses, both store issue orders,
both cache regions, scalar/vector forwarding, DCache request/response stalls,
at least ten outstanding scoreboard entries, and four cross-feature combination
counters derived from generated burst fields must be nonzero in every accepted
seed. Stress forwarding stores use
non-overlapping positive/negative strided addresses; zero-stride vector loads
remain in the independent mixed-load coverage, while repeated-address stores
remain in deterministic overlap tests. Ordered-indexed vector issue
continues to be checked by `random-mixed`, where its required older-LSQ drain is
modeled explicitly. A stress result with a passing terminal marker but missing
one of these combinations is rejected by `verify-stress-results`.

Before a duration campaign starts, the executable, Verilated model, xspcomm,
and prepared RTL metadata are copied into one read-only runtime directory and
hashed. The runner and verifier both use that frozen RTL metadata rather than
the mutable preparation path. `STRESS_TRANSACTIONS` is also passed to both the
stress command and the verifier's generic transaction-count check. These are
acceptance invariants: an all-pass simulator result is evidence, but is not an
accepted campaign when its frozen provenance cannot be verified.

## Complete Verification-Point Inventory

The following inventory is the closure target for the MemBlock boundary. Each
row is classified as `implemented`, `partial`, or `planned`; a planned row is a
known gap and must not be reported as green merely because the surrounding
cacheable tests pass.

### Instruction and data-shape points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| Integer loads | `lb/lbu/lh/lhu/lw/lwu/ld`, all destination classes, all issue lanes, zero/sign-extension patterns | Implemented |
| Integer stores | `sb/sh/sw/sd`, address-first/data-first, byte masks, all issue lanes, commit timing | Partial; issue/commit and modeled readback are covered, but the boundary exposes no store payload monitor |
| Floating loads/stores | FLW/FLD and narrow/widening formats, NaN-boxing, FP exception bits, integer/FP destination separation | Planned at MemBlock boundary |
| Vector unit stride | EEW 8/16/32/64, `vl=0..VLEN`, `vstart` at start/middle/end, `vm`, mask holes, `vma/vta` | Implemented for modeled 128-bit operations |
| Vector strided | Positive, zero, and negative legal load strides; non-overlapping positive and negative store strides; element gaps and split windows | Implemented for modeled 128-bit operations; overlapping stores remain excluded because their final memory value is not a single deterministic oracle |
| Vector indexed unordered | Repeated indices, aliasing, non-monotonic indices, all EEWs, masked elements | Partial; basic unordered mode implemented |
| Vector indexed ordered | Strict element order, repeated/aliasing indices, split beats/pages | Partial; basic ordered mode implemented |
| Vector segmented/whole-register | NF/segment count, multi-uop streams, fault-only-first and partial completion | Planned |
| Vector data patterns | all zero/one, ramps, alternating bits, random bytes, same-byte aliases, old-destination merge | Implemented/partial by operation class |
| Software prefetch | `prefetch.i/r/w`, mapped/unmapped, cacheable/NC, all lanes, duplicate and outstanding requests | Implemented for modeled software prefetch |
| Atomics | LR/SC, AMOADD/XOR/AND/OR/SWAP/MIN/MAX and signed/unsigned variants, AMOCAS, reservation loss, alignment | Partial; all exposed W/D-width AMO variants, AMOCAS.W/D compare success/failure, LR/SC success/failure, and every illegal byte offset for representative D/W operations execute in `atomic-contracts`; `atomic-dchannel-errors` crosses denied/corrupt with all 22 refill-capable W/D LR/AMO/AMOCAS operations, checks initial exception/RF contracts, later poisoned-line load hits, SC.W/D hits on denied/corrupt metadata, exact request counts, and clean AMO recovery. The SC checks do not claim internal reservation observability. SC cannot have a cold-miss D response because MainPipe returns failure before a request when the line or usable reservation is absent. Cross-hart reservation interference, full opcode-by-offset alignment crosses, and ordering with concurrent traffic remain |
| CBO/CMO/fences | clean/invalidate/flush/zero, `fence`, `fence.i`, `sfence.vma`, ordering with outstanding traffic | Partial; cacheable `CBO.ZERO` StoreQueue/SBuffer line-zero and readback are executable (`cbo-zero-contracts`), and global `SFENCE.VMA` leaf-update behavior is implemented; CMO CLEAN/FLUSH/INVAL, `fence.i`, and full ordering remain because `cmoOpResp` is internal to DCache rather than a MemBlock top-level port |
| Hypervisor memory ops | HLV/HLVX/HSV, effective privilege/SPVP, execute permission, guest/host faults | Planned |

### Address, translation, and protection points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| Alignment | every byte offset for widths 1/2/4/8/16, beat boundary, 64-byte line, 4 KiB page, two-page and two-line splits | Partial; common scalar/vector split classes implemented |
| Virtual address classes | Sv39 canonical low/high halves, Sv48 canonical low/high halves, noncanonical bits, page-offset boundaries, aliasing VAs, VA wraparound | Partial; low/high canonical halves and both noncanonical sign-extension mismatch directions execute for Sv39 and Sv48; broader page-offset, aliasing, and wraparound crosses remain |
| Physical address classes | cacheable, uncached, device, reserved, high PA bits, line/set/way aliases | Partial; cacheable and PBMT-NC plus a non-DebugModule SoC `c=0` PMA device interval and the guarded DebugModule denial are executable in `mmio-contracts`; reserved/high-PA/alias matrix remains |
| Stage-1 Sv39 walk | Bare/Sv39 mode, L2/L1/L0 leaves (1-GiB/2-MiB/4-KiB), invalid/non-leaf, misaligned superpage, canonicality, permission/PBMT/A-D combinations | Partial; all leaf levels, high-half canonical VA, both noncanonical sign-extension directions, malformed superpage, U/S, SUM, MXR, missing-A/D, and 13 invalid/reserved/PBMT/NAPOT encoding cases execute for loads and stores; broader crosses remain |
| Stage-1 Sv48 walk | Bare/Sv48 mode, L3/L2/L1/L0 leaves (512-GiB/1-GiB/2-MiB/4-KiB), L3 faults, canonicality, permission/PBMT/A-D combinations | Partial; all leaf levels, high-half canonical VA, both noncanonical sign-extension directions, U/S, SUM, MXR, missing-A/D, and the same 13 encoding cases execute for loads and stores; broader crosses remain |
| G-stage Sv39x4 walk | 16-KiB root, widened root index, 41-bit GPA, all leaf levels, high-GPA overflow, G-stage permissions | Partial; all leaf levels, high-GPA overflow, load/store permission faults including `D=0`, all valid two-stage PBMT combinations, and 13 invalid/reserved/PBMT/NAPOT encoding cases execute for loads and stores |
| G-stage Sv48x4 walk | 16-KiB root, widened root index, 50-bit GPA, all leaf levels, high-GPA overflow, G-stage permissions | Partial; all leaf levels, high-GPA overflow, load/store permission faults including `D=0`, all valid two-stage PBMT combinations, and the same 13 encoding cases execute for loads and stores |
| Nested mode matrix | `Sv39->Sv39x4`, `Sv39->Sv48x4`, `Sv48->Sv39x4`, `Sv48->Sv48x4`, plus `vsatp`/`hgatp` Bare degenerations | Partial; all four 4-KiB pairs are executable in `translation-matrix`, including cold/warm TLB reuse; VS-only/G-only/fully-Bare degenerations execute in `translation-bare`; the same four non-Bare pairs are independently weighted and required in `random-mixed` |
| Stage-only translation | HS/S/U stage-1 only, VS/VU stage-1 only, G-stage only for implicit page-table/HLV-class accesses | Partial; only current data-access paths are modeled |
| TLB behavior | cold miss, hit, refill, duplicate miss, replay, invalidation, `sfence.vma`, concurrent page walks | Partial; `translation-fence-all` holds stale stage-1, VS-only, G-only, and fully nested VS/G leaf responses across global and selective fences for both supported modes and all four nested pairs, rejects canceled writeback, requires new-data refills, and co-issues two distinct-page stage-1 plus nested walks before the first delayed PTW response; `random-mixed` requires observed cold-walk and warm-reuse windows plus every enabled legal fence cross; broader duplicate-miss source-reuse stress remains |
| Page permissions | R/W/X/U/G, SUM/MXR at HS/VS stage, G-stage U-mode rule, read-only store, execute-only, access/dirty bit updates, privilege transitions | Partial; `translation-permissions` executes 58 cases across Sv39/Sv48 U/S, SUM, MXR, A/D, VSUM/VMXR, all four nested mode pairs, and G-stage load/store R/A/D/U permissions with exact readback, fault cause, manager non-use, and SQ conservation; `translation-faults` covers PBMT/reserved encoding faults, while `translation-pbmt` covers valid PBMT composition |
| Mode/context switching | `satp/vsatp/hgatp` root and MODE changes, ASID/VMID reuse, `V` transitions, same VA under distinct contexts | Implemented for the modeled modes; `translation-context` covers five drained context families and 14 distinct-data same-address accesses; `translation-fence` adds same-ID host-ASID/VS-ASID/VMID fenced root reuse; `translation-inflight-context-all` holds an old PTW response for 256 cycles, changes root/ID/MODE or `V`, redirects the old load, reuses its ROB/LQ identity, and requires the new PA for both host mode directions plus all four nested starting pairs and both host/nested directions |
| Translation fences | `SFENCE.VMA`, `HFENCE.VVMA`, `HFENCE.GVMA`, selective/global scope and updates with outstanding traffic | Partial; global/selective leaf updates, targeted same-ID host-ASID/VS-ASID/VMID root reuse, stale-response races, and co-issued distinct-page stage-1/nested walks spanning both stage-1 modes, both G-stage modes, and all four fully nested pairs are implemented by `translation-fence-all`; the separate context-race matrix checks redirected root/ID changes while a response remains outstanding |
| PMP/PMA | TOR/NA4/NAPOT, overlap priority, lock, M/R/W/X, cacheability, atomic/MMIO permissions, exact region edges | Planned |
| Fault classes | load/store/instruction access fault, stage-1 page fault, G-stage guest-page fault, noncanonical VA, high-GPA overflow, address-misaligned, access-denied, bus/ECC error | Partial; load/store/page/misaligned, noncanonical, high-GPA, and denied/corrupt D-channel cases are executable; PMP/PMA access-denied and physical ECC injection remain |
| Fault metadata | exact VA, GPA/PTE address, first failing level/stage, shifted `htval`-class value, guest marker, cause priority, single reporting and replay suppression | Partial; VS-non-leaf path implemented |

### Cache, memory-system, and coherence points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| Frontend pass-through | ICache line reads, instruction-Uncache reads, ICache-control Get/PutFull/PutPartial, source/size/mask/data, simultaneous paths, A/D backpressure | Implemented at the MemBlock top boundary by `frontend-bridge`; downstream ICache/device semantics remain integration responsibilities |
| DCache lookup | warm hit, cold miss, same-line merge, bank conflict, set pressure beyond associativity, synonym/alias | Partial; cold/warm and dirty set pressure implemented |
| Refill/replay | delayed A/D responses, beat reordering where legal, partial refill, killed request, replay after miss | Partial |
| Eviction | clean release, dirty ReleaseData, partial byte masks, replacement under pressure, release backpressure | Partial; immutable whole-line snapshot is checked for the dedicated dirty-pressure phase, while broader release/response classes remain planned |
| TileLink coherence | Probe/B/C/E traffic, source reuse, denied/corrupt/error responses, manager ordering | Partial; `dcache-coherence` executes three manager Probes spanning clean no-data, clean requested-data, and dirty mandatory-data responses, checks byte-exact C beats, forces E backpressure, and matches every GrantAck sink. `random-mixed` adds constrained dirty toB/toN Probes with requested/mandatory data and checked toB cleanup. Load and atomic denied/corrupt D responses are also injected and checked; concurrent Probe overlap, malformed responses, multiple Probe sources, and broader source-reuse stress remain planned |
| Uncache/MMIO | Get/Put widths, byte enables, side effects, ordering, response delay, denied/error response | Partial; PBMT-NC Get widths/byte lanes and scalar denied/corrupt response propagation are executable (`uncache-widths`, `uncache-errors`); PBMT=IO's direct three-cycle load metadata bypass plus scalar store request/response/SQ-retirement, DCache non-use, denied/corrupt load metadata preservation, and a physical non-DebugModule `c=0` PMA load/store pair are executable (`mmio-contracts`); cacheable CBO.ZERO line-zero/readback is executable (`cbo-zero-contracts`); device side effects and malformed/duplicate/early/late responses remain |
| ECC/cache errors | correctable/uncorrectable data, error lifetime, retry or architectural exception | Partial; D-channel denied/corrupt metadata persistence and subsequent clean AtomicsUnit recovery are executable in `dcache-errors` and `atomic-dchannel-errors`; physical tag/data-array ECC injection and retry policy remain planned |
| PTW manager | request/response backpressure, source reuse, malformed/denied response, concurrent walks | Partial; legal backpressure and response identity are implemented. Two co-issued distinct-page DTLB walks remain pending around a delayed response even where that scenario observes manager depth one; the separate IFU-plus-DTLB overlap reaches manager depth two. Two same-VPN IFU requests return twice while coalescing into one three-request Sv39 memory walk. Malformed/denied injection and broader source-reuse stress remain |

### Queue, ordering, and control points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| LQ/SQ occupancy | empty/near-full/full, wrap flags, simultaneous enqueue/dequeue, same-slot reuse | Implemented/partial |
| ROB age | ordinary and wrapped pointers, flag transitions, same-cycle issue/commit/redirect | Partial |
| Store-to-load forwarding | scalar-scalar, vector-vector, scalar-vector, vector-scalar, partial overlap, byte masks, older/younger stores | Implemented for modeled classes |
| Exception priority | multiple legal faults, differing ROB vs LQ/SQ age, load/store/vector competition | Planned |
| Redirect/recovery | kill each producer class, in-flight miss/replay, canceled prefetch, pointer reuse, survivor data | Implemented for basic redirect; VLS/segment cases planned |
| Fence/commit ordering | outstanding cache/uncache/PTW traffic across commit and fence boundaries | Planned |
| Backpressure | every producer/consumer ready-low pattern, long stalls, alternating stalls, response delay cross-product | Implemented for DCache/PTW/uncache and Probe responses; negative-error timing crosses remain planned |
| Reset/quiescence | reset asserted/deasserted at legal boundaries, idle cycles, reset with outstanding traffic, repeated reset | Partial; initial reset plus repeated reset with an outstanding translated load and post-reset survivor are executable (`reset-recovery`); reset of every producer class remains planned |

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
constraints, not a fixed list of replay cases. `random-stress` now derives
independent traffic, shape, payload, and scheduling streams from the scenario
seed with SplitMix64; the derivation is deterministic and part of replay
provenance. The per-seed gate still combines generator-side feature bins with
observed writeback/manager progress, so it is not a substitute for a future
independent starvation monitor or general simulator cross-coverage database.

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

## Top-Level IO Gap Audit (2026-09-06)

The checked manifest contains 1,335 flattened ports. Structural pin coverage is
not equivalent to semantic verification, so the review grouped the ports by
transaction owner and compared each group with a driver, monitor, oracle, and
coverage gate. The current group sizes are: 532 OOO-to-Mem, 260 Mem-to-OOO,
351 miscellaneous/control, 88 frontend TileLink, 53 DCache TileLink, 21 PTW
TileLink, 20 Uncache TileLink, eight performance, and two infrastructure ports.

| Boundary found in TOP IO | Semantic status | Closure action |
| --- | --- | --- |
| Frontend ICache, ICache-control, and instruction-Uncache TileLink (88 ports) | Implemented | `frontend-bridge` keeps independent request/response scoreboards and source credits for all three paths |
| DCache A/B/C/D/E TileLink (53 ports) | Implemented | A/C/D refill, error, eviction, and ReleaseData paths plus deterministic B Probe and E GrantAck are checked. `dcache-coherence` accepts two distinct B source IDs before an unrelated cold miss writes back, requires accepted Probe depth >= 2, and address-matches both C responses; C-source remains an independent WritebackQueue ID. `random-mixed` constrains dirty toB/toN Probe sequences and requested/mandatory data |
| PTW and Uncache manager boundaries (41 ports) | Partial | Legal traffic, long latency, and backpressure are implemented. Add malformed/duplicate/early/late responses as explicit negative protocol modes rather than normal workload traffic |
| IFU-to-Mem PTW request/response (`io_fetch_to_mem_itlb_*`, 64 ports) | Implemented | `ifetch-ptw-bridge` drives raw IFU PTW requests for valid/faulting Sv39/Sv48, all four nested pairs, both VS-only/G-only mode variants, PBMT=NC/IO, and all four nested pairs crossed with VS-leaf, final-G-leaf, and implicit page-table G-stage faults while forcing response backpressure and requiring cold page-table traffic. A 256-cycle IFU root-PTE delay overlaps a cold scalar DTLB walk and requires both translations, exact load writeback, and PTW outstanding depth >= 2. Two same-VPN IFU requests are accepted before the first response and must return twice while issuing only one three-level Sv39 memory walk. Delayed requests are invalidated by context switches and global/selective stage-appropriate fences; no stale response may escape, and the subsequent walk must return the independently computed replacement mapping |
| Backend `memoryViolation` (8), `ldCancel` (3), and wakeup (12) outputs | Partial, high priority | `load-feedback` independently samples all wakeup/cancel pins, matches lane and destination metadata, requires one uncanceled wakeup per completed cold/warm load without constraining replay count, and checks three resident same-bank loads with no TileLink request, at least two cancellations, and cross-lane wakeup conservation under replay migration. It separately classifies an Sv39 load-page fault, PBMT=IO MMIO, PBMT=NC, and store-forwarding data wait: the fault cancels every wakeup without DCache traffic; MMIO/NC each retain one uncanceled wakeup, use PTW plus Uncache, and avoid DCache; MMIO also exercises `pendingMMIOld`; forwarding requires a cancel while matching store data is unavailable and one exact final wakeup after data arrival without post-warmup DCache traffic. `dcache-errors` requires D-channel denied/access-fault and corrupt/hardware-error wakeups to be wholly canceled, and `scalar-guest-fault` applies that rule to an independently modeled G-stage fault. `random-mixed` gates canceled/uncanceled observations on every lane under mixed pressure; with hardware prefetch enabled it freezes this architectural gate before training and records full-run raw counts separately because prefetch cancels have no backend wakeup. `memory-violation` checks all eight redirect fields, a same-line byte-disjoint negative control, three-candidate oldest selection with ROB wraparound, a vector load flow behind an unresolved store, and two store-address lanes accepted together with the lane-1 candidate deliberately older to verify cross-source ROB-age arbitration; `rar-violation` checks dirty-line release ordering, the architectural enable, RAR's distinct `flushAfter` result, and three simultaneous RAR sources whose ROB identities cross 158, 159, and 0 with exact oldest selection. Add PMP/PMA denial and physical ECC cancellation crosses after those injection paths exist |
| IFU software instruction-prefetch outputs (`io_ifetchPrefetch_*`, 6 ports) | Implemented | `ifetch-prefetch` checks lane and virtual address for all three LoadUnit outputs, rejects data-prefetch false positives, and issues three unmapped requests together under active Sv39 while requiring exact per-lane VAs and zero PTW/DCache traffic. This matches LoadUnit's explicit `s0_tlb_no_query`; frontend owns subsequent fetch translation/faults. `random-mixed` requires an observed `prefetch.i` output in every seed |
| L2/L3 prefetch sender outputs (5 ports) | Partial | `hardware-prefetch` checks fixed-PC stride confidence, exact source-12 L2 address/count, PC-independent 12-line spatial-stream activation, exact source-11 640-line lookahead/four-line width, stream-over-stride priority, CSR-disable suppression, and the configured-off L3 invariant. `random-mixed` adds weighted fixed-PC stride streams under ordinary/corner miss, refill, translation, Probe, and latency pressure with a per-seed L2 source-12 gate. Add SMS/PHT source-10 causality, exact random-stream address attribution, and a positive L3-enabled configuration; direct SMS AGT generation is hard-disabled in this RTL |
| Store/vector IQ slow feedback | Implemented | `iq-slow-feedback` records every valid pulse and all exposed fields; checks both STA lanes returning same-cycle cold misses and warm hits with exact SQ identities, both VSTU lanes returning same-cycle hits with exact LQ/SQ and inactive replay metadata, and a strided misaligned vector-store partial replay with exact queue identity, nonzero replay mask, and merge-buffer index |
| Performance, trace, interrupt, DFT, hart/reset, and bypass plumbing | Structural or integration-owned | Keep pin/reset sanity in `pin-space`; test architectural interrupt/trace/DFT semantics at their owning integration boundary instead of claiming them from MemBlock data-path tests |

The ordering for the next closure work is: first physical-error `ldCancel`/
wakeup crosses once their injection paths exist, followed by hardware-prefetch
causality and negative protocol injection. Translation mode depth itself is no
longer
the dominant gap: deterministic tests already execute Sv48 and all four
Sv39/Sv48 x Sv39x4/Sv48x4 nested combinations. The separate IFU top-level
transaction now covers the same mode depth plus PBMT, stage-1/nested faults,
same-VPN duplicate coalescing, IFU/DTLB cross-source concurrency, context
changes, and global/selective stage-appropriate fences.

## Interface Assumptions

| Interface family | Environment assumption | Enforced rule |
| --- | --- | --- |
| Issue | Legal operation encoding and LSQ/ROB pointer; payload held until `ready` | Typed drivers retain `valid` and payload through acceptance |
| LSQ dispatch/commit | Allocations and commits are in legal backend order | Drivers allocate the correct scalar/vector count and never commit unallocated work |
| Redirect | ROB pointer/flag and level describe a legal backend redirect | The scoreboard removes only architecturally younger work |
| DCache manager | Coherent 64-byte lines on a 256-bit bus; finite randomized delay | Agent drives and observes A/B/C/D/E traffic, distinguishes Get/AcquireBlock/AcquirePerm, checks ProbeAck(Data), ReleaseData, and GrantAck, and applies A/C/D/E timing pressure; common random Probe injection is implemented while concurrent/multiple-source and malformed response modes remain planned |
| PTW manager | PTE memory and response source/size match the programmed roots | Reference page tables and PTW agent share sparse memory; malformed response validation is planned |
| Uncache manager | Only modeled Get/Put requests receive AccessAck/Data | Ordered byte-level update is modeled; scalar size/address/mask/lane, response identity, denied/corrupt exception checks, and PBMT=IO store request/retirement are executable (`uncache-widths`, `uncache-errors`, `mmio-contracts`); CBO.ZERO is covered through the cacheable SBuffer path (`cbo-zero-contracts`), while malformed/duplicate/early/late responses remain planned |

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
The `frontend-bridge` scenario now adds semantic coverage for all 88 ports in
that manifest group; it does not promote unrelated pin-sweep-only groups to
functional coverage.

## Known Boundary Gaps

The current harness does not yet own a complete legal producer, independent
reference model, or externally observable contract for the following. These
are planned work items, not silently accepted coverage:

- floating-point store formatting, NaN-boxed MMIO loads, and FP exception side
  effects (cacheable FLW/FLD destination-class and NaN-boxed FLW data are
  covered by `fp-loads`);
- MMIO/device reads and writes with side effects, ordering, denied responses,
  and ROB marking;
- cross-hart LR/SC reservation interference, atomic ordering with concurrent
  traffic, full opcode-by-offset alignment crosses, and tag/data-array ECC
  injection (all operation encodings and reachable D-channel denied/corrupt
  cases are covered);
- CBO/CMO line operations, `fence`, and `fence.i` (translation-fence ordering
  for global/selective leaf updates, both supported stage modes, and all four
  fully nested VS/G pairs is covered by `translation-fence-all`);
- VSegment/VFOF takeover, multi-uop segment streams, fault-only-first, and
  segment-specific redirect behavior;
- HLV, HLVX, HSV, SPVP, final physical execute permission, and hypervisor PMP;
- architectural `satp`/`vsatp`/`hgatp` write/readback and WARL mode filtering;
  the MemBlock UT directly supplies the post-CSR `TlbCsrBundle` and therefore
  cannot establish software-visible Sv48/Sv48x4 enablement by itself;
- complete PMP/PMA TOR/NA4/NAPOT, lock, priority, cacheability, and region-edge
  matrices;
- hardware A/D updates and concurrent page-walk invalidation races (missing-A
  load/store faults, missing-D store faults, and
  SUM/MXR/VSUM/VMXR permission outcomes are covered by
  `translation-permissions`; global/selective single-walk races for both stage
  modes and all four fully nested pairs are covered by
  `translation-fence-all`);
- manager-originated TileLink probes, probe acknowledgements, denied/corrupt
  responses, ECC errors, and coherence error recovery;
- simultaneous malformed/duplicate/early/late PTW and uncache responses;
- concurrent wrapped-age load/store/vector exception priority;
- multiple simultaneous split misaligned loads under LQ-RAR pressure;
- multi-uop vector streams and broader overlapping indexed operations;
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
3. A multi-seed, six-scenario matrix passes with backpressure enabled. The
   sixth scenario is `frontend-bridge`; its offline verifier independently
   requires three A requests and four D beats per requested transaction,
   request/response/source-credit stalls, and at least 39 field checks per
   transaction.
4. The executable, Verilated model, xspcomm, resolved system libraries, runner,
   streaming verifier, runtime-freeze script, controller sources, and RTL metadata are frozen and
   hashed. The verifier script is passed as a controller input so its acceptance
   logic is part of the recorded provenance.

The final campaign runs at least 28,800 monotonic seconds (eight hours) with eight
workers and the `random-mixed --constraints spec` scenario. Each seed requests 16,384 total
actions, including the mandatory coverage prefix and a constrained-random tail
with five-class overlap windows. Every window
randomizes producer parameters, legal alignment class, data, masks, vector
shape, issue order, store half order, and manager delay; mandatory sanity waves
are kept only where the interface has a proven legal encoding. The artifact
records the requested command count and completed summary count, and the
independent verifier checks both. At least 128 complete seeds are required, and
the final result must complete after the duration deadline. Work already
submitted at the deadline is allowed to finish. Any nonzero return, timeout, assertion, scoreboard error,
coverage-gate failure, provenance change, or discontinuous seed range fails
acceptance.

The separate `random-boundary-hunt` campaign is a diagnostic and repair gate:
it creates a fresh Sv39x4 environment for each sample, randomizes the faulting
VA offset, EEW, `vl/vstart`, mask, lane, data, and backpressure, then compares
the exact VS-non-leaf GPA and first-active-element VA against the software
oracle. On the historical clean RTL it must produce at least one mismatch;
after the fix it must produce an all-pass campaign with the same oracle.

An independent streaming verifier (whose own SHA-256 and runtime-freeze script
are recorded in the controller inputs) checks the result artifact, duration,
scenario set, transaction counts, per-seed coverage, aggregate counts,
continuous seeds, eight-worker configuration, complete RTL identity, and
before/after frozen-runtime hashes. It also requires schema-2 completion state,
a unique run id, finite timing values, strict scenario/seed/count terminal
summaries, and per-seed submit/complete offsets spanning the requested duration.

## Failure Triage

1. Replay the exact scenario, seed, transaction count, and frozen hashes.
2. Reduce the transaction prefix while preserving memory and backpressure RNG
   streams.
3. Check the external request/response transcript against the reference model.
4. Add a focused architecture/protocol reproducer before inspecting internal
   state.
5. If historical sensitivity matters, regenerate a separate revert RTL tree
   and require clean pass plus mutant fail under identical test sources.
