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
is added to the harness.

## Stable Oracles

| Contract | Independent oracle | DUT obligation | Status |
| --- | --- | --- | --- |
| Scalar load | Byte-addressed sparse memory plus ISA width/sign extension | Exactly one completion with the expected ROB/destination, data, and exception bits | Implemented for modeled scalar loads |
| Scalar store | ISA store width/mask applied to an architectural reference separate from bus backing memory | Address/data writebacks compare exception/ROB/flush/debug metadata; committed bytes are recovered by an independent load. PBMT=IO stores must use Uncache, avoid DCache, complete the response/writeback sequence, and retire the SQ entry | Implemented for modeled scalar stores and scalar PBMT=IO contract |
| Vector load | Independent unit/strided/indexed address decoder, `vl`/`vstart`/mask rules, old destination, and legal `vma/vta` agnostic values | Exact active data and active-element mask for EEW 8/16/32/64; inactive data is constrained by RVV policy | Implemented for modeled 128-bit operations |
| Vector store | The same independent address/mask decoder applied to source bytes | Eventual completion/commit, exact vector readback of every active byte, RF write-enable/flush metadata, and optional trigger/debug metadata | Implemented for modeled 128-bit stores |
| Address translation | Mode-parameterized software walk for Bare/Sv39/Sv48 and Sv39x4/Sv48x4; independent canonicality, PTE validity, leaf level, alignment, permission, PBMT, and A/D checks | Accesses reach the independently calculated PA; invalid walks report the access-specific page/access/guest-page fault | Partial: generic walker, Bare degenerations, all four 4-KiB nested paths, and superpage leaves are implemented; full protection/fault matrix remains |
| Nested translation | Independent VS-stage walk followed by independent G-stage walk for all four `vsatp` x `hgatp` mode pairs, including implicit page-table accesses | Exact host PA or stage-specific fault; no stage may be skipped or silently treated as Bare | Partial: all four 4-KiB pairs and VS/G/Bare degenerations are covered by `translation-matrix`/`translation-bare`; context/fence isolation remains |
| L2-to-L1 DTLB boundary | Drive all retained `io_l2_tlb_req_req_*` fields, including ordinary and prefetch requests, kill/no-translate controls, and response timing | Legal response valid/miss/PBMT/fault fields and exported PMP/MMIO classification; cold misses are delegated to the external L2 TLB | Implemented for ordinary and prefetch miss responses in `l2-tlb-contracts`; the MemBlock boundary has no L2 refill response input, so hit refill and external retry remain integration-level tests |
| L2 hint propagation | Valid/invalid `io_l2_hint`, all `sourceId` values, and `isKeyword` polarity at an idle/no-matching-MSHR boundary | Hint is registered and distributed without producing a ghost writeback, queue corruption, or protocol error; matching-MSHR replay semantics are integration-tested with L2 | Implemented for both keyword polarities and all 16 source IDs with an idle no-MSHR safety oracle in `l2-tlb-contracts`; matching-MSHR replay remains an L2 integration scenario |
| Guest-fault metadata | Reference VS/G-stage walk from PTE addresses, including explicit data faults and implicit VS-page-table faults | Exact fault VA, faulting PTE GPA, shifted `htval`-class value where observable, and VS-non-leaf-PTE marker | Partial: current VA/GPA/marker cases are covered; broader fault classes are planned |
| Misalignment | Byte concatenation/splitting across 16-byte, line, and page boundaries | Exact value/bytes when enabled; specified address-misaligned exception when disallowed by memory type/control | Partial: common scalar/vector splits are covered |
| Exception side effects | RISC-V exception contract | Exact exception bit; exceptional scalar load has no integer/FP RF write; software prefetch never raises a load exception or writes an RF | Partial: concurrent priority and full cause matrix are planned |
| Redirect | ROB age and redirect level supplied by a legal backend transaction | Redirected younger work has no terminal writeback; surviving work completes with the same data | Partial: basic redirect is covered; cancellation observation is driver-accounted |
| Cache coherence boundary | TileLink opcode/source/size/mask/data reference agent with separate bus and architectural memories | Stable producer payload while stalled, complete refill, ReleaseAck, byte-exact dirty ReleaseData, atomic refill/update, and denied/corrupt D-channel handling | Partial: DCache denied/corrupt load errors and all 22 refill-capable W/D LR/AMO/AMOCAS error paths are executable; `atomic-dchannel-errors` checks poisoned-line installation, persistent denied/corrupt metadata on later load and SC hits, exact exceptions, suppressed `rfWen`, and clean error-state recovery (`dcache-errors`, `atomic-contracts`, `atomic-dchannel-errors`). Manager-originated probes and E-channel behavior remain planned |
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
| Scalar loads | `lb/lh/lw/ld/lbu/lhu/lwu`; all three lanes; aligned and misaligned | ISA extension, exact data, metadata, replay, exception, LQ drain |
| Scalar stores | `sb/sh/sw/sd`; both address/data lanes and both issue orders | Both completions, exact byte mask/readback, SQ drain |
| Vector loads | EEW 8/16/32/64; both lanes; unit, strided, indexed unordered/ordered; mask, `vstart`, partial `vl`; split windows | Exact 128-bit result, active mask, metadata, replay, LQ drain; each address mode counted independently |
| Vector stores | All EEWs and address modes; mask, `vstart`, partial `vl`; misaligned and cross-page split/replay | Exact active-byte readback, completion, commit, SQ drain; each address mode counted independently |
| Software prefetch | `prefetch.i/r/w`, all scalar issue lanes, mapped and unmapped VAs | Completion without RF write or exception; LQ drain |
| Translation mode selection | `satp` Bare/Sv39/Sv48; `vsatp` Bare/Sv39/Sv48; `hgatp` Bare/Sv39x4/Sv48x4; supported-to-unsupported mode transitions | CSR mode is reflected after the required flush; no stale translation from the previous mode |
| Sv39 | 3-level walk; 4-KiB, 2-MiB, and 1-GiB leaves; low and high canonical VAs; cold/warm reuse | PA-derived data, exact page/access fault, PTW activity/reuse, leaf alignment |
| Sv48 | 4-level walk; 4-KiB, 2-MiB, 1-GiB, and 512-GiB leaves; L3 non-leaf and leaf faults; low and high canonical VAs; noncanonical VA | PA-derived data, exact page/access fault, fourth-level walk, canonicality fault; 4-KiB and superpage paths plus noncanonical execution are implemented |
| G-stage Sv39x4 | 3-level walk with 16-KiB root and 41-bit GPA; 4-KiB/2-MiB/1-GiB leaves; high-GPA overflow | Host PA-derived data or guest-page fault; exact GPA and root/index alignment; high-GPA execution is covered by `translation-faults` |
| G-stage Sv48x4 | 4-level walk with 16-KiB root and 50-bit GPA; 4-KiB/2-MiB/1-GiB/512-GiB leaves; high-GPA overflow | Host PA-derived data or guest-page fault; exact GPA and fourth-level walk; 4-KiB and superpage paths are implemented |
| Nested translation | `Sv39 -> Sv39x4`, `Sv39 -> Sv48x4`, `Sv48 -> Sv39x4`, `Sv48 -> Sv48x4`; VS-only and G-only Bare degenerations; cold/warm reuse | Correct stage composition, stage-specific permissions/faults, exact VS VA and G-stage GPA metadata; Bare and high-GPA cases are executable |
| Translation faults | Invalid/non-leaf-at-level-0, W=1/R=0, invalid PTE, misaligned superpage, reserved/N/PBMT bits, stage-1 vs G-stage access fault | First-fault level and cause are independent of DUT internals; no data or forbidden RF/store side effect; scalar store against a read-only leaf must produce `StorePageFault` without a DCache/Uncache request |
| Translation context | HS/VS/VU privilege, ASID/VMID changes, same VA with different roots, `V` transition, `MXR/SUM` at the correct stage | Context-tagged translations do not alias; stage-specific permission rules and flush behavior hold |
| Translation fences | `SFENCE.VMA`, `HFENCE.VVMA`, `HFENCE.GVMA`, root/mode/ASID/VMID changes with outstanding requests | Global/selective `SFENCE.VMA`, selective `HFENCE.VVMA`, and global `HFENCE.GVMA` leaf-update invalidation are implemented by `translation-fence`; outstanding-walk ordering remains |
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
  forwarding directions.

There is one canonical mixed generator. Realistic traffic, balanced coverage,
and corner pressure are constraint sets over that generator, not independently
maintained scenario implementations. `--constraints coverage|spec|corner`
selects a baseline and repeatable `--constraint key=value` arguments override
operation mix, address locality, heterogeneous overlap, TLB flush rate,
misalignment, vector corner bias, and response latency. The complete interface
and performance-counter calibration are specified in
[`CONSTRAINED_RANDOM.md`](CONSTRAINED_RANDOM.md).

Every result records both resolved targets and observed counts. Each nonzero
operation/locality weight is a per-seed coverage obligation; nonzero TLB-flush
and `spec` latency constraints similarly require observed events. This prevents
a valid constraint set from producing an accidentally untested short seed.

The seed fails if any required class has a zero count, fewer than four mixed
windows, no sample with two unresolved classes, or if final queue conservation
fails. Each window first enqueues all five producer classes, then varies issue
order, scalar store address/data order, vector address mode, mask, alignment,
cache residency, translation state, and manager delay while scoreboards remain
outstanding. A bounded drain occurs only after the window, preserving real
heterogeneous overlap without allowing unbounded pointer reuse.

### Translation Closure Phases

Translation coverage is closed in phases so a long green cacheable run cannot
mask an untested mode or a self-consistent reference-model bug:

| Phase | Required implementation | Exit criterion |
| --- | --- | --- |
| T0: mode contract | Enumerate legal `satp`, `vsatp`, and `hgatp` MODE values and reject unsupported values without changing the old context | CSR mode transition tests pass; unsupported writes do not create a new translation context |
| T1: independent walks | Parameterized Sv39/Sv48 and Sv39x4/Sv48x4 builders, canonical/high-bit checks, root alignment, all leaf levels, superpage alignment | Four-level 4-KiB builders/walks and all supported superpage leaf levels are exercised by `translation-superpages`; fault differential tests remain |
| T2: nested composition | Independent VS walk plus G walk for all four mode pairs, plus VS-only/G-only/Bare degenerations | Four-pair 4-KiB matrix has cold and warm PA checks, implicit page-table accesses, and no stage elision; Bare degenerations execute in `translation-bare` |
| T3: protection/faults | PTE V/R/W/X/U/G/A/D, PBMT/N/reserved bits, SUM/MXR, stage-specific access type, noncanonical VA, high-GPA overflow | Each invalid class produces the correct stage/cause/VA/GPA and no forbidden side effect |
| T4: context and fences | ASID/VMID reuse, root changes, `V` transitions, `SFENCE.VMA`, `HFENCE.VVMA`, `HFENCE.GVMA`, outstanding walks | Global/selective `SFENCE.VMA`, selective `HFENCE.VVMA`, and global `HFENCE.GVMA` leaf-update visibility are implemented; outstanding-walk ordering and full context isolation remain |
| T5: MemBlock stress | Mix all closed translation modes with LSQ wrap, split accesses, cache misses, redirect, and manager backpressure | Per-seed translation coverage and queue/progress gates remain green under long random runs |

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
| Virtual address classes | Sv39 canonical low/high halves, Sv48 canonical low/high halves, noncanonical bits, page-offset boundaries, aliasing VAs, VA wraparound | Partial; matrix covers low Sv39 and high-half Sv48, while noncanonical/wraparound cases remain |
| Physical address classes | cacheable, uncached, device, reserved, high PA bits, line/set/way aliases | Partial; cacheable and PBMT-NC plus a non-DebugModule SoC `c=0` PMA device interval and the guarded DebugModule denial are executable in `mmio-contracts`; reserved/high-PA/alias matrix remains |
| Stage-1 Sv39 walk | Bare/Sv39 mode, L2/L1/L0 leaves (1-GiB/2-MiB/4-KiB), invalid/non-leaf, misaligned superpage, permission/PBMT/A-D combinations | Partial; 4-KiB plus 2-MiB/1-GiB leaf execution is covered by `translation-superpages`, invalid root and malformed 2-MiB alignment faults by `translation-faults`, and read-only/execute-only read plus read-only scalar-store rejection by `translation-permissions`; non-leaf/PBMT/A-D matrix remains |
| Stage-1 Sv48 walk | Bare/Sv48 mode, L3/L2/L1/L0 leaves (512-GiB/1-GiB/2-MiB/4-KiB), L3 faults, canonicality, permission/PBMT/A-D combinations | Partial; four-level 4-KiB and all superpage leaf levels execute in `translation-superpages`, noncanonical and permission cases execute in `translation-faults`/`translation-permissions`; invalid/PBMT/A-D matrix remains |
| G-stage Sv39x4 walk | 16-KiB root, widened root index, 41-bit GPA, all leaf levels, high-GPA overflow, G-stage permissions | Partial; 4-KiB plus 2-MiB/1-GiB leaf execution is covered by `translation-superpages`, while high-GPA execution remains in `translation-faults`; permission faults remain boundary-limited |
| G-stage Sv48x4 walk | 16-KiB root, widened root index, 50-bit GPA, all leaf levels, high-GPA overflow, G-stage permissions | Partial; four-level 4-KiB and all superpage leaf levels execute in `translation-superpages`, high-GPA overflow in `translation-faults`, and execute-only data-read rejection in `translation-permissions`; full permission/invalid matrix remains |
| Nested mode matrix | `Sv39->Sv39x4`, `Sv39->Sv48x4`, `Sv48->Sv39x4`, `Sv48->Sv48x4`, plus `vsatp`/`hgatp` Bare degenerations | Partial; all four 4-KiB pairs are executable in `translation-matrix`, including cold/warm TLB reuse; VS-only/G-only/fully-Bare degenerations execute in `translation-bare` |
| Stage-only translation | HS/S/U stage-1 only, VS/VU stage-1 only, G-stage only for implicit page-table/HLV-class accesses | Partial; only current data-access paths are modeled |
| TLB behavior | cold miss, hit, refill, duplicate miss, replay, invalidation, `sfence.vma`, concurrent page walks | Partial |
| Page permissions | R/W/X/U/G, SUM/MXR at HS/VS stage, G-stage U-mode rule, read-only store, execute-only, access/dirty bit updates, privilege transitions | Partial; stage-1 and G-stage read-only loads plus execute-only data-read rejection and stage-1 read-only scalar-store rejection execute in `translation-permissions`; SUM/MXR/A-D/privilege matrix remains |
| Mode/context switching | `satp/vsatp/hgatp` root and MODE changes, ASID/VMID reuse, `V` transitions, same VA under distinct contexts | Partial; `translation-context` covers same-VA Sv39-to-Sv48 root/MODE switch; VS/G-stage context isolation and `V` transitions remain |
| Translation fences | `SFENCE.VMA`, `HFENCE.VVMA`, `HFENCE.GVMA`, selective/global scope and updates with outstanding traffic | Partial; global/selective `SFENCE.VMA`, selective `HFENCE.VVMA`, and global `HFENCE.GVMA` leaf-update invalidation are implemented by `translation-fence` |
| PMP/PMA | TOR/NA4/NAPOT, overlap priority, lock, M/R/W/X, cacheability, atomic/MMIO permissions, exact region edges | Planned |
| Fault classes | load/store/instruction access fault, stage-1 page fault, G-stage guest-page fault, noncanonical VA, high-GPA overflow, address-misaligned, access-denied, bus/ECC error | Partial; load/store/page/misaligned, noncanonical, high-GPA, and denied/corrupt D-channel cases are executable; PMP/PMA access-denied and physical ECC injection remain |
| Fault metadata | exact VA, GPA/PTE address, first failing level/stage, shifted `htval`-class value, guest marker, cause priority, single reporting and replay suppression | Partial; VS-non-leaf path implemented |

### Cache, memory-system, and coherence points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| DCache lookup | warm hit, cold miss, same-line merge, bank conflict, set pressure beyond associativity, synonym/alias | Partial; cold/warm and dirty set pressure implemented |
| Refill/replay | delayed A/D responses, beat reordering where legal, partial refill, killed request, replay after miss | Partial |
| Eviction | clean release, dirty ReleaseData, partial byte masks, replacement under pressure, release backpressure | Partial; immutable whole-line snapshot is checked for the dedicated dirty-pressure phase, while broader release/response classes remain planned |
| TileLink coherence | Probe/B/C/E traffic, source reuse, denied/corrupt/error responses, manager ordering | Partial; load and atomic denied/corrupt D responses are injected with backpressure and checked, while manager-originated probes, malformed responses, source-reuse stress, and E-channel behavior remain planned |
| Uncache/MMIO | Get/Put widths, byte enables, side effects, ordering, response delay, denied/error response | Partial; PBMT-NC Get widths/byte lanes and scalar denied/corrupt response propagation are executable (`uncache-widths`, `uncache-errors`); PBMT=IO's direct three-cycle load metadata bypass plus scalar store request/response/SQ-retirement, DCache non-use, denied/corrupt load metadata preservation, and a physical non-DebugModule `c=0` PMA load/store pair are executable (`mmio-contracts`); cacheable CBO.ZERO line-zero/readback is executable (`cbo-zero-contracts`); device side effects and malformed/duplicate/early/late responses remain |
| ECC/cache errors | correctable/uncorrectable data, error lifetime, retry or architectural exception | Partial; D-channel denied/corrupt metadata persistence and subsequent clean AtomicsUnit recovery are executable in `dcache-errors` and `atomic-dchannel-errors`; physical tag/data-array ECC injection and retry policy remain planned |
| PTW manager | request/response backpressure, source reuse, malformed/denied response, concurrent walks | Partial; legal backpressure and response identity are implemented, while malformed/denied injection and concurrent-walk stress remain |

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

## Interface Assumptions

| Interface family | Environment assumption | Enforced rule |
| --- | --- | --- |
| Issue | Legal operation encoding and LSQ/ROB pointer; payload held until `ready` | Typed drivers retain `valid` and payload through acceptance |
| LSQ dispatch/commit | Allocations and commits are in legal backend order | Drivers allocate the correct scalar/vector count and never commit unallocated work |
| Redirect | ROB pointer/flag and level describe a legal backend redirect | The scoreboard removes only architecturally younger work |
| DCache manager | Coherent 64-byte lines on a 256-bit bus; finite randomized delay | Agent drives/observes modeled A/C traffic and timing; independent E/response legality checks are planned |
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
- CBO/CMO line operations, `fence`, `fence.i`, and outstanding-traffic
  translation-fence ordering (global/selective `sfence.vma`, selective
  `hfence.vvma`, and global `hfence.gvma` leaf-update behavior are covered by
  `translation-fence`);
- VSegment/VFOF takeover, multi-uop segment streams, fault-only-first, and
  segment-specific redirect behavior;
- HLV, HLVX, HSV, SPVP, final physical execute permission, and hypervisor PMP;
- complete PMP/PMA TOR/NA4/NAPOT, lock, priority, cacheability, and region-edge
  matrices;
- VS/G-stage context isolation and `V` transitions (noncanonical and high-GPA
  fault execution is covered by `translation-faults`; the four-level 4-KiB
  builders, all four nested pairs, all Bare
  degenerations, and all supported superpage leaves are covered by
  `translation-matrix`/`translation-bare`/`translation-superpages`; a direct
  Sv39-to-Sv48 root/MODE switch is covered by `translation-context`);
- A/D updates, SUM/MXR combinations, invalidation races, and concurrent page
  walks;
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
3. A multi-seed, five-scenario matrix passes with backpressure enabled.
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
