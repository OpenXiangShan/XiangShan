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
| Floating load | Byte-addressed sparse memory plus FLH/FLW NaN-boxing and FLD bit preservation; translation, PMP, PBMT, and manager-error causes remain independent inputs | Exact FP destination/data on success with no integer-RF enable; exact memory exception and neither RF enable on fault; early faults issue no DCache/Uncache data request | Implemented for aligned and cacheable split FLH/FLW/FLD, PBMT=IO normal/error responses, and page/PMP/permission/guest-page/PBMT-misalignment faults in `fp-loads` |
| Scalar store | ISA store width/mask applied to an architectural reference separate from bus backing memory | Address/data writebacks compare exception/ROB/flush/debug metadata; committed bytes are recovered by an independent load. PBMT=IO stores must use Uncache, avoid DCache, complete the response/writeback sequence, and retire the SQ entry | Implemented for modeled scalar stores and scalar PBMT=IO contract |
| Vector load | Independent unit/strided/indexed address decoder, `vl`/`vstart`/mask rules, old destination, and legal `vma/vta` agnostic values | Exact active data and active-element mask for EEW 8/16/32/64; inactive data is constrained by RVV policy | Implemented for modeled 128-bit operations |
| Vector store | The same independent address/mask decoder applied to source bytes | Eventual completion/commit, exact vector readback of every active byte, RF write-enable/flush metadata, and optional trigger/debug metadata | Implemented for modeled 128-bit stores |
| Address translation | Mode-parameterized software walk for Bare/Sv39/Sv48 and Sv39x4/Sv48x4; independent canonicality, PTE validity, PPN physical/GPA-width overflow, leaf level, alignment, Svnapot VPN-low-bit substitution, permission, PBMT/N/reserved, and A/D checks | Accesses reach the independently calculated PA; invalid walks report the access-specific page/access/guest-page fault without a data-manager request | Partial: generic walker, Bare degenerations, all four 4-KiB nested paths, superpage leaves, all 16 subpages of legal 64-KiB Svnapot leaves in Sv39/Sv48 stage-1 and Sv39x4/Sv48x4 G-only modes plus all four nested mode pairs crossed with VS-only/G-only/both-stage NAPOT, each with load/store/readback, and schema-12 random traffic over distinct stage-1 and four nested leaf-topology regions are implemented. Sv39/Sv48 high-half canonical and both noncanonical sign-extension directions, physical PPN bits 48/55 at every ordinary stage-1/VS-only/G-stage PTE level, every-level VS-generated 41/50-bit GPA overflow across all four nested pairs, permission cases, the complete valid two-stage PBMT matrix, and 52 invalid/reserved/PBMT/NAPOT encodings are implemented for both scalar loads and stores; remaining permission/fault crosses remain |
| Nested translation | Independent VS-stage walk followed by independent G-stage walk for all four `vsatp` x `hgatp` mode pairs, including implicit page-table accesses, VS-generated GPA width, and VS-over-G PBMT priority | Exact host PA or stage-specific fault; no stage may be skipped or silently treated as Bare; final PMA/NC/IO class follows architectural composition | Partial: all four 4-KiB pairs, VS/G/Bare degenerations, every-level 41/50-bit VS-generated GPA overflow across all four non-Bare pairs and load/store, all 36 valid PBMT combinations, VS/G context switches, host/nested `V` transitions, same-ID root reuse with targeted fences, global/selective stale-response races, co-issued distinct-page walks, and redirected root/ASID/VMID/MODE/`V` changes with a delayed walk are covered; the common random tail weights and gates every mode pair, all enabled VS/G NAPOT topologies, and cold/reuse observations |
| Data-side PMP/PMA | Hand-calculated TOR/NAPOT regions and architectural first-match, permission, lock, current/effective privilege, platform-grain rules, and fixed SoC PMA entries | Exact allow/fault result and constrained manager behavior on denial; ordinary load/store/AMO denials emit no DCache/Uncache request, while the current HLVX R-allowed/X-denied pipeline exposes exactly one killed early DCache lookup and no Uncache/RF effect | Partial: `pmp-contracts` covers TOR/NAPOT edges, R/W/AMO denial, overlap priority, M-mode bypass, lock, and 4-KiB-grain WARL behavior. `hypervisor-contracts` crosses M-mode HLV/HLVX/HSV with SPVP=U/S and R/X/RW/RX plus locked R/RWX PMP regions, then maps all three families to a fixed `c=0` PMA device window for Uncache/execute-denial behavior; hypervisor region edges and broader PMA crosses remain |
| L2-to-L1 DTLB boundary | Drive all retained `io_l2_tlb_req_req_*` controls with the legal read command, including ordinary/prefetch, kill, and `no_translate`; sample the separately registered PMP output at its specified later cycle | Exact miss/hit valid and, only when `miss=0`, exact PA, PBMT, fault, PMP-deny, and PMA-MMIO fields. An ordinary cold miss starts a shared PTW refill, and the same-VA retry must hit without an additional external PTW TileLink A request | Partial: `l2-tlb-contracts` covers ordinary miss then exact cacheable no-fault refill hit, prefetch miss, kill/no-response, pruned-zero-address `no_translate`, PMP allow, locked 4-KiB PMP deny, and MMIO classification. Add positive PBMT NC/IO and stage-1 PF, nested GPF, and PTW AF hit payloads; external L2 retry policy remains integration-owned |
| IFU-to-Mem PTW bridge | Typed `PtwReq` driver over `io_fetch_to_mem_itlb_*` with an independent page-table image | Exact Sv39/Sv48 and nested sector response metadata; request acceptance and response payload stability under backpressure | Implemented: `ifetch-ptw-bridge` covers valid Sv39/Sv48, all four Sv39/Sv48 x Sv39x4/Sv48x4 4-KiB pairs, Sv39/Sv48 VS-only and G-only degenerations, PBMT=NC/IO leaves, invalid L0 leaves, stage-1 and G-stage `G` bits at leaf/non-leaf levels, a delayed IFU walk overlapped with a cold scalar DTLB walk, and two same-VPN IFU requests coalesced into one three-request Sv39 walk with two exact responses. Stage-1 `G=0` may not be promoted, while legal conservative demotion of `G=1` is counted; G-stage `G` may be exposed as raw internal metadata but cannot affect translation, permission, or fault results. Every nested pair is crossed with VS-leaf, final-G-leaf, and implicit VS-page-table G-stage faults. Delayed stage-1 and nested walks are invalidated by root/mode/ASID/VMID changes plus global and selective `SFENCE.VMA`, `HFENCE.VVMA`, and `HFENCE.GVMA`; each requires stale-response suppression and the exact replacement mapping. The active-stage ASID/VMID, permission, PPN, index, PBMT, fault level, fault bits, both leaf PTE addresses, and cross-source PTW outstanding depth are checked |
| L2 hint propagation | Valid/invalid `io_l2_hint`, all `sourceId` values, and `isKeyword` polarity at an idle/no-matching-MSHR boundary | Hint is registered and distributed without producing a ghost writeback, queue corruption, or protocol error; matching-MSHR replay semantics are integration-tested with L2 | Implemented for both keyword polarities and all 16 source IDs with an idle no-MSHR safety oracle in `l2-tlb-contracts`; matching-MSHR replay remains an L2 integration scenario |
| Frontend bridge | Independent request/response sequences for ICache, ICache-control, and instruction-Uncache, including the fields synthesized or narrowed by their diplomacy edges | Every accepted A request emerges once and in order with exact opcode/size/source/address/mask/data/user fields; every accepted D beat returns once with exact observable fields; a source is not reused before its response completes; payload remains stable under request and response stalls | Implemented in `frontend-bridge`: all 88 top-level frontend TileLink fields are explicitly driven or checked, the three paths run concurrently, source credits are enforced and observed, ICache uses two response beats, and the ICache-control path exercises Get/PutFull/PutPartial, sizes 1-8 bytes, all 32 sources, masks, and D-channel backpressure |
| Guest-fault metadata | Reference VS/G-stage walk from PTE addresses, including explicit data faults and implicit VS-page-table faults | Exact fault VA, faulting PTE GPA, shifted `htval`-class value where observable, and VS-non-leaf-PTE marker | Partial: current VA/GPA/marker cases are covered; broader fault classes are planned |
| Misalignment | Byte concatenation/splitting across 16-byte, line, and page boundaries | Exact value/bytes when enabled; specified address-misaligned exception when disallowed by memory type/control | Partial: common scalar/vector splits are covered; `scalar-misaligned` also holds three split loads at the LQ head, completes 60 younger loads to pressure RAR, and drains the splits in ROB order with exact data and no spurious violation |
| Exception side effects | RISC-V cause rules plus independent circular ROB/uop-age ordering, deliberately opposed to LQ/SQ allocation order | Exact exception bit; exceptional scalar load has no integer/FP RF write; software prefetch never raises a load exception or writes an RF; the exported exception VA belongs to the oldest coexisting fault | Partial: `exception-contracts` checks simultaneous scalar faults across ROB wrap with reversed LQ/SQ order, switches the retained load/store source for both scalar and vector load faults, makes an older `vuopIdx=0` vector fault replace a younger `vuopIdx=1` fault at the same ROB, chooses between co-issued page and PBMT-NC-misaligned faults by age, and replaces retained scalar/vector faults in both source directions; the broader cause/source matrix remains |
| Redirect | ROB age and redirect level supplied by a legal backend transaction | Redirected younger work has no terminal writeback; surviving work completes with the same data | Partial: basic redirect is covered; each known redirect samples the retained top-level LQ/SQ cancellation counts once after their two-cycle queue-output latency and the directed scalar case requires exactly one LQ cancellation. Broader producer-class arbitration remains |
| Cache coherence boundary | TileLink opcode/source/size/mask/data reference agent with separate bus and architectural memories | Stable producer payload while stalled, complete refill, sink-exact GrantAck, clean/dirty ProbeAck(Data), ReleaseAck, byte-exact dirty data, atomic refill/update, and denied/corrupt D-channel handling | Partial: `dcache-coherence` checks clean ProbeAck, requested clean ProbeAckData, mandatory dirty ProbeAckData, invalidation/refill, and E-channel GrantAck with forced backpressure. It also accepts two distinct Probe sources while an unrelated cold miss is outstanding and address-matches both responses. The common random tail generates byte-exact dirty toB/toN Probes and crosses requested/mandatory data; toB retention is followed by a checked toN cleanup. DCache denied/corrupt load errors and all 22 refill-capable W/D LR/AMO/AMOCAS error paths are executable. Malformed responses and broader source-reuse stress remain planned |
| PTW/uncache boundary | TileLink and uncache ready-valid agents with deterministic memory | Stable request/response while stalled, legal source/opcode/size/address/mask, ordered NC/MMIO store data, exact beat/lane load response, denied/corrupt error propagation, and SQ retirement | Partial: legal backpressure and response identity are executable. `ptw-errors` crosses denied/corrupt with Sv39/Sv48 stage-1, isolated Sv39x4/Sv48x4, fully nested, bitmap, load/store, and root/intermediate/leaf walks; corrupt is split across first/last beats, denied obeys the fixed-field/data-corrupt TileLink rules, and every fault is followed by a stage-appropriate fence plus a checked clean retry. Scalar Uncache width/lane and denied/corrupt propagation are executable in `uncache-widths`/`uncache-errors`; PBMT=IO direct MMIO load/store bypass, metadata/error path, SQ retirement, and an ordered side-effecting device log are executable in `mmio-contracts`. Malformed/duplicate/early/late response injection remains planned |
| Trigger and DynInst sidebands | Independent CSR trigger update plus explicit LSQ enqueue exception/trigger/flush and scalar issue RVC/FTQ/store-set/load-wait fields | Exact action/cause/`vstart` policy, target-manager suppression with legal-prefix accounting, store-image preservation, exception-vector mapping, and no dropped issue sideband | Partial: `trigger-contracts` covers all four memory-trigger slots, EQ/GE/LT hit/miss boundaries, both legal actions, enable/load/store/select/breakpoint/debug-state gates, a two-entry chain hit/miss pair, aligned/misaligned scalar stores, and eight vector load/store cases spanning unit/strided/indexed-unordered/indexed-ordered addressing, EEW 8/16/32/64, and two-field segment traffic. Scalar issue RVC/FTQ/store-set/load-wait paths and generated enqueue exception-vector mapping are also executable; the top-level issueLda boundary does not expose that vector. Broad randomized trigger/sideband crosses remain |
| Progress | Manager fairness: every observed request is eventually made ready and answered | Every non-canceled modeled operation terminates before the generous scenario deadline | Partial: LSQ enqueue transactions, dequeues, and redirect cancellation counts are independently observed. Exceptional and reset teardown that has no redirect remains explicitly classified as unobserved |
| Resource conservation | Accepted LSQ allocation, architectural commit, and redirect events | Allocated entries equal dequeued plus explicitly canceled entries at scenario end | Partial: all six LSQ dispatch lanes are passively sampled with independent LQ/SQ flow accounting and checked against DUT dequeue outputs. Each redirect samples the retained `lqCancelCnt/sqCancelCnt` values once; Schema 2 requires mixed-test cancellation conservation and zero unobserved cancellation |

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
| Vector loads | EEW/SEW 8/16/32/64; fractional/integer LMUL and legal derived EMUL; both lanes; unit, strided, indexed unordered/ordered; independently constrained `vm`, `vma`, `vta`, `vl`, and `vstart`; split windows | Exact 128-bit result, active mask, metadata, replay, LQ drain; schema-11 `random-mixed` gates every enabled shape/policy class, requires observable mask/tail agnostic elements, and expands complete 1..8-uop instructions |
| Vector stores | All legal EEW/SEW/LMUL/EMUL shapes and address modes; independently constrained mask, `vstart`, and partial `vl`; misaligned and cross-page split/replay | Exact active-byte readback, completion, commit, SQ drain; schema-11 policy/shape counts conserve against instructions and queue-capacity windows preserve full uop identity |
| Software prefetch | `prefetch.i/r/w`, all scalar issue lanes, mapped and unmapped VAs | Completion without RF write or exception; LQ drain |
| Hardware data prefetch | Fixed-PC cold loads with a 128-byte stride, weighted by the common `stride-stream` constraint and mixed with cache/TLB/manager pressure | Focused exact L2 target/source/confidence checks; random-mixed source-12 observation; no output after CSR disable; L3 idle in the current disabled configuration |
| Translation mode selection | Legal decoded `satp` Bare/Sv39/Sv48, `vsatp` Bare/Sv39/Sv48, and `hgatp` Bare/Sv39x4/Sv48x4 inputs | Selected mode is reflected after the required flush; no stale translation from the previous legal mode; architectural CSR WARL write/readback behavior is integration-level |
| Sv39 | 3-level walk; 4-KiB, 64-KiB Svnapot, 2-MiB, and 1-GiB leaves; low and high canonical VAs; both noncanonical sign-extension directions; PPN beyond 48-bit PA; cold/warm reuse | PA-derived data, exact page/access fault, PTW activity/reuse, leaf alignment; all 16 Svnapot subpages with load/store/readback, high-half canonical, both noncanonical directions, and lowest/highest overflowing PPN bits for load/store are executable |
| Sv48 | 4-level walk; 4-KiB, 64-KiB Svnapot, 2-MiB, 1-GiB, and 512-GiB leaves; L3 non-leaf and leaf faults; low and high canonical VAs; both noncanonical sign-extension directions; PPN beyond 48-bit PA | PA-derived data, exact page/access fault, fourth-level walk, canonicality fault; all 16 Svnapot subpages with load/store/readback, 4-KiB and superpage paths, high-half canonical, both noncanonical directions, and lowest/highest overflowing PPN bits for load/store are executable |
| G-stage Sv39x4 | 3-level walk with 16-KiB root and 41-bit GPA; 4-KiB/64-KiB Svnapot/2-MiB/1-GiB leaves; high-GPA and physical-PPN overflow | Host PA-derived data, guest-page fault, or original-access fault; exact GPA and root/index alignment; all 16 G-only Svnapot subpages with load/store/readback, final/VS-generated GPA overflow, and every-level physical-PPN overflow are covered |
| G-stage Sv48x4 | 4-level walk with 16-KiB root and 50-bit GPA; 4-KiB/64-KiB Svnapot/2-MiB/1-GiB/512-GiB leaves; high-GPA and physical-PPN overflow | Host PA-derived data, guest-page fault, or original-access fault; exact GPA and fourth-level walk; all 16 G-only Svnapot subpages with load/store/readback, 4-KiB, superpage, final/VS-generated GPA overflow, and every-level physical-PPN overflow paths are implemented |
| Nested translation | `Sv39 -> Sv39x4`, `Sv39 -> Sv48x4`, `Sv48 -> Sv39x4`, `Sv48 -> Sv48x4`; VS-only and G-only Bare degenerations; 64-KiB Svnapot at either/both stages; cold/warm reuse | Correct stage composition, stage-specific permissions/faults, exact VS VA and G-stage GPA metadata; Bare, every VS-only/G-only/both-stage Svnapot subpage with load/store/readback, VS-only all-level physical PPN overflow, final high-GPA, and every-level VS-generated GPA-width cases are executable |
| Translation faults | Invalid/non-leaf-at-level-0, W=1/R=0, invalid PTE, misaligned superpage, reserved/N/PBMT bits, PTE.PPN beyond the 48-bit PA width at every level, VS PTE-generated GPA beyond 41/50 bits, stage-1 vs G-stage access fault | First-fault level and cause are independent of DUT internals; no data or forbidden RF/store side effect; physical PPN overflow produces original load/store access faults, VS-generated GPA overflow produces guest-page faults with the exact non-leaf marker, and neither issues DCache/Uncache requests; scalar store against a read-only leaf must produce `StorePageFault` without a data request |
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
- unit-stride vector segment load/store traffic, all four EEWs, and NF 1..7,
  with interleaved field addressing and no LSQ allocation;
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
- HLV/HLVX/HSV traffic under nested translation, with independent family
  weights and per-seed family coverage.

There is one canonical mixed generator. Realistic traffic, balanced coverage,
and corner pressure are constraint sets over that generator, not independently
maintained scenario implementations. `--constraints coverage|spec|corner`
selects a baseline and repeatable `--constraint key=value` arguments override
operation mix, address locality, heterogeneous overlap, translation regime and
Sv39/Sv48/VS/G modes, context-switch and legal fence kind/scope rates,
misalignment, vector corner bias, vector-segment direction, atomic family/width,
hypervisor family, NC/MMIO direction, legal
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
| T1: independent walks | Parameterized Sv39/Sv48 and Sv39x4/Sv48x4 builders, canonical/high-bit checks, root alignment, all leaf levels, Svnapot, and superpage alignment | Four-level 4-KiB builders/walks, all supported superpage leaf levels, all 16 legal 64-KiB Svnapot subpages in stage-1 and G-only modes, valid high halves, both noncanonical sign-extension directions, and G-stage high-bit overflow are executable; broader boundary crosses remain |
| T2: nested composition | Independent VS walk plus G walk for all four mode pairs, VS/G Svnapot combinations, plus VS-only/G-only/Bare degenerations | Four-pair 4-KiB matrix has cold and warm PA checks, implicit page-table accesses, and no stage elision; all four pairs also cross every subpage of VS-only/G-only/both-stage 64-KiB Svnapot with isolated 4-KiB page-table mappings; Bare degenerations execute in `translation-bare` |
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
| Integer loads | `lb/lbu/lh/lhu/lw/lwu/ld`, all destination classes, all issue lanes, zero/sign-extension patterns, signed `src + imm` address generation | Implemented; directed loads cross immediate 0, 1, 2047, -1, and -2048, including cache-line boundaries, while random scenarios retain zero as the dominant realistic case |
| Integer stores | `sb/sh/sw/sd`, address-first/data-first, byte masks, all issue lanes, commit timing, signed `src + imm` address generation | Partial; issue/commit and modeled readback are covered, and store forwarding crosses immediate 0, 1, 2047, -1, and -2048, but the boundary exposes no store payload monitor |
| Floating loads/stores | FLH/FLW/FLD and narrow/widening formats, NaN-boxing, memory exceptions, integer/FP destination separation | Implemented for the MemBlock-observable load boundary: `fp-loads` checks cacheable and PBMT=IO FLH/FLW/FLD destination class and exact payload/NaN-boxing, cacheable cross-line/page misalignment, and page, PMP, permission, guest-page, PBMT=NC misalignment, denied, and corrupt exception write suppression. Store-data formatting is upstream because `issueStd.src(0)` already carries formatted raw bits |
| Vector unit stride | EEW 8/16/32/64, SEW 8/16/32/64, LMUL fractional 1/8 through 8, legal derived EMUL, `vl=0..VLMAX`, `vstart` at start/middle/end, `vm`, mask holes, `vma/vta` | Implemented for modeled operations; `vector-addressing` exhausts all 78 legal ELEN=64 EEW/SEW/LMUL/EMUL configurations with 404 load/readback and 202 store uops, independent `vsew/veew/vlmul` checks, exact data/readback, conservative two-flow unit-stride LSQ allocation, queue wrap, and ROB wrap. Schema-11 `random-mixed` exposes the same shape and execution-policy dimensions as weights and checks complete multi-uop instructions under mixed translation/cache traffic |
| Vector strided | All 78 legal ELEN=64 EEW/SEW/LMUL/EMUL configurations; positive, zero, and negative legal load strides; non-overlapping positive and negative store strides; element gaps and split windows | Implemented for modeled operations: `vector-addressing` alternates positive/negative strides across the full legal matrix, checks 404 load/readback and 202 store uops with exact data and metadata, streams the largest instructions across conservative queue windows, and drains 2,304 LQ/1,152 SQ entries. Schema-11 `random-mixed` uses the same flow calculation with randomized signs and optional zero-stride loads. Overlapping stores remain excluded because their final memory value is not a single deterministic oracle |
| Vector indexed unordered | Repeated indices, aliasing, non-monotonic indices, all EEWs, masked elements | Implemented for modeled operations: all 78 legal ELEN=64 EEW/SEW/LMUL/EMUL configurations execute exact load/store/readback with reverse uop issue, including index-register reuse when LMUL exceeds EMUL and 28 `EMUL>LMUL` shared-Vd configurations. Schema-11 `random-mixed` generates complete unique-offset index groups, randomly reverses uop issue, and conserves 1..8-uop shapes. Directed cases retain repeated-load aliases, cache-line/Bare-page crossings, and mask/`vstart` filtering |
| Vector indexed ordered | Strict element order, repeated/aliasing indices, split beats/pages | Partial only at the system-side ordering boundary: all 78 legal ELEN=64 EEW/SEW/LMUL/EMUL configurations execute exact in-order load/store/readback, including index-register reuse and 28 `EMUL>LMUL` shared-Vd configurations. Schema-11 `random-mixed` adds weighted complete shapes under ordinary mixed traffic. Directed cases retain repeated/non-monotonic indices and split boundaries. Architecturally observable ordering against a side-effecting translated target remains |
| Vector segmented/whole-register | NF/segment count, multi-uop streams, load/store direction, EEW, mask/`vstart`, fault-only-first, and partial completion | Partial: `vector-segment` checks lane-0 takeover, interleaved load/store data, multi-uop identity, zero LSQ allocation, and all addressing modes. Every addressing mode exhausts 338 legal NF 2..8 x EEW/SEW/LMUL/EMUL configurations per mode with exact load/store/readback; strided cases cover positive/negative strides, while indexed cases cover the complete index group and 252 index-only uops required by 32 `EMUL>LMUL*NF` configurations across both modes. `vector-segment-fof` covers first/later faults; `trigger-contracts` covers field-1 indexed and strided-store breakpoints; `vector-addressing` exhausts all ordinary addressing matrices plus all 16 whole-register NF/EEW combinations. Schema-9-and-later `random-mixed` exposes addressing, EEW, SEW, LMUL, EMUL, NF, and direction weights through one interface and independently gates every enabled class. Full-core `waitForward`/`blockBackward` serialization makes an active segment plus redirect unreachable, so that combination is an upstream assumption rather than a MemBlock stimulus. Overlapping indexed stores and ordered access to a side-effecting translated target remain |
| Vector data patterns | all zero/one, ramps, alternating bits, random bytes, same-byte aliases, old-destination merge | Implemented/partial by operation class |
| Software prefetch | `prefetch.i/r/w`, mapped/unmapped, cacheable/NC/IO, all lanes, signed `src + imm`, duplicate and outstanding requests | Partial: `ifetch-prefetch` checks every class/lane, all signed-12-bit boundary classes through 0/1/2047/-1/-2048, a same-cycle `i/r/w` batch, unmapped cold data-hint drop without PTW/data traffic, warmed Sv39 cacheable data-prefetch hits, individual `r/w` DCache requests, and the intentional PBMT policy: resident-TLB NC `r/w` issue DCache hints without Uncache while IO `r/w` reach neither manager; broader duplicate/same-line contention remains |
| Atomics | LR/SC, AMOADD/XOR/AND/OR/SWAP/MIN/MAX and signed/unsigned variants, AMOCAS, reservation loss, alignment | Partial; all exposed W/D-width AMO variants, AMOCAS.W/D compare success/failure, LR/SC success/failure, all 24 W/D opcodes crossed with every illegal byte offset (120 cases), and a fixed-device-PMA `AMOADD.D` denial execute in `atomic-contracts`. The alignment matrix checks LR load-misaligned versus all other store-misaligned exception classes, suppressed RF write, zero new DCache traffic, and ROB pointer wrap; the PMA fault suppresses RF write and all manager/memory side effects. `atomic-dchannel-errors` crosses denied/corrupt with all 22 refill-capable W/D LR/AMO/AMOCAS operations, checks initial exception/RF contracts, later poisoned-line load hits, SC.W/D hits on denied/corrupt metadata, exact request counts, and clean AMO recovery. The SC checks do not claim internal reservation observability. SC cannot have a cold-miss D response because MainPipe returns failure before a request when the line or usable reservation is absent. Cross-hart reservation interference and ordering with concurrent traffic remain |
| CBO/CMO/fences | clean/invalidate/flush/zero, `fence`, `fence.i`, `sfence.vma`, ordering with outstanding traffic | Partial; cacheable `CBO.ZERO` StoreQueue/SBuffer line-zero and readback are executable (`cbo-zero-contracts`), and global `SFENCE.VMA` leaf-update behavior is implemented; CMO CLEAN/FLUSH/INVAL, `fence.i`, and full ordering remain because `cmoOpResp` is internal to DCache rather than a MemBlock top-level port |
| Hypervisor memory ops | HLV/HLVX/HSV, effective privilege/SPVP, VSUM/VMXR, execute permission, guest/host faults | Partial: `hypervisor-contracts` executes all exposed HLV/HLVX/HSV encodings, SPVP user/supervisor cases, VSUM/VMXR permission changes, VS- and G-stage faults, HLVX execute-only access, representative operations under all four Sv39/Sv48 and Sv39x4/Sv48x4 pairs, a five-combination PBMT basis, and cacheable misaligned split paths. Eighteen M-mode cases cross all three families and SPVP=U/S with R/X/RW/RX plus locked R/RWX physical PMP regions, requiring R, R+X, and W; three more target the fixed `c=0` PMA device interval and check HLV/HSV Uncache plus HLVX access fault. Schema-7+ `random-mixed` weights all three families. Broader PMA and hypervisor PMP region-edge crosses remain |

### Address, translation, and protection points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| Alignment | every byte offset for widths 1/2/4/8/16, beat boundary, 64-byte line, 4 KiB page, two-page and two-line splits | Partial; common scalar/vector split classes implemented |
| Virtual address classes | Sv39 canonical low/high halves, Sv48 canonical low/high halves, noncanonical bits, page-offset boundaries, aliasing VAs, VA wraparound | Partial; low/high canonical halves and both noncanonical sign-extension mismatch directions execute for Sv39 and Sv48; broader page-offset, aliasing, and wraparound crosses remain |
| Physical address classes | cacheable, uncached, device, reserved, high PA bits, line/set/way aliases | Partial; cacheable and PBMT-NC plus a non-DebugModule SoC `c=0` PMA device interval, guarded DebugModule denial, exact device-to-DDR manager selection on both sides of `0x80000000`, and read-clear/partial-write device semantics are executable in `mmio-contracts`; reserved/high-PA/alias matrix remains |
| Stage-1 Sv39 walk | Bare/Sv39 mode, L2/L1/L0 leaves (1-GiB/2-MiB/4-KiB plus 64-KiB Svnapot), invalid/non-leaf, misaligned superpage, canonicality, permission/PBMT/A-D combinations | Partial; all leaf levels, all 16 legal Svnapot subpages with load/store/readback, high-half canonical VA, both noncanonical sign-extension directions, malformed superpage, physical-PPN overflow at L2/L1/L0, U/S, SUM, MXR, missing-A/D, and 13 invalid/reserved/PBMT/NAPOT encoding cases execute for loads and stores; broader crosses remain |
| Stage-1 Sv48 walk | Bare/Sv48 mode, L3/L2/L1/L0 leaves (512-GiB/1-GiB/2-MiB/4-KiB plus 64-KiB Svnapot), L3 faults, canonicality, permission/PBMT/A-D combinations | Partial; all leaf levels, all 16 legal Svnapot subpages with load/store/readback, high-half canonical VA, both noncanonical sign-extension directions, physical-PPN overflow at L3/L2/L1/L0, U/S, SUM, MXR, missing-A/D, and the same 13 encoding cases execute for loads and stores; broader crosses remain |
| G-stage Sv39x4 walk | 16-KiB root, widened root index, 41-bit GPA, all leaf levels including 64-KiB Svnapot, high-GPA overflow, G-stage permissions | Partial; all leaf levels, all 16 legal G-only Svnapot subpages with load/store/readback, final and every-level VS-generated GPA overflow, physical-PPN overflow at L2/L1/L0, load/store permission faults including `D=0`, all valid two-stage PBMT combinations, and 13 invalid/reserved/PBMT/NAPOT encoding cases execute for loads and stores |
| G-stage Sv48x4 walk | 16-KiB root, widened root index, 50-bit GPA, all leaf levels including 64-KiB Svnapot, high-GPA overflow, G-stage permissions | Partial; all leaf levels, all 16 legal G-only Svnapot subpages with load/store/readback, final and every-level VS-generated GPA overflow, physical-PPN overflow at L3/L2/L1/L0, load/store permission faults including `D=0`, all valid two-stage PBMT combinations, and the same 13 encoding cases execute for loads and stores |
| Nested mode matrix | `Sv39->Sv39x4`, `Sv39->Sv48x4`, `Sv48->Sv39x4`, `Sv48->Sv48x4`, plus `vsatp`/`hgatp` Bare degenerations and VS/G Svnapot placement | Partial; all four 4-KiB pairs are executable in `translation-matrix`, including cold/warm TLB reuse; `translation-superpages` crosses all four pairs with VS-only, G-only, and simultaneous VS/G NAPOT across every 4-KiB subpage; VS-only/G-only/fully-Bare degenerations execute in `translation-bare`; the same four non-Bare pairs are independently weighted and required in `random-mixed` |
| Stage-only translation | HS/S/U stage-1 only, VS/VU stage-1 only, G-stage only for implicit page-table/HLV-class accesses | Partial; only current data-access paths are modeled |
| TLB behavior | cold miss, hit, refill, duplicate miss, replay, invalidation, `sfence.vma`, concurrent page walks | Partial; `translation-fence-all` holds stale stage-1, VS-only, G-only, and fully nested VS/G leaf responses across global and selective fences for both supported modes and all four nested pairs, rejects canceled writeback, requires new-data refills, and co-issues two distinct-page stage-1 plus nested walks before the first delayed PTW response; `random-mixed` requires observed cold-walk and warm-reuse windows plus every enabled legal fence cross; broader duplicate-miss source-reuse stress remains |
| Page permissions | R/W/X/U/G, SUM/MXR at HS/VS stage, G-stage U-mode rule, read-only store, execute-only, access/dirty bit updates, privilege transitions | Partial; `translation-permissions` executes 106 cases: 16 stage-1 loads, 17 stage-1 stores, 33 two-stage loads, and 40 two-stage stores. Sv39/Sv48 U/S, SUM, MXR, A/D, VSUM/VMXR, and G-stage load/store R/A/D/U variants are checked with every nested permission variant crossed over all four VS/G mode pairs, plus exact readback, fault cause, manager non-use, and SQ conservation; `translation-faults` covers PBMT/reserved encoding faults, while `translation-pbmt` covers valid PBMT composition |
| Mode/context switching | `satp/vsatp/hgatp` root and MODE changes, ASID/VMID reuse, `V` transitions, same VA under distinct contexts | Implemented for the modeled modes; `translation-context` covers five drained context families and 14 distinct-data same-address accesses; `translation-fence` adds same-ID host-ASID/VS-ASID/VMID fenced root reuse; `translation-inflight-context-all` holds an old PTW response for 256 cycles, changes root/ID/MODE or `V`, redirects the old load, reuses its ROB/LQ identity, and requires the new PA for both host mode directions plus all four nested starting pairs and both host/nested directions |
| Translation fences | `SFENCE.VMA`, `HFENCE.VVMA`, `HFENCE.GVMA`, selective/global scope and updates with outstanding traffic | Partial; global/selective leaf updates, targeted same-ID host-ASID/VS-ASID/VMID root reuse, stale-response races, and co-issued distinct-page stage-1/nested walks spanning both stage-1 modes, both G-stage modes, and all four fully nested pairs are implemented by `translation-fence-all`; the separate context-race matrix checks redirected root/ID changes while a response remains outstanding |
| PMP/PMA | TOR/NA4/NAPOT, overlap priority, lock, M/R/W/X, cacheability, atomic/MMIO permissions, exact region edges | Partial; `pmp-contracts` programs the distributed PMP CSR boundary and checks TOR/NAPOT exact edges, R/W plus AMO denial, overlap priority, M-mode bypass, lock enforcement, and lock immutability. `PlatformGrain=12` makes NA4 unselectable and coerces `A=2` to a minimum 4-KiB NAPOT region. HLV/HLVX/HSV check SPVP-selected physical R/R+X/W permissions under M-mode for unlocked and locked regions plus a fixed SoC PMA device mapping; DebugModule access, the fixed device-to-DDR boundary, and device-PMA AMO denial are also focused. Broader fixed-PMA class/overlap/edge matrices and hypervisor PMP region edges remain |
| Fault classes | load/store/instruction access fault, stage-1 page fault, G-stage guest-page fault, noncanonical VA, high-GPA overflow, address-misaligned, access-denied, bus/ECC error | Partial; load/store/page/misaligned, noncanonical, high-GPA, PMP access-denied, denied/corrupt D-channel, one-shot physical DCache tag single/double-bit plus all-bank data single/double-bit ECC, and same-cycle injected/clean load isolation are executable; instruction-side PMP and broader ECC timing/concurrency crosses remain |
| Fault metadata | exact VA, GPA/PTE address, first failing level/stage, shifted `htval`-class value, guest marker, cause priority, single reporting and replay suppression | Partial; VS-non-leaf metadata, concurrent scalar load/store oldest-address selection across ROB wrap, same-ROB vector `uopIdx` replacement, one simultaneous page-fault/misaligned pair, bidirectional scalar/vector replacement, and vector-load/store buffer selection under both arrival orders are implemented; the broader cross-cause/source matrix remains |

### Cache, memory-system, and coherence points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| Frontend pass-through | ICache line reads, instruction-Uncache reads, ICache-control Get/PutFull/PutPartial, source/size/mask/data, simultaneous paths, A/D backpressure | Implemented at the MemBlock top boundary by `frontend-bridge`; downstream ICache/device semantics remain integration responsibilities |
| DCache lookup | warm hit, cold miss, same-line merge, bank conflict, set pressure beyond associativity, synonym/alias | Partial; cold/warm, same-line merge under a 128-cycle delayed refill, bank conflict, and dirty set pressure are executable; broader synonym/alias crosses remain |
| Refill/replay | delayed A/D responses, critical-beat-first ordering, partial refill, killed request, replay after miss | Partial; `single-load` forces both `isKeyword` polarities and corresponding two-beat GrantData orders, checks exact data, merges two same-line loads into one delayed refill, and proves a keyword load writes back from its critical beat while the noncritical beat is held for 128 cycles without a later duplicate. Delayed and killed refills are exercised elsewhere; broader replay/response crosses remain |
| Eviction | clean release, dirty ReleaseData, partial byte masks, replacement under pressure, release backpressure | Partial; immutable whole-line snapshot is checked for the dedicated dirty-pressure phase, while broader release/response classes remain planned |
| TileLink coherence | Probe/B/C/E traffic, source reuse, denied/corrupt/error responses, manager ordering | Partial; `dcache-coherence` executes three manager Probes spanning clean no-data, clean requested-data, and dirty mandatory-data responses, checks byte-exact C beats, forces E backpressure, and matches every GrantAck sink. It separately accepts two distinct Probe sources before an unrelated delayed cold miss completes and checks outstanding depth and response addresses. `random-mixed` adds constrained dirty toB/toN Probes with requested/mandatory data and checked toB cleanup. Load and atomic denied/corrupt D responses are also injected and checked; malformed responses and broader source-reuse stress remain planned |
| Uncache/MMIO | Get/Put widths, byte enables, side effects, ordering, response delay, denied/error response | Partial; PBMT-NC Get widths/byte lanes and scalar denied/independent-corrupt load propagation plus denied-store propagation are executable (`uncache-widths`, `uncache-errors`), and the denied store must preserve initialized backing bytes; PBMT=IO's direct three-cycle load metadata bypass plus scalar store request/response/SQ-retirement, DCache non-use, denied/independent-corrupt load metadata preservation, a physical non-DebugModule `c=0` PMA load/store pair, device-to-DDR edge loads, and a seven-request read-clear/partial-write device sequence with load-error and denied-store side-effect suppression plus an exact ordered log are executable (`mmio-contracts`). Corrupt is forbidden on data-less store `AccessAck`. Three same-address device loads queued before the first request prove external serialization under 512/128-cycle delays despite enabled Uncache outstanding mode. A separate pre-issued `load -> SW -> load` sequence crosses LQ/SQ and two 256-cycle response delays, requiring external depth one plus exact read-clear/write/readback state transitions. Cacheable CBO.ZERO line-zero/readback is executable (`cbo-zero-contracts`); malformed/duplicate/early/late responses remain |
| ECC/cache errors | correctable/uncorrectable data, error lifetime, retry or architectural exception | Partial; D-channel denied/corrupt metadata persistence and subsequent clean AtomicsUnit recovery are executable in `dcache-errors` and `atomic-dchannel-errors`. `dcache-errors` also uses `L1DCacheCtrl` one-shot injection on proven-resident lines. The RTL tag pseudo-error path selects bank 0, where representative single- and double-bit masks must cancel, redirect, suppress terminal writeback, report the exact physical address to BEU, and recover on a clean survivor. Data ECC crosses all eight banks with spread single-bit and adjacent double-bit masks; in default `EnableAccurateLoadError=false` mode every case must report BEU, return the exact XOR-corrupted value without cancellation, and recover on a clean survivor. A same-cycle selected-bank/clean-bank pair checks lane isolation and automatic one-shot clear without a disable write. Broader masks, delayed/persistent injection, and wider mixed-traffic concurrency crosses remain |
| PTW manager | request/response backpressure, source reuse, malformed/denied response, concurrent walks | Partial; legal backpressure, response identity, and denied/corrupt error propagation are implemented. `ptw-errors` covers 16 stage-1/G-stage/nested/bitmap load/store cases, exact walk cutoff, access-fault writeback, cache-refill suppression, and zero data-manager side effects. Its eight corrupt cases split 4/4 across the first/last data beat; every error case then fences the owning translation stage and requires a clean same-address recovery with an exact failed-block reread and one DCache access. Two co-issued distinct-page DTLB walks remain pending around a delayed response even where that scenario observes manager depth one; the separate IFU-plus-DTLB overlap reaches manager depth two. Two same-VPN IFU requests return twice while coalescing into one three-request Sv39 memory walk. Malformed responses and broader source-reuse stress remain |

### Queue, ordering, and control points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| LQ/SQ occupancy | empty/near-full/full, wrap flags, simultaneous enqueue/dequeue, same-slot reuse | Implemented/partial |
| ROB age | ordinary and wrapped pointers, flag transitions, same-cycle issue/commit/redirect | Partial |
| Store-to-load forwarding | scalar-scalar, vector-vector, scalar-vector, vector-scalar, partial overlap, byte masks, older/younger stores | Implemented for modeled classes |
| Exception priority | multiple legal faults, differing ROB/uop vs LQ/SQ age, load/store/vector competition | Partial; wrapped ROB age deliberately disagrees with LQ/SQ order for simultaneous scalar load/store faults, the retained load/store selector is switched while both buffers hold scalar or vector load exceptions in both source-arrival orders, a same-ROB LMUL=2 vector pair checks `uopIdx`, page and PBMT-NC-misaligned faults are co-issued, and scalar/vector page faults replace each other in both arrival directions. Additional cause pairs and same-operation multi-cause priority remain planned |
| Redirect/recovery | kill each producer class, in-flight miss/replay, canceled prefetch, pointer reuse, survivor data | Implemented for basic scalar/vector LSQ redirect. Direct active-VSegment redirect is excluded because full-core serialization prevents that state; broader simultaneous producer-class arbitration remains |
| Fence/commit ordering | outstanding cache/uncache/PTW traffic across commit and fence boundaries | Partial; `sbuffer-flush` commits a cacheable store while a PBMT-NC store response is delayed 1,024 cycles, pulses the direct SBuffer flush, requires both paths to drain, and checks exact readback. `translation-fence-all` holds targeted PTW responses across global/selective stage-appropriate fences and rejects stale completion. Full architectural `fence`, `fence.i`, CMO ordering, and wider simultaneous manager crosses remain |
| Backpressure | every producer/consumer ready-low pattern, long stalls, alternating stalls, response delay cross-product | Implemented for DCache/PTW/uncache and Probe responses; negative-error timing crosses remain planned |
| Reset/quiescence | reset asserted/deasserted at legal boundaries, idle cycles, reset with outstanding traffic, repeated reset | Partial; `reset-tree-contracts` checks three repeated functional-reset pulse widths, asynchronous assertion, exact six-cycle synchronized release through two ResetGen levels, DFT functional-mode isolation and three-cycle release, plus the combinational scan override at `io_reset_backend`. `reset-recovery` independently resets accepted DCache-refill, PTW-walk, and Uncache/MMIO traffic under a 256-cycle response delay, while `frontend-reset-recovery` clears stalled requests on all three frontend bridges and one observably backpressured ICache-control response before distinct survivor requests. Reset during additional LSQ producer/replay classes remains planned |

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
| Frontend ICache, ICache-control, and instruction-Uncache TileLink (88 ports) | Implemented | `frontend-bridge` keeps independent request/response scoreboards and source credits for all three paths; `frontend-reset-recovery` checks reset cancellation and distinct post-reset requests for all three A paths plus the only observably backpressured D path |
| DCache A/B/C/D/E TileLink (53 ports) | Implemented | A/C/D refill, error, eviction, and ReleaseData paths plus deterministic B Probe and E GrantAck are checked. `dcache-coherence` accepts two distinct B source IDs before an unrelated cold miss writes back, requires accepted Probe depth >= 2, and address-matches both C responses; C-source remains an independent WritebackQueue ID. `random-mixed` constrains dirty toB/toN Probe sequences and requested/mandatory data |
| PTW and Uncache manager boundaries (41 ports) | Partial | Legal traffic, long latency, and backpressure are implemented. PTW denied/corrupt responses cover every translation regime, both data beats, fault cutoff, cache suppression, and post-fence recovery. Denied/corrupt Uncache stores require exact exception metadata plus one external error report at the aligned physical line address. Add malformed/duplicate/early/late responses as explicit negative protocol modes rather than normal workload traffic |
| IFU-to-Mem PTW request/response (`io_fetch_to_mem_itlb_*`, 64 ports) | Implemented | `ifetch-ptw-bridge` drives raw IFU PTW requests for valid/faulting Sv39/Sv48, all four nested pairs, both VS-only/G-only mode variants, PBMT=NC/IO, stage-1 and G-stage leaf/non-leaf `G` inputs, and all four nested pairs crossed with VS-leaf, final-G-leaf, and implicit page-table G-stage faults while forcing response backpressure and requiring cold page-table traffic. `G=0` cannot be promoted; stage-1 global mappings may be conservatively demoted, and G-stage `G` cannot change the functional result. A 256-cycle IFU root-PTE delay overlaps a cold scalar DTLB walk and requires both translations, exact load writeback, and PTW outstanding depth >= 2. Two same-VPN IFU requests are accepted before the first response and must return twice while issuing only one three-level Sv39 memory walk. Delayed requests are invalidated by context switches and global/selective stage-appropriate fences; no stale response may escape, and the subsequent walk must return the independently computed replacement mapping |
| Backend `memoryViolation` (8), `ldCancel` (3), and wakeup (12) outputs | Partial, high priority | `load-feedback` checks lane/destination metadata and wakeup/cancel conservation for cold, resident, same-bank, page-fault, PMP-denied, MMIO, NC, and forwarding-data-wait loads. Page and PMP faults leave no uncanceled wakeup or data-manager request; MMIO/NC retain one final wakeup through Uncache. DCache denied/corrupt, G-stage guest faults, and guarded fixed-PMA denial apply the same exceptional oracle in their owning scenarios. `dcache-errors` adds physical tag ECC wakeup/cancel conservation and the default non-accurate data-ECC uncanceled-writeback contract, both with exact-address BEU reports. `random-mixed` gates canceled/uncanceled observations on every lane and freezes the architectural gate before hardware-prefetch training. `memory-violation` and `rar-violation` check all eight redirect fields, no-overlap controls, ROB wrap, vector participation, and concurrent cross-source oldest selection. `pmp-contracts` additionally checks terminal load/store/AMO access faults |
| ROB/LSQ pending boundary (`pendingMMIOld`, `pendingst`, `pendingPtr`) | Implemented for elaborated fields | MMIO retirement holds `pendingMMIOld` with the matching ROB pointer until Uncache completion; replaying misaligned stores hold `pendingst` and the same pointer across retries. `pendingld`, `pendingVst`, and `pendingPtrNext` are source-level bundle members but are pruned from this generated MemBlock top, so no functional claim is made for nonexistent pins |
| WFI request/safe boundary (2 ports) | Implemented | `wfi-safety` requires `wfiSafe=0` while independently delayed DCache, PTW, or Uncache manager work is outstanding, then requires assertion only after each manager drains; deasserting `wfiReq` clears the safe indication |
| Top-down miss/pressure boundary (8 ports) | Implemented | `topdown-contracts` checks exact one-cycle L2/L3 input propagation, L1 miss and replay allocation during a delayed cold load, StoreQueue full after 56 accepted unissued stores, and SBuffer full after 16 distinct committed lines are held behind a delayed refill. The monitor remains active in every functional scenario |
| Direct maintenance/configuration controls | Partial | Focused tests cover direct SBuffer flush, timeout-driven eviction, Uncache outstanding enable/disable behavior, MBMC BME/CMODE/BCLEAR/BMA bitmap policy, architectural prefetch-control defaults, guarded DebugModule PMA access, all outer-L2 flush enable/completion combinations, and the simple top-level control/metadata bridges. L2 enable, hart ID, power-down, MSI ack, and frontend reset are checked combinationally; L2 completion, reset vector, halt, critical error, all seven external-interrupt outputs, MSI-info, CLINT, I-cache BEU, and 67 shared perf-event lanes are checked at exact one-cycle latency; all six L2-prefetch-control fields are checked at two cycles. These oracles run on every monitored functional-test cycle. Dynamic reprogramming during arbitrary overlapping traffic and the remaining CSR cross-product remain |
| IFU software instruction-prefetch outputs (`io_ifetchPrefetch_*`, 6 ports) | Implemented | `ifetch-prefetch` checks lane and virtual address for all three LoadUnit outputs, rejects data-prefetch false positives, and issues three unmapped requests together under active Sv39 while requiring exact per-lane VAs and zero PTW/DCache traffic. A separate same-cycle `prefetch.i/r/w` cross proves only the instruction operation emits this output while cold data hints drop on DTLB miss and warmed cacheable data hints reach DCache. Resident-TLB PBMT=NC data hints intentionally use DCache rather than Uncache, while PBMT=IO hints reach neither manager; neither class emits an IFU pulse. This matches LoadUnit's explicit `s0_tlb_no_query`; frontend owns subsequent instruction-prefetch translation/faults. `random-mixed` requires an observed `prefetch.i` output in every seed |
| L2/L3 prefetch sender outputs (5 ports) | Partial | `hardware-prefetch` checks fixed-PC stride confidence, exact source-12 L2 address/count, PC-independent 12-line spatial-stream activation, exact source-11 640-line lookahead/four-line width, stream-over-stride priority, CSR-disable suppression, and source-10 SMS/PHT causality: twelve same-PC training lines with output disabled produce exactly six unique requests at learned offsets 5..10 after enable and a new-region trigger. `random-mixed` adds weighted fixed-PC stride streams under ordinary/corner miss, refill, translation, Probe, and latency pressure with a per-seed L2 source-12 gate. Exact random-stream address attribution and a positive L3-enabled configuration remain; direct SMS AGT generation is hard-disabled in this RTL |
| Store/vector IQ slow feedback | Implemented | `iq-slow-feedback` records every valid pulse and all exposed fields; checks both STA lanes returning same-cycle cold misses and warm hits with exact SQ identities, both VSTU lanes returning same-cycle hits with exact LQ/SQ and inactive replay metadata, and a strided misaligned vector-store partial replay with exact queue identity, nonzero replay mask, and merge-buffer index |
| Trace interface bypass (45 ports) | Implemented for transport | `trace-bridge-contracts` checks one-cycle encoder feedback, group valid/type/retire-count and `mstatus`; valid-gated address/last-size/privilege holds; exact 50-bit address-plus-FTQ-offset arithmetic; and trap cause/tval updates only for group-0 Exception/Interrupt. All group-valid combinations, itypes, FTQ offsets, privileges, and encoder states execute. Architectural trace-content generation remains owned by backend/trace integration |
| DFT bridge transport (24 ports) | Implemented for routing and observable backend reset | `dft-bridge-contracts` holds external functional reset and exhausts all 1,024 combinations of seven SRAM-broadcast plus three DFT-reset inputs. Ten frontend and four backend outputs match combinationally before idle restoration and a fresh reset. `reset-tree-contracts` additionally checks DFT functional/scan effects at `io_reset_backend`; MBIST and physical SRAM behavior remain integration-owned |
| Performance-event meaning | Integration-owned | `top-control-contracts` proves interrupt and 67-lane performance-event transport but not event meaning. Test performance semantics at the owning integration boundary instead of claiming them from MemBlock data-path tests |

The development-phase ordering is breadth first: close executable `planned`
and `partial` rows with short directed and modest constrained-random runs,
then restore million-action endurance only after the major semantic gaps have
oracles and registered scenarios. Physical-error wakeup/cancel, bank/mask,
and same-cycle isolation crosses are now executable, as are the complete legal
whole-register NF/EEW matrix and ordinary unit-stride/strided/indexed
EEW/SEW/LMUL/EMUL matrices. Schema-11 `random-mixed` now carries those ordinary
vector shapes through the common constraint interface with per-seed coverage
and uop conservation; heterogeneous overlap windows still use their baseline
single-uop vector members. All six LSQ dispatch lanes now have passive
pre-edge acceptance and flow-count observation, including width histograms
used by the common random coverage gate. Redirect cancellation is likewise
sampled from the elaborated top after its defined latency and checked by the
Schema-2 queue-conservation gate. The next closure work is negative protocol
response injection. Translation mode depth itself is no
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
| DCache manager | Coherent 64-byte lines on a 256-bit bus; finite randomized delay | Agent drives and observes A/B/C/D/E traffic, distinguishes Get/AcquireBlock/AcquirePerm, checks ProbeAck(Data), ReleaseData, and GrantAck, and applies A/C/D/E timing pressure. Denied data responses also assert corrupt, denied data-less Grant is propagated, and corrupt injection on data-less Grant is rejected. Common random Probe injection is implemented while concurrent/multiple-source and malformed response modes remain planned |
| PTW manager | PTE memory and response source/size match the programmed roots | Reference page tables and PTW agent share sparse memory. `denied` is fixed over a multibeat response and implies `corrupt` on data; independent `corrupt` can select all, first, or last data beats. Stage-appropriate fencing is required before a clean retry of a cached access-fault result. Malformed response validation remains planned |
| Uncache manager | Only modeled Get/Put requests receive AccessAck/Data | Ordered byte-level update plus an optional side-effecting device window are modeled. Denied data responses imply corrupt, a denied Put cannot update either backing-memory class, and corrupt on data-less `AccessAck` is rejected. Scalar size/address/mask/lane, response identity, legal denied/corrupt exception checks, PBMT=IO store request/retirement, read-clear behavior, error-response side-effect suppression, exact partial-write logging, same-class queued MMIO, and mixed load/store MMIO serialization are executable (`uncache-widths`, `uncache-errors`, `mmio-contracts`); CBO.ZERO is covered through the cacheable SBuffer path (`cbo-zero-contracts`), while malformed/duplicate/early/late responses remain planned |

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

- cross-hart LR/SC reservation interference and atomic ordering with concurrent
  traffic (all operation encodings, the complete opcode-by-illegal-offset
  matrix, and reachable D-channel denied/corrupt cases are covered);
- CBO/CMO line operations, `fence`, and `fence.i` (translation-fence ordering
  for global/selective leaf updates, both supported stage modes, and all four
  fully nested VS/G pairs is covered by `translation-fence-all`);
- all 78 legal ordinary unit-stride, strided, indexed-unordered, and
  indexed-ordered configurations, all 338 legal segment
  NF/EEW/SEW/LMUL/EMUL configurations per addressing mode, all legal
  whole-register NF/EEW combinations, direct load/store/FOF redirect
  cancellation, and both first- and later-element segment FOF faults are
  covered;
- broader PMA region/permission/edge crosses for HLV/HLVX/HSV plus hypervisor
  PMP region-edge cases;
  all four nested translation mode pairs, PBMT PMA/NC/IO priority, cacheable
  misaligned split paths, basic SPVP/VSUM/VMXR, stage faults, HLVX execute
  permission, the M-mode/SPVP physical unlocked/locked R/R+X/W PMP matrix, and
  one fixed PMA device target for all three families are covered;
- architectural `satp`/`vsatp`/`hgatp` write/readback and WARL mode filtering;
  the MemBlock UT directly supplies the post-CSR `TlbCsrBundle` and therefore
  cannot establish software-visible Sv48/Sv48x4 enablement by itself;
- remaining instruction PMP permissions, hypervisor PMP region edges, and fixed-PMA
  cacheability, atomic, MMIO, overlap, and region-edge matrices; data-side
  TOR/NAPOT, lock, priority, R/W/AMO denial, and 4-KiB-grain WARL behavior are
  covered by `pmp-contracts`;
- hardware A/D updates and concurrent page-walk invalidation races (missing-A
  load/store faults, missing-D store faults, and
  SUM/MXR/VSUM/VMXR permission outcomes are covered by
  `translation-permissions`; global/selective single-walk races for both stage
  modes and all four fully nested pairs are covered by
  `translation-fence-all`);
- malformed TileLink coherence responses, broader Probe/source-reuse stress,
  broader-mask/delayed/persistent/wider-concurrency physical ECC injection, and
  coherence error recovery (ordinary clean/dirty Probes, two-source overlap
  with an unrelated miss, GrantAck, denied/corrupt load/atomic D responses,
  bank-0 single/double-bit tag ECC, and all-eight-bank single/double-bit data
  ECC plus same-cycle injected/clean bank isolation are covered);
- simultaneous malformed/duplicate/early/late PTW and uncache responses;
- the broader cross-cause/source exception matrix beyond the co-issued scalar
  page-fault/PBMT-NC-misaligned pair, bidirectional scalar/vector page-fault
  replacement, and bidirectional vector-load/store buffer selection;
  wrapped-age load/store selection and same-ROB vector `uopIdx` ordering are
  also covered;
- broader randomized split-load timing while RAR is near capacity (the directed three-pending-split/60-younger-load case is implemented);
- overlapping indexed operations and architecturally observable ordered-index
  accesses to a side-effecting translated target;
- four-state/X behavior and reset-sensitive uninitialized storage;
- reset with producer classes beyond the DCache-refill, PTW-walk, and
  Uncache/MMIO manager paths (all three now have accepted outstanding-request
  cancellation and distinct post-reset survivor checks);
- performance-counter/top-down attribution and hardware-prefetch training
  metadata, which have no stable architectural MemBlock oracle.

These gaps are reported per historical commit in
`HISTORICAL_BUG_AUDIT.md`; a reset-held pin toggle is not counted as closing
them. The historical `vector-guest-fault-split` mismatch is now fixed and is
included in the repaired sentinel and boundary-hunt gates.

## Regression Acceptance

Before a duration run:

1. `check-ports`, `check-rtl`, and all Python unit tests pass.
2. `benchmark-tests` passes every leaf scenario registered by the C++ dispatch
   on the current frozen binary and complete RTL hash. A unit check requires its
   inventory to remain exactly equal to the executable dispatch inventory.
3. A multi-seed, six-scenario matrix passes with backpressure enabled. The
   sixth scenario is `frontend-bridge`; its offline verifier independently
   requires three A requests and four D beats per requested transaction,
   request/response/source-credit stalls, and at least 39 field checks per
   transaction.
4. The executable, Verilated model, xspcomm, resolved system libraries, runner,
   streaming verifier, runtime-freeze script, controller sources, and RTL metadata are frozen and
   hashed. The verifier script is passed as a controller input so its acceptance
   logic is part of the recorded provenance.

The time-based breadth campaign runs at least 28,800 monotonic seconds (eight
hours) with eight workers and the `random-mixed --constraints spec` scenario.
Each seed requests 16,384 total actions, including the mandatory coverage prefix and a constrained-random tail
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

Final acceptance additionally runs a finite within-seed endurance campaign:
eight independent `spec` seeds, each with exactly 1,000,000 actions and an
eight-hour per-seed timeout. Its verifier requires all 8,000,000 requested
actions, every seed's functional/backpressure gates, and unchanged frozen RTL,
runtime, runner, and controller hashes. The two campaigns are complementary:
the duration campaign explores more initial seeds, while the endurance
campaign stresses long-lived queue, pointer-wrap, cache, translation, and
manager state without process restarts.

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
