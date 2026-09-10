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
The later normative clarification that MXR affects explicit loads, not
implicit address-translation reads, is tracked by merged RISC-V ISA Manual
[PR #1543](https://github.com/riscv/riscv-isa-manual/pull/1543).

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
| Address translation | Mode-parameterized software walk for Bare/Sv39/Sv48 and Sv39x4/Sv48x4; independent canonicality, PTE validity, PPN physical/GPA-width overflow, leaf level, alignment, Svnapot VPN-low-bit substitution, permission, PBMT/N/reserved, and A/D checks | Accesses reach the independently calculated PA; invalid walks report the access-specific page/access/guest-page fault without a data-manager request | Partial: generic walker, Bare degenerations, all four 4-KiB nested paths, superpage leaves, all 16 subpages of legal 64-KiB Svnapot leaves in Sv39/Sv48 stage-1 and Sv39x4/Sv48x4 G-only modes plus all four nested mode pairs crossed with VS-only/G-only/both-stage NAPOT, each with load/store/readback, and schema-12-and-later random traffic over distinct stage-1 and four nested leaf-topology regions are implemented. Sv39/Sv48 high-half canonical and both noncanonical sign-extension directions, physical PPN bits 48/55 at every ordinary stage-1/VS-only/G-stage PTE level, every-level VS-generated 41/50-bit GPA overflow across all four nested pairs, permission cases, the complete valid two-stage PBMT matrix, and 52 invalid/reserved/PBMT/NAPOT encodings are implemented for both scalar loads and stores. Schema 19 adds independently located root/intermediate/leaf PTW manager faults and clean retries across every supported stage/mode pair; remaining permission/fault crosses remain |
| Nested translation | Independent VS-stage walk followed by independent G-stage walk for all four `vsatp` x `hgatp` mode pairs, including implicit page-table accesses, VS-generated GPA width, and VS-over-G PBMT priority | Exact host PA or stage-specific fault; no stage may be skipped or silently treated as Bare; final PMA/NC/IO class follows architectural composition | Partial: all four 4-KiB pairs, VS/G/Bare degenerations, every-level 41/50-bit VS-generated GPA overflow across all four non-Bare pairs and load/store, all 36 valid PBMT combinations, VS/G context switches, host/nested `V` transitions, same-ID root reuse with targeted fences, global/selective stale-response races, co-issued distinct-page walks, and redirected root/ASID/VMID/MODE/`V` changes with a delayed walk are covered. The common random tail weights and gates every mode pair, all enabled VS/G NAPOT topologies, cold/reuse observations, and manager errors at implicit-G, VS-PTE, and final-G nested sites |
| Data-side PMP/PMA | Hand-calculated TOR/NAPOT regions and architectural first-match, permission, lock, current/effective privilege, platform-grain rules, and fixed SoC PMA entries | Exact allow/fault result and constrained manager behavior on denial; ordinary load/store/AMO denials emit no DCache/Uncache request. A split hypervisor load may expose at most the independently allowed prefix line before its terminal physical-PMP fault; no Uncache or architectural effect is allowed | Partial: `pmp-contracts` covers TOR/NAPOT edges, R/W/AMO denial, overlap priority, M-mode bypass, lock, and 4-KiB-grain WARL behavior. `hypervisor-contracts` crosses M-mode HLV/HLVX/HSV with SPVP=U/S and R/X/RW/RX plus locked R/RWX PMP regions, then maps all three families to a fixed `c=0` PMA device window. Schema 26 closes seven 4-KiB NAPOT edge relations across every family x SPVP pair; other sizes, TOR/lock/permission/overlap-by-edge, and broader PMA crosses remain |
| L2-to-L1 DTLB boundary | Drive all retained `io_l2_tlb_req_req_*` controls with the legal read command, including ordinary/prefetch, kill, and `no_translate`; sample the separately registered PMP output at its specified later cycle | Exact miss/hit valid; on non-fault hits, exact PA, PBMT, fault, PMP-deny, and PMA-MMIO fields; on fault hits, exact PF/GPF/AF with address/protection payload treated as invalid. A cold miss starts a shared PTW refill, and the same-VA retry must hit without an additional external PTW TileLink A request | Implemented at the MemBlock boundary: `l2-tlb-contracts` covers ordinary/prefetch miss, cacheable/PBMT-NC/PBMT-IO refill hits, stage-1 PF, nested GPF, PTW AF, kill/no-response, pruned-zero-address `no_translate`, PMP allow, locked 4-KiB PMP deny, and MMIO classification. External-L2 retry policy remains integration-owned |
| IFU-to-Mem PTW bridge | Typed `PtwReq` driver over `io_fetch_to_mem_itlb_*` with an independent page-table image | Exact Sv39/Sv48 and nested sector response metadata; request acceptance and response payload stability under backpressure | Implemented: `ifetch-ptw-bridge` covers valid Sv39/Sv48, all four Sv39/Sv48 x Sv39x4/Sv48x4 4-KiB pairs, Sv39/Sv48 VS-only and G-only degenerations, PBMT=NC/IO leaves, invalid L0 leaves, and all eight sector indices in one Sv39 leaf-PTE block. The sector matrix interleaves three PPN-high groups and checks exact `valididx`, one-hot `pteidx`, every selected `ppn_low`, reconstructed PPN, three cold PTW reads, and zero subsequent reads. Stage-1 and G-stage `G` bits are covered at leaf/non-leaf levels; stage-1 `G=0` may not be promoted, while legal conservative demotion of `G=1` is counted. G-stage `G` may be exposed as raw internal metadata but cannot affect translation, permission, or fault results. Every nested pair is crossed with VS-leaf, final-G-leaf, and implicit VS-page-table G-stage faults. A delayed IFU walk overlaps a cold scalar DTLB walk, and two same-VPN IFU requests coalesce into one three-request Sv39 walk with two exact responses. Delayed stage-1 and nested walks are invalidated by root/mode/ASID/VMID changes plus global and selective `SFENCE.VMA`, `HFENCE.VVMA`, and `HFENCE.GVMA`; each requires stale-response suppression and the exact replacement mapping. The active-stage ASID/VMID, permission, PPN, index, PBMT, fault level, fault bits, both leaf PTE addresses, and cross-source PTW outstanding depth are checked |
| L2 hint propagation | Valid/invalid `io_l2_hint`, all `sourceId` values, and `isKeyword` polarity at an idle/no-matching-MSHR boundary | Hint is registered and distributed without producing a ghost writeback, queue corruption, or protocol error; matching-MSHR replay semantics are integration-tested with L2 | Implemented for both keyword polarities and all 16 source IDs with an idle no-MSHR safety oracle in `l2-tlb-contracts`; matching-MSHR replay remains an L2 integration scenario |
| Frontend bridge | Independent request/response sequences for ICache, ICache-control, and instruction-Uncache, including the fields synthesized or narrowed by their diplomacy edges | Every accepted A request emerges once and in order with exact opcode/size/source/address/mask/data/user fields; every accepted D beat returns once with exact observable fields; a source is not reused before its response completes; payload remains stable under request and response stalls | Implemented in `frontend-bridge`: all 88 top-level frontend TileLink fields are explicitly driven or checked, the three paths run concurrently, source credits are enforced and observed, ICache uses two response beats, and the ICache-control path exercises Get/PutFull/PutPartial, sizes 1-8 bytes, all 32 sources, masks, and D-channel backpressure |
| Guest-fault metadata | Reference VS/G-stage walk from PTE addresses, including explicit data faults and implicit VS-page-table faults | Exact fault VA, faulting PTE GPA, shifted `htval`-class value where observable, and VS-non-leaf-PTE marker | Partial: current VA/GPA/marker cases are covered; broader fault classes are planned |
| Misalignment | Byte concatenation/splitting across 16-byte, line, and page boundaries | Exact value/bytes when enabled; specified address-misaligned exception when disallowed by memory type/control | Partial: common scalar/vector splits are covered; `scalar-misaligned` also holds three split loads at the LQ head, completes 60 younger loads to pressure RAR, and drains the splits in ROB order with exact data and no spurious violation |
| Exception side effects | RISC-V cause rules plus independent circular ROB/uop-age ordering, deliberately opposed to LQ/SQ allocation order | Exact exception bit; exceptional scalar load has no integer/FP RF write; software prefetch never raises a load exception or writes an RF; the exported exception VA belongs to the oldest coexisting fault | Partial: `exception-contracts` checks simultaneous scalar faults across ROB wrap with reversed LQ/SQ order, switches the retained load/store source for both scalar and vector load faults, makes an older `vuopIdx=0` vector fault replace a younger `vuopIdx=1` fault at the same ROB, chooses between co-issued page and PBMT-NC-misaligned faults by age, and replaces retained scalar/vector faults in both source directions; the broader cause/source matrix remains |
| Redirect | ROB age and redirect level supplied by a legal backend transaction | Redirected younger work has no terminal writeback; surviving work completes with the same data | Partial: basic redirect is covered; each known redirect samples the retained top-level LQ/SQ cancellation counts once after their two-cycle queue-output latency and the directed scalar case requires exactly one LQ cancellation. Broader producer-class arbitration remains |
| Cache coherence boundary | TileLink opcode/source/size/mask/data reference agent with separate bus and architectural memories | Stable producer payload while stalled, complete refill, sink-exact GrantAck, clean/dirty ProbeAck(Data), ReleaseAck, byte-exact dirty data, atomic/CMO update, and denied/corrupt D-channel handling | Partial: `dcache-coherence` checks clean ProbeAck, requested clean ProbeAckData, mandatory dirty ProbeAckData, invalidation/refill, and E-channel GrantAck with forced backpressure. It also accepts two distinct Probe sources while an unrelated cold miss is outstanding and address-matches both responses. `cmo-contracts` adds custom CLEAN/FLUSH/INVAL A requests, all operation x clean/dirty toB/toN reports before delayed CBOAck, retained/refilled line state, a younger cold MSHR canceled by CMO `flushPipe`, and six denied/corrupt cases. Schema-15 `random-mixed` adds weighted translated CMO operations, clean/dirty state, randomized CBOAck delay, optional younger delayed-MSHR cancellation, and opcode-qualified CBOAck errors crossed with every operation. Schema 33 covers the standard configuration's complete eight-entry ProbeQueue: a delayed cold miss remains unwritten while zero through seven clean auxiliary Probes and the dirty primary Probe are queued on distinct active B sources, C stays unready until all selected B requests are accepted, all address-matched responses complete, and measured accepted-but-unanswered depth equals one through eight. All 32 depth x toB/toN x requested/mandatory-data bins close; toB retention gets a checked toN cleanup. Schema 34 derives the complete six-bit B-source namespace from the generated port, rejects active-ID reuse, and exactly checks unique accepted IDs, completed-ID reuse, and every 63-to-0 wrap. Schema 36 closes 48 CLEAN/FLUSH/INVAL x clean/dirty x one-through-eight-depth bins while CBOAck is pending. Schema 37 closes 54 AMO/LRSC/AMOCAS x W/D x zero-through-eight-depth bins: depth zero retains ordinary atomic traffic, while nonzero depth uses a held cold refill with exact auxiliary-response and atomic-result attribution. DCache denied/corrupt load errors and all 22 refill-capable W/D LR/AMO/AMOCAS error paths are executable. Malformed responses and replacement cross-operation Probe bursts remain planned |
| PTW/uncache boundary | TileLink and uncache ready-valid agents with deterministic memory | Stable request/response while stalled, legal source/opcode/size/address/mask, ordered NC/MMIO store data, exact beat/lane load response, denied/corrupt error propagation, and SQ retirement | Partial: legal backpressure and response identity are executable. `ptw-errors` crosses denied/corrupt with Sv39/Sv48 stage-1, isolated Sv39x4/Sv48x4, fully nested, bitmap, load/store, and root/intermediate/leaf walks; corrupt is split across first/last beats, denied obeys the fixed-field/data-corrupt TileLink rules, and every fault is followed by a stage-appropriate fence plus a checked clean retry. Schema 19 composes that address-qualified oracle with common random traffic at five host/G/nested sites, and conserves duplicate requests plus every response beat. Scalar Uncache width/lane and denied/corrupt propagation are executable in `uncache-widths`/`uncache-errors`; PBMT=IO direct MMIO load/store bypass, metadata/error path, SQ retirement, and an ordered side-effecting device log are executable in `mmio-contracts`. Malformed/duplicate/early/late response injection remains planned |
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
| Vector loads | EEW/SEW 8/16/32/64; fractional/integer LMUL and legal derived EMUL; both lanes; unit, strided, indexed unordered/ordered; independently constrained `vm`, `vma`, `vta`, `vl`, and `vstart`; split windows | Exact 128-bit result, active mask, metadata, replay, LQ drain; schema-41 `random-mixed` closes direction x addressing x legal EEW/SEW/LMUL crosses, requires observable mask/tail agnostic elements, and expands complete 1..8-uop instructions |
| Vector stores | All legal EEW/SEW/LMUL/EMUL shapes and address modes; independently constrained mask, `vstart`, and partial `vl`; misaligned and cross-page split/replay, including back-to-back translated cross-page stores | Exact active-byte readback, completion, commit, SQ drain; schema-41 direction-specific cross and schema-11 policy counts conserve against instructions and queue-capacity windows preserve full uop identity; `misaligned-stores` also requires two independent cross-page stores to make progress and read back exactly |
| Software prefetch | `prefetch.i/r/w`, all scalar issue lanes, mapped and unmapped VAs | Completion without RF write or exception; LQ drain |
| Hardware data prefetch | Fixed-PC cold loads with a 128-byte stride, weighted by the common `stride-stream` constraint and mixed with cache/TLB/manager pressure | Exact load completion plus CSR-disable suppression; current sender source/address/count observations are implementation characterization, not architectural PASS gates |
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
- HLV/HLVX/HSV traffic under nested translation, with independent family and
  SPVP=U/S constraints plus per-seed family x effective-privilege coverage.
- address-qualified PTW manager errors across host stage-1, G-only, and all
  three nested walk sites, with root/intermediate/leaf and denied/first-beat-
  corrupt/last-beat-corrupt coverage.

There is one canonical mixed generator. Realistic traffic, balanced coverage,
and corner pressure are constraint sets over that generator, not independently
maintained scenario implementations. `--constraints coverage|spec|corner`
selects a baseline and repeatable `--constraint key=value` arguments override
operation mix, address locality, heterogeneous overlap, translation regime and
Sv39/Sv48/VS/G modes, context-switch and legal fence kind/scope rates,
misalignment, vector corner bias, vector-segment direction, atomic family/width,
hypervisor family/SPVP, CMO operation/line state/younger overlap/error presence and
kind, NC/MMIO direction and Uncache error presence/load-error kind,
PTW error site/level/direction/error kind,
legal special overlap, Probe/refill overlap, hardware stride-stream pressure, and independent
DCache/PTW/Uncache response latency. The complete interface
and performance-counter calibration are specified in
[`CONSTRAINED_RANDOM.md`](CONSTRAINED_RANDOM.md).

Constraint-interface closure is tracked separately from architectural prefix
coverage. Atomic AMO/LRSC/AMOCAS family, W/D width, D-channel error
presence/kind, and Probe depth, CMO operation/line state/
younger-load overlap/error presence/error kind, DCache scalar-load refill-error
presence/kind, Probe/refill overlap, NC/MMIO
load/store ratios, Uncache response-error presence/load kind, legal NC/MMIO
load overlap, translation regime/mode/fence selection, and per-manager response
latency are now validated, replayable fields with
observed coverage counters. Atomic operations
remain serializing actions because the MemBlock boundary explicitly blocks the
pipeline while LR/SC/AMO is active. Atomic, CMO, ordinary scalar-load refill, and
Uncache errors are now in the common interface, including both legal Uncache
store-error contracts. Schema 19 also brings PTW errors into that interface;
the focused `ptw-errors` scenario remains the deterministic contract, not a
separately maintained random generator. Schema 20 adds same-line load merging
as another weighted operation with adjustable 2/3-way depth and exact-address,
same-beat, or cross-beat locality under the shared translation and manager-
latency constraints. Schema 38 adds weighted same-set clean/dirty replacement
with adjustable 9/10-line depth, B/H/W/D width, set-index quarter, independent
held-refill overlap, address-qualified Release backpressure, and
one-through-eight replacement windows, crossed with Bare/stage-1/nested
translation and the same
manager latency. Multi-window overlap actions keep one address-qualified D
response per set pending and require every target set to satisfy its own
replacement minimum before any response is released.
Schema 43 extends the ordinary cold-miss burst dimension with scalar-only and
scalar+vector-load compositions at depth 2..16 and initial same-cycle scalar
issue width 1..3. It closes every enabled depth x legal width x composition x
Bare/stage-1/nested bin, skips lines already seen at the external manager, then
arms and holds each address-qualified refill until externally observed request
depth reaches the target. Exact scalar/vector writeback and LQ-flow
conservation remain mandatory; target-request and global refill/GrantAck counts
allow legal duplicate/background traffic. SPEC
performance counters motivate the profile weights but are not used as a
correctness oracle. Schema 45 adds a capacity-plus-one depth-17 class. It first
holds 16 address-qualified external responses, releases one fair response, and
requires the seventeenth request plus every exact terminal result and LQ
disposition to complete. It does not use internal reject/replay state as an
oracle. Schema 40 adds an
explicit `bank-conflict` dimension for the SPEC-observed same-bank/replay
class. It issues two- or three-way resident scalar waves over all eight legal
8-byte line-offset classes and every enabled Bare/stage-1/nested context. The
correctness contract is external: exact per-identity data, one terminal
writeback and LQ dequeue per member, and legal queue/protocol termination. Bank identity,
replay/arbitration, wakeup/`ld2Cancel`, and prefetch traffic are diagnostic
observations only; no internal counter or fixed historical address is used.
The first scalar/vector/AMO/miss-burst interaction reduction is executable
with the same external identity/data/queue oracle. The miss burst itself now
contains a legal two-flow vector load in its mixed composition; full ordinary-
shape and heterogeneous-window composition, including bank waves with every
vector and atomic class, remains a planned gap.
Schema 36 composes every CLEAN/FLUSH/INVAL and clean/dirty target state with
one through eight accepted manager Probes while CBOAck remains pending. It
closes 48 operation x line-state x depth bins and independently conserves
B-source, C-response, target-data, and error-path traffic.
Schema 37 composes every successful AMO/LRSC/AMOCAS family and W/D width with
depth zero ordinary traffic or one through eight auxiliary clean Probes around
a fresh cold target refill. For nonzero depth the atomic D response is held
before writeback, B may legally backpressure during that interval, and C is
held after D release until the selected burst is accepted. The 54 family x
width x depth bins, exact atomic result, no-data Probe responses, outstanding
depth, and manager/source lifecycle all close.
Schema 22 crosses every enabled HLV/HLVX/HSV family with SPVP=U/S and uses
independently mapped user regions for all four 4-KiB/Svnapot VS/G leaf
combinations, so translated data and store side effects remain checked against
physical reference memory.
Schema 23 reuses the common `misaligned` constraint and requires every enabled
HLV/HLVX/HSV x SPVP=S/U x aligned/misaligned combination per seed. The
generator excludes byte operations when misalignment is forced and checks the
selected address class before applying the existing two-stage data oracle.
Schema 24 adds five VS/G PBMT pair weights covering final PMA/NC/IO and both
VS-over-G priority directions. Every enabled HLV/HLVX/HSV x SPVP=S/U x PBMT
pair executes per seed with an independent final-PA/PBMT oracle, exact physical
load/store bytes, and exact DCache-versus-Uncache routing. Non-PMA PBMT random
actions remain naturally aligned pending an explicit exception-priority model.
Schema 25 adds a DDR/fixed-device class across the same family/SPVP pairs.
Schema 26 adds no-PMP, exact first/last allowed, immediately below/above, and
lower/upper crossing relations for a 4-KiB NAPOT allow region nested ahead of
a 16-KiB deny entry. Every enabled HLV/HLVX/HSV x SPVP x relation executes per
seed with independent first-match interval arithmetic, exact success/fault,
manager-side-effect, post-HSV HLV readback, wakeup-cancel, redirect, and LSQ
conservation checks.

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
MMIO load. Its vector members are complete legal 1..8-uop load/store
instructions rather than representative single uops. Schema 42 rotates through
all reachable direction x addressing x single/multi-uop composition classes,
then varies issue order, scalar store address/data order, vector shape and
policy, alignment, cache residency, translation state, and manager delay while
scoreboards remain outstanding. A bounded drain occurs only after the window,
preserving real heterogeneous overlap without allowing unbounded pointer reuse.

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
| Vector segmented/whole-register | NF/segment count, multi-uop streams, load/store direction, EEW, mask/`vstart`, fault-only-first, and partial completion | Partial: `vector-segment` checks lane-0 takeover, interleaved load/store data, multi-uop identity, zero LSQ allocation, and all addressing modes. Every addressing mode exhausts 338 legal NF 2..8 x EEW/SEW/LMUL/EMUL configurations per mode with exact load/store/readback; strided cases cover positive/negative strides, while indexed cases cover the complete index group and 252 index-only uops required by 32 `EMUL>LMUL*NF` configurations across both modes. `vector-segment-fof` covers first/later faults; schema 44 brings ordinary VFOF into `random-mixed` and closes first/later fault x Sv39/Sv48 x EEW with exact fix-VL conservation. `trigger-contracts` covers field-1 indexed and strided-store breakpoints; `vector-addressing` exhausts all ordinary addressing matrices plus all 16 whole-register NF/EEW combinations. Schema-9-and-later `random-mixed` exposes addressing, EEW, SEW, LMUL, EMUL, NF, and direction weights through one interface and independently gates every enabled class. Full-core `waitForward`/`blockBackward` serialization makes an active segment plus redirect unreachable, so that combination is an upstream assumption rather than a MemBlock stimulus. Segment FOF remains focused rather than common. Overlapping indexed stores and ordered access to a side-effecting translated target remain |
| Vector data patterns | all zero/one, ramps, alternating bits, random bytes, same-byte aliases, old-destination merge | Implemented/partial by operation class |
| Software prefetch | `prefetch.i/r/w`, mapped/unmapped, cacheable/NC/IO, all lanes, signed `src + imm`, duplicate and outstanding requests | Partial: `ifetch-prefetch` checks every class/lane, all signed-12-bit boundary classes through 0/1/2047/-1/-2048, a same-cycle `i/r/w` batch, unmapped cold data-hint drop without PTW/data traffic, warmed Sv39 cacheable data-prefetch hits, individual `r/w` DCache requests, and the intentional PBMT policy: resident-TLB NC `r/w` issue DCache hints without Uncache while IO `r/w` reach neither manager; broader duplicate/same-line contention remains |
| Atomics | LR/SC, AMOADD/XOR/AND/OR/SWAP/MIN/MAX and signed/unsigned variants, AMOCAS, reservation loss, alignment | Partial; all exposed W/D-width AMO variants, AMOCAS.W/D compare success/failure, LR/SC success/failure, all 24 W/D opcodes crossed with every illegal byte offset (120 cases), and a fixed-device-PMA `AMOADD.D` denial execute in `atomic-contracts`. The alignment matrix checks LR load-misaligned versus all other store-misaligned exception classes, suppressed RF write, zero new DCache traffic, and ROB pointer wrap; the PMA fault suppresses RF write and all manager/memory side effects. `atomic-dchannel-errors` crosses denied/corrupt with all 22 refill-capable W/D LR/AMO/AMOCAS operations, checks initial exception/RF contracts, later poisoned-line load hits, SC.W/D hits on denied/corrupt metadata, exact request counts, and clean AMO recovery. Schema-18 `random-mixed` adds common `atomic-error`/`atomic-error-denied` controls and requires all 18 enabled family x width x clean/corrupt/denied outcomes, exact exception and manager accounting, unchanged backing memory at response time, and a deterministic private poisoned-line image for later ReleaseData. Schema 37 adds all 54 successful family x width x zero-through-eight-depth bins. Depth zero keeps the ordinary path; nonzero depth checks exact old-value/result and one cold target request, forbids early writeback while D is held, and validates source/address-matched no-data Probe completions after D release. The SC checks do not claim internal reservation observability. SC cannot have a cold-miss D response because MainPipe returns failure before issuing TileLink traffic when the line or usable reservation is absent. Cross-hart reservation interference and replacement/concurrent ordering remain |
| CBO/CMO/fences | clean/invalidate/flush/zero, `fence`, `fence.i`, `sfence.vma`, ordering with outstanding traffic | Partial; cacheable `CBO.ZERO` StoreQueue/SBuffer line-zero and readback are executable (`cbo-zero-contracts`). `cmo-contracts` covers CLEAN/FLUSH/INVAL opcode/source/line alignment, all six operation x clean/dirty line states with exact TtoB/BtoB/TtoN/BtoN reports, automatic SBuffer drain of a previously committed dirty store without a direct flush, SBuffer-before-request ordering proven by the resulting exact dirty Probe data, clean no-data transitions, retained-hit/cold-refill outcomes, delayed completion, `flushPipe`, and every operation crossed with denied/corrupt CBOAck. It also accepts a younger cold load into a distinct MSHR while CBOAck is pending, then requires the legal CMO `flushAfter` to cancel exactly that LQ entry and suppress its delayed response. Schema-15 `random-mixed` exposes CLEAN/FLUSH/INVAL, clean/dirty, younger-overlap, error presence, and corrupt/denied kind as common constraints. It runs in the active Bare/stage-1/nested context, randomizes CBOAck and younger-refill latency, requires every enabled operation x error-kind cross, and conserves failed CMOs separately from success-path Probes. Error responses require the exact StoreAccessFault/HardwareError, no C response, unchanged bus memory, and redirect cleanup. Schema 36 adds one-through-eight-source Probe bursts during every successful operation/state class, closes 48 bins, and requires exact accepted depth, source/address response matching, target dirty data, and manager conservation while errors retain zero Probe. The internal `cmoOpResp` is inferred through external DCache A/B/C/D and final StoreQueue writeback. Translation fences are covered separately; full ISA `fence.i` is outside this top-level transaction boundary, while multiple simultaneous CMO sources and wider replacement/atomic ordering remain |
| Hypervisor memory ops | HLV/HLVX/HSV, effective privilege/SPVP, VSUM/VMXR, execute permission, guest/host faults | Partial: `hypervisor-contracts` executes all exposed HLV/HLVX/HSV encodings, SPVP user/supervisor cases, VSUM/VMXR permission changes, VS- and G-stage faults, HLVX execute-only access, representative operations under all four Sv39/Sv48 and Sv39x4/Sv48x4 pairs, a five-combination PBMT basis, and cacheable misaligned split paths. Eighteen M-mode cases cross all three families and SPVP=U/S with R/X/RW/RX plus locked R/RWX physical PMP regions, requiring R, R+X, and W; nine fixed-PMA cases cover an interior device target plus every family immediately below and at the exact `0x80000000` device-to-DDR boundary. Schemas 22-25 close family x SPVP, alignment, five-pair PBMT, and DDR/fixed-device dimensions with independent PA/data/store/manager oracles. Schema 26 closes every family x SPVP x no-PMP/first/last/below/above/cross-lower/cross-upper bin for a nested 4-KiB NAPOT allow region, including precise faults, manager non-use or allowed-prefix behavior, and recovery. Other fixed-PMA boundaries and broader PMP size/TOR/lock/permission/overlap-by-edge crosses remain |

### Address, translation, and protection points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| Alignment | every byte offset for widths 1/2/4/8/16, beat boundary, 64-byte line, 4 KiB page, two-page and two-line splits | Partial; common scalar/vector split classes implemented |
| Virtual address classes | Sv39 canonical low/high halves, Sv48 canonical low/high halves, noncanonical bits, page-offset boundaries, aliasing VAs, VA wraparound | Partial; low/high canonical halves and both noncanonical sign-extension mismatch directions execute for Sv39 and Sv48; broader page-offset, aliasing, and wraparound crosses remain |
| Physical address classes | cacheable, uncached, device, reserved, high PA bits, line/set/way aliases | Partial; cacheable and PBMT-NC plus a non-DebugModule SoC `c=0` PMA device interval, guarded DebugModule denial, exact device-to-DDR manager selection on both sides of `0x80000000`, and read-clear/partial-write device semantics are executable in `mmio-contracts`. `hypervisor-contracts` now checks the same exact boundary after two-stage translation for HLV, HLVX, and HSV; reserved/high-PA/alias matrix remains |
| Stage-1 Sv39 walk | Bare/Sv39 mode, L2/L1/L0 leaves (1-GiB/2-MiB/4-KiB plus 64-KiB Svnapot), invalid/non-leaf, misaligned superpage, canonicality, permission/PBMT/A-D combinations | Partial; all leaf levels, all 16 legal Svnapot subpages with load/store/readback, high-half canonical VA, both noncanonical sign-extension directions, malformed superpage, physical-PPN overflow at L2/L1/L0, U/S, SUM, MXR, missing-A/D, and 13 invalid/reserved/PBMT/NAPOT encoding cases execute for loads and stores; broader crosses remain |
| Stage-1 Sv48 walk | Bare/Sv48 mode, L3/L2/L1/L0 leaves (512-GiB/1-GiB/2-MiB/4-KiB plus 64-KiB Svnapot), L3 faults, canonicality, permission/PBMT/A-D combinations | Partial; all leaf levels, all 16 legal Svnapot subpages with load/store/readback, high-half canonical VA, both noncanonical sign-extension directions, physical-PPN overflow at L3/L2/L1/L0, U/S, SUM, MXR, missing-A/D, and the same 13 encoding cases execute for loads and stores; broader crosses remain |
| G-stage Sv39x4 walk | 16-KiB root, widened root index, 41-bit GPA, all leaf levels including 64-KiB Svnapot, high-GPA overflow, G-stage permissions | Partial; all leaf levels, all 16 legal G-only Svnapot subpages with load/store/readback, final and every-level VS-generated GPA overflow, physical-PPN overflow at L2/L1/L0, load/store permission faults including `D=0`, all valid two-stage PBMT combinations, and 13 invalid/reserved/PBMT/NAPOT encoding cases execute for loads and stores |
| G-stage Sv48x4 walk | 16-KiB root, widened root index, 50-bit GPA, all leaf levels including 64-KiB Svnapot, high-GPA overflow, G-stage permissions | Partial; all leaf levels, all 16 legal G-only Svnapot subpages with load/store/readback, final and every-level VS-generated GPA overflow, physical-PPN overflow at L3/L2/L1/L0, load/store permission faults including `D=0`, all valid two-stage PBMT combinations, and the same 13 encoding cases execute for loads and stores |
| Nested mode matrix | `Sv39->Sv39x4`, `Sv39->Sv48x4`, `Sv48->Sv39x4`, `Sv48->Sv48x4`, plus `vsatp`/`hgatp` Bare degenerations and VS/G Svnapot placement | Partial; all four 4-KiB pairs are executable in `translation-matrix`, including cold/warm TLB reuse; `translation-superpages` crosses all four pairs with VS-only, G-only, and simultaneous VS/G NAPOT across every 4-KiB subpage; VS-only/G-only/fully-Bare degenerations execute in `translation-bare`; the same four non-Bare pairs are independently weighted and required in `random-mixed` |
| Stage-only translation | HS/S/U stage-1 only, VS/VU stage-1 only, G-stage only for implicit page-table/HLV-class accesses | Partial; only current data-access paths are modeled |
| TLB behavior | cold miss, hit, refill, duplicate miss, replay, invalidation, `sfence.vma`, concurrent page walks | Partial; `translation-fence-all` holds stale stage-1, VS-only, G-only, and fully nested VS/G leaf responses across global and selective fences for both supported modes and all four nested pairs, rejects canceled writeback, requires new-data refills, and co-issues two distinct-page stage-1 plus nested walks before the first delayed PTW response; `random-mixed` requires observed cold-walk and warm-reuse windows plus every enabled legal fence cross; broader duplicate-miss source-reuse stress remains |
| Page permissions | R/W/X/U/G, SUM/MXR at HS/VS stage, G-stage U-mode rule, read-only store, execute-only, access/dirty bit updates, privilege transitions | Partial; `translation-permissions` executes 194 cases: 16 stage-1 loads, 17 stage-1 stores, 75 two-stage loads, and 86 two-stage stores. Sv39/Sv48 U/S, SUM, MXR, A/D, VSUM/VMXR, and final G-stage load/store R/A/D/U variants are checked with every nested permission variant crossed over all four VS/G mode pairs. A 42-configuration implicit-walk cross applies G-stage X-only+MXR, U=0, and A=0 to every VS PTE level for both original load and store, producing 84 access-specific GPF transactions with exact VA/GPA/marker, early walk termination, manager non-use, and SQ conservation; four positive stores prove R-only W=0/D=0 VS-page-table mappings are sufficient for implicit PTE reads. Passing cases require exact readback; `translation-faults` covers PBMT/reserved encoding faults, while `translation-pbmt` covers valid PBMT composition |
| Mode/context switching | `satp/vsatp/hgatp` root and MODE changes, ASID/VMID reuse, `V` transitions, same VA under distinct contexts | Implemented for the modeled modes; `translation-context` covers five drained context families and 14 distinct-data same-address accesses; `translation-fence` adds same-ID host-ASID/VS-ASID/VMID fenced root reuse; `translation-inflight-context-all` holds an old PTW response for 256 cycles, changes root/ID/MODE or `V`, redirects the old load, reuses its ROB/LQ identity, and requires the new PA for both host mode directions plus all four nested starting pairs and both host/nested directions |
| Translation fences | `SFENCE.VMA`, `HFENCE.VVMA`, `HFENCE.GVMA`, selective/global scope and updates with outstanding traffic | Partial; global/selective leaf updates, targeted same-ID host-ASID/VS-ASID/VMID root reuse, stale-response races, and co-issued distinct-page stage-1/nested walks spanning both stage-1 modes, both G-stage modes, and all four fully nested pairs are implemented by `translation-fence-all`; the separate context-race matrix checks redirected root/ID changes while a response remains outstanding |
| PMP/PMA | TOR/NA4/NAPOT, overlap priority, lock, M/R/W/X, cacheability, atomic/MMIO permissions, exact region edges | Partial; `pmp-contracts` programs the distributed PMP CSR boundary and checks TOR/NAPOT exact edges, R/W plus AMO denial, overlap priority, M-mode bypass, lock enforcement, and lock immutability. `PlatformGrain=12` makes NA4 unselectable and coerces `A=2` to a minimum 4-KiB NAPOT region. HLV/HLVX/HSV check SPVP-selected physical R/R+X/W permissions, fixed-PMA device behavior and its exact `0x80000000` edge; schema 25 makes the interior class weighted. Schema 26 crosses seven exact 4-KiB NAPOT edge relations with every family and SPVP value. DebugModule access and device-PMA AMO denial are also focused. Other fixed-PMA matrices and PMP sizes/TOR/lock/permission/overlap-by-edge remain |
| Fault classes | load/store/instruction access fault, stage-1 page fault, G-stage guest-page fault, noncanonical VA, high-GPA overflow, address-misaligned, access-denied, bus/ECC error | Partial; load/store/page/misaligned, noncanonical, high-GPA, PMP access-denied, response-wide and per-beat denied/corrupt D-channel, one-shot physical DCache tag single/double-bit plus all-bank data single/double-bit ECC, and injected/clean load isolation are executable; instruction-side PMP and broader ECC cause combinations remain |
| Fault metadata | exact VA, GPA/PTE address, first failing level/stage, shifted `htval`-class value, guest marker, cause priority, single reporting and replay suppression | Partial; VS-non-leaf metadata, concurrent scalar load/store oldest-address selection across ROB wrap, same-ROB vector `uopIdx` replacement, one simultaneous page-fault/misaligned pair, bidirectional scalar/vector replacement, and vector-load/store buffer selection under both arrival orders are implemented; the broader cross-cause/source matrix remains |

### Cache, memory-system, and coherence points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| Frontend pass-through | ICache line reads, instruction-Uncache reads, ICache-control Get/PutFull/PutPartial, source/size/mask/data, simultaneous paths, A/D backpressure | Implemented at the MemBlock top boundary by `frontend-bridge`; downstream ICache/device semantics remain integration responsibilities |
| DCache lookup | warm hit, cold miss, same-line merge, bank conflict, distinct-line MLP, set pressure beyond associativity, synonym/alias | Partial; cold/warm and bank conflict are executable. Schema-20 `random-mixed` promotes same-line merging into the common constraint interface: fresh cold lines cross 2/3 same-cycle load lanes, exact-address/same-beat/cross-beat patterns, both critical beats, Bare/stage-1/nested translation, and the selected manager latency. Schema 38 drives 9/10 distinct clean-load or dirty-store tags through one to eight independent eight-way sets in every set quarter, access width, translation regime, no-overlap/held-refill class, and target Release backpressure class. Target windows containing any line covered by an earlier DCache request are skipped before backing memory is initialized. Clean lines are revisited in reverse with exact data and attributed replacement misses; dirty lines retain exact request, writeback, dequeue, and ReleaseData attribution. Schemas 43/45 drive ordinary never-repeated cold-line bursts, cross depth 2..17 with legal initial scalar issue width 1..3, scalar-only/scalar+vector-load composition, and translation, and conserve target lines, scalar/vector terminal effects, LQ flows, refills, and GrantAcks without inspecting internal MSHRs. Depth 17 first proves external saturation at 16 pending responses, then requires capacity-plus-one forward progress after one response is released. Broader physical synonym/alias crosses remain |
| Refill/replay | delayed A/D responses, critical-beat-first ordering, partial refill, killed request, replay after miss | Partial; `single-load` forces both `isKeyword` polarities and corresponding two-beat GrantData orders, checks exact data, merges two same-line loads into one delayed refill, and proves a keyword load writes back from its critical beat while the noncritical beat is held for 128 cycles without a later duplicate. `dcache-errors` crosses both orders with fixed denied and first/last-only corrupt, holds the second beat for 256 cycles, merges an opposite-half load, and accepts an unrelated healthy cold MSHR before that beat arrives. Schema-17 `random-mixed` adds response-wide corrupt/denied scalar-load cold refills under the common translation/latency/operation constraints, exact exception and redirect recovery, and sink-attributed GrantAck accounting even when a clean hardware prefetch is concurrent. Schema 18 applies the same address-qualified two-beat and GrantAck/refill conservation to atomic misses across family and width. Schema 20 varies clean merge depth, address placement, critical beat, translation, and calibrated response delay while allowing matched background prefetch refill/GrantAck pairs. Delayed and killed refills are exercised elsewhere; malformed/reordered response crosses remain |
| Eviction | clean release, dirty ReleaseData, partial byte masks, replacement under pressure, release backpressure | Partial; immutable whole-line snapshots are checked by the dedicated dirty-pressure phase and schema-21 random pressure. Schema 27 adds clean pressure; schema 28 adds held-refill overlap; schema 29 adds target C backpressure; schema 38 closes 1536 clean/dirty x no-overlap/held-refill-overlap x no-C-stall/C-stall x one-through-eight-window x 9/10-line x B/H/W/D x translation bins plus four set quarters. Clean actions require exact initial/revisit data, address-attributed target Release counts, zero target ReleaseData, and load/LQ conservation. Dirty actions retain exact target request/store-WB/SQ-dequeue counts and separate byte checks for target and background ReleaseData. Each multi-window overlap action holds one independent refill per set and requires every target set to reach its release minimum before any refill may complete. Each selected backpressure action stalls exactly one target Release for 16 valid cycles, compares the complete C payload on all 16 held transitions, and prevents unrelated C traffic from satisfying the target oracle. Replacement composition with Probe/CMO/atomic traffic remains |
| TileLink coherence | Probe/B/C/E traffic, source reuse, denied/corrupt/error responses, manager ordering | Partial; `dcache-coherence` executes three manager Probes spanning clean no-data, clean requested-data, and dirty mandatory-data responses, checks byte-exact C beats, forces E backpressure, and matches every GrantAck sink. It separately accepts two distinct Probe sources before an unrelated cold miss completes and checks outstanding depth and response addresses. `cmo-contracts` checks custom A opcode 12/13/14, fixed source 17, CBOAck opcode 8, all clean/dirty permission reports, Probe-before-Ack ordering, exact dirty C beats, a concurrent younger MSHR plus cancellation, and denied/corrupt propagation. Schema 33 `random-mixed` adds constrained dirty toB/toN Probes with requested/mandatory data, checked toB cleanup, and hierarchical one-through-eight-source bursts while a delayed refill remains outstanding; schema 15 retains translated CMO operation/state/younger-MSHR/error crosses. Schema 36 repeats all eight accepted depths while every successful CMO operation/state class is pending and closes 48 composition bins. Schema 37 repeats zero through eight auxiliary Probes for each successful atomic family/width. It permits legal B backpressure while the target D response is held, then holds C until the full burst is accepted after D release. CMO and atomic errors are excluded exactly from Probe conservation. The Probe oracle matches every C response by active B source and address. Schema 34 walks all 64 B-source IDs across accepted traffic, rejects active-ID reuse, and independently checks completed reuse plus wrap counts. `dcache-errors` rejects A-source reuse until final D completion, reaches two simultaneous AcquireBlock lifetimes, and checks exact D-beat/GrantAck drain. Load and atomic denied/corrupt D responses are also injected and checked; malformed responses and replacement cross-operation bursts remain planned |
| Uncache/MMIO | Get/Put widths, byte enables, side effects, ordering, response delay, denied/error response | Partial; PBMT-NC Get widths/byte lanes and scalar denied/independent-corrupt load propagation plus denied-store propagation are executable (`uncache-widths`, `uncache-errors`), and the denied store must preserve initialized backing bytes. Schema-16 `random-mixed` adds common Uncache error-presence and load error-kind constraints, forcing and conserving every enabled NC/MMIO x load/store x legal outcome bin. It distinguishes committed NC store errors, which report externally and dequeue without redirect, from precise MMIO store StoreAccessFault writeback. `mmio-contracts` checks PBMT=IO's direct load metadata bypass independently of final data/exception writeback: all three load-unit inputs, a lane-permuted same-cycle triple covering all three ROB-ordered compacted output slots and exact ROB/cycle values, exactly-one pulse before response under 64/128/256-cycle delays, plus cacheable/PBMT-NC negative controls. It also covers scalar store request/response/SQ-retirement, DCache non-use, denied/independent-corrupt metadata preservation, a physical non-DebugModule `c=0` PMA load/store pair, device-to-DDR edge loads, and a seven-request read-clear/partial-write device sequence with exact ordered logging. Corrupt is forbidden on data-less store `AccessAck`. Three same-address device loads queued before the first request prove external serialization under 512/128-cycle delays despite enabled Uncache outstanding mode. A separate pre-issued `load -> SW -> load` sequence crosses LQ/SQ and two 256-cycle response delays, requiring external depth one plus exact read-clear/write/readback state transitions. Cacheable CBO.ZERO line-zero/readback is executable (`cbo-zero-contracts`); malformed/duplicate/early/late responses remain |
| ECC/cache errors | correctable/uncorrectable data, error lifetime, retry or architectural exception | Partial; D-channel denied/corrupt propagation is executable for loads, atomics, and all three CMO operations in `dcache-errors`, `atomic-dchannel-errors`, and `cmo-contracts`. The CMO matrix requires exact StoreAccessFault/HardwareError and unchanged memory for all six cases; schema-15 repeats those operation/error crosses under common constrained clean/dirty, translation, latency, and optional younger-miss pressure. Schema 17 adds common scalar-load error presence/kind, dedicated unique cold lines, exact two-beat denied/corrupt accounting, errored refill/GrantAck identity, and precise redirect/LQ recovery. Schema 18 adds the complete enabled atomic family x width x outcome cross and distinguishes manager backing memory from the intentionally retained poisoned cache image. `dcache-errors` crosses both critical-beat orders with fixed denied plus first/last-only independent corrupt, a 256-cycle interbeat gap, same-MSHR merge, a concurrent healthy MSHR, poisoned/healthy resident hits, exact error priority, and queue/wakeup conservation. It also uses `L1DCacheCtrl` one-shot injection on proven-resident lines. The RTL tag pseudo-error path selects bank 0, where representative single- and double-bit masks must cancel, redirect, suppress terminal writeback, report the exact physical address to BEU, and recover on a clean survivor. Data ECC crosses all eight banks with spread single-bit and adjacent double-bit masks; in default `EnableAccurateLoadError=false` mode every case must report BEU, return the exact XOR-corrupted value without cancellation, and recover on a clean survivor. A same-cycle selected-bank/clean-bank pair checks lane isolation and automatic one-shot clear without a disable write. Broader masks, delayed/persistent injection, and wider mixed-traffic concurrency crosses remain |
| PTW manager | request/response backpressure, source reuse, malformed/denied response, concurrent walks | Partial; legal backpressure, response identity, and denied/corrupt error propagation are implemented. `ptw-errors` covers 16 stage-1/G-stage/nested/bitmap load/store cases, exact walk cutoff, access-fault writeback, cache-refill suppression, and zero data-manager side effects. Its eight corrupt cases split 4/4 across the first/last data beat; every error case then fences the owning translation stage and requires a clean same-address recovery with an exact failed-block reread and one DCache access. Two co-issued distinct-page DTLB walks remain pending around a delayed response even where that scenario observes manager depth one; the separate IFU-plus-DTLB overlap reaches manager depth two. Two same-VPN IFU requests return twice while coalescing into one three-request Sv39 memory walk. Malformed responses and broader source-reuse stress remain |

### Queue, ordering, and control points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| LQ/SQ occupancy | empty/near-full/full, wrap flags, simultaneous enqueue/dequeue, same-slot reuse | Implemented/partial |
| ROB age | ordinary and wrapped pointers, flag transitions, same-cycle issue/commit/redirect | Partial |
| Store-to-load forwarding | scalar-scalar, vector-vector, scalar-vector, vector-scalar, partial overlap, byte masks, older/younger stores | Implemented for modeled classes |
| Exception priority | multiple legal faults, differing ROB/uop vs LQ/SQ age, load/store/vector competition | Partial; wrapped ROB age deliberately disagrees with LQ/SQ order for simultaneous scalar load/store faults, the retained load/store selector is switched while both buffers hold scalar or vector load exceptions in both source-arrival orders, a same-ROB LMUL=2 vector pair checks `uopIdx`, page and PBMT-NC-misaligned faults are co-issued, and scalar/vector page faults replace each other in both arrival directions. Additional cause pairs and same-operation multi-cause priority remain planned |
| Redirect/recovery | kill each producer class, in-flight miss/replay, canceled prefetch, pointer reuse, survivor data | Implemented for basic scalar/vector LSQ redirect. Direct active-VSegment redirect is excluded because full-core serialization prevents that state; broader simultaneous producer-class arbitration remains |
| Fence/commit ordering | outstanding cache/uncache/PTW traffic across commit and fence boundaries | Partial; `sbuffer-flush` commits a cacheable store while a PBMT-NC store response is delayed 1,024 cycles, pulses the backend-decoded direct SBuffer flush, requires both paths to drain, and checks exact readback. `translation-fence-all` holds targeted PTW responses across global/selective stage-appropriate fences and rejects stale completion. Both `cmo-contracts` and schema-13-and-later `random-mixed` prove a younger cold DCache miss can overlap delayed CBOAck and is then canceled without writeback by CMO `flushPipe`; the random path varies operation, line state, translation context, error state, and both response delays. MemBlock exposes no complete FENCE/FENCE.I instruction transaction, so their decode/ROB semantics remain integration-owned; wider simultaneous manager crosses remain |
| Backpressure | every producer/consumer ready-low pattern, long stalls, alternating stalls, response delay cross-product | Implemented for DCache/PTW/uncache and Probe responses; DCache negative responses now cross a fixed 256-cycle interbeat gap and concurrent MSHRs, while wider randomized negative-error timing remains planned |
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
| DCache A/B/C/D/E TileLink (53 ports) | Implemented | A/C/D refill, error, eviction, ReleaseData, and CMO paths plus deterministic B Probe and E GrantAck are checked. `dcache-coherence` accepts two distinct B source IDs before an unrelated cold miss writes back, requires accepted Probe depth >= 2, and address-matches both C responses; C-source remains an independent WritebackQueue ID. `cmo-contracts` observes CLEAN/FLUSH/INVAL and CBOAck at this boundary, including every clean/dirty permission transition, a simultaneous younger MSHR, flush cancellation, and all error cases. Schema 33 `random-mixed` constrains dirty toB/toN Probes, requested/mandatory data, and all one-through-eight-source bursts with exact active-source, response-address, delayed-refill, accepted-depth, and 32-bin cross checks. Schema 34 derives all 64 B-source IDs from the generated port and checks accepted uniqueness, reuse only after completion, and every namespace wrap. Schema 36 repeats one-through-eight-source bursts during every successful CMO operation/state class. Schema 37 closes zero-through-eight Probe depth for every successful atomic family/width; nonzero depth includes a held cold D response and exact result. Schemas 17/18 add scalar-load and atomic denied/corrupt refill conservation. Schema 35 drives 9/10 fresh clean or dirty tags through one, two, three, or four selected eight-way sets, attributes clean Release and dirty ReleaseData by target address, checks every data-bearing C transaction, can hold four address-qualified refills while later D sources and target releases progress, and stalls one target C transaction per action for 16 valid cycles with a full-payload stability oracle. GrantAck is matched by sink rather than arrival order |
| PTW and Uncache manager boundaries (41 ports) | Partial | Legal traffic, long latency, and backpressure are implemented. PTW denied/corrupt responses cover every translation regime, both data beats, fault cutoff, cache suppression, and post-fence recovery. Denied/corrupt Uncache stores require exact exception metadata plus one external error report at the aligned physical line address. Add malformed/duplicate/early/late responses as explicit negative protocol modes rather than normal workload traffic |
| IFU-to-Mem PTW request/response (`io_fetch_to_mem_itlb_*`, 64 ports) | Implemented | `ifetch-ptw-bridge` drives raw IFU PTW requests for valid/faulting Sv39/Sv48, all four nested pairs, both VS-only/G-only mode variants, PBMT=NC/IO, stage-1 and G-stage leaf/non-leaf `G` inputs, and all four nested pairs crossed with VS-leaf, final-G-leaf, and implicit page-table G-stage faults while forcing response backpressure and requiring cold page-table traffic. `G=0` cannot be promoted; stage-1 global mappings may be conservatively demoted, and G-stage `G` cannot change the functional result. A 256-cycle IFU root-PTE delay overlaps a cold scalar DTLB walk and requires both translations, exact load writeback, and PTW outstanding depth >= 2. Two same-VPN IFU requests are accepted before the first response and must return twice while issuing only one three-level Sv39 memory walk. Delayed requests are invalidated by context switches and global/selective stage-appropriate fences; no stale response may escape, and the subsequent walk must return the independently computed replacement mapping |
| Backend `memoryViolation` (8), `ldCancel` (3), and wakeup (12) outputs | Partial, high priority | `load-feedback` checks lane/destination metadata and records wakeup/cancel behavior for cold, resident, same-bank, page-fault, PMP-denied, MMIO, NC, and forwarding-data-wait loads. Page/PMP faults still require exact architectural exception, no forbidden data-manager request, and queue cleanup; MMIO/NC require their independent Uncache contract. DCache denied/corrupt, G-stage guest faults, and guarded fixed-PMA denial apply their own terminal oracles. `random-mixed` records feedback per lane but does not require a count relation or a prefetch-free window. `memory-violation` and `rar-violation` check all eight exposed output fields. `pmp-contracts` additionally checks terminal load/store/AMO access faults |
| ROB/LSQ pending boundary (`pendingMMIOld`, `pendingst`, `pendingPtr`) | Implemented for elaborated fields | MMIO retirement holds `pendingMMIOld` with the matching ROB pointer until Uncache completion; replaying misaligned stores hold `pendingst` and the same pointer across retries. `pendingld`, `pendingVst`, and `pendingPtrNext` are source-level bundle members but are pruned from this generated MemBlock top, so no functional claim is made for nonexistent pins |
| WFI request/safe boundary (2 ports) | Implemented | `wfi-safety` requires `wfiSafe=0` while independently delayed DCache, PTW, or Uncache manager work is outstanding, then requires assertion only after each manager drains; deasserting `wfiReq` clears the safe indication |
| Top-down miss/pressure boundary (8 ports) | Implemented | `topdown-contracts` checks exact one-cycle L2/L3 input propagation, L1 miss and replay allocation during a delayed cold load, StoreQueue full after 56 accepted unissued stores, and SBuffer full after 16 distinct committed lines are held behind a delayed refill. The monitor remains active in every functional scenario |
| Direct maintenance/configuration controls | Partial | Focused tests cover direct SBuffer flush, timeout-driven eviction, Uncache outstanding enable/disable behavior, MBMC BME/CMODE/BCLEAR/BMA bitmap policy, architectural prefetch-control defaults, guarded DebugModule PMA access, all outer-L2 flush enable/completion combinations, and the simple top-level control/metadata bridges. L2 enable, hart ID, power-down, MSI ack, and frontend reset are checked combinationally; L2 completion, reset vector, halt, critical error, all seven external-interrupt outputs, MSI-info, CLINT, I-cache BEU, and 67 shared perf-event lanes are checked at exact one-cycle latency; all six L2-prefetch-control fields are checked at two cycles. These oracles run on every monitored functional-test cycle. Dynamic reprogramming during arbitrary overlapping traffic and the remaining CSR cross-product remain |
| IFU software instruction-prefetch outputs (`io_ifetchPrefetch_*`, 6 ports) | Implemented | `ifetch-prefetch` checks lane and virtual address for all three LoadUnit outputs, rejects data-prefetch false positives, and issues three unmapped requests together under active Sv39 while requiring exact per-lane VAs and zero PTW/DCache traffic. A separate same-cycle `prefetch.i/r/w` cross proves only the instruction operation emits this output while cold data hints drop on DTLB miss and warmed cacheable data hints reach DCache. Resident-TLB PBMT=NC data hints intentionally use DCache rather than Uncache, while PBMT=IO hints reach neither manager; neither class emits an IFU pulse. This matches LoadUnit's explicit `s0_tlb_no_query`; frontend owns subsequent instruction-prefetch translation/faults. `random-mixed` requires an observed `prefetch.i` output in every seed |
| L2/L3 prefetch sender outputs (5 ports) | Partial | `hardware-prefetch` is an implementation-characterization scenario: it records current source/address/count behavior and checks CSR suppression, but trigger depth, source selection, and exact algorithmic counts are not part of the reusable architectural oracle. `random-mixed` adds weighted fixed-PC stride-shaped traffic and reports sender observations without a per-seed source-12 PASS gate. Exact random-stream attribution and a positive L3-enabled configuration remain performance/integration work |
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
and uop conservation. Schema 42 now uses complete legal 1..8-uop vector load
and store instructions in every heterogeneous overlap window and closes all 16
reachable direction x addressing x single/multi-uop composition bins. The
focused `vector-issue-order` scenario independently checks a two-uop indexed-
unordered instruction in both forward and reverse issue order, plus an
eight-uop masked indexed-ordered instruction, using the same identity-matched
per-element data oracle. All six LSQ dispatch lanes now have passive
pre-edge acceptance and flow-count observation, including width histograms
used by the common random coverage gate. Redirect cancellation is likewise
sampled from the elaborated top after its defined latency and checked by the
Schema-2 queue-conservation gate. Schema 38 now crosses real same-set clean/dirty
replacement with one through eight independent held refills and address-qualified
C-channel backpressure, proves each target set releases while all selected
refills remain pending, and checks every target payload transition during a
forced stall. Schemas 43/45 separately reach two through sixteen outstanding
ordinary cold-line misses and the capacity-plus-one depth-17 case, with one-
through three-lane initial scalar issue, scalar-only/scalar+vector-load
composition, and exact external manager/architectural/queue conservation.
Depth 17 records that 16 responses were held, one fair response release was
scheduled, and the overflow target was externally requested for every selected
action. Internal reject/replay state remains diagnostic only. The next DCache
breadth closure is replacement composition with Probe/CMO/atomic traffic.
Malformed manager responses
remain an explicit agent/SVA negative-protocol qualification task rather than
normal CPU workload traffic. Translation mode depth itself is no
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
| DCache manager | Coherent 64-byte lines on a 256-bit bus; finite randomized delay | Agent drives and observes A/B/C/D/E traffic, distinguishes Get/AcquireBlock/AcquirePerm/CLEAN/FLUSH/INVAL, checks ProbeAck(Data), Release/ReleaseData, GrantAck, and CBOAck, and applies A/C/D/E timing pressure. Random CMO varies operation, line state, overlap, locality, translation, latency, and Ack errors. Schema 33 Probe traffic queues one through eight distinct active B sources while an unrelated refill is delayed, holds C until every selected B request is accepted, and requires matching accepted-outstanding depth. The capacity is tracked as `queue.dcache_probe_entries=8` and checked against the XiangShan DCache parameters. Schema 34 also tracks `tilelink.dcache_probe_source_bits=6`, forbids active-source reuse, and validates the exact accepted unique/reuse/wrap lifecycle. Schemas 17/18 add address-qualified load and atomic errors. Schema 28 adds phase-local compact C observation and address-qualified response holding; schema 38 exercises that hold queue with as many as eight complete D transactions, retains in-progress multibeat ReleaseData in the idle contract, and checks its ceiling against `queue.dcache_miss_entries=16` from the standard configuration. Schema 29 adds a target-line release-ready window that bypasses unrelated C traffic, forces 16 valid stall cycles on one selected target transaction, and compares opcode, param, size, source, address, user metadata, echo, corrupt, and data throughout the stall. Schema 36 crosses every CMO operation and clean/dirty target state with one through eight accepted Probe sources while CBOAck is pending. Schema 37 crosses every successful atomic family/width with zero through eight auxiliary Probes, holding the target D response before writeback and applying C backpressure only after D is released so legal B backpressure is respected. Denied data responses also assert corrupt, while data-less Grant and CBOAck errors follow their operation-specific contracts. Concurrent multiple-CMO/source, replacement composition with Probe/CMO/atomic traffic, and malformed response modes remain planned |
| PTW manager | PTE memory and response source/size match the programmed roots | Reference page tables and PTW agent share sparse memory. `denied` is fixed over a multibeat response and implies `corrupt` on data; independent `corrupt` can select all, first, or last data beats. Stage-appropriate fencing is required before a clean retry of a cached access-fault result. Schema 19 makes target address, five walk sites, direction, level, mode pair, and legal error beat independently constrainable, with exact manager accounting. Malformed response validation remains planned |
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
- wider simultaneous multi-class/multiple-CMO ordering and full-core
  `fence`/`fence.i` integration (CBO.ZERO and
  CLEAN/FLUSH/INVAL functional/error paths plus constrained random operation,
  clean/dirty, address-context/locality, latency, and younger cold-load
  cancellation plus common constrained CMO denied/corrupt injection are
  covered; translation-fence ordering for global/selective
  leaf updates, both supported stage modes, and all four fully nested VS/G
  pairs are covered);
- low-rate segment FOF and redirect composition in the common constrained-
  random interface remains open; schema 44 ordinary VFOF closes first/later
  fault x Sv39/Sv48 x EEW and exact fix-VL/LQ conservation. All 78 legal ordinary unit-
  stride, strided, indexed-unordered, and indexed-ordered configurations, all
  338 legal segment NF/EEW/SEW/LMUL/EMUL configurations per addressing mode,
  all legal whole-register NF/EEW combinations, direct load/store/FOF redirect
  cancellation, and focused first- and later-element segment FOF faults are
  covered;
- broader fixed-PMA region/permission/edge crosses for HLV/HLVX/HSV beyond the
  covered `0x80000000` device-to-DDR transition,
  non-PMA PBMT/misalignment priority, plus PMP sizes/TOR/lock/permission/
  overlap-by-edge combinations beyond schema 26's 4-KiB NAPOT relation cross;
  all four nested translation mode pairs, directed and schema-24 random PBMT
  PMA/NC/IO priority, schema-25 random DDR/fixed-device classification, cacheable
  misaligned split paths, basic SPVP/VSUM/VMXR, stage faults, HLVX execute
  permission, the M-mode/SPVP physical unlocked/locked R/R+X/W PMP matrix, and
  one fixed PMA device class for all three families and both SPVP values are
  covered;
- architectural `satp`/`vsatp`/`hgatp` write/readback and WARL mode filtering;
  the MemBlock UT directly supplies the post-CSR `TlbCsrBundle` and therefore
  cannot establish software-visible Sv48/Sv48x4 enablement by itself;
- remaining instruction PMP permissions, broader hypervisor PMP matrices, and fixed-PMA
  cacheability, atomic, MMIO, overlap, and region-edge matrices; data-side
  TOR/NAPOT, lock, priority, R/W/AMO denial, and 4-KiB-grain WARL behavior are
  covered by `pmp-contracts`;
- hardware A/D updates and concurrent page-walk invalidation races (missing-A
  load/store faults, missing-D store faults, and
  SUM/MXR/VSUM/VMXR permission outcomes are covered by
  `translation-permissions`; global/selective single-walk races for both stage
  modes and all four fully nested pairs are covered by
  `translation-fence-all`);
- malformed TileLink coherence responses and replacement cross-operation Probe
  bursts,
  broader-mask/delayed/persistent/wider-concurrency physical ECC injection, and
  coherence error recovery (ordinary clean/dirty Probes, one-through-eight-source
  overlap with an unrelated miss, the complete 64-ID source lifecycle,
  one-through-eight-depth CMO and zero-through-eight-depth atomic composition,
  GrantAck,
  denied/corrupt load/atomic D responses,
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
