# MemBlock Oracle Catalog

This document defines how the MemBlock testbench decides whether a result is
correct. The oracle is derived from the transaction inputs, an independent
architectural reference model, and externally visible protocol events. It must
not be derived from the RTL implementation, an expected cycle number, or a
historical failing value.

## Oracle Rules

1. Compute expected architectural state before driving the DUT. The reference
   memory, page tables, permissions, masks, and instruction fields are owned by
   the testbench and are not read back from RTL state.
2. Compare externally visible effects after allowing any legal latency and
   ordering permitted by the interface contract. A cycle-by-cycle match is
   used only when the interface explicitly specifies a cycle relationship.
3. Match responses by architectural identity (ROB/uop, destination, queue
   identity, source, address, and operation), never by arrival order alone.
4. For stores, update the reference memory only at the modeled architectural
   commit point. A speculative request must not become visible merely because
   the DUT emitted a TileLink request.
5. For faults, calculate the faulting address and cause from the independent
   page-table/permission walk. Internal state is useful for diagnosing a
   mismatch, but cannot redefine the expected result.
6. A test passes only when the expected terminal effect occurs exactly once,
   no forbidden side effect occurs, all accepted work terminates or is
   explicitly canceled, and all ready/valid obligations hold.

The translation oracle follows the supported Kunminghu-v2 mode set, rather
than treating all page-based modes as interchangeable: `satp`/`vsatp` support
Bare, Sv39, and Sv48; `hgatp` supports Bare, Sv39x4, and Sv48x4. A nested
translation case is identified by its ordered `(vsatp.MODE, hgatp.MODE)` pair.
Sv57/Sv57x4 are outside this build's advertised capability and must not be
silently accepted by a test.

## Stable Oracles

| Domain | Independent calculation | DUT-observable check | Status |
| --- | --- | --- | --- |
| Scalar load data | Read bytes from pre-state sparse memory, then apply operation width and sign/zero extension | One matching scalar writeback with exact data, destination, ROB/uop identity, and exception bits; optional debug sidebands are compared only when the transaction owns a stable expectation | Implemented |
| Floating load data and side effects | Read bytes from pre-state sparse memory; NaN-box FLH/FLW to XLEN and preserve all FLD bits independently of the DUT. Derive memory faults from the translation/PMP/PBMT oracles | One matching FP-destination writeback with exact data on success and no integer-RF enable. Page, PMP access, permission, guest-page, misalignment, denied, and corrupt faults retain the exact cause while suppressing both RF enables; early faults issue no DCache/Uncache data request | Implemented for aligned and cacheable split FLH/FLW/FLD, PBMT=IO normal/error responses, and every listed MemBlock-observable early fault in `fp-loads` |
| Scalar load early wakeup | Use the accepted issue lane and its independently generated `rfWen`, `fpWen`, and `pdest`; classify cache residency only by absence of a new external TileLink request | Every normal load that reaches one terminal writeback has one more matching wakeup than lane-local `ld2Cancel` pulses; an exceptional load has no uncanceled wakeup. Replay count and exact cycle are deliberately unconstrained | Partial: `load-feedback` covers all three lanes, cold misses, resident hits, both destination classes, bank conflict, page fault, PMP denial, PBMT=IO/NC, and forwarding data wait. DCache denied/corrupt, G-stage guest faults, and fixed-PMA denial apply the same oracle in their owning scenarios; `random-mixed` requires canceled and uncanceled wakeups on every lane under mixed pressure. With hardware prefetch enabled, this gate uses a pre-training snapshot because prefetch cancel pulses intentionally have no backend wakeup; raw full-run counters are also recorded. `dcache-errors` adds resident one-shot tag ECC with representative single- and double-bit masks, nonzero wakeup/cancel, no terminal writeback, exact-address BEU reporting, immediate redirect, and clean LQ-slot reuse. In the default non-accurate-load-error mode, one-shot data ECC crosses all eight banks with single- and double-bit masks; every case has a final uncanceled wakeup and an independently predicted XOR-data writeback while still reporting BEU. A same-cycle two-lane case requires only the selected bank to be corrupted and reported, the companion bank to remain clean, both lanes to wake without cancellation, and clean survivors after automatic one-shot clear |
| Memory ordering violation | For RAW, allocate an older unresolved-address store and let younger same-byte loads complete before resolving the store; calculate age from the 160-entry circular ROB identity. For RAR, let a younger load complete before an older load, force the shared dirty line through DCache writeback/release, then issue the older load. ROB/FTQ/RVC metadata is independently chosen | Exactly one `memoryViolation` pulse identifies the oldest conflicting younger load with self-flush for RAW, or the older load with `flushAfter` for RAR; byte-disjoint same-line traffic must not redirect | Partial: exact scalar RAW/RAR, RAW non-overlap, three-candidate oldest selection, ROB wraparound, vector RAW participation, concurrent two-STA-source RAW arbitration, concurrent three-LoadUnit RAR arbitration, and all eight exposed output fields are checked; broader vector-store and mixed RAW/RAR source crosses remain |
| Memory trigger metadata | Program every `mem_trigger.tUpdate` slot, enable/load/store/select controls, EQ/GE/LT comparison, action, breakpoint gate, debug state, and chain relation independently | A qualifying scalar/vector load/store reports the exact trigger action, exception policy, and vector `vstart`; the targeted access does not reach DCache, while legal accesses before a later-element/field hit may produce an exactly counted prefix refill. Triggered store images remain unchanged. A disabled, mismatched, or debug-suppressed trigger permits a normal cold request and completion. Breakpoint suppresses scalar-load RF write; DebugMode carries `trigger=1` with no breakpoint exception, and only its architecturally irrelevant data field may be excluded from comparison | Partial: `trigger-contracts` executes 25 cases across all four slots, all three supported match types, both actions, five control suppressions, a chain hit/miss pair, aligned/misaligned scalar stores, and eight vector load/store cases spanning all four EEWs/addressing modes plus two segment cases. Broad randomized trigger/sideband crosses remain |
| Scalar store data | Apply byte enables and little-endian store bytes to a copy of reference memory at commit | Address/data issue plus strict exception/ROB/flush/debug checks and independent post-commit readback; MMIO stores also require the Uncache request path and SQ retirement | Implemented for modeled scalar stores; output-only data pulses remain filtered when identity is unavailable |
| Vector load data | Decode unit/strided/indexed addresses independently, including ordinary multi-uop unit-stride and strided `vuopIdx` base advancement, indexed data-SEW versus index-EEW sizing, `max(LMUL,EMUL)` uop count, index-register reuse, special-index split/Vd mapping, and whole-register 16-byte uop windows; apply global-element `vl`/`vstart`/mask slicing, tail/merge policy, and old destination. Whole-register EVL is derived independently from NF and EEW | Exact 128-bit writeback, active-element behavior, independent `vsew/veew/vlmul` metadata, operation encoding, and identity | Implemented for modeled vector memory operations, including all 78 legal ELEN=64 EEW/SEW/LMUL/EMUL configurations for unit-stride, alternating positive/negative strided, indexed-unordered, and indexed-ordered load/store/readback. Indexed directed aliases/non-monotonic addresses cross cache-line and Bare 4-KiB boundaries; the full indexed matrix adds 56 `EMUL>LMUL` configurations across both modes. All 16 NF 1/2/4/8 x EEW 8/16/32/64 whole-register combinations also run with an intentionally irrelevant input VL |
| Vector store data | Generate each active element's byte addresses and bytes from source data; apply mask, split, indexed permutation, indexed SEW/EEW and LMUL/EMUL mapping, and whole-register EVL/address rules | Active-byte readback, completion, replay progress, queue drain, RF write enables, flush, debug sidebands, indexed readback, and whole-register load readback are checked for modeled stores | Partial; ordered/unordered indexed stores and readbacks exhaust all 78 legal EEW/SEW/LMUL/EMUL configurations per mode, directed cases retain non-monotonic unique indices, and all 16 whole-register NF/EEW store combinations are byte-exact, while generic mixed stores remain limited by the output-only store boundary |
| Store forwarding | Overlay the youngest legal store bytes on the pre-state load bytes, per byte and per age rule | Scalar/vector load returns the byte-accurate overlay before store commit | Implemented |
| Stage-1 translation | Walk independently populated Bare/Sv39/Sv48 PTEs, checking canonical VA, valid/leaf, all supported leaf levels, superpage alignment, permissions, SUM/MXR, A/D, PBMT/N, and reserved-bit policy | Returned data comes from calculated PA, or exact access/page fault and fault VA; faulting accesses must not issue a DCache/Uncache data request, while PTW traffic is recorded but not forbidden | Partial: generic Sv39/Sv48 walker, Bare bypass, all leaf levels, high-half canonical and both noncanonical sign-extension directions, alignment faults, the U/S/SUM/MXR/A/D matrix, and 26 Sv39/Sv48 invalid/reserved/PBMT/NAPOT encodings are implemented for both scalar loads and stores; broader fault crosses remain |
| G-stage translation | Walk independently populated Sv39x4/Sv48x4 PTEs with a 16-KiB root, widened root index, 41/50-bit GPA checks, G-stage U-mode permission rule, PBMT/N/reserved encoding rules, and guest-page-fault semantics | Exact host PA or guest fault cause, fault VA/GPA, stage marker, and no data-manager request on a fault | Partial: generic 41/50-bit walker, Bare bypass, all leaf levels, high-GPA faults, nested load/store permission cases including G-stage `D=0`, all valid two-stage PBMT combinations, and 26 Sv39x4/Sv48x4 invalid/reserved/PBMT/NAPOT encodings are implemented for both scalar loads and stores |
| Nested translation | Compose independent VS-stage and G-stage walks for `Sv39->Sv39x4`, `Sv39->Sv48x4`, `Sv48->Sv39x4`, and `Sv48->Sv48x4`; model Bare degenerations separately | No stage skipped; exact host PA, stage-specific fault, VA/GPA metadata, implicit VS-page-table accesses, and VS-over-G PBMT priority | Partial: all four 4-KiB pairs, VS-only/G-only/fully-Bare degenerations, VSUM/VMXR/G-stage-MXR permission selection, VS/G context isolation, final-data G-stage encoding faults, and the 36-case PBMT composition matrix are executable; broader stage-only accesses remain |
| Data-side PMP/PMA | Hand-calculated TOR/NAPOT interval membership, first-match priority, lock, current/effective privilege, R/W/X access rules, and fixed SoC PMA classification | Exact load/store/AMO allow or access-fault result; ordinary denied accesses emit no DCache/Uncache request. A cacheable HLVX with R allowed but X denied currently exposes exactly one early DCache request before its physical-PMP exception cancels architectural use; no Uncache request or RF effect is allowed | Partial: `pmp-contracts` covers exact TOR/NAPOT edges, overlap priority, R/W and AMO denial, unlocked/locked M-mode behavior, lock immutability, and the 4-KiB-grain NA4-to-NAPOT WARL rule. `hypervisor-contracts` executes M-mode HLV/HLVX/HSV with SPVP=U/S across unlocked and locked permission regions and maps all three families to a fixed `c=0` PMA device window. `mmio-contracts` checks the fixed device-to-DDR boundary at `0x80000000`. Broader PMA class/permission/edge crosses remain |
| L2-to-L1 DTLB boundary | Drive the top-level `io_l2_tlb_req_req_*` request fields and observe the corresponding response/PMP fields | Accepted requests return a legal hit/miss response, preserve PBMT and fault semantics, and classify PMP/MMIO consistently; a miss is delegated to the external L2 TLB | Implemented for ordinary and prefetch miss responses in `l2-tlb-contracts`; this boundary has no refill response input, so translation-hit refill and external L2 retry remain outside MemBlock |
| L2 hint propagation | Independent valid/source-id/keyword stimulus at the top-level hint input | Registered hint delivery must not create an unsolicited writeback or queue/protocol violation when no matching MSHR exists | Implemented for all 16 source IDs and `isKeyword=0/1` idle pulses in `l2-tlb-contracts`; matching-MSHR replay behavior remains an L2 integration responsibility |
| Outer L2 flush bridge | Current-cycle `flush_l2_enable` and independent previous-cycle `l2_flush_done` | `outer_l2_flush_en` equals the enable combinationally and backend `l2FlushDone` equals the preceding completion input on every monitored cycle | Implemented continuously; `l2-flush-contracts` additionally covers all four input combinations, four enable transitions, and five completion transitions |
| Top-level control and metadata bridges | Independently generated hart/reset/power/halt/error, eight interrupt sinks, MSI acknowledgement, frontend-reset bypass, valid-gated MSI-info/CLINT, I-cache BEU metadata, hardware-counter events, and L2 prefetch control | Combinational paths match current inputs; registered metadata and each interrupt output match the exact one- or two-cycle input history on every monitored cycle, including `nmi_31 = nmi(0) OR beu_local`, with payload checked under its valid contract | Implemented continuously; `top-control-contracts` covers all eight power/halt/error combinations, eight hart/reset-vector patterns, all 256 interrupt input combinations, all 32 five-bit L2-prefetch enable combinations, all four MSI/CLINT valid combinations, zero/maximum delay, all 67 shared perf-event lanes, and all 64 event values. The standalone elaboration ties output event lane 0 to zero and prunes input 0/output 68, so only the 67 shared lanes carry an end-to-end claim |
| Trace bypass transport | Independently generated encoder feedback, common status/trap metadata, and three trace groups; update reference state only under the RTL's group-valid and trap-itype enables | Exact one-cycle outputs, valid-gated holds, 50-bit `iaddr + (ftqOffset << 1)` wraparound, and no non-trap overwrite of retained trap metadata on every monitored cycle | Implemented continuously; `trace-bridge-contracts` covers all eight group-valid combinations, all 16 itypes, all 16 FTQ offsets, all eight privilege encodings, four encoder states, both trap classes, and payload hold intervals. This is a bridge-transport oracle, not an architectural trace-content oracle |
| DFT bridge transport | Enumerate the ten standalone SRAM-broadcast/DFT-reset input bits while external functional reset is asserted | Every elaborated frontend/backend bridge output equals its corresponding current input for all 1,024 combinations; idle controls are restored before a fresh functional reset | Implemented in `dft-bridge-contracts` for ten frontend and four backend outputs. This proves MemBlock combinational routing only, not MBIST or physical SRAM behavior |
| Backend reset tree | Functional reset, DFT functional reset, DFT scan override, repeated pulse widths | `io_reset_backend` asserts asynchronously; functional release occurs after two three-stage ResetGen levels, DFT functional release after one level, external reset is isolated in DFT mode, and scan output directly follows active-low `lgc_rst_n` | Implemented in `reset-tree-contracts`; internal leaf resets remain outside the top-level oracle |
| Frontend bridge | Generate independent, indexed request/response streams for ICache, ICache-control, and instruction-Uncache; derive legal TileLink constants, byte masks, source IDs, source credits, and response beats without sampling bridge outputs | Per-path FIFO scoreboards compare every observable A/D field on every valid cycle, forbid same-source reuse before D completion, count request/response/credit stalls, reject loss/duplication/reordering, and require quiescence after drain | Implemented in `frontend-bridge` for all 88 frontend TileLink pins, simultaneous traffic, all source values, Get/PutFull/PutPartial, size/mask crosses, two-beat ICache responses, corrupt polarity, and randomized backpressure. `frontend-reset-recovery` cancels three buffered A requests plus one observably stalled ICache-control D response and requires three distinct post-reset survivors |
| DCache refill, partial progress, and same-line merge | Choose cold-load virtual addresses with bit 5 clear/set and build each 64-byte line independently in sparse memory. For merging, co-issue two offsets in one cold line before a deliberately delayed response. For partial progress, delay only the noncritical beat of a keyword refill | AcquireBlock echo must report one non-keyword and one keyword request; the manager returns the corresponding ordinary/critical-beat-first GrantData order, both exact load values complete, and each sink receives one GrantAck. The merged pair must return two exact values from exactly one external refill. The critical-beat load must write back while only one GrantData beat has fired, then receive the remaining beat without a duplicate writeback | Implemented in `single-load` |
| Translation context/fences | Include `satp/vsatp/hgatp` MODE/root changes, ASID/VMID tags, `V` transitions, `SFENCE.VMA`, `HFENCE.VVMA`, and `HFENCE.GVMA` scope | Updated translations observe only architecturally ordered page-table writes; stale contexts cannot alias | Implemented for modeled contexts: direct mode/root, host ASID/root, VS ASID/root, G-stage VMID/root, host/nested `V` transitions, global/selective fence leaf updates, same-ID root reuse, delayed-response races, distinct-page walks, and redirected root/ASID/VMID/MODE/`V` changes with an outstanding response span both stage-1 modes, both isolated VS/G modes, all four fully nested starting mode pairs, and both host/nested directions with distinct-data oracles |
| Misalignment | Concatenate/split bytes across 16-byte beats, cache lines, and pages; independently apply enabled/disabled policy | Exact value/bytes when legal, or exact address-misaligned cause when prohibited | Implemented for current scalar/vector cases. The scalar directed case additionally keeps three split loads pending ahead of 60 completed younger loads, then checks in-order misalign-buffer progress, all 63 exact writebacks, LQ drain, and no spurious RAR violation |
| Exceptions | Reference ISA cause plus circular ROB/uop age, independent of LQ/SQ allocation order | Cause bits, fault VA/GPA, marker, single terminal exception writeback, and oldest-fault address when several faults coexist | Partial: exact scalar/vector causes and metadata, concurrent scalar load/store exception-address selection across ROB wrap with deliberately disagreeing LQ/SQ order, same-ROB vector `uopIdx` replacement, simultaneous page-fault versus PBMT-NC-misaligned age selection, bidirectional scalar/vector retained-fault replacement, scalar-load/store and vector-load/store buffer selection in both arrival directions, data-side PMP load/store/AMO access faults, and HLV/HLVX/HSV physical PMP faults are implemented. One-shot physical tag/data ECC is executable, but default `EnableAccurateLoadError=false` makes data ECC an asynchronous BEU report rather than a synchronous load exception; the broader cross-cause/source, instruction-PMP, and ECC-priority matrices remain |
| Register side effects | Architectural rule for the instruction class, including no RF write on exceptional load and no RF write for prefetch | RF writeback present only when allowed, with exact destination/data | Implemented for modeled scalar/vector/prefetch paths |
| Prefetch | Prefetch has no architectural destination and does not require data return; translation policy is modeled separately. Only `prefetch.i`, not data read/write prefetch, is allowed to request an IFU-side instruction prefetch | Completion without RF write or load exception, with legal request behavior; exact lane and virtual address on `io_ifetchPrefetch_*` for `prefetch.i`, with no such pulse for data prefetch | Partial: all three classes and instruction-output lanes, same-cycle `i/r/w`, unmapped cold data-hint drop with no PTW/data request, warmed Sv39 data-prefetch hits, individual cacheable `r/w` DCache requests, and PBMT memory-type policy are executable. Ordinary PBMT=NC/IO loads first prove Uncache classification; resident-TLB NC `r/w` become DCache hints while IO `r/w` reach neither manager, matching the intentional RTL policy. Broader duplicate/same-line contention remains |
| Hardware data prefetch | For isolated fixed-PC stride, PC-independent spatial streams, and same-PC SMS/PHT regions, independently advance confidence/active-region state and calculate L2 targets from the RTL depth, width, and learned block pattern | Exact source/address/count beginning at confidence or active-region qualification; stream suppresses a simultaneously mature stride; disabled output gates suppress requests without preventing PHT training | Partial: exact positive source-12 stride, 12-line source-11 stream activation with 640-line lookahead and four-line width, stream-over-stride priority, and disabled negative controls are implemented. A source-10 phase trains offsets 0..11 with PHT output disabled, then requires six unique requests at offsets 5..10 in a new region after enable. `random-mixed` composes weighted fixed-PC stride streams with ordinary and corner traffic and gates source-12 observation; exact random-stream address attribution and an L3-enabled configuration remain |
| Cache refill | Sparse memory is authoritative for returned line bytes; line address and transfer size are decoded independently | Complete legal refill, no byte corruption, and eventual response consumption | Implemented |
| Dirty eviction | Reference memory tracks committed bytes; an independent immutable pre-eviction line image is captured before replacement | Correct Release/ReleaseData/ReleaseAck sequence and exact bytes for the captured pressure lines | Partial; the dedicated pressure phase is byte-checked, while general release coverage remains planned |
| Uncache/PBMT-NC | Ordered byte memory model with request size, mask, beat address, and source identity | Correct AccessAck/Data, legal size/address/mask, ordered store visibility, exact load data at every byte lane, denied/corrupt exception propagation, aligned external store-error address, scalar PBMT=IO store request/retirement, side-effecting device log, and CBO.ZERO line image | Partial; width/lane and denied/corrupt load/store response propagation are executable (`uncache-widths`, `uncache-errors`), including one external Uncache store-error pulse and no DCache error pulse; PBMT=IO direct three-cycle load metadata plus scalar store bypass/retirement, a physical non-DebugModule `c=0` PMA load/store pair, guarded DebugModule access-fault case, fixed device-to-DDR boundary loads, an eight-access error/side-effect sequence, queued same-class and mixed load/store device serialization, and cacheable CBO.ZERO line-zero/readback are executable (`mmio-contracts`, `cbo-zero-contracts`); malformed/duplicate/late response handling remains |
| Top-down status | Independent previous-cycle L2/L3 input state plus observable queue, buffer, miss, and replay pressure | Exact one-cycle L2/L3 propagation and nonzero causally triggered L1 miss, replay allocation, SQ-full, and SBuffer-full intervals | Implemented in `topdown-contracts`; the L2/L3 delay oracle is active on every monitored cycle |
| Ready/valid | Protocol definition: payload remains stable while `valid && !ready`; accepted items need identity-preserving disposition | Generated SVA checks producer stability; channel monitors validate response identity, legality, and D-channel error flags | Partial; DCache and Uncache denied/corrupt injection are executable, while malformed/duplicate/early/late PTW and Uncache responses remain planned |
| Queue conservation | Accepted LQ/SQ enqueue handshakes, architectural dequeues, and redirect events | `allocated = dequeued + canceled` at quiescence, with identity and flag preservation | Partial; current allocation/cancellation totals are driver-accounted and acceptance observation is planned; `reset-recovery` also checks explicit LQ cancellation for independently accepted DCache, PTW, and Uncache requests |
| Redirect | Independent ROB age comparison determines younger work to cancel | Younger work has no terminal writeback; older work retains its expected result; pointer reuse is legal | Partial; basic modeled redirect is checked, but cancellation is currently explicitly accounted by the driver |
| Progress | Fair manager model eventually accepts requests and returns legal responses | Every non-canceled accepted operation terminates before the scenario deadline | Partial; fairness is enforced by the test agent, while accepted-event accounting is being strengthened; `reset-recovery` checks that canceled DCache, PTW, and Uncache responses cannot leak into distinct post-reset work |
| Coverage | Coverage is a property of generated input/observed events, not a correctness result | Required class counts and mixed-overlap gates are nonzero and complete | Partial; several mixed counters are generator bookkeeping rather than independent monitors |

The outstanding-walk oracle follows the ratified
[RISC-V Supervisor specification](https://docs.riscv.org/reference/isa/v20260120/priv/supervisor.html):
a walk that sampled a PTE before a subsuming `SFENCE.VMA` cannot later install
that stale translation. The harness makes this observable without internal TLB
signals by snapshotting an old root-leaf response in the delayed PTW agent,
changing the PTE, fencing, and requiring a post-fence load to return data from a
different physical page. The old load is redirected, so it must produce no
terminal writeback. The ratified
[RISC-V Hypervisor specification](https://docs.riscv.org/reference/isa/v20260120/priv/hypervisor.html)
defines `HFENCE.VVMA` and `HFENCE.GVMA` as the corresponding fences for VS- and
G-stage structures; the same oracle is applied both with the other stage Bare
and with Sv39/Sv39x4 active together. Fully nested tests identify the exact VS
or final G PTE from accepted top-level PTW request address/size, not an assumed
internal walk sequence.

## Reference Model Details

### Byte memory

`SparseMemory` is a byte-addressed little-endian model. An expected load reads
the bytes at the calculated physical or uncache address from the pre-state
image. An expected store constructs a byte-enable mask and a post-state image;
unwritten bytes remain unchanged. Data patterns include zero, all ones,
alternating bits, byte ramps, random values, and values crossing every modeled
beat boundary.

### Scalar operations

The operation decoder maps each legal load/store encoding to its byte width and
signedness. The expected value is assembled from bytes and extended to XLEN
without consulting the DUT's internal decoder. The current response key
primarily uses ROB value/flag plus destination and operation fields available at
the boundary; queue/source/address identity is not always observable. A
duplicate or unknown identity is rejected where the monitor can distinguish it,
while unmatched output-only store-data pulses remain a documented residual
risk.

### Vector operations

The vector model independently calculates an element address for unit stride,
constant stride, indexed-unordered, and indexed-ordered operations. It then
applies `vl`, `vstart`, `vm`, the mask bits, EEW, tail/mask policy, and old
destination bytes. Stores use the same address decoder and only modify active
bytes. The model does not assume a particular split-buffer state machine or
replay count; it checks the final byte effect, legal replay progress, and
single architectural completion.

When a mixed window contains both a vector load and vector store, replay
feedback is matched to the originating transaction with its load/store queue
identity before reissue. Treating every replay as belonging to one arbitrarily
chosen vector operation is a testbench error, not evidence of an RTL failure.

### Translation and faults

The page-table builder must write known PTEs into sparse memory and retain the
root, mode, ASID, VMID, and privilege context independently from the DUT. The
reference walk is parameterized by level count and root-index width:

| Walk | Levels | Root alignment | Incoming address | Leaf sizes |
| --- | ---: | ---: | --- | --- |
| Sv39 | 3 | 4 KiB | 39-bit canonical VA | 4 KiB, 2 MiB, 1 GiB |
| Sv48 | 4 | 4 KiB | 48-bit canonical VA | 4 KiB, 2 MiB, 1 GiB, 512 GiB |
| Sv39x4 | 3 | 16 KiB | 41-bit zero-extended GPA | 4 KiB, 2 MiB, 1 GiB |
| Sv48x4 | 4 | 16 KiB | 50-bit zero-extended GPA | 4 KiB, 2 MiB, 1 GiB, 512 GiB |

At every level the model checks `V`, the illegal `W=1,R=0` combination, leaf
alignment, reserved/PBMT/N bits, and the access-specific R/W/X/U/G/A/D policy.
For two-stage translation it first translates the guest virtual address using
`vsatp`; every page-table memory access made by that walk is then translated by
`hgatp`; the final guest physical address is translated by G-stage as well.
G-stage permission checks treat accesses as U-mode accesses, while VS-stage
`MXR`/`SUM` do not override G-stage protection. A G-stage failure is a
guest-page fault, not a stage-1 page fault. Bare/one-stage and stage-2-only
paths are separate cases and must not be inferred from a successful nested
case.

For Svpbmt, a nonzero G-stage PBMT first overrides the physical memory
attribute. A nonzero VS-stage PBMT then overrides that intermediate result, so
the independent composition is `VS != PMA ? VS : G`. `translation-pbmt`
exhaustively crosses PMA, NC, and IO at both stages for all four supported mode
pairs. The oracle distinguishes the final type through DCache/Uncache routing,
IO commit gating, exact data, and store readback rather than reading a DUT TLB
entry.

The constrained-random tail maintains an explicit translation context separate
from the DUT. Context choices cover Bare, host Sv39/Sv48, and the four nested
`VS={Sv39,Sv48}` x `G={Sv39x4,Sv48x4}` pairs. A context switch is legal only
after all scoreboards and LSQ entries drain. Each translated window compares
the external PTW request count before and after the operation, recording a cold
walk or a reuse observation; the returned data and independent page-table walk
remain the correctness oracle. Generated `SFENCE.VMA`, `HFENCE.VVMA`, and
`HFENCE.GVMA` operations are restricted to compatible active contexts and use
independent global/selective coverage points. NC and MMIO random actions are
restricted to translated PBMT contexts because this UT boundary has no
programmable PMA-region input.

The model returns a physical address or a structured fault containing the
first failing stage/level, exact faulting VA, guest physical address of an
implicit page-table access when applicable, high-bit/canonicality reason, and
access type. For a VS-non-leaf fault, the reported vector VA is the first
active element selected by `vstart` and masking, while the GPA is the guest
physical address of the failing page-table access; it is not adjusted by a
vector element offset. This rule is architectural and is deliberately tested
with randomized vector width, mask, and offset.

### Forwarding and commit

Forwarding is modeled as a byte overlay ordered by the legal age relation. It
does not require the DUT to expose a forwarding hit signal. A store becomes
visible to the reference memory only after the testbench's legal commit
operation succeeds. This separates speculative cache traffic from architectural
state and catches stale, partial, and wrong-half store data.

## Temporal and Protocol Oracles

Latency, queue arbitration, and manager response order are intentionally
nondeterministic within the legal contract. The following are checked without
over-constraining implementation timing:

- a producer holds all payload bits and sideband fields while stalled;
- every independently observed accepted request receives at most one matching
  response;
- a response is not emitted for a redirected/canceled identity where the
  current monitor can observe that identity;
- the current agents drive only supported producer-side TileLink combinations;
  D-channel response opcode/source/size/sink/data and denied/corrupt fields are
  checked on every handshake, while full E-channel probe/coherence behavior is
  still planned;
- release data is captured by line, beat, source, and byte contents, but a
  separate immutable line snapshot is required before claiming full integrity;
- PTW and uncache request/response identity and denied/corrupt legality are
  partially modeled; malformed response injection is planned;
- an accepted non-canceled operation eventually completes under a fair agent;
- queue pointers may wrap, but identity flags and architectural age remain
  unambiguous.

The oracle never requires a particular number of retries, a particular cache
bank, an internal FSM state, or a fixed cycle count unless the public protocol
requires it.

## Planned Oracles for Current Boundary Gaps

These are required before the corresponding scenarios can be reported as
functionally verified:

| Gap | Required independent oracle | Required observations |
| --- | --- | --- |
| Atomics/LR-SC/AMO | Reservation and atomic-memory model with success/failure, alignment, ordering, and optional bus error injection | Partial: all exposed W/D-width AMO ALU variants, AMOCAS.W/D compare success/failure, LR/SC old-value/writeback, success/failure, cache visibility, the complete 24-opcode x every-illegal-byte-offset alignment matrix, and fixed-PMA atomic denial are executable in `atomic-contracts`. The 120 alignment cases distinguish LR load-misaligned from SC/AMO/AMOCAS store-misaligned exceptions, suppress RF write, forbid new DCache traffic, and cross the ROB pointer wrap. The PMA case requires `StoreAccessFault`, suppressed `rfWen`, no manager request, and unchanged backing memory. `atomic-dchannel-errors` covers the complete refill-capable W/D opcode x denied/corrupt matrix, persistent poisoned-line exceptions, exact request counts, and clean error-lifetime recovery. Exceptional data and internal reservation state are not used as oracles; cross-hart reservation interference and concurrent ordering remain |
| CBO/CMO/fence | Line-state model and explicit ordering/flush points | Correct invalidate/clean/zero effect, required completion, and ordering relative to loads/stores | Partial; cacheable CBO.ZERO's StoreQueue/SBuffer line-zero and readback are executable (`cbo-zero-contracts`); CMO CLEAN/FLUSH/INVAL, fence.i, and full ordering remain because the CMO response channel is internal to DCache and absent from the MemBlock top-level harness |
| VSegment/VFOF | Segment-element address/data model across multiple uops, fault-only-first truncation, per-segment mask, whole-register EVL/addressing, and ROB redirect cancellation | Partial: multi-uop identity, takeover/ready behavior, unit-stride/strided/indexed-unordered/indexed-ordered field offsets, store readback, later-element FOF truncation, first-element architectural fault/fault-VA preservation, exact fix-VL, load/store redirect, survivor reuse, FOF data/fix-VL cancellation, all 16 legal whole-register NF/EEW load/store/readback combinations, and all 78 legal ordinary unit-stride, strided, indexed-unordered, and indexed-ordered EEW/SEW/LMUL/EMUL configurations are executable; segmented LMUL/EMUL crosses remain |
| HLV/HLVX/HSV | Privilege/virtualization/PMP/PMA reference model including SPVP and execute permission | Effective mode, permission cause, final PA, RF/store side effect, and exception metadata. All three operation families execute under every Sv39/Sv48 and Sv39x4/Sv48x4 pair; a five-case PBMT basis covers final PMA/NC/IO and VS-over-G priority, cacheable misaligned HLV.D/HLVX.WU/HSV.D use exact split-data/readback oracles, M-mode execution crosses SPVP=U/S with physical PMP R/X/RW/RX plus locked R/RWX permissions, and a fixed PMA device target checks HLV/HSV Uncache plus HLVX execute denial. Broader PMA and hypervisor PMP region-edge crosses remain |
| Sv48 stage-1 | Four-level independent walk with canonical VA checks, L3/L2/L1/L0 leaves, 512-GiB alignment, and stage-1 permission rules | Fourth-level PTW activity and exact PA/page fault; no fallback to a three-level Sv39 walk. The 4-KiB and superpage data paths, high-half canonical VA, and both noncanonical sign-extension directions are implemented; broader fault crosses remain |
| Sv48x4 G-stage | Four-level independent G-stage walk with 16-KiB root, 50-bit GPA checks, and guest-page-fault semantics | Exact host PA or G-stage fault/GPA; no fallback to Sv39x4 indexing. The 4-KiB and superpage data paths are implemented; executed overflow fault remains |
| Nested mode cross | Independent composition of all four VS/G-stage pairs plus Bare degenerations and stage-2-only accesses | Correct mode pair, ASID/VMID isolation, stage-specific fault, and no stale translation after a mode change. All four 4-KiB pairs, Bare degenerations, context isolation, same-ID fenced root reuse, and redirected root/ASID/VMID/MODE/`V` changes with an outstanding walk are implemented |
| Translation fences | Independent page-table update model for `SFENCE.VMA`, `HFENCE.VVMA`, and `HFENCE.GVMA`, including selective/global scope and responses already accepted by the PTW manager | Updates become visible only after the architecturally required fence and only to matching context; global/selective leaf updates, targeted host-ASID/VS-ASID/VMID root reuse, both stage-1 and isolated VS/G modes, all four fully nested mode pairs, delayed stale-response races, and distinct-page co-issued walks are implemented |
| Remaining PMP/PMA matrix | Extend the current hand-calculated TOR/NAPOT cases with broader fixed-PMA class/permission boundaries and hypervisor region edges | Access allowed/denied, cause, constrained manager requests, and exact region boundary behavior. Data-side TOR/NAPOT, R/W/AMO denial, lock/priority, 4-KiB-grain NA4 WARL behavior, M-mode HLV/HLVX/HSV using SPVP-selected unlocked and locked PMP R/X/W permissions, one fixed device PMA target, the device-to-DDR cacheability edge, and device atomic denial are executable |
| Probes/coherence errors | TileLink probe/ack/data state model and corrupt/denied response semantics | Probe response legality, line state, replay/error propagation, and no lost dirty bytes |
| Remaining cross-cause/source exception competition | Reference architectural cause priority plus ROB/uop age relation independent of LQ/SQ index | Partial: wrapped ROB ordering, same-ROB vector `uopIdx`, simultaneous page-fault/PBMT-NC-misaligned age selection, both scalar/vector retained-fault replacement directions, and vector-load/store exception-buffer selection under both arrival orders are executable; additional cause pairs and same-operation multi-cause priority remain |
| Four-state/X behavior | Four-state simulator or formal/X-aware checker | Unknown propagation and reset initialization; Verilator two-state results are insufficient |

Until these models and observations exist, the scenarios remain explicit
boundary gaps. Pin toggling, an internal signal match, or a passing smoke test
does not close them.

## Anti-Oracles

The following are not correctness oracles:

- matching a historical buggy value such as a known `+8` GPA;
- expecting a specific cycle, arbitration winner, replay count, or cache bank;
- treating a TileLink request as proof that a store committed;
- treating a warm TLB/cache hit as proof of translation/data correctness;
- using the DUT's own translated address, mask, or decoded operation as the
  expected value;
- accepting a response because its data matches while its identity is wrong;
- counting reset-held pin toggles as semantic instruction coverage.

## Implementation Limitations

The current harness has several checks that are intentionally weaker than the
contract language above. `enqueue_*` counters increment after a driver tick and
do not observe a dedicated top-level allocation handshake. Redirect scenarios
explicitly account for expected cancellations because the MemBlock boundary
does not expose a cancellation event. The scalar store writeback monitor must
also tolerate the output-only `writebackStd` interface and therefore filters
some unmatched pulses; this is a known residual risk until an issue-epoch
channel is exposed or a stronger monitor is added. The uncache/PTW/DCache agents
currently synthesize legal responses from captured requests rather than
injecting and independently checking all response fields. Mixed overlap,
forwarding-direction, and dirty-pressure counters include generator-side
bookkeeping; they are not by themselves proof that the corresponding RTL event
occurred.

The streaming verifier script is included in the campaign controller hash set;
changing its acceptance logic invalidates the artifact. Boundary-hunt remains a
diagnostic campaign and does not substitute for the full mixed-run verifier.

These limitations are why the status column uses `Partial` and why the planned
oracle rows cannot be closed by another long green run alone.

## Failure Classification

When an oracle fails, the runner records the seed, transaction prefix, phase,
identity fields, reference pre/post memory digest, translation walk, manager
transcript, backpressure seed, and frozen artifact hashes. Reduction must
preserve the architectural mismatch while removing unrelated traffic. Only
after the independent model and transcript are checked is the failure
classified as an RTL candidate.
