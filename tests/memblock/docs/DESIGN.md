# MemBlock Design and Verification Boundary

## Reading this document

This document describes the MemBlock implemented by this repository and the
boundary presented to its functional unit test. It is a design map, not a claim
that every described behavior is verified. [IO_COVERAGE.md](IO_COVERAGE.md) maps
pins to stimulus and checks; [ORACLES.md](ORACLES.md) defines expected behavior;
[VERIFICATION_PLAN.md](VERIFICATION_PLAN.md) owns verification points and acceptance.

The implementation starts in
[MemBlock.scala](../../../src/main/scala/xiangshan/mem/MemBlock.scala):
MemBlock wraps MemBlockInlined, and MemBlockImp connects its cloned IO and
performance IO to the inner implementation. MemBlockInlined creates the lazy
cache, uncache, PTW, frontend bridge, TileLink buffers, and interrupt nodes;
MemBlockInlinedImp creates and connects the execution and queue logic.

Scala bundles describe a configurable superset. The actual flattened boundary is
[expected_ports.json](../config/expected_ports.json), grouped and dimensioned
by [memblock.json](../config/memblock.json). The generated boundary has three
scalar load lanes, two store-address lanes, two store-data lanes, two shared
vector-memory lanes, and six LSQ dispatch lanes. It exposes 256-bit DCache/PTW
memory beats and 64-bit uncache beats. It has no hybrid issue lanes, writeback
ready pins, or standalone io_mem_to_ooo_lsqio_vstart/vl pins. Vector writeback
metadata does contain vstart and vl. Do not infer an accessible input or an
observable output from a Scala field that is absent from the manifest.

## What MemBlock does

MemBlock executes backend scalar and vector memory operations, performs address
translation and protection, tracks speculative memory work, forwards older
stores to younger loads, accesses L1 DCache or uncached devices, and reports
writeback, exceptions, replay, and memory-order violations. It also carries
frontend memory traffic and several tile/backend control paths across this
boundary. The UT supplies the missing backend, frontend, memory managers,
page-table contents, interrupt sources, and control producers.

The principal internal owners are:

| Owner | Role and connection |
| --- | --- |
| [LoadUnit](../../../src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala) | Forms scalar/vector-flow addresses; queries DTLB, PMP, DCache and forwarding; produces data, replay/queue updates, wakeup/cancel and prefetch training. |
| [StoreUnit](../../../src/main/scala/xiangshan/mem/pipeline/StoreUnit.scala) and MemExeUnit store-data units | Address/permission processing and independent data delivery into SQ; address-side writeback and slow issue feedback. |
| [LsqWrapper](../../../src/main/scala/xiangshan/mem/lsqueue/LSQWrapper.scala) | Connects load and store queues, dispatch, ROB state, replay, forwarding, violation redirects, exception records and uncached work. |
| [Sbuffer](../../../src/main/scala/xiangshan/mem/sbuffer/Sbuffer.scala) | Receives committed SQ entries, merges buffered bytes, supplies load forwarding, and drains to DCache; owns replacement/flush/timeout behavior. |
| [DCacheWrapper](../../../src/main/scala/xiangshan/cache/dcache/DCacheWrapper.scala) | L1 load/store/atomic/CMO service and coherence, including miss, probe and writeback machinery. |
| [Uncache](../../../src/main/scala/xiangshan/cache/dcache/Uncache.scala) | Serial or configured outstanding uncached transactions, flush state and bus-error reporting. |
| [AtomicsUnit](../../../src/main/scala/xiangshan/mem/pipeline/AtomicsUnit.scala) | LR/SC/AMO path, store-buffer drain, translation/protection, DCache atomic access and scalar result. |
| [VSplit.scala](../../../src/main/scala/xiangshan/mem/vector/VSplit.scala), vector merge buffers, [VSegmentUnit](../../../src/main/scala/xiangshan/mem/vector/VSegmentUnit.scala), [VfofBuffer](../../../src/main/scala/xiangshan/mem/vector/VfofBuffer.scala) | Split vector uops into element/flow work, merge results, execute segments and generate fault-only-first VL updates. |
| [LoadMisalignBuffer](../../../src/main/scala/xiangshan/mem/lsqueue/LoadMisalignBuffer.scala), [StoreMisalignBuffer](../../../src/main/scala/xiangshan/mem/lsqueue/StoreMisalignBuffer.scala) | Coordinate split accesses, replay and exceptional partial progress. |
| [TLBNonBlock](../../../src/main/scala/xiangshan/cache/mmu/TLB.scala), [L2TLB](../../../src/main/scala/xiangshan/cache/mmu/L2TLB.scala), [Repeater.scala](../../../src/main/scala/xiangshan/cache/mmu/Repeater.scala), PMP/PMPChecker | Load/store/prefetch translation, PTW filtering/repeating, page-table reads, PMP/PMA checks and fence/context handling. |
| FrontendBridge, ICacheBuffer, ICacheCtrlBuffer, InstrUncacheBuffer in MemBlock.scala | Buffered transport of frontend instruction-fetch, cache-control and instruction-MMIO A/D channels. |

### Scalar load transaction

1. Backend dispatch allocates LSQ state with enqLsq, including ROB/LQ/SQ
   identities and operation metadata. This is distinct from execution issue.
2. issueLda supplies a uop, source base and immediate to a ready load lane.
   LoadUnit computes the access address, translates it and checks protection.
3. Eligible bytes may come from older SQ/Sbuffer entries, DCache hit data or
   refill forwarding. A miss, dependency, conflict or translation condition can
   defer/replay work; acceptance is not completion.
4. Load-queue machinery tracks completion, replay and ordering. A conflicting
   store or release-related ordering condition can produce memoryViolation.
5. The result returns on writebackLda with identity, write-enable, exception,
   trigger, replay and class metadata. Early wakeup and ldCancel signals have
   separate purposes and are not architectural completions. Atomics and split
   loads arbitrate onto scalar writeback ports in MemBlockInlinedImp.

### Scalar store, atomic and maintenance transaction

A store allocates SQ state and issues address and data independently through
issueSta and issueStd. The address path supplies translation, byte mask,
protection and exception information; the data path supplies the operand. Their
writebacks do not establish that the bytes are globally visible. Backend
scommit and pending-head controls authorize progress. Committed cacheable bytes
pass through Sbuffer into DCache; uncached/device stores use the uncache path.
Loads may forward bytes before the store reaches external memory.

Atomic function types on the store issue path select AtomicsUnit; the unit
collects the matching data, drains buffered stores, performs translation and the
DCache operation, and returns through scalar load writeback. The normal store
pipeline is suppressed for the selected atomic input. Correct operand counts,
uop indices and serialization belong to the backend contract; a general Scala
operation decoder does not establish that the UT generates every atomic shape.

flushSb, the CMO path, atomics and segments feed the store-buffer/uncache flush
logic. sbIsEmpty reports the registered combination of those drain states.
CBO.ZERO and cache block management are distinct operations: the former writes
zero bytes; the latter uses the cache-management path and external response.

### Vector transaction

Both vector loads and vector stores enter issueVldu. Function type/opcode,
SEW/EEW, LMUL, VL, VSTART, VM/mask, field count, uop index/last-uop state and source
operands determine the legal shape. Ordinary loads and stores select VLSplitImp
and VSSplitImp; their flows reuse scalar memory pipelines and return through
load/store merge buffers. Segment operations select VSegmentUnit from the head
issue lane and return on vector lane 0. Fault-only-first processing can produce
both data and a separate fix-VL writeback; it is not one completion per input
instruction regardless of uop shape.

vstuIqFeedback can request a partial replay with queue identity, replay mask and
merge-buffer index. The UT must reissue the corresponding transaction, preserving
that feedback state. Returned bytes, write mask, destination index and policy
metadata together describe the result consumed by backend merge. Exceptional
partial progress is a separate contract from ordinary successful completion.

### Translation and protection transaction

tlbCsr supplies SATP/VSATP/HGATP mode, root and context identifiers, privilege,
virtualization, permission, pointer-masking, PBMT and bitmap controls. sfence
supplies selective/global and virtualization fence information. MemBlock delays
these controls before distributing them to DTLBs/PTW. Load, store and prefetch
DTLBs feed a filtered shared data-PTW path; frontend ITLB requests use a separate
PTW repeater into the same L2TLB wrapper. PTW memory reads leave through the PTW
TileLink port. Distributed CSR writes program protection/control state, including
PMP, independently of page-table construction.

The external l2_tlb_req port uses a prefetch-side DTLB requestor and returns a
translation plus l2_pmp_resp. Its generated boundary has no request-ready or
response-ready pin; the UT issues a one-cycle valid request and waits for the
response. IFetch PTW instead retains request-ready and response-ready and can
exercise response backpressure. Neither path models an external L2's request
scheduling or retry policy.

## Semantic interface ownership and legal inputs

The exact prefixes for every retained pin family are in
[IO_COVERAGE.md](IO_COVERAGE.md#pin-family-stimulus-and-check-map). This table
explains why those families exist and which producer owns their inputs.

| Semantic family | Input owner and legal use | Observable consequence |
| --- | --- | --- |
| Clock, functional reset, reset-tree/DFT controls | Test environment supplies edges and the selected reset mode; reset invalidates both DUT work and the modeled manager link state. DFT SRAM routing tests run as routing tests, not as ordinary memory operations. | Backend reset, frontend/backend DFT projections and clean post-reset traffic. |
| LSQ dispatch and ROB/pending controls | Backend model reserves capacity, allocates unique live circular identities, dispatches in order and commits only eligible work. needAlloc and vector element counts agree with the instruction shape. | Dequeue/cancellation counts, LQ pointer, MMIO head requests and eventual authorized effects. |
| Scalar load issue | Backend model holds valid payload until ready; signed 12-bit immediate and source form the intended address; register-write class and identity match dispatch. | Scalar data/exception writeback, wakeup/cancel, ordering and prefetch events. |
| Scalar store-address/store-data issue | Backend model matches SQ/ROB identity across independent halves and provides operation-appropriate operands. Atomics/CMO use their function types and required serialization. | Address/data acknowledgements, slow feedback and memory effects through cache or uncache. The UT does not model the backend split of a naturally aligned cross-page scalar store; a single-uop cross-page store is not a legal top-level input. |
| Vector memory issue | Backend model supplies a consistent vector configuration, field/uop decomposition, source values and replay metadata; inactive elements must not create forbidden accesses. | Vector data/control writeback, store replay feedback, exception progress and optional VL update. |
| Redirect and exception selection | Backend model provides the live ROB boundary, wrap flag, redirect level and vector-exception classification; isStoreException selects exception-address ownership. | Legal cancellation, retained older work, correct fault address/GPA and no canceled side effects. |
| Translation, fences and distributed CSR | Backend/CSR model supplies legal mode/root alignment, ASID/VMID, privilege and protection state; page tables reside in independent memory. Context changes and fences use the exposed changed/selector fields. | PTW reads, translated accesses, faults and context isolation. |
| DCache TileLink | Manager model drives A/C/E ready, B probes and D responses. Probes and grants use valid identity, permission, size, beat and error combinations. | A acquisition/read/CMO requests, C releases/probe responses and E grant acknowledgements. |
| PTW/uncache TileLink | Memory/device models accept A and return matching D source, size and response type; error injection is explicit. Uncache byte masks must agree with aligned width/address. | Page-table reads; uncached loads/stores and device side effects; exposed errors. |
| Frontend A/D bridges | Frontend model owns each in-side A input and cache-control D ready; external manager owns each out-side A ready and D input. Retained payload fields follow each bridge's actual width and constants. | Buffered, ordered delivery and reset cancellation. These paths do not execute instructions. |
| IFetch PTW and L2 DTLB side port | Frontend/L2 request models provide VPN/translation mode or address/command/kill/prefetch/no-translate; IFetch consumes response with ready. | PTW entry response, translation/fault/PMP response or legal killed-request silence. |
| WFI, store flush, L2 flush, power/halt/error | Backend/top models request drain/quiescence and provide downstream flush completion. | wfiSafe, sbIsEmpty, L2 enable/done and routed power/halt/critical-error state. |
| Interrupt/time/MSI/hart/reset-vector and ICache error bypass | Tile/top/backend input producers supply interrupt levels and valid payloads. BEU local interrupt participates in NMI aggregation. | Delayed/direct projections to backend/top; no interrupt-service or instruction-retirement model. |
| Trigger and prefetch CSR controls | CSR model programs exposed trigger fields and supported enables; software/hardware prefetch traffic remains subordinate to access/exception rules. | Trigger metadata, instruction-prefetch requests, L2/L3 prefetch requests/control. Prefetch quality is outside the functional oracle. |
| Trace | Backend supplies trace groups, offsets, privilege/status and trap information; encoder side supplies enable/stall. | Registered valid/data and enable/hold behavior, including address-plus-offset projection. No encoder/decoder integration claim. |
| Performance/top-down | L2/top supplies miss indicators and event lanes; CSR controls select performance events. | Registered bypass and activity observations. Counter selection, training quality and whole-core attribution require integration checks. |

## Supported input values and combinations

These are current typed-driver and generated-boundary contracts, not an
assertion that every listed value/cross has run. Exact numeric opcodes and
transaction fields live in [model.inc](../cpp/environment/model.inc); Scala
FuType/opcode bundles are a configurable superset. Raw invalid patterns in
pin-space remain structural-only stimulus.

| Input family | Supported values and legality | Source of truth / current limit |
| --- | --- | --- |
| Scalar integer loads | lb/lh/lw/ld/lbu/lhu/lwu encode 0..6. Effective address is source plus signed 12-bit immediate (-2048..2047). The 3 generated lanes are 0..2. | LoadOp, scalar_immediate_is_valid and issue_load; no arbitrary reserved opcode stimulus. FP FLH/FLW/FLD cases use explicit raw-width/result/write-class expectations. |
| Scalar/hypervisor stores and loads | sb/sh/sw/sd encode 0..3; HSV.B/H/W/D encode 0x10..0x13. HLV signed B/H/W/D encode 0x10..0x13; HLV unsigned B/H/W encode 0x14..0x16; HLVX.HU/WU encode 0x1d/0x1e. STA/STD lanes 0..1 match ROB/SQ identities. | LoadOp/StoreOp and [issue.cpp](../cpp/environment/issue.cpp). H-access legality additionally depends on privilege, VS/G, SPVP, protection and memory type. |
| Prefetch and cache block operations | prefetch.i/r/w encode 0x8/0x9/0xa. CBO.ZERO is StoreOp 7 and writes 64 bytes. CLEAN/FLUSH/INVAL encode 0x0c/0x0d/0x0e and use the CMO handshake/commit contract. | PrefetchOp/StoreOp; [maintenance_reset.inc](../cpp/scenarios/maintenance_reset.inc) and [mixed_maintenance.cpp](../cpp/scenarios/mixed_maintenance.cpp). Hint completion is not a demand-load value. |
| Atomics | LR/SC, SWAP/ADD/XOR/AND/OR/MIN/MAX/MINU/MAXU and CAS in W/D widths. Each AtomicOp has an explicit encoding; operands/compare and uop construction must match that family. Naturally aligned W/D addresses are successful classes; illegal byte offsets are deliberate exception cases. | AtomicOp and issue_atomic. Non-speculative serialized issue is required. This does not claim arbitrary Scala atomic widths or every operand decomposition. |
| Vector configuration | SEW/EEW 8/16/32/64; LMUL/EMUL in 1/8, 1/4, 1/2, 1, 2, 4, 8. For ordinary non-indexed operations EMUL=LMUL*EEW/SEW; indexed EEW sizes indices and SEW sizes data. Unsupported/reserved or out-of-range derived shapes are filtered. VL is bounded by independently calculated VLMAX; vm/vma/vta, vstart and mask must describe the same instruction across uops. | VectorMemoryTransaction, vector_* helpers and [mixed_vectors.cpp](../cpp/scenarios/mixed_vectors.cpp). Bits 0..127 of the mask are modeled and driven; `mask_bits` holds 0..15 and `mask_high` holds 16..127. Ordered side-effect semantics remain unverified. |
| Vector operation/decomposition | Unit/strided/indexed unordered/indexed ordered, load/store, segment fields 2..8 (nf encodes fields-1), whole-register fields 1/2/4/8. Legal segment register products and uop counts are enumerated by the shape builders; SEW/EEW/LMUL/derived EMUL/NF must fit those combinations. FOF is a load encoding; segment FOF is unit-stride. Register/index uops, flow_num, vuop_idx and last_uop must agree. | vector_fu_op_type and builder/decoder constraints, [VSegmentUnit](../../../src/main/scala/xiangshan/mem/vector/VSegmentUnit.scala). Ordinary vector uses LSQ flows; segment enqueue is intentionally a no-op. |
| Circular identities and dispatch | ROB values 0..159, LQ 0..71, SQ 0..55 plus wrap flags. Six dispatch lanes; need_alloc 1 selects LQ and 2 SQ, num_ls_elem is nonzero and matches flows. Driver capacity headroom is 6 LQ / 4 SQ. | Circular-pointer helpers and [QueueAccounting](../cpp/environment/queue_accounting.cpp). Exposed bit width alone does not make every bit pattern a valid pointer. |
| Redirect/commit | Redirect level 0 means flush-after, 1 includes the selected uop. A VLS exception enters as an including/flush redirect. Commit/head signals must follow backend age; speculative store halves may overlap older work but cannot commit early. | redirect_after and queue/control helpers. Executing atomic cancellation and active-segment redirect are not general legal producer classes. |
| Translation modes and roots | SATP/VSATP mode 0 is Bare, 8 Sv39, 9 Sv48; HGATP 0 is Bare, 8 Sv39x4, 9 Sv48x4. Stage1 roots are 4-KiB aligned; x4 G roots 16-KiB aligned. Sv39/Sv48 canonical addresses and 41/50-bit GPA limits apply before appropriate translation. | [MMUConst.scala](../../../src/main/scala/xiangshan/cache/mmu/MMUConst.scala), reference_page_walk and context/builders. Arbitrary 4-bit modes are not legal semantic coverage; CSR WARL is outside this boundary. |
| Privilege, pointer mask and protection | Reference privilege U=0,S=1,M=3; virtualization/SPVP/SUM/MXR/VSUM/VMXR select access-specific rules. Pointer masking disabled=0,PMLEN7=2,PMLEN16=3. PMP uses implemented OFF/TOR/NAPOT with 4-KiB grain and lock/priority semantics. | ReferencePrivilegeMode/PointerMaskingMode and [protection.inc](../cpp/scenarios/protection.inc). Reserved modes and arbitrary reprogramming races are not claimed. |
| DCache A and manager response | A Get 4, AcquireBlock 6, AcquirePerm 7, CMO 12..14. Acquires are 64-byte aligned,size 6,mask 0xffffffff,source below 16; grow 0..2 with AcquirePerm excluding 0. CMO source 17,size 6,line aligned. Get uses legal aligned size/mask/source. Manager B/D fields must match size, identity, permission and error/beat rules; denied data beats imply corrupt. | [dcache_a_contract.inc](../cpp/environment/dcache_a_contract.inc), [dcache_agent.inc](../cpp/environment/dcache_agent.inc). Accepted-A checking is not complete instruction attribution or all-channel lifecycle proof. |
| PTW/Uncache/frontend | PTW manager supports Get/AcquireBlock up to64 bytes. Uncache supports Get/PutFull/PutPartial, 1/2/4/8-byte naturally aligned transfers with matching masks. Frontend bridge shapes follow retained request/response fields and fixed/pruned values. | [ptw_agent.inc](../cpp/environment/ptw_agent.inc), [uncache_agent.inc](../cpp/environment/uncache_agent.inc), [frontend.cpp](../cpp/environment/frontend.cpp). Legal response generation does not cover malformed/duplicate/unsolicited responses. |
| Reset, DFT and sideband controls | Reset/DFT controls are Boolean levels with mode-dependent routing. Ten retained DFT control bits permit 1024 routing combinations. Trace group-valid, payload-enable and trap classification determine which fields are meaningful. Interrupts are levels, MSI/time/error payloads obey their retained valid contract. | [BridgeMonitor](../cpp/environment/bridge_monitor.cpp), reset/control helpers and manifest widths. All raw width-fitting values are not automatically architecturally legal events. |

Source relative links resolve to the exact current declarations. The supported
operation tables are finite; adding an encoding requires its legal producer,
independent expectation and observation rather than only a pin-level value.

## UT implementation boundary

[Environment](../cpp/memblock_env.hpp) is the scenario-facing API.
[DutAdapter](../cpp/environment/dut_adapter.hpp) provides typed drive/read
operations over generated ports;
[semantic_ports.json](../config/semantic_ports.json) maps private semantic names
to flattened names. These mappings are access metadata, not behavioral oracles.

[cycle.cpp](../cpp/environment/cycle.cpp), Environment::Impl::tick, drives
managers, refreshes combinational state, captures channel transfers and bridge
inputs, samples writebacks before the edge, steps the DUT, and then updates
manager state and registered queue/feedback observations.
[FeedbackMonitor](../cpp/environment/feedback_monitor.cpp) feeds the scalar,
store and vector scoreboards and records sidebands;
[QueueAccounting](../cpp/environment/queue_accounting.cpp) independently counts
accepted dispatch, retirement and cancellation in aggregate; these counts do
not identify every output event to an individual uop;
[BridgeMonitor](../cpp/environment/bridge_monitor.cpp) derives delay/hold
expectations from input captures, not observed outputs.

[model.inc](../cpp/environment/model.inc) contains transaction descriptions,
byte memory and reference translation/permission calculations.
[scoreboards.inc](../cpp/environment/scoreboards.inc) matches outstanding work
by exposed identities and checks the configured data/metadata contract.
[completion.cpp](../cpp/environment/completion.cpp) advances legal completions,
replays, store commit/reference effects and redirect recovery. The three memory
agents service DUT requests using separate bus memory; reference bytes are not
learned from returned DUT load data. Internal signal handles are for debugging
only.

Present limits matter when interpreting a pass: manager D-input readback is not
an independent proof that the DUT consumed the response correctly; protocol
lifecycle checks are incomplete; the vector transaction model has only a 16-bit
global element mask; exceptional VSTART is checked only when explicitly
expected; and the mixed generator has no comprehensive final readback of every
modified store location. See [IO_COVERAGE.md](IO_COVERAGE.md#unimplemented-or-incomplete-stimulus-and-checks)
and [ORACLES.md](ORACLES.md) before making a coverage claim.
