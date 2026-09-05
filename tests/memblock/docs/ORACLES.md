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
| Memory trigger metadata | Program `mem_trigger.tUpdate`, enable/load/store select, and match address independently | Breakpoint trigger produces the expected action/exception and suppresses the architectural write; load/prefetch compare trigger, flush, and debug sidebands, while scalar-store/vector paths compare every observable metadata field with optional trigger expectations | Partial; scalar load breakpoint is exact, scalar store/vector metadata is exact where the boundary exposes it, and vector/STA action regeneration remains optional |
| Scalar store data | Apply byte enables and little-endian store bytes to a copy of reference memory at commit | Address/data issue plus strict exception/ROB/flush/debug checks and independent post-commit readback; MMIO stores also require the Uncache request path and SQ retirement | Implemented for modeled scalar stores; output-only data pulses remain filtered when identity is unavailable |
| Vector load data | Decode unit/strided/indexed addresses independently; apply EEW, `vl`, `vstart`, mask, tail/merge policy, and old destination | Exact 128-bit writeback, active-element behavior, metadata, and identity | Implemented for modeled vector memory operations |
| Vector store data | Generate each active element's byte addresses and bytes from source data; apply mask and split rules | Active-byte readback, completion, replay progress, queue drain, RF write enables, flush, and debug sidebands are checked for modeled stores | Partial; generic mixed stores remain limited by the output-only store boundary |
| Store forwarding | Overlay the youngest legal store bytes on the pre-state load bytes, per byte and per age rule | Scalar/vector load returns the byte-accurate overlay before store commit | Implemented |
| Stage-1 translation | Walk independently populated Bare/Sv39/Sv48 PTEs, checking canonical VA, valid/leaf, all supported leaf levels, superpage alignment, permissions, SUM/MXR, A/D, PBMT/N, and reserved-bit policy | Returned data comes from calculated PA, or exact access/page fault and fault VA; encoding faults must not issue a DCache/Uncache data request | Partial: generic Sv39/Sv48 walker, Bare bypass, all leaf levels, canonical/alignment faults, the U/S/SUM/MXR/A/D matrix, and 26 Sv39/Sv48 invalid/reserved/PBMT/NAPOT encodings are implemented; store-side encoding faults and broader fault crosses remain |
| G-stage translation | Walk independently populated Sv39x4/Sv48x4 PTEs with a 16-KiB root, widened root index, 41/50-bit GPA checks, G-stage U-mode permission rule, and guest-page-fault semantics | Exact host PA or guest fault cause, fault GPA, and stage metadata | Partial: generic 41/50-bit walker, Bare bypass, all leaf levels, high-GPA faults, and nested VSUM/VMXR plus G-stage MXR/A permission cases are implemented; G-stage store-D, reserved/PBMT, and broader invalid-PTE crosses remain |
| Nested translation | Compose independent VS-stage and G-stage walks for `Sv39->Sv39x4`, `Sv39->Sv48x4`, `Sv48->Sv39x4`, and `Sv48->Sv48x4`; model Bare degenerations separately | No stage skipped; exact host PA, stage-specific fault, VA/GPA metadata, and implicit VS-page-table accesses | Partial: all four 4-KiB pairs, VS-only/G-only/fully-Bare degenerations, and VSUM/VMXR/G-stage-MXR permission selection are executable; context isolation and broader stage-only accesses remain |
| L2-to-L1 DTLB boundary | Drive the top-level `io_l2_tlb_req_req_*` request fields and observe the corresponding response/PMP fields | Accepted requests return a legal hit/miss response, preserve PBMT and fault semantics, and classify PMP/MMIO consistently; a miss is delegated to the external L2 TLB | Implemented for ordinary and prefetch miss responses in `l2-tlb-contracts`; this boundary has no refill response input, so translation-hit refill and external L2 retry remain outside MemBlock |
| L2 hint propagation | Independent valid/source-id/keyword stimulus at the top-level hint input | Registered hint delivery must not create an unsolicited writeback or queue/protocol violation when no matching MSHR exists | Implemented for all 16 source IDs and `isKeyword=0/1` idle pulses in `l2-tlb-contracts`; matching-MSHR replay behavior remains an L2 integration responsibility |
| Translation context/fences | Include `satp/vsatp/hgatp` MODE/root changes, ASID/VMID tags, `V` transitions, `SFENCE.VMA`, `HFENCE.VVMA`, and `HFENCE.GVMA` scope | Updated translations observe only architecturally ordered page-table writes; stale contexts cannot alias | Partial: direct mode/root, host ASID/root, VS ASID/root, G-stage VMID/root, host/nested `V` transitions, and global/selective fence leaf updates are implemented with distinct-data oracles; same-ID reuse with fences and outstanding-walk ordering remain |
| Misalignment | Concatenate/split bytes across 16-byte beats, cache lines, and pages; independently apply enabled/disabled policy | Exact value/bytes when legal, or exact address-misaligned cause when prohibited | Implemented for current scalar/vector cases |
| Exceptions | Reference ISA cause and access-type priority from the generated transaction and page-table/PMP model | Cause bits, fault VA/GPA, marker, and single terminal exception writeback | Partially implemented; concurrent priority matrix planned |
| Register side effects | Architectural rule for the instruction class, including no RF write on exceptional load and no RF write for prefetch | RF writeback present only when allowed, with exact destination/data | Implemented for modeled scalar/vector/prefetch paths |
| Prefetch | Prefetch has no architectural destination and does not require data return; translation policy is modeled separately | Completion without RF write or load exception, with legal request behavior | Implemented for software prefetch cases |
| Cache refill | Sparse memory is authoritative for returned line bytes; line address and transfer size are decoded independently | Complete legal refill, no byte corruption, and eventual response consumption | Implemented |
| Dirty eviction | Reference memory tracks committed bytes; an independent immutable pre-eviction line image is captured before replacement | Correct Release/ReleaseData/ReleaseAck sequence and exact bytes for the captured pressure lines | Partial; the dedicated pressure phase is byte-checked, while general release coverage remains planned |
| Uncache/PBMT-NC | Ordered byte memory model with request size, mask, beat address, and source identity | Correct AccessAck/Data, legal size/address/mask, ordered store visibility, exact load data at every byte lane, denied/corrupt exception propagation, scalar PBMT=IO store request/retirement, and CBO.ZERO line image | Partial; width/lane and denied/corrupt scalar response propagation are executable (`uncache-widths`, `uncache-errors`); PBMT=IO direct three-cycle load metadata plus scalar store bypass/retirement, a physical non-DebugModule `c=0` PMA load/store pair, guarded DebugModule access-fault case, and cacheable CBO.ZERO line-zero/readback are executable (`mmio-contracts`, `cbo-zero-contracts`); device side effects and malformed/duplicate/late response handling remain |
| Ready/valid | Protocol definition: payload remains stable while `valid && !ready`; accepted items need identity-preserving disposition | Generated SVA checks producer stability; channel monitors validate response identity, legality, and D-channel error flags | Partial; DCache and Uncache denied/corrupt injection are executable, while malformed/duplicate/early/late PTW and Uncache responses remain planned |
| Queue conservation | Accepted LQ/SQ enqueue handshakes, architectural dequeues, and redirect events | `allocated = dequeued + canceled` at quiescence, with identity and flag preservation | Partial; current allocation/cancellation totals are driver-accounted and acceptance observation is planned; `reset-recovery` also checks explicit cancellation of an outstanding LQ entry |
| Redirect | Independent ROB age comparison determines younger work to cancel | Younger work has no terminal writeback; older work retains its expected result; pointer reuse is legal | Partial; basic modeled redirect is checked, but cancellation is currently explicitly accounted by the driver |
| Progress | Fair manager model eventually accepts requests and returns legal responses | Every non-canceled accepted operation terminates before the scenario deadline | Partial; fairness is enforced by the test agent, while accepted-event accounting is being strengthened; `reset-recovery` checks no stale pre-reset completion leaks into post-reset work |
| Coverage | Coverage is a property of generated input/observed events, not a correctness result | Required class counts and mixed-overlap gates are nonzero and complete | Partial; several mixed counters are generator bookkeeping rather than independent monitors |

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
| MMIO and device ordering | A side-effecting device model with read values, write log, byte enables, access width, and ordering points | Uncache/MMIO request class, ROB marking, no cacheable refill, exact device log, exception/error response |
| FP and NaN-boxed MMIO loads | IEEE bit-pattern/reference load formatter plus NaN-box rule for narrow FP values | Exact FP destination bits, exception behavior, and no accidental integer formatting |
| Atomics/LR-SC/AMO | Reservation and atomic-memory model with success/failure, alignment, ordering, and optional bus error injection | Partial: all exposed W/D-width AMO ALU variants, AMOCAS.W/D compare success/failure, LR/SC old-value/writeback, success/failure, cache visibility, and representative misaligned D/W exception metadata are executable in `atomic-contracts`. `atomic-dchannel-errors` covers the complete refill-capable W/D opcode x denied/corrupt matrix, persistent poisoned-line exceptions, suppressed exceptional `rfWen`, exact request counts, and clean error-lifetime recovery. Exceptional data and internal reservation state are not used as oracles; cross-hart reservation interference, full alignment crosses, and concurrent ordering remain |
| CBO/CMO/fence | Line-state model and explicit ordering/flush points | Correct invalidate/clean/zero effect, required completion, and ordering relative to loads/stores | Partial; cacheable CBO.ZERO's StoreQueue/SBuffer line-zero and readback are executable (`cbo-zero-contracts`); CMO CLEAN/FLUSH/INVAL, fence.i, and full ordering remain because the CMO response channel is internal to DCache and absent from the MemBlock top-level harness |
| VSegment/VFOF | Segment-element address/data model across multiple uops, fault-only-first truncation, and per-segment mask | Multi-uop identity, takeover/ready behavior, partial completion, exact fault element, and no stale prefetch effect |
| HLV/HLVX/HSV | Privilege/virtualization/PMP/PMA reference model including SPVP and execute permission | Effective mode, permission cause, final PA, RF/store side effect, and exception metadata |
| Sv48 stage-1 | Four-level independent walk with canonical VA checks, L3/L2/L1/L0 leaves, 512-GiB alignment, and stage-1 permission rules | Fourth-level PTW activity and exact PA/page fault; no fallback to a three-level Sv39 walk. The 4-KiB and superpage data paths are implemented; full fault matrix remains |
| Sv48x4 G-stage | Four-level independent G-stage walk with 16-KiB root, 50-bit GPA checks, and guest-page-fault semantics | Exact host PA or G-stage fault/GPA; no fallback to Sv39x4 indexing. The 4-KiB and superpage data paths are implemented; executed overflow fault remains |
| Nested mode cross | Independent composition of all four VS/G-stage pairs plus Bare degenerations and stage-2-only accesses | Correct mode pair, ASID/VMID isolation, stage-specific fault, and no stale translation after a mode change. All four 4-KiB pairs are implemented; Bare/context-transition isolation remains |
| Translation fences | Independent page-table update model for `SFENCE.VMA`, `HFENCE.VVMA`, and `HFENCE.GVMA`, including selective/global scope | Updates become visible only after the architecturally required fence and only to matching context; global/selective `SFENCE.VMA`, selective `HFENCE.VVMA`, and global `HFENCE.GVMA` are implemented |
| PMP/PMA matrix | Region matcher for TOR/NA4/NAPOT, permissions, lock, priority, cacheability, and misalignment | Access allowed/denied, cause, no forbidden request, and exact region boundary behavior |
| Probes/coherence errors | TileLink probe/ack/data state model and corrupt/denied response semantics | Probe response legality, line state, replay/error propagation, and no lost dirty bytes |
| Concurrent exception priority | Reference ROB/uop age relation independent of LQ/SQ index | Oldest legal exception selected across wrapped pointers; younger effects suppressed |
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
