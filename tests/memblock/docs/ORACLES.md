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

## Stable Oracles

| Domain | Independent calculation | DUT-observable check | Status |
| --- | --- | --- | --- |
| Scalar load data | Read bytes from pre-state sparse memory, then apply operation width and sign/zero extension | One matching scalar writeback with exact data, destination, ROB/uop identity, and exception bits | Implemented |
| Scalar store data | Apply byte enables and little-endian store bytes to a copy of reference memory at commit | Address/data halves are accepted, commit is legal, and a later independent load observes exactly the committed bytes | Implemented |
| Vector load data | Decode unit/strided/indexed addresses independently; apply EEW, `vl`, `vstart`, mask, tail/merge policy, and old destination | Exact 128-bit writeback, active-element behavior, metadata, and identity | Implemented for modeled vector memory operations |
| Vector store data | Generate each active element's byte addresses and bytes from source data; apply mask and split rules | Exact active-byte readback after commit, one completion, replay progress, and queue drain | Implemented for modeled vector memory operations |
| Store forwarding | Overlay the youngest legal store bytes on the pre-state load bytes, per byte and per age rule | Scalar/vector load returns the byte-accurate overlay before store commit | Implemented |
| Sv39 translation | Walk independently populated PTEs, checking valid/leaf, level, permissions, SUM/MXR, A/D, and PBMT policy | Returned data comes from calculated PA, or exact page/access fault and fault VA | Partially implemented; permission matrix expansion planned |
| Sv39x4 translation | Perform VS walk followed by G-stage walk; retain the exact failing PTE GPA and VS-non-leaf classification | Exact host PA or guest fault cause, VA, GPA, and marker | Implemented for current modeled cases |
| Misalignment | Concatenate/split bytes across 16-byte beats, cache lines, and pages; independently apply enabled/disabled policy | Exact value/bytes when legal, or exact address-misaligned cause when prohibited | Implemented for current scalar/vector cases |
| Exceptions | Reference ISA cause and access-type priority from the generated transaction and page-table/PMP model | Cause bits, fault VA/GPA, marker, and single terminal exception writeback | Partially implemented; concurrent priority matrix planned |
| Register side effects | Architectural rule for the instruction class, including no RF write on exceptional load and no RF write for prefetch | RF writeback present only when allowed, with exact destination/data | Implemented for modeled scalar/vector/prefetch paths |
| Prefetch | Prefetch has no architectural destination and does not require data return; translation policy is modeled separately | Completion without RF write or load exception, with legal request behavior | Implemented for software prefetch cases |
| Cache refill | Sparse memory is authoritative for returned line bytes; line address and transfer size are decoded independently | Complete legal refill, no byte corruption, and eventual response consumption | Implemented |
| Dirty eviction | Reference memory tracks committed bytes; an independent immutable pre-eviction line image is required for every release | Correct Release/ReleaseData/ReleaseAck sequence and exact bytes for the whole line | Partial; current agent updates the shared model while consuming release data, so full independent line comparison is planned |
| Uncache/PBMT-NC | Ordered byte memory model with request size, mask, and source identity | Correct AccessAck/Data, ordered store visibility, and exact load data | Partial; current modeled Get/Put path is checked, but denied/corrupt/error response classes are planned |
| Ready/valid | Protocol definition: payload remains stable while `valid && !ready`; accepted items need identity-preserving disposition | Generated SVA checks producer stability; channel monitors must independently validate response identity and legality | Partial; response opcode/source/size/denied/corrupt checks are planned |
| Queue conservation | Accepted LQ/SQ enqueue handshakes, architectural dequeues, and redirect events | `allocated = dequeued + canceled` at quiescence, with identity and flag preservation | Partial; current allocation/cancellation totals are driver-accounted and acceptance observation is planned |
| Redirect | Independent ROB age comparison determines younger work to cancel | Younger work has no terminal writeback; older work retains its expected result; pointer reuse is legal | Partial; basic modeled redirect is checked, but cancellation is currently explicitly accounted by the driver |
| Progress | Fair manager model eventually accepts requests and returns legal responses | Every non-canceled accepted operation terminates before the scenario deadline | Partial; fairness is enforced by the test agent, while accepted-event accounting is being strengthened |
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
without consulting the DUT's internal decoder. Destination, ROB flag, and
queue identity are part of the response key. A duplicate response or a
response with an unknown identity is a failure even when its data happens to
match.

### Vector operations

The vector model independently calculates an element address for unit stride,
constant stride, indexed-unordered, and indexed-ordered operations. It then
applies `vl`, `vstart`, `vm`, the mask bits, EEW, tail/mask policy, and old
destination bytes. Stores use the same address decoder and only modify active
bytes. The model does not assume a particular split-buffer state machine or
replay count; it checks the final byte effect, legal replay progress, and
single architectural completion.

### Translation and faults

The page-table builder writes known PTEs into the sparse memory and records the
root and mode separately from the DUT. The current software walk returns a
physical address or a limited structured fault containing validity, leaf level,
alignment, and guest/host walk information. It does not yet model the complete
RISC-V permission matrix, SUM/MXR, A/D updates, or access-specific fault reason;
those are planned independent extensions. For a VS-non-leaf fault, the GPA is
the guest physical address of the failing page-table access; it is not adjusted
by a vector element offset. This rule is architectural and is deliberately
tested with randomized vector width, mask, and offset.

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
- producer-side TileLink fields are checked for legal combinations; independent
  consumer-side response opcode/source/size/denied/corrupt validation is planned;
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
| Atomics/LR-SC/AMO | Reservation and atomic-memory model with success/failure, alignment, ordering, and optional bus error injection | Atomic read-modify-write result, reservation invalidation, failure code, one memory update, error lifetime |
| CBO/CMO/fence | Line-state model and explicit ordering/flush points | Correct invalidate/clean/zero effect, required completion, and ordering relative to loads/stores |
| VSegment/VFOF | Segment-element address/data model across multiple uops, fault-only-first truncation, and per-segment mask | Multi-uop identity, takeover/ready behavior, partial completion, exact fault element, and no stale prefetch effect |
| HLV/HLVX/HSV | Privilege/virtualization/PMP/PMA reference model including SPVP and execute permission | Effective mode, permission cause, final PA, RF/store side effect, and exception metadata |
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

These limitations are why the status column uses `Partial` and why the planned
oracle rows cannot be closed by another long green run alone.

## Failure Classification

When an oracle fails, the runner records the seed, transaction prefix, phase,
identity fields, reference pre/post memory digest, translation walk, manager
transcript, backpressure seed, and frozen artifact hashes. Reduction must
preserve the architectural mismatch while removing unrelated traffic. Only
after the independent model and transcript are checked is the failure
classified as an RTL candidate.
