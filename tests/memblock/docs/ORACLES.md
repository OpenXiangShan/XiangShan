# MemBlock Oracle Catalog

This catalog describes the checks implemented by the current UT and the checks
that are incomplete or missing. It is a source map, not evidence that a
scenario ran or that a verification point is closed. The canonical mixed
generator is `constrained-random`. Stimulus requirements belong to
[VERIFICATION_PLAN.md](VERIFICATION_PLAN.md); generated interfaces and
stimulus ownership belong to [IO_COVERAGE.md](IO_COVERAGE.md).

## Oracle Rules

The following distinctions apply throughout the catalog.

| Responsibility | Current meaning | What it does not establish |
| --- | --- | --- |
| Generator expectation | A scenario chooses legal inputs and declares expected data, exception, destination, lane, or optional sideband fields before completion. | Declaring an expectation does not prove issue acceptance or a DUT effect. |
| Reference model | Software computes bytes, vector element addresses, page walks, permissions, and selected post-state from independently owned inputs. | A model update alone does not prove that the DUT changed memory. |
| Driver-owned state | Issue, commit, replay, reset, and queue cursors control stimulus and track obligations. | Driver bookkeeping is not a passive observation of accepted DUT work. |
| Passive observation | Monitors sample top-level handshakes, writebacks, queue counts, feedback, and bridge outputs. | A sampled event is not automatically matched to a unique instruction or checked against an expectation. |
| Terminal oracle | A scoreboard compares a completion to a live expectation, or a scenario compares an externally visible effect to its independent expected value. | A terminal data check does not imply every intermediate protocol or side-effect rule is checked. |
| Observational coverage | Counters describe selected inputs or sampled events. | A nonzero bin is not a correctness proof; an unobserved bin is not automatically a test failure. |
| Missing check | A documented rule has no sufficient active checker in the described path. | A passing run cannot establish that rule. |

The concrete ownership is visible in
[model.inc](../cpp/environment/model.inc),
[scoreboards.inc](../cpp/environment/scoreboards.inc),
[cycle.cpp](../cpp/environment/cycle.cpp), and
[mixed_reporting.cpp](../cpp/scenarios/mixed_reporting.cpp).

Expected architectural values must come from transaction inputs and independently
owned memory or translation state. DUT internal state can localize a failure;
it cannot supply an independent expected value. This is the methodology used to
interpret the checks below, not a claim that every existing scenario satisfies
it. Implementation-dependent exceptions are identified explicitly.

## Stable Oracles

“Implemented” below means that the named path has the stated active comparison.
It does not mean complete coverage of the ISA, interface, concurrency space,
or every caller of that path.

| Domain | Implemented terminal comparison | Limits and missing checks | Source |
| --- | --- | --- | --- |
| Scalar loads | `LoadScoreboard` matches ROB value and wrap flag; checks destination, exception policy, replay clear, RF/FP write enables, trigger, flush, optional debug classes, and normally exact data. An unknown completion fails; a consumed expectation cannot complete again. A caller can opt in to one `ld2Cancel` non-replay data pulse as a legal canceled attempt; the scenario must then account the LQ cancellation. | Expectations are registered before issue and are not an independent accepted-issue ledger. Data on an expected exception is skipped unless `check_data_on_exception` is set. Optional sidebands are unchecked when absent. Exact replay after a canceled attempt is scenario-owned. | [LoadScoreboard](../cpp/environment/scoreboards.inc), [expect_load/expect_load_data](../cpp/environment/control.cpp) |
| Software prefetch | Shares the scalar identity map; checks zero exception, no replay or RF/FP write, expected trigger/flush and optional debug fields. | No returned-data comparison, destination comparison, required cache fill, or required hardware-prefetch event. | [expect_prefetch/observe](../cpp/environment/scoreboards.inc) |
| Scalar stores | Tracks address/data input acceptance separately. Address writeback checks declared output lane, ROB flag, exact exception and flush, optional trigger/MMIO/NCIO. Normal retirement requires address and data observations; an exceptional address result retires without data. Constrained-random finishes with an independent readback of every tracked cacheable identity-mapped committed word. | The store scoreboard itself has no byte comparison. Data writeback has no ROB flag and is matched by ROB value, accepted-data status and lane; unmatched pulses are ignored. The mixed final readback remains identity-mapped; `translation-alias` separately observes a committed store through a second VA to the same PA after drain and with SBuffer non-empty at an in-flight load issue. Committed non-cacheable store/atomic words are compared against the manager bus image after drain; CMO-maintained lines and device side effects are not claimed. A naturally aligned store that crosses a 4-KiB page is not modeled as one legal top-level uop: the backend/misalign splitter that produces the pieces is absent. Driving one uop can complete without checking the second page; this is a legal-input boundary, not an RTL claim, and cross-page scalar split-store fault side effects remain untested. | [StoreScoreboard](../cpp/environment/scoreboards.inc), [commit_store](../cpp/environment/completion.cpp), [finish](../cpp/scenarios/mixed_reporting.cpp), [translation-alias](../cpp/scenarios/translation_permissions.inc) |
| Vector loads/stores | Matches ROB value/flag plus `vuop_idx` and `pdest`; checks exception policy, no replay, flush, RF enables, operation and optional trigger/debug fields. Successful writing loads check element data, write mask, and destination chunk index. Segment completions must use lane 0. Constrained-random cacheable identity-mapped vector-store commits participate in the final word readback. | Exception results skip normal data/mask, destination-index and SEW/EEW/LMUL/VL metadata checks. Fault `vstart` is unchecked by default; explicit expectations now cover a two-uop `vstart=8` load fault and single-uop later-element/masked `vstart=1` load faults, masked, unmasked, first-element, first-segment-field, multi-element segment and PMP-denied/masked/cross-page/segment (including first-field) and EEW-1/2 vector-store faults with exact addresses and unchanged target pages, plus PMP-denied unmasked/masked vector loads, a partially masked unmapped-element load control, plus an all-masked unmapped control. The exception-address payload for the intra-uop shapes is sampled after the vector feedback settles and must equal the faulting element address. The vector scoreboard itself has no memory-byte comparison, and aliased/non-cacheable stores remain outside the final readback. | [VectorMemoryScoreboard](../cpp/environment/scoreboards.inc), [finish](../cpp/scenarios/mixed_reporting.cpp) |
| FOF data and fix-VL | Successful fix-VL checks the exact VL byte, remaining zero bytes, all-ones mask, write enables, destination and lane (segment 0, ordinary 1). Callers independently establish first/later fault and expected VL; allowed arbitrary FOF data elements are explicit. | FOF exception allowances and arbitrary-element exclusions are caller declarations, not inferred faults. Per-uop allowed first-fault exceptions require a scenario-level check that a fault actually occurred. Exceptional completions are not counted by the successful fix-VL branch. | [scoreboards.inc](../cpp/environment/scoreboards.inc), [mixed_vectors.cpp](../cpp/scenarios/mixed_vectors.cpp), [vector_memory.inc](../cpp/scenarios/vector_memory.inc) |
| Scalar/vector committed bytes | Commit helpers update only the architectural reference image; subsequent loads, explicitly expected ReleaseData, or probes can compare externally visible bytes to it. Directed vector stores include load readback; high-VA segment store actions read every active field element back. | Generic constrained-random stores are not each followed by forced byte observation. Recording a post-state is not a memory-effect check. A final unobserved store can escape a later load/release comparison. | [completion.cpp](../cpp/environment/completion.cpp), [execute_scalar_store](../cpp/scenarios/mixed_action_scalar.cpp), [execute_vector_memory](../cpp/scenarios/mixed_action_vector.cpp), [vector_memory.inc](../cpp/scenarios/vector_memory.inc) |
| Atomics | Mixed AMO/CAS/LR/SC uses independently calculated old-value or SC-result scalar writebacks, W sign extension, ALU/compare post-state, and error causes. Error actions compare the selected manager response/beat/GrantAck/refill counts and unchanged backing memory at the check point. Successful cacheable identity-mapped atomic post-state joins the final word readback; non-cacheable atomic words join the manager-bus final check. `atomic-contracts` additionally forces a ProbeAckData on the dirty AMO line and checks the updated line bytes and manager image. | Cross-hart reservation interference is not modeled. A directed atomic-then-store/CBO.ZERO sequence is not yet executable: after the atomic, the younger store remains in the Sbuffer with no writeback even after a dummy ROB commit pulse. The UT lacks the atomic ROB-commit/SQ-drain precondition model; this is a harness ordering gap, not a confirmed RTL violation. The exceptional private-cache image rule is implementation-dependent, as described below; error-path poisoned images and aliased atomics are not claimed by the final readback. Non-cacheable atomic words use the manager-bus check. | [mixed_transactions.cpp](../cpp/scenarios/mixed_transactions.cpp), [mixed_action_atomic.cpp](../cpp/scenarios/mixed_action_atomic.cpp) |
| Translation/faults | Software walks independently written PTEs; scenarios declare expected PA, causes and isolated fault metadata, compare terminal completion, and selectively check manager traffic and recovery. | The scalar/vector scoreboards do not themselves walk page tables, derive PMP/PMA policy, sample fault VA/GPA, or forbid memory requests. Those checks exist only where the scenario invokes them. | [reference walkers](../cpp/environment/model.inc), [translation_permissions.inc](../cpp/scenarios/translation_permissions.inc), [guest_faults.inc](../cpp/scenarios/guest_faults.inc) |
| Queue conservation | Passive enqueue monitor counts every exposed valid dispatch lane and its LQ/SQ element allocation; checks legal `need_alloc` and nonzero count; cross-checks driver totals. Dequeues are sampled from DUT outputs. Redirect counts are sampled once per driven redirect. | Totals do not identify each retired/canceled uop. Software-only cancellation APIs remain available and count separately. There is no enqueue-ready pin in this monitor. | [queue_accounting.cpp](../cpp/environment/queue_accounting.cpp), [redirect handling](../cpp/environment/completion.cpp) |
| Progress | Issue and completion helpers time out if the requested ready/completion/drain condition is not reached. `check_components` collects manager and scoreboard failures and queue-accounting errors. | A deadline is a harness bound, not an ISA latency bound. Waiting for one class does not drain every manager or check every memory byte. | [cycle.cpp](../cpp/environment/cycle.cpp), [completion.cpp](../cpp/environment/completion.cpp) |

### Scenario-owned boundary contracts

These checks live outside the common scoreboards. Their scope is the constructed
case and its observed result, not every execution of a similar instruction.

| Path | Implemented comparison | Limit |
| --- | --- | --- |
| FP loads | `run_fp_loads` independently NaN-boxes FLH/FLW, preserves FLD bits, and uses the scalar scoreboard to check the FP destination and suppress integer writes; selected fault cases check cause, queue retirement and request suppression. | The common byte-load helper does not calculate FP boxing by itself. See [load_feedback.inc](../cpp/scenarios/load_feedback.inc). |
| Triggers | `run_trigger_contracts` declares action/cause and selected vector fault `vstart`, checks selected DCache request suppression/prefixes, and drives trigger enable/match/action/chain controls. | Triggered-store reference-image unchanged checks alone are not DUT readback. There is no complete common trigger interpreter. See [load_feedback.inc](../cpp/scenarios/load_feedback.inc). |
| RAW/RAR redirect | Directed conflict/nonoverlap and candidate-age cases compare pulse count and RVC/ROB/FTQ/level metadata chosen from independent input identities. | General feedback sampling has no such expected conflict model; broader source/concurrency combinations remain. See `run_memory_violation` and RAR cases in [load_feedback.inc](../cpp/scenarios/load_feedback.inc). |
| Fault range and exception selection | Byte-walked selected canonical/GPA boundary cases compare exact fault address, cause, observed queue recovery and control/survivor data. Directed competing-exception cases select expected output from independently supplied identities. `translation-permissions` also reads the target word back after a permission-fault store, using a readable mapping or Bare identity fallback, to prove no bytes leaked; `pmp-contracts` does the same after a PMP-denied store by re-enabling read access. | Isolated/selected cases do not establish a general simultaneous exception ledger. Device fault paths still need another observation route. See [mixed_boundaries.cpp](../cpp/scenarios/mixed_boundaries.cpp), [translation_faults.inc](../cpp/scenarios/translation_faults.inc), [translation_permissions.inc](../cpp/scenarios/translation_permissions.inc). |
| Translation invalidation | Mapping/context/fence cases use distinct physical data and delayed snapshotted PTW responses, then compare post-fence survivor data and suppression of redirected stale work. | Selected held walks/context combinations do not establish every concurrent invalidation or hardware A/D interaction. See [translation_context.inc](../cpp/scenarios/translation_context.inc). |
| L2 TLB and IFU PTW | L2 cases check cold miss then retry hit/fault, no extra retry PTW read, and only declared PA/PBMT/protection fields. IFU cases derive response fields from page tables and compare sectors/metadata; the driver compares response stability during a deliberate stall. | Miss/fault fields are checked only when meaningful to the case; external consumer policy remains outside the UT. See [protection.inc](../cpp/scenarios/protection.inc), [translation_context.inc](../cpp/scenarios/translation_context.inc), [translation_ports.cpp](../cpp/environment/translation_ports.cpp). |
| Replacement/CMO/CBO | Directed cases compare immutable dirty release data, operation/permission results, selected delayed acknowledgments, load readback, same-line CBO.ZERO/store ordering in both directions, a same-line repeated CBO.ZERO pair, a three-line CBO.ZERO burst, an older-store/younger-zero/younger-load forward window, a zero/younger-store/younger-load age control, an early younger load, and cancellation of selected younger work. | These local constructions are not a complete coherent permission/state model or simultaneous maintenance-order model. See [cache_queue.inc](../cpp/scenarios/cache_queue.inc), [maintenance_reset.inc](../cpp/scenarios/maintenance_reset.inc). |
| Reset/WFI | Selected delayed-manager reset cases cancel old work and check distinct survivors; WFI cases require unsafe during held work and eventual safe after drain. | Software queue teardown can be used; dirty persistence and every partial/error/reset state are not established. See [maintenance_reset.inc](../cpp/scenarios/maintenance_reset.inc). |
| Frontend/DFT transport | Frontend exercise compares declared request/response payloads, handshakes and extra transfers; selected reset cases suppress buffered stale traffic. DFT exhausts ten Boolean inputs against combinational frontend/backend outputs while reset is held. | DFT pin transport is not functional scan/clock/reset verification. See [frontend.cpp](../cpp/environment/frontend.cpp), [control.cpp](../cpp/environment/control.cpp). |
| Top-down/prefetch | Bridge delay relations are exact; focused top-down cases also gate selected pressure/event counters. | Event presence is implementation characterization, not architectural performance attribution or a generic correctness oracle. See [load_feedback.inc](../cpp/scenarios/load_feedback.inc), [feedback_monitor.cpp](../cpp/environment/feedback_monitor.cpp). |

## Reference Model Details

### Memory ownership and scalar values

The environment owns two `SparseMemory` instances. `memory_` is the reference
image and mirrors ordinary initialization writes into `bus_memory_`;
`write_reference_byte` changes only the reference image. Managers use
`bus_memory_`. This separation is established in the
[environment constructor](../cpp/environment/implementation.hpp) and
[SparseMemory](../cpp/environment/model.inc).

`SparseMemory` reads little-endian bytes, returns zero for unspecified storage,
and supports lazily described repeating incrementing regions. `expected_load`
derives byte width from the low operation bits and sign/zero extension from the
load encoding. It is not a complete scalar instruction interpreter. Explicit
`expect_load_data` values are used where a scenario owns a different result
calculation, such as atomic return values or FP boxing. The scoreboard does not
independently validate how an explicit expected value was calculated.

`commit_store`, `commit_vector_store`, `record_committed_store`,
`record_committed_vector_store`, and `record_atomic_result` apply predicted
post-state to the reference image. Ordinary store helpers use a recorded SQ
dequeue target and backend commit stimulus; record-only helpers trust their
caller to select a legal point. CBO.ZERO writes 64 zero bytes at the containing
line base; CMO changes no reference bytes. These functions are
[reference-state transitions](../cpp/environment/completion.cpp), not passive
proof of a successful memory update. DCache ReleaseData is compared before the
manager writes accepted bytes into its backing image. Uncache writes instead
apply captured A data/mask to backing memory when not denied; a separate
readback or device-transcript assertion is needed to establish intended bytes.

Store forwarding is checked by scenarios supplying explicit younger-load
expectations from older store inputs, including byte-width extension, and by
older-load/younger-store controls that must not forward. This is
not a general reference memory-ordering engine: the
[scalar/vector forwarding cases](../cpp/scenarios/vector_memory.inc) and
[mixed transaction helpers](../cpp/scenarios/mixed_transactions.cpp) own their
particular age, byte-overlay, and issue arrangement.

### Scalar completion details

`LoadScoreboard::expect` computes expected RF/FP enables from the *expected*
exception: both are suppressed when a fault is expected. Required exception
bits must be present and bits outside the required plus allowed-additional
mask must be absent. Data comparison follows the expected exception, not a
DUT-decided exception. Disabling normal data checking is accepted only for a
declared DebugMode trigger; exceptional data is skipped separately by the
default fault policy.

`StoreScoreboard` uses declared address/data issue lanes and optional different
completion lanes. Acceptance markers are idempotent to allow replay. Address
pulses before accepted issue or before its recorded cycle are ignored for an
existing identity. When explicitly enabled, one preliminary zero-exception
address result may precede the expected exceptional result. The data path
requires a unique accepted owner with the observed ROB value and lane, but
compares no data-writeback payload. Lack of a match is ignored; ambiguous,
wrong-lane, and already-seen matches fail. The distinction matters for
exactly-once claims. See [StoreScoreboard](../cpp/environment/scoreboards.inc).

### Vector values and exceptions

[model.inc](../cpp/environment/model.inc) computes unit-stride, signed-stride,
indexed-ordered and indexed-unordered element addresses, ordinary uop base
advancement, indexed SEW versus index EEW, split destination chunks, and
segment field/chunk addresses. Whole-register effective VL is derived from
register count and EEW. Active elements use global element position, VL,
`vstart`, `vm` and mask bits. Expected successful load data starts with the
supplied old destination and replaces active bytes with reference memory.

The successful load comparator accepts old data or all-ones for each
individually mask/tail-agnostic element; active and undisturbed elements must
match exactly. Explicit `fof_unspecified_elements` may exclude only active
elements of a FOF load and qualify both data and mask. Additional optional
vector exceptions are restricted to the load-page-fault bit of a FOF load.
The [scoreboard](../cpp/environment/scoreboards.inc) checks the low 16 active
mask bits and requires any remaining returned mask bytes to be zero. A vector
uop writes exactly 16 local element-mask bits, so this is not a 16-bit limit on
the instruction mask.

The transaction carries mask bits 0..15 in `mask_bits` and bits 16..127 in
`mask_high`. `vector_mask_enabled` applies both to global element positions,
and the issue driver writes all 16 `src(3)` mask bytes. Directed unit-stride, LMUL8 full-range, ordinary/special-indexed, and
store/readback cases exercise high active and inactive elements;
the constrained-random vector generators also populate the high bits. This
removes the former "global elements at or above 16 are always disabled" model
limitation, but it does not establish ordered side-effect semantics or every
illegal/reserved vector shape.

A directed two-uop vector page fault supplies `expected_writeback_vstart=8` and
checks the exact fault VA at the second page. `translation-faults` also issues a
single 16-byte unit-stride vector load whose first element is in a mapped page
and whose second element begins in an unmapped page. Because the access starts
at offset `0xff8` and crosses the 128-bit split boundary, it consumes two load
queue flows. The writeback must carry a load page fault and exact `vstart=1`,
both unmasked and with only element 1 active (mask bit 0 clear). The masked
variant proves `vstart` is the architectural element index, not the number of
active elements. A separate load with no active element at an unmapped VA must
complete successfully with zero `vstart`, and the same run observes no PTW or
DCache request for it. A partially masked load with only element 0 active and
element 1 pointing into an unmapped page must also complete with the mapped
element's data and no exception. The exception-address payload is sampled after a short
settle because the vector feedback reaches the LqExceptionBuffer one cycle
after the writeback; both intra-uop cases then require the exact faulting
element address. A single-uop 16-byte vector store with element 0 in a
writable page and element 1 in a read-only page must fault with `vstart=1` and
the exact faulting-element address in the read-only page; the driver selects
the store exception address and a scalar load then reads the read-only page
word back unchanged. A masked variant activates only element 1; it must not
write the inactive element 0's writable page either. A first-element variant
puts element 0 itself in a read-only page; it must fault with `vstart=0` and
must not write either that page or the following writable page. A segment-store
variant writes field 0 to a writable page and faults field 1 in a read-only
page; both data and fix-VL uops must carry the store page fault, `vstart=0`
(segment `vstart` is the vector element index, not the field index), the exact
field-1 address, and the read-only page must stay unchanged. A two-element
variant faults element 1 after element 0's fields are writable; it requires
`vstart=1`, the element-1 field-0 address, and an unchanged read-only element
page. A first-field variant puts field 0 itself in a read-only page; it must
fault with `vstart=0`, the field-0 address, and must not write the following
writable field-1 page either. A PMP read-only page vector store must raise a
store access fault at the element address, issue no DCache/uncache request,
leave memory unchanged, and remain readable through a scalar load. A masked
variant activates only element 1 in the PMP-denied page; it must fault at
element 1 with `vstart=1`, not at the masked-off element 0, and both element
addresses must read back unchanged. A boundary-crossing variant places element
0 in a PMP read/write page and element 1 in a PMP read-only page; the fault
must be at element 1 with `vstart=1`, no request may issue, and the read-only
element page must stay unchanged. A segment PMP variant puts field 0 in a
read/write page and field 1 in a read-only page; both data and fix-VL uops must
report store access fault with `vstart=0`, no request, and the read-only field
page unchanged. The first-field variant swaps the permissions: field 0 is
read-only and field 1 read/write; the fault must be at field 0 and the
following writable field page must not be written. EEW 1 (2-byte) and EEW 2
(4-byte) single-element PMP-denied stores must also fault at the element
address, issue no request, and leave the page unchanged. A PMP no-access page
vector load must raise load access fault at the element address, issue no
DCache/uncache request, and retire the load-queue entry without a data
writeback. A masked variant activates only element 1 in the PMP-denied page;
it must fault at element 1 with `vstart=1`, not at the masked-off element 0,
and issue no manager request.
Sampling immediately after the writeback would read the previous buffer value
and is a testbench timing error, not a DUT result. For
exceptional vector completions generally, default `vstart` checking is still
skipped unless a caller supplies an expectation, and ordinary data/mask plus
SEW/EEW/LMUL/VL checks are skipped.
`expected_writeback_vstart` enables an exact value check even on an exception;
without it, only a successful completion is required to return zero. A field
existing in the transaction type does not mean every fault producer sets it.

FOF callers construct data and fix-VL uops separately. A later fault can leave
the faulting/remainder elements explicitly unspecified while checking the
mapped prefix and reduced VL; a first fault requires an architectural exception
plus the caller's original-VL expectation. Segment cases own the complete
expected data-uop/fix-VL completion counts. The
[focused cases](../cpp/scenarios/vector_memory.inc) and
[mixed FOF producer](../cpp/scenarios/mixed_vectors.cpp) perform these
scenario checks; the scoreboard alone cannot establish the instruction-level
fault selection or complete producer stream.

### Translation and protection

The [reference model](../cpp/environment/model.inc) supports Bare, Sv39 and
Sv48, with the latter two interpreted as Sv39x4/Sv48x4 for G-stage walks.
It checks canonical host/VS virtual addresses, 41/50-bit zero-extended GPAs,
PTE V and illegal R/W encodings, reserved/PBMT/N bits, nonleaf restrictions,
superpage alignment and the modeled 64-KiB NAPOT encoding. Valid PTE PPNs are
checked against the configured 48-bit PA width; VS-generated GPAs are checked
against the selected G-stage width. This is a supported-mode model, not a
WARL or software-visible CSR implementation.

Permission helpers independently implement modeled R/W/X/U/A/D, SUM and MXR
selection, HLVX execute permission, and G-stage U-mode access rules. An implicit
G-stage read of a VS PTE requires U, R and A; original store direction does not
turn that read into a write, and MXR does not substitute X for R. Nested access
walks distinguish stage-1 page, guest-page, and access faults. Implicit VS-PTE
access faults carry the exact PTE GPA and stage marker; a VS PPN that constructs
an over-width GPA uses the failing VS level for the marker. The PBMT composition
helper returns non-PMA VS PBMT when present, otherwise G PBMT.

The address-only walker and access-aware permission walker are separate
functions. A scenario using `reference_two_stage_walk` has not automatically
checked every access permission. Selected
[permission/PBMT cases](../cpp/scenarios/translation_permissions.inc) use
access-aware results, explicit terminal metadata and manager-routing checks.
PMP/PMA and trigger expectations are scenario-specific calculations and
programming in [protection.inc](../cpp/scenarios/protection.inc) and
[control.inc](../cpp/scenarios/control.inc), rather than a shared complete
protection/trigger interpreter. Pointer masking and bitmap word/bit selection
have pure helper calculations in [model.inc](../cpp/environment/model.inc).

Fault VA/GPA outputs have no valid bit or transaction identity.
[guest_faults.inc](../cpp/scenarios/guest_faults.inc) compares them in isolated
cases with independently established fault ownership. The vector VS-PTE-fault
VA is calculated from the first active element; the implicit PTE GPA is not
shifted by that element offset. Such a payload cannot be attributed to an
arbitrary newer writeback in a mixed fault window.

Side-effect checks are not universal. The
[DCache faulting-load helper](../cpp/environment/dcache_a_contract.inc) permits
background hardware-prefetch Acquires and, when explicitly enabled, target-line
CPU demand Acquires. The suppressed-demand helper permits a specifically
declared one-request PMP prefix. Some
[translation cases](../cpp/scenarios/translation_permissions.inc) instead
require an unchanged total DCache/Uncache request count. These policies have
not been unified around request-class/address ownership and legal early
demand. Passing one policy does not validate the other, and speculative
permission traffic is not proof of a committed store.

### Atomic post-state

The [mixed atomic calculation](../cpp/scenarios/mixed_transactions.cpp)
implements W/D arithmetic, logical, signed/unsigned min/max and swap results;
W updates preserve the other word in the predicted 64-bit image. CAS uses an
independent compare, and mixed LR/SC predicts an immediate successful SC.
`execute_atomic` sends expected old-value/SC-result completions through the
scalar scoreboard and records predicted successful memory state. It does not
force an external read of every final updated value before returning.

For an errored refill,
[mixed_action_atomic.cpp](../cpp/scenarios/mixed_action_atomic.cpp) also
predicts an AMO/CAS update to a private poisoned-cache image so that a later
ReleaseData matches that implementation policy. This expectation is explicitly
implementation-dependent. It is not an independent architectural proof of
permitted exceptional memory effects; the unchanged backing-memory comparison
only establishes the backing image at its sampling point.

## Temporal and Protocol Oracles

### Sampling, bridge checks and feedback

[cycle.cpp](../cpp/environment/cycle.cpp) samples completion pins and enqueue
inputs before the clock edge; dequeue counts and slow replay feedback are
sampled after it. Manager capture/update follows the same edge separation.
`tick(false)` skips pre-edge scoreboards, enqueue sampling and bridge checks;
it still advances managers and post-edge observations. Claims of continuous
monitoring apply only while the corresponding monitor is enabled.

[BridgeMonitor](../cpp/environment/bridge_monitor.cpp) compares independently
captured inputs against explicit combinational, one-cycle, two-cycle or
valid/hold relations. These include L2 flush, hart ID, reset vector, halt/error,
interrupt/MSI/time, frontend reset/BEU, shared hardware-counter event lanes,
L2 prefetch controls, trace fields and registered L2/L3 miss status. Trace
address uses input instruction address plus twice the FTQ offset within the
exposed width; privilege/trap/payload updates obey their declared valid/hold
conditions. Only these explicit bridge relations justify fixed-cycle checks.

[FeedbackMonitor](../cpp/environment/feedback_monitor.cpp) samples wakeups,
load cancels, MMIO pulses, slow IQ feedback, memory violations, bus-error
reports, and hardware/IFU prefetch requests into statistics. Sampling alone
does not compare an architectural expectation. Vector replay requests are
matched to a transaction's circular load/store queue flow range before reissue;
this is driver progress handling, not a second terminal completion.

There is an unresolved implementation/norm conflict: the scalar guest-fault
case in [guest_faults.inc](../cpp/scenarios/guest_faults.inc) fails unless cancel
count is nonzero and equals wakeup count. The pins otherwise serve as diagnostic
observations; their cardinality is not an independent architectural result.
This gate must not be reported as architectural fault verification.

Manager-count baselines are taken after Sbuffer/DCache drain plus a bounded
settle. A committed store may leave LSQ/Sbuffer before its DCache A is accepted,
so queue retirement alone is not a request-count quiescence point. Hypervisor
store suppression attributes CPUStoreData requests covering the translated
target line; total source counts are diagnostic because a prior committed store
can still drain. The load path attributes CPU demand loads and their target
line. Background prefetch and unrelated prior stores are not proof of a
forbidden operation.

### DCache manager

| Rule | Active check/model | Incomplete or missing portion |
| --- | --- | --- |
| Accepted A legality | [dcache_a_contract.inc](../cpp/environment/dcache_a_contract.inc) validates supported opcode, size and corruption. Acquires require full-line alignment/size/mask, miss-source range, legal grow and request-source classes, and VA alias/page-offset consistency. Get checks param, alignment, mask and source range. CMO checks line size/alignment and the dedicated source. | Get user fields and CMO unspecified fields are intentionally unconstrained. Opcode legality does not by itself match an instruction to an A request. |
| A source credit | [update_after_tick](../cpp/environment/dcache_agent.inc) rejects reuse while the source is active, activates on A acceptance, frees on final corresponding D; same-edge D completion is processed before new A. | Credit is not retained through GrantAck. It is separate from E-sink lifetime. |
| D response | The manager constructs response beats from captured A and backing memory, then compares its driven handshake fields/data to queued beats. GrantData supports keyword beat order; denied is fixed and implies corrupt on data beats; independent corrupt injection can select first/last/all beats. | This is a manager/adapter consistency check of manager-owned output, not an independent DUT response scoreboard or malformed-response campaign. |
| E GrantAck | Sink must match a queued expectation; matching consumes exactly one entry. An E is accepted only after the first corresponding Grant/GrantData D beat, or in that same cycle, and every sink stays busy from first D acceptance until E. Drains check expected E entries separately from D response queues. | The expectation entry is still created when `respond` constructs Grant/GrantData; the first-D/sink-busy check supplies the causal eligibility gate. A same-cycle D/E is accepted. A sink's A source may still be reused only after its final D, which is separately checked. |
| Probe lifecycle | `request_probe` allocates rotating 6-bit B sources and rejects reuse while a response expectation exists; a directed window holds C until eight B requests are accepted and checks eight ProbeAck identities. Accepted B counts, source reuse/wrap, and accepted outstanding depth are observed. Accepted B is matched by source to its expectation, marks that expectation eligible for C, and duplicate acceptance of one expectation fails. | The expectation is queued before B fires; the accepted-B flag is the causal gate for ProbeAck. Same-address B/C attribution and broader alias/ownership interleavings remain incomplete. |
| ProbeAck/Data | Requires an accepted B expectation at the same address (C's client-owned source is not tied to B source), then matches address to expectation; check opcode, permission report, line size/address, corrupt clear, exact expected data beats, and stable multibeat C `source`; TileLink user/echo fields are DUT-specific here and are not required stable. | Address+opcode+report lookup does not disambiguate multiple same-address expectations: a directed two-probe experiment returned a data-less C with report 5 while the accepted expectation expected report 2, and C was observed before the manager's second B acceptance. C's client-owned source cannot be tied to B, so source/ownership attribution is still missing; this is an oracle gap, not a confirmed RTL violation. |
| Release/Data | Data-less Release validates this DUT's full-line size, 64-byte line-aligned raw address and shrink `param` subset 0..2 before acknowledgement; `corrupt` is ignored because it is not a data-less Release validity field. ReleaseData verifies each byte against an immutable declared line or reference memory before changing backing memory, then emits ReleaseAck. Raw address, base, size, source, param and beat-count consistency reject interleaved/inconsistent data streams. | `corrupt` is per-beat and not a ReleaseData stability field; TileLink user/echo fields are DUT-specific and not required stable. Data-less Release user fields are not validated. |
| C under stall | A snapshot includes all sampled C fields and data. Dropped valid or a changed payload after a stalled cycle fails. Target release windows count attributed stalls and comparisons. | Stability between stalled cycles is distinct from multibeat identity across accepted beats. The latter is incomplete. |

All DCache lifecycle details above come from
[dcache_agent.inc](../cpp/environment/dcache_agent.inc), especially
`capture_before_tick`, `update_after_tick`, `respond`, `request_probe`,
`accept_probe_response`, `accept_release`, and the idle predicates. A manager
response drain does not imply cache-byte observation or coherence closure.

For D/E timing, the checked-in
[TileLink monitor `legalizeDESink`](../../../rocket-chip/src/main/scala/tilelink/Monitor.scala)
sets a sink in flight on the first accepted D beat and permits E in that same
cycle or while the sink remains in flight. Thus an early E after the first
beat can be legal; requiring final-D acceptance would be an invalid strengthening.
The missing UT check is accepted-first-D provenance and sink uniqueness through
E, not a blanket ban on early E. The presence of this source does not establish
that a particular generated runtime instantiated or enabled that monitor.

A PBMT=NC `CBO.ZERO`/CBO probe is not an oracle in this UT: the interface
reports a store access fault before any uncache request, while the architectural
behavior for a CBO to non-cacheable memory is unspecified. The UT therefore
does not assert either a fault or a sequence of uncached writes for that case.

### PTW and Uncache managers

| Channel | Active check/model | Missing check or scope limit |
| --- | --- | --- |
| PTW A/D | [ptw_agent.inc](../cpp/environment/ptw_agent.inc) accepts Get/AcquireBlock up to size 6, snapshots response bytes at construction, emits calculated beats and checks its driven D identity/payload. A per-source table rejects source reuse before the final D and a D that completes an inactive source. Outstanding count falls on final D; error injection supports selected/persistent target responses and first/last/all corrupt beats. | Still lacks a complete A alignment/mask/user legality check and deliberate duplicate/early/late/malformed D campaigns. Source lifetime is checked, but malformed manager input remains outside the legal-response generator. |
| Uncache A/D | [uncache_agent.inc](../cpp/environment/uncache_agent.inc) accepts PutFull/PutPartial/Get with at most an 8-byte beat; checks natural alignment, beat bounds and exact modeled mask; compares its driven D identity/data/error flags. A per-source table rejects reuse before D and a D completing an inactive source. Gets return full aligned beats; successful masked writes update backing bytes; denied writes do not. | Still lacks a full A param/corrupt rule and independent per-instruction A-data comparison. Source lifetime is checked; response-order permutations and malformed-response campaigns remain absent. |
| Device side effects | The Uncache device-window transcript records accepted read/write fields and response errors. Successful read-clear zeros only selected bytes; denied/corrupt reads do not clear; denied writes do not update. | A transcript is evidence only when a scenario compares it to its declared device operations. General ordered-indexed vector side effects are not established by ordinary memory bytes. |
| Delays/backpressure | Managers retain a presented response until handshake and vary pre-response/interbeat gaps. | This legal response producer does not test all illegal manager behavior or establish an unlimited DUT progress bound. |

### Assertions and queue/cancellation attribution

The checked-in [protocol SVA](../sv/memblock_protocol_sva.sv), generated by
[generate_sva.py](../scripts/generate_sva.py), checks valid retention and payload
stability for the manifest-derived DUT-output ready/valid producer interfaces.
It covers the selected translation and TileLink producer bundles; it is not a
general assertion for every top-level pulse or manager-driven input. A separate
StoreQueue bind checks preservation of an already-valid address for the same
entry identity across a TLB-miss update. That is an internal implementation
assertion, not an independent expected architectural state. Both depend on an
assertion-enabled build containing these binds.

[QueueAccounting](../cpp/environment/queue_accounting.cpp) compares observed
enqueue totals to driver allocation after public operations. Circular LQ/SQ/ROB
value/flag pairs are calculated from absolute cursors in
[model.inc](../cpp/environment/model.inc); this is stimulus legality.
`run_until_queues_retired` waits until aggregate dequeue plus cancellation
reaches allocation; its local final comparison checks only for a shortfall,
not an excess. Exact equality is checked separately by selected scenario gates.
A known redirect is driven, then retained cancellation counts are sampled after the driver's fixed interface latency; those are aggregate output
counts, not transaction IDs. Software cancellation is separately counted as
unobserved. Reset cancellation of a scalar scoreboard entry requires the
just-completed reset boundary and a matching live obligation. It does not create
a general reset/redirect identity ledger. See
[completion.cpp](../cpp/environment/completion.cpp) and
[observations.cpp](../cpp/environment/observations.cpp).

The constrained-random
[finish gate](../cpp/scenarios/mixed_reporting.cpp) checks requested action
count, an empty expected GrantAck queue, queue balance, at least one observed
redirect, observed cancellation totals with no software-only cancellations, and the
expected observed Probe source lifecycle. These are the checks actually invoked
there; broad data visibility and coverage completeness must not be inferred.

## Planned Oracles for Current Boundary Gaps

These are missing or incomplete checks, not guarantees already enforced.
They must be designed against the exposed architectural/protocol contract
before their absence can be removed from the catalog.

| Gap | Check still needed | Current evidence boundary |
| --- | --- | --- |
| Remaining aliased and non-cacheable final visibility | Force external observation of final bytes for the remaining non-identity translations and uncached/device targets, including the last generated update. | Cacheable identity-mapped commits use the final readback; permitted/aligned hypervisor cacheable stores use a same-context HLV readback, high-VA segment stores use element readbacks, and `translation-alias` checks one cross-alias path after drain and with SBuffer non-empty at an in-flight load issue. Other aliases are counted as skips; committed non-cacheable words are checked against the manager bus image, while device side-effect and CMO semantics remain scenario/transcript-owned. |
| Exceptional vector state | Populate independently derived fault `vstart` where required; identify which exceptional metadata/data fields have a stable boundary contract. | Explicit checks now cover `vstart=8` in a two-uop page fault, `vstart=1` for single-uop later-element and masked page faults, and an all-masked unmapped control that observes no manager traffic. Default exceptional paths still skip `vstart`; the intra-uop page-crossing faulting-VA payload is now checked after the feedback settles. |
| Accepted-work identity | Match accepted scalar/vector issue, terminal completion and legal cancellation using a complete per-operation ledger. | Load/vector scoreboards register declarations; store issue is partly tracked; queue cancellation is aggregate. |
| Store-data pulse attribution | Distinguish unsolicited, stale and duplicate bare data pulses by an observable acceptance epoch/identity. | Unmatched data pulses are currently ignored. |
| B/C causal lifecycle and C multibeat identity | Prove ProbeAck follows accepted B and preserve every required accepted multibeat field. | E first-D/sink lifetime is now checked; B/C acceptance provenance and complete C multibeat-field comparison remain incomplete. A two-probe same-address experiment could not be uniquely matched by address/opcode/report, confirming that source/ownership attribution is still missing. |
| PTW/Uncache malformed response campaigns | Add bounded legal/illegal duplicate, early, late and malformed response campaigns. | Per-source credit/lifetime and queued legal response construction exist. |
| Fault side-effect policy | Unify request class/address attribution and legal early demand across fault scenarios; prove forbidden committed bytes remain unchanged with sufficient external observation. | Hypervisor store suppression now attributes CPUStoreData requests; helper-based exemptions and mixed total-request policies still coexist elsewhere. |
| Full vector masks and ordered effects | Model all mask bits of larger register groups and observe ordered indexed transactions to a side-effecting translated target. | Bits 0..127 are modeled and driven, including directed high-bit load/store/indexed cases; ordered device effects and every reserved shape remain outside the claim. |
| Atomic ordering/reservation | Model broader concurrent memory order and cross-hart reservation interference without internal reservation state. | Current successful mixed sequences and terminal values do not establish these interactions. A single-atomic-then-younger-store directed attempt leaves the store in the Sbuffer; the harness lacks the atomic ROB-commit/SQ-drain model, so atomic-before-store/CBO ordering remains unexecuted. |
| Architectural translation control | Exercise software-visible CSR/WARL and hardware A/D-update behavior through an appropriate integration boundary. | This UT supplies post-CSR context and models fault-on-clear A/D policy. |
| Wider coherence/reset/error state | Extend independently owned permission/data and cancellation models to additional overlapping coherence errors, reset points and dirty persistence. | Existing selected cases and aggregate counts are not a complete state model. |

The verification plan owns prioritization and required execution for these gaps.
No case count, duration, result artifact, or observed bin alone converts them
into implemented checks.

## Anti-Oracles

The following cannot independently establish architectural correctness:

- Copying a DUT-translated address, decoded operation, mask, internal cache
  image or replay decision into the expected result.
- Treating a reference-memory write, driver commit pulse, queue dequeue or
  generated coverage increment as proof that intended bytes became visible.
- Accepting correct data with the wrong identity, allowing a declared fault to
  hide a wrong cause, or dropping a completion without a legal cancellation.
- Requiring a fixed execution cycle, arbitration winner, bank, cache residency,
  replay count, wakeup count or hardware-prefetch count without an explicit
  interface contract. Existing implementation-specific gates remain identified
  as such, including the scalar guest-fault feedback gate.
- Counting manager-driven D-field consistency as an independent DUT-result
  comparison, or interpreting stalled-payload stability as complete multibeat
  transaction/lifecycle checking.
- Modifying backing memory behind a potentially cached line and assuming the
  next load must see it without a modeled invalidation/coherence action.
- Counting reset-held pin activity or a selected input class as a checked
  semantic operation.

## Implementation Limitations

The preceding tables describe local limitations. The following result-level
limits are particularly important when reading PASS output.

| Surface | Actual behavior | Unsupported inference |
| --- | --- | --- |
| Constrained-random result verifier | The default [verify_regression.py](../scripts/verify_regression.py) path calls `_check_mixed_accounting` for constrained-random results, checking queue balance and conditional LSQ/redirect accounting. `--require-coverage` instead invokes `_check_mixed_coverage` and `_check_constraint_coverage`. | The opt-in checker requires observed bins and the current constraint schema, but it remains observational; the strict-coverage target is not part of `final-acceptance`, and a nonzero bin is not a terminal data oracle. A 2×4608 coverage probe is rejected because enabled probe-depth classes 4..7 are unobserved. |
| Schema presence | `_check_mixed_accounting` accepts absent `lsq_monitor_schema` as 0, as well as 1 or 2; observed enqueue/cancellation fields are required only for the corresponding schema. The strict mode requires the current constraint coverage schema and arrays. | The default result path still does not require the current constraint schema; a parsed PASS on that path does not prove current-schema coverage fields or passive cancellation evidence were present. |
| Backpressure evidence | `--require-backpressure` alone checks campaign configuration. Combined with `--require-coverage`, `_check_mixed_coverage` also requires positive per-result backpressure bins. | The default path's configuration flag alone does not prove every requested stall/delay class occurred. |
| Summary/record consistency | The verifier reparses simulator summary, compares recorded fields, command/seed/scenario/counts and grant totals, and validates campaign timing/runtime layout. | Artifact consistency is not independent observation of DUT semantics; provenance is not coverage. |
| Coverage producers | [mixed_reporting.cpp](../cpp/scenarios/mixed_reporting.cpp) and [coverage.inc](../cpp/random/coverage.inc) combine generator selections and monitor statistics. | A generator-selected overlap/forwarding/shape or a passive pulse counter cannot substitute for a corresponding terminal/data/protocol comparison. |
| Final observation scope | `finish()` reads back tracked cacheable identity-mapped committed words after a Sbuffer/DCache/queue drain. Hypervisor permitted/aligned cacheable stores add a same-context HLV readback; high-VA segment stores have element readbacks. CBO.INVAL and errored CMO lines are dropped; successful CBO.CLEAN/FLUSH lines remain tracked so their final bytes are observed. Remaining aliased, uncached/device, fault-path and reset-interrupted effects are not claimed. `final_observation_observed_words` is incremented per successful batch and `finish()` gates equality with the registered count; a zero-word run is not behavioral evidence. | A pass proves those tracked/read-back words, not every modified byte outside those forms or any effect performed on a faulted/CMO-maintained line. |

The implementation also permits explicit data expectations and optional metadata
expectations, so oracle quality depends on the producer. The separate reference
and backing images reduce common dependence but do not remove implementation
assumptions in an individual producer. In particular, the exceptional atomic
private-cache image is not an independent architectural oracle.

A successful simulation therefore means its active checkers accepted its
executed traffic under their documented limits. It does not establish every
planned oracle, every legal input, every fault field, aliased or non-cacheable
final-byte visibility, or exhaustive protocol lifecycle coverage.

## Failure Classification

[FailureState](../cpp/failure.hpp) records component, code, message, optional
cycle and source site. Serialized records are explicitly `untriaged`; component
ownership is not RTL culpability. Records are deduplicated, capped at 64 and
report truncation. `error()` preserves the first message, so later distinct
failures require the structured records.

| Classification | Required distinction |
| --- | --- |
| Stimulus/driver failure | Illegal field/identity/context, incorrect issue/commit/replay/cancellation, exhausted construction space, or a deadline that cannot establish a violated external contract. |
| Model/oracle failure | Wrong independently computed pre/post-state, wrongly attributed metadata, implementation-dependent expectation treated as architecture, or a missing legal latency/order allowance. |
| Manager/monitor failure | Wrong response construction, source/beat bookkeeping, sampling edge, pulse attribution, or manager-owned transcript. A manager-contract failure message alone does not select the culprit. |
| Provenance/build failure | Runtime, interface, configuration or source mismatch prevents a valid comparison. Reproduction follows [README.md](../README.md#reproduction). |
| Untriaged DUT-contract candidate | Deterministic replay still violates an externally observable rule after stimulus, manager, model, monitor, and runtime explanations are excluded. |
| Confirmed RTL violation | The primary agent establishes the external contract violation and root cause; a failed checker, result row or worker finding alone cannot make this classification. |

Retain the exact scenario, seed, arguments, runtime identity and decisive
accepted-event/completion transcript needed for replay. Add address, identity,
walk and memory evidence when the failing path actually captured it; neither
`FailureState` nor every scenario automatically records a complete transcript.
Reduction must retain the violated comparison and its legal prerequisites.
A coverage omission is reported as an unverified capability, not fabricated
into an RTL failure or silently converted into coverage closure.
