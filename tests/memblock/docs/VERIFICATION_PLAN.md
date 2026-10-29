# MemBlock Verification Plan

## Objective

Verify externally observable memory behavior: correct data and permissions,
identified exceptions, exactly-once completion or legal cancellation, queue
conservation, legal memory protocols, and reset/redirect/translation recovery.
This block-level UT does not execute a full instruction stream or replace
full-core, multi-hart, physical-memory or four-state verification.

[DESIGN.md](DESIGN.md) describes the design and legal boundary inputs.
[IO_COVERAGE.md](IO_COVERAGE.md) maps retained ports to actual stimulus.
[ORACLES.md](ORACLES.md) separates implemented checks from missing rules.
[CONSTRAINED_RANDOM.md](CONSTRAINED_RANDOM.md) defines the current generator.
[RESULTS.md](RESULTS.md) is the current qualification snapshot. Source symbols
and the catalog, rather than a passing summary alone, establish executability.

## Translation Specification Scope

The defaults in src/main/scala/xiangshan/Parameters.scala enable H and Sv48;
cache/mmu/MMUConst.scala defines mode encodings. The independent model
supports Bare, Sv39, Sv48, VS-only, G-only and these nested pairs:

- Sv39 -> Sv39x4
- Sv39 -> Sv48x4
- Sv48 -> Sv39x4
- Sv48 -> Sv48x4

Canonicality, per-byte range validity, PTE encoding/permission, superpage
alignment, PPN reachability, PBMT and selected NAPOT behavior are calculated in
cpp/environment/model.inc. G-stage x4 uses a 16-KiB root; Sv48x4 has a
50-bit GPA limit. Builders supply page tables and drivers supply post-CSR
controls. SFENCE.VMA, HFENCE.VVMA and HFENCE.GVMA are decoded inputs; selected
stale-response races are checked. Architectural CSR WARL and full-core
retirement are outside that claim. Platform PMP grain is 4 KiB: NA4 unselectable
is the pmp-contracts WARL expectation.

### Translation Closure Phases

| Executable layer | Present capability | Missing qualification |
| --- | --- | --- |
| Walk model/builders | Stage-1/G/nested walks and explicit mappings | More independent permission/encoding mutation checks |
| Address/permission scenarios | Cause/VA/GPA, translated data, selected request suppression | Uniform forbidden-demand checks and broader multi-cause priority |
| Fence/context scenarios | Held responses, mode/ASID/VMID changes, survivors | Wider simultaneous walks, A/D and invalidation races |
| Common generator | Weighted contexts/leaf/fault/boundary/fence traffic | Nonidentity ordinary-vector crosses and observed-bin analysis |

These are capability layers, not four completed closure stages.

## Stable Oracles

Expectations come from transaction inputs, testbench-owned byte memory/page
tables and external handshakes. Responses are matched by transaction identity.
Legal latency/background traffic is allowed. Internal mem_direct observations
localize errors, not architectural PASS. Current exceptions to that ideal,
including diagnostic pulse gates and private poisoned-cache modeling, remain
explicit limitations in ORACLES.md.

## Complete Verification-Point Inventory

Implemented subset means a concrete producer and check exist for the named
subset. Partial means fields, interactions or independent checks remain
missing. Integration-owned requires a wider boundary. None means all legal
combinations have occurred or establishes a coverage percentage.

| Domain | Executable subset and source | Remaining scope |
| --- | --- | --- |
| Scalar/FP loads | Canonical integer/FLH/FLW/FLD format selection in ordinary/concurrent/SQ-dependency/unresolved graphs, byte-wise memory and older-store overlay boxing, payload sign bins, class/destination bounds and strict correct-data refetch; directed selected NC/MMIO/fault cases | Complete rename allocation history, other helper formats and exceptional misaligned FP enable semantics; wider timing/error interactions |
| Ordinary mixed execution | Weighted variable scalar/FP load, vector load, scalar store, vector store and prefetch composition; configured 1..4096-instruction epochs (default draw 8..256) and 2..8 live instructions; ready-event operand/uop/retirement scheduling with per-store ordinary STA feedback/retry state, sliding admission after prefix retirement, randomized integer-load-to-address/scalar-store-data edges with checked producer lifetimes, independent admission-order byte overlays, exact replay, bounded retained-prefix scalar/FP recovery with an optional held cold role and final readback; mixed_stream.cpp | Epoch/context drains, full rename/cancellation history, broader unfinished sliding overlap, general vector-result/index dependencies, unbounded histories and special/atomic/CMO/fault overlap |
| Stores/forwarding | Random 1..4 older-store/1..3 younger-load chains with shuffled operand arrival, partial byte overlays and optional younger-store exclusion; address/data order, masks, commit, cross-producer forwarding, directed readback, constrained-random final word readback for cacheable identity-mapped and declared permuted-page commits and same-context hypervisor readback for permitted/aligned cacheable stores | Output-only pulses, remaining aliased/non-cacheable/fault-path final state. A naturally aligned cross-page scalar store is not modeled as one legal top-level uop; the backend split is absent, so cross-page scalar split-store fault effects are untested and a single-uop completion is not an RTL claim. |
| Vector addressing | Unit/strided/indexed/segment/whole-register shapes, bounded random index spacing/permutation and repeated gathers with disjoint scatter spans, class-bounded distinct per-uop destinations, repeated-Vd source chains and randomized dependency-preserving indexed issue, data/identity/policy, full 128-bit mask operand with directed high-element load/store/indexed cases; cache_queue.inc, vector_memory.inc | Ordered side-effect targets, all concurrent shapes, and reserved/illegal mask-shape combinations remain |
| Vector faults/FOF | First/later faults, prefix data, selected VA/GPA, final VL/fix completions, a two-uop page fault with `vstart=8` and exact VA, single-uop later-element/masked `vstart=1` faults with exact faulting-element addresses, legal two-uop segment noncanonical loads/stores whose later uop must expose the fault with no forbidden traffic, masked, unmasked, first-element, segment (field-1, element-1 and first-field) PMP-denied masked/unmasked vector loads, and PMP-denied (including masked, cross-page, both segment field orders and EEW 1/2) vector store faults whose inactive/read-only/writable target pages are read back unchanged, a partially masked load with an inactive unmapped element, and an all-masked unmapped control that observes no PTW/DCache traffic | Default exceptional vstart skip for other shapes, arbitrary restart/progress, broader fault-address attribution, and canonical-alias sensitivity. Per-uop success order is not an architectural oracle when a later uop of the same instruction exposes the fault before commit. |
| Translation | Seeded ordinary host/composed VS/G 4-KiB unrestricted page permutations, immutable per-byte data maps and physical final readback; covering root-reuse fences; mode pairs, permissions, leaf sizes, encoding/PPN/range, PBMT/NAPOT, permission-fault and PMP-denied store readback (readable mapping, Bare fallback, or re-enabled PMP region) | Cross-colour grants use old-owner recall and independent drained checkpoints; arbitrary simultaneous synonym writes and PTE/root/identifier churn remain outside static maps. Uniform fault-side-effect checks for unreadable/device mappings, nested boundaries/GPA wrap/A-D races |
| Protection/H | PMP TOR/NAPOT/lock/edges, HLV/HLVX/HSV; protection.inc | Instruction PMP and broader PMA/PMP/H/misalignment crosses |
| Refill/MLP | Critical beat, merge, distinct misses, held/reversed D, capacity-plus-one, DCache/PTW/Uncache source lifetime | Broader error/interleaving and malformed response lifecycle |
| Coherence/replacement | Clean/dirty Probes, an eight-deep probe window, Releases/data, replacement plus selected Probe/CMO/atomic, E first-D/sink lifetime, accepted-B-before-C provenance, fixed C multibeat source/param/size/raw address, DUT full-line data-less Release size/alignment/shrink param | TileLink user/echo C fields are DUT-specific; data-less Release corrupt is ignored; PA/VA-alias directory, deferred grants and old-owner data recall use drained reference checkpoints; same-PA B is serialized through final C without B/C source equality; arbitrary simultaneous synonyms/error-grant/CMO ownership and broader ordering remain |
| Atomics | W/D AMO/CAS/LRSC, alignment/errors, reservation Probe, directed readback, successful cacheable final word readback, and forced dirty-line ProbeAckData persistence | Aliased/error-path final writes not forced observable, cross-hart/wider concurrency. Atomic-then-store/CBO ordering is not yet executable: the current single-atomic harness leaves the younger store in Sbuffer without a writeback, so no RTL claim is made. |
| CBO/CMO | ZERO/CLEAN/FLUSH/INVAL, clean/dirty/error, delayed ack, younger cancellation, single/sequential/same-line-repeated/three-line consecutive CBO.ZERO completion, same-line zero/store both orders, an older-store/younger-zero/younger-load forward window, a zero/younger-store/younger-load age control and an early younger load | A cacheable CBO.ZERO remains the ROB head until cboZeroStout after SBuffer drain; two CBO.ZEROs cannot legally occupy the same deferred-completion window. Multiple sources/classes, dirty persistence/reset, PBMT=NC behavior unspecified, wider same-line forward-timing variants remain |
| Uncache/MMIO | Width/mask/data, device side effects, errors, serialization | Source credit and malformed manager inputs |
| ECC | Selected tag/data masks, banks/ways, reports/survivors | Wider masks, persistent/delayed events and concurrency |
| LSQ/ROB/redirect | Six-lane enqueue, aggregate retire/cancel, wrap/stale suppression | Full per-retirement attribution and output-only pulse identity |
| RAW/RAR/feedback | Random aligned 1..4-older-store/1..4-load unresolved-address conflicts, surviving disjoint prefixes and whole-suffix refetch, plus integrated retained-prefix sliding recovery and all-disjoint controls with producer-ready event order and youngest-writer byte overlays; isolated touched-word readbacks and integrated global final committed-image readback; strict warm initial and known-address epoch data, optional youngest held cold-load cancellation/late-response drain, matching current FTQ flush-itself return and repeated frontend suffix refetch before store commit; selected sources/arbitration/metadata; mixed_unresolved.cpp, load_feedback.inc | General unfinished/late-response overlap beyond one youngest held cold role, arbitrary predictor histories, broader unfinished sliding overlap, trained prediction, general redirect arbitration, all source crosses and stable architectural feedback rules |
| Reset | Manager/frontend, cold atomic, partial LR/LD, clean CMO | Partial modifying/error refills, dirty state, more replay producers |
| Bridges/control | Frontend TL/PTW, trace/DFT/reset, interrupts, L2/TLB/hint/flush/WFI | Matching-MSHR/external consumer policy and dynamic controls |
| Performance/prefetch | Transport, characterization, selected suppression | Integration-owned meaning/training/selection |
| X/physical arrays | Reset-held pin connectivity only | Four-state storage, physical SRAM/MBIST |

## Stimulus Matrix

cpp/scenarios/catalog.json registers 91 scenarios: 90 directed-tests and
one constrained-random. The 85 entries with benchmark_order are selected
by the default benchmark. Six registered directed cases require explicit
invocation: cmo-store-flush, redirect-store-flush, tag-ecc-ways,
vector-fof-high, vector-segment-noncanonical, translation-alias. The benchmark script cannot
select entries outside its own list. Its inventory unit test compares only
that selected subset.

| Source module | Registered purposes |
| --- | --- |
| control.inc | Smoke/pins, top/L2/trace/DFT, frontend/reset |
| load_feedback.inc | Loads/FP, feedback, RAW/RAR, prefetch, trigger/metadata |
| manager_errors.inc | Cache/atomic/uncache errors, widths/outstanding, SBuffer/MBMC/tag ways |
| cache_queue.inc | Coherence/replacement, issue/redirect/pressure, split, vector shapes/exceptions |
| maintenance_reset.inc | MMIO/CBO/CMO, WFI/reset, younger-store cancellation |
| vector_memory.inc | Load/split/FOF/segment, forwarding, high/noncanonical VA |
| protection.inc | PMP/H/pointer masking/L2 DTLB |
| translation_context.inc | IFU PTW, stages, fences/inflight/context |
| Translation fault/permission/guest modules | PTE/range/permission/error, guest-address contracts, and cross-alias store/load visibility |
| constrained_random.inc, mixed_*.cpp | Common generator and weighted policies |

New directions normally extend common constraints. Focused scenarios serve
distinct contracts. --list-tests and the catalog are the exact inventory.

## Constrained-Random Distribution

coverage/spec/corner/cmo-stress share one generator. Actions are not bus
requests and can contain multiple uops. No complete-bin prefix or
coverage-completeness PASS gate is active. Coverage is observational;
functional/identity/queue/implemented-protocol checks stay active.

minimum_serial_actions() estimates enabled serial dimensions: 4362 for
coverage/corner and 4246 for spec. Its scheduling reservation is capped at half
the initial available action budget, keeping ordinary concurrency reachable in
short runs; sufficiently large runs retain the full estimate. Inspect observed
windows/uops, manager depth and backpressure instead of configured weights.
An allocated concurrency budget does not establish actual overlap.

## Top-Level IO and Interface Assumptions

Legal stimulus respects decoded opcodes, consistent ROB/LSQ identities,
paired ordinary-store STA/STD issue lanes, valid destinations for simultaneously
live dependency loads, valid-payload stability, architectural age/commit order, translation context,
legal vector shapes and finite manager responses. AMOs execute at the backend
non-speculative point; redirect-canceling an executing AMO is illegal. Segment
serialization is an upstream assumption. A legal segment has at least two data
uops; the UT may allow the later uop to expose the instruction-level fault but
must not claim an impossible one-data-uop shape. Younger instructions cannot
become the ROB pending head while an older operation must retire first. In
particular, a younger CBO.ZERO cannot become head while the older CBO.ZERO is
waiting for cboZeroStout and SBuffer drain.

pin-space drives raw patterns under primary reset. It proves connectivity,
not semantics. Exported pins are configuration-specific; pruned Scala bundle
members are not controllable ports. IO_COVERAGE.md is the semantic map.

## Known Boundary Gaps

Prioritize gaps that ordinary user workloads can reach: scalar/FP values and
byte forwarding, older-store/younger-load dependencies, address translation,
cache refill/replacement, overlapping queues and observable committed bytes.
Use the canonical generator for new directions and measure actual structural
variation. Stream-only profiles can select ordinary producer subsets, including
scalar/FP dependency traffic and full-capacity vector loads without mandatory
legacy producers; report the actual accepted class mix and queue activity. Rare PMP/H/error/CMO combinations follow these unless they block a
correct oracle or reproducible failure. Neither a template count nor a larger
constraint set guarantees a design bug.

The common scalar dependency direction now varies store/load counts, overlap,
operand arrival and issue order with independent byte-overlay expectations and
younger-store exclusion controls. Seed/address-dependent ordinary initial bytes
also distinguish representative wrong-line substitutions. Next normal-workload
gaps include broader unfinished unresolved cancellation/late-response overlap and longer dynamic
address/temporal streams and mapping/invalidation overlap. Ordinary static
4-KiB nonidentity mappings now use seeded host/VS/G page permutations with
byte-wise load/store expectations, covering root-reuse fences and physical final
readback. Permutations may change cache colour; the manager recalls the stored
old alias and checks its data against an independent drained reference checkpoint
before constructing the replacement grant. Context changes drain SBuffer/D/E;
general simultaneous synonym writes and cross-alias replay stay outside this
direction. A bounded 1..4-older-store/1..4-load direction now checks provisional
execution with all addresses unresolved, randomized operand-ready arrival,
per-STA oldest-victim selection, surviving prefixes, repeated known-address suffix images and all-disjoint
byte-mask controls. A conditional youngest cold role stays pending through the first
redirect, consumes an observed positive LQ cancellation and drains its old D/E
lifecycle before same-identity strict refetch; unavailable fresh-line attempts
remain explicit. Expected byte ownership follows program age, and every
touched word receives a strict terminal readback. Bounded random indexed vectors
now vary offset spacing/permutation and repeated gathers for ordinary and
segment operations; partial byte overlap and overlapping scatters remain open.

Ordinary scalar streams return to event selection after an accepted STA rather
than synchronously waiting for its slow feedback. One per-store attempt cursor
matches lane/SQ value/wrap, preserves miss cooldown and suppresses retries after
hit; independent ready work can progress meanwhile. `stream_sta` measures
accepted admission and producer events during those waits. The fixed 16-cycle
enqueue settle, blocking issue handshakes/retirement and synchronous unresolved
graph operands still restrict density. Source audit finds no ordinary receiver
minimum enqueue gap; removing that wait requires explicit retirement-credit
maturity and redirect-rollback guards because acceptance is producer-owned and
there is no generated ready pin. Same-cycle heterogeneous issue and decoupled
commit/SQ lifecycle remain high priorities.

Ordinary mixed streams vary producer counts and instruction ages, choose ready
operand/uop and completed-head retirement events, and admit new work after a completed prefix retires while
younger instructions remain live. Epochs are randomly placed among serial
helpers. A zero `concurrent-stream` rate retains the fixed legacy prefix;
legacy windows also remain for clean NC/MMIO overlaps. Dispatch now freezes scalar-store opposite-
queue value/wrap snapshots; selected store/vector-list waits retain unrelated
obligations. Heterogeneous head movement waits for older special-load completion,
and vector retirement uses recorded SQ targets without scalar commit pulses.
The stream holds a single architectural head and independent byte-prefix oracle,
with distinct live class destinations. Ordinary admissions can independently
sample address and scalar-store data edges from older live integer loads.
Reference-predicted results select page-bounded ALU addresses/data; checked
producer completion gates each operand, with lifetime retained across producer
retirement. Seeds vary edge source, direction and chain depth. General vector
result/index chains remain absent. Its configured 1..4096-instruction epochs (default draw 8..256), 2..8 live
depth and conservative older-address gating do not implement a complete
ROB/rename/cancellation ledger. The integrated unresolved graph now retains an older mixed prefix, limits
conflicts to a non-store actual head/anchor, services prefix producer progress,
checks observed queue recovery and strictly refetches whole scalar/FP suffixes
without episode drains. It resumes ordinary admission while graph roles retire.
Execution resolution and final committed observation remain separate obligations.
A conditional youngest cold role can also remain pending in this integrated
graph. Freshness excludes accepted A coverage of every line byte, explicit
reference writes and older admitted store bytes, so no SBuffer drain is needed.
Its exact observed cancellation boundary and strict later refetch share the
independent epoch ledger. Target-line old Grant completion through both final D
and E plus 32 quiet cycles precedes same-identity refetch; prefix producers and
retirement remain eligible. That quiet interval is a bounded witness, not a full
CPU pipeline-empty proof. Next ordinary-workload work should interleave broader
unfinished recovery and early-release/late-response overlap, model store-headed redirect/walk levels and producer
history, and reduce bounded stream/context drains where supported. Canonical FP formats now share
ordinary/concurrent/dependency/unresolved builders with independent value/boxing
checks. Vector-data/VL destinations stay within their separate default files;
repeated-Vd indexed loads use distinct renames, independently modeled prior
source bytes and exact predecessor completion, while stores reuse their source.
Unordered indexed uop schedules vary subject to those dependencies; broader FP helper/error combinations remain explicit gaps. Dependency windows now interleave loads with unissued older STD operands after
all older addresses resolve; their independent byte overlays reject premature
old-memory data. Multiple speculative unresolved addresses remain separate from
this known-address direction. These gaps take priority
over expanding rare error/protection crosses; structural uniqueness alone cannot
guarantee a CPU bug.

Priority work, each requiring executable stimulus and an independent check:

1. Remove unconditional enqueue settling with independently tracked capacity,
   mature producer retirement credits and redirect-rollback guards; allow
   independent ready producers to progress across stalled peers and form legal
   dense/same-cycle issue combinations. Separate architectural store retirement
   from SQ dequeue where the CPU contract permits. Measure actual accepted port
   sets, consecutive-cycle work and committed-but-still-queued stores. Ordinary
   per-store STA feedback state removes one global wait; blocking handshake,
   retirement and unresolved-operand paths remain explicit work.
2. Extend unfinished cancellation beyond the bounded youngest cold role in
   isolated and integrated sliding graphs; add broader temporal/address dependencies,
   and extend the canonical FP checks beyond their four implemented builders. Extend final observation beyond cacheable
   identity-mapped and declared permuted-page commits to remaining legal aliases, non-cacheable/device effects and
   fault-path side-effect checks; one cross-alias path is now directed, but
   the first-access alias mismatch now requires its exact flush-itself request,
   observed queue recovery and correct-data frontend refetch. Its retained DUT
   case dequeues both LQ epochs with zero cancellations; nonzero-cancellation
   alias refetch still needs qualification, as does a correct-first-data alias
   redirect. General IQ/LQ replay and arbitrary
   refetch overlaps remain open. Keep the CMO exclusion
   explicit. Single-uop later-element and masked page faults now
   require `vstart=1`, and an all-masked unmapped load must complete without a
   fault while the run observes no PTW/DCache request. The intra-uop
   page-crossing faults also publish the exact faulting-element address after
   the vector feedback settles. Extend this to broader exceptional `vstart`,
   fault address attribution, and forbidden-demand attribution. The directed
   PMP page-crossing load now checks accepted demand through redirect recovery,
   allowing only its readable prefix line and independent prefetch; a cold
   both-readable control observes the second page and checks its returned data.
3. Preserve accepted-B provenance, same-PA serialization, C multibeat identity,
   DCache E first-D/sink and PTW/Uncache source-lifetime checks. Broaden legal
   ordering/reset qualification and independent owner-version handling beyond
   the checkpointed pool.
4. Repair verifier schema/backpressure/statistic validation. Regression and
   benchmark sidecars now use distinct campaign run-ID namespaces, including
   reruns that replace an aggregate file; the unique-parent controller-isolation
   requirement remains. Sidecar creation errors remain recorded
   diagnostics rather than an acceptance gate.
5. Expand the now-full 128-bit vector mask to more fault/index/mapping and
   concurrency crosses, plus reset/protection crosses.  A driver-truncation
   mutation is detected by the directed high-mask case; RTL mutation
   sensitivity of actual random paths remains open.

Malformed/duplicate/unsolicited responses, cross-hart interference, full-core
fences/CSR WARL, A/D races, four-state X and physical arrays are not closed.
More runtime cannot implement missing checks.

## Coverage Closure and Exit Criteria

Functional PASS means implemented rules accepted that run, not every enabled
class occurred. Code-defined bins/pin counts/shape matrices are not an overall
coverage percentage. Closure requires reviewed scope, independent effect
oracles, analyzed observed bins/crosses, meaningful mutation sensitivity,
reproducible completed campaigns and no unclassified failure. Partial/missing
rows remain visible. These are review obligations, not all active script gates.

Git tracks Chisel RTL sources and UT source/documentation, not generated
`build/rtl`, Picker/Verilator models, runtimes, traces, coverage sidecars, or
campaign artifacts. When a change creates useful or visible progress and the
focused validation appropriate to its risk passes, commit and push it in a
focused logical unit without waiting for overall closure. Do not commit a
failing, transient, unattributed, or generated intermediate state. The commit
must identify the relevant verification or review evidence.

## Independent Review and No-Progress Audits

Material oracle/monitor/distribution/coverage/plan changes receive fresh-context
non-author review before closure-scale regression. Review uses the actual diff
and source contracts, dispositions every finding, and records only current
verdict/scope in RESULTS.md. Author conclusions are not fixed reviewer inputs.

Every CPU RTL bug claim additionally requires a fresh-context independent review
of the complete CPU path that can produce the UT input. The review must prove
that instruction/uop decomposition, ROB commit and pending-head movement,
queue completion, and reset/redirect/cancellation timing can all occur as the
stimulus drives them. A deterministic UT failure caused by directly forcing a
state or ordering the full CPU cannot produce is a stimulus/driver defect and
must not receive a `CPU_BUG_*` document or confirmed-bug wording.

If a reachable class is absent, inspect eligibility/probability/scheduling and
observation before extending runtime. If capability does not improve over a
24-hour campaign interval, stop that campaign and perform a bounded
scope/oracle/stimulus audit. This is a workflow rule, not an automated timer.
Advancing action/cycle checkpoints provide progress evidence; CPU use alone
does not.

## Regression Acceptance

The Makefile configures these actual commands; execution alone is not closure:

| Target | Current workload / verifier settings |
| --- | --- |
| unit, check-rtl | Infrastructure tests; manifest/generated/schema/catalog/filelist checks |
| benchmark-tests | Selected 85, seed 1, budget 16384, 8 workers, 7200s timeout; catalog overrides |
| regression | 8 finite seeds, 16384 actions, default coverage |
| extended-regression | 21600s, 16384 actions, spec; verifier 128+ results, 8 workers |
| final-regression | 172800s, 16384 actions, spec, timeout 12000s; verifier 128+ results, 8 workers |
| long-final-regression | 43200s, 16384 actions, spec; verifier 128+ results, 8 workers |
| endurance-regression | 8 seeds, 1000000 actions each, timeout 720000s, spec; finite exact-result verifier |
| strict-coverage-regression | 2 finite coverage seeds by default (`STRICT_CONSTRAINTS`, optional `STRICT_CONSTRAINT` overrides), ACTIONS default 16384, 8 workers; strict coverage/backpressure verifier (opt-in) |
| probe-strict-coverage | 1 finite coverage seed, 4608 actions, `probe=1000`; focused standalone probe-depth/cross audit |
| balanced-strict-coverage | 1 finite coverage seed, 4608 actions, `probe=1000 canonical-boundary=500`; balanced probe/canonical-boundary audit |
| known-bug-sentinel | vector-guest-fault-split with exact successful external metadata signature |

The global ACTIONS default is 16384. It wins over the extended/long recipe's
fallback expressions, so both effective defaults are also 16384. An explicit
ACTIONS=8192 or ACTIONS=32768 changes both execution and the corresponding
verifier expectation; those values are not separate effective defaults.

final-acceptance runs unit/benchmark/sentinel/final+verifier/endurance+verifier;
not all 91 catalog entries. verify-final-results also runs dcache-release and
sentinel via the mutable build, not necessarily the campaign frozen binary.

verify_regression.py checks completion/run ID, finite timings/duration,
continuous seeds, command/terminal agreement, requested/completed actions,
aggregate totals, queue and GrantAck conservation, and selected runtime-layout
requirements. For constrained-random the default path calls accounting checks
only; `--require-coverage` opts into the full `_check_mixed_coverage` checker,
including the current constraint schema and complete coverage arrays. Combined
with `--require-backpressure`, that mode also requires nonzero per-result
backpressure bins. The new `strict-coverage-regression` target exercises this
opt-in mode on a finite campaign; `final-acceptance` still uses the default
path. A 2×4608 diagnostic run was functionally clean but failed strict coverage
because enabled probe-depth classes 4..7 were unobserved; a 1×16384 run was
also clean but still selected only six standalone probe sequences, so action
count alone is not the lever. That is the intended sparse-coverage rejection,
not a functional failure; `STRICT_CONSTRAINT='probe=500'` targets the depth
bin, and a 150-sequence probe-heavy run covered all eight depth classes but
still left some enabled probe-cap/need-data cross classes unobserved; a
317-sequence `probe=1000` run closed all probe bins but left one vector
canonical-boundary cross unobserved; the directed canonical-range matrix
already exercises that vector-store shape, so the residual is mixed-generator
bin sparsity. A balanced run covered that class but re-sparsified probe-cross
bins at 4608 actions; a longer mixed campaign is required. The
in-process final readback is a functional oracle before the summary is
written; the verifier does not re-derive its expected data. Runtime metadata
records paths/sizes/resolved dependencies, not source-to-model identity. These
remain acceptance gaps for closure.

Defaults are configurable. Report actual short-run budgets; they cannot
substitute for configured long campaigns. Current readiness is partial until
scope/oracle/acceptance gaps are addressed.

## Failure Triage

Replay exact binary/model/libraries, scenario/seed/profile/overrides/actions.
Separate argument/controller/build errors, timeouts, UT/manager/model/oracle
defects and external violations. Verify source packaging/model provenance.
Minimize without weakening the oracle. A shorter passing prefix does not
prove a longer timeout live. Only deterministic external violation after
excluding UT/provenance errors and localizing RTL justifies an RTL-bug claim.
