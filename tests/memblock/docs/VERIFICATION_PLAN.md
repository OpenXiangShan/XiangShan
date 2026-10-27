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
| Scalar/FP loads | Width/sign/lane/immediate, FP boxing/destination, data/errors; load_feedback.inc, scoreboards | Wider timing/error interactions |
| Stores/forwarding | Address/data order, masks, commit, cross-producer forwarding, directed readback, constrained-random final word readback for cacheable identity-mapped commits and same-context hypervisor readback for permitted/aligned cacheable stores | Output-only pulses, remaining aliased/non-cacheable/fault-path final state |
| Vector addressing | Unit/strided/indexed/segment/whole-register shapes, data/identity/policy, full 128-bit mask operand with directed high-element load/store/indexed cases; cache_queue.inc, vector_memory.inc | Ordered side-effect targets, all concurrent shapes, and reserved/illegal mask-shape combinations remain |
| Vector faults/FOF | First/later faults, prefix data, selected VA/GPA, final VL/fix completions, a two-uop page fault with `vstart=8` and exact VA, single-uop later-element/masked `vstart=1` faults with exact faulting-element addresses, masked, unmasked and first-element vector store page faults whose inactive/read-only/writable target pages are read back unchanged, a partially masked load with an inactive unmapped element, and an all-masked unmapped control that observes no PTW/DCache traffic | Default exceptional vstart skip for other shapes, arbitrary restart/progress, broader fault-address attribution, and canonical-alias sensitivity |
| Translation | Mode pairs, permissions, leaf sizes, encoding/PPN/range, PBMT/NAPOT, permission-fault and PMP-denied store readback (readable mapping, Bare fallback, or re-enabled PMP region) | Uniform fault-side-effect checks for unreadable/device mappings, nested boundaries/GPA wrap/A-D races |
| Protection/H | PMP TOR/NAPOT/lock/edges, HLV/HLVX/HSV; protection.inc | Instruction PMP and broader PMA/PMP/H/misalignment crosses |
| Refill/MLP | Critical beat, merge, distinct misses, held/reversed D, capacity-plus-one, DCache/PTW/Uncache source lifetime | Broader error/interleaving and malformed response lifecycle |
| Coherence/replacement | Clean/dirty Probes, an eight-deep probe window, Releases/data, replacement plus selected Probe/CMO/atomic, E first-D/sink lifetime, accepted-B-before-C provenance, fixed C multibeat source/param/size/raw address, DUT full-line data-less Release size/alignment/shrink param | TileLink user/echo C fields are DUT-specific; data-less Release corrupt is ignored; same-address B/C attribution and broader ordering remain |
| Atomics | W/D AMO/CAS/LRSC, alignment/errors, reservation Probe, directed readback, successful cacheable final word readback, and forced dirty-line ProbeAckData persistence | Aliased/error-path final writes not forced observable, cross-hart/wider concurrency |
| CBO/CMO | ZERO/CLEAN/FLUSH/INVAL, clean/dirty/error, delayed ack, younger cancellation, single/sequential/back-to-back/same-line-repeated/three-line CBO.ZERO completion, same-line zero/store both orders, an older-store/younger-zero/younger-load forward window, a zero/younger-store/younger-load age control and an early younger load (see CPU bug doc) | Multiple sources/classes, dirty persistence/reset, PBMT=NC behavior unspecified, wider same-line forward-timing variants |
| Uncache/MMIO | Width/mask/data, device side effects, errors, serialization | Source credit and malformed manager inputs |
| ECC | Selected tag/data masks, banks/ways, reports/survivors | Wider masks, persistent/delayed events and concurrency |
| LSQ/ROB/redirect | Six-lane enqueue, aggregate retire/cancel, wrap/stale suppression | Full per-retirement attribution and output-only pulse identity |
| RAW/RAR/feedback | Selected sources/arbitration/metadata; load_feedback.inc | All source crosses and stable architectural feedback rules |
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

minimum_serial_actions() reserves budget before heterogeneous windows. Its
unmodified-profile floor is 4362 for coverage/corner and 4246 for spec. Thus
256/1024-action passes do not prove heterogeneous overlap. Inspect observed
windows/uops, manager depth and backpressure instead of configured weights.

## Top-Level IO and Interface Assumptions

Legal stimulus respects decoded opcodes, consistent ROB/LSQ identities,
valid-payload stability, architectural age/commit order, translation context,
legal vector shapes and finite manager responses. AMOs execute at the backend
non-speculative point; redirect-canceling an executing AMO is illegal. Segment
serialization is an upstream assumption. Younger stores cannot become ROB
head/commit while an older operation must retire first.

pin-space drives raw patterns under primary reset. It proves connectivity,
not semantics. Exported pins are configuration-specific; pruned Scala bundle
members are not controllable ports. IO_COVERAGE.md is the semantic map.

## Known Boundary Gaps

Priority work, each requiring executable stimulus and an independent check:

1. Extend final observation beyond cacheable identity-mapped commits to the
   remaining legal aliased translations, non-cacheable/device effects and
   fault-path side-effect checks; one cross-alias path is now directed, but
   first-access cancellation is detected and LQ-accounted, but the backend replay that follows it is still not modeled. Keep the CMO
   exclusion explicit. Single-uop later-element and masked page faults now
   require `vstart=1`, and an all-masked unmapped load must complete without a
   fault while the run observes no PTW/DCache request. The intra-uop
   page-crossing faults also publish the exact faulting-element address after
   the vector feedback settles. Extend this to broader exceptional `vstart`,
   fault address attribution, and forbidden-demand attribution.
2. Preserve the accepted-B-before-C probe provenance now implemented; add
   complete C multibeat identity checking. Retain the DCache E first-D/sink and
   PTW/Uncache source-lifetime checks.
3. Repair verifier schema/backpressure/statistic validation and report
   namespaces across controllers.
4. Expand the now-full 128-bit vector mask to more fault/index/mapping and
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

## Independent Review and No-Progress Audits

Material oracle/monitor/distribution/coverage/plan changes receive fresh-context
non-author review before closure-scale regression. Review uses the actual diff
and source contracts, dispositions every finding, and records only current
verdict/scope in RESULTS.md. Author conclusions are not fixed reviewer inputs.

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
bin sparsity. The
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
