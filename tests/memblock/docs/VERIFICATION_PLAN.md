# MemBlock Verification Plan

## Objective

Verify the generated Kunminghu-v2 `MemBlock` against contracts that remain
valid across internal pipeline, queue, and state-machine refactors. Functional
acceptance is based on RISC-V-visible data/exception behavior and external
ready-valid protocols. Internal implementation signals may be used to localize
a historical mutation, but they are not substitutes for an architectural
oracle.

The catalog has exactly two test families: **84 `directed-tests`** and
**1 `constrained-random`** scenario. `make benchmark-tests` runs all 85 once;
it is a benchmark command, not a third family.

> **Contract note (2026-10-24):** random scenarios sample and report coverage
> but do not require coverage completeness for PASS. Historical "closes all
> bins" statements describe the older coverage-gated generator and are
> provenance, not current acceptance requirements.

The current generated RTL under test is validated from its complete ordered
file list and external interface manifest. Historical clean-baseline and mutant
revisions, including hashes in old reports, are provenance only. Source, binary,
and RTL hashes are not acceptance gates. A failure is reported as an RTL
candidate only after a deterministic replay excludes driver, manager,
reference-model, and scoreboard errors.

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

The correctness contracts, independent calculations, temporal and protocol rules,
oracle gaps, anti-oracles, implementation limitations, and failure classification
are defined only in [`ORACLES.md`](ORACLES.md#stable-oracles). This plan does not
restate or reinterpret them.

The `Stimulus Matrix` owns required stimulus and acceptance flow. The `Complete
Verification-Point Inventory` owns implementation status and gap traceability.
Oracle selection must always come from `ORACLES.md`, not from this plan.

A row marked `implemented` requires an observed result; `partial` and `planned`
retain an explicit limitation and cannot be promoted by a green run alone.

## Stimulus Matrix

This matrix owns the required stimulus and externally observable acceptance flow
for each operational area. It does not define an alternate oracle; calculations
remain in `ORACLES.md`. The inventory below owns implementation status and gap
traceability for the same verification points.

| Area | Required stimulus | Acceptance checks |
| --- | --- | --- |
| Reset | External reset, internal reset drain, idle interval | No terminal completion while idle |
| Frontend bridge | Concurrent ICache, ICache-control, and instruction-Uncache A/D traffic; credit-safe source wrap; Get/PutFull/PutPartial; 1/2/4/8-byte control requests; full/partial masks; two-beat ICache responses; corrupt instruction data; randomized producer/consumer stalls | Exact in-order field transformation across both buffer stages, no loss/duplication, request/response/source-credit stall coverage, SVA stability, and final quiescence |
| Scalar loads | `lb/lh/lw/ld/lbu/lhu/lwu`; all three lanes; aligned and misaligned | ISA extension, exact data, metadata, replay, exception, LQ drain |
| Scalar stores | `sb/sh/sw/sd`; both address/data lanes and both issue orders | Both completions, exact byte mask/readback, SQ drain |
| Vector loads | EEW/SEW 8/16/32/64; fractional/integer LMUL and legal derived EMUL; both lanes; unit, strided, indexed unordered/ordered; independently constrained `vm`, `vma`, `vta`, `vl`, and `vstart`; split windows | Exact 128-bit result, active mask, metadata, replay, LQ drain; the constrained-random generator closes direction x addressing x legal EEW/SEW/LMUL crosses, requires observable mask/tail agnostic elements, and expands complete 1..8-uop instructions |
| Vector stores | All legal EEW/SEW/LMUL/EMUL shapes and address modes; independently constrained mask, `vstart`, and partial `vl`; misaligned and cross-page split/replay, including back-to-back translated cross-page stores | Exact active-byte readback, completion, commit, SQ drain; the constrained-random generator's direction/shape crosses and execution-policy counts conserve against instructions, and queue-capacity windows preserve full uop identity; `misaligned-stores` also requires two independent cross-page stores to make progress and read back exactly |
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
| DCache | Cold miss, warm hit, pressure beyond the eight-line capacity of two four-way sets, dirty eviction | Refill correctness, no extra miss for mandatory warm control, dirty ReleaseData preservation |
| Forwarding | Scalar-to-scalar, vector-to-vector, scalar-to-vector, vector-to-scalar; masks and widths | Byte-accurate overlay before store commit |
| Mixed pressure | Constrained-random windows enqueue scalar load, scalar store, vector load, vector store, and prefetch together; issue order, store address/data order, vector modes, alignment, and manager delays vary before a bounded drain | Every window records at least two unresolved classes (normally all five); all scoreboards drain; per-class coverage is reported and exact LQ/SQ accounting is enforced |
| Redirect | Younger cold miss redirected while traffic is outstanding | No stale writeback and legal pointer reuse |
| Queue pressure/wrap | Two 60-entry LQ waves; more than 72 LQ and 160 ROB positions over long runs | Every accepted item retires or is explicitly canceled; flag/value identity remains continuous |
| Backpressure | Independent deterministic gaps on DCache A/D, PTW A/D, and uncache request/response | Ready-valid stability plus eventual progress |

Every `constrained-random` seed is constrained-random traffic from its first action;
there is no mandatory prefix and no coverage-completeness PASS requirement. The
requested transaction count is the total action budget, organized as rolling
five-class windows rather than isolated tests:

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
store-error contracts. The constrained-random generator also brings PTW errors into that interface;
the focused `ptw-errors` scenario remains the deterministic contract, not a
separately maintained random generator. The constrained-random generator adds same-line load merging
as another weighted operation with adjustable 2/3-way depth and exact-address,
same-beat, or cross-beat locality under the shared translation and manager-
latency constraints. The constrained-random generator adds weighted clean/dirty replacement with
adjustable 9/10-line depth, B/H/W/D width, base-index quarter, independent
held-refill overlap, address-qualified Release backpressure, and one through
eight windows over disjoint pairs of the generated four-way sets, crossed
with Bare/stage-1/nested translation and the same manager latency. Each
multi-window overlap action keeps one address-qualified D response per window pending
and requires every target window to satisfy its own replacement minimum before
any response is released. Clean scheduling establishes all holds before any
target access. The constrained-random generator admits dirty-overlap bins only after an external
baseline-ownership proof: exact committed stores, SBuffer/D/E drain, an
externally visible full-line store NtoT acquisition for every fresh baseline
line, unchanged errors, and zero completed target Releases after every hold is
established. The action quiesces the L1 stride prefetcher through its external CSR
enable, runs a 4-cycle configure step, 8 explicit cycles, and a 4-cycle
restore (16 DUT cycles total), and drains DCache idle before the baseline; the
prior enable is restored after the action. A background prefetch A request
therefore cannot land in a target set during the ownership window. The quiesce
consumes no RNG or architectural action. Rejected
construction completes normally under all functional oracles and has separate
traffic accounting; at most four fresh attempts retain the selected profile
tuple. Exhaustion is a stimulus-construction failure, not a CPU-bug finding.
The constrained-random generator extends the ordinary cold-miss burst dimension with scalar-only and
scalar+vector-load compositions at depth 2..16 and initial same-cycle scalar
issue width 1..3. It closes every enabled depth x legal width x composition x
Bare/stage-1/nested bin, skips lines already seen at the external manager, then
arms and holds each address-qualified refill until externally observed request
depth reaches the target. Exact scalar/vector writeback and LQ-flow
conservation remain mandatory; target-request and global refill/GrantAck counts
allow legal duplicate/background traffic. SPEC
performance counters motivate the profile weights but are not used as a
correctness oracle. The constrained-random generator adds a capacity-plus-one depth-17 class. It first
holds 16 address-qualified external responses, releases one fair response, and
requires the seventeenth request plus every exact terminal result and LQ
disposition to complete. It does not use internal reject/replay state as an
oracle. The constrained-random generator extends the low-rate unit-stride segment fault-only-first
subclass across all 338 decoder-legal EEW/SEW/LMUL/derived-EMUL/NF shapes. It
closes all 1,352 later/first fault x stage-1 mode x shape bins, checks the
independent mapped/unmapped page layout, and accounts for the exact one-through-
eight data uops plus one fix-VL writeback per FOF action without depending on
field selection, replay count, or timing.
The constrained-random generator adds an
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
The constrained-random generator composes every CLEAN/FLUSH/INVAL and clean/dirty target state with
one through eight accepted manager Probes while CBOAck remains pending. It
closes 48 operation x line-state x depth bins and independently conserves
B-source, C-response, target-data, and error-path traffic.
The constrained-random generator composes every successful AMO/LRSC/AMOCAS family and W/D width with
depth zero ordinary traffic or one through eight auxiliary clean Probes around
a fresh cold target refill. For nonzero depth the atomic D response is held
before writeback, B may legally backpressure during that interval, and C is
held after D release until the selected burst is accepted. The 54 family x
width x depth bins, exact atomic result, no-data Probe responses, outstanding
depth, and manager/source lifecycle all close.
The constrained-random generator crosses every enabled HLV/HLVX/HSV family with SPVP=U/S and uses
independently mapped user regions for all four 4-KiB/Svnapot VS/G leaf
combinations, so translated data and store side effects remain checked against
physical reference memory.
The constrained-random generator reuses the common `misaligned` constraint and requires every enabled
HLV/HLVX/HSV x SPVP=S/U x aligned/misaligned combination per seed. The
generator excludes byte operations when misalignment is forced and checks the
selected address class before applying the existing two-stage data oracle.
The constrained-random generator adds five VS/G PBMT pair weights covering final PMA/NC/IO and both
VS-over-G priority directions. Every enabled HLV/HLVX/HSV x SPVP=S/U x PBMT
pair executes per seed with an independent final-PA/PBMT oracle, exact physical
load/store bytes, and exact DCache-versus-Uncache routing. Non-PMA PBMT random
actions remain naturally aligned pending an explicit exception-priority model.
The constrained-random generator adds a DDR/fixed-device class across the same family/SPVP pairs.
The constrained-random generator adds no-PMP, exact first/last allowed, immediately below/above, and
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
instructions rather than representative single uops. The constrained-random generator rotates through
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

`constrained-random` with configurable constraints is the primary campaign path.
New workload directions are added as common constraint dimensions rather than
new generators.

Before a duration campaign starts, the executable, Verilated model, xspcomm,
and prepared RTL metadata are copied into one read-only runtime directory. The
runner and verifier resolve artifacts from that directory rather than the
mutable preparation path. The recorded action count and frozen provenance are
acceptance invariants: an all-pass simulator result is evidence, but is not an
accepted campaign when the frozen provenance cannot be verified.

## Complete Verification-Point Inventory

The following inventory is the closure target for the MemBlock boundary. Each
row is classified as `implemented`, `partial`, or `planned`; a planned row is a
known gap and must not be reported as green merely because the surrounding
cacheable tests pass.

### Instruction and data-shape points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| Integer loads | `lb/lbu/lh/lhu/lw/lwu/ld`, all destination classes, all issue lanes, zero/sign-extension patterns, signed `src + imm` address generation | Implemented; directed loads cross immediate 0, 1, 2047, -1, and -2048, including cache-line boundaries, while random scenarios retain zero as the dominant realistic case. `single-load` also holds an older aligned or split cross-line load's refill while a younger store to the same line/byte is issued; the load must retain its pre-store value and the store must not become the pending ROB head first |
| Integer stores | `sb/sh/sw/sd`, address-first/data-first, byte masks, all issue lanes, commit timing, signed `src + imm` address generation | Partial; issue/commit and modeled readback are covered, and store forwarding crosses immediate 0, 1, 2047, -1, and -2048. The scoreboard rejects a writeback on a lane other than its expected issue lane, with an explicit legal remap for misaligned lane-0 address completion, but the boundary exposes no complete store payload monitor |
| Floating loads/stores | FLH/FLW/FLD and narrow/widening formats, NaN-boxing, memory exceptions, integer/FP destination separation | Implemented for the MemBlock-observable load boundary: `fp-loads` checks cacheable and PBMT=IO FLH/FLW/FLD destination class and exact payload/NaN-boxing, cacheable cross-line/page misalignment, and page, PMP, permission, guest-page, PBMT=NC misalignment, denied, and corrupt exception write suppression. Store-data formatting is upstream because `issueStd.src(0)` already carries formatted raw bits |
| Vector unit stride | EEW 8/16/32/64, SEW 8/16/32/64, LMUL fractional 1/8 through 8, legal derived EMUL, `vl=0..VLMAX`, `vstart` at start/middle/end, `vm`, mask holes, `vma/vta` | Implemented for modeled operations; `vector-addressing` exhausts all 78 legal ELEN=64 EEW/SEW/LMUL/EMUL configurations with 404 load/readback and 202 store uops, independent `vsew/veew/vlmul` checks, exact data/readback, conservative two-flow unit-stride LSQ allocation, queue wrap, and ROB wrap. The constrained-random generator exposes the same shape and execution-policy dimensions as weights and checks complete multi-uop instructions under mixed translation/cache traffic |
| Vector strided | All 78 legal ELEN=64 EEW/SEW/LMUL/EMUL configurations; positive, zero, and negative legal load strides; non-overlapping positive and negative store strides; element gaps and split windows | Implemented for modeled operations: `vector-addressing` alternates positive/negative strides across the full legal matrix, checks 404 load/readback and 202 store uops with exact data and metadata, streams the largest instructions across conservative queue windows, and drains 2,304 LQ/1,152 SQ entries. The constrained-random generator uses the same flow calculation with randomized signs and optional zero-stride loads. Overlapping stores remain excluded because their final memory value is not a single deterministic oracle |
| Vector indexed unordered | Repeated indices, aliasing, non-monotonic indices, all EEWs, masked elements | Implemented for modeled operations: all 78 legal ELEN=64 EEW/SEW/LMUL/EMUL configurations execute exact load/store/readback with reverse uop issue, including index-register reuse when LMUL exceeds EMUL and 28 `EMUL>LMUL` shared-Vd configurations. The constrained-random generator generates complete unique-offset index groups, randomly reverses uop issue, and conserves 1..8-uop shapes. Directed cases retain repeated-load aliases, cache-line/Bare-page crossings, and mask/`vstart` filtering |
| Vector indexed ordered | Strict element order, repeated/aliasing indices, split beats/pages | Partial only at the system-side ordering boundary: all 78 legal ELEN=64 EEW/SEW/LMUL/EMUL configurations execute exact in-order load/store/readback, including index-register reuse and 28 `EMUL>LMUL` shared-Vd configurations. The constrained-random generator adds weighted complete shapes under ordinary mixed traffic. Directed cases retain repeated/non-monotonic indices and split boundaries. Architecturally observable ordering against a side-effecting translated target remains |
| Vector segmented/whole-register | NF/segment count, multi-uop streams, load/store direction, EEW, mask/`vstart`, fault-only-first, and partial completion | Partial: `vector-segment` checks lane-0 takeover, interleaved load/store data, multi-uop identity, the driver-side no-`enqLsq` input contract, and all addressing modes. Every addressing mode exhausts 338 legal NF 2..8 x EEW/SEW/LMUL/EMUL configurations per mode with exact load/store/readback; strided cases cover positive/negative strides, while indexed cases cover the complete index group and 252 index-only uops required by 32 `EMUL>LMUL*NF` configurations across both modes. `vector-fof` and `vector-segment-fof` cover first/later faults, including exact first-fault VA in an isolated exception epoch; the constrained-random generator brings ordinary VFOF into `constrained-random` and closes first/later fault x Sv39/Sv48 x EEW with identified exception writeback and exact fix-VL conservation. The constrained-random generator extends common unit-stride segment FOF across all 338 decoder-legal EEW/SEW/LMUL/derived-EMUL/NF shapes and closes all 1,352 first/later fault x Sv39/Sv48 x shape bins with independent mapped-prefix data, identified exception/suppression, final-VL, fix-VL, identity, and terminal-writeback accounting. The decoder has no indexed or strided Segment FOF encoding. Field-level exception choice and faulting/remainder destination elements remain unspecified where required by the vector specification. `trigger-contracts` covers field-1 indexed and strided-store breakpoints; `vector-addressing` exhausts all ordinary addressing matrices plus all 16 whole-register NF/EEW combinations. The constrained-random generator exposes addressing, EEW, SEW, LMUL, EMUL, NF, and direction weights through one interface and independently reports every enabled class. Full-core `waitForward`/`blockBackward` serialization makes an active segment plus redirect unreachable, so that combination is an upstream assumption rather than a MemBlock stimulus. Overlapping indexed stores and ordered access to a side-effecting translated target remain |
| Vector segment high-half canonical | Full-XLEN high-half Sv39/Sv48 and all four nested VS/G ordinary segment load/store aliases, aligned, cross-16-byte and cross-page accesses, all enabled addressing modes and legal EEW/SEW/LMUL/EMUL/NF shapes | Partial; `vector-segment-canonical` covers the 12-case shared subset. The constrained-random generator adds eight stage-1 mode x direction x alignment bins; the constrained-random generator preserves those bins and adds all four nested VS/G pairs x direction x alignment, using an independent per-byte stage-1 or two-stage walk, exact completion/data/uop conservation, and store readback. High-half FOF composition is covered by `vector-fof-high`; segment noncanonical faults now have a deterministic failing reproducer (`vector-segment-noncanonical`) for an open RTL bug: the noncanonical segment load completes without the required page fault even though `VSegmentUnit` sends `fullva`/`checkfullva` and imports the DTLB `excp(0).pf`; the exact root (DTLB `checkfullva`/`pregpf` or `s_wait_tlb_resp`/`s_pm` propagation) is under focused review. Arbitrary partial-fault progress remains |
| Vector data patterns | all zero/one, ramps, alternating bits, random bytes, same-byte aliases, old-destination merge | Implemented/partial by operation class |
| Software prefetch | `prefetch.i/r/w`, mapped/unmapped, cacheable/NC/IO, all lanes, signed `src + imm`, duplicate and outstanding requests | Partial: `ifetch-prefetch` checks every class/lane, all signed-12-bit boundary classes through 0/1/2047/-1/-2048, a same-cycle `i/r/w` batch, unmapped cold data-hint drop without PTW/data traffic, warmed Sv39 cacheable data-prefetch hits, individual `r/w` DCache requests, and the intentional PBMT policy: resident-TLB NC `r/w` issue DCache hints without Uncache while IO `r/w` reach neither manager; broader duplicate/same-line contention remains |
| Atomics | LR/SC, AMOADD/XOR/AND/OR/SWAP/MIN/MAX and signed/unsigned variants, AMOCAS, reservation loss, alignment | Partial; all exposed W/D-width AMO variants, AMOCAS.W/D compare success/failure, LR/SC success/failure, all 24 W/D opcodes crossed with every illegal byte offset (120 cases), and a fixed-device-PMA `AMOADD.D` denial execute in `atomic-contracts`. The alignment matrix checks LR load-misaligned versus all other store-misaligned exception classes, suppressed RF write, zero new DCache traffic, and ROB pointer wrap; the PMA fault suppresses RF write and all manager/memory side effects. `atomic-dchannel-errors` crosses denied/corrupt with all 22 refill-capable W/D LR/AMO/AMOCAS operations, checks initial exception/RF contracts, later poisoned-line load hits, SC.W/D hits on denied/corrupt metadata, exact request counts, and clean AMO recovery. The constrained-random generator adds common `atomic-error`/`atomic-error-denied` controls and requires all 18 enabled family x width x clean/corrupt/denied outcomes, exact exception and manager accounting, unchanged backing memory at response time, and a deterministic private poisoned-line image for later ReleaseData. The constrained-random generator adds all 54 successful family x width x zero-through-eight-depth bins. Depth zero keeps the ordinary path; nonzero depth checks exact old-value/result and one cold target request, forbids early writeback while D is held, and validates address-matched no-data Probe completions after D release. The SC checks do not claim internal reservation observability. SC cannot have a cold-miss D response because MainPipe returns failure before issuing TileLink traffic when the line or usable reservation is absent. Reservation loss through a manager Probe is covered by `atomic-contracts`: after an LR.D, a toN dirty ProbeAckData invalidates the reservation, and the following SC.D must fail without a new data request and leave memory unchanged. Cross-hart reservation interference and broader concurrent atomic ordering remain; the directed replacement-plus-atomic case is covered by `dcache-release` |
| CBO/CMO/fences | clean/invalidate/flush/zero, `fence`, `fence.i`, `sfence.vma`, ordering with outstanding traffic | Partial; cacheable `CBO.ZERO` StoreQueue/SBuffer line-zero and readback are executable (`cbo-zero-contracts`). `cmo-contracts` covers CLEAN/FLUSH/INVAL opcode/source/line alignment, all six operation x clean/dirty line states with exact TtoB/BtoB/TtoN/BtoN reports, automatic SBuffer drain of a previously committed dirty store without a direct flush, SBuffer-before-request ordering proven by the resulting exact dirty Probe data, clean no-data transitions, retained-hit/cold-refill outcomes, delayed completion, `flushPipe`, and every operation crossed with denied/corrupt CBOAck. It also accepts a younger cold load into a distinct MSHR while CBOAck is pending, then requires the legal CMO `flushAfter` to cancel exactly that LQ entry and suppress its delayed response. The constrained-random generator exposes CLEAN/FLUSH/INVAL, clean/dirty, younger-overlap, error presence, and corrupt/denied kind as common constraints. A dedicated `cmo-stress` preset weights CMO at 1000 with a scalar 1/1 control, fixes dirty/younger-overlap at 1000, disables CMO errors and ordinary Probe/error subclasses, and retains the CMO Probe-depth weights; its clean path commits the CMO before the ROB commit-time flushAfter redirect. It runs in the active Bare/stage-1/nested context, randomizes CBOAck and younger-refill latency, requires every enabled operation x error-kind cross, and conserves failed CMOs separately from success-path Probes. Error responses require the exact StoreAccessFault/HardwareError, no C response, unchanged bus memory, and redirect cleanup. The constrained-random generator adds one-through-eight-source Probe bursts during every successful operation/state class, closes 48 bins, and requires exact accepted depth, response-address matching, target dirty data, and manager conservation while errors retain zero Probe. The internal `cmoOpResp` is inferred through external DCache A/B/C/D and final StoreQueue writeback. Translation fences are covered separately; full ISA `fence.i` is outside this top-level transaction boundary. A single CMO overlapping a held dirty replacement release is now covered by `dcache-release`; the CMO store itself does not redirect; the ROB's `flushOut` does at the CMO commit, so the CMO tests apply `redirect_after` after `commit_store`. AMOs are non-speculative (`DecodeUnit.scala:238-245` `noSpec`/`blockBack`), so a redirect cannot cancel an executing AMO; the earlier `cmo-atomic-flush`/`redirect-atomic-flush` scenarios were invalid testbench stimuli and were removed. The `cmo-store-flush` and `redirect-store-flush` controls confirm the younger-store SBuffer/SQ path drops its data for both redirect sources. Multiple simultaneous CMO sources and broader atomic ordering remain |
| Hypervisor memory ops | HLV/HLVX/HSV, effective privilege/SPVP, VSUM/VMXR, execute permission, guest/host faults | Partial: `hypervisor-contracts` executes all exposed HLV/HLVX/HSV encodings, SPVP user/supervisor cases, VSUM/VMXR permission changes, VS- and G-stage faults, HLVX execute-only access, representative operations under all four Sv39/Sv48 and Sv39x4/Sv48x4 pairs, a five-combination PBMT basis, and cacheable misaligned split paths. Eighteen M-mode cases cross all three families and SPVP=U/S with R/X/RW/RX plus locked R/RWX physical PMP regions, requiring R, R+X, and W; nine fixed-PMA cases cover an interior device target plus every family immediately below and at the exact `0x80000000` device-to-DDR boundary. The constrained-random generator-25 close family x SPVP, alignment, five-pair PBMT, and DDR/fixed-device dimensions with independent PA/data/store/manager oracles. The constrained-random generator closes every family x SPVP x no-PMP/first/last/below/above/cross-lower/cross-upper bin for a nested 4-KiB NAPOT allow region, including precise faults, manager non-use or allowed-prefix behavior, and recovery. Same-lane scalar H/vector split-flow overlap remains a stimulus gap; no arbitration winner is required by the oracle. Other fixed-PMA boundaries and broader PMP size/TOR/lock/permission/overlap-by-edge crosses remain |

### Address, translation, and protection points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| Alignment | every byte offset for widths 1/2/4/8/16, beat boundary, 64-byte line, 4 KiB page, two-page and two-line splits | Partial; common scalar/vector split classes implemented |
| Virtual address classes | Sv39 canonical low/high halves, Sv48 canonical low/high halves, noncanonical bits, page-offset boundaries, aliasing VAs, VA wraparound | Partial; low/high canonical halves and both noncanonical sign-extension mismatch directions execute for Sv39 and Sv48. `translation-faults` crosses both canonical boundaries at every 2/4/8-byte distance for scalar loads/stores and strided/indexed-unordered/indexed-ordered vector stores (220 faults), with 440 permissive-alias controls, 440 unchanged-edge readbacks, and 220 same-instance store/load recovery pairs. The constrained-random generator composes the scalar mode x load/store x direction x width cross into `constrained-random`; the constrained-random generator adds 36 eligible singleton strided/indexed store mode x addressing x direction x width bins with full reserved-flow recovery and committed survivors. A separate directed G-only matrix adds 110 upper-limit guest-page faults across 41/50-bit GPA, the same five scalar/vector access kinds, and every 2/4/8-byte crossing distance, with 220 permissive controls, 220 unchanged-edge readbacks, and 110 committed store/load survivors. The constrained-random generator composes 12 scalar and 18 eligible singleton vector-store G-only upper-limit bins into the same mixed scheduler, independently of ordinary translation-regime/VS weights. Broader vector shapes/multi-element faults, vector-load and unit-stride crossings, nested boundary composition, pointer masking/trigger combinations, broader aliasing, and wraparound crosses remain |
| Physical address classes | cacheable, uncached, device, reserved, high PA bits, line/set/way aliases | Partial; cacheable and PBMT-NC plus a non-DebugModule SoC `c=0` PMA device interval, DebugModule LD/SD denial and allowed adjacent intervals across initial nondebug, Debug Mode, and exit at exact aligned region edges, exact device-to-DDR manager selection on both sides of `0x80000000`, and read-clear/partial-write device semantics are executable in `mmio-contracts`. `hypervisor-contracts` now checks the same exact boundary after two-stage translation for HLV, HLVX, and HSV; reserved/high-PA/alias matrix remains |
| Stage-1 Sv39 walk | Bare/Sv39 mode, L2/L1/L0 leaves (1-GiB/2-MiB/4-KiB plus 64-KiB Svnapot), invalid/non-leaf, misaligned superpage, canonicality, permission/PBMT/A-D combinations | Partial; all leaf levels, all 16 legal Svnapot subpages with load/store/readback, high-half canonical VA, both noncanonical sign-extension directions, malformed superpage, physical-PPN overflow at L2/L1/L0, U/S, SUM, MXR, missing-A/D, and 13 invalid/reserved/PBMT/NAPOT encoding cases execute for loads and stores; broader crosses remain |
| Stage-1 Sv48 walk | Bare/Sv48 mode, L3/L2/L1/L0 leaves (512-GiB/1-GiB/2-MiB/4-KiB plus 64-KiB Svnapot), L3 faults, canonicality, permission/PBMT/A-D combinations | Partial; all leaf levels, all 16 legal Svnapot subpages with load/store/readback, high-half canonical VA, both noncanonical sign-extension directions, physical-PPN overflow at L3/L2/L1/L0, U/S, SUM, MXR, missing-A/D, and the same 13 encoding cases execute for loads and stores; broader crosses remain |
| G-stage Sv39x4 walk | 16-KiB root, widened root index, 41-bit GPA, all leaf levels including 64-KiB Svnapot, high-GPA overflow, G-stage permissions | Partial; all leaf levels, all 16 legal G-only Svnapot subpages with load/store/readback, final and every-level VS-generated GPA overflow, physical-PPN overflow at L2/L1/L0, load/store permission faults including `D=0`, all valid two-stage PBMT combinations, and 13 invalid/reserved/PBMT/NAPOT encoding cases execute for loads and stores |
| G-stage Sv48x4 walk | 16-KiB root, widened root index, 50-bit GPA, all leaf levels including 64-KiB Svnapot, high-GPA overflow, G-stage permissions | Partial; all leaf levels, all 16 legal G-only Svnapot subpages with load/store/readback, final and every-level VS-generated GPA overflow, physical-PPN overflow at L3/L2/L1/L0, load/store permission faults including `D=0`, all valid two-stage PBMT combinations, and the same 13 encoding cases execute for loads and stores |
| Nested mode matrix | `Sv39->Sv39x4`, `Sv39->Sv48x4`, `Sv48->Sv39x4`, `Sv48->Sv48x4`, plus `vsatp`/`hgatp` Bare degenerations and VS/G Svnapot placement | Partial; all four 4-KiB pairs are executable in `translation-matrix`, including cold/warm TLB reuse; `translation-superpages` crosses all four pairs with VS-only, G-only, and simultaneous VS/G NAPOT across every 4-KiB subpage; VS-only/G-only/fully-Bare degenerations execute in `translation-bare`; the same four non-Bare pairs are independently weighted and required in `constrained-random` |
| Stage-only translation | HS/S/U stage-1 only, VS/VU stage-1 only, G-stage only for implicit page-table/HLV-class accesses | Partial; only current data-access paths are modeled |
| TLB behavior | cold miss, hit, refill, duplicate miss, replay, invalidation, `sfence.vma`, concurrent page walks | Partial; `translation-fence-all` holds stale stage-1, VS-only, G-only, and fully nested VS/G leaf responses across global and selective fences for both supported modes and all four nested pairs, rejects canceled writeback, requires new-data refills, and co-issues two distinct-page stage-1 plus nested walks before the first delayed PTW response; `constrained-random` requires observed cold-walk and warm-reuse windows plus every enabled legal fence cross; broader duplicate-miss source-reuse stress remains |
| Page permissions | R/W/X/U/G, SUM/MXR at HS/VS stage, G-stage U-mode rule, read-only store, execute-only, access/dirty bit updates, privilege transitions | Partial; `translation-permissions` executes 194 cases: 16 stage-1 loads, 17 stage-1 stores, 75 two-stage loads, and 86 two-stage stores. Sv39/Sv48 U/S, SUM, MXR, A/D, VSUM/VMXR, and final G-stage load/store R/A/D/U variants are checked with every nested permission variant crossed over all four VS/G mode pairs. A 42-configuration implicit-walk cross applies G-stage X-only+MXR, U=0, and A=0 to every VS PTE level for both original load and store, producing 84 access-specific GPF transactions with exact VA/GPA/marker, early walk termination, manager non-use, and SQ conservation; four positive stores prove R-only W=0/D=0 VS-page-table mappings are sufficient for implicit PTE reads. Passing cases require exact readback; `translation-faults` covers PBMT/reserved encoding faults, while `translation-pbmt` covers valid PBMT composition |
| Mode/context switching | `satp/vsatp/hgatp` root and MODE changes, ASID/VMID reuse, `V` transitions, same VA under distinct contexts | Implemented for the modeled modes; `translation-context` covers five drained context families and 14 distinct-data same-address accesses; `translation-fence` adds same-ID host-ASID/VS-ASID/VMID fenced root reuse; `translation-inflight-context-all` holds an old PTW response for 256 cycles, changes root/ID/MODE or `V`, redirects the old load, reuses its ROB/LQ identity, and requires the new PA for both host mode directions plus all four nested starting pairs and both host/nested directions |
| Translation fences | `SFENCE.VMA`, `HFENCE.VVMA`, `HFENCE.GVMA`, selective/global scope and updates with outstanding traffic | Partial; global/selective leaf updates, targeted same-ID host-ASID/VS-ASID/VMID root reuse, stale-response races, and co-issued distinct-page stage-1/nested walks spanning both stage-1 modes, both G-stage modes, and all four fully nested pairs are implemented by `translation-fence-all`; the separate context-race matrix checks redirected root/ID changes while a response remains outstanding |
| PMP/PMA | TOR/NA4/NAPOT, overlap priority, lock, M/R/W/X, cacheability, atomic/MMIO permissions, exact region edges | Partial; `pmp-contracts` programs the distributed PMP CSR boundary and checks TOR/NAPOT exact edges, R/W plus AMO denial, overlap priority, M-mode bypass, lock enforcement, and lock immutability. It also checks that a TOR base address is locked when the following entry is a locked TOR entry: rewriting that base beyond the upper bound is rejected and the original range remains readable. A split misaligned S-mode load starts four bytes inside an allowed 4-KiB NAPOT region and crosses into the denied next page; it must report `LoadAccessFault`, may issue only its allowed-prefix line request, and must not request or return data from the denied line. The matching store case must report `StoreAccessFault` and must not partially update the allowed line or write the denied line. A locked `A=OFF` entry must remain locked when a later write attempts to turn it into a NAPOT allow; the S-mode load at the original address must stay denied. A separate S-mode case locks a NAPOT allow entry, attempts to move it to a different address, and requires the old address to remain allowed while the new address stays denied; M-mode's default allow for unmatched addresses cannot mask this rewrite. A NAPOT size matrix covers 4 KiB, 8 KiB, 16 KiB, 64 KiB, 256 KiB, 1 MiB, and 4 MiB regions with allowed first/last doublewords and denied immediately-below/above doublewords. A reserved `W=1,R=0` NAPOT entry must behave as a data-deny entry for a store, and an `X`-only NAPOT entry must deny data loads as well as stores; both checks require no manager request. Two Sv39 cases cross a PMP-denied final physical page with PBMT=NC and PBMT=IO and require the translated load to report `LoadAccessFault` before any DCache/Uncache data request; a TLB-hit store-issue variant requires `StoreAccessFault` with the same manager non-use. The PTW pages remain PMP-allowed. `PlatformGrain=12` makes NA4 unselectable and coerces `A=2` to a minimum 4-KiB NAPOT region. HLV/HLVX/HSV check SPVP-selected physical R/R+X/W permissions, fixed-PMA device behavior and its exact `0x80000000` edge; the constrained-random generator makes the interior class weighted. The constrained-random generator crosses seven exact 4-KiB NAPOT edge relations with every family and SPVP value. DebugModule LD/SD crosses the first/last aligned doublewords and both adjacent doublewords with initial nondebug, Debug Mode, and exit; exact fault/manager/state and observed queue disposition are checked. Misaligned DebugModule crossings and restrictive-PMP override are gaps. Device-PMA AMO denial is also focused. Other fixed-PMA matrices and additional PMP size/TOR/lock/permission/overlap-by-edge crosses remain |
| Fault classes | load/store/instruction access fault, stage-1 page fault, G-stage guest-page fault, noncanonical VA, high-GPA overflow, address-misaligned, access-denied, bus/ECC error | Partial; load/store/page/misaligned, noncanonical, high-GPA, PMP access-denied, response-wide and per-beat denied/corrupt D-channel, one-shot physical DCache tag single/double-bit plus all-bank data single/double-bit ECC, and injected/clean load isolation are executable; instruction-side PMP and broader ECC cause combinations remain |
| Fault metadata | exact VA, GPA/PTE address, first failing level/stage, shifted `htval`-class value, guest marker, cause priority, single reporting and replay suppression | Partial; VS-non-leaf metadata, concurrent scalar load/store oldest-address selection across ROB wrap, same-ROB vector `uopIdx` replacement, one simultaneous page-fault/misaligned pair, bidirectional scalar/vector replacement, and vector-load/store buffer selection under both arrival orders are implemented; the broader cross-cause/source matrix remains |

### Cache, memory-system, and coherence points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| Frontend pass-through | ICache line reads, instruction-Uncache reads, ICache-control Get/PutFull/PutPartial, source/size/mask/data, simultaneous paths, A/D backpressure | Implemented at the MemBlock top boundary by `frontend-bridge`; downstream ICache/device semantics remain integration responsibilities |
| DCache lookup | warm hit, cold miss, same-line merge, bank conflict, distinct-line MLP, set pressure beyond associativity, synonym/alias | Partial; cold/warm and bank conflict are executable. The constrained-random generator promotes same-line merging into the common constraint interface: fresh cold lines cross 2/3 same-cycle load lanes, exact-address/same-beat/cross-beat patterns, both critical beats, Bare/stage-1/nested translation, and the selected manager latency. The constrained-random generator drives 9/10 distinct clean-load or dirty-store lines through one to eight independent windows over disjoint pairs of the generated four-way sets, with every base-index quarter, access width, translation regime, no-overlap/held-refill class, and target Release backpressure class. Target windows containing any line covered by an earlier DCache request are skipped before backing memory is initialized. Clean lines are revisited in reverse with exact data and attributed replacement misses. Clean overlap establishes all held responses before issuing any target; an older allocated target prefix retires before the remaining targets are allocated, avoiding both a residency assumption and LQ saturation. Dirty lines retain exact request, writeback, dequeue, and ReleaseData attribution; older overflow stores allocate SQ before younger held-load dispatch, with address/data issue and commit deferred until holds are established. The constrained-random generator requires external baseline-ownership qualification before dirty overlap; rejected attempts complete normally and retain separate accounting without populating accepted bins. Directed single-set associativity is covered by `dcache-release`: six lines in one four-way set force two attributed dirty replacements, while random pressure windows still span two sets. The constrained-random generator drive ordinary never-repeated cold-line bursts, cross depth 2..17 with legal initial scalar issue width 1..3, scalar-only/scalar+vector-load composition, and translation, and conserve target lines, scalar/vector terminal effects, LQ flows, refills, and GrantAcks without inspecting internal MSHRs. Depth 17 first proves external saturation at 16 pending responses, then requires capacity-plus-one forward progress after one response is released. Broader physical synonym/alias crosses remain |
| Refill/replay | delayed A/D responses, critical-beat-first ordering, partial refill, killed request, replay after miss | Partial; `single-load` forces both `isKeyword` polarities and corresponding two-beat GrantData orders, checks exact data, merges two same-line loads into one delayed refill, proves a keyword load writes back from its critical beat while the noncritical beat is held for 128 cycles without a later duplicate, and releases two held distinct-source load refills in reverse request order so each scoreboard identity must receive its own line data while the older load remains pending. `dcache-errors` crosses both orders with fixed denied and first/last-only corrupt, holds the second beat for 256 cycles, merges an opposite-half load, and accepts an unrelated healthy cold MSHR before that beat arrives. The constrained-random generator adds response-wide corrupt/denied scalar-load cold refills under the common translation/latency/operation constraints, exact exception and redirect recovery, and sink-attributed GrantAck accounting even when a clean hardware prefetch is concurrent. The constrained-random generator applies the same address-qualified two-beat and GrantAck/refill conservation to atomic misses across family and width. The constrained-random generator varies clean merge depth, address placement, critical beat, translation, and calibrated response delay while allowing matched background prefetch refill/GrantAck pairs. Delayed and killed refills are exercised elsewhere; malformed response crosses and reordered beats within one refill remain |
| Eviction | clean release, dirty ReleaseData, partial byte masks, replacement under pressure, release backpressure | Partial; immutable whole-line snapshots are checked by the dedicated dirty-pressure phase and constrained-random replacement pressure. The constrained-random generator adds clean pressure, held-refill overlap, target C backpressure, and reports 1536 clean/dirty x no-overlap/held-refill-overlap x no-C-stall/C-stall x one-through-eight-window x 9/10-line x B/H/W/D x translation bins plus four base-index quarters. It qualifies retained dirty baseline coherence responsibility from accepted full-line store NtoT acquisitions, completed D/E drain, unchanged errors, and zero completed target Releases at the established hold boundary; array residency is not required. Clean actions require exact initial/revisit data, address-attributed target Release counts, zero target ReleaseData, and load/LQ conservation. Dirty actions retain exact target request/store-WB/SQ-dequeue counts and separate byte checks for target and background ReleaseData. Each multi-window overlap action holds one independent refill per window over two physical sets and requires every target window to reach its release minimum before any held response is released. Clean targets issue only after all holds are established; dirty overflow issues only after all holds and ownership qualification. Rejected attempts retain all data/identity/protocol checks, finish normal WB/commit/queue/manager cleanup, and consume separate attempt/traffic totals; four unsuccessful fresh attempts report stimulus construction exhaustion. Required during-hold Releases use final-C-beat acceptance. Each selected backpressure action stalls exactly one target Release for 16 valid cycles, compares the complete C payload on all 16 held transitions, and prevents unrelated C traffic from satisfying the target oracle. Replacement composition with a same-set Probe, a clean CMO, a denied CMO response, and an atomic refill is implemented in `dcache-release`: one held dirty victim release overlaps a Probe to the just-refilled fifth line; the clean CMO form overlaps a delayed CBOAck dirty cleanup; the error form overlaps a delayed denied CBOAck while two Probes keep C stalled; the atomic form holds a delayed cold `AMOADD.D` old-value response while the victim release/probe C drain. All forms require byte-exact C payloads and readbacks; the CMO-error form reports up to two youngest committed LQ entries at the directed-drain lookahead boundary. Broader concurrent atomic ordering remains.|
| TileLink coherence | Probe/B/C/E traffic, source reuse, denied/corrupt/error responses, manager ordering | Partial; `dcache-coherence` executes three manager Probes spanning clean no-data, clean requested-data, and dirty mandatory-data responses, checks byte-exact C beats, forces E backpressure, and matches every GrantAck sink. It separately accepts two distinct Probe sources before an unrelated cold miss completes and checks outstanding depth and response addresses. `cmo-contracts` checks custom A opcode 12/13/14, fixed C source 17, CBOAck opcode 8, all clean/dirty permission reports, Probe-before-Ack ordering, exact dirty C beats, a concurrent younger MSHR plus cancellation, and denied/corrupt propagation. The constrained-random generator adds constrained dirty toB/toN Probes with requested/mandatory data, checked toB cleanup, and hierarchical one-through-eight-source bursts while a delayed refill remains outstanding; the constrained-random generator retains translated CMO operation/state/younger-MSHR/error crosses. The constrained-random generator repeats all eight accepted depths while every successful CMO operation/state class is pending and closes 48 composition bins. The constrained-random generator repeats zero through eight auxiliary Probes for each successful atomic family/width. It permits legal B backpressure while the target D response is held, then holds C until the full burst is accepted after D release. CMO and atomic errors are excluded exactly from Probe conservation. The Probe oracle matches every C response by address; C source belongs to the client namespace and need not equal the B source. The constrained-random generator walks all 64 B-source IDs across accepted traffic, rejects active-ID reuse, and independently checks completed reuse plus wrap counts. `dcache-errors` rejects A-source reuse until final D completion, reaches two simultaneous AcquireBlock lifetimes, and checks exact D-beat/GrantAck drain. Load and atomic denied/corrupt D responses are also injected and checked; malformed responses and replacement cross-operation bursts remain planned |
| Uncache/MMIO | Get/Put widths, byte enables, side effects, ordering, response delay, denied/error response | Partial; PBMT-NC Get widths/byte lanes and scalar denied/independent-corrupt load propagation plus denied-store propagation are executable (`uncache-widths`, `uncache-errors`), and the denied store must preserve initialized backing bytes. The constrained-random generator adds common Uncache error-presence and load error-kind constraints, forcing and conserving every enabled NC/MMIO x load/store x legal outcome bin. It distinguishes committed NC store errors, which report externally and dequeue without redirect, from precise MMIO store StoreAccessFault writeback. `mmio-contracts` checks PBMT=IO's direct load metadata bypass independently of final data/exception writeback: all three load-unit inputs, a lane-permuted same-cycle triple covering all three ROB-ordered compacted output slots and exact ROB/cycle values, exactly-one pulse before response under 64/128/256-cycle delays, plus cacheable/PBMT-NC negative controls. It also covers scalar store request/response/SQ-retirement, DCache non-use, denied/independent-corrupt metadata preservation, a physical non-DebugModule `c=0` PMA load/store pair, device-to-DDR edge loads, and a seven-request read-clear/partial-write device sequence with exact ordered logging. Corrupt is forbidden on data-less store `AccessAck`. Three same-address device loads queued before the first request prove external serialization under 512/128-cycle delays despite enabled Uncache outstanding mode. A separate pre-issued `load -> SW -> load` sequence crosses LQ/SQ and two 256-cycle response delays, requiring external depth one plus exact read-clear/write/readback state transitions. Cacheable CBO.ZERO line-zero/readback is executable (`cbo-zero-contracts`); malformed/duplicate/early/late responses remain |
| ECC/cache errors | correctable/uncorrectable data, error lifetime, retry or architectural exception | Partial; D-channel denied/corrupt propagation is executable for scalar loads, cached poisoned-line vector loads, atomics, and all three CMO operations in `dcache-errors`, `atomic-dchannel-errors`, and `cmo-contracts`. The CMO matrix requires exact StoreAccessFault/HardwareError and unchanged memory for all six cases; the constrained-random generator repeats those operation/error crosses under common constrained clean/dirty, translation, latency, and optional younger-miss pressure. The constrained-random generator adds common scalar-load error presence/kind, dedicated unique cold lines, exact two-beat denied/corrupt accounting, errored refill/GrantAck identity, and precise redirect/LQ recovery. The constrained-random generator adds the complete enabled atomic family x width x outcome cross and distinguishes manager backing memory from the intentionally retained poisoned cache image. `dcache-errors` crosses both critical-beat orders with fixed denied plus first/last-only independent corrupt, a 256-cycle interbeat gap, same-MSHR merge, a concurrent healthy MSHR, poisoned/healthy resident hits, exact error priority, and queue/wakeup conservation. It also uses `L1DCacheCtrl` one-shot injection on proven-resident lines. The RTL tag pseudo-error path selects bank 0, where representative single- and double-bit masks must cancel, redirect, suppress terminal writeback, report the exact physical address to BEU, and recover on a clean survivor. Data ECC crosses all eight banks with spread single-bit and adjacent double-bit masks; in default `EnableAccurateLoadError=false` mode every case must report BEU, return the exact XOR-corrupted value without cancellation, and recover on a clean survivor. A same-cycle selected-bank/clean-bank pair checks lane isolation and automatic one-shot clear without a disable write. Broader masks, delayed/persistent injection, and wider mixed-traffic concurrency crosses remain |
| PTW manager | request/response backpressure, source reuse, malformed/denied response, concurrent walks | Partial; legal backpressure, response identity, and denied/corrupt error propagation are implemented. `ptw-errors` covers 16 stage-1/G-stage/nested/bitmap load/store cases, exact walk cutoff, access-fault writeback, cache-refill suppression, and zero data-manager side effects. Its eight corrupt cases split 4/4 across the first/last data beat; every error case then fences the owning translation stage and requires a clean same-address recovery with an exact failed-block reread and one DCache access. Two co-issued distinct-page DTLB walks remain pending around a delayed response even where that scenario observes manager depth one; the separate IFU-plus-DTLB overlap reaches manager depth two. Two same-VPN IFU requests return twice while coalescing into one three-request Sv39 memory walk. Malformed responses and broader source-reuse stress remain |

### Queue, ordering, and control points

| Point family | Values and crosses to generate | Current status |
| --- | --- | --- |
| LQ/SQ occupancy | empty/near-full/full, wrap flags, simultaneous enqueue/dequeue, same-slot reuse | Implemented/partial |
| ROB age | ordinary and wrapped pointers, flag transitions, same-cycle issue/commit/redirect | Partial |
| Store-to-load forwarding | scalar-scalar, vector-vector, scalar-vector, vector-scalar, partial overlap, byte masks, older/younger stores | Implemented for modeled classes |
| Exception priority | multiple legal faults, differing ROB/uop vs LQ/SQ age, load/store/vector competition | Partial; wrapped ROB age deliberately disagrees with LQ/SQ order for simultaneous scalar load/store faults, the retained load/store selector is switched while both buffers hold scalar or vector load exceptions in both source-arrival orders, a same-ROB LMUL=2 vector pair checks `uopIdx`, page and PBMT-NC-misaligned faults are co-issued, and scalar/vector page faults replace each other in both arrival directions. Additional cause pairs and same-operation multi-cause priority remain planned |
| Redirect/recovery | kill each producer class, in-flight miss/replay, canceled prefetch, pointer reuse, survivor data, held/stale D response | Implemented for basic scalar/vector LSQ redirect and a held cold D response: `redirect` holds the D beat, redirects the load, releases the stale beat, requires no writeback, and checks a fresh successful load (`redirect_cancels_observed=2,2,0`). Direct active-VSegment redirect is excluded because full-core serialization prevents that state; broader simultaneous producer-class arbitration remains |
| Fence/commit ordering | outstanding cache/uncache/PTW traffic across commit and fence boundaries | Partial; `sbuffer-flush` commits a cacheable store while a PBMT-NC store response is delayed 1,024 cycles, pulses the backend-decoded direct SBuffer flush, requires both paths to drain, and checks exact readback. `translation-fence-all` holds targeted PTW responses across global/selective stage-appropriate fences and rejects stale completion. Both `cmo-contracts` and the constrained-random generator prove a younger cold DCache miss can overlap delayed CBOAck and is then canceled without writeback by CMO `flushPipe`; the random path varies operation, line state, translation context, error state, and both response delays. MemBlock exposes no complete FENCE/FENCE.I instruction transaction, so their decode/ROB semantics remain integration-owned; wider simultaneous manager crosses remain |
| Backpressure | every producer/consumer ready-low pattern, long stalls, alternating stalls, response delay cross-product | Implemented for DCache/PTW/uncache and Probe responses; DCache negative responses now cross a fixed 256-cycle interbeat gap and concurrent MSHRs, while wider randomized negative-error timing remains planned |
| Reset/quiescence | reset asserted/deasserted at legal boundaries, idle cycles, reset with outstanding traffic, repeated reset | Partial; `reset-tree-contracts` checks three repeated functional-reset pulse widths, asynchronous assertion, exact six-cycle synchronized release through two ResetGen levels, DFT functional-mode isolation and three-cycle release, plus the combinational scan override at `io_reset_backend`. `reset-recovery` independently resets accepted DCache-refill, PTW-walk, and Uncache/MMIO traffic under a 256-cycle response delay, while `frontend-reset-recovery` clears stalled requests on all three frontend bridges and one observably backpressured ICache-control response before distinct survivor requests. `reset-recovery` also crosses LR/AMOADD/AMOCAS x W/D (six cases), resetting after one cold request is accepted and before any GrantData beat. Each case rejects canceled-identity writeback, checks a distinct same-family survivor and two full-doubleword readbacks, and monitors beyond the original delayed response deadline. Four further LR.W/D x first/second-half cases reset after exactly one accepted GrantData beat, validate any legal early result or explicitly cancel the still-pending identity, and check a distinct LR survivor, unchanged readbacks, five accepted data beats, and post-reset drain. `reset-partial-load` additionally crosses singleton and same-line opposite-half LD pairs over three primary issue lanes and both target halves (twelve cases) at the one-beat boundary and adds one clean CMO reset case with a delayed CBOAck. These cases retain any observed early completion/LQ dequeue, cancel only remaining reset obligations, and require a distinct survivor plus a readback of every old target and the survivor with exact queue conservation. Completed/canceled branches and observed request beat orders are reported separately for LR and LD. Clean CMO reset is covered; partial AMOADD/AMOCAS or denied/corrupt refills and additional LSQ producer/replay classes remain planned |

### Protocol and robustness points

| Point family | Required checks | Current status |
| --- | --- | --- |
| Ready/valid stability | Every DUT producer holds payload and sideband stable while stalled | Implemented with generated SVA |
| Exactly-once identity | No lost, duplicated, or mismatched response; source/ROB/uop/queue identity preserved | Partial; some output-only pulses and response fields are not independently observable |
| Manager legality | Opcode/size/mask/address/data combinations are legal for each channel | Partial; current modeled channels covered |
| Fairness/progress | Every accepted non-canceled request eventually terminates under bounded fair delays | Partial; fair delays are supplied, but all accepted events are not independently observed |
| Four-state robustness | Unknown/reset values, uninitialized arrays, X-sensitive assertions | Planned; requires four-state simulator/formal check |
| Malformed external responses | denied/corrupt/early/late/duplicate response handling | Planned |
| Long-run reproducibility | Frozen binary/model/libraries, deterministic seed replay, recorded artifact paths | Implemented |

## Constrained-Random Distribution

`constrained-random` is the canonical legal-value generator and weight layer; its
traffic, shape, payload, and scheduling streams are deterministically derived
from the scenario seed with SplitMix64 for replay. Per-seed gates combine
generator-side feature bins with observed writeback and manager progress. There
is still no independent starvation monitor or general simulator cross-coverage
database, so these counters are evidence of stimulus intent, not proof that every
required cross occurred.

Required cross coverage remains operation class x issue lane x alignment; vector
EEW x address mode x mask/vstart/vl; cache residency x translation state x
memory type; producer overlap x issue order x backpressure; fault cause x
privilege/translation stage x queue age; store width x overlap x forwarding
direction; and redirect point x outstanding type x pointer-wrap state. The
executable fields, presets, counters, and replay contract are defined only in
[`CONSTRAINED_RANDOM.md`](CONSTRAINED_RANDOM.md).

Profiles may reweight rare bins but must not change legal-value constraints or the
oracle. A generated value that the reference model cannot explain is a generator
failure, not an RTL failure.


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

## Top-Level IO and Interface Assumptions

[`IO_COVERAGE.md`](IO_COVERAGE.md) is the sole authority for interface-to-driver
mapping, legal-value dimensions, implementation status, and interface gaps.
[`PORTS.md`](PORTS.md) is the generated summary and
`config/expected_ports.json` is the exact flattened inventory. `make check-docs`
checks inventory and constraint-schema consistency; this plan does not duplicate
port counts or per-family driver details.

The environment makes only these behavioral assumptions:

- Issue inputs use legal operation encodings and consistent LSQ/ROB pointers;
  typed drivers hold `valid` and payload stable until acceptance.
- Backend allocation, commit, redirect, and age information follow their legal
  order; the scoreboard removes only architecturally younger work. In
  particular, the mixed heterogeneous window never makes a page-crossing
  younger store the pending ROB head or commits it while an older load is
  outstanding. Page-crossing scalar-store replay and readback remain directed
  contracts in `scalar-misaligned` and `misaligned-stores`; the mixed window
  retains line-crossing misaligned stores.
- Reference page tables, sparse memory, permissions, masks, and instruction
  fields are testbench-owned. Manager responses correspond to captured requests
  and obey their modeled opcode, source, size, address, mask, and beat rules.
- DCache, PTW, and Uncache traffic is coherent and finite-delay; only modeled
  requests receive responses, and denied data implies corrupt where the protocol
  requires it.
- Generated SVA checks DUT producer stability. Every backpressure-enabled mixed
  seed requires nonzero DCache/PTW/Uncache request-stall and response-delay
  observations, so a manager cannot remain accidentally always-ready.

These are driver guarantees, not assumptions about an internal FSM. The
StoreQueue TLB-miss property remains targeted historical mutation evidence and
is secondary to architectural store/readback and progress checks.

With primary reset asserted, `pin-space` drives every other input bit through zero,
all ones, and 254 deterministic mixed patterns, then checks readback and output
sampling. This proves manifest completeness and wrapper connectivity only. It does
not promote protocol-invalid combinations or any pin to semantic coverage; only
typed drivers and independently checked observations can do that.

## Known Boundary Gaps

The current harness does not yet own a complete legal producer, independent
reference model, or externally observable contract for the following. These
are planned work items, not silently accepted coverage:

The required independent calculation and observation contract for each gap is
defined in [`ORACLES.md#planned-oracles-for-current-boundary-gaps`](ORACLES.md#planned-oracles-for-current-boundary-gaps).
This section owns the plan status, closure conditions, and cross-document
traceability.

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
- active-segment redirect remains a full-core serialization assumption rather
  than a legal MemBlock stimulus; the constrained-random generator ordinary VFOF closes first/later
  fault x Sv39/Sv48 x EEW with exact fix-VL/LQ conservation, while The constrained-random generator
  closes all 1,352 decoder-legal unit-stride Segment FOF fault/mode/shape bins
  with exact data-uop, fix-VL, and terminal-writeback conservation. The decoder
  has no indexed or strided Segment FOF encoding. All 78 legal ordinary unit-
  stride, strided, indexed-unordered, and indexed-ordered configurations, all
  338 legal segment NF/EEW/SEW/LMUL/EMUL configurations per addressing mode,
  all legal whole-register NF/EEW combinations, direct load/store/FOF redirect
  cancellation, and focused first- and later-element segment FOF faults are
  covered;
- broader fixed-PMA region/permission/edge crosses for HLV/HLVX/HSV beyond the
  covered `0x80000000` device-to-DDR transition,
  non-PMA PBMT/misalignment priority (translated PBMT=NC/IO load and store denial is now covered; misalignment priority remains), plus PMP sizes/TOR/lock/permission/
  overlap-by-edge combinations beyond the constrained-random 4-KiB NAPOT relation cross;
  all four nested translation mode pairs, directed and constrained-random PBMT
  PMA/NC/IO priority, random DDR/fixed-device classification, cacheable
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
- reset with dirty CMO persistence, partial AMOADD/AMOCAS or denied/corrupt
  refills, and additional LSQ producer/replay states beyond the covered
  DCache-refill, PTW-walk, Uncache/MMIO, six LR/AMOADD/AMOCAS x W/D cold-refill
  cases, four partial LR.W/D refill cases, twelve singleton/opposite-half-pair
  scalar LD lane/target-half cases, and one clean CMO reset case. Covered
  paths cancel accepted outstanding manager work and check distinct post-reset
  survivors. Partial-refill cases retain any validated early scalar completion
  and observed LQ dequeue, canceling only remaining completion/queue obligations.
  Atomic and partial scalar cases additionally check unchanged reset-target data,
  exact survivor data, and a monitored tail longer than the original response
  delay. Dirty-cache persistence and LR reservation reset are not claimed;
- performance-counter/top-down attribution and hardware-prefetch training
  metadata, which have no stable architectural MemBlock oracle.

These gaps are reported per historical commit in
`HISTORICAL_BUG_AUDIT.md`; a reset-held pin toggle is not counted as closing
them. The historical `vector-guest-fault-split` mismatch is now fixed and is
included in the repaired sentinel and directed boundary case.

## Independent Review and No-Progress Audits

### Independent review gate

A material change to an oracle decision rule, behavioral monitor, acceptance
criterion, constrained-random distribution, coverage claim, or verification-plan
contract requires a non-author, fresh-context review before the next
closure-scale regression. Mechanical prose-only edits do not require a separate
review unless they change one of those contracts.

The reviewer starts from the actual diff and normative source sections, not the
author's summary, and does not receive the author's proposed root cause or
conclusion as a fixed decision. The review is read-only by default. Use one
reviewer; add a second only when oracle correctness and stimulus/coverage validity
are independent risks. If no independent context is available, record the work as
`self-reviewed` and do not claim independence.

The review packet must contain a stable `review_id` and trigger; base and reviewed
revision or exact dirty-file snapshot; author/reviewer identities and context
independence; affected contract and source-of-truth sections; intended behavior,
fixed decisions, and non-goals; changed files and coverage claim; commands, result
paths, replay evidence, and known gaps; and a bounded output request and stop
conditions.

The reviewer checks whether any IO group, legal transaction field, or interaction
is unstimulated or incorrectly constrained; whether plan/`IO_COVERAGE.md` claims
exceed implementation; whether an oracle depends on DUT timing, internal state, a
historical value, or the implementation under test; whether profiles reach the
claimed bins; whether realistic and corner/error traffic are both represented;
whether representative historical mutations remain detectable; and whether the
proposed regression adds useful state-space exploration rather than only runtime.

Return severity-ordered findings with `file:line` or artifact evidence and a
verdict of `pass`, `pass-with-gaps`, `fail`, or `blocked`. The primary agent must
disposition every finding as accepted, rejected with rationale, or deferred with
a tracked gap. Any change to reviewed decision logic requires review of the
changed portion. Record the `review_id`, reviewed revision, scope, reviewer
independence, findings, review finding dispositions, verdict, commands, and
verification artifacts in `RESULTS.md`.

### No-progress gate

At each campaign checkpoint, record measurable progress. Valid progress is a newly
exercised semantic bin with a real oracle, a stimulus dimension that reaches its
intended distribution, a detectable historical mutation, a root-caused failure, a
removed false positive/negative, or a material reproducibility or isolation
improvement. Runtime, action count, and passing-seed count alone are not
progress.

If approximately 24 elapsed hours pass without recorded progress for the same
revision and constraint profile, stop submitting new campaign work. Preserve the
existing artifacts and finish an in-flight seed only when its evidence is needed;
do not extend a campaign merely because it is already running.

Before restarting closure-scale work, perform one bounded read-only historical
review of three to five representative fixes or one coherent bug theme from
`origin/kunminghu-v2`, using the status vocabulary in
[`HISTORICAL_BUG_AUDIT.md`](HISTORICAL_BUG_AUDIT.md#scope-and-evidence-rules).
Return a bounded set of candidate reproducers or constraint/coverage updates with
evidence paths and explicit non-applicable boundary reasons; do not return an
unrestricted history dump. The primary agent dispositions the proposal, then uses
a separately scoped implementation or verification task. Run `check-rtl`, focused
capabilities, and a short regression that reaches the new semantic points before
launching another long campaign.

## Regression Acceptance

Before a duration run:

1. `check-ports`, `check-rtl`, and all Python unit tests pass.
2. `benchmark-tests` passes every scenario registered by the C++ dispatch on
   the current frozen binary. A unit check requires its inventory to remain
   exactly equal to the executable dispatch inventory.
3. The executable, Verilated model, xspcomm, resolved system libraries, and RTL
   metadata are frozen and their resolved paths and runtime layout are checked.
   `final-acceptance` runs `benchmark-tests`, `known-bug-sentinel`,
   `final-regression` + `verify-final-results`, and
   `endurance-regression` + `verify-endurance-results`; each verifier requires
   backpressure, a recorded timeout, and the recorded runtime
   artifact/root/dependencies to match the supplied frozen runtime metadata.

The time-based breadth campaign runs at least 172,800 monotonic seconds (48
hours) with eight workers and the `constrained-random --constraints spec` scenario.

> **Measured performance basis (2026-10-24, no coverage prefix):** the
> `make benchmark-tests` passes 85/85 in **781.752 s**;
> `constrained-random` seed 1/16,384 takes 760.884 s
> (1,840,675 cycles, ~21.5 actions/s). This supersedes the
> 2026-10-23 gated-generator figures (3,750.501 s / 3,721.673 s). The current
> **48 h / 128 seeds** and **200 h / 8 x 1,000,000 actions** budgets were
> derived from the old prefix-heavy stimulus and have not been recalibrated;
> the duration campaigns must be re-measured under the new contract before
> those budgets are used again.

Each seed requests a configurable number of random actions organized as
five-class overlap windows. Every window randomizes producer parameters, legal
alignment class, data, masks, vector shape, issue order, store half order, and
manager delay; sanity waves are kept only where the interface has a proven
legal encoding. There is no mandatory coverage prefix and no
coverage-completeness gate: coverage statistics are written to a per-run report
and remain observational. The artifact records the requested action count and
completed summary count, and the independent verifier checks both. At least 128
complete seeds are required, and the final result must complete after the
duration deadline. Work already submitted at the deadline is allowed to finish.
Any nonzero return, timeout, assertion, scoreboard error, accounting
inconsistency, provenance change, or discontinuous seed range fails acceptance.

Final acceptance additionally runs a finite within-seed endurance campaign:
eight independent `spec` seeds, each with exactly 1,000,000 actions and a
200-hour per-seed timeout. Its verifier requires all 8,000,000 requested
actions, every seed's functional/backpressure gates, and a valid frozen runtime
layout. The two campaigns are complementary: the duration campaign explores
more initial seeds, while the endurance campaign stresses long-lived queue,
pointer-wrap, cache, translation, and manager state without process restarts.

An independent streaming verifier checks the result artifact, duration, scenario
set, action counts, per-seed accounting and reported coverage statistics,
aggregate counts, continuous seeds, eight-worker configuration, and
frozen-runtime layout. It also requires The constrained-random generator completion state, a unique run
id, finite timing values, strict scenario/seed/count terminal summaries, and
per-seed submit/complete offsets spanning the requested duration.

## Failure Triage

Do not add temporary RTL `printf`/debug logic to diagnose a UT failure. Keep
the RTL unchanged and use Picker `mem_direct` from the C++ harness or an FST
waveform analyzed with `pywellen`; see
[`TOOLCHAIN.md#debug-instrumentation-and-rebuild-policy`](TOOLCHAIN.md#debug-instrumentation-and-rebuild-policy). UT-only changes rebuild
only the C++ harness and reuse the model; RTL regeneration/export is only for
a real RTL fix.

1. Replay the exact scenario, seed, action count, and frozen runtime.
2. Reduce the action count while preserving memory and backpressure RNG
   streams.
3. Check the external request/response transcript against the reference model.
4. Add a focused architecture/protocol reproducer before inspecting internal
   state.
5. If historical sensitivity matters, regenerate a separate revert RTL tree
   and require clean pass plus mutant fail under identical test sources.
