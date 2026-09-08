# MemBlock Constrained-Random Interface

## One Generator, Multiple Directions

`random-mixed` is the canonical workload generator. A verification direction is
a constraint set, not another scenario implementation. The same scheduler,
drivers, scoreboards, architectural reference memory, translation machinery,
and completion checks are used for realistic traffic, balanced coverage, and
corner-heavy traffic.

Select a starting preset with `--constraints`, then override individual fields
with repeatable `--constraint key=value` arguments. Presets are conveniences,
not separate tests. An override is applied after the preset.

```sh
# Balanced, high-overlap coverage.
make random-mixed SEED=1 TRANSACTIONS=16384 CONSTRAINTS=coverage

# SPEC-like ordinary traffic and calibrated miss response latency.
make random-mixed SEED=2 TRANSACTIONS=65536 CONSTRAINTS=spec

# Start from SPEC, but deliberately increase translation and MMIO pressure.
make random-mixed SEED=3 TRANSACTIONS=32768 CONSTRAINTS=spec \
  CONSTRAINT='translation-nested=20 translation-switch=100 tlb-flush=100 mmio=30 nc=20 vector-load=80 vector-store=40'

# Restrict the same generator to a scalar-load locality experiment.
make random-mixed SEED=4 TRANSACTIONS=16384 CONSTRAINTS=spec \
  CONSTRAINT='scalar-load=1000 scalar-store=0 vector-load=0 vector-store=0 prefetch=0 atomic=0 nc=0 mmio=0 locality-hot=1000 locality-warm=0 locality-cold=0 concurrent=0 tlb-flush=0 latency=compact'
```

The regression controller exposes the same interface:

```sh
python3 scripts/run_regression.py \
  --binary build/memblock/picker/UT_MemBlock/build/UTMemBlock_example \
  --rtl-metadata build/memblock/rtl.json \
  --output build/memblock/spec-mixed.json \
  --scenarios random-mixed --seeds 32 --mixed-transactions 65536 \
  --constraints spec --constraint concurrent=250
```

## Constraint Fields

Operation and locality fields are relative weights. They do not need to sum to
1000. A zero weight disables that class in the constrained tail. Probability
fields use per-mille values in the inclusive range `0..1000`.

| Field | Meaning |
| --- | --- |
| `scalar-load`, `scalar-store` | Relative scalar load/store weights |
| `vector-load`, `vector-store`, `vector-segment` | Relative vector memory weights; ordinary and segment shapes are selected by the dimensions below |
| `prefetch`, `atomic`, `nc`, `mmio`, `hypervisor`, `cmo`, `ptw-error`, `load-merge`, `set-pressure` | Relative special-operation weights. `load-merge` is one compound action containing two or three same-line scalar loads; `set-pressure` contains nine or ten same-set clean loads or dirty stores |
| `atomic-amo`, `atomic-lrsc`, `atomic-cas` | Relative atomic-family weights inside the `atomic` class |
| `atomic-w`, `atomic-d` | Relative W/D atomic-width weights |
| `atomic-error` | Per-mille share of atomic actions receiving an address-qualified error on a cold AcquireBlock; zero strictly disables injection |
| `atomic-error-denied` | Per-mille denied share among atomic D-channel errors; the other class is independent corrupt |
| `hypervisor-hlv`, `hypervisor-hlvx`, `hypervisor-hsv` | Relative family weights inside the `hypervisor` class |
| `hypervisor-spvp-user` | Per-mille share of hypervisor actions using SPVP=U; zero selects only SPVP=S, 1000 only SPVP=U, and intermediate values require both |
| `hypervisor-pbmt-pma-pma`, `hypervisor-pbmt-pma-nc`, `hypervisor-pbmt-pma-io`, `hypervisor-pbmt-nc-io`, `hypervisor-pbmt-io-nc` | Relative VS/G leaf-PBMT pair weights inside the hypervisor class. The five-pair basis covers final PMA/NC/IO selection and both VS-over-G priority directions |
| `hypervisor-pma-device` | Per-mille share of hypervisor actions translated to an interior address in the fixed SoC `c=0`, R/W, X=0 PMA device interval. Zero selects only ordinary DDR aliases, 1000 only the device interval, and intermediate values require both |
| `hypervisor-pmp-none`, `hypervisor-pmp-first`, `hypervisor-pmp-last`, `hypervisor-pmp-below`, `hypervisor-pmp-above`, `hypervisor-pmp-cross-lower`, `hypervisor-pmp-cross-upper` | Relative physical-PMP relation weights. Edge actions use a PMA/PMA DDR alias and a first-match 4-KiB NAPOT RWX region inside an enclosing deny region; first/last are allowed, while below/above/crossing relations fault |
| `cmo-clean`, `cmo-flush`, `cmo-inval` | Relative operation weights inside the `cmo` class |
| `cmo-dirty` | Per-mille share of CMO target lines made dirty by a committed store that CMO must drain from SBuffer |
| `cmo-younger-overlap` | Per-mille share of CMO actions that issue a younger cold load into another MSHR and require `flushPipe` cancellation with no writeback |
| `cmo-error` | Per-mille share of CMO actions whose opcode-qualified `CBOAck` carries a legal error; zero strictly disables error injection |
| `cmo-error-denied` | Per-mille denied share among error CMO actions; false selects independent corrupt and true selects denied |
| `dcache-load-error` | Per-mille share of weighted scalar-load actions receiving an address-qualified error on a cold AcquireBlock; zero strictly disables injection |
| `dcache-load-error-denied` | Per-mille denied share among DCache load errors; the other class is independent corrupt |
| `ptw-error-stage1`, `ptw-error-gstage`, `ptw-error-nested-g-implicit`, `ptw-error-nested-vs`, `ptw-error-nested-g-final` | Relative PTW manager-error site weights: host stage-1, G-only, implicit G translation of a VS PTE address, VS PTE data, and final nested G walk |
| `ptw-error-root`, `ptw-error-intermediate`, `ptw-error-leaf` | Relative faulting page-table level classes; Sv48/Sv48x4 intermediate selection also reaches both internal levels |
| `ptw-error-store` | Per-mille store share for PTW manager-error actions |
| `ptw-error-denied` | Per-mille denied share; the other class is independent corrupt |
| `ptw-error-corrupt-first` | Per-mille first-beat share among corrupt responses; the other class corrupts the last beat |
| `load-merge-depth2`, `load-merge-depth3` | Relative two- and three-load batch weights inside `load-merge` |
| `load-merge-same-address`, `load-merge-same-beat`, `load-merge-cross-beat` | Relative address patterns inside `load-merge`: exact duplicate, distinct offsets in the critical 32-byte beat, or distinct offsets spanning both beats |
| `set-pressure-depth9`, `set-pressure-depth10` | Relative same-set access counts; an eight-way DCache must replace at least one or two target lines respectively |
| `set-pressure-sb`, `set-pressure-sh`, `set-pressure-sw`, `set-pressure-sd` | Relative access-width weights inside `set-pressure`; clean actions use the corresponding unsigned load width and dirty actions use the store width |
| `set-pressure-set-q0` .. `set-pressure-set-q3` | Relative weights for the four 32-set quarters of the 128-set DCache index space |
| `set-pressure-dirty` | Per-mille share of dirty-store pressure; zero selects only clean-load replacement, 1000 selects only dirty replacement, and intermediate values require both states |
| `set-pressure-refill-overlap` | Per-mille share that holds one address-qualified cold refill response per selected pressure set while replacement produces an attributed Release or ReleaseData; zero/1000 are strict endpoints |
| `set-pressure-release-backpressure` | Per-mille share that forces the first address-attributed target Release or ReleaseData to retain its full C payload for 16 valid cycles; zero/1000 are strict endpoints and unrelated C traffic does not consume the target stall budget |
| `set-pressure-dual-window` | Per-mille share that selects two independently allocated pressure sets instead of one; an overlap action holds one address-qualified cold refill per set, and zero/1000 are strict single/dual endpoints |
| `set-pressure-triple-window` | Per-mille share that selects three pressure sets before single/dual selection. If triple is not selected, `set-pressure-dual-window` chooses between one and two sets; zero disables triple and 1000 selects only triple |
| `locality-hot` | Lines selected from a 32-line hot set |
| `locality-warm` | Lines selected from a 512-line warm set |
| `locality-cold` | Permutation of an 8192-line cold set |
| `translation-bare`, `translation-stage1`, `translation-nested` | Relative Bare, host stage-1, and nested VS+G context weights |
| `stage1-sv39`, `stage1-sv48` | Relative host stage-1 mode weights |
| `vs-sv39`, `vs-sv48` | Relative VS-stage mode weights in nested contexts |
| `g-sv39x4`, `g-sv48x4` | Relative G-stage mode weights in nested contexts |
| `translation-stage1-napot` | Per-mille share of eligible host stage-1 accesses placed in a legal 64-KiB Svnapot region |
| `translation-vs-napot`, `translation-g-napot` | Independent per-mille Svnapot selection for the VS and final G leaf of eligible nested accesses; their product generates all four leaf topologies |
| `translation-switch` | Per-mille chance of choosing a new translation context at a drained action/window boundary |
| `fence-sfence`, `fence-hfence-vvma`, `fence-hfence-gvma` | Relative weights for fence kinds compatible with the active context |
| `fence-global`, `fence-selective` | Relative global/selective scope weights for generated translation fences |
| `concurrent` | Per-mille share of the tail reserved for heterogeneous overlap windows |
| `special-concurrent` | Per-mille chance that a legal overlap window also contains an NC or MMIO load |
| `tlb-flush` | Per-mille chance of a legal translation flush before an operation |
| `misaligned` | Per-mille chance of a misaligned address when width permits it |
| `vector-corner` | Per-mille chance of corner-biased vector shape/address generation |
| `vector-masked` | Per-mille share of ordinary vector instructions with `vm=0`; generated masked instructions contain an observable mask-off body element |
| `vector-vma`, `vector-vta` | Per-mille share of ordinary vector instructions with mask/tail agnostic policy enabled |
| `vector-partial-vl` | Per-mille share of ordinary vector instructions with `vl < VLMAX` |
| `vector-nonzero-vstart` | Per-mille share of ordinary vector instructions with `vstart != 0` |
| `vector-unit-stride`, `vector-strided`, `vector-indexed-unordered`, `vector-indexed-ordered` | Relative ordinary vector addressing-mode weights shared by loads and stores |
| `vector-eew8` .. `vector-eew64`, `vector-sew8` .. `vector-sew64` | Relative ordinary vector index/memory EEW and data SEW weights |
| `vector-lmul-mf8` .. `vector-lmul-m8`, `vector-emul-mf8` .. `vector-emul-m8` | Relative ordinary vector LMUL and derived EMUL weights. Only legal `EMUL = EEW - SEW + LMUL` shapes are generated |
| `vector-segment-store` | Per-mille store share within the vector-segment class |
| `vector-segment-unit-stride`, `vector-segment-strided`, `vector-segment-indexed-unordered`, `vector-segment-indexed-ordered` | Relative segment addressing-mode weights |
| `vector-segment-eew8` .. `vector-segment-eew64`, `vector-segment-sew8` .. `vector-segment-sew64` | Relative segment index/memory EEW and data SEW weights |
| `vector-segment-lmul-mf8` .. `vector-segment-lmul-m8`, `vector-segment-emul-mf8` .. `vector-segment-emul-m8` | Relative fractional/integer LMUL and derived EMUL weights. A shape is enabled only when `EMUL = EEW - SEW + LMUL` and both selected weights are nonzero |
| `vector-segment-nf2` .. `vector-segment-nf8` | Relative segment field-count weights |
| `probe` | Per-mille chance that a completed cacheable scalar store is followed by a manager-originated dirty Probe sequence |
| `probe-to-b` | Per-mille share of generated Probe sequences that retain the line in Branch state; the generator follows each with a toN cleanup Probe |
| `probe-need-data` | Per-mille share of generated Probe sequences that explicitly request data; dirty lines must return exact data even when this is zero |
| `probe-overlap` | Per-mille share of generated Probe sequences that hold an unrelated cold load refill open and queue at least one clean auxiliary Probe plus the primary dirty Probe on distinct B-source IDs |
| `probe-triple-overlap` | Conditional per-mille share of overlapping Probe sequences that use at least two clean auxiliary Probes plus the primary dirty Probe; zero selects depth two, 1000 selects the depth-3..8 group, and intermediate values enable both |
| `probe-depth3` .. `probe-depth8` | Relative weights inside the deep Probe-overlap group. A zero weight disables that exact accepted-but-unanswered depth |
| `nc-store`, `mmio-store` | Per-mille store share within each memory-type class |
| `uncache-error` | Per-mille share of NC/MMIO actions receiving a legal error response; zero strictly disables injection |
| `uncache-load-error-denied` | Per-mille denied share among error loads; the other load class is independent corrupt. Error stores always use denied because `AccessAck` carries no data |
| `stride-stream` | Per-mille chance that a scalar load joins a fixed-PC, 128-byte-stride cold stream; nonzero settings reserve eight closing loads so every seed can train the L1 stride prefetcher |
| `latency` | Set DCache, PTW, and Uncache to `compact` or `spec` together |
| `dcache-latency`, `ptw-latency`, `uncache-latency` | Override one manager's latency profile independently |

Invalid names, all-zero operation/locality, enabled atomic/hypervisor-family,
atomic-width, or enabled vector-shape dimensions, unreachable vector shape
classes, incompatible fixed vector shape/policy combinations, out-of-range
per-mille values, inconsistent special-concurrency or
manager-latency settings, enabled PTW errors with no reachable site, level, or
governing page-table mode, enabled load merging with no reachable depth or
address pattern, enabled set pressure with no reachable depth, width, or set
quarter, and unknown latency profiles fail before simulation
traffic begins. The harness has no programmable PMA region at this boundary,
so randomized NC and MMIO traffic requires stage-1 or nested PBMT translation.
An NC/MMIO-only operation mix cannot also request Bare coverage.
Hypervisor traffic requires nested translation; a hypervisor-only mix cannot
also require Bare or host-stage-1 contexts. Fixed-PMA device traffic requires
the PMA/PMA PBMT pair and an aligned class; a device-only mix cannot enable a
non-PMA PBMT pair or misalignment.
An enabled `stride-stream` requires nonzero scalar-load and cold-locality
weights because the prefetch oracle depends on real cold load misses.
`random-mixed` requires at least 1056 actions so the mandatory architectural
prefix, four overlap windows, and each enabled constrained class can coexist.
An enabled `atomic-error` requires a nonzero atomic operation weight. An
all-error Uncache mix requires `special-concurrent=0`, because the current
special overlap slot is a nonfaulting load and cannot legally satisfy a
100-percent precise-error constraint.

## Constraint Extension Contract

The interface is the verification abstraction, not only a command-line
convenience. A new workload direction must be expressible as a set of fields on
`RandomConstraints` and must continue to use the same transaction builders,
scheduler, reference model, scoreboards, and final drain. Presets only assign
defaults to those fields. They must not add hidden phases or select another
scenario implementation.

For every new field, the generator must also provide all of the following:

- strict value/range validation before any request is issued;
- the resolved target in the replayable terminal summary and regression
  artifact;
- an observed counter that distinguishes the generated class from neighboring
  classes;
- a per-seed coverage obligation whenever the field enables a class;
- deterministic replay from the seed and complete constraint assignment.

The interface audit tracks both completed dimensions and the remaining common
generator work. These are interface dimensions, not reasons to create more
scenario implementations:

| Dimension | Implemented common-interface behavior | Remaining work |
| --- | --- | --- |
| Concurrent operation mix | Base windows overlap scalar load/store, vector load/store, and prefetch; `special-concurrent` can add NC/MMIO loads and records each class | Add more legal dependency-aware window shapes as their upstream scheduling contracts are modeled |
| Atomic subtype and errors | `atomic-amo`, `atomic-lrsc`, `atomic-cas`, `atomic-w`, and `atomic-d` select legal AMO, LR/SC, and compare-dependent AMOCAS sequences. Schema 18 adds `atomic-error`/`atomic-error-denied`, closes all 18 enabled family x width x clean/corrupt/denied outcomes, and checks the exact exception plus two-beat refill, errored GrantAck/refill, unchanged manager memory at response time, and deterministic poisoned-line cache image if it is later released. LRSC error actions stop at LR because a cold SC cannot request a refill | Cross-hart reservation interference remains integration-level |
| Hypervisor subtype, effective privilege, alignment, PBMT, PMA class, and PMP edge relation | `hypervisor-hlv`, `hypervisor-hlvx`, and `hypervisor-hsv` select legal nested-translation operations. Schema 22 adds `hypervisor-spvp-user`, maps independent U=1 VS regions for all four 4-KiB/Svnapot VS/G leaf combinations to known physical bytes, and closes every enabled family x SPVP=S/U cross per seed. Schema 23 reuses `misaligned` and closes every enabled family x SPVP x aligned/misaligned cross with a physical data/side-effect oracle. Schema 24 adds the five-pair `PMA/PMA`, `PMA/NC`, `PMA/IO`, `NC/IO`, and `IO/NC` PBMT basis and closes every enabled family x SPVP x PBMT pair while checking final PA, bytes, and DCache/Uncache route. Schema 25 adds `hypervisor-pma-device` and closes every enabled family x SPVP x DDR/fixed-device cross. Schema 26 adds no-PMP/first/last/below/above/cross-lower/cross-upper weights for a 4-KiB NAPOT allow region nested in a 16-KiB deny region and closes every family x SPVP x relation with exact fault, manager, queue-recovery, and post-HSV HLV-readback checks | Compose non-PMA PBMT with misalignment only after its exception-priority contract is modeled; add other fixed-PMA boundaries and PMP sizes/TOR/lock/permission/overlap-by-edge crosses to the same class |
| Cache maintenance | `cmo`, `cmo-clean`, `cmo-flush`, `cmo-inval`, `cmo-dirty`, `cmo-younger-overlap`, `cmo-error`, and `cmo-error-denied` select CMO actions in the active Bare/stage-1/nested context. Success derives exact clean/dirty Probe reports and data; error responses require the exact exception, no Probe, unchanged backing memory, and redirect cleanup, with optional younger-miss cancellation in both paths | Add simultaneous multi-class windows and multiple CMO sources |
| Ordinary vector shape | Addressing, EEW, SEW, LMUL, and derived EMUL are composable weights shared by vector loads/stores. The generator enumerates all legal shapes, prioritizes uncovered classes, expands one instruction into 1..8 uops, applies the indexed `EMUL>LMUL` shared-Vd mapping, and streams large flow groups through queue-capacity windows | Lift these shapes into every heterogeneous overlap-window slot; the current rolling windows retain their baseline single-uop vector members while the constrained serial tail interleaves full shapes with all other operation classes |
| Vector segment | Addressing, EEW, SEW, LMUL, derived EMUL, NF, and load/store direction are composable weights. The generator enumerates only decoder-legal shapes, prioritizes uncovered enabled classes, models complete index groups and index-only uops, and reports/conserves every dimension | Lift FOF and redirect into low-rate common dimensions only after their multi-uop cancellation scheduling is modeled without hidden directed phases |
| NC/MMIO direction and errors | `nc-store` and `mmio-store` steer load/store direction. Schema 16 adds `uncache-error` and `uncache-load-error-denied`; all enabled NC/MMIO x load/store x legal clean/corrupt/denied outcomes close per seed. Loads require exact HardwareError/LoadAccessFault, MMIO denied stores require final StoreAccessFault, and committed NC denied stores require only an external error report, unchanged memory, normal SQ dequeue, and no redirect | Concurrent special stores and malformed/duplicate/early/late responses remain deferred |
| Translation state | Bare/Sv39/Sv48 and all four Sv39/Sv48 x Sv39x4/Sv48x4 pairs are weighted tail contexts; host NAPOT and independent nested VS/G NAPOT placement select distinct real page-table regions; switches occur only at drained boundaries; every enabled leaf topology, cold walk/reuse, and the legal fence kind/scope matrix are per-seed gates | Distinct-page walks and redirected root/ASID/VMID/MODE/`V` changes with delayed PTW responses are covered by directed matrices; random context changes remain restricted to drained boundaries |
| Response latency | `latency` sets all managers; `dcache-latency`, `ptw-latency`, and `uncache-latency` override them independently, with separate observed histograms and gates | Add finer numeric/distribution controls only when a calibrated workload needs them |
| Cache Probe | `probe`, `probe-to-b`, `probe-need-data`, `probe-overlap`, `probe-triple-overlap`, and `probe-depth3`..`probe-depth8` generate manager Probes after randomized dirty scalar stores, check exact 64-byte ProbeAckData, cover toB/toN and requested/mandatory data, and invalidate retained toB lines with a checked cleanup Probe. Schema 33 derives the eight-entry capacity from the standard DCache configuration, holds an unrelated cold refill, queues up to seven clean auxiliaries plus the dirty primary, and keeps C unready until every selected B request is accepted. All 32 depth x cap x need-data bins close with distinct active B sources, address-matched C responses, exact outstanding depth, and no early delayed-load writeback. Schema 34 walks the complete six-bit B-source namespace across sequences, rejects active-ID reuse, and exactly checks unique IDs, completed-ID reuse, and every 63-to-0 wrap | Compose Probe overlap with more operation classes and malformed manager traffic |
| Set replacement concurrency | Schema 31 adds hierarchical `set-pressure-triple-window` selection after schema 30's single/dual control, crosses one/two/three windows with every clean/dirty, refill-overlap, C-backpressure, depth, width, and translation class, and closes 576 bins. Multi-window actions allocate distinct set quarters. Overlap actions keep one address-qualified D response per set pending and require every set to reach its own replacement minimum before any response is released; all request/writeback/dequeue accounting is weighted by window count | Extend to four-or-more independent sets and compose replacement with Probe/CMO concurrency only after their legal scheduling and attribution contracts are explicit |
| Hardware data prefetch | `stride-stream` composes fixed-PC stride training with the common scalar/vector/atomic/NC/MMIO, translation, miss/refill, latency, and Probe generator; every enabled seed must observe source 12 on the L2 sender | Add SMS/stream causality and arbitration plus a positive L3-enabled configuration |
| Error injection | Schema 15 adds opcode-qualified CMO denied/corrupt injection. Schema 16 adds Uncache errors with exact response/D-beat accounting and the distinct NC versus MMIO store contracts. Schema 17 adds ordinary scalar-load refill errors with exact clean/corrupt/denied, D-beat, errored-refill, and sink-attributed GrantAck accounting under Bare or translated traffic. Schema 18 adds the same common control and manager conservation to AMO/LR/AMOCAS across W/D widths. Schema 19 adds address-qualified PTW denied/first-beat-corrupt/last-beat-corrupt injection at five host/G/nested walk sites across load/store, root/intermediate/leaf, and all Sv39/Sv48 and Sv39x4/Sv48x4 modes. All enabled outcomes close per seed | Malformed, duplicate, and unsolicited manager responses remain deferred |

The remaining rows do not change the architecture: `coverage`, `spec`, and
`corner` are settings of the same generator. Closing them means lifting each
choice into the common interface and its coverage contract, not adding
`random-spec`, `random-corner`, or feature-specific random scenario functions.

## Shipped Presets

Operation columns are relative weights. Locality is `hot/warm/cold`; the
remaining numeric direction columns are per-mille values.

| Preset | Scalar L/S | Vector L/S | VSegment | Prefetch | Atomic | NC | MMIO | Hypervisor | CMO | Locality | Concurrent | TLB flush | Misaligned | Vector corner | Probe/overlap | Stride stream | Latency |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- | ---: | ---: | ---: | ---: | --- | ---: | --- |
| `coverage` | 200/150 | 150/150 | 75 | 100 | 100 | 75 | 75 | 75 | 75 | 250/250/500 | 1000 | 50 | 500 | 1000 | 20/500 | 500 | compact |
| `spec` | 648/270 | 20/10 | 1 | 35 | 5 | 5 | 5 | 1 | 1 | 800/150/50 | 100 | 20 | 5 | 100 | 1/10 | 100 | spec |
| `corner` | 125/125 | 125/125 | 125 | 125 | 125 | 125 | 125 | 125 | 125 | 100/200/700 | 500 | 100 | 500 | 1000 | 100/750 | 750 | spec |

Atomic family weights (AMO/LRSC/CAS) are `8/2/2`, `90/5/5`, and `1/1/1` for
`coverage`, `spec`, and `corner`; all three use `1/1` W/D weights. Their
hypervisor family weights (HLV/HLVX/HSV) are `1/1/1`, `90/5/5`, and `1/1/1`.
Their fixed-PMA device shares are `500`, `1`, and `500` per mille, preserving a
low-probability but coverage-forced device sample in the SPEC-like profile.
Their physical-PMP relation weights are `1/1/1/1/1/1/1`,
`999994/1/1/1/1/1/1`, and `1/1/1/1/1/1/1`; the first slot is the no-PMP
control. Deficit scheduling still closes each rare SPEC-like edge relation.
Their CMO operation weights (CLEAN/FLUSH/INVAL) are `1/1/1` in every preset.
Dirty-line rates are `500`, `50`, and `500` per mille, and younger-load overlap
rates are `500`, `10`, and `750`. Thus `spec` retains a verification floor
without making cache maintenance or its pipeline flush artificially common.
CMO error rates are `100`, `0`, and `500` per mille for `coverage`, `spec`, and
`corner`; the denied share is `500` in all three presets. Ordinary SPEC-like
traffic therefore injects no manager error, balanced coverage keeps errors
rare after its mandatory cross closes, and corner traffic emphasizes them.
Atomic error rates use the same `100`, `0`, and `500` values, with a 500
per-mille denied share. Error actions use unique cold identity-mapped lines;
ordinary SPEC-like atomic traffic therefore remains free of synthetic errors.
PTW error operation weights are `75`, `0`, and `125` for
`coverage`, `spec`, and `corner`. All presets retain equal nonzero site and
level weights plus 500-per-mille store, denied, and first-corrupt-beat shares,
but the zero `spec` operation weight keeps synthetic translation errors out of
the realistic workload. Coverage and corner runs use fresh page-table roots
and addresses so each enabled fault is forced through the selected walk level.
Load-merge operation weights are `100`, `10`, and `125` for `coverage`, `spec`,
and `corner`. Coverage and corner use equal `2/3`-way and
same-address/same-beat/cross-beat weights. SPEC favors depth two by `19/1` and
same-beat locality by `1/8/1`, retaining low-rate duplicate and cross-beat
cases without making them look like ordinary traffic.
Set-pressure operation weights are `75`, `2`, and `125` for `coverage`, `spec`,
and `corner`. Coverage and corner use equal depth, width, and set-quarter
weights. SPEC favors depth nine by `9/1`, widths by `1/2/6/20`, and keeps set
quarters uniform. Thus realistic traffic retains occasional conflict eviction
without turning every ordinary store into an artificial capacity test.
Uncache error rates use the same `100`, `0`, and `500` values, with a 500
per-mille denied share among error loads. The `spec` preset therefore models
ordinary traffic without frequent external errors, while `coverage` and
`corner` retain the complete legal outcome matrix.
DCache scalar-load error rates also use `100`, `0`, and `500`, with a 500
per-mille denied share. Error actions use dedicated cold lines; clean actions
retain the configured hot/warm/cold locality mix. A 1000 error rate is
incompatible with nonzero `stride-stream` or `concurrent`: stride-prefetch
training requires clean scalar loads, while a faulting scalar load precisely
redirects and therefore cannot occupy the fixed clean mixed-window slot.
Their vector-segment store shares are `500`, `300`, and `500` per mille.
`coverage` and `corner` weight every ordinary and segment
addressing/EEW/SEW/LMUL/EMUL class equally, plus every segment NF. `spec`
favors unit stride (`980/10/5/5` across unit, strided, indexed-unordered,
indexed-ordered), M1/M2, and 32-bit SEW for both operation families, and small
segment NF, while retaining a nonzero verification floor for every legal
class. Per-seed closure temporarily prioritizes uncovered values; subsequent
ordinary and segment choices follow the products of their configured dimension
weights.
NC/MMIO store shares are respectively `500/500`, `300/300`, and `500/500`.
Their legal NC/MMIO overlap rates are `500`, `20`, and `750` per mille.
All three presets split generated Probes equally between toB/toN and explicit
need-data/no-need-data requests. Since the candidate line is dirty, both
need-data values require exact ProbeAckData; the bit tests the protocol rule,
not whether the oracle checks returned bytes. Their overlap rates are `500`,
`10`, and `750` per mille. Thus normal `spec` traffic retains a low-rate
two-source concurrency floor while `corner` emphasizes it. SQ retirement only
transfers a committed store into SBuffer, so the Probe sequence first drains
older SBuffer traffic. This prevents a legal early NtoN response from being
misclassified as a dirty-line failure under the long-tail latency profile.
Their stride-stream rates are `500`, `100`, and `750` per mille. The lower
`spec` value keeps prefetch training present without turning ordinary load
traffic into an artificial continuous stream; mandatory closing loads retain
the per-seed output gate even at this low rate.
Set-pressure dirty shares are `500`, `50`, and `500` per mille. Thus `spec`
keeps clean replacement dominant while preserving a nonzero dirty floor;
coverage and corner balance both line states.
Refill-overlap shares are `500`, `10`, and `750` per mille. Deficit scheduling
still requires every enabled clean/dirty x overlap cross in finite runs, while
the steady-state SPEC-like distribution keeps this expensive coincidence rare.
`spec` and `corner` use the calibrated long-tail profile independently on all
three managers; `coverage` uses compact latency.

Translation presets use these relative weights and per-mille switch rates:

| Preset | Bare/stage-1/nested | Stage-1 Sv39/Sv48 | VS Sv39/Sv48 | G Sv39x4/Sv48x4 | Stage-1/VS/G NAPOT | SFENCE/VVMA/GVMA | Global/selective | Switch |
| --- | --- | --- | --- | --- | --- | --- | --- | ---: |
| `coverage` | 1/1/1 | 1/1 | 1/1 | 1/1 | 500/500/500 | 1/1/1 | 1/1 | 500 |
| `spec` | 5/990/5 | 95/5 | 1/1 | 1/1 | 1/1/1 | 98/1/1 | 95/5 | 1 |
| `corner` | 1/1/1 | 1/1 | 1/1 | 1/1 | 500/500/500 | 1/1/1 | 1/1 | 750 |

The mandatory per-seed gate overrides sampling order only until every enabled
mode, nested pair, enabled ordinary/NAPOT leaf topology, and compatible fence
kind/scope has appeared. Later choices follow the configured weights. At that
boundary the generator immediately
reselects the highest-weight enabled context, so the last mandatory rare mode
is not retained for the normal low switch interval. Later random context
switches still select from the configured distribution. This preserves
coverage for short seeds without turning a long `spec` campaign into an
equal-probability corner campaign.

`coverage` is appropriate for short pre-submit checks. `spec` is the default
for extended/final campaigns. `corner` is the default for boundary hunts. A
campaign can sweep several constraint sets, but each run still invokes the same
generator and records the resolved targets in its terminal summary.

`random-stress` remains as a compatibility scenario for historical artifacts
and its older burst-specific acceptance gates. It is not the template for new
workload directions. New directions should add a reusable constraint dimension
to `RandomConstraints`, then exercise it through `random-mixed`.

## Performance Calibration

The `spec` preset was calibrated from the final measurement blocks of 4,206
SPEC CPU checkpoint logs in these local datasets:

- `cr260831-4f29a0951-KunminghuV2Config` (1,092 checkpoints)
- `cr260902-5d3934132-KunminghuV2Config` (3,114 checkpoints)

Only the final counter block in each `simulator_err.txt` was counted, avoiding
double counting periodic cumulative dumps. Relevant aggregates were:

| Dataset | Loads | Stores | Load-unit first-issue TLB misses | DCache real misses | Miss allocations | Mean MSHR A-to-D |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `4f29a0951` | 5,449,853,667 | 2,335,870,195 | 89,439,821 | 572,769,182 | 483,469,995 | 30.28 cycles |
| `5d3934132` | 15,297,507,427 | 6,294,915,699 | 475,042,094 | 1,491,848,202 | 1,039,068,487 | 32.16 cycles |
| Combined | 20,747,361,094 | 8,630,785,894 | 564,481,915 | 2,064,617,384 | 1,522,538,482 | about 31.6 cycles |

The TLB column is specifically the sum of the three load-unit
`s1_tlb_miss_first_issue` counters. It is not a sum of every DTLB port's
`first_miss` counter.

The additional `cr260831-8f8494560-KunminghuV2Config` dataset contains 27 mcf
checkpoints. Its final blocks report 196,764,856 loads, 54,612,731 stores,
83,439,410 load-unit first-issue TLB misses, 128,703,205 DCache real misses,
78,242,102 miss allocations, and a 36.35-cycle mean MSHR A-to-D latency. This
small, memory-bound subset is useful as a high-miss stress reference, but is
not pooled at equal weight with the two broad SPEC datasets above.

The combined ordinary memory mix is about 70.6% loads and 29.4% stores. Atomic
miss allocations were only 19,008 and 225,708 in the two datasets; reported
MMIO loads/stores were also only thousands, and the sampled NC counters were
zero. Those events therefore receive small verification floors rather than
being made artificially common in `spec`. CMO error injection is disabled in
the `spec` preset and remains concentrated in focused contracts plus balanced
or corner campaigns.

The calibrated first-beat DCache/PTW/Uncache response latency distribution is
approximately 74.1% below 20 cycles, 14.4% at 20-39, 5.1% at 40-99, and 6.4%
at 100-400 cycles. The original histogram counters overlap at some boundaries,
so these probabilities are a test model rather than an exact performance-model
claim. The first four `spec` responses deterministically cover one sample in
each latency class; later responses follow the distribution statistically.

## Coverage And Replay Contract

Every terminal line prints `constraint_schema=34`, the resolved target weights,
and actual operation, atomic family/width, hypervisor family/SPVP/alignment/PBMT/
DDR-versus-fixed-PMA-device/PMP-relation crosses, CMO operation/
line-state/younger-overlap/error presence/error kind, DCache scalar-load
clean/corrupt/denied and manager-error accounting, ordinary-vector
direction/addressing/EEW/SEW/LMUL/EMUL/instruction/uop counts, vector-segment direction/
addressing/EEW/SEW/LMUL/EMUL/NF, NC/MMIO direction, legal special overlap,
locality, translation regime/mode/pair and stage-1/nested leaf topology, fence
kind/scope, cold-walk/reuse,
TLB-flush, hit/miss, Probe sequence/cap/need-data/overlap and maximum outstanding
depth plus B-source lifecycle, all three scalar-load
wakeup/cancel lanes, IFU software instruction-prefetch observations, L2
stride-prefetch observations, and per-manager latency counts. Each enabled
class must be observed at least once. Every ordinary shape dimension conserves
against `actual_vector_shape_ops`; uops must remain in the architectural 1..8
range and any enabled multi-uop shape must produce a multi-uop instruction.
The five ordinary-vector binary policies report false/true instruction counts.
A target of zero permits only false, 1000 permits only true, and an intermediate
target requires both; each pair also conserves against
`actual_vector_shape_ops`. When both `vma` and masking are enabled, at least one
load must contain an actual mask-off body element; when both `vta` and partial
VL are enabled, at least one load must contain an actual tail element. These
semantic observations are reported as `actual_vector_agnostic=mask,tail`, so a
control bit without a relevant inactive element cannot close coverage.
CMO operation, clean/dirty line-state, and no-overlap/younger-overlap counts each
conserve exactly against the CMO operation count. A zero operation weight or a
fixed binary target must also leave its disabled observed bin at zero.
CMO error presence also conserves against that count. When errors are enabled,
corrupt/denied counts and every enabled CLEAN/FLUSH/INVAL x error-kind cross are
required and conserved; an error CMO contributes no manager Probe because the
operation failed. The offline verifier subtracts exactly those actions from
CMO-derived Probe conservation.
DCache scalar-load clean/corrupt/denied outcomes are likewise exact enabled-bin
gates. Their aggregate clean/error and corrupt/denied counters must conserve
against the outcome bins. For `N` error actions containing `D` denied actions,
the manager tuple `error responses, denied beats, corrupt beats, errored
GrantAcks, errored refills` must equal `N, 2D, 2N, N, N`; sink attribution
keeps this invariant valid when a clean hardware prefetch refill is concurrent.
Load merging reports 12 depth x address-pattern x critical-beat bins plus three
translation-regime bins. Every enabled shape and Bare/stage-1/nested regime
must occur. A batch uses a fresh line from a 64-MiB region, all members issue in
one cycle, and the target line must produce exactly one DCache request. Global
refill and GrantAck deltas remain equal and at least one because the same loads
may also trigger legal hardware prefetches; scalar writebacks must equal the
sum of the generated batch depths.
Set pressure reports 192 clean/dirty x no-overlap/refill-overlap x
no-C-stall/C-stall x depth x width x translation crosses, four set-index
quarters, both store-half issue orders, and manager tuples. Each
action uses fresh lines in one physical set. Generated stores, target requests,
store writebacks, and SQ dequeues must match exactly. Dirty target ReleaseData
must reach at least `depth - 8`, every global ReleaseData must pass its byte
oracle, and every attributed dirty target release must appear with the stored
bytes in manager memory. Clean actions require exact data on both the initial
fill and reverse revisit, one initial request per target line, at least
`depth - 8` revisit misses and address-attributed target Releases, zero target
ReleaseData, and exact load-writeback/LQ-dequeue conservation. Release address
history is enabled only for the duration of one action, so this oracle does not
grow memory with regression length. In an overlap action, the manager holds the
response for one independently addressed cold load outside the pressure set and
allows later D responses on other sources to bypass it. At least `depth - 8`
target Releases must complete while that load remains pending; its exact one
request, writeback, and LQ dequeue are separately conserved. The 4-GiB sparse
region supports at least one million actions per fixed set quarter without
reusing a target line. The C-backpressure manager tuple is `selected actions,
stalled target releases, target stall cycles, target payload-stability checks,
completed windows`. A selected action requires exactly `1,1,16,16,1`; an
unselected action requires `0,0,0,0,1`. The agent selects by target line and
Release opcode, so ProbeAck and background Release traffic remain legal but do
not satisfy the oracle. Clean refill-overlap establishes the held response
before filling the target set; dirty overlap completes all older stores before
holding the younger refill and committing those stores.
PTW errors report 90 site x direction x level-class x outcome bins, 20
site-specific mode bins, 20 target-level bins, and a manager tuple of error
responses/denied beats/corrupt beats. Every enabled bin must be nonzero and
every disabled bin zero. Duplicate requests for one faulting PTE are legal, so
response count may exceed action count; denied beats must be even and cover at
least two beats per denied action, while corrupt beats must equal all response
beats plus the additional denied beat. The action also requires the precise
access fault, no target DCache or Uncache request, a clean same-address retry,
and a fresh reread of the injected PTE block. PTW-only constraint sets do not
inherit ordinary translation-switch or TLB-flush coverage gates.
Probe subclass counts conserve against the generated sequence count. Manager
Probe traffic additionally conserves primary sequences, toB cleanup requests,
CMO-derived Probes, and `depth - 1` auxiliary clean Probes. Schema 33 uses
`probe-overlap` to select depth one versus overlap, conditionally uses
`probe-triple-overlap` to select depth two versus the deep group, and applies
`probe-depth3`..`probe-depth8` inside that group. The authoritative eight-field
`actual_probe_depth` counts must conserve to sequences and project exactly onto
the compatibility `actual_probe_overlap` counts. `actual_probe_cross` closes
all 32 enabled depth x cap x need-data bins and must reproduce every depth/cap/
data marginal. Holding C until all selected B requests fire makes the maximum
accepted-but-unanswered depth an exact external observation. Schema 34 reports
`probe_source_space=64` from the generated B-source width and
`probe_source_lifecycle=unique,reuse,wrap`. For `N` accepted manager
Probes, the required tuple is `min(N,64),N-min(N,64),(N == 0 ? 0 :
(N - 1) / 64)`; the source allocator also forbids reuse while an earlier
response with that ID is active. Every
load lane must observe both backend wakeup and cancel events without constraining
their count relationship: a split or replayed request can legally emit multiple
cancels for one architectural load. Every seed must emit at least one `prefetch.i`
request toward the frontend. More than one
enabled translation context also
requires an observed switch, and any translated profile requires both a PTW
walk window and a reuse window. With backpressure, each manager set to `spec`
must independently observe all four latency classes. The simulator and offline
artifact verifier both enforce these obligations. They are minimum gates;
distribution quality is evaluated over long multi-seed campaigns from the
recorded counts.

Hardware prefetch traffic shares load-pipeline cancel outputs but has no
backend wakeup by design. When `stride-stream` is enabled, the architectural
load replay gate therefore uses a counter snapshot taken immediately before
the prefetcher is enabled. The terminal summary reports that gate window as
`load_wakeups/load_cancels` and the full simulation as
`raw_load_wakeups/raw_load_cancels`; schema 11 requires each raw count to be at
least its corresponding snapshot count. L2 source-12 observation is checked
separately, so this separation neither hides prefetch activity nor mistakes it
for a failed backend load.

The mandatory prefix still closes architectural invariants that should never be
left to chance, including width/lane legality, nested translation mode pairs,
fault metadata, dirty eviction, redirect, and queue accounting. The configurable
tail determines the workload direction and composes those mechanisms under
pressure.

The harness also models upstream scheduling contracts needed for legal replay.
Scalar stores are reissued when the DUT reports a replay. A vector load whose
active element crosses a 16-byte boundary is advanced to the modeled ROB head
before issue, matching `LoadMisalignBuffer` eligibility. Failure to drive that
backend state creates a UT deadlock, not an RTL failure. Atomic operations are
also intentionally absent from overlap windows: `MemBlock` documents that
LR/SC/AMO blocks the pipeline and redirects load-unit-0 control while active.
They remain constrained-random serializing actions in the same generator.
Translated MMIO stores reissue their address from observed store-TLB miss
feedback until a hit is observed before the ROB-head pulse. NC and MMIO actions
are never emitted in Bare because only PBMT supplies those attributes in this
harness. Misaligned vector stores similarly drive ROB-head/pending state and
replay; concurrent vector replay feedback is matched to the originating load
or store transaction by queue identity. Their SQ retirement target is captured
when their flows are enqueued, so a writeback that retires before the helper's
explicit commit step cannot be counted twice.
Scalar store retirement uses the same enqueue-time target. This matters for a
misaligned store held at the ROB head: it can legally leave the SQ while the
other transaction classes in the overlap window are still draining, before
the driver reaches its explicit architectural-memory update.
