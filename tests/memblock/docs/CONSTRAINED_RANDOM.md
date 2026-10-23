# MemBlock Constrained-Random Interface

## One Generator, Multiple Directions

`random-mixed` is the canonical workload generator. A verification direction is a constraint set,
not another scenario implementation. The same scheduler, drivers, scoreboards, architectural
reference memory, translation machinery, and completion checks are used for realistic traffic,
balanced coverage, and corner-heavy traffic. The current executable contract is `constraint_schema=53`.

The complete top-level interface mapping and interface gaps are in
`IO_COVERAGE.md`; `VERIFICATION_PLAN.md` owns the cross-domain verification
gap inventory. This document defines configurable behavioral dimensions and the
generator-level exclusions they do not cross. It does not claim that every
flattened pin is a standalone random variable. Select a preset with
`--constraints`, then override individual fields with repeatable
`--constraint key=value` arguments. Overrides are applied after the preset.

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
  --output build/memblock/spec-mixed.json \
  --scenarios random-mixed --seeds 32 --mixed-transactions 65536 \
  --constraints spec --constraint concurrent=250
```

## Constraint Fields

Operation and locality fields are relative weights and do not need to sum to
1000. A zero weight disables that class in the constrained tail. Probability
fields use per-mille values in the inclusive range `0..1000`.

| Field | Meaning |
| --- | --- |
| `scalar-load`, `scalar-store` | Relative scalar load/store weights |
| `vector-load`, `vector-store`, `vector-segment` | Relative vector memory weights; ordinary and segment shapes are selected by the dimensions below |
| `prefetch`, `atomic`, `nc`, `mmio`, `hypervisor`, `cmo`, `ptw-error`, `load-merge`, `set-pressure`, `miss-burst` | Relative special-operation weights. `load-merge` is one compound action containing two or three same-line scalar loads; `set-pressure` contains replacement traffic in disjoint windows of two physical sets; `miss-burst` issues distinct cold-line scalar loads or a scalar/vector-load mixture while their refills are held |
| `atomic-amo`, `atomic-lrsc`, `atomic-cas` | Relative atomic-family weights inside the `atomic` class |
| `atomic-w`, `atomic-d` | Relative W/D atomic-width weights |
| `atomic-error` | Per-mille share of atomic actions receiving an address-qualified error on a cold AcquireBlock; zero strictly disables injection |
| `atomic-error-denied` | Per-mille denied share among atomic D-channel errors; the other class is independent corrupt |
| `atomic-probe-depth0` .. `atomic-probe-depth8` | Relative Probe-burst weights for successful atomic actions. Depth zero preserves the ordinary path. Depths one through eight hold a cold target refill, queue that many distinct clean auxiliary Probes under C backpressure, then release D. The DCache may backpressure B until D is released; afterward the full burst must be accepted before C resumes. Errors issue no Probe |
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
| `cmo-probe-depth1` .. `cmo-probe-depth8` | Relative total Probe-depth weights for successful CMO actions. The target-line Probe plus zero through seven distinct clean auxiliary Probes are accepted while CBOAck is pending; error CMOs issue no Probe |
| `dcache-load-error` | Per-mille share of weighted scalar-load actions receiving an address-qualified error on a cold AcquireBlock; zero strictly disables injection |
| `dcache-load-error-denied` | Per-mille denied share among DCache load errors; the other class is independent corrupt |
| `ptw-error-stage1`, `ptw-error-gstage`, `ptw-error-nested-g-implicit`, `ptw-error-nested-vs`, `ptw-error-nested-g-final` | Relative PTW manager-error site weights: host stage-1, G-only, implicit G translation of a VS PTE address, VS PTE data, and final nested G walk |
| `ptw-error-root`, `ptw-error-intermediate`, `ptw-error-leaf` | Relative faulting page-table level classes; Sv48/Sv48x4 intermediate selection also reaches both internal levels |
| `ptw-error-store` | Per-mille store share for PTW manager-error actions |
| `ptw-error-denied` | Per-mille denied share; the other class is independent corrupt |
| `ptw-error-corrupt-first` | Per-mille first-beat share among corrupt responses; the other class corrupts the last beat |
| `load-merge-depth2`, `load-merge-depth3` | Relative two- and three-load batch weights inside `load-merge` |
| `load-merge-same-address`, `load-merge-same-beat`, `load-merge-cross-beat` | Relative address patterns inside `load-merge`: exact duplicate, distinct offsets in the critical 32-byte beat, or distinct offsets spanning both beats |
| `set-pressure-depth9`, `set-pressure-depth10` | Relative counts of nine/ten distinct target lines per window. The generated four-way, 256-set DCache places each 8-KiB-stride window in two physical sets, with total capacity eight; at least one/two target Releases are required over the complete fill |
| `set-pressure-sb`, `set-pressure-sh`, `set-pressure-sw`, `set-pressure-sd` | Relative access-width weights inside `set-pressure`; clean actions use the corresponding unsigned load width and dirty actions use the store width |
| `set-pressure-set-q0` .. `set-pressure-set-q3` | Relative weights for four quarters of the 128 low-seven-bit base indices. Each base index selects a pair of physical sets separated by 128 |
| `set-pressure-dirty` | Per-mille share of dirty-store pressure; zero selects only clean-load replacement, 1000 selects only dirty replacement, and intermediate values require both states |
| `set-pressure-refill-overlap` | Per-mille share that holds one address-qualified cold refill response per selected pressure window while replacement produces an attributed Release or ReleaseData; zero/1000 are strict endpoints |
| `set-pressure-release-backpressure` | Per-mille share that forces the first address-attributed target Release or ReleaseData to retain its full C payload for 16 valid cycles; zero/1000 are strict endpoints and unrelated C traffic does not consume the target stall budget |
| `set-pressure-window1` .. `set-pressure-window8` | Relative weights for one through eight independent windows over disjoint pairs of physical sets. A zero weight disables that exact window count; an overlap action holds one address-qualified cold refill per selected window |
| `miss-burst-depth2` .. `miss-burst-depth17` | Relative weights for distinct cold target lines in one ordinary-load burst. Depths 2..16 hold every response concurrently. Depth 17 is the generated 16-entry DCache capacity plus one: it first reaches 16 externally pending responses, releases one fair response, then requires every target request and architectural operation to finish |
| `miss-burst-issue-width1` .. `miss-burst-issue-width3` | Relative weights for the same-cycle scalar-load issue width at the head of a burst; widths larger than the composition's scalar-load count are excluded |
| `miss-burst-scalar-only`, `miss-burst-scalar-vector-load` | Relative composition weights. The mixed class replaces one scalar line with one legal two-flow unit-stride vector-load uop while preserving the selected total target-line depth |
| `bank-conflict` | Per-mille share of clean scalar-load actions replaced by a two/three-way resident same-bank issue wave; the address bank, depth, translation regime, and line identity remain randomized |
| `locality-hot` | Ordinary addresses selected from a 32-line hot set; each explicitly prewarmed `bank-conflict` target wave also contributes one resident/hot observation |
| `locality-warm` | Lines selected from a 512-line warm set |
| `locality-cold` | Permutation of an 8192-line cold set |
| `translation-bare`, `translation-stage1`, `translation-nested` | Relative Bare, host stage-1, and nested VS+G context weights |
| `stage1-sv39`, `stage1-sv48` | Relative host stage-1 mode weights |
| `vs-sv39`, `vs-sv48` | Relative VS-stage mode weights in nested contexts |
| `g-sv39x4`, `g-sv48x4` | Relative G-stage mode weights in nested contexts and the independent G-only boundary subclass |
| `translation-stage1-napot` | Per-mille share of eligible host stage-1 accesses placed in a legal 64-KiB Svnapot region |
| `translation-vs-napot`, `translation-g-napot` | Independent per-mille Svnapot selection for the VS and final G leaf of eligible nested accesses; their product generates all four leaf topologies |
| `translation-switch` | Per-mille chance of choosing a new translation context at a drained action/window boundary |
| `fence-sfence`, `fence-hfence-vvma`, `fence-hfence-gvma` | Relative weights for fence kinds compatible with the active context |
| `fence-global`, `fence-selective` | Relative global/selective scope weights for generated translation fences |
| `concurrent` | Per-mille share of the tail reserved for heterogeneous overlap windows |
| `special-concurrent` | Per-mille chance that a legal overlap window also contains an NC or MMIO load |
| `tlb-flush` | Per-mille chance of a legal translation flush before an operation |
| `misaligned` | Per-mille chance of a misaligned address when width permits it |
| `canonical-boundary` | Per-mille share of eligible scalar or singleton vector-store actions replaced by a Sv39/Sv48 canonical crossing; requires stage-1 traffic and a reachable scalar/vector subclass. Zero disables all such actions. Nonzero closes enabled 24 scalar and 36 vector bins before weighted sampling; vector eligibility intersects addressing, EEW=SEW, LMUL=EMUL=1 and policy gates below. Distances, lanes, data, index offsets, stride signs, and scalar issue order remain randomized. Presets: SPEC 1, coverage 100, corner 250 |
| `gstage-boundary` | Per-mille share of eligible scalar/singleton vector-store actions replaced by G-only upper-limit crossings. Requires a reachable scalar/vector subclass and G-mode weight; independent of ordinary translation-regime and VS-mode weights. Closes 12 scalar and 18 vector bins before weighted sampling. Presets: SPEC 1, coverage 100, corner 250 |
| `vector-corner` | Per-mille chance of corner-biased vector shape/address generation |
| `vector-fof` | Low-rate per-mille share of ordinary vector-load actions replaced by unit-stride VLEFF. A nonzero value requires vector loads and stage-1 translation; 1000 is rejected because the profile must retain ordinary vector-shape closure |
| `vector-fof-first-fault` | Conditional per-mille share of VFOF actions whose first element faults. The other class places element zero on a mapped page and element one on the next unmapped page |
| `vector-masked` | Per-mille share of ordinary vector instructions with `vm=0`; generated masked instructions contain an observable mask-off body element |
| `vector-vma`, `vector-vta` | Per-mille share of ordinary vector instructions with mask/tail agnostic policy enabled |
| `vector-partial-vl` | Per-mille share of ordinary vector instructions with `vl < VLMAX` |
| `vector-nonzero-vstart` | Per-mille share of ordinary vector instructions with `vstart != 0` |
| `vector-unit-stride`, `vector-strided`, `vector-indexed-unordered`, `vector-indexed-ordered` | Relative ordinary vector addressing-mode weights shared by loads and stores |
| `vector-eew8` .. `vector-eew64`, `vector-sew8` .. `vector-sew64` | Relative ordinary vector index/memory EEW and data SEW weights |
| `vector-lmul-mf8` .. `vector-lmul-m8`, `vector-emul-mf8` .. `vector-emul-m8` | Relative ordinary vector LMUL and derived EMUL weights. Only legal `EMUL = EEW - SEW + LMUL` shapes are generated |
| `vector-segment-store` | Per-mille store share within the vector-segment class |
| `vector-segment-high-va` | Per-mille share of ordinary non-FOF segment actions using a legal high-half canonical alias under Sv39/Sv48. Requires stage-1 traffic and a compatible shape/direction/alignment. Zero disables the subclass; nonzero closes enabled mode x load/store x aligned/misaligned bins, then enables weighted selection after ordinary constraint coverage closes. SPEC 1, coverage 100, corner 250 |
| `vector-segment-unit-stride`, `vector-segment-strided`, `vector-segment-indexed-unordered`, `vector-segment-indexed-ordered` | Relative segment addressing-mode weights |
| `vector-segment-eew8` .. `vector-segment-eew64`, `vector-segment-sew8` .. `vector-segment-sew64` | Relative segment index/memory EEW and data SEW weights |
| `vector-segment-lmul-mf8` .. `vector-segment-lmul-m8`, `vector-segment-emul-mf8` .. `vector-segment-emul-m8` | Relative fractional/integer LMUL and derived EMUL weights. A shape is enabled only when `EMUL = EEW - SEW + LMUL` and both selected weights are nonzero |
| `vector-segment-nf2` .. `vector-segment-nf8` | Relative segment field-count weights |
| `vector-segment-fof` | Per-mille share of vector-segment load actions replaced by unit-stride fault-only-first loads. The legal shape space has all 338 decoder-legal EEW/SEW/LMUL/derived-EMUL/NF shapes; there is no indexed or strided Segment FOF encoding |
| `vector-segment-fof-first-fault` | Conditional per-mille share of segment FOF actions whose first element faults. The other class maps the first element and leaves the next segment element page unmapped |
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

## Validation and Extension Contract

Invalid names, all-zero operation/locality or enabled sub-dimensions, an unreachable enabled class,
incompatible fixed vector shape/policy settings, out-of-range per-mille values, inconsistent manager
concurrency/latency, or an enabled error/fault subclass with no legal site or governing translation
mode fails before traffic begins. In particular, NC/MMIO require stage-1 or nested PBMT translation
and cannot use a Bare-only context.

- Hypervisor traffic requires nested translation. Non-PMA hypervisor PBMT and fixed-PMA device
  traffic are aligned-only. Fixed-PMA device traffic requires the PMA/PMA PBMT pair and cannot
  enable a non-PMA pair at a device-only endpoint; PMP edge relations require the no-PMP PMA/PMA control. A
  hypervisor-only mix cannot claim Bare or host stage-1 coverage.
- `stride-stream` requires nonzero scalar-load and cold-locality weights. `vector-fof` requires
  vector loads, stage-1 translation, and a nonzero ordinary vector-load share. Segment FOF requires
  segment loads, stage-1 translation, a legal unit-stride shape, and a nonzero ordinary segment-load share.
- A nonzero segment high-VA rate requires a reachable ordinary stage-1 segment shape, direction, and
  alignment. Non-FOF loads require `vector-segment-fof != 1000`; misaligned classes require data EEW
  above eight bits.
- `dcache-load-error=1000` requires `stride-stream=0` and `concurrent=0`. `uncache-error=1000`
  requires `special-concurrent=0`. Endpoint error or vector-policy settings must leave every other
  enabled compatibility class reachable.

`random-mixed` requires at least 6144 actions so the mandatory architectural
prefix, replacement windows, CMO/atomic Probe bins, miss-burst and Segment FOF
crosses, and overlap traffic can close together. `--allow-short-mixed` is a
debug-only relaxation for a narrowed constraint set; all online and offline
coverage gates remain active, and regression controllers do not use it.

The constraint interface, not the CLI wrapper, is the verification abstraction.
A new direction must be a field on `RandomConstraints` and must use the common
transaction builders, scheduler, reference model, scoreboards, and final drain.
Every new field must provide:

- range and reachability validation before the first request;
- its resolved target in the replayable terminal summary and artifact;
- an observed class counter distinct from neighboring classes;
- a per-seed coverage obligation whenever it enables a class;
- deterministic replay from the seed and complete constraint assignment.

Presets only assign defaults; they must not add hidden phases or select another
scenario. `random-stress` remains a compatibility scenario for historical
artifacts and is not the template for new work.

## Shipped Presets

Operation columns are relative weights. Locality is `hot/warm/cold`; remaining
numeric rate columns are per-mille values.

| Preset | Scalar L/S | Vector L/S | VSegment | Prefetch | Atomic | NC | MMIO | Hypervisor | CMO | Locality | Concurrent | TLB flush | Misaligned | Vector corner | Probe/overlap | Stride stream | Bank conflict | Latency |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- | ---: | ---: | ---: | ---: | --- | ---: | ---: | --- |
| `coverage` | 200/150 | 150/150 | 75 | 100 | 100 | 75 | 75 | 75 | 75 | 250/250/500 | 1000 | 50 | 500 | 1000 | 20/500 | 500 | 500 | compact |
| `spec` | 648/270 | 90/45 | 20 | 35 | 5 | 5 | 5 | 1 | 1 | 800/150/50 | 100 | 20 | 5 | 100 | 1/10 | 100 | 250 | spec |
| `corner` | 125/125 | 125/125 | 125 | 125 | 125 | 125 | 125 | 125 | 125 | 100/200/700 | 500 | 100 | 500 | 1000 | 100/750 | 750 | 750 | spec |

Dimension-specific defaults:

| Dimension | `coverage` | `spec` | `corner` |
| --- | --- | --- | --- |
| PTW error / load merge / set pressure / miss burst operation weights | 75/100/75/100 | 0/10/2/15 | 125/125/125/125 |
| Atomic family AMO/LRSC/CAS; W/D | 8/2/2; 1/1 | 90/5/5; 1/1 | 1/1/1; 1/1 |
| Atomic error; denied share | 100; 500 | 0; 500 | 500; 500 |
| Atomic Probe-depth0..8 weights | all 1 | 1000/10/5/2/1/1/1/1/1 | 1/1/1/1/2/4/8/16/32 |
| Hypervisor HLV/HLVX/HSV | 1/1/1 | 90/5/5 | 1/1/1 |
| Hypervisor SPVP=U; fixed-PMA device share | 500; 500 | 1; 1 | 500; 500 |
| Hypervisor PBMT pair weights | all 1 | 996 + 1/1/1/1 | all 1 |
| Hypervisor PMP relation weights | all 1 | 999994 + 1 x6 | all 1 |
| CMO CLEAN/FLUSH/INVAL | 1/1/1 | 1/1/1 | 1/1/1 |
| CMO dirty / younger overlap / error / denied | 500/500/100/500 | 50/10/0/500 | 500/750/500/500 |
| CMO Probe-depth1..8 weights | all 1 | 1000/10/5/2/1/1/1/1 | 1/1/1/2/4/8/16/32 |
| PTW error site/level weights; store/denied/first-corrupt | all 1; 500/500/500 | all 1; 500/500/500 | all 1; 500/500/500 |
| Load-merge depth; same-address/same-beat/cross-beat | 1/1; 1/1/1 | 19/1; 1/8/1 | 1/1; 1/1/1 |
| Set-pressure depth; width; set quarter; windows | 1/1; all 1; all 1; all 1 | 9/1; 1/2/6/20; all 1; 1000/10/2/1/1/1/1/1 | 1/1; all 1; all 1; 1/1/1/2/4/8/16/32 |
| Set-pressure dirty / refill overlap / C backpressure | 500/500/500 | 50/10/10 | 500/750/750 |
| DCache-load / Uncache error; denied share | 100/100; 500/500 | 0/0; 500/500 | 500/500; 500/500 |
| Vector-segment store; ordinary vector dimensions | 500; uniform | 300; weighted below | 500; uniform |
| Ordinary VFOF / segment FOF; first-fault share | 100/100; 500/500 | 10/10; 100/100 | 250/250; 500/500 |
| Vector masking / partial VL / nonzero vstart | 500/500/500 | 100/100/20 | 500/500/500 |
| Vector VMA / VTA | 500/500 | 500/500 | 500/500 |
| NC and MMIO store; special-concurrent | 500/500; 500 | 300/300; 20 | 500/500; 750 |
| Probe; toB; need-data; overlap; triple overlap | 20; 500; 500; 500; 500 | 1; 500; 500; 10; 10 | 100; 500; 500; 750; 750 |
| Probe-depth3..8 weights | all 1 | 32/16/8/4/2/1 | all 1 |
| Stride stream / bank conflict | 500/500 | 100/250 | 750/750 |
| Canonical / G-stage / segment high-VA | 100/100/100 | 1/1/1 | 250/250/250 |

The `spec` vector shape weights favor unit stride (`980/10/5/5`), data SEW
32-bit (`1/2/5/2`), and M1/M2 LMUL/EMUL (`1/1/2/16/8/2/1`) while retaining a
nonzero floor for every legal class. Its segment NF weights are
`32/16/8/4/2/1/1`. `coverage` and `corner` use uniform vector and segment
dimensions. Per-seed deficit scheduling temporarily prioritizes uncovered
legal classes, then returns to steady-state weights.

Translation presets use these relative weights and switch rates:

| Preset | Bare/stage-1/nested | Stage-1 Sv39/Sv48 | VS Sv39/Sv48 | G Sv39x4/Sv48x4 | Stage-1/VS/G NAPOT | SFENCE/VVMA/GVMA | Global/selective | Switch |
| --- | --- | --- | --- | --- | --- | --- | --- | ---: |
| `coverage` | 1/1/1 | 1/1 | 1/1 | 1/1 | 500/500/500 | 1/1/1 | 1/1 | 500 |
| `spec` | 5/990/5 | 95/5 | 1/1 | 1/1 | 1/1/1 | 98/1/1 | 95/5 | 1 |
| `corner` | 1/1/1 | 1/1 | 1/1 | 1/1 | 500/500/500 | 1/1/1 | 1/1 | 750 |

The per-seed gate overrides sampling order only until every enabled mode,
nested pair, ordinary/NAPOT leaf topology, and compatible fence kind/scope has
appeared. It then reselects the highest-weight enabled context once so a rare
mandatory mode is not retained for a full switch interval; later switches use
the configured distribution.

`coverage` is intended for short pre-submit checks, `spec` for extended or final
campaigns, and `corner` for boundary hunts. A campaign may sweep several
constraint sets, but every run uses the same generator and records its resolved
targets.

## Calibration Conclusions

The `spec` preset is calibrated from generated workload counters, but those
counters are never correctness oracles. The reproducible analysis entry is:

```sh
make analyze-spec-counters \
  SPEC_COUNTER_ROOT=/path/to/checkpoints \
  SPEC_COUNTER_JSON=../../build/memblock/spec-counters.json
```

The analyzer counts only the latest timestamp that reaches the stable generated
terminator, avoiding cumulative-dump double counting and truncated final
blocks. Each future dataset must record its own discovered and accepted counts;
there is no frozen corpus-size constant.

Calibration conclusions are:

- Scalar memory is load-dominant, while MMIO and sampled NC traffic are rare. `spec` therefore keeps
  scalar traffic highest, NC/MMIO at a verification floor, and synthetic CMO/DCache/Uncache errors disabled.
- Vector issue traffic, resident-bank replay, multi-enqueue misses, replacement, and Probe traffic are
  frequent enough to justify dedicated dimensions and nonzero `spec` floors. Queue-level issue
  attempts are not distinct uops or architectural instructions, and do not provide a segment-only frequency.
- The memory-bound mcf subset is a useful high-miss reference but is not pooled at equal weight with
  broad SPEC-like data.
- The calibrated first-beat DCache/PTW/Uncache latency model is approximately 74.1% below 20 cycles,
  14.4% at 20-39, 5.1% at 40-99, and 6.4% at 100-400. Histogram boundaries overlap, so this is a test
  model, not an exact performance-model claim. `spec` deterministically covers all four classes before
  later responses follow the distribution statistically.

These observations justify stimulus priorities only. They are not converted
directly into transaction probabilities or expected DUT results.

## Coverage And Replay Contract

Every terminal line prints `constraint_schema=53`, resolved targets, and actual
counts for operations, atomic/Probe/hypervisor/CMO/DCache state, ordinary and
segment vector shapes, FOF, NC/MMIO, translation and leaf topology, fences,
cache events, all load wakeup/cancel lanes, prefetch diagnostics, and manager
latencies. The simulator and `verify_regression.py` enforce the same gates per seed.

Optional `MEMBLOCK_MIXED_PROGRESS_ACTIONS=N` diagnostics flush seed, action
count, simulation cycle, and last phase to stderr at mixed-loop boundaries.
They consume no random draws and advance no DUT cycles. Checkpoints are not
terminal summaries, coverage evidence, or acceptance; N=0/unset disables them.

### General Accounting

- Every enabled architectural or stimulus-coverage class must occur; a zero
  target enables no observed class, a 0/1000 binary endpoint excludes the
  opposite class, and an intermediate target requires both classes.
- Class marginals, totals, and accounting tuples reconstruct source counts
  exactly; disabled bins remain zero. These are per-seed gates unless a field
  explicitly requires campaign-level distribution analysis.
- Functional PASS uses independent data, exception, identity, cancellation,
  queue, and protocol oracles. Internal bank selection, arbitration, replay
  count, MSHR/FSM state, cycle timing, prefetch causality, and feedback pulses
  are diagnostic unless an external contract explicitly requires them.
- The mandatory prefix closes width/lane legality, translation pairs, fault
  metadata, dirty eviction, redirect, and queue accounting. The configurable
  tail determines workload direction and composes those mechanisms under pressure.

### Stimulus Closure

Ordinary vectors report five shape marginals and the 896-slot `actual_vector_cross`
direction x addressing x EEW x SEW x LMUL space. The 624 legal RVV classes are enabled by
configured weights; EMUL is derived as `EEW - SEW + LMUL`, while illegal relations and
policy-incompatible VLMAX classes remain zero. Each enabled class occurs, and every instruction
produces one through eight data uops, with every enabled multi-uop shape producing a multi-uop
instruction. Heterogeneous windows overlap scalar load/store, vector load/store, and prefetch;
`special-concurrent` adds NC/MMIO loads, and windows report all 16 reachable direction x addressing x
single/multi-uop classes. The five binary policy marginals report false/true counts and conserve against
`actual_vector_shape_ops`; a mask/tail endpoint requires an actual inactive body/tail element.

Ordinary VFOF reports the ordinary/fault split, later/first-fault position, stage-1-mode and EEW
marginals, and all 16 enabled fault-position x mode x EEW bins. A later fault returns the first
mapped element with VL=1 and no exception; a first-element fault preserves the load-page-fault,
fault VA, and VL=2. Both require one unique fix-VL writeback and no extra LQ allocation. Segment
FOF covers all 338 decoder-legal unit-stride shapes and all 1,352 later/first-fault x Sv39/Sv48 x
shape bins. A later segment fault requires the mapped element's exact bytes, suppresses the
page-fault writeback, and trims VL; a first-element fault preserves the original VL and requires
at least one identified page-fault writeback. Every segment action has exactly one fix-VL writeback
and its exact one-through-eight data-uop count; ordinary segment dimension marginals conserve after
subtracting FOF actions. Segment-field exception choice and faulting/remainder destinations remain unspecified.

Load merge reports 12 depth x address-pattern x critical-beat bins and three translation-regime
bins. Every enabled shape and regime occurs. A batch skips lines already observed at the manager
and uses a fresh line from a 64-MiB region; all members issue in one cycle, target requests and
global refill/GrantAck deltas conserve, and scalar writebacks equal the sum of batch depths.

Set pressure reports 1536 clean/dirty x overlap/no-overlap x C-ready/stalled x one-through-eight-window x depth x width x
translation crosses, four base-index quarters, and both store-half issue orders. Actions use fresh lines in one through
eight windows, each spanning two of the generated DCache's 256 four-way sets. Generated stores, target requests, store writebacks, and SQ dequeues match exactly. Dirty target
ReleaseData reaches at least `depth - 8`; attributed releases carry the stored bytes into manager memory. Clean actions
require exact initial and reverse-revisit data, one initial target request per line, at least `depth - 8` revisit misses
and target Releases, zero target ReleaseData, and exact load-writeback/LQ-dequeue conservation. Overlap holds one
independently addressed cold load and requires at least `depth - 8` target Releases while pending. The C-stall tuple is
`selected actions, stalled target releases, target stall cycles, payload-stability checks, completed windows` and is
exactly `1,1,16,16,1` for a selected action or `0,0,0,0,1` otherwise.
Clean overlap first allocates four older target loads per window without issuing them. It then establishes every
younger held response, issues the older targets, and requires their exact writebacks and observed LQ dequeues before
allocating the remaining targets. Thus all target accesses and required Releases occur while every associated held
load remains pending, without assuming that a completed baseline remains resident. At eight windows, initial LQ
occupancy is at most 40 and retained tail occupancy at most 56, below the 66-flow dispatch watermark. C backpressure
is armed before the first target issue. Transaction creation order and metadata RNG assignment differ from earlier
binaries; replay requires the matching frozen runtime. Schema 53 preserves the 1536 accepted replacement bins and adds ownership/construction accounting.
Dirty overlap allocates every older overflow store into SQ before dispatching the younger held loads, then issues
address/data and commits only after all holds are established. Baseline batches use at most 32 SQ entries and
preallocated overflow at most 16, below the 52-entry watermark. It retains the committed baseline and incremental
ReleaseData gate. Before the action, schema 53 drains baseline SBuffer/D/E, disables the L1 stride prefetcher through its external
CSR, runs 4 + 8 + 4 = 16 DUT cycles and drains DCache idle, establishes every held load, and requires exactly one
store-class full-line AcquireBlock NtoT per fresh baseline line, unchanged manager error counters, and zero
completed target Releases of either opcode; the current source drains the release channel before the ownership snapshot and after selected C backpressure, so an in-flight target ReleaseData is counted before qualification (normal seed 531/6,144, post-fix 90-leaf 90/90, and post-fix matrix 12/12 pass; mutation sensitivity still pending, review `independent-review-schema53-20261023`). The prior prefetch enable is restored after the action. No DUT cycle separates qualification from target
C-stall arming. Coherence responsibility can remain in a writeback queue; array residency is not a prerequisite.
A rejected attempt releases its held responses, issues each preallocated overflow once, commits in order with
independent reference updates, and drains exact WB/queues/SBuffer/D/E/C under every functional oracle. The
unqualified overlap/stall minima alone are inapplicable. Retries keep the selected dirty/overlap/depth/window/
width/translation/base-index-quarter/C-backpressure tuple and use fresh addresses/identities without rewinding RNG.
The cap is four attempts; exhaustion is `stimulus_construction_exhausted`, never PASS or a CPU-bug classification.
Rejected construction contributes actual store issue-order, cacheable traffic, bus/response latency, WB/SQ/load/LQ,
and cycle totals, plus separate rejected-manager totals; it does not increment the logical action or accepted bin.
Only qualified and fully completed attempts populate accepted bins. Release overlap means completed transaction
acceptance at the final C beat, not first-beat timing. Target Releases are attributed by line and opcode, so
background ProbeAck/Release traffic remains legal without satisfying the target oracle.

The ordinary miss-burst cross is 288 depth x issue-width x composition x translation bins. Depths
2 through 17, legal issue width, scalar-only or one scalar+vector-load composition, every enabled
translation regime, exact per-identity data, scalar writebacks, vector uops/writebacks, and LQ
flows conserve. A vector-load uop uses two LQ flows. Depth-17 actions must observe 16 externally
pending responses, release one response, and then request and complete all 17 target lines.
`actual_miss_burst_saturation` is exactly `reached,release-scheduled,overflow-requested` for every
depth-17 action. Target requests/refills can exceed target lines because hardware prefetch is
legal; global response/GrantAck pairing still conserves.

Bank conflict reports the 48 depth(2/3) x bank(0..7) x translation bins. Target lines are freshly
warmed and then issued in one wave. Every member has exact data, one scalar writeback, and one LQ
dequeue. The addresses are ordinary mapped 2-MiB traffic, not NAPOT placement. Wakeup, `ld2Cancel`,
arbitration, prefetch traffic, and replay classification remain diagnostic. Only the target wave
contributes a hit/resident-hot observation; setup fills are not action misses or locality claims.

Atomic clean/error actions cover all 18 enabled family x width x clean/corrupt/denied outcomes.
Errors require the exact exception, two-beat refill, errored GrantAck/refill, unchanged manager
memory at response time, and the checked poisoned-line image if released; LR/SC errors stop at LR
because a cold SC cannot request a refill. Successful atomic Probe bursts cover all 54 family x
width x depth bins with exact old value/result, target-refill attribution, accepted Probe depth,
address-qualified completion, zero auxiliary data, and manager conservation.

Hypervisor traffic closes every enabled family x SPVP x alignment x five PBMT pair x
DDR/fixed-device x seven PMP-relation class. Checks include final PA, exact bytes, DCache versus Uncache
route, fault and queue recovery, and post-hypervisor load readback. The PMP edge fixture is a
first-match 4-KiB NAPOT RWX region inside an enclosing 16-KiB deny region.

CMO closes operation x line state x younger-overlap x error presence and all 48 successful operation x clean/dirty x
depth bins. Success checks exact ProbeAckData and drains older SBuffer traffic first. Errors require the exact exception,
unchanged backing memory, redirect cleanup, and no manager Probe. Successful categories plus error counts reconstruct
`actual_cmo_operation`; depth-weighted accepted B requests and maximum accepted-but-unanswered depth match generation.

DCache scalar-load clean/corrupt/denied outcome bins are exact enabled-class gates. For `N` error actions containing `D`
denied actions, `error responses, denied beats, corrupt beats, errored GrantAcks, errored refills` equals `N,2D,2N,N,N`;
sink attribution keeps this invariant under a concurrent clean hardware prefetch. NC/MMIO errors require exact
response/D-beat accounting and distinct NC versus MMIO store contracts; error stores always use denied because
`AccessAck` carries no data.

PTW errors report 90 site x direction x level x outcome bins, 20 site-specific-mode bins, and 20 target-level bins.
Duplicate faulting-PTE requests are legal, so responses may exceed actions. Denied beats are even and cover at least
two beats per denied action; corrupt beats equal all response beats plus the additional denied beat. Each action
requires the precise access fault, no target DCache/Uncache request, a clean same-address retry, and a fresh reread of
the injected PTE block. PTW-only sets do not inherit ordinary translation-switch or TLB-flush gates. Coverage/corner
use fresh page-table roots and addresses so each fault reaches its selected walk level; `spec` disables synthetic PTW
errors while retaining nonzero sub-dimensions.

Manager Probe subclasses conserve against generated sequences. Probe depth, toB cleanup, CMO-derived Probes, atomic
auxiliary Probes, and `depth - 1` auxiliary clean Probes reconstruct manager traffic. The authoritative eight-field
`actual_probe_depth` conserves to sequences and projects exactly onto compatibility `actual_probe_overlap`.
`actual_probe_cross` closes all 32 depth x capacity x need-data bins and reconstructs its marginals. The B-source space
is 64: for `N` accepted Probes, lifecycle `unique,reuse,wrap` equals `min(N,64), N-min(N,64), 0 if N=0 else (N-1)/64`;
an active source ID cannot be reused. A Probe sequence first drains older SBuffer traffic, preventing an early legal
NtoN response from being misclassified as a dirty-line failure under the long-tail profile.

### Translation and Boundary Gates

Every enabled translation regime, stage-1/VS/G mode, nested VS-G pair, ordinary/NAPOT leaf
topology, and compatible fence kind/scope occurs. More than one enabled context requires an
observed switch; translated profiles require both a PTW walk and a translation-reuse window.
`prefetch.i` occurs at least once per seed. Under backpressure, each manager set to `spec`
independently observes all four latency classes. `stride-stream` does not require source-12: an
implementation may train, arbitrate, merge, suppress, or issue another legal stream.

Scalar canonical coverage is the 24-bin cross `((invalid_first * 2 + mode) * 2 + store) * 3 +
(eew - 1)`, with mode 0/1 = Sv39/Sv48, store 0/1 = load/store, and EEW 1/2/3 = 2/4/8 bytes.
Total = sum(bins), recoveries = total, controls = `2 * total + 2 * store_faults`, and store checks
= `store_faults`. Both aliases have permissive 4-KiB mappings; aligned controls prove them before
the fault. Each fault has the first independently walked invalid VA, exact page-fault completion,
observed redirect/queue conservation, clean edge readbacks, and unchanged manager memory. Legal
speculative first-page traffic is allowed. The exception address is sampled only after other
operations drain because it has no transaction identity.

Vector canonical coverage is the 36-bin cross `((invalid_first * 2 + mode) * 3 + addressing - 1) *
3 + (eew - 1)`, with addressing 1/2/3 = strided/indexed-unordered/indexed-ordered. The subclass
uses one active element, `vl=1`, `vstart=0`, unmasked, EEW=SEW, LMUL=EMUL=1, and reserves
`16 >> eew` SQ flows. Enabled bins require matching store, stage-1/mode/addressing/EEW/SEW/LMUL/
EMUL weights plus `vector-partial-vl>0`, `vector-nonzero-vstart<1000`, and `vector-masked<1000`;
VMA/VTA honor their endpoints. Each crossing requires an identified store-page fault, exact
first-invalid VA, four edge controls, unchanged manager memory, and SQ dequeue/cancellation of
every reserved flow. A raw VLS exception redirect handles cancellation; one committed scalar
store/load pair in the same DUT proves recovery. Total, controls, recoveries, store checks, and
flows conserve with the four-controls formula and `16 >> eew` multiplier. Ordinary vector stores
plus canonical and G-only vector faults equal the vector-store action count.

G-only coverage uses VS=Bare, Sv39x4/Sv48x4, and 41/50-bit zero-extended GPA limits. Scalar has 12
bins `(mode * 2 + store) * 3 + eew - 1`; vector has 18 bins
`(mode * 3 + addressing - 1) * 3 + eew - 1`. Ordinary translation-regime and VS-mode weights do
not restrict this subclass, but a reachable scalar/vector class and G-mode weight are required.
Every access byte is independently walked; the fault is the access-specific guest-page fault with
exact first-invalid VA/GPA at the limit and VS-non-leaf-PTE false. Separate G roots and two actual
loads prove legal permissive mappings; two unchanged-edge loads and a committed survivor prove
side-effect suppression and recovery. Legal speculative prefix traffic is allowed. Per fault,
controls/recovery/store checks conserve at four/one/one, vector flows use `16 >> eew`, and the VLS
qualifier is present. G-only activation drains prior work, flushes pending fences, invalidates the
ordinary context, and issues HFENCE.GVMA; the next ordinary action reenters its own context.

Canonical and G-only prefixes are finite and run before sampled replacement. At rate 1000 ordinary
coverage still closes first. After ordinary and Probe requirements close, both subclasses sample
independently; a collision resolves by random equal choice, so both remain reachable even at rate
1000. Their fault actions do not populate ordinary DCache outcomes, successful vector shape/policy
counters, or ordinary leaf-topology accounting. Scalar ordinary plus canonical/G-only scalar faults
conserve against scalar operation counts and are excluded from DCache outcomes.

Segment high-VA coverage has 24 bins. Eight stage-1 bins use
`(stage1_mode * 2 + store) * 2 + misaligned`; sixteen nested bins use all
four VS/G pairs with the same `(store, misaligned)` dimensions, indexed from
eight. A bin requires nonzero `vector-segment-high-va`, segment operation, the
selected stage-1 or nested translation traffic, a reachable
shape/direction/alignment, and enabled segment-store/load weights. Non-FOF
loads require `vector-segment-fof != 1000`; misaligned bins require data EEW
above eight bits, using SEW for indexed addressing. A finite missing-bin prefix
contains at most 24 instructions, and weighted replacement starts only after
ordinary coverage closes, preserving ordinary shape/regime/leaf reachability
even at target 1000. Each action uses the randomized ordinary physical address
through a full-XLEN high-half alias and its own 4-KiB mappings; all active
effective VA bytes are independently walked through the selected stage-1 or
two-stage page table to the declared physical bytes. Shape, NF, stride/index
values, data, partial VL, vstart, and mask remain randomized, and at least one
field element is active. Exact vector completion identity/data and observed
completion counts are checked; stores are followed by actual DUT loads of every
active field element. The operation count equals the bin total;
direction/stage-1/nested-pair accounting cannot exceed matching ordinary
translation traffic; data uops are 2..8 times actions and active elements are
2..128 times actions. These are successful ordinary segment instructions, not
a new operation or fault subclass; with NAPOT-only ordinary traffic they are
excluded from ordinary leaf-topology accounting while shape, direction, and
FOF accounting retain their meaning.

### Reporter Identifiers

These names are part of the replayable coverage contract; `verify_regression.py`
checks them from the terminal summary:

| Identifier | Required interpretation |
| --- | --- |
| `actual_vector_shape_ops` | Total ordinary vector instructions used to conserve shape and policy marginals |
| `actual_vector_cross` | 896-slot direction x addressing x EEW x SEW x LMUL cross; the configured 624 legal RVV classes must close |
| `actual_vector_agnostic` | Observed mask/tail agnostic classes; an enabled endpoint requires the corresponding inactive body or tail element |
| `actual_concurrent_vector_shape` | 16 reachable heterogeneous direction x addressing x single/multi-uop classes |
| `actual_concurrent_vector_uops` | Load/store uop conservation inside heterogeneous windows |
| `target_miss_burst_composition` | Requested scalar-only versus scalar+vector-load miss-burst composition |
| `target_miss_burst_depth` | Requested depth class for ordinary miss bursts |
| `target_miss_burst_issue_width` | Requested initial scalar issue width |
| `actual_miss_burst_max_outstanding` | Externally observed maximum held target depth, used as a fair-progress sanity floor |
| `actual_miss_burst_saturation` | `reached,release-scheduled,overflow-requested` count for depth-17 actions |
| `actual_cmo_operation` | CMO operation marginal used to reconstruct the enabled operation/error crosses |
| `target_cmo_probe_depth` | Requested CMO-derived Probe depth |
| `actual_cmo_probe_depth` | Observed CMO-derived accepted Probe depth marginals |
| `actual_cmo_probe_cross` | 48 operation x clean/dirty x depth bins |
| `target_set_pressure_window` | Requested one-through-eight replacement-window class |
| `actual_set_pressure_window_count` | Authoritative accepted replacement-window distribution |
| `actual_set_pressure_dual_window` | Two-field compatibility tuple `[non-dual, dual]`; the fields sum to all set-pressure actions and project exactly from the authoritative window distribution |
| `actual_set_pressure_ownership` | Schema 53: attempts, qualified, accepted, rejected, cleaned, acquisition-reason, error-reason, Release-reason, in-flight. Reasons may overlap; normal termination requires attempts=accepted+rejected, qualified=accepted, rejected=cleaned, in-flight=0, and accepted equal to dirty-overlap cross actions |
| `actual_set_pressure_rejected_manager` | Schema 53: rejected attempts, stores, baseline stores, overflow stores, held loads, target A, held A, store WB, SQ dequeues, load WB, LQ dequeues, target C, target ReleaseData, global ReleaseData, verified global ReleaseData, expected target ReleaseData, preserved target stores, all A. Each target needs at least one accepted covering A; extra acquisitions are legal on rejection. Target ReleaseData counts transactions, while expected immutable images/preserved stores count distinct lines; WB/SQ and held-load/WB/LQ remain exact |
| `actual_set_pressure_rejected_issue_order` | Schema 53: rejected STA-first/SDA-first store totals; sum equals rejected stores |
| `target_bank_conflict` | Requested resident two-/three-way bank-conflict class |
| `actual_bank_conflict_terminal` | Four-value tuple `actions, accepted loads, writebacks, LQ dequeues`; all members conserve and internal replay/arbitration observations remain diagnostic |
| `actual_probe_depth` | Authoritative accepted-but-unanswered Probe depth distribution |
| `actual_probe_overlap` | Compatibility overlap counts projected from `actual_probe_depth` |
| `actual_probe_cross` | Enabled depth x cap x need-data Probe bins |
| `actual_canonical_boundary` | Scalar canonical-fault 24-bin total and marginal source |
| `actual_vector_canonical_boundary` | Singleton vector canonical-fault 36-bin total |
| `actual_vector_canonical_boundary_flows` | Queue-flow count weighted by `16 >> eew` |
| `actual_gstage_boundary` | G-only scalar upper-limit 12-bin total |
| `actual_vector_gstage_boundary` | G-only singleton vector upper-limit 18-bin total |
| `actual_vector_segment_high_va_cross` | Twenty-four high-half segment bins: eight stage-1 mode x direction x alignment bins followed by all four nested VS/G pairs x direction x alignment |

### Legal Driver and Replay Contracts

The harness must model upstream scheduling needed for legal stimulus. Scalar
stores reissue when the DUT reports replay. A vector load whose active element
crosses a 16-byte boundary advances to the modeled ROB head before issue,
matching `LoadMisalignBuffer` eligibility; failure to drive that state is a UT
deadlock, not RTL failure. Atomic operations are serializing and absent from
overlap windows because LR/SC/AMO blocks the pipeline and redirects load-unit-0
control while active. Translated MMIO stores reissue from observed store-TLB
miss feedback until a hit is observed before the ROB-head pulse. NC/MMIO actions
are never emitted in Bare because only PBMT supplies those attributes here.
Misaligned vector stores drive ROB-head/pending state and replay; concurrent
vector feedback is matched to the originating load/store by queue identity. SQ
retirement uses the enqueue-time target, preventing double counting when a
writeback retires before the helper's explicit commit step; scalar stores use
the same target. Every LQ/SQ identity derives its modulo value and wrap flag from
the same absolute cursor, an input-legality invariant checked by unit tests and
not used as an internal RTL or architectural-data oracle.

### Explicit Gaps

The current contract does not claim malformed, duplicate, or unsolicited
manager responses; replacement composed with Probe/CMO/atomic traffic; multiple
simultaneous CMO sources; cross-hart atomic reservation; active segment
redirect; full vector/atomic same-bank crossing; cross-operation miss-burst
composition; or an independent starvation monitor/general simulator
cross-coverage database. Dependency-aware overlap-window shapes, the full
624-shape ordinary-vector cross against every concurrent producer, broader
ordinary VFOF producer interactions, and concurrent special stores are not yet
closed. Hardware-prefetch causality and positive L3-enabled integration remain
characterization/configuration gaps.

Boundary gaps include multi-active-element faults, vector-load and unit-stride
canonical crossings, additional LMUL/EMUL/SEW shapes, nested boundary
composition, GPA wrap, high-half FOF segment traffic, noncanonical segment
faults, and broader partial-fault progress. Hypervisor gaps include
non-PMA PBMT with misalignment and additional fixed-PMA/PMP sizes, TOR, lock,
permission, and overlap-by-edge crosses. Do not infer these gaps from aggregate
event counts; when a reachable known class cannot be reached, stop extending
runtime and review trigger probability, per-seed closure, and oracle strength.
