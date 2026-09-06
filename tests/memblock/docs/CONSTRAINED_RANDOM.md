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
| `vector-load`, `vector-store` | Relative vector memory weights |
| `prefetch`, `atomic`, `nc`, `mmio` | Relative special-operation weights |
| `atomic-amo`, `atomic-lrsc`, `atomic-cas` | Relative atomic-family weights inside the `atomic` class |
| `atomic-w`, `atomic-d` | Relative W/D atomic-width weights |
| `locality-hot` | Lines selected from a 32-line hot set |
| `locality-warm` | Lines selected from a 512-line warm set |
| `locality-cold` | Permutation of an 8192-line cold set |
| `translation-bare`, `translation-stage1`, `translation-nested` | Relative Bare, host stage-1, and nested VS+G context weights |
| `stage1-sv39`, `stage1-sv48` | Relative host stage-1 mode weights |
| `vs-sv39`, `vs-sv48` | Relative VS-stage mode weights in nested contexts |
| `g-sv39x4`, `g-sv48x4` | Relative G-stage mode weights in nested contexts |
| `translation-switch` | Per-mille chance of choosing a new translation context at a drained action/window boundary |
| `fence-sfence`, `fence-hfence-vvma`, `fence-hfence-gvma` | Relative weights for fence kinds compatible with the active context |
| `fence-global`, `fence-selective` | Relative global/selective scope weights for generated translation fences |
| `concurrent` | Per-mille share of the tail reserved for heterogeneous overlap windows |
| `special-concurrent` | Per-mille chance that a legal overlap window also contains an NC or MMIO load |
| `tlb-flush` | Per-mille chance of a legal translation flush before an operation |
| `misaligned` | Per-mille chance of a misaligned address when width permits it |
| `vector-corner` | Per-mille chance of corner-biased vector shape/address generation |
| `probe` | Per-mille chance that a completed cacheable scalar store is followed by a manager-originated dirty Probe sequence |
| `probe-to-b` | Per-mille share of generated Probe sequences that retain the line in Branch state; the generator follows each with a toN cleanup Probe |
| `probe-need-data` | Per-mille share of generated Probe sequences that explicitly request data; dirty lines must return exact data even when this is zero |
| `nc-store`, `mmio-store` | Per-mille store share within each memory-type class |
| `latency` | Set DCache, PTW, and Uncache to `compact` or `spec` together |
| `dcache-latency`, `ptw-latency`, `uncache-latency` | Override one manager's latency profile independently |

Invalid names, all-zero operation/locality or enabled atomic-family/width
weights, out-of-range per-mille values, inconsistent special-concurrency or
manager-latency settings, and unknown latency profiles fail before simulation
traffic begins. The harness has no programmable PMA region at this boundary,
so randomized NC and MMIO traffic requires stage-1 or nested PBMT translation.
An NC/MMIO-only operation mix cannot also request Bare coverage.
`random-mixed` requires at least 256 actions so the mandatory architectural
prefix, four overlap windows, and each enabled constrained class can coexist.

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
| Atomic subtype | `atomic-amo`, `atomic-lrsc`, `atomic-cas`, `atomic-w`, and `atomic-d` select legal AMO, LR/SC, and compare-dependent AMOCAS sequences | Cross-hart reservation interference remains integration-level |
| NC/MMIO direction | `nc-store` and `mmio-store` steer load/store direction and each direction has an independent coverage gate | Concurrent special stores remain deferred until multi-store ROB/commit scheduling is modeled |
| Translation state | Bare/Sv39/Sv48 and all four Sv39/Sv48 x Sv39x4/Sv48x4 pairs are weighted tail contexts; switches occur only at drained boundaries; cold walk/reuse and the legal fence kind/scope matrix are per-seed gates | Distinct-page walks and redirected root/ASID/VMID/MODE/`V` changes with delayed PTW responses are covered by directed matrices; random context changes remain restricted to drained boundaries |
| Response latency | `latency` sets all managers; `dcache-latency`, `ptw-latency`, and `uncache-latency` override them independently, with separate observed histograms and gates | Add finer numeric/distribution controls only when a calibrated workload needs them |
| Cache Probe | `probe`, `probe-to-b`, and `probe-need-data` generate manager Probes after randomized dirty scalar stores, check exact 64-byte ProbeAckData, cover toB/toN and requested/mandatory data, and invalidate retained toB lines with a checked cleanup Probe | Overlap Probes with unrelated misses/refills and support multiple outstanding Probe sources |
| Error injection | Errors are confined to focused deterministic contracts | Add a normally-zero or very-low random error rate with independently checked denied/corrupt outcomes; realistic presets must keep this rare |

The remaining rows do not change the architecture: `coverage`, `spec`, and
`corner` are settings of the same generator. Closing them means lifting each
choice into the common interface and its coverage contract, not adding
`random-spec`, `random-corner`, or feature-specific random scenario functions.

## Shipped Presets

Operation columns are relative weights. Locality is `hot/warm/cold`; the next
five columns are per-mille values.

| Preset | Scalar L/S | Vector L/S | Prefetch | Atomic | NC | MMIO | Locality | Concurrent | TLB flush | Misaligned | Vector corner | Probe | Latency |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | ---: | ---: | ---: | ---: | ---: | --- |
| `coverage` | 200/150 | 150/150 | 100 | 100 | 75 | 75 | 250/250/500 | 1000 | 50 | 500 | 1000 | 20 | compact |
| `spec` | 650/270 | 20/10 | 35 | 5 | 5 | 5 | 800/150/50 | 100 | 20 | 5 | 100 | 1 | spec |
| `corner` | 125/125 | 125/125 | 125 | 125 | 125 | 125 | 100/200/700 | 500 | 100 | 500 | 1000 | 100 | spec |

Atomic family weights (AMO/LRSC/CAS) are `8/2/2`, `90/5/5`, and `1/1/1` for
`coverage`, `spec`, and `corner`; all three use `1/1` W/D weights. Their
NC/MMIO store shares are respectively `500/500`, `300/300`, and `500/500`.
Their legal NC/MMIO overlap rates are `500`, `20`, and `750` per mille.
All three presets split generated Probes equally between toB/toN and explicit
need-data/no-need-data requests. Since the candidate line is dirty, both
need-data values require exact ProbeAckData; the bit tests the protocol rule,
not whether the oracle checks returned bytes. SQ retirement only transfers a
committed store into SBuffer, so the Probe sequence first allows half of the
bounded manager-completion window for older SBuffer traffic to drain. This
prevents a legal early NtoN response from being misclassified as a dirty-line
failure under the long-tail latency profile.
`spec` and `corner` use the calibrated long-tail profile independently on all
three managers; `coverage` uses compact latency.

Translation presets use these relative weights and per-mille switch rates:

| Preset | Bare/stage-1/nested | Stage-1 Sv39/Sv48 | VS Sv39/Sv48 | G Sv39x4/Sv48x4 | SFENCE/VVMA/GVMA | Global/selective | Switch |
| --- | --- | --- | --- | --- | --- | --- | ---: |
| `coverage` | 1/1/1 | 1/1 | 1/1 | 1/1 | 1/1/1 | 1/1 | 500 |
| `spec` | 5/990/5 | 95/5 | 1/1 | 1/1 | 98/1/1 | 95/5 | 1 |
| `corner` | 1/1/1 | 1/1 | 1/1 | 1/1 | 1/1/1 | 1/1 | 750 |

The mandatory per-seed gate overrides sampling order only until every enabled
mode, nested pair, and compatible fence kind/scope has appeared. Later choices
follow the configured weights. At that boundary the generator immediately
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
being made artificially common in `spec`. Error injection remains in focused
error contracts and corner campaigns, not ordinary SPEC-like traffic.

The calibrated first-beat DCache/PTW/Uncache response latency distribution is
approximately 74.1% below 20 cycles, 14.4% at 20-39, 5.1% at 40-99, and 6.4%
at 100-400 cycles. The original histogram counters overlap at some boundaries,
so these probabilities are a test model rather than an exact performance-model
claim. The first four `spec` responses deterministically cover one sample in
each latency class; later responses follow the distribution statistically.

## Coverage And Replay Contract

Every terminal line prints `constraint_schema=3`, the resolved target weights,
and actual operation, atomic family/width, NC/MMIO direction, legal special
overlap, locality, translation regime/mode/pair, fence kind/scope, cold-walk/
reuse, TLB-flush, hit/miss, Probe sequence/cap/need-data, and per-manager
latency counts. Each enabled class must be observed at least once. Probe counts
must also conserve sequences and their toB cleanup requests. More than one
enabled translation context also
requires an observed switch, and any translated profile requires both a PTW
walk window and a reuse window. With backpressure, each manager set to `spec`
must independently observe all four latency classes. The simulator and offline
artifact verifier both enforce these obligations. They are minimum gates;
distribution quality is evaluated over long multi-seed campaigns from the
recorded counts.

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
