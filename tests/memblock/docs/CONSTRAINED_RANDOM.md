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
  CONSTRAINT='tlb-flush=100 mmio=30 nc=20 vector-load=80 vector-store=40'

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
| `locality-hot` | Lines selected from a 32-line hot set |
| `locality-warm` | Lines selected from a 512-line warm set |
| `locality-cold` | Permutation of an 8192-line cold set |
| `concurrent` | Per-mille share of the tail reserved for heterogeneous overlap windows |
| `tlb-flush` | Per-mille chance of a legal translation flush before an operation |
| `misaligned` | Per-mille chance of a misaligned address when width permits it |
| `vector-corner` | Per-mille chance of corner-biased vector shape/address generation |
| `latency` | `compact` for 1-4 cycles or `spec` for calibrated long-tail responses |

Invalid names, all-zero operation/locality weights, out-of-range per-mille
values, and unknown latency profiles fail before simulation traffic begins.
`random-mixed` requires at least 256 actions so the mandatory architectural
prefix, four overlap windows, and each enabled constrained class can coexist.

## Shipped Presets

Operation columns are relative weights. Locality is `hot/warm/cold`; the next
four columns are per-mille values.

| Preset | Scalar L/S | Vector L/S | Prefetch | Atomic | NC | MMIO | Locality | Concurrent | TLB flush | Misaligned | Vector corner | Latency |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | ---: | ---: | ---: | ---: | --- |
| `coverage` | 200/150 | 150/150 | 100 | 100 | 75 | 75 | 250/250/500 | 1000 | 50 | 500 | 1000 | compact |
| `spec` | 650/270 | 20/10 | 35 | 5 | 5 | 5 | 800/150/50 | 100 | 20 | 5 | 100 | spec |
| `corner` | 125/125 | 125/125 | 125 | 125 | 125 | 125 | 100/200/700 | 500 | 100 | 500 | 1000 | spec |

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

| Dataset | Loads | Stores | First TLB misses | DCache real misses | Miss allocations | Mean MSHR A-to-D |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `4f29a0951` | 5,449,853,667 | 2,335,870,195 | 89,439,821 | 572,769,182 | 483,469,995 | 30.28 cycles |
| `5d3934132` | 15,297,507,427 | 6,294,915,699 | 475,042,094 | 1,491,848,202 | 1,039,068,487 | 32.16 cycles |
| Combined | 20,747,361,094 | 8,630,785,894 | 564,481,915 | 2,064,617,384 | 1,522,538,482 | about 31.6 cycles |

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

Every terminal line prints the resolved target weights and actual operation,
locality, TLB-flush, hit/miss, and latency counts. Each enabled operation and
locality class must be observed at least once. A nonzero TLB-flush constraint
must produce a flush. With backpressure and `latency=spec`, all four latency
classes must be observed. These are minimum gates; distribution quality is
evaluated over long multi-seed campaigns from the recorded counts.

The mandatory prefix still closes architectural invariants that should never be
left to chance, including width/lane legality, nested translation mode pairs,
fault metadata, dirty eviction, redirect, and queue accounting. The configurable
tail determines the workload direction and composes those mechanisms under
pressure.

The harness also models upstream scheduling contracts needed for legal replay.
Scalar stores are reissued when the DUT reports a replay. A vector load whose
active element crosses a 16-byte boundary is advanced to the modeled ROB head
before issue, matching `LoadMisalignBuffer` eligibility. Failure to drive that
backend state creates a UT deadlock, not an RTL failure.
