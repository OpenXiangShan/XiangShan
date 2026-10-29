# MemBlock Constrained-Random Interface

## One Generator, Multiple Directions

The executable scenario is `constrained-random`. `RandomConstraints` supplies
four profiles: `coverage`, `spec`, `corner`, and `cmo-stress`. Directions share
one mixed scheduler, transaction builders, managers, architectural reference
memory, scoreboards, and final accounting. The serialized interface is
`constraint_schema=66`.

This document defines the implemented constraint interface and observations.
[IO_COVERAGE.md](IO_COVERAGE.md) maps top-level pins;
[VERIFICATION_PLAN.md](VERIFICATION_PLAN.md) owns verification scope and
regression acceptance; [ORACLES.md](ORACLES.md) owns expected behavior.
The parser, presets, validation, and target serialization are in
[RandomConstraints](../cpp/random/constraints.inc).

Normal workload correctness has priority: ordinary scalar/FP and vector data,
store-to-load dependencies, cache/TLB overlap and final byte visibility precede
additional exotic error/protection combinations. This is workload-style block
stimulus, not execution of a SPEC CPU program. Seeds vary transaction values, producer counts and ready-event schedules;
bounded stream epochs and serialized helper boundaries remain explicit below. More constraints or longer runs do not guarantee an RTL bug.

Choose the profile with `--constraints PROFILE`. Repeat `--constraint key=value`
to override individual fields after the profile; later assignments to the same
key replace earlier ones. From `tests/memblock`:

```sh
make constrained-random SEED=1 ACTIONS=16384 CONSTRAINTS=coverage
make constrained-random SEED=2 ACTIONS=16384 CONSTRAINTS=corner
make constrained-random SEED=3 ACTIONS=256 CONSTRAINTS=cmo-stress
make constrained-random SEED=4 ACTIONS=16384 CONSTRAINTS=spec \
  CONSTRAINT='translation-nested=20 translation-switch=100 tlb-flush=100'
```

Make defaults `ACTIONS` to 16384 and `JOBS` to 8. The regression controller
defaults to the `constrained-random` scenario, `coverage`, 8 seeds, 8 jobs,
16384 actions, and a 1800-second case timeout; long-run Make targets select
`spec` unless overridden. Defaults are execution choices, not acceptance
thresholds. See the [Makefile](../Makefile) and
[run_regression.py](../scripts/run_regression.py).

For an already published frozen runtime, use a distinct output parent:

```sh
python3 scripts/run_regression.py \
  --runtime-metadata ../../build/memblock/runtime/runtime.json \
  --output ../../build/memblock/campaign-a/results.json \
  --scenarios constrained-random --constraints spec \
  --start-seed 1 --seeds 8 --jobs 8 --actions 16384 \
  --constraint concurrent=250
```

Follow [README.md](../README.md#parallel-work-and-shared-state) for build
ownership, runtime publication, trace isolation, and concurrent execution.

## Constraint Fields

Weights are nonnegative unsigned relative weights; they need not sum to 1000.
A zero disables that weighted choice. Rates are integer per-mille values in
`0..1000`, applied only at an eligible decision site. A rate is not an exact
percentage of all requests or observed completions. Compound actions, fixed
concurrent-window composition, translation prioritization, and legal-shape
filtering change the resulting distribution.

The grouped spellings below denote separate supported keys. `8/16/32/64`,
`mf8/mf4/mf2/m1/m2/m4/m8`, and numeric ranges enumerate those suffixes only;
they do not imply arbitrary numeric suffix support.

### Operations, locality, and scheduling

| Keys | Type and meaning |
| --- | --- |
| `scalar-load`, `scalar-store`, `vector-load`, `vector-store`, `vector-segment`, `prefetch`, `atomic`, `nc`, `mmio`, `hypervisor`, `cmo`, `ptw-error`, `load-merge`, `set-pressure`, `miss-burst` | Relative operation weights, in this serialized order. Each selects a complete helper action, which can issue multiple transactions. |
| `locality-hot`, `locality-warm`, `locality-cold` | Relative address-pool choices for helpers using locality. |
| `concurrent` | Rate allocating the mixed concurrency action budget, subject to serial reservation. |
| `concurrent-stream` | Rate selecting a variable ordinary mixed stream instead of a fixed heterogeneous window at an eligible concurrency site. Default 750 in all presets; 0 preserves the legacy prefix schedule, 1000 selects streams only. A residual budget below seven actions always uses a stream when enabled. |
| `special-concurrent` | Rate for adding one clean NC/MMIO load to an eligible heterogeneous window; NC/MMIO weights choose the class. |
| `translation-switch` | Rate for resampling the translation context after required context/leaf/fence observations. |
| `tlb-flush` | Rate for a compatible translation fence after required fence observations. |
| `misaligned` | Misalignment rate where width, operation, and legal address class permit it. |
| `stride-stream` | Rate selecting a fixed-PC, 128-byte-stride cold scalar-load stream. |
| `bank-conflict` | Rate replacing an eligible clean scalar-load action with a two- or three-load same-bank resident wave; banks are derived from request addresses. |
| `scalar-fp` | FP format rate for eligible ordinary scalar loads, heterogeneous-window scalar loads, dependency-window younger loads and unresolved provisional/refetch instructions. Defaults coverage/spec/corner/cmo-stress: 250/200/500/0. Zero selects integer formats and 1000 selects FP formats at these sites; auxiliary loads retain their own contracts. |
| `scalar-fp16`, `scalar-fp32`, `scalar-fp64` | Relative FLH/FLW/FLD width weights when FP is selected. Coverage/corner use 1/1/1, spec uses 0/1/1, and cmo-stress uses 1/1/1 with FP disabled. Enabled FP requires a nonzero sum; zero disables that FP width. |
| `scalar-unresolved` | Rate replacing an eligible ordinary scalar-store action with a 1..4-older-store/1..4-younger-load delayed-address graph, before the `scalar-dependency` selection. Defaults coverage/spec/corner/cmo-stress: 200/100/300/0. |
| `scalar-unresolved-conflict` | Conditional window rate with at least one older store overlapping the load; default 500 in all presets. Zero forces disjoint-byte controls and 1000 forces conflicts. |
| `scalar-unresolved-inflight` | Conditional conflicting-window rate selecting one youngest unfinished cold scalar/FP load, with a held late response and observed redirect cancellation; default 250 in all presets. Selection needs a same-page line with no accepted external A history; unavailable attempts are reported. Zero retains all-completed windows. |
| `scalar-dependency` | Rate replacing a remaining eligible ordinary scalar-store action with a randomized dependency window: 1..4 older stores, 1..3 younger loads, and an optional store younger than those loads. Widths, byte overlap, payloads, lanes, operand arrival, issue order and 0..7-cycle gaps vary. Defaults coverage/spec/corner/cmo-stress: 350/200/500/0. |
| `latency` | `compact` or `spec`, applied to all three managers. |
| `dcache-latency`, `ptw-latency`, `uncache-latency` | `compact` or `spec`, applied to one manager. |

### Scalar errors, atomics, and hypervisor accesses

| Keys | Type and meaning |
| --- | --- |
| `dcache-load-error`, `dcache-load-error-denied` | Rates: cold cacheable scalar-load error injection, then conditional denied share; the alternative error is independent corrupt. |
| `atomic-amo`, `atomic-lrsc`, `atomic-cas`; `atomic-w`, `atomic-d` | Relative family and width weights. |
| `atomic-error`, `atomic-error-denied` | Rates: atomic cold-refill error injection, then conditional denied share. |
| `atomic-probe-depth0` .. `atomic-probe-depth8` | Relative successful-atomic Probe burst depth. Zero selects no burst; nonzero depths use clean auxiliary lines and controlled C/D backpressure. Error atomics do not issue these Probes. |
| `hypervisor-hlv`, `hypervisor-hlvx`, `hypervisor-hsv` | Relative hypervisor family weights. |
| `hypervisor-spvp-user` | Rate for SPVP=U rather than S. |
| `hypervisor-pbmt-pma-pma`, `hypervisor-pbmt-pma-nc`, `hypervisor-pbmt-pma-io`, `hypervisor-pbmt-nc-io`, `hypervisor-pbmt-io-nc` | Relative VS/G leaf-PBMT pair weights, in this order. |
| `hypervisor-pma-device` | Rate for an aligned interior address in the fixed SoC noncacheable R/W, non-executable device interval, rather than a DDR alias. |
| `hypervisor-pmp-none`, `hypervisor-pmp-first`, `hypervisor-pmp-last`, `hypervisor-pmp-below`, `hypervisor-pmp-above`, `hypervisor-pmp-cross-lower`, `hypervisor-pmp-cross-upper` | Relative PMP relation weights, in this order. Edges use a PMA/PMA DDR alias and a 4-KiB NAPOT RWX region inside an enclosing deny region: first/last allow; below/above/crossing deny. |

### CMO, PTW errors, and pressure actions

| Keys | Type and meaning |
| --- | --- |
| `cmo-clean`, `cmo-flush`, `cmo-inval` | Relative CMO operation weights. |
| `cmo-dirty`, `cmo-younger-overlap` | Rates for dirtying the target by a committed store and for a younger cold-load cancellation check. |
| `cmo-error`, `cmo-error-denied` | Rates for an opcode-qualified CBOAck error and conditional denied share; otherwise corrupt. |
| `cmo-probe-depth1` .. `cmo-probe-depth8` | Relative successful-CMO total Probe depth: target plus zero through seven auxiliary lines. Error CMOs omit these Probes. |
| `ptw-error-stage1`, `ptw-error-gstage`, `ptw-error-nested-g-implicit`, `ptw-error-nested-vs`, `ptw-error-nested-g-final` | Relative manager-error site weights, in this order. |
| `ptw-error-root`, `ptw-error-intermediate`, `ptw-error-leaf` | Relative faulting level classes; Sv48/Sv48x4 intermediate selection can choose either internal level. |
| `ptw-error-store`, `ptw-error-denied`, `ptw-error-corrupt-first` | Rates for store direction, denied response, and first rather than last beat among corrupt responses. |
| `load-merge-depth2`, `load-merge-depth3` | Relative scalar-load batch depth. |
| `load-merge-same-address`, `load-merge-same-beat`, `load-merge-cross-beat` | Relative address patterns: duplicate address, distinct offsets within one 32-byte beat, or across both beats of a line. |
| `set-pressure-depth9`, `set-pressure-depth10` | Relative target-line count per window. The generated cache's four ways and two-set window supply replacement pressure. |
| `set-pressure-sb`, `set-pressure-sh`, `set-pressure-sw`, `set-pressure-sd` | Relative access widths. |
| `set-pressure-set-q0` .. `set-pressure-set-q3` | Relative physical-set quarter selection. |
| `set-pressure-window1` .. `set-pressure-window8` | Relative number of independent windows. |
| `set-pressure-dirty`, `set-pressure-refill-overlap`, `set-pressure-release-backpressure` | Rates for dirty lines, held-refill overlap, and C-channel backpressure. |
| `miss-burst-depth2` .. `miss-burst-depth17` | Relative distinct cold-line depth with held refills; depth17 exceeds the generated 16 miss entries. |
| `miss-burst-issue-width1` .. `miss-burst-issue-width3` | Relative scalar issue width, filtered by available scalar members. |
| `miss-burst-scalar-only`, `miss-burst-scalar-vector-load` | Relative composition; the mixed composition replaces one scalar load with a vector load. |

### Translation, vector shapes, and memory type

| Keys | Type and meaning |
| --- | --- |
| `translation-bare`, `translation-stage1`, `translation-nested` | Relative ordinary context weights. |
| `stage1-sv39`, `stage1-sv48`; `vs-sv39`, `vs-sv48`; `g-sv39x4`, `g-sv48x4` | Relative host, VS, and G mode weights. |
| `translation-permuted` | Rate selecting seeded nonidentity 4-KiB page permutations for eligible ordinary cacheable addresses in translated contexts. Defaults coverage/spec/corner/cmo-stress: 500/500/500/0. Bare, NAPOT and special high-VA/FOF/error/helper pools retain their existing mappings. |
| `translation-stage1-napot`, `translation-vs-napot`, `translation-g-napot` | Rates for legal 64-KiB NAPOT leaf topology; nested VS/G choices are independent. |
| `fence-sfence`, `fence-hfence-vvma`, `fence-hfence-gvma` | Relative compatible fence-kind weights. |
| `fence-global`, `fence-selective` | Relative fence-scope weights. |
| `canonical-boundary`, `gstage-boundary` | Rates replacing eligible scalar or singleton vector-store actions with canonical stage1 or unsigned G-only upper-limit boundary checks. G-only selection uses G-mode weights independently of the ordinary regime weights. Simultaneous selections are randomly arbitrated. |
| `vector-corner` | Rate enabling helper-specific corner policy/address choices, including segment partial VL/masking/vstart and ordinary strided-load zero stride. |
| `vector-fof`, `vector-fof-first-fault` | Rates for unit-stride VLEFF among serial vector loads and conditional first-element fault; otherwise element1 faults after mapped element0. |
| `vector-masked`, `vector-vma`, `vector-vta`, `vector-partial-vl`, `vector-nonzero-vstart` | Ordinary vector rates for vm=0, VMA, VTA, VL below VLMAX, and nonzero vstart. These are not segment-policy knobs. |
| `vector-unit-stride`, `vector-strided`, `vector-indexed-unordered`, `vector-indexed-ordered` | Relative ordinary addressing weights, in this order. |
| `vector-index-repeat` | Per-mille chance that each indexed load element after element0 reuses an earlier offset; shared by ordinary and segment loads. Default 250 in all presets. Zero yields distinct offsets; 1000 repeats element0 throughout. Stores always use disjoint whole-field spans. |
| `vector-eew8/16/32/64`, `vector-sew8/16/32/64` | Relative ordinary EEW/SEW weights. For indexed operations EEW is index width; data width is SEW. |
| `vector-lmul-mf8/mf4/mf2/m1/m2/m4/m8`, `vector-emul-mf8/mf4/mf2/m1/m2/m4/m8` | Relative ordinary LMUL and derived EMUL weights. |
| `vector-segment-unit-stride`, `vector-segment-strided`, `vector-segment-indexed-unordered`, `vector-segment-indexed-ordered` | Relative segment addressing weights, in this order. |
| `vector-segment-eew8/16/32/64`, `vector-segment-sew8/16/32/64` | Relative segment EEW/SEW weights. |
| `vector-segment-lmul-mf8/mf4/mf2/m1/m2/m4/m8`, `vector-segment-emul-mf8/mf4/mf2/m1/m2/m4/m8` | Relative segment LMUL/derived EMUL weights. |
| `vector-segment-nf2` .. `vector-segment-nf8` | Relative field-count weights. |
| `vector-segment-store` | Store-direction rate within the segment class. |
| `vector-segment-high-va` | Rate selecting a legal high-half canonical alias for an ordinary non-FOF segment action. Enabled crosses include host Sv39/Sv48 and all four nested VS/G pairs, each by direction and alignment. |
| `vector-segment-fof`, `vector-segment-fof-first-fault` | Unit-stride segment FOF rate among segment loads and conditional first-element fault; otherwise element1 faults. |
| `nc-store`, `mmio-store` | Store-direction rates within the serial NC/MMIO classes. |
| `uncache-error`, `uncache-load-error-denied` | Rates for legal NC/MMIO response error and conditional denied share among error loads; error stores use denied AccessAck. |

### Probes

| Keys | Type and meaning |
| --- | --- |
| `probe` | Rate for a manager-originated dirty Probe sequence after an eligible completed cacheable scalar store. |
| `probe-to-b`, `probe-need-data` | Rates for retaining Branch state before toN cleanup and for explicitly requesting data. Dirty data must be exact even without needData. |
| `probe-overlap` | Rate for holding an unrelated cold refill and issuing clean auxiliary Probes with the dirty target Probe. |
| `probe-triple-overlap` | Conditional rate selecting depth3..8 rather than depth2 in an overlap sequence. |
| `probe-depth3` .. `probe-depth8` | Relative depth weights within that deep group. Distinct accepted-but-unanswered B-source identities are used. |

## Shipped Presets

These are the values assigned by `RandomConstraints::preset`, including defaults
for subclasses disabled by an operation weight or rate. `all 1` means every
member of the listed array is one. Operation arrays follow the 15-key order in
[Constraint Fields](#constraint-fields); other array orders are named in the row
or the corresponding field table. Rates are per mille; arrays are weights.

| Dimension | `coverage` | `spec` | `corner` | `cmo-stress` |
| --- | --- | --- | --- | --- |
| Operation weights, in the order listed below | 200/150/150/150/75/100/100/75/75/75/75/75/100/75/100 | 648/270/90/45/20/35/5/5/5/1/1/0/10/2/15 | 125/125/125/125/125/125/125/125/125/125/125/125/125/125/125 | 1/1/0/0/0/0/0/0/0/0/1000/0/0/0/0 |
| Locality hot/warm/cold | 250/250/500 | 800/150/50 | 100/200/700 | 250/250/500 |
| Atomic AMO/LRSC/CAS | 8/2/2 | 90/5/5 | all 1 | 8/2/2 |
| Atomic W/D | all 1 | all 1 | all 1 | all 1 |
| `atomic-error` | 100 | 0 | 500 | 0 |
| `atomic-error-denied` | 500 | 500 | 500 | 500 |
| Atomic Probe depth 0..8 | all 1 | 1000/10/5/2/1/1/1/1/1 | 1/1/1/1/2/4/8/16/32 | all 1 |
| Hypervisor HLV/HLVX/HSV | all 1 | 90/5/5 | all 1 | all 1 |
| `hypervisor-spvp-user` | 500 | 1 | 500 | 500 |
| Hypervisor PBMT pair, in listed order | all 1 | 996/1/1/1/1 | all 1 | all 1 |
| `hypervisor-pma-device` | 500 | 1 | 500 | 500 |
| Hypervisor PMP relation, in listed order | all 1 | 999994/1/1/1/1/1/1 | all 1 | all 1 |
| CMO clean/flush/inval | all 1 | all 1 | all 1 | all 1 |
| `cmo-dirty` | 500 | 50 | 500 | 1000 |
| `cmo-younger-overlap` | 500 | 10 | 750 | 1000 |
| `cmo-error` | 100 | 0 | 500 | 0 |
| `cmo-error-denied` | 500 | 500 | 500 | 500 |
| CMO Probe depth 1..8 | all 1 | 1000/10/5/2/1/1/1/1 | 1/1/1/2/4/8/16/32 | all 1 |
| `dcache-load-error` | 100 | 0 | 500 | 0 |
| `dcache-load-error-denied` | 500 | 500 | 500 | 500 |
| PTW error site, in listed order | all 1 | all 1 | all 1 | all 1 |
| PTW root/intermediate/leaf | all 1 | all 1 | all 1 | all 1 |
| `ptw-error-store` | 500 | 500 | 500 | 500 |
| `ptw-error-denied` | 500 | 500 | 500 | 500 |
| `ptw-error-corrupt-first` | 500 | 500 | 500 | 500 |
| Load-merge depth 2/3 | all 1 | 19/1 | all 1 | all 1 |
| Load-merge same-address/same-beat/cross-beat | all 1 | 1/8/1 | all 1 | all 1 |
| Set-pressure depth 9/10 | all 1 | 9/1 | all 1 | all 1 |
| Set-pressure sb/sh/sw/sd | all 1 | 1/2/6/20 | all 1 | all 1 |
| Set-pressure set quarter q0..q3 | all 1 | all 1 | all 1 | all 1 |
| `set-pressure-dirty` | 500 | 50 | 500 | 500 |
| `set-pressure-refill-overlap` | 500 | 10 | 750 | 500 |
| `set-pressure-release-backpressure` | 500 | 10 | 750 | 500 |
| Set-pressure window 1..8 | all 1 | 1000/10/2/1/1/1/1/1 | 1/1/1/2/4/8/16/32 | all 1 |
| Miss-burst depth 2..17 | all 1 | 1000/200/100/50/20/10/5/2/1/1/1/1/1/1/1/1 | 1/1/1/1/1/1/1/2/2/4/4/8/8/16/32/64 | all 1 |
| Miss-burst issue width 1/2/3 | all 1 | 100/20/1 | 1/2/4 | all 1 |
| Miss-burst scalar-only/scalar-vector-load | all 1 | 8/1 | 1/4 | all 1 |
| Translation Bare/stage1/nested | all 1 | 5/990/5 | all 1 | all 1 |
| Stage1 Sv39/Sv48 | all 1 | 95/5 | all 1 | all 1 |
| VS Sv39/Sv48 | all 1 | all 1 | all 1 | all 1 |
| G Sv39x4/Sv48x4 | all 1 | all 1 | all 1 | all 1 |
| `translation-permuted` | 500 | 500 | 500 | 0 |
| `translation-stage1-napot` | 500 | 1 | 500 | 0 |
| `translation-vs-napot` | 500 | 1 | 500 | 0 |
| `translation-g-napot` | 500 | 1 | 500 | 0 |
| Fence sfence/hfence-vvma/hfence-gvma | all 1 | 98/1/1 | all 1 | all 1 |
| Fence global/selective | all 1 | 95/5 | all 1 | all 1 |
| `concurrent` | 1000 | 100 | 500 | 0 |
| `concurrent-stream` | 750 | 750 | 750 | 750 |
| `special-concurrent` | 500 | 20 | 750 | 0 |
| `translation-switch` | 500 | 1 | 750 | 500 |
| `tlb-flush` | 50 | 20 | 100 | 50 |
| `misaligned` | 500 | 5 | 500 | 500 |
| `canonical-boundary` | 100 | 1 | 250 | 0 |
| `gstage-boundary` | 100 | 1 | 250 | 0 |
| `vector-corner` | 1000 | 100 | 1000 | 0 |
| `vector-fof` | 100 | 10 | 250 | 0 |
| `vector-fof-first-fault` | 500 | 100 | 500 | 500 |
| `vector-masked` | 500 | 100 | 500 | 500 |
| `vector-vma` | 500 | 500 | 500 | 500 |
| `vector-vta` | 500 | 500 | 500 | 500 |
| `vector-partial-vl` | 500 | 100 | 500 | 500 |
| `vector-nonzero-vstart` | 500 | 20 | 500 | 500 |
| Vector addressing, in listed order | all 1 | 980/10/5/5 | all 1 | all 1 |
| Vector EEW 8/16/32/64 | all 1 | 5/3/2/1 | all 1 | all 1 |
| Vector SEW 8/16/32/64 | all 1 | 1/2/5/2 | all 1 | all 1 |
| Vector LMUL mf8/mf4/mf2/m1/m2/m4/m8 | all 1 | 1/1/2/16/8/2/1 | all 1 | all 1 |
| Vector EMUL mf8/mf4/mf2/m1/m2/m4/m8 | all 1 | 1/1/2/16/8/2/1 | all 1 | all 1 |
| Segment addressing, in listed order | all 1 | 980/10/5/5 | all 1 | all 1 |
| Segment EEW 8/16/32/64 | all 1 | 5/3/2/1 | all 1 | all 1 |
| Segment SEW 8/16/32/64 | all 1 | 1/2/5/2 | all 1 | all 1 |
| Segment LMUL mf8/mf4/mf2/m1/m2/m4/m8 | all 1 | 1/1/2/16/8/2/1 | all 1 | all 1 |
| Segment EMUL mf8/mf4/mf2/m1/m2/m4/m8 | all 1 | 1/1/2/16/8/2/1 | all 1 | all 1 |
| Segment NF 2..8 | all 1 | 32/16/8/4/2/1/1 | all 1 | all 1 |
| `vector-segment-store` | 500 | 300 | 500 | 500 |
| `vector-segment-high-va` | 100 | 1 | 250 | 0 |
| `vector-segment-fof` | 100 | 10 | 250 | 0 |
| `vector-segment-fof-first-fault` | 500 | 100 | 500 | 500 |
| `probe` | 20 | 1 | 100 | 0 |
| `probe-to-b` | 500 | 500 | 500 | 500 |
| `probe-need-data` | 500 | 500 | 500 | 500 |
| `probe-overlap` | 500 | 10 | 750 | 500 |
| `probe-triple-overlap` | 500 | 10 | 750 | 500 |
| Probe depth 3..8 | all 1 | 32/16/8/4/2/1 | all 1 | all 1 |
| `nc-store` | 500 | 300 | 500 | 500 |
| `mmio-store` | 500 | 300 | 500 | 500 |
| `uncache-error` | 100 | 0 | 500 | 0 |
| `uncache-load-error-denied` | 500 | 500 | 500 | 500 |
| `stride-stream` | 500 | 100 | 750 | 0 |
| `bank-conflict` | 500 | 250 | 750 | 0 |
| `scalar-dependency` | 350 | 200 | 500 | 0 |
| `scalar-unresolved` | 200 | 100 | 300 | 0 |
| `scalar-unresolved-conflict` | 500 | 500 | 500 | 500 |
| `scalar-unresolved-inflight` | 250 | 250 | 250 | 250 |
| Latency DCache/PTW/Uncache | compact/compact/compact | spec/spec/spec | spec/spec/spec | compact/compact/compact |

Profiles specify directions, not measured workload fidelity or observed coverage.
`spec` favors hot scalar traffic and unit-stride vectors while retaining small
weights for several stress classes. `coverage` spreads choices more broadly;
`corner` increases cold, error, deep-pressure, and overlap selection.
`cmo-stress` selects CMO with scalar load/store control traffic and disables the
heterogeneous scheduler. Overrides still pass through the same validation.

## Legal Constraints And Endpoint Rejection

[validate()](../cpp/random/constraints.inc) rejects malformed assignments,
unknown profiles/keys, values exceeding unsigned range, rates outside
`0..1000`, all-zero operation/locality/translation weights, and required
all-zero subclass arrays before traffic. Legality is conditional; setting one
rate to an endpoint can require disabling incompatible classes explicitly.

- Vector tuples satisfy `log2(EMUL) = log2(EEW/SEW) + log2(LMUL)`, supported
  LMUL/EMUL bounds, and minimum VLMAX for forced policy combinations. Segment
  tuples additionally fit at most eight data-register uops across NF fields.
  Enabled dimensions must each have a reachable legal tuple. Tuple selection
  uses the product of constituent weights over the legal set, not independent
  unfiltered draws. Concurrent vectors must fit the reserved LQ/SQ flow budget.
- Nonzero `concurrent` requires nonzero scalar-load, scalar-store, vector-load,
  vector-store, and prefetch weights. `special-concurrent` requires nonzero
  `concurrent` and NC or MMIO. `probe` requires scalar stores; enabled deep,
  CMO, and atomic Probe groups require nonzero depth weights where used.
- NC/MMIO require a translated PBMT context, and hypervisor traffic requires
  nested translation. A hypervisor-only mix cannot enable Bare/host-stage1;
  a Bare context with NC/MMIO also needs a supported cacheable control class.
  PTW-error sites require the corresponding host, VS, and G mode weights.
- Non-PMA hypervisor PBMT pairs and fixed-PMA device accesses are aligned.
  Device traffic needs PMA/PMA and a DDR class for non-PMA PBMT pairs.
  PMP edges need the no-PMP control, PMA/PMA, and DDR; natural edges need an
  aligned class and crossing edges need a misaligned class. Invalid fixed
  endpoint combinations are rejected rather than silently made reachable.
- `vector-fof` requires vector loads and host-stage1 traffic; `1000` is rejected
  to retain ordinary vector loads. Segment FOF requires segment loads,
  host-stage1, and a legal enabled unit-stride tuple; its rate also cannot be
  `1000`. Segment high-VA and canonical/G-stage boundary rates require at least
  one eligible shape, direction, alignment, and mode combination.
- `dcache-load-error=1000` requires `concurrent=0` and `stride-stream=0`;
  `bank-conflict` needs a clean scalar-load share. `uncache-error=1000` requires
  `special-concurrent=0`. Error rates require their governing operation class.
  `stride-stream` needs scalar loads and cold locality.
- `dcache-latency=spec` needs cacheable traffic; `ptw-latency=spec` needs
  translated traffic and nonzero `tlb-flush`; `uncache-latency=spec` needs NC or
  MMIO. A nonzero TLB-flush rate needs a fence kind compatible with an enabled
  translated regime and a nonzero scope group. NAPOT choices need supported
  scalar/vector/segment/prefetch traffic, or hypervisor traffic for nested NAPOT.

Validation proves supported choices exist. It does not prove a seed reaches
those choices, that all Cartesian crosses are legal, or that a pin toggles.

## Scheduling And Action Counts

[mixed_scheduler.cpp](../cpp/scenarios/mixed_scheduler.cpp) initializes the
reference memory and page tables, then intersperses budgeted concurrency epochs
with serial weighted actions when `concurrent-stream` is enabled. The probability
of selecting concurrency is its remaining action budget divided by all remaining
actions. A selected epoch chooses a sliding stream or one legacy window using
`concurrent-stream`. Zero preserves legacy windows followed by serial actions.
The run finishes with a canceled-load/survivor redirect check and drain. The drain ends with a DUT readback of every tracked cacheable,
identity-mapped or declared permuted-page committed word before the accounting gates run. There is no
mandatory operation-coverage prefix or coverage-completeness PASS gate. The
final redirect check reserves two actions; `prepare_tail()` needs at least that
budget, so CLI acceptance of a positive `--actions` value alone does not
guarantee successful execution.

The scheduler still reserves `minimum_serial_actions()` from
[constraints.inc](../cpp/random/constraints.inc). For an initial available
budget `A = requested_actions - 2`:

```text
requested_concurrent = floor(A * concurrent / 1000)
minimum_concurrent  = concurrent == 0 ? 0 : min(A, special_enabled ? 64 : 56)
concurrent_limit   = min(max(requested_concurrent, minimum_concurrent),
                         A - min(minimum_serial_actions(), A))
```

This floor is a scheduling reservation calculated from enabled dimensions,
not a minimum legal run size and not a promise that serial bins will be filled.
With unmodified presets, the floor is 4362 for `coverage` and `corner`, and
4246 for `spec`. Therefore ordinary 256- or 1024-action runs of those presets
have zero concurrency epochs even when `concurrent` is nonzero. A custom
profile can change the floor. Increasing only a concurrency rate cannot bypass
this reservation. See [prepare_tail()](../cpp/scenarios/mixed_initialization.cpp).

A heterogeneous window requires seven available actions, or eight with its
optional clean NC/MMIO load. It includes scalar load, vector load, scalar
store, vector store, and prefetch producers, plus scalar/vector store readbacks.
Issue order is randomized subject to ordered-indexed-load dependencies; vector
shapes are restricted to the concurrent flow budget. Store dispatch retains the
architectural-prefix LQ snapshot and wrap bit. The pending ROB head cannot pass
an older incomplete special load, and simultaneous integer destinations are
distinct. Scalar-store and vector-list waits select only their declared
transactions; vector retirement and pending-store replay drive no scalar commit
pulse. This legacy helper drains before the next window. A requested concurrency rate is a budget, not a measured
simultaneity fraction; use the reported unresolved classes and samples as
observations. See [run_concurrent_windows()](../cpp/scenarios/mixed_concurrent.cpp).

The ordinary [stream executor](../cpp/scenarios/mixed_stream.cpp) draws an epoch
of 8..32 architectural instructions, clipped to the remaining budget (so 1..7
are legal), and a live depth of 2..8. It repeatedly samples from current legal
admission, scalar load/prefetch, STA, STD and vector-uop events. Producer counts
are drawn from the five configured operation weights instead of a fixed count
per class. Admission accounts for actual vector LQ/SQ flows and queue headroom;
retired prefixes free capacity for new work while younger instructions remain
live. Integer, FP and vector writing destinations are distinct within each live
class. One architectural deque owns the oldest ROB head and held `pendingst`;
completion-driven LQ dequeue cannot advance it. Only a completed prefix retires,
with scalar commit pulses excluded for vector stores. Context and epoch
boundaries drain, but admission and prefix retirement within an epoch do not.

An admission-order byte overlay freezes scalar/FP/vector load values from initial
reference bytes plus every older input store, independently of issue/completion
order. Loads wait until all older store addresses are known; older scalar STD
may remain unissued. Vector-store address readiness conservatively waits for
its checked writebacks. Beat-crossing loads and scalar STA, ordered-indexed loads and
misaligned vector stores wait for the actual head. STD remains independently
eligible; a blocking hit wait cannot prevent older work from retiring while a
misaligned STA waits for SQ-head eligibility. Exact vector replay service
retains unrelated requests and does not move the head. Scalar address retries
apply only while its STA obligation remains pending. The address pool biases
three of four line choices toward eight lines near a seeded arena and otherwise
uses the configured cacheable pool; per-access offsets, shapes and data vary.
Multiple speculative unresolved addresses, full rename history, atomics/CMO,
segment/fault instructions and NC/MMIO within this stream remain outside it.
Legacy windows still provide optional clean NC/MMIO overlaps.

Serial operation selection is weighted from its first action. Ordinary vector
shape and binary-policy choices do not prioritize uncovered bins. Translation
selection does prioritize unobserved enabled contexts, nested mode pairs,
leaf topologies, and compatible fences before using dominant/weighted context
selection and the configured switch/flush rates. Consequently a small nonzero
NAPOT or flush rate may be overrepresented early. Context changes drain pending
work. See [mixed_serial_selection.cpp](../cpp/scenarios/mixed_serial_selection.cpp),
[mixed_vectors.cpp](../cpp/scenarios/mixed_vectors.cpp), and
[mixed_translation_schedule.cpp](../cpp/scenarios/mixed_translation_schedule.cpp).

`transactions` is the completed top-level action count and must equal
`--actions`. It is not a scalar/vector uop, memory-request, LSQ-entry, or
writeback count. A load-merge, same-bank wave, set-pressure window, miss burst,
atomic sequence, FOF sequence, or readback can contain multiple such events.
`actual_ops` and subclass counters describe their own sampled scopes and need
not sum to `transactions`.

## Coverage And Replay Contract

Coverage is observational. Sparse or zero counters are valid; successful
completion does not assert complete shape, opcode, crossing, latency, Probe,
or mixed-overlap coverage. Shape support, enabled bins, requested traffic,
accepted requests, terminal completions, and reached observations are distinct
claims. Neither runtime duration nor a profile name substitutes for counters
and a corresponding oracle.

The in-process PASS decision checks completion and protocol/oracle errors,
requested action count, an empty expected GrantAck queue, queue allocation/dequeue/legal
cancellation conservation, observed redirect cancellation, absence of unobserved
queue cancellation, and Probe-source lifecycle accounting. It does not require
coverage completion or nonzero stall counters. See
[finish()](../cpp/scenarios/mixed_reporting.cpp).

### Final dirty-write observation

While the mixed run executes, `commit_store`, `commit_vector_store`,
`record_committed_store`, `record_atomic_result`, and
`record_committed_vector_store` register the 8-byte words covered by each
cacheable identity-mapped or declared permuted-page committed write. Permuted
stores register the actual physical word for each byte, including discontinuous
page boundaries. `finish()` drains Sbuffer, DCache and
the queues, switches to Bare, then issues ordinary scalar loads in batches of up
to three and compares every tracked final word with the independently maintained
reference image. A word is compared whole, so unchanged bytes sharing the word
with a modified byte are also checked; vector masked/tail elements must remain
untouched by a store. The readbacks are not counted as constrained-random actions;
their allocations/dequeues remain part of the reported LQ accounting.

Non-cacheable stores are marked `cacheable = false` and remain owned by the
uncache/device transcript oracle. Legacy aliases that declare only a contiguous `oracle_address` remain counted
in `final_observation_alias_skips`; declared permuted-page writes join the
physical readback. For other aliases, the hypervisor action additionally reads back each permitted,
aligned, cacheable final-PMA store through the same translated context, and
high-VA vector segment stores already have scenario-owned element readbacks.
CBO.INVAL and errored CMOs drop the target line from the tracked set because
the former may legally discard dirty data and the latter has no stable
final-byte contract. Successful CBO.CLEAN/FLUSH keep the line tracked so a
later readback detects a stale or corrupted data byte that is visible through
the cache/memory path. It does not establish post-CMO clean/dirty state or
memory persistence of a dirty line that remains cache-resident; the CMO
scenario checks the probe report/data before completion, not the post-completion
state. The same commit may overwrite a tracked word more than once; the
final comparison observes the resulting word, and `final_observation_bytes`
counts committed bytes rather than unique bytes.
`final_observation_observed_words` is incremented after each successful
readback batch, and `finish()` gates that it equals the registered word count;
it remains short of the total if the sweep does not complete.

[run_regression.py](../scripts/run_regression.py) checks terminal-summary
identity, seed, action count, and return status. It writes a per-case report at
`OUTPUT_PARENT/coverage-reports/SCENARIO-seedSEED-INDEX.json`, preserving status,
summary, command, elapsed time, a `metrics` object, and captured failure
terminal output. The ordinary regression controller keeps parsed summary fields
on the aggregate case result but does not populate `metrics`; read the report
summary or aggregate fields for those counters. `INDEX` is the controller's
result-completion index, not a global ID.
Two controllers with different result filenames but the same parent can collide
on report paths, including their temporary files. Give each controller a
unique output parent directory and follow the README trace-isolation rules.
A report-write error is recorded as `coverage_report_error`.

The default constrained-random branch of
[verify_regression.py](../scripts/verify_regression.py) calls
`_check_mixed_accounting`; it verifies replay command/summary consistency and
queue/LSQ accounting, plus available grant/ack accounting and requested campaign
metadata checks. With `--require-coverage` it instead calls
`_check_mixed_coverage`, which requires the constraint schema, complete coverage
arrays and, when `--require-backpressure` is also set, nonzero per-result
backpressure bins. `make -C tests/memblock strict-coverage-regression` generates
a finite coverage campaign and runs that opt-in mode. `STRICT_CONSTRAINTS`
selects the preset (default `coverage`) and repeatable `STRICT_CONSTRAINT`
assignments override individual fields, e.g.
`STRICT_CONSTRAINT='probe=500'` (or the `probe-strict-coverage` target, which
uses `probe=1000`) to exercise the standalone probe-depth classes. A 2×4608 diagnostic run
was functionally clean but strict coverage rejected it because enabled
probe-depth classes 4..7 were unobserved; a 1×16384 run selected only six
standalone probe sequences and also failed. Use `STRICT_CONSTRAINT='probe=500'`
to target the standalone probe-depth classes explicitly; a 4608-action run then
covered all eight depth classes but still left some enabled probe-cap/need-data
cross classes unobserved; `probe=1000` closed all probe bins but shifted the
remaining strict gap to a vector canonical-boundary cross that the directed
`translation-faults` range matrix already exercises. Adding
`canonical-boundary=500` covered that class but re-sparsified four probe-cross
bins at 4608 actions, so a longer mixed campaign is needed. Documentation schema
consistency is separately checked by
[check_io_coverage.py](../scripts/check_io_coverage.py).
[VERIFICATION_PLAN.md#regression-acceptance](VERIFICATION_PLAN.md#regression-acceptance)
is the sole owner of closure-scale requirements.

Replay requires the recorded seed, action count, profile, ordered overrides,
backpressure options, runtime artifacts/dependencies, and relevant environment.
Preserve the aggregate result and per-case report. Use the recorded command
with the published runtime and the
[README reproduction workflow](../README.md#reproduction); source and runtime
revisions are provenance, not coverage or cryptographic PASS gates.
`MEMBLOCK_MIXED_PROGRESS_ACTIONS=N` emits flushed action/cycle/phase checkpoints
without consuming randomness or advancing the DUT. It is diagnostic output,
not evidence that a coverage objective has been reached.

### Report Identifier Guide

Array order and sampling code are authoritative in
[coverage.inc](../cpp/random/coverage.inc),
[constraints.inc](../cpp/random/constraints.inc), and
[mixed_reporting.cpp](../cpp/scenarios/mixed_reporting.cpp).

| Identifier | Interpretation |
| --- | --- |
| `constraint_schema`, `constraints`, `target_*` | Serialization version, selected profile name, and resolved weights/rates/manager latency. The profile name alone does not encode overrides. |
| `transactions`, `cycle`, `*_writebacks`, `*_requests`, `dcache_refills`, `grant_acks` | Completed actions, simulation cycles, and externally counted completion/manager events. Units differ. |
| `actual_ops`, `actual_locality` | Sampled operation and address-pool observations in the field orders above. |
| `actual_vector_*`, `actual_vector_segment_*` | Instruction-level shape/policy/subclass observations, with separate uop, multi-uop, FOF fix-VL, and high-VA counts. The mixed summary also reports `masked`, `unmasked`, and `high_mask`; `high_mask` counts masked vector uops with at least one enabled global element >=16. A shape tuple does not imply every mask, index, fault, or concurrency variant. |
| `actual_canonical_boundary*`, `actual_vector_canonical_boundary*`, `actual_gstage_boundary*`, `actual_vector_gstage_boundary*` | Sampled boundary cases and their controls, recoveries, store checks, and flows. |
| `actual_translation`, `actual_stage1_mode`, `actual_vs_mode`, `actual_g_mode`, `actual_nested_pairs`, `actual_*leaf*`, `actual_fences` | Sampled regime/mode/leaf/fence observations; not an arbitrary page-table or address-remapping inventory. |
| `actual_atomic_*`, `actual_hypervisor_*`, `actual_cmo_*`, `actual_ptw_error_*`, `actual_uncache_*` | Sampled family, width, privilege, memory-type, error, and helper-specific manager observations. |
| `actual_load_merge_*`, `actual_bank_conflict_*`, `actual_set_pressure_*`, `actual_miss_burst_*` | Compound-action shapes, manager/terminal counts, and pressure observations. Same-bank stimulus does not require a particular RTL replay policy. |
| `vector_operand_chains` | Repeated-logical-Vd load-source edges, store-source reuses and completed predecessor waits. Generated load edges must equal completed waits in a passing run; this is a driver obligation, not a physical-register-file monitor. |
| `scalar_dependency_interleaving` | Accepted loads before all store halves, with overlapping older STD unissued, with multiple such STD operands, and windows with early loads. These count input timing/byte overlaps; they do not require an internal replay or bypass. |
| `concurrent_ops` | Legacy-window producer counts: scalar load, vector load, scalar store, vector store, prefetch. |
| `stream` | Epochs, admitted instructions, retired instructions, admissions with older accepted work pending, admissions after a prefix retired while older work remains live, prefix retirements with younger live work, maximum live instructions, maximum accepted-and-pending instructions, initial issue events, vector replay issues, epochs after serial work, last global action index. These are driver/scoreboard observations, not an internal pipeline occupancy oracle. |
| `stream_ops`, `stream_issue`, `stream_vector_uops`, `stream_scalar_retries` | Retired instruction counts in the same five-class order; initial scalar-load/STA/STD/vector-load-uop/vector-store-uop/prefetch issues; admitted vector-load/store uops; repeated STA issues excluded from initial counts. Schema 62 requires conservation and bounds in both verifier modes without a positive-bin gate. The opt-in strict checker retains minimum epoch/action/overlap observations for a stream-only configuration. |
| `concurrent` | Windows, counted window actions, unresolved-overlap samples, maximum unresolved transactions, maximum unresolved producer classes. |
| `backpressure` | DCache request stalls/response delays, PTW request stalls/response delays, Uncache request stalls/response delays. These can be zero. |
| `dcache_latency_*`, `ptw_latency_*`, `uncache_latency_*` | Manager response-latency samples, four bucket counts, total cycles, and maximum. These are observed responses, not architectural deadlines. |
| `lq`, `sq` | Dequeued + legally canceled / allocated entries. Final-observation readbacks are included in LQ allocation/dequeue. |
| `lsq_monitor_schema`, `lsq_enqueued_observed`, `redirect_cancels_observed`, `unobserved_cancels` | Independently observed enqueue/cancellation accounting and any unobserved cancellation residual. |
| `final_observation_words`, `final_observation_observed_words`, `final_observation_bytes`, `final_observation_alias_skips` | Deduplicated 8-byte words registered for readback, words actually issued and retired by successful readback batches, committed cacheable bytes registered before CMO exclusion, and cacheable commits excluded because their physical and virtual forms differ. The finish gate requires the first two fields to be equal; a zero-word run is not behavioral evidence. |
| `final_nc_words`, `final_nc_bytes` | Committed non-cacheable store/atomic words and bytes compared against the manager bus image after drain. A bus-image match is not a DUT uncache readback or a device side-effect oracle. |
| `probe_source_space`, `probe_source_lifecycle`, `probe_max_outstanding` | Available manager source IDs, unique/reuse/wrap counts, and maximum outstanding Probes. |
| `load_wakeups`, `load_cancels`, `raw_load_*` | Feedback diagnostics; they do not define independent expected memory values. |

## Ordinary FP Load Formats

`ScalarLoadPolicy` chooses destination class and width before address alignment
and dependency construction. Integer choices retain LB/LH/LW/LD/LBU/LHU/LWU;
FP choices use FLH/FLW/FLD with exclusive FP write enable. Existing address,
translation, misalignment, payload, operand-order and scheduler choices still
apply. FP formats enter ordinary loads, the scalar member of heterogeneous
windows, randomized SQ dependency graphs and unresolved provisional/refetch
instructions. Auxiliary warmups/readbacks, bank/merge/pressure helpers,
atomics, H/NC/MMIO helpers and final physical observation retain their existing
formats. The directed FP scenario separately covers selected NC/MMIO/fault paths.

The common model assembles declared physical bytes and NaN-boxes FLH/FLW to
FLEN=64, preserving all low payload bits; FLD preserves all 64 bits, including
NaN payloads. Dependency expectations apply older input-store byte overlays
before the same format rule. Unresolved executions use the pre-store image,
then the program-order image after identified recovery; refetch preserves FP
class and reuses its restored destination. No DUT data/address determines an
expected result. The ordinary factory samples integer destinations 1..223 and
FP destinations 1..191, within the generated 224/192-entry files. Simultaneous
dependency destinations and unresolved warm/provisional/readback roles remain
distinct; selecting a legal numeric domain does not model a complete rename
free-list history. FP physical zero is writable but is not selected here.

`scalar_format_choices` contains four integer/FP pairs in ordinary,
heterogeneous-window, SQ-dependency and unresolved-instruction order. These are
architectural selections, counted once even when an unresolved load refetches.
`scalar_fp` counts sampled checker executions by FLH/FLW/FLD width;
`scalar_fp_origin` splits them into the same four origins. A refetch adds a
separate execution epoch; schema 65 permits up to five executions per unresolved
FP instruction. `scalar_fp_sign` contains two low-payload sign bins
per width; declared faults instead contribute to `scalar_fp_faults` by width.
These are scenario/checker observations in successful runs, not independent
physical-register-file monitors. `scalar_fp_overlay` counts independently
expected older-store/snapshot bytes in dependency and unresolved executions;
it does not count internal forwarding events. `scalar_fp_misaligned` counts
sampled unaligned FP addresses. Accounting checks conserve choices, origins,
widths, signs/faults and endpoint/zero-width controls without requiring positive
bins. Fault data follows the existing declared-exception policy.

Exceptional misaligned FP destination-enable semantics remain unqualified:
the main load path gates both enables on fault, while the misalignment source
copies FP enable. A complete CPU-path disposition is required before treating
that difference as an externally visible functional violation. This does not
restrict ordinary successful misaligned FP traffic.

## Ordinary Scalar Dependency Windows

The common generator's `scalar-dependency` direction retains the selected
cacheable context and address pool. All accesses are naturally aligned and
contained in one cache line. It allocates ROB/LSQ identities in program order,
holds retirement behind an older non-memory instruction, and shuffles store
address/data arrival independently of age. The common ordinary-store factory
uses one paired STA/STD lane as the CPU scheduler does. Simultaneous dependency
loads have distinct numeric destinations within their class domains: a window
containing FP reserves indices in 1..191 and an integer-only window in 1..223.
A ready-event sampler interleaves STA, STD and load issue. A load waits for all
older STA helpers to return identified TLB-hit feedback, while one or several
older STD operands may remain unissued. The optional younger store's halves may
also interleave, but that store remains excluded from each load's SQ snapshot. No store may dequeue before those loads complete. Stores
then commit in architectural order, and every touched word joins final readback.

Each expected load comes from a pre-window byte snapshot overlaid with only
older store inputs in program order. This checks youngest-older-store selection,
partial cache/store byte assembly, signed/unsigned widths and younger-store
exclusion without depending on the DUT's forwarding or replay choices.
`scalar_dependency` reports windows, stores, loads, younger-store controls,
bytes supplied by older stores, bytes supplied by the initial image, and loads
whose final bytes come from multiple older stores. `scalar_dependency_interleaving`
reports accepted loads before all store operands, accepted loads overlapping one
or more older stores whose STD is unissued, accepted loads overlapping multiple
such stores, and windows containing early loads. These are accepted-issue/operand
and semantic source counts, not assertions that a particular internal bypass or
replay occurred. A window is
one compound action; its uops still participate in ordinary completion and queue
accounting. A short run may have no windows when eligible stores are not drawn.

Ordinary bare/cache0/cache1, atomic and stage1-NAPOT pools, plus merge/miss/bank
pools, now use lazy bytes calculated from the seed and full physical word
address before traffic. Read order consumes no random state. Manager and
reference images start equal; architectural commit writes remain independent of
bus writes. This removes representative +256-byte and +16-KiB repeated-pattern
blind spots. It does not make every individual byte unique or randomize the
remaining fault/device setup pools.

## Unresolved Scalar Store Addresses

`scalar-unresolved` uses the ordinary cacheable context and one warm line,
drawing 1..4 older stores and 1..4 younger scalar/FP loads. All store addresses remain
unknown through initial execution. Windows without a cold role strictly complete
every load; an optional youngest cold load instead remains pending until observed
cancellation. Stores/primary load use one
16-byte comparison region; other loads can use the full line. Conflict windows
contain an overlapping primary load; controls keep every load disjoint from every
store. Other load/store edges are unrestricted, so different STAs may select
earlier, equal or later victim ages. Store/load widths, aligned offsets, formats,
lanes, payloads, early STD selection, remaining operand events, initial issue
permutations and each suffix-refetch permutation/gap vary independently.

The source-ordering witness is `A,S0,P1,S1,...,L0,L1,...`: A groups S0's
base/data producers with an independent unfinished FP member; later producer
ROB groups are separated by stores. Memory instructions do not compress.
A's unfinished component holds retirement until all recovery completes. This is
a partial-order backend premise, not qualification of a specific FU's ability to
remain unfinished for every randomized PTW/round duration.

Each successful STA is checked independently. Scan input load intervals in
program age to choose its oldest conflict. Stop further address events, require
that load's full flush-itself ROB/FTQ/RVC identity, return it at observed violation
cycle+2 through the isolated backend registers, and drain existing redirect
filters. One request per conflicting STA is a fixture scheduling invariant;
several STAs issued before recovery could legally produce repeated requests.
All older stores and their dispatch snapshots survive, with no SQ cancellation
or dequeue. LQ cursor rewind uses only that round's observed cancellation,
bounded by its suffix length; completed VirtualLQ entries may already have
dequeued while RAW records survive.

Before refetch, issue any still-pending STD belonging to a known address in a
random order. Its later planned event is then consumed without duplicate issue.
Unknown-address STD can already have arrived, but cannot supply bytes. Allocate
the entire victim-and-younger suffix in program age with fresh LQ/scoreboard
epochs, then issue its shuffled ready events. Disjoint younger loads also require
strict reexecution. Each expectation overlays only currently known-address older
stores in program age, choosing the youngest known writer per byte. Unknown
addresses remain speculative. A newly visible writer is repaired against the
preceding input image so stale data differs; a writer hidden by younger known
stores can legitimately leave data unchanged and still require recovery.
Finish every strict suffix completion before the next STA. A load can execute
up to five times, including its initial epoch. Surviving-prefix values must
remain consistent with the known-store image at each round. After all operands
resolve, every current load must match the complete store overlay. Commit stores
in age order, drain and read back every touched aligned word. Controls retain a
32-cycle no-redirect quiet interval; all windows check final redirect count.

Warm/live-load/readback roles reserve distinct numeric destinations in 1..191
when any load is FP, otherwise 1..223. Refetch restores original ROB/pdest, PC,
class and SQ snapshot. Load instruction PCs form one contiguous packet, with
halfword offsets matching 2/4-byte instruction lengths; RVC is selected only for
LW/LD/FLD forms with ordinary RV64 C witnesses. On each flush-itself, the suffix
starts at the victim PC in FTQ entry victim.current_ftq+1 with offsets restarting
at zero. The group fits 16 bytes; initial FTQ values 0..23 and at most four rounds
avoid pointer wrap. Later redirects match the current epoch's FTQ metadata.

Initially all store/load PC indices are assumed distinct and unassigned in the
external dependency predictor, with no periodic clear during this bounded window.
Each new store PC trains its first conflict; a trained victim refetches with
`store_set_hit=true`. Original untrained surviving stores never entered LFST,
so `load_wait_bit=false` and `load_wait_strict=false` remain legal. The ignored
wait-ROB value is canonically zero. Store PC/FTQ/predictor fields are pruned from
the generated STA inputs: this is an external producer witness, not dynamically
exercised SSIT/LFST training. Full predictor, rename and frontend execution remain
outside the block oracle.

`scalar-unresolved-inflight` can reserve one of the four roles as the youngest
load on a different line of the same 4-KiB page. Other roles remain on the warm
line, with at least one completed conflict. Drain prior committed SBuffer stores
before the seeded cyclic search: a never-requested line could otherwise receive
forwarded data from a pending committed store. Choose a line with no accepted
physical A history and arm its per-line D hold before any further DUT tick. No available line records an unavailable attempt and retains the
completed-load path. A prefetch can own the held A; this is a pending-load and
late-response check, not unique request attribution. The cold load's independent
bytes and FP boxing come from the mapped architectural input image.

All warm loads complete, while the accepted cold load remains pending with its
response held. The first conflicting STA selects a completed warm victim and
flushes the entire suffix. Strict pending expectations stay active through the
three `redirect_after` ticks, allowing legal writeback-stage boundary behavior.
Only then may a matching pending killed scalar obligation be canceled, using
the just-observed redirect and its positive LQ cancellation budget. Wrong age,
identity, queue snapshot, duplicate cancellation, missing cancellation or stale
boundary fails. Removing the expectation makes a later old writeback unexpected;
no data waiver is added. Hold D through the redirect-filter drain, wait another
random 0..7 cycles, then release and drain the old D/E lifecycle plus a 32-cycle
quiet check before registering any identical-ROB/destination refetch. Late cache
refill is legal; late RF/FP completion is not. Refetch the complete suffix and
finish every later round strictly. Only the first initial cold execution is
canceled; every refetched execution must complete.

A group obligation retains every instruction across completed, canceled,
invalidated and refetched epochs. It rejects missing/duplicate initial or suffix completion,
wrong current victim metadata, changed architectural destination/identity, stale
FTQ epoch, premature next redirect and incomplete terminal readback. Already
consumed execution scoreboards are never waived or canceled. The optional
unfinished role adds one explicit initial cancellation obligation;
its suffix cannot be returned or completed without that observed cancellation.

The existing `scalar_unresolved` tuple remains per-window: windows, conflicts,
controls, initial execution boundary (warm completions and optional cold pending),
windows with matched/returned/completed recovery,
committed readback, any early STD, primary overlapping-byte union, and total
observed LQ cancellations across rounds. `scalar_unresolved_graph` retains store,
event, peak, inversion, prefix, touched-word and older-store-pair meanings.
`scalar_unresolved_first_conflict` still bins the first conflicting STA's
resolution rank. `scalar_unresolved_load_group` counts distinct initial loads,
multi-load windows, successful initial completions, never-invalidated survivors, distinct
invalidated/refetched loads, globally disjoint/conflicting invalidated loads,
additional first-STA competitors, initial issue inversions and maximum loads.
`scalar_unresolved_victim` now bins the earliest victim across all rounds,
which defines the union of invalidated instructions.

Schema 65 adds `scalar_unresolved_epochs`: redirect rounds, multi-round windows,
invalidated executions, refetched executions, repeated refetches, refetches while
any older STA remains unknown, changed-value and unchanged-value refetches,
sum of known-store counts at each refetch round, promoted STD operands, refetch
issue inversions, maximum rounds, and trained-hit refetches.
`scalar_unresolved_round_victim` bins every round's victim age;
`scalar_unresolved_victim_movement` counts older/equal/younger transitions.
Schema 66 adds `scalar_unresolved_inflight`: selected attempts, fresh-line
unavailability, materialized windows, accepted held-pending loads, canceled
expectations, late responses drained before refetch, strict cold refetch
executions, FP cold roles, and extra release-gap cycles. Initial successful
completions plus initial cancellations equal distinct initial loads. The cold
role is disjoint, always invalidated, and may refetch once per recovery round.

The verifier checks necessary uint64, window/unique-load/epoch/peak, suffix,
knowledge, data, predictor, cancellation and movement bounds without positive
sparse-bin gates. It does not reconstruct full joint histories.

General unfinished cancellation/late-completion overlap, unresolved sliding admission,
arbitrary trained-predictor states, full rename history and cycle-accurate
producer/frontend timing remain gaps. A timeout is not an RTL liveness proof.

## Random Indexed Operands

Ordinary and non-FOF segment indexed instructions generate one architectural
index operand before uop packing. Each instruction chooses an eligible arena
from 64, 256, 1024 and 2048 bytes, capped at 256 bytes for EEW8 indices.
It samples and shuffles aligned slots across that arena. Each slot contains one
complete `data_bytes * fields` group (one field for ordinary accesses).
Index values are unsigned byte offsets encoded at the selected index EEW;
all addressed field bytes stay within the selected mapped pool's margin.

Stores draw distinct groups. Loads initially draw distinct groups and may
reuse any earlier offset according to `vector-index-repeat`. This varies
permutation, spacing, locality and repeated gathers without imposing an
execution order on overlapping stores. Uops sharing a VS2 register receive the
same complete operand slice. Segment input uops receive successive VS2 chunks;
the reference model retains the complete instruction operand. Expected load
bytes and committed scatter bytes come from the input offsets and independent
architectural memory; ordinary final readback observes tracked cacheable words.

`vector_indices` reports ordinary loads/stores, segment loads/stores, total
index elements, repeated elements, adjacent descending offsets and maximum byte
offset. Each instruction is sampled once, including inactive elements: these
are generated operand observations, not accepted-memory-request counts or a
proof of active-element coverage. A topology-only 512-plan unit sample has 512
distinct offset vectors without using address bases, payloads or issue timing.
The production ordinary builder is also checked across all 78 indexed shapes
for shared-register consistency and disjoint store elements.

## Vector Destination And Source Legality

Canonical vector-data builders use the default 128-entry vector file, selecting
nonzero writing destinations 1..127. Every writing uop has a distinct destination
within its instruction, including EMUL>LMUL indexed splits that revisit the same
logical Vd. FOF fix-VL uops independently select 1..31 in the separate 32-entry VL
file. Stores and operand-only segment uops have no writing destination and drive
zero. These are bounded allocation witnesses; complete rename/free-list history
remain unmodeled. Sliding streams additionally reserve distinct destinations across
all simultaneously live writing instructions of the same register class.

For EMUL>LMUL indexed loads, each repeated-Vd uop consumes the preceding rename's
old-destination value. The driver computes it from prior input operands and
independent memory bytes, then applies the external backend producer's all-ones
mask/tail-agnostic policy. A no-op (`vstart >= vl`) retains its input. This models
the CPU producer outside MemBlock; it does not strengthen the MemBlock output
oracle or use a DUT result as an expected value. The existing comparator still
accepts preserved or all-ones agnostic output elements while an instruction has
a body. For a no-op it requires every old-destination byte unchanged, following
RVV 1.0 element definitions. Repeated-Vd stores instead
reuse the same unchanged source bytes. Generated values retain full payload,
mask, index, width and page-map variation.

Unordered indexed issue samples ready uops from a dependency graph, permitting
independent Vd chains to interleave while keeping each load predecessor first.
The dependent successor waits for that exact ROB/flag/uop/destination completion;
registered expectations for unissued siblings remain live. Ordered indexed uops
retain architectural order. The monitored vector slow-replay channel is store
only: its LQ snapshot must never attribute a replay to a vector load. Ordinary,
heterogeneous, sliding-stream and vector-store readback builders share these
rules. Stream epochs retain explicit boundaries and do not model full rename history.

## Ordinary Nonidentity Page Mappings

`translation-permuted` changes ordinary address relationships rather than
selecting a separate fixed transaction template. A mapping-only RNG derived
from the seed shuffles all 132 pages independently for host Sv39/Sv48,
VS Sv39/Sv48 and G Sv39x4/Sv48x4. Each nested reference map composes the
independently declared VS and G permutations for its mode pair. The VA window at
`0x2000000000` and GPA window at `0x2400000000` map into a dedicated cacheable
physical pool, initialized with seed-and-physical-address bytes before traffic.
VA and PA cache colours may differ. The manager records the granted VA alias per
physical line, recalls the old alias to Nothing before replacing it, checks the
returned bytes, and constructs the new grant only after complete data recovery.
Identity controls
and all six translated contexts share the ordinary scalar dependency, unresolved,
vector/segment and heterogeneous-window builders; dedicated helper pools remain
unchanged. High-half segment aliases opt out of this selection.

Transactions retain an immutable declared map for their context. Scalar expected
loads map each byte, and vectors compute architectural element VAs before mapping
each active byte. Store reference updates and final-word registration use the
same independently declared byte addresses. The setup validates both page edges
and load/store permissions with the independent page-table walkers; walker
results do not supply expected data addresses. Vector prevalidation also checks
active byte translations, including intra-element page crossings. This does not
make a cross-page scalar store legal as one backend uop.

Root/mode activation reuses fixed ASID/VMID values and therefore issues covering
global SFENCE.VMA or HFENCE.VVMA/HFENCE.GVMA before further traffic. When this
direction is enabled, translation-context entry also drains SBuffer and D/E,
then freezes an independent reference image for each resident pool line. Mapped
store commits record independent grouped updates only for the same owner alias; a later alias's
commit cannot overwrite the drained old image used to check recall. Voluntary
ReleaseData freezes the baseline and known updates at its first accepted beat,
then checks a complete line consistent with whole aligned scalar-store/vector-
element updates and overlapping update order; disjoint updates may propagate
independently. Misaligned bytes may tear. A new-alias recall requires a refreshed
drain checkpoint with no pending owner updates. Final
physical observation takes the same checkpoint before its Bare readbacks. This
is a bounded quiescent context transition contract: arbitrary simultaneous
synonym stores, error-grant ownership and synonym CMO remain unqualified.
The separate directed alias test
owns its in-flight redirect/refetch contract. Context fences are reported separately
from randomly selected fence coverage.

`permuted_contexts` counts successful helper samples in host Sv39/Sv48 then nested
VS39/G39, VS39/G48, VS48/G39, VS48/G48 order. `permuted_kinds` splits scalar load,
scalar store, vector load and vector store samples. `permuted_pages` counts
context/virtual-page pairs with active bytes; `permuted_cross_page_uops` counts
samples touching more than one virtual page, and `permuted_split_elements` counts
vector elements whose bytes cross a page. These are semantic helper observations,
not cache-demand counts or exhaustive permutation coverage; all-masked uops add
no page work. `context_fences` reports activation fences in SFENCE/VVMA/GVMA order.
`alias_recalls` counts automatic recall requests separately from selected
standalone/cleanup/overlap/CMO/atomic probes; `alias_recall_responses` counts
complete matching C transactions. Both use the ordinary B-source allocator and
are included in total Probe lifecycle conservation. The artifact verifier checks field conservation and bounds without requiring
positive bins in a sparse run. Unit substitution tests exercise wrong-page and
contiguous-PA results against production scoreboards.

## Explicit Stimulus And Oracle Gaps

- Ordinary random vector traffic explores legal EEW/SEW/LMUL/EMUL and addressing
  tuples and draws mask bits 0..127 when `vm=0`; a shape tuple or nonzero
  high-mask draw still does not establish every mask/element/vector-length
  combination. Ordinary and segment indices now vary offsets, permutation
  and repeated load groups, but stay within a bounded arena with alignment to
  complete field groups. Partial byte overlap, overlapping stores and arbitrary
  full-width index values remain outside this direction. Ordered encoding on
  ordinary memory does not prove architecturally observable order at a
  side-effecting target.
- Ordinary 4-KiB translations now include seeded host and nested page
  permutations, with byte-wise data checks and physical final readback. The
  bounded 132-page maps are static and may change cache colour. The manager's
  automatic recalls use drained context checkpoints for independent old-owner
  byte checks; general simultaneous synonym writes are not generated.
  NAPOT/special helper pools retain their
  own mappings, and context transitions drain SBuffer. Arbitrary root/ASID/VMID
  churn, dynamic PTE mutation/invalidation races and general in-flight
  cross-alias recovery remain outside this direction.
- Ordinary VLEFF uses VL=2 and faults at element0 or element1. Segment FOF
  supports legal unit-stride shape tuples but also uses those two fault
  positions. Later arbitrary fault positions, all FOF policy combinations,
  strided/indexed FOF encodings, and active-segment redirect are not generated
  by this random interface. The segment FOF encoding is unit-stride only.
- Legacy heterogeneous windows have a fixed five-producer composition, optional
  clean NC/MMIO load and a drain between windows. Sliding streams vary ordinary
  producer composition and interleave admission/retirement within 1..32-instruction
  epochs, with 2..8 live instructions, but still drain at each epoch. Serial
  helpers supply local Probe/refill/pressure overlaps.
  The scalar dependency direction adds variable ordinary store/load chains;
  the unresolved direction adds 1..4 delayed older stores and 1..4 completed
  provisional loads, exact oldest-victim selection, surviving prefix, repeated whole-suffix
  refetch and disjoint-mask controls. A conditional youngest held cold role adds
  explicit observed cancellation and old-response drain before refetch. Broader unfinished
  cancellation, sliding unresolved admission and dynamic predictor training remain
  absent. Unbounded mixtures, arbitrary dependency/rename histories,
  multiple CMO/atomic ordering, cross-hart reservations, arbitrary fault overlap, and broad reset
  injection are not provided by these constraints.
- The final readback forces cacheable identity-mapped store/atomic effects and
  declared permuted-page store effects, but does not observe remaining aliases, non-cacheable/device effects,
  stores that may have happened on a faulted operation, or dirty data that a CMO
  legally discards. Those paths retain the separate checks and limits described
  in [ORACLES.md](ORACLES.md#implementation-limitations).
- The implemented manager response profiles generate selected legal timing,
  denied, and corrupt cases. They do not establish exhaustive malformed,
  duplicate, early/late, or missing-response detection. Some counters describe
  generator choices or sampled windows rather than independent DUT events;
  request-count hit/miss classification can include prefetch traffic.
  Output-only store-data identity and two-state simulation retain limitations
  described in [ORACLES.md](ORACLES.md#implementation-limitations).

New fields should extend the shared parser, pretraffic validation, resolved
`target_*` serialization, actual observation counters, and deterministic replay.
A material distribution, oracle, or coverage-claim change needs the independent
review specified in
[VERIFICATION_PLAN.md](VERIFICATION_PLAN.md#independent-review-and-no-progress-audits).
No new field should introduce a hidden coverage-completeness PASS gate.

## Structural Randomness Review

The canonical generator varies dependency graph sizes and byte overlap, store
operand event permutations, vector uop/flow shapes, index vectors, masks and
page mappings. Dedicated seed samples exclude payload and timing when measuring
structural uniqueness. Such samples establish those builders' variation, not
unique full-run schedules or an assurance that a design bug exists.

Ordinary stream epochs now vary the number, age and class of instructions,
interleave STA/STD/vector/load events, and admit new work after prefix retirement
while younger work remains live. They are randomly placed among serial helpers;
`concurrent-stream=0` retains the fixed prefix for reproduction. Dedicated
compiled executor tests measure schedules without payload/address/delay fields.
The highest remaining ordinary-workload limitations are epoch/context drains,
conservative older-address readiness and isolated completed-load recovery graphs.
Dependency windows also vary counts and interleave independent operand halves
with loads after older addresses resolve. Canonical FP formats now participate in ordinary,
heterogeneous, SQ dependency and unresolved/refetch traffic with independent
boxing/data checks. The unresolved direction interleaves 1..4 older store operand-ready events,
varies 1..4 younger load issue permutations and victim age, and refetches the
whole invalidated suffix at each conflicting STA, with visible/hidden writer
versions and earlier/equal/later victims. Prioritize broader unfinished cancellation
and integration into sliding admission and broader temporal/address dependencies; broader
FP helper and exceptional-misalignment combinations remain open.
Broader temporal locality/dependent-address streams and scalar/vector SQ
forwarding follow these. Rare error/protection/CMO crosses remain secondary.
