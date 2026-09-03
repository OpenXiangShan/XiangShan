# MemBlock Verification Results

## Current Repaired RTL

- Branch: `kunminghu-v2`
- CPU commit: `0fa7bb8259a7922481289d8d5932797afce84030`
- MemBlock top-file SHA-256: `0b6c6aa34dc8aa148d0f6da91091df58e6622415136e60baf985d7560824e7e3`
- Complete ordered RTL SHA-256: `0b6c6aa34dc8aa148d0f6da91091df58e6622415136e60baf985d7560824e7e3`
- Repaired mixed-test executable SHA-256: `1b689baead6f77c05488a8feab6f8a00bc19aebcae884b103d8404d79fbb2f29`
- Repaired Verilated model SHA-256: `0d8683c215556a96ecd46ba862f3df19e2c000cc7210e344828c1f2c53003`
- Frozen xspcomm SHA-256: `0592b633c82eb884fc7a5accd3bfd5337d3f58cb69253db6a109f614ae6b9f74`
- Frozen runtime manifest SHA-256: `47eb52bff692a6e4c2fa6facc027788cb37709c4fe9f42f39dd577126baf9035`
- Picker commit: `c100874936aad4030d3bc4c8425ab652f2fbc7ad`
- xcomm commit: `23ba5c47310a74dab1567a4ca54ad85dec4512cb`

## Focused Runs

## Current Harness and RTL Fixes

The current constrained-random acceptance pass uses the rebuilt harness after
the stale-runtime issue was removed. `random-mixed` now randomizes scalar and
vector data, widths, masks, `vl/vstart`, legal address classes, vector modes,
issue order, store address/data order, and inter-window delays while preserving
explicit interface constraints. The boundary diagnostic records all generated
parameters and verifies the independent Sv39x4 GPA oracle.

Current acceptance evidence:

- 61 Python unit tests and the complete port/SVA checks pass;
- all focused MemBlock scenarios pass on repaired RTL hash
  `0b6c6aa34dc8aa148d0f6da91091df58e6622415136e60baf985d7560824e7e3`;
- 32 continuous boundary-hunt seeds produced 2048/2048 repaired oracle passes;
- the five-scenario matrix completed 25/25 invocations and 5480 actions;
- eight 4096-action `random-mixed` seeds completed 32768 actions with all
  per-seed coverage and queue-conservation gates passing.

The historical VS-non-leaf GPA defect in `VMergeBuffer` is repaired by leaving
the page-walk GPA unchanged when `isForVSnonLeafPTE` is asserted, while keeping
the first-active-element offset for the reported vector VA. The deterministic
sentinel and randomized boundary oracle both pass on the repaired hash.

On 2026-09-02, the mixed driver was corrected after reproducing two harness
false positives at long queue depths. Scalar store address/data writebacks are
now accepted only after the corresponding issue input handshake and issue
epoch; this handles the RTL's output-only `writebackStd` port, which has no
`ready` or ROB flag, without confusing stale valid pulses across ROB wrap.
The mixed-window commit boundary now holds `pendingPtr` at the last uop in the
window, preventing the next window's first scalar store from being implicitly
committed before its explicit commit operation. The contract suite gained two
checks for these rules.

Focused scenarios and 20 independent `random-mixed` seeds, each with 512
actions and backpressure enabled, passed after the fix. Seeds 1-20 completed
with no scoreboard, timeout, coverage, assertion, or queue-accounting failure.
The repaired executable SHA-256 is
`1b689baead6f77c05488a8feab6f8a00bc19aebcae884b103d8404d79fbb2f29`;
the repaired complete RTL SHA-256 is
`0b6c6aa34dc8aa148d0f6da91091df58e6622415136e60baf985d7560824e7e3`.

| Test | Result | Key observation |
| --- | --- | --- |
| Idle smoke | Pass | 38 cycles; registered DUT clock and internal reset release |
| Complete pin space | Pass | 749 inputs/7,155 bits and 586 outputs/5,434 bits; 256 patterns; digest `0xc36e86e25361ff60` |
| Single cold load | Pass | One AcquireBlock and one checked 64-bit writeback |
| Vector loads | Pass | Four EEWs, both vector lanes, four exact 128-bit results |
| Vector addressing | Pass | Strided, indexed-unordered, and indexed-ordered vector stores each committed and read back exactly; all four load modes are checked |
| Vector split load | Pass | Three checked writebacks including a split cold-load replay shape |
| Store forwarding | Pass | Four store widths and four matching scalar loads |
| Vector forwarding | Pass | Four vector stores and loads with byte-accurate SQ overlay |
| PBMT=NC store order | Pass | Two stores, two SQ dequeues, two PTW requests, one uncache request |
| Store TLB-miss preservation | Pass | Cycle 156; two misses and two PTW requests; allocated SQ entry remained address-valid |
| DCache dirty release | Pass | Ten stores; two ReleaseData writebacks preserved |
| Redirect | Pass | Canceled miss suppressed; LQ slot reused |
| Queue pressure | Pass | Two legal 60-entry waves; 120 checked writebacks |
| Stateful mixed seed 1 | Pass | 96 actions; scalar/vector load/store, all scalar/vector address modes, `prefetch.i/r/w`, Sv39/Sv39x4, PBMT-NC, cross-forwarding, dirty ReleaseData, redirect recovery, six manager backpressure classes, exact LQ/SQ drain |

The early idle writeback observation was a harness error: the Picker clock had
not been registered. It is not an RTL defect.

## Historical Mutation Checks

Six 2026 LSU fixes were reverted independently from the baseline. The same
checked test sources and generated SVA were compiled against each mutant; the
executable-reported hash was required to match that mutant's complete RTL
metadata. Four mutants fail a stable oracle. Two pass the best currently legal
scenario and are retained as negative evidence, not claimed as reproduced.

| Reverted fix | Mutant complete RTL SHA-256 | Clean result | Revert result |
| --- | --- | --- | --- |
| `e541289b19a5661536d4e4a0d01d2abc9a37b1f0` (2026-08-10), preserve SQ address state on a TLB miss | `c525bef7dbb231ace69254e57b6fc7d7069dcb10ddea17176d548c9277565b8e` | `store-tlb-miss-preserve` passed at cycle 156 with two misses and two PTW requests | Failed at simulation tick 283: allocated entry 0 lost `addrvalid` on a TLB miss |
| `e12436c7cba86b195deec24981976d78bc263661` (2026-08-14), prevent out-of-order `rdataPtr` advance | `757dfc00827ed605d63db23a2d9bc995732628abf18b4b6cc203ef8190c49a30` | `store-rdata-order` passed at cycle 309 with two ordered SQ dequeues | Failed at cycle 309: the NC store used out-of-order SQ read data |
| `45318c5d` (2026-04-20), clear RF write enable on a load exception | `341dff7baa1442e4050c131cc4b2a9f864d1facbf584cdecdabfb81bb6c32ffb` | `exception-contracts` passed with exact page-fault and no RF write | Failed: exceptional scalar load requested a scalar RF write |
| `856b821f` (2026-08-11), propagate vector VS-non-leaf-PTE metadata | `3abf90d701fdb3252ec68d1426707affe4d0606bb002a1b02d710be577baa5a7` | `vector-guest-fault` passed at cycle 152 with marker 1 and exact VA/GPA | Failed at cycle 152: marker was 0 while VA/GPA stayed correct |
| `9ee7b335` (2026-08-10), misaligned vector-store progress | `9000f90adc416d1de6c7b4e8ce2b0129cf98564431ae835bcfc6d857f89e4df2` | `misaligned-stores` passed at cycle 879 | Mutant also passed; not reproduced |
| `fbb1e349` (2026-07-21), cross-page vector-store `s_block` progress | `603720f0cb797e679097244a107ca2892f00bbfe0392327fdc935ee2093b2594` | `misaligned-stores` passed at cycle 879 with three vector replays | Mutant also passed; not reproduced |

The TLB-miss scenario first establishes a hit on an allocated SQ entry without
sending store data or allowing dequeue. The SVA antecedent requires that entry
to be both allocated and address-valid, records its ROB/uop identity, and only
checks the next cycle if the same entry remains allocated. This excludes
dequeue, redirect cancellation, and same-slot reallocation false positives.

Every mutant RTL tree was regenerated from its independently reverted Scala
source. A full generated-RTL search found no temporary `SQPROBE`, `TLBCHK`, or
diagnostic `$display` instrumentation. Apart from the generated RTL hash
comment, mutants use the same final C++ harness and generated SVA semantics as
clean. `HISTORICAL_BUG_AUDIT.md` records the strict status and boundary contract
for all 58 commits in scope.

The source hashes below identify the harness revision used for the historical
results above. They are retained as historical provenance only; the previous
artifact did not enforce them:

- historical `memblock_main.cpp`: `2b8a372c88565e26231ecdd87f917335b0f1c2a2980bad7590cd0502e12acc10`;
- historical `memblock_env.hpp`: `f1a638a87547df505696a05200350e1c1b0fa79f15947f9325278a89d77563ad`;
- historical protocol SVA: `31feec579cf939d04f446114071e5860e17ed990e8c680a67f6f9b5c0c91ff6d`.

The previous runner did not hash these source files, so these values were not
independently enforced by the JSON artifact. The reviewed runner now records
and verifies source hashes as controller inputs for new campaigns. The final
six-hour artifact above predates that change and is therefore not
provenance-complete for this reviewed harness revision; a new final campaign
is required after this review.

The current mixed summary reports vector load and store address-mode coverage
separately (`vec_load_modes` and `vec_store_modes`), so a load cannot mask a
missing store mode. Each backpressure-enabled mixed seed also reports nonzero
DCache, PTW, and uncache request-stall and response-delay counts.

## Development Regression

On 2026-08-30, a four-process mixed run completed eight seeds in each of four
scenarios:

- 8,000 randomized scalar-load transactions;
- 8,000 randomized vector-load transactions with 16,000/16,000 LQ entries retired;
- 384 randomized scalar store-forwarding transactions;
- 192 randomized vector store-forwarding transactions;
- all scalar/vector widths and all scalar/vector issue lanes covered;
- randomized TileLink A/D delays enabled;
- zero scoreboard, assertion, timeout, or RTL failures.

Artifact: `build/memblock/regression.json` (generated, not tracked).

The enhanced stateful mixed test was then checked across seeds 1-12 with 64
actions per seed. All 768 actions passed per-seed coverage gates. Each seed had
three PTW requests, two uncache requests, at least five dirty ReleaseData
transactions, two simultaneous scalar/vector issue points, both cross-type
forwarding directions, all three software-prefetch operations, one redirect,
and exact final queue accounting. A four-seed all-scenario regression also
passed 20/20 scenario invocations and 960 reported actions.

Artifacts: `build/memblock/mixed-short.json` and
`build/memblock/regression-short.json` (generated, not tracked).

On 2026-09-01, the prior fully frozen runtime also passed all 12 focused scenarios
and an eight-seed, five-scenario matrix:

- 12/12 focused scenarios passed, including complete pin space, vector
  load/store forwarding, DCache dirty release, redirects, both StoreQueue
  historical-bug scenarios, and queue pressure;
- 40/40 random scenario invocations passed;
- 17,088 reported transactions covered scalar/vector loads, scalar/vector
  forwarding, and stateful mixed traffic;
- runtime, external-library, and complete RTL hashes were consistent.

Artifacts: `build/memblock/runtime-short.json` and
`build/memblock/runtime-matrix.json` (generated, not tracked).

## Historical Extended Campaign (Stale Artifact)

An earlier eight-process duration-based `random-mixed` campaign was intended to
run for four hours against the historical baseline RTL. Its original artifact
was overwritten during development by a one-second smoke artifact and is no
longer available for independent verification. The numbers below are retained
as historical notes only and must not be used as acceptance evidence:

- requested duration: 14,400 seconds;
- measured monotonic elapsed time: 14,400.633 seconds;
- 215,359/215,359 consecutive seeds passed, from seed 1 through seed 215,359;
- 13,782,976 stateful mixed actions completed (64 per seed);
- every result reported return code zero and the same complete RTL SHA-256
  `39709aa5225aa56ce6764569bbcbd20089ff25ff89eafaf0d3e7b9e3632ea815`;
- simulator, Verilated model, xspcomm, five resolved system libraries, runner
  source, runtime manifest, and RTL metadata had identical before/after hashes;
- no assertion, scoreboard, timeout, process, coverage-gate, or RTL failure was
  observed.

Artifact: `build/memblock/extended-mixed-frozen-4h.json` is currently a stale
one-second development artifact (not tracked), SHA-256
`3943cd27585ffc0b36c35b7d8ed3dd8c225da4bf0dc3e290bf66632c5523af0f`.
`make verify-extended-results` is expected to reject it because its duration and
provenance do not satisfy the four-hour gate. The final six-hour campaign below
is the only duration artifact accepted for the repaired RTL.

## Earlier Extended Campaign

On 2026-09-01 (Asia/Shanghai), the mixed-test executable completed a
four-process duration-based campaign against the baseline RTL:

- requested duration: 14,400 seconds;
- measured monotonic elapsed time: 14,400.555 seconds;
- 100,132/100,132 seeds passed, with no failures, errors, or timeouts;
- 6,408,448 stateful mixed actions completed (64 per seed);
- every result reported `random-mixed`, return code zero, and the same complete
  RTL SHA-256 `39709aa5225aa56ce6764569bbcbd20089ff25ff89eafaf0d3e7b9e3632ea815`;
- the executable SHA-256 remained
  `30d9758abd3fc0a36e46d5b4ef24ac64e6e7ea7c6e2a8485f95c7ded7b1d8aff`.

Artifact: `build/memblock/extended-mixed-4h.json` (generated, not tracked),
SHA-256 `307e9006ac5bd316f2defebc756d13b2b141cb727e0beba768a0d9f7e86d9a6d`.

This earlier artifact hashes the executable and complete RTL, but the
executable dynamically loaded model and xspcomm shared libraries outside the
frozen directory. It is retained as behavioral evidence, not used as the final
immutable-runtime provenance result.

## Worker Scaling

On the 24-logical-CPU verification host, the same 256 `random-mixed` seeds
(16,384 actions) took 33.087 seconds with four workers and 17.190 seconds with
eight workers. Both runs passed and reported the same RTL SHA-256. Eight workers
were therefore selected as the default for normal and duration-based regression.

## Current Boundary Finding

The historical clean baseline had a deterministic vector guest-fault split
candidate: `vector-guest-fault-split` reported GPA `0x94001808`, while the
independent VS/G-stage page walk required `0x94001800`. The conditional
`VMergeBuffer` fix was regenerated into the current RTL; scalar, aligned-vector,
split-vector, and randomized boundary controls now all report the exact oracle
GPA. The clean-RTL failure remains in `VECTOR_GUEST_FAULT_SPLIT.md` as mutation
evidence, while the repaired test is part of the green sentinel gate.

The final acceptance section is populated only after the current six-hour
frozen campaign has completed and `make verify-final-results` has independently
validated its artifact. Historical baseline and mutation results above are
retained as evidence and are not claims about the repaired RTL.

## Final Six-Hour Campaign

On 2026-09-03 (Asia/Shanghai), the repaired frozen runtime completed the final
fully mixed campaign:

- requested duration: 21,600 seconds;
- measured monotonic elapsed time: 21,637.196513 seconds;
- 3,802/3,802 continuous seeds passed, from seed 1 through seed 3,802;
- 15,572,992 constrained-random mixed actions completed (4,096 per seed);
- every result used scenario `random-mixed`, return code zero, and complete RTL
  SHA-256 `0b6c6aa34dc8aa148d0f6da91091df58e6622415136e60baf985d7560824e7e3`;
- frozen binary, Verilated model, xspcomm, resolved system libraries, runtime
  manifest, controller inputs, and RTL metadata were unchanged before/after;
- no assertion, scoreboard, timeout, queue-accounting, process, or coverage-gate
  failure occurred.

Artifact: `build/memblock/final-frozen-6h.json`, SHA-256
`704c403d1470846143d24bdb90e0587c6b5e16840aa8be89741e3ed6af30a4e5`.
`make verify-final-results` independently validated the artifact and reported
`MEMBLOCK_REGRESSION_ARTIFACT_PASS` for seeds `1..3802`.

This is evidence that the repaired RTL satisfies the tested contracts under the
pre-review harness. It is not a proof that untested MemBlock boundary gaps are
bug-free, and it is not the acceptance artifact for the reviewed oracle/source
provenance changes described above.
No additional CPU bug was exposed by this repaired campaign. Historical mutant
results remain the evidence for the four independently reproduced LSU defects
listed above; any future failure should be triaged from its recorded seed and
runtime provenance rather than treated as a known-good result.
