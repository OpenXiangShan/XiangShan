# MemBlock Verification Results

## Current Repaired RTL

- Branch: `codex/memblock-ut-closure-20260905`
- CPU baseline commit: `0fa7bb8259a7922481289d8d5932797afce84030`
- CPU repair commits: `f5b553973` (VS-non-leaf vector fault GPA),
  `f8bb99518` (Uncache exception preservation), and `e1424686a`
  (exceptional atomic `rfWen` suppression).
- Retracted RTL change: `8eedb3ad0` changed the intentional atomic D-channel
  poisoned-line policy and was reverted by `db6f6d844` after design review.
- Verification harness baseline: `98bdebbe0777ef051fa8451bd36641eb45f81963`;
  subsequent harness changes are recorded in branch history.
- MemBlock top-file SHA-256: `d47b43afe6c1bd142c50728e40e9a10b8a55c32a1ad5c51b0ca183a204bfdca2`
- Complete ordered RTL SHA-256: `774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`
- Current rebuilt and frozen UT executable SHA-256: `38f4283585fed1b1288cfc6c9bf18858ab436a635da18baa48b31e957652ddca`
- Historical frozen mixed-test executable SHA-256: `2254bb50285a4d0c05a45bd96f43582240b44a9b52d08a188a14b8396716c6d0`
- Current rebuilt and frozen Verilated model SHA-256: `af39b980658fd5cf913d1ad0d768b41643ef2331c45edb95584159dc537161a0`
- Frozen xspcomm SHA-256: `0592b633c82eb884fc7a5accd3bfd5337d3f58cb69253db6a109f614ae6b9f74`
- Frozen RTL metadata SHA-256: `0814ee0cdc63c87d1799f3ced61562a2f9072593e76a94f16fdb01b719feda1c`
- Frozen runtime manifest SHA-256: `c067c81c687ef86decb3576e5add6f92c5781503eb7fedd3b3d43195a21cbd5f`
- Picker commit: `c100874936aad4030d3bc4c8425ab652f2fbc7ad`
- xcomm commit: `23ba5c47310a74dab1567a4ca54ad85dec4512cb`

## Superseded Pre-Clarification Stress

### Provenance-rejected one-hour stress run

On 2026-09-05, 191/191 continuous `random-stress` seeds completed successfully
with eight workers and 16,384 actions per seed. The run executed 3,129,344
actions over 3,741.172444 seconds; every per-seed summary passed the independent
coverage, backpressure, queue-accounting, command, and RTL-hash checks. The
aggregate artifact is deliberately non-accepting because the mutable prepared
`build/memblock/rtl.json` disappeared before the shutdown controller hash.
The frozen binary/model/xspcomm and system dependencies remained unchanged,
and regenerating the metadata reproduced its launch hash exactly. The same
investigation found that `verify-stress-results` expected 4,096 generic actions
while the runner recorded 16,384. Both framework defects are fixed; the original
artifact remains unmodified evidence rather than being relabeled as a pass.
See [`REGRESSION_PROVENANCE_FAILURE.md`](REGRESSION_PROVENANCE_FAILURE.md).

These runs used complete RTL hash `b69e387e...`, which contained the invalid
local atomic D-channel policy change later reverted by `db6f6d844`. They remain
useful framework/stress history but are not acceptance evidence for the current
RTL hash.

Artifact: `build/memblock/stress-frozen-a32d74a61-1h-16384.json` (generated,
not tracked), SHA-256
`0c4d8a1a68bc9325df35bd465f60a0a0ab4ed3cd368e7f80cd03ef7372ae04e1`.

These 16,384-action direct runs were completed on that superseded RTL; the
newer 32,768-action pair below is also historical:

- `random-stress --seed 29 --transactions 16384` completed 16,384 actions in
  357,217 cycles. It reached 12 outstanding entries, 10,923 vector load/store
  operations, 5,463 masked/unmasked operations, 1,020 scalar misaligned
  operations, 2,731 scalar/vector forwarding overlaps, all four required
  cross-feature bins, and nonzero DCache request stalls and response delays.
- `random-mixed --seed 31 --transactions 16384` completed 16,384 actions in
  467,369 cycles. It produced 7,032 scalar, 4,672 vector-load, 2,336
  vector-store, and 2,343 store writebacks, with 15 PTW requests, 2 Uncache
  requests, dirty ReleaseData, nested translation, exceptions, redirect,
  forwarding, and all six manager backpressure counters nonzero.
- `random-stress --seed 37 --transactions 16384` completed 16,384 actions in
  356,752 cycles with 12 outstanding entries, 2,731 forwarding overlaps,
  1,048 scalar misaligned operations, all required stress combinations, and
  nonzero DCache request stalls and response delays.
- `random-mixed --seed 41 --transactions 16384` completed 16,384 actions in
  467,199 cycles with 7,033 scalar, 4,671 vector-load, 2,336 vector-store,
  and 2,343 store writebacks; it exercised nested translation, exceptions,
  redirect, forwarding, dirty ReleaseData, and all six manager backpressure
  counters.

These are pre-clarification exploratory evidence and are not silently promoted
to current-RTL acceptance evidence.

The post-translation-matrix `random-stress` driver was exercised against the
current rebuilt binary with a finite multi-seed campaign:

- 8 continuous seeds (`1..8`) passed with eight workers;
- 32,768 stress actions completed (`4,096` actions per seed; the runner maps
  stress actions to `mixed_transactions_per_seed`);
- every seed built one- and two-group bursts with scalar/vector forwarding,
  mask/vstart/vl variation, vector EEW and addressing variation, queue pressure,
  and randomized issue order;
- every seed reached at least 12 outstanding scoreboard entries, all required
  stress combinations, both cache regions, both DCache backpressure classes,
  and balanced LQ/SQ accounting;
- each terminal summary recorded four independent SplitMix64-derived RNG
  streams for traffic, shape, payload, and scheduling;
- `verify_regression.py --allow-finite --require-backpressure` checked every
  recorded summary, continuous seed, command replay, RTL/controller hash,
  coverage field, and artifact integrity;
- the campaign completed in 68.134896 seconds with no scoreboard, assertion,
  timeout, or queue-accounting failures.

Artifact: `build/memblock/stress-current-translation-superpages.json` (generated,
not tracked). Artifact SHA-256:
`5ae6e1bfc90afe791d3e1e7034472905829626d0327b2a9aa62924f4370fabb`.
An earlier 32-seed exploratory run found two false positives from zero-stride
vector forwarding stores. The stress generator now constrains forwarding stores
to non-overlapping positive/negative strides, matching the deterministic byte
oracle; zero-stride independent loads remain covered by `random-mixed`, and
repeated-address stores remain in explicit overlap tests.
The duration-based acceptance target is `make stress-regression`, which uses
the frozen runtime and a one-hour minimum by default. The earlier
`stress-large-provenance-final.json` artifact predates the translation-matrix
controller change and is historical rather than current acceptance evidence.

## Sv48/Nested Translation Matrix

The current harness now exercises the four non-Bare mode pairs independently:

- `Sv39->Sv39x4`
- `Sv39->Sv48x4`
- `Sv48->Sv39x4`
- `Sv48->Sv48x4`

`make translation-matrix` passed all four pairs with DCache/PTW
backpressure, a high-half canonical Sv48 VA, G-stage mappings for every VS
page-table page, and cold/warm accesses. The run completed 1,129 cycles per
pair on average, with 40 PTW requests and four data TileLink requests total;
the second access in every pair reused the translation without an additional
PTW request. This is the 4-KiB nested path; Bare degenerations and the full
permission/fault matrix remain explicit boundary work.

`make translation-fence` also passed: a same-VA Sv39 leaf update stayed on the
old translation before the fence and refilled to the new physical page after a
global and selective `SFENCE.VMA`; nested VS and G-stage leaf updates refilled
after selective `HFENCE.VVMA` and global `HFENCE.GVMA` respectively (35 PTW
requests and ten load writebacks across all checks).

`make translation-context` passed the direct Sv39-to-Sv48 `satp` root/MODE and
ASID switch on one VA: the first access used the Sv39 physical page, the switch
forced a new four-level walk, and the second access used the Sv48 physical page.

`make translation-superpages` passed all ten deterministic leaf cases: Sv39 and
Sv48 stage-1 2 MiB/1 GiB leaves, Sv48 512 GiB, and the corresponding
Sv39x4/Sv48x4 G-stage leaves. Every case matched the independent leaf-address
oracle and completed an architectural load.

`make translation-faults` passed all five deterministic fault cases: a
noncanonical Sv48 virtual address, an invalid Sv39 root PTE, Sv39x4 and Sv48x4
GPAs above their architectural limits, and a malformed Sv39 2 MiB leaf with a
misaligned physical base. The expected stage-specific page-fault or
guest-page-fault contract was observed.

`make translation-permissions` passed 36 fresh-environment permission cases:
16 stage-1 loads, 11 stage-1 stores, and nine two-stage loads. The independent
truth table covered Sv39/Sv48 U/S pages, SUM, MXR, missing A/D, VSUM/VMXR, and
G-stage MXR/A selection. Passing stores committed and matched exact scalar
readback; faulting stores reported `StorePageFault`, issued no DCache/Uncache
request, and balanced the SQ through explicit cancellation.

`make fp-loads` passed cacheable 32-bit and 64-bit FP destination transactions.
The 32-bit result was checked as a NaN-boxed 64-bit value, with integer RF
write disabled and FP write enabled in the observed writeback.

## Historical Frozen Eight-Hour Acceptance

This record predates the `random-stress` controller addition. It remains a
valid historical result for the frozen executable listed above; after any
controller change, `verify-final-results` must be regenerated before this
record is treated as the current acceptance artifact.

On 2026-09-04 (Asia/Shanghai), the reviewed harness completed the final
frozen `random-mixed` campaign:

- run id: `9d0fdae3136e4330b49dd4694cab3cb6`;
- requested duration: 28,800 seconds; measured elapsed time:
  28,960.984777 seconds;
- 1,201/1,201 continuous seeds passed, from seed 1 through seed 1,201;
- 19,677,184 mixed actions completed, with 16,384 actions per seed;
- every result used eight workers, backpressure enabled, and complete RTL
  SHA-256 `0b6c6aa34dc8aa148d0f6da91091df58e6622415136e60baf985d7560824e7e3`;
- no scoreboard, assertion, timeout, process, coverage-gate, or queue-accounting
  failure occurred.

Artifact: `build/memblock/final-frozen-8h-16384.json`, SHA-256
`c104f21bc2dc9198f5987ed626b046fd058519b18de4464b1809060c14a1b216`.
The independent verifier reported `MEMBLOCK_REGRESSION_ARTIFACT_PASS` for
seeds `1..1201` and verified the schema-2 completion marker, frozen runtime,
controller hashes, finite timing, continuous seeds, per-seed coverage,
backpressure, and exact LQ/SQ accounting. The final acceptance target also
passed the dedicated dirty ReleaseData, repaired known-bug sentinel, and
32-seed boundary-hunt gates.

## Focused Runs

Deterministic focused and constrained-random evidence are tied independently to
their recorded RTL/runtime hashes below.

## Current Harness and RTL Fixes

The rebuilt harness freezes all acceptance inputs after the stale-runtime issue
was removed. `random-mixed` randomizes scalar and
vector data, widths, masks, `vl/vstart`, legal address classes, vector modes,
issue order, store address/data order, and inter-window delays while preserving
explicit interface constraints. The boundary diagnostic records all generated
parameters and verifies the independent Sv39x4 GPA oracle.

Latest focused correction evidence:

- 104 Python unit tests and the complete port/SVA/filelist checks pass;
- the common constrained-random interface passed an override run whose tail
  enabled only scalar loads (`seed=29`, 256 actions): actual constrained
  operations were `155,0,0,0,0,0,0,0`, and all 155 locality selections used
  the requested hot set;
- the final frozen runtime passed the minimum accepted `spec` run (`seed=1`,
  256 actions) in 11,490 cycles. It produced all eight operation classes,
  exactly four heterogeneous overlap windows, six TLB flushes, all four DCache
  latency buckets, and a 387-cycle maximum response delay. The independent
  verifier accepted the frozen hashes, exact command options, coverage fields,
  and queue accounting; artifact SHA-256 is
  `be0646adb73e281ece4b2461f933972be8f4fba4abce2c0e03ecf808498102eb`;
- one final-frozen comparison used the same `random-mixed` generator for two
  4,096-action seeds under each shipped preset (24,576 total actions). All six
  seeds passed independent artifact, command-replay, backpressure, coverage,
  and queue-accounting verification. The constrained tails resolved to:
  `coverage=2281,1141,2279,1140,1140,3,4,2`,
  `spec=4916,2086,361,168,351,35,39,34`, and
  `corner=1671,1037,1619,1097,1041,473,533,519` for scalar load/store,
  vector load/store, prefetch, atomic, NC, and MMIO respectively. `coverage`
  formed 569 overlap windows per seed with compact delays; `spec` reached a
  400-cycle response and 158 TLB flushes; `corner` reached 396 cycles, 312 TLB
  flushes, and 413 dirty ReleaseData beats. Artifact SHA-256 values are
  `5655306119aa50a5e02cba60d1b00c4bc6086d8f6380a68486eb15b64d8f19f4`
  (`coverage`),
  `38e1b9628a8367086fd765fb354e7f7c3af40ba1d452b4e81133bde883ad59c7`
  (`spec`), and
  `01881180f71972ee4939ad0afd3dfeb6155995ff7d0efe0c56431d948df97290`
  (`corner`);
- `random-mixed --seed 31 --transactions 16384 --constraints spec` passed
  16,384 actions in 577,224 cycles. Its constrained tail produced 9,971 scalar
  loads, 4,187 scalar stores, 756 vector loads, 394 vector stores, 755
  prefetches, 86 atomics, 63 NC accesses, and 71 MMIO accesses. It observed 313
  TLB flushes and DCache latency buckets `1380,263,93,112`, with a 394-cycle
  maximum;
- `random-mixed --seed 37 --transactions 4096 --constraints corner` passed
  4,096 actions in 193,043 cycles. It included 223 atomics, 218 NC accesses,
  257 MMIO accesses, 144 TLB flushes, 218 dirty ReleaseData beats, and all four
  response-latency buckets, with a 396-cycle maximum;
- `atomic-dchannel-errors` passes on current complete RTL hash
  `774dd52e91209904f30e4761d6e46f2fcc547b15b34f519c4c333aeb841b8cf9`;
- neighboring `atomic-contracts`, `dcache-errors`, `uncache-errors`,
  `mmio-contracts`, `cbo-zero-contracts`, and `reset-recovery` also pass on
  that hash;
- older full-mode and long constrained-random runs are retained below as
  superseded evidence and will not be described as current-RTL acceptance.

The first attempts at the two constrained runs above found a UT reference-model
bug at dirty atomic line `0x802a0100`: AMO old-value checking and the local AMO
model advanced, but the architectural reference memory did not. A later
ReleaseData was therefore compared with stale initialization data. The fix
updates only the architectural reference after a successful AMO, leaving bus
memory unchanged until the checked ReleaseData arrives. Both original seeds
then passed without weakening the ReleaseData checker. See
[`CONSTRAINED_RANDOM_ATOMIC_RELEASE_ORACLE.md`](CONSTRAINED_RANDOM_ATOMIC_RELEASE_ORACLE.md).

Frozen-boundary validation also exposed two UT framework defects, neither an
RTL failure. The old 128-action lower bound could not always fit four overlap
windows after the architectural prefix, and the offline verifier did not yet
recognize the new constraint command options. The lower bound is now 256 while
historical 128-action artifacts retain their enhanced coverage checks; command
options must exactly match campaign configuration. See
[`CONSTRAINED_RANDOM_MINIMUM_BUDGET.md`](CONSTRAINED_RANDOM_MINIMUM_BUDGET.md)
and
[`CONSTRAINED_RANDOM_COMMAND_REPLAY.md`](CONSTRAINED_RANDOM_COMMAND_REPLAY.md).

Neighboring current-binary checks also pass: `vector-addressing` (including the
cross-16-byte ROB-head contract), `atomic-contracts`, `dcache-errors`,
`uncache-errors`, `mmio-contracts`, and a 4,096-action legacy `random-stress`
seed that reached 12 outstanding scoreboard entries.

The current RTL contains three confirmed CPU/MemBlock bug fixes. The first
described here concerns Uncache: denied or corrupt D-channel responses set
exception bits in `UncacheEntry`, but the
LoadUnit S1 NC path previously replaced those incoming bits with a TLB-only
exception value. NC requests do not query the TLB but still traverse S1, so the
response error was erased before scalar writeback. The adapter now preserves all
source-generated exception bits and S1 ORs them with TLB exceptions. MMIO is a
separate S0-to-three-cycle metadata bypass path and is not implicated by this
reproducer. The
`uncache-errors` scenario passes both denied and corrupt cases on the regenerated
RTL, and the generated MemBlock boundary now retains the corresponding
exception-vector fields. The full reproducer and root-cause evidence are in
[`UNCACHE_DCHANNEL_ERROR.md`](UNCACHE_DCHANNEL_ERROR.md).

The second bug was found by extending `atomic-contracts` with misaligned
`AMOADD.D` and `AMOOR.W` cases. `AtomicsUnit` returned the expected
`storeAddrMisaligned=0x40`, but propagated `uop.rfWen=1` on that exceptional
writeback. The output now masks `rfWen` whenever `exceptionVec` is nonzero;
the full finding, before/after behavior, and scope are recorded in
[`ATOMIC_EXCEPTION_RF_WEN.md`](ATOMIC_EXCEPTION_RF_WEN.md).

The apparent third atomic D-channel bug was a UT oracle error. MainPipe
intentionally installs denied and corrupt atomic refills together with
`DCacheExtraMeta.error`; later loads hit the poisoned line and re-report
`loadAccessFault` or `hardwareError`. Exceptional data is non-architectural and
cannot establish an architecturally visible atomic side effect. The corrected
`atomic-dchannel-errors` contract covers both errors across all 22
refill-capable W/D LR/AMO/AMOCAS operations, 44 poisoned-line readback hits,
SC.W/D hits on both metadata types, and two clean error-lifetime recoveries. It
passed in 7,108 cycles with the exact 46 cold TileLink requests. Full analysis is in
[`ATOMIC_DCHANNEL_ERROR.md`](ATOMIC_DCHANNEL_ERROR.md).

During this iteration, a temporary harness defect changed the `mmio-contracts`
page mapping's PBMT=IO argument while adding the CBO.ZERO case. The existing
MMIO metadata oracle immediately rejected the mismatch (`isMMIO` expected 1,
actual 0); the call was restored, and the corrected scenario passed at cycle
818. This was a testbench configuration regression, not an RTL defect.

The first version of `l2-tlb-contracts` also assumed that warming an ordinary
DTLB port would make the independent L2-to-L1 requestor hit. The observed
`miss=1` response led to a source-level interface review: this requestor is an
independent L1 lookup whose miss is deliberately delegated to the external L2,
and MemBlock exposes no refill response input for it. The oracle was corrected
to require legal miss delegation, `no_translate` completion, and kill-without-
response; the final contract passes at cycle 236. This was an oracle assumption
defect, not a CPU/RTL defect.

The first `store-rdata-order` run also exposed an oracle defect rather than an
RTL failure. Its exceptional StoreUnit address writeback carried the expected
`storeAddrMisaligned=0x40` and `TriggerAction.None=15`, but the test had
incorrectly expected zero (the zero-initialized value belongs to the separate
standalone store-data adapter). The expectation now uses `kTriggerNone`; the
current run passes at cycle 342 with both SQ entries dequeued. The failure did
not indicate a data-ordering regression.

The third confirmed repair is the historical VS-non-leaf GPA defect in
`VMergeBuffer`. It is repaired by leaving
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
These 20 short seeds are retained as historical pre-current-worktree evidence;
the current executable and RTL hashes are recorded at the top of this file and
in the direct-run sections below. The historical executable SHA-256 is
`1b689baead6f77c05488a8feab6f8a00bc19aebcae884b103d8404d79fbb2f29`;
the historical complete RTL SHA-256 is
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
| Uncache D-channel errors | Pass | One denied and one corrupt response each reached scalar exception writeback; two uncache requests |
| Uncache widths/byte lanes | Pass | 29 scalar NC loads across all seven opcodes and legal 8-byte-beat lanes; 29 uncache requests, two request stalls, 90 response-delay cycles |
| MMIO metadata/error path | Pass | Cycle 818; one normal, one denied, and one corrupt PBMT=IO load plus one cold-TLB scalar PBMT=IO store; a separate non-DebugModule `c=0` PMA load/store pair passed and a guarded DebugModule PMA access produced `LoadAccessFault` with no manager request; `dcache_requests=0`, four Uncache requests, exact load/store metadata, and SQ retirement matched |
| CBO.ZERO cache-line zeroing | Pass | Cycle 370; cacheable `0x7` CBO.ZERO used the StoreQueue/SBuffer `wline` path, survived one forced DCache A stall and four response-delay cycles, produced exact non-MMIO store metadata, and a pre-mirror cache readback returned an all-zero line; no Uncache request was emitted |
| Atomic operations and exception metadata | Pass | Cycle 1216; all 9 W-width and 9 D-width AMOs, AMOCAS.W/D compare success/failure, LR/SC success/failure, and all 7 forbidden D-width plus 3 forbidden W-width byte offsets; exceptional writeback carried `storeAddrMisaligned=0x40`, suppressed `rfWen`, and emitted no additional DCache request |
| Atomic D-channel errors | Pass | Cycle 7,108; 22 W/D LR/AMO/AMOCAS operations crossed with denied and corrupt; all 44 later loads hit poisoned lines and re-reported exact errors, four SC hits reported cached errors, two clean AMO recoveries passed, exceptional `rfWen` stayed suppressed, and exactly 46 cold requests were issued |
| L2-to-L1 DTLB boundary | Pass | Cycle 396; ordinary and prefetch requests returned legal L1 miss responses, `no_translate=1` completed without a translation/fault, `kill=1` produced no response for 128 cycles, 16 source IDs × two L2 hint polarities (32 pulses) were accepted without ghost traffic, PBMT stayed zero, and exported PMP/MMIO classification was observed; miss delegation to external L2 is explicit because MemBlock has no refill response input |
| Reset recovery | Pass | Cycle 170; repeated reset with outstanding translated traffic, explicit cancellation of one pre-reset LQ entry, and one post-reset survivor with no stale writeback |
| Store TLB-miss preservation | Pass | Cycle 156; two misses and two PTW requests; allocated SQ entry remained address-valid |
| DCache dirty release | Pass | Ten stores; two ReleaseData writebacks preserved |
| Redirect | Pass | Canceled miss suppressed; LQ slot reused |
| Queue pressure | Pass | Two legal 60-entry waves; 120 checked writebacks |
| Stateful mixed seed 1 | Pass | 96 actions; scalar/vector load/store, all scalar/vector address modes, `prefetch.i/r/w`, Sv39/Sv39x4, PBMT-NC, cross-forwarding, dirty ReleaseData, redirect recovery, six manager backpressure classes, exact LQ/SQ drain |

## Earlier Current-Worktree 32K Direct Runs

After the atomic exceptional-writeback fix and the store-rdata oracle
correction, the pre-matrix-summary executable completed an independent
32,768-action pair. Both runs used complete RTL SHA-256
`670cf5d399c55e40c9d51c70183315b4cdd730e73843543aec5789414558b846` and
executable SHA-256
`bb25d12211d34a93a666a2835a7e7c0db1f87b21e508b56341b5ca35a4c90d86`.

| Scenario | Result | Observed coverage |
| --- | --- | --- |
| `random-stress --seed 83 --transactions 32768` | Pass | 713,485 cycles; 32,298 TileLink requests; 12 maximum outstanding; 2,007 scalar misaligned operations; 5,461 forwarding overlaps; all required stress combinations; four RNG streams; 10,826 request-stall and 135,240 response-delay cycles; LQ/SQ `38072+0/38072`, `27150+0/27150` |
| `random-mixed --seed 89 --transactions 32768` | Pass | 933,358 cycles; 14,052 scalar, 4,670 prefetch, 4,683 scalar-store, 9,355 vector-load, and 4,677 vector-store writebacks; 15 PTW, two Uncache, and 10 ReleaseData transactions; nested translation, three exception waves, redirect, dirty data, four forwarding classes, all dispatch lanes, both vector stride signs, and all manager backpressure counters nonzero; LQ/SQ `71250+1/71251`, `33213+0/33213` |

## Superseded Pre-Clarification 64K Direct Runs

To extend the request count beyond the normal 16K/32K campaigns, the
pre-clarification executable completed a 65,536-action stress/mixed pair. Both runs
passed with complete RTL SHA-256
`b69e387eb081a3f311311079ade435206817c7c6a20bd8f3a5f11889ec1dcbf4`.
That RTL contained `8eedb3ad0`; these results are historical rather than
current-RTL acceptance evidence.

| Scenario | Result | Observed coverage |
| --- | --- | --- |
| `random-stress --seed 43 --transactions 65536` | Pass | 1,427,238 cycles; 64,740 TileLink requests; 12 maximum outstanding; 4,078 scalar misaligned operations; 10,923 scalar and 10,922 vector forwarding overlaps; four RNG streams; all required stress combinations; 21,278 request-stall and 271,007 response-delay cycles; LQ/SQ `76294+0/76294`, `54453+0/54453` |
| `random-mixed --seed 47 --transactions 65536` | Pass | 1,861,069 cycles; 28,098 scalar, 9,351 prefetch, 9,365 scalar-store, 18,714 vector-load, and 9,358 vector-store writebacks; 15 PTW, two Uncache, ten ReleaseData, nested translation, three exception waves, redirect, dirty data, all forwarding classes and dispatch lanes, both vector stride signs, and all six manager backpressure counters nonzero; LQ/SQ `143506+1/143507`, `66307+0/66307` |

## Superseded Pre-Clarification Full-Mode Sweep

All 46 scenarios then registered by the UT executable completed at least one
passing run on the superseded hash above: 39 deterministic modes and seven random
modes. The larger supplemental random runs were:

| Scenario | Result | Observed coverage |
| --- | --- | --- |
| `random-loads --seed 53 --transactions 8192` | Pass | 182,480 cycles; 8,192 checked writebacks across all seven scalar load operations and three issue lanes; 7,323 hits and 869 misses |
| `random-vector-loads --seed 59 --transactions 8192` | Pass | 218,030 cycles; 8,192 checked vector writebacks, 16,384 balanced LQ allocations/dequeues, 845 TileLink requests, 18 releases, all four EEWs, both lanes, mask states, `vstart`, full/partial `vl`, and aligned/split addresses |
| `random-boundary-hunt --seed 71 --transactions 4096` | Pass | 4,096 constrained-random VS-non-leaf boundary cases; zero failures |

The specialized scalar/vector forwarding scenarios also passed, but their
drivers intentionally clamp a single run to 48 and 24 transactions. High-count
forwarding pressure is supplied by `random-stress`, which observed 10,923
scalar and 10,922 vector forwarding overlaps in the 65,536-action run.

## Historical 32K Direct Runs

The rebuilt executable also completed one 32,768-action pair with independent
seeds and randomized manager backpressure. These are direct binary runs, not
short smoke tests. They used the pre-L2-contract harness binary
`74371d94790266646d4093a6c450fed4916cab85f0092593bad08e40829f3677`; the
complete RTL hash for that historical run was `c97a89cd...`. The subsequent
L2-only harness additions and the atomic exception fix changed the current
binary and RTL hash, which are recorded at the top of this file.

| Scenario | Result | Observed coverage |
| --- | --- | --- |
| `random-stress --seed 59 --transactions 32768` | Pass | 713,251 cycles; 32,289 DCache requests; 12 maximum outstanding; 2,051 scalar misaligned operations; 5,461 forwarding overlaps; 5,460 masked and 5,462 unmasked vector operations; four vector mode combinations; 10,766 request-stall and 134,830 response-delay cycles; LQ/SQ `38062+0/38062`, `27140+0/27140` |
| `random-mixed --seed 61 --transactions 32768` | Pass | 931,075 cycles; 14,056 scalar, 9,351 vector-load, 4,676 vector-store, and 4,684 store writebacks; 15 PTW and two Uncache requests; 10 ReleaseData writebacks; nested translation, three exception waves, redirect, five forwarding classes, four dirty lines, and all six manager backpressure counters nonzero; LQ/SQ `70554+1/70555`, `32874+0/32874` |

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
and verifies source hashes as controller inputs. The current eight-hour
artifact above is provenance-complete for this reviewed harness revision.

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
provenance do not satisfy the four-hour gate. The current frozen eight-hour
campaign above is the duration artifact accepted for the repaired RTL.

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

The final acceptance section is populated only after the current eight-hour,
16,384-action-per-seed
frozen campaign has completed and `make verify-final-results` has independently
validated its artifact. Historical baseline and mutation results above are
retained as evidence and are not claims about the repaired RTL.

## Historical Pre-Review Six-Hour Campaign

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
That historical campaign did not expose an additional CPU bug. The later
current-worktree Uncache finding is documented separately in
`UNCACHE_DCHANNEL_ERROR.md`; historical mutant results remain the evidence for
the four independently reproduced LSU defects listed above. Any future failure
should be triaged from its recorded seed and runtime provenance rather than
treated as a known-good result.
