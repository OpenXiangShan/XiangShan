# MemBlock Random Verification

This directory contains a block-level verification environment for the generated
Kunminghu-v2 `MemBlock`. It uses Picker/XS-MLVP for the C++ DUT wrapper and keeps
the testbench source independent from generated build products.

The current environment verifies modeled scalar and 128-bit vector load/store
behavior, all vector load and store address modes independently, software
prefetch, scalar/vector misalignment and cross-page splits, the virtual load and
store queues, Sv39/Sv48 and all four VS/G-stage two-stage translation pairs,
exact guest-page-fault metadata, DCache misses/refills and a byte-checked
dirty-pressure phase, all four store-forwarding directions, PBMT=NC, redirects,
and queue pressure. D-width AMOs (ADD/XOR/AND/OR/SWAP/MIN/MAX, signed and
unsigned) plus LR/SC are covered through the atomic unit, including old-value
writeback, AMOCAS compare success/failure, reservation success/failure, and
cache visibility. The L2-to-L1 DTLB and L2 hint input boundaries are checked for
legal miss/cancel/metadata behavior. Uncache denied and corrupt D-channel responses are checked
through scalar exception writeback. PBMT=IO MMIO metadata and error propagation
are covered; MMIO device side effects, CMO CLEAN/FLUSH/INVAL, HLV/HLVX/HSV, VSegment, and
manager-originated probes remain explicit boundary gaps; they are not silently
randomized as though they were legal cacheable flows.

The MemBlock-facing L2-to-L1 DTLB request/response boundary is also exercised.
`l2-tlb-contracts` checks request-field acceptance, L1 miss responses for both
ordinary and prefetch requests, PBMT/fault-field legality, and the exported PMP
classification. A miss is intentionally handed back to the external L2 TLB;
the MemBlock top level has no refill response input for this port.

## Architecture

```text
load/store sequences -> generated lane adapters -> MemBlock
                                                 |       |
                              writeback monitors <-       -> TileLink agent
                                      |                       |
                                  scoreboards       independent reference
                                                           + bus memory
                                      |
                              functional coverage
```

The reusable C++ components are in `cpp/memblock_env.hpp`:

- reset and cycle control with registered Picker clock;
- typed LSQ, scalar load/store, vector load/store, and software-prefetch drivers;
- coherent TileLink A/D memory agent with randomized ready/response delay;
- PTW TileLink agent with independent request and response backpressure;
- uncache TileLink agent with forced-first and randomized request/response stalls;
- TileLink C-channel Release/ReleaseData capture, ReleaseAck, and writeback;
- separate byte-addressed architectural reference and bus backing memories;
- ISA load formatting plus byte-exact checks on every dirty ReleaseData beat;
- scalar load/prefetch, scalar store, and vector memory scoreboards;
- byte-accurate vector masking, old-destination, and SQ-forwarding models;
- deterministic transaction and functional coverage reporting.

The correctness contracts are cataloged separately in
`docs/ORACLES.md`. `docs/VERIFICATION_PLAN.md` contains the complete test-point
inventory, including explicit planned gaps for MMIO device side effects,
reservation interference and full atomic alignment crosses, CMO CLEAN/FLUSH/INVAL,
VSegment, hypervisor accesses, PMP/PMA matrices, coherence probes, error
injection, concurrent exception priority, and four-state behavior. A passing
cacheable mixed campaign must not be interpreted as verification of those
planned rows.

The structure follows UVM responsibilities without requiring a SystemVerilog
class runtime:

| UVM responsibility | MemBlock implementation |
| --- | --- |
| Sequence/sequencer | Focused scenarios and deterministic seeded generators in `memblock_main.cpp` |
| Driver | Generated typed LSQ, scalar, vector, prefetch, redirect, and commit adapters |
| Active agent | Coherent DCache, PTW, and uncache memory agents with independent backpressure |
| Monitor | Per-cycle writeback, queue-dequeue, TLB-feedback, and TileLink handshake sampling |
| Reference model | Byte-addressed `SparseMemory` plus scalar extension, vector mask, and forwarding functions |
| Scoreboard | Scalar load/prefetch, scalar store, and vector memory scoreboards with duplicate rejection |
| Coverage | Per-seed operation/lane/cache/TLB/queue/redirect/forwarding gates and JSON summaries |
| Assertions | Generated producer stability binds plus all-entry StoreQueue TLB-miss preservation checks |

`scripts/generate_cpp.py` derives repetitive lane accessors from the checked RTL
manifest. No test hard-codes a flattened port without that interface first being
present in `config/expected_ports.json`.

## Build From A Clean Checkout

The UT does not elaborate Chisel itself. It consumes the split SystemVerilog
generated by the XiangShan top-level build, so there are two distinct build
stages: generate XiangShan RTL from the repository root, then build the
MemBlock model from `tests/memblock`.

The environment used for the recorded results was Ubuntu 24.04 with JDK 17,
Mill 0.12.3, Python 3.12, GNU Make 4.3, GCC/G++ 13.3, CMake 3.28, and Verilator
5.048. These are a known-working baseline, not all strict minimum versions.
Picker requires SWIG 4.2 or newer; `bootstrap-picker` uses a system SWIG when
available and otherwise attempts a local extraction with `apt-get download`
and `dpkg-deb`.

From a clean clone of the branch under test, initialize the submodules and
generate exactly the RTL configuration used by this environment:

```sh
cd /path/to/XiangShan
git submodule sync --recursive
make init
make verilog CONFIG=DefaultConfig NUM_CORES=1 ISSUE=E.b JVM_XMX=40G
test -s build/rtl/MemBlock.sv
test -s build/rtl/filelist.f
```

`make verilog` invokes `top.TopMain`, resolves the matching Firtool through the
Mill build, emits split SystemVerilog under `build/rtl`, and generates the SRAM
wrapper files referenced by `filelist.f`. On the machine used for this work it
took roughly 3-5 minutes and reached 20-27 GiB resident memory. Allow at least
32 GiB of usable RAM (more is preferable with `JVM_XMX=40G`) and 10 GiB of free
disk for RTL, Picker, the Verilated model, and intermediate objects. Reducing
`JVM_XMX` is possible on another host, but has not been qualified here.

Then build the pinned Picker toolchain and the MemBlock simulator:

```sh
make -C tests/memblock bootstrap-picker JOBS=8
make -C tests/memblock prepare-rtl check-ports check-rtl unit
make -C tests/memblock smoke JOBS=8
```

The first `smoke` invocation performs the expensive Picker export and Verilator
compile. Later test targets reuse
`build/memblock/picker/UT_MemBlock/build/UTMemBlock_example` and normally only
rebuild the small C++ harness after a testbench source change. A successful
bootstrap records the exact Picker/xcomm revisions and executable path in
`build/memblock/tools/picker.json`; the generated RTL identity is recorded in
`build/memblock/rtl.json`.

For an existing Picker binary, skip `bootstrap-picker` and pass it explicitly:

```sh
make -C tests/memblock smoke PICKER=/path/to/picker JOBS=8
```

Do not reuse `build/rtl` after changing `CONFIG`, `ISSUE`, generation flags, or
the RTL commit. Those command-line choices are not all Make prerequisites.
Regenerate the RTL in a clean build tree, then rerun `prepare-rtl`; otherwise a
stale `filelist.f` can silently select the wrong design until the hash/port
checks reject it. Do not run `make ports` merely to make a mismatch disappear:
the tracked port manifest should change only when an intentional MemBlock
interface change has been reviewed.

Common setup failures are:

- `build/rtl/MemBlock.sv` or `build/rtl/filelist.f` missing: run the root-level
  `make verilog` command above, not only a target under `tests/memblock`.
- Mill/Chisel import or missing-module errors: rerun `make init` and confirm the
  checkout has the branch's recorded submodule commits.
- Java heap allocation or an OOM kill: free memory or move the RTL generation
  to a host with at least the capacity noted above; lowering `JOBS` does not
  materially lower the Chisel elaboration heap.
- `Picker requires SWIG >= 4.2`: install SWIG 4.2+ on `PATH`, or ensure
  `apt-get download` and `dpkg-deb` are available to the bootstrap script.
- Picker export reports a missing RTL file: inspect paths in
  `build/rtl/filelist.f`; relative entries are resolved from `build/rtl`, and
  all listed `.sv`/`.v` files must belong to the same generation.
- `check-ports` reports a stale manifest: first confirm the expected branch,
  `DefaultConfig`, and RTL commit. Treat a genuine port delta as a design
  review item rather than regenerating the checked manifest blindly.

## Focused Tests

```sh
make ports prepare-rtl check-ports check-rtl unit
make smoke PICKER="$PICKER" JOBS=8
make pin-space PICKER="$PICKER" JOBS=8
make single-load PICKER="$PICKER" JOBS=8
make fp-loads PICKER="$PICKER" JOBS=8
make trigger-contracts PICKER="$PICKER" JOBS=8
make metadata-contracts PICKER="$PICKER" JOBS=8
make dcache-errors PICKER="$PICKER" JOBS=8
make uncache-errors PICKER="$PICKER" JOBS=8
make uncache-widths PICKER="$PICKER" JOBS=8
make mmio-contracts PICKER="$PICKER" JOBS=8
make cbo-zero-contracts PICKER="$PICKER" JOBS=8
make reset-recovery PICKER="$PICKER" JOBS=8
make atomic-contracts PICKER="$PICKER" JOBS=8
make atomic-dchannel-errors PICKER="$PICKER" JOBS=8
make scalar-misaligned PICKER="$PICKER" JOBS=8
make misaligned-stores PICKER="$PICKER" JOBS=8
make exception-contracts PICKER="$PICKER" JOBS=8
make l2-tlb-contracts PICKER="$PICKER" JOBS=8
make two-stage-translation PICKER="$PICKER" JOBS=8
make translation-matrix PICKER="$PICKER" JOBS=8
make translation-fence PICKER="$PICKER" JOBS=8
make translation-context PICKER="$PICKER" JOBS=8
make translation-bare PICKER="$PICKER" JOBS=8
make translation-faults PICKER="$PICKER" JOBS=8
make translation-permissions PICKER="$PICKER" JOBS=8
make translation-superpages PICKER="$PICKER" JOBS=8
make scalar-guest-fault PICKER="$PICKER" JOBS=8
make vector-guest-fault PICKER="$PICKER" JOBS=8
make vector-load PICKER="$PICKER" JOBS=8
make vector-split-load PICKER="$PICKER" JOBS=8
make vector-addressing PICKER="$PICKER" JOBS=8
make vector-store-forwarding PICKER="$PICKER" JOBS=8
make store-forwarding PICKER="$PICKER" JOBS=8
make store-rdata-order PICKER="$PICKER" JOBS=8
make store-tlb-miss-preserve PICKER="$PICKER" JOBS=8
make dcache-release PICKER="$PICKER" JOBS=8
make redirect PICKER="$PICKER" JOBS=8
make queue-pressure PICKER="$PICKER" JOBS=8
make random-mixed PICKER="$PICKER" JOBS=8 SEED=1 TRANSACTIONS=16384
make random-stress PICKER="$PICKER" JOBS=8 SEED=1 TRANSACTIONS=16384
```

`translation-matrix` exercises all four VS/G-stage combinations:
`Sv39->Sv39x4`, `Sv39->Sv48x4`, `Sv48->Sv39x4`, and `Sv48->Sv48x4`. It maps
the VS page-table pages through G-stage, checks a high-half Sv48 VA, and
requires the second access to reuse the cold translation without new PTW
requests.

`translation-fence` updates live stage-1 and nested leaves, then checks global
and selective `SFENCE.VMA`, selective `HFENCE.VVMA`, and global
`HFENCE.GVMA` visibility after the required refill. Same-ID ASID/VMID reuse
and outstanding-walk ordering remain separate coverage points.

`translation-context` checks five context families with 14 architectural
loads: direct Sv39-to-Sv48 mode/root switching, same-mode `satp` ASID/root,
`vsatp` ASID/root under a retained G context, `hgatp` VMID/root under a retained
VS context, and host/nested/host `virt` transitions. Each context maps the same
input address to distinct data so stale translation reuse is externally
observable.

`translation-superpages` walks 2 MiB and 1 GiB leaves in Sv39/Sv48 and their
Sv39x4/Sv48x4 G-stage equivalents, plus the Sv48 512 GiB leaf. Each case is
checked against the independent leaf-address oracle and an architectural load.

`translation-bare` covers stage-1 Bare, G-only, VS-only, and fully Bare
degenerations. It checks that the selected stage is bypassed exactly once and
that no stale page-table walk is required.

`translation-faults` executes a noncanonical Sv48 VA, Sv39x4 and Sv48x4 GPAs
above their architectural limits, a malformed non-aligned Sv39 2 MiB leaf, and
52 fresh-environment PTE encoding faults split evenly across Sv39/Sv48 stage-1
and Sv39x4/Sv48x4 G-stage. The shared encoding table crosses V/W/R, both ends
of reserved bits 60:54, PBMT=3, disabled PBMTE, exhausted L0, illegal non-leaf
U/A/D/PBMT/N fields, and an invalid NAPOT encoding. It checks the independent
walk's failing PTE/level, exact stage-specific exception and fault VA/GPA, and
that no faulting data request reaches DCache or Uncache.

`translation-permissions` executes 36 independent cases. Its table-driven
oracle covers Sv39/Sv48 U/S access, SUM, MXR, missing A/D, VS-stage VSUM/VMXR,
and G-stage MXR/A behavior for scalar loads and stores. Passing stores require
exact post-commit readback; faulting stores must not reach DCache or Uncache
and must balance through explicit SQ cancellation.

`fp-loads` exercises the separate FP destination-enable path for 32-bit and
64-bit load widths. The scoreboard requires no integer-register write and an
exact FP writeback payload for each transaction.

`trigger-contracts` programs a memory breakpoint through the top-level CSR
trigger interface and checks the breakpoint exception bit, trigger action, and
suppressed register writeback. LSQ enqueue metadata (`exceptionVec`, trigger,
and `flushPipe`) is driven explicitly on every transaction; scalar load/store
and vector writeback adapters compare the observable flush, RF-enable, and
MMIO/NCIO/perf debug metadata. Vector and misaligned-store trigger fields are
observed but are not forced to equal the enqueue value when their RTL path
legally regenerates the action.

`metadata-contracts` drives non-default RVC/FTQ/store-set/load-wait values on
the scalar issue interface and completes a load. The top-level `issueLda`
contract does not expose `exceptionVec`; LSQ retains enqueue exception bits for
its internal exception machinery, while page/access/guest-page faults are
recomputed from the TLB in S1. The generated enqueue adapter is covered by
unit tests for exception-vector bit mapping.

`dcache-errors` injects one denied and one corrupt DCache response and checks
the corresponding scalar load access-fault and hardware-error writebacks with
RF writes suppressed.

`uncache-errors` injects one denied and one corrupt Uncache response and checks
the exception contract through the PBMT=NC adapter. This test caught and now
guards the LoadUnit S1 path that previously discarded response-generated
exception bits. MMIO uses a distinct S0-to-three-cycle metadata bypass and is
not implicated by this reproducer. The complete reproducer and root-cause analysis are in
[`docs/UNCACHE_DCHANNEL_ERROR.md`](docs/UNCACHE_DCHANNEL_ERROR.md).

`uncache-widths` exercises all seven scalar load opcodes at every legal byte
lane for 8-, 16-, 32-, and 64-bit Uncache transfers. The manager returns the
complete 8-byte beat, checks the generated size/address/mask contract, and
applies deterministic request and response backpressure before the scalar
scoreboard checks sign/zero extension.

`mmio-contracts` maps a page as PBMT=IO and checks the MMIO load's direct
three-cycle metadata path: the load must bypass DCache, report `isMMIO=1` and
`isNCIO=0`, and preserve denied/corrupt response exceptions through writeback.
It also cold-misses and reissues a scalar PBMT=IO store, requiring one Uncache
Put, no DCache request, exact store writeback metadata, and SQ retirement. The
PBMT=IO page is backed by the DDR PMA region, so the store writeback's
`debug.isMMIO=0` denotes `memBackTypeMM=1`. A second bare-mode pair accesses the
SoC's non-DebugModule `c=0` PMA interval and requires physical PMA MMIO
classification for both load and store. A side-effecting device model remains
a planned boundary. `cbo-zero-contracts` drives the `CBO.ZERO` encoding through
the cacheable StoreQueue/SBuffer wline path under randomized DCache
backpressure, checks exact writeback metadata, and reads the resulting line
back before updating the reference mirror.

`reset-recovery` asserts reset again while a translated load has outstanding
traffic, accounts the canceled queue entry, then reconfigures translation and
requires a post-reset load to complete normally. It also rejects any stale
writeback from the canceled request.

`atomic-contracts` drives all currently exposed W/D-width AMOs, AMOCAS.W/D, and
LR/SC through the atomic store-address/data ports, checks old-value writeback,
compare success/failure, reservation success/failure, and then verifies cache
visibility through ordinary scalar loads. It also checks representative
misaligned D/W atomics for `storeAddrMisaligned`, suppressed exceptional
`rfWen`, and no DCache request. Atomic uops are intentionally not counted as
LSQ entries because the RTL routes them through `AtomicsUnit`.

`atomic-dchannel-errors` injects denied and corrupt responses into cold misses
for all 22 refill-capable W/D operations: LR, the nine AMO ALU operations, and
AMOCAS. Every operation checks the exact exception class, suppressed `rfWen`,
and the expected miss/hit request-count delta. Both denied and corrupt refills
are installed as poisoned lines; a later scalar load must hit without new
traffic and re-report `loadAccessFault` or `hardwareError`. SC.W/D checks the
same cached error metadata after LR.W/LR.D, and clean AMO/readback sequences
after each error-kind batch guard AtomicsUnit error lifetime. Exceptional data
is deliberately not compared because it is non-architectural. SC itself cannot
receive a cold-miss D response in this implementation: a missing line or usable
reservation makes MainPipe return SC failure before issuing TileLink traffic. See
[`docs/ATOMIC_DCHANNEL_ERROR.md`](docs/ATOMIC_DCHANNEL_ERROR.md).


`vector-guest-fault-split` is the deterministic regression for the historical
VS-non-leaf GPA bug. Before the RTL fix it reproduced `0x94001808` instead of
the independently walked `0x94001800`; the repaired RTL must report
`0x94001800`, and `known-bug-sentinel` checks that repaired state explicitly.

The first Picker build compiles the Verilated model. Subsequent targets reuse it
and only rebuild the small C++ harness when its sources change.

## Random Regression

The normal regression includes the focused random scenarios and `random-mixed`.
`random-mixed` is one configurable generator: realistic, balanced-coverage,
and corner-heavy directions are selected with the `spec`, `coverage`, and
`corner` constraint presets, and every field can be overridden without adding
another scenario implementation. See
[`docs/CONSTRAINED_RANDOM.md`](docs/CONSTRAINED_RANDOM.md) for the complete
interface, preset values, SPEC counter calibration, and coverage gates.
The mixed scenario keeps heterogeneous transactions outstanding in one
simulation. Its constrained-random tail enqueues scalar load, scalar store,
vector load, vector store, and software prefetch in the same rolling window,
then randomizes issue order, store address/data order, and vector mode before a
bounded drain. It includes simultaneous scalar/vector issue, every scalar width,
every vector EEW and every load/store address mode independently, scalar/vector misalignment, software
`prefetch.i/r/w`, both cross-forwarding directions, Sv39 and the currently
modeled Sv39x4 cold/warm translation, a vector guest-page fault with exact
VA/GPA metadata, PBMT=NC,
dirty same-set replacement, redirect/reallocation, and randomized DCache/PTW/
uncache backpressure. Every seed drives all six LSQ dispatch lanes and widths,
checks committed scalar/vector stores through architectural readback, validates
dirty ReleaseData before updating the separate bus memory, and meets bounded
coverage plus final LSQ-accounting gates. Manager-originated coherence traffic
remains outside this modeled boundary.

For example, these commands run the same generator in two directions:

```sh
make random-mixed PICKER="$PICKER" SEED=1 TRANSACTIONS=65536 \
  CONSTRAINTS=spec
make random-mixed PICKER="$PICKER" SEED=2 TRANSACTIONS=32768 \
  CONSTRAINTS=corner CONSTRAINT='tlb-flush=200 concurrent=750'
```

`extended-regression`, `final-regression`, and `long-final-regression` default
to `spec`; boundary hunts default to `corner`. `CONSTRAINTS` and `CONSTRAINT`
override those defaults. The ordinary `regression` and direct `random-mixed`
target default to `coverage` when no preset is supplied.

`random-stress` is the high-pressure constrained-random campaign. Each burst
builds one or two groups before any drain. A group contains independent and
forwarding scalar loads/stores, vector loads/stores, and a prefetch; issue order
is randomized subject only to the actual forwarding dependencies. The stress
driver checks byte overlays for younger scalar/vector loads, all scalar widths,
all vector EEWs, unit/strided/indexed-unordered modes, mask/vstart/vl shapes,
misaligned scalar addresses, both vector lanes, two cache regions, DCache
backpressure, and final LSQ conservation. Strided forwarding stores use
non-overlapping positive or negative strides; zero-stride loads remain covered
by the independent random-mixed vector-load phase because repeated-address
stores do not have a single deterministic forwarding order. Its coverage gate
uses independent SplitMix64-derived streams for traffic, shape, payload, and
scheduling, preserving exact seed replay while decoupling coverage dimensions. It
also requires at least ten simultaneous outstanding scoreboard entries and four nonzero
feature crosses derived from generated burst fields. Ordered-indexed vector issue remains in the
`random-mixed` baseline because the DUT requires older LSQ retirement before
that operation can be accepted.
This separate entry point is retained for compatibility with its historical
artifacts and burst-specific acceptance checks. New workload directions belong
in the common `random-mixed` constraint interface.

For a reproducible local pressure run:

```sh
make random-stress PICKER="$PICKER" JOBS=8 SEED=1 TRANSACTIONS=16384
```

For a long multi-seed campaign and machine-checked artifact:

```sh
make stress-regression PICKER="$PICKER" REGRESSION_JOBS=8 \
  DURATION_SECONDS=3600 STRESS_TRANSACTIONS=16384
make verify-stress-results PICKER="$PICKER" REGRESSION_JOBS=8 \
  MIN_DURATION_SECONDS=3600 STRESS_TRANSACTIONS=16384
```

```sh
make regression PICKER="$PICKER" \
  REGRESSION_JOBS=8 SEEDS=32 TRANSACTIONS=16384 FORWARD_TRANSACTIONS=48 \
  MIXED_TRANSACTIONS=16384
```

The JSON result defaults to `build/memblock/regression.json` and includes every
command, seed, scenario, simulator output, elapsed time, executable hash, full
ordered-filelist RTL hash, and aggregate status. The run fails if the hash
reported by any simulation differs from the prepared RTL metadata.

For a time-based campaign, the regression targets first freeze the executable,
Verilated model, pinned xspcomm library, and the exact prepared RTL metadata into
`build/memblock/runtime`. It verifies that both shared libraries resolve from
that directory, makes all four artifacts read-only, and records their SHA-256
values plus every resolved system-library hash in `runtime.json`. Duration
campaigns use the frozen `runtime/rtl.json`; rebuilding or removing the mutable
`build/memblock/rtl.json` cannot invalidate an otherwise unchanged run at
shutdown.

```sh
make extended-regression PICKER="$PICKER" \
  REGRESSION_JOBS=8 DURATION_SECONDS=21600 MIXED_TRANSACTIONS=8192 \
  EXTENDED_RESULT="$PWD/../../build/memblock/extended-mixed-frozen-6h-8192.json"
```

The final acceptance campaign is eight hours of the fully mixed scenario. Each
seed requests 16,384 actions and contains repeated five-class overlap windows:

```sh
make final-regression PICKER="$PICKER" \
  REGRESSION_JOBS=8 DURATION_SECONDS=28800 TRANSACTIONS=4096 \
  MIXED_TRANSACTIONS=16384 TIMEOUT_SECONDS=1800 \
  FINAL_RESULT="$PWD/../../build/memblock/final-frozen-8h-16384.json"
```

The runner stops submitting new work only after the requested duration has
elapsed. Already running seeds are allowed to finish, so the recorded wall time
is at least the requested duration. Any failing or timed-out seed stops further
submission but preserves all completed results in JSON. At launch, the output
is atomically replaced by a schema-2 `running` marker with a unique run id, so
an interrupted campaign cannot leave an older accepting artifact at that path.

The default scalar/vector random-load levels and mixed level are now 16,384
transactions per seed; this keeps ordinary regression pressure comparable to
the dedicated stress campaign. `FORWARD_TRANSACTIONS=48` remains the default
requested scalar forwarding level;
the vector forwarding scenario is capped at 24 transactions per invocation to
avoid reusing an LSQ pointer within a focused scenario. Long-duration pressure
comes from consecutive seeded invocations. The verifier separately checks the
requested command value and these bounded completed counts.

Before and after the campaign, the runner verifies the frozen artifacts,
system libraries, runner source, RTL metadata, the runtime-freeze script, and
the C++/SVA/config controller files listed in `CONTROLLER_FILES`. A hash change
in any of them makes the result fail. The verifier also requires the recorded
worker count to be eight, at least 128 complete seeds, finite timestamps, and a
result completion after the duration deadline. It rejects conflicting terminal
summaries and scenario/seed/count mismatches. The runtime may also
be prepared and inspected directly:

```sh
make freeze-runtime PICKER="$PICKER"
LD_LIBRARY_PATH="$PWD/../../build/memblock/runtime" \
  ldd ../../build/memblock/runtime/memblock_sim
```

`stress-regression` and `verify-stress-results` use the same
`STRESS_TRANSACTIONS` value for both the command and verifier configuration.
This prevents a 16,384-action stress artifact from being rejected by a stale
4,096-action verifier default. The 2026-09-05 failure that exposed both
hardening issues is recorded in
[`docs/REGRESSION_PROVENANCE_FAILURE.md`](docs/REGRESSION_PROVENANCE_FAILURE.md).

The old four-hour artifact was overwritten by a one-second development smoke
run and is intentionally non-accepting. `make verify-extended-results` should
reject the current stale file because its duration and provenance do not meet
the historical gate. The pre-review six-hour artifact and this eight-hour
artifact are historical evidence only after the current stress controller
changes. A new eight-hour mixed run must be generated before
`verify-final-results` can accept it:

```sh
make verify-final-results
```

## Reproduction

A load failure can be replayed directly:

```sh
build/memblock/picker/UT_MemBlock/build/UTMemBlock_example \
  --test random-loads --seed 17 --transactions 16384
```

A forwarding failure uses:

```sh
build/memblock/picker/UT_MemBlock/build/UTMemBlock_example \
  --test random-forwarding --seed 17 --transactions 48
```

Vector failures use the same seed contract with `random-vector-loads` or
`random-vector-forwarding`. A mixed failure is reproduced without any other
scenario:

```sh
build/memblock/picker/UT_MemBlock/build/UTMemBlock_example \
  --test random-mixed --seed 17 --transactions 16384
```

A campaign seed should be replayed from its recorded frozen runtime:

```sh
LD_LIBRARY_PATH="$PWD/../../build/memblock/runtime" \
  ../../build/memblock/runtime/memblock_sim \
  --test random-mixed --seed 17 --transactions 256
```

## Complete Pin Audit

`config/expected_ports.json` is the exact machine-checked inventory: 749 inputs
and 586 outputs (1,335 pins, representing 7,155 input bits and 5,434 output
bits). `make pin-space` derives directly from that inventory and therefore has
no handwritten omission list. With primary reset held active, it drives and
reads back every non-clock/non-primary-reset input using zero, all-one, and 254
independently seeded mixed patterns. Every output is sampled after every pattern
and folded into the reported digest. Normal reset and clock operation exercise
the remaining two inputs.

This sweep proves structural connectivity and gives each input bit both binary
values plus broad multi-bit combinations. It does not claim that arbitrary
reset-held combinations are legal protocol transactions. Semantic claims come
from the scalar/vector drivers, protocol assertions, TileLink agent, and
scoreboards described above. The generated per-pin inventory is also rendered
in `docs/PORTS.md` for review.

Do not use `XData::CSelf()` to read signal values. It returns the address of the
`XData` object. The environment uses `U()`, `B()`, and `GetBytes()` instead.

See `docs/VERIFICATION_PLAN.md` for the test matrix and `docs/RESULTS.md` for
recorded campaigns. `docs/HISTORICAL_BUG_AUDIT.md` gives the status and evidence
level for every 2026 commit touching `src/main/scala/xiangshan/mem`.
