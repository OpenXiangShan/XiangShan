# MemBlock Random Verification

This directory contains a block-level verification environment for the generated
Kunminghu-v2 `MemBlock`. It uses Picker/XS-MLVP for the C++ DUT wrapper and keeps
the testbench source independent from generated build products.

The current environment verifies scalar and 128-bit vector load/store behavior,
all vector load and store address modes independently, software prefetch, scalar/vector misalignment and
cross-page splits, the virtual load and store queues, Sv39 and two-stage
Sv39x4 translation, exact guest-page-fault metadata, DCache misses/refills and
dirty replacement, all four store-forwarding directions, PBMT=NC, redirects,
and queue pressure. MMIO, atomics, CBO/CMO, HLV/HLVX/HSV, VSegment, and
manager-originated probes remain explicit boundary gaps; they are not silently
randomized as though they were legal cacheable flows.

## Architecture

```text
load/store sequences -> generated lane adapters -> MemBlock
                                                 |       |
                              writeback monitors <-       -> TileLink agent
                                      |                       |
                                  scoreboards             sparse memory
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
- byte-addressed reference memory and load data formatting model;
- scalar load/prefetch, scalar store, and vector memory scoreboards;
- byte-accurate vector masking, old-destination, and SQ-forwarding models;
- deterministic transaction and functional coverage reporting.

The correctness contracts are cataloged separately in
`docs/ORACLES.md`. `docs/VERIFICATION_PLAN.md` contains the complete test-point
inventory, including explicit planned gaps for MMIO, atomics, CBO/CMO,
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

## Prerequisites

The XiangShan RTL and `build/rtl/filelist.f` must already exist. Picker may be
bootstrapped at its pinned revision or supplied explicitly:

```sh
cd tests/memblock
make bootstrap-picker JOBS=8
```

For an existing Picker binary:

```sh
export PICKER=/path/to/picker
```

## Focused Tests

```sh
make ports prepare-rtl check-ports check-rtl unit
make smoke PICKER="$PICKER" JOBS=8
make pin-space PICKER="$PICKER" JOBS=8
make single-load PICKER="$PICKER" JOBS=8
make scalar-misaligned PICKER="$PICKER" JOBS=8
make misaligned-stores PICKER="$PICKER" JOBS=8
make exception-contracts PICKER="$PICKER" JOBS=8
make two-stage-translation PICKER="$PICKER" JOBS=8
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
make random-mixed PICKER="$PICKER" JOBS=8 SEED=1 TRANSACTIONS=512
```

`vector-guest-fault-split` is the deterministic regression for the historical
VS-non-leaf GPA bug. Before the RTL fix it reproduced `0x94001808` instead of
the independently walked `0x94001800`; the repaired RTL must report
`0x94001800`, and `known-bug-sentinel` checks that repaired state explicitly.

The first Picker build compiles the Verilated model. Subsequent targets reuse it
and only rebuild the small C++ harness when its sources change.

## Random Regression

The normal regression includes the focused random scenarios and `random-mixed`.
The mixed scenario keeps heterogeneous transactions outstanding in one
simulation. Its constrained-random tail enqueues scalar load, scalar store,
vector load, vector store, and software prefetch in the same rolling window,
then randomizes issue order, store address/data order, and vector mode before a
bounded drain. It includes simultaneous scalar/vector issue, every scalar width,
every vector EEW and every load/store address mode independently, scalar/vector misalignment, software
`prefetch.i/r/w`, both cross-forwarding directions, Sv39 and Sv39x4 cold/warm
translation, a vector guest-page fault with exact VA/GPA metadata, PBMT=NC,
dirty same-set replacement, redirect/reallocation, and randomized DCache/PTW/
uncache backpressure. Every seed must meet its own coverage and final
LSQ-accounting gates.

```sh
make regression PICKER="$PICKER" \
  REGRESSION_JOBS=8 SEEDS=32 TRANSACTIONS=2000 FORWARD_TRANSACTIONS=48 \
  MIXED_TRANSACTIONS=512
```

The JSON result defaults to `build/memblock/regression.json` and includes every
command, seed, scenario, simulator output, elapsed time, executable hash, full
ordered-filelist RTL hash, and aggregate status. The run fails if the hash
reported by any simulation differs from the prepared RTL metadata.

For a time-based campaign, the regression targets first freeze
the executable, Verilated model, and pinned xspcomm library into
`build/memblock/runtime`. It verifies that both shared libraries resolve from
that directory, makes all three artifacts read-only, and records their SHA-256
values plus every resolved system-library hash in `runtime.json`.

```sh
make extended-regression PICKER="$PICKER" \
  REGRESSION_JOBS=8 DURATION_SECONDS=21600 MIXED_TRANSACTIONS=4096 \
  REGRESSION_OUTPUT="$PWD/../../build/memblock/extended-regression.json"
```

The final acceptance campaign is six hours of the fully mixed scenario. Each
seed requests 4096 actions and contains repeated five-class overlap windows:

```sh
make final-regression PICKER="$PICKER" \
  REGRESSION_JOBS=8 DURATION_SECONDS=21600 TRANSACTIONS=1000 \
  MIXED_TRANSACTIONS=4096 \
  REGRESSION_OUTPUT="$PWD/../../build/memblock/final-frozen-6h.json"
```

The runner stops submitting new work only after the requested duration has
elapsed. Already running seeds are allowed to finish, so the recorded wall time
is at least the requested duration. Any failing or timed-out seed stops further
submission but preserves all completed results in JSON.

`FORWARD_TRANSACTIONS=48` is the default requested scalar forwarding level;
the vector forwarding scenario is capped at 24 transactions per invocation to
avoid reusing an LSQ pointer within a focused scenario. Long-duration pressure
comes from consecutive seeded invocations. The verifier separately checks the
requested command value and these bounded completed counts.

Before and after the campaign, the runner verifies the frozen artifacts,
system libraries, runner source, RTL metadata, and the C++/SVA/config controller
files listed in `CONTROLLER_FILES`. A hash change in any of them makes the
result fail. The runtime may also be prepared and inspected directly:

```sh
make freeze-runtime PICKER="$PICKER"
LD_LIBRARY_PATH="$PWD/../../build/memblock/runtime" \
  ldd ../../build/memblock/runtime/memblock_sim
```

The old four-hour artifact was overwritten by a one-second development smoke
run and is intentionally non-accepting. `make verify-extended-results` should
reject the current stale file because its duration and published hash do not
meet that historical gate. Use the final six-hour artifact for current
acceptance:

```sh
make verify-extended-results
```

```sh
make verify-final-results
```

## Reproduction

A load failure can be replayed directly:

```sh
build/memblock/picker/UT_MemBlock/build/UTMemBlock_example \
  --test random-loads --seed 17 --transactions 2000
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
  --test random-mixed --seed 17 --transactions 64
```

A campaign seed should be replayed from its recorded frozen runtime:

```sh
LD_LIBRARY_PATH="$PWD/../../build/memblock/runtime" \
  ../../build/memblock/runtime/memblock_sim \
  --test random-mixed --seed 17 --transactions 64
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
