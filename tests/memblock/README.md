# MemBlock Unit Verification

This directory verifies generated Kunminghu-v2 MemBlock with C++ transactions
and a Picker/Verilator model. It supplies decoded backend/frontend/control and
memory-manager traffic; it does not execute a full-core program. Byte memory,
page tables and transaction expectations are testbench-owned.

## Documentation

| Document | Current responsibility |
| --- | --- |
| [DESIGN.md](docs/DESIGN.md) | Design flow, IO functions, legal inputs and boundary ownership |
| [PORTS.md](docs/PORTS.md), [manifest](config/expected_ports.json) | Generated flattened interface/dimensions/idle policy |
| [IO_COVERAGE.md](docs/IO_COVERAGE.md) | Current stimulus/check paths and missing IO stimulus |
| [ORACLES.md](docs/ORACLES.md) | Implemented models/decision rules and missing oracles |
| [VERIFICATION_PLAN.md](docs/VERIFICATION_PLAN.md) | Objectives, point inventory, gaps/review/acceptance |
| [CONSTRAINED_RANDOM.md](docs/CONSTRAINED_RANDOM.md) | Knobs/profiles, scheduling, coverage/replay |
| [RESULTS.md](docs/RESULTS.md) | Current qualification/review scope |
| [TOOLCHAIN.md](docs/TOOLCHAIN.md) | Tools, lock resolution and diagnostics |

These describe the present tree, not a campaign diary. A capability is not
coverage completeness. The catalog has 90 scenarios: 89 directed-tests and one
constrained-random. Default benchmark selects 85; explicitly run the remaining
cmo-store-flush, redirect-store-flush, tag-ecc-ways, vector-fof-high and
vector-segment-noncanonical when qualifying those contracts.

## Harness Layout

| Path | Responsibility |
| --- | --- |
| cpp/memblock_main.cpp and scenarios/catalog.json | CLI and exact inventory |
| cpp/scenarios/*.inc and mixed_*.cpp | Directed sequences and common scheduler |
| cpp/random/ | Constraints and observational coverage |
| environment/model.inc and page_table_builder.* | Independent byte/address/permission rules |
| Environment agents/monitors/scoreboards | Data/identity/queue/protocol/transport checking |
| environment/dut_adapter.* | Generated-port boundary |
| scripts/, unit/, sv/ | Build/runtime/controllers, infrastructure tests, assertions |

## Build From A Clean Checkout

From XiangShan root:

    git submodule sync --recursive
    make init
    export NOOP_HOME="$PWD"
    make verilog CONFIG=DefaultConfig NUM_CORES=1 ISSUE=E.b JVM_XMX=40G
    test -s build/rtl/MemBlock.sv
    test -s build/rtl/filelist.f
    make -C tests/memblock bootstrap-picker JOBS=8
    make -C tests/memblock prepare-rtl check-ports check-rtl unit
    make -C tests/memblock smoke JOBS=8

Tools include Java 17, Python 3, Make, C++20, CMake, SWIG and Verilator. Check
host memory/disk before RTL generation or native-model compilation.

Do not regenerate ports merely to silence mismatch. For intentional reviewed
interface changes, run ports and review generated manifest/C++/SVA/adapters
and IO inventory together. check-docs validates inventory/schema, not prose
semantics. Scala bundle fields pruned from generated ports are not controllable.

C++-only edits use make -C tests/memblock picker-harness JOBS=8. Source copies
are synchronized before rebuilding. After RTL/configuration/generation-option
changes, regenerate root out/ and build/rtl and rebuild prepared/exported
model. Make prerequisites do not encode every configuration dependency; an
interface check or familiar binary path is not source-to-model provenance.

## Focused Tests

From XiangShan root:

    make -C tests/memblock vector-segment-noncanonical
    make -C tests/memblock translation-fence-all
    make -C tests/memblock translation-inflight-context-all
    make -C tests/memblock known-bug-sentinel
    build/memblock/picker/UT_MemBlock/build/UTMemBlock_example --list-tests

The sentinel checks a current split guest-fault success signature; its target
name does not imply an open RTL defect. Segment noncanonical counts both
exceptional data/fix completions in their direction. Its lack of permissive
canonical-alias controls remains an oracle limitation.

## Constrained Random And Regression

coverage, spec, corner and cmo-stress share one generator. Examples:

    make -C tests/memblock constrained-random SEED=1 ACTIONS=4608 CONSTRAINTS=spec
    make -C tests/memblock constrained-random SEED=2 ACTIONS=4608 CONSTRAINTS=coverage CONSTRAINT='concurrent=250 latency=compact'
    make -C tests/memblock regression REGRESSION_JOBS=8 SEEDS=8 ACTIONS=16384

Sparse coverage does not cause functional FAIL. A 256/1024-action default
profile does not reach heterogeneous windows because serial budget is reserved;
inspect observed counters. One action is not necessarily one uop/bus request.

benchmark-tests selects 85, not all 90. final-acceptance composes unit,
benchmark, sentinel, a 48h duration campaign and eight million-action seeds.
See actual configured budgets and verifier gaps in VERIFICATION_PLAN.md.
Return-code PASS does not establish full coverage closure.

## Parallel Work And Shared State

One build owner controls root out/build RTL, prepared RTL, tracked generated
interfaces, Picker/model/harness and canonical runtime per checkout. Build
writers cannot run concurrently with users of mutable paths. Alternate RTL
or mutation experiments need private outputs/worktrees. The Picker lock is
not a universal artifact lock.

Freeze once, then use direct runtime/script invocations:

    make -C tests/memblock freeze-runtime JOBS=8
    python3 tests/memblock/scripts/run_regression.py --runtime-metadata build/memblock/runtime/runtime.json --output build/memblock/my-campaign/results.json --jobs 8 --seeds 8 --actions 16384 --constraints spec

Every concurrent controller needs a different parent output directory, not
merely a different JSON filename. coverage-reports uses controller-local
scenario/seed/index names and collides within a parent. Use unique trace
files. Never replace an active frozen runtime. Budget all process counts
together. Workers must not use Make targets that may rebuild prerequisites.

Runtime metadata checks paths/sizes and local dependency resolution; it does
not establish source-to-model identity. Keep source/config/tool revision and
build commands with the campaign. Frozen and mutable binaries may expose
different scenario inventories.

## Reproduction

Replay the result command with its runtime, scenario/seed/actions/profile and
overrides. From XiangShan root:

    LD_LIBRARY_PATH="$PWD/build/memblock/runtime" build/memblock/runtime/memblock_sim --test constrained-random --seed 1 --actions 4608 --constraints spec

Passing result entries retain
summaries/report paths; full stdout is retained for failures/timeouts rather
than every pass. Save stdout explicitly if a full transcript is required.
A running marker replaces the result at controller launch; incomplete/timeout
artifacts are not accepting campaigns.

MEMBLOCK_MIXED_PROGRESS_ACTIONS=64 adds action/cycle checkpoints.
MEMBLOCK_TRACE_ADDR_LO/HI logs architectural reference-memory history.
Internal mem_direct/FST traces are secondary diagnostics; no temporary RTL
printf and no internal-derived expected values. Use unique private artifacts.
MEMBLOCK_FORCE_DIRTY_OWNERSHIP_REJECT_ONCE is harness-only cleanup injection,
not evidence of natural RTL ownership behavior.

Exclude illegal backend traffic, model/manager/oracle errors and stale builds
before classifying RTL. A shorter passing prefix does not prove a longer
timeout is only slow. See ORACLES.md Failure Classification.
