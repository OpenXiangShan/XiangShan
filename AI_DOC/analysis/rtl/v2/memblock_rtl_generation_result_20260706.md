# V2 memblock RTL generation result

## Context

This document records the V2 memblock RTL generation attempts for the
`mem_ut_uvm_v2` migration on 2026-07-06.

Branch context:

```text
worktree: /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2
branch: mem_ut_uvm_v2
base: origin/kunminghu-v2
base commit: 2acbf327cf7fb514593acc00d4c41117ec499e08
local HEAD when reviewed: d555ee14a8418264c79a1e5f014cb5a040f6a46b
```

## Generation Entry

The V2 generation entry added by this migration is:

```text
scripts/generate_memblock_rtl.sh
src/main/scala/top/MemBlockTop.scala
```

The script uses the V2 build profile:

```text
build system: build.sc
default config: KunminghuV2Config
firtool: build.sc resolver by default
main class: top.MemBlockTopMain
expected output dir: build_memblock/rtl
```

It intentionally does not require the V3-only `firtool-1.135.0`,
`build.mill`, `TLConfig`, or V3 RTL-generation assumptions.

## Attempt 1: script stopped before mill

Command:

```bash
./scripts/generate_memblock_rtl.sh
```

Result:

The attempt stopped before invoking `mill` because required V2 submodules were
not initialized:

```text
error: required submodules are not initialized:
  ChiselAIA
  ChiselIOPMP
  coupledL2
  difftest
  fudian
  huancun
  openLLC
  ready-to-run
  rocket-chip
  utility
  yunsuan
hint: run git submodule update --init --recursive before generating V2 RTL
```

No V2 RTL was generated in this attempt.

## Attempt 2: submodule initialization failed

Command:

```bash
git submodule update --init --recursive
```

Result:

Submodule initialization did not close. The flow failed while processing the
`ready-to-run` dependency, so the worktree still did not have a complete V2
build-source state for RTL generation.

This means the first failure was not only a script precheck issue. The V2
submodule dependency graph itself was not fully initialized in this worktree.

## Attempt 3: script entered mill but build source was incomplete

Command:

```bash
./scripts/generate_memblock_rtl.sh
```

Result:

After the partial submodule update, the script progressed far enough to enter
the `mill` flow, but the V2 build still failed. The reported failures were:

```text
ready-to-run/.../modules/ready-to-run is not a git repository
missing rocket-chip/cde/common.sc
missing rocket-chip/hardfloat/common.sc
missing rocket-chip/common.sc
```

The failure category is therefore V2 submodule/build-source closure, not a
confirmed RTL elaboration, firtool, Verilog emission, or UVM compile failure.

## Output Status

V2 RTL was not generated successfully.

The required success files were not proven to exist as non-empty generated V2
artifacts:

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
```

If later V2 generation produces equivalent files with different names, the V2
profile and `mem_ut/ver/ut/memblock/cfg/rtl.f` must be updated in the same
change.

## Conclusion

The V2 RTL generation flow has been migrated into the branch: the script entry,
V2 `MemBlockTopMain`, and V2 profile now exist.

The V2 RTL itself has not been generated successfully. The current blocker is
that the V2 submodule and build-source state is not closed, specifically around
`ready-to-run` and required `rocket-chip` build files. Because no authoritative
V2 Verilog was generated, this branch should not continue to `eda_compile` yet.

Next work should first close the V2 submodule/build-source state, then rerun
`./scripts/generate_memblock_rtl.sh`, then check the generated Verilog outputs.
Only after that should remote VCS compile be attempted.
