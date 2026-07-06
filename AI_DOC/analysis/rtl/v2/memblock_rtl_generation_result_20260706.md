# V2 memblock RTL generation result

## Context

This document records the first V2 memblock RTL generation attempt for the
`mem_ut_uvm_v2` migration.

Branch context:

```text
branch: mem_ut_uvm_v2
base: origin/kunminghu-v2
base commit: 2acbf327cf7fb514593acc00d4c41117ec499e08
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
```

It intentionally does not require the V3-only `firtool-1.135.0`,
`build.mill`, `TLConfig`, or V3 `top.MemBlockTopMain` assumptions.

## Attempted Command

```bash
./scripts/generate_memblock_rtl.sh
```

## Result

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

## Follow-up

Before re-running V2 RTL generation, initialize the V2 submodules in the V2
worktree:

```bash
git submodule update --init --recursive
```

After submodule initialization, rerun:

```bash
./scripts/generate_memblock_rtl.sh
```

Successful generation must produce non-empty files, unless the V2 RTL profile
is updated with an explicit equivalent mapping:

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
```
