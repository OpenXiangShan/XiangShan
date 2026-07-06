# mem_ut_uvm_v2 migration V3 baseline record

## Purpose

This document records the immutable V3 mem_ut baseline before starting the
`mem_ut_uvm_v2` migration work. It is a traceability record only; it does not
define V2 DUT adaptation behavior.

## Branch Baselines

| Item | Value |
|---|---|
| V3 mem_ut branch | `mem_ut_uvm` |
| V3 baseline commit | `1f96d06acbd75f00d619885ca27155810f72d922` |
| V3 commit message | `feat: add memblock virtual sequence dispatch` |
| V3 commit date | `2026-07-06 11:06:56 +0800` |
| V2 design base branch | `origin/kunminghu-v2` |
| V2 base commit | `2acbf327cf7fb514593acc00d4c41117ec499e08` |
| V2 base commit message | ``fix(MemBlock): `vSegmentUnit` needs to connect `ready` to `IQ` (#6123)`` |
| V2 base commit date | `2026-06-24 14:24:27 +0800` |

## Source Worktree State

The source worktree used for this migration request was:

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2
```

It was on branch `mem_ut_uvm` at commit
`1f96d06acbd75f00d619885ca27155810f72d922`.

The source worktree had untracked local files or directories before migration:

```text
.codex
AI_DOC/plan/test_framework/plan/undo/mem_ut_uvm_v2_branch_migration_plan_20260706.md
build_memblock/
coupledL2/
novas.conf
novas.rc
openLLC/
verdiLog/
```

To avoid polluting that worktree, the V2 migration is executed in a separate
worktree:

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2
```

## V3 Generated RTL Check

The source worktree was checked for the expected V3 generated RTL files before
starting V2 migration. The expected files were not present as non-empty files:

| Path | Result |
|---|---|
| `build_memblock/rtl/filelist.f` | Missing |
| `build_memblock/rtl/MemBlock.sv` | Missing |
| `build_memblock/rtl/MemBlockTop.sv` | Missing |

Observed `build_memblock/` content was Verdi/log-oriented output plus
`build_memblock/MemBlockTop.MbistMemBlk.csv`; it was not a reusable memblock RTL
snapshot.

## Execution Plan

Associated plan:

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_uvm_v2_branch_migration_plan_20260706.md
```

The plan file was copied into this V2 worktree so the final implementation
review can reference a path that exists on the `mem_ut_uvm_v2` branch.
