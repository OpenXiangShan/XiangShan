# mem_ut_uvm_v2 branch migration implementation review

## 1. Review Scope

关联 plan：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_uvm_v2_branch_migration_plan_20260706.md
```

本 review 覆盖 `mem_ut_uvm_v2` 分支当前 5 个本地提交的实现状态，以及本轮收尾文档对
RTL 生成结果的真实状态更新。本 review 不新增源码修改，不移动 plan，不执行 push。
本 review 文档和 profile 状态更新作为独立收尾 commit 提交，便于 review。

工作区与分支：

```text
worktree: /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2
branch: mem_ut_uvm_v2
upstream/base: origin/kunminghu-v2
origin/kunminghu-v2: 2acbf327cf7fb514593acc00d4c41117ec499e08
reviewed HEAD: d555ee14a8418264c79a1e5f014cb5a040f6a46b
branch status before this document: ahead of origin/kunminghu-v2 by 5 commits
```

## 2. Commit Chain

| Order | Commit | Subject | Stage |
|---|---|---|---|
| 1 | `f6d94eea7eaee0aecc55eec5a77d7c50e145b930` | `docs(mem_ut): record v3 baseline before v2 migration` | V3 baseline record |
| 2 | `8003cc08417858b3f74042d1ab26591ec7d3c095` | `docs(mem_ut): add versioned v2 v3 rule profiles` | Rule/profile versioning |
| 3 | `e11e15b6a28c8be5b8372d005d07ceefe89195a6` | `chore(mem_ut): align v2 branch to kunminghu-v2 baseline` | V2 baseline marker |
| 4 | `cead7d6fc6d4dc9888bd3352a5a8cf6493cd673a` | `build(memblock): add v2 rtl generation flow` | V2 RTL generation entry |
| 5 | `d555ee14a8418264c79a1e5f014cb5a040f6a46b` | `mem_ut: port base environment to v2 rtl path` | Base mem_ut environment port |

## 3. Stage Review

### 3.1 V3 baseline record

Commit `f6d94eea7` added:

```text
AI_DOC/analysis/rtl/v2/mem_ut_uvm_v2_migration_v3_baseline_20260706.md
```

This satisfies the plan requirement to leave a traceable V3 baseline before V2
migration work. The document records the source V3 worktree, V3 commit, V2 base
commit, and the fact that this V2 migration is executed in an independent
worktree.

### 3.2 Rule and profile versioning

Commit `8003cc084` added the common AI/project rules, updated `AGENTS.md`, and
created V2/V3 version profiles under:

```text
mem_ut/ver/ut/memblock/rule/version/v2
mem_ut/ver/ut/memblock/rule/version/v3
```

The implementation matches the plan direction: common rules remain single-copy,
while branch-dependent policy, RTL generation profile, DUT interface baseline,
L2TLB profile, and verified status are separated by version. This reduces the
risk that later V2 work accidentally reuses V3 DUT or tool assumptions.

### 3.3 V2 baseline alignment

Commit `e11e15b6` is an empty stage marker named
`chore(mem_ut): align v2 branch to kunminghu-v2 baseline`. It records that the
branch is based on:

```text
origin/kunminghu-v2: 2acbf327cf7fb514593acc00d4c41117ec499e08
```

This is consistent with the plan's request for a separate V2 baseline switch
stage. Because the current branch history is `origin/kunminghu-v2` plus the 5
local migration commits, the reviewed branch shape is traceable.

### 3.4 V2 RTL generation entry

Commit `cead7d6fc` added:

```text
scripts/generate_memblock_rtl.sh
src/main/scala/top/MemBlockTop.scala
AI_DOC/analysis/rtl/v2/memblock_rtl_generation_result_20260706.md
```

The script locates the repository root, sets `MEMBLOCK_PROJECT` to the parent of
the XiangShan worktree by default, checks recursive submodule initialization,
uses `build.sc` through `mill xiangshan.runMain`, and expects V2 RTL outputs in
`build_memblock/rtl`.

Source support:

```text
scripts/generate_memblock_rtl.sh
  CONFIG="${CONFIG:-KunminghuV2Config}"
  TARGET_DIR="${TARGET_DIR:-build_memblock/rtl}"
  mill_args=(... xiangshan.runMain top.MemBlockTopMain ...)

src/main/scala/top/MemBlockTop.scala
  class MemBlockTop extends LazyModule
  object MemBlockTopMain extends App
```

中文伪代码：

这段逻辑在 V2 RTL 生成 flow 中提供独立入口。脚本先确定仓库根目录和输出目录，再检查
submodule 是否完整；如果依赖未初始化，直接报错退出，避免进入不完整 build。依赖满足后，
脚本通过 `mill` 调用 `top.MemBlockTopMain`，用 `KunminghuV2Config` 生成 MemBlock 顶层
SystemVerilog，并在命令完成后检查 `filelist.f`、`MemBlock.sv`、`MemBlockTop.sv` 是否非空。
Scala 入口则实例化 `xiangshan.mem.MemBlock`，把 MemBlock 的 `io` 和 `io_perf` 暴露到
standalone 顶层，供生成脚本作为独立 RTL 顶层使用。

Review 结论：V2 RTL generation flow 已迁移，但当前失败点证明 V2 RTL 未生成成功，不能把
该阶段记为 RTL 生成验收通过。

### 3.5 Base mem_ut environment port

Commit `d555ee14a` added the base `mem_ut/ver/ut/memblock` UVM environment,
including agent directories, cfg/filelist, common package, env, sequences,
testbench connect files, testcase files, and remote EDA make entries.

The plan allowed base environment migration as a preparation stage, but explicitly
required V2 RTL as the authority before real V2 DUT interface adaptation. Current
state should therefore be read as "environment ported to V2 worktree and wired to
the expected V2 RTL path", not as "V2 DUT adaptation completed".

Important path check:

```text
mem_ut/ver/ut/memblock/cfg/rtl.f
  -F $MEMBLOCK_PROJECT/XiangShan/build_memblock/rtl/filelist.f

mem_ut/ver/ut/memblock/sim/Makefile
  eda_compile / eda_run / eda_run_bg / eda_status / eda_tail
  route through remote_eda_make.sh and eda01_entry.sh
```

中文伪代码：

`rtl.f` 仍把 RTL 权威输入定义为 `$MEMBLOCK_PROJECT/XiangShan/build_memblock/rtl/filelist.f`。
远端 Makefile 目标负责把本地 mem_ut 仿真请求转交给 `eda01` 执行。由于当前 V2
`filelist.f` 没有被成功生成，这条远端编译链路的入口存在，但还没有可用于编译的 V2 RTL
输入。

## 4. Plan 对齐检查

| Plan item | Reviewed status |
|---|---|
| Create independent `mem_ut_uvm_v2` branch based on `kunminghu-v2` | Done. Current branch is `mem_ut_uvm_v2`, ahead of `origin/kunminghu-v2` by 5 commits. |
| Record V3 baseline before migration | Done in `f6d94eea7`. |
| Add common rule routing and V2/V3 profiles | Done in `8003cc084`. |
| Align V2 branch to `origin/kunminghu-v2` | Stage marker done in `e11e15b6`; reviewed base is `2acbf327c`. |
| Add V2 RTL generation entry | Done in `cead7d6fc`. |
| Generate V2 RTL | Not done. Generation failed because V2 submodule/build-source state is incomplete. |
| Port base mem_ut environment to V2 RTL path | Done in `d555ee14a` as preparation. |
| Complete V2 DUT interface/agent adaptation | Not done and not claimed. Requires generated V2 Verilog first. |
| Run remote `eda_compile` | Not run by design, because V2 RTL generation did not succeed. |

## 5. 实现与 Plan 不一致项

### 5.1 V2 RTL generation did not pass

Plan original logic:

The plan expected `scripts/generate_memblock_rtl.sh` to generate V2 RTL and
produce non-empty `filelist.f`, `MemBlock.sv`, and `MemBlockTop.sv` or a
documented V2 equivalent.

Current implementation result:

The generation entry exists, but generation failed in dependency/build-source
setup:

```text
first run: required submodules were not initialized
submodule update: failed at ready-to-run
second run: entered mill, then failed because ready-to-run/.../modules/ready-to-run is not a git repository
second run also reported missing rocket-chip/cde/common.sc, rocket-chip/hardfloat/common.sc, rocket-chip/common.sc
```

Reason:

The V2 dependency tree is not closed in this worktree. The failure happened
before a trustworthy V2 RTL artifact could be produced.

Treatment:

Keep the plan in `undo`; do not run `eda_compile`; close V2 submodules/build
sources before continuing.

### 5.2 Base mem_ut port happened before V2 RTL success

Plan original logic:

The plan says V2 DUT interface adaptation must wait until V2 Verilog exists as
the authority. It also has a base environment migration milestone.

Current implementation result:

The base environment was ported into `mem_ut/ver/ut/memblock` and connected to
the expected RTL filelist path, but the generated V2 RTL is missing.

Reason:

This is acceptable as environment preparation, but it is not evidence that DUT
ports or internal paths are adapted to V2.

Treatment:

Do not describe the current testbench/agent state as V2 DUT adapted. A later
dedicated V2 DUT adaptation plan remains required after RTL generation succeeds.

## 6. Plan 未说明但 Coding 落实的细节

### 6.1 Script-level submodule guard

The plan required the flow to classify generation failures, but did not spell out
a pre-mill recursive submodule guard.

Implemented detail:

`scripts/generate_memblock_rtl.sh` checks `git submodule status --recursive` and
exits before `mill` if any submodule is uninitialized or missing a `.git`
directory.

Why it matters:

This prevents a noisy build from hiding the real dependency issue. The guard
correctly caught the first failure and pointed to submodule initialization.

### 6.2 Environment-variable overrides

The plan required configurable target dir, config, firtool, and JVM settings.
The script implements overrides for:

```text
MILL
JVM_XMX
JVM_XSS
CONFIG
CHISEL_TARGET
TARGET_DIR
FIRTOOL_OPT
MEMBLOCK_FIRTOOL / FIRTOOL
MEMBLOCK_PROJECT
```

Why it matters:

This keeps the V2 flow usable for later tool or output-path experiments without
editing the script.

### 6.3 Empty V2 baseline marker commit

The plan allowed an empty commit if the V2 baseline switch needed a reviewable
stage marker. Commit `e11e15b6` is that marker.

Why it matters:

It separates branch/baseline history from later RTL script and mem_ut environment
port commits.

## 7. Verification Results

Completed checks reviewed for this document:

```text
git log --oneline --decorate --max-count=12
git rev-parse HEAD origin/kunminghu-v2
git show --stat --oneline f6d94eea7 8003cc084 e11e15b6 cead7d6fc d555ee14a
git diff --name-status origin/kunminghu-v2...HEAD
rg -n "ready-to-run|common\\.sc|submodule|generate_memblock_rtl|V2|MemBlockTopMain|KunminghuV2Config" AI_DOC scripts src/main/scala/top mem_ut/ver/ut/memblock/rule AGENTS.md
```

Main-agent final checks:

```text
git diff --check origin/kunminghu-v2..HEAD -- AGENTS.md AI_DOC mem_ut/ver/ut/memblock scripts src/main/scala/top
rg -n "kunminghu-v3|kunminghu-v2|mem_ut_uvm|mem_ut_uvm_v2|AI_DOC/design_plan|最新 DUT" AGENTS.md AI_DOC mem_ut/ver/ut/memblock/rule
git diff --name-only origin/kunminghu-v2..HEAD -- src build.sc build.mill dependencies coupledL2 openLLC scripts
git status --short --branch --ignore-submodules=all
```

Main-agent review result:

```text
git diff --check: pass
version keyword check: only route/profile/plan/baseline hits observed
design-side diff check: only scripts/generate_memblock_rtl.sh and src/main/scala/top/MemBlockTop.scala
superproject status check: usable only with --ignore-submodules=all because ready-to-run gitdir is broken
```

RTL generation verification:

```text
./scripts/generate_memblock_rtl.sh
```

did not complete successfully. The final recorded failure is incomplete
submodule/build-source state around `ready-to-run` and missing `rocket-chip`
build files.

Remote EDA compile:

```text
cd mem_ut/ver/ut/memblock/sim
make eda_compile ...
```

was intentionally not run after RTL generation failed. Running VCS now would use
an absent or stale `build_memblock/rtl/filelist.f` and would not verify V2 RTL.

## 8. Unfinished Items And Risks

1. V2 submodule/build-source closure is incomplete. `ready-to-run` and
   `rocket-chip` build files must be fixed before rerunning RTL generation.
2. V2 RTL has not been generated successfully. `build_memblock/rtl/filelist.f`,
   `MemBlock.sv`, and `MemBlockTop.sv` are not proven valid V2 outputs.
3. V2 DUT interface adaptation is not complete. Current `dut_inst.sv`,
   `*_agent_connect.sv`, agent interfaces, xactions, drivers, monitors, env, RM,
   sequences, and cfg still require verification against generated V2 Verilog.
4. Remote `eda_compile` has not been run for this V2 branch because the RTL input
   prerequisite is missing.
5. The base mem_ut environment port is large and should receive a separate
   source-level review after V2 RTL is available, especially for DUT top ports,
   internal hierarchy paths, and L2TLB request/response semantics.

## 9. Non-current-worktree State

This review is scoped to the V2 worktree above. It does not attempt to clean,
revert, commit, or push anything.

The subagent generated the RTL result update and this implementation review
document. Main-agent review then updated the V2 verified-status profile and
normalized this review document to the required plan-alignment section names.

Known status from this review:

```text
mem_ut_uvm_v2 is ahead of origin/kunminghu-v2 by 5 commits before this document update.
```

Non-source/generated areas such as `build_memblock/` or simulator outputs are
not treated as completed deliverables in this review.

## 10. Plan Archive Decision

The plan remains in:

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_uvm_v2_branch_migration_plan_20260706.md
```

It is not moved to `do` because two key closure conditions are missing:

1. V2 RTL generation has not completed successfully.
2. Remote compile has not been run and should not be run until generated V2 RTL
   exists.

## 11. Review Conclusion

The migration branch has a traceable 5-commit local chain. Rule versioning,
V2 RTL generation entry, and base mem_ut environment port are present.

The branch is not fully closed. The V2 RTL generation flow has been migrated, but
V2 RTL generation itself failed due to incomplete V2 submodule/build-source
state. No downstream V2 DUT adaptation or `eda_compile` success should be
claimed from the current state.
