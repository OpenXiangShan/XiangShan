# mem_ut_uvm_v2 分支迁移实现 Review

## 1. Review 范围

关联 plan：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_uvm_v2_branch_migration_plan_20260706.md
```

本 review 覆盖 `mem_ut_uvm_v2` 分支当前 5 个迁移提交的实现状态，以及收尾文档对 RTL 生成结果真实状态的更新。本 review 不新增源码修改，不移动 plan，不执行 push。本 review 文档和 profile 状态更新作为独立收尾 commit 提交，便于 review。

工作区与分支：

```text
worktree: /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2
branch: mem_ut_uvm_v2
upstream/base: origin/kunminghu-v2
origin/kunminghu-v2: 2acbf327cf7fb514593acc00d4c41117ec499e08
reviewed HEAD: d555ee14a8418264c79a1e5f014cb5a040f6a46b
branch status before this document: ahead of origin/kunminghu-v2 by 5 commits
```

## 2. 提交链

| 顺序 | Commit | 提交说明 | 阶段 |
|---|---|---|---|
| 1 | `f6d94eea7eaee0aecc55eec5a77d7c50e145b930` | `docs(mem_ut): record v3 baseline before v2 migration` | V3 基线记录 |
| 2 | `8003cc08417858b3f74042d1ab26591ec7d3c095` | `docs(mem_ut): add versioned v2 v3 rule profiles` | 规则/profile 版本化 |
| 3 | `e11e15b6a28c8be5b8372d005d07ceefe89195a6` | `chore(mem_ut): align v2 branch to kunminghu-v2 baseline` | V2 基线阶段标记 |
| 4 | `cead7d6fc6d4dc9888bd3352a5a8cf6493cd673a` | `build(memblock): add v2 rtl generation flow` | V2 RTL 生成入口 |
| 5 | `d555ee14a8418264c79a1e5f014cb5a040f6a46b` | `mem_ut: port base environment to v2 rtl path` | mem_ut 基础环境搬迁 |

## 3. 阶段 Review

### 3.1 V3 基线记录

提交 `f6d94eea7` 新增：

```text
AI_DOC/analysis/rtl/v2/mem_ut_uvm_v2_migration_v3_baseline_20260706.md
```

该提交满足 plan 中“切到 `mem_ut_uvm_v2` 后先记录 V3 修改前基线”的要求。文档记录了 V3 来源 worktree、V3 commit、V2 base commit，以及本次 V2 迁移在独立 worktree 中执行的事实。

Review 结论：第一笔提交只做基线记录，没有混入 RTL 生成脚本、V2 harness 或 mem_ut UVM 环境搬迁，阶段边界清楚。

### 3.2 规则和 profile 版本化

提交 `8003cc084` 新增通用 AI/project 规则、更新 `AGENTS.md`，并在以下目录建立 V2/V3 版本 profile：

```text
mem_ut/ver/ut/memblock/rule/version/v2
mem_ut/ver/ut/memblock/rule/version/v3
```

实现方式符合 plan 方向：通用规则保持单份，分支策略、RTL 生成 profile、DUT interface 基线、L2TLB profile 和 verified status 按版本拆分。这样后续 V2 工作不会误用 V3 DUT、V3 上游分支或 V3 工具链假设。

Review 结论：规则/profile 拆分满足“通用规则 + V2/V3 profile”的要求。

### 3.3 V2 基线对齐

提交 `e11e15b6` 是空提交阶段标记，提交名为：

```text
chore(mem_ut): align v2 branch to kunminghu-v2 baseline
```

该阶段记录当前分支基于：

```text
origin/kunminghu-v2: 2acbf327cf7fb514593acc00d4c41117ec499e08
```

这符合 plan 中“规则版本化提交之后，再独立记录 V2 基线切换结果”的要求。当前分支历史是 `origin/kunminghu-v2` 加 5 个迁移提交，因此分支形态可追溯。

Review 结论：空提交用于阶段隔离是可接受的，后续 RTL 生成脚本和 mem_ut 搬迁没有混入 V2 base 标记提交。

### 3.4 V2 RTL 生成入口

提交 `cead7d6fc` 新增：

```text
scripts/generate_memblock_rtl.sh
src/main/scala/top/MemBlockTop.scala
AI_DOC/analysis/rtl/v2/memblock_rtl_generation_result_20260706.md
```

脚本会定位仓库根目录，默认把 `MEMBLOCK_PROJECT` 设置为 XiangShan worktree 的上一级目录，检查 recursive submodule 初始化状态，通过 `build.sc` 对应的 `mill xiangshan.runMain` 调用 V2 生成入口，并期望 V2 RTL 输出到 `build_memblock/rtl`。

源码支撑：

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

这段逻辑在 V2 RTL 生成 flow 中提供独立入口。脚本先确定仓库根目录和输出目录，再检查 submodule 是否完整；如果依赖未初始化，直接报错退出，避免进入不完整 build。依赖满足后，脚本通过 `mill` 调用 `top.MemBlockTopMain`，用 `KunminghuV2Config` 生成 MemBlock 顶层 SystemVerilog，并在命令完成后检查 `filelist.f`、`MemBlock.sv`、`MemBlockTop.sv` 是否非空。Scala 入口则实例化 `xiangshan.mem.MemBlock`，把 MemBlock 的 `io` 和 `io_perf` 暴露到 standalone 顶层，供生成脚本作为独立 RTL 顶层使用。

Review 结论：V2 RTL generation flow 已迁移，但当前失败点证明 V2 RTL 未生成成功，不能把该阶段记为 RTL 生成验收通过。

### 3.5 mem_ut 基础环境搬迁

提交 `d555ee14a` 新增基础 `mem_ut/ver/ut/memblock` UVM 环境，包括 agent 目录、cfg/filelist、common package、env、sequence、testbench connect 文件、testcase 文件和远端 EDA make 入口。

plan 允许把基础环境搬到 V2 worktree 作为准备阶段，但明确要求真实 V2 DUT interface 适配必须以生成后的 V2 RTL 为权威输入。因此当前状态应理解为“环境已搬到 V2 worktree，并接到预期 V2 RTL 路径”，不能理解为“V2 DUT 已完成适配”。

关键路径检查：

```text
mem_ut/ver/ut/memblock/cfg/rtl.f
  -F $MEMBLOCK_PROJECT/XiangShan/build_memblock/rtl/filelist.f

mem_ut/ver/ut/memblock/sim/Makefile
  eda_compile / eda_run / eda_run_bg / eda_status / eda_tail
  route through remote_eda_make.sh and eda01_entry.sh
```

中文伪代码：

`rtl.f` 仍把 RTL 权威输入定义为 `$MEMBLOCK_PROJECT/XiangShan/build_memblock/rtl/filelist.f`。远端 Makefile 目标负责把本地 mem_ut 仿真请求转交给 `eda01` 执行。由于当前 V2 `filelist.f` 没有被成功生成，这条远端编译链路的入口存在，但还没有可用于编译的 V2 RTL 输入。

Review 结论：基础环境搬迁作为后续 V2 DUT 适配准备是合理的，但当前不能宣称 testbench、agent、driver、monitor 已和 V2 RTL 接口闭环。

## 4. Plan 对齐检查

| Plan 项 | Review 状态 |
|---|---|
| 基于 `kunminghu-v2` 创建独立 `mem_ut_uvm_v2` 分支 | 已完成。当前分支是 `mem_ut_uvm_v2`，相对 `origin/kunminghu-v2` ahead 5 个迁移提交。 |
| 迁移前记录 V3 基线 | 已在 `f6d94eea7` 完成。 |
| 建立通用规则路由和 V2/V3 profile | 已在 `8003cc084` 完成。 |
| 将 V2 分支对齐到 `origin/kunminghu-v2` | 已由 `e11e15b6` 阶段标记记录；review base 是 `2acbf327c`。 |
| 新增 V2 RTL 生成入口 | 已在 `cead7d6fc` 完成。 |
| 生成 V2 RTL | 未完成。生成失败，原因是 V2 submodule/build-source 状态不完整。 |
| 将 mem_ut 基础环境接到 V2 RTL 路径 | 已在 `d555ee14a` 作为准备工作完成。 |
| 完成 V2 DUT interface/agent 适配 | 未完成，也未声称完成；必须先有生成后的 V2 Verilog。 |
| 运行远端 `eda_compile` | 未运行；V2 RTL 生成失败后不应继续远端编译。 |

## 5. 实现与 Plan 不一致项

### 5.1 V2 RTL 生成未通过

Plan 原有逻辑：

plan 期望 `scripts/generate_memblock_rtl.sh` 能生成 V2 RTL，并产出非空的 `filelist.f`、`MemBlock.sv`、`MemBlockTop.sv`，或在 V2 profile 中记录明确等价产物。

当前实现结果：

生成入口已经存在，但生成在依赖/build-source 准备阶段失败：

```text
first run: required submodules were not initialized
submodule update: failed at ready-to-run
second run: entered mill, then failed because ready-to-run/.../modules/ready-to-run is not a git repository
second run also reported missing rocket-chip/cde/common.sc, rocket-chip/hardfloat/common.sc, rocket-chip/common.sc
```

不一致原因：

当前 V2 worktree 的依赖树没有闭合。失败发生在可信 V2 RTL 产物生成之前。

处理结论：

plan 继续留在 `undo`；不运行 `eda_compile`；后续必须先修复 V2 submodule/build-source 状态，再重新执行 RTL 生成。

### 5.2 mem_ut 基础环境早于 V2 RTL 成功生成完成搬迁

Plan 原有逻辑：

plan 说明 V2 DUT interface 适配必须等待 V2 Verilog 作为权威输入，同时也包含基础环境迁移里程碑。

当前实现结果：

基础环境已经搬到 `mem_ut/ver/ut/memblock`，并连接到预期 RTL filelist 路径，但 V2 RTL 还没有成功生成。

不一致原因：

这作为环境准备可以接受，但不能证明 DUT 端口或内部路径已经适配到 V2。

处理结论：

不得把当前 testbench/agent 状态描述成 V2 DUT 已适配。RTL 生成成功后，仍需要单独的 V2 DUT 适配 plan。

## 6. Plan 未说明但 Coding 落实的细节

### 6.1 脚本级 submodule 防护

plan 要求 RTL 生成 flow 能归类失败原因，但没有明确要求在进入 `mill` 前做 recursive submodule 防护。

已实现细节：

`scripts/generate_memblock_rtl.sh` 会检查 `git submodule status --recursive`，如果任何 submodule 未初始化或缺少 `.git` 目录，就在调用 `mill` 前退出。

作用：

该防护避免不完整依赖树触发更难读的 build 报错。第一次失败中，它正确指出需要先初始化 submodule。

### 6.2 环境变量覆盖入口

plan 要求 config、target dir、firtool 和 JVM 参数可覆盖。脚本实现了以下覆盖入口：

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

作用：

后续如果需要切换 V2 config、输出路径或 firtool，不需要直接修改脚本，降低后续调试成本。

### 6.3 V2 基线空提交阶段标记

plan 允许在没有实际文件变化时使用空提交留下 V2 基线切换阶段记录。提交 `e11e15b6` 即为该阶段标记。

作用：

它把分支/base 记录与后续 RTL 脚本、mem_ut 环境搬迁分开，便于 review 提交边界。

## 7. 验证结果

本 review 检查过以下命令或结果：

```text
git log --oneline --decorate --max-count=12
git rev-parse HEAD origin/kunminghu-v2
git show --stat --oneline f6d94eea7 8003cc084 e11e15b6 cead7d6fc d555ee14a
git diff --name-status origin/kunminghu-v2...HEAD
rg -n "ready-to-run|common\.sc|submodule|generate_memblock_rtl|V2|MemBlockTopMain|KunminghuV2Config" AI_DOC scripts src/main/scala/top mem_ut/ver/ut/memblock/rule AGENTS.md
```

主 agent 最终检查：

```text
git diff --check origin/kunminghu-v2..HEAD -- AGENTS.md AI_DOC mem_ut/ver/ut/memblock scripts src/main/scala/top
rg -n "kunminghu-v3|kunminghu-v2|mem_ut_uvm|mem_ut_uvm_v2|AI_DOC/design_plan|最新 DUT" AGENTS.md AI_DOC mem_ut/ver/ut/memblock/rule
git diff --name-only origin/kunminghu-v2..HEAD -- src build.sc build.mill dependencies coupledL2 openLLC scripts
git status --short --branch --ignore-submodules=all
```

主 agent review 结果：

```text
git diff --check: pass
version keyword check: 只观察到 route/profile/plan/baseline 相关命中
design-side diff check: 只包含 scripts/generate_memblock_rtl.sh 和 src/main/scala/top/MemBlockTop.scala
superproject status check: 需要使用 --ignore-submodules=all，因为 ready-to-run gitdir 当前损坏
```

RTL 生成验证：

```text
./scripts/generate_memblock_rtl.sh
```

未成功完成。最终记录的失败原因是 `ready-to-run` 相关 submodule/build-source 状态不完整，以及缺少 `rocket-chip` build 文件。

远端 EDA 编译：

```text
cd mem_ut/ver/ut/memblock/sim
make eda_compile ...
```

未运行。RTL 生成失败后继续运行 VCS 会使用缺失或过期的 `build_memblock/rtl/filelist.f`，不能证明 V2 RTL 正确。

## 8. 未完成项和风险

1. V2 submodule/build-source 尚未闭合。必须先修复 `ready-to-run` 和 `rocket-chip` build 文件问题，再重新运行 RTL 生成。
2. V2 RTL 还没有成功生成。`build_memblock/rtl/filelist.f`、`MemBlock.sv`、`MemBlockTop.sv` 尚未被证明是有效 V2 产物。
3. V2 DUT interface 适配还未完成。当前 `dut_inst.sv`、`*_agent_connect.sv`、agent interface、xaction、driver、monitor、env、RM、sequence 和 cfg 都需要在 V2 RTL 生成后重新对照。
4. 本 V2 分支还没有运行远端 `eda_compile`，因为 RTL 输入前置条件缺失。
5. mem_ut 基础环境搬迁范围很大，V2 RTL 生成后需要单独做源码级 review，重点检查 DUT 顶层端口、内部层级路径和 L2TLB request/response 语义。

## 9. 非当前 worktree 状态

本 review 只针对上述 V2 worktree，不清理、不回滚、不 push 其他工作区内容。

subagent 生成了 RTL 结果更新和本 implementation review 文档。主 agent review 后更新了 V2 verified-status profile，并把 review 文档调整为项目规则要求的 plan 对齐章节名称。

本 review 观察到的状态：

```text
mem_ut_uvm_v2 is ahead of origin/kunminghu-v2 by 5 commits before this document update.
```

`build_memblock/` 或仿真输出等非源码/生成目录不作为本 review 的完成交付物。

## 10. Plan 归档决定

plan 仍保留在：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_uvm_v2_branch_migration_plan_20260706.md
```

不移动到 `do`，原因是两个关键收敛条件缺失：

1. V2 RTL 生成尚未成功完成。
2. 远端编译尚未运行，并且在 V2 RTL 生成前不应运行。

## 11. Review 结论

迁移分支已经形成可追溯的本地提交链。规则版本化、V2 RTL 生成入口、mem_ut 基础环境搬迁都已存在。

该分支尚未完全收敛。V2 RTL generation flow 已迁移，但 V2 RTL 生成本身因 V2 submodule/build-source 状态不完整而失败。当前状态不能宣称 V2 DUT 已适配，也不能宣称 `eda_compile` 已通过。
