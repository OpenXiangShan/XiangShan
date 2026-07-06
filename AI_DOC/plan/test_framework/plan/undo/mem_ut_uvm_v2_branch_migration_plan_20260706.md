# mem_ut_uvm_v2 分支迁移与 V2 RTL 生成准备 Plan

## Plan 定位

本文档用于规划 `mem_ut_uvm` 从当前基于 `kunminghu-v3` 的工作形态，迁移出独立
`mem_ut_uvm_v2` 分支并挂钩 `kunminghu-v2` 最新设计代码的执行方案。

本 plan 的重点是分支基线、规则版本化、V2 RTL 生成能力迁移和后续 V2 DUT 适配准备。
本 plan 不直接完成完整 V2 DUT interface/agent 适配，不直接修复所有 V2 编译错误。

适用版本：

- V3：`mem_ut_uvm`，上游设计分支 `kunminghu-v3`
- V2：`mem_ut_uvm_v2`，上游设计分支 `kunminghu-v2`

## 当前基线

当前只读检查结果：

- 当前仓库：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2`
- 当前分支：`mem_ut_uvm`
- 当前 HEAD：`1f96d06acbd75f00d619885ca27155810f72d922`
- 当前提交信息：`feat: add memblock virtual sequence dispatch`
- `origin/kunminghu-v2`：`2acbf327cf7fb514593acc00d4c41117ec499e08`
- `origin/kunminghu-v3`：`d97789de12ea371909ebc1bf89a4fbaf136bb994`
- `origin/mem_ut_uvm`：`e6affc8e9c079ffe49cfab9383d7aeb6680ac6bb`
- 当前未发现本地或远端 `mem_ut_uvm_v2` 分支。
- 当前工作区存在未跟踪目录或文件：
  - `AI_DOC/plan/test_framework/plan/undo/mem_ut_uvm_v2_branch_migration_plan_20260706.md`
  - `.codex`
  - `build_memblock/`
  - `coupledL2/`
  - `openLLC/`
  - `novas.conf`
  - `novas.rc`
  - `verdiLog/`

当前工作区未清理前，不允许执行 `checkout`、`rebase`、`merge`、`pull` 或会改变基线的命令。

## 目标

1. 建立独立的 `mem_ut_uvm_v2` 工作分支，使其后续通过 rebase `kunminghu-v2` 获取最新 V2 设计代码。
2. 在切换到 `mem_ut_uvm_v2` 后，先提交当前 V3 RTL/UVM 基线 commit 号作为修改前记录。
3. 将规则和文档整理为“通用规则 + V2/V3 profile 覆盖层”，避免 V2 工作误用 V3 规则。
4. 在规则版本化提交之后，将 `mem_ut_uvm_v2` 的设计/RTL 基线切换并 rebase 到最新 V2 版本。
5. 将 memblock RTL 生成能力迁移到 V2 分支，并能生成 V2 版本 RTL。
6. 为后续基于 V2 RTL 适配当前 mem_ut UVM 环境准备权威输入、检查入口和验收标准。

## 非目标

本 plan 不完成以下工作：

1. 不直接把当前 `mem_ut_uvm` rebase 到 `kunminghu-v2`。
2. 不在脏工作区中直接切分支或同步上游。
3. 不把 V3 的 `scripts/generate_memblock_rtl.sh` 原样拷贝到 V2 后直接使用。
4. 不在未生成 V2 Verilog 前改 DUT agent/interface/xaction/driver/monitor。
5. 不自动 push 到远端。
6. 不把 `build_memblock/` 生成产物默认提交进源码，除非后续明确要求保存 RTL snapshot。
7. 不用 V3 设计源码、RTL 源码或构建源码去解决 V2 rebase 冲突；冲突中的设计代码必须以 V2 为准。

## 验收标准

- AC-1：工作区基线可追溯
  - 执行前能明确当前未跟踪内容的归属和处理方式。
  - `mem_ut_uvm_v2` 的创建基线明确记录为 `origin/kunminghu-v2` 的具体 commit。

- AC-2：修改前记录独立提交
  - `mem_ut_uvm_v2` 上第一笔提交只记录迁移前 V3 基线，不混入 RTL 生成迁移或 UVM 适配代码。
  - 记录内容至少包含 V3 branch、V3 HEAD、V2 base commit、当前 V3 RTL 产物检查摘要。

- AC-3：规则版本化清晰
  - `AGENTS.md` 能根据当前分支或用户指定版本路由到 V2/V3 profile。
  - `mem_ut_uvm` 默认使用 V3 profile。
  - `mem_ut_uvm_v2` 默认使用 V2 profile。
  - `rg -n "kunminghu-v3|kunminghu-v2|mem_ut_uvm|mem_ut_uvm_v2|最新 DUT" AGENTS.md AI_DOC mem_ut/ver/ut/memblock/rule` 的命中均能解释为通用路由或明确版本 profile。

- AC-4：V2 RTL 生成能力可验证
  - `mem_ut_uvm_v2` 已在规则版本化提交之后 rebase 到最新 `origin/kunminghu-v2`。
  - rebase 冲突中涉及 V2 设计源码、RTL 源码、构建源码时，最终内容与 V2 版本保持一致，不带入 V3 设计修改。
  - rebase 完成后有独立 commit 记录 V2 基线切换结果。

- AC-5：V2 RTL 生成能力可验证
  - V2 分支上存在独立可执行的 memblock RTL 生成入口。
  - 生成脚本不硬套 V3 的 Chisel/FIRRTL/firtool 假设。
  - 生成成功后存在 `filelist.f`、`MemBlock.sv`、`MemBlockTop.sv` 或经 V2 profile 明确定义的等价产物。

- AC-6：V2 DUT 适配准备完成
  - V2 RTL 生成后，能从 `top_tb.sv` 展开 DUT 接线链路。
  - 后续适配有明确的接口比对清单和 agent 同步范围。
  - 在开始 V2 DUT 适配前，已有 V2 Verilog 作为权威接口来源。

## 总体流程

```text
准备工作区
  -> 创建或切换 mem_ut_uvm_v2
  -> 提交 V3 修改前记录
  -> 建立通用规则 + V2/V3 profile
  -> 提交规则版本化修改
  -> 将 mem_ut_uvm_v2 rebase 到最新 V2 设计/RTL 基线
  -> 冲突解决完全以 V2 设计代码为准
  -> 提交 V2 基线切换结果
  -> 迁移 V2 RTL 生成能力
  -> 生成并检查 V2 RTL
  -> 准备 V2 DUT interface 适配清单
  -> 后续单独执行 V2 DUT 适配 plan
```

## 里程碑 1：工作区与分支基线处理

### 目标

在执行任何会改变 Git 基线的命令前，确保当前工作区状态可解释、可恢复、可 review。

### 执行步骤

1. 在仓库根目录运行：

   ```bash
   git status --short --branch
   git branch -a | rg 'mem_ut_uvm|kunminghu-v[23]'
   git rev-parse HEAD origin/kunminghu-v2 origin/kunminghu-v3 origin/mem_ut_uvm
   ```

2. 分类当前未跟踪内容：

   ```text
   build_memblock/   RTL 生成产物候选
   coupledL2/        V2 依赖或本地残留，需确认
   openLLC/          V2 依赖或本地残留，需确认
   verdiLog/         仿真工具产物
   novas.*           Verdi/Novas 工具配置或产物
   .codex            本地工具状态
   ```

3. 明确处理策略：

   - 本 plan 文档属于本轮新增 plan，应在执行迁移前纳入提交或明确保留在当前分支。
   - 生成产物默认不提交。
   - 如需保留现场，优先移动到仓库外备份目录或用单独文档记录。
   - 在未清理前，不执行 `checkout`、`rebase`、`merge`、`pull`。

4. 确认 `mem_ut_uvm_v2` 是否存在：

   - 若远端已有真实 `mem_ut_uvm_v2`，以远端分支为准。
   - 若远端不存在，在工作区清理后先执行 `git fetch origin kunminghu-v2`，再从最新
     `origin/kunminghu-v2` 新建本地 `mem_ut_uvm_v2`。

### 风险

- `coupledL2/` 和 `openLLC/` 可能是 V2 依赖，不能在未确认前删除。
- 当前 `mem_ut_uvm` 相对 `origin/mem_ut_uvm` 存在 ahead/behind，不能把远端同步和 V2 迁移混成一个操作。

## 里程碑 2：提交 V3 修改前记录

### 目标

满足“切换到 `mem_ut_uvm_v2` 后先提交当前 V3 版本 RTL commit 号作为修改前记录”的要求。

### 执行步骤

1. 在 `mem_ut_uvm_v2` 上新建基线记录文档，建议路径：

   ```text
   AI_DOC/analysis/rtl/v2/mem_ut_uvm_v2_migration_v3_baseline_20260706.md
   ```

2. 文档至少记录：

   ```text
   V3 mem_ut branch: mem_ut_uvm
   V3 baseline commit: 1f96d06acbd75f00d619885ca27155810f72d922
   V3 commit message: feat: add memblock virtual sequence dispatch
   V2 design base: origin/kunminghu-v2
   V2 base commit: 2acbf327cf7fb514593acc00d4c41117ec499e08
   V3 generated RTL summary:
     build_memblock/rtl/filelist.f exists and non-empty
     build_memblock/rtl/MemBlock.sv exists and non-empty
     build_memblock/rtl/MemBlockTop.sv exists and non-empty
   ```

3. 第一笔 commit 只包含该记录文档和必要索引入口，不包含代码迁移：

   ```bash
   git add AI_DOC/analysis/rtl/v2/mem_ut_uvm_v2_migration_v3_baseline_20260706.md
   git commit -m "docs(mem_ut): record v3 baseline before v2 migration"
   ```

### 验收

- `git show --stat HEAD` 只显示基线记录文档或必要索引文档。
- 该提交不包含 `scripts/`、`mem_ut/`、`src/` 的迁移代码。

## 里程碑 3：规则版本化

### 目标

把当前硬编码 V3 的规则整理成可按分支路由的版本化规则，避免后续 V2 工作误读 V3 规则。

### 建议目录

保留通用规则单份，版本差异放 profile：

```text
mem_ut/ver/ut/memblock/rule/version/
  v3/
    branch_policy.md
    memblock_rtl_profile.md
    dut_interface_baseline.md
    l2tlb_interface_profile.md
    verified_status.md
  v2/
    branch_policy.md
    memblock_rtl_profile.md
    dut_interface_baseline.md
    l2tlb_interface_profile.md
    verified_status.md
```

分析文档按版本放入：

```text
AI_DOC/analysis/rtl/v3/
AI_DOC/analysis/rtl/v2/
AI_DOC/analysis/interface/v3/
AI_DOC/analysis/interface/v2/
```

### 需要版本化的规则

1. `mem_ut/ver/ut/memblock/rule/memblock_update_code_rule.md`
   - 通用流程保留：先 `git status`，干净后 fetch/rebase，失败保留现场。
   - V3 profile：`mem_ut_uvm` rebase `origin/kunminghu-v3`。
   - V2 profile：`mem_ut_uvm_v2` rebase `origin/kunminghu-v2`。

2. `AI_DOC/memblock_rtl生成规则.md`
   - 通用流程保留：使用脚本生成、检查产物、生成后进入远端编译验证。
   - V3 profile：记录 Chisel 7.3.0、firtool 1.135.0、`top.MemBlockTopMain`、`TLConfig`。
   - V2 profile：记录 V2 实际验证后的 build system、Chisel/firtool、main class、config、输出产物。

3. `mem_ut/ver/ut/memblock/rule/memblock_latest_dut_adapt_rule.md`
   - 通用流程保留：以生成 Verilog 和 `top_tb.sv` 展开接线为权威。
   - V3/V2 profile：分别记录 DUT 顶层端口、关键内部层级和 agent connect 基线。

4. `mem_ut/ver/ut/memblock/rule/memblock_l2tlb_agent_rule.md`
   - 通用语义保留：L2TLB agent 建模 DTLB -> L2TLB request/response。
   - V2/V3 profile：记录各版本真实连接点、字段、路径和 takeover 默认值。

5. `AGENTS.md`
   - 只放路由规则，不堆叠版本细节。
   - 当前分支为 `mem_ut_uvm` 时默认读 V3 profile。
   - 当前分支为 `mem_ut_uvm_v2` 时默认读 V2 profile。
   - 用户显式指定 V2/V3 时，以用户指定优先。
   - 当前分支和用户指定版本冲突时，停止并确认。

### 保持通用的规则

以下规则原则上不复制为 V2/V3 两套：

- `AI_DOC/project_management/ai_doc_file_management_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_agent_add_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_cfg_add_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_sequence_add_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_code_comment_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_user_ctrl_usage.md`

如这些规则涉及版本特有字段、接口、plus cfg、connect 宏默认值，只在版本 profile 中补充。

### 验收

执行：

```bash
rg -n "kunminghu-v3|kunminghu-v2|mem_ut_uvm|mem_ut_uvm_v2|最新 DUT|AI_DOC/design_plan" \
  AGENTS.md AI_DOC mem_ut/ver/ut/memblock/rule
git diff --check -- AGENTS.md AI_DOC mem_ut/ver/ut/memblock/rule
```

要求：

- 命中项均有明确版本归属或通用路由语义。
- 不再存在无版本上下文的 `kunminghu-v3` 更新硬编码。
- 旧 `AI_DOC/design_plan` 引用被迁移、解释或标注为历史引用。

### 提交要求

规则版本化完成并通过静态检查后，必须先提交该阶段修改：

```bash
git add AGENTS.md AI_DOC mem_ut/ver/ut/memblock/rule
git commit -m "docs(mem_ut): add versioned v2 v3 rule profiles"
```

该提交完成后，才进入 V2 设计/RTL 基线切换阶段。

## 里程碑 4：切换并 rebase 到 V2 设计/RTL 基线

### 目标

在完成规则版本化提交之后，将 `mem_ut_uvm_v2` 的设计代码、RTL 源码和构建源码切换到最新
`kunminghu-v2`。如果 rebase 发生冲突，冲突解决必须保证 V2 设计代码与 V2 分支一致，不能用
V3 修改覆盖或改写 V2 版本设计。

### 基本原则

1. `mem_ut_uvm_v2` 的设计基线必须来自最新 `origin/kunminghu-v2`。
2. 对 `src/`、`build.sc`、`build.mill`、`dependencies/`、`coupledL2/`、`openLLC/`、RTL 生成相关设计源码的冲突，默认以 V2 版本为准。
3. V3 分支上的 mem_ut 环境、文档和后续脚本迁移可以保留为后续 commit 的工作内容，但不得把 V3 设计实现混入 V2 设计源码。
4. 如果某个冲突文件同时包含 mem_ut 环境代码和 V2 设计代码，必须拆开处理：V2 设计部分以 V2 为准，mem_ut 相关部分另行移植。
5. rebase 完成并检查无误后，必须先提交 V2 基线切换结果，再进入 RTL 生成能力迁移。

### 执行步骤

1. 确保规则版本化提交已经完成：

   ```bash
   git log --oneline --max-count=5
   git status --short --branch
   ```

2. 获取最新 V2 设计：

   ```bash
   git fetch origin kunminghu-v2
   ```

3. 将 `mem_ut_uvm_v2` rebase 到最新 V2：

   ```bash
   git rebase origin/kunminghu-v2
   ```

4. 如发生冲突，按文件类型处理：

   ```text
   V2 设计源码/RTL 源码/构建源码：
     以 origin/kunminghu-v2 内容为准，不带入 V3 设计修改。

   mem_ut UVM 环境源码：
     保留当前 mem_ut 迁移目标需要的内容，但不得修改 V2 DUT 设计语义。

   AI_DOC / rule / AGENTS.md：
     保留已提交的版本化路由和 V2/V3 profile 语义。

   生成产物：
     不作为冲突解决依据，默认不提交。
   ```

5. 冲突解决后检查设计侧是否误带 V3 修改：

   ```bash
   git diff --name-only origin/kunminghu-v2 -- src build.sc build.mill dependencies coupledL2 openLLC
   git diff --check -- src build.sc build.mill dependencies coupledL2 openLLC mem_ut AI_DOC AGENTS.md
   ```

   对设计源码差异逐项解释。无法解释为 mem_ut 必要移植或版本 profile 的差异，必须回退为 V2 内容。

6. rebase 完成后提交 V2 基线切换结果：

   ```bash
   git status --short
   git commit -m "chore(mem_ut): align v2 branch to kunminghu-v2 baseline"
   ```

   如果 rebase 后工作区没有可提交文件变化，但仍需要留下审查用阶段记录，可以使用空提交：

   ```bash
   git commit --allow-empty -m "chore(mem_ut): align v2 branch to kunminghu-v2 baseline"
   ```

### 验收

- `mem_ut_uvm_v2` 基于最新 `origin/kunminghu-v2`。
- V2 设计源码、RTL 源码、构建源码没有被 V3 设计修改污染。
- 所有保留差异都能归类为 mem_ut 环境、规则文档、profile 或后续 RTL 生成迁移准备。
- 已有独立 commit 记录 V2 基线切换结果。

## 里程碑 5：迁移 V2 RTL 生成能力

### 目标

在 `mem_ut_uvm_v2` 上建立可重复执行的 memblock RTL 生成入口，后续每次 V2 版本更新后，通过 rebase `kunminghu-v2` 并重新生成 V2 RTL 获取最新设计接口。

### 已知差异

当前 V3 脚本依赖：

```text
build.mill
Chisel 7.3.0
firtool 1.135.0
top.MemBlockTopMain
TLConfig
build_memblock/rtl/filelist.f
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
```

V2 分支只读检查到的差异：

```text
build.sc，而不是 build.mill
Chisel 6.7.0
默认 top flow 是 top.TopMain
存在 KunminghuV2Config
未发现 scripts/generate_memblock_rtl.sh
未发现 top.MemBlockTopMain
V2 可能依赖 coupledL2/openLLC
```

### 执行步骤

1. 在 V2 分支确认构建系统：

   ```bash
   git show origin/kunminghu-v2:build.sc | sed -n '1,220p'
   git grep -n "chiselVersion\\|firtool\\|TopMain\\|KunminghuV2Config\\|MemBlock" origin/kunminghu-v2 -- build.sc src scripts
   ```

2. 确认 V2 是否已有可复用的 top/harness：

   - 若已有可直接生成 MemBlock 的 main class，优先复用。
   - 若没有，新增 V2 专用 `MemBlockTopMain` 或等价 harness。
   - harness 只负责生成 memblock RTL，不混入 UVM 适配逻辑。

3. 确认 V2 config：

   - 优先确认 `KunminghuV2Config` 是否为目标 V2 设计配置。
   - 若 memblock 需要专用最小 config，则在 V2 profile 中记录原因和入口。

4. 确认 firtool：

   - 不硬套 V3 的 `firtool-1.135.0`。
   - 优先使用 V2 构建体系推荐的 firtool resolver 或实际验证版本。
   - 在 V2 `memblock_rtl_profile.md` 中记录版本、路径、失败特征和替代方式。

5. 新增或迁移 RTL 生成脚本：

   ```text
   scripts/generate_memblock_rtl.sh
   ```

   要求：

   - 自动定位仓库根目录。
   - 可通过环境变量覆盖 target dir、config、firtool、JVM 参数。
   - 成功后检查 V2 profile 定义的 RTL 产物。
   - 如 V2 filelist 不自动生成，脚本必须显式补齐 filelist 生成规则。
   - ClockGate 或 memory wrapper 的补丁逻辑必须按 V2 实际文件名重新验证。

6. 生成 V2 RTL：

   ```bash
   scripts/generate_memblock_rtl.sh
   ```

7. 记录 V2 RTL 生成结果：

   ```text
   AI_DOC/analysis/rtl/v2/memblock_rtl_generation_result_YYYYMMDD.md
   ```

### 验收

- 脚本能在 V2 分支上生成 RTL。
- `filelist.f` 和关键 Verilog 文件存在且非空，或 V2 profile 记录了等价产物。
- 生成失败时，失败点能归类为 build system、firtool、main class、config、submodule 依赖或 Scala 编译问题。
- 不引入 V3-only 的 Chisel/firtool/main class 假设。

## 里程碑 6：迁移 mem_ut 基础环境

### 目标

在 V2 分支上恢复 mem_ut 基础目录、filelist、远端编译入口和 `MEMBLOCK_PROJECT` 路径规则，为后续 DUT 适配提供可执行环境。

### 执行步骤

1. 将当前 `mem_ut` UVM 环境迁入 V2 分支。
2. 保留远端编译入口：

   ```text
   mem_ut/ver/ut/memblock/sim/Makefile
   mem_ut/ver/ut/memblock/sim/remote_eda_make.sh
   mem_ut/ver/ut/memblock/sim/eda01_entry.sh
   ```

3. 检查 `MEMBLOCK_PROJECT`：

   ```text
   $MEMBLOCK_PROJECT/XiangShan/build_memblock/rtl/filelist.f
   ```

   如果 V2 文件系统采用独立目录名或 symlink，必须在 V2 profile 中明确：

   - `MEMBLOCK_PROJECT` 指向哪里。
   - `rtl.f` 如何引用 V2 RTL。
   - 本地与 eda01 看到的路径是否一致。

4. 保持 V2 文件系统独立：

   - V2 工作目录与 V3 工作目录分开，避免 `build_memblock/` 互相覆盖。
   - V2 生成产物默认位于 V2 仓库自己的 `build_memblock/rtl`。
   - 如需共存多个 RTL snapshot，使用明确命名目录并在 `rtl.f` 或 profile 中记录。

### 验收

- `mem_ut/ver/ut/memblock/cfg/rtl.f` 能指向 V2 生成的 RTL filelist。
- `eda01_entry.sh` 能在远端计算出正确的 `MEMBLOCK_PROJECT`。
- 远端编译入口存在并可调用。

## 里程碑 7：V2 DUT 适配准备

### 目标

在不立即完成适配的前提下，建立后续适配所需的权威接口清单和检查入口。

### 权威来源

以后续生成的 V2 Verilog 为准：

```text
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
build_memblock/rtl/filelist.f
```

如果 V2 产物命名不同，以 V2 profile 中记录的等价文件为准。

### 接线展开入口

后续适配必须从以下链路展开：

```text
mem_ut/ver/ut/memblock/tb/top_tb.sv
mem_ut/ver/ut/memblock/tb/dut_inst.sv
mem_ut/ver/ut/memblock/tb/tc_if_connect.sv
mem_ut/ver/ut/memblock/tb/memblock_connect.sv
mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
```

### 必查接口类别

- `io_ooo_to_mem_*`
- `io_mem_to_ooo_*`
- L2TLB / DTLB request-response
- DCache / Load / Store / MissQueue 相关交互
- prefetch 相关交互
- CSR / redirect / exception / replay 相关交互
- LSQ enqueue / commit / deq
- issue / writeback / wakeup / IQ feedback
- `robIdx`、`lqIdx`、`sqIdx`、`uopIdx`、`vuopIdx` 位宽和合法范围

### 同步范围

每个接口变化点必须同步检查：

```text
tb/dut_inst.sv
tb/*_agent_connect.sv
agent/subagent interface
transaction/xaction
driver
monitor
env/src/memblock_env.sv
env/src/memblock_env_cfg.sv
env/src/memblock_rm.sv
seq/*
cfg/tb.f
```

L2TLB 相关修改必须继续遵循：

```text
mem_ut/ver/ut/memblock/rule/memblock_l2tlb_agent_rule.md
```

不得把 L2TLB agent 误接成 L2TLB 到 L2Cache/PTW/memory 的下游模型。

### 验收

- 已生成 V2 RTL。
- 已形成 V2 顶层端口和 testbench connect 差异清单。
- 旧端口名、旧层级路径、旧字段名有对应 `rg` 检查项。
- 后续可以基于差异清单单独生成 V2 DUT 适配 coding plan。

## 验证计划

### 静态检查

规则和文档修改后执行：

```bash
git diff --check -- AGENTS.md AI_DOC mem_ut/ver/ut/memblock/rule scripts
rg -n "kunminghu-v3|kunminghu-v2|mem_ut_uvm|mem_ut_uvm_v2|AI_DOC/design_plan|最新 DUT" \
  AGENTS.md AI_DOC mem_ut/ver/ut/memblock/rule
```

RTL 生成脚本修改后执行：

```bash
scripts/generate_memblock_rtl.sh
test -s build_memblock/rtl/filelist.f
test -s build_memblock/rtl/MemBlock.sv
test -s build_memblock/rtl/MemBlockTop.sv
```

如 V2 profile 定义等价产物，则按 V2 profile 的产物清单执行。

### 远端编译 smoke

V2 RTL 生成成功并完成基础 mem_ut 搬迁后执行：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

如果编译失败，先按错误归类：

- RTL filelist 或路径问题
- DUT 顶层端口不匹配
- 内部层级路径不存在
- agent interface 字段不匹配
- package/filelist 顺序问题
- V2 构建生成产物缺失

本 plan 不要求 `tc_sanity` 在 V2 适配前通过仿真。

## 提交拆分建议

建议最少拆为以下本地提交：

1. `docs(mem_ut): record v3 baseline before v2 migration`
   - 只记录 V3 修改前基线。

2. `docs(mem_ut): add versioned v2 v3 rule profiles`
   - 新增 V2/V3 profile。
   - 更新 `AGENTS.md` 路由。
   - 清理无版本归属的 V3 硬编码。

3. `chore(mem_ut): align v2 branch to kunminghu-v2 baseline`
   - 在规则版本化提交之后执行。
   - 将 `mem_ut_uvm_v2` rebase 到最新 `origin/kunminghu-v2`。
   - 冲突解决中设计源码、RTL 源码和构建源码必须以 V2 内容为准。
   - 确认无 V3 设计修改污染后提交。

4. `build(memblock): add v2 rtl generation flow`
   - 迁移或新增 V2 RTL 生成入口。
   - 新增必要 V2 harness/main/config。
   - 更新 V2 RTL profile。

5. `mem_ut: port base environment to v2 rtl path`
   - 搬迁 mem_ut 基础环境。
   - 维护 `rtl.f`、远端编译入口和 `MEMBLOCK_PROJECT` 规则。

6. 后续单独提交：
   - `mem_ut: adapt agents to v2 dut interfaces`
   - 该提交不属于本 plan 的直接实现范围，应另起 V2 DUT 适配 plan。

## 不允许的高风险操作

- 不在当前未跟踪产物未处理前切分支或 rebase。
- 不把 `mem_ut_uvm` 原地 rebase 到 `kunminghu-v2`。
- 不在规则版本化提交前开始 V2 设计基线 rebase。
- 不用 V3 设计源码解决 V2 rebase 冲突。
- 不把 V3 RTL 生成脚本原样拷到 V2 后直接宣布完成。
- 不强制使用 V3 的 `firtool-1.135.0` 作为 V2 默认。
- 不在无 V2 Verilog 权威输入前修改 DUT agent 接口。
- 不只根据单个 VCS 报错局部修补 DUT 端口。
- 不自动 push。

## 后续独立 Plan

本 plan 完成后，应新增独立 plan：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_dut_interface_adapt_plan_<YYYYMMDD>.md
```

该后续 plan 专门处理：

- V2 Verilog 顶层端口对比
- `dut_inst.sv` 端口声明与实例连接
- `*_agent_connect.sv` 路径和字段同步
- agent interface/xaction/driver/monitor 同步
- env/RM/sequence/cfg 受影响逻辑同步
- 远端编译和仿真验证闭环
