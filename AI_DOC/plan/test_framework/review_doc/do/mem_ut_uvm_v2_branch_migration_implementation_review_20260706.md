# mem_ut_uvm_v2 分支迁移实现 Review

## 1. Review 范围

关联 plan：

```text
AI_DOC/plan/test_framework/plan/do/mem_ut_uvm_v2_branch_migration_plan_20260706.md
```

本 review 覆盖 `mem_ut_uvm_v2` 分支迁移、V2 RTL 生成能力迁移、V2 worktree RTL
路径接入、远端 VCS compile smoke，以及后续 V2 DUT 适配准备状态。本 review 不声称完成
V2 DUT interface/agent 适配。

工作区与分支：

```text
worktree: /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2
branch: mem_ut_uvm_v2
upstream/base: origin/kunminghu-v2
origin/kunminghu-v2: 2acbf327cf7fb514593acc00d4c41117ec499e08
```

## 2. 提交链 Review

| 顺序 | Commit | 提交说明 | 阶段 |
|---|---|---|---|
| 1 | `f6d94eea7eaee0aecc55eec5a77d7c50e145b930` | `docs(mem_ut): record v3 baseline before v2 migration` | V3 基线记录 |
| 2 | `8003cc08417858b3f74042d1ab26591ec7d3c095` | `docs(mem_ut): add versioned v2 v3 rule profiles` | 规则/profile 版本化 |
| 3 | `e11e15b6a28c8be5b8372d005d07ceefe89195a6` | `chore(mem_ut): align v2 branch to kunminghu-v2 baseline` | V2 基线阶段标记 |
| 4 | `cead7d6fc6d4dc9888bd3352a5a8cf6493cd673a` | `build(memblock): add v2 rtl generation flow` | V2 RTL 生成入口初版 |
| 5 | `d555ee14a8418264c79a1e5f014cb5a040f6a46b` | `mem_ut: port base environment to v2 rtl path` | mem_ut 基础环境搬迁 |
| 6 | `4fa7f3b5edaca8fe3592e26e6fcde40ce789a8dd` | `docs(mem_ut): review v2 branch migration implementation` | 初版 review |
| 7 | `1933eadd1...` | `docs(mem_ut): translate v2 migration review to chinese` | 中文 review |

本轮新增修改将在上述提交后形成收尾 commit，内容包括 V2 standalone wrapper 修正、V2
RTL 路径修正、公共库补齐、文档状态更新和最终 review。

## 3. 关键实现 Review

### 3.1 V2 standalone MemBlockTop wrapper

修改文件：

```text
src/main/scala/top/MemBlockTop.scala
```

修改前逻辑：

初版 V2 `MemBlockTop` 只是简单实例化 `xiangshan.mem.MemBlock` 并 clone 暴露 `io` /
`io_perf`。该方式无法闭合 V2 `MemBlock` 依赖的 Diplomacy 图，也没有为 frontend/backend
边界、TL/interrupt/prefetch/PTW/DCache/Uncache 连接提供 standalone 生成环境。

修改后逻辑：

当前 wrapper 仍实例化 V2 `xiangshan.mem.MemBlock()`，不修改 V2 `MemBlock.scala` 内部
LSU/TLB/cache 设计逻辑；新增的是 standalone RTL 生成所需的外部封装：

- `BlackBoxMemBlockFrontend` 和 `BlackBoxMemBlockBackend` 作为 frontend/backend 边界。
- `makeCachedManagerNode`、`makeUncachedManagerNode` 补齐 TL manager/client 连接。
- `frontendBridge`、DCache、Uncache、PTW、interrupt、prefetch bridge 的 Diplomacy 图闭合。
- `MemBlockTopImp` 使用 `LazyRawModuleImp` 暴露 standalone 顶层端口。
- `MemBlockTopMain` 绑定 V2 `XSCoreParamsKey`、backend params，并调用 `Generator.execute`。

正确性检查：

```bash
./scripts/generate_memblock_rtl.sh
```

已成功生成：

```text
291 build_memblock/rtl/filelist.f
4.6M build_memblock/rtl/MemBlock.sv
2.0M build_memblock/rtl/MemBlockTop.sv
```

边界说明：

该 wrapper 是 RTL 生成 harness，不是 V2 设计逻辑替换。V2 设计源码冲突仍必须以
`origin/kunminghu-v2` 为准，不能用 V3 设计代码覆盖 V2。

### 3.2 V2 RTL path 使用 MEMBLOCK_XS_HOME

修改文件：

```text
scripts/generate_memblock_rtl.sh
mem_ut/ver/ut/memblock/cfg/rtl.f
mem_ut/ver/ut/memblock/cfg/tb.f
mem_ut/ver/ut/memblock/sim/eda01_entry.sh
mem_ut/ver/ut/memblock/sim/remote_eda_make.sh
AGENTS.md
AI_DOC/memblock_rtl生成规则.md
AI_DOC/mem_ut远端编译仿真方案与流程.md
```

修改前逻辑：

`rtl.f` 使用：

```text
-F $MEMBLOCK_PROJECT/XiangShan/build_memblock/rtl/filelist.f
```

在 V2 独立 worktree 名为 `XiangShan_V2_mem_ut_uvm_v2` 的情况下，这会误读同级旧
`XiangShan` worktree 的 RTL。

修改后逻辑：

V2 `rtl.f` 使用：

```text
-F $MEMBLOCK_XS_HOME/build_memblock/rtl/filelist.f
```

`eda01_entry.sh` 从当前 sim 目录向上定位当前 XiangShan worktree 根目录，并导出：

```text
MEMBLOCK_XS_HOME=/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2
MEMBLOCK_PROJECT=/nfs/home/lixiangrui/work/memblock_ut
```

`remote_eda_make.sh` 只在外层显式设置时转发 `MEMBLOCK_XS_HOME` /
`MEMBLOCK_PROJECT`，避免空值覆盖 `eda01_entry.sh` 的路径推导。

正确性检查：

在 eda01 上确认：

```text
$MEMBLOCK_XS_HOME/build_memblock/rtl/filelist.f exists
$MEMBLOCK_XS_HOME/mem_ut/ver/common/tcnt_assertion/sva_lib.f exists
```

### 3.3 mem_ut 公共库补齐

修改文件：

```text
mem_ut/ver/common/tcnt_assertion/sva_lib.f
mem_ut/ver/common/tcnt_base/**
mem_ut/ver/ut/memblock/cfg/tb.f
mem_ut/ver/ut/memblock/cfg/rtl.f
```

修改前逻辑：

V2 worktree 只搬迁了 `mem_ut/ver/ut/memblock`，缺少 `mem_ut/ver/common`。远端
VCS 在打开 `../../../common/tcnt_assertion/sva_lib.f` 时失败。

修改后逻辑：

补齐 V3 mem_ut 中已跟踪且与当前 V3/V2 worktree 一致的 `mem_ut/ver/common` 公共库。
同时把 `rtl.f`、`tb.f` 中公共库入口改为：

```text
$MEMBLOCK_XS_HOME/mem_ut/ver/common/...
```

这样 VCS 不依赖嵌套 `-F` filelist 的相对路径解析规则。

### 3.4 远端 compile smoke

命令：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

执行演进：

1. 初始失败：缺少 `../../../../scr/verif/project_cfg.mk`。
2. 修正：`sim/Makefile` 改为 include 已搬迁的 `../cfg/verif/project_cfg.mk`。
3. 第二次失败：默认 `SIM_TOOLS=xrun`，远端环境只加载 VCS/Verdi。
4. 修正：`eda_*` 远端目标默认传 `REMOTE_SIM_TOOLS=vcs`。
5. 第三次失败：公共库相对路径解析不稳定。
6. 修正：公共库 filelist 改为 `$MEMBLOCK_XS_HOME/...`。
7. 最终结果：VCS 成功进入当前 V2 RTL 和 TB 解析，失败在 DUT 端口不匹配。

最终 VCS 首批错误：

```text
Error-[UPIMI-E] Undefined port in module instantiation
Port "io_ooo_to_mem_backendToTopBypass_cpuWfi" is not defined in module 'MemBlock'

Port "io_ooo_to_mem_csrCtrl_bp_ctrl_ubtbEnable" is not defined in module 'MemBlock'
Port "io_ooo_to_mem_csrCtrl_bp_ctrl_abtbEnable" is not defined in module 'MemBlock'
Port "io_ooo_to_mem_csrCtrl_bp_ctrl_mbtbEnable" is not defined in module 'MemBlock'
Port "io_ooo_to_mem_csrCtrl_bp_ctrl_tageEnable" is not defined in module 'MemBlock'
Port "io_ooo_to_mem_csrCtrl_bp_ctrl_scEnable" is not defined in module 'MemBlock'
Port "io_ooo_to_mem_csrCtrl_bp_ctrl_ittageEnable" is not defined in module 'MemBlock'
Port "io_ooo_to_mem_enqLsq_needAlloc_6" is not defined in module 'MemBlock'
Port "io_ooo_to_mem_enqLsq_needAlloc_7" is not defined in module 'MemBlock'
Port "io_ooo_to_mem_enqLsq_req_6_valid" is not defined in module 'MemBlock'
```

Review 结论：

这证明远端 VCS 已经读到当前 V2 generated RTL，剩余失败属于预期的后续 V2 DUT
interface/agent 适配范围。不能通过把 V2 generated RTL 改回 V3 端口名来解决。

## 4. Plan 对齐检查

| Plan 项 | Review 状态 |
|---|---|
| 基于 `kunminghu-v2` 创建独立 `mem_ut_uvm_v2` 分支 | 已完成。当前分支为 `mem_ut_uvm_v2`，base 为 `origin/kunminghu-v2`。 |
| 迁移前记录 V3 基线 | 已完成。 |
| 建立通用规则路由和 V2/V3 profile | 已完成，并补充 `MEMBLOCK_XS_HOME` 路径规则。 |
| 将 V2 分支对齐到 `origin/kunminghu-v2` | 已完成，且有独立阶段提交。 |
| 新增 V2 RTL 生成入口 | 已完成。 |
| 生成 V2 RTL | 已完成。`filelist.f`、`MemBlock.sv`、`MemBlockTop.sv` 非空。 |
| 将 mem_ut 基础环境接到 V2 RTL 路径 | 已完成到 compile 可读取 V2 RTL 的程度；公共库和 filelist 路径已补齐。 |
| 完成 V2 DUT interface/agent 适配 | 未完成，且不属于本 plan 直接目标。 |
| 运行远端 `eda_compile` smoke | 已运行。失败点为 DUT port mismatch，作为后续 V2 DUT 适配输入。 |

## 5. 与 plan 不一致的实现

### 5.1 `MEMBLOCK_XS_HOME` 替代固定 `$MEMBLOCK_PROJECT/XiangShan`

plan 原逻辑：

文档中多处沿用 `$MEMBLOCK_PROJECT/XiangShan/build_memblock/rtl/filelist.f` 作为示例路径。

当前实现：

V2 使用 `$MEMBLOCK_XS_HOME/build_memblock/rtl/filelist.f`。

原因：

V2 worktree 实际目录名不是 `XiangShan`，而是 `XiangShan_V2_mem_ut_uvm_v2`。继续使用
`$MEMBLOCK_PROJECT/XiangShan` 会读错另一个 worktree 的 RTL。

### 5.2 远端目标默认使用 VCS

plan 原逻辑：

只要求使用远端 compile smoke，没有明确本地 `project_cfg.mk` 默认 `SIM_TOOLS=xrun`
与远端 bootstrap 的关系。

当前实现：

`eda_*` 远端目标默认传 `REMOTE_SIM_TOOLS=vcs`，与已验证的 VCS/Verdi module bootstrap
一致。本地普通 `make compile` 仍保留 `project_cfg.mk` 的默认配置。

## 6. Plan 未说明但实现补充的细节

### 6.1 `build_memblock/` 和远端 compile 产物 ignore

新增 `.gitignore` 规则：

```text
/build_memblock/
/mem_ut/ver/ut/memblock/sim/*/exec/
/mem_ut/ver/ut/memblock/sim/*/log/
/mem_ut/ver/ut/memblock/sim/*/wave/
/mem_ut/ver/ut/memblock/sim/*/cov/
```

作用：

RTL snapshot 和 VCS 中间产物不进入源码提交。本轮只提交可复现脚本、wrapper、规则和
review 文档。

### 6.2 后续适配 seed 文档

新增：

```text
AI_DOC/analysis/interface/v2/memblock_v2_dut_interface_delta_seed_20260706.md
```

作用：

把当前 VCS 暴露的第一批 V2 DUT 端口差异记录下来，作为后续
`mem_ut_v2_dut_interface_adapt_plan_<YYYYMMDD>.md` 的输入。

## 7. 验证结果

已执行：

```bash
proxychains git submodule update --init --recursive
./scripts/generate_memblock_rtl.sh
test -s build_memblock/rtl/filelist.f
test -s build_memblock/rtl/MemBlock.sv
test -s build_memblock/rtl/MemBlockTop.sv
ssh eda01 'test -s .../build_memblock/rtl/filelist.f'
ssh eda01 'test -s .../mem_ut/ver/common/tcnt_assertion/sva_lib.f'
cd mem_ut/ver/ut/memblock/sim && make eda_compile tc=tc_sanity mode=base_fun
```

结果：

- V2 RTL 生成通过。
- V2 RTL 和 mem_ut common 路径在 eda01 可见。
- 远端 VCS compile 进入当前 V2 RTL/TB 解析。
- VCS compile 未通过，失败点为 `dut_inst.sv` 仍使用 V3/旧 DUT 端口名和端口规模。

## 8. 剩余风险和后续工作

1. V2 DUT interface/agent 适配未完成。必须基于 generated V2 Verilog 单独执行后续 plan。
2. 当前 UVM `dut_inst.sv`、`*_agent_connect.sv`、agent interface/xaction/driver/monitor 仍含
   V3/旧端口名，例如 `cpuWfi`、camelCase BPU ctrl 字段和 LSQ enq 宽度假设。
3. 当前没有运行 `eda_run`，因为 `eda_compile` 已在 DUT port mismatch 阶段停止。
4. `build_memblock/` 为生成产物，已 ignore，不作为源码提交内容。

## 9. 主 agent review 结论

本轮迁移已满足分支迁移和 V2 RTL 生成准备 plan 的直接目标：

- V2 分支可追溯地基于 `origin/kunminghu-v2`。
- V2/V3 profile 和路径规则已版本化。
- V2 standalone RTL 生成能力已可重复执行。
- mem_ut 基础环境已接到当前 V2 worktree 的 RTL 路径。
- 远端 VCS smoke 已证明下一阶段 blocker 是 DUT interface mismatch，而不是 submodule、
  wrong worktree RTL、公共库缺失或工具入口问题。

本 review 未发现需要把 V3 设计逻辑覆盖到 V2 设计源码的理由；后续应严格以 V2 generated
RTL 为权威输入做 UVM 适配。
