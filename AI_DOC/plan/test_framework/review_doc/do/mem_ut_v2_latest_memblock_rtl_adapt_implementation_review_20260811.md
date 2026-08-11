# V2 最新 MemBlock RTL 测试框架适配实施 Review

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| DUT | 被测试设计，本次指最新生成的 V2 `MemBlock` 模块。 | `build/rtl/MemBlock.sv` | 顶层端口和内部层级均以该文件为准。 |
| RTL | 由 Chisel 构建流程生成的 Verilog/SystemVerilog 硬件实现。 | `build/rtl/filelist.f`、`build/rtl/MemBlock.sv` | UVM 编译通过 filelist 读取 RTL。 |
| filelist | 按编译顺序列出 RTL 源文件的清单。 | `build/rtl/filelist.f`、`cfg/rtl.f` | 远端 VCS 编译从 `cfg/rtl.f` 间接展开。 |
| wrapper | 为独立生成入口额外包裹 MemBlock 的顶层模块。 | 已删除的 `src/main/scala/top/MemBlockTop.scala` | 当前 V2 使用整核 `top.TopMain`，不再需要该 standalone wrapper。 |
| whole-core top | 生成整核 Verilog 的顶层入口。 | `top.TopMain`、`scripts/generate_memblock_rtl.sh` | 产物中的 `MemBlock.sv` 作为 UVM DUT 顶层。 |
| UVM | 本项目用于驱动、采样和检查 DUT 的 SystemVerilog 验证框架。 | `mem_ut/ver/ut/memblock` | 后续 interface、driver、monitor 适配的执行主体。 |
| review round | 子 agent 修改后由主 agent 独立检查的一轮闭环。 | 本文第 4 节及后续功能单元章节 | 发现问题时回派修正；无问题后才允许提交。 |

## 2. 评审范围与权威基线

本 review 覆盖用户要求的最新 V2 MemBlock DUT 适配工作。当前分支为
`mem_ut_uvm_v2`，`HEAD` 为 `d1db8e1cb7`，并已确认
`origin/kunminghu-v2` 的当前提交 `75b106b551` 是该 HEAD 的祖先。因此，本轮
不再对脏工作区执行 rebase，而以已生成的下列 RTL 为唯一接口权威来源：

```text
build/rtl/filelist.f
build/rtl/MemBlock.sv
```

本节先记录功能单元一。后续 DUT 端口、agent 和测试框架逻辑适配会按完成顺序追加到本文，
每一单元均由主 agent review 通过后单独本地提交。

## 3. 功能单元一：RTL 输出路径迁移与过时 wrapper 清理

### 3.1 功能目标

将 UVM、生成脚本和现行规则统一到 `build/rtl`，消除对旧
`build_memblock/rtl` 快照的依赖；同时删除已不参与当前 V2 整核生成链路的
`MemBlockTop.scala`。

### 3.2 修改前逻辑

`cfg/rtl.f` 曾从 `$MEMBLOCK_XS_HOME/build_memblock/rtl/filelist.f` 读取 RTL，
而最新整核生成命令实际写入 `build/rtl`。两套目录并存时，测试可能编译旧快照，导致
测试环境和本轮接口检查使用的 DUT 不一致。

旧 `src/main/scala/top/MemBlockTop.scala` 是 standalone wrapper；它不再是当前 V2
`top.TopMain` 生成路径的入口，却可能误导后续维护者把不存在的
`MemBlockTop.sv` 当作必需产物。

### 3.3 修改后逻辑

`mem_ut/ver/ut/memblock/cfg/rtl.f` 现在固定展开
`$MEMBLOCK_XS_HOME/build/rtl/filelist.f`。生成脚本默认将 `make verilog` 的
`BUILD_DIR` 设为 `build`，并以 `build/rtl/MemBlock.sv` 和 `filelist.f` 作为成功产物。
规则、V2/V3 profile 和远端流程说明同步使用同一目录。删除 wrapper 后，V2 的唯一
生成模型是 `top.TopMain -> build/rtl/MemBlock.sv`。

本单元没有引入运行期 UVM 状态、队列或扫描逻辑，因此不涉及高频路径性能风险。

### 3.4 变更文件与职责

| 类别 | 文件 | 修改后的职责 |
|---|---|---|
| UVM 编译入口 | `mem_ut/ver/ut/memblock/cfg/rtl.f` | 从当前 worktree 的 `build/rtl/filelist.f` 读取 DUT RTL。 |
| 生成入口 | `scripts/generate_memblock_rtl.sh` | 默认调用整核 V2 生成并校验 `build/rtl` 产物。 |
| Scala 旧入口 | `src/main/scala/top/MemBlockTop.scala` | 删除，不再提供与当前生成流不一致的 standalone wrapper。 |
| 规则与 profile | `AGENTS.md`、`AI_DOC/*.md`、`mem_ut/ver/ut/memblock/rule/**/*.md` | 把活动路径统一为 `build/rtl`，保留历史分析/旧仿真缓存的原始记录。 |
| 忽略规则 | `.gitignore` | 由已有的通用 `build` 忽略规则覆盖生成 RTL，移除已废弃的专属目录规则。 |

### 3.5 源码支撑材料

源码位置：`scripts/generate_memblock_rtl.sh`，生成目录和产物闭合检查。

抽象功能描述：该脚本为 V2 RTL 生成提供唯一默认输出目录，调用整核构建入口后确认
UVM 所需的 filelist 和 DUT 顶层均存在；它不负责启动 UVM 仿真。

```bash
TARGET_BUILD_DIR="${TARGET_BUILD_DIR:-build}"
TARGET_RTL_DIR="${TARGET_RTL_DIR:-${TARGET_BUILD_DIR}/rtl}"
CHECK_REFERENCE="${CHECK_REFERENCE:-0}"

make_cmd+=(verilog)
make_vars=("BUILD_DIR=${TARGET_BUILD_DIR}" "CONFIG=${CONFIG}" ...)
"${make_cmd[@]}" "${make_vars[@]}"

for required in "${TARGET_RTL_DIR}/filelist.f" "${TARGET_RTL_DIR}/MemBlock.sv"; do
  if [[ ! -s "${required}" ]]; then
    exit 1
  fi
done
```

该片段先把默认 build 目录设为 `build`，再把 RTL 子目录派生为 `build/rtl`；
`make verilog` 完成后仅接受同时存在非空 filelist 和 `MemBlock.sv` 的结果。关闭默认的
旧目录内容比较后，脚本不会把已废弃的快照重新引入当前生成流程。

### 3.6 正确性检查

| 检查项 | 结果 | 结论 |
|---|---|---|
| V2 上游基线 | `origin/kunminghu-v2` 是当前 HEAD 的祖先 | 当前 `build/rtl/MemBlock.sv` 对应已同步 V2 设计代码。 |
| 脚本语法 | `bash -n scripts/generate_memblock_rtl.sh` 通过 | 默认目录修改没有 Shell 语法错误。 |
| 生成器校验 | `FORCE_REGENERATE=0 scripts/generate_memblock_rtl.sh` 通过 | 读取 `build/rtl`，确认 `filelist.f` 有 2005 行且 `MemBlock.sv` 非空。 |
| wrapper 依赖 | 对活动源码、脚本和 UVM 配置检索无 `MemBlockTop.scala` 或 `MemBlockTopMain` 引用 | 删除 standalone wrapper 不会切断当前 V2 生成入口。 |
| 文本与空白 | `git diff --check` 通过 | 本单元没有空白错误。 |

### 3.7 主 agent Review 第 1 轮结论

主 agent 已独立检查生成器 diff、当前 V2 profile、UVM filelist 入口和生成结果。未发现
阻止提交的问题：所有活动 UVM/规则路径均已指向 `build/rtl`，生成器的产物检查与当前
V2 `top.TopMain` 流一致，删除的 wrapper 不存在活动引用。该功能单元可以单独提交。

## 4. Plan 对齐检查

已在以下路径检索与本轮“最新 MemBlock RTL 路径迁移及接口适配”直接对应的 plan：

```text
AI_DOC/plan/test_framework/plan/undo
AI_DOC/plan/test_framework/plan/do
```

检索到的既有 V2 编译参数、分支迁移和历史 DUT 适配 plan 不描述本次
`build_memblock/rtl -> build/rtl` 迁移及新增端口的实现，因此未找到可作为本轮 coding
验收依据的对应 plan。本 review 依据用户本次要求、
`memblock_latest_dut_adapt_rule.md` 和 V2 profile 进行检查，不能表述为“与 plan 一致”。

## 5. 非本次修改的逻辑分析

### 5.1 git status 对比结论

当前工作区还存在下列不纳入功能单元一提交和功能正确性判断的已有改动：

| 类别 | 文件 | 不纳入原因 |
|---|---|---|
| RTL 知识库分析文档 | `AI_DOC/analysis/interface/v2/agents/l2tlb_agent.md`、`AI_DOC/analysis/interface/v2/index.md`、`AI_DOC/analysis/rtl/v2/flows/memory_flush_pipe_flow.md`、`AI_DOC/analysis/rtl/v2/index.md` | 属于已有 L2TLB/RTL 分析资料，不是本次路径迁移或 DUT 接口接线实现。 |
| 后续接口功能单元 | `mem_ut/ver/ut/memblock/tb/**`、`mem_ut/ver/ut/memblock/agent/**` 的后续改动 | 将在各自完成后追加独立 review，并各自单独提交。 |
| 历史仿真缓存 | `mem_ut/ver/ut/memblock/sim/**/partitionlib/**` 中的旧绝对路径 | 已生成且被忽略的编译缓存，不是当前 `rtl.f` 的输入，不能作为源码修改提交。 |

本轮提交仅包含第 3 节列出的路径迁移、wrapper 删除和本文档，避免把既有分析文档或后续
接口适配混入同一 commit。
