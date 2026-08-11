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
| VLS exception | 向 MemBlock 说明当前 redirect 属于 vector load/store exception 的控制标志。 | `io_redirect_bits_isVlsException`、`is_vls_exception` | 为 1 时 RTL 将 raw `level` 的实际生效值压为 0。 |
| sideband | 不属于主 request payload、但会改变控制或状态观察结果的附加信号。 | `msiAck`、`io_outer_msi_ack` | backend 驱动 `msiAck`，DUT 将其直通到顶层输出。 |
| raw level | driver 实际送往 DUT 的原始 redirect level。 | `memblock_redirect_payload_t.level` | VLS 场景中 raw level 可以为 1，但不等于 DUT 生效值。 |
| effective level | 应用 VLS 规则后的 DUT 实际 redirect level。 | `memblock_redirect_effective_level()`、redirect anchor | `is_vls_exception=1` 时为 0，用于对账和同 ROB flush 判定。 |
| anchor | monitor 记录的一次已被 DUT 采样的 redirect 事实，用于与 cancel record 对账。 | `dispatch_raw_redirect_anchor_t` | raw/effective level、ROB key 和 sample 序号必须属于同一次 redirect。 |
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

## 6. 功能单元二：20260811 V2 新增控制端口适配

### 6.1 功能目标与 RTL 依据

本单元使测试环境与当前 `build/rtl/MemBlock.sv` 的三个新增控制端口一致，并把会改变
redirect 语义的 VLS sideband 同步到测试框架状态机。权威 RTL 中
`io_redirect_bits_isVlsException` 和 `io_ooo_to_mem_backendToTopBypass_msiAck` 是 DUT input，
`io_outer_msi_ack` 是 DUT output；RTL 还明确将前者用于将 redirect 的有效 `level` 压为 0，
并将 `msiAck` 直通到 `io_outer_msi_ack`。

| DUT 端口 | DUT 方向 | Agent 归属 | 本轮接口职责 |
|---|---|---|---|
| `io_redirect_bits_isVlsException` | input | `redirect_agent` | 由 redirect sequence/driver 驱动，并由 monitor 作为 DUT 已采样的 redirect anchor 事实记录。 |
| `io_ooo_to_mem_backendToTopBypass_msiAck` | input | `backendToTopBypass_agent` | 由 backend transaction/driver 驱动；idle 与默认 transaction 都保持 0。 |
| `io_outer_msi_ack` | output | `other_ctrl_agent` | 只从 DUT 采样到 `other_ctrl` monitor；不属于 backend agent 的输出，也不由任何 driver 驱动。 |

静态 RTL 定位为：`build/rtl/MemBlock.sv:261`、`:264`、`:1157`、`:7289` 和 `:31058`。
其中 `:7289` 是 VLS 对 `level` 的覆盖条件，`:31058` 是 input `msiAck` 到 output
`io_outer_msi_ack` 的直通赋值。

### 6.2 三个端口的接线和 UVM 对象适配

修改前，三个端口均不在对应 interface、xaction 或 connect 宏中。新 RTL 编译时会出现
未连接的顶层端口；即使编译绕过，VLS 语义和 MSI 回环也无法被框架正确表示。

修改后，`tb/dut_inst.sv` 展开三条连接宏；每个宏按 DUT 方向 force 到对应 interface。
`redirect_agent` 和 `backendToTopBypass_agent` 的 interface 同时将新增字段列入 driver 和
monitor clocking block，xaction 将字段注册到 UVM field/显示/比较逻辑；driver 的 `send_pkt()`
和每一种 idle mode 均覆盖新增 input。这样定向 sequence 可以显式发起控制事件，而没有
显式赋值的历史 sequence 保持低电平。

`other_ctrl_agent` 的 interface 仅把 `io_outer_msi_ack` 列为 input；其 xaction 字段不随机化，
monitor 采样并执行 X/Z 检查。该 agent 现有的 transaction publish 代码本来就是注释状态，
所以本轮没有虚构新的 analysis-port/scoreboard 数据流：字段兼容已经完成，运行期仍是
“采样与 X/Z 检查”这一既有 monitor 行为，未来若启用该 agent 的 publish 路径会自动携带字段。

特别检查了方向边界：`other_ctrl_agent_connect.sv` 使用
`force U_IF_NAME.io_outer_msi_ack = RTL_PATH.io_outer_msi_ack`；不存在
`force RTL_PATH.io_outer_msi_ack`、`drv_cb.io_outer_msi_ack` 或 backend agent 中对该输出的反向
赋值。因此 MSI output 只由 `other_ctrl_agent` monitor 观察，不会被测试环境与 DUT 形成多驱动。

### 6.3 VLS raw/effective redirect 语义

修改前，`memblock_redirect_payload_t` 只保存 `level`，redirect anchor 与 cancel record 的对账也
只有 raw `level`。最新 RTL 在 `isVlsException=1` 时不使用 raw `level` 的原含义，因此模型若只
保存 raw 值，会把同一个 DUT 行为解释为不同的 flush 范围。

修改后，payload 新增 `is_vls_exception`，但保留 raw `level` 用于如实记录送给 DUT 的端口值；
`memblock_redirect_effective_level()` 只负责产生语义投影，VLS 时返回 0，非 VLS 时返回 raw
`level`。`dispatch_raw_redirect_anchor_t` 同时记录 raw `level`、VLS 标志和 effective `level`，
cancel record 的匹配、payload 等价和 redirect event 去重均把 VLS 标志纳入比较，避免把两次
sideband 不同的 redirect 合并成同一事件。

| 场景 | raw `level` | `is_vls_exception` | effective `level` | 框架处理 |
|---|---:|---:|---:|---|
| 历史普通 redirect | 原值 | 0 | 原值 | 保持已有 cancel、flush 和事件去重行为。 |
| VLS redirect | 可保留原值 | 1 | 0 | anchor/cancel 对账和同 ROB flush 使用 effective 语义，raw 值仍可供定位 DUT 输入。 |
| 当前 memoryViolation 生成路径 | `memory_violation_level` | 0 | 等于 raw 值 | 当前测试框架 adapter 不产生 VLS memoryViolation，显式置 0，不改变历史基础行为。 |

`memblock_redirect_dispatch_base_sequence` 的 idle item 显式设置
`io_redirect_bits_isVlsException=0`，通过 payload 发射时透传该字段；`soft_test` 和 directed
cancel-reconcile 路径同样显式设置 `is_vls_exception=0`。因此默认场景不会因新增非随机字段
引入未知值，也不会改变非 VLS redirect 行为。

### 6.4 redirect 框架逻辑与首轮 review 修复

抽象功能描述：`memblock_redirect_effective_level()` 为 payload 提供与 RTL 一致的语义
`level`，它不改写驱动到 DUT 的 raw 值，也不自行决定任何 uid 是否被 flush。

抽象功能描述：`rob_order_util::rob_need_flush()` 是所有 redirect 覆盖判断的统一入口；它输入
待检查的 ROB key 和 redirect payload，输出该 uop 是否应由该 redirect 取消。它不创建 cancel
record、不修改 active map，只为 redirect flush、writeback 过滤和 directed check 提供一致结果。

调用关系如下：

| 调用阶段 | 调用对象 | 本轮职责 |
|---|---|---|
| memoryViolation adapter | `dispatch_monitor_event_adapter` | 构造 redirect payload 时显式写 `is_vls_exception=0`。 |
| redirect 发射 | `memblock_redirect_dispatch_base_sequence` -> redirect xaction/driver -> DUT | 将 raw `level` 和 VLS 标志一同送到 DUT。 |
| redirect 采样 | redirect monitor -> `push_raw_redirect_anchor()` | 记录同一采样点的 raw/effective level 与 ROB key。 |
| cancel/MMIO 对账 | `common_data_transaction` | 将 anchor 与 cancel record 的 raw 值、VLS 标志、effective 值和 ROB key 一起检查。 |
| 事件归并与状态处理 | `dispatch_monitor_batch_handler`、`common_data_transaction` | payload 等价和去重纳入 VLS 标志；所有 ROB 覆盖判断继续通过 `rob_need_flush()`。 |

主 agent 第 1 轮 review 发现：原 `rob_need_flush()` 对同一 ROB 直接使用
`redirect.flush_itself`。VLS 时 RTL 实际使用的 `level` 已压为 0，但软件仍可能因
`flush_itself=1` 取消同一 ROB，造成模型比 DUT 多 flush 一条 transaction。

修复后，该 helper 在 VLS 情形将用于同 ROB 分支的 `effective_flush_itself` 固定为 0；非 VLS
仍使用原 `flush_itself`。更年轻 ROB 的判断仍由 `rob_is_after()` 独立完成，未被 VLS 分支改变。
这使“raw 值可保留、语义值为 0、同 ROB 不取消、年轻 ROB 维持原环形 ROB 顺序判断”在一个统一
入口落地。

主 agent 第 2 轮 review 继续发现：上述修复虽然得到正确的 VLS 值，却在
`rob_need_flush()` 内直接写出 `is_vls_exception ? 0 : flush_itself`，没有复用
`memblock_redirect_effective_level()`。这样会让 payload、anchor/cancel 对账和 ROB flush
拥有两个有效 `level` 的表达来源，后续若 RTL 的 VLS 覆盖规则变化，只有一部分路径会同步更新。

第 2 轮修复将 VLS 分支改为调用 `memblock_redirect_effective_level(redirect)`，非 VLS 分支仍直接
使用历史 `redirect.flush_itself`。因此 helper 的输入、输出和职责边界不变：VLS 时唯一语义来源
给出 0，同 ROB 不取消；非 VLS 不因 raw `level` 与 `flush_itself` 的潜在独立含义而改变既有
行为；更年轻 ROB 仍只由 `rob_is_after()` 决定。

该修改只新增固定宽度 payload/anchor 字段和常数时间条件判断，没有扩展
`apply_redirect_flush_range()` 的 uid 扫描窗口、队列容量或循环次数；性能影响限于每次 redirect
比较多一次 bit 比较。

### 6.5 变更文件与职责

本功能单元的实现文件如下；未把已有 `AI_DOC/analysis/**` 改动纳入本单元。

| 功能层 | 文件 |
|---|---|
| DUT/agent 接线 | `mem_ut/ver/ut/memblock/tb/dut_inst.sv`、`mem_ut/ver/ut/memblock/tb/redirect_agent_connect.sv`、`mem_ut/ver/ut/memblock/tb/backendToTopBypass_agent_connect.sv`、`mem_ut/ver/ut/memblock/tb/other_ctrl_agent_connect.sv` |
| redirect agent | `mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_interface.sv`、`mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_xaction.sv`、`mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_driver.sv`、`mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_monitor.sv` |
| backend MSI agent | `mem_ut/ver/ut/memblock/agent/backendToTopBypass_agent_agent/src/backendToTopBypass_agent_agent_interface.sv`、`mem_ut/ver/ut/memblock/agent/backendToTopBypass_agent_agent/src/backendToTopBypass_agent_agent_xaction.sv`、`mem_ut/ver/ut/memblock/agent/backendToTopBypass_agent_agent/src/backendToTopBypass_agent_agent_driver.sv`、`mem_ut/ver/ut/memblock/agent/backendToTopBypass_agent_agent/src/backendToTopBypass_agent_agent_monitor.sv` |
| output-only MSI observer | `mem_ut/ver/ut/memblock/agent/other_ctrl_agent_agent/src/other_ctrl_agent_agent_interface.sv`、`mem_ut/ver/ut/memblock/agent/other_ctrl_agent_agent/src/other_ctrl_agent_agent_xaction.sv`、`mem_ut/ver/ut/memblock/agent/other_ctrl_agent_agent/src/other_ctrl_agent_agent_monitor.sv` |
| redirect 公共状态和语义 | `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`、`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`、`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`、`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_batch_handler.sv`、`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`、`mem_ut/ver/ut/memblock/seq/base_seq_help/rob_order_util.sv` |
| sequence/default 路径 | `mem_ut/ver/ut/memblock/seq/base_seq/memblock_redirect_dispatch_base_sequence.sv`、`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv`、`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_cancel_reconcile_vseq.sv` |

### 6.6 正确性检查与第二轮 review 前置项

已完成的静态检查包括：三个端口在最新 `MemBlock.sv` 中的集合、方向和 RTL 直通关系；各自
agent 的 connect/interface/xaction/driver/monitor 覆盖；VLS raw/effective anchor 对账；以及
`git diff --check -- mem_ut/ver/ut/memblock`。本单元尚未运行远端 VCS 编译或仿真，因此不能把
静态检查替代为仿真通过结论。

主 agent 第二轮 review 需要在给出“无问题”结论前逐项确认：

1. 最新 `build/rtl/MemBlock.sv` 的端口集合和输入/输出方向均与 `dut_inst.sv` 一致。
2. 三个端口分别只归属于 redirect、backendToTopBypass 和 other_ctrl agent，尤其
   `io_outer_msi_ack` 没有反向驱动。
3. VLS 的 raw/effective level、`memblock_redirect_effective_level()` 唯一语义来源、同 ROB 不 flush
   和年轻 ROB `rob_is_after()` 判定均正确。
4. `is_vls_exception=0` 的默认/定向/memoryViolation 路径保持旧行为，旧端口或旧字段不存在
   残留连接。
5. 远端 `make eda_compile tc=tc_sanity mode=base_fun` 和
   `make eda_run tc=tc_sanity mode=base_fun` 通过，日志中 `UVM_ERROR=0`、`UVM_FATAL=0` 且出现
   `TEST CASE PASSED`。

### 6.7 Plan 对齐检查

本次 VLS redirect 框架逻辑与以下已执行 plan 的 redirect 语义相关，已在对应文件追加
20260811 的已落地兼容说明：

- `AI_DOC/plan/test_framework/plan/do/memblock_virtual_sequence_unified_dispatch_plan_20260703.md`
- `AI_DOC/plan/test_framework/plan/do/dispatch_100k_performance_optimization_plan_20260614.md`

补充内容明确了 VLS effective level 对 `rob_need_flush()` 同 ROB 判定的影响、非 VLS 默认值和
memoryViolation 默认值，以及 MSI input/output 的 agent 归属。该补充不新增 RM/checker/coverage
实现，不把本轮已落地兼容写成未决建议。

### 6.8 源码支撑材料

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`，函数：
`memblock_redirect_effective_level()`。

抽象功能描述：该函数将 redirect payload 的 raw `level` 投影为与最新 RTL 一致的有效值，供
框架的语义判断使用；它不驱动 DUT、不修改 payload，也不执行 uid 扫描。

```systemverilog
function automatic bit memblock_redirect_effective_level(
    input memblock_redirect_payload_t redirect
);
    return redirect.is_vls_exception ? 1'b0 : redirect.level;
endfunction
```

中文伪代码：该函数首先接收已完整保存的 redirect payload。若 `is_vls_exception` 为 1，就返回 0，
表示 RTL 不会把 raw `level` 当作 flush-self 级别使用；否则原样返回 raw `level`。函数没有队列、
map 或状态表副作用，调用者据此决定自己的语义判断，DUT 输入仍由 redirect driver 保持 raw 值。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/rob_order_util.sv`，函数：
`rob_order_util::rob_need_flush()`。

抽象功能描述：该函数为 redirect flush、writeback stale 过滤和 directed check 提供唯一的 ROB
覆盖判定；它根据一个待检查 ROB key 和 redirect payload 返回布尔结果，不创建 cancel record 或
改变任何 transaction 状态。

```systemverilog
same_rob = rob_to_map_key(uop_rob) == rob_to_map_key(redirect.rob_key);
effective_flush_itself = redirect.is_vls_exception ?
                          memblock_redirect_effective_level(redirect) :
                          redirect.flush_itself;
return (effective_flush_itself && same_rob) ||
       rob_is_after(uop_rob, redirect.rob_key);
```

中文伪代码：函数先比较待检查 ROB 与 redirect ROB 是否相同。VLS 分支调用唯一的 effective-level
helper 得到 0，因此同一 ROB 不会被错误取消；非 VLS 分支继续使用历史 `flush_itself`，避免改变
既有 payload 中 raw `level` 与 `flush_itself` 的独立语义。最后无论是否 VLS，函数都调用
`rob_is_after()` 判断更年轻 ROB；该子函数只按环形 ROB 顺序比较 key，不访问状态表。两种结果取
或后作为返回值，上层据此执行已有的 flush 或 stale-event 处理。

源码位置：`mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_monitor.sv`，
任务：`redirect_agent_agent_monitor::mon_data()` 的有效 redirect 采样分支。

抽象功能描述：该分支在 DUT 真正采样到一个有效 redirect 时生成时间锚点，用于后续 cancel 对账；
它不生成新的 recovery event，也不更新 pass/fail/terminal 状态。

```systemverilog
anchor.valid = 1'b1;
anchor.level = io_redirect_bits_level;
anchor.is_vls_exception = io_redirect_bits_isVlsException;
anchor.effective_level = io_redirect_bits_isVlsException ? 1'b0 :
                         io_redirect_bits_level;
anchor.rob_flag = io_redirect_bits_robIdx_flag;
anchor.rob_value = io_redirect_bits_robIdx_value;
anchor.sample_seq = sample_seq;
anchor.cycle = $time;
memblock_sync_pkg::push_raw_redirect_anchor(anchor);
```

中文伪代码：monitor 在既有 reset 和 valid 门控通过后，保存该拍 raw `level`、VLS 标志和对应的
effective level，再保存同拍 ROB key、全局 sample 序号和时间。随后
`push_raw_redirect_anchor()` 把完整事实送入专用 anchor 队列，供 cancel record 依据同一采样边界
对账；该调用不将输入 redirect 回灌为 semantic recovery event，因此不会产生第二个状态写者。

源码位置：`mem_ut/ver/ut/memblock/tb/backendToTopBypass_agent_connect.sv` 和
`mem_ut/ver/ut/memblock/tb/other_ctrl_agent_connect.sv`，接线宏中的 MSI 端口。

抽象功能描述：这两条接线分别把 backend 输入交给 driver 所属 agent，并把 DUT output 交给只读
monitor 所属 agent；它们不建立额外的 MSI reference model 或响应机制。

```systemverilog
force RTL_PATH.io_ooo_to_mem_backendToTopBypass_msiAck =
    U_IF_NAME.io_ooo_to_mem_backendToTopBypass_msiAck;

force U_IF_NAME.io_outer_msi_ack = RTL_PATH.io_outer_msi_ack;
```

中文伪代码：第一条连接把 backend agent interface 的 `msiAck` 值驱动到 DUT input，因此
backend driver 是唯一输入写者。第二条连接把 DUT `io_outer_msi_ack` 镜像到 other_ctrl interface，
因此 other_ctrl monitor 只能读取并检查该 output；不存在把 output 反向 force 到 RTL 或由 driver
写入的路径，避免形成多驱动。

### 6.9 实现与 Plan 不一致项

原始的两份已归档 plan 没有预先列出本次上游新增的 VLS/MSI 顶层端口，因此严格按执行前文本比较，
本单元属于接口更新触发的实现补充，而不是原 plan 已覆盖的 coding。实现已在两份 plan 的
`[IMPLEMENTATION_DELTA]` 段显式登记：VLS 的同 ROB 判定、默认值、anchor 对账及 MSI 的 agent
方向均与本节源码一致。源码位置、控制流和副作用见第 6.8 节；处理结论是保留当前实现和该
implementation delta，不回改为旧 plan 行为。

### 6.10 Plan 未说明但 Coding 落实的细节

本轮额外落实的工程细节为：VLS 标志被纳入 payload 等价、事件去重和 anchor/cancel 对账；普通
memoryViolation、soft-test 和 directed redirect 都显式写 0；`io_outer_msi_ack` 只进入 existing
output-observation/XZ 路径而不虚构 analysis-port producer。原 plan 聚焦 redirect reissue 和 virtual
sequence 调度，未逐端口展开这些适配细节；它们已回写到 implementation delta，且第 6.8 节的源码
片段和中文伪代码记录了写者、读取者及状态副作用。无需新增 runtime 参数、RM/checker 或 coverage
plan。

### 6.11 主 agent 第 3 轮最终静态 Review

主 agent 重新核对了 `build/rtl/MemBlock.sv` 的三条端口声明、VLS level 覆盖赋值和 MSI 直通赋值，
以及 `dut_inst.sv` 的 `reg`/`wire` 类型、默认值和实例连接。随后检查了四条 connect 宏、三个
agent 的 clocking 方向、xaction/driver/monitor 字段覆盖、types 在 `rob_order_util.sv` 前的编译
顺序，以及 VLS 字段在 payload 等价、event 去重、anchor 对账和 ROB 覆盖判断中的全链路使用。

结论：第 1、2 轮发现的问题均已修复；本轮未发现新的功能、方向、时序或高频路径问题。已执行
`git diff --check -- mem_ut/ver/ut/memblock AI_DOC/plan/test_framework`，结果通过。本次没有启动
远端 VCS，因为 `dut_inst.sv` 仍有后续 vector writeback 和 TopDown 端口差异需要同轮处理；完成全部
接口差异后统一执行 `tc_sanity/base_fun` 编译和仿真，避免把一个预期端口错误误归因到本功能单元。

### 6.12 当前工作区覆盖性与非本次修改

当前功能单元覆盖第 6.5 节列出的全部 `mem_ut` 源码文件、本文和两份关联 plan；没有将其它
`mem_ut` 逻辑或生成物混入待提交范围。`git status --short` 中仍存在但不纳入本单元正确性判断的
改动如下：

| 类别 | 文件/目录 | 判断与原因 |
|---|---|---|
| 既有 RTL/接口分析 | `AI_DOC/analysis/interface/v2/**`、`AI_DOC/analysis/rtl/v2/**` | 不属于当前 UVM 接口接线或测试框架逻辑适配，保持原状且不 stage。 |
| 后续 DUT 适配 | vector writeback、TopDown、`msiInfo` 宽度相关的文件 | 尚未修改或 review，后续以独立功能单元和 commit 处理。 |

## 7. 功能单元三：V2 vector writeback 删除 `vdIdx` 字段

### 7.1 功能目标与影响边界

最新 `build/rtl/MemBlock.sv` 在两个 vector writeback payload 中保留
`vdIdxInField[2:0]`，但不再声明 `vdIdx[2:0]`。旧 testbench 同时声明并连接两个字段会导致
`dut_inst.sv` 端口不存在、connect macro 层级不存在，不能通过 elaboration。

本单元完整删除两条失效字段的顶层 wire、实例连接、四组 connect force、interface clocking 字段、
xaction field automation 和 monitor local/sample 赋值。该 agent 是顶层 DUT output observer，当前
scalar-only flow 对任何 vector `valid` 非 0 仍立即 fatal，且没有 sequence、RM、scoreboard、公共状态或
transaction producer 消费 `vdIdx`；因此字段删除只恢复接口闭合，不改变测试框架运行期状态机。

### 7.2 保留字段和 monitor 行为

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_vec_wb_agent_agent/src/io_mem_to_ooo_vec_wb_agent_agent_monitor.sv`，任务：
`io_mem_to_ooo_vec_wb_agent_agent_monitor::mon_data()`。

抽象功能描述：该任务持续采样 vector writeback 输出，并在 scalar-only 环境检测到任何有效 vector
writeback 时终止测试；它不将 vector payload 写入 semantic raw queue 或公共 transaction 状态。

```systemverilog
io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField =
    this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField;
io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField =
    this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField;

if (io_mem_to_ooo_writebackVldu_0_valid !== 1'b0 ||
    io_mem_to_ooo_writebackVldu_1_valid !== 1'b0) begin
    `uvm_fatal("MEMBLOCK_VEC_WB_UNSUPPORTED", "scalar-only flow observed writebackVldu")
end
```

中文伪代码：monitor 保留对两个 `vdIdxInField` 的采样，因为该字段仍是最新 DUT payload 的组成部分。
随后它继续对两个 valid 做四态安全检查：只要任一端口为 1、X 或 Z，就报告 scalar-only flow 不支持的
fatal；两个 valid 都为 0 时不生成 event，也不写 raw queue、status、pass/fail 或 terminal。已删除的
`vdIdx` 不再有 interface、transaction 或 monitor 读取者，因此不会在这个流程中留下悬空引用。

### 7.3 接线与 transaction 清理

源码位置：`mem_ut/ver/ut/memblock/tb/dut_inst.sv`、
`mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_vec_wb_agent_connect.sv` 和 vector-WB agent 的
interface/xaction。

抽象功能描述：这些声明与连接共同定义 DUT output 到 observer agent 的字段集合；本次使集合精确等于
生成后 Verilog，且不改变仍存在字段的位宽或方向。

```systemverilog
wire [2:0] io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField;
wire [2:0] io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField;

.io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField
    (io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField),
.io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField
    (io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField),
```

中文伪代码：顶层 testbench 只声明并实例化最新 RTL 仍提供的两个 `vdIdxInField` output。connect macro
把这两个 output 镜像到 agent interface，xaction 的 UVM field automation 只保留该字段，monitor 再从
clocking block 采样它。旧 `vdIdx` 的 wire、端口连接、force、field macro 和局部变量均被同时删除，
避免任一层仍访问不存在的 RTL 路径。

### 7.4 Plan 对齐、Review 与验证状态

相关 plan：
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_monitor_output_framework_adapt_execution_plan_20260708.md`。
该 plan 原有的 scalar-only vector valid gate 保持不变；本轮新写入的 `[IMPLEMENTATION_DELTA]` 仅记录
上游删除 `vdIdx` 后的字段集合闭合。

实现与原 plan 不一致的部分是原 plan 未枚举这两个具体 payload 字段，原因是字段删除来自最新 DUT。
处理结论是保留当前精确接口集合，不保留兼容字段。Plan 未说明但 Coding 落实的细节为四组 connect
force 与 xaction field automation 的同步删除；这些细节不新增任何运行期逻辑或性能路径。

主 agent review 已检查：两个已删除端口不存在于最新 RTL；`rg` 对 memblock 环境没有任何
`vdIdx(?!InField)` 残留；`vdIdxInField` 在 dut instance、connect、interface、xaction 和 monitor 中
仍完整；`git diff --check -- mem_ut/ver/ut/memblock` 通过。未运行远端 VCS，因为仍有 TopDown 与
`msiInfo` 顶层差异待处理，统一闭合后再执行编译/仿真。

## 8. 功能单元四：V2 TopDown 端口与 MSI 12 位 payload 适配

### 8.1 RTL 差异与职责分类

当前生成后的 `MemBlock.sv` 删除了 `io_topDownInfo_toBackend_lqEmpty`、
`io_topDownInfo_toBackend_sqEmpty` 和 input `io_topDownInfo_toBackend_noUopsIssued`；新增的
`io_topDownInfo_toBackend_replayAllocate`、`io_topDownInfo_toBackend_sqFull`、
`io_topDownInfo_toBackend_sbFull` 都是 1-bit DUT output。现有 mem_ut 中这六个名称只出现于
`dut_inst.sv`，没有 agent、sequence、RM、scoreboard 或状态机消费者。

同时，DUT top-level input `io_fromTopToBackend_msiInfo_bits` 和 output
`io_mem_to_ooo_topToBackendBypass_msiInfo_bits` 都从 13 位变为 12 位。前者没有 agent owner，当前
顶层初始块固定 `valid=0`、`bits=0`；后者由 `io_mem_to_ooo_ctrl_agent` 的 output observation/XZ
路径采样，analysis producer 仍为 deferred。

| 变化 | DUT 方向 | 测试框架处理 | 对运行期逻辑的影响 |
|---|---|---|---|
| `lqEmpty`、`sqEmpty` 删除 | output | 删除过时 wire/实例连接。 | 无消费者，不影响 sequence 或状态。 |
| `noUopsIssued` 删除 | input | 删除 reg、初始 0 tie-off 和实例连接。 | 不再向不存在的 DUT input 驱动默认值。 |
| `replayAllocate`、`sqFull`、`sbFull` 新增 | output | 新增 output wire/实例连接。 | 只保证 elaboration 闭合；不建立无消费者 agent。 |
| top-level/top-to-backend `msiInfo_bits` | input/output | input tie-off 与 control-agent 观察链统一为 12 位。 | 保持 valid=0 和 deferred monitor 行为；X/Z 检查缩为 12 位。 |

不新增 TopDown agent 的原因是三条新增端口都没有现有 UVM consumer，也不会反向影响 MemBlock。
创建 agent 会额外引入 transaction、analysis FIFO、RM 端口或无意义观察 loop，却不能为当前 scalar
dispatch、redirect、LSQ 或结束条件增加可验证行为；本轮明确保留为 top-level observation wire，并在
后续真正需要 performance/topdown checker 时再建立专项 owner。

### 8.2 MSI 位宽的 interface、transaction 与 monitor 同步

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_xaction.sv`，字段：
`io_mem_to_ooo_topToBackendBypass_msiInfo_bits`。

抽象功能描述：该字段承载 DUT 输出到 backend 的 MSI payload，供 control monitor 的观测 transaction
表示使用；它不驱动 DUT，当前也不触发 analysis-port 生产或公共状态更新。

```systemverilog
// 中文注释：V2 MemBlock 输出到 backend 的 MSI payload，最新 RTL 固定为 12 位。
// 该字段仅由 control monitor 采样；当前 deferred analysis 不把它驱动回 DUT。
rand bit [11:0] io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
```

中文伪代码：transaction 将 payload 宽度固定为 12 位，使 field automation、显示和比较不会再保留
不存在的 bit 12。字段只接受 monitor 从 DUT output 采集的值；它不会成为 driver 输入，也不会改变
当前 deferred analysis 的 producer 状态，因此本次位宽收窄不引入新的刺激或状态写者。

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv`，任务：
`io_mem_to_ooo_ctrl_agent_agent_monitor::mon_data()` 的 MSI 采样/XZ 分支。

抽象功能描述：该分支在 reset 后观察 top-to-backend MSI payload，并按实际总线宽度执行 X/Z 检查；
它不将观察值转换为 semantic raw event。

```systemverilog
logic [11:0] io_mem_to_ooo_topToBackendBypass_msiInfo_bits;

io_mem_to_ooo_topToBackendBypass_msiInfo_bits =
    this.vif.mon_mp.mon_cb.io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
`TCNT_CHECK_SIG_XZ(io_mem_to_ooo_topToBackendBypass_msiInfo_bits,
                   io_mem_to_ooo_topToBackendBypass_msiInfo_bits, 12);
```

中文伪代码：monitor 在既有 post-reset callback 读取 12 位 DUT output，再把完全相同的 12 位范围交给
X/Z 宏检查。检查通过后不入队、不修改 status；检查失败只沿用 monitor 的既有错误报告语义。由于
`io_fromTopToBackend_msiInfo_valid` 在当前 top-level tie-off 为 0，默认 sanity flow 不会凭空产生
MSI transaction。

### 8.3 TopDown 实例闭合

源码位置：`mem_ut/ver/ut/memblock/tb/dut_inst.sv`，`MemBlock U_MEMBLOCK` 的 TopDown port map。

抽象功能描述：该 port map 将生成后 Verilog 的纯 output 状态线暴露给 testbench，确保顶层实例与
DUT 集合一致；它不驱动 output，也不将这些性能/容量提示纳入验证框架状态机。

```systemverilog
wire io_topDownInfo_toBackend_replayAllocate;
wire io_topDownInfo_toBackend_sqFull;
wire io_topDownInfo_toBackend_sbFull;

.io_topDownInfo_toBackend_replayAllocate(io_topDownInfo_toBackend_replayAllocate),
.io_topDownInfo_toBackend_sqFull(io_topDownInfo_toBackend_sqFull),
.io_topDownInfo_toBackend_sbFull(io_topDownInfo_toBackend_sbFull),
```

中文伪代码：testbench 为三个新增 DUT output 各声明一个 wire，并将同名端口连接到该 wire。没有任何
driver、force 或 initial 赋值写这些 wire；它们仅保留输出观察可能性。旧 `lqEmpty`、`sqEmpty` 连接和
`noUopsIssued` 的 reg/初始赋值同时删除，因此不会存在旧 input tie-off 或失效 port map。

### 8.4 Plan 对齐与主 agent Review

关联 plan：
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_monitor_output_framework_adapt_execution_plan_20260708.md`。
该 plan 已新增 `[IMPLEMENTATION_DELTA]`，记录 MSI 12 位观察链与 TopDown 无消费者边界。原 plan
未逐项枚举这些上游端口差异，属于最新 DUT 驱动的实现补充；处理结论为保持当前精确端口集合，不保留
13-bit compatibility field 或已删除 TopDown input。

主 agent 最终静态 review 已完成：生成 RTL 与 `dut_inst.sv` 的动态端口集合比较结果为空；旧
TopDown 名称零残留；所有相关 MSI 声明、transaction、interface、monitor local 和 X/Z width 都是
12；`git diff --check -- mem_ut/ver/ut/memblock AI_DOC/plan/test_framework` 通过。此单元不增加
高频扫描、queue、map 或 lifecycle owner。下一步执行远端 VCS compile/run，以验证全部已闭合的
顶层端口和 SystemVerilog 类型。
