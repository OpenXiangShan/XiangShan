# mem_ut V2 编译期参数与宽度适配执行 Plan

## 1. Plan 定位

本文是 V2/V3 静态结构和 `FuType` 编码差异的测试框架执行 plan。目标是在不改变测试框架主体调度、状态表和 handler 架构的前提下，把 ROB/LQ/SQ index 宽度、DUT 端口 `fuType` 宽度及 one-hot bit、LSQ enqueue slot/握手风格、issue pipe/port style 和 SQ deq pointer capability 收敛到单一权威入口，并让所有 directed ROB value 构造经过同一个无静默截断的 package helper。

本 plan 主要处理编译期结构参数和 runtime 参数 clamp，同时负责按 V2 当前未闭环范围收敛 software dispatch smoke 默认场景，并在运行期参数加载和主表构造边界检测 AMO/CBO；不实现 LSQ enqueue、issue、writeback、L2TLB 或 monitor 的其它业务逻辑修改，这些内容由后续专项 plan 执行。

本 compile/width 专项是第5章已列且第12章已完成的既有 width/port 参数、
`seq_pkg::fit_directed_rob_value_or_fatal()` 共享实现、real manual 与 software smoke 两个
`make_directed_transaction()` 的 ROB value fit、V2 AMO/CBO运行期检测，以及 V2 software dispatch smoke
从 load/store/AMO 三笔收敛为 load/store 两笔的唯一 owner。其它专项只能消费这些已产出的
localparam、helper 和 smoke 结果，不得复制 fit helper、再次用 slice 转换 directed ROB value，也不得
保留或重新引入 software smoke 的 AMO/MOU 第三笔；归档后新增的其它结构宏不自动归入本 plan。

执行状态：已完成，完成日期为2026-07-13。源码、参数、同步文档和验证结果见第12章；
归档后本文件位于`AI_DOC/plan/test_framework/plan/do`。

归档后依赖边界：后续 LSQ MMIO/status `undo` plan 会在同一个
`memblock_compile_params.svh` 中新增 SQ deq count、cancel count 和 redirect/cancel 时序宏。
这些尚未实现的宏、派生公式和 consumer 修改由该 `undo` plan 唯一拥有，不属于本 `do` plan
的已完成范围；本文件只保留“所有硬件结构参数继续进入统一 compile header、不得建立 runtime
plus 第二权威”这一公共约束。

## 2. 范围边界

涉及文件：

```text
mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh
mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/main_control_transaction.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/status_transaction.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv
mem_ut/ver/ut/memblock/env/plus.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv
mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg
mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv
mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/tb/lsqcommit_agent_connect.sv
mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/tb/lintsissue_agent_connect.sv
mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/tb/vecissue_agent_connect.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_iq_feedback_agent_agent/src/io_mem_to_ooo_iq_feedback_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_iq_feedback_agent_agent/src/io_mem_to_ooo_iq_feedback_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_iq_feedback_agent_agent/src/io_mem_to_ooo_iq_feedback_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_iq_feedback_agent_connect.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_int_wb_agent_connect.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_ctrl_agent_connect.sv
mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/tb/redirect_agent_connect.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_vec_wb_agent_agent/src/io_mem_to_ooo_vec_wb_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_vec_wb_agent_agent/src/io_mem_to_ooo_vec_wb_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_vec_wb_agent_agent/src/io_mem_to_ooo_vec_wb_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_vec_wb_agent_connect.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/issue_field_assigner.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv
mem_ut/ver/ut/memblock/seq/base_seq/memblock_issue_dispatch_base_sequence.sv
mem_ut/ver/ut/memblock/seq/base_seq/memblock_redirect_dispatch_base_sequence.sv
mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_manual_main_table_sequence.sv
mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv
AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md
AI_DOC/mem_ut_flow_doc/lsq_admission_flow.md
AI_DOC/mem_ut_flow_doc/soft_test_and_mixed_directed_flow.md
AI_DOC/plan/test_framework/plan/undo/mem_ut_test_framework_todo_20260614.md
```

`load_sta_std_issue_flow.md`和`lsq_admission_flow.md`是本plan物理资源参数迁移的同步写目标，必须反映scheduler、slot清理和runtime limit的最终参数来源；`soft_test_and_mixed_directed_flow.md`仍只作software smoke原行为核对输入。`mem_ut_test_framework_todo_20260614.md`是Atomic/MOU与CBO完整闭环的同步写目标；本plan不修改其它flow或TODO文档。

只允许做：

- 复用现有 `MEMBLOCK_DUT_*` 宏体系，只补缺失的 V2/V3 静态结构、port-style capability 和 `FuType` 编码宏；不得建立第二套 `*_CFG` 权威。
- 将 `MEMBLOCK_ROB_VALUE_W`、`MEMBLOCK_LQ_VALUE_W`、`MEMBLOCK_SQ_VALUE_W`、DUT 端口 `fuType` 宽度/编码、LSQ enqueue物理slot、LOAD/STA/STD物理pipe数及 fired-mask layout 改为消费编译期宏/localparam。
- 从 `plus.sv/default.cfg/seq_csr_common.sv` 删除五个 `MEMBLOCK_REAL_*` 物理结构参数、加载、快照和getter，不保留兼容字段、兼容getter或旧参数命令行检测；原consumer直接改读compile宏/localparam。
- 在 `seq_csr_common::apply_runtime_resource_limits()` 中集中用编译期真实上限处理 `MEMBLOCK_ENQ_PER_CYCLE` 和三类 `*_PIP_NUM_LIMIT`，由 `validate_and_clamp()` 唯一调用。
- `check_compile_param_consistency()` 只做纯检查，不得写任何 runtime 字段；所有 runtime资源fatal/clamp只发生在 `seq_csr_common::apply_runtime_resource_limits()`，其它参数检查仍由 `validate_and_clamp()`编排。
- `seq_pkg` 内所有结构 consumer 直接读取 `memblock_dispatch_types.sv` 的 package localparam；不得新增 `get_compile_*()` getter。agent package 和 `memblock_sync_pkg` 因编译顺序不能依赖 `seq_pkg`，只能直接消费同一组 `MEMBLOCK_DUT_*` 宏，不得自建数值常量。
- 在 `memblock_dispatch_types.sv` 新增唯一 package function `fit_directed_rob_value_or_fatal()`；real manual 与 software smoke 的 directed transaction builder 都必须调用它，两个 class 内不得各自实现 fit 或保留任何低位 slice。
- 将 V2 software dispatch smoke 默认主表固定为一笔 load 和一笔 store，删除 AMO 第三笔及 AMO/MOU 构造分支，并同步两笔 transaction、三条 issue target、两笔 ROB commit、LQ/SQ 各 deq 一笔和 final 状态检查的验收口径。
- runtime plus只保留 `MEMBLOCK_ENQ_PER_CYCLE`、三类 `*_PIP_NUM_LIMIT`及随机开关，表达“本 testcase 本拍入队数量/本拍最多使用多少pipe”，不得再表达物理slot/pipe总数。

不允许做：

- 用 runtime plusarg 改变 packed typedef 宽度、interface 数组维度或 connect-time 端口数。
- 为 V2/V3 宽度差异引入两套状态表。
- 通过截断赋值掩盖位宽不一致。
- 把测试框架内部公共 `fuType` 容器宽度缩成 V2 DUT 端口宽度。内部容器仍保留最大 36-bit，但其中的 one-hot 常量必须按当前版本编码；不能继续使用 V3 bit16/17/18 后只裁剪成 V2 35 bit。
- 在两个 directed builder 中分别复制范围检查，或用固定/参数化低位 slice 替代 `fit_directed_rob_value_or_fatal()`。
- 为保持 software smoke 三笔数量而把 AMO 改写成 MOU、CBO、普通 store 别名或任何其它第三笔；V2 默认 smoke 只能包含 load/store 两笔。

### 2.1 执行前 RTL 基线确认

执行本 plan 前必须从仓库根目录确认当前 V2 RTL 权威输入真实存在：

```bash
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

V2 权威 RTL 只取当前 profile 指定的 `build_memblock/rtl`。`build/rtl/MemBlock.sv` 可作差异辅助输入，但不是必须存在的第二权威源；两者分叉时必须以 V2 profile 和 `build_memblock/rtl/filelist.f` 为准。不得沿用不存在的 `MemBlockTop.sv` 或同级旧 worktree。

## 3. 问题依据

V2 接口分析记录：

- V2 `robIdx_value` 多处为 8 bit，V3 多处为 9 bit。
- V2 `enqLsq_req_*_bits_fuType` 为 `[34:0]`，V3 为 `[35:0]`。
- V2 `ftqOffset` 常见 4 bit，V3 常见 5 bit。
- V2 `io_mem_to_ooo_memoryViolation_bits_ftqOffset` 在 `build_memblock/rtl/MemBlock.sv` 中明确为 `[3:0]`；当前 ctrl agent interface/xaction/monitor 仍为 `[4:0]`，monitor 的 X/Z 检查也仍写宽度 5，属于必须修正的 V2 字段宽度错误。
- V2 issue 端口为 `issueLda[0..2]`、`issueSta[0..1]`、`issueStd[0..1]`，LSQ enqueue 当前为 6 slot。
- V2 顶层 LSQ enqueue 只有已经由 `LsqEnqCtrl` 接受并分配 key 后的一拍 `needAlloc/req`，没有顶层 canAccept/response；V2 顶层 SQ deq 只有 `sqDeq` count，没有 `sqDeqPtr`。
- V2 `FuType` 不是 V3 编码的低 35 bit。V2 `FuType.scala` 中 LDU/STU/MOU 是 one-hot bit15/16/17，VLDU/VSTU/VSEGLDU/VSEGSTU 是 bit31/32/33/34；当前测试框架常量为 bit16/17/18 和 bit32/33/34/35，整体错一位。

当前源码问题：

- `memblock_compile_params.svh` 已有 `MEMBLOCK_DUT_ROB/LQ/SQ_SIZE`、`MEMBLOCK_DUT_COMMIT_WIDTH` 和 `MEMBLOCK_DUT_ROB/LQ/SQ_VALUE_W`；这些是现有权威宏，不得再新增同义 `*_CFG` 宏。
- `memblock_dispatch_types.sv` 已从上述宏派生 key value 宽度，但 `MEMBLOCK_FUTYPE_*` 仍硬编码成 V3 one-hot bit；只做 36->35 bit fit 会把 V2 LDU 当成 STU、STU 当成 MOU，属于功能语义错误。
- `main_control_transaction.sv`、`status_transaction.sv` 以及 ctrl agent MMIO ROB value 字段仍有 `[6:0]/[5:0]/[7:0]` 等硬编码，需要纳入同一宽度审计。
- `memblock_sync_pkg::dispatch_raw_int_wb_t` 和 `dispatch_raw_iq_feedback_t` 的 `lq_value/sq_value`、`dispatch_raw_ctrl_t` 的 LQ/SQ deq pointer value 仍硬编码为 7/6 bit；raw queue 是 monitor 与 adapter 的公共传递边界，必须和 agent 及公共 key typedef 同源。
- IQ feedback、int-WB、lintsissue/vecissue、ctrl、redirect、LSQ enqueue/commit 和 vector-WB agent 的 interface/xaction/monitor 局部变量、X/Z 检查宽度及 connect 端点仍存在直接数值或只完成部分宏化的情况，必须按第 5.2 节逐链审计，不能只修改主表 transaction。
- 旧源码在 `plus.sv/default.cfg/seq_csr_common.sv` 中同时保存 `MEMBLOCK_REAL_LSQ_ENQ_MAX`、`MEMBLOCK_REAL_ENQ_WIDTH` 和三个 `MEMBLOCK_REAL_*_PIPE_NUM`，允许runtime参数形成第二套物理结构权威；本轮已确认这些参数只有default cfg，没有testcase preset覆盖，可以直接删除而不保留兼容快照。
- `MEMBLOCK_OP_CLASS_AMO_WT` 当前在 `plus.sv`、`default.cfg` 和 `seq_csr_common` 的默认值均为 1；但当前 V2 scalar enqueue/issue/writeback 没有 atomic 闭环，默认仍可能构造 MOU/AMO。本轮不新增 compile capability，而是在运行期参数加载和主表构造边界明确检测并拒绝。
- `MEMBLOCK_OP_CLASS_CBO_WT` 当前默认值虽为 0，但 `apply_minimal_op_template()`、boundary candidate cache 和 `validate_main_table_entry()` 都能合法构造/接纳 CBO；manual/fixed transaction 或用户显式非零权重可使 CBO 在 issue fail-fast 前先完成主表落表和 LSQ enqueue。本轮同样不新增 compile capability，改为参数初始化和主表构造边界双重运行期检测。
- `memblock_main_dispatch_manual_main_table_sequence::make_directed_transaction()` 当前把 `int unsigned rob_value` 固定切成 `rob_value[8:0]`；`soft_test_memblock_dispatch_smoke_sequence::make_directed_transaction()` 当前使用 `rob_value[MEMBLOCK_ROB_VALUE_W-1:0]`。两种写法都会在检查原始输入前静默截断，且两个 builder 分别承担转换，必须改成共同调用唯一 package helper，先检查可表示范围、再显式转换为 `MEMBLOCK_ROB_VALUE_W`。
- `soft_test_memblock_dispatch_smoke_sequence` 当前把 `dispatch_smoke_trans_num` 固定为 3，并构造 load/store/AMO 三笔主表；其中 AMO 使用 `MEMBLOCK_FUTYPE_MOU` 和 `MEMBLOCK_LSQ_FLOW_ATOMIC`。当前 V2没有 atomic闭环，默认 software smoke继续构造该功能会导致无法闭环，必须删除第三笔和 AMO case，而不是把它替换成 MOU 或其它第三笔。
- V2 当前实际 LSQ enqueue 口径必须按当前 RTL 和 agent 确认。已知当前 connect/xaction 使用 slot 0..5 共 6 个，执行前必须用 `rg` 确认，不得被旧分析中的 req_6/7 误导。

## 4. 修改原因

V2/V3 静态结构差异决定编译后 SystemVerilog 类型、interface 字段宽度和 case 分支范围。若继续把这些结构写成 runtime plus 或散落硬编码，后续 LSQ enqueue、issue、writeback 适配会出现三类问题：

1. V2 下字段宽度和 typedef 不一致，编译或连接阶段可能出现截断/扩展风险。
2. V3 下回归时被 V2 临时默认值影响。
3. 各 flow 自己各改一处，会让公共状态表 key 宽度、raw queue 宽度和 agent 字段宽度语义分裂。

## 5. 修改后方案

### 5.1 编译期宏

在 `memblock_compile_params.svh` 复用现有宏，并只新增缺失的带默认值宏。权威集合固定为：

```text
MEMBLOCK_DUT_ROB_SIZE                 // 已有
MEMBLOCK_DUT_LQ_SIZE                  // 已有
MEMBLOCK_DUT_SQ_SIZE                  // 已有
MEMBLOCK_DUT_COMMIT_WIDTH             // 已有
MEMBLOCK_DUT_ROB_VALUE_W              // 已有
MEMBLOCK_DUT_LQ_VALUE_W               // 已有
MEMBLOCK_DUT_SQ_VALUE_W               // 已有
MEMBLOCK_INTERNAL_FUTYPE_W
MEMBLOCK_DUT_FUTYPE_W
MEMBLOCK_DUT_FUTYPE_LDU_BIT
MEMBLOCK_DUT_FUTYPE_STU_BIT
MEMBLOCK_DUT_FUTYPE_MOU_BIT
MEMBLOCK_DUT_FUTYPE_VLDU_BIT
MEMBLOCK_DUT_FUTYPE_VSTU_BIT
MEMBLOCK_DUT_FUTYPE_VSEGLDU_BIT
MEMBLOCK_DUT_FUTYPE_VSEGSTU_BIT
MEMBLOCK_DUT_FTQ_PTR_VALUE_W
MEMBLOCK_DUT_FTQ_OFFSET_W
MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM
MEMBLOCK_DUT_LOAD_PIPE_NUM
MEMBLOCK_DUT_STA_PIPE_NUM
MEMBLOCK_DUT_STD_PIPE_NUM
MEMBLOCK_DUT_LOAD_PORT_BASE
MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM
MEMBLOCK_DUT_ISSUE_PORT_STYLE_SPLIT
MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP
MEMBLOCK_DUT_HAS_SQ_DEQ_PTR
```

V2 默认固定为：

```text
MEMBLOCK_DUT_ROB_VALUE_W = 8
MEMBLOCK_DUT_LQ_VALUE_W = 7
MEMBLOCK_DUT_SQ_VALUE_W = 6
MEMBLOCK_INTERNAL_FUTYPE_W = 36
MEMBLOCK_DUT_FUTYPE_W = 35
MEMBLOCK_DUT_FUTYPE_LDU/STU/MOU_BIT = 15/16/17
MEMBLOCK_DUT_FUTYPE_VLDU/VSTU/VSEGLDU/VSEGSTU_BIT = 31/32/33/34
MEMBLOCK_DUT_FTQ_PTR_VALUE_W = 6
MEMBLOCK_DUT_FTQ_OFFSET_W = 4
MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM = 6
MEMBLOCK_DUT_LOAD/STA/STD_PIPE_NUM = 3/2/2
MEMBLOCK_DUT_LOAD_PORT_BASE = 0
MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM = 3
MEMBLOCK_DUT_ISSUE_PORT_STYLE_SPLIT = 1
MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP = 0
MEMBLOCK_DUT_HAS_SQ_DEQ_PTR = 0
```

V3 profile 覆盖时必须显式提供自身 width/bit/port-style 值。特别是 V3 LDU/STU/MOU bit16/17/18 不能与 V2 共用；不能通过运行期 plusarg 选择编码。

`MEMBLOCK_DUT_LOAD_PORT_BASE` 的参数合同固定如下，且只在本 compile/width plan coding：

```text
memblock_compile_params.svh：
  `ifndef MEMBLOCK_DUT_LOAD_PORT_BASE
  `define MEMBLOCK_DUT_LOAD_PORT_BASE 0
  `endif

memblock_dispatch_types.sv：
  localparam int unsigned MEMBLOCK_DUT_LOAD_PORT_BASE =
      `MEMBLOCK_DUT_LOAD_PORT_BASE;
```

输入是编译命令或版本 profile 在宏默认生效前提供的可选 compile-time override；无 override 时宏输出 V2 默认 0，同名 package localparam只读暴露最终宏值。两层都不读写 runtime cfg、queue、map、transaction或状态。重复源码定义、宏/localparam不同源、base/count/total区间不自洽时由静态唯一性检查或 `check_compile_param_consistency()` fatal；不得 fallback到第二常量。split issue plan及其它 consumer只能读取该 localparam或同源宏，不能复制默认值。

### 5.2 类型统一

在 `memblock_dispatch_types.sv` 中：

- `MEMBLOCK_ROB_VALUE_W` 等 localparam 从宏派生。
- 新增 `MEMBLOCK_FTQ_PTR_VALUE_W`、`MEMBLOCK_FTQ_OFFSET_W`、`MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM`、`MEMBLOCK_DUT_LOAD_PIPE_NUM`、`MEMBLOCK_DUT_STA_PIPE_NUM`、`MEMBLOCK_DUT_STD_PIPE_NUM`、`MEMBLOCK_DUT_LOAD_PORT_BASE` 等 package localparam。`seq_pkg` 内的 sequence/helper 直接读这些 localparam，禁止再包一层 `get_compile_*()`。
- 保留 `MEMBLOCK_INTERNAL_FUTYPE_W=36` 作为跨版本最大容器；`MEMBLOCK_FUTYPE_*` 用 `1 << MEMBLOCK_DUT_FUTYPE_*_BIT` 生成当前版本 one-hot 值。`lsq_ctrl_model::derive_op_behavior()`、`is_vector_ls_futype()`、主表默认值和 directed helper 必须全部消费这些版本化常量。
- `MEMBLOCK_DUT_FUTYPE_W` 仅表示 DUT-facing width。所有写入 DUT `fuType` 的位置调用 `encode_and_fit_dut_futype()`；helper 先验证当前版本合法 one-hot/语义，再检查宽度，不能只检查高位是否为 0。
- LSQ enqueue、STA/STD issue 的 DUT-facing interface/xaction/driver/monitor 字段统一使用 `MEMBLOCK_DUT_FUTYPE_W`；主表和公共状态仍使用 36-bit 容器。
- `main_control_transaction`、`status_transaction`、raw struct 和 ctrl agent 的 ROB/LQ/SQ value 字段必须消费 `MEMBLOCK_DUT_*_VALUE_W`。MMIO load 端口循环/packed array消费 `MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM`，不得保留 `[7:0]` 和固定 3 作为第二权威。
- 所有 raw key typedef 继续从 `MEMBLOCK_*_VALUE_W` 派生。

宽度 consumer 必须按以下完整链路修改或确认。表中的“宏”指跨 package 的 `MEMBLOCK_DUT_*` 宏；“localparam”指 `memblock_dispatch_types.sv` 中从同一宏派生的 package localparam。

| consumer 链 | 必须修改/确认的字段 | 唯一宽度来源和 V2 值 |
|---|---|---|
| `memblock_sync_pkg.sv` | `dispatch_raw_int_wb_t`、`dispatch_raw_iq_feedback_t` 的 `rob/lq/sq_value`；`dispatch_raw_ctrl_t` 的 `lq/sq_deq_ptr_value` 和 memoryViolation ROB/FTQ 字段 | package 编译顺序早于 `seq_pkg`，直接用宏；ROB/LQ/SQ=8/7/6，FTQ ptr/offset=6/4 |
| `main_control_transaction.sv`、`status_transaction.sv` | ROB/LQ/SQ value 字段和 getter/setter 返回 key | `MEMBLOCK_ROB/LQ/SQ_VALUE_W` localparam |
| LSQ enqueue agent + connect | 六个 req 的 ROB/LQ/SQ 和 `fuType`，driver/monitor X/Z 宽度 | ROB/LQ/SQ=8/7/6，DUT FuType=35；slot 数=6 |
| LSQ commit agent + connect | `pendingPtr_value`、LQ/SQ deq pointer value，所有 driver/monitor 局部变量和 X/Z 宽度 | pendingPtr 使用 ROB value width=8；LQ/SQ=7/6；`sqDeqPtr` 是否存在由 capability 决定 |
| lintsissue agent + connect | LDA/STA/STD ROB/LQ/SQ、`waitForRobIdx`、FTQ ptr/offset、STA/STD `fuType`，monitor X/Z 参数 | ROB/LQ/SQ=8/7/6，FTQ=6/4，FuType=35 |
| split issue fired-mask consumer | lintsissue xaction mask宽度、sequence `port_idx_for_item()`/`mark_fired_items()`/full-mask、driver ready bit和fire report | 只读 package `MEMBLOCK_DUT_LOAD/STA/STD_PORT_BASE`、pipe count、total/mask width；LOAD base默认与同名localparam由本 plan唯一拥有，split plan不得复制默认值 |
| vecissue agent + connect | `issueVldu` 的 ROB/LQ/SQ、FTQ ptr/offset 和 `fuType` 字段，仅做编译宽度一致性 | 同上；功能仍由 split issue plan 固定 fatal |
| IQ feedback agent + connect | STA/VSTU feedback 的 LQ/SQ value、monitor 局部变量及 X/Z 参数 | LQ/SQ=7/6；STA SQ-only raw和VSTU valid fatal由IQ feedback/replay专项唯一处理 |
| int-WB agent + connect | writebackLda/Sta/Std 的 ROB value、interface/xaction/monitor 声明及 X/Z 参数 | ROB=8；STD value-only 语义不因宽度适配而伪造 flag |
| ctrl agent + connect | memoryViolation ROB、`ftqIdx_value`、`ftqOffset`，load/store MMIO uop ROB value，monitor X/Z 参数 | ROB=8，FTQ ptr=6，`memoryViolation.ftqOffset=4`；禁止保留 `[4:0]` 或 X/Z width 5 |
| redirect agent + connect | redirect ROB value 的 interface/xaction/driver/monitor 和约束 | ROB=8；当前已宏化的字段保留并纳入静态检查 |
| vector-WB agent + connect | `writebackVldu` ROB value | ROB=8；本plan只做编译宽度一致性，valid的scalar-mode fatal由monitor output专项唯一实现 |
| `common_data_transaction`、`lsq_ctrl_model`、`issue_queue_scheduler`、`lsq_commit_handler`、`dispatch_monitor_event_adapter`、redirect sequence | 任何直接声明、slice、format 或临时 key | 只使用 package key typedef/localparam；不得保留 `[7:0]/[6:0]/[5:0]` 作为结构权威 |
| `seq_pkg::fit_directed_rob_value_or_fatal()`、real manual 与 software smoke 两个 `make_directed_transaction()` | `int unsigned rob_value` 到 `tr.robIdx_value` 的 directed 输入转换 | package helper 是唯一转换 owner；先按 `MEMBLOCK_ROB_VALUE_W` 检查原值可表示范围，再做显式 sized cast；两个 builder 只调用 helper，禁止固定 `[8:0]`、参数化低位 slice 或任何静默截断 |

每个 agent 链都必须同时检查四类位置：interface/xaction 声明、driver/monitor 临时变量、`TCNT_CHECK_SIG_XZ` 宽度实参、connect 是否存在显式 slice/补零。只改 interface 声明而保留 monitor `8/7/6/5` 常量不算完成。

issue 物理布局 localparam 从 pipe 数派生：

```text
MEMBLOCK_DUT_LOAD_PORT_BASE = `MEMBLOCK_DUT_LOAD_PORT_BASE
MEMBLOCK_DUT_STA_PORT_BASE = MEMBLOCK_DUT_LOAD_PORT_BASE + MEMBLOCK_DUT_LOAD_PIPE_NUM
MEMBLOCK_DUT_STD_PORT_BASE = MEMBLOCK_DUT_STA_PORT_BASE + MEMBLOCK_DUT_STA_PIPE_NUM
MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM = MEMBLOCK_DUT_STD_PORT_BASE + MEMBLOCK_DUT_STD_PIPE_NUM
MEMBLOCK_DUT_SCALAR_ISSUE_MASK_W = MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM
```

`MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM` 在该布局中是物理 mask 的独占索引上界；三个连续区间覆盖 `[MEMBLOCK_DUT_LOAD_PORT_BASE, MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM)`。本 compile/width plan 是 `MEMBLOCK_DUT_LOAD_PORT_BASE` 的唯一 owner：`memblock_compile_params.svh` 定义默认值 0，`memblock_dispatch_types.sv` 暴露同名 package localparam。后续 split issue plan 的 fired-mask、target offset、full-mask 和 driver bit mapping只能消费这些 localparam，不得重复定义默认值或建立第二 owner；port style 由 capability 宏隔离，不能把 V3 聚合 `intIssue` 当成 V2 split issue 的同形数组。

### 5.3 runtime 参数 clamp

`seq_csr_common::validate_and_clamp()` 仍是 runtime 参数初始化总入口，资源数量收敛统一下沉到同文件的 `apply_runtime_resource_limits()`。处理顺序固定为：

- 先调用纯检查 `check_compile_param_consistency()`，确认 compile localparam 本身非 0、FuType bit 唯一且可被 DUT width 表达、port base/total 算术一致。该调用不写任何字段。
- `plus::reload_from_cmdline()` 不再声明或加载五个已删除的 `MEMBLOCK_REAL_*` 名称，也不增加旧名称扫描；合法runtime参数继续按原路径加载。
- `MEMBLOCK_ENQ_PER_CYCLE` 必须位于 `[1:MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM]`，越界表示固定模式激励配置错误，保持fatal，不做clamp。
- `load_pip_num_limit` clamp 到 `MEMBLOCK_DUT_LOAD_PIPE_NUM`。
- `sta_pip_num_limit` clamp 到 `MEMBLOCK_DUT_STA_PIPE_NUM`。
- `std_pip_num_limit` clamp 到 `MEMBLOCK_DUT_STD_PIPE_NUM`。
- `MEMBLOCK_ENQ_PER_CYCLE_RAND_EN=1` 时，`get_enq_per_cycle()`继续按原语义在 `[1:MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM]` 随机，不把固定模式的 `MEMBLOCK_ENQ_PER_CYCLE` 改成第二个物理宽度。
- 物理slot循环、slot越界检查和 scheduler 每拍route扫描上限直接读取 package localparam，不经过 `seq_csr_common` getter。
- 结构数量小于1的compile配置由纯检查fatal；pipe runtime使用量小于1或超过物理上限按现有 `clamp_int()` warning+clamp。
- 当前 V2只要 `op_class_amo_wt!=0` 或 `op_class_cbo_wt!=0`，就在运行期初始化直接 `uvm_fatal`，不得静默改回 0。两项分别报告，避免把显式 CBO 请求误归类为 atomic；该检测不依赖任何 compile capability。
- 资源函数返回后，`validate_and_clamp()`继续处理地址、权重、timeout和未支持功能检测，不再散落第二组slot/pipe clamp。

### 5.4 scalar atomic/CBO运行期检测和默认参数

本轮 V2固定不支持 scalar atomic/MOU，但不增加编译期 capability：

1. `memblock_compile_params.svh` 和 `memblock_dispatch_types.sv` 不新增 atomic support capability宏/localparam，也不在 `check_compile_param_consistency()` 中检查 atomic支持状态。
2. `plus.sv`、`seq_csr_common.sv` 的静态默认和 `seq/plus_cfg/default.cfg` 将 `MEMBLOCK_OP_CLASS_AMO_WT` 默认值统一改为 0。
3. `seq_csr_common::validate_and_clamp()` 在 V2加载后的 `op_class_amo_wt!=0` 时直接 `uvm_fatal`，不得静默 clamp 为 0。AMO各fuOpType子权重可保留为dormant配置，但本轮不会消费。
4. `memblock_dispatch_base_sequence::validate_main_table_entry()` 无条件拒绝 `op_class==AMO`、`fuType==MOU`或`lsq_flow==ATOMIC`。`fuOpType`只有在这些字段已经确定AMO语义后才由后续模板校验解释；不能脱离op class/FuType/flow独立识别，因为普通load/store与AMO复用部分LSUOp数值。该运行期入口同时覆盖随机主表、boundary cache结果、manual table和任何fixed/directed transaction。
5. `soft_test_memblock_dispatch_smoke_sequence` 删除固定 AMO 第三笔，`dispatch_smoke_trans_num` 从 3 改为 2，只构造 load/store；其 `make_directed_transaction()` 删除 AMO case，任何 AMO/MOU 输入都由 default 分支 fatal，不允许用 MOU 或其它 op class 补足第三笔。
6. software smoke 的 admission、required-target 和 final-status 循环继续以 `data.main_trans_num` 为上界，因此导入两笔后只遍历 uid 0..1；ROB commit 期望值继续读取 `dispatch_smoke_trans_num`，随字段变为 2；LQ/SQ 各只有一笔分配，仍分别执行 `apply_dut_lq_deq(1, ...)` 和 `apply_dut_sq_deq(1, ...)`。
7. enqueue 和 split issue 专项继续各自保留下游 fail-fast；即使某个调用者绕过主表 validation，也不能把 MOU 送入 V2 scalar DUT flow。

本轮同时固定不支持 scalar CBO，也只做运行期检测：

1. `memblock_compile_params.svh` 和 `memblock_dispatch_types.sv` 不新增 CBO support capability宏/localparam，也不在 `check_compile_param_consistency()` 中检查CBO支持状态。
2. `MEMBLOCK_OP_CLASS_CBO_WT` 在 `plus.sv`、`seq_csr_common.sv` 和 `default.cfg` 中继续保持默认 0；本 plan 不把默认 0误写成“当前无法构造 CBO”。
3. `seq_csr_common::validate_and_clamp()` 在 V2加载后的 `op_class_cbo_wt!=0` 时直接 `uvm_fatal`，不静默 clamp，不等待主表生成或 issue 阶段再失败。
4. `validate_main_table_entry()` 在任何 `derive_op_behavior()`、写入 `main_table_by_uid`、置 `main_table_ready` 和 LSQ admission 之前，CBO 前置检测只检查 `tr.op_class==MEMBLOCK_OP_CLASS_CBO`、`tr.lsq_flow==MEMBLOCK_LSQ_FLOW_CBO` 和 `lsq_ctrl_model::is_cbo_fuoptype(tr.fuOpType)`，任一命中即拒绝。这三项足以覆盖 CBO；CBO 与普通 store 共用 STU，不能用 `tr.fuType==MEMBLOCK_FUTYPE_STU` 单独判定 CBO。通过该检测后才调用 `derive_op_behavior()`；random、boundary、manual、fixed/directed/import 路径必须共用该检测。
5. enqueue 和 split issue 保留下游 CBO fail-fast，作为绕过主表校验时的防御边界；它们不能替代 admission 前检测。

software prefetch 不受 atomic/CBO运行期检测影响。`MEMBLOCK_OP_CLASS_PREFETCH_WT` 默认继续为 1，LDU + prefetch fuOpType 继续得到 `MEMBLOCK_OP_BEHAVIOR_PREFETCH/is_prefetch=1` 并只 route 到 LOAD/`issueLda`。

该修改会改变atomic默认生成行为以及atomic/CBO非法配置失败策略，但不改变普通load/store/prefetch的主表生成算法、权重选择算法或状态机。Atomic/MOU和CBO完整闭环分别记录在`AI_DOC/plan/test_framework/plan/undo/mem_ut_test_framework_todo_20260614.md`第2章和第6.3节；闭环完成前保持本节运行期拒绝，完成后删除拒绝分支，不新增compile capability开关。

### 5.5 共享 directed ROB fit 与 software smoke 收敛

`seq_pkg::fit_directed_rob_value_or_fatal()` 固定实现在 `memblock_dispatch_types.sv` 的 ROB/LQ/SQ width localparam 之后。该文件在 `seq_pkg.sv` 中先于 transaction 和所有 sequence class include，因此 real manual 与 software smoke 两个 builder 都能直接调用同一 package function；不新增文件、不调整 `seq_pkg.sv`/`seq.f` include 顺序，也不创建 class-local wrapper。

helper 使用 `automatic` function，输入只包含 `int unsigned value` 和诊断用 `string context`。它先确认 `MEMBLOCK_ROB_VALUE_W` 大于 0且小于 `longint unsigned` 位数，再用 64-bit 值计算独占上限 `2**MEMBLOCK_ROB_VALUE_W`；只有原始输入小于该上限时，才执行 `MEMBLOCK_ROB_VALUE_W'(value)` 显式 sized cast 并返回。函数不读写 transaction、queue、map、counter、runtime snapshot 或任何 package/class 状态；非法 width 或 value 直接 `uvm_fatal`，没有 clamp、fallback、饱和、低位截断或失败返回值。

两个 `make_directed_transaction()` 都先调用该 helper 获取同宽局部值，再写 `tr.robIdx_value`。real manual 继续只接受 load/store。software smoke 同样只接受 load/store，并完成以下固定场景：

```text
构造阶段：
  new()把dispatch_smoke_trans_num设为2；
  build_directed_main_table()清空旧表；
  uid 0构造load，uid 1构造store；
  不构造uid 2，不调用AMO，不写MEMBLOCK_FUTYPE_MOU；
  import_manual_main_table()导入后令data.main_trans_num为2。

软件闭环阶段：
  admission和required-target循环只遍历uid 0..1；
  load产生1条LOAD target，store产生1条STA和1条STD target；
  三条target全部fire并各注入一条pass writeback event；
  commit检查恰好2个uid可提交；
  LQ按count=1 deq，SQ按count=1 deq；
  final检查两笔transaction均terminal_done，active ROB/LQ/SQ map为空，LQ/SQ free count恢复满值；
  全部检查通过后正常退出并打印software smoke completed。

失败和退出：
  directed ROB width/value非法、transaction创建失败、AMO或其它未支持op class、issue无进展、commit数不是2、final状态或free count不一致时立即fatal；
  不通过timeout、fallback、第三笔占位transaction或MOU替代退出；
  正常退出只发生在两笔transaction完成既有commit/deq/final闭环之后。
```

### 5.6 执行前 slot 口径确认

执行本 plan 前必须确认当前实际 slot 最大编号：

```bash
rg -n "enqLsq_req_[0-9]+|enqLsq_needAlloc_[0-9]+" \
  build_memblock/rtl/MemBlock.sv \
  mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv \
  mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src
```

验收口径以当前存在且实际连接的最大 slot 为准。当前已知 V2 环境按 slot 0..5 共 6 个执行；如果重新生成 RTL 后出现差异，本 plan 必须停止并报告基线不一致，由另行授权的 profile/接口适配任务先建立新基线。本 plan 执行过程不得修改 profile 或其它 plan。

### 5.7 load/store 地址复用 Flow 不变边界

`apply_addr_reuse_window()` 属于 `build_random_main_table()` 主表构建期的既有 owner。本 plan 不新增或修改该函数合同，也不修改 recent load/store 候选队列维护、复用种类选择、复用使能与 size 保持概率、load-after-store/load-after-load/store-after-load/store-after-store 地址关系、fallback 或地址复用算法。normal 自动主表地址从 PADDR 窗口解耦到 MAIN_VADDR 窗口由 `mem_ut_v2_main_table_vaddr_generation_adapt_execution_plan_20260713.md` 唯一负责，不计入本 compile/width 专项函数合同。

既有地址复用完成后，transaction 仍按原调用链完成 entry 校验并写入主表，随后才进入 LSQ admission、enqueue 和 split issue。V2 适配仅在这些后续边界消费本 plan 统一后的 compile 宽度、FuType 编码和对应字段链；不得把版本差异反向注入地址复用候选或算法，也不得因地址复用额外改写 ROB/LQ/SQ key、issue generation、MMIO active-instance provenance、pass/fail 或 terminal。这里的“不得改变 issue generation”是指本 plan 不修改既有 target 派生和调度算法，不否认 transaction 原有 load/store 类型按既有规则决定 LOAD/STA/STD target。

coding 时只复核该既有 flow 能消费适配后的参数和字段，不对 `apply_addr_reuse_window()` 产生行为 diff；因此该函数不进入第 6 章函数合同，也不计入本 plan 的12个新增或修改函数合同。

## 6. 函数/任务级伪代码

### 6.1 `check_compile_param_consistency()`

函数目的：纯检查 `memblock_dispatch_types.sv` 的 compile package localparam 是否自洽。该函数属于初始化低频路径，只诊断/fatal，绝不 clamp、赋值或同步 runtime 字段。

输入：

- `MEMBLOCK_ROB/LQ/SQ_VALUE_W`、`MEMBLOCK_FTQ_PTR_VALUE_W`、`MEMBLOCK_FTQ_OFFSET_W`。
- `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM`、LOAD/STA/STD pipe 数和派生 port base/total。
- DUT/internal FuType width、各 one-hot bit localparam。
- enqueue/issue/SQ-deq capability localparam。

输出/副作用：

- 正常返回表示 compile profile 自洽。
- 不返回结构数，不修改任何字段、queue、map、transaction 或 runtime snapshot。
- 任一 compile 不变量失败时 `uvm_fatal`。

源码级伪代码：

```text
function void check_compile_param_consistency();
    if any key/FTQ/FuType width == 0: fatal;
    if any slot/pipe count == 0: fatal;

    if STA_PORT_BASE != LOAD_PORT_BASE + LOAD_PIPE_NUM: fatal;
    if STD_PORT_BASE != STA_PORT_BASE + STA_PIPE_NUM: fatal;
    if SCALAR_ISSUE_PORT_NUM != STD_PORT_BASE + STD_PIPE_NUM: fatal;
    if SCALAR_ISSUE_MASK_W != SCALAR_ISSUE_PORT_NUM: fatal;
    build LOAD/STA/STD half-open port intervals from base and count;
    if LOAD_PORT_BASE >= SCALAR_ISSUE_PORT_NUM: fatal;
    if any interval overlaps, leaves a gap in [LOAD_PORT_BASE, total),
       exceeds total, or a scalar port belongs to != 1 interval: fatal;

    if any FuType bit >= INTERNAL_FUTYPE_W: fatal;
    if any scalar FuType bit >= DUT_FUTYPE_W: fatal;
    if any two FuType bits are equal: fatal;

endfunction
```

中文文字伪代码：

该函数先检查所有 packed width 和物理数量非 0。随后验证 LOAD/STA/STD base、total 和 mask 的可编码算术关系，以三个半开区间检查互不重叠、无空洞、完整覆盖 `[LOAD_PORT_BASE, SCALAR_ISSUE_PORT_NUM)`，并确认区间内每个 scalar port恰好归属一个 target；最后验证 FuType one-hot bit不重复、都能被内部容器表达，scalar bit还能被 DUT-facing width表达。该纯检查不读取版本标签，也不比较任何版本的精确默认值。V2 精确默认值由 `memblock_compile_params.svh` 默认定义和静态 RTL/profile审计保证；V3通过编译期覆盖提供自身值，并接受同一组通用不变量检查。任一错误直接 fatal；函数没有任何赋值，所以不会把错误配置“修好后继续跑”。consumer直接读取 package localparam，本 plan明确不新增任何 `get_compile_*()` API。

### 6.2 `seq_csr_common::validate_and_clamp()`

函数目的：在 plus 参数完成加载后编排所有runtime合法性处理；slot/pipe资源限制只调用同文件 `apply_runtime_resource_limits()`，不再维护物理结构兼容快照。

输入：

- `load_from_plus()` 已写入的 runtime 参数快照。
- `memblock_dispatch_types.sv` package localparam 物理上限。

输出/副作用：

- 通过资源helper检查 `enq_per_cycle`并clamp三类pipe limit。
- 对pipe limit越界打印 `uvm_warning`；固定每拍enqueue数量越界fatal。
- 当前V2只要AMO或CBO op-class权重非0就`uvm_fatal`，不静默修正测试意图，也不依赖compile capability。
- 不修改 packed type、agent interface、queue、map 或主表。

源码级伪代码：

```text
function void validate_and_clamp();
    check_compile_param_consistency(); // 只检查compile localparam，不修改状态
    apply_runtime_resource_limits(); // 唯一slot/pipe runtime收敛点

    if (op_class_amo_wt != 0) fatal; // V2当前未闭环
    if (op_class_cbo_wt != 0) fatal; // V2当前未闭环
    if any post-clamp value remains out of range: fatal;
endfunction
```

中文文字伪代码：

该函数先调用纯检查确认物理 profile 可用，再调用 `apply_runtime_resource_limits()`：该helper以编译期slot/pipe localparam为唯一上限，固定enqueue数量越界fatal，三类pipe limit使用现有 `clamp_int()` warning+clamp。五个旧 `REAL_*` 参数已经退出plus和runtime快照，不再同步兼容字段。随后分别检查 atomic 和 CBO：当前V2任何非零 `MEMBLOCK_OP_CLASS_AMO_WT` 或 `MEMBLOCK_OP_CLASS_CBO_WT` 都代表显式请求未闭环功能，必须fatal。两个运行期检查互不替代，且不读取compile capability。PREFETCH权重不参与这两个运行期检测，默认1保持不变。

### 6.2.1 `seq_csr_common::apply_runtime_resource_limits()`

函数目的：集中管理所有直接受LSQ enqueue slot和LOAD/STA/STD pipe物理数量约束的runtime行为参数，后续物理数量变化只修改compile宏，runtime上限自动跟随。

输入：`enq_per_cycle`、三类`*_pip_num_limit`运行期快照，以及四个`MEMBLOCK_DUT_*`资源localparam。

输出/副作用：固定enqueue数量非法时fatal；三类pipe limit通过`clamp_int()`修改为合法范围；不读取或修改interface、queue、map、transaction。

文字伪代码：

```text
确认四个编译期物理资源数量均非0，否则fatal；
确认enq_per_cycle位于[1:DUT_LSQ_ENQ_SLOT_NUM]，否则fatal；
把load_pip_num_limit clamp到[1:DUT_LOAD_PIPE_NUM]；
把sta_pip_num_limit clamp到[1:DUT_STA_PIPE_NUM]；
把std_pip_num_limit clamp到[1:DUT_STD_PIPE_NUM]；
返回validate_and_clamp继续处理其它参数。
```

### 6.3 `encode_and_fit_dut_futype()`

函数目的：在内部 36-bit `fuType` 写入 V2 DUT 35-bit 端口前检查当前版本 one-hot 编码集合和宽度，禁止把 V3 编码裁剪后直接送给 V2；AMO/CBO是否支持不由该编码helper判断。

源码落点固定为 `memblock_dispatch_types.sv` 的 package function，由 LSQ enqueue 和 issue field assigner 直接调用；不得复制 class-local 版本。

输入：内部规范宽度 `bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] internal_fuType`。

输出/副作用：返回 `bit [MEMBLOCK_DUT_FUTYPE_W-1:0]`；高于 DUT 宽度的 bit 非 0、vector LS或未知编码均 `uvm_fatal`，不存在失败返回值，不修改公共状态。

源码级伪代码：

```text
function bit [MEMBLOCK_DUT_FUTYPE_W-1:0] encode_and_fit_dut_futype(internal_fuType, caller);
    case (internal_fuType)
      MEMBLOCK_FUTYPE_LDU,
      MEMBLOCK_FUTYPE_STU,
      MEMBLOCK_FUTYPE_MOU: pass encoding check;
      MEMBLOCK_FUTYPE_VLDU,
      MEMBLOCK_FUTYPE_VSTU,
      MEMBLOCK_FUTYPE_VSEGLDU,
      MEMBLOCK_FUTYPE_VSEGSTU: fatal unsupported vector;
      default: fatal unknown encoding or value outside compile-time constant set;
    endcase
    if ((internal_fuType >> MEMBLOCK_DUT_FUTYPE_W) != '0) begin
        fatal("%s fuType has bits above DUT width", caller);
    end
    return internal_fuType[MEMBLOCK_DUT_FUTYPE_W-1:0];
endfunction
```

中文文字伪代码：

该 helper 是版本化 `FuType` 与 DUT 端口之间的唯一转换边界。由于它位于 `lsq_ctrl_model.sv` 之前，不能反向调用 class helper；它直接比较 package one-hot 常量。LDU/STU/MOU只按当前版本编码做fit；vector和未知编码fatal。MOU是否允许进入V2主流程由`validate_main_table_entry()`和下游运行期检测负责。最后才做高位检查并返回低 `MEMBLOCK_DUT_FUTYPE_W` 位。这样 V3 bit 编码即使能落进 35 bit，也会因不匹配当前 package 常量而被拒绝。

### 6.4 `memblock_dispatch_base_sequence::validate_main_table_entry()`

函数目的：在主表 entry 落表前执行V2 scalar未支持功能检测，保证random、boundary、manual和fixed/directed四种来源都不能把AMO/CBO送入未闭环主流程；本函数不读取compile capability。

输入：`main_control_transaction tr` 和 caller 字符串。

输出/副作用：合法时继续现有 entry 校验；非法时 fatal。不写主表、状态表、queue、map 或 pointer。

源码级伪代码：

```text
function void validate_main_table_entry(tr, caller);
    if (tr == null) fatal;
    if (tr.op_class == MEMBLOCK_OP_CLASS_AMO ||
         tr.fuType == MEMBLOCK_FUTYPE_MOU ||
         tr.lsq_flow == MEMBLOCK_LSQ_FLOW_ATOMIC) begin
        fatal("%s requested unsupported scalar atomic", caller);
    end
    if (tr.op_class == MEMBLOCK_OP_CLASS_CBO ||
         tr.lsq_flow == MEMBLOCK_LSQ_FLOW_CBO ||
         lsq_ctrl_model::is_cbo_fuoptype(tr.fuOpType)) begin
        fatal("%s requested unsupported scalar CBO", caller);
    end
    preserve existing vaddr/ROB/vector/derive_op_behavior/numLsElem/template checks;
endfunction
```

中文文字伪代码：

该函数先完成null检查，再在任何`derive_op_behavior()`、主表写入或模板合法分支之前分别检查atomic和CBO语义。atomic检测使用op class、FuType和LSQ flow三个无歧义字段；不能把AMO fuOpType数值单独作为atomic证据，因为`LD/SD`等普通操作与AMO共享部分编码。CBO检测检查op class、CBO LSQ flow和CBO fuOpType，因为CBO与普通store共用STU，但当前CBO编码不与合法scalar store/prefetch模板重叠。任一无歧义维度表达未支持功能都立即fatal。通过两个检测后才继续原有地址、ROB范围、vector、`numLsElem`和op template校验。函数只验证待落表对象，不改变主表或状态，因此random/boundary、manual/fixed的AMO/CBO都会在`main_table_ready`和admission前失败。普通load/store及默认权重为1的software prefetch保持原校验与LDU->LOAD行为。

### 6.5 `seq_pkg::fit_directed_rob_value_or_fatal()`

函数目的：为 `seq_pkg` 中所有 `int unsigned` directed ROB value 提供唯一的检查和显式转换边界，消除固定 slice、参数化 slice 及两个 builder 各自实现范围检查的分叉。

源码落点：`memblock_dispatch_types.sv` 中 ROB/LQ/SQ width localparam 之后、所有 transaction 和 sequence class include 之前。声明为 package-level `automatic function`，不新增 class wrapper。

输入：

- `int unsigned value`：调用者尚未截断的原始 directed ROB value。
- `string context`：调用点诊断上下文，只用于 fatal 文本。
- 只读 package localparam `MEMBLOCK_ROB_VALUE_W`。

输出/副作用/失败/退出：

- 合法时返回 `bit [MEMBLOCK_ROB_VALUE_W-1:0]`，且返回前只执行一次 `MEMBLOCK_ROB_VALUE_W'(value)` 显式 sized cast。
- 不读写 transaction、queue、map、counter、runtime snapshot 或 package/class 状态；除 fatal report 外无状态副作用。
- `MEMBLOCK_ROB_VALUE_W==0`、width 无法安全计算 64-bit 独占上限，或原始 `value` 大于等于独占上限时 `uvm_fatal`；不存在 clamp、fallback、饱和值或失败返回值。
- 合法路径在返回 fitted value 后退出；fatal 路径不返回调用者。

源码级伪代码：

```text
function automatic bit [MEMBLOCK_ROB_VALUE_W-1:0]
fit_directed_rob_value_or_fatal(input int unsigned value,
                                input string context);
    longint unsigned exclusive_limit;

    if (MEMBLOCK_ROB_VALUE_W == 0) fatal(context, "ROB width is zero");
    if (MEMBLOCK_ROB_VALUE_W >= $bits(longint unsigned))
        fatal(context, "ROB width cannot form an exclusive 64-bit limit");

    exclusive_limit = 64'd1 << MEMBLOCK_ROB_VALUE_W;
    if (longint unsigned'(value) >= exclusive_limit)
        fatal(context, value, MEMBLOCK_ROB_VALUE_W, exclusive_limit - 1);

    return MEMBLOCK_ROB_VALUE_W'(value);
endfunction
```

中文文字伪代码：

该 helper 先验证当前 ROB value width 能形成合法返回宽度和 64-bit 独占上限；width 不合法时携带 `context` fatal，不继续位移。随后用 64-bit 的 1 左移 `MEMBLOCK_ROB_VALUE_W` 计算第一个不可表示值，并把仍为完整 `int unsigned` 的输入提升到 `longint unsigned` 后比较；原值超范围时打印调用点、原值、width 和最大合法值并 fatal。只有检查通过后才执行 sized cast并返回。函数没有任何共享状态，因此两个 builder 的调用顺序不会互相影响，返回值只供当前 transaction 的 `robIdx_value` 赋值。

### 6.6 `memblock_main_dispatch_manual_main_table_sequence::make_directed_transaction()`

函数目的：让 real mixed directed builder 复用唯一 ROB fit helper，消除当前固定 `[8:0]` 静默截断，同时保持既有 load/store transaction 模板。

输入：`tr_name`、`op_class`、未截断的 `int unsigned rob_value` 和 `base_addr`。

输出/副作用/失败/退出：返回一笔 load 或 store `main_control_transaction`。合法路径只创建并填充新对象，不修改主表、status、queue、map 或 cursor。共享 fit helper 拒绝非法 width/value；对象创建失败或 op class 不是 load/store 时继续按既有规则 fatal；成功执行 `update_vaddr()` 后返回。

源码级伪代码：

```text
function main_control_transaction make_directed_transaction(...);
    fitted_rob_value = fit_directed_rob_value_or_fatal(
        rob_value,
        context containing class/function/tr_name);

    tr = create transaction;
    if (tr == null) fatal;
    tr.robIdx_value = fitted_rob_value;
    fill common directed fields;
    case (op_class)
      INT_LOAD: fill LDU/LOAD/LD template;
      STORE:    fill STU/STORE/SD template;
      default:  fatal unsupported real mixed op_class;
    endcase
    tr.update_vaddr(); // 根据src_0和imm刷新该对象的vaddr，不修改公共表
    return tr;
endfunction
```

中文文字伪代码：

函数把调用者原始 `rob_value` 和可定位到 transaction 名称的 context 交给共享 helper；helper 在不读取或修改 transaction 的前提下检查 width/上限并返回 fitted value，失败则本函数不再继续。成功后创建对象并检查 handle，把 fitted value写入 `tr.robIdx_value`，再按原顺序填公共字段和 load/store 模板。default 分支仍拒绝其它 op class。最后 `update_vaddr()` 只更新新对象的派生地址并返回对象。函数内不得再出现任何 `rob_value[...]` slice 或第二套上限计算。

### 6.7 `soft_test_memblock_dispatch_smoke_sequence::new()`

函数目的：把 V2 software dispatch smoke 的默认 transaction 数从 3 收敛为与 load/store 两笔主表一致的 2。

输入：可选对象名 `name`。输出/副作用：调用 `super.new(name)` 完成基类初始化后设置当前 sequence 实例的 `dispatch_smoke_trans_num=2`；不创建 transaction，不修改公共表或 LSQ 状态。该函数无 fallback/fatal 分支，赋值完成后正常返回构造后的对象。

源码级伪代码：

```text
function new(name);
    super.new(name); // 完成memblock_dispatch_base_sequence/UVM对象初始化
    dispatch_smoke_trans_num = 2;
endfunction
```

中文文字伪代码：

构造函数先保留既有基类初始化，再把本场景唯一 transaction 数期望设置为 2。后续 commit 数检查和 final `main_trans_num` 检查继续读取该字段，因此不再保留值 3或新增第二个数量字段。

### 6.8 `soft_test_memblock_dispatch_smoke_sequence::build_directed_main_table()`

函数目的：把 software smoke 默认主表从 load/store/AMO 三笔改成 load/store 两笔，使默认场景与 V2 当前没有scalar atomic闭环的范围一致。

输入：本 sequence 固定的两个 transaction 名称、ROB value 0/1和 base address。输出：导入后的 `data.main_trans_num=2`，uid 0为 load、uid 1为 store。副作用：清空并重建 manual main table，再由 `import_manual_main_table()` 写入公共 main table。transaction 创建、共享 ROB fit、entry validation 或导入失败时 fatal；两笔导入完成后退出，不生成 uid 2、AMO、MOU 或占位第三笔。

源码级伪代码：

```text
task build_directed_main_table();
    clear_manual_main_table(); // 清除本sequence上一次手工表内容
    set_manual_main_transaction(0,
        make_directed_transaction(load, INT_LOAD, 0, load_addr));
    set_manual_main_transaction(1,
        make_directed_transaction(store, STORE, 1, store_addr));
    import_manual_main_table(); // 校验并导入恰好两笔，建立uid 0..1
endtask
```

中文文字伪代码：

task 先清空旧 manual table，避免前次运行残留 uid。随后分别调用本 class 的 directed builder 构造 load 和 store；builder 内部调用共享 fit helper检查 ROB value并填 transaction。每笔对象由 `set_manual_main_transaction()` 写入索引 0或1。最后 `import_manual_main_table()` 按公共入口校验并导入两笔，令后续循环只看到 uid 0..1。源码中删除第三次 setter 调用和 `dispatch_smoke_amo`，不允许用 MOU、CBO 或重复 load/store 补第三笔。

### 6.9 `soft_test_memblock_dispatch_smoke_sequence::make_directed_transaction()`

函数目的：让 software-only builder 与 real manual builder 共用唯一 ROB fit，并删除本轮 V2未闭环范围内不应存在的 AMO/MOU 构造 case。

输入：`tr_name`、`op_class`、未截断的 `int unsigned rob_value` 和 `base_addr`。输出：一笔 load 或 store `main_control_transaction`。副作用仅限创建和填充新对象，不修改 main/status/queue/map/LSQ 状态。共享 helper 拒绝非法 width/value；对象创建失败，或 `op_class` 为 AMO/CBO/其它非 load/store 值时 default 分支 fatal；成功更新对象 `vaddr` 后返回。

源码级伪代码：

```text
function main_control_transaction make_directed_transaction(...);
    fitted_rob_value = fit_directed_rob_value_or_fatal(
        rob_value,
        context containing class/function/tr_name);

    tr = create transaction;
    if (tr == null) fatal;
    tr.robIdx_value = fitted_rob_value;
    fill common software directed fields;
    case (op_class)
      INT_LOAD: fill LDU/LOAD/LD template with numLsElem=1;
      STORE:    fill STU/STORE/SD template with numLsElem=1;
      default:  fatal unsupported smoke op_class, including AMO;
    endcase
    tr.update_vaddr(); // 只刷新当前新对象的派生地址
    return tr;
endfunction
```

中文文字伪代码：

函数先把未截断的 ROB value交给共享 helper；非法 width/value 时 helper fatal，本函数不创建可继续使用的 transaction。合法时创建对象并写入 fitted value，再填充公共字段。case 只保留 load 和 store：load 使用 LDU/LOAD/LD，store 使用 STU/STORE/SD，两者 `numLsElem=1`。删除 AMO case 后，显式传入 `MEMBLOCK_OP_CLASS_AMO` 会进入 default 并 fatal；源码中不再引用 `MEMBLOCK_FUTYPE_MOU`、`MEMBLOCK_LSQ_FLOW_ATOMIC` 或 AMO fuOpType。最后只更新当前对象地址并返回，不写公共运行期状态。

### 6.10 software smoke 后续循环、期望和退出复核

以下函数体保持现有参数化实现，不计入新增/修改函数合同，但 coding 和 review 必须逐项确认它们消费两笔主表后语义一致：

```text
body()：
  保持build -> admission/route -> fire -> writeback -> commit/deq -> final check顺序；
  只有所有步骤返回后打印completed并退出。

admit_lsq_and_route_issue()与all_required_targets_dispatched()：
  继续遍历data.main_trans_num，因此只处理uid 0..1；
  load需要LOAD target，store需要STA和STD target，不存在atomic target。

commit_and_deq_lsq()：
  build_lsqcommit_xaction负责收集当前可提交uid且不修改DUT接口；
  commit_uids.size必须等于dispatch_smoke_trans_num，即2，否则fatal；
  mark_rob_commit_batch更新两笔ROB commit状态；
  apply_dut_lq_deq(1, ...)只释放load的一笔LQ；
  apply_dut_sq_deq(1, ...)只释放store的一笔SQ。

check_final_status()：
  data.main_trans_num必须等于2；
  循环只检查uid 0..1的enq/issue/writeback/pass/commit/deq/success/terminal状态；
  load的LOAD以及store的STA/STD target状态必须全部完成；
  active ROB/LQ/SQ map必须为空，LQ/SQ free count必须恢复到满值；
  任一检查失败fatal，全部通过后返回body正常退出。
```

`soft_test_memblock_dispatch_fault_smoke_sequence` 和 `soft_test_memblock_dispatch_replay_smoke_sequence` 继承同一 `build_directed_main_table()`。二者继续使用 uid 0 load与uid 1 store，不依赖已删除的 uid 2 AMO；本 plan 不修改其函数体，但必须运行对应 smoke，确认继承后的两笔数量、commit/deq 和 final/terminal 检查闭环。

## 7. 验收标准

1. `memblock_compile_params.svh` 中每个新增宏都有默认值和中文注释。
2. `memblock_dispatch_types.sv` 中 ROB/LQ/SQ 宽度从现有 `MEMBLOCK_DUT_*` 宏派生，内部 `fuType` 保留 36-bit 容器；DUT 宽度和 LDU/STU/MOU/vector one-hot bit 均由当前 profile 宏派生。
3. `seq_csr_common::validate_and_clamp()` 明确区分编译期结构上限和 runtime 行为限制。
4. `check_compile_param_consistency()` 中不存在对 runtime 字段的赋值、`ref` 参数、clamp 调用、版本标签分支或 V2/V3 精确值比较；它只检查 width/count 非零、bit 范围与唯一性、base/total/连续区间/port 唯一归属、mask width等可编码不变量。V2 精确默认值由 `.svh` 默认和静态 RTL/profile审计保证，V3由编译期覆盖适配；`validate_and_clamp()` 是唯一 runtime 修改点。
5. 五个 `MEMBLOCK_REAL_*` 物理结构参数不再出现在plus定义、加载、default cfg、runtime快照、getter或consumer中，不新增旧名称扫描或兼容拒绝helper。`MEMBLOCK_ENQ_PER_CYCLE`和三类pipe limit分别按编译期slot/pipe上限执行fatal或clamp。
6. 所有 `seq_pkg` consumer 直接使用 `memblock_dispatch_types.sv` package localparam；`rg -n "get_compile_"` 在本适配范围内无新增结果。
7. 所有写入 V2 DUT `fuType` 端口的位置调用固定落在 `memblock_dispatch_types.sv` 的 `encode_and_fit_dut_futype()`；V2 LDU/STU/MOU 必须为 bit15/16/17，不允许隐式截断。
8. 执行前用 `rg` 确认当前 LSQ enqueue slot 仍为 0..5 共 6 个；如不是，本 plan 验收失败并报告基线不一致，不在本次执行中修改接口 profile 或其它 plan。
9. `rg -n "pipe_idx < 3|pipe_idx < 2|slot < 6" mem_ut/ver/ut/memblock/seq` 不再出现需要版本化的新硬编码；历史无法迁移项必须在 review 中说明。
10. 不新增 RM、coverage、checker 逻辑。
11. `MEMBLOCK_DUT_LOAD_PORT_BASE` 只在 `memblock_compile_params.svh` 默认定义为 0，并在 `memblock_dispatch_types.sv` 暴露同名 localparam；scalar issue total port、LOAD/STA/STD 区间、fired-mask width/full-mask均由 base/count派生，三个区间互不重叠、无空洞、完整覆盖 `[LOAD_PORT_BASE, total)` 且区间内每个 port唯一归属，无固定 `+3/+5/[6:0]/7'h7f` 第二权威。
12. `MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP=0` 和 `MEMBLOCK_DUT_HAS_SQ_DEQ_PTR=0` 可被后续 LSQ plan 编译期分支消费；V2 不读取不存在字段。
13. `memblock_sync_pkg` 的 raw ROB/LQ/SQ 和 deq pointer value 全部消费宏，不再硬编码 8/7/6。
14. IQ feedback、int-WB、lintsissue/vecissue、ctrl、redirect、LSQ enqueue/commit、vector-WB agent 的 interface/xaction/monitor/XZ/connect 宽度均完成第 5.2 节审计。
15. ctrl agent 的 `memoryViolation.ftqOffset` 为 4 bit，monitor X/Z width 为 `MEMBLOCK_DUT_FTQ_OFFSET_W`；MMIO ROB value 和 load port 数消费宏。
16. 不新增scalar atomic compile capability；`plus.sv`、`seq_csr_common`和`default.cfg`的`MEMBLOCK_OP_CLASS_AMO_WT`默认均为0，V2运行期对显式非0及random/boundary/manual/fixed AMO/MOU检测并fatal。
17. 不新增scalar CBO compile capability；`MEMBLOCK_OP_CLASS_CBO_WT`默认继续为0，显式非0在`validate_and_clamp()` fatal，random/boundary/manual/fixed CBO在主表落表及admission前由运行期语义检测fatal。
18. `MEMBLOCK_OP_CLASS_PREFETCH_WT` 默认仍为 1；LDU + prefetch fuOpType 仍按 PREFETCH behavior 只 route LOAD，不被atomic/CBO运行期检测拒绝。
19. `seq_pkg::fit_directed_rob_value_or_fatal()` 在 `memblock_dispatch_types.sv` 中只有一个 package-level `automatic function` 实现；它先检查 `MEMBLOCK_ROB_VALUE_W` 和原始 `int unsigned value` 的可表示上限，再做 `MEMBLOCK_ROB_VALUE_W'(value)` sized cast，且无状态副作用或失败返回值。
20. real manual 与 software smoke 两个 `make_directed_transaction()` 都调用该 helper；两个函数中均不存在 `rob_value[8:0]`、`rob_value[MEMBLOCK_ROB_VALUE_W-1:0]`、其它参数化低位 slice、第二套上限检查或隐式静默截断。
21. `soft_test_memblock_dispatch_smoke_sequence::new()` 设置 `dispatch_smoke_trans_num=2`，`build_directed_main_table()` 只设置 uid 0 load和uid 1 store；文件中不存在 `dispatch_smoke_amo`、AMO case、`MEMBLOCK_FUTYPE_MOU`、`MEMBLOCK_LSQ_FLOW_ATOMIC` 或 AMO fuOpType。
22. software smoke 的 admission/required-target/final 循环只随 `data.main_trans_num=2` 处理 uid 0..1；预期 issue target为一条 LOAD、一条 STA和一条 STD，ROB commit 数为 2，LQ/SQ deq count仍各为 1，final map/free-count/terminal检查全部通过。
23. `tc_dispatch_smoke`、`tc_dispatch_fault_smoke` 和 `tc_dispatch_replay_smoke` 均在继承两笔 load/store 主表后通过，日志包含各自 completed 和 `TEST CASE PASSED`，且 `UVM_ERROR`、`UVM_FATAL` 均为 0。
24. compile/width 专项是共享 fit helper、两个 directed builder 转换和 software smoke 去 AMO 的唯一 coding owner；其它专项没有重复实现、重复修改或保留旧行为。
25. 函数合同机械统计为4个新增、8个修改，共12个；`MEMBLOCK_DUT_LOAD_PORT_BASE`参数合同另计1个默认宏owner和1个同名package localparam载体，split plan新增0个同义owner。

## 8. 验证命令或静态检查

```bash
git diff --check -- mem_ut/ver/ut/memblock/cfg mem_ut/ver/ut/memblock/env mem_ut/ver/ut/memblock/seq AI_DOC
! rg -n "MEMBLOCK_REAL_(LSQ_ENQ_MAX|ENQ_WIDTH|LOAD_PIPE_NUM|STA_PIPE_NUM|STD_PIPE_NUM)|reject_removed_hardware_structure_plusargs" \
  mem_ut/ver/ut/memblock/env/plus.sv \
  mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv \
  mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg
rg -n "MEMBLOCK_ROB_VALUE_W|MEMBLOCK_LQ_VALUE_W|MEMBLOCK_SQ_VALUE_W|MEMBLOCK_DUT_FTQ_(PTR_VALUE|OFFSET)_W|MEMBLOCK_INTERNAL_FUTYPE_W|MEMBLOCK_DUT_FUTYPE_W|MEMBLOCK_DUT_FUTYPE_.*_BIT|MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM|MEMBLOCK_DUT_(LOAD|STA|STD)_PORT_BASE|MEMBLOCK_DUT_LOAD_PIPE_NUM|encode_and_fit_dut_futype|fit_directed_rob_value_or_fatal" mem_ut/ver/ut/memblock
test "$(rg -n '^`define MEMBLOCK_DUT_LOAD_PORT_BASE 0$' mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh | wc -l)" -eq 1
test "$(rg -n 'localparam[[:space:]]+int[[:space:]]+unsigned[[:space:]]+MEMBLOCK_DUT_LOAD_PORT_BASE' mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv | wc -l)" -eq 1
sed -n '/localparam[[:space:]]\+int[[:space:]]\+unsigned[[:space:]]\+MEMBLOCK_DUT_LOAD_PORT_BASE/,/;/p' mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv | rg -n '`MEMBLOCK_DUT_LOAD_PORT_BASE'
sed -n '/function void check_compile_param_consistency/,/endfunction:check_compile_param_consistency/p' mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv | rg -n 'PORT_BASE|PORT_NUM|MASK_W|FUTYPE|CAPABILITY|uvm_fatal'
! sed -n '/function void check_compile_param_consistency/,/endfunction:check_compile_param_consistency/p' mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv | rg -n 'MEMBLOCK_VERSION|V2_PROFILE|V3_PROFILE|LOAD_PORT_BASE[[:space:]]*(==|!=)[[:space:]]*0|clamp|[[:space:]]ref[[:space:]]'
rg -n "get_compile_" mem_ut/ver/ut/memblock/seq
rg -n "memoryViolation_bits_ftqOffset|TCNT_CHECK_SIG_XZ.*memoryViolation.*ftqOffset" mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent
rg -n "bit \[6:0\].*(lq_value|lqIdx_value)|bit \[5:0\].*(sq_value|sqIdx_value)|logic \[7:0\].*robIdx_value|TCNT_CHECK_SIG_XZ.*robIdx_value.*,8" mem_ut/ver/ut/memblock/common mem_ut/ver/ut/memblock/agent
rg -n "MEMBLOCK_OP_CLASS_(AMO|CBO|PREFETCH)_WT" mem_ut/ver/ut/memblock/env/plus.sv mem_ut/ver/ut/memblock/seq/base_seq_help mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg
! rg -n "MEMBLOCK_DUT_SCALAR_(ATOMIC|CBO)_FLOW_SUPPORTED" mem_ut/ver/ut/memblock
rg -n "fit_directed_rob_value_or_fatal" \
  mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv \
  mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_manual_main_table_sequence.sv \
  mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv
! rg -n "rob_value\[" \
  mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_manual_main_table_sequence.sv \
  mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv
rg -n "dispatch_smoke_trans_num = 2|uid < data.main_trans_num|commit_uids.size\(\) != dispatch_smoke_trans_num|apply_dut_(lq|sq)_deq\(1" \
  mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv
test "$(rg -c "set_manual_main_transaction\(" mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv)" -eq 2
! rg -n "dispatch_smoke_amo|MEMBLOCK_OP_CLASS_AMO|MEMBLOCK_FUTYPE_MOU|MEMBLOCK_LSQ_FLOW_ATOMIC|MEMBLOCK_LSUOP_AMO" \
  mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv
rg -n "\[7:0\].*loadMmioUop|memblock_dispatch_fired_mask\[6:0\]|7'h7f|uop_index \+ 3|uop_index \+ 5" mem_ut/ver/ut/memblock
rg -n "enqLsq_req_[0-9]+|enqLsq_needAlloc_[0-9]+" build_memblock/rtl/MemBlock.sv mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src
rg -n "pipe_idx < 3|pipe_idx < 2|slot < 6" mem_ut/ver/ut/memblock/seq
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
make eda_run tc=tc_dispatch_smoke mode=base_fun
make eda_run tc=tc_dispatch_fault_smoke mode=base_fun
make eda_run tc=tc_dispatch_replay_smoke mode=base_fun
```

完成远端编译后依次执行 sanity、software dispatch、fault 和 replay smoke。`tc_dispatch_smoke` 必须显示两笔 ROB commit、LQ/SQ 各 deq 一笔并正常完成；fault/replay smoke 必须证明继承两笔 load/store 主表后仍以 uid 0 load和uid 1 store闭环。四个 testcase 都必须看到 `TEST CASE PASSED`，且 `UVM_ERROR`、`UVM_FATAL` 均为 0。

## 9. 风险与非目标

风险：

- 如果当前 agent interface/xaction 已经由脚本生成固定宽度字段，typedef 宽度改动可能暴露出接口字段仍硬编码的问题；这属于本 plan 应暴露的编译风险，不应通过截断掩盖。
- V3 不做运行期自动推断；现有 V3 profile继续通过编译期入口显式覆盖完整 width/bit/count/port-style宏集合。V3精确覆盖是否齐全由静态 profile审计保证，`check_compile_param_consistency()` 只负责在最终常量违反通用可编码不变量时 fatal，不能被写成识别版本或证明某组精确默认值的运行条件。实现不得改写 V3已验证值，本 plan没有执行者选择项。

非目标：

- 不修改 LSQ enqueue 字段填充策略。
- 不修改 issue scheduler 主调度算法。
- 不修改 writeback event 归一化。
- 不处理 L2TLB response 字段链路。

## 11. 原执行范围差异分析

### 原测试框架逻辑对比和修改类型总结

原逻辑由散落硬编码、部分既有宏和 runtime plus共同表达结构，既没有 LOAD base单一 owner，也没有集中检查 base/total/port唯一性；版本精确值若写进通用运行条件还会阻断编译期覆盖。修改原因是 V2/V3 的 width、FuType bit和port布局属于 compile-time事实，必须在进入主体 flow前同源且可审计。修改后由 `.svh` 默认/override和 package localparam形成唯一参数链，通用 check只验证可编码不变量；V2精确默认值由默认定义与RTL/profile静态审计保证，V3由编译期覆盖适配。主体 issue仲裁、状态表、handler和合法 scalar调度算法不因参数所有权调整而改变。

| 修改项 | 类型 | 是否改变功能行为 |
|---|---|---|
| ROB/LQ/SQ、FTQ ptr/offset、FuType、slot/pipe/port 数宏化 | 字段/位宽/编译参数适配 | 不改变合法 scalar flow 的调度语义 |
| `memblock_sync_pkg` raw 和各 agent/interface/xaction/monitor/XZ/connect 同源 | 字段链路适配 | 不改变 event 内容，只消除截断/扩展 |
| `memoryViolation.ftqOffset` 从当前错误 5 bit 改为 V2 4 bit | 字段宽度修复 | 不改变 redirect 判定，只修正采样宽度 |
| 删除五个`MEMBLOCK_REAL_*` plus参数，物理slot/pipe只读compile localparam | 参数所有权修复 | 默认6/3/2/2行为不变；不保留旧参数兼容检测，旧名称不再属于测试框架配置接口 |
| runtime资源使用量由`apply_runtime_resource_limits()`集中收敛 | 新增集中参数处理功能 | 固定enqueue数量越界fatal，pipe limit越界warning+clamp；后续物理参数变化只改compile宏 |
| 删除 `get_compile_*` 方案，consumer 直接读 package localparam | 实现结构修改 | 不改变值，只删除第二读取层 |
| V2 FuType one-hot 改为 bit15/16/17、bit31..34 | 修改已有功能语义 | 改变送入 DUT 的编码，修复错误激励 |
| AMO默认权重改为0且V2运行期拒绝AMO/MOU | 参数默认修改+运行期检测 | 默认不再随机生成未闭环atomic，显式构造在admission前fatal |
| 非零 AMO 权重、manual/fixed MOU/AMO fatal | 新增功能边界 | 未支持激励从可能进入主流程改为 fail-fast |
| CBO默认权重保持0且V2运行期独立检测CBO语义 | 运行期检测边界/保留参数默认 | 默认行为不变，CBO不再先enqueue后等issue失败 |
| 非零 CBO 权重、random/boundary/manual/fixed CBO admission 前 fatal | 新增功能边界 | CBO 不再先 enqueue 后等 issue fatal；PREFETCH 不受影响 |
| 新增 `seq_pkg::fit_directed_rob_value_or_fatal()`，两个 directed builder 共同调用 | 共享 fit helper 新增 + 修改已有构造功能 | 超范围输入从两处静默截断改为唯一 package 边界 fatal，合法值显式转换后语义不变 |
| software smoke 从固定 load/store/AMO 三笔改为 load/store 两笔 | scenario 默认功能修改 | 默认场景不再构造未闭环的AMO/MOU；ROB commit 期望从 3改2，LQ/SQ deq仍各1 |
| compile/width 专项统一拥有 helper、两个 builder 和 smoke 去 AMO | coding owner 收敛 | 其它专项只消费结果，不重复实现或保留冲突行为 |
| compile 常量通用纯检查 | 新增初始化检查功能 | 只验证可编码不变量，不识别版本或比较版本精确默认值，不进入每拍路径 |
| `MEMBLOCK_DUT_LOAD_PORT_BASE` 唯一 owner 和区间完整性检查 | 编译参数适配 + 初始化纯检查 | 默认布局不变；消除 split plan 的第二 owner，并让重叠、空洞或重复归属 fail-fast |

LOAD base 原逻辑：split issue草案直接写固定起点，compile参数集没有把宏默认、package暴露和consumer检查完整收敛为一个owner。变更原因：固定值分散会让base/count/total、mask索引和driver解释产生第二权威，也无法由版本compile入口统一覆盖。变更后：`.svh`唯一默认宏为0，`memblock_dispatch_types.sv`唯一同名localparam直接引用该宏，STA/STD base、total和mask上界均由base/count派生，通用check验证连续区间和唯一归属；split plan只消费。主体影响：当前V2物理布局和合法fire语义不变，只把错误配置提前fail-fast。跨plan依赖：compile/width plan拥有定义与检查，split issue plan拥有consumer映射，不得互换或复制owner。

除 software smoke 的默认 transaction 集合和数量外，主体 main-table uid 顺序、LSQ admission cursor、issue queue 仲裁、active map、writeback/redirect/replay handler、commit/deq、pass/fail 和 terminal 算法均不改变。两笔 smoke 继续复用原算法，只删除本轮V2尚未闭环的AMO输入。

### 11.2 字段/位宽适配的修改前后逻辑

修改目的：让 packed type、raw queue、agent transaction 和 DUT connect 对同一 ROB/LQ/SQ/FTQ 字段使用同一版本宽度，避免只修一层后在下一层再次截断。

修改前文字伪代码：

```text
主表key部分使用MEMBLOCK_DUT宏；
raw int-WB/IQ feedback仍直接写7-bit LQ和6-bit SQ；
多个agent仍直接声明8/7/6或在XZ检查中写常数；
ctrl memoryViolation.ftqOffset声明5 bit并按5 bit检查；
sequence中的FTQ helper先生成5 bit，再在LDA写端口时截成4 bit。
```

修改后文字伪代码：

```text
memblock_compile_params定义跨package宏；
memblock_dispatch_types从宏派生seq_pkg唯一localparam；
seq_pkg内所有key、临时变量、slice和port布局直接读localparam；
memblock_sync_pkg及agent package直接读同一宏；
monitor的XZ检查宽度也读宏；
connect只做同宽直连，不允许显式slice、补零或隐式截断；
V2 memoryViolation.ftqOffset从interface到monitor固定为4 bit。
```

输入/输出/副作用：输入是 V2 profile 和 `MemBlock.sv` 端口事实；输出是统一的编译类型。该修改不写运行期状态，不改变 raw queue push/pop、uid 反查或 redirect handler。

失败/退出：宏、packed type、agent 字段或 connect 仍不一致时必须在静态检查、编译或 `check_compile_param_consistency()` 阶段失败，不允许用 slice/补零继续；全部同源且 compile check通过后进入既有初始化流程。

### 11.3 新增 `check_compile_param_consistency()`

添加原因：原框架没有集中验证 FuType bit、port base/total等compile不变量；但按命名规则，`check_*` 不能承担 clamp。

输入：`memblock_dispatch_types.sv` 的 package localparam。

输出/副作用：正常返回或 `uvm_fatal`；无返回值、无字段赋值、无 queue/map/transaction 副作用。

失败/退出：任一 compile 不变量失败时 fatal且不返回；全部检查通过时 void return到 `validate_and_clamp()`，由后者继续处理 runtime 字段。本函数没有 warning、clamp 或 fallback。

修改前文字伪代码：

```text
不存在统一纯检查；
各flow在运行时依赖固定3/2/2/6或等到driver赋值才暴露profile错误。
```

修改后文字伪代码：

```text
检查所有width、slot和pipe数非0；
检查LOAD/STA/STD base、total port和mask width算术一致；
检查三个端口区间互不重叠、无空洞、完整覆盖且每个port唯一归属；
检查FuType bit不重复且落在对应容器宽度内；
任何失败fatal；
绝不调用clamp_int，也不写任何runtime字段。
```

差异影响：新增一次性 fail-fast，不增加运行期扫描。原逻辑没有集中检查，修订前草案还试图把版本精确默认值作为自然语言运行条件，既无法由通用函数可靠编码，也会阻断编译期覆盖。改后只检查 width/count非零、bit范围与唯一性、base/total/区间覆盖与port唯一归属。V2精确默认值由宏默认和静态 RTL/profile审计保证，V3由编译期覆盖适配；主体 runtime参数收敛逻辑不变。原草案中“由该 check 做 clamp”的方案也被删除。

### 11.4 修改 `seq_csr_common::validate_and_clamp()`

修改原因：旧函数用固定6和可由plus修改的`real_*_pipe_num`做上限，且AMO默认权重仍为1；物理结构和runtime行为上限没有彻底分离，资源限制也散落在大函数中。

输入：`load_from_plus()` 后的 runtime 字段以及 package localparam。

输出/副作用：调用集中资源helper完成enqueue/pipe参数收敛；非法atomic/CBO请求fatal。不维护历史物理结构快照，不修改编译类型、主表、状态表、queue或map。

失败/退出：固定enqueue数量越界fatal；pipe limit越界按既有`clamp_int()` warning+clamp；当前V2显式请求AMO/CBO时由运行期检测fatal且不进入后续生成；合法配置处理完成后void return。

修改前文字伪代码：

```text
real_enq_width和real_lsq_enq_max按固定6 clamp；
要求两个字段完全相等，否则fatal；
pipe limit按可被plus修改的real_*_pipe_num clamp；
AMO权重1被视为合法默认；
compile profile没有独立纯检查。
```

修改后文字伪代码：

```text
先调用纯check_compile_param_consistency确认compile常量满足通用不变量；
调用apply_runtime_resource_limits集中按package slot/pipe localparam处理资源参数；
AMO op-class权重非0时按V2未闭环策略fatal；
CBO op-class权重非0时按独立运行期检测fatal；
PREFETCH默认权重1和LDU到LOAD路由不参与上述gate；
继续处理地址、权重、timeout等其它runtime参数。
```

差异影响：参数所有权和失败策略改变；合法load/store默认配置及后续调度算法不变。

### 11.4.1 新增 `seq_csr_common::apply_runtime_resource_limits()`

添加原因：资源参数上限原本散落在`validate_and_clamp()`，且引用可由plus修改的第二套物理数量。集中helper便于后续只修改compile宏即可统一更新runtime边界。

输入：四个compile资源localparam、`enq_per_cycle`和三类pipe limit快照。

输出/副作用：固定enqueue数量非法fatal；三类pipe limit warning+clamp；不修改其它参数或运行期状态。

修改前文字伪代码：不存在独立资源收敛函数，固定6、`real_enq_width`和`real_*_pipe_num`散落在`validate_and_clamp()`。

修改后文字伪代码：先确认compile slot/pipe数量非0；检查`enq_per_cycle`位于物理slot范围；分别按三类compile pipe数clamp runtime limit；返回总入口。

差异影响：新增初始化期O(1)参数处理，不进入每拍路径，不改变合法默认调度行为。

### 11.4.2 修改 `plus::reload_from_cmdline()`

修改原因：五个`MEMBLOCK_REAL_*`是DUT物理slot/pipe的runtime镜像，会与compile profile形成第二权威。

输入：进程命令行和cfg展开后的当前合法runtime plusarg。

输出/副作用：继续加载保留的行为参数；不再声明、加载或保存五个旧物理结构参数，不新增旧名称扫描、兼容warning或fatal。

修改前文字伪代码：声明并加载五个`MEMBLOCK_REAL_*`数值，consumer可在runtime读取第二套物理slot/pipe数量。

修改后文字伪代码：从plus字段定义和reload加载表删除五个旧名称；保留参数继续按原顺序加载；物理consumer直接读取compile宏/localparam。

差异影响：合法默认配置不变；五个旧名称退出测试框架配置接口，后续不承担兼容检测职责。

### 11.4.3 修改 `seq_csr_common::get_enq_per_cycle()`

修改原因：随机模式原来读取已删除的`real_enq_width`。

输入：`enq_per_cycle_rand_en`和编译期`MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM`。

输出/副作用：随机模式返回`[1:物理slot数]`随机值，固定模式返回已校验`enq_per_cycle`；不修改状态。

修改前文字伪代码：随机模式以runtime`real_enq_width`为上限。

修改后文字伪代码：随机模式直接以compile物理slot数为上限，保持V2默认`[1:6]`分布。

差异影响：默认随机分布不变；runtime不能再改变物理随机上界。

### 11.5 新增 `encode_and_fit_dut_futype()`

添加原因：V2 与 V3 的 FuType one-hot 位置不同，仅裁剪 36->35 bit 无法识别语义错位。

输入：36-bit internal FuType 和 caller 字符串。

输出/副作用：返回 `MEMBLOCK_DUT_FUTYPE_W` 位 DUT 编码；未知、vector或超宽值fatal；MOU只做编码fit，是否允许进入主流程由运行期语义检测负责；不修改公共状态。

失败/退出：未知编码、值不属于compile-time常量集合、vector或DUT width以上存在非零bit时fatal且无返回值；合法LDU/STU/MOU完成编码检查后返回DUT-facing value，功能支持状态不在本helper判断。

修改前文字伪代码：

```text
LDU/STU/MOU使用V3 bit16/17/18常量；
STA/STD和enqueue可直接赋给V2 35-bit字段；
只要bit35为0，错误编码仍可能通过。
```

修改后文字伪代码：

```text
调用者传入编译期 package 常量；
helper直接比较LDU/STU/MOU/vector常量，不反向依赖lsq_ctrl_model；
LDU/STU/MOU继续编码fit；
vector和未知编码fatal；
检查DUT宽度以上位全0；
返回低DUT_FUTYPE_W位。
```

差异影响：DUT input FuType 功能语义被修正；helper 固定落在 `memblock_dispatch_types.sv`，不存在第二实现或 getter。

### 11.6 修改 `memblock_dispatch_base_sequence::validate_main_table_entry()`

修改原因：仅把随机 AMO/CBO 权重设 0不能阻止 manual/fixed/directed transaction 直接构造 MOU或 CBO；当前 CBO 模板本身还是合法的，若没有前置运行期检测会先进入admission。

输入：待落表 `main_control_transaction` 和 caller。

输出/副作用：合法时继续原校验，atomic/CBO 不支持时 fatal；不落表、不改状态、不推进 pointer。

失败/退出：null entry、unsupported atomic/CBO或后续既有 entry不变量失败时 fatal，且 `main_table_ready`、admission和 pointer均不推进；全部校验通过时 void return给 import/build调用者继续落表。

修改前文字伪代码：

```text
检查地址、ROB、vector和derive_op_behavior；
AMO模板只要fuType=MOU、lsq_flow=ATOMIC且fuOpType合法就通过；
manual table因此可以把atomic带入后续flow。
```

修改后文字伪代码：

```text
先做null检查；
op_class/fuType/lsq_flow任一无歧义表达atomic时按V2未闭环策略立即fatal；
AMO fuOpType只在上述语义域内校验，不能脱离语义域误判编码重叠的普通load/store；
tr.op_class/tr.lsq_flow/CBO fuOpType任一表达CBO时按V2未闭环策略立即fatal；
CBO与普通store共用STU，不以tr.fuType==STU单独判定CBO；
两个前置检测通过后才调用derive_op_behavior，再继续原地址、ROB、vector、numLsElem和模板检查；
random、boundary、manual、fixed入口共用该检测，并在main_table_ready/admission前结束；
LDU+software prefetch继续合法且只route LOAD。
```

差异影响：新增未支持功能边界；普通 scalar load/store/prefetch 的 entry 校验和主表算法不变。

### 11.7 新增 `seq_pkg::fit_directed_rob_value_or_fatal()`

修改类型：共享 fit helper 新增。本 helper 及其两个调用点只由 compile/width 专项 coding，其它专项不得新增同义 helper 或 class-local wrapper。

添加原因：real manual 当前固定读取 `rob_value[8:0]`，software smoke 当前读取 `rob_value[MEMBLOCK_ROB_VALUE_W-1:0]`。两者都先截断再赋值，无法发现调用者原值超范围；若分别在两个 class 中补检查，又会形成两个 width conversion owner。

修改前逻辑行为：

```text
不存在共享fit helper；
real manual builder创建transaction后取rob_value固定低9 bit；
software smoke builder创建transaction后取rob_value参数化低MEMBLOCK_ROB_VALUE_W bit；
两个builder都不比较原始int unsigned输入和当前width的可表示上限；
超范围输入被静默改值，函数仍继续填transaction并正常返回。
```

输入：未截断的 `int unsigned value`、只用于诊断的 `string context`，以及只读 package localparam `MEMBLOCK_ROB_VALUE_W`。

输出/副作用：合法时返回 `bit [MEMBLOCK_ROB_VALUE_W-1:0]`。函数不创建或修改 transaction，不读写 queue/map/counter/runtime snapshot，不保存 static/package/class 状态；除 fatal report 外无状态副作用。

失败/退出：width 为 0、width 不能形成 64-bit 独占上限、或原始 value 超出当前 width 可表示范围时 `uvm_fatal`，没有 clamp、fallback、饱和、slice或失败返回值；合法路径完成一次 sized cast 后立即返回。

修改后详细文字伪代码：

```text
fit_directed_rob_value_or_fatal承担seq_pkg directed ROB value的唯一转换边界；
先检查MEMBLOCK_ROB_VALUE_W大于0；
再检查MEMBLOCK_ROB_VALUE_W小于longint unsigned位数，确保后续左移能形成独占上限；
任一width检查失败时使用context报告fatal，调用者不再继续；
用64-bit的1左移MEMBLOCK_ROB_VALUE_W，计算第一个不可表示值；
把仍未截断的value提升到longint unsigned后与独占上限比较；
如果value大于等于上限，打印context、原值、width和最大合法值并fatal；
只有原值合法时执行MEMBLOCK_ROB_VALUE_W'(value)显式sized cast；
返回fitted value，供当前builder写入新transaction；
函数退出前不修改任何公共或调用者状态。
```

差异影响：新增一个低频 O(1) 纯转换 helper；超范围 directed 输入从静默截断改为构造期 fail-fast，合法输入值不变。helper 固定落在 `memblock_dispatch_types.sv`，不新增文件、include 或 getter。

### 11.8 修改 `memblock_main_dispatch_manual_main_table_sequence::make_directed_transaction()`

修改原因：该函数当前固定取 `rob_value[8:0]`，既不适配 V2 8-bit ROB value，也绕过共享 fit owner。

输入：`tr_name`、load/store `op_class`、未截断的 `int unsigned rob_value` 和 `base_addr`。

输出/副作用：返回一笔新建的 load 或 store transaction；合法路径只填新对象，不修改 main/status/queue/map/cursor。共享 helper 负责 width/value fatal，object create失败或非 load/store op class继续 fatal。

失败/退出：任一 fatal 路径不返回可用对象；合法路径在 `update_vaddr()` 更新当前对象的派生地址后返回，不存在 fallback transaction。

修改前文字伪代码：

```text
创建transaction，handle为空时fatal；
把rob_value固定低9 bit直接写入tr.robIdx_value；
按load/store case填其余字段，其它op class fatal；
调用update_vaddr只更新当前对象地址；
返回transaction；
不检查调用者原值是否已经被slice改变。
```

修改后文字伪代码：

```text
调用fit_directed_rob_value_or_fatal：
  传入完整rob_value和包含class/function/tr_name的context；
  由helper检查package width、计算独占上限并显式转换；
  helper fatal时本函数不继续，成功时取得同宽fitted value且无公共状态副作用；
创建transaction，handle为空时fatal；
把fitted value写入tr.robIdx_value，不再读取rob_value低位slice；
按原load/store case填其余字段，其他op class仍fatal；
调用update_vaddr只更新当前对象地址；
返回transaction；
函数内不复制helper的width检查或cast逻辑。
```

差异影响：合法 real mixed load/store smoke 的 transaction 语义和退出顺序不变；非法 ROB value 改为共享边界 fatal，函数不再拥有独立转换逻辑。

### 11.9 software dispatch smoke scenario 默认功能修改

修改类型：scenario 默认功能修改，同时复用新增共享 fit helper。该修改只由compile/width专项coding，因为它直接收敛V2当前没有scalar atomic闭环的默认场景。

修改原因：当前 `soft_test_memblock_dispatch_smoke_sequence` 固定构造 load/store/AMO 三笔，且 AMO 使用 MOU/ATOMIC；这与V2当前没有scalar atomic闭环且AMO默认权重应为0的边界冲突。其 builder 还使用参数化低位 slice，若只修 real manual builder，software-only 路径仍可静默截断。

scenario 输入：固定 load/store 地址、ROB value 0/1，以及继承的 common data、LSQ model、issue scheduler、writeback/commit handler。

scenario 输出/副作用：导入两笔 main transaction；软件推进 admission、LOAD/STA/STD fire、三条 pass writeback、两笔 ROB commit、LQ/SQ 各 deq 一笔；最终两笔 status terminal，active map清空且 free count恢复。它修改 software-only 公共模型状态，不驱动真实 DUT interface，不新增状态字段或参数。

scenario 失败/退出：transaction/fit/import、issue progress、writeback、commit数量、deq或 final状态任一不满足时 fatal；AMO/CBO/其它 op class由 builder default fatal；不以 MOU、第三笔占位、fallback 或 timeout作为正常路径。两笔 transaction 全部通过既有 final检查后打印 completed并退出。

修改前逻辑行为：

```text
new把dispatch_smoke_trans_num设为3；
build_directed_main_table清表后构造uid0 load、uid1 store、uid2 AMO；
software make_directed_transaction用参数化低位slice写ROB value；
AMO case写MEMBLOCK_FUTYPE_MOU、MEMBLOCK_LSQ_FLOW_ATOMIC和AMOADD_D；
import后admission、required-target和final循环遍历3个uid；
commit_and_deq_lsq期望3笔ROB commit；
LQ/SQ仍各deq 1；
final检查3笔status、active map和free count后退出。
```

修改后 scenario 文字伪代码：

```text
body保持原调用顺序：build -> admission/route -> fire -> writeback -> commit/deq -> final check；
new把dispatch_smoke_trans_num设为2；
build清空旧表，只构造uid0 load和uid1 store，不创建uid2；
两个构造调用都进入software make_directed_transaction；
builder先调用共享fit helper检查完整ROB value并取得显式转换结果；
builder只保留INT_LOAD和STORE case，AMO/MOU/CBO/其它值走default fatal；
import后data.main_trans_num必须为2；
admission和required-target循环只遍历uid0..1；
uid0 load产生一条LOAD target，uid1 store产生一条STA和一条STD target；
三条target全部fire并各生成一条带snapshot的pass writeback event；
commit helper收集并标记恰好2笔ROB commit；
LQ按count=1释放uid0的load entry，SQ按count=1释放uid1的store entry；
final循环检查两笔transaction及LOAD/STA/STD target全部完成；
final继续要求active ROB/LQ/SQ map为空且LQ/SQ free count恢复满值；
所有检查通过后打印software smoke completed并正常退出。
```

#### `soft_test_memblock_dispatch_smoke_sequence::new()` 合同

输入/输出/副作用/失败/退出：输入可选对象名；输出是完成基类初始化的 sequence 实例。唯一新增副作用是本实例 `dispatch_smoke_trans_num=2`；不写公共状态，无新增失败分支，赋值后返回。

修改前文字伪代码：调用 `super.new(name)` 完成 UVM/基类对象初始化，再设置 `dispatch_smoke_trans_num=3`并退出。

修改后文字伪代码：调用同一 `super.new(name)`，再设置唯一数量字段 `dispatch_smoke_trans_num=2`并退出；不新增第二个期望数量字段，commit/final继续消费该字段。

#### `soft_test_memblock_dispatch_smoke_sequence::build_directed_main_table()` 合同

输入/输出/副作用/失败/退出：输入是固定 transaction 名称、ROB value和地址；输出为 uid 0 load、uid 1 store及 `data.main_trans_num=2`。task 清空并重建 manual/common main table；builder create/fit/default、公共 entry validation或import失败时 fatal，两笔导入完成后返回。

修改前文字伪代码：清表；分别调用 builder 构造 load、store、AMO；setter写索引 0、1、2；`import_manual_main_table()` 校验并导入三笔后退出。

修改后文字伪代码：清表；调用 builder 构造 load和store；setter只写索引 0、1；`import_manual_main_table()` 校验并导入两笔后退出。删除第三次 setter、`dispatch_smoke_amo` 和 uid 2，不用 MOU/CBO/重复 load-store补位。

#### `soft_test_memblock_dispatch_smoke_sequence::make_directed_transaction()` 合同

输入/输出/副作用/失败/退出：输入 `tr_name/op_class/int unsigned rob_value/base_addr`；输出一笔 load或store transaction。只创建并填当前对象，不写公共表或运行期状态。共享 helper拒绝 width/value，对象创建失败或非 load/store op class由本函数 fatal；成功更新当前对象 `vaddr` 后返回。

修改前文字伪代码：创建对象；用 `rob_value[MEMBLOCK_ROB_VALUE_W-1:0]` 写 ROB 字段；load/store/AMO 三个 case分别填 LDU/STU/MOU模板；AMO 使用 ATOMIC 和 AMOADD_D；其它值 fatal；更新 `vaddr` 后返回。

修改后文字伪代码：先调用共享 helper检查未截断 ROB value并取得 fitted value；创建对象并写 fitted value；case只保留 LDU/LOAD/LD 与 STU/STORE/SD；`MEMBLOCK_OP_CLASS_AMO` 和其它值进入 default fatal；源码不再引用 MOU/ATOMIC/AMO fuOpType；更新 `vaddr` 后返回。

差异影响：默认 software smoke 功能从三笔 mixed 场景改为 V2 当前支持的两笔 scalar load/store 场景；issue/commit/deq/final算法本身不改。`admit_lsq_and_route_issue()`、`all_required_targets_dispatched()`、`commit_and_deq_lsq()`、`check_final_status()` 保持参数化函数体并按 2笔复核，不计入修改函数合同。fault/replay smoke 继续继承 uid 0 load和uid 1 store，必须用各自 testcase验证。按用户要求，coding完成后同步`soft_test_and_mixed_directed_flow.md`中的默认两笔场景和共享ROB fit边界。

### 11.10 参数默认修改和无函数对象

`plus.sv`、`seq_csr_common.sv` 静态默认、`default.cfg` 的 `MEMBLOCK_OP_CLASS_AMO_WT` 从 1 改为 0。AMO fuOpType 子权重保留但在 op-class 权重为 0时不被消费。`MEMBLOCK_OP_CLASS_CBO_WT` 已经为 0并保持不变；显式把AMO或CBO op-class权重设为非0，都会由各自独立的V2运行期检测在`validate_and_clamp()` fatal。`MEMBLOCK_OP_CLASS_PREFETCH_WT=1` 保持不变。

`MEMBLOCK_REAL_LSQ_ENQ_MAX`、`MEMBLOCK_REAL_ENQ_WIDTH`、`MEMBLOCK_REAL_LOAD_PIPE_NUM`、`MEMBLOCK_REAL_STA_PIPE_NUM`、`MEMBLOCK_REAL_STD_PIPE_NUM` 从`plus.sv`、`default.cfg`和`seq_csr_common`完整删除，不保留字段、加载、快照或getter。对应物理值只由`MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM`和三类`MEMBLOCK_DUT_*_PIPE_NUM`编译期宏及同名package localparam表达。

`get_compile_*()` 不新增、也不保留兼容 wrapper。原计划中的双读取层方案已删除，所有 `seq_pkg` consumer 直接读取 package localparam。

### 11.11 明确不变和延后边界

- runtime plusarg仍可通过固定`MEMBLOCK_ENQ_PER_CYCLE`和三类`*_PIP_NUM_LIMIT`控制本次行为使用量，但不能配置物理slot/pipe总数；随机enqueue模式保持在完整编译期物理slot范围内采样。
- load/store 地址复用原逻辑继续由主表构建期既有 owner 维护：`build_random_main_table()` 维护 recent load/store 候选，`apply_addr_reuse_window()` 按既有权重选择复用种类并形成地址关系。V2 复核只为确认复用后的 transaction 正确消费 compile 宽度/FuType、LSQ enqueue 和 split issue 字段链；最终不修改候选选择、复用概率、load/store 地址关系、fallback 或复用算法，也不改变 ROB/LQ/SQ key、既有 issue generation、MMIO active-instance provenance、pass/fail 或 terminal。normal 自动主表地址的参数来源和合法槽失败策略由主表 VADDR 专项修改；`apply_addr_reuse_window()` 在两个专项中均无行为 diff，不计入本 plan 的12个函数合同。
- 本 plan 不修改 LSQ enqueue payload 生成、issue target 选择、writeback event 归一化、MMIO 状态功能或 redirect/replay 生命周期。
- 本plan对vecissue/vector-WB只完成编译宽度一致性；scalar调度禁用和vecissue driver fatal由split issue专项
  实现，VSTU feedback fatal由IQ feedback/replay专项实现，`writebackVldu` fatal由monitor output专项实现。
- atomic 和 CBO 完整支持分别延后到对应专项 plan；本 plan 负责参数初始化和所有scalar主表入口的运行期检测，在admission前fail-fast，以及默认 software smoke 不再构造 AMO/MOU。
- software prefetch 默认权重1、PREFETCH behavior和LDU->LOAD/`issueLda`路径不变。
- compile/width 专项是 package fit helper、两个 builder 调用和 software smoke 两笔收敛的唯一 owner；其它专项不重复 coding。
- compile/width 专项也是 `MEMBLOCK_DUT_LOAD_PORT_BASE` 默认宏、同名 package localparam及 base/total/port唯一性检查的唯一 owner；split issue plan只能消费，不能定义默认或复制检查权威。
- 本 plan 不实现 RM/checker/coverage。

### 11.12 新增/修改函数合同计数

本 plan 最终包含12个新增或修改函数合同；LSQ enqueue的`clear_lsqenq_xaction()/assign_lsqenq_slot()`和split issue的`route_all_ready_uids()`继续由各自专项拥有函数合同，本plan只提供其消费的compile localparam，不重复计数。

- 新增4个：`check_compile_param_consistency()`、`encode_and_fit_dut_futype()`、`seq_pkg::fit_directed_rob_value_or_fatal()`、`seq_csr_common::apply_runtime_resource_limits()`。
- 修改8个：`plus::reload_from_cmdline()`、`seq_csr_common::validate_and_clamp()`、`seq_csr_common::get_enq_per_cycle()`、`memblock_dispatch_base_sequence::validate_main_table_entry()`、`memblock_main_dispatch_manual_main_table_sequence::make_directed_transaction()`、`soft_test_memblock_dispatch_smoke_sequence::new()`、`soft_test_memblock_dispatch_smoke_sequence::build_directed_main_table()`、`soft_test_memblock_dispatch_smoke_sequence::make_directed_transaction()`。

总控应按12个compile/width函数合同重算。第6.10/11.9节列出的admission、required-target、commit/deq、final、fault和replay函数只复核两笔场景下的既有参数化合同，函数体不修改，因此不计入上述12个。

参数合同单独机械计数：`MEMBLOCK_DUT_LOAD_PORT_BASE`和四个slot/pipe资源参数均各自只有`.svh`默认宏与package同名只读localparam两个同源载体；不计入12个函数合同。split issue和LSQ enqueue专项新增0个同义参数owner，只消费localparam。

## 12. Coding执行记录

### 12.1 完成内容

- `memblock_compile_params.svh`已建立V2 ROB/LQ/SQ、FTQ、FuType、slot/pipe、MMIO
  load port和能力宏默认值；`memblock_dispatch_types.sv`只读暴露同源localparam。
- raw类型、公共transaction和本plan列出的agent interface/xaction/monitor/XZ字段已改读
  compile宏；DUT-facing FuType写入统一调用`encode_and_fit_dut_futype()`。
- 五个`MEMBLOCK_REAL_*`物理结构plus已从定义、加载、快照、getter和default cfg删除；
  `apply_runtime_resource_limits()`统一处理runtime enqueue/pipe使用量。
- AMO默认权重已改为0，显式AMO/CBO配置和无歧义主表语义在admission前fatal；
  software prefetch保持合法。
- package唯一`fit_directed_rob_value_or_fatal()`已被real manual和software两个builder调用；
  两个builder均无低位slice。
- software smoke已固定为uid 0 load和uid 1 store；normal/fault/replay三项smoke均按两笔场景通过。
- `load_sta_std_issue_flow.md`、`lsq_admission_flow.md`、
  `soft_test_and_mixed_directed_flow.md`、参数管理说明和Atomic/CBO TODO已同步当前边界。

### 12.2 IMPLEMENTATION_DELTA

1. Plan草案原要求`validate_main_table_entry()`脱离语义域单独调用
   `is_amo_fuoptype()`拒绝AMO。实际仿真证明LSUOp编码在普通load/store和AMO之间复用，
   例如普通`LD`数值也可被AMO分类helper命中。最终实现只用`op_class=AMO`、`fuType=MOU`
   或`lsq_flow=ATOMIC`三个无歧义字段前置拒绝，再由既有模板校验检查语义域内fuOpType；
   这避免误杀合法scalar load/store，同时仍覆盖所有主表来源。
2. VCS Q-2020不接受`longint unsigned'(value)`语法。共享ROB fit helper改为先赋值到
   `longint unsigned promoted_value`再比较独占上限，检查和显式sized cast合同不变。
3. 为满足compile port layout单一权威，`issue_field_assigner::is_valid_pipe_idx()`和
   `memblock_issue_dispatch_base_sequence::mark_fired_items()`分别改读pipe count和port base
   localparam。这是既有函数内的机械参数consumer替换，不新增target路由、ready映射或业务函数
   owner，因此不计入第11.12节12个功能合同。完整split issue物理ready/fire映射仍由split专项负责。
4. `lsq_ctrl_model::is_vector_ls_futype()`的输入容器同步改为
   `MEMBLOCK_INTERNAL_FUTYPE_W`。该文件未在最初机械字段清单中单列，但它是公共FuType容器
   consumer，若保留固定36会形成第二宽度权威；行为和vector不支持边界不变。
5. 按用户“执行完成同步对应文档”的明确要求，`soft_test_and_mixed_directed_flow.md`从只读
   核对输入升级为同步写目标，记录两笔software smoke和共享ROB fit边界。
6. `tb/dut_inst.sv`中三条`loadMmioUop.robIdx_value`仍保留V2 RTL展开后的固定8-bit wire。
   该文件是DUT边界声明，不是测试框架内部第二宽度权威；agent/raw consumer已使用compile宏。

### 12.3 验证结果

静态检查于2026-07-13通过：

- `git diff --check`通过。
- 五个旧`MEMBLOCK_REAL_*`和旧reject helper无残留。
- compile getter、directed ROB低位slice、software smoke AMO第三笔、sequence内固定3/2/2、
  `+3/+5`和固定full-mask扫描均无残留。
- shared ROB fit helper定义唯一且两个builder均调用；software main table setter恰好两次。
- V2 LSQ enqueue权威端口仍为slot 0..5，不存在slot 6及以上。
- ROB/LQ/SQ、FTQ和FuType目标agent字段及X/Z宽度扫描通过；connect无新增slice或补零。

远端VCS验证结果：

| 验证项 | 结果 | 说明 |
|---|---|---|
| `make eda_compile tc=tc_sanity mode=base_fun` | 通过 | VCS Q-2020完整编译、partition和stitch均为0 error |
| `tc_sanity` | 通过 | 使用已编译`simv`执行`batch_run`；关闭不适用于无主表sanity的LSQ enqueue/commit常驻sequence |
| `tc_dispatch_smoke` | 通过 | 两笔ROB commit、LQ/SQ各deq一笔，`UVM_ERROR/FATAL=0` |
| `tc_dispatch_fault_smoke` | 通过 | uid 0 load fault、uid 1 store正常收敛，`UVM_ERROR/FATAL=0` |
| `tc_dispatch_replay_smoke` | 通过 | uid 1 store STA replay后收敛，`UVM_ERROR/FATAL=0` |

`make eda_run`会先再次执行compile；一次被人工终止的增量compile造成VCS生成数据库损坏，
删除工具明确报告的`tdc.sdb`后重新完整compile通过。最终仿真统一使用
`make eda_batch_run`消费该已验证`simv`，避免重复compile。默认`tc_sanity`不建立主表，若保持
`MEMBLOCK_LSQENQ_SEQ_EN=1/MEMBLOCK_LSQCOMMIT_SEQ_EN=1`会由两个常驻sequence持续等待；
sanity验证因此显式置两者为0，该限制不影响三项dispatch smoke。

## 与初步 plan 差异说明

本章只总结本 `do` plan 已完成范围相对初步方案的功能差异，不承载后续 LSQ MMIO/status 的
pending 实现。后续 SQ deq/cancel count 与 redirect/cancel latency 宏由对应 `undo` plan 唯一拥有。

| 修改项 | 修改类型 | 修改前逻辑 | 变更原因 | 最终逻辑与影响 |
|---|---|---|---|---|
| 编译期结构权威 | 编译期参数/字段适配 | ROB/LQ/SQ/FuType/slot/pipe散落固定值，并保留同义runtime硬件参数 | V2/V3结构必须在elaboration前固定 | `memblock_compile_params.svh`是既有结构唯一默认入口，package只读暴露localparam；删除五个硬件结构plus，runtime只保留行为使用量 |
| FuType写入 | 编码与失败策略修改 | 36-bit内部/V3编码可能被直接裁剪到V2 DUT 35-bit | 裁剪可能把不支持target伪装成合法编码 | `encode_and_fit_dut_futype()`检查one-hot bit和DUT宽度后无损转换；非法组合fatal，不改变合法route顺序 |
| runtime资源限制 | runtime参数收敛修改 | 多个getter各自使用固定slot/pipe上限 | 分散clamp会形成第二结构权威 | `apply_runtime_resource_limits()`集中按compile localparam收敛enqueue/pipe使用量；`check_compile_param_consistency()`只校验、不写回 |
| directed ROB value | 字段宽度与失败策略修改 | real/software builder分别做低位slice | V2 ROB value缩窄后可能静默截断 | `fit_directed_rob_value_or_fatal()`是共享入口，先检查可编码再显式cast；两个builder均调用，合法UID/ROB顺序不变 |
| software smoke | 默认场景功能修改 | 固定load/store/AMO三笔，AMO在V2尚未闭环 | 默认场景会生成当前不支持激励 | 场景收敛为load/store两笔，commit期望改为2，LQ/SQ仍各deq一笔；主表、issue、writeback、commit/deq算法不变 |

关键 helper 差异：新增的 `check_compile_param_consistency()` 输入为compile localparam、只产生
fatal诊断；`encode_and_fit_dut_futype()` 输入内部FuType和target、输出可直接驱DUT的无损编码；
`fit_directed_rob_value_or_fatal()` 输入directed整数和caller、输出V2 ROB value；
`apply_runtime_resource_limits()` 输入已加载runtime快照、只修改行为上限。修改后的
`validate_and_clamp()` 只编排公共检查与资源收敛，不再读取已删除硬件plus；两个
`make_directed_transaction()` 不再自行slice。上述函数的逐分支文字伪代码见第6章，实际完成差异与
验证见第12章。

### 审稿用四要素伪代码

```text
修改目的：
  用compile profile替代散落literal/runtime镜像，并拒绝FuType或directed ROB value静默截断。
修改前逻辑行为：
  getter各自按固定slot/pipe clamp；builder直接slice ROB value；36-bit内部FuType可能裁成35-bit DUT值；
  software smoke固定生成load/store/AMO三笔。
修改后逻辑行为：
  validate_and_clamp统一调用compile一致性检查和runtime资源收敛；两个builder调用共享ROB fit helper；
  FuType先检查target one-hot和DUT位宽再转换；V2 software smoke只生成load/store两笔。
差异影响：
  改变编译期参数来源、越界失败策略和V2 smoke默认内容；不改变合法ROB/LQ/SQ顺序、issue、WB、commit/deq算法。
```

### 新增/修改 Helper 详细伪代码

```text
check_compile_param_consistency()：
  添加原因：集中发现profile宽度、端口数和FuType bit冲突。
  输入为compile localparam；无输出和状态副作用；逐项检查非零、派生关系、bit唯一且可表示，失败fatal。

encode_and_fit_dut_futype(internal_futype,target)：
  添加原因：禁止把V3/36-bit编码直接裁剪到V2 35-bit接口。
  校验target受支持、编码one-hot且置位bit小于DUT宽度；成功返回无损DUT编码，失败fatal；不修改transaction。

fit_directed_rob_value_or_fatal(value,caller)：
  添加原因：两个directed builder不能各自保留低位slice。
  检查value可由MEMBLOCK_DUT_ROB_VALUE_W完整表示；成功显式cast并返回，失败携caller fatal；无表状态副作用。

apply_runtime_resource_limits(runtime_snapshot)：
  添加原因：runtime行为使用量必须统一受compile物理资源约束。
  读取已加载runtime值和compile上限；只收敛enqueue/pipe行为上限，不修改compile参数或接口宽度。

validate_and_clamp() / make_directed_transaction() 修改：
  前者修改前分散读取硬件plus，修改后只编排check_compile_param_consistency和resource limits；
  后者修改前自行slice ROB value，修改后调用共享fit helper，再保持原transaction字段构造顺序。
```
