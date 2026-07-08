# mem_ut V2 接口对齐问题 review 记录

## 1. Review 目标

本文记录 2026-07-08 对当前 `mem_ut_uvm_v2` 分支测试环境和 V2 生成 RTL 的接口对齐复查结果。

本文先记录只读复查发现的问题；2026-07-08 后续已按用户要求对“非测试框架、非需用户确认”的明确问题完成代码修复，并在本文中同步标注修复状态。复查范围包括：

```text
build/rtl/MemBlock.sv
mem_ut/ver/ut/memblock/tb/dut_inst.sv
mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
mem_ut/ver/ut/memblock/agent/*/src/*_interface.sv
mem_ut/ver/ut/memblock/agent/*/src/*_xaction.sv
mem_ut/ver/ut/memblock/agent/*/src/*_driver.sv
mem_ut/ver/ut/memblock/agent/*/src/*_monitor.sv
```

Review 目标是检查当前测试环境 interface、接口连接、`dut_inst`、agent transaction、driver、monitor 是否和 V2 `MemBlock.sv` 对齐，是否存在遗漏、多余、方向错误或旧 V3 字段残留。

## 2. 已确认闭合项

### 2.1 `dut_inst.sv` 顶层端口闭合

以 `build/rtl/MemBlock.sv` 的 `module MemBlock` 端口为权威来源，对比 `tb/dut_inst.sv` 的端口声明和实例连接：

```text
V2 RTL 顶层端口数：1334
dut_inst 实例连接端口数：1334
缺失实例端口：0
多余实例端口：0
声明方向/位宽不匹配：0
```

仅 `clock` 和 `reset` 没有在 `dut_inst.sv` 内按普通 wire/reg 声明，它们分别由 testbench 顶层 `clk` 和 reset 连接提供，不属于 DUT 端口遗漏。

### 2.2 connect 引用存在性闭合

扫描 `tb/*_agent_connect.sv` 中所有 `RTL_PATH.*` 和 `U_IF_NAME.*`：

- 未发现 connect 引用不存在的 V2 RTL 顶层端口。
- 未发现 connect 引用不存在的 V2 RTL 内部 wire。
- 未发现 connect 引用不存在的 interface 信号。

### 2.3 interface 与 xaction 字段基本闭合

扫描 `agent/*/src/*_interface.sv` 中声明的 interface 信号与同名 agent `*_xaction.sv` 字段：

- 已连接到 connect 的非 ready 信号，均能在对应 xaction 中找到字段。
- xaction 中额外出现的 `super_result` 以及少量 dispatch 控制字段属于 agent 本地控制字段，不是 DUT 接口残留。

### 2.4 V3-only 顶层端口残留检查

使用 V2 `build/rtl/MemBlock.sv` 和旧 V3 `build_memblock/rtl/MemBlock.sv` 的端口集合做精确 token 扫描：

```text
agent：未发现精确匹配的 V3-only 顶层端口残留
tb：未发现精确匹配的 V3-only 顶层端口残留
seq：未发现精确匹配的 V3-only 顶层端口残留
env：未发现精确匹配的 V3-only 顶层端口残留
```

这说明当前主要问题不是 V3 顶层端口名仍直接残留，而是部分 V2 映射后的 agent 字段链路仍没有完全裁剪或补齐。

## 3. 阻塞问题

### 3.1 `other_ctrl_agent_connect.sv` 漏接 `io_reset_backend`：已修复

问题位置：

```text
mem_ut/ver/ut/memblock/tb/other_ctrl_agent_connect.sv
mem_ut/ver/ut/memblock/agent/other_ctrl_agent_agent/src/other_ctrl_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/other_ctrl_agent_agent/src/other_ctrl_agent_agent_monitor.sv
```

V2 RTL 中 `io_reset_backend` 是真实存在的 DUT output：

```text
build/rtl/MemBlock.sv:1303
mem_ut/ver/ut/memblock/tb/dut_inst.sv:1255
mem_ut/ver/ut/memblock/tb/dut_inst.sv:3341
```

当前 interface 和 monitor 已经有该字段：

```text
other_ctrl_agent_agent_interface.sv:34
other_ctrl_agent_agent_interface.sv:56
other_ctrl_agent_agent_interface.sv:79
other_ctrl_agent_agent_monitor.sv:70
other_ctrl_agent_agent_monitor.sv:89
```

但 `other_ctrl_agent_connect.sv` 只连接到 `io_outer_cpu_halt`，没有：

```systemverilog
force U_IF_NAME.io_reset_backend = RTL_PATH.io_reset_backend;
```

中文伪代码：这段连接逻辑承担“把 V2 DUT 顶层 backend reset 状态送入 other_ctrl agent interface”的功能。connect 从 RTL 顶层 `io_reset_backend` 读取真实输出值，再 force 到 `U_IF_NAME.io_reset_backend`，后续 monitor 才能在 interface 上采样并做 X/Z 检查。该逻辑只采样 DUT output，不允许 driver 反向驱动该信号。

影响：

- monitor 会读取未被 connect 驱动的 `io_reset_backend`。
- `TCNT_CHECK_SIG_XZ` 可能对未连接 interface 信号报 X/Z。
- 测试环境无法真实采样 V2 DUT 的 backend reset 状态。

后续代码修复建议：

- 在 `MEMBLOCK__OTHER_CTRL_AGENT_CONNECT` 的 `MEMBLOCK_UT` 和非 `MEMBLOCK_UT` 分支都补齐 `io_reset_backend` 从 RTL 到 interface 的连接。
- 同时确认 `other_ctrl_agent_agent_interface.sv` 中 `io_reset_backend` 在 `drv_cb` 保持 `input`，driver 不应驱动该 DUT output。

本轮修复状态：

- 已在 `mem_ut/ver/ut/memblock/tb/other_ctrl_agent_connect.sv` 的 `MEMBLOCK_UT` 和非 `MEMBLOCK_UT` 分支补齐：
  `force U_IF_NAME.io_reset_backend = RTL_PATH.io_reset_backend;`
- 已复查 `other_ctrl_agent_agent_interface.sv`，`io_reset_backend` 在 `drv_cb` 和 `mon_cb` 中均为 `input`。
- 修复后 monitor 不再读取悬空的 `io_reset_backend` interface 字段。

### 3.2 `io_mem_to_ooo_int_wb_agent` 保留大量 V2 RTL/connect 无来源字段：非测试框架字段已修复，公共状态字段转测试框架 plan

问题位置：

```text
mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_int_wb_agent_connect.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv
```

当前 `io_mem_to_ooo_int_wb_agent` interface 有 198 个信号，connect 只连接 125 个，剩余 73 个 interface 信号没有来源。这 73 个信号不是纯注释字段，它们同时存在于 monitor 和 xaction 中。

未连接字段按端口分组如下：

| 分组 | 未连接字段数量 | 典型字段 |
|---|---:|---|
| `io_mem_to_ooo_intWriteback_0_0` | 17 | `exceptionVec_0/1/2/8/9/10/11/12/14/16/17/18/20/22`、`isRVC`、`lqIdx_flag/value` |
| `io_mem_to_ooo_intWriteback_1_0` | 21 | `exceptionVec_0/1/2/6/7/8/9/10/11/12/14/15/16/17/18/20/22/23`、`isRVC`、`lqIdx_flag/value` |
| `io_mem_to_ooo_intWriteback_2_0` | 21 | 同类 `exceptionVec`、`isRVC`、`lqIdx_flag/value` 字段 |
| `io_mem_to_ooo_intWriteback_3_0` | 5 | `ready`、`pdest`、`isRVC`、`sqIdx_flag/value` |
| `io_mem_to_ooo_intWriteback_4_0` | 3 | `isRVC`、`sqIdx_flag/value` |
| `io_mem_to_ooo_intWriteback_5_0` | 3 | `robIdx_flag`、`sqIdx_flag/value` |
| `io_mem_to_ooo_intWriteback_6_0` | 3 | `robIdx_flag`、`sqIdx_flag/value` |

具体判断依据：

- V2 `build/rtl/MemBlock.sv` 对 `writebackStd_0/1` 顶层只暴露 `valid` 和 `robIdx_value`，没有 `robIdx_flag`、`sqIdx_flag`、`sqIdx_value`。
- V2 `writebackSta_0/1` 和 `writebackLda_0/1/2` 顶层暴露的是分散后的字段子集，不等价于 V3 聚合 `intWriteback_*` 完整 bundle。
- 当前 monitor 会读取这些未连接字段并做 X/Z 检查。
- `io_mem_to_ooo_int_wb_agent_agent_monitor.sv` 还会将部分字段写入 `memblock_sync_pkg::dispatch_raw_int_wb_t`，例如 `rob_flag`、`lq_flag`、`lq_value`、`exception_vec[]` 等。

影响：

- 未连接字段可能为 X，造成 X/Z 检查误报。
- 未连接字段若进入 `raw_int_wb` 公共队列，会污染后续 writeback 状态处理。
- 当前 agent 命名仍保留 `intWriteback_*` 聚合形态，实际 V2 RTL 是 `writebackLda/Sta/Std` split 形态，字段语义没有完全收敛。

分类处理建议：

- 未进入 `memblock_sync_pkg::dispatch_raw_int_wb_t`、不影响公共状态流的 V2 无来源字段，应从 interface、xaction、monitor 和 X/Z 检查中删除，不再保留悬空采样字段。已确认的直接删除候选包括 `isRVC` 残留字段、`io_mem_to_ooo_intWriteback_3_0_ready` 和无 V2 来源的 `io_mem_to_ooo_intWriteback_3_0_bits_pdest`。本轮已从 interface、xaction、monitor、driver 和 X/Z 检查中删除这些字段。
- 已经写入 `memblock_sync_pkg::dispatch_raw_int_wb_t` 的字段属于测试框架运行期逻辑输入，不应在 connect 层猜测常量或直接删除；这些字段必须进入 V2 测试框架适配 plan，明确 V2 语义来源、默认值策略或公共状态替代路径。
- 对 `writebackStd_0/1` 只暴露 `robIdx_value` 的情况，V2 测试框架适配 plan 必须明确 `rob_valid/rob_flag/sq_valid/sq_flag` 的语义来源，避免继续读取未连接 interface 字段。
- 本项测试框架待办已登记到：
  `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_int_wb_writeback_framework_adapt_plan_20260708.md`。

本轮直接删除字段如下：

```text
io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_isRVC
io_mem_to_ooo_intWriteback_3_0_ready
io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_isRVC
io_mem_to_ooo_intWriteback_3_0_bits_pdest
io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_isRVC
io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_isRVC
io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_isRVC
```

本轮保留为测试框架待办的字段包括 `rob_flag/lq_flag/lq_value/sq_flag/sq_value/exception_vec[]` 等会进入 `dispatch_raw_int_wb_t` 的字段。这些字段后续必须在测试框架 plan 中明确 V2 语义来源，不能在本轮 connect 层猜常量。

## 4. 非阻塞问题和语义风险

### 4.1 所有顶层 agent monitor 当前不向 analysis port 写 transaction

问题位置：

```text
mem_ut/ver/ut/memblock/agent/*/src/*_monitor.sv
```

扫描结果显示当前 20 个顶层 agent monitor 均没有实际执行 `mon_item_port.write(mon_tr)`，相关 transaction 创建和写出逻辑大多处于注释状态。

受影响 agent 包括：

```text
L2tlb_agent_agent
backendToTopBypass_agent_agent
csr_ctrl_agent_agent
dcache_agent_agent
fence_agent_agent
int_sink_agent_agent
io_mem_to_ooo_ctrl_agent_agent
io_mem_to_ooo_int_wb_agent_agent
io_mem_to_ooo_iq_feedback_agent_agent
io_mem_to_ooo_vec_wb_agent_agent
io_mem_to_ooo_wakeup_agent_agent
itlb_agent_agent
lintsissue_agent_agent
lsqcommit_agent_agent
lsqenq_agent_agent
other_ctrl_agent_agent
prefetch_agent_agent
redirect_agent_agent
sbuffer_agent_agent
vecissue_agent_agent
```

风险：

- 即使 interface 和 connect 已经采样到 V2 DUT 信号，标准 UVM analysis port 链路不会输出 transaction。
- RM/scoreboard 或 coverage 如果依赖 agent monitor transaction，将拿不到事件。
- 当前部分 flow 可能绕过 analysis port，直接写 `memblock_sync_pkg` raw queue；这种路径需要单独确认覆盖范围，不能视为所有 agent monitor 都已闭合。

后续 plan 记录建议：

- 后续测试框架适配 plan 需要逐个 agent 判断 monitor 的职责：只做 X/Z 检查、写 raw queue、写 analysis port，还是三者都需要。
- 对需要进入 RM/scoreboard 的 agent，后续修复 plan 可考虑恢复或重写 `mon_tr` 创建、字段赋值、`unpack()` 和 `mon_item_port.write(mon_tr)`。
- 对明确不需要 transaction 输出的 agent，在规则或 review 文档中说明原因，避免误以为已完整闭合。
- 本项已登记到测试框架待办 plan：
  `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_monitor_output_framework_adapt_plan_20260708.md`。

### 4.2 `other_ctrl_agent` 中 `io_outer_cpu_halt` 的 `drv_cb` 方向存在卫生风险：已修复

已确认 `other_ctrl_agent_agent_interface.sv` 中 `io_outer_cpu_halt` 是由 RTL 驱动到 interface 的 DUT output，但在 `drv_cb` 中仍声明成 `output`：

```text
mem_ut/ver/ut/memblock/agent/other_ctrl_agent_agent/src/other_ctrl_agent_agent_interface.sv:36
mem_ut/ver/ut/memblock/agent/other_ctrl_agent_agent/src/other_ctrl_agent_agent_interface.sv:58
mem_ut/ver/ut/memblock/tb/other_ctrl_agent_connect.sv:32
mem_ut/ver/ut/memblock/tb/other_ctrl_agent_connect.sv:50
```

当前 driver 扫描未发现该字段被实际赋值，因此暂未形成“driver 正在驱动 DUT output”的直接错误。但该声明方向不符合规则中“driver 只能驱动 DUT input”的要求。

风险：

- 后续扩展 driver 时容易误驱 DUT output。
- interface clocking 方向不能准确表达 DUT 与 agent 的职责边界。

后续 plan 记录建议：

- 本轮已将 `io_outer_cpu_halt` 在 `drv_cb` 中改为 `input`，与 DUT output 方向一致。
- 最后一轮 review 额外发现 `io_mem_to_ooo_int_wb_agent_agent_interface.sv` 中新增的 V2 `io_mem_to_ooo_writebackLda/Sta_*` DUT output 在 `drv_cb` 中也曾声明为 `output`，本轮已统一修正为 `input`。
- 本轮再次全量扫描 `MEMBLOCK_UT` 活动分支后，已补齐其他同类方向问题的逐字段证据，并完成修复：
  - `io_mem_to_ooo_ctrl_agent_agent_interface.sv` 中 8 个 `lsqio_loadMmio/storeMmio` DUT output 字段已由 `drv_cb output` 改为 `drv_cb input`。
  - `io_mem_to_ooo_iq_feedback_agent_agent_interface.sv` 中 6 个 `vstuIqFeedback` replay 字段已由 `drv_cb output` 改为 `drv_cb input`。
  - `io_mem_to_ooo_vec_wb_agent_agent_interface.sv` 中 73 个 `writebackVldu_0/1` DUT output 字段已由 `drv_cb output` 改为 `drv_cb input`。
  - `vecissue_agent_agent_interface.sv` 中 `io_ooo_to_mem_issueVldu_0_ready`、`io_ooo_to_mem_issueVldu_1_ready` 已由 `drv_cb output` 改为 `drv_cb input`。
  - 修复前扫描已确认这些字段在当前 driver 中没有实际赋值、monitor 中有采样；本轮修改只收紧 interface 方向，不改变 driver/monitor 行为。

### 4.3 L2TLB response 中 `s2_entry_perm_g/u` 常量化问题已修复

问题位置与修复位置：

```text
mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv
mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_interface.sv
mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_xaction.sv
mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv
build/rtl/MemBlock.sv
```

V2 RTL 内部存在：

```text
_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g
_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_u
```

修改前 connect 将这两个字段固定为 0：

```systemverilog
force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g = '0;
force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_u = '0;
```

中文伪代码：这段旧逻辑在修改前承担“给 V2 内部 S2 G/U 权限位提供占位值”的功能。connect active 分支直接把 DUT 内部 `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u` force 为 0，不读取 `L2TLB_agent` transaction，也不允许 sequence 按 TLB entry 动态控制这两个权限位。该旧逻辑的副作用是 responder 即使查表得到 `pte_g/pte_u`，也无法把结果送回 DUT 内部 response。

本轮已补齐真实驱动链路：

```text
memblock_tlb_entry.pte_g/pte_u
  -> memblock_l2tlb_base_sequence::fill_dtlb_resp_from_entry()
  -> L2tlb_agent_agent_xaction.io_ptw_resp_bits_s2_entry_perm_g/u
  -> L2tlb_agent_agent_driver::send_pkt()
  -> L2tlb_agent_agent_interface.io_ptw_resp_bits_s2_entry_perm_g/u
  -> L2tlb_agent_connect.sv active 分支
  -> RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u
```

修改后 connect 不再把 active 接管路径固定为 0，而是由 `L2TLB_agent`
transaction/sequence 真实驱动。`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=0`
的非接管分支仍保持 interface 非激活置 0，延续原有“关闭接管即不声明
agent 驱动 DUT response”的行为。

当前结论：

- 该项不再作为未修复问题保留。
- `s2_entry_perm_g/u` 语义来源与 S1 `perm_g/u` 保持一致，均来自 `memblock_tlb_entry.pte_g/pte_u`。
- 后续若 V2 RTL 重新生成导致 `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u` 内部 wire 名变化，需要按 V2 L2TLB profile 重新复查连接点。

### 4.4 部分 V2 新增顶层 output 仅在 `dut_inst.sv` 中实例化，待分析是否影响主功能

当前 V2 顶层存在一些 output 已经在 `dut_inst.sv` 中声明并实例化，但没有对应 agent 采样策略：

```text
io_l2_tlb_req_resp_*
io_l2_pmp_resp_*
io_outer_l2PfCtrl_*
io_wfi_wfiSafe
```

其中 `io_l2_tlb_req_resp_*` 和 `io_l2_pmp_resp_*` 是 V2 顶层 L2 侧 TLB/PMP response，不应误接到当前 `L2TLB_agent`，因为当前 `L2TLB_agent` 接管的是内部 DTLB -> L2TLB/PTW responder 通路。

当前判断：

- 这些信号目前不是 `tc_sanity/base_fun` 已知主激励闭环的必要输入，也没有证据显示当前主功能流必须依赖它们进入 agent transaction。
- 因此本轮不直接要求接入 agent，避免扩大接口适配范围。
- 但它们属于 V2 顶层真实 output，后续必须分析是否影响主功能、RM/checker 观察点、低功耗或 L2 侧 TLB/PMP/PF 控制场景。

后续 plan 记录建议：

- 已写入测试环境待办 plan：`AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_monitor_output_framework_adapt_plan_20260708.md`。
- 后续先按测试目标和主功能依赖分析这些接口是否需要接入 agent；若不影响当前主功能，暂不接入。
- 若分析确认 testcase、RM、checker 或 coverage 需要观察，再决定新增独立 monitor/agent 或并入 `other_ctrl_agent`。
- 不要把 `io_l2_tlb_req_resp_*` 误接到内部 L2TLB responder agent。

## 5. 本轮未发现的问题

本轮未发现以下机械类问题：

- `dut_inst.sv` 相对 V2 `MemBlock.sv` 顶层端口有遗漏或多余。
- `dut_inst.sv` 中已声明端口的方向或位宽与 V2 RTL 不一致。
- `tb/*_agent_connect.sv` 中存在不存在的 `RTL_PATH.*` 引用。
- `tb/*_agent_connect.sv` 中存在不存在的 `U_IF_NAME.*` 引用。
- connected 非 ready interface 字段在对应 xaction 中缺失。
- `agent/tb/seq/env` 中存在精确匹配的 V3-only 顶层端口残留。
- 当前 driver 实际赋值语句中发现对 DUT output 的写入。

## 6. 后续建议

建议后续修复按以下顺序拆分：

1. `other_ctrl_agent_connect.sv` 的 `io_reset_backend` 漏接已修复。
2. `io_mem_to_ooo_int_wb_agent` 中不进入公共状态流的 V2 无来源字段已删除；进入 `dispatch_raw_int_wb_t` 的字段继续按 V2 测试框架适配 plan 处理。
3. `other_ctrl_agent` 的 `io_outer_cpu_halt` 已修正为 `drv_cb input`；本轮再次扫描发现的 `io_mem_to_ooo_ctrl`、`io_mem_to_ooo_iq_feedback`、`io_mem_to_ooo_vec_wb`、`vecissue` 同类 `drv_cb` 方向问题也已修复。
4. 后续按 `mem_ut_v2_monitor_output_framework_adapt_plan_20260708.md` 建立 monitor 输出策略：哪些 agent 只做 X/Z，哪些写 raw queue，哪些必须写 analysis port。
5. 后续按同一 plan 对 `io_l2_tlb_req_resp_*`、`io_l2_pmp_resp_*`、`io_outer_l2PfCtrl_*` 和 `io_wfi_wfiSafe` 先做主功能影响分析；若不影响当前主功能，暂不接入 agent。

## 7. 复查结论摘要

当前结论如下：

- `dut_inst.sv` 顶层端口无遗漏/多余，方向和位宽匹配。
- connect 中未发现不存在 RTL 路径或不存在 interface 字段的机械错误。
- `other_ctrl_agent_connect.sv` 漏接 `io_reset_backend` 已修复。
- `other_ctrl_agent` 中 `io_outer_cpu_halt` 的 `drv_cb` 方向卫生风险已修复。
- 本轮再次 review 发现的 89 个 DUT output 在 `drv_cb` 中误标为 `output` 的方向卫生问题已修复，当前未发现活动 `MEMBLOCK_UT` 分支仍存在 `RTL -> interface` 但 `drv_cb output` 的字段。
- `io_mem_to_ooo_int_wb_agent` 中不进入公共状态流的 V2 无来源字段已删除；进入 `dispatch_raw_int_wb_t` 的字段保留为测试框架 plan 待办。
- 剩余非阻塞问题包括 monitor analysis port 输出策略未闭合、V2 新增顶层 output 尚未完成主功能影响分析。
