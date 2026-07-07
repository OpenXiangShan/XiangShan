# mem_ut V2 DUT 接口宏隔离与参数化适配修改方案

## 1. Plan 定位

本文是接口适配修改方案，只分析当前 `mem_ut` testbench/interface 与当前 V2 生成后 Verilog 接口之间的差异，并提出后续 coding 时通过 V2/V3 宏隔离和参数宏统一适配的修改方案。

本文不修改源码，不提交 git，不修改测试框架运行期逻辑。后续 coding 范围仅限 DUT 接口声明、connect 宏、agent interface、transaction/xaction、driver/monitor 字段和编译期参数宏；不修改 sequence 主循环、monitor service loop、handler、adapter、scheduler、公共状态表、CSR runtime snapshot 或现有测试框架调度逻辑。

## 2. 规则与权威来源

本方案遵守以下规则：

- `AI_DOC/project_management/ai_doc_language_rule.md`
- `AI_DOC/project_management/ai_doc_file_management_rule.md`
- `AI_DOC/project_management/mem_ut_test_framework_plan_review_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_latest_dut_adapt_rule.md`
- `mem_ut/ver/ut/memblock/rule/version/v2/dut_interface_baseline.md`
- `mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md`
- `mem_ut/ver/ut/memblock/rule/memblock_l2tlb_agent_rule.md`
- `mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md`

接口权威来源为当前 V2 Verilog：

- `build_memblock/rtl/MemBlock.sv`
- `build_memblock/rtl/MemBlockTop.sv`

当前 testbench 入口为：

- `mem_ut/ver/ut/memblock/tb/top_tb.sv`
- `mem_ut/ver/ut/memblock/tb/dut_inst.sv`
- `mem_ut/ver/ut/memblock/tb/tc_if_connect.sv`
- `mem_ut/ver/ut/memblock/tb/memblock_connect.sv`
- `mem_ut/ver/ut/memblock/tb/*_agent_connect.sv`
- `mem_ut/ver/ut/memblock/agent/**`

当前仓库中未发现 `mem_ut/ver/ut/memblock/subagent` 目录，因此本次扫描记录为 `subagent/**` 不存在。

## 3. 扫描方法

本次扫描使用结构化脚本读取 Verilog module header、`dut_inst.sv` 实例端口、connect 宏中的 `RTL_PATH.*` 引用，以及 agent interface/xaction 字段。核心方法如下：

```bash
git branch --show-current
git status --short

sed -n '1,220p' mem_ut/ver/ut/memblock/tb/top_tb.sv
sed -n '1,260p' mem_ut/ver/ut/memblock/tb/dut_inst.sv
sed -n '1,220p' mem_ut/ver/ut/memblock/tb/memblock_connect.sv
sed -n '1,220p' mem_ut/ver/ut/memblock/tb/tc_if_connect.sv

python3 - <<'PY'
# 提取 MemBlock/MemBlockTop module header 端口集合。
# 提取 dut_inst.sv 中 MemBlock U_MEMBLOCK 的实例端口集合。
# 对比 dut_inst 端口是否存在于 build_memblock/rtl/MemBlock.sv。
# 提取 *_agent_connect.sv 中 RTL_PATH.<signal> 引用。
# 对比引用是否存在于 build_memblock/rtl/MemBlock.sv 文本和 MemBlock 顶层端口。
# 提取 agent/*/src/*_interface.sv 与 *_xaction.sv 字段。
PY

rg -n "l2_tlb_req|l2_pmp_resp|dtlb|DTLB|L2TLB|L2tlb|pmp" \
  build_memblock/rtl/MemBlock.sv \
  build_memblock/rtl/MemBlockTop.sv \
  mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv \
  mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/*.sv
```

## 4. 当前入口展开结果

`top_tb.sv` 当前展开顺序为：

```text
top_tb.sv
  include dut_inst.sv
  include tc_if_connect.sv
  include memblock_connect.sv
  `MEMBLOCK_CONNECT(env, top_tb.U_MEMBLOCK)
```

`memblock_connect.sv` 当前接入 20 个 agent connect：

```text
backendToTopBypass_agent
fence_agent
csr_ctrl_agent
lsqcommit_agent
lsqenq_agent
lintsissue_agent
vecissue_agent
redirect_agent
sbuffer_agent
dcache_agent
int_sink_agent
L2tlb_agent
itlb_agent
prefetch_agent
io_mem_to_ooo_ctrl_agent
io_mem_to_ooo_int_wb_agent
io_mem_to_ooo_vec_wb_agent
io_mem_to_ooo_wakeup_agent
io_mem_to_ooo_iq_feedback_agent
other_ctrl_agent
```

当前 `dut_inst.sv` 实例化的是：

```systemverilog
MemBlock U_MEMBLOCK (...)
```

因此 `dut_inst.sv` 的直接端口一致性应优先对比 `build_memblock/rtl/MemBlock.sv` 中的 `module MemBlock`。`build_memblock/rtl/MemBlockTop.sv` 作为 V2 生成后 wrapper 顶层事实，用于识别 `outer_cpu_halt`、`l2_tlb_req_*` 和 `l2_pmp_resp_*` 这类 V2 顶层暴露信号。

## 5. 实际扫描结果摘要

### 5.1 RTL 端口规模

| 项目 | 数量 | 说明 |
|---|---:|---|
| `MemBlock` 端口 | 6751 | `dut_inst.sv` 当前实例化对象 |
| `MemBlockTop` 端口 | 755 | V2 生成后 wrapper 顶层 |
| `dut_inst.sv` 实例连接端口 | 1393 | `MemBlock U_MEMBLOCK` 中 `.port(signal)` 连接 |
| `dut_inst.sv` 中 V2 `MemBlock` 不存在端口 | 462 | 需要宏隔离或删除旧版本连接 |
| V2 `MemBlock` 未被 `dut_inst.sv` 连接端口 | 5820 | 当前 UVM 只覆盖 memblock 相关子集，不能直接作为错误处理 |

### 5.2 `dut_inst.sv` 中 V2 RTL 不存在端口类别

| 类别 | 数量 | 重点差异种子 |
|---|---:|---|
| issue 字段集合 | 253 | `io_ooo_to_mem_intIssue_*`、`io_ooo_to_mem_vecIssue_*` 整族在 V2 `MemBlock` 顶层不存在 |
| writeback 字段集合 | 140 | `io_mem_to_ooo_intWriteback_*`、`io_mem_to_ooo_vecWriteback_*` 整族在 V2 `MemBlock` 顶层不存在 |
| LSQ enqueue/commit | 31 | V3 风格 8 路 `enqLsq` 中 `_6/_7` 在 V2 不存在，V2 当前为 6 路 |
| redirect/control | 25 | `updateLFST_*`、部分 `memoryViolation/mdpTrain` 字段在 V2 不存在或字段集合不同 |
| BPU/CSR ctrl | 8 | `bp_ctrl_ubtbEnable/abtbEnable/mbtbEnable/tageEnable/scEnable/ittageEnable`，以及 writeback trigger 字段差异 |
| CPU halt/WFI/backend bypass | 2 | `cpuWfi`、`io_outer_cpu_wfi` 与 V2 `cpuHalted/io_outer_cpu_halt/outer_cpu_halt` 命名不一致 |
| other | 3 | `auto_inner_l3_pf_sender_out_*` 预取相关字段 |

重点差异种子必须在后续 coding 中优先处理：

- CPU halt：旧 testbench 使用 `cpuWfi/io_outer_cpu_wfi`，V2 `MemBlock` 可见 `io_ooo_to_mem_backendToTopBypass_cpuHalted`、`io_outer_cpu_halt`，V2 `MemBlockTop` 可见 `outer_cpu_halt`。
- BPU ctrl：旧 `csr_ctrl_agent` 仍连接 `bp_ctrl_*Enable`，当前 V2 `MemBlock` 顶层未暴露这些字段。
- LSQ enqueue：旧环境按 8 路 `enqLsq` 生成和连接，当前 V2 `MemBlock` 只存在 `_0` 到 `_5`。
- issue/writeback：旧 `intIssue/vecIssue/intWriteback/vecWriteback` 大量顶层端口在当前 V2 `MemBlock` 不存在，应按版本宏整体隔离。
- ctrl 字段集合：`updateLFST_*`、`mdpTrain_*`、部分 `memoryViolation` 字段缺失。
- L2TLB/PMP：V2 `MemBlockTop` 暴露 `l2_tlb_req_*` 和 `l2_pmp_resp_*`，现有 `L2tlb_agent_connect.sv` 仍基于内部 `_inner_dtlbRepeater_*` 与 `_inner_ptw_io_tlb_1_*`。

### 5.3 共同端口位宽不一致

`dut_inst.sv` 与 V2 `MemBlock` 共同存在端口中，扫描到 33 处声明位宽不一致：

| 位宽差异组 | 数量 | V2 RTL | 当前 TB 声明 |
|---|---:|---|---|
| `robIdx` | 20 | `[7:0]` | `[8:0]` |
| `fuType` | 6 | `[34:0]` | `[35:0]` |
| trace `iretire` | 3 | `[6:0]` | `[7:0]` |
| `pendingPtr` | 1 | `[7:0]` | `[8:0]` |
| `ftqOffset` | 1 | `[3:0]` | `[4:0]` |
| `auto_inner_frontendBridge_icachectrl_out_a_bits_address` | 1 | `[47:0]` | `[29:0]` |
| 其他 | 1 | 需逐字段复核 | 需逐字段复核 |

这些差异不应通过散落的硬编码位宽修复，应统一转为 V2/V3 参数宏，例如 `MEMBLOCK_ROB_IDX_W`、`MEMBLOCK_FU_TYPE_W`、`MEMBLOCK_FTQ_OFFSET_W`、`MEMBLOCK_LSQ_ENQ_CHANNELS`。

### 5.4 connect 中 V2 RTL 不存在引用的 agent/数量

本节统计 `*_agent_connect.sv` 中 `RTL_PATH.<signal>` 引用。`missing_in_MemBlock_text` 表示该唯一引用在当前 `build_memblock/rtl/MemBlock.sv` 文本中完全找不到；这类引用通常会直接导致层级引用错误。`missing_as_port` 表示该引用不是 `MemBlock` 顶层端口；部分内部信号若真实存在，可以继续作为内部层级接管点，但必须受版本宏保护。

| agent connect | 唯一 `RTL_PATH.*` 引用 | `MemBlock.sv` 文本不存在 | 非 `MemBlock` 顶层端口 |
|---|---:|---:|---:|
| `backendToTopBypass_agent` | 3 | 1 | 1 |
| `csr_ctrl_agent` | 94 | 6 | 6 |
| `io_mem_to_ooo_ctrl_agent` | 94 | 26 | 26 |
| `io_mem_to_ooo_int_wb_agent` | 145 | 145 | 145 |
| `io_mem_to_ooo_vec_wb_agent` | 142 | 142 | 142 |
| `lintsissue_agent` | 126 | 126 | 126 |
| `lsqenq_agent` | 121 | 30 | 30 |
| `other_ctrl_agent` | 20 | 1 | 1 |
| `prefetch_agent` | 13 | 3 | 3 |
| `vecissue_agent` | 127 | 127 | 127 |
| 合计 | 885 | 607 | 607 |

其余 agent 当前 `RTL_PATH.*` 引用在 `MemBlock.sv` 文本中存在：

```text
L2tlb_agent: 61 个引用在 MemBlock.sv 文本存在，但均不是 MemBlock 顶层端口。
dcache_agent/fence_agent/int_sink_agent/io_mem_to_ooo_iq_feedback_agent/
io_mem_to_ooo_wakeup_agent/itlb_agent/lsqcommit_agent/redirect_agent/sbuffer_agent:
未发现文本不存在引用。
```

`L2tlb_agent` 必须单独处理：现有 connect 使用内部 `_inner_dtlbRepeater_io_ptw_req_0_*` 和 `_inner_ptw_io_tlb_1_*`；V2 profile 指出生成后 `MemBlockTop.sv` 已暴露 `l2_tlb_req_*` request/response 端口和 `l2_pmp_resp_*` 端口。后续不得把 L2TLB agent 写成 L2Cache/PTW/memory 下游模型，必须继续按 DTLB -> L2TLB request、L2TLB -> DTLB response 的 responder 语义适配。

### 5.5 agent 与 subagent 字段扫描

当前 `agent` 目录包含 20 个 agent 目录；未发现 `subagent` 目录。

重点命中如下：

| agent | interface/xaction 字段声明数 | 命中差异种子 |
|---|---:|---|
| `backendToTopBypass_agent_agent` | 11 | `cpuWfi` |
| `csr_ctrl_agent_agent` | 193 | `bp_ctrl` |
| `lsqenq_agent_agent` | 249 | `_6/_7` 通道、`robIdx`、`fuType` |
| `lintsissue_agent_agent` | 261 | `intIssue`、`robIdx`、`fuType`、`ftqOffset` |
| `vecissue_agent_agent` | 259 | `vecIssue`、`robIdx`、`fuType`、`ftqOffset` |
| `io_mem_to_ooo_int_wb_agent_agent` | 313 | `intWriteback`、`robIdx` |
| `io_mem_to_ooo_vec_wb_agent_agent` | 289 | `vecWriteback`、`robIdx` |
| `io_mem_to_ooo_ctrl_agent_agent` | 193 | `robIdx`、`ftqOffset` |
| `io_mem_to_ooo_iq_feedback_agent_agent` | 121 | `robIdx` |
| `redirect_agent_agent` | 13 | `robIdx` |
| `L2tlb_agent_agent` | 127 | `io_ptw_req`、`io_ptw_resp` |

结论：后续不能只改 `dut_inst.sv` 或 connect。只要某个端口字段被版本宏隔离，必须同步检查对应 agent 的 interface、xaction、driver、monitor，避免 V2 下保留不可达字段或 V3 下误删有效字段。

## 6. 宏隔离总体策略

### 6.1 版本宏唯一入口

新增或整理编译期版本宏时，统一放在：

```text
mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh
```

建议定义显式版本选择检查：

```systemverilog
`ifdef MEMBLOCK_DUT_V2
  `ifdef MEMBLOCK_DUT_V3
    `error "MEMBLOCK_DUT_V2 and MEMBLOCK_DUT_V3 are mutually exclusive"
  `endif
`endif

`ifndef MEMBLOCK_DUT_V2
  `ifndef MEMBLOCK_DUT_V3
    `error "one of MEMBLOCK_DUT_V2 or MEMBLOCK_DUT_V3 must be defined"
  `endif
`endif
```

后续 coding 时必须保证 V2/V3 显式选择且互斥：同时定义时报错，同时未定义也报错。禁止在 connect 文件中解析 runtime plusarg 来改变版本。

### 6.2 参数宏统一入口

位宽、channel 数、idx 范围必须由同一套版本宏派生，禁止在各 agent 文件中散落写 V2/V3 数字。

建议参数值：

```systemverilog
`ifdef MEMBLOCK_DUT_V2
  `define MEMBLOCK_ROB_IDX_W       8
  `define MEMBLOCK_FU_TYPE_W       35
  `define MEMBLOCK_FTQ_OFFSET_W    4
  `define MEMBLOCK_LSQ_ENQ_CHANNELS 6
`elsif defined(MEMBLOCK_DUT_V3)
  `define MEMBLOCK_ROB_IDX_W       9
  `define MEMBLOCK_FU_TYPE_W       36
  `define MEMBLOCK_FTQ_OFFSET_W    5
  `define MEMBLOCK_LSQ_ENQ_CHANNELS 8
`else
  `error "MEMBLOCK_DUT_V2 or MEMBLOCK_DUT_V3 must be defined"
`endif
```

实际 coding 前必须从当前 V3 profile 或 V3 Verilog 重新确认 V3 默认值，不能只按本次 V2 扫描反推。

建议追加字段级参数宏：

```systemverilog
`define MEMBLOCK_LQ_IDX_W          7
`define MEMBLOCK_SQ_IDX_W          6
`define MEMBLOCK_UOP_IDX_W         7
`define MEMBLOCK_TRACE_IRETIRE_W   7
`define MEMBLOCK_PADDR_W           48
`define MEMBLOCK_GPADDR_W          64
```

若 V2/V3 对应字段不同，必须在同一个版本宏区块中覆盖，禁止在 driver/monitor/xaction 中二次推导。

### 6.3 字段族隔离策略

字段族按三类处理：

| 类型 | 处理方式 | 示例 |
|---|---|---|
| 版本独有整族 | 用版本宏包住声明、实例连接、connect、interface、xaction、driver、monitor | `intIssue`、`vecIssue`、`intWriteback`、`vecWriteback` |
| 通道数不同 | 保留同一字段模板，用 channel 参数宏控制展开范围；超出版本通道的字段不声明、不连接、不驱动、不采样 | `enqLsq_req/resp/needAlloc` V2 6 路、V3 8 路 |
| 同名但位宽不同 | 使用参数宏定义位宽，字段名保持一致 | `robIdx`、`fuType`、`ftqOffset`、`pendingPtr` |

## 7. 分模块修改方案

### 7.1 `dut_inst.sv`

目标：

- 保留同一份 `dut_inst.sv`。
- 顶层端口声明和 `MemBlock U_MEMBLOCK` 连接按 V2/V3 宏隔离。
- 同名位宽差异改为参数宏。

文字伪代码：

```text
进入 dut_inst.sv：
  先 include memblock_compile_params.svh，获得版本宏和参数宏。
  对所有共同端口：
    使用 MEMBLOCK_*_W 宏声明位宽。
    保持 `.port(signal)` 连接名不变。
  对 V3 独有端口族：
    放入 `ifdef MEMBLOCK_DUT_V3`。
    V2 编译时不声明、不连接。
  对 V2 命名不同但语义相同字段：
    在 V2 分支连接 V2 端口名。
    在 V3 分支连接 V3 端口名。
  对 V2 `MemBlockTop` wrapper 才暴露的端口：
    先确认当前 testbench 是否改为实例化 `MemBlockTop`。
    如果仍实例化 `MemBlock`，不得直接连接 wrapper 端口。
```

CPU halt 伪代码：

```systemverilog
`ifdef MEMBLOCK_DUT_V2
  wire io_ooo_to_mem_backendToTopBypass_cpuHalted;
  wire io_outer_cpu_halt;
`elsif defined(MEMBLOCK_DUT_V3)
  reg io_ooo_to_mem_backendToTopBypass_cpuWfi;
  wire io_outer_cpu_wfi;
`endif

MemBlock U_MEMBLOCK (
`ifdef MEMBLOCK_DUT_V2
  .io_ooo_to_mem_backendToTopBypass_cpuHalted(io_ooo_to_mem_backendToTopBypass_cpuHalted),
  .io_outer_cpu_halt(io_outer_cpu_halt),
`elsif defined(MEMBLOCK_DUT_V3)
  .io_ooo_to_mem_backendToTopBypass_cpuWfi(io_ooo_to_mem_backendToTopBypass_cpuWfi),
  .io_outer_cpu_wfi(io_outer_cpu_wfi),
`endif
);
```

### 7.2 `memblock_connect.sv` 和 `*_agent_connect.sv`

目标：

- `MEMBLOCK_CONNECT` 保持单一入口。
- 每个 agent connect 内部负责版本字段隔离，不在 `MEMBLOCK_CONNECT` 中复制两套 agent 列表。
- 整族缺失的 agent connect 在 V2 下可以保留 interface 实例和 uvm_config_db，但不引用不存在的 `RTL_PATH.*`。

文字伪代码：

```text
进入 MEMBLOCK_CONNECT：
  逐个调用已有 agent connect 宏。
  agent connect 宏内部读取版本宏。

进入单个 *_agent_connect.sv：
  先实例化 interface 并 set virtual interface。
  对公共字段：
    按现有方向 force。
  对 V2 不存在字段：
    `ifdef MEMBLOCK_DUT_V3` 包住 force。
    V2 分支不生成该 force。
  对 V2/V3 名称不同字段：
    V2 分支 force V2 信号。
    V3 分支 force V3 信号。
  对通道数组：
    只展开 0 到 MEMBLOCK_*_CHANNELS-1。
    若当前手写文件无法循环生成，至少用宏块隔离 `_6/_7`。
```

LSQ enqueue 伪代码：

```systemverilog
// 0..5 为 V2/V3 公共通道，使用参数宏位宽。
force U_IF_NAME.io_ooo_to_mem_enqLsq_req_0_bits_fuType =
      RTL_PATH.io_ooo_to_mem_enqLsq_req_0_bits_fuType;

`ifdef MEMBLOCK_DUT_V3
  force U_IF_NAME.io_ooo_to_mem_enqLsq_needAlloc_6 =
        RTL_PATH.io_ooo_to_mem_enqLsq_needAlloc_6;
  force U_IF_NAME.io_ooo_to_mem_enqLsq_req_6_valid =
        RTL_PATH.io_ooo_to_mem_enqLsq_req_6_valid;
  force U_IF_NAME.io_ooo_to_mem_enqLsq_resp_6_lqIdx_value =
        RTL_PATH.io_ooo_to_mem_enqLsq_resp_6_lqIdx_value;
`endif
```

issue/writeback 伪代码：

```systemverilog
`ifdef MEMBLOCK_DUT_V3
  `MEMBLOCK__LINTSISSUE_AGENT_FORCE_FIELDS(U_IF_NAME, RTL_PATH)
  `MEMBLOCK__VECISSUE_AGENT_FORCE_FIELDS(U_IF_NAME, RTL_PATH)
  `MEMBLOCK__IO_MEM_TO_OOO_INT_WB_AGENT_FORCE_FIELDS(U_IF_NAME, RTL_PATH)
  `MEMBLOCK__IO_MEM_TO_OOO_VEC_WB_AGENT_FORCE_FIELDS(U_IF_NAME, RTL_PATH)
`elsif defined(MEMBLOCK_DUT_V2)
  // V2 当前 MemBlock 顶层无该整族端口。
  // 保留 vif set，禁止生成任何不存在 RTL_PATH.* force。
`endif
```

### 7.3 agent interface/xaction/driver/monitor

目标：

- 字段声明、transaction 字段、driver reset/idle/send、monitor sample 必须和 connect 宏同版本。
- 不保留 V2 不可达字段的驱动或采样路径。
- 不改变运行期调度和状态生命周期。

文字伪代码：

```text
进入 interface：
  公共字段用 MEMBLOCK_*_W 宏声明位宽。
  V3 独有字段用 MEMBLOCK_DUT_V3 包住。
  V2 独有字段用 MEMBLOCK_DUT_V2 包住。
  clocking block 与字段声明使用同一宏条件。

进入 xaction：
  字段声明与 interface 条件一致。
  uvm_field_int、constraint、psdisplay、compare 与字段声明使用同一宏条件。
  位宽统一使用 MEMBLOCK_*_W 宏。

进入 driver：
  reset_phase/drive_idle/send_pkt 只访问当前版本已声明字段。
  V2 分支不驱动 issue/writeback 整族不存在字段。

进入 monitor：
  mon_data 只采样当前版本已声明字段。
  若 V2 下某 agent 暂无 DUT 接口字段，monitor 不写半事务到 analysis port。
```

## 8. L2TLB/PMP 专项适配策略

L2TLB 必须遵守现有规则：`L2TLB_agent` 建模的是 DTLB -> L2TLB request 和 L2TLB -> DTLB response，不是 L2TLB 到 L2Cache/PTW/memory 的下游模型。

当前扫描事实：

- `MemBlockTop.sv` 暴露 `l2_tlb_req_req_*`、`l2_tlb_req_resp_*` 和 `l2_pmp_resp_*`。
- `L2tlb_agent_connect.sv` 当前 61 个 `RTL_PATH.*` 引用均不是 `MemBlock` 顶层端口，但在 `MemBlock.sv` 文本中存在。
- 当前 `L2tlb_agent_agent_interface.sv` 和 `L2tlb_agent_agent_xaction.sv` 使用 `io_ptw_req_0_*`、`io_ptw_resp_*` 字段，字段集合偏 PTW/TLB response 模板。

后续 V2 适配应拆成两个宏分支：

```systemverilog
`ifdef MEMBLOCK_DUT_V2
  // 优先使用 V2 MemBlockTop 暴露的 l2_tlb_req_* / l2_pmp_resp_* 语义。
  // 如果当前仍实例化 MemBlock，则先确认是否切换到 MemBlockTop，
  // 或是否继续使用内部 DTLB/L2TLB 层级接管点。
`elsif defined(MEMBLOCK_DUT_V3)
  // 保留当前已验证的 V3 DTLB/L2TLB 接管方式。
`endif
```

L2TLB V2 字段映射原则：

| 方向 | V2 顶层候选 | agent 语义 |
|---|---|---|
| request | `l2_tlb_req_req_valid/ready/bits_*` | DTLB -> L2TLB_agent request 采样 |
| response | `l2_tlb_req_resp_valid/ready/bits_*` | L2TLB_agent -> DTLB response 驱动或观察 |
| PMP | `l2_pmp_resp_ld/st/instr/mmio/atomic` | 作为 V2 L2TLB/PMP 结果字段，不得误写为 L2Cache 下游请求 |

L2TLB 方案边界：

- 本 plan 不修改 `memblock_l2tlb_base_sequence.sv`。
- 本 plan 不修改 TLB lookup API、CSR runtime snapshot 或 common data 表。
- 如后续 V2 字段映射需要改变 response transaction 结构，只能同步 interface/xaction/driver/monitor 字段，不改变 responder sequence 主流程。
- 不得根据 `paddr` 请求去查 L2TLB 表；lookup 仍以 DTLB request 的 `vpn/s2xlate` 和 runtime CSR 的 `asid/vmid` 为语义来源。

## 9. 修改批次

### 批次 1：编译期版本与参数宏

修改范围：

- `mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh`

任务：

1. 定义 V2/V3 互斥版本宏。
2. 定义 `MEMBLOCK_ROB_IDX_W`、`MEMBLOCK_FU_TYPE_W`、`MEMBLOCK_FTQ_OFFSET_W`、`MEMBLOCK_LSQ_ENQ_CHANNELS` 等参数宏。
3. 补齐默认行为说明和覆盖方式。

验收：

- 宏互斥检查生效。
- V2 默认值来自当前 V2 Verilog 扫描。
- V3 默认值经 V3 profile 或 V3 Verilog 复核后再落地。

### 批次 2：`dut_inst.sv` 顶层连接隔离

修改范围：

- `mem_ut/ver/ut/memblock/tb/dut_inst.sv`

任务：

1. 用参数宏替换同名字段位宽硬编码。
2. 用版本宏隔离 V2 不存在的 462 个旧端口。
3. 修复 CPU halt/WFI 命名差异。
4. 对 `MemBlockTop` 才暴露的 V2 端口，先决定实例化层级，再连接。

验收：

- V2 编译预处理后不出现当前 V2 `MemBlock` 不存在的端口连接。
- V3 分支保留 V3 已有连接语义。

### 批次 3：connect 宏隔离

修改范围：

- `mem_ut/ver/ut/memblock/tb/memblock_connect.sv`
- `mem_ut/ver/ut/memblock/tb/*_agent_connect.sv`

任务：

1. 对 607 个 V2 `MemBlock.sv` 文本不存在引用按 agent 分批隔离。
2. 对 issue/writeback 整族在 V2 下禁止生成 `RTL_PATH.*` force。
3. 对 LSQ enqueue `_6/_7` 用 V3 宏保护。
4. 对 L2TLB 保持 responder 语义，单独确认 V2 `l2_tlb_req/l2_pmp_resp` 映射。

验收：

- `rg` 检查 V2 分支下不再展开不存在的 `RTL_PATH.*`。
- `MEMBLOCK_CONNECT` 仍为单一入口。

### 批次 4：agent 字段参数化与版本隔离

修改范围：

- `mem_ut/ver/ut/memblock/agent/**/src/*_interface.sv`
- `mem_ut/ver/ut/memblock/agent/**/src/*_xaction.sv`
- `mem_ut/ver/ut/memblock/agent/**/src/*_driver.sv`
- `mem_ut/ver/ut/memblock/agent/**/src/*_monitor.sv`

任务：

1. 对 `robIdx/fuType/ftqOffset/pendingPtr/trace iretire` 使用参数宏。
2. 对 issue/writeback 整族字段加 V3 宏。
3. 对 LSQ enqueue channel `_6/_7` 加 V3 宏。
4. L2TLB agent 字段按 V2/V3 connect 分支同步。

验收：

- interface、xaction、driver、monitor 字段集合一致。
- V2 下无不可达字段驱动、采样和 compare/psdisplay 残留。

### 批次 5：静态验证与远端 smoke

本 plan 只生成方案，不执行 coding。后续 coding 完成后至少执行：

```bash
git diff --check -- mem_ut/ver/ut/memblock AI_DOC

python3 <接口扫描脚本>

rg -n "io_ooo_to_mem_intIssue|io_ooo_to_mem_vecIssue|io_mem_to_ooo_intWriteback|io_mem_to_ooo_vecWriteback|io_ooo_to_mem_enqLsq_req_6|io_ooo_to_mem_enqLsq_req_7|cpuWfi|io_outer_cpu_wfi" \
  mem_ut/ver/ut/memblock/tb \
  mem_ut/ver/ut/memblock/agent

rg -n "L2TLB|L2tlb|l2tlb|DTLB|dtlb|l2_tlb_req|l2_pmp_resp" \
  mem_ut/ver/ut/memblock AI_DOC mem_ut/ver/ut/memblock/rule
```

如涉及可编译代码变更，按远端 flow 验证：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

通过标准：

- 编译无 DUT 端口不存在、层级路径不存在、interface 字段不存在、transaction 字段不存在错误。
- 仿真看到 `TEST CASE PASSED`。
- `UVM_ERROR` 和 `UVM_FATAL` 均为 0。

## 10. 不修改运行期逻辑的明确边界

本 plan 不修改以下内容：

- sequence 主循环。
- virtual sequence/scenario sequence/responder sequence 调度。
- monitor service loop 和 raw queue 消费策略。
- handler、adapter、scheduler。
- common data transaction、TLB 表、CSR runtime snapshot。
- RM、scoreboard、checker、coverage。
- testcase 激励参数和 plus cfg。

若后续接口字段变化导致某 sequence 或状态表需要新增语义字段，应另起测试框架运行期逻辑 plan，不能混入本接口适配 plan。

## 11. 风险与待确认项

1. 当前 `dut_inst.sv` 实例化 `MemBlock`，但 V2 profile 和 `MemBlockTop.sv` 中的 `l2_tlb_req_*`、`l2_pmp_resp_*`、`outer_cpu_halt` 位于 `MemBlockTop`。后续必须先决定是否将 testbench 实例化对象切到 `MemBlockTop`，或继续针对 `MemBlock` 内部层级接管。
2. V3 参数默认值需要从 V3 profile 或 V3 Verilog 重新确认，不能只按当前 V2 差异反推。
3. issue/writeback 整族在 V2 顶层不存在，但 agent 内仍保留大量字段；若只隔离 connect 而不隔离 interface/xaction/driver/monitor，容易留下不可达事务或编译残留。
4. L2TLB 现有字段名仍偏 `io_ptw_req/io_ptw_resp`，V2 顶层为 `l2_tlb_req/l2_pmp_resp`。字段映射必须按 DTLB/L2TLB responder 语义重审，不能按名称直接替换。
5. 当前工作区已有其他修改和未跟踪 plan；后续 coding 时必须只 stage 目标 `mem_ut/**` 和对应文档，避免混入无关变更。

## 12. 与本次扫描结论的对齐

本方案覆盖本次扫描发现的核心问题：

- `dut_inst.sv` 中 462 个 V2 `MemBlock` 不存在端口，按版本宏隔离。
- connect 中 10 个 agent 合计 607 个 V2 `MemBlock.sv` 文本不存在引用，按 agent 分批隔离。
- LSQ enqueue 6/8 路差异，按 `MEMBLOCK_LSQ_ENQ_CHANNELS` 参数宏处理。
- `robIdx/fuType/ftqOffset/pendingPtr/trace iretire` 位宽差异，按统一参数宏处理。
- CPU halt/WFI、BPU ctrl、LSQ enqueue、L2TLB/PMP、issue/writeback/ctrl 字段集合差异，均作为后续 coding 的优先批次。
- 明确本 plan 不修改测试框架运行期逻辑。
