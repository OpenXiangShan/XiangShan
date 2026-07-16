# mem_ut V2 Monitor Output 测试框架适配 Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| 关联执行 plan | `mem_ut_v2_monitor_output_framework_adapt_execution_plan_20260708.md` |
| 适配原则 | 区分 runtime raw queue 与 UVM analysis transaction，不批量恢复 producer，不误接 V2-only output |
| 创建/修订日期 | 2026-07-15 |

## 1. 范围与边界

本 plan 只整理 V2 monitor output 适配时需要解决的问题。每个问题均说明 V2 问题、修改原因、最终方案、修改的逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- 顶层 agent monitor 的 runtime 职责分类。
- 已连接但 producer 未发送的 UVM analysis port 状态确认。
- V2-only 顶层 output 的主功能影响分类。
- `io_l2_tlb_req_resp_*` 与内部 `L2TLB_agent` 接管点的语义隔离。

本轮不支持：

- 批量恢复 `mon_item_port.write(mon_tr)`。
- 删除现有 `memblock_env` analysis FIFO 或 `memblock_rm` blocking consumer。
- 实现 RM、scoreboard、checker 或 coverage。
- 把 V2 顶层 L2/L2Cache 侧 TLB/PMP response 接到内部 `L2TLB_agent`。

## 2. 问题一：Analysis consumer 已连接但 monitor producer 未发送

### V2 问题

当前多个顶层 agent 在 `memblock_env.sv` 中已经建立：

```text
monitor.mon_item_port -> uvm_tlm_analysis_fifo -> memblock_rm blocking_get
```

但 monitor 内部 `mon_tr` 构造和 `mon_item_port.write(mon_tr)` 多数仍处于注释或未执行状态。旧 plan 容易把这些端口描述成“无 consumer”或“只做 X/Z”，这会掩盖 RM consumer 已经永久等待的事实。

### 修改原因

runtime raw queue 与 UVM analysis transaction 是两条不同链路：

- raw queue 服务 dispatch 测试框架公共状态推进。
- analysis transaction 服务 RM/scoreboard/coverage 等标准 UVM 消费者。

不能因为 raw queue 已经覆盖当前主 flow，就宣称 analysis 链路完整；也不能因为 RM consumer 已连接，就在没有字段契约时批量恢复 producer。

### 修改方案与修改逻辑

每个 monitor 同时给出两个分类：

| 分类维度 | 允许值 | 修改逻辑 |
|---|---|---|
| runtime role | `RAW_QUEUE`、`RESPONDER_OBSERVATION`、`DRIVER_OBSERVATION_XZ`、`OUTPUT_OBSERVATION_XZ`、`UNSUPPORTED_VECTOR_OBSERVATION` | 决定当前测试框架主状态是否消费该 monitor |
| RM role | `ANALYSIS_PORT_DEFERRED` | 表示 FIFO/RM consumer 已存在，但本轮不恢复 producer |

本轮不删除 FIFO，不恢复 `mon_item_port.write()`，只把“producer 未发送”写成明确 deferred blocker。后续若 testcase、RM 或 coverage 依赖标准 transaction，必须另立 monitor/RM 专项，一次性确定字段、采样条件、producer 和 consumer。

### 文字伪代码

```text
分类每个顶层 monitor：
  读取 memblock_env.sv 中该 agent 的 analysis FIFO 连接；
  读取 memblock_rm.sv 中对应 blocking_get consumer；
  读取 monitor 是否写 memblock_sync_pkg raw queue；
  如果 monitor 写 raw queue：
    runtime role 标为 RAW_QUEUE；
    检查 raw struct、push 条件和 consumer 是否自洽；
  否则如果 agent 由 driver/sequence 代理下游响应：
    runtime role 标为 RESPONDER_OBSERVATION；
    不把观察字段写入公共状态；
  否则如果该 monitor 只回看 DUT input：
    runtime role 标为 DRIVER_OBSERVATION_XZ；
  否则如果该 monitor 只观察 DUT output：
    runtime role 标为 OUTPUT_OBSERVATION_XZ；
  否则如果属于本轮 scalar flow 不支持的 vector 端口：
    runtime role 标为 UNSUPPORTED_VECTOR_OBSERVATION；
  无论 runtime role 如何：
    RM role 固定标为 ANALYSIS_PORT_DEFERRED；
    记录 env/RM consumer 已连接但 monitor producer 未发送；
    本轮不创建 mon_tr、不调用 unpack()、不调用 mon_item_port.write()；
```

## 3. 问题二：Runtime raw producer 与 monitor 观察字段混用

### V2 问题

当前 dispatch 主 flow 只依赖少数 raw producer，例如 CSR、fence、ctrl、int writeback 和 IQ feedback。其它 monitor 即使采样接口字段，也不等于这些字段已经进入公共状态。

旧 plan 将 “XZ_ONLY / RAW_QUEUE / ANALYSIS_PORT” 混成一个分类，容易把 raw queue producer、driver 回看、responder 观察和 RM analysis 缺口混在一起。

### 修改原因

公共状态只有一个 owner 时，后续 V2 适配才不会重复更新同一语义。例如 redirect、MMIO tag、writeback、IQ feedback、L2TLB response 都已有各自专项 plan；monitor output plan 不应再新增第二套状态来源。

### 修改方案与修改逻辑

只允许以下 raw producer 修改公共状态：

```text
csr_ctrl_agent_agent_monitor
fence_agent_agent_monitor
io_mem_to_ooo_ctrl_agent_agent_monitor
io_mem_to_ooo_int_wb_agent_agent_monitor
io_mem_to_ooo_iq_feedback_agent_agent_monitor
```

其它 monitor 的新增 V2 字段若只用于观察或 X/Z 检查，则保持在 interface/xaction/connect/monitor 同名字段链，不写入 `memblock_sync_pkg`，也不参与 pass/fail/terminal/redirect/replay。

### 文字伪代码

```text
处理 monitor 新增字段：
  如果字段属于已定义 raw producer 的 raw struct：
    在对应专项 plan 中补齐字段来源、default、push 条件和 consumer；
    本 plan 只记录该字段归属，不新增第二条 raw path；
  否则如果字段只用于 X/Z 或接口保真：
    保持 interface、xaction、connect、monitor 局部搬运；
    不写 raw queue；
    不写 runtime CSR snapshot、status table、pass/fail 或 terminal；
  否则如果后续 RM/coverage 需要标准 transaction：
    标为 ANALYSIS_PORT_DEFERRED；
    等 monitor/RM 专项定义完整 mon_tr 字段后再恢复 producer；
```

## 4. 问题三：V2-only 顶层 output 未做主功能分类

### V2 问题

V2 顶层新增或暴露了若干 output 族：

```text
io_l2_tlb_req_resp_*
io_l2_pmp_resp_*
io_outer_l2PfCtrl_*
io_wfi_wfiSafe
io_outer_cpu_halt
io_reset_backend
auto_inner_frontendBridge_icache_out_a_bits_user_needHint
```

这些信号在 `dut_inst.sv` 中可见，但当前没有证据表明它们属于 dispatch scalar 主 flow 的必要观察点。

### 修改原因

顶层 output 不能因为“存在”就接入 agent。误接会带来两个问题：

1. 把与当前主功能无关的观察信号写入 pass/fail 或 terminal。
2. 把 V2 顶层 L2/L2Cache 侧接口误当成内部 `L2TLB_agent` 接管点。

### 修改方案与修改逻辑

本轮按以下分类处理：

| output group | 分类 | 修改逻辑 |
|---|---|---|
| `io_l2_tlb_req_resp_*` | `UNUSED_IN_CURRENT_MAIN_FLOW` | 保留顶层连接，不接内部 `L2TLB_agent` |
| `io_l2_pmp_resp_*` | `UNUSED_IN_CURRENT_MAIN_FLOW` | 不进入 dispatch raw/pass/fail |
| `io_outer_l2PfCtrl_*` | `OUTPUT_OBSERVATION_DEFERRED` | 后续 prefetch control 专项处理 |
| `io_wfi_wfiSafe`、`io_outer_cpu_halt`、`io_reset_backend` | `STATUS_OBSERVATION_ONLY` | 仅作为状态观察，不触发 dispatch stop |
| `auto_inner_frontendBridge_icache_out_a_bits_user_needHint` | `OUTPUT_OBSERVATION_DEFERRED` | 保留连接，不进入 runtime agent |

`io_l2_tlb_req_resp_*` 是顶层 L2/L2Cache 侧 TLB response，不是当前内部 `dtlbRepeater <-> inner_ptw` 接管点；不得接入 `L2TLB_agent`。

### 文字伪代码

```text
处理 V2-only output：
  对每个 output group 检查当前 testcase、公共状态、RM/checker 和 coverage 是否依赖；
  如果不影响当前 dispatch scalar 主 flow：
    保留 dut_inst 顶层连接；
    不新增 agent、raw queue、status 字段或 terminal 条件；
  如果后续专项确认需要观察：
    新建专项 plan；
    明确归属 agent 或新增 monitor；
    补齐 interface、xaction、connect、monitor 和 env/RM 连接；
  如果 output group 是 io_l2_tlb_req_resp_*：
    固定禁止接入内部 L2TLB_agent；
    需要 L2/L2Cache 侧 TLB observation 时另立 agent/interface 方案；
```

## 5. 验收标准

1. 每个顶层 agent monitor 同时具备 runtime role 和 RM role。
2. 文档不再把已连接但未发送的 analysis port 描述为“无 consumer”。
3. 本轮不批量恢复 `mon_item_port.write()`，也不删除 FIFO/RM consumer。
4. `io_l2_tlb_req_resp_*` 不被误接到内部 `L2TLB_agent`。
5. V2-only output 若不影响当前主功能，不新增 raw/status/pass/fail/terminal 逻辑。
6. 通过 `git diff --check -- AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_monitor_output_framework_adapt_plan_20260708.md`。
