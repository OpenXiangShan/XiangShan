# mem_ut V2 Monitor Output 测试框架适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 计划类型 | monitor output 分类、raw queue 边界和 V2-only output 职责收敛 |
| 创建/修订日期 | 2026-07-15 |

## 1. 范围与边界

本 plan 只整理 V2 monitor output 适配时需要解决的问题。每个问题均说明 V2 问题、修改原因、最终方案、修改的逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- 复核 20 个顶层 agent monitor 的 runtime role 和 RM analysis role。
- 保留 5 条现有 raw queue 主路径：CSR、sfence、ctrl、int writeback、IQ feedback。
- 明确所有 20 条 env/RM analysis FIFO consumer 已连接，但 monitor producer 当前 deferred。
- 明确 V2-only output、字段级 output 和 DCache/SBuffer/L2TLB 职责边界。
- 对 IQ feedback 的 V2 STA SQ-only raw 与 VSTU scalar-only fatal 边界给出 coding 落点。

本轮不支持：

- 不批量恢复 `mon_item_port.write(mon_tr)`。
- 不删除 env/RM analysis FIFO、RM port 或 blocking get loop。
- 不实现 RM、checker、scoreboard 或 coverage。
- 不新增 V2-only output agent。
- 不把内部 `L2TLB_agent` 改接成顶层 L2/L2Cache/PTW/PMP output monitor。
- 不实现 DCache `io_l2_hint_*` 和 `io_l2_flush_done` 驱动；该输入 sideband 由 DCache L2 sideband 专项唯一实现。

本 plan 只修改分类、职责边界和少量 monitor raw 生产合同，不改变 pass/fail/terminal、commit cursor、redirect/replay 仲裁或公共 batch 主状态机。

## 2. 问题一：monitor 分类把 runtime raw 和 RM analysis 混在一起

### V2 问题

当前 20 个 agent 在 `memblock_env` 中均建立了 `mon_item_port -> uvm_tlm_analysis_fifo -> memblock_rm blocking_get` 链路，`memblock_rm` 也启动了对应 blocking get 线程。但 20 个 monitor 当前均未实际执行 `mon_item_port.write(mon_tr)`。

旧 plan 容易把这些端口描述为“无 consumer”或“debug-only XZ”。这会掩盖真实状态：consumer 已存在，producer deferred。

### 修改原因

dispatch runtime 使用 `memblock_sync_pkg` raw queue 推进公共状态；RM analysis FIFO 是另一条标准 transaction 链。两者不能互相替代。若把 raw queue 闭环误写成 RM transaction 闭环，后续 testcase 或 checker 可能错误依赖一个实际不会收到 transaction 的 RM port。

### 修改方案与修改逻辑

采用二维分类：

| 维度 | 分类 | 本轮含义 |
|---|---|---|
| runtime role | `RAW_QUEUE` | monitor 直接向 `memblock_sync_pkg` raw queue 入队，dispatch runtime 当前消费 |
| runtime role | `RESPONDER_OBSERVATION` | agent 主要由 driver/sequence 代理下游响应，monitor 只观察握手和 X/Z |
| runtime role | `DRIVER_OBSERVATION_XZ` | DUT input 由 agent driver 驱动，monitor 只回看和 X/Z |
| runtime role | `OUTPUT_OBSERVATION_XZ` | DUT output 当前只观察和 X/Z，不进入主状态 |
| runtime role | `UNSUPPORTED_VECTOR_OBSERVATION` | vector 端口存在，但 scalar flow 不支持，valid 时 fatal 或专项处理 |
| RM role | `ANALYSIS_PORT_DEFERRED` | env/RM consumer 已连接，但 monitor producer 当前不 write |

20 个 agent 的 RM role 本轮统一为 `ANALYSIS_PORT_DEFERRED`。后续若恢复某个 analysis producer，必须由 monitor/RM 专项成对确认 transaction 字段、采样条件、RM pack 逻辑和 consumer 需求。

### 文字伪代码

```text
classify_monitor_roles()：
  枚举 mem_ut/ver/ut/memblock/agent 下全部 20 个 monitor；
  对每个 monitor 检查是否调用 push_raw/push_raw_csr/push_raw_sfence；
  检查对应 agent 是 responder、DUT input driver、DUT output observer 还是 unsupported vector observer；
  写入 runtime role；

  检查 memblock_env 中该 agent 的 analysis FIFO 和 analysis_export 连接；
  检查 memblock_rm 中该 agent 的 blocking_get port 和 get loop；
  检查 monitor 是否实际调用 mon_item_port.write；
  如果 consumer 存在但 producer 不 write，则 RM role 固定为 ANALYSIS_PORT_DEFERRED；

  raw queue 可作为 dispatch runtime 输入；
  analysis FIFO 不得被 raw queue 的存在视为已闭环；
  发现 RM transaction 需求时只登记后续专项，不在本 plan 顺手恢复 producer。
```

## 3. 问题二：20 个 agent 的 runtime 职责需要完整落表

### V2 问题

原文存在示例式分类和跨专项描述，但 coding/review 需要能逐项判断每个 monitor 的职责、当前 runtime consumer 和本轮是否修改。只抽查 5 个 raw producer 会遗漏纯 output、driver observation、responder 和 unsupported vector 边界。

### 修改原因

monitor output 适配的目标不是新增所有 output consumer，而是防止错误接入公共状态。完整表可以避免后续 coding 时把未支持的 output、vector activity 或 responder observation 误写入 raw/status/pass/fail/terminal。

### 修改方案与修改逻辑

20 个 agent 的本轮结论如下：

| agent/monitor | Runtime 角色 | 当前 runtime 路径 | RM 角色 | 本轮 coding 结论 |
|---|---|---|---|---|
| `backendToTopBypass_agent_agent` | `DRIVER_OBSERVATION_XZ` | 回看 backend bypass input | `ANALYSIS_PORT_DEFERRED` | 无 runtime 修改 |
| `csr_ctrl_agent_agent` | `RAW_QUEUE` | CSR raw -> runtime CSR snapshot | `ANALYSIS_PORT_DEFERRED` | CSR 字段缺口由 CSR 专项修复 |
| `dcache_agent_agent` | `RESPONDER_OBSERVATION` | DCache TL responder/XZ | `ANALYSIS_PORT_DEFERRED` | TL responder 保持；L2 sideband input 由 DCache 专项 |
| `fence_agent_agent` | `RAW_QUEUE` | sfence raw -> TLB invalidation | `ANALYSIS_PORT_DEFERRED` | 保持 raw；`flushPipe` 边界由 CSR 专项 |
| `int_sink_agent_agent` | `RESPONDER_OBSERVATION` | integer sink 握手/XZ | `ANALYSIS_PORT_DEFERRED` | 无 runtime 修改 |
| `io_mem_to_ooo_ctrl_agent_agent` | `RAW_QUEUE`；字段级 `OUTPUT_OBSERVATION_XZ` | ctrl raw；`externalInterrupt_debug` 仅字段观察 | `ANALYSIS_PORT_DEFERRED` | MMIO/tag/raw 字段由 pending-MMIO 专项；`externalInterrupt_debug` 不入 raw |
| `io_mem_to_ooo_int_wb_agent_agent` | `RAW_QUEUE` | split LDA/STA/STD raw writeback | `ANALYSIS_PORT_DEFERRED` | 由 int-WB 专项修复 |
| `io_mem_to_ooo_iq_feedback_agent_agent` | `RAW_QUEUE` | STA IQ feedback/replay raw | `ANALYSIS_PORT_DEFERRED` | STA SQ-only raw 与 VSTU fatal |
| `io_mem_to_ooo_vec_wb_agent_agent` | `UNSUPPORTED_VECTOR_OBSERVATION` | vector writeback 只做 unsupported gate | `ANALYSIS_PORT_DEFERRED` | scalar testcase 中 `writebackVldu` valid fatal |
| `io_mem_to_ooo_wakeup_agent_agent` | `OUTPUT_OBSERVATION_XZ` | wakeup output 仅 X/Z | `ANALYSIS_PORT_DEFERRED` | 不进入 dispatch 主状态 |
| `itlb_agent_agent` | `RESPONDER_OBSERVATION` | ITLB request/response observation | `ANALYSIS_PORT_DEFERRED` | 不纳入 MemBlock scalar dispatch 状态 |
| `L2tlb_agent_agent` | `RESPONDER_OBSERVATION` | DTLB -> L2TLB request、L2TLB -> DTLB response | `ANALYSIS_PORT_DEFERRED` | 保持内部 DTLB/L2TLB 接管点 |
| `lintsissue_agent_agent` | `DRIVER_OBSERVATION_XZ` | scalar split issue input 回看 | `ANALYSIS_PORT_DEFERRED` | 驱动字段由 split issue 专项 |
| `lsqcommit_agent_agent` | `DRIVER_OBSERVATION_XZ` | ROB->LSQ sideband/flushSb input 回看 | `ANALYSIS_PORT_DEFERRED` | 驱动逻辑由 LSQ MMIO/status 专项 |
| `lsqenq_agent_agent` | `DRIVER_OBSERVATION_XZ` | LSQ enqueue input 回看 | `ANALYSIS_PORT_DEFERRED` | 驱动字段和单拍确认由 enqueue 专项 |
| `other_ctrl_agent_agent` | `OUTPUT_OBSERVATION_XZ` | halt/reset/error status X/Z | `ANALYSIS_PORT_DEFERRED` | 不进入 pass/fail/terminal |
| `prefetch_agent_agent` | `OUTPUT_OBSERVATION_XZ` | L2/L3/ifetch prefetch observation | `ANALYSIS_PORT_DEFERRED` | `io_outer_l2PfCtrl_*` 后续专项 |
| `redirect_agent_agent` | `DRIVER_OBSERVATION_XZ` | redirect input 回看 | `ANALYSIS_PORT_DEFERRED` | 不新增第二 redirect 状态来源 |
| `sbuffer_agent_agent` | `RESPONDER_OBSERVATION` | SBuffer TL responder/XZ | `ANALYSIS_PORT_DEFERRED` | 不接公共状态 |
| `vecissue_agent_agent` | `UNSUPPORTED_VECTOR_OBSERVATION` | VLD issue input 回看 | `ANALYSIS_PORT_DEFERRED` | scalar testcase 不启动随机 vector sequence，非零 valid fatal |

### 文字伪代码

```text
review_monitor_table()：
  逐行打开 20 个 monitor；
  如果 monitor 写 raw queue，确认 raw factory、入队条件、consumer 和生命周期；
  如果 monitor 只观察 driver/responder/output，确认不写 raw/status/pass/fail/terminal；
  如果 monitor 属于 vector unsupported，确认 scalar reset 完成后 valid 非 0 会 fatal；
  对每个 agent 同时检查 env/RM analysis consumer 已连接且 producer deferred；
  新增或删除 agent 时必须同步更新本表，不能只更新 raw producer 清单。
```

## 4. 问题三：字段级 output 和 V2-only output 容易被错误接入公共状态

### V2 问题

`io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug` 位于 ctrl monitor 所属接口中，但它不是 `dispatch_raw_ctrl_t` 字段。若只按 agent 级别把 ctrl monitor 归为 `RAW_QUEUE`，coding 时可能把该 1 bit debug output 写入 CSR snapshot、redirect、pass/fail 或 terminal。

另外，V2 顶层存在多组当前主 flow 不消费的 output，旧文档容易把它们误接到内部 agent 或 responder。

### 修改原因

同一个 monitor 可以同时包含业务 raw 字段和纯观察 output。职责必须精确到字段级，否则公共状态会出现第二来源或错误来源。V2-only output 当前没有主状态 schema、采样条件和生命周期，不能在分类 plan 中临时发明 consumer。

### 修改方案与修改逻辑

`externalInterrupt_debug` 只保留同名字段链：

```text
interface -> xaction -> connect -> monitor/XZ
```

它不进入：

```text
dispatch_raw_ctrl_t
CSR snapshot
status
pass/fail/terminal
redirect/replay
analysis producer
driver
```

V2-only output 分类如下：

| output group | 当前分类 | 本轮处理 | 不支持边界 |
|---|---|---|---|
| `io_l2_tlb_req_resp_*` | `UNUSED_IN_CURRENT_MAIN_FLOW` | 保留 DUT 顶层连接 | 不接内部 `L2TLB_agent` |
| `io_l2_pmp_resp_*` | `UNUSED_IN_CURRENT_MAIN_FLOW` | 不进入 dispatch raw/pass/fail | 后续 PMP observation/RM 专项 |
| `io_outer_l2PfCtrl_*` | `OUTPUT_OBSERVATION_DEFERRED` | 当前不新增 agent | 后续 prefetch control 专项 |
| `io_wfi_wfiSafe` | `STATUS_OBSERVATION_ONLY` | 不作为通过或 terminal 条件 | 后续低功耗专项 |
| `io_outer_cpu_halt` | `STATUS_OBSERVATION_ONLY` | V2 halt status，不能称为旧 `cpuWfi` | 不触发 dispatch stop |
| `io_reset_backend` | `STATUS_OBSERVATION_ONLY` | 与 TB `reset_backend_done` 分离 | 不替代 driver gate |
| `auto_inner_frontendBridge_icache_out_a_bits_user_needHint` | `OUTPUT_OBSERVATION_DEFERRED` | 保留现有连接，不归属 20-agent 主表 | 后续 bus/TL user schema 专项 |

### 文字伪代码

```text
classify_field_level_outputs()：
  对 ctrl monitor：
    如果字段是 externalInterrupt_debug：
      只检查 interface/xaction/connect/monitor/XZ 同名搬运；
      不写 raw、CSR snapshot、status、pass/fail、terminal、redirect 或 replay；
      不恢复 mon_item_port.write；

  对 V2-only output：
    如果当前主 flow 无 schema、无 consumer、无生命周期：
      保留 DUT 顶层连接；
      只记录职责分类和后续专项；
      不新增 raw queue、status 字段、cursor、map、analysis producer 或 helper；

  对 io_l2_tlb_req_resp 和 io_l2_pmp_resp：
    确认它们是顶层 L2/L2Cache/PTW/PMP 侧接口；
    不接入内部 DTLB -> L2TLB responder agent。
```

## 5. 问题四：DCache/SBuffer 与 L2TLB agent 边界需要防止串接

### V2 问题

DCache/SBuffer agent 的主职责是 TileLink responder。`io_l2_hint_*` 和 `io_l2_flush_done` 是 DUT input sideband，不是 DCache TL D response，也不是 monitor output。若把 DCache responder “保持不变”写得过宽，可能遗漏这些 input 在 scalar flow 中必须 known-zero/fail-fast。

同时，顶层 `io_l2_tlb_req_resp_*` 和 `io_l2_pmp_resp_*` 不是当前内部 `L2TLB_agent` 的接管点。

### 修改原因

DCache/SBuffer responder、DCache L2 sideband input 和 L2TLB 内部 responder 是三类不同职责。混写会导致 agent 方向错误、driver owner 重复或 output 被接到错误 responder。

### 修改方案与修改逻辑

DCache/SBuffer：

- TL A/B/C/D/E responder 和已有 `denied/corrupt` 策略保持。
- PBMT/permission 不作为 DCache/SBuffer response 字段。
- `io_l2_hint_valid`、`io_l2_hint_bits_sourceId`、`io_l2_hint_bits_isKeyword` 和 `io_l2_flush_done` 只由 DCache L2 sideband 专项实现 time-zero、xaction、helper、driver known-zero 和非零 fail-fast。
- 本 plan 不为这四个 input 新增 runtime helper、raw 或公共状态。

L2TLB：

- `L2tlb_agent_agent` 保持 DTLB -> L2TLB request、L2TLB -> DTLB response 方向。
- 顶层 L2/L2Cache/PTW/PMP observation 不接入该内部 agent。

### 文字伪代码

```text
check_responder_boundaries()：
  对 DCache/SBuffer：
    保留 TL responder 和 X/Z observation；
    不把 PBMT/permission 写成 DCache/SBuffer response 字段；
    遇到 io_l2_hint_* 或 io_l2_flush_done：
      判定为 DUT input sideband；
      转交 DCache L2 sideband 专项；
      本 plan 不新增 raw、status、helper 或 driver 逻辑；

  对 L2TLB：
    保持内部 DTLB request/response agent 方向；
    如果看到顶层 io_l2_tlb_req_resp_* 或 io_l2_pmp_resp_*：
      只记录为 V2-only output/deferred observation；
      不接到 L2tlb_agent_agent。
```

## 6. 问题五：V2 STA IQ feedback raw 不能伪造 ROB/LQ，vector feedback 必须 fail-fast

### V2 问题

V2 scalar `staIqFeedback.feedbackSlow` 只有：

```text
valid
hit
sqIdx_flag
sqIdx_value
```

它没有 ROB 或 LQ payload。旧逻辑若把 `rob_valid/lq_valid/sq_valid` 全部置 1，并把默认 0 payload 当真实 ROB/LQ key，会导致 adapter 用伪 key 解析 uid。

VSTU feedback 在 scalar-only 当前范围中没有闭环，不能 info/drop 后继续。

### 修改原因

STA feedback 的 uid、ROB key、`issue_epoch/replay_seq` 应由 IQ feedback/replay 专项的 issue-generation token 按真实 SQ key补齐。monitor 只能忠实保存接口真实字段。vector feedback 若进入公共状态，会形成未支持的 vector replay/writeback 路径。

### 修改方案与修改逻辑

修改 `io_mem_to_ooo_iq_feedback_agent_agent_monitor::mon_data()` 的 V2 raw 构造：

- STA valid 时只置 `sq_valid=1` 并复制真实 SQ key。
- `rob_valid=0`、`lq_valid=0`。
- raw 中保留 `hit`。
- uid、ROB key、`issue_epoch/replay_seq` 不在 monitor 反推。
- reset 完成后任一 `vstuIqFeedback_0/1_feedbackSlow_valid` 非 0，立即 `uvm_fatal`。
- 本 plan 不恢复 `mon_item_port.write()`。

### 文字伪代码

```text
io_mem_to_ooo_iq_feedback_agent_agent_monitor::mon_data()：
  先执行原有采样和 X/Z 检查；

  如果 reset 已完成且任一 VSTU feedback valid 非 0：
    uvm_fatal；
    不生成 vector raw；

  如果 STA feedback valid：
    raw = make_empty_raw_iq_feedback()；
    raw.valid = 1；
    raw.hit = sampled_hit；
    raw.sq_valid = 1；
    raw.sq_flag = sampled_sqIdx_flag；
    raw.sq_value = sampled_sqIdx_value；
    raw.rob_valid = 0；
    raw.lq_valid = 0；
    push_raw_iq_feedback(raw)；

  后续由 IQ feedback/replay 专项：
    用 SQ key 做 map/token 查询；
    补 uid、ROB key、issue_epoch 和 replay_seq；
    再进入原 batch/replay flow；
  monitor 不扫描 main_trans_num，不从 status 全表反推 generation。
```

## 7. Coding 落点汇总

| 文件 | 对应问题与修改 |
|---|---|
| `mem_ut/ver/ut/memblock/agent/*/src/*_monitor.sv` | 问题一、二：逐 monitor 复核 runtime role 和 analysis producer deferred 状态 |
| `mem_ut/ver/ut/memblock/env/src/memblock_env.sv` | 问题一：确认 20 个 analysis FIFO consumer 连接事实 |
| `mem_ut/ver/ut/memblock/env/src/memblock_rm.sv` | 问题一：确认 20 个 blocking get consumer 存在但 producer deferred |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/*` | 问题三：`externalInterrupt_debug` 字段级观察链，不进 raw |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_iq_feedback_agent_agent/src/io_mem_to_ooo_iq_feedback_agent_agent_monitor.sv` | 问题五：STA SQ-only raw，VSTU valid fatal |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_vec_wb_agent_agent/src/io_mem_to_ooo_vec_wb_agent_agent_monitor.sv` | 问题二：scalar-only `writebackVldu` valid fatal 边界 |
| `mem_ut/ver/ut/memblock/agent/dcache_agent_agent/src/*` | 问题四：只复核 TL responder 边界；L2 sideband input 不在本 plan 修改 |
| `mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/*` | 问题四：保持内部 DTLB/L2TLB responder 方向 |

明确不修改：

```text
pass/fail/terminal owner
commit/deq cursor
redirect/replay batch 仲裁
RM/checker/scoreboard/coverage
analysis producer 批量恢复
DCache io_l2_hint_* / io_l2_flush_done 驱动实现
顶层 V2-only output 新 agent
```

## 8. 修改类型与原逻辑对比总结

| 修改项 | 类型 | 修改前逻辑 | 修改原因 | 修改后逻辑 |
|---|---|---|---|---|
| monitor 分类维度 | 文档/职责适配 | raw、XZ、analysis 单维混写 | consumer 已连接但 producer deferred 的事实被掩盖 | runtime role 和 RM role 二维分类 |
| 20-agent 表 | 文档/审计适配 | 示例式分类，易漏 agent | coding/review 需要完整职责边界 | 20 个 agent 全部落表 |
| analysis FIFO/RM consumer | 边界更正 | 可能描述为无 consumer | env/RM 实际已连接 blocking get | 统一 `ANALYSIS_PORT_DEFERRED` |
| `externalInterrupt_debug` | 字段级职责适配 | ctrl monitor 整体归为 raw | 该字段是 output observation，不是 ctrl raw | 只保留字段链和 X/Z，不入公共状态 |
| V2-only output | 职责分类 | 部分 output 缺职责记录或可能误接内部 agent | 当前无 schema/consumer/lifecycle | 只分类和 deferred，不新增 runtime 逻辑 |
| DCache L2 sideband | 跨专项 owner 收敛 | 可能混在 DCache responder“不变”中 | 这些是 DUT input，需要 driver owner | 转交 DCache L2 sideband 专项 |
| L2TLB 方向 | agent 边界 | 顶层 L2/PMP output 容易误接内部 agent | 内部 L2TLB agent 是 DTLB/L2TLB responder | 不接顶层 L2/PMP output |
| STA IQ feedback raw | 功能逻辑修改 | 伪造 ROB/LQ valid | V2 只有真实 SQ key | raw 只保存 SQ key，generation 由 IQ 专项 token 补齐 |
| VSTU feedback | unsupported gate | adapter 可能 info/drop | scalar flow 不支持 vector feedback | monitor 边界 fatal |

保持不变的主体逻辑：5 条 raw queue 架构、raw batch 消费顺序、pass/fail/terminal、commit/deq、redirect/replay 仲裁、DCache/SBuffer TL responder、内部 L2TLB responder方向和 RM/checker/coverage deferred 状态。
