# V2 IQ Feedback Replay 测试框架适配 Implementation Review

| 项目 | 内容 |
|---|---|
| 关联 Plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_iq_feedback_replay_framework_adapt_execution_plan_20260711.md` |
| 目标版本 | V2，分支`mem_ut_uvm_v2` |
| Review 范围 | scalar STA IQ raw、current snapshot反查、同拍batch顺序、deferred ctrl、严格STA real-WB顺序、software replay smoke及同步文档 |
| Review 日期 | 2026-07-22 |
| Review 状态 | 本agent源码/plan独立review通过；subagent因thread limit不可用，未取得独立subagent结论 |

## 1. 结论

本轮实现与子plan目标一致，完成了V2 scalar STA IQ feedback最小适配：

- monitor不再伪造ROB0/LQ0，只产生真实SQ-only raw。
- adapter用active SQ map反查唯一uid，并从current status补齐ROB、issue epoch和replay
  sequence；当前必需event无法关联时fail-fast。
- 同一采样batch先处理IQ raw、再处理int-WB raw，仍统一进入redirect-first仲裁。
- ctrl deq延后到semantic batch完成后应用，避免本拍LQ/SQ mapping提前被释放。
- 严格模式要求STA real-WB/fault-WB前已经观察到current IQ hit。
- replay smoke不再直接伪造完整replay event，而是注入与真实monitor相同的SQ-only raw。
- VSTU/vector IQ与STD IQ继续属于unsupported边界，观察到有效事件时fatal。

没有新增generation token、claim map、tombstone、STA seen字段或第二套pass/fail/terminal
owner。`normalize_feedback_event()`、issue fire、recovery、commit/deq主体控制行为保持不变。

## 2. 修改前后逻辑

### 2.1 修改前

旧STA IQ monitor把`rob_valid/lq_valid`都置为1，但V2端口没有对应payload，empty raw中的0会
被误当成ROB0/LQ0。adapter随后只复制raw key，不补current issue snapshot；第一次replay后，
缺失`issue_epoch/replay_seq`的合法IQ event可能被normalize丢弃。

ctrl raw在semantic batch之前立即调用`apply_raw_ctrl_deq()`，同拍deq可能先删除STA IQ或
real-WB需要的active SQ mapping。writeback handler也没有显式阻止STA real-WB绕过IQ hit。

software replay smoke直接构造带uid/ROB/SQ/epoch/replay sequence的完整event，因而没有覆盖
V2真实SQ-only raw到adapter的转换缺口。

### 2.2 修改后

```text
DUT STA IQ valid/hit/sqIdx
  -> monitor生成SQ-only raw
  -> adapter以SQ查active uid并校验current status owner
  -> event补齐ROB/SQ/issue_epoch/replay_seq
  -> 同batch按IQ、int-WB、memoryViolation顺序收集semantic event
  -> batch handler统一normalize和redirect-first
  -> 未覆盖IQ hit设置sta_issue_feedback_success；miss进入replay
  -> 未覆盖STA real-WB在严格模式检查IQ hit后进入原pass/fault owner
  -> semantic batch结束后按raw FIFO应用ctrl deq
  -> service尾部沿用原exception/replay recovery
```

这个变化包含字段适配和局部功能顺序修正，但没有重写测试框架主体状态机。

## 3. 源码修改审查

### 3.1 IQ monitor只产生真实SQ raw并隔离VSTU

源码位置：
`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_iq_feedback_agent_agent/src/io_mem_to_ooo_iq_feedback_agent_agent_monitor.sv`，
task：`mon_data()`。

该task是IQ raw producer。它先拒绝scalar-only范围外的VSTU，再把两路STA真实
`valid/hit/sqIdx`写入共享raw queue，不读取status或反查uid。

```systemverilog
if (io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid !== 1'b0 ||
    io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid !== 1'b0) begin
    `uvm_fatal("IQ_FEEDBACK_MON",
               "VSTU IQ feedback is outside the scalar-only V2 flow")
end
```
中文伪代码：

该分支承担scalar/vector边界隔离。reset backend完成后先读取两路VSTU valid；任一路不是
确定的0时立即fatal，停止本拍后续raw构造。它不生成vector raw、不修改status，也不把VSTU
静默当成STA。

```systemverilog
raw_iq_feedback = memblock_sync_pkg::make_empty_raw_iq_feedback();
raw_iq_feedback.valid     = 1'b1;
raw_iq_feedback.is_sta    = 1'b1;
raw_iq_feedback.hit       = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit;
raw_iq_feedback.sq_valid  = 1'b1;
raw_iq_feedback.sq_flag   = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
raw_iq_feedback.sq_value  = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value;
raw_iq_feedback.rob_valid = 1'b0;
raw_iq_feedback.lq_valid  = 1'b0;
raw_iq_feedback.cycle     = $time;
memblock_sync_pkg::push_raw_iq_feedback(raw_iq_feedback);
```
中文伪代码：

该分支承担V2 STA端口事实采集。先创建全中性的empty raw；再写valid、STA来源、真实hit和
完整SQ key；明确清ROB/LQ valid，防止默认0值获得key语义；最后冻结采样时间并调用
`push_raw_iq_feedback()`写入共享队列。第二路STA执行相同逻辑，仅`port_id`和信号来源不同。

正确性检查：monitor不写uid/ROB/LQ/epoch，不与公共状态表形成第二个owner；VSTU event不会
进入下游永远无法完成的scalar flow。

### 3.2 SQ-only event反查current issue snapshot

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，
function：`attach_current_issue_snapshot()`。

该分支把V2 STA SQ-only event关联到当前唯一active store，并复用已有
`fill_current_issue_snapshot()`完成统一的active、target、ROB/SQ owner和epoch检查。

```systemverilog
if (wb_event.source == MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK &&
    wb_event.target == MEMBLOCK_ISSUE_TARGET_STA &&
    wb_event.has_sq && !wb_event.has_rob && !wb_event.has_lq) begin
    if (!data.is_valid_sq_key(wb_event.sq_key))
        `uvm_fatal("IQ_FEEDBACK_ATTACH", "STA IQ raw SQ key is incomplete")
    if (!data.lookup_active_uid_by_sq(wb_event.sq_key, iq_uid))
        `uvm_fatal("IQ_FEEDBACK_ATTACH", "no active uid for STA IQ SQ key")
    iq_status = data.get_status(iq_uid);
end
```
中文伪代码：

该逻辑先确认event确实是STA feedback且只有SQ有效；检查SQ value落在物理SQ范围；调用
`lookup_active_uid_by_sq()`从active SQ map查唯一uid。没有命中时立即fatal，不能warning/drop
后让主动flow等待。命中后读取该uid的current status，后续所有补齐字段都来自这个真源。

```systemverilog
if (!iq_status.active_sq_mapped || !iq_status.sta_dispatched ||
    canonical_sq.flag != wb_event.sq_key.flag ||
    canonical_sq.value != wb_event.sq_key.value) begin
    `uvm_fatal("IQ_FEEDBACK_ATTACH", "STA IQ SQ owner mismatch")
end
iq_candidate = wb_event;
iq_candidate.uid = iq_uid;
iq_candidate.has_uid = 1'b1;
iq_candidate.rob_key = iq_status.get_rob_key();
iq_candidate.has_rob = 1'b1;
if (!fill_current_issue_snapshot(iq_candidate, iq_uid,
                                 iq_candidate.rob_key, 1'b0, 0, 1'b1))
    `uvm_fatal("IQ_FEEDBACK_ATTACH", "STA IQ current snapshot validation failed")
wb_event = iq_candidate;
```
中文伪代码：

该逻辑从status重建canonical SQ并检查active mapping、STA已真实发射、raw SQ仍属于同一uid；
任一条件失败立即fatal。检查通过后先把uid和canonical ROB写入局部candidate，再调用
`fill_current_issue_snapshot()`：该公共helper检查uid active、未terminal/flush/kill/redirect、
STA target dispatched、ROB owner、SQ owner和非零issue epoch，并补齐SQ、issue epoch、
replay sequence。全部成功后才原子替换输出event；helper不修改status、map或queue。

正确性检查：反查最多一次SQ map lookup，不扫描主表；event身份只有current status一个真源；
replay重发后读取新issue epoch和新replay sequence，不使用旧raw中的伪字段。

### 3.3 IQ converter的能力检查和hit/miss语义

源码位置：同文件，function：`convert_raw_iq_feedback()`。

```systemverilog
if (raw.vector_feedback)
    `uvm_fatal("DISP_MON_ADAPT", "vector IQ feedback is unsupported")
if (!raw.is_sta && !raw.is_std)
    `uvm_fatal("DISP_MON_ADAPT", "IQ feedback has no supported scalar target")
if (raw.is_std)
    `uvm_fatal("DISP_MON_ADAPT", "STD IQ feedback cannot complete strict V2 STD real-WB target")
if (!raw.sq_valid || raw.rob_valid || raw.lq_valid)
    `uvm_fatal("DISP_MON_ADAPT", "STA IQ feedback must be SQ-only")
```
中文伪代码：

converter先过滤能力边界：vector、未知scalar target和STD都不是本plan支持输入，立即fatal；
合法STA必须恰好只有SQ key，缺SQ或携带伪ROB/LQ同样fatal。所有检查发生在event落表和replay
入队前，不会留下半更新状态。

```systemverilog
wb_event.has_sq = raw_sq_to_key(raw.sq_valid, raw.sq_flag,
                                raw.sq_value, wb_event.sq_key);
attach_current_issue_snapshot(wb_event);
if (!wb_event.has_uid || !wb_event.has_rob || !wb_event.has_sq ||
    !wb_event.has_issue_epoch || !wb_event.has_replay_seq)
    `uvm_fatal("DISP_MON_ADAPT", "STA IQ feedback snapshot is incomplete")
wb_event.iq_feedback_valid  = 1'b1;
wb_event.iq_feedback_hit    = raw.hit;
wb_event.iq_feedback_failed = !raw.hit;
wb_event.replay_valid       = !raw.hit;
```
中文伪代码：

converter先只复制真实SQ，再调用snapshot helper补齐身份；补齐后再次要求uid、ROB、SQ、issue
epoch和replay sequence全部有效。随后把hit映射到`iq_feedback_hit`，把miss映射到
`iq_feedback_failed/replay_valid`；只生成IQ/replay语义，不设置`real_wb_valid`或pass/fault。

### 3.4 同一采样batch固定IQ先于int-WB

源码位置：同文件，task：`check_raw_sample_cycle()`、`collect_writeback_events_batch()`。

```systemverilog
if (!sample_cycle_valid) begin
    sample_cycle = raw_cycle;
    sample_cycle_valid = 1'b1;
end else if (sample_cycle != raw_cycle) begin
    `uvm_fatal("DISP_MON_BATCH", "mixed monitor sample cycle")
end
```
中文伪代码：

第一个被消费的raw建立本次service batch的采样时间；之后IQ、int-WB或ctrl raw都必须具有相同
时间。发现不同时间立即fatal，说明“一拍采样、下一service边界消费”的调度合同已破坏；本plan
不悄悄跨拍重排，也不在一个service中执行多次recovery。

```systemverilog
while (memblock_sync_pkg::pop_raw_iq_feedback(raw_iq)) begin
    check_raw_sample_cycle(raw_iq.cycle, sample_cycle,
                           sample_cycle_valid, "iq_feedback");
    if (convert_raw_iq_feedback(raw_iq, wb_event))
        events.push_back(wb_event);
end
while (memblock_sync_pkg::pop_raw_int_wb(raw_int_wb)) begin
    check_raw_sample_cycle(raw_int_wb.cycle, sample_cycle,
                           sample_cycle_valid, "int_wb");
    if (convert_raw_int_wb(raw_int_wb, wb_event))
        events.push_back(wb_event);
end
```
中文伪代码：

先按IQ raw queue自身FIFO逐项出队、校验cycle、转换并追加semantic event；再对int-WB queue
执行相同步骤。各producer queue内部顺序不变，只固定同拍跨队列顺序。后续所有event仍作为
同一个数组交给batch handler，因此IQ-first不会跳过active redirect或same-batch redirect-first。

正确性检查：合法同拍STA IQ hit和STA real-WB先记录feedback success再检查writeback；同拍
IQ miss加未覆盖real-WB仍fatal，符合当前V2合同。

### 3.5 ctrl/deq延后到semantic batch之后

源码位置：同文件，task：`collect_ctrl_redirect_events_batch()`。

```systemverilog
while (memblock_sync_pkg::pop_raw_ctrl(raw_ctrl)) begin
    check_raw_sample_cycle(raw_ctrl.cycle, sample_cycle,
                           sample_cycle_valid, "ctrl");
    deferred_ctrl.push_back(raw_ctrl);
    if (convert_raw_memory_violation(raw_ctrl, wb_event))
        events.push_back(wb_event);
end
```
中文伪代码：

按raw ctrl FIFO出队并校验它与本batch同拍；把完整raw保存到局部`deferred_ctrl`；若含
memoryViolation，只把redirect semantic event追加到events。此时不调用
`apply_raw_ctrl_deq()`，所以本拍LQ/SQ active map继续供IQ/WB owner检查使用。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv`，
task：`collect_monitor_event_batch()`。

```systemverilog
monitor_adapter.collect_writeback_events_batch(events,
                                               sample_cycle,
                                               sample_cycle_valid);
monitor_adapter.collect_ctrl_redirect_events_batch(events,
                                                   deferred_ctrl,
                                                   sample_cycle,
                                                   sample_cycle_valid);
monitor_batch_handler.process_monitor_event_batch(events);
foreach (deferred_ctrl[idx]) begin
    monitor_adapter.apply_raw_ctrl_deq(deferred_ctrl[idx]);
end
```
中文伪代码：

主batch先收集IQ/int-WB，再收集memoryViolation；调用`process_monitor_event_batch()`统一normalize、
选择oldest redirect并只让未覆盖event落状态。该函数返回后，按`deferred_ctrl`原FIFO逐项调用
`apply_raw_ctrl_deq()`更新`sb_is_empty`并把LQ/SQ deq交给commit handler释放mapping。task不新增
recovery调用，外层`service_monitor_once()`仍只在最后调用一次
`exception_redirect_replay_task()`。

正确性检查：deq只改变batch内应用时点，不改变count/pointer语义，也不以`scommit`推进deq；
redirect-first和commit/deq owner职责保持分离。

### 3.6 严格STA real-WB顺序检查

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv`，
function：`handle_real_writeback_event()`。

```systemverilog
status = data.get_status(uid);
if (wb_event.target == MEMBLOCK_ISSUE_TARGET_STA &&
    target_real_wb_pass_enabled(MEMBLOCK_ISSUE_TARGET_STA) &&
    !status.sta_issue_feedback_success) begin
    `uvm_fatal("WB_STATUS_STA_ORDER",
               "STA real writeback arrived before IQ hit")
end
```
中文伪代码：

确认event具有real-WB或fault语义后，读取其current uid status。仅当target为STA且严格
`MEMBLOCK_STA_REAL_WB_PASS_EN`已开启时，检查`sta_issue_feedback_success`；未置位立即fatal，
不写writeback/pass/fault。检查通过后继续执行原有fault分支或normal pass分支。LOAD、STD和
关闭严格开关的兼容路径不受此检查影响。

正确性检查：IQ hit的唯一写者仍是`mark_issue_feedback_success()`；real-WB的唯一完成owner仍是
原`mark_target_normal_pass()/mark_target_fault()`，没有新增第二套完成状态。

### 3.7 software replay smoke覆盖真实adapter入口

源码位置：
`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_replay_smoke_sequence.sv`，
task：`submit_raw_sta_iq_feedback()`。

```systemverilog
raw_iq = memblock_sync_pkg::make_empty_raw_iq_feedback();
raw_iq.valid    = 1'b1;
raw_iq.is_sta   = 1'b1;
raw_iq.sq_valid = 1'b1;
raw_iq.sq_flag  = item.sq_key.flag;
raw_iq.sq_value = item.sq_key.value;
raw_iq.hit      = hit;
raw_iq.cycle    = $time;
memblock_sync_pkg::push_raw_iq_feedback(raw_iq);
collect_monitor_event_batch();
```
中文伪代码：

soft test从已真实software fire的STA item取得SQ key，构造与monitor相同的SQ-only raw并设置
hit/miss；写入共享IQ raw queue后立即调用公共batch入口。该task不填uid/ROB/epoch/replay
sequence，必须由被测adapter反查；因此能够覆盖旧`make_replay_wb_event()`绕过的适配缺口。

```systemverilog
submit_raw_sta_iq_feedback(first_sta_item, 1'b0);
exception_redirect_replay_task();
fire_replay_sta_item(1, replay_sta_item);
submit_raw_sta_iq_feedback(replay_sta_item, 1'b1);
if (!data.get_status(1).sta_issue_feedback_success)
    `uvm_fatal(get_type_name(), "replay STA IQ hit did not record feedback success")
```
中文伪代码：

首次STA item注入miss后调用原recovery，使status进入STA replay pending并递增replay sequence；
重新route/fire后得到current replay item；再注入hit并要求current status记录feedback success。
随后原sequence继续注入旧epoch、旧replay sequence和最终current real-WB，验证stale保护及终态。

## 4. 调用关系与状态副作用

| 顺序 | 函数/task | 本流程职责 | 状态/队列副作用 |
|---:|---|---|---|
| 1 | `io_mem_to_ooo_iq_feedback_agent_agent_monitor::mon_data()` | 采V2 STA真实字段 | SQ-only raw入`raw_iq_feedback_q` |
| 2 | `collect_writeback_events_batch()` | IQ-first跨队列收集 | raw出队，semantic event入局部`events` |
| 3 | `convert_raw_iq_feedback()` | 校验能力并构造hit/miss | 不直接写status |
| 4 | `attach_current_issue_snapshot()` | active SQ owner反查 | 只补event身份，不改map/status |
| 5 | `collect_ctrl_redirect_events_batch()` | 收memoryViolation并暂存deq | ctrl raw出队，写局部`deferred_ctrl` |
| 6 | `process_monitor_event_batch()` | normalize和redirect-first | 未覆盖event交给handler，redirect入recovery queue |
| 7 | `handle_issue_feedback_event()` | hit/miss分类 | hit置feedback success；miss入recovery queue |
| 8 | `handle_real_writeback_event()` | 严格顺序和完成owner | 合法后更新pass/fault；非法顺序fatal |
| 9 | `apply_raw_ctrl_deq()` | semantic batch后释放LSQ map | 更新SB empty，推进DUT deq模型 |
| 10 | `exception_redirect_replay_task()` | 原replay/redirect/fault恢复 | miss清STA旧状态、bump replay sequence并允许重发 |

## 5. Plan 对齐检查

| Plan要求 | 实现检查 | 结论 |
|---|---|---|
| scalar STA raw只保留真实SQ | 两路monitor均`rob_valid/lq_valid=0` | 一致 |
| VSTU valid fail-fast | raw push前检查两路VSTU valid | 一致 |
| active SQ map/current status补snapshot | adapter唯一SQ分支复用`fill_current_issue_snapshot()` | 一致 |
| 必需event关联失败不得静默drop | SQ/key/owner/snapshot失败均fatal | 一致 |
| 不修改generic normalize | `common_data_transaction.sv`无本轮差异 | 一致 |
| ctrl/deq延后到semantic batch后 | 局部`deferred_ctrl`在batch handler返回后应用 | 一致 |
| STA real-WB前必须IQ hit | 严格开关下`WB_STATUS_STA_ORDER` fatal | 一致 |
| 不新增token/tombstone/seen字段 | 相关类型和status均无本轮差异 | 一致 |
| software smoke覆盖SQ-only入口 | 删除完整replay event builder，改推raw并调用batch | 一致 |

## 6. 与原Plan不一致的实现

执行前原Plan规定STA IQ hit必须先于real-WB，但未明确两个独立raw queue在同一采样拍的
跨队列顺序。coding后发现原收集顺序是int-WB先于IQ，会让合法同拍hit/WB触发严格顺序fatal。

实际实现按`IMPLEMENTATION_DELTA`调整为同一batch先IQ、后int-WB。它只改变跨队列append
顺序，各queue内部FIFO、batch redirect-first、handler owner和recovery次数均不变。该差异已经
写入plan的`执行中补充/修正（IMPLEMENTATION_DELTA）`，并同步到权威flow。

除该项外，源码修改均与执行前Plan一致。

## 7. Plan未说明但实现补充的细节

- `check_raw_sample_cycle()`把IQ/int-WB/ctrl约束到同一`$time`采样拍；它是plan问题三中
  “单service单batch”合同的可执行fail-fast，不实现跨拍排序器。
- software smoke在replay hit后立即读取`sta_issue_feedback_success`并fatal检查，防止后续
  stale real-WB测试掩盖IQ hit未落表。
- 关键新增源码注释按项目规则使用中文，解释SQ owner、IQ-first和严格STA阶段职责。

这些补充不改变计划定义的状态机和功能边界。

## 8. 文档同步检查

已新增当前权威flow：

- `AI_DOC/mem_ut_flow_doc/iq_feedback_replay_v2_flow.md`

已同步：

- `replay_flow.md`、`normal_pass_flow.md`、`fault_exception_flow.md`、`redirect_flow.md`、
  `rob_commit_lq_sq_deq_flow.md`、`writeback_function_call_flow.md`。
- `soft_test_and_mixed_directed_flow.md`已删除旧`make_replay_wb_event()`主链路描述。
- `dispatch_monitor_event_adapter.md`、`dispatch_monitor_batch_handler.md`、
  `memblock_dispatch_base_sequence.md`、`writeback_status_handler.md`和software replay sequence源码分析。
- `dispatch_testbench_global_sync.md`已同步collector函数签名、IQ-first和deferred ctrl时序。
- V2 agent interface matrix已把旧token/claim提案替换为当前active SQ/current status实现。

早期大篇幅token/claim图只在旧综合flow顶部明确标为历史方案，不作为当前源码权威；当前IQ
调用链以新增专项flow和本review为准。

## 9. 验证结果

| 验证项 | 结果 | 说明 |
|---|---|---|
| `git diff --check` | 通过 | 本专项源码和文档无空白错误 |
| VCS/Verdi全量编译 | 通过 | 清理损坏增量库后`0 error(s), 0 warning(s)` |
| `tc_sanity`基础运行 | 通过 | `TEST CASE PASSED`，`UVM_ERROR=0`、`UVM_FATAL=0`，退出码0；运行依赖编译阶段曾出现VCS incremental `SIGSEGV`，但后续partcomp/link/sim完成 |
| 真实store smoke | 部分通过 | 已走到`STD real-WB -> STA IQ -> STA real-WB -> ROB commit`；后续既有SQ deq pointer mismatch归LSQ专项 |
| `tc_dispatch_replay_smoke` | 被前置环境阻塞 | sequence前触发int-WB monitor `STD0 valid is X/Z` |
| 临时关闭int-WB monitor | 不可用于验证 | env connect phase对disabled monitor空句柄解引用；未进入IQ sequence |
| subagent review | 未执行 | spawn持续返回`agent thread limit reached`，已有agent id均为`not_found` |

## 10. 残余风险

1. software replay正向场景尚未实际运行到SQ-only raw注入点；编译和真实store路径不能完全替代
   `miss -> replay -> reissue -> hit -> real-WB`专项运行证明。
2. mixed-cycle fatal依赖当前“monitor posedge采样、service negedge逐拍消费”合同；未来若service
   允许停顿或批量积压，需要另建跨拍排序方案，不能直接放宽fatal。
3. VSTU/vector和STD IQ没有正向支持；本轮有意fail-fast，不能把fatal边界解释为功能覆盖。
4. 独立subagent review因工具并发限制未取得结果；本agent已按git diff、源码调用链、plan和
   编译/运行日志独立审查，但该项仍是流程层未完成证据。
5. `eda_run`依赖编译阶段仍偶发VCS incremental `SIGSEGV`；本次后续partcomp/link/sim完成并
   通过基础case，后续若再次出现应按规则清理增量数据库后重建。

## 11. 非本次修改的逻辑分析

### 11.1 `git status`对比结论

本review覆盖本轮5个SystemVerilog源码文件、IQ子plan归档、总控plan记录、新增IQ专项flow、
本轮同步的flow/analysis文档以及本implementation review。

当前工作区还存在以下不属于本IQ子计划的修改，本轮不分析其功能正确性，也不纳入IQ commit：

| 类别 | 文件/目录 | 判断 | 原因 |
|---|---|---|---|
| 文档搬迁/整理 | `AI_DOC/plan/test_framework/review_doc/undo/ai_doc_mem_ut_uvm_v2_sync_review_20260706.md`等11份旧review显示删除 | 非本次逻辑 | 对应文件出现在`review_doc/do`，属于其他任务的review归档整理。 |
| 文档搬迁/整理 | `AI_DOC/plan/test_framework/review_doc/do/*.md`中对应11份未跟踪文件 | 非本次逻辑 | 与上项构成旧review从`undo`到`do`的路径搬迁，不涉及本轮IQ源码。 |
| 测试配置变更 | 无 | 无 | ignored的`user_cfg.local.sv`临时验证改动已经恢复，不进入工作区diff。 |
| 仿真/生成产物 | 无需提交 | 非源码review | `sim/base_fun`编译数据库属于ignored生成物，不进入git状态。 |

提交时必须显式stage本review覆盖的文件，不能用目录级`git add`把上述搬迁混入。

## 12. 最终判断

代码和文档实现没有发现阻塞性逻辑错误，满足V2 scalar STA IQ feedback最小适配目标。当前
可归档本子plan并提交独立commit；software replay环境阻塞和subagent不可用必须保留为明确
验证边界，后续不能把它们记成专项运行已通过。
