# mem_ut V2 IQ Feedback Replay 最小 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 测试框架入口 | `memblock_dispatch_base_sequence::collect_monitor_event_batch()` |
| 适配原则 | monitor 只采真实字段；event 只绑定 current status；保持现有 handler/recovery；不增加历史 generation 防御系统 |
| 创建/修订日期 | 2026-07-21 |

## 1. 范围与成立条件

本 plan 只负责 V2 scalar STA IQ feedback 到现有 STA hit/miss、real-WB 和 replay 流程之间的最小适配，
并唯一负责同一 monitor 上 VSTU feedback 的 scalar-only fail-fast gate。

SCALAR_LDA/STA/STD int-WB 的 raw source、lane、metadata、key 归一化、`replayInst` 和 AMO 检查，以
`mem_ut_v2_int_wb_writeback_framework_adapt_execution_plan_20260708.md` 为唯一实现依据。本 plan 只复用
其归一化后的 real-WB event 和共用的 current snapshot helper，不重复定义 int-WB 修改逻辑。

本轮支持：

- scalar STA IQ feedback：V2 顶层真实字段为 `valid/hit/sqIdx`。
- reset 完成后任一 VSTU feedback valid 非 0，在 monitor raw push 前立即 fatal，不生成 vector raw。
- 使用真实 SQ key 查找当前 active UID，并补齐 current `issue_epoch/replay_seq`。
- `IQ miss -> 现有 replay recovery -> 再次 issue`。
- `IQ hit -> 等待现有 STA real-WB handler 完成`。
- 同拍 `memoryViolation` 继续进入现有 redirect-first 仲裁。

本轮不支持：

- vector IQ feedback的正向转换/处理、`writebackVldu`、vector partial replay；VSTU valid fail-fast 是
  unsupported边界，不代表支持vector IQ flow。
- STD IQ feedback。
- issue-generation token、closed tombstone、claim map 或历史 event 匹配。
- 为 duplicate、RTL 不可达迟到 event 建立额外 pending/seen 状态机。
- 新增通用 expected-fatal 运行脚本。

current status 可以作为 event generation 权威，依赖以下 V2 运行期合同：

```text
STA真实issue fire：
  status.sta_dispatched=1；
  accepted fire保存当前sta_issue_epoch；
  status保存当前replay_seq。

旧代没有IQ返回：
  sta_dispatched保持1；
  scheduler要求!sta_dispatched才可再次issue；
  因此不存在新generation fire。

旧代IQ hit：
  sta_dispatched保持1；
  当前代等待real-WB；
  因此不存在新generation fire。

旧代IQ miss：
  miss先进入当前代handler；
  recovery调用mark_replay_pending()；
  清sta_dispatched并递增replay_seq；
  旧代已经以replay关闭后，才允许下一次issue fire。

StoreUnit scalar STA TLB miss：
  S1产生IQ miss并kill当前flow；
  当前flow不进入后级，因此不会再产生同代合法STA real-WB。
```

所以本 plan 不处理“gen0 尚未关闭时已有 gen1 fire”或“gen0 miss 后仍有合法 WB 与 gen1 重叠”的情况。
如果未来 RTL 改变上述合同，再单独设计历史 token/tombstone，不在本次 V2 适配中预留。

## 2. 问题一：STA IQ raw 伪造不存在的 ROB/LQ key

### V2 问题

V2 `staIqFeedback_0/1.feedbackSlow` 只提供：

```text
valid
hit
sqIdx.flag
sqIdx.value
```

当前 monitor 却把 `rob_valid/lq_valid` 置为 1，而对应 value 来自 empty raw 的默认 0。adapter 因此可能
把 ROB0/LQ0 当成真实 key，导致 UID 错配或 event 被丢弃。

### 最小修改方案

只修改 scalar STA IQ raw builder：

```text
如果feedbackSlow.valid=1：
  raw.valid=1；
  raw.is_sta=1；
  raw.hit=真实hit；
  raw.sq_valid=1；
  raw.sq_flag/raw.sq_value=真实sqIdx；
  raw.rob_valid=0；
  raw.lq_valid=0；
  raw.cycle=$time；
  push一次raw_iq_feedback_q。
```

monitor 不查 UID、不读 status、不补 generation，也不构造 ROB/LQ。

### 同一 monitor 的 VSTU unsupported 边界

`io_mem_to_ooo_iq_feedback_agent_agent_monitor::mon_data()` 同时可见 scalar STA 和 VSTU feedback，因此本
plan 是该 monitor 源码的唯一 owner。reset 完成后，任一
`vstuIqFeedback_0/1_feedbackSlow_valid !== 0` 必须在任何 raw push 前 `uvm_fatal`；valid 均为 0 时不生成
vector raw，也不修改 status、pass/fail 或 terminal。split issue plan 和 monitor output plan 只引用该合同，
不重复修改此 monitor。

```text
IQ feedback monitor：
  先完成原有采样和X/Z检查；
  如果reset已完成且任一VSTU feedback valid !== 0：
    uvm_fatal；
    不生成任何scalar/vector raw；
  否则继续处理scalar STA SQ-only raw。
```

SCALAR_LDA/STA int-WB 缺失 LQ/SQ 的修正由 int-WB 专项 plan 负责，本问题不重复修改
`io_mem_to_ooo_int_wb_agent_agent_monitor.sv`。

## 3. 问题二：replay 后 STA IQ event 缺少 current snapshot

### V2 问题

V2 STA IQ feedback 不携带测试框架的 `uid/issue_epoch/replay_seq`。当前
`normalize_feedback_event()` 在第一次 replay 后会拒绝缺少 `issue_epoch/replay_seq` 的非 STD event，
导致合法 gen1 IQ feedback 被 warning/drop。

### 最小修改方案

在 `dispatch_monitor_event_adapter` 中保留一个轻量
`attach_current_issue_snapshot(ref memblock_wb_event_t event)`。本 plan 只定义 STA IQ 分支：

```text
attach_current_issue_snapshot(STA_IQ event)：
  要求event.valid=1；
  要求source=STA_FEEDBACK、target=STA；
  要求只有真实SQ key有效，ROB/LQ均无效；

  使用active SQ map查唯一uid；
  查不到则fatal，不做全表扫描或warning/drop；

  status=get_status(uid)；
  要求status.active=1；
  要求status.sta_dispatched=1；
  要求status的canonical SQ仍属于该uid；
  要求status.sta_issue_epoch非0；

  event.uid=uid；
  event.rob_key=status canonical ROB；
  event.sq_key=status canonical SQ；
  event.issue_epoch=status.sta_issue_epoch；
  event.replay_seq=status.replay_seq；
  置对应has_uid/has_rob/has_sq/has_issue_epoch/has_replay_seq；

  helper不修改status、active map、queue或handler状态。
```

`convert_raw_iq_feedback()` 的顺序固定为：

```text
用raw真实SQ构造partial STA IQ event
→ attach_current_issue_snapshot()
→ 本地检查UID/ROB/SQ/issue_epoch/replay_seq均完整
→ 设置IQ hit/miss和replay语义
→ 交给现有normalize及batch handler
```

attach 或本地完整性检查失败时固定 fatal。该 event 是 DUT valid 返回，不能静默 drop。

SCALAR_LDA/STA int-WB 使用同名 helper 的 ROB 分支，但具体检查由 int-WB 专项 plan 定义。

### 不修改 generic normalize

本 plan 不新增 `event_requires_current_issue_snapshot()`，也不修改
`common_data_transaction::normalize_feedback_event()` 的全局 fallback 规则。

原因是本 plan 产生的 STA IQ event 在进入 normalize 前已经携带完整 snapshot，正常情况下不会进入
fallback。将检查放在 converter 本地即可解决 gen1 drop，同时不改变 redirect、STD、synthetic event
或其它 producer 的既有语义。

## 4. 问题三：ctrl/deq 在同拍 semantic event 处理前修改 active map

### V2 问题

当前 `collect_ctrl_redirect_events_batch()` 在 `process_monitor_event_batch()` 之前直接调用
`apply_raw_ctrl_deq()`。如果同一采样拍同时存在 writeback/IQ feedback 和 LSQ deq，deq 可能先删除
LQ/SQ active map，使本拍 event 在 normalize/handler 阶段无法完成 current owner 检查。

monitor 在 posedge 采样，service loop 每个 negedge 调用一次 `service_monitor_once()`，正常运行时一个
service batch 只包含前一采样拍的 raw。因此不实现冻结三个 queue、按最小 cycle 循环分组和逐组
recovery。

### 最小修改方案

只把当前 service batch 内的 ctrl/deq 延后：

```text
collect_monitor_event_batch()：
  events=[]；
  deferred_ctrl=[]；
  sample_cycle_valid=0；

  drain当前可见int-WB和IQ raw：
    检查raw.cycle与本batch sample_cycle一致；
    转换成功的event加入events；

  drain当前可见ctrl raw：
    检查raw.cycle与本batch sample_cycle一致；
    保存到deferred_ctrl；
    memoryViolation有效时转换成redirect event并加入events；
    此时不调用apply_raw_ctrl_deq()；

  如果同一service drain出不同raw.cycle：
    fatal，说明“每negedge消费一次”的当前调度合同被破坏；

  process_monitor_event_batch(events)：
    继续使用现有redirect-first逻辑；

  按deferred_ctrl原顺序调用apply_raw_ctrl_deq()；

  返回service_monitor_once()；
  继续由现有代码调用一次exception_redirect_replay_task()。
```

不改变 int-WB 与 IQ producer 各自的 queue 顺序，不增加多 cycle 排序，不在
`collect_monitor_event_batch()` 内新增 recovery 调用。

## 5. 问题四：STA real-WB 不应早于 IQ hit

### V2 问题

V2 scalar STA 的合法顺序只有：

```text
IQ miss → replay
IQ hit  → real-WB或fault-WB
```

当前状态中已经存在：

```text
sta_issue_feedback_success
sta_writeback
sta_pass
sta_fault
```

因此不需要再增加 `sta_iq_feedback_seen/sta_real_wb_seen`。

### 最小修改方案

复用现有状态：

```text
处理scalar STA IQ hit：
  先调用mark_issue_feedback_success()；
  置sta_issue_feedback_success=1；
  如果MEMBLOCK_STA_REAL_WB_PASS_EN=1：
    返回成功并等待real-WB；
  否则：
    保持现有兼容路径，再调用mark_target_normal_pass()。

处理STA real-WB或fault-WB：
  读取当前status；
  如果sta_issue_feedback_success=0：
    以STA_WB_BEFORE_IQ fatal；
  否则进入现有handle_real_writeback_event()逻辑。

处理scalar STA IQ miss：
  保持现有push_feedback_event()；
  recovery调用mark_replay_pending()；
  现有逻辑清sta_dispatched、sta_writeback、sta_issue_feedback_success、sta_pass；
  递增replay_seq后重新入issue queue。
```

现有 `sta_pass/sta_fault` 已能阻止同一 target 再次完成；本 plan 不为 duplicate IQ、duplicate WB、
miss 后不可达 WB 增加专用状态或额外 lifecycle helper。

`MEMBLOCK_STA_REAL_WB_PASS_EN=0` 保持现有兼容语义，不新增“兼容 pass 后继续跟踪真实 WB”的 no-op
状态机和专项配置。默认严格路径仍使用 `MEMBLOCK_STA_REAL_WB_PASS_EN=1`。

## 6. 最小实现流程

```text
真实STA issue fire
→ 现有逻辑记录sta_issue_epoch/replay_seq并置sta_dispatched

DUT返回scalar STA IQ raw
→ monitor只采valid/hit/SQ
→ adapter用SQ查current active UID
→ 从status补ROB/SQ/issue_epoch/replay_seq
→ converter本地检查snapshot完整

若同拍有memoryViolation
→ 放入同一个semantic batch
→ 现有batch handler执行redirect-first

未被redirect覆盖的IQ event
→ hit：记录sta_issue_feedback_success，等待real-WB
→ miss：进入现有replay queue/recovery

semantic batch处理完成
→ 再应用本拍LSQ deq
→ service尾部执行现有一次recovery
```

## 7. Coding 落点

| 文件 | 最小修改 |
|---|---|
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_iq_feedback_agent_agent/src/io_mem_to_ooo_iq_feedback_agent_agent_monitor.sv` | 唯一实现scalar STA SQ-only raw和VSTU valid fail-fast；ROB/LQ valid固定为0，不生成vector raw |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv` | 增加 STA IQ current snapshot attach；ctrl raw 延后 apply；可见 raw 必须同 cycle |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv` | STA IQ hit 始终先记录现有 feedback success；STA WB 前检查该状态 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv` | semantic batch 后按原顺序 apply deferred ctrl；保留 service 尾部一次 recovery |
| `mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test` | 复用现有 soft-test 结构增加最小正向场景，不新增通用 expected-fatal runner |

int-WB monitor、raw struct、metadata guard 和 ROB 分支 snapshot attach 的具体实现，继续由
`mem_ut_v2_int_wb_writeback_framework_adapt_execution_plan_20260708.md` 管理。

明确不修改或不新增：

```text
common_data_transaction::normalize_feedback_event()
common_data_transaction::event_requires_current_issue_snapshot()
status_transaction中的STA seen字段
check_current_issue_event_stage()/commit_current_issue_event_stage()
多cycle raw local queue排序循环
issue-generation token/tombstone
issue_queue_scheduler::mark_issue_fire()
issue_queue_scheduler::mark_issue_fire_already_accepted()
LSQ enqueue、ROB commit/deq owner
通用run_expected_fatal.sh
vector LS正向闭环、RM、scoreboard、checker和coverage（VSTU valid fail-fast除外）
```

## 8. 验证与验收

最小正向场景：

1. `STA IQ hit -> STA real-WB`：IQ 和 WB 都绑定同一 `issue_epoch/replay_seq`，最终正常 pass。
2. `STA IQ miss -> replay -> gen1 IQ hit -> gen1 real-WB`：gen1 event 不再被 normalize drop，最终收敛。
3. `memoryViolation + 被覆盖 event` 同拍：redirect-first 丢弃被覆盖 event，状态不被错误写成 pass。
4. IQ/WB 与 LSQ deq 同拍：event 先完成 owner/handler 检查，deq 后应用，active map 不被提前删除。

局部检查：

- STA IQ raw 的 `rob_valid/lq_valid` 必须为 0，`sq_valid` 必须为 1。
- reset完成后任一VSTU feedback valid非0必须在raw push前fatal；本monitor不得生成vector raw。
- attach 后必须具有 UID、ROB、SQ、`issue_epoch/replay_seq`。
- gen0 未返回或 IQ hit 等待 WB 时不得出现同 UID STA gen1 fire。
- IQ miss recovery 后 `replay_seq` 只递增一次，下一次真实 fire 分配新的 `issue_epoch`。
- 默认严格模式下 STA WB 到达前必须已经记录 IQ hit。
- 每次 service drain 的 int-WB、IQ、ctrl raw 必须属于同一采样 cycle。

不新增通用 expected-fatal 脚本。需要验证 `STA_WB_BEFORE_IQ` 时，复用仓库现有仿真命令和日志检查
方式即可；该验证基础设施不作为本功能 coding 的前置条件。

## 9. 修改前后对比

| 修改项 | 修改前 | 最小修改后 |
|---|---|---|
| STA IQ raw | 伪造 ROB0/LQ0 | 只保留真实 SQ |
| generation | replay 后缺 snapshot 被 drop | 用真实 SQ 查 current status 并附加 snapshot |
| normalize | 计划增加全局 snapshot 强制谓词 | generic normalize 不变，converter 本地检查 |
| raw batch | 计划按多个 cycle 分组并逐组 recovery | 每 service 单 batch；mixed-cycle fatal；ctrl/deq 延后 |
| STA 阶段 | 计划新增两个 seen 位和 check/commit helper | 复用 `sta_issue_feedback_success/sta_writeback/pass/fault` |
| 迟到/duplicate | 计划增加额外 lifecycle 防御 | 依赖当前 V2 合同，不增加历史状态 |
| 负向验证 | 计划新增通用 expected-fatal runner | 不属于功能适配，复用现有运行方式 |

保持不变的主体逻辑：issue fire 建账、IQ hit/miss handler、real-WB pass/fault handler、
redirect-first 仲裁、exception/replay recovery、terminal/pass/fail 定义。
