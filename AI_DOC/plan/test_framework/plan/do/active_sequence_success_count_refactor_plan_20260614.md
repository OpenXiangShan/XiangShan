# 主动发射型 sequence 公共 success 退出改造方案

## 1. 背景

当前 `memblock_lintsissue_dispatch_sequence::drive_dispatch_issue_loop()` 每拍调用：

```systemverilog
issue_sched.route_all_ready_uids();
send_issue_cycle(cycle_idx, has_fire);
issue_sched.advance_issue_queue_delays();
pending_issue_work = issue_sched.has_pending_issue_work();
```

其中旧版 `issue_queue_scheduler::has_pending_issue_work()` 设计会扫描 load/STA/STD 三个 issue
queue，并继续扫描 active uid 窗口判断是否还有潜在 route work。该类局部 pending-work
判断虽然能辅助判断 issue flow 是否仍有工作，但每拍扫描 active window 成本较高，而且会和
后续统一的 global stop 退出条件形成第二套停止判断真源。当前实现已删除未接入主流程的
`uid_has_pending_route_work()` helper。

测试框架已经有一个更适合作为全局完成条件的公共进度：

```systemverilog
data.advance_success_prefix();
return data.dispatch_progress.success_prefix_uid >= data.main_trans_num;
```

这里的 `success_prefix_uid` 不是 issue fire 成功数，而是公共状态表里从 uid=0 开始连续
最终完成的 transaction 数。只有 transaction 已经完成 writeback/pass、ROB commit、LSQ deq
和 active map retire 后，才会推进到 `status.success=1` 并进入 success prefix。

因此，主动发射型 sequence 不需要自己维护“issue target fire success count”，也不应在每个
子 sequence 里分别调用 `advance_success_prefix()` 并判断 `success_prefix_uid >= main_trans_num`。
公共最终完成条件应由顶层 orchestration 统一维护，并在完成后置位：

```systemverilog
data.request_global_stop_if_done()
```

各子 sequence 只读取 `data.is_global_stop_requested()` 作为收尾退出条件。

## 2. 关键语义

### 2.1 `success_prefix_uid` 表示什么

`success_prefix_uid` 表示从 0 开始连续完成的 uid 个数。

它代表的是最终 transaction 完成，不是：

- issue queue fire 成功；
- LSQ enqueue 成功；
- writeback pass；
- ROB commit 单独完成。

完成条件由公共状态表综合判断，最终在 active ROB/LQ/SQ map 释放后 retire。

### 2.2 已 success 的 uid 不需要回退

已经 `status.success=1` 的 uid 不应该再被 redirect/replay/flush 覆盖。当前源码中
`common_data_transaction::prepare_uid_for_redirect_reissue()` 遇到 success uid 会 fatal：

```systemverilog
if (status.success) begin
    `uvm_fatal("COMMON_DATA",
               $sformatf("redirect tries to flush already success uid=%0d", uid))
end
```

因此，使用 `success_prefix_uid` 作为顶层 `global_stop_requested` 触发条件时，不需要设计额外的
redirect/replay 回退计数。未最终完成的 transaction 不会推进 success prefix，顶层不会置位
`global_stop_requested`，各子 sequence 会继续运行并等待后续 reissue/replay/commit/deq。

## 3. Sequence 分类规则

### 3.1 主 transaction driver

主 transaction driver 是测试框架主动向 DUT 主流程输入接口发送主表 transaction 的 sequence。
这类 sequence 的工作对象来自 `main_table_by_uid`，最终完成由公共状态表和 success prefix 描述。

适用对象：

- `memblock_lsqenq_dispatch_sequence`
- `memblock_lintsissue_dispatch_sequence`
- `memblock_lsqcommit_dispatch_sequence`

统一原则：

```text
正常退出受顶层 global_stop_requested 约束：
  顶层 flow 调用 data.request_global_stop_if_done()；
  子 sequence 只读 data.is_global_stop_requested()；

global_stop_requested=0 时继续运行；
global_stop_requested=1 时进入退出或 drain 退出。
```

主 transaction driver 不再用每拍 pending-work 扫描决定正常退出。

注意：`success_prefix_uid >= main_trans_num` 是顶层发起 `global_stop_requested` 的条件。它的设计语义是：
只有每一笔主表 transaction 完成写回、commit/deq、active map 释放和状态清理后，才会推进
`status.success` 和 success prefix。因此当 success prefix 到达 `main_trans_num` 时，理论上所有主流程
要求写的都已经完成，相关运行期状态也应已经被清理。

但是，issue queue、redirect queue、raw monitor queue、active map、flushSb、PTW wait 等状态仍必须在
最终 `end_test_check()` 中再次一致性校验。如果这些状态在 success prefix 完成后仍有残留，说明状态推进
或清理逻辑存在 bug，应由最终检查报错。

### 3.1.1 `global_stop_requested` 与最终一致性检查

本文统一使用 `global_stop_requested` 表示顶层通知各常驻 sequence 进入收尾退出阶段。
不再混用 `global_stop` 和 `global_stop_requested` 两个名字。

触发条件：

```text
transaction_done = (success_prefix_uid >= main_trans_num)

if transaction_done:
  global_stop_requested = 1
```

统一管理落点：

```text
common_data_transaction 统一保存 global_stop_requested。
顶层 orchestration / real smoke flow 负责在检测到 transaction_done 后置位。
LSQ enqueue / issue / LSQ commit / redirect / responder sequence 只读取该 stop request。
单个子 sequence 不允许根据自己的 idle 状态置位 global_stop_requested。
```

建议 API 形态：

```text
function bit transaction_done();
  advance_success_prefix();
  return success_prefix_uid >= main_trans_num;

function void request_global_stop_if_done();
  if transaction_done():
    global_stop_requested = 1;

function bit is_global_stop_requested();
  return global_stop_requested;
```

第一版实现时，如果不想扩展复杂 stop manager，也应至少把 `global_stop_requested` 做成
`common_data_transaction` 中的公共运行期状态，由顶层 flow 单点置位、各 sequence 只读消费。

这里不把以下状态作为 `global_stop_requested` 的前置条件：

```text
all_issue_queues_empty
all_recovery_queues_empty
redirect_idle
flush_idle
active_maps_empty
raw_monitor_queues_empty
```

原因是这些状态理论上应随每笔 transaction 最终 success 一起清理。它们的职责是最终一致性校验，
不是阻塞 stop 请求的条件。

最终 `end_test_check()` 需要校验：

```text
success_prefix_uid == main_trans_num
load_issue_q / sta_issue_q / std_issue_q 为空
exception_event_q / ptw_wait_replay_q 为空
pending_redirect_drive_q 为空
redirect_drive_inflight == 0
active_redirect 无效
flush_in_progress == 0
flushsb_pending == 0
flushsb_waiting_empty == 0
flushsb_scheduled_pending == 0
uid_by_active_rob / uid_by_lq / uid_by_sq 为空
raw monitor queue 为空
```

如果当前 `common_data_transaction::end_test_check()` 已经覆盖这些条件，则保持现有机制；如果缺项，
后续实现时需要补齐。最终检查失败应报错，因为这表示 success prefix 推进与状态清理之间存在不一致。

### 3.2 事件驱动型主动 driver

事件驱动型主动 driver 也是测试框架主动 drive DUT input，但它的工作对象不是主表顺序 uid，而是
运行期事件队列。

适用对象：

- `memblock_redirect_dispatch_sequence`

Redirect drive 消费 `pending_redirect_drive_q`，负责把测试框架生成的 redirect payload drive 到
DUT。它不能因为当前队列为空就退出；否则如果 testcase 前半段没有 redirect，sequence 会提前结束，
后续 DUT 再产生 memory violation / exception 时，新的 redirect 将无人 drive。

统一原则：

```text
global_stop_requested 之前：
  常驻运行；
  pending_redirect_drive_q 有事件就 drive redirect；
  pending_redirect_drive_q 为空只表示本拍 idle，不表示 sequence 可以退出。

global_stop_requested 之后：
  等 pending_redirect_drive_q 为空；
  等 redirect_drive_inflight == 0；
  等 active_redirect 无效；
  drain 完后再退出。
```

文字伪代码：

```text
redirect_drive_loop:
  forever:
    if global_stop_requested:
      if pending_redirect_drive_q 为空
         且 redirect_drive_inflight == 0
         且 active_redirect 无效:
        break;

    if 可以从 pending_redirect_drive_q 取 redirect:
      drive_redirect_payload();
    else:
      drive_idle_once();
```

因此，`pending_redirect_drive_q`、`redirect_drive_inflight`、`active_redirect` 只用于判断 stop 后是否
drain 完，不能作为 testcase 中途正常退出条件。

### 3.3 被动响应型 sequence

被动响应型 sequence 的请求来自 DUT 或 monitor，测试框架事先不知道总请求数。

适用对象：

- `memblock_l2tlb_base_sequence`
- DCache/SBuffer memory responder sequence
- monitor service loop
- 其它只在 DUT request 到来后响应的 sequence

统一原则：

```text
有 request/response progress：idle_count = 0
没有 progress：idle_count++
global_stop_requested 之前：不因 idle_count 达到阈值退出
global_stop_requested 之后：idle_count 达到 idle_stop_cycle 后退出
```

被动响应型 sequence 继续使用 idle cycle 表达“当前无请求”，不强行套用 success prefix。
但在全局 testcase 尚未完成前，idle 不应导致 responder 提前退出；否则后续 DUT request 可能无人响应。
更合理的生命周期是：global stop 之前常驻，global stop 之后连续 idle 达到 `idle_stop_cycle` 再退出。

如果被动响应型 sequence 仍存在固定 `max_cycles`，后续应取消。被动 responder 不知道 DUT
请求总数，固定 max cycle 既可能过早退出，也可能掩盖真实 idle 语义。统一使用
`idle_stop_cycle` 或同类参数作为“global stop 后连续无 progress 再退出”的收尾方式。

文字伪代码：

```text
responder_loop:
  idle_count = 0;

  forever:
    if 有 DUT request 或完成 response progress:
      service_request_or_response();
      idle_count = 0;
    else:
      idle_count++;

    if data.is_global_stop_requested():
      if idle_count >= idle_stop_cycle:
        break;
    else:
      if idle_count >= idle_stop_cycle:
        打印 warning/debug；
        idle_count = 0；
        // 不退出，继续等待后续 DUT request。
```

因此，被动 responder 的 `idle_stop_cycle` 只表示“顶层允许结束后的 drain idle 周期”，不再表示
testcase 中途可独立退出。

## 4. Issue Flow 改造目标

目标：`memblock_lintsissue_dispatch_sequence::drive_dispatch_issue_loop()` 不再每拍调用
`issue_sched.has_pending_issue_work()`。

当前逻辑：

```systemverilog
issue_sched.route_all_ready_uids();
send_issue_cycle(cycle_idx, has_fire);
issue_sched.advance_issue_queue_delays();
pending_issue_work = issue_sched.has_pending_issue_work();

if (has_fire) begin
    idle_count = 0;
end else begin
    idle_count++;
    if (!pending_issue_work) begin
        if (idle_stop == 0 || idle_count >= idle_stop) begin
            break;
        end
    end
end
```

建议改造后：

```systemverilog
issue_sched.route_all_ready_uids();
send_issue_cycle(cycle_idx, has_fire);
issue_sched.advance_issue_queue_delays();

if (data.is_global_stop_requested()) begin
    break;
end

if (has_fire) begin
    idle_count = 0;
end else begin
    idle_count++;
    if (no_progress_warn_cycles != 0 &&
        idle_count >= no_progress_warn_cycles) begin
        `uvm_warning(get_type_name(),
                     $sformatf("no issue fire for %0d cycles before all transactions success: success_prefix=%0d main_trans_num=%0d",
                               idle_count,
                               data.dispatch_progress.success_prefix_uid,
                               data.main_trans_num))
        idle_count = 0;
    end
end
```

要点：

- Issue sequence 不单独调用 `advance_success_prefix()`，也不单独判断
  `success_prefix_uid >= main_trans_num`。
- 正常退出只看统一的 `data.is_global_stop_requested()`。
- `global_stop_requested` 由顶层 orchestration 根据公共 success prefix 单点置位。
- `has_pending_issue_work()` 不再参与每拍退出判断。
- `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES` 只表示 no-fire warning/debug 阈值。
- 真正兜底由 UVM timeout 或 testcase 顶层 timeout 负责。

## 5. 为什么不需要 `data.no_runtime_activity_for_issue()`

不建议新增：

```systemverilog
data.no_runtime_activity_for_issue()
```

原因是这个函数如果要判断 issue 是否真的没有潜在工作，必然又要扫描 issue queue、active
uid、replay/redirect 状态，性能问题会回到原点。

因此，issue sequence 的判断应保持简单：

```text
global_stop_requested=1 -> 退出
global_stop_requested=0 -> 继续跑
长时间没有 fire：报 uvm_warning/debug 作为 debug 线索，但不退出
```

## 6. 主动 driver no-progress 参数统一方向

本次改造直接删除主动主流程 driver 的旧退出参数，不做兼容：

```text
MEMBLOCK_DISPATCH_ISSUE_IDLE_STOP
MEMBLOCK_DISPATCH_ISSUE_START_TIMEOUT
MEMBLOCK_LSQENQ_IDLE_STOP
MEMBLOCK_LSQENQ_START_TIMEOUT
MEMBLOCK_LSQCOMMIT_IDLE_STOP
MEMBLOCK_LSQCOMMIT_START_TIMEOUT
```

采用公共 success / global stop 模型后，主动发射型 sequence 只保留一个统一 no-progress
debug 参数：

```text
连续 N 个周期没有成功 drive/fire/progress 时报告 uvm_warning/debug，用于定位卡住位置。
该参数不是正常退出条件。
```

统一参数：

```systemverilog
MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES
```

默认值应相对较大，例如：

```text
10000 或 60000
```

原因：

- 小默认值容易在 DUT 合法 backpressure、TLB miss、cache miss、PTW wait 时误报。
- 真正永久挂死仍由 UVM timeout 兜底。
- 该参数用于定位“哪个主动 sequence 很久没有成功推进”，不是用于裁剪仿真时长，也不默认导致 testcase fail。

## 7. Issue Flow 详细伪代码

```text
drive_dispatch_issue_loop:
  cycle_idx = 0;
  idle_count = 0;

  forever:
    调用 route_all_ready_uids:
      从公共状态表中把 ready uid 路由到 load/STA/STD issue queue；
      该函数内部可以继续使用有限扫描或未来 cursor 优化；

    调用 send_issue_cycle:
      从三个 issue queue 中选择候选；
      driver 真实 fire 后 mark_fired_items；
      has_fire=1 表示本拍至少一个 port 被 DUT 接收；

    调用 advance_issue_queue_delays:
      已入 queue 但还未 ready 的 item 递减 ready_cycle；

    检查 global_stop_requested:
      不在 issue sequence 内调用 advance_success_prefix；
      不在 issue sequence 内判断 success_prefix_uid >= main_trans_num；
      如果 data.is_global_stop_requested():
        break，issue sequence 正常结束；

    如果 has_fire:
      idle_count = 0；
    否则:
      idle_count++；
      如果 idle_count 达到 active sequence no-progress 阈值:
        报 uvm_warning/debug，打印 success_prefix/main_trans_num/queue size 等 debug 信息；
        idle_count = 0；

    cycle_idx++;
```

## 8. 对 redirect/replay/flush 的影响

使用公共最终 success prefix 后，issue sequence 不需要自己判断 redirect/replay/flush 是否已经
完全恢复。

原因：

- redirect/replay/flush 覆盖的 uid 不会变成 `status.success=1`。
- success prefix 只跨过真正最终完成的 uid。
- 只要还有任何 uid 未最终完成，`success_prefix_uid < main_trans_num`，主动 sequence 就继续运行。
- 如果恢复逻辑卡住，issue sequence 会持续运行并周期性报 no-fire warning/debug，最终由 UVM timeout 兜底。

## 9. 主 transaction driver 生命周期统一方案

`success_prefix_uid` 是最终完成条件。一个 uid 进入 success prefix 前，通常需要完成：

```text
LSQ enqueue -> issue -> writeback/pass -> ROB commit -> LQ/SQ deq / active map retire
```

因此不能只改 `memblock_lintsissue_dispatch_sequence`。如果 issue sequence 等待最终 success，
但 LSQ enqueue 或 LSQ commit 仍按 idle/start timeout 提前退出，后续 redirect/replay 重新 admission
或 commit/deq 时可能无人驱动，测试框架闭环会断开。

本方案要求三个主 transaction driver 同步采用同一生命周期：

```text
memblock_lsqenq_dispatch_sequence
memblock_lintsissue_dispatch_sequence
memblock_lsqcommit_dispatch_sequence
```

统一原则：

```text
global_stop_requested 之前：
  常驻运行；
  能 drive 就 drive；
  不能 drive 时只累计 no-progress 并打印 warning/debug；
  不允许因为 idle/start timeout 正常 break。

global_stop_requested 之后：
  退出或完成必要 drain 后退出。
```

### 9.1 LSQ Enqueue

`memblock_lsqenq_dispatch_sequence` 当前按 progress/idle 退出。该逻辑需要和 issue / LSQ commit
一起改，否则 redirect/replay 后需要重新 admission 时，LSQ enqueue sequence 可能已经退出。

修改方向：

```text
global_stop_requested 之前：
  不因为 progress/idle 为空正常退出；
  不因为 start_timeout 正常退出；
  no-progress 达到阈值只 warning/debug，不 break。

global_stop_requested 之后：
  可以退出。
```

注意：LSQ enqueue 负责 admission，可能早于最终 success 很多周期结束。若让它一直等最终
success，会保持 sequence 活着但不一定每拍 drive item。这个空转成本可以接受，因为它避免了
redirect/replay 恢复路径缺少 admission driver 的闭环风险。

### 9.2 LSQ Commit

`memblock_lsqcommit_dispatch_sequence` 是主 transaction driver，需要和 LSQ enqueue / issue 一样
在 `global_stop_requested` 前常驻运行。这里需要把三个问题拆开：

```text
1. sequence 是否可以正常退出；
2. 本拍 pendingPtr/commit 应该驱动哪些 uid。
3. start_timeout / idle timeout 是否可以作为正常退出。
```

当前 `has_pending_lsqcommit_work()` 的主要用途是判断 sequence 是否还有 pending work，进而配合
`idle_stop` 决定是否退出。它不是本拍 commit 候选选择的唯一来源。为了做这个退出判断，该函数
可能每拍从 `uid=0` 扫到 `main_trans_num`：

```systemverilog
for (int unsigned uid = 0; uid < data.main_trans_num; uid++) begin
    if (commit_handler.uid_is_commit_candidate(uid)) begin
        return 1'b1;
    end
end
```

这类全表扫描不适合作为每拍退出判断。改造后，LSQ commit sequence 不再通过
`has_pending_lsqcommit_work()` 判断能否正常退出；它只在顶层置位 `global_stop_requested` 后进入收尾。

本拍 commit 候选选择仍应交给 `lsq_commit_handler` 的 cursor/顺序推进逻辑，而不是通过
`has_pending_lsqcommit_work()` 每拍全表扫描决定。也就是说：

```text
global_stop_requested 之前：
  常驻运行；
  不因为 has_pending_lsqcommit_work() 为 0 退出；
  不使用 start_timeout；
  no-progress 只 warning/debug，然后清零计数继续等待。

本拍驱动：
  继续调用 commit_handler 的 cursor-based 选择逻辑；
  cursor 从当前可能 commit 的位置向后推进，不从 uid=0 每拍重扫完整主表。

global_stop_requested 之后：
  不再需要继续等待新的 commit activity；
  如果 flushSb 状态仍在收尾，则完成必要 drain；
  flushsb_pending == 0
  flushsb_waiting_empty == 0
  flushsb_scheduled_pending == 0
  之后退出。
```

no-progress 与 global stop 处理伪代码：

```text
if !data.is_global_stop_requested() && idle_count >= no_progress_warn_cycles:
  打印 warning/debug；
  idle_count = 0；
  continue；

if data.is_global_stop_requested():
  if flushSb 状态已空:
    break；
  else:
    继续 drive/等待 flushSb drain；
```

### 9.3 Redirect Drive

Redirect drive 是事件驱动型主动 driver，不属于主 transaction driver。它不能只看
`success_prefix_uid` 退出，也不能因为当前 redirect 队列为空就退出。

正确语义是：

```text
global_stop_requested 之前：
  常驻运行；
  pending_redirect_drive_q 有事件就 drive redirect；
  pending_redirect_drive_q 为空只表示本拍 idle。

global_stop_requested 之后：
  等 pending_redirect_drive_q 为空；
  等 redirect_drive_inflight=0；
  等 active_redirect 无效；
  drain 完后退出。
```

顶层 testcase 的最终结束由 real smoke / top flow 统一判断；redirect sequence 只负责在其生命周期内
持续消费 redirect 事件并完成 stop 后 drain。

## 10. Debug 输出建议

主动发射型 sequence 触发 no-progress warning/debug 时应至少打印：

```text
cycle_idx
idle_count
success_prefix_uid
main_trans_num
load_issue_q.size()
sta_issue_q.size()
std_issue_q.size()
flush_in_progress
active_redirect.valid
exception_event_q.size()
ptw_wait_replay_q.size()
```

这些字段用于快速判断卡在：

- 没有 route；
- issue queue 有项但 DUT 不 ready；
- redirect/replay/fault 恢复；
- writeback/commit/deq 后续闭环。

## 11. 当前 sequence 检查结论

本章只保留检查结论和章节索引，不重复前文具体方案。

| Sequence / 模块 | 当前结论 | 方案位置 |
| --- | --- | --- |
| `memblock_lintsissue_dispatch_sequence.sv` | 必须与 LSQ enqueue / LSQ commit 同步修改；删除每拍 `has_pending_issue_work()` 退出判断，global stop 前常驻。 | 第 4 节、第 7 节、第 9 节 |
| `memblock_lsqenq_dispatch_sequence.sv` | 必须与 issue / LSQ commit 同步修改；global stop 前不能按 progress/idle/start timeout 正常退出。 | 第 9.1 节 |
| `memblock_lsqcommit_dispatch_sequence.sv` | 必须与 LSQ enqueue / issue 同步修改；退出判断与 commit 候选选择拆开，global stop 前不能按 pending/start timeout 正常退出。 | 第 9.2 节 |
| `memblock_l2tlb_base_sequence.sv` | 不按公共 success 退出；global stop 前常驻响应，stop 后使用 `idle_stop_cycle` drain 退出。 | 第 3.3 节、第 13 节 |
| `memblock_redirect_dispatch_sequence.sv` | 事件驱动型主动 driver；global stop 前常驻消费 redirect，stop 后等待 queue/inflight/active redirect drain 完再退出。 | 第 3.2 节、第 9.3 节 |
| `memblock_dispatch_real_smoke_sequence.sv` | 已符合公共 success 退出规则，可作为主动发射型参考。 | 第 2 节、第 3.1 节 |

## 12. 实施建议

第一阶段必须同步修改三个主 transaction driver，不能只改 issue flow：

1. `memblock_lintsissue_dispatch_sequence::drive_dispatch_issue_loop()` 删除每拍 `has_pending_issue_work()` 正常退出判断。
2. `memblock_lsqenq_dispatch_sequence` 取消 progress/idle/start timeout 正常 break；global stop 前常驻。
3. `memblock_lsqcommit_dispatch_sequence` 取消 `has_pending_lsqcommit_work()` 和 start timeout 正常 break；global stop 前常驻，commit 候选继续走 cursor。
4. 三个 driver 的 no-progress 只 warning/debug，不作为正常退出条件。
5. 顶层由 `success_prefix_uid >= main_trans_num` 置位 `global_stop_requested`。
6. 同步更新 `load_sta_std_issue_flow.md`、LSQ enqueue/commit flow 文档和相关源码分析文档。

主动发射型参数统一要求：

1. 新增 `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES`。
2. 三个主 transaction driver 统一使用该参数作为 no-progress warning 阈值。
3. 删除旧 `MEMBLOCK_DISPATCH_ISSUE_IDLE_STOP`、`MEMBLOCK_LSQENQ_IDLE_STOP`、
   `MEMBLOCK_LSQCOMMIT_IDLE_STOP` 以及对应 `*_START_TIMEOUT`，不保留兼容入口。
4. 三个主 transaction driver 的 enable 默认值必须保守：`MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=0`、
   `MEMBLOCK_LSQENQ_SEQ_EN=0`、`MEMBLOCK_LSQCOMMIT_SEQ_EN=0`。真实 dispatch smoke 通过
   `seq/plus_cfg/tc_dispatch_real*.cfg` 显式打开，避免普通 testcase 没有主表时启动常驻
   active driver。
5. enable=0 时必须保持 idle 并返回，不允许调用父类随机 default sequence。原因是这些
   sequence 已经被 `tc_base` 挂到对应 agent 的 main_phase default sequence 上；如果关闭时
   继续 `super.body()`，普通 testcase 仍可能收到随机 LSQENQ/LINTSISSUE/LSQCOMMIT 激励。
6. LSQENQ 这类常驻主动 driver 在“本轮无候选”时也必须消费仿真时间。当前实现是发送
   all-idle xaction，让 driver 走一个 clocking block；不允许在 `forever` loop 中直接
   return 到下一轮，避免 main table ready 后无候选时 zero-time 空转。

## 13. Max Cycle 清理原则

后续凡是 sequence loop 存在固定 `max_cycles`，按以下规则处理：

```text
主动发射型：
  不使用 max_cycles 作为正常退出；
  正常退出使用 global_stop_requested；
  卡死保护使用 UVM timeout 和 no-progress warning/debug。

被动响应型：
  不使用 max_cycles 作为正常退出；
  global stop 前常驻响应；
  global stop 后使用 idle_stop_cycle 表示连续无 progress 后退出。
```

固定 `max_cycles` 的问题是：它既不表达最终 transaction 完成，也不表达 responder idle。
在 redirect/replay/flush 或 DUT backpressure 场景中，max cycle 很容易变成不稳定的经验值。

## 14. 其它测试用例构建与扫描优化方案

本章只记录前文未展开的其它优化点。已在前文展开的方案不在本章重复：

```text
Issue pending work 每拍扫描：
  见第 4 节、第 7 节、第 11 章。

LSQ commit pending work 每拍扫描：
  见第 9.2 节、第 11 章。
```

通用判断原则放在 `AI_DOC/project_management/mem_ut_test_framework_logic_build_rule.md`。本章只保留
TLB/sfence 和已合理结构的具体结论。

主表地址参考与违例场景构造方案已拆到独立文档，本次不做修改：

```text
AI_DOC/plan/test_framework/plan/undo/main_table_addr_reuse_window_plan_20260614.md、
```

### 14.1 TLB / sfence 失效扫描

涉及源码：

```text
mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv
  common_data_transaction::apply_sfence_invalidate()
  common_data_transaction::sfence_match_entry()
```

当前结论：

```text
sfence/hfence 对 tlb_entry_by_key 的遍历属于中频离散事件路径，不是每拍路径。
当前可以保留扫描当前 TLB entry 集合的实现。
```

暂不优化原因：

```text
1. 该扫描只在 sfence/hfence 事件到来时发生。
2. 扫描对象是当前已建立的 TLB entry，不是 main_trans_num 全表。
3. 引入二级索引会增加 entry 插入、删除、失效同步复杂度。
```

后续触发优化的条件：

```text
如果 TLB entry 数量显著变大，且 sfence/hfence 在压力测试中高频出现，
再考虑按 s2xlate/asid/vmid/vpn 或 flush 类型建立二级索引。
```

### 14.2 已经合理、不需要优化的结构

以下结构当前已经采用了更合适的数据结构，不建议改成扫描逻辑：

```text
common_data_transaction::resolve_uid_for_event()
  使用 ROB/LQ/SQ active map 反查 uid。

lsq_commit_handler
  使用 commit cursor 做顺序推进。
```

保留原因：

```text
monitor event 反查 uid 应使用 active map，不应全表扫 main_table。
顺序 commit 应使用 cursor，不应每拍从 uid=0 重扫。
当前优化重点是消掉仍存在的每拍 active-window/main_trans_num 扫描。
```
