# [不支持] Recovery Frontier Candidate 仲裁方案

## 0. 不支持结论

本方案归档为“不支持”，不建议按本文实现。

不支持原因：

- 本方案将 redirect/replay 先放入 recovery candidate queue，再等待 commit frontier 命中对应 uid 后执行。
- 该思路不适合 memblock 的 `memoryViolation/loadReplay` redirect。结合 XiangShan Scala 源码，memoryViolation 是后端检测到访存违例后的微架构恢复信号，触发后应立即进入 redirect/loadReplay 恢复路径，不需要等待 ROB commit。
- 如果对 memoryViolation redirect 增加 commit-frontier 等待，会让测试框架行为偏离 DUT：DUT 已经准备恢复，而测试框架仍允许该 redirect 悬挂到 commit cursor，可能掩盖或制造错误时序。
- exception、flushPipe、replayInst 属于 ROB 精确提交类 flush，才需要等 ROB head/commit 边界。它们和 memoryViolation redirect 的触发源、精确性要求和恢复时机不同，不能用同一套全局候选队列统一处理。

本文仅作为历史方案归档。后续如果需要建模 ROB 精确提交类 flush，应单独立项；memblock memoryViolation redirect 应继续走 monitor 采集后即时设置 active redirect / flush 的路径。

特别修正：本文中“transaction_done 必须包含 recovery idle”的方向不应落地。`transaction_done()` 仍只判断 success 前缀是否完成；recovery candidate queue、exception queue、PTW wait replay、active redirect、redirect drive 和 flush 状态是否 idle，应放入 `end_test_check()` 作为 testcase 结束时的检查项。

## 0. 背景

当前 mem_ut 测试框架已经实现 batch 级 redirect-first 仲裁：

```text
raw monitor event
  -> collect_monitor_event_batch()
  -> dispatch_monitor_batch_handler::process_monitor_event_batch()
  -> 同一 service batch 内选 oldest redirect
  -> 被 redirect 覆盖的 pass/fault/replay drop
  -> redirect/replay/fault 进入 exception_event_q
  -> exception_redirect_replay_handler::process_pending_events()
```

这个机制能解决“同一 service batch 内 redirect 晚于 writeback/replay/fault 落状态”的问题，但仍有一个更长时间窗口的问题：

```text
t0: uid=20 先触发 redirect 或 STA replay
t1: 测试框架立即处理 uid=20 recovery
t3: uid=10 后续才触发 redirect
t4: uid=10 redirect flush 覆盖 uid=20 之后的所有动态实例
```

这时 uid=20 的 redirect drive 或 STA replay 重发从最终执行流看是多余的。功能上通常不会错，因为后续 older redirect 会再次 flush/reissue；但会造成重复恢复、无效重发、状态窗口复杂化，并增加 debug 难度。

本方案把 redirect/replay 先保存为 recovery candidate，等 commit frontier 推进到 candidate uid 时才真正执行 recovery。等待期间不暂停 LSQ admission、issue route、issue queue select/fire、ROB commit 或 LQ/SQ deq，用于模拟 DUT 在 older redirect 真正执行前仍继续流动的行为。

## 1. 问题本质

redirect 和 STA replay 都是 commit 前由 DUT 输出反馈发现的 recovery 事件：

- redirect 来自 `memoryViolation` 等控制输出，表示需要恢复到某个 ROB 位置。
- STA replay 来自 IQ feedback failed，表示该 STA target 需要后端控制下重新发射。

它们不能等“本 uid 自己 commit”后再触发，因为：

```text
STA replay:
  必须先 replay STA
  才可能重新 pass/writeback
  才可能进入 ROB commit

redirect:
  必须先 flush/reissue
  才可能让被覆盖路径重新走到 commit
```

所以“等本 uid commit 后再 replay/redirect”会形成闭环，逻辑不成立。

真正需要等待的是：

```text
该 recovery event 前面更老的 uid 是否已经收敛。
```

如果更老 uid 尚未完成，它们未来仍可能产生更老 redirect。此时立即执行当前年轻 recovery，可能被未来 older redirect 覆盖。

这里的等待只影响 recovery candidate 是否 eligible，不阻塞更年轻请求继续流动。commit frontier 是 recovery 执行门槛，不是 admission/issue/commit 的全局 hold 边界。

## 2. 设计目标

新增 recovery frontier candidate 仲裁：

```text
recovery event 早采集
recovery event 先进入 candidate queue
commit cursor/frontier 扫到 event uid 时先检查 candidate
有 candidate 则先处理 recovery，不提交该 uid
再真正执行 redirect/replay
```

核心目标：

1. 保留 monitor 早采集，不丢 DUT 输出事件。
2. 不等本 uid 自己 commit，避免 replay/redirect 死循环。
3. 当 commit frontier 到达 recovery event uid 时，先处理 candidate，避免该 uid 先 success 后再 recovery。
4. 等待期间不暂停 LSQ admission、issue route、issue queue select/fire、ROB commit 或 LQ/SQ deq，保持 DUT 行为模拟。
5. older redirect 真正执行后，再通过 redirect flush 清理被覆盖 uid 的状态和 candidate。
6. redirect 仍高于 replay；同一个可执行窗口内先处理 ROB oldest redirect。

## 3. 新增概念

### 3.1 Recovery Candidate Queue

新增队列：

```systemverilog
memblock_wb_event_t recovery_candidate_q[$];
```

语义：

```text
保存已经由 monitor 采集并 normalize 的 redirect/replay recovery event。
redirect 和 replay 不立即执行，只作为 candidate 等待 recovery frontier。
fault 是否延迟需要单独决策，第一版建议 fault 仍可沿用当前逻辑落状态，因为 fault 本身会阻止 success。
```

第一版建议只把 redirect 和 replay 纳入 frontier 延迟仲裁：

```text
redirect:
  必须延迟到 frontier，避免 younger redirect 被 older redirect 覆盖。

STA replay:
  必须延迟到 frontier，避免 younger replay 重发后又被 older redirect 覆盖。

fault:
  当前已经有 redirect-first 防护，且 fault 会阻断 commit success。
  第一版不强制迁入 frontier，可作为后续增强。
```

### 3.2 Recovery Frontier / Candidate Tracking

新增跟踪状态：

```systemverilog
bit            recovery_candidate_valid;
memblock_uid_t recovery_frontier_uid;
memblock_rob_key_t recovery_frontier_rob_key;
```

语义：

```text
当存在 pending redirect/replay candidate 时，记录最老 pending candidate 的 uid，作为 debug、no-progress 检查和 eligibility 计算的辅助信息。
同时记录该 candidate 的 ROB key，便于日志、oldest redirect 比较和 stale candidate 诊断。
recovery_frontier_uid 不用于阻塞后续请求流动。
recovery_frontier_rob_key 也不用于阻塞后续请求流动。
candidate 是否可执行由 commit cursor/frontier 是否扫到 candidate.uid 决定。
```

易懂约束：

```text
recovery_frontier_uid 和 recovery_frontier_rob_key 只是“当前最老 pending recovery 的记录标签”，
用来打印日志、定位卡住原因、辅助 debug。

它们不是“暂停开关”，也不是“从这个 uid 开始全部停住”的边界。

举例：
  如果 recovery_frontier_uid = 10，表示 uid=10 有一个 redirect/replay candidate 正在等待 commit frontier。
  这不代表 uid=10 之后的请求都要停住。
  uid=11、uid=12 仍然可以继续 LSQ admission、route 到 issue queue、被 issue select/fire，ROB commit 也可以继续处理 uid=10 前面的更老 uid。

真正需要停下来的时刻只有一个：
  commit cursor 正好扫到 uid=10，且发现 uid=10 仍有 recovery candidate。
  这时只是不允许 uid=10 自己被普通 commit/success，必须先处理 uid=10 的 recovery。

如果后面出现更老的 redirect，例如 uid=5 redirect，
  uid=5 redirect 真正执行后会通过 redirect flush 清理 uid=5 之后尚未 success 的旧动态实例，
  包括 uid=10/uid=11/uid=12 的旧状态和 pending candidate。
```

需要同步约束的实现点：

```text
commit flow 到达 candidate.uid 时必须先处理 candidate。
candidate.uid 不允许先 success 后再 recovery。
因此第一版不需要支持 success_prefix 回滚，也不要求 redirect flush 清理已经 success 的 uid。
```

## 4. 触发时机

### 4.1 当前触发方式

当前逻辑：

```text
redirect event:
  push_feedback_event()
  -> exception_event_q
  -> process_pending_events()
  -> request_redirect_flush()
  -> push_redirect_drive()
  -> apply_redirect_flush()

STA replay event:
  push_feedback_event()
  -> exception_event_q
  -> process_pending_events()
  -> mark_replay_pending()
  -> 后续 route replay target
```

特点：

```text
只要当前 batch/queue 内没有更老 redirect 阻挡，就会尽快触发。
不能等待未来几拍才到达的 older redirect。
```

### 4.2 新触发方式

新逻辑：

```text
redirect/replay event 被采集后：
  normalize
  push recovery_candidate_q
  update recovery_frontier_uid for tracking only
  不立即 request_redirect_flush
  不立即 push_redirect_drive
  不立即设置 active_redirect
  不立即 mark_replay_pending

每拍 service recovery:
  advance_success_prefix()
  process_pending_events() 只推进 active redirect / PTW wait replay
  candidate 本体由 commit cursor/frontier 到达对应 uid 时触发
```

这里的“等 commit”不是等 event 自己 commit 完成，也不是要求该 uid 已经满足普通
`uid_is_commit_candidate()`。真正含义是：

```text
commit_cursor_uid == event.uid && recovery_candidate_q has event.uid
```

含义：

```text
event.uid 前面的 uid 已经可以继续按正常 commit/deq 收敛。
当 commit cursor/frontier 扫到 event.uid 时，说明它已经是当前提交前沿。
此时必须先处理 recovery candidate，阻止该 uid 继续走普通 commit 判定并被置 success。
candidate 检查必须早于 uid_is_commit_candidate() 检查。
```

典型例子：

```text
t0: uid=10 先触发 redirect/replay，进入 recovery_candidate_q。
t1: uid=0..4 正常 writeback/commit/success，commit_cursor_uid 推进到 5。
t2: uid=5 也触发 redirect，并进入 recovery_candidate_q。
t3: commit cursor 扫到 uid=5。
t4: 先检查 has_recovery_candidate_for_uid(5)，命中 uid=5 redirect candidate。
t5: 不要求 uid=5 已经满足 uid_is_commit_candidate()，直接执行 uid=5 redirect recovery。
t6: uid=5 redirect apply flush 后，清理 uid>5 的未 success 动态实例和被覆盖 candidate。
t7: uid=10 candidate 被 uid=5 redirect 覆盖并 drop，不再等待 uid=10 到达 commit frontier。
```

等待期间 younger uid 不受 recovery candidate 阻塞：

```text
LSQ admission:
  按原 admission 规则继续。

issue route:
  按 active/enq/issue_ready/replay_target 等原 route 规则继续。

issue queue select/fire:
  按 ready_cycle、send_pri、ROB age、pipe num 等原规则继续。

ROB commit / LQ/SQ deq:
  按原 commit/deq 规则继续；
  但 commit flow 选到 candidate.uid 时必须先处理 candidate，
  本轮不提交该 uid，不设置 success，不推进该 uid 之后的 success_prefix。
```

## 5. 等待期间请求流动规则

### 5.1 LSQ Admission

建议：

```text
不因为 recovery candidate 暂停 LSQ admission。
memblock_lsqenq_dispatch_sequence / lsq_ctrl_model 继续按原逻辑选择 admission uid。
```

原因：

```text
candidate 只是 recovery 执行门槛。
DUT 在 older redirect 真正执行前仍会接收和推进年轻请求，测试框架也应继续模拟。
```

逻辑伪代码：

```text
选择 LSQ admission uid 时:
  不检查 recovery_frontier_uid；
  不检查 recovery_candidate_q；
  按原 active/enq/resource/ordering 条件选择；
  如果后续 older redirect 覆盖该 uid，由 apply_redirect_flush 清理。
```

### 5.2 Issue Route

建议：

```text
不因为 recovery candidate 阻止 uid route 到 LOAD/STA/STD issue queue。
issue_queue_scheduler::is_uid_route_ready() 不增加 recovery frontier 阻塞条件。
```

逻辑伪代码：

```text
route_all_ready_uids 扫描 uid:
  不因为 pending recovery candidate 跳过 uid；
  按 active/enq/issue_ready/replay_target 等原规则 route；
  若后续 redirect flush 覆盖该 uid，清理 issue queue 和 route 状态。
```

### 5.3 Issue Queue Select / Fire

建议：

```text
不因为 recovery candidate 阻止已经入队的 item 被 select/fire。
issue_queue_scheduler::item_is_eligible() 不增加 recovery frontier 阻塞条件。
```

逻辑伪代码：

```text
选择 issue candidate 时:
  遍历 LOAD/STA/STD queue item；
  不因为 item.uid >= recovery_frontier_uid 跳过；
  按 ready_cycle、send_pri、ROB age、pipe num 原规则选择；
  已经 fire 到 DUT pipeline 的请求继续等待 DUT 返回。
```

### 5.4 已经 Fire 到 DUT Pipeline 的请求

不能撤回。

处理策略：

```text
已经 fire 的请求继续等待 DUT 返回。
返回的 writeback/IQ feedback/memoryViolation 仍会被 monitor 采集。
如果它们被后续 older redirect 覆盖：
  由 redirect flush 清理或 drop stale event。
如果它们帮助更老 uid commit/deq：
  允许帮助 commit frontier 走到 candidate.uid。
```

原因：

```text
DUT pipeline 内已经接受的请求，测试框架不能从接口层撤回。
只能通过后续 redirect flush / replay_seq / issue_epoch / active map 过滤迟到事件。
```

### 5.5 ROB Commit / LQ/SQ Deq

建议：

```text
不因为 recovery candidate 全局暂停 ROB commit 或 LQ/SQ deq。
但当 commit cursor/frontier 扫到某个 uid 时，必须先检查该 uid 是否存在 recovery candidate。
```

原因：

```text
frontier 仲裁依赖更老 uid 继续收敛，所以不能全局暂停 commit/deq。
但 recovery candidate 所属 uid 不能先于自己的 recovery 被置 success。
否则 success_prefix 会把该 uid 当成不可回滚的提交完成项。
```

逻辑伪代码：

```text
选择 commit/deq uid 时:
  不检查 recovery_frontier_uid；
  取当前 commit cursor/frontier 对应 uid；
  先检查该 uid 是否存在 recovery candidate；
  如果存在 recovery candidate:
    不设置 rob_commit；
    不设置 success；
    不推进该 uid 之后的 success_prefix；
    先触发该 uid 的 recovery；
    跳出本轮 commit batch；
  如果该 uid 没有 recovery candidate:
    再按原 ROB/LQ/SQ 条件判断该 uid 是否可 commit/deq；
    按原逻辑 commit/deq 并推进 success_prefix。
```

## 6. Candidate 仲裁规则

### 6.1 Eligible 条件

candidate 可执行条件：

```text
commit cursor/frontier 扫到 candidate.uid
```

如果 candidate 无法解析 uid，则在入队前 drop，不进入 candidate queue。

如果 candidate 的 uid 小于 success_prefix_uid：

```text
说明该 uid 已经被错误地 success/retire 在 candidate 之前。
第一版不支持 success_prefix 回滚，这属于调度顺序错误；
应 uvm_error 或 fatal，提示 commit flow 绕过了 recovery candidate 检查。
```

为什么不让 `success_prefix_uid` 越过 candidate 后再处理：

```text
success_prefix_uid 表示已经成功提交的连续前缀。
如果它越过 candidate.uid，说明 candidate.uid 已经被当作 success。
后续再执行 redirect/replay 会破坏“success 不回滚”的第一版假设。
因此 commit flow 到达 candidate.uid 时必须先处理 candidate，而不是先提交再 recovery。
```

实现建议：

```text
candidate 可以执行的第一条件是 commit_cursor_uid == candidate.uid；
candidate 检查必须在 uid_is_commit_candidate() 之前；
真正执行前仍必须再次检查 event 快照是否仍属于当前动态实例。
redirect 检查 ROB key/dynamic_epoch/active 或可清理状态；
STA replay 检查 issue_epoch/replay_seq/target_dispatched。
```

### 6.2 Redirect 和 Replay 优先级

同一个 frontier 上如果同时有 redirect 和 replay：

```text
redirect 优先。
```

原因：

```text
redirect 是全局恢复，会覆盖后续动态实例；
replay 是局部 target 重新发射。
先 replay 再 redirect 仍可能造成 replay 多余。
```

同一轮多个 redirect：

```text
选择 ROB oldest redirect。
被该 redirect 覆盖的 younger redirect/replay 从 candidate queue 中 drop。
未覆盖的 candidate 保留。
```

### 6.3 Replay 执行规则

当 STA replay candidate eligible 且没有 redirect 抢占：

```text
重新检查 issue_epoch / replay_seq / target_dispatched。
检查通过才 mark_replay_pending。
检查失败则 drop，并 warning/debug。
```

原因：

```text
candidate 可能等待了多个 cycle。
期间 uid 可能被 redirect/reissue 改变 dynamic_epoch，或者 replay_seq 已更新。
必须在真正执行 replay 前再次确认它仍属于当前动态实例。
```

### 6.4 Redirect Apply Flush 清理规则

older redirect 真正执行后，覆盖范围内的 younger uid 必须清理：

```text
active map / pass map / fault map / success map；
issue queue item / route state / replay target / replay pending；
LSQ admission、LQ/SQ deq 相关软件状态；
recovery_candidate_q 中被覆盖的 redirect/replay candidate；
迟到 monitor event 的匹配状态。
```

如果当前实现存在“已经 success 的 uid 不允许 flush 清理”的保护：

```text
第一版不调整该保护。
因为 commit flow 会在 candidate.uid 处先处理 recovery，不允许 candidate.uid 先 success。
如果仍出现 redirect flush 命中 success uid，说明 commit frontier candidate 检查被绕过，应报错。
```

## 7. 函数级修改建议

本章按 plan 规则描述新增/修改函数。每个关键函数都包含：函数目的、输入、输出/副作用、源码级伪代码、中文文字伪代码。中文文字伪代码不是简单翻译函数名，而是说明该逻辑在 recovery frontier feature 中承担的职责、每个关键分支为什么这样走、调用子函数的输入来源和状态副作用。

### 7.1 `common_data_transaction` 新增字段

字段目的：保存“已经由 monitor 采集到、但尚未允许真正执行”的 redirect/replay recovery event，并记录最老 pending candidate 供 debug 和 no-progress 诊断。

字段功能：

- `recovery_candidate_q`：保存 pending redirect/replay candidate。它是事件队列，不是执行队列；入队不代表已经 drive redirect 或 mark replay。
- `recovery_candidate_valid`：表示当前是否存在 pending candidate。
- `recovery_frontier_uid`：记录最老 pending candidate 的 uid，只用于 tracking/debug，不用于阻塞 uid。
- `recovery_frontier_rob_key`：记录最老 pending candidate 的 ROB key，只用于日志、oldest 比较和 stale 诊断。

输入：来自 `dispatch_monitor_batch_handler` normalize 后的 `memblock_wb_event_t`。

输出/副作用：新增公共队列和 tracking 字段；后续 `lsq_commit_handler` 查询 candidate，`exception_redirect_replay_handler` 消费 candidate，redirect flush 清理被覆盖 candidate。

源码级伪代码：

```text
字段定义:
  memblock_wb_event_t recovery_candidate_q[$]
  bit recovery_candidate_valid
  memblock_uid_t recovery_frontier_uid
  memblock_rob_key_t recovery_frontier_rob_key

reset_all_tables():
  recovery_candidate_q.delete()
  recovery_candidate_valid = 0
  recovery_frontier_uid = 0
  recovery_frontier_rob_key = 0

end_test_check():
  if recovery_candidate_q.size() != 0:
    uvm_error
```

中文文字伪代码：

```text
这组字段在当前 feature 中承担“候选 recovery 暂存区”的角色。
monitor 采集到 redirect/replay 后，不再立即触发 redirect drive 或 replay pending，而是先把完整 event 存入 recovery_candidate_q。
reset_all_tables 初始化新 testcase 时必须清空队列和 tracking 字段，避免上一个 testcase 的 candidate 污染新测试。
end_test_check 结束时必须检查队列为空；如果不为空，说明存在采集到但没有被 commit frontier 消费或没有被 redirect flush drop 的 recovery event，应报错。
recovery_frontier_uid 和 recovery_frontier_rob_key 只用于观察当前最老 candidate，不允许被 admission、route、issue 或 commit 当成 hold 边界。
```

### 7.2 `common_data_transaction::push_recovery_candidate()`

函数目的：把 monitor 已经采集并 normalize 的 redirect/replay event 放入 recovery candidate queue，替代旧的 `push_feedback_event()->exception_event_q` 即时恢复路径。

输入：`wb_event`，来源于 `dispatch_monitor_batch_handler` 对 raw monitor event 的 normalize 结果。

输出/副作用：可能向 `recovery_candidate_q` 入队；更新 `recovery_candidate_valid/recovery_frontier_uid/recovery_frontier_rob_key`；对无法定位 uid 或已越过 success prefix 的 event 打 warning/error/fatal。

源码级伪代码：

```text
push_recovery_candidate(wb_event):
  if normalize_feedback_event(wb_event, normalized_event) 返回 false:
    warning/drop
    return

  if normalized_event 不是 redirect 且不是 replay:
    fatal 或 warning/drop
    return

  uid = normalized_event.uid
  if uid < dispatch_progress.success_prefix_uid:
    error/fatal: candidate 已经落在 success 前缀内
    return

  if normalized_event.has_rob == 0:
    warning/drop
    return

  recovery_candidate_q.push_back(normalized_event)
  update_recovery_frontier_tracking()
```

中文文字伪代码：

```text
该函数负责把“早采集但不能早执行”的 recovery event 转成 pending candidate。
先调用 normalize_feedback_event：这个 helper 根据 ROB/LQ/SQ active map 解析 uid，补齐 issue_epoch/replay_seq 等快照字段；如果解析失败，说明 event 已经过期或无法和当前动态实例对应，函数直接 drop，不入队。
随后检查 event 类型，只允许 redirect/replay 进入 candidate queue；fault/normal pass 不属于本函数职责，避免把普通状态更新误放进 recovery frontier 机制。
接着检查 uid 是否已经小于 success_prefix_uid：success_prefix_uid 表示连续完成提交的前缀，如果 candidate 落在这个范围，说明 commit flow 已经绕过了 recovery candidate，这违反 success 不回滚假设，需要报错。
最后把 normalized_event 追加到 recovery_candidate_q，并调用 update_recovery_frontier_tracking 更新 debug 用的最老 candidate 信息；入队本身不设置 active_redirect，不 push_redirect_drive，也不 mark_replay_pending。
```

### 7.3 `common_data_transaction::update_recovery_frontier_tracking()`

函数目的：维护当前最老 pending recovery candidate 的 debug/tracking 字段。

输入：`recovery_candidate_q` 当前内容。

输出/副作用：更新 `recovery_candidate_valid`、`recovery_frontier_uid`、`recovery_frontier_rob_key`；不改变 admission/issue/commit 判断结果。

源码级伪代码：

```text
update_recovery_frontier_tracking():
  if recovery_candidate_q.size() == 0:
    recovery_candidate_valid = 0
    recovery_frontier_uid = 0
    recovery_frontier_rob_key = 0
    return

  selected = recovery_candidate_q[0]
  foreach candidate in recovery_candidate_q:
    if candidate 是 redirect 且 selected 不是 redirect:
      selected = candidate
    else if candidate 和 selected 都有 ROB 且 candidate ROB older:
      selected = candidate
    else if candidate.uid < selected.uid:
      selected = candidate

  recovery_candidate_valid = 1
  recovery_frontier_uid = selected.uid
  recovery_frontier_rob_key = selected.rob_key
```

中文文字伪代码：

```text
该函数只维护观察字段，不参与阻塞策略。
每次有 candidate 入队、出队或被 redirect flush 删除后，都要重新扫描 recovery_candidate_q，而不是只在第一次入队时设置一次。
如果 candidate queue 为空，就清掉 valid、uid 和 rob_key，表示当前没有等待 commit frontier 的 recovery。
如果 queue 非空，就在队列里重新选择当前最老、最值得关注的 candidate。优先按 ROB oldness 选择；在本方案第一版 uid 顺序等价 ROB commit 顺序的前提下，uid 更小通常也代表更老。
举例：uid=10 先触发 redirect/replay 时，recovery_frontier_uid 会被更新为 10；后面 uid=8 又触发 redirect/replay 并入队后，update_recovery_frontier_tracking 会重新扫描整个队列，发现 uid=8 比 uid=10 更老，于是把 recovery_frontier_uid 从 10 更新为 8。
这个更新只改变 debug/tracking 记录，不会因为 frontier 从 10 变成 8 就暂停 uid>=8 的 admission、route、issue 或 commit。真正的 recovery 执行仍然只在 commit cursor 扫到对应 uid 时发生。
```

### 7.4 `common_data_transaction::has_recovery_candidate_for_uid()`

函数目的：给 commit frontier 提供精确查询：当前 commit cursor 对应 uid 是否已经有 pending recovery candidate。

输入：`uid`，由 `lsq_commit_handler` 当前 commit cursor 给出。

输出/副作用：返回 bit；不修改队列或状态。

源码级伪代码：

```text
has_recovery_candidate_for_uid(uid):
  foreach candidate in recovery_candidate_q:
    if candidate.uid == uid:
      return 1
  return 0
```

中文文字伪代码：

```text
该函数是 commit frontier 的局部查询 helper。
它只回答“当前 uid 是否有 candidate”，不会检查 uid 之后的 younger candidate，也不会使用 recovery_frontier_uid 做范围阻塞。
如果返回 1，lsq_commit_handler 必须停止普通 commit 流程，先调用 recovery handler 处理该 uid；如果返回 0，commit handler 才能继续检查 uid_is_commit_candidate。
该函数无状态副作用，避免查询本身改变 candidate queue 顺序。
```

### 7.5 `common_data_transaction::pop_recovery_candidate_for_uid()`

函数目的：当 commit cursor/frontier 扫到某个 uid 后，从 candidate queue 中取出该 uid 应执行的 recovery event。

输入：`uid`，由 commit frontier 命中的 blocked recovery uid 给出。

输出/副作用：返回是否成功取到 candidate；通过输出参数返回 selected event；从 `recovery_candidate_q` 删除 selected；可能删除同 uid 已被 redirect 覆盖的 replay candidate；更新 tracking 字段。

源码级伪代码：

```text
pop_recovery_candidate_for_uid(uid, selected):
  same_uid_candidates = recovery_candidate_q 中 uid 相同的所有 candidate
  if same_uid_candidates 为空:
    selected = empty event
    return 0

  if same_uid_candidates 中存在 redirect:
    selected = ROB oldest redirect
    从 recovery_candidate_q 删除 selected
    删除同 uid 中被 selected 覆盖或语义上已无效的 replay candidate
    update_recovery_frontier_tracking()
    return 1

  selected = same_uid_candidates 中最早 replay candidate
  从 recovery_candidate_q 删除 selected
  update_recovery_frontier_tracking()
  return 1
```

中文文字伪代码：

```text
该函数负责把“等待状态的 candidate”转成“本轮准备执行的 recovery”。
先只筛选当前 uid 的 candidate，保证 commit cursor 扫到 uid=5 时不会误取 uid=10 的 candidate。
如果同 uid 内存在 redirect，就优先选 redirect，因为 redirect 是全局恢复，会覆盖 replay 这种局部重发；选中 redirect 后，同 uid 的 replay candidate 已经没有独立执行价值，应删除或在后续 redirect cleanup 中清掉。
如果没有 redirect，才选择 replay candidate，交给 replay 逻辑做 issue_epoch/replay_seq/target_dispatched 重检。
每次删除 candidate 后都调用 update_recovery_frontier_tracking，让 debug 字段反映剩余 pending candidate 的最老项。
```

### 7.6 `common_data_transaction::drop_candidates_covered_by_redirect()`

函数目的：redirect 真正 apply flush 后，删除被该 redirect 覆盖的 younger redirect/replay candidate，避免被覆盖路径后续再次执行 recovery。

输入：`redirect`，来自当前 `active_redirect`。

输出/副作用：遍历并删除 `recovery_candidate_q` 中被 `redirect` 覆盖的 candidate；打印 drop 原因；更新 tracking 字段。

源码级伪代码：

```text
drop_candidates_covered_by_redirect(redirect):
  if !redirect.valid:
    fatal

  for idx 从 recovery_candidate_q.size()-1 downto 0:
    candidate = recovery_candidate_q[idx]
    if candidate.has_rob == 0:
      warning/drop 或保留并等待动态实例重检
      continue

    if rob_order_util::rob_need_flush(candidate.rob_key, redirect):
      uvm_info 或 uvm_warning 打印 uid/rob/source/原因
      recovery_candidate_q.delete(idx)

  update_recovery_frontier_tracking()
```

中文文字伪代码：

```text
该函数在 redirect flush cleanup 阶段执行，负责清理 candidate queue，而不是清状态表。
它从队尾向队头遍历，避免删除队列元素时影响还没扫描的下标。
对每个 candidate，先确认有 ROB key；有 ROB key 时调用 rob_order_util::rob_need_flush 判断该 candidate 的 ROB 是否在 redirect 覆盖范围内。
如果被覆盖，就删除 candidate 并记录 uid、ROB、source 和 drop 原因。这样 uid=5 redirect 执行后，uid=10 的 redirect/replay candidate 不会继续等待自己的 commit frontier。
最后调用 update_recovery_frontier_tracking，保证队列清理后 tracking 字段不会指向已经删除的 candidate。
```

### 7.7 `lsq_commit_handler::select_rob_commit_batch()` / `memblock_lsqcommit_dispatch_sequence::send_lsqcommit_cycle()`

函数目的：把 recovery candidate 的执行门槛放到真实 ROB commit frontier 上，确保 candidate uid 不会先 success 后 recovery。

输入：`commit_cursor_uid`、`recovery_candidate_q`、当前 uid 的 status 表、LSQ commit driver cycle。

输出/副作用：生成本轮可提交的 `commit_uids`；如果当前 cursor uid 有 candidate，则输出 `blocked_recovery_uid_valid/blocked_recovery_uid`；不提交 blocked uid；由 LSQ commit sequence 调用 recovery handler。

源码级伪代码：

```text
select_rob_commit_batch(commit_uids, blocked_recovery_uid_valid, blocked_recovery_uid):
  commit_uids.delete()
  blocked_recovery_uid_valid = 0

  if issue_blocked_by_global_flush():
    return

  advance_commit_cursor_past_done()
  uid = commit_cursor_uid

  while uid < main_trans_num && commit_uids.size < MEMBLOCK_COMMIT_WIDTH:
    if status[uid].success:
      uid++
      continue

    if data.has_recovery_candidate_for_uid(uid):
      blocked_recovery_uid = uid
      blocked_recovery_uid_valid = 1
      return

    if !uid_is_commit_candidate(uid):
      return

    commit_uids.push_back(uid)
    uid++

send_lsqcommit_cycle():
  build_lsqcommit_xaction(commit_uids, has_commit, blocked_recovery_uid_valid, blocked_recovery_uid)
  start_item/finish_item(tr)

  if blocked_recovery_uid_valid:
    recovery_handler.process_recovery_candidate_for_uid(blocked_recovery_uid)
    return

  if has_commit:
    commit_handler.mark_rob_commit_batch(commit_uids)
```

中文文字伪代码：

```text
这段逻辑负责把“候选 recovery 是否可以执行”的判断挂到真实 commit cursor 上。
select_rob_commit_batch 先跳过已经 success 的连续 uid，找到当前 commit frontier。
对当前 uid，必须先调用 has_recovery_candidate_for_uid：这个 helper 只查询当前 uid 是否有 pending candidate。如果返回 1，说明该 uid 不能继续普通 commit，函数记录 blocked_recovery_uid 并立即返回，不再调用 uid_is_commit_candidate，也不继续扫描更年轻 uid。
如果当前 uid 没有 candidate，才调用 uid_is_commit_candidate 检查 active/writeback/pass/required target/ROB commit/fault/replay/redirect 等普通提交条件；不满足则停止本轮 batch。
LSQ commit sequence 拿到 blocked_recovery_uid 后，应发 idle 或空 commit xaction 保持接口节奏，然后调用 process_recovery_candidate_for_uid。该调用会真正启动 redirect 或 replay。只有没有 blocked recovery 时，才允许 mark_rob_commit_batch 把 commit_uids 标记 rob_commit 并进一步 try_retire_committed_uid。
```

### 7.8 `exception_redirect_replay_handler::process_recovery_candidate_for_uid()`

函数目的：只处理 commit frontier 指定 uid 的 recovery candidate，把 pending candidate 转成真正执行的 redirect 或 replay。

输入：`uid`，来自 `lsq_commit_handler` 的 blocked recovery uid；读取 `recovery_candidate_q`、`active_redirect`、PTW wait queue。

输出/副作用：可能设置 `active_redirect`、`flush_in_progress`、`issue_freeze_ack`，可能向 `pending_redirect_drive_q` 入队，可能调用 replay pending 更新，可能 drop stale candidate。

源码级伪代码：

```text
process_recovery_candidate_for_uid(uid):
  service_ptw_wait_replay()
  advance_active_redirect()

  if active_redirect.valid:
    return

  if !data.pop_recovery_candidate_for_uid(uid, selected):
    warning/drop
    return

  if selected 是 redirect:
    data.request_redirect_flush(selected.redirect)
    data.push_redirect_drive(selected.redirect)
    return

  if selected 是 replay:
    handle_sta_replay_candidate(selected)
    return
```

中文文字伪代码：

```text
该函数是 candidate 真正执行的入口，只允许 commit frontier 指定 uid 调用。
先调用 service_ptw_wait_replay：这个 helper 释放已经满足 TLB 条件或超时的 PTW-back replay 等待项，可能重新置 replay_pending；它处理的是之前已经允许执行的 replay，不会扫描新的 candidate。
再调用 advance_active_redirect：这个 helper 推进正在执行的 redirect，如果 redirect drive 已完成，会调用 apply_redirect_flush 清状态并解除 freeze。若 active_redirect 仍有效，说明当前已有 redirect 尚未完成，本函数直接返回，blocked uid 后续 cycle 继续等待，不允许并行启动另一个 redirect/replay。
如果没有 active redirect，就调用 pop_recovery_candidate_for_uid(uid) 精确取当前 uid 的 candidate；取不到说明 candidate 已被 older redirect flush/drop 或动态实例过期，函数 warning/drop 后返回。
如果 selected 是 redirect，调用 request_redirect_flush 设置 active_redirect/flush_in_progress/issue_freeze_ack，再调用 push_redirect_drive 把 redirect payload 放入待 drive 队列；此时才真正置高 active_redirect。
如果 selected 是 replay，调用 handle_sta_replay_candidate 做 issue_epoch/replay_seq/target 检查，检查通过才 mark_replay_pending。
```

### 7.9 `exception_redirect_replay_handler::advance_active_redirect()` / `common_data_transaction::apply_redirect_flush()`

函数目的：在 redirect 已经启动后，持续推进 redirect drive done 检查和 flush cleanup，不依赖 LSQ commit loop。

输入：`active_redirect`、`pending_redirect_drive_q`、`redirect_drive_inflight`、当前 status 表和 active map。

输出/副作用：redirect drive 完成后清理被覆盖 uid 的状态、LSQ admission 镜像、issue queue、PTW wait replay、candidate queue，并解除全局 flush/freeze。

函数边界：第一版建议直接扩展现有 `apply_redirect_flush()`，不额外新增 `apply_redirect_flush_cleanup()` wrapper；如果 coding 时为了拆分职责新增 wrapper，也必须保持本节描述的输入、输出和副作用不变。

源码级伪代码：

```text
advance_active_redirect():
  if !active_redirect.valid:
    return

  redirect = active_redirect
  if data.redirect_drive_done_for(redirect):
    data.apply_redirect_flush(redirect)
  else if redirect_freeze_timeout 到期:
    fatal

apply_redirect_flush(redirect):
  apply_redirect_flush_range(redirect)
  clear_ptw_wait_replay_by_redirect(redirect)
  drop_candidates_covered_by_redirect(redirect)
  clear_redirect_drive_queue()
  redirect_phase = STATE_FLUSH_APPLIED
  flush_in_progress = 0
  dispatch_flush_in_progress = 0
  issue_freeze_ack = 0
  active_redirect = 0
  redirect_phase = IDLE
```

中文文字伪代码：

```text
advance_active_redirect 在 service loop 中每拍调用，负责让已经启动的 redirect 继续向前走。
如果 active_redirect 无效，说明当前没有正在执行的 redirect，函数直接返回。
如果 redirect_drive_done_for 返回 1，说明 redirect sequence 已经从 pending_redirect_drive_q 取出 payload 并完成 DUT io_redirect drive，函数调用 apply_redirect_flush 开始软件状态清理。
如果 redirect 长时间没有 drive done，则按照 redirect_freeze_timeout 报 fatal，避免 freeze 永久卡住。
apply_redirect_flush 先调用 apply_redirect_flush_range 扫描 active/admission 窗口：这个 helper 对被 redirect 覆盖且未 success 的 uid 调用 prepare_uid_for_redirect_reissue，清旧动态实例状态并让同 uid 后续可重新 admission。
随后调用 clear_ptw_wait_replay_by_redirect 清掉被 redirect 覆盖的 PTW wait replay；调用 drop_candidates_covered_by_redirect 清掉被 redirect 覆盖的 pending candidate；调用 clear_redirect_drive_queue 清 redirect driver 队列。
最后清 flush_in_progress、dispatch_flush_in_progress、issue_freeze_ack 和 active_redirect，让 admission/issue/commit flow 恢复。
```

### 7.10 `common_data_transaction::prepare_uid_for_redirect_reissue()`

函数目的：把 redirect 覆盖的旧动态实例清理成“可重新 admission/reissue”的状态。

输入：被 flush 的 `uid` 和当前 `redirect` payload。

输出/副作用：清 active map 或 issue queue，回退 LSQ admission 镜像计数，清旧 writeback/pass/fault/replay/commit/deq/success 状态，设置 `redirect_pending/flushed`，递增 `dynamic_epoch`。

源码级伪代码：

```text
prepare_uid_for_redirect_reissue(uid, redirect):
  if !redirect.valid:
    fatal

  status = get_status(uid)
  if status.success:
    fatal

  main_tr = get_main_transaction(uid)
  had_lq_mapping = status.active_lq_mapped
  had_sq_mapping = status.active_sq_mapped

  if status.active:
    retire_active_uid(uid)
  else:
    remove_uid_from_issue_queues(uid)

  if had_lq_mapping:
    pending_lq_cancel_count += main_tr.numLsElem
  if had_sq_mapping:
    pending_sq_cancel_count += main_tr.numLsElem

  clear_uid_dispatch_result(uid)
  status.redirect_pending = 1
  status.flushed = 1
  status.dynamic_epoch++
  status.active = 0
  status.success = 0
```

中文文字伪代码：

```text
该函数负责清理被 redirect kill 的旧动态实例，并准备同一个 uid 后续重新进入 LSQ admission。
先检查 redirect 必须有效；如果 uid 已经 success，说明 commit frontier candidate 检查失效，因为 success uid 不应再被 redirect flush，必须 fatal。
读取 main transaction 是为了知道这条指令占用多少 LSQ 元素；读取 status 的 LQ/SQ mapping 是为了决定是否需要回退软件 LSQ admission 镜像。
如果 uid 仍 active，就调用 retire_active_uid 删除 ROB/LQ/SQ active map；如果 uid 已经不 active，但可能还有 issue queue item，就调用 remove_uid_from_issue_queues 清 pending issue 项。
如果原来有 LQ/SQ mapping，就累加 pending_lq_cancel_count 或 pending_sq_cancel_count，后续 LSQ admission sequence 用这些计数回退软件 LQ/SQ 指针。
最后调用 clear_uid_dispatch_result 清 enq、issue、writeback、pass、fault、replay、commit、deq、success 等旧状态，再设置 redirect_pending/flushed 标记该 uid 需要重走；dynamic_epoch++ 让旧 monitor event 在后续检查中失效。
```

### 7.11 `exception_redirect_replay_handler::handle_sta_replay_candidate()` / `common_data_transaction::mark_replay_pending()`

函数目的：当 replay candidate 到达 commit frontier 后，把指定 LOAD/STA target 重新置为可 route/fire 状态。

输入：selected replay event，包含 uid、target、issue_epoch、replay_seq、PTW-back 标志。

输出/副作用：可能进入 `ptw_wait_replay_q`；或设置 `replay_pending/replay_target_*`，清该 target 的 dispatched/writeback/pass/queued 状态，递增 replay_seq。

源码级伪代码：

```text
handle_sta_replay_candidate(event):
  if !resolve_uid_for_event(event, uid):
    warning/drop
    return

  issue_epoch = get_event_issue_epoch(event, uid)
  replay_seq = get_event_replay_seq(event, uid)

  if event_should_wait_ptw(event):
    push_ptw_wait_replay(uid, event.target, issue_epoch, replay_seq, cycle)
    return

  mark_replay_pending(uid, event.target, issue_epoch, replay_seq, event.cycle)

mark_replay_pending(uid, target, issue_epoch, replay_seq, cycle):
  status = get_status(uid)
  if target 是 STD:
    warning/drop
    return 0

  if !status.active 或 status.issue_killed 或 !target_dispatched 或 epoch/seq 不匹配:
    return 0

  delete_issue_queue_entry(target, uid, 0, 0)
  status.replay_pending = 1
  status.writeback = 0
  status.pass = 0
  status.success = 0
  按 target 清 dispatched/writeback/pass/feedback/queued
  设置 replay_target_load 或 replay_target_sta
  bump_replay_seq(uid)
  return 1
```

中文文字伪代码：

```text
handle_sta_replay_candidate 负责把已经被 commit frontier 放行的 replay candidate 转成 replay pending 状态。
先调用 resolve_uid_for_event：这个 helper 用 event 中的 ROB/LQ/SQ 信息确认 event 仍对应当前 active uid；解析失败说明 event 过期或已被 redirect 清掉，直接 drop。
接着读取 issue_epoch 和 replay_seq，它们是 replay 的动态实例快照，用于防止旧 feedback 修改新动态实例。
如果 event_should_wait_ptw 返回 1，说明 replay 需要等待 PTW/TLB 条件满足，本函数调用 push_ptw_wait_replay 入等待队列后返回；这个等待队列只允许在 candidate 已到 frontier 后产生。
如果不需要等 PTW，就调用 mark_replay_pending。
mark_replay_pending 只接受 LOAD/STA，因为当前 MemBlock 没有后端 STD replay 反馈路径；STD replay 会 warning/drop。
随后检查 uid 是否 active、是否未被 issue_killed、target 是否确实发射过、issue_epoch/replay_seq 是否和 status 中快照匹配；任何检查失败都表示 stale replay，函数返回 0。
检查通过后删除该 target 旧 issue queue item，清 writeback/pass/success 和该 target 的 dispatched/writeback/pass/queued 标志，设置 replay_target_load 或 replay_target_sta，最后 bump_replay_seq 让旧 replay/writeback event 失效。后续 route_all_ready_uids 会根据 replay_target_* 只把需要重发的 target 重新入 issue queue。
```

### 7.12 `dispatch_monitor_batch_handler::process_monitor_event_batch()` 修改

函数目的：把 monitor batch 中的 redirect/replay 从旧即时恢复队列改送 recovery candidate queue，同时保留 batch redirect-first 对同批 pass/fault/replay 的覆盖过滤。

输入：同一 service cycle 收集到的 `memblock_wb_event_t events[$]`。

输出/副作用：redirect/replay 入 `recovery_candidate_q`；normal pass/fault 按规则落状态或入 fault 消费队列；被同批 oldest redirect 覆盖的 event drop。

源码级伪代码：

```text
process_monitor_event_batch(events):
  normalize_event_batch(events, normalized_events)
  if normalized_events 为空:
    return

  if active_redirect.valid:
    foreach event:
      if event_covered_by_redirect(event, active_redirect):
        drop
      else if event 是 redirect 或 replay:
        push_recovery_candidate(event)
      else:
        process_allowed_non_redirect_event(event)
    return

  if select_oldest_redirect(normalized_events, selected_redirect_event):
    push_recovery_candidate(selected_redirect_event)
    foreach 其他 event:
      if same_redirect_event(event, selected_redirect_event):
        continue
      if event_covered_by_redirect(event, selected_redirect):
        drop
      else if event 是 redirect 或 replay:
        push_recovery_candidate(event)
      else:
        process_allowed_non_redirect_event(event)
    return

  foreach event:
    if event 是 replay:
      push_recovery_candidate(event)
    else:
      process_allowed_non_redirect_event(event)
```

中文文字伪代码：

```text
该函数是 monitor event 进入新 recovery frontier 机制的入口。
先调用 normalize_event_batch：这个 helper 把 raw monitor event 转成带 uid/rob/epoch/seq 的 normalized event，并 drop 无法定位当前动态实例的旧 event。
如果当前已有 active_redirect，就先用 event_covered_by_redirect 判断 event 是否被正在执行的 redirect 覆盖；覆盖则 drop，不覆盖的 redirect/replay 仍然只能 push_recovery_candidate，不能 push_feedback_event。
如果当前没有 active_redirect，就在同一 batch 内调用 select_oldest_redirect 选 ROB 最老 redirect。选中的 redirect 入 recovery_candidate_q，但不立即 request_redirect_flush。被它覆盖的同批 pass/fault/replay 直接 drop；未被覆盖的其它 redirect/replay 也进入 recovery_candidate_q，等待各自 commit frontier。
如果同批没有 redirect，replay 仍进入 recovery_candidate_q；normal pass/fault 才交给 process_allowed_non_redirect_event 处理。
```

### 7.13 Candidate 等待期间 Service Loop

函数目的：说明 pending candidate 存在时，顶层 service loop 和主动 sequence 不应因为 recovery_frontier_uid 暂停正常流动。

输入：service clock、monitor event batch、LSQ admission/issue/commit 三类主动 sequence 的当前状态。

输出/副作用：normal flow 继续推进；只有 active_redirect 真正置位后，现有 global flush/freeze 才阻塞 admission/issue/commit。

源码级伪代码：

```text
service_loop_each_cycle():
  collect_runtime_context_events()
  collect_monitor_event_batch()
  exception_redirect_replay_task()
  route_all_issue_queues()

LSQ admission / issue select-fire / LSQ commit:
  不检查 uid >= recovery_frontier_uid
  不因为 recovery_candidate_q 非空而退出或暂停
  只有 issue_blocked_by_global_flush() 为 1 时按现有 active_redirect/flush 机制暂停
```

中文文字伪代码：

```text
该逻辑约束 candidate 等待期和 active redirect 执行期的边界。
当 recovery_candidate_q 非空但 active_redirect 还没置位时，说明 recovery 只是被采集、尚未到 commit frontier；LSQ admission、issue route、issue select/fire、ROB commit/LQ/SQ deq 都应继续按原规则流动。
这些 flow 不允许读取 recovery_frontier_uid 做 uid 范围 hold，也不允许因为队列非空提前退出。
只有 process_recovery_candidate_for_uid 选中 redirect 并调用 request_redirect_flush 后，issue_blocked_by_global_flush 才变为 1；此后 admission/issue/commit 按现有 active redirect/global flush 机制暂停，直到 service loop 的 advance_active_redirect 完成 apply flush 并解除 freeze。
```

### 7.14 不新增 Hold 过滤

函数目的：明确哪些现有高频路径不能添加 recovery frontier hold 条件，避免把 debug tracking 字段误用成全局阻塞边界。

输入：`recovery_frontier_uid/recovery_frontier_rob_key/recovery_candidate_q`。

输出/副作用：这是实现约束，不新增状态副作用；要求相关函数保持原有 eligibility 逻辑。

源码级伪代码：

```text
issue_queue_scheduler::is_uid_route_ready(uid):
  不检查 uid >= recovery_frontier_uid

issue_queue_scheduler::item_is_eligible(item):
  不检查 item.uid >= recovery_frontier_uid

lsq_commit_handler::uid_is_commit_candidate(uid):
  不检查 uid >= recovery_frontier_uid

memblock_lsqenq_dispatch_sequence admission selection:
  不检查 uid >= recovery_frontier_uid

如果需要 recovery 检查:
  只在 lsq_commit_handler 当前 commit cursor uid 上调用 has_recovery_candidate_for_uid(uid)
```

中文文字伪代码：

```text
这条规则防止实现时把 recovery_frontier_uid 当成 hold_begin_uid。
route、issue、admission 这些高频路径只能看原有 active/enq/issue_ready/replay/redirect/exception/global_flush 条件，不能因为某个更年轻或更老 candidate 存在而暂停 uid 范围。
commit path 也不能在 uid_is_commit_candidate 内部做范围阻塞；真正的 recovery 检查只能发生在 select_rob_commit_batch 当前 cursor uid 上，并且只查询 has_recovery_candidate_for_uid(uid)。
这样才能满足“candidate 等待期间不暂停 DUT 行为模拟，older redirect 到达 frontier 后再统一 flush”的最终目标。
```

## 8. 与现有机制的关系

### 8.1 与 `active_redirect`

`active_redirect` 保留。

关系：

```text
recovery_candidate_q:
  表示已经采集但尚未真正触发的 recovery。

active_redirect:
  表示已经决定执行，并且 redirect drive/apply flush 尚未完成的 recovery。
```

置位时机：

```text
monitor 采到 redirect 时:
  只允许进入 recovery_candidate_q；
  不允许置 active_redirect。

commit cursor/frontier 扫到 candidate.uid 且该 candidate 被选中执行时:
  process_recovery_candidate_for_uid(uid)
    -> request_redirect_flush(selected.redirect)
    -> active_redirect = selected.redirect
    -> push_redirect_drive(selected.redirect)
```

`active_redirect` 执行期间是否保留现有全局 freeze，由当前 redirect drive/apply flush 机制决定；本方案只规定 candidate 等待期间不 freeze。

### 8.2 与 `issue_epoch` / `replay_seq`

保留并加强。

原因：

```text
candidate 延迟后，迟到/过期事件更多。
真正执行 replay 或处理 writeback/fault 时仍必须检查 issue_epoch/replay_seq。
```

### 8.3 与 `success_prefix_uid`

`success_prefix_uid` 继续表示已经 success 的连续提交前缀。

语义：

```text
success_prefix_uid 之前的 uid 已经成功提交，不参与后续 recovery。
commit cursor/frontier 扫到 candidate.uid 时，必须先处理 candidate。
candidate 处理完成前，candidate.uid 不能置 success，success_prefix_uid 也不能越过 candidate.uid。
```

注意：

```text
success_prefix_uid 不回滚。
advance_success_prefix() 不感知 recovery_candidate_q，也不增加 candidate stop 条件；
它仍然只根据 status.success 连续前缀推进。
本方案用 commit frontier candidate 检查避免 success 后再 recovery。
candidate 执行前仍必须做动态实例重检。
```

### 8.4 与 `global_stop_requested`

如果存在 recovery candidate：

```text
transaction_done 不应为 true。
global_stop_requested 不能置位。
end_test_check 需要检查 candidate queue 为空。
```

## 9. 需要确认的问题

1. fault 是否也纳入 recovery candidate frontier。

   第一版建议不纳入。fault 已经会阻断 success，且当前 batch redirect-first 已防止被同批 redirect 覆盖。若后续发现 older redirect 后到也会导致 fault 多余落表，可再把 fault 迁入 candidate。

2. 如果 redirect flush 命中已经 success 的 uid 怎么办。

   第一版不允许这种情况。若发生，说明 commit flow 绕过了 candidate 检查，让需要 recovery 的 uid 先 success，应作为错误处理。这样可以保留 `success_prefix_uid` 单调不回滚语义。

3. already queued 的年轻 issue item 是否删除。

   candidate 等待期间不删除、不阻塞。older redirect 真正执行后，由现有或新增 redirect flush 清理 issue queue 和相关状态。

4. 如果 candidate.uid 长时间等不到 frontier 怎么处理。

   建议复用 no-progress warning 和 UVM timeout 兜底，不新增短 timeout fatal。因为等待可能来自真实 DUT backpressure/cache/TLB。

## 10. 现有测试框架适配风险与补充约束

结合当前测试框架源码，除了 `active_redirect` 置位时机外，还必须补充以下约束。

### 10.1 redirect/replay 必须从旧 `exception_event_q` 即时恢复路径分离

涉及函数：`dispatch_monitor_batch_handler::process_monitor_event_batch()`、`writeback_status_handler::handle_issue_feedback_event()`。

函数目的：把 redirect/replay 从旧 `exception_event_q` 即时恢复路径迁移到 `recovery_candidate_q`，避免 monitor 一采到 redirect/replay 就立刻执行 recovery。

输入：同一 service cycle normalize 后的 monitor event；IQ feedback failed event；当前 `active_redirect` 和 candidate queue。

输出/副作用：redirect/replay 入 `recovery_candidate_q`；fault 可继续入 `exception_event_q`；normal pass/fault 按状态表规则落表或 drop；不设置 `active_redirect`。


当前旧路径：

```text
dispatch_monitor_batch_handler
  -> data.push_feedback_event(event)
  -> exception_event_q
  -> exception_redirect_replay_handler::process_pending_events()
  -> pop_feedback_event()
  -> redirect: request_redirect_flush + push_redirect_drive
  -> replay: mark_replay_pending
```

风险：

```text
如果 redirect/replay 仍进入 exception_event_q，
process_pending_events() 会继续按旧逻辑即时执行 recovery。
这样 uid=10 redirect 先到时仍可能提前置 active_redirect，
uid=5 到 commit frontier 时会被 uid=10 active_redirect 屏蔽。
```

补充约束：

```text
redirect/replay monitor event 只能进入 recovery_candidate_q；
不能进入 exception_event_q；
不能通过 push_feedback_event() 间接进入旧即时恢复路径。

fault event 第一版可继续进入 exception_event_q；
normal pass 继续由 writeback_status_handler 直接落状态。
```

源码级伪代码：

```text
process_monitor_event_batch(event):
  normalized = normalize(event)

  if event_is_redirect(normalized):
    data.push_recovery_candidate(normalized)
    return

  if event_is_replay(normalized):
    data.push_recovery_candidate(normalized)
    return

  if event_is_fault(normalized):
    writeback_handler.mark_fault_status(normalized)
    data.push_feedback_event(normalized)  // 仅 fault 仍走旧消费队列
    return

  if event_is_normal_pass(normalized):
    writeback_handler.mark_normal_pass(normalized)
    return
```

中文文字伪代码：

```text
这段逻辑在当前 feature 中负责切断 redirect/replay 的旧即时恢复入口。
process_monitor_event_batch 先判断 event 类型：redirect/replay 只能进入 recovery_candidate_q，函数返回后不会设置 active_redirect，也不会进入 exception_event_q。
fault 第一版仍可调用 writeback handler 先落 fault 状态，再进入 fault 消费队列；normal pass 只做普通 pass/writeback 状态更新。
handle_issue_feedback_event 遇到 iq_feedback_failed 时不能再 push_feedback_event，因为 push_feedback_event 会进入 exception_event_q 并被 process_pending_events 即时消费；它必须把 replay 交给 batch handler 或直接入 candidate queue。
```

`writeback_status_handler` 也要同步约束：

源码级伪代码：

```text
handle_issue_feedback_event():
  if wb_event.iq_feedback_failed:
    不调用 data.push_feedback_event(wb_event)
    把 wb_event 交给 dispatch_monitor_batch_handler 分类
    或直接 data.push_recovery_candidate(wb_event)
    return
```

中文文字伪代码：

```text
handle_issue_feedback_event 处理 IQ feedback failed 时，旧逻辑会 push_feedback_event，导致 replay 进入 exception_event_q 并被即时 mark_replay_pending。
新方案中 failed feedback 代表 replay candidate，只能交给 batch handler 或直接调用 push_recovery_candidate。
函数返回后不能产生 active_redirect，也不能产生 replay_pending；真正 replay_pending 只能等 commit frontier 调用 process_recovery_candidate_for_uid 后产生。
```

### 10.1.1 写回更新状态前必须检查“写回前一状态”

涉及函数：`writeback_status_handler::handle_real_writeback_event()`、`common_data_transaction::mark_target_normal_pass()`、`common_data_transaction::mark_target_fault()`、`common_data_transaction::conditional_set_target_status_field()`。

函数目的：防止 redirect/replay/reissue 后迟到的旧流水 writeback，或者状态机已经越过目标阶段的 event，把 uid 错误推进到 writeback/pass/fault。

输入：normalized writeback event、status 表、target、issue_epoch、replay_seq。

输出/副作用：

- 如果 uid/target 正处于合法写回前置状态，则允许更新 target writeback/pass 或 target writeback/fault。
- 如果 uid/target 不处于合法写回前置状态，则不允许修改任何状态，只打印 `uvm_warning`，说明 uid、target、当前状态和拒绝原因。

“写回前一状态”定义：

```text
必须满足:
  status.active == 1
  status.issue_killed == 0
  status.redirect_pending == 0
  status.flushed == 0
  normal pass 路径要求 status.fault == 0 且 status.exception_pending == 0
  target 已经 dispatched
  target 还没有 writeback/pass/fault done
  status.get_target_issue_epoch(target) == event.issue_epoch
  target_replay_seq_match(status, target, event.replay_seq) == 1

不满足任意一项:
  不调用 set_status_field
  不设置 target writeback/pass/fault
  不设置 uid 级 writeback/pass/fault/exception_pending
  打印 uvm_warning/drop stale writeback event
```

为什么需要这个检查：

```text
writeback event 到达时能查到 uid，并不等于这个 uid 当前仍在等待这一次 writeback。

典型风险:
  uid 已 fire 到 DUT pipeline；
  后续 redirect flush 清掉旧动态实例；
  uid 又重新 admission/issue；
  旧流水 writeback 晚到，并通过 ROB/LQ/SQ active map 命中新动态实例；
  如果只看 uid 能查到，就可能错误设置新动态实例的 writeback/pass。

因此写状态前必须检查“当前 status 是否正好停在 writeback 前一状态”。
只有已经 issue fire、target_dispatched=1、epoch/replay_seq 匹配且未被 redirect/replay/exception 改写的 target，才允许被 writeback 推进。
```

源码级伪代码：

```text
check_target_writeback_pre_state(uid, target, issue_epoch, replay_seq, is_fault):
  status = get_status(uid)

  if !status.active:
    warning/drop; return false
  if status.issue_killed || status.redirect_pending || status.flushed:
    warning/drop; return false
  if !target_dispatched(status, target):
    warning/drop; return false
  if target_entry_done(status, target):
    warning/drop; return false
  if status.get_target_issue_epoch(target) != issue_epoch:
    warning/drop; return false
  if !target_replay_seq_match(status, target, replay_seq):
    warning/drop; return false
  if !is_fault && (status.fault || status.exception_pending):
    warning/drop; return false
  if is_fault && status.exception_pending:
    warning/drop; return false

  return true

mark_target_normal_pass(uid, target, issue_epoch, replay_seq, cycle):
  if !check_target_writeback_pre_state(uid, target, issue_epoch, replay_seq, 0):
    return 0
  set target writeback
  set target pass
  if required_targets_done:
    set uid writeback/pass

mark_target_fault(uid, target, issue_epoch, replay_seq, exception_vec, cycle):
  if !check_target_writeback_pre_state(uid, target, issue_epoch, replay_seq, 1):
    return 0
  set target writeback
  set target fault
  set uid fault/exception_pending
```

中文文字伪代码：

```text
writeback handler 不能因为 event 能解析出 uid 就直接更新状态。
它必须先问 status：这个 uid 的这个 target 当前是不是“已经发射出去、正在等写回”的状态。
如果 target 没有 dispatched，说明这个 writeback 不是当前应该等待的返回；如果 target 已经 pass/fault/writeback，说明这是重复或迟到返回；如果 redirect_pending/flushed/issue_killed 为 1，说明旧动态实例已经被 recovery 清掉；如果 issue_epoch/replay_seq 不匹配，说明 event 属于旧发射实例。
这些情况都只能 warning 并 drop，不能继续 set_status_field。
检查通过后，normal pass 才能设置 target writeback/pass；fault 才能设置 target writeback/fault 和 uid exception 状态。
```

### 10.2 `process_pending_events()` 只允许推进已执行中的 recovery

函数目的：限制 `process_pending_events()` 只处理已经启动的 recovery 和 fault-only 消费，不再从旧队列主动启动 redirect/replay。

输入：`exception_event_q`、`active_redirect`、`ptw_wait_replay_q`。

输出/副作用：推进 active redirect；释放已允许的 PTW wait replay；消费 fault event；如果旧队列里出现 redirect/replay，应 warning/fatal 或转入 candidate queue，不能直接执行。

当前旧 `process_pending_events()` 会主动从 `exception_event_q` 中找 redirect/replay 并触发 recovery。
新方案下它不能越过 commit frontier 主动找 candidate。

补充约束：

```text
process_pending_events():
  可以 service_ptw_wait_replay()
  可以 advance_active_redirect()
  可以消费 fault event
  不能主动 pop redirect/replay 并触发 request_redirect_flush/mark_replay_pending
  不能扫描 recovery_candidate_q 找可执行项
```

源码级伪代码：

```text
process_pending_events():
  service_ptw_wait_replay()
  advance_active_redirect()

  while pop_feedback_event(event):
    if event 是 redirect/replay:
      fatal 或 warning 后重新转入 recovery_candidate_q
      continue

    if event 是 fault:
      consume fault event
      continue
```

中文文字伪代码：

```text
process_pending_events 在新方案里只承担“推进已经启动的 recovery”和“消费 fault-only 事件”的职责。
它先调用 service_ptw_wait_replay，处理已经被 commit frontier 放行后进入 PTW wait 的 replay；再调用 advance_active_redirect，推进已经置位的 active_redirect。
随后从 exception_event_q 只消费 fault 事件。若发现 redirect/replay，说明旧路径仍在把 recovery event 送入即时队列，必须 warning/fatal 或迁移到 recovery_candidate_q，但不能直接 request_redirect_flush 或 mark_replay_pending。
函数不能扫描 recovery_candidate_q，因为 candidate 是否可执行只能由 LSQ commit frontier 决定。
```

### 10.3 commit frontier 命中 candidate 后必须有明确执行 owner

涉及函数：`lsq_commit_handler::build_lsqcommit_xaction()`、`memblock_lsqcommit_dispatch_sequence::send_lsqcommit_cycle()`、`exception_redirect_replay_handler::process_recovery_candidate_for_uid()`。

函数目的：保证发现 blocked recovery uid 的 commit handler 和执行 recovery 的 handler 在同一真实 LSQ commit flow 中衔接，避免不同 helper 实例维护不同 cursor。

输入：真实 LSQ commit sequence 的 `commit_handler`、`blocked_recovery_uid_valid/uid`、recovery handler。

输出/副作用：命中 candidate 时发 idle/空 commit xaction，不 mark ROB commit，转而调用 recovery handler；未命中 candidate 时保持原 commit pendingPtr 行为。

当前 `memblock_lsqcommit_dispatch_sequence` 和 `memblock_dispatch_base_sequence` 中存在不同 helper：

```text
memblock_lsqcommit_dispatch_sequence:
  commit_handler

memblock_dispatch_base_sequence:
  monitor_commit_handler
  exception_handler
```

风险：

```text
真正推进 commit_cursor_uid 的是 LSQ commit driver sequence 内的 commit_handler。
如果 recovery candidate 的触发逻辑写在另一个 monitor_commit_handler 或顶层 service loop 中，
可能出现 cursor 状态不一致，或者 commit handler 发现 candidate 但无人调用 recovery handler。
```

补充约束：

```text
commit frontier candidate 检查必须落在实际 drive lsqcommit 的 commit_handler 路径上。
命中 blocked_recovery_uid 后，必须在同一个 LSQ commit flow 中调用 recovery handler。
不能依赖另一个 commit_handler 实例替代判断。
```

源码级伪代码：

```text
lsq_commit_handler::build_lsqcommit_xaction(...):
  输出 blocked_recovery_uid_valid / blocked_recovery_uid

memblock_lsqcommit_dispatch_sequence::send_lsqcommit_cycle():
  commit_handler.build_lsqcommit_xaction(..., blocked_recovery_uid_valid, blocked_recovery_uid)

  如果 blocked_recovery_uid_valid:
    start/finish idle lsqcommit xaction
    recovery_handler.process_recovery_candidate_for_uid(blocked_recovery_uid)
    return

  否则:
    按原逻辑 drive pendingPtr 并 mark_rob_commit_batch()
```

中文文字伪代码：

```text
这段逻辑要求 blocked_recovery_uid 的发现和执行发生在真实 LSQ commit driver flow 中。
build_lsqcommit_xaction 输出 blocked_recovery_uid_valid/uid 后，send_lsqcommit_cycle 必须先 drive 一个空/idle commit transaction 保持接口时序，然后调用 process_recovery_candidate_for_uid。
如果 blocked_recovery_uid_valid 为 1，就不能 mark_rob_commit_batch，也不能 try_retire_committed_uid，因为该 uid 必须先 recovery。
如果没有 blocked recovery，原有 pendingPtr drive 和 mark_rob_commit_batch 才继续执行。
```

如果保留多个 commit handler 实例：

```text
commit cursor 不能只存在 object 私有字段中；
要么只保留一个 commit handler owner；
要么把 commit cursor 状态迁到 common_data_transaction 统一管理。
```

### 10.4 `transaction_done` 保持 success 前缀语义，`end_test_check` 检查 recovery 队列

涉及函数：`common_data_transaction::transaction_done()`、`request_global_stop_if_done()`、`end_test_check()`。

函数目的：`transaction_done()` 继续只表达“所有 uid success 前缀完成”；recovery/redirect/fault 相关队列和状态是否 idle，作为 `end_test_check()` 的结束一致性检查项。

输入：`success_prefix_uid/main_trans_num`、`recovery_candidate_q`、`exception_event_q`、`ptw_wait_replay_q`、`active_redirect`、redirect drive queue、flush 状态。

输出/副作用：`transaction_done()` 不因为 recovery idle 检查改变返回语义；`request_global_stop_if_done()` 仍跟随 transaction_done；`end_test_check()` 对结束时残留队列报错。

当前 `transaction_done()` 只看 `success_prefix_uid >= main_trans_num`，这个主流程完成语义应保持不变。
新方案只要求 `end_test_check()` 额外确认没有 pending recovery work。

风险：

```text
如果存在 stale recovery_candidate_q / exception_event_q / ptw_wait_replay_q，
但 success_prefix 已经到 main_trans_num，
顶层可能提前 request_global_stop。
这会掩盖 candidate 未清理或错误绕过的问题。
```

源码级伪代码：

```text
transaction_done():
  advance_success_prefix()
  return success_prefix_uid >= main_trans_num

end_test_check():
  if success_prefix_uid < main_trans_num:
    uvm_error("not all uid success at end_test_check")
  if recovery_candidate_q is not empty or exception_event_q is not empty or ptw_wait_replay_q is not empty:
    uvm_error("recovery queues are not idle at end_test_check")
  if active_redirect.valid or has_pending_redirect_drive() or flush_in_progress:
    uvm_error("redirect/flush state is not idle at end_test_check")
```

中文文字伪代码：

```text
transaction_done 先推进 success_prefix，只用 success_prefix 判断主流程是否完成。
它不检查 recovery_candidate_q、exception_event_q、ptw_wait_replay_q、active_redirect 或 pending redirect drive。
end_test_check 在 testcase 结束时检查这些队列和状态。
如果 recovery_candidate_q 非空，说明还有采集到但未执行或未被 flush 清掉的 recovery；如果 exception_event_q 或 ptw_wait_replay_q 非空，说明还有待消费 fault/replay；如果 active_redirect 或 pending redirect drive 非 idle，说明 redirect 清理还没结束。
这些情况都应在 end_test_check 报错，不能静默结束。
```

`end_test_check()` 也必须新增检查：

源码级伪代码：

```text
if recovery_candidate_q.size() != 0:
  uvm_error("recovery_candidate_q is not empty at end_test_check")

if exception_event_q.size() != 0:
  uvm_error("exception_event_q is not empty at end_test_check")
```

中文文字伪代码：

```text
end_test_check 是 testcase 结束时的最后防线。
如果 recovery_candidate_q 非空，说明还有 redirect/replay candidate 没有被执行或 drop；如果 exception_event_q 非空，说明还有 fault 或错误流入的 event 没被消费。
这两种情况都不能静默结束，应报 uvm_error 暴露流程闭环缺失。
```

### 10.5 redirect flush 必须清理 candidate queue，且不能依赖 active window

涉及函数：`common_data_transaction::drop_candidates_covered_by_redirect()`、`apply_redirect_flush()`。

函数目的：redirect apply flush 后清理整个 candidate queue 中被覆盖的 pending recovery event，不只清 active/admission window 中的 status。

输入：当前 `active_redirect` payload 和整个 `recovery_candidate_q`。

输出/副作用：删除被 redirect 覆盖的 candidate，打印 drop 原因，更新 recovery frontier tracking。

当前 `apply_redirect_flush_range()` 主要扫描 active/admission 窗口。
但 `recovery_candidate_q` 可能保存的是已经采集到的 younger event，不能只靠 active window 清理。

补充约束：

```text
drop_candidates_covered_by_redirect(redirect):
  遍历整个 recovery_candidate_q；
  对每个 candidate:
    如果 candidate.has_rob && rob_need_flush(candidate.rob_key, redirect):
      drop candidate；
      uvm_info/uvm_warning 打印 uid、rob、source、原因；
    否则保留。
```

源码级伪代码：

```text
apply_redirect_flush(active_redirect):
  apply_redirect_flush_range(active_redirect)      // 清 active/status/LSQ/issue 状态
  clear_ptw_wait_replay_by_redirect(active_redirect)
  drop_candidates_covered_by_redirect(active_redirect) // 清所有被覆盖 candidate
  clear_redirect_drive_queue()
  clear active_redirect / flush state
```

中文文字伪代码：

```text
apply_redirect_flush 先清 status/active map/issue queue 等执行状态，再清 PTW wait replay 和 recovery candidate queue。
drop_candidates_covered_by_redirect 必须遍历整个 recovery_candidate_q，而不是只看 active window，因为 candidate 可能只是已采集事件，不一定仍在 active/admission 窗口中。
如果 rob_need_flush 判断 candidate 被当前 redirect 覆盖，就删除该 candidate 并打印原因；否则保留，等待它自己的 commit frontier。
最后清 redirect drive 和 active_redirect 状态，让全局 flush/freeze 解除。
```

### 10.6 PTW wait replay 只能在 candidate 被选中后产生

涉及函数：`handle_sta_replay_candidate()`、`push_ptw_wait_replay()`。

函数目的：防止 replay event 在 commit frontier 放行前提前进入 PTW wait replay 队列。

输入：selected replay candidate、`ptw_back_replay` 标志、issue_epoch/replay_seq。

输出/副作用：只有 selected replay candidate 需要等待 PTW 时，才向 `ptw_wait_replay_q` 入队；monitor 采集阶段不入该队列。

当前 `handle_replay_event()` 可能因为 `ptw_back_replay` 调用 `push_ptw_wait_replay()`。
新方案下 replay event 刚被 monitor 采到时不能调用该函数。

补充约束：

```text
push_ptw_wait_replay() 只能由 process_recovery_candidate_for_uid(uid) 内部触发。
也就是 replay candidate 已经到达 commit frontier、已经被选中执行后，才允许进入 ptw_wait_replay_q。
```

源码级伪代码：

```text
monitor 采到 ptw_back_replay:
  push_recovery_candidate(event)
  不 push_ptw_wait_replay

commit cursor 到 event.uid:
  process_recovery_candidate_for_uid(uid)
    if event_should_wait_ptw(event):
      push_ptw_wait_replay(uid, target, issue_epoch, replay_seq)
      return
```

中文文字伪代码：

```text
monitor 采到 ptw_back_replay 时仍然只是采集事件，不能提前进入 ptw_wait_replay_q。
只有 commit cursor 到达该 uid，process_recovery_candidate_for_uid 取出 replay candidate 后，才检查 event_should_wait_ptw。
如果需要等待 PTW，push_ptw_wait_replay 会把 uid、target、issue_epoch、replay_seq 放入等待队列；后续 service_ptw_wait_replay 根据 TLB ready 或 timeout 再调用 mark_replay_pending。
这样可以保证 PTW wait replay 不会绕过 commit frontier 提前影响状态。
```

### 10.7 所有 success 写入路径必须防御 candidate 绕过

涉及函数：`try_retire_committed_uid()`、`set_status_field(MEMBLOCK_STATUS_SUCCESS)`。

函数目的：防止存在 recovery candidate 的 uid 被提前置 success，破坏 success_prefix 不回滚假设。

输入：准备 retire 的 uid、status 表、candidate queue。

输出/副作用：若 uid 有 pending candidate，则 fatal/error 并拒绝 success；否则按原逻辑 retire active uid 并推进 success prefix。

当前 `advance_success_prefix()` 只扫描 `status.success`，这是可以保留的。
真正要防的是某个 uid 存在 recovery candidate 时仍被置 success。

补充约束：

```text
try_retire_committed_uid(uid):
  如果 data.has_recovery_candidate_for_uid(uid):
    uvm_fatal 或 uvm_error；
    return；

set_status_field(MEMBLOCK_STATUS_SUCCESS, 1):
  原则上只能由 try_retire_committed_uid() 间接调用；
  如果保留公共入口，也必须检查该 uid 没有 recovery candidate。
```

源码级伪代码：

```text
try_retire_committed_uid(uid):
  if has_recovery_candidate_for_uid(uid):
    fatal("candidate uid must not retire before recovery")
    return

  if rob_commit && lq/sq mapping 都释放:
    set success
    retire_active_uid(uid)
```

中文文字伪代码：

```text
try_retire_committed_uid 是 uid 进入 success 的关键入口，因此必须先检查该 uid 是否还有 pending recovery candidate。
如果有 candidate，说明该 uid 必须先执行 redirect/replay，不能被置 success；函数应 fatal/error 并返回。
如果没有 candidate，才按原逻辑检查 rob_commit 和 LQ/SQ mapping 是否都完成，然后设置 success 并 retire active uid。
如果保留 set_status_field(SUCCESS) 的公共入口，也要加同样保护，防止其它调用绕过 try_retire_committed_uid。
```

### 10.8 本方案依赖 uid 顺序等价于 ROB commit 顺序

当前随机主表按 uid 递增生成 ROB，manual main table 也会按 ROB key 排序后分配 uid。
因此当前测试框架里 commit cursor 按 uid 顺序推进基本等价于 ROB 顺序提交。

风险：

```text
如果后续 testcase 允许 uid 顺序和 ROB 顺序不一致，
commit cursor 按 uid 查 recovery candidate 就不再严格等价于 ROB commit frontier。
```

补充约束：

```text
本方案第一版要求 uid 顺序必须等价 ROB commit 顺序。
build/import main table 时需要保留或新增检查：
  uid 单调递增时 ROB key 也按 commit order 单调递增。
如果未来允许乱序 uid，则 commit frontier 必须改为 ROB-order cursor，而不是 uid cursor。
```

如 coding 时新增检查 helper，建议命名为 `check_uid_rob_order()`。

函数目的：在主表构建完成后检查 uid 顺序是否等价 ROB commit 顺序，防止 commit frontier 用 uid cursor 时语义错误。

输入：`main_table_by_uid` 或 `main_table` 中每个 uid 对应的 ROB key。

输出/副作用：检查通过不修改状态；检查失败则 `uvm_fatal` 或 `uvm_error`，阻止该 testcase 继续运行。

源码级伪代码：

```text
check_uid_rob_order():
  if main_trans_num <= 1:
    return

  prev_rob = get_main_transaction(0).get_rob_key()
  for uid from 1 to main_trans_num-1:
    cur_rob = get_main_transaction(uid).get_rob_key()
    if cur_rob 不在 prev_rob 之后:
      fatal("uid order is not ROB commit order")
      return
    prev_rob = cur_rob
```

中文文字伪代码：

```text
这个 helper 属于构建期检查，不在每拍 service loop 中执行。
它从 uid0 开始读取每条 main transaction 的 ROB key，逐个比较后一个 uid 的 ROB 是否在前一个 uid 之后。
如果发现 uid 顺序和 ROB commit 顺序不一致，说明 `commit_cursor_uid` 不能再代表真实 commit frontier，继续执行会让 recovery candidate 触发时机错误，因此应 fatal/error。
检查通过时不修改任何状态，只证明本方案第一版的 uid cursor 假设成立。
```

### 10.9 active_redirect 期间新 event 的处理也不能回旧路径

涉及函数：`dispatch_monitor_batch_handler::process_monitor_event_batch()`。

函数目的：在已有 active redirect 正在执行时，继续正确分类新采集到的 monitor event，避免未被覆盖的 redirect/replay 回到旧 `exception_event_q`。

输入：active redirect payload、normalized event batch、candidate queue。

输出/副作用：被 active redirect 覆盖的 event 被 drop；未覆盖的 redirect/replay 入 `recovery_candidate_q`；普通 pass/fault 走 non-redirect 处理路径。

当前 `dispatch_monitor_batch_handler` 在 `active_redirect.valid` 时，对未被覆盖 redirect 会继续
`data.push_feedback_event()`。

补充约束：

```text
active_redirect.valid 期间:
  被 active_redirect 覆盖的 event 直接 drop；
  未被覆盖的 redirect/replay 仍进入 recovery_candidate_q；
  不能进入 exception_event_q；
  normal pass/fault 按动态实例检查后处理。
```

源码级伪代码：

```text
if active_redirect.valid:
  for event in normalized_events:
    if event_covered_by_redirect(event, active_redirect):
      drop event
      continue

    if event_is_redirect(event) || event_is_replay(event):
      data.push_recovery_candidate(event)
      continue

    process_allowed_non_redirect_event(event)
```

中文文字伪代码：

```text
active_redirect 期间 batch handler 仍会收到 monitor event。
先用 event_covered_by_redirect 判断 event 是否已被当前 active_redirect 覆盖，覆盖则直接 drop。
未被覆盖的 redirect/replay 仍然不能走 exception_event_q，而是进入 recovery_candidate_q 等待自己的 commit frontier。
普通 pass/fault 才交给 process_allowed_non_redirect_event，并继续依赖动态实例快照过滤 stale event。
```

### 10.10 同一 uid 多个 candidate 必须有确定优先级

涉及函数：`common_data_transaction::pop_recovery_candidate_for_uid()`。

函数目的：当同一 uid 同时存在 redirect/replay candidate 时，确定唯一、稳定、符合 recovery 语义的执行顺序。

输入：commit frontier 指定 uid、`recovery_candidate_q` 中该 uid 的所有 candidate。

输出/副作用：返回 selected candidate；从队列删除 selected；redirect 被选中时删除或使同 uid replay candidate 失效；更新 frontier tracking。

风险：

```text
同一 uid 可能先后进入多个 candidate，例如:
  STA replay candidate
  memoryViolation redirect candidate

如果 pop_recovery_candidate_for_uid(uid) 随机取一个，
可能先执行 replay，再执行 redirect，造成多余重发。
```

补充约束：

```text
同一 uid 内:
  redirect 优先于 replay；
  多个 redirect 选择 ROB oldest，同 ROB 时选择更早 port/source；
  redirect 被选中后，该 uid 上的 replay candidate 也应 drop 或被 redirect 覆盖清理；
  replay 只有在同 uid 没有 redirect candidate 时才允许执行。
```

源码级伪代码：

```text
pop_recovery_candidate_for_uid(uid):
  candidates = recovery_candidate_q 中 uid 相同的所有项

  if candidates 中存在 redirect:
    selected = candidates 中 ROB oldest redirect
    删除 selected
    删除同 uid 被 selected 覆盖或语义上已无效的 replay candidate
    update_recovery_frontier_tracking()
    return selected

  if candidates 中存在 replay:
    selected = 最早 replay candidate
    删除 selected
    update_recovery_frontier_tracking()
    return selected
```

中文文字伪代码：

```text
同一 uid 有多个 candidate 时，pop_recovery_candidate_for_uid 必须先看 redirect。
如果存在 redirect，选择 ROB 最老 redirect，并删除同 uid 中已经没有独立意义的 replay candidate，避免先 replay 后又 redirect。
如果没有 redirect，才选择 replay candidate。每次删除后都更新 frontier tracking，保证 debug 字段指向剩余 candidate。
```

### 10.11 pending candidate 存在时，normal pass/fault 后到的处理必须受限

涉及函数：`dispatch_monitor_batch_handler::process_allowed_non_redirect_event()`、`writeback_status_handler::handle_real_writeback_event()`。

函数目的：防止某 uid 已有 recovery candidate 后，迟到 normal pass/fault 又继续更新该 uid 的 writeback/pass/fault 状态。

输入：normalized non-redirect event、candidate queue、status 表。

输出/副作用：如果同 uid 已有 candidate，则 drop 并打印 debug；否则按原 pass/fault 路径更新状态或入 fault 消费队列。

风险：

```text
uid 已经有 redirect/replay candidate 后，
同一 uid 或 younger uid 的 normal pass/fault 仍可能继续被 monitor 采到。
如果这些 event 继续落状态，可能让 candidate uid 被 writeback/pass/fault 状态推进，
增加 commit frontier 判断歧义。
```

补充约束：

```text
对同一 uid:
  如果 has_recovery_candidate_for_uid(uid):
    normal pass 不允许落 pass/writeback；
    fault 第一版也不应覆盖已有 redirect/replay candidate 状态；
    应 drop stale event 或只记录 debug。

对被已有 pending redirect candidate 覆盖的 younger uid:
  normal pass/fault 可以按动态实例检查落表，但最终会被 older redirect flush 清理；
  如果实现复杂，第一版可在 batch handler 中检测 pending older redirect coverage 并 drop。
```

源码级伪代码：

```text
process_allowed_non_redirect_event(event):
  if data.has_recovery_candidate_for_uid(event.uid):
    uvm_info("drop non-recovery event because uid has pending recovery candidate")
    return

  按原 writeback/fault 逻辑处理
```

中文文字伪代码：

```text
process_allowed_non_redirect_event 在处理 normal pass/fault 前，先看同 uid 是否已经有 pending recovery candidate。
如果有 candidate，说明这个 uid 正在等待 commit frontier 执行 recovery；此时迟到的 pass/fault 不应再更新 writeback/pass/fault 状态，否则可能让该 uid 进入普通 commit 条件。
因此第一版直接 drop 并打印 debug。
如果没有 candidate，才继续走原 writeback/fault 处理路径。
```

### 10.12 active_redirect 执行后的推进 owner 不能依赖 LSQ commit loop

涉及函数：`memblock_dispatch_base_sequence::exception_redirect_replay_task()`、`exception_redirect_replay_handler::advance_active_redirect()`、`common_data_transaction::apply_redirect_flush()`。

函数目的：规定 active redirect 启动后的推进 owner 是 service loop/recovery handler，而不是可能被 global flush 阻塞的 LSQ commit loop。

输入：service loop 时钟、`active_redirect`、redirect drive queue/inflight 状态、flush 状态。

输出/副作用：redirect drive done 后执行 apply flush，清状态并解除 freeze；LSQ commit loop 即使 idle 也不影响 active redirect 完成。

当前 `memblock_lsqcommit_dispatch_sequence::send_lsqcommit_cycle()` 在
`data.issue_blocked_by_global_flush()` 为 1 时会直接发 idle xaction 并 return。
而 `request_redirect_flush()` 会设置：

```text
active_redirect.valid = 1
flush_in_progress = 1
issue_freeze_ack = 1
memblock_sync_pkg::dispatch_flush_in_progress = 1
```

风险：

```text
一旦 redirect candidate 被选中并置 active_redirect，
LSQ commit flow 自己可能因为 global flush 阻塞而不再继续处理 candidate。
如果 active_redirect 的 drive done / apply flush 只挂在 commit loop 后续路径，
就可能没人清理 active_redirect。
```

补充约束：

```text
active_redirect 的推进 owner 必须是 service loop 中持续调用的 exception_redirect_replay_handler。
LSQ commit flow 只负责在 commit frontier 命中 candidate 时启动 recovery；
启动后即使 LSQ commit 被 global flush 阻塞，service loop 仍必须继续:
  advance_active_redirect()
  redirect_drive_done_for()
  apply_redirect_flush()
```

源码级伪代码：

```text
service_monitor_once():
  collect_runtime_context_events()
  collect_monitor_event_batch()
  exception_redirect_replay_task()

exception_redirect_replay_task():
  service_ptw_wait_replay()
  advance_active_redirect()
  consume fault-only feedback events
```

中文文字伪代码：

```text
active_redirect 启动后，LSQ commit loop 可能因为 issue_blocked_by_global_flush 只发 idle transaction 并返回，所以 redirect 清理不能依赖 commit loop 自己继续推进。
service_monitor_once 每拍仍会调用 exception_redirect_replay_task。
exception_redirect_replay_task 调用 advance_active_redirect 检查 redirect drive 是否完成；完成后 apply_redirect_flush 清理状态并解除 freeze。
因此 LSQ commit flow 只负责启动 recovery，active_redirect 后续完成由 service loop 负责。
```

### 10.13 global_stop 保持 success 前缀语义，recovery idle 放入 end_test_check

涉及函数：`transaction_done()`、`request_global_stop_if_done()`、`end_test_check()`、各主动 sequence 的 `is_global_stop_requested()` 退出判断。

函数目的：明确主流程完成判断和结束一致性检查的边界。`transaction_done()` / `request_global_stop_if_done()` 只跟随 success prefix；recovery/redirect/fault 相关队列是否 idle，不作为主动 sequence 继续运行的条件，而是在 `end_test_check()` 中检查。

输入：success prefix、main_trans_num、recovery candidate queue、exception queue、PTW wait queue、active redirect/drive/flush 状态。

输出/副作用：`global_stop_requested` 仍由 success prefix 完成触发；如果 testcase 结束时 recovery 相关队列或 active redirect 没有 idle，`end_test_check()` 报错。

风险：

```text
如果把 recovery idle 放进 transaction_done/request_global_stop_if_done，
会把结束一致性检查和主流程完成条件混在一起。
这在“不支持”的 commit-frontier candidate 方案里尤其容易误导实现，
让 memoryViolation redirect 被错误延迟到 commit/global stop 边界。
```

源码级伪代码：

```text
request_global_stop_if_done():
  如果 transaction_done() 返回 true，则置 global_stop_requested。

transaction_done():
  advance_success_prefix()
  return success_prefix_uid >= main_trans_num

end_test_check():
  如果 success_prefix_uid < main_trans_num，报错。
  如果 recovery_candidate_q、exception_event_q 或 ptw_wait_replay_q 非空，报错。
  如果 active_redirect、pending_redirect_drive、flush_in_progress 或 issue_freeze_ack 未清空，报错。
```

中文文字伪代码：

```text
request_global_stop_if_done 只看 transaction_done。
transaction_done 只表达所有 uid 是否已经形成连续 success 前缀。
它不检查 recovery_candidate_q、exception_event_q、PTW wait replay、active_redirect 或 redirect drive。
这些 recovery idle 条件由 end_test_check 在 testcase 结束时检查。
如果结束时仍有 recovery 队列或 redirect/flush 状态残留，说明流程没有闭环，应报错暴露问题。
```

### 10.14 first-version fault 策略需要明确覆盖关系

涉及函数：`writeback_status_handler::handle_real_writeback_event()`、`common_data_transaction::mark_target_fault()`、`prepare_uid_for_redirect_reissue()`。

函数目的：明确第一版 fault 不进入 recovery candidate 时，fault 状态如何被 older redirect flush 清理，以及什么条件下需要把 fault 也迁入 frontier 机制。

输入：fault writeback event、status 表、active/older redirect payload。

输出/副作用：fault event 可设置 fault/exception_pending；如果后续被 older redirect 覆盖，prepare_uid_for_redirect_reissue 必须清掉 fault/exception 状态；如果 fault 阻塞 older redirect frontier，则需调整方案。

第一版建议 fault 不进入 recovery candidate，但这会带来一个边界：

```text
uid=10 fault 先落表；
uid=5 redirect 后到并覆盖 uid=10；
uid=10 fault 必须能被 uid=5 redirect flush 清理。
```

补充约束：

```text
fault 不进入 recovery_candidate_q 的前提是:
  mark_target_fault() 设置的 fault/exception_pending/pass/writeback 状态
  能被 prepare_uid_for_redirect_reissue() 完整清理；
  fault uid 如果被 older redirect 覆盖，不允许阻止 redirect flush；
  end_test_check 中不应残留被 flush 掉旧动态实例的 fault/exception_pending。
```

如果实现发现 fault 会阻塞 commit cursor，导致 older redirect candidate 无法到达 frontier：

```text
需要把 fault 也纳入 recovery_candidate_q，或在 commit frontier 对 fault 做专门处理。
```

源码级伪代码：

```text
handle_real_writeback_event(fault_event):
  mark_target_fault(uid, target, issue_epoch, replay_seq, exception_vec, cycle)
  push_feedback_event(fault_event)  // 第一版仅 fault 消费队列

prepare_uid_for_redirect_reissue(uid, redirect):
  if uid 被 redirect 覆盖且未 success:
    clear_uid_dispatch_result(uid)
    清 fault/exception_pending/exception_vec/pass/writeback
    设置 redirect_pending/flushed
```

中文文字伪代码：

```text
第一版 fault 仍然可以先落 fault/exception_pending 状态，因为它不是 redirect/replay recovery candidate。
如果后续 older redirect 覆盖该 fault uid，prepare_uid_for_redirect_reissue 必须通过 clear_uid_dispatch_result 清掉 fault、exception_pending、exception_vec、pass/writeback 等旧动态实例状态。
这样 uid=10 fault 先到、uid=5 redirect 后到时，uid=10 的 fault 不会残留到 reissue 后的新动态实例。
如果实际实现中 fault 会让 commit cursor 停住，从而导致更老 redirect candidate 无法到达 frontier，则说明 fault 也需要纳入 recovery_candidate_q 或增加 commit frontier fault 专门处理。
```

## 11. 推荐实施顺序

1. 在 `common_data_transaction` 增加 recovery candidate queue 和 frontier tracking API。
2. 修改 `dispatch_monitor_batch_handler`，redirect/replay 通过 batch 仲裁后进入 candidate queue。
3. 修改 `lsq_commit_handler`，在 commit cursor/frontier 扫到某个 uid 时先检查该 uid 是否有 recovery candidate；有则先触发 recovery 并停止本轮 commit。
4. 修改 `exception_redirect_replay_handler`，提供 `process_recovery_candidate_for_uid(uid)`，只处理 commit frontier 指定 uid 的 candidate。
5. 确认 LSQ admission、issue route、issue select/fire、ROB commit/LQ/SQ deq 不因 pending candidate 增加全局阻塞。
6. 检查 redirect flush 清理策略，确保被 older redirect 覆盖的未 success uid 能清理；如果命中 success uid，应报告 commit frontier 检查缺失。
7. 更新 flow 文档：
   - `AI_DOC/mem_ut_flow_doc/writeback_function_call_flow.md`
   - `AI_DOC/mem_ut_flow_doc/redirect_flow.md`
   - `AI_DOC/mem_ut_flow_doc/replay_flow.md`
   - `AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md`
   - `AI_DOC/mem_ut_flow_doc/lsq_admission_flow.md`
   - `AI_DOC/mem_ut_flow_doc/rob_commit_lq_sq_deq_flow.md`
8. 添加 directed 场景验证：
   - younger redirect 先到，older redirect 后到，只执行 older redirect。
   - younger STA replay 先到，older redirect 后到，STA replay 不提前 mark_replay_pending。
   - candidate 等待期间，LSQ admission、issue route、issue select/fire、ROB commit/LQ/SQ deq 继续运行。
   - older redirect 真正执行后，覆盖范围内未 success 的 younger uid 能被 flush 清理。
   - 若覆盖到 success uid，测试应报错，说明 candidate commit-frontier 检查被绕过。

## 12. 验收标准

1. `recovery_candidate_q` 中 redirect/replay 不会立即触发 recovery。
2. 只有 commit cursor/frontier 扫到 `candidate.uid` 且动态实例重检通过时 candidate 才允许执行。
3. `advance_success_prefix()` 不查询 candidate queue，只按 `status.success` 连续前缀推进。
4. pending candidate 存在时，不因为 `uid >= recovery_frontier_uid` 暂停 admission、route、issue fire、ROB commit 或 LQ/SQ deq。
5. LSQ admission、issue route、issue select/fire、ROB commit/LQ/SQ deq 在 candidate 等待期间继续按原逻辑运行。
6. older redirect 后到时，能覆盖并 drop younger redirect/replay candidate。
7. older redirect apply flush 能清理覆盖范围内未 success uid 的 active/pass/replay/issue/LSQ/candidate 状态；如果命中 success uid，应报错。
8. STA replay candidate 只有在 frontier eligible 且重检通过后才 mark_replay_pending。
9. 所有 drop stale candidate 的地方需要 `uvm_warning` 或 `uvm_info` 说明 uid、rob、source 和原因。
10. 同一 uid 同时存在 redirect/replay candidate 时，redirect 必须优先，replay 不得先执行。
11. 有 pending recovery candidate 的 uid 不允许继续落 normal pass/fault 推进状态。
12. active_redirect 启动后，即使 LSQ commit loop 因 global flush 发 idle，service loop 仍能推进 drive done 和 apply flush。
13. 写回更新状态前必须检查“写回前一状态”：代码完成后必须确认 `mark_target_normal_pass()` 和 `mark_target_fault()` 或二者复用的公共 helper 中已经实现 active、issue_killed、redirect_pending、flushed、target_dispatched、target_entry_done、issue_epoch、replay_seq 等前置检查；检查失败只能 `uvm_warning` 并 drop event，不能修改 target/uid 状态。
