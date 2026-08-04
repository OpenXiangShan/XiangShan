# Redirect Flush 批处理主路径 Review

本文审查当前 mem_ut dispatch 框架中 redirect flush 批处理主路径。重点不是解释 redirect 概念，而是检查当前代码是否形成了完整闭环：DUT 反馈触发 redirect，TB 冻结旧上下文，真实驱动 `io_redirect`，等待驱动完成后按 ROB 范围批量清理 uid，并让后续 LSQ admission 从正确位置重新开始。

相关源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/exception_redirect_replay_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_redirect_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/rob_order_util.sv`

## 1. Review 结论

当前主路径已经按阶段化 redirect recovery 实现：

```text
DUT feedback event
  -> exception_event_q
  -> process_pending_events()
  -> select_oldest_redirect()
  -> request_redirect_flush()
  -> push_redirect_drive()
  -> redirect sequence drive io_redirect
  -> mark_redirect_drive_done()
  -> apply_redirect_flush()
  -> apply_redirect_flush_range()
  -> prepare_uid_for_redirect_reissue()
  -> rollback_max_enqueued_uid()
  -> 后续 LSQ admission apply_pending_lsq_cancels() 后重新入队
```

核心设计是正确的：软件状态清理没有在发现 redirect 的同一时刻立即执行，而是先冻结 dispatch，再把 redirect payload 真实驱动到 DUT input 接口，等驱动完成并跨过一个 service cycle 后，才批量清理被 flush 覆盖的 uid。

这个流程解决了三个关键问题：

- 避免 redirect 期间继续发射旧路径 transaction。
- 避免 TB 只改软件状态但 DUT 没收到 `io_redirect`。
- 避免被 flush 的 uid 被当作终态完成，而是重新从 LSQ admission 走新一轮动态实例。

## 2. 主路径分阶段审查

### 2.1 周期性入口

入口在 `memblock_dispatch_base_sequence::exception_redirect_replay_task()`：

```systemverilog
task memblock_dispatch_base_sequence::exception_redirect_replay_task();
    if (exception_handler == null) begin
        exception_handler = exception_redirect_replay_handler::type_id::create("exception_handler");
    end
    exception_handler.process_pending_events();
endtask:exception_redirect_replay_task
```

功能：

- 这是 redirect/replay/fault 处理的后台服务入口。
- 它不是单次直连调用，而是在 dispatch real smoke service loop 中周期执行。
- 这样 active redirect 可以跨多个 cycle 推进：第一拍发现 redirect，后续拍等待 redirect sequence drive done，再后续拍做状态 flush。

Review 判断：

- 入口设计合理。redirect recovery 需要和 issue、LSQ admission、commit 并行协同，周期性服务比一次性函数更适合。

### 2.1.1 `exception_event_q` 数据输入和消耗

`exception_event_q` 定义在 `common_data_transaction` 中：

```systemverilog
memblock_wb_event_t exception_event_q[$];
```

这个队列名字里有 `exception`，但它不是只保存 exception。更准确地说，它是 writeback monitor/adapter 与 replay/fault/redirect recovery handler 之间的 **feedback 仲裁缓冲队列**。进入这个队列的 event 都不是普通“立即 pass”事件，而是需要统一仲裁、延后处理或可能触发 recovery 的事件。

当前主要进入队列的 event 类型如下：

| event 类型 | 来源 | 入队原因 |
|---|---|---|
| redirect event | memoryViolation 或其他 redirect feedback | 需要先选最老 redirect，再冻结并真实驱动 `io_redirect`。 |
| replay event | IQ feedback miss、backend replay 类反馈 | 需要由 recovery handler 设置 replay pending，并决定是否等待 PTW。 |
| fault event | writeback exception vector 非 0 | 需要记录 fault 后交给 exception/recovery 路径继续处理。 |
| deferred event | writeback handler 临时弹出但不应由它处理的 event | 写回 handler只处理普通 pass，replay/fault/redirect 会放回队列。 |
| requeued event | redirect 仲裁时同批未被 flush 覆盖的 event | 当前 redirect recovery 完成后再处理，避免同轮混改状态。 |

#### 数据输入路径

正式入队入口是 `common_data_transaction::push_feedback_event()`：

```systemverilog
function void push_feedback_event(input memblock_wb_event_t wb_event);
    memblock_wb_event_t normalized_event;

    if (!normalize_feedback_event(wb_event, normalized_event)) begin
        return;
    end
    exception_event_q.push_back(normalized_event);
endfunction
```

它先调用 `normalize_feedback_event()`，再 `push_back()` 入队。`normalize_feedback_event()` 会补齐或校验：

- `uid/has_uid`：通过 ROB/LQ/SQ active map 反查当前 active uid。
- `rob_key/has_rob`：redirect payload 或 status 中的 ROB key。
- `issue_epoch`：普通非 redirect event 必须能对应到本次 target 发射快照。
- `replay_seq`：用于防止 replay 后迟到旧 event 更新新动态实例。
- event action：必须是 redirect、replay、fault 或 raw writeback pass 中的一类。

如果反查不到 active uid，或者 replay 后缺少必要快照，该 event 不会进入 `exception_event_q`。这一步的意义是把 raw monitor fact 转成“可被公共状态机处理的归一化 feedback event”，避免 handler 后面拿到半成品 event。

#### 输入来源 1：writeback / IQ feedback

service loop 中先调用：

```systemverilog
collect_writeback_events();
```

它会通过 adapter 消费 raw queue：

```text
raw_int_wb_q      -> convert_raw_int_wb()
raw_iq_feedback_q -> convert_raw_iq_feedback()
```

转换后交给：

```systemverilog
writeback_handler.handle_event(wb_event);
```

`writeback_status_handler::handle_event()` 的分类行为如下：

| 分类 | 行为 |
|---|---|
| normal pass | 直接调用 `mark_target_normal_pass()` 更新状态，不进入 `exception_event_q`。 |
| replay | 调用 `data.push_feedback_event(wb_event)` 入队。 |
| redirect | 调用 `data.push_feedback_event(wb_event)` 入队。 |
| fault | 先调用 `mark_target_fault()` 标记 fault，再调用 `data.push_feedback_event(wb_event)` 入队。 |

所以普通 pass 不是这个队列的主要消费者路径；它通常在 writeback handler 中直接完成。`exception_event_q` 主要服务 replay/fault/redirect 这种需要额外仲裁的事件。

#### 输入来源 2：memoryViolation

memoryViolation 来自 ctrl raw event。service loop 中：

```systemverilog
collect_exception_and_redirect_events();
```

内部流程是：

```text
raw_ctrl_q
  -> apply_raw_ctrl_deq()
  -> convert_raw_memory_violation()
  -> writeback_handler.handle_event()
  -> data.push_feedback_event()
  -> exception_event_q
```

`convert_raw_memory_violation()` 会把 memoryViolation 转成 redirect event：

```systemverilog
wb_event.source                 = MEMBLOCK_WB_EVENT_SOURCE_MEMORY_VIOLATION;
wb_event.target                 = MEMBLOCK_ISSUE_TARGET_NONE;
wb_event.redirect_valid         = 1'b1;
wb_event.redirect.valid         = 1'b1;
wb_event.redirect.flush_itself  = raw.memory_violation_level;
wb_event.redirect.level         = raw.memory_violation_level;
wb_event.redirect.rob_key       = wb_event.rob_key;
```

这说明 memoryViolation 不直接调用 `request_redirect_flush()`。它先进入 `exception_event_q`，再由 `process_pending_events()` 和同批其他 event 一起仲裁，保证 redirect 优先级和 ROB 年龄顺序一致。

#### 输入来源 3：队列内部 requeue/defer

除了 `push_feedback_event()`，还有两个“内部回写”场景。

第一个是 redirect 仲裁后的 requeue：

```systemverilog
data.exception_event_q.push_front(wb_item);
```

它发生在 `requeue_events_not_flushed_by_redirect()` 中。当前 service cycle 已经 pop 出来的非 redirect event，如果没有被当前 active redirect 覆盖，就用 `push_front()` 放回队列头，等待当前 redirect recovery 完成后再处理。

旧版曾有 writeback handler 的 deferred 回写路径，会 pop pending queue 后再把 replay/fault/redirect 放回。该路径已删除，避免 `exception_event_q` 出现 writeback handler 与 recovery handler 两个消费者。

#### 数据消耗路径

主消费方是 `exception_redirect_replay_handler::process_pending_events()`：

```systemverilog
while (data.pop_feedback_event(wb_event)) begin
    events.push_back(wb_event);
end
```

`pop_feedback_event()` 使用 FIFO：

```systemverilog
wb_event = exception_event_q.pop_front();
```

但需要注意，redirect requeue 使用 `push_front()`，所以“未被当前 redirect 覆盖的同批 event”会被放回队头，优先在 recovery 完成后的下一轮被重新处理。

`process_pending_events()` 消费后有两种主分支：

```text
如果本批 events 中有 redirect:
  select_oldest_redirect()
  request_redirect_flush()
  push_redirect_drive()
  requeue_events_not_flushed_by_redirect()
  return

如果本批 events 中没有 redirect:
  handle_replay_event()
  handle_fault_event()
```

这意味着 redirect 是该队列最高优先级动作。只要本批 event 中存在 redirect，普通 replay/fault 不会在同一轮继续处理，而是被丢弃或 requeue。

#### pending queue 单消费者

`writeback_status_handler` 不再 pop `exception_event_q`。writeback / feedback / memoryViolation raw event 经 adapter 转换后直接调用 `handle_event()` 分类：normal pass 立即落表，fault 首次标记后入队，replay/redirect 入队。入队后的 replay/fault/redirect 统一由 `exception_redirect_replay_handler::process_pending_events()` 仲裁。

#### 队列语义总结

`exception_event_q` 的完整数据流可以概括为：

```text
DUT raw monitor event
  -> dispatch_monitor_event_adapter 转 memblock_wb_event_t
  -> writeback_status_handler.handle_event()
       normal pass: 直接更新状态，不入队
       replay/fault/redirect: push_feedback_event()
  -> exception_event_q
  -> exception_redirect_replay_handler.process_pending_events()
       redirect: 选最老并启动 recovery
       replay: 设置 replay pending
       fault: 设置 fault/exception 状态
       被 redirect 覆盖的 stale event: 丢弃
       未覆盖 event: requeue 等 recovery 后处理
```

Review 判断：

- `exception_event_q` 的定位合理：它不是普通 writeback pass 队列，而是 replay/fault/redirect 的统一仲裁缓冲。
- raw monitor 不直接修改 recovery 状态，而是经 adapter、handler、队列后统一处理，能避免同拍多事件顺序分裂。
- redirect 优先级通过 `process_pending_events()` 和 writeback handler 的 queue guard 双重保证。
- `push_front()` requeue 只用于未被 redirect 覆盖的旧批次 event，能避免 redirect recovery 期间丢失 older/不相关反馈。

### 2.2 事件收集与最老 redirect 选择

核心函数在 `exception_redirect_replay_handler::process_pending_events()`：

```systemverilog
service_ptw_wait_replay();
advance_active_redirect();
if (data.active_redirect.valid) begin
    return;
end

while (data.pop_feedback_event(wb_event)) begin
    events.push_back(wb_event);
end

if (select_oldest_redirect(events, redirect_event)) begin
    redirect = redirect_from_event(redirect_event);
    ...
    data.request_redirect_flush(redirect);
    data.push_redirect_drive(redirect);
end
```

功能：

- 每次先处理已经存在的 active redirect。
- 如果 active redirect 还没结束，本轮直接返回，不再处理新事件。
- 如果没有 active redirect，则取出当前 feedback queue 中的所有事件。
- 如果其中有 redirect，使用 `select_oldest_redirect()` 选择 ROB 顺序最老的 redirect。

为什么要选择最老 redirect：

- 一个 service cycle 内可能同时采到多个 redirect 事件。
- 处理器 flush 语义以 ROB 年龄为准；更老的 redirect 会覆盖它之后的 younger uop。
- 如果先处理 younger redirect，随后 older redirect 又覆盖更大范围，软件状态可能经历无意义甚至冲突的中间状态。

Review 判断：

- `select_oldest_redirect()` 的存在是必要的。
- 当前逻辑在 active redirect 期间不处理新事件，能避免多个 redirect/replay/fault 同时修改状态表。

### 2.2.1 `process_pending_events()` 内部子函数展开

`process_pending_events()` 是 redirect/replay/fault recovery 的主服务函数。它内部的子函数不能只按调用链理解，因为这些子函数会决定本轮是否继续处理新事件、是否进入 redirect recovery、是否把 replay 延后等待 PTW、以及哪些事件会被丢弃或重新入队。

调用顺序和主流程影响如下：

| 调用顺序 | 子函数 | 简要功能 | 主流程影响 |
|---|---|---|---|
| 1 | `ensure_data()` | 确保 `data` 指向 `common_data_transaction` 单例。 | 后续所有状态表、事件队列、redirect 状态访问都依赖它。 |
| 2 | `service_ptw_wait_replay()` | 释放已经满足条件或超时的 PTW-back replay 等待项。 | 可能调用 `mark_replay_pending()`，但不启动 redirect。 |
| 3 | `advance_active_redirect()` | 如果已有 active redirect，则等待真实 redirect drive done；完成后调用 `apply_redirect_flush()`。 | 若 active redirect 仍有效，主函数随后提前 return，不再处理新事件。 |
| 4 | `pop_feedback_event()` | 从公共 feedback queue 取出本轮待处理 event。 | 本轮 event 先进入局部 `events[$]`，之后统一选择 redirect 或处理 replay/fault。 |
| 5 | `select_oldest_redirect()` | 从本轮 event 中选择 ROB 顺序最老的 redirect。 | 如果找到 redirect，进入 freeze 和 redirect drive queue 投递。 |
| 6 | `redirect_from_event()` | 从 writeback event 中提取 redirect payload。 | payload 无效会 fatal，避免无效 redirect 进入 recovery。 |
| 7 | `request_redirect_flush()` | 设置全局 flush/freeze 状态并记录 `active_redirect`。 | admission、issue、commit 等路径开始被 `issue_blocked_by_global_flush()` 阻塞。 |
| 8 | `push_redirect_drive()` | 将 redirect payload 放入待驱动队列。 | redirect sequence 后续真实驱动 DUT `io_redirect`。 |
| 9 | `requeue_events_not_flushed_by_redirect()` | 对同批非 redirect event 做 stale drop 或重新入队。 | 被 redirect 覆盖的旧路径 event 丢弃，未覆盖 event 保留到 recovery 后继续处理。 |
| 10 | `handle_replay_event()` / `handle_fault_event()` | 在没有 redirect 时处理 replay/fault。 | replay 可能重新入队 target；fault 只消费 recovery 队列事件，目标 fault 状态已由 writeback handler 首次落表。 |

这张表体现了 `process_pending_events()` 的一个关键规则：**redirect 优先级高于本批普通 replay/fault**。一旦本批 events 中存在 redirect，handler 会先建立 active redirect，随后把未被覆盖的普通 event 放回队列，等 redirect recovery 完成后再处理。

### 2.2.2 `service_ptw_wait_replay()` 内部逻辑

源码片段：

```systemverilog
function void service_ptw_wait_replay();
    memblock_ptw_wait_replay_t wait_item;
    bit timed_out;

    ensure_data();
    while (data.pop_ready_ptw_wait_replay(seq_csr_common::get_replay_wait_ptw_timeout(),
                                          wait_item,
                                          timed_out)) begin
        if (timed_out) begin
            `uvm_warning("EXC_REDIRECT",
                         $sformatf("release ptw_wait_replay by timeout uid=%0d target=%0d replay_seq=%0d",
                                   wait_item.uid,
                                   wait_item.target,
                                   wait_item.replay_seq))
        end
        void'(data.mark_replay_pending(wait_item.uid,
                                       wait_item.target,
                                       wait_item.issue_epoch,
                                       wait_item.replay_seq,
                                       memblock_sync_pkg::get_dispatch_service_cycle()));
    end
endfunction
```

功能：

- 处理之前被延后的 PTW-back replay。
- PTW-back replay 指的是 replay 事件依赖 L2TLB/PTW response 回填结果，不能在 event 刚到时马上重新发射。
- 该函数每次从 `ptw_wait_replay_q` 中弹出已经 ready 或已经 timeout 的等待项。

输入来源：

- 等待项来自 `handle_replay_event()` 中的 `data.push_ptw_wait_replay()`。
- timeout 配置来自 `seq_csr_common::get_replay_wait_ptw_timeout()`。
- 当前处理周期来自 `memblock_sync_pkg::get_dispatch_service_cycle()`。

输出和副作用：

- 如果等待项 ready 或 timeout，则调用 `data.mark_replay_pending()`。
- `mark_replay_pending()` 会根据 target 设置 replay pending 状态，并让后续 issue 路由重新发对应 target。
- 如果是 timeout 释放，会打印 warning，但仍然推进 replay pending，避免永远卡死。

为什么它放在 `advance_active_redirect()` 前：

- PTW wait replay 是已经挂起的 replay 等待项，需要周期性服务。
- 但它不会启动 redirect，也不会清 active redirect。
- 即使随后发现 active redirect 仍未完成，当前函数释放出的 replay pending 仍会受到全局 flush 阻塞或后续 redirect stale 过滤保护。

Review 判断：

- 该函数是 replay 侧的后台服务，不是 redirect 主恢复入口。
- 当前文档必须把它展开，否则读者容易误以为 `process_pending_events()` 一进来只处理 redirect。

### 2.2.3 `advance_active_redirect()` 内部逻辑

源码片段：

```systemverilog
function void advance_active_redirect();
    memblock_redirect_payload_t redirect;

    if (!data.active_redirect.valid) begin
        return;
    end
    redirect = data.active_redirect;
    if (data.redirect_drive_done_for(redirect)) begin
        data.apply_redirect_flush(redirect);
    end else if (seq_csr_common::is_initialized() &&
                 seq_csr_common::get_redirect_freeze_timeout() != 0 &&
                 (memblock_sync_pkg::get_dispatch_service_cycle() - data.redirect_freeze_cycle) >=
                     seq_csr_common::get_redirect_freeze_timeout()) begin
        `uvm_fatal("EXC_REDIRECT",
                   $sformatf("timeout waiting redirect drive rob=%0d/%0d",
                             redirect.rob_key.flag,
                             redirect.rob_key.value))
    end
endfunction
```

功能：

- 推进已经进入 recovery 的 active redirect。
- 如果 redirect payload 已经由 redirect sequence 真实驱动完成，并且跨过 drive done cycle，则调用 `apply_redirect_flush()` 清软件状态。
- 如果长时间没有 drive done，则 fatal，定位 redirect sequence 没启动、agent 没挂接或 payload 没被消费的问题。

输入来源：

- `data.active_redirect`：由 `request_redirect_flush()` 设置的当前 redirect payload。
- `data.redirect_freeze_cycle`：开始 freeze 的 service cycle。
- `seq_csr_common::get_redirect_freeze_timeout()`：plus 配置的 freeze timeout。

输出和副作用：

- drive done 时调用 `data.apply_redirect_flush(redirect)`。
- `apply_redirect_flush()` 会批量清理被 redirect 覆盖的 uid，清 PTW wait replay，清 redirect drive queue，并释放全局 flush/freeze 状态。
- timeout 时直接 fatal，不允许系统永久 freeze。

回到主流程后的影响：

```systemverilog
advance_active_redirect();
if (data.active_redirect.valid) begin
    return;
end
```

- 如果 `advance_active_redirect()` 没能完成 flush，`active_redirect` 仍有效，`process_pending_events()` 立即返回。
- 这保证 active redirect 期间不会继续弹出和处理新的 feedback event。
- 如果 `advance_active_redirect()` 完成了 `apply_redirect_flush()`，`active_redirect` 会被清零，本轮可以继续处理新的 event。

Review 判断：

- 这个函数是 redirect 跨周期状态机的推进点，必须在文档中展开说明。
- 当前逻辑符合“先 drive redirect，再 apply software flush”的阶段化要求。

### 2.2.4 `select_oldest_redirect()` 与 `redirect_event_is_older()`

源码片段：

```systemverilog
function bit select_oldest_redirect(input memblock_wb_event_t events[$],
                                    output memblock_wb_event_t selected);
    bit found;

    found = 1'b0;
    selected = data.make_empty_wb_event();
    foreach (events[idx]) begin
        if (!event_is_redirect(events[idx])) begin
            continue;
        end
        if (!found || redirect_event_is_older(events[idx], selected)) begin
            selected = events[idx];
            found = 1'b1;
        end
    end
    return found;
endfunction
```

```systemverilog
function bit redirect_event_is_older(input memblock_wb_event_t candidate,
                                     input memblock_wb_event_t best);
    memblock_redirect_payload_t cand_redirect;
    memblock_redirect_payload_t best_redirect;

    cand_redirect = redirect_from_event(candidate);
    best_redirect = redirect_from_event(best);
    if (cand_redirect.rob_key.flag == best_redirect.rob_key.flag &&
        cand_redirect.rob_key.value == best_redirect.rob_key.value) begin
        return candidate.port_id < best.port_id;
    end
    return rob_order_util::rob_is_after(best_redirect.rob_key, cand_redirect.rob_key);
endfunction
```

功能：

- `select_oldest_redirect()` 从本轮局部 event 数组里挑出一个最老 redirect。
- `redirect_event_is_older()` 用 ROB 环形顺序判断 candidate 是否比当前 best 更老。
- 如果两个 redirect 的 ROB key 完全相同，则用 `port_id` 做稳定 tie-break。

输入来源：

- `events[$]` 来自本轮从 `exception_event_q` 弹出的 feedback event。
- 每个 redirect event 内部携带 `redirect.rob_key` 和 `port_id`。

输出和副作用：

- 函数只返回 selected event，不直接修改公共状态。
- 真正修改公共状态发生在后续 `request_redirect_flush()`。

Review 判断：

- 选择最老 redirect 是正确的；不能按 event 入队顺序或 port 顺序直接选。
- 使用 `rob_order_util` 是必要约束，避免 ROB wrap 后裸 value 比较错误。

### 2.2.5 `handle_replay_event()` 和 `handle_fault_event()`

源码片段：

```systemverilog
function void handle_replay_event(input memblock_wb_event_t wb_event);
    memblock_uid_t uid;
    int unsigned   issue_epoch;
    int unsigned   replay_seq;

    if (!data.resolve_uid_for_event(wb_event, uid)) begin
        `uvm_warning("EXC_REDIRECT", "drop replay wb_event because active uid lookup failed")
        return;
    end
    issue_epoch = data.get_event_issue_epoch(wb_event, uid);
    replay_seq  = data.get_event_replay_seq(wb_event, uid);
    if (event_should_wait_ptw(wb_event)) begin
        data.push_ptw_wait_replay(uid,
                                  wb_event.target,
                                  issue_epoch,
                                  replay_seq,
                                  memblock_sync_pkg::get_dispatch_service_cycle());
        return;
    end
    void'(data.mark_replay_pending(uid, wb_event.target, issue_epoch, replay_seq, wb_event.cycle));
endfunction
```

```systemverilog
function void handle_fault_event(input memblock_wb_event_t wb_event);
    memblock_uid_t uid;
    int unsigned   issue_epoch;
    int unsigned   replay_seq;

    // fault target 状态已经由 writeback_status_handler 首次采到 fault 时落表。
    // 这里只消费 recovery 队列中的 fault event，不重复写 target fault 状态。
    if (!data.resolve_uid_for_event(wb_event, uid)) begin
        `uvm_warning("EXC_REDIRECT", "drop fault wb_event because active uid lookup failed")
        return;
    end
    issue_epoch = data.get_event_issue_epoch(wb_event, uid);
    replay_seq  = data.get_event_replay_seq(wb_event, uid);
    `uvm_info("EXC_REDIRECT",
              $sformatf("consume fault recovery event uid=%0d target=%0d issue_epoch=%0d replay_seq=%0d",
                        uid, wb_event.target, issue_epoch, replay_seq),
              UVM_HIGH)
endfunction
```

功能：

- `handle_replay_event()` 处理没有 redirect 抢占时的 replay event。
- `handle_fault_event()` 处理没有 redirect 抢占时的 fault event。
- 两者都先通过 event 中的 ROB/LQ/SQ key 反查当前 active uid，再用 issue epoch 和 replay seq 保护旧事件不会更新新动态实例。

输入来源：

- `wb_event` 来自本轮 feedback event 数组。
- uid 解析依赖 `common_data_transaction` 中的 active ROB/LQ/SQ map。
- `issue_epoch/replay_seq` 来自 event 或 status snapshot。

输出和副作用：

- replay event 可能直接调用 `mark_replay_pending()`，或者进入 `ptw_wait_replay_q`。
- fault event 只消费 recovery 队列事件并解析 uid/epoch/replay_seq；目标 fault/exception 状态已由 writeback handler 首次采到 fault 时落表，不在 recovery handler 中重复写状态。
- 如果 uid 反查失败，只 warning 并丢弃，不 fatal。原因是 redirect/flush 后旧事件可能已经失去 active map。

Review 判断：

- replay/fault 处理被放在 redirect 之后是合理的；redirect 一旦存在，本轮 replay/fault 不会立即处理。
- uid 反查失败时 drop 是合理边界，避免 flush 后 stale event 误伤新实例。

### 2.3 stale event 处理

当本轮选中 redirect 后：

```systemverilog
if (data.active_redirect.valid) begin
    requeue_events_not_flushed_by_redirect(events, data.active_redirect);
    return;
end
```

`requeue_events_not_flushed_by_redirect()` 的逻辑：

```systemverilog
if (wb_item.has_rob &&
    rob_order_util::rob_need_flush(wb_item.rob_key, redirect)) begin
    continue;
end
data.exception_event_q.push_front(wb_item);
```

功能：

- 对同批已经弹出的非 redirect event 做二次处理。
- 如果 event 的 ROB 被当前 redirect 覆盖，说明它属于旧路径 stale feedback，直接丢弃。
- 如果 event 不被当前 redirect 覆盖，则重新放回 `exception_event_q`，等 redirect recovery 完成后再处理。

Review 判断：

- 这一步是必要的。否则本轮已经 pop 出来的 replay/fault/pass 事件可能在 active redirect 期间丢失。
- 对被 redirect 覆盖的 event 直接丢弃也合理，因为它们不应更新新动态实例状态。

## 3. Freeze 与 Redirect Drive

### 3.1 `request_redirect_flush()`

源码：

```systemverilog
function void request_redirect_flush(input memblock_redirect_payload_t redirect);
    if (!redirect.valid) begin
        `uvm_fatal("COMMON_DATA", "request_redirect_flush requires valid redirect")
    end
    redirect_phase    = MEMBLOCK_REDIRECT_PHASE_DETECTED;
    flush_in_progress = 1'b1;
    memblock_sync_pkg::dispatch_flush_in_progress = 1'b1;
    memblock_sync_pkg::dispatch_flush_epoch++;
    issue_freeze_ack  = 1'b1;
    active_redirect   = redirect;
    redirect_phase    = MEMBLOCK_REDIRECT_PHASE_FREEZE_REQUESTED;
    redirect_freeze_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
endfunction
```

字段含义和作用：

| 字段 | 含义 | 在主路径中的作用 |
|---|---|---|
| `redirect_phase` | redirect recovery 阶段状态。 | debug 和 `redirect_drive_done_for()` 判断都依赖它。 |
| `flush_in_progress` | `common_data_transaction` 内部 flush 标志。 | 阻止 issue/admission/commit 继续推进旧上下文。 |
| `dispatch_flush_in_progress` | `memblock_sync_pkg` 全局 flush 标志。 | 给跨组件的 sequence/adapter 提供统一阻塞条件。 |
| `dispatch_flush_epoch` | 全局 flush 轮次。 | 用于区分 flush 前后的状态/事件周期。 |
| `issue_freeze_ack` | TB 软件侧冻结已确认。 | `issue_blocked_by_global_flush()` 会检查它。 |
| `active_redirect` | 当前正在处理的 redirect payload。 | 后续 drive done、range flush、stale event drop 都使用它。 |
| `redirect_freeze_cycle` | 开始 freeze 的 service cycle。 | 用于 redirect freeze timeout 检查。 |

Review 判断：

- 发现 redirect 后先 freeze，而不是立即 `apply_redirect_flush()`，这是当前实现中最重要的正确性点。
- `active_redirect` 是单例状态，意味着当前模型一次只处理一个 redirect。这符合简化 TB recovery 设计。

### 3.2 `issue_blocked_by_global_flush()`

源码：

```systemverilog
function bit issue_blocked_by_global_flush();
    return flush_in_progress ||
           active_redirect.valid ||
           issue_freeze_ack ||
           has_pending_redirect_drive() ||
           memblock_sync_pkg::dispatch_flush_in_progress;
endfunction
```

功能：

- 为 issue queue scheduler、LSQ admission、commit handler、LSQ commit sequence 提供统一阻塞判断。
- 只要 redirect 还在检测、冻结、待驱动、已驱动未清理中的任一状态，就阻止旧路径继续推进。

Review 判断：

- 阻塞条件覆盖比较完整，不只看 `active_redirect`。
- `has_pending_redirect_drive()` 能覆盖 payload 已排队但还没被 redirect sequence 消费的窗口。

### 3.3 `push_redirect_drive()` / `try_pop_redirect_drive()`

源码：

```systemverilog
function void push_redirect_drive(input memblock_redirect_payload_t payload);
    pending_redirect_drive_q.push_back(payload);
endfunction

function bit try_pop_redirect_drive(output memblock_redirect_payload_t payload);
    if (pending_redirect_drive_q.size() == 0 || redirect_drive_inflight) begin
        payload = '{default:'0};
        return 1'b0;
    end
    payload = pending_redirect_drive_q.pop_front();
    redirect_drive_inflight_payload = payload;
    redirect_drive_inflight = 1'b1;
    return 1'b1;
endfunction
```

功能：

- `push_redirect_drive()` 将 recovery payload 投递给 redirect sequence。
- `try_pop_redirect_drive()` 保证同一时间只有一个 redirect payload 处于 inflight。

Review 判断：

- 单 inflight 设计合理。redirect recovery 本身是全局 flush 级别事件，不应并发驱动多个 recovery payload。

### 3.4 `memblock_redirect_dispatch_sequence`

核心逻辑：

```systemverilog
forever begin
    if (data.try_pop_redirect_drive(payload)) begin
        drive_redirect_payload(payload);
        idle_count = 0;
    end else begin
        drive_idle_once(...);
        ...
    end
end
```

`drive_redirect_payload()`：

```systemverilog
assign_redirect_xaction(tr, payload);
start_item(tr);
finish_item(tr);
data.mark_redirect_drive_done(payload);
```

功能：

- 常驻在 redirect agent 上。
- 没有 payload 时驱动 idle transaction。
- 有 payload 时真实驱动 `io_redirect_valid`、`io_redirect_bits_level`、`io_redirect_bits_robIdx_*`。
- drive 完成后调用 `mark_redirect_drive_done()`。

Review 判断：

- 当前实现不是只在 TB 内部清状态，而是经过 redirect agent 真实驱动 DUT input，这是正确的闭环行为。
- `MEMBLOCK_REDIRECT_SEQ_EN=0` 时，如果出现 redirect event，handler 会 fatal，避免 “需要 redirect 但没人驱动接口” 的 silent failure。

## 4. Drive Done 与跨拍保护

### 4.1 `mark_redirect_drive_done()`

源码：

```systemverilog
function void mark_redirect_drive_done(input memblock_redirect_payload_t payload);
    if (redirect_drive_inflight &&
        !redirect_payload_equal(payload, redirect_drive_inflight_payload)) begin
        `uvm_fatal("COMMON_DATA", "mark_redirect_drive_done got payload that does not match inflight redirect")
    end
    redirect_drive_inflight = 1'b0;
    redirect_drive_inflight_payload = '{default:'0};
    redirect_drive_done_epoch++;
    redirect_drive_done_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    if (active_redirect.valid && redirect_payload_equal(payload, active_redirect)) begin
        redirect_phase = MEMBLOCK_REDIRECT_PHASE_REDIRECT_DRIVEN;
    end
endfunction
```

功能：

- 校验完成的 payload 必须和 inflight payload 一致。
- 记录 drive done 的 service cycle。
- 将阶段推进到 `MEMBLOCK_REDIRECT_PHASE_REDIRECT_DRIVEN`。

Review 判断：

- inflight payload 校验是必要的，能防止队列/sequence 顺序错误。
- 记录 `redirect_drive_done_cycle` 是跨拍保护的基础。

### 4.2 `redirect_drive_done_for()`

源码：

```systemverilog
function bit redirect_drive_done_for(input memblock_redirect_payload_t payload);
    if (redirect_drive_inflight && redirect_payload_equal(payload, redirect_drive_inflight_payload)) begin
        return 1'b0;
    end
    foreach (pending_redirect_drive_q[idx]) begin
        if (redirect_payload_equal(payload, pending_redirect_drive_q[idx])) begin
            return 1'b0;
        end
    end
    return redirect_drive_done_epoch != 0 &&
           redirect_phase >= MEMBLOCK_REDIRECT_PHASE_REDIRECT_DRIVEN &&
           memblock_sync_pkg::get_dispatch_service_cycle() > redirect_drive_done_cycle;
endfunction
```

功能：

- 确认 payload 不在 pending queue 中。
- 确认 payload 不在 inflight 中。
- 确认已经进入 `REDIRECT_DRIVEN` 阶段。
- 要求当前 service cycle 大于 drive done cycle。

为什么要求跨 cycle：

- redirect sequence 驱动 `io_redirect` 后，DUT 和其他 monitor/sequence 对该拍的观察可能还没完成。
- 如果同一个 service cycle 内马上清公共状态，可能出现“边 drive redirect，边让 LSQ/issue 恢复”的竞态。

Review 判断：

- 跨 cycle 条件正确，是当前主路径避免同拍状态竞争的关键保护。

## 5. 批量 Flush 与 Reissue 准备

### 5.1 `advance_active_redirect()`

源码：

```systemverilog
function void advance_active_redirect();
    if (!data.active_redirect.valid) begin
        return;
    end
    redirect = data.active_redirect;
    if (data.redirect_drive_done_for(redirect)) begin
        data.apply_redirect_flush(redirect);
    end else if (...) begin
        `uvm_fatal("EXC_REDIRECT", "timeout waiting redirect drive ...")
    end
endfunction
```

功能：

- active redirect 存在时，只推进它。
- 如果 redirect sequence 已完成真实 drive，则开始软件状态 flush。
- 如果长时间没 drive done，则 fatal。

Review 判断：

- timeout 检查必要。否则 redirect sequence 没启动、agent 没挂接、payload 没被消费时，主流程会永久 freeze。

### 5.2 `apply_redirect_flush()`

源码：

```systemverilog
function void apply_redirect_flush(input memblock_redirect_payload_t redirect);
    apply_redirect_flush_range(redirect);
    clear_ptw_wait_replay_by_redirect(redirect);
    clear_redirect_drive_queue();
    redirect_phase = MEMBLOCK_REDIRECT_PHASE_STATE_FLUSH_APPLIED;
    flush_in_progress  = 1'b0;
    memblock_sync_pkg::dispatch_flush_in_progress = 1'b0;
    issue_freeze_ack   = 1'b0;
    active_redirect    = '{default:'0};
    redirect_phase     = MEMBLOCK_REDIRECT_PHASE_IDLE;
endfunction
```

功能：

- 调用范围扫描清理被 redirect 覆盖的 uid。
- 清理被 redirect 覆盖的 PTW wait replay。
- 清掉 redirect drive 队列残留。
- 释放全局 flush/freeze 状态。

Review 判断：

- `apply_redirect_flush()` 是公共状态 flush 总入口。
- 它在最后才释放 `flush_in_progress` 和 `active_redirect`，保证清状态期间其他模块仍被阻塞。

#### 5.2.1 `apply_redirect_flush()` 内部子函数展开

| 调用顺序 | 子函数 | 简要功能 | 主流程影响 |
|---|---|---|---|
| 1 | `apply_redirect_flush_range(redirect)` | 扫描活跃 uid 窗口，找出被 ROB redirect 覆盖的 uid 并准备 reissue。 | 这是真正清 uid 状态的地方。 |
| 2 | `clear_ptw_wait_replay_by_redirect(redirect)` | 删除无效或被 redirect 覆盖的 PTW wait replay 等待项。 | 防止旧路径 replay 在 flush 后重新生效。 |
| 3 | `clear_redirect_drive_queue()` | 清空 redirect drive 队列和 inflight 标志。 | recovery 收尾，避免残留 payload 影响下一次 redirect。 |

`clear_ptw_wait_replay_by_redirect()` 源码逻辑：

```systemverilog
for (int idx = ptw_wait_replay_q.size(); idx > 0; idx--) begin
    status_transaction status;

    if (!is_valid_uid(ptw_wait_replay_q[idx - 1].uid)) begin
        ptw_wait_replay_q.delete(idx - 1);
        continue;
    end
    status = get_status(ptw_wait_replay_q[idx - 1].uid);
    if (!status.active ||
        rob_order_util::rob_need_flush(status.get_rob_key(), redirect)) begin
        ptw_wait_replay_q.delete(idx - 1);
    end
end
```

它的功能是清理 `ptw_wait_replay_q`。这个队列保存“已经收到 PTW-back replay，但还在等 L2TLB/PTE 回填或 timeout 的 replay 项”。redirect flush 后，如果等待项对应 uid 已经无效、已经 inactive，或者 ROB 被当前 redirect 覆盖，就不能继续保留。否则旧路径 replay 会在 flush 完成后又被 `service_ptw_wait_replay()` 释放出来，导致已被 kill 的 uid/target 重新进入 replay 路径。

`clear_redirect_drive_queue()` 的功能是清理 `pending_redirect_drive_q` 和 `redirect_drive_inflight`。它位于 `apply_redirect_flush_range()` 之后，说明只有状态 flush 完成后才允许清掉 drive 队列状态。这个顺序能避免“payload 已经排队或 inflight，但软件状态先恢复 idle”的错误。

### 5.3 `apply_redirect_flush_range()`

源码：

```systemverilog
advance_success_prefix();
begin_uid = get_active_scan_begin_uid();
end_uid   = get_active_scan_end_uid();

for (memblock_uid_t uid = begin_uid; uid < end_uid; uid++) begin
    status = get_status(uid);
    if (status.success || (!status.active && !status.writeback && !status.pass)) begin
        continue;
    end
    rob_key = status.get_rob_key();
    if (rob_order_util::rob_need_flush(rob_key, redirect)) begin
        ...
        prepare_uid_for_redirect_reissue(uid, redirect);
    end
end
if (found_flushed) begin
    rollback_max_enqueued_uid(oldest_flushed_uid);
end
```

功能：

- 只扫描公共活跃窗口 `[success_prefix_uid, max_enqueued_uid + 1)`。
- 跳过已经 `success` 的 uid。
- 对 active 或已经 writeback/pass 但未 success 的 uid，用 ROB 顺序判断是否被当前 redirect flush 覆盖。
- 命中 uid 调用 `prepare_uid_for_redirect_reissue()`。
- 记录最老被 flush uid，并把 LSQ admission 上界回退到它前一个。

为什么不能用 uid 大小判断 flush：

- uid 是测试框架主表索引，只代表生成顺序。
- DUT flush 语义基于 ROB 环形指针 `{flag,value}`。
- ROB wrap 后，裸 `robIdx_value` 或 uid 大小都可能判断错。
- 因此必须调用 `rob_order_util::rob_need_flush()`。

Review 判断：

- 范围扫描比全表扫描更适合 100k testcase。
- 但是否 flush 仍使用 ROB helper，未把 uid scan window 当作 flush 语义本身，这一点正确。

#### 5.3.1 `apply_redirect_flush_range()` 内部子函数展开

| 调用顺序 | 子函数 | 简要功能 | 主流程影响 |
|---|---|---|---|
| 1 | `advance_success_prefix()` | 推进从 uid0 开始连续 success 的前缀。 | 确定本轮 redirect scan 的起点，已连续完成的 uid 不再扫描。 |
| 2 | `get_active_scan_begin_uid()` | 返回 `success_prefix_uid`。 | 活跃扫描窗口起点。 |
| 3 | `get_active_scan_end_uid()` | 返回 `max_enqueued_uid + 1`，若还没有 admission 则返回 `success_prefix_uid`。 | 活跃扫描窗口终点。 |
| 4 | `rob_order_util::rob_need_flush(rob_key, redirect)` | 按 ROB 环形顺序判断该 uid 是否被 redirect 覆盖。 | 只有命中才调用 reissue 准备。 |
| 5 | `prepare_uid_for_redirect_reissue(uid, redirect)` | 清该 uid 旧动态实例并置 redirect pending。 | 被 flush uid 进入等待重新 admission 的中间态。 |
| 6 | `rollback_max_enqueued_uid(oldest_flushed_uid)` | 将 LSQ admission 上界回退到最老 flushed uid 前一个。 | 后续 LSQ admission 会从最老 flushed uid 重新开始。 |

`advance_success_prefix()` 与扫描窗口：

```systemverilog
while (dispatch_progress.success_prefix_uid < main_trans_num) begin
    status = get_status(dispatch_progress.success_prefix_uid);
    if (!status.success) begin
        break;
    end
    dispatch_progress.success_prefix_uid++;
end
```

`success_prefix_uid` 表示“从 uid0 开始连续 success 后的第一个未完成 uid”。redirect flush 不需要再扫描这个前缀之前的 uid，因为它们已经是终态成功，`prepare_uid_for_redirect_reissue()` 也禁止 flush 已 success uid。该 helper 把 scan 起点推进到最早未完成 uid，从而减少 100k 长流场景的扫描量。

`get_active_scan_end_uid()` 的核心逻辑：

```systemverilog
if (!dispatch_progress.max_enqueued_uid_valid) begin
    return dispatch_progress.success_prefix_uid;
end
return dispatch_progress.max_enqueued_uid + 1;
```

`max_enqueued_uid` 是当前连续有效 LSQ admission 上界。redirect flush 只需要扫描已经 admission 过、可能 active 或已有 writeback/pass 的 uid；还没 admission 的 uid 不可能已经进入 DUT 旧路径，也不需要清旧动态实例。

`rob_need_flush()` 的核心逻辑：

```systemverilog
same_rob = rob_to_map_key(uop_rob) == rob_to_map_key(redirect.rob_key);
return (redirect.flush_itself && same_rob) || rob_is_after(uop_rob, redirect.rob_key);
```

这个 helper 表达 DUT redirect flush 语义：如果 `flush_itself=1`，redirect 自己所在 ROB 也被覆盖；无论是否 flush itself，ROB 年龄在 redirect 之后的 younger uop 都要被 flush。这里必须使用 ROB key 和环形顺序 helper，不能用 uid 或 `robIdx_value` 裸比较。

`rollback_max_enqueued_uid(oldest_flushed_uid)` 的功能是回退 LSQ admission 上界。它不清具体 uid 状态，具体状态已经由 `prepare_uid_for_redirect_reissue()` 完成；它只负责让下一条新 admission uid 重新指向最老 flushed uid。这样后续 LSQ sequence 不需要自己维护一份 redirect reissue 列表。

### 5.4 `prepare_uid_for_redirect_reissue()`

源码核心：

```systemverilog
status = get_status(uid);
if (status.success) begin
    `uvm_fatal("COMMON_DATA",
               $sformatf("redirect tries to flush already success uid=%0d", uid))
end

main_tr = get_main_transaction(uid);
had_lq_mapping = status.active_lq_mapped;
had_sq_mapping = status.active_sq_mapped;

if (status.active) begin
    retire_active_uid(uid);
end else begin
    remove_uid_from_issue_queues(uid);
end

if (had_lq_mapping) begin
    pending_lq_cancel_count += main_tr.numLsElem;
end
if (had_sq_mapping) begin
    pending_sq_cancel_count += main_tr.numLsElem;
end

clear_uid_dispatch_result(uid);
status.redirect_pending = 1'b1;
status.flushed          = 1'b1;
status.dynamic_epoch++;
status.active           = 1'b0;
status.success          = 1'b0;
status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
```

功能：

- 对单个被 flush uid 清理旧动态实例。
- 删除 issue queue 残留项。
- 删除 active ROB/LQ/SQ map，避免旧 event 继续命中新状态。
- 累计 LQ/SQ cancel 数，交给后续 LSQ admission sequence 回退软件 LSQ 镜像。
- 清 writeback/pass/fault/commit/deq 等旧结果。
- 置 `redirect_pending/flushed`，表示该 uid 不是终态，而是等待 reissue。
- 递增 `dynamic_epoch`，区分同 uid 的新旧动态实例。

Review 判断：

- `prepare_uid_for_redirect_reissue()` 是 reissue 准备的单一入口，避免状态清理散落在多个 task。
- 它不直接重新入队，也不直接发射，这是正确的。真正重新入队由 LSQ admission 高水位回退后自然发生。

#### 5.4.1 `prepare_uid_for_redirect_reissue()` 内部子函数展开

| 调用顺序 | 子函数 | 简要功能 | 主流程影响 |
|---|---|---|---|
| 1 | `get_status(uid)` | 取得 uid 当前运行状态。 | 判断是否已经 success，以及是否 active/mapped。 |
| 2 | `get_main_transaction(uid)` | 取得主表 transaction。 | 读取 `numLsElem`，用于计算 LQ/SQ cancel 数。 |
| 3 | `retire_active_uid(uid)` | 删除 active ROB/LQ/SQ map，并删除 issue queue 残留。 | active 旧实例彻底从反查 map 中摘除。 |
| 4 | `remove_uid_from_issue_queues(uid)` | 删除 LOAD/STA/STD 队列中的该 uid 项。 | inactive 但仍有队列残留时，防止旧 item 继续发射。 |
| 5 | `clear_uid_dispatch_result(uid)` | 清旧动态实例的 enq/issue/writeback/pass/fault/commit/deq 状态。 | 新动态实例不会继承旧结果。 |

`retire_active_uid(uid)` 的功能：

- 先调用 `remove_uid_from_issue_queues(uid)` 清发射队列残留。
- 检查并删除 `uid_by_active_rob` 中的 ROB -> uid 映射。
- 如果 `active_lq_mapped=1`，检查并删除 `uid_by_lq` 中的 LQ -> uid 映射。
- 如果 `active_sq_mapped=1`，检查并删除 `uid_by_sq` 中的 SQ -> uid 映射。
- 最后清 `status.active=0`。

这一步是 redirect reissue 的关键保护。旧动态实例如果还留在 active map 中，后续迟到的 writeback、deq、replay event 可能通过同一个 ROB/LQ/SQ key 反查到这个 uid，并错误更新已经准备重发的新实例。

`remove_uid_from_issue_queues(uid)` 的功能：

```systemverilog
delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_LOAD, uid, 0, 1'b0);
delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_STA, uid, 0, 1'b0);
delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_STD, uid, 0, 1'b0);
status_by_uid[uid].queued_load = 1'b0;
status_by_uid[uid].queued_sta  = 1'b0;
status_by_uid[uid].queued_std  = 1'b0;
```

它清理三个 issue queue 中该 uid 的残留项。这里不按 target 精确删除，是因为 redirect flush 是整条 transaction 的旧动态实例失效，不是普通 replay 只重发某个 target。对 store 来说，STA/STD 中任何旧队列项都不能继续发射。

`clear_uid_dispatch_result(uid)` 的功能：

- 清 `enq/issue_ready/queued_*`，表示旧实例不再处于已入队或待发射状态。
- 清 `*_dispatched/*_writeback/*_pass/writeback/pass`，表示旧实例发射和写回结果作废。
- 清 `fault/exception_pending/replay_pending` 以及 exception 信息，避免旧异常或旧 replay 污染新实例。
- 清 `rob_commit/lsq_deq/success`，表示旧实例不能提交完成。
- 设置 `issue_killed=1`，标记当前旧 issue 结果已被 kill。

该函数会先把 `redirect_pending` 清 0，随后 `prepare_uid_for_redirect_reissue()` 再置回 1。这不是逻辑冲突，而是“先统一清旧动态实例所有 pending 状态，再按 redirect reissue 语义设置新的 pending 状态”。这样可以避免旧 replay/fault pending 与新 redirect pending 混在一起。

## 6. LSQ 软件镜像回退

### 6.1 为什么需要 pending cancel

redirect flush 命中的 uid 如果已经占用了 LQ/SQ 分配，软件侧 `lsq_ctrl_model` 的 enq pointer 也必须回退。否则后续同 uid reissue 时，TB 预测出来的 LQ/SQ key 会继续往后走，和 DUT redirect/cancel 后的资源状态不一致。

当前实现没有在 `prepare_uid_for_redirect_reissue()` 里直接调用 `lsq_ctrl.cancel_lq/cancel_sq()`，而是累加：

```systemverilog
pending_lq_cancel_count += main_tr.numLsElem;
pending_sq_cancel_count += main_tr.numLsElem;
```

这样做的原因：

- `common_data_transaction` 不直接持有或操作 `lsq_ctrl_model`。
- 避免 redirect handler 和 LSQ admission sequence 同时改 LSQ 软件镜像。
- 所有 LSQ 资源镜像回退集中在 LSQ admission sequence 开头完成。

### 6.2 `apply_pending_lsq_cancels()`

源码：

```systemverilog
function void memblock_lsqenq_dispatch_sequence::apply_pending_lsq_cancels();
    ensure_helpers();
    if (data.pending_lq_cancel_count != 0) begin
        lsq_ctrl.cancel_lq(data.pending_lq_cancel_count);
        data.pending_lq_cancel_count = 0;
    end
    if (data.pending_sq_cancel_count != 0) begin
        lsq_ctrl.cancel_sq(data.pending_sq_cancel_count);
        data.pending_sq_cancel_count = 0;
    end
endfunction
```

调用位置：

```systemverilog
task memblock_lsqenq_dispatch_sequence::send_lsqenq_cycle(...);
    has_progress = 1'b0;
    apply_pending_lsq_cancels();
    ...
endtask
```

Review 判断：

- 回退时机正确：在下一轮 LSQ enqueue 尝试之前先修正软件 LSQ 镜像。
- 这和 `rollback_max_enqueued_uid()` 配合，使被 flush uid 后续重新 admission 时拿到一致的 LQ/SQ key。

## 7. 状态机完整性检查

### 7.1 Redirect phase

当前 phase 定义：

```systemverilog
MEMBLOCK_REDIRECT_PHASE_IDLE                = 0,
MEMBLOCK_REDIRECT_PHASE_DETECTED            = 1,
MEMBLOCK_REDIRECT_PHASE_FREEZE_REQUESTED    = 2,
MEMBLOCK_REDIRECT_PHASE_REDIRECT_DRIVEN     = 3,
MEMBLOCK_REDIRECT_PHASE_STATE_FLUSH_APPLIED = 4
```

主路径状态转换：

```text
IDLE
  -> DETECTED
  -> FREEZE_REQUESTED
  -> REDIRECT_DRIVEN
  -> STATE_FLUSH_APPLIED
  -> IDLE
```

Review 判断：

- 当前状态枚举能表达关键阶段。
- `STATE_FLUSH_APPLIED` 当前在 `apply_redirect_flush()` 中设置后很快回到 `IDLE`，主要用于 debug 观察，不是长期驻留态。

### 7.2 被 flush uid 的状态

被 flush uid 预期状态变化：

```text
旧动态实例:
  active/enq/issue/writeback/pass/fault/commit/deq 等清零
  issue_killed = 1
  redirect_pending = 1
  flushed = 1
  dynamic_epoch++
  active = 0
  success = 0

后续 re-admission 成功:
  set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1)
  redirect_pending = 0
  flushed = 0
  issue_killed = 0
```

对应源码在 `set_status_field(MEMBLOCK_STATUS_ENQ)`：

```systemverilog
if (value && !old_value) begin
    mark_uid_enqueued(uid);
    if (status.redirect_pending || status.flushed) begin
        status.redirect_pending = 1'b0;
        status.flushed          = 1'b0;
        status.issue_killed     = 1'b0;
    end
end
```

Review 判断：

- `redirect_pending/flushed` 不作为终态 success。
- 重新 admission 是清除 pending/flushed 的合法入口。

## 8. 正确性检查清单

| 检查项 | 当前实现 | Review 结论 |
|---|---|---|
| redirect event 是否来自 DUT feedback 统一事件队列 | `exception_event_q` -> `process_pending_events()` | 符合 |
| 多个 redirect 是否选最老 | `select_oldest_redirect()` 使用 ROB helper | 符合 |
| active redirect 期间是否阻止处理新事件 | `process_pending_events()` 开头检查 `active_redirect` | 符合 |
| redirect 是否真实驱动 DUT input | `memblock_redirect_dispatch_sequence::drive_redirect_payload()` | 符合 |
| 未开启 redirect sequence 时是否报错 | `get_redirect_seq_en()==0` 且有 redirect event 时 fatal | 符合 |
| drive 完成前是否禁止软件 flush | `redirect_drive_done_for()` | 符合 |
| drive 完成后是否跨 cycle 再 flush | `get_dispatch_service_cycle() > redirect_drive_done_cycle` | 符合 |
| flush 范围是否用 ROB 语义 | `rob_order_util::rob_need_flush()` | 符合 |
| 是否避免 100k 全表扫描 | 扫描 `[success_prefix_uid, max_enqueued_uid + 1)` | 符合 |
| 被 flush uid 是否清 active map | `retire_active_uid()` | 符合 |
| 被 flush uid 是否清 issue queue 残留 | `remove_uid_from_issue_queues()` | 符合 |
| 被 flush uid 是否不直接 success | `redirect_pending/flushed`，`success=0` | 符合 |
| LSQ admission 上界是否回退 | `rollback_max_enqueued_uid(oldest_flushed_uid)` | 符合 |
| LSQ 软件镜像是否回退 | `pending_lq/sq_cancel_count` -> `apply_pending_lsq_cancels()` | 符合 |

## 9. 风险和后续定向验证建议

### 9.1 需要定向覆盖 memoryViolation redirect

当前基础 smoke 能说明路径没有破坏普通 flow，但还需要定向 testcase 覆盖：

```text
uid0 success
uid1/uid2/uid3 已 admission
uid2 触发 memoryViolation redirect
redirect 覆盖 uid2/uid3
uid2/uid3 被 prepare_uid_for_redirect_reissue()
max_enqueued_uid 回退到 uid1
uid2/uid3 重新 admission/issue/writeback/pass
```

重点检查：

- uid2/uid3 的 `dynamic_epoch` 是否递增。
- 旧 active ROB/LQ/SQ map 是否删除。
- stale feedback 是否被丢弃。
- reissue 后 `redirect_pending/flushed/issue_killed` 是否清除。

### 9.2 需要覆盖同批非 redirect event requeue

构造同一 service cycle 中既有 redirect，又有不被该 redirect 覆盖的 older event：

```text
events = {older pass, younger redirect, younger replay}
```

预期：

- younger replay 被 redirect 覆盖则丢弃。
- older pass 不被覆盖则 requeue，redirect recovery 完成后继续处理。

### 9.3 需要覆盖 redirect sequence 缺失或超时

配置 `MEMBLOCK_REDIRECT_SEQ_EN=0` 或让 redirect payload 长时间不被消费。

预期：

- 有 redirect event 但 sequence disabled 时 fatal。
- active redirect 超过 `MEMBLOCK_REDIRECT_FREEZE_TIMEOUT` 时 fatal。

### 9.4 需要覆盖 LSQ cancel 计数

构造 load/store 已经 active mapped 后被 redirect flush。

预期：

- `pending_lq_cancel_count/pending_sq_cancel_count` 按 `numLsElem` 累加。
- 下一轮 `send_lsqenq_cycle()` 开头调用 `apply_pending_lsq_cancels()`。
- cancel 后重入队分配的 LQ/SQ key 和 DUT 预期一致。

## 10. 总结

当前 redirect flush 批处理主路径的实现是阶段化闭环：

- `exception_redirect_replay_handler` 负责事件选择和 recovery 推进。
- `common_data_transaction` 负责全局 freeze、redirect drive queue、批量 flush、uid reissue 状态准备。
- `memblock_redirect_dispatch_sequence` 负责真实驱动 DUT redirect input。
- `memblock_lsqenq_dispatch_sequence` 负责在恢复 admission 前消费 LSQ cancel 计数。

Review 结论：主路径设计和当前代码实现整体一致，没有发现主流程缺失。后续主要风险在定向 testcase 覆盖不足，尤其是 memoryViolation redirect、stale feedback drop、LSQ cancel/reissue 一致性这三类场景。
