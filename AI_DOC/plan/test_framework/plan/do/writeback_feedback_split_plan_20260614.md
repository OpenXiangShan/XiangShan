# Writeback / IQ Feedback 分流重构计划

## 0. 当前实现状态

当前已实现：

- `memblock_wb_event_t` 已新增 `real_wb_valid`、`iq_feedback_valid`、`iq_feedback_hit`、`iq_feedback_failed`、`iq_feedback_flush_state`。
- 旧 raw writeback 兼容字段已删除；真实 writeback 只使用 `real_wb_valid`，IQ feedback 只使用 `iq_feedback_*`。
- `dispatch_monitor_event_adapter::convert_raw_int_wb()` 只写真实 writeback 语义。
- `dispatch_monitor_event_adapter::convert_raw_iq_feedback()` 只写 IQ feedback 语义；STA miss 置 replay，STD miss warning/drop。
- `dispatch_monitor_event_adapter` 已改为 `collect_writeback_events_batch()` / `collect_ctrl_redirect_events_batch()`，只转换并收集 event，不直接驱动状态修改。
- `dispatch_monitor_batch_handler::process_monitor_event_batch()` 已成为真实 monitor flow 的统一入口，负责 normalize、active redirect stale 过滤和同批 redirect-first 仲裁。
- `writeback_status_handler` 已收窄为 batch 放行后的非 redirect 状态更新器；monitor redirect 不再从 writeback handler 入口进入。
- `status_transaction` / `common_data_transaction` 已增加 issue feedback success 状态和 `mark_issue_feedback_success()`。

实现约束：

- 同一 service batch 内先收集全部 raw monitor event，再按 oldest redirect 做覆盖仲裁；被 oldest redirect 覆盖的 pass/fault/replay 不能落状态。
- 无法 normalize 到 active uid/ROB key 的 monitor event 在 batch handler 中 `uvm_warning` 后 drop。
- 多个 redirect 同 batch 时，先处理 oldest redirect；未被 oldest 覆盖的其它 redirect 保留进入 recovery queue，等当前 redirect drive/flush 完成后再处理。
- recovery handler 保留 active redirect 单飞和 oldest pending redirect drive 保护，只作为跨 batch/drive 层保护，不参与 writeback 状态仲裁。

## 1. 背景和目标

旧实现中，`dispatch_monitor_event_adapter` 会把 raw int writeback、raw IQ feedback 和 memoryViolation 都转换成 `memblock_wb_event_t`，再统一调用 `writeback_status_handler::handle_event()`。这个结构便于复用 `uid/rob/lq/sq` 反查、`issue_epoch/replay_seq` 过滤和 redirect/replay 队列，但容易造成语义混淆：

- int writeback 是 DUT 的真实写回/完成接口，会进入 backend ROB/RF writeback 路径。
- IQ feedback 是 DUT 给 IssueQueue 的发射反馈，表达本次 issue 是 `finalSuccess` 还是 `failed/replay`，不是 ROB/RF writeback。
- memoryViolation 是 MemBlock 到 backend 的 redirect/recovery 请求，不是 writeback，也不是 IQ feedback。

本计划目标是保留统一 raw monitor drain 和统一 recovery queue，但把 monitor event 的入口拆成 adapter、batch handler、writeback handler 三层：

```text
raw int writeback
  -> convert_raw_int_wb()
  -> collect_writeback_events_batch()
  -> dispatch_monitor_batch_handler::process_monitor_event_batch()
  -> handle_real_writeback_event()

raw IQ feedback
  -> convert_raw_iq_feedback()
  -> collect_writeback_events_batch()
  -> dispatch_monitor_batch_handler::process_monitor_event_batch()
  -> handle_issue_feedback_event()

raw memoryViolation / redirect
  -> convert_raw_memory_violation()
  -> collect_ctrl_redirect_events_batch()
  -> dispatch_monitor_batch_handler::process_monitor_event_batch()
  -> exception_event_q / recovery handler
```

文字伪代码：

```text
如果 monitor 采到真实 int writeback：
  先把端口字段翻译成 LOAD/STA/STD target 的真实写回事件；
  先把端口字段翻译成 LOAD/STA/STD target 的真实写回事件；
  再进入 batch handler；
  如果没有被 active/same-batch redirect 覆盖，再按真实写回语义处理 pass 或 fault。

如果 monitor 采到 IQ feedback：
  先把 feedback 的 hit/failed/flush_state 翻译成 issue feedback 事件；
  先把 feedback 的 hit/failed/flush_state 翻译成 issue feedback 事件；
  再进入 batch handler；
  如果没有被 redirect 覆盖，再按 issue response 语义处理 issue 成功、replay 或兼容 pass。

如果 monitor 采到 memoryViolation：
  先把 memoryViolation 翻译成 redirect 事件；
  再由 batch handler 在本 service batch 中优先选择 oldest redirect；
  最后交给 redirect/recovery 流程做 flush 和恢复。
```

同时必须保留并强化 redirect 优先级：当同一批 monitor event 中存在 redirect 时，redirect 覆盖范围内的 replay、fault、normal pass 都不能先落状态；它们属于旧动态实例结果，应由 redirect flush 覆盖。

## 2. Scala/DUT 语义依据

### 2.1 int writeback 是真实写回/完成路径

`src/main/scala/xiangshan/mem/MemBlock.scala` 中 `mem_to_ooo` 暴露：

```scala
val intWriteback: MixedVec[MixedVec[MemWriteBack]] = intSchdParams.genMemWriteBackBundle
```

`src/main/scala/xiangshan/backend/Backend.scala` 中接入 backend：

```scala
intRegion.io.memWriteback.zip(io.mem.intWriteback).foreach { case (sinkWriteback, sourceWriteback) =>
  sinkWriteback.zip(sourceWriteback).foreach { case (sink, source) =>
    sink.valid := source.toRob.valid
    sink.bits := source.toNewExuOutputBundle()
  }
}
```

文字伪代码：

```text
遍历 MemBlock 输出的每个 int writeback port；
对每个 port，如果 source.toRob.valid 为 1，就告诉 backend 该写回有效；
把 MemBlock 的 writeback payload 转成 backend 新执行单元输出格式；
backend 后续用该 payload 更新 ROB/RF/writeback datapath。
```

因此 int writeback 的测试框架语义应是：真实 target writeback/pass/fault。它可以更新 `*_writeback`、`*_pass`、`*_fault` 和 uid 总体 `writeback/pass/fault`。

### 2.2 IQ feedback 是 IssueQueue 发射反馈

`src/main/scala/xiangshan/Bundle.scala` 定义：

```scala
class RSFeedback(isVector: Boolean = false)(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val hit = Bool()
  val flushState = Bool()
  val sourceType = RSFeedbackType()
  val dataInvalidSqIdx = new SqPtr
  val sqIdx = new SqPtr
  val lqIdx = new LqPtr
}

class MemRSFeedbackIO(isVector: Boolean = false)(implicit p: Parameters) extends XSBundle {
  val feedbackSlow = ValidIO(new RSFeedback(isVector)) // dcache miss queue full, dtlb miss
  val feedbackFast = ValidIO(new RSFeedback(isVector)) // bank conflict
}
```

文字伪代码：

```text
RSFeedback 只描述一次 issue 的反馈结果：
  robIdx/sqIdx/lqIdx 用来定位这次反馈属于哪条指令和 LSQ entry；
  hit=1 表示这次 issue 被 IssueQueue 认为成功；
  hit=0 表示这次 issue 失败，需要走 replay/fail 响应；
  flushState/sourceType 是失败原因或 PTW/TLB 相关元信息。

MemRSFeedbackIO 把反馈拆成 slow/fast 两类：
  feedbackSlow 用于 TLB miss、dcache miss queue full 等慢反馈；
  feedbackFast 用于 bank conflict 等快反馈。
```

`src/main/scala/xiangshan/backend/Region.scala` 中 STA feedback 进入 IssueQueue response：

```scala
val feedBack = io.staFeedback.get(i).feedbackSlow
imp.io.s2Resp.get.head.failed := feedBack.valid && !feedBack.bits.hit
imp.io.s2Resp.get.head.finalSuccess := feedBack.valid && feedBack.bits.hit
```

文字伪代码：

```text
从 MemBlock 接收 STA feedback；
如果 feedback 有效且 hit=0，就通知 IssueQueue 本次发射失败；
如果 feedback 有效且 hit=1，就通知 IssueQueue 本次发射最终成功；
这个判断只影响 IssueQueue 的 entry 成功/失败响应，不等价于 ROB/RF 写回。
```

`src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala` 也说明：

```scala
// Feedback to RS in s2, for store issue control
val feedBackSlow = ValidIO(new RSFeedback)
```

文字伪代码：

```text
StoreUnit 在 S2 阶段产生给 reservation station / issue queue 的反馈；
这个反馈用于控制 store 地址发射是否成功、是否需要重发；
它不是 store 的最终 writeback payload。
```

因此 IQ feedback 的测试框架语义应是：issue response。`hit=1` 表示本次 issue 对 IssueQueue finalSuccess，`hit=0` 表示 failed/replay；它不能天然等价于 ROB/RF writeback pass。

### 2.3 memoryViolation 是 redirect/recovery 请求

memoryViolation 来自 MemBlock 内部 rollback/nack/nuke 等 redirect 源的 oldest 选择，进入 backend redirect/recovery 流程。测试框架应把它作为 redirect event，而不是 writeback event。

## 3. 数据结构调整建议

当前 `memblock_wb_event_t` 可继续作为统一 semantic carrier，但字段语义需要拆清楚。最小改法是在保留现有字段基础上新增 IQ feedback 专用字段：

```systemverilog
typedef struct {
    bit                         valid;
    memblock_wb_event_source_e  source;
    int unsigned                port_id;
    memblock_issue_target_e     target;

    memblock_uid_t              uid;
    bit                         has_uid;
    memblock_rob_key_t          rob_key;
    bit                         has_rob;
    memblock_lq_key_t           lq_key;
    bit                         has_lq;
    memblock_sq_key_t           sq_key;
    bit                         has_sq;

    int unsigned                issue_epoch;
    bit                         has_issue_epoch;
    int unsigned                replay_seq;
    bit                         has_replay_seq;

    // 只表示 DUT 真实 writeback，不再给 IQ feedback hit 复用。
    bit                         real_wb_valid;
    bit                         has_exception;
    bit [23:0]                  exception_vec;

    // 只表示 DUT IssueQueue feedback。
    bit                         iq_feedback_valid;
    bit                         iq_feedback_hit;
    bit                         iq_feedback_failed;
    bit                         iq_feedback_flush_state;
    bit                         ptw_back_replay;

    bit                         replay_valid;
    bit                         redirect_valid;
    memblock_redirect_payload_t redirect;

    bit                         vector_ls;
    int unsigned                uop_index;
    longint unsigned            cycle;
} memblock_wb_event_t;
```

文字伪代码：

```text
一个统一 event 需要同时保存三类信息：
  身份字段：source、target、uid、ROB/LQ/SQ key，用于定位这条事件属于谁；
  时序字段：issue_epoch、replay_seq、cycle，用于过滤旧发射或旧 replay 的迟到反馈；
  行为字段：real_wb_valid、iq_feedback_*、replay_valid、redirect，用于决定后续执行 pass、fault、replay 还是 redirect。

真实 writeback 只能设置 real_wb_valid；
IQ feedback 只能设置 iq_feedback_*；
redirect 只能设置 redirect payload；
后续 handler 不能再把 IQ feedback hit 当成真实 writeback valid 使用。
```

兼容迁移时可以先保留旧 `real_wb_valid` 字段，但代码中应逐步改名为 `real_wb_valid`，并禁止 IQ feedback 写该字段。

## 4. Adapter 转换伪代码

### 4.1 raw int writeback 转换

```systemverilog
function bit convert_raw_int_wb(raw, output event);
    event = make_empty_monitor_event();
    if (!raw.valid) return 0;

    event.valid          = 1;
    event.real_wb_valid  = 1;
    event.has_exception  = raw.exception_vec != 0;
    event.exception_vec  = raw.exception_vec;
    event.has_rob        = raw_rob_to_key(raw.rob_valid, raw.rob_flag, raw.rob_value, event.rob_key);
    event.cycle          = raw.cycle;

    case (raw.port_id)
        0, 1, 2: begin
            event.source = MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB;
            event.target = MEMBLOCK_ISSUE_TARGET_LOAD;
            event.has_lq = raw_lq_to_key(raw.lq_valid, raw.lq_flag, raw.lq_value, event.lq_key);
        end
        3, 4: begin
            event.source = MEMBLOCK_WB_EVENT_SOURCE_STORE_WB;
            event.target = MEMBLOCK_ISSUE_TARGET_STA;
            event.has_sq = raw_sq_to_key(raw.sq_valid, raw.sq_flag, raw.sq_value, event.sq_key);
        end
        5, 6: begin
            event.source = MEMBLOCK_WB_EVENT_SOURCE_STORE_WB;
            event.target = MEMBLOCK_ISSUE_TARGET_STD;
            event.has_sq = raw_sq_to_key(raw.sq_valid, raw.sq_flag, raw.sq_value, event.sq_key);
        end
        default: return 0;
    endcase

    return 1;
endfunction
```

文字伪代码：

```text
创建一个空 event；
如果 raw int writeback 本身无效，直接返回“没有事件”；

把该 raw event 标记成真实 writeback：
  real_wb_valid=1；
  exception_vec 非零则说明这是 fault writeback；
  ROB key 从 raw writeback 的 ROB 字段提取；
  cycle 记录采样周期。

根据 writeback port_id 判断来源：
  port 0/1/2 是 load writeback，所以 target=LOAD，并记录 LQ key；
  port 3/4 是 store address writeback，所以 target=STA，并记录 SQ key；
  port 5/6 是 store data writeback，所以 target=STD，并记录 SQ key；
  其它端口不是本框架支持的 MemBlock writeback port，丢弃。

最后用 ROB/LQ/SQ key 反查 active uid；
如果能找到 active uid，说明该 writeback 属于当前仍有效的动态实例，返回有效；
如果找不到，说明可能是 flush 后迟到的旧 writeback，丢弃。
```

### 4.2 raw IQ feedback 转换

```systemverilog
function bit convert_raw_iq_feedback(raw, output event);
    event = make_empty_monitor_event();
    if (!raw.valid) return 0;
    if (raw.vector_feedback) return 0;
    if (!raw.is_sta && !raw.is_std) return 0;

    event.valid                   = 1;
    event.iq_feedback_valid       = 1;
    event.iq_feedback_hit         = raw.hit;
    event.iq_feedback_failed      = !raw.hit;
    event.iq_feedback_flush_state = raw.flush_state;
    event.ptw_back_replay         = raw.is_sta && !raw.hit && raw.flush_state;
    event.has_rob                 = raw_rob_to_key(raw.rob_valid, raw.rob_flag, raw.rob_value, event.rob_key);
    event.has_sq                  = raw_sq_to_key(raw.sq_valid, raw.sq_flag, raw.sq_value, event.sq_key);
    event.cycle                   = raw.cycle;

    if (raw.is_sta) begin
        event.source = MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK;
        event.target = MEMBLOCK_ISSUE_TARGET_STA;
        event.has_lq = 0;
    end else begin
        event.source = MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK;
        event.target = MEMBLOCK_ISSUE_TARGET_STD;
        event.has_lq = raw_lq_to_key(raw.lq_valid, raw.lq_flag, raw.lq_value, event.lq_key);
    end

    if (raw.is_std && !raw.hit) begin
        `uvm_warning("DISP_MON_ADAPT", "drop STD feedback failed: no backend STD replay path")
        return 0;
    end

    return event_has_active_uid(event);
endfunction
```

文字伪代码：

```text
创建一个空 event；
如果 raw IQ feedback 无效，直接丢弃；
如果是 vector feedback，当前 scalar 框架不处理，丢弃；
如果既不是 STA 也不是 STD feedback，说明 target 无法识别，丢弃。

把该 raw event 标记成 IssueQueue feedback：
  iq_feedback_valid=1；
  iq_feedback_hit=raw.hit；
  iq_feedback_failed=!raw.hit；
  iq_feedback_flush_state=raw.flush_state；
  如果是 STA 且 hit=0 且 flush_state=1，则额外标记 ptw_back_replay。

补齐身份字段：
  ROB key 来自 raw rob 字段；
  SQ key 来自 raw sq 字段；
  STA feedback 设置 source=STA_FEEDBACK、target=STA；
  STD feedback 设置 source=STD_FEEDBACK、target=STD，并额外记录 LQ key。

如果 STD feedback failed，当前不把它作为真实 backend replay 来源，直接 warning 后丢弃；
adapter 不反查 active uid，也不直接进入 handler；所有可转换的 feedback 先进入本轮 batch，
由 batch handler 在 normalize 阶段统一检查 uid/ROB 归属。
```

IQ feedback 转换后不能设置 `real_wb_valid`。是否把 `hit=1` 当作 pass，由 handler 根据真实 writeback 使能参数决定。

### 4.3 memoryViolation 转换

```systemverilog
function bit convert_raw_memory_violation(raw, output event);
    event = make_empty_monitor_event();
    if (!raw.memory_violation_valid) return 0;

    event.valid                  = 1;
    event.source                 = MEMBLOCK_WB_EVENT_SOURCE_MEMORY_VIOLATION;
    event.target                 = MEMBLOCK_ISSUE_TARGET_NONE;
    event.redirect_valid         = 1;
    event.redirect.valid         = 1;
    event.redirect.flush_itself  = raw.memory_violation_level;
    event.redirect.level         = raw.memory_violation_level;
    event.has_rob                = raw_rob_to_key(raw.memory_violation_rob_valid,
                                                  raw.memory_violation_rob_flag,
                                                  raw.memory_violation_rob_value,
                                                  event.rob_key);
    event.redirect.rob_key       = event.rob_key;
    event.cycle                  = raw.cycle;

    return 1;
endfunction
```

文字伪代码：

```text
创建一个空 event；
如果 raw ctrl 中没有 memoryViolation，有效 redirect 事件不存在，直接返回无事件；

把 memoryViolation 翻译成 redirect event：
  source=MEMORY_VIOLATION；
  target=NONE，因为 redirect 是全局恢复边界，不是 LOAD/STA/STD target；
  redirect.valid=1；
  redirect.flush_itself 和 level 来自 raw memoryViolation level；
  ROB key 来自 memoryViolation 指向的 ROB；
  redirect.rob_key 使用同一个 ROB key。

转换阶段只保留 raw fact，不再直接反查 active uid 或驱动状态修改；
uid/ROB 归属由 batch handler 在 normalize 阶段统一检查。
```

## 5. Batch 与 handler 分流伪代码

真实 monitor flow 的唯一入口是 `dispatch_monitor_batch_handler::process_monitor_event_batch()`：

```systemverilog
task process_monitor_event_batch(events);
    normalized_events = normalize_event_batch(events);
    if (active_redirect.valid) filter_covered_events(normalized_events);

    if (select_oldest_redirect(normalized_events, redirect_event)) begin
        data.push_feedback_event(redirect_event);
        foreach (normalized_events[idx]) begin
            if (covered_by_redirect(normalized_events[idx], redirect_event.redirect)) drop;
            else if (is_redirect(normalized_events[idx])) data.push_feedback_event(normalized_events[idx]);
            else process_allowed_non_redirect_event(normalized_events[idx]);
        end
    end else begin
        foreach (normalized_events[idx]) process_allowed_non_redirect_event(normalized_events[idx]);
    end
endtask
```

文字伪代码：

```text
先把本轮 monitor event batch 标准化：
  反查 uid；
  补 ROB key；
  补 issue_epoch；
  补 replay_seq；
  丢弃无效或无法归属 active uid 的 event。

如果当前已有 active redirect：
  检查该 event 的 ROB 是否被 active redirect 覆盖；
  若被覆盖，说明这是旧动态实例迟到结果，直接丢弃。

如果同批存在 redirect：
  选择 oldest redirect 入 recovery queue；
  被该 redirect 覆盖的非 redirect event 全部丢弃；
  未覆盖的非 redirect event 才交给 writeback handler；
  未覆盖的其它 redirect 保留进入 recovery queue，等待当前 redirect 完成后再处理。
```

真实 writeback 处理：

```systemverilog
function bit handle_real_writeback_event(event);
    uid         = event.uid;
    issue_epoch = event.issue_epoch;
    replay_seq  = event.replay_seq;

    if (event.has_exception || event.exception_vec != 0) begin
        // 注意：同批 redirect 覆盖检查不能只依赖 active_redirect，
        // 还需要 process_pending_events() 的 batch redirect 优先级保证。
        if (!data.mark_target_fault(uid, event.target, issue_epoch, replay_seq,
                                    event.exception_vec, event.cycle)) begin
            return 0;
        end
        data.push_feedback_event(event);
        return 1;
    end

    if (!event.real_wb_valid) return 0;

    return data.mark_target_normal_pass(uid, event.target, issue_epoch, replay_seq, event.cycle);
endfunction
```

文字伪代码：

```text
从 event 中取出 uid、issue_epoch、replay_seq；
这些字段用于保证该 writeback 对应当前发射版本，而不是旧版本迟到结果。

如果 event 带 exception：
  先尝试把对应 target 标记为 writeback+fault；
  如果 epoch/replay_seq/active 状态不匹配，说明事件过期，丢弃；
  如果成功落 fault 状态，再把 fault event 放入 recovery queue；
  recovery handler 后续只消费该 fault 事件，不重复写 fault 状态。

如果 event 不带 exception：
  必须要求 real_wb_valid=1；
  然后标记对应 target 的 writeback/pass；
  如果该 uid 所需所有 target 都完成，再更新 uid 总体 pass/writeback。
```

IQ feedback 处理：

```systemverilog
function bit handle_issue_feedback_event(event);
    uid         = event.uid;
    issue_epoch = event.issue_epoch;
    replay_seq  = event.replay_seq;

    if (!event.iq_feedback_valid) return 0;

    if (event.iq_feedback_failed) begin
        if (event.target == MEMBLOCK_ISSUE_TARGET_STD) begin
            return 0;
        end
        event.replay_valid = 1;
        data.push_feedback_event(event);
        return 1;
    end

    if (event.iq_feedback_hit) begin
        if (event.target == MEMBLOCK_ISSUE_TARGET_STA &&
            seq_csr_common::get_sta_real_wb_pass_en()) begin
            return data.mark_issue_feedback_success(uid, event.target, issue_epoch, replay_seq, event.cycle);
        end

        if (event.target == MEMBLOCK_ISSUE_TARGET_STD &&
            seq_csr_common::get_std_real_wb_pass_en()) begin
            return data.mark_issue_feedback_success(uid, event.target, issue_epoch, replay_seq, event.cycle);
        end

        // 兼容/bring-up 模式：没有真实 writeback pass 时，feedback hit 可闭环为 target pass。
        return data.mark_target_normal_pass(uid, event.target, issue_epoch, replay_seq, event.cycle);
    end

    return 0;
endfunction
```

文字伪代码：

```text
从 event 中取出 uid、issue_epoch、replay_seq；
这些字段用于确认该 feedback 对应当前 target 的发射版本。

如果 event 不是 IQ feedback，直接丢弃；

如果 feedback 表示 failed：
  STD failed 当前不作为真实后端 replay 来源，直接丢弃；
  STA failed 转成 replay event；
  replay event 进入 recovery queue，由 replay handler 决定是否等待 PTW、是否重新 route。

如果 feedback 表示 hit：
  若 target 是 STA 且真实 STA writeback pass 开启：
    只记录 STA issue feedback success，不置 STA pass；
    最终 pass 必须等待真实 STA writeback。
  若 target 是 STD 且真实 STD writeback pass 开启：
    只记录 STD issue feedback success，不置 STD pass；
    最终 pass 必须等待真实 STD writeback。
  若对应真实 writeback pass 关闭：
    这是 bring-up/兼容模式；
    允许把 IQ feedback hit 当作 target pass 来闭环。

如果既不是 failed 也不是 hit，丢弃。
```

redirect event 处理由 batch handler 承担：

```text
如果 event 没有 canonical redirect payload，丢弃；
如果同批 redirect 被选为 oldest，把它放入 recovery queue；
如果其它 redirect 未被 oldest 覆盖，也放入 recovery queue 延后处理；
后续由 exception_redirect_replay_handler 选择 oldest redirect、冻结发射、驱动 redirect 接口并 apply flush。
```

## 6. Batch 级 redirect-first 仲裁方案

### 6.1 设计目标

batch 级仲裁不是在旧逻辑外面再加一层防御，而是把 redirect 优先级提升到 monitor service 的唯一入口。目标是让 flow 更清晰：

```text
adapter：只负责 raw fact -> event
batch handler：统一 normalize、redirect-first 仲裁、active redirect 过滤
writeback handler：只处理已经允许生效的 non-redirect event
recovery handler：只处理 redirect/replay/fault 后续恢复
```

这样可以避免同一 service cycle 中 pass/fault/replay 比 redirect 先落状态，导致旧动态实例污染 `status_transaction`。

### 6.2 当前逻辑和问题

旧逻辑是边 drain 边处理：

```text
drain_writeback_events()
  pop raw int writeback
    -> convert_raw_int_wb()
    -> writeback_status_handler::handle_event()

  pop raw IQ feedback
    -> convert_raw_iq_feedback()
    -> writeback_status_handler::handle_event()

drain_exception_and_redirect_events()
  pop raw ctrl
    -> apply_raw_ctrl_deq()
    -> convert_raw_memory_violation()
    -> writeback_status_handler::handle_event()
```

文字伪代码：

```text
看到一个 raw event 后立刻转换；
转换成功后立刻调用 handle_event；
handle_event 可能马上 mark pass、mark fault、push replay 或 push redirect；
redirect 如果在同一轮稍后才出现，就可能晚于前面的 pass/fault/replay。
```

问题示例：

```text
同一 service cycle 采到：
  uid=10 normal pass
  uid=8  memoryViolation redirect，flush uid=8 之后 younger 指令

旧逻辑可能先处理 uid=10 pass：
  mark_target_normal_pass(uid=10)

随后才处理 uid=8 redirect：
  request redirect flush

严格语义下 uid=10 pass 应该被 uid=8 redirect 覆盖，不能先落状态。
```

### 6.3 新 flow 总体伪代码

新 flow 改为先收集 batch，再统一仲裁：

```text
service_monitor_once()
  events = {}

  collect_runtime_context_events()
    -> drain_csr_events()
    -> drain_sfence_events()

  monitor_adapter.collect_writeback_events_batch(events)
    -> raw int writeback 转 event，只 push 到 events
    -> raw IQ feedback 转 event，只 push 到 events

  monitor_adapter.collect_ctrl_redirect_events_batch(events)
    -> raw ctrl deq/sbIsEmpty 仍按原逻辑即时处理
    -> memoryViolation 转 redirect event，只 push 到 events

  batch_event_handler.process_monitor_event_batch(events)
    -> normalize 所有 event
    -> 如果 active_redirect 已存在，先过滤被 active_redirect 覆盖的 event
    -> 如果本批存在 redirect，先选 oldest redirect
    -> 被 redirect 覆盖的同批 event 全部丢弃
    -> 未被覆盖的 event 才允许进入 writeback/replay/fault 状态更新

  exception_redirect_replay_handler.process_pending_events()
    -> 执行 redirect drive/flush/replay/fault recovery 后续流程
```

核心原则：

```text
同一 service cycle 内，任何 monitor raw event 都不能直接落 pass/fault/replay 状态；
必须先进入 batch，经过 normalize 和 redirect-first 仲裁后，才允许更新 status 或进入 recovery queue。
```

### 6.4 修改点 1：adapter 只收集，不处理

之前逻辑：

```text
convert_raw_int_wb(raw, event)
writeback_handler.handle_event(event)

convert_raw_iq_feedback(raw, event)
writeback_handler.handle_event(event)

convert_raw_memory_violation(raw, event)
writeback_handler.handle_event(event)
```

更改逻辑：

```text
convert_raw_int_wb(raw, event)
events.push_back(event)

convert_raw_iq_feedback(raw, event)
events.push_back(event)

convert_raw_memory_violation(raw, event)
events.push_back(event)
```

建议接口：

```systemverilog
task collect_writeback_events_batch(ref memblock_wb_event_t events[$]);
task collect_ctrl_redirect_events_batch(ref memblock_wb_event_t events[$]);
```

文字伪代码：

```text
collect_writeback_events_batch:
  循环 pop raw int writeback；
    convert 成功则 push 到 events；
  循环 pop raw IQ feedback；
    convert 成功则 push 到 events；
  不调用 handle_event；
  不调用 mark_target_normal_pass；
  不调用 mark_target_fault；
  不调用 push_feedback_event。

collect_ctrl_redirect_events_batch:
  循环 pop raw ctrl；
    lqDeq/sqDeq/sbIsEmpty 仍然即时交给 commit/ctrl 状态处理；
    memoryViolation valid 时 convert 成 redirect event；
    convert 成功则 push 到 events；
  不在 adapter 内请求 redirect flush。
```

### 6.5 修改点 2：normalize 移到 batch handler

之前逻辑：

```text
writeback_status_handler::handle_event()
  -> data.normalize_feedback_event()
  -> 再分类处理
```

更改逻辑：

```text
batch_event_handler::process_monitor_event_batch()
  -> normalize 所有 event
  -> 只有 normalized event 才进入 redirect 仲裁和状态更新

writeback_status_handler
  -> 只接受已经 normalized 且已经通过 batch 仲裁的 event
```

文字伪代码：

```text
batch handler 遍历 events：
  如果 event 无效，丢弃；
  如果 event 无法解析 active uid，丢弃；
  如果 event 缺 ROB key，但能通过 uid/status 补齐，则补齐；
  如果 event 缺 issue_epoch/replay_seq，则按当前状态补齐或按原规则丢弃；
  normalize 成功后放入 normalized_events。

这样 batch 仲裁可以准确知道：
  event 属于哪个 uid；
  event 的 ROB key 是什么；
  event 是否属于当前 issue_epoch/replay_seq；
  event 是否被 redirect 覆盖。
```

### 6.6 修改点 3：batch handler 统一 redirect-first 仲裁

建议新增：

```systemverilog
task process_monitor_event_batch(input memblock_wb_event_t events[$]);
```

文字伪代码：

```text
process_monitor_event_batch:
  normalized_events = normalize(events)

  如果 active_redirect 当前有效：
    遍历 normalized_events：
      如果 event 被 active_redirect 覆盖：
        丢弃
      否则：
        process_allowed_non_redirect_event(event)
    return

  如果 normalized_events 中存在 redirect：
    redirect = select_oldest_redirect(normalized_events)

    request_redirect_flush(redirect)
    push_redirect_drive(redirect)

    遍历 normalized_events：
      如果 event 是 redirect：
        如果是 selected redirect，跳过；
        如果被 selected redirect 覆盖，丢弃；
        如果未被 selected redirect 覆盖，可延后到后续 batch 或重新入 recovery queue；

      如果 event 不是 redirect：
        如果被 selected redirect 覆盖：
          丢弃，不能 mark pass/fault/replay；
        如果未被 selected redirect 覆盖：
          process_allowed_non_redirect_event(event)

    return

  如果本批没有 redirect：
    遍历 normalized_events：
      process_allowed_non_redirect_event(event)
```

这里建议第一版采用“未覆盖 event 同轮直接处理”的简单策略：

```text
同批 redirect 未覆盖的 event：
  直接调用 process_allowed_non_redirect_event(event)
```

原因：

```text
未覆盖 event 与 selected redirect 没有 ROB flush 冲突；
直接处理更简单，不需要额外 pending_batch_q；
如果后续发现 redirect drive 阶段需要更强冻结，再单独改为延后处理。
```

### 6.7 修改点 4：writeback handler 职责收窄

之前逻辑：

```text
writeback_status_handler::handle_event()
  normalize
  active_redirect stale filter
  redirect/replay/fault/pass/IQ feedback 分类
```

更改逻辑：

```text
writeback_status_handler
  apply_real_writeback_event(event)
  apply_issue_feedback_event(event)
```

文字伪代码：

```text
apply_real_writeback_event:
  输入必须是 normalized 且 batch 已允许生效的 event；
  如果 exception_vec 非零：
    mark_target_fault；
    push_feedback_event 进入 recovery queue；
  否则：
    mark_target_normal_pass。

apply_issue_feedback_event:
  输入必须是 normalized 且 batch 已允许生效的 IQ feedback event；
  如果 iq_feedback_failed：
    STA failed -> push_feedback_event 进入 replay recovery；
    STD failed -> warning/drop，不建 backend replay；
  如果 iq_feedback_hit：
    real writeback pass enabled -> mark_issue_feedback_success；
    real writeback pass disabled -> mark_target_normal_pass 作为兼容闭环。
```

`writeback_status_handler` 不再承担：

```text
不再 normalize event；
不再处理 redirect event；
不再做 active_redirect stale filter；
不再决定同批 redirect 优先级。
```

这些逻辑统一放到 batch handler。

### 6.8 修改点 5：去除冗余防御逻辑

为了避免新旧逻辑叠加导致 flow 不清晰，batch 重构时需要同步清理以下冗余：

| 旧逻辑 | 是否保留 | 原因 |
|---|---|---|
| `writeback_status_handler::event_blocked_by_active_redirect()` | 移除或不再调用 | active redirect 覆盖过滤改由 batch handler 统一做。 |
| `writeback_status_handler::handle_redirect_event()` | 移除或不再作为 monitor redirect 入口 | redirect 是 batch 仲裁最高优先级，应在 batch handler 中处理。 |
| `writeback_status_handler::handle_event()` 内 normalize | 移除 | batch 仲裁前必须先 normalize，否则无法准确判断 ROB 覆盖。 |
| adapter 中直接调用 `handle_event()` | 移除 | adapter 只做 raw 转 event，不做状态更新。 |
| 长期保留旧 raw writeback 兼容字段 | 已删除 | synthetic pass 已改为 `real_wb_valid`，避免长期双 writeback-valid 语义。 |

保留但职责变窄的逻辑：

```text
exception_redirect_replay_handler::process_pending_events()
  仍保留 recovery queue 中 redirect/replay/fault 的后续处理；
  但同一 raw monitor batch 的 redirect-first 判断不再依赖它完成。
```

### 6.9 event 覆盖判断文字伪代码

```text
event_covered_by_redirect(event, redirect):
  如果 event 是无效 event：
    return false

  如果 event 没有 rob_key：
    如果 event 有 uid，尝试从 status 中补 rob_key；
    如果仍然没有 rob_key：
      对 pass/fault/replay event，warning 后丢弃更保守；
      对纯 debug event，可以忽略；

  调用 rob_order_util::rob_need_flush(event.rob_key, redirect)
    如果返回 1：
      event 被 redirect 覆盖，不能更新状态；
    如果返回 0：
      event 不在 flush 范围内，可以继续处理。
```

实现要求：

```text
所有 ROB 顺序判断必须走 rob_order_util；
不要在 batch handler 中手写 ROB wrap/flag/value 比较；
normalize 阶段应尽量补齐 non-redirect event 的 rob_key。
```

### 6.10 新 flow 的最终结构

最终代码结构建议：

```text
dispatch_monitor_event_adapter
  collect_writeback_events_batch(events)
  collect_ctrl_redirect_events_batch(events)

dispatch_monitor_batch_handler
  process_monitor_event_batch(events)
  normalize_event_batch(events, normalized_events)
  select_oldest_redirect(normalized_events)
  event_covered_by_redirect(event, redirect)
  process_allowed_non_redirect_event(event)

writeback_status_handler
  apply_real_writeback_event(event)
  apply_issue_feedback_event(event)

exception_redirect_replay_handler
  process_pending_events()
  handle_replay_event()
  handle_fault_event()
  advance_active_redirect()
```

文字伪代码：

```text
adapter 只回答“DUT 端口发生了什么”；
batch handler 回答“这一批 event 哪些允许生效”；
writeback handler 回答“允许生效的 writeback/IQ feedback 如何更新状态表”；
recovery handler 回答“redirect/replay/fault 后续如何恢复流水线”。
```

## 7. 状态字段补充建议

为避免 IQ feedback hit 被误解释为真实 writeback pass，建议新增 issue feedback 成功状态：

```systemverilog
bit load_issue_feedback_success;
bit sta_issue_feedback_success;
bit std_issue_feedback_success;
```

文字伪代码：

```text
这两个字段只记录 IssueQueue feedback hit 已经返回；
它们不代表真实 writeback/pass；
当真实 STA/STD writeback pass 开启时，最终 pass 仍必须等待 int writeback；
当兼容模式关闭真实 writeback pass 要求时，才允许 feedback hit 直接转 pass。
```

helper：

```systemverilog
function bit mark_issue_feedback_success(uid, target, issue_epoch, replay_seq, cycle);
    status = get_status(uid);
    if (!status.active || status.issue_killed) return 0;
    if (!target_dispatched(status, target)) return 0;
    if (status.get_target_issue_epoch(target) != issue_epoch) return 0;
    if (!target_replay_seq_match(status, target, replay_seq)) return 0;

    case (target)
        MEMBLOCK_ISSUE_TARGET_LOAD: status.load_issue_feedback_success = 1;
        MEMBLOCK_ISSUE_TARGET_STA:  status.sta_issue_feedback_success  = 1;
        MEMBLOCK_ISSUE_TARGET_STD:  status.std_issue_feedback_success  = 1;
        default: return 0;
    endcase

    status.last_event_cycle = cycle;
    return 1;
endfunction
```

文字伪代码：

```text
根据 uid 找到状态表；
如果 uid 不 active，或者该动态实例已经被 kill，丢弃；
如果该 target 还没有发射，说明 feedback 不合法，丢弃；
如果 issue_epoch 不匹配，说明 feedback 属于旧发射，丢弃；
如果 replay_seq 不匹配，说明 feedback 属于旧 replay 轮次，丢弃；

通过所有检查后：
  target=STA 时设置 sta_issue_feedback_success；
  target=STD 时设置 std_issue_feedback_success；
  记录 last_event_cycle；
  返回成功。

该函数只标记 IssueQueue 层面的 finalSuccess，不设置 target pass/writeback。
```

当 `MEMBLOCK_STA_REAL_WB_PASS_EN=1` 或 `MEMBLOCK_STD_REAL_WB_PASS_EN=1` 时：

- IQ feedback hit 只标记 `*_issue_feedback_success`。
- 最终 `*_pass/writeback` 等待真实 int writeback。

当对应 real writeback pass 关闭时：

- IQ feedback hit 可以作为 bring-up/兼容闭环，调用 `mark_target_normal_pass()`。

## 8. 分阶段实施建议

### 阶段 1：handler 分流（已实现）

- `handle_event()` 内按 `source` 调用 `handle_real_writeback_event()`、`handle_issue_feedback_event()`。
- `real_wb_valid` 只表示真实 writeback，不再给 IQ feedback 复用。
- 保持现有 testcase 通过。

### 阶段 2：字段语义拆分（已实现）

- 新增 `real_wb_valid` 和 `iq_feedback_*` 字段。
- `convert_raw_int_wb()` 只设置 `real_wb_valid`。
- `convert_raw_iq_feedback()` 只设置 `iq_feedback_*`，不再设置 `real_wb_valid`。
- 修改 `feedback_event_has_action()` 和 `event_is_normal_pass()`。

### 阶段 3：redirect batch 优先级与冗余逻辑清理（已实现）

- adapter 新增 `collect_*_events_batch()` 形式的纯转换接口，收集本 service cycle 的 event list，不立即落状态。
- 新增 `dispatch_monitor_batch_handler::process_monitor_event_batch()`，在 batch handler 内统一 normalize、active redirect 覆盖过滤和同批 redirect-first 仲裁。
- 同批存在 redirect 时先选 oldest redirect，丢弃被覆盖的 pass/fault/replay，未覆盖非 redirect event 同轮处理。
- 同批其它 redirect 如果未被 oldest 覆盖，则保留进入 recovery queue，等当前 redirect drive/flush 完成后再按 oldest 规则处理。
- 将 `writeback_status_handler` 收窄为状态更新器：handler 内不再 normalize、不再做 active redirect stale filter、不再作为 monitor redirect 入口。
- 旧 raw writeback 兼容字段已删除，所有真实 writeback 和 synthetic pass 均改用 `real_wb_valid`。

### 阶段 4：issue feedback success 状态（已实现）

- 增加 `load_issue_feedback_success/sta_issue_feedback_success/std_issue_feedback_success`。
- `MEMBLOCK_*_REAL_WB_PASS_EN=1` 时，IQ feedback hit 只标记 issue feedback success。
- `MEMBLOCK_*_REAL_WB_PASS_EN=0` 时，IQ feedback hit 才作为 pass 兼容路径。

## 9. 验收建议

需要至少覆盖以下 directed 场景：

1. STA IQ feedback hit + STA real writeback pass enabled：只标记 issue feedback success，最终 pass 来自 int writeback。
2. STA IQ feedback hit + STA real writeback pass disabled：feedback hit 可直接闭环为 STA pass。
3. STA IQ feedback miss：进入 replay，重新 route STA，不影响已完成 STD。
4. STD IQ feedback hit + STD real writeback pass enabled：只标记 issue feedback success，最终 pass 来自 int writeback。
5. 同批 normal pass 和 redirect，normal pass 被 redirect 覆盖：不得置 pass。
6. 同批 fault 和 redirect，fault 被 redirect 覆盖：不得置 fault/exception_pending。
7. 同批 replay 和 redirect，replay 被 redirect 覆盖：不得置 replay_pending。
8. 同批多个 redirect：只处理 oldest redirect，被 oldest 覆盖的 younger redirect 丢弃，未覆盖 redirect 延后。
9. active redirect drive 未完成期间，被覆盖的 writeback/feedback 继续被 stale filter 丢弃。
