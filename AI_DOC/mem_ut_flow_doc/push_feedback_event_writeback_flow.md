# push_feedback_event Writeback 后续总览

本文是 writeback 之后 normal pass / redirect / replay / fault 四类 flow 的总览和索引。具体场景不在本文混写长流程，而是分别维护在独立文档中：

- [normal_pass_flow.md](normal_pass_flow.md)：真实 writeback normal pass，直接落 status，不进入 `push_feedback_event()`。
- [redirect_flow.md](redirect_flow.md)：`memoryViolation` redirect 进入 `push_feedback_event()` 后的 flush/reissue。
- [replay_flow.md](replay_flow.md)：STA IQ feedback miss或LDA `replayInst`进入
  `push_feedback_event()`后的replay pending/route/issue及generation token生命周期。
- [fault_exception_flow.md](fault_exception_flow.md)：真实 writeback fault 先 `mark_target_fault()`，再入队由 `handle_fault_event()` 消费。

## 1. 总览 Flow 图

```mermaid
flowchart TD
    A[service_monitor_once] --> B[collect_monitor_event_batch]
    B --> C[dispatch_monitor_event_adapter]
    C --> C1[convert_raw_int_wb]
    C --> C2[convert_raw_iq_feedback]
    C --> C3[convert_raw_memory_violation]
    C1 --> C11[attach_issue_generation_snapshot LDA/STA]
    C2 --> C21[STA SQ-only + attach_issue_generation_snapshot]
    C11 --> D[dispatch_monitor_batch_handler::process_monitor_event_batch]
    C21 --> D
    C3 --> D
    D --> E[normalize_event_batch]
    E --> F[common_data_transaction::normalize_feedback_event]
    D --> G{redirect-first arbitration}
    G -->|covered| G0[drop without validate or commit; redirect apply closes token]
    G -->|allowed correlated| G1[validate_issue_generation_claim no side effect]
    G -->|allowed non-correlated| H
    G1 -->|normal pass allowed| H[handle_real_writeback_event]
    H -->|event_is_normal_pass| I[mark_target_normal_pass]
    I --> CMT[handler accepted or unique STA compat no-op then commit claim]
    CMT --> J[no push_feedback_event]
    H -->|fault| K[mark_target_fault]
    K --> L[push_feedback_event]
    G -->|selected redirect| L
    G1 -->|STA/LDA replay allowed| M[handle_issue_feedback_event or backend replay route]
    M -->|iq_feedback_failed STA| L
    L --> N[normalize_feedback_event again]
    N --> O[exception_event_q.push_back]
    O --> CMT2[if correlated and accepted commit claim; non-correlated continues]
    CMT2 --> P[exception_redirect_replay_handler::process_pending_events]
    P --> Q{recovery type}
    Q -->|redirect| R[request_redirect_flush / push_redirect_drive / apply_redirect_flush]
    Q -->|replay| S[handle_replay_event / mark_replay_pending]
    Q -->|fault| T[handle_fault_event consume only]
```

## 1.1 函数调用 Flow 图整体文字伪代码

```text
push_feedback_event Writeback 后续总览：

1. monitor event 统一转换：
   service_monitor_once 调用 collect_monitor_event_batch；
   LOAD/STA accepted fire已经建立不可变generation token；
   adapter把raw int writeback、STA SQ-only IQ feedback、raw memoryViolation转成event；
   STA IQ及LDA/STA WB raw的`sample_flush_epoch/cycle`由monitor valid采样拍同拍冻结，
   adapter只消费该provenance，禁止在出队时补current epoch；
   LDA/STA先通过active map/token附`uid/issue_epoch/replay_seq`，STD不建token；
   batch handler对同一service cycle的event做normalize和redirect-first仲裁；
   covered event不validate/commit；未覆盖correlated event先只读validate对应kind，再进入
   原handler，只有handler成功或唯一允许的STA compat no-op后才commit。

2. normal pass 分支：
   如果事件是真实 writeback normal pass 且未被 redirect 覆盖，调用 handle_real_writeback_event；
   mark_target_normal_pass 直接更新 status；
   normal pass 不进入 push_feedback_event，因为它不需要跨 cycle recovery。

3. recovery 分支：
   如果事件是selected redirect、STA replay、LDA backend replay或fault，才调用
   push_feedback_event；
   STA miss/LDA replay/fault先validate，原handler或replay入队成功后才commit token kind；
   push_feedback_event调用normalize_feedback_event再次校验，correlated event必须保留
   token snapshot并禁止status fallback；
   normalize 成功后写入 exception_event_q。

4. recovery queue 消费：
   process_pending_events 是 exception_event_q 的统一消费者；
   redirect 优先级最高，会建立 active_redirect 并驱动 io_redirect；
   replay 在无 redirect 抢占时 mark_replay_pending 并等待 route/issue 重发；
   fault在mark_target_fault后只由handle_fault_event消费，不重复落fault状态；
   redirect apply、fault、replay、terminal/deq按reason关闭token，re-fire建立新token。
```


## 2. 场景边界

源码位置：涉及以下文件：

- `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_batch_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/exception_redirect_replay_handler.sv`

真实逻辑摘要：

```systemverilog
// normal pass: no recovery queue
if (event_is_normal_pass(wb_event)) begin
    data.mark_target_normal_pass(...);
    return 1'b1;
end

// fault: mark first, then queue recovery event
if (event_has_fault(wb_event)) begin
    data.mark_target_fault(...);
    data.push_feedback_event(wb_event);
    return 1'b1;
end

// replay: STA IQ feedback failed or LDA replayInst backend replay
if (wb_event.iq_feedback_failed && wb_event.target != MEMBLOCK_ISSUE_TARGET_STD) begin
    data.push_feedback_event(wb_event);
    return 1'b1;
end

// redirect: selected by batch redirect-first arbitration
data.push_feedback_event(selected_redirect_event);
```

功能解释：

`push_feedback_event()` 不是 writeback 后所有事件的统一入口，而是 recovery 类事件入口。normal pass 直接更新状态；redirect/replay/fault 需要跨队列、跨 cycle 处理，所以进入 `exception_event_q`。

输入/输出：

- 输入：redirect、replay、fault语义的event；V2 LOAD/STA correlated event已附token
  snapshot并在redirect-first放行后通过只读validate。
- 输出：normalize 成功后写入 `common_data_transaction::exception_event_q`。
- 非输入：normal pass 不调用 `push_feedback_event()`。
- 副作用：本函数不commit或关闭token；只校验并入recovery queue。调用方只在确认
  handler/入队成功后commit claim。

文字伪代码：

```text
adapter 把 raw monitor fact 转换为 memblock_wb_event_t；
V2 LOAD/STA adapter先匹配fire token并附snapshot，但不消费pending；
batch handler 先 normalize，再做 active redirect 和同批 redirect-first 仲裁；
covered event drop且不validate/commit；allowed correlated event先validate claim资格；
如果事件是 normal pass：
  进入 normal_pass_flow.md；
  调用 mark_target_normal_pass；
  不进入 push_feedback_event；
如果事件是 selected redirect：
  进入 redirect_flow.md；
  调用 push_feedback_event 后由 process_pending_events 建立 active redirect；
如果事件是STA miss或LDA replayInst：
  进入 replay_flow.md；
  调用 push_feedback_event，成功入队后commit对应kind；随后由
  process_pending_events/handle_replay_event 设置 replay pending；
如果事件是 fault：
  进入 fault_exception_flow.md；
  先调用 mark_target_fault 落表，再调用 push_feedback_event；
  handler成功返回后commit claim；后续 handle_fault_event 只消费，不重复 mark。
```

内部子调用：

- `normalize_event_batch()`：batch handler 的第一道规范化，解析 uid 和 event 快照。
- `push_feedback_event()`：recovery queue 入队前的第二道规范化。
- `process_pending_events()`：`exception_event_q` 的唯一消费者。

## 3. `push_feedback_event()` 共同骨架

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`

真实逻辑摘要：

```systemverilog
function void push_feedback_event(input memblock_wb_event_t wb_event);
    memblock_wb_event_t normalized_event;

    if (!normalize_feedback_event(wb_event, normalized_event)) begin
        return;
    end
    exception_event_q.push_back(normalized_event);
endfunction
```

功能解释：

该函数只做两件事：先把 event 规范化，再把规范化后的 event 放入 `exception_event_q`。它不决定 redirect/replay/fault 具体怎么恢复，具体恢复由 `process_pending_events()` 根据事件类型处理。

输入/输出：

- 输入：`wb_event`。
- 输出：`exception_event_q.push_back(normalized_event)`。

文字伪代码：

```text
调用normalize_feedback_event：correlated LOAD/STA要求已有token snapshot并禁止status
fallback；非correlated event按原active map/fallback规则处理；
如果 normalize 失败，直接返回，不入队；
如果 normalize 成功，把 normalized_event push_back 到 exception_event_q；
等待 exception_redirect_replay_handler::process_pending_events 消费。
```

内部子调用：

- `normalize_feedback_event()`：入队前防御，保证队列中 event 能定位到当前 active uid。
- `exception_event_q.push_back()`：保存 recovery event。

## 4. `normalize_feedback_event()` 共同骨架

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`

真实逻辑摘要：

```systemverilog
if (!normalized_event.valid || !feedback_event_has_action(normalized_event)) return 1'b0;
if (normalized_event.redirect.valid && !normalized_event.has_rob) begin
    normalized_event.rob_key = normalized_event.redirect.rob_key;
    normalized_event.has_rob = 1'b1;
end
if (!resolve_uid_for_event(normalized_event, uid)) return 1'b0;
status = get_status(uid);
normalized_event.uid = uid;
normalized_event.has_uid = 1'b1;
...
if (!normalized_event.has_replay_seq) begin
    normalized_event.replay_seq = status.replay_seq;
    normalized_event.has_replay_seq = 1'b1;
end
```

功能解释：

这是`push_feedback_event()`后续能正确处理的前提。redirect允许`target=NONE`，但必须
通过ROB找到active uid；V2 correlated LOAD/STA replay/fault必须自带fire token
snapshot，不允许在第一次replay后从当前status补。明确非correlated兼容event才允许
使用原受限fallback。

文字伪代码：

```text
调用 feedback_event_has_action：确认 event 至少有 redirect/replay/fault/real writeback/IQ feedback 语义；
如果是 redirect 且缺 ROB key，从 redirect.rob_key 补齐；
调用 resolve_uid_for_event：通过 uid/ROB/LQ/SQ 反查 active uid，并检查多 key 一致；
补齐 uid/has_uid；
如果不是 redirect：
  调用 feedback_event_target_is_valid：要求 target 为 LOAD/STA/STD；
  如果generation_correlated=1，要求uid/issue_epoch/replay_seq完整并原样保留；
  correlated event缺snapshot时fatal，禁止status fallback；
  只有非correlated兼容event才沿用“replay后缺snapshot drop、首次可在
  target_dispatched后补issue_epoch/replay_seq”的原规则；
返回 normalized_event。
```

内部子调用：

- `feedback_event_has_action()`：判断 event 是否值得处理。
- `resolve_uid_for_event()`：通过 active ROB/LQ/SQ map 反查 uid。
- `feedback_event_target_is_valid()`：检查非 redirect target 合法性。
- `target_dispatched()`：只用于非correlated兼容event缺issue epoch时的原补齐前提。

## 5. `process_pending_events()` 共同消费骨架

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/exception_redirect_replay_handler.sv`

真实逻辑摘要：

```systemverilog
service_ptw_wait_replay();
advance_active_redirect();
if (data.active_redirect.valid) return;

while (data.pop_feedback_event(wb_event)) begin
    events.push_back(wb_event);
end

if (select_oldest_redirect(events, redirect_event)) begin
    data.request_redirect_flush(redirect);
    data.push_redirect_drive(redirect);
end

if (data.active_redirect.valid) begin
    requeue_events_not_flushed_by_redirect(events, data.active_redirect);
    return;
end

foreach (events[idx]) begin
    if (event_is_replay(events[idx])) handle_replay_event(events[idx]);
    else if (event_is_fault(events[idx])) handle_fault_event(events[idx]);
end
```

功能解释：

这是 `push_feedback_event()` 入队后的共同消费者。redirect 优先级最高；如果 recovery queue 中存在 redirect，replay/fault 可能被 drop 或 requeue。只有没有 active redirect 且当前 recovery events 中没有 redirect 时，replay/fault 才被消费。注意：同一 monitor batch 中未被 redirect 覆盖的 non-redirect event 是由 batch handler 直接继续处理，不走这里的 requeue 规则。

correlated event的token kind已在第一次monitor batch中按
`validate -> original handler/enqueue -> commit`完成消费，本函数不得再次validate或commit。
recovery queue中后出现redirect时，可drop/requeueevent；token已经按STA_MISS、
LOAD_REPLAY或FAULT reason关闭，closed tombstone保证迟到event不会绑定新generation。
redirect自身在apply阶段关闭真正被覆盖uid仍open的token。

文字伪代码：

```text
调用 service_ptw_wait_replay：释放 ready/timeout 的 PTW wait replay；
调用 advance_active_redirect：推进已有 active redirect，必要时 apply flush；
如果 active_redirect 仍有效，返回，不处理新队列；
调用 pop_feedback_event：把 exception_event_q 全部弹到本地 events；
调用 select_oldest_redirect：如果存在 redirect，先处理 ROB 最老 redirect；
如果建立 active_redirect：
  调用 requeue_events_not_flushed_by_redirect：覆盖事件 drop，未覆盖事件回队列；
  redirect drive完成后apply flush，在active map删除前关闭被覆盖uid的open token；
  返回；
如果没有 redirect：
  replay 调用 handle_replay_event；
  fault 调用 handle_fault_event；
normal pass 不在 exception_event_q，所以不会到这里。
```

内部子调用：

- `service_ptw_wait_replay()`：PTW-back replay 延迟释放。
- `advance_active_redirect()`：redirect drive done 后 apply flush。
- `pop_feedback_event()`：从 `exception_event_q` 出队。
- `select_oldest_redirect()`：redirect-first recovery 仲裁。
- `handle_replay_event()`：replay pending/等待 PTW。
- `handle_fault_event()`：fault event 消费，不重复 mark。

## 5.1 generation 生命周期边界

```text
issue fire：LOAD注册real-WB pending；STA注册IQ+real-WB pending；STD不建token；
reset：清raw queue后清open token/tombstone，再清active map和epoch；
redirect：covered event不validate/commit，apply只关闭真正被覆盖uid token；未覆盖token保持open；
STA hit：handler成功/唯一compat no-op后commit IQ并等待WB；STA miss：replay入队成功后
commit IQ、取消WB、close STA_MISS；
LDA normal/fault/replayInst：handler/入队成功后commit唯一WB并按ALL_CONSUMED/FAULT/
LOAD_REPLAY关闭；
reissue：mark_replay_pending只bump seq，新accepted fire再注册新token；
terminal/deq：active map删除前required pending泄漏fatal，optional才可按reason关闭；
stale：未来epoch/current无token/duplicate/key-pipe错配fatal；旧epoch、早于fire或
tombstone命中按reason info/drop；不得从可变status附新generation。
```

上述token动作不改变原batch redirect-first、pass/fault handler、replay queue或
terminal定义，只增加event correlation和生命周期完整性检查。

## 6. 端到端行为总结

```text
normal pass：
  LOAD/STA fire token -> real writeback exception_vec=0 -> attach generation
  -> process_monitor_event_batch
  -> redirect-first放行 -> validate REAL_WB claim资格
  -> handle_real_writeback_event
  -> mark_target_normal_pass
  -> handler成功或唯一STA compat no-op -> commit REAL_WB
  -> 不调用 push_feedback_event
  -> 详见 normal_pass_flow.md

redirect：
  memoryViolation
  -> process_monitor_event_batch selected oldest redirect
  -> push_feedback_event
  -> exception_event_q
  -> process_pending_events
  -> request_redirect_flush / push_redirect_drive / apply_redirect_flush / reissue
  -> 详见 redirect_flow.md

replay：
  STA fire token -> SQ-only IQ miss -> attach generation
  -> redirect-first放行 -> validate IQ claim资格
  -> handle_issue_feedback_event
  -> push_feedback_event
  -> exception_event_q
  -> handler成功返回 -> commit IQ/cancel WB/close STA_MISS
  -> process_pending_events
  -> handle_replay_event
  -> mark_replay_pending
  -> route/issue -> 新fire token
  -> 详见 replay_flow.md

LDA replay：
  LOAD fire token -> LDA replayInst -> attach generation
  -> redirect-first放行 -> validate WB claim资格
  -> push_feedback_event -> exception_event_q
  -> 入队成功 -> commit WB/close LOAD_REPLAY
  -> mark_replay_pending -> route/re-fire新token

fault：
  real writeback exception_vec!=0 -> attach generation
  -> redirect-first放行 -> validate REAL_WB claim资格
  -> handle_real_writeback_event
  -> mark_target_fault
  -> push_feedback_event
  -> exception_event_q
  -> handler成功返回 -> commit REAL_WB/close FAULT
  -> process_pending_events
  -> handle_fault_event 只消费，不重复 mark
  -> 详见 fault_exception_flow.md
```

端到端文字伪代码描述：

```text
normal pass：
  V2 LOAD/STA real-WB先匹配fire token；batch放行后只读validate REAL_WB再进入handler；
  handle_real_writeback_event 调用 mark_target_normal_pass 更新 target/uid pass 状态；
  handler成功或唯一STA compat no-op后才commit REAL_WB；
  该事件不进入 push_feedback_event，因为它不需要 recovery queue 后续处理。

redirect：
  如果 batch 中存在 memoryViolation redirect，batch handler 先选 oldest redirect；
  selected redirect 通过 push_feedback_event 进入 exception_event_q；
  process_pending_events 负责建立 active_redirect、驱动 redirect、flush 被覆盖 uid 并触发 reissue；
  被redirect覆盖的同批pass/fault/replay不允许落状态且不validate/commit token；apply阶段关闭
  被覆盖uid的open token。

replay：
  STA SQ-only miss先attach fire token；未被redirect覆盖时先validate IQ且不修改token；
  handler将其作为replay event成功入队后才commit IQ、取消WB并close STA_MISS；
  process_pending_events 在没有 redirect 抢占时调用 handle_replay_event；
  V2 STA没有flushState，直接mark_replay_pending；PTW wait仅保留给其它明确来源；
  mark_replay_pending 只设置需要重发的 target，后续由 route/issue 重新发射。

LDA replay：
  replayInst result通过LOAD token附generation，放行后先validate WB；
  push_feedback_event成功后commit WB并close LOAD_REPLAY，再复用同一recovery flow，
  新accepted fire创建新token。

fault：
  V2 LOAD/STA fault先attach并validate token real-WB资格，再由handler mark_target_fault；
  fault 状态落表后再 push_feedback_event，供 recovery queue 统一消费；
  handler成功返回后外层commit real-WB claim；
  handle_fault_event 只做消费和调试上下文解析，不重复写 fault 状态。
```
