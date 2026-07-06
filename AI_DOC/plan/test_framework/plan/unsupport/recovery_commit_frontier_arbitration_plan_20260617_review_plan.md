# [不支持] Recovery Commit Frontier 仲裁可实施 Review Plan

## 0. 不支持结论

本方案归档为“不支持”，不建议按本文实现。

不支持原因：

- 本方案把 redirect/replay 统一设计成 recovery candidate，并要求等 commit frontier 扫到对应 uid 后才执行。
- 结合 XiangShan Scala 源码，memblock 的 `memoryViolation/loadReplay` redirect 属于后端微架构恢复事件，不需要等 ROB commit；DUT 会在检测到 memory violation 后通过 redirect/loadReplay 路径立即触发恢复。
- 如果把 memblock memoryViolation redirect 也放进 commit-frontier 延迟队列，会把本应立即发生的 flush/reissue 推迟，和 DUT 行为不一致。
- ROB 精确提交类 flush，例如 exception、flushPipe、replayInst，才适合 commit/head/frontier 边界；这类事件不能和 memoryViolation redirect 混成同一套全局延迟机制。

本文仅保留为历史方案和反例说明。后续实现应保持 memoryViolation redirect 的即时处理路径；如果需要支持 ROB 精确提交类 flush，应单独设计，不复用本文的全局 recovery candidate 仲裁方案。

特别修正：`transaction_done_with_recovery_idle` 不应作为替代 `transaction_done()` 的新完成条件。`transaction_done()` 仍只表达“所有 uid 的 success 前缀完成”；recovery 相关队列和 active redirect 是否为空，应作为 `end_test_check()` 的结束一致性检查项，用来发现 testcase 结束时仍有 recovery 残留。

## 1. 最终 Flow 和代码逻辑

### 1.1 Monitor Batch 采集与分类 Flow

最终方案中，monitor 仍然每个 service cycle 早采集 DUT 输出，但 redirect/replay 不再立即进入旧 `exception_event_q` 执行恢复。所有 redirect/replay 先变成 recovery candidate，等待 commit frontier 扫到对应 uid 后再真正执行。

核心原则：

- 同一 batch 内仍先做 redirect-first 仲裁。
- batch 内 ROB oldest redirect 覆盖的 pass/fault/replay 直接 drop。
- 未被覆盖的 redirect/replay 进入 `recovery_candidate_q`。
- fault 第一版仍可按原 fault 路径处理，但不能覆盖已有同 uid candidate。
- normal pass 只在写回前置状态检查通过后更新状态。
- pending candidate uid 不允许 normal pass/fault 再推进状态。
- active redirect 期间的新 event 也不能回到旧 `exception_event_q` redirect/replay 即时恢复路径。

文字伪代码：

```text
process_monitor_event_batch 的功能：
  处理同一个 service cycle 内 monitor adapter 收集到的 normalized event batch。
  它负责先做 redirect 覆盖仲裁，再决定 event 是落普通 writeback/fault 状态，还是进入 recovery_candidate_q 等待 commit frontier。

流程：
  第一步调用 normalize_event_batch：
    normalize_event_batch 负责把 raw monitor event 解析成带 uid、ROB key、target、issue_epoch、replay_seq 的标准 event。
    如果 normalize 后没有任何有效 event，本轮 batch 没有后续状态副作用，直接结束。

  如果当前已经存在 active_redirect：
    说明某个 redirect candidate 已经被 commit frontier 放行，正在等待 redirect drive/apply flush。
    对 batch 内每个 event 做覆盖判断：
      如果 event 的 ROB 被 active_redirect 覆盖，说明它属于旧动态实例，直接 drop，并打印 uid、rob、source 和 drop 原因。
      如果 event 没有被覆盖且是 redirect/replay，调用 push_recovery_candidate 把它保存到 recovery_candidate_q；该调用只入队，不启动 redirect，也不 mark replay。
      如果 event 没有被覆盖且是 normal pass/fault，调用 process_allowed_non_redirect_event 继续走普通状态更新检查。
    active_redirect 分支处理完后直接结束，避免旧即时 recovery 路径被重新启用。

  如果当前没有 active_redirect：
    调用 select_oldest_redirect 在本 batch 中选择 ROB oldest redirect。
    如果找到了 oldest redirect：
      先调用 push_recovery_candidate 保存这个 redirect candidate，等待 commit frontier 执行。
      再遍历本 batch 其余 event：
        selected redirect 本身已经入队，跳过。
        被 selected redirect 覆盖的 normal pass/fault/replay/redirect 直接 drop，并打印原因。
        未覆盖的 redirect/replay 调用 push_recovery_candidate 入队。
        未覆盖的 normal pass/fault 调用 process_allowed_non_redirect_event 继续处理。
      本 batch 处理完成后结束。

  如果本 batch 没有 redirect：
    遍历所有 normalized event。
    replay event 调用 push_recovery_candidate 入队，等待 commit frontier；不能进入 exception_event_q 即时 replay。
    normal pass/fault 调用 process_allowed_non_redirect_event，只有通过前置状态检查后才允许落状态。

process_allowed_non_redirect_event 的功能：
  处理未被 redirect 覆盖的 normal pass/fault event。
  它必须先检查同 uid 是否已有 pending recovery candidate，避免 candidate uid 被迟到 pass/fault 推进状态。

process_allowed_non_redirect_event 流程：
  先调用 has_recovery_candidate_for_uid(event.uid)：
    这个 helper 只查询当前 uid 是否已经在 recovery_candidate_q 中有 pending redirect/replay。
    如果查询结果表示“存在 pending candidate”，说明该 uid 正在等待 commit frontier recovery，当前 normal pass/fault 只能 drop 并打印原因，不能修改 writeback/pass/fault 状态。
  如果 event 是 normal pass：
    调用 writeback_status_handler.handle_real_writeback_event。
    handle_real_writeback_event 最终必须经过 check_target_writeback_pre_state，检查通过后才更新 target/uid pass 状态。
  如果 event 是 fault：
    先调用 writeback_status_handler.handle_real_writeback_event 落 fault 状态。
    第一版 fault 仍可进入 push_feedback_event，用于 fault-only 消费；但 redirect/replay 不允许再走 push_feedback_event 即时恢复路径。
```

实现 review 时要确认：`dispatch_monitor_batch_handler` 和 `writeback_status_handler::handle_issue_feedback_event()` 中 redirect/replay 不再通过 `push_feedback_event()` 进入旧即时恢复路径。

### 1.2 Normal Pass / Fault 写回状态更新 Flow

normal pass/fault 写状态前必须检查“写回前一状态”。能查到 uid 不代表这个 event 仍属于当前动态实例；redirect/reissue 后的迟到 writeback 可能通过新 active map 命中新实例，因此状态更新必须由 target 状态机前置条件保护。

写回前置状态定义：

- `status.active == 1`
- `status.issue_killed == 0`
- `status.redirect_pending == 0`
- `status.flushed == 0`
- 对 normal pass，`status.fault == 0` 且 `status.exception_pending == 0`
- target 已经 dispatched
- target 尚未完成 writeback/pass/fault
- target issue_epoch 等于 event issue_epoch
- target replay_seq 与 event replay_seq 匹配

文字伪代码：

```text
check_target_writeback_pre_state 的功能：
  在 normal pass 或 fault 写状态前，确认当前 status 正好停在“target 已经 fire/dispatched，正在等待本次 writeback”的状态。
  该函数只做检查和 warning，不允许修改 target/uid 状态。

流程：
  读取 uid 对应的 status：
    status 是当前 uid 的软件状态真源，包含 active、target dispatched、issue_epoch、replay_seq、fault、redirect 等字段。

  检查 uid 是否仍 active：
    如果 active 为 0，说明 uid 当前没有有效动态实例，当前 writeback 是迟到或无法匹配的旧事件，打印 warning 并返回失败。

  检查 issue_killed、redirect_pending、flushed：
    如果任一字段为 1，说明该 uid 的旧动态实例已经被 redirect/recovery 清理，当前 writeback 不能再推进状态，打印 warning 并返回失败。

  检查 target 是否已经 dispatched：
    如果 target 没有 dispatched，说明软件状态机并未等待该 target 的 writeback，当前 event 可能是旧实例或非法 event，打印 warning 并返回失败。

  检查 target 是否已经完成：
    如果 target 已经 writeback/pass/fault done，说明这是重复或迟到 event，打印 warning 并返回失败，避免重复写状态。

  检查 issue_epoch：
    issue_epoch 用于区分同一 uid/target 的不同 issue fire 实例。
    如果 status 中记录的 target issue_epoch 与 event 携带的 issue_epoch 不一致，说明 event 属于旧 fire 实例，打印 warning 并返回失败。

  检查 replay_seq：
    replay_seq 用于区分 replay 前后的动态 target 实例。
    如果 replay_seq 不匹配，说明 event 属于 replay 前旧实例，打印 warning 并返回失败。

  检查 pass/fault 特有条件：
    normal pass 路径要求 uid 当前没有 fault/exception_pending，否则不能把异常路径改成 pass。
    fault 路径如果已经 exception_pending，则当前 fault event 不能重复覆盖已有异常状态。

  所有检查通过后返回成功，调用方才允许写 target/uid 状态。

mark_target_normal_pass 的功能：
  在真实 writeback normal pass 通过前置状态检查后，把 target 和 uid 级 pass/writeback 状态落到 status 表。

mark_target_normal_pass 流程：
  先调用 check_target_writeback_pre_state，输入 uid、target、issue_epoch、replay_seq 和 is_fault=0。
  如果检查失败，向调用方报告“本次 normal pass 不接受”，并且不修改任何状态。
  检查通过后，设置该 target 的 writeback 和 pass 字段，表示这个 target 已经正常完成。
  调用 required_targets_done 判断该 uid 所需 target 是否全部完成。
  如果所有 target 完成，并且 uid 没有 replay/redirect/exception pending，则设置 uid 级 writeback/pass。
  最后向调用方报告“本次 normal pass 已接受并落表”。

mark_target_fault 的功能：
  在真实 writeback fault 通过前置状态检查后，把 target fault 和 uid 级 fault/exception_pending 状态落到 status 表。

mark_target_fault 流程：
  先调用 check_target_writeback_pre_state，输入 uid、target、issue_epoch、replay_seq 和 is_fault=1。
  如果检查失败，向调用方报告“本次 fault 不接受”，并且不修改任何状态。
  检查通过后，设置该 target 的 writeback 和 fault 字段。
  设置 uid 级 fault 和 exception_pending，保存 exception_vec，阻止该 uid 后续按 normal pass/success 继续提交。
  最后向调用方报告“本次 fault 已接受并落表”。
```

代码完成后的检查点：必须确认 `mark_target_normal_pass()` 和 `mark_target_fault()` 或二者复用的公共 helper 已实现上述前置检查；检查失败只能 `uvm_warning` 并 drop event，不能修改 target/uid 状态。

### 1.3 Redirect / Replay Candidate 入队 Flow

recovery candidate 是“已经采集到，但暂时不能执行”的 redirect/replay event。candidate 入队不设置 `active_redirect`，不 push redirect drive，不 mark replay pending。

文字伪代码：

```text
push_recovery_candidate 的功能：
  把 monitor 已经采集并 normalize 的 redirect/replay event 保存到 recovery_candidate_q。
  入队只表示“等待 commit frontier 放行”，不代表 recovery 已经执行。

流程：
  调用 normalize_feedback_event：
    normalize_feedback_event 负责解析 uid，补齐 ROB key、issue_epoch、replay_seq 等字段。
    如果 normalize 失败，说明 event 无法绑定到当前有效动态实例，打印 warning 并 drop，不入队。

  检查 event 类型：
    只有 redirect/replay 可以进入 recovery_candidate_q。
    如果 normalized_event 不是 redirect/replay，打印 warning 或 fatal，避免 normal pass/fault 误进 recovery 队列。

  检查 uid 是否已经落在 success_prefix 之前：
    success_prefix 表示不可回滚的连续成功前缀。
    如果 candidate.uid 小于 success_prefix，说明 commit flow 已经绕过 recovery candidate，必须报错或 fatal。

  检查 ROB key 是否有效：
    recovery candidate 后续需要按 ROB 顺序比较、按 redirect 覆盖范围清理。
    如果 has_rob 为 0，说明 candidate 没有 ROB 身份，不能安全进入 pending queue，只能 warning/drop。

  将 normalized_event push 到 recovery_candidate_q：
    队列元素是完整 normalized event，后续由 commit frontier 的 process_recovery_candidate_for_uid 出队消费。

  调用 update_recovery_frontier_tracking：
    该 helper 重新扫描 recovery_candidate_q，更新 debug/tracking 字段，方便日志和 no-progress 诊断。
    它不决定执行顺序，也不阻塞 admission/route/issue/commit。

update_recovery_frontier_tracking 的功能：
  维护 recovery_candidate_valid、recovery_frontier_uid、recovery_frontier_rob_key 三个观察字段。
  它只描述当前 pending candidate 队列的最老 ROB 项，用于 debug/tracking。

update_recovery_frontier_tracking 流程：
  如果 recovery_candidate_q 为空：
    清 recovery_candidate_valid、recovery_frontier_uid、recovery_frontier_rob_key。
    这表示当前没有 pending recovery candidate。

  如果 recovery_candidate_q 非空：
    从队列头开始扫描所有 candidate。
    如果发现 candidate 缺少 ROB key，说明队列被非法 event 污染；打印 warning 或 fatal，并按实现策略跳过或删除该非法项。
    对所有合法 candidate，比较 ROB 顺序，选择 ROB 最老的 candidate 作为 tracking selected。

  扫描完成后：
    如果没有任何合法 candidate，清 tracking 字段。
    如果找到了 selected candidate，置 recovery_candidate_valid=1，并把 selected.uid 和 selected.rob_key 写入 tracking 字段。

  重要限制：
    本函数不能做全局 redirect-first 仲裁。
    redirect 优先 replay 只发生在 commit cursor 命中同一个 uid 后，由 pop_recovery_candidate_for_uid(uid) 在同 uid 候选集合内处理。
    本函数也不能用 uid 小作为无 ROB candidate 的正常 fallback。
```

注意：`recovery_frontier_uid/recovery_frontier_rob_key` 只是 debug/tracking 字段，不能被 admission、route、issue、commit 当作全局 hold 边界。redirect 优先 replay 的规则只在 commit cursor 命中某个 uid 后，由 `pop_recovery_candidate_for_uid(uid)` 在同一 uid 的候选集合内执行；`update_recovery_frontier_tracking()` 不能做全局 redirect-first 仲裁，也不能用 uid 小作为无 ROB key candidate 的正常 fallback。

### 1.4 Candidate 等待期间的 Admission / Route / Issue / Commit Flow

pending candidate 不阻塞正常请求流动。DUT 在 older redirect 真正执行前仍可能继续 admission、issue、writeback、commit，测试框架也要保留这个行为。

等待期间规则：

- LSQ admission 不检查 `recovery_candidate_q` 或 `recovery_frontier_uid`。
- issue route 不因为 pending candidate 跳过 uid。
- issue queue select/fire 不因为 `item.uid >= recovery_frontier_uid` 跳过。
- ROB commit / LQ/SQ deq 不全局暂停。
- 唯一特殊点：commit cursor 扫到某个 uid 时，必须先检查该 uid 是否有 recovery candidate。

文字伪代码：

```text
LSQ admission 等待期间流程：
  admission 逻辑继续按原条件选择 uid。
  它可以检查 active、enq、LSQ 资源、顺序入队条件和全局 flush 状态。
  它不能因为 recovery_candidate_q 非空而停止，也不能因为 uid 大于等于 recovery_frontier_uid 而停止。
  如果后续 older redirect 覆盖这些已经 admission 的 uid，apply_redirect_flush_range 会负责清理旧动态实例。

Issue route 等待期间流程：
  route_all_ready_uids 继续按原条件扫描 uid。
  is_uid_route_ready 仍检查 active、enq、issue_ready、replay_target、exception/redirect/flushed 等状态。
  pending candidate 本身不是 route 阻塞条件。
  满足 route 条件的 uid 仍可进入 LOAD/STA/STD issue queue。

Issue select/fire 等待期间流程：
  issue queue select 继续按 ready_cycle、send_pri、ROB age、pipe num 等原规则选择 item。
  item.uid 与 recovery_frontier_uid 的大小关系不能作为跳过条件。
  已经 fire 到 DUT pipeline 的请求不能撤回；如果之后被 older redirect 覆盖，靠 redirect flush 和 stale writeback 过滤处理。

ROB commit / LQ/SQ deq 等待期间流程：
  commit/deq 不因为 recovery_candidate_q 非空而全局暂停。
  commit cursor 扫到某个 uid 时，必须先调用 has_recovery_candidate_for_uid(uid)。
  如果当前 uid 有 candidate，本轮切到 candidate recovery flow，不允许该 uid 先普通 success。
  如果当前 uid 没有 candidate，继续按原 uid_is_commit_candidate、ROB commit、LQ/SQ deq 条件处理。
```

### 1.5 Commit Frontier 触发 Candidate Flow

candidate 的执行门槛是 commit cursor/frontier 扫到 `candidate.uid`。检查必须发生在 `uid_is_commit_candidate()` 之前，避免 candidate uid 先被置 success。

文字伪代码：

```text
select_rob_commit_batch 的功能：
  构造本拍要 drive 给 DUT/接口模型的 ROB commit uid 列表。
  新方案要求它在检查普通 commit 条件前，先检查当前 commit cursor uid 是否有 pending recovery candidate。

select_rob_commit_batch 流程：
  开始时先清空输出 commit_uids：
    commit_uids 保存本拍普通 commit 的 uid 列表。
    清空它可以避免上一拍残留 uid 被重复提交。

  同时清 blocked_recovery_uid_valid：
    这个标志表示本拍是否被某个 recovery candidate 卡在 commit frontier。
    初始化为 0，表示默认没有 blocked recovery。

  先检查 issue_blocked_by_global_flush：
    该 helper 汇总 flush_in_progress、active_redirect、issue_freeze_ack、pending_redirect_drive 和 dispatch_flush_in_progress。
    如果全局 flush 正在进行，本拍不构造普通 commit，也不启动新的 candidate，直接返回空 batch。

  调用 advance_commit_cursor_past_done：
    这个 helper 跳过已经 success 的连续 uid，把 commit cursor 对齐到当前最老未完成 uid。

  从 commit_cursor_uid 开始扫描：
    如果当前 uid 已经 success，继续跳过到下一个 uid。
    先调用 has_recovery_candidate_for_uid(uid)。如果查询结果表示“当前 uid 有 pending recovery”，必须阻止普通 commit：
      设置 blocked_recovery_uid 和 blocked_recovery_uid_valid。
      立即返回，不再调用 uid_is_commit_candidate，也不扫描更年轻 uid。
    如果当前 uid 没有 candidate，才调用 uid_is_commit_candidate：
      uid_is_commit_candidate 检查 active、writeback/pass、target done、fault/replay/redirect pending、ROB/LQ/SQ 条件等普通提交条件。
      如果不满足，当前 commit frontier 还不能推进，返回当前已收集的 batch。
    如果普通 commit 条件满足，把 uid 加入 commit_uids，继续扫描直到达到 commit 宽度或遇到不能 commit 的 uid。

send_lsqcommit_cycle 的功能：
  消费 select_rob_commit_batch 的输出，真正 drive commit transaction，并决定是否启动 candidate recovery。

send_lsqcommit_cycle 流程：
  调用 commit_handler 构造 commit transaction，并取得 commit_uids、blocked_recovery_uid_valid、blocked_recovery_uid。
  如果 blocked_recovery_uid_valid 为 1：
    drive idle/empty lsqcommit xaction，保持接口节奏，但不提交任何 uid。
    调用 process_recovery_candidate_for_uid(blocked_recovery_uid)，把当前 commit frontier 的 candidate 转成真正 redirect/replay recovery。
    本轮结束，不能调用 mark_rob_commit_batch。
  如果没有 blocked recovery 且 commit_uids 非空：
    drive normal commit xaction。
    调用 mark_rob_commit_batch，把这些 uid 标记为 ROB commit，并继续走 retire/success 逻辑。
```

实现 review 时要确认：candidate 检查落在真实 drive LSQ commit 的 commit handler 路径上，不能写到另一个 monitor-only commit handler 实例里。

### 1.6 Candidate 执行选择 Flow

同一个 uid 可能同时有 redirect 和 replay candidate，必须有稳定优先级：redirect 优先 replay；多个 redirect 选择 ROB oldest；redirect 被选中后，同 uid replay candidate 应删除或失效。

文字伪代码：

```text
process_recovery_candidate_for_uid 的功能：
  只处理 commit frontier 当前 uid 的 recovery candidate。
  它是 pending candidate 真正变成 active redirect 或 replay_pending 的唯一入口。

process_recovery_candidate_for_uid 流程：
  先调用 service_ptw_wait_replay：
    这个 helper 只处理之前已经被 frontier 放行、但还在等 PTW/TLB 条件的 replay。
    它不能扫描新的 recovery_candidate_q，也不能绕过 commit frontier 启动 replay。

  再调用 advance_active_redirect：
    这个 helper 推进已经启动的 active_redirect。
    如果 redirect drive 已完成，它会调用 apply_redirect_flush 清理状态并解除 flush。

  如果 advance 后 active_redirect 仍有效：
    说明当前已有 redirect 正在执行，不能并行启动另一个 redirect/replay。
    当前 blocked uid 留到后续 service cycle 再处理。

  调用 pop_recovery_candidate_for_uid(uid)：
    这个 helper 只从 recovery_candidate_q 中取当前 uid 的 candidate。
    如果取不到，说明 candidate 已被 older redirect 清理或已过期，打印 warning/drop 后结束。

  调用 recheck_recovery_candidate_instance：
    重新检查 selected candidate 的 uid、ROB、issue_epoch、replay_seq、target_dispatched 等快照是否仍属于当前动态实例。
    如果重检失败，说明 candidate 已过期，drop 并打印原因。

  如果 selected 是 redirect：
    调用 request_redirect_flush 设置 active_redirect、flush_in_progress、issue_freeze_ack 和 dispatch_flush_in_progress。
    随后显式调用 push_redirect_drive，把 redirect payload 放入 redirect driver 队列。
    本轮结束，后续由 redirect sequence drive，并由 service loop 的 advance_active_redirect 推进 apply flush。

  如果 selected 是 replay：
    调用 handle_sta_replay_candidate。
    replay 只修改对应 uid/target 的 replay_pending/target 状态，不设置全局 flush，也不阻塞其它 uid 的 admission/issue。

pop_recovery_candidate_for_uid 的功能：
  从 recovery_candidate_q 中删除并返回当前 uid 应执行的 candidate。
  同一 uid 内 redirect 优先 replay；多个 redirect 选 ROB oldest。

pop_recovery_candidate_for_uid 流程：
  扫描 recovery_candidate_q，收集 uid 等于输入 uid 的 candidate。
  如果没有同 uid candidate，返回失败，不改变队列。
  对同 uid candidate 做合法性检查：如果发现缺 ROB key 的非法 candidate，打印 warning/fatal，并按实现策略删除。
  如果同 uid 中存在 redirect candidate：
    选择 ROB oldest redirect 作为 selected。
    从 recovery_candidate_q 删除 selected。
    删除同 uid 中已被该 redirect 覆盖或语义上无效的 replay candidate，并打印 drop reason。
    调用 update_recovery_frontier_tracking 刷新 debug/tracking 字段。
    返回成功。
  如果同 uid 中没有 redirect：
    选择最早入队的 replay candidate 作为 selected。
    从 recovery_candidate_q 删除 selected。
    调用 update_recovery_frontier_tracking。
    返回成功。
```

### 1.7 Active Redirect Drive / Apply Flush Flow

`active_redirect` 表示已经决定执行并正在 drive/apply 的 redirect。它的推进 owner必须是 service loop / recovery handler，而不能依赖 LSQ commit loop，因为 LSQ commit 在 global flush 期间可能只发 idle 并返回。

文字伪代码：

```text
exception_redirect_replay_task 的功能：
  每个 service cycle 推进已经启动的 recovery。
  在新方案下，它不主动扫描 recovery_candidate_q 启动 redirect/replay。

exception_redirect_replay_task 流程：
  调用 service_ptw_wait_replay：
    处理已经被 frontier 放行并进入 PTW wait 队列的 replay。
  调用 advance_active_redirect：
    检查 active_redirect 是否已经完成 drive；完成后执行 apply_redirect_flush。
  消费 fault-only feedback event：
    fault 第一版仍可保留旧消费路径，但 redirect/replay 不允许在这里即时启动。

advance_active_redirect 的功能：
  负责 active_redirect 启动后的持续推进，直到 redirect drive 完成并 apply flush。

advance_active_redirect 流程：
  如果 active_redirect 无效，说明当前没有正在执行的 redirect，直接结束。
  如果 active_redirect 有效，读取当前 redirect payload。
  调用 redirect_drive_done_for 判断 redirect driver 是否已经完成该 payload 的 drive，并至少跨过必要 service cycle。
  如果 drive 已完成，调用 apply_redirect_flush 清理被覆盖 uid、candidate、PTW wait replay，并解除 global flush 状态。
  如果 drive 长时间未完成且超过 redirect_freeze_timeout，只上报 uvm_warning，提示 redirect drive 长时间未完成；本函数不能直接 fatal 或主动退出 testcase，最终退出必须交给现有统一 no-progress/global stop 退出流程。

request_redirect_flush 的功能：
  建立 active redirect 的全局冻结状态。
  它只设置状态，不隐式把 payload 放入 redirect drive queue。

request_redirect_flush 流程：
  保存 active_redirect payload，供 monitor batch 过滤和 apply flush 使用。
  设置 flush_in_progress、issue_freeze_ack 和 dispatch_flush_in_progress，后续 admission/route/issue/commit 会通过 issue_blocked_by_global_flush 被阻塞。
  不调用 push_redirect_drive；redirect drive 入队必须由 process_recovery_candidate_for_uid 的 redirect 分支显式完成。

process_recovery_candidate_for_uid 的 redirect 分支功能：
  把 commit frontier 选中的 redirect candidate 从 pending 状态转成真正 active redirect。

redirect 分支流程：
  先调用 request_redirect_flush 建立全局 flush/freeze 状态。
  再调用 push_redirect_drive，把同一个 redirect payload 放入 redirect sequence 的待 drive 队列。
  返回后不再处理其它 candidate，等待 redirect sequence 和 advance_active_redirect 推进。

apply_redirect_flush 的功能：
  redirect 已经 drive 完成后，清理被覆盖旧动态实例，并解除全局 flush/freeze。

apply_redirect_flush 流程：
  调用 apply_redirect_flush_range 扫描 active/admitted uid 窗口，找出被 redirect 覆盖且未 success 的 uid，并调用 prepare_uid_for_redirect_reissue 清状态。
  调用 clear_ptw_wait_replay_by_redirect，删除被 redirect 覆盖的 PTW wait replay。
  调用 drop_candidates_covered_by_redirect，删除 recovery_candidate_q 中被当前 redirect 覆盖的 younger candidate。
  调用 clear_redirect_drive_queue，清理 redirect drive pending/inflight 记录。
  清 flush_in_progress、issue_freeze_ack、dispatch_flush_in_progress、active_redirect 和 redirect_phase，让 admission/route/issue/commit 恢复正常。
```

### 1.8 Redirect Reissue Flow

redirect apply flush 后，被覆盖且未 success 的 uid 要清成“旧动态实例失效，等待同 uid 重新 admission/reissue”的状态。第一版不支持 success 回滚；如果 flush 命中 success uid，说明 commit frontier 检查被绕过，应报错。

文字伪代码：

```text
apply_redirect_flush_range 的功能：
  在 redirect apply 阶段扫描当前已 admission 的 uid 范围，找出被 redirect 覆盖的旧动态实例并准备重新入队。

apply_redirect_flush_range 流程：
  先调用 advance_success_prefix，把 success_prefix_uid 推到当前连续 success 前缀之后。
  以 success_prefix_uid 作为扫描起点，以当前已 admission 上界作为扫描终点。
  对每个 uid 读取 status：
    如果 uid 已经 success，但又被当前 redirect 覆盖，说明 success 不回滚假设被破坏，必须 fatal/error。
    如果 uid 没有 active、writeback、pass、fault、replay、issue 等旧动态实例痕迹，说明它不需要清理，跳过。
    如果 rob_need_flush 判断该 uid 的 ROB 被 redirect 覆盖，调用 prepare_uid_for_redirect_reissue 清理该 uid，并记录最老被 flush 的 uid。
  扫描完成后，如果本次确实 flush 了 uid，回退 max_enqueued_uid/admission progress，让后续 LSQ admission 从最老 flush uid 重新开始。

prepare_uid_for_redirect_reissue 的功能：
  把被 redirect 覆盖的 uid 清成“旧动态实例已经失效，等待同 uid 重新 admission”的状态。

prepare_uid_for_redirect_reissue 流程：
  先检查 redirect payload 必须 valid。
  读取 uid 当前 status，如果 status.success 为 1，说明 commit frontier 保护失效，必须 fatal。
  读取 main transaction，保存该 uid 原来是否占用 LQ/SQ mapping。
  如果 status.active 为 1，调用 retire_active_uid：
    retire_active_uid 会删除 active ROB/LQ/SQ map，并清理该 uid 在 issue queue 中的残留项。
  如果 status.active 为 0，调用 remove_uid_from_issue_queues：
    删除该 uid 可能残留在 LOAD/STA/STD issue queue 中的 item。
  根据原 LQ/SQ mapping 累加 pending_lq_cancel_count/pending_sq_cancel_count：
    这些计数后续由 LSQ admission/cancel 镜像消费，用于恢复软件 LSQ 资源模型。
  调用 clear_uid_dispatch_result：
    清 enq、issue_ready、queued/dispatched、writeback/pass、fault、replay、commit、deq、success 等旧动态结果。
  设置 redirect_pending 和 flushed：
    表示该 uid 需要重新 admission/reissue，且旧实例的迟到 event 不能继续落状态。
  递增 dynamic_epoch，用于 debug 和后续增强 stale event 匹配。
  设置 active=0 和 success=0，等待同 uid 后续重新进入 LSQ admission。
```

### 1.9 STA Replay Flow

STA replay candidate 只有在 commit frontier 命中且没有 redirect 抢占后才允许执行。monitor 采到 replay 时不能提前 `mark_replay_pending()`，也不能提前进入 `ptw_wait_replay_q`。

文字伪代码：

```text
handle_sta_replay_candidate 的功能：
  把已经被 commit frontier 放行的 replay candidate 转成 replay_pending 或 PTW wait replay。

handle_sta_replay_candidate 流程：
  先调用 resolve_uid_for_event：
    通过 event 中的 uid/ROB/LQ/SQ 信息重新确认该 event 仍能定位到当前 active uid。
    如果解析失败，说明 candidate 已过期或被 redirect 清理，打印 warning 并 drop。
  调用 get_event_issue_epoch 和 get_event_replay_seq：
    取得 replay candidate 对应的 issue 实例快照，用于 mark_replay_pending 的 stale event 检查。
  调用 event_should_wait_ptw：
    如果 replay 需要等待 PTW/TLB 条件，调用 push_ptw_wait_replay 把 uid、target、issue_epoch、replay_seq 放入 PTW wait replay 队列。
    这个队列只能在 candidate 已被 commit frontier 放行后产生，不能由 monitor 早采集阶段直接产生。
  如果不需要等待 PTW，调用 mark_replay_pending：
    mark_replay_pending 会做 active、issue_killed、target_dispatched、issue_epoch、replay_seq 检查。
    检查通过后才设置 replay_pending 和 replay target。

mark_replay_pending 的功能：
  将指定 uid/target 从“已发射等待反馈”退回到“需要重新 route/fire”的 replay 状态。

mark_replay_pending 流程：
  读取 uid status。
  如果 target 是 STD，当前第一版不支持 STD backend replay，打印 warning/drop 并返回失败。
  检查 status.active 和 issue_killed：
    非 active 或已 killed 的 uid 不能进入 replay，返回失败。
  检查 target_dispatched：
    只有已经发射过的 target 才能 replay；未 dispatched 的 target 说明 event 过期或非法。
  检查 issue_epoch 和 replay_seq：
    不匹配说明 replay event 属于旧实例，返回失败。
  删除该 uid/target 在 issue queue 中的旧 item，避免旧 replay_seq item 再次被 fire。
  清 uid 级 writeback/pass/success，因为该 target 需要重新完成。
  清该 target 的 queued、dispatched、writeback、pass、feedback success 状态。
  设置 replay_target_load 或 replay_target_sta，告诉 route 逻辑只重新 route 需要 replay 的 target。
  bump replay_seq，让旧 writeback/replay event 失效。
```

### 1.10 Success / End Test 检查 Flow

`success_prefix_uid` 继续单调前进，不感知 candidate queue。防止 candidate 被绕过的责任在 success 写入路径和 commit frontier 检查。

文字伪代码：

```text
try_retire_committed_uid 的功能：
  在 uid 已 ROB commit 且 LQ/SQ mapping 已释放后，把 uid 标记成最终 success 并 retire active map。

try_retire_committed_uid 流程：
  在写 success 前先调用 has_recovery_candidate_for_uid(uid)。
  如果该 uid 仍有 pending candidate，说明 commit frontier candidate 检查被绕过，必须 fatal/error，并拒绝写 success。
  如果没有 pending candidate，再按原逻辑检查 rob_commit、LQ/SQ deq、fault/replay/redirect/flushed 等条件。
  条件满足后设置 status.success，并调用 retire_active_uid 删除 active ROB/LQ/SQ map。
  最后调用 advance_success_prefix 推进连续 success 前缀。

transaction_done 的功能：
  判断所有 uid 是否已经形成连续 success 前缀。它不负责检查 recovery 队列和 active redirect 是否 idle。

transaction_done 流程：
  先调用 advance_success_prefix 更新连续 success 前缀。
  如果 success_prefix_uid 已到 main_trans_num，则表示所有 uid 从主流程角度已经 success，可以返回完成。
  不把 recovery_candidate_q、exception_event_q、ptw_wait_replay_q、active_redirect、redirect drive 或 flush 状态混入 transaction_done，避免把结束一致性检查误写成主流程完成条件。

end_test_check 的功能：
  testcase 结束时做最后一致性检查。除 success_prefix 已完成外，还必须确认 recovery 相关队列和 active redirect 全部 idle，防止未消费 recovery event 被静默忽略。

end_test_check 流程：
  如果 recovery_candidate_q 非空，报错，说明还有 redirect/replay candidate 没有执行或没有被 redirect 清理。
  如果 exception_event_q 非空，报错，说明还有 fault 或误入旧队列的 event 未消费。
  如果 ptw_wait_replay_q 非空，报错，说明已经放行的 replay 仍在等待。
  如果 active_redirect 有效或 redirect drive 队列未空，报错，说明 redirect recovery 尚未完成。
```

### 1.11 最终验收检查点

代码完成后必须逐项确认：

1. `recovery_candidate_q` 中 redirect/replay 不会立即触发 recovery。
2. 只有 commit cursor/frontier 扫到 `candidate.uid` 且动态实例重检通过时 candidate 才允许执行。
3. `advance_success_prefix()` 不查询 candidate queue，只按 `status.success` 连续前缀推进。
4. pending candidate 存在时，不因为 `uid >= recovery_frontier_uid` 暂停 admission、route、issue fire、ROB commit 或 LQ/SQ deq。
5. older redirect 后到时，能覆盖并 drop younger redirect/replay candidate。
6. older redirect apply flush 能清理覆盖范围内未 success uid 的 active/pass/replay/issue/LSQ/candidate 状态；如果命中 success uid，应报错。
7. STA replay candidate 只有在 frontier eligible 且重检通过后才 mark replay pending。
8. 所有 stale/drop candidate 路径需要 `uvm_warning` 或 `uvm_info` 说明 uid、rob、source 和原因。
9. 同一 uid 同时存在 redirect/replay candidate 时，redirect 必须优先。
10. 有 pending recovery candidate 的 uid 不允许继续落 normal pass/fault 推进状态。
11. active_redirect 启动后，即使 LSQ commit loop 因 global flush 发 idle，service loop 仍能推进 drive done 和 apply flush。
12. 写回更新状态前必须检查“写回前一状态”，检查失败只能 warning/drop，不能修改状态。

## 2. 具体实施清单

### 2.1 `common_data_transaction` 新增字段

字段：

```systemverilog
memblock_wb_event_t recovery_candidate_q[$];
bit                 recovery_candidate_valid;
memblock_uid_t      recovery_frontier_uid;
memblock_rob_key_t  recovery_frontier_rob_key;
```

功能：

- `recovery_candidate_q` 保存 pending redirect/replay candidate。
- `recovery_candidate_valid` 表示当前是否存在 pending candidate。
- `recovery_frontier_uid` 记录最老 pending candidate uid，仅用于 debug/tracking。
- `recovery_frontier_rob_key` 记录最老 pending candidate ROB key，仅用于日志和诊断。

输入：monitor batch normalize 后的 redirect/replay event。

输出/副作用：为 commit handler、recovery handler、redirect flush cleanup 提供统一 candidate 状态。

文字伪代码：

```text
reset_all_tables 中的字段初始化：
  新 testcase 开始时清空 recovery_candidate_q，避免上一个 testcase 的 pending recovery event 污染当前测试。
  将 recovery_candidate_valid 清 0，表示当前没有 pending candidate。
  将 recovery_frontier_uid 和 recovery_frontier_rob_key 清零，清除 debug/tracking 旧值。

end_test_check 中的字段检查：
  testcase 结束时检查 recovery_candidate_q 必须为空。
  如果不为空，说明存在已经采集但没有被 commit frontier 消费、也没有被 redirect flush 清掉的 recovery event，应报错。
```

### 2.2 新增 `push_recovery_candidate()`

功能：把 monitor 已采集并 normalize 的 redirect/replay 放入 candidate queue。

输入：`memblock_wb_event_t wb_event`。

输出/副作用：可能入队，更新 frontier tracking；失败时 warning/drop 或 fatal。

文字伪代码：

```text
push_recovery_candidate 的执行流程：
  首先调用 normalize_feedback_event，把输入 event 规范化成携带 uid、ROB key、issue_epoch、replay_seq 的 normalized_event。
  如果 normalize 失败，打印 source/reason 后 drop，函数返回失败，不修改 recovery_candidate_q。

  检查 normalized_event 类型。
  如果它不是 redirect/replay，说明调用方把普通 event 错送到 candidate 入口，打印 uid/source/type 后 warning 或 fatal，函数返回失败。

  读取 normalized_event.uid，并检查它是否小于 success_prefix_uid。
  如果已经落在 success prefix 之前，说明该 uid 已被视为不可回滚 success，candidate 到达太晚或 commit flow 绕过了检查，应 error/fatal 并返回失败。

  检查 normalized_event.has_rob。
  如果没有 ROB key，后续无法做 ROB older 比较，也无法被 redirect 覆盖判断安全清理，只能 warning/drop，不能进入 pending queue。

  所有检查通过后，将 normalized_event 追加到 recovery_candidate_q。
  入队后调用 update_recovery_frontier_tracking，刷新 debug/tracking 字段。
  返回成功，表示 candidate 已等待 commit frontier 消费。
```

### 2.3 新增 `update_recovery_frontier_tracking()`

功能：维护当前最老 pending candidate 的 debug 字段。

输入：`recovery_candidate_q`。

输出/副作用：更新 `recovery_candidate_valid/recovery_frontier_uid/recovery_frontier_rob_key`。

文字伪代码：

```text
update_recovery_frontier_tracking 的执行流程：
  先检查 recovery_candidate_q 是否为空。
  如果为空，清 recovery_candidate_valid、recovery_frontier_uid 和 recovery_frontier_rob_key，表示当前没有 pending candidate，然后结束。

  如果队列非空，遍历 recovery_candidate_q 中所有 candidate。
  对每个 candidate 先检查 has_rob。
  如果 has_rob 为 0，说明队列里存在非法 candidate，打印 warning 或 fatal，并按实现策略跳过或删除该项。

  对所有合法 candidate，按 ROB 顺序选择最老的一项。
  如果还没有 selected，就把当前 candidate 作为 selected。
  如果已有 selected，则调用 ROB 顺序比较 helper 判断当前 candidate 是否比 selected 更老；更老则替换 selected。

  遍历完成后，如果没有任何合法 candidate，清 tracking 字段并结束。
  如果存在 selected，设置 recovery_candidate_valid=1，并把 selected.uid 和 selected.rob_key 写入 recovery_frontier_uid/recovery_frontier_rob_key。

  本函数只更新观察字段。
  它不启动 recovery，不选择同 uid redirect/replay 优先级，不阻塞 admission/route/issue/commit，也不允许无 ROB candidate 通过 uid fallback 成正常语义。
```

### 2.4 新增 `has_recovery_candidate_for_uid()`

功能：给 commit frontier 精确查询当前 uid 是否有 pending candidate。

输入：`memblock_uid_t uid`。

输出/副作用：返回 bit；无状态副作用。

文字伪代码：

```text
has_recovery_candidate_for_uid 的执行流程：
  遍历 recovery_candidate_q 中的每个 candidate。
  对每个 candidate 比较 candidate.uid 和输入 uid。
  如果找到相同 uid，立即向调用方报告“该 uid 有 pending candidate”，表示 commit cursor 到这个 uid 时必须先处理 recovery。
  如果遍历结束仍未找到，向调用方报告“该 uid 没有 pending candidate”。
  本函数只查询，不删除 candidate，不更新 tracking，也不修改任何 status。
```

### 2.5 新增 `pop_recovery_candidate_for_uid()`

功能：commit cursor 命中 uid 后取出该 uid 应执行的 candidate。

输入：`uid`。

输出/副作用：输出 selected event；删除 selected；可能删除同 uid 无效 replay；更新 tracking。

文字伪代码：

```text
pop_recovery_candidate_for_uid 的执行流程：
  遍历 recovery_candidate_q，收集所有 candidate.uid 等于输入 uid 的队列位置。
  如果没有同 uid candidate，返回失败，不修改队列。

  对同 uid candidate 做合法性检查。
  如果发现缺 ROB key 的非法 candidate，打印 warning 或 fatal，并从队列中删除该非法项，避免后续 ROB 顺序判断出错。

  如果剩余同 uid candidate 中存在 redirect：
    在这些 redirect 中按 ROB 顺序选择最老 redirect 作为 selected。
    从 recovery_candidate_q 删除 selected。
    再检查同 uid replay candidate；如果 replay 被 selected redirect 覆盖，或语义上属于同一旧动态实例，删除这些 replay 并打印 drop reason。
    调用 update_recovery_frontier_tracking 刷新 tracking 字段。
    返回成功。

  如果同 uid 中没有 redirect：
    选择同 uid 最早入队的 replay candidate 作为 selected。
    从 recovery_candidate_q 删除 selected。
    调用 update_recovery_frontier_tracking。
    返回成功。

  redirect 优先只在同一个 uid 的候选集合内生效，不能扩展成全局 redirect 优先。
```

### 2.6 新增 `drop_candidates_covered_by_redirect()`

功能：redirect apply flush 后清理被当前 redirect 覆盖的 pending candidate。

输入：`redirect_payload_t redirect`。

输出/副作用：删除被覆盖 candidate，打印 drop 原因，更新 tracking。

文字伪代码：

```text
drop_candidates_covered_by_redirect 的执行流程：
  遍历整个 recovery_candidate_q，而不是只看 active window。
  对每个 candidate，先确认它有 ROB key。
  调用 rob_need_flush(candidate.rob_key, redirect) 判断该 candidate 是否被当前 redirect 覆盖。
  如果被覆盖，从 recovery_candidate_q 删除该 candidate，并打印 uid、rob、source 和 drop reason。
  遍历完成后调用 update_recovery_frontier_tracking，确保 tracking 字段不再指向已删除 candidate。
```

### 2.7 新增 `check_target_writeback_pre_state()`

功能：作为 normal pass/fault 写状态前的统一前置检查。

输入：`uid`、`target`、`issue_epoch`、`replay_seq`、`is_fault`。

输出/副作用：返回 bit；失败时打印 `uvm_warning`；不得修改状态。

文字伪代码：

```text
check_target_writeback_pre_state 的执行流程：
  读取 uid 对应 status，作为后续判断的状态真源。
  如果 uid 非 active，打印 warning 并返回失败。
  如果 issue_killed、redirect_pending 或 flushed 任一为 1，说明旧动态实例已经失效，打印 warning 并返回失败。
  如果 target 没有 dispatched，说明该 target 当前不处于等待 writeback 的状态，打印 warning 并返回失败。
  如果 target 已经 done，说明 event 是重复或迟到返回，打印 warning 并返回失败。
  如果 target issue_epoch 与 event issue_epoch 不匹配，打印 warning 并返回失败。
  如果 target replay_seq 与 event replay_seq 不匹配，打印 warning 并返回失败。
  normal pass 路径额外检查 uid 不能已有 fault/exception_pending；否则打印 warning 并返回失败。
  fault 路径额外检查不能重复覆盖已有 exception_pending；否则打印 warning 并返回失败。
  全部检查通过后返回成功。
  本函数不写 status，只给 mark_target_normal_pass/mark_target_fault 做前置门禁。
```

### 2.8 修改 `mark_target_normal_pass()` / `mark_target_fault()`

功能：接入写回前置状态检查，防止旧 event 错误推进 uid 状态。

输入：原函数输入，加上 event issue_epoch/replay_seq。

输出/副作用：检查通过才写 target/uid 状态；失败只 warning/drop。

文字伪代码：

```text
mark_target_normal_pass 的执行流程：
  先调用 check_target_writeback_pre_state，确认 uid/target 正在等待本次 normal pass writeback。
  如果检查失败，向调用方报告“normal pass 未接受”，不写 target writeback，不写 target pass，也不写 uid pass。
  检查通过后，设置 target writeback/pass，并记录 cycle。
  调用 required_targets_done 判断 uid 所有必需 target 是否都完成。
  如果所有 target 完成，且 uid 没有 replay/redirect/exception pending，设置 uid 级 writeback/pass。
  最后向调用方报告“normal pass 已被接受”。

mark_target_fault 的执行流程：
  先调用 check_target_writeback_pre_state，确认 uid/target 正在等待本次 fault writeback。
  如果检查失败，向调用方报告“fault 未接受”，不写 target fault，也不写 uid exception 状态。
  检查通过后，设置 target writeback/fault，并记录 cycle。
  设置 uid fault 和 exception_pending，保存 exception_vec。
  最后向调用方报告“fault 已被接受”。
```

### 2.9 修改 `dispatch_monitor_batch_handler::process_monitor_event_batch()`

功能：redirect/replay 从旧即时恢复路径迁移到 candidate queue，同时保留 batch redirect-first 覆盖过滤。

输入：同一 service cycle event batch。

输出/副作用：redirect/replay 入 candidate；被覆盖 event drop；normal pass/fault 按前置检查处理。

文字伪代码：

```text
process_monitor_event_batch 的执行流程：
  调用 normalize_event_batch，把原始 batch 转成 normalized_events。
  如果没有有效 normalized event，直接结束。

  如果 active_redirect 有效：
    对每个 event 先判断是否被 active_redirect 覆盖。
    覆盖则 drop 并打印原因。
    未覆盖的 redirect/replay 调用 push_recovery_candidate 入队。
    未覆盖的 normal pass/fault 调用 process_allowed_non_redirect_event，继续做 pending candidate 和写回前置状态检查。

  如果 active_redirect 无效：
    调用 select_oldest_redirect 找本 batch 的 ROB oldest redirect。
    如果存在 selected redirect，先把它 push_recovery_candidate。
    对同 batch 其余 event：selected 本身跳过；被 selected 覆盖的 event drop；未覆盖 redirect/replay 入 candidate；未覆盖 normal pass/fault 走普通处理。

  如果本 batch 没有 redirect：
    replay event 进入 push_recovery_candidate。
    normal pass/fault 进入 process_allowed_non_redirect_event。
```

### 2.10 修改 `writeback_status_handler::handle_issue_feedback_event()`

功能：IQ feedback failed / replay 不再 `push_feedback_event()` 到旧 `exception_event_q`。

输入：IQ feedback event。

输出/副作用：replay event 进入 candidate queue；不立即 `mark_replay_pending()`。

文字伪代码：

```text
handle_issue_feedback_event 的执行流程：
  如果 event 表示 iq_feedback_failed/replay：
    先检查 replay candidate 需要的 uid、ROB、target、issue_epoch、replay_seq 等快照信息是否齐全。
    如果信息不足，打印 warning 并 drop，不能进入 candidate queue。
    如果信息齐全，调用 push_recovery_candidate 入队，等待 commit frontier。
    入队后直接返回，不能调用 push_feedback_event，也不能 mark_replay_pending。

  如果 event 是 issue feedback success：
    按原 success 逻辑处理；如果该 target 需要真实 writeback，则只记录 issue feedback success，不设置 pass。
```

### 2.11 修改 `exception_redirect_replay_handler::process_pending_events()`

功能：只推进已启动 recovery 和 fault-only 消费，不再主动 pop redirect/replay 执行。

输入：`exception_event_q`、`active_redirect`、`ptw_wait_replay_q`。

输出/副作用：推进 active redirect；消费 fault；若发现 redirect/replay 进入旧队列则 warning/fatal 或转入 candidate。

文字伪代码：

```text
process_pending_events 的执行流程：
  先调用 service_ptw_wait_replay：
    只处理已经被 commit frontier 放行后进入 PTW wait 的 replay。
  再调用 advance_active_redirect：
    推进已经启动的 active redirect，必要时 apply flush 并解除全局 flush。

  然后从 exception_event_q 中消费剩余 event。
  如果发现 redirect/replay event：
    说明仍有旧路径把 recovery event 送入即时队列。
    实现可以 warning/fatal，或迁移调用 push_recovery_candidate；但绝不能在这里直接 request_redirect_flush 或 mark_replay_pending。
  如果 event 是 fault：
    按 fault-only 消费路径处理；fault 第一版不作为 recovery candidate 的主路径。
```

### 2.12 新增 `process_recovery_candidate_for_uid()`

功能：commit frontier 命中 uid 后执行该 uid 的 candidate。

输入：`uid`。

输出/副作用：可能启动 active redirect；可能 mark replay pending；可能 drop stale candidate。

文字伪代码：

```text
process_recovery_candidate_for_uid 的执行流程：
  调用 service_ptw_wait_replay，处理此前已经放行的 PTW wait replay，不启动新的 candidate。
  调用 advance_active_redirect，推进已有 active redirect。
  如果 active_redirect 仍有效，说明当前 redirect 尚未完成，本函数直接返回，避免并行 recovery。

  调用 pop_recovery_candidate_for_uid(uid) 获取当前 commit frontier uid 的 candidate。
  如果没有取到 candidate，打印 warning/drop 后返回。

  调用 recheck_recovery_candidate_instance 重新确认 candidate 仍属于当前动态实例。
  如果重检失败，打印 stale candidate warning 并 drop。

  如果 selected 是 redirect：
    调用 request_redirect_flush 设置 active/global flush 状态。
    调用 push_redirect_drive 把 redirect payload 交给 redirect driver。
    返回，后续由 service loop 继续推进 active_redirect。

  如果 selected 是 replay：
    调用 handle_sta_replay_candidate，把 replay candidate 转成 replay_pending 或 PTW wait replay。
```

### 2.13 修改 `lsq_commit_handler` / `memblock_lsqcommit_dispatch_sequence`

功能：commit cursor 到 candidate.uid 前优先处理 candidate，阻止 uid 普通 success。

输入：commit cursor、status 表、candidate queue。

输出/副作用：输出 blocked recovery uid；blocked 时发 idle/empty commit 并调用 recovery handler。

文字伪代码：

```text
build/select commit batch 的执行流程：
  如果 issue_blocked_by_global_flush 判断当前处于 active redirect 或 flush，本轮只生成空 commit batch。
  调用 advance_commit_cursor_past_done，把 cursor 对齐到最老未完成 uid。
  从 commit_cursor_uid 开始扫描，直到 batch 满或遇到不能提交的 uid。
  如果当前 uid 已 success，跳过它。
  对每个未 success uid，先调用 has_recovery_candidate_for_uid。
  如果当前 uid 有 candidate，设置 blocked_recovery_uid 和 valid 标志，返回空 batch；不能继续检查 uid_is_commit_candidate。
  如果没有 candidate，再调用 uid_is_commit_candidate 判断普通提交条件。
  普通提交条件满足时，把 uid 加入 commit_uids；不满足时返回当前 batch。

send_lsqcommit_cycle 的执行流程：
  调用 build/select commit batch 取得 commit_uids 和 blocked_recovery_uid。
  如果 blocked_recovery_uid_valid 为 1：
    drive idle/empty lsqcommit xaction，保持接口节奏。
    不调用 mark_rob_commit_batch，避免该 uid 被错误标记 commit。
    调用 process_recovery_candidate_for_uid(blocked_uid) 启动该 uid 的 recovery。
  如果没有 blocked recovery 且 commit_uids 非空：
    drive normal commit xaction。
    调用 mark_rob_commit_batch，把这些 uid 标记 rob_commit，并进入后续 retire/success 流程。
```

### 2.14 修改 `apply_redirect_flush()`

功能：redirect apply flush 时同时清 status/LSQ/issue/PTW/candidate，并解除 active redirect。

输入：`active_redirect`。

输出/副作用：清被覆盖 uid 状态；清被覆盖 candidate；解除 flush/freeze。

文字伪代码：

```text
apply_redirect_flush 的执行流程：
  调用 apply_redirect_flush_range 清理被 redirect 覆盖的 uid 状态和 active map，并回退 admission progress。
  调用 clear_ptw_wait_replay_by_redirect，删除被 redirect 覆盖的 PTW wait replay 项。
  调用 drop_candidates_covered_by_redirect，删除被 redirect 覆盖的 pending recovery candidate。
  调用 clear_redirect_drive_queue，清 redirect drive pending/inflight 状态。
  清 active_redirect、flush_in_progress、issue_freeze_ack 和 dispatch_flush_in_progress。
  清理完成后，admission/route/issue/commit 可以通过 issue_blocked_by_global_flush 恢复正常。
```

### 2.15 修改 `prepare_uid_for_redirect_reissue()`

功能：保证 redirect reissue 清理旧动态实例状态完整，包含 fault/pass/replay/issue/LSQ/candidate 相关痕迹。

输入：`uid`、`redirect`。

输出/副作用：删除 active map / issue queue；回退 LSQ cancel count；清状态；设置 redirect_pending/flushed；`dynamic_epoch++`。

文字伪代码：

```text
prepare_uid_for_redirect_reissue 的执行流程：
  如果 uid 已经 success，说明 success 不回滚假设被破坏，触发 fatal。
  如果 uid 当前 active，调用 retire_active_uid 删除 active ROB/LQ/SQ map，并清 issue queue 中该 uid 的 item。
  如果 uid 不 active 但可能还有 issue queue 残留，调用 remove_uid_from_issue_queues 清理 LOAD/STA/STD queue item。
  根据该 uid 原有 LQ/SQ mapping 累加 pending_lq_cancel_count/pending_sq_cancel_count，供 LSQ 软件镜像后续恢复资源。
  调用 clear_uid_dispatch_result，清 enq、issue、writeback、pass、fault、replay、commit、deq、success 等旧动态状态。
  设置 redirect_pending 和 flushed，标记该 uid 需要重新 admission/reissue。
  递增 dynamic_epoch，记录该 uid 已进入新的动态实例版本。
```

### 2.16 修改 `try_retire_committed_uid()` / success 写入入口

功能：防止存在 pending candidate 的 uid 被置 success。

输入：准备 retire 的 uid。

输出/副作用：有 candidate 时 fatal/error 并拒绝 success；无 candidate 时按原逻辑 retire。

文字伪代码：

```text
try_retire_committed_uid 的执行流程：
  在写 success 前调用 has_recovery_candidate_for_uid(uid)。
  如果该 uid 有 pending candidate，说明 commit frontier recovery 被绕过，触发 fatal/error 并返回。
  如果没有 pending candidate，继续原 success/retire 流程。
  原流程会检查 rob_commit、LQ/SQ mapping 是否释放、fault/replay/redirect/flushed 等状态。
  检查通过后写 success，并 retire active uid。
```

### 2.17 修改 `end_test_check()`，不修改 `transaction_done()` 完成语义

功能：`transaction_done()` 仍只判断 success prefix 是否覆盖全部 uid；recovery idle 只作为 `end_test_check()` 的结束一致性检查。

输入：success prefix、candidate queue、exception queue、PTW wait queue、active redirect 状态。

输出/副作用：`transaction_done()` 不因为 recovery 队列改变返回语义；`end_test_check()` 对结束时的 recovery 残留报错。

文字伪代码：

```text
transaction_done 的执行流程：
  调用 advance_success_prefix 更新连续 success 前缀。
  如果 success_prefix_uid 已经覆盖 main_trans_num，返回完成。
  不检查 recovery_candidate_q、exception_event_q、ptw_wait_replay_q、active_redirect、pending_redirect_drive 或 flush_in_progress。

end_test_check 的执行流程：
  testcase 结束时先确认 success_prefix_uid 已覆盖 main_trans_num。
  然后检查 recovery_candidate_q 是否为空，确认没有 pending redirect/replay candidate。
  然后检查 exception_event_q 和 ptw_wait_replay_q 是否为空，确认没有未消费 fault/replay 等待项。
  然后检查 active_redirect、pending_redirect_drive、flush_in_progress 是否全部 idle。
  任一队列或状态未清空都要报错，防止测试提前退出。
```

### 2.18 可选新增 `check_uid_rob_order()`

功能：检查第一版方案依赖的 uid 顺序等价 ROB commit 顺序。

输入：主表中每个 uid 对应 ROB key。

输出/副作用：检查失败 fatal/error；检查通过无副作用。

文字伪代码：

```text
check_uid_rob_order 的执行流程：
  从 uid0 的 ROB key 开始作为 prev。
  按 uid 从小到大遍历主表。
  每次读取当前 uid 的 ROB key，并用 rob order helper 判断当前 ROB 是否在 prev 之后。
  如果发现 uid 顺序和 ROB 顺序不一致，触发 fatal/error，因为 commit cursor 不能再用 uid 顺序代表 ROB commit frontier。
  检查通过后只说明第一版前提成立，不修改状态表或队列。
```

## 3. 可优化点、潜在问题和解决方案

### 3.1 Fault 是否也纳入 Recovery Candidate

当前最终 flow 第一版保持原 plan 策略：fault 不进入 `recovery_candidate_q`，仍可先落 fault/exception 状态。

潜在问题：

```text
uid=10 fault 先落表；
uid=5 redirect 后到并覆盖 uid=10；
uid=10 fault 必须被 redirect reissue 清理。
```

解决方案：

- 第一版要求 `prepare_uid_for_redirect_reissue()` / `clear_uid_dispatch_result()` 完整清 fault、exception_pending、exception_vec、writeback、pass。
- 如果实现后发现 fault 会阻塞 commit cursor，导致 older redirect candidate 到不了 frontier，则需要把 fault 也迁入 candidate queue，或在 commit frontier 对 fault 增加专门处理。

### 3.2 Commit Handler Owner 可能存在多个实例

潜在问题：

```text
memblock_lsqcommit_dispatch_sequence 有实际 drive LSQ commit 的 commit_handler；
memblock_dispatch_base_sequence 可能还有 monitor_commit_handler。
```

如果 candidate 检查写在非真实 drive owner 的 handler 中，commit cursor 状态可能不一致。

解决方案：

- candidate 检查必须放在实际 `memblock_lsqcommit_dispatch_sequence` 使用的 commit handler 路径。
- 如果保留多个 handler，commit cursor 状态应迁到 `common_data_transaction` 统一管理，避免 object 私有 cursor 分叉。

### 3.3 `recovery_frontier_uid` 容易被误用成全局 Hold 边界

潜在问题：

开发时可能在 admission、route、issue eligibility 中加入 `uid >= recovery_frontier_uid` 阻塞，破坏“不阻塞正常流动”的设计目标。

解决方案：

- `recovery_frontier_uid/recovery_frontier_rob_key` 仅允许用于 debug、日志、no-progress 诊断。
- 高频路径只允许在 commit cursor 当前 uid 调 `has_recovery_candidate_for_uid(uid)`。
- review 时全局搜索 `recovery_frontier_uid` 使用点，确认没有 eligibility/hold 逻辑。

### 3.4 Redirect Reissue 后迟到 Writeback 仍需前置状态检查兜底

潜在问题：

如果旧流水 writeback 在 uid 重新 admission 后才到，active map 可能重新命中新动态实例。

解决方案：

- `check_target_writeback_pre_state()` 必须检查 target dispatched、target 未 done、issue_epoch、replay_seq、redirect_pending/flushed/issue_killed。
- 所有 normal pass/fault 状态写入口必须复用该 helper。
- coding 后必须用 review 检查点确认失败路径没有任何状态写入。

### 3.5 Success Prefix 不回滚是假设，不是恢复机制

潜在问题：

如果 candidate uid 已经 success 后再执行 redirect/replay，会破坏第一版 success 不回滚模型。

解决方案：

- commit cursor 到 uid 时先查 candidate，再查 `uid_is_commit_candidate()`。
- `try_retire_committed_uid()` 和公共 success 写入口必须防御 `has_recovery_candidate_for_uid(uid)`。
- redirect flush 命中 success uid 时报错，不尝试回滚。

### 3.6 Active Redirect 推进不能挂在 LSQ Commit Loop 后续路径

潜在问题：

`request_redirect_flush()` 后 global flush 可能让 LSQ commit loop 只发 idle 并 return；如果 apply flush 依赖 commit loop，就会卡住。

解决方案：

- `exception_redirect_replay_task()` 每个 service cycle 调用 `advance_active_redirect()`。
- LSQ commit flow 只负责启动 candidate recovery。
- drive done 和 apply flush 的 owner 是 service/recovery handler。

### 3.7 PTW Wait Replay 不能绕过 Frontier

潜在问题：

如果 monitor 采到 replay 时立即 `push_ptw_wait_replay()`，则 PTW wait replay 会绕过 commit frontier，提前影响状态。

解决方案：

- monitor 采到 replay 只允许 `push_recovery_candidate()`。
- `push_ptw_wait_replay()` 只能由 `handle_sta_replay_candidate()` 在 candidate 被 frontier 选中后调用。

### 3.8 需要补充 Directed 验证场景

建议最小 directed 场景：

1. younger redirect 先到，older redirect 后到，只执行 older redirect，younger candidate 被 drop。
2. younger STA replay 先到，older redirect 后到，STA replay 不提前 mark replay pending。
3. candidate 等待期间，LSQ admission、issue route、issue fire、ROB commit/LQ/SQ deq 继续运行。
4. older redirect apply flush 清理覆盖范围内未 success uid 的 status、issue queue、LSQ 镜像、candidate queue。
5. 构造 candidate uid 尝试先 success 的路径，应触发 fatal/error。
6. 构造 stale writeback，检查写回前置状态失败时只 warning/drop，不改 pass/fault/writeback 状态。
