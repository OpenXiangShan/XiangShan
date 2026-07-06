# common_data_transaction.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`

## 1. 文件定位与使用场景

全框架唯一公共数据 owner，单例入口为 `common_data_transaction::get()`。它存在的原因是 dispatch 框架是多 sequence 并行运行：LSQ sequence、issue sequence、L2TLB sequence、monitor service loop 和 commit sequence 必须看到同一份状态。这个 class 就是共享状态和一致性检查的中心。

输入来自所有阶段：主表生成写入 `main_table_by_uid`，LSQ 入队写入 active 映射，L2TLB responder 按 req 写入 by-key TLB 表，issue scheduler 写入 issue queue，monitor adapter 写入事件，commit handler 释放映射。输出给所有 helper 查询和更新。

控制逻辑字段是 `status_by_uid[]`、三类 issue queue、`exception_event_q`、active ROB/LQ/SQ 映射、redirect drive 队列、flushSb 等待状态、PTW-back replay 等待队列、`dispatch_progress`、`pending_lq_cancel_count/pending_sq_cancel_count`、`flush_in_progress/active_redirect/global_issue_epoch/main_table_ready`。`main_table_by_uid[]` 保存 transaction payload；`tlb_entry_by_key[]` 保存 live TLB cache，`uid_tlb_record_by_uid[]` 保存每个 uid 的发射上下文和 PTE 回填历史。

关键字段：

- 表：`main_table_by_uid[]`、`status_by_uid[]`、`tlb_entry_by_key[]`、`uid_tlb_record_by_uid[]`。
- 运行时 CSR：`mmu_csr_state`。
- 发射队列：`load_issue_q[$]`、`sta_issue_q[$]`、`std_issue_q[$]`。
- 事件队列：`exception_event_q[$]`。
- redirect 回灌：`pending_redirect_drive_q[$]`、`redirect_phase`、`redirect_drive_inflight`。
- 可选等待：`ptw_wait_replay_q[$]`、`flushsb_scheduled_pending/flushsb_pending/flushsb_waiting_empty/last_sb_is_empty`。
- active 映射：`uid_by_active_rob`、`uid_by_lq`、`uid_by_sq`。
- 公共进度：`dispatch_progress.terminal_done_uid/max_enqueued_uid/max_enqueued_uid_valid`，用于 10 万笔场景下限制 admission/route/redirect 扫描窗口。
- redirect 回退：`pending_lq_cancel_count/pending_sq_cancel_count`，记录被 redirect flush 的 LSQ 分配数量，由 LSQ admission sequence 在恢复后回退软件 LSQ 镜像。
- TLB lookup：`tlb_entry_by_key` 按 `{vpn,asid,vmid,s2xlate}` 命中；uid 追踪只查 `uid_tlb_record_by_uid`。
- 全局控制：`flush_in_progress`、`active_redirect`、`global_issue_epoch`、`issue_freeze_ack`、`main_table_ready`。

主要 API：

- `reset_all_tables(main_trans_num_i)`：重置本轮表、队列、active 映射、TLB lookup、raw monitor queue 和 CSR runtime state，并创建 `status_by_uid[]`。
- `alloc_uid()`：连续分配 uid，越界 fatal。
- `set_main_transaction(uid,tr)`、`get_main_transaction(uid)`：写/读主表，禁止覆盖 active uid。
- `init_status_for_uid(uid)`、`get_status(uid)`：创建/读取状态表。
- `set_status_field(uid,field,value)`、`get_status_field(uid,field)`：统一状态字段访问；`active` 必须通过 `activate_uid/retire_active_uid` 改。`MEMBLOCK_STATUS_ENQ=1` 会调用 `mark_uid_enqueued()` 推进公共 admission 高水位；`MEMBLOCK_STATUS_SUCCESS=1` 只记录 normal pass 结果；`MEMBLOCK_STATUS_TERMINAL_DONE=1` 会调用 `advance_terminal_done_uid()`。
- `mark_uid_enqueued()`、`rollback_max_enqueued_uid()`、`advance_terminal_done_uid()`、`get_active_scan_begin_uid()`、`get_active_scan_end_uid()`、`get_next_new_admit_uid()`：公共 progress helper。`max_enqueued_uid` 表示当前连续有效 LSQ admission 高水位，redirect 后允许回退。
- `conditional_set_target_status_field()`：按 active、target 级 issue epoch 和 target-aware replay seq 过滤迟到事件；LOAD/STA 严格匹配 `replay_seq`，STD 只依赖 issue epoch 等状态保护。
- `alloc_issue_epoch()`、`mark_issue_snapshot(uid,target,issue_epoch)`：发射时分配 epoch 并记录到对应 target。
- `mark_target_normal_pass()`：目标 pass 后置 target writeback/pass；所有 required target 完成后置全局 `writeback/pass`。若 uid 正在 replay，只阻塞正在 replay 的 target，STA replay 不会阻塞同 uid 已发射 STD 的 pass。
- `mark_target_fault()`：记录 fault 和 exception pending。
- `mark_issue_feedback_success()`：记录 IssueQueue feedback success，只设置 `*_issue_feedback_success`，不设置 target writeback/pass。
- `mark_replay_pending()`：只清理 LOAD/STA replay 对应 target 的 issue queue 项和已发射/pass 状态，置 replay pending 和 target mask，并 bump `replay_seq`；STD replay request 只 warning 后忽略。
- `clear_replay_target_after_fire()`：replay target 重新发射后清 mask；所有 mask 清完后清 replay pending。
- `request_redirect_flush()`、`push_redirect_drive()`、`try_pop_redirect_drive()`、`mark_redirect_drive_done()`、`apply_redirect_flush()`：分阶段处理 redirect，先冻结 admission/issue/commit，再等待 redirect sequence 真实驱动 DUT，最后调用 `apply_redirect_flush_range()` 在 `[terminal_done_uid, max_enqueued_uid + 1)` 内按 `rob_order_util::rob_need_flush()` 判断 flush。被 flush 的 uid 不再作为终态，而是进入 `redirect_pending/flushed` 中间态，等待同 uid reissue。
- `prepare_uid_for_redirect_reissue(uid, redirect)`：清理旧动态实例的 issue queue、active map、writeback/pass/fault/commit/deq 状态，置 `redirect_pending/flushed` 并递增 `dynamic_epoch`。主表 transaction 保留，后续 admission 沿用主表 ROB/LQ/SQ key。
- `issue_blocked_by_global_flush()`：统一判断 admission、issue、commit 是否应因 redirect/flush 暂停。
- `arm_scheduled_flushsb()`、`request_scheduled_flushsb_if_due()`、`scheduled_flushsb_pending()`、`request_flushsb()`、`mark_flushsb_driven()`、`update_sb_is_empty()`、`flushsb_timed_out()`：directed flushSb/sbIsEmpty 等待状态管理。`request_flushsb()` 只在 `MEMBLOCK_FLUSHSB_SEQ_EN=1` 时接受请求；scheduled 状态放在公共数据层，避免 real smoke 在未来 request cycle 到达前提前收尾。若 due cycle 遇到 redirect/global flush，scheduled 状态保持 pending，等 flush 解除后再驱动。
- `push_ptw_wait_replay()`、`pop_ready_ptw_wait_replay()`、`clear_ptw_wait_replay_by_redirect()`：可选 PTW-back replay 等待队列管理。
- `make_empty_wb_event()`、`normalize_feedback_event()`、`push_feedback_event()`、`pop_feedback_event()`：统一 writeback event 规范化和排队。
- `resolve_uid_for_event()`：用 uid/ROB/LQ/SQ 交叉解析 active uid，不一致 fatal。
- `activate_uid(uid,map_lq,map_sq)`：建立 active ROB 映射，并按行为建立 LQ/SQ active 映射。
- `retire_active_uid(uid)`：释放 active ROB/LQ/SQ 映射，保留历史表项。
- `release_uid_lq_mapping()`、`release_uid_sq_mapping()`：DUT deq 后释放单侧 LSQ 映射，更新 `lsq_deq`。
- `try_retire_committed_uid(uid)`：ROB commit 且 LQ/SQ 都释放时进入最终 retire；normal pass 设置 `success=1 && terminal_done=1`，fault/exception 设置 `success=0 && terminal_done=1`。
- `make_tlb_key_by_req()`、`get_or_create_tlb_entry_by_req()`、`update_uid_tlb_records_by_entry()`、`get_uid_tlb_record()`：TLB by-key 建/查和 uid record 回填。
- `apply_raw_csr_runtime(raw, raw_csr_seq)`：由 raw CSR monitor 更新 runtime CSR；用 `last_applied_raw_csr_seq` 过滤重复 snapshot，不因 `update_seq` 变化清 TLB entry 或 uid record。
- `decode_raw_sfence()`、`sfence_vpn_match()`、`sfence_match_entry()`、`apply_sfence_invalidate()`、`apply_raw_sfence()`：把 fence monitor 采到的 raw sfence/hfence 转成内部 payload，按地址、ASID/VMID、`g` 位和 entry level 匹配 live `tlb_entry_by_key`，只删除命中的 live entry cache，不删除 `uid_tlb_record_by_uid` 和主表历史。
- `last_applied_raw_csr_seq`：记录已经应用到 `mmu_csr_state` 的 latest CSR seq，避免多个 drain 调用方重复应用同一条 CSR changed snapshot。
- `push_issue_queue_item()`、`delete_issue_queue_entry()`、`remove_uid_from_issue_queues()`、`clear_issue_queues()`：发射队列管理。
- `check_main_table_complete()`：确认 uid 分配和主表/status 完整，并置 `main_table_ready=1`。
- `end_test_check()`：关闭 monitor capture，清残留 raw queue，检查 active/pending/queue/flush 状态为空。

## 2. 字段与函数/task 设计原理

`common_data_transaction` 是整个公共测试框架最重要的 class。它是单例 owner，负责所有跨 sequence 共享数据。它的设计原则是：任何状态变化都走公共 API，sequence 不直接改别人的局部变量。

关键字段：

| 字段 | 含义 | 设计原理 |
|---|---|---|
| `m_inst` | 单例句柄 | 所有 sequence/helper 通过 `get()` 获取同一份公共数据。 |
| `main_trans_num`、`next_uid`、`main_table_ready` | 主表生命周期控制 | `next_uid` 保证 uid 连续分配；`main_table_ready` 阻止 real sequence 在主表未建完时抢跑。 |
| `main_table_by_uid[]` | 主控制表 | 保存静态 transaction，commit 后不立即删除，便于追溯。 |
| `status_by_uid[]` | 状态表 | 保存运行时状态，commit 后可 inactive，但记录保留到 testcase 结束。 |
| `tlb_entry_by_key[]` | live TLB entry cache | L2TLB responder 按 `{vpn,asid,vmid,s2xlate}` 直接查/建 entry，同 key 复用同一条映射。 |
| `uid_tlb_record_by_uid[]` | uid 追踪记录 | uid 发射后保存 request 预期上下文；L2TLB entry 确定后回填 PTE 关键字段，PTW-back replay 等待项以 `pte_valid` 判断 ready。 |
| `mmu_csr_state` | runtime CSR 快照 | TLB lookup 使用 DUT 当前 CSR 语境。 |
| `load_issue_q/sta_issue_q/std_issue_q` | 三个发射队列 | 队列元素是轻量 issue item，不复制完整 transaction；需要字段时通过 uid 回主表/状态表查。 |
| `exception_event_q` | 异常、replay、redirect 事件队列 | writeback handler 先分类，复杂事件交给 recovery handler 统一处理。 |
| `uid_by_active_rob`、`uid_by_lq`、`uid_by_sq` | DUT index 到 uid 的 active map | monitor 事件常只带 ROB/LQ/SQ，必须通过 active map 反查 uid。 |
| `tlb_entry_by_key` | TLB lookup key 到 entry 的索引 | L2TLB request 根据 vpn/s2xlate/CSR 查表；miss 时自动创建 entry，不回退到 per-uid 表。 |
| `dispatch_progress` | 公共扫描边界 | `terminal_done_uid` 只跨过连续 terminal_done uid；`success` 只表示 normal pass 结果；`max_enqueued_uid` 是当前连续有效 admission 高水位，redirect 后回退到最老 flush uid 前一项。 |
| `pending_lq_cancel_count/pending_sq_cancel_count` | redirect 后 LSQ 软件镜像回退计数 | `prepare_uid_for_redirect_reissue()` 累加被 flush uid 占用的 LQ/SQ 数量，LSQ admission sequence 恢复后调用 `cancel_lq/cancel_sq` 消费，确保重入队 key 预测与 DUT 一致。 |
| `flush_in_progress`、`active_redirect`、`issue_freeze_ack` | redirect/flush 全局控制 | flush 时 admission、issue、commit 停止选新项，避免旧 uid 与 redirect 后状态冲突。 |
| `pending_redirect_drive_q`、`redirect_phase`、`redirect_drive_inflight` | redirect 回灌状态 | recovery handler 只负责入队和冻结，redirect sequence 负责真实驱动 `io.redirect`。drive done 后才允许 apply flush；payload 保留 `rob_key/flush_itself/level`。 |
| `flushsb_scheduled_pending/flushsb_pending/flushsb_waiting_empty/last_sb_is_empty` | directed flushSb 状态 | `MEMBLOCK_FLUSHSB_SEQ_EN=1` 且 directed flow 请求后才发 `flushSb`，随后等待 ctrl monitor 采到 `sbIsEmpty=1`。`MEMBLOCK_FLUSHSB_REQUEST_CYCLE` 会先登记为 scheduled pending，real smoke 看到该状态时不会提前结束；若 due cycle 被 global flush 挡住，pending 不清除，后续 cycle 继续尝试。timeout 使用 dispatch service-cycle。 |
| `ptw_wait_replay_q` | 可选 PTW-back replay 等待队列 | `MEMBLOCK_REPLAY_WAIT_PTW_EN=1` 时，STA `hit=0 && flush_state=1` 可先等待 L2TLB response done 或 timeout，再重新进入 replay route。 |
| `global_issue_epoch` | 发射时代计数器 | 每次 issue fire 分配新 epoch，writeback 条件更新使用它过滤迟到事件。 |

主要函数/task按职责分组：

| 职责 | 函数/task | 参数 | 功能和设计原理 |
|---|---|---|---|
| 单例和重置 | `get()`、`reset_all_tables(main_trans_num_i)` | 表项数 | `get()` 保证唯一 owner；`reset_all_tables()` 分配主表/状态表/TLB 表，清队列、active map、raw monitor event 和 CSR runtime。 |
| uid 和表访问 | `alloc_uid()`、`is_valid_uid(uid)`、`check_uid(uid, caller)`、`set_main_transaction(uid,tr)`、`get_main_transaction(uid)` | uid、调用者、transaction | uid 连续分配，所有数组访问先做边界检查；禁止覆盖 active uid，避免同一 uid 生命周期重叠。 |
| 状态表访问 | `ensure_status_exists(uid,caller)`、`init_status_for_uid(uid)`、`get_status(uid)` | uid | 确保每个 uid 有状态对象；初始化时从主表快照 ROB/LQ/SQ key。 |
| 状态字段更新 | `set_status_field(uid,field,value)`、`get_status_field(uid,field)` | uid、字段枚举、值 | 对外提供字段级 API，集中限制哪些字段可被普通调用方修改，例如 `active` 不能直接写。 |
| 条件更新 | `conditional_set_target_status_field(...)`、`target_dispatched(...)`、`target_replay_seq_match(...)` | uid、字段、值、target、epoch、replay_seq | 只有 active、未 killed、对应 target 已 dispatched、target 级 epoch 匹配，且 LOAD/STA replay_seq 匹配时才更新。STD 不用 replay_seq 失效旧事件，但必须至少满足 `std_dispatched` 和当前 target epoch 保护，避免还没发射的 STD 被反馈误置 pass/fault。 |
| issue 快照 | `alloc_issue_epoch()`、`mark_issue_snapshot(uid,target,issue_epoch)` | uid、target、epoch | 发射成功时把 epoch 写入对应 target，后续 event 必须匹配。 |
| target 状态映射 | `target_writeback_field(target)`、`target_pass_field(target)`、`target_fault_field(target)` | LOAD/STA/STD | 把 target 映射到状态表字段，避免各 handler 散落重复 case。 |
| 完成判断 | `target_entry_done(status,target)`、`required_targets_done(uid)` | 状态、target、uid | load 只要求 LOAD 完成，store 要 STA+STD 都完成；commit 和 pass 用同一套完成标准。 |
| pass/fault/issue feedback 标记 | `mark_target_normal_pass(uid,target,issue_epoch,replay_seq,cycle)`、`mark_target_fault(...)`、`mark_issue_feedback_success(...)` | uid、target、快照、时间、异常向量 | 正常 pass 不能覆盖 fault/redirect，也不能覆盖正在 replay 的同 target；更新前还必须确认该 target 已经真实发射。`mark_issue_feedback_success()` 使用同样的 active/epoch/replay_seq 保护，但只说明 IssueQueue finalSuccess，不设置 target writeback/pass。fault 会清 pass 并置 `exception_pending`。 |
| replay | `mark_replay_pending(...)`、`set_replay_target_mask(...)`、`replay_target_requested(...)`、`replay_targets_empty(...)`、`clear_replay_target_after_fire(...)`、`bump_replay_seq(uid)` | uid、target、mask、快照 | `replay_valid=1` 的 LOAD/STA replay 事件只重发被标记 target；STA replay 不清同 uid 的 STD pending 项，也不要求 STD 跟随新的 replay_seq。香山源码中 STA 有 `staIqFeedback`/RS feedback replay，STD 没有 MemBlock 执行后返回后端的 replay feedback 通路，因此 STD replay request 不建 `replay_pending`。 |
| 公共进度 | `mark_uid_enqueued(uid)`、`rollback_max_enqueued_uid(uid)`、`advance_terminal_done_uid()`、`get_active_scan_begin_uid()`、`get_active_scan_end_uid()`、`get_next_new_admit_uid()` | uid | 集中维护 terminal_done 前缀和 admission 高水位。route、redirect flush、LSQ admission 都从这里取边界，不再各自维护本地游标。 |
| redirect/flush | `request_redirect_flush(redirect)`、`push_redirect_drive(payload)`、`try_pop_redirect_drive(payload)`、`mark_redirect_drive_done(payload)`、`apply_redirect_flush(redirect)`、`apply_redirect_flush_range(redirect)`、`prepare_uid_for_redirect_reissue(uid,redirect)`、`clear_uid_dispatch_result(uid)` | redirect payload、uid | 请求 flush 时冻结 admission/issue/commit；redirect sequence 真实驱动 `io.redirect` 后，只扫描公共活跃窗口，用 ROB 年龄判断 flush。命中 uid 清旧动态实例并进入 redirect reissue，`flushed` 不再是 terminal success。 |
| feedback event | `make_empty_wb_event()`、`normalize_feedback_event(...)`、`push_feedback_event(wb_event)`、`pop_feedback_event(wb_event)`、`clear_feedback_events()` | event | 统一 event 默认值、uid 反查、epoch/replay_seq 补齐和入队。有效 action 包含 redirect/replay/fault、`real_wb_valid`、`iq_feedback_valid` 和旧兼容 `real_wb_valid`。replay 后 LOAD/STA event 必须带 issue_epoch/replay_seq 快照；STD raw feedback 若没有 issue_epoch，只能在 `std_dispatched=1` 后补当前 `std_issue_epoch`，否则直接丢弃。复杂事件统一给 recovery handler。 |
| event 分类 | `feedback_event_is_redirect/replay/has_fault/has_action()`、`feedback_event_target_is_valid(target)` | event/target | 保证 writeback、replay、redirect 优先级判断一致；redirect 分类以 canonical `redirect.valid` 为准，`redirect_valid` 必须与其一致，不靠 source 单独触发行为。 |
| event 反查 | `resolve_uid_for_event(wb_event, uid)`、`get_event_issue_epoch(...)`、`get_event_replay_seq(...)` | event、uid | 支持通过 uid/ROB/LQ/SQ 多路径反查，并检查多路径是否指向同一 uid。 |
| active map | `activate_uid(uid,map_lq,map_sq)`、`activate_uid_by_behavior(uid,behavior)`、`lookup_active_uid_by_rob/lq/sq(...)`、`get_active_uid_by_rob(...)`、`retire_active_uid(uid)` | uid、key、behavior | 入队后建立 active 映射；commit/deq/flush 后释放映射。ROB/LQ/SQ 重复 active 会 fatal。 |
| LSQ 释放 | `release_uid_lq_mapping(uid)`、`release_uid_sq_mapping(uid)`、`try_retire_committed_uid(uid)` | uid | DUT deq 后释放 LQ/SQ map；ROB 已 commit 且 LQ/SQ 都释放后才 retire active uid。 |
| TLB 表 | `make_tlb_key_by_req(vpn,s2xlate)`、`get_or_create_tlb_entry_by_req(...)`、`build_tlb_entry_for_key(key)`、`get_tlb_entry(key)`、`insert_tlb_entry(key,entry)`、`register_uid_tlb_record_on_issue(uid)`、`update_uid_tlb_records_by_entry(key,entry)`、`get_uid_tlb_record(uid)`、`tlb_entry_ready_for_uid(uid)` | uid、TLB key、vpn/s2xlate | live TLB entry 按 key 唯一存储；miss 时由 `build_tlb_entry_for_key()` 调用 `tlb_map_builder` 生成 entry，保证 paddr/PTE 随机逻辑只有一处；uid record 只保存发射上下文和 PTE 回填历史，不参与 entry 命中仲裁。CSR `update_seq` 不再清索引或拒绝命中。 |
| runtime CSR | `apply_raw_csr_runtime(raw, raw_csr_seq)` | latest CSR snapshot 和 seq | 更新 runtime CSR；同一 seq 只应用一次，不因 `update_seq` 变化清 TLB entry 或 uid record。进程/阶段切换由 lookup key 中的 ASID/VMID/S2xlate 自然区分，真实失效由后续 `sfence/hfence` entry 级逻辑处理。 |
| sfence/hfence 失效 | `decode_raw_sfence(raw)`、`sfence_vpn_match(entry_vpn,entry_level,addr)`、`sfence_match_entry(payload,key,entry)`、`apply_sfence_invalidate(payload)`、`apply_raw_sfence(raw)` | raw sfence、payload、TLB key/entry | fence monitor 只采原始接口字段，不直接删表；公共数据层统一 decode `rs1/rs2/hv/hg`，再遍历 `tlb_entry_by_key` 做 entry 级删除。普通 sfence 按 ASID 语义，ASID 精确 flush 不删 `pte_g=1` entry；`hfence_v` 处理虚拟 ASID/当前 VMID 语义；`hfence_g` 按 VMID 失效并忽略 ASID。删除范围只限 live TLB entry cache，uid record 保留用于后续 debug 和已发射 uid 的 PTE 历史追踪。 |
| issue queue | `clear_issue_queues()`、`issue_queue_contains(target,uid,replay_seq)`、`push_issue_queue_item(item)`、`delete_issue_queue_entry(...)`、`remove_uid_from_issue_queues(uid)`、`get_issue_queue_size(target)` | target、uid、item、replay_seq | 队列只保存轻量 item；发射成功或 replay/flush 时删除队列项，追溯仍靠主表和状态表。 |
| directed 等待 | `arm_scheduled_flushsb()`、`request_scheduled_flushsb_if_due()`、`request_flushsb()`、`update_sb_is_empty()`、`push_ptw_wait_replay()`、`pop_ready_ptw_wait_replay()` | flushSb / replay wait item | 只管理测试框架等待状态，不模拟 MemBlock 内部 LoadQueueReplay、SBuffer 内部状态机或 PTW 下游行为；等待时间统一按 dispatch service-cycle 计数。 |
| 收尾检查 | `check_main_table_complete()`、`end_test_check()` | 无 | 主表生成结束后置 `main_table_ready`；testcase 结束时检查 active map、队列、flush、pending event 都已清空。 |
