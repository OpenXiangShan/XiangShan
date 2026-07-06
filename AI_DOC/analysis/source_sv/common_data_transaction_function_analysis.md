# common_data_transaction.sv 函数级源码讲解

对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`

本文按源码中的 function 顺序和功能分组说明 `common_data_transaction`。该文件当前只定义 `function`，没有 `task`。

## 1. 文件定位

`common_data_transaction` 是 memblock dispatch 测试框架的公共数据 owner。它不是一个普通 transaction，而是一个单例公共状态仓库，用来让主表生成、LSQ 入队、TLB responder、issue scheduler、writeback/replay/redirect handler、commit sequence 等模块共享同一套运行期数据。

可以把它理解成测试框架里的“公共数据库”：

- 主表：`main_table_by_uid[]`，保存每条 transaction 的静态输入字段。
- 状态表：`status_by_uid[]`，保存每条 transaction 当前处于入队、发射、写回、replay、redirect、commit、retire 的哪个阶段。
- TLB 表：`tlb_table_by_uid[]` 和 `uid_by_tlb_key[]`，服务 L2TLB responder 查表。
- Issue 队列：`load_issue_q/sta_issue_q/std_issue_q`，保存等待发射到 load/STA/STD pipe 的轻量队列项。
- Active 反查表：`uid_by_active_rob/uid_by_lq/uid_by_sq`，把 DUT monitor 回来的 ROB/LQ/SQ key 反查回 uid。
- Feedback 队列：`exception_event_q`，保存需要 replay/redirect/fault handler 继续处理的统一事件。
- Redirect/flushSb/PTW wait 状态：协调 redirect input 回灌、flushSb 等待 sbIsEmpty、PTW-back replay 等跨 sequence 的闭环流程。

整体调用链可以概括为：

```text
memblock_dispatch_base_sequence
  -> reset_all_tables / alloc_uid / set_main_transaction / init_status_for_uid
  -> check_main_table_complete

lsq_ctrl_model / lsqenq sequence
  -> set_main_transaction / activate_uid / set_status_field(ENQ)

tlb_map_builder / l2tlb sequence
  -> set_tlb_transaction / register_tlb_lookup
  -> lookup_tlb_uid_by_req / get_tlb_transaction / mark_tlb_response_done

issue_queue_scheduler / lintsissue sequence
  -> push_issue_queue_item / delete_issue_queue_entry
  -> alloc_issue_epoch / mark_issue_snapshot / clear_replay_target_after_fire

dispatch_monitor_event_adapter / writeback_status_handler
  -> resolve_uid_for_event / normalize_feedback_event
  -> mark_target_normal_pass / mark_target_fault / push_feedback_event

exception_redirect_replay_handler / redirect sequence
  -> pop_feedback_event / mark_replay_pending
  -> request_redirect_flush / push_redirect_drive / try_pop_redirect_drive
  -> mark_redirect_drive_done / apply_redirect_flush

lsq_commit_handler / lsqcommit sequence
  -> release_uid_lq_mapping / release_uid_sq_mapping / try_retire_committed_uid
  -> request_scheduled_flushsb_if_due / should_drive_flushsb / mark_flushsb_driven
```

### 1.1 关键调用链函数功能、输入输出速查

下面按调用链把关键函数的功能、输入输出和链路作用集中列出。后续章节会再按源码顺序展开每个函数的内部逻辑。

#### `memblock_dispatch_base_sequence` 主表构建链路

这条链路负责从“还没有公共表”进入“主表、状态表、TLB 数组和运行期队列都已初始化完成”的状态。它是整个 dispatch 测试框架的起点。

| 函数 | 功能 | 输入 | 输出 | 在调用关系中的作用 |
|---|---|---|---|---|
| `reset_all_tables(main_trans_num_i)` | 按 transaction 数量重置公共数据表、状态表、TLB 表、issue queue、feedback queue、active map、redirect/flushSb/PTW wait 状态。 | `main_trans_num_i`：本轮主表 transaction 数量。 | 无返回；副作用是重新分配数组并清空运行期状态。 | `build_random_main_table()` 或 `import_manual_main_table()` 的第一步，保证新主表不会继承上一轮残留状态。 |
| `alloc_uid()` | 分配连续递增的 uid。 | 无显式输入，依赖 `next_uid/main_trans_num`。 | 返回 `memblock_uid_t uid`。 | 主表生成循环中为每条 transaction 分配公共主键，后续所有表都按 uid 关联。 |
| `set_main_transaction(uid, tr)` | 把 transaction 写入 `main_table_by_uid[uid]`，并修正 `tr.uid`。 | `uid`、`main_control_transaction tr`。 | 无返回；副作用是更新主表。 | 主表生成阶段保存随机或手动 transaction；LSQ 入队确认前也用它写回 DUT 分配的 LQ/SQ index。 |
| `init_status_for_uid(uid)` | 初始化该 uid 的状态表项，并从主表 snapshot 静态字段。 | `uid`。 | 返回 `status_transaction` 句柄。 | 主表全部生成后建立每条 transaction 的初始状态，后续入队、发射、写回都只更新状态表。 |
| `check_main_table_complete()` | 检查 uid 分配数量、主表和状态表完整性，并置 `main_table_ready=1`。 | 无。 | 无返回；副作用是设置 `main_table_ready`，并可能 arm scheduled flushSb。 | 主表构建结束闸口。下游 LSQ 入队、L2TLB responder、issue sequence 看到 ready 后才能消费公共表。 |

#### `lsq_ctrl_model / lsqenq sequence` LSQ admission 链路

这条链路负责在 DUT 接受 LSQ 入队后，把主表项从“已生成但未进入 DUT”切换成“active，可被 monitor 反查”的状态。

| 函数 | 功能 | 输入 | 输出 | 在调用关系中的作用 |
|---|---|---|---|---|
| `set_main_transaction(uid, tr)` | 将 DUT admission response 中确认的 `lqIdx/sqIdx/numLsElem` 写回主表。 | `uid`、带入队结果的 `main_control_transaction tr`。 | 无返回；更新主表项。 | LSQ 入队成功后，主表中的 LQ/SQ index 必须和 DUT 返回一致，后续 issue 字段和 monitor 反查都依赖这些值。 |
| `activate_uid(uid, map_lq, map_sq)` | 将 uid 标记 active，并建立 ROB 反查表；按需建立 LQ/SQ 反查表。 | `uid`、`map_lq`、`map_sq`。 | 无返回；副作用是更新 `uid_by_active_rob/uid_by_lq/uid_by_sq` 和 `status.active`。 | admission 成功后的关键状态切换。只有 active uid 才允许被 writeback、feedback、deq、redirect 事件命中。 |
| `set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1'b1)` | 设置该 uid 已完成入队。 | `uid`、字段枚举 `MEMBLOCK_STATUS_ENQ`、值 `1'b1`。 | 无返回；更新 `status.enq`。 | issue scheduler 会要求 `active/enq/tlb_mapped` 等状态满足后才允许 route 和发射。 |

#### `tlb_map_builder / l2tlb sequence` TLB 建表和响应链路

这条链路负责把主表地址和 runtime CSR snapshot 变成 L2TLB responder 可查的 TLB 表项，并在真实回包后标记 response done。

| 函数 | 功能 | 输入 | 输出 | 在调用关系中的作用 |
|---|---|---|---|---|
| `set_tlb_transaction(uid, tr)` | 保存 uid 对应的 TLB 表项，并清 `tlb_response_done_by_uid[uid]`。 | `uid`、`tlb_transaction tr`。 | 无返回；更新 `tlb_table_by_uid[uid]`。 | `tlb_map_builder` 生成映射后写入公共 TLB 表，供 L2TLB responder 回填 response。 |
| `register_tlb_lookup(key, uid)` | 建立 `TLB lookup key -> uid` 索引，并检查同 key 映射冲突。 | `memblock_tlb_lookup_key_t key`、`uid`。 | 无返回；更新 `uid_by_tlb_key[key]`。 | L2TLB request 只带 VPN/S2xlate 等信息，sequence 需要通过索引找到对应 uid。 |
| `lookup_tlb_uid_by_req(vpn, s2xlate, uid)` | 根据 DTLB request 的 `vpn/s2xlate` 和当前 runtime CSR snapshot 查 uid。 | `vpn`、`s2xlate`。 | output `uid`，返回 bit 表示是否命中。 | `memblock_l2tlb_base_sequence` 收到 request 后的查表入口，命中后才能取 TLB 表项构造 response。 |
| `get_tlb_transaction(uid)` | 读取 uid 对应 TLB 表项。 | `uid`。 | 返回 `tlb_transaction` 句柄。 | L2TLB responder 命中 uid 后读取 PPN、权限位、异常位等字段回填给 DTLB agent。 |
| `mark_tlb_response_done(uid)` | 标记该 uid 的 L2TLB response 已真实发出。 | `uid`。 | 无返回；设置 `tlb_response_done_by_uid[uid]=1`。 | PTW-back replay wait 依赖这个标志，避免 TLB response 尚未回包时就重新发射 replay。 |

#### `issue_queue_scheduler / lintsissue sequence` 发射队列链路

这条链路负责把 active uid 拆成 load/STA/STD issue queue item，并在 DUT ready/fire 后更新发射快照。

| 函数 | 功能 | 输入 | 输出 | 在调用关系中的作用 |
|---|---|---|---|---|
| `push_issue_queue_item(item)` | 将 issue item 放入 load/STA/STD 队列，重复项会被忽略。 | `memblock_issue_q_item_t item`，包含 uid、target、replay_seq、优先级等。 | 无返回；更新对应 issue queue。 | `issue_queue_scheduler::route_target()` 的最终入队动作，把主表/状态表中的 uid 转换成等待发射的轻量队列项。 |
| `delete_issue_queue_entry(target, uid, replay_seq, match_replay_seq)` | 从指定 target 队列删除匹配项。 | target、uid、replay_seq、是否匹配 replay_seq。 | 无返回；删除队列项。 | 发射成功后删除已发射项；replay 重新入队前也先清旧项，避免重复发射。 |
| `alloc_issue_epoch()` | 分配新的全局 issue epoch。 | 无。 | 返回 `int unsigned issue_epoch`。 | 每次 DUT 接受一个 issue item 时生成新 epoch，用于后续 writeback/replay 事件匹配。 |
| `mark_issue_snapshot(uid, target, issue_epoch)` | 在 status 中记录本次发射 target 级 issue epoch。 | uid、target、issue_epoch。 | 无返回；更新 status 中对应 target 的 issue epoch。 | 把“该 uid 的某个 target 已经被 DUT 接受”记录下来，writeback handler 用它过滤迟到事件。 |
| `clear_replay_target_after_fire(uid, target)` | replay target 重新发射后清除对应 replay mask。 | uid、target。 | 无返回；必要时清 `replay_pending`。 | replay 重新入队后，一旦对应 target 再次发射，就把该 target 从 replay 请求集中移除。 |

#### `dispatch_monitor_event_adapter / writeback_status_handler` 写回事件链路

这条链路负责把 DUT monitor 采到的 raw event 转成带 uid/epoch 的公共事件，并把 normal pass/fault/replay/redirect 分流到对应处理路径。

| 函数 | 功能 | 输入 | 输出 | 在调用关系中的作用 |
|---|---|---|---|---|
| `resolve_uid_for_event(wb_event, uid)` | 根据 event 携带的 uid、ROB key、LQ key、SQ key 反查 active uid，并检查多 key 一致性。 | `memblock_wb_event_t wb_event`。 | output `uid`，返回 bit 表示反查成功。 | monitor raw event 进入状态表前必须先找到 active uid；找不到说明事件迟到或不属于当前 active transaction。 |
| `normalize_feedback_event(wb_event, normalized_event)` | 标准化 event，补齐 uid、ROB key、issue_epoch、replay_seq，并丢弃无效或迟到事件。 | 原始 `wb_event`。 | output `normalized_event`，返回 bit 表示事件有效。 | `writeback_status_handler` 和 feedback queue 的入口，保证后续状态更新拿到的是同一格式事件。 |
| `mark_target_normal_pass(uid, target, issue_epoch, replay_seq, cycle)` | 标记某个 target 正常完成；如果该 transaction 所需 target 都完成，则设置总 writeback/pass。 | uid、target、issue_epoch、replay_seq、cycle。 | 返回 bit 表示是否成功更新。 | normal writeback/IQ feedback 的状态落点，处理 load 单 target 和 store STA/STD 双 target 聚合。 |
| `mark_target_fault(uid, target, issue_epoch, replay_seq, exception_vec, cycle)` | 标记某个 target 发生异常，并设置总 fault/exception pending。 | uid、target、issue_epoch、replay_seq、exception_vec、cycle。 | 返回 bit 表示是否成功更新。 | fault writeback 的状态落点。后续 exception/replay/redirect handler 会继续处理 pending fault。 |
| `push_feedback_event(wb_event)` | 标准化后把需要后续处理的 event 放入 `exception_event_q`。 | `wb_event`。 | 无返回；可能向 queue 入队。 | redirect、replay、fault 不在 writeback handler 内直接完成，先进入统一 feedback queue 等 recovery handler 处理。 |

#### `exception_redirect_replay_handler / redirect sequence` replay 与 redirect 恢复链路

这条链路负责消费 feedback queue 中的 replay/redirect/fault 事件。redirect 会先冻结发射，再通过 redirect sequence 真实驱动 DUT input，最后执行软件状态 flush。

| 函数 | 功能 | 输入 | 输出 | 在调用关系中的作用 |
|---|---|---|---|---|
| `pop_feedback_event(wb_event)` | 从 `exception_event_q` 弹出一个待处理 event。 | 无。 | output `wb_event`，返回 bit 表示是否弹出。 | recovery handler 每拍消费 pending feedback 的入口。 |
| `mark_replay_pending(uid, target, issue_epoch, replay_seq, cycle)` | 标记某 target 需要 backend replay，清掉旧发射结果并 bump replay_seq。 | uid、target、issue_epoch、replay_seq、cycle。 | 返回 bit 表示是否成功标记。 | replay event 的状态落点。标记后 issue scheduler 会重新 route 对应 target。 |
| `request_redirect_flush(redirect)` | 进入 redirect freeze 状态，设置全局 flush 标志、flush epoch、active redirect。 | `memblock_redirect_payload_t redirect`。 | 无返回；更新全局 redirect/flush 状态。 | redirect event 被选中后的第一步，阻止旧上下文继续 issue/commit。 |
| `push_redirect_drive(redirect)` | 将 redirect payload 放入待驱动队列。 | redirect payload。 | 无返回；更新 `pending_redirect_drive_q`。 | 把 recovery handler 选出的 redirect 交给 `memblock_redirect_dispatch_sequence` 去真实 drive DUT input。 |
| `try_pop_redirect_drive(payload)` | 从待驱动队列取出一个 redirect payload，并标记 inflight。 | 无。 | output `payload`，返回 bit 表示是否取到。 | redirect sequence 的取任务入口，同时保证同一时间只有一个 redirect inflight。 |
| `mark_redirect_drive_done(payload)` | redirect sequence 完成 drive 后回写完成状态。 | 已驱动的 payload。 | 无返回；清 inflight，并可能推进 redirect phase。 | 告诉 recovery handler：DUT input 已经收到 redirect，下一拍可以执行软件状态 flush。 |
| `apply_redirect_flush(redirect)` | 按 ROB 范围清理被 redirect flush 的 active uid、issue queue、PTW wait 和 redirect queue，并解除全局 flush。 | redirect payload。 | 无返回；更新状态表、active map 和全局 flush 状态。 | redirect 闭环最后一步，确保 TB 公共状态和 DUT redirect 后的上下文对齐。 |

#### `lsq_commit_handler / lsqcommit sequence` commit、LSQ deq 与 flushSb 链路

这条链路负责处理 DUT 返回的 LQ/SQ deq、ROB commit 推进以及 directed flushSb。

| 函数 | 功能 | 输入 | 输出 | 在调用关系中的作用 |
|---|---|---|---|---|
| `release_uid_lq_mapping(uid)` | 释放 uid 对应 LQ active map；若 LQ/SQ 都已释放则设置 `lsq_deq`。 | uid。 | 无返回；更新 LQ map 和 status。 | DUT `lqDeq` monitor 被 adapter/commit handler 消费后的 LQ 资源释放动作。 |
| `release_uid_sq_mapping(uid)` | 释放 uid 对应 SQ active map；若 LQ/SQ 都已释放则设置 `lsq_deq`。 | uid。 | 无返回；更新 SQ map 和 status。 | DUT `sqDeq` monitor 被消费后的 SQ 资源释放动作。 |
| `try_retire_committed_uid(uid)` | 如果 uid 已 ROB commit 且 LQ/SQ map 都释放，则设置 success 并 retire active uid。 | uid。 | 无返回；可能调用 retire。 | commit 和 LSQ deq 两条异步路径的汇合点，保证两边都完成才清 active。 |
| `request_scheduled_flushsb_if_due(cycle_idx)` | 到达预设 cycle 时发起 flushSb request。 | 当前 cycle index。 | 返回 bit 表示本拍是否触发 request。 | lsqcommit sequence 每拍检查 directed flushSb 是否到期。 |
| `should_drive_flushsb()` | 判断当前是否应真实驱动 flushSb。 | 无。 | 返回 bit。 | flushSb driver 的 gating 条件，避免 redirect/flush 期间或正在等待 empty 时重复驱动。 |
| `mark_flushsb_driven(cycle)` | 标记 flushSb 已驱动，进入等待 `sbIsEmpty` 状态。 | 当前 service cycle。 | 无返回；设置 `flushsb_waiting_empty` 和 sync pkg 标志。 | flushSb drive 后的状态切换，后续由 ctrl monitor 的 `sbIsEmpty` 解除等待。 |

## 2. 生命周期与主表管理

### `new(name)`

源码位置：`common_data_transaction.sv:56`

源码关键逻辑：构造对象，初始化 `main_trans_num/next_uid/main_table_ready`，清空 redirect、issue freeze、flushSb 等全局状态，并创建 `mmu_csr_runtime_state` 后 reset。

功能：建立一个干净的公共数据对象初始状态。这里还没有分配主表数组，主表数组必须等 `reset_all_tables()` 根据 transaction 数量创建。

输入：`name`，UVM object 名称，默认 `"common_data_transaction"`。

输出：无显式返回；构造后的对象内部字段被初始化。

主要调用方：`get()` 第一次创建单例时调用，也可能由 UVM factory 直接创建。

内部依赖：调用 `mmu_csr_runtime_state::type_id::create()` 和 `mmu_csr_state.reset()`，保证 CSR runtime snapshot 一开始是已知状态。

### `get()`

源码位置：`common_data_transaction.sv:82`

源码关键逻辑：如果静态句柄 `m_inst` 为空，就 `new()` 一个；否则直接返回已有句柄。

功能：提供 `common_data_transaction` 单例入口。所有 helper 和 sequence 都通过它拿到同一份公共数据，避免多个 sequence 各自维护状态。

输入：无。

输出：`common_data_transaction` 单例句柄。

主要调用方：`memblock_dispatch_base_sequence::pre_body()`，`lsq_ctrl_model`，`issue_queue_scheduler`，`writeback_status_handler`，`exception_redirect_replay_handler`，`dispatch_monitor_event_adapter`，各 real/soft sequence。

内部依赖：调用 `new()` 创建单例。

### `reset_all_tables(main_trans_num_i)`

源码位置：`common_data_transaction.sv:89`

源码关键逻辑：检查 transaction 数量非 0；设置 `main_trans_num/next_uid/main_table_ready`；清空 flush/redirect/flushSb/PTW wait/issue queue/feedback queue/active map/TLB lookup；打开 monitor capture；按数量创建 `main_table_by_uid/status_by_uid/tlb_table_by_uid/tlb_response_done_by_uid` 数组，并为每个 uid 创建和 reset `status_transaction`。

功能：每次构建新主表前的全局 reset。它相当于测试框架公共状态的“开局清场”，保证上一轮 raw monitor event、active map、issue queue、redirect 状态不会污染本轮 testcase。

输入：`main_trans_num_i`，本轮主表 transaction 数量。

输出：无显式返回；公共表和全局状态全部重置。

主要调用方：`memblock_dispatch_base_sequence::build_random_main_table()`，`memblock_dispatch_base_sequence::import_manual_main_table()`。

内部依赖：调用 `clear_issue_queues()`、`clear_feedback_events()`、`clear_redirect_drive_queue()`、`clear_ptw_wait_replay_queue()`，并调用 `memblock_sync_pkg::clear_raw_monitor_queues()` 清 raw monitor 队列。

### `alloc_uid()`

源码位置：`common_data_transaction.sv:143`

源码关键逻辑：检查已经 reset 且 `next_uid < main_trans_num`，返回当前 `next_uid`，然后自增。

功能：给主表 transaction 分配连续 uid。uid 是本测试框架内部最稳定的主键，后续 ROB/LQ/SQ/TLB lookup 都最终回到 uid。

输入：无。

输出：新分配的 `memblock_uid_t uid`。

主要调用方：`memblock_dispatch_base_sequence::build_random_main_table()` 和 `import_manual_main_table()`。

内部依赖：只依赖 `main_trans_num/next_uid`。

### `set_main_transaction(uid, tr)`

源码位置：`common_data_transaction.sv:176`

源码关键逻辑：检查 uid 合法、transaction 非空、该 uid 尚未 active；把 `tr.uid` 修正为 uid 后写入 `main_table_by_uid[uid]`。

功能：把主表 transaction 放入公共主表。入队后也会再次调用该函数更新 DUT 分配回来的 `lqIdx/sqIdx` 等字段，但要求该 uid 尚未 active 前完成。

输入：`uid` 和 `main_control_transaction tr`。

输出：无显式返回；更新 `main_table_by_uid[uid]`。

主要调用方：`memblock_dispatch_base_sequence` 主表生成；`lsq_ctrl_model::commit_allocate()` 和 `commit_allocate_with_resp()` 在 LSQ admission 成功后写回 LQ/SQ index。

内部依赖：调用 `check_uid()` 做合法性检查。

### `get_main_transaction(uid)`

源码位置：`common_data_transaction.sv:188`

源码关键逻辑：检查 uid 合法且主表项非空，返回 `main_table_by_uid[uid]`。

功能：按 uid 读取主表 transaction。几乎所有后续 helper 都通过它拿静态字段，例如 fuType、fuOpType、ROB、LQ/SQ、地址、send priority。

输入：`uid`。

输出：`main_control_transaction` 句柄。

主要调用方：`required_targets_done()`、`activate_uid()`、`issue_queue_scheduler::route_uid()`、`issue_field_assigner`、`tlb_map_builder`、real smoke sequence 等。

内部依赖：调用 `check_uid()`。

### `check_main_table_complete()`

源码位置：`common_data_transaction.sv:1613`

源码关键逻辑：检查已经 reset，检查 `next_uid == main_trans_num`，逐 uid 检查主表和状态表非空；根据 `seq_csr_common::get_flushsb_request_cycle()` 预置 flushSb 定时请求；最后置 `main_table_ready=1`。

功能：主表构建完成的验收闸口。它让后续 LSQ 入队、TLB responder、issue sequence 知道主表已经可消费。

输入：无。

输出：无显式返回；设置 `main_table_ready`，并可能 arm 定时 flushSb。

主要调用方：`memblock_dispatch_base_sequence::build_random_main_table()` 和 `import_manual_main_table()`。

内部依赖：调用 `arm_scheduled_flushsb()`。

### `end_test_check()`

源码位置：`common_data_transaction.sv:1634`

源码关键逻辑：关闭 monitor capture；清理剩余 raw monitor event；检查 uid 分配完成、每个 status 不再 active、不再有 exception/replay/redirect pending；检查 active map、issue queue、flush/redirect、flushSb、PTW wait 队列均为空。

功能：test 收尾一致性检查。它不是功能驱动逻辑，而是保证测试框架在结束时没有遗漏 transaction、迟到事件或悬挂状态。

输入：无。

输出：无显式返回；不满足条件时 `uvm_fatal`。

主要调用方：`memblock_dispatch_real_smoke_sequence`、`memblock_dispatch_real_mixed_smoke_sequence`、`soft_test_memblock_dispatch_replay_smoke_sequence`。

内部依赖：调用 `memblock_sync_pkg::raw_monitor_queue_size()`、`clear_raw_monitor_queues()`、`has_pending_redirect_drive()`。

## 3. 合法性检查与状态表基础访问

### `is_valid_uid(uid)`

源码位置：`common_data_transaction.sv:158`

功能：判断 uid 是否落在本轮主表范围内。

输入：`uid`。

输出：bit，`main_trans_num != 0 && uid < main_trans_num` 时为 1。

主要调用方：`check_uid()`，active map lookup，issue field assigner，real smoke sequence，PTW wait 清理等。

内部依赖：无。

### `is_valid_lq_key(key)`

源码位置：`common_data_transaction.sv:162`

功能：判断 LQ key 的 value 是否小于 `MEMBLOCK_LQ_SIZE`。

输入：`memblock_lq_key_t key`。

输出：bit。

主要调用方：`activate_uid()`、`lookup_active_uid_by_lq()`、`retire_active_uid()`、`release_uid_lq_mapping()`。

内部依赖：无。

### `is_valid_sq_key(key)`

源码位置：`common_data_transaction.sv:166`

功能：判断 SQ key 的 value 是否小于 `MEMBLOCK_SQ_SIZE`。

输入：`memblock_sq_key_t key`。

输出：bit。

主要调用方：`activate_uid()`、`lookup_active_uid_by_sq()`、`retire_active_uid()`、`release_uid_sq_mapping()`。

内部依赖：无。

### `check_uid(uid, caller)`

源码位置：`common_data_transaction.sv:170`

功能：公共 uid fatal 检查。调用方传入自身名字，错误日志能直接定位是哪条路径传了非法 uid。

输入：`uid` 和 `caller` 字符串。

输出：无；非法时 `uvm_fatal`。

主要调用方：大量 public API，例如 `set_main_transaction()`、`get_status()`、`set_tlb_transaction()`、`push_issue_queue_item()`。

内部依赖：调用 `is_valid_uid()`。

### `ensure_status_exists(uid, caller)`

源码位置：`common_data_transaction.sv:196`

功能：保证 `status_by_uid[uid]` 存在；不存在时创建并 reset。

输入：`uid` 和 `caller`。

输出：无显式返回；必要时创建状态表项。

主要调用方：`set_status_field()`、`mark_issue_snapshot()`、`activate_uid()`。

内部依赖：调用 `check_uid()` 和 `status_transaction::reset()`。

### `init_status_for_uid(uid)`

源码位置：`common_data_transaction.sv:204`

源码关键逻辑：拿或创建 status；若 status 已 active 则 fatal；reset 后从主表 snapshot 静态字段。

功能：主表生成完成后初始化每条 transaction 的状态表。它把 ROB/LQ/SQ 等主表静态字段复制到 status 中，方便后续 active map 和结束检查使用。

输入：`uid`。

输出：初始化后的 `status_transaction` 句柄。

主要调用方：`memblock_dispatch_base_sequence::init_status_for_main_table()`。

内部依赖：调用 `check_uid()`、`status.reset()`、`status.snapshot_from_main()`。

### `get_status(uid)`

源码位置：`common_data_transaction.sv:223`

功能：按 uid 读取状态表项，并检查非空。

输入：`uid`。

输出：`status_transaction` 句柄。

主要调用方：几乎所有状态更新路径：issue scheduler、writeback handler、redirect handler、LSQ commit handler、TLB/issue queue helper。

内部依赖：调用 `check_uid()`。

### `set_status_field(uid, field, value)`

源码位置：`common_data_transaction.sv:231`

源码关键逻辑：根据 `memblock_status_field_e` 枚举更新对应 status bit；禁止直接改 `ACTIVE`，要求通过 `activate_uid()` 和 `retire_active_uid()` 维护 active map 一致性。

功能：统一状态字段写入口。它让 helper 不需要直接知道 `status_transaction` 内部字段名。

输入：`uid`、状态字段枚举 `field`、写入值 `value`。

输出：无。

主要调用方：`lsq_ctrl_model` 设置 ENQ；`tlb_map_builder` 设置 TLB_MAPPED；`issue_queue_scheduler` 设置 queued/dispatched；部分内部 conditional setter。

内部依赖：调用 `ensure_status_exists()`。

### `get_status_field(uid, field)`

源码位置：`common_data_transaction.sv:275`

功能：按枚举读取状态字段。

输入：`uid`、状态字段枚举 `field`。

输出：对应字段 bit。

主要调用方：当前主要作为公共 API 预留，源码中没有外部直接调用。

内部依赖：调用 `get_status()`。

### target 级 issue_epoch/replay_seq 过滤

当前实现只保留 target 级条件更新路径：`conditional_set_target_status_field()` 使用
`status.get_target_issue_epoch(target)` 和 `replay_seq` 判断反馈是否属于当前发射实例。
旧的 uid 级粗粒度 issue 快照字段和通用条件更新入口已移除，避免和
`load_issue_epoch/sta_issue_epoch/std_issue_epoch` 形成重复状态。

字段语义：

| 字段 | 含义 | 为什么需要 |
|---|---|---|
| target 级 `issue_epoch` | 某个 issue target 被 DUT ready/fire 接受时分配的发射实例编号，保存在 `load_issue_epoch/sta_issue_epoch/std_issue_epoch` 中。 | 区分同一 uid、同一 target 的不同发射实例，避免旧发射 attempt 的迟到 feedback 更新新发射状态。 |
| `replay_seq` | 该 uid 当前 replay 轮次，初始为 0，每次 `mark_replay_pending()` 后递增。 | replay 后 uid 仍然 active，ROB/LQ/SQ 反查仍可能命中同一个 uid，需要 replay 轮次判断 event 是否属于当前轮。 |
| `target` | 本次状态更新对应的发射路径，通常是 `LOAD`、`STA` 或 `STD`。 | store 会拆成 STA/STD 两条路径，同一个 uid 的不同 target 不能互相误更新。 |

这里要注意一个容易误解的点：replay/redirect 确实通常来自 MemBlock 已经接收 issue 后的后续 feedback、writeback 或 ctrl output，不是未入流水线时凭空产生。但这仍然不能说明旧发射不会误写新状态。风险主要不是“旧 uid 写到新 uid”，而是“同一个 uid 的旧发射实例写到这个 uid 的新 replay 发射状态”。

典型 replay 时间线：

```text
uid=5 第一次发射
  issue_epoch = 10
  replay_seq  = 0

DUT 返回 replay
  mark_replay_pending()
  replay_seq 变成 1
  uid=5 重新入队

uid=5 第二次发射
  issue_epoch = 18
  replay_seq  = 1
```

如果第一次发射的旧 pass/writeback 事件由于 raw monitor queue、同拍处理顺序、STA/STD 分路或 synthetic feedback 等原因晚到，只按 uid 更新就可能把第二次发射状态错误置成 pass。target 级 `issue_epoch/replay_seq` 的意义就是把同一 uid、同一 target 下的不同 attempt 分开。

redirect 后的风险相对小一些，因为 redirect flush 会清 dispatch 结果并 `retire_active_uid()`，旧 feedback 再回来通常会因为 active map 反查失败或 status inactive 被 drop。但 replay 后 uid 仍 active，因此 replay 场景更依赖 `issue_epoch/replay_seq` 做防护。

## 4. Issue 发射快照与目标状态

### `alloc_issue_epoch()`

源码位置：`common_data_transaction.sv:339`

功能：分配全局递增 issue epoch。每次某个 issue item 被 DUT 接受时分配新 epoch，用于区分旧发射和新 replay 发射。

输入：无。

输出：新的 `int unsigned issue_epoch`。

主要调用方：`issue_queue_scheduler::mark_issue_fire()` 和 `mark_issue_fire_already_accepted()`。

内部依赖：自增 `global_issue_epoch`。

### `mark_issue_snapshot(uid, target, issue_epoch)`

源码位置：`common_data_transaction.sv:344`

功能：在 status 中记录本次发射 target 的 issue epoch，并清掉 `issue_killed`。

输入：uid、target、issue epoch。

输出：无。

主要调用方：`issue_queue_scheduler::mark_issue_fire()` 和 `mark_issue_fire_already_accepted()`。

内部依赖：调用 `ensure_status_exists()` 和 `status.set_target_issue_epoch()`。

### `target_writeback_field(target)`

源码位置：`common_data_transaction.sv:357`

功能：把 issue target 映射到对应 writeback 状态字段：LOAD -> `LOAD_WRITEBACK`，STA -> `STA_WRITEBACK`，STD -> `STD_WRITEBACK`。

输入：`memblock_issue_target_e target`。

输出：`memblock_status_field_e`。

主要调用方：`mark_target_normal_pass()`、`mark_target_fault()`。

内部依赖：无。

### `target_pass_field(target)`

源码位置：`common_data_transaction.sv:369`

功能：把 issue target 映射到对应 pass 字段。

输入：target。

输出：状态字段枚举。

主要调用方：`mark_target_normal_pass()`。

内部依赖：无。

### `target_fault_field(target)`

源码位置：`common_data_transaction.sv:381`

功能：把 issue target 映射到对应 fault 字段。

输入：target。

输出：状态字段枚举。

主要调用方：`mark_target_fault()`。

内部依赖：无。

### `target_entry_done(status, target)`

源码位置：`common_data_transaction.sv:393`

功能：判断某个 target 是否已经结束。结束条件是该 target pass 或 fault。

输入：`status_transaction status` 和 target。

输出：bit。

主要调用方：`required_targets_done()`、`mark_target_normal_pass()`。

内部依赖：无。

### `required_targets_done(uid)`

源码位置：`common_data_transaction.sv:409`

源码关键逻辑：load 只要求 LOAD target 完成；store/MOU 要求 STA 和 STD 都完成。

功能：判断这条 transaction 需要的所有 issue target 是否都完成。store 被拆成 STA/STD 两路，因此必须两路都 pass 或 fault 后才算整条 store 完成。

输入：uid。

输出：bit。

主要调用方：`mark_target_normal_pass()`，`lsq_commit_handler`。

内部依赖：调用 `get_status()`、`get_main_transaction()`、`target_entry_done()`。

### `conditional_set_target_status_field(uid, field, value, target, issue_epoch, replay_seq)`

源码位置：`common_data_transaction.sv:401`

功能：target 级别的条件状态更新。它用 `status.get_target_issue_epoch(target)` 和 `replay_seq` 过滤迟到事件。

输入：uid、字段和值、target、issue epoch、replay seq。

输出：bit，更新成功为 1。

主要调用方：`mark_target_normal_pass()`、`mark_target_fault()`。

内部依赖：调用 `get_status()`、`set_status_field()`。

### `mark_target_normal_pass(uid, target, issue_epoch, replay_seq, cycle)`

源码位置：`common_data_transaction.sv:420`

源码关键逻辑：若该 target 已 fault/replay/redirect 或已经 done，则忽略；否则设置 target writeback 和 pass 字段；若整条 transaction 的 required targets 都完成，则设置总 `writeback/pass`。

功能：处理 DUT 返回的正常成功事件。对 load 是 load writeback pass；对 store 是 STA/STD feedback/pass 的统一状态更新。

入口处的保护条件是：

```systemverilog
if (status.fault || status.exception_pending ||
    status.replay_pending || status.redirect_pending ||
    target_entry_done(status, target)) begin
    return 1'b0;
end
```

这几个状态表示该 uid 已经不适合再被 normal pass 更新：

| 状态 | 含义 | 典型出现时机 | 为什么阻止 normal pass |
|---|---|---|---|
| `fault` | 该 uid 已经产生 fault/异常结果，是结果属性。 | `writeback_status_handler` 首次采到 fault event 后调用 `mark_target_fault()` 置 1；recovery handler 只消费 fault event，不重复写状态。 | 已经确认异常后，迟到的正常 pass 不能覆盖 fault。 |
| `exception_pending` | 异常已经发生，异常处理/recovery 尚未收敛，是流程状态。 | 当前实现中与 `fault` 一起在 `mark_target_fault()` 中置 1。 | pending 异常期间不能继续把该 uid 收敛成正常完成。 |
| `replay_pending` | 该 uid 正在 backend replay 流程中，至少一个 target 需要重新发射。 | replay event 进入 `mark_replay_pending()` 后置 1；对应 replay target 重新 fire 并清空所有 replay mask 后再清 0。 | replay 期间旧 pass 可能属于 replay 前的旧发射，不能提前设置 pass/writeback。 |
| `redirect_pending` | 该 uid 处于 redirect/flush 影响范围内，旧发射结果需要失效。 | redirect flush 命中 active uid 时在 `apply_redirect_flush()` 中短暂置 1，随后清 dispatch 结果、置 `flushed/issue_killed` 并 retire active uid。 | redirect/flush 期间旧流水线反馈不能继续更新状态。 |
| `target_entry_done(status,target)` | 当前 target 已经 pass 或 fault。 | load 的 LOAD target、store 的 STA/STD target 已经完成其一。 | 防止重复 feedback 二次写状态，或者 pass 覆盖已有 target fault。 |

因此这段检查的本质是“normal pass 只能更新仍在正常流中的当前 target”。一旦该 uid 已经进入 fault、exception、replay、redirect，或者该 target 已经完成，normal pass 会被当作迟到/重复/无效事件丢弃。

输入：uid、target、issue epoch、replay seq、事件 cycle。

输出：bit，成功更新为 1，迟到或无效事件为 0。

主要调用方：`writeback_status_handler::handle_event()`。

内部依赖：调用 `target_entry_done()`、`conditional_set_target_status_field()`、`target_writeback_field()`、`target_pass_field()`、`required_targets_done()`。

### `mark_target_fault(uid, target, issue_epoch, replay_seq, exception_vec, cycle)`

源码位置：`common_data_transaction.sv:449`

功能：处理带异常的 writeback/feedback。它设置 target writeback、target fault、总 fault、exception pending，并清掉 pass/success。

输入：uid、target、issue epoch、replay seq、exception vector、事件 cycle。

输出：bit，成功更新为 1。

主要调用方：`writeback_status_handler::handle_event()`。`exception_redirect_replay_handler::handle_fault_event()` 只消费 recovery 队列中的 fault event，不再调用本函数。

内部依赖：调用 `conditional_set_target_status_field()`、`target_writeback_field()`、`target_fault_field()`、`get_status()`。

## 5. Replay 状态

### `set_replay_target_mask(status, replay_load, replay_sta, replay_std)`

源码位置：`common_data_transaction.sv:498`

功能：直接设置 status 中三类 replay target mask。

输入：status 句柄，以及 load/STA/STD 三个 replay bit。

输出：无。

主要调用方：当前源码中没有外部直接调用，属于 replay target mask 的预留工具函数。

内部依赖：只检查 status 非空。

### `mark_replay_pending(uid, target, issue_epoch, replay_seq, cycle)`

源码位置：`common_data_transaction.sv:485`

源码关键逻辑：检查 active、未 kill、issue_epoch 和 replay_seq 匹配；先从 issue queue 删除该 uid；设置 `replay_pending`，清除 pass/success；按 target 清 dispatched/writeback/pass，并设置对应 `replay_target_*`；最后 `bump_replay_seq()`。

功能：把 DUT feedback 触发的 backend replay 转换成测试框架可重新发射的状态。它不会直接发射，而是标记状态，后续 `issue_queue_scheduler` 会重新 route 对应 target。

输入：uid、target、issue epoch、replay seq、事件 cycle。

输出：bit，成功标记为 1；迟到或不匹配事件为 0。

主要调用方：`exception_redirect_replay_handler::handle_replay_event()` 和 `service_ptw_wait_replay()`。

内部依赖：调用 `get_status()`、`remove_uid_from_issue_queues()`、`bump_replay_seq()`。

### `replay_target_requested(status, target)`

源码位置：`common_data_transaction.sv:556`

功能：判断某个 target 是否被 replay mask 请求。

输入：status、target。

输出：bit。

主要调用方：`issue_queue_scheduler::route_target()`，用于只把需要 replay 的 target 重新入队。

内部依赖：无。

### `replay_targets_empty(status)`

源码位置：`common_data_transaction.sv:572`

功能：判断 load/STA/STD 三个 replay target mask 是否全部为 0。

输入：status。

输出：bit。

主要调用方：`clear_replay_target_after_fire()`。

内部依赖：无。

### `clear_replay_target_after_fire(uid, target)`

源码位置：`common_data_transaction.sv:581`

功能：replay target 被再次发射后清掉对应 target mask；如果所有 replay target 都清空，则清 `replay_pending`。

输入：uid、target。

输出：无。

主要调用方：`issue_queue_scheduler::mark_issue_fire()` 和 `mark_issue_fire_already_accepted()`。

内部依赖：调用 `get_status()`、`replay_targets_empty()`。

### `bump_replay_seq(uid)`

源码位置：`common_data_transaction.sv:602`

功能：递增该 uid 的 replay sequence。新一轮 replay 发射必须使用新的 replay_seq，旧反馈再回来时会被过滤掉。

输入：uid。

输出：无。

主要调用方：`mark_replay_pending()`。

内部依赖：调用 `get_status()`。

### `clear_uid_dispatch_result(uid)`

源码位置：`common_data_transaction.sv:609`

功能：清除一个 uid 的 queued/dispatched/writeback/pass/replay/redirect 等发射结果，并置 `issue_killed=1`。通常用于 redirect flush 范围内的 transaction。

输入：uid。

输出：无。

主要调用方：`apply_redirect_flush()`。

内部依赖：调用 `get_status()`。

## 6. Redirect 与全局发射冻结

### `request_redirect_flush(redirect)`

源码位置：`common_data_transaction.sv:636`

源码关键逻辑：检查 redirect valid；设置 `flush_in_progress` 和 `memblock_sync_pkg::dispatch_flush_in_progress`；递增 `dispatch_flush_epoch`；置 `issue_freeze_ack`；保存 `active_redirect`；记录 freeze cycle。

功能：收到 DUT output memoryViolation 等 redirect event 后，先冻结 issue/admission/commit 相关 flow，避免旧上下文继续发射。

输入：`memblock_redirect_payload_t redirect`。

输出：无。

主要调用方：`exception_redirect_replay_handler::process_pending_events()`。

内部依赖：调用 `memblock_sync_pkg::get_dispatch_service_cycle()`。

### `redirect_payload_equal(left, right)`

源码位置：`common_data_transaction.sv:650`

功能：比较两个 redirect payload 是否完全相同，包括 valid、flush_itself、level、rob flag/value。

输入：两个 redirect payload。

输出：bit。

主要调用方：`mark_redirect_drive_done()`、`redirect_drive_done_for()`。

内部依赖：无。

### `push_redirect_drive(payload)`

源码位置：`common_data_transaction.sv:659`

功能：把需要真实驱动到 DUT `io_redirect_*` 的 payload 放入 `pending_redirect_drive_q`。

输入：redirect payload。

输出：无。

主要调用方：`exception_redirect_replay_handler::process_pending_events()`。

内部依赖：无。

### `try_pop_redirect_drive(payload)`

源码位置：`common_data_transaction.sv:666`

源码关键逻辑：如果 queue 为空或已有 inflight redirect，则返回 0；否则 pop front，记录 inflight payload 并置 `redirect_drive_inflight=1`。

功能：供 `memblock_redirect_dispatch_sequence` 获取一个待驱动 redirect。它保证同一时间只允许一个 redirect payload inflight。

输入：无。

输出：output `payload`，返回 bit 表示是否取到。

主要调用方：`memblock_redirect_dispatch_sequence::body()`。

内部依赖：读写 `pending_redirect_drive_q` 和 inflight 状态。

### `mark_redirect_drive_done(payload)`

源码位置：`common_data_transaction.sv:677`

功能：redirect sequence 驱动完 DUT input 后回写完成状态。若 payload 和 inflight 不一致则 fatal；匹配 active redirect 时推进 phase 到 `REDIRECT_DRIVEN`。

输入：已驱动完成的 redirect payload。

输出：无。

主要调用方：`memblock_redirect_dispatch_sequence::mark_drive_done()`。

内部依赖：调用 `redirect_payload_equal()` 和 `memblock_sync_pkg::get_dispatch_service_cycle()`。

### `has_pending_redirect_drive()`

源码位置：`common_data_transaction.sv:694`

功能：判断是否还有待驱动或正在驱动的 redirect。

输入：无。

输出：bit。

主要调用方：`issue_blocked_by_global_flush()`、`end_test_check()`。

内部依赖：读 `pending_redirect_drive_q` 和 `redirect_drive_inflight`。

### `redirect_drive_done_for(payload)`

源码位置：`common_data_transaction.sv:698`

功能：判断指定 redirect payload 是否已经完成驱动，并且至少跨过一个 service cycle。跨 cycle 是为了避免同一 delta/同拍内刚驱动就立刻清状态。

输入：redirect payload。

输出：bit。

主要调用方：`exception_redirect_replay_handler::advance_active_redirect()`。

内部依赖：调用 `redirect_payload_equal()` 和 `memblock_sync_pkg::get_dispatch_service_cycle()`。

### `clear_redirect_drive_queue()`

源码位置：`common_data_transaction.sv:715`

功能：清空 pending redirect queue 和 inflight 状态。

输入：无。

输出：无。

主要调用方：`reset_all_tables()`、`apply_redirect_flush()`。

内部依赖：无。

### `issue_blocked_by_global_flush()`

源码位置：`common_data_transaction.sv:721`

功能：统一判断 issue/admission 是否应被全局 flush/redirect 阻塞。只要 flush in progress、active redirect、issue freeze、pending redirect drive 或 sync pkg flush flag 任一为真，就返回 1。

输入：无。

输出：bit。

主要调用方：`issue_queue_scheduler`、`memblock_lintsissue_dispatch_sequence`、`memblock_lsqcommit_dispatch_sequence`。

内部依赖：调用 `has_pending_redirect_drive()`。

### `apply_redirect_flush(redirect)`

源码位置：`common_data_transaction.sv:729`

源码关键逻辑：遍历所有 active uid；用 `rob_order_util::rob_need_flush()` 判断是否落在 redirect flush 范围；对命中的 uid 标记 redirect/flushed、清 dispatch 结果、retire active map；清 PTW wait、redirect drive queue；释放全局 flush/redirect 状态。

功能：redirect 被真实驱动给 DUT 后，测试框架执行软件状态清理。它保证被 flush 的 uid 不再留在 active map、issue queue 或 replay wait queue 中。

输入：redirect payload。

输出：无。

主要调用方：`exception_redirect_replay_handler::advance_active_redirect()`。

内部依赖：调用 `get_status()`、`rob_order_util::rob_need_flush()`、`clear_uid_dispatch_result()`、`retire_active_uid()`、`clear_ptw_wait_replay_by_redirect()`、`clear_redirect_drive_queue()`。

## 7. Writeback/Feedback 事件队列

### `make_empty_wb_event()`

源码位置：`common_data_transaction.sv:760`

功能：构造一个所有字段清零、source 为 NONE、target 为 NONE 的空 `memblock_wb_event_t`。

输入：无。

输出：空 wb event。

主要调用方：`normalize_feedback_event()`、`pop_feedback_event()`、`exception_redirect_replay_handler::select_oldest_redirect()`。

内部依赖：无。

### `feedback_event_is_redirect(wb_event)`

源码位置：`common_data_transaction.sv:793`

功能：判断事件是否为 redirect 类事件。分类以 canonical `wb_event.redirect.valid` 为准；`wb_event.redirect_valid` 作为兼容/显式标志必须与 `wb_event.redirect.valid` 一致，不一致时 fatal。`source` 只表示来源，不能单独触发 redirect。

输入：wb event。

输出：bit。

主要调用方：`feedback_event_has_action()`、`normalize_feedback_event()`。

内部依赖：无。

### `feedback_event_is_replay(wb_event)`

源码位置：`common_data_transaction.sv:798`

功能：判断事件是否为 replay 类事件。

输入：wb event。

输出：bit。

主要调用方：`feedback_event_has_action()`。

内部依赖：无。

### `feedback_event_has_fault(wb_event)`

源码位置：`common_data_transaction.sv:802`

功能：判断事件是否携带异常。既看 `has_exception`，也看 `exception_vec != 0`。

输入：wb event。

输出：bit。

主要调用方：`feedback_event_has_action()`。

内部依赖：无。

### `feedback_event_has_action(wb_event)`

源码位置：`common_data_transaction.sv:806`

功能：判断事件是否有实际动作：redirect、replay、fault 或 normal raw writeback。

输入：wb event。

输出：bit。

主要调用方：`normalize_feedback_event()`。

内部依赖：调用 `feedback_event_is_redirect()`、`feedback_event_is_replay()`、`feedback_event_has_fault()`。

### `feedback_event_target_is_valid(target)`

源码位置：`common_data_transaction.sv:813`

功能：检查 target 是否是 LOAD/STA/STD 三类合法 issue target。

输入：target。

输出：bit。

主要调用方：`normalize_feedback_event()`。

内部依赖：无。

### `normalize_feedback_event(wb_event, normalized_event)`

源码位置：`common_data_transaction.sv:819`

源码关键逻辑：丢弃 invalid 或无动作事件；redirect 事件若没有 ROB key，则从 payload 补；通过 `resolve_uid_for_event()` 反查 active uid；补齐 uid、ROB key、issue_epoch、replay_seq；replay 后缺少 issue_epoch/replay_seq 的事件会被丢弃，防止旧反馈误命中。

功能：把 monitor adapter 或 handler 送来的事件标准化成“带 uid、issue_epoch、replay_seq”的统一事件。这是 writeback/replay/redirect 正确落到状态表的关键入口。

输入：原始 wb event。

输出：output `normalized_event`，返回 bit 表示是否有效。

主要调用方：`push_feedback_event()`、`writeback_status_handler::handle_event()`。

内部依赖：调用 `feedback_event_has_action()`、`make_empty_wb_event()`、`resolve_uid_for_event()`、`get_status()`、`feedback_event_is_redirect()`、`feedback_event_target_is_valid()`。

### `push_feedback_event(wb_event)`

源码位置：`common_data_transaction.sv:868`

功能：标准化事件后压入 `exception_event_q`，供 replay/redirect/fault handler 后续消费。

输入：wb event。

输出：无。

主要调用方：`writeback_status_handler::handle_event()`。

内部依赖：调用 `normalize_feedback_event()`。

### `pop_feedback_event(wb_event)`

源码位置：`common_data_transaction.sv:877`

功能：从 `exception_event_q` 弹出一个事件；队列为空时返回空事件和 0。

输入：无。

输出：output wb event，返回 bit 表示是否弹出。

主要调用方：`exception_redirect_replay_handler::process_pending_events()`。

内部依赖：调用 `make_empty_wb_event()` 构造空事件。

### `clear_feedback_events()`

源码位置：`common_data_transaction.sv:886`

功能：清空 feedback/exception event 队列。

输入：无。

输出：无。

主要调用方：`reset_all_tables()`。

内部依赖：无。

### `resolve_uid_for_event(wb_event, uid)`

源码位置：`common_data_transaction.sv:890`

源码关键逻辑：事件可携带 uid、ROB key、LQ key、SQ key 中任意组合；函数逐项反查 active map。如果多个 key 都存在，必须反查到同一个 uid，否则 fatal。

功能：把 DUT monitor 采到的 ROB/LQ/SQ 事实映射回测试框架 uid。它是 raw monitor 世界和公共状态表世界的桥梁。

输入：wb event。

输出：output uid，返回 bit 表示是否成功找到 active uid。

主要调用方：`normalize_feedback_event()`、`dispatch_monitor_event_adapter::event_has_active_uid()`、`exception_redirect_replay_handler::handle_replay_event()`、`handle_fault_event()`。

内部依赖：调用 `check_uid()`、`lookup_active_uid_by_rob()`、`lookup_active_uid_by_lq()`、`lookup_active_uid_by_sq()`。

### `get_event_issue_epoch(wb_event, uid)`

源码位置：`common_data_transaction.sv:940`

功能：从事件读取 issue epoch；若事件未携带，则从 status 的 target issue epoch 补齐。

输入：wb event、uid。

输出：issue epoch。

主要调用方：`exception_redirect_replay_handler::handle_replay_event()` 和 `handle_fault_event()`。

内部依赖：调用 `get_status()`。

### `get_event_replay_seq(wb_event, uid)`

源码位置：`common_data_transaction.sv:951`

功能：从事件读取 replay seq；若事件未携带，则从 status 补齐。

输入：wb event、uid。

输出：replay seq。

主要调用方：`exception_redirect_replay_handler::handle_replay_event()` 和 `handle_fault_event()`。

内部依赖：调用 `get_status()`。

## 8. Active ROB/LQ/SQ 映射

### `activate_uid(uid, map_lq, map_sq)`

源码位置：`common_data_transaction.sv:959`

源码关键逻辑：从主表 snapshot 到 status；检查 ROB key 未 active；按 `map_lq/map_sq` 建 LQ/SQ active map；最后建立 ROB active map 并置 `status.active=1`。

功能：当 transaction 真实入队成功后，把 uid 变成 active。active 的含义是后续 DUT monitor 返回 ROB/LQ/SQ 事件时，可以合法反查到这条 transaction。

输入：uid，以及是否建立 LQ/SQ map 的 bit。

输出：无。

主要调用方：`lsq_ctrl_model::commit_allocate()` 和 `commit_allocate_with_resp()`。

内部依赖：调用 `get_main_transaction()`、`ensure_status_exists()`、`rob_order_util::rob_to_map_key()`、`is_valid_lq_key()`、`rob_order_util::lq_to_map_key()`、`is_valid_sq_key()`、`rob_order_util::sq_to_map_key()`。

### `activate_uid_by_behavior(uid, behavior)`

源码位置：`common_data_transaction.sv:1015`

功能：根据 `memblock_op_behavior_t` 的 `uses_lq/uses_sq` 调用 `activate_uid()`。

输入：uid、op behavior。

输出：无。

主要调用方：当前源码中没有直接外部调用，属于更语义化的 wrapper。

内部依赖：调用 `activate_uid()`。

### `lookup_active_uid_by_rob(rob_key, uid)`

源码位置：`common_data_transaction.sv:1020`

功能：用 ROB key 反查 active uid，并检查 map 没有 stale。

输入：ROB key。

输出：output uid，返回 bit 表示是否找到。

主要调用方：`resolve_uid_for_event()`；`get_active_uid_by_rob()`。

内部依赖：调用 `rob_order_util::rob_to_map_key()` 和 `is_valid_uid()`。

### `lookup_active_uid_by_lq(lq_key, uid)`

源码位置：`common_data_transaction.sv:1035`

功能：用 LQ key 反查 active uid。

输入：LQ key。

输出：output uid，返回 bit。

主要调用方：`resolve_uid_for_event()`，`lsq_commit_handler` 处理 `lqDeq`。

内部依赖：调用 `is_valid_lq_key()`、`rob_order_util::lq_to_map_key()`、`is_valid_uid()`。

### `lookup_active_uid_by_sq(sq_key, uid)`

源码位置：`common_data_transaction.sv:1053`

功能：用 SQ key 反查 active uid。

输入：SQ key。

输出：output uid，返回 bit。

主要调用方：`resolve_uid_for_event()`，`lsq_commit_handler` 处理 `sqDeq`。

内部依赖：调用 `is_valid_sq_key()`、`rob_order_util::sq_to_map_key()`、`is_valid_uid()`。

### `get_active_uid_by_rob(rob_key)`

源码位置：`common_data_transaction.sv:1071`

功能：必须成功的 ROB 反查版本；找不到 active uid 时 fatal。

输入：ROB key。

输出：uid。

主要调用方：当前源码中没有外部直接调用，适合作为严格路径 helper。

内部依赖：调用 `lookup_active_uid_by_rob()`。

### `retire_active_uid(uid)`

源码位置：`common_data_transaction.sv:1080`

源码关键逻辑：检查 uid active；先从 issue queue 删除；检查并删除 ROB map；若 LQ/SQ map active，也检查一致性后删除；最后置 `status.active=0`。

功能：释放这条 transaction 在公共状态中的 active 身份。commit 完成、LSQ deq 完成或 redirect flush 时最终都要 retire，否则 monitor 后续会把旧 key 误反查到旧 uid。

输入：uid。

输出：无。

主要调用方：`try_retire_committed_uid()`，`apply_redirect_flush()`。

内部依赖：调用 `get_status()`、`remove_uid_from_issue_queues()`、`rob_order_util::*_to_map_key()`、`is_valid_lq_key()`、`is_valid_sq_key()`。

### `release_uid_lq_mapping(uid)`

源码位置：`common_data_transaction.sv:1142`

功能：DUT 返回 LQ deq 后释放 uid 对应 LQ active map。若 LQ/SQ map 都已释放，则置 `status.lsq_deq=1`。

输入：uid。

输出：无。

主要调用方：`lsq_commit_handler` 处理 `lqDeq`。

内部依赖：调用 `get_status()`、`is_valid_lq_key()`、`rob_order_util::lq_to_map_key()`。

### `release_uid_sq_mapping(uid)`

源码位置：`common_data_transaction.sv:1172`

功能：DUT 返回 SQ deq 后释放 uid 对应 SQ active map。若 LQ/SQ map 都已释放，则置 `status.lsq_deq=1`。

输入：uid。

输出：无。

主要调用方：`lsq_commit_handler` 处理 `sqDeq`。

内部依赖：调用 `get_status()`、`is_valid_sq_key()`、`rob_order_util::sq_to_map_key()`。

### `try_retire_committed_uid(uid)`

源码位置：`common_data_transaction.sv:1202`

源码关键逻辑：只有 active 且 `rob_commit=1`，同时 LQ/SQ map 都已释放时才 retire；success 要求没有 fault/exception/replay/redirect/flushed。

功能：commit 和 LSQ deq 两条异步路径的汇合点。ROB commit 先到或 LSQ deq 先到都可以，只有两边都完成才 retire active uid。

输入：uid。

输出：无。

主要调用方：`lsq_commit_handler` 在 commit pointer 推进和 LQ/SQ deq 后调用。

内部依赖：调用 `get_status()`、`retire_active_uid()`。

## 9. TLB 表与 CSR runtime

### `set_tlb_transaction(uid, tr)`

源码位置：`common_data_transaction.sv:1227`

功能：按 uid 保存 TLB 表项，并清 `tlb_response_done_by_uid[uid]`。

输入：uid、`tlb_transaction tr`。

输出：无。

主要调用方：`tlb_map_builder::build_for_uid()`。

内部依赖：调用 `check_uid()`。

### `mark_tlb_response_done(uid)`

源码位置：`common_data_transaction.sv:1237`

功能：L2TLB responder 真实向 DTLB 回包后，标记该 uid 的 TLB response 已完成。

输入：uid。

输出：无。

主要调用方：`memblock_l2tlb_base_sequence`。

内部依赖：调用 `check_uid()`。

### `get_tlb_transaction(uid)`

源码位置：`common_data_transaction.sv:1246`

功能：按 uid 读取 TLB 表项。

输入：uid。

输出：`tlb_transaction` 句柄。

主要调用方：`memblock_l2tlb_base_sequence` 查到 uid 后回填 response 字段。

内部依赖：调用 `check_uid()`。

### `register_tlb_lookup(key, uid)`

源码位置：`common_data_transaction.sv:1254`

源码关键逻辑：若同 key 已存在旧 uid，且旧 uid TLB 表项与新 uid 的 ppn 或 csr_update_seq 冲突，则 fatal；否则建立 `key -> uid` 映射。

功能：建立 L2TLB request 查表索引。key 通常包含 vpn/asid/vmid/s2xlate/runtime CSR seq。

输入：TLB lookup key、uid。

输出：无。

主要调用方：`tlb_map_builder::build_for_uid()`。

内部依赖：调用 `check_uid()`、`is_valid_uid()`。

### `lookup_tlb_uid(key, csr_update_seq, uid)`

源码位置：`common_data_transaction.sv:1271`

功能：按完整 TLB key 查 uid，并要求表项的 CSR update seq 与输入一致。

输入：lookup key、CSR update seq。

输出：output uid，返回 bit。

主要调用方：`lookup_tlb_uid_by_req()`；`tlb_map_builder::lookup_tlb_uid()` wrapper。

内部依赖：调用 `is_valid_uid()`。

### `lookup_tlb_uid_by_req(vpn, s2xlate, uid)`

源码位置：`common_data_transaction.sv:1288`

源码关键逻辑：用当前 `mmu_csr_state` 生成 full lookup key；先查 `uid_by_tlb_key`；失败后扫描 `tlb_table_by_uid[]`，按 `vpn/s2xlate/csr_update_seq` 做 fallback 匹配。

功能：L2TLB responder 根据 DTLB request 的 `vpn/s2xlate` 查到 uid。asid/vmid 来自 runtime CSR snapshot。

输入：request 中的 38-bit vpn 和 2-bit s2xlate。

输出：output uid，返回 bit。

主要调用方：`memblock_l2tlb_base_sequence::lookup_tlb_response()`。

内部依赖：调用 `mmu_csr_state.make_lookup_key()`、`lookup_tlb_uid()`。

### `tlb_entry_ready_for_uid(uid)`

源码位置：`common_data_transaction.sv:1318`

功能：判断 uid 的 TLB 表项存在且 L2TLB response 已完成。

输入：uid。

输出：bit。

主要调用方：`pop_ready_ptw_wait_replay()`。

内部依赖：调用 `check_uid()`。

### `clear_tlb_lookup_index()`

源码位置：`common_data_transaction.sv:1325`

功能：清空 TLB lookup 关联数组。

输入：无。

输出：无。

主要调用方：`apply_raw_csr_runtime()` 在 CSR runtime 变化后调用。

内部依赖：无。

### `apply_raw_csr_runtime(raw)`

源码位置：`common_data_transaction.sv:1329`

源码关键逻辑：raw 无效则返回；确保 `mmu_csr_state` 存在；调用 `update_from_raw_csr()`；若 update_seq 变化，清空 TLB lookup index。

功能：把 CSR monitor 采到的 runtime CSR 事实同步到公共 CSR snapshot。TLB 查表必须使用 runtime CSR，而不是初始配置。

输入：`memblock_sync_pkg::dispatch_raw_csr_t raw`。

输出：无。

主要调用方：`dispatch_monitor_event_adapter::drain_csr_events()`。

内部依赖：调用 `mmu_csr_state.update_from_raw_csr()`、`clear_tlb_lookup_index()`。

## 10. Issue 队列维护

### `clear_issue_queues()`

源码位置：`common_data_transaction.sv:1346`

功能：清空 load/STA/STD 三个 issue 队列。

输入：无。

输出：无。

主要调用方：`reset_all_tables()`。

内部依赖：无。

### `issue_queue_contains(target, uid, replay_seq)`

源码位置：`common_data_transaction.sv:1352`

功能：判断指定 target 队列里是否已经存在同 uid、同 replay_seq 的 item，防止重复入队。

输入：target、uid、replay seq。

输出：bit。

主要调用方：`push_issue_queue_item()`。

内部依赖：扫描对应 issue queue。

### `delete_issue_queue_entry(target, uid, replay_seq, match_replay_seq)`

源码位置：`common_data_transaction.sv:1384`

功能：从指定 target 队列删除 uid 对应 item。`match_replay_seq=1` 时只删指定 replay_seq；为 0 时删除该 uid 的所有 replay_seq 项。

输入：target、uid、replay seq、是否匹配 replay seq。

输出：无。

主要调用方：`issue_queue_scheduler::route_target()`、`mark_issue_fire()`、`mark_issue_fire_already_accepted()`，以及 `remove_uid_from_issue_queues()`。

内部依赖：扫描并删除对应队列。

### `remove_uid_from_issue_queues(uid)`

源码位置：`common_data_transaction.sv:1419`

功能：从 load/STA/STD 三个队列删除该 uid 的所有项，并清 status 的 queued bit。

输入：uid。

输出：无。

主要调用方：`mark_replay_pending()`、`retire_active_uid()`。

内部依赖：调用三次 `delete_issue_queue_entry()`，并用 `is_valid_uid()` 防御非法 uid。

### `push_issue_queue_item(item)`

源码位置：`common_data_transaction.sv:1583`

功能：把 issue item 放入对应 load/STA/STD 队列。如果同 target、uid、replay_seq 已存在，则忽略。

输入：`memblock_issue_q_item_t item`。

输出：无。

主要调用方：`issue_queue_scheduler::route_target()`。

内部依赖：调用 `check_uid()`、`issue_queue_contains()`。

### `get_issue_queue_size(target)`

源码位置：`common_data_transaction.sv:1601`

功能：读取指定 issue 队列当前元素数量。

输入：target。

输出：队列 size。

主要调用方：当前源码中没有外部直接调用，属于调试/统计类公共 API。

内部依赖：无。

## 11. PTW-back Replay 等待

### `push_ptw_wait_replay(uid, target, issue_epoch, replay_seq, start_cycle)`

源码位置：`common_data_transaction.sv:1430`

功能：把需要等待 L2TLB/PTW response done 的 replay 放入 `ptw_wait_replay_q`。同 uid/target/replay_seq 已存在时不重复插入。

输入：uid、target、issue epoch、replay seq、开始等待的 service cycle。

输出：无。

主要调用方：`exception_redirect_replay_handler::handle_replay_event()` 在 `ptw_back_replay` 且配置要求等待 PTW 时调用。

内部依赖：调用 `check_uid()`。

### `pop_ready_ptw_wait_replay(timeout, wait_item, timed_out)`

源码位置：`common_data_transaction.sv:1455`

源码关键逻辑：扫描 wait queue；如果对应 uid 的 TLB response done，或等待超过 timeout，则弹出该项；`timed_out` 表示是否因超时释放。

功能：PTW-back replay 不能在 L2TLB response 尚未回包时立即重新发射，这个函数用于等到 ready 或超时后再让 replay 进入 `mark_replay_pending()`。

输入：timeout。

输出：output wait item、output timed_out，返回 bit 表示是否弹出。

主要调用方：`exception_redirect_replay_handler::service_ptw_wait_replay()`。

内部依赖：调用 `tlb_entry_ready_for_uid()` 和 `memblock_sync_pkg::get_dispatch_service_cycle()`。

### `release_ptw_wait_replay(uid)`

源码位置：`common_data_transaction.sv:1485`

功能：删除某 uid 的所有 PTW wait replay 项。

输入：uid。

输出：无。

主要调用方：当前源码中没有外部直接调用，属于清理 helper。

内部依赖：扫描 `ptw_wait_replay_q`。

### `clear_ptw_wait_replay_by_redirect(redirect)`

源码位置：`common_data_transaction.sv:1493`

功能：redirect flush 时清除被 flush 或已 inactive 的 PTW wait replay 项，避免 flush 后的旧 replay 重新进入 issue queue。

输入：redirect payload。

输出：无。

主要调用方：`apply_redirect_flush()`。

内部依赖：调用 `is_valid_uid()`、`get_status()`、`rob_order_util::rob_need_flush()`。

### `clear_ptw_wait_replay_queue()`

源码位置：`common_data_transaction.sv:1509`

功能：清空 PTW wait replay 队列。

输入：无。

输出：无。

主要调用方：`reset_all_tables()`。

内部依赖：无。

## 12. flushSb 协调

### `request_flushsb()`

源码位置：`common_data_transaction.sv:1513`

功能：请求发起 flushSb。若 `MEMBLOCK_FLUSHSB_SEQ_EN=0`，只打印 warning 并忽略。

输入：无。

输出：无；设置 `flushsb_pending=1`。

主要调用方：`request_scheduled_flushsb_if_due()`；也可作为手动请求 API。

内部依赖：调用 `seq_csr_common::get_flushsb_seq_en()`。

### `arm_scheduled_flushsb(cycle)`

源码位置：`common_data_transaction.sv:1521`

功能：配置某个 dispatch cycle 后触发 flushSb。若 sequence 未使能或 cycle 为 0，则清掉 scheduled 状态。

输入：cycle。

输出：无。

主要调用方：`check_main_table_complete()`，`memblock_lsqcommit_dispatch_sequence` 初始化时也会设置。

内部依赖：调用 `seq_csr_common::get_flushsb_seq_en()`。

### `request_scheduled_flushsb_if_due(cycle_idx)`

源码位置：`common_data_transaction.sv:1533`

功能：当前 cycle 到达 scheduled cycle 时调用 `request_flushsb()`，并把 scheduled 标记为 issued。

输入：当前 cycle index。

输出：bit，表示本次是否触发了 scheduled flushSb。

主要调用方：`memblock_lsqcommit_dispatch_sequence` 每拍服务 commit/flushSb 时调用。

内部依赖：调用 `request_flushsb()`。

### `scheduled_flushsb_pending(cycle_idx)`

源码位置：`common_data_transaction.sv:1545`

功能：判断是否仍存在未发出的 scheduled flushSb。

输入：当前 cycle index；源码当前没有使用该参数参与比较，只返回 pending 且未 issued。

输出：bit。

主要调用方：`memblock_lsqcommit_dispatch_sequence` 判断是否需要继续工作。

内部依赖：无。

### `should_drive_flushsb()`

源码位置：`common_data_transaction.sv:1550`

功能：判断 flushSb sequence 当前是否应驱动 DUT。要求有 pending、没有正在等待 sbIsEmpty、且没有全局 flush/redirect 阻塞。

输入：无。

输出：bit。

主要调用方：`memblock_lsqcommit_dispatch_sequence::drive_flushsb_if_needed()`。

内部依赖：调用 `issue_blocked_by_global_flush()`。

### `mark_flushsb_driven(cycle)`

源码位置：`common_data_transaction.sv:1556`

功能：flushSb 已经驱动 DUT 后，清 pending，进入等待 sbIsEmpty 状态，并同步设置 `memblock_sync_pkg::dispatch_flushsb_waiting_empty=1`。

输入：当前 service cycle。

输出：无。

主要调用方：`memblock_lsqcommit_dispatch_sequence` 驱动 flushSb 后调用。

内部依赖：无。

### `update_sb_is_empty(sb_is_empty)`

源码位置：`common_data_transaction.sv:1564`

功能：由 ctrl monitor adapter 更新 DUT `sbIsEmpty` 状态；当正在等待 flushSb empty 且 `sb_is_empty=1` 时，退出等待。

输入：DUT output `sbIsEmpty`。

输出：无。

主要调用方：`dispatch_monitor_event_adapter::apply_raw_ctrl_deq()`。

内部依赖：写 `memblock_sync_pkg::dispatch_flushsb_waiting_empty`。

### `flushsb_timed_out(timeout)`

源码位置：`common_data_transaction.sv:1572`

功能：判断 flushSb 等待 sbIsEmpty 是否超时。

输入：timeout。

输出：bit。

主要调用方：`memblock_lsqcommit_dispatch_sequence`。

内部依赖：调用 `memblock_sync_pkg::get_dispatch_service_cycle()`。

## 13. 函数调用关系中的关键设计点

### 13.1 uid 是公共主键，ROB/LQ/SQ/TLB 都只是反查索引

主表生成时通过 `alloc_uid()` 分配 uid；LSQ 入队成功后 `activate_uid()` 建立 active ROB/LQ/SQ map；monitor 回来时 `resolve_uid_for_event()` 再把 ROB/LQ/SQ key 反查成 uid。这样做的好处是状态表只需要按 uid 管理，不需要在多个表里重复保存完整 transaction。

### 13.2 active 不能直接改

`set_status_field()` 明确禁止更新 `MEMBLOCK_STATUS_ACTIVE`。原因是 active 状态不仅是一个 bit，还绑定了 `uid_by_active_rob/uid_by_lq/uid_by_sq` 三个反查表。必须通过 `activate_uid()` 和 `retire_active_uid()` 成对维护，才能避免 monitor 反查到 stale uid。

### 13.3 target 级 issue_epoch 和 replay_seq 用来过滤迟到反馈

`mark_issue_snapshot()` 在发射成功时记录对应 target 的 issue epoch；`mark_replay_pending()` 会 bump replay seq。后续 `conditional_set_target_status_field()`、`normalize_feedback_event()` 会用这两个值判断 event 是否还属于当前发射实例。这个机制主要防 replay 后同一 uid 仍 active 时，旧发射 attempt 的迟到 writeback/pass/replay feedback 误更新新 replay 轮次的状态。redirect 后旧反馈通常还能被 active map 或 inactive status 挡住；replay 后不能只靠 uid 区分。

### 13.4 redirect 必须先冻结、再驱动、再清状态

`request_redirect_flush()` 只负责进入 freeze 和记录 active redirect；`push_redirect_drive()` 让 redirect sequence 真实驱动 DUT input；`mark_redirect_drive_done()` 表示 DUT input 已经驱动；`apply_redirect_flush()` 最后清软件状态。这个顺序避免了“TB 先清状态，但 DUT 还没看到 redirect”的状态分叉。

### 13.5 flushSb 是 commit sequence 与 ctrl monitor 的协作

flushSb 请求由 `request_flushsb()` 或 scheduled 触发；commit sequence 通过 `should_drive_flushsb()` 决定是否驱动；驱动后 `mark_flushsb_driven()` 进入等待；ctrl monitor 采到 `sbIsEmpty` 后经 `update_sb_is_empty()` 解除等待。

## 14. 当前未定义 task

本文件没有 `task` 定义。所有跨拍、等待和驱动行为都放在 sequence 或 handler 中实现；`common_data_transaction` 只提供无时间推进的 function，用于维护公共表和状态。
