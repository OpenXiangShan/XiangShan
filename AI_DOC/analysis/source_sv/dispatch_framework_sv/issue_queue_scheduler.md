# issue_queue_scheduler.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`

## 1. 文件定位与使用场景

轻量发射队列路由与选择器。它存在的原因是 LSQ 入队和 TLB 映射完成后，操作不一定立刻能发射，还要根据 target、delay、replay、send priority、pipe 数选择每拍要发哪些。

输入是主表、status 表、LSQ behavior 和三类 issue queue 当前内容。输出是 `memblock_issue_q_item_t` 候选数组，给 `memblock_lintsissue_dispatch_sequence` 填 xaction；发射成功后它再更新 status 的 dispatched/epoch/replay mask。

控制逻辑字段包括 queue item 的 `target/send_pri/ready_cycle/replay_seq/uop_index/uop_count` 以及 status 的 queued/dispatched/pass/replay 字段。route 扫描边界统一来自 `common_data_transaction.dispatch_progress`，ROB/LQ/SQ key 同时是 payload 和年龄/一致性判断依据。

函数：

- `make_issue_item(uid,target,behavior)`：从主表/status 生成 queue item；STD 使用 `send_pri_std`。
- `is_uid_route_ready(uid)`：active、enq、issue_ready 且无 flush/redirect/exception；replay 时要求 replay pending，并由 replay target mask 限定目标队列。`issue_ready` 表示 LSQ admission 后允许进入 issue queue，不等同于真实 L2TLB 映射完成。
- `prepare_issue_route_for_uid(uid)`：公共 admission 后处理入口，检查 uid 已 active/enq，置 `MEMBLOCK_STATUS_ISSUE_READY`，然后调用 `route_uid(uid)`。真实 LSQ admission 和 soft test admission 都通过该逻辑完成 issue route 准备。
- `target_already_queued_or_done(status,target)`：避免重复入队。
- `route_target()`、`route_uid()`、`route_all_ready_uids()`：从公共连续 terminal_done 前缀和 admission 高水位形成的窗口扫描 uid，并将 ready uid 放入 LOAD/STA/STD queue。
- `advance_issue_queue_delays()`：每拍递减 `ready_cycle`。
- `is_issue_item_eligible(item)`：检查 active、enq、tlb、target-aware replay seq、delay、target 状态；已经在队列中的非 replay sibling target 不会因为同 uid 进入 replay pending 而被挡住。
- `item_is_older()`：同 ROB key 按 uid，小于为老；否则用 ROB 环形比较。
- `item_is_better(candidate,best,compare_pri)`：send priority 模式先比较优先级，相同再按年龄。
- `find_global_max_send_pri()`：跨三队列找全局最高 eligible priority。
- `select_target_candidates()`、`select_issue_candidates()`：按每类 pipe 采样数选择候选。`send_pri_mode_en=1` 时队列内比较 priority；当 `sample_global_send_pri_en()` 采样为 1 且找到全局最大 priority 时，只选全局最高优先级的项。
- `mark_issue_fire(item)`：分配 issue epoch，删除队列项，清 queued，置 dispatched，清 replay target mask。
- `mark_issue_fire_already_accepted(item)`：redirect/flush 已经开始但 driver 确认某个端口已经 ready/fire 时使用；它绕过普通全局 flush 阻塞，只给已被 DUT 接受的项补记 dispatch，避免重复发射。

## 2. 字段与函数/task 设计原理

`issue_queue_scheduler` 只负责“谁可以进入发射队列、这一拍从队列里选谁、发射成功后状态怎么改”。它不关心具体 lintsissue payload 字段，字段赋值交给 `issue_field_assigner`。

主要数据来自 `common_data_transaction` 的三条队列：

- `load_issue_q`：load target 队列。
- `sta_issue_q`：store address target 队列。
- `std_issue_q`：store data target 队列。

队列元素是 `memblock_issue_q_item_t` 轻量结构，保存 `uid/rob_key/target/send_pri/ready_cycle/replay_seq/lq_key/sq_key/numLsElem/uop_index/uop_count`。这样队列删除和重发只操作轻量 item，完整 transaction 仍通过 uid 查主表和状态表。

`uop_index` 和 `uop_count` 的当前语义需要区分：

- `uop_index` 在 `make_issue_item()` 中先初始化为 0，不代表主表里的真实 micro-op 编号。真实发射前，`memblock_lintsissue_dispatch_sequence::assign_issue_items()` 会把本 target 候选数组下标 `idx` 写入 `fired_item.uop_index`。因此当前它主要表示“本次发射放到了该 target 的第几个 pipe/port”：LOAD 0/1/2 对应 `intIssue_0/1/2`，STA 0/1 对应 `intIssue_3/4`，STD 0/1 对应 `intIssue_5/6`。`mark_fired_items()` 再用它映射 `fired_mask`。
- `uop_count` 在普通标量 load/store 下为 1。atomic/AMOCAS 路径会根据 `lsq_ctrl_model::derive_op_behavior()` 写成 `atomic_sta_uop_count` 或 `atomic_data_uop_count`，表示理论上该 target 需要几个地址侧/数据侧 uop。但当前 issue queue 没有把一个 item 展开成多个 queue item，也没有按 `uop_count` 多次填 payload；所以它是 atomic 多 uop 的记录/预留字段，不是完整多 uop 发射实现。

端口选择不是随机的。`select_target_candidates()` 先按优先级/ROB 年龄选出每个 target 的候选数组，然后 `assign_issue_items()` 按数组顺序映射端口：第 0 个 load 候选进 load port0，第 1 个进 load port1，以此类推。driver 等待各端口 ready，已经 fire 的端口写入 `fired_mask`；未 fire 的端口不会从队列删除。

route 不再保存本地完成前缀。`route_all_ready_uids()` 每拍调用 `data.advance_terminal_done_uid()`，随后扫描 `[data.get_active_scan_begin_uid(), data.get_active_scan_end_uid())`，并用 `seq_csr_common::get_real_lsq_enq_max()` 限制每拍最多扫描的 uid 数。这样 10 万笔请求下不会每拍从 0 全表扫描，扫描窗口默认 8，对应当前 8-wide LSQ enqueue 总 slot 配置。

主要函数：

| 函数 | 参数 | 功能和设计原理 |
|---|---|---|
| `ensure_data()` | 无 | 延迟绑定公共 owner，防止构造顺序导致 null。 |
| `make_empty_item()` | 无 | 返回安全空 item，所有字段清零，避免选项残留。 |
| `make_issue_item(uid,target,behavior)` | uid、target、op behavior | 从主表/状态表提取发射需要的轻量字段；STD 使用 `send_pri_std`，其它 target 使用 `send_pri`。其中 `uop_index` 只是初始化为 0，真正发射端口编号由 `assign_issue_items()` 在本拍按候选数组下标覆盖；`uop_count` 普通标量为 1，atomic 根据 behavior 写入理论 uop 数。 |
| `is_uid_route_ready(uid)` | uid | active、enq、TLB mapped 且没有 flush/redirect/exception 时才允许入队；replay pending 时继续允许路由，但只把 replay target mask 指定的目标重新入队。 |
| `target_already_queued_or_done(status,target)` | 状态、target | 防止同一 target 重复进入队列；load 若已全局 pass/writeback 也不再入队。 |
| `set_target_queued(uid,target,value)`、`set_target_dispatched(uid,target,value)` | uid、target、值 | 统一更新状态表对应 target 位，避免调用方直接写字段。 |
| `route_target(uid,target,behavior)` | uid、target、behavior | 生成 issue item 并压入对应队列；replay 时只 route 被 replay mask 指定的 target，入队前先清同 uid/target 的旧 pending 项。 |
| `route_uid(uid)`、`route_all_ready_uids()` | uid/无 | `route_uid()` 根据 `derive_op_behavior()` 拆到 LOAD/STA/STD 队列；`route_all_ready_uids()` 每拍从公共 active scan begin 到 end 的右开窗口内最多扫描 `seq_csr_common::get_real_lsq_enq_max()` 个 uid，避免大表场景 O(main_trans_num) 扫描。 |
| `advance_issue_queue_delays()` | 无 | 每拍递减队列项 ready delay，直到 0 才可选中。 |
| `is_issue_item_eligible(item)` | issue item | 发射前再次检查 active、enq、TLB、flush、redirect、exception、issue_killed、delay 和 target 状态。LOAD/STA 队列项必须匹配当前 replay_seq；STD 队列项不因同 uid 的 STA replay bump replay_seq 而失效，也不因 STA replay pending 被挡住。这样 store 的 STA 需要重发时，已经排队的 STD 仍可按自身状态继续发射；但 STD 仍受 `std_dispatched`、target issue_epoch 后续反馈检查和全局 flush/redirect 保护。 |
| `item_is_older(left,right)` | 两个 item | ROB 年龄相同时用 uid tie-break，保证确定性。 |
| `item_is_better(candidate,best,compare_pri)` | 候选和当前最佳 | 启用 send priority 时先比优先级，否则按 ROB 年龄；优先级相等仍按年龄。 |
| `find_global_max_send_pri(max_pri)` | 输出最大优先级 | global 模式采样命中时跨 LOAD/STA/STD 找全局最大优先级；找不到 eligible item 时调度退化为 non-global。 |
| `select_target_candidates(target,max_count,compare_pri,use_global_pri,global_pri,selected)` | target、数量、是否比较 priority、是否全局过滤、输出队列 | 从单个 target 队列选最多 `max_count` 个合法 item；`compare_pri=1` 时先按 priority 再按年龄，`use_global_pri=1` 时先过滤非全局最大 priority。 |
| `select_issue_candidates(load_items,sta_items,std_items)` | 三个输出队列 | 每拍总调度入口；flush 中只置 `issue_freeze_ack` 不发射；否则采样 pipe 数和 global 模式，按三模式选择三类候选。 |
| `mark_issue_fire(item)` | 已发射 item | 分配 issue epoch，删除队列项，清 queued，置 dispatched，清 replay target mask。只有 mark fire 后才认为真正发到 DUT。 |
| `mark_issue_fire_already_accepted(item)` | driver 已确认 ready/fire 的 item | redirect 中止边界专用。即使全局 flush 已开始，也允许把已被 DUT 接受的端口补记为 dispatched；未被 fire mask 覆盖的 item 不会调用该函数。 |

## 3. 当前支持边界

当前 issue queue/scheduler 主路径完整覆盖的是标量 load、标量 store 的 LOAD/STA/STD 发射、replay_seq 过滤、send priority 和 flush/redirect 边界处理。以下能力是明确不支持或仅简化支持：

- vector LS 不支持。`lsq_ctrl_model::derive_op_behavior()` 遇到 vector LS fuType 会 fatal；vector IQ feedback 在 adapter 中 drop，vector writeback 在 `writeback_status_handler` 中 fatal。
- atomic/MOU 是简化支持，不是完整 atomic 协议。当前 MOU AMO 不分配普通 LQ/SQ，route 到 STA/STD，并记录 `atomic_sta_uop_count/atomic_data_uop_count`；但 issue queue 不会按 `uop_count` 展开多个 item，`MEMBLOCK_WB_EVENT_SOURCE_ATOMIC_WB` 仍是 reserved，真实 atomic writeback/AMOCAS 多 uop 闭环还未完整实现。
- 多元素 LSQ 范围映射不完整。当前 active map 主要保存 base LQ/SQ key，vector/multi-element 的范围分配、逐元素 deq 和 event 反查没有完整实现。
- STD backend replay 不支持。STA miss 可转 replay；STD miss 在 `dispatch_monitor_event_adapter` 中 warning/drop，`mark_replay_pending()` 对 STD replay 也不会进入重发流程。
- 非 LDU/STU/MOU 的 fuType、以及 LDU/STU/MOU 下非法 fuOpType 会 fatal；当前框架没有通用未知 FU 的 fallback。
- CBO/prefetch 目前按 load/store-like 路径建模，特殊完成语义和异常专项覆盖不是完整闭环。
