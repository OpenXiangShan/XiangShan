# status_transaction.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/status_transaction.sv`

## 1. 文件定位与使用场景

这是每个 `uid` 的运行状态记录，类似一张生命周期 checklist。主表说明“这条操作是什么”，status 表说明“这条操作当前走到哪一步”。框架的绝大部分调度判断都看它。

输入来自各个阶段的 helper：LSQ 入队会置 `active/enq`，admission 完成后置 `issue_ready` 并允许 route issue queue，L2TLB responder 建/查 entry 并回填 uid record 后置 `tlb_mapped`，issue scheduler 会置 dispatched/epoch，writeback handler 会置 pass/fault/replay，commit handler 会置 `rob_commit`/deq/success。输出给 issue route、commit candidate 选择、end check 和 debug report。

控制逻辑字段包括生命周期、发射、pass、replay、redirect、active 映射和 target 级 issue epoch。异常地址、ROB/LQ/SQ 快照等字段更多是 payload/debug 信息，但也用于事件解析和一致性检查。

字段：

- 生命周期：`active`、`enq`、`issue_ready`、`tlb_mapped`、`rob_commit`、`lsq_deq`、`success`。
- 发射队列/发射完成：`queued_load/sta/std`、`load/sta/std_dispatched`。
- writeback/pass/fault：全局 `writeback/pass/fault` 和分 target 的 `load/sta/std_writeback/pass/fault`。
- replay：`replay_pending`、`replay_target_load/sta/std`、`replay_seq`。
- redirect/flush：`redirect_pending`、`flushed`、`issue_killed`、`dynamic_epoch`。
- active 映射：`active_lq_mapped`、`active_sq_mapped`。
- 索引快照：`robIdx_*`、`lqIdx_*`、`sqIdx_*`。
- target 级 issue epoch：`load_issue_epoch`、`sta_issue_epoch`、`std_issue_epoch`。
- 动态实例版本：`dynamic_epoch`。
- 异常信息：`exception_vec`、`exception_vaddr`、`exception_gpaddr`、`last_event_cycle`。

函数：

- `new(name)`：构造后 `reset(0)`。
- `reset(uid_i)`：清所有状态并绑定 uid。
- `snapshot_from_main(tr)`：从主表复制 ROB/LQ/SQ。
- `get_rob_key()`：返回 ROB key。
- `get_target_issue_epoch(target)`、`set_target_issue_epoch(target, issue_epoch_i)`：分 target 管理 issue epoch。

## 2. 字段与函数/task 设计原理

`status_transaction` 是每个 uid 的运行时状态。它的字段故意按阶段拆得比较细，目的是让调度、replay、commit 和 debug 都能判断这条 transaction 卡在哪一步。

状态字段分组：

| 字段组 | 字段 | 含义和设计原理 |
|---|---|---|
| 生命周期 | `active`、`enq`、`issue_ready`、`tlb_mapped`、`rob_commit`、`lsq_deq`、`success` | 从 LSQ 入队到最终完成的主路径。`issue_ready` 表示该 uid 可以进入 issue queue；`tlb_mapped` 表示 L2TLB req 已建/查 entry 并回填 PTE。`active` 表示当前 DUT 中仍可能返回该 uid 的事件；ROB commit 后不删除主表和状态表，只释放 active map。 |
| 队列状态 | `queued_load/sta/std` | 表示 uid 已经在对应 issue queue 中，防止重复入队，也用于 replay/redirect 清理。 |
| 发射状态 | `load_dispatched/sta_dispatched/std_dispatched` | 表示对应 target 已经发往 DUT。与 queued 分开，能区分“排队等待”和“已经发射”。 |
| writeback/pass/fault | `load_writeback/sta_writeback/std_writeback`、`load_pass/sta_pass/std_pass`、`load_fault/sta_fault/std_fault`、`writeback/pass/fault` | target 级状态与全局状态分离。store 必须 STA 和 STD 都完成后才能全局 pass，避免一个 target 覆盖另一个 target。 |
| issue feedback success | `load_issue_feedback_success/sta_issue_feedback_success/std_issue_feedback_success` | 只记录 IssueQueue feedback `hit/finalSuccess` 已返回。它不等价于真实 writeback/pass；当 `MEMBLOCK_STA_REAL_WB_PASS_EN` 或 `MEMBLOCK_STD_REAL_WB_PASS_EN` 打开时，最终 pass 仍等待真实 int writeback。 |
| 异常和 replay | `exception_pending`、`replay_pending`、`replay_target_load/sta/std` | `replay_valid=1` 的 LOAD/STA replay 事件可以只重发某个 target。mask 化字段保留 target 粒度；当前真实 DUT 路径不把 STD 建模成 backend replay 来源。 |
| redirect/flush | `redirect_pending`、`flushed`、`issue_killed`、`dynamic_epoch` | redirect 后旧发射结果必须失效。`flushed` 表示当前动态实例已被 flush 覆盖，但不是终态；`redirect_pending` 表示等待同 uid 重新 admission；`dynamic_epoch` 在 reissue 准备时递增，用于区分同一 uid 的不同动态实例。 |
| DUT index 快照 | `robIdx_*`、`lqIdx_*`、`sqIdx_*` | monitor 事件可能只带 ROB/LQ/SQ，不带 uid；状态表保留快照用于反查和 debug。 |
| 防污染快照 | `load_issue_epoch`、`sta_issue_epoch`、`std_issue_epoch`、`dynamic_epoch`、`replay_seq` | 每个 target 发射时记录对应 epoch，replay 递增 seq，redirect reissue 递增 `dynamic_epoch`。LOAD/STA 的 pass/fault/replay 必须匹配 target epoch 和 replay_seq；STD 没有 MemBlock 后端 replay 通路，旧 STD pending/pass/fault 不用 replay_seq 失效，依靠 target epoch、active/flush/redirect/exception/issue_killed/std_dispatched 等条件防迟到。 |
| 异常信息 | `exception_vec`、`exception_vaddr`、`exception_gpaddr`、`last_event_cycle` | 保存最后一次异常或事件信息，便于日志和 waveform 定位。 |
| active map 标志 | `active_lq_mapped`、`active_sq_mapped` | 表示 uid 当前是否占用 LQ/SQ active map，deq 时用它决定是否可 retire。 |

主要函数：

| 函数 | 参数 | 功能和设计原理 |
|---|---|---|
| `reset(uid_i)` | uid | 重置所有运行状态并绑定 uid。所有 pending/queue/pass/fault 位都归零，避免复用对象时残留。 |
| `snapshot_from_main(tr)` | 主表 transaction | 把主表中的 ROB/LQ/SQ key 快照到状态表。这样后续主表作为只读追溯记录，状态表作为运行时反查记录。 |
| `get_rob_key()` | 无 | 返回状态表中的 ROB key，redirect/flush 和 active map 统一使用该格式。 |
| `get_target_issue_epoch(target)` | LOAD/STA/STD target | 返回 target 级发射 epoch。store 的 STA 和 STD 可以不同拍发射，所以不能只用一个全局 epoch。 |
| `set_target_issue_epoch(target, issue_epoch_i)` | target、epoch | 发射成功时记录该 target 的 epoch，后续 writeback 必须匹配。 |
