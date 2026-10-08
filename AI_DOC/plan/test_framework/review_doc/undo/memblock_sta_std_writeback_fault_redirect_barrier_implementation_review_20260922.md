# STA/STD 双真实写回异常屏障 Implementation Review

关联 plan：`AI_DOC/plan/test_framework/plan/undo/memblock_sta_std_writeback_fault_redirect_barrier_plan_20260922.md`。

## 1. Review 范围、术语与抽象功能说明

| 英文术语 | 当前含义 | 代码落点 | 示例 |
| --- | --- | --- | --- |
| `real writeback` | 当前 issue epoch 的 DUT STA/STD 完成事件，不等同 IQ feedback。 | `sta_writeback`、`std_writeback` | STA fault raw 计为 STA WB。 |
| `pending fault` | 任一 store target 已见异常、但尚未同时获得 STA/STD WB 的暂存状态。 | `pending_store_fault_by_uid` | STA fault 先到时等待 STD。 |
| `barrier STD` | 在 pending fault 下唯一尚未 WB、必须继续完成的 STD queue item。 | `std_issue_q` | SQ deq 先于 STD WB。 |
| `live SQ owner` | 当前动态实例对物理 SQ key 的可复用映射。 | `uid_by_sq`、`active_sq_mapped` | SQ deq 后该 map 删除。 |
| `dynamic epoch` | 同一 UID 每次 redirect/reissue 的动态实例版本。 | `status.dynamic_epoch` | 旧 epoch 不能消费新 instance WB。 |

抽象功能说明：本 review 只复查本次测试框架改动。异常证据、commit/terminal 和 redirect 仍各由原有 owner
维护；新增状态不建立第二套 ROB、SQ 或 redirect 生命周期。

## 2. 结论

初始 UID31 死锁已定位为 SQ deq 删除了 pending fault 的唯一 STD，随后 scheduler 因无 live SQ owner 拒绝该 STD。
最终实现以有限例外保留并完成该 STD，之后才允许 fault finalize、commit、terminal 与 redirect。该例外有 UID、
dynamic epoch、target、WB 状态和保存 SQ key 的多重边界，不会使普通 stale STD 在无 owner 时发射。

## 3. 状态屏障与 commit 门控

### 3.1 `common_data_transaction::capture_pending_store_fault()` / `finalize_pending_store_fault()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。

抽象功能描述：前者收集当前动态实例的 STA/STD fault 证据；后者仅在两个 real WB 均完成后写 UID 级 fault，
不拥有 issue queue、LSQ map、ROB commit 或 redirect。

```systemverilog
if (!status.sta_writeback || !status.std_writeback) begin
    return 1'b0;
end
status.fault = 1'b1;
status.exception_pending = 1'b1;
quiesce_fault_uid_pending_work(uid, 1'b0);
clear_pending_store_fault(uid);
```

中文伪代码：先读取当前 active dynamic instance 的两个真实写回位；只要有一个未到达，就保留 pending record 且不改变
UID fault。两个都到达后才设置 fault/exception 状态，清理除必要 STD 外的等待工作，最后删除 pending record，避免重复
finalize。

正确性检查：`writeback_status_handler` 在 pending fault 存在时仍记录 STD normal WB；
`lsq_commit_handler` 的 candidate、fault commit 和 terminal/redirect 均再次调用
`store_real_writebacks_done()`，不能由 IQ feedback 或单 target fault 越过屏障。

## 4. SQ deq 后的唯一 barrier STD

### 4.1 `common_data_transaction::release_uid_sq_mapping()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。

抽象功能描述：函数释放 live SQ map。通常删除未 fire 的 STD item；仅在当前 UID 的 pending fault 仍等待 STD WB 时，
保留已有 item 和历史 key，让该动态实例完成第二个 WB。

```systemverilog
if (!(pending_store_fault_matches(uid) && !status.std_writeback)) begin
    delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_STD, uid, 0, 1'b0);
    status.queued_std = 1'b0;
end
uid_by_sq.delete(sq_map_key);
status.active_sq_mapped = 1'b0;
```

中文伪代码：先判断 pending record 是否属于当前 dynamic epoch，且 STD 尚未 writeback；不满足时维持旧行为，删除未 fire
STD 并清零 `queued_std`。满足时跳过删除，只释放可复用 live map。无论哪条分支，之后都删除 `uid_by_sq` 和
`active_sq_mapped`，所以历史 key 不会变成新的物理 owner。

### 4.2 `issue_queue_scheduler`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv`，函数：
`make_issue_item()`、`route_target()`、`is_issue_item_state_eligible()`。

抽象功能描述：scheduler 默认以 live SQ owner 作为 STA/STD 的可发射前提；SQ deq 后不允许重新 route/new item，
它仅对 pending fault、SQ 已 deq、STA 已 WB、STD 未 WB 的既有 STD 接受保存 key，完成一次 issue。

```systemverilog
std_waits_fault_barrier = item.target == MEMBLOCK_ISSUE_TARGET_STD &&
                          data.pending_store_fault_matches(item.uid) &&
                          status.sta_writeback && !status.std_writeback;
...
(status.active_sq_mapped || std_waits_fault_barrier)
```

中文伪代码：先确认 item 是 STD，pending record 与当前 instance 匹配，并且 STA 已完成、STD 未完成；只有该条件成立时，
可用保存的 SQ key 替代 live map。随后仍检查当前 dynamic epoch、当前 ROB key、active/enq/issue-ready、非 flush、
非 redirect、未 dispatched、`queued_std`、未 writeback、key 完整一致。其它 target 或其它 STD 任一检查失败即拒绝。

## 5. WB monitor 的历史 key 归属

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，函数：
`fill_current_issue_snapshot()`；`common_data_transaction::resolve_uid_for_event()` 与
`normalize_feedback_event()`。

抽象功能描述：这些函数负责将 raw STD WB 附着到当前 instance。SQ deq 已删除 live map 时，只针对 barrier STD 使用
ROB、issue epoch、flush epoch 和 status 保存 SQ key 进行归属；不会接受普通无 owner store event。

```systemverilog
if (wb_event.target != MEMBLOCK_ISSUE_TARGET_STD || !status.lsq_deq ||
    !data.pending_store_fault_matches(uid) || !status.sta_writeback ||
    status.std_writeback) begin
    return 1'b0;
end
```

中文伪代码：当 active SQ map 不存在时，先拒绝 STA、非 deq、无 pending fault、STA 未 WB 或 STD 已 WB 的 event；
只有唯一合法 STD 才附着保存 SQ key 和当前 issue provenance。后续 writeback handler 因此能记录 STD WB，触发
`finalize_pending_store_fault()`。

## 6. 与 plan 对齐及验证

`IMPLEMENTATION_DELTA 8.5` 记录了执行中发现的唯一必要扩展：SQ deq 早于 STD real-WB 时保留 barrier STD。
先前“无条件删除 STD”已被明确替换，不再作为最终方案。独立复查进一步确认 scheduler 必须同时校验
`lsq_deq`、STA WB、dynamic epoch 和 ROB key；该边界已补入实现。redirect sideband 保持已有单飞仲裁，未新增
generation 表或第二套 fault token，且不作为 UID31 根因。

独立 review 结论：`REVIEW_PASS`。三处最小修复的 owner 保留、scheduler eligibility 和历史 SQ key 归属边界均无 blocker。
特别确认普通无 owner STD、跨 UID/旧 epoch item、SQ key 被新 UID 复用时均不会被放行。剩余测试缺口是需要单独构造
SQ deq 早于 STD WB、key 复用和 SQ deq 前 route 的定向回归。

静态检查：`git diff --check -- AI_DOC mem_ut/ver/ut/memblock/seq/base_seq_help` 已通过。

远端编译：VCS/Verdi 报告 `0 error(s), 0 warning(s)`。

指定运行的已确认关键日志：

```text
9337.8ns  SQ deq before STD real-WB uid=31
9362.8ns  finalize store fault uid=31 sta_wb=1 std_wb=1
9370.3ns  request scalar fault redirect uid=31
```

这证明原 UID31 barrier 死锁已解除。最新代码已完成完整 10000 transaction 仿真，仿真结束时间
`2454647.800ns`；global stop 在 `2454617.8ns` 提交，最终 `UVM_ERROR : 0`、`UVM_FATAL : 0`，并正常 `$finish`。
日志没有单独的 `TEST CASE PASSED` 文本，但 UVM 汇总与 global stop 结果满足当前 `basicTest` 验收语义。
