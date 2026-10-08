# 更老 Redirect 冲刷时清除 Pending Fault 草稿与方案 A 旧发射项

## 1. 方案目标与术语

### 1.1 目标

本方案修正更老 redirect 作废旧动态实例时的两处清理缺口，二者互不扩大对方的入口：

1. scalar store 在 STA/STD 双 real-WB 之前，异常只记录在 `pending_store_fault_by_uid`。更老 redirect 冲掉这个尚未 `rob_commit` 的 UID 时，普通 reissue 会作废旧动态实例并重新入队，但没有同步删除这张草稿表。
2. 双 real-WB 已完成、正式 fault 位已置位、尚未 `rob_commit` 的 UID 进入方案 A 时，active 分支只释放活映射。`reset_uid_after_older_redirect()` 会把 `queued_*` 清成 0，但 LOAD 和 STA 的旧发射项还留在队列里。

双 real-WB 之前不把该 UID 认定成正式 fault，这个规则保持不变。本方案不提前置正式 fault 状态，不让该 UID 发出自己的异常 redirect，也不改变 admission 水位。方案 A 的入口条件也不扩大。

### 1.2 术语与抽象功能说明

| 术语 | 当前含义 | 代码对象 | 示例 |
| --- | --- | --- | --- |
| `pending fault 草稿` | 双 real-WB 前按 UID 暂存的 store 异常证据。 | `pending_store_fault_by_uid` | STA fault 已见、STD real-WB 未到。 |
| `正式 fault 状态` | 双 real-WB 后才允许进入 fault commit 的 UID 级状态。 | `status.fault`、`exception_pending`、`sta_fault`、`std_fault` | 只由 `finalize_pending_store_fault()` 写入。 |
| `普通 reissue` | 未命中方案 A 时，flush 范围内 UID 的既有取消和重新入队路径。 | `prepare_uid_for_redirect_reissue()` 后半段 | `retire_active_uid()` 后 `dynamic_epoch++`。 |
| `方案 A` | 正式 fault 位已置位且尚未 `rob_commit` 时，完整重置旧动态实例。 | `reset_uid_after_older_redirect()` | 本方案不扩大其入口，只补齐重置前的队列删除。 |
| `issue item` | 等待发射的 LOAD、STA 或 STD 队列元素。 | `load_issue_q`、`sta_issue_q`、`std_issue_q` | 方案 A 重置前必须按 UID 全部删除。 |
| `admission 水位` | 下一次新 admission 的 UID 上界。 | `rollback_max_enqueued_uid()`、`get_next_new_admit_uid()` | 更老 redirect 仍从被冲掉的 UID 重新入队。 |

抽象功能说明：草稿只服务“双 real-WB 尚未齐、还不能正式 fault”的窗口。旧动态实例一旦被 redirect 作废，草稿必须在同一次清理中删除。后续清扫或新 capture 覆盖不能代替这次同步删除。

正式 fault 进入方案 A 时，重置旧实例之前还必须删除该 UID 的全部旧发射项。`queued_*` 被清零不能代替队列元素删除。

## 2. 当前行为与问题

### 2.1 双 real-WB 前不正式 fault 是正确行为

`mark_target_fault()` 对 scalar store 只置对应 target 的 real-WB，并调用 `capture_pending_store_fault()`。`finalize_pending_store_fault()` 在 `sta_writeback && std_writeback` 之前返回，因此以下四位保持 0：

- `status.fault`
- `status.exception_pending`
- `status.sta_fault`
- `status.std_fault`

STA-only 与 STD-only 都走这条路径。late STA tombstone 还会主动清掉上述正式状态位。这样 ROB 不会提前 fault commit，该 UID 也不会发出自己的异常 redirect。本方案不修改这段判定。

### 2.2 更老 redirect 漏删草稿

`prepare_uid_for_redirect_reissue()` 只用四个正式状态位选择方案 A：

```text
(status.fault || status.exception_pending || status.sta_fault || status.std_fault)
    && !status.rob_commit
```

双 real-WB 前条件为假，不会调用 `reset_uid_after_older_redirect()`，也不会打印 `reset older-redirect pending-fault`。

随后的普通 reissue 仍然：

1. `retire_active_uid()` 释放 ROB/LQ/SQ 映射，并删除该 UID 的全部 issue item；
2. `clear_uid_dispatch_result()` 清除发射、写回和正式 fault 状态；
3. `dynamic_epoch` 增加，置位 `redirect_pending` 与 `flushed`。

`apply_redirect_flush_range()` 只要该 UID 落在 flush 范围，仍调用 `rollback_max_enqueued_uid(oldest_flushed_uid)`。水位没有算错，该 UID 仍会重新入队。

缺口只在草稿表。`clear_uid_dispatch_result()` 不调用 `clear_pending_store_fault()`。`pending_store_fault_by_uid` 的注释要求动态实例失效时清除，当前普通 reissue 没有遵守。

### 2.3 残留草稿不会立即变成新 fault

`pending_store_fault_matches()` 要求 UID 仍 active，且记录中的 `dynamic_epoch` 等于当前 epoch。redirect 后 active 为 0、epoch 已增加，旧记录不匹配。下一次 `finalize_pending_store_faults()` 会删除不匹配记录；新实例再次 fault 时，新 capture 也会覆盖旧记录。

因此当前后果不是漏重发，也不是旧异常立即被 commit。后果是旧异常证据比旧动态实例多存活到下一次清扫。这个窗口必须在 redirect 清理当时关闭。

### 2.4 已提交 fault head 不属于本问题

扫描先识别已 `rob_commit`、尚未 `terminal_done`、ROB 位置等于 redirect head 且正式 fault 位已置位的 UID，把它交给 `preserve_fault_uid_during_redirect()`。该 UID 不进入 `prepare_uid_for_redirect_reissue()`，也不进入 `oldest_flushed_uid`。它自己的异常 redirect 从下一条 UID 重新 admission。本方案不改变这条路径。

### 2.5 方案 A 的 active 分支漏删发射队列

四个正式 fault 位已经置位且尚未 `rob_commit` 时，`prepare_uid_for_redirect_reissue()` 进入方案 A。UID 仍 active 时目前只做：

1. `release_uid_lq_mapping()` 释放 LQ 映射，不删除发射项；
2. `release_uid_sq_mapping()` 释放 SQ 映射，并且只在 SQ 仍有映射时删除 STD 发射项；
3. 删除 ROB 活映射，并把 `active` 置 0；
4. `reset_uid_after_older_redirect()` 清除正式 fault 状态和 `queued_load`、`queued_sta`、`queued_std`，然后递增 `dynamic_epoch`。

inactive 分支会调用 `remove_uid_from_issue_queues()`。active 分支不会。同一次 flush 扫描开始时该 UID 仍是 active，扫描前段对 inactive UID 的队列清理也不会再覆盖到它。

因此 reset 之后记账位已经表示队列为空，LOAD 和 STA 的旧元素还在。SQ 当时已经没有映射时，STD 旧元素也会留下。`release_uid_sq_mapping()` 对 STD 的删除不能代替三个队列的统一删除。

这些残留元素有两种后果：

- `has_pending_issue_work()` 只看三个队列是否非空。旧元素已经不能发射，issue 循环仍把它们当成 pending work。
- 新实例重新 admission 并把 `enq` 从 0 置 1 时，会清掉 `redirect_pending`、`flushed` 和 `issue_killed`。普通 STA、LOAD eligibility 不比较发射项中的 `dynamic_epoch`。重新 route 之前，旧 STA 或 LOAD 元素可能按新实例被发射。`route_target()` 入队前会删除同 UID 的旧元素，但这只在它先于 issue 仲裁执行时成立。

`retire_active_uid()` 虽然会删除三个队列，但不能替换方案 A 的 active 分支。它只接受 active UID；对已经发出 STD 的正式 fault，还会调用 `capture_std_late_raw_tombstone()` 留下迟到 raw 身份。方案 A 是更老 redirect 作废尚未提交的实例，不需要这份身份。它也不负责正式 fault 清理和 `dynamic_epoch` 递增。真正冻结 redirect owner 的 `capture_redirect_deleted_owner()` 已在进入分支前调用，active 分支继续只释放活映射，避免对同一代 LQ、SQ 键重复 capture。

## 3. 代码修改方案

### 3.1 在旧实例清理入口删除草稿

抽象功能描述：凡是通过 `clear_uid_dispatch_result()` 作废当前动态实例的 dispatch 结果，都同时删除该 UID 的 pending fault 草稿。

修改位置：`common_data_transaction::clear_uid_dispatch_result()` 开头。

修改要求：

- 调用已有的 `clear_pending_store_fault(uid)`；
- 不新增第二套删除逻辑；
- 不改变该函数现有的 status、epoch、queue 和 mapping 清理顺序。

该函数当前只有两个调用方，都是在作废旧动态实例：

- `prepare_uid_for_redirect_reissue()` 的普通 reissue 路径；
- `reset_uid_after_older_redirect()`。

因此双 real-WB 前的 pending 窗口会在普通 reissue 中同步删草稿。方案 A 原有的 `pending_store_fault_by_uid.delete(uid)` 保留，重复删除无副作用。

伪代码：

```text
clear_uid_dispatch_result(uid):
    clear_pending_store_fault(uid);
    清除原有 dispatch、issue、writeback 和正式 fault 状态；
```

### 3.2 日志

只在本次确实删除了一条草稿时打印一条新日志，建议 ID 为 `PENDING_FAULT_DRAFT`，文本明确为 `drop pending store fault draft on redirect reissue`，并带上 UID 与被作废实例的 `dynamic_epoch`。

不要复用 `reset older-redirect pending-fault`。那条日志表示进入了方案 A。

实现时可让 `clear_pending_store_fault()` 返回是否删除了记录，由普通 reissue 路径在 epoch 增加前打印。方案 A 路径继续使用自己的现有日志，不为同一次重置重复打印草稿日志。

### 3.3 方案 A 重置前删除全部旧发射项

抽象功能描述：正式 fault 且尚未 `rob_commit` 的 UID 进入方案 A 后，无论当前是否 active，都先删除该 UID 的全部旧发射项，再重置旧动态实例。

修改位置：`common_data_transaction::prepare_uid_for_redirect_reissue()` 的方案 A 分支。

修改要求：

- active 时保持现有的 `release_uid_lq_mapping()`、`release_uid_sq_mapping()` 和 ROB 活映射删除，不改为 `retire_active_uid()`；
- 删除原来只在 inactive 时调用 `remove_uid_from_issue_queues()` 的 `else` 分支；
- 在 `reset_uid_after_older_redirect()` 之前，无条件调用一次 `remove_uid_from_issue_queues(uid)`；
- 不修改方案 A 的入口条件。

`remove_uid_from_issue_queues()` 按 UID 删除 LOAD、STA、STD，并把三个 `queued_*` 置 0。`release_uid_sq_mapping()` 可能已经删除 STD，重复删除无副作用。

伪代码：

```text
正式 fault 且尚未 rob_commit:
    如果 active:
        释放 LQ、SQ 和 ROB 活映射；
        active = 0；
    remove_uid_from_issue_queues(uid);
    reset_uid_after_older_redirect(uid);
```

## 4. 明确不修改

- 不把 `pending_store_fault_matches(uid)` 加入方案 A 入口。双 real-WB 前继续走普通 reissue，草稿由 `clear_uid_dispatch_result()` 删除；只有正式 fault 位进入方案 A。
- 不在 `capture_pending_store_fault()` 或 late STA tombstone 路径提前置 `status.fault`、`exception_pending`、`sta_fault`、`std_fault`。
- 不修改 `finalize_pending_store_fault()` 的双 real-WB 门控。
- 不把方案 A 的 active 分支改成 `retire_active_uid()`。
- 不修改 `rollback_max_enqueued_uid()`、`preserve_fault_uid_during_redirect()` 和 pendingPtr 双 real-WB 门控。
- 不删除 `UID31_STD_TRACE`，也不把该调试打印纳入本方案。

## 5. 修改后的行为

STA 已见 fault、STD real-WB 尚未到达时，更老 redirect 覆盖该 UID：

```text
四个正式 fault 位仍为 0
  -> 不进入方案 A
  -> 不产生该 UID 自己的异常 redirect
  -> retire 旧映射和全部 issue item
  -> 同步删除 pending_store_fault_by_uid[uid]
  -> dynamic_epoch 增加
  -> admission 水位退回该 UID
  -> 新实例重新发射 STA 和 STD
```

STD-only pending fault 使用同一清理。已 `rob_commit` 的 fault head 仍从下一条 UID 重新 admission。

双 real-WB 已完成、尚未 `rob_commit` 的正式 fault 被更老 redirect 覆盖时：

```text
进入方案 A
  -> 释放活映射，redirect owner 只冻结一次
  -> 删除该 UID 的 LOAD、STA、STD 旧发射项，queued_* 为 0
  -> reset：清除正式 fault，dynamic_epoch 加一
  -> admission 水位退回该 UID
  -> 新实例重新 route 后才产生新发射项
```

## 6. 验证

定向场景至少覆盖：

1. scalar store 的 STA fault real-WB 已记录、STD real-WB 未到，更老 redirect 覆盖该 UID。期望草稿在本次 reissue 中删除，正式 fault 位保持 0，`dynamic_epoch` 加一，同一 UID 重新入队并发射 STA 与 STD。
2. 同一窗口换成 STD fault real-WB 已记录、STA real-WB 未到。期望与第 1 条相同。
3. 双 real-WB 已完成且尚未 `rob_commit`，STA 或 LOAD 仍在发射队列中，更老 redirect 覆盖该 UID。期望仍进入 `reset_uid_after_older_redirect()`，且重置前三个队列都不再有该 UID 的旧项，`queued_*` 为 0，`dynamic_epoch` 加一。同一 UID 重新入队后，重新 route 才产生新的 STA 和 STD 项。
4. 已 `rob_commit` 的 fault head 自己产生异常 redirect。期望该 UID 不重发，水位从下一条 UID 开始。

既有指定 seed 只证明提交后的 fault redirect 和全局 stop，不能替代第 1、2、3 条定向场景。
