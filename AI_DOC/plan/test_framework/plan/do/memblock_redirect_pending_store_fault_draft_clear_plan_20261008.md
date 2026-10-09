# 更老 Redirect 冲刷时清除 Pending Fault 草稿与方案 A 旧发射项

## 1. 方案目标与术语

### 1.1 目标

本方案修正更老 redirect 作废旧动态实例时的两处清理缺口，二者互不扩大对方的入口：

1. scalar store 在 STA/STD 双 real-WB 之前，异常只记录在 `pending_store_fault_by_uid`。更老 redirect 冲掉这个尚未 `rob_commit` 的 UID 时，普通 reissue 会作废旧动态实例并重新入队，但没有同步删除这张草稿表。
2. 双 real-WB 已完成、正式 fault 位已置位、尚未 `rob_commit` 的 UID 进入方案 A 时，active 分支只释放活映射，inactive 分支才删除发射项。重置前改为无论是否 active 都调用 `remove_uid_from_issue_queues()`，消掉这个分支差。scalar store 在 finalize 时已经删过 LOAD 和 STA，STD 也已在 real-WB 前出队，因此到达方案 A 时三个队列通常已经为空。这里不是为了清一个仍然存在的 STA 或 LOAD 旧项。

双 real-WB 之前不把该 UID 认定成正式 fault，这个规则保持不变。本方案不提前置正式 fault 状态，不让该 UID 发出自己的异常 redirect，也不改变 admission 水位。方案 A 的入口条件也不扩大。

### 1.2 术语与抽象功能说明

| 术语 | 当前含义 | 代码对象 | 示例 |
| --- | --- | --- | --- |
| `pending fault 草稿` | 双 real-WB 前按 UID 暂存的 store 异常证据。 | `pending_store_fault_by_uid` | STA fault 已见、STD real-WB 未到。 |
| `正式 fault 状态` | 双 real-WB 后才允许进入 fault commit 的 UID 级状态。 | `status.fault`、`exception_pending`、`sta_fault`、`std_fault` | 只由 `finalize_pending_store_fault()` 写入。 |
| `普通 reissue` | 未命中方案 A 时，flush 范围内 UID 的既有取消和重新入队路径。 | `prepare_uid_for_redirect_reissue()` 后半段 | `retire_active_uid()` 后 `dynamic_epoch++`。 |
| `方案 A` | 正式 fault 位已置位且尚未 `rob_commit` 时，完整重置旧动态实例。 | `reset_uid_after_older_redirect()` | 本方案不扩大其入口，只补齐重置前的队列删除。 |
| `issue item` | 等待发射的 LOAD、STA 或 STD 队列元素。 | `load_issue_q`、`sta_issue_q`、`std_issue_q` | 方案 A 重置前按 UID 删除；正式 fault 到达时通常已经为空。 |
| `admission 水位` | 下一次新 admission 的 UID 上界。 | `rollback_max_enqueued_uid()`、`get_next_new_admit_uid()` | 更老 redirect 仍从被冲掉的 UID 重新入队。 |

抽象功能说明：草稿只服务“双 real-WB 尚未齐、还不能正式 fault”的窗口。旧动态实例一旦被 redirect 作废，草稿必须在同一次清理中删除。后续清扫或新 capture 覆盖不能代替这次同步删除。

正式 fault 进入方案 A 时，重置前仍按 UID 删除三个队列中的发射项，使 active 与 inactive 使用同一次清理。当前 scalar 正式 fault 到达该分支时，这些队列通常已经为空。

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

### 2.5 方案 A 的 active 与 inactive 分支不一致

四个正式 fault 位已经置位且尚未 `rob_commit` 时，`prepare_uid_for_redirect_reissue()` 进入方案 A。UID 仍 active 时目前只释放 LQ、SQ、ROB 活映射，不调用 `remove_uid_from_issue_queues()`。inactive 分支会调用该函数。

这不是“正式 fault 时 LOAD 或 STA 旧项仍在队列里”。`finalize_pending_store_fault()` 在置正式 fault 位时调用 `quiesce_fault_uid_pending_work(uid, 0)`，该调用删除该 UID 全部 `replay_seq` 的 LOAD 和 STA，并清掉 `queued_load`、`queued_sta`，且不删除 STD。非 scalar 的 `mark_target_fault()` 同样做这次 quiesce。`is_uid_route_ready()` 在 `exception_pending` 时不再 route 新的 LOAD 或 STA。STD 要让 `std_writeback` 为 1，必须先 fire，fire 时已经按该 `replay_seq` 出队。因此 scalar 正式 fault 到达方案 A 时，三个队列通常已经为空。

`remove_uid_from_issue_queues()` 仍然要在 reset 前无条件执行。它用 `match_replay_seq=0` 删除三个队列的全部 `replay_seq` 并清三个 `queued_*`。`release_uid_sq_mapping()` 只在 SQ 仍映射时删除 STD，不能代替这次统一删除；重复删除是空操作。本 plan 不修改 `quiesce_fault_uid_pending_work()`，也不为了制造队列残留而重新入队。

`retire_active_uid()` 不能替换 active 分支。它只接受 active UID，并对已发出的 fault STD 调用 `capture_std_late_raw_tombstone()`。方案 A 作废的是尚未提交的旧实例，不建立这份迟到 raw 身份。`capture_redirect_deleted_owner()` 已在进入分支前调用，active 分支继续只释放活映射。

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

只在普通 reissue 确实删除了一条草稿时打印。日志 ID 固定为 `PENDING_FAULT_DRAFT`，文本固定为 `drop pending store fault draft on redirect reissue`，并带上 UID 与被作废实例的 `dynamic_epoch`。不要复用 `reset older-redirect pending-fault`。

`clear_pending_store_fault()` 改为返回本次是否删除了记录，函数内部不打印。`finalize_pending_store_fault()` 和 `finalize_pending_store_faults()` 也会调用它，把日志放进该函数会把正式落表或失配清扫打成 redirect reissue。

`clear_uid_dispatch_result()` 在开头调用 `clear_pending_store_fault()`，并把该返回值作为自己的返回值。它仍然不修改 `dynamic_epoch`。普通 reissue 在 `clear_uid_dispatch_result()` 返回后、`dynamic_epoch++` 之前，仅当返回 1 时打印。此时若 UID 原来 active，`retire_active_uid()` 已经把 `active` 清为 0，epoch 仍是旧值，日志读取这一刻的 `status.dynamic_epoch`。删除之后不得再索引 `pending_store_fault_by_uid[uid]`。

`reset_uid_after_older_redirect()` 保持先直接删除草稿，再调用 `clear_uid_dispatch_result()`。因此方案 A 得到的返回值是 0，不打印 `PENDING_FAULT_DRAFT`。不得把这次提前删除挪到 `clear_uid_dispatch_result()` 之后。

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
- 不额外调用 `release_ptw_wait_replay()`。`prepare_uid_for_redirect_reissue()` 只由 `apply_redirect_flush_range()` 调用，而 `apply_redirect_flush()` 在 range 扫描之后调用 `clear_ptw_wait_replay_by_redirect()`。方案 A 和普通 reissue 返回前都已经把 `active` 清为 0，该函数会按 `!status.active` 删除对应 PTW wait 项。
- 不额外清理 STA late-fault tombstone。`clear_uid_dispatch_result()` 已经调用 `status.clear_sta_late_fault_tombstones()`，两条 reissue 路径都会执行到它。
- 不调用 `capture_std_late_raw_tombstone()`。该记录只由 `retire_active_uid()` 为已发出的 fault STD 保存迟到 raw 身份；方案 A 作废的是尚未提交的旧实例，不建立这份身份。

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
  -> remove_uid_from_issue_queues：此时三个队列通常已经为空
  -> reset：清除正式 fault，dynamic_epoch 加一，不打印 PENDING_FAULT_DRAFT
  -> admission 水位退回该 UID
  -> 新实例重新 route 后才产生新发射项
```

## 6. 验证

定向场景至少覆盖：

1. scalar store 的 STA fault real-WB 已记录、STD real-WB 未到，更老 redirect 覆盖该 UID。期望草稿在本次 reissue 中删除，正式 fault 位保持 0，`dynamic_epoch` 加一，同一 UID 重新入队并发射 STA 与 STD。
2. 同一窗口换成 STD fault real-WB 已记录、STA real-WB 未到。期望与第 1 条相同。
3. 双 real-WB 已完成且尚未 `rob_commit` 的正式 store fault 被更老 redirect 覆盖。进入方案 A 前，LOAD 和 STA 已由 finalize 的 quiesce 删除，STD 已在 fire 时出队，三个队列通常为空。期望仍进入 `reset_uid_after_older_redirect()`，reset 后三个队列仍没有该 UID 的旧项，`dynamic_epoch` 加一，并且不打印 `PENDING_FAULT_DRAFT`。本场景不要求先构造残留的 STA 或 LOAD 发射项。
4. 已 `rob_commit` 的 fault head 自己产生异常 redirect。现有 `soft_test_memblock_dispatch_fault_smoke_sequence` 的 uid0 fault-head 路径覆盖 software-only 终态；real smoke 中该 UID 不重发，水位从下一条 UID 开始。

本 plan 不新增 virtual sequence，也不新增 plus 参数。可执行验证使用已有入口：

```text
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=basicTest ts=virtual_base_sequence mode=base_fun
make eda_run tc=basicTest ts=virtual_base_sequence mode=base_fun
make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_smoke
make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_mmu_sv39_smoke seed=666666
```

前三条是测试框架 plan 执行规则要求的基础验证。最后一条是此前尚未闭环的 fault smoke。这些命令不能构造第 1、2 条。`soft_test_memblock_dispatch_fault_smoke_sequence` 对同一 store 的 STA 注入 fault、对其余已 fire 项包括 STD 注入 pass，pending 窗口不会留下。`memblock_dispatch_real_cancel_reconcile_vseq` 在 writeback 前注入 redirect，store 一旦已有 writeback 就直接 fatal。因此 seed 未出现 `PENDING_FAULT_DRAFT` 时，不得把第 1、2 条写成已通过。第 4 条的 real smoke 通过标准是全局 stop、`UVM_ERROR=0` 和 `UVM_FATAL=0`。

第 1、2 条的最小后续入口是复用 fault smoke 的手工主表和 `submit_writeback_event()`，以及 cancel reconcile 已有的 `request_redirect_flush()` 与 `push_redirect_drive()`，只给一个 target 注入 fault real-WB，再用更老 ROB 冲刷这颗尚未 `rob_commit` 的 store。该入口还必须满足 `apply_redirect_flush_range()` 已有的 redirect anchor 和 LSQ sample 水位，不能直接调用未锚定的 flush。本 plan 不新增这套 sequence，也不新做测试框架。

## 7. 评审修正记录

本节记录执行前 review 相对初稿的修正，供后续对照。它不新增实现范围。

- 日志不放进 `clear_pending_store_fault()`。该函数还被 finalize 和失配清扫调用。`clear_uid_dispatch_result()` 返回是否删除，普通 reissue 在返回后、`dynamic_epoch++` 前打印 `PENDING_FAULT_DRAFT`。方案 A 仍先直接删除草稿，因此不会打印这条日志。
- 方案 A 的队列删除保留，但理由改为消除 active 与 inactive 的分支差。正式 fault 到达时三个队列通常已经为空，不把“残留 STA 或 LOAD 被当成新实例发射”写成当前可达行为，也不修改 quiesce。
- PTW wait 由 `apply_redirect_flush()` 在 range 之后按 `active==0` 删除。STA late-fault tombstone 已由 `clear_uid_dispatch_result()` 清理。方案 A 不捕获 STD late raw tombstone。这三项不新增调用。
- 第 1、2 条没有现成 sequence。本 plan 写明最小后续入口和 anchor 限制，但不在本次 coding 中新增 sequence。seed `666666` 未命中时不得宣称这些窗口已验证。

## 8. 与初步 plan 差异说明

本章只用于 review，不作为 coding 依据。

初步 plan 只要求在普通 reissue 中删除双 real-WB 前的 pending fault 草稿，并明确不处理方案 A 的旧发射项。正式 plan 增加了方案 A 重置前对 LOAD、STA、STD 旧发射项的统一删除。方案 A 入口、`retire_active_uid()` 的使用边界、水位和已提交 fault head 的保留路径没有改变。

函数差异：不新增 helper。复用 `clear_pending_store_fault()`、`remove_uid_from_issue_queues()` 和 `reset_uid_after_older_redirect()`。`clear_pending_store_fault()` 的返回值从无返回改为返回是否删除，只用于决定是否打印日志，不改变删除条件。
