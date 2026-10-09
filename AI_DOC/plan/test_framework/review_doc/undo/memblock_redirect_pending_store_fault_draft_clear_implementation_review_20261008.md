# Pending Fault 草稿与方案 A 发射项清理实现 Review

关联 plan：`AI_DOC/plan/test_framework/plan/do/memblock_redirect_pending_store_fault_draft_clear_plan_20261008.md`。

本 review 对照评审后的 plan。执行前 subagent review 已要求修改 plan，修改记录在 plan 第 7、8 节。那些修改发生在 coding 之前，不是 `IMPLEMENTATION_DELTA`。

## 1. 专有名词

| 术语 | 当前含义 | 代码落点 | 示例 |
| --- | --- | --- | --- |
| `pending fault draft` | 双 real-WB 前的 store 异常证据。 | `pending_store_fault_by_uid` | STA fault 已见、STD 未写回。 |
| `普通 reissue` | 未进入方案 A 的 flush 重入队。 | `prepare_uid_for_redirect_reissue()` 后半段 | 删除草稿后 `dynamic_epoch++`。 |
| `方案 A` | 正式 fault 且尚未 `rob_commit` 的重置。 | `reset_uid_after_older_redirect()` | 重置前统一删除发射项。 |

抽象功能描述：`clear_pending_store_fault()` 只按 UID 删除草稿并返回是否删除，不打印日志，也不判断调用场景。

抽象功能描述：`clear_uid_dispatch_result()` 在清除 dispatch 结果前删除草稿，并把是否删除返回给调用方。它不递增 `dynamic_epoch`，也不打印 reissue 日志。

抽象功能描述：`prepare_uid_for_redirect_reissue()` 在普通 reissue 中根据返回值打印草稿日志，在方案 A 中先释放活映射、再删除发射项、最后重置。它不把双 real-WB 前的 pending 记录提升为正式 fault。

## 2. 实现对照

### 2.1 草稿删除

修改前：`clear_pending_store_fault()` 无返回值。普通 reissue 的 `clear_uid_dispatch_result()` 不删除 `pending_store_fault_by_uid`。

修改后：

```text
clear_pending_store_fault(uid):
    若记录存在则删除并返回 1，否则返回 0；
    不打印日志。

clear_uid_dispatch_result(uid):
    dropped = clear_pending_store_fault(uid);
    清除原有 dispatch、writeback 和正式 fault 状态；
    返回 dropped。

普通 reissue:
    retire 或删除发射项；
    记下当前 dynamic_epoch；
    若 clear_uid_dispatch_result 返回 1，打印 PENDING_FAULT_DRAFT；
    然后 dynamic_epoch++。
```

`finalize_pending_store_fault()` 与失配清扫仍调用 `clear_pending_store_fault()`，不会打印 `PENDING_FAULT_DRAFT`。方案 A 仍先直接删除草稿，再调用 `clear_uid_dispatch_result()`，返回值为 0，不打印该日志。

### 2.2 方案 A 发射项

修改前：active 分支只释放 LQ、SQ、ROB 映射；只有 inactive 分支调用 `remove_uid_from_issue_queues()`。

修改后：

```text
正式 fault 且尚未 rob_commit:
    若 active，只释放活映射并把 active 清 0；
    无论是否 active，都调用 remove_uid_from_issue_queues(uid)；
    再调用 reset_uid_after_older_redirect(uid)。
```

没有改成 `retire_active_uid()`。没有修改 quiesce、方案 A 入口、水位或 fault head 保留路径。

## 3. 与 plan 不一致的实现

均与评审后的 plan 保持一致。

## 4. plan 未说明但实现补充的细节

代码行为没有超出评审后的 plan。flow 文档另外删掉了已经不存在的 barrier STD 描述。该描述与本次 coding 前已经提交的源码不一致，本次只是让 flow 不再写成仍保留 STD。它不改变本次源码行为。

## 5. 非本次代码逻辑

`UID31_STD_TRACE`、pendingPtr 门控、`preserve_fault_uid_during_redirect()` 和 `rollback_max_enqueued_uid()` 未改。

第 1、2 条定向窗口没有新增 sequence。现有 fault smoke 会在同一次注入里补上另一个 target 的 pass，cancel reconcile 又在 writeback 前注入 redirect，二者都到不了“单 target fault 草稿加上已锚定的更老 redirect”。plan 已把该缺口写成后续入口，而不是本次通过项。

## 6. git status 对比

本次逻辑源码只改 `common_data_transaction.sv`。文档改 plan、fault/redirect/writeback/ROB flow、UID31 旧分析注记和本 review。仿真产物和 Verdi 会话文件不纳入。

## 7. 验证结果

三条可执行仿真均 `$finish`，`UVM_ERROR=0`，`UVM_FATAL=0`：

- `tc=basicTest ts=virtual_base_sequence mode=base_fun`
- `tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_smoke`
- `tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_mmu_sv39_smoke seed=666666`

`seed=666666` 日志出现 `PENDING_FAULT_DRAFT` 1339 次，并出现 `reset older-redirect pending-fault`。第 1、2 条单 target pending 定向窗口没有单独 sequence，不把这三条仿真写成该窗口的定向证明。
