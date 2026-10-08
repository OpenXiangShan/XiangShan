# Fault Commit PendingPtr 前置约束与删除 Barrier STD 修改方案

## 1. 方案目标与术语

### 1.1 目标

本方案修正 `mem_ut` 测试框架对 scalar store fault 的建模顺序，恢复与 DUT RTL 一致的简单生命周期：

```text
STA/STD real-WB 均完成
  -> fault UID 到达 ROB head
  -> fault commit transaction 携带 pendingPtr/pendingst/isStoreException
  -> DUT 根据 pendingPtr 处理并出队当前 SQ entry
  -> fault terminal_done
  -> exception redirect
```

必须删除此前为兼容错误观测顺序而加入的 `barrier STD` 逻辑，不再允许 SQ deq 后保留、重发或补发历史 STD。

### 1.2 术语与抽象功能说明

| 术语 | 当前含义 | 代码对象/信号 | 示例 |
| --- | --- | --- | --- |
| `real writeback` | DUT 对当前 dynamic instance 产生的真实 STA/STD 完成事件。 | `sta_writeback`、`std_writeback` | 两者均为 1 后才可 fault commit。 |
| `fault commit transaction` | ROB head fault 发送给 LSQ/DUT 的提交 transaction。 | `rob_commit`、`pendingPtr`、`pendingst` | store fault 携带 `isStoreException=1`。 |
| `pendingPtr` | ROB 当前待处理 LSQ entry 的 ROB 指针。 | `io_rob_pendingPtr` | StoreQueue 用它匹配当前 SQ entry。 |
| `barrier STD` | 旧方案中 SQ deq 后保留并继续发射的历史 STD item。 | `std_issue_q` 特殊例外 | 本方案删除。 |
| `fault terminal` | fault commit、当前 LSQ owner 收口后的终态。 | `terminal_done=1` | terminal 后才能申请 redirect。 |
| `exception redirect` | fault terminal 后 flush younger instruction 的 redirect。 | `io_exceptionRedirect_valid` | 不替代 fault commit。 |

抽象功能说明：测试框架只允许 fault UID 在 STA/STD 两个真实写回完成并位于 modeled ROB head 后进入 fault commit；
`pendingPtr` 是 fault commit 的输入，不由 STA fault WB 直接触发；redirect 只负责 fault terminal 后的 younger flush。

## 2. 当前错误建模与修正结论

### 2.1 当前错误建模

旧实现看到 SQ deq 早于 STD WB 时，会：

1. 保留未发射 STD queue item；
2. 在 scheduler 中放宽无 live SQ owner 的 STD eligibility；
3. 在 monitor 中使用历史 SQ key 归属迟到 STD raw。

这组逻辑形成了 `barrier STD` 兼容路径，实际等价于允许：

```text
SQ owner 已释放
 -> 历史 STD 仍可继续 issue
 -> 迟到 STD WB 补齐 fault barrier
```

该行为不符合本方案确认的 RTL 合同。波形应证明 fault commit transaction 已提前携带 `pendingPtr`，因此正常路径
不会出现“当前 fault SQ 已先出队、STD 后写回”的合法情况。若测试框架观察到此顺序，应判为事件归属、采样时序或
动态 epoch 错误，而不是通过 barrier STD 兼容。

### 2.2 修正结论

测试框架必须恢复以下简单规则：

```text
SQ deq 后：
  删除该 UID 尚未 fire 的 STD issue item；
  清除 queued_std；
  删除 live SQ mapping；
  不允许 STA/STD 重新 route；
  不允许历史 STD 在无 owner 时发射；
  不允许用历史 SQ key 归属普通迟到 STD WB。
```

STA/STD 双 WB 屏障仍然保留，但它只用于 fault candidate/commit gate，不再通过保留 STD 来“补齐”屏障。

## 3. 代码修改方案

### 3.1 删除 `release_uid_sq_mapping()` 的 barrier 保留分支

抽象功能描述：SQ deq 时统一清理当前 dynamic instance 的未完成 STD queue item 和 live SQ owner。

修改要求：

- 删除 `pending_store_fault_matches(uid) && !status.std_writeback` 例外；
- 永远执行 `delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_STD, uid, ...)`；
- 永远清零 `status.queued_std`；
- 删除 `uid_by_sq` 并清除 `active_sq_mapped`；
- 若随后收到没有 active SQ owner 的 STD raw，直接判为 stale/invalid，不得写入 `std_writeback`。

伪代码：

```text
SQ deq 到达；
删除 uid 对应所有尚未 fire 的 STD item；
清零 queued_std；
删除 uid_by_sq；
清零 active_sq_mapped；
不创建任何新的 STD item。
```

### 3.2 恢复 `issue_queue_scheduler` 的 live owner 约束

抽象功能描述：scheduler 只允许仍拥有 live SQ mapping 的 STA/STD item进入 issue queue 和 fire 阶段。

修改要求：

- `route_target()` 对 STA/STD 缺少 `active_sq_mapped` 时直接返回；
- 删除 `std_waits_fault_barrier`；
- `is_issue_item_state_eligible()` 对 STD 恢复 `status.active_sq_mapped` 必须为 1；
- 删除 `lsq_deq`、历史 SQ key、barrier pending fault 等放宽条件；
- `make_issue_item()` 不再为无 owner STD 设置 `has_sqIdx=1`。

伪代码：

```text
如果 target 是 STA/STD 且 active_sq_mapped=0：
  不 route；
  不进入 issue queue；
  不允许 fire。
否则按普通 live owner、dynamic epoch 和 key 检查处理。
```

### 3.3 删除 adapter/common-data 的历史 SQ key 归属例外

抽象功能描述：monitor 只接受能够通过当前 live SQ map 校验的 STD raw event；不再使用已释放的历史 SQ key 补认普通
STD writeback。

修改要求：

- 删除 `use_dequeued_barrier_std_sq_identity`；
- `resolve_uid_for_event()` 对 `has_sq` 统一要求 `lookup_active_uid_by_sq()` 成功；
- `fill_current_issue_snapshot()` 对 `active_sq_mapped=0` 的 STD raw 直接 drop 或报告 stale；
- 保留 ROB/issue epoch/replay identity 的普通 stale 防护，但不将其作为无 SQ owner 的替代所有权。

### 3.4 保留双 real-WB fault commit gate

以下逻辑保留，不因删除 barrier STD 而放宽：

```text
scalar store fault candidate：sta_writeback && std_writeback
mark_fault_rob_commit_uid：再次检查 sta_writeback && std_writeback
fault terminal/redirect：必须位于 fault commit 之后
```

`STA fault WB` 只建立 pending fault；若 STD WB 未到，UID 不能成为 fault candidate，也不能生成 fault commit transaction、
`pendingPtr` 或 redirect。

## 4. RTL 时序验收要求

必须在 RTL 波形中对同一 fault UID/ROB key 观察到以下顺序：

```text
1. STA real-WB 与 STD real-WB 均完成；
2. ROB head 对应该 UID；
3. fault commit transaction 有效；
4. `pendingPtr` 指向当前 fault SQ entry；
5. `pendingst=1`，store exception 类型有效；
6. StoreQueue 产生当前 SQ entry 的 `sqDeq`；
7. 测试框架将 UID 收口为 `terminal_done=1`；
8. `exceptionRedirect_valid` 再出现，且 redirect ROB anchor 对应 fault UID。
```

如果波形显示：

```text
STA fault WB -> pendingPtr/commit -> SQ deq -> redirect
```

则说明 RTL 合同正常，不允许测试框架再保留 barrier STD。

如果波形显示 `SQ deq` 早于 `pendingPtr`，应判定为 RTL/monitor 归属或采样错误，不能通过保留历史 STD 掩盖。

## 5. 风险与回退判据

### 5.1 预期风险

- 删除 barrier STD 后，历史迟到 STD raw 可能被直接 drop；这是预期行为，需通过 dynamic epoch/ROB/SQ 波形定位其来源。
- 旧 UID31 场景若再次出现 `SQ deq before STD WB`，应立即产生明确诊断，而不是继续 issue 历史 STD。
- 需要新增 `pendingPtr_fire`、`sqDeq`、`exceptionRedirect_valid` 的同 UID 关联日志，避免只依赖测试框架事件时间。

### 5.2 禁止回退到 barrier STD

本方案明确禁止以下“修复”：

- SQ deq 后保留 STD queue item；
- 无 active SQ owner 时允许 STD fire；
- 使用历史 SQ key 替代 live SQ map；
- 以 pending fault 为理由重新 route 已释放 owner 的 STD。

若删除后仿真失败，优先检查 RTL 波形和事件归属，不得直接恢复 barrier STD。

## 6. 验证计划

### 6.1 静态验证

- 检索并确认 `std_waits_fault_barrier`、`use_dequeued_barrier_std_sq_identity`、
  `pending_store_fault && !std_writeback` 的 scheduler/adapter 放宽分支全部删除；
- 确认所有 STA/STD route/eligibility 都要求 `active_sq_mapped`；
- 确认 fault candidate、fault commit 和 redirect 均保留双 real-WB 门控；
- 执行 `git diff --check`。

### 6.2 定向波形验证

至少检查：

1. 同一 fault UID 的 STA/STD WB 均已完成后才出现 `pendingPtr`；
2. `pendingPtr` 之后才出现当前 SQ `sqDeq`；
3. `sqDeq` 和 terminal_done 之后才出现 exception redirect；
4. STA-only fault 不产生 pendingPtr、fault commit 或 redirect；
5. 无 owner 的迟到 STD raw 被 drop 并打印 stale 诊断，不被重新 route。

### 6.3 指定用例

```text
tc=basicTest
ts=memblock_dispatch_real_smoke_vseq
cfg=tc_dispatch_real_mmu_sv39_smoke
seed=666666
partcmp_op=off
```

验收标准：无 barrier STD 兼容路径；同 UID 波形满足上述 RTL 顺序；最终 `UVM_ERROR=0`、`UVM_FATAL=0`。

## 7. 方案结论

本方案接受“SQ deq 早于 STD WB 是错误场景”的前提，删除 barrier STD 逻辑，恢复简单 owner 规则。fault UID 必须在
STA/STD 双写回完成后到达 ROB head，由 fault commit transaction 提供 `pendingPtr/pendingst/isStoreException`；
StoreQueue 完成当前 entry 出队后，测试框架才允许 terminal，最后由 fault token 产生 exception redirect。

## 8. 执行中补充/修正（IMPLEMENTATION_DELTA）

### 8.1 pendingPtr 本身必须受 fault 双写回门控

- 来源：删除 barrier STD 后专项仿真仍在 `uid=31` 处观察到 `SQ deq before STD real-WB`。
- 原方案不足：仅删除历史 STD 保留/发射逻辑，但 `clear_lsqcommit_xaction()` 仍会持续发布 modeled ROB head
  的 `pendingPtr`；V2 `Rob.scala` 也将 `pendingPtr := RegNext(deqPtr)`，而 StoreQueue 的 `committed`/`sqDeq`
  路径使用 pendingPtr 比较和 entry completed 状态，并不要求 pendingst 同拍为 1。
- 实现补充：对 `pending_store_fault && !sta_writeback/std_writeback_done` 的当前 scalar-store head，
  暂停发布该 head 的 pendingPtr，并保持 `pendingst=0`；有已提交 watermark 时发布 watermark，否则发布
  当前 ROB head 的前一 key，避免 StoreQueue 的 `isNotAfter` 比较把 fault SQ entry 提前视为 committed。
- 影响：pendingPtr 只有在 STA/STD 双 real-WB、fault finalize、ROB head fault commit 后才恢复指向当前 fault UID；
  SQ deq 早于 STD WB 不再是合法测试框架路径。若波形仍出现该顺序，应按 RTL/monitor 归属错误处理，而不是恢复
  barrier STD。

### 8.2 所有 scalar store 前置门控与 ROB 前一 key 计算

- 来源：第一轮仅在 pending fault 已建立后才压住 pendingPtr，UID31 仍在 `9337.8ns` SQ deq；第二轮扩展为
  所有未双 WB 的 scalar store 后，UID31 仍复现同一现象。
- 原 plan：只说明 fault candidate/commit gate，未说明 StoreQueue 可以基于持续发布的 `pendingPtr` 在 fault
  状态落表前推进 SQ；也未规定 circular ROB 前一 key 的 flag 计算。
- 实现调整：对所有当前 head scalar store，在 `sta_writeback && std_writeback` 前都不发布当前 head；
  先使用已提交 watermark，否则使用当前 head 的真实前一 circular ROB key。前一 key 在 value 非 0 时只减 value，
  value 为 0 时才翻转 flag 并置为 `ROB_SIZE-1`。不得用 `rob_advance(head, ROB_SIZE-1)` 代替倒退一步，
  因为那会翻转 flag，把旧项算成未来一圈的项。
- 验证：专项 seed 必须不再出现 UID31 的 `SQ deq before STD real-WB`，并最终完成 dual WB、fault commit、
  SQ deq、terminal、redirect 与全局 stop。

### 8.3 专项执行结果与当前阻塞

- 已按本方案删除 barrier STD 的保留、scheduler 放宽和历史 SQ key 归属逻辑，并新增所有 scalar store 的
  `pendingPtr` 双 real-WB 门控。
- VCS/Verdi 增量编译通过：`0 error(s), 0 warning(s)`。
- 指定 seed 重新运行后，UID31 未再出现新的 `SQ deq before STD real-WB`；日志出现连续
  `hold pendingPtr before scalar fault dual real-WB`，证明 pendingPtr 门控生效。
- 但仿真仍停在 `terminal_done_uid=31`，并报告 `no LSQ commit/flushSb progress` 与 `no LSQ enqueue progress`。
  STD real-WB 在该动态实例没有继续完成，说明 StoreQueue 当前 `committed/completed/sqDeq` 路径还依赖
  pendingPtr 推进；只在测试框架侧延迟 pendingPtr 会形成新的等待闭环。
- 本轮没有继续修改 RTL/Scala 或新增接口语义，专项仿真已停止。当前方案不能标记为通过；后续必须先明确
  RTL StoreQueue 在 pendingPtr 延迟期间允许 STA/STD 完成的真实条件，再决定是否需要 DUT/接口级配合修改。

### 8.4 方案 A 实现补充

- 对 older redirect 覆盖、尚未 `rob_commit` 的 pending-fault UID，新增完整
  `reset_uid_after_older_redirect()`：清除 pending fault record、fault/exception 状态、STA/STD/LOAD
  writeback/issue 状态和旧 dynamic instance，保留 UID admission 水位，使 `get_next_new_admit_uid()` 回到该 UID。
- 已 committed fault head 不进入该 reset 分支，仍由 fault self redirect 保留 fault UID，仅从 UID+1 继续 younger re-admission。
- older redirect 的 `rollback_max_enqueued_uid(oldest_flushed_uid)` 原有逻辑保留，因此 older redirect 从被覆盖 UID
  重新 admission；fault self redirect 不回退 fault UID。
