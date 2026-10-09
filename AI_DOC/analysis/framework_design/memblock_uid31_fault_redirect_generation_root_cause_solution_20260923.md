# UID31 双写回屏障卡住根因与解决方案

> 注记：本文记录 2026-09-23 的旧根因分析，其中 barrier STD 保留路径已经删除。当前行为以
> `AI_DOC/plan/test_framework/plan/do/memblock_redirect_pending_store_fault_draft_clear_plan_20261008.md`
> 和最新 `fault_exception_flow.md` 为准。


## 1. 分析范围与术语

### 1.1 术语与抽象功能说明

| 术语 | 当前含义 | 代码落点 | 示例 |
| --- | --- | --- | --- |
| `real writeback` | DUT 对当前 issue dynamic instance 产生的真实 STA/STD 完成事件；IQ feedback 不等价于它。 | `sta_writeback`、`std_writeback` | STA fault raw 也必须先置 `sta_writeback`。 |
| `pending fault` | 已采到 STA 或 STD fault，但尚未同时取得两个 real writeback 的暂存证据。 | `pending_store_fault_by_uid` | STA fault 先到，等待 STD WB。 |
| `SQ owner` | 当前动态实例对物理 SQ key 的 live 归属；SQ deq 后 live map 会删除。 | `uid_by_sq`、`active_sq_mapped` | UID31 的 SQ key 在 STD WB 前已 deq。 |
| `barrier STD` | pending store fault 已存在、STA 已 WB、STD 未 WB 时必须完成的唯一 STD item。 | `std_issue_q`、保存的 `sqIdx` | 它不是可任意 reissue 的 stale item。 |
| `fault terminal` | fault 已 ROB commit、LSQ owner 收口后的非成功终态。 | `terminal_done`、`success=0` | terminal 后才允许该 UID 的 redirect 链路完成。 |

抽象功能说明：本文只分析 V2 `mem_ut` 测试框架的状态机和事件归属，不改变 DUT RTL、L2TLB 或 RM。目标顺序是
`STA/STD real WB -> fault finalize -> fault commit/terminal -> fault redirect`；SQ deq 若位于两次 WB 之间，
必须仍能完成同一动态实例的 STD WB。

## 2. 已确认的根因

指定用例：

```text
tc=basicTest
ts=memblock_dispatch_real_smoke_vseq
cfg=tc_dispatch_real_mmu_sv39_smoke
seed=666666
partcmp_op=off
```

首次卡死时，UID31 的最后动态实例有以下直接证据：

| 时间 | 日志/状态 | 含义 |
| --- | --- | --- |
| `9322.8ns` | `STA fault real-WB` | STA 已写回并建立 pending fault。 |
| `9337.8ns` | `SQ deq`，`sta_dispatch/wb=1/1`、`std_dispatch/queued/wb/pass=0/1/0/0`、`pending_fault=1` | STD 已在 issue queue，但尚未发射/写回；SQ live map 已被释放。 |
| 随后 | `terminal_done_uid=31` 不前进 | 双 real-WB 条件永远无法满足，fault commit 合法地被阻止。 |

旧实现于 SQ deq 无条件删除未 fire 的 STD queue item，同时 scheduler 要求 STD 必须持有
`active_sq_mapped`。这形成了不可恢复的闭环：

```text
STA fault WB
 -> pending fault
 -> SQ deq 删除尚未发射的 STD
 -> scheduler 因无 live SQ owner 拒绝 STD
 -> STD real WB 永不发生
 -> 双 real-WB 屏障不通过
 -> fault commit/terminal/redirect 不发生
```

因此根因是测试框架对 SQ deq 后唯一 barrier STD 的错误清理和拒绝发射，不是 RTL bug，也不是 L2TLB responder。

## 3. 非根因说明

### 3.1 更老 redirect 与 UID31 reissue

更老 UID 的 redirect 可以按既有规则 flush/reissue 尚未成为 committed fault head 的 UID31；这是正常的年轻
实例恢复。它不能解释 UID31 最后一次动态实例中“已 pending fault、STD 尚未 WB、SQ 已 deq”的永久状态。

UID31 成为最终最老未完成实例后，已没有更老 redirect 可以继续覆盖它；此时仍卡住进一步证明问题在 STD 无法完成，
而非 redirect generation 的无限覆盖。

### 3.2 redirect sideband

测试框架保留 fault commit 前的 redirect sideband 单飞等待，避免 sideband 抢先绕过 fault token。它是已有 recovery
仲裁的一部分，但日志在修复后已显示 UID31 正常产生 `request scalar fault redirect`，故它不是这次死锁的主根因。
本问题不新增 redirect intent、generation history 或第二套 token 状态机。

### 3.3 L2TLB no-progress warning

`L2TLB responder no-progress diagnostic` 只在 global stop 尚未建立时报告 watchdog warning，并保持 owner active。
UID31 未 terminal 导致 transaction drain 不完整，才使该 warning 出现；它不是停止仿真的条件，也没有证据显示 RTL
握手异常。

## 4. 最小修复方案

### 4.1 `release_uid_sq_mapping()`：保留唯一 barrier STD

抽象功能描述：该函数释放已 deq 的 live SQ map；通常会清理不再合法的 STD item。对已建立 pending fault 且未 WB 的
唯一 STD，它只删除 live map，保留 item 和历史 SQ key，供该动态实例完成第二个 real writeback。

规则：

```text
若 pending_store_fault_matches(uid) 且 std_writeback=0：
  保留已有 STD queue item；删除 uid_by_sq 和 active_sq_mapped。
否则：
  删除未 fire STD item，清 queued_std；删除 uid_by_sq 和 active_sq_mapped。
```

这不是允许任意 SQ deq 后 STD 重发。pending record 必须匹配当前 `dynamic_epoch`，并且 STD WB 后立即失效。

### 4.2 `issue_queue_scheduler`：仅放行该 item

抽象功能描述：scheduler 默认要求 STA/STD 具备 live SQ owner。SQ deq 后它不会新建或重路由 STD；仅对 deq 前已经
排队的 barrier STD，允许保存的 SQ key 参与一次 issue，仍校验 UID、dynamic epoch、ROB key、`lsq_deq`、
STA 已 WB、`queued_std`、未 dispatched、未 writeback、pending fault 和 key 一致性。

STA、普通 STD、redirect/flush 后旧动态实例都不适用该例外；scheduler 不会借此重建新的无 owner item。

### 4.3 monitor 归属：使用历史 SQ identity

抽象功能描述：adapter 和 `resolve_uid_for_event()` 将 raw STD WB 归属到当前 active dynamic instance。live SQ map 已删除时，
只为 barrier STD 结合 active ROB、current issue epoch、flush epoch 和 status 保存的 SQ key 完成归属，避免第二次 live-map
反查把合法 WB 丢弃。

### 4.4 双写回后保持原有 fault 链路

`finalize_pending_store_fault()` 只有 `sta_writeback && std_writeback` 后才写 UID fault；commit selector、fault commit 和
terminal/redirect 均重复检查 `store_real_writebacks_done()`。因此异常、terminal 与 redirect 不会早于 STD real-WB。

## 5. 修复后证据与验收

最新运行已观察到 UID31：

```text
9337.8ns  SQ deq before STD real-WB uid=31
9362.8ns  finalize store fault uid=31 sta_wb=1 std_wb=1
9370.3ns  request scalar fault redirect uid=31
```

这证明原死锁已解除，实际顺序为：

```text
STA WB -> SQ deq -> retained STD issue -> STD WB
-> fault finalize -> fault terminal -> UID31 redirect
```

远端 VCS/Verdi 编译已通过，报告 `0 error(s), 0 warning(s)`。指定 10000 transaction 仿真已完成，
仿真结束时间 `2454647.800ns`，global stop 在 `2454617.8ns` 提交，最终 `UVM_ERROR : 0`、`UVM_FATAL : 0`。
该日志没有单独的 `TEST CASE PASSED` 文本，但已正常 `$finish`，可按当前 `basicTest` 流程判定通过。
