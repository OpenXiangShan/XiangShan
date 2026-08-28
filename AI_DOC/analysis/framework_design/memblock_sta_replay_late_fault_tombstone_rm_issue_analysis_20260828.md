# MemBlock STA replay 迟到 fault tombstone RM 问题分析（2026-08-28）

| 项目 | 内容 |
| --- | --- |
| 状态 | 已完成第二轮日志和首轮 VCD 取证；判定为测试框架/RM 生命周期问题，不是 RTL 问题 |
| 版本 | V2，分支 `mem_ut_uvm_v2` |
| testcase | `basicTest`，`memblock_dispatch_real_smoke_vseq` |
| cfg / seed | `tc_dispatch_real_mmu_sv39_smoke` / `666666` |
| 目标规模 | `MEMBLOCK_MAIN_TRANS_NUM=10000` |
| 第二轮 mode | `rm_sv39_10k_sta_replay_drain_20260828` |
| 第二轮日志 | `mem_ut/ver/ut/memblock/sim/rm_sv39_10k_sta_replay_drain_20260828/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.log` |
| 首轮可读波形 | `/nfs/home/lixiangrui/memblock_inflight_fault_20260828.vcd` |
| RTL 修改 | 无；本记录只定义 UVM/RM 修复边界 |

## 1. 专有名词与判定范围

| 术语 | 当前中文含义 | 代码落点 | 示例 |
| --- | --- | --- | --- |
| `STA` | store address 子操作，使用 `MEMBLOCK_ISSUE_TARGET_STA`。 | `status_transaction.sta_dispatched` | UID9、UID25 的地址侧发射。 |
| `IQ feedback` | StoreUnit 到 issue queue 的慢反馈；`hit=0` 只表示该次 STA 需要 replay。 | `convert_raw_iq_feedback()` | 两个 UID 都观察到 `hit=0`。 |
| `raw STA` | 顶层 `writebackSta_*` 采到的 StoreUnit 原始写回。 | `convert_raw_int_wb()` | UID25 的 exception raw。 |
| `current snapshot` | 当前 STA 发射的 `issue_epoch`、`replay_seq` 和 target flush epoch。 | `mark_issue_snapshot()` | 正常 raw 必须严格绑定它。 |
| `tombstone` | replay 已开放后保存旧 STA 身份的小型历史记录，仅允许迟到的 fault 找回旧实例。 | `status_transaction.sta_late_fault_tombstone_q` | UID25 的旧 issue snapshot。 |
| `dynamic epoch` | 同一 UID 被 redirect 后的动态实例编号。 | `status.dynamic_epoch` | redirect 后旧 tombstone 必须失效。 |
| `terminal fault` | fault 已写入 status，后续 replay/issue 不得再改变该 UID 的完成结果。 | `mark_target_fault()`、fault retire | 迟到 AF/PF 终止当前 UID。 |

本文只判定 UVM 事件归属和状态生命周期。若后续波形显示一个 STA raw fault 既没有
同 ROB 的 active UID，也没有当前 snapshot 或本方案定义的 tombstone，才是 RTL 候选，
届时按用户要求启动独立 RTL review；当前证据不满足该条件。

## 2. 二次回归事实

原修复在 STA `hit=0` 时保持 `sta_dispatched`，直到 raw STA 到达再调用
`mark_replay_pending()`。该行为修复了 UID25 的首轮 `INT_WB_ATTACH`，但第二轮回归
在没有新 RM fatal 的情况下停滞：

```text
terminal_done_uid=9
load_q=0 sta_q=0 std_q=1
issue queue has pending work but no fire for 60000 iterations
```

日志中的 UID9 与 UID25 形成了完整反例对：

| UID | 时刻 | 可观察事实 | 结论 |
| --- | --- | --- | --- |
| UID9 | `307.8ns` | STD normal pass，UID 仍需 STA 侧完成。 | 该 UID 仍活跃。 |
| UID9 | `317.8ns` | `STA replay drain pending`，`issue_epoch=6`。 | IQ `hit=0` 已被接收。 |
| UID9 后续 | 至少运行到 `1,000,000ns` | 没有 raw STA fault、normal raw 或 drain consume。 | 合法纯 replay 不产生旧 raw；等待 raw 会永久阻断重发。 |
| UID25 | `527.8ns` | `STA replay drain pending`，`issue_epoch=92`。 | 同样是 IQ `hit=0`。 |
| UID25 | `537.8ns` | raw STA fault，`exception_vec=0x8080`。 | 旧 pipeline 的真实异常会迟到。 |

首轮 VCD 中 UID25 的原始顶层路径为：

```text
top_tb.U_MEMBLOCK.io_ooo_to_mem_issueSta_0_*
top_tb.U_MEMBLOCK.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_*
top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_*
top_tb.U_MEMBLOCK._inner_StoreUnit_0_io_{feedback_slow,stout}_*
```

V2 对外 STA IQ feedback 只有 `valid`、`hit` 和 `sqIdx`，没有可供 TB 采样的
`flushState` 或 generation 标识。因此 RM 不能在 `hit=0` 当拍判断该 replay 是
“纯 replay”还是“随后有 fault 的旧流水线实例”。

## 3. 根因与 RTL 排除理由

replay-drain 方案把两种合法时序强行收敛为“必有 raw”的单一路径：

```text
STA IQ hit=0
  -> 保持 sta_dispatched
  -> 等待 raw STA
  -> 只有 raw 到达才调用 mark_replay_pending
```

UID9 证明最后一步不一定发生，导致 `sta_dispatched=1` 长期阻止 issue scheduler
重新选择 STA。UID25 又证明删除旧 snapshot 也不正确，因为迟到的 raw fault 仍需要历史
身份归属。两者同时成立，说明错误来自 RM 过度建模了 `hit=0` 后的时序，而不是 DUT
产生无来源输出。

当前不启动 RTL subagent，理由如下：

1. UID9 没有无来源 raw、异常或协议断言，只是 RM 自身没有重新发射请求。
2. UID25 的 raw fault 前存在合法 STA fire，首轮 VCD 可从 issue、feedback 到 writeback
   连续追踪同一 ROB/SQ。
3. 接口确实没有足以区分两个分支的 feedback 字段，RM 必须以后续 raw 的实际异常性做
   窄范围处理，不能要求 RTL 补发一个不存在的 raw。

## 4. 最优修复方案

采用“立即 replay + late-fault tombstone”，而不是延迟 replay：

```text
STA IQ hit=0
  -> 冻结旧 STA snapshot 到该 UID 的 tombstone queue
  -> 立即复用 mark_replay_pending() 开放重发

后续 normal raw STA
  -> 只能使用 current snapshot；没有 current snapshot 仍为 framework fatal

后续 raw STA fault
  -> 先以 active ROB 找到 UID
  -> 在该 UID 的有界 tombstone queue 中匹配旧 ROB/SQ、dynamic epoch、
     issue_epoch/replay_seq 和 target flush epoch
  -> 命中后以旧 snapshot 记入 terminal fault，取消同 UID 的 replay work
  -> 不命中后回退到既有严格 current snapshot
```

每条 tombstone 保存 `ROB`、`SQ`、`issue_epoch`、`replay_seq`、`dynamic_epoch`、
target flush epoch 与创建 cycle。队列只在该 UID 内查找，容量由 V2 的
`MEMBLOCK_DUT_SQ_SIZE` 上界限制，不扫描 `main_trans_num`、active map 或全局 status 表。

清理点为：fault 被消费、UID terminal retire、redirect/flush、`status.reset()`。
普通 replay transition 不清 tombstone；否则 UID25 的迟到 fault 又会失去归属。

## 5. 正确性边界

1. 不接受迟到 normal STA raw。它缺少 current snapshot 时仍报 framework fatal，避免把
   普通写回宽松绑定到错误 generation。
2. 迟到例外只覆盖 `raw.exception_vec != 0` 的 STA raw，且要求 active ROB、同 UID 的
   dynamic epoch、ROB/SQ 和 target flush epoch 均可验证。
3. 同一 UID 已有新 STA 发射时，fault 归属到 tombstone 或 current snapshot 都会写入同一
   architectural UID 的 terminal fault；随后删除 issue queue，禁止新 replay 再改变结果。
4. redirect 后 `clear_uid_dispatch_result()` 清 tombstone，`dynamic_epoch` 变化的旧 raw
   不得附着到新实例。
5. 不增加固定等待周期，不新增 DUT interface、cfg 或 RTL 改动。

## 6. 后续执行与验收

可执行 plan：

```text
AI_DOC/plan/test_framework/plan/do/memblock_sta_replay_late_fault_tombstone_coding_plan_20260828.md
```

实现后用相同 Sv39/U 态、seed `666666`、10000 笔 cfg 回归。最低验收点：

```text
UID9: STA hit=0 后出现 immediate replay / 重新 issue，而不再等待 raw
UID25: STA hit=0 后可记录 tombstone，迟到 0x8080 fault 被消费
no:    INT_WB_ATTACH writeback target was not dispatched
no:    issue queue has pending work but no fire for 60000 iterations
```
