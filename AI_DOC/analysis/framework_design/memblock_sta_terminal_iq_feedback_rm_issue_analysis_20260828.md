# MemBlock STA terminal 后迟到 IQ feedback 的 RM 问题分析（2026-08-28）

| 项目 | 内容 |
| --- | --- |
| 结论 | 测试框架/RM 生命周期问题，不是 RTL 问题 |
| 版本 | V2，`mem_ut_uvm_v2` |
| testcase | `basicTest`，`memblock_dispatch_real_smoke_vseq` |
| cfg / seed | `tc_dispatch_real_mmu_sv39_smoke` / `666666` |
| 回归 mode | `rm_sv39_10k_sta_late_fault_tombstone_20260828` |
| 失败时间 | `897.800ns` |
| UVM 报错 | `IQ_FEEDBACK_ATTACH: STA IQ SQ owner mismatch uid=9 raw=0/3 status=0/3 mapped=1` |
| RTL 修改 | 无；本记录只定义 UVM/RM 修复边界 |

## 1. 专有名词与判定范围

| 术语 | 当前中文含义 | 代码落点 | 本例 |
| --- | --- | --- |
| `STA` | Store Address 子操作。 | `MEMBLOCK_ISSUE_TARGET_STA` | UID9、ROB `0/122`、SQ `0/3`。 |
| IQ feedback | StoreUnit 向 IssueQueue 返回的 `hit` 结果，不是写回。 | `io_mem_to_ooo_staIqFeedback_*_feedbackSlow_*` | UID9 的 `hit=0` 和后续 `hit=1`。 |
| terminal fault | RM 已将该 UID 的异常结果锁定，后续 replay/feedback 不得再改变状态。 | `fault`、`exception_pending`、`sta_fault` | UID9 的 late `0x8080` 已消费。 |
| current snapshot | 当前可接受 STA 发射的 UID/ROB/SQ/issue epoch/replay sequence 身份。 | `sta_dispatched` 与 `sta_issue_epoch` | terminal 后不应再创建。 |
| stale IQ feedback | DUT 已物理接受但 RM 在采样时已将该 UID 终止的 feedback。 | adapter 输入 raw | UID9 在 fault 后到达的 `hit=1`。 |

本文仅分析 UVM raw adapter 的归属策略。若 raw SQ 不存在 active owner、owner 的 SQ 不一致、或
terminal 状态之外的未发射 feedback 到达，仍保留 fatal，不在本方案中放宽。

## 2. 失败事实

10000 笔 Sv39/U 态回归在 tombstone 修复后不再卡在 UID9 的 replay-drain，而是在 `897.8ns`
因下一条 UID9 STA IQ feedback 停止。日志中的关键顺序如下：

| 时间 | DUT / RM 事实 | 解释 |
| --- | --- | --- |
| `855.3ns` | `issueSta_0` 发射 ROB `0/0x7a`、SQ `0/0x03`。 | UID9 的重发实例已被物理接受。 |
| `865.3ns` | `staIqFeedback_0.valid=1, hit=0, SQ=0/3`。 | 该实例请求 replay；RM 保存 issue epoch `177` 的 tombstone。 |
| `875.3ns` | `writebackSta_0.valid=1, ROB=0/0x7a, exception[15]=1, exception[7]=1`。 | 旧 pipeline 实例输出 `0x8080`。 |
| `880.3ns` | `issueSta_0` 再次发射同一 ROB/SQ。 | driver 已在 raw fault 被 monitor 消费前物理发射 reissue。 |
| `882.8ns` | adapter 以 tombstone 还原 UID9 的旧 issue epoch `6`，`mark_sta_late_fault_from_tombstone()` 消费 fault。 | UID9 进入 terminal fault；待发射 replay 被清理。 |
| `885.3ns` | issue sequence 打印 `skip stale issue item uid=9 target=2`。 | fault guard 已阻止新的软件 issue。 |
| `890.3ns` | `staIqFeedback_0.valid=1, hit=1, SQ=0/3`。 | 在 terminal 后收到先前物理发射的 STA IQ feedback。 |
| `897.8ns` | `attach_current_issue_snapshot()` 因 `sta_dispatched=0` 报 `IQ_FEEDBACK_ATTACH` fatal。 | adapter 误把 terminal stale feedback 当作 map owner 损坏。 |

失败日志：

```text
mem_ut/ver/ut/memblock/sim/rm_sv39_10k_sta_late_fault_tombstone_20260828/log/
tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.log
```

波形：

```text
mem_ut/ver/ut/memblock/sim/rm_sv39_10k_sta_late_fault_tombstone_20260828/wave/
tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.fsdb
```

关键波形路径：

```text
top_tb.U_MEMBLOCK.io_ooo_to_mem_issueSta_0_*
top_tb.U_MEMBLOCK.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_*
top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_*
top_tb.U_MEMBLOCK._inner_StoreUnit_0_io_{feedback_slow,stout}_*
```

## 3. 根因与 RTL 排除理由

`dispatch_monitor_event_adapter::convert_raw_iq_feedback()` 把 SQ-only feedback 直接交给
`attach_current_issue_snapshot()`。后者要求 `status.sta_dispatched=1` 才能补齐 event identity。
late fault 成功消费后，框架有意清除 `sta_dispatched`、replay queue 和 PTW wait；但 active SQ map
在 fault retire 前仍保留，故 UID9 能被正确查到、SQ 也确实一致，却必然在当前 snapshot 前置条件失败。

这不是 RTL 问题，理由如下：

1. 波形连续显示同一 ROB/SQ 的 STA fire、IQ `hit=0`、旧实例 fault、下一次 physical STA fire 与
   后续 `hit=1`，没有无来源 raw 或 SQ 错配。
2. `hit=1` 对应的是 fault 被 monitor 消费前已经物理发射的 request；DUT 不会因为软件 RM 在后续
   采样点设置 terminal 而撤回已产生的 feedback。
3. RM 的 terminal 语义已要求该 feedback 不得改变 issue/replay/fault 状态；fatal 仅来自 adapter
   试图为一个不应再下发的事件构造 current identity。

因此不触发 RTL subagent review。按用户要求，只有出现缺乏正常 DUT 时序解释的 RTL 候选时才启动
独立 RTL review；本例波形与日志已经充分指向 RM 生命周期遗漏。

## 4. 最优修改方案

采用“terminal 后窄范围消费 STA IQ feedback”：在 `convert_raw_iq_feedback()` 做完 raw 协议
结构检查并形成 SQ key 后、调用严格 current attach 前，新增
`try_drop_terminal_sta_iq_feedback()`。

该 helper 的规则：

1. 以 SQ key 查询 active UID；无 active owner 时不消费，继续原 fatal。
2. 验证 active status 的 canonical SQ 与 raw SQ 相同；不一致时不消费，继续原 fatal。
3. 仅当 `fault || exception_pending || terminal_done` 为真时，记录 `STA_IQ_TERMINAL_DROP` 信息并
   返回“已消费”；converter 返回 `0`，不产生下游 feedback/replay event。
4. 非 terminal UID 保持原 `attach_current_issue_snapshot()` 全部严格检查，尤其是
   `sta_dispatched=0` 的非 terminal 异常不能被静默掩盖。

相比在 handler 中丢弃或放宽所有 `sta_dispatched=0` 的方案，该位置最优：raw 尚未被转换为缺少
generation 的 event，且 active SQ map 能证明它属于 terminal UID；不会修改 PMA/PMP、TLB、DCache
ledger、issue queue 或 DUT interface，也不会掩盖非 terminal 的 generation/owner 违规。

## 5. 验收标准

1. 同 seed 重跑时，UID9 `890.3ns` 类 feedback 输出 `STA_IQ_TERMINAL_DROP`，不产生
   `IQ_FEEDBACK_ATTACH` fatal。
2. raw SQ 无 owner、SQ owner 不一致、或非 terminal `sta_dispatched=0` 仍维持原 fatal。
3. `mark_issue_feedback_success()`、`mark_replay_pending()`、PTW wait、tombstone 和 RM exception
   expectation 不新增写入或放宽。
4. 10000 笔回归继续直到 `TEST CASE PASSED`，或出现新的 RM 问题后进入同样的分析-方案-修复流程。
