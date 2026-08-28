# MemBlock STA replay 后迟到写回的 RM 生命周期问题分析（2026-08-28）

| 项目 | 内容 |
| --- | --- |
| 状态 | 已完成首轮波形取证；原 replay-drain 修复在二次回归中暴露纯 replay 死锁，已由 late-fault tombstone 方案替代 |
| 版本 | V2，分支 `mem_ut_uvm_v2` |
| testcase | `basicTest`，`memblock_dispatch_real_smoke_vseq` |
| cfg / seed | `tc_dispatch_real_mmu_sv39_smoke` / `666666` |
| 目标规模 | `MEMBLOCK_MAIN_TRANS_NUM=10000` |
| 失败时刻 | `537.800ns` |
| 失败日志 | `mem_ut/ver/ut/memblock/sim/rm_sv39_10k_inflight_fault_20260828/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.log` |
| FSDB | `mem_ut/ver/ut/memblock/sim/rm_sv39_10k_inflight_fault_20260828/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.fsdb` |
| 可读 VCD | `/nfs/home/lixiangrui/memblock_inflight_fault_20260828.vcd` |
| RTL 修改 | 无；本记录只定义 UVM/RM 修复边界 |

> 更新说明（2026-08-28）：本文第 5 节的 replay-drain 方案已不再作为实现依据。
> 二次回归证明 `STA IQ hit=0` 后不一定存在 raw STA writeback；最新结论、波形证据
> 和替代方案见
> `AI_DOC/analysis/framework_design/memblock_sta_replay_late_fault_tombstone_rm_issue_analysis_20260828.md`。

## 1. 术语与判定范围

| 术语 | 当前含义 | 代码落点 | 本次示例 |
| --- | --- | --- | --- |
| `STA` | store address 子操作，使用 `MEMBLOCK_ISSUE_TARGET_STA`。 | `status_transaction.sta_dispatched` | UID25 的地址侧发射。 |
| `IQ feedback` | StoreUnit 返回给 issue queue 的慢反馈；`hit=0` 表示该次 STA 需要重放，而不是一条新的 ROB 写回。 | `io_mem_to_ooo_staIqFeedback_*`、`convert_raw_iq_feedback()` | SQ `0/10` 的 feedback。 |
| `raw writeback` | DUT 顶层 `writebackSta_*` 的原始观察值，RM 必须先绑定本次 issue snapshot 才能处理。 | `convert_raw_int_wb()` | ROB `0/138` 的 `writebackSta_0`。 |
| `replay drain` | 已收到 STA replay 请求，但旧流水线实例仍可能送出一条 raw writeback 的短暂阶段。此阶段保持旧 snapshot，等待该 raw 被消费后才清 dispatched 并允许重发。 | 本方案新增的 STA 状态字段/API | `520.3ns` 到 `530.3ns`。 |
| `issue snapshot` | 某次真实 fire 对应的 issue epoch、replay sequence 和当前实例 flush epoch。 | `mark_issue_snapshot()` | UID25 STA fire 后的旧实例。 |

本文只判断观测顺序与 RM 状态机的归属。若修复后仍出现“没有真实 STA fire、没有当前 snapshot，却从 DUT 产生 STA raw”的现象，才升级为 RTL 候选并按用户要求启动独立 subagent 复核。

## 2. 可复现事实

失败原文：

```text
UVM_FATAL @ 537.800ns ... [INT_WB_ATTACH]
writeback target was not dispatched: uid=25 target=2
```

同一日志还显示：

```text
507.800ns  normal pass uid=25 target=3 ... rob=0/138 ... sq=0/10
515.300ns  dispatch issue fire sta_port=0
537.800ns  INT_WB_ATTACH writeback target was not dispatched: uid=25 target=2
```

VCD 的关键顶层路径如下：

```text
top_tb.U_MEMBLOCK.io_ooo_to_mem_issueSta_0_valid
top_tb.U_MEMBLOCK.io_ooo_to_mem_issueSta_0_ready
top_tb.U_MEMBLOCK.io_ooo_to_mem_issueSta_0_bits_uop_robIdx_{flag,value}
top_tb.U_MEMBLOCK.io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_{flag,value}
top_tb.U_MEMBLOCK.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_{valid,bits_hit,bits_sqIdx_*}
top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_{valid,bits_uop_robIdx_*}
top_tb.U_MEMBLOCK._inner_StoreUnit_0_io_{feedback_slow,stout}_*
```

| 时间 | 波形/日志事实 | RM 当时应保持的语义 |
| --- | --- | --- |
| `510.3ns` | `issueSta_0.valid=1`、`ready=1`，payload 为 ROB `0/138`、SQ `0/10`。 | UID25 STA 已进入真实握手窗口。 |
| `515.3ns` | driver 打印 `dispatch issue fire sta_port=0`，StoreUnit 接到同一 ROB/SQ。 | 建立 UID25 STA issue snapshot，并置 `sta_dispatched=1`。 |
| `520.3ns` | `staIqFeedback_0.valid=1`、SQ `0/10`、`hit=0`。 | 这表示请求 replay；旧实例的 raw 输出仍可能在流水线中。 |
| `530.3ns` | `writebackSta_0.valid=1`，ROB 为 `0/138`。 | 该 raw 属于尚未 drain 的旧 STA snapshot，不能当作当前 replay 实例的正常 pass。 |
| `537.8ns` | adapter 根据 ROB 找到 UID25，但 `sta_dispatched=0`，触发 fatal。 | 旧 snapshot 已被 replay 路径过早删除。 |

## 3. 根因

当前流程把 `iq_feedback_failed` 立即写入 `exception_event_q`。随后
`exception_redirect_replay_handler::handle_replay_event()` 调用
`common_data_transaction::mark_replay_pending()`，该函数立即清零：

```text
status.sta_dispatched
status.sta_writeback
status.sta_issue_feedback_success
status.sta_pass
```

因此 `dispatch_monitor_event_adapter::attach_current_issue_snapshot()` 在后到的
`writebackSta_0` 上无法通过 `target_dispatched()` 检查。这个 fatal 并不表示 DUT
无输入地产生写回：同一 ROB/SQ 已在此前真实 fire，且 StoreUnit 输出和 IQ feedback
均能在波形中反查到该实例。

`feedbackSlow.hit=0` 也不能被 RM 简化成“旧流水线绝不会再出现 raw writeback”。它是
issue queue 的重放反馈；RM 必须在保留旧 snapshot 的条件下处理其后到的流水线观察值。

## 4. 为什么不是 RTL 问题

当前证据同时满足以下框架问题特征：

1. `issueSta_0.valid && ready` 和 driver fire 都已存在，UID25 的输入来源明确。
2. raw writeback ROB 与已 fire 的 UID25 ROB 完全一致，且 StoreUnit 内部路径可追踪。
3. 失败发生在 adapter 的软件状态检查，而不是 DUT assertion、X/Z、错误 ROB 或错误 SQ。
4. `mark_replay_pending()` 的已有实现正好会清除 fatal 所需的 `sta_dispatched` 位。

所以本轮不启动 RTL subagent，也不修改 RTL。后续只有在 replay-drain 修复后，波形显示
没有对应 STA fire 或旧 snapshot、却仍出现无法匹配的 STA raw 时，才按用户规定升级为
RTL 候选并启动独立复核。

## 5. 最优修改方案

采用“延迟应用 replay，而不是宽松地接受无 snapshot 写回”的方案：

```text
STA fire
  -> sta_dispatched=1，建立旧 issue snapshot
  -> STA IQ feedback(hit=0)
  -> 设置 sta_replay_drain_pending，不清 dispatched、不 bump replay_seq
  -> 对应 raw STA writeback 到达
  -> 验证旧 issue_epoch/replay_seq 后将该 raw 作为 replay drain 消费
  -> 调用原有 mark_replay_pending()，清旧状态、bump replay_seq、允许重发
```

设计要点：

- 只影响 `STA + IQ feedback hit=0`；Load、STD、正常 STA hit 和 fault path 保持原行为。
- replay drain 期间只有一个旧 generation，重发尚未开放，避免 raw ROB 不含 generation
  时把旧输出错误绑定到新实例。
- raw STA 若带异常，异常优先：清 drain 标志并走既有 `mark_target_fault()`，不能把真实
  fault 降级为 replay。
- raw 与 pending descriptor 的 epoch/replay/key 不匹配仍是 framework 一致性错误，保留
  `uvm_fatal`，不静默 drop。
- 不添加固定延迟或全表扫描；状态只保存在 UID 的 `status_transaction`，查询为 O(1)。

对应的可执行 plan：

```text
AI_DOC/plan/test_framework/plan/undo/memblock_sta_replay_drain_writeback_plan_20260828.md
```

## 6. 验证与波形复查点

修复后同一 seed 必须观察到：

```text
STA IQ feedback hit=0 uid=25 ... enter replay drain
STA replay drain consume uid=25 ... issue_epoch=<old>
```

并且不再出现 `INT_WB_ATTACH writeback target was not dispatched: uid=25 target=2`。
随后继续 Sv39/U 态 10000 笔激励；每个新的 RM/framework 报错按本任务要求建立独立分析、
plan、修复和单独 commit。若确认 RTL 问题，则记录错误点和本节波形路径后停止，不改 RTL。
