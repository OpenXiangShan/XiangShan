# `memblock_issue_dispatch_base_sequence.sv` 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_issue_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_xaction.sv`

## 1. Sequence 职责

该 sequence 把 `issue_queue_scheduler` 选出的 scalar LOAD/STA/STD 候选转换成 V2
`issueLda/issueSta/issueStd` transaction，并只对 driver 确认 fire 的 item 推进
queued/dispatched/issue-epoch 状态。

它不负责 LSQ admission、最终 writeback/commit/deq，也不改变 redirect/replay 的 queue 恢复
算法。scalar testcase 不支持 `issueVldu`；vector default sequence 被移除，vecissue driver 对
非零 valid fail-fast。

## 2. 每拍控制流

```text
drive_dispatch_issue_loop：
  调用 route_all_ready_uids，在 compile slot 上界内补 route；
  调用 send_issue_cycle，完成本拍选择、驱动和 fire 标记；
  调用 advance_issue_queue_delays，推进尚未 ready 的 queue item；
  调用 has_pending_issue_work，只读取三条 queue size；
  如果 global_stop_requested，正常退出；
  如果本拍有 fire，清 no-progress 计数；
  否则如果 queue 仍有 pending，累计计数并按阈值整数倍报告 uvm_error，但不退出；
  否则 queue 已空，清计数并合法等待 writeback/commit/deq/terminal。
```

no-progress 不再把“queue 已空但系统仍在 drain”误判为 issue stall，也不作为正常退出条件。

## 3. Candidate 到物理端口

`assign_issue_items()` 对每类 target 使用候选数组下标作为 local pipe。字段赋值入口
`issue_field_assigner::assign_issue_item_fields()` 先检查 FuType/fuOpType/behavior/target 合法
矩阵，再写真实 V2 split port。

| target | local pipe | DUT 端口 | mask bit |
|---|---:|---|---:|
| LOAD | `i` | `issueLda_i` | `MEMBLOCK_DUT_LOAD_PORT_BASE + i` |
| STA | `i` | `issueSta_i` | `MEMBLOCK_DUT_STA_PORT_BASE + i` |
| STD | `i` | `issueStd_i` | `MEMBLOCK_DUT_STD_PORT_BASE + i` |

`port_idx_for_item()` 统一完成 item 到 mask bit 的转换，并检查 local pipe、port count 和 mask
width。xaction 的 `memblock_dispatch_fired_mask` 宽度也由同一 compile-time base/count 表达式
派生。

## 4. Driver ready/fire 合同

driver 先把 xaction 的 V2 split payload 放到 clocking block。之后：

- blocking 模式持续等待所有 valid port fire；每个 `valid && ready` 通过
  `record_dispatch_issue_fire()` 写入真实 fired-mask，sequence 返回后要求该 mask 覆盖本拍
  `candidate_mask`，不再用 all-ones mask 伪造 fire。
- nonblocking 模式只等待一个 sample 边界；只有本拍 `valid && ready` 的 port 写 mask，未 ready
  item 保留在 queue 中等待后续仲裁。
- 等待期间发生 redirect/flush 时，driver 清 remaining valid 并置
  `memblock_dispatch_aborted_by_redirect`。已确认 fire 的 mask 保留，未 fire port 不得推进状态。

`record_dispatch_issue_fire()` 和 `report_dispatch_issue_fire()` 使用 compile-time LOAD/STA/STD
base/limit，不再使用固定 `<=2`、`<=4`、`-3`、`-5`。

## 5. `send_issue_cycle()` 文字伪代码

```text
创建并清零 lintsissue xaction；
设置 wait_ready、nonblocking、ready_timeout、flush_epoch，并清 fired_mask；
若当前未被 global flush 阻塞：
  调用 scheduler 选择 LOAD/STA/STD 候选；
  再次检查 flush；
  调用 assign_issue_items，把候选写入 V2 split port并保存 fired_items；
start_item/finish_item，由 driver 驱动并回填 fired_mask/abort；

根据 fired_items 构造 candidate_mask；
effective_fired_mask = fired_mask 与 candidate_mask 的交集；
如果 fired_mask 含 candidate 之外的 bit，立即 fatal；
blocking 且没有 abort/flush 时，要求 effective_fired_mask 覆盖 candidate_mask，否则 fatal；
effective_fired_mask 非零时先调用 mark_fired_items，并置 has_fire=1；

如果 driver 因 redirect 中止，或 sequence 返回时 flush/epoch 已变化：
  只取消尚未确认 fire 的 candidate；已确认 fire 已在上一步推进；
  未命中的 item 不删除、不置 dispatched；
  返回；

正常结束时同样只使用 driver 返回的真实 fired_mask，不生成 all-ones mask。
```

## 6. `mark_fired_items()`

该函数使用 `port_idx_for_item()` 把 target-local pipe 转成参数化 mask bit。mask 未命中的 item
直接跳过；命中的 item 根据当前 flush 状态调用 `mark_issue_fire()` 或
`mark_issue_fire_already_accepted()`。只有 mark 成功后才允许走 STD 兼容 accept-pass 路径。

因此状态推进的唯一依据是 DUT 真实 fire，而不是候选存在、固定端口编号或 redirect 前曾经拉高
valid。

## 7. 参数与支持边界

- `MEMBLOCK_DISPATCH_ISSUE_SEQ_EN`：主动 issue sequence 总开关。
- `MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN`：选择单次 sample 或阻塞等待模式。
- `MEMBLOCK_DISPATCH_READY_TIMEOUT`：blocking ready 等待上限。
- `MEMBLOCK_LOAD/STA/STD_PIP_NUM_LIMIT`：testcase 行为使用量，统一受 compile pipe 数收敛。
- `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES`：pending queue stall 诊断周期，不控制正常退出。

物理 pipe 数、port base 和 mask width 不是 runtime plus 参数，统一来自
`memblock_compile_params.svh`。本轮不实现 vector issue、MOU/AMO/CBO completion、RM/checker 或
coverage，也不修改 pass/fail/terminal 主体算法。
