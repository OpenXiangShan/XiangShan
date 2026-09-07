# MemBlock STD value-only 无主写回问题分析（2026-08-28）

| 项目 | 内容 |
| --- | --- |
| 状态 | 已完成波形取证，确认是 UVM framework 的 issue-fire 记账时序问题；尚未发现 RTL 问题 |
| 适用版本 | V2，分支 `mem_ut_uvm_v2` |
| 首次失败场景 | `basicTest` / `memblock_dispatch_real_smoke_vseq` / `tc_dispatch_real_mmu_sv39_smoke` / seed `666666` |
| 失败日志 | `mem_ut/ver/ut/memblock/sim/rm_sv39_10k_std_diag_20260828/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.log` |
| 首次失败时刻 | `397.800ns` |
| 关联目标 | Sv39、U 态、10000 笔主表请求回归 |
| 波形 | `mem_ut/ver/ut/memblock/sim/rm_sv39_10k_std_diag_20260828/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.fsdb` |
| RTL 修改 | 禁止；本分析仅涉及 UVM 测试框架的 issue-fire 与 event 归属逻辑 |

## 1. 术语与判定范围

| 术语 | 本文含义 | 对应对象 | 示例 |
| --- | --- | --- | --- |
| `STD` | Store Data execution/writeback 通路；V2 顶层只导出 `robIdx.value`，没有 ROB wrap flag。 | `io_mem_to_ooo_writebackStd_0/1_*` | raw event 只报告 value `10`。 |
| value-only | raw event 缺少完整 ROB key，只能在 flag=0/1 两个候选中反查。 | `dispatch_raw_int_wb_t.rob_value_only_without_flag` | `rob_value=10` 可能对应 `0/10` 或 `1/10`。 |
| active map | 当前动态实例的完整 ROB key 到 UID 映射；实例 retire 后会被删除。 | `uid_by_active_rob` | 当前 active map 中没有 `0/10` 和 `1/10` 时，STD 无法直接归属。 |
| stale event | 已被 fault retire、redirect/reissue 或其他明确生命周期终结覆盖的旧实例端口输出；它不得更新新实例的 pass/fault 状态。 | raw int-WB queue 与状态生命周期 | 老 STD output 晚于该 UID 的 active map 删除到达。 |
| tombstone | 在删除 active map 前保存的有限旧实例摘要，只用于证明某个 value-only raw 是 stale 并丢弃，不恢复 UID、不改变状态。 | 后续候选实现：`common_data_transaction` | 必须包含完整 ROB key、目标已 issue 事实和结束原因。 |
| 无主写回 | raw event 到达时，两个完整 ROB flag 候选均不能形成当前合法 STD owner。 | `resolve_std_uid_by_rob_value_only()` | 当前 fatal `INT_WB_STD_KEY`。 |

本文当前只回答“无主 STD raw 是测试框架归属问题还是 RTL 非法输出”。在未完成 raw 采样时刻、两组 ROB map、UID 生命周期和波形交叉验证之前，不把它标记为 RTL 问题，也不允许修改 RTL。

## 2. 已观察到的失败

第一次 10000 笔回归在主表仅完成约三个 RM compare 时停止，最终错误为：

```text
UVM_FATAL @ 397.800ns
../seq/./base_seq_help/dispatch_monitor_event_adapter.sv(430)
[INT_WB_STD_KEY] STD ROB value=10 has zero valid active STD flag candidates
```

同一日志中的相邻事实如下：

```text
392.800ns  [WB_STATUS] fault feedback uid=52 target=1 rob=1/5 exception_vec=0x2020
392.800ns  [EXC_REDIRECT] consume fault recovery event uid=52 target=1 issue_epoch=28 replay_seq=0
395.300ns  dispatch issue fire sta_port=0
395.300ns  dispatch issue fire std_port=0
397.800ns  [INT_WB_STD_KEY] STD ROB value=10 has zero valid active STD flag candidates
```

已完成的 UID 0、1、2 的 RM compare 均为 PASS。Load 的 `0x2000 -> 0x2020`、Store 的
`0x8000 -> 0x8080` raw vector 差异已经作为架构 cause 相同的诊断，不是本次终止根因。

## 3. 当前源码事实

1. `dispatch_monitor_event_adapter::resolve_std_uid_by_rob_value_only()` 仅 probe `flag=0` 和
   `flag=1` 的 active ROB map。两个候选都无效时直接 fatal。
2. `fill_current_issue_snapshot()` 除了要求 active map 命中外，还要求实例 active、未 terminal、
   未 flushed/redirect/kill、STD 已 dispatched，并且 SQ owner 仍有效。
3. `common_data_transaction::retire_active_uid()` 会删除 `uid_by_active_rob` 和 SQ map。fault retire
   与 redirect/reissue 都可能在晚到 STD raw 被 adapter 消费前完成这个删除。
4. V2 `writebackStd` 顶层没有 ROB flag。软件不能在 active map 已删除后仅凭 value 重新推导完整
   UID；把 value 盲目匹配到新实例会造成更严重的误记账。

## 4. 修正后的波形取证

本轮实际读取的 VCD 为：

```text
/tmp/memblock_std_diag_20260828.vcd
```

此前把 `dut_inst.sv` 内用于连接的顶层 `reg` 误当作 DUT-facing 信号。实际应观察
`top_tb.U_MEMBLOCK` 层级中的端口。对应 VCD identifier 如下：

| 信号 | VCD identifier | 关键变化 |
| --- | --- | --- |
| `top_tb.U_MEMBLOCK.io_ooo_to_mem_issueStd_0_valid` | `I7` | `390.300ns` 置 1，`395.300ns` 清 0。 |
| `top_tb.U_MEMBLOCK.io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value` | `L7` | 发射 payload 的 ROB value 为 `10`。 |
| `top_tb.U_MEMBLOCK.io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_value` | `N7` | 同一 payload 带 SQ value `12`。 |
| `top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackStd_0_valid` | `z*` | `390.300ns` 为 1，`395.300ns` 清 0。 |
| `top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackStd_0_bits_uop_robIdx_value` | `{*` | 同一写回携带 value `10`。 |

时序事实为：

```text
390.300ns  DUT-facing issueStd_0_valid=1，ROB value=10；writebackStd_0_valid 同时为 1。
395.300ns  driver 观察到 valid&&ready，打印 "dispatch issue fire std_port=0"，并撤销 valid。
397.800ns  main service 消费 raw STD writeback；value-only resolver 找到 flag=1/10 的 UID 57，
           但 status.std_dispatched=0、std_issue_epoch=0、std_instance_flush_epoch_valid=0，触发 fatal。
```

`UID 57` 在 fatal 时仍为 `active=1`、`enq=1`、`terminal=0`、`flushed=0`、
`redirect_pending=0`、`issue_killed=0`。因此该事件不是 active map 已删除后的旧实例，
也不是真正 redirect 后的迟到输出。`EXC_REDIRECT` 日志只表示 fault recovery 消费器的类别名，
本次没有 `request_redirect_flush()` 或 `apply_redirect_flush()` 证据。

## 5. 根因：真实握手与状态记账的时序断层

`lintsissue_agent_agent_driver::clear_ready_dispatch_issue_ports()` 在 `valid && ready` 观察点调用
`record_dispatch_issue_fire()`，当前只回填 `tr.memblock_dispatch_fired_mask`。真正写入下列 framework
状态的调用发生在 `memblock_issue_dispatch_base_sequence::send_issue_cycle()` 的 `finish_item(tr)` 返回之后：

```text
issue_queue_scheduler::mark_issue_fire()
  -> common_data_transaction::mark_issue_snapshot()
  -> delete_issue_queue_entry()
  -> status.std_dispatched = 1
```

在本失败中，DUT output monitor 已经在 driver 返回 `finish_item()` 之前采到 STD writeback；main service
在 `397.800ns` 处理它时，sequence 侧还没有执行 `mark_fired_items()`。因此 adapter 对 UID 57 的严格
检查正确地拒绝了“尚未 dispatched”的候选，`INT_WB_STD_KEY` 是 framework 时序缺陷的暴露点，
不是 RM 期望模型或 RTL functional mismatch。

## 6. 已排除的 tombstone 方向

原先的 tombstone 方案只适用于“已经终结且曾真实发射的旧 STD 实例在 map 删除后迟到写回”。本次
取证与此前假设相反：candidate 仍是当前 active UID，且尚未被标记为真实发射。因此添加 tombstone
会掩盖真正的 issue bookkeeping 延迟，并不能建立 `std_issue_epoch`、target instance flush epoch 或
STD 的状态所有权。

本问题不采用 tombstone。value-only resolver 继续保留严格的双 flag/current-status 检查，避免未知
DUT 输出被静默丢弃。

## 7. 最优修复方案

关联可执行 plan：

```text
AI_DOC/plan/test_framework/plan/undo/memblock_dispatch_issue_fire_boundary_bookkeeping_plan_20260828.md
```

方案把状态更新移动到真实 `valid && ready` 的逻辑边界，但不允许 driver 直接写
`common_data_transaction`：

1. 复用每个 lintsissue xaction 已有的 `memblock_dispatch_fired_mask`，作为 driver 到当前 sequence 的
   持久 fire 状态。
2. driver 在 `record_dispatch_issue_fire()` 设置 fired mask；该点仍是唯一的真实 handshake 判定点。
3. sequence 在 `finish_item()` 前启动 watcher。watcher 等待“candidate 内且尚未记账”的 fired bit，
   因此不会因 UVM 同一 delta 的瞬时 event 调度顺序漏掉 fire；随后它使用本轮已冻结的
   `memblock_issue_q_item_t` candidate descriptor 调用原有
   `issue_queue_scheduler::mark_issue_fire()` 或 redirect 场景的
   `mark_issue_fire_already_accepted()`。
4. `finish_item()` 返回后只对未被 watcher 处理的 fired bit 做一次幂等补偿；不再为已处理 port 分配
   第二个 issue epoch、重复出队或重复置 `*_dispatched`。
5. issue xaction 固定 `pre_pkt_gap=0` 与 `post_pkt_gap=0`，使本框架周期 item 不携带无关随机 gap。

该方案不增加全表扫描、全局 raw queue 或第二套 UID map。每次 watcher 唤醒只遍历当前周期最多的 scalar
issue port candidate，状态真源仍是既有 `common_data_transaction` 和 `issue_queue_scheduler`。

## 8. 后续判定规则

修复后重新运行相同的 10000 笔 Sv39/U 态场景：

- 若出现新的 RM/framework 报错，继续在分析文档中记录日志、波形路径、根因和修复方案，然后修改
  非 RTL 测试代码并重跑。
- 只有当波形证明 DUT 在没有任何真实 `valid && ready` 输入、且 framework 已完成当前实例记账的前提下
  仍产生无法归属的输出，才把问题升级为候选 RTL 问题；届时必须启动独立 subagent 复核。
- RTL 代码保持不修改；若 subagent 确认 RTL 问题，本任务只记录出错点、波形路径和分析结论后结束。
