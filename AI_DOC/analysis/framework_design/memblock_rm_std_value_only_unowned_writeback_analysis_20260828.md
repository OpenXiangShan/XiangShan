# MemBlock STD value-only 无主写回问题分析（2026-08-28）

| 项目 | 内容 |
| --- | --- |
| 状态 | 正在复现与取证，尚未判定 RTL 问题 |
| 适用版本 | V2，分支 `mem_ut_uvm_v2` |
| 首次失败场景 | `basicTest` / `memblock_dispatch_real_smoke_vseq` / `tc_dispatch_real_mmu_sv39_smoke` / seed `666666` |
| 失败日志 | `mem_ut/ver/ut/memblock/sim/rm_sv39_10k_20260828/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.log` |
| 首次失败时刻 | `397.800ns` |
| 关联目标 | Sv39、U 态、10000 笔主表请求回归 |
| RTL 修改 | 禁止；本分析仅涉及 UVM 测试框架的 event 归属逻辑 |

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

## 4. 不能直接判定为 redirect 或 RTL 的原因

首次日志没有 `request_redirect_flush()`、`apply_redirect_flush()` 或 redirect drive 的可见记录，
只有 `EXC_REDIRECT` 类对 fault recovery event 的消费记录。该类名不等价于发生了真实 redirect。
因此当前有三种尚未排除的来源：

| 假设 | 含义 | 目前证据 | 后续判定方式 |
| --- | --- | --- | --- |
| H1：已终结旧实例的迟到 STD | fault retire 或真正 redirect 已删 active map，但旧 execution output 晚到。 | 与 value-only 接口限制一致。 | 打印/波形确认该 value 对应 UID 的 `std_dispatched`、终结原因和 map 删除时序。 |
| H2：测试框架生命周期或归属缺陷 | framework 过早删除 map，或没有为 fault/redirect 后的 value-only output 保留有限证明。 | 当前 fatal 发生在 adapter，而非 RM compare。 | 诊断显示 raw 可唯一关联到已终结且确实发射过 STD 的旧实例。 |
| H3：RTL 无法解释的输出 | DUT 在没有任何曾发射、可证明已终结的 STD owner 时输出 valid。 | 当前证据不足。 | 诊断和 FSDB 均无法将 raw value 关联到有效历史实例时，启动独立 subagent 做 RTL review。 |

## 5. 本轮取证与波形计划

第一次命令使用了 `wave=1`，而 VCS Makefile 只有 `wave=on` 才传入 FSDB/UCLI 参数，因此原目录
`rm_sv39_10k_20260828/wave/` 为空，不能据此得出无波形结论。

本轮先增加不改变行为的 diagnostic fatal 信息，至少输出：

- raw STD value、raw sample flush epoch、当前 global flush epoch；
- flag=0/1 的 active ROB map 是否存在；
- 若存在，UID 的 `active/enq/std_dispatched/std_writeback/fault/terminal/flushed/redirect_pending/issue_killed`；
- 对应 `std_issue_epoch` 和 STD instance flush epoch；
- 当前 active redirect 和 cancel record 数量。

然后以 `wave=on` 重跑同一 seed，保留以下路径的 FSDB：

```text
mem_ut/ver/ut/memblock/sim/rm_sv39_10k_std_diag_20260828/wave/
```

波形至少检查：

```text
top_tb.u_memblock__io_mem_to_ooo_int_wb_agent_if.io_mem_to_ooo_writebackStd_0_valid
top_tb.u_memblock__io_mem_to_ooo_int_wb_agent_if.io_mem_to_ooo_writebackStd_0_bits_uop_robIdx_value
top_tb.u_memblock__io_mem_to_ooo_int_wb_agent_if.io_mem_to_ooo_writebackStd_1_valid
top_tb.u_memblock__io_mem_to_ooo_int_wb_agent_if.io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value
top_tb.u_memblock__lintsissue_agent_if.io_ooo_to_mem_issueStd_0_valid
top_tb.u_memblock__lintsissue_agent_if.io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value
top_tb.u_memblock__lintsissue_agent_if.io_ooo_to_mem_issueStd_1_valid
top_tb.u_memblock__lintsissue_agent_if.io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value
```

实际 hierarchy 以 FSDB signal list 为准；上述端口名来自当前 `dut_inst.sv`/interface 连接。

## 6. 候选最优修复及安全边界

若 H1/H2 得到确认，最优方案是在 `common_data_transaction` 的生命周期 owner 内保存受硬件 ROB
容量约束的 STD tombstone，而不是在 adapter 中对所有零候选 warning/drop：

1. 在 active ROB/SQ map 删除前，只为“已经 `std_dispatched` 但尚未完成 STD writeback”的旧实例登记完整 ROB key、UID、SQ key、dynamic/replay/issue epoch、实例 flush epoch 和终结原因。
2. 重新激活任意相同 ROB value 的新实例时删除该 value 的历史，避免 value-only raw 被旧记录错误吸收。
3. adapter 在两个 current candidate 都不存在时才查询该表；只有两个 flag 中恰好一个 tombstone 与 raw sample epoch、旧实例 issue epoch 和终结边界一致，且没有当前 active owner 时，记录 `INT_WB_STD_STALE_DROP` 并丢弃。
4. 两个历史候选、存在新的 active owner、sample epoch 不在旧实例有效区间、或没有历史候选，全部保留 fatal。这保证未知 DUT output 不会被静默吞掉。
5. 表按完整 ROB key 关联存储，最大键空间受 `2 * MEMBLOCK_ROB_SIZE` 约束；查询固定两次 map probe，不扫描 10000 笔主表。

H3 得到确认前不得按“RTL 问题”结束；若满足 H3，必须启动独立 subagent 复核 RTL，再在本文补充出错信号、时间、FSDB 路径和复核结论，且不修改 RTL。

## 7. 当前结论

当前是测试框架 event 归属 fatal，尚无足够证据证明 RTL 行为非法。下一步先完成同 seed 的诊断与波形复现；只有诊断证明某一终结实例可唯一解释 raw 时才实施 tombstone 修复，否则转入 RTL 独立复核流程。
