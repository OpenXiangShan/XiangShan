# memblock_lsqcommit_dispatch_sequence.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`

## 1. 文件定位与使用场景

真实 lsqcommit pendingPtr 驱动 sequence。它负责告诉 DUT 当前 ROB commit pendingPtr，并在软件侧标记对应 uid 已 ROB commit。LQ/SQ entry 的释放不是它单独决定，而是还要等 ctrl monitor 捕获到 DUT deq。

输入是 status 表中的 commit candidate。输出是 `lsqcommit_agent_agent_xaction` 和 `rob_commit` 状态。真正 retire 还依赖 `apply_dut_lq_deq/apply_dut_sq_deq()`。

关键 task/function：

- `drive_lsqcommit_loop()`：global stop 前常驻运行，周期性尝试发 pendingPtr/commit/flushSb；global stop 后等待 pending flushSb drain 完再退出。
- `send_lsqcommit_cycle()`：`build_lsqcommit_xaction()` 后 start/finish item，有 commit 则 mark batch。
- `wait_clock_tick()`：统一等待 `@(posedge lsqcommit_vif.clk)`，替代旧的 `#1`
  time-step 等待，保证 sequence 等待行为和 DUT clock 对齐。
- `drive_flushsb_if_needed()`：处理 directed 或 scheduled `flushSb` request，驱动一个 cycle 后等待 `sbIsEmpty`。

## 2. 调度关系与参数数据流

该 sequence 是真实 DUT flow 中负责 LSQ commit 端口刺激的 sequence。它挂在 `env.u_lsqcommit_agent_agent.sqr.main_phase`，由 `MEMBLOCK_LSQCOMMIT_SEQ_EN` 控制是否真正驱动。

调度关系：

| 阶段 | 条件 | 动作 | 输出 |
|---|---|---|---|
| 等待主表 | `main_table_ready=1` | 进入 commit loop | 可读取 status 表 |
| 寻找 candidate | uid active、target pass、无 fault/replay/redirect | `lsq_commit_handler` 选择 ROB 顺序上可提交 uid | commit batch |
| 构造 xaction | commit batch 或 pendingPtr 需要推进 | 填 `lsqcommit_agent_agent_xaction` | DUT commit/pendingPtr payload |
| 驱动接口 | `start_item/finish_item` | 发给 `lsqcommit_agent` driver | DUT 看到 commit 刺激 |
| 软件标记 | 有 commit batch | `mark_rob_commit_batch()` | status.rob_commit 更新 |
| 等待释放 | DUT ctrl monitor 捕获 deq/cancel | `apply_dut_lq_deq/apply_dut_sq_deq()` | LQ/SQ active map 释放，uid retire |

参数数据流：

- `MEMBLOCK_LSQCOMMIT_SEQ_EN`：总开关。
- `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES`：控制 commit loop 连续无 commit/flushSb progress 时的 warning 周期；只用于 debug，不作为退出条件。
- `MEMBLOCK_FLUSHSB_SEQ_EN`：允许 directed `flushSb` 机制；默认关闭，不会自动发 pulse。
- `MEMBLOCK_FLUSHSB_REQUEST_CYCLE`：非 0 时，在公共数据层登记为 scheduled pending；commit sequence 的 0-based cycle index 到达该值后发起一次 `request_flushsb()`。这是最小 directed 入口；没有显式 request 时即使 `MEMBLOCK_FLUSHSB_SEQ_EN=1` 也不自动发。等待该 request cycle 期间不计入 idle-stop，real smoke 也会因为公共 scheduled pending 状态而不会提前收尾；若 due cycle 被 redirect/global flush 挡住，scheduled pending 保留，等 flush 解除后继续尝试。
- `MEMBLOCK_FLUSHSB_TIMEOUT`：发出 `flushSb` 后等待 `sbIsEmpty=1` 的 service-cycle timeout。
- ROB 顺序判断不使用普通 int 递增，而通过 `rob_order_util` 和 `memblock_rob_key_t` 处理 flag/value/wrap 语义。
- LSQ free count 与 active LQ/SQ map 最终以 DUT monitor 的 deq/cancel 事件为准。

状态输出：

- `rob_commit`：表示框架已经向 DUT commit 端口推进该 uid。
- `lsq_deq/lq_deq/sq_deq`：不由本 sequence 单独完成，依赖 monitor 事件释放。
- `success`：必须等 ROB commit 和 LSQ deq 条件都满足后，才由公共数据层 retire。

边界：

- 该 sequence 不是 writeback/pass 生成者；它只选择已 pass 的 uid 推 commit。
- 它不能替代 DUT ctrl deq monitor。若 commit 发出但 DUT 没有释放 LQ/SQ，uid 仍不能 retire。
- `flushSb` 只对当前 RTL 暴露的 `io_ooo_to_mem_flushSb` 做 directed pulse 和 `sbIsEmpty` 等待，不建模 SBuffer 内部状态机，也不依赖 `mmioBusy`。
- scheduled `flushSb` 状态由 `common_data_transaction` 维护，不保存在本 sequence 私有变量中，避免 service loop 和 commit sequence 对未来 request 是否仍 pending 的判断不一致。commit sequence 在每个 cycle 的 early flush return 前也会调用 due 检查，due 后若暂时不能驱动，`flushsb_pending` 会保留到下一次非 blocked cycle；scheduled/pending/waiting 任一状态存在时，即使顶层已置位 global stop，也要继续 drain，不能提前退出。
