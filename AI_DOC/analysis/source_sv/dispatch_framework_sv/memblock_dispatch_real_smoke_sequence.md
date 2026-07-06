# memblock_dispatch_real_smoke_sequence.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`

## 1. 文件定位与使用场景

真实 DUT smoke 顶层服务 sequence。它本身更像“调度服务循环”，不是接口驱动者：它负责建主表、打开 monitor capture、循环处理 monitor 原始事件、把 ready uid route 到 issue queue，并判断所有 transaction 是否完成。真正对 DUT 发 valid/ready 的是各 agent default sequence。

输入是 plus/testcase 指定的主表规模和 op 权重，以及 monitor raw queue。输出是公共 status 的推进和最终 `end_test_check()`。

流程：

- `body()`：`build_main_table()`，然后 `service_real_dispatch_flow()`，最后 `end_test_check()`。
- `service_real_dispatch_flow()`：获取 lintsissue agent interface 作为 service clock，在每个下降沿调用 `service_monitor_once()` 和 `route_all_issue_queues()`，直到 `all_transactions_terminal_done()`。
- `service_monitor_once()`：collect CSR、writeback/IQ feedback、exception/redirect raw event；真实 monitor event 先由 `collect_monitor_event_batch()` 收集成同一批，再交给 `dispatch_monitor_batch_handler` 做 normalize、active redirect 过滤和同批 redirect-first 仲裁；未被 redirect 覆盖的非 redirect event 才进入 `writeback_status_handler`，`exception_event_q` 后续只由 `exception_redirect_replay_task()` 消费。
- `all_transactions_terminal_done()`：通过 `dispatch_progress.terminal_done_uid >= main_trans_num` 做 O(1) 完成判断，并请求 `global_stop_requested`。`terminal_done_uid` 能跨过 normal pass 的 `success=1 && terminal_done=1`，也能跨过允许 retire 的 fault/exception `success=0 && terminal_done=1`；`flushed`、`redirect_pending` 或 replay 中间态不会被当作合法终态。
- `report_unfinished_status()`：超时时打印 status 和关键 HDL path。

真实接口驱动不在这个 sequence 中直接做，而是通过 testcase 配置的 agent default sequence 并行运行。

## 2. 调度关系与参数数据流

`memblock_dispatch_real_smoke_sequence` 是真实 DUT dispatch flow 的顶层服务循环。它和四个真实接口 sequence 并行工作，但职责不同：

- 本 sequence 负责建表、置 `main_table_ready`、服务 monitor、处理 replay/redirect/commit 状态和最终检查。
- `memblock_lsqenq_dispatch_sequence`、`memblock_lintsissue_dispatch_sequence`、`memblock_lsqcommit_dispatch_sequence`、`memblock_l2tlb_base_sequence` 负责各自 agent 的真实 xaction 驱动。

调度步骤：

| 步骤 | 动作 | 影响 |
|---|---|---|
| `pre_body()` | 继承 base sequence 初始化 helper 和 `seq_csr_common` | 所有并行 sequence 看到同一份参数快照 |
| `build_main_table()` | 随机或手动生成主表 | 写入主表/status，最终置 `main_table_ready` |
| `service_real_dispatch_flow()` | 每个 service clock 下降沿服务 monitor 和 issue route | 将 DUT 反馈回写公共表，并把 ready uid 推向 issue queue，直到所有 uid terminal_done |
| `service_monitor_once()` | 同拍 raw event 统一收集，redirect/replay 优先，再普通 pass | 避免 memoryViolation/redirect 与 writeback 同拍时先把旧动态实例标成 pass |
| `end_test_check()` | 检查 terminal_done/active/queue 状态 | 决定 testcase 是否完成 |

参数数据流：

- `MEMBLOCK_MAIN_TRANS_NUM`、op class 权重、地址/PTE/send priority/delay 权重由 base sequence 在建表时消费。
- `MEMBLOCK_LSQENQ_SEQ_EN`、`MEMBLOCK_DISPATCH_ISSUE_SEQ_EN`、`MEMBLOCK_LSQCOMMIT_SEQ_EN`、`MEMBLOCK_L2TLB_SEQ_EN` 不由本 sequence 驱动接口，而是由对应 agent sequence 自己读取。
- monitor 事件通过 `memblock_sync_pkg` 原始队列进入 `dispatch_monitor_event_adapter`，再写回 `common_data_transaction`。

该 sequence 是真实 flow 的“状态服务者”，不是某个单独 DUT 接口的 driver。
