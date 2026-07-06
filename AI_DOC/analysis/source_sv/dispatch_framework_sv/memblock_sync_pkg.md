# memblock_sync_pkg.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`

`memblock_sync_pkg` 是 memblock UT 中用于 dispatch real smoke 的共享同步包和 monitor raw queue。它不是 transaction，也不直接驱动 DUT；它更像 monitor 到 dispatch 公共框架之间的轻量邮箱，同时保存少量跨 agent/sequence 共享的全局状态。

## 1. 文件定位与使用场景

输入来自各 monitor 或 connect 侧调用的 `push_raw_*()`，输出给 `dispatch_monitor_event_adapter` 调用的 `pop_raw_*()`。`dispatch_monitor_capture_en` 是关键控制开关，避免非 dispatch 场景或 testcase 收尾阶段继续把 raw event 塞进公共状态机。

主要使用链路：

| 阶段 | 使用方式 |
|---|---|
| reset/backend done | `top_tb.sv` 更新 `reset_backend_done`，driver/monitor/sequence 用它判断 DUT 是否进入可工作阶段。 |
| real smoke active | real smoke testcase 设置 `dispatch_real_smoke_active`，部分 driver 根据它选择 dispatch 专用默认值。 |
| L2TLB responder active | `L2tlb_agent_connect.sv` 更新 `l2tlb_responder_active`，L2TLB driver/sequence 用它决定是否接管 DTLB/L2TLB response。 |
| flush 协同 | `common_data_transaction` 更新 `dispatch_flush_in_progress` 和 `dispatch_flush_epoch`，LSQ enqueue driver 用它中止被 redirect 冲掉的 admission。 |
| monitor raw 采集 | writeback、IQ feedback、ctrl、CSR、fence monitor 调用 `push_raw_*()`；redirect input monitor 不反馈 recovery。 |
| service loop drain | `dispatch_monitor_event_adapter` 周期性 `pop_raw_*()` 并转换成公共 event/status 更新。 |

## 2. 全局同步字段

| 字段 | 含义 | 使用场景 |
|---|---|---|
| `reset_backend_done` | backend reset/初始化完成标志。 | driver、monitor、L2TLB responder、base sequence 等等待该标志后才进入 dispatch 活动。 |
| `dispatch_flush_in_progress` | dispatch 框架正在处理 redirect/flush。 | issue scheduler 停止发射；LSQ enqueue driver 检测到后中止当前 admission。 |
| `dispatch_flush_epoch` | flush 版本号。 | sequence 发出 xaction 时保存 epoch，driver 若发现 epoch 变化则认为该请求被 redirect 取消。 |
| `dispatch_service_cycle` | dispatch service loop 的软件周期计数。 | redirect freeze、flushSb 等待、PTW-back replay 等 timeout 均使用该计数，避免直接依赖 `$time`。 |
| `dispatch_monitor_capture_en` | monitor raw queue 采集开关。 | 只在 dispatch real smoke 期间采集；end check 前关闭并清残留。 |
| `l2tlb_responder_active` | 当前 L2TLB agent 是否接管 DTLB/L2TLB response。 | L2TLB driver idle ready 行为和 L2TLB sequence 启动检查依赖它。语义是上游 DTLB responder，不是 L2Cache/PTW 下游模型。 |
| `dispatch_real_smoke_active` | 当前是否处于 dispatch real smoke 场景。 | CSR driver 等组件用它选择 dispatch 场景下的默认驱动值。 |
| `dispatch_flushsb_waiting_empty` | directed flushSb 已发出且正在等待 `sbIsEmpty`。 | ctrl monitor 在等待期间持续采集 `sbIsEmpty` raw ctrl event。 |

## 3. raw event struct

| 类型 | 来源 monitor | 主要内容 | 后续消费者 |
|---|---|---|---|
| `dispatch_raw_int_wb_t` | `io_mem_to_ooo_int_wb_agent_agent_monitor.sv` | writeback port、ROB/LQ/SQ key、exception 等。 | `dispatch_monitor_event_adapter::convert_raw_int_wb()` 转为 LOAD/STA/STD writeback event。 |
| `dispatch_raw_iq_feedback_t` | `io_mem_to_ooo_iq_feedback_agent_agent_monitor.sv` | IQ feedback hit、ROB/LQ/SQ key、flush/PTW-back state。 | `convert_raw_iq_feedback()` 只转成 `iq_feedback_*` IssueQueue response；STA `hit=0` 额外转 replay；STD `hit=0` warning/drop。`hit=1` 是否兼容 pass 由 handler 根据 real writeback pass 配置决定。 |
| `dispatch_raw_ctrl_t` | `io_mem_to_ooo_ctrl_agent_agent_monitor.sv` | LQ/SQ deq 数量和指针、memory violation、`sbIsEmpty` 等。 | deq 交给 `lsq_commit_handler`；memory violation 当前归一成 redirect event；`sbIsEmpty` 解除 flushSb 等待。 |
| `dispatch_raw_csr_t` | `csr_ctrl_agent_agent_monitor.sv` | MMU CSR 当前 raw 值和权限状态。 | `push_raw_csr()` 覆盖 latest CSR snapshot，`drain_csr_events()` 按 seq 同步到 `mmu_csr_runtime_state`。 |
| `dispatch_raw_sfence_t` | `fence_agent_agent_monitor.sv` | `io_ooo_to_mem_sfence_valid/rs1/rs2/addr/id/hv/hg` 和采样 cycle。 | `dispatch_monitor_event_adapter::drain_sfence_events()` 调用 `common_data_transaction::apply_raw_sfence()`，删除命中的 live TLB entry。 |

## 4. raw queue 与 API

| 函数/task | 功能和设计原理 |
|---|---|
| `make_empty_raw_*()` | 构造对应 raw event 的安全空值，monitor 填字段前统一从空结构开始，避免旧字段残留。 |
| `raw_csr_payload_changed()` | 判断 CSR raw payload 是否真正变化，避免每拍重复推送相同 CSR snapshot。 |
| `push_raw_*()` | 除 CSR 外，在 `dispatch_monitor_capture_en && valid` 时把 raw event push 到对应 FIFO。CSR 是 runtime 状态，不走 FIFO，`push_raw_csr()` 只覆盖 `latest_raw_csr` 并递增 `latest_raw_csr_seq`。sfence/hfence 也走 FIFO，因为每条 fence 都是一次离散失效事件，不能像 CSR snapshot 那样只保留 latest。monitor 不直接改 status 表，保持采集和状态更新解耦。 |
| `pop_raw_*()` | FIFO 弹出 raw event，供 `dispatch_monitor_event_adapter` drain。 |
| `clear_raw_monitor_queues()` | 清空所有 raw queue。testcase reset、flush 收尾或 end check 前用于消除残留事件。 |
| `tick_dispatch_service_cycle()`、`get_dispatch_service_cycle()` | 维护/读取 service-cycle 计数，供 timeout 和 directed wait 使用。 |
| `raw_monitor_queue_size()` | 返回所有 raw queue 总残留数量。`end_test_check()` 用它判断 monitor 采集是否还有未消费事件。 |

## 5. 设计约束

- raw queue 只保存 monitor 原始事件，不直接更新 `common_data_transaction` 状态。
- 所有 raw event 必须经 `dispatch_monitor_event_adapter` 归一化后再进入 writeback/replay/redirect/commit/CSR 处理链路。
- sfence/hfence raw event 也必须经 adapter 调用公共数据 API；fence monitor 不能直接删除 `tlb_entry_by_key`。
- `io_redirect_*` 是 TB 驱动 DUT 的 input 接口，redirect monitor 不再 push raw event；Self Redirect Filter 已删除/停用。
- `dispatch_monitor_capture_en` 关闭后，monitor 不应继续 push 新事件；若仍有残留，end check 会 warning 并清空。
- L2TLB 相关字段只表示 DTLB/L2TLB 上游 responder 是否 active，不表示 L2TLB 到 cache/memory 下游访问模型。
