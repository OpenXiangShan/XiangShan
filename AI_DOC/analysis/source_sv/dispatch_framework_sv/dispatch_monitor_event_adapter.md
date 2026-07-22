# dispatch_monitor_event_adapter.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`

## 1. 文件定位与使用场景

`dispatch_monitor_event_adapter.sv` 是 raw monitor 事件到 dispatch 公共状态机的适配层。它本身不是 monitor，也不是 driver；它不直接采 DUT pin，也不直接驱动 DUT。它的职责是从 `memblock_sync_pkg` 中各 monitor 原始队列取出 raw fact，把接口级原始事实翻译成 dispatch 框架统一理解的 writeback/replay/redirect/deq/CSR 语义。

raw monitor 指各个 agent monitor 从 DUT/interface 上直接采集到的“原始信号事实”。这些 raw event 只说明端口上发生了什么，还没有解释成哪个 `uid`、哪个 LOAD/STA/STD target、是 pass/fault/replay/redirect，或是否 stale。因此 raw monitor 不直接改 `status_transaction`，而是先进入 `memblock_sync_pkg` raw queue，再由 adapter 统一解释。

输入是 `raw_int_wb_q/raw_iq_feedback_q/raw_ctrl_q/raw_sfence_q` 和 latest CSR snapshot。输出给三类地方：CSR latest snapshot 更新 `mmu_csr_runtime_state`，sfence/hfence raw event 触发 live TLB entry 失效，writeback/IQ/memoryViolation redirect event 只收集到 batch 队列后交给 `dispatch_monitor_batch_handler`，ctrl deq event 交给 commit handler。

核心链路：

```text
agent monitor 采集 DUT/interface raw 信号
        ↓
memblock_sync_pkg raw queue
        ↓
dispatch_monitor_event_adapter 转换 raw fact
        ↓
dispatch_monitor_batch_handler normalize 和 redirect-first 仲裁
        ↓
writeback/replay/redirect/commit/CSR 状态机
```

raw event 到统一语义的映射：

| raw 来源 | 原始含义 | adapter 输出 |
|---|---|---|
| `raw_int_wb` | V2 split int writeback 采到的 `source_kind`、类别内 lane、真实 ROB/exception/metadata 和 sample epoch；STD 只有 ROB value。 | `SCALAR_LDA` -> `LOAD_WB + LOAD`；`STA` -> `STORE_WB + STA`；`STD` -> `STORE_WB + STD`。adapter 先补 current snapshot 并做 capability/key 校验，成功后才设置 `real_wb_valid`。 |
| `raw_iq_feedback` | IQ feedback 端口采到的 STA/STD hit、flush/PTW-back 信息。 | STA -> `STA_FEEDBACK`，adapter 只设置 `iq_feedback_*`，不设置真实 writeback/pass；STA `hit=0` 额外置 `replay_valid`。当前严格 V2 路径不支持 STD IQ feedback，观察到该 event 即 fatal。`flush_state` 只保留为 PTW-back/状态元信息，不单独触发 replay。 |
| `raw_ctrl` | ctrl monitor 采到的 LQ/SQ deq、memory violation、`sbIsEmpty`。 | `lqDeq/sqDeq` 直接交给 `lsq_commit_handler`；memory violation 转 `MEMORY_VIOLATION`，按 redirect 类处理；`sbIsEmpty` 更新 directed flushSb 等待状态。 |
| `raw_csr` | CSR monitor 采到的实时 MMU CSR 状态。 | 更新 `mmu_csr_runtime_state`，影响后续 TLB lookup 的 ASID/VMID/权限上下文。 |
| `raw_sfence` | fence monitor 采到的 `io_ooo_to_mem_sfence_*`。 | 在 CSR runtime 同步后调用 `common_data_transaction::apply_raw_sfence()`，按 sfence/hfence 语义删除命中的 live `tlb_entry_by_key`。 |

为什么需要 adapter：

- monitor 的职责是采事实，不负责解释 transaction 生命周期。
- raw event 往往只带 ROB/LQ/SQ key，不一定带 TB 内部 `uid`。
- 状态机需要统一的 `memblock_wb_event_t`，并通过 active map 反查 uid、检查 target issue epoch 和 target-aware replay seq。
- deq、CSR、writeback、redirect 的后续消费者不同，adapter 可以把分发逻辑集中在一个地方，避免多个 monitor 各自改状态表。

函数/task：

- `fill_current_issue_snapshot()`、`attach_current_issue_snapshot()`：使用 active ROB map 和当前 status 校验 active/target/epoch/owner，再补 UID、ROB、LQ/SQ、`issue_epoch/replay_seq`；不修改 pass/fault 状态。
- `probe_std_candidate()`、`resolve_std_uid_by_rob_value_only()`：对 STD 缺失 ROB flag 的情况固定探测 flag 0/1，先过滤合法 STD current candidate，再要求唯一命中并从 status 补 SQ；零命中、双命中或 owner 不一致均 fatal。
- `check_raw_int_wb_capability()`、`check_raw_int_wb_metadata()`：按 V2 source/lane 检查 metadata presence、exceptionVec 位图及当前 unsupported trigger/flush/replay 组合。
- `convert_raw_int_wb(raw,wb_event)`：按 `SCALAR_LDA/STA/STD` source kind 选择 target，完成 current snapshot、metadata 和 key 归一化后设置 `real_wb_valid`。
- `convert_raw_iq_feedback(raw,wb_event)`：当前只接受 STA IQ feedback。香山源码中 IQ feedback 是 IssueQueue response，`hit=1` 表示 finalSuccess，`hit=0` 表示 failed；adapter 只设置 `iq_feedback_valid/iq_feedback_hit/iq_feedback_failed/iq_feedback_flush_state`。STA `hit=0` 同时设置 `replay_valid` 和可选 `ptw_back_replay`；STD IQ feedback 不是当前 V2 completion source，入口直接 fatal。
- `convert_raw_memory_violation(raw,wb_event)`：memory violation 当前转 redirect event，保留 `memoryViolation.bits.level`，并按当前 RTL/Scala 语义用 `level(0)` 派生 `flush_itself`。
- `apply_raw_ctrl_deq(raw)`：先更新 `sbIsEmpty`，再调用 `lsq_commit_handler::apply_raw_ctrl_deq()`。
- `drain_csr_events()`：只更新 runtime CSR，不消费 sfence/hfence 离散事件。L2TLB responder 查表前、writeback/ctrl drain 前如果只需要最新 CSR，应调用这个 CSR-only 入口。
- `drain_sfence_events()`：排空 raw sfence queue，把 fence monitor 采到的 `rs1/rs2/addr/id/hv/hg` 交给公共数据层执行 entry 级 TLB 失效。adapter 不直接操作 `tlb_entry_by_key`，避免 monitor 侧绕过公共 owner。统一 service loop 由 `memblock_dispatch_base_sequence::collect_runtime_context_events()` 显式保证先 `drain_csr_events()`、再 `drain_sfence_events()`。
- `collect_writeback_events_batch(events)`：处理 raw int wb 和 IQ feedback，只把转换成功的 semantic event push 到 batch，不调用 writeback handler。
- `collect_ctrl_redirect_events_batch(events)`：处理 raw ctrl deq、`sbIsEmpty` 和 memory violation；deq/sbIsEmpty 仍即时更新，memoryViolation 只 push redirect event 到 batch。

## 2. 字段与函数/task 设计原理

`dispatch_monitor_event_adapter` 是 DUT monitor/raw sync event 到公共 event/status API 的适配层。它的价值是把接口字段解析集中起来，后续 handler 只处理统一的 `memblock_wb_event_t`。

| 函数/task | 参数 | 功能和设计原理 |
|---|---|---|
| `bind_commit_handler(handler)`、`ensure_handles()` | commit handler | 让 ctrl deq event 可以直接进入 LSQ commit handler。 |
| `make_wb_event_base()` | 无 | 生成带默认 cycle 的空 event，所有转换函数从同一默认值开始。 |
| `raw_rob_to_key(valid,flag,value,key)`、`raw_lq_to_key(...)`、`raw_sq_to_key(...)` | raw valid/flag/value、输出 key | 将 monitor 原始字段转成统一 key，invalid 时返回 0。 |
| `event_has_active_uid(wb_event)` | event | 快速判断 event 是否能反查到 active uid，避免无效 monitor event 进入状态机。 |
| `fill_current_issue_snapshot()`、`attach_current_issue_snapshot()` | partial event、raw sample epoch | 通过 current active map/status 补齐 UID、资源 key 和 target issue/replay 快照；旧实例、缺 key 或 owner 不一致不允许进入 handler。 |
| `probe_std_candidate()`、`resolve_std_uid_by_rob_value_only()` | STD ROB value-only raw | 最多探测两个 ROB flag，唯一命中后补完整 ROB/SQ identity，不扫描主表。 |
| `check_raw_int_wb_capability()`、`check_raw_int_wb_metadata()` | raw int writeback | 按 V2 split lane 检查真实字段能力、exception mask 和 unsupported metadata 行为。 |
| `convert_raw_int_wb(raw,wb_event)` | raw int writeback | 将 V2 `SCALAR_LDA/STA/STD` 转换成 LOAD/STA/STD target event，完成 snapshot/key 校验后设置 `real_wb_valid`。 |
| `convert_raw_iq_feedback(raw,wb_event)` | raw IQ feedback | 将受支持的 STA IssueQueue response 转成 `iq_feedback_*` event；不再把 `hit` 写成 `real_wb_valid`，STD feedback 直接 fatal。 |
| `convert_raw_memory_violation(raw,wb_event)` | raw ctrl event | 将 memory violation 归一成 redirect 类 event，把 `memoryViolation.bits.level` 放进 payload，同时用该 level 派生 `flush_itself`，供后续 `io.redirect` 回灌和软件 flush 使用。 |
| `apply_raw_ctrl_deq(raw)` | raw ctrl event | 把 `sbIsEmpty` 写回公共数据，再把 LQ/SQ deq 数量和指针交给 `lsq_commit_handler`。 |
| `drain_csr_events()` | 无 | 从 raw sync 包中读取 latest CSR snapshot 和 seq，并交给 `common_data_transaction::apply_raw_csr_runtime(raw, seq)`；是否重复 apply 由公共数据侧统一判断，避免多个 adapter 实例重复应用同一个 changed pulse。该函数不再调用 `drain_sfence_events()`。 |
| `drain_sfence_events()` | 无 | 从 `memblock_sync_pkg::raw_sfence_q` 弹出 raw fence event，调用 `common_data_transaction::apply_raw_sfence()`。这个函数只负责桥接 raw queue 和公共数据 API，不在 adapter 内手写失效规则。调用方必须显式决定是否以及何时消费 sfence。 |
| `collect_writeback_events_batch(events)` | event queue | 把 raw writeback/IQ feedback 转换成 `memblock_wb_event_t` 并追加到本轮 batch，不直接更新状态。 |
| `collect_ctrl_redirect_events_batch(events)` | event queue | 排空 ctrl raw event，deq/sbIsEmpty 即时更新，`memoryViolation` 转 redirect event 后追加到本轮 batch。 |
