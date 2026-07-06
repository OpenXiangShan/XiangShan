# dispatch_monitor_batch_handler.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_batch_handler.sv`

## 1. 文件定位

`dispatch_monitor_batch_handler` 是 monitor semantic event 的 batch 级仲裁层。adapter 只负责把 raw int writeback、raw IQ feedback、memoryViolation 转成 `memblock_wb_event_t` 并收集到同一个 batch；batch handler 再统一 normalize、过滤 active redirect 覆盖的 stale event，并在同批存在 redirect 时先选择 oldest redirect。

职责边界：

- adapter：raw fact -> semantic event，不改 pass/fault/replay 状态。
- batch handler：normalize、active redirect stale filter、同批 redirect-first 仲裁。
- writeback handler：只处理 batch 已放行的非 redirect event。
- recovery handler：只处理 recovery queue 中 redirect/replay/fault 的跨 batch/drive 后续流程。

## 2. 核心流程

```text
process_monitor_event_batch(events)
  -> normalize_event_batch()
  -> active_redirect 有效时过滤覆盖 event
  -> 本批有 redirect 时选择 oldest redirect
  -> selected redirect 入 exception_event_q
  -> 被 selected redirect 覆盖的 pass/fault/replay/drop
  -> 未覆盖的非 redirect event 同轮交给 writeback_status_handler
  -> 未覆盖的其它 redirect 保留进 recovery queue，等当前 redirect 完成后再处理
```

无法解析 active uid/ROB key 的 event 会在 normalize 阶段 `uvm_warning` 后丢弃。这样无法定位归属的 pass/fault/replay 不会绕过 redirect 仲裁落状态。

## 3. 函数说明

| 函数/task | 作用 |
|---|---|
| `bind_writeback_handler(handler)` | 绑定状态更新 handler，保证 batch 放行的非 redirect event 进入同一个 writeback 状态更新器。 |
| `normalize_event_batch(events, normalized_events)` | 调用 `common_data_transaction::normalize_feedback_event()` 补齐 uid/ROB/issue_epoch/replay_seq；失败 warning/drop。 |
| `select_oldest_redirect(events, selected)` | 在同批 normalized event 中用 `rob_order_util` 选择 oldest redirect。 |
| `event_covered_by_redirect(event, redirect)` | 统一用 `rob_order_util::rob_need_flush()` 判断 event 是否被 redirect flush 覆盖。 |
| `process_allowed_non_redirect_event(event)` | 对 batch 已放行的真实 writeback/IQ feedback 调用 writeback handler；其它 replay/fault 语义入 recovery queue。 |
| `process_monitor_event_batch(events)` | batch 总入口，执行 redirect-first 仲裁和状态更新分发。 |

## 4. 删除/收窄点

- `writeback_status_handler` 不再负责 monitor event 的 normalize、active redirect stale filter 或 redirect 入口。
- `dispatch_monitor_event_adapter` 不再边 drain 边调用 `writeback_handler.handle_event()`。
- 旧 raw writeback 兼容字段已删除，真实 writeback 只看 `real_wb_valid`，IQ feedback 只看 `iq_feedback_*`。
