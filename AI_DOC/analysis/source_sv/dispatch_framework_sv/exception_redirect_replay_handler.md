# exception_redirect_replay_handler.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/exception_redirect_replay_handler.sv`

## 1. 文件定位与使用场景

处理 `exception_event_q` 中的 replay/redirect/fault。普通 pass 可以直接改状态，但 replay/redirect 会影响 issue queue、active uid 和 flush 范围，因此单独集中处理。

输入是 `exception_event_q`。输出是 `mark_replay_pending()`、redirect drive queue 投递和 `apply_redirect_flush()` 对公共状态的修改；fault target 状态由 writeback handler 首次采到 fault 时调用 `mark_target_fault()` 落表。当前实现是后端接口级阶段化恢复模型：会真实驱动 DUT redirect 接口，但仍不建模完整前端 refetch 闭环。

函数/task：

- `redirect_from_event()`：从 wb_event 构造 redirect payload。
- `redirect_event_is_older()`、`select_oldest_redirect()`：多个 redirect 选最老。
- `handle_replay_event()`：解析 uid 后调用 `mark_replay_pending()`；LOAD/STA replay 会重新入队对应 target，STD replay request 由公共数据层 warning 后忽略。开启 `MEMBLOCK_REPLAY_WAIT_PTW_EN` 且 event 是 PTW-back replay 时，先进入 `ptw_wait_replay_q`。
- `handle_fault_event()`：消费 fault recovery event，解析 uid/epoch/replay_seq 用于过滤失效事件和调试，不再重复调用 `mark_target_fault()`。
- `service_ptw_wait_replay()`：周期性释放 L2TLB response done 或 timeout 的 PTW-back replay 等待项，timeout 使用 dispatch service-cycle。
- `advance_active_redirect()`：等待 redirect sequence 完成真实 drive，完成后 apply flush；等待超时同样使用 dispatch service-cycle。
- `requeue_events_not_flushed_by_redirect()`：redirect 等待期间把同批未被 redirect 覆盖的非 redirect 事件放回队列；被 `rob_need_flush()` 覆盖的普通反馈作为旧动态实例事件丢弃。
- `process_pending_events()`：先推进已有 redirect，再从事件队列中选最老 redirect；若有 redirect，先 request freeze 并投递给 redirect sequence，等待 drive done 后再 apply flush；没有 redirect 时才处理 replay/fault。

当前 redirect 不是同一 task 内同步 request/apply。发现 redirect 后会冻结 admission/issue/commit，投递 `pending_redirect_drive_q`，等待 `memblock_redirect_dispatch_sequence` 驱动 `io.redirect` 后再 flush 软件状态。这样避免 TB 内部已经 flush、DUT 却没有收到 redirect 的状态分叉。

若检测到 redirect event 但 `MEMBLOCK_REDIRECT_SEQ_EN=0`，handler 直接 fatal。该配置只适合没有 redirect/memoryViolation 事件的 flow；一旦需要 recovery，必须启用 redirect sequence，否则 freeze 后的 pending drive 无法被消费。

## 2. 字段与函数/task 设计原理

`exception_redirect_replay_handler` 处理公共事件队列中的 replay、fault、redirect。当前实现是最简化合法模型，不模拟完整后端重取和复杂 flush 时序，但保证状态不冲突。

| 函数/task | 参数 | 功能和设计原理 |
|---|---|---|
| `event_is_redirect/replay/fault(wb_event)` | event | 与 writeback handler 保持同样分类语义；redirect 以 canonical `redirect.valid` 判定，`redirect_valid` 必须与其一致，不一致 fatal，`source` 不能单独触发 redirect。 |
| `redirect_from_event(wb_event)` | event | 提取 canonical `redirect.valid=1` 的 redirect payload；payload 无效时 fatal，不靠 ROB key 单独合成 redirect。 |
| `redirect_event_is_older(candidate,best)` | 两个 event | 多个 redirect 同拍出现时选择 ROB 年龄最老的，降低错误 flush 范围风险。 |
| `select_oldest_redirect(events,selected)` | event 队列、输出 event | 从一批 pending event 中找最老 redirect。 |
| `handle_replay_event(wb_event)` | event | 通过 event 反查 uid 和发射快照，调用 `mark_replay_pending()`。LOAD/STA replay 让对应 target 重新进入发射路径；STD 不作为真实 DUT backend replay 源建模，收到后不建立 `replay_pending`。 |
| `handle_fault_event(wb_event)` | event | 消费 fault recovery event；target fault 和 exception vec 已由 writeback handler 落表，本函数不再重复标记。 |
| `service_ptw_wait_replay()` | 无 | 开启 `MEMBLOCK_REPLAY_WAIT_PTW_EN` 后释放已由 L2TLB responder 回过 response 或 timeout 的 PTW-back replay 等待项。默认关闭时不会改变 replay 行为，等待周期按 dispatch service-cycle 计数。 |
| `advance_active_redirect()` | 无 | active redirect 存在时，只等待真实 redirect drive 完成或 timeout；drive done 后调用 `apply_redirect_flush()`。等待期间不会继续弹出新事件，避免非 redirect event 丢失。 |
| `requeue_events_not_flushed_by_redirect(events,redirect)` | event 队列、redirect payload | 当同批事件中出现 redirect 时，只保留未被该 redirect 覆盖的非 redirect 事件。被覆盖的 writeback/pass 属于旧动态实例，不能继续更新状态。 |
| `process_pending_events()` | 无 | 先推进 active redirect；若没有 active redirect，再从事件队列选择最老 redirect。redirect 优先检查 `MEMBLOCK_REDIRECT_SEQ_EN`，关闭时 fatal；开启时冻结并投递 drive queue。只有没有 pending redirect 时才处理 replay/fault。 |
