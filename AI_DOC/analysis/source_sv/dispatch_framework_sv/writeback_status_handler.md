# writeback_status_handler.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv`

## 1. 文件定位与使用场景

处理已经通过 batch 仲裁的非 redirect `memblock_wb_event_t`。它存在的原因是 int writeback、IQ feedback、fault 来源不同，但最终都要落到同一个 status 表。真实 int writeback 才能直接置 target writeback/pass/fault；IQ feedback 只表示 IssueQueue response；redirect 仲裁和 active redirect 过滤已迁移到 `dispatch_monitor_batch_handler`。

输入是 `dispatch_monitor_batch_handler` 放行后的 normalized event，或软件 smoke 构造且已经带 uid/ROB/issue_epoch/replay_seq 的 event。输出是 status 更新，或写入 `common_data_transaction::exception_event_q` 等待 recovery handler。

函数：

- `event_has_fault()`、`event_is_redirect()`、`event_is_replay()`、`event_is_real_writeback()`、`event_is_issue_feedback()`、`event_is_normal_pass()`：事件分类。
- `validate_event_target()`：非 redirect event 必须是 LOAD/STA/STD。
- `handle_real_writeback_event(wb_event)`：处理真实 int writeback。严格 STA 模式先要求
  `sta_issue_feedback_success=1`；未观察到 current IQ hit 时以 `WB_STATUS_STA_ORDER` fatal。
  顺序合法后，无异常时调用 `mark_target_normal_pass()`，有异常时先 `mark_target_fault()`
  再入 recovery 队列。
- `handle_issue_feedback_event(wb_event)`：处理 IQ feedback。STD target 或 `STD_FEEDBACK` source 在入口直接 fatal；STA `hit=1` 且 `MEMBLOCK_STA_REAL_WB_PASS_EN=1` 时只调用 `mark_issue_feedback_success()`，关闭该开关时才作为兼容 pass 调 `mark_target_normal_pass()`；STA `hit=0` 入 replay 队列。
- `handle_event(wb_event)`：兼容 synthetic event 的轻量入口；要求 event 已经 normalized，不再执行 monitor normalize、active redirect stale filter 或 redirect 仲裁。
- 已删除旧的 `process_event_queue()` 二次整理入口；`exception_event_q` 只由 `exception_redirect_replay_handler::process_pending_events()` 消费。

## 2. 字段与函数/task 设计原理

`writeback_status_handler` 负责把 monitor 或 software smoke 构造的反馈 event 转成状态表更新。它只处理“能立即决定”的真实 writeback pass/fault 或 IQ feedback success；redirect/replay/fault 事件会压入公共事件队列，交给 recovery handler 做统一处理。

| 函数/task | 参数 | 功能和设计原理 |
|---|---|---|
| `ensure_data()` | 无 | 绑定公共 owner。 |
| `event_has_fault(wb_event)` | event | 判断 event 是否带 exception。 |
| `event_is_redirect(wb_event)` | event | 以 canonical `redirect.valid` 判定 redirect，优先级高于 normal pass；`redirect_valid` 作为兼容/显式标志必须与 `redirect.valid` 一致，不一致 fatal；`source` 只作为来源标签，不能单独触发 redirect。 |
| `event_is_replay(wb_event)` | event | 只看 `replay_valid`，不再由 `source == BACKEND_REPLAY` 单独触发；后端 replay source 只用于 debug/追踪。 |
| `event_is_real_writeback(wb_event)` | event | 判断 event 是否来自真实 writeback 语义，只看 `real_wb_valid`。 |
| `event_is_issue_feedback(wb_event)` | event | 判断 event 是否是 IQ feedback。该分支不能直接等同真实 writeback。 |
| `event_is_normal_pass(wb_event)` | event | 只有 real writeback valid 且无 redirect/replay/fault 时才是 normal pass。 |
| `target_real_wb_pass_enabled(target)` | target | 只查询 STA 是否要求等待真实 writeback pass；STD 不再查询 runtime 开关。 |
| `validate_event_target(wb_event,caller)` | event、调用者 | normal event 必须指向 LOAD/STA/STD target，防止 monitor adapter 生成非法 target。 |
| `handle_real_writeback_event(wb_event)` | event | 真实 writeback 分支。严格 STA 模式先检查 current issue 已有 IQ hit；通过后 pass 更新 target writeback/pass，fault 更新 target fault并入 recovery 队列。 |
| `handle_issue_feedback_event(wb_event)` | event | IQ feedback 分支。STD 入口 fatal；STA hit 只表示 IssueQueue finalSuccess，并根据 STA real-WB 开关记录 feedback success 或走兼容 pass；STA failed 进入 replay。 |
| `handle_event(wb_event)` | event | 只接受已 normalized 的非 redirect event；redirect 会 warning/drop，真实 monitor redirect 必须从 batch handler 进入 recovery queue。 |

## 3. STA IQ 与 real-WB 顺序

当`MEMBLOCK_STA_REAL_WB_PASS_EN=1`时，STA合法完成顺序固定为：

```text
IQ hit -> mark_issue_feedback_success -> STA real-WB/fault-WB -> pass/fault owner
IQ miss -> replay recovery，不允许同一动态实例继续 real-WB
```

`handle_real_writeback_event()`在修改任何writeback/pass/fault状态前读取current status。若
target为STA且`sta_issue_feedback_success=0`，立即fatal。这个检查不会影响LOAD/STD，也不会
新增pass/fail/terminal owner；它只阻止STA real-WB绕过IQ replay判定。同一采样拍的IQ和
int-WB由adapter先按IQ顺序放入batch，因此合法同拍hit/WB不会被误报。
