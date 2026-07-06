# Soft Test 与真实 DUT Flow 函数调用关系说明

本文用于记录 memblock dispatch 测试框架中一类容易混淆的函数关系：

- soft test sequence 内部会保留若干 software-only helper，用来人工推进 admission、issue fire、writeback event。
- 真实 DUT 接口驱动 sequence 通常不继承 `memblock_dispatch_base_sequence`，而是继承对应 agent 的 default sequence。
- 因此真实 DUT flow 不调用 soft-only helper，而是直接调用同一个核心 helper object，例如 `issue_queue_scheduler`、`issue_field_assigner`、`dispatch_monitor_event_adapter`、`writeback_status_handler` 或 `lsq_commit_handler`。

判断某个函数是否属于真实 DUT flow 时，需要写清楚 class scope。只看函数名容易误判，因为不同 class 中可能存在同名函数。

## 1. 为什么真实 DUT Agent Sequence 不直接调用 Soft Helper

真实 DUT 接口驱动 sequence 需要挂在对应 agent sequencer 上，例如：

```systemverilog
u_lsqenq_agent_agent.sqr.main_phase  -> memblock_lsqenq_dispatch_sequence
u_lintsissue_agent_agent.sqr.main_phase -> memblock_lintsissue_dispatch_sequence
u_lsqcommit_agent_agent.sqr.main_phase -> memblock_lsqcommit_dispatch_sequence
```

这些 sequence 继承的是各自 agent 的 default sequence：

```systemverilog
class memblock_lsqenq_dispatch_sequence extends lsqenq_agent_agent_default_sequence;
class memblock_lintsissue_dispatch_sequence extends lintsissue_agent_agent_default_sequence;
class memblock_lsqcommit_dispatch_sequence extends lsqcommit_agent_agent_default_sequence;
```

它们不是 `soft_test_memblock_dispatch_smoke_sequence` 的子类，也不应依赖 software-only sequence 的成员 task/function。真实 flow 通过组合 helper object 的方式复用逻辑。

例如：

```systemverilog
issue_queue_scheduler issue_sched;
issue_field_assigner  field_assigner;
```

这种结构的含义是：

- soft-only helper 主要服务于 software smoke/replay sequence。
- 真实 agent sequence 直接调用 helper object 中的核心函数。
- 两边共享的是 helper object 中的真实状态更新逻辑，而不是共享 soft helper 本身。

## 2. 已下沉到 Soft Sequence 的 Helper 与真实 DUT Helper 对照

### 2.1 `prepare_issue_route_for_uid`

soft helper：

```systemverilog
soft_test_memblock_dispatch_smoke_sequence::prepare_issue_route_for_uid(uid)
```

soft test 调用位置：

```systemverilog
soft_test_memblock_dispatch_smoke_sequence::admit_lsq_and_route_issue()
    -> prepare_issue_route_for_uid(uid)
```

真实 DUT flow 不直接调用该 soft helper。真实 LSQ admission 成功后的调用链是：

```systemverilog
memblock_lsqenq_dispatch_sequence::confirm_lsq_candidates()
    -> complete_admission(uid)
        -> issue_sched.prepare_issue_route_for_uid(uid)
```

真实执行的核心函数是：

```systemverilog
issue_queue_scheduler::prepare_issue_route_for_uid(uid)
```

该核心函数负责：

- 检查 uid 对应状态必须已经 `active && enq`。
- 设置 `MEMBLOCK_STATUS_ISSUE_READY = 1`。
- 调用 `route_uid(uid)` 将 uid 放入 load/STA/STD issue queue。

注意：这个函数不表示 TLB 建表。`tlb_mapped` 必须等 L2TLB request 到来，建表/查表成功并回填 uid TLB record 后才能置位。

### 2.2 `select_issue_candidates`

soft helper：

```systemverilog
soft_test_memblock_dispatch_smoke_sequence::select_issue_candidates(load_items, sta_items, std_items)
```

soft test 调用位置：

```systemverilog
soft_test_memblock_dispatch_smoke_sequence::fire_all_issue_items()
soft_test_memblock_dispatch_replay_smoke_sequence::fire_replay_sta_item()
```

真实 DUT issue driver 不调用该 soft helper，而是在真实 issue cycle 中直接调用：

```systemverilog
memblock_lintsissue_dispatch_sequence::send_issue_cycle()
    -> issue_sched.select_issue_candidates(load_items, sta_items, std_items)
```

该 helper 根据当前 issue queue、`send_pri`、pipe 数量、flush/redirect 状态等条件选择本拍可以发射的 load/STA/STD item。

### 2.3 `mark_issue_item_fire`

soft helper：

```systemverilog
soft_test_memblock_dispatch_smoke_sequence::mark_issue_item_fire(item, fired)
```

soft test 调用位置：

```systemverilog
soft_test_memblock_dispatch_smoke_sequence::fire_selected_items()
soft_test_memblock_dispatch_replay_smoke_sequence::fire_replay_sta_item()
```

soft test 使用它模拟“该 issue item 已经发射成功”。

真实 DUT issue driver 不调用该 soft helper，而是在真实 lintsissue xaction drive 完成后，根据 fired mask 调用：

```systemverilog
memblock_lintsissue_dispatch_sequence::mark_fired_items()
    -> issue_sched.mark_issue_fire(item)
    -> issue_sched.mark_issue_fire_already_accepted(item)  // redirect/flush 边界场景
```

真实 flow 中 fired 状态来自真实 agent driver 与 DUT ready/fire 结果；soft test 中 fired 状态由测试序列人工推进。

### 2.4 `submit_writeback_event`

soft helper：

```systemverilog
soft_test_memblock_dispatch_smoke_sequence::submit_writeback_event(wb_event)
```

soft test 调用位置：

```systemverilog
soft_test_memblock_dispatch_smoke_sequence::inject_writeback_events()
soft_test_memblock_dispatch_replay_smoke_sequence::body()
```

soft test 使用它人工构造 pass/replay writeback event，直接送入 `writeback_status_handler`。

真实 DUT flow 不通过该函数注入 writeback。真实 flow 的来源是 DUT output monitor：

```systemverilog
DUT memblock output
    -> raw monitor queue
        -> dispatch_monitor_event_adapter
            -> writeback_status_handler
```

因此 `submit_writeback_event()` 是 software synthetic event 入口，不能理解为真实 DUT writeback 采集路径。

## 3. Soft Test 高层任务与真实 DUT Flow 的对应关系

以下函数不是 base wrapper，但也属于 soft test 人工推进状态，而真实 DUT flow 有对应 agent sequence 或 monitor 路径。

| Soft test 任务 | Soft 中的作用 | 真实 DUT flow 对应路径 |
|---|---|---|
| `soft_test_memblock_dispatch_smoke_sequence::admit_lsq_and_route_issue()` | 直接调用 `lsq_ctrl.commit_allocate()` / `commit_non_lsq_admission()`，模拟 LSQ admission 成功，然后 route issue queue。 | `memblock_lsqenq_dispatch_sequence` 驱动真实 LSQ enqueue 接口；DUT accept 后执行 `complete_admission()`。 |
| `soft_test_memblock_dispatch_smoke_sequence::fire_all_issue_items()` / `fire_selected_items()` | 从 issue queue 中取 item，人工调用 `mark_issue_item_fire()`，模拟 issue fire。 | `memblock_lintsissue_dispatch_sequence` 驱动真实 load/STA/STD issue 接口，并根据 fired mask 更新状态。 |
| `soft_test_memblock_dispatch_smoke_sequence::inject_writeback_events()` | 人工构造 pass writeback event。 | DUT output monitor 采集真实 writeback/feedback，再由 monitor adapter 归一化。 |
| `soft_test_memblock_dispatch_replay_smoke_sequence::make_replay_wb_event()` 相关路径 | 人工构造 replay event，验证 replay 状态机。 | DUT feedback/ctrl output monitor 采集 replay/redirect 相关事件。 |
| `soft_test_memblock_dispatch_smoke_sequence::commit_and_deq_lsq()` | 直接调用 commit handler 更新 ROB commit、LQ/SQ deq 状态。 | `memblock_lsqcommit_dispatch_sequence` 驱动真实 LSQ commit 接口；deq/释放状态由 DUT monitor 或 commit handler 校验路径更新。 |

这些 soft test 任务的目的不是严格模拟真实接口时序，而是用软件事件快速验证公共状态表、issue queue、writeback/replay/commit handler 的闭环逻辑。

## 4. 容易误判但不属于 Soft-only 的函数

以下函数虽然 soft test 可能会调用，但真实 DUT flow orchestration 或真实 service loop 也会调用，因此不能归类为“只给软件测试用”。

### 4.1 `route_all_issue_queues`

```systemverilog
memblock_dispatch_base_sequence::route_all_issue_queues()
```

soft test 会调用它，但 `memblock_dispatch_real_smoke_sequence::service_real_dispatch_flow()` 也会调用它。真实 issue driver 内部也会直接调用：

```systemverilog
issue_sched.route_all_ready_uids()
```

所以它不是纯 soft-only。更准确说，它是 base sequence 的公共 wrapper；真实 agent sequence 通常直接调用 scheduler helper。

### 4.2 Monitor / feedback service 相关 wrapper

这些函数会被 `memblock_dispatch_real_smoke_sequence::service_monitor_once()` 调用：

```systemverilog
collect_runtime_context_events()
collect_monitor_event_batch()
exception_redirect_replay_task()
```

因此它们属于真实 DUT flow service loop 的一部分，不应归类为 soft-only。

## 5. 当前疑似保留或低使用率 Wrapper

以下 wrapper 当前不属于“soft-only 直接调用”，而是更接近保留封装或低使用率接口。真实 agent sequence 通常直接调用 helper object。

| Base wrapper | 当前情况 | 真实 flow 常用核心 helper |
|---|---|---|
| `route_issue_queue_for_uid()` | 已删除。真实 flow 不再保留单 uid base wrapper。 | `issue_queue_scheduler::route_uid()` / `route_all_ready_uids()` |
| `assign_main_issue_fields()` | base wrapper 已删除。 | `issue_field_assigner::assign_main_issue_fields()` |
| `assign_issue_dep_fields()` | base wrapper 已删除。 | `issue_field_assigner::assign_issue_dep_fields()` |
| `assign_backend_meta_fields()` | base wrapper 已删除。 | `issue_field_assigner::assign_backend_meta_fields()` |
| `assign_issue_item_fields()` | base wrapper 已删除；真实 issue driver 直接调用 helper。 | `issue_field_assigner::assign_issue_item_fields()` |

后续如果要简化框架，优先删除这种“base sequence 只转发到真实 helper、且没有真实 flow 调用”的 wrapper；文档需要直接指向真实 helper，避免把旧 wrapper 当成实际 agent driver 必经入口。

## 6. 命名与文档建议

为了避免继续混淆，后续文档和代码注释中建议使用完整 class scope：

```text
soft_test_memblock_dispatch_smoke_sequence::prepare_issue_route_for_uid()
issue_queue_scheduler::prepare_issue_route_for_uid()
```

不要只写 `prepare_issue_route_for_uid()`，否则无法判断说的是 soft helper 还是 scheduler 核心函数。

对于 soft synthetic event，也建议明确写：

```text
software synthetic writeback event
```

不要把 `submit_writeback_event()` 描述成真实 DUT writeback 采集路径。真实 DUT writeback 采集路径应描述为：

```text
DUT output monitor -> dispatch_monitor_event_adapter -> writeback_status_handler
```
