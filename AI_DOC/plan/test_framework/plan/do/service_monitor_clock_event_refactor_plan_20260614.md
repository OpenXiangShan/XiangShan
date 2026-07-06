# service_monitor_once 时钟与事件驱动重构方案

## 1. 问题背景

当前 `memblock_dispatch_real_smoke_sequence::service_real_dispatch_flow()` 使用如下轮询方式推进测试框架：

```systemverilog
for (int unsigned step = 0; step < max_service_steps; step++) begin
    service_monitor_once();
    route_all_issue_queues();
    if (all_transactions_success()) begin
        service_monitor_once();
        return;
    end
    #1;
end
```

该方式存在两个问题：

- `#1` 是仿真时间轮询，不是 DUT clock 周期语义。当前 `top_tb.sv` 为 `timescale 1ns/1ps`，`CLK_GEN(clk,200)` 约为 200MHz，即 5ns 一个 clk，`#1` 会在一个 clk 内轮询多次。
- `service_monitor_once()` 和 monitor 的采样边界没有严格对齐。各 monitor 是通过 `vif.mon_mp.mon_cb` 在 clocking block 上采样，再 push raw event 到 `memblock_sync_pkg`；`service_monitor_once()` 不应通过时间轮询猜测何时有事件。

本方案目标是去掉 `#1` 轮询，将真实 smoke 主服务循环改成 DUT clock 下降沿驱动。
本次落地版本先不把 raw queue/CSR pending 纳入 `all_transactions_success()`，也不新增
raw pending helper；退出条件仍只使用现有 `all_transactions_success()`。

## 2. 当前事件来源确认

当前触发源不是 `service_monitor_once()` 直接通过 vif 采样，而是各 agent monitor 通过 vif 采样后写入 `memblock_sync_pkg`：

```text
DUT/interface signal
  -> agent monitor @vif.mon_mp.mon_cb 采样
  -> monitor 构造 raw_xxx
  -> memblock_sync_pkg::push_raw_xxx(raw_xxx)
  -> service_monitor_once()/adapter pop raw queue
  -> common_data_transaction/writeback_status_handler 更新状态
```

当前 raw 数据缓存如下：

| 类型 | monitor | memblock_sync_pkg 存储 | 消费入口 |
|---|---|---|---|
| CSR runtime | `csr_ctrl_agent_agent_monitor` | `latest_raw_csr` snapshot | `collect_csr_runtime_events()` |
| sfence/hfence | `fence_agent_agent_monitor` | `raw_sfence_q[$]` | `drain_sfence_events()` |
| int writeback | `io_mem_to_ooo_int_wb_agent_agent_monitor` | `raw_int_wb_q[$]` | `collect_writeback_events()` |
| IQ feedback | `io_mem_to_ooo_iq_feedback_agent_agent_monitor` | `raw_iq_feedback_q[$]` | `collect_writeback_events()` |
| ctrl/memoryViolation/deq | `io_mem_to_ooo_ctrl_agent_agent_monitor` | `raw_ctrl_q[$]` | `collect_exception_and_redirect_events()` |

因此本次重构不改变 monitor 采样职责：monitor 仍然只负责采样并 push raw 数据；主 sequence 只负责按事件/时钟调度消费。

## 3. 目标调度模型

目标模型分成两条独立服务线：

```text
每个 DUT clk 下降沿:
  -> 若 all_transactions_success() 已经成立，则 break
  -> service_monitor_once()
  -> route_all_issue_queues()
  -> 若 all_transactions_success() 成立，则 break
```

关键要求：

- 不再使用 `#1` 轮询。
- `service_monitor_once()` 和 `route_all_issue_queues()` 都在同一个下降沿服务循环中执行。
- 同一下降沿内先 `service_monitor_once()`，再 `route_all_issue_queues()`。
- `route_all_issue_queues()` 固定在时钟下降沿执行，避免和 monitor 正沿采样、driver 正沿驱动混在同一调度点。
- 取消 `max_service_steps` 控制及 `MEMBLOCK_REAL_SMOKE_MAX_SERVICE_STEPS` 参数。
- 不在本次修改中新增 raw queue/CSR pending 完成判据；该项后续如需要再单独处理。

## 4. memblock_sync_pkg 增强方案（后续可选）

本次落地版本不修改 `memblock_sync_pkg.sv`，不新增 raw event 和 pending helper。
如果后续需要让 `service_monitor_once()` 只在 raw queue 非空时消费，可再按本节扩展。

### 4.1 新增 raw event 通知

新增一个公共事件：

```systemverilog
event raw_monitor_event;
```

所有 `push_raw_xxx()` 在成功写入 raw queue 或 CSR snapshot 后触发：

```systemverilog
function void push_raw_sfence(input dispatch_raw_sfence_t item);
    if (dispatch_monitor_capture_en && item.valid) begin
        raw_sfence_q.push_back(item);
        -> raw_monitor_event;
    end
endfunction
```

CSR 虽然不是 FIFO，但 `push_raw_csr()` 更新 latest snapshot 后也需要触发该事件：

```systemverilog
function void push_raw_csr(input dispatch_raw_csr_t item);
    if (dispatch_monitor_capture_en && item.valid) begin
        latest_raw_csr = item;
        latest_raw_csr_valid = 1'b1;
        latest_raw_csr_seq++;
        -> raw_monitor_event;
    end
endfunction
```

### 4.2 新增 pending 查询函数

建议新增以下 helper：

```systemverilog
function bit has_raw_int_wb();
function bit has_raw_iq_feedback();
function bit has_raw_ctrl();
function bit has_raw_sfence();
function bit has_raw_csr_update(input int unsigned last_seen_csr_seq);
function bit has_raw_monitor_pending(input int unsigned last_seen_csr_seq);
```

其中：

- `has_raw_int_wb()`：`raw_int_wb_q.size() != 0`
- `has_raw_iq_feedback()`：`raw_iq_feedback_q.size() != 0`
- `has_raw_ctrl()`：`raw_ctrl_q.size() != 0`
- `has_raw_sfence()`：`raw_sfence_q.size() != 0`
- `has_raw_csr_update(last_seen_csr_seq)`：`latest_raw_csr_valid && latest_raw_csr_seq != last_seen_csr_seq`
- `has_raw_monitor_pending(last_seen_csr_seq)`：上述任意一个为真

这样 sequence 不需要知道 queue 的内部变量名。

## 5. dispatch_monitor_event_adapter 调整方案

当前实现已拆分 CSR runtime drain 与 sfence/hfence 离散事件 drain。`drain_csr_events()` 只同步 CSR runtime，统一 service loop 通过 `collect_runtime_context_events()` 显式先 drain CSR、再 drain sfence。后续如果继续做 pending drain，可按下面形式集中判断：

```systemverilog
function void drain_pending_monitor_events(ref int unsigned last_seen_csr_seq);
    if (memblock_sync_pkg::has_raw_csr_update(last_seen_csr_seq) ||
        memblock_sync_pkg::has_raw_sfence()) begin
        drain_csr_events();
        drain_sfence_events();
        last_seen_csr_seq = memblock_sync_pkg::get_raw_csr_seq();
    end

    if (memblock_sync_pkg::has_raw_int_wb() ||
        memblock_sync_pkg::has_raw_iq_feedback()) begin
        drain_writeback_events(writeback_handler);
    end

    if (memblock_sync_pkg::has_raw_ctrl()) begin
        drain_exception_and_redirect_events(writeback_handler);
    end
endfunction
```

说明：

- CSR 和 sfence 在统一 service loop 中相邻且显式按序 drain，是因为 sfence/hfence 匹配需要 CSR runtime 上下文；但 CSR-only 路径不应隐式消费 sfence。
- writeback 和 IQ feedback 走 `writeback_status_handler`。
- ctrl/memoryViolation/deq 走 exception/redirect 和 LSQ free count 同步路径。
- 如果后续要求严格保持 CSR 与 sfence 的采样先后顺序，需要进一步把 CSR 也改成 ordered event，或在 raw sfence 中携带采样时 CSR snapshot。本方案先保持现有 latest CSR 模型，但消除 `#1` 轮询。

## 6. real smoke sequence 调整方案

### 6.1 新增 service clock 获取

`memblock_dispatch_real_smoke_sequence` 需要获取一个稳定的 service clock 来源。
由于 `seq_pkg` 先于 `tc_pkg` 编译，sequence 中不直接引用 `tc_if` 类型。
当前实现复用 `lintsissue_agent_agent_interface` 中的 `clk/rst_n` 作为 service clock。

示意：

```systemverilog
virtual lintsissue_agent_agent_interface service_vif;

if (!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null, get_full_name(), "vif", service_vif) &&
    !uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null, "uvm_test_top.env.u_lintsissue_agent_agent*", "vif", service_vif)) begin
    `uvm_fatal(get_type_name(), "failed to get lintsissue service clock vif")
end
```

### 6.2 下降沿服务 loop

`service_real_dispatch_flow()` 改成 `forever` 循环，不再使用 `max_service_steps` 和 `#1`：

```systemverilog
task service_real_dispatch_flow();
    ensure_service_vif();
    forever begin
        @(negedge service_vif.clk);
        if (service_vif.rst_n !== 1'b1 ||
            memblock_sync_pkg::reset_backend_done !== 1'b1) begin
            continue;
        end

        if (all_transactions_success()) begin
            break;
        end

        service_monitor_once();
        route_all_issue_queues();
        if (all_transactions_success()) begin
            break;
        end
    end
endtask
```

这样做的目的：

- monitor 多数在正沿 clocking block 采样，下降沿 service/route 可以避开同一正沿调度竞争。
- issue queue routing 是软件状态更新，不直接采 DUT 输出；放在下降沿可以形成“正沿采样事件，下降沿准备下一批发射”的节奏。
- 后续真正 drive 到 agent 仍由对应 driver/sequence 按自身 clocking block 时序完成。

## 7. service_monitor_once 的处理方式

`service_monitor_once()` 本次保持原有职责不变：统一 drain CSR、writeback、exception/redirect，
并推进 replay/recovery handler。区别只是调用节拍从 `#1` 软件轮询变成 DUT clock 下降沿。

## 8. 需要同步修改的文件

预计涉及：

- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`
  - 本次不修改

- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
  - 本次不修改

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`
  - 删除 `#1` loop
  - 删除 `max_service_steps`
  - 新增 `service_vif` 获取
  - `service_real_dispatch_flow()` 改为下降沿 `forever` loop

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_mixed_smoke_sequence.sv`
  - 删除 `max_service_steps` 参数读取

- `mem_ut/ver/ut/memblock/env/plus.sv`
  - 删除 `MEMBLOCK_REAL_SMOKE_MAX_SERVICE_STEPS`

- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
  - 删除 `real_smoke_max_service_steps` 字段、load、validate 和 getter

## 9. 风险与约束

- 取消 `max_service_steps` 后，若状态无法收敛，真实 smoke sequence 会一直等待。
  本次按需求移除该局部 timeout，依赖外层 testcase/仿真全局 timeout 兜底。
- 本次未把 raw queue/CSR pending 纳入 `all_transactions_success()`。如果 success 时仍有 raw event 残留，
  仍可能在 `end_test_check()` 中暴露，需要后续单独完善完成判据。
- `route_all_issue_queues()` 放到下降沿后，需要检查与 lintsissue/lsqenq sequence 的时序关系。

## 10. 验收点

- `memblock_dispatch_real_smoke_sequence.sv` 中不再存在 `#1` 轮询。
- `memblock_dispatch_real_smoke_sequence.sv` 不再使用 `max_service_steps`。
- `MEMBLOCK_REAL_SMOKE_MAX_SERVICE_STEPS` 从 `plus.sv`、`seq_csr_common.sv` 和 real smoke cfg 中删除。
- `service_monitor_once()` 和 `route_all_issue_queues()` 在 service clock 下降沿调用。
- `tc_sanity/base_fun` 远端编译通过。
- `tc_sanity/base_fun` 远端仿真通过，`UVM_ERROR=0`，`UVM_FATAL=0`。
