# 全局 Dispatch/Testbench Sync 设计说明

本文说明 mem_ut dispatch 测试框架中的全局同步层，核心源码位于：

- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`

## 1. 这个特性是什么

全局 Dispatch/Testbench Sync 是 testbench 内部的同步基础设施。它不模拟 DUT 的 MemBlock 逻辑，也不直接决定一条 transaction 是成功、replay 还是 redirect。它的作用是让 monitor、driver、sequence 和公共状态表之间共享同一套同步状态。

可以把它理解成 testbench 的全局协调层：

- monitor 只负责采 DUT 接口事实。
- driver/sequence 只负责驱动 DUT 输入接口。
- `common_data_transaction` 和各类 handler 才负责更新公共状态表。
- `memblock_sync_pkg` 提供这些模块之间共享的全局标志、软件周期和 raw event queue。

没有这层同步时，每个 monitor 可能各自直接更新状态，每个 driver 也可能各自判断是否 flush。这样一旦发生 redirect、replay、flushSb 或 L2TLB response，状态更新顺序会分散，迟到事件也容易误命中新 transaction。

## 2. 总体数据流

典型 real dispatch flow 如下：

```text
DUT 接口变化
  -> monitor 采样
  -> memblock_sync_pkg::push_raw_*() 入 raw queue
  -> memblock_dispatch_real_smoke_sequence::service_monitor_once()
  -> dispatch_monitor_event_adapter drain raw queue
  -> 转成 memblock_wb_event_t / LSQ deq / CSR runtime update
  -> writeback_status_handler / exception_redirect_replay_handler 更新公共状态
  -> issue/commit/redirect/L2TLB sequence 根据状态继续驱动 DUT
```

其中 `memblock_sync_pkg` 做两类事：

- 保存全局同步标志，例如是否处于 flush、monitor 是否允许采样、当前 service cycle。
- 保存 monitor 采到但尚未被 adapter 消费的 raw fact queue。

## 3. monitor 采样开关：dispatch_monitor_capture_en

源码字段：

```systemverilog
bit dispatch_monitor_capture_en = 1'b0;
```

使用位置：

```systemverilog
function void push_raw_int_wb(input dispatch_raw_int_wb_t item);
    if (dispatch_monitor_capture_en && item.valid) begin
        raw_int_wb_q.push_back(item);
    end
endfunction
```

`push_raw_iq_feedback()`、`push_raw_ctrl()` 也采用同样 FIFO 模式。`push_raw_csr()` 例外：
CSR runtime 是最新状态快照，只覆盖 `latest_raw_csr` 并递增 `latest_raw_csr_seq`。

### 3.1 字段含义

`dispatch_monitor_capture_en` 是 raw monitor 入队总开关。它不是控制 monitor 是否运行，而是控制 monitor 采到的接口事实是否允许进入公共 raw queue。

monitor 本身通常一直挂在接口旁边，如果 DUT pin 有变化，它可以看到。但 reset 期、主表未初始化、test 收尾阶段的接口变化不一定属于当前 testcase 的有效 transaction。如果这些事件进入 raw queue，后续 adapter 可能会尝试反查 active uid，造成错误日志或误更新。

### 3.2 为什么需要

testbench 中存在几个不应该采样为有效事件的窗口：

- reset 过程中 DUT 输出可能有初始值变化。
- 主表和状态表还没有建立，active ROB/LQ/SQ map 为空。
- test 结束后，DUT 可能还有迟到 writeback 或接口残留。
- `reset_all_tables()` 后，上一轮 queue 中残留的 raw fact 不应该影响下一轮。

因此，monitor 采样和状态机消费之间需要一个显式开关。

### 3.3 在本 flow 中怎么用

`common_data_transaction::reset_all_tables()` 中会清理公共状态，并打开采样：

```systemverilog
memblock_sync_pkg::clear_raw_monitor_queues();
memblock_sync_pkg::dispatch_monitor_capture_en = 1'b1;
```

`end_test_check()` 中会关闭采样：

```systemverilog
memblock_sync_pkg::dispatch_monitor_capture_en = 1'b0;
```

这样只有 real dispatch flow 的有效执行窗口内，monitor raw fact 才会进入公共队列。

### 3.4 例子

假设 reset 后某个 writeback valid 短暂为 1。如果没有 `dispatch_monitor_capture_en`，int writeback monitor 会调用 `push_raw_int_wb()` 把它放进 `raw_int_wb_q`。随后 adapter 消费这个事件时，可能发现没有 active uid，轻则打印无意义 warning，重则如果 ROB key 恰好复用，可能污染当前 transaction。

有了这个开关，reset 或收尾阶段的事件不会进入 raw queue。

## 4. 软件服务周期：dispatch_service_cycle

源码字段和函数：

```systemverilog
longint unsigned dispatch_service_cycle = 0;

function void tick_dispatch_service_cycle();
    dispatch_service_cycle++;
endfunction

function longint unsigned get_dispatch_service_cycle();
    return dispatch_service_cycle;
endfunction
```

使用位置：

```systemverilog
task memblock_dispatch_real_smoke_sequence::service_monitor_once();
    memblock_sync_pkg::tick_dispatch_service_cycle();
    collect_runtime_context_events();
    collect_monitor_event_batch();
    exception_redirect_replay_task();
endtask
```

### 4.1 字段含义

`dispatch_service_cycle` 是 testbench dispatch service loop 的软件周期计数。它不是 DUT clock，也不是仿真时间 `$time`，而是 real smoke sequence 每处理一轮 monitor/service 时递增一次。

### 4.2 为什么不用 `$time`

同一个仿真时间内可能发生多个 delta cycle，不同 agent 的 clocking block 采样顺序也可能不同。如果 recovery、timeout、raw monitor queue drain 都直接用 `$time` 或各自计数，跨模块判断会不一致。

`dispatch_service_cycle` 提供一个统一口径：只要进入一次 `service_monitor_once()`，就认为公共状态机推进了一轮。

### 4.3 在本 flow 中承担的功能

它被多个特性共享：

- redirect freeze timeout：记录 `redirect_freeze_cycle`，超时未 drive redirect 则 fatal。
- redirect drive done 边界：`redirect_drive_done_for()` 要求当前 cycle 大于 drive done cycle，避免同一轮刚 drive 就清表。
- PTW wait replay：`ptw_wait_replay_q` 记录 start cycle，用于 timeout。
- flushSb waiting empty：`flushsb_start_cycle` 用于等待 `sbIsEmpty` 超时。
- redirect input monitor 去反馈：`io_redirect_*` 是 TB 驱动 DUT 的 input 接口，不再回采后反馈 recovery；因此 service-cycle 不再用于 self redirect filter。

### 4.4 例子

redirect recovery 中：

```systemverilog
redirect_freeze_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
```

如果 redirect sequence 没有消费 payload，`advance_active_redirect()` 会根据当前 service cycle 和 `redirect_freeze_cycle` 计算等待时间。这样 timeout 表示“公共状态机已经推进了多少轮”，比用仿真时间更贴近 testbench flow。

## 5. redirect/flush 全局状态：dispatch_flush_in_progress

源码字段：

```systemverilog
bit dispatch_flush_in_progress = 1'b0;
```

设置位置：

```systemverilog
function void request_redirect_flush(input memblock_redirect_payload_t redirect);
    flush_in_progress = 1'b1;
    memblock_sync_pkg::dispatch_flush_in_progress = 1'b1;
    memblock_sync_pkg::dispatch_flush_epoch++;
    ...
endfunction
```

清除位置：

```systemverilog
function void apply_redirect_flush(input memblock_redirect_payload_t redirect);
    ...
    flush_in_progress  = 1'b0;
    memblock_sync_pkg::dispatch_flush_in_progress = 1'b0;
endfunction
```

### 5.1 字段含义

`dispatch_flush_in_progress` 是全局 redirect/flush recovery 标志。它表示当前 testbench 已经进入 recovery，旧上下文不应该继续 admission、issue、LSQENQ 或 commit。

`common_data_transaction` 内部也有 `flush_in_progress`，但 driver 和部分 agent 不一定能直接访问公共数据对象。因此 `memblock_sync_pkg` 提供一个全局可见标志，供 driver/sequence 快速判断。

### 5.2 为什么需要

redirect recovery 不只影响公共状态表，也影响正在等待 ready 的 driver。

例如 issue driver 正在等待某些 issue port ready。此时另一个 monitor 发现 memoryViolation 并触发 redirect。如果 issue driver 不知道全局 flush 已经开始，它可能继续 drive 旧 transaction 的 valid，让旧上下文进入 DUT。

### 5.3 在源码中的作用

issue driver 中：

```systemverilog
if (memblock_sync_pkg::dispatch_flush_in_progress ||
    tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
    clear_dispatch_issue_ports(tr);
    this.send_pkt(tr);
    tr.memblock_dispatch_aborted_by_redirect = 1'b1;
    return;
end
```

LSQENQ driver 中也有类似判断。

### 5.4 例子

假设一个 issue xaction 有 7 个 port：

- port 0、1 已经 fire。
- port 2、3 还在等 ready。
- 此时发生 redirect。

`dispatch_flush_in_progress=1` 后，driver 会停止继续 drive port 2、3，并通过 fired mask 只回报 port 0、1 已 fire。这样不会把未被 DUT 接受的 uop 从软件队列中误删。

## 6. flush 版本号：dispatch_flush_epoch

源码字段：

```systemverilog
int unsigned dispatch_flush_epoch = 0;
```

递增位置：

```systemverilog
memblock_sync_pkg::dispatch_flush_epoch++;
```

使用位置：

```systemverilog
tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
...
if (tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
    ...
end
```

### 6.1 字段含义

`dispatch_flush_epoch` 是全局 flush 版本号。每发生一次 redirect/flush recovery，就递增一次。

它比单纯的 `dispatch_flush_in_progress` 更精确。`dispatch_flush_in_progress` 只能表示“现在是否处于 flush”，而 epoch 可以表示“从 xaction 开始到现在，是否发生过新的 flush”。

### 6.2 为什么需要

driver 等 ready 时可能跨多个 cycle。某些情况下，driver 进入等待时 `dispatch_flush_in_progress=0`，但等待过程中发生了 redirect，又很快完成 software flush，导致 `dispatch_flush_in_progress` 重新变回 0。

如果只看 boolean，driver 可能错过这次 flush。epoch 快照可以解决这个问题：xaction 开始时保存旧 epoch，返回时发现全局 epoch 不一致，就说明中途发生过 flush。

### 6.3 例子

issue sequence 发 xaction 前：

```systemverilog
tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
```

driver 等 ready 时发现：

```systemverilog
tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch
```

说明这个 xaction 已经跨过 recovery 边界。即使当前 `dispatch_flush_in_progress` 已经清 0，也不能按正常成功处理。

## 7. raw event queue：monitor 只采事实

`memblock_sync_pkg` 中定义了三类 raw event queue，并单独维护 CSR latest snapshot：

```systemverilog
dispatch_raw_int_wb_t      raw_int_wb_q[$];
dispatch_raw_iq_feedback_t raw_iq_feedback_q[$];
dispatch_raw_ctrl_t        raw_ctrl_q[$];
dispatch_raw_csr_t         latest_raw_csr;
bit                        latest_raw_csr_valid;
int unsigned               latest_raw_csr_seq;
```

### 7.1 为什么是 raw queue

monitor 处在接口旁边，只应该采集事实，不应该直接改变公共状态表。原因是 monitor 缺少完整上下文：

- 它不知道当前 uid 是否仍 active。
- 它不知道 issue_epoch/replay_seq 是否匹配。
- 它不知道 redirect recovery 是否正在进行。
- 它不知道同一轮其他 monitor 是否也采到了更老的 DUT output recovery 事件。

因此 monitor 只调用 `push_raw_*()`，把接口事实放进 raw queue。后续由 `dispatch_monitor_event_adapter` 统一消费。

### 7.2 raw_int_wb_q

结构字段：

```systemverilog
typedef struct {
    bit               valid;
    int unsigned      port_id;
    bit               rob_valid;
    bit               rob_flag;
    bit [8:0]         rob_value;
    bit               lq_valid;
    bit               lq_flag;
    bit [6:0]         lq_value;
    bit               sq_valid;
    bit               sq_flag;
    bit [5:0]         sq_value;
    bit [23:0]        exception_vec;
    longint unsigned  cycle;
} dispatch_raw_int_wb_t;
```

功能：

- 保存 int writeback port 采到的 ROB/LQ/SQ 身份。
- 保存 exception vector。
- 保存 port_id，用于 adapter 判断是 load writeback、STA 相关 writeback 还是 STD 相关 writeback。

例子：

load writeback monitor 采到 port 0 valid 后，构造 `dispatch_raw_int_wb_t` 并调用：

```systemverilog
memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
```

adapter 后续在 `convert_raw_int_wb()` 中根据 port_id 设置 `source` 和 `target`，并通过 active map 解析 uid。

### 7.3 raw_iq_feedback_q

结构中关键字段：

```systemverilog
bit is_sta;
bit is_std;
bit hit;
bit flush_state;
bit [3:0] source_type;
bit vector_feedback;
```

功能：

- 保存 STA/STD IQ feedback。
- `hit=1` 表示 final success/pass。
- `hit=0` 表示 failed/replay。
- `flush_state` 只作为 PTW-back replay 的 metadata，不单独触发 redirect。
- `vector_feedback` 用于过滤当前尚未完整支持的 vector feedback。

例子：

STA feedback 返回 `hit=0, flush_state=1`。adapter 会设置：

```systemverilog
wb_event.replay_valid    = 1'b1;
wb_event.ptw_back_replay = 1'b1;
```

随后 exception/replay handler 会根据 `MEMBLOCK_REPLAY_WAIT_PTW_EN` 决定是否进入 PTW wait 队列。

### 7.4 redirect input monitor 不进入 raw recovery

`io_redirect_*` 是 redirect sequence/driver 送入 DUT 的 input 接口。`redirect_agent_agent_monitor` 可以继续做 X/Z 检查、日志、coverage 或 RM 预留，但不再构造 `dispatch_raw_redirect_t`，也不再调用 `push_raw_redirect()` 反馈 dispatch recovery。

redirect recovery 的触发源来自 DUT output ctrl monitor，例如 `io_mem_to_ooo_ctrl.memoryViolation` 进入 `raw_ctrl_q` 后由 adapter 转为 redirect 类 event。旧的 raw redirect queue 和 Self Redirect Filter 已删除/停用，不再作为公共特性。

### 7.5 raw_ctrl_q

结构中关键字段：

```systemverilog
bit [3:0]  lq_deq;
bit [1:0]  sq_deq;
bit        memory_violation_valid;
bit [8:0]  memory_violation_rob_value;
bit        memory_violation_level;
bit        sb_is_empty;
```

功能：

- 保存 LSQ deq 事实，用于释放本地 LQ/SQ 资源。
- 保存 memoryViolation，用于转换成 redirect event。
- 保存 `sbIsEmpty`，用于 flushSb directed flow 判断 Store Buffer 是否已经清空。

例子：

ctrl monitor 中有特殊判断：

```systemverilog
if (io_mem_to_ooo_lqDeq != '0 ||
    io_mem_to_ooo_sqDeq != '0 ||
    io_mem_to_ooo_memoryViolation_valid ||
    memblock_sync_pkg::dispatch_flushsb_waiting_empty) begin
    ...
    memblock_sync_pkg::push_raw_ctrl(raw_ctrl);
end
```

即使没有 deq 或 memoryViolation，只要正在等 flushSb empty，也会继续采 `sbIsEmpty`。

### 7.6 latest_raw_csr

结构中包含：

- satp/vsatp/hgatp mode、asid、vmid、ppn
- priv mxr/sum/vmxr/vsum/virt/spvp
- priv imode/dmode
- PBMT enable
- changed pulse

功能：

- 保存 runtime CSR snapshot。
- 给 L2TLB lookup 和地址转换相关逻辑使用。

`raw_csr_payload_changed()` 用于过滤重复 CSR level 状态，只有 CSR payload 变化或 changed pulse 出现时才值得推入 raw queue。

### 7.7 raw_sfence_q

`raw_sfence_q` 保存 fence monitor 采到的 `io_ooo_to_mem_sfence_*` 离散事件。它和 CSR runtime 不同：CSR 是 latest snapshot，可以覆盖；sfence/hfence 是一条条失效命令，必须 FIFO 消费，不能只保留 latest。

当前 service loop 不再让 `drain_csr_events()` 顺手处理 sfence/hfence，而是通过 `collect_runtime_context_events()` 显式先同步 CSR runtime，再调用 `drain_sfence_events()` 消费 `raw_sfence_q`。这样 L2TLB lookup、writeback drain、ctrl drain 这类只需要 CSR latest snapshot 的路径不会意外清掉 fence 事件。

## 8. adapter 统一消费 raw queue

核心类：

```systemverilog
class dispatch_monitor_event_adapter extends uvm_object;
```

核心 drain 函数：

```systemverilog
function void drain_csr_events();
function void drain_sfence_events();
task collect_writeback_events_batch(ref memblock_wb_event_t events[$]);
task collect_ctrl_redirect_events_batch(ref memblock_wb_event_t events[$]);
```

`drain_csr_events()` 只同步 CSR runtime latest snapshot，不消费 sfence/hfence。`drain_sfence_events()` 只消费 `raw_sfence_q` 中的离散 fence 事件。真实主控 service loop 通过 `collect_runtime_context_events()` 显式按 `drain_csr_events()` -> `drain_sfence_events()` 顺序处理，保证 sfence/hfence 匹配使用最新 runtime CSR，同时避免 CSR-only 路径隐式消费 fence 事件。

### 8.1 collect_monitor_event_batch

处理顺序：

```systemverilog
monitor_adapter.collect_writeback_events_batch(events);
monitor_adapter.collect_ctrl_redirect_events_batch(events);
monitor_batch_handler.process_monitor_event_batch(events);
```

作用：

- 把同一 service cycle 的 int writeback、IQ feedback 和 ctrl memoryViolation 收集进同一个 `events` batch。
- 由 `dispatch_monitor_batch_handler` 统一 normalize、选择 oldest redirect，并过滤被 redirect 覆盖的 stale event。
- 只有 batch 放行后的非 redirect event 才进入 `writeback_status_handler` 更新状态。

### 8.2 collect_ctrl_redirect_events_batch

处理顺序：

```systemverilog
while (memblock_sync_pkg::pop_raw_ctrl(raw_ctrl)) begin
    apply_raw_ctrl_deq(raw_ctrl);
    if (convert_raw_memory_violation(raw_ctrl, wb_event)) begin
        void'(writeback_handler.handle_event(wb_event));
    end
end
```

作用：

- 消费 ctrl raw event。
- ctrl event 中的 LSQ deq 和 `sbIsEmpty` 先落状态。
- 如果 ctrl event 带 memoryViolation，再转换成 redirect event，作为 redirect recovery 触发源。

### 8.3 为什么 adapter 要检查 active uid

转换函数会调用：

```systemverilog
event_has_active_uid(wb_event)
```

它最终通过 `common_data_transaction::resolve_uid_for_event()` 检查 event 是否能命中当前 active uid。

这样可以过滤：

- flush 后迟到的 writeback。
- reset 或收尾窗口残留事件。
- 已经 commit/deq/retire 的 transaction 的旧 feedback。

## 9. flushSb 等待提示：dispatch_flushsb_waiting_empty

源码字段：

```systemverilog
bit dispatch_flushsb_waiting_empty = 1'b0;
```

设置位置：

```systemverilog
function void mark_flushsb_driven(input longint unsigned cycle);
    flushsb_waiting_empty = 1'b1;
    memblock_sync_pkg::dispatch_flushsb_waiting_empty = 1'b1;
endfunction
```

清除位置：

```systemverilog
function void update_sb_is_empty(input bit sb_is_empty);
    if (flushsb_waiting_empty && sb_is_empty) begin
        flushsb_waiting_empty = 1'b0;
        memblock_sync_pkg::dispatch_flushsb_waiting_empty = 1'b0;
    end
endfunction
```

### 9.1 字段含义

`dispatch_flushsb_waiting_empty` 表示 testbench 已经 drive 过 `flushSb`，当前正在等待 DUT 的 `sbIsEmpty=1`。

它不是通用 monitor enable，也不是 redirect/self-filter 相关状态。它只服务 flushSb/sbIsEmpty directed 闭环。

### 9.2 为什么需要

`sbIsEmpty` 是 level 信号。DUT 可能在没有 lqDeq、sqDeq、memoryViolation 的情况下把 `sbIsEmpty` 变成 1。如果 ctrl monitor 只在 deq 或 memoryViolation 时采样，就可能永远看不到 empty 完成。

因此等待 flushSb empty 时，需要额外提示 ctrl monitor 继续采 raw ctrl。

### 9.3 例子

flushSb 已 drive：

```systemverilog
dispatch_flushsb_waiting_empty = 1'b1;
```

ctrl monitor 即使没有 deq，也会采样：

```systemverilog
memblock_sync_pkg::push_raw_ctrl(raw_ctrl);
```

adapter 消费 raw ctrl 后调用：

```systemverilog
data.update_sb_is_empty(raw.sb_is_empty);
```

看到 `sb_is_empty=1` 后，flushSb waiting 状态结束。

## 10. L2TLB 接管标志：l2tlb_responder_active

源码字段：

```systemverilog
bit l2tlb_responder_active = 1'b0;
```

connect 中设置：

```systemverilog
memblock_sync_pkg::l2tlb_responder_active = U_IF_NAME``_l2tlb_active;
```

sequence 中检查：

```systemverilog
if (!memblock_sync_pkg::l2tlb_responder_active) begin
    `uvm_fatal(...)
end
```

### 10.1 字段含义

`l2tlb_responder_active` 表示 mem_ut 的 L2TLB agent 是否已经接管 DTLB 与 L2TLB 之间的上游 request/response 接口。

这里的 L2TLB agent 代替的是 L2TLB 对上游 DTLB 的 responder 功能，不是 L2TLB 到 PTW/L2Cache 的下游模型。

### 10.2 为什么需要

PTW-back replay 等待需要确认 L2TLB response 真的通过接口回给 DUT。如果 connect 没有接管，sequence 即使构造了 response xaction，也不会真正驱动到 DTLB 接口。

因此 L2TLB base sequence 启动时必须检查该标志。

### 10.3 例子

如果 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=1`，connect 会让 L2TLB agent 接管 DTLB/L2TLB 接口，并置：

```systemverilog
l2tlb_responder_active = 1'b1;
```

此时 `memblock_l2tlb_base_sequence` 可以监听 DTLB request、查 common_data_transaction 中的 TLB 表，并把 response 发回 DTLB。

如果该标志为 0，sequence fatal，避免测试误以为 L2TLB response 已经真实返回。

## 11. 清理函数：clear_raw_monitor_queues

源码：

```systemverilog
function void clear_raw_monitor_queues();
    raw_int_wb_q.delete();
    raw_iq_feedback_q.delete();
    raw_ctrl_q.delete();
    latest_raw_csr = make_empty_raw_csr();
    latest_raw_csr_valid = 1'b0;
    latest_raw_csr_seq = 0;
    dispatch_service_cycle = 0;
endfunction
```

### 11.1 功能

它清理所有 raw queue，并重置 service cycle。

### 11.2 为什么需要

testbench 的 raw queue 是跨 monitor 和 adapter 的缓冲区。如果 reset 表时不清空，上一轮还没消费的 writeback、IQ feedback、ctrl event 会污染下一轮；CSR latest snapshot 也需要同步清 valid/seq，避免下一轮读取旧 CSR 状态。

`io_redirect_*` input monitor 不再反馈 recovery，因此没有 self redirect filter 残留需要清理。

### 11.3 使用场景

`reset_all_tables()`：

```systemverilog
memblock_sync_pkg::clear_raw_monitor_queues();
memblock_sync_pkg::dispatch_monitor_capture_en = 1'b1;
```

`end_test_check()`：

```systemverilog
if (memblock_sync_pkg::raw_monitor_queue_size() != 0) begin
    memblock_sync_pkg::clear_raw_monitor_queues();
end
```

## 12. 典型场景串联

### 12.1 普通 load writeback

1. int writeback monitor 采到 load writeback。
2. monitor 构造 `dispatch_raw_int_wb_t`，调用 `push_raw_int_wb()`。
3. `dispatch_monitor_capture_en=1`，raw event 入 `raw_int_wb_q`。
4. real smoke 调用 `service_monitor_once()`，递增 `dispatch_service_cycle`。
5. adapter 调用 `pop_raw_int_wb()` 消费 raw event。
6. `convert_raw_int_wb()` 转成 `memblock_wb_event_t`。
7. `event_has_active_uid()` 确认 event 命中当前 active uid。
8. `writeback_status_handler.handle_event()` 更新 load pass/writeback 状态。

### 12.2 redirect recovery

1. ctrl monitor 采到 `memoryViolation`，调用 `push_raw_ctrl()`。
2. adapter 消费 raw ctrl，先处理 deq/sbIsEmpty，再调用 `convert_raw_memory_violation()`。
3. memoryViolation 被转成 redirect event。
4. handler 调用 `request_redirect_flush()`。
5. `dispatch_flush_in_progress=1`，`dispatch_flush_epoch++`。
6. 正在等 ready 的 issue/LSQENQ driver 看到 flush 或 epoch 变化，停止继续 drive 旧 valid。
7. redirect sequence 消费 payload，真实 drive `io.redirect`。
8. drive done 后 software flush 清理 active uid。

### 12.3 flushSb directed flow

1. LSQ commit sequence 到指定 cycle，调用 `request_scheduled_flushsb_if_due()`。
2. 请求转成 `flushsb_pending`。
3. `should_drive_flushsb()` 确认没有 global flush 后，sequence drive `flushSb`。
4. `mark_flushsb_driven()` 置 `dispatch_flushsb_waiting_empty=1`。
5. ctrl monitor 即使没有 deq，也继续采 raw ctrl。
6. adapter 调用 `update_sb_is_empty()`。
7. 看到 `sbIsEmpty=1` 后，清 waiting 状态。

### 12.4 L2TLB response done

1. L2TLB connect 设置 `l2tlb_responder_active=1`。
2. L2TLB sequence 确认接管生效。
3. sequence 监听 DTLB request，结合 runtime CSR 和 TLB 表查到 uid。
4. sequence 真实发送 L2TLB response。
5. entry 确定后调用 `update_uid_tlb_records_by_entry(key, entry)` 回填 uid record。
6. PTW-back replay wait 看到 TLB 表项存在且 response done，才释放 replay。

## 13. 总结

全局 Dispatch/Testbench Sync 的核心价值是把 testbench 中分散的 monitor、driver、sequence、状态表串成一个稳定的闭环。

它的关键设计点是：

- `dispatch_monitor_capture_en` 控制有效采样窗口。
- `dispatch_service_cycle` 提供统一软件周期。
- `dispatch_flush_in_progress` 和 `dispatch_flush_epoch` 协调 redirect/flush 与 driver 等待 ready 的边界。
- raw event queue 让 monitor 只采事实，不直接改状态。
- adapter 统一消费 raw fact，并检查 active uid。
- `dispatch_flushsb_waiting_empty` 服务 flushSb/sbIsEmpty directed 闭环。
- `l2tlb_responder_active` 确认 L2TLB agent 已接管上游 DTLB responder 接口。

通俗地说，这一层就是 dispatch testbench 的“全局交通灯 + 事件缓冲区 + 软件周期计数器”。它不替 DUT 做决定，但保证 TB 内部所有组件在同一个时间口径和状态口径下协作。
