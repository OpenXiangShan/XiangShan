# flushSb 队列式请求实现 Review

状态：undo review
日期：2026-06-24
对应 Plan：`AI_DOC/plan/test_framework/plan/do/flushsb_queue_based_request_plan_20260623.md`

## 1. Review 结论

本次实现把 flushSb 从旧的“预约式 pending/scheduled 状态”改成“公共请求队列 + LSQ commit 单 consumer”模型。新的数据流是：

```text
producer
  -> common_data_transaction::push_flushsb_request()
  -> flushsb_req_q
  -> memblock_lsqcommit_dispatch_sequence::send_lsqcommit_cycle()
  -> 在普通 lsqcommit xaction 上附加 io_ooo_to_mem_flushSb=1
  -> ctrl monitor 回采 sbIsEmpty
  -> common_data_transaction::update_sb_is_empty()
```

Review 结果：

- 旧 `flushsb_pending/scheduled` 源码路径已删除。
- `MEMBLOCK_FLUSHSB_SEQ_EN` 只控制周期 producer，不 gate directed `push_flushsb_request()`。
- `MEMBLOCK_FLUSHSB_REQUEST_CYCLE=0` 表示不自动周期触发，不禁止 directed 入队。
- waiting empty 只阻止下一个 flushSb 请求出队，不阻止普通 ROB commit。
- timeout 改为一次性 `uvm_warning`，不 fatal、不退出 sequence。
- 基础验证通过：`eda_compile/tc_sanity`、`eda_run/tc_sanity`、`eda_run/tc_dispatch_real_smoke` 带周期 flushSb cfg。

## 2. 修改前逻辑

旧逻辑把 flushSb 拆成多组状态：预约状态、pending 状态、waiting empty 状态。`memblock_lsqcommit_dispatch_sequence` 每拍先把到期预约转成 pending，再单独构造 flushSb transaction；如果本拍发了 flushSb 或正在 waiting empty，会跳过普通 commit xaction。

旧行为的问题：

- producer 和 LSQ commit consumer 强耦合，其它 flow 触发 flushSb 必须复用 scheduled/pending 私有语义。
- flushSb drive 和普通 pendingPtr/commit drive 被拆成不同 xaction，不能自然表达“同拍普通 commit 字段 + flushSb pulse”。
- timeout 直接 fatal，不符合后续“只诊断、不由 flushSb flow 自行退出”的策略。

## 3. 修改后逻辑

新逻辑只有一个公共请求入口和一个 consumer：

- 所有 producer 只调用 `push_flushsb_request()` 入队。
- LSQ commit sequence 每拍构造普通 commit xaction，再尝试 `try_pop_flushsb_request()`。
- pop 成功时只把当前 xaction 的 `io_ooo_to_mem_flushSb` 置 1，不覆盖 pendingPtr/commit 字段。
- `mark_flushsb_driven()` 建立 active 请求，`update_sb_is_empty()` 等 ctrl monitor 回采后清状态。
- global stop 后，real smoke service loop 和 LSQ commit loop 都要等 `flushsb_request_pending()==0` 后退出。

## 4. 字段和类型

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`，类型：`memblock_flushsb_req_t`

函数功能简析：该类型是 flushSb 队列元素。它不参与 DUT 接口赋值，只用于记录请求 id、入队时间和来源，便于 timeout、completion 和 debug 日志定位。

```systemverilog
typedef struct {
    // flushSb请求编号只用于日志/debug，不参与DUT接口赋值。
    int unsigned              req_id;
    // 请求入队时的dispatch service cycle，用于定位请求滞留时间。
    longint unsigned          enqueue_cycle;
    // 请求来源标签：0=directed/unknown，1=periodic，后续可扩展其它producer。
    int unsigned              source;
} memblock_flushsb_req_t;
```

中文伪代码：

本段定义 flushSb 请求在测试框架内部的保存格式。
`req_id` 给每个请求分配 debug 编号，不送到 DUT。
`enqueue_cycle` 记录入队时的 dispatch service cycle，用来判断请求滞留时间。
`source` 记录请求来源，当前 0 表示 directed/unknown，1 表示周期 producer。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，字段组：flushSb runtime state

函数功能简析：这些字段是 flushSb queue flow 的公共真源。producer 写 `flushsb_req_q`，LSQ commit sequence 消费队列并写 active 状态，ctrl monitor 回采 `sbIsEmpty` 后清 active 状态。

```systemverilog
// flushSb待处理请求队列。所有producer只入队，LSQ commit sequence是唯一consumer。
memblock_flushsb_req_t      flushsb_req_q[$];
// 当前已经随lsqcommit xaction drive到DUT、正在等待sbIsEmpty的请求备份。
memblock_flushsb_req_t      active_flushsb_req;
bit                         active_flushsb_req_valid;
int unsigned                next_flushsb_req_id;
bit                         flushsb_waiting_empty;
longint unsigned            flushsb_start_cycle;
bit                         last_sb_is_empty;
bit                         flushsb_timeout_warned;
```

中文伪代码：

本段把 flushSb 状态拆成“未消费请求”和“已 drive 等待完成”两层。
`flushsb_req_q` 保存还没打到 DUT 的请求。
`active_flushsb_req/active_flushsb_req_valid` 保存已经打到 DUT、正在等待 `sbIsEmpty` 的请求备份。
`next_flushsb_req_id` 为新请求分配编号。
`flushsb_waiting_empty` 是 active 请求是否仍在等待 DUT SBuffer empty 的主状态。
`flushsb_start_cycle/flushsb_timeout_warned` 用于 timeout warning，且每个 active 请求最多报一次。
`last_sb_is_empty` 保存最近 monitor 回采值，便于 debug。

## 5. 公共队列 API

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`push_flushsb_request`

函数功能简析：这是所有 flushSb producer 的统一入口。输入是来源标签 `source`，副作用是向 `flushsb_req_q` 追加一个请求并递增 `next_flushsb_req_id`。

```systemverilog
function void push_flushsb_request(input int unsigned source = 0);
    memblock_flushsb_req_t req;

    req.req_id        = next_flushsb_req_id;
    req.enqueue_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    req.source        = source;
    next_flushsb_req_id++;
    flushsb_req_q.push_back(req);
    `uvm_info("COMMON_DATA",
              $sformatf("push flushSb request: req_id=%0d source=%0d enqueue_cycle=%0d queue_size=%0d",
                        req.req_id,
                        req.source,
                        req.enqueue_cycle,
                        flushsb_req_q.size()),
              UVM_LOW)
endfunction:push_flushsb_request
```

中文伪代码：

该逻辑负责把一个 flushSb 请求写入公共队列。
先创建局部请求对象 `req`。
把当前 `next_flushsb_req_id` 写入 `req_id`，再递增计数器。
调用 `memblock_sync_pkg::get_dispatch_service_cycle()` 读取当前 service cycle，记录请求入队时刻。
保存调用方传入的 `source`。
把请求追加到 `flushsb_req_q` 队尾。
打印 info，说明请求编号、来源、入队 cycle 和当前队列深度。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`try_pop_flushsb_request`

函数功能简析：这是 LSQ commit consumer 的唯一出队入口。输出参数 `req` 返回被消费的请求；如果当前已有 active flushSb、处于 global flush/redirect/freeze，或队列为空，则不出队。

```systemverilog
function bit try_pop_flushsb_request(output memblock_flushsb_req_t req);
    req = '{default:'0};
    if (flushsb_busy()) begin
        return 1'b0;
    end
    if (issue_blocked_by_global_flush()) begin
        return 1'b0;
    end
    if (!has_pending_flushsb_request()) begin
        return 1'b0;
    end
    req = flushsb_req_q.pop_front();
    return 1'b1;
endfunction:try_pop_flushsb_request
```

中文伪代码：

该逻辑负责判断本拍是否允许发一个新的 flushSb pulse。
先把输出 `req` 清成默认值，避免失败返回时带旧数据。
调用 `flushsb_busy()` 检查是否已有 active 请求正在等 `sbIsEmpty`；如果有，返回 false，不允许重叠发第二个 flushSb。
调用 `issue_blocked_by_global_flush()` 检查当前是否处于 redirect/global flush/issue freeze 控制期；如果是，返回 false，请求留在队列里。
调用 `has_pending_flushsb_request()` 检查队列是否非空；如果为空，返回 false。
所有条件都允许时，从队头弹出一个请求写入 `req`，返回 true，通知 LSQ commit sequence 本拍可以置 `io_ooo_to_mem_flushSb=1`。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`mark_flushsb_driven`

函数功能简析：该函数在 LSQ commit xaction 已经附加 flushSb pulse 后调用，把出队请求切换成 active waiting 状态。输入是请求和 drive cycle，副作用是设置 waiting 状态和 monitor 同步标志。

```systemverilog
function void mark_flushsb_driven(input memblock_flushsb_req_t req,
                                  input longint unsigned cycle);
    active_flushsb_req       = req;
    active_flushsb_req_valid = 1'b1;
    flushsb_waiting_empty    = 1'b1;
    flushsb_start_cycle      = cycle;
    last_sb_is_empty         = 1'b0;
    flushsb_timeout_warned   = 1'b0;
    memblock_sync_pkg::dispatch_flushsb_waiting_empty = 1'b1;
    `uvm_info("COMMON_DATA",
              $sformatf("drive flushSb request: req_id=%0d source=%0d enqueue_cycle=%0d start_cycle=%0d queue_size=%0d",
                        req.req_id,
                        req.source,
                        req.enqueue_cycle,
                        cycle,
                        flushsb_req_q.size()),
              UVM_LOW)
endfunction:mark_flushsb_driven
```

中文伪代码：

该逻辑记录一个请求已经真正打到 DUT。
把刚出队的 `req` 保存到 `active_flushsb_req`。
置 `active_flushsb_req_valid=1`，表示 active 请求备份有效。
置 `flushsb_waiting_empty=1`，表示后续需要等待 DUT 回报 `sbIsEmpty=1`。
记录 `flushsb_start_cycle`，供 timeout warning 计算等待时长。
清 `last_sb_is_empty` 和 `flushsb_timeout_warned`，避免沿用上一次请求的采样和 warning 状态。
置 `dispatch_flushsb_waiting_empty=1`，通知 ctrl monitor 在等待期间持续采样 `sbIsEmpty`。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`update_sb_is_empty`

函数功能简析：该函数由 ctrl monitor drain 路径调用。输入是当前采到的 `sbIsEmpty`，副作用是在 active 请求完成时清掉 waiting 状态。

```systemverilog
function void update_sb_is_empty(input bit sb_is_empty);
    last_sb_is_empty = sb_is_empty;
    if (flushsb_waiting_empty && sb_is_empty) begin
        `uvm_info("COMMON_DATA",
                  $sformatf("flushSb request completed: req_id=%0d source=%0d start_cycle=%0d done_cycle=%0d",
                            active_flushsb_req.req_id,
                            active_flushsb_req.source,
                            flushsb_start_cycle,
                            memblock_sync_pkg::get_dispatch_service_cycle()),
                  UVM_LOW)
        flushsb_waiting_empty    = 1'b0;
        active_flushsb_req       = '{default:'0};
        active_flushsb_req_valid = 1'b0;
        flushsb_start_cycle      = 0;
        flushsb_timeout_warned   = 1'b0;
        memblock_sync_pkg::dispatch_flushsb_waiting_empty = 1'b0;
    end
endfunction:update_sb_is_empty
```

中文伪代码：

该逻辑负责用 DUT 回采的 `sbIsEmpty` 完成 active flushSb 请求。
先把本次采样写入 `last_sb_is_empty`。
如果当前没有 active waiting，函数只记录采样后返回。
如果 `flushsb_waiting_empty=1` 且本次 `sb_is_empty=1`，说明 DUT SBuffer 已空。
打印 completion info，带请求编号、来源、开始 cycle 和完成 cycle。
清 `flushsb_waiting_empty`、`active_flushsb_req`、`active_flushsb_req_valid`、`flushsb_start_cycle` 和 `flushsb_timeout_warned`。
清 `dispatch_flushsb_waiting_empty`，让 ctrl monitor 回到普通采样条件。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`warn_flushsb_timeout_if_needed`

函数功能简析：该函数每拍检查 active flushSb 是否等待过久。输入 `timeout` 来自 `MEMBLOCK_FLUSHSB_TIMEOUT`，副作用是最多打印一次 warning，不清状态、不退出 sequence。

```systemverilog
function void warn_flushsb_timeout_if_needed(input int unsigned timeout);
    longint unsigned age;

    if (!flushsb_waiting_empty || timeout == 0 || flushsb_timeout_warned) begin
        return;
    end
    age = (memblock_sync_pkg::get_dispatch_service_cycle() >= flushsb_start_cycle) ?
          (memblock_sync_pkg::get_dispatch_service_cycle() - flushsb_start_cycle) : 0;
    if (age >= timeout) begin
        `uvm_warning("COMMON_DATA",
                     $sformatf("flushSb request timeout warning: req_id=%0d source=%0d age=%0d timeout=%0d start_cycle=%0d last_sb_is_empty=%0d",
                               active_flushsb_req.req_id,
                               active_flushsb_req.source,
                               age,
                               timeout,
                               flushsb_start_cycle,
                               last_sb_is_empty))
        flushsb_timeout_warned = 1'b1;
    end
endfunction:warn_flushsb_timeout_if_needed
```

中文伪代码：

该逻辑只做 timeout 诊断。
如果当前没有 active waiting，直接返回。
如果 `timeout=0`，表示关闭 timeout warning，直接返回。
如果当前 active 请求已经报过 timeout warning，直接返回，避免每拍刷屏。
否则用当前 dispatch service cycle 减去 `flushsb_start_cycle` 得到等待时间。
如果等待时间达到 timeout，打印一次 `uvm_warning`，并置 `flushsb_timeout_warned=1`。

## 6. LSQ commit consumer

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`，task：`drive_lsqcommit_loop`

函数功能简析：该 task 是 LSQ commit sequence 的长期循环。它在 global stop 后继续运行，直到 flushSb 队列和 active waiting 都收敛。

```systemverilog
forever begin
    bit has_progress;

    if (data.is_global_stop_requested() &&
        !data.flushsb_request_pending()) begin
        `uvm_info(get_type_name(),
                  $sformatf("stop LSQ commit loop by global_stop_requested at cycle=%0d",
                            cycle_idx),
                  UVM_LOW)
        break;
    end

    send_lsqcommit_cycle(cycle_idx, has_progress);
    cycle_idx++;
    if (has_progress) begin
        idle_count = 0;
    end else begin
        idle_count++;
        if (no_progress_warn_cycles != 0 &&
            idle_count >= no_progress_warn_cycles) begin
            `uvm_warning(get_type_name(), ...)
            idle_count = 0;
        end
    end
end
```

中文伪代码：

该逻辑负责持续驱动 pendingPtr 和 flushSb consumer。
每轮先检查 `global_stop_requested` 和 `flushsb_request_pending()`；只有主 transaction 完成且 flushSb 没有待消费或 waiting 状态时才退出。
调用 `send_lsqcommit_cycle()` 执行本拍核心逻辑，它会返回 `has_progress`。
如果本拍有普通 commit、成功 drive flushSb，或 active flushSb 仍在等待，则清 idle 计数。
如果没有进展，则累计 idle 计数；达到阈值只打印 warning，不退出。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`，task：`send_lsqcommit_cycle`

函数功能简析：该 task 是队列式 flushSb 的唯一消费点。输入是 cycle index，输出 `has_progress`；副作用是驱动一个 LSQ commit transaction，并在允许时附加 flushSb pulse。

```systemverilog
has_commit = 1'b0;
has_flushsb_progress = 1'b0;
has_progress = 1'b0;
data.warn_flushsb_timeout_if_needed(seq_csr_common::get_flushsb_timeout());
if (data.issue_blocked_by_global_flush()) begin
    tr = lsqcommit_agent_agent_xaction::type_id::create($sformatf("lsqcommit_dispatch_idle_tr_%0d", cycle_idx));
    commit_handler.clear_lsqcommit_xaction(tr);
    start_item(tr);
    finish_item(tr);
    has_progress = data.flushsb_request_pending();
    return;
end
commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit);
tr.set_name($sformatf("lsqcommit_dispatch_tr_%0d", cycle_idx));
if (data.try_pop_flushsb_request(flushsb_req)) begin
    tr.io_ooo_to_mem_flushSb = 1'b1;
    data.mark_flushsb_driven(flushsb_req,
                             memblock_sync_pkg::get_dispatch_service_cycle());
    has_flushsb_progress = 1'b1;
end

start_item(tr);
finish_item(tr);

if (has_commit) begin
    commit_handler.mark_rob_commit_batch(commit_uids);
end
has_progress = has_commit ||
               has_flushsb_progress ||
               data.flushsb_busy();
```

中文伪代码：

该逻辑把普通 commit 和 flushSb pulse 合并到同一个 xaction。
先清本拍局部状态。
调用 `warn_flushsb_timeout_if_needed()` 检查 active 请求是否超时；该子函数只 warning，不终止。
调用 `issue_blocked_by_global_flush()` 检查 redirect/global flush/issue freeze；如果被阻塞，则只驱动 idle xaction，并根据 `flushsb_request_pending()` 返回 progress，不弹出队列。
如果未被阻塞，调用 `build_lsqcommit_xaction()` 构造普通 commit xaction；该子函数选择连续可 commit uid 并填写 pendingPtr，默认 flushSb 为 0。
调用 `try_pop_flushsb_request()` 尝试消费一个 flushSb 请求；该子函数会检查 active waiting、global flush 和队列空闲条件。
如果出队成功，在同一个 `tr` 上置 `io_ooo_to_mem_flushSb=1`，并调用 `mark_flushsb_driven()` 进入 waiting empty。
驱动 `tr`。
如果本拍有普通 commit，调用 `mark_rob_commit_batch()` 把 commit 落到软件状态表。
最后用普通 commit、flushSb 出队或 active waiting 生成 `has_progress`，供外层 no-progress 统计。

## 7. 周期 producer

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_flushsb_base_sequence.sv`，task：`body`

函数功能简析：该 sequence 是周期性 producer，不直接驱动 DUT。输入来自 `MEMBLOCK_FLUSHSB_SEQ_EN`、`MEMBLOCK_FLUSHSB_REQUEST_CYCLE` 和 `MEMBLOCK_LSQCOMMIT_SEQ_EN`；副作用是按周期调用 `push_flushsb_request(1)`。

```systemverilog
seq_csr_common::init();
configure_from_plus();
if (!enable) begin
    `uvm_info(get_type_name(), "MEMBLOCK_FLUSHSB_SEQ_EN=0, periodic flushSb producer stays idle", UVM_LOW)
    return;
end
if (request_cycle == 0) begin
    `uvm_info(get_type_name(),
              "MEMBLOCK_FLUSHSB_REQUEST_CYCLE=0, periodic flushSb producer stays idle; directed producers can still push requests",
              UVM_LOW)
    return;
end
if (!seq_csr_common::get_lsqcommit_seq_en()) begin
    `uvm_info(get_type_name(),
              "MEMBLOCK_LSQCOMMIT_SEQ_EN=0, periodic flushSb producer cannot be consumed and stays idle",
              UVM_LOW)
    return;
end
```

中文伪代码：

该逻辑负责判断周期 producer 是否应该运行。
先初始化参数系统并读取 plus 配置。
如果 `MEMBLOCK_FLUSHSB_SEQ_EN=0`，打印 info 后退出，表示不启用周期 producer。
如果 `MEMBLOCK_FLUSHSB_REQUEST_CYCLE=0`，打印 info 后退出，表示不自动定时入队；directed producer 仍可直接调用公共入队 API。
如果 `MEMBLOCK_LSQCOMMIT_SEQ_EN=0`，打印 info 后退出，因为没有 consumer 能驱动 `io_ooo_to_mem_flushSb`。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_flushsb_base_sequence.sv`，task：`body`

函数功能简析：该片段是周期入队主循环。它等 reset/backend/main table ready 后开始计数，达到周期时向队列入一个 source=periodic 请求。

```systemverilog
ensure_handles();
cycle_count = 0;
forever begin
    if (data.is_global_stop_requested()) begin
        break;
    end
    wait_clock_tick();
    if (data.is_global_stop_requested()) begin
        break;
    end
    if (service_vif.rst_n !== 1'b1 ||
        memblock_sync_pkg::reset_backend_done !== 1'b1 ||
        !data.main_table_ready) begin
        continue;
    end
    cycle_count++;
    if ((cycle_count % request_cycle) == 0) begin
        data.push_flushsb_request(FLUSHSB_SOURCE_PERIODIC);
    end
end
```

中文伪代码：

该逻辑负责按周期产生请求。
调用 `ensure_handles()` 获取 common data 和 service clock vif。
循环中先检查 global stop；如果主 flow 已请求停止，则 producer 退出。
等待一个 service clock 上升沿。
再次检查 global stop，避免 stop 后多入队一个请求。
如果 reset 未释放、backend 未完成 reset、或主表还没 ready，则跳过本拍计数。
当运行条件满足时递增 `cycle_count`。
如果计数达到 `request_cycle` 的整数倍，调用 `push_flushsb_request(FLUSHSB_SOURCE_PERIODIC)` 向公共队列入队。

## 8. Top Flow 接入

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`，task：`service_real_dispatch_flow`

函数功能简析：这是 real smoke 的 runtime service loop。它 fork 周期 producer，并在 global stop 后继续服务 monitor，直到 flushSb flow drain 干净。

```systemverilog
task memblock_dispatch_real_smoke_sequence::service_real_dispatch_flow();
    memblock_flushsb_base_sequence flushsb_seq;

    ensure_service_vif();
    flushsb_seq = memblock_flushsb_base_sequence::type_id::create("flushsb_seq");
    fork
        flushsb_seq.start(null);
    join_none
    forever begin
        @(negedge service_vif.clk);
        if (service_vif.rst_n !== 1'b1 ||
            memblock_sync_pkg::reset_backend_done !== 1'b1) begin
            continue;
        end
        service_monitor_once();
        if (!data.is_global_stop_requested()) begin
            route_all_issue_queues();
        end
        void'(all_transactions_success());
        if (data.is_global_stop_requested() &&
            !data.flushsb_request_pending()) begin
            break;
        end
    end
endtask:service_real_dispatch_flow
```

中文伪代码：

该逻辑把周期 producer 纳入 real smoke 主服务循环。
先获取 service vif。
创建 `memblock_flushsb_base_sequence`，用 `fork/join_none` 后台启动。
每拍等待 service clock 负边沿，reset/backend 未 ready 时跳过。
调用 `service_monitor_once()`，继续处理 writeback、ctrl monitor、redirect/replay 和 `sbIsEmpty` 回采。
如果还没有 global stop，则继续 route issue queue；global stop 后不再 route 新 issue。
调用 `all_transactions_success()` 推进全局完成判断。
只有 `global_stop_requested=1` 且 `flushsb_request_pending()` 为 false，才退出 service loop，保证 flushSb 队列和 active waiting 都已清空。

## 9. 参数语义

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`，函数：参数合法性检查段

函数功能简析：该段调整 flushSb 参数语义。`flushsb_timeout=0` 不再 clamp，`LSQCOMMIT_SEQ_EN=0` 时也不 fatal，只提示周期 producer 会 idle。

```systemverilog
if (flushsb_seq_en && flushsb_request_cycle != 0 && !lsqcommit_seq_en) begin
    `uvm_info("SEQ_CSR_CFG",
              "MEMBLOCK_FLUSHSB_REQUEST_CYCLE is configured but MEMBLOCK_LSQCOMMIT_SEQ_EN=0; periodic flushSb producer will stay idle",
              UVM_LOW)
end
```

中文伪代码：

该逻辑只做配置提示。
如果用户开启周期 flushSb，且配置了非 0 周期，但关闭了 LSQ commit sequence，则没有 consumer 能驱动 flushSb。
此时打印 info，说明 periodic producer 会 idle。
不把配置改成 0，不报 fatal，也不阻止 directed producer 在其它场景中使用公共入队 API。

源码位置：`mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_smoke_flushsb.cfg`

函数功能简析：该 cfg 是本次新增的基础验证入口，用于稳定打开周期 flushSb 并运行 real smoke，避免多个 plusarg 通过 Makefile `plus_arg` 传递时被转义成一个参数。

```systemverilog
+MEMBLOCK_FLUSHSB_SEQ_EN=1
+MEMBLOCK_FLUSHSB_REQUEST_CYCLE=20
+MEMBLOCK_FLUSHSB_TIMEOUT=2000
```

中文伪代码：

该配置打开周期 flushSb producer。
每 20 个有效 service clock 向公共队列入队一个 flushSb 请求。
active flushSb 等待 `sbIsEmpty` 超过 2000 个 dispatch service cycle 时只打印 warning。
该 cfg 用于回归复现本次 queue producer/consumer 路径。

## 10. 文件接入和文档同步

源码位置：`mem_ut/ver/ut/memblock/seq/seq_pkg.sv`，package include

函数功能简析：该片段把新 sequence 编入 `seq_pkg`，使 testcase 能通过 factory 创建 `memblock_flushsb_base_sequence`。

```systemverilog
`include "memblock_lsqcommit_dispatch_sequence.sv"
`include "memblock_flushsb_base_sequence.sv"
`include "memblock_redirect_dispatch_sequence.sv"
```

中文伪代码：

该逻辑负责 sequence 编译接入。
先 include LSQ commit sequence。
再 include 新增 flushSb 周期 producer sequence。
后续 real smoke 中创建 `memblock_flushsb_base_sequence` 时，UVM factory 能找到该类。

文档同步：

- `AI_DOC/mem_ut_flow_doc/flushsb_test_flow.md`：重写为队列式 flushSb flow。
- `AI_DOC/mem_ut_flow_doc/rob_commit_lq_sq_deq_flow.md`：同步 LSQ commit 上附加 flushSb pulse 的真实调用链。
- `AI_DOC/mem_ut_flow_doc/main_table_build_and_stimulus_flow.md`：同步 real smoke service loop 中启动 producer 和等待 drain 的行为。
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`：同步参数语义，不再引用 `request_flushsb()`。

## 11. 正确性检查

| 检查点 | 结论 |
|---|---|
| 旧预约式字段是否删除 | 源码中 `flushsb_pending/flushsb_scheduled_*` 无残留。 |
| 旧函数是否删除 | 源码中 `request_flushsb/arm_scheduled_flushsb/request_scheduled_flushsb_if_due/should_drive_flushsb/flushsb_timed_out` 无残留。 |
| 是否允许 directed producer 绕过 seq enable 入队 | `push_flushsb_request()` 不检查 `MEMBLOCK_FLUSHSB_SEQ_EN`。 |
| waiting empty 是否阻止普通 commit | `send_lsqcommit_cycle()` 先 build 普通 commit xaction，再 `try_pop_flushsb_request()`；`flushsb_busy()` 只阻止新 flushSb 出队。 |
| global flush 时是否丢请求 | `try_pop_flushsb_request()` 在 `issue_blocked_by_global_flush()` 时返回 false，队列不 pop。 |
| timeout 是否不再 fatal | `warn_flushsb_timeout_if_needed()` 只 `uvm_warning` 并置 `flushsb_timeout_warned`。 |
| final check 是否覆盖未完成 flushSb | `end_test_check()` 调用 `flushsb_request_pending()`。 |

## 12. 验证结果

执行命令：

```text
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke_flushsb
```

结果：

- `eda_compile tc_sanity/base_fun`：通过。
- `eda_run tc_sanity/base_fun`：`TEST CASE PASSED`，`UVM_ERROR=0`，`UVM_FATAL=0`；存在既有 main table wait warning，因为 `tc_sanity` 不构建 real smoke main table。
- `eda_run tc_dispatch_real_smoke/base_fun cfg=tc_dispatch_real_smoke_flushsb`：`TEST CASE PASSED`，`UVM_WARNING=0`，`UVM_ERROR=0`，`UVM_FATAL=0`。

定向 flushSb 运行日志确认：

```text
push flushSb request: req_id=0 source=1 enqueue_cycle=20 queue_size=1
drive flushSb request: req_id=0 source=1 enqueue_cycle=20 start_cycle=20 queue_size=0
flushSb request completed: req_id=0 source=1 start_cycle=20 done_cycle=22
```

## 13. Plan 对齐检查

找到对应 plan：

- `AI_DOC/plan/test_framework/plan/do/flushsb_queue_based_request_plan_20260623.md`

### 13.1 实现与 Plan 不一致项

均和计划保持一致，没有发现需要解释的行为偏离。

### 13.2 Plan 未说明但 Coding 落实的细节

细节 1：新增 `tc_dispatch_real_smoke_flushsb.cfg` 作为可复现验证入口。

源码位置：`mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_smoke_flushsb.cfg`

```systemverilog
+MEMBLOCK_FLUSHSB_SEQ_EN=1
+MEMBLOCK_FLUSHSB_REQUEST_CYCLE=20
+MEMBLOCK_FLUSHSB_TIMEOUT=2000
```

中文伪代码：

该配置不是新功能逻辑，而是验证入口。
它打开周期 producer，并设置 20 cycle 入队一次。
这样可以通过 `cfg=tc_dispatch_real_smoke_flushsb` 稳定复现队列入队、LSQ commit 消费、`sbIsEmpty` 回采和 final drain。
增加该 cfg 的原因是 Makefile 的 `plus_arg` 一次传多个参数时会被远端 shell 转义成一个 plusarg，导致解析失败。

细节 2：`flushsb_request_pending()` 额外检查 `active_flushsb_req_valid`。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`flushsb_request_pending`

```systemverilog
function bit flushsb_request_pending();
    return has_pending_flushsb_request() ||
           flushsb_busy() ||
           active_flushsb_req_valid;
endfunction:flushsb_request_pending
```

中文伪代码：

该逻辑判断 flushSb flow 是否完全收尾。
先检查队列是否还有未消费请求。
再检查是否仍在等待 `sbIsEmpty`。
最后检查 active 请求备份有效位，作为状态一致性的兜底检查。
该细节不改变正常流程；正常完成时 `update_sb_is_empty()` 会同时清 `flushsb_waiting_empty` 和 `active_flushsb_req_valid`。

## 14. Diff 覆盖检查

本 review 对照本次准备提交的 diff 覆盖以下文件：

- `AI_DOC/mem_ut_flow_doc/flushsb_test_flow.md`
- `AI_DOC/mem_ut_flow_doc/main_table_build_and_stimulus_flow.md`
- `AI_DOC/mem_ut_flow_doc/rob_commit_lq_sq_deq_flow.md`
- `AI_DOC/plan/test_framework/plan/do/flushsb_queue_based_request_plan_20260623.md`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_smoke_flushsb.cfg`
- `mem_ut/ver/ut/memblock/seq/seq.f`
- `mem_ut/ver/ut/memblock/seq/seq_pkg.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_flushsb_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`

不纳入本次 review 和提交的既有脏改：

- `mem_ut/ver/ut/memblock/seq/base_seq/exception_redirect_replay_handler.sv`
- `AI_DOC/project_management/mem_ut_code_review_document_rule.md`
- `AI_DOC/project_management/mem_ut_flow_document_rule.md`
- recovery commit frontier 相关删除和其它未跟踪目录
