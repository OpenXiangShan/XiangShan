# active sequence global stop 生命周期源码 Review

## 1. Review 范围

本文审查本轮 mem_ut dispatch 主动 sequence 生命周期修改。核心目标是把 LSQENQ、LINTSISSUE、LSQCOMMIT 三类主动 sequence 的正常退出条件统一到 `common_data_transaction::global_stop_requested`，删除旧的 idle/start/max_cycles 正常退出逻辑，并用 `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES` 只做 debug warning。

涉及源码和配置文件：

| 文件 | 本轮覆盖点 |
|---|---|
| `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv` | 新增 `global_stop_requested`，新增 `transaction_done()`、`request_global_stop_if_done()`、`is_global_stop_requested()`。 |
| `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv` | 顶层 `all_transactions_success()` 改为单点请求 global stop。 |
| `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv` | LSQ admission loop 改为 global stop 退出；`enable=0` 不再调用 `super.body()`；无候选时发送 idle item。 |
| `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv` | issue loop 改为 global stop 退出；`enable=0` 不再调用 `super.body()`；保留 ready timeout。 |
| `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv` | commit loop 改为 global stop 加 flushSb drain 退出；新增 `lsqcommit_vif` clock wait；删除旧 activity 扫描。 |
| `mem_ut/ver/ut/memblock/env/plus.sv` | 删除旧 idle/start/max_cycles 参数，新增 `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES`。 |
| `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv` | 删除旧 getter 和 clamp，新增统一 debug 参数 getter。 |
| `mem_ut/ver/ut/memblock/seq/plus_cfg/*.cfg` | 默认保持 `MEMBLOCK_LSQENQ_SEQ_EN=1` 和 `MEMBLOCK_LSQCOMMIT_SEQ_EN=1`；dispatch real cfg 使用统一 warning 阈值。 |
| `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` | 同步参数语义说明。 |
| `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv` | 删除未接入主流程的 `uid_has_pending_route_work()`，保留 route/select/fire 三层真实检查链。 |

术语说明：

| 术语 | 通俗解释 | 代码落点 |
|---|---|---|
| global stop | 顶层确认所有主表 transaction 已最终 success 后设置的统一停止标志。 | `common_data_transaction::global_stop_requested` |
| success 前缀 | 从 uid0 开始连续 `success=1` 的完成区间。达到 `main_trans_num` 表示全体完成。 | `dispatch_progress.success_prefix_uid` |
| 主动 sequence | 不靠 agent 默认随机激励，而是常驻读取公共主表/状态并主动驱动 DUT 的 sequence。 | LSQENQ、LINTSISSUE、LSQCOMMIT 三个 dispatch sequence |
| no progress warning | 没有功能进展达到阈值时打印 warning；它不是退出条件。 | `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES` |

## 2. 特性一：公共 global_stop 统一停止条件

### 2.1 修改前逻辑

修改前 `memblock_dispatch_real_smoke_sequence::all_transactions_success()` 同时检查 issue queue、raw monitor queue、active map、flush/redirect/flushSb/PTW wait 等全局状态，再推进 success 前缀并返回完成状态。三个主动 sequence 各自还有 idle stop、start timeout 或 max cycles，容易形成多个停止真源。

旧行为的问题是：顶层认为 testcase 尚未完成时，某个子 sequence 可能因 idle 或 max_cycles 先退出；或者子 sequence 等待主表时用 start timeout fatal，和真实 dispatch smoke 的启动顺序耦合。对于长 testcase 或 10 万笔场景，这类局部退出条件会变成误停风险。

### 2.2 修改后逻辑

`common_data_transaction` 新增 `global_stop_requested`。顶层 real smoke 的运行期 `all_transactions_success()` 为了 10 万笔场景性能，只通过 success 前缀请求 `global_stop_requested`；子 sequence 不再重复判断成功条件，只调用 `is_global_stop_requested()` 决定退出。

这不表示最终一致性检查被删除。`memblock_dispatch_real_smoke_sequence::body()` 在 `service_real_dispatch_flow()` 返回后仍调用 `data.end_test_check()`。`end_test_check()` 会检查 raw monitor queue、active ROB/LQ/SQ map、load/STA/STD issue queue、flush/redirect 控制状态、redirect drive 状态、flushSb 状态和 PTW wait replay queue 是否收尾干净。也就是说，本轮修改把“每拍运行期 stop 判断”从全局状态扫描改成 success 前缀，但保留“测试结束时最终一致性检查”。

### 2.3 行为概述

```text
顶层 real smoke body：
  先调用 build_main_table() 生成主表、状态表和公共进度。
  再进入 service_real_dispatch_flow()，按 service_vif.clk 下降沿推进真实 dispatch flow。
  service loop 返回后，调用 data.end_test_check() 做最终一致性检查。

service_real_dispatch_flow 每拍逻辑：
  等待 service_vif.clk 下降沿。
  如果 rst_n 不是 1，或者 reset_backend_done 还没完成：
    跳过本拍，不做 monitor 收集和 issue route。
  先调用 all_transactions_success()：
    如果已经完成，则退出 service loop。
  调用 service_monitor_once()：
    推进 dispatch service cycle。
    收集 CSR/runtime context。
    收集 raw writeback、IQ feedback、memoryViolation batch。
    处理 exception、redirect、replay。
  调用 route_all_issue_queues()：
    把已满足条件的 uid route 到 load/STA/STD issue queue。
  再调用 all_transactions_success()：
    如果本拍 monitor/route 后 success 前缀已经到 main_trans_num，则退出 service loop。

all_transactions_success 运行期停止判断：
  如果 data 为空，或者 main_trans_num 为 0：
    返回 false。
  调用 data.request_global_stop_if_done()：
    内部先推进 success 前缀。
    如果 success_prefix_uid >= main_trans_num：
      设置 global_stop_requested = 1。
  返回 data.is_global_stop_requested()。

data.end_test_check 最终一致性检查：
  先关闭 dispatch monitor capture。
  如果 raw monitor queue 还有残留：
    报 uvm_error，并清空 raw monitor queue。
  如果 main_trans_num 为 0：
    直接返回。
  如果 next_uid != main_trans_num：
    报 uid 分配数量错误。
  遍历每个 uid：
    如果 status_by_uid[uid] 为空：
      报 uvm_fatal。
    如果该 uid 仍 active，或者 exception/replay/redirect pending 未清：
      报 uvm_error。
  如果 active ROB/LQ/SQ map 不为空：
    报 uvm_error。
  如果 load/STA/STD issue queue 不为空：
    报 uvm_error。
  如果 flush/redirect/freeze 控制状态不 idle：
    报 uvm_error。
  如果 redirect drive queue 或 redirect phase 不 idle：
    报 uvm_error。
  如果 flushSb pending/waiting/scheduled 状态不 idle：
    报 uvm_error。
  如果 PTW wait replay queue 不为空：
    报 uvm_error。

三类 active sequence 退出规则：
  子 sequence 不再自己判断 testcase 是否完成。
  子 sequence 每轮读取 data.is_global_stop_requested()。
  读到 1 后打印 stop info 并退出自己的主循环。
  无进展达到阈值时只打印 warning，不退出主循环。
```

### 2.4 正确性检查

- 停止真源唯一：只有顶层通过 success 前缀请求 stop，LSQENQ/LINTSISSUE/LSQCOMMIT 只读标志。
- 不再因局部 idle 停止：无进展只 warning，不能提前退出主动 driver。
- reset 正确：`new()` 和 `reset_all_tables()` 都清 `global_stop_requested`，下一轮 testcase 不继承旧 stop。
- 性能边界：完成判断继续依赖 success 前缀，不需要每拍全表扫描。
- 最终一致性边界：运行期 `all_transactions_success()` 不再每拍检查队列/map/flush 状态，但 `body()` 结束前的 `end_test_check()` 仍检查这些对象，防止残留 active map、issue queue 或 flush/redirect/PTW 状态被静默放过。

### 2.5 源码支撑片段

这组片段只做本特性的局部佐证。每个源码块只对应一个独立逻辑对象，源码后紧跟该对象自己的中文伪代码。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，字段：`global_stop_requested`。
函数功能描述：`global_stop_requested` 是 common data 中的公共停止请求标志，由顶层 completion 判断写入，被 LSQENQ/LINTSISSUE/LSQCOMMIT 读取。

```systemverilog
bit            global_stop_requested;
```

中文伪代码：

```text
定义 global_stop_requested：
  保存顶层 orchestration 发出的全局停止请求。
  子 sequence 不修改它，只读取它决定是否退出主动驱动 loop。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`new()`。
函数功能描述：构造 common data 对象时初始化主表元信息，并清零 `global_stop_requested`。

```systemverilog
function new(string name = "common_data_transaction");
    super.new(name);
    main_trans_num      = 0;
    next_uid            = 0;
    main_table_ready    = 1'b0;
    global_stop_requested = 1'b0;
endfunction:new
```

中文伪代码：

```text
new：
  初始化主表数量为 0。
  初始化下一个 uid 为 0。
  标记主表尚未 ready。
  将 global_stop_requested 清 0，保证新对象默认不请求停止。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`reset_all_tables()`。
函数功能描述：每次重建主表时重置主表元信息和 global stop 标志，避免上一轮状态残留。

```systemverilog
function void reset_all_tables(input int unsigned main_trans_num_i);
    main_trans_num      = main_trans_num_i;
    next_uid            = 0;
    main_table_ready    = 1'b0;
    global_stop_requested = 1'b0;
endfunction
```

中文伪代码：

```text
reset_all_tables：
  接收新的 main_trans_num_i。
  将 main_trans_num 更新为本轮主表数量。
  将 uid 分配游标 next_uid 清 0。
  将 main_table_ready 清 0，表示主表还在重建。
  将 global_stop_requested 清 0，避免上一轮 testcase 的 stop 请求影响本轮。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`transaction_done()`。
函数功能描述：根据 success 前缀判断全部主表 transaction 是否完成。

```systemverilog
function bit transaction_done();
    advance_success_prefix();
    return dispatch_progress.success_prefix_uid >= main_trans_num;
endfunction:transaction_done
```

中文伪代码：

```text
transaction_done：
  先调用 advance_success_prefix() 推进连续 success uid 边界。
  如果 success_prefix_uid 已经达到 main_trans_num：
    返回 true，表示全部 transaction 完成。
  否则返回 false。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`request_global_stop_if_done()`。
函数功能描述：在全部 transaction 完成时写入 global stop 请求。

```systemverilog
function void request_global_stop_if_done();
    if (transaction_done()) begin
        global_stop_requested = 1'b1;
    end
endfunction:request_global_stop_if_done
```

中文伪代码：

```text
request_global_stop_if_done：
  调用 transaction_done()。
  如果 transaction_done() 返回 true：
    将 global_stop_requested 置 1。
  如果未完成：
    不改变 global_stop_requested。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`is_global_stop_requested()`。
函数功能描述：给子 sequence 提供只读查询接口，返回当前 global stop 请求状态。

```systemverilog
function bit is_global_stop_requested();
    return global_stop_requested;
endfunction:is_global_stop_requested
```

中文伪代码：

```text
is_global_stop_requested：
  返回 global_stop_requested 当前值。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`，函数：`all_transactions_success()`。
函数功能描述：顶层 service loop 的运行期完成判断入口；它只请求 global stop，不直接扫描全部队列/map。

```systemverilog
function bit memblock_dispatch_real_smoke_sequence::all_transactions_success();
    if (data == null || data.main_trans_num == 0) begin
        return 1'b0;
    end
    data.request_global_stop_if_done();
    return data.is_global_stop_requested();
endfunction:all_transactions_success
```

中文伪代码：

```text
all_transactions_success：
  如果 data 为空，或者 main_trans_num 为 0：
    返回 false。
  调用 data.request_global_stop_if_done()：
    让 common data 根据 success 前缀决定是否置 global_stop_requested。
  返回 data.is_global_stop_requested()：
    作为 service loop 是否退出的判断。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`，函数：`body()`。
函数功能描述：real smoke 顶层入口，负责建表、运行 service loop，并在 loop 结束后调用最终一致性检查。

```systemverilog
task memblock_dispatch_real_smoke_sequence::body();
    build_main_table();
    `uvm_info(get_type_name(),
              $sformatf("real dispatch smoke main table ready: main_trans_num=%0d",
                        data.main_trans_num),
              UVM_LOW)
    service_real_dispatch_flow();
    data.end_test_check();
    `uvm_info(get_type_name(), "real dispatch smoke sequence completed", UVM_LOW)
endtask:body
```

中文伪代码：

```text
body：
  调用 build_main_table() 生成 dispatch 主表。
  打印主表 transaction 数量。
  调用 service_real_dispatch_flow() 进入真实 dispatch service loop。
  service loop 退出后调用 data.end_test_check() 做最终一致性检查。
  打印 real dispatch smoke sequence completed。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`，函数：`service_real_dispatch_flow()`。
函数功能描述：按 service clock 推进真实 dispatch flow，每拍先检查完成，再收集 monitor/recovery 事件并 route issue。

```systemverilog
task memblock_dispatch_real_smoke_sequence::service_real_dispatch_flow();
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
endtask:service_real_dispatch_flow
```

中文伪代码：

```text
service_real_dispatch_flow：
  确保 service_vif 已获取。
  forever 循环：
    等待 service_vif.clk 下降沿。
    如果 reset 未完成：
      continue，跳过本拍。
    先调用 all_transactions_success()：
      如果已完成，break。
    调用 service_monitor_once()：
      收集 runtime/monitor/recovery 事件。
    调用 route_all_issue_queues()：
      将 ready uid 路由到 issue queue。
    再调用 all_transactions_success()：
      如果本拍处理后已完成，break。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`end_test_check()`。
函数功能描述：testcase 结束时的最终一致性检查，关闭 monitor capture，检查公共队列/map/recovery 状态是否清空，并对残留状态报错。

```systemverilog
function void end_test_check();
    int unsigned uid;

    memblock_sync_pkg::dispatch_monitor_capture_en = 1'b0;
    if (memblock_sync_pkg::raw_monitor_queue_size() != 0) begin
        `uvm_error("COMMON_DATA",
                         $sformatf("clear %0d raw monitor events at end_test_check",
                                   memblock_sync_pkg::raw_monitor_queue_size()))
        memblock_sync_pkg::clear_raw_monitor_queues();
    end
    if (main_trans_num == 0) begin
        return;
    end
    if (next_uid != main_trans_num) begin
        `uvm_error("COMMON_DATA", $sformatf("uid allocation mismatch: next_uid=%0d main_trans_num=%0d", next_uid, main_trans_num))
    end
    for (uid = 0; uid < main_trans_num; uid++) begin
        if (status_by_uid[uid] == null) begin
            `uvm_fatal("COMMON_DATA", $sformatf("status_by_uid[%0d] is null at end_test_check", uid))
        end
        if (status_by_uid[uid].active ||
            status_by_uid[uid].exception_pending ||
            status_by_uid[uid].replay_pending ||
            status_by_uid[uid].redirect_pending) begin
            `uvm_error("COMMON_DATA", $sformatf("uid=%0d has unfinished status at end_test_check", uid))
        end
    end
    if (uid_by_active_rob.num() != 0 || uid_by_lq.num() != 0 || uid_by_sq.num() != 0) begin
        `uvm_error("COMMON_DATA", "active ROB/LQ/SQ mapping is not empty at end_test_check")
    end
    if (load_issue_q.size() != 0 || sta_issue_q.size() != 0 || std_issue_q.size() != 0) begin
        `uvm_error("COMMON_DATA", "issue queues are not empty at end_test_check")
    end
    if (flush_in_progress || active_redirect.valid || issue_freeze_ack) begin
        `uvm_error("COMMON_DATA", "global flush/redirect control state is not idle at end_test_check")
    end
    if (has_pending_redirect_drive() || redirect_phase != MEMBLOCK_REDIRECT_PHASE_IDLE) begin
        `uvm_error("COMMON_DATA", "redirect drive queue/state is not idle at end_test_check")
    end
    if (flushsb_pending || flushsb_waiting_empty || flushsb_scheduled_pending) begin
        `uvm_error("COMMON_DATA", "flushSb state is not idle at end_test_check")
    end
    if (ptw_wait_replay_q.size() != 0) begin
        `uvm_error("COMMON_DATA", "ptw_wait_replay queue is not empty at end_test_check")
    end
endfunction:end_test_check
```

中文伪代码：

```text
end_test_check：
  关闭 dispatch monitor capture。
  如果 raw monitor queue 非空：
    报 uvm_error。
    清空 raw monitor queue。
  如果 main_trans_num 为 0：
    return。
  如果 next_uid 不等于 main_trans_num：
    报 uid allocation mismatch。
  遍历所有 uid：
    如果 status_by_uid[uid] 为空：
      报 uvm_fatal。
    如果该 uid 仍 active，或者 exception/replay/redirect pending 未清：
      报 uvm_error。
  如果 active ROB/LQ/SQ map 非空：
    报 uvm_error。
  如果 load/STA/STD issue queue 非空：
    报 uvm_error。
  如果 flush_in_progress、active_redirect 或 issue_freeze_ack 未清：
    报 uvm_error。
  如果 redirect drive queue 非空，或者 redirect phase 不是 IDLE：
    报 uvm_error。
  如果 flushSb pending/waiting/scheduled 未清：
    报 uvm_error。
  如果 ptw_wait_replay_q 非空：
    报 uvm_error。
```

关键结论：`all_transactions_success()` 的职责是“高频 stop 请求”，不是“最终一致性验收”。最终验收仍在 `end_test_check()`，因此不能把本轮修改理解为完全不检查队列、map 或 flush 状态。

### 2.6 新增对象说明

| 对象 | 输入 | 输出/副作用 | 调用者 | 内部关键 helper | Flow 职责 |
|---|---|---|---|---|---|
| `global_stop_requested` | 顶层完成判断结果 | 保存统一停止请求 | 顶层写，子 sequence 读 | 无 | 主动 sequence 生命周期真源。 |
| `transaction_done()` | `success_prefix_uid`、`main_trans_num`、状态表 success | 返回是否全部完成 | `request_global_stop_if_done()` | `advance_success_prefix()` | 把 success 前缀推进到最新完成边界。 |
| `request_global_stop_if_done()` | `transaction_done()` 结果 | 全部完成时置 `global_stop_requested=1` | `all_transactions_success()` | `transaction_done()` | 顶层 orchestration 的 stop 请求入口。 |
| `is_global_stop_requested()` | `global_stop_requested` | 返回 stop 标志 | 三类主动 sequence | 无 | 子 sequence 退出检查入口。 |

## 3. 特性二：三类 active sequence 生命周期

### 3.1 LSQENQ 生命周期

#### 修改前逻辑

LSQENQ 使用 `idle_stop` 作为连续空闲退出条件，等待主表使用 `start_timeout` fatal。`enable=0` 时调用 `super.body()`，可能回退到父类 default sequence。默认参数中 `MEMBLOCK_LSQENQ_SEQ_EN=1`，对不生成 dispatch 主表的 testcase 有误启用风险。

#### 修改后逻辑

LSQENQ 默认保持打开；运行后等待 `main_table_ready`，等待期间只按统一阈值 warning。主循环 `forever` 运行，只有 `data.is_global_stop_requested()` 为 1 才正常退出。无 LSQ 候选时仍发送一个 idle xaction，保持 sequencer/driver 调度节拍。

#### 行为概述

```text
LSQENQ body：
  调用 seq_csr_common::init()，保证 plus 配置已解析。
  调用 configure_from_plus()：
    读取 MEMBLOCK_LSQENQ_SEQ_EN。
    读取 MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES。
    读取 MEMBLOCK_LSQENQ_READY_TIMEOUT。
  如果 enable 为 0：
    打印“LSQ enqueue dispatch sequence stays idle”。
    直接 return，不调用 super.body()。
  调用 ensure_helpers()：
    获取 common_data_transaction 单例。
    获取 lsq_ctrl_model 单例。
    创建或复用 issue_queue_scheduler。
    创建或复用 dispatch_monitor_event_adapter。
    如果任一关键对象为空，则 uvm_fatal。
  调用 wait_for_main_table()：
    只要 data.main_table_ready 还是 0，就每 #1 等待一次。
    如果等待计数达到 no_progress_warn_cycles 的整数倍：
      打印 uvm_warning，但继续等待，不 fatal，不退出。
  调用 drive_lsqenq_loop()。

drive_lsqenq_loop 主循环：
  初始化 idle_count=0、cycle_idx=0。
  forever 循环：
    如果 data.is_global_stop_requested() 为 1：
      打印 stop 信息。
      break 退出 LSQENQ 主循环。
    调用 send_lsqenq_cycle(cycle_idx, has_progress) 执行一拍 admission。
    cycle_idx 自增。
    如果 has_progress 为 1：
      idle_count 清零。
    否则：
      idle_count 自增。
      如果 no_progress_warn_cycles 非 0 且 idle_count 达到阈值：
        打印 no LSQ enqueue progress 的 uvm_warning。
        将 idle_count 清零，继续下一轮，不退出。

send_lsqenq_cycle 单拍逻辑：
  先将 has_progress 置 0。
  调用 apply_pending_lsq_cancels()：
    如果 pending_lq_cancel_count 非 0，则回退 lsq_ctrl 的 LQ 镜像并清零计数。
    如果 pending_sq_cancel_count 非 0，则回退 lsq_ctrl 的 SQ 镜像并清零计数。
  调用 admit_non_lsq_if_ready(has_progress)：
    如果下一条 uid 是不需要 LSQ entry 的 AMO/LR/SC 等路径并且可以 admission：
      完成 non-LSQ admission，has_progress 可能置 1，然后 return。
  调用 collect_lsq_candidates() 收集本拍可入队的 LSQ candidate。
  如果没有 LSQ candidate：
    创建 idle lsqenq xaction。
    clear_lsqenq_xaction() 清空 payload。
    设置 memblock_dispatch_wait_can_accept=0，表示 driver 不等待 DUT canAccept。
    设置 ready_timeout、aborted_by_redirect、flush_epoch、gap 字段。
    start_item/finish_item 发送 idle item。
    return。
  如果存在 LSQ candidate：
    创建真实 lsqenq xaction。
    clear_lsqenq_xaction() 清空默认字段。
    设置 memblock_dispatch_wait_can_accept=1，表示 driver 需要等待 DUT canAccept。
    记录 ready_timeout、aborted_by_redirect、flush_epoch。
    对每个 candidate 调用 assign_lsqenq_slot() 填入对应 slot。
    start_item/finish_item 交给 driver。
    调用 confirm_lsq_candidates()：
      根据 driver/DUT 接收结果确认 admission。
      更新公共主表、状态表和 LSQ 映射。
      设置 has_progress。
```

#### 正确性检查

- `default.cfg` 保持 `MEMBLOCK_LSQENQ_SEQ_EN=1`，不破坏依赖默认 LSQENQ 开启的旧 testcase；不需要 LSQENQ 的 testcase 应显式配置 `+MEMBLOCK_LSQENQ_SEQ_EN=0`。
- `enable=0` 直接返回，不会调用父类随机 default sequence。
- redirect 后的 LSQ cancel 仍在每轮 `send_lsqenq_cycle()` 开头处理。
- 无候选时 idle item 不设置 `memblock_dispatch_wait_can_accept`，不会误等待 DUT `canAccept`。
- 正常退出只由 global stop 控制，避免 idle stop 在长 backpressure 或等待 replay/redirect 恢复时提前退出。

#### 源码支撑片段

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`，函数：`new()`。
函数功能描述：LSQENQ 构造函数提供本地初始值；最终 enable 由 `MEMBLOCK_LSQENQ_SEQ_EN` 控制，当前 plus/default.cfg 默认保持打开。

```systemverilog
function memblock_lsqenq_dispatch_sequence::new(string name = "memblock_lsqenq_dispatch_sequence");
    super.new(name);
    enable = 1'b0;
    no_progress_warn_cycles = 10000;
    ready_timeout = 1000;
endfunction:new
```

中文伪代码：

```text
创建 LSQENQ sequence 时：
  先调用父类构造函数。
  构造函数本地初始值不作为最终配置；后续 `configure_from_plus()` 会读取 `MEMBLOCK_LSQENQ_SEQ_EN`，当前默认配置为打开。
  设置无进展 warning 周期默认值。
  设置等待 DUT canAccept 的 ready timeout 默认值。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`，函数：`body()`。
函数功能描述：`body()` 读取 plus 配置；当 `enable=0` 时直接 idle 返回，避免调用父类 default sequence；打开后才绑定 helper、等待主表并进入 LSQENQ 主循环。

```systemverilog
task memblock_lsqenq_dispatch_sequence::body();
    seq_csr_common::init();
    configure_from_plus();
    if (!enable) begin
        `uvm_info(get_type_name(), "MEMBLOCK_LSQENQ_SEQ_EN=0, LSQ enqueue dispatch sequence stays idle", UVM_LOW)
        return;
    end
    ensure_helpers();
    wait_for_main_table();
    drive_lsqenq_loop();
endtask:body
```

中文伪代码：

```text
LSQENQ body 启动时：
  初始化 seq_csr_common 并读取 plus 配置。
  如果 LSQENQ sequence 被关闭：
    打印 idle 信息。
    直接返回，不调用父类 body。
  如果已打开：
    绑定公共 data、LSQ helper 等依赖。
    等待主表生成完成。
    进入 LSQENQ 主动 admission 循环。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`，函数：`drive_lsqenq_loop()`。
函数功能描述：LSQENQ 主循环每轮先检查 global stop，未停止时执行一拍 admission，并用 `idle_count` 只做 no-progress warning。

```systemverilog
forever begin
    bit has_progress;

    if (data.is_global_stop_requested()) begin
        `uvm_info(get_type_name(),
                  $sformatf("stop LSQ enqueue loop by global_stop_requested at cycle=%0d",
                            cycle_idx),
                  UVM_LOW)
        break;
    end
    send_lsqenq_cycle(cycle_idx, has_progress);
    cycle_idx++;
    if (has_progress) begin
        idle_count = 0;
    end else begin
        idle_count++;
        if (no_progress_warn_cycles != 0 &&
            idle_count >= no_progress_warn_cycles) begin
            `uvm_warning(get_type_name(), "no LSQ enqueue progress")
            idle_count = 0;
        end
    end
end
```

中文伪代码：

```text
LSQENQ 主循环每一拍：
  如果公共 data 已请求 global stop：
    打印停止信息。
    退出 LSQENQ 主循环。
  否则执行一拍 send_lsqenq_cycle。
  如果这一拍有 admission 进展：
    idle_count 清零。
  如果这一拍没有进展：
    idle_count 加一。
    达到 no-progress warning 阈值时只打印 warning。
    warning 后继续循环，不把 idle 当作正常退出条件。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`，函数：`send_lsqenq_cycle()`。
函数功能描述：当本拍没有 LSQ candidate 时，发送 idle xaction 保持 driver 节拍，但不等待 DUT `canAccept`。

```systemverilog
if (!collect_lsq_candidates(uids, trs, behaviors, lq_keys, sq_keys)) begin
    tr = lsqenq_agent_agent_xaction::type_id::create($sformatf("lsqenq_dispatch_idle_tr_%0d", cycle_idx));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create idle lsqenq xaction")
    end
    clear_lsqenq_xaction(tr);
    tr.memblock_dispatch_wait_can_accept = 1'b0;
    tr.memblock_dispatch_ready_timeout = ready_timeout;
    tr.memblock_dispatch_aborted_by_redirect = 1'b0;
    tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    start_item(tr);
    finish_item(tr);
    return;
end
```

中文伪代码：

```text
LSQENQ 本拍没有可 admission candidate 时：
  创建一个 idle xaction。
  清空 xaction 默认字段。
  设置 wait_can_accept=0，表示 idle item 不等待 DUT canAccept。
  记录 ready_timeout、redirect abort 标志和当前 flush epoch。
  发送 idle xaction 给 driver。
  直接返回，不更新 LSQ 映射或 admission 状态。
```

关键分支说明：`body()` 的 disabled 分支只打印并返回；主循环先检查 global stop 再发下一拍；无候选分支发送 idle item，用于保持 sequence 驱动节奏但不等待 `canAccept`。

### 3.2 LINTSISSUE 生命周期

#### 修改前逻辑

LINTSISSUE 有 `max_cycles`、`idle_stop`、`start_timeout`。这些参数可能让 issue sequence 在队列暂时为空、主表尚未 ready 或长 testcase 中提前结束。

#### 修改后逻辑

LINTSISSUE `forever` 运行，每轮执行 `route_all_ready_uids()`、`send_issue_cycle()`、`advance_issue_queue_delays()`，然后检查 global stop。没有 fire 时只按 `no_progress_warn_cycles` warning。

#### 行为概述

```text
LINTSISSUE body：
  调用 seq_csr_common::init()。
  调用 configure_from_plus()：
    读取 MEMBLOCK_DISPATCH_ISSUE_SEQ_EN。
    读取 MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES。
  如果 enable 为 0：
    打印“lintsissue dispatch sequence stays idle”。
    直接 return，不调用 super.body()。
  调用 ensure_helpers()：
    获取 common_data_transaction。
    创建或复用 issue_queue_scheduler。
    创建或复用 issue_field_assigner。
    创建或复用 writeback_status_handler。
    如果对象为空则 uvm_fatal。
  调用 wait_for_main_table()：
    等待 data.main_table_ready。
    每达到 no_progress_warn_cycles 的整数倍打印 warning。
    不因为等待超时退出或 fatal。
  调用 drive_dispatch_issue_loop()。

drive_dispatch_issue_loop 主循环：
  初始化 cycle_idx=0、idle_count=0。
  forever 循环：
    调用 issue_sched.route_all_ready_uids()：
      把已完成 admission 且 issue_ready 的 uid route 到 load/STA/STD issue queue。
    调用 send_issue_cycle(cycle_idx, has_fire)：
      尝试从 issue queue 选择候选并驱动 intIssue xaction。
    调用 issue_sched.advance_issue_queue_delays()：
      推进 issue queue 中 item 的延迟计数。
    如果 data.is_global_stop_requested() 为 1：
      打印 stop 信息。
      break 退出 issue 主循环。
    如果 has_fire 为 1：
      idle_count 清零。
    否则：
      idle_count 自增。
      如果 no_progress_warn_cycles 非 0 且 idle_count 达到阈值：
        打印 no issue fire warning，带 success_prefix、main_trans_num 和三个 issue queue size。
        idle_count 清零，继续循环。

send_issue_cycle 单拍逻辑：
  创建 lintsissue xaction；创建失败则 uvm_fatal。
  调用 field_assigner.clear_lintsissue_xaction() 清空 item。
  设置 memblock_dispatch_wait_ready=1，让 driver 在 send_pkt 后等待 DUT ready。
  设置 memblock_dispatch_ready_timeout，防止 DUT 长期不 ready。
  将 memblock_dispatch_aborted_by_redirect 清 0，等待 driver 回填。
  记录当前 dispatch_flush_epoch。
  将 memblock_dispatch_fired_mask 清 0，等待 driver 回填真实 fire mask。
  如果当前没有被 global flush 阻塞：
    调用 issue_sched.select_issue_candidates() 从 load/STA/STD queue 选候选。
    再次检查 global flush：
      如果仍未被 flush 阻塞：
        分别调用 assign_issue_items() 填 load、STA、STD port。
        把待标记 fired 的 item 记录到 fired_items。
  start_item/finish_item 交给 driver。
  如果 driver 回填 memblock_dispatch_aborted_by_redirect 为 1：
    如果 fired_mask 非 0：
      只对 fired_mask 标记的 port 调用 mark_fired_items()。
      设置 has_fire=1。
    如果 fired_items 非空：
      打印 partial issue fire marking 信息。
    return。
  如果当前已经进入 global flush，或者 xaction 记录的 flush_epoch 已经过期：
    如果 fired_items 非空，打印 skip issue fire marking 信息。
    return，不标记 issue fired。
  如果 fired_items 非空且没有 flush/redirect 干扰：
    调用 mark_fired_items(fired_items, 7'h7f) 标记本拍被驱动的候选。
    设置 has_fire=1。
```

#### 正确性检查

- `route_all_ready_uids()` 在 stop 检查前执行一次，允许本轮 service 已生成的 ready uid 被处理。
- ready 等待仍受 `MEMBLOCK_DISPATCH_READY_TIMEOUT` 保护，删除 max_cycles 不等于无限等 ready。
- redirect/flush 期间通过 `memblock_dispatch_aborted_by_redirect`、`flush_epoch`、`issue_blocked_by_global_flush()` 保护 fire 标记，避免跨 flush 误标记。
- 无进展只 warning，避免 queue 暂空时退出。

#### 源码支撑片段

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`，函数：`drive_dispatch_issue_loop()`。
函数功能描述：LINTSISSUE 主循环负责 route ready uid、发送一拍 issue xaction、推进 queue delay，并在 global stop 后退出。

```systemverilog
task memblock_lintsissue_dispatch_sequence::drive_dispatch_issue_loop();
    int unsigned cycle_idx;
    int unsigned idle_count;

    cycle_idx = 0;
    idle_count = 0;
    forever begin
        bit has_fire;

        issue_sched.route_all_ready_uids();
        send_issue_cycle(cycle_idx, has_fire);
        issue_sched.advance_issue_queue_delays();

        if (data.is_global_stop_requested()) begin
            `uvm_info(get_type_name(),
                      $sformatf("stop dispatch issue loop by global_stop_requested at cycle=%0d",
                                cycle_idx),
                      UVM_LOW)
            break;
        end
        if (has_fire) begin
            idle_count = 0;
        end else begin
            idle_count++;
        end
        cycle_idx++;
    end
endtask
```

中文伪代码：

```text
LINTSISSUE 主循环每一拍：
  先把已经 ready 的 uid route 到 LOAD/STA/STD issue queue。
  再发送一拍 issue xaction。
  推进 issue queue delay。
  如果公共 data 已请求 global stop：
    打印停止信息。
    退出 issue 主循环。
  如果本拍有真实 fire：
    idle_count 清零。
  如果本拍没有 fire：
    idle_count 加一，仅用于 debug 统计。
  cycle_idx 加一进入下一拍。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`，函数：`send_issue_cycle()`。
函数功能描述：这些 `memblock_dispatch_*` 字段是 sequence/driver 协作字段，用于等待 ready、识别 redirect/flush 边界和记录真实 fired port。

```systemverilog
tr.memblock_dispatch_wait_ready = 1'b1;
tr.memblock_dispatch_ready_timeout = seq_csr_common::get_dispatch_ready_timeout();
tr.memblock_dispatch_aborted_by_redirect = 1'b0;
tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
tr.memblock_dispatch_fired_mask = '0;
```

中文伪代码：

```text
构造真实 issue xaction 时：
  要求 driver 等待 DUT ready。
  设置 ready timeout，避免 ready 永久等待。
  清空 redirect abort 标志。
  记录当前 flush epoch，用于后续判断是否跨过 flush 边界。
  清空 fired mask，等待 driver 回填本拍真实 fire 的 port。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`，函数：`send_issue_cycle()`。
函数功能描述：redirect abort 后只按 driver 回填的 fired mask 标记已经被 DUT 接收的 port，避免把未 fire 的候选误标为已发射。

```systemverilog
if (tr.memblock_dispatch_aborted_by_redirect) begin
    if (tr.memblock_dispatch_fired_mask != '0) begin
        mark_fired_items(fired_items, tr.memblock_dispatch_fired_mask);
        has_fire = 1'b1;
    end
    if (fired_items.size() != 0) begin
        `uvm_info(get_type_name(), "partial issue fire marking after redirect abort", UVM_LOW)
    end
    return;
end
```

中文伪代码：

```text
如果 issue xaction 被 redirect abort：
  检查 driver 回填的 fired mask。
  如果 fired mask 非空：
    只把 mask 标出的 port 标记为已经 fire。
    设置 has_fire=1。
  如果本拍存在待标记 item：
    打印 partial fire 信息，说明 redirect 边界只确认了部分 port。
  直接返回，不再按普通路径标记全部候选。
```

关键状态说明：`memblock_dispatch_fired_mask` 是 driver 回填的真实 fire mask，不是 DUT payload；它让 redirect 边界拍 partial fire 只标记已经被 DUT ready 接收的 port。

### 3.3 LSQCOMMIT 生命周期

#### 修改前逻辑

LSQCOMMIT 有 `max_cycles`、`idle_stop`、`start_timeout`，并通过 `has_lsqcommit_activity()` 全表扫描判断是否出现运行时活动。该扫描在长 testcase 下成本高，而且“未看到 activity”不应成为正常退出依据。

#### 修改后逻辑

LSQCOMMIT 删除 activity 扫描，主循环常驻运行。若 global stop 已请求且没有 scheduled/pending flushSb 或 waiting empty，则退出；否则继续 drain flushSb 或 commit。等待主表从 `#1` 改为 `@(posedge lsqcommit_vif.clk)`，使等待节拍和 agent clock 对齐。

#### 行为概述

```text
LSQCOMMIT body：
  调用 seq_csr_common::init()。
  调用 configure_from_plus()：
    读取 MEMBLOCK_LSQCOMMIT_SEQ_EN。
    读取 MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES。
    读取 MEMBLOCK_FLUSHSB_REQUEST_CYCLE。
  如果 enable 为 0：
    打印“LSQ commit dispatch sequence stays idle”。
    直接 return，不调用 super.body()。
  调用 ensure_helpers()：
    获取 common_data_transaction。
    创建或复用 lsq_commit_handler。
    将 commit_handler 绑定到 lsq_ctrl_model。
    获取 lsqcommit_vif。
    调用 data.arm_scheduled_flushsb(flushsb_request_cycle) 记录计划 flushSb。
  调用 wait_for_main_table()：
    等待 data.main_table_ready。
    每达到 no_progress_warn_cycles 的整数倍打印 warning。
    每次等待使用 wait_clock_tick()，也就是 @(posedge lsqcommit_vif.clk)。
  调用 drive_lsqcommit_loop()。

drive_lsqcommit_loop 主循环：
  初始化 cycle_idx=0、idle_count=0。
  forever 循环：
    如果 data.is_global_stop_requested() 为 1，并且 flushsb_request_pending(cycle_idx) 为 0：
      说明所有 transaction 已完成且没有 scheduled/pending/waiting flushSb。
      打印 stop 信息。
      break 退出 LSQCOMMIT 主循环。
    调用 send_lsqcommit_cycle(cycle_idx, has_commit)。
    cycle_idx 自增。
    如果 has_commit 为 1：
      idle_count 清零。
    否则：
      idle_count 自增。
      如果 no_progress_warn_cycles 非 0 且 idle_count 达到阈值：
        打印 no LSQ commit/flushSb progress warning，带 success_prefix、main_trans_num 和 flushSb 状态。
        idle_count 清零，继续循环。

send_lsqcommit_cycle 单拍逻辑：
  将 has_commit 置 0。
  调用 data.request_scheduled_flushsb_if_due(cycle_idx)：
    如果已到计划 flushSb cycle，则设置 flushSb pending。
  如果 data.issue_blocked_by_global_flush() 为 1：
    创建 idle lsqcommit xaction。
    调用 commit_handler.clear_lsqcommit_xaction() 清空字段。
    start_item/finish_item 发送 idle item。
    return，不做 commit。
  调用 drive_flushsb_if_needed(cycle_idx, did_flushsb_drive)：
    先检查 flushSb 是否 timeout，timeout 则 uvm_fatal。
    如果当前不应该 drive flushSb，则 return。
    如果应该 drive，则创建 flushSb xaction，设置 io_ooo_to_mem_flushSb=1。
    发送后调用 data.mark_flushsb_driven() 记录已驱动，并设置 did_drive=1。
  如果 did_flushsb_drive 为 1，或者 data.flushsb_waiting_empty 为 1：
    创建 flushsb_wait idle xaction，保持 driver 节拍。
    设置 has_commit=1，把这类 flushSb 推进也算作本 loop 的进展。
    return。
  调用 commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit)：
    由 commit handler 选择可 commit 的 uid，并填充 pendingPtr 类 xaction。
  设置 xaction 名字。
  start_item/finish_item 发送 commit xaction。
  如果 has_commit 为 1：
    调用 commit_handler.mark_rob_commit_batch(commit_uids) 标记本批 uid ROB commit。

flushsb_request_pending 判断：
  如果 scheduled_flushsb_pending(cycle_idx) 为 1，返回 true。
  如果 flushsb_pending 为 1，返回 true。
  如果 flushsb_waiting_empty 为 1，返回 true。
  否则返回 false。
```

#### 正确性检查

- global stop 后仍允许 flushSb drain，避免 stop 请求截断已经计划或已经发出的 flushSb。
- `send_lsqcommit_cycle()` 保留 `issue_blocked_by_global_flush()` idle item，redirect/flush 期间不推进 commit。
- `drive_flushsb_if_needed()` 仍执行 `flushsb_timed_out()` fatal，删除 max_cycles 不影响 flushSb timeout 兜底。
- `ensure_lsqcommit_vif()` 找不到 vif 会 fatal，避免 clock wait 静默挂住。

#### 源码支撑片段

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`，函数：`drive_lsqcommit_loop()`。
函数功能描述：LSQCOMMIT 只有在 global stop 已请求且没有 flushSb pending/waiting/scheduled 时才退出，保证 stop 后仍能 drain flushSb。

```systemverilog
if (data.is_global_stop_requested() &&
    !flushsb_request_pending(cycle_idx)) begin
    `uvm_info(get_type_name(),
              $sformatf("stop LSQ commit loop by global_stop_requested at cycle=%0d",
                        cycle_idx),
              UVM_LOW)
    break;
end
```

中文伪代码：

```text
LSQCOMMIT 主循环准备退出时：
  只有同时满足两个条件才退出：
    公共 data 已经请求 global stop。
    当前没有 scheduled flushSb、pending flushSb 或 waiting-empty flushSb。
  满足后打印停止信息并 break。
  如果还有 flushSb 相关工作，即使 global stop 已请求也继续 drain。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`，函数：`wait_clock_tick()`。
函数功能描述：LSQCOMMIT 等待主表时改用 agent clock，避免使用无时钟语义的 `#1` 延时。

```systemverilog
task memblock_lsqcommit_dispatch_sequence::wait_clock_tick();
    ensure_lsqcommit_vif();
    @(posedge lsqcommit_vif.clk);
endtask:wait_clock_tick
```

中文伪代码：

```text
LSQCOMMIT 等待一个时钟 tick 时：
  先确保 lsqcommit_vif 已经获取。
  然后等待 lsqcommit_vif.clk 的上升沿。
  该等待和 agent clock 对齐，不使用 #1 作为轮询节拍。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`，函数：`ensure_lsqcommit_vif()`。
函数功能描述：该函数通过 `uvm_config_db` 获取 LSQCOMMIT agent 的 virtual interface，获取失败时 fatal，避免后续 clock wait 静默挂住。

```systemverilog
function void memblock_lsqcommit_dispatch_sequence::ensure_lsqcommit_vif();
    if (lsqcommit_vif != null) begin
        return;
    end
    if (!uvm_config_db#(virtual lsqcommit_agent_agent_interface)::get(null, get_full_name(), "vif", lsqcommit_vif) &&
        !uvm_config_db#(virtual lsqcommit_agent_agent_interface)::get(null, "uvm_test_top.env.u_lsqcommit_agent_agent*", "vif", lsqcommit_vif)) begin
        `uvm_fatal(get_type_name(), "LSQ commit virtual interface is not set")
    end
endfunction
```

中文伪代码：

```text
确保 LSQCOMMIT vif 可用时：
  如果本地已经缓存 vif：
    直接返回。
  否则从当前 sequence 路径查询 uvm_config_db。
  如果当前路径查不到，再用 env 下 LSQCOMMIT agent 的通配路径查询。
  两次都失败时 fatal，说明 sequence 无法等待真实 agent clock。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`，函数：`flushsb_request_pending()`。
函数功能描述：该 helper 汇总 scheduled flushSb、已 pending flushSb 和 waiting empty 三类状态，用于决定 global stop 后是否还要继续 LSQCOMMIT loop。

```systemverilog
function bit memblock_lsqcommit_dispatch_sequence::flushsb_request_pending(input int unsigned cycle_idx);
    return data.scheduled_flushsb_pending(cycle_idx) ||
           data.flushsb_pending ||
           data.flushsb_waiting_empty;
endfunction:flushsb_request_pending
```

中文伪代码：

```text
判断 flushSb 是否仍有待处理工作：
  如果当前 cycle 已经到达 scheduled flushSb 触发点，返回 true。
  如果已经存在 pending flushSb，返回 true。
  如果 flushSb 已发出但还在等待队列清空，返回 true。
  三类状态都不存在时返回 false。
```

### 3.4 三类 sequence 调用关系

| 调用顺序 | LSQENQ | LINTSISSUE | LSQCOMMIT |
|---|---|---|---|
| 1 | `body()` 初始化 cfg，disabled 直接 return | `body()` 初始化 cfg，disabled 直接 return | `body()` 初始化 cfg，disabled 直接 return |
| 2 | `ensure_helpers()` 绑定 data/lsq/issue/monitor helper | `ensure_helpers()` 绑定 data/scheduler/assigner/wb handler | `ensure_helpers()` 绑定 data/commit handler/vif 并 arm flushSb |
| 3 | `wait_for_main_table()` 周期 warning | `wait_for_main_table()` 周期 warning | `wait_for_main_table()` 按 vif clock 周期 warning |
| 4 | `drive_lsqenq_loop()` global stop 前常驻 admission | `drive_dispatch_issue_loop()` global stop 前常驻 route/issue | `drive_lsqcommit_loop()` global stop 后 drain flushSb 再退出 |
| 5 | `send_lsqenq_cycle()` cancel、non-LSQ admission、LSQ candidate 或 idle item | `send_issue_cycle()` select/assign/wait ready/mark fire | `send_lsqcommit_cycle()` scheduled flushSb、commit item、mark commit |

## 4. 特性三：参数删除与统一 debug 参数

### 4.1 修改前逻辑

旧参数包括：

| 旧对象 | 旧行为 | 问题 |
|---|---|---|
| `MEMBLOCK_DISPATCH_ISSUE_MAX_CYCLES` | 限制 issue sequence 最多轮数。 | 长 testcase 可能未完成就退出。 |
| `MEMBLOCK_DISPATCH_ISSUE_IDLE_STOP` | 连续无 issue fire 后退出。 | 队列暂空、redirect/replay 等待期间可能误停。 |
| `MEMBLOCK_DISPATCH_ISSUE_START_TIMEOUT` | 等主表超时 fatal。 | agent 启动早于主控 vseq 时误 fatal。 |
| `MEMBLOCK_LSQENQ_IDLE_STOP` | LSQENQ 连续 idle 后退出。 | admission 暂无候选时误停。 |
| `MEMBLOCK_LSQENQ_START_TIMEOUT` | 等主表超时 fatal。 | 普通 testcase 或启动顺序差异下误 fatal。 |
| `MEMBLOCK_LSQCOMMIT_MAX_CYCLES` | 限制 commit 轮数。 | 长 testcase 或 flushSb drain 被截断。 |
| `MEMBLOCK_LSQCOMMIT_IDLE_STOP` | commit 空闲后退出。 | commit candidate 暂未出现不代表流程结束。 |
| `MEMBLOCK_LSQCOMMIT_START_TIMEOUT` | 等 activity 或主表超时。 | 依赖全表 activity 扫描，语义不是完成条件。 |

### 4.2 修改后逻辑

新增 `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES`，统一提供 LSQENQ、LINTSISSUE、LSQCOMMIT 的无进展 warning 阈值。它只打印 warning 并继续运行，不参与正常退出。旧参数从 `plus.sv`、`seq_csr_common.sv` getter/clamp 和 cfg 中删除，不保留兼容。

`mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` 也同步更新了参数语义，不只是文件清单变化。该规则文档现在明确：

- 主动主流程 sequence 的统一 debug 参数是 `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES`，达到阈值只 `uvm_warning` 并继续运行。
- `MEMBLOCK_DISPATCH_ISSUE_MAX_CYCLES`、`MEMBLOCK_DISPATCH_ISSUE_IDLE_STOP`、`MEMBLOCK_DISPATCH_ISSUE_START_TIMEOUT`、`MEMBLOCK_LSQENQ_IDLE_STOP`、`MEMBLOCK_LSQENQ_START_TIMEOUT`、`MEMBLOCK_LSQCOMMIT_MAX_CYCLES`、`MEMBLOCK_LSQCOMMIT_IDLE_STOP`、`MEMBLOCK_LSQCOMMIT_START_TIMEOUT` 不再作为生命周期参数。
- `MEMBLOCK_LSQENQ_SEQ_EN` 默认必须为 1；需要关闭 LSQENQ 的 testcase 必须通过 cfg 显式设置为 0。
- LSQENQ/LINTSISSUE/LSQCOMMIT 的正常退出由 global stop 控制；LSQCOMMIT 在 global stop 后仍 drain scheduled/pending flushSb 或 waiting empty。

### 4.3 行为概述

```text
plus.sv 参数定义逻辑：
  保留 MEMBLOCK_DISPATCH_ISSUE_SEQ_EN，用来打开或关闭 lintsissue dispatch sequence。
  保留 MEMBLOCK_DISPATCH_READY_TIMEOUT，用来限制 intIssue driver 等 ready 的周期。
  保留 MEMBLOCK_LSQENQ_SEQ_EN，用来打开或关闭 LSQ enqueue sequence。
  保留 MEMBLOCK_LSQENQ_READY_TIMEOUT，用来限制 LSQENQ driver 等 canAccept 的周期。
  保留 MEMBLOCK_LSQCOMMIT_SEQ_EN，用来打开或关闭 LSQ commit sequence。
  新增 MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES，默认 10000。
  删除 DISPATCH_ISSUE/LSQENQ/LSQCOMMIT 各自的 max_cycles、idle_stop、start_timeout 参数。

seq_csr_common 初始化逻辑：
  从 plus 读取 MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES。
  使用 get_non_negative_int() 保证该值非负。
  从 plus 读取三类 sequence enable。
  从 plus 读取仍保留的 ready_timeout 和 flushSb 参数。
  不再读取旧 max_cycles、idle_stop、start_timeout。
  不再对旧参数做 clamp。
  提供 get_active_seq_no_progress_warn_cycles()：
    调用前检查 seq_csr_common 已 init。
    返回 active_seq_no_progress_warn_cycles。

active sequence 使用统一 warning 参数：
  每个主动 sequence configure_from_plus() 都读取同一个 get_active_seq_no_progress_warn_cycles()。
  主循环中如果本拍有进展：
    idle_count 清零。
  如果本拍没有进展：
    idle_count 自增。
    如果阈值非 0 且 idle_count 达到阈值：
      打印 uvm_warning。
      idle_count 清零。
      继续循环，不退出。
```

### 4.4 正确性检查

- 参数语义单一：退出看 global stop，卡住调试看 warning 和 testcase/UVM timeout。
- 不保留旧参数，避免用户以为 old idle/max_cycles 还能控制生命周期。
- `default.cfg` 保持 `MEMBLOCK_LSQENQ_SEQ_EN=1` 和 `MEMBLOCK_LSQCOMMIT_SEQ_EN=1` 默认打开；dispatch real cfg 仍可显式打开三类 sequence 并设置 warning 阈值 60000。
- ready timeout 未删除：`MEMBLOCK_DISPATCH_READY_TIMEOUT`、`MEMBLOCK_LSQENQ_READY_TIMEOUT` 仍用于防止 driver 等 ready/canAccept 永久阻塞。
- `plus_demo_migration_plan.md` 已同步规则说明，避免后续新增 cfg 或 testcase 时继续使用被删除的 idle/start/max_cycles 参数。

### 4.5 源码支撑片段

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`，参数定义：`MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES` 及 active sequence enable/timeout 参数。
函数功能描述：`plus.sv` 定义可由 cfg/plusarg 覆盖的公共测试框架参数；本轮删除旧生命周期参数，只保留 enable、ready timeout 和统一 no-progress warning 阈值。

```systemverilog
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES, int, 10000)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_DISPATCH_ISSUE_SEQ_EN, bit, 1'b0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_DISPATCH_READY_TIMEOUT, int, 1000)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LSQENQ_SEQ_EN, bit, 1'b1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LSQENQ_READY_TIMEOUT, int, 1000)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LSQCOMMIT_SEQ_EN, bit, 1'b1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_FLUSHSB_SEQ_EN, bit, 1'b0)
```

中文伪代码：

```text
plus.sv 参数定义阶段：
  保留 active sequence 的统一 no-progress warning 参数。
  保留三类主动 sequence 的 enable 开关。
  保留 issue/LSQENQ 的 ready timeout。
  保留 flushSb sequence enable。
  不再定义旧的 max_cycles、idle_stop、start_timeout 生命周期参数。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`，字段/函数：`active_seq_no_progress_warn_cycles` / `get_active_seq_no_progress_warn_cycles()`。
函数功能描述：`seq_csr_common` 负责读取 plus 参数并提供 getter；三类 active sequence 统一通过该 getter 获取 no-progress warning 阈值。

```systemverilog
static int unsigned active_seq_no_progress_warn_cycles = 10000;
static bit          dispatch_issue_seq_en = 1'b0;
static int unsigned dispatch_ready_timeout = 1000;
static bit          lsqenq_seq_en = 1'b1;
static int unsigned lsqenq_ready_timeout = 1000;
static bit          lsqcommit_seq_en = 1'b1;

active_seq_no_progress_warn_cycles = get_non_negative_int("MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES", plus::MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES);
dispatch_issue_seq_en       = plus::MEMBLOCK_DISPATCH_ISSUE_SEQ_EN;
dispatch_ready_timeout      = get_non_negative_int("MEMBLOCK_DISPATCH_READY_TIMEOUT", plus::MEMBLOCK_DISPATCH_READY_TIMEOUT);
lsqenq_seq_en               = plus::MEMBLOCK_LSQENQ_SEQ_EN;
lsqenq_ready_timeout        = get_non_negative_int("MEMBLOCK_LSQENQ_READY_TIMEOUT", plus::MEMBLOCK_LSQENQ_READY_TIMEOUT);
lsqcommit_seq_en            = plus::MEMBLOCK_LSQCOMMIT_SEQ_EN;

static function int unsigned get_active_seq_no_progress_warn_cycles();
    check_initialized("get_active_seq_no_progress_warn_cycles");
    return active_seq_no_progress_warn_cycles;
endfunction
```

中文伪代码：

```text
seq_csr_common 初始化 active sequence 参数时：
  设置统一 no-progress warning 默认值。
  设置 active sequence 默认值：LSQENQ 和 LSQCOMMIT 默认打开，dispatch issue 默认关闭。
  从 plus/cfg 读取 no-progress warning、enable 和 ready timeout。
  getter 被调用时先检查 seq_csr_common 已初始化。
  返回统一 no-progress warning 阈值给 LSQENQ/LINTSISSUE/LSQCOMMIT 使用。
```

```text
+MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES=10000
+MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=0
+MEMBLOCK_LSQENQ_SEQ_EN=1
+MEMBLOCK_LSQCOMMIT_SEQ_EN=1
```

```text
+MEMBLOCK_LSQENQ_SEQ_EN=1
+MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1
+MEMBLOCK_LSQCOMMIT_SEQ_EN=1
+MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES=60000
```

这段文档片段说明规则文档已经同步新语义：

```text
MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES 是主动主流程 driver 的统一无进展
debug 阈值，覆盖 LSQ enqueue、lintsissue dispatch issue 和 LSQ commit sequence。
达到阈值只打印 uvm_warning 并继续运行，不作为正常退出条件。

LSQENQ 不再有专用 max_cycles、idle stop 或 start timeout；global stop 前常驻
admission，global stop 后退出。

LSQ commit 不再使用固定 max_cycles、pending-work 扫描、idle stop 或 start timeout
作为正常退出条件；global stop 前常驻运行。
```

## 5. 特性四：删除未接入主流程的 route work 判断 helper

### 5.1 修改前逻辑

修改前 issue routing 相关判断主要依赖 `route_uid()`、`route_target()` 和现有 issue queue 状态。曾新增过 `uid_has_pending_route_work()`，它想解决的问题是：若只用 `status.active` 或 `status.issue_ready` 判断“还有 route 工作”，会过宽。一个 uid 可能已经 active 且 issue_ready，但它的 load/STA/STD target 已经 queued、dispatched 或 done，此时 `route_uid()` 实际不会再入队。

但复查后确认：`uid_has_pending_route_work()` 没有接入 active sequence global stop 主路径，也没有被发射前检查调用。继续保留这个 helper 会带来两个问题：

- 维护者可能误以为发射合法性依赖该 helper，实际主路径并不会调用它。
- 后续若有人把它接入 no-progress 或停止条件，容易形成第二套 route pending 判断真源，和当前 global stop 统一退出设计相冲突。

### 5.2 修改后逻辑

删除 `issue_queue_scheduler.sv` 中未使用的 `uid_has_pending_route_work(uid)`。当前 route 和发射合法性由真实主路径分层完成：

- route 入队前：`route_uid()` 调用 `is_uid_route_ready()`，`route_target()` 调用 `target_already_queued_or_done()` 和 `data.replay_target_requested()`，防止重复入队和 replay target 错发。
- select 发射前：`select_issue_candidates()` / `select_target_candidates()` 调用 `is_issue_item_eligible()`，检查全局 flush/redirect、uid active/enq/issue_ready、flushed/redirect/exception、`replay_seq`、`ready_cycle`、dispatched/pass 状态。
- fire 标记前：`mark_issue_fire()` / `mark_issue_fire_already_accepted()` 再次检查 item eligible，只对 DUT 实际接收的 item 删除 issue queue 并置 dispatched。

因此当前测试框架不需要单独的 route pending helper 参与发射合法性检查；global stop 也不再依赖局部 pending-work 扫描。

### 5.3 行为概述

```text
route 阶段：
  route_all_ready_uids() 在公共 active scan 窗口内有限扫描 uid。
  route_uid(uid) 先调用 is_uid_route_ready(uid)。
  route_target(uid,target,behavior) 按 target 做重复过滤和 replay mask 过滤。

select 阶段：
  select_issue_candidates() 如果全局 flush/redirect 阻塞则直接返回。
  select_target_candidates() 只选择 is_issue_item_eligible(item)=1 的 item。

fire 标记阶段：
  driver 回填 fired_mask。
  mark_issue_fire() 再次判断 item 是否仍 eligible。
  合法 item 才会 delete_issue_queue_entry、清 queued、置 dispatched。

退出条件：
  active sequence 正常退出只看 common_data_transaction::global_stop_requested。
  不再通过 uid route pending helper 判断是否退出。
```

### 5.4 正确性检查

- 删除对象无调用点：`rg uid_has_pending_route_work` 只应剩文档说明，不应有源码调用。
- 发射合法性不丢失：真实发射前仍由 `is_issue_item_eligible()` 和 `mark_issue_fire()` 做检查。
- route 去重不丢失：`target_already_queued_or_done()` 仍在 `route_target()` 主路径中调用。
- replay 语义不丢失：`route_target()` 仍在 replay pending 时调用 `data.replay_target_requested()`。
- 退出逻辑更统一：active sequence 不再保留第二套 pending-work helper。

### 5.5 源码支撑片段

这段代码证明 route 入队阶段已经有 target 级去重和 replay mask 检查，不依赖额外 helper：

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`，函数：`route_target()`。
函数功能描述：该函数是真正向 issue queue 入队的入口；先判断 target 是否已 queued/dispatched/done，再判断 replay pending 下该 target 是否被请求重发，最后才 push queue 并置 queued。

```systemverilog
function void route_target(input memblock_uid_t uid,
                           input memblock_issue_target_e target,
                           input memblock_op_behavior_t behavior);
    status_transaction      status;
    memblock_issue_q_item_t item;

    ensure_data();
    status = data.get_status(uid);
    if (target_already_queued_or_done(status, target)) begin
        return;
    end
    if (status.replay_pending &&
        !data.replay_target_requested(status, target)) begin
        return;
    end
    data.delete_issue_queue_entry(target, uid, status.replay_seq, 1'b0);
    item = make_issue_item(uid, target, behavior);
    data.push_issue_queue_item(item);
    set_target_queued(uid, target, 1'b1);
endfunction:route_target
```

中文伪代码：

```text
route_target：
  读取 uid 状态。
  如果这个 target 已经 queued/dispatched/done：返回。
  如果处于 replay_pending 且 replay mask 未请求该 target：返回。
  删除同 target/uid/replay_seq 的旧 queue 项，做幂等保护。
  构造新的 issue item。
  push 到对应 issue queue。
  设置该 target queued=1。
```

这段代码证明发射标记前仍会重新检查 item eligible，避免 select 后状态变化导致误标记：

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`，函数：`mark_issue_fire()`。
函数功能描述：该函数只在全局没有 flush/redirect 阻塞且 item 当前仍 eligible 时，才分配 issue epoch、删除 queue 项并置 dispatched。

```systemverilog
function bit mark_issue_fire(input memblock_issue_q_item_t item);
    int unsigned issue_epoch;

    ensure_data();
    if (data.issue_blocked_by_global_flush()) begin
        return 1'b0;
    end
    if (!is_issue_item_eligible(item)) begin
        return 1'b0;
    end
    issue_epoch = data.alloc_issue_epoch();
    data.mark_issue_snapshot(item.uid, item.target, issue_epoch);
    data.delete_issue_queue_entry(item.target, item.uid, item.replay_seq, 1'b1);
    set_target_queued(item.uid, item.target, 1'b0);
    set_target_dispatched(item.uid, item.target, 1'b1);
    data.clear_replay_target_after_fire(item.uid, item.target);
    return 1'b1;
endfunction:mark_issue_fire
```

中文伪代码：

```text
mark_issue_fire：
  如果当前全局 flush/redirect 阻塞：返回 false。
  如果 item 当前状态已经不 eligible：返回 false。
  分配 issue_epoch 并记录 issue snapshot。
  删除已 fire 的 issue queue item。
  清 queued，置 dispatched。
  如果这是 replay target，清除对应 replay target pending。
  返回 true。
```

### 5.6 新增对象说明

| 对象 | 输入 | 输出/副作用 | 调用者 | 内部关键 helper | Flow 职责 |
|---|---|---|---|---|---|
| 无 | 无 | 本节删除未接入主路径的 helper，没有新增对象。 | 无 | route/select/fire 继续使用既有主路径 helper | 简化 issue scheduler 职责，避免未使用判断入口。 |

## 6. 特性五：enable=0 不调用 super.body()

### 6.1 修改前逻辑

三类 dispatch sequence 都继承 agent default sequence。修改前 disabled 时调用 `super.body()`，这等于“当前 dispatch sequence 不工作，但父类默认 sequence 仍可能继续发随机或安全默认 transaction”。

旧行为的问题是 disabled 不等于 idle。对普通 testcase，父类随机 default sequence 可能绕开 dispatch 主表，破坏“由公共状态统一驱动”的前提。

### 6.2 修改后逻辑

disabled 分支只打印 `uvm_info` 并 return，不调用 `super.body()`。这让 enable bit 成为真正的主动 sequence 开关。

### 6.3 行为概述

```text
每个主动 sequence 的 body 都先读取 enable：
  如果 enable 为 0：
    打印该 sequence stays idle 的 uvm_info。
    直接 return。
    不调用 super.body()。

这样 disabled 的含义是：
  该主动 sequence 完全不驱动。
  不回退父类 default sequence。
  不产生绕开 common_data_transaction 主表的随机 transaction。

如果 enable 为 1：
  才继续 ensure_helpers()。
  等待 main table。
  进入该 sequence 自己的主动驱动 loop。
```

### 6.4 正确性检查

- 任一主动 sequence 被 cfg 显式关闭时，不会回退到父类随机 default sequence。
- dispatch real cfg 显式打开三类 sequence，真实 smoke 仍由公共主表驱动。
- 该修改和 `MEMBLOCK_LSQENQ_SEQ_EN` enable 开关配套；默认保持打开，需要关闭的 testcase 通过 cfg 显式置 0。

### 6.5 源码支撑片段

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`，函数：`body()`。
函数功能描述：LSQENQ 关闭时只打印 idle 信息并返回，不调用父类 default sequence。

```systemverilog
if (!enable) begin
    `uvm_info(get_type_name(), "MEMBLOCK_LSQENQ_SEQ_EN=0, LSQ enqueue dispatch sequence stays idle", UVM_LOW)
    return;
end
```

中文伪代码：

```text
LSQENQ body 检查 enable 时：
  如果 enable 为 0：
    打印 LSQENQ idle 信息。
    直接返回。
  不调用 super.body()，避免父类 default sequence 继续驱动。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`，函数：`body()`。
函数功能描述：LINTSISSUE 关闭时只打印 idle 信息并返回，不驱动 intIssue，也不回退父类随机发射。

```systemverilog
if (!enable) begin
    `uvm_info(get_type_name(), "MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=0, lintsissue dispatch sequence stays idle", UVM_LOW)
    return;
end
```

中文伪代码：

```text
LINTSISSUE body 检查 enable 时：
  如果 enable 为 0：
    打印 LINTSISSUE idle 信息。
    直接返回。
  不调用 super.body()，避免父类 default issue sequence 继续发射。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`，函数：`body()`。
函数功能描述：LSQCOMMIT 关闭时只打印 idle 信息并返回，不驱动 pendingPtr/commit/flushSb 相关 xaction。

```systemverilog
if (!enable) begin
    `uvm_info(get_type_name(), "MEMBLOCK_LSQCOMMIT_SEQ_EN=0, LSQ commit dispatch sequence stays idle", UVM_LOW)
    return;
end
```

中文伪代码：

```text
LSQCOMMIT body 检查 enable 时：
  如果 enable 为 0：
    打印 LSQCOMMIT idle 信息。
    直接返回。
  不调用 super.body()，避免父类 default sequence 驱动 commit 或 flushSb 相关事务。
```

## 7. 完整修改源码逻辑

本节集中补充本轮修改后的关键源码逻辑。每个源码块只对应一个独立函数、task 或关键分支，源码后立刻跟对应中文伪代码。

### 7.1 `common_data_transaction` global stop 逻辑

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`advance_success_prefix()`。
功能简析：推进 success 前缀，只跨过从当前前缀开始连续 `success=1` 的 uid。

```systemverilog
function void advance_success_prefix();
    status_transaction status;

    while (dispatch_progress.success_prefix_uid < main_trans_num) begin
        status = get_status(dispatch_progress.success_prefix_uid);
        if (!status.success) begin
            break;
        end
        dispatch_progress.success_prefix_uid++;
    end
endfunction:advance_success_prefix
```

中文伪代码：

```text
advance_success_prefix：
  从当前 success_prefix_uid 开始检查。
  如果 uid 还小于 main_trans_num：
    读取当前 uid 的 status。
    如果 status.success 不是 1：
      break，停止推进。
    如果 status.success 是 1：
      success_prefix_uid 加 1，继续检查下一个 uid。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`transaction_done()`。
功能简析：以 success 前缀是否达到 `main_trans_num` 判断所有 transaction 是否最终完成。

```systemverilog
function bit transaction_done();
    advance_success_prefix();
    return dispatch_progress.success_prefix_uid >= main_trans_num;
endfunction:transaction_done
```

中文伪代码：

```text
transaction_done：
  调用 advance_success_prefix()，先推进连续 success 前缀。
  如果 success_prefix_uid >= main_trans_num：
    返回 true。
  否则返回 false。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`request_global_stop_if_done()`。
功能简析：当 `transaction_done()` 确认全部完成后，置位全局停止请求。

```systemverilog
function void request_global_stop_if_done();
    if (transaction_done()) begin
        global_stop_requested = 1'b1;
    end
endfunction:request_global_stop_if_done
```

中文伪代码：

```text
request_global_stop_if_done：
  调用 transaction_done()。
  如果 transaction_done() 为 true：
    将 global_stop_requested 设置为 1。
  如果 transaction_done() 为 false：
    不修改 global_stop_requested。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`is_global_stop_requested()`。
功能简析：给 active sequence 提供只读 stop 查询接口。

```systemverilog
function bit is_global_stop_requested();
    return global_stop_requested;
endfunction:is_global_stop_requested
```

中文伪代码：

```text
is_global_stop_requested：
  返回 global_stop_requested 当前值。
```

### 7.2 `memblock_dispatch_real_smoke_sequence` 顶层 stop 与 end check

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`，函数：`body()`。
功能简析：real smoke 顶层入口，负责建表、运行 service loop，并在 loop 结束后调用最终一致性检查。

```systemverilog
task memblock_dispatch_real_smoke_sequence::body();
    build_main_table();
    `uvm_info(get_type_name(),
              $sformatf("real dispatch smoke main table ready: main_trans_num=%0d",
                        data.main_trans_num),
              UVM_LOW)
    service_real_dispatch_flow();
    data.end_test_check();
    `uvm_info(get_type_name(), "real dispatch smoke sequence completed", UVM_LOW)
endtask:body
```

中文伪代码：

```text
body：
  调用 build_main_table() 生成主表。
  打印 main_trans_num。
  调用 service_real_dispatch_flow() 运行真实 dispatch flow。
  service loop 返回后调用 data.end_test_check() 做最终一致性检查。
  打印 completed。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`，函数：`service_real_dispatch_flow()`。
功能简析：按 service clock 推进真实 dispatch flow，每拍先检查完成，再收集 monitor/recovery 事件并 route issue。

```systemverilog
task memblock_dispatch_real_smoke_sequence::service_real_dispatch_flow();
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
endtask:service_real_dispatch_flow
```

中文伪代码：

```text
service_real_dispatch_flow：
  确保 service_vif 已获取。
  forever 循环：
    等待 service_vif.clk 下降沿。
    如果 reset 未完成：
      continue，跳过本拍。
    调用 all_transactions_success()：
      如果已完成，break。
    调用 service_monitor_once() 收集并处理 monitor/recovery 事件。
    调用 route_all_issue_queues() 路由 ready uid。
    再次调用 all_transactions_success()：
      如果本拍处理后已完成，break。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`，函数：`all_transactions_success()`。
功能简析：运行期 stop 判断入口，只请求 global stop，不做最终一致性验收。

```systemverilog
function bit memblock_dispatch_real_smoke_sequence::all_transactions_success();
    if (data == null || data.main_trans_num == 0) begin
        return 1'b0;
    end
    data.request_global_stop_if_done();
    return data.is_global_stop_requested();
endfunction:all_transactions_success
```

中文伪代码：

```text
all_transactions_success：
  如果 data 为空或 main_trans_num 为 0：
    返回 false。
  调用 data.request_global_stop_if_done()。
  返回 data.is_global_stop_requested()。
```

### 7.3 `common_data_transaction::end_test_check()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数：`end_test_check()`。
功能简析：testcase 结束时的最终一致性检查，关闭 monitor capture，检查公共队列/map/recovery 状态是否清空，并对残留状态报错。

```systemverilog
function void end_test_check();
    int unsigned uid;

    memblock_sync_pkg::dispatch_monitor_capture_en = 1'b0;
    if (memblock_sync_pkg::raw_monitor_queue_size() != 0) begin
        `uvm_error("COMMON_DATA",
                         $sformatf("clear %0d raw monitor events at end_test_check",
                                   memblock_sync_pkg::raw_monitor_queue_size()))
        memblock_sync_pkg::clear_raw_monitor_queues();
    end
    if (main_trans_num == 0) begin
        return;
    end
    if (next_uid != main_trans_num) begin
        `uvm_error("COMMON_DATA", $sformatf("uid allocation mismatch: next_uid=%0d main_trans_num=%0d", next_uid, main_trans_num))
    end
    for (uid = 0; uid < main_trans_num; uid++) begin
        if (status_by_uid[uid] == null) begin
            `uvm_fatal("COMMON_DATA", $sformatf("status_by_uid[%0d] is null at end_test_check", uid))
        end
        if (status_by_uid[uid].active ||
            status_by_uid[uid].exception_pending ||
            status_by_uid[uid].replay_pending ||
            status_by_uid[uid].redirect_pending) begin
            `uvm_error("COMMON_DATA", $sformatf("uid=%0d has unfinished status at end_test_check", uid))
        end
    end
    if (uid_by_active_rob.num() != 0 || uid_by_lq.num() != 0 || uid_by_sq.num() != 0) begin
        `uvm_error("COMMON_DATA", "active ROB/LQ/SQ mapping is not empty at end_test_check")
    end
    if (load_issue_q.size() != 0 || sta_issue_q.size() != 0 || std_issue_q.size() != 0) begin
        `uvm_error("COMMON_DATA", "issue queues are not empty at end_test_check")
    end
    if (flush_in_progress || active_redirect.valid || issue_freeze_ack) begin
        `uvm_error("COMMON_DATA", "global flush/redirect control state is not idle at end_test_check")
    end
    if (has_pending_redirect_drive() || redirect_phase != MEMBLOCK_REDIRECT_PHASE_IDLE) begin
        `uvm_error("COMMON_DATA", "redirect drive queue/state is not idle at end_test_check")
    end
    if (flushsb_pending || flushsb_waiting_empty || flushsb_scheduled_pending) begin
        `uvm_error("COMMON_DATA", "flushSb state is not idle at end_test_check")
    end
    if (ptw_wait_replay_q.size() != 0) begin
        `uvm_error("COMMON_DATA", "ptw_wait_replay queue is not empty at end_test_check")
    end
endfunction:end_test_check
```

中文伪代码：

```text
end_test_check：
  关闭 dispatch monitor capture。
  如果 raw monitor queue 非空：报 uvm_error 并清空 raw queue。
  如果 main_trans_num 为 0：return。
  如果 next_uid != main_trans_num：报 uid 分配错误。
  遍历每个 uid：
    如果 status 为空：报 uvm_fatal。
    如果 active 或 exception/replay/redirect pending 未清：报 uvm_error。
  如果 active ROB/LQ/SQ map 非空：报 uvm_error。
  如果 load/STA/STD issue queue 非空：报 uvm_error。
  如果 flush/redirect/freeze 状态不 idle：报 uvm_error。
  如果 redirect drive queue 非空或 redirect phase 非 IDLE：报 uvm_error。
  如果 flushSb 状态不 idle：报 uvm_error。
  如果 ptw_wait_replay_q 非空：报 uvm_error。
```

### 7.4 LSQENQ 修改后关键源码逻辑

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`，函数：`body()`。
功能简析：LSQENQ body 处理配置初始化、enable gate、helper 初始化、等待主表和进入主循环。

```systemverilog
task memblock_lsqenq_dispatch_sequence::body();
    seq_csr_common::init();
    configure_from_plus();
    if (!enable) begin
        `uvm_info(get_type_name(), "MEMBLOCK_LSQENQ_SEQ_EN=0, LSQ enqueue dispatch sequence stays idle", UVM_LOW)
        return;
    end
    ensure_helpers();
    wait_for_main_table();
    drive_lsqenq_loop();
endtask:body
```

中文伪代码：

```text
LSQENQ body：
  初始化 seq_csr_common。
  读取 plus/cfg 配置。
  如果 enable 为 0：打印 idle 并 return。
  初始化 helper。
  等待 main table ready。
  进入 drive_lsqenq_loop()。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`，函数：`drive_lsqenq_loop()`。
功能简析：LSQENQ 主循环负责 global stop 退出和 no-progress warning。

```systemverilog
task memblock_lsqenq_dispatch_sequence::drive_lsqenq_loop();
    int unsigned idle_count;
    int unsigned cycle_idx;

    idle_count = 0;
    cycle_idx = 0;
    forever begin
        bit has_progress;

        if (data.is_global_stop_requested()) begin
            `uvm_info(get_type_name(),
                      $sformatf("stop LSQ enqueue loop by global_stop_requested at cycle=%0d",
                                cycle_idx),
                      UVM_LOW)
            break;
        end
        send_lsqenq_cycle(cycle_idx, has_progress);
        cycle_idx++;
        if (has_progress) begin
            idle_count = 0;
        end else begin
            idle_count++;
            if (no_progress_warn_cycles != 0 &&
                idle_count >= no_progress_warn_cycles) begin
                `uvm_warning(get_type_name(),
                             $sformatf("no LSQ enqueue progress for %0d cycles: cycle=%0d success_prefix=%0d main_trans_num=%0d",
                                       idle_count,
                                       cycle_idx,
                                       data.dispatch_progress.success_prefix_uid,
                                       data.main_trans_num))
                idle_count = 0;
            end
        end
    end
endtask:drive_lsqenq_loop
```

中文伪代码：

```text
drive_lsqenq_loop：
  初始化 idle_count 和 cycle_idx。
  forever 循环：
    如果 global stop 已请求：打印并 break。
    调用 send_lsqenq_cycle() 执行一拍 admission。
    cycle_idx 加 1。
    如果有进展：idle_count 清零。
    如果无进展：idle_count 加 1；达到阈值时 warning 并清零。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`，函数：`send_lsqenq_cycle()`。
功能简析：LSQENQ 单拍逻辑，处理 cancel、non-LSQ admission、idle item 和真实 LSQ candidate 驱动。

```systemverilog
task memblock_lsqenq_dispatch_sequence::send_lsqenq_cycle(input int unsigned cycle_idx,
                                                          output bit has_progress);
    lsqenq_agent_agent_xaction tr;
    memblock_uid_t            uids[$];
    main_control_transaction  trs[$];
    memblock_op_behavior_t    behaviors[$];
    memblock_lq_key_t         lq_keys[$];
    memblock_sq_key_t         sq_keys[$];

    has_progress = 1'b0;
    apply_pending_lsq_cancels();
    if (admit_non_lsq_if_ready(has_progress)) begin
        return;
    end
    if (!collect_lsq_candidates(uids, trs, behaviors, lq_keys, sq_keys)) begin
        tr = lsqenq_agent_agent_xaction::type_id::create($sformatf("lsqenq_dispatch_idle_tr_%0d", cycle_idx));
        if (tr == null) begin
            `uvm_fatal(get_type_name(), "failed to create idle lsqenq xaction")
        end
        clear_lsqenq_xaction(tr);
        tr.memblock_dispatch_wait_can_accept = 1'b0;
        tr.memblock_dispatch_ready_timeout = ready_timeout;
        tr.memblock_dispatch_aborted_by_redirect = 1'b0;
        tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
        tr.pre_pkt_gap = 0;
        tr.post_pkt_gap = 0;
        start_item(tr);
        finish_item(tr);
        return;
    end

    tr = lsqenq_agent_agent_xaction::type_id::create($sformatf("lsqenq_dispatch_tr_%0d", cycle_idx));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create lsqenq xaction")
    end
    clear_lsqenq_xaction(tr);
    tr.memblock_dispatch_wait_can_accept = 1'b1;
    tr.memblock_dispatch_ready_timeout = ready_timeout;
    tr.memblock_dispatch_aborted_by_redirect = 1'b0;
    tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    foreach (uids[idx]) begin
        assign_lsqenq_slot(tr, idx, uids[idx], trs[idx], behaviors[idx], lq_keys[idx], sq_keys[idx]);
    end

    start_item(tr);
    finish_item(tr);
    confirm_lsq_candidates(tr, uids, trs, behaviors, has_progress);
endtask:send_lsqenq_cycle
```

中文伪代码：

```text
send_lsqenq_cycle：
  清 has_progress。
  处理 pending LQ/SQ cancel。
  如果 non-LSQ admission 成功：return。
  如果没有 LSQ candidate：
    创建 idle xaction；wait_can_accept=0；发送后 return。
  如果有 LSQ candidate：
    创建真实 xaction；wait_can_accept=1。
    为每个 candidate 填 slot。
    发送 xaction。
    调用 confirm_lsq_candidates() 确认 admission 并更新 has_progress。
```

### 7.5 LINTSISSUE 修改后关键源码逻辑

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`，函数：`drive_dispatch_issue_loop()`。
功能简析：LINTSISSUE 主循环负责 route ready uid、发送一拍 issue xaction、推进 queue delay，并在 global stop 后退出。

```systemverilog
task memblock_lintsissue_dispatch_sequence::drive_dispatch_issue_loop();
    int unsigned cycle_idx;
    int unsigned idle_count;

    cycle_idx = 0;
    idle_count = 0;
    forever begin
        bit has_fire;

        issue_sched.route_all_ready_uids();
        send_issue_cycle(cycle_idx, has_fire);
        issue_sched.advance_issue_queue_delays();

        if (data.is_global_stop_requested()) begin
            `uvm_info(get_type_name(),
                      $sformatf("stop dispatch issue loop by global_stop_requested at cycle=%0d",
                                cycle_idx),
                      UVM_LOW)
            break;
        end

        if (has_fire) begin
            idle_count = 0;
        end else begin
            idle_count++;
            if (no_progress_warn_cycles != 0 &&
                idle_count >= no_progress_warn_cycles) begin
                `uvm_warning(get_type_name(),
                             $sformatf("no issue fire for %0d cycles: cycle=%0d success_prefix=%0d main_trans_num=%0d load_q=%0d sta_q=%0d std_q=%0d",
                                       idle_count,
                                       cycle_idx,
                                       data.dispatch_progress.success_prefix_uid,
                                       data.main_trans_num,
                                       data.load_issue_q.size(),
                                       data.sta_issue_q.size(),
                                       data.std_issue_q.size()))
                idle_count = 0;
            end
        end
        cycle_idx++;
    end
endtask:drive_dispatch_issue_loop
```

中文伪代码：

```text
drive_dispatch_issue_loop：
  初始化 cycle_idx 和 idle_count。
  forever 循环：
    route ready uid 到 issue queue。
    发送一拍 issue xaction。
    推进 queue delay。
    如果 global stop 已请求：打印并 break。
    如果 has_fire：idle_count 清零。
    如果没有 fire：idle_count 加 1；达到阈值时 warning 并清零。
    cycle_idx 加 1。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`，函数：`send_issue_cycle()`。
功能简析：LINTSISSUE 单拍发射逻辑，负责创建 xaction、选择候选、发送，并根据 redirect/flush 边界决定是否标记 fired。

```systemverilog
task memblock_lintsissue_dispatch_sequence::send_issue_cycle(input int unsigned cycle_idx,
                                                             output bit has_fire);
    lintsissue_agent_agent_xaction tr;
    memblock_issue_q_item_t load_items[$];
    memblock_issue_q_item_t sta_items[$];
    memblock_issue_q_item_t std_items[$];
    memblock_issue_q_item_t fired_items[$];

    has_fire = 1'b0;
    tr = lintsissue_agent_agent_xaction::type_id::create($sformatf("lintsissue_dispatch_tr_%0d", cycle_idx));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create lintsissue xaction")
    end

    field_assigner.clear_lintsissue_xaction(tr);
    tr.memblock_dispatch_wait_ready = 1'b1;
    tr.memblock_dispatch_ready_timeout = seq_csr_common::get_dispatch_ready_timeout();
    tr.memblock_dispatch_aborted_by_redirect = 1'b0;
    tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    tr.memblock_dispatch_fired_mask = '0;
    if (!data.issue_blocked_by_global_flush()) begin
        issue_sched.select_issue_candidates(load_items, sta_items, std_items);
        if (!data.issue_blocked_by_global_flush()) begin
            assign_issue_items(tr, load_items, fired_items);
            assign_issue_items(tr, sta_items, fired_items);
            assign_issue_items(tr, std_items, fired_items);
        end
    end

    start_item(tr);
    finish_item(tr);

    if (tr.memblock_dispatch_aborted_by_redirect) begin
        if (tr.memblock_dispatch_fired_mask != '0) begin
            mark_fired_items(fired_items, tr.memblock_dispatch_fired_mask);
            has_fire = 1'b1;
        end
        if (fired_items.size() != 0) begin
            `uvm_info(get_type_name(), "partial issue fire marking after redirect abort", UVM_LOW)
        end
        return;
    end

    if (data.issue_blocked_by_global_flush() ||
        tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
        if (fired_items.size() != 0) begin
            `uvm_info(get_type_name(), "skip issue fire marking because redirect/flush is in progress", UVM_LOW)
        end
        return;
    end

    if (fired_items.size() != 0) begin
        mark_fired_items(fired_items, 7'h7f);
        has_fire = 1'b1;
    end
endtask:send_issue_cycle
```

中文伪代码：

```text
send_issue_cycle：
  清 has_fire。
  创建 xaction，失败则 fatal。
  清空 xaction，并设置 wait_ready、ready_timeout、flush_epoch、fired_mask。
  如果没有 global flush：选择 issue candidate 并填充 xaction。
  发送 xaction。
  如果 driver 报 redirect abort：
    只按 fired_mask 标记真实 fired port，设置 has_fire，然后 return。
  如果发送后发现 flush 或 flush_epoch 过期：
    跳过 fire 标记并 return。
  如果 fired_items 非空且没有 flush/redirect 干扰：
    标记 fired_items，设置 has_fire。
```

### 7.6 LSQCOMMIT 修改后关键源码逻辑

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`，函数：`drive_lsqcommit_loop()`。
功能简析：LSQCOMMIT 主循环以 global stop 加 flushSb drain 作为退出条件，并用 no-progress warning 做 debug。

```systemverilog
task memblock_lsqcommit_dispatch_sequence::drive_lsqcommit_loop();
    int unsigned cycle_idx;
    int unsigned idle_count;

    cycle_idx = 0;
    idle_count = 0;
    forever begin
        bit has_commit;

        if (data.is_global_stop_requested() &&
            !flushsb_request_pending(cycle_idx)) begin
            `uvm_info(get_type_name(),
                      $sformatf("stop LSQ commit loop by global_stop_requested at cycle=%0d",
                                cycle_idx),
                      UVM_LOW)
            break;
        end

        send_lsqcommit_cycle(cycle_idx, has_commit);
        cycle_idx++;
        if (has_commit) begin
            idle_count = 0;
        end else begin
            idle_count++;
            if (no_progress_warn_cycles != 0 &&
                idle_count >= no_progress_warn_cycles) begin
                `uvm_warning(get_type_name(),
                             $sformatf("no LSQ commit/flushSb progress for %0d cycles: cycle=%0d success_prefix=%0d main_trans_num=%0d flushsb_pending=%0d waiting_empty=%0d scheduled=%0d",
                                       idle_count,
                                       cycle_idx,
                                       data.dispatch_progress.success_prefix_uid,
                                       data.main_trans_num,
                                       data.flushsb_pending,
                                       data.flushsb_waiting_empty,
                                       data.flushsb_scheduled_pending))
                idle_count = 0;
            end
        end
    end
endtask:drive_lsqcommit_loop
```

中文伪代码：

```text
drive_lsqcommit_loop：
  初始化 cycle_idx 和 idle_count。
  forever 循环：
    如果 global stop 已请求且没有 flushSb pending：打印并 break。
    调用 send_lsqcommit_cycle() 执行一拍 commit/flushSb。
    cycle_idx 加 1。
    如果有 commit/flushSb 进展：idle_count 清零。
    如果无进展：idle_count 加 1；达到阈值时 warning 并清零。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`，函数：`send_lsqcommit_cycle()`。
功能简析：LSQCOMMIT 单拍逻辑，按顺序处理 scheduled flushSb、global flush idle、flushSb drive/wait 和 commit xaction。

```systemverilog
task memblock_lsqcommit_dispatch_sequence::send_lsqcommit_cycle(input int unsigned cycle_idx,
                                                                output bit has_commit);
    lsqcommit_agent_agent_xaction tr;
    memblock_uid_t                commit_uids[$];
    bit                           did_flushsb_drive;

    has_commit = 1'b0;
    void'(data.request_scheduled_flushsb_if_due(cycle_idx));
    if (data.issue_blocked_by_global_flush()) begin
        tr = lsqcommit_agent_agent_xaction::type_id::create($sformatf("lsqcommit_dispatch_idle_tr_%0d", cycle_idx));
        commit_handler.clear_lsqcommit_xaction(tr);
        start_item(tr);
        finish_item(tr);
        return;
    end
    drive_flushsb_if_needed(cycle_idx, did_flushsb_drive);
    if (did_flushsb_drive || data.flushsb_waiting_empty) begin
        tr = lsqcommit_agent_agent_xaction::type_id::create($sformatf("lsqcommit_flushsb_wait_tr_%0d", cycle_idx));
        commit_handler.clear_lsqcommit_xaction(tr);
        start_item(tr);
        finish_item(tr);
        has_commit = 1'b1;
        return;
    end
    commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit);
    tr.set_name($sformatf("lsqcommit_dispatch_tr_%0d", cycle_idx));

    start_item(tr);
    finish_item(tr);

    if (has_commit) begin
        commit_handler.mark_rob_commit_batch(commit_uids);
    end
endtask:send_lsqcommit_cycle
```

中文伪代码：

```text
send_lsqcommit_cycle：
  清 has_commit。
  根据 cycle_idx 请求 scheduled flushSb。
  如果 global flush 阻塞：发送 idle xaction 后 return。
  调用 drive_flushsb_if_needed()。
  如果刚 drive flushSb 或正在等 sb empty：发送 wait xaction，设置 has_commit=1，return。
  构造 commit xaction 并发送。
  如果 has_commit 为 1：标记 commit_uids 已 ROB commit。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`，函数：`flushsb_request_pending()`。
功能简析：汇总 scheduled flushSb、pending flushSb 和 waiting empty 状态，决定 global stop 后是否继续 loop。

```systemverilog
function bit memblock_lsqcommit_dispatch_sequence::flushsb_request_pending(input int unsigned cycle_idx);
    return data.scheduled_flushsb_pending(cycle_idx) ||
           data.flushsb_pending ||
           data.flushsb_waiting_empty;
endfunction:flushsb_request_pending
```

中文伪代码：

```text
flushsb_request_pending：
  如果当前 cycle 已到 scheduled flushSb：返回 true。
  如果 flushsb_pending 为 1：返回 true。
  如果 flushsb_waiting_empty 为 1：返回 true。
  否则返回 false。
```

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`，函数：`drive_flushsb_if_needed()`。
功能简析：在需要 flushSb 时驱动 `io_ooo_to_mem_flushSb`，并记录 flushSb 已发出。

```systemverilog
task memblock_lsqcommit_dispatch_sequence::drive_flushsb_if_needed(input int unsigned cycle_idx,
                                                                   output bit did_drive);
    lsqcommit_agent_agent_xaction tr;

    did_drive = 1'b0;
    if (data.flushsb_timed_out(seq_csr_common::get_flushsb_timeout())) begin
        `uvm_fatal(get_type_name(), "timeout waiting sbIsEmpty after flushSb")
    end
    if (!data.should_drive_flushsb()) begin
        return;
    end
    tr = lsqcommit_agent_agent_xaction::type_id::create($sformatf("lsqcommit_flushsb_tr_%0d", cycle_idx));
    commit_handler.clear_lsqcommit_xaction(tr);
    tr.io_ooo_to_mem_flushSb = 1'b1;
    start_item(tr);
    finish_item(tr);
    data.mark_flushsb_driven(memblock_sync_pkg::get_dispatch_service_cycle());
    did_drive = 1'b1;
endtask:drive_flushsb_if_needed
```

中文伪代码：

```text
drive_flushsb_if_needed：
  先将 did_drive 清 0。
  如果等待 sbIsEmpty 超时：uvm_fatal。
  如果当前不需要 drive flushSb：return。
  创建 flushSb xaction 并清空默认字段。
  设置 io_ooo_to_mem_flushSb=1。
  发送 xaction。
  调用 mark_flushsb_driven() 记录已驱动。
  将 did_drive 置 1。
```

## 8. 旧对象替换总结

| 删除/替换对象 | 新对象 | 替代方式 |
|---|---|---|
| 各 sequence 私有 `max_cycles` | `global_stop_requested` + 外部 testcase/UVM timeout | 正常退出由顶层完成条件控制，卡死由整体 timeout 兜底。 |
| 各 sequence 私有 `idle_stop` | `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES` | 空闲只 warning，不退出。 |
| 各 sequence 私有 `start_timeout` | `wait_for_main_table()` 周期 warning | 等主表不再当作局部 fatal；真实失败由顶层 timeout 暴露。 |
| LSQCOMMIT `has_lsqcommit_activity()` | 常驻 loop + global stop + flushSb pending 判断 | 不再全表扫描 activity，也不把“暂无 activity”作为退出依据。 |
| disabled 分支 `super.body()` | disabled 分支直接 return | 关闭主动 sequence 时真正 idle，不回退父类行为。 |

典型场景：dispatch real smoke 启动时，LSQENQ/LINTSISSUE/LSQCOMMIT 可能早于主控 vseq 进入 body。旧 start timeout 会在主表未 ready 时 fatal；新逻辑只周期 warning，等主控建表后进入常驻 loop。所有 uid success 后，顶层设置 global stop，三个 sequence 再按同一个标志退出。

## 9. 验证记录

用户提供的已有验证记录：

| 验证项 | 结果 |
|---|---|
| `git diff --check` | 通过。 |
| `make eda_compile tc=tc_sanity mode=base_fun` | 通过，KDB 0 error / 0 warning。 |
| `make eda_run tc=tc_sanity mode=base_fun` | 历史验证记录：通过，`UVM_WARNING=0` / `UVM_ERROR=0` / `UVM_FATAL=0`。该记录产生于本次恢复 `MEMBLOCK_LSQENQ_SEQ_EN=1` 默认值之前，不能作为当前默认 cfg 的最新运行结论。 |
| `make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke` | 通过。 |
| subagent 复审 | 之前两个 blocker 均已解决，无新 blocker。 |

本文未重新运行仿真，只基于当前 `git diff`、`git status` 和上述验证记录生成 review。

## 10. 残余风险和主 agent 复查点

1. `memblock_lsqcommit_dispatch_sequence::wait_for_main_table()` 改用 `lsqcommit_vif.clk`。需要确认所有运行该 sequence 的环境都能通过 `get_full_name()` 或 `"uvm_test_top.env.u_lsqcommit_agent_agent*"` 配到 vif。
2. 运行期 `all_transactions_success()` 只用 success 前缀请求 `global_stop_requested`，不再每拍扫描 raw queue、issue queue、active map、flush/redirect/PTW wait 等状态；最终一致性仍由 `data.end_test_check()` 检查。残余风险更准确地说是：后续新增 `success=1` 设置路径时，必须保证 success 只在该 uid 的最终闭环完成后置位，否则可能提前请求 global stop，然后由 `end_test_check()` 报出残留状态。
3. `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES=0` 会关闭 warning。若用户误设为 0，卡住时只剩 testcase/UVM timeout 暴露，需要文档或 cfg 说明。
4. `default.cfg` 保持 `MEMBLOCK_LSQENQ_SEQ_EN=1` 和 `MEMBLOCK_LSQCOMMIT_SEQ_EN=1`，避免破坏依赖默认 LSQENQ/LSQCOMMIT 开启的旧 testcase；不需要对应 sequence 的 testcase 应显式 cfg 关闭。

## 11. 覆盖性结论

本 review 已覆盖当前 `mem_ut/ver/ut/memblock` diff 中和 active sequence global stop 生命周期相关的所有源码、cfg 和规则说明文件。文档迁移类 diff 不属于本 review 的功能范围，未展开。
