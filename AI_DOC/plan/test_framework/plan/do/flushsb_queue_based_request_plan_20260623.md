# flushSb 队列式请求重构方案

状态：待实现
创建日期：2026-06-23
归属：mem_ut 测试框架 plan

## 1. 背景和目标

当前 flushSb directed flow 是“预约式”实现：`common_data_transaction` 中维护
`flushsb_scheduled_pending/flushsb_scheduled_issued/flushsb_scheduled_cycle`，由
`memblock_lsqcommit_dispatch_sequence` 每拍调用
`request_scheduled_flushsb_if_due(cycle_idx)` 把预约转换成 `flushsb_pending`，再单独构造一个
`lsqcommit_flushsb_tr` 去驱动 `io_ooo_to_mem_flushSb=1`。

这个实现有两个问题：

1. flushSb 触发入口和 LSQ commit sequence 绑得太紧。后续其它 flow 想触发 flushSb，也必须复用
   scheduled 状态或直接碰 `flushsb_pending`。
2. flushSb drive 被做成独立 transaction，驱动 flushSb 时会绕开本拍正常 LSQ commit 赋值，不符合
   “flushSb 可以和 LSQ commit 字段同拍赋值”的接口使用方式。

本方案把 flushSb 改成队列式运行期请求：

```text
任意 producer
  -> common_data_transaction.flushsb_req_q.push_back(req)
  -> memblock_lsqcommit_dispatch_sequence 每拍消费队列
  -> 在本拍 lsqcommit xaction 上附加 io_ooo_to_mem_flushSb=1
  -> ctrl monitor 继续采样 sbIsEmpty
  -> common_data_transaction.update_sb_is_empty() 清 waiting_empty
```

实现约束：本方案落地后不保留旧预约式兼容路径。所有旧方案中不再被新队列式 flow 使用的字段、函数、调用点和文档描述都必须删除，不允许同时维护 scheduled/pending 旧路径和 queue 新路径。

## 2. 总体设计

### 2.1 请求生产和请求消费解耦

flushSb 请求只通过 `common_data_transaction` 中的队列传递。

- producer 只负责调用 `push_flushsb_request()`。
- consumer 只有 `memblock_lsqcommit_dispatch_sequence`。
- `flushsb_base_sequence` 是一个普通 producer，由 plus 参数控制是否按周期向队列塞请求。
- 后续 fence/CBO/专项 testcase/soft directed flow 都可以通过同一个队列入口触发 flushSb。

### 2.2 `MEMBLOCK_FLUSHSB_SEQ_EN` 语义调整

新语义：

- `MEMBLOCK_FLUSHSB_SEQ_EN=1`：启动 `flushsb_base_sequence` 周期性 producer。
- `MEMBLOCK_FLUSHSB_SEQ_EN=0`：不启动周期性 producer。
- 该参数不再作为 `common_data_transaction::push_flushsb_request()` 的全局 gate。

原因：如果 `push_flushsb_request()` 继续检查 `MEMBLOCK_FLUSHSB_SEQ_EN`，那么其它真实场景或 testcase
即使明确向队列放请求，也会被静默忽略。这和“之后想要触发 flushSb 均可向队列存放一个值”的目标冲突。

### 2.3 `MEMBLOCK_FLUSHSB_REQUEST_CYCLE` 语义调整

新语义：

- `MEMBLOCK_FLUSHSB_REQUEST_CYCLE=0`：不启用周期性定时 producer；flushSb 仍支持由其它 producer 通过 `push_flushsb_request()` 不定时触发。
- `MEMBLOCK_FLUSHSB_REQUEST_CYCLE=N`：`flushsb_base_sequence` 每间隔 N 个 clk 向队列 push 一个 flushSb 请求。

该参数不再表示“一次性预约 cycle”，而是周期触发间隔。

### 2.4 `MEMBLOCK_FLUSHSB_TIMEOUT` 语义调整

新语义：

- timeout 到达只打印 `uvm_warning`。
- 不退出 sequence。
- 不 fatal。
- 不清除 waiting 状态。
- 等后续 ctrl monitor 采到 `sbIsEmpty=1` 后仍按正常流程完成。

为避免每拍刷 warning，每个 flushSb 请求最多报一次 timeout warning。新请求重新 reset warning 标志。

## 3. 数据结构和公共 API

### 3.1 新增 flushSb 请求结构

建议在 `memblock_dispatch_types.sv` 中新增：

```systemverilog
typedef struct packed {
    int unsigned     req_id;
    longint unsigned enqueue_cycle;
    int unsigned     source;
} memblock_flushsb_req_t;
```

字段含义：

- `req_id`：请求编号，只用于 debug 和日志。
- `enqueue_cycle`：请求入队时的 dispatch service cycle。
- `source`：请求来源。第一版可先使用简单 `int unsigned`，例如 0 表示 directed/unknown，1 表示 periodic。

如果第一版不需要细分来源，也可以只保留 `req_id/enqueue_cycle`。但建议保留 `source`，方便后续区分
periodic、fence directed、testcase directed 等来源。

### 3.2 `common_data_transaction` 状态字段修改

当前旧实现中的 flushSb 状态含义和新方案去留如下：

| 字段 | 当前含义 | 新方案处理 |
| --- | --- | --- |
| `flushsb_pending` | 已经有一个 flushSb 请求待 drive，但还没打到 DUT。 | 删除。由 `flushsb_req_q.size()!=0` 表示“还有未消费请求”。 |
| `flushsb_waiting_empty` | flushSb 已经 drive 到 DUT，正在等 `sbIsEmpty=1`。 | 保留。仍表示 active flushSb 请求还没完成。 |
| `flushsb_scheduled_pending` | 已预约一个未来 cycle 的 flushSb 请求。 | 删除。由 `flushsb_base_sequence` 本地计数后直接 `push_flushsb_request()` 替代。 |
| `flushsb_scheduled_issued` | 预约请求是否已经转换成 pending。 | 删除。队列 pop 和 `active_flushsb_req_valid` 天然表达是否已经发出。 |
| `flushsb_scheduled_cycle` | 预约 flushSb 的目标触发 cycle。 | 删除。周期触发间隔由 `flushsb_base_sequence` 的 counter 和 `MEMBLOCK_FLUSHSB_REQUEST_CYCLE` 管理。 |
| `flushsb_start_cycle` | 当前 active flushSb drive 到 DUT 的 cycle，用于 timeout 统计。 | 保留。timeout warning 仍需要根据它计算等待时间。 |

总结：旧的 `pending/scheduled_*` 字段只服务“一次性预约转 pending”模型。新方案改成请求队列后，待消费状态由队列表示，已发出状态由 `active_flushsb_req_valid/flushsb_waiting_empty` 表示，因此旧预约字段需要删除；等待 empty 和 timeout 相关字段仍然保留。

删除旧预约字段：

```systemverilog
bit                         flushsb_pending;
bit                         flushsb_scheduled_pending;
bit                         flushsb_scheduled_issued;
int unsigned                flushsb_scheduled_cycle;
```

新增队列字段：

```systemverilog
memblock_flushsb_req_t      flushsb_req_q[$];
memblock_flushsb_req_t      active_flushsb_req;
bit                         active_flushsb_req_valid;
int unsigned                next_flushsb_req_id;
bit                         flushsb_timeout_warned;
```

保留字段：

```systemverilog
bit                         flushsb_waiting_empty;
longint unsigned            flushsb_start_cycle;
bit                         last_sb_is_empty;
```

新增字段含义：

| 字段 | 含义 | 主要更新时机 |
| --- | --- | --- |
| `flushsb_req_q[$]` | flushSb 待处理请求队列。所有 producer 只向这个队列 push 请求，LSQ commit sequence 是唯一 consumer。 | `push_flushsb_request()` 入队；`try_pop_flushsb_request()` 在可 drive 时出队；`reset_all_tables()` 清空。 |
| `active_flushsb_req` | 当前已经 drive 到 DUT、正在等待 `sbIsEmpty` 的 flushSb 请求备份。用于 timeout 和 debug 日志打印请求 id、来源和入队 cycle。 | `mark_flushsb_driven()` 记录出队并已 drive 的请求；`update_sb_is_empty()` 完成后清默认值；`reset_all_tables()` 清默认值。 |
| `active_flushsb_req_valid` | `active_flushsb_req` 是否有效。为 1 表示当前存在一个已 drive 但尚未完成的 flushSb 请求。 | `mark_flushsb_driven()` 置 1；`update_sb_is_empty(sb_is_empty=1)` 清 0；`reset_all_tables()` 清 0。 |
| `next_flushsb_req_id` | 下一个 flushSb 请求编号。只用于给请求分配递增 id，便于日志和 debug，不影响 DUT 端口值。 | `push_flushsb_request()` 使用后递增；`reset_all_tables()` 清 0。 |
| `flushsb_timeout_warned` | 当前 active flushSb 请求是否已经打印过 timeout warning。避免等待 `sbIsEmpty` 期间每拍重复报同一个 warning。 | `mark_flushsb_driven()` 清 0；`warn_flushsb_timeout_if_needed()` 报警后置 1；`update_sb_is_empty()` 完成后清 0；`reset_all_tables()` 清 0。 |

保留字段含义：

- `flushsb_waiting_empty`：flushSb 已经 drive 到 DUT，正在等 `sbIsEmpty=1`。
- `flushsb_start_cycle`：当前 active flushSb drive 到 DUT 的 cycle，用于 timeout 统计。
- `last_sb_is_empty`：最近一次 ctrl monitor/adapter 采到的 `sbIsEmpty` 值，用于状态观察和 debug。

### 3.3 新增/替换公共函数

新增：

```systemverilog
function void push_flushsb_request(input int unsigned source = 0);
function bit has_pending_flushsb_request();
function bit try_pop_flushsb_request(output memblock_flushsb_req_t req);
function bit flushsb_busy();
function bit flushsb_request_pending();
function void warn_flushsb_timeout_if_needed(input int unsigned timeout);
```

保留并调整：

```systemverilog
function void mark_flushsb_driven(input memblock_flushsb_req_t req,
                                  input longint unsigned cycle);
function void update_sb_is_empty(input bit sb_is_empty);
```

函数功能边界：

| 函数 | 功能 | 主要调用方 / 使用场景 |
| --- | --- | --- |
| `push_flushsb_request(source)` | producer 统一入口，向公共 flushSb 请求队列追加一个请求。 | `flushsb_base_sequence` 周期性触发；后续 testcase/fence/CBO directed flow 也通过该函数触发。 |
| `has_pending_flushsb_request()` | 只判断请求队列是否还有未消费请求。 | `flushsb_request_pending()`、debug 日志、end check 辅助判断。 |
| `try_pop_flushsb_request(req)` | consumer 统一入口，在允许 drive 时从队列弹出一个请求。 | `memblock_lsqcommit_dispatch_sequence` 每拍构造 lsqcommit xaction 时调用。 |
| `flushsb_busy()` | 判断当前是否已有 flushSb drive 到 DUT 后仍在等待 `sbIsEmpty`。 | `try_pop_flushsb_request()`、`flushsb_request_pending()`、debug/end check。 |
| `flushsb_request_pending()` | 判断 flushSb flow 是否还有未完成工作，包括队列待消费或 active waiting。 | `memblock_lsqcommit_dispatch_sequence` 的 global stop drain 条件。 |
| `mark_flushsb_driven(req, cycle)` | 记录某个请求已经随 lsqcommit xaction drive 到 DUT，并进入 waiting empty。 | `memblock_lsqcommit_dispatch_sequence` 成功设置 `io_ooo_to_mem_flushSb=1` 后调用。 |
| `update_sb_is_empty(sb_is_empty)` | 记录 ctrl monitor 采到的 `sbIsEmpty`，并在 active 请求完成时清 waiting 状态。 | ctrl monitor event adapter / common data 更新路径。 |
| `warn_flushsb_timeout_if_needed(timeout)` | 对 active flushSb 等待超时进行一次性 warning，不退出、不 fatal。 | `memblock_lsqcommit_dispatch_sequence` 每拍调用。 |

删除：

```systemverilog
function void arm_scheduled_flushsb(input int unsigned cycle);
function bit request_scheduled_flushsb_if_due(input int unsigned cycle_idx);
function bit scheduled_flushsb_pending(input int unsigned cycle_idx);
function bit should_drive_flushsb();
function bit flushsb_timed_out(input int unsigned timeout);
```

删除要求：

- 上述旧字段和旧函数不得保留 unused 兼容实现。
- `check_main_table_complete()` 不再调用 `arm_scheduled_flushsb()`。
- `memblock_lsqcommit_dispatch_sequence` 不再调用 `request_scheduled_flushsb_if_due()`、`should_drive_flushsb()` 或 `flushsb_timed_out()`。
- flow 文档不再描述 scheduled flushSb 预约路径。
- `rg -n "flushsb_pending|flushsb_scheduled|arm_scheduled_flushsb|request_scheduled_flushsb_if_due|scheduled_flushsb_pending|should_drive_flushsb|flushsb_timed_out" mem_ut/ver/ut/memblock AI_DOC/mem_ut_flow_doc` 中不应残留旧实现语义；如保留名字用于迁移说明，必须只出现在 do/undo plan 或 review 文档中。

## 4. 函数级伪代码和文字伪代码描述

本章节要求每个新增/调整函数都同时给出两类描述：

- 逻辑伪代码：接近实现代码结构，明确条件、状态更新和返回值。
- 文字伪代码描述：用中文按执行顺序解释每一步在 flow 中承担的功能，若调用子函数，需要说明子函数的作用。

### 4.1 `common_data_transaction::push_flushsb_request()`

功能：向公共 flushSb 请求队列追加一个请求。

逻辑伪代码：

```text
function push_flushsb_request(source):
  req.req_id = next_flushsb_req_id
  next_flushsb_req_id = next_flushsb_req_id + 1
  req.enqueue_cycle = memblock_sync_pkg::get_dispatch_service_cycle()
  req.source = source

  flushsb_req_q.push_back(req)
  print info(req_id, source, queue_size)
endfunction
```

文字伪代码描述：

```text
创建一个新的 flushSb 请求对象 req。
给 req 分配递增 req_id，用于后续日志和 debug 定位。
记录当前 dispatch service cycle，表示该请求是什么时候入队的。
记录调用方传入的 source，表示请求来源，例如 periodic 或 directed。
把 req 追加到 flushsb_req_q 尾部。
打印低等级 info，说明 req_id、source 和当前队列深度。
```

注意：该函数不检查 `MEMBLOCK_FLUSHSB_SEQ_EN`。是否允许周期性 producer 运行由
`flushsb_base_sequence` 自己判断。

### 4.2 `common_data_transaction::has_pending_flushsb_request()`

功能：只判断公共 flushSb 请求队列中是否还有未消费请求，不关心 active 请求是否正在等待 empty。

逻辑伪代码：

```text
function has_pending_flushsb_request():
  return flushsb_req_q.size() != 0
endfunction
```

文字伪代码描述：

```text
检查 flushsb_req_q 是否非空。
如果队列非空，说明还有请求没有被 LSQ commit sequence 消费，返回 true。
如果队列为空，说明没有待消费请求，返回 false。
该函数只看队列，不判断已经 drive 后正在等待 sbIsEmpty 的 active 请求。
```

### 4.3 `common_data_transaction::flushsb_busy()`

功能：判断是否已有一个 flushSb 请求被 drive 到 DUT，并且还在等待 `sbIsEmpty=1`。

逻辑伪代码：

```text
function flushsb_busy():
  return flushsb_waiting_empty
endfunction
```

文字伪代码描述：

```text
读取 flushsb_waiting_empty。
如果 flushsb_waiting_empty 为 1，说明已经有一个 flushSb 请求打到 DUT，当前仍在等待 sbIsEmpty 返回 1。
如果 flushsb_waiting_empty 为 0，说明当前没有 active flushSb 等待完成。
```

说明：第一版可以直接返回 `flushsb_waiting_empty`。`active_flushsb_req_valid` 用于 debug 和一致性检查，不建议替代 `flushsb_waiting_empty` 作为等待完成的唯一判断，避免 ctrl monitor 清状态路径不清晰。

### 4.4 `common_data_transaction::flushsb_request_pending()`

功能：判断 flushSb flow 是否还有未完成工作。它覆盖两类状态：队列里有请求还没消费，或者已有请求 drive 后还在等待 empty。

逻辑伪代码：

```text
function flushsb_request_pending():
  if has_pending_flushsb_request():
    return true

  if flushsb_busy():
    return true

  return false
endfunction
```

文字伪代码描述：

```text
调用 has_pending_flushsb_request：
  判断 flushSb 请求队列是否还有未消费请求。
如果队列非空：
  返回 true，表示 flushSb flow 还不能认为完成。

调用 flushsb_busy：
  判断是否存在 active flushSb 正在等待 sbIsEmpty。
如果 active 请求仍在等待：
  返回 true，表示 flushSb flow 仍有未完成工作。

如果队列为空且没有 active waiting：
  返回 false，表示 flushSb flow 已经 drain 干净。
```

说明：该函数是 LSQ commit sequence 的 drain 判断入口。`global_stop_requested=1` 后，只有该函数返回 false，LSQ commit sequence 才能退出。

### 4.5 `common_data_transaction::try_pop_flushsb_request()`

功能：从队列中取出一个可以 drive 的 flushSb 请求。

逻辑伪代码：

```text
function try_pop_flushsb_request(output req):
  if flushsb_busy():
    return false

  if issue_blocked_by_global_flush():
    return false

  if !has_pending_flushsb_request():
    return false

  req = flushsb_req_q.pop_front()
  return true
endfunction
```

文字伪代码描述：

```text
调用 flushsb_busy：
  判断当前是否已有一个 flushSb 请求 drive 后仍在等待 sbIsEmpty。
如果 busy 为 true：
  不弹出新请求，返回 false，避免多个 flushSb 请求重叠成无法对应的 active 状态。

调用 issue_blocked_by_global_flush：
  判断当前是否处于 redirect/global flush/issue freeze 这类全局恢复控制期。
如果处于全局恢复控制期：
  不弹出请求，返回 false，队列内容保留，等恢复控制解除后再消费。

调用 has_pending_flushsb_request：
  判断队列是否非空。
如果队列为空：
  返回 false。

如果以上条件都允许：
  从 flushsb_req_q 队头弹出一个请求。
  通过 output req 返回给 LSQ commit sequence。
  返回 true，表示本拍可以把 io_ooo_to_mem_flushSb 置 1。
```

### 4.6 `common_data_transaction::mark_flushsb_driven()`

功能：记录某个 flushSb 请求已经驱动到 DUT，并进入等待 `sbIsEmpty` 状态。

逻辑伪代码：

```text
function mark_flushsb_driven(req, cycle):
  active_flushsb_req = req
  active_flushsb_req_valid = 1
  flushsb_waiting_empty = 1
  flushsb_start_cycle = cycle
  flushsb_timeout_warned = 0
  last_sb_is_empty = 0
  memblock_sync_pkg::dispatch_flushsb_waiting_empty = 1
endfunction
```

文字伪代码描述：

```text
记录刚刚 drive 到 DUT 的 req，作为当前 active flushSb 请求。
置 active_flushsb_req_valid，表示 active_flushsb_req 内容有效。
置 flushsb_waiting_empty，表示后续需要等待 ctrl monitor 采到 sbIsEmpty=1。
记录 flushsb_start_cycle，用于 timeout warning 的等待时间计算。
清 flushsb_timeout_warned，表示这个新请求还没有报过 timeout warning。
清 last_sb_is_empty，避免沿用上一次 flushSb flow 的 empty 采样结果。
置 memblock_sync_pkg::dispatch_flushsb_waiting_empty，通知 ctrl monitor/adapter 在等待期间持续关注 sbIsEmpty。
```

### 4.7 `common_data_transaction::update_sb_is_empty()`

功能：由 ctrl monitor/adapter 更新 `sbIsEmpty` 采样结果，并在 active flushSb 完成时清状态。

逻辑伪代码：

```text
function update_sb_is_empty(sb_is_empty):
  last_sb_is_empty = sb_is_empty

  if flushsb_waiting_empty && sb_is_empty:
    flushsb_waiting_empty = 0
    active_flushsb_req_valid = 0
    active_flushsb_req = default
    flushsb_timeout_warned = 0
    memblock_sync_pkg::dispatch_flushsb_waiting_empty = 0
endfunction
```

文字伪代码描述：

```text
先记录 ctrl monitor/adapter 最近一次采到的 sbIsEmpty。
如果当前没有等待 active flushSb，函数只更新 last_sb_is_empty，不改变其它状态。
如果当前正在等待 active flushSb，且本次 sbIsEmpty 为 1：
  清 flushsb_waiting_empty，表示本次 flushSb flow 完成。
  清 active_flushsb_req_valid，表示 active 请求不再有效。
  清 active_flushsb_req，避免后续 debug 误读旧请求。
  清 flushsb_timeout_warned，为下一次 flushSb 请求重新开始 timeout warning 统计。
  清 dispatch_flushsb_waiting_empty，通知 raw monitor/adapter 不再需要为 flushSb waiting 强制采样 sbIsEmpty。
```

### 4.8 `common_data_transaction::warn_flushsb_timeout_if_needed()`

功能：检测 active flushSb 等待时间是否超过 timeout，只报警，不退出。

逻辑伪代码：

```text
function warn_flushsb_timeout_if_needed(timeout):
  if !flushsb_busy():
    return

  if timeout == 0:
    return

  if flushsb_timeout_warned:
    return

  age = memblock_sync_pkg::get_dispatch_service_cycle() - flushsb_start_cycle
  if age >= timeout:
    uvm_warning(req_id, source, age, timeout, last_sb_is_empty)
    flushsb_timeout_warned = 1
endfunction
```

文字伪代码描述：

```text
调用 flushsb_busy：
  判断当前是否存在 active flushSb 正在等待 sbIsEmpty。
如果没有 active waiting：
  直接返回，不检查 timeout。

检查 timeout 参数：
  如果 timeout 为 0，表示关闭 timeout warning，直接返回。

检查 flushsb_timeout_warned：
  如果当前 active 请求已经报过 timeout warning，直接返回，避免每拍重复打印同一个 warning。

计算 age：
  使用当前 dispatch service cycle 减去 flushsb_start_cycle，得到本次 active flushSb 已经等待多少 cycle。
如果 age 大于等于 timeout：
  打印一次 uvm_warning，日志中包含 active req_id、source、age、timeout 和 last_sb_is_empty。
  置 flushsb_timeout_warned，表示本次 active 请求已经报过 timeout warning。
```

## 5. `memblock_lsqcommit_dispatch_sequence` 改造

### 5.1 当前逻辑问题

当前 `send_lsqcommit_cycle()` 先处理 scheduled flushSb，再调用 `drive_flushsb_if_needed()`。
如果本拍驱动 flushSb 或正在 waiting empty，会构造 idle xaction 并 return，导致普通 LSQ commit 本拍不走。

当前逻辑伪代码：

```text
如果 scheduled flushSb 到期：
  转成 flushsb_pending；

如果 global flush 阻塞：
  发 idle xaction；
  return；

如果 should_drive_flushsb：
  单独创建 flushSb xaction；
  drive io_ooo_to_mem_flushSb=1；
  mark_flushsb_driven；

如果 did_flushsb_drive 或 flushsb_waiting_empty：
  发 idle xaction；
  return；

否则 build_lsqcommit_xaction；
驱动普通 commit；
```

### 5.2 新逻辑目标

新逻辑要求 flushSb 是本拍 lsqcommit xaction 的附加字段，不阻塞普通 commit 字段赋值。

逻辑伪代码：

```text
task send_lsqcommit_cycle(cycle_idx, output has_progress):
  has_commit = 0
  has_flushsb_progress = 0
  commit_uids.delete()

  data.warn_flushsb_timeout_if_needed(seq_csr_common::get_flushsb_timeout())

  if data.issue_blocked_by_global_flush():
    tr = create idle lsqcommit xaction
    commit_handler.clear_lsqcommit_xaction(tr)
    drive tr
    has_progress = data.flushsb_request_pending()
    return

  commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit)

  if data.try_pop_flushsb_request(req):
    tr.io_ooo_to_mem_flushSb = 1
    data.mark_flushsb_driven(req, memblock_sync_pkg::get_dispatch_service_cycle())
    has_flushsb_progress = 1

  drive tr

  if has_commit:
    commit_handler.mark_rob_commit_batch(commit_uids)

  has_progress = has_commit || has_flushsb_progress || data.flushsb_busy()
endtask
```

文字伪代码描述：

```text
每拍开始先清本拍局部状态：
  has_commit 表示本拍是否有普通 ROB/LSQ commit。
  has_flushsb_progress 表示本拍是否从 flushSb 队列弹出请求并 drive 了 flushSb pulse。
  commit_uids 记录本拍普通 commit 的 uid 列表。

调用 data.warn_flushsb_timeout_if_needed：
  检查当前 active flushSb 是否等待 sbIsEmpty 超过 MEMBLOCK_FLUSHSB_TIMEOUT。
  该函数只报一次 warning，不 fatal，不改变 sequence 退出条件。

调用 data.issue_blocked_by_global_flush：
  判断当前是否处于 redirect/global flush/issue freeze 这类全局恢复控制期。
如果被全局恢复控制阻塞：
  构造 idle lsqcommit xaction。
  调用 commit_handler.clear_lsqcommit_xaction，把 pendingPtr/commit/flushSb 字段清成 idle 值。
  drive idle xaction 到 DUT。
  不调用 try_pop_flushsb_request，因此 flushSb 队列不弹出，请求留队等待。
  不调用 build_lsqcommit_xaction，因此普通 commit 保持现有 gating，不在恢复控制期推进。
  has_progress 只根据 data.flushsb_request_pending 判断是否仍有 flushSb 未完成工作，便于 no-progress debug。
  return。

如果没有全局恢复控制阻塞：
  调用 commit_handler.build_lsqcommit_xaction。
  该函数照常选择本拍可 commit 的 uid，填写 pendingPtr/commit/deq 相关字段。
  该函数默认保持 tr.io_ooo_to_mem_flushSb=0。

调用 data.try_pop_flushsb_request：
  该函数内部会检查 flushsb_busy、issue_blocked_by_global_flush 和队列是否为空。
  如果当前正在 waiting empty，函数返回 false，不弹出新请求。
  如果队列为空，函数返回 false。
  如果允许 drive，函数从队头弹出一个 req 并返回 true。

如果 try_pop 成功：
  在同一个 lsqcommit xaction 上设置 tr.io_ooo_to_mem_flushSb=1。
  调用 data.mark_flushsb_driven，记录 active req 并进入 waiting empty。
  置 has_flushsb_progress，表示本拍 flushSb flow 有推进。

发送本拍 tr：
  tr 同时承载普通 lsqcommit 字段和可选 flushSb pulse。
  flushSb 不覆盖 pendingPtr/commit 字段。

如果 has_commit 为 true：
  调用 commit_handler.mark_rob_commit_batch(commit_uids)。
  该函数把本拍已经驱动的 commit_uids 同步到测试框架状态表，推进 ROB commit / LQ/SQ deq / success 相关状态。

最后计算 has_progress：
  如果本拍有普通 commit，认为有进展。
  如果本拍 drive 了新的 flushSb pulse，认为有进展。
  如果当前仍在等待 active flushSb 的 sbIsEmpty，说明 sequence 仍有未完成工作，也认为不是完全 idle。
```

关键约束：

- `tr.io_ooo_to_mem_flushSb=1` 不覆盖 pendingPtr/commit 字段。
- 队列为空时不赋值 flushSb，保持 `0`。
- waiting empty 时不再弹出新请求，因为 `try_pop_flushsb_request()` 会通过 `flushsb_busy()` 返回 false。
- waiting empty 只阻止新的 flushSb 出队，不阻止普通 commit；普通 commit 仍由 `build_lsqcommit_xaction()` 正常选择和驱动。
- global flush 阻塞时，普通 commit 保持现有 gating；flushSb 请求也不弹出，留在队列里等待恢复控制解除。

### 5.3 `flushsb_request_pending()` 改造

旧函数依赖 scheduled/pending/waiting 三类状态。

新函数改为：

```text
如果 data.flushsb_req_q.size() != 0：
  返回 true；
如果 data.flushsb_waiting_empty 为 1：
  返回 true；
否则返回 false；
```

该函数继续用于 LSQ commit sequence 的 global stop drain 判断：

```text
如果 global_stop_requested 且 flushsb_request_pending() 为 false：
  退出 LSQ commit loop；
```

## 6. 新增 `flushsb_base_sequence`

### 6.1 文件位置

新增文件：

```text
mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_flushsb_base_sequence.sv
```

同步更新：

```text
mem_ut/ver/ut/memblock/seq/seq_pkg.sv
mem_ut/ver/ut/memblock/seq/seq.f
```

### 6.2 sequence 职责

`flushsb_base_sequence` 只做周期性 producer：

- 读取 `seq_csr_common::get_flushsb_seq_en()`。
- 读取 `seq_csr_common::get_flushsb_request_cycle()`。
- enable 为 0 时直接退出，不启动周期 producer。
- request cycle 为 0 时不启动周期定时循环；这只表示本 sequence 不自动按周期 push，请求队列仍允许其它 producer 不定时触发。
- request cycle 非 0 时进入循环，每 N 个 clk push 一次 flushSb request。
- 退出条件使用公共 global stop。

### 6.3 文字伪代码

```text
body:
  seq_csr_common::init()；
  data = common_data_transaction::get()；
  ensure_vif()；

  如果 MEMBLOCK_FLUSHSB_SEQ_EN == 0：
    return；

  interval = MEMBLOCK_FLUSHSB_REQUEST_CYCLE；
  如果 interval == 0：
    打印 uvm_info，说明周期性 flushSb producer 未启用；
    return；

  如果 MEMBLOCK_LSQCOMMIT_SEQ_EN == 0：
    打印 uvm_info，说明 LSQ commit sequence 关闭，周期性 flushSb producer 不启动；
    return；

  wait data.main_table_ready；

  counter = 0；
  forever:
    @(posedge clk)；

    如果 data.is_global_stop_requested()：
      break；

    counter++；
    如果 counter >= interval：
      data.push_flushsb_request(FLUSHSB_SRC_PERIODIC)；
      counter = 0；
```

说明：该 sequence 不直接访问 `lsqcommit_agent` sequencer，不驱动 DUT，只往公共队列放请求。

## 7. 参数和 cfg 同步要求

保留现有 plus key：

```text
MEMBLOCK_FLUSHSB_SEQ_EN
MEMBLOCK_FLUSHSB_REQUEST_CYCLE
MEMBLOCK_FLUSHSB_TIMEOUT
```

需要修改 `seq_csr_common::validate_and_clamp()`：

1. 不再把 `flushsb_seq_en && flushsb_request_cycle != 0 && !lsqcommit_seq_en` 静默改成
   `flushsb_request_cycle=0`。
2. 不使用 `uvm_fatal`。`MEMBLOCK_LSQCOMMIT_SEQ_EN=0` 是允许配置，表示 consumer 关闭；周期性
   `flushsb_base_sequence` 运行时打印 `uvm_info` 后退出即可。
3. `flushsb_timeout=0` 可以表示关闭 timeout warning，不需要 clamp 到 1。

文字伪代码：

```text
如果 flushsb_seq_en == 1 且 flushsb_request_cycle != 0 且 lsqcommit_seq_en == 0：
  不 fatal；
  允许该配置通过；
  后续 flushsb_base_sequence 在 body 中打印 uvm_info 后退出，不产生周期性请求；

如果 flushsb_timeout == 0：
  允许，表示不做 timeout warning；
```

`default.cfg` 默认值保持：

```text
+MEMBLOCK_FLUSHSB_SEQ_EN=0
+MEMBLOCK_FLUSHSB_REQUEST_CYCLE=0
+MEMBLOCK_FLUSHSB_TIMEOUT=1000
```

## 8. end check 和状态清理

`common_data_transaction::reset_all_tables()` 需要清空：

```systemverilog
flushsb_req_q.delete();
active_flushsb_req = '{default:'0};
active_flushsb_req_valid = 1'b0;
next_flushsb_req_id = 0;
flushsb_timeout_warned = 1'b0;
```

`end_test_check()` 需要检查：

```text
flushsb_req_q.size() == 0
flushsb_waiting_empty == 0
active_flushsb_req_valid == 0
```

如果不为空，报 `uvm_error`，说明 testcase 结束时仍有 flushSb 请求未消费或未等到 empty。

## 9. 对现有 flow 文档的影响

实现后需要同步修改：

```text
AI_DOC/mem_ut_flow_doc/flushsb_test_flow.md
AI_DOC/mem_ut_flow_doc/rob_commit_lq_sq_deq_flow.md
AI_DOC/mem_ut_flow_doc/main_table_build_and_stimulus_flow.md
```

主要文档变化：

- 删除“预约 scheduled flushSb”的描述。
- 删除 `check_main_table_complete()` 中 arm scheduled flushSb 的描述。
- 新增“flushSb 请求队列 producer/consumer”描述。
- 新增 `flushsb_base_sequence` 周期性 producer flow。
- 更新 LSQ commit flow：flushSb 变成本拍 xaction 的附加字段，不再独占本拍 commit drive。

## 10. 方案收益

1. flushSb 触发入口统一。任何场景只要 push 队列即可触发，后续 fence/CBO/专用 directed testcase 都能复用。
2. flushSb 和 LSQ commit 字段可以同拍驱动，更接近接口真实使用方式。
3. 周期性 flushSb 从 LSQ commit sequence 中拆出，职责更清晰。
4. timeout 从 fatal 改成 warning，避免因为 SBuffer empty 慢导致测试框架主动退出。
5. end check 仍保留未完成状态检查，避免请求丢失或等待状态残留被忽略。

## 11. 风险和约束

### 11.1 waiting empty 期间是否继续 commit

本方案按用户要求不让 flushSb 影响 LSQ commit 字段赋值：waiting empty 期间普通 commit 仍可继续。

风险：这和真实 fence 指令“等待 SBuffer empty 后再继续后续 fence 完成”的完整 backend 行为不是同一个层次。
但当前 mem_ut 通过 `io_ooo_to_mem_flushSb` 做 directed 接口验证，本方案的目标是验证 flushSb/sbIsEmpty 闭环和
接口驱动能力，而不是完整 fence backend stall 行为。

### 11.2 global flush 期间是否允许消费队列

第一版建议不在 `issue_blocked_by_global_flush()` 为 1 时弹出 flushSb 请求。这样可以避免一次性 pulse 在
framework flush/redirect 管控期间被打出后难以追踪。请求保留在队列，解除阻塞后继续消费。

如后续要严格模拟 Fence.scala 中“flush signal must receive at any time”的行为，可以单独调整该 gating。

### 11.3 周期性 producer 和 testcase 结束

`flushsb_base_sequence` 在 `global_stop_requested` 后退出，不再产生新请求。LSQ commit sequence 继续 drain
已有队列和 waiting 状态，直到 `flushsb_request_pending()==0` 后退出。

## 12. 验证建议

代码实现后至少执行：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke plus_arg="+MEMBLOCK_FLUSHSB_SEQ_EN=1 +MEMBLOCK_FLUSHSB_REQUEST_CYCLE=20"
```

检查点：

- 编译无 `seq_pkg` include 顺序问题。
- 默认 `tc_sanity` 不产生周期性 flushSb。
- 打开 `MEMBLOCK_FLUSHSB_SEQ_EN` 且设置 interval 后，日志中可见 flushSb request 入队、消费、等待
  `sbIsEmpty`、完成。
- timeout 只报 warning，不 fatal。
- `end_test_check()` 不报 flushSb queue/waiting 残留。
