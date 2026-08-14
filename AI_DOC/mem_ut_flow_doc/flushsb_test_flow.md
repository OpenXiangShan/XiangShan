# `flushSb` 请求、发送确认与 `sbIsEmpty` 完成 Flow（V2）

本文描述当前 `mem_ut_uvm_v2` 测试框架中 `flushSb` 的真实调用链。它同时覆盖普通 periodic/directed 请求与 CSR/SFence/`check_store` 控制屏障使用的 owner 请求。控制屏障的完整状态机见归档后的 `csr_sfence_check_store_rob_control_coding_plan_20260813.md`；本文只说明 `flushSb` 这一段如何入队、交付、回采和被控制 service 消费。

## 1. 专有名词与对象职责

| 名词 | 当前语义与代码落点 | 示例 |
|---|---|---|
| `flushSb` request | `memblock_flushsb_req_t`，位于 `common_data_transaction::flushsb_req_q`。producer 只入队，LSQ commit sequence 是唯一出队并驱动者。 | periodic producer 的无 owner request。 |
| owner request | `owner_valid=1` 的 request，携带 `uid + dynamic_epoch + action_generation + kind`。其完成不能直接写控制状态表。 | SFence 或 `check_store` 的 SBuffer 清空请求。 |
| attached | request 已从 FIFO pop 并附加到待发送 `lsqcommit` xaction，由 `mark_flushsb_request_attached_to_lsqcommit_xaction()` 记录。它不是 driver sendover。 | `io_ooo_to_mem_flushSb=1` 已写入 `tr`，但尚未返回 `finish_item()`。 |
| driver sendover | `finish_item()` 返回后由 `mark_flushsb_request_driver_sendover()` 记录的接口交付边界。只有此后才允许消费 `sbIsEmpty`。 | request 保存当前 latest observation 序号作为 freshness baseline。 |
| `sbIsEmpty` observation | ctrl monitor 每个有效 sample 发布的 owner-neutral latest level，含递增 observation 序号。deferred raw 也冻结同一序号。 | sendover 前已经为高的 level 不能完成新 request。 |
| completed slot | `flushsb_completed`，仅保存一笔 owner request 的完成事实。control service 按 `req_id + owner` 取得后立即清除。 | SFence service 将其转成 fence action token。 |
| normal request | `owner_valid=0` 的既有请求。达到新鲜 `sbIsEmpty=1` 时直接清 active 状态，不进入 completed slot。 | `memblock_flushsb_base_sequence` 的周期请求。 |

## 2. 函数调用 Flow 图

```mermaid
flowchart TD
    A[普通 producer: push_flushsb_request\n或 control service: push_owner_flushsb_request] --> B[common_data_transaction.flushsb_req_q]
    B --> C[memblock_lsqcommit_dispatch_base_sequence.send_lsqcommit_cycle]
    C --> D{try_pop_flushsb_request}
    D -- 否: busy/flush/empty --> C
    D -- 是 --> E[tr.io_ooo_to_mem_flushSb = 1]
    E --> F[mark_flushsb_request_attached_to_lsqcommit_xaction]
    F --> G[start_item / finish_item]
    G --> H[mark_flushsb_request_driver_sendover]
    H --> I[ctrl monitor mon_data\npublish_control_sb_is_empty_observation\npush raw ctrl]
    I --> J[dispatch_monitor_event_adapter.apply_raw_ctrl_deq]
    J --> K[lsq_commit_handler.apply_raw_ctrl_deq]
    K --> L[common_data_transaction.update_sb_is_empty(raw)]
    L --> M{owner_valid}
    M -- 否 --> N[清 active request]
    M -- 是 --> O[flushsb_completed]
    O --> P[control barrier service\ntry_consume_control_flushsb_completion]
    P --> Q[SFence token 或 check_store L2 ASSERT]
```

### 函数调用 Flow 图整体文字伪代码

```text
1. producer 阶段：
  普通 producer 调用 push_flushsb_request(source)，仅创建无 owner FIFO 项。
  SFence/check_store service 调用 push_owner_flushsb_request(owner, request)，
  由公共数据对象分配 req_id 并把 owner 写入 FIFO 项；此时不驱动 DUT，也不触发专用 flushSb event。

2. LSQ commit consumer 阶段：
  send_lsqcommit_cycle 先执行原有 global flush/redirect gating 和 normal commit 构造。
  try_pop_flushsb_request 只有在没有 attached/active request、没有全局阻塞且 FIFO 非空时才 pop。
  consumer 把 flushSb=1 附加到同一 lsqcommit xaction，并记录 attached。
  start_item/finish_item 完成后才记录 driver sendover，冻结 sbIsEmpty latest observation 序号并打开 capture gate。

3. monitor 与 deferred raw 阶段：
  ctrl monitor 在控制 reset ack 之后的每个有效 sample 发布 sbIsEmpty latest observation；
  raw ctrl 同时携带本 sample 的 immutable observation 序号。
  adapter 先完成既有 semantic/raw 仲裁，再将可消费 raw 交给 lsq_commit_handler。

4. 完成阶段：
  update_sb_is_empty 只接受 sendover 后、observation 序号更大且 level=1 的 raw。
  无 owner request 直接结束；owner request 把 req_id、owner 和 observation 写入 completed slot。
  control service 只消费属于当前 active control owner 的 completed slot：
  SFence 写 persistent fence token；check_store 进入 L2 ASSERT；其他 request 不会误推进控制屏障。
```

## 3. producer 入队

### `push_flushsb_request()` 与 `push_owner_flushsb_request()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`

抽象功能描述：

两个函数是 `flushSb` 的唯一公共 producer API。它们只分配 request id、填写 FIFO 元数据并入队，不驱动 interface，也不修改 `sbIsEmpty` waiting 状态。

真实逻辑摘要：

```systemverilog
req.req_id = next_flushsb_req_id;
req.enqueue_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
req.owner_valid = 1'b1;
req.owner = owner;
next_flushsb_req_id++;
flushsb_req_q.push_back(req);
```

输入/输出：

- 输入：普通 API 的 `source`，或 owner API 的有效 `memblock_control_owner_t`。
- 输出：写入 `flushsb_req_q`；owner API 同时返回完整 request，使 service 归档其 `req_id`。

文字伪代码：

```text
普通 API：
  分配单调 req_id，记录 source 和 enqueue_cycle，设置 owner_valid=0 后入 FIFO。

owner API：
  校验 owner 有效；若仍有 attached/active/completed owner request 或 FIFO 中已有 owner request，fatal。
  分配 req_id，复制 owner，设置 owner_valid=1 后入 FIFO，并返回该 request。
  该全局单 owner 约束保证一个 sbIsEmpty 完成不会在两个 control marker 之间产生归属歧义。
```

## 4. LSQ commit consumer

### `try_pop_flushsb_request()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`

抽象功能描述：

该函数是 FIFO 的唯一 pop gate。它不构造 xaction，只保证 attached、sendover waiting 和 global recovery 期间不会取出第二笔 request。

文字伪代码：

```text
将输出 request 清零。
若 attached request、active request 或 waiting-empty 存在：返回 false。
若 issue_blocked_by_global_flush 为真：返回 false，FIFO 保持不变。
若 FIFO 为空：返回 false。
否则 pop FIFO 队头，返回该 request 和 true。
```

### `send_lsqcommit_cycle()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqcommit_dispatch_base_sequence.sv`

抽象功能描述：

该 task 在不改变普通 ROB commit 构造方式的前提下，把最多一笔可发送的 `flushSb` request 附加到当前 `lsqcommit` xaction，并在准确的 sequence/driver 边界记录 attached 与 sendover。

真实逻辑摘要：

```systemverilog
if (data.try_pop_flushsb_request(flushsb_req)) begin
    tr.io_ooo_to_mem_flushSb = 1'b1;
    data.mark_flushsb_request_attached_to_lsqcommit_xaction(flushsb_req, cycle);
    has_flushsb_progress = 1'b1;
end
start_item(tr);
finish_item(tr);
if (has_flushsb_progress) begin
    data.mark_flushsb_request_driver_sendover(flushsb_req, cycle);
end
```

文字伪代码：

```text
先执行既有 global flush/redirect 检查；若阻塞，发送 idle xaction，不 pop FIFO。
按原有 handler 构造 normal/fault/control ROB commit 的 lsqcommit 字段。
调用 try_pop_flushsb_request：成功时仅在当前 xaction 加上 flushSb=1，随后记录 attached。
调用 start_item/finish_item：这是 xaction 交付到 driver 并收到 item_done 的边界。
若本 item 附带 flushSb，调用 sendover helper；它才允许后续 sbIsEmpty raw 完成本 request。
普通 commit 与 flushSb 可以同拍存在；flushSb 不创建第二个 lsqcommit driver 或第二种 commit transaction。
```

## 5. attached、sendover 与新鲜度

### `mark_flushsb_request_attached_to_lsqcommit_xaction()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`

抽象功能描述：

该 helper 只把已 pop request 放入 attached 槽，并记录诊断时间。它不置 `flushsb_waiting_empty`，因此不允许 sendover 前的旧 high level 结束 request。

### `mark_flushsb_request_driver_sendover()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`

抽象功能描述：

该 helper 在 `finish_item()` 返回后把 attached request 转为 active request，冻结当前 `sbIsEmpty` observation 序号作为新鲜完成下界，并打开 ctrl monitor capture gate。

真实逻辑摘要：

```systemverilog
void'(memblock_sync_pkg::get_latest_control_sb_is_empty_observation(observation));
active_flushsb_req = attached_flushsb_req;
active_flushsb_req.sb_is_empty_observation_seq_at_sendover = observation.observation_seq;
attached_flushsb_req_valid = 1'b0;
active_flushsb_req_valid = 1'b1;
flushsb_waiting_empty = 1'b1;
memblock_sync_pkg::dispatch_flushsb_waiting_empty = 1'b1;
```

文字伪代码：

```text
校验输入 req 与 attached 槽的 req_id 一致，且没有 active/waiting request；不一致 fatal。
读取 latest sbIsEmpty observation，即使当前没有有效 observation 也只得到零序号 baseline。
把 attached request 移到 active 槽，并保存 observation_seq_at_sendover。
清 attached，置 active 和 waiting-empty；通知 ctrl monitor 后续 raw capture 需要持续关注 sbIsEmpty。
此刻只证明 driver 已完成接口交付，不证明 DUT 已完成 SBuffer 清空。
```

## 6. monitor 采样与 raw 消费

### `io_mem_to_ooo_ctrl_agent_agent_monitor::mon_data()`

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv`

抽象功能描述：

ctrl monitor 在 reset handshake 完成后持续采样 DUT 的 `io_mem_to_ooo_sbIsEmpty`。它只发布 owner-neutral observation/raw，不理解 UID、ROB 或当前控制状态。

真实逻辑摘要：

```systemverilog
if (control_reset_ack_sample) begin
    memblock_sync_pkg::ack_control_ctrl_monitor_reset(control_reset_epoch);
end else begin
    memblock_sync_pkg::publish_control_sb_is_empty_observation(
        io_mem_to_ooo_sbIsEmpty, sample_seq);
end
```

文字伪代码：

```text
当前 control reset epoch 的首个有效 sample 仅发布 ctrl monitor ack，不发布可消费 observation。
ready 之后的每个有效 sample 调用 publish_control_sb_is_empty_observation：
  覆盖 latest level，递增 observation_seq，并把同一序号冻结到 raw ctrl。
raw 可因 LQ/SQ resync 延后处理，但 immutable observation 序号不会被后续 latest level 覆盖。
```

### `dispatch_monitor_event_adapter::apply_raw_ctrl_deq()` 与 `lsq_commit_handler::apply_raw_ctrl_deq()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`、`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`

抽象功能描述：

adapter 负责先完成已有 MMIO/raw 规范化，再把 full raw 委托给 commit handler。commit handler 首先处理 `sbIsEmpty`，随后继续现有 LQ/SQ deq 与 head 同步逻辑。

真实逻辑摘要：

```systemverilog
apply_raw_ctrl_mmio_tags(raw);
return monitor_commit_handler.apply_raw_ctrl_deq(raw);

data.update_sb_is_empty(raw);
// 后续仍按既有逻辑预检并处理 LQ/SQ deq。
```

文字伪代码：

```text
adapter 先规范化 raw 的 MMIO 标签；这不消耗 sbIsEmpty 语义。
commit handler 把完整 raw 交给 update_sb_is_empty，因此该函数可以读取 frozen observation 序号。
若 LQ/SQ raw 因 resync 尚不能消费，deferred FIFO 保留队首并在后续 service tick 重试；
update_sb_is_empty 的调用仍只按 active request、sendover baseline 和 level 选择是否完成。
```

## 7. `update_sb_is_empty()` 与控制 service 消费

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`、`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_control_barrier_service.sv`

抽象功能描述：

`update_sb_is_empty(raw)` 是唯一将 raw level 转为 request completion 的函数。它不直接更新 `status_transaction`；owner completion 由 control service 依据当前 active owner 取走。

真实逻辑摘要：

```systemverilog
if (flushsb_waiting_empty && raw.sb_is_empty &&
    raw.sb_is_empty_observation_seq >
        active_flushsb_req.sb_is_empty_observation_seq_at_sendover) begin
    if (active_flushsb_req.owner_valid) begin
        flushsb_completed.valid = 1'b1;
        flushsb_completed.req_id = active_flushsb_req.req_id;
        flushsb_completed.owner = active_flushsb_req.owner;
    end
    flushsb_waiting_empty = 1'b0;
    active_flushsb_req_valid = 1'b0;
end
```

文字伪代码：

```text
先记录最近一次 sbIsEmpty level 供 timeout/debug 使用。
若没有 sendover waiting、level 不是 1，或 raw observation 序号不新于 sendover baseline：不完成 request。
满足三个条件时：
  若 request 无 owner，直接清 active/waiting 状态。
  若 request 有 owner，先检查 completed slot 为空；写入 req_id、owner、observation，再清 active/waiting 状态。
control service 调用 try_consume_control_flushsb_completion(owner, req_id)：
  SFence 将状态推进到 SFENCE_REQ，并先入 sfence_control_action_q 再触发 action event。
  check_store 将状态推进到 CHECK_STORE_L2_CSR_ASSERT，随后等待独立 L2 flush flow。
  owner 或 req_id 不匹配只返回 false，不改变当前控制状态。
```

## 8. 边界、退出与端到端行为

| 条件 | 行为 |
|---|---|
| global flush/redirect/issue freeze | `try_pop_flushsb_request()` 返回 false，request 保留 FIFO。 |
| attached 或 active request 存在 | 不 pop 第二笔 request；控制 owner completion slot 也必须先被 service 消费。 |
| sendover 前已有 `sbIsEmpty=1` | 不完成新 request；只有更大的 observation 序号才能通过。 |
| deferred raw 暂不能应用 | raw 保留队首重试；其 frozen observation 序号保持不变。 |
| timeout | `warn_flushsb_timeout_if_needed()` 只发一次 warning，不清 request，不伪造完成。 |
| global stop | LSQ commit sequence 和 runtime drain 仍等待 FIFO、attached/active request 与 owner completed slot 全部收敛。 |

端到端行为总结：

```text
普通 request：
  producer -> flushsb_req_q -> LSQ commit attached -> driver sendover
  -> 新鲜 sbIsEmpty raw -> update_sb_is_empty -> active 清除。

SFence request：
  control service owner request -> attached -> sendover -> 新鲜 sbIsEmpty raw
  -> completed(req_id, owner) -> SFence service -> persistent fence token -> C0/C4 flow。

check_store request：
  control service owner request -> attached -> sendover -> 新鲜 sbIsEmpty raw
  -> completed(req_id, owner) -> check_store service -> L2 ASSERT/hold/done/release flow。
```

端到端文字伪代码描述：

```text
所有 producer 都只入同一个 FIFO，因此接口 owner 始终是 LSQ commit sequence。
attached 与 sendover 分离后，queue 被消费不再被误当成 DUT 已收到 pulse；
sendover baseline 又排除了 request 发出前的空缓冲高电平。
ctrl monitor 只发布事实，commit handler 只把 raw 交给公共 request 生命周期；
control service 最后才按 owner 消费完成事实，所以 monitor 不需要知道 UID/ROB，
periodic/directed 请求也不会解除 CSR/SFence/check_store 的屏障。
```
