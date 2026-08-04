# DCache/Uncache 返回延迟分组控制专项 Implementation Review

| 项目 | 内容 |
|---|---|
| 关联 Plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_dcache_uncache_response_delay_control_plan_20260730.md` |
| 目标版本 | V2，`mem_ut_uvm_v2` |
| 核心源码 | `mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv` |
| 评审范围 | DCache/Uncache response queue、延迟参数、动态 sink、C reservation、D hold、文档与 TODO 同步 |
| 评审结论 | 通过；本专项代码、参数和文档与 plan 对齐，未覆盖的压力/错误注入场景保留给后续独立专项 |

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `response record` | 一条已经真实 A/C `fire`、尚未完成最后 D beat 的回复状态 | `dcache_rsp_q`、`uncache_rsp_q` | Acquire、CBO、Release 或 Uncache A.fire 后建表 |
| `current D hold` | scheduler 已选中、正在稳定驱动 D.valid/payload 的唯一 record | 两个 sequence 的 `current_d_record/current_d_valid` | D.ready=0 时保持，不重抽延迟 |
| `eligible cycle` | record 最早可参加当前通道返回仲裁的 service cycle | response record 的 `eligible_cycle` | 本拍新建 record 不能本拍返回 |
| `dynamic sink` | Grant/GrantData 与 E GrantAck 的唯一关联编号 | `sink`、`grant_ack_wait_q` | 多个 Grant 的 E.fire 按 sink 反查 owner |
| `reservation` | ReleaseData 首 C beat 为未来 ReleaseAck 占用的 response capacity | `c_assembly_response_reserved` | 16 条 record 接近满时不让两拍 C transaction 半完成 |
| `ORDERED/REORDER` | queue 选择最早到期 record 或在到期集合中随机选 record 的运行模式 | 两个 `*_RSP_REORDER_EN` | 两通道独立启用，不改变 request fire |
| `D hold watchdog` | Uncache D.valid 长期未获 D.ready 时的一次性诊断 | `service_uncache_d_hold_watchdog()` | 1000 个 driver 边界只 warning，不丢 response |

本专项只重构 memory responder 内部调度。主表、LSQ、issue、writeback、ROB commit/deq、redirect/replay、
pass/fail 和 terminal 的 owner 都没有迁移到 responder。

## 2. 修改前后对比

| 项目 | 修改前 | 修改后 | 修改原因 |
|---|---|---|---|
| DCache reply | 单一 `pending_d_*`，一次只能保存一笔 D reply | `dcache_rsp_q + current_d_record`，Grant/GrantData/CBOAck/ReleaseAck 共用 16 条 record | V2 要支持多个独立请求返回，不能让新的 A/C 覆盖旧 pending state |
| Grant sink | 固定 sink 0、单一 GrantAck owner | Allocate 动态 sink、`grant_ack_wait_q` 按 sink 等待 E.fire | 多笔 Grant 在途时必须有唯一 E 关联键 |
| DCache delay | 三档、A.fire 时直接生成 due cycle | 四档、eligible record 出现后由独立 scheduler timer 一次采样 | 支持 0/短/中/长延迟且不阻塞 A/C handshake |
| Uncache reply | A.fire 后立即生成单笔 pending D；非 Put opcode 隐式当 load | response queue、独立 timer、白名单 Put*/Get | 支持多个 record、延迟/乱序，并拒绝无效 TL opcode |
| ReleaseData | 两拍 C 完整后才尝试建立 ReleaseAck | 首 beat 预留 record，完成时原子转为 ReleaseAck | 防止容量满时已接受的两拍 C 流程无法闭环 |
| Hint | 独立单 pending 状态 | 保存在 GrantData record，最终调度选中时进入 Hint queue | REORDER 下 Hint 不会与另一条 GrantData 错配 |

## 3. 调用关系

| 顺序 | 函数/task | 在本流程中的功能 |
|---|---|---|
| 1 | `dcache_mem__access_base_sequence::body()` | 确认上一拍 A/B/C/D/E fire，调度 DCache queue，驱动下一拍 item。 |
| 2 | `accept_dcache_a_request()` / C assembly helper | 将真实 A/C fire 固化为 coherent response record。 |
| 3 | `service_dcache_response_scheduler()` | 将一条已到期 DCache record 变为 current D hold。 |
| 4 | `process_d_fire()` / `process_e_fire()` | 释放 record，或按 sink 完成 GrantAck 与 cache map 更新。 |
| 5 | `sbuffer_mem_access_base_sequence::body()` | 确认 Uncache A/D fire，建立/排空 Uncache record。 |
| 6 | `create_uncache_response_record()` | 按 Put*/Get 一次访问 shared memory，生成 AccessAck/AccessAckData record。 |
| 7 | `service_uncache_response_scheduler()` | 独立选择 Uncache record 并进入 Uncache D hold。 |

## 4. DCache Record 与动态 Sink

### 4.1 response record 替代旧 `pending_d_*`

修改前：`pending_d_valid` 和一组 `pending_d_*` 字段同时保存唯一 response，新的 A/C request
没有独立容器，不能表达多条已接受回复。

修改后：`dcache_response_record_t` 保存完整 D payload 与 lifecycle 字段，queue 和 current hold
共同构成唯一的 response capacity 真源。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
字段定义与 `get_dcache_response_count()`。

抽象功能描述：该字段组记录每条 DCache response 的协议语义、D beat 进度、capacity 和可选 Hint；
计数 helper 只报告当前容量占用，供 A/C 准入和 overflow 检查读取，不创建或释放任何 record。

```systemverilog
typedef struct {
    dcache_pending_d_kind_e kind;
    longint unsigned        eligible_cycle;
    int unsigned            beat_count;
    int unsigned            beat_idx;
    bit [5:0]               source;
    bit [9:0]               sink;
    bit [47:0]              line_addr;
    bit [1:0]               line_alias;
    bit                     hint_pending;
    bit [3:0]               hint_source_id;
    bit                     hint_isKeyword;
} dcache_response_record_t;

function int unsigned dcache_mem__access_base_sequence::get_dcache_response_count();
    return dcache_rsp_q.size() + (current_d_valid ? 1 : 0) +
           (c_assembly_response_reserved ? 1 : 0);
endfunction:get_dcache_response_count
```

中文伪代码：该结构把旧分散的 pending 字段合并为每条 record 的自包含状态。队列中的 record、当前
D hold 和 ReleaseData 的预留名额都算入同一个容量；已经完成 D 但等待 E 的 Grant 不再占这张
response 表。后续 `has_dcache_response_capacity()` 只读取这一计数，因此不存在 CBO、Release 和
Acquire 各自维护第二份容量的情况。

正确性检查：D 最后 beat fire 前 current record 仍计数；ReleaseData 首 beat 的 reservation 也计数；
二者都防止第 17 条 response 被接受。Grant 等 E 的 sink 不占 record，故不会错误阻塞 CBOAck/ReleaseAck。

### 4.2 `service_dcache_response_scheduler()`

修改前：A.fire 时直接为唯一 pending response 计算 due cycle。它无法从多条已接受 record 中选择，
也没有隔离“本拍入队”和“本拍可返回”。

修改后：scheduler 使用 `eligible_cycle`、visible queue snapshot 和独立 timer。它只移动 record，
不重新访问 memory 或修改 sink。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
函数 `service_dcache_response_scheduler()`。

抽象功能描述：该函数由 DCache `body()` 在每个 service cycle 调用；没有 current D hold 时，
它为本轮返回抽样一次 delay，到期后按 ORDERED/REORDER 将一个已到期 record 转为 current D hold。

```systemverilog
if (current_d_valid) begin
    return;
end
selected_index = find_dcache_eligible_response(current_cycle, visible_count);
if (!dcache_rsp_timer_active) begin
    if (selected_index < 0) begin
        return;
    end
    response_delay = sample_dcache_response_delay();
    dcache_rsp_timer_active    = 1'b1;
    dcache_rsp_timer_due_cycle = current_cycle + response_delay;
end
if (current_cycle < dcache_rsp_timer_due_cycle) begin
    return;
end
selected_index = find_dcache_eligible_response(current_cycle, visible_count);
if (selected_index < 0) begin
    dcache_rsp_timer_active = 1'b0;
    return;
end
selected_record = dcache_rsp_q[selected_index];
dcache_rsp_q.delete(selected_index);
current_d_record = selected_record;
current_d_valid  = 1'b1;
dcache_rsp_timer_active = 1'b0;
```

中文伪代码：如果前一条 D response 仍在等待 D.ready，scheduler 什么也不做，保证 payload 不变。
否则先只在当前拍可见且已到期的 queue 中查候选；没有 timer 时，调用 delay helper 一次并保存 due
cycle，后续周期不重复随机。timer 未到直接返回；到期后重新按当前模式选择实际 record，移动到
current D hold 并清 timer。`find_dcache_eligible_response()` 在 ORDERED 模式取最早到期 record，
在 REORDER 模式只从已到期集合随机，因此新入队 record 不会被当拍穿透返回。

正确性检查：`visible_count` 由 `body()` 在 A/C fire 前取 queue size，新增 record 不在本拍扫描范围；
timer 和 D hold 分离，D.ready=0 不会重抽 delay、重选 record 或提前释放 capacity。

### 4.3 `process_d_fire()` 与 `process_e_fire()`

修改前：最后 D.fire 只写单一 `waiting_grant_ack/pending_grant_expected_sink`；固定 sink 0 使多笔
Grant 无法区分。

修改后：最后 D.fire 将每笔 Grant 的 line/alias/sink 入队；E.fire 必须以已知 sink 唯一命中。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
函数 `process_d_fire()`、`process_e_fire()`。

抽象功能描述：`process_d_fire()` 只在真实 D.fire 推进 beat 并在最后 beat 转移 resource owner；
`process_e_fire()` 只完成已存在 GrantAck owner，不创建新的 response 或选择 scheduler record。

```systemverilog
if ((current_d_record.kind == DCACHE_PENDING_D_GRANT_DATA) &&
    ((current_d_record.beat_idx + 1) < current_d_record.beat_count)) begin
    current_d_record.beat_idx++;
    return;
end
completed_record = current_d_record;
case (completed_record.kind)
    DCACHE_PENDING_D_GRANT_DATA,
    DCACHE_PENDING_D_GRANT: begin
        grant_ack_record.line_addr  = completed_record.line_addr;
        grant_ack_record.line_alias = completed_record.line_alias;
        grant_ack_record.sink       = completed_record.sink;
        grant_ack_wait_q.push_back(grant_ack_record);
    end
endcase
clear_current_d_state();
```

中文伪代码：两拍 GrantData 的首 beat fire 只递增 beat index，record 和 sink 不释放。最后 beat 或单拍
Grant fire 时，将该 response 的物理 line、alias 和 sink 写进 GrantAck wait queue，再清 current D
hold，使 response capacity 立即归还。CBO/Release 的最后 D.fire 不进入 sink queue，由自身路径释放。

```systemverilog
if ($isunknown(dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink)) begin
    `uvm_fatal(get_type_name(), "GrantAck E.bits.sink sampled as X/Z on E.fire")
end
observed_sink = dcache_vif.drv_cb.auto_inner_dcache_client_out_e_bits_sink;
foreach (grant_ack_wait_q[i]) begin
    if (grant_ack_wait_q[i].sink == observed_sink) begin
        record_cached_line(grant_ack_wait_q[i].line_addr,
                           grant_ack_wait_q[i].line_alias);
        grant_ack_wait_q.delete(i);
        return;
    end
end
`uvm_fatal(get_type_name(),
           $sformatf("GrantAck sink=%0d does not match any pending Grant owner", observed_sink))
```

中文伪代码：E.fire 时先拒绝未知 sink，防止 `logic` 到 `bit` 的折叠误把 X/Z 当作 0。随后遍历仅最多
16 条的 GrantAck wait queue，找到相同 sink 后才把 line/alias 写入 cached line table 并删除该 owner，
此时 sink 才可复用。没有命中表示 DUT E response 与当前 response lifecycle 不一致，立即 fatal 而不
静默删除任何 record。

正确性检查：D record capacity 在最后 D.fire 释放、sink 在 E.fire 释放，两层资源没有重复释放或
相互阻塞；`record_cached_line()` 只在真实 GrantAck 之后调用，维持旧 Probe 候选表的时序语义。

### 4.4 ReleaseData reservation 与 Hint 绑定

抽象功能描述：ReleaseData reservation 保证 C 两拍接收和未来 ReleaseAck capacity 一起成立；Hint
绑定保证 REORDER 下 sideband 与最终 GrantData 同属一条 record。这两个机制不改变 shared-memory
写入时机和 GrantAck 生命周期。

```systemverilog
if (!has_dcache_response_capacity()) begin
    `uvm_fatal(get_type_name(),
               "ReleaseData C.fire occurred without a reservable ReleaseAck response slot")
end
c_assembly_response_reserved = 1'b1;
consume_c_beat(c_req_xact, accept_cycle);

// ReleaseData completion
c_assembly_response_reserved = 1'b0;
enqueue_dcache_response(response_record);
```

中文伪代码：ReleaseData 首 C.fire 在开始 assembly 前先确认有一条可用 D response 容量，并立即置
reservation。第二 beat 完整收齐后，先完成原有 overlay write/line 删除逻辑，再先清 reservation、
随后入队 ReleaseAck record；这两个动作在同一 task 内完成，计数从“预留”原子转换为“实际 record”，
不会短暂占用两条容量，也不会丢失已接受的 C transaction。

```systemverilog
if (current_d_record.hint_pending) begin
    hint_record.due_cycle = current_cycle;
    hint_record.source_id = current_d_record.hint_source_id;
    hint_record.isKeyword = current_d_record.hint_isKeyword;
    dcache_hint_q.push_back(hint_record);
    current_d_record.hint_pending = 1'b0;
end
```

中文伪代码：只有 scheduler 真正把一条 GrantData 变为 current D hold 时，才读取该 record 的 Hint
字段、写入 Hint queue 并清 pending 标记。`service_hint()` 在同一轮从 queue 消费一次；因此
REORDER 的最终 D response 与 Hint source/isKeyword 一致，D.ready backpressure 也不会重复发送 Hint。

## 5. Uncache Response Pipeline

### 5.1 `decode_uncache_a_opcode()` 与 `create_uncache_response_record()`

修改前：`is_store_opcode()` 只判断 opcode 0/1，其它所有 opcode 都误入 load 分支，且只存在一个
立即生成的 pending D。

修改后：白名单先决定 response kind，shared memory 只在真实 A.fire 时访问一次，结果保存为 queue
record。错误注入不在本专项混入。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
函数 `decode_uncache_a_opcode()`、`create_uncache_response_record()`。

抽象功能描述：decode 函数只验证并分类 A opcode；create task 在 capacity 可用时完成一次 memory
读/写并建立 future D response。它们不启动 timer，也不在 D hold 时重做访问。

```systemverilog
case (req_xact.auto_inner_buffers_out_a_bits_opcode)
    UNCACHE_A_OPCODE_PUT_FULL,
    UNCACHE_A_OPCODE_PUT_PARTIAL: return UNCACHE_RESPONSE_STORE_ACK;
    UNCACHE_A_OPCODE_GET:         return UNCACHE_RESPONSE_LOAD_DATA;
    default: begin
        `uvm_fatal(get_type_name(),
                   $sformatf("unsupported Uncache A opcode=%0d source=%0d address=0x%0h size=%0d param=%0d",
                             req_xact.auto_inner_buffers_out_a_bits_opcode,
                             req_xact.auto_inner_buffers_out_a_bits_source,
                             req_xact.auto_inner_buffers_out_a_bits_address,
                             req_xact.auto_inner_buffers_out_a_bits_size,
                             req_xact.auto_inner_buffers_out_a_bits_param))
    end
endcase
```

中文伪代码：只允许 V2 Uncache 实际 producer 会输出的 PutFullData、PutPartialData 和 Get。Put 类返回
STORE_ACK，Get 返回 LOAD_DATA；其它 TileLink opcode 没有本 responder 的合法语义，立即 fatal，
不建立 record、不写 memory，也不会伪装成 load。

```systemverilog
response_kind = decode_uncache_a_opcode(req_xact);
sbuffer_mem_access_task(req_xact.auto_inner_buffers_out_a_bits_address,
                        response_kind == UNCACHE_RESPONSE_STORE_ACK,
                        req_xact.auto_inner_buffers_out_a_bits_mask,
                        req_xact.auto_inner_buffers_out_a_bits_data,
                        corrupt, denied, load_data);
response_record.kind           = response_kind;
response_record.eligible_cycle = accept_cycle + 1;
response_record.accept_cycle   = accept_cycle;
response_record.denied         = denied;
response_record.data           = (response_kind == UNCACHE_RESPONSE_STORE_ACK) ? '0 : load_data;
response_record.corrupt        = (response_kind == UNCACHE_RESPONSE_STORE_ACK) ? 1'b0 : corrupt;
uncache_rsp_q.push_back(response_record);
```

中文伪代码：真实 A.fire 后先得到固定的读写类别，再调用一次 shared-memory helper。store 只进入
Uncache write batch，并建立数据为 0 的 AccessAck；load 固化当前 committed merged data，建立
AccessAckData。record 最早下一 service cycle 可选；D hold、timer 和重排只移动这份已固定 payload，
不会重复写 overlay 或重新读 memory。D-error 专项以后只在此处覆写 denied/corrupt 一次。

### 5.2 `service_uncache_response_scheduler()` 与 watchdog

抽象功能描述：scheduler 将到期 Uncache record 转为唯一 D hold；watchdog 仅诊断 hold 过久，二者
不影响 DCache queue 或全局完成判断。

```systemverilog
if (current_d_valid) begin
    return;
end
selected_index = find_uncache_eligible_response(current_cycle, visible_count);
if (!uncache_rsp_timer_active) begin
    if (selected_index < 0) begin
        return;
    end
    response_delay = sample_uncache_response_delay();
    uncache_rsp_timer_active    = 1'b1;
    uncache_rsp_timer_due_cycle = current_cycle + response_delay;
end
if (current_cycle < uncache_rsp_timer_due_cycle) begin
    return;
end
selected_record = uncache_rsp_q[selected_index];
uncache_rsp_q.delete(selected_index);
current_d_record = selected_record;
current_d_valid  = 1'b1;
```

中文伪代码：没有 current Uncache D hold 时，先在本拍前可见的 eligible queue 中查候选。没有运行
timer 就按 Uncache 四档权重抽一次 delay；timer 未到只等待，到期后按 ORDERED/REORDER 取出一条
record，成为唯一 current D hold。D.ready=0 时这个函数下一拍立即返回，保持 payload；D.fire 后
`process_uncache_d_fire()` 才清 hold、归还 capacity。

`service_uncache_d_hold_watchdog()` 在 current D hold 已经由上一 item 驱动且持续未 fire 时递增本地
计数，1000 个边界打印一次 warning。它不释放 record、不把长 backpressure 变成 pass/fail，也不影响
global stop 的 drain 条件。

## 6. 参数、文档与 TODO 同步检查

已同步：

- `env/plus.sv`、`seq_csr_common.sv`、`seq/plus_cfg/default.cfg`：DCache/Uncache 四档 delay 与
  两个 reorder enable；四档全零由 `validate_and_clamp()` fail-fast。
- `cfg/memblock_compile_params.svh` 与 `memblock_dispatch_types.sv`：两个固定 16 的
  `MEMBLOCK_DUT_*_MAX_OUTSTANDING` 宏与 typed localparam；没有 runtime plus 镜像。
- `tc_dispatch_real_l2cache_model.cfg`：显式补齐 zero、Uncache delay 和 reorder 配置。
- active flow、`mem_base_sequence.md`、`plus.md`、V2 compile parameter audit 和参数管理规则：
  已替换旧的单 pending/固定 sink/即时 Uncache 描述。
- TODO：移除已实现的 request-bound Hint responder TODO；只保留 `io_l2_flush_done` completion TODO。

## 7. 验证结果

| 命令 | 结果 | 覆盖含义 |
|---|---|---|
| `make eda_run tc=basicTest ts=virtual_base_sequence mode=dcache_rsp_delay_verify_20260804 partcmp_op=off` | `TEST_PASS`，`UVM_ERROR=0`，`UVM_FATAL=0` | 当前源码全量 VCS 编译、参数初始化和环境启动通过；该 vseq 本身为空，不把它当作 responder 功能覆盖。 |
| `make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq cfg=tc_dispatch_real_l2cache_model mode=dcache_rsp_delay_verify_20260804 partcmp_op=off` | `TEST_PASS`，`UVM_ERROR=0`，`UVM_FATAL=0` | 真实 dispatch smoke 启动 DCache responder；日志出现 `DCache responder draining complete` 和 `published terminal idle`，证明 DCache request/response/stop drain 主链路闭环。 |

第二条日志显示 `cached_lines=1` 后自然 drain；默认 cfg 中 response delay 和 reorder 参数被命令行展开。
本次 smoke 的主表规模为 1，因此它不宣称覆盖 16 条容量满、REORDER 随机次序、多个 GrantAck sink、
Uncache 多 outstanding 或 D.ready 1000-cycle watchdog；这些是后续 directed/stress 覆盖项，而不是
本次通过日志已经证明的功能。

## 8. Plan 对齐检查

### 8.1 与 Plan 一致的实现

- DCache/Uncache 分离 queue、timer、D hold、ORDERED/REORDER 和 16 条 compile-time capacity 已落实；
- DCache Grant sink 与 response capacity 分层，最后 D.fire 释放 record、E.fire 释放 sink 已落实；
- ReleaseData reservation、Uncache opcode 白名单、Uncache D hold warning、global-stop drain 已落实；
- 参数链、preset、flow/analysis 和 TODO 已同步。

### 8.2 实现与 Plan 不一致项

无未记录的不一致项。以下三项均已写入关联 plan 的 `IMPLEMENTATION_DELTA`，并在实现中按该差异执行：

| 项目 | Plan 原有逻辑 | 当前实现 | 处理结论 |
|---|---|---|---|
| outstanding 配置 | 早期参数段同时出现 plus 与 compile 描述 | 固定为 compile macro/typed localparam，不建立 plus 镜像 | 有意修正，避免第二权威。 |
| Uncache error helper | 早期正文引用尚不存在的 `apply_uncache_d_error_injection()` | 当前 record 保留 backend 值，D-error 专项在同一创建点后续接入 | 有意拆分，保证本专项独立编译。 |
| Hint 时机 | 未定义 REORDER 下 timer 候选和最终 D 的绑定 | 仅最终 record 变为 current D hold 时发 Hint | 有意修正，避免跨 record Hint。 |

#### 8.2.1 outstanding 配置的源码支撑

源码位置：`mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh` 与
`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`。

抽象功能描述：这组定义提供 DCache/Uncache response capacity 的唯一编译期权威；它们只决定
responder 能接受多少已 fire 的 memory reply，不构成 testcase runtime 随机参数。

```systemverilog
`ifndef MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING
    `define MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING 16
`endif
`ifndef MEMBLOCK_DUT_UNCACHE_MAX_OUTSTANDING
    `define MEMBLOCK_DUT_UNCACHE_MAX_OUTSTANDING 16
`endif

localparam int unsigned MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING =
    `MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING;
localparam int unsigned MEMBLOCK_DUT_UNCACHE_MAX_OUTSTANDING =
    `MEMBLOCK_DUT_UNCACHE_MAX_OUTSTANDING;
```

中文伪代码：编译时先给两个 V2 固定结构值默认 16，再由 typed localparam 导出给 scheduler 使用。
没有 `plus.sv` 字段、cfg key 或 runtime getter 能重写这两个上限，因此 testcase 不能把物理 in-flight
能力伪装成可变参数；容量检查只读这里导出的值。

#### 8.2.2 Uncache error helper 拆分的源码支撑

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
函数 `create_uncache_response_record()`。

抽象功能描述：当前专项固定 response kind 和 backend 返回值，提供后续 D-error plan 的唯一接入点；
当前函数不持有 error injection 权重，也不在 scheduler/D hold 中重新采样。

```systemverilog
response_record.denied  = denied;
response_record.data    = (response_kind == UNCACHE_RESPONSE_STORE_ACK) ? '0 : load_data;
response_record.corrupt = (response_kind == UNCACHE_RESPONSE_STORE_ACK) ? 1'b0 : corrupt;
uncache_rsp_q.push_back(response_record);
```

中文伪代码：shared-memory helper 返回的 denied/corrupt 在 record 创建时保存。store 的 AccessAck
协议不携带 corrupt，因此固定为 0；load 保留 backend 的 corrupt。随后把 record 入队，后续 timer、
重排和 D hold 只移动这份固定值。D-error plan 可以在同一位置插入一次归一化/注入，而无需碰 scheduler。

#### 8.2.3 Hint 绑定的源码支撑

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
函数 `service_dcache_response_scheduler()`。

抽象功能描述：该分支只在 scheduler 的最终选择已经确定后，将 Hint 从对应 GrantData record 转移到
单拍 sideband queue；它不重新选择 record 或改变 D payload。

```systemverilog
if (current_d_record.hint_pending) begin
    hint_record.due_cycle = current_cycle;
    hint_record.source_id = current_d_record.hint_source_id;
    hint_record.isKeyword = current_d_record.hint_isKeyword;
    dcache_hint_q.push_back(hint_record);
    current_d_record.hint_pending = 1'b0;
end
```

中文伪代码：timer 到期并把最终 record 移入 current D hold 后，才检查该 record 是否有待发 Hint。
命中时复制 source/isKeyword 到 Hint queue 并清本条 record 标记，确保下一轮不会重复发送；这使
REORDER 的 Hint 仍然只属于当前 D response。

### 8.3 Plan 未说明但 Coding 落实的细节

- `process_e_fire()` 对 E sink 做 `$isunknown()` 检查，再进行队列匹配，防止未知值折叠为 sink 0；
- response scheduler 通过 `visible_count` 抑制本拍新入队 record 当拍被选择；
- `global stop` 额外等待 Hint queue、scheduler timer、C reservation 和 GrantAck wait 收敛。

这些是实现 plan 所要求的“唯一 owner、当拍不可返回、D hold 稳定和自然 drain”时需要的最小防御性
细节，不改变主表或 LSQ 主体控制。

## 9. 结论与剩余边界

本专项通过。DCache/Uncache responder 现在具备独立、可配置且不改变主框架控制流的 response
调度能力；DCache 多笔 Grant 使用动态 sink 闭环，Uncache 不再把非法 opcode 静默当 load。

仍待后续专项处理：D-error 注入、multi Probe/toB、CBO probe closure、alias conflict、L2 flush
completion，以及多 outstanding/reorder/capacity-full 的 directed stress testcase。它们不得以恢复旧
`pending_d_*` 或固定 sink 逻辑的方式实现。
