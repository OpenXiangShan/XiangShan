# mem_ut V2 L2TLB Response 与生命周期适配 Implementation Review

| 项目 | 内容 |
|---|---|
| 关联 Plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_response_permission_adapt_execution_plan_20260708.md` |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| 核验基线 | `cf63e12ebd00db93524edc35c1fab646b6a48e31` 的 staged snapshot |
| Review 日期 | 2026-07-23 |
| Review 范围 | L2TLB responder request/response、runtime CSR/flush sideband、参数、driver、G/U 字段链与文档同步 |
| 不属于本轮 | 独立 S1/S2 PTE 权限、RM/checker/coverage、L2Cache/PTW memory 下游模型 |

## 1. 范围与术语

### 1.1 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `lifecycle owner` | 唯一维护 L2TLB 请求账本和 ready/response 调度的 sequence 实例 | `l2tlb_lifecycle_owner_*`、`try_claim/release_l2tlb_lifecycle_owner()` | legacy default sequence 与显式 vseq 不能并发各建一份队列 |
| `token` | 每次真实 request fire 的测试框架动态实例编号，不进入 DUT payload | `memblock_l2tlb_pending_req::request_token` | 相同 lookup key 连续 fire 两次仍生成两个 token |
| `pending` | 已被 DUT 接受但尚未放到 response 端口的 request | `pending_q` | 延迟尚未到期或等待 ordered head |
| `driving` | response 已写入本拍 cycle item，等待下一 DUT sample 确认完成的唯一 request | `driving_req/driving_valid` | 不能在选择 response 时提前从 outstanding 账本删除 |
| `outstanding` | pending 与 driving 的总数 | `outstanding_count()` | queue-full ready backpressure 和最终 drain |
| `due sample` | 某 token 最早允许完成 response 的 sample 序号 | `due_sample_seq` | 1C/MID/LONG 只定义最早边界，拥塞可更晚 |
| `runtime snapshot` | CSR monitor 发布的不可破坏 latest 视图 | `runtime_csr_snapshot` | request fire 时冻结 ASID/VMID/权限上下文 |
| `flush event` | CSR changed 或 sfence monitor 发布的 L2TLB 生命周期版本事件 | `l2tlb_flush_event_seq` | 取消旧 pending，并暂停 ready 4 个 sample |
| `cycle item` | sequence 每个 driver 边界交付的一拍 ready/response transaction | `L2tlb_agent_agent_xaction` | gap 必须为 0，driver 只搬运字段 |
| `semantic raw` | dispatch service 消费的 CSR/sfence 语义事件视图 | `latest_raw_csr/raw_sfence_q` | 与 L2TLB latest sideband共享原始事实，但不能被 responder pop |
| `lifecycle block` | reset、CSR freshness 或 flush hold 导致 responder 暂时不能接受新 request 的状态 | `csr_snapshot_valid`、`accept_hold_until_sample`、reset 分支 | block 期间 ready=0，idle-stop 不应伪造退出 |
| `ready opportunity` | lifecycle block 解除后至少向 DUT 发送一拍可接受 `ready=1` 的机会，不等同于真实 fire | `ready_opportunity_since_lifecycle_block` | flush hold 结束且 `idle_stop=1` 时先发一拍 ready |
| `freshness` | sideband 的 sample 时间/序号仍对应当前 service 边界，未被迟到事件污染 | `flush_sample_time`、`runtime_csr_snapshot_seq` | active ready已开放后看到旧 flush 时间必须 fatal |
| `hold` | flush event 后到 DUT filter 清空前关闭 ready/response 的保护窗口 | `accept_hold_until_sample`、`MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES` | V2 顶层观测点到 filter 清空共 4 个 sample |

### 1.2 Review 结论边界

本轮实现保持 `DTLB -> L2TLB_agent request` 和 `L2TLB_agent -> DTLB response` 方向，没有接管顶层 `io_l2_tlb_req_*` 或构造 L2Cache/PTW memory 下游模型。实现新增多 outstanding 与回复调度，属于功能逻辑新增；G/U 信号搬运属于字段适配。

S1 与 S2 的 response 接口字段已经分别驱动，但两组 `perm_g/u` 仍读取同一份 `memblock_tlb_entry.pte_g/pte_u`。独立二阶段 PTE/权限建模继续是 TODO，本 review 不宣称该能力已经完成。

## 2. Request 采样与 Outstanding 生命周期

### 2.1 修改前逻辑

旧 `send_l2tlb_cycle()` 只判断 request valid，先发 ready item，再把随机延迟写入 response transaction 的 `pre_pkt_gap`。driver 在 gap 期间仍可能保持 ready，但 sequence 阻塞在当前 item，无法记录后续真实 request fire。旧实现没有 per-fire token、pending queue、driving slot 或 accepted/completed/canceled 守恒。

### 2.2 修改后逻辑

抽象功能描述：`send_l2tlb_cycle()` 在一个 driver sample 中完成 request snapshot、上一 response 确认、CSR/flush 同步、新 request 接受、下一 response 选择和 cycle item 发送；它是 responder 状态机的唯一 service 入口，不负责 DUT 数据正确性比较。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`，task `send_l2tlb_cycle()`。

```systemverilog
task memblock_l2tlb_base_sequence::send_l2tlb_cycle(output bit has_progress,
                                                    output bit should_exit);
    longint unsigned flush_event_seq;
    time flush_sample_time;
    bit flush_event_valid;
    bit new_flush_event;
    bit request_killed;
    bit response_selected;
    bit hold_active;
    bit lifecycle_blocked;
    bit next_ready;
    L2tlb_agent_agent_xaction cycle_tr;
    memblock_sync_pkg::dispatch_raw_csr_t ignored_runtime_csr;
    int unsigned latest_runtime_csr_seq;

    has_progress = 1'b0;
    should_exit = 1'b0;
    sampled_req_valid = (l2tlb_vif.drv_cb.io_ptw_req_0_valid === 1'b1);
    sampled_req_ready = (l2tlb_vif.mon_cb.io_ptw_req_0_ready === 1'b1);
    sampled_req_vpn = l2tlb_vif.drv_cb.io_ptw_req_0_bits_vpn;
    sampled_req_s2xlate = l2tlb_vif.drv_cb.io_ptw_req_0_bits_s2xlate;

    uvm_wait_for_nba_region();
    memblock_sync_pkg::get_latest_l2tlb_flush_event(flush_event_seq,
                                                    flush_sample_time,
                                                    flush_event_valid);

    if (l2tlb_vif.rst_n !== 1'b1 ||
        memblock_sync_pkg::reset_backend_done !== 1'b1) begin
        cancel_outstanding_by_reset();
        acceptance_opened_since_reset = 1'b0;
        ready_opportunity_since_lifecycle_block = 1'b0;
        csr_snapshot_valid = 1'b0;
        if (!require_post_reset_csr_refresh) begin
            void'(memblock_sync_pkg::get_latest_runtime_csr_snapshot(
                ignored_runtime_csr, latest_runtime_csr_seq));
            reset_runtime_csr_seq_baseline = latest_runtime_csr_seq;
            require_post_reset_csr_refresh = 1'b1;
        end
        accept_hold_until_sample = 0;
        idle_count = 0;
        stopping = data.is_global_stop_requested();
        if (flush_event_valid) begin
            last_seen_flush_event_seq = flush_event_seq;
        end
        cycle_tr = create_l2tlb_xaction($sformatf("l2tlb_reset_idle_%0d", sample_seq));
        send_l2tlb_item(cycle_tr);
        should_exit = stopping;
        return;
    end

    if (flush_event_valid && flush_event_seq < last_seen_flush_event_seq) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB flush event sequence moved backwards: last=%0d latest=%0d",
                             last_seen_flush_event_seq, flush_event_seq))
    end
    new_flush_event = flush_event_valid &&
                      flush_event_seq > last_seen_flush_event_seq;
    if (new_flush_event && acceptance_opened_since_reset &&
        flush_sample_time != $time) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("stale/future L2TLB flush event before lifecycle mutation: event_seq=%0d sample_time=%0t current_time=%0t",
                             flush_event_seq, flush_sample_time, $time))
    end

    if (driving_valid) begin
        complete_driving_response();
        has_progress = 1'b1;
    end

    drain_csr_runtime_events();

    request_killed = 1'b0;
    if (new_flush_event) begin
        void'(handle_l2tlb_flush_event(flush_event_seq,
                                       flush_sample_time,
                                       request_killed));
        has_progress = 1'b1;
    end

    if (request_fire() && !request_killed) begin
        if (!csr_snapshot_valid) begin
            `uvm_fatal(get_type_name(), "L2TLB request fired before first runtime CSR snapshot")
        end
        void'(capture_fired_request());
        has_progress = 1'b1;
    end

    if (data.is_global_stop_requested()) begin
        stopping = 1'b1;
    end

    hold_active = sample_seq < accept_hold_until_sample;
    lifecycle_blocked = !csr_snapshot_valid || hold_active;
    if (has_progress || lifecycle_blocked || stopping || outstanding_count() != 0 ||
        !acceptance_opened_since_reset ||
        !ready_opportunity_since_lifecycle_block) begin
        idle_count = 0;
    end else begin
        idle_count++;
        if (idle_count >= idle_stop_cycle) begin
            stopping = 1'b1;
            `uvm_info(get_type_name(),
                      $sformatf("L2TLB responder idle-stop at sample=%0d idle_count=%0d",
                                sample_seq, idle_count),
                      UVM_LOW)
        end
    end

    cycle_tr = null;
    response_selected = 1'b0;
    if (csr_snapshot_valid && !hold_active) begin
        response_selected = select_due_response(sample_seq + 1, cycle_tr);
        if (response_selected) begin
            has_progress = 1'b1;
        end
    end
    if (cycle_tr == null) begin
        cycle_tr = create_l2tlb_xaction($sformatf("l2tlb_cycle_%0d", sample_seq));
    end

    next_ready = !stopping && csr_snapshot_valid && !hold_active &&
                 outstanding_count() < max_outstanding;
    cycle_tr.io_ptw_req_0_ready = next_ready;
    cycle_tr.pre_pkt_gap = 0;
    cycle_tr.post_pkt_gap = 0;
    if (next_ready) begin
        acceptance_opened_since_reset = 1'b1;
    end
    if (hold_active && cycle_tr.io_ptw_resp_valid) begin
        `uvm_fatal(get_type_name(), "flush hold attempted to drive an L2TLB response")
    end

    send_l2tlb_item(cycle_tr);
    if (next_ready) begin
        ready_opportunity_since_lifecycle_block = 1'b1;
    end
    if (stopping && outstanding_count() == 0) begin
        if (cycle_tr.io_ptw_req_0_ready || cycle_tr.io_ptw_resp_valid) begin
            `uvm_fatal(get_type_name(), "L2TLB stop exit requires a final inactive cycle item")
        end
        should_exit = 1'b1;
    end
endtask:send_l2tlb_cycle
```

中文伪代码：

```text
该逻辑负责按固定顺序推进一个完整 responder sample，并把同一 sample 的 DUT request payload 与实际 ready 配成唯一 fire 事实。
先清本拍输出标志，从 drv_cb input sample 锁存 valid、vpn、s2xlate，再从 mon_cb input sample 锁存上一 cycle item 的实际 ready；等待 NBA 后读取非破坏性的 flush latest。
若 reset 或 backend 尚未就绪，先调用 cancel_outstanding_by_reset 把 pending/driving token归入reset-canceled；每个reset窗口只在首次blocked sample记录CSR序号基线，随后保持该基线；再吸收flush序号、发送inactive item并立即返回。
正常路径先验证flush序号单调和sample时间新鲜度；这两项检查发生在response完成、CSR应用、queue删除或新token建立之前，非法sideband直接fatal且不留下部分状态变更。
校验通过后，先调用complete_driving_response确认上一拍已驱动response，再调用drain_csr_runtime_events应用可用的最新CSR；随后处理新flush，删除旧pending并识别同拍被kill的fire。
若锁存的valid与ready均为1且本拍fire未被flush取消，先检查CSR gate，再调用capture_fired_request创建token、冻结上下文并放入pending_q；该helper增加accepted_count和outstanding，不更新pass/fail/terminal。
之后锁存global stop，并按真实进展、CSR/hold阻塞、stop、outstanding以及本次阻塞后是否已提供ready机会维护idle计数；只有已经重新开放过ready且完全空闲达到阈值才进入stopping。
CSR有效且不在hold时调用select_due_response，最多选择一笔到期pending进入driving slot；无到期response时创建全清零transaction。
根据stopping、CSR gate、hold和outstanding容量计算下一拍ready，强制两个gap为0；生成ready=1时同时记录reset后曾开放和本次生命周期阻塞后已提供ready机会；hold期间若仍出现resp_valid立即fatal。
最后调用send_l2tlb_item发送唯一cycle item；只有stopping且outstanding已清空、并确认本拍item的ready和resp_valid均为0后，才置should_exit结束owner循环。
```

正确性检查：

- ready 与 request payload来自同一时钟 sample，避免下一拍 ready 污染当前 fire。
- 只有 `valid && ready` 才创建 token，valid level 不会被重复接收。
- response 选择后仍保存在 driving slot，直到下一 sample 才转入 completed。
- queue 满时下一 cycle ready 拉低，不允许接受第 `MAX_OUTSTANDING+1` 笔。

抽象功能描述：`check_l2tlb_lifecycle_accounting()` 只读累计计数和两个 outstanding 容器，验证每个已接受 token 都有唯一归属；它不修改 queue、DUT 接口或公共主表。

源码位置：同文件，function `check_l2tlb_lifecycle_accounting()`。

```systemverilog
accounted_count = completed_count + flush_canceled_count +
                  reset_canceled_count + outstanding_count();
if (accepted_count != accounted_count) begin
    `uvm_fatal(get_type_name(),
               $sformatf("L2TLB lifecycle mismatch context=%s accepted=%0d completed=%0d flush_canceled=%0d reset_canceled=%0d pending=%0d driving=%0d accounted=%0d",
                         audit_context, accepted_count, completed_count,
                         flush_canceled_count, reset_canceled_count,
                         pending_q.size(), driving_valid, accounted_count))
end
```

中文伪代码：

```text
该逻辑负责 responder 内部 token 守恒审计。
把已完成、flush取消、reset取消、当前pending和当前driving相加。
如果总数不等于accepted_count，立即fatal并输出各分类计数。
该函数没有子调用产生状态副作用，只通过outstanding_count读取容器大小。
```

调用关系：

| 调用者 | 被调用者 | 功能 |
|---|---|---|
| `send_l2tlb_cycle()` | `capture_fired_request()` | 为真实 fire 冻结 CSR/key/entry/response 并入队 |
| `capture_fired_request()` | `get_or_create_tlb_entry_by_req()` | 查询或创建 live by-key entry |
| `capture_fired_request()` | `memblock_tlb_entry::copy_from()` | 建立 request-time 不可变 entry 快照 |
| request/flush/reset/complete helper | `check_l2tlb_lifecycle_accounting()` | 每个状态转换后检查 token 守恒 |

## 3. Response 调度、延迟与 Driver

### 3.1 修改前逻辑

旧参数是 `MIN/MAX_LATENCY` 连续区间，driver 使用 `pre_pkt_gap` 阻塞等待。该方案既不能表达三档权重，也使 driver 同时拥有延迟和 idle ready 行为，无法支持多 outstanding。

### 3.2 修改后逻辑

抽象功能描述：`choose_latency()` 为每个 token选择最早 due 档位；`select_due_response()` 在到期 token 中按 ordered/reorder 策略选择一笔进入 driving slot。两者不确认 response 完成，完成动作只发生在下一 sample 的 `complete_driving_response()`。

源码位置：`memblock_l2tlb_base_sequence.sv`，function `choose_latency()`。

```systemverilog
function int unsigned memblock_l2tlb_base_sequence::choose_latency(
    output memblock_l2tlb_latency_bucket_e bucket);
    if (!std::randomize(bucket) with {
            bucket dist {
                L2TLB_LATENCY_1C   := resp_1c_wt,
                L2TLB_LATENCY_MID  := resp_mid_wt,
                L2TLB_LATENCY_LONG := resp_long_wt
            };
        }) begin
        `uvm_fatal(get_type_name(), "failed to randomize L2TLB response latency bucket")
    end
    case (bucket)
        L2TLB_LATENCY_1C:   return 1;
        L2TLB_LATENCY_MID:  return resp_mid_latency;
        L2TLB_LATENCY_LONG: return resp_long_latency;
        default: begin
            `uvm_fatal(get_type_name(), "randomized invalid L2TLB latency bucket")
            return 1;
        end
    endcase
endfunction:choose_latency
```

中文伪代码：

```text
该逻辑负责给每个request确定最早response边界并选择下一拍唯一response。
先用UVM/SystemVerilog dist按三个配置权重选择1C、MID或LONG。
把档位转换为1、mid_latency或long_latency并计算due_sample。
默认保序模式只检查pending_q头；未到期时后项不能越过。
reorder模式收集所有已到期索引，再均匀随机一项。
命中项从pending_q移动到driving_req，outstanding总数不变。
global stop后即使开启reorder也改为按头排空，保证退出确定性。
```

抽象功能描述：`select_due_response()` 从已到期的 pending token 中选择下一笔 response，按 ordered/reorder 和 stopping 状态决定是否允许越过队头，并把选中 token 转入 driving slot；它不完成 response，也不更新 UID 状态。

源码位置：`memblock_l2tlb_base_sequence.sv`，function `select_due_response()`。

```systemverilog
if (stopping || !resp_reorder_en) begin
    if (pending_q[0].due_sample_seq > next_sample_seq) begin
        return 1'b0;
    end
    selected_index = 0;
end else begin
    foreach (pending_q[idx]) begin
        if (pending_q[idx].due_sample_seq <= next_sample_seq) begin
            eligible_indices.push_back(idx);
        end
    end
    if (eligible_indices.size() == 0) begin
        return 1'b0;
    end
    eligible_count = eligible_indices.size();
    if (!std::randomize(choice) with {
            choice < eligible_count;
        }) begin
        `uvm_fatal(get_type_name(), "failed to randomize eligible L2TLB response index")
    end
    selected_index = eligible_indices[choice];
end
if (pending_q[selected_index].accept_flush_event_seq != last_seen_flush_event_seq) begin
    `uvm_fatal(get_type_name(),
               $sformatf("selected stale L2TLB token=%0d accept_event=%0d current_event=%0d",
                         pending_q[selected_index].request_token,
                         pending_q[selected_index].accept_flush_event_seq,
                         last_seen_flush_event_seq))
end
driving_req = pending_q[selected_index];
pending_q.delete(selected_index);
driving_valid = 1'b1;
cycle_tr = driving_req.resp_tr;
if (cycle_tr == null) begin
    `uvm_fatal(get_type_name(), "selected L2TLB pending record has null response transaction")
end
check_l2tlb_lifecycle_accounting("response_select");
return 1'b1;
```

中文伪代码：

```text
该逻辑负责从已到期pending中确定下一笔response，并把它移入唯一driving slot。
若正在stop或未开启乱序，先检查队头due是否已经到达；未到达就返回0，不越过队头。
若允许乱序，遍历pending队列收集所有due不晚于下一sample的索引；没有候选就返回0，有候选则用随机索引选一笔。
检查选中token的accept flush epoch仍等于当前epoch；不一致说明旧token未被flush清理，立即fatal。
把选中record复制到driving_req，从pending_q删除，置driving_valid，并取出保存的response transaction；transaction为空时fatal。
最后调用check_l2tlb_lifecycle_accounting核对token仍在pending或driving分类中，然后返回已选择。
```

抽象功能描述：`L2tlb_agent_agent_driver::main_phase()` 每个 `drv_cb` 边界先检查 lifecycle owner。
没有 owner 时驱动 inactive；owner 已声明时阻塞等待当前边界必须交付的一个 gap=0 cycle
item 并逐字段驱动。它不维护 latency、queue 或 stop，也不改写 owner。

源码位置：`mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv`。

```systemverilog
@this.vif.drv_mp.drv_cb;
if (!memblock_sync_pkg::l2tlb_lifecycle_owner_claimed) begin
    this.drive_idle(this.cfg.drv_mode);
end
else begin
    req = null;
    seq_item_port.get_next_item(req);
    if (req == null) begin
        `uvm_fatal(get_type_name(), "active L2TLB lifecycle owner returned a null cycle item")
    end
    if (req.pre_pkt_gap != 0 || req.post_pkt_gap != 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB cycle item requires gap=0, got pre=%0d post=%0d",
                             req.pre_pkt_gap, req.post_pkt_gap))
    end
    this.send_pkt(req);
    seq_item_port.item_done();
end
```

中文伪代码：

```text
该逻辑负责一拍一次的VIF字段搬运和无owner时的接口收敛。
每个driver clocking边界先读取l2tlb_lifecycle_owner_claimed；为0时调用drive_idle，ready、resp_valid和payload保持inactive。
owner为1时清空req句柄并阻塞get_next_item；sequence在service loop前claim、最终inactive item完成后release，因此该区间的每个边界都必有item。
空item或非零gap立即fatal；合法item调用send_pkt驱动ready、resp_valid和全部S1/S2 payload，然后item_done。
sequence disabled或自然退出后owner为0，driver不进入阻塞item握手。
```

参数链：

```text
memblock_compile_params.svh
  -> MEMBLOCK_DUT_L2TLB_DFILTER_SIZE / FLUSH_HOLD_CYCLES
  -> memblock_dispatch_types typed localparam

env/plus.sv
  -> seq_csr_common load/validate/apply_runtime_resource_limits/getter
  -> memblock_l2tlb_base_sequence::configure_from_plus()
```

`memblock_compile_params.svh` 中带反引号的同名对象是编译期宏；
`memblock_dispatch_types.sv` 随后把它们转换成 `int unsigned` typed localparam。
sequence 和 `seq_csr_common` 中写成不带反引号的
`MEMBLOCK_DUT_L2TLB_DFILTER_SIZE`、`MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES`
引用的是 typed localparam，不是绕过类型层直接消费宏。

旧 `MEMBLOCK_L2TLB_MIN_LATENCY/MAX_LATENCY` 已从代码和 default cfg 删除。`MAX_OUTSTANDING=0` fatal，超过 compile filter size 时统一 clamp；MID 必须大于 1，LONG 必须大于 MID，三个权重不得全 0。

## 4. Runtime CSR 与 Flush 生命周期

### 4.1 修改前逻辑

旧 L2TLB sequence 通过 dispatch adapter 读取受 `dispatch_monitor_capture_en` 控制的 semantic CSR latest。legacy default responder 在主表 flow尚未打开 capture gate时可能拿不到首份 CSR；同时旧实现没有 non-destructive flush event，无法取消已排队 request。

### 4.2 修改后逻辑

抽象功能描述：CSR monitor无条件维护 post-reset runtime latest；semantic raw仍保留原 capture gate。`publish/get_latest_runtime_csr_snapshot()` 提供不破坏读取，两个 consumer最终调用同一 `apply_raw_csr_runtime()`，由统一 sequence幂等去重。

源码位置：`csr_ctrl_agent_agent_monitor.sv`、`memblock_sync_pkg.sv`。

```systemverilog
runtime_payload_changed =
    !has_last_runtime_csr ||
    memblock_sync_pkg::raw_csr_payload_changed(last_runtime_csr, raw_csr);
memblock_sync_pkg::publish_runtime_csr_snapshot(raw_csr,
                                                 runtime_payload_changed);
if (memblock_sync_pkg::dispatch_monitor_capture_en)
    memblock_sync_pkg::push_raw_csr(raw_csr);
```

中文伪代码：

```text
该逻辑负责把CSR level状态同时服务给L2TLB lifecycle和dispatch semantic consumer。
post-reset每拍构造同一个raw_csr事实。
与monitor私有baseline比较；首次或payload变化时刷新runtime latest并递增统一seq。
只有semantic capture gate打开时才刷新dispatch raw latest。
两个路径不复制CSR字段解释；consumer都调用common_data的apply helper，同一seq只应用一次。
```

抽象功能描述：`handle_l2tlb_flush_event()` 取消新 event 之前接受但尚未 driving 的 pending token，记录同拍旧 ready形成的 killed fire，并建立 V2 顶层 monitor 到 DTLB filter 的 4-sample ready hold。它不 pop semantic sfence queue，也不回滚已经在当前边界完成的 driving response。

源码位置：`memblock_l2tlb_base_sequence.sv`。

```systemverilog
int unsigned drop_count;

drop_count = 0;
for (int idx = int'(pending_q.size()) - 1; idx >= 0; idx--) begin
    if (pending_q[idx].accept_flush_event_seq < event_seq) begin
        pending_q.delete(idx);
        drop_count++;
    end
end
flush_canceled_count += drop_count;
last_seen_flush_event_seq = event_seq;
accept_hold_until_sample =
    sample_seq + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES;
ready_opportunity_since_lifecycle_block = 1'b0;
if (acceptance_opened_since_reset && event_sample_time == $time && request_fire()) begin
    record_flush_killed_request(event_seq, event_sample_time);
    request_killed = 1'b1;
end
```

中文伪代码：

```text
该逻辑负责把CSR changed或sfence变成L2TLB request生命周期取消边界。
从pending_q尾部向前扫描并删除accept event早于新event的token；每次删除递增drop_count，循环结束后一次性累加flush_canceled_count。
更新本地last_seen event，把ready关闭到当前sample加compile hold，并清除本次阻塞后的ready机会标志。
若本sample使用旧ready又真实fire，为该fire分配token并直接归入flush canceled。
不删除driving项，因为上一cycle response已经在当前sample先完成。
不消费raw_sfence_q，dispatch semantic失效仍由原owner处理。
```

优先级为：reset最高；post-reset先检查 flush event freshness，再确认上一 driving response，再应用 CSR latest，再处理 flush/current fire，最后调度下一 cycle item。active ready开放后若首次看到的 event时间不是当前 sample，状态变化前 fatal，避免错误重锚。

## 5. Permission 字段与 UID 回填

### 5.1 G/U 字段链

抽象功能描述：`fill_dtlb_resp_from_entry()` 把 request-time entry snapshot转换为完整 response payload；driver和connect只逐层搬运，不重新解释 permission。

源码位置：`memblock_l2tlb_base_sequence.sv`，function `fill_dtlb_resp_from_entry()`。

```systemverilog
resp.io_ptw_resp_bits_s1_entry_perm_g = entry.pte_g;
resp.io_ptw_resp_bits_s1_entry_perm_u = entry.pte_u;
resp.io_ptw_resp_bits_s2_entry_perm_g = entry.pte_g;
resp.io_ptw_resp_bits_s2_entry_perm_u = entry.pte_u;
```

中文伪代码：

```text
该逻辑负责把冻结entry中的G/U位分别写入S1和S2接口字段。
xaction保存两个阶段的独立端口字段。
driver把两组字段驱到interface，active connect再force到V2内部PTW response线。
当前两组值来自同一pte_g/pte_u，不等于已经实现独立S1/S2权限模型。
独立二阶段PTE、directed GPF/GAF和stage2 legal leaf继续归TODO。
```

完整链为：

```text
memblock_tlb_entry.pte_g/pte_u
  -> fill_dtlb_resp_from_entry()
  -> L2tlb_agent_agent_xaction
  -> L2tlb_agent_agent_driver::send_pkt()
  -> L2tlb_agent_agent_interface
  -> L2tlb_agent_connect active takeover
  -> _inner_ptw_io_tlb_1_resp_bits_s1/s2_entry_perm_g/u
```

### 5.2 UID record 回填

修改前零匹配会产生 `uvm_error`，把合法 prefetch/无 UID request误判为 testcase failure。修改后 `complete_driving_response()` 在真实 response sample 后才调用 `update_uid_tlb_records_by_entry()`；有匹配则更新，零匹配仅输出 `UVM_LOW`，token仍正常完成。

该调整不提前设置 pass/fail/terminal，也不把“零 UID 匹配”变成 DUT checker 豁免。存在 active UID 且 key匹配时，原有 PTE-ready和 `MEMBLOCK_STATUS_TLB_MAPPED` 更新保持不变。

## 6. Owner、Reset 与退出

package级 `try_claim/release_l2tlb_lifecycle_owner()` 防止 default sequence和显式 vseq并发维护两份 queue。DUT reset只把 pending/driving归入 `reset_canceled_count`，不释放 owner；自然退出前必须满足 token守恒且 outstanding为0。

`global_stop_requested` 关闭新 ready但继续排空已接受 token。idle-stop只在 CSR已有效、没有 flush hold、没有 outstanding、本拍无进展且本次 lifecycle block 后已经提供过 ready opportunity 时累计；命中后先发送 `ready=0/resp_valid=0` 的最终 cycle item，再 release owner。强制 kill后在同一仿真重新 handoff不在本轮支持范围。

active L2TLB driver默认固定为 `DRV_0`：

- `memblock_env_cfg::post_randomize()` 建立公共默认值。
- `tc_sanity` 显式设置 `drv_mode=DRV_0`、`xz_sw=OFF`。
- testcase/user cfg主动覆盖为其它 pattern mode时，driver reset phase仍 fail-fast。

## 7. 实现与 Plan 不一致项

### 7.1 Ready 采样 clocking block

Plan 原有逻辑：每个 service sample 锁存 request valid、VPN、`s2xlate` 和“实际 ready”，但未指定 ready 的 clocking view。

当前源码逻辑：request payload 从 `drv_cb` input sample 读取，ready 从同边界 `mon_cb` input sample 读取；`request_fire()` 只消费这四个快照。

不一致原因：`drv_cb.io_ptw_req_0_ready` 是 driver output view，读取它可能看到当前 delta 已写入的下一 cycle item，而不是 DUT 在本边界采样的 ready。改用 `mon_cb` 只收紧采样合同，不改变容量、flush 或 stop 算法。

抽象功能描述：该采样逻辑在每个 service sample 建立唯一 request fire 事实，避免 NBA 后 live VIF 变化把不同拍的 valid/payload 与 ready 拼在一起；它不创建 token，token 仍由 `capture_fired_request()` 建立。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`，`send_l2tlb_cycle()` 与 `request_fire()`。

```systemverilog
sampled_req_valid = (l2tlb_vif.drv_cb.io_ptw_req_0_valid === 1'b1);
sampled_req_ready = (l2tlb_vif.mon_cb.io_ptw_req_0_ready === 1'b1);
sampled_req_vpn = l2tlb_vif.drv_cb.io_ptw_req_0_bits_vpn;
sampled_req_s2xlate = l2tlb_vif.drv_cb.io_ptw_req_0_bits_s2xlate;

function bit memblock_l2tlb_base_sequence::request_fire();
    return sampled_req_valid && sampled_req_ready;
endfunction:request_fire
```

中文伪代码：

```text
该逻辑负责冻结同一个DUT采样边界的request握手事实。
先从drv_cb的输入视图读取valid、vpn和s2xlate，再从mon_cb的输入视图读取DUT实际看到的ready；后续NBA等待和flush/CSR处理都只读取这些保存值。
request_fire把已保存的valid和ready相与；只有两者都为1才返回真实握手，valid持续保持不会被单独当作新请求。
该函数不修改queue或counter；调用方在返回1且未被flush kill时才调用capture_fired_request建立token。
```

处理结论：保持当前实现，并已回写 Plan 的 `IMPLEMENTATION_DELTA`；无需用户确认。

### 7.2 Reset 后 runtime CSR freshness

Plan 原有逻辑：reset 清本地 CSR gate，但没有定义 package latest 在 reset 中不清除时，如何区分 reset 前旧快照和 reset 后新快照。

当前源码逻辑：每个 reset 窗口只在首个 blocked sample 保存 `runtime_csr_snapshot_seq` 基线；reset 释放后，只有 latest seq 大于该基线才重新置 `csr_snapshot_valid=1`。

不一致原因：直接复用 package latest 会让 responder 用 reset 前 ASID/VMID 重新开放 ready；清 package latest 又会破坏其它 consumer 的 non-destructive 读取。序号门槛是局部且最小的修复。

抽象功能描述：reset freshness gate 防止 responder 在 mid-test reset 后复用 reset 前 CSR 快照重新开放 ready；它不清公共 latest，也不改变 semantic raw consumer。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`，`drain_csr_runtime_events()` 与 reset 分支。

```systemverilog
if (!require_post_reset_csr_refresh) begin
    void'(memblock_sync_pkg::get_latest_runtime_csr_snapshot(
        ignored_runtime_csr, latest_runtime_csr_seq));
    reset_runtime_csr_seq_baseline = latest_runtime_csr_seq;
    require_post_reset_csr_refresh = 1'b1;
end
if (require_post_reset_csr_refresh &&
    raw_csr_seq <= reset_runtime_csr_seq_baseline)
    return;
csr_snapshot_valid = 1'b1;
require_post_reset_csr_refresh = 1'b0;
```

中文伪代码：

```text
每个reset窗口的首个blocked sample读取package当前latest的序号作为baseline，并关闭本地CSR-ready gate；
后续reset sample看到require_post_reset_csr_refresh已置位，不再覆盖该baseline，避免把reset期间的新snapshot反复吸收成新门槛；
reset释放后每次读取latest时比较序号；如果没有超过baseline，保持inactive并继续处理flush/global stop；
monitor首个post-reset sample发布更高序号后，apply_raw_csr_runtime更新公共CSR状态；
随后清除refresh gate，下一次cycle才可能派生ready；等待期间不累计idle-stop；
该逻辑只保护responder恢复时序，不清理runtime latest，也不改主表状态。
```

处理结论：保持当前实现，并已回写 Plan 的 `IMPLEMENTATION_DELTA`；reset 后动态 freshness 仍需 directed testcase 覆盖。

### 7.3 Active driver 默认配置

Plan 原有逻辑：active responder 拒绝非 `DRV_0`，但未指定公共 cfg 和 `tc_sanity` 的默认写者。

当前源码逻辑：`memblock_env_cfg::post_randomize()` 将 L2TLB agent 固定为 `DRV_0`、关闭 X/Z 检查；`tc_sanity::build_phase()` 也显式覆盖为相同值。driver reset phase仍对用户后续错误覆盖 fail-fast。

不一致原因：首轮验证随机得到 `DRV_X` 后在 0ns 命中合同 fatal。只修 `tc_sanity` 会让普通 `tc_base` 继续暴露同一随机配置缺口。

抽象功能描述：公共 cfg 片段为 active responder 建立确定的 idle 基线，不生成 request/response；testcase 片段保证 bring-up smoke 不被 generic pattern mode 污染。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_env_cfg.sv`，`post_randomize()`。

```systemverilog
this.u_L2tlb_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON;
this.u_L2tlb_agent_agent_cfg.drv_sw = tcnt_dec_base::ON;
this.u_L2tlb_agent_agent_cfg.mon_sw = tcnt_dec_base::ON;
this.u_L2tlb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
this.u_L2tlb_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
```

中文伪代码：

```text
该配置逻辑负责建立所有普通testcase共享的L2TLB agent默认状态。
打开sequencer、driver和monitor，关闭不适用于bring-up idle端口的X/Z扫描，并把driver模式固定为全零idle。
后续user cfg仍可覆盖这些字段；若把active responder改成非DRV_0，driver reset phase会按协议合同fatal。
```

源码位置：`mem_ut/ver/ut/memblock/tc/src/tc_sanity.sv`，`build_phase()`。

```systemverilog
sanity_cfg.u_L2tlb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
sanity_cfg.u_L2tlb_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
```

中文伪代码：

```text
该testcase逻辑负责让tc_sanity的reset和bring-up驱动保持确定。
显式关闭L2TLB X/Z检查并选择DRV_0，避免该testcase随机覆盖公共默认值后制造无owner的ready或response。
这两个赋值不启用L2TLB sequence，也不改变其它agent的配置。
```

处理结论：保持当前实现，并已回写 Plan；错误的显式用户配置仍由 driver 拒绝。

### 7.4 Reset 后首次 ready 的 idle-stop 边界

Plan 原有逻辑：idle counter 在 CSR 有效、无 outstanding/progress 时即可累计，没有保证 reset 后至少先发一拍 `ready=1`。

当前源码逻辑：`acceptance_opened_since_reset=0` 时强制清 idle counter；合法 `next_ready=1` item 生成后才置该标志。`idle_stop_cycle=1` 因此也会先给 DUT 一次 admission 机会。

不一致原因：idle-stop 是被动 responder 的辅助收敛条件，不应让 responder 在第一次公开 capability 前退出。

抽象功能描述：该边界保护 reset 后第一次可接受周期；它只延后 idle-stop，不预先接受 request，也不修改 pending/driving 账本。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`，`send_l2tlb_cycle()`。

```systemverilog
if (has_progress || lifecycle_blocked || stopping ||
    outstanding_count() != 0 || !acceptance_opened_since_reset ||
    !ready_opportunity_since_lifecycle_block) begin
    idle_count = 0;
end else begin
    idle_count++;
end

if (next_ready) begin
    acceptance_opened_since_reset = 1'b1;
end
send_l2tlb_item(cycle_tr);
if (next_ready)
    ready_opportunity_since_lifecycle_block = 1'b1;
```

中文伪代码：

```text
该逻辑负责防止reset后的首次可接受周期被idle-stop抢先关闭。
如果本拍有进展、CSR或hold仍阻塞、已经stop、还有outstanding，或者reset后历史上还没有发送过ready，就清零idle_count。
只有所有阻塞解除且已经提供过ready机会时，才允许idle_count增加。
本拍最终计算出next_ready为1时先记录reset后曾开放ready；调用send_l2tlb_item完成UVM握手后，才记录本次生命周期阻塞后已提供ready机会；下一sample才可能按idle阈值退出。
outstanding_count只读取pending与driving数量，不修改账本；next_ready仍由stop、CSR、hold和容量共同决定。
```

处理结论：保持当前实现。已有 direct `tc_sanity` 验证了 startup 首次 ready 后再 idle-stop，但未覆盖真实 request fire。

### 7.5 Flush hold 后重新开放 ready 的 idle-stop 边界

Plan 原有逻辑：`acceptance_opened_since_reset` 在 flush 中保持为 1，Plan 没有单独描述“本次 flush hold 后是否已经重新提供 ready 机会”。

当前源码逻辑：新增 `ready_opportunity_since_lifecycle_block`。reset 和每次新 flush event清零；idle 判断在该标志恢复前不累计；本拍真正生成 `next_ready=1` 后才置位。

不一致原因：若只使用 `acceptance_opened_since_reset`，`idle_stop_cycle=1` 会在 hold 结束的首个 sample 先置 stopping，DUT 在 hold 期间保持的 valid永远不能 fire。该历史标志还承担 active flush event freshness，不能在 flush 时直接清零。

抽象功能描述：新增字段只记录 reset/flush 这类生命周期阻塞解除后是否向 DUT 提供过一次 request admission 机会；它不表示 request 已 fire，也不参与 flush event 新鲜度判断。

源码位置：同文件，字段定义与 `initialize_lifecycle_state()`。

```systemverilog
bit ready_opportunity_since_lifecycle_block;

ready_opportunity_since_lifecycle_block = 1'b0;
```

中文伪代码：

```text
该字段保存“最近一次reset或flush阻塞之后，是否已经生成过一拍可接受ready”。
sequence新建时初始化为0；该值不是token状态，也不进入DUT payload。
字段唯一写者是L2TLB lifecycle owner，idle判断是唯一消费者之一。
```

源码位置：同文件，`handle_l2tlb_flush_event()`。

```systemverilog
accept_hold_until_sample = sample_seq + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES;
ready_opportunity_since_lifecycle_block = 1'b0;
```

中文伪代码：

```text
该flush分支先把ready hold边界设置为当前sample加V2编译期总延迟。
随后清除ready机会标志，声明本次flush之后还没有重新向DUT开放request。
pending取消、同拍killed fire和flush计数仍由同一个helper的其它语句处理；本字段不重复恢复或删除token。
```

源码位置：同文件，`send_l2tlb_cycle()` 的 idle 与 ready 派生分支。

```systemverilog
if (has_progress || lifecycle_blocked || stopping ||
    outstanding_count() != 0 || !acceptance_opened_since_reset ||
    !ready_opportunity_since_lifecycle_block) begin
    idle_count = 0;
end else begin
    idle_count++;
end

next_ready = !stopping && csr_snapshot_valid && !hold_active &&
             outstanding_count() < max_outstanding;
if (next_ready) begin
    acceptance_opened_since_reset = 1'b1;
end
send_l2tlb_item(cycle_tr);
if (next_ready)
    ready_opportunity_since_lifecycle_block = 1'b1;
```

中文伪代码：

```text
该分支在flush hold解除后的首个sample看到ready机会标志仍为0，因此清零idle_count，不允许进入stopping。
随后按原有stop、CSR、hold和容量条件计算next_ready；条件满足时生成ready=1的cycle item，并把机会标志置1。
如果DUT在hold期间一直保持valid，该ready会在下一DUT sample形成真实fire；下一轮request_fire将建立token。
只有已经提供过这次ready机会且仍然没有request/progress/outstanding时，后续sample才允许累计idle-stop。
```

处理结论：第一轮独立 review 将此项判为高风险；源码、Plan、flow、analysis 和网页必须同步后重新 review。本修复不改变主表或 token 语义。

### 7.6 验证 mode

旧增量 mode 的 `tdc.sdb` 损坏，且 `make eda_run` 的 `run: compile batch_run` 依赖会再次触发
VCS 增量/KDB elaboration；该工具阶段曾出现 `SIGSEGV`，不能把它误判为 RTL 或 UVM 逻辑失败。
最终验收从 `HEAD` 建立 detached worktree、只应用 staged diff，并使用全新的
`l2tlb_stage_verify_20260722_r5` mode 编译；仿真直接运行同一编译产物，避免再次触发增量编译。
仿真产物不纳入源码提交，该处理不改变实现逻辑。

处理结论：这是验证执行方式与原 Plan 的差异，不是源码功能差异。7.5 修复并纳入 8.8
request-time CSR snapshot helper 补强后的 r5 staged-only 全量编译和 direct `tc_sanity` 已完成，
最终证据见第10章；r3/r4 结果只作为对应补强前的历史结果，不再作为归档验收依据。

## 8. Plan 未说明但 Coding 落实的细节

### 8.1 显式冻结 TLB entry

细节功能：`memblock_tlb_entry` 没有字段级 UVM automation，因此不能依赖默认 `copy()`；实现新增 `copy_from()`，逐字段冻结 request-time entry。

为什么 Plan 未覆盖：Plan 要求 entry snapshot 独立，但没有展开无 UVM field automation 时的具体复制方法。

在本特性中的作用：live TLB table 后续被 sfence 删除或被其它请求更新时，已接受 token 的 response payload 和 UID 回填仍使用同一不可变来源。

抽象功能描述：`copy_from()` 由 `capture_fired_request()` 在 request fire 时调用，把 live entry 复制到 token 私有 object；它不写 live table，也不修改 CSR 或主表状态。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_tlb_entry.sv`，`copy_from()`。

```systemverilog
lookup_key = source.lookup_key;
vaddr = source.vaddr;
paddr = source.paddr;
vpn = source.vpn;
ppn = source.ppn;
pte_r = source.pte_r;
pte_w = source.pte_w;
pte_x = source.pte_x;
pte_u = source.pte_u;
pte_g = source.pte_g;
pte_a = source.pte_a;
pte_d = source.pte_d;
pte_n = source.pte_n;
pte_v = source.pte_v;
pbmt = source.pbmt;
tlbAF = source.tlbAF;
tlbPF = source.tlbPF;
tlbGPF = source.tlbGPF;
pmaAF = source.pmaAF;
asid = source.asid;
vmid = source.vmid;
s2xlate = source.s2xlate;
priv_mode = source.priv_mode;
level = source.level;
addr_low = source.addr_low;
foreach (ppn_low[idx]) begin
    ppn_low[idx] = source.ppn_low[idx];
    valididx[idx] = source.valididx[idx];
    pteidx[idx] = source.pteidx[idx];
end
```

中文伪代码：

```text
该逻辑负责建立一份与live table句柄完全独立的request-time entry。
先复制lookup key、虚拟/物理地址和VPN/PPN，再逐项复制R/W/X/U/G/A/D/N/V权限、PBMT和fault字段。
随后复制ASID、VMID、s2xlate、privilege、level和低位地址信息，最后遍历8个索引槽复制ppn_low、valididx和pteidx数组。
调用方在复制完成后用该对象构造response；本函数不调用查表或状态更新helper，也不保留source句柄作为后续真源。
```

是否回写 Plan：已在 `IMPLEMENTATION_DELTA`/执行结果中记录显式 copy 原因，保持当前实现。

### 8.2 Request 字段只作为 response transaction 的 debug metadata

细节功能：pending response transaction保存 request valid/VPN/`s2xlate`，方便 token 日志追踪；active connect 的 request方向仍由RTL驱动，driver不拥有这些输入。

为什么 Plan 未覆盖：Plan 只规定 response payload 和 request采样边界，没有明确 transaction 中保留 request副本的 debug用途。

抽象功能描述：该赋值发生在 token创建时，只把冻结的 request字段写入 object；真正对DUT的有效输出仍只有 ready和response字段。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`，`capture_fired_request()`。

```systemverilog
pending.resp_tr = create_l2tlb_xaction(
    $sformatf("l2tlb_resp_token_%0d", pending.request_token));
pending.resp_tr.io_ptw_req_0_valid = 1'b1;
pending.resp_tr.io_ptw_req_0_bits_vpn = pending.vpn;
pending.resp_tr.io_ptw_req_0_bits_s2xlate = pending.s2xlate;
fill_dtlb_resp_from_entry(pending.entry_snapshot, pending.resp_tr);
```

中文伪代码：

```text
该逻辑为当前token创建一份已清零的response transaction。
把token冻结的request valid、vpn和s2xlate写入transaction作为debug副本，再调用fill_dtlb_resp_from_entry把entry snapshot转换成真正的response payload。
driver的send_pkt不驱动request valid/VPN/s2xlate；active connect从RTL采样这些输入，因此debug副本不会反向覆盖DUT request。
```

是否回写 Plan：只保留在 review 中作为工程细节，Plan 继续以接口所有权为准。

### 8.3 合法 no-UID response 不升级为 testcase error

细节功能：prefetch或尚无 dispatch UID record 的合法 DTLB request也必须完成 token；零匹配只记 `UVM_LOW`。

为什么 Plan 未覆盖：Plan 说明 response可没有UID，但没有展开旧 `uvm_error` 到 info 的失败策略调整。

抽象功能描述：`complete_driving_response()` 在真实 response sample后尝试回填匹配UID；匹配数只用于日志，不决定 token是否完成，也不修改 pass/fail/terminal。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`，`complete_driving_response()`。

```systemverilog
record_update_count = data.update_uid_tlb_records_by_entry(
    driving_req.lookup_key,
    driving_req.entry_snapshot);
driving_req = null;
driving_valid = 1'b0;
completed_count++;
check_l2tlb_lifecycle_accounting("response_complete");
```

中文伪代码：

```text
该逻辑在response已被DUT采样后调用公共helper，按保存的lookup key查找尚未回填的UID record并复制同一entry snapshot。
helper返回更新数量；数量为0时只表示该request没有对应UID，不阻止后续完成。
随后清空driving slot、增加completed_count，并调用生命周期审计确认token从outstanding唯一转入completed。
该路径不写主表pass/fail/terminal，也不豁免任何独立checker错误。
```

是否回写 Plan：失败策略表已同步记录零匹配为合法边界。

### 8.4 Non-destructive runtime latest 与 flush event

细节功能：runtime CSR latest和 L2TLB flush event均可被多个consumer重复读取，不与 semantic raw consumer竞争 pop。

为什么 Plan 未覆盖：初始 Plan 只关注 response permission；多 outstanding生命周期扩展后才需要独立 sideband。

在本特性中的作用、真实源码和中文伪代码已在第4章分别覆盖 `publish_runtime_csr_snapshot()` 和 `handle_l2tlb_flush_event()`；两者最终共享公共 `mmu_csr_state`，不建立第二套CSR模型。该细节已经回写执行 Plan，不再重复贴同一源码块。

### 8.5 当前 staged snapshot 不修 virtual-sequence join 生命周期

细节功能：明确本专项验证范围不把 background responder 的 `join_none` 清理问题算作已完成。

为什么 Plan 未覆盖：该逻辑属于 virtual-sequence/调度 owner，且主工作树已有另一组未暂存修改，本专项不能把它混入 L2TLB独立commit。

抽象功能描述：当前 staged 基线先后台启动 responder，再执行 core flow，随后立即清 activity bit；是否等待 forked responder 退出不由本专项修改。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv`，本专项 staged 基线的 `body()`。

```systemverilog
fork : background_responder_fork
    start_background_responders();
join_none

start_core_dispatch_flow();

memblock_sync_pkg::dispatch_real_smoke_active = 1'b0;
```

中文伪代码：

```text
该基线逻辑用join_none启动后台responder，因此母sequence不等待该fork完成就继续执行core dispatch flow。
core flow返回后立即清dispatch_real_smoke_active；后台responder是否已经观察到最终stop不由这段基线保证。
本L2TLB专项不修改该文件，也不把主工作树中另一个owner的未暂存修复纳入当前staged验证或commit。
```

是否回写 Plan：作为明确依赖边界记录；后续由 virtual-sequence专项独立review和commit。

### 8.6 网页函数目录同步

细节功能：网页必须能搜索本次新增/修改的 lifecycle helper、driver函数，并显示真实调用边、源码骨架和中文逻辑说明。

为什么 Plan 未覆盖：原 Plan只列“同步网页”，未逐个列出可搜索函数目录要求。

实现落点：`AI_DOC/web/memblock_dispatch_control_flow_callgraph_enhanced/assets/app.js`。本轮补齐 `outstanding_count()`、`cancel_outstanding_by_reset()`、runtime latest getter、L2TLB driver `main_phase()/send_pkt()/drive_idle()` 等节点，并为核心 lifecycle节点提供 `source` 与 `logicNotes`。

是否回写 Plan：执行结果与文档同步清单已记录；网页通过 `node --check` 后才允许归档。

### 8.7 明确未实现边界

独立 S1/S2 G/U、directed GPF/GAF、non-bare PTW-miss directed testcase仍是后续工作。本项不是隐藏 coding 细节：当前两个阶段的接口字段均从共享 `entry.pte_g/pte_u` 驱动，第5章已给出真实源码和边界，TODO文档继续保留独立建模项。

### 8.8 request-time CSR snapshot 贯穿 get-or-create

细节功能：L2TLB request fire 后，公共 TLB entry 的 key 生成和新 entry 构造均显式消费该 token 的
`csr_snapshot`，不再只在 sequence 中保存 snapshot、却让公共建表 helper读取 live CSR。

发现原因：独立复核发现原实现的 `pending.lookup_key` 使用 request snapshot，但
`get_or_create_tlb_entry_by_req()` 未命中时调用 `build_tlb_entry_for_key()`，后者读取
`common_data_transaction.mmu_csr_state`。CSR 更新边界可能出现 key 与 entry 构造上下文不一致，违反
request-time freeze 合同。

修改后源码：`common_data_transaction.sv` 新增
`get_or_create_tlb_entry_by_req_with_snapshot()` 和 `build_tlb_entry_for_key_with_csr()`；旧 API
保留为 live-CSR 兼容包装，L2TLB `capture_fired_request()` 改为传入 `pending.csr_snapshot`。

```systemverilog
key = csr_snapshot.make_lookup_key({26'b0, vpn}, s2xlate);
if (has_tlb_entry(key)) begin
    entry = tlb_entry_by_key[key];
end else begin
    entry = build_tlb_entry_for_key_with_csr(key, csr_snapshot);
    insert_tlb_entry(key, entry);
end
```

中文伪代码：

```text
在真实 request fire 边界复制 CSR；
用同一副本生成 lookup key；
命中公共 by-key 表则只更新命中时间，未命中则用该副本构造 entry 并插入；
随后仍对 live entry 做显式 copy_from，response 和 UID 回填不重新读取 live CSR。
```

与原逻辑对比：这是 request-time snapshot 功能逻辑的补强，不改变 TLB 表的唯一 owner、sfence
invalidate、response completion 时点或主表 pass/fail/terminal。该补丁已纳入本专项 staged 范围，需与
L2TLB 源码一起重新执行编译检查。

### 8.9 sequence disabled 与 active 同拍 item 的 owner 门控握手

第一轮独立 reviewer 发现：无条件阻塞 `get_next_item()` 在
`MEMBLOCK_L2TLB_SEQ_EN=0` 时没有 producer。中间修正改用每拍 `try_next_item()` + idle
fallback，但新一轮独立 reviewer 指出 driver 和 sequence 在同一 `drv_cb` 唤醒时存在调度
竞态：driver 若先执行，会把 sequence 稍后生成的当前拍 item 误判为空并插入伪 idle。
因此两种无门控方案都不是最终实现。

抽象功能描述：最终 driver 把 `l2tlb_lifecycle_owner_claimed` 当作当前边界 item
是否必然存在的合同。owner=0 时不取 item 并驱动 inactive；owner=1 时通过
`get_owned_item_or_abort()` 阻塞等待 sequence 在该边界交付唯一 gap=0 item。
sequence 仍负责 queue、latency、flush、stop 和 owner 写入；driver 只读 owner，且能在
owner 撤销或 phase 终止时中断等待。

源码位置：`mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv`，task
`main_phase()`。

```systemverilog
while(1) begin
    @this.vif.drv_mp.drv_cb;
    if (!memblock_sync_pkg::l2tlb_lifecycle_owner_claimed) begin
        this.drive_idle(this.cfg.drv_mode);
    end
    else begin
        bit got_item;
        bit aborted;
        this.get_owned_item_or_abort(phase, req, got_item, aborted);
        if (aborted) begin
            this.drive_idle(this.cfg.drv_mode);
            return;
        end
        if (!got_item || req == null) begin
            `uvm_fatal(get_type_name(), "active L2TLB lifecycle owner returned no cycle item")
        end
        if (req.pre_pkt_gap != 0 || req.post_pkt_gap != 0) begin
            `uvm_fatal(get_type_name(), "L2TLB cycle item requires gap=0")
        end
        this.send_pkt(req);
        seq_item_port.item_done();
    end
end
```

中文伪代码：

```text
sequence启用后先claim owner，然后进入每个drv_cb都发送一笔cycle item的service loop。
driver在同一drv_cb先检查owner；owner为1时通过get_owned_item_or_abort阻塞等待，不会因调度顺序驱动伪idle。
若sequence被kill或phase进入终止态，abort分支关闭get_next_item等待、驱动一拍idle并返回；正常owner=1分支仍要求每个边界都有item。
sequence disabled时不claim，自然退出后已release；这两种情况driver直接drive_idle，不会阻塞在sequencer。
正常分支的null item或非零gap说明owner合同被破坏，立即fatal；driver不维护pending_q、driving_req、token、latency或global stop。
```

与原逻辑对比：旧 driver 用 gap 和 idle ready 参与 responder 时间调度。最终方案将
时间和状态判断留在 sequence，driver 只增加一个 owner 只读门控，并在 active 区间使用标准
阻塞 item 握手和可中断的 phase/owner abort。这是 driver/sequence 时序细节修正，不改变 L2TLB request/response 方向、
token 守恒或主表状态。r6 是已废弃中间方案的历史验证，最终方案需重新验证。

### 8.10 owner-gated 实现 r7 重新验证

修正后以当前 index diff 建立 detached staged-only worktree `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_l2tlb_stage_verify_r7`，只应用 L2TLB 专项 staged 快照；worktree 名与仿真 mode 名不同。

```text
make eda_compile tc=tc_sanity mode=l2tlb_stage_verify_20260722_r7
  -> VCS/KDB exit=0
  -> Verdi KDB: 0 error(s), 0 warning(s)

basicTest + VSEQ_MAIN=virtual_base_sequence + MEMBLOCK_L2TLB_SEQ_EN=0
  -> 265.3ns TEST_PASS, UVM_ERROR=0, UVM_FATAL=0

tc_sanity + VSEQ_MAIN=memblock_dispatch_real_smoke_vseq + cfg=tc_dispatch_real_smoke
  -> owner claim、flush/hold、idle-stop均有日志
  -> 226.3us TEST CASE PASSED, UVM_ERROR=0, UVM_FATAL=0
```

`r7` 的验证与中间 `r6` 不同：验证产物中的 driver 已是 owner=0 idle、owner=1 阻塞取必有 item 的实现。这证明 disabled 不会因取 item 而卡住，active owner 也能在同一 service loop 中正常 claim、发送 cycle item 并退出。后续 `r8` 再加入强制停序补强；动态 non-bare PTW miss、multiple outstanding/reorder、reset/flush cancel 和非零 G/U payload 仍未被这两条 smoke 覆盖。

### 8.11 强制停序与 phase 终止清理

新一轮 review 指出单靠自然 `body()` 退出不足以覆盖 UVM 强制停序：sequence 可能在 `get_next_item()` 等待时被杀，而 `post_body()` 不会被保证调用。本轮补强包括：

```text
sequence.do_kill()
  -> try_release_l2tlb_lifecycle_owner(lifecycle_owner_name)

driver.get_owned_item_or_abort(phase)
  -> fork: get_next_item(req)
  -> fork: wait drv_cb and check owner/phase terminal state
  -> owner cleared or phase >= READY_TO_END: disable get branch, drive_idle, return

driver.phase_ended(phase)
  -> if package owner still claimed, try_release as final component cleanup
```

这条路径只清理 owner 和 driver 取item线程，不创建新 token，不调整 pending/completed 计数，也不改写主表 pass/fail/terminal。
只有已被取到的正常 item 才走 `send_pkt/item_done`；中断分支不会使用悬空 item 调用 `item_done`。

### 8.12 r8 重新验证记录

`l2tlb_stage_verify_20260722_r8` 位于隔离 staged-only worktree `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_l2tlb_stage_verify_r8`，而不是主工作树的 sim 目录；为避免用户脏改动混入验证，该隔离工作树是故意的。关键可审计结果：

```text
make eda_compile tc=tc_sanity mode=l2tlb_stage_verify_20260722_r8
  -> VCS/KDB exit=0; Verdi KDB 0 error(s), 0 warning(s)

basicTest + VSEQ_MAIN=memblock_dispatch_real_smoke_vseq + cfg=tc_dispatch_real_smoke
  -> 265.3ns TEST_PASS; UVM_ERROR=0; UVM_FATAL=0
```

日志与 compile output 保存在该隔离 worktree 的 `mem_ut/ver/ut/memblock/sim/l2tlb_stage_verify_20260722_r8/log/`。该目录只有本节列出的 compile log 和一条 active basic smoke log；这里不声称动态命中 `do_kill()`、`stop_sequences()`、`phase_ended()`，也不声称覆盖 dynamic PTW miss/multiple outstanding/reorder/GU payload。

## 9. 与原测试框架逻辑对比和修改类型总结

| 修改项 | 类型 | 原有逻辑 | 变更原因 | 修改后逻辑 |
|---|---|---|---|---|
| G/U response链 | 字段适配 | S2 G/U存在断链/常量风险 | V2 response真实包含字段 | 从 entry经xaction/driver/interface/connect完整驱动 |
| request接受 | 功能逻辑修改 | valid-triggered、ready item后串行处理 | gap期间会漏真实fire | 同sample valid&&ready，每fire独立token |
| request-time快照 | 功能逻辑新增/补强 | response前读取live CSR/TLB entry；新entry建表仍可能读取live CSR | 延迟期间及CSR边界不能混用上下文 | fire时冻结CSR，并以同一snapshot生成key、get-or-create entry、显式entry副本和response payload |
| outstanding | 功能逻辑新增 | 无queue、可靠单笔 | V2 filter支持多笔inflight | bounded pending_q + driving slot |
| response次序 | 功能逻辑新增 | 隐式串行顺序 | V2按内容匹配且多路径返回 | 默认ordered，可配置到期项reorder |
| latency | 参数与功能修改 | MIN/MAX均匀随机+driver gap | 不能表达权重且阻塞driver | 1/MID/LONG dist，due只表示最早拍 |
| CSR获取 | 公共状态 plumbing修改 | 依赖semantic capture gate，reset后可能复用旧latest | legacy responder可能无CSR，mid-test reset需等待新快照 | monitor独立runtime latest，共享raw类型和apply seq；reset后要求seq前进 |
| flush/reset | 功能逻辑新增 | 无pending取消账本 | 旧request可能在filter flush后收到孤立response | canceled分类、4拍hold、token守恒 |
| driver | 时序逻辑修改 | try_next_item/gap/idle ready | driver拥有部分生命周期且可能无人记录fire；无条件阻塞不支持disabled，逐拍try又与sequence同拍竞态 | owner=0驱动inactive；owner=1阻塞取当前边界必有的gap0 item，idle ready=0 |
| active driver配置 | 配置细节新增 | L2TLB drv_mode可被随机成generic pattern | active takeover会制造无owner ready/response | env与tc_sanity默认DRV_0，错误显式覆盖仍fatal |
| UID回填 | 时点与失败策略修改 | response驱动前更新；零匹配error | 软件状态早于真实response，prefetch误报 | sample完成后更新；零匹配info |
| owner/stop | 功能逻辑新增/边界修正 | 多sequence实例无互斥；idle可能在首次ready或flush hold后重开放前退出 | 可能双queue、退出后ready残留或漏掉admission | package owner、每次lifecycle block后至少一拍ready、drain、最终inactive item |

主表生成、LSQ allocation、issue、writeback、ROB commit、LQ/SQ deq以及 pass/fail/terminal主体定义未被本专项修改。

## 10. 验证结果与剩余风险

已完成：

- 从当前 `HEAD` 建立临时 detached worktree，只应用 `git diff --cached` 并复用同版本
  `build_memblock/rtl`；该快照不包含主工作树中 MMIO/cancel 等未暂存 flow 改动。
- 历史 staged-only mode `l2tlb_stage_verify_20260722_r6` 已完成一次完整 `make eda_compile`，
  命令退出 0 并生成可运行 `simv`；源码无 compile error，持久化 compile log 的 Verdi KDB 结尾为
  `0 error(s), 0 warning(s)`。编译命令包含 VCS `-lca` 的预期 `LCA_FEATURES_ENABLED` usage warning，
  因此不宣称整个工具输出绝对零 warning。r6 已包含第8.8节的 request-time CSR snapshot helper，
  但其 driver 仍是后续 review 否定的 `try_next_item()` 中间实现，因此不是最终验收快照。
- `make eda_run` 的重复编译阶段发生 VCS 工具 `SIGSEGV`，未进入可接受的 runtime 结果；随后在
  同一 staged-only 编译产物上直接运行 `simv`，分别覆盖 disabled 和 active driver 边界：
  `l2tlb_disabled_direct_20260722_r6.log` 使用 `MEMBLOCK_L2TLB_SEQ_EN=0` 和空
  `virtual_base_sequence`，在 `265.3ns` 输出 `TEST_PASS`、`UVM_ERROR=0`、`UVM_FATAL=0`；
  `l2tlb_active_cfg_direct_20260722_r6.log` 使用启用 L2TLB 的 1 条 scalar load real-smoke，在
  `380.3ns` 输出 `TEST_PASS`、`UVM_ERROR=0`、`UVM_FATAL=0`。两次运行的既有 warning 均来自未配置
  的 vecissue default sequence。这两条 smoke 同样只属于r6中间实现的历史记录。
- r5 的生命周期 smoke 记录仍有效：flush event 在 `sample=2` 建立 `hold_until=6`，hold解除后的
  `sample=6` 先提供一拍 ready机会，到 `sample=7` 才以 `idle_count=1` idle-stop，且退出时无
  outstanding request；该结果覆盖第一轮 reviewer 指出的 post-flush 首次 ready 退出风险。
- owner-gated driver 已落源码与文档；r7 staged-only 编译和 disabled/active smoke已执行成功；r8证明强制停序补强后的源码可编译且 active basic smoke可结束，但未动态命中 `do_kill()`、`stop_sequences()` 或 `phase_ended()`。最终独立 reviewer 已确认当前 staged snapshot 无 blocker，结论为 `FINAL PASS`。

验证边界：

- 当前 staged-only disabled smoke 的 CSR 为 `satp_mode=0` bare translation，且没有 `accept L2TLB` 或
  `complete L2TLB`；active smoke 虽完成 1 条 scalar load real-smoke，但日志也没有真实 non-bare PTW
  miss response。因此不能把两次运行解释为本专项完整 request/response 动态覆盖，也不能宣称覆盖
  multiple outstanding 或 real-smoke 全部路径。
- 尚未动态覆盖：non-bare PTW miss、multiple outstanding、ordered/reorder、queue-full、mid-test reset 的
  pending/driving cancel、post-reset CSR freshness、stale/future flush、带 outstanding 的 global-stop drain、
  owner 冲突、自然release后handoff、强制kill/stop_sequences/phase_ended回调、以及非零 G/U payload。
- 完整动态覆盖需要后续 non-bare CSR + directed DTLB miss/reset/flush testcase；在此之前以上路径保留仿真覆盖风险，
  当前只能依赖静态链路和编译合同检查。
- 独立 S1/S2 G/U 权限模型仍为 TODO，不影响当前“两个接口字段均从共享 entry驱动”的适配结论。
- 本轮后续补强已把 `csr_snapshot` 贯穿新 entry 的 get-or-create；动态 non-bare/PTW-miss 场景仍未在 smoke 中覆盖。

## 11. 非本次修改的逻辑分析

### 11.1 git status 对比结论

本 review 只分析当前 L2TLB staged snapshot。工作树中同时存在用户或其它专项的未暂存修改，未被回滚、覆盖或纳入本专项 commit：

| 类别 | 文件/目录示例 | 判断 | 原因 |
|---|---|---|---|
| MMIO/commit/deq/cancel 功能 | `memblock_lsqcommit_dispatch_base_sequence.sv`、`lsq_commit_handler.sv`、ctrl agent、MMIO plans/reviews | 其它测试框架专项 | 负责 pendingPtr、scommit、MMIO tag、SQ deq/cancel 对账，不属于 L2TLB responder |
| redirect/replay 与主表 | `memblock_redirect_dispatch_base_sequence.sv`、`exception_redirect_replay_handler.sv`、main sequence | 其它 recovery 专项 | 修改 ROB/LQ/SQ active状态和redirect/cancel，不由L2TLB token owner消费 |
| virtual-sequence 调度 | `memblock_dispatch_real_smoke_vseq.sv`、新增 cancel/MMIO vseq | 其它 sequence owner | 包含 background responder join/stop 和directed场景，必须独立review、验证和commit |
| 公共类型/状态的未暂存部分 | `common_data_transaction.sv`、`memblock_dispatch_types.sv`、`memblock_sync_pkg.sv`、`seq_csr_common.sv` 的 worktree diff | 混合文件中的其它专项改动 | 本专项只提交已审核的index hunks；不得用整文件add混入未暂存逻辑 |
| 文档规则与review归档 | `AGENTS.md`、project management rules、`review_doc/do` 搬迁 | 文档管理专项 | 不改变本轮L2TLB运行期逻辑，保留现有工作树状态 |
| 新增未跟踪源码/配置 | cancel reconcile、pending-MMIO sequence/vseq/cfg、`memblock_op_behavior_util.sv` | 其它功能实现 | 尚未纳入本review覆盖范围，也不应进入本专项commit |

本专项最终 stage 时必须再次逐文件核对 index，确保上述 worktree-only 修改没有被 `git add` 整文件带入。

## 12. Review 结论

本轮源码实现、网页调用图与扩展后的 V2 L2TLB execution plan主体一致；实现差异和额外细节已在第7、8章逐项记录。

一轮 review 发现 disabled 模式不能无条件阻塞取 item；随后 r6 中间实现使用逐拍
`try_next_item()`。最新独立 reviewer 进一步发现该中间实现与 sequence 有同拍竞态，结论为
`FINAL FAIL`。当前已改为第8.9节的 owner-gated blocking handshake，同时覆盖 disabled
inactive 和 active 必有 item 两条边界，并在第8.10节完成 r7 重新编译与 smoke、在第8.12节完成 r8 补强源码编译与 active basic smoke；r8 不作为强制停序回调的动态命中证据。

最终独立 reviewer 直接检查当前 staged snapshot、r7/r8 隔离工作树日志、UVM 1.2 `do_kill()` 调用链以及共享文件的 staged/unstaged 边界，确认：

- 没有实现 blocker或 staged 文档矛盾；
- r7 的 disabled/active 结论与日志一致，r8 只声明 compile和单条 active basic smoke；
- MMIO/cancel 等其它专项改动未进入本专项 index；
- 强制停序回调、multi-outstanding/reorder、non-bare PTW miss和非零G/U仍是已准确记录的动态覆盖缺口。

最终结论：`FINAL PASS`。

plan 已按规则移动到 `AI_DOC/plan/test_framework/plan/do/`；本 review 按文档管理规则继续保留在 `review_doc/undo`，除非用户另行明确要求归档 review。
