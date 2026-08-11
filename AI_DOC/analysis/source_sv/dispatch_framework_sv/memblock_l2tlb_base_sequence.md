# memblock_l2tlb_base_sequence.sv 源码分析

本文档对应源码：

- mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv

本文档描述当前 V2 responder 实现。L2TLB_agent 的方向固定为 DTLB 到 L2TLB request、L2TLB 到 DTLB response；它不是 L2Cache、PTW 或 memory 的下游访问模型。

## 1. 术语与抽象职责

| 术语 | 当前源码中的含义 | 主要落点 |
|---|---|---|
| request fire | 同一 service sample 中 drv_cb 的 valid 与 mon_cb 的 ready 同时为 1 | request_fire() |
| pending record | 一笔 fire 的 request-time 上下文和冻结 response | memblock_l2tlb_pending_req |
| pending_q | 已接受但尚未进入 response driving slot 的请求队列 | pending_q |
| driving slot | 已选中并等待下一 DUT sample 确认完成的唯一 response | driving_req/driving_valid |
| due sample | 最早允许 response 被 DUT 采样的 sample 序号 | due_sample_seq |
| runtime CSR latest | CSR monitor 发布的可重复读取最新快照 | get_latest_runtime_csr_snapshot() |
| request-fire C-2 CSR | 当前真实 request fire 对应、由 V2 filter 实际可见的 CSR history 项 | pending.csr_snapshot、get_request_csr_snapshot() |
| issue-time CSR | UID 进入 WAITING 时保存的历史 CSR；它不等同于 request 实际 fire 时的 CSR | memblock_uid_tlb_record.csr_snapshot |
| UID request-fire marker | WAITING UID 首次被真实 request fire 覆盖的 global sample | uid_tlb_first_request_fire_sample_seq |
| flush event | CSR changed 或 sfence 产生的非破坏性生命周期 sideband | get_latest_l2tlb_flush_event() |
| lifecycle owner | 唯一拥有 ready、queue、token 和 response 调度权的 sequence | memblock_sync_pkg owner state |
| ready opportunity | reset或flush阻塞解除后至少发送一拍可接受ready的机会，不等同于request fire | ready_opportunity_since_lifecycle_block |

该 sequence 的抽象职责是每拍推进一次 responder service：从稳定的 clocking block sample 识别真实 request fire，为每次 fire 冻结 CSR、lookup key、TLB entry 和 response payload，按 due/order 策略逐拍返回，并在 reset、flush、stop 时闭合 token 生命周期。

## 2. 类和成员状态

### 2.1 memblock_l2tlb_pending_req

抽象功能描述：该 UVM object 保存一笔请求从 fire 到 response 完成或取消所需的不可变上下文，使等待期间 live CSR/TLB 表变化不会污染已接受请求。

| 字段 | 作用 |
|---|---|
| request_token | 测试框架内部单调编号，不写入 DUT |
| vpn/s2xlate | fire 边界从 drv_cb 采样的 request 字段 |
| csr_snapshot | fire 时取得的 MMU CSR runtime 副本 |
| lookup_key | 由 request 和 CSR 组成的 vpn/asid/vmid/s2xlate key |
| entry_snapshot | live entry 通过 memblock_tlb_entry::copy_from() 得到的副本 |
| resp_tr | 已由 entry snapshot 填好的 response transaction |
| accept_sample_seq | request fire 的 sample 序号 |
| latency_bucket/min_latency | 1C、MID、LONG 档及其最早间隔 |
| due_sample_seq | accept_sample_seq 加 min_latency |
| accept_flush_event_seq | 接受请求时看到的 flush event 版本 |

record 只存在于 pending_q 或 driving_req 中。response 完成、flush 取消或 reset 取消后释放，不修改主表 terminal 状态。

### 2.2 sequence 成员分组

| 分组 | 成员 | 作用 |
|---|---|---|
| 配置 | enable、max_outstanding、resp_reorder_en、三档 latency/weight、idle_stop_cycle | 保存 seq_csr_common 已校验的运行参数 |
| 容器 | pending_q、driving_req、driving_valid | 形成 bounded outstanding |
| 账本 | accepted_count、completed_count、flush_canceled_count、reset_canceled_count、next_request_token | 验证每个 fire 的最终归类 |
| 生命周期 | sample_seq、last_seen_flush_event_seq、accept_hold_until_sample、pre_ready_hold_until_sample、owner_start_baseline_done、acceptance_opened_since_reset、ready_opportunity_since_lifecycle_block、csr_snapshot_valid、reset_runtime_csr_seq_baseline、require_post_reset_csr_refresh、stopping、idle_count | 分离启动历史 event 的 baseline hold 与运行期 C0/C4 flush hold |
| 输入快照 | sampled_req_valid、sampled_req_ready、sampled_req_vpn、sampled_req_s2xlate | 避免 NBA 或后续 delta 重新读取 live VIF |

## 3. 启动、配置和上下文

### 3.1 body()

抽象功能描述：body() 是 sequence 生命周期入口，负责初始化公共参数、确认 takeover、claim 唯一 owner，并在循环结束后验证排空和释放 owner。

真实逻辑摘要：

~~~systemverilog
seq_csr_common::init();
configure_from_plus();
if (!enable) return;
ensure_context();
if (!memblock_sync_pkg::l2tlb_responder_active)
    `uvm_fatal(...);
if (!memblock_sync_pkg::try_claim_l2tlb_lifecycle_owner(
        lifecycle_owner_name, current_owner))
    `uvm_fatal(...);
initialize_lifecycle_state();
drive_l2tlb_loop();
check_l2tlb_lifecycle_accounting("owner_release");
if (outstanding_count() != 0) `uvm_fatal(...);
if (!memblock_sync_pkg::try_release_l2tlb_lifecycle_owner(
        lifecycle_owner_name, current_owner))
    `uvm_fatal(...);
~~~

文字伪代码：

~~~text
初始化 seq_csr_common 并读取配置；
enable 为 0 时直接返回且不 claim owner；
获取 common data 和 VIF，takeover 未激活时 fatal；
以 sequence 全名 claim owner，冲突时报告当前 owner 并 fatal；
初始化本实例生命周期状态并进入逐拍 loop；
loop 自然结束后检查 token 等式和 outstanding 为 0；
最终 inactive item 完成后按名称 release owner。
~~~

### 3.2 configure_from_plus()

抽象功能描述：该函数只读取 seq_csr_common 已完成合法性检查和资源收敛的配置，不直接读取 plusarg 或 compile 宏。

~~~systemverilog
enable = seq_csr_common::get_l2tlb_seq_en();
max_outstanding = seq_csr_common::get_l2tlb_max_outstanding();
resp_reorder_en = seq_csr_common::get_l2tlb_resp_reorder_en();
resp_mid_latency = seq_csr_common::get_l2tlb_resp_mid_latency();
resp_long_latency = seq_csr_common::get_l2tlb_resp_long_latency();
resp_1c_wt = seq_csr_common::get_l2tlb_resp_1c_wt();
resp_mid_wt = seq_csr_common::get_l2tlb_resp_mid_wt();
resp_long_wt = seq_csr_common::get_l2tlb_resp_long_wt();
idle_stop_cycle = seq_csr_common::get_l2tlb_idle_stop_cycle();
~~~

参数语义：

- MEMBLOCK_L2TLB_MAX_OUTSTANDING 是行为上限，最终不超过 V2 MEMBLOCK_DUT_L2TLB_DFILTER_SIZE。
- MEMBLOCK_L2TLB_RESP_REORDER_EN 为 0 时按队头回复，为 1 时在所有 due 项中随机选择。
- 1C 档固定为 1 sample；MID/LONG 使用对应参数，且 MID 大于 1、LONG 大于 MID。
- 三个 latency 权重使用 std::randomize() dist 选择，不能全部为 0。
- MEMBLOCK_L2TLB_IDLE_STOP_CYCLE 只控制真正空闲时退出，不会让有 outstanding 或 hold 的 responder 提前退出。

### 3.3 ensure_context() 与 initialize_lifecycle_state()

抽象功能描述：ensure_context() 取得 common_data_transaction 和 L2TLB VIF；initialize_lifecycle_state() 初始化当前 sequence 的动态容器、计数和 sample 状态，不清 package 级 latest snapshot 或 owner。

文字伪代码：

~~~text
ensure_context：
  获取 common_data_transaction singleton，失败 fatal；
  按当前 sequence 路径或 agent 通配路径获取 VIF，失败 fatal；

initialize_lifecycle_state：
  清 pending_q 和 driving slot；
  清 accepted/completed/flush/reset canceled 计数及 next token；
  清 sample、flush baseline、hold、CSR valid、reset后CSR刷新门槛、ready机会、stop、idle 和 sampled request 字段；
  不清 runtime CSR latest、flush event latest 或 owner claim。
~~~

## 4. 逐拍 service loop

### 4.1 drive_l2tlb_loop()

抽象功能描述：该 task 只建立稳定的 drv_cb service tick，递增 sample 序号并调用唯一的 cycle helper。

~~~systemverilog
forever begin
    @(l2tlb_vif.drv_cb);
    sample_seq++;
    send_l2tlb_cycle(has_progress, should_exit);
    if (should_exit) break;
end
~~~

文字伪代码：

~~~text
等待 drv_cb；
sample_seq 加 1；
调用 send_l2tlb_cycle 推进本拍所有 lifecycle；
只有 should_exit 置 1 才结束 loop。
~~~

### 4.2 send_l2tlb_cycle()

抽象功能描述：该 task 是唯一的 per-sample 调度中心，负责输入锁存、sideband 同步、reset/flush、request acceptance、response 选择、ready 计算和退出判断。

源码关键逻辑：

~~~systemverilog
sampled_req_valid = (l2tlb_vif.drv_cb.io_ptw_req_0_valid === 1'b1);
sampled_req_ready = (l2tlb_vif.mon_cb.io_ptw_req_0_ready === 1'b1);
sampled_req_vpn = l2tlb_vif.drv_cb.io_ptw_req_0_bits_vpn;
sampled_req_s2xlate = l2tlb_vif.drv_cb.io_ptw_req_0_bits_s2xlate;

uvm_wait_for_nba_region();
memblock_sync_pkg::get_latest_l2tlb_flush_event(...);

if (driving_valid)
    complete_driving_response();
drain_csr_runtime_events();
if (new_flush_event)
    handle_l2tlb_flush_event(...);
if (request_fire() && !request_killed)
    capture_fired_request();

if (has_progress || lifecycle_blocked || stopping || outstanding_count() != 0 ||
    !acceptance_opened_since_reset || !ready_opportunity_since_lifecycle_block)
    idle_count = 0;
else
    idle_count++;
response_selected = select_due_response(sample_seq + 1, cycle_tr);
next_ready = !stopping && csr_snapshot_valid && !hold_active &&
             outstanding_count() < max_outstanding;
cycle_tr.io_ptw_req_0_ready = next_ready;
if (next_ready) begin
    acceptance_opened_since_reset = 1'b1;
end
cycle_tr.pre_pkt_gap = 0;
cycle_tr.post_pkt_gap = 0;
send_l2tlb_item(cycle_tr);
if (next_ready)
    ready_opportunity_since_lifecycle_block = 1'b1;
~~~

文字伪代码：

~~~text
从 drv_cb 锁存 valid/vpn/s2xlate，从 mon_cb 锁存同边界实际 ready；
等待 NBA，使 CSR/fence monitor 完成本边界发布；
读取 flush latest；
reset/backend 未就绪时取消所有 outstanding、对齐 flush baseline、发送 inactive 并返回；
正常状态先检查 event_seq 不倒退，ready 曾开放后迟到或未来 event 在状态变更前 fatal；owner 首次开放 ready 前检查 transport、pending、barrier 和 WAITING UID 为空；
确认上一拍 driving response；
读取并幂等应用 runtime CSR latest；
处理新 flush event 和同拍被 kill 的 fire；
处理正常 valid&&ready fire 并创建 pending record；
观察 global stop；尚未开放过 ready或本次reset/flush阻塞后尚未重新提供ready机会时不累计 idle stop；
只有已经开放过 ready、本次阻塞后已重新提供ready机会且其余 lifecycle block 全部解除时才更新 idle stop；
从 due pending 中最多选择一笔 response；
只有 CSR 有效、非 hold、非 stopping 且容量未满才给下一拍 ready；
构造并发送 gap 为 0 的 cycle item；
stopping 且 outstanding 为空时要求当前 item 完全 inactive 并返回退出。
~~~

关键边界：vpn/s2xlate/valid 不从 live VIF 二次读取；ready 取 mon_cb sample 只用于识别真实 fire。response 没有 ready，选入 driving slot 不等于完成，完成登记延后一拍。

### 4.3 send_l2tlb_item()

抽象功能描述：该 task 把当前唯一 cycle item通过标准 UVM sequence handshake交给 L2TLB driver，并在进入 driver 前强制时间间隔字段为零。

~~~systemverilog
if (tr == null)
    `uvm_fatal(get_type_name(), "send_l2tlb_item got null xaction")
if (tr.pre_pkt_gap != 0 || tr.post_pkt_gap != 0)
    `uvm_fatal(get_type_name(), "L2TLB cycle item must use pre_pkt_gap=0 and post_pkt_gap=0")
start_item(tr);
finish_item(tr);
~~~

文字伪代码：

~~~text
检查transaction非空；
检查pre_pkt_gap和post_pkt_gap都为0，非零立即fatal；
调用start_item/finish_item把本拍ready和response payload交给driver；
sequence不在此处等待额外周期，延迟和生命周期都由send_l2tlb_cycle维护。
~~~

## 5. Request fire、冻结和账本

### 5.1 request_fire() 与 outstanding_count()

抽象功能描述：request_fire() 定义动态请求边界；outstanding_count() 为 ready 容量和生命周期审计提供统一统计。

~~~systemverilog
function bit request_fire();
    return sampled_req_valid && sampled_req_ready;
endfunction

function int unsigned outstanding_count();
    return pending_q.size() + (driving_valid ? 1 : 0);
endfunction
~~~

driving_req 在下一 sample 完成前仍占容量，因此不能在 select_due_response() 后立即释放名额。相同 key 的不同 fire 也必须有独立 record。

### 5.2 capture_fired_request()

抽象功能描述：该函数在 fire 边界构造冻结的 pending record，完成 request-time 查表和 response payload 准备，但不提前更新 UID record。

执行边界：真实 fire 仍在同拍 stop/close 写入之前被正常接收；但一旦本地
`release_close_requested` 或共享 admission seal 已经可见，函数先 fatal，再创建 pending、分配 token 或写入 UID marker。
这样可以区分合法的 C0 同拍 fire 与 cutoff 之后的非法 fire。

源码关键逻辑：

~~~systemverilog
pending.request_token = next_request_token;
next_request_token++;
pending.vpn = sampled_req_vpn;
pending.s2xlate = sampled_req_s2xlate;
get_request_csr_snapshot(pending.csr_snapshot);
pending.lookup_key = pending.csr_snapshot.make_lookup_key(
    {26'b0, pending.vpn}, pending.s2xlate);
data.get_or_create_tlb_entry_by_req(..., returned_key, live_entry, created);
pending.entry_snapshot.copy_from(live_entry);
pending.resp_tr = create_l2tlb_xaction(...);
fill_dtlb_resp_from_entry(pending.entry_snapshot, pending.resp_tr);
pending.min_latency = choose_latency(pending.latency_bucket);
pending.accept_sample_seq = sample_seq;
pending.due_sample_seq = sample_seq + pending.min_latency;
pending.accept_flush_event_seq = last_seen_flush_event_seq;
pending_q.push_back(pending);
accepted_count++;
~~~

文字伪代码：

~~~text
检查 outstanding 未达到 max；
分配 token 并复制 sampled request；
取得本次 request-fire 的 C-2 CSR 副本，用同一副本构造 lookup key；
命中或创建 live TLB entry，返回 key 与 snapshot key 不一致时 fatal；
显式 copy_from 保存 entry snapshot，避免后续 live table 变化污染回复；
从 entry snapshot 填 resp_tr；
按三档权重计算 due sample 并保存接受时 flush 版本；
push pending_q、accepted 加 1、检查生命周期等式；
调用 UID request-fire marker helper；该 helper 只用本次 pending 的 C-2 CSR/key 为 bounded
WAITING candidate 写 marker，不以 UID issue-time CSR 拒绝候选。
此时不调用 response-to-UID completion helper。
~~~

#### P1：UID request-fire marker 的 CSR 来源

抽象功能描述：`mark_waiting_uid_records_on_request_fire()` 在 `capture_fired_request()` 已确认真实握手后，
把本次 request 生命周期写入可能关联的 UID waiting record。它不分配 token、不回填 payload，也不负责 ROB
redirect；其唯一 CSR 输入是本次 request-fire 的 C-2 snapshot。

正确的文字伪代码：

~~~text
从 {pending.vpn, pending.s2xlate} 的 bounded waiting bucket 读取候选 UID；
对每个有效 WAITING UID，用 pending.csr_snapshot 重建 request key；
该 key 与 pending.request_lookup_key 一致时，若 marker 为 0 则写入当前 fire sample；
不得使用 record.csr_snapshot（UID issue-time CSR）重新构造第二个 key 作为拒绝条件。
~~~

当前源码复核状态：`capture_fired_request()` 已保存 request-fire C-2 snapshot；但
`common_data_transaction::mark_waiting_uid_records_on_request_fire()` 仍额外从 `record.csr_snapshot`
构造 `candidate_key` 并执行硬比较。因此上述 P1 合同目前仅完成 request capture 一侧，UID marker 一侧仍待
coding 修正；本文不能将该项记为已完成。

### 5.3 check_l2tlb_lifecycle_accounting() 与 cancel_outstanding_by_reset()

抽象功能描述：前者验证每个 token 必须落在 completed、flush canceled、reset canceled 或当前 outstanding 中；后者在 reset 时归类并清除当前容器。

~~~systemverilog
accounted_count = completed_count + flush_canceled_count +
                  reset_canceled_count + outstanding_count();
if (accepted_count != accounted_count)
    `uvm_fatal(...);

canceled_count = outstanding_count();
reset_canceled_count += canceled_count;
pending_q.delete();
driving_req = null;
driving_valid = 1'b0;
~~~

reset 不回退 next_request_token、accepted 或其它累计计数，也不回填 UID record。

## 6. Due latency 和 response 调度

### 6.1 choose_latency()

抽象功能描述：该函数通过 std::randomize() dist 选择每笔 request 的最早 due 间隔；它不等待时钟，也不修改 driver gap。

~~~systemverilog
if (!std::randomize(bucket) with {
        bucket dist {
            L2TLB_LATENCY_1C   := resp_1c_wt,
            L2TLB_LATENCY_MID  := resp_mid_wt,
            L2TLB_LATENCY_LONG := resp_long_wt
        };
    })
    `uvm_fatal(...);

case (bucket)
    L2TLB_LATENCY_1C:   return 1;
    L2TLB_LATENCY_MID:  return resp_mid_latency;
    L2TLB_LATENCY_LONG: return resp_long_latency;
endcase
~~~

返回值只决定 due_sample_seq；单 response 端口竞争或 ordered head 未到期会使真实完成更晚。

### 6.2 select_due_response()

抽象功能描述：该函数从 pending_q 选择最多一笔已到期 record，并移动到唯一 driving slot。

~~~systemverilog
if (stopping || !resp_reorder_en) begin
    if (pending_q[0].due_sample_seq > next_sample_seq)
        return 1'b0;
    selected_index = 0;
end else begin
    foreach (pending_q[idx])
        if (pending_q[idx].due_sample_seq <= next_sample_seq)
            eligible_indices.push_back(idx);
    std::randomize(choice) with { choice < eligible_count; };
    selected_index = eligible_indices[choice];
end

if (pending_q[selected_index].accept_flush_event_seq !=
    last_seen_flush_event_seq)
    `uvm_fatal(...);
driving_req = pending_q[selected_index];
pending_q.delete(selected_index);
driving_valid = 1'b1;
cycle_tr = driving_req.resp_tr;
~~~

文字伪代码：

~~~text
pending 为空则不选；
ordered 或 stopping 只看队头，队头未到期则整个队列等待；
reorder 扫描全部 due 项并随机选择一个；
选择项的 accept flush 版本必须仍是 latest，否则 fatal；
从 pending 移入 driving，返回冻结 response；
移动后 token 仍属于 outstanding，不增加 completed。
~~~

### 6.3 complete_driving_response()

抽象功能描述：该函数在下一 sample确认上一 cycle response 已被 DUT 固定采样，并在真实完成点回填 UID record。

~~~systemverilog
if (complete_sample_seq < driving_req.due_sample_seq)
    `uvm_fatal(...);
record_update_count = data.update_uid_tlb_records_by_entry(
    driving_req.lookup_key, driving_req.entry_snapshot);
driving_req = null;
driving_valid = 1'b0;
completed_count++;
check_l2tlb_lifecycle_accounting("response_complete");
~~~

回填使用 request-time key 和 entry snapshot，保证软件 PTE-ready 不早于 DUT response sample。

## 7. Flush、reset、owner 和 stop

### 7.1 handle_l2tlb_flush_event() 与 record_flush_killed_request()

抽象功能描述：flush helper 删除新 event 之前接受但尚未 driving 的 pending，建立 V2 filter 清空 hold，并把同一 sample 由旧 ready形成的真实 fire记为 canceled token。

~~~text
倒序扫描 pending_q，删除 accept_flush_event_seq < event_seq 的 record；
删除数加入 flush_canceled_count；
更新 last_seen_flush_event_seq；
若 event 属于 owner 启动前已存在的历史 event：只更新
pre_ready_hold_until_sample，不调用 active flush handler，不建立 barrier 或取消 token/UID；
accept_hold_until_sample = sample_seq + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES；
清ready_opportunity_since_lifecycle_block，hold解除后必须先重新发送一拍ready；
若 ready 曾开放、event_time 等于当前 $time 且 request_fire 为 1：
  record_flush_killed_request 分配 token；
  accepted_count 和 flush_canceled_count 各加 1；
  不读 CSR、不建 entry、不入 pending、不返回 response；
检查生命周期等式。
~~~

迟到 event 在进入 helper 前已经 fatal，不能错误取消新 flush 后的 request。startup 阶段 ready从未开放，只建立保守 hold，不创建 killed token。

### 7.2 生命周期 owner 与 stop

抽象功能描述：owner状态保证两个 sequence实例不会各自维护一套 queue；stop只关闭新请求并排空已有正常 response。

~~~text
global stop 或 idle stop 置 stopping；
stopping 使 next_ready=0，response 选择强制 ordered；
首次可接受边界前 acceptance_opened_since_reset=0，idle counter保持0；
每次flush hold后ready_opportunity_since_lifecycle_block=0，重新发送一拍ready前idle counter仍保持0；
pending/driving 存在时继续逐拍回复；
outstanding 为空后发送最终 ready=0、resp_valid=0 的 cycle item；
body 检查账本、确认 outstanding 为 0 并 release owner。
~~~

## 8. Response transaction 与 G/U 字段

### 8.1 create_l2tlb_xaction() 与 clear_l2tlb_xaction()

抽象功能描述：create函数分配 xaction并调用clear函数；clear函数为 ready、response和全部 payload建立确定的 inactive基线。

关键行为：

- request ready/valid、response valid默认清零。
- S1/S2 tag、ASID/VMID、PTE permission、PPN、index、PF/AF/GPF/GAF和 gap字段全部清零。
- pre_pkt_gap和post_pkt_gap固定为0，任何非零值由sequence和driver fatal。

### 8.2 fill_dtlb_resp_from_entry()

抽象功能描述：该函数把冻结的 memblock_tlb_entry 映射为 DTLB response payload，负责字段链完整性而不负责调度。

~~~systemverilog
resp.io_ptw_resp_bits_s1_entry_perm_g = entry.pte_g;
resp.io_ptw_resp_bits_s1_entry_perm_u = entry.pte_u;
...
resp.io_ptw_resp_bits_s2_entry_perm_g = entry.pte_g;
resp.io_ptw_resp_bits_s2_entry_perm_u = entry.pte_u;
~~~

字段链：

~~~text
entry_snapshot.pte_g/pte_u
  -> L2tlb_agent_agent_xaction S1/S2 perm_g/perm_u
  -> L2tlb_agent_agent_driver::send_pkt()
  -> L2tlb_agent_agent_interface drv_cb
  -> L2tlb_agent_connect takeover
  -> RTL _inner_ptw_io_tlb_1_resp_bits_s1/s2_entry_perm_g/u
~~~

L2tlb_agent_agent_monitor 在 mon_cb 侧采样这些 response 字段并执行 X/Z 检查；monitor 是观测链路，不参与 pending queue 选择、token 完成或 UID 回填。

S1/S2接口字段已经分开，但当前 sequence 用同一份 entry.pte_g/pte_u 填两阶段字段。独立 S1/S2权限、stage2 legal-leaf 和 directed GPF/GAF不在本专项范围。

## 9. 辅助函数和公共状态副作用

### 9.1 drain_csr_runtime_events()

抽象功能描述：该函数读取 package runtime latest并按统一序号幂等应用到公共 MMU runtime state，不消费 raw CSR 或 sfence queue。

~~~systemverilog
if (!memblock_sync_pkg::get_latest_runtime_csr_snapshot(raw_csr, raw_csr_seq))
    return;
if (require_post_reset_csr_refresh &&
    raw_csr_seq <= reset_runtime_csr_seq_baseline)
    return;
data.apply_raw_csr_runtime(raw_csr, raw_csr_seq);
csr_snapshot_valid = 1'b1;
require_post_reset_csr_refresh = 1'b0;
~~~

首份 snapshot有效前，sequence不开放 ready、不接受 fire、不选择 response，也不因等待 CSR而累计 idle stop。

每个reset窗口的首次blocked sample记录当前 package runtime snapshot sequence；后续reset sample不再覆盖该基线。reset释放后若 sequence 尚未前进，继续保持 CSR gate关闭。CSR monitor发布新的 post-reset snapshot后才清除 `require_post_reset_csr_refresh` 并允许 ready恢复，避免复用 reset 前的 latest，也避免把reset期间已发布的新snapshot反复吸收到基线中。

### 9.2 update_uid_tlb_records_by_entry()

源码位置：mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv

抽象功能描述：在 response真正完成后，按冻结 key更新所有未完成且 key完全匹配的 UID record；它允许合法的无 UID DTLB request。

~~~text
遍历 uid_tlb_record_by_uid；
跳过 null、record_valid=0 或 pte_valid=1；
对 vpn/s2xlate/asid/vmid 全匹配者复制 entry 字段并置 MEMBLOCK_STATUS_TLB_MAPPED；
match_count为0时输出 UVM_LOW info，允许 prefetch 或无 UID request；
返回 match_count供 completion 日志使用；
不修改主表 pass/fail/terminal。
~~~

## 10. 与主表框架的边界

- sequence只负责 L2TLB response lifecycle，不写主表、不分配 LSQ、不推进 ROB commit/deq。
- UID record回填只表示 TLB response已完成，不直接置主表 pass/fail/terminal。
- TLB entry查表 key是 request vpn/s2xlate与runtime CSR asid/vmid；request不携带 paddr。
- pending record保存 entry snapshot，sfence删除 live entry不会污染已接受 response。
- 合法 prefetch或独立 DTLB request可能没有 UID，零匹配只记 info。
- internal token不写入 DUT payload；DUT按 response内容匹配 request。
- responder driver每拍只搬运一个 gap为0的 cycle item；driver不维护 queue、latency、owner或 stop。
