# V2 L2TLB SFENCE/HFENCE Token 时序修正实施 Review

| 项目 | 内容 |
|---|---|
| 关联执行 plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_sfence_flush_token_timing_correction_plan_20260805.md` |
| 目标版本 | V2，`mem_ut_uvm_v2` |
| Review 范围 | L2TLB request/response transport sample、C0/C4 flush token、CSR/fence sample、UID 生命周期、release stop/final/recycle，以及对应 V2 flow 文档 |
| Review 日期 | 2026-08-09 |
| 当前结论 | `FINAL PASS`：P0/P1/B0/B1 修复后的 explicit/base compile 与 smoke 均通过，独立终审确认无 blocker。 |

## 1. 术语与抽象功能说明

| 英文术语 | 本文含义 | 代码落点 | 示例 |
|---|---|---|---|
| `sample` | 一个真实 `drv_cb` 边界采集到的 VIF 快照及其共享采样序号 | `memblock_l2tlb_drv_sample_t` | sample C 同时保存 request fire、response valid、reset 和 item provenance |
| `sample epoch` | 运行期 reset 之后的单调代际编号 | `sampled_reset_epoch`、`current_reset_epoch` | reset 前的 sample 不能在新 epoch 建 token |
| `flush event` | CSR/fence monitor 在一个 sample 归并出的失效事件 | `memblock_l2tlb_event_record_t` | C0 观察 SFENCE 后登记 event，C4 才执行取消 |
| `anchor` | flush event 首次被采样的 sample 序号 | `anchor_sample_seq` | C0 是 anchor，不能把 C4 当作事件产生时间 |
| `C0/C4` | C0 是 flush event 的 anchor sample；C4 是 V2 filter 完成该 event 清理的 due sample。 | `anchor_sample_seq`、`MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES` | C1-C3 仍可 drain 旧 response，C4 不得 response fire。 |
| `barrier` | 从 anchor 到 DUT filter 完成清理期间阻止新 ready/旧 token 选择的时序记录 | `barrier_q` | C0 建 barrier，C4 应用 barrier |
| `watermark` | 某职责已完整处理到的单调 sample/event 边界，只表示进度，不替代 token 或 release 状态。 | `*_published_seq`、`*_settled_sample_seq` | fence producer 的 watermark 到 C11 才允许 close intake。 |
| `provenance` | frozen sample/item 的来源身份，包含 owner、generation、reset epoch 和 transport sample 序号。 | `sampled_item_*`、`item_*` | stop/final 只接受同一 owner/epoch 产生的 item。 |
| `global stop` | parent 请求 owner 结束当前 lifecycle 的意图，不是已经完成的 transport close。 | `global_stop_requested`、`close_l2tlb_admission_for_release()` | owner 先结算当前 request fire，再发送 `RELEASE_STOP`。 |
| `baseline proof` | reset release 后，一次 NORMAL/inactive item 被严格后续真实 sample 证明无 ready/fire/response 的公共记录。 | `l2tlb_post_reset_baseline_done_*` | epoch 非 0 的 close/release 必须读取它。 |
| `token` | 每次真实 request fire 的独立 pending response 账本 | `memblock_l2tlb_pending_req` | 同一 VPN 的两次 fire 也有两个 token |
| `UID waiting record` | 主表 UID 等待 L2TLB response 的动态记录 | `uid_tlb_record_by_uid` | 只有真实 fire 标记过的 WAITING record 才能在 response 时完成 |
| `transport sample mailbox` | driver 与唯一 semantic owner 之间的单槽 frozen sample 状态 | sequencer slot 与 `l2tlb_transport_sample_mailbox_*` | `EMPTY -> PUBLISHED -> CONSUMED -> EMPTY` |
| `semantic owner` | 唯一解释 sample、建 token、驱动 response 和结束 release 的 L2TLB sequence | `memblock_l2tlb_base_sequence` | driver 不替 owner 修改 token/UID |
| `RELEASE_STOP` | 一次性关闭后续 request admission 的带 provenance item | `item_kind` | sample 确认 `ready=0 && fire=0` 后写 admission closed |
| `final inactive` | response/UID/adapter 都收敛后发送的最终无效 item | `RELEASE_FINAL_INACTIVE` | 它的 frozen sample 被 owner ack 后才能 begin closing |
| `terminal proof` | 某个 sample 已完成最终 transport 事实确认的不可变证明 | `sampled_final_inactive_proof_*` | 不能用 live mailbox level 代替它 |
| `recycle` | driver 在后续真实 `drv_cb` 将已 ack sample slot 归还 EMPTY | `recycle_transport_sample_at_drv_cb()` | owner ack 发生在 F，slot 在 F+1 recycle |
| `NOT_READY` | 当前 sample 的 CSR/event producer watermark 尚未齐全 | `MEMBLOCK_L2TLB_SAMPLE_NOT_READY` | 只能延后 semantic 解释，不能覆盖 terminal proof |

本文中的“抽象功能描述”只说明函数在完整 flow 中承担的外部职责；后面的文字伪代码再说明关键控制流。

## 2. 变更目标与原有问题

### 2.1 原有 transport 逻辑

原 driver 在每个 `drv_cb` 取得上一拍 item、驱动 VIF，并由 sequence 直接读取 VIF/共享 live 状态。该方式有三个
时序风险：

1. `req_valid && req_ready` 的 C0 fire 可能与同拍 SFENCE/HFENCE 混在一起，旧逻辑会把已被 DUT 接收的 request
   直接记成 killed。
2. final inactive 的物理采样、monitor 处理、semantic owner ack 和 mailbox recycle 没有统一的 frozen sample，
   release 可能卡在中间状态。
3. stop/final 是一次性边界，但旧循环把它当成持续 idle level；response 可能被清掉，UVM phase 结束前 driver 也
   没有自然退出。

### 2.2 修改后的总行为

```text
真实 drv_cb
  -> driver 采集冻结 sample
  -> monitor 同步消费同一 wrapper
  -> semantic owner 处理 sample
  -> 若 request fire：建立独立 token 和 UID fire marker
  -> 若 C0 flush：只建立 barrier，C4 才取消仍 pending 工作
  -> 若有 response：保留 valid 直到 DUT sample 真实确认
  -> global stop：只发送一次 RELEASE_STOP
  -> response/UID/raw queue 收敛：发送 RELEASE_FINAL_INACTIVE
  -> final sample 被 owner ack
  -> 下一 drv_cb recycle；owner 已 release 且 mailbox EMPTY 后 driver return
```

这部分是功能时序修正，不是简单字段适配；L2TLB response payload 的随机构造已由独立 `plan/do` 专项完成，
但不属于本 timing review 的重复验收范围；只有 NAPOT/range index 仍由其 `plan/undo` 专项负责。

## 3. 关键实现 Review

### 3.1 `sample_previous_vif()`：建立唯一 frozen transport sample

抽象功能描述：该 task 在 driver 的一个 `drv_cb` 中采集本拍物理接口、共享 sample 序号、reset epoch、
producer watermark 和上一 item 的 provenance，并将它们冻结为一份不可变 sample；它不创建 token、不修改主表。

源码位置：`mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv`，task：`sample_previous_vif()`。

```systemverilog
sample.transport_sample_seq = ++transport_sample_seq;
sample.dut_sample_seq = memblock_sync_pkg::peek_current_dut_global_sample();
sample.sample_valid = memblock_sync_pkg::dut_sample_time_valid &&
                      memblock_sync_pkg::dut_sample_time == $time &&
                      sample.dut_sample_seq != 0;
sample.sampled_reset_active = this.physical_reset_active() ||
                              memblock_sync_pkg::l2tlb_reset_active();
sample.sampled_reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
sample.sampled_req_fire = (sample.sampled_req_valid === 1'b1) &&
                          (sample.sampled_req_ready === 1'b1);
```

中文伪代码：该 task 先分配严格递增的 transport sample 序号，再读取当前 global sample；如果 monitor 没有在同一
时间点发布 global sample，就把 `sample_valid` 置为假。随后合并物理 reset 与 runtime reset，冻结当前 epoch，
并在保留 4-state 值的前提下只把明确的 `1 && 1` 计算为 request fire。`physical_reset_active()` 只负责判断
driver 侧是否需要保持接口安全，不会替代 runtime reset coordinator。

### 3.2 `publish_transport_sample()`：发布与 mailbox 状态分离

抽象功能描述：该函数把 frozen sample 包装成 UVM object，在唯一 semantic slot 空闲且 owner 有效时先 reserve
slot，再同步交给 monitor analysis port，最后才发布给 owner；它是 mailbox 从 EMPTY 进入 PUBLISHED 的唯一公共入口。

源码位置：同一 driver，函数：`publish_transport_sample()`。

```systemverilog
publish_semantic_sample = memblock_sync_pkg::l2tlb_lifecycle_owner_claimed &&
                          !suppress_semantic_samples_after_final &&
                          !reset_quiescent && this.transport_slot_empty();
if (publish_semantic_sample) begin
    transport_slot_owner.publish_transport_sample(wrapper);
end
transport_sample_ap.write(wrapper);
if (publish_semantic_sample) begin
    memblock_sync_pkg::mark_l2tlb_transport_sample_mailbox_nonempty();
    transport_slot_owner.notify_transport_sample_published();
end
```

中文伪代码：函数先复制并冻结 sample。只有 owner 已 claim、没有 final 抑制、不在 reset-quiescent 且单槽为空时，
才 reserve sequencer slot；随后通过 analysis port 同步调用 monitor，保证 monitor 对 final sample 的 settled 标记先于
owner 处理。slot reserve 与 monitor write 都成功后，函数才把公共 mailbox 标为 non-empty，并唤醒 owner。
`mark_l2tlb_final_inactive_at_drv_cb()` 不参与这一步的 mailbox 写入，因此 final proof 不会反过来阻止自己的 wrapper
发布。

### 3.3 `send_l2tlb_cycle()`：C0/C4 与 stop/final 时序

抽象功能描述：该 task 消费一份 frozen sample，完成 response fire、request fire、flush barrier、下一 item
选择和 release 状态推进；它是唯一 semantic owner 调度 token 和 response 的主循环。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`，task：`send_l2tlb_cycle()`。

```systemverilog
if (request_fire()) begin
    capture_fired_request();
end

if (!release_close_requested && stopping) begin
    mark_l2tlb_owner_admission_settled(lifecycle_owner_name, sample_seq);
    release_generation = close_l2tlb_admission_for_release(
        lifecycle_owner_name, sample_seq);
    release_close_requested = 1'b1;
end

if (release_close_requested && !l2tlb_release_admission_closed) begin
    cycle_tr.io_ptw_req_0_ready = 1'b0;
    stamp_lifecycle_item(cycle_tr, MEMBLOCK_L2TLB_ITEM_RELEASE_STOP,
                         release_generation, 1'b0);
end else if (release_close_requested) begin
    cycle_tr.io_ptw_req_0_ready = 1'b0;
    stamp_lifecycle_item(cycle_tr, MEMBLOCK_L2TLB_ITEM_NORMAL, 0, 1'b0);
end
```

中文伪代码：先处理当前 sample 已经真实发生的 request/response fire；C0 同拍观察到 flush 时仍调用
`capture_fired_request()`，建立 token，不立即删除。global stop 只在尚未提出 close request 时生成一次
generation。close 尚未被 driver sample 确认时，发送带 generation 的 `RELEASE_STOP`；一旦 confirmed，后续只
发送 ready=0 的普通 inactive item，不再重复 confirm。若当前 item 已经带有 selected response，只改变 ready，
不清除 response valid，直到下一 sample 真实确认 response fire。到 C4，`apply_due_l2tlb_flush_barriers()` 才
删除仍 pending 的旧 token，并取消拥有 request-fire marker 的 UID waiting record。

### 3.4 final proof 优先级

抽象功能描述：该分支消费 final inactive 的 frozen transport proof，确认 monitor 已同步处理同一 sample，建立
release closing 并 ack sample；它不依赖 CSR/event semantic watermark。

源码位置：同一 sequence，task：`send_l2tlb_cycle()` 的 final proof 分支。

```systemverilog
if (sample.sampled_final_inactive_proof_valid) begin
    if (!final_item_sent ||
        sample.sampled_item_kind != MEMBLOCK_L2TLB_ITEM_RELEASE_FINAL_INACTIVE ||
        sample.sampled_req_ready !== 1'b0 || sample.sampled_req_fire ||
        sample.sampled_resp_valid !== 1'b0 ||
        !memblock_sync_pkg::monitor_final_sample_settled(
             sample.sampled_reset_epoch, sample.transport_sample_seq)) begin
        `uvm_fatal(get_type_name(), "invalid L2TLB final inactive proof")
    end
    memblock_sync_pkg::begin_l2tlb_release_closing(lifecycle_owner_name);
    ack_l2tlb_transport_sample(sample.transport_sample_seq,
                                MEMBLOCK_L2TLB_SAMPLE_CONSUMED);
    return;
end
```

中文伪代码：只要 driver 在有效 sample 中给出 final proof，sequence 先验证 item kind、owner/generation/epoch、
ready/fire/response 和 monitor settled tuple；任一不一致就 fatal。验证成功后调用
`begin_l2tlb_release_closing()` 锁定 release 阶段，再用同一个 sample ack helper 完成 CONSUMED。该分支位于
`NOT_READY` 判断之前，因此 producer watermark 暂时未齐不会丢失 terminal handoff。

final item 的建立还要求 `barrier_q` 已为空。也就是说，response/UID 已为空并不等于本地 C4 已执行；必须先由
`apply_due_l2tlb_flush_barriers()` 消费所有 due barrier，才允许把 final sample 交给 driver。

### 3.5 `recycle_transport_sample_at_drv_cb()`：一次性 recycle 与自然退出

抽象功能描述：该函数在后续真实 driver callback 中回收 owner 已 ack 的 slot，并更新公共 EMPTY proof；如果回收
的是 final sample，则清理 final provenance，且为 driver 自然退出设置一次性抑制状态。

源码位置：`mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv`，函数：`recycle_transport_sample_at_drv_cb()`。

```systemverilog
if (transport_slot_owner.get_recyclable_transport_sample_seq(recycle_seq)) begin
    transport_slot_owner.recycle_transport_sample(recycle_seq);
    memblock_sync_pkg::mark_l2tlb_transport_sample_recycled(recycle_seq);
    memblock_sync_pkg::mark_l2tlb_transport_sample_mailbox_empty();
    if (recycle_seq == memblock_sync_pkg::l2tlb_release_final_inactive_transport_sample_seq) begin
        suppress_semantic_samples_after_final = 1'b1;
        last_item_kind = MEMBLOCK_L2TLB_ITEM_NORMAL;
        last_item_reset_epoch = 0;
    end
end
```

中文伪代码：函数先检查 slot 是否已经被 owner 以 CONSUMED/DROPPED 终态确认；没有可回收 sample 就返回。可回收
时先清 sequencer slot，再写公共 recycle/EMPTY 证明。若序号等于 final sample，才清 final item provenance，
防止下一 callback 重复调用 final confirm，并置 `suppress_semantic_samples_after_final`。主 driver loop 在下一
callback 看到该抑制、owner 已 release、transport slot 和 mailbox 都为空时直接返回，完成自然退出。

### 3.6 `update_last_driven_metadata()`：防止 terminal provenance 被 idle 覆盖

抽象功能描述：该函数记录上一 item 的最小 provenance，供下一真实 sample 解释 VIF；对于尚未证明完成的 baseline
或 final item，普通 idle/NOT_READY item 不能覆盖其 metadata。

源码位置：同一 driver，函数：`update_last_driven_metadata()`。

```systemverilog
if (got_item && tr != null &&
    post_reset_baseline_pending && last_item_is_post_reset_baseline &&
    !tr.is_post_reset_baseline &&
    tr.item_kind == MEMBLOCK_L2TLB_ITEM_NORMAL &&
    last_item_reset_epoch == sampled_reset_epoch &&
    tr.item_reset_epoch == sampled_reset_epoch && !sampled_reset_active) begin
    if (tr.io_ptw_req_0_ready !== 1'b0 || tr.io_ptw_resp_valid !== 1'b0) begin
        `uvm_fatal(get_type_name(), "baseline pending NORMAL item must remain transport inactive")
    end
    return;
end
if (got_item && tr != null) begin
    last_item_kind = tr.item_kind;
    last_item_generation = tr.item_generation;
    last_item_reset_epoch = tr.item_reset_epoch;
    last_item_owner_name = tr.item_owner_name;
    last_item_is_post_reset_baseline = tr.is_post_reset_baseline;
end else if (post_reset_baseline_pending && last_item_is_post_reset_baseline &&
             last_item_reset_epoch == sampled_reset_epoch && !sampled_reset_active) begin
    return;
end else if (last_item_kind == MEMBLOCK_L2TLB_ITEM_RELEASE_FINAL_INACTIVE &&
             !suppress_semantic_samples_after_final &&
             last_item_reset_epoch == sampled_reset_epoch &&
             !sampled_reset_active) begin
    return;
end else begin
    last_item_kind = MEMBLOCK_L2TLB_ITEM_NORMAL;
    last_item_generation = 0;
    last_item_reset_epoch = 0;
    last_item_owner_name = "";
end
```

中文伪代码：若 baseline 尚待严格更晚 sample 证明，先判断本轮只是同 epoch 的普通 NORMAL/inactive 或没有 item；二者
都只代表当前 semantic sample 尚未可解释，不能覆盖上一 baseline 的 driver provenance；若该普通 item 的 ready 或
resp_valid 非零则直接 fatal，不能把活动 transport 伪装成 idle。否则，有真实 item 时保存其
kind、generation、epoch、owner 和 baseline tag；没有 item 时，如果 final 尚未 recycle 且仍属当前 epoch，就保留 final
provenance；否则清为普通 idle。这样 `NOT_READY`、无 anchor 或 sequencer 暂时没有 item 都不会让 baseline/final
一次性 transport 事实提前丢失。

## 4. Review 发现与修正记录

| 发现 | 原因 | 修正 | 结果 |
|---|---|---|---|
| final proof 先改 mailbox 为 non-empty | proof helper 早于 wrapper 占槽执行，publish predicate 自己拒绝发布 | mailbox non-empty 只由成功 publish 写入 | final sample 可被 owner 消费 |
| stop 每拍重复发送 | close request 被当作 level，而不是一次 cutoff | closed 前只发一次 STOP，closed 后 NORMAL inactive | 不重复 confirm，不吞 response |
| stop/final 依赖 `READY` | semantic watermark 暂时未齐会丢 terminal transport 事实 | stop/final 以有效 frozen sample 为边界；final 优先于 NOT_READY | terminal handoff 可收敛 |
| final metadata 被 idle 覆盖 | item provenance 没有保存到 recycle 边界 | recycle 前保留，recycle 后清理 | 不重复 final confirm |
| driver 依赖 phase kill | owner release 后采样线程仍无限循环 | final recycle + owner release + 两个 EMPTY 条件满足后 return | lifecycle 自然退出 |
| release gate 遗漏本地 barrier | stop 后 token/UID 已为空就提前发 final | final 还必须等待 `barrier_q` 清空，owner-release audit 也检查该条件 | C4 本地账本与 adapter 同步收敛 |
| `NOT_READY` 覆盖 baseline provenance | 非 baseline NORMAL/inactive 写入 metadata 时没有保护 pending baseline | pending 期间保留同 epoch baseline tag，直到真实 proof 或 reset | 非零 epoch 可产生公共 proof，不会卡住 close/release |
| epoch 0 调用 public proof helper | startup baseline 与 runtime reset 共用分支 | epoch 0 只清 driver-local pending，非零 epoch 才写公共 proof | startup 不触发非法 epoch fatal |

### 4.1 P0：flush hold 误禁止 C1-C3 response

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`，`send_l2tlb_cycle()`。

修改前：response selector 要求 `!hold_active`，并且 hold 期间若 `cycle_tr.io_ptw_resp_valid` 为 1 直接 fatal。这样
C0 建 barrier 后，C1-C3 的旧 token 即使已经 due 也无法发 response。

修改后源码片段：

```systemverilog
if (cycle_tr == null && csr_snapshot_valid && request_csr_history_valid &&
    !due_barrier_this_sample) begin
    response_selected = select_due_response(sample_seq + 1, cycle_tr);
end

next_ready = !stopping && !release_close_requested &&
             csr_snapshot_valid && request_csr_history_valid &&
             !hold_active && outstanding_count() < max_outstanding;
```

中文伪代码：hold 生效时，新的 request 一律保持 `ready=0`；旧 token 若下一 sample 不会落入 C4 due，则仍可被选为
response。当前 sample 到 C4 时，`due_barrier_this_sample` 阻止选择，已 driving 的 response 也由前置检查 fatal，不能跨过
filter 清理边界。

状态副作用：C1-C3 的 `pending_q -> driving_req -> complete` 可以继续推进；`accept_hold_until_sample` 不再错误清除
`io_ptw_resp_valid`，但仍保留对 `next_ready` 的 admission 限制。

### 4.2 P1：baseline 只在 driver 私有状态中完成

源码位置：

- `mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv`，`sample_previous_vif()`；
- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`，
  `close_l2tlb_admission_for_release()` 与 `release_grantable()`。

修改前：driver 在后续无活动 sample 中只清 `post_reset_baseline_pending`，公共 package 没有 proof；close/release
gate 无法确认当前 reset epoch 的 transport baseline 已存在。

修改后源码片段：

```systemverilog
if (sample.sample_valid && sample.dut_sample_seq > baseline_sent_sample_seq) begin
    if (sample.sampled_req_ready !== 1'b0 ||
        sample.sampled_req_fire || sample.sampled_resp_valid) begin
        `uvm_fatal(get_type_name(), "post-reset baseline proof observed active transport")
    end
    memblock_sync_pkg::mark_l2tlb_post_reset_baseline_done(
        sample.sampled_reset_epoch, sample.dut_sample_seq);
    post_reset_baseline_pending = 1'b0;
end
```

中文伪代码：baseline item 发送后，driver 必须等到严格更晚的有效 sample；若该 sample 的 ready、request fire 或
response valid 非零则 fatal。只有三者均为零时，driver 先把 `{epoch, sample}` 写成公共 proof，再清私有 pending。
close 对非零 epoch 缺 proof 直接 fatal，`release_grantable()` 对同样条件返回 0；epoch 0 不加入此限制。

状态副作用：所有 release consumer 读同一份 `l2tlb_post_reset_baseline_done_*`，reset direct writer 清该 proof 后不会
让旧 epoch 泄漏到 re-arm 后 lifecycle。

### 4.3 B0：`NOT_READY` 普通 inactive 覆盖待证明 baseline

源码位置：`mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv`，
`update_last_driven_metadata()`。

修改前：sequence 在 `sample_valid=0` 或 `sample_ready_result=NOT_READY` 时合法发送一个普通
`NORMAL/inactive` item。driver 原本把它视为新的上一 item，覆盖此前 tagged baseline 的
`last_item_is_post_reset_baseline`。下一次真实 sample 即使接口无活动，也不会进入 baseline proof 分支。

修改后源码片段：

```systemverilog
if (post_reset_baseline_pending && last_item_is_post_reset_baseline &&
    !sampled_reset_active && last_item_reset_epoch == sampled_reset_epoch &&
    ((!got_item) || (tr != null && !tr.is_post_reset_baseline &&
                     tr.item_kind == MEMBLOCK_L2TLB_ITEM_NORMAL &&
                     tr.item_reset_epoch == sampled_reset_epoch))) begin
    assert(tr.io_ptw_req_0_ready == 0 && tr.io_ptw_resp_valid == 0);
    return;
end
```

中文伪代码：baseline item 已发送但尚未被后续真实 sample 证明时，普通 idle 或 NOT_READY 的 NORMAL/inactive
不能替代上一 item 的 metadata。driver 继续以保留的 baseline tag 解释下一真实 VIF sample；该 sample 确认
ready/fire/response 都为零后，才完成 proof 并允许 metadata 在以后正常替换。

状态副作用：只改变 driver private latch 的覆盖条件；不重发 baseline，不修改 VIF wire，也不改变 sequence 的
`item_done()` 次数。

### 4.4 B1：epoch 0 不是 runtime-reset public proof 的合法输入

源码位置：同一 driver，`sample_previous_vif()` 与 `update_reset_quiescent()`；公共 helper 位于
`memblock_sync_pkg::mark_l2tlb_post_reset_baseline_done()`。

修改前：baseline proof 分支对所有 epoch 一视同仁，而公共 helper 的输入合同要求 `reset_epoch != 0`。因此正常
testcase startup 的 virtual epoch 0 可能被误判为非法 runtime-reset proof。

修改后源码片段：

```systemverilog
if (sample.sampled_reset_epoch != 0) begin
    memblock_sync_pkg::mark_l2tlb_post_reset_baseline_done(
        sample.sampled_reset_epoch, sample.dut_sample_seq);
end
post_reset_baseline_pending = 1'b0;

if (sample.sampled_reset_epoch == 0) begin
    return; // reset-quiescent runtime transaction 尚未建立
end
```

中文伪代码：epoch 0 仍需经过一拍严格更晚的无活动 sample，保证启动期 driver 状态收敛；但它没有 reset coordinator
创建的 runtime epoch，因此只清本地 pending。只有 epoch 非零时才记录公共 proof，并受 close/release gate 消费。

状态副作用：epoch 0 的 startup topology、owner 和 release 行为保持原样；非零 reset epoch 仍必须拥有公共 proof，
不会放宽 runtime-reset 的安全条件。

## 5. 与原测试框架逻辑对比

| 修改类型 | 原逻辑 | 修改原因 | 修改后逻辑 |
|---|---|---|---|
| 功能逻辑修正 | C0 同拍 flush 直接删除 request；C0 fire 被视为 killed | DUT filter 在 C4 才完成 flush | C0 建 token，C4 才取消仍 pending token/UID |
| 功能逻辑修正 | flush hold 同时禁止 ready 和 response | C1-C3 仍可能有合法旧 response | hold 只关闭新 ready，旧 response 保持到真实 fire |
| 功能逻辑修正 | stop item 每拍重复生成并清 response | 一次 admission cutoff 被错误建模成 level | STOP 只确认一次，closed 后 inactive item 保留 response |
| 功能逻辑修正 | final proof 依赖 semantic ready，idle 会覆盖 final metadata | transport terminal 事实与 CSR/event watermark 是两种时序 | final proof 优先处理，metadata 保留到 recycle |
| 功能逻辑修正 | driver 在线程中持续采样直到 phase 被杀 | 可能泄漏 owner/slot 生命周期，难以诊断 | final recycle、owner release、slot/mailbox EMPTY 后自然 return |
| 功能逻辑修正 | `NOT_READY` 的普通 inactive 覆盖待确认 baseline metadata | baseline proof 只能读取上一 item provenance，覆盖后无法证明 | pending baseline 保留 provenance 到严格更晚 real sample 或新 reset epoch |
| 功能逻辑修正 | epoch 0 调用仅接受 runtime-reset epoch 的 public proof | helper 明确拒绝零 epoch，startup 可能 fatal | epoch 0 完成本地 baseline；仅非零 epoch 写/读公共 proof |
| 字段/接口保持 | request/response payload interface 不变 | 本 plan 只修时序 | payload 字段仍由现有 xaction/driver 链路传递 |

## 6. 历史验证与当前重新验收要求

以下是 P0/P1 修复后、B0/B1 修复前的历史重新验收命令：

```text
make eda_compile tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=l2tlb_timing_p01_20260809 cfg=tc_dispatch_real_smoke
make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=l2tlb_timing_p01_20260809 cfg=tc_dispatch_real_smoke wave=off \
  plus_arg='+MEMBLOCK_MAIN_TRANS_NUM=2'
```

历史结果：VCS compile exit code 0；仿真在 `482.800ns` 结束并输出 `TEST_PASS`，`UVM_ERROR=0`、`UVM_FATAL=0`。
该结果覆盖 owner stop、旧 response drain、final sample publish/ack/recycle 和 global stop 收敛，但不覆盖 B0/B1 后的
driver metadata/proof 分支，不能作为本专项最终验收。

基础回归命令：

```text
make eda_compile tc=basicTest ts=virtual_base_sequence mode=base_fun
make eda_run tc=basicTest ts=virtual_base_sequence mode=base_fun wave=off
```

历史结果：VCS compile exit code 0；基础 sequence 在 `265.300ns` 结束并输出 `TEST_PASS`，`UVM_ERROR=0`、
`UVM_FATAL=0`。该结果确认当时的 no-dispatch 默认启动路径没有被 baseline public gate 误阻塞；B0/B1 后必须重跑。

### 6.1 B0/B1 后重新验收结果

已按本节同一组远端命令重新执行：

| 验收项 | compile 结果 | run 结果 | 结论 |
|---|---|---|---|
| `memblock_dispatch_real_smoke_vseq`，`l2tlb_timing_p01_20260809` | exit code 0 | `482.800ns TEST_PASS`，`UVM_ERROR=0`、`UVM_FATAL=0` | active dispatch lifecycle 通过 |
| `virtual_base_sequence`，`base_fun` | exit code 0 | `265.300ns TEST_PASS`，`UVM_ERROR=0`、`UVM_FATAL=0` | no-dispatch startup 路径通过 |

`KDB-OPTIONS` 和旧 `.nfs*` 文件清理提示不包含 VCS compile error，也没有形成 UVM error/fatal。B0 的
inactive fail-fast 在正常 smoke 中未触发；B1 的 epoch-0 路径在 base smoke 中未出现非法 public proof fatal。

仿真由 `mem_ut/ver/ut/memblock/sim` 的 `eda_compile/eda_run` 远端目标完成。当前工作树没有
`.envrc`，本地 shell 的 `source` 提示文件不存在，但 Makefile 远端 bootstrap 成功加载 VCS，实际编译和运行
结果有效。VCS 日志中的 `KDB-OPTIONS`、默认 sequence `UVM_WARNING` 和远端临时目录清理提示未产生
`UVM_ERROR/UVM_FATAL`，不影响本专项通过。

本阶段不评价以下相邻专项的功能完成度：

- `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_response_random_payload_plan_20260729.md`：payload builder、独立
  S1/S2 fault/permission/level/PPN 已由其自身专项实现并归档；本 timing review 不把该实现重新计入本阶段。
- `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_range_lookup_napot_plan_20260806.md`：range index、NAPOT coverage rank
  和重叠 candidate 仲裁已由提交 `6a1b2d947e` 实现并归档；本 timing review 不重新计入其完成度。

P0 与 P1 专项 subagent 已在修复后分别给出 `PASS`；其后 B0/B1 又修改了 driver 的 baseline/proof 分支，因此历史
验证和旧 `FINAL PASS` 不能作为当前结论。本 agent 末轮复核还发现并删除了
`memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once()` 中同一 service tick 的
重复 `collect_runtime_context_events()` 调用；删除后源码与 plan 的“每个 dispatch sample 恰好调用一次”合同一致。

## 7. 源码 Diff 覆盖检查

本节以本轮 `git diff` 为基准逐文件核对。下表中的“覆盖结论”表示该文件的有逻辑意义修改已在本 review 或关联
plan 中说明；纯 package 引入、连接检查和注释调整也明确列出，避免把文件遗漏误认为“没有逻辑变化”。

| 修改文件 | 本轮逻辑变化 | 覆盖位置/结论 |
|---|---|---|
| `agent/L2tlb_agent_agent/L2tlb_agent_agent_pkg.sv` | 导入共享 lifecycle/sample 类型，供 agent transport 对象使用 | §3.1-§3.6；字段来源与 package 依赖已覆盖 |
| `agent/L2tlb_agent_agent/src/L2tlb_agent_agent.sv` | connect 阶段检查 driver/sequencer/monitor，并绑定 slot owner 和同步 analysis port | §3.2、§3.5；空组件和重复连接会 fatal |
| `agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv` | 唯一 `drv_cb` 采样、4-state fire、reset quiescent、baseline、stop/final provenance、sample publish/recycle、自然退出 | §3.1、§3.2、§3.5、§3.6；覆盖 driver 主循环及关键 helper |
| `agent/L2tlb_agent_agent/src/L2tlb_agent_agent_monitor.sv` | 删除独立 transport 采样，改为同步 analysis imp；保留 X/Z、passive req 诊断和 reset/final settled 记录 | §3.2、§3.4；monitor 不建 token、不拥有 mailbox |
| `agent/L2tlb_agent_agent/src/L2tlb_agent_agent_sequencer.sv` | 增加单槽 `EMPTY/PUBLISHED/CONSUMED/DROPPED` 状态和 publish/ack/recycle API | §3.2、§3.5；slot 不能覆盖未消费 sample |
| `agent/L2tlb_agent_agent/src/L2tlb_agent_agent_xaction.sv` | 增加冻结 transport payload wrapper 和非 DUT lifecycle metadata | §3.1-§3.6；metadata 不连接 DUT payload |
| `agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv` | CSR monitor 唯一推进 global sample，发布 C-2 history，负责 runtime reset 起止与 producer ack | §2、§3.1；CSR change 事件与权限/debug snapshot 分离 |
| `agent/fence_agent_agent/src/fence_agent_agent_monitor.sv` | 读取同拍 CSR anchor，记录 fence event/sample，参与 producer barrier，reset 时只清本地状态 | §2、§3.3；不自行推进 global sample |
| `agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv` | ctrl raw 的 cancel/MMIO sample 固定使用共享 anchor，reset 期间不消费 | §7 覆盖矩阵；避免消费时重写 sample 序号 |
| `agent/redirect_agent_agent/src/redirect_agent_agent_monitor.sv` | redirect anchor 使用共享 sample anchor，reset 期间不发布 | §7 覆盖矩阵；不改 status/pass/fail |
| `cfg/memblock_compile_params.svh` | 增加 CSR history、sample mailbox、probe、baseline、reset watchdog 等编译期上限 | §1 术语/plan §1.1；均为同步协议上限，不是 runtime plus |
| `common/memblock_common/src/memblock_sync_pkg.sv` | 增加 global sample、producer mask、CSR history、event history、reset ack、release grant、mailbox proof 和 raw-fence intake 状态 | §2-§3、plan §4-§6；direct-writer 合同由 plan 明确 |
| `seq/base_seq/memblock_l2tlb_base_sequence.sv` | owner claim、frozen sample 消费、C0 token capture、C4 barrier cancel、response-visible CSR、final release | §3.3-§3.6；sequence 是唯一 semantic owner |
| `seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv` | pending LSQ sample 改为只读共享 global sample | §7 覆盖矩阵；不再创建第二 sample writer |
| `seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv` | dispatch service 先处理 lifecycle context，parent 发 final grant，等待 owner claim；删除同拍重复 context service | §3.3、§5；最终每 sample 只服务一次 |
| `seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv` | directed sideband watermark 改读 shared global sample | §7 覆盖矩阵；不改变 MMIO 语义本身 |
| `seq/base_seq_help/common_data_transaction.sv` | table reset 不回退 sample；增加 TLB key 比较、live entry 清理、UID request-fire marker/cancel/response completion | §3.3、§5；UID 账本与主表状态分离 |
| `seq/base_seq_help/dispatch_monitor_event_adapter.sv` | adapter 负责 CSR/fence service、C4 live-entry 删除和 raw event 生命周期 | §3.3、plan §5；不由 responder sequence 重复消费 raw fence |
| `seq/base_seq_help/memblock_dispatch_base_sequence.sv` | 注册/注销唯一 adapter service，reset 时停止后续事件消费 | §3.3、§5；owner 交接由 lifecycle 管理 |
| `seq/base_seq_help/memblock_dispatch_types.sv` | 暴露 V2 L2TLB flush hold 等 compile-time 类型/常量 | §7 覆盖矩阵；与 compile 参数保持单一来源 |
| `seq/base_seq_help/memblock_tlb_entry.sv` | UID TLB record 增加 request-fire/correlation 生命周期字段和状态 helper | §3.3、§5；未把 token 强绑单 UID |
| `seq/virtual_sequence/memblock_dispatch_real_cancel_reconcile_vseq.sv` | explicit L2TLB sequence 启动前等待统一 barrier | §7 覆盖矩阵；避免 main table 尚未 ready 时 claim |
| `seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv` | 增加 explicit start barrier helper，并按 virtual sequencer 启动 responder | §7 覆盖矩阵；不改变 legacy default topology |
| `tb/L2tlb_agent_connect.sv` | connect takeover 与 dispatch topology 解耦，默认接口保持 inactive | §4.1、plan IMPLEMENTATION_DELTA；不把 connect capability 当 owner |
| `tb/top_tb.sv` | time 0 初始化 sample coordinator | §4.1、§5；后续 software reset 不回退 sample |
| `tc/src/tc_dispatch_real_smoke.sv` | 显式声明 dispatch topology active | §4.1；testcase 是 topology 写者 |
| `AI_DOC/mem_ut_flow_doc/tlb_l2tlb_responder_flow.md` | 增加 V2 当前 transport/lifecycle 合同，明确旧章节仅作历史对比 | §7.3；当前 responder flow 的权威入口已更新 |
| `AI_DOC/mem_ut_flow_doc/sfence_flow.md` | 将文档职责收敛为 entry-level invalidation，并链接 V2 responder lifecycle flow | §7.3；不再把旧直接 drain 链路当作 V2 transport 入口 |

### 7.1 关键公共状态覆盖

`memblock_sync_pkg.sv` 中的新增状态按职责分成四组：global sample/CSR history 用于跨 monitor 的同拍对齐；
event/barrier history 用于 C0 登记与 C4 到期；token/UID cancel 用于 response-owner 账本；reset/release/mailbox
字段用于生命周期收敛。没有任何 monitor 直接清 token，也没有 driver 直接修改主表或 UID semantic 状态。

### 7.2 主流程子调用展开

| 调用顺序 | 子函数 | 在本流程中的功能 | 返回主流程后的影响 |
|---|---|---|---|
| 1 | `collect_runtime_context_events()` | 由 dispatch parent 先同步 CSR、再把 raw fence 交给 adapter 的 C0/C4 live-entry owner | reset 未收敛时立即返回，后续 batch 不消费 stale raw |
| 2 | `wait_for_l2tlb_transport_sample()` | 从 sequencer 单槽取得 driver 已发布且 monitor 已同步处理的冻结 sample | sequence 只处理该 sample，不读取 live VIF |
| 3 | `apply_due_l2tlb_flush_barriers()` | 在 due sample 删除仍 pending token，并按 fire marker 取消 UID waiting | due barrier 消费后才允许重新计算 ready |
| 4 | `complete_driving_response()` | 用 response-visible C-2 CSR 完成 token，并回填匹配 UID | response raw-hit 不使用 UID 建立时旧 CSR |
| 5 | `ack_l2tlb_transport_sample()` | 把本 sample 标记为 consumed/dropped，释放单槽回收资格 | driver 后续 callback 才能 recycle，不会覆盖 sample |
| 6 | `grant_l2tlb_final_release()` | parent 在 global stop 后向仍持有 owner 的 sequence 发一次 release grant | sequence 完成 final proof 后原子 release 并自然退出 |

### 7.3 Flow 文档同步检查

`tlb_l2tlb_responder_flow.md` 新增了当前 V2 的 sample、mailbox、C0/C4、adapter 和 release grant 调用链；
`sfence_flow.md` 明确只保留 entry-level invalidation 基线，并将 V2 transport 生命周期指向 responder flow。
因此 flow 文档不再把独立 L2TLB monitor `@mon_cb`、sequence 直接消费 latest flush 或旧
`record_flush_killed_request()` 描述为当前 V2 行为。

## 8. 实现与 Plan 不一致项

相对于执行前 plan 加上 `IMPLEMENTATION_DELTA` 后的最终执行合同，未发现实现与 Plan 不一致项。需要特别区分：
以下四类是 coding 过程中发现并已经用显式 `[IMPLEMENTATION_DELTA]` 写回执行 plan 的调整，不能倒推为原始
plan 在 coding 前已经包含，但当前源码均与这些 delta 一致：

| `IMPLEMENTATION_DELTA` | 当前实现落点 | 对齐结论 |
|---|---|---|
| no-dispatch testcase 不启动 responder | `tc_dispatch_real_smoke.sv`、`memblock_l2tlb_base_sequence::body()`、connect macro | 与 delta 一致；connect capability 和 dispatch topology 分离 |
| sample coordinator 不覆盖 topology | `memblock_sync_pkg::initialize_l2tlb_sample_coordinator()` | 与 delta 一致；重复初始化在 lifecycle active 后 fatal |
| final proof 与 mailbox PUBLISHED 分离 | `mark_l2tlb_final_inactive_at_drv_cb()`、`publish_transport_sample()` | 与 delta 一致；只有成功 publish 才置 mailbox non-empty |
| stop/final 二次采样收敛 | driver `RELEASE_STOP`/NORMAL 分支、metadata/recycle helper | 与 delta 一致；旧 response 保留到真实 fire |
| final eligibility 等待本地 barrier | sequence final gate、`owner_release` accounting | 与 delta 一致；`barrier_q` 非空不能 release |
| baseline provenance 跨 `NOT_READY` 保留 | driver `update_last_driven_metadata()` | 与 delta 一致；普通 inactive/idle 不覆盖待证明 baseline |
| epoch 0 跳过 runtime-reset public proof | driver `sample_previous_vif()`、`update_reset_quiescent()` | 与 delta 一致；仅非零 epoch 写/读公共 proof |
| 同步 V2 responder 与 SFENCE flow 文档 | `AI_DOC/mem_ut_flow_doc/tlb_l2tlb_responder_flow.md`、`AI_DOC/mem_ut_flow_doc/sfence_flow.md` | 与 delta 一致；当前 flow 入口和历史基线边界已明确 |

本轮 review 中发现的同拍重复 `collect_runtime_context_events()` 已在最终提交前删除；删除后保留的调用次数为每个
dispatch sample 一次，属于按原 plan 回正实现，不形成未解决的不一致项。

## 9. Plan 未说明但 Coding 落实的细节

未发现 Plan 未说明但 Coding 额外落实、且会改变本专项语义的细节。B0/B1 已作为新增
`IMPLEMENTATION_DELTA` 回写 plan；bounded NBA probe、4-state handshake 检查、reset/baseline watchdog、空组件检查、
单槽状态检查和 explicit vseq 启动屏障也均已在执行 plan 主体或 delta 中明确；它们是边界诊断，不是新的 payload 或 RM 功能。

## 10. 非本次修改的逻辑分析

### 10.1 `git status` 对比结论

本次 review 覆盖的 `mem_ut` 源码文件为 §7 表中列出的全部文件，以及本专项 plan 和 review 文档。
当前工作区另有以下内容，不纳入本专项 commit：

| 类别 | 文件/目录 | 判断 | 原因 |
|---|---|---|---|
| RTL 分析文档 | `AI_DOC/analysis/rtl/v2/index.md` | 非本次逻辑 | 仅增加 Store TLB replay flow 索引，属于独立 RTL 知识库任务 |
| RTL 分析文档 | `AI_DOC/analysis/rtl/v2/flows/store_tlb_hit_replay_and_retry_flow.md` | 非本次逻辑 | 新增 Store TLB flow 分析，不改变 mem_ut 源码 |
| 仿真产物 | `mem_ut/ver/ut/memblock/sim/.eda_remote/` | 非源码 | 远端编译/运行生成的临时同步目录，不进入 commit |
| skill 运行记录 | `.humanize/skill/` | 非源码 | 工具运行记录，不属于 L2TLB 功能实现 |

## 11. 当前结论与重新验收

P0/P1 与其后 B0/B1 修复均已写入工作树；B0/B1 后的 explicit/base compile 与 smoke 已通过。独立终审明确给出
`FINAL PASS`，本专项无已知 blocker；执行 plan 可归档到 `plan/do`。response payload 与 range/NAPOT 均已由各自独立
`plan/do` 专项完成；本 timing 专项通过不重复声称覆盖它们。

## 12. 2026-08-10 收尾修正与重新验收

本节优先级高于第 11 节中较早的 `FINAL PASS` 记录。第 11 节保留为当时工作树的历史验收；以下两项修正完成并经过
新一轮独立 review 后，才能再次给出最终结论。

### 12.1 reset release sample anchor 修正

**功能特性：** 在 runtime reset 解除的同一 clocking-block 边界，让 CSR 和 fence monitor 无论先后调度都完成同一个
global sample 的 producer barrier。

**修改前逻辑：** CSR monitor 在本拍结束 reset 后直接跳过 sample。它规避了“fence 先运行”时的半个 barrier，
却在“CSR 先运行”时让随后运行的 fence monitor 等待不存在的同拍 anchor；基础 smoke 报出
`no CSR sample anchor at time=170.000ns`。

**修改后逻辑：** CSR monitor 在 reset 实际解除后照常发布首个 post-reset sample。fence monitor 若先观察到
reset active，先完成 fence reset ack，再在同一时刻的 NBA/delta 区域复查；只有 reset 仍 active 才结束本拍，
否则继续读取 CSR anchor、采样 `sfence` 并写 FENCE done。`sfence.valid=1` 在 release 边也不会被静默丢弃。

**正确性检查：** CSR/fence 两种调度顺序都只会得到“完整同拍 barrier”或“reset 仍未结束而无 sample”两种结果；
不会留下 CSR-only sample，也不会让 fence 在无 anchor 的 release 边 fatal。

### 12.2 no-owner stop、request fail-fast 与 phase ended 修正

**功能特性：** 保证 `DISABLED/NO_OWNER + NO_DISPATCH` testcase 在 global stop 时不创建 release grant，也不掩盖残留 owner。

**修改前逻辑：** parent 在确认 responder mode 前，只要看到 `l2tlb_lifecycle_owner_claimed` 就可能调用
`grant_l2tlb_final_release()`；随后才进入 no-owner 分支。错误状态会被先赋予 grant，违反 no-owner 不 claim、不 release 的
生命周期边界。

**修改后逻辑：** no-owner 分支先验证 dispatch/response 均关闭，且 `owner_claimed`、`owner_claimed_once`、
`release_granted` 都为 0，然后直接结束等待。仅 enabled、dispatch-active、需要 response 的分支可以在真实 owner claim
存在时调用 `grant_l2tlb_final_release()`。

**正确性检查：** 正常 no-owner testcase 不再等待 owner，也不会生成 grant；如果 topology 或历史状态残留 owner/grant，
立即 `uvm_fatal`，不会把错误状态伪装成正常收尾。

**相邻防御行为：** `L2tlb_agent_agent_monitor` 在 testcase topology 已初始化、非 reset 且 responder disabled 时，
观察到 `sampled_req_valid===1` 立即 `uvm_fatal`，并打印 transport sample、DUT sample、VPN、`s2xlate` 和 topology；
它不等待永远不会出现的 `valid && ready`。`L2tlb_agent_agent_driver::phase_ended()` 若仍观察到 active owner claim，
同样立即 `uvm_fatal`，不调用 release helper、不清 claim，也不补发 inactive item。二者分别防止错误 request 在
no-owner topology 中永久悬挂，以及 phase 强制结束掩盖未收敛 owner。

### 12.3 重新验收记录

在独立 mode `l2tlb_single_owner_final` 中完成以下验收，避免遗留 `base_fun` VCS partition 进程干扰：

| 验收项 | 命令 | 结果 |
|---|---|---|
| base no-dispatch | `make eda_compile tc=basicTest ts=virtual_base_sequence mode=l2tlb_single_owner_final`；`make eda_run tc=basicTest ts=virtual_base_sequence mode=l2tlb_single_owner_final wave=off` | compile 成功；`265.300ns TEST_PASS`，`UVM_ERROR=0`、`UVM_FATAL=0` |
| real dispatch | `make eda_compile tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=l2tlb_single_owner_final cfg=tc_dispatch_real_smoke`；`make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=l2tlb_single_owner_final cfg=tc_dispatch_real_smoke wave=off` | compile 成功；`427.800ns TEST_PASS`，`UVM_ERROR=0`、`UVM_FATAL=0` |

关联执行 plan 为
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_sfence_flush_token_timing_correction_plan_20260805.md`；上述收尾
`IMPLEMENTATION_DELTA` 已同步写回。

最终独立只读 review 已完成：reset-anchor/producer-barrier 复核与 no-owner/owner-release 复核均明确给出
`FINAL PASS`，没有新增源码、plan 或 review 遗漏。结合第 12.3 节的 base/real-dispatch 重新验收，本专项当前无已知
blocker；本文件和关联执行 plan 可以作为当前已实现行为的归档证据。response payload 与 range/NAPOT 均保持各自独立
`plan/do` 已完成状态，不属于本 timing review 的重复覆盖。

## 12.4 本轮 P2 coding：close/cutoff 前置保护

**关联 plan：** `mem_ut_v2_l2tlb_sfence_flush_token_timing_correction_plan_20260805.md` 的
`IMPLEMENTATION_DELTA` close/cutoff 条目。

**源码：** `mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`，
`capture_fired_request()`。

**抽象功能描述：** 在真实 request fire 转换为软件生命周期记录前，确认 responder admission 仍开放；关闭后出现的新
fire 属于 DUT/driver 时序违规，立即诊断，而不是留下无法归属的 token。

**修改后逻辑：**

```text
本拍先观察到 request fire：
  若 local close 尚未置位且 shared admission 尚未 seal：继续创建 token/pending，并写 request-fire marker；
  若 local close 或 shared seal 已置位：在任何状态分配前 uvm_fatal。
stop 在本 task 后续才写入 close，因此 C0 同拍 fire 仍然合法。
```

本项没有修改 token payload、C0/C4 取消规则或 release drain；只把 cutoff 检查前移到状态分配之前。源码、flow、analysis
和 plan 已同步，`git diff --check` 通过；专项 compile/smoke 待三个 P2 修改全部完成后统一执行。
