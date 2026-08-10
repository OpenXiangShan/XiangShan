# V2 SFENCE/HFENCE 分阶段 Live Entry 失效实现 Review（2026-08-10）

关联 plan：`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_sfence_hfence_stage_aware_live_entry_invalidation_plan_20260804.md`。

Review 范围：raw fence/CSR context、adapter C4 调度、stage-aware matcher、reset/release proof 与同步文档。不涉及
RM、scoreboard、coverage，也不将 L2TLB agent 改为 L2Cache/PTW 下游模型。

## 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `raw fence` | monitor 采样的一条未解码 fence 事实。 | `dispatch_raw_sfence_t`、`raw_sfence_q` | C0 raw 先入 FIFO，不立即删表。 |
| `CSR context` | raw fence 同 sample 的虚拟化、VMID、mode 快照。 | `l2tlb_sfence_csr_context` | fence 先到时由 CSR monitor 绑定。 |
| `pending invalidate` | C0 已登记、等到 C4 的 live-entry 删除工作。 | `sfence_invalidate_pending_q` | due=`anchor+4`。 |
| `adapter owner` | raw FIFO 的唯一 destructive consumer。 | `dispatch_monitor_event_adapter` | 只有它可 `peek/pop` raw fence。 |
| `architecture matcher` | 对 framework logical live entry 的精确 stage/range/ID 匹配。 | `sfence_match_entry()` | 不复刻 local cache 的 over-fence。 |
| `frozen provenance` | entry 创建时冻结的 stage-active、mode、level、generation。 | `memblock_tlb_entry` | rs1=x0 也必须验证。 |
| `C0/C4` | fence 原点和 V2 filter flush 的 delete 边界。 | `anchor_sample_seq/due_sample_seq` | C1-C3 不删 entry。 |
| `direct writer` | 对一种 runtime state 负责 reset clear/ack 的唯一组件。 | CSR monitor、adapter、fence monitor | adapter 不清 CSR context。 |

| 重点函数 | 抽象功能描述 |
|---|---|
| `service_l2tlb_sfence_events()` | 每个 dispatch sample 唯一调度 raw drain/C4 apply；不处理 token。 |
| `drain_l2tlb_sfence_events()` | 将可信 FIFO 队首转成公共 C4 work；失败前不 pop。 |
| `apply_due_sfence_invalidate()` | C4 扫描 live entry 并删除命中项；不改 UID/token。 |
| `sfence_match_entry()` | 判断单个 entry 是否命中一个解码 fence；不删除 entry。 |
| `publish_l2tlb_sfence_csr_context()` | 发布 context 并只绑定同 sample raw 队尾；不扫描整条 FIFO。 |

## Review 结论

初审发现的编译阻断、adapter 私有 pending queue、C0 destructive pop、`rs1=x0` 绕过 provenance、双 service
调度和 CSR context 全 FIFO 扫描均已解决。末轮还发现两个当前 flow 文档仍引用删除的 immediate-delete API，已改为
唯一 C0/C4 service 链路。默认 matcher 现在明确是 framework logical model，未混入 V2 `TLBStorage` 或
`PageTableCache` 的不同 over-fence 规则。未发现本轮代码与已修正 plan 的 blocker。

| 修改范围 | 修改后职责 | 结果 |
|---|---|---|
| CSR/fence monitor、sync package | 同 sample raw/context、epoch 和 topology gate | 通过 |
| dispatch adapter | 单一 raw consumer、公共 C4 queue、drain proof | 通过 |
| common data | stage-aware decode/match/schedule/apply | 通过 |
| dispatch service | 每 sample 唯一 adapter service | 通过 |
| plan/flow 文档 | 移除旧 immediate-delete 描述，并同步 CSR/writeback flow | 通过 |

## Raw Context 与 Direct Writer 边界

**修改前逻辑：** raw fence 没有完整 C0 context，adapter 无法证明 `priv_virt`、VMID/mode 来源；adapter reset 和 CSR
reset 都清同一 context；context publish 会遍历整个 raw FIFO。

**修改后逻辑：** CSR monitor 每拍发布 context，fence raw 携带 sample/reset provenance；同步包只绑定同 sample
队尾。CSR monitor 是 context 的 reset clear 唯一 writer，adapter reset 只清 raw FIFO、live entry/pending work 和 proof。

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`，函数：
`publish_l2tlb_sfence_csr_context()`。

抽象功能描述：将 CSR monitor 本拍事实绑定到同拍 raw fence；不解释 fence、不消费队列、不改 live entry。

```systemverilog
if (raw_sfence_q.size() != 0) begin
    if (!raw_sfence_q[$].context_valid && raw_sfence_q[$].sample_seq < sample_seq) begin
        `uvm_fatal("MEMBLOCK_L2TLB_SFENCE_CONTEXT", "raw fence missed same-sample CSR context")
    end
    if (raw_sfence_q[$].sample_seq > sample_seq) begin
        `uvm_fatal("MEMBLOCK_L2TLB_SFENCE_CONTEXT", "raw fence sample advanced ahead of CSR context")
    end
    if (raw_sfence_q[$].sample_seq == sample_seq) begin
        raw_sfence_q[$] = bind_raw_sfence_context(raw_sfence_q[$], context);
    end
end
```

中文伪代码：FIFO 非空时只检查队尾。队尾若是旧 sample 且未绑定，说明已错过唯一 context，fatal；若是未来
sample 也 fatal；恰好同 sample 时复制 immutable context。一个 interface 每拍最多一条 raw，队尾操作保持 O(1)。

正确性：旧 raw 不会因下一拍 CSR 被错误补绑，且 context 发布不再随 raw backlog 线性增长。

### 采集器与 Payload 类型的逐文件覆盖

下面四个文件此前只在范围表中概括提及。本节补齐它们各自的真实改动、调用位置和状态副作用，确保 review 覆盖本轮
所有有逻辑意义的源码修改。

源码位置：`mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv`，
task：`csr_ctrl_agent_agent_monitor::mon_data()`。

抽象功能描述：CSR monitor 在每个 post-reset sample 把同拍 CSR 事实发布成 fence 专用 context；它不解析 fence
操作数、不消费 raw FIFO，也不删除 live entry。

```systemverilog
memblock_sync_pkg::publish_l2tlb_csr_history(raw_csr, current_sample_seq);
memblock_sync_pkg::publish_l2tlb_sfence_csr_context(
    raw_csr,
    current_sample_seq,
    memblock_sync_pkg::get_l2tlb_current_reset_epoch(),
    $time);
if (raw_csr.satp_changed || raw_csr.vsatp_changed ||
    raw_csr.hgatp_changed || raw_csr.priv_virt_changed) begin
    memblock_sync_pkg::note_l2tlb_flush_event(
        $time, memblock_sync_pkg::MEMBLOCK_L2TLB_REASON_CSR);
end
```

中文伪代码：该逻辑先把本拍 CSR 放进固定深度的 request history，再发布同一拍、同一 reset epoch 的 fence context，
使随后到达或已经排队的同拍 raw fence 能冻结正确的虚拟化模式和 VMID。只有翻译上下文变化才登记 CSR flush
sideband；权限、debug、misalign 等运行期字段不会因此触发 live-entry 删除。`publish_l2tlb_sfence_csr_context()` 只绑定
同拍队尾，之后的 adapter 只读取已经冻结的 raw 字段。

源码位置：`mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_monitor.sv`，
task：`fence_agent_agent_monitor::mon_data()`。

抽象功能描述：fence monitor 把一个有效 fence 与它所在 global sample、sample time 和 reset epoch 关联起来，再交给
同步包按固定 topology 保存或丢弃；它不选择 S1/S2 target，也不直接调用 matcher。

```systemverilog
raw_sfence = memblock_sync_pkg::make_empty_raw_sfence();
raw_sfence.sample_seq = sample_seq;
raw_sfence.sample_time = $time;
raw_sfence.reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
if (io_ooo_to_mem_sfence_valid === 1'b1) begin
    if (memblock_sync_pkg::dispatch_l2tlb_lookup_active &&
        memblock_sync_pkg::l2tlb_raw_fence_intake_closed) begin
        `uvm_fatal(get_type_name(), "SFENCE/HFENCE arrived after raw intake close")
    end
    event_seq = memblock_sync_pkg::note_l2tlb_flush_event(
        $time, memblock_sync_pkg::MEMBLOCK_L2TLB_REASON_FENCE);
    raw_sfence.lifecycle_event_seq = event_seq;
    memblock_sync_pkg::push_raw_sfence(raw_sfence);
end
```

中文伪代码：每拍先取得 CSR monitor 已建立的 sample anchor，并将 sample time 和 reset epoch 写进空 raw。有效 fence
若发生在 dispatch-active 的 release intake 已关闭之后，先报错且不分配 lifecycle event，避免留下无人消费的 event
history。否则记录 fence reason，保存返回的 event 序号，再由 `push_raw_sfence()` 根据 topology 决定入 FIFO 或在
no-dispatch 下以 `EVENT_SEQ_NONE` 丢弃；该 helper 同时负责同拍 context 绑定。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv`，
函数：`collect_runtime_context_events()`。

抽象功能描述：这个 collector 只把最新 CSR runtime 镜像同步给公共状态；它不再是 raw fence consumer，因此不会与主
dispatch service 同拍重复调用 destructive adapter 操作。

```systemverilog
if (memblock_sync_pkg::reset_backend_done !== 1'b1 ||
    memblock_sync_pkg::l2tlb_reset_active()) begin
    monitor_adapter.reset_l2tlb_sfence_state();
    return;
end
monitor_adapter.drain_csr_events();
```

中文伪代码：reset 未完成或 L2TLB runtime reset 仍在进行时，只请求 adapter 清理其所属 raw/live 状态并返回。正常
sample 只调用 `drain_csr_events()` 更新公共 CSR runtime；不调用 `service_l2tlb_sfence_events()`，因此 raw fence 的
`peek -> schedule -> pop` 只能由 `service_monitor_once()` 的唯一调用点执行。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`，
类型：`memblock_sfence_target_stage_e`、`memblock_sfence_payload_t`。

抽象功能描述：该类型定义把 raw fence 解码后的目标 stage 与冻结的地址/context provenance 作为一个内部 payload
传给 scheduler 和 matcher；类型本身不保存 live entry，也不驱动 DUT 接口。

```systemverilog
typedef enum int unsigned {
    MEMBLOCK_SFENCE_TARGET_HS_S1 = 0,
    MEMBLOCK_SFENCE_TARGET_VS_S1 = 1,
    MEMBLOCK_SFENCE_TARGET_G_S2  = 2
} memblock_sfence_target_stage_e;

typedef struct {
    memblock_sfence_target_stage_e target_stage;
    bit [37:0] s1_vpn;
    bit [51:0] s2_gvpn;
    bit priv_virt_at_sample;
    bit [15:0] hgatp_vmid_at_sample;
    longint unsigned sample_seq;
    longint unsigned reset_epoch;
    longint unsigned lifecycle_event_seq;
} memblock_sfence_payload_t;
```

中文伪代码：解码阶段必须在三种唯一 target 中选择一种：HS S1、VS S1 或 G S2；随后保存对应 S1 VPN 或 S2 GVPN，
并保留采样时的虚拟化、VMID、sample、reset 和 lifecycle event 证明。C4 matcher 依据这些不可变字段工作，不能再从
可变化的 runtime CSR 推导 target 或 VMID。

## Adapter 单一 C4 Work 流

**修改前逻辑：** adapter 先 `pop_raw_sfence()`，写私有 `pending_l2tlb_sfence_q`，C4 时又调用已删除的
`apply_raw_sfence()`。这既无法编译，也会在 context/schedule 失败后丢 raw，并产生第二份 due 真源。

**修改后逻辑：** `common_data_transaction::sfence_invalidate_pending_q` 是唯一 C4 work 真源。adapter 固定为
`peek -> validate -> decode -> schedule -> pop`，C4 只调用 `apply_due_sfence_invalidate()`；release proof 同时检查
raw FIFO 和公共 pending queue。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，函数：
`drain_l2tlb_sfence_events()`。

抽象功能描述：dispatch-active 下唯一 raw-fence destructive consumer，将可信 raw 转为公共 C4 work；不拥有 token、UID 或 response。

```systemverilog
while (memblock_sync_pkg::peek_raw_sfence(raw_sfence)) begin
    if (!raw_sfence.context_valid && raw_sfence.sample_seq == current_sample_seq) begin
        return;
    end
    payload = data.decode_raw_sfence(raw_sfence);
    if (!data.schedule_sfence_invalidate(payload,
                                          raw_sfence.sample_seq,
                                          raw_sfence.reset_epoch,
                                          raw_sfence.lifecycle_event_seq)) begin
        `uvm_fatal("DISP_RAW_SFENCE", "raw fence schedule returned failure")
    end
    if (!memblock_sync_pkg::pop_raw_sfence(consumed_sfence)) begin
        `uvm_fatal("DISP_RAW_SFENCE", "scheduled raw fence pop failed")
    end
end
```

中文伪代码：先读取不删除的队首。stale epoch raw 记录后丢弃，future epoch、event provenance 或 context epoch
错误 fatal。当前 sample context 尚未到时保留队首。只有解码和公共 schedule 都成功后才 pop，因而 queue 不会在失败路径丢失
事实。之后 C4 apply 只扫描 logical live map，不触碰 responder token/UID。

源码位置：同文件，函数：`service_l2tlb_sfence_events()`。

抽象功能描述：一个 global sample 只执行一次完整 raw drain/C4 apply，防止以后新增 caller 形成双 consumer。

```systemverilog
if (last_l2tlb_sfence_service_sample_seq == current_sample_seq) begin
    `uvm_fatal("DISP_RAW_SFENCE", "duplicate raw-fence service sample")
end
last_l2tlb_sfence_service_sample_seq = current_sample_seq;
drain_l2tlb_sfence_events(current_sample_seq);
apply_due_l2tlb_sfence_events(current_sample_seq);
publish_l2tlb_adapter_drain_proof(current_sample_seq);
```

中文伪代码：watermark 到达当前 sample 后先拒绝同拍重复 service，再登记该 sample 已服务。接着 drain C0 raw、应用 C4
due，并在 raw FIFO 与公共 pending queue 全空时发布 drain proof。reset active 时不处理 raw，只执行 adapter reset clear/ack。

正确性：`rg` 已确认 `apply_raw_sfence`、`apply_sfence_invalidate`、`drain_sfence_events` 和 adapter 私有 pending queue
均无残留。

## 唯一调度点

**修改前逻辑：** `collect_runtime_context_events()` 同时做 CSR drain 和 raw-fence service，未来 caller 很容易同拍二次调用。

**修改后逻辑：** collector 只同步 CSR；`service_monitor_once()` 在其后唯一调用 adapter service。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv`，
task：`service_monitor_once()`。

抽象功能描述：每个 dispatch tick 的唯一 context/adapter/batch 顺序入口；不解释 raw payload。

```systemverilog
memblock_sync_pkg::tick_dispatch_service_cycle();
collect_runtime_context_events();
if (monitor_adapter == null) begin
    monitor_adapter = dispatch_monitor_event_adapter::type_id::create("monitor_adapter");
end
monitor_adapter.service_l2tlb_sfence_events();
if (memblock_sync_pkg::reset_backend_done !== 1'b1 ||
    memblock_sync_pkg::l2tlb_reset_active()) begin
    return;
end
```

中文伪代码：先推进 service cycle 并同步 CSR/reset。然后确保 adapter 对象存在且只在这里 service raw fence。reset 未收敛
时直接返回，后续 LSQ batch 不会消费 stale state；正常时才进入 monitor batch 与 redirect/replay 流程。

## 分阶段 Matcher 与 C4 删除

**修改前逻辑：** 旧 matcher 使用共享 `key.vpn`、共享 level 和 current CSR，S1/G-stage 混用，且 `rs1=x0` 可绕过
stage/mode/level 结构检查。

**修改后逻辑：** raw decode 固定为 HS S1、VS S1 或 G S2。eligible entry 无论 rs1 是否为 x0 都先验证 frozen
stage-active/mode/level；S1 address 只读 S1 tag/level/PTE.N/valididx，S2 address 只读 S2 tag/level/PTE.N。
allStage 任一选中 stage 命中均删除完整 logical entry。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，函数：`sfence_match_entry()`。

抽象功能描述：只判断 canonical live entry 是否在解码 fence 作用域；不删 entry，也不读 mutable CSR。

```systemverilog
case (payload.target_stage)
    MEMBLOCK_SFENCE_TARGET_HS_S1: begin
        if (key.s2xlate != 2'd0) return 1'b0;
        validate_frozen_stage_level(MEMBLOCK_SFENCE_TARGET_HS_S1, entry);
    end
    MEMBLOCK_SFENCE_TARGET_VS_S1: begin
        if (!(key.s2xlate inside {2'd1, 2'd3})) return 1'b0;
        validate_frozen_stage_level(MEMBLOCK_SFENCE_TARGET_VS_S1, entry);
    end
    MEMBLOCK_SFENCE_TARGET_G_S2: begin
        if (!(key.s2xlate inside {2'd2, 2'd3})) return 1'b0;
        validate_frozen_stage_level(MEMBLOCK_SFENCE_TARGET_G_S2, entry);
    end
endcase
```

中文伪代码：先按 target stage 排除无资格 `s2xlate`，这是正常不命中。对有资格 entry 先验证 frozen provenance，
所以 all-address fence 也不会静默删除坏 entry。HS S1 使用 ASID/global；VS S1 额外强制 sampled VMID；G S2 只比较
GVPN 与低 14 位 S2 VMID，不读取 S1 PTE.G。普通地址/ID 不匹配只返回不命中。

`schedule_sfence_invalidate()` 只登记 `due=C0+4`；`apply_due_sfence_invalidate()` 在 C4 收集 key 后统一删除 canonical
entry。它不删除 main/status/UID history 或 pending response snapshot。

## V2 源码边界

当前默认不是 `TLBStorage` 或 `PageTableCache` 的 source-equivalent flush。两处 V2 cache 的 over-fence eligible stage
和地址 widening 都不同，不能简化为将 `addr_ok` 设为 true。本实现只支持 framework logical live-entry 的 architecture
matcher；未来若要验证某个具体 cache，必须另建按该 cache 定义的专项。

## Plan 对齐检查

| Plan 要求 | 实现情况 |
|---|---|
| raw 使用 C0 sample/reset/context provenance | 已完成。 |
| adapter 唯一 destructive consumer | 已完成，旧 API 删除。 |
| C0 schedule、C4 delete | 已完成，公共 pending queue 是唯一真源。 |
| S1/S2 matcher 与 sampled VMID | 已完成，rs1=x0 也检查 provenance。 |
| no-dispatch 不建立 raw/live entry | 已完成 topology gate。 |
| reset direct-writer 边界 | 已分离 CSR context 与 adapter raw/live clear。 |
| V2 local over-fence | 明确不支持，不混入默认 matcher。 |

## 验证与风险

已执行：

```text
git diff --check
rg 检查旧 API/旧 private queue
make eda_compile tc=basicTest ts=virtual_base_sequence mode=base_fun
```

远端 `eda_compile` 已通过。首次 `eda_run` 会无条件再次 compile，遇到遗留 `vcs1` 占用的
`base_fun/exec/simv.daidir/work.lib++/tdc.sdb` 并报 `VFS_SDB_ERROR`；该进程已终止，生成数据库已按 VCS 提示清理。

随后在同一份成功编译的 `simv` 上执行以下只运行目标，均通过：

```text
make eda_batch_run tc=basicTest ts=virtual_base_sequence mode=base_fun
make eda_batch_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_smoke
```

两次运行均为 `UVM_ERROR=0`、`UVM_FATAL=0`；后者实际覆盖一笔 load 的 L2TLB responder、writeback、ROB commit 和
LQ deq 闭环。尚未补跑的定向场景仅包括 HS/VS/GVMA 的全部 rs1/rs2 组合、VMID delayed drain、C4 delete 与
reset stale work。

## 实现与 Plan 不一致项

均已通过关联 plan 的 `IMPLEMENTATION_DELTA` 记录。调整包括删除 adapter 私有 queue、将唯一 service 移到
`service_monitor_once()`，以及把 CSR context bind 从全 FIFO 扫描改为同 sample 队尾绑定。

## Plan 未说明但实现补充的细节

1. 同 sample 第二次 `service_l2tlb_sfence_events()` 直接 fatal，防止未来新 caller 建立双 consumer。
2. fence monitor 在创建 lifecycle event 前检查 raw intake close，避免遗留无人消费 event history。
3. no-dispatch 的 `push_raw_sfence()` 要求 event sequence 为 `NONE`，在 producer 边界 fail-fast。
4. 末轮文档扫描发现 `writeback_function_call_flow.md` 和 `csr_runtime_sync_flow.md` 仍写旧
   `drain_sfence_events()`。两者已改为 collector 仅 drain CSR，`service_monitor_once()` 独占调用
   `service_l2tlb_sfence_events()` 的当前实现。

最终结论：初审和末轮文档问题均已闭环，源码与已修正 plan 一致，基础及 real-dispatch smoke 通过；该 plan 已归档。
